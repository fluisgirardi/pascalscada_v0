unit HMIAlarmLogger;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, StrUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LazUTF8, HMIZones, PLCTag, ProtocolTypes, HMIDBConnection, psBufDataset,
  hmibasiccolletion;

type

  { TAlarmItem }

  TAlarmItem=class(TZone)
  private
    FAlarmMessage: String;
    FDefaultZone: Boolean;
    FExtraInfo: String;
    FLastAlarmGUID: TGuid;
    FLastAlarmIntID: Int64;
    FLastAlarmPointerID: Pointer;
    FLastTagValue: Double;
    FPLCTag: TPLCTag;
    FLastValueInitialized:Boolean;
    procedure SetAlarmMessage(AValue: String);
    procedure SetAsDefaultZone(AValue: Boolean);
    procedure SetExtraInfo(AValue: String);
    procedure SetLastTagValue(AValue: Double);
    procedure SetPLCTag(AValue: TPLCTag);
  public
    property LastAlarmGUID:TGuid read FLastAlarmGUID write FLastAlarmGUID;
    property LastAlarmPointerID:Pointer read FLastAlarmPointerID write FLastAlarmPointerID;
    property LastAlarmIntID:Int64 read FLastAlarmIntID write fLastAlarmIntID;
    property LastTagValue:Double read FLastTagValue write SetLastTagValue;
    function LastValueInitialized:Boolean;
    function AlarmActive:Boolean;
  published
    property PLCTag:TPLCTag read FPLCTag write SetPLCTag;
    property AlarmMessage:String read FAlarmMessage write SetAlarmMessage;
    property ExtraInfo:String read FExtraInfo write SetExtraInfo;
  end;

  { TAlarmMessagesCollection }

  TAlarmMessagesCollection = class(THMIBasicColletion)
  public
    function Add: TAlarmItem;
  end;

  TIncomingAlarm          = procedure(Sender:TObject; aTimeStamp:TDateTime; AlarmMsgItem:TAlarmItem; AlarmIntID:Int64; AlarmGUID:TGuid; var AlarmIncommingSQL:UTF8String) of object;
  TOutgoingAlarm          = procedure(Sender:TObject; aTimeStamp:TDateTime; AlamrIntID:Int64; AlarmGUID:TGuid; var OutgoingAlarmSQL:UTF8String) of object;
  TFinishAllPendingAlarms = procedure(Sender:TObject; var FinishAllPendingAlarmsSQL:UTF8String) of object;
  TGenerateNewAlarmID     = function (var AlarmIntID:Int64; var AlarmGUID:TGuid):Boolean of object;

  { THMIAlarmLogger }

  THMIAlarmLogger = class(TComponent)
  private
    FAlarmMessages: TAlarmMessagesCollection;
    FAsyncDBConnection: THMIDBConnection;
    FFinishAllPendingAlarms: TFinishAllPendingAlarms;
    FGenerateNewAlarmID: TGenerateNewAlarmID;
    FIncomingAlarm: TIncomingAlarm;
    FOnRefreshActiveAlarms: TNotifyEvent;
    FOutgoingAlarm: TOutgoingAlarm;
    FInternalAlarmIDCounter:Integer;
    procedure FinishAllPendingAlarmsDelayed;
    procedure GetComponentState(var CurState: TComponentState);
    procedure RefreshActiveAlarmTables(Sender: TObject; DS: TFPSBufDataSet;
      error: Exception);
    procedure RefreshActiveAlarmTablesDelayed;
    procedure SetAlarmMessages(AValue: TAlarmMessagesCollection);
    procedure SetAsyncDBConnection(AValue: THMIDBConnection);
    procedure TagFromListChanged(Sender: TObject);

  protected
    procedure Loaded; override;
    procedure DoIncomingAlarm         (Sender:TObject; aTimeStamp:TDateTime; AlarmMsgItem:TAlarmItem; AlarmIntID:Int64; AlarmGUID:TGuid; var AlarmIncommingSQL:UTF8String); virtual;
    procedure DoOutgoingAlarm(Sender: TObject; aTimeStamp: TDateTime;
      AlarmIntID: Int64; AlarmGUID: TGuid; var OutgoingAlarmSQL: UTF8String); virtual;
    procedure DoFinishAllPendingAlarms(Sender:TObject; var FinishAllPendingAlarmsSQL:UTF8String); virtual;
    function  GenerateNewAlarmID      (var AlarmIntID:Int64; var AlarmGUID:TGuid):Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   FinishAllAlarms;
  published
    property AlarmMessages:TAlarmMessagesCollection read FAlarmMessages write SetAlarmMessages;
    property AsyncDBConnection:THMIDBConnection read FAsyncDBConnection write SetAsyncDBConnection;
  published
    property OnIncomingAlarm         :TIncomingAlarm          read FIncomingAlarm          write FIncomingAlarm     ;
    property OnOutgoingAlarm         :TOutgoingAlarm          read FOutgoingAlarm          write FOutgoingAlarm     ;
    property OnFinishAllPendingAlarms:TFinishAllPendingAlarms read FFinishAllPendingAlarms write FFinishAllPendingAlarms;
    property OnGenerateNewAlarmID    :TGenerateNewAlarmID     read FGenerateNewAlarmID     write FGenerateNewAlarmID;
    property OnRefreshActiveAlarms   :TNotifyEvent            read FOnRefreshActiveAlarms  write FOnRefreshActiveAlarms;
  end;

implementation

uses Math;


{ TAlarmItem }

procedure TAlarmItem.SetLastTagValue(AValue: Double);
begin
  if FLastValueInitialized and (FLastTagValue=AValue) then Exit;
  FLastTagValue:=AValue;
  FLastValueInitialized:=true;
end;

procedure TAlarmItem.SetAlarmMessage(AValue: String);
begin
  if FAlarmMessage=AValue then Exit;
  FAlarmMessage:=AValue;
end;

procedure TAlarmItem.SetAsDefaultZone(AValue: Boolean);
begin
  if FDefaultZone=AValue then Exit;
  FDefaultZone:=AValue;
end;

procedure TAlarmItem.SetExtraInfo(AValue: String);
begin
  if FExtraInfo=AValue then Exit;
  FExtraInfo:=AValue;
end;

procedure TAlarmItem.SetPLCTag(AValue: TPLCTag);
begin
  if FPLCTag=AValue then Exit;

  if Collection.Owner is THMIAlarmLogger then begin
    if assigned(FPLCTag) then begin
      FPLCTag.RemoveAllHandlersFromObject(Collection.Owner as THMIAlarmLogger);
      (Collection.Owner as THMIAlarmLogger).RemoveFreeNotification(FPLCTag);
    end;

    if assigned(AValue) and (((Collection as THMIBasicColletion).GetComponentState*[csReading,csLoading])=[]) then begin
      AValue.AddTagChangeHandler(@THMIAlarmLogger(Collection.Owner).TagFromListChanged);
      (Collection.Owner as THMIAlarmLogger).FreeNotification(AValue);
    end;
  end;

  FPLCTag:=AValue;


end;

function TAlarmItem.LastValueInitialized: Boolean;
begin
  exit(FLastValueInitialized);
end;

function TAlarmItem.AlarmActive: Boolean;
begin
  exit((not IsEqualGUID(fLastAlarmGUID, GUID_NULL)) or (fLastAlarmIntID<>0) or Assigned(FLastAlarmPointerID))
end;

{ TAlarmMessagesCollection }

function TAlarmMessagesCollection.Add: TAlarmItem;
begin
  Result:=TAlarmItem.Create(Self);
end;

{ THMIAlarmLogger }

procedure THMIAlarmLogger.SetAlarmMessages(AValue: TAlarmMessagesCollection);
begin
  if Assigned(FAlarmMessages) then
    FAlarmMessages.Assign(AValue);
end;

procedure THMIAlarmLogger.FinishAllPendingAlarmsDelayed;
var
  SQL: UTF8String;
begin
  DoFinishAllPendingAlarms(Self, SQL);
  if Assigned(FAsyncDBConnection) and FAsyncDBConnection.Connected and (UTF8Length(UTF8Trim(SQL))>0) then
    FAsyncDBConnection.ExecSQL(SQL,nil,false)

end;

procedure THMIAlarmLogger.GetComponentState(var CurState: TComponentState);
begin
  CurState:=ComponentState;
end;

procedure THMIAlarmLogger.RefreshActiveAlarmTables(Sender: TObject;
  DS: TFPSBufDataSet; error: Exception);
begin
  TThread.ForceQueue(nil, @RefreshActiveAlarmTablesDelayed);
end;

procedure THMIAlarmLogger.RefreshActiveAlarmTablesDelayed;
begin
  if Assigned(FOnRefreshActiveAlarms) then
    try
      FOnRefreshActiveAlarms(Self);
    except
    end;
end;

procedure THMIAlarmLogger.SetAsyncDBConnection(AValue: THMIDBConnection);
begin
  if FAsyncDBConnection=AValue then Exit;

  if Assigned(FAsyncDBConnection) then begin
    FAsyncDBConnection.RemoveFreeNotification(self);
  end;

  if Assigned(AValue) then
    AValue.FreeNotification(Self);

  FAsyncDBConnection:=AValue;
end;

procedure THMIAlarmLogger.TagFromListChanged(Sender: TObject);
var
  c, i: Integer;
  auxItem: TAlarmItem;
  NewIntID, bit, value, NewAlarmIntID: Int64;
  TagValue: Double;
  NewGUID, NewAlarmeGUID: TGuid;
  SQL: UTF8String;
  sqlcmds: THMIDBConnectionStatementList;
  EventTimestamp: TDateTime;
  AlarmActive: Boolean;
begin
  if ([csReading, csLoading]*ComponentState)<>[] then exit;
  EventTimestamp:=Now;
  if Assigned(FAlarmMessages) then begin
    for c:=0 to FAlarmMessages.Count-1 do begin
      auxItem:=TAlarmItem(FAlarmMessages.Items[c]);
      if assigned(auxItem.PLCTag) and (auxItem.PLCTag=Sender) then begin
        TagValue:=(auxItem.PLCTag as ITagNumeric).GetValue;
        try
          if auxItem.LastValueInitialized and (TagValue=auxItem.LastTagValue) then
            exit;

          if auxItem.ZoneType=ztBit  then begin
            bit := Trunc(auxItem.Value1);
            bit := Trunc(Power(2,bit));
          end;
          value := Trunc(TagValue);


          AlarmActive :=((auxItem.ZoneType=ztEqual      ) and (TagValue=auxItem.Value1)) or
                        ((auxItem.ZoneType=ztRange      ) and (((TagValue>auxItem.Value1) OR (auxItem.IncludeValue1 AND (TagValue>=auxItem.Value1))) AND ((TagValue<auxItem.Value2) OR (auxItem.IncludeValue2 AND (TagValue<=auxItem.Value2))))) OR
                        ((auxItem.ZoneType=ztBit        ) and (((value and bit)=bit)=auxitem.IncludeValue1)) OR
                        ((auxItem.ZoneType=ztNotEqual   ) and (TagValue<>auxItem.Value1)) OR
                        ((auxItem.ZoneType=ztOutOfRange ) and (((TagValue<auxItem.Value1) OR (auxItem.IncludeValue1 AND (TagValue<=auxItem.Value1))) OR ((TagValue>auxItem.Value2) OR (auxItem.IncludeValue2 AND (TagValue>=auxItem.Value2))))) OR
                        ((auxItem.ZoneType=ztGreaterThan) and ((TagValue>auxItem.Value1) OR (auxItem.IncludeValue1 AND (TagValue>=auxItem.Value1)))) OR
                        ((auxItem.ZoneType=ztLessThan   ) and ((TagValue<auxItem.Value1) OR (auxItem.IncludeValue1 AND (TagValue<=auxItem.Value1))));

          if AlarmActive then begin
            //alarme ativo e nao tenho ID do alarme, registra o alarme.
            if IsEqualGUID(auxItem.LastAlarmGUID, GUID_NULL) AND (auxItem.LastAlarmIntID=0) AND (auxItem.LastAlarmPointerID=nil) and GenerateNewAlarmID(NewAlarmIntID, NewAlarmeGUID) then begin
              DoIncomingAlarm(auxItem.PLCTag, EventTimestamp, auxItem, NewAlarmIntID, NewAlarmeGUID, SQL);
              if Assigned(FAsyncDBConnection) and FAsyncDBConnection.Connected and (UTF8Length(UTF8Trim(SQL))>0) then
                FAsyncDBConnection.ExecSQL(SQL,@RefreshActiveAlarmTables,false);
              auxItem.LastAlarmGUID      := NewAlarmeGUID;
              auxItem.LastAlarmIntID     := NewAlarmIntID;
              auxItem.LastAlarmPointerID := nil;
            end;
          end else begin
            //alarme inativo e tenho algum ID do alarme, registra a saida do alarme.
            if (not IsEqualGUID(auxItem.LastAlarmGUID, GUID_NULL)) or (auxItem.LastAlarmIntID<>0) or Assigned(auxItem.LastAlarmPointerID) then begin
              DoOutgoingAlarm(auxItem.PLCTag, EventTimestamp, auxItem.LastAlarmIntID, auxItem.LastAlarmGUID, SQL);
              if Assigned(FAsyncDBConnection) and FAsyncDBConnection.Connected and (UTF8Length(UTF8Trim(SQL))>0) then
                FAsyncDBConnection.ExecSQL(SQL,@RefreshActiveAlarmTables,false);
              auxItem.LastAlarmGUID  := GUID_NULL;
              auxItem.LastAlarmIntID := 0;
              auxItem.LastAlarmPointerID:=nil;
            end;
          end;
        finally
          auxItem.LastTagValue := TagValue;
        end;
      end;
    end;
  end;
end;

procedure THMIAlarmLogger.Loaded;
var
  c: Integer;
  auxItem: TAlarmItem;
begin
  inherited Loaded;

  if Assigned(FAlarmMessages) then begin
    for c:=0 to FAlarmMessages.Count-1do begin
      auxItem:=TAlarmItem(FAlarmMessages.Items[c]);
      if assigned(auxItem.PLCTag) then begin
        auxItem.PLCTag.FreeNotification(Self);
        auxItem.PLCTag.AddTagChangeHandler(@TagFromListChanged);
      end;
    end;
    AlarmMessages.Loaded;
  end;


  TThread.ForceQueue(nil, @FinishAllPendingAlarmsDelayed);
end;

procedure THMIAlarmLogger.DoIncomingAlarm(Sender: TObject;
  aTimeStamp: TDateTime; AlarmMsgItem: TAlarmItem; AlarmIntID: Int64;
  AlarmGUID: TGuid; var AlarmIncommingSQL: UTF8String);
begin
  if Assigned(FIncomingAlarm) then
    FIncomingAlarm(Sender, aTimeStamp, AlarmMsgItem, AlarmIntID, AlarmGUID, AlarmIncommingSQL);
end;

procedure THMIAlarmLogger.DoOutgoingAlarm(Sender: TObject;
  aTimeStamp: TDateTime; AlarmIntID: Int64; AlarmGUID: TGuid;
  var OutgoingAlarmSQL: UTF8String);
begin
   if Assigned(FOutgoingAlarm) then
    FOutgoingAlarm(Sender, aTimeStamp, AlarmIntID, AlarmGUID, OutgoingAlarmSQL);
end;

procedure THMIAlarmLogger.DoFinishAllPendingAlarms(Sender: TObject;
  var FinishAllPendingAlarmsSQL: UTF8String);
begin
  if Assigned(FFinishAllPendingAlarms) then
    FFinishAllPendingAlarms(Sender, FinishAllPendingAlarmsSQL);
end;

function THMIAlarmLogger.GenerateNewAlarmID(var AlarmIntID: Int64;
  var AlarmGUID: TGuid): Boolean;
begin
  Result:=false;
  if Assigned(FGenerateNewAlarmID) then
    Result:=FGenerateNewAlarmID(AlarmIntID, AlarmGUID)
  else begin
    inc(FInternalAlarmIDCounter);
    AlarmIntID:=FInternalAlarmIDCounter;

    CreateGUID(AlarmGUID);
    exit(true);
  end;
end;

constructor THMIAlarmLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlarmMessages:=TAlarmMessagesCollection.Create(Self, TAlarmItem);
  FAlarmMessages.OnNeedCompState:=@GetComponentState;
end;

destructor THMIAlarmLogger.Destroy;
var
  auxItem: TAlarmItem;
  c: Integer;
begin
  AsyncDBConnection:=nil; //release the connection

  if Assigned(FAlarmMessages) then begin
    for c:=FAlarmMessages.Count-1 downto 0 do begin
      auxItem:=TAlarmItem(FAlarmMessages.Items[c]);
      if assigned(auxItem.PLCTag) then begin
        auxItem.PLCTag.RemoveFreeNotification(Self);
      end;
    end;
  end;

  inherited Destroy;
end;

procedure THMIAlarmLogger.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  auxItem: TAlarmItem;
  c: Integer;
  aCurrentTimestamp: TDateTime;
  SQL: UTF8String;
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and  Assigned(FAlarmMessages) then begin
    if AComponent=FAsyncDBConnection then begin
      FAsyncDBConnection:=nil;
      exit;
    end;

    aCurrentTimestamp:=Now;

    for c:=FAlarmMessages.Count-1 downto 0 do begin
      auxItem:=TAlarmItem(FAlarmMessages.Items[c]);
      if (auxItem.PLCTag=AComponent) and  assigned(auxItem.PLCTag) then begin
        //Alarm is pending?
        if (not IsEqualGUID(auxItem.LastAlarmGUID, GUID_NULL)) or (auxItem.LastAlarmIntID<>0) or Assigned(auxItem.LastAlarmPointerID) then begin
          DoOutgoingAlarm(auxItem.PLCTag, aCurrentTimestamp, auxItem.LastAlarmIntID, auxItem.LastAlarmGUID, SQL);
          if Assigned(FAsyncDBConnection) and FAsyncDBConnection.Connected and (UTF8Length(UTF8Trim(SQL))>0) then
            FAsyncDBConnection.ExecSQL(SQL,nil,false);
          auxItem.LastAlarmGUID     := GUID_NULL;
          auxItem.LastAlarmIntID    := 0;
          auxItem.LastAlarmPointerID:=nil;
        end;

        auxItem.FPLCTag:=nil;
        FAlarmMessages.Delete(c);
      end;
    end;
  end;
end;

procedure THMIAlarmLogger.FinishAllAlarms;
begin
  TThread.ForceQueue(nil, @FinishAllPendingAlarmsDelayed);
end;

end.
