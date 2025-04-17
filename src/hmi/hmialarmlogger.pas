unit HMIAlarmLogger;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, StrUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LazUTF8, HMIZones, PLCTag, ProtocolTypes, HMIDBConnection;

type

  { TAlarmItem }

  TAlarmItem=class(TZone)
  private
    FAlarmMessage: String;
    FExtraInfo: String;
    FLastEventGUID: TGuid;
    FLastEventIntID: Int64;
    FLastEventPointerID: Pointer;
    FLastTagValue: Double;
    FPLCTag: TPLCTag;
    FLastValueInitialized:Boolean;
    procedure SetAlarmMessage(AValue: String);
    procedure SetExtraInfo(AValue: String);
    procedure SetLastTagValue(AValue: Double);
    procedure SetPLCTag(AValue: TPLCTag);
    property DefaultZone;
  public
    property LastAlarmGUID:TGuid read FLastEventGUID write FLastEventGUID;
    property LastAlarmPointerID:Pointer read FLastEventPointerID write FLastEventPointerID;
    property LastAlarmIntID:Int64 read FLastEventIntID write fLastEventIntID;
    property LastTagValue:Double read FLastTagValue write SetLastTagValue;
    function LastValueInitialized:Boolean;
  published
    property PLCTag:TPLCTag read FPLCTag write SetPLCTag;
    property AlarmMessage:String read FAlarmMessage write SetAlarmMessage;
    property ExtraInfo:String read FExtraInfo write SetExtraInfo;
  end;

  { TAlarmMessagesCollection }

  TAlarmMessagesCollection = class(TOwnedCollection)
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
    FOutgoingAlarm: TOutgoingAlarm;
    FInternalAlarmIDCounter:Integer;
    procedure FinishAllPendingAlarmsDelayed;
    procedure SetAlarmMessages(AValue: TAlarmMessagesCollection);
    procedure SetAsyncDBConnection(AValue: THMIDBConnection);
    procedure TagFromListChanged(Sender: TObject);

  protected
    procedure Loaded; override;
    procedure DoIncomingAlarm         (Sender:TObject; aTimeStamp:TDateTime; AlarmMsgItem:TAlarmItem; AlarmIntID:Int64; AlarmGUID:TGuid; var AlarmIncommingSQL:UTF8String);
    procedure DoOutgoingAlarm(Sender: TObject; aTimeStamp: TDateTime;
      AlarmIntID: Int64; AlarmGUID: TGuid; var OutgoingAlarmSQL: UTF8String);
    procedure DoFinishAllPendingAlarms(Sender:TObject; var FinishAllPendingAlarmsSQL:UTF8String);
    function  GenerateNewAlarmID      (var AlarmIntID:Int64; var AlarmGUID:TGuid):Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property AlarmMessages:TAlarmMessagesCollection read FAlarmMessages write SetAlarmMessages;
    property AsyncDBConnection:THMIDBConnection read FAsyncDBConnection write SetAsyncDBConnection;
  published
    property OnIncomingAlarm         :TIncomingAlarm          read FIncomingAlarm          write FIncomingAlarm     ;
    property OnOutgoingAlarm         :TOutgoingAlarm          read FOutgoingAlarm          write FOutgoingAlarm     ;
    property OnFinishAllPendingAlarms:TFinishAllPendingAlarms read FFinishAllPendingAlarms write FFinishAllPendingAlarms;
    property OnGenerateNewAlarmID    :TGenerateNewAlarmID     read FGenerateNewAlarmID     write FGenerateNewAlarmID;
  end;

implementation


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

procedure TAlarmItem.SetExtraInfo(AValue: String);
begin
  if FExtraInfo=AValue then Exit;
  FExtraInfo:=AValue;
end;

procedure TAlarmItem.SetPLCTag(AValue: TPLCTag);
begin
  if FPLCTag=AValue then Exit;
  FPLCTag:=AValue;
end;

function TAlarmItem.LastValueInitialized: Boolean;
begin
  exit(FLastValueInitialized);
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
  NewIntID: Int64;
  TagValue: Double;
  NewGUID: TGuid;
  SQL: UTF8String;
  sqlcmds: THMIDBConnectionStatementList;
  EventTimestamp: TDateTime;
begin
  EventTimestamp:=Now;
  if Assigned(FAlarmMessages) then begin
    for c:=0 to FAlarmMessages.Count-1 do begin
      auxItem:=TAlarmItem(FAlarmMessages.Items[c]);
      if assigned(auxItem.PLCTag) and (auxItem.PLCTag=Sender) then begin
        TagValue:=(auxItem.PLCTag as ITagNumeric).GetValue;
        try
          if auxItem.LastValueInitialized and (TagValue=auxItem.LastTagValue) then
            exit;

          if (not IsEqualGUID(auxItem.LastAlarmGUID, GUID_NULL)) or (auxItem.LastAlarmIntID<>0) or Assigned(auxItem.LastAlarmPointerID) then begin
            DoOutgoingAlarm(auxItem.PLCTag, EventTimestamp, auxItem.LastAlarmIntID, auxItem.LastAlarmGUID, SQL);
            if Assigned(FAsyncDBConnection) and FAsyncDBConnection.Connected and (UTF8Length(UTF8Trim(SQL))>0) then
              FAsyncDBConnection.ExecSQL(SQL,nil,false);
            auxItem.LastAlarmGUID  := GUID_NULL;
            auxItem.LastAlarmIntID := 0;
            auxItem.LastAlarmPointerID:=nil;
          end;

          //TODO Check if in alarm

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

end.
