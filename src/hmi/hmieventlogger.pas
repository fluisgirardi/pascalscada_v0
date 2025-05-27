unit HMIEventLogger;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, PLCTag,
  Tag, ProtocolTypes, HMIDBConnection;

type

  { TEventCollectionItem }

  TEventCollectionItem = class(TCollectionItem)
  private
    FEventColor: TColor;
    FEventDescription: String;
    FEventValue: Integer;
    procedure SetEventColor(AValue: TColor);
    procedure SetEventDescription(AValue: String);
    procedure SetEventValue(AValue: Integer);
  protected
    function GetDisplayName: string; override;
  published
    property EventDescription:String read FEventDescription write SetEventDescription;
    property EventValue:Integer read FEventValue write SetEventValue;
    property EventColor:TColor read FEventColor write SetEventColor;
  end;

  { TEventCollection }

  TEventCollection = class(TOwnedCollection)
  public
    function Add: TEventCollectionItem;
    constructor Create(AOwner: TPersistent);
  end;

  { TEventTagColletionItem }

  TEventTagColletionItem=class(TCollectionItem)
  private
    FIgnoreDescriptionList: Boolean;
    FLastEventGUID: TGuid;
    FLastEventIntID: Int64;
    FLastEventPointerID: Pointer;
    FLastTagValue: Double;
    FPendingUpdate: Boolean;
    FPLCTag: TPLCTag;
    FTagDesc: String;
    FTagID: Integer;
    FTagPath: String;
    FLastValueInitialized:Boolean;
    procedure SetLastTagValue(AValue: Double);
    procedure SetPLCTag(AValue: TPLCTag);
    procedure SetTagDesc(AValue: String);
    procedure SetTagID(AValue: Integer);
    procedure SetTagPath(AValue: String);
  protected
    function GetDisplayName: string; override;
  public
    property LastEventGUID:TGuid read FLastEventGUID write FLastEventGUID;
    property LastEventPointerID:Pointer read FLastEventPointerID write FLastEventPointerID;
    property LastEventIntID:Int64 read FLastEventIntID write fLastEventIntID;
    property LastTagValue:Double read FLastTagValue write SetLastTagValue;
    property PendingUpdate:Boolean read FPendingUpdate write FPendingUpdate;
    function LastValueInitialized:Boolean;
  published
    property PLCTag:TPLCTag read FPLCTag write SetPLCTag;
    property TagID:Integer  read FTagID write SetTagID;
    property TagPath:String read FTagPath write SetTagPath;
    property TagDesc:String read FTagDesc write SetTagDesc;
    property IgnoreDescriptionList:Boolean read FIgnoreDescriptionList write FIgnoreDescriptionList;
  end;

  { TEventTagColletion }

  TEventTagColletion=class(TOwnedCollection)
  public
    function Add: TEventTagColletionItem;
    constructor Create(AOwner: TPersistent);
  end;

  TTagEventFinished = procedure (Sender:TObject; EventIntID:Int64; EventGUID:TGuid; var FinishEventSQL:String) of object;
  TFinishAllTagEvents = procedure(Sender:TObject; var FinishAllEventsSQL:String) of object;
  TNewTagEvent = procedure(Sender:TObject; TagItem:TEventTagColletionItem; EventIntID:Int64; EventGUID:TGuid; EventDesc:TEventCollectionItem; var NewTagEventSQL:THMIDBConnectionStatementList) of object;
  TGenerateNewEventID = function(var EventIntID:Int64; var EventGUID:TGuid):Boolean of object;

  { THMIEventLogger }

  THMIEventLogger = class(TComponent)
  private
    FAsyncDBConnection: THMIDBConnection;
    FCurrentEventTimestamp: TDateTime;
    FEventDescriptions: TEventCollection;
    FEventTags: TEventTagColletion;
    FOnFinishAllTagEvents: TFinishAllTagEvents;
    FOnGenerateNewEventID: TGenerateNewEventID;
    FOnNewTagEvent: TNewTagEvent;
    FOnTagEventFinished: TTagEventFinished;
    FInternalEventIDCounter:Integer;
    procedure SetAsyncDBConnection(AValue: THMIDBConnection);
    procedure SetEventDescriptions(AValue: TEventCollection);
    procedure SetEventTags(AValue: TEventTagColletion);
    procedure TagChangedDelayed;
    procedure TagChangedDelayed2(Data: PtrInt);
    procedure TagFromListChanged(Sender: TObject);
    procedure FinishAllEventsDelayed;
  protected
    procedure FinishCurrentEvent(aItem: TEventTagColletionItem);
    procedure Loaded; override;
    procedure DoTagEventFinished(Sender:TObject; EventIntID:Int64; EventGUID:TGuid; var FinishEventSQL:String); virtual;
    procedure DoFinishAllTagEvents(Sender:TObject; var FinishAllEventsSQL:String); virtual;
    procedure DoNewTagEvent(Sender: TObject; TagItem:TEventTagColletionItem; EventIntID: Int64; EventGUID: TGuid;
      EventDesc: TEventCollectionItem;
  var NewEventSQL: THMIDBConnectionStatementList); virtual;
    function  GenerateNewEventID(var EventIntID:Int64; var EventGUID:TGuid):Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published                                                                                        
    property AsyncDBConnection:THMIDBConnection read FAsyncDBConnection write SetAsyncDBConnection;
    property EventDescriptions:TEventCollection read FEventDescriptions write SetEventDescriptions;
    property EventTags:TEventTagColletion read FEventTags write SetEventTags;
  published
    property OnTagEventFinished:TTagEventFinished read FOnTagEventFinished write FOnTagEventFinished;
    property OnFinishAllTagEvents:TFinishAllTagEvents read FOnFinishAllTagEvents write FOnFinishAllTagEvents;
    property OnNewTagEvent:TNewTagEvent read FOnNewTagEvent write FOnNewTagEvent;
    property OnGenerateNewEventID:TGenerateNewEventID read FOnGenerateNewEventID write FOnGenerateNewEventID;
    property CurrentEventTimestamp:TDateTime read FCurrentEventTimestamp;
  end;

implementation

{ TEventCollectionItem }

procedure TEventCollectionItem.SetEventColor(AValue: TColor);
begin
  if FEventColor=AValue then Exit;
  FEventColor:=AValue;
end;

procedure TEventCollectionItem.SetEventDescription(AValue: String);
begin
  if FEventDescription=AValue then Exit;
  FEventDescription:=AValue;
end;

procedure TEventCollectionItem.SetEventValue(AValue: Integer);
begin
  if FEventValue=AValue then Exit;
  FEventValue:=AValue;
end;

function TEventCollectionItem.GetDisplayName: string;
begin
  Result:=FEventValue.ToString+'="'+FEventDescription+'" ('+ColorToString(FEventColor)+')';
end;

{ TEventCollection }

function TEventCollection.Add: TEventCollectionItem;
begin
  //Result:=TEventCollectionItem.Create(Self);
  Result := TEventCollectionItem(inherited Add);
end;

constructor TEventCollection.Create(AOwner: TPersistent);
begin
  //inherited Create(TEventCollectionItem);
    inherited Create(AOwner, TEventCollectionItem);
end;

{ TEventTagColletionItem }

procedure TEventTagColletionItem.SetPLCTag(AValue: TPLCTag);
begin
  if FPLCTag=AValue then Exit;

  if Collection.Owner is THMIEventLogger then begin
    if assigned(FPLCTag) then begin
      FPLCTag.RemoveAllHandlersFromObject(Collection.Owner as THMIEventLogger);
      (Collection.Owner as THMIEventLogger).RemoveFreeNotification(FPLCTag);
    end;

    if assigned(AValue) and ((((Collection as TOwnedCollection).Owner as TComponent).ComponentState*[csReading,csLoading])=[]) then begin
      AValue.AddTagChangeHandler(@THMIEventLogger(Collection.Owner).TagFromListChanged);
      (Collection.Owner as THMIEventLogger).FreeNotification(AValue);
    end;
  end;

  FPLCTag:=AValue;
end;

procedure TEventTagColletionItem.SetLastTagValue(AValue: Double);
begin
  if FLastValueInitialized and (FLastTagValue=AValue) then Exit;
  FLastTagValue:=AValue;
  FLastValueInitialized:=true;
end;

procedure TEventTagColletionItem.SetTagDesc(AValue: String);
begin
  if FTagDesc=AValue then Exit;
  FTagDesc:=AValue;
end;

procedure TEventTagColletionItem.SetTagID(AValue: Integer);
begin
  if FTagID=AValue then Exit;
  FTagID:=AValue;
end;

procedure TEventTagColletionItem.SetTagPath(AValue: String);
begin
  if FTagPath=AValue then Exit;
  FTagPath:=AValue;
end;

function TEventTagColletionItem.GetDisplayName: string;
begin
  Result:=inttostr(FTagID) + ' = ' + FTagDesc+' (';
  if Assigned(FPLCTag) then begin
    if String(PLCTag.Name).IsEmpty then
      Result:=Result+'<empty name>'
    else
      Result:=Result+FPLCTag.Name;
  end else
    Result:=Result+'<Nil>';

  Result:=Result+')';
end;

function TEventTagColletionItem.LastValueInitialized: Boolean;
begin
  exit(FLastValueInitialized);
end;

{ TEventTagColletion }

function TEventTagColletion.Add: TEventTagColletionItem;
begin
  Result:=TEventTagColletionItem(inherited Add);
end;

constructor TEventTagColletion.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TEventTagColletionItem);
end;

{ THMIEventLogger }

procedure THMIEventLogger.SetEventDescriptions(AValue: TEventCollection);
begin
  if assigned(FEventDescriptions) then
    FEventDescriptions.Assign(AValue);
end;

procedure THMIEventLogger.SetAsyncDBConnection(AValue: THMIDBConnection);
begin
  if FAsyncDBConnection=AValue then Exit;

  if Assigned(FAsyncDBConnection) then begin
    FAsyncDBConnection.RemoveFreeNotification(self);
  end;

  if Assigned(AValue) then
    AValue.FreeNotification(Self);

  FAsyncDBConnection:=AValue;
end;

procedure THMIEventLogger.SetEventTags(AValue: TEventTagColletion);
begin
  if Assigned(FEventTags) then
    FEventTags.Assign(AValue);
end;

procedure THMIEventLogger.TagChangedDelayed;
var
  auxItem: TEventTagColletionItem;
  c: Integer;
begin
  if Assigned(FEventTags) then begin
     for c:=0 to FEventTags.Count-1 do begin
       auxItem:=TEventTagColletionItem(FEventTags.Items[c]);
       if Assigned(auxItem.PLCTag) then begin
         if auxItem.PendingUpdate then begin
           TagFromListChanged(auxItem.PLCTag);
         end;
       end else
         auxItem.PendingUpdate:=false;
     end;
  end;
end;

procedure THMIEventLogger.TagChangedDelayed2(Data: PtrInt);
begin
  TagFromListChanged(TObject(Data));
end;

procedure THMIEventLogger.TagFromListChanged(Sender: TObject);
var
  c, i: Integer;
  auxItem: TEventTagColletionItem;
  TagValue, NewIntID: Int64;
  auxReal: Double;
  auxItemEvt: TEventCollectionItem;
  NewGUID: TGuid;
  SQL: String;
  sqlcmds: THMIDBConnectionStatementList;
  b, a, c1: Boolean;
begin
  if ([csReading,csLoading]*ComponentState<>[]) then
    exit;

  if Assigned(FEventTags) then begin
    for c:=0 to FEventTags.Count-1 do begin
      auxItem:=TEventTagColletionItem(FEventTags.Items[c]);
      if assigned(auxItem.PLCTag) and (auxItem.PLCTag=Sender) then begin
        auxReal:=(auxItem.PLCTag as ITagNumeric).GetValue;

        try
          if auxItem.LastValueInitialized and (auxReal=auxItem.LastTagValue) then
            exit;

          TagValue:=trunc(auxReal);
          FCurrentEventTimestamp:=Now;

          FinishCurrentEvent(auxItem);

          for i:=0 to FEventDescriptions.Count-1 do begin
            auxItemEvt:=TEventCollectionItem(FEventDescriptions.Items[i]);
            if (auxItemEvt.EventValue=TagValue) or (auxItem.FIgnoreDescriptionList) then begin
              if not GenerateNewEventID(NewIntID, NewGUID) then exit;
              try
                sqlcmds:=THMIDBConnectionStatementList.Create;
                DoNewTagEvent(auxItem.PLCTag, auxItem, NewIntID, NewGUID,auxItemEvt, sqlcmds);
                a:=Assigned(FAsyncDBConnection);
                b:=FAsyncDBConnection.Connected;
                c1:=(sqlcmds.Count > 0);
                if a and b and c1 then begin
                  FAsyncDBConnection.ExecTransaction(sqlcmds,nil,true, false);
                  auxItem.PendingUpdate:=false;
                end else begin
                  FreeAndNil(sqlcmds);
                  if FAsyncDBConnection.Connected=false then begin
                    auxItem.PendingUpdate:=true;
                    auxItem.FLastValueInitialized:=false;
                    Application.QueueAsyncCall(@TagChangedDelayed2, PtrInt(Sender));
                  end;
                end;
                exit;
              finally
                auxItem.LastEventGUID:= NewGUID;
                auxItem.LastEventIntID:= NewIntID;
              end;
              break;
            end;
          end;
        finally
          if not auxItem.PendingUpdate then
            auxItem.LastTagValue:=auxReal;
        end;
      end;
    end;
  end;
end;

procedure THMIEventLogger.FinishAllEventsDelayed;
var
  SQL: String;
begin
  DoFinishAllTagEvents(Self, SQL);
  if Assigned(FAsyncDBConnection) and FAsyncDBConnection.Connected then
    FAsyncDBConnection.ExecSQL(SQL,nil,false)
end;

procedure THMIEventLogger.FinishCurrentEvent(aItem:TEventTagColletionItem);
var
  auxItem: TEventTagColletionItem;
  SQL: String;
begin
  auxItem:=aItem;
  if (not IsEqualGUID(auxItem.LastEventGUID, GUID_NULL)) or (auxItem.LastEventIntID<>0) or Assigned(auxItem.LastEventPointerID) then begin
    DoTagEventFinished(auxItem.PLCTag, auxItem.LastEventIntID, auxItem.LastEventGUID, SQL);
    if Assigned(FAsyncDBConnection) and FAsyncDBConnection.Connected and not SQL.Trim.IsEmpty then
      FAsyncDBConnection.ExecSQL(sql,nil,false);
    auxItem.LastEventGUID     := GUID_NULL;
    auxItem.LastEventIntID    := 0;
    auxItem.LastEventPointerID:=nil;
  end;
end;

procedure THMIEventLogger.Loaded;
var
  c: Integer;
  auxItem: TEventTagColletionItem;
begin
  inherited Loaded;

  if Assigned(FEventTags) then begin
    for c:=0 to FEventTags.Count-1do begin
      auxItem:=TEventTagColletionItem(FEventTags.Items[c]);
      if assigned(auxItem.PLCTag) then begin
        auxItem.PLCTag.FreeNotification(Self);
        auxItem.PLCTag.AddTagChangeHandler(@TagFromListChanged);
      end;
    end;
  end;

  TThread.ForceQueue(nil, @FinishAllEventsDelayed);
end;

procedure THMIEventLogger.DoTagEventFinished(Sender: TObject;
  EventIntID: Int64; EventGUID: TGuid; var FinishEventSQL: String);
begin
  if Assigned(FOnTagEventFinished) then
    FOnTagEventFinished(Sender, EventIntID, EventGUID, FinishEventSQL);
end;

procedure THMIEventLogger.DoFinishAllTagEvents(Sender: TObject; var FinishAllEventsSQL: String);
begin
  if Assigned(FOnFinishAllTagEvents) then
    FOnFinishAllTagEvents(Sender, FinishAllEventsSQL);
end;

procedure THMIEventLogger.DoNewTagEvent(Sender: TObject;
  TagItem: TEventTagColletionItem; EventIntID: Int64; EventGUID: TGuid;
  EventDesc: TEventCollectionItem;
  var NewEventSQL: THMIDBConnectionStatementList);
begin
  if Assigned(FOnNewTagEvent) then
    FOnNewTagEvent(Sender, TagItem, EventIntID, EventGUID, EventDesc, NewEventSQL);
end;

function THMIEventLogger.GenerateNewEventID(var EventIntID: Int64;
  var EventGUID: TGuid): Boolean;
begin
  Result:=false;
  if Assigned(FOnGenerateNewEventID) then
    Result:=FOnGenerateNewEventID(EventIntID, EventGUID)
  else begin
    inc(FInternalEventIDCounter);
    EventIntID:=FInternalEventIDCounter;

    CreateGUID(EventGUID);
    exit(true);
  end;
end;

constructor THMIEventLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventTags:=TEventTagColletion.Create(Self);
  FEventDescriptions:=TEventCollection.Create(Self);
end;

destructor THMIEventLogger.Destroy;
var
  auxItem: TEventTagColletionItem;
  c: Integer;
begin
  if Assigned(FEventDescriptions) then
    FreeAndNil(FEventDescriptions);

  AsyncDBConnection:=nil; //release the connection

  if Assigned(FEventTags) then begin
    for c:=FEventTags.Count-1 downto 0 do begin
      auxItem:=TEventTagColletionItem(FEventTags.Items[c]);
      if assigned(auxItem.PLCTag) then begin
        auxItem.PLCTag.RemoveFreeNotification(Self);
      end;
    end;
    FreeAndNil(FEventTags);
  end;

  inherited Destroy;
end;

procedure THMIEventLogger.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  auxItem: TEventTagColletionItem;
  c: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and  Assigned(FEventTags) then begin
    if AComponent=FAsyncDBConnection then begin
      FAsyncDBConnection:=nil;
      exit;
    end;

    for c:=FEventTags.Count-1 downto 0 do begin
      auxItem:=TEventTagColletionItem(FEventTags.Items[c]);
      if (auxItem.PLCTag=AComponent) and  assigned(auxItem.PLCTag) then begin
        auxItem.FPLCTag:=nil;
        //TODO finish the active event?
        FEventTags.Delete(c);
      end;
    end;
  end;
end;

end.
