{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Registro dos alarmes do CLP S7-1200/1500 (S7CommPlus) no mesmo fluxo do THMIAlarmLogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  THMIS7PlusAlarmLogger é um THMIAlarmLogger que, além dos alarmes baseados em tags
  (AlarmMessages), registra os alarmes do próprio CLP: o driver (TS7CommPlusDriver)
  assina o subsistema de alarmes e entrega o que chega ao logger. Cada alarme que
  chega ganha um ID novo (OnGenerateNewAlarmID, ou o contador interno) e dispara
  OnS7PlusAlarmIncoming; quando sai, OnS7PlusAlarmOutgoing, com o mesmo ID. Os dois
  devolvem o SQL que vai para o AsyncDBConnection, como OnIncomingAlarm/OnOutgoingAlarm.
  O horário é o da CPU (convertido para a hora local).

  Ao assinar (e a cada reconexão) o driver manda primeiro o retrato dos alarmes que já
  estavam ativos; o logger o compara com o que ele tem aberto: o que está ativo e ele
  não conhece entra, o que ele tem aberto e já não está ativo sai. Assim um alarme que
  chegou ou saiu com a conexão caída não fica errado no banco.

  Os eventos são disparados na thread principal.
}
{$ELSE}
{:
  @abstract(Alarms of an S7-1200/1500 PLC (S7CommPlus) in the same flow as THMIAlarmLogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  THMIS7PlusAlarmLogger is a THMIAlarmLogger that, besides the tag based alarms
  (AlarmMessages), logs the alarms of the PLC itself: the driver (TS7CommPlusDriver)
  subscribes to the alarm subsystem and hands what arrives to the logger. Each alarm
  that comes gets a new ID (OnGenerateNewAlarmID, or the internal counter) and raises
  OnS7PlusAlarmIncoming; when it goes, OnS7PlusAlarmOutgoing, with the same ID. Both
  return the SQL that goes to the AsyncDBConnection, like OnIncomingAlarm/OnOutgoingAlarm.
  The time is the CPU's (converted to local time).

  When subscribing (and at each reconnect) the driver first sends the alarms that were
  already active; the logger compares them with what it has open: what is active and
  unknown comes in, what it has open and is no longer active goes out. So an alarm that
  came or went while the connection was down is not left wrong in the database.

  The events are raised in the main thread.
}
{$ENDIF}
unit HMIS7PlusAlarmLogger;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, HMIAlarmLogger, S7PlusFamily, S7PlusAlarms, S7PlusAlarmTracker;

type
  TS7PlusAlarmIncoming = procedure(Sender:TObject; aTimeStamp:TDateTime; const Alarm:TS7PlusAlarm;
                                   AlarmIntID:Int64; AlarmGUID:TGuid; var AlarmIncomingSQL:UTF8String) of object;
  TS7PlusAlarmOutgoing = procedure(Sender:TObject; aTimeStamp:TDateTime; const Alarm:TS7PlusAlarm;
                                   AlarmIntID:Int64; AlarmGUID:TGuid; var AlarmOutgoingSQL:UTF8String) of object;

  THMIS7PlusAlarmLogger = class(THMIAlarmLogger)
  private
    FActive: Boolean;
    FCreditLimit: SmallInt;
    FDriver: TS7CommPlusDriver;
    FLanguageId: Cardinal;
    FOnIncoming: TS7PlusAlarmIncoming;
    FOnOutgoing: TS7PlusAlarmOutgoing;
    FSendTexts: Boolean;
    FAttached: Boolean;
    FTracker: TS7PlusAlarmTracker;
    FPending: array of record
      Alarms:TS7PlusAlarmArray;
      Snapshot:Boolean;
    end;
    FPendingCS: TCriticalSection;
    procedure Attach;
    procedure Detach;
    procedure DriverAlarms(Sender:TObject; const Alarms:TS7PlusAlarmArray; IsSnapshot:Boolean);
    procedure ProcessPending;
    function  TrackerOpen(const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime; out IntID:Int64; out Guid:TGuid):Boolean;
    procedure TrackerClose(const Open:TS7PlusOpenAlarm; const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime);
    procedure SetActive(AValue: Boolean);
    procedure SetDriver(AValue: TS7CommPlusDriver);
    function  CanTalk:Boolean;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    //: Applies alarms to what the logger has open: coming opens (once per CpuAlarmId), going closes.
    //: With IsSnapshot the list is the complete state of the PLC: what is open and is not in it
    //: (as coming) is closed. The driver's alarms reach this in the main thread; it is public so
    //: that the alarms can also be fed from elsewhere (and tested).
    procedure   ProcessAlarms(const Alarms:TS7PlusAlarmArray; IsSnapshot:Boolean);
    //: How many alarms of the PLC are open now, and the CPU alarm id of one of them (0-based).
    function    OpenAlarmCount:Integer;
    function    OpenAlarmCpuId(Index:Integer):QWord;
  published
    //: The S7CommPlus driver that gets the alarms.
    property Driver:TS7CommPlusDriver read FDriver write SetDriver;
    //: Subscribe to the alarms of the PLC (needs a Driver); False stops it (what is open stays open).
    property Active:Boolean read FActive write SetActive default false;
    //: Language ID of the texts (S7PlusLang_Portuguese_BR = 1046, S7PlusLang_English_US = 1033...);
    //: 0 keeps every language the PLC sends.
    property LanguageId:Cardinal read FLanguageId write FLanguageId default 0;
    //: Ask the PLC to send the texts with each alarm. An S7-1200 without TLS refuses it: then the
    //: subscription is not created; use False there (the alarm still comes, without texts).
    property SendTexts:Boolean read FSendTexts write FSendTexts default true;
    //: Credit limit of the subscription (see TS7CommPlusDriver.SubscriptionCreditLimit).
    property CreditLimit:SmallInt read FCreditLimit write FCreditLimit default 10;
    property OnS7PlusAlarmIncoming:TS7PlusAlarmIncoming read FOnIncoming write FOnIncoming;
    property OnS7PlusAlarmOutgoing:TS7PlusAlarmOutgoing read FOnOutgoing write FOnOutgoing;
  end;

implementation

constructor THMIS7PlusAlarmLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPendingCS := TCriticalSection.Create;
  FTracker := TS7PlusAlarmTracker.Create(@TrackerOpen, @TrackerClose);
  FSendTexts := true;
  FCreditLimit := 10;
end;

destructor THMIS7PlusAlarmLogger.Destroy;
begin
  Detach;
  TThread.RemoveQueuedEvents(nil, @ProcessPending);
  SetLength(FPending, 0);
  FreeAndNil(FPendingCS);
  FreeAndNil(FTracker);
  FDriver := nil;
  inherited Destroy;
end;

function THMIS7PlusAlarmLogger.CanTalk:Boolean;
begin
  Result := FActive and Assigned(FDriver) and
            ([csDesigning, csLoading, csReading, csDestroying]*ComponentState=[]);
end;

procedure THMIS7PlusAlarmLogger.Loaded;
begin
  inherited Loaded;
  if CanTalk then Attach;
end;

procedure THMIS7PlusAlarmLogger.Attach;
var
  Langs:array of Cardinal;
begin
  if FAttached or (FDriver=nil) then exit;
  FAttached := true;
  FDriver.AddAlarmHandler(@DriverAlarms);
  if FLanguageId=0 then
    SetLength(Langs, 0)
  else begin
    SetLength(Langs, 1);
    Langs[0] := FLanguageId;
  end;
  FDriver.AlarmRequest(Langs, FSendTexts, FCreditLimit);
end;

procedure THMIS7PlusAlarmLogger.Detach;
begin
  if not FAttached then exit;
  FAttached := false;
  if Assigned(FDriver) then begin
    FDriver.RemoveAlarmHandler(@DriverAlarms);
    FDriver.AlarmUnsubscribe;
  end;
end;

procedure THMIS7PlusAlarmLogger.SetActive(AValue: Boolean);
begin
  if FActive=AValue then exit;
  FActive := AValue;
  if FActive then begin
    if CanTalk then Attach;
  end else
    Detach;
end;

procedure THMIS7PlusAlarmLogger.SetDriver(AValue: TS7CommPlusDriver);
begin
  if FDriver=AValue then exit;
  Detach;
  if Assigned(FDriver) then
    FDriver.RemoveFreeNotification(Self);
  FDriver := AValue;
  if Assigned(FDriver) then
    FDriver.FreeNotification(Self);
  if CanTalk then Attach;
end;

procedure THMIS7PlusAlarmLogger.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation=opRemove) and (AComponent=FDriver) then begin
    //the driver is going away: its handler list goes with it
    FAttached := false;
    FDriver := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

//Any thread (the scan thread of the driver): the alarms are kept and handled in the main thread.
procedure THMIS7PlusAlarmLogger.DriverAlarms(Sender:TObject; const Alarms:TS7PlusAlarmArray; IsSnapshot:Boolean);
var
  n:Integer;
begin
  if FPendingCS=nil then exit;
  FPendingCS.Enter;
  try
    n := Length(FPending);
    SetLength(FPending, n+1);
    FPending[n].Alarms := Alarms;
    FPending[n].Snapshot := IsSnapshot;
  finally
    FPendingCS.Leave;
  end;
  TThread.ForceQueue(nil, @ProcessPending);
end;

procedure THMIS7PlusAlarmLogger.ProcessPending;
var
  Batches:array of record
    Alarms:TS7PlusAlarmArray;
    Snapshot:Boolean;
  end;
  i:Integer;
begin
  if FPendingCS=nil then exit;
  FPendingCS.Enter;
  try
    SetLength(Batches, Length(FPending));
    for i:=0 to High(FPending) do begin
      Batches[i].Alarms := FPending[i].Alarms;
      Batches[i].Snapshot := FPending[i].Snapshot;
    end;
    SetLength(FPending, 0);
  finally
    FPendingCS.Leave;
  end;
  for i:=0 to High(Batches) do
    ProcessAlarms(Batches[i].Alarms, Batches[i].Snapshot);
end;

function THMIS7PlusAlarmLogger.TrackerOpen(const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime; out IntID:Int64; out Guid:TGuid):Boolean;
var
  SQL:UTF8String;
begin
  IntID := 0;
  Guid := GUID_NULL;
  Result := GenerateNewAlarmID(IntID, Guid);
  if not Result then exit;
  SQL := '';
  if Assigned(FOnIncoming) then
    FOnIncoming(Self, aTimeStamp, Alarm, IntID, Guid, SQL);
  ExecAlarmSQL(SQL);
end;

procedure THMIS7PlusAlarmLogger.TrackerClose(const Open:TS7PlusOpenAlarm; const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime);
var
  SQL:UTF8String;
begin
  SQL := '';
  if Assigned(FOnOutgoing) then
    FOnOutgoing(Self, aTimeStamp, Alarm, Open.IntID, Open.GUID, SQL);
  ExecAlarmSQL(SQL);
end;

procedure THMIS7PlusAlarmLogger.ProcessAlarms(const Alarms:TS7PlusAlarmArray; IsSnapshot:Boolean);
begin
  FTracker.Apply(Alarms, IsSnapshot);
end;

function THMIS7PlusAlarmLogger.OpenAlarmCount:Integer;
begin
  Result := FTracker.Count;
end;

function THMIS7PlusAlarmLogger.OpenAlarmCpuId(Index:Integer):QWord;
begin
  Result := FTracker.KeyAt(Index);
end;

end.
