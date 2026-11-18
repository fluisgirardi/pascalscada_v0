{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Controle dos alarmes do CLP S7 que estão abertos: o que chegou, o que saiu, o que mudou durante uma queda da conexão.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Quem registra os alarmes (THMIS7PlusAlarmLogger) recebe do driver lotes de alarmes
  ("chegou"/"saiu") e, a cada assinatura, o retrato completo dos ativos. Esta classe
  guarda quais alarmes (pelo CpuAlarmId) estão abertos e decide, para cada lote, o que
  abrir e o que fechar; quem a usa só dá os dois eventos. Não depende de LCL nem de
  banco, então é testável sem CLP.
}
{$ELSE}
{:
  @abstract(Book-keeping of the alarms of an S7 PLC that are open: what came, what went, what changed while the connection was down.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Whoever logs the alarms (THMIS7PlusAlarmLogger) gets batches of alarms ("coming"/"going")
  from the driver and, at each subscription, the complete list of the active ones. This
  class keeps which alarms (by CpuAlarmId) are open and decides, for each batch, what to
  open and what to close; the user only supplies the two events. It depends on neither
  the LCL nor a database, so it can be tested without a PLC.
}
{$ENDIF}
unit S7PlusAlarmTracker;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, DateUtils, S7PlusAlarms;

type
  //: An alarm that is open.
  TS7PlusOpenAlarm = record
    Key:QWord;           //CpuAlarmId
    Alarm:TS7PlusAlarm;  //as it came
    IntID:Int64;
    GUID:TGuid;
  end;

  //: An alarm has to be opened: fill IntID/GUID and return True to keep it open (False: do not track it).
  TS7PlusAlarmOpenEvent = function(const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime; out IntID:Int64; out Guid:TGuid):Boolean of object;
  //: An open alarm has to be closed. Alarm is the one that said it went; for an alarm the PLC no longer has
  //: (after a reconnect) it is the last one seen, and the time is the moment it was noticed.
  TS7PlusAlarmCloseEvent = procedure(const Open:TS7PlusOpenAlarm; const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime) of object;

  TS7PlusAlarmTracker = class
  private
    FOpen:array of TS7PlusOpenAlarm;
    FOnOpen:TS7PlusAlarmOpenEvent;
    FOnClose:TS7PlusAlarmCloseEvent;
    function Find(Key:QWord):Integer;
    procedure CloseAt(Idx:Integer; const Alarm:TS7PlusAlarm; AtTime:TDateTime);
  public
    constructor Create(AOnOpen:TS7PlusAlarmOpenEvent; AOnClose:TS7PlusAlarmCloseEvent);
    //: coming opens (once per CpuAlarmId), going closes the open one with that id. With IsSnapshot the list is
    //: the complete state: what is open and is not in it as coming is closed too.
    procedure Apply(const Alarms:TS7PlusAlarmArray; IsSnapshot:Boolean);
    function Count:Integer;
    function KeyAt(Index:Integer):QWord;
    //: The time of the CPU in local time; Now when the alarm has none.
    class function EventTime(const Alarm:TS7PlusAlarm):TDateTime;
  end;

implementation

constructor TS7PlusAlarmTracker.Create(AOnOpen:TS7PlusAlarmOpenEvent; AOnClose:TS7PlusAlarmCloseEvent);
begin
  inherited Create;
  FOnOpen := AOnOpen;
  FOnClose := AOnClose;
end;

class function TS7PlusAlarmTracker.EventTime(const Alarm:TS7PlusAlarm):TDateTime;
begin
  if Alarm.TimestampNs=0 then
    Result := Now
  else
    Result := UniversalTimeToLocal(Alarm.Timestamp);
end;

function TS7PlusAlarmTracker.Find(Key:QWord):Integer;
var
  i:Integer;
begin
  Result := -1;
  for i:=0 to High(FOpen) do
    if FOpen[i].Key=Key then begin
      Result := i;
      exit;
    end;
end;

procedure TS7PlusAlarmTracker.CloseAt(Idx:Integer; const Alarm:TS7PlusAlarm; AtTime:TDateTime);
var
  Rec:TS7PlusOpenAlarm;
  j:Integer;
begin
  Rec := FOpen[Idx];
  for j:=Idx+1 to High(FOpen) do
    FOpen[j-1] := FOpen[j];
  SetLength(FOpen, Length(FOpen)-1);
  if Assigned(FOnClose) then
    FOnClose(Rec, Alarm, AtTime);
end;

procedure TS7PlusAlarmTracker.Apply(const Alarms:TS7PlusAlarmArray; IsSnapshot:Boolean);
var
  i, k, n:Integer;
  Seen:array of QWord;
  Found:Boolean;
  Rec:TS7PlusOpenAlarm;
  Gone:TS7PlusAlarm;
begin
  SetLength(Seen, 0);
  for i:=0 to High(Alarms) do
    case Alarms[i].Event of
      aeComing: begin
        SetLength(Seen, Length(Seen)+1);
        Seen[High(Seen)] := Alarms[i].CpuAlarmId;
        //already open: the same alarm seen twice (snapshot and notification)
        if Find(Alarms[i].CpuAlarmId)>=0 then continue;
        Rec := Default(TS7PlusOpenAlarm);
        Rec.Key := Alarms[i].CpuAlarmId;
        Rec.Alarm := Alarms[i];
        Rec.GUID := GUID_NULL;
        if Assigned(FOnOpen) then
          if not FOnOpen(Alarms[i], EventTime(Alarms[i]), Rec.IntID, Rec.GUID) then continue;
        n := Length(FOpen);
        SetLength(FOpen, n+1);
        FOpen[n] := Rec;
      end;
      aeGoing: begin
        k := Find(Alarms[i].CpuAlarmId);
        if k>=0 then
          CloseAt(k, Alarms[i], EventTime(Alarms[i]));
      end;
    end;

  if IsSnapshot then
    for k:=High(FOpen) downto 0 do begin
      Found := false;
      for i:=0 to High(Seen) do
        if Seen[i]=FOpen[k].Key then begin
          Found := true;
          break;
        end;
      if not Found then begin
        Gone := FOpen[k].Alarm; //a copy: closing shifts the list
        CloseAt(k, Gone, Now);
      end;
    end;
end;

function TS7PlusAlarmTracker.Count:Integer;
begin
  Result := Length(FOpen);
end;

function TS7PlusAlarmTracker.KeyAt(Index:Integer):QWord;
begin
  Result := FOpen[Index].Key;
end;

end.
