unit ut.s7plusalarmtracker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry, S7PlusAlarms, S7PlusAlarmTracker;

type
  TTestS7PlusAlarmTracker = class(TTestCase)
  private
    FLog:TStringList;
    FTimes:array of TDateTime;
    FNextId:Int64;
    FRefuse:Boolean;
    FTracker:TS7PlusAlarmTracker;
    function  DoOpen(const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime; out IntID:Int64; out Guid:TGuid):Boolean;
    procedure DoClose(const Open:TS7PlusOpenAlarm; const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime);
    function  Al(AId:QWord; AEvent:TS7PlusAlarmEvent; ANs:QWord=0):TS7PlusAlarm;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AComingAlarmOpensOnceWithAnID;
    procedure AGoingAlarmClosesWithTheSameID;
    procedure AGoingAlarmThatIsNotOpenIsIgnored;
    procedure TheSameAlarmSeenTwiceIsOpenedOnce;
    procedure SeveralAlarmsAreKeptApart;
    procedure AComingAndGoingInTheSameBatchOpenAndClose;
    procedure ASnapshotOpensWhatIsActiveAndClosesWhatIsNot;
    procedure AnEmptySnapshotClosesEverything;
    procedure ANotificationDoesNotCloseWhatItDoesNotMention;
    procedure TheClosedByTheSnapshotCarriesTheLastAlarmSeen;
    procedure TheEventTimeIsTheCPUsInLocalTime;
    procedure NoTimestampUsesNow;
    procedure AnAlarmTheOwnerRefusesIsNotTracked;
  end;

implementation

function TTestS7PlusAlarmTracker.Al(AId:QWord; AEvent:TS7PlusAlarmEvent; ANs:QWord):TS7PlusAlarm;
begin
  Result := Default(TS7PlusAlarm);
  Result.CpuAlarmId := AId;
  Result.Event := AEvent;
  Result.TimestampNs := ANs;
  Result.Timestamp := S7PlusTimestampToDateTime(ANs);
end;

procedure TTestS7PlusAlarmTracker.SetUp;
begin
  FLog := TStringList.Create;
  FNextId := 0;
  FRefuse := false;
  SetLength(FTimes, 0);
  FTracker := TS7PlusAlarmTracker.Create(@DoOpen, @DoClose);
end;

procedure TTestS7PlusAlarmTracker.TearDown;
begin
  FTracker.Free;
  FLog.Free;
end;

function TTestS7PlusAlarmTracker.DoOpen(const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime; out IntID:Int64; out Guid:TGuid):Boolean;
begin
  Inc(FNextId);
  IntID := FNextId;
  CreateGUID(Guid);
  Result := not FRefuse;
  FLog.Add(Format('in %d id=%d', [Alarm.CpuAlarmId, IntID]));
  SetLength(FTimes, Length(FTimes)+1);
  FTimes[High(FTimes)] := aTimeStamp;
end;

procedure TTestS7PlusAlarmTracker.DoClose(const Open:TS7PlusOpenAlarm; const Alarm:TS7PlusAlarm; aTimeStamp:TDateTime);
begin
  FLog.Add(Format('out %d id=%d', [Alarm.CpuAlarmId, Open.IntID]));
  SetLength(FTimes, Length(FTimes)+1);
  FTimes[High(FTimes)] := aTimeStamp;
end;

procedure TTestS7PlusAlarmTracker.AComingAlarmOpensOnceWithAnID;
begin
  FTracker.Apply([Al(10, aeComing)], false);
  AssertEquals(1, FLog.Count);
  AssertEquals('in 10 id=1', FLog[0]);
  AssertEquals(1, FTracker.Count);
  AssertEquals(10, Int64(FTracker.KeyAt(0)));
end;

procedure TTestS7PlusAlarmTracker.AGoingAlarmClosesWithTheSameID;
begin
  FTracker.Apply([Al(10, aeComing)], false);
  FTracker.Apply([Al(10, aeGoing)], false);
  AssertEquals(2, FLog.Count);
  AssertEquals('out 10 id=1', FLog[1]);
  AssertEquals(0, FTracker.Count);
end;

procedure TTestS7PlusAlarmTracker.AGoingAlarmThatIsNotOpenIsIgnored;
begin
  FTracker.Apply([Al(10, aeGoing)], false);
  AssertEquals(0, FLog.Count);
end;

procedure TTestS7PlusAlarmTracker.TheSameAlarmSeenTwiceIsOpenedOnce;
begin
  //the snapshot and the first notification may both carry it
  FTracker.Apply([Al(10, aeComing)], true);
  FTracker.Apply([Al(10, aeComing)], false);
  AssertEquals(1, FLog.Count);
end;

procedure TTestS7PlusAlarmTracker.SeveralAlarmsAreKeptApart;
begin
  FTracker.Apply([Al(1, aeComing), Al(2, aeComing), Al(3, aeComing)], false);
  FTracker.Apply([Al(2, aeGoing)], false);
  AssertEquals(2, FTracker.Count);
  AssertEquals('out 2 id=2', FLog[3]);
  FTracker.Apply([Al(1, aeGoing), Al(3, aeGoing)], false);
  AssertEquals(0, FTracker.Count);
  AssertEquals('out 3 id=3', FLog[5]);
end;

procedure TTestS7PlusAlarmTracker.AComingAndGoingInTheSameBatchOpenAndClose;
begin
  FTracker.Apply([Al(7, aeComing), Al(7, aeGoing)], false);
  AssertEquals(2, FLog.Count);
  AssertEquals('in 7 id=1', FLog[0]);
  AssertEquals('out 7 id=1', FLog[1]);
  AssertEquals(0, FTracker.Count);
end;

procedure TTestS7PlusAlarmTracker.ASnapshotOpensWhatIsActiveAndClosesWhatIsNot;
begin
  FTracker.Apply([Al(1, aeComing), Al(2, aeComing)], false);
  FLog.Clear;
  //while disconnected: 1 went, 3 came; 2 is still there
  FTracker.Apply([Al(2, aeComing), Al(3, aeComing)], true);
  AssertEquals(2, FLog.Count);
  AssertEquals('in 3 id=3', FLog[0]);
  AssertEquals('out 1 id=1', FLog[1]);
  AssertEquals(2, FTracker.Count);
end;

procedure TTestS7PlusAlarmTracker.AnEmptySnapshotClosesEverything;
begin
  FTracker.Apply([Al(1, aeComing), Al(2, aeComing)], false);
  FLog.Clear;
  FTracker.Apply([], true);
  AssertEquals(2, FLog.Count);
  AssertEquals(0, FTracker.Count);
end;

procedure TTestS7PlusAlarmTracker.ANotificationDoesNotCloseWhatItDoesNotMention;
begin
  FTracker.Apply([Al(1, aeComing), Al(2, aeComing)], false);
  FTracker.Apply([Al(3, aeComing)], false);
  AssertEquals(3, FTracker.Count);
end;

procedure TTestS7PlusAlarmTracker.TheClosedByTheSnapshotCarriesTheLastAlarmSeen;
begin
  FTracker.Apply([Al(5, aeComing, QWord(1577836800)*QWord(1000000000))], false);
  FLog.Clear;
  FTracker.Apply([], true);
  AssertEquals('out 5 id=1', FLog[0]);
end;

procedure TTestS7PlusAlarmTracker.TheEventTimeIsTheCPUsInLocalTime;
var
  Ns:QWord;
begin
  Ns := QWord(1577836800)*QWord(1000000000); //2020-01-01 00:00:00 UTC
  FTracker.Apply([Al(1, aeComing, Ns)], false);
  AssertTrue(Abs(FTimes[0]-UniversalTimeToLocal(EncodeDate(2020,1,1)))<1/86400);
end;

procedure TTestS7PlusAlarmTracker.NoTimestampUsesNow;
begin
  FTracker.Apply([Al(1, aeComing, 0)], false);
  AssertTrue(Abs(FTimes[0]-Now)<5/86400);
end;

procedure TTestS7PlusAlarmTracker.AnAlarmTheOwnerRefusesIsNotTracked;
begin
  FRefuse := true;
  FTracker.Apply([Al(1, aeComing)], false);
  AssertEquals(0, FTracker.Count);
  //and a going for it is ignored
  FTracker.Apply([Al(1, aeGoing)], false);
  AssertEquals(1, FLog.Count);
end;

initialization
  RegisterTest(TTestS7PlusAlarmTracker);

end.
