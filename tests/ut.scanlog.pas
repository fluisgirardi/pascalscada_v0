{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do log de scan, threads, tags e eventos.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um driver de teste entrega valores e resultados programados a um
  TPLCTagNumber de verdade; o que se confere e' o que chega ao hub: as threads
  do driver iniciando e terminando, as excecoes do scan (antes engolidas), os
  pedidos e resultados de escrita, a leitura que passa a falhar e volta (uma
  vez so'), a mudanca de valor e os eventos disparados.
}
{$ELSE}
{:
  @abstract(Scan, thread, tag and event logging tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A test driver hands programmed values and results to a real TPLCTagNumber;
  what is checked is what reaches the hub: the driver threads starting and
  finishing, the scan exceptions (swallowed before), write requests and
  results, reading that starts failing and comes back (once), the value
  change and the events fired.
}
{$ENDIF}
unit ut.scanlog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, ProtocolDriver, PLCTagNumber, pscadalog,
  testsupport.logcollector;

type

  { TLogTestDriver }

  TLogTestDriver = class(TProtocolDriver)
  public
    ReadResult, WriteResult:TProtocolIOResult;
    Value:Double;
    RaiseInScanRead:Boolean;
    //writes: how long the "device" takes, how many and the last values (scan thread)
    WriteDelayMs:Integer;
    WriteCount:Integer;
    LastWritten:TArrayOfDouble;
    //a driver whose port is missing: every scan pass ends with the 50 ms pause
    NoPort:Boolean;
  protected
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    //a real driver checks the address here; this one takes any tag
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
  public
    constructor Create(AOwner:TComponent); override;
    function  NeedsExternalPort:Boolean; override;
    function  SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
  end;

  { TTestScanLog }

  TTestScanLog = class(TTestCase)
  private
    FOwner:TComponent;
    FLog:TLogCollector;
    FDriver:TLogTestDriver;
    FRaiseInHandler:Boolean;
    procedure NewLog(Categories:TpSCADALogCategories; MinLevel:TpSCADALogLevel);
    function  NewDriver:TLogTestDriver;
    function  NewTag(const AName:String):TPLCTagNumber;
    //: Pumps the main thread queue until an entry with the prefix shows up.
    function  WaitForEntry(Category:TpSCADALogCategory; const Prefix:String; Ms:Integer = 3000):Integer;
    function  CountOf(Category:TpSCADALogCategory; const Prefix:String):Integer;
    function  WaitValue(T:TPLCTagNumber; V:Double; Ms:Integer = 3000):Boolean;
    procedure Pump(Ms:Integer);
    procedure RaisingHandler(Sender:TObject);
    procedure QuietHandler(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheDriverThreadsPostStartAndFinish;
    procedure AnExceptionInTheScanIsPosted;
    procedure AWriteIsPostedWithItsValuesAndResult;
    procedure AFailedWriteIsAWarning;
    procedure AReadFailureIsPostedOnceAndTheRecoveryToo;
    procedure EveryReadIsPostedAtTrace;
    procedure AValueChangeIsPostedWithTheValue;
    procedure TheEventsFiredArePosted;
    procedure AnExceptionInAHandlerIsPostedAndTheOtherTagsStillUpdate;
    procedure ATagDestroyedWhileTheDriverWaitsIsNotTouchedAfterwards;
  end;

implementation

{ TLogTestDriver }

constructor TLogTestDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  ReadResult := ioOk;
  WriteResult := ioOk;
end;

procedure TLogTestDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep := 5;
  if RaiseInScanRead then
    raise Exception.Create('the device answered nonsense');
end;

procedure TLogTestDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin
  SetLength(values.Values, 1);
  values.Values[0] := Value;
  values.LastQueryResult := ReadResult;
  values.ClkMonotonicTStamp := GetTickCount64;
end;

function TLogTestDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  if WriteDelayMs>0 then Sleep(WriteDelayMs);
  LastWritten := Copy(Values);
  Inc(WriteCount);
  Result := WriteResult;
  //what was written is what is read back
  if (Result=ioOk) and (Length(Values)>0) then Value := Values[0];
end;

function TLogTestDriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  SetLength(Values, 1);
  Values[0] := Value;
  Result := ReadResult;
end;

procedure TLogTestDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
begin
  inherited DoAddTag(TagObj, true);
end;

function TLogTestDriver.NeedsExternalPort:Boolean;
begin
  Result := NoPort;
end;

function TLogTestDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  ProtocolTagType := ptByte;
  Result := 8;
end;

{ TTestScanLog }

procedure TTestScanLog.SetUp;
begin
  FOwner := TComponent.Create(nil);
  FOwner.Name := 'Form1';
  FRaiseInHandler := false;
end;

procedure TTestScanLog.TearDown;
begin
  FreeAndNil(FOwner); //tags and driver
  FDriver := nil;
  FreeAndNil(FLog);
end;

procedure TTestScanLog.NewLog(Categories:TpSCADALogCategories; MinLevel:TpSCADALogLevel);
begin
  FLog := TLogCollector.Create(Categories, MinLevel);
end;

function TTestScanLog.NewDriver:TLogTestDriver;
begin
  FDriver := TLogTestDriver.Create(FOwner);
  FDriver.Name := 'Drv1';
  Result := FDriver;
end;

function TTestScanLog.NewTag(const AName:String):TPLCTagNumber;
begin
  Result := TPLCTagNumber.Create(FOwner);
  Result.Name := AName;
  Result.RefreshTime := 20;
  Result.ProtocolDriver := FDriver;
end;

procedure TTestScanLog.Pump(Ms:Integer);
var
  start:QWord;
begin
  start := GetTickCount64;
  repeat
    CheckSynchronize(5);
  until GetTickCount64-start>=QWord(Ms);
end;

function TTestScanLog.WaitForEntry(Category:TpSCADALogCategory; const Prefix:String;
  Ms:Integer):Integer;
var
  start:QWord;
begin
  start := GetTickCount64;
  repeat
    CheckSynchronize(5);
    FLog.Flush;
    Result := FLog.IndexOf(Category, Prefix);
    if Result>=0 then exit;
  until GetTickCount64-start>=QWord(Ms);
end;

function TTestScanLog.CountOf(Category:TpSCADALogCategory; const Prefix:String):Integer;
var
  i:Integer;
begin
  Result := 0;
  i := FLog.IndexOf(Category, Prefix);
  while i>=0 do begin
    Inc(Result);
    i := FLog.IndexOf(Category, Prefix, i+1);
  end;
end;

function TTestScanLog.WaitValue(T:TPLCTagNumber; V:Double; Ms:Integer):Boolean;
var
  start:QWord;
begin
  start := GetTickCount64;
  repeat
    CheckSynchronize(5);
    if T.Value=V then exit(true);
  until GetTickCount64-start>=QWord(Ms);
  Result := false;
end;

procedure TTestScanLog.RaisingHandler(Sender:TObject);
begin
  if FRaiseInHandler then
    raise Exception.Create('bug in the screen code');
end;

procedure TTestScanLog.QuietHandler(Sender:TObject);
begin
end;

procedure TTestScanLog.TheDriverThreadsPostStartAndFinish;
var
  i:Integer;
begin
  NewLog([lcThread], llDebug);
  NewDriver;
  AssertTrue('scan thread started'+FLog.Dump, WaitForEntry(lcThread, 'TScanThread started')>=0);
  AssertTrue('update thread started'+FLog.Dump, WaitForEntry(lcThread, 'TScanUpdate started')>=0);
  FreeAndNil(FDriver);
  AssertTrue('flush', FLog.Flush);
  AssertTrue('scan thread finished'+FLog.Dump, FLog.IndexOf(lcThread, 'TScanThread finished')>=0);
  AssertTrue('update thread finished'+FLog.Dump, FLog.IndexOf(lcThread, 'TScanUpdate finished')>=0);
  //the threads are created in AfterConstruction, before the driver has a name, and
  //post when they begin to run: whether the name was set by then depends on the
  //scheduler (usually not on Linux, usually yes on Windows), so either is right
  i := FLog.IndexOf(lcThread, 'TScanThread started');
  AssertTrue('the driver is the source, unnamed or named: '+FLog[i].Source,
             (FLog[i].Source='Form1.<TLogTestDriver>') or (FLog[i].Source='Form1.Drv1'));
  AssertTrue('posted by the thread itself', FLog[i].ThreadNo>0);
  i := FLog.IndexOf(lcThread, 'TScanThread finished');
  AssertEquals('named by the end', 'Form1.Drv1', FLog[i].Source);
end;

procedure TTestScanLog.AnExceptionInTheScanIsPosted;
var
  i:Integer;
begin
  NewLog([lcScan], llError);
  NewDriver.RaiseInScanRead := true;
  i := WaitForEntry(lcScan, 'scan read raised');
  AssertTrue('the exception was posted'+FLog.Dump, i>=0);
  AssertEquals('error', PSLogLevelName(llError), PSLogLevelName(FLog[i].Level));
  AssertEquals('source', 'Form1.Drv1', FLog[i].Source);
  AssertTrue('with the message: '+FLog[i].Msg, Pos('the device answered nonsense', FLog[i].Msg)>0);
end;

procedure TTestScanLog.AWriteIsPostedWithItsValuesAndResult;
var
  T:TPLCTagNumber;
  i:Integer;
begin
  NewDriver;
  T := NewTag('Speed');
  NewLog([lcTag], llDebug);
  T.Value := 5;
  i := WaitForEntry(lcTag, 'scan write requested');
  AssertTrue('request posted'+FLog.Dump, i>=0);
  AssertEquals('with the value', 'scan write requested: [5]', FLog[i].Msg);
  AssertEquals('the tag is the source', 'Form1.Speed', FLog[i].Source);
  i := WaitForEntry(lcTag, 'scan write ok');
  AssertTrue('result posted'+FLog.Dump, i>=0);
  AssertEquals('debug', PSLogLevelName(llDebug), PSLogLevelName(FLog[i].Level));
end;

procedure TTestScanLog.AFailedWriteIsAWarning;
var
  T:TPLCTagNumber;
  i:Integer;
begin
  NewDriver.WriteResult := ioTimeOut;
  T := NewTag('Speed');
  NewLog([lcTag], llWarning);
  T.Value := 9;
  i := WaitForEntry(lcTag, 'scan write failed');
  AssertTrue('failure posted'+FLog.Dump, i>=0);
  AssertEquals('message', 'scan write failed: ioTimeOut', FLog[i].Msg);
  AssertEquals('warning', PSLogLevelName(llWarning), PSLogLevelName(FLog[i].Level));
  AssertEquals('the request is only Debug', -1, FLog.IndexOf(lcTag, 'scan write requested'));
end;

procedure TTestScanLog.AReadFailureIsPostedOnceAndTheRecoveryToo;
var
  T:TPLCTagNumber;
  i:Integer;
begin
  NewDriver.ReadResult := ioCommError;
  NewLog([lcTag], llInfo);
  T := NewTag('Level');
  i := WaitForEntry(lcTag, 'read failed');
  AssertTrue('failure posted'+FLog.Dump, i>=0);
  AssertEquals('message', 'read failed: ioCommError', FLog[i].Msg);
  AssertEquals('warning', PSLogLevelName(llWarning), PSLogLevelName(FLog[i].Level));
  //many more failed scans: still one message
  Pump(300);
  FLog.Flush;
  AssertEquals('only once'+FLog.Dump, 1, CountOf(lcTag, 'read failed'));

  FDriver.Value := 3;
  FDriver.ReadResult := ioOk;
  i := WaitForEntry(lcTag, 'reading again');
  AssertTrue('recovery posted'+FLog.Dump, i>=0);
  AssertEquals('info', PSLogLevelName(llInfo), PSLogLevelName(FLog[i].Level));
  Pump(200);
  FLog.Flush;
  AssertEquals('recovery only once'+FLog.Dump, 1, CountOf(lcTag, 'reading again'));
  AssertTrue('and the value arrived', T.Value=3);
end;

procedure TTestScanLog.EveryReadIsPostedAtTrace;
begin
  NewDriver;
  NewLog([lcTag], llTrace);
  NewTag('Level');
  AssertTrue('a read posted'+FLog.Dump, WaitForEntry(lcTag, 'scan read ioOk')>=0);
end;

procedure TTestScanLog.AValueChangeIsPostedWithTheValue;
var
  T:TPLCTagNumber;
  i:Integer;
begin
  NewDriver.Value := 42.5;
  NewLog([lcTag], llTrace);
  T := NewTag('Temp');
  AssertTrue('value arrived', WaitValue(T, 42.5));
  i := WaitForEntry(lcTag, 'value changed');
  AssertTrue('change posted'+FLog.Dump, i>=0);
  AssertEquals('with the value', 'value changed: '+FloatToStr(42.5), FLog[i].Msg);
  AssertEquals('source', 'Form1.Temp', FLog[i].Source);
end;

procedure TTestScanLog.TheEventsFiredArePosted;
var
  T:TPLCTagNumber;
begin
  NewDriver.Value := 1;
  NewLog([lcEvent], llTrace);
  T := NewTag('Motor');
  T.OnValueChangeFirst := @QuietHandler;
  T.OnValueChange := @QuietHandler;
  AssertTrue('value arrived', WaitValue(T, 1));
  AssertTrue('OnValueChangeFirst'+FLog.Dump, WaitForEntry(lcEvent, 'OnValueChangeFirst')>=0);
  AssertTrue('OnValueChange'+FLog.Dump, WaitForEntry(lcEvent, 'OnValueChange')>=0);
  AssertEquals('an event not assigned is not posted', -1, FLog.IndexOf(lcEvent, 'OnReadFail'));
end;

procedure TTestScanLog.AnExceptionInAHandlerIsPostedAndTheOtherTagsStillUpdate;
var
  A, B:TPLCTagNumber;
  i:Integer;
begin
  NewDriver.Value := 10;
  NewLog([lcEvent], llError);
  FRaiseInHandler := true;
  A := NewTag('TagA');
  A.OnValueChange := @RaisingHandler;
  B := NewTag('TagB');
  i := WaitForEntry(lcEvent, 'exception while updating the tag');
  AssertTrue('the exception was posted'+FLog.Dump, i>=0);
  AssertEquals('the tag is the source', 'Form1.TagA', FLog[i].Source);
  AssertTrue('with the message: '+FLog[i].Msg, Pos('bug in the screen code', FLog[i].Msg)>0);
  //the value goes on changing: B keeps following it
  FDriver.Value := 11;
  AssertTrue('the other tag is still updated', WaitValue(B, 11));
end;

//GetMultipleValues kept the interface of the last tag it looked at until its epilogue, after
//it had left the lock and paused 50 ms (no port): a tag destroyed meanwhile got its _Release
//on freed memory - an access violation swallowed by the update loop, which also leaked the
//values of the pass. The freed tag's memory is filled with garbage at once (the next block of
//its size usually is the same one), so a late _Release faults instead of reading stale bytes.
procedure TTestScanLog.ATagDestroyedWhileTheDriverWaitsIsNotTouchedAfterwards;
var
  i, size:Integer;
  T:TPLCTagNumber;
  junk:Pointer;
begin
  NewLog([lcScan, lcThread], llError);
  NewDriver.NoPort := true;
  for i:=1 to 20 do begin
    T := NewTag('Victim'+IntToStr(i));
    size := T.InstanceSize;
    Pump(5+(i mod 4)*10);         //a pass sees the tag, then the driver pauses
    T.Free;
    junk := GetMem(size);
    FillChar(junk^, size, $A5);
    Pump(70);                     //the pass ends (and its epilogue runs) on the garbage
    FreeMem(junk);
  end;
  AssertTrue('flush', FLog.Flush);
  AssertEquals('nothing raised in the update thread'+FLog.Dump, 0,
               CountOf(lcScan, 'tag update raised')+CountOf(lcThread, 'TScanUpdate stopped'));
end;

initialization
  RegisterTest(TTestScanLog);

end.
