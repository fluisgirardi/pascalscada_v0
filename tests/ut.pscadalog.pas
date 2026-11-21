{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do log do PascalSCADA: hub global, regras e TpSCADALogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os tempos sao folgados (o CI roda em ARM emulado): as assercoes esperam com
  Flush, nunca pelo relogio.
}
{$ELSE}
{:
  @abstract(PascalSCADA logging tests: global hub, rules and TpSCADALogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Timings are generous (CI runs on emulated ARM): the assertions wait with
  Flush, never on the clock.
}
{$ENDIF}
unit ut.pscadalog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  pscadalog;

type

  //: Screen target that just keeps the lines it got.
  TFakeLogDisplay = class(TComponent, IpSCADALogDisplay)
  public
    Lines:TStringList;
    Calls:Integer;
    WrongThread:Boolean;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure AddLogLines(ALines:TStrings);
  end;

  //: Sources of messages with a class hierarchy to match ClassMask against.
  TFakeLogBase = class(TComponent);
  TFakeLogDriverA = class(TFakeLogBase);
  TFakeLogDriverB = class(TFakeLogBase);
  TFakeLogOther = class(TComponent);

  { TFloodThread }

  TFloodThread = class(TThread)
  private
    FSource:TComponent;
    FCount:Integer;
    FTag:String;
  protected
    procedure Execute; override;
  public
    constructor Create(ASource:TComponent; ACount:Integer; const ATag:String);
  end;

  { TTestPSCADALog }

  TTestPSCADALog = class(TTestCase)
  private
    FDir:String;
    FOwner:TComponent;
    FEvents:TStringList;
    FEventEntries:array of TpSCADALogEntry;
    FEventWrongThread:Boolean;
    procedure OnLogEvent(Sender:TObject; const Entry:TpSCADALogEntry; const Line:String);
    function  NewLogger:TpSCADALogger;
    function  ReadLines(const AFileName:String):TStringList;
    function  CountLines(const AFileName:String):Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //funcoes auxiliares / helpers
    procedure MasksMatchWithStarAndQuestionIgnoringCase;
    procedure SourceNameFollowsTheOwnerChain;
    procedure SourceNameOfNonComponentsIsTheClassName;
    procedure HexDumpsTheBytes;

    //mascara global / global mask
    procedure WithNoLoggerNothingIsWanted;
    procedure TheMaskFollowsTheRules;
    procedure AnInactiveLoggerOpensNoCategory;
    procedure AnExclusionRuleDoesNotOpenTheMask;
    procedure ChangingARuleAtRunTimeUpdatesTheMask;

    //roteamento / routing
    procedure TheEventGetsTheEntryInTheMainThread;
    procedure TheDisplayGetsTheFormattedLines;
    procedure SourceMaskSelectsTheSource;
    procedure ClassMaskAlsoMatchesAncestors;
    procedure AnExclusionWithStopProcessingDropsTheMessage;
    procedure AMessageBelowMinLevelIsNotRouted;
    procedure CategoriesFilterTheMessages;
    procedure ADisabledRuleIsIgnored;
    procedure LineFormatIsApplied;
    procedure ThePayloadIsWrittenAsHex;
    procedure ThreadsAreNumberedInTheOrderTheyPost;

    //arquivos / files
    procedure TwoRulesOnTheSameFileWriteTheLineOnce;
    procedure FileNameMacrosSplitTheFilesBySource;
    procedure ARelativeFileNameIsRelativeToTheExecutable;
    procedure AFileTargetWithoutFileNameIsIgnored;
    procedure TheFileIsRotatedBySize;
    procedure AppendsToAnExistingFile;

    //threads e carga / threads and load
    procedure ManyThreadsLoseNoMessage;
    procedure AFullQueueDropsAndReportsIt;
    procedure DestroyingTheLoggerWhileThreadsPostIsSafe;
    procedure TwoLoggersBothGetTheMessages;
  end;

implementation

{ TFakeLogDisplay }

constructor TFakeLogDisplay.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Lines := TStringList.Create;
end;

destructor TFakeLogDisplay.Destroy;
begin
  Lines.Free;
  inherited Destroy;
end;

procedure TFakeLogDisplay.AddLogLines(ALines:TStrings);
begin
  if GetCurrentThreadId<>MainThreadID then WrongThread := true;
  Inc(Calls);
  Lines.AddStrings(ALines);
end;

{ TFloodThread }

constructor TFloodThread.Create(ASource:TComponent; ACount:Integer; const ATag:String);
begin
  FSource := ASource;
  FCount := ACount;
  FTag := ATag;
  FreeOnTerminate := false;
  inherited Create(false);
end;

procedure TFloodThread.Execute;
var
  i:Integer;
begin
  for i:=1 to FCount do begin
    if Terminated then exit;
    PSLog(FSource, lcComm, llInfo, FTag+' '+IntToStr(i));
  end;
end;

function SizeOfFile(const AFileName:String):Int64;
var
  sr:TSearchRec;
begin
  Result := -1;
  if FindFirst(AFileName, faAnyFile, sr)=0 then begin
    Result := sr.Size;
    FindClose(sr);
  end;
end;

{ TTestPSCADALog }

procedure TTestPSCADALog.SetUp;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir(false))+
          'pscadalog_'+IntToStr(GetProcessID)+'_'+IntToStr(Random(1000000));
  ForceDirectories(FDir);
  FDir := IncludeTrailingPathDelimiter(FDir);
  FOwner := TComponent.Create(nil);
  FOwner.Name := 'Form1';
  FEvents := TStringList.Create;
  FEventEntries := nil;
  FEventWrongThread := false;
end;

procedure TTestPSCADALog.TearDown;
var
  sr:TSearchRec;
begin
  FreeAndNil(FOwner);
  FreeAndNil(FEvents);
  if FindFirst(FDir+'*', faAnyFile, sr)=0 then begin
    repeat
      if (sr.Attr and faDirectory)=0 then DeleteFile(FDir+sr.Name);
    until FindNext(sr)<>0;
    FindClose(sr);
  end;
  RemoveDir(FDir);
end;

procedure TTestPSCADALog.OnLogEvent(Sender:TObject; const Entry:TpSCADALogEntry;
  const Line:String);
begin
  if GetCurrentThreadId<>MainThreadID then FEventWrongThread := true;
  FEvents.Add(Entry.Msg);
  if FEvents.Count>Length(FEventEntries) then
    SetLength(FEventEntries, 2*FEvents.Count+16);
  FEventEntries[FEvents.Count-1] := Entry;
end;

function TTestPSCADALog.NewLogger:TpSCADALogger;
begin
  Result := TpSCADALogger.Create(FOwner);
  Result.FlushIntervalMs := 20;
  Result.OnLog := @OnLogEvent;
end;

function TTestPSCADALog.ReadLines(const AFileName:String):TStringList;
var
  fs:TFileStream;
begin
  Result := TStringList.Create;
  if not FileExists(AFileName) then exit;
  //the logger keeps the file open for writing: LoadFromFile asks fmShareDenyWrite,
  //which Windows refuses while somebody writes (a log viewer must share it)
  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result.LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

function TTestPSCADALog.CountLines(const AFileName:String):Integer;
var
  sl:TStringList;
begin
  sl := ReadLines(AFileName);
  try
    Result := sl.Count;
  finally
    sl.Free;
  end;
end;

procedure TTestPSCADALog.MasksMatchWithStarAndQuestionIgnoringCase;
begin
  AssertTrue('star matches all', PSLogMatchMask('*', 'anything'));
  AssertTrue('star matches empty', PSLogMatchMask('*', ''));
  AssertTrue('prefix', PSLogMatchMask('ModBus*', 'ModBusTCP1'));
  AssertTrue('ignores case', PSLogMatchMask('modbus*', 'ModBusTCP1'));
  AssertTrue('middle star', PSLogMatchMask('Form1.*TCP*', 'Form1.ModBusTCP1'));
  AssertTrue('question mark', PSLogMatchMask('Port?', 'Port1'));
  AssertFalse('question needs a char', PSLogMatchMask('Port?', 'Port'));
  AssertFalse('no prefix match', PSLogMatchMask('S7*', 'ModBusTCP1'));
  AssertFalse('exact needs the whole string', PSLogMatchMask('Port', 'Port1'));
  AssertTrue('star then suffix', PSLogMatchMask('*Driver', 'TModBusDriver'));
  AssertFalse('suffix must match the end', PSLogMatchMask('*Driver', 'TModBusDriverX'));
end;

procedure TTestPSCADALog.SourceNameFollowsTheOwnerChain;
var
  dm, c, unnamed:TComponent;
begin
  dm := TComponent.Create(FOwner);
  dm.Name := 'DM';
  c := TComponent.Create(dm);
  c.Name := 'ModBusTCP1';
  unnamed := TComponent.Create(dm);
  AssertEquals('nested', 'Form1.DM.ModBusTCP1', PSLogSourceName(c));
  AssertEquals('root', 'Form1', PSLogSourceName(FOwner));
  AssertEquals('unnamed shows the class', 'Form1.DM.<TComponent>', PSLogSourceName(unnamed));
end;

procedure TTestPSCADALog.SourceNameOfNonComponentsIsTheClassName;
var
  o:TStringList;
begin
  o := TStringList.Create;
  try
    AssertEquals('class name', 'TStringList', PSLogSourceName(o));
    AssertEquals('nil', '', PSLogSourceName(nil));
  finally
    o.Free;
  end;
end;

procedure TTestPSCADALog.HexDumpsTheBytes;
begin
  AssertEquals('bytes', '00 01 AB FF', PSLogHex(TBytes.Create($00, $01, $AB, $FF)));
  AssertEquals('one', '7F', PSLogHex(TBytes.Create($7F)));
  AssertEquals('empty', '', PSLogHex(nil));
end;

procedure TTestPSCADALog.WithNoLoggerNothingIsWanted;
var
  c:TpSCADALogCategory;
begin
  for c:=Low(c) to High(c) do
    AssertFalse('category '+PSLogCategoryName(c), PSLogWants(c, llError));
end;

procedure TTestPSCADALog.TheMaskFollowsTheRules;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.Categories := [lcComm];
  R.MinLevel := llDebug;
  R.Targets := [ltEvent];
  AssertTrue('comm debug', PSLogWants(lcComm, llDebug));
  AssertTrue('comm error', PSLogWants(lcComm, llError));
  AssertFalse('comm trace', PSLogWants(lcComm, llTrace));
  AssertFalse('tag error', PSLogWants(lcTag, llError));
  L.Free;
  AssertFalse('after the logger is gone', PSLogWants(lcComm, llError));
end;

procedure TTestPSCADALog.AnInactiveLoggerOpensNoCategory;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.MinLevel := llTrace;
  R.Targets := [ltEvent];
  AssertTrue('active', PSLogWants(lcTag, llTrace));
  L.Active := false;
  AssertFalse('inactive', PSLogWants(lcTag, llError));
  L.Active := true;
  AssertTrue('active again', PSLogWants(lcTag, llTrace));
end;

procedure TTestPSCADALog.AnExclusionRuleDoesNotOpenTheMask;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.MinLevel := llTrace;
  R.Targets := [];
  R.StopProcessing := true;
  AssertFalse('only an exclusion', PSLogWants(lcComm, llError));
end;

procedure TTestPSCADALog.ChangingARuleAtRunTimeUpdatesTheMask;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.Targets := [ltEvent];
  R.MinLevel := llWarning;
  AssertFalse('info before', PSLogWants(lcScan, llInfo));
  R.MinLevel := llInfo;
  AssertTrue('info after', PSLogWants(lcScan, llInfo));
  R.Enabled := false;
  AssertFalse('disabled', PSLogWants(lcScan, llError));
end;

procedure TTestPSCADALog.TheEventGetsTheEntryInTheMainThread;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  src:TFakeLogDriverA;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.MinLevel := llDebug;
  R.Targets := [ltEvent];
  src := TFakeLogDriverA.Create(FOwner);
  src.Name := 'Drv1';
  PSLog(src, lcConnection, llWarning, 'timeout');
  AssertTrue('flush', L.Flush);
  AssertEquals('one event', 1, FEvents.Count);
  AssertEquals('msg', 'timeout', FEventEntries[0].Msg);
  AssertEquals('source', 'Form1.Drv1', FEventEntries[0].Source);
  AssertTrue('class', FEventEntries[0].SourceClass=TFakeLogDriverA);
  AssertTrue('category', FEventEntries[0].Category=lcConnection);
  AssertTrue('level', FEventEntries[0].Level=llWarning);
  AssertFalse('called in the main thread', FEventWrongThread);
end;

procedure TTestPSCADALog.TheDisplayGetsTheFormattedLines;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  D:TFakeLogDisplay;
  i:Integer;
begin
  D := TFakeLogDisplay.Create(FOwner);
  L := NewLogger;
  L.Display := D;
  L.LineFormat := '%level%|%msg%';
  R := L.Rules.Add;
  R.Targets := [ltScreen];
  for i:=1 to 50 do
    PSLog(nil, lcUser, llInfo, 'm'+IntToStr(i));
  AssertTrue('flush', L.Flush);
  AssertEquals('lines', 50, D.Lines.Count);
  AssertEquals('first', 'INFO |m1', D.Lines[0]);
  AssertEquals('last', 'INFO |m50', D.Lines[49]);
  AssertTrue('in batches', D.Calls<50);
  AssertFalse('main thread', D.WrongThread);
  AssertEquals('screen only: no event', 0, FEvents.Count);
end;

procedure TTestPSCADALog.SourceMaskSelectsTheSource;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  a, b:TFakeLogDriverA;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.SourceMask := 'Form1.ModBus*; Form1.Other';
  R.Targets := [ltEvent];
  a := TFakeLogDriverA.Create(FOwner); a.Name := 'ModBusTCP1';
  b := TFakeLogDriverA.Create(FOwner); b.Name := 'S7TCP1';
  PSLog(a, lcComm, llInfo, 'from a');
  PSLog(b, lcComm, llInfo, 'from b');
  AssertTrue('flush', L.Flush);
  AssertEquals('only a', 'from a', FEvents.Text.Trim);
end;

procedure TTestPSCADALog.ClassMaskAlsoMatchesAncestors;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  a:TFakeLogDriverA;
  b:TFakeLogDriverB;
  o:TFakeLogOther;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.ClassMask := 'TFakeLogBase';
  R.Targets := [ltEvent];
  a := TFakeLogDriverA.Create(FOwner);
  b := TFakeLogDriverB.Create(FOwner);
  o := TFakeLogOther.Create(FOwner);
  PSLog(a, lcComm, llInfo, 'a');
  PSLog(b, lcComm, llInfo, 'b');
  PSLog(o, lcComm, llInfo, 'o');
  PSLog(nil, lcComm, llInfo, 'nil');
  AssertTrue('flush', L.Flush);
  AssertEquals('descendants only', 2, FEvents.Count);
  AssertEquals('a', 'a', FEvents[0]);
  AssertEquals('b', 'b', FEvents[1]);
end;

procedure TTestPSCADALog.AnExclusionWithStopProcessingDropsTheMessage;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  a:TFakeLogDriverA;
  b:TFakeLogDriverB;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.ClassMask := 'TFakeLogDriverB';
  R.Targets := [];
  R.StopProcessing := true;
  R := L.Rules.Add;
  R.Targets := [ltEvent];
  a := TFakeLogDriverA.Create(FOwner);
  b := TFakeLogDriverB.Create(FOwner);
  PSLog(b, lcComm, llError, 'excluded');
  PSLog(a, lcComm, llError, 'kept');
  AssertTrue('flush', L.Flush);
  AssertEquals('only kept', 'kept', FEvents.Text.Trim);
end;

procedure TTestPSCADALog.AMessageBelowMinLevelIsNotRouted;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := NewLogger;
  //the mask lets debug through because of the second rule, but the first
  //rule (the one with the event) only wants warnings
  R := L.Rules.Add;
  R.MinLevel := llWarning;
  R.Targets := [ltEvent];
  R := L.Rules.Add;
  R.MinLevel := llDebug;
  R.Targets := [ltScreen];
  PSLog(nil, lcUser, llDebug, 'debug');
  PSLog(nil, lcUser, llWarning, 'warning');
  AssertTrue('flush', L.Flush);
  AssertEquals('warning only', 'warning', FEvents.Text.Trim);
end;

procedure TTestPSCADALog.CategoriesFilterTheMessages;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.Categories := [lcTag, lcEvent];
  R.Targets := [ltEvent];
  PSLog(nil, lcTag, llInfo, 'tag');
  PSLog(nil, lcComm, llInfo, 'comm');
  PSLog(nil, lcEvent, llInfo, 'event');
  AssertTrue('flush', L.Flush);
  AssertEquals('two', 2, FEvents.Count);
  AssertEquals('tag', 'tag', FEvents[0]);
  AssertEquals('event', 'event', FEvents[1]);
end;

procedure TTestPSCADALog.ADisabledRuleIsIgnored;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.Targets := [];
  R.StopProcessing := true;
  R.Enabled := false;
  R := L.Rules.Add;
  R.Targets := [ltEvent];
  PSLog(nil, lcUser, llInfo, 'passes');
  AssertTrue('flush', L.Flush);
  AssertEquals('the disabled exclusion does not drop it', 'passes', FEvents.Text.Trim);
end;

procedure TTestPSCADALog.LineFormatIsApplied;
var
  L:TpSCADALogger;
  E:TpSCADALogEntry;
  src:TFakeLogDriverA;
begin
  L := NewLogger;
  src := TFakeLogDriverA.Create(FOwner);
  src.Name := 'Drv';
  E := Default(TpSCADALogEntry);
  E.Timestamp := EncodeDate(2026, 11, 17)+EncodeTime(14, 3, 22, 417);
  E.ThreadID := TThreadID(1234);
  E.ThreadNo := 3;
  E.Category := lcConnection;
  E.Level := llWarning;
  E.Source := PSLogSourceName(src);
  E.SourceClass := src.ClassType;
  E.Msg := 'timeout %data%';
  AssertEquals('default format',
    '2026-11-17 14:03:22.417 [WARN ] [connection] T3 Form1.Drv (TFakeLogDriverA): timeout %data%',
    L.FormatEntry(E));
  L.LineFormat := '%thread%/%threadid%';
  AssertEquals('thread number and id', 'T3/1234', L.FormatEntry(E));
  E.ThreadNo := 0;
  AssertEquals('main thread', 'main/1234', L.FormatEntry(E));
  L.LineFormat := '%date% %category%/%level% %msg%';
  AssertEquals('custom format', '2026-11-17 connection/WARN  timeout %data%', L.FormatEntry(E));
  L.LineFormat := '100%% %MSG% %unknown% 50%';
  AssertEquals('literal percent signs and case', '100%% timeout %data% %unknown% 50%', L.FormatEntry(E));
  L.LineFormat := '';
  AssertEquals('empty goes back to the default', PSLogDefaultLineFormat, L.LineFormat);
end;

procedure TTestPSCADALog.ThePayloadIsWrittenAsHex;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  D:TFakeLogDisplay;
begin
  D := TFakeLogDisplay.Create(FOwner);
  L := NewLogger;
  L.Display := D;
  L.LineFormat := '%msg%%data%';
  R := L.Rules.Add;
  R.MinLevel := llTrace;
  R.Targets := [ltScreen];
  PSLog(nil, lcComm, llTrace, 'TX', TBytes.Create($00, $01, $FE));
  AssertTrue('flush', L.Flush);
  AssertEquals('hex', 'TX | 00 01 FE', D.Lines.Text.Trim);
end;

procedure TTestPSCADALog.ThreadsAreNumberedInTheOrderTheyPost;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  T1, T2:TFloodThread;
  i, n1, n2:Integer;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.Targets := [ltEvent];
  PSLog(nil, lcUser, llInfo, 'main');
  T1 := TFloodThread.Create(nil, 2, 'A');
  T1.WaitFor; T1.Free;
  T2 := TFloodThread.Create(nil, 2, 'B');
  T2.WaitFor; T2.Free;
  AssertTrue('flush', L.Flush);
  AssertEquals('events', 5, FEvents.Count);
  AssertEquals('main thread is 0', 0, FEventEntries[0].ThreadNo);
  n1 := FEventEntries[1].ThreadNo;
  n2 := FEventEntries[3].ThreadNo;
  AssertTrue('a thread gets a number', n1>0);
  AssertEquals('same thread, same number', n1, FEventEntries[2].ThreadNo);
  //the system may give the second thread the id of the first one, which ended
  if FEventEntries[3].ThreadID<>FEventEntries[1].ThreadID then
    AssertTrue('another thread, another number', n2<>n1);
  for i:=0 to FEvents.Count-1 do
    AssertTrue('never negative for a posted message', FEventEntries[i].ThreadNo>=0);
end;

procedure TTestPSCADALog.TwoRulesOnTheSameFileWriteTheLineOnce;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.Targets := [ltFile];
  R.FileName := FDir+'same.log';
  R := L.Rules.Add;
  R.Targets := [ltFile];
  R.FileName := FDir+'same.log';
  PSLog(nil, lcUser, llInfo, 'once');
  AssertTrue('flush', L.Flush);
  AssertEquals('one line', 1, CountLines(FDir+'same.log'));
end;

procedure TTestPSCADALog.FileNameMacrosSplitTheFilesBySource;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  a, b:TFakeLogDriverA;
  today:String;
  sl:TStringList;
begin
  L := NewLogger;
  L.LineFormat := '%msg%';
  R := L.Rules.Add;
  R.Targets := [ltFile];
  R.FileName := FDir+'%source%_%class%_%category%_%date%.log';
  a := TFakeLogDriverA.Create(FOwner); a.Name := 'A';
  b := TFakeLogDriverA.Create(FOwner); b.Name := 'B';
  today := FormatDateTime('yyyy-mm-dd', Now);
  PSLog(a, lcComm, llInfo, 'a1');
  PSLog(b, lcComm, llInfo, 'b1');
  PSLog(a, lcComm, llInfo, 'a2');
  AssertTrue('flush', L.Flush);
  sl := ReadLines(FDir+'Form1.A_TFakeLogDriverA_comm_'+today+'.log');
  try
    AssertEquals('file of a', 'a1'+LineEnding+'a2'+LineEnding, sl.Text);
  finally
    sl.Free;
  end;
  sl := ReadLines(FDir+'Form1.B_TFakeLogDriverA_comm_'+today+'.log');
  try
    AssertEquals('file of b', 'b1'+LineEnding, sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TTestPSCADALog.ARelativeFileNameIsRelativeToTheExecutable;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  rel, full:String;
begin
  rel := 'pscadalog_rel_'+IntToStr(GetProcessID)+'.log';
  full := ExtractFilePath(ParamStr(0))+rel;
  DeleteFile(full);
  try
    L := NewLogger;
    R := L.Rules.Add;
    R.Targets := [ltFile];
    R.FileName := rel;
    PSLog(nil, lcUser, llInfo, 'rel');
    AssertTrue('flush', L.Flush);
    L.Free; //closes the file
    AssertEquals('next to the executable', 1, CountLines(full));
  finally
    DeleteFile(full);
  end;
end;

procedure TTestPSCADALog.AFileTargetWithoutFileNameIsIgnored;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.Targets := [ltFile];
  AssertFalse('no file name, nothing wanted', PSLogWants(lcUser, llError));
  R.Targets := [ltFile, ltEvent];
  PSLog(nil, lcUser, llInfo, 'event still works');
  AssertTrue('flush', L.Flush);
  AssertEquals('event', 1, FEvents.Count);
end;

procedure TTestPSCADALog.TheFileIsRotatedBySize;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  i:Integer;
begin
  L := NewLogger;
  L.LineFormat := '%msg%';
  L.MaxFileSize := 1000;
  L.MaxRotatedFiles := 2;
  R := L.Rules.Add;
  R.Targets := [ltFile];
  R.FileName := FDir+'rot.log';
  //each flush is a batch: the size is checked per batch
  for i:=1 to 40 do begin
    PSLog(nil, lcUser, llInfo, StringOfChar('x', 99));
    AssertTrue('flush', L.Flush);
  end;
  L.Free;
  AssertTrue('current', FileExists(FDir+'rot.log'));
  AssertTrue('.1', FileExists(FDir+'rot.log.1'));
  AssertTrue('.2', FileExists(FDir+'rot.log.2'));
  AssertFalse('no .3', FileExists(FDir+'rot.log.3'));
  AssertTrue('current under the limit', SizeOfFile(FDir+'rot.log')<=1000);
  AssertTrue('.1 under the limit', SizeOfFile(FDir+'rot.log.1')<=1000);
end;

procedure TTestPSCADALog.AppendsToAnExistingFile;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  sl:TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('old line');
    sl.SaveToFile(FDir+'app.log');
  finally
    sl.Free;
  end;
  L := NewLogger;
  L.LineFormat := '%msg%';
  R := L.Rules.Add;
  R.Targets := [ltFile];
  R.FileName := FDir+'app.log';
  PSLog(nil, lcUser, llInfo, 'new line');
  AssertTrue('flush', L.Flush);
  L.Free;
  sl := ReadLines(FDir+'app.log');
  try
    AssertEquals('both', 'old line'+LineEnding+'new line'+LineEnding, sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TTestPSCADALog.ManyThreadsLoseNoMessage;
const
  NThreads = 4;
  PerThread = 3000;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  T:array[0..NThreads-1] of TFloodThread;
  i, j, last, n:Integer;
  sl:TStringList;
  tag:String;
begin
  L := NewLogger;
  L.LineFormat := '%msg%';
  R := L.Rules.Add;
  R.Targets := [ltFile];
  R.FileName := FDir+'mt.log';
  for i:=0 to NThreads-1 do
    T[i] := TFloodThread.Create(FOwner, PerThread, 'T'+IntToStr(i));
  for i:=0 to NThreads-1 do begin
    T[i].WaitFor;
    T[i].Free;
  end;
  AssertTrue('flush', L.Flush(20000));
  AssertEquals('nothing dropped', 0, L.Dropped);
  sl := ReadLines(FDir+'mt.log');
  try
    AssertEquals('every line', NThreads*PerThread, sl.Count);
    //per thread the order is kept
    for i:=0 to NThreads-1 do begin
      tag := 'T'+IntToStr(i)+' ';
      last := 0;
      for j:=0 to sl.Count-1 do
        if Copy(sl[j], 1, Length(tag))=tag then begin
          n := StrToInt(Copy(sl[j], Length(tag)+1, MaxInt));
          AssertEquals('order of '+tag, last+1, n);
          last := n;
        end;
      AssertEquals('count of '+tag, PerThread, last);
    end;
  finally
    sl.Free;
  end;
end;

procedure TTestPSCADALog.AFullQueueDropsAndReportsIt;
const
  Total = 2000;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  i, written:Integer;
  warned:Boolean;
begin
  L := NewLogger;
  L.MaxQueue := 10;
  L.FlushIntervalMs := 1000;
  R := L.Rules.Add;
  R.Targets := [ltEvent];
  for i:=1 to Total do
    PSLog(nil, lcComm, llInfo, 'm');
  //one more message carries the "dropped" warning out
  PSLog(nil, lcComm, llInfo, 'm');
  AssertTrue('flush', L.Flush);
  written := 0;
  warned := false;
  for i:=0 to FEvents.Count-1 do
    if FEvents[i]='m' then
      Inc(written)
    else if Pos('dropped', FEvents[i])>0 then
      warned := true;
  AssertTrue('something was dropped', L.Dropped>0);
  AssertEquals('written + dropped', Total+1, written+Integer(L.Dropped));
  AssertTrue('the drop is reported', warned);
end;

procedure TTestPSCADALog.DestroyingTheLoggerWhileThreadsPostIsSafe;
var
  L:TpSCADALogger;
  R:TpSCADALogRule;
  T:array[0..2] of TFloodThread;
  i:Integer;
begin
  L := NewLogger;
  R := L.Rules.Add;
  R.Targets := [ltFile, ltEvent];
  R.FileName := FDir+'race.log';
  for i:=0 to High(T) do
    T[i] := TFloodThread.Create(FOwner, 200000, 'T');
  Sleep(30);
  FEvents.Clear;
  L.Free;
  AssertEquals('no OnLog while the logger is being destroyed', 0, FEvents.Count);
  AssertFalse('nothing wanted after the logger is gone', PSLogWants(lcComm, llError));
  for i:=0 to High(T) do begin
    T[i].WaitFor;
    T[i].Free;
  end;
  //events queued for the UI were discarded with the logger
  CheckSynchronize(0);
end;

procedure TTestPSCADALog.TwoLoggersBothGetTheMessages;
var
  L1, L2:TpSCADALogger;
  R:TpSCADALogRule;
begin
  L1 := NewLogger;
  R := L1.Rules.Add;
  R.Targets := [ltEvent];
  L2 := NewLogger;
  R := L2.Rules.Add;
  R.Targets := [ltEvent];
  PSLog(nil, lcUser, llInfo, 'both');
  AssertTrue('flush', PSLogFlushAll);
  AssertEquals('two events', 2, FEvents.Count);
end;

initialization
  RegisterTest(TTestPSCADALog);

end.
