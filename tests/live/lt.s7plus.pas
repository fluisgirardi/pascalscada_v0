{$i ../../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  Testes AO VIVO do driver S7CommPlus: falam com um PLC (ou PLCSim) de verdade.

  Precisam dos DBs de tests/plc (TestDB_Opt, TestDB_NonOpt, TestDB_Big, TestDB_Dyn, as FCs
  FC_TestDynamics e FC_TestPattern chamadas no OB1) baixados no PLC e em RUN. Ficam FORA da
  suite comum (pascalscada_tests), que nao depende de rede: este projeto e'
  tests/live/pascalscada_live_tests.

  Variaveis de ambiente (nada de senha no repositorio):
    S7PLUS_HOST       IP do PLC; sem ele todos os testes sao ignorados
    S7PLUS_PORT       porta (102)
    S7PLUS_USER       usuario da legitimacao (opcional)
    S7PLUS_PASSWORD   senha da legitimacao (opcional)
    S7PLUS_DEBUG      1: mostra as mensagens do driver

  Os testes que escrevem devolvem o valor que encontraram; o ultimo (ZZ) pede ao CLP que
  recarregue os valores por elemento ("TestDB_Dyn".LoadPattern).
}
{$ELSE}
{:
  LIVE tests of the S7CommPlus driver: they talk to a real PLC (or PLCSim).

  They need the DBs of tests/plc (TestDB_Opt, TestDB_NonOpt, TestDB_Big, TestDB_Dyn, and the FCs
  FC_TestDynamics and FC_TestPattern called in OB1) downloaded to the PLC, in RUN. They stay OUT of
  the common suite (pascalscada_tests), which needs no network: this project is
  tests/live/pascalscada_live_tests.

  Environment variables (no password in the repository):
    S7PLUS_HOST       IP of the PLC; without it every test is ignored
    S7PLUS_PORT       port (102)
    S7PLUS_USER       legitimation user (optional)
    S7PLUS_PASSWORD   legitimation password (optional)
    S7PLUS_DEBUG      1: show the driver's own messages

  The tests that write put back the value they found; the last one (ZZ) asks the PLC to reload the
  per element values ("TestDB_Dyn".LoadPattern).
}
{$ENDIF}
unit lt.s7plus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, fpcunit, testregistry,
  Tag, ProtocolTypes, PLCTagNumber, PLCString, PLCBlock, PLCBlockElement, PLCStruct, PLCStructElement, PLCStructString,
  tcp_udpport, S7PlusFamily, S7PlusConnection, S7PlusTypeInfo, S7PlusTypes, S7PlusTagPlan;

type
  { TLiveCase }

  //Base of the live tests: ignored when no PLC is configured; frees what the test created.
  TLiveCase = class(TTestCase)
  private
    FOwned:TList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function Own(AComp:TComponent):TComponent;
    procedure Pump(Ms:Integer);
  end;

  { TTestLiveConnection }

  TTestLiveConnection = class(TLiveCase)
  published
    procedure TheDriverLogsInAndFindsTheTestDBs;
    procedure AuthWithTheSameCredentialsIsOk;
    procedure ListTagsDescribesTheTestDBs;
    procedure RepeatedListsOfTheDBsAreNoChange;
  end;

  { TTestLiveReads }

  TTestLiveReads = class(TLiveCase)
  published
    procedure EveryStartValueOfTheOptimizedDB;
    procedure EveryStartValueOfTheNonOptimizedDB;
    procedure TheBigDBHoldsThePattern;
    procedure TheDynamicDBChangesByItself;
    procedure ABatchOfBigValuesDoesNotKillTheSession;
  end;

  { TTestLiveWrites }

  TTestLiveWrites = class(TLiveCase)
  published
    procedure ScalarsOfEverySizeComeBackAsWritten;
    procedure StringsComeBackAsWritten;
    procedure MembersInStructuresAndArraysOfStructuresComeBackAsWritten;
    procedure ArrayElementsComeBackAsWritten;
    procedure TheSameOnTheNonOptimizedDB;
    procedure ThePLCEchoesWhatWasWritten;
    procedure TheResetCommandZeroesTheCounter;
    procedure DisablingFreezesTheCounter;
  end;

  { TTestLiveTags }

  TTestLiveTags = class(TLiveCase)
  published
    procedure AWholeDBFromTheImportPlanReadsEveryStartValue;
    procedure TheSameOnChange;
    procedure ABigWholeDBIsReadInSeveralRequests;
    procedure ANumberTagWritesAndReadsBack;
    procedure AStringTagWritesAndReadsBack;
    procedure AnOnChangeTagFollowsTheCounter;
    procedure AWholeDBStructureWritesAnItem;
    procedure AnArrayElementTagWritesOnlyItsElement;
  end;

  { TTestLiveReconnect }

  TTestLiveReconnect = class(TLiveCase)
  published
    procedure TheTagsComeBackAfterThePortIsClosedAndOpened;
  end;

  { TTestLiveCertificate }

  TTestLiveCertificate = class(TLiveCase)
  private
    function Connects(const ACertificate:AnsiString):Boolean;
  published
    procedure ThePlcSendsACertificate;
    procedure TheCertificateOfThePlcIsAccepted;
    procedure AnotherCertificateIsRefused;
    procedure AnInvalidValueFailsClosed;
  end;

  { TTestLiveZZRestore }

  TTestLiveZZRestore = class(TLiveCase)
  published
    procedure ZZLoadPatternRestoresThePerElementValues;
  end;

implementation

var
  GPort:TTCP_UDPPort;
  GDriver:TS7CommPlusDriver;
  GSink:TObject;
  GChanges:Integer;                 //how many times the driver said the program changed
  GBlocks:TS7PlusDataBlockInfoArray;
  GBrowsed:array of record
    Name:String;
    Number:Integer;
    Vars:TS7PlusVarInfoArray;
  end;
  GHost, GUser, GPassword:String;
  GPortNumber:Integer;
  GTried:Boolean;
  GReady:Boolean;
  GVerbose:Boolean;

type
  TSink = class
    procedure Debug(Sender:TObject; const Msg:String);
  end;

procedure TSink.Debug(Sender:TObject; const Msg:String);
begin
  if Pos('CheckProgramNow: o programa do CLP mudou', Msg)>0 then
    Inc(GChanges);
  //S7PLUS_DEBUG=1: the driver's own messages (not the TPKT/TLS noise) on the screen
  if GVerbose and (Pos('TPKT', Msg)=0) and (Pos('SSL_read', Msg)=0) and (Pos('COTP', Msg)=0) and
     (Pos('LogicalRecv', Msg)=0) and (Pos('Reassembled', Msg)=0) and (Pos('CreditTick', Msg)=0) and
     (Pos('SendRequest: IntegrityId', Msg)=0) and (Pos('respPayload', Msg)=0) and (Pos('functionCode', Msg)=0) then
    WriteLn(' [driver] ', Copy(Msg, 1, 200));
end;

function ConfiguredHost:String;
begin
  Result := GetEnvironmentVariable('S7PLUS_HOST');
end;

procedure ReadEnvironment;
begin
  GHost := ConfiguredHost;
  GUser := GetEnvironmentVariable('S7PLUS_USER');
  GPassword := GetEnvironmentVariable('S7PLUS_PASSWORD');
  GPortNumber := StrToIntDef(GetEnvironmentVariable('S7PLUS_PORT'), 102);
  GVerbose := GetEnvironmentVariable('S7PLUS_DEBUG')='1';
end;

procedure OpenPort(var APort:TTCP_UDPPort; var ADriver:TS7CommPlusDriver);
var
  w:Integer;
begin
  APort := TTCP_UDPPort.Create(nil);
  ADriver := TS7CommPlusDriver.Create(nil);
  APort.Host := GHost;
  APort.Port := GPortNumber;
  APort.Timeout := 8000;
  APort.EnableAutoReconnect := true;
  ADriver.CommunicationPort := APort;
  if GUser<>'' then begin
    ADriver.Username := GUser;
    ADriver.Password := GPassword;
  end;
  APort.Active := true;
  w := 0;
  while (not APort.ReallyActive) and (w<6000) do begin
    Sleep(100);
    Inc(w, 100);
  end;
end;

//One session shared by the tests that do not need their own.
function LiveReady:Boolean;
begin
  if GTried then begin
    Result := GReady;
    exit;
  end;
  GTried := true;
  ReadEnvironment;
  if GHost='' then exit(false);
  OpenPort(GPort, GDriver);
  GSink := TSink.Create;
  GDriver.OnDebug := @TSink(GSink).Debug;
  GReady := GPort.ReallyActive and (GDriver.ListDataBlocks(GBlocks)=ioOk);
  Result := GReady;
end;

function DbNumberOf(const AName:String):Integer;
var
  i:Integer;
begin
  Result := -1;
  for i:=0 to High(GBlocks) do
    if SameText(GBlocks[i].Name, AName) then
      Result := GBlocks[i].Number;
end;

function DbVars(const AName:String; out Number:Integer):TS7PlusVarInfoArray;
var
  i, n:Integer;
begin
  Number := DbNumberOf(AName);
  SetLength(Result, 0);
  if Number<0 then exit;
  for i:=0 to High(GBrowsed) do
    if GBrowsed[i].Number=Number then begin
      Result := GBrowsed[i].Vars;
      exit;
    end;
  if GDriver.BrowseDB(Number, Result)<>ioOk then exit;
  n := Length(GBrowsed);
  SetLength(GBrowsed, n+1);
  GBrowsed[n].Name := AName;
  GBrowsed[n].Number := Number;
  GBrowsed[n].Vars := Result;
end;

function VarOf(const ADb, AMember:String; out V:TS7PlusVarInfo; out Number:Integer):Boolean;
var
  Vars:TS7PlusVarInfoArray;
  i:Integer;
  Full:String;
begin
  Result := false;
  Vars := DbVars(ADb, Number);
  Full := 'DB'+IntToStr(Number)+'.'+AMember;
  for i:=0 to High(Vars) do
    if SameText(Vars[i].Name, Full) then begin
      V := Vars[i];
      Result := true;
      exit;
    end;
end;

function RawOf(const ADb, AMember:String; out B:TBytes):Boolean;
var
  V:TS7PlusVarInfo;
  N:Integer;
begin
  SetLength(B, 0);
  Result := VarOf(ADb, AMember, V, N) and
            (GDriver.ReadSymbolic(S7PlusIds_DBAccessAreaBase+Cardinal(N), V.Lids, B)=ioOk);
end;

function WriteRaw(const ADb, AMember:String; const Data:TBytes):Boolean;
var
  V:TS7PlusVarInfo;
  N:Integer;
begin
  Result := VarOf(ADb, AMember, V, N) and
            (GDriver.WriteSymbolic(S7PlusIds_DBAccessAreaBase+Cardinal(N), V.Lids, Data, V.SoftDataType)=ioOk);
end;

function BEBytes(Value:Int64; Size:Integer):TBytes;
var
  i:Integer;
  u:QWord;
begin
  SetLength(Result, Size);
  u := QWord(Value);
  for i:=Size-1 downto 0 do begin
    Result[i] := u and $FF;
    u := u shr 8;
  end;
end;

function SingleBytes(Value:Single):TBytes;
var
  u:LongWord absolute Value;
begin
  Result := BEBytes(u, 4);
end;

function DoubleBytes(Value:Double):TBytes;
var
  u:QWord absolute Value;
begin
  Result := BEBytes(Int64(u), 8);
end;

function StringBytes(const Text:String; MaxLen:Integer):TBytes;
var
  i:Integer;
begin
  SetLength(Result, MaxLen+2);
  FillChar(Result[0], Length(Result), 0);
  Result[0] := MaxLen;
  Result[1] := Length(Text);
  for i:=1 to Length(Text) do
    Result[1+i] := Ord(Text[i]);
end;

function BEValue(const B:TBytes; Signed:Boolean):Int64;
var
  i:Integer;
  u:QWord;
begin
  u := 0;
  for i:=0 to High(B) do
    u := (u shl 8) or B[i];
  Result := Int64(u);
  if Signed and (Length(B)<8) and (Length(B)>0) and ((B[0] and $80)<>0) then
    Result := Int64(u)-(Int64(1) shl (8*Length(B)));
end;

function SingleOf(const B:TBytes):Double;
var
  u:LongWord;
  s:Single absolute u;
begin
  u := LongWord(BEValue(B, false));
  Result := s;
end;

function DoubleOf(const B:TBytes):Double;
var
  u:QWord;
  d:Double absolute u;
begin
  u := QWord(BEValue(B, false));
  Result := d;
end;

function TextOf(const B:TBytes):String;
var
  k:Integer;
begin
  Result := '';
  if Length(B)<2 then exit;
  for k:=2 to 1+B[1] do
    if k<Length(B) then
      Result := Result+Chr(B[k]);
end;

//--- start value checks -------------------------------------------------------------------

type
  TChecker = class
  public
    Db:String;
    Failures:TStringList;
    Count:Integer;
    constructor Create(const ADb:String);
    destructor Destroy; override;
    procedure Int(const M:String; Expected:Int64; Signed:Boolean = true);
    procedure UInt(const M:String; Expected:QWord);
    procedure Real_(const M:String; Expected:Double; IsDouble:Boolean = false);
    procedure Text(const M, Expected:String);
    function Report:String;
  end;

constructor TChecker.Create(const ADb:String);
begin
  Db := ADb;
  Failures := TStringList.Create;
end;

destructor TChecker.Destroy;
begin
  Failures.Free;
  inherited Destroy;
end;

procedure TChecker.Int(const M:String; Expected:Int64; Signed:Boolean);
var
  B:TBytes;
  g:Int64;
begin
  Inc(Count);
  if not RawOf(Db, M, B) then begin
    Failures.Add(Db+'.'+M+': not read');
    exit;
  end;
  g := BEValue(B, Signed);
  if g<>Expected then
    Failures.Add(Format('%s.%s: read %d, expected %d', [Db, M, g, Expected]));
end;

procedure TChecker.UInt(const M:String; Expected:QWord);
var
  B:TBytes;
  g:QWord;
begin
  Inc(Count);
  if not RawOf(Db, M, B) then begin
    Failures.Add(Db+'.'+M+': not read');
    exit;
  end;
  g := QWord(BEValue(B, false));
  if g<>Expected then
    Failures.Add(Format('%s.%s: read %u, expected %u', [Db, M, g, Expected]));
end;

procedure TChecker.Real_(const M:String; Expected:Double; IsDouble:Boolean);
var
  B:TBytes;
  g:Double;
begin
  Inc(Count);
  if not RawOf(Db, M, B) then begin
    Failures.Add(Db+'.'+M+': not read');
    exit;
  end;
  if IsDouble then g := DoubleOf(B) else g := SingleOf(B);
  if not SameValue(g, Expected, 0.0001) then
    Failures.Add(Format('%s.%s: read %g, expected %g', [Db, M, g, Expected]));
end;

procedure TChecker.Text(const M, Expected:String);
var
  B:TBytes;
begin
  Inc(Count);
  if not RawOf(Db, M, B) then begin
    Failures.Add(Db+'.'+M+': not read');
    exit;
  end;
  if TextOf(B)<>Expected then
    Failures.Add(Format('%s.%s: read "%s", expected "%s"', [Db, M, TextOf(B), Expected]));
end;

function TChecker.Report:String;
begin
  Result := Format('%d of %d values differ:', [Failures.Count, Count])+LineEnding+Failures.Text;
end;

//Every start value of tests/plc/TestDB_Opt.db (and TestDB_NonOpt.db, the same), with the
//per element values FC_TestPattern loads.
procedure CheckStartValues(C:TChecker);
var
  k:Integer;
  s:String;
begin
  C.Int('Bool_1', 1); C.Int('Bool_2', 0); C.Int('Bool_3', 1); C.Int('Bool_4', 1);
  C.Int('Byte_1', $12, false); C.Int('Char_1', 65, false); C.Int('SInt_1', -5); C.Int('USInt_1', 200, false);
  C.Int('Word_1', $1234, false); C.Int('Int_1', -1234); C.Int('UInt_1', 60000, false);
  C.UInt('DWord_1', $DEADBEEF); C.Int('DInt_1', -123456); C.UInt('UDInt_1', 4000000000);
  C.Real_('Real_1', 3.14);
  C.UInt('LWord_1', QWord($0123456789ABCDEF)); C.Int('LInt_1', -1234567890123); C.UInt('ULInt_1', 12345678901234);
  C.Real_('LReal_1', 2.718281828, true);
  C.Text('String_Short', 'hello'); C.Text('String_Mid', 'middle string value');
  C.Text('String_Default', 'default text'); C.Text('String_Empty', '');
  for k:=0 to 9 do C.Int(Format('Arr_DInt[%d]', [k]), 100+k);
  for k:=0 to 4 do C.Real_(Format('Arr_Real[%d]', [k]), 0.5+k);
  for k:=0 to 15 do C.Int(Format('Arr_Byte[%d]', [k]), k, false);
  for k:=0 to 7 do C.Int(Format('Arr_Bool[%d]', [k]), 1-(k mod 2));
  for k:=-2 to 2 do C.Int(Format('Arr_NegLow[%d]', [k]), k*10);
  C.Real_('Arr_LReal[0]', 1.25, true); C.Real_('Arr_LReal[1]', 2.5, true);
  for k:=0 to 99 do C.Int(Format('Arr_Word100[%d]', [k]), 1000+k, false);
  for k:=0 to 2 do begin s := 'str'+IntToStr(k); C.Text(Format('Arr_Str[%d]', [k]), s); end;
  C.Int('Mix.v1', $11, false); C.Int('Mix.v2', $2222, false); C.UInt('Mix.v3', $33333333); C.Int('Mix.v4', 1);
  C.Real_('Mix.Static_1', 1.5); C.Int('Mix.Static_2', 0); C.Int('Mix.Static_3', 1); C.Int('Mix.Static_4', 0);
  C.Text('Mix.Text', 'mix text'); C.Text('Mix.Text_2', 'second text');
  C.Int('Mix.Inner.a', 4444); C.Int('Mix.Inner.b', 55); C.Int('Mix.Inner.c', 6, false); C.Int('Mix.Inner.d', 1);
  C.Text('Mix.Inner.e', 'inner');
  for k:=0 to 3 do C.Int(Format('Mix.Counts[%d]', [k]), k+1);
  for k:=0 to 2 do begin
    C.Int(Format('Mix.Deep[%d].x', [k]), 300+k);
    C.Int(Format('Mix.Deep[%d].y', [k]), -k);
  end;
  for k:=0 to 4 do begin
    C.Int(Format('Rec_Small[%d].Dint1', [k]), 1000+k);
    C.Int(Format('Rec_Small[%d].aBool', [k]), k mod 2);
    C.Int(Format('Rec_Small[%d].aInt', [k]), -10-k);
  end;
  for k:=0 to 19 do begin
    C.Int(Format('Rec_Text[%d].Dint1', [k]), 2000+k);
    C.Int(Format('Rec_Text[%d].aBool', [k]), Ord(k mod 3=0));
    C.Int(Format('Rec_Text[%d].aInt', [k]), 100+k);
    C.Text(Format('Rec_Text[%d].Descr', [k]), 'record');
  end;
  for k:=0 to 3 do begin
    C.Int(Format('Motors[%d].Run', [k]), Ord(k mod 2=0));
    C.Real_(Format('Motors[%d].Speed', [k]), 10*k+0.5);
    C.Int(Format('Motors[%d].Total', [k]), 5000+k);
    C.Text(Format('Motors[%d].Descr', [k]), 'motor');
  end;
  C.Int('Motor_One.Run', 1); C.Real_('Motor_One.Speed', 12.5); C.Int('Motor_One.Total', 5000);
  C.Int('Motor_One.Status', $A0, false); C.Text('Motor_One.Descr', 'motor');
end;

//--- TLiveCase ----------------------------------------------------------------------------

procedure TLiveCase.SetUp;
begin
  inherited SetUp;
  FOwned := TList.Create;
  if GVerbose then WriteLn(' ### ', ClassName, '.', TestName);
  if not LiveReady then
    Ignore('No PLC: set S7PLUS_HOST (and S7PLUS_USER / S7PLUS_PASSWORD) to run the live tests.');
end;

procedure TLiveCase.TearDown;
var
  i:Integer;
begin
  //tags first, then what they point to
  if FOwned<>nil then begin
    for i:=FOwned.Count-1 downto 0 do
      TComponent(FOwned[i]).Free;
    FOwned.Free;
  end;
  inherited TearDown;
end;

function TLiveCase.Own(AComp:TComponent):TComponent;
begin
  FOwned.Add(AComp);
  Result := AComp;
end;

procedure TLiveCase.Pump(Ms:Integer);
var
  t:QWord;
begin
  t := GetTickCount64;
  while GetTickCount64-t<QWord(Ms) do begin
    Sleep(10);
    CheckSynchronize;
  end;
end;

//--- TTestLiveConnection -------------------------------------------------------------------

procedure TTestLiveConnection.TheDriverLogsInAndFindsTheTestDBs;
begin
  AssertTrue('TestDB_Opt', DbNumberOf('TestDB_Opt')>=0);
  AssertTrue('TestDB_NonOpt', DbNumberOf('TestDB_NonOpt')>=0);
  AssertTrue('TestDB_Big', DbNumberOf('TestDB_Big')>=0);
  AssertTrue('TestDB_Dyn', DbNumberOf('TestDB_Dyn')>=0);
end;

procedure TTestLiveConnection.AuthWithTheSameCredentialsIsOk;
begin
  if GUser='' then Ignore('No S7PLUS_USER: this PLC needs no login.');
  AssertTrue('logged in again', GDriver.Auth(GUser, GPassword)=ioOk);
  AssertTrue('and it still reads', GDriver.ListDataBlocks(GBlocks)=ioOk);
end;

procedure TTestLiveConnection.ListTagsDescribesTheTestDBs;
var
  Tags:TPLCTagInfoArray;
  i:Integer;
  function Find(const P:String):Integer;
  var
    k:Integer;
  begin
    Result := -1;
    for k:=0 to High(Tags) do
      if SameText(Tags[k].Path, P) then Result := k;
  end;
begin
  AssertTrue('ListTags', GDriver.ListTags(Tags, true)=ioOk);
  i := Find('TestDB_Opt');
  AssertTrue('the DB is a structure', (i>=0) and (Tags[i].Kind=tkStructure));
  i := Find('TestDB_Opt.String_Mid');
  AssertTrue('a string', (i>=0) and (Tags[i].Kind=tkString));
  AssertEquals('its maximum length is read from the PLC', 50, Tags[i].ElementCount);
  i := Find('TestDB_Opt.String_Default');
  AssertTrue(i>=0);
  AssertEquals('the default length', 254, Tags[i].ElementCount);
  i := Find('TestDB_Opt.Arr_DInt');
  AssertTrue('an array', (i>=0) and (Tags[i].Kind=tkArray));
  AssertEquals(10, Tags[i].ElementCount);
  i := Find('TestDB_Opt.DInt_1');
  AssertTrue('a scalar', (i>=0) and (Tags[i].Kind=tkScalar) and (Tags[i].TagType=pttLongInt));
  i := Find('TestDB_Opt.Rec_Text');
  AssertTrue('an array of structures', (i>=0) and (Tags[i].Kind=tkStructure));
  AssertEquals(20, Tags[i].ElementCount);
  i := Find('TestDB_Opt.Time_1');
  AssertTrue('a type that is not a tag', (i>=0) and (Tags[i].Kind=tkUnsupported));
end;

procedure TTestLiveConnection.RepeatedListsOfTheDBsAreNoChange;
var
  Before, i:Integer;
  V:TS7PlusVarInfoArray;
begin
  DbVars('TestDB_Opt', i);
  GDriver.BrowseDB(i, V);
  Before := GChanges;
  for i:=1 to 4 do
    AssertTrue(GDriver.ListDataBlocks(GBlocks)=ioOk);
  AssertEquals('the program did not change, the driver must not say so', Before, GChanges);
end;

//--- TTestLiveReads ------------------------------------------------------------------------

procedure TTestLiveReads.EveryStartValueOfTheOptimizedDB;
var
  C:TChecker;
begin
  C := TChecker.Create('TestDB_Opt');
  try
    CheckStartValues(C);
    AssertEquals(C.Report, 0, C.Failures.Count);
  finally
    C.Free;
  end;
end;

procedure TTestLiveReads.EveryStartValueOfTheNonOptimizedDB;
var
  C:TChecker;
begin
  C := TChecker.Create('TestDB_NonOpt');
  try
    CheckStartValues(C);
    AssertEquals(C.Report, 0, C.Failures.Count);
  finally
    C.Free;
  end;
end;

procedure TTestLiveReads.TheBigDBHoldsThePattern;
var
  C:TChecker;
  k:Integer;
begin
  C := TChecker.Create('TestDB_Big');
  try
    for k:=0 to 499 do C.Int(Format('Arr_Big[%d]', [k]), 100000+k);
    for k:=0 to 599 do C.Int(Format('Arr_Bytes600[%d]', [k]), k mod 256, false);
    for k:=0 to 9 do begin
      C.Int(Format('Texts[%d].Value', [k]), 9000+k);
      C.Text(Format('Texts[%d].Descr', [k]), 'text');
    end;
    AssertEquals(C.Report, 0, C.Failures.Count);
  finally
    C.Free;
  end;
end;

procedure TTestLiveReads.TheDynamicDBChangesByItself;
var
  B:TBytes;
  First, Second:Int64;
  Ramp1, Ramp2:Double;
begin
  AssertTrue(RawOf('TestDB_Dyn', 'CountValue', B));
  First := BEValue(B, true);
  AssertTrue(RawOf('TestDB_Dyn', 'Ramp', B));
  Ramp1 := SingleOf(B);
  Sleep(1500);
  AssertTrue(RawOf('TestDB_Dyn', 'CountValue', B));
  Second := BEValue(B, true);
  AssertTrue(RawOf('TestDB_Dyn', 'Ramp', B));
  Ramp2 := SingleOf(B);
  AssertTrue('the counter moved: '+IntToStr(First)+' -> '+IntToStr(Second), Second<>First);
  AssertTrue('the ramp moved', not SameValue(Ramp1, Ramp2, 0.0001));
  AssertTrue(RawOf('TestDB_Dyn', 'Text', B));
  AssertTrue('the text is TICK or TOCK', (TextOf(B)='TICK') or (TextOf(B)='TOCK'));
end;

procedure TTestLiveReads.ABatchOfBigValuesDoesNotKillTheSession;
var
  Items:TS7PlusMultiReadItemArray;
  Results:TS7PlusMultiReadResultArray;
  V:TS7PlusVarInfo;
  N, k, Ok:Integer;
begin
  //20 strings of 256 bytes in ONE call: 5 KB, far over the ~990 bytes of a PDU
  SetLength(Items, 20);
  for k:=0 to 19 do begin
    AssertTrue(VarOf('TestDB_Opt', Format('Rec_Text[%d].Descr', [k]), V, N));
    Items[k].AccessArea := S7PlusIds_DBAccessAreaBase+Cardinal(N);
    Items[k].Lids := V.Lids;
    Items[k].ExpectedSize := V.DataSize;
  end;
  AssertTrue('the batch', GDriver.ReadMultipleSymbolic(Items, Results)=ioOk);
  Ok := 0;
  for k:=0 to High(Results) do
    if Results[k].Ok and (TextOf(Results[k].Data)='record') then Inc(Ok);
  AssertEquals('every string came back', 20, Ok);
  AssertTrue('and the session is alive', GDriver.ListDataBlocks(GBlocks)=ioOk);
end;

//--- TTestLiveWrites -----------------------------------------------------------------------

type
  TWrittenValue = record
    Member:String;
    Data:TBytes;
  end;

//Writes Data to the member, reads it back and puts the original back.
function RoundTrip(const ADb, AMember:String; const Data:TBytes; out Got, Original:TBytes):Boolean;
begin
  Result := false;
  if not RawOf(ADb, AMember, Original) then exit;
  if not WriteRaw(ADb, AMember, Data) then exit;
  Sleep(50);
  Result := RawOf(ADb, AMember, Got);
  WriteRaw(ADb, AMember, Original);
end;

procedure TTestLiveWrites.ScalarsOfEverySizeComeBackAsWritten;
  procedure Same(const M:String; const D:TBytes);
  var
    Got, Orig:TBytes;
    k:Integer;
    A, B:String;
  begin
    AssertTrue(M+' round trip', RoundTrip('TestDB_Opt', M, D, Got, Orig));
    A := '';
    B := '';
    for k:=0 to High(D) do A := A+IntToHex(D[k], 2);
    for k:=0 to High(Got) do B := B+IntToHex(Got[k], 2);
    AssertEquals(M+' came back', A, B);
    AssertTrue(M+' restored', RawOf('TestDB_Opt', M, Got));
    A := '';
    for k:=0 to High(Orig) do A := A+IntToHex(Orig[k], 2);
    B := '';
    for k:=0 to High(Got) do B := B+IntToHex(Got[k], 2);
    AssertEquals(M+' restored to what it was', A, B);
  end;
begin
  Same('Bool_2', BEBytes(1, 1));
  Same('Byte_1', BEBytes($AB, 1));
  Same('SInt_1', BEBytes(-100, 1));
  Same('Word_1', BEBytes($BEEF, 2));
  Same('Int_1', BEBytes(-31000, 2));
  Same('UInt_1', BEBytes(12345, 2));
  Same('DWord_1', BEBytes($0BADF00D, 4));
  Same('DInt_1', BEBytes(-2000000000, 4));
  Same('UDInt_1', BEBytes(3000000000, 4));
  Same('Real_1', SingleBytes(-7.25));
  Same('LWord_1', BEBytes(Int64($1122334455667788), 8));
  Same('LInt_1', BEBytes(-9000000000000, 8));
  Same('LReal_1', DoubleBytes(1.0e100));
end;

procedure TTestLiveWrites.StringsComeBackAsWritten;
var
  Got, Orig:TBytes;
begin
  AssertTrue(RoundTrip('TestDB_Opt', 'String_Short', StringBytes('abc', 10), Got, Orig));
  AssertEquals('abc', TextOf(Got));
  AssertTrue(RoundTrip('TestDB_Opt', 'String_Mid', StringBytes('a longer text than the short one', 50), Got, Orig));
  AssertEquals('a longer text than the short one', TextOf(Got));
  AssertTrue(RoundTrip('TestDB_Opt', 'String_Default', StringBytes('default length string', 254), Got, Orig));
  AssertEquals('default length string', TextOf(Got));
  AssertTrue(RawOf('TestDB_Opt', 'String_Short', Got));
  AssertEquals('restored', 'hello', TextOf(Got));
end;

procedure TTestLiveWrites.MembersInStructuresAndArraysOfStructuresComeBackAsWritten;
var
  Got, Orig:TBytes;
begin
  AssertTrue(RoundTrip('TestDB_Opt', 'Mix.v3', BEBytes($0A0B0C0D, 4), Got, Orig));
  AssertEquals($0A0B0C0D, BEValue(Got, false));
  AssertTrue(RoundTrip('TestDB_Opt', 'Mix.Inner.a', BEBytes(-777, 4), Got, Orig));
  AssertEquals(-777, BEValue(Got, true));
  AssertTrue(RoundTrip('TestDB_Opt', 'Mix.Inner.e', StringBytes('changed', 10), Got, Orig));
  AssertEquals('changed', TextOf(Got));
  AssertTrue(RoundTrip('TestDB_Opt', 'Rec_Text[5].Dint1', BEBytes(555555, 4), Got, Orig));
  AssertEquals(555555, BEValue(Got, true));
  AssertTrue(RoundTrip('TestDB_Opt', 'Rec_Text[5].Descr', StringBytes('five', 254), Got, Orig));
  AssertEquals('five', TextOf(Got));
  AssertTrue(RoundTrip('TestDB_Opt', 'Motors[2].Speed', SingleBytes(99.5), Got, Orig));
  AssertTrue(SameValue(99.5, SingleOf(Got), 0.0001));
  AssertTrue('the neighbours were not touched', RawOf('TestDB_Opt', 'Rec_Text[4].Dint1', Got));
  AssertEquals(2004, BEValue(Got, true));
  AssertTrue(RawOf('TestDB_Opt', 'Rec_Text[6].Dint1', Got));
  AssertEquals(2006, BEValue(Got, true));
end;

procedure TTestLiveWrites.ArrayElementsComeBackAsWritten;
var
  Got, Orig:TBytes;
begin
  AssertTrue(RoundTrip('TestDB_Opt', 'Arr_DInt[3]', BEBytes(-33, 4), Got, Orig));
  AssertEquals(-33, BEValue(Got, true));
  AssertTrue(RoundTrip('TestDB_Opt', 'Arr_NegLow[-1]', BEBytes(-999, 2), Got, Orig));
  AssertEquals(-999, BEValue(Got, true));
  AssertTrue(RoundTrip('TestDB_Opt', 'Arr_Bool[1]', BEBytes(1, 1), Got, Orig));
  AssertEquals(1, BEValue(Got, false));
  AssertTrue(RoundTrip('TestDB_Opt', 'Arr_Real[2]', SingleBytes(123.5), Got, Orig));
  AssertTrue(SameValue(123.5, SingleOf(Got), 0.0001));
  AssertTrue('Arr_DInt[2] was not touched', RawOf('TestDB_Opt', 'Arr_DInt[2]', Got));
  AssertEquals(102, BEValue(Got, true));
  AssertTrue('Arr_DInt[4] was not touched', RawOf('TestDB_Opt', 'Arr_DInt[4]', Got));
  AssertEquals(104, BEValue(Got, true));
end;

procedure TTestLiveWrites.TheSameOnTheNonOptimizedDB;
var
  Got, Orig:TBytes;
begin
  AssertTrue(RoundTrip('TestDB_NonOpt', 'DInt_1', BEBytes(-42, 4), Got, Orig));
  AssertEquals(-42, BEValue(Got, true));
  AssertTrue(RoundTrip('TestDB_NonOpt', 'Mix.Inner.a', BEBytes(8888, 4), Got, Orig));
  AssertEquals(8888, BEValue(Got, true));
  AssertTrue(RoundTrip('TestDB_NonOpt', 'String_Mid', StringBytes('non optimized', 50), Got, Orig));
  AssertEquals('non optimized', TextOf(Got));
  AssertTrue(RoundTrip('TestDB_NonOpt', 'Rec_Small[3].aInt', BEBytes(-12345, 2), Got, Orig));
  AssertEquals(-12345, BEValue(Got, true));
end;

procedure TTestLiveWrites.ThePLCEchoesWhatWasWritten;
var
  B:TBytes;
  k:Integer;
begin
  for k:=1 to 5 do begin
    AssertTrue(WriteRaw('TestDB_Dyn', 'Echo_In', BEBytes(k*1000, 4)));
    Sleep(150);
    AssertTrue(RawOf('TestDB_Dyn', 'Echo_Out', B));
    AssertEquals('Echo_Out = Echo_In + 1', k*1000+1, BEValue(B, true));
  end;
  WriteRaw('TestDB_Dyn', 'Echo_In', BEBytes(0, 4));
end;

procedure TTestLiveWrites.TheResetCommandZeroesTheCounter;
var
  B:TBytes;
begin
  AssertTrue(WriteRaw('TestDB_Dyn', 'StepSize', BEBytes(1000, 4)));
  Sleep(300);
  AssertTrue(RawOf('TestDB_Dyn', 'CountValue', B));
  AssertTrue('the counter is going up fast', BEValue(B, true)>1000);
  AssertTrue(WriteRaw('TestDB_Dyn', 'ResetCmd', BEBytes(1, 1)));
  Sleep(200);
  AssertTrue(RawOf('TestDB_Dyn', 'ResetCmd', B));
  AssertEquals('the PLC cleared the command bit', 0, BEValue(B, false));
  WriteRaw('TestDB_Dyn', 'StepSize', BEBytes(1, 4));
  AssertTrue(RawOf('TestDB_Dyn', 'CountValue', B));
  AssertTrue('and the counter started over (it is small)', BEValue(B, true)<10000000);
end;

procedure TTestLiveWrites.DisablingFreezesTheCounter;
var
  B:TBytes;
  A, C:Int64;
begin
  AssertTrue(WriteRaw('TestDB_Dyn', 'Enable', BEBytes(0, 1)));
  try
    Sleep(300);
    AssertTrue(RawOf('TestDB_Dyn', 'CountValue', B));
    A := BEValue(B, true);
    Sleep(1000);
    AssertTrue(RawOf('TestDB_Dyn', 'CountValue', B));
    C := BEValue(B, true);
    AssertEquals('frozen', A, C);
  finally
    WriteRaw('TestDB_Dyn', 'Enable', BEBytes(1, 1));
  end;
  Sleep(300);
  AssertTrue(RawOf('TestDB_Dyn', 'CountValue', B));
  AssertTrue('and moving again', BEValue(B, true)<>A);
end;

//--- TTestLiveTags -------------------------------------------------------------------------

//A structure of a whole DB with an item for every variable, the way the Tag Builder builds it.
function BuildDBStructure(ACase:TLiveCase; const ADb:String; Scan:Integer;
  out Items:TList; out Strs:TList):TPLCStruct;
var
  Cands:TS7PlusCandidateArray;
  Checked:array of Boolean;
  Plan:TS7PlusImportItems;
  Vars:TS7PlusVarInfoArray;
  N, k:Integer;
  It:TPLCStructItem;
  Ss:TPLCStructString;
begin
  Items := TList.Create;
  Strs := TList.Create;
  SetLength(Cands, 0);
  Vars := DbVars(ADb, N);
  S7PlusAddBrowsed(Cands, Vars, ADb, S7PlusIds_DBAccessAreaBase+Cardinal(N));
  S7PlusMarkArrays(Cands);
  GDriver.ResolveStringLengths(Cands);
  SetLength(Checked, Length(Cands));
  for k:=0 to High(Checked) do Checked[k] := true;
  Plan := S7PlusBuildImportPlan(Cands, Checked, 'p', Scan, true);
  if Length(Plan)=0 then exit(nil);

  Result := TPLCStruct.Create(nil);
  ACase.Own(Result);       //freed last: the items are freed before it
  Result.Name := 'live_'+ADb;
  Result.LongAddress := Plan[0].Path;
  Result.Size := Plan[0].Size;
  Result.RefreshTime := Plan[0].Scan;
  Result.ProtocolDriver := GDriver;
  for k:=0 to High(Plan[0].StructItems) do
    if Plan[0].StructItems[k].IsString then begin
      Ss := TPLCStructString.Create(nil);
      ACase.Own(Ss);
      Ss.Name := Plan[0].StructItems[k].CompName;
      Ss.PLCBlock := Result;
      Ss.Index := Plan[0].StructItems[k].Offset;
      Ss.StringType := stSIEMENS;
      Ss.StringSize := Plan[0].StructItems[k].StringSize;
      Strs.Add(Ss);
    end else begin
      It := TPLCStructItem.Create(nil);
      ACase.Own(It);
      It.Name := Plan[0].StructItems[k].CompName;
      It.PLCBlock := Result;
      It.Index := Plan[0].StructItems[k].Offset;
      It.TagType := Plan[0].StructItems[k].TagType;
      It.SwapBytes := Plan[0].StructItems[k].SwapBytes;
      It.SwapWords := Plan[0].StructItems[k].SwapWords;
      It.SwapDWords := Plan[0].StructItems[k].SwapDWords;
      Items.Add(It);
    end;
end;

function ItemNamed(Items:TList; const AName:String):TPLCStructItem;
var
  i:Integer;
begin
  Result := nil;
  for i:=0 to Items.Count-1 do
    if TPLCStructItem(Items[i]).Name=AName then Result := TPLCStructItem(Items[i]);
end;

function TextNamed(Strs:TList; const AName:String):TPLCStructString;
var
  i:Integer;
begin
  Result := nil;
  for i:=0 to Strs.Count-1 do
    if TPLCStructString(Strs[i]).Name=AName then Result := TPLCStructString(Strs[i]);
end;

procedure CheckStructValues(Items, Strs:TList; const ADb:String; out Report:String);
var
  Wrong:TStringList;
  procedure Num(const Member:String; Expected:Double);
  var
    It:TPLCStructItem;
  begin
    It := ItemNamed(Items, S7PlusSuggestedName(ADb+'.'+Member, 'p'));
    if It=nil then Wrong.Add(Member+': no item')
    else if not SameValue(It.Value, Expected, 0.001) then
      Wrong.Add(Format('%s: read %g, expected %g', [Member, It.Value, Expected]));
  end;
  procedure Txt(const Member, Expected:String);
  var
    S:TPLCStructString;
  begin
    S := TextNamed(Strs, S7PlusSuggestedName(ADb+'.'+Member, 'p'));
    if S=nil then Wrong.Add(Member+': no text item')
    else if Trim(StringReplace(S.Value, #0, '', [rfReplaceAll]))<>Expected then
      Wrong.Add(Format('%s: read "%s", expected "%s"', [Member, S.Value, Expected]));
  end;
var
  k:Integer;
begin
  Wrong := TStringList.Create;
  try
    Num('Bool_1', 1); Num('Byte_1', $12); Num('SInt_1', -5); Num('Word_1', $1234); Num('Int_1', -1234);
    Num('DWord_1', $DEADBEEF); Num('DInt_1', -123456); Num('UDInt_1', 4000000000); Num('Real_1', 3.14);
    Num('LInt_1', -1234567890123);
    Txt('String_Short', 'hello'); Txt('String_Mid', 'middle string value');
    Txt('String_Default', 'default text');
    for k:=0 to 9 do Num(Format('Arr_DInt[%d]', [k]), 100+k);
    for k:=-2 to 2 do Num(Format('Arr_NegLow[%d]', [k]), k*10);
    Num('Mix.v1', $11); Num('Mix.v2', $2222); Num('Mix.v3', $33333333); Num('Mix.Static_1', 1.5);
    Txt('Mix.Text', 'mix text'); Txt('Mix.Text_2', 'second text');
    Num('Mix.Inner.a', 4444); Num('Mix.Inner.b', 55); Txt('Mix.Inner.e', 'inner');
    for k:=0 to 3 do Num(Format('Mix.Counts[%d]', [k]), k+1);
    for k:=0 to 4 do begin
      Num(Format('Rec_Small[%d].Dint1', [k]), 1000+k);
      Num(Format('Rec_Small[%d].aInt', [k]), -10-k);
    end;
    for k:=0 to 19 do begin
      Num(Format('Rec_Text[%d].Dint1', [k]), 2000+k);
      Num(Format('Rec_Text[%d].aInt', [k]), 100+k);
      Txt(Format('Rec_Text[%d].Descr', [k]), 'record');
    end;
    for k:=0 to 3 do begin
      Num(Format('Motors[%d].Speed', [k]), 10*k+0.5);
      Num(Format('Motors[%d].Total', [k]), 5000+k);
    end;
    Report := Format('%d items differ:', [Wrong.Count])+LineEnding+Wrong.Text;
    if Wrong.Count=0 then Report := '';
  finally
    Wrong.Free;
  end;
end;

procedure TTestLiveTags.AWholeDBFromTheImportPlanReadsEveryStartValue;
var
  St:TPLCStruct;
  Items, Strs:TList;
  Rep:String;
begin
  St := BuildDBStructure(Self, 'TestDB_Opt', 500, Items, Strs);
  try
    AssertTrue('a structure came out of the plan', St<>nil);
    Pump(4000);
    CheckStructValues(Items, Strs, 'TestDB_Opt', Rep);
    AssertEquals(Rep, '', Rep);
    AssertTrue('many items', Items.Count+Strs.Count>150);
  finally
    Items.Free;
    Strs.Free;
  end;
end;

procedure TTestLiveTags.TheSameOnChange;
var
  St:TPLCStruct;
  Items, Strs:TList;
  Rep:String;
begin
  //RefreshTime 0: the whole DB arrives through subscriptions
  St := BuildDBStructure(Self, 'TestDB_NonOpt', 0, Items, Strs);
  try
    AssertTrue(St<>nil);
    Pump(5000);
    CheckStructValues(Items, Strs, 'TestDB_NonOpt', Rep);
    AssertEquals(Rep, '', Rep);
  finally
    Items.Free;
    Strs.Free;
  end;
end;

procedure TTestLiveTags.ABigWholeDBIsReadInSeveralRequests;
var
  St:TPLCStruct;
  Items, Strs:TList;
  It:TPLCStructItem;
  k:Integer;
  Bad:String;
begin
  //TestDB_Big is about 2.6 KB: a read over a PDU (~990 bytes) kills the session if it goes in one
  St := BuildDBStructure(Self, 'TestDB_Big', 500, Items, Strs);
  try
    AssertTrue(St<>nil);
    AssertTrue('as big as the DB', St.Size>=2600);
    Pump(5000);
    Bad := '';
    for k:=0 to 499 do begin
      It := ItemNamed(Items, S7PlusSuggestedName(Format('TestDB_Big.Arr_Big[%d]', [k]), 'p'));
      if (It=nil) or (Trunc(It.Value)<>100000+k) then Bad := Bad+' '+IntToStr(k);
    end;
    AssertEquals('Arr_Big items that differ:'+Bad, '', Bad);
    AssertTrue('and the session is alive', GDriver.ListDataBlocks(GBlocks)=ioOk);
  finally
    Items.Free;
    Strs.Free;
  end;
end;

procedure TTestLiveTags.ANumberTagWritesAndReadsBack;
var
  T:TPLCTagNumber;
  Orig:Double;
begin
  T := TPLCTagNumber(Own(TPLCTagNumber.Create(nil)));
  T.Name := 'live_dint';
  T.TagType := pttLongInt;
  T.SwapBytes := true;
  T.SwapWords := true;
  T.RefreshTime := 200;
  T.LongAddress := 'TestDB_Opt.DInt_1';
  T.ProtocolDriver := GDriver;
  Pump(1500);
  AssertEquals('start value', -123456, Trunc(T.Value));
  Orig := T.Value;
  T.Value := 424242;
  Pump(1500);
  AssertEquals('written', 424242, Trunc(T.Value));
  T.Value := Orig;
  Pump(1000);
  AssertEquals('restored', -123456, Trunc(T.Value));
end;

procedure TTestLiveTags.AStringTagWritesAndReadsBack;
var
  T:TPLCString;
begin
  T := TPLCString(Own(TPLCString.Create(nil)));
  T.Name := 'live_str';
  T.StringType := stSIEMENS;
  T.StringSize := 50;
  T.RefreshTime := 200;
  T.LongAddress := 'TestDB_Opt.String_Mid';
  T.ProtocolDriver := GDriver;
  Pump(1500);
  AssertEquals('middle string value', T.Value);
  T.Value := 'written by a tag';
  Pump(1500);
  AssertEquals('written by a tag', T.Value);
  T.Value := 'middle string value';
  Pump(1000);
  AssertEquals('middle string value', T.Value);
end;

type
  TCounter = class
    Updates:Integer;
    procedure Update(Sender:TObject);
  end;

procedure TCounter.Update(Sender:TObject);
begin
  Inc(Updates);
end;

procedure TTestLiveTags.AnOnChangeTagFollowsTheCounter;
var
  T:TPLCTagNumber;
  C:TCounter;
  Last:Double;
begin
  C := TCounter.Create;
  try
    T := TPLCTagNumber(Own(TPLCTagNumber.Create(nil)));
    T.Name := 'live_count';
    T.TagType := pttLongInt;
    T.SwapBytes := true;
    T.SwapWords := true;
    T.RefreshTime := 0;
    T.OnUpdate := @C.Update;
    T.LongAddress := 'TestDB_Dyn.CountValue';
    T.ProtocolDriver := GDriver;
    Pump(3000);
    AssertTrue('many updates, with no polling: '+IntToStr(C.Updates), C.Updates>=5);
    Last := T.Value;
    Pump(1000);
    AssertTrue('and it keeps moving', T.Value<>Last);
    T.OnUpdate := nil;
  finally
    C.Free;
  end;
end;

procedure TTestLiveTags.AWholeDBStructureWritesAnItem;
var
  St:TPLCStruct;
  Items, Strs:TList;
  It:TPLCStructItem;
  B:TBytes;
begin
  //Writing through the structure of a whole DB reaches the TOP LEVEL scalar members only: what is
  //inside a structure is in the real layout of the PLC, which a write cannot be cut from.
  St := BuildDBStructure(Self, 'TestDB_Opt', 300, Items, Strs);
  try
    AssertTrue(St<>nil);
    Pump(3000);
    It := ItemNamed(Items, S7PlusSuggestedName('TestDB_Opt.DInt_1', 'p'));
    AssertTrue('the item of DInt_1', It<>nil);
    AssertEquals(-123456, Trunc(It.Value));
    It.Value := 1234;
    Pump(1500);
    AssertTrue(RawOf('TestDB_Opt', 'DInt_1', B));
    AssertEquals('the PLC has it', 1234, BEValue(B, true));
    It.Value := -123456;
    Pump(1500);
    AssertTrue(RawOf('TestDB_Opt', 'DInt_1', B));
    AssertEquals('restored', -123456, BEValue(B, true));
  finally
    Items.Free;
    Strs.Free;
  end;
end;

procedure TTestLiveTags.AnArrayElementTagWritesOnlyItsElement;
var
  Blk:TPLCBlock;
  El:TPLCBlockElement;
  B:TBytes;
  k:Integer;
begin
  //one element of an array: the driver writes a variable whole, so the rest must come back as it was
  Blk := TPLCBlock(Own(TPLCBlock.Create(nil)));
  Blk.Name := 'live_arr';
  Blk.TagType := pttLongInt;
  Blk.SwapBytes := true;
  Blk.SwapWords := true;
  Blk.Size := 10;
  Blk.RefreshTime := 300;
  Blk.LongAddress := 'TestDB_Opt.Arr_DInt';
  Blk.ProtocolDriver := GDriver;
  El := TPLCBlockElement(Own(TPLCBlockElement.Create(nil)));
  El.Name := 'live_arr_3';
  El.PLCBlock := Blk;
  El.Index := 3;
  Pump(2000);
  AssertEquals('element 3 start value', 103, Trunc(El.Value));
  El.Value := -4242;
  Pump(2000);
  AssertTrue(RawOf('TestDB_Opt', 'Arr_DInt[3]', B));
  AssertEquals('the PLC has it', -4242, BEValue(B, true));
  for k:=0 to 9 do
    if k<>3 then begin
      AssertTrue(RawOf('TestDB_Opt', Format('Arr_DInt[%d]', [k]), B));
      AssertEquals(Format('element %d was not touched', [k]), 100+k, BEValue(B, true));
    end;
  El.Value := 103;
  Pump(2000);
  AssertTrue(RawOf('TestDB_Opt', 'Arr_DInt[3]', B));
  AssertEquals('restored', 103, BEValue(B, true));
end;

//--- TTestLiveReconnect --------------------------------------------------------------------

procedure TTestLiveReconnect.TheTagsComeBackAfterThePortIsClosedAndOpened;
var
  Port:TTCP_UDPPort;
  Drv:TS7CommPlusDriver;
  T:TPLCTagNumber;
  t0:QWord;
begin
  //its own session: the port is closed and opened under a subscription
  AssertTrue(WriteRaw('TestDB_Dyn', 'Echo_In', BEBytes(777, 4)));
  OpenPort(Port, Drv);
  T := TPLCTagNumber.Create(nil);
  try
    T.Name := 'live_rc';
    T.TagType := pttLongInt;
    T.SwapBytes := true;
    T.SwapWords := true;
    T.RefreshTime := 0;
    T.LongAddress := 'TestDB_Dyn.Echo_In';
    T.ProtocolDriver := Drv;
    Pump(2500);
    AssertEquals('the first value arrived', 777, Trunc(T.Value));

    Port.Active := false;
    Pump(3000);
    //written by the shared session while this one is down
    AssertTrue(WriteRaw('TestDB_Dyn', 'Echo_In', BEBytes(31415, 4)));
    Pump(500);
    Port.Active := true;

    t0 := GetTickCount64;
    while (Trunc(T.Value)<>31415) and (GetTickCount64-t0<25000) do
      Pump(100);
    AssertEquals('the value written during the outage arrived', 31415, Trunc(T.Value));

    AssertTrue(WriteRaw('TestDB_Dyn', 'Echo_In', BEBytes(27182, 4)));
    t0 := GetTickCount64;
    while (Trunc(T.Value)<>27182) and (GetTickCount64-t0<5000) do
      Pump(100);
    AssertEquals('and a new one arrives by notification', 27182, Trunc(T.Value));
  finally
    WriteRaw('TestDB_Dyn', 'Echo_In', BEBytes(0, 4));
    T.Free;
    Drv.Free;
    Port.Free;
  end;
end;

//--- TTestLiveCertificate ------------------------------------------------------------------

const
  //a certificate that is NOT the PLC's (test one, EC P-256, made only for the tests)
  OtherCertificate =
    'MIIBkTCCATegAwIBAgIUBvIA5qkHpiHCUMF1pz+r3nLw3kAwCgYIKoZIzj0EAwIwHTEbMBkGA1UEAwwScGFzY2Fsc2NhZGEtdGVzdC1i'
    + 'MCAXDTI2MTAyOTAyNDQyMloYDzIxMjYxMDA1MDI0NDIyWjAdMRswGQYDVQQDDBJwYXNjYWxzY2FkYS10ZXN0LWIwWTATBgcqhkjOPQIB'
    + 'BggqhkjOPQMBBwNCAARarWggb14Yj/ZWfpHUHTnqIO2ojpAQanMu1I4+YvQp6vMSvaS7+YgyQ/FzQs7cgkevHEiRi3j30SaF5BKYGLQ3'
    + 'o1MwUTAdBgNVHQ4EFgQU5xpuwbieTXFz3CNa18WrECp/+WwwHwYDVR0jBBgwFoAU5xpuwbieTXFz3CNa18WrECp/+WwwDwYDVR0TAQH/'
    + 'BAUwAwEB/zAKBggqhkjOPQQDAgNIADBFAiEAqgwkYNzHz+G4hgm/5rmVicjtBI1UBLSgrcYdznYaH0sCIBnsoUR7DeoBueLOw7G1jkrF'
    + 'PXfWTkjwhR7ocVj9zmVk';

//Its own session with ServerCertificate set; true when it logs in and lists the DBs.
function TTestLiveCertificate.Connects(const ACertificate:AnsiString):Boolean;
var
  Port:TTCP_UDPPort;
  Drv:TS7CommPlusDriver;
  Blocks:TS7PlusDataBlockInfoArray;
  w:Integer;
begin
  Port := TTCP_UDPPort.Create(nil);
  Drv := TS7CommPlusDriver.Create(nil);
  try
    Port.Host := GHost;
    Port.Port := GPortNumber;
    Port.Timeout := 8000;
    Drv.CommunicationPort := Port;
    if GUser<>'' then begin
      Drv.Username := GUser;
      Drv.Password := GPassword;
    end;
    Drv.ServerCertificate := ACertificate;
    Drv.OnDebug := @TSink(GSink).Debug;
    Port.Active := true;
    w := 0;
    while (not Port.ReallyActive) and (w<6000) do begin
      Sleep(100);
      Inc(w, 100);
    end;
    Result := Port.ReallyActive and (Drv.ListDataBlocks(Blocks)=ioOk);
  finally
    Drv.Free;
    Port.Free;
  end;
end;

procedure TTestLiveCertificate.ThePlcSendsACertificate;
begin
  AssertTrue('the shared session (no certificate set) keeps the one the PLC presented',
    Length(GDriver.PeerCertificate)>200);
end;

procedure TTestLiveCertificate.TheCertificateOfThePlcIsAccepted;
begin
  AssertTrue('trusting the certificate the PLC presented', Connects(GDriver.PeerCertificate));
end;

procedure TTestLiveCertificate.AnotherCertificateIsRefused;
begin
  AssertFalse('trusting only another certificate', Connects(OtherCertificate));
end;

procedure TTestLiveCertificate.AnInvalidValueFailsClosed;
begin
  AssertFalse('a value that is not a certificate does not mean "accept any"', Connects('not a certificate'));
end;

//--- ZZ ------------------------------------------------------------------------------------

procedure TTestLiveZZRestore.ZZLoadPatternRestoresThePerElementValues;
var
  B:TBytes;
begin
  AssertTrue(WriteRaw('TestDB_Opt', 'Rec_Small[1].Dint1', BEBytes(1, 4)));
  AssertTrue(WriteRaw('TestDB_Dyn', 'LoadPattern', BEBytes(1, 1)));
  Sleep(500);
  AssertTrue(RawOf('TestDB_Opt', 'Rec_Small[1].Dint1', B));
  AssertEquals('the pattern is back', 1001, BEValue(B, true));
  AssertTrue(RawOf('TestDB_Dyn', 'LoadPattern', B));
  AssertEquals('and the PLC cleared the command', 0, BEValue(B, false));
end;

initialization
  RegisterTest(TTestLiveConnection);
  RegisterTest(TTestLiveReads);
  RegisterTest(TTestLiveWrites);
  RegisterTest(TTestLiveTags);
  RegisterTest(TTestLiveReconnect);
  RegisterTest(TTestLiveCertificate);
  RegisterTest(TTestLiveZZRestore);

finalization
  GDriver.Free;
  GPort.Free;
  GSink.Free;

end.
