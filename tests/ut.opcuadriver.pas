{
  Testes do driver OPC UA (TOPCUADriver) contra um servidor open62541 no próprio processo.
  Ignorados quando a libopen62541 1.5 não está instalada (ou OPEN62541_LIB não aponta para ela).

  Tests of the OPC UA driver (TOPCUADriver) against an in-process open62541 server.
  Ignored when libopen62541 1.5 is not installed (or OPEN62541_LIB does not point to it).
}
unit ut.opcuadriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Math, DateUtils, open62541, Tag, ProtocolTypes,
  PLCTagNumber, PLCBlock, PLCString, OPCUAValues, OPCUAClient, OPCUADriver, testsupport.opcuaserver,
  pscadalog, testsupport.logcollector;

type

  { TTestOPCUAValues }

  TTestOPCUAValues = class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure EveryNumericTypeBecomesItsNumber;
    procedure AnArrayGivesOneValuePerElement;
    procedure ADateTimeBecomesALocalTDateTime;
    procedure TextAndNumbersBecomeText;
    procedure AValueOutsideTheTypeIsRefused;
    procedure NumbersBuildScalarsAndArraysOfTheNodeType;
    procedure TextIsParsedForANumericNode;
    procedure TheAddressSplitsOnlyOnAValidIndexRange;
    procedure StatusCodesMapToIOResults;
  end;

  { TOPCUACase }

  TOPCUACase = class(TTestCase)
  protected
    FServer:TOPCUATestServer;
    FDriver:TOPCUADriver;
    FOwned:TList;
    FLog:TStringList;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure StartServer(const Opt:TOPCUATestServerOptions);
    procedure StopServer;
    function NewDriver:TOPCUADriver;
    procedure DriverLog(Sender:TObject; Level:TOPCUALogLevel; const Msg:String);
    function Own(C:TComponent):TComponent;
    function NumTag(const Address:String; Refresh:Integer = 50):TPLCTagNumber;
    function BlockTag(const Address:String; Size:Integer; Refresh:Integer = 50):TPLCBlock;
    function TextTag(const Address:String; Size:Integer = 30; Refresh:Integer = 50):TPLCString;
    procedure Pump(Ms:Integer);
    function WaitRaw(B:TPLCBlock; Idx:Integer; V:Double; Ms:Integer = 5000):Boolean;
    function WaitStatus(T:TPLCTagNumber; R:TProtocolIOResult; Ms:Integer = 5000):Boolean;
    function WaitNotOk(T:TPLCTagNumber; Ms:Integer = 5000):Boolean;
    function WaitValue(T:TPLCTagNumber; V:Double; Ms:Integer = 5000):Boolean;
    function WaitText(T:TPLCString; const V:String; Ms:Integer = 5000):Boolean;
  end;

  { TTestOPCUADriver }

  TTestOPCUADriver = class(TOPCUACase)
  private
    procedure ReadEveryScalar(Subscriptions:Boolean);
  published
    procedure EveryScalarTypeIsReadBySubscription;
    procedure EveryScalarTypeIsReadByPolling;
    procedure ArraysAreReadIntoBlocks;
    procedure AnIndexRangeAddressReadsOneElement;
    procedure TheNamespaceUriAddressWorks;
    procedure TheDriverMessagesReachTheLogHub;
    procedure ATextTagReadsAndWrites;
    procedure EveryScalarTypeIsWritten;
    procedure OneElementOfABlockIsWritten;
    procedure ANumberTooBigForTheNodeIsRefused;
    procedure AReadOnlyNodeRefusesTheWrite;
    procedure AnUnknownNodeIsReported;
    procedure TheCounterIsFollowedOnChange;
    procedure TheDriverReconnectsAfterTheServerRestarts;
    procedure ListTagsFindsTheTestVariables;
    procedure VariableInfoTellsTypeRankAndAccess;
  end;

  { TTestOPCUASecurity }

  TTestOPCUASecurity = class(TOPCUACase)
  published
    procedure SignAndEncryptConnectsAndReads;
    procedure TheRightPasswordOpensTheSession;
    procedure AWrongPasswordIsAnAuthError;
    procedure TheGeneratedClientCertificateIsKept;
  end;

implementation

uses
  Generics.Collections;

function LibOk(Test:TTestCase):Boolean;
var
  why:String;
begin
  Result := OPCUATestLibraryAvailable(why);
  if not Result then
    OPCUATestNeedLibrary(Test);
end;

{ TTestOPCUAValues }

procedure TTestOPCUAValues.SetUp;
begin
  inherited SetUp;
  LibOk(Self);
end;

procedure TTestOPCUAValues.EveryNumericTypeBecomesItsNumber;
  procedure Check1(Idx:Integer; P:Pointer; Expected:Double);
  var
    v:UA_Variant;
    vals:TArrayOfDouble;
  begin
    UA_Variant_init(v);
    UA_Variant_setScalar(@v, P, @UA_TYPES[Idx]);
    AssertTrue(UA_TYPES[Idx].typeName, OPCUAVariantToNumbers(v, vals)=ioOk);
    AssertEquals(UA_TYPES[Idx].typeName+' count', 1, Length(vals));
    AssertEquals(UA_TYPES[Idx].typeName, Expected, vals[0], 1e-6);
  end;
var
  b:UA_Boolean; sb:UA_SByte; by:UA_Byte; i16:UA_Int16; u16:UA_UInt16; i32:UA_Int32;
  u32:UA_UInt32; i64:UA_Int64; u64:UA_UInt64; f:UA_Float; d:UA_Double; sc:UA_StatusCode;
begin
  b := true;        Check1(UA_TYPES_BOOLEAN, @b, 1);
  sb := -5;         Check1(UA_TYPES_SBYTE, @sb, -5);
  by := 250;        Check1(UA_TYPES_BYTE, @by, 250);
  i16 := -30000;    Check1(UA_TYPES_INT16, @i16, -30000);
  u16 := 60000;     Check1(UA_TYPES_UINT16, @u16, 60000);
  i32 := -2000000;  Check1(UA_TYPES_INT32, @i32, -2000000);
  u32 := 4000000000; Check1(UA_TYPES_UINT32, @u32, 4000000000);
  i64 := -5000000000; Check1(UA_TYPES_INT64, @i64, -5000000000);
  u64 := 5000000000; Check1(UA_TYPES_UINT64, @u64, 5000000000);
  f := -2.5;        Check1(UA_TYPES_FLOAT, @f, -2.5);
  d := 1e100;       Check1(UA_TYPES_DOUBLE, @d, 1e100);
  sc := $80340000;  Check1(UA_TYPES_STATUSCODE, @sc, $80340000);
end;

procedure TTestOPCUAValues.AnArrayGivesOneValuePerElement;
var
  a:array[0..3] of UA_Int16;
  v:UA_Variant;
  vals:TArrayOfDouble;
  i:Integer;
begin
  for i:=0 to 3 do a[i] := i*-7;
  UA_Variant_init(v);
  UA_Variant_setArray(@v, @a[0], 4, @UA_TYPES[UA_TYPES_INT16]);
  AssertTrue(OPCUAVariantToNumbers(v, vals)=ioOk);
  AssertEquals(4, Length(vals));
  for i:=0 to 3 do AssertEquals(i*-7, vals[i], 0);
end;

procedure TTestOPCUAValues.ADateTimeBecomesALocalTDateTime;
var
  dt:UA_DateTime;
  v, back:UA_Variant;
  vals:TArrayOfDouble;
  utc:TDateTime;
begin
  utc := EncodeDate(2026,5,6)+EncodeTime(7,8,9,10);
  dt := UA_DateTimeFromDateTime(utc);
  UA_Variant_init(v);
  UA_Variant_setScalar(@v, @dt, @UA_TYPES[UA_TYPES_DATETIME]);
  AssertTrue(OPCUAVariantToNumbers(v, vals)=ioOk);
  AssertEquals('local time', UniversalTimeToLocal(utc), vals[0], 1/86400000);
  AssertTrue(OPCUANumbersToVariant(@UA_TYPES[UA_TYPES_DATETIME], false, vals, back)=ioOk);
  AssertTrue('round trip', Abs(PUA_DateTime(back.data)^-dt)<UA_DATETIME_MSEC);
  UA_Variant_clear(back);
end;

procedure TTestOPCUAValues.TextAndNumbersBecomeText;
var
  v:UA_Variant;
  s:AnsiString;
  us:UA_String;
  lt:UA_LocalizedText;
  d:UA_Double;
  b:UA_Boolean;
  t:UTF8String;
begin
  s := 'Ol'#$C3#$A1' mundo'; //UTF-8 bytes
  us := _UA_STRING(s);
  UA_Variant_init(v);
  UA_Variant_setScalar(@v, @us, @UA_TYPES[UA_TYPES_STRING]);
  AssertTrue(OPCUAVariantToText(v, t)=ioOk);
  AssertEquals('string', 'Ol'#$C3#$A1' mundo', RawByteString(t));

  FillChar(lt, SizeOf(lt), 0);
  lt.text := _UA_STRING(s);
  UA_Variant_setScalar(@v, @lt, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  AssertTrue(OPCUAVariantToText(v, t)=ioOk);
  AssertEquals('localized text', 'Ol'#$C3#$A1' mundo', RawByteString(t));

  d := 2.75;
  UA_Variant_setScalar(@v, @d, @UA_TYPES[UA_TYPES_DOUBLE]);
  AssertTrue(OPCUAVariantToText(v, t)=ioOk);
  AssertEquals('double, with a dot', '2.75', t);

  b := false;
  UA_Variant_setScalar(@v, @b, @UA_TYPES[UA_TYPES_BOOLEAN]);
  AssertTrue(OPCUAVariantToText(v, t)=ioOk);
  AssertEquals('boolean', 'False', t);

  AssertEquals('bytes padded with zeros', 6, Length(OPCUATextToValues('abc', 6)));
  AssertEquals('and back', 'abc', OPCUAValuesToText(OPCUATextToValues('abc', 6)));
end;

procedure TTestOPCUAValues.AValueOutsideTheTypeIsRefused;
var
  v:UA_Variant;
  vals:TArrayOfDouble;
begin
  SetLength(vals, 1);
  vals[0] := 300;
  AssertTrue('byte 300', OPCUANumbersToVariant(@UA_TYPES[UA_TYPES_BYTE], false, vals, v)=ioIllegalValue);
  vals[0] := -40000;
  AssertTrue('int16 -40000', OPCUANumbersToVariant(@UA_TYPES[UA_TYPES_INT16], false, vals, v)=ioIllegalValue);
  vals[0] := -1;
  AssertTrue('uint32 -1', OPCUANumbersToVariant(@UA_TYPES[UA_TYPES_UINT32], false, vals, v)=ioIllegalValue);
  vals[0] := NaN;
  AssertTrue('NaN', OPCUANumbersToVariant(@UA_TYPES[UA_TYPES_INT32], false, vals, v)=ioIllegalValue);
  vals[0] := 1;
  AssertTrue('a string node is not numeric', OPCUANumbersToVariant(@UA_TYPES[UA_TYPES_STRING], false, vals, v)=ioIllegalValue);
end;

procedure TTestOPCUAValues.NumbersBuildScalarsAndArraysOfTheNodeType;
var
  v:UA_Variant;
  vals:TArrayOfDouble;
begin
  SetLength(vals, 3);
  vals[0] := 1.6; vals[1] := -2; vals[2] := 3;
  AssertTrue(OPCUANumbersToVariant(@UA_TYPES[UA_TYPES_INT32], false, vals, v)=ioOk);
  AssertTrue('scalar', UA_Variant_hasScalarType(@v, @UA_TYPES[UA_TYPES_INT32]));
  AssertEquals('rounded', 2, PUA_Int32(v.data)^);
  UA_Variant_clear(v);
  AssertTrue(OPCUANumbersToVariant(@UA_TYPES[UA_TYPES_FLOAT], true, vals, v)=ioOk);
  AssertTrue('array', UA_Variant_hasArrayType(@v, @UA_TYPES[UA_TYPES_FLOAT]));
  AssertEquals(3, v.arrayLength);
  AssertEquals(-2, PUA_Float(v.data)[1], 0);
  UA_Variant_clear(v);
  vals[0] := 0.4;
  AssertTrue(OPCUANumbersToVariant(@UA_TYPES[UA_TYPES_BOOLEAN], false, vals, v)=ioOk);
  AssertTrue('non zero is true', PUA_Boolean(v.data)^);
  UA_Variant_clear(v);
end;

procedure TTestOPCUAValues.TextIsParsedForANumericNode;
var
  v:UA_Variant;
begin
  AssertTrue(OPCUATextToVariant(@UA_TYPES[UA_TYPES_DOUBLE], '12.5', v)=ioOk);
  AssertEquals(12.5, PUA_Double(v.data)^, 0);
  UA_Variant_clear(v);
  AssertTrue(OPCUATextToVariant(@UA_TYPES[UA_TYPES_BOOLEAN], 'true', v)=ioOk);
  AssertTrue(PUA_Boolean(v.data)^);
  UA_Variant_clear(v);
  AssertTrue('not a number', OPCUATextToVariant(@UA_TYPES[UA_TYPES_INT32], 'abc', v)=ioIllegalValue);
  AssertTrue(OPCUATextToVariant(@UA_TYPES[UA_TYPES_STRING], 'xyz', v)=ioOk);
  AssertEquals('xyz', UA_StringToStr(PUA_String(v.data)^));
  UA_Variant_clear(v);
end;

procedure TTestOPCUAValues.TheAddressSplitsOnlyOnAValidIndexRange;
var
  n, r:AnsiString;
begin
  OPCUASplitAddress('ns=3;s="DB1"."Arr"|4', n, r);
  AssertEquals('ns=3;s="DB1"."Arr"', n);
  AssertEquals('4', r);
  OPCUASplitAddress(' ns=2;i=7 | 0:3 ', n, r);
  AssertEquals('ns=2;i=7', n);
  AssertEquals('0:3', r);
  OPCUASplitAddress('ns=2;s=A|B', n, r);
  AssertEquals('a "|" that is part of the name', 'ns=2;s=A|B', n);
  AssertEquals('', r);
  OPCUASplitAddress('ns=2;s=Plain', n, r);
  AssertEquals('ns=2;s=Plain', n);
  AssertEquals('', r);
end;

procedure TTestOPCUAValues.StatusCodesMapToIOResults;
begin
  AssertTrue(OPCUAStatusToIOResult(UA_STATUSCODE_GOOD)=ioOk);
  AssertTrue('uncertain is a value', OPCUAStatusToIOResult(UA_STATUSCODE_UNCERTAIN)=ioOk);
  AssertTrue(OPCUAStatusToIOResult(UA_STATUSCODE_BADNODEIDUNKNOWN)=ioObjectNotExists);
  AssertTrue(OPCUAStatusToIOResult(UA_STATUSCODE_BADNOTWRITABLE)=ioObjectAccessNotAllowed);
  AssertTrue(OPCUAStatusToIOResult(UA_STATUSCODE_BADUSERACCESSDENIED)=ioObjectAccessNotAllowed);
  AssertTrue(OPCUAStatusToIOResult(UA_STATUSCODE_BADTYPEMISMATCH)=ioIllegalValue);
  AssertTrue(OPCUAStatusToIOResult(UA_STATUSCODE_BADIDENTITYTOKENREJECTED)=ioAuthError);
  AssertTrue(OPCUAStatusToIOResult(UA_STATUSCODE_BADTIMEOUT)=ioTimeOut);
  AssertTrue(OPCUAStatusToIOResult(UA_STATUSCODE_BADCONNECTIONCLOSED)=ioCommError);
  AssertTrue(OPCUAIsConnectionLost(UA_STATUSCODE_BADSESSIONIDINVALID));
  AssertFalse(OPCUAIsConnectionLost(UA_STATUSCODE_BADNODEIDUNKNOWN));
end;

{ TOPCUACase }

procedure TOPCUACase.SetUp;
begin
  inherited SetUp;
  //first: an Ignore raised by SetUp skips TearDown, so nothing may be created before it
  LibOk(Self);
  FOwned := TList.Create;
  FLog := TStringList.Create;
end;

procedure TOPCUACase.TearDown;
var
  i:Integer;
begin
  if FOwned<>nil then
    for i:=FOwned.Count-1 downto 0 do
      TComponent(FOwned[i]).Free;
  FreeAndNil(FOwned);
  FreeAndNil(FDriver);
  StopServer;
  if (FLog<>nil) and (GetEnvironmentVariable('OPCUA_TEST_DEBUG')='1') then
    WriteLn(FLog.Text);
  FreeAndNil(FLog);
  inherited TearDown;
end;

procedure TOPCUACase.StartServer(const Opt:TOPCUATestServerOptions);
begin
  FServer := TOPCUATestServer.Create(Opt);
end;

procedure TOPCUACase.StopServer;
begin
  FreeAndNil(FServer);
end;

procedure TOPCUACase.DriverLog(Sender:TObject; Level:TOPCUALogLevel; const Msg:String);
begin
  if GetEnvironmentVariable('OPCUA_TEST_DEBUG')='1' then
    WriteLn(StdErr, FormatDateTime('nn:ss.zzz', Now), ' [driver] ', Msg);
  FLog.Add(Msg);
end;

function TOPCUACase.NewDriver:TOPCUADriver;
begin
  FDriver := TOPCUADriver.Create(nil);
  FDriver.LibraryName := GetEnvironmentVariable('OPEN62541_LIB');
  FDriver.EndpointUrl := FServer.Url;
  FDriver.CertificateDir := IncludeTrailingPathDelimiter(GetTempDir(false))+'pascalscada-opcua-test-pki';
  FDriver.ReconnectInterval := 300;
  FDriver.OnLog := @DriverLog;
  FDriver.LogLevel := ollInfo;
  Result := FDriver;
end;

function TOPCUACase.Own(C:TComponent):TComponent;
begin
  FOwned.Add(C);
  Result := C;
end;

function TOPCUACase.NumTag(const Address:String; Refresh:Integer):TPLCTagNumber;
begin
  Result := TPLCTagNumber(Own(TPLCTagNumber.Create(nil)));
  Result.RefreshTime := Refresh;
  Result.LongAddress := Address;
  Result.ProtocolDriver := FDriver;
end;

function TOPCUACase.BlockTag(const Address:String; Size:Integer; Refresh:Integer):TPLCBlock;
begin
  Result := TPLCBlock(Own(TPLCBlock.Create(nil)));
  Result.RefreshTime := Refresh;
  Result.Size := Size;
  Result.LongAddress := Address;
  Result.ProtocolDriver := FDriver;
end;

function TOPCUACase.TextTag(const Address:String; Size:Integer; Refresh:Integer):TPLCString;
begin
  Result := TPLCString(Own(TPLCString.Create(nil)));
  Result.RefreshTime := Refresh;
  Result.StringType := stC;
  Result.StringSize := Size;
  Result.LongAddress := Address;
  Result.ProtocolDriver := FDriver;
end;

procedure TOPCUACase.Pump(Ms:Integer);
var
  t:QWord;
begin
  t := GetTickCount64;
  while GetTickCount64-t<QWord(Ms) do begin
    Sleep(5);
    CheckSynchronize;
  end;
end;

function TOPCUACase.WaitRaw(B:TPLCBlock; Idx:Integer; V:Double; Ms:Integer):Boolean;
var
  t:QWord;
begin
  t := GetTickCount64;
  repeat
    CheckSynchronize;
    if SameValue(B.ValueRaw[Idx], V, 1e-9) then exit(true);
    Sleep(5);
  until GetTickCount64-t>=QWord(Ms);
  Result := SameValue(B.ValueRaw[Idx], V, 1e-9);
end;

function TOPCUACase.WaitStatus(T:TPLCTagNumber; R:TProtocolIOResult; Ms:Integer):Boolean;
var
  tt:QWord;
begin
  tt := GetTickCount64;
  repeat
    CheckSynchronize;
    if T.LastASyncReadStatus=R then exit(true);
    Sleep(5);
  until GetTickCount64-tt>=QWord(Ms);
  Result := T.LastASyncReadStatus=R;
end;

function TOPCUACase.WaitNotOk(T:TPLCTagNumber; Ms:Integer):Boolean;
var
  tt:QWord;
begin
  tt := GetTickCount64;
  repeat
    CheckSynchronize;
    if T.LastASyncReadStatus<>ioOk then exit(true);
    Sleep(5);
  until GetTickCount64-tt>=QWord(Ms);
  Result := T.LastASyncReadStatus<>ioOk;
end;

function TOPCUACase.WaitValue(T:TPLCTagNumber; V:Double; Ms:Integer):Boolean;
var
  tt:QWord;
begin
  tt := GetTickCount64;
  repeat
    CheckSynchronize;
    if SameValue(T.Value, V, 1e-6) then exit(true);
    Sleep(5);
  until GetTickCount64-tt>=QWord(Ms);
  Result := SameValue(T.Value, V, 1e-6);
end;

function TOPCUACase.WaitText(T:TPLCString; const V:String; Ms:Integer):Boolean;
var
  tt:QWord;
begin
  tt := GetTickCount64;
  repeat
    CheckSynchronize;
    if T.Value=V then exit(true);
    Sleep(5);
  until GetTickCount64-tt>=QWord(Ms);
  Result := T.Value=V;
end;

{ TTestOPCUADriver }

procedure TTestOPCUADriver.ReadEveryScalar(Subscriptions:Boolean);
type
  TExp = record N:String; V:Double; end;
const
  Exp:array[0..10] of TExp = (
    (N:'Bool'; V:1), (N:'SByte'; V:-12), (N:'Byte'; V:200), (N:'Int16'; V:-1234),
    (N:'UInt16'; V:54321), (N:'Int32'; V:-123456), (N:'UInt32'; V:3000000000),
    (N:'Int64'; V:-9000000000), (N:'UInt64'; V:9000000000), (N:'Float'; V:1.5), (N:'Double'; V:3.25));
var
  tags:array[0..10] of TPLCTagNumber;
  dt:TPLCTagNumber;
  i:Integer;
  bad:String;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver.UseSubscriptions := Subscriptions;
  for i:=0 to High(Exp) do
    tags[i] := NumTag(FServer.NodeId(Exp[i].N));
  dt := NumTag(FServer.NodeId('DateTime'));
  bad := '';
  for i:=0 to High(Exp) do
    if not WaitValue(tags[i], Exp[i].V, 3000) then
      bad := bad+Exp[i].N+' = '+FloatToStr(Exp[i].V)+' (got '+FloatToStr(tags[i].Value)+', '+
             tags[i].LastASyncReadStatus.ToString+'); ';
  AssertEquals('wrong values ('+FServer.NodeId('Bool')+')', '', bad);
  AssertTrue('DateTime', WaitValue(dt, UniversalTimeToLocal(EncodeDate(2026,1,2)+EncodeTime(3,4,5,0)), 3000));
  AssertTrue('status', tags[0].LastASyncReadStatus=ioOk);
end;

procedure TTestOPCUADriver.EveryScalarTypeIsReadBySubscription;
begin
  ReadEveryScalar(true);
end;

procedure TTestOPCUADriver.EveryScalarTypeIsReadByPolling;
begin
  ReadEveryScalar(false);
end;

procedure TTestOPCUADriver.ArraysAreReadIntoBlocks;
var
  bi, bd:TPLCBlock;
  i:Integer;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  bi := BlockTag(FServer.NodeId('Int32Array'), 10);
  bd := BlockTag(FServer.NodeId('DoubleArray'), 5);
  AssertTrue('int32 array', WaitRaw(bi, 9, 90));
  for i:=0 to 9 do AssertEquals('int '+IntToStr(i), i*10, bi.ValueRaw[i], 0);
  AssertTrue('double array', WaitRaw(bd, 4, 4.5));
  for i:=0 to 4 do AssertEquals('double '+IntToStr(i), i+0.5, bd.ValueRaw[i], 0);
end;

procedure TTestOPCUADriver.AnIndexRangeAddressReadsOneElement;
var
  t:TPLCTagNumber;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  t := NumTag(FServer.NodeId('Int32Array')+'|3');
  AssertTrue('element 3'+LineEnding+FLog.Text, WaitValue(t, 30));
  t.SyncWrites := true;
  t.Value := 333;
  AssertTrue('written: '+t.LastSyncWriteStatus.ToString, t.LastSyncWriteStatus=ioOk);
  AssertTrue('read back', WaitValue(t, 333));
end;

procedure TTestOPCUADriver.TheNamespaceUriAddressWorks;
var
  t:TPLCTagNumber;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  t := NumTag(FServer.NodeIdByUri('Int16'));
  AssertTrue('nsu= address'+LineEnding+FLog.Text, WaitValue(t, -1234));
end;

procedure TTestOPCUADriver.TheDriverMessagesReachTheLogHub;
var
  t:TPLCTagNumber;
  L:TLogCollector;
  i, mine:Integer;
begin
  L := TLogCollector.Create([lcProtocol], llTrace);
  try
    StartServer(OPCUATestDefaultOptions);
    NewDriver;  //LogLevel ollInfo: the client reports its connection
    FDriver.Name := 'OPCUA1';
    t := NumTag(FServer.NodeId('Int16'));
    AssertTrue('connected and read'+LineEnding+FLog.Text, WaitValue(t, -1234));
    AssertTrue('flush', L.Flush);
    mine := 0;
    for i:=0 to L.Count-1 do
      if L[i].SourceClass=TOPCUADriver then begin
        Inc(mine);
        AssertEquals('source', 'OPCUA1', L[i].Source);
        AssertTrue('the same message reached OnLog: '+L[i].Msg, FLog.IndexOf(L[i].Msg)>=0);
        AssertTrue('nothing below the driver LogLevel', L[i].Level>=llInfo);
      end;
    AssertTrue('the driver posted to the hub'+L.Dump, mine>0);
  finally
    L.Free;
  end;
end;

procedure TTestOPCUADriver.ATextTagReadsAndWrites;
var
  t, e:TPLCString;
  raw:RawByteString;
  txt:UTF8String;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  t := TextTag(FServer.NodeId('String'));
  AssertTrue('read: "'+t.Value+'"', WaitText(t, 'hello'));
  //"Olá, OPC UA" built byte by byte (an accented literal depends on the code page of the source)
  raw := 'Ol'#$C3#$A1', OPC UA';
  SetCodePage(raw, CP_UTF8, false);
  txt := raw;
  t.SyncWrites := true;
  t.Value := txt;
  AssertTrue('write status '+t.LastSyncWriteStatus.ToString, t.LastSyncWriteStatus=ioOk);
  AssertTrue('read back: "'+t.Value+'"', WaitText(t, txt));
  //a text tag on a numeric node shows the number
  e := TextTag(FServer.NodeId('Double'));
  AssertTrue('number as text: "'+e.Value+'"', WaitText(e, '3.25'));
  //and an element of an array of strings
  e := TextTag(FServer.NodeId('StringArray')+'|1');
  AssertTrue('string array element: "'+e.Value+'"', WaitText(e, 'two'));
end;

procedure TTestOPCUADriver.EveryScalarTypeIsWritten;
type
  TW = record N:String; V:Double; end;
const
  W:array[0..10] of TW = (
    (N:'Bool'; V:0), (N:'SByte'; V:100), (N:'Byte'; V:7), (N:'Int16'; V:32000),
    (N:'UInt16'; V:3), (N:'Int32'; V:2000000000), (N:'UInt32'; V:4294967295.0),
    (N:'Int64'; V:-123456789012), (N:'UInt64'; V:123456789012), (N:'Float'; V:-0.25), (N:'Double'; V:1e-300));
var
  i:Integer;
  t:TPLCTagNumber;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  for i:=0 to High(W) do begin
    t := NumTag(FServer.NodeId(W[i].N));
    t.SyncWrites := true;
    t.Value := W[i].V;
    AssertTrue(W[i].N+' write: '+t.LastSyncWriteStatus.ToString+LineEnding+FLog.Text, t.LastSyncWriteStatus=ioOk);
    AssertTrue(W[i].N+' read back '+FloatToStr(t.Value), WaitValue(t, W[i].V));
  end;
end;

procedure TTestOPCUADriver.OneElementOfABlockIsWritten;
var
  b:TPLCBlock;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  b := BlockTag(FServer.NodeId('Int32Array'), 10);
  AssertTrue('loaded', WaitRaw(b, 9, 90));
  b.SyncWrites := true;
  b.ValueRaw[4] := -44;
  AssertTrue('write: '+b.LastSyncWriteStatus.ToString+LineEnding+FLog.Text, b.LastSyncWriteStatus=ioOk);
  AssertTrue('element 4', WaitRaw(b, 4, -44));
  AssertEquals('the others stay', 30, b.ValueRaw[3], 0);
  AssertEquals('the others stay', 50, b.ValueRaw[5], 0);
end;

procedure TTestOPCUADriver.ANumberTooBigForTheNodeIsRefused;
var
  t:TPLCTagNumber;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  t := NumTag(FServer.NodeId('Byte'));
  AssertTrue(WaitValue(t, 200));
  t.SyncWrites := true;
  t.Value := 300;
  AssertTrue('status '+t.LastSyncWriteStatus.ToString, t.LastSyncWriteStatus=ioIllegalValue);
end;

procedure TTestOPCUADriver.AReadOnlyNodeRefusesTheWrite;
var
  t:TPLCTagNumber;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  t := NumTag(FServer.NodeId('ReadOnlyInt32'));
  AssertTrue(WaitValue(t, 777));
  t.SyncWrites := true;
  t.Value := 1;
  AssertTrue('status '+t.LastSyncWriteStatus.ToString, t.LastSyncWriteStatus=ioObjectAccessNotAllowed);
end;

procedure TTestOPCUADriver.AnUnknownNodeIsReported;
var
  t, good:TPLCTagNumber;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  t := NumTag(FServer.NodeId('DoesNotExist'));
  good := NumTag(FServer.NodeId('Int32'));
  AssertTrue('the good one reads', WaitValue(good, -123456));
  AssertTrue('the unknown one fails: '+t.LastASyncReadStatus.ToString,
    WaitStatus(t, ioObjectNotExists));
end;

procedure TTestOPCUADriver.TheCounterIsFollowedOnChange;
var
  t:TPLCTagNumber;
  first:Double;
  changes:Integer;
  last:Double;
  t0:QWord;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  t := NumTag(FServer.NodeId('Counter'), 20);
  AssertTrue('first value', WaitNotOk(t, 0) or (t.Value>0) or WaitStatus(t, ioOk));
  Pump(200);
  AssertTrue('counting', t.Value>0);
  first := t.Value;
  last := first;
  changes := 0;
  t0 := GetTickCount64;
  while GetTickCount64-t0<1000 do begin
    CheckSynchronize;
    if t.Value<>last then begin
      AssertTrue('only goes up', t.Value>last);
      last := t.Value;
      Inc(changes);
    end;
    Sleep(5);
  end;
  AssertTrue('it changed several times in a second ('+IntToStr(changes)+')', changes>=5);
end;

procedure TTestOPCUADriver.TheDriverReconnectsAfterTheServerRestarts;
var
  t:TPLCTagNumber;
  opt:TOPCUATestServerOptions;
begin
  opt := OPCUATestDefaultOptions;
  StartServer(opt);
  NewDriver;
  t := NumTag(FServer.NodeId('Int32'));
  AssertTrue(WaitValue(t, -123456));
  t.SyncWrites := true;
  t.Value := 5;
  AssertTrue(WaitValue(t, 5));
  StopServer;
  AssertTrue('the loss is seen: '+t.LastASyncReadStatus.ToString,
    WaitNotOk(t, 8000));
  StartServer(opt);
  //a new server: the start value again
  AssertTrue('reconnected: '+t.LastASyncReadStatus.ToString+LineEnding+FLog.Text, WaitValue(t, -123456, 10000));
  AssertTrue(t.LastASyncReadStatus=ioOk);
end;

procedure TTestOPCUADriver.ListTagsFindsTheTestVariables;
var
  tags:TPLCTagInfoArray;
  i:Integer;
  found:specialize TDictionary<String,TPLCTagInfo>;
  info:TPLCTagInfo;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  AssertTrue('list: '+FDriver.LastError, FDriver.ListTags(tags)=ioOk);
  found := specialize TDictionary<String,TPLCTagInfo>.Create;
  try
    for i:=0 to High(tags) do
      found.AddOrSetValue(tags[i].Name, tags[i]);
    AssertTrue('Int32', found.TryGetValue('Int32', info));
    AssertTrue('a scalar', info.Kind=tkScalar);
    AssertEquals('nsu address', FServer.NodeIdByUri('Int32'), info.Path);
    AssertEquals('Int32', info.NativeType);
    AssertTrue('Int32Array', found.TryGetValue('Int32Array', info));
    AssertTrue('an array', info.Kind=tkArray);
    AssertEquals(10, info.ElementCount);
    AssertTrue('String', found.TryGetValue('String', info));
    AssertTrue('a text', info.Kind=tkString);
    AssertFalse('the Server object is not walked', found.ContainsKey('ServerStatus'));
  finally
    found.Free;
  end;
end;

procedure TTestOPCUADriver.VariableInfoTellsTypeRankAndAccess;
var
  info:TOPCUAVariableInfo;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  AssertTrue(FDriver.GetVariableInfo(FServer.NodeIdByUri('Int32Array'), info)=ioOk);
  AssertEquals('Int32', info.DataTypeName);
  AssertTrue('builtin', info.BuiltinType=@UA_TYPES[UA_TYPES_INT32]);
  AssertEquals('one dimension', 1, info.ValueRank);
  AssertEquals(1, Length(info.ArrayDimensions));
  AssertEquals(10, info.ArrayDimensions[0]);
  AssertTrue('read/write', (info.AccessLevel and UA_ACCESSLEVELMASK_WRITE)<>0);
  AssertTrue(FDriver.GetVariableInfo(FServer.NodeId('ReadOnlyInt32'), info)=ioOk);
  AssertEquals('scalar', -1, info.ValueRank);
  AssertTrue('read only', (info.AccessLevel and UA_ACCESSLEVELMASK_WRITE)=0);
  AssertTrue('unknown', FDriver.GetVariableInfo(FServer.NodeId('Nope'), info)=ioObjectNotExists);
end;

{ TTestOPCUASecurity }

procedure TTestOPCUASecurity.SignAndEncryptConnectsAndReads;
var
  opt:TOPCUATestServerOptions;
  t:TPLCTagNumber;
begin
  opt := OPCUATestDefaultOptions;
  opt.Encrypted := true;
  StartServer(opt);
  NewDriver.SecurityMode := osmSignAndEncrypt;
  FDriver.SecurityPolicy := ospBasic256Sha256;
  t := NumTag(FServer.NodeId('Int32'));
  AssertTrue('read over SignAndEncrypt'+LineEnding+FDriver.LastError+LineEnding+FLog.Text, WaitValue(t, -123456, 10000));
end;

procedure TTestOPCUASecurity.TheRightPasswordOpensTheSession;
var
  opt:TOPCUATestServerOptions;
  t:TPLCTagNumber;
begin
  opt := OPCUATestDefaultOptions;
  opt.Encrypted := true;
  opt.Username := 'operator';
  opt.Password := 's3cr3t';
  StartServer(opt);
  NewDriver.SecurityMode := osmSignAndEncrypt;
  FDriver.Username := 'operator';
  FDriver.Password := 's3cr3t';
  t := NumTag(FServer.NodeId('Int32'));
  AssertTrue('read with user/password'+LineEnding+FDriver.LastError+LineEnding+FLog.Text, WaitValue(t, -123456, 10000));
end;

procedure TTestOPCUASecurity.AWrongPasswordIsAnAuthError;
var
  opt:TOPCUATestServerOptions;
  t:TPLCTagNumber;
begin
  opt := OPCUATestDefaultOptions;
  opt.Encrypted := true;
  opt.Username := 'operator';
  opt.Password := 's3cr3t';
  StartServer(opt);
  NewDriver.SecurityMode := osmSignAndEncrypt;
  FDriver.Username := 'operator';
  FDriver.Password := 'wrong';
  t := NumTag(FServer.NodeId('Int32'));
  AssertTrue('auth error: '+t.LastASyncReadStatus.ToString+LineEnding+FDriver.LastError,
    WaitStatus(t, ioAuthError, 8000));
end;

procedure TTestOPCUASecurity.TheGeneratedClientCertificateIsKept;
var
  c1, c2:TBytes;
  i:Integer;
begin
  StartServer(OPCUATestDefaultOptions);
  NewDriver;
  AssertTrue('generated', FDriver.GetClientCertificate(c1));
  AssertTrue('not empty', Length(c1)>100);
  AssertTrue('again', FDriver.GetClientCertificate(c2));
  AssertEquals('the same', Length(c1), Length(c2));
  for i:=0 to High(c1) do
    if c1[i]<>c2[i] then Fail('the certificate changed at byte '+IntToStr(i));
end;

initialization
  RegisterTest(TTestOPCUAValues);
  RegisterTest(TTestOPCUADriver);
  RegisterTest(TTestOPCUASecurity);
end.
