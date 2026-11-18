{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TOPCUAServer: os tags da aplicacao lidos por um cliente OPC UA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um cliente open62541 no mesmo processo le as variaveis publicadas: o tipo
  nativo de cada tag, o valor, a qualidade (status da leitura do tag), as
  pastas e o ciclo de vida (item adicionado/removido, tag destruido).
}
{$ELSE}
{:
  @abstract(TOPCUAServer tests: the application tags read by an OPC UA client.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  An open62541 client in the same process reads the published variables: the
  native type of each tag, the value, the quality (status of the tag's read),
  the folders and the life cycle (item added/removed, tag destroyed).
}
{$ENDIF}
unit ut.opcuaserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, fpcunit, testregistry,
  open62541, Tag, PLCTag, PLCTagNumber, PLCBlock, PLCString, TagBit, LinearScaleProcessor,
  OPCUAClient, OPCUAValues, opcuaserver, testsupport.opcuaserver, testsupport.fakeserver, ut.scanlog;

type

  { TTestOPCUAServer }

  TTestOPCUAServer = class(TTestCase)
  private
    FOwner:TComponent;
    FServer:TOPCUAServer;
    FClient:PUA_Client;
    FFPU:TFPUExceptionMask;
    function  NewTag(const AName:String; TT:TTagType):TPLCTagNumber;
    function  Publish(ATag:TPLCTag; const AFolder:String = ''; const ANodeName:String = ''):TOPCUAServerItem;
    procedure Start;
    procedure Connect;
    function  Read(const Id:String; out TypeIndex:Integer; out Num:Double; out Text:String):UA_StatusCode;
    function  ReadNum(const Id:String):Double;
    function  StatusOf(const Id:String):UA_StatusCode;
    function  WaitStatus(const Id:String; St:UA_StatusCode; Ms:Integer = 3000):Boolean;
    function  WaitNum(const Id:String; V:Double; Ms:Integer = 3000):Boolean;
    procedure CheckType(const Id:String; Expected:Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EachTagTypeBecomesItsNativeType;
    procedure ADefaultTypeWithoutDriverIsDouble;
    procedure ADefaultTypeFollowsTheDriverWordSize;
    procedure ASingleBitIsBooleanAndABitRangeUnsigned;
    procedure AStringTagIsAString;
    procedure ScaledDoublePublishesTheEngineeringValue;
    procedure FoldersAndNodeNamesMakeTheNodeId;
    procedure AValueChangeReachesTheClient;
    procedure TheReadStatusOfTheTagIsTheQuality;
    procedure AnItemAddedWhileRunningIsPublished;
    procedure ARemovedItemDisappears;
    procedure ADestroyedTagDisappears;
    procedure ANotWritableVariableRefusesWrites;
    procedure AnUnsupportedTagIsReportedAndTheOthersPublished;
    procedure APortInUseDoesNotStart;
    procedure ItStopsAndStartsAgain;
  end;

implementation

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF} Sockets;

const
  TIMEOUT = 3000;

{$IFDEF UNIX}
//An ordinary IPv6 listener on the port (open62541 listens on IPv4 and IPv6).
function ListenIPv6(Port:Word):LongInt;
var
  addr:TInetSockAddr6;
  one:LongInt;
begin
  Result := fpSocket(AF_INET6, SOCK_STREAM, 0);
  if Result<0 then exit;
  one := 1;
  fpSetSockOpt(Result, IPPROTO_IPV6, IPV6_V6ONLY, @one, SizeOf(one));
  FillChar(addr, SizeOf(addr), 0);
  addr.sin6_family := AF_INET6;
  addr.sin6_port := htons(Port);
  if (fpBind(Result, @addr, SizeOf(addr))<>0) or (fpListen(Result, 1)<>0) then begin
    CloseSocket(Result);
    Result := -1;
  end;
end;
{$ENDIF}

{ TTestOPCUAServer }

procedure TTestOPCUAServer.SetUp;
var
  why:String;
begin
  if not OPCUATestLibraryAvailable(why) then
    Ignore(why);
  FFPU := OPCUAMaskFPU;
  FOwner := TComponent.Create(nil);
  FOwner.Name := 'Form1';
  FServer := TOPCUAServer.Create(FOwner);
  FServer.Port := OPCUATestNextPort;
  FServer.LibraryName := GetEnvironmentVariable('OPEN62541_LIB');
  FClient := nil;
end;

procedure TTestOPCUAServer.TearDown;
begin
  if FClient<>nil then begin
    UA_Client_disconnect(FClient);
    UA_Client_delete(FClient);
    FClient := nil;
  end;
  FreeAndNil(FOwner);
  OPCUARestoreFPU(FFPU);
end;

function TTestOPCUAServer.NewTag(const AName:String; TT:TTagType):TPLCTagNumber;
begin
  Result := TPLCTagNumber.Create(FOwner);
  Result.Name := AName;
  Result.TagType := TT;
end;

function TTestOPCUAServer.Publish(ATag:TPLCTag; const AFolder:String; const ANodeName:String):TOPCUAServerItem;
begin
  Result := FServer.Items.Add;
  Result.Tag := ATag;
  Result.Folder := AFolder;
  Result.NodeName := ANodeName;
end;

procedure TTestOPCUAServer.Start;
begin
  FServer.Active := true;
  AssertTrue('server running: '+FServer.LastError, FServer.Running);
  Connect;
end;

procedure TTestOPCUAServer.Connect;
var
  st:UA_StatusCode;
begin
  FClient := UA_Client_new();
  UA_ClientConfig_setDefault(UA_Client_getConfig(FClient));
  st := UA_Client_connect(FClient, PAnsiChar('opc.tcp://localhost:'+IntToStr(FServer.Port)));
  AssertEquals('client connects', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(st));
end;

function TTestOPCUAServer.Read(const Id:String; out TypeIndex:Integer; out Num:Double;
  out Text:String):UA_StatusCode;
var
  s:AnsiString;
  v:UA_Variant;
  vals:TArrayOfDouble;
  t:UTF8String;
  k:Integer;
begin
  //the server applies what the main thread posted between iterations: give it a moment
  CheckSynchronize(0);
  s := Id;
  TypeIndex := -1;
  Num := NaN;
  Text := '';
  Result := UA_Client_readValueAttribute_range(FClient, UA_NODEID_STRING(FServer.NamespaceIndex, s), '', v);
  if v._type<>nil then
    for k:=0 to UA_TYPES_COUNT-1 do
      if v._type=@UA_TYPES[k] then begin
        TypeIndex := k;
        break;
      end;
  if (v.data<>nil) and (OPCUAVariantToNumbers(v, vals)=ioOk) and (Length(vals)>0) then
    Num := vals[0];
  if (v.data<>nil) and (OPCUAVariantToText(v, t)=ioOk) then
    Text := t;
  UA_clear(@v, @UA_TYPES[UA_TYPES_VARIANT]);
end;

function TTestOPCUAServer.ReadNum(const Id:String):Double;
var
  ti:Integer;
  t:String;
begin
  Read(Id, ti, Result, t);
end;

function TTestOPCUAServer.StatusOf(const Id:String):UA_StatusCode;
var
  ti:Integer;
  n:Double;
  t:String;
begin
  Result := Read(Id, ti, n, t);
end;

function TTestOPCUAServer.WaitStatus(const Id:String; St:UA_StatusCode; Ms:Integer):Boolean;
var
  t0:QWord;
begin
  t0 := GetTickCount64;
  repeat
    if StatusOf(Id)=St then exit(true);
    CheckSynchronize(10);
  until GetTickCount64-t0>=QWord(Ms);
  Result := false;
end;

function TTestOPCUAServer.WaitNum(const Id:String; V:Double; Ms:Integer):Boolean;
var
  t0:QWord;
begin
  t0 := GetTickCount64;
  repeat
    if SameValue(ReadNum(Id), V) then exit(true);
    CheckSynchronize(10);
  until GetTickCount64-t0>=QWord(Ms);
  Result := false;
end;

procedure TTestOPCUAServer.CheckType(const Id:String; Expected:Integer);
var
  ti:Integer;
  n:Double;
  t:String;
  st:UA_StatusCode;
begin
  st := Read(Id, ti, n, t);
  AssertEquals(Id+' status', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(st));
  AssertEquals(Id+' type', String(UA_TYPES[Expected].typeName), String(UA_TYPES[ti].typeName));
end;

procedure TTestOPCUAServer.EachTagTypeBecomesItsNativeType;
const
  Types:array[0..9] of TTagType = (pttShortInt, pttByte, pttSmallInt, pttWord, pttLongInt,
                                   pttDWord, pttFloat, pttInt64, pttQWord, pttDouble);
  Expected:array[0..9] of Integer = (UA_TYPES_SBYTE, UA_TYPES_BYTE, UA_TYPES_INT16, UA_TYPES_UINT16,
                                     UA_TYPES_INT32, UA_TYPES_UINT32, UA_TYPES_FLOAT, UA_TYPES_INT64,
                                     UA_TYPES_UINT64, UA_TYPES_DOUBLE);
  Values:array[0..9] of Double = (-5, 200, -1234, 54321, -123456, 3000000000, 1.5, -9000000000, 9000000000, 3.25);
var
  i:Integer;
  T:TPLCTagNumber;
begin
  for i:=0 to High(Types) do begin
    T := NewTag('T'+IntToStr(i), Types[i]);
    T.Value := Values[i];
    Publish(T);
  end;
  Start;
  for i:=0 to High(Types) do begin
    CheckType('T'+IntToStr(i), Expected[i]);
    AssertEquals('T'+IntToStr(i)+' value', Values[i], ReadNum('T'+IntToStr(i)), 0.0001);
  end;
end;

procedure TTestOPCUAServer.ADefaultTypeWithoutDriverIsDouble;
var
  T:TPLCTagNumber;
begin
  T := NewTag('Mem', pttDefault);
  T.Value := 2.5;
  Publish(T);
  Start;
  CheckType('Mem', UA_TYPES_DOUBLE);
  AssertEquals('value', 2.5, ReadNum('Mem'), 0.0001);
end;

procedure TTestOPCUAServer.ADefaultTypeFollowsTheDriverWordSize;
var
  T:TPLCTagNumber;
  D:TLogTestDriver;
begin
  D := TLogTestDriver.Create(FOwner);
  D.Value := 7;
  T := NewTag('ByDriver', pttDefault);
  T.RefreshTime := 20;
  T.ProtocolDriver := D;  //its word size is 8 bits
  Publish(T);
  Start;
  AssertTrue('value read', WaitNum('ByDriver', 7));
  CheckType('ByDriver', UA_TYPES_BYTE);
end;

procedure TTestOPCUAServer.ASingleBitIsBooleanAndABitRangeUnsigned;
var
  W:TPLCTagNumber;
  B0, B3:TTagBit;
begin
  W := NewTag('Word1', pttWord);
  B0 := TTagBit.Create(FOwner);
  B0.Name := 'Bit0';
  B0.PLCTag := W;
  B0.StartBit := 0;
  B0.EndBit := 0;
  B3 := TTagBit.Create(FOwner);
  B3.Name := 'Bits0to2';
  B3.PLCTag := W;
  B3.StartBit := 0;
  B3.EndBit := 2;
  //a bit tag takes its value when the source changes
  W.Value := 5;
  Publish(B0);
  Publish(B3);
  Start;
  CheckType('Bit0', UA_TYPES_BOOLEAN);
  AssertEquals('bit 0 of 5', 1, ReadNum('Bit0'), 0);
  CheckType('Bits0to2', UA_TYPES_UINT16);
  AssertEquals('bits 0..2 of 5', 5, ReadNum('Bits0to2'), 0);
end;

procedure TTestOPCUAServer.AStringTagIsAString;
var
  S:TPLCString;
  ti:Integer;
  n:Double;
  txt:String;
begin
  S := TPLCString.Create(FOwner);
  S.Name := 'Recipe';
  S.StringSize := 20;
  S.Value := 'abc';
  Publish(S);
  Start;
  CheckType('Recipe', UA_TYPES_STRING);
  Read('Recipe', ti, n, txt);
  AssertEquals('text', 'abc', txt);
end;

procedure TTestOPCUAServer.ScaledDoublePublishesTheEngineeringValue;
var
  T:TPLCTagNumber;
  sp:TLinearScaleProcessor;
  it:TOPCUAServerItem;
begin
  sp := TLinearScaleProcessor.Create(FOwner);
  sp.PLCMin := 0;
  sp.PLCMax := 100;
  sp.SysMin := 0;
  sp.SysMax := 1000;
  T := NewTag('Level', pttWord);
  T.ScaleProcessor := sp;
  T.ValueRaw := 10;
  Publish(T, '', 'LevelRaw');
  it := Publish(T, '', 'LevelScaled');
  it.DataType := odtScaledDouble;
  Start;
  CheckType('LevelRaw', UA_TYPES_UINT16);
  AssertEquals('raw', 10, ReadNum('LevelRaw'), 0);
  CheckType('LevelScaled', UA_TYPES_DOUBLE);
  AssertEquals('engineering value', 100, ReadNum('LevelScaled'), 0.0001);
end;

procedure TTestOPCUAServer.FoldersAndNodeNamesMakeTheNodeId;
var
  T:TPLCTagNumber;
  it:TOPCUAServerItem;
begin
  T := NewTag('PumpSpeed', pttWord);
  T.Value := 3;
  it := Publish(T, 'Line1/Pumps', 'Speed');
  AssertEquals('node id text', 'Line1.Pumps.Speed', it.NodeIdText);
  Publish(NewTag('Other', pttWord));
  Start;
  AssertEquals('in its folder', 3, ReadNum('Line1.Pumps.Speed'), 0);
  AssertEquals('the folder is a node', OPCUAStatusText(UA_STATUSCODE_BADATTRIBUTEIDINVALID),
               OPCUAStatusText(StatusOf('Line1.Pumps')));
  AssertEquals('default name is the tag name', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(StatusOf('Other')));
end;

procedure TTestOPCUAServer.AValueChangeReachesTheClient;
var
  T:TPLCTagNumber;
begin
  T := NewTag('Temp', pttSmallInt);
  T.Value := 20;
  Publish(T);
  Start;
  AssertEquals('first', 20, ReadNum('Temp'), 0);
  T.Value := -7;
  AssertTrue('the new value reaches the client', WaitNum('Temp', -7));
end;

procedure TTestOPCUAServer.TheReadStatusOfTheTagIsTheQuality;
var
  T:TPLCTagNumber;
  D:TLogTestDriver;
begin
  D := TLogTestDriver.Create(FOwner);
  D.Value := 3;
  D.ReadResult := ioCommError;
  T := NewTag('Remote', pttByte); //the test driver reads bytes
  T.RefreshTime := 20;
  T.ProtocolDriver := D;
  Publish(T);
  Start;
  AssertTrue('a failing read is BadCommunicationError',
             WaitStatus('Remote', UA_STATUSCODE_BADCOMMUNICATIONERROR));
  D.ReadResult := ioOk;
  AssertTrue('a good read is Good again', WaitStatus('Remote', UA_STATUSCODE_GOOD));
  AssertEquals('with the value', 3, ReadNum('Remote'), 0);
end;

procedure TTestOPCUAServer.AnItemAddedWhileRunningIsPublished;
var
  T:TPLCTagNumber;
begin
  Publish(NewTag('First', pttWord));
  Start;
  AssertEquals('not there yet', OPCUAStatusText(UA_STATUSCODE_BADNODEIDUNKNOWN), OPCUAStatusText(StatusOf('Late')));
  T := NewTag('Late', pttWord);
  T.Value := 9;
  Publish(T);
  AssertTrue('published while running', WaitNum('Late', 9));
end;

procedure TTestOPCUAServer.ARemovedItemDisappears;
var
  it:TOPCUAServerItem;
begin
  it := Publish(NewTag('Gone', pttWord));
  Start;
  AssertEquals('there', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(StatusOf('Gone')));
  it.Free;
  AssertTrue('removed', WaitStatus('Gone', UA_STATUSCODE_BADNODEIDUNKNOWN));
end;

procedure TTestOPCUAServer.ADestroyedTagDisappears;
var
  T:TPLCTagNumber;
begin
  T := NewTag('Temporary', pttWord);
  Publish(T);
  Start;
  AssertEquals('there', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(StatusOf('Temporary')));
  T.Free;
  AssertTrue('removed with its tag', WaitStatus('Temporary', UA_STATUSCODE_BADNODEIDUNKNOWN));
  AssertTrue('and the item lost it', FServer.Items[0].Tag=nil);
end;

procedure TTestOPCUAServer.ANotWritableVariableRefusesWrites;
var
  T:TPLCTagNumber;
  v:UA_Variant;
  x:UA_UInt16;
  s:AnsiString;
  st:UA_StatusCode;
begin
  T := NewTag('Fixed', pttWord);
  T.Value := 1;
  Publish(T);
  Start;
  x := 2;
  UA_Variant_init(v);
  UA_Variant_setScalar(@v, @x, @UA_TYPES[UA_TYPES_UINT16]);
  s := 'Fixed';
  st := UA_Client_writeValueAttribute(FClient, UA_NODEID_STRING(FServer.NamespaceIndex, s), @v);
  AssertTrue('refused: '+OPCUAStatusText(st), st<>UA_STATUSCODE_GOOD);
  AssertEquals('tag untouched', 1, T.Value, 0);
end;

procedure TTestOPCUAServer.AnUnsupportedTagIsReportedAndTheOthersPublished;
var
  B:TPLCBlock;
begin
  B := TPLCBlock.Create(FOwner);
  B.Name := 'Block1';
  Publish(B);
  Publish(NewTag('Fine', pttWord));
  Start;
  AssertTrue('reported: '+FServer.LastError, Pos('not supported', FServer.LastError)>0);
  AssertEquals('the other one is there', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(StatusOf('Fine')));
end;

procedure TTestOPCUAServer.APortInUseDoesNotStart;
var
  other:TServidorDeTeste;
  v6:LongInt;
begin
  //ordinary listeners, IPv4 and IPv6 (no SO_REUSEPORT: open62541 servers share a port
  //with each other)
  other := TServidorDeTeste.Create;
  v6 := -1;
  try
    {$IFDEF UNIX}
    v6 := ListenIPv6(other.Porta);
    {$ENDIF}
    FServer.Port := other.Porta;
    FServer.Active := true;
    AssertFalse('not running', FServer.Running);
    AssertFalse('Active went back to false', FServer.Active);
    AssertTrue('the reason: '+FServer.LastError, FServer.LastError<>'');
  finally
    {$IFDEF UNIX}
    if v6>=0 then CloseSocket(v6);
    {$ENDIF}
    other.Free;
  end;
end;

procedure TTestOPCUAServer.ItStopsAndStartsAgain;
var
  T:TPLCTagNumber;
begin
  T := NewTag('Again', pttWord);
  T.Value := 4;
  Publish(T);
  Start;
  UA_Client_disconnect(FClient);
  UA_Client_delete(FClient);
  FClient := nil;
  FServer.Active := false;
  AssertFalse('stopped', FServer.Running);
  T.Value := 6;
  Start;
  AssertEquals('published again, current value', 6, ReadNum('Again'), 0);
end;

initialization
  RegisterTest(TTestOPCUAServer);

end.
