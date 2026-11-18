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
  PLCStruct, PLCStructElement, PLCStructString,
  OPCUAClient, OPCUAValues, opcuaserver, testsupport.opcuaserver, testsupport.fakeserver, ut.scanlog;

type

  { TClientWriter: writes from its own client and thread (the write ends in the main thread) }

  TClientWriter = class(TThread)
  protected
    procedure Execute; override;
    function  WriteRange(c:PUA_Client; const v:UA_Variant):UA_StatusCode;
  public
    Url, Id:AnsiString;
    Ns:Word;
    TypeIndex:Integer;
    Num:Double;
    Text:UTF8String;
    Values:TArrayOfDouble;   //array writes (with Range: an index range)
    Range:AnsiString;
    Status:UA_StatusCode;
    Elapsed:QWord;
    constructor Create(const AUrl:AnsiString; ANs:Word; const AId:AnsiString; ATypeIndex:Integer;
                       ANum:Double; const AText:UTF8String = ''; const AValues:TArrayOfDouble = nil;
                       const ARange:AnsiString = '');
  end;

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
    function  StartWriter(const Id:String; TypeIndex:Integer; Num:Double; const Text:UTF8String = ''):TClientWriter;
    function  Finish(W:TClientWriter; Ms:Integer = 5000):UA_StatusCode;
    function  WriteNode(const Id:String; TypeIndex:Integer; Num:Double; const Text:UTF8String = ''):UA_StatusCode;
    function  NewDriverTag(const AName:String; TT:TTagType; out D:TLogTestDriver):TPLCTagNumber;
    function  ReadArray(const Id, Range:String; out TypeIndex:Integer; out Vals:TArrayOfDouble):UA_StatusCode;
    function  NewBlock(const AName:String; TT:TTagType; Size:Integer):TPLCBlock;
    function  NewStruct(out S:TPLCStruct; out W:TPLCStructItem; out F:TPLCStructItem; out Txt:TPLCStructString):TOPCUAServerItem;
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
    procedure AnItemThatCannotBeAddedIsReportedAndTheOthersPublished;
    procedure APortInUseDoesNotStart;
    procedure ItStopsAndStartsAgain;

    //writes (P3)
    procedure AWriteToAMemoryTagIsGood;
    procedure AWriteReachesTheDeviceAndTheClientWaitsForIt;
    procedure ADeviceFailureReachesTheClient;
    procedure ATypeMismatchIsRefused;
    procedure AValueOutOfTheTagLimitsIsRefusedWithoutWriting;
    procedure ScaledDoubleWritesTheRawValue;
    procedure ASingleBitIsWrittenIntoItsWord;
    procedure ABitRangeIsWrittenIntoItsWord;
    procedure AStringIsWritten;
    procedure TheServerAnswersOthersWhileAWriteIsPending;
    procedure ARemovedItemEndsItsPendingWrite;
    procedure TheWrittenValueIsReadBackAtOnce;

    //blocks and structures (P4)
    procedure ABlockIsAnArrayOfItsType;
    procedure AnIndexRangeReadsPartOfTheArray;
    procedure AWholeArrayIsWritten;
    procedure AnIndexRangeWritesPartOfTheArray;
    procedure AStructureIsAnObjectWithItsItems;
    procedure AStructItemIsWrittenWithItsOwnType;
    procedure AStringInsideAStructureIsWritten;
    procedure RemovingTheStructureRemovesItsItems;
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

{ TClientWriter }

constructor TClientWriter.Create(const AUrl:AnsiString; ANs:Word; const AId:AnsiString;
  ATypeIndex:Integer; ANum:Double; const AText:UTF8String; const AValues:TArrayOfDouble;
  const ARange:AnsiString);
begin
  Values := Copy(AValues);
  Range := ARange;
  Url := AUrl;
  Ns := ANs;
  Id := AId;
  TypeIndex := ATypeIndex;
  Num := ANum;
  Text := AText;
  Status := UA_STATUSCODE_BADINTERNALERROR;
  FreeOnTerminate := false;
  inherited Create(false);
end;

function TClientWriter.WriteRange(c:PUA_Client; const v:UA_Variant):UA_StatusCode;
var
  item:UA_WriteValue;
  req:UA_WriteRequest;
  resp:UA_WriteResponse;
begin
  FillChar(item, SizeOf(item), 0);
  item.nodeId := UA_NODEID_STRING(Ns, Id);
  item.attributeId := Ord(UA_ATTRIBUTEID_VALUE);
  item.indexRange := _UA_STRING(Range);
  item.value.value := v;
  item.value.flags := UA_DATAVALUE_HASVALUE;
  UA_WriteRequest_init(req);
  req.nodesToWrite := @item;
  req.nodesToWriteSize := 1;
  resp := UA_Client_Service_write(c, req);
  Result := resp.responseHeader.serviceResult;
  if (Result=UA_STATUSCODE_GOOD) and (resp.resultsSize=1) then
    Result := resp.results^;
  UA_WriteResponse_clear(resp);
end;

procedure TClientWriter.Execute;
var
  c:PUA_Client;
  v:UA_Variant;
  vals:TArrayOfDouble;
  t0:QWord;
begin
  OPCUAMaskFPU;
  c := UA_Client_new();
  try
    UA_ClientConfig_setDefault(UA_Client_getConfig(c));
    Status := UA_Client_connect(c, PAnsiChar(Url));
    if Status<>UA_STATUSCODE_GOOD then exit;
    if TypeIndex=UA_TYPES_STRING then
      OPCUATextToVariant(@UA_TYPES[TypeIndex], Text, v)
    else if Values<>nil then
      OPCUANumbersToVariant(@UA_TYPES[TypeIndex], true, Values, v)
    else begin
      SetLength(vals, 1);
      vals[0] := Num;
      OPCUANumbersToVariant(@UA_TYPES[TypeIndex], false, vals, v);
    end;
    t0 := GetTickCount64;
    if Range<>'' then
      Status := WriteRange(c, v)
    else
      Status := UA_Client_writeValueAttribute(c, UA_NODEID_STRING(Ns, Id), @v);
    Elapsed := GetTickCount64-t0;
    UA_clear(@v, @UA_TYPES[UA_TYPES_VARIANT]);
    UA_Client_disconnect(c);
  finally
    UA_Client_delete(c);
  end;
end;

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

function TTestOPCUAServer.StartWriter(const Id:String; TypeIndex:Integer; Num:Double;
  const Text:UTF8String):TClientWriter;
begin
  Result := TClientWriter.Create('opc.tcp://localhost:'+IntToStr(FServer.Port), FServer.NamespaceIndex,
                                 Id, TypeIndex, Num, Text);
end;

function TTestOPCUAServer.Finish(W:TClientWriter; Ms:Integer):UA_StatusCode;
var
  t0:QWord;
begin
  //the write is done in this (the main) thread: keep its queue going
  t0 := GetTickCount64;
  while (not W.Finished) and (GetTickCount64-t0<QWord(Ms)) do
    CheckSynchronize(5);
  W.WaitFor;
  Result := W.Status;
  W.Free;
end;

function TTestOPCUAServer.WriteNode(const Id:String; TypeIndex:Integer; Num:Double;
  const Text:UTF8String):UA_StatusCode;
begin
  Result := Finish(StartWriter(Id, TypeIndex, Num, Text));
end;

function TTestOPCUAServer.NewDriverTag(const AName:String; TT:TTagType; out D:TLogTestDriver):TPLCTagNumber;
begin
  D := TLogTestDriver.Create(FOwner);
  Result := NewTag(AName, TT);
  Result.RefreshTime := 20;
  Result.ProtocolDriver := D;
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

procedure TTestOPCUAServer.AnItemThatCannotBeAddedIsReportedAndTheOthersPublished;
begin
  //two items with the same NodeId: the second can not be added
  Publish(NewTag('Dup1', pttWord), '', 'Same');
  Publish(NewTag('Dup2', pttWord), '', 'Same');
  Publish(NewTag('Fine', pttWord));
  Start;
  StatusOf('Fine'); //the server has applied the commands
  AssertTrue('reported: '+FServer.LastError, Pos('Same', FServer.LastError)>0);
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

procedure TTestOPCUAServer.AWriteToAMemoryTagIsGood;
var
  T:TPLCTagNumber;
begin
  T := NewTag('Setpoint', pttWord);
  Publish(T).Writable := true;
  Start;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Setpoint', UA_TYPES_UINT16, 42)));
  AssertEquals('the tag has it', 42, T.Value, 0);
end;

procedure TTestOPCUAServer.AWriteReachesTheDeviceAndTheClientWaitsForIt;
var
  T:TPLCTagNumber;
  D:TLogTestDriver;
  W:TClientWriter;
  st:UA_StatusCode;
  spent:QWord;
begin
  T := NewDriverTag('Speed', pttByte, D);
  D.WriteDelayMs := 300;
  Publish(T).Writable := true;
  Start;
  W := StartWriter('Speed', UA_TYPES_BYTE, 9);
  try
    while not W.Finished do CheckSynchronize(5);
    st := W.Status;
    spent := W.Elapsed;
  finally
    W.WaitFor;
    W.Free;
  end;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(st));
  AssertTrue('the client waited for the device ('+IntToStr(spent)+' ms)', spent>=250);
  AssertEquals('one device write', 1, D.WriteCount);
  AssertEquals('the value written', 9, D.LastWritten[0], 0);
end;

procedure TTestOPCUAServer.ADeviceFailureReachesTheClient;
var
  T:TPLCTagNumber;
  D:TLogTestDriver;
begin
  T := NewDriverTag('Valve', pttByte, D);
  D.WriteResult := ioCommError;
  Publish(T).Writable := true;
  Start;
  AssertEquals('the device failure', OPCUAStatusText(UA_STATUSCODE_BADCOMMUNICATIONERROR),
               OPCUAStatusText(WriteNode('Valve', UA_TYPES_BYTE, 1)));
  D.WriteResult := ioTimeOut;
  AssertEquals('a timeout too', OPCUAStatusText(UA_STATUSCODE_BADCOMMUNICATIONERROR),
               OPCUAStatusText(WriteNode('Valve', UA_TYPES_BYTE, 1)));
end;

procedure TTestOPCUAServer.ATypeMismatchIsRefused;
var
  T:TPLCTagNumber;
  st:UA_StatusCode;
begin
  T := NewTag('Count', pttWord);
  T.Value := 3;
  Publish(T).Writable := true;
  Start;
  st := WriteNode('Count', UA_TYPES_INT32, 5);
  AssertTrue('refused: '+OPCUAStatusText(st), st<>UA_STATUSCODE_GOOD);
  AssertEquals('tag untouched', 3, T.Value, 0);
end;

procedure TTestOPCUAServer.AValueOutOfTheTagLimitsIsRefusedWithoutWriting;
var
  T:TPLCTagNumber;
  D:TLogTestDriver;
begin
  T := NewDriverTag('Limited', pttByte, D);
  T.EnableMaxValue := true;
  T.MaxValue := 100;
  Publish(T).Writable := true;
  Start;
  AssertEquals('out of the limits', OPCUAStatusText(UA_STATUSCODE_BADOUTOFRANGE),
               OPCUAStatusText(WriteNode('Limited', UA_TYPES_BYTE, 200)));
  AssertEquals('the device was not written', 0, D.WriteCount);
  AssertEquals('inside the limits', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Limited', UA_TYPES_BYTE, 50)));
end;

procedure TTestOPCUAServer.ScaledDoubleWritesTheRawValue;
var
  T:TPLCTagNumber;
  D:TLogTestDriver;
  sp:TLinearScaleProcessor;
begin
  sp := TLinearScaleProcessor.Create(FOwner);
  sp.PLCMin := 0;
  sp.PLCMax := 100;
  sp.SysMin := 0;
  sp.SysMax := 1000;
  T := NewDriverTag('Level', pttByte, D);
  T.ScaleProcessor := sp;
  with Publish(T) do begin
    Writable := true;
    DataType := odtScaledDouble;
  end;
  Start;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Level', UA_TYPES_DOUBLE, 500)));
  AssertEquals('the raw value reached the device', 50, D.LastWritten[0], 0.0001);
end;

procedure TTestOPCUAServer.ASingleBitIsWrittenIntoItsWord;
var
  W:TPLCTagNumber;
  B:TTagBit;
begin
  W := NewTag('Word1', pttWord);
  B := TTagBit.Create(FOwner);
  B.Name := 'Bit1';
  B.PLCTag := W;
  B.StartBit := 1;
  B.EndBit := 1;
  W.Value := 5;
  Publish(B).Writable := true;
  Start;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Bit1', UA_TYPES_BOOLEAN, 1)));
  AssertEquals('5 with bit 1 set', 7, W.Value, 0);
  AssertEquals('cleared again', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Bit1', UA_TYPES_BOOLEAN, 0)));
  AssertEquals('5 again', 5, W.Value, 0);
end;

procedure TTestOPCUAServer.ABitRangeIsWrittenIntoItsWord;
var
  W:TPLCTagNumber;
  B:TTagBit;
begin
  W := NewTag('Word2', pttWord);
  B := TTagBit.Create(FOwner);
  B.Name := 'Nibble1';
  B.PLCTag := W;
  B.StartBit := 4;
  B.EndBit := 7;
  W.Value := $0F0F;
  Publish(B).Writable := true;
  Start;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Nibble1', UA_TYPES_UINT16, 3)));
  AssertEquals('bits 4..7 = 3, the others kept', $0F3F, W.Value, 0);
  AssertEquals('more than 4 bits hold', OPCUAStatusText(UA_STATUSCODE_BADOUTOFRANGE),
               OPCUAStatusText(WriteNode('Nibble1', UA_TYPES_UINT16, 16)));
end;

procedure TTestOPCUAServer.AStringIsWritten;
var
  S:TPLCString;
begin
  S := TPLCString.Create(FOwner);
  S.Name := 'Batch';
  S.StringSize := 20;
  S.Value := 'old';
  Publish(S).Writable := true;
  Start;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Batch', UA_TYPES_STRING, 0, 'new batch')));
  AssertEquals('the tag has it', 'new batch', S.Value);
end;

procedure TTestOPCUAServer.TheServerAnswersOthersWhileAWriteIsPending;
var
  T, O:TPLCTagNumber;
  D:TLogTestDriver;
  W:TClientWriter;
  t0, spent:QWord;
begin
  T := NewDriverTag('Slow', pttByte, D);
  D.WriteDelayMs := 800;
  Publish(T).Writable := true;
  O := NewTag('Other', pttWord);
  O.Value := 11;
  Publish(O);
  Start;
  W := StartWriter('Slow', UA_TYPES_BYTE, 4);
  try
    //let the write reach the device
    t0 := GetTickCount64;
    while (D.WriteCount=0) and (GetTickCount64-t0<300) do CheckSynchronize(5);
    Sleep(50);
    t0 := GetTickCount64;
    AssertEquals('another variable is read', 11, ReadNum('Other'), 0);
    spent := GetTickCount64-t0;
    AssertTrue('right away ('+IntToStr(spent)+' ms)', spent<300);
    AssertFalse('while the write is still pending', W.Finished);
  finally
    AssertEquals('and the write ends good', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(Finish(W)));
  end;
end;

procedure TTestOPCUAServer.ARemovedItemEndsItsPendingWrite;
var
  T:TPLCTagNumber;
  D:TLogTestDriver;
  W:TClientWriter;
  it:TOPCUAServerItem;
  t0:QWord;
begin
  T := NewDriverTag('Going', pttByte, D);
  D.WriteDelayMs := 1500;
  it := Publish(T);
  it.Writable := true;
  Start;
  W := StartWriter('Going', UA_TYPES_BYTE, 2);
  t0 := GetTickCount64;
  while (D.WriteCount=0) and (GetTickCount64-t0<1000) do CheckSynchronize(5);
  //the device is still busy: the item goes away
  t0 := GetTickCount64;
  while (GetTickCount64-t0<100) do CheckSynchronize(5);
  it.Free;
  AssertEquals('ended right away, not after the device', OPCUAStatusText(UA_STATUSCODE_BADNODEIDUNKNOWN),
               OPCUAStatusText(Finish(W, 1000)));
end;

procedure TTestOPCUAServer.TheWrittenValueIsReadBackAtOnce;
var
  T:TPLCTagNumber;
  D:TLogTestDriver;
begin
  T := NewDriverTag('Echo', pttByte, D);
  T.RefreshTime := 5000; //no scan to bring it in
  Publish(T).Writable := true;
  Start;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Echo', UA_TYPES_BYTE, 77)));
  AssertEquals('read back at once', 77, ReadNum('Echo'), 0);
end;

function TTestOPCUAServer.ReadArray(const Id, Range:String; out TypeIndex:Integer;
  out Vals:TArrayOfDouble):UA_StatusCode;
var
  s:AnsiString;
  v:UA_Variant;
  k:Integer;
begin
  CheckSynchronize(0);
  s := Id;
  TypeIndex := -1;
  Vals := nil;
  Result := UA_Client_readValueAttribute_range(FClient, UA_NODEID_STRING(FServer.NamespaceIndex, s), Range, v);
  if v._type<>nil then
    for k:=0 to UA_TYPES_COUNT-1 do
      if v._type=@UA_TYPES[k] then begin
        TypeIndex := k;
        break;
      end;
  if v.data<>nil then
    OPCUAVariantToNumbers(v, Vals);
  UA_clear(@v, @UA_TYPES[UA_TYPES_VARIANT]);
end;

function TTestOPCUAServer.NewBlock(const AName:String; TT:TTagType; Size:Integer):TPLCBlock;
var
  i:Integer;
  vals:TArrayOfDouble;
begin
  Result := TPLCBlock.Create(FOwner);
  Result.Name := AName;
  Result.TagType := TT;
  Result.Size := Size;
  SetLength(vals, Size);
  for i:=0 to Size-1 do vals[i] := i+1;
  Result.ValuesRaw := vals;
end;

//a 12 byte structure: a Word at 0, a Float at 2 and a C string of 4 bytes at 6
function TTestOPCUAServer.NewStruct(out S:TPLCStruct; out W:TPLCStructItem; out F:TPLCStructItem;
  out Txt:TPLCStructString):TOPCUAServerItem;
begin
  S := TPLCStruct.Create(FOwner);
  S.Name := 'Recipe1';
  S.Size := 12;
  W := TPLCStructItem.Create(FOwner);
  W.Name := 'Speed';
  W.PLCBlock := S;
  W.TagType := pttWord;
  W.Index := 0;
  F := TPLCStructItem.Create(FOwner);
  F.Name := 'Ratio';
  F.PLCBlock := S;
  F.TagType := pttFloat;
  F.Index := 2;
  Txt := TPLCStructString.Create(FOwner);
  Txt.Name := 'Code';
  Txt.PLCBlock := S;
  Txt.StringSize := 4;
  Txt.Index := 6;
  W.Value := 1234;
  F.Value := 2.5;
  Txt.Value := 'ab';
  Result := Publish(S);
end;

procedure TTestOPCUAServer.ABlockIsAnArrayOfItsType;
var
  ti:Integer;
  vals:TArrayOfDouble;
begin
  Publish(NewBlock('Block1', pttWord, 5));
  Start;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(ReadArray('Block1', '', ti, vals)));
  AssertEquals('type', String(UA_TYPES[UA_TYPES_UINT16].typeName), String(UA_TYPES[ti].typeName));
  AssertEquals('5 elements', 5, Length(vals));
  AssertEquals('first', 1, vals[0], 0);
  AssertEquals('last', 5, vals[4], 0);
end;

procedure TTestOPCUAServer.AnIndexRangeReadsPartOfTheArray;
var
  ti:Integer;
  vals:TArrayOfDouble;
begin
  Publish(NewBlock('Block2', pttSmallInt, 6));
  Start;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(ReadArray('Block2', '2:4', ti, vals)));
  AssertEquals('3 elements', 3, Length(vals));
  AssertEquals('element 2', 3, vals[0], 0);
  AssertEquals('element 4', 5, vals[2], 0);
  AssertEquals('one element', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(ReadArray('Block2', '5', ti, vals)));
  AssertEquals('it', 6, vals[0], 0);
  AssertEquals('past the end', OPCUAStatusText(UA_STATUSCODE_BADINDEXRANGENODATA),
               OPCUAStatusText(ReadArray('Block2', '9:10', ti, vals)));
end;

procedure TTestOPCUAServer.AWholeArrayIsWritten;
var
  B:TPLCBlock;
  W:TClientWriter;
  vals:TArrayOfDouble;
begin
  B := NewBlock('Block3', pttWord, 4);
  Publish(B).Writable := true;
  Start;
  SetLength(vals, 4);
  vals[0] := 10; vals[1] := 20; vals[2] := 30; vals[3] := 40;
  W := TClientWriter.Create('opc.tcp://localhost:'+IntToStr(FServer.Port), FServer.NamespaceIndex,
                            'Block3', UA_TYPES_UINT16, 0, '', vals);
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(Finish(W)));
  AssertEquals('element 0', 10, B.ValueRaw[0], 0);
  AssertEquals('element 3', 40, B.ValueRaw[3], 0);
end;

procedure TTestOPCUAServer.AnIndexRangeWritesPartOfTheArray;
var
  B:TPLCBlock;
  W:TClientWriter;
  vals:TArrayOfDouble;
  ti:Integer;
  back:TArrayOfDouble;
begin
  B := NewBlock('Block4', pttWord, 5);
  Publish(B).Writable := true;
  Start;
  SetLength(vals, 2);
  vals[0] := 70; vals[1] := 80;
  W := TClientWriter.Create('opc.tcp://localhost:'+IntToStr(FServer.Port), FServer.NamespaceIndex,
                            'Block4', UA_TYPES_UINT16, 0, '', vals, '1:2');
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(Finish(W)));
  AssertEquals('element 0 kept', 1, B.ValueRaw[0], 0);
  AssertEquals('element 1', 70, B.ValueRaw[1], 0);
  AssertEquals('element 2', 80, B.ValueRaw[2], 0);
  AssertEquals('element 3 kept', 4, B.ValueRaw[3], 0);
  ReadArray('Block4', '', ti, back);
  AssertEquals('read back at once', 70, back[1], 0);
end;

procedure TTestOPCUAServer.AStructureIsAnObjectWithItsItems;
var
  S:TPLCStruct;
  W, F:TPLCStructItem;
  Txt:TPLCStructString;
  ti:Integer;
  n:Double;
  t:String;
begin
  NewStruct(S, W, F, Txt);
  Start;
  AssertEquals('the structure is an object (no value)', OPCUAStatusText(UA_STATUSCODE_BADATTRIBUTEIDINVALID),
               OPCUAStatusText(StatusOf('Recipe1')));
  CheckType('Recipe1.Speed', UA_TYPES_UINT16);
  AssertEquals('word', 1234, ReadNum('Recipe1.Speed'), 0);
  CheckType('Recipe1.Ratio', UA_TYPES_FLOAT);
  AssertEquals('float', 2.5, ReadNum('Recipe1.Ratio'), 0.0001);
  CheckType('Recipe1.Code', UA_TYPES_STRING);
  Read('Recipe1.Code', ti, n, t);
  AssertEquals('string', 'ab', t);
end;

procedure TTestOPCUAServer.AStructItemIsWrittenWithItsOwnType;
var
  S:TPLCStruct;
  W, F:TPLCStructItem;
  Txt:TPLCStructString;
begin
  NewStruct(S, W, F, Txt).Writable := true;
  Start;
  AssertEquals('word', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Recipe1.Speed', UA_TYPES_UINT16, 4321)));
  AssertEquals('float', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Recipe1.Ratio', UA_TYPES_FLOAT, 7.25)));
  //encoded in the structure by their types: the neighbours are intact
  AssertEquals('word in the structure', 4321, W.Value, 0);
  AssertEquals('float in the structure', 7.25, F.Value, 0.0001);
  AssertEquals('string untouched', 'ab', Txt.Value);
end;

procedure TTestOPCUAServer.AStringInsideAStructureIsWritten;
var
  S:TPLCStruct;
  W, F:TPLCStructItem;
  Txt:TPLCStructString;
begin
  NewStruct(S, W, F, Txt).Writable := true;
  Start;
  AssertEquals('status', OPCUAStatusText(UA_STATUSCODE_GOOD),
               OPCUAStatusText(WriteNode('Recipe1.Code', UA_TYPES_STRING, 0, 'xyz')));
  AssertEquals('the string', 'xyz', Txt.Value);
  AssertEquals('the word untouched', 1234, W.Value, 0);
end;

procedure TTestOPCUAServer.RemovingTheStructureRemovesItsItems;
var
  S:TPLCStruct;
  W, F:TPLCStructItem;
  Txt:TPLCStructString;
  it:TOPCUAServerItem;
begin
  it := NewStruct(S, W, F, Txt);
  Start;
  AssertEquals('there', OPCUAStatusText(UA_STATUSCODE_GOOD), OPCUAStatusText(StatusOf('Recipe1.Speed')));
  it.Free;
  AssertTrue('its items are gone', WaitStatus('Recipe1.Speed', UA_STATUSCODE_BADNODEIDUNKNOWN));
  AssertTrue('and the object', WaitStatus('Recipe1', UA_STATUSCODE_BADNODEIDUNKNOWN));
end;

initialization
  RegisterTest(TTestOPCUAServer);

end.
