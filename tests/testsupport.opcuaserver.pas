{
  Servidor OPC UA de teste, no próprio processo (open62541 pelo binding fpopen62541).
  In-process OPC UA test server (open62541 through the fpopen62541 binding).

  Namespace "urn:pascalscada:test", NodeIds de texto / string NodeIds "Test.<Nome>":
    Bool, SByte, Byte, Int16, UInt16, Int32, UInt32, Int64, UInt64, Float, Double,
    String, DateTime, Int32Array[10], DoubleArray[5], StringArray[3],
    ReadOnlyInt32 (só leitura / read only), Counter (muda a cada 50 ms / changes every 50 ms).
}
unit testsupport.opcuaserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, open62541;

type
  TOPCUATestServerOptions = record
    Port:Word;
    Encrypted:Boolean;  //Basic256Sha256 & co, besides None
    Username, Password:AnsiString; //non empty: anonymous sessions are refused
  end;

  { TOPCUATestServer }

  TOPCUATestServer = class(TThread)
  private
    FServer:PUA_Server;
    FNs:Word;
    FCounter:Integer;
    FLastCount:QWord;
    FStartError:String;
    FStarted:Boolean;
    FOptions:TOPCUATestServerOptions;
    procedure AddVar(const Name:AnsiString; DataType:PUA_DataType; Value:Pointer; Count:Integer; Writable:Boolean);
    procedure Build;
  protected
    procedure Execute; override;
  public
    constructor Create(const AOptions:TOPCUATestServerOptions);
    destructor Destroy; override;
    function Url:AnsiString;
    function NodeId(const Name:AnsiString):AnsiString;
    function NodeIdByUri(const Name:AnsiString):AnsiString;
    //: reads a variable of the server directly (from the test thread: the server thread is paused meanwhile)
    property Namespace:Word read FNs;
    property StartError:String read FStartError;
    property Started:Boolean read FStarted;
  end;

const
  OPCUATestNamespaceUri = 'urn:pascalscada:test';

//: Loads libopen62541 once for the tests. False (and the reason) when it is not installed.
function OPCUATestLibraryAvailable(out Reason:String):Boolean;
{: Skips a test without the library: Ignore, or Fail when OPEN62541_REQUIRED is set (the CI
   jobs that download it: a broken download must not pass as a few ignored tests). Call it
   before allocating anything in SetUp (a failing SetUp skips TearDown). }
procedure OPCUATestNeedLibrary(Test:TTestCase);
function OPCUATestDefaultOptions:TOPCUATestServerOptions;
//: A free TCP port for a new server.
function OPCUATestNextPort:Word;

implementation

var
  GLibTried, GLibOk:Boolean;
  GLibReason:String;
  GNextPort:Word = 0;

function OPCUATestLibraryAvailable(out Reason:String):Boolean;
var
  lib:String;
begin
  if not GLibTried then begin
    GLibTried := true;
    lib := GetEnvironmentVariable('OPEN62541_LIB');
    try
      LoadOpen62541(lib);
      GLibOk := true;
    except
      on e:Exception do begin
        GLibOk := false;
        GLibReason := 'libopen62541 1.5 not available ('+e.Message+'); set OPEN62541_LIB to its path';
      end;
    end;
  end;
  Reason := GLibReason;
  Result := GLibOk;
end;

procedure OPCUATestNeedLibrary(Test:TTestCase);
var
  why:String;
begin
  if OPCUATestLibraryAvailable(why) then exit;
  if GetEnvironmentVariable('OPEN62541_REQUIRED')<>'' then
    Test.Fail(why+' (OPEN62541_REQUIRED is set)')
  else
    Test.Ignore(why);
end;

function OPCUATestDefaultOptions:TOPCUATestServerOptions;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Port := OPCUATestNextPort;
end;

function OPCUATestNextPort:Word;
begin
  if GNextPort=0 then begin
    Randomize;
    //below the ephemeral ports (Linux 32768-60999, Windows 49152-65535): a server on a
    //port some client socket is using locally may get the bind (open62541 sets
    //SO_REUSEPORT) and refuse the connections
    GNextPort := 20000+Random(10000);
  end;
  Inc(GNextPort);
  Result := GNextPort;
end;

{ TOPCUATestServer }

constructor TOPCUATestServer.Create(const AOptions:TOPCUATestServerOptions);
var
  t:QWord;
begin
  FOptions := AOptions;
  //created suspended and started here: a thread created running only starts after the
  //constructor (AfterConstruction), the wait below would never see it start
  inherited Create(true);
  Start;
  t := GetTickCount64;
  while (not FStarted) and (FStartError='') and (GetTickCount64-t<10000) do
    Sleep(10);
  if FStartError<>'' then
    raise Exception.Create('OPC UA test server: '+FStartError);
end;

destructor TOPCUATestServer.Destroy;
begin
  Terminate;
  WaitFor;
  inherited Destroy;
end;

function TOPCUATestServer.Url:AnsiString;
begin
  Result := 'opc.tcp://localhost:'+IntToStr(FOptions.Port);
end;

function TOPCUATestServer.NodeId(const Name:AnsiString):AnsiString;
begin
  Result := 'ns='+IntToStr(FNs)+';s=Test.'+Name;
end;

function TOPCUATestServer.NodeIdByUri(const Name:AnsiString):AnsiString;
begin
  Result := 'nsu='+OPCUATestNamespaceUri+';s=Test.'+Name;
end;

procedure TOPCUATestServer.AddVar(const Name:AnsiString; DataType:PUA_DataType; Value:Pointer; Count:Integer; Writable:Boolean);
var
  attr:UA_VariableAttributes;
  full:AnsiString;
  st:UA_StatusCode;
  dims:UA_UInt32;
begin
  attr := UA_VariableAttributes_default;
  full := 'Test.'+Name;
  if Count<0 then begin
    UA_Variant_setScalar(@attr.value, Value, DataType);
    attr.valueRank := UA_VALUERANK_SCALAR;
  end else begin
    UA_Variant_setArray(@attr.value, Value, Count, DataType);
    attr.valueRank := UA_VALUERANK_ONE_DIMENSION;
    dims := Count;
    attr.arrayDimensions := @dims;
    attr.arrayDimensionsSize := 1;
  end;
  attr.dataType := DataType^.typeId;
  attr.displayName := _UA_LOCALIZEDTEXT_ALLOC('en-US', Name);
  if Writable then
    attr.accessLevel := UA_ACCESSLEVELMASK_READ or UA_ACCESSLEVELMASK_WRITE
  else
    attr.accessLevel := UA_ACCESSLEVELMASK_READ;
  attr.userAccessLevel := attr.accessLevel;
  st := UA_Server_addVariableNode(FServer, UA_NODEID_STRING_ALLOC(FNs, full),
          UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER), UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
          _UA_QUALIFIEDNAME_ALLOC(FNs, Name), UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE),
          attr, nil, nil);
  UA_clear(@attr.displayName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  if st<>UA_STATUSCODE_GOOD then
    raise Exception.Create('addVariableNode '+Name+': '+UA_StatusCode_name(st));
end;

procedure TOPCUATestServer.Build;
var
  b:UA_Boolean;
  sb:UA_SByte;
  by:UA_Byte;
  i16:UA_Int16;
  u16:UA_UInt16;
  i32:UA_Int32;
  u32:UA_UInt32;
  i64:UA_Int64;
  u64:UA_UInt64;
  f:UA_Float;
  d:UA_Double;
  s:UA_String;
  dt:UA_DateTime;
  ai:array[0..9] of UA_Int32;
  ad:array[0..4] of UA_Double;
  sa:array[0..2] of UA_String;
  txt, t1, t2, t3, uri:AnsiString;
  k:Integer;
begin
  uri := OPCUATestNamespaceUri;
  FNs := UA_Server_addNamespace(FServer, PAnsiChar(uri));
  b := true;                   AddVar('Bool', @UA_TYPES[UA_TYPES_BOOLEAN], @b, -1, true);
  sb := -12;                   AddVar('SByte', @UA_TYPES[UA_TYPES_SBYTE], @sb, -1, true);
  by := 200;                   AddVar('Byte', @UA_TYPES[UA_TYPES_BYTE], @by, -1, true);
  i16 := -1234;                AddVar('Int16', @UA_TYPES[UA_TYPES_INT16], @i16, -1, true);
  u16 := 54321;                AddVar('UInt16', @UA_TYPES[UA_TYPES_UINT16], @u16, -1, true);
  i32 := -123456;              AddVar('Int32', @UA_TYPES[UA_TYPES_INT32], @i32, -1, true);
  u32 := 3000000000;           AddVar('UInt32', @UA_TYPES[UA_TYPES_UINT32], @u32, -1, true);
  i64 := -9000000000;          AddVar('Int64', @UA_TYPES[UA_TYPES_INT64], @i64, -1, true);
  u64 := 9000000000;           AddVar('UInt64', @UA_TYPES[UA_TYPES_UINT64], @u64, -1, true);
  f := 1.5;                    AddVar('Float', @UA_TYPES[UA_TYPES_FLOAT], @f, -1, true);
  d := 3.25;                   AddVar('Double', @UA_TYPES[UA_TYPES_DOUBLE], @d, -1, true);
  txt := 'hello';
  s := _UA_STRING(txt);        AddVar('String', @UA_TYPES[UA_TYPES_STRING], @s, -1, true);
  dt := UA_DateTimeFromDateTime(EncodeDate(2026,1,2)+EncodeTime(3,4,5,0)); //UTC
                               AddVar('DateTime', @UA_TYPES[UA_TYPES_DATETIME], @dt, -1, true);
  for k:=0 to 9 do ai[k] := k*10;
                               AddVar('Int32Array', @UA_TYPES[UA_TYPES_INT32], @ai[0], 10, true);
  for k:=0 to 4 do ad[k] := k+0.5;
                               AddVar('DoubleArray', @UA_TYPES[UA_TYPES_DOUBLE], @ad[0], 5, true);
  t1 := 'one'; t2 := 'two'; t3 := 'three';
  sa[0] := _UA_STRING(t1); sa[1] := _UA_STRING(t2); sa[2] := _UA_STRING(t3);
                               AddVar('StringArray', @UA_TYPES[UA_TYPES_STRING], @sa[0], 3, true);
  i32 := 777;                  AddVar('ReadOnlyInt32', @UA_TYPES[UA_TYPES_INT32], @i32, -1, false);
  i32 := 0;                    AddVar('Counter', @UA_TYPES[UA_TYPES_INT32], @i32, -1, false);
end;

procedure TOPCUATestServer.Execute;
var
  conf:PUA_ServerConfig;
  st:UA_StatusCode;
  v:UA_Variant;
  key, cert:UA_ByteString;
  subj:array[0..1] of UA_String;
  alt:array[0..1] of UA_String;
  s0, s1, a0, a1:AnsiString;
  login:UA_UsernamePasswordLogin;
  counterId:UA_NodeId;
  cname:AnsiString;
  lg:PUA_Logger;
begin
  FServer := UA_Server_new();
  try
    try
      conf := UA_Server_getConfig(FServer);
      if FOptions.Encrypted then begin
        s0 := 'C=BR'; s1 := 'CN=PascalSCADA test server';
        a0 := 'DNS:localhost'; a1 := 'URI:urn:open62541.server.application';
        subj[0] := _UA_STRING(s0); subj[1] := _UA_STRING(s1);
        alt[0] := _UA_STRING(a0); alt[1] := _UA_STRING(a1);
        FillChar(key, SizeOf(key), 0);
        FillChar(cert, SizeOf(cert), 0);
        lg := UA_Log_Stdout_new(UA_LOGLEVEL_WARNING);
        st := UA_CreateCertificate(lg, @subj[0], 2, @alt[0], 2, UA_CERTIFICATEFORMAT_DER, nil, @key, @cert);
        if Assigned(lg^.clear) then lg^.clear(lg);
        if st<>UA_STATUSCODE_GOOD then raise Exception.Create('server certificate: '+UA_StatusCode_name(st));
        st := UA_ServerConfig_setDefaultWithSecurityPolicies(conf, FOptions.Port, @cert, @key, nil, 0, nil, 0, nil, 0);
        UA_clear(@key, @UA_TYPES[UA_TYPES_BYTESTRING]);
        UA_clear(@cert, @UA_TYPES[UA_TYPES_BYTESTRING]);
      end else
        st := UA_ServerConfig_setMinimal(conf, FOptions.Port, nil);
      if st<>UA_STATUSCODE_GOOD then raise Exception.Create('server config: '+UA_StatusCode_name(st));
      if FOptions.Username<>'' then begin
        login.username := _UA_STRING(FOptions.Username);
        login.password := _UA_STRING(FOptions.Password);
        st := UA_AccessControl_default(conf, false, nil, 1, @login);
        if st<>UA_STATUSCODE_GOOD then raise Exception.Create('access control: '+UA_StatusCode_name(st));
      end;
      Build;
      st := UA_Server_run_startup(FServer);
      if st<>UA_STATUSCODE_GOOD then raise Exception.Create('startup: '+UA_StatusCode_name(st));
    except
      on e:Exception do begin
        FStartError := e.Message;
        exit;
      end;
    end;
    FStarted := true;
    cname := 'Test.Counter';
    counterId := UA_NODEID_STRING(FNs, cname);
    while not Terminated do begin
      UA_Server_run_iterate(FServer, false);
      if GetTickCount64-FLastCount>=50 then begin
        FLastCount := GetTickCount64;
        Inc(FCounter);
        UA_Variant_init(v);
        UA_Variant_setScalar(@v, @FCounter, @UA_TYPES[UA_TYPES_INT32]);
        UA_Server_writeValue(FServer, counterId, v);
      end;
      Sleep(2);
    end;
    UA_Server_run_shutdown(FServer);
  finally
    UA_Server_delete(FServer);
    FServer := nil;
  end;
end;

end.
