unit TestOpcUa1;

{ Demo of the binding (open62541 1.5): a client (connect, read/write a variable) and a server
  running in a thread (add/write an Int32 variable). The log of both goes to the memo.

  The library is loaded once, when the form is created: libopen62541 must be installed,
  next to the executable or given in the OPEN62541_LIB environment variable.

  TestOpcUa --selftest does what the buttons do (start the server, add and write a variable,
  connect, read and write it), prints the log and closes. }

{$IFDEF FPC}
  {$mode objfpc}
  {$WRITEABLECONST OFF}
{$ENDIF}
{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, open62541;

type
  {$IF NOT DECLARED(PtrInt)}
  PtrInt = NativeInt;
  {$IFEND}

  TTestOpcUaForm = class;

  { TServerThread }

  TServerThread = class(TThread)
    running:UA_Boolean;
    server:PUA_Server;
    FOwner:TTestOpcUaForm;
    StartResult:UA_StatusCode;
    constructor create(owner:TTestOpcUaForm);
    destructor destroy;override;
    procedure execute;override;
  end;

  { TTestOpcUaForm }

  TTestOpcUaForm = class(TForm)
    btnConnect: TButton;
    btnReadVariable: TButton;
    btnWriteVariable: TButton;
    btnServerStart: TToggleBox;
    btnServerAddVariable: TButton;
    btnServerWriteVariable: TButton;
    cbNodeType: TComboBox;
    cbNS: TComboBox;
    cbServerNS: TComboBox;
    cbServer: TComboBox;
    eNodeId: TEdit;
    eServerVariableName: TEdit;
    eNodeValue: TEdit;
    eServerVariableValue: TEdit;
    gbVariables: TGroupBox;
    gbServerVariable: TGroupBox;
    Label1: TLabel;
    lNamespace: TLabel;
    lNamespace1: TLabel;
    lNodeId: TLabel;
    lVariableName: TLabel;
    lNodeType: TLabel;
    lNodeValue: TLabel;
    lNodeValue1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    tabClient: TTabSheet;
    tabServer: TTabSheet;
    procedure btnConnectClick(Sender: TObject);
    procedure btnReadVariableClick(Sender: TObject);
    procedure btnServerAddVariableClick(Sender: TObject);
    procedure btnServerStartClick(Sender: TObject);
    procedure btnServerWriteVariableClick(Sender: TObject);
    procedure btnWriteVariableClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServer: TServerThread;
    FLibLoaded: Boolean;
    procedure CheckConnection;
    procedure CheckLibrary;
    procedure SubscriptionCallback(Data: PtrInt);
    procedure LogQueued(data:PtrInt);
    procedure SelfTest(data:PtrInt);
  public
    procedure QueueLog(const msg:string);
  end;

var
  TestOpcUaForm: TTestOpcUaForm;

implementation

{$R *.lfm}

const
  FPUExceptions = [exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision];

var
  client: PUA_Client = nil;

{ --- log --- }

(* Called by the library, from the thread that runs the client or the server: the
 * message goes to the memo through the main thread. The C va_list is formatted
 * with UA_FormatLogMessage. An exception must never leave a callback. *)
procedure log_cb(logContext: Pointer; level: UA_LogLevel; category: UA_LogCategory;
  msg: PAnsiChar; args: Pointer); cdecl;
begin
  try
    if (level >= UA_LOGLEVEL_INFO) and Assigned(TestOpcUaForm) then
      TestOpcUaForm.QueueLog(PAnsiChar(logContext) + UA_FormatLogMessage(msg, args));
  except
  end;
end;

(* The default config already has a logger (UA_Log_Stdout_new), shared with the
 * EventLoop and the certificate groups: it is redirected in place, its "clear"
 * still frees it with the config. *)
procedure RedirectLogger(logger: PUA_Logger; const prefix: PAnsiChar);
begin
  if logger = nil then exit;
  logger^.context := prefix;
  logger^.log := @log_cb;
end;

{ TServerThread }

constructor TServerThread.create(owner:TTestOpcUaForm);
begin
  FOwner:=owner;
  running:=true;
  server:=UA_Server_new();
  if server<>nil then begin
    RedirectLogger(UA_Server_getConfig(server)^.logging, '(server) ');
    //port 4840, no security, anonymous access
    StartResult:=UA_ServerConfig_setDefault(UA_Server_getConfig(server));
  end else
    StartResult:=UA_STATUSCODE_BADOUTOFMEMORY;
  inherited create(StartResult<>UA_STATUSCODE_GOOD);
end;

destructor TServerThread.destroy;
begin
  running:=false; //UA_Server_run returns
  inherited destroy; //waits for execute
  if server<>nil then
    UA_Server_delete(server);
end;

procedure TServerThread.execute;
begin
  SetExceptionMask(FPUExceptions);
  UA_Server_run(server, @running);
end;

{ TTestOpcUaForm }

procedure TTestOpcUaForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if client <> nil then begin
    UA_Client_delete(client); // disconnects the client
    client := nil;
  end;
  FreeAndNil(FServer);
end;

procedure TTestOpcUaForm.FormCreate(Sender: TObject);
begin
  SetExceptionMask(FPUExceptions);
  try
    LoadOpen62541(GetEnvironmentVariable('OPEN62541_LIB'));
    FLibLoaded := true;
    Memo1.Lines.Add('Loaded ' + Open62541LibraryName);
  except
    on e: Exception do
      Memo1.Lines.Add(e.Message);
  end;
  if Application.HasOption('selftest') then
    Application.QueueAsyncCall(@SelfTest, 0);
end;

procedure TTestOpcUaForm.SelfTest(data: PtrInt);

  procedure Step(const what: string; btn: TNotifyEvent);
  begin
    Memo1.Lines.Add('--- ' + what);
    try
      btn(nil);
    except
      on e: Exception do
        Memo1.Lines.Add('EXCEPTION: ' + e.Message);
    end;
    // lets the queued log arrive
    Sleep(200);
    Application.ProcessMessages;
  end;

begin
  Step('server start', @btnServerStartClick);
  // issue #12: the server must keep running until it is stopped
  Sleep(3000);
  Application.ProcessMessages;
  if Assigned(FServer) and not FServer.Finished then
    Memo1.Lines.Add('server still running after 3 s')
  else
    Memo1.Lines.Add('FAIL: the server stopped by itself');
  cbServerNS.Text := '1';
  eServerVariableName.Text := 'the.answer';
  eServerVariableValue.Text := '42';
  Step('server add variable', @btnServerAddVariableClick);
  eServerVariableValue.Text := '43';
  Step('server write variable', @btnServerWriteVariableClick);
  cbServer.Text := 'opc.tcp://localhost:4840/';
  Step('connect', @btnConnectClick);
  cbNS.Text := '1';
  cbNodeType.ItemIndex := 1;
  eNodeId.Text := 'the.answer';
  Step('read', @btnReadVariableClick);
  Memo1.Lines.Add('read value: ' + eNodeValue.Text);
  eNodeValue.Text := '-7';
  Step('write', @btnWriteVariableClick);
  eNodeValue.Text := '';
  Step('read back', @btnReadVariableClick);
  Memo1.Lines.Add('read value: ' + eNodeValue.Text);
  eNodeValue.Text := 'abc';
  Step('write a text to an Int32 (must fail)', @btnWriteVariableClick);
  cbNodeType.ItemIndex := 0;
  cbNS.Text := '0';
  eNodeId.Text := IntToStr(UA_NS0ID_SERVER_SERVERSTATUS_STATE);
  Step('read ns=0;i=2259 (ServerState)', @btnReadVariableClick);
  if Assigned(FServer) and not FServer.Finished then
    Memo1.Lines.Add('server still running before stop')
  else
    Memo1.Lines.Add('FAIL: the server stopped by itself');
  Step('server stop', @btnServerStartClick);
  WriteLn(Memo1.Lines.Text);
  WriteLn('--- memo2');
  WriteLn(Memo2.Lines.Text);
  Close;
end;

procedure TTestOpcUaForm.FormDestroy(Sender: TObject);
begin
  Application.RemoveAsyncCalls(Self);
  if FLibLoaded then
    UnloadOpen62541();
end;

procedure TTestOpcUaForm.CheckConnection;
begin
  if client = nil then raise Exception.Create('Application is not connected to OPC UA Server!');
end;

procedure TTestOpcUaForm.CheckLibrary;
begin
  if not FLibLoaded then raise Exception.Create('The open62541 library is not loaded (see the log).');
end;

procedure TTestOpcUaForm.SubscriptionCallback(Data: PtrInt);
begin
  Memo1.Lines.Append(Format('Main thread Subscription Callback: %d',[Data]));
end;

procedure TTestOpcUaForm.QueueLog(const msg: string);
var
  s: PString;
begin
  new(s);
  s^:=msg;
  Application.QueueAsyncCall(@LogQueued, PtrInt(s));
end;

procedure TTestOpcUaForm.LogQueued(data:PtrInt);
begin
  Memo1.Lines.Add(PString(data)^);
  Dispose(PString(data));
end;

// data change callback (runs inside UA_Client_run_iterate, in the main thread)
procedure handler_TheAnswerChanged(client:PUA_Client; subId: UA_UInt32; subContext: Pointer; monId: UA_UInt32; monContext: Pointer; value: PUA_DataValue); cdecl;
var i: PtrInt;
begin
  try
    if UA_Variant_hasScalarType(@(value^.value), @UA_TYPES[UA_TYPES_INT32]) then
      i := UA_Variant_getInteger(value^.value)
    else
      i := 0;
    TestOpcUaForm.Memo2.Lines.Append(Format('Subscription Callback: %d',[i]));
    Application.QueueAsyncCall(@TestOpcUaForm.SubscriptionCallback, i);
  except
  end;
end;

function StatusText(res: UA_StatusCode): string;
begin
  Result := Format('%x %s', [res, AnsiString(UA_StatusCode_name(res))]);
end;

procedure TTestOpcUaForm.btnConnectClick(Sender: TObject);
var
  res: UA_StatusCode;
  channelState: UA_SecureChannelState;
  sessionState: UA_SessionState;
  conf: PUA_ClientConfig;
  s: AnsiString;
  i32: UA_Int32;
  value: UA_Variant; (* Variants can hold scalar values and arrays of any type *)
  nodeId: UA_NodeId;
  dts: UA_DateTimeStruct;
  size: size_t;
  pdimension: PUA_UInt32;
  {$IFDEF HAVE_PROSYSSIMULATIONSERVER}
  lt: UA_LocalizedText;
  qn: UA_QualifiedName;
  nodeName: AnsiString;
  // Subscriptions:
  request: UA_CreateSubscriptionRequest;
  response: UA_CreateSubscriptionResponse;
  monRequest: UA_MonitoredItemCreateRequest;
  monResponse: UA_MonitoredItemCreateResult;
  {$ENDIF}
begin
  CheckLibrary;
  if client <> nil then begin
    UA_Client_delete(client);
    client := nil;
  end;

  client := UA_Client_new(); // already with the default config
  conf := UA_Client_getConfig(client);
  RedirectLogger(conf^.logging, '(client) ');
  UA_clear(@conf^.clientDescription.applicationName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  conf^.clientDescription.applicationName := _UA_LOCALIZEDTEXT_ALLOC('en-US','My Test Application');

  (*** Get the client connection status ***)
  UA_Client_getState(client,@channelState,@sessionState,nil);
  WriteStr(s, sessionState, '/', channelState);
  Memo1.Lines.Append(Format('Before connect state: %d/%d %s', [ord(sessionState), ord(channelState), s]));

  (*** Connection - Anonymous user, Security None ***)
  res := UA_Client_connect(client, PAnsiChar(AnsiString(cbServer.Text)));
  //res := UA_Client_connectUsername(client, cbServer.Text, 'user', 'password');
  //(a password needs encryption: see tools/login_test.pas)
  if res <> UA_STATUSCODE_GOOD then begin
    Memo1.Lines.Append('Fail status code: ' + StatusText(res));
    UA_Client_delete(client);
    client := nil;
    Exit;
  end;

  UA_Client_getState(client,@channelState,@sessionState,nil);
  WriteStr(s, sessionState, '/', channelState);
  Memo1.Lines.Append(Format('After connect state: %d/%d %s', [ord(sessionState), ord(channelState), s]));

 (* Read the value attribute of the node. UA_Client_readValueAttribute is a
  * wrapper for the raw read service available as UA_Client_Service_read. *)
  nodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERARRAY);
  UA_Variant_init(value);
  res := UA_Client_readValueAttribute(client, nodeId, @value);
  if (res = UA_STATUSCODE_GOOD) and (UA_Variant_hasArrayType(@value, @UA_TYPES[UA_TYPES_STRING])) then begin
    i32 := 0;
    UA_Client_readValueRankAttribute(client, nodeId, @i32);
    size := 0;
    pdimension := nil;
    UA_Client_readArrayDimensionsAttribute(client, nodeId, @size, @pdimension);
    if size > 0 then
      Memo1.Lines.Append(Format('ServerArray variable (ValueRank=%d, ArrayDimensions=size:%d,dimensions[0]:%d):', [i32, size, pdimension^]))
    else
      Memo1.Lines.Append(Format('ServerArray variable (ValueRank=%d, no ArrayDimensions):', [i32]));
    UA_Array_delete(pdimension, size, @UA_TYPES[UA_TYPES_UINT32]);
    Memo1.Lines.Append('  '+UA_Variant_getString(value, 0));
  end;
  UA_Variant_clear(value);

 (* NodeId of the variable holding the current time *)
  nodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_CURRENTTIME);
  res := UA_Client_readValueAttribute(client, nodeId, @value);
  if res <> UA_STATUSCODE_GOOD then
    Memo1.Lines.Append('Fail status code: ' + StatusText(res));
  if (res = UA_STATUSCODE_GOOD) and (UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_DATETIME])) then begin
    UA_Client_readValueRankAttribute(client, nodeId, @i32);
    dts := UA_DateTime_toStruct(PUA_DateTime(value.data)^);
    Memo1.Lines.Append(Format('Date on server is: %d.%d.%d %d:%d:%d UTC (ValueRank=%d)', [dts.day, dts.month, dts.year, dts.hour, dts.min, dts.sec, i32]));
  end;
  UA_Variant_clear(value);

  nodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_BUILDINFO_PRODUCTNAME);
  if (UA_Client_readValueAttribute(client, nodeId, @value) = UA_STATUSCODE_GOOD) and
     UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_STRING]) then begin
    UA_Client_readValueRankAttribute(client, nodeId, @i32);
    Memo1.Lines.Append(Format('Product Name is: %s (ValueRank=%d)', [UA_Variant_getString(value), i32]));
  end;
  UA_Variant_clear(value);

{ $DEFINE HAVE_PROSYSSIMULATIONSERVER}
{$IFDEF HAVE_PROSYSSIMULATIONSERVER}
  // read variable "Counter"
  nodeName := 'Counter';
  nodeId := UA_NODEID_STRING(3, nodeName);
  res := UA_Client_readBrowseNameAttribute(client, nodeId, @qn);
  Memo1.Lines.Append(Format('Counter qname (%s): %s', [StatusText(res), UA_StringToStr(qn.name)]));
  UA_clear(@qn, @UA_TYPES[UA_TYPES_QUALIFIEDNAME]);

  res := UA_Client_readDisplayNameAttribute(client, nodeId, @lt);
  Memo1.Lines.Append(Format('Counter name (%s): %s', [StatusText(res), UA_LocalizedTextToStr(lt)]));
  UA_clear(@lt, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);

  res := UA_Client_readValueAttribute(client, nodeId, @value);
  if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_INT32]) then
    Memo1.Lines.Append(Format('Counter value (%s): %d', [StatusText(res), UA_Variant_getInteger(value)]));
  UA_Variant_clear(value);

  // read variable "Sinusoid"
  nodeName := 'Sinusoid';
  nodeId := UA_NODEID_STRING(3, nodeName);
  res := UA_Client_readValueAttribute(client, nodeId, @value);
  if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_DOUBLE]) then
    Memo1.Lines.Append(Format('Sinusoid value (%s): %.8f', [StatusText(res), UA_Variant_getDouble(value)]));
  UA_Variant_clear(value);

  // read and write variable "TestNode1"
  nodeName := 'TestNode1';
  nodeId := UA_NODEID_STRING(3, nodeName);
  res := UA_Client_readValueAttribute(client, nodeId, @value);
  if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_INT64]) then begin
    Memo1.Lines.Append(Format('TestNode1 read value (%s): %d', [StatusText(res), UA_Variant_getInt64(value)]));
    i32 := UA_Variant_getInt64(value);
    UA_Variant_clear(value);
    UA_Variant_setInt64(value, i32+1);
    res := UA_Client_writeValueAttribute(client, nodeId, @value);
    Memo1.Lines.Append(Format('TestNode1 write value (%s): %d', [StatusText(res), i32+1]));
  end;
  UA_Variant_clear(value);

  // read variable "TestNode2"
  nodeName := 'TestNode2';
  nodeId := UA_NODEID_STRING(3, nodeName);
  res := UA_Client_readValueAttribute(client, nodeId, @value);
  if (UA_Variant_hasArrayType(@value, @UA_TYPES[UA_TYPES_STRING])) then begin
    for i32:=0 to value.arrayLength-1 do
      Memo1.Lines.Append(Format('TestNode2 read [%d]: %s', [i32, UA_Variant_getString(value, i32)]));
  end;
  UA_Variant_clear(value);

  (*************************)
  (* Create a subscription *)
  (*************************)
  request := UA_CreateSubscriptionRequest_default();
  request.requestedPublishingInterval := 1000; // 1000 ms
  response := UA_Client_Subscriptions_create(client, request, nil, nil, nil);
  if response.responseHeader.serviceResult = UA_STATUSCODE_GOOD then
    Memo1.Lines.Append(Format('Create subscription succeeded (PublishInterval=%.0f)',[response.revisedPublishingInterval]));

  (* Add a MonitoredItem *)
  nodeName := 'Counter';
  monRequest := UA_MonitoredItemCreateRequest_default(UA_NODEID_STRING(3, nodeName));
  monRequest.requestedParameters.samplingInterval := 500;

  monResponse := UA_Client_MonitoredItems_createDataChange(client, response.subscriptionId, UA_TIMESTAMPSTORETURN_BOTH, monRequest, nil, @handler_TheAnswerChanged, nil);
  if monResponse.statusCode = UA_STATUSCODE_GOOD then
    Memo1.Lines.Append('Monitoring ...');

  (* The first publish request should return the initial value of the variable.
   * The callbacks only run inside UA_Client_run_iterate: an application calls it
   * all the time (timer or thread). *)
  UA_Client_run_iterate(client, 5000);
  Memo1.Lines.Append('Iterate ...');
  Application.ProcessMessages;

  UA_Client_run_iterate(client, 1000);
  Memo1.Lines.Append('Iterate ...');
  Application.ProcessMessages;

  UA_Client_MonitoredItems_deleteSingle(client, response.subscriptionId, monResponse.monitoredItemId);
  (* Delete the subscription *)
  if UA_Client_Subscriptions_deleteSingle(client, response.subscriptionId) = UA_STATUSCODE_GOOD then
    Memo1.Lines.Append('Subscription removed');
  UA_clear(@response, @UA_TYPES[UA_TYPES_CREATESUBSCRIPTIONRESPONSE]);
  UA_clear(@monResponse, @UA_TYPES[UA_TYPES_MONITOREDITEMCREATERESULT]);

  UA_Client_run_iterate(client, 500);
  Memo1.Lines.Append('Iterate ...');
{$ENDIF}
end;

procedure TTestOpcUaForm.btnReadVariableClick(Sender: TObject);
var
  nodeId, dataType: UA_NodeId;
  res: UA_StatusCode;
  value: UA_Variant;
  arrayDimensionsSize: size_t;
  i: integer;
  s: AnsiString;
  data: PUA_Byte;
  ext: PUA_ExtensionObject;
begin
  CheckConnection;

  case cbNodeType.ItemIndex of
    0:   NodeId := UA_NODEID_NUMERIC(StrToInt(cbNS.Text), StrToInt(eNodeId.Text));
    else NodeId := UA_NODEID_STRING_ALLOC(StrToInt(cbNS.Text), eNodeId.Text);
  end;
  FillChar(dataType, SizeOf(dataType), 0);
  UA_Variant_init(value);
  try
    // get data type of requested variable
    if UA_Client_readDataTypeAttribute(client, nodeId, @dataType) = UA_STATUSCODE_GOOD then
      Memo1.Lines.Append(Format('Node "%s" data type: %s (%s)', [eNodeId.Text, UA_NodeIdToStr(dataType), UA_DataTypeToStr(dataType)]));

    // get value of requested variable
    res := UA_Client_readValueAttribute(client, nodeId, @value);
    if res <> UA_STATUSCODE_GOOD then
      raise Exception.CreateFmt('Error reading value of variable "%s"! (%s)', [eNodeId.Text, AnsiString(UA_StatusCode_name(res))]);

    if UA_Variant_isEmpty(@value) then
      eNodeValue.Text := ''
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_BOOLEAN]) then
      eNodeValue.Text := BoolToStr(PUA_Boolean(value.data)^, true)
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_BYTE]) then
      eNodeValue.Text := IntToStr(UA_Variant_getByte(value))
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_INT16]) then
      eNodeValue.Text := IntToStr(UA_Variant_getSmallint(value))
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_UINT16]) then
      eNodeValue.Text := IntToStr(PUA_UInt16(value.data)^)
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_INT32]) then
      eNodeValue.Text := IntToStr(UA_Variant_getInteger(value))
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_UINT32]) then
      eNodeValue.Text := IntToStr(PUA_UInt32(value.data)^)
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_INT64]) then
      eNodeValue.Text := IntToStr(UA_Variant_getInt64(value))
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_FLOAT]) then
      eNodeValue.Text := FloatToStr(UA_Variant_getFloat(value))
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_DOUBLE]) then
      eNodeValue.Text := FloatToStr(UA_Variant_getDouble(value))
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_STRING]) then
      eNodeValue.Text := UA_Variant_getString(value)
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_DATETIME]) then
      eNodeValue.Text := DateTimeToStr(UA_DateTimeToDateTime(PUA_DateTime(value.data)^))
    else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_EXTENSIONOBJECT]) then begin
      // A value that is not of a built-in type (a structure the library doesn't know)
      // comes wrapped into an ExtensionObject, still binary encoded.
      ext := PUA_ExtensionObject(value.data);
      s := '';
      if ext^.encoding = UA_EXTENSIONOBJECT_ENCODED_BYTESTRING then begin
        data := ext^.content.encoded.body.data;
        // test for one specific custom data type: Int32 x, Int32 y, String z
        // (binary encoding: an Int32 length, then the chars)
        if (dataType.namespaceIndex=3) and (dataType.identifierType=UA_NODEIDTYPE_STRING) and
           (_UA_String_equal(dataType.identifier._string, 'DT_"GRZ"."POZ"."KNT"')) and
           (ext^.content.encoded.body.length >= 12) then begin
          SetString(s, PAnsiChar(data+12), Max(0, Min(PUA_Int32(data+8)^, ext^.content.encoded.body.length-12)));
          Memo1.Lines.Append(Format('Node "%s" MyCustomType: %d,%d,%s', [eNodeId.Text, PUA_Int32(data)^, PUA_Int32(data+4)^, s]));
        end;
        SetString(s, PAnsiChar(data), ext^.content.encoded.body.length);
        for i:=1 to Length(s) do if (s[i]<#32) then s[i]:=' ';
      end;
      eNodeValue.Text := Format('<Extension Object; Encoding=%d (encoded.TypeId="%s"), Body=(%d):%s>',
                                [ord(ext^.encoding), UA_NodeIdToStr(ext^.content.encoded.typeId), ext^.content.encoded.body.length, s]);
    end
    else if not UA_Variant_isScalar(@value) then begin // array
      arrayDimensionsSize := value.arrayDimensionsSize;
      s:='';
      for i:=0 to Integer(arrayDimensionsSize)-1 do s:=Format('%s[%d]',[s, value.arrayDimensions[i]]);
      eNodeValue.Text := Format('<Array Length=%d, Dimensions=%d:%s>', [value.arrayLength, arrayDimensionsSize, s]);
      if UA_Variant_hasArrayType(@value, @UA_TYPES[UA_TYPES_STRING]) then
        for i:=0 to Integer(value.arrayLength)-1 do
          Memo1.Lines.Append(Format('Node "%s"[%d] %s', [eNodeId.Text, i, UA_Variant_getString(value, i)]));
    end
    else
      eNodeValue.Text := '<'+value._type^.typeName+'>';

    Memo1.Lines.Append(Format('Node "%s" read value: %s (Size=%d, Type=%s (typeId=%s,typeKind=%d))',
                              [eNodeId.Text, eNodeValue.Text, UA_DataType_memSize(value._type), value._type^.typeName,
                               UA_NodeIdToStr(value._type^.typeId), Ord(UA_DataType_typeKind(value._type))]));
  finally
    UA_Variant_clear(value);
    UA_NodeId_clear(dataType);
    UA_NodeId_clear(nodeId);
  end;
end;

procedure TTestOpcUaForm.btnWriteVariableClick(Sender: TObject);
var
  nodeId, dataType: UA_NodeId;
  res: UA_StatusCode;
  t: PUA_DataType;
  value: UA_Variant;
  txt: string;
begin
  CheckConnection;

  case cbNodeType.ItemIndex of
    0:   NodeId := UA_NODEID_NUMERIC(StrToInt(cbNS.Text), StrToInt(eNodeId.Text));
    else NodeId := UA_NODEID_STRING_ALLOC(StrToInt(cbNS.Text), eNodeId.Text);
  end;
  FillChar(dataType, SizeOf(dataType), 0);
  UA_Variant_init(value);
  try
    // the value is sent with the data type of the variable on the server
    res := UA_Client_readDataTypeAttribute(client, nodeId, @dataType);
    if res <> UA_STATUSCODE_GOOD then
      raise Exception.CreateFmt('Can not read the data type of "%s" (%s)', [eNodeId.Text, AnsiString(UA_StatusCode_name(res))]);
    t := UA_findDataType(@dataType);
    txt := eNodeValue.Text;
    if t = @UA_TYPES[UA_TYPES_BOOLEAN] then UA_Variant_setBoolean(value, StrToBool(txt))
    else if t = @UA_TYPES[UA_TYPES_BYTE] then UA_Variant_setByte(value, Byte(StrToInt(txt)))
    else if t = @UA_TYPES[UA_TYPES_INT16] then UA_Variant_setSmallint(value, Smallint(StrToInt(txt)))
    else if t = @UA_TYPES[UA_TYPES_UINT16] then UA_Variant_setUInt16(value, UInt16(StrToInt(txt)))
    else if t = @UA_TYPES[UA_TYPES_INT32] then UA_Variant_setInteger(value, StrToInt(txt))
    else if t = @UA_TYPES[UA_TYPES_UINT32] then UA_Variant_setUInt32(value, UInt32(StrToInt64(txt)))
    else if t = @UA_TYPES[UA_TYPES_INT64] then UA_Variant_setInt64(value, StrToInt64(txt))
    else if t = @UA_TYPES[UA_TYPES_UINT64] then UA_Variant_setUInt64(value, StrToQWord(txt))
    else if t = @UA_TYPES[UA_TYPES_FLOAT] then UA_Variant_setFloat(value, StrToFloat(txt))
    else if t = @UA_TYPES[UA_TYPES_DOUBLE] then UA_Variant_setDouble(value, StrToFloat(txt))
    else if t = @UA_TYPES[UA_TYPES_STRING] then UA_Variant_setString(value, txt)
    else
      raise Exception.CreateFmt('Unimplemented variable data type "%s"!', [UA_DataTypeToStr(dataType)]);

    // write value into server variable
    res := UA_Client_writeValueAttribute(client, nodeId, @value);
    Memo1.Lines.Append(Format('Node "%s" write value: %s (Result=%s)', [eNodeId.Text, txt, StatusText(res)]));
    case res of
      UA_STATUSCODE_BADNOTWRITABLE, UA_STATUSCODE_BADUSERACCESSDENIED:
        raise Exception.Create('The access level does not allow writing to the Node!');
      UA_STATUSCODE_BADTYPEMISMATCH:
        raise Exception.Create('The value supplied for the attribute is not of the same type as the attribute''s value!');
    end;
  finally
    UA_Variant_clear(value);
    UA_NodeId_clear(dataType);
    UA_NodeId_clear(nodeId);
  end;
end;

procedure TTestOpcUaForm.btnServerStartClick(Sender: TObject);
begin
  if Assigned(FServer) then begin
    FreeAndNil(FServer);
    btnServerStart.Checked := False;
    Memo1.Lines.Add('stopped');
  end else
  begin
    CheckLibrary;
    FServer:=TServerThread.create(self);
    if FServer.StartResult <> UA_STATUSCODE_GOOD then begin
      Memo1.Lines.Add('server config failed: ' + StatusText(FServer.StartResult));
      FreeAndNil(FServer);
      btnServerStart.Checked := False;
      exit;
    end;
    Memo1.Lines.Add('started');
  end;
end;

procedure TTestOpcUaForm.btnServerAddVariableClick(Sender: TObject);
var
  attr: UA_VariableAttributes;
  intVariable: UA_Int32;
  intVariableNodeId, parentNodeId, parentReferenceNodeId: UA_NodeId;
  intVariableName: UA_QualifiedName;
  res: UA_StatusCode;
  varlang:AnsiString;
  varname:AnsiString;
begin
  if not assigned(FServer) then
  begin
    Memo1.Lines.Append('server not started');
    exit;
  end;
  varlang:='en-US';
  varname:=eServerVariableName.Text;
  (* Define the attribute of the myInteger variable node *)
  attr := UA_VariableAttributes_default;
  intVariable := StrToInt(eServerVariableValue.Text);
  UA_Variant_setScalar(@attr.value, @intVariable, @UA_TYPES[UA_TYPES_INT32]);
  attr.description := _UA_LOCALIZEDTEXT(varlang, varname);
  attr.displayName := _UA_LOCALIZEDTEXT(varlang, varname);
  attr.dataType := UA_TYPES[UA_TYPES_INT32].typeId;
  attr.valueRank := UA_VALUERANK_SCALAR;
  attr.accessLevel := UA_ACCESSLEVELMASK_READ or UA_ACCESSLEVELMASK_WRITE;
  attr.userAccessLevel := attr.accessLevel;

  (* Add the variable node to the information model (copied by the server) *)
  intVariableNodeId := UA_NODEID_STRING(StrToInt(cbServerNS.Text){name space index}, varname);
  intVariableName := _UA_QUALIFIEDNAME(StrToInt(cbServerNS.Text), varname);
  parentNodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
  parentReferenceNodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES);
  res := UA_Server_addVariableNode(FServer.server, intVariableNodeId, parentNodeId,
                             parentReferenceNodeId, intVariableName,
                             UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), attr, nil, nil);
  Memo1.Lines.Append(Format('Server add variable "%s" (Result=%s)', [eServerVariableName.Text, StatusText(res)]));
end;

procedure TTestOpcUaForm.btnServerWriteVariableClick(Sender: TObject);
var
  intVariableNodeId: UA_NodeId;
  value: UA_Variant;
  res: UA_StatusCode;
begin
  if not assigned(FServer) then
  begin
    Memo1.Lines.Append('server not started');
    exit;
  end;
  intVariableNodeId := UA_NODEID_STRING_ALLOC(StrToInt(cbServerNS.Text){name space index}, eServerVariableName.Text);
  UA_Variant_init(value);
  UA_Variant_setInteger(value, StrToInt(eServerVariableValue.Text));
  res := UA_Server_writeValue(Fserver.server, intVariableNodeId, value);
  Memo1.Lines.Append(Format('Server write to variable "%s" (Result=%s)', [eServerVariableName.Text, StatusText(res)]));
  UA_Variant_clear(value);
  UA_NodeId_clear(intVariableNodeId);
end;

end.
