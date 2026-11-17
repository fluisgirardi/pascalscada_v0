unit TestOpcUa1;

{$IFDEF FPC}
  {$mode objfpc}
  {$WRITEABLECONST OFF}
{$ENDIF}
{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
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
    constructor create(owner:TTestOpcUaForm);
    destructor destroy;override;
    procedure execute;override;
  private
    procedure Log(const msg: string);
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
    procedure CheckConnection;
    procedure SubscriptionCallback(Data: PtrInt);
    procedure LogClient(const msg:string);
    procedure LogServer(data:ptrint);
  public

  end;

var
  TestOpcUaForm: TTestOpcUaForm;

implementation

{$R *.lfm}

type
  TLogFunction =
   procedure (logContext: Pointer; level: UA_LogLevel;
     category: UA_LogCategory; msg: PAnsiChar);cdecl;

const
{$IFDEF WINDOWS}
  libpascallog='ua_pascallog.dll';
{$ELSE}
  libpascallog='libua_pascallog.so';
{$ENDIF}


var UA_Pascal_logger:  function(context: pointer; func:TLogFunction):UA_logger;cdecl=nil;
    LoggerLibHandle: TLibHandle;

procedure LoadLogLibrary;
begin
  LoggerLibHandle := LoadLibrary(libpascallog);
  if LoggerLibHandle=NilHandle then
  begin
    exit;
  end;
  pointer(UA_Pascal_logger) := GetProcedureAddress(LoggerLibHandle,'UA_Pascal_logger');
end;

procedure UnloadLogLibrary;
begin
  if LoggerLibHandle<>NilHandle then
    UnloadLibrary(LoggerLibHandle);
end;

var
  client: PUA_Client;

{ TServerThread }

procedure client_log_cb(logContext: Pointer; level: UA_LogLevel;
  category: UA_LogCategory; msg: PAnsiChar); cdecl;
begin
  TTestOpcUaForm(logContext).LogClient(msg);
end;

procedure server_log_cb(logContext: Pointer; level: UA_LogLevel;
  category: UA_LogCategory; msg: PAnsiChar); cdecl;
begin
  TServerThread(logContext).Log(msg);
end;

constructor TServerThread.create(owner:TTestOpcUaForm);
begin
  LoadOpen62541();
  FOwner:=Owner;
  inherited create(false);
end;

destructor TServerThread.destroy;
begin
  running:=false;
  inherited destroy;
  UnloadOpen62541();
end;

procedure TServerThread.Log(const msg:string);
var
  s: PString;
begin
  new(s);
  s^:=msg;
  Application.QueueAsyncCall(@FOwner.LogServer,ptrint(s));
end;

procedure TServerThread.execute;
var
  res: UA_StatusCode;
  conf: PUA_ServerConfig;
begin
  server:=UA_Server_new();
  conf:=UA_Server_getConfig(server);
  if UA_Pascal_logger<>nil then
    conf^.logger:=UA_Pascal_logger(self, @server_log_cb);
  res:=UA_ServerConfig_setDefault(conf);
  running:=true;
  res:=UA_Server_run(server, @running);
end;

procedure TTestOpcUaForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if client <> nil then
    UA_Client_delete(client); // Disconnects the client internally
                              //  UA_Client_delete() -> UA_ClientConfig_clear() -> UA_ApplicationDescription_clear() -> UA_clear() ... -> UA_Array_delete() -> UA_free
  FreeAndNil(FServer);
  UnloadOpen62541();
end;

procedure TTestOpcUaForm.FormCreate(Sender: TObject);
begin
  LoadLogLibrary;
end;

procedure TTestOpcUaForm.FormDestroy(Sender: TObject);
begin
  UnloadLogLibrary;
end;

procedure TTestOpcUaForm.CheckConnection;
begin
  if client = nil then raise Exception.Create('Application is not connected to OPC UA Server!');
end;

procedure TTestOpcUaForm.SubscriptionCallback(Data: PtrInt);
begin
  Memo1.Lines.Append(Format('Main thread Subscription Callback: %d',[Data]));
end;

procedure TTestOpcUaForm.LogClient(const msg: string);
begin
  Memo1.Lines.Add('(client) '+msg);
end;

procedure TTestOpcUaForm.LogServer(data:ptrint);
begin
  Memo1.Lines.Add('(server) '+PString(data)^);
  Dispose(PString(data));
end;

// callback
procedure handler_TheAnswerChanged(client:PUA_Client; subId: UA_UInt32; subContext: Pointer; monId: UA_UInt32; monContext: Pointer; value: PUA_DataValue); cdecl;
var i: PtrInt;
begin
  if UA_Variant_hasScalarType(@(value^.value), @UA_TYPES[UA_TYPES_INT32]) then
    i := UA_Variant_getInteger(value^.value)
  else
    i := 0;
  TestOpcUaForm.Memo2.Lines.Append(Format('Subscription Callback: %d',[i]));
{$IFDEF FPC}
  Application.QueueAsyncCall(@TestOpcUaForm.SubscriptionCallback, i);
{$ENDIF}
end;


(*** Custom data type definition ***)
type
  MyStruct = packed record
    x: UA_UINT32;
    y: UA_UINT32;
    z: UA_String;
  end;

const
  MyStructMembers: array[0..2] of UA_DataTypeMember = (
    (memberTypeIndex: UA_TYPES_UINT32;       padding:0; flag:1{namespaceZero=1}; memberName: 'x'),
    (memberTypeIndex: UA_TYPES_UINT32;       padding:4; flag:1{namespaceZero=1}; memberName: 'y'),
    (memberTypeIndex: {ns=3;i=}3014{STRING}; padding:8; flag:0{namespaceZero=0}; memberName: 'z')
  );

  MyStructTypeName: array[0..15] of AnsiChar = 'MyStructTypeName';
  {$IF UA_VER = 1.2}
  MyStructType: UA_DataType = (
    typeId: (namespaceIndex:3; identifierType:UA_NODEIDTYPE_STRING; identifier:(_string:(length:Length(MyStructTypeName);data:{$IFDEF FPC}@MyStructTypeName{$ELSE}nil{$ENDIF}))); (* .typeId *) // MUST BE ONLY "UA_NODEIDTYPE_NUMERIC"
    binaryEncodingId: (namespaceIndex:3; identifierType:UA_NODEIDTYPE_NUMERIC; identifier:(numeric:0));
    memSize: SizeOf(MyStruct);
    typeIndex: 0;                            (* .typeIndex, in the array of custom types *)
    typeKind: Ord(UA_DATATYPEKIND_STRUCTURE);
    pointerFree: 0;
    overlayable: 0;
    membersSize: 3;
    {!!! works only if binaryEncodingId==identifier.numeric !!!}
    members: @MyStructMembers;
    typeName: 'MyStructType';
  );
  {$ELSE}
  MyStructType: UA_DataType = (
    typeName: 'MyStructType';
    typeId: (namespaceIndex:3; identifierType:UA_NODEIDTYPE_STRING; identifier:(_string:(length:Length(MyStructTypeName);data:{$IFDEF FPC}@MyStructTypeName{$ELSE}nil{$ENDIF}))); (* .typeId *) // MUST BE ONLY "UA_NODEIDTYPE_NUMERIC"
    binaryEncodingId: (namespaceIndex:3; identifierType:UA_NODEIDTYPE_NUMERIC; identifier:(numeric:0));
    memSize: SizeOf(MyStruct);
    typeKind: Ord(UA_DATATYPEKIND_STRUCTURE);
    pointerFree: 0;
    overlayable: 0;
    membersSize: 3;
    {!!! works only if binaryEncodingId==identifier.numeric !!!}
    members: @MyStructMembers;
  );
  {$IFEND}

  MyCustomDataTypes: UA_DataTypeArray = (
    next: nil;
    typesSize: 1;
    types: @MyStructType;
  );


procedure TTestOpcUaForm.btnConnectClick(Sender: TObject);
var
  res: UA_StatusCode;
  channelState: UA_SecureChannelState;
  sessionState: UA_SessionState;
  conf: PUA_ClientConfig;
  s: AnsiString;
  i32: longint;
  value: UA_Variant; (* Variants can hold scalar values and arrays of any type *)
  nodeId: UA_NodeId;
  dts: UA_DateTimeStruct;
  lt: UA_LocalizedText;
  qn: UA_QualifiedName;
  size: size_t;
  pdimension: PUA_UInt32;
  // Subscriptions:
  request: UA_CreateSubscriptionRequest;
  response: UA_CreateSubscriptionResponse;
  monRequest: UA_MonitoredItemCreateRequest;
  monResponse: UA_MonitoredItemCreateResult;
begin
  (*** Load dynamic library ***)
  LoadOpen62541();

  client := nil;
  client := UA_Client_new();
  conf := UA_Client_getConfig(client);

  conf^.clientDescription.applicationName := _UA_LOCALIZEDTEXT_ALLOC('en-US','My Test Application');
  UA_ClientConfig_setDefault(conf);

  if UA_Pascal_logger<>nil then
    conf^.logger:=UA_Pascal_logger(self, @client_log_cb);

  conf^.customDataTypes := @MyCustomDataTypes;

  (*** Get the client connection status ***)
  UA_Client_getState(client,@channelState,@sessionState,nil);
  {$IFDEF FPC}WriteStr(s, sessionState, '/', channelState);{$ELSE}s:='';{$ENDIF}
  Memo1.Lines.Append(Format('Before connect state: %d/%d %s', [ord(sessionState), ord(channelState), s]));


  (*** Connection - Anonymous user, Security None ***)
  res := UA_Client_connect(client, cbServer.Text);
  //res := UA_Client_connectUsername(client, PAnsiChar(cbServer.Text),'test','test');
  if res <> UA_STATUSCODE_GOOD then begin
    Memo1.Lines.Append(Format('Fail status code: %x %s', [res, AnsiString(UA_StatusCode_name(res))]));
    UA_Client_delete(client);
    client := nil;
    Exit;
  end;

  UA_Client_getState(client,@channelState,@sessionState,nil);
  {$IFDEF FPC}WriteStr(s, sessionState, '/', channelState);{$ELSE}s:='';{$ENDIF}
  Memo1.Lines.Append(Format('After connect state: %d/%d %s', [ord(sessionState), ord(channelState), s]));

 (* Read the value attribute of the node. UA_Client_readValueAttribute is a
  * wrapper for the raw read service available as UA_Client_Service_read. *)
  nodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERARRAY);
  UA_Variant_init(value);
  res := UA_Client_readValueAttribute(client, nodeId, value);
  if (res = UA_STATUSCODE_GOOD) and (UA_Variant_hasArrayType(@value, @UA_TYPES[UA_TYPES_STRING])) then begin
    UA_Client_readValueRankAttribute(client, nodeId, i32);
    UA_Client_readArrayDimensionsAttribute(client, nodeId, size, pdimension);
    Memo1.Lines.Append(Format('ServerArray variable (ValueRank=%d, ArrayDimensions=size:%d,dimensions[0]:%d):', [i32, size, pdimension^]));
    Memo1.Lines.Append('  '+UA_Variant_getString(value));
  end;
  UA_Variant_clear(value);

 (* NodeId of the variable holding the current time *)
  nodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_CURRENTTIME);
  res := UA_Client_readValueAttribute(client, nodeId, value);
  if res <> UA_STATUSCODE_GOOD then
    Memo1.Lines.Append(Format('Fail status code: %x %s', [res, AnsiString(UA_StatusCode_name(res))]));
  if (res = UA_STATUSCODE_GOOD) and (UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_DATETIME])) then begin
    UA_Client_readValueRankAttribute(client, nodeId, i32);
    dts := UA_DateTime_toStruct(PUA_DateTime(value.data)^);
    Memo1.Lines.Append(Format('Date on server is: %d.%d.%d %d:%d:%d (ValueRank=%d)', [dts.day, dts.month, dts.year, dts.hour, dts.min, dts.sec, i32]));
  end;
  UA_Variant_clear(value);

  nodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_BUILDINFO_PRODUCTNAME);
  if UA_Client_readValueAttribute(client, nodeId, s) = UA_STATUSCODE_GOOD then begin
    UA_Client_readValueRankAttribute(client, nodeId, i32);
    Memo1.Lines.Append(Format('Product Name is: %s (ValueRank=%d)', [s,i32]));
  end;

{ $DEFINE HAVE_PROSYSSIMULATIONSERVER}
{$IFDEF HAVE_PROSYSSIMULATIONSERVER}
  // read variable "Counter"
  nodeId := UA_NODEID_STRING(3, 'Counter');
  res := UA_Client_readBrowseNameAttribute(client, nodeId, qn);
  Memo1.Lines.Append(Format('Counter qname (%x %s): %s', [res, AnsiString(UA_StatusCode_name(res)), UA_StringToStr(qn.name)]));
  UA_clear(@qn, @UA_TYPES[UA_TYPES_QUALIFIEDNAME]);

  res := UA_Client_readDisplayNameAttribute(client, nodeId, lt);
  Memo1.Lines.Append(Format('Counter name (%x %s): %s', [res, AnsiString(UA_StatusCode_name(res)), UA_LocalizedTextToStr(lt)]));
  UA_clear(@lt, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);

  res := UA_Client_readValueAttribute(client, nodeId, i32);
  Memo1.Lines.Append(Format('Counter value (%x %s): %d', [res, AnsiString(UA_StatusCode_name(res)), i32]));

  // read variable "Sinusoid"
  nodeId := UA_NODEID_STRING(3, 'Sinusoid');
  res := UA_Client_readValueAttribute(client, nodeId, value);
  if UA_Variant_isScalar(@value) then
    Memo1.Lines.Append(Format('Sinusoid value (%x): %.8f', [res, PUA_Double(value.data)^]));
  UA_Variant_clear(value);

  // read and write variable "TestNode1"
  nodeId := UA_NODEID_STRING(3, 'TestNode1');
  res := UA_Client_readValueAttribute(client, nodeId, value);
  if UA_Variant_isScalar(@value) then begin
    Memo1.Lines.Append(Format('TestNode1 read value (%x): %d', [res, PUA_Int64(value.data)^]));
    UA_Variant_setInt64(value, UA_Variant_getInt64(value)+1);
    res := UA_Client_writeValueAttribute(client, nodeId, value);
    Memo1.Lines.Append(Format('TestNode1 write value (%x): %d', [res, PUA_Int64(value.data)^]));
  end;
  UA_Variant_clear(value);

  // read and write variable "TestNode2"
  nodeId := UA_NODEID_STRING(3, 'TestNode2');
  res := UA_Client_readValueAttribute(client, nodeId, value);
  if (UA_Variant_hasArrayType(@value, @UA_TYPES[UA_TYPES_STRING])) then begin
    for i32:=0 to value.arrayLength-1 do
      Memo1.Lines.Append(Format('TestNode2 read [%d]: %s', [i32, UA_Variant_getString(value, i32)]));
  end;
  //UA_Variant_setInt64(value, 33);
  //res := UA_Client_writeValueAttribute(client, nodeId, value);
  //Memo1.Lines.Append(Format('TestNode2 write value (%x): %d', [res, PUA_Int64(value.data)^]));

  (*************************)
  (* Create a subscription *)
  (*************************)
  request := UA_CreateSubscriptionRequest_default();
  request.requestedPublishingInterval := 1000; // 1000 ms
  response := UA_Client_Subscriptions_create(client, request, nil, nil, nil);
  if response.responseHeader.serviceResult = UA_STATUSCODE_GOOD then
    Memo1.Lines.Append(Format('Create subscription succeeded (PublishInterval=%.0f)',[response.revisedPublishingInterval]));

  (* Add a MonitoredItem *)
  //monRequest := UA_MonitoredItemCreateRequest_default(UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_CURRENTTIME));
  monRequest := UA_MonitoredItemCreateRequest_default(UA_NODEID_STRING(3, 'Counter'));
  monRequest.requestedParameters.samplingInterval := 500;

  // pripoj MonitoredItem ku Subscription
  monResponse := UA_Client_MonitoredItems_createDataChange(client, response.subscriptionId, UA_TIMESTAMPSTORETURN_BOTH, monRequest, nil, @handler_TheAnswerChanged, nil);
  if monResponse.statusCode = UA_STATUSCODE_GOOD then
    Memo1.Lines.Append('Monitoring ...');

  (* The first publish request should return the initial value of the variable *)
  UA_Client_run_iterate(client, 5000);
  Memo1.Lines.Append('Iterate ...');
  Application.ProcessMessages;

  (* Take another look at the.answer *)
  UA_Client_run_iterate(client, 1000);
  Memo1.Lines.Append('Iterate ...');
  Application.ProcessMessages;

  UA_Client_MonitoredItems_deleteSingle(client, response.subscriptionId, monResponse.monitoredItemId);
  (* Delete the subscription *)
  if UA_Client_Subscriptions_deleteSingle(client, response.subscriptionId) = UA_STATUSCODE_GOOD then
    Memo1.Lines.Append('Subscription removed');

  UA_Client_run_iterate(client, 500);
  Memo1.Lines.Append('Iterate ...');
  UA_Client_run_iterate(client, 500);
  Memo1.Lines.Append('Iterate ...');
{$ENDIF}
end;

procedure TTestOpcUaForm.btnReadVariableClick(Sender: TObject);
type
  TMyCustomType=packed record
    x: UA_Int32;
    y: UA_Int32;
    z: AnsiString;
  end;
var
  nodeId, dataType: UA_NodeId;
  res: UA_StatusCode;
  value: UA_Variant;
  valueRank: UA_Int32;
  arrayDimensionsSize: size_t;
  arrayDimensions: PUA_UInt32;
  i: integer;
  s: AnsiString;
  data: PUA_Byte;
  myCustomType: TMyCustomType;
begin
  CheckConnection;

  case cbNodeType.ItemIndex of
    0:   NodeId := UA_NODEID_NUMERIC(StrToInt(cbNS.Text), StrToInt(eNodeId.Text));
    else NodeId := UA_NODEID_STRING_ALLOC(StrToInt(cbNS.Text), eNodeId.Text);
  end;

  // get data type of requested variable
  if UA_Client_readDataTypeAttribute(client, nodeId, dataType) = UA_STATUSCODE_GOOD then
    Memo1.Lines.Append(Format('Node "%s" data type: %s (%s)', [eNodeId.Text, UA_NodeIdToStr(dataType), UA_DataTypeToStr(dataType)]));

  // get value of requested variable
  res := UA_Client_readValueAttribute(client, nodeId, value);
  case res of
    UA_STATUSCODE_GOOD:
      if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_BYTE]) then
        eNodeValue.Text := IntToStr(UA_Variant_getByte(value))
      else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_INT16]) then
        eNodeValue.Text := IntToStr(UA_Variant_getSmallint(value))
      else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_INT32]) then
        eNodeValue.Text := IntToStr(UA_Variant_getInteger(value))
      else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_STRING]) then
        eNodeValue.Text := UA_Variant_getString(value) // value._type^.memSize == SizeOf(UA_STRING)
      else if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_EXTENSIONOBJECT]) then begin
        // The standard mandates that variants contain built-in data types only. If the value is not of a builtin type, it is wrapped into an ExtensionObject.
        //  UA_EXTENSIONOBJECT_ENCODED_BYTESTRING = 1
        //  (ua_types_encoding_binary.c: The binary encoding has a different nodeid from the data type.
        //    UA_findDataTypeByBinaryInternal(): We only store a ***numeric identifier*** for the encoding nodeid of data types)
        //    If the ExtensionObject is decoded then .encoding = UA_EXTENSIONOBJECT_DECODED
        if PUA_ExtensionObject(value.data)^.encoding = UA_EXTENSIONOBJECT_ENCODED_BYTESTRING then begin
          data := PUA_ExtensionObject(value.data)^.content.encoded.body.data;
          // test for one specific custom data type
          if (dataType.namespaceIndex=3) and (dataType.identifierType=UA_NODEIDTYPE_STRING) and
             (_UA_String_equal(dataType.identifier._string, 'DT_"GRZ"."POZ"."KNT"')) then begin
            // +0
            myCustomType.x := PUA_Int32(data)^;
            // +4
            myCustomType.y := PUA_Int32(data+4)^;
            // +8 (4 bytes length followed by chars); on 64-bit also 4 or 8 bytes?
            SetString(myCustomType.z, PAnsiChar(data+8+SizeOf(NativeInt)), PNativeInt(data+8)^);
            Memo1.Lines.Append(Format('Node "%s" MyCustomType: %d,%d,%s', [eNodeId.Text, myCustomType.x, myCustomType.y, myCustomType.z]));
          end;
          SetString(s, PAnsiChar(data), PUA_ExtensionObject(value.data)^.content.encoded.body.length);
          for i:=1 to Length(s) do if (s[i]<#32) then s[i]:=' ';
        end
        else
          s:='';
        eNodeValue.Text := Format('<Extension Object; Encoding=%d (encoded.TypeId="%s"), Body=(%d):%s>',[ord(PUA_ExtensionObject(value.data)^.encoding), UA_NodeIdToStr(PUA_ExtensionObject(value.data)^.content.encoded.typeId), PUA_ExtensionObject(value.data)^.content.encoded.body.length, s]);
        // value._type^.memSize == SizeOf(UA_EXTENSIONOBJECT)
      end
      else if not UA_Variant_isScalar(@value) then begin // Is array ?
        //UA_Client_readValueRankAttribute(client, nodeId, valueRank);
        // Rank = 0-One or more dimensions; 1-One dimension; -2-Any (The value can be a scalar or an array with any number of dimensions)
        //UA_Client_readArrayDimensionsAttribute(client, nodeId, arrayDimensionsSize, arrayDimensions);

        // check UA_Variant for array properties
        arrayDimensionsSize := value.arrayDimensionsSize;
        s:='';
        for i:=0 to arrayDimensionsSize-1 do s:=Format('%s[%d]',[s, value.arrayDimensions[i]]);
        eNodeValue.Text := Format('<Array Length=%d, Dimensions=%d:%s>', [value.arrayLength, arrayDimensionsSize, s]);

        if UA_Variant_hasArrayType(@value, @UA_TYPES[UA_TYPES_EXTENSIONOBJECT]) then begin
          for i:=0 to value.arrayLength-1 do
            Memo1.Lines.Append(Format('Node "%s"[%d] <Extension Object; Encoding=%d (encoded.TypeId="%s"), Body length=%d>',[eNodeId.Text, i, ord(PUA_ExtensionObject(value.data)[i].encoding), UA_NodeIdToStr(PUA_ExtensionObject(value.data)[i].content.encoded.typeId), PUA_ExtensionObject(value.data)[i].content.encoded.body.length]));
        end;
      end
      else
        eNodeValue.Text := '';
    UA_STATUSCODE_BADTYPEMISMATCH:
      begin
      res := UA_Client_readDataTypeAttribute(client, nodeId, dataType);
      raise Exception.CreateFmt('Error reading value of variable "%s"! (expected "String" type, but variable is "%s" type)', [eNodeId.Text, UA_DataTypeToStr(dataType)]);
      end
    else
      raise Exception.CreateFmt('Error reading value of variable "%s"! (%s)', [eNodeId.Text, AnsiString(UA_StatusCode_name(res))]);
  end;

  {$IF UA_VER = 1.2}
  Memo1.Lines.Append(Format('Node "%s" read value: %s (Size=%d, Type=%s (typeId=%s,typeIndex=%d); Result=%x)', [eNodeId.Text, eNodeValue.Text, value._type^.memSize, value._type^.typeName, UA_NodeIdToStr(value._type^.typeId), value._type^.typeIndex, res]));
  {$ELSE}
  Memo1.Lines.Append(Format('Node "%s" read value: %s (Size=%d, Type=%s (typeId=%s,typeKind=%d); Result=%x)', [eNodeId.Text, eNodeValue.Text, value._type^.memSize, value._type^.typeName, UA_NodeIdToStr(value._type^.typeId), value._type^.typeKind, res]));
  {$IFEND}
  //pDataType := value._type;
  //Memo1.Lines.Append(Format('Data Type: typeName=%s, typeId=%s, memSize=%d, typeIndex=%d, flags=%d, binaryEncodingId=%d, members=%p', [pDataType^.typeName, UA_NodeIdToStr(pDataType^.typeId), pDataType^.memSize, pDataType^.typeIndex, pDataType^.flags, pDataType^.binaryEncodingId, pDataType^.members]));

  UA_NodeId_clear(nodeId);
end;

procedure TTestOpcUaForm.btnWriteVariableClick(Sender: TObject);
var
  nodeId, dataType: UA_NodeId;
  res: UA_StatusCode;
  pDataType: PUA_DataType;
  value: UA_Variant;
begin
  CheckConnection;

  case cbNodeType.ItemIndex of
    0:   NodeId := UA_NODEID_NUMERIC(StrToInt(cbNS.Text), StrToInt(eNodeId.Text));
    else NodeId := UA_NODEID_STRING_ALLOC(StrToInt(cbNS.Text), eNodeId.Text);
  end;

  // check varible Data Type on server
  if UA_Client_readDataTypeAttribute(client, nodeId, dataType) = UA_STATUSCODE_GOOD then begin
    pDataType := UA_findDataType(@dataType);
    if pDataType <> nil then begin
      case {$IF UA_VER = 1.2}pDataType^.typeIndex{$ELSE}pDataType^.typeKind{$IFEND} of
        UA_TYPES_BYTE:
          UA_Variant_setByte(value, Byte(StrToInt(eNodeValue.Text)));
        UA_TYPES_INT16:
          UA_Variant_setSmallint(value, Smallint(StrToInt(eNodeValue.Text)));
        UA_TYPES_INT32:
          UA_Variant_setInteger(value, StrToInt(eNodeValue.Text));
        UA_TYPES_DOUBLE:
          UA_Variant_setDouble(value, StrToFloat(eNodeValue.Text));
        UA_TYPES_STRING:
          UA_Variant_setString(value, eNodeValue.Text);
        //else
        //  raise Exception.CreateFmt('Unimplemented variable data type "%s"!',[pDataType^.typeName]);
      end;
      // write value into server variable
      res := UA_Client_writeValueAttribute(client, nodeId, value);
      Memo1.Lines.Append(Format('Node "%s" write value: %s (Result=%x)', [eNodeId.Text, eNodeValue.Text, res]));
      UA_Variant_clear(value);
      // check result of write
      case res of
        UA_STATUSCODE_BADNOTWRITABLE:
          raise Exception.Create('The access level does not allow writing to the Node!');
        UA_STATUSCODE_BADTYPEMISMATCH:
          raise Exception.Create('The value supplied for the attribute is not of the same type as the attribute''s value!');
      end;
    end;
  end;

  UA_NodeId_clear(nodeId);
end;

procedure TTestOpcUaForm.btnServerStartClick(Sender: TObject);
var
  res: UA_StatusCode;
begin
  if Assigned(FServer) then begin
    FreeAndNil(FServer);
    btnServerStart.Checked := False;
    Memo1.Lines.Add('stopped');
  end else
  begin
    FServer:=TServerThread.create(self);
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
      Memo1.Lines.Append('server not connected');
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
  attr.accessLevel := UA_ACCESSLEVELMASK_READ or UA_ACCESSLEVELMASK_WRITE;

  (* Add the variable node to the information model *)
  intVariableNodeId := UA_NODEID_STRING(StrToInt(cbServerNS.Text){name space index}, varname);
  intVariableName := _UA_QUALIFIEDNAME(StrToInt(cbServerNS.Text), varname);
  parentNodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
  parentReferenceNodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES);
  res := UA_Server_addVariableNode(FServer.server, intVariableNodeId, parentNodeId,
                             parentReferenceNodeId, intVariableName,
                             UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), attr, nil, nil);
  Memo1.Lines.Append(Format('Server add variable "%s" (Result=%x)', [eServerVariableName.Text, res]));
end;

procedure TTestOpcUaForm.btnServerWriteVariableClick(Sender: TObject);
var
  intVariableNodeId: UA_NodeId;
  value: UA_Variant;
  res: UA_StatusCode;
begin
  if not assigned(FServer) then
  begin
      Memo1.Lines.Append('server not connected');
     exit;
  end;
  intVariableNodeId := UA_NODEID_STRING_ALLOC(StrToInt(cbServerNS.Text){name space index}, eServerVariableName.Text);
  UA_Variant_init(value);
  UA_Variant_setInteger(value, StrToInt(eServerVariableValue.Text));
  res := UA_Server_writeValue(Fserver.server, intVariableNodeId, value);
  Memo1.Lines.Append(Format('Server write to variable "%s" (Result=%x)', [eServerVariableName.Text, res]));
  UA_NodeId_clear(intVariableNodeId);
end;

end.

