{ Tests the client subscriptions with a server and a client in the same
  process, the way an application usually does it (issue: exception on
  UA_Client_Subscriptions_create):

  - UA_CreateSubscriptionRequest_default + UA_Client_Subscriptions_create;
  - UA_MonitoredItemCreateRequest_default + UA_Client_MonitoredItems_createDataChange;
  - UA_Client_run_iterate delivers the initial value and every change written
    by the server to the data change callback;
  - UA_Client_MonitoredItems_deleteSingle / UA_Client_Subscriptions_deleteSingle.

    fpc -Fu.. -Fi.. subscription_test.pas && ./subscription_test [/path/to/libopen62541.so]

  Exit code 0 when every check passes. }
program subscription_test;
{$MODE OBJFPC}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Math, SyncObjs, open62541;

const
  Port = 48492;
  VarId = 3;

type

  { TServerThread }

  TServerThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Started: TEvent;
    StartError: String;
    NewValue: Double;          // written by the main thread...
    WriteRequested: Boolean;   // ...and applied by the server thread
    constructor Create;
    destructor Destroy; override;
  end;

var
  Server: PUA_Server = nil;
  Ns: UA_UInt16 = 1;
  Failures: Integer = 0;
  // filled by the data change callback (client thread = main thread)
  Notifications: Integer = 0;
  LastValue: Double = NaN;
  LastSubId, LastMonId: UA_UInt32;
  LastMonContext: Pointer = nil;
  DeletedMonId: UA_UInt32 = 0;

procedure Check(Cond: Boolean; const What: String);
begin
  if Cond then
    WriteLn('ok   ', What)
  else begin
    WriteLn('FAIL ', What);
    Inc(Failures);
  end;
end;

function Name(st: UA_StatusCode): String;
begin
  Result := UA_StatusCode_name(st);
end;

procedure DataChanged(client: PUA_Client; subId: UA_UInt32; subContext: Pointer; monId: UA_UInt32;
  monContext: Pointer; value: PUA_DataValue); cdecl;
begin
  Inc(Notifications);
  LastSubId := subId;
  LastMonId := monId;
  LastMonContext := monContext;
  LastValue := UA_Variant_getDouble(value^.value);
end;

procedure MonitoredItemDeleted(client: PUA_Client; subId: UA_UInt32; subContext: Pointer;
  monId: UA_UInt32; monContext: Pointer); cdecl;
begin
  DeletedMonId := monId;
end;

{ TServerThread }

constructor TServerThread.Create;
begin
  Started := TEvent.Create(nil, true, false, '');
  inherited Create(false);
end;

destructor TServerThread.Destroy;
begin
  inherited Destroy;
  Started.Free;
end;

procedure TServerThread.Execute;
var
  st: UA_StatusCode;
  attr: UA_VariableAttributes;
  d: Double;
  v: UA_Variant;
  BrowseName: AnsiString;
begin
  BrowseName := 'Value';
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  Server := UA_Server_new();
  try
    try
      st := UA_ServerConfig_setMinimal(UA_Server_getConfig(Server), Port, nil);
      if st <> UA_STATUSCODE_GOOD then raise Exception.Create('config: ' + Name(st));
      // a Double variable at ns=1;i=3, like the PLC variable of the issue
      attr := UA_VariableAttributes_default;
      d := 1.5;
      UA_Variant_setScalar(@attr.value, @d, @UA_TYPES[UA_TYPES_DOUBLE]);
      attr.dataType := UA_TYPES[UA_TYPES_DOUBLE].typeId;
      attr.valueRank := UA_VALUERANK_SCALAR;
      attr.accessLevel := UA_ACCESSLEVELMASK_READ;
      attr.userAccessLevel := attr.accessLevel;
      st := UA_Server_addVariableNode(Server, UA_NODEID_NUMERIC(Ns, VarId),
              UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER), UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
              _UA_QUALIFIEDNAME(Ns, BrowseName), UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE),
              attr, nil, nil);
      if st <> UA_STATUSCODE_GOOD then raise Exception.Create('add variable: ' + Name(st));
      st := UA_Server_run_startup(Server);
      if st <> UA_STATUSCODE_GOOD then raise Exception.Create('startup: ' + Name(st));
    except
      on E: Exception do begin
        StartError := E.Message;
        Started.SetEvent;
        Exit;
      end;
    end;
    Started.SetEvent;
    while not Terminated do begin
      if WriteRequested then begin
        d := NewValue;
        UA_Variant_init(v);
        UA_Variant_setScalar(@v, @d, @UA_TYPES[UA_TYPES_DOUBLE]);
        UA_Server_writeValue(Server, UA_NODEID_NUMERIC(Ns, VarId), v);
        WriteRequested := false;
      end;
      UA_Server_run_iterate(Server, false);
      Sleep(1);
    end;
    UA_Server_run_shutdown(Server);
  finally
    UA_Server_delete(Server);
    Server := nil;
  end;
end;

{ runs the client until MinNotifications arrived or the timeout }
function IterateUntil(Client: PUA_Client; MinNotifications: Integer; TimeoutMs: QWord): Boolean;
var
  t0: QWord;
begin
  t0 := GetTickCount64;
  while (Notifications < MinNotifications) and (GetTickCount64 - t0 < TimeoutMs) do
    UA_Client_run_iterate(Client, 50);
  Result := Notifications >= MinNotifications;
end;

var
  srv: TServerThread;
  client: PUA_Client;
  st: UA_StatusCode;
  request: UA_CreateSubscriptionRequest;
  response: UA_CreateSubscriptionResponse;
  monRequest: UA_MonitoredItemCreateRequest;
  monResult: UA_MonitoredItemCreateResult;
  value: UA_Variant;
  ctx: Integer;
  got: Boolean;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  LoadOpen62541(ParamStr(1));
  srv := TServerThread.Create;
  try
    srv.Started.WaitFor(5000);
    if srv.StartError <> '' then begin
      WriteLn('server did not start: ', srv.StartError);
      Halt(2);
    end;
    client := UA_Client_new();
    UA_ClientConfig_setDefault(UA_Client_getConfig(client));
    st := UA_Client_connect(client, PAnsiChar('opc.tcp://localhost:' + IntToStr(Port)));
    if st <> UA_STATUSCODE_GOOD then begin
      WriteLn('connect: ', Name(st));
      Halt(2);
    end;
    try
      // plain read first, as in the issue
      UA_Variant_init(value);
      st := UA_Client_readValueAttribute(client, UA_NODEID_NUMERIC(Ns, VarId), @value);
      Check((st = UA_STATUSCODE_GOOD) and (UA_Variant_getDouble(value) = 1.5),
            'readValueAttribute (' + Name(st) + ', ' + FloatToStr(UA_Variant_getDouble(value)) + ')');
      UA_Variant_clear(value);

      // subscription
      request := UA_CreateSubscriptionRequest_default();
      request.requestedPublishingInterval := 100;
      response := UA_Client_Subscriptions_create(client, request, nil, nil, nil);
      st := response.responseHeader.serviceResult;
      Check(st = UA_STATUSCODE_GOOD, 'Subscriptions_create (' + Name(st) + ', id ' +
            IntToStr(response.subscriptionId) + ')');
      Check((response.revisedPublishingInterval = 100), 'revised publishing interval (' +
            FloatToStr(response.revisedPublishingInterval) + ')');

      // monitored item
      monRequest := UA_MonitoredItemCreateRequest_default(UA_NODEID_NUMERIC(Ns, VarId));
      monRequest.requestedParameters.samplingInterval := 50;
      monResult := UA_Client_MonitoredItems_createDataChange(client, response.subscriptionId,
                     UA_TIMESTAMPSTORETURN_BOTH, monRequest, @ctx, @DataChanged, @MonitoredItemDeleted);
      Check(monResult.statusCode = UA_STATUSCODE_GOOD, 'MonitoredItems_createDataChange (' +
            Name(monResult.statusCode) + ', id ' + IntToStr(monResult.monitoredItemId) + ')');

      // the initial value arrives
      got := IterateUntil(client, 1, 3000);
      Check(got and (LastValue = 1.5),
            'initial value notified (' + FloatToStr(LastValue) + ')');
      Check((LastSubId = response.subscriptionId) and (LastMonId = monResult.monitoredItemId) and
            (LastMonContext = @ctx), 'callback gets the subscription, item and context');

      // changes written by the server arrive
      srv.NewValue := 2.25;
      srv.WriteRequested := true;
      got := IterateUntil(client, 2, 3000);
      Check(got and (LastValue = 2.25),
            'changed value notified (' + FloatToStr(LastValue) + ')');
      srv.NewValue := -7.125;
      srv.WriteRequested := true;
      got := IterateUntil(client, 3, 3000);
      Check(got and (LastValue = -7.125),
            'second change notified (' + FloatToStr(LastValue) + ')');

      // delete
      st := UA_Client_MonitoredItems_deleteSingle(client, response.subscriptionId, monResult.monitoredItemId);
      Check((st = UA_STATUSCODE_GOOD) and (DeletedMonId = monResult.monitoredItemId),
            'MonitoredItems_deleteSingle (' + Name(st) + ')');
      st := UA_Client_Subscriptions_deleteSingle(client, response.subscriptionId);
      Check(st = UA_STATUSCODE_GOOD, 'Subscriptions_deleteSingle (' + Name(st) + ')');
      UA_clear(@response, @UA_TYPES[UA_TYPES_CREATESUBSCRIPTIONRESPONSE]);
      UA_clear(@monResult, @UA_TYPES[UA_TYPES_MONITOREDITEMCREATERESULT]);
    finally
      UA_Client_disconnect(client);
      UA_Client_delete(client);
    end;
  finally
    srv.Terminate;
    srv.WaitFor;
    srv.Free;
  end;
  if Failures = 0 then
    WriteLn('all checks passed')
  else
    WriteLn(Failures, ' check(s) failed');
  Halt(Ord(Failures > 0));
end.
