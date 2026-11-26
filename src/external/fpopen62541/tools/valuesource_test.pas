{ Tests the callback value source (DataSource) of the server and its async
  operations, with a server and a client in the same process:

  - read: the value comes from the callback, with its status;
  - sync write: the callback refuses a value with a status the client gets;
  - async write: the callback returns GOODCOMPLETESASYNCHRONOUSLY and the result
    (good or bad) is given later with UA_Server_setAsyncWriteResult, while the
    server keeps answering other requests;
  - a result for an operation no longer waiting is BADNOTFOUND;
  - UA_Server_deleteNode.

    fpc -Fu.. -Fi.. valuesource_test.pas && ./valuesource_test [/path/to/libopen62541.so]

  Exit code 0 when every check passes. }
program valuesource_test;
{$MODE OBJFPC}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Math, SyncObjs, open62541;

const
  Port = 48491;
  NsUri = 'urn:fpopen62541:valuesource_test';

type

  { TServerThread }

  TServerThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Started: TEvent;
    StartError: String;
    constructor Create;
    destructor Destroy; override;
  end;

  { TSlowWriter: a second client that writes to the async variable }

  TSlowWriter = class(TThread)
  protected
    procedure Execute; override;
  public
    Value: Integer;
    Result: UA_StatusCode;
    constructor Create(AValue: Integer);
  end;

var
  Server: PUA_Server = nil;
  Ns: UA_UInt16 = 0;
  // the "application" side of the variables
  SyncValue: UA_Int32 = 42;
  SyncStatus: UA_StatusCode = UA_STATUSCODE_GOOD;
  AsyncValue: UA_Int32 = 0;
  // async write waiting for its result (touched by the server thread only)
  PendingValue: PUA_DataValue = nil;
  PendingDue: QWord = 0;
  PendingWritten: UA_Int32 = 0;
  LastPendingPtr: PUA_DataValue = nil;
  StaleResult: UA_StatusCode = 0;
  Failures: Integer = 0;

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

{ --- value source callbacks (run in the server thread) --- }

function ReadSync(server: PUA_Server; const sessionId: PUA_NodeId; sessionContext: Pointer;
  const nodeId: PUA_NodeId; nodeContext: Pointer; includeSourceTimeStamp: UA_Boolean;
  const range: PUA_NumericRange; value: PUA_DataValue): UA_StatusCode; cdecl;
begin
  UA_Variant_setScalarCopy(@value^.value, @SyncValue, @UA_TYPES[UA_TYPES_INT32]);
  value^.status := SyncStatus;
  value^.flags := value^.flags or UA_DATAVALUE_HASVALUE or UA_DATAVALUE_HASSTATUS;
  Result := UA_STATUSCODE_GOOD;
end;

function WriteSync(server: PUA_Server; const sessionId: PUA_NodeId; sessionContext: Pointer;
  const nodeId: PUA_NodeId; nodeContext: Pointer; const range: PUA_NumericRange;
  const value: PUA_DataValue): UA_StatusCode; cdecl;
var
  v: UA_Int32;
begin
  if (value^.value._type <> @UA_TYPES[UA_TYPES_INT32]) or (value^.value.data = nil) then
    Exit(UA_STATUSCODE_BADTYPEMISMATCH);
  v := PUA_Int32(value^.value.data)^;
  if (v < 0) or (v > 100) then
    Exit(UA_STATUSCODE_BADOUTOFRANGE);
  SyncValue := v;
  Result := UA_STATUSCODE_GOOD;
end;

function ReadAsyncVar(server: PUA_Server; const sessionId: PUA_NodeId; sessionContext: Pointer;
  const nodeId: PUA_NodeId; nodeContext: Pointer; includeSourceTimeStamp: UA_Boolean;
  const range: PUA_NumericRange; value: PUA_DataValue): UA_StatusCode; cdecl;
begin
  UA_Variant_setScalarCopy(@value^.value, @AsyncValue, @UA_TYPES[UA_TYPES_INT32]);
  value^.flags := value^.flags or UA_DATAVALUE_HASVALUE;
  Result := UA_STATUSCODE_GOOD;
end;

function WriteAsyncVar(server: PUA_Server; const sessionId: PUA_NodeId; sessionContext: Pointer;
  const nodeId: PUA_NodeId; nodeContext: Pointer; const range: PUA_NumericRange;
  const value: PUA_DataValue): UA_StatusCode; cdecl;
begin
  if (value^.value._type <> @UA_TYPES[UA_TYPES_INT32]) or (value^.value.data = nil) then
    Exit(UA_STATUSCODE_BADTYPEMISMATCH);
  // the "device" answers 300 ms later (see TServerThread.Execute)
  PendingValue := value;
  LastPendingPtr := value;
  PendingWritten := PUA_Int32(value^.value.data)^;
  PendingDue := GetTickCount64 + 300;
  Result := UA_STATUSCODE_GOODCOMPLETESASYNCHRONOUSLY;
end;

procedure AddValueSourceVar(const AName: AnsiString; ARead: UA_CallbackValueSource_read;
  AWrite: UA_CallbackValueSource_write);
var
  attr: UA_VariableAttributes;
  src: UA_CallbackValueSource;
  st: UA_StatusCode;
begin
  attr := UA_VariableAttributes_default;
  attr.dataType := UA_TYPES[UA_TYPES_INT32].typeId;
  attr.valueRank := UA_VALUERANK_SCALAR;
  attr.displayName := _UA_LOCALIZEDTEXT_ALLOC('en-US', AName);
  attr.accessLevel := UA_ACCESSLEVELMASK_READ or UA_ACCESSLEVELMASK_WRITE;
  attr.userAccessLevel := attr.accessLevel;
  src.read := ARead;
  src.write := AWrite;
  st := UA_Server_addCallbackValueSourceVariableNode(Server, UA_NODEID_STRING_ALLOC(Ns, AName),
          UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER), UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
          _UA_QUALIFIEDNAME_ALLOC(Ns, AName), UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE),
          attr, src, nil, nil);
  UA_clear(@attr.displayName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  if st <> UA_STATUSCODE_GOOD then
    raise Exception.Create('add ' + AName + ': ' + Name(st));
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
  done: PUA_DataValue;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  Server := UA_Server_new();
  try
    try
      st := UA_ServerConfig_setMinimal(UA_Server_getConfig(Server), Port, nil);
      if st <> UA_STATUSCODE_GOOD then raise Exception.Create('config: ' + Name(st));
      Ns := UA_Server_addNamespace(Server, PAnsiChar(NsUri));
      AddValueSourceVar('SyncVar', @ReadSync, @WriteSync);
      AddValueSourceVar('AsyncVar', @ReadAsyncVar, @WriteAsyncVar);
      AddValueSourceVar('ReadOnlyVar', @ReadSync, nil);
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
      UA_Server_run_iterate(Server, false);
      // the "device" answered: the result goes to the waiting client
      if (PendingValue <> nil) and (GetTickCount64 >= PendingDue) then begin
        done := PendingValue;
        PendingValue := nil;
        if PendingWritten = 13 then
          UA_Server_setAsyncWriteResult(Server, done, UA_STATUSCODE_BADCOMMUNICATIONERROR)
        else begin
          AsyncValue := PendingWritten;
          UA_Server_setAsyncWriteResult(Server, done, UA_STATUSCODE_GOOD);
        end;
        // the same pointer again: that operation is no longer waiting
        StaleResult := UA_Server_setAsyncWriteResult(Server, done, UA_STATUSCODE_GOOD);
      end;
      Sleep(1);
    end;
    UA_Server_run_shutdown(Server);
  finally
    UA_Server_delete(Server);
    Server := nil;
  end;
end;

{ client helpers }

function NewClient: PUA_Client;
var
  st: UA_StatusCode;
begin
  Result := UA_Client_new();
  UA_ClientConfig_setDefault(UA_Client_getConfig(Result));
  st := UA_Client_connect(Result, PAnsiChar('opc.tcp://localhost:' + IntToStr(Port)));
  if st <> UA_STATUSCODE_GOOD then
    raise Exception.Create('connect: ' + Name(st));
end;

function ReadInt(Client: PUA_Client; AName: AnsiString; out V: Integer): UA_StatusCode;
var
  val: UA_Variant;
begin
  V := -1;
  Result := UA_Client_readValueAttribute_range(Client, UA_NODEID_STRING(Ns, AName), '', val);
  if (val.data <> nil) and (val._type = @UA_TYPES[UA_TYPES_INT32]) then
    V := PUA_Int32(val.data)^;
  UA_clear(@val, @UA_TYPES[UA_TYPES_VARIANT]);
end;

function WriteInt(Client: PUA_Client; AName: AnsiString; V: UA_Int32): UA_StatusCode;
var
  val: UA_Variant;
begin
  UA_Variant_init(val);
  UA_Variant_setScalar(@val, @V, @UA_TYPES[UA_TYPES_INT32]);
  Result := UA_Client_writeValueAttribute(Client, UA_NODEID_STRING(Ns, AName), @val);
end;

{ TSlowWriter }

constructor TSlowWriter.Create(AValue: Integer);
begin
  Value := AValue;
  inherited Create(false);
end;

procedure TSlowWriter.Execute;
var
  c: PUA_Client;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  c := NewClient;
  try
    Result := WriteInt(c, 'AsyncVar', Value);
  finally
    UA_Client_disconnect(c);
    UA_Client_delete(c);
  end;
end;

var
  srv: TServerThread;
  client: PUA_Client;
  v: Integer;
  st: UA_StatusCode;
  t0, spent: QWord;
  writer: TSlowWriter;
  roName: AnsiString;
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
    client := NewClient;
    try
      // read through the callback
      st := ReadInt(client, 'SyncVar', v);
      Check((st = UA_STATUSCODE_GOOD) and (v = 42), 'read from the callback (' + Name(st) + ', ' + IntToStr(v) + ')');

      // sync write accepted / refused by the callback
      st := WriteInt(client, 'SyncVar', 50);
      Check(st = UA_STATUSCODE_GOOD, 'sync write accepted (' + Name(st) + ')');
      ReadInt(client, 'SyncVar', v);
      Check(v = 50, 'the written value is read back (' + IntToStr(v) + ')');
      st := WriteInt(client, 'SyncVar', 500);
      Check(st = UA_STATUSCODE_BADOUTOFRANGE, 'sync write refused with the callback status (' + Name(st) + ')');

      // the status of the value reaches the client
      SyncStatus := UA_STATUSCODE_BADCOMMUNICATIONERROR;
      st := ReadInt(client, 'SyncVar', v);
      Check(st = UA_STATUSCODE_BADCOMMUNICATIONERROR, 'the value status reaches the client (' + Name(st) + ')');
      SyncStatus := UA_STATUSCODE_GOOD;

      // a variable without write callback is not writable
      st := WriteInt(client, 'ReadOnlyVar', 1);
      Check(st <> UA_STATUSCODE_GOOD, 'no write callback, no write (' + Name(st) + ')');

      // async write: the client waits for the result given later
      t0 := GetTickCount64;
      st := WriteInt(client, 'AsyncVar', 7);
      spent := GetTickCount64 - t0;
      Check(st = UA_STATUSCODE_GOOD, 'async write completed good (' + Name(st) + ')');
      Check(spent >= 250, 'the client waited for the async result (' + IntToStr(spent) + ' ms)');
      ReadInt(client, 'AsyncVar', v);
      Check(v = 7, 'async value applied (' + IntToStr(v) + ')');
      Check(StaleResult = UA_STATUSCODE_BADNOTFOUND, 'a result for a finished operation is BADNOTFOUND (' + Name(StaleResult) + ')');

      st := WriteInt(client, 'AsyncVar', 13);
      Check(st = UA_STATUSCODE_BADCOMMUNICATIONERROR, 'async write failure reaches the client (' + Name(st) + ')');

      // while one client waits for an async write, the server answers the others
      writer := TSlowWriter.Create(21);
      try
        Sleep(50);
        t0 := GetTickCount64;
        st := ReadInt(client, 'SyncVar', v);
        spent := GetTickCount64 - t0;
        Check((st = UA_STATUSCODE_GOOD) and (spent < 200),
              'the server answers during a pending async write (' + IntToStr(spent) + ' ms)');
        writer.WaitFor;
        Check(writer.Result = UA_STATUSCODE_GOOD, 'and the pending write completes (' + Name(writer.Result) + ')');
      finally
        writer.Free;
      end;

      // delete a node
      roName := 'ReadOnlyVar';
      st := UA_Server_deleteNode(Server, UA_NODEID_STRING(Ns, roName), true);
      Check(st = UA_STATUSCODE_GOOD, 'deleteNode (' + Name(st) + ')');
      st := ReadInt(client, 'ReadOnlyVar', v);
      Check(st = UA_STATUSCODE_BADNODEIDUNKNOWN, 'a deleted node is unknown (' + Name(st) + ')');
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
