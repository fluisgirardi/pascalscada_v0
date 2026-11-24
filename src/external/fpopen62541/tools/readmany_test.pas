{ Reads several nodes with one request (UA_Client_Service_read), with a server in the
  same process (issue #10): a Float, a UInt16 and a String variable with string
  NodeIds, plus a NodeId that does not exist.

  Each entry of response.results is a UA_DataValue in the order of nodesToRead:
  its own status (one node can fail while the others are read), the
  UA_DATAVALUE_HAS* flags and the value (a UA_Variant).

    fpc -Fu.. -Fi.. readmany_test.pas && ./readmany_test [/path/to/libopen62541.so]

  Exit code 0 when every check passes. }
program readmany_test;
{$MODE OBJFPC}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Math, SyncObjs, open62541;

const
  Port = 48495;
  NS = 1;

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

var
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
  server: PUA_Server;
  st: UA_StatusCode;

  procedure AddVar(const AName: AnsiString; const AValue: Pointer; AType: Integer);
  var
    attr: UA_VariableAttributes;
  begin
    attr := UA_VariableAttributes_default;
    UA_Variant_setScalar(@attr.value, AValue, @UA_TYPES[AType]);
    attr.dataType := UA_TYPES[AType].typeId;
    attr.valueRank := UA_VALUERANK_SCALAR;
    st := UA_Server_addVariableNode(server, UA_NODEID_STRING_ALLOC(NS, AName),
            UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER), UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
            _UA_QUALIFIEDNAME_ALLOC(NS, AName), UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE),
            attr, nil, nil);
    if st <> UA_STATUSCODE_GOOD then
      raise Exception.Create('add ' + AName + ': ' + Name(st));
  end;

var
  f: UA_Float;
  w: UA_UInt16;
  s: UA_String;
  txt: AnsiString;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  server := UA_Server_new();
  try
    try
      st := UA_ServerConfig_setMinimal(UA_Server_getConfig(server), Port, nil);
      if st <> UA_STATUSCODE_GOOD then raise Exception.Create('config: ' + Name(st));
      // the server copies the values
      f := 12.5;
      AddVar('::OPCUA:Variable_01', @f, UA_TYPES_FLOAT);
      w := 4711;
      AddVar('::OPCUA:Variable_02', @w, UA_TYPES_UINT16);
      txt := 'hello';
      s := _UA_STRING(txt);
      AddVar('::OPCUA:Variable_03', @s, UA_TYPES_STRING);
      st := UA_Server_run_startup(server);
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
      UA_Server_run_iterate(server, false);
      Sleep(1);
    end;
    UA_Server_run_shutdown(server);
  finally
    UA_Server_delete(server);
  end;
end;

const
  ItemCount = 4;
  ItemNames: array[0..ItemCount-1] of AnsiString = (
    '::OPCUA:Variable_01',  // Float
    '::OPCUA:Variable_02',  // UInt16
    '::OPCUA:Variable_03',  // String
    '::OPCUA:DoesNotExist'
  );

var
  srv: TServerThread;
  client: PUA_Client;
  st: UA_StatusCode;
  items: array[0..ItemCount-1] of UA_ReadValueId;
  names: array[0..ItemCount-1] of AnsiString;
  request: UA_ReadRequest;
  response: UA_ReadResponse;
  dv: PUA_DataValue;
  i: Integer;
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
    try
      st := UA_Client_connect(client, PAnsiChar('opc.tcp://localhost:' + IntToStr(Port)));
      if st <> UA_STATUSCODE_GOOD then begin
        WriteLn('connect: ', Name(st));
        Halt(2);
      end;

      // the request: one UA_ReadValueId per node. The NodeIds point to the
      // strings in "names" (UA_NODEID_STRING does not copy), so nothing here
      // needs to be freed: the request is not cleared.
      for i := 0 to ItemCount-1 do begin
        names[i] := ItemNames[i];
        FillChar(items[i], SizeOf(items[i]), 0);
        items[i].nodeId := UA_NODEID_STRING(NS, names[i]);
        items[i].attributeId := Ord(UA_ATTRIBUTEID_VALUE);
      end;
      UA_ReadRequest_init(request);
      request.nodesToRead := @items[0];
      request.nodesToReadSize := ItemCount;
      request.timestampsToReturn := UA_TIMESTAMPSTORETURN_SOURCE;

      response := UA_Client_Service_read(client, request);
      try
        st := response.responseHeader.serviceResult;
        Check(st = UA_STATUSCODE_GOOD, 'Service_read (' + Name(st) + ')');
        Check(response.resultsSize = ItemCount, 'one result per node (' + IntToStr(response.resultsSize) + ')');
        if response.resultsSize = ItemCount then begin
          // results[i] answers nodesToRead[i]
          dv := @response.results[0];
          Check((dv^.status = UA_STATUSCODE_GOOD) and ((dv^.flags and UA_DATAVALUE_HASVALUE) <> 0) and
                UA_Variant_hasScalarType(@dv^.value, @UA_TYPES[UA_TYPES_FLOAT]) and
                (UA_Variant_getFloat(dv^.value) = 12.5),
                'Variable_01 Float = ' + FloatToStr(UA_Variant_getFloat(dv^.value)));
          dv := @response.results[1];
          Check((dv^.status = UA_STATUSCODE_GOOD) and UA_Variant_hasScalarType(@dv^.value, @UA_TYPES[UA_TYPES_UINT16]) and
                (PUA_UInt16(dv^.value.data)^ = 4711),
                'Variable_02 UInt16 = ' + IntToStr(PUA_UInt16(dv^.value.data)^));
          dv := @response.results[2];
          Check((dv^.status = UA_STATUSCODE_GOOD) and UA_Variant_hasScalarType(@dv^.value, @UA_TYPES[UA_TYPES_STRING]) and
                (UA_Variant_getString(dv^.value) = 'hello'),
                'Variable_03 String = ' + UA_Variant_getString(dv^.value));
          Check((response.results[0].flags and UA_DATAVALUE_HASSOURCETIMESTAMP) <> 0, 'source timestamp returned');
          dv := @response.results[3];
          Check((dv^.status = UA_STATUSCODE_BADNODEIDUNKNOWN) and ((dv^.flags and UA_DATAVALUE_HASVALUE) = 0),
                'a missing node fails alone (' + Name(dv^.status) + ')');
        end;
      finally
        // frees the results (the values, strings...)
        UA_ReadResponse_clear(response);
      end;
      UA_Client_disconnect(client);
    finally
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
