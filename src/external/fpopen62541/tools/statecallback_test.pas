{ Tests the client stateCallback and timeout, configured the way issue #11 does it
  (fields set first, then UA_ClientConfig_setDefault), with a server in the same process:

  - connect: the callback reports the channel OPEN and the session ACTIVATED;
  - the server goes away: inside UA_Client_run_iterate the callback reports that
    the channel is no longer open (the client then tries to reconnect);
  - connecting to a host that does not answer gives up after config.timeout
    (1 s here), not after the 5 s default;
  - disconnect reports the session and the channel CLOSED.

  The callback runs inside the client calls (connect, run_iterate, disconnect...):
  an application has to call UA_Client_run_iterate all the time (timer or thread).

    fpc -Fu.. -Fi.. statecallback_test.pas && ./statecallback_test [/path/to/libopen62541.so]

  Exit code 0 when every check passes. }
program statecallback_test;
{$MODE OBJFPC}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Math, SyncObjs, open62541;

const
  Port = 48494;
  // TEST-NET-1 (RFC 5737): nothing answers there
  DeadEndpoint = 'opc.tcp://192.0.2.1:4840';

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
  Calls: Integer = 0;
  LastChannel: UA_SecureChannelState;
  LastSession: UA_SessionState;
  LastStatus: UA_StatusCode;

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

function StateText: String;
begin
  WriteStr(Result, LastChannel, '/', LastSession, '/', Name(LastStatus));
end;

procedure StateCallback(client: PUA_Client; channelState: UA_SecureChannelState;
  sessionState: UA_SessionState; connectStatus: UA_StatusCode); cdecl;
begin
  Inc(Calls);
  LastChannel := channelState;
  LastSession := sessionState;
  LastStatus := connectStatus;
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
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  server := UA_Server_new();
  try
    st := UA_ServerConfig_setMinimal(UA_Server_getConfig(server), Port, nil);
    if st = UA_STATUSCODE_GOOD then
      st := UA_Server_run_startup(server);
    if st <> UA_STATUSCODE_GOOD then begin
      StartError := Name(st);
      Started.SetEvent;
      Exit;
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

function NewClient: PUA_Client;
var
  cc: PUA_ClientConfig;
begin
  Result := UA_Client_new();
  cc := UA_Client_getConfig(Result);
  // as in the issue: the fields first, then setDefault (which keeps them)
  cc^.timeout := 1000;
  cc^.connectivityCheckInterval := 1000;
  cc^.stateCallback := @StateCallback;
  UA_ClientConfig_setDefault(cc);
end;

var
  srv: TServerThread;
  client: PUA_Client;
  st: UA_StatusCode;
  t0, spent: QWord;
  callsBefore: Integer;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  LoadOpen62541(ParamStr(1));
  srv := TServerThread.Create;
  srv.Started.WaitFor(5000);
  if srv.StartError <> '' then begin
    WriteLn('server did not start: ', srv.StartError);
    Halt(2);
  end;

  client := NewClient;
  try
    Check(UA_Client_getConfig(client)^.timeout = 1000, 'setDefault keeps the timeout set before it (' +
          IntToStr(UA_Client_getConfig(client)^.timeout) + ')');

    st := UA_Client_connect(client, PAnsiChar('opc.tcp://localhost:' + IntToStr(Port)));
    Check(st = UA_STATUSCODE_GOOD, 'connect (' + Name(st) + ')');
    Check((Calls > 0) and (LastChannel = UA_SECURECHANNELSTATE_OPEN) and (LastSession = UA_SESSIONSTATE_ACTIVATED),
          'stateCallback called ' + IntToStr(Calls) + ' times, last ' + StateText);

    // the server goes away
    callsBefore := Calls;
    srv.Terminate;
    srv.WaitFor;
    FreeAndNil(srv);
    t0 := GetTickCount64;
    while (LastChannel = UA_SECURECHANNELSTATE_OPEN) and (GetTickCount64 - t0 < 5000) do
      UA_Client_run_iterate(client, 100);
    // (the client then tries to reconnect by itself: noReconnect = false)
    Check((Calls > callsBefore) and (LastChannel <> UA_SECURECHANNELSTATE_OPEN),
          'server gone: reported inside run_iterate, last ' + StateText);

    st := UA_Client_disconnect(client);
    Check((LastChannel = UA_SECURECHANNELSTATE_CLOSED) and (LastSession = UA_SESSIONSTATE_CLOSED),
          'disconnect (' + Name(st) + '), last ' + StateText);
  finally
    UA_Client_delete(client);
    srv.Free;
  end;

  // nothing answers: the connect gives up after config.timeout
  client := NewClient;
  try
    callsBefore := Calls;
    t0 := GetTickCount64;
    st := UA_Client_connect(client, DeadEndpoint);
    spent := GetTickCount64 - t0;
    Check(st <> UA_STATUSCODE_GOOD, 'connect to ' + DeadEndpoint + ' fails (' + Name(st) + ')');
    Check(spent < 2500, 'after about config.timeout = 1000 ms (' + IntToStr(spent) + ' ms)');
    Check((Calls > callsBefore) and (LastStatus <> UA_STATUSCODE_GOOD), 'reported to the callback, last ' + StateText);
  finally
    UA_Client_delete(client);
  end;

  if Failures = 0 then
    WriteLn('all checks passed')
  else
    WriteLn(Failures, ' check(s) failed');
  Halt(Ord(Failures > 0));
end.
