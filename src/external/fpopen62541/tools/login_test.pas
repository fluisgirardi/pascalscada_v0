{ Tests a client logging in with user name and password, with a server and
  clients in the same process. The server has a certificate (None, Sign and
  SignAndEncrypt endpoints) and accepts only the user "user"/"secret".

  - a client with UA_ClientConfig_setDefault has no encrypted SecurityPolicy, so
    it can not encrypt the password: the connection is refused, also with
    allowNonePolicyPassword when the server does not accept a password in clear
    text (the default of open62541 servers and of most PLCs);
  - with UA_ClientConfig_setDefaultEncryption the password is encrypted even on
    a channel without security (securityMode None);
  - a wrong password is BADUSERACCESSDENIED, anonymous is refused;
  - the same login on a SignAndEncrypt channel.

    fpc -Fu.. -Fi.. login_test.pas && ./login_test [/path/to/libopen62541.so]

  Exit code 0 when every check passes. }
program login_test;
{$MODE OBJFPC}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Math, SyncObjs, open62541;

const
  Port = 48493;
  // the applicationUri of each side must be the URI in the SAN of its certificate
  ServerUri = 'urn:open62541.unconfigured.application'; // default of UA_ServerConfig_setDefault*
  ClientUri = 'urn:fpopen62541:login_test';             // set in clientDescription below

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
  Server: PUA_Server = nil;
  Failures: Integer = 0;
  ServerCert, ServerKey, ClientCert, ClientKey: UA_ByteString;
  EndpointUrl: AnsiString;
  Logger: PUA_Logger = nil;

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

{ self signed certificate (DER) with the application URI in the SAN }
procedure MakeCertificate(const CN, AppUri: AnsiString; out Cert, Key: UA_ByteString);
var
  subject: array[0..1] of AnsiString;
  san: array[0..1] of AnsiString;
  subj: array[0..1] of UA_String;
  alt: array[0..1] of UA_String;
  kv: array[0..1] of UA_KeyValuePair;
  params: UA_KeyValueMap;
  keySize, days: UA_UInt16;
  k1, k2: AnsiString;
  i: Integer;
  st: UA_StatusCode;
begin
  subject[0] := 'O=fpopen62541';
  subject[1] := 'CN=' + CN;
  san[0] := 'DNS:localhost';
  san[1] := 'URI:' + AppUri;
  for i := 0 to 1 do begin
    subj[i] := _UA_STRING(subject[i]);
    alt[i] := _UA_STRING(san[i]);
  end;
  keySize := 2048;
  days := 30;
  k1 := 'key-size-bits';
  k2 := 'expires-in-days';
  FillChar(kv, SizeOf(kv), 0);
  kv[0].key := _UA_QUALIFIEDNAME(0, k1);
  UA_Variant_setScalar(@kv[0].value, @keySize, @UA_TYPES[UA_TYPES_UINT16]);
  kv[1].key := _UA_QUALIFIEDNAME(0, k2);
  UA_Variant_setScalar(@kv[1].value, @days, @UA_TYPES[UA_TYPES_UINT16]);
  params.mapSize := 2;
  params.map := @kv[0];
  FillChar(Cert, SizeOf(Cert), 0);
  FillChar(Key, SizeOf(Key), 0);
  st := UA_CreateCertificate(Logger, @subj[0], 2, @alt[0], 2, UA_CERTIFICATEFORMAT_DER, @params, @Key, @Cert);
  if st <> UA_STATUSCODE_GOOD then
    raise Exception.Create('UA_CreateCertificate: ' + Name(st));
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
  conf: PUA_ServerConfig;
  login: UA_UsernamePasswordLogin;
  user, pwd: AnsiString;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  Server := UA_Server_new();
  try
    try
      conf := UA_Server_getConfig(Server);
      // the client certificate in the trust list (needed by Sign/SignAndEncrypt)
      st := UA_ServerConfig_setDefaultWithSecurityPolicies(conf, Port, @ServerCert, @ServerKey,
              @ClientCert, 1, nil, 0, nil, 0);
      if st <> UA_STATUSCODE_GOOD then raise Exception.Create('config: ' + Name(st));
      user := 'user';
      pwd := 'secret';
      login.username := _UA_STRING(user);
      login.password := _UA_BYTESTRING(pwd);
      // no anonymous; the user token policy is chosen by the server (encrypted)
      st := UA_AccessControl_default(conf, false, nil, 1, @login);
      if st <> UA_STATUSCODE_GOOD then raise Exception.Create('access control: ' + Name(st));
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
      Sleep(1);
    end;
    UA_Server_run_shutdown(Server);
  finally
    UA_Server_delete(Server);
    Server := nil;
  end;
end;

type
  TClientSetup = (csPlain, csPlainAllowNone, csEncryption);

{ connects a new client and returns the status; the client is deleted }
function TryLogin(Setup: TClientSetup; Mode: UA_MessageSecurityMode; const User, Pwd: AnsiString): UA_StatusCode;
var
  client: PUA_Client;
  cc: PUA_ClientConfig;
begin
  client := UA_Client_new();
  try
    cc := UA_Client_getConfig(client);
    if Setup = csEncryption then begin
      // trusts the server certificate
      Result := UA_ClientConfig_setDefaultEncryption(cc, ClientCert, ClientKey, @ServerCert, 1, nil, 0);
      if Result <> UA_STATUSCODE_GOOD then Exit;
      UA_clear(@cc^.clientDescription.applicationUri, @UA_TYPES[UA_TYPES_STRING]);
      cc^.clientDescription.applicationUri := _UA_STRING_ALLOC(ClientUri);
    end else
      UA_ClientConfig_setDefault(cc);
    cc^.allowNonePolicyPassword := Setup = csPlainAllowNone;
    cc^.securityMode := Mode;
    if User <> '' then
      Result := UA_Client_connectUsername(client, EndpointUrl, User, Pwd)
    else
      Result := UA_Client_connect(client, PAnsiChar(EndpointUrl));
    UA_Client_disconnect(client);
  finally
    UA_Client_delete(client);
  end;
end;

var
  srv: TServerThread;
  st: UA_StatusCode;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  LoadOpen62541(ParamStr(1));
  Logger := UA_Log_Stdout_new(UA_LOGLEVEL_WARNING);
  EndpointUrl := 'opc.tcp://localhost:' + IntToStr(Port);
  MakeCertificate('login_test server', ServerUri, ServerCert, ServerKey);
  MakeCertificate('login_test client', ClientUri, ClientCert, ClientKey);
  srv := TServerThread.Create;
  try
    srv.Started.WaitFor(10000);
    if srv.StartError <> '' then begin
      WriteLn('server did not start: ', srv.StartError);
      Halt(2);
    end;

    st := TryLogin(csPlain, UA_MESSAGESECURITYMODE_NONE, 'user', 'secret');
    Check(st <> UA_STATUSCODE_GOOD, 'setDefault: the password can not be encrypted, refused (' + Name(st) + ')');

    st := TryLogin(csPlainAllowNone, UA_MESSAGESECURITYMODE_NONE, 'user', 'secret');
    Check(st <> UA_STATUSCODE_GOOD,
          'setDefault + allowNonePolicyPassword: the server wants it encrypted, refused (' + Name(st) + ')');

    st := TryLogin(csEncryption, UA_MESSAGESECURITYMODE_NONE, 'user', 'secret');
    Check(st = UA_STATUSCODE_GOOD, 'setDefaultEncryption, channel None: login (' + Name(st) + ')');

    st := TryLogin(csEncryption, UA_MESSAGESECURITYMODE_NONE, 'user', 'wrong');
    Check(st = UA_STATUSCODE_BADUSERACCESSDENIED, 'wrong password (' + Name(st) + ')');

    st := TryLogin(csEncryption, UA_MESSAGESECURITYMODE_NONE, '', '');
    Check(st <> UA_STATUSCODE_GOOD, 'anonymous refused (' + Name(st) + ')');

    st := TryLogin(csEncryption, UA_MESSAGESECURITYMODE_SIGNANDENCRYPT, 'user', 'secret');
    Check(st = UA_STATUSCODE_GOOD, 'setDefaultEncryption, channel SignAndEncrypt: login (' + Name(st) + ')');
  finally
    srv.Terminate;
    srv.WaitFor;
    srv.Free;
    UA_clear(@ServerCert, @UA_TYPES[UA_TYPES_BYTESTRING]);
    UA_clear(@ServerKey, @UA_TYPES[UA_TYPES_BYTESTRING]);
    UA_clear(@ClientCert, @UA_TYPES[UA_TYPES_BYTESTRING]);
    UA_clear(@ClientKey, @UA_TYPES[UA_TYPES_BYTESTRING]);
    if Assigned(Logger^.clear) then Logger^.clear(Logger);
  end;
  if Failures = 0 then
    WriteLn('all checks passed')
  else
    WriteLn(Failures, ' check(s) failed');
  Halt(Ord(Failures > 0));
end.
