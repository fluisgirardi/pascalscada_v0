{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Sessão OPC UA (sobre a open62541) usada pelo TOPCUADriver.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Cuida da biblioteca (carregada só na primeira conexão), da configuração do cliente,
  da segurança (modo, política, certificado do cliente - gerado e guardado se não for
  informado -, lista de confiança dos servidores), do usuário/senha, da tabela de
  namespaces (endereços "nsu=") e do browse. Não é thread-safe: quem a usa serializa.
}
{$ELSE}
{:
  @abstract(OPC UA session (on top of open62541) used by TOPCUADriver.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Takes care of the library (loaded on the first connection only), the client
  configuration, the security (mode, policy, client certificate - generated and kept
  when none is given -, trust list of the servers), user/password, the namespace table
  ("nsu=" addresses) and browsing. It is not thread safe: its user serializes the calls.
}
{$ENDIF}
unit OPCUAClient;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Math, open62541;

type
  {$IFDEF PORTUGUES}
  //: Modo de segurança das mensagens.
  {$ELSE}
  //: Message security mode.
  {$ENDIF}
  TOPCUASecurityMode = (osmNone, osmSign, osmSignAndEncrypt);

  {$IFDEF PORTUGUES}
  //: Política de segurança. ospAuto deixa o cliente escolher a melhor que o servidor oferece no modo pedido.
  {$ELSE}
  //: Security policy. ospAuto lets the client pick the best the server offers for the requested mode.
  {$ENDIF}
  TOPCUASecurityPolicy = (ospAuto, ospBasic128Rsa15, ospBasic256, ospBasic256Sha256,
                          ospAes128Sha256RsaOaep, ospAes256Sha256RsaPss);

  TOPCUALogLevel = (ollTrace, ollDebug, ollInfo, ollWarning, ollError, ollFatal, ollNone);

  TOPCUALogEvent = procedure(Sender:TObject; Level:TOPCUALogLevel; const Msg:String) of object;

  TOPCUANodeClass = (oncUnspecified, oncObject, oncVariable, oncMethod, oncObjectType,
                     oncVariableType, oncReferenceType, oncDataType, oncView);

  //: A reference returned by TOPCUAClient.Browse.
  TOPCUABrowseItem = record
    NodeId:AnsiString;         //printed with "nsu=" when the namespace is not 0
    NodeIdNs:AnsiString;       //printed with "ns="
    BrowseName:AnsiString;
    DisplayName:AnsiString;
    NodeClass:TOPCUANodeClass;
    TypeDefinition:AnsiString;
  end;
  TOPCUABrowseItemArray = array of TOPCUABrowseItem;

  //: What a variable node is (TOPCUAClient.ReadVariableInfo).
  TOPCUAVariableInfo = record
    DataTypeId:AnsiString;
    DataTypeName:AnsiString;
    BuiltinType:PUA_DataType; //nil when the type is not a builtin/enumeration (structures)
    ValueRank:Integer;
    ArrayDimensions:array of Cardinal;
    AccessLevel:Byte;
  end;

  { TOPCUAClient }

  TOPCUAClient = class
  private
    FClient:PUA_Client;
    FLogger:UA_Logger;
    FEndpointUrl:AnsiString;
    FUsername, FPassword:AnsiString;
    FSecurityMode:TOPCUASecurityMode;
    FSecurityPolicy:TOPCUASecurityPolicy;
    FCertificateFile, FPrivateKeyFile:String;
    FCertificateDir:String;
    FTrustListDir:String;
    FApplicationUri, FApplicationName:AnsiString;
    FTimeout, FSessionTimeout:Cardinal;
    FAllowUnencryptedPassword:Boolean;
    FLibraryName:String;
    FLibLoaded:Boolean;
    FLogLevel:TOPCUALogLevel;
    FOnLog:TOPCUALogEvent;
    FLastError:String;
    FNamespaces:array of AnsiString;
    function SecurityPolicyUri:AnsiString;
    function SetupSecurity(Config:PUA_ClientConfig):UA_StatusCode;
    procedure SetConfigString(var S:UA_String; const Value:AnsiString);
    procedure ReadNamespaces;
    function FormatNodeId(const Id:UA_NodeId; UseUri:Boolean):AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    //: Loads libopen62541 (the first call only). Returns false (see LastError) when it can not.
    function LoadLibrary:Boolean;
    {: Opens the session. Returns the status of UA_Client_connect (or of the configuration). }
    function Connect:UA_StatusCode;
    procedure Disconnect;
    //: True while the session is activated.
    function Connected:Boolean;
    //: Processes what the server sent (subscription notifications) for up to TimeoutMs.
    function Iterate(TimeoutMs:Cardinal):UA_StatusCode;

    {: Parses a NodeId written as text: "ns=3;s=Name", "i=2258", "nsu=urn:...;s=Name"
       (the namespace URI is looked up in the namespace array of the server). The NodeId
       must be cleared by the caller (UA_NodeId_clear). }
    function ParseNodeId(const Text:AnsiString; out Id:UA_NodeId):UA_StatusCode;
    //: The index of a namespace URI on the server, -1 when it is not there.
    function NamespaceIndex(const Uri:AnsiString):Integer;
    function NamespaceUri(Index:Integer):AnsiString;

    {: The forward hierarchical references of a node (its children). }
    function Browse(const NodeIdText:AnsiString; out Items:TOPCUABrowseItemArray):UA_StatusCode;
    //: DataType, ValueRank, ArrayDimensions and AccessLevel of a variable.
    function ReadVariableInfo(const Id:UA_NodeId; out Info:TOPCUAVariableInfo):UA_StatusCode;

    {: The client certificate and key (DER or PEM) in use: CertificateFile/PrivateKeyFile, or the
       ones generated in CertificateDir when those are empty (generated on the first call). }
    function GetClientCertificate(out Cert, Key:TBytes):Boolean;
    //: The certificates the endpoints of the server offer (DER), to be trusted.
    function GetServerCertificates(out Certs:TStringList):UA_StatusCode;

    property Handle:PUA_Client read FClient;
    property LastError:String read FLastError;

    property EndpointUrl:AnsiString read FEndpointUrl write FEndpointUrl;
    property Username:AnsiString read FUsername write FUsername;
    property Password:AnsiString read FPassword write FPassword;
    property SecurityMode:TOPCUASecurityMode read FSecurityMode write FSecurityMode;
    property SecurityPolicy:TOPCUASecurityPolicy read FSecurityPolicy write FSecurityPolicy;
    property CertificateFile:String read FCertificateFile write FCertificateFile;
    property PrivateKeyFile:String read FPrivateKeyFile write FPrivateKeyFile;
    //: Where the generated client certificate is kept (default: <app config dir>/opcua-pki).
    property CertificateDir:String read FCertificateDir write FCertificateDir;
    //: Directory with the trusted server certificates. Empty accepts any server certificate.
    property TrustListDir:String read FTrustListDir write FTrustListDir;
    property ApplicationUri:AnsiString read FApplicationUri write FApplicationUri;
    property ApplicationName:AnsiString read FApplicationName write FApplicationName;
    property Timeout:Cardinal read FTimeout write FTimeout;
    property SessionTimeout:Cardinal read FSessionTimeout write FSessionTimeout;
    property AllowUnencryptedPassword:Boolean read FAllowUnencryptedPassword write FAllowUnencryptedPassword;
    property LibraryName:String read FLibraryName write FLibraryName;
    property LogLevel:TOPCUALogLevel read FLogLevel write FLogLevel;
    property OnLog:TOPCUALogEvent read FOnLog write FOnLog;
  end;

const
  //: Default application URI of the client (it goes in the SAN of the generated certificate).
  OPCUADefaultApplicationUri = 'urn:pascalscada:opcua:client';

{$IFDEF PORTUGUES}
{:
  Carrega a libopen62541 1.5 (uma vez por processo). Com LibName vazio tenta, nesta ordem:
  o nome padrão do sistema, o nome com a versão (libopen62541.so.1.5, Linux) e os mesmos
  nomes na pasta do executável - assim a lib pode ir junto com a aplicação. Em caso de
  falha devolve false e, em Error, os lugares tentados.
}
{$ELSE}
{:
  Loads libopen62541 1.5 (once per process). With an empty LibName it tries, in this order:
  the default system name, the versioned name (libopen62541.so.1.5, Linux) and the same
  names in the folder of the executable - so the library may ship with the application.
  On failure returns false and, in Error, the places tried.
}
{$ENDIF}
function OPCUALoadLibrary(const LibName:String; out Error:String):Boolean;

function OPCUAStatusText(Status:UA_StatusCode):String;

{: FPC unmasks the floating point exceptions, C code does not expect them: a division by
   zero or an invalid operation inside libopen62541 would become a Pascal exception that
   unwinds through the library (and leaves its event loop marked as running for good).
   Mask them around the calls to the library (the mask is per thread). }
function OPCUAMaskFPU:TFPUExceptionMask;
procedure OPCUARestoreFPU(const Mask:TFPUExceptionMask);

implementation

uses
  {$IFDEF UNIX}Unix{$ELSE}Windows{$ENDIF};

function OPCUALoadLibrary(const LibName:String; out Error:String):Boolean;
var
  names:TStringList;
  exeDir:String;
  i:Integer;
begin
  Error := '';
  names := TStringList.Create;
  try
    if LibName<>'' then
      names.Add(LibName)
    else begin
      exeDir := ExtractFilePath(ParamStr(0));
      names.Add(libopen62541);
      {$IFDEF LINUX}
      //the run time package has no unversioned (-dev) symlink
      names.Add('libopen62541.so.1.5');
      {$ENDIF}
      //dlopen does not look in the folder of the executable (Windows does)
      names.Add(exeDir+libopen62541);
      {$IFDEF LINUX}
      names.Add(exeDir+'libopen62541.so.1.5');
      {$ENDIF}
    end;
    for i:=0 to names.Count-1 do
      try
        LoadOpen62541(names[i]);
        exit(true);
      except
        on e:Exception do
          Error := Error+e.Message+sLineBreak;
      end;
    Error := 'libopen62541 1.5 not found. Install it, put it next to the executable or give its path '+
             '(LibraryName / OPEN62541_LIB).'+sLineBreak+Trim(Error);
    Result := false;
  finally
    names.Free;
  end;
end;

function OPCUAStatusText(Status:UA_StatusCode):String;
begin
  if Open62541Loaded then
    Result := Format('%s (0x%.8x)', [String(UA_StatusCode_name(Status)), Status])
  else
    Result := Format('0x%.8x', [Status]);
end;

function OPCUAMaskFPU:TFPUExceptionMask;
begin
  Result := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end;

procedure OPCUARestoreFPU(const Mask:TFPUExceptionMask);
begin
  ClearExceptions(false);
  SetExceptionMask(Mask);
end;

function HostName:AnsiString;
begin
  {$IFDEF UNIX}
  Result := GetHostName;
  {$ELSE}
  Result := GetEnvironmentVariable('COMPUTERNAME');
  {$ENDIF}
  if Result='' then Result := 'localhost';
end;

function ReadFileBytes(const FileName:String; out Data:TBytes):Boolean;
var
  fs:TFileStream;
begin
  SetLength(Data, 0);
  Result := FileExists(FileName);
  if not Result then exit;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Data, fs.Size);
    if Length(Data)>0 then
      fs.ReadBuffer(Data[0], Length(Data));
  finally
    fs.Free;
  end;
end;

procedure WriteFileBytes(const FileName:String; const Data; Size:Integer);
var
  fs:TFileStream;
begin
  ForceDirectories(ExtractFileDir(FileName));
  fs := TFileStream.Create(FileName, fmCreate);
  try
    if Size>0 then
      fs.WriteBuffer(Data, Size);
  finally
    fs.Free;
  end;
end;

function BytesToByteString(const B:TBytes):UA_ByteString;
begin
  Result.length := Length(B);
  if Length(B)>0 then
    Result.data := @B[0]
  else
    Result.data := nil;
end;

procedure ClientLog(logContext:Pointer; level:UA_LogLevel; category:UA_LogCategory; msg:PAnsiChar; args:Pointer); cdecl;
var
  c:TOPCUAClient;
  l:TOPCUALogLevel;
begin
  c := TOPCUAClient(logContext);
  if (c=nil) or (not Assigned(c.FOnLog)) then exit;
  case level of
    UA_LOGLEVEL_TRACE:   l := ollTrace;
    UA_LOGLEVEL_DEBUG:   l := ollDebug;
    UA_LOGLEVEL_INFO:    l := ollInfo;
    UA_LOGLEVEL_WARNING: l := ollWarning;
    UA_LOGLEVEL_ERROR:   l := ollError;
    else                 l := ollFatal;
  end;
  if l<c.FLogLevel then exit;
  try
    c.FOnLog(c, l, String(UA_FormatLogMessage(msg, args)));
  except
    //never let an exception cross the C library
  end;
end;

{ TOPCUAClient }

constructor TOPCUAClient.Create;
begin
  inherited Create;
  FTimeout := 5000;
  FSessionTimeout := 60000;
  FApplicationUri := OPCUADefaultApplicationUri;
  FApplicationName := 'PascalSCADA OPC UA Client';
  FLogLevel := ollWarning;
  FCertificateDir := IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'opcua-pki';
  FillChar(FLogger, SizeOf(FLogger), 0);
  FLogger.log := @ClientLog;
  FLogger.context := Self;
  FLogger.clear := nil; //the logger belongs to this object, not to the client config
end;

destructor TOPCUAClient.Destroy;
begin
  Disconnect;
  if FLibLoaded then
    UnloadOpen62541;
  inherited Destroy;
end;

function TOPCUAClient.LoadLibrary:Boolean;
var
  err:String;
begin
  if not FLibLoaded then begin
    FLibLoaded := OPCUALoadLibrary(FLibraryName, err);
    if not FLibLoaded then
      FLastError := err;
  end;
  Result := FLibLoaded;
end;

function TOPCUAClient.SecurityPolicyUri:AnsiString;
begin
  if FSecurityMode=osmNone then
    exit(UA_SECURITY_POLICY_NONE_URI);
  case FSecurityPolicy of
    ospBasic128Rsa15:       Result := UA_SECURITY_POLICY_BASIC128RSA15_URI;
    ospBasic256:            Result := UA_SECURITY_POLICY_BASIC256_URI;
    ospBasic256Sha256:      Result := UA_SECURITY_POLICY_BASIC256SHA256_URI;
    ospAes128Sha256RsaOaep: Result := UA_SECURITY_POLICY_AES128SHA256RSAOAEP_URI;
    ospAes256Sha256RsaPss:  Result := UA_SECURITY_POLICY_AES256SHA256RSAPSS_URI;
    else                    Result := ''; //auto
  end;
end;

procedure TOPCUAClient.SetConfigString(var S:UA_String; const Value:AnsiString);
begin
  UA_String_clear(S);
  S := _UA_STRING_ALLOC(Value);
end;

function TOPCUAClient.GetClientCertificate(out Cert, Key:TBytes):Boolean;
var
  certFile, keyFile:String;
  subject:array[0..2] of AnsiString;
  san:array[0..1] of AnsiString;
  subj:array[0..2] of UA_String;
  alt:array[0..1] of UA_String;
  outKey, outCert:UA_ByteString;
  st:UA_StatusCode;
  i:Integer;
  params:UA_KeyValueMap;
  kv:array[0..1] of UA_KeyValuePair;
  keySize, days:UA_UInt16;
  kname1, kname2:AnsiString;
begin
  Result := false;
  if FCertificateFile<>'' then begin
    if not ReadFileBytes(FCertificateFile, Cert) then begin
      FLastError := 'Client certificate not found: '+FCertificateFile;
      exit;
    end;
    if not ReadFileBytes(FPrivateKeyFile, Key) then begin
      FLastError := 'Client private key not found: '+FPrivateKeyFile;
      exit;
    end;
    exit(true);
  end;

  //generated certificate, kept so that the server needs to trust it only once
  certFile := IncludeTrailingPathDelimiter(FCertificateDir)+'own'+PathDelim+'pascalscada-client.der';
  keyFile  := IncludeTrailingPathDelimiter(FCertificateDir)+'private'+PathDelim+'pascalscada-client.key';
  if ReadFileBytes(certFile, Cert) and ReadFileBytes(keyFile, Key) and (Length(Cert)>0) and (Length(Key)>0) then
    exit(true);

  {$IF DECLARED(UA_ClientConfig_setDefaultEncryption)}
  if not LoadLibrary then exit;
  subject[0] := 'C=BR';
  subject[1] := 'O=PascalSCADA';
  subject[2] := 'CN='+FApplicationName+'@'+HostName;
  san[0] := 'DNS:'+HostName;
  san[1] := 'URI:'+FApplicationUri;
  for i:=0 to 2 do subj[i] := _UA_STRING(subject[i]);
  for i:=0 to 1 do alt[i] := _UA_STRING(san[i]);
  keySize := 2048;
  days := 3650;
  kname1 := 'key-size-bits';
  kname2 := 'expires-in-days';
  FillChar(kv, SizeOf(kv), 0);
  kv[0].key := _UA_QUALIFIEDNAME(0, kname1);
  UA_Variant_setScalar(@kv[0].value, @keySize, @UA_TYPES[UA_TYPES_UINT16]);
  kv[1].key := _UA_QUALIFIEDNAME(0, kname2);
  UA_Variant_setScalar(@kv[1].value, @days, @UA_TYPES[UA_TYPES_UINT16]);
  params.mapSize := 2;
  params.map := @kv[0];
  FillChar(outKey, SizeOf(outKey), 0);
  FillChar(outCert, SizeOf(outCert), 0);
  st := UA_CreateCertificate(@FLogger, @subj[0], 3, @alt[0], 2, UA_CERTIFICATEFORMAT_DER, @params, @outKey, @outCert);
  if st<>UA_STATUSCODE_GOOD then begin
    FLastError := 'Could not create the client certificate: '+OPCUAStatusText(st);
    exit;
  end;
  try
    SetLength(Cert, outCert.length);
    if outCert.length>0 then Move(outCert.data^, Cert[0], outCert.length);
    SetLength(Key, outKey.length);
    if outKey.length>0 then Move(outKey.data^, Key[0], outKey.length);
    try
      WriteFileBytes(certFile, Cert[0], Length(Cert));
      WriteFileBytes(keyFile, Key[0], Length(Key));
    except
      on e:Exception do
        FLastError := 'The generated client certificate could not be saved: '+e.Message;
    end;
  finally
    UA_clear(@outCert, @UA_TYPES[UA_TYPES_BYTESTRING]);
    UA_clear(@outKey, @UA_TYPES[UA_TYPES_BYTESTRING]);
  end;
  Result := true;
  {$ELSE}
  FLastError := 'This binding was built without encryption support';
  {$IFEND}
end;

function TOPCUAClient.SetupSecurity(Config:PUA_ClientConfig):UA_StatusCode;
var
  cert, key:TBytes;
  trust:array of TBytes;
  trustBS:array of UA_ByteString;
  sr:TSearchRec;
  n, i:Integer;
  ext:String;
begin
  if FSecurityMode=osmNone then begin
    Result := UA_ClientConfig_setDefault(Config);
    exit;
  end;
  {$IF DECLARED(UA_ClientConfig_setDefaultEncryption)}
  if not GetClientCertificate(cert, key) then
    exit(UA_STATUSCODE_BADCERTIFICATEINVALID);

  n := 0;
  SetLength(trust, 0);
  if (FTrustListDir<>'') and (FindFirst(IncludeTrailingPathDelimiter(FTrustListDir)+'*', faAnyFile, sr)=0) then
    try
      repeat
        ext := LowerCase(ExtractFileExt(sr.Name));
        if ((sr.Attr and faDirectory)=0) and ((ext='.der') or (ext='.pem') or (ext='.crt') or (ext='.cer')) then begin
          SetLength(trust, n+1);
          if ReadFileBytes(IncludeTrailingPathDelimiter(FTrustListDir)+sr.Name, trust[n]) and (Length(trust[n])>0) then
            Inc(n);
        end;
      until FindNext(sr)<>0;
    finally
      FindClose(sr);
    end;
  SetLength(trustBS, n);
  for i:=0 to n-1 do
    trustBS[i] := BytesToByteString(trust[i]);

  if n>0 then
    Result := UA_ClientConfig_setDefaultEncryption(Config, BytesToByteString(cert), BytesToByteString(key),
                                                   @trustBS[0], n, nil, 0)
  else begin
    Result := UA_ClientConfig_setDefaultEncryption(Config, BytesToByteString(cert), BytesToByteString(key),
                                                   nil, 0, nil, 0);
    //no trust list: any server certificate is accepted
    if Result=UA_STATUSCODE_GOOD then begin
      if Assigned(Config^.certificateVerification.clear) then
        Config^.certificateVerification.clear(@Config^.certificateVerification);
      UA_CertificateGroup_AcceptAll(@Config^.certificateVerification);
    end;
  end;
  {$ELSE}
  Result := UA_STATUSCODE_BADNOTSUPPORTED;
  {$IFEND}
end;

function TOPCUAClient.Connect:UA_StatusCode;
var
  config:UA_ClientConfig;
  policy:AnsiString;
begin
  Disconnect;
  FLastError := '';
  if not LoadLibrary then
    exit(UA_STATUSCODE_BADINTERNALERROR);

  FillChar(config, SizeOf(config), 0);
  config.logging := @FLogger;
  Result := SetupSecurity(@config);
  if Result<>UA_STATUSCODE_GOOD then begin
    if FLastError='' then
      FLastError := 'Client configuration failed: '+OPCUAStatusText(Result);
    UA_ClientConfig_clear(@config);
    exit;
  end;

  config.timeout := FTimeout;
  config.requestedSessionTimeout := FSessionTimeout;
  config.noReconnect := false;
  config.allowNonePolicyPassword := FAllowUnencryptedPassword;
  SetConfigString(config.clientDescription.applicationUri, FApplicationUri);
  UA_clear(@config.clientDescription.applicationName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  config.clientDescription.applicationName := _UA_LOCALIZEDTEXT_ALLOC('en-US', FApplicationName);
  case FSecurityMode of
    osmNone:           config.securityMode := UA_MESSAGESECURITYMODE_NONE;
    osmSign:           config.securityMode := UA_MESSAGESECURITYMODE_SIGN;
    osmSignAndEncrypt: config.securityMode := UA_MESSAGESECURITYMODE_SIGNANDENCRYPT;
  end;
  policy := SecurityPolicyUri;
  SetConfigString(config.securityPolicyUri, policy);

  if FUsername<>'' then begin
    Result := UA_ClientConfig_setAuthenticationUsername(@config, PAnsiChar(FUsername), PAnsiChar(FPassword));
    if Result<>UA_STATUSCODE_GOOD then begin
      FLastError := 'Username configuration failed: '+OPCUAStatusText(Result);
      UA_ClientConfig_clear(@config);
      exit;
    end;
  end;

  FClient := UA_Client_newWithConfig(@config); //the config is moved into the client
  if FClient=nil then begin
    UA_ClientConfig_clear(@config);
    FLastError := 'UA_Client_newWithConfig failed';
    exit(UA_STATUSCODE_BADOUTOFMEMORY);
  end;

  Result := UA_Client_connect(FClient, PAnsiChar(FEndpointUrl));
  if Result<>UA_STATUSCODE_GOOD then begin
    FLastError := 'Connection to '+FEndpointUrl+' failed: '+OPCUAStatusText(Result);
    UA_Client_delete(FClient);
    FClient := nil;
    exit;
  end;
  ReadNamespaces;
end;

procedure TOPCUAClient.Disconnect;
begin
  if FClient<>nil then begin
    UA_Client_disconnect(FClient);
    UA_Client_delete(FClient);
    FClient := nil;
  end;
  SetLength(FNamespaces, 0);
end;

function TOPCUAClient.Connected:Boolean;
var
  cs:UA_SecureChannelState;
  ss:UA_SessionState;
  st:UA_StatusCode;
begin
  if FClient=nil then exit(false);
  UA_Client_getState(FClient, @cs, @ss, @st);
  Result := (ss=UA_SESSIONSTATE_ACTIVATED) and (cs=UA_SECURECHANNELSTATE_OPEN) and (not UA_StatusCode_isBad(st));
end;

function TOPCUAClient.Iterate(TimeoutMs:Cardinal):UA_StatusCode;
begin
  if FClient=nil then exit(UA_STATUSCODE_BADNOTCONNECTED);
  Result := UA_Client_run_iterate(FClient, TimeoutMs);
end;

procedure TOPCUAClient.ReadNamespaces;
var
  v:UA_Variant;
  i:Integer;
begin
  SetLength(FNamespaces, 0);
  if FClient=nil then exit;
  UA_Variant_init(v);
  if (UA_Client_readValueAttribute(FClient, UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_NAMESPACEARRAY), @v)=UA_STATUSCODE_GOOD) and
     UA_Variant_hasArrayType(@v, @UA_TYPES[UA_TYPES_STRING]) then begin
    SetLength(FNamespaces, v.arrayLength);
    for i:=0 to v.arrayLength-1 do
      FNamespaces[i] := UA_StringToStr(PUA_String(v.data)[i]);
  end;
  UA_Variant_clear(v);
end;

function TOPCUAClient.NamespaceIndex(const Uri:AnsiString):Integer;
var
  i:Integer;
begin
  if (Length(FNamespaces)=0) and (FClient<>nil) then
    ReadNamespaces;
  for i:=0 to High(FNamespaces) do
    if FNamespaces[i]=Uri then exit(i);
  Result := -1;
end;

function TOPCUAClient.NamespaceUri(Index:Integer):AnsiString;
begin
  if (Length(FNamespaces)=0) and (FClient<>nil) then
    ReadNamespaces;
  if (Index>=0) and (Index<=High(FNamespaces)) then
    Result := FNamespaces[Index]
  else
    Result := '';
end;

function TOPCUAClient.ParseNodeId(const Text:AnsiString; out Id:UA_NodeId):UA_StatusCode;
var
  t, uri, rest:AnsiString;
  p, idx:Integer;
begin
  FillChar(Id, SizeOf(Id), 0);
  t := Trim(Text);
  if SameText(Copy(t, 1, 4), 'nsu=') then begin
    p := Pos(';', t);
    if p<=0 then exit(UA_STATUSCODE_BADNODEIDINVALID);
    uri := Copy(t, 5, p-5);
    rest := Copy(t, p+1, MaxInt);
    idx := NamespaceIndex(uri);
    if idx<0 then exit(UA_STATUSCODE_BADNODEIDUNKNOWN);
    t := 'ns='+IntToStr(idx)+';'+rest;
  end;
  Result := UA_StrToNodeId(t, Id);
end;

function TOPCUAClient.FormatNodeId(const Id:UA_NodeId; UseUri:Boolean):AnsiString;
var
  s, uri:AnsiString;
  p:Integer;
begin
  s := UA_NodeIdToStr(Id);
  if UseUri and (Id.namespaceIndex<>0) then begin
    uri := NamespaceUri(Id.namespaceIndex);
    if uri<>'' then begin
      p := Pos(';', s);
      if (p>0) and (Copy(s, 1, 3)='ns=') then
        s := 'nsu='+uri+Copy(s, p, MaxInt);
    end;
  end;
  Result := s;
end;

function TOPCUAClient.Browse(const NodeIdText:AnsiString; out Items:TOPCUABrowseItemArray):UA_StatusCode;
var
  req:UA_BrowseRequest;
  desc:UA_BrowseDescription;
  resp:UA_BrowseResponse;
  nreq:UA_BrowseNextRequest;
  nresp:UA_BrowseNextResponse;
  cont:UA_ByteString;
  res:PUA_BrowseResult;
  i, n:Integer;

  procedure AddRefs(r:PUA_BrowseResult);
  var
    k:Integer;
    ref:PUA_ReferenceDescription;
  begin
    for k:=0 to r^.referencesSize-1 do begin
      ref := @r^.references[k];
      SetLength(Items, n+1);
      Items[n].NodeId := FormatNodeId(ref^.nodeId.nodeId, true);
      Items[n].NodeIdNs := FormatNodeId(ref^.nodeId.nodeId, false);
      Items[n].BrowseName := UA_StringToStr(ref^.browseName.name);
      Items[n].DisplayName := UA_LocalizedTextToStr(ref^.displayName);
      case ref^.nodeClass of
        UA_NODECLASS_OBJECT:        Items[n].NodeClass := oncObject;
        UA_NODECLASS_VARIABLE:      Items[n].NodeClass := oncVariable;
        UA_NODECLASS_METHOD:        Items[n].NodeClass := oncMethod;
        UA_NODECLASS_OBJECTTYPE:    Items[n].NodeClass := oncObjectType;
        UA_NODECLASS_VARIABLETYPE:  Items[n].NodeClass := oncVariableType;
        UA_NODECLASS_REFERENCETYPE: Items[n].NodeClass := oncReferenceType;
        UA_NODECLASS_DATATYPE:      Items[n].NodeClass := oncDataType;
        UA_NODECLASS_VIEW:          Items[n].NodeClass := oncView;
        else                        Items[n].NodeClass := oncUnspecified;
      end;
      Items[n].TypeDefinition := FormatNodeId(ref^.typeDefinition.nodeId, false);
      Inc(n);
    end;
  end;

begin
  SetLength(Items, 0);
  n := 0;
  if FClient=nil then exit(UA_STATUSCODE_BADNOTCONNECTED);
  UA_BrowseDescription_init(desc);
  if NodeIdText='' then
    desc.nodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER)
  else begin
    Result := ParseNodeId(NodeIdText, desc.nodeId);
    if Result<>UA_STATUSCODE_GOOD then exit;
  end;
  desc.browseDirection := UA_BROWSEDIRECTION_FORWARD;
  desc.referenceTypeId := UA_NODEID_NUMERIC(0, UA_NS0ID_HIERARCHICALREFERENCES);
  desc.includeSubtypes := true;
  desc.nodeClassMask := 0; //all
  desc.resultMask := Ord(UA_BROWSERESULTMASK_ALL);

  UA_BrowseRequest_init(req);
  req.requestedMaxReferencesPerNode := 0;
  req.nodesToBrowse := @desc;
  req.nodesToBrowseSize := 1;
  resp := UA_Client_Service_browse(FClient, req);
  UA_NodeId_clear(desc.nodeId);
  Result := resp.responseHeader.serviceResult;
  if (Result=UA_STATUSCODE_GOOD) and (resp.resultsSize=1) then begin
    res := @resp.results[0];
    Result := res^.statusCode;
    if Result=UA_STATUSCODE_GOOD then begin
      AddRefs(res);
      //continuation points
      FillChar(cont, SizeOf(cont), 0);
      UA_copy(@res^.continuationPoint, @cont, @UA_TYPES[UA_TYPES_BYTESTRING]);
      i := 0;
      while (cont.length>0) and (i<1000) do begin
        FillChar(nreq, SizeOf(nreq), 0);
        nreq.continuationPoints := @cont;
        nreq.continuationPointsSize := 1;
        nresp := UA_Client_Service_browseNext(FClient, nreq);
        UA_clear(@cont, @UA_TYPES[UA_TYPES_BYTESTRING]);
        if (nresp.responseHeader.serviceResult=UA_STATUSCODE_GOOD) and (nresp.resultsSize=1) and
           (nresp.results[0].statusCode=UA_STATUSCODE_GOOD) then begin
          AddRefs(@nresp.results[0]);
          UA_copy(@nresp.results[0].continuationPoint, @cont, @UA_TYPES[UA_TYPES_BYTESTRING]);
        end;
        UA_clear(@nresp, @UA_TYPES[UA_TYPES_BROWSENEXTRESPONSE]);
        Inc(i);
      end;
      UA_clear(@cont, @UA_TYPES[UA_TYPES_BYTESTRING]);
    end;
  end;
  UA_BrowseResponse_clear(resp);
end;

function TOPCUAClient.ReadVariableInfo(const Id:UA_NodeId; out Info:TOPCUAVariableInfo):UA_StatusCode;
var
  items:array[0..3] of UA_ReadValueId;
  req:UA_ReadRequest;
  resp:UA_ReadResponse;
  i:Integer;
  dv:PUA_DataValue;
  dt:PUA_DataType;
begin
  Info.DataTypeId := '';
  Info.DataTypeName := '';
  Info.BuiltinType := nil;
  Info.ValueRank := -1;
  SetLength(Info.ArrayDimensions, 0);
  Info.AccessLevel := 0;
  if FClient=nil then exit(UA_STATUSCODE_BADNOTCONNECTED);
  FillChar(items, SizeOf(items), 0);
  for i:=0 to 3 do items[i].nodeId := Id;
  items[0].attributeId := Ord(UA_ATTRIBUTEID_DATATYPE);
  items[1].attributeId := Ord(UA_ATTRIBUTEID_VALUERANK);
  items[2].attributeId := Ord(UA_ATTRIBUTEID_ARRAYDIMENSIONS);
  items[3].attributeId := Ord(UA_ATTRIBUTEID_ACCESSLEVEL);
  UA_ReadRequest_init(req);
  req.nodesToRead := @items[0];
  req.nodesToReadSize := 4;
  resp := UA_Client_Service_read(FClient, req);
  Result := resp.responseHeader.serviceResult;
  if (Result=UA_STATUSCODE_GOOD) and (resp.resultsSize=4) then begin
    dv := @resp.results[0];
    if (dv^.flags and UA_DATAVALUE_HASSTATUS)<>0 then
      Result := dv^.status;
    if UA_Variant_hasScalarType(@dv^.value, @UA_TYPES[UA_TYPES_NODEID]) then begin
      Info.DataTypeId := UA_NodeIdToStr(PUA_NodeId(dv^.value.data)^);
      dt := UA_findDataType(PUA_NodeId(dv^.value.data));
      if dt<>nil then begin
        Info.DataTypeName := dt^.typeName;
        if UA_DataType_typeKind(dt)=UA_DATATYPEKIND_ENUM then
          Info.BuiltinType := @UA_TYPES[UA_TYPES_INT32]
        else if Ord(UA_DataType_typeKind(dt))<=Ord(UA_DATATYPEKIND_DIAGNOSTICINFO) then
          Info.BuiltinType := dt;
      end else
        Info.DataTypeName := Info.DataTypeId;
    end;
    dv := @resp.results[1];
    if UA_Variant_hasScalarType(@dv^.value, @UA_TYPES[UA_TYPES_INT32]) then
      Info.ValueRank := PUA_Int32(dv^.value.data)^;
    dv := @resp.results[2];
    if UA_Variant_hasArrayType(@dv^.value, @UA_TYPES[UA_TYPES_UINT32]) then begin
      SetLength(Info.ArrayDimensions, dv^.value.arrayLength);
      for i:=0 to dv^.value.arrayLength-1 do
        Info.ArrayDimensions[i] := PUA_UInt32(dv^.value.data)[i];
    end;
    dv := @resp.results[3];
    if UA_Variant_hasScalarType(@dv^.value, @UA_TYPES[UA_TYPES_BYTE]) then
      Info.AccessLevel := PUA_Byte(dv^.value.data)^;
  end;
  UA_ReadResponse_clear(resp);
end;

function TOPCUAClient.GetServerCertificates(out Certs:TStringList):UA_StatusCode;
var
  c:PUA_Client;
  n:size_t;
  eps:PUA_EndpointDescription;
  i:Integer;
  s:AnsiString;
begin
  Certs := TStringList.Create;
  if not LoadLibrary then exit(UA_STATUSCODE_BADINTERNALERROR);
  c := UA_Client_new();
  try
    UA_Client_getConfig(c)^.timeout := FTimeout;
    n := 0;
    eps := nil;
    Result := UA_Client_getEndpoints(c, PAnsiChar(FEndpointUrl), @n, @eps);
    if Result=UA_STATUSCODE_GOOD then begin
      for i:=0 to n-1 do begin
        s := UA_StringToStr(eps[i].serverCertificate);
        if (s<>'') and (Certs.IndexOf(s)<0) then
          Certs.Add(s);
      end;
      UA_Array_delete(eps, n, @UA_TYPES[UA_TYPES_ENDPOINTDESCRIPTION]);
    end;
  finally
    UA_Client_delete(c);
  end;
end;

end.
