{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Driver de protocolo OPC UA (cliente), sobre a biblioteca open62541 1.5.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Não usa CommunicationPort: a conexão é aberta pela própria biblioteca a partir de
  EndpointUrl (@code(opc.tcp://host:4840)). A libopen62541 (.so/.dll) só é carregada na
  primeira conexão; um programa que não usa o driver não precisa dela.

  O endereço de um tag (LongAddress) é o NodeId da variável em texto:
  @unorderedList(
    @item(@code(ns=3;s="DB1"."Velocidade") - índice do namespace;)
    @item(@code(nsu=urn:servidor:ns;s=Nome) - URI do namespace (não muda se o servidor reordenar os namespaces);)
    @item(@code(ns=2;i=1001), @code(ns=2;g=...), @code(ns=2;b=...);)
    @item(um "|" seguido de um IndexRange escolhe elementos de um array: @code(ns=3;s=Arr|4), @code(ns=3;s=Arr|0:9).)
  )

  Tags numéricos (TPLCTagNumber, TPLCBlock) recebem o valor já convertido (o tipo vem do
  servidor): deixe TagType em pttDefault. Tags de texto (TPLCString, StringType=stC)
  recebem o texto em UTF-8. Com UseSubscriptions (padrão) os valores chegam por
  subscrição (o servidor avisa as mudanças, RefreshTime do tag é o intervalo de
  amostragem); sem ela, os tags são lidos periodicamente em lotes.
}
{$ELSE}
{:
  @abstract(OPC UA (client) protocol driver, on top of the open62541 1.5 library.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It uses no CommunicationPort: the library itself opens the connection to EndpointUrl
  (@code(opc.tcp://host:4840)). libopen62541 (.so/.dll) is loaded on the first connection
  only; a program that does not use the driver does not need it.

  The address of a tag (LongAddress) is the NodeId of the variable as text:
  @unorderedList(
    @item(@code(ns=3;s="DB1"."Speed") - namespace index;)
    @item(@code(nsu=urn:server:ns;s=Name) - namespace URI (does not change when the server reorders its namespaces);)
    @item(@code(ns=2;i=1001), @code(ns=2;g=...), @code(ns=2;b=...);)
    @item(a "|" followed by an IndexRange picks elements of an array: @code(ns=3;s=Arr|4), @code(ns=3;s=Arr|0:9).)
  )

  Numeric tags (TPLCTagNumber, TPLCBlock) get the value already converted (the type comes
  from the server): keep TagType as pttDefault. Text tags (TPLCString, StringType=stC)
  get the text in UTF-8. With UseSubscriptions (the default) the values come through a
  subscription (the server reports the changes, the RefreshTime of the tag is the
  sampling interval); without it, the tags are read periodically in batches.
}
{$ENDIF}
unit OPCUADriver;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, ProtocolDriver, ProtocolTypes, Tag, PLCTag,
  PLCTagNumber, PLCBlock, PLCString, PLCStruct, TagBlock, Math, open62541, OPCUAClient, OPCUAValues;

//: Registers the Tag Builder of the OPC UA driver (the design package does it, the tool has forms).
procedure SetTagBuilderToolForOPCUAProtocol(TagBuilderTool:TOpenTagEditor);

type
  { TOPCUAItem }

  //: A node read by the driver (shared by every tag with the same address). Internal.
  TOPCUAItem = class
  public
    Serial:Cardinal;
    Path:AnsiString;
    NodeText, IndexRange:AnsiString;
    TagCount, TextTags:Integer;
    RefreshMs:Cardinal;
    NodeId:UA_NodeId;
    Resolved:Boolean;
    NextResolve:QWord;
    ValueType:PUA_DataType;
    IsArray:Boolean;
    TypeKnown:Boolean;
    Value:UA_Variant;
    HasValue:Boolean;
    LastResult:TProtocolIOResult;
    Stamp:QWord;
    Monitored:Boolean;
    MonitoredId:UA_UInt32;
    NextMonitor:QWord;
    NextPoll:QWord;
    constructor Create(const APath:AnsiString);
    destructor Destroy; override;
    procedure ClearNode;
    procedure SetResult(R:TProtocolIOResult);
    procedure SetDataValue(const DV:UA_DataValue);
  end;

  { TOPCUADriver }

  {$IFDEF PORTUGUES}
  {:
    Driver de protocolo OPC UA (cliente). Ver a unit OPCUADriver.
  }
  {$ELSE}
  {:
    OPC UA (client) protocol driver. See the OPCUADriver unit.
  }
  {$ENDIF}
  TOPCUADriver = class(TProtocolDriver)
  private
    FClient:TOPCUAClient;
    FItems:TStringList;                          //Path -> TOPCUAItem (sorted)
    FBySerial:TDictionary<Cardinal, TOPCUAItem>; //subscription context -> item
    FNextSerial:Cardinal;
    FNextConnect:QWord;
    FConnectResult:TProtocolIOResult;
    FReconnectPending:Boolean;
    FWasConnected:Boolean;
    FUseSubscriptions:Boolean;
    FSubId:UA_UInt32;
    FSubOk:Boolean;
    FSubLost:Boolean;
    FNextSubTry:QWord;
    FPublishingInterval:Cardinal;
    FReconnectInterval:Cardinal;
    FMaxItemsPerRequest:Integer;
    FOnLog:TOPCUALogEvent;
    FLibLoadFailed:Boolean;
    FLockFPU:TFPUExceptionMask;

    function GetEndpointUrl:AnsiString;
    procedure SetEndpointUrl(const AValue:AnsiString);
    function GetUsername:AnsiString;
    procedure SetUsername(const AValue:AnsiString);
    function GetPassword:AnsiString;
    procedure SetPassword(const AValue:AnsiString);
    function GetSecurityMode:TOPCUASecurityMode;
    procedure SetSecurityMode(AValue:TOPCUASecurityMode);
    function GetSecurityPolicy:TOPCUASecurityPolicy;
    procedure SetSecurityPolicy(AValue:TOPCUASecurityPolicy);
    function GetCertificateFile:String;
    procedure SetCertificateFile(const AValue:String);
    function GetPrivateKeyFile:String;
    procedure SetPrivateKeyFile(const AValue:String);
    function GetCertificateDir:String;
    procedure SetCertificateDir(const AValue:String);
    function GetTrustListDir:String;
    procedure SetTrustListDir(const AValue:String);
    function GetApplicationUri:AnsiString;
    procedure SetApplicationUri(const AValue:AnsiString);
    function GetApplicationName:AnsiString;
    procedure SetApplicationName(const AValue:AnsiString);
    function GetTimeout:Cardinal;
    procedure SetTimeout(AValue:Cardinal);
    function GetSessionTimeout:Cardinal;
    procedure SetSessionTimeout(AValue:Cardinal);
    function GetAllowUnencryptedPassword:Boolean;
    procedure SetAllowUnencryptedPassword(AValue:Boolean);
    function GetLibraryName:String;
    procedure SetLibraryName(const AValue:String);
    function GetLogLevel:TOPCUALogLevel;
    procedure SetLogLevel(AValue:TOPCUALogLevel);
    procedure SetUseSubscriptions(AValue:Boolean);
    procedure SetPublishingInterval(AValue:Cardinal);
    procedure SetMaxItemsPerRequest(AValue:Integer);
    procedure ClientLog(Sender:TObject; Level:TOPCUALogLevel; const Msg:String);
    procedure Log(Level:TOPCUALogLevel; const Msg:String);
    procedure SettingChanged;
    procedure LockDriverIO;
    procedure UnlockDriverIO;

    function TagPathOf(TagObj:TTag; out Path:AnsiString):Boolean;
    function FindItem(const Path:AnsiString):TOPCUAItem;
    function GetItem(const Path:AnsiString):TOPCUAItem;
    procedure FreeItemIfUnused(Item:TOPCUAItem);
    function ItemIsText(Item:TOPCUAItem):Boolean;

    function EnsureConnected(Now_:QWord):Boolean;
    procedure DropConnection(R:TProtocolIOResult);
    procedure CheckStatus(Status:UA_StatusCode);
    function Resolve(Item:TOPCUAItem; Now_:QWord):Boolean;
    function LearnType(Item:TOPCUAItem):TProtocolIOResult;
    procedure ManageSubscription(Now_:QWord);
    procedure PollDue(Now_:QWord; out NextDue:QWord);
    procedure ReadItems(const List:array of TOPCUAItem);
    procedure ScanReadMasked(var NeedSleep:LongInt);
    procedure MultiWriteImpl(var Writes:TMultiWriteArray; Sync:Boolean);
    function ReadImpl(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
    function WriteImpl(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
    function ValuesOf(Item:TOPCUAItem; const TagRec:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
    function BuildWrite(Item:TOPCUAItem; const tagrec:TTagRec; const Values:TArrayOfDouble;
                        out WV:UA_WriteValue; out NeedsWholeArray:Boolean):TProtocolIOResult;
    function WriteWhole(Item:TOPCUAItem; Offset:Integer; const Values:TArrayOfDouble):TProtocolIOResult;
    function WriteOne(var WV:UA_WriteValue):UA_StatusCode;
  protected
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    procedure DoDelTag(TagObj:TTag); override;
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    procedure DoMultiWrite(var Writes:TMultiWriteArray; Sync:Boolean); override;
    function DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function NeedsExternalPort:Boolean; override;
    function SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
    function SupportsListTags:Boolean; override;
    function HasTabBuilderEditor:Boolean; override;
    procedure OpenTagEditor(InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); override;
    //: DataType, ValueRank, ArrayDimensions and AccessLevel of a variable (address as in LongAddress).
    function GetVariableInfo(const NodeId:AnsiString; out Info:TOPCUAVariableInfo):TProtocolIOResult;
    {: Every variable under the Objects folder (browsed recursively, up to MaxDepth levels and
       MaxNodes variables). Path is the NodeId with the namespace URI ("nsu="). }
    function ListTags(out Tags:TPLCTagInfoArray; ForceReload:Boolean = false):TProtocolIOResult; override;
    //: The children of a node ('' = the Objects folder).
    function Browse(const NodeId:AnsiString; out Items:TOPCUABrowseItemArray):TProtocolIOResult;
    //: True while the session is open.
    function Connected:Boolean;
    //: The certificates the server offers (DER), to be saved in TrustListDir.
    function GetServerCertificates(out Certs:TStringList):TProtocolIOResult;
    //: The client certificate (DER/PEM) the server has to trust (generated on the first call when CertificateFile is empty).
    function GetClientCertificate(out Cert:TBytes):Boolean;
    //: The last connection error.
    function LastError:String;
  published
    {$IFDEF PORTUGUES}
    //: Endereço do servidor, @code(opc.tcp://host:4840).
    {$ELSE}
    //: Server address, @code(opc.tcp://host:4840).
    {$ENDIF}
    property EndpointUrl:AnsiString read GetEndpointUrl write SetEndpointUrl;
    {$IFDEF PORTUGUES}
    //: Usuário; vazio = sessão anônima.
    {$ELSE}
    //: User name; empty = anonymous session.
    {$ENDIF}
    property Username:AnsiString read GetUsername write SetUsername;
    property Password:AnsiString read GetPassword write SetPassword;
    property SecurityMode:TOPCUASecurityMode read GetSecurityMode write SetSecurityMode default osmNone;
    property SecurityPolicy:TOPCUASecurityPolicy read GetSecurityPolicy write SetSecurityPolicy default ospAuto;
    {$IFDEF PORTUGUES}
    //: Certificado do cliente (DER ou PEM). Vazio: um certificado é gerado e guardado em CertificateDir.
    {$ELSE}
    //: Client certificate (DER or PEM). Empty: one is generated and kept in CertificateDir.
    {$ENDIF}
    property CertificateFile:String read GetCertificateFile write SetCertificateFile;
    property PrivateKeyFile:String read GetPrivateKeyFile write SetPrivateKeyFile;
    property CertificateDir:String read GetCertificateDir write SetCertificateDir;
    {$IFDEF PORTUGUES}
    //: Pasta com os certificados de servidores confiáveis. Vazia: aceita qualquer servidor.
    {$ELSE}
    //: Folder with the trusted server certificates. Empty: any server is accepted.
    {$ENDIF}
    property TrustListDir:String read GetTrustListDir write SetTrustListDir;
    property ApplicationUri:AnsiString read GetApplicationUri write SetApplicationUri;
    property ApplicationName:AnsiString read GetApplicationName write SetApplicationName;
    property Timeout:Cardinal read GetTimeout write SetTimeout default 5000;
    property SessionTimeout:Cardinal read GetSessionTimeout write SetSessionTimeout default 60000;
    property AllowUnencryptedPassword:Boolean read GetAllowUnencryptedPassword write SetAllowUnencryptedPassword default false;
    property ReconnectInterval:Cardinal read FReconnectInterval write FReconnectInterval default 3000;
    property UseSubscriptions:Boolean read FUseSubscriptions write SetUseSubscriptions default true;
    property PublishingInterval:Cardinal read FPublishingInterval write SetPublishingInterval default 100;
    property MaxItemsPerRequest:Integer read FMaxItemsPerRequest write SetMaxItemsPerRequest default 500;
    {$IFDEF PORTUGUES}
    //: Nome/caminho da libopen62541; vazio usa o padrão do sistema.
    {$ELSE}
    //: Name/path of libopen62541; empty uses the system default.
    {$ENDIF}
    property LibraryName:String read GetLibraryName write SetLibraryName;
    property LogLevel:TOPCUALogLevel read GetLogLevel write SetLogLevel default ollWarning;
    //: Messages of the driver and of the library. Called from the driver threads.
    property OnLog:TOPCUALogEvent read FOnLog write FOnLog;
    property ReadOnly;
  end;

implementation

uses
  CrossEvent, pascalScadaMTPCPU, pscadalog;

const
  OPCUAToPSLogLevel:array[TOPCUALogLevel] of TpSCADALogLevel =
    (llTrace, llDebug, llInfo, llWarning, llError, llError, llError);

type
  TPLCTagAccess = class(TPLCTag);

var
  OPCUATagBuilderEditor:TOpenTagEditor = nil;

procedure SetTagBuilderToolForOPCUAProtocol(TagBuilderTool:TOpenTagEditor);
begin
  if Assigned(OPCUATagBuilderEditor) then
    raise Exception.Create('A Tag Builder editor for the OPC UA protocol was already assigned.');
  OPCUATagBuilderEditor := TagBuilderTool;
end;

const
  ResolveRetryMs = 5000;

{ TOPCUAItem }

constructor TOPCUAItem.Create(const APath:AnsiString);
begin
  inherited Create;
  Path := APath;
  OPCUASplitAddress(APath, NodeText, IndexRange);
  FillChar(NodeId, SizeOf(NodeId), 0);
  FillChar(Value, SizeOf(Value), 0);
  LastResult := ioNone;
  RefreshMs := High(Cardinal);
end;

destructor TOPCUAItem.Destroy;
begin
  ClearNode;
  if HasValue then
    UA_Variant_clear(Value);
  inherited Destroy;
end;

procedure TOPCUAItem.ClearNode;
begin
  if Resolved then
    UA_NodeId_clear(NodeId);
  Resolved := false;
  Monitored := false;
  MonitoredId := 0;
end;

procedure TOPCUAItem.SetResult(R:TProtocolIOResult);
begin
  if (R<>LastResult) or (R<>ioOk) then begin
    LastResult := R;
    Stamp := GetTickCount64;
  end;
end;

procedure TOPCUAItem.SetDataValue(const DV:UA_DataValue);
var
  st:UA_StatusCode;
begin
  st := UA_STATUSCODE_GOOD;
  if (DV.flags and UA_DATAVALUE_HASSTATUS)<>0 then
    st := DV.status;
  if UA_StatusCode_isBad(st) then begin
    LastResult := OPCUAStatusToIOResult(st);
    Stamp := GetTickCount64;
    exit;
  end;
  if (DV.flags and UA_DATAVALUE_HASVALUE)=0 then begin
    LastResult := ioEmptyPacket;
    Stamp := GetTickCount64;
    exit;
  end;
  if HasValue then
    UA_Variant_clear(Value);
  UA_copy(@DV.value, @Value, @UA_TYPES[UA_TYPES_VARIANT]);
  HasValue := true;
  if Value._type<>nil then begin
    ValueType := Value._type;
    IsArray := not UA_Variant_isScalar(@Value);
    TypeKnown := true;
  end;
  LastResult := ioOk;
  Stamp := GetTickCount64;
end;

{ subscription callbacks (run inside UA_Client_run_iterate/services, i.e. under the driver locks) }

function DriverOf(client:PUA_Client):TOPCUADriver;
begin
  Result := TOPCUADriver(UA_Client_getConfig(client)^.clientContext);
end;

procedure DataChangeCB(client:PUA_Client; subId:UA_UInt32; subContext:Pointer; monId:UA_UInt32;
                       monContext:Pointer; value:PUA_DataValue); cdecl;
var
  drv:TOPCUADriver;
  item:TOPCUAItem;
begin
  drv := TOPCUADriver(subContext);
  if (drv=nil) or (value=nil) then exit;
  //no exception may cross the C library: it would leave its event loop marked as running
  try
    if drv.FBySerial.TryGetValue(Cardinal(PtrUInt(monContext)), item) then
      item.SetDataValue(value^);
  except
    on e:Exception do
      drv.Log(ollError, 'OPC UA: data change callback: '+e.ClassName+': '+e.Message);
  end;
end;

procedure SubDeletedCB(client:PUA_Client; subId:UA_UInt32; subContext:Pointer); cdecl;
var
  drv:TOPCUADriver;
begin
  drv := TOPCUADriver(subContext);
  if (drv<>nil) and (drv.FSubId=subId) then
    drv.FSubLost := true;
end;

procedure SubInactiveCB(client:PUA_Client; subscriptionId:UA_UInt32; subContext:Pointer); cdecl;
var
  drv:TOPCUADriver;
begin
  drv := TOPCUADriver(subContext);
  if drv=nil then drv := DriverOf(client);
  if drv<>nil then
    drv.FSubLost := true;
end;

{ TOPCUADriver }

constructor TOPCUADriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FClient := TOPCUAClient.Create;
  FClient.OnLog := ClientLog;
  FItems := TStringList.Create;
  FItems.Sorted := true;
  FItems.CaseSensitive := true;
  FItems.Duplicates := dupError;
  FBySerial := TDictionary<Cardinal, TOPCUAItem>.Create;
  FNextSerial := 1;
  FUseSubscriptions := true;
  FPublishingInterval := 100;
  FReconnectInterval := 3000;
  FMaxItemsPerRequest := 500;
  FConnectResult := ioConnectPLCFailed;
  PReadSomethingAlways := true;
  PDeliverReadAhead := true; //values arrive (subscription) whenever they change: hand them over
end;

destructor TOPCUADriver.Destroy;
var
  i:Integer;
begin
  inherited Destroy; //stops the scan threads first
  if FClient<>nil then
    FClient.Disconnect;
  if FItems<>nil then
    for i:=0 to FItems.Count-1 do
      FItems.Objects[i].Free;
  FreeAndNil(FItems);
  FreeAndNil(FBySerial);
  FreeAndNil(FClient); //unloads the library: after the items, which hold library memory
end;

function TOPCUADriver.NeedsExternalPort:Boolean;
begin
  Result := false;
end;

function TOPCUADriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  if aTag is TPLCString then begin
    //text: one byte per value
    ProtocolTagType := ptByte;
    exit(8);
  end;
  //numbers: the values are handed already converted (ptUnknown = no conversion), one
  //per element whatever the TagType - the word size follows the TagType so that the tag
  //asks for exactly one value per element
  ProtocolTagType := ptUnknown;
  Result := 64;
  if aTag is TPLCTag then
    case TPLCTagAccess(aTag).TagType of
      pttShortInt, pttByte:          Result := 8;
      pttSmallInt, pttWord:          Result := 16;
      pttLongInt, pttDWord, pttFloat: Result := 32;
      else                           Result := 64;
    end;
end;

function TOPCUADriver.SupportsListTags:Boolean;
begin
  Result := true;
end;

function TOPCUADriver.HasTabBuilderEditor:Boolean;
begin
  Result := true;
end;

procedure TOPCUADriver.OpenTagEditor(InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
begin
  if Assigned(OPCUATagBuilderEditor) then
    OPCUATagBuilderEditor(Self, Self.Owner, InsertHook, CreateProc)
  else
    inherited OpenTagEditor(InsertHook, CreateProc);
end;

function TOPCUADriver.GetVariableInfo(const NodeId:AnsiString; out Info:TOPCUAVariableInfo):TProtocolIOResult;
var
  id:UA_NodeId;
  st:UA_StatusCode;
  nodeText, range:AnsiString;
begin
  FillChar(Info, SizeOf(Info), 0);
  LockDriverIO;
  try
    if not EnsureConnected(GetTickCount64) then exit(FConnectResult);
    OPCUASplitAddress(NodeId, nodeText, range);
    st := FClient.ParseNodeId(nodeText, id);
    if st<>UA_STATUSCODE_GOOD then exit(OPCUAStatusToIOResult(st));
    try
      st := FClient.ReadVariableInfo(id, Info);
    finally
      UA_NodeId_clear(id);
    end;
    CheckStatus(st);
    Result := OPCUAStatusToIOResult(st);
  finally
    UnlockDriverIO;
  end;
end;

{ settings }

procedure TOPCUADriver.SettingChanged;
begin
  //takes effect on the next connection; an open session is closed by the scan thread
  FReconnectPending := FClient.Handle<>nil;
  FNextConnect := 0;
  FLibLoadFailed := false;
end;

function TOPCUADriver.GetEndpointUrl:AnsiString; begin Result := FClient.EndpointUrl; end;
procedure TOPCUADriver.SetEndpointUrl(const AValue:AnsiString); begin if AValue=FClient.EndpointUrl then exit; FClient.EndpointUrl := AValue; SettingChanged; end;
function TOPCUADriver.GetUsername:AnsiString; begin Result := FClient.Username; end;
procedure TOPCUADriver.SetUsername(const AValue:AnsiString); begin if AValue=FClient.Username then exit; FClient.Username := AValue; SettingChanged; end;
function TOPCUADriver.GetPassword:AnsiString; begin Result := FClient.Password; end;
procedure TOPCUADriver.SetPassword(const AValue:AnsiString); begin if AValue=FClient.Password then exit; FClient.Password := AValue; SettingChanged; end;
function TOPCUADriver.GetSecurityMode:TOPCUASecurityMode; begin Result := FClient.SecurityMode; end;
procedure TOPCUADriver.SetSecurityMode(AValue:TOPCUASecurityMode); begin if AValue=FClient.SecurityMode then exit; FClient.SecurityMode := AValue; SettingChanged; end;
function TOPCUADriver.GetSecurityPolicy:TOPCUASecurityPolicy; begin Result := FClient.SecurityPolicy; end;
procedure TOPCUADriver.SetSecurityPolicy(AValue:TOPCUASecurityPolicy); begin if AValue=FClient.SecurityPolicy then exit; FClient.SecurityPolicy := AValue; SettingChanged; end;
function TOPCUADriver.GetCertificateFile:String; begin Result := FClient.CertificateFile; end;
procedure TOPCUADriver.SetCertificateFile(const AValue:String); begin if AValue=FClient.CertificateFile then exit; FClient.CertificateFile := AValue; SettingChanged; end;
function TOPCUADriver.GetPrivateKeyFile:String; begin Result := FClient.PrivateKeyFile; end;
procedure TOPCUADriver.SetPrivateKeyFile(const AValue:String); begin if AValue=FClient.PrivateKeyFile then exit; FClient.PrivateKeyFile := AValue; SettingChanged; end;
function TOPCUADriver.GetCertificateDir:String; begin Result := FClient.CertificateDir; end;
procedure TOPCUADriver.SetCertificateDir(const AValue:String); begin if AValue=FClient.CertificateDir then exit; FClient.CertificateDir := AValue; SettingChanged; end;
function TOPCUADriver.GetTrustListDir:String; begin Result := FClient.TrustListDir; end;
procedure TOPCUADriver.SetTrustListDir(const AValue:String); begin if AValue=FClient.TrustListDir then exit; FClient.TrustListDir := AValue; SettingChanged; end;
function TOPCUADriver.GetApplicationUri:AnsiString; begin Result := FClient.ApplicationUri; end;
procedure TOPCUADriver.SetApplicationUri(const AValue:AnsiString); begin if AValue=FClient.ApplicationUri then exit; FClient.ApplicationUri := AValue; SettingChanged; end;
function TOPCUADriver.GetApplicationName:AnsiString; begin Result := FClient.ApplicationName; end;
procedure TOPCUADriver.SetApplicationName(const AValue:AnsiString); begin if AValue=FClient.ApplicationName then exit; FClient.ApplicationName := AValue; SettingChanged; end;
function TOPCUADriver.GetTimeout:Cardinal; begin Result := FClient.Timeout; end;
procedure TOPCUADriver.SetTimeout(AValue:Cardinal); begin if AValue=FClient.Timeout then exit; FClient.Timeout := AValue; SettingChanged; end;
function TOPCUADriver.GetSessionTimeout:Cardinal; begin Result := FClient.SessionTimeout; end;
procedure TOPCUADriver.SetSessionTimeout(AValue:Cardinal); begin if AValue=FClient.SessionTimeout then exit; FClient.SessionTimeout := AValue; SettingChanged; end;
function TOPCUADriver.GetAllowUnencryptedPassword:Boolean; begin Result := FClient.AllowUnencryptedPassword; end;
procedure TOPCUADriver.SetAllowUnencryptedPassword(AValue:Boolean); begin if AValue=FClient.AllowUnencryptedPassword then exit; FClient.AllowUnencryptedPassword := AValue; SettingChanged; end;
function TOPCUADriver.GetLibraryName:String; begin Result := FClient.LibraryName; end;
procedure TOPCUADriver.SetLibraryName(const AValue:String); begin FClient.LibraryName := AValue; FLibLoadFailed := false; end;
function TOPCUADriver.GetLogLevel:TOPCUALogLevel; begin Result := FClient.LogLevel; end;
procedure TOPCUADriver.SetLogLevel(AValue:TOPCUALogLevel); begin FClient.LogLevel := AValue; end;

procedure TOPCUADriver.SetUseSubscriptions(AValue:Boolean);
begin
  if FUseSubscriptions=AValue then exit;
  FUseSubscriptions := AValue;
  SettingChanged;
end;

procedure TOPCUADriver.SetPublishingInterval(AValue:Cardinal);
begin
  if AValue<10 then AValue := 10;
  if FPublishingInterval=AValue then exit;
  FPublishingInterval := AValue;
  SettingChanged;
end;

procedure TOPCUADriver.SetMaxItemsPerRequest(AValue:Integer);
begin
  FMaxItemsPerRequest := Max(1, AValue);
end;

procedure TOPCUADriver.ClientLog(Sender:TObject; Level:TOPCUALogLevel; const Msg:String);
begin
  //the client (open62541) already left out what is below LogLevel
  if (Level<>ollNone) and PSLogWants(lcProtocol, OPCUAToPSLogLevel[Level]) then
    PSLog(Self, lcProtocol, OPCUAToPSLogLevel[Level], Msg);
  if Assigned(FOnLog) then
    FOnLog(Self, Level, Msg);
end;

procedure TOPCUADriver.Log(Level:TOPCUALogLevel; const Msg:String);
begin
  if Level<FClient.LogLevel then exit;
  if (Level<>ollNone) and PSLogWants(lcProtocol, OPCUAToPSLogLevel[Level]) then
    PSLog(Self, lcProtocol, OPCUAToPSLogLevel[Level], Msg);
  if Assigned(FOnLog) then
    try
      FOnLog(Self, Level, Msg);
    except
    end;
end;

procedure TOPCUADriver.LockDriverIO;
begin
  while not FPause.ResetEvent do
    CrossThreadSwitch;
  FWriteCS.Enter;
  FReadCS.Enter;
  FLockFPU := OPCUAMaskFPU; //see OPCUAMaskFPU (calls from the main thread, e.g. at design time)
end;

procedure TOPCUADriver.UnlockDriverIO;
begin
  OPCUARestoreFPU(FLockFPU);
  FReadCS.Leave;
  FWriteCS.Leave;
  FPause.SetEvent;
end;

function TOPCUADriver.LastError:String;
begin
  Result := FClient.LastError;
end;

function TOPCUADriver.Connected:Boolean;
begin
  LockDriverIO;
  try
    Result := FClient.Connected;
  finally
    UnlockDriverIO;
  end;
end;

{ items }

function TOPCUADriver.TagPathOf(TagObj:TTag; out Path:AnsiString):Boolean;
begin
  Path := '';
  if TagObj is TPLCStruct then exit(false); //byte structures have no OPC UA equivalent
  if TagObj is TPLCTagNumber then
    Path := TPLCTagNumber(TagObj).LongAddress
  else if TagObj is TPLCBlock then
    Path := TPLCBlock(TagObj).LongAddress
  else if TagObj is TPLCString then
    Path := TPLCString(TagObj).LongAddress;
  Path := Trim(Path);
  Result := Path<>'';
end;

function TOPCUADriver.FindItem(const Path:AnsiString):TOPCUAItem;
var
  i:Integer;
begin
  if FItems.Find(Trim(Path), i) then
    Result := TOPCUAItem(FItems.Objects[i])
  else
    Result := nil;
end;

function TOPCUADriver.GetItem(const Path:AnsiString):TOPCUAItem;
begin
  Result := FindItem(Path);
  if Result=nil then begin
    Result := TOPCUAItem.Create(Trim(Path));
    Result.Serial := FNextSerial;
    Inc(FNextSerial);
    FItems.AddObject(Result.Path, Result);
    FBySerial.Add(Result.Serial, Result);
  end;
end;

procedure TOPCUADriver.FreeItemIfUnused(Item:TOPCUAItem);
var
  i:Integer;
begin
  if (Item=nil) or (Item.TagCount>0) then exit;
  if Item.Monitored and FSubOk and (FClient.Handle<>nil) then
    UA_Client_MonitoredItems_deleteSingle(FClient.Handle, FSubId, Item.MonitoredId);
  FBySerial.Remove(Item.Serial);
  if FItems.Find(Item.Path, i) then
    FItems.Delete(i);
  Item.Free;
end;

function TOPCUADriver.ItemIsText(Item:TOPCUAItem):Boolean;
begin
  if Item.TypeKnown and OPCUAIsTextType(Item.ValueType) then exit(true);
  Result := (Item.TextTags>0) and (Item.TextTags=Item.TagCount);
end;

procedure TOPCUADriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
var
  Path:AnsiString;
  item:TOPCUAItem;
  tr:TTagRec;
  ok:Boolean;
begin
  ok := TagPathOf(TagObj, Path);
  if ok then begin
    item := GetItem(Path);
    Inc(item.TagCount);
    if TagObj is TPLCString then
      Inc(item.TextTags);
    if Supports(TagObj, IScanableTagInterface) then begin
      (TagObj as IScanableTagInterface).BuildTagRec(tr, 0, 0);
      if Cardinal(tr.UpdateTime)<item.RefreshMs then begin
        item.RefreshMs := tr.UpdateTime;
        //a faster tag on a monitored node: monitor it again with the new interval
        if item.Monitored and FSubOk and (FClient.Handle<>nil) then begin
          UA_Client_MonitoredItems_deleteSingle(FClient.Handle, FSubId, item.MonitoredId);
          item.Monitored := false;
        end;
      end;
    end;
    item.NextPoll := 0;
  end;
  inherited DoAddTag(TagObj, ok);
end;

procedure TOPCUADriver.DoDelTag(TagObj:TTag);
var
  Path:AnsiString;
  item:TOPCUAItem;
begin
  if TagPathOf(TagObj, Path) then begin
    item := FindItem(Path);
    if item<>nil then begin
      Dec(item.TagCount);
      if TagObj is TPLCString then
        Dec(item.TextTags);
      FreeItemIfUnused(item);
    end;
  end;
  inherited DoDelTag(TagObj);
end;

{ connection }

procedure TOPCUADriver.DropConnection(R:TProtocolIOResult);
var
  i:Integer;
  item:TOPCUAItem;
begin
  FClient.Disconnect;
  FSubOk := false;
  FSubLost := false;
  FSubId := 0;
  for i:=0 to FItems.Count-1 do begin
    item := TOPCUAItem(FItems.Objects[i]);
    item.ClearNode; //namespace indexes may change when the server restarts
    item.NextMonitor := 0;
    item.NextPoll := 0;
    if item.TagCount>0 then
      item.SetResult(R);
  end;
  if FWasConnected then
    Log(ollWarning, 'OPC UA: connection to '+EndpointUrl+' lost');
  FWasConnected := false;
end;

procedure TOPCUADriver.CheckStatus(Status:UA_StatusCode);
begin
  if OPCUAIsConnectionLost(Status and $FFFF0000) then
    DropConnection(ioCommError);
end;

function TOPCUADriver.EnsureConnected(Now_:QWord):Boolean;
var
  st:UA_StatusCode;
  i:Integer;
begin
  if FReconnectPending then begin
    FReconnectPending := false;
    if FClient.Handle<>nil then
      DropConnection(ioCommError);
  end;
  if FClient.Handle<>nil then begin
    if FClient.Connected then exit(true);
    DropConnection(ioCommError);
  end;
  if (Now_<FNextConnect) or (Trim(EndpointUrl)='') then exit(false);
  if FLibLoadFailed then exit(false);
  if not FClient.LoadLibrary then begin
    FLibLoadFailed := true;
    FConnectResult := ioDriverError;
    Log(ollError, 'OPC UA: '+FClient.LastError);
    for i:=0 to FItems.Count-1 do
      if TOPCUAItem(FItems.Objects[i]).TagCount>0 then
        TOPCUAItem(FItems.Objects[i]).SetResult(ioDriverError);
    exit(false);
  end;
  st := FClient.Connect;
  if st<>UA_STATUSCODE_GOOD then begin
    FNextConnect := GetTickCount64+FReconnectInterval;
    FConnectResult := OPCUAStatusToIOResult(st);
    //a session refused for the user (at activation) is a login problem, not a node access one
    if (st and $FFFF0000)=UA_STATUSCODE_BADUSERACCESSDENIED then
      FConnectResult := ioAuthError
    else if FConnectResult in [ioCommError, ioTimeOut, ioPLCError, ioIllegalRequest, ioObjectAccessNotAllowed] then
      FConnectResult := ioConnectPLCFailed;
    Log(ollError, 'OPC UA: '+FClient.LastError);
    for i:=0 to FItems.Count-1 do
      if TOPCUAItem(FItems.Objects[i]).TagCount>0 then
        TOPCUAItem(FItems.Objects[i]).SetResult(FConnectResult);
    exit(false);
  end;
  //we reconnect ourselves (the subscriptions are created again)
  UA_Client_getConfig(FClient.Handle)^.clientContext := Self;
  UA_Client_getConfig(FClient.Handle)^.noReconnect := true;
  UA_Client_getConfig(FClient.Handle)^.subscriptionInactivityCallback := @SubInactiveCB;
  FWasConnected := true;
  FSubOk := false;
  FSubLost := false;
  FNextSubTry := 0;
  Log(ollInfo, 'OPC UA: connected to '+EndpointUrl);
  Result := true;
end;

function TOPCUADriver.Resolve(Item:TOPCUAItem; Now_:QWord):Boolean;
var
  st:UA_StatusCode;
begin
  if Item.Resolved then exit(true);
  if Now_<Item.NextResolve then exit(false);
  st := FClient.ParseNodeId(Item.NodeText, Item.NodeId);
  if st<>UA_STATUSCODE_GOOD then begin
    FillChar(Item.NodeId, SizeOf(Item.NodeId), 0);
    Item.NextResolve := Now_+ResolveRetryMs;
    Item.SetResult(ioObjectNotExists);
    Log(ollWarning, 'OPC UA: invalid address "'+Item.Path+'": '+OPCUAStatusText(st));
    exit(false);
  end;
  Item.Resolved := true;
  Result := true;
end;

function TOPCUADriver.LearnType(Item:TOPCUAItem):TProtocolIOResult;
var
  info:TOPCUAVariableInfo;
  st:UA_StatusCode;
begin
  if Item.TypeKnown then exit(ioOk);
  //the value tells the actual (builtin) type and whether it is an array
  ReadItems([Item]);
  if Item.TypeKnown then exit(ioOk);
  //no value (e.g. an empty string or array): the attributes of the node
  st := FClient.ReadVariableInfo(Item.NodeId, info);
  CheckStatus(st);
  if UA_StatusCode_isBad(st) then exit(OPCUAStatusToIOResult(st));
  if info.BuiltinType=nil then exit(ioIllegalValue);
  Item.ValueType := info.BuiltinType;
  Item.IsArray := info.ValueRank>=0;
  Item.TypeKnown := true;
  Result := ioOk;
end;

{ reading }

procedure TOPCUADriver.ReadItems(const List:array of TOPCUAItem);
var
  ids:array of UA_ReadValueId;
  req:UA_ReadRequest;
  resp:UA_ReadResponse;
  i, n:Integer;
  st:UA_StatusCode;
  ranges:array of AnsiString;
begin
  n := Length(List);
  if (n=0) or (FClient.Handle=nil) then exit;
  SetLength(ids, n);
  SetLength(ranges, n);
  FillChar(ids[0], n*SizeOf(UA_ReadValueId), 0);
  for i:=0 to n-1 do begin
    ids[i].nodeId := List[i].NodeId;
    ids[i].attributeId := Ord(UA_ATTRIBUTEID_VALUE);
    ranges[i] := List[i].IndexRange;
    ids[i].indexRange := _UA_STRING(ranges[i]);
  end;
  UA_ReadRequest_init(req);
  req.nodesToRead := @ids[0];
  req.nodesToReadSize := n;
  req.timestampsToReturn := UA_TIMESTAMPSTORETURN_NEITHER;
  resp := UA_Client_Service_read(FClient.Handle, req);
  st := resp.responseHeader.serviceResult;
  if (st=UA_STATUSCODE_GOOD) and (resp.resultsSize=n) then begin
    for i:=0 to n-1 do
      List[i].SetDataValue(resp.results[i]);
  end else begin
    if st=UA_STATUSCODE_GOOD then st := UA_STATUSCODE_BADUNEXPECTEDERROR;
    for i:=0 to n-1 do
      List[i].SetResult(OPCUAStatusToIOResult(st));
  end;
  UA_ReadResponse_clear(resp);
  CheckStatus(st);
end;

procedure TOPCUADriver.PollDue(Now_:QWord; out NextDue:QWord);
var
  batch:array of TOPCUAItem;
  i, n:Integer;
  item:TOPCUAItem;
  interval:QWord;
begin
  NextDue := Now_+100;
  SetLength(batch, 0);
  n := 0;
  for i:=0 to FItems.Count-1 do begin
    item := TOPCUAItem(FItems.Objects[i]);
    if (item.TagCount<=0) or item.Monitored or (not item.Resolved) then continue;
    if Now_>=item.NextPoll then begin
      if n>=Length(batch) then SetLength(batch, n+64);
      batch[n] := item;
      Inc(n);
      if item.RefreshMs=0 then interval := 100 else interval := item.RefreshMs;
      item.NextPoll := Now_+interval;
      if n>=FMaxItemsPerRequest then begin
        ReadItems(Copy(batch, 0, n));
        n := 0;
        if FClient.Handle=nil then exit;
      end;
    end;
    if item.NextPoll<NextDue then NextDue := item.NextPoll;
  end;
  if n>0 then
    ReadItems(Copy(batch, 0, n));
end;

procedure TOPCUADriver.ManageSubscription(Now_:QWord);
var
  req:UA_CreateSubscriptionRequest;
  resp:UA_CreateSubscriptionResponse;
  pending:array of TOPCUAItem;
  i, j, n, chunk:Integer;
  item:TOPCUAItem;
  mreq:UA_CreateMonitoredItemsRequest;
  items:array of UA_MonitoredItemCreateRequest;
  ctx:array of Pointer;
  cbs:array of UA_Client_DataChangeNotificationCallback;
  dcbs:array of UA_Client_DeleteMonitoredItemCallback;
  mresp:UA_CreateMonitoredItemsResponse;
  st:UA_StatusCode;
  ranges:array of AnsiString;
begin
  if FSubLost then begin
    FSubLost := false;
    FSubOk := false;
    Log(ollWarning, 'OPC UA: subscription lost, creating it again');
    if FClient.Handle<>nil then
      UA_Client_Subscriptions_deleteSingle(FClient.Handle, FSubId);
    for i:=0 to FItems.Count-1 do begin
      TOPCUAItem(FItems.Objects[i]).Monitored := false;
      TOPCUAItem(FItems.Objects[i]).NextMonitor := 0;
    end;
  end;

  if not FSubOk then begin
    if Now_<FNextSubTry then exit;
    req := UA_CreateSubscriptionRequest_default();
    req.requestedPublishingInterval := FPublishingInterval;
    req.requestedMaxKeepAliveCount := 10;
    req.requestedLifetimeCount := 100;
      resp := UA_Client_Subscriptions_create(FClient.Handle, req, Self, nil, @SubDeletedCB);
    st := resp.responseHeader.serviceResult;
    if st=UA_STATUSCODE_GOOD then begin
      FSubId := resp.subscriptionId;
      FSubOk := true;
    end else begin
      FNextSubTry := Now_+FReconnectInterval;
      Log(ollWarning, 'OPC UA: the subscription could not be created ('+OPCUAStatusText(st)+'): the tags are polled');
    end;
    UA_clear(@resp, @UA_TYPES[UA_TYPES_CREATESUBSCRIPTIONRESPONSE]);
    CheckStatus(st);
    if not FSubOk then exit;
  end;

  //items still not monitored
  n := 0;
  SetLength(pending, 0);
  for i:=0 to FItems.Count-1 do begin
    item := TOPCUAItem(FItems.Objects[i]);
    if (item.TagCount>0) and item.Resolved and (not item.Monitored) and (Now_>=item.NextMonitor) then begin
      SetLength(pending, n+1);
      pending[n] := item;
      Inc(n);
    end;
  end;

  i := 0;
  while (i<n) and FSubOk and (FClient.Handle<>nil) do begin
    chunk := Min(FMaxItemsPerRequest, n-i);
    SetLength(items, chunk);
    SetLength(ctx, chunk);
    SetLength(cbs, chunk);
    SetLength(dcbs, chunk);
    SetLength(ranges, chunk);
    for j:=0 to chunk-1 do begin
      item := pending[i+j];
      items[j] := UA_MonitoredItemCreateRequest_default(item.NodeId);
      ranges[j] := item.IndexRange;
      items[j].itemToMonitor.indexRange := _UA_STRING(ranges[j]);
      if item.RefreshMs=High(Cardinal) then
        items[j].requestedParameters.samplingInterval := -1 //the publishing interval
      else
        items[j].requestedParameters.samplingInterval := item.RefreshMs;
      items[j].requestedParameters.queueSize := 1;
      items[j].requestedParameters.discardOldest := true;
      ctx[j] := Pointer(PtrUInt(item.Serial));
      cbs[j] := @DataChangeCB;
      dcbs[j] := nil;
    end;
    FillChar(mreq, SizeOf(mreq), 0);
    mreq.subscriptionId := FSubId;
    mreq.timestampsToReturn := UA_TIMESTAMPSTORETURN_NEITHER;
    mreq.itemsToCreate := @items[0];
    mreq.itemsToCreateSize := chunk;
    //Pointer(array), not @array[0]: in Delphi mode "@" on a procedure variable is its value
    //(the address of the routine), not the address of the element
    mresp := UA_Client_MonitoredItems_createDataChanges(FClient.Handle, mreq, Pointer(ctx), Pointer(cbs), Pointer(dcbs));
    st := mresp.responseHeader.serviceResult;
    if (st=UA_STATUSCODE_GOOD) and (mresp.resultsSize=chunk) then begin
      for j:=0 to chunk-1 do begin
        item := pending[i+j];
        if UA_StatusCode_isBad(mresp.results[j].statusCode) then begin
          item.NextMonitor := Now_+ResolveRetryMs;
          item.SetResult(OPCUAStatusToIOResult(mresp.results[j].statusCode));
        end else begin
          item.Monitored := true;
          item.MonitoredId := mresp.results[j].monitoredItemId;
        end;
      end;
    end else begin
      for j:=0 to chunk-1 do
        pending[i+j].NextMonitor := Now_+ResolveRetryMs;
      Log(ollWarning, 'OPC UA: monitored items could not be created: '+OPCUAStatusText(st));
    end;
    UA_clear(@mresp, @UA_TYPES[UA_TYPES_CREATEMONITOREDITEMSRESPONSE]);
    CheckStatus(st);
    Inc(i, chunk);
  end;
end;

procedure TOPCUADriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
var
  fpu:TFPUExceptionMask;
begin
  fpu := OPCUAMaskFPU;
  try
    try
      ScanReadMasked(NeedSleep);
    except
      on e:Exception do begin
        Log(ollError, 'OPC UA: scan: '+e.ClassName+': '+e.Message);
        NeedSleep := 100;
      end;
    end;
  finally
    OPCUARestoreFPU(fpu);
  end;
end;

procedure TOPCUADriver.ScanReadMasked(var NeedSleep:LongInt);
var
  now_, nextDue:QWord;
  i, used:Integer;
  st:UA_StatusCode;
begin
  used := 0;
  for i:=0 to FItems.Count-1 do
    if TOPCUAItem(FItems.Objects[i]).TagCount>0 then
      Inc(used);
  if used=0 then begin
    if FReconnectPending and (FClient.Handle<>nil) then begin
      FReconnectPending := false;
      DropConnection(ioCommError);
    end;
    NeedSleep := 50;
    exit;
  end;

  now_ := GetTickCount64;
  if not EnsureConnected(now_) then begin
    NeedSleep := 50;
    exit;
  end;

  for i:=0 to FItems.Count-1 do
    if TOPCUAItem(FItems.Objects[i]).TagCount>0 then
      Resolve(TOPCUAItem(FItems.Objects[i]), now_);

  if FUseSubscriptions then begin
    ManageSubscription(now_);
    if FClient.Handle=nil then begin
      NeedSleep := 50;
      exit;
    end;
  end;

  //whatever is not monitored (subscriptions off, or refused) is polled
  PollDue(now_, nextDue);
  if FClient.Handle=nil then begin
    NeedSleep := 50;
    exit;
  end;

  //notifications (and keep alive of the session)
  st := FClient.Iterate(0);
  if UA_StatusCode_isBad(st) or (not FClient.Connected) then begin
    DropConnection(ioCommError);
    NeedSleep := 50;
    exit;
  end;

  now_ := GetTickCount64;
  if nextDue>now_ then
    NeedSleep := Max(1, Min(10, nextDue-now_))
  else
    NeedSleep := 1;
end;

function TOPCUADriver.ValuesOf(Item:TOPCUAItem; const TagRec:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
var
  all:TArrayOfDouble;
  txt:UTF8String;
  i, n, off:Integer;
begin
  SetLength(Values, 0);
  if not Item.HasValue then exit(ioEmptyPacket);
  if ItemIsText(Item) then begin
    Result := OPCUAVariantToText(Item.Value, txt);
    if Result=ioOk then
      Values := OPCUATextToValues(txt, TagRec.Size);
    exit;
  end;
  Result := OPCUAVariantToNumbers(Item.Value, all);
  if Result<>ioOk then exit;
  n := TagRec.Size;
  if n<=0 then n := Length(all);
  off := Max(0, TagRec.OffSet);
  SetLength(Values, n);
  for i:=0 to n-1 do
    if off+i<Length(all) then
      Values[i] := all[off+i]
    else
      Values[i] := 0;
end;

procedure TOPCUADriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
var
  item:TOPCUAItem;
  r:TProtocolIOResult;
begin
  SetLength(values.Values, 0);
  item := FindItem(TagRec.Path);
  if item=nil then begin
    values.LastQueryResult := ioNullDriver;
    exit;
  end;
  values.ClkMonotonicTStamp := item.Stamp;
  values.Offset := 0;
  values.RealOffset := 0;
  if (item.LastResult=ioOk) then begin
    r := ValuesOf(item, TagRec, values.Values);
    values.LastQueryResult := r;
    if r=ioOk then begin
      values.ReadsOK := 1;
      values.ReadFaults := 0;
    end else begin
      values.ReadsOK := 0;
      values.ReadFaults := 1;
    end;
  end else begin
    values.LastQueryResult := item.LastResult;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
  end;
end;

function TOPCUADriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  fpu:TFPUExceptionMask;
begin
  fpu := OPCUAMaskFPU;
  try
    Result := ReadImpl(tagrec, Values, Sync);
  finally
    OPCUARestoreFPU(fpu);
  end;
end;

function TOPCUADriver.ReadImpl(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  item:TOPCUAItem;
begin
  SetLength(Values, 0);
  if not EnsureConnected(GetTickCount64) then exit(FConnectResult);
  item := GetItem(tagrec.Path);
  try
    if not Resolve(item, 0) then exit(item.LastResult);
    ReadItems([item]);
    if item.LastResult<>ioOk then exit(item.LastResult);
    Result := ValuesOf(item, tagrec, Values);
  finally
    FreeItemIfUnused(item);
  end;
end;

{ writing }

function TOPCUADriver.WriteOne(var WV:UA_WriteValue):UA_StatusCode;
var
  req:UA_WriteRequest;
  resp:UA_WriteResponse;
begin
  UA_WriteRequest_init(req);
  req.nodesToWrite := @WV;
  req.nodesToWriteSize := 1;
  resp := UA_Client_Service_write(FClient.Handle, req);
  Result := resp.responseHeader.serviceResult;
  if (Result=UA_STATUSCODE_GOOD) then begin
    if resp.resultsSize=1 then
      Result := resp.results[0]
    else
      Result := UA_STATUSCODE_BADUNEXPECTEDERROR;
  end;
  UA_WriteResponse_clear(resp);
  CheckStatus(Result);
end;

function TOPCUADriver.BuildWrite(Item:TOPCUAItem; const tagrec:TTagRec; const Values:TArrayOfDouble;
                                 out WV:UA_WriteValue; out NeedsWholeArray:Boolean):TProtocolIOResult;
var
  range:AnsiString;
  n, off:Integer;
begin
  FillChar(WV, SizeOf(WV), 0);
  NeedsWholeArray := false;
  WV.nodeId := Item.NodeId; //not owned: not cleared
  WV.attributeId := Ord(UA_ATTRIBUTEID_VALUE);
  WV.value.flags := UA_DATAVALUE_HASVALUE;
  if ItemIsText(Item) then begin
    if Item.IndexRange<>'' then exit(ioIllegalRequest);
    Result := OPCUATextToVariant(Item.ValueType, OPCUAValuesToText(Values), WV.value.value);
    exit;
  end;
  n := Length(Values);
  if n=0 then exit(ioIllegalValue);
  off := Max(0, tagrec.OffSet);
  if Item.IndexRange<>'' then begin
    //the address picks the elements: written as an array with the same range
    if off>0 then exit(ioIllegalRequest);
    WV.indexRange := _UA_STRING_ALLOC(Item.IndexRange);
    Result := OPCUANumbersToVariant(Item.ValueType, true, Values, WV.value.value);
  end else if Item.IsArray then begin
    if (off=0) and ((not Item.HasValue) or (n>=Integer(Item.Value.arrayLength))) then
      Result := OPCUANumbersToVariant(Item.ValueType, true, Values, WV.value.value)
    else begin
      //part of the array: an index range (the server may refuse it, see DoWrite)
      if n=1 then range := IntToStr(off) else range := IntToStr(off)+':'+IntToStr(off+n-1);
      WV.indexRange := _UA_STRING_ALLOC(range);
      Result := OPCUANumbersToVariant(Item.ValueType, true, Values, WV.value.value);
      NeedsWholeArray := true;
    end;
  end else
    Result := OPCUANumbersToVariant(Item.ValueType, false, Values, WV.value.value);
end;

function TOPCUADriver.WriteWhole(Item:TOPCUAItem; Offset:Integer; const Values:TArrayOfDouble):TProtocolIOResult;
var
  all:TArrayOfDouble;
  i:Integer;
  WV:UA_WriteValue;
  st:UA_StatusCode;
begin
  //read-modify-write of the whole array
  ReadItems([Item]);
  if Item.LastResult<>ioOk then exit(Item.LastResult);
  Result := OPCUAVariantToNumbers(Item.Value, all);
  if Result<>ioOk then exit;
  if Offset+Length(Values)>Length(all) then exit(ioIllegalMemoryAddress);
  for i:=0 to High(Values) do
    all[Offset+i] := Values[i];
  FillChar(WV, SizeOf(WV), 0);
  WV.nodeId := Item.NodeId;
  WV.attributeId := Ord(UA_ATTRIBUTEID_VALUE);
  WV.value.flags := UA_DATAVALUE_HASVALUE;
  Result := OPCUANumbersToVariant(Item.ValueType, true, all, WV.value.value);
  if Result<>ioOk then exit;
  st := WriteOne(WV);
  UA_Variant_clear(WV.value.value);
  Result := OPCUAStatusToIOResult(st);
end;

function TOPCUADriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  fpu:TFPUExceptionMask;
begin
  fpu := OPCUAMaskFPU;
  try
    Result := WriteImpl(tagrec, Values, Sync);
  finally
    OPCUARestoreFPU(fpu);
  end;
end;

function TOPCUADriver.WriteImpl(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  item:TOPCUAItem;
  WV:UA_WriteValue;
  whole:Boolean;
  st:UA_StatusCode;
begin
  if not EnsureConnected(GetTickCount64) then exit(FConnectResult);
  item := GetItem(tagrec.Path);
  try
    if not Resolve(item, 0) then exit(item.LastResult);
    Result := LearnType(item);
    if Result<>ioOk then exit;
    Result := BuildWrite(item, tagrec, Values, WV, whole);
    if Result=ioOk then begin
      st := WriteOne(WV);
      Result := OPCUAStatusToIOResult(st);
      if whole and (((st and $FFFF0000)=UA_STATUSCODE_BADINDEXRANGEINVALID) or
                    ((st and $FFFF0000)=UA_STATUSCODE_BADWRITENOTSUPPORTED)) then
        Result := WriteWhole(item, Max(0, tagrec.OffSet), Values);
    end;
    UA_Variant_clear(WV.value.value);
    UA_String_clear(WV.indexRange);
  finally
    FreeItemIfUnused(item);
  end;
end;

procedure TOPCUADriver.DoMultiWrite(var Writes:TMultiWriteArray; Sync:Boolean);
var
  fpu:TFPUExceptionMask;
begin
  fpu := OPCUAMaskFPU;
  try
    MultiWriteImpl(Writes, Sync);
  finally
    OPCUARestoreFPU(fpu);
  end;
end;

procedure TOPCUADriver.MultiWriteImpl(var Writes:TMultiWriteArray; Sync:Boolean);
var
  i, j, n, start:Integer;
  items:array of TOPCUAItem;
  wvs:array of UA_WriteValue;
  idx:array of Integer;
  whole:Boolean;
  req:UA_WriteRequest;
  resp:UA_WriteResponse;
  st:UA_StatusCode;
  r:TProtocolIOResult;
begin
  if Length(Writes)=0 then exit;
  if not EnsureConnected(GetTickCount64) then begin
    for i:=0 to High(Writes) do Writes[i].Result := FConnectResult;
    exit;
  end;
  SetLength(items, Length(Writes));
  SetLength(wvs, Length(Writes));
  SetLength(idx, Length(Writes));
  n := 0;
  try
    for i:=0 to High(Writes) do begin
      items[i] := GetItem(Writes[i].Tag.Path);
      Writes[i].Result := ioNone;
      if not Resolve(items[i], 0) then begin
        Writes[i].Result := items[i].LastResult;
        continue;
      end;
      r := LearnType(items[i]);
      if r<>ioOk then begin
        Writes[i].Result := r;
        continue;
      end;
      r := BuildWrite(items[i], Writes[i].Tag, Writes[i].Values, wvs[n], whole);
      if (r<>ioOk) or whole then begin
        //a part of an array goes alone (it may need the read-modify-write)
        UA_Variant_clear(wvs[n].value.value);
        UA_String_clear(wvs[n].indexRange);
        if r=ioOk then
          Writes[i].Result := DoWrite(Writes[i].Tag, Writes[i].Values, Sync)
        else
          Writes[i].Result := r;
        continue;
      end;
      idx[n] := i;
      Inc(n);
    end;

    start := 0;
    while (start<n) and (FClient.Handle<>nil) do begin
      j := Min(FMaxItemsPerRequest, n-start);
      UA_WriteRequest_init(req);
      req.nodesToWrite := @wvs[start];
      req.nodesToWriteSize := j;
      resp := UA_Client_Service_write(FClient.Handle, req);
      st := resp.responseHeader.serviceResult;
      for i:=0 to j-1 do
        if (st=UA_STATUSCODE_GOOD) and (resp.resultsSize=j) then
          Writes[idx[start+i]].Result := OPCUAStatusToIOResult(resp.results[i])
        else
          Writes[idx[start+i]].Result := OPCUAStatusToIOResult(IfThen(st=UA_STATUSCODE_GOOD, UA_STATUSCODE_BADUNEXPECTEDERROR, st));
      UA_WriteResponse_clear(resp);
      CheckStatus(st);
      Inc(start, j);
    end;
    for i:=0 to High(Writes) do
      if Writes[i].Result=ioNone then
        Writes[i].Result := ioCommError;
  finally
    for i:=0 to n-1 do begin
      UA_Variant_clear(wvs[i].value.value);
      UA_String_clear(wvs[i].indexRange);
    end;
    for i:=0 to High(items) do
      if items[i]<>nil then
        FreeItemIfUnused(items[i]);
  end;
end;

{ browsing }

function TOPCUADriver.Browse(const NodeId:AnsiString; out Items:TOPCUABrowseItemArray):TProtocolIOResult;
var
  st:UA_StatusCode;
begin
  SetLength(Items, 0);
  LockDriverIO;
  try
    if not EnsureConnected(GetTickCount64) then exit(FConnectResult);
    st := FClient.Browse(NodeId, Items);
    CheckStatus(st);
    Result := OPCUAStatusToIOResult(st);
  finally
    UnlockDriverIO;
  end;
end;

function TOPCUADriver.ListTags(out Tags:TPLCTagInfoArray; ForceReload:Boolean):TProtocolIOResult;
const
  MaxDepth = 12;
  MaxNodes = 20000;
var
  n, visited:Integer;
  seen:TStringList;

  procedure AddVariable(const B:TOPCUABrowseItem; const ParentPath:String);
  var
    id:UA_NodeId;
    info:TOPCUAVariableInfo;
    i, count:Integer;
  begin
    if FClient.ParseNodeId(B.NodeId, id)<>UA_STATUSCODE_GOOD then exit;
    try
      if FClient.ReadVariableInfo(id, info)<>UA_STATUSCODE_GOOD then exit;
    finally
      UA_NodeId_clear(id);
    end;
    SetLength(Tags, n+1);
    FillChar(Tags[n], SizeOf(Tags[n]), 0);
    Tags[n].Path := B.NodeId;
    Tags[n].Name := B.BrowseName;
    Tags[n].ParentPath := ParentPath;
    Tags[n].NativeType := info.DataTypeName;
    Tags[n].TagType := pttDefault;
    count := 1;
    for i:=0 to High(info.ArrayDimensions) do
      if info.ArrayDimensions[i]>0 then count := count*info.ArrayDimensions[i];
    if OPCUAIsTextType(info.BuiltinType) then begin
      Tags[n].Kind := tkString;
      Tags[n].ElementCount := 1;
      Tags[n].SizeInBytes := 0;
    end else if (info.BuiltinType<>nil) and OPCUAIsNumericType(info.BuiltinType) then begin
      if info.ValueRank>=0 then begin
        Tags[n].Kind := tkArray;
        Tags[n].ElementCount := count;
      end else begin
        Tags[n].Kind := tkScalar;
        Tags[n].ElementCount := 1;
      end;
      Tags[n].SizeInBytes := UA_DataType_memSize(info.BuiltinType)*Tags[n].ElementCount;
    end else
      Tags[n].Kind := tkUnsupported;
    Inc(n);
  end;

  procedure Walk(const NodeId, ParentPath:String; Depth:Integer);
  var
    children:TOPCUABrowseItemArray;
    i:Integer;
    p:String;
  begin
    if (Depth>MaxDepth) or (visited>=MaxNodes) then exit;
    if FClient.Browse(NodeId, children)<>UA_STATUSCODE_GOOD then exit;
    for i:=0 to High(children) do begin
      if visited>=MaxNodes then exit;
      if seen.IndexOf(children[i].NodeId)>=0 then continue;
      seen.Add(children[i].NodeId);
      Inc(visited);
      if ParentPath='' then p := children[i].BrowseName else p := ParentPath+'.'+children[i].BrowseName;
      case children[i].NodeClass of
        oncVariable: begin
          AddVariable(children[i], ParentPath);
          //variables may have properties/components of their own
          Walk(children[i].NodeId, p, Depth+1);
        end;
        oncObject:
          if children[i].NodeIdNs<>'i=2253' then //the Server object: diagnostics, not process data
            Walk(children[i].NodeId, p, Depth+1);
      end;
    end;
  end;

begin
  SetLength(Tags, 0);
  n := 0;
  visited := 0;
  LockDriverIO;
  seen := TStringList.Create;
  try
    seen.Sorted := true;
    seen.CaseSensitive := true;
    if not EnsureConnected(GetTickCount64) then exit(FConnectResult);
    Walk('', '', 0);
    Result := ioOk;
  finally
    seen.Free;
    UnlockDriverIO;
  end;
end;

function TOPCUADriver.GetServerCertificates(out Certs:TStringList):TProtocolIOResult;
begin
  LockDriverIO;
  try
    Result := OPCUAStatusToIOResult(FClient.GetServerCertificates(Certs));
  finally
    UnlockDriverIO;
  end;
end;

function TOPCUADriver.GetClientCertificate(out Cert:TBytes):Boolean;
var
  key:TBytes;
begin
  LockDriverIO;
  try
    Result := FClient.GetClientCertificate(Cert, key);
  finally
    UnlockDriverIO;
  end;
end;

end.
