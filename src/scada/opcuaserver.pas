{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(TOPCUAServer: um servidor OPC UA interno que publica os tags da aplicacao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os tags publicados ficam na colecao Items. Cada item vira uma variavel OPC UA
  cujo valor mora na aplicacao (value source por callback do open62541): o
  servidor pergunta a cada leitura e responde com o valor, a qualidade (status
  da ultima leitura do tag) e o horario da ultima atualizacao.

  Threads: so' a thread do servidor toca no UA_Server. A thread principal, que
  e' onde os tags mudam, so' enfileira comandos (adicionar/remover variavel,
  valor novo) que a thread do servidor aplica num espelho antes de cada
  iteracao.
}
{$ELSE}
{:
  @abstract(TOPCUAServer: an internal OPC UA server that publishes the application tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The published tags are in the Items collection. Each item becomes an OPC UA
  variable whose value lives in the application (open62541 callback value
  source): the server asks on every read and answers with the value, the
  quality (status of the tag's last read) and the time of the last update.

  Threads: only the server thread touches the UA_Server. The main thread, where
  the tags change, only queues commands (add/remove a variable, a new value)
  that the server thread applies to a mirror before each iteration.
}
{$ENDIF}
unit opcuaserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Math, DateUtils, Generics.Collections,
  open62541, Tag, PLCTag;

type
  {$IFDEF PORTUGUES}
  {:
  Tipo da variavel OPC UA de um item.
  @value(odtNative O tipo nativo do tag (TagType) com o valor bruto (ValueRaw).)
  @value(odtScaledDouble Double com o valor em unidade de engenharia (Value, com o processador de escala).)
  }
  {$ELSE}
  {:
  OPC UA type of the variable of an item.
  @value(odtNative The tag's native type (TagType) with the raw value (ValueRaw).)
  @value(odtScaledDouble Double with the value in engineering units (Value, through the scale processor).)
  }
  {$ENDIF}
  TOPCUAServerDataType = (odtNative, odtScaledDouble);

  {$IFDEF PORTUGUES}
  {:
  Endpoints do servidor.
  @value(ossNone So' sem seguranca (padrao).)
  @value(ossNoneAndSecure Sem seguranca e com Sign/SignAndEncrypt (todas as politicas do open62541).)
  @value(ossSecureOnly Dados so' com Sign/SignAndEncrypt; o endpoint sem seguranca fica so' para a descoberta (o cliente pega o certificado).)
  }
  {$ELSE}
  {:
  Endpoints of the server.
  @value(ossNone Without security only (default).)
  @value(ossNoneAndSecure Without security and with Sign/SignAndEncrypt (every policy of open62541).)
  @value(ossSecureOnly Data only with Sign/SignAndEncrypt; the endpoint without security is left for discovery only (the client gets the certificate there).)
  }
  {$ENDIF}
  TOPCUAServerSecurity = (ossNone, ossNoneAndSecure, ossSecureOnly);

  TOPCUAServer = class;
  TOPCUAServerItem = class;

  {$IFDEF PORTUGUES}
  //: Login de um cliente (thread principal): Accepted ja' vem com o resultado da lista Users.
  {$ELSE}
  //: Login of a client (main thread): Accepted comes with the result of the Users list.
  {$ENDIF}
  TOPCUAServerCheckUserEvent = procedure(Sender:TObject; const User, Password:String; var Accepted:Boolean) of object;
  {$IFDEF PORTUGUES}
  //: Escrita de um cliente num item (thread principal). User vazio: login anonimo.
  {$ELSE}
  //: Write of a client to an item (main thread). An empty User: anonymous login.
  {$ENDIF}
  TOPCUAServerCanWriteEvent = procedure(Sender:TObject; Item:TOPCUAServerItem; const User:String; var Allowed:Boolean) of object;

  { TOPCUAServerItem }

  {$IFDEF PORTUGUES}
  {:
  Um tag publicado. O NodeId da variavel e' ns=<NamespaceUri>;s=<Folder com '.'>.<NodeName>
  (NodeName vazio usa o nome do tag).
  }
  {$ELSE}
  {:
  A published tag. The NodeId of the variable is ns=<NamespaceUri>;s=<Folder with '.'>.<NodeName>
  (an empty NodeName uses the tag's name).
  }
  {$ENDIF}
  TOPCUAServerItem = class(TCollectionItem)
  private
    FDataType:TOPCUAServerDataType;
    FDescription:String;
    FFolder:String;
    FNodeName:String;
    FTag:TPLCTag;
    FWritable:Boolean;
    //runtime, kept by the server
    FKey:Integer;
    FTypeIndex:Integer;
    FIsText:Boolean;
    FLastStatus:UA_StatusCode;
    FStatusTag:TPLCTag;
    FCanWrite:Boolean;
    FIsArray:Boolean;       //a TPLCBlock: an array of its type
    FIsObject:Boolean;      //a TPLCStruct: an object with a variable per item
    FParentKey:Integer;     //an item of a published structure (internal)
    FPublishedId:String;    //NodeId it was published with
    procedure SetDataType(AValue:TOPCUAServerDataType);
    procedure SetDescription(const AValue:String);
    procedure SetFolder(const AValue:String);
    procedure SetNodeName(const AValue:String);
    procedure SetTag(AValue:TPLCTag);
    procedure SetWritable(AValue:Boolean);
    function  GetServer:TOPCUAServer;
  protected
    function GetDisplayName:String; override;
  public
    constructor Create(ACollection:TCollection); override;
    procedure Assign(Source:TPersistent); override;
    //: NodeName, or the tag's name when it is empty.
    function EffectiveNodeName:String;
    //: The string identifier of the variable's NodeId (Folder with '.' + NodeName).
    function NodeIdText:String;
    //: True while the variable exists on the running server.
    function Published:Boolean;
    property Server:TOPCUAServer read GetServer;
  published
    property Tag:TPLCTag read FTag write SetTag;
    property NodeName:String read FNodeName write SetNodeName;
    //: Folder of the variable, levels separated by '/' (e.g. 'Line1/Pumps').
    property Folder:String read FFolder write SetFolder;
    property Description:String read FDescription write SetDescription;
    //: Whether OPC UA clients may write to it. Writing reaches the device.
    property Writable:Boolean read FWritable write SetWritable default false;
    property DataType:TOPCUAServerDataType read FDataType write SetDataType default odtNative;
  end;

  { TOPCUAServerItems }

  TOPCUAServerItems = class(TOwnedCollection)
  private
    FInternal:Boolean;
    function GetItem(Index:Integer):TOPCUAServerItem;
  protected
    procedure Update(Item:TCollectionItem); override;
    procedure Notify(Item:TCollectionItem; Action:Classes.TCollectionNotification); override;
  public
    constructor Create(AOwner:TOPCUAServer);
    function Add:TOPCUAServerItem;
    //: First item publishing this tag, or nil.
    function FindByTag(ATag:TPLCTag):TOPCUAServerItem;
    property Items[Index:Integer]:TOPCUAServerItem read GetItem; default;
  end;

  TOPCUAServerThread = class;
  TOPCUAServerTagMap = specialize TDictionary<TPLCTag, TFPList>;

  //: A write of a client, from the server thread to the main thread (internal).
  TOPCUAServerWriteRequest = record
    Id:Int64;
    Key:Integer;
    User:String;
    Num:Double;
    Text:UTF8String;
    //arrays
    Values:TArrayOfDouble;
    Offset:Integer;
  end;

  //: A write waiting for the result of the tag (internal, main thread).
  TOPCUAServerPendingWrite = class
  public
    Id:Int64;
    Key:Integer;
    ResultTag:TPLCTag;
    ReqID:Int64;
    Num:Double;
    Text:UTF8String;
    Values:TArrayOfDouble;
    Offset:Integer;
    Done:Boolean;
    Res:TProtocolIOResult;
  end;

  {$IFDEF PORTUGUES}
  //: Nivel minimo das mensagens do open62541 que vao para o log (lcProtocol).
  {$ELSE}
  //: Lowest level of the open62541 messages that go to the log (lcProtocol).
  {$ENDIF}
  TOPCUAServerLogLevel = (oslTrace, oslDebug, oslInfo, oslWarning, oslError, oslNone);

  { TOPCUAServer }

  {$IFDEF PORTUGUES}
  {:
  Servidor OPC UA interno: publica os tags da colecao Items. A biblioteca
  libopen62541 1.5 e' carregada ao ativar (LibraryName, ou o nome padrao e a
  pasta do executavel). Inativo em tempo de projeto.
  }
  {$ELSE}
  {:
  Internal OPC UA server: publishes the tags of the Items collection. The
  libopen62541 1.5 library is loaded when it is activated (LibraryName, or the
  default name and the executable's folder). Inactive at design time.
  }
  {$ENDIF}
  TOPCUAServer = class(TComponent)
  private
    FActive:Boolean;
    FItems:TOPCUAServerItems;
    FAutoItems:TOPCUAServerItems; //the items of the published structures
    FSecurity:TOPCUAServerSecurity;
    FCertificateDir:String;
    FTrustListDir:String;
    FAllowAnonymous:Boolean;
    FUsers:TStringList;
    FOnCheckUser:TOPCUAServerCheckUserEvent;
    FOnCanWrite:TOPCUAServerCanWriteEvent;
    FWriteTimeoutMs:Cardinal;
    FLibraryName:String;
    FLogLevel:TOPCUAServerLogLevel;
    FNamespaceUri:String;
    FPort:Word;
    FServerName:String;
    FLastError:String;
    FErrorLock:TCriticalSection;
    FThread:TOPCUAServerThread;
    FNextKey:Integer;
    FStartQueued:Boolean;
    FNamespaceIndex:Word;
    //tag -> items that publish it or take their status from it
    FTagItems:TOPCUAServerTagMap;
    //writes of the clients: queued by the server thread, done in the main thread
    FWriteLock:TCriticalSection;
    FWriteQueue:array of TOPCUAServerWriteRequest;
    FWriteQueueCount:Integer;
    FWriteQueued:Boolean;
    FPendingWrites:TFPList;     //TOPCUAServerPendingWrite waiting for the tag's result
    FCurrentWrite:TOPCUAServerPendingWrite;
    procedure SetUsers(AValue:TStrings);
    function  GetUsers:TStrings;
    procedure SetSecurity(AValue:TOPCUAServerSecurity);
    procedure SetAllowAnonymous(AValue:Boolean);
    procedure SetCertificateDir(const AValue:String);
    procedure SetTrustListDir(const AValue:String);
    function  GetLastError:String;
    function  GetRunning:Boolean;
    procedure SetActive(AValue:Boolean);
    procedure SetItems(AValue:TOPCUAServerItems);
    procedure SetLibraryName(const AValue:String);
    procedure SetNamespaceUri(const AValue:String);
    procedure SetPort(AValue:Word);
    procedure SetServerName(const AValue:String);
    procedure DeferredStart;
    procedure StartServer;
    procedure StopServer;
    procedure PublishItem(Item:TOPCUAServerItem);
    procedure UnpublishItem(Item:TOPCUAServerItem);
    procedure PostValue(Item:TOPCUAServerItem);
    procedure LinkTag(ATag:TPLCTag; Item:TOPCUAServerItem);
    procedure UnlinkTag(ATag:TPLCTag; Item:TOPCUAServerItem);
    procedure TagChanged(Sender:TObject);
    procedure TagReadDone(Sender:TObject);
    procedure TagRemoved(Sender:TObject);
    procedure DoInactiveCheck;
    procedure QueueWrite(const Req:TOPCUAServerWriteRequest);
    procedure ProcessWrites;
    function  StartWrite(Item:TOPCUAServerItem; const Req:TOPCUAServerWriteRequest):UA_StatusCode;
    procedure TagCommandResult(Sender:TPLCTag; TagCommand:TTagCommand; ReqID:LongWord; LastResult:TProtocolIOResult);
    procedure CompleteWrite(P:TOPCUAServerPendingWrite; Status:UA_StatusCode);
    procedure FailPendingWrites(Key:Integer; ResultTag:TPLCTag; Status:UA_StatusCode);
    procedure ReleaseResultTag(ATag:TPLCTag);
    function  ItemByKey(Key:Integer):TOPCUAServerItem;
    procedure PublishStructItems(Item:TOPCUAServerItem);
    procedure UnpublishStructItems(Item:TOPCUAServerItem);
    procedure ForEachItem(ATag:TComponent; Action:Integer);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent:TComponent; Operation:TOperation); override;
    procedure ItemChanged(Items:TOPCUAServerItems; Item:TOPCUAServerItem);
    procedure ItemRemoving(Item:TOPCUAServerItem);
    procedure SetError(const Msg:String);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    //: Whether the server is running (Active and started without error).
    property Running:Boolean read GetRunning;
    //: Last error (start, a variable that could not be added...); empty when none.
    property LastError:String read GetLastError;
    //: Index of NamespaceUri on the running server (the ns= of the variables); 0 when stopped.
    property NamespaceIndex:Word read FNamespaceIndex;
  published
    property Active:Boolean read FActive write SetActive default false;
    //: TCP port of the opc.tcp:// endpoint.
    property Port:Word read FPort write SetPort default 4840;
    //: Display name of the root folder of the published tags (under Objects).
    property ServerName:String read FServerName write SetServerName;
    //: Namespace of the variables (its index is what goes in ns=).
    property NamespaceUri:String read FNamespaceUri write SetNamespaceUri;
    //: Path/name of libopen62541; empty tries the default name and the executable's folder.
    property LibraryName:String read FLibraryName write SetLibraryName;
    property LogLevel:TOPCUAServerLogLevel read FLogLevel write FLogLevel default oslWarning;
    property Items:TOPCUAServerItems read FItems write SetItems;
    //: Endpoints without and/or with security (Sign/SignAndEncrypt).
    property Security:TOPCUAServerSecurity read FSecurity write SetSecurity default ossNone;
    {:
    Where the server certificate is kept (server_cert.der, server_key.der): created
    (self-signed) on the first secure start; replace them by your own if you wish.
    }
    property CertificateDir:String read FCertificateDir write SetCertificateDir;
    //: Client certificates accepted (.der/.pem/.crt/.cer); empty accepts any client.
    property TrustListDir:String read FTrustListDir write SetTrustListDir;
    property AllowAnonymous:Boolean read FAllowAnonymous write SetAllowAnonymous default true;
    //: Users that may log in, one per line: login=password (needs Security other than ossNone).
    property Users:TStrings read GetUsers write SetUsers;
    //: Decides a login (main thread); Accepted comes with the result of Users.
    property OnCheckUser:TOPCUAServerCheckUserEvent read FOnCheckUser write FOnCheckUser;
    {:
    Longest wait for the device's result of a write; the client then gets
    BadTimeout. 0 keeps the open62541 default (2 minutes).
    }
    property WriteTimeoutMs:Cardinal read FWriteTimeoutMs write FWriteTimeoutMs default 0;
    //: Decides each write of a client (main thread); refused writes get BadUserAccessDenied.
    property OnCanWrite:TOPCUAServerCanWriteEvent read FOnCanWrite write FOnCanWrite;
  end;

  { server thread: internal }

  TOPCUAServerCmdKind = (sckAdd, sckRemove, sckValue, sckWriteResult, sckAddObject, sckRemoveObject);

  TOPCUAServerCmd = record
    Kind:TOPCUAServerCmdKind;
    Key:Integer;
    //add
    NodeId, BrowseName, Folder, Description:String;
    TypeIndex:Integer;
    IsText:Boolean;
    IsArray:Boolean;
    Writable:Boolean;
    //write result
    WriteId:Int64;
    //add and value
    Num:Double;
    Text:UTF8String;
    Values:TArrayOfDouble;
    Status:UA_StatusCode;
    TimeUTC:TDateTime;
  end;

  //: Mirror of one variable; owned and touched by the server thread only.
  TOPCUAServerNode = class
  public
    Key:Integer;
    NodeId:AnsiString;
    TypeIndex:Integer;
    IsText:Boolean;
    IsArray:Boolean;
    Num:Double;
    Text:UTF8String;
    Values:TArrayOfDouble;
    Status:UA_StatusCode;
    Time:UA_DateTime;
    Thread:TOPCUAServerThread;
  end;
  TOPCUAServerNodeMap = specialize TDictionary<Integer, TOPCUAServerNode>;

  { TOPCUAServerThread }

  TOPCUAServerThread = class(TThread)
  private
    FOwner:TOPCUAServer;
    FPort:Word;
    FNamespaceUri, FServerName:AnsiString;
    FServer:PUA_Server;
    FNs:UA_UInt16;
    FRootId:UA_NodeId;
    FLogger:UA_Logger;
    FLock:TCriticalSection;
    FCmds:array of TOPCUAServerCmd;
    FCmdCount:Integer;
    FWake:TEventObject;
    FStarted:TEventObject;
    FStartError:String;
    FNodes:TOPCUAServerNodeMap;
    FFolders:TStringList;
    //writes waiting for their result: id -> the value pointer open62541 gave
    FPending:specialize TDictionary<Int64, PUA_DataValue>;
    FWriteSeq:Int64;
    //security, copied from the owner when created
    FSecurity:TOPCUAServerSecurity;
    FCertificateDir, FTrustListDir:String;
    FAllowAnonymous:Boolean;
    FUsers:TStringList;
    FUseCheckUser:Boolean;
    FWriteTimeoutMs:Cardinal;
    //names of the users logged in: a session context is the index+1 of its name (0 anonymous)
    FUserNames:TStringList;
    FCheckUser, FCheckPassword:String;
    FCheckAccepted:Boolean;
    procedure DoCheckUser;
    function  CheckLogin(const User, Password:String):Boolean;
    function  UserOf(SessionContext:Pointer):String;
    procedure SetupSecurity(Conf:PUA_ServerConfig);
    function  BeginWrite(Node:TOPCUAServerNode; const Value:PUA_DataValue; const Range:PUA_NumericRange;
                         SessionContext:Pointer):UA_StatusCode;
    procedure EndWrite(Id:Int64; Status:UA_StatusCode);
    procedure ApplyCmds;
    procedure AddNode(const Cmd:TOPCUAServerCmd);
    procedure RemoveNode(Key:Integer);
    procedure AddObject(const Cmd:TOPCUAServerCmd);
    procedure RemoveObject(const Cmd:TOPCUAServerCmd);
    function  EnsureFolder(const Path:AnsiString):Boolean;
    procedure Setup;
    procedure TearDown;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner:TOPCUAServer);
    destructor Destroy; override;
    procedure Post(const Cmd:TOPCUAServerCmd);
    function  WaitStarted(TimeoutMs:Cardinal):Boolean;
    property  StartError:String read FStartError;
  end;

  {$IFDEF PORTUGUES}
  //: Status OPC UA de um resultado de E/S do PascalSCADA.
  {$ELSE}
  //: OPC UA status of a PascalSCADA I/O result.
  {$ENDIF}
  function OPCUAServerStatusOf(Res:TProtocolIOResult):UA_StatusCode;

implementation

uses
  PLCNumber, TagBit, PLCBlock, PLCBlockElement, PLCString, PLCStructString,
  PLCStruct, PLCStructElement, NumExprTag, ProtocolTypes, ValueProcessor,
  OPCUAClient, OPCUAValues, pscadalog{$IFDEF UNIX}, Unix{$ENDIF};

function HostName:AnsiString;
begin
  {$IFDEF UNIX}
  Result := GetHostName;
  {$ELSE}
  Result := GetEnvironmentVariable('COMPUTERNAME');
  {$ENDIF}
  if Result='' then Result := 'localhost';
end;

function ReadFileBytes(const FileName:String; out Data:RawByteString):Boolean;
var
  fs:TFileStream;
begin
  Data := '';
  Result := false;
  if not FileExists(FileName) then exit;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Data, fs.Size);
    if fs.Size>0 then fs.ReadBuffer(Data[1], fs.Size);
    Result := fs.Size>0;
  finally
    fs.Free;
  end;
end;

procedure WriteFileBytes(const FileName:String; const Data:RawByteString);
var
  fs:TFileStream;
begin
  ForceDirectories(ExtractFileDir(FileName));
  fs := TFileStream.Create(FileName, fmCreate);
  try
    if Data<>'' then fs.WriteBuffer(Data[1], Length(Data));
  finally
    fs.Free;
  end;
end;

function AsByteString(const S:RawByteString):UA_ByteString;
begin
  Result.length := Length(S);
  if S<>'' then Result.data := @S[1] else Result.data := nil;
end;

type
  TPLCTagAccess = class(TPLCTag);
  TPLCNumberAccess = class(TPLCNumber);

const
  StartTimeoutMs = 10000;

{ helpers }

function OPCUAServerStatusOf(Res:TProtocolIOResult):UA_StatusCode;
begin
  case Res of
    ioOk, ioNullDriver, ioPartialOk:
      Result := UA_STATUSCODE_GOOD;
    ioNone:
      Result := UA_STATUSCODE_BADWAITINGFORINITIALDATA;
    ioCommError, ioTimeOut, ioConnectPLCFailed, ioGatewayUnavailable,
    ioDeviceGatewayFailedToRespond, ioEmptyPacket, ioNACK:
      Result := UA_STATUSCODE_BADCOMMUNICATIONERROR;
    ioCommPortClosed, ioNullCommPort, ioAdapterInitFail:
      Result := UA_STATUSCODE_BADNOCOMMUNICATION;
    ioAuthError:
      Result := UA_STATUSCODE_BADUSERACCESSDENIED;
    ioReadOnlyProtocol, ioObjectAccessNotAllowed:
      Result := UA_STATUSCODE_BADNOTWRITABLE;
    ioIllegalValue, ioIllegalRegSize:
      Result := UA_STATUSCODE_BADOUTOFRANGE;
    ioIllegalFunction, ioIllegalRegAddress, ioIllegalMemoryAddress, ioIllegalRequest,
    ioIllegalStationAddress, ioObjectNotExists, ioTagError, ioNullTagBlock:
      Result := UA_STATUSCODE_BADCONFIGURATIONERROR;
    ioBusy:
      Result := UA_STATUSCODE_BADRESOURCEUNAVAILABLE;
  else
    Result := UA_STATUSCODE_BADDEVICEFAILURE;
  end;
end;

//The tag whose read status is the item's: the source tag of a bit, the block of an element.
function StatusTagOf(T:TPLCTag):TPLCTag;
begin
  Result := T;
  if T is TTagBit then
    Result := TTagBit(T).PLCTag
  else if T is TPLCBlockElement then
    Result := TPLCBlockElement(T).PLCBlock;
end;

function TagStatus(Item:TOPCUAServerItem):UA_StatusCode;
var
  src:TPLCTag;
begin
  src := Item.FStatusTag;
  if src=nil then
    exit(UA_STATUSCODE_BADCONFIGURATIONERROR);
  //an expression or a memory tag (no driver) has no device to fail
  if (src is TNumericExprTag) or (TPLCTagAccess(src).ProtocolDriver=nil) then
    exit(UA_STATUSCODE_GOOD);
  Result := OPCUAServerStatusOf(src.LastASyncReadStatus);
end;

function WordSizeType(Bits:Integer):Integer;
begin
  case Bits of
    1:  Result := UA_TYPES_BOOLEAN;
    8:  Result := UA_TYPES_BYTE;
    16: Result := UA_TYPES_UINT16;
    32: Result := UA_TYPES_UINT32;
    64: Result := UA_TYPES_UINT64;
  else
    Result := UA_TYPES_DOUBLE;
  end;
end;

function TagTypeIndex(TT:TTagType; Owner:TPLCTag):Integer;
begin
  case TT of
    pttShortInt: Result := UA_TYPES_SBYTE;
    pttByte:     Result := UA_TYPES_BYTE;
    pttSmallInt: Result := UA_TYPES_INT16;
    pttWord:     Result := UA_TYPES_UINT16;
    pttLongInt:  Result := UA_TYPES_INT32;
    pttDWord:    Result := UA_TYPES_UINT32;
    pttFloat:    Result := UA_TYPES_FLOAT;
    pttInt64:    Result := UA_TYPES_INT64;
    pttQWord:    Result := UA_TYPES_UINT64;
    pttDouble:   Result := UA_TYPES_DOUBLE;
  else
    //pttDefault: unsigned, of the driver's word size (a memory tag has none)
    if (Owner=nil) or (TPLCTagAccess(Owner).ProtocolDriver=nil) then
      Result := UA_TYPES_DOUBLE
    else
      Result := WordSizeType(TPLCTagAccess(Owner).FProtocolWordSize);
  end;
end;

{$IFDEF PORTUGUES}
//: Tipo da variavel de um item (indice em UA_TYPES). Falso quando o tag nao e' suportado.
{$ELSE}
//: Type of the variable of an item (index in UA_TYPES). False when the tag is not supported.
{$ENDIF}
function ItemType(Item:TOPCUAServerItem; out TypeIndex:Integer; out IsText:Boolean;
  out Why:String):Boolean;
var
  T:TPLCTag;
  bits:Integer;
  blk:TPLCBlock;
begin
  Result := false;
  IsText := false;
  Item.FIsArray := false;
  Item.FIsObject := false;
  TypeIndex := UA_TYPES_DOUBLE;
  Why := '';
  T := Item.Tag;
  if T is TPLCStruct then begin
    Item.FIsObject := true;
    exit(true);
  end;
  if (T is TPLCBlock) and not (T is TPLCString) then begin
    Item.FIsArray := true;
    TypeIndex := TagTypeIndex(TPLCTagAccess(T).TagType, T);
    exit(true);
  end;
  if (T is TPLCString) or (T is TPLCStructString) then begin
    IsText := true;
    TypeIndex := UA_TYPES_STRING;
    exit(true);
  end;
  if not (T is TPLCNumber) then begin
    Why := T.ClassName+' is not supported yet';
    exit;
  end;
  if (Item.DataType=odtScaledDouble) or (T is TNumericExprTag) then
    exit(true);
  if T is TTagBit then begin
    bits := TTagBit(T).EndBit-TTagBit(T).StartBit+1;
    if bits<=1 then
      TypeIndex := UA_TYPES_BOOLEAN
    else if bits<=16 then
      TypeIndex := UA_TYPES_UINT16
    else if bits<=32 then
      TypeIndex := UA_TYPES_UINT32
    else
      TypeIndex := UA_TYPES_UINT64;
    exit(true);
  end;
  if T is TPLCBlockElement then begin
    //an element has the type of its block, unless it has its own (struct items)
    blk := TPLCBlockElement(T).PLCBlock;
    if TPLCTagAccess(T).TagType<>pttDefault then
      TypeIndex := TagTypeIndex(TPLCTagAccess(T).TagType, blk)
    else if blk<>nil then
      TypeIndex := TagTypeIndex(TPLCTagAccess(TPLCTag(blk)).TagType, blk)
    else
      TypeIndex := UA_TYPES_DOUBLE;
    exit(true);
  end;
  TypeIndex := TagTypeIndex(TPLCTagAccess(T).TagType, T);
  Result := true;
end;

procedure ServerLog(logContext:Pointer; level:UA_LogLevel; category:UA_LogCategory; msg:PAnsiChar; args:Pointer); cdecl;
const
  ToLevel:array[TOPCUAServerLogLevel] of TpSCADALogLevel = (llTrace, llDebug, llInfo, llWarning, llError, llError);
var
  t:TOPCUAServerThread;
  l:TOPCUAServerLogLevel;
begin
  t := TOPCUAServerThread(logContext);
  if t=nil then exit;
  case level of
    UA_LOGLEVEL_TRACE:   l := oslTrace;
    UA_LOGLEVEL_DEBUG:   l := oslDebug;
    UA_LOGLEVEL_INFO:    l := oslInfo;
    UA_LOGLEVEL_WARNING: l := oslWarning;
  else
    l := oslError;
  end;
  if (l<t.FOwner.FLogLevel) or (t.FOwner.FLogLevel=oslNone) then exit;
  if not PSLogWants(lcProtocol, ToLevel[l]) then exit;
  try
    PSLog(t.FOwner, lcProtocol, ToLevel[l], String(UA_FormatLogMessage(msg, args)));
  except
    //never let an exception cross the C library
  end;
end;

{$IFDEF PORTUGUES}
//: Leitura de uma variavel: o espelho vira o DataValue (thread do servidor).
{$ELSE}
//: Read of a variable: the mirror becomes the DataValue (server thread).
{$ENDIF}
function NodeRead(server:PUA_Server; const sessionId:PUA_NodeId; sessionContext:Pointer;
  const nodeId:PUA_NodeId; nodeContext:Pointer; includeSourceTimeStamp:UA_Boolean;
  const range:PUA_NumericRange; value:PUA_DataValue):UA_StatusCode; cdecl;
var
  n:TOPCUAServerNode;
  vals:TArrayOfDouble;
  res:TProtocolIOResult;
  f:UA_Float;
  d:UA_Double;
begin
  //a read with no value returns its error (open62541 turns an empty value with a
  //Good result into BadWaitingForInitialData); with a value the quality goes in it
  Result := UA_STATUSCODE_GOOD;
  try
    n := TOPCUAServerNode(nodeContext);
    value^.flags := value^.flags or UA_DATAVALUE_HASSTATUS;
    if (range<>nil) and (range^.dimensionsSize>0) and not n.IsArray then begin
      value^.status := UA_STATUSCODE_BADINDEXRANGEINVALID;
      exit(UA_STATUSCODE_BADINDEXRANGEINVALID);
    end;
    if n.Status=UA_STATUSCODE_BADWAITINGFORINITIALDATA then begin
      value^.status := n.Status;
      exit(n.Status);
    end;
    if n.IsArray then begin
      if (range<>nil) and (range^.dimensionsSize>0) then begin
        //one dimension; min..max inside the array
        if (range^.dimensionsSize<>1) or (range^.dimensions^.min>range^.dimensions^.max) then begin
          value^.status := UA_STATUSCODE_BADINDEXRANGEINVALID;
          exit(UA_STATUSCODE_BADINDEXRANGEINVALID);
        end;
        if range^.dimensions^.min>=UA_UInt32(Length(n.Values)) then begin
          value^.status := UA_STATUSCODE_BADINDEXRANGENODATA;
          exit(UA_STATUSCODE_BADINDEXRANGENODATA);
        end;
        vals := Copy(n.Values, range^.dimensions^.min,
                     Min(range^.dimensions^.max, High(n.Values))-range^.dimensions^.min+1);
      end else
        vals := n.Values;
      res := OPCUANumbersToVariant(@UA_TYPES[n.TypeIndex], true, vals, value^.value);
    end else if n.IsText then
      res := OPCUATextToVariant(@UA_TYPES[n.TypeIndex], n.Text, value^.value)
    else if IsNan(n.Num) and (n.TypeIndex=UA_TYPES_DOUBLE) then begin
      d := n.Num;
      res := OPCUAStatusToIOResult(UA_Variant_setScalarCopy(@value^.value, @d, @UA_TYPES[UA_TYPES_DOUBLE]));
    end else if IsNan(n.Num) and (n.TypeIndex=UA_TYPES_FLOAT) then begin
      f := n.Num;
      res := OPCUAStatusToIOResult(UA_Variant_setScalarCopy(@value^.value, @f, @UA_TYPES[UA_TYPES_FLOAT]));
    end else begin
      SetLength(vals, 1);
      vals[0] := n.Num;
      res := OPCUANumbersToVariant(@UA_TYPES[n.TypeIndex], false, vals, value^.value);
    end;
    if res<>ioOk then begin
      //the raw value does not fit the type (e.g. a negative one in an unsigned type)
      value^.status := UA_STATUSCODE_BADOUTOFRANGE;
      exit(UA_STATUSCODE_BADOUTOFRANGE);
    end;
    value^.flags := value^.flags or UA_DATAVALUE_HASVALUE;
    value^.status := n.Status;
    if includeSourceTimeStamp then begin
      value^.sourceTimestamp := n.Time;
      value^.flags := value^.flags or UA_DATAVALUE_HASSOURCETIMESTAMP;
    end;
  except
    value^.status := UA_STATUSCODE_BADINTERNALERROR;
    Result := UA_STATUSCODE_BADINTERNALERROR;
  end;
end;

{$IFDEF PORTUGUES}
{:
Escrita de um cliente (thread do servidor): confere o valor e o entrega a
thread principal; o resultado do tag chega depois (setAsyncWriteResult).
}
{$ELSE}
{:
Write of a client (server thread): checks the value and hands it to the main
thread; the tag's result comes later (setAsyncWriteResult).
}
{$ENDIF}
function NodeWrite(server:PUA_Server; const sessionId:PUA_NodeId; sessionContext:Pointer;
  const nodeId:PUA_NodeId; nodeContext:Pointer; const range:PUA_NumericRange;
  const value:PUA_DataValue):UA_StatusCode; cdecl;
var
  n:TOPCUAServerNode;
begin
  try
    n := TOPCUAServerNode(nodeContext);
    Result := n.Thread.BeginWrite(n, value, range, sessionContext);
  except
    Result := UA_STATUSCODE_BADINTERNALERROR;
  end;
end;

{$IFDEF PORTUGUES}
{:
Login de uma sessao (thread do servidor). O contexto da sessao guarda o
usuario (o indice+1 do nome em FUserNames; 0 = anonimo), que as escritas leem.
}
{$ELSE}
{:
Login of a session (server thread). The session context keeps the user (the
index+1 of the name in FUserNames; 0 = anonymous), which the writes read.
}
{$ENDIF}
function ServerLogin(const userName:PUA_String; const password:PUA_ByteString;
  usernamePasswordLoginSize:size_t; const usernamePasswordLogin:PUA_UsernamePasswordLogin;
  sessionContext:PPointer; loginContext:Pointer):UA_StatusCode; cdecl;
var
  t:TOPCUAServerThread;
  user, pass:String;
  idx:Integer;
begin
  try
    t := TOPCUAServerThread(loginContext);
    user := '';
    pass := '';
    if (userName<>nil) and (userName^.length>0) then begin
      SetString(user, PAnsiChar(userName^.data), userName^.length);
      SetCodePage(RawByteString(user), CP_UTF8, false);
    end;
    if (password<>nil) and (password^.length>0) then
      SetString(pass, PAnsiChar(password^.data), password^.length);
    if user='' then begin
      //anonymous (refused before this when AllowAnonymous is off)
      sessionContext^ := nil;
      exit(UA_STATUSCODE_GOOD);
    end;
    if not t.CheckLogin(user, pass) then begin
      if PSLogWants(lcSecurity, llWarning) then
        PSLog(t.FOwner, lcSecurity, llWarning, 'OPC UA login failed: '+user);
      exit(UA_STATUSCODE_BADUSERACCESSDENIED);
    end;
    idx := t.FUserNames.IndexOf(user);
    if idx<0 then idx := t.FUserNames.Add(user);
    sessionContext^ := Pointer(PtrUInt(idx+1));
    if PSLogWants(lcSecurity, llInfo) then
      PSLog(t.FOwner, lcSecurity, llInfo, 'OPC UA login: '+user);
    Result := UA_STATUSCODE_GOOD;
  except
    Result := UA_STATUSCODE_BADINTERNALERROR;
  end;
end;

{ TOPCUAServerItem }

constructor TOPCUAServerItem.Create(ACollection:TCollection);
begin
  FKey := 0;
  FTypeIndex := -1;
  FLastStatus := UA_STATUSCODE_GOOD;
  inherited Create(ACollection);
end;

procedure TOPCUAServerItem.Assign(Source:TPersistent);
var
  S:TOPCUAServerItem;
begin
  if Source is TOPCUAServerItem then begin
    S := TOPCUAServerItem(Source);
    FNodeName := S.FNodeName;
    FFolder := S.FFolder;
    FDescription := S.FDescription;
    FWritable := S.FWritable;
    FDataType := S.FDataType;
    SetTag(S.FTag);
    Changed(false);
  end else
    inherited Assign(Source);
end;

function TOPCUAServerItem.GetServer:TOPCUAServer;
begin
  if (Collection<>nil) and (Collection.Owner is TOPCUAServer) then
    Result := TOPCUAServer(Collection.Owner)
  else
    Result := nil;
end;

function TOPCUAServerItem.GetDisplayName:String;
begin
  Result := NodeIdText;
  if Result='' then Result := inherited GetDisplayName;
end;

function TOPCUAServerItem.EffectiveNodeName:String;
begin
  Result := Trim(FNodeName);
  if (Result='') and (FTag<>nil) then
    Result := FTag.Name;
end;

function TOPCUAServerItem.NodeIdText:String;
var
  f:String;
begin
  Result := EffectiveNodeName;
  if Result='' then exit;
  f := StringReplace(Trim(FFolder), '\', '/', [rfReplaceAll]);
  while (f<>'') and (f[1]='/') do Delete(f, 1, 1);
  while (f<>'') and (f[Length(f)]='/') do Delete(f, Length(f), 1);
  if f<>'' then
    Result := StringReplace(f, '/', '.', [rfReplaceAll])+'.'+Result;
end;

function TOPCUAServerItem.Published:Boolean;
begin
  Result := FKey<>0;
end;

procedure TOPCUAServerItem.SetTag(AValue:TPLCTag);
var
  srv:TOPCUAServer;
begin
  if FTag=AValue then exit;
  srv := Server;
  if (srv<>nil) and (FTag<>nil) then FTag.RemoveFreeNotification(srv);
  FTag := AValue;
  if (srv<>nil) and (FTag<>nil) then FTag.FreeNotification(srv);
  Changed(false);
end;

procedure TOPCUAServerItem.SetDataType(AValue:TOPCUAServerDataType);
begin
  if FDataType=AValue then exit;
  FDataType := AValue;
  Changed(false);
end;

procedure TOPCUAServerItem.SetDescription(const AValue:String);
begin
  if FDescription=AValue then exit;
  FDescription := AValue;
  Changed(false);
end;

procedure TOPCUAServerItem.SetFolder(const AValue:String);
begin
  if FFolder=AValue then exit;
  FFolder := AValue;
  Changed(false);
end;

procedure TOPCUAServerItem.SetNodeName(const AValue:String);
begin
  if FNodeName=AValue then exit;
  FNodeName := AValue;
  Changed(false);
end;

procedure TOPCUAServerItem.SetWritable(AValue:Boolean);
begin
  if FWritable=AValue then exit;
  FWritable := AValue;
  Changed(false);
end;

{ TOPCUAServerItems }

constructor TOPCUAServerItems.Create(AOwner:TOPCUAServer);
begin
  inherited Create(AOwner, TOPCUAServerItem);
end;

function TOPCUAServerItems.Add:TOPCUAServerItem;
begin
  Result := TOPCUAServerItem(inherited Add);
end;

function TOPCUAServerItems.GetItem(Index:Integer):TOPCUAServerItem;
begin
  Result := TOPCUAServerItem(inherited Items[Index]);
end;

function TOPCUAServerItems.FindByTag(ATag:TPLCTag):TOPCUAServerItem;
var
  i:Integer;
begin
  for i:=0 to Count-1 do
    if Items[i].Tag=ATag then exit(Items[i]);
  Result := nil;
end;

procedure TOPCUAServerItems.Update(Item:TCollectionItem);
begin
  inherited Update(Item);
  if Owner is TOPCUAServer then
    TOPCUAServer(Owner).ItemChanged(Self, TOPCUAServerItem(Item));
end;

procedure TOPCUAServerItems.Notify(Item:TCollectionItem; Action:Classes.TCollectionNotification);
begin
  if (Action in [Classes.cnDeleting, Classes.cnExtracting]) and (Owner is TOPCUAServer) then
    TOPCUAServer(Owner).ItemRemoving(TOPCUAServerItem(Item));
  inherited Notify(Item, Action);
end;

{ TOPCUAServerThread }

constructor TOPCUAServerThread.Create(AOwner:TOPCUAServer);
begin
  FOwner := AOwner;
  FPort := AOwner.FPort;
  FNamespaceUri := AOwner.FNamespaceUri;
  FServerName := AOwner.FServerName;
  FLock := TCriticalSection.Create;
  FWake := TEventObject.Create(nil, false, false, '');
  FStarted := TEventObject.Create(nil, true, false, '');
  FNodes := TOPCUAServerNodeMap.Create;
  FFolders := TStringList.Create;
  FFolders.Sorted := true;
  FPending := specialize TDictionary<Int64, PUA_DataValue>.Create;
  FSecurity := AOwner.FSecurity;
  FCertificateDir := AOwner.FCertificateDir;
  FTrustListDir := AOwner.FTrustListDir;
  FAllowAnonymous := AOwner.FAllowAnonymous;
  FUsers := TStringList.Create;
  FUsers.Assign(AOwner.FUsers);
  FUseCheckUser := Assigned(AOwner.FOnCheckUser);
  FWriteTimeoutMs := AOwner.FWriteTimeoutMs;
  FUserNames := TStringList.Create;
  FillChar(FLogger, SizeOf(FLogger), 0);
  FLogger.log := @ServerLog;
  FLogger.context := Self;
  FLogger.clear := nil;
  inherited Create(false);
end;

destructor TOPCUAServerThread.Destroy;
var
  n:TOPCUAServerNode;
begin
  inherited Destroy;
  for n in FNodes.Values do n.Free;
  FNodes.Free;
  FFolders.Free;
  FPending.Free;
  FUsers.Free;
  FUserNames.Free;
  FStarted.Free;
  FWake.Free;
  FLock.Free;
end;

function TOPCUAServerThread.WaitStarted(TimeoutMs:Cardinal):Boolean;
begin
  Result := FStarted.WaitFor(TimeoutMs)=wrSignaled;
end;

procedure TOPCUAServerThread.Post(const Cmd:TOPCUAServerCmd);
begin
  FLock.Enter;
  try
    if FCmdCount>=Length(FCmds) then
      SetLength(FCmds, 64+2*FCmdCount);
    FCmds[FCmdCount] := Cmd;
    Inc(FCmdCount);
  finally
    FLock.Leave;
  end;
  FWake.SetEvent;
end;

procedure TOPCUAServerThread.ApplyCmds;
var
  cmds:array of TOPCUAServerCmd;
  count, i:Integer;
  n:TOPCUAServerNode;
begin
  FLock.Enter;
  try
    cmds := FCmds;
    count := FCmdCount;
    FCmds := nil;
    FCmdCount := 0;
  finally
    FLock.Leave;
  end;
  for i:=0 to count-1 do
    case cmds[i].Kind of
      sckAdd:    AddNode(cmds[i]);
      sckRemove: RemoveNode(cmds[i].Key);
      sckWriteResult: EndWrite(cmds[i].WriteId, cmds[i].Status);
      sckAddObject: AddObject(cmds[i]);
      sckRemoveObject: RemoveObject(cmds[i]);
      sckValue:
        if FNodes.TryGetValue(cmds[i].Key, n) then begin
          n.Num := cmds[i].Num;
          n.Text := cmds[i].Text;
          n.Values := cmds[i].Values;
          n.Status := cmds[i].Status;
          n.Time := UA_DateTimeFromDateTime(cmds[i].TimeUTC);
        end;
    end;
end;

//Creates the folders of Path ('A.B.C') that do not exist yet, under the root folder.
function TOPCUAServerThread.EnsureFolder(const Path:AnsiString):Boolean;
var
  rest, part, sub, parentPath:AnsiString;
  p:Integer;
  attr:UA_ObjectAttributes;
  st:UA_StatusCode;
  parent:UA_NodeId;
begin
  Result := true;
  if (Path='') or (FFolders.IndexOf(Path)>=0) then exit;
  rest := Path;
  parentPath := '';
  while rest<>'' do begin
    p := Pos('.', rest);
    if p=0 then p := Length(rest)+1;
    part := Copy(rest, 1, p-1);
    Delete(rest, 1, p);
    if parentPath='' then sub := part else sub := parentPath+'.'+part;
    if FFolders.IndexOf(sub)<0 then begin
      if parentPath='' then
        parent := FRootId
      else
        parent := UA_NODEID_STRING(FNs, parentPath);
      attr := UA_ObjectAttributes_default;
      attr.displayName := _UA_LOCALIZEDTEXT_ALLOC('', part);
      st := UA_Server_addObjectNode(FServer, UA_NODEID_STRING(FNs, sub), parent,
              UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES), _UA_QUALIFIEDNAME(FNs, part),
              UA_NODEID_NUMERIC(0, UA_NS0ID_FOLDERTYPE), attr, nil, nil);
      UA_clear(@attr.displayName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
      if st<>UA_STATUSCODE_GOOD then begin
        FOwner.SetError(Format('folder "%s": %s', [sub, OPCUAStatusText(st)]));
        exit(false);
      end;
      FFolders.Add(sub);
    end;
    parentPath := sub;
  end;
end;

procedure TOPCUAServerThread.AddNode(const Cmd:TOPCUAServerCmd);
var
  n:TOPCUAServerNode;
  attr:UA_VariableAttributes;
  src:UA_CallbackValueSource;
  parent:UA_NodeId;
  st:UA_StatusCode;
  idText, folder, browse:AnsiString;
  p:Integer;
  dims:UA_UInt32;
begin
  if FNodes.ContainsKey(Cmd.Key) then RemoveNode(Cmd.Key);
  idText := Cmd.NodeId;
  browse := Cmd.BrowseName;
  p := LastDelimiter('.', idText);
  if p>0 then folder := Copy(idText, 1, p-1) else folder := '';
  if not EnsureFolder(folder) then exit;
  if folder='' then
    parent := FRootId
  else
    parent := UA_NODEID_STRING(FNs, folder);

  n := TOPCUAServerNode.Create;
  n.Key := Cmd.Key;
  n.NodeId := idText;
  n.TypeIndex := Cmd.TypeIndex;
  n.IsText := Cmd.IsText;
  n.IsArray := Cmd.IsArray;
  n.Values := Cmd.Values;
  n.Num := Cmd.Num;
  n.Text := Cmd.Text;
  n.Status := Cmd.Status;
  n.Time := UA_DateTimeFromDateTime(Cmd.TimeUTC);
  n.Thread := Self;

  attr := UA_VariableAttributes_default;
  attr.dataType := UA_TYPES[Cmd.TypeIndex].typeId;
  if Cmd.IsArray then begin
    attr.valueRank := UA_VALUERANK_ONE_DIMENSION;
    dims := Length(Cmd.Values);
    attr.arrayDimensions := @dims;
    attr.arrayDimensionsSize := 1;
  end else
    attr.valueRank := UA_VALUERANK_SCALAR;
  attr.displayName := _UA_LOCALIZEDTEXT_ALLOC('', Cmd.BrowseName);
  if Cmd.Description<>'' then
    attr.description := _UA_LOCALIZEDTEXT_ALLOC('', Cmd.Description);
  src.read := @NodeRead;
  if Cmd.Writable then begin
    attr.accessLevel := UA_ACCESSLEVELMASK_READ or UA_ACCESSLEVELMASK_WRITE;
    src.write := @NodeWrite;
  end else begin
    attr.accessLevel := UA_ACCESSLEVELMASK_READ;
    src.write := nil;
  end;
  attr.userAccessLevel := attr.accessLevel;
  st := UA_Server_addCallbackValueSourceVariableNode(FServer, UA_NODEID_STRING(FNs, idText), parent,
          UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES), _UA_QUALIFIEDNAME(FNs, browse),
          UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), attr, src, n, nil);
  UA_clear(@attr.displayName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  if Cmd.Description<>'' then
    UA_clear(@attr.description, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  if st<>UA_STATUSCODE_GOOD then begin
    n.Free;
    FOwner.SetError(Format('variable "%s": %s', [idText, OPCUAStatusText(st)]));
    exit;
  end;
  FNodes.Add(Cmd.Key, n);
end;

function TOPCUAServerThread.UserOf(SessionContext:Pointer):String;
var
  idx:PtrUInt;
begin
  idx := PtrUInt(SessionContext);
  if (idx>0) and (idx<=PtrUInt(FUserNames.Count)) then
    Result := FUserNames[idx-1]
  else
    Result := '';
end;

//main thread (Synchronize)
procedure TOPCUAServerThread.DoCheckUser;
begin
  if Assigned(FOwner.FOnCheckUser) then
    FOwner.FOnCheckUser(FOwner, FCheckUser, FCheckPassword, FCheckAccepted);
end;

function TOPCUAServerThread.CheckLogin(const User, Password:String):Boolean;
var
  i:Integer;
begin
  Result := false;
  for i:=0 to FUsers.Count-1 do
    if (FUsers.Names[i]=User) and (FUsers.ValueFromIndex[i]=Password) then begin
      Result := true;
      break;
    end;
  if FUseCheckUser then begin
    FCheckUser := User;
    FCheckPassword := Password;
    FCheckAccepted := Result;
    Synchronize(@DoCheckUser);
    Result := FCheckAccepted;
    FCheckPassword := '';
  end;
end;

procedure TOPCUAServerThread.SetupSecurity(Conf:PUA_ServerConfig);
var
  st:UA_StatusCode;
  dir, certFile, keyFile, ext:String;
  cert, key:RawByteString;
  certBS, keyBS:UA_ByteString;
  trust:array of RawByteString;
  trustBS:array of UA_ByteString;
  sr:TSearchRec;
  n, i:Integer;
  subject:array[0..2] of AnsiString;
  san:array[0..2] of AnsiString;
  subj:array[0..2] of UA_String;
  alt:array[0..2] of UA_String;
  outCert, outKey:UA_ByteString;
  json:RawByteString;
  dummy:UA_UsernamePasswordLogin;
  dummyName:AnsiString;
begin
  dummyName := '#pascalscada';
  if FSecurity=ossNone then begin
    st := UA_ServerConfig_setMinimal(Conf, FPort, nil);
    if st<>UA_STATUSCODE_GOOD then
      raise Exception.Create('server configuration: '+OPCUAStatusText(st));
  end else begin
    {$IF DECLARED(UA_ServerConfig_setDefaultWithSecurityPolicies)}
    //the certificate is kept, so that the clients need to trust it only once
    dir := IncludeTrailingPathDelimiter(FCertificateDir);
    certFile := dir+'server_cert.der';
    keyFile := dir+'server_key.der';
    if not (ReadFileBytes(certFile, cert) and ReadFileBytes(keyFile, key)) then begin
      subject[0] := 'C=BR';
      subject[1] := 'O=PascalSCADA';
      subject[2] := 'CN='+FServerName+'@'+HostName;
      //the application URI the server reports (the open62541 default)
      san[0] := 'URI:urn:open62541.server.application';
      san[1] := 'DNS:'+HostName;
      san[2] := 'DNS:localhost';
      for i:=0 to 2 do subj[i] := _UA_STRING(subject[i]);
      for i:=0 to 2 do alt[i] := _UA_STRING(san[i]);
      FillChar(outCert, SizeOf(outCert), 0);
      FillChar(outKey, SizeOf(outKey), 0);
      st := UA_CreateCertificate(@FLogger, @subj[0], 3, @alt[0], 3, UA_CERTIFICATEFORMAT_DER, nil, @outKey, @outCert);
      if st<>UA_STATUSCODE_GOOD then
        raise Exception.Create('server certificate: '+OPCUAStatusText(st));
      try
        SetString(cert, PAnsiChar(outCert.data), outCert.length);
        SetString(key, PAnsiChar(outKey.data), outKey.length);
      finally
        UA_clear(@outCert, @UA_TYPES[UA_TYPES_BYTESTRING]);
        UA_clear(@outKey, @UA_TYPES[UA_TYPES_BYTESTRING]);
      end;
      WriteFileBytes(certFile, cert);
      WriteFileBytes(keyFile, key);
    end;
    //accepted client certificates (none: any client)
    n := 0;
    trust := nil;
    if (FTrustListDir<>'') and (FindFirst(IncludeTrailingPathDelimiter(FTrustListDir)+'*', faAnyFile, sr)=0) then
      try
        repeat
          ext := LowerCase(ExtractFileExt(sr.Name));
          if ((sr.Attr and faDirectory)=0) and ((ext='.der') or (ext='.pem') or (ext='.crt') or (ext='.cer')) then begin
            SetLength(trust, n+1);
            if ReadFileBytes(IncludeTrailingPathDelimiter(FTrustListDir)+sr.Name, trust[n]) then
              Inc(n);
          end;
        until FindNext(sr)<>0;
      finally
        FindClose(sr);
      end;
    SetLength(trustBS, n);
    for i:=0 to n-1 do trustBS[i] := AsByteString(trust[i]);
    certBS := AsByteString(cert);
    keyBS := AsByteString(key);
    //ossSecureOnly keeps the None endpoint for the discovery only (below): without
    //it a client can not get the certificate of the server
    st := UA_ServerConfig_setDefaultWithSecurityPolicies(Conf, FPort, @certBS, @keyBS,
            PUA_ByteString(Pointer(trustBS)), n, nil, 0, nil, 0);
    if st<>UA_STATUSCODE_GOOD then
      raise Exception.Create('secure server configuration: '+OPCUAStatusText(st));
    {$ELSE}
    raise Exception.Create('this binding was built without encryption');
    {$IFEND}
  end;
  //what UA_ServerConfig fields this binding does not reach, through JSON
  json := '';
  if FSecurity=ossSecureOnly then
    json := '"securityPolicyNoneDiscoveryOnly": true';
  if FWriteTimeoutMs>0 then begin
    if json<>'' then json := json+', ';
    //an integer: open62541 reads a decimal point with the C locale of the process
    //(a pt-BR one takes a comma, and "500.0" would not be a number)
    json := json+Format('"asyncOperationTimeout": %d', [FWriteTimeoutMs]);
  end;
  if json<>'' then begin
    if not Assigned(UA_ServerConfig_updateFromFile) then
      raise Exception.Create('this libopen62541 was built without JSON: ossSecureOnly and WriteTimeoutMs are not available');
    json := '{'+json+'}';
    st := UA_ServerConfig_updateFromFile(Conf, AsByteString(json));
    if st<>UA_STATUSCODE_GOOD then
      raise Exception.Create('server configuration (JSON '+json+'): '+OPCUAStatusText(st));
  end;

  //logins: anonymous and/or users, through ServerLogin (it also keeps the session's user).
  //open62541 offers the username login only with a non empty list: ServerLogin checks it
  dummy.username := _UA_STRING(dummyName);
  dummy.password := _UA_STRING(dummyName);
  if (FUsers.Count>0) or FUseCheckUser then
    st := UA_AccessControl_defaultWithLoginCallback(Conf, FAllowAnonymous, nil, 1, @dummy, @ServerLogin, Self)
  else
    st := UA_AccessControl_defaultWithLoginCallback(Conf, FAllowAnonymous, nil, 0, nil, @ServerLogin, Self);
  if st<>UA_STATUSCODE_GOOD then
    raise Exception.Create('access control: '+OPCUAStatusText(st));
end;

function TOPCUAServerThread.BeginWrite(Node:TOPCUAServerNode; const Value:PUA_DataValue;
  const Range:PUA_NumericRange; SessionContext:Pointer):UA_StatusCode;
var
  req:TOPCUAServerWriteRequest;
  vals:TArrayOfDouble;
  txt:UTF8String;
  id:Int64;
  stale:Int64;
  pair:specialize TPair<Int64, PUA_DataValue>;
begin
  if (Value^.flags and UA_DATAVALUE_HASVALUE)=0 then
    exit(UA_STATUSCODE_BADTYPEMISMATCH);
  if Value^.value._type<>@UA_TYPES[Node.TypeIndex] then
    exit(UA_STATUSCODE_BADTYPEMISMATCH);
  req := Default(TOPCUAServerWriteRequest);
  if Node.IsArray then begin
    //the whole array (or its beginning), or the elements of an index range
    if OPCUAVariantToNumbers(Value^.value, vals)<>ioOk then exit(UA_STATUSCODE_BADTYPEMISMATCH);
    if (Range<>nil) and (Range^.dimensionsSize>0) then begin
      if (Range^.dimensionsSize<>1) or (Range^.dimensions^.min>Range^.dimensions^.max) then
        exit(UA_STATUSCODE_BADINDEXRANGEINVALID);
      if (Range^.dimensions^.max>=UA_UInt32(Length(Node.Values))) then
        exit(UA_STATUSCODE_BADINDEXRANGENODATA);
      if Length(vals)<>Range^.dimensions^.max-Range^.dimensions^.min+1 then
        exit(UA_STATUSCODE_BADTYPEMISMATCH);
      req.Offset := Range^.dimensions^.min;
    end else begin
      if UA_Variant_isScalar(@Value^.value) or (Length(vals)=0) or (Length(vals)>Length(Node.Values)) then
        exit(UA_STATUSCODE_BADTYPEMISMATCH);
      req.Offset := 0;
    end;
    req.Values := vals;
  end else if (Range<>nil) and (Range^.dimensionsSize>0) then
    exit(UA_STATUSCODE_BADINDEXRANGEINVALID)
  else if not UA_Variant_isScalar(@Value^.value) then
    //the variable has one value
    exit(UA_STATUSCODE_BADTYPEMISMATCH)
  else if Node.IsText then begin
    if OPCUAVariantToText(Value^.value, txt)<>ioOk then exit(UA_STATUSCODE_BADTYPEMISMATCH);
    req.Text := txt;
  end else begin
    if (OPCUAVariantToNumbers(Value^.value, vals)<>ioOk) or (Length(vals)<>1) then
      exit(UA_STATUSCODE_BADTYPEMISMATCH);
    req.Num := vals[0];
  end;
  //an operation cancelled by the server (timeout, session closed) is not told to
  //us: if its memory came back for this one, the old entry is dead
  stale := 0;
  for pair in FPending do
    if pair.Value=Value then begin
      stale := pair.Key;
      break;
    end;
  if stale<>0 then FPending.Remove(stale);
  Inc(FWriteSeq);
  id := FWriteSeq;
  FPending.Add(id, Value);
  req.Id := id;
  req.Key := Node.Key;
  req.User := UserOf(SessionContext);
  FOwner.QueueWrite(req);
  Result := UA_STATUSCODE_GOODCOMPLETESASYNCHRONOUSLY;
end;

procedure TOPCUAServerThread.EndWrite(Id:Int64; Status:UA_StatusCode);
var
  v:PUA_DataValue;
begin
  if not FPending.TryGetValue(Id, v) then exit;
  FPending.Remove(Id);
  //BADNOTFOUND when the server gave up on it meanwhile: nothing to do
  UA_Server_setAsyncWriteResult(FServer, v, Status);
end;

//An object node (a structure) at Cmd.NodeId: its items are variables under it.
procedure TOPCUAServerThread.AddObject(const Cmd:TOPCUAServerCmd);
var
  attr:UA_ObjectAttributes;
  st:UA_StatusCode;
  parent:UA_NodeId;
  idText, folder, browse:AnsiString;
  p:Integer;
begin
  idText := Cmd.NodeId;
  browse := Cmd.BrowseName;
  if FFolders.IndexOf(idText)>=0 then exit;
  p := LastDelimiter('.', idText);
  if p>0 then folder := Copy(idText, 1, p-1) else folder := '';
  if not EnsureFolder(folder) then exit;
  if folder='' then parent := FRootId else parent := UA_NODEID_STRING(FNs, folder);
  attr := UA_ObjectAttributes_default;
  attr.displayName := _UA_LOCALIZEDTEXT_ALLOC('', Cmd.BrowseName);
  if Cmd.Description<>'' then
    attr.description := _UA_LOCALIZEDTEXT_ALLOC('', Cmd.Description);
  st := UA_Server_addObjectNode(FServer, UA_NODEID_STRING(FNs, idText), parent,
          UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES), _UA_QUALIFIEDNAME(FNs, browse),
          UA_NODEID_NUMERIC(0, UA_NS0ID_BASEOBJECTTYPE), attr, nil, nil);
  UA_clear(@attr.displayName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  if Cmd.Description<>'' then
    UA_clear(@attr.description, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  if st<>UA_STATUSCODE_GOOD then begin
    FOwner.SetError(Format('object "%s": %s', [idText, OPCUAStatusText(st)]));
    exit;
  end;
  //its items go under it as if it were a folder
  FFolders.Add(idText);
end;

procedure TOPCUAServerThread.RemoveObject(const Cmd:TOPCUAServerCmd);
var
  idText:AnsiString;
  idx:Integer;
begin
  idText := Cmd.NodeId;
  idx := FFolders.IndexOf(idText);
  if idx<0 then exit;
  FFolders.Delete(idx);
  UA_Server_deleteNode(FServer, UA_NODEID_STRING(FNs, idText), true);
end;

procedure TOPCUAServerThread.RemoveNode(Key:Integer);
var
  n:TOPCUAServerNode;
  idText:AnsiString;
begin
  if not FNodes.TryGetValue(Key, n) then exit;
  idText := n.NodeId;
  UA_Server_deleteNode(FServer, UA_NODEID_STRING(FNs, idText), true);
  //no callback can come for it after deleteNode (same thread)
  FNodes.Remove(Key);
  n.Free;
end;

procedure TOPCUAServerThread.Setup;
var
  conf:PUA_ServerConfig;
  st:UA_StatusCode;
  attr:UA_ObjectAttributes;
begin
  FServer := UA_Server_new();
  if FServer=nil then raise Exception.Create('UA_Server_new failed');
  conf := UA_Server_getConfig(FServer);
  SetupSecurity(conf);
  //the default logger is shared (event loop, PKI...): it is not replaced, its
  //log function and context are; its memory is still released by the library
  if conf^.logging<>nil then begin
    conf^.logging^.log := FLogger.log;
    conf^.logging^.context := FLogger.context;
  end;
  FNs := UA_Server_addNamespace(FServer, PAnsiChar(FNamespaceUri));
  //root folder of the published tags, under Objects
  FRootId := UA_NODEID_NUMERIC(FNs, 1);
  attr := UA_ObjectAttributes_default;
  attr.displayName := _UA_LOCALIZEDTEXT_ALLOC('', FServerName);
  st := UA_Server_addObjectNode(FServer, FRootId, UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER),
          UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES), _UA_QUALIFIEDNAME(FNs, FServerName),
          UA_NODEID_NUMERIC(0, UA_NS0ID_FOLDERTYPE), attr, nil, nil);
  UA_clear(@attr.displayName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
  if st<>UA_STATUSCODE_GOOD then
    raise Exception.Create('root folder: '+OPCUAStatusText(st));
  st := UA_Server_run_startup(FServer);
  if st<>UA_STATUSCODE_GOOD then
    raise Exception.Create(Format('startup on port %d: %s', [FPort, OPCUAStatusText(st)]));
end;

procedure TOPCUAServerThread.TearDown;
var
  n:TOPCUAServerNode;
begin
  if FServer=nil then exit;
  UA_Server_run_shutdown(FServer);
  UA_Server_delete(FServer);
  FServer := nil;
  for n in FNodes.Values do n.Free;
  FNodes.Clear;
end;

procedure TOPCUAServerThread.Execute;
begin
  OPCUAMaskFPU; //per thread; this thread only calls the library
  try
    try
      Setup;
    except
      on E:Exception do begin
        FStartError := E.Message;
        TearDown;
        FStarted.SetEvent;
        exit;
      end;
    end;
    FStarted.SetEvent;
    while not Terminated do begin
      ApplyCmds;
      UA_Server_run_iterate(FServer, false);
      FWake.WaitFor(5);
    end;
  finally
    TearDown;
  end;
end;

{ TOPCUAServer }

constructor TOPCUAServer.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FPort := 4840;
  FServerName := 'PascalSCADA';
  FNamespaceUri := 'urn:pascalscada:tags';
  FLogLevel := oslWarning;
  FErrorLock := TCriticalSection.Create;
  FTagItems := TOPCUAServerTagMap.Create;
  FWriteLock := TCriticalSection.Create;
  FPendingWrites := TFPList.Create;
  FItems := TOPCUAServerItems.Create(Self);
  FAutoItems := TOPCUAServerItems.Create(Self);
  FAutoItems.FInternal := true;
  FAllowAnonymous := true;
  FUsers := TStringList.Create;
  FCertificateDir := IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'opcua-server-pki';
end;

destructor TOPCUAServer.Destroy;
var
  l:TFPList;
begin
  Destroying;
  StopServer;
  TThread.RemoveQueuedEvents(@DeferredStart);
  FreeAndNil(FAutoItems);
  FreeAndNil(FItems);
  FreeAndNil(FUsers);
  for l in FTagItems.Values do l.Free;
  FreeAndNil(FTagItems);
  FreeAndNil(FPendingWrites);
  FreeAndNil(FWriteLock);
  FreeAndNil(FErrorLock);
  inherited Destroy;
end;

procedure TOPCUAServer.Loaded;
begin
  inherited Loaded;
  if FActive then begin
    //the tags of the form get their driver (and word size) while it loads: start
    //once everything was loaded
    FStartQueued := true;
    TThread.Queue(nil, @DeferredStart);
  end;
end;

procedure TOPCUAServer.DeferredStart;
begin
  FStartQueued := false;
  if FActive and (FThread=nil) and ([csDestroying, csDesigning]*ComponentState=[]) then
    StartServer;
end;

procedure TOPCUAServer.Notification(AComponent:TComponent; Operation:TOperation);
var
  i:Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent is TPLCTag) and (FPendingWrites<>nil) then
    FailPendingWrites(0, TPLCTag(AComponent), UA_STATUSCODE_BADCONFIGURATIONERROR);
  if Operation=opRemove then
    ForEachItem(AComponent, 0);
end;

{$IFDEF PORTUGUES}
//: Action 0: os itens do tag o perdem. 1: os itens que tiram o status dele sao publicados de novo.
{$ELSE}
//: Action 0: the items of the tag lose it. 1: the items that take their status from it are published again.
{$ENDIF}
procedure TOPCUAServer.ForEachItem(ATag:TComponent; Action:Integer);
  procedure Walk(Items:TOPCUAServerItems);
  var
    i:Integer;
    it:TOPCUAServerItem;
  begin
    if Items=nil then exit;
    i := 0;
    while i<Items.Count do begin
      it := Items[i];
      if (Action=0) and (it.FTag=ATag) then
        it.Tag := nil
      else if (Action=1) and (it.FStatusTag=ATag) and (it.FTag<>ATag) then begin
        UnpublishItem(it);
        PublishItem(it);
      end;
      //removing a structure frees its items: the list may have shrunk
      if (i<Items.Count) and (Items[i]=it) then Inc(i);
    end;
  end;
begin
  Walk(FItems);
  Walk(FAutoItems);
end;

function TOPCUAServer.GetLastError:String;
begin
  FErrorLock.Enter;
  try
    Result := FLastError;
  finally
    FErrorLock.Leave;
  end;
end;

function TOPCUAServer.GetRunning:Boolean;
begin
  Result := FThread<>nil;
end;

procedure TOPCUAServer.SetError(const Msg:String);
begin
  FErrorLock.Enter;
  try
    FLastError := Msg;
  finally
    FErrorLock.Leave;
  end;
  if PSLogWants(lcProtocol, llError) then
    PSLog(Self, lcProtocol, llError, Msg);
end;

procedure TOPCUAServer.DoInactiveCheck;
begin
  if Running then
    raise Exception.Create('Deactivate the OPC UA server to change this property');
end;

procedure TOPCUAServer.SetActive(AValue:Boolean);
begin
  if FActive=AValue then exit;
  FActive := AValue;
  if [csLoading, csReading, csDesigning]*ComponentState<>[] then exit;
  if FActive then begin
    StartServer;
    if FThread=nil then FActive := false;
  end else
    StopServer;
end;

function TOPCUAServer.GetUsers:TStrings;
begin
  Result := FUsers;
end;

procedure TOPCUAServer.SetUsers(AValue:TStrings);
begin
  DoInactiveCheck;
  FUsers.Assign(AValue);
end;

procedure TOPCUAServer.SetSecurity(AValue:TOPCUAServerSecurity);
begin
  if FSecurity=AValue then exit;
  DoInactiveCheck;
  FSecurity := AValue;
end;

procedure TOPCUAServer.SetAllowAnonymous(AValue:Boolean);
begin
  if FAllowAnonymous=AValue then exit;
  DoInactiveCheck;
  FAllowAnonymous := AValue;
end;

procedure TOPCUAServer.SetCertificateDir(const AValue:String);
begin
  if FCertificateDir=AValue then exit;
  DoInactiveCheck;
  FCertificateDir := AValue;
end;

procedure TOPCUAServer.SetTrustListDir(const AValue:String);
begin
  if FTrustListDir=AValue then exit;
  DoInactiveCheck;
  FTrustListDir := AValue;
end;

procedure TOPCUAServer.SetItems(AValue:TOPCUAServerItems);
begin
  FItems.Assign(AValue);
end;

procedure TOPCUAServer.SetLibraryName(const AValue:String);
begin
  if FLibraryName=AValue then exit;
  DoInactiveCheck;
  FLibraryName := AValue;
end;

procedure TOPCUAServer.SetNamespaceUri(const AValue:String);
begin
  if FNamespaceUri=AValue then exit;
  DoInactiveCheck;
  FNamespaceUri := AValue;
end;

procedure TOPCUAServer.SetPort(AValue:Word);
begin
  if FPort=AValue then exit;
  DoInactiveCheck;
  FPort := AValue;
end;

procedure TOPCUAServer.SetServerName(const AValue:String);
begin
  if FServerName=AValue then exit;
  DoInactiveCheck;
  FServerName := AValue;
end;

procedure TOPCUAServer.StartServer;
var
  err:String;
  i:Integer;
  t:TOPCUAServerThread;
begin
  if FThread<>nil then exit;
  SetError('');
  //open62541 takes a password only encrypted with the server certificate
  if (FSecurity=ossNone) and ((FUsers.Count>0) or Assigned(FOnCheckUser)) then begin
    SetError('OPC UA server: user logins need Security ossNoneAndSecure or ossSecureOnly '+
             '(the password travels encrypted with the server certificate)');
    exit;
  end;
  if not OPCUALoadLibrary(FLibraryName, err) then begin
    SetError(err);
    exit;
  end;
  t := TOPCUAServerThread.Create(Self);
  if (not t.WaitStarted(StartTimeoutMs)) or (t.StartError<>'') then begin
    if t.StartError<>'' then err := t.StartError else err := 'the server did not start in time';
    t.Terminate;
    t.WaitFor;
    t.Free;
    SetError('OPC UA server: '+err);
    exit;
  end;
  FThread := t;
  FNamespaceIndex := t.FNs;
  if PSLogWants(lcConnection, llInfo) then
    PSLog(Self, lcConnection, llInfo, Format('OPC UA server started on port %d', [FPort]));
  for i:=0 to FItems.Count-1 do
    PublishItem(FItems[i]);
end;

procedure TOPCUAServer.StopServer;
var
  i:Integer;
begin
  if FThread=nil then exit;
  for i:=0 to FItems.Count-1 do
    UnpublishItem(FItems[i]);
  for i:=FAutoItems.Count-1 downto 0 do
    FAutoItems[i].Free;
  //the server cancels what is still waiting when it shuts down
  TThread.RemoveQueuedEvents(@ProcessWrites);
  FWriteLock.Enter;
  try
    FWriteQueue := nil;
    FWriteQueueCount := 0;
    FWriteQueued := false;
  finally
    FWriteLock.Leave;
  end;
  FailPendingWrites(0, nil, UA_STATUSCODE_BADSHUTDOWN);
  FThread.Terminate;
  FThread.FWake.SetEvent;
  FThread.WaitFor;
  FreeAndNil(FThread);
  FNamespaceIndex := 0;
  if PSLogWants(lcConnection, llInfo) then
    PSLog(Self, lcConnection, llInfo, 'OPC UA server stopped');
end;

procedure TOPCUAServer.LinkTag(ATag:TPLCTag; Item:TOPCUAServerItem);
var
  l:TFPList;
begin
  if ATag=nil then exit;
  if not FTagItems.TryGetValue(ATag, l) then begin
    l := TFPList.Create;
    FTagItems.Add(ATag, l);
    ATag.AddTagChangeHandler(@TagChanged);
    ATag.AddReadOkHandler(@TagReadDone);
    ATag.AddReadFaultHandler(@TagReadDone);
    ATag.AddRemoveTagHandler(@TagRemoved);
  end;
  if l.IndexOf(Item)<0 then l.Add(Item);
end;

procedure TOPCUAServer.UnlinkTag(ATag:TPLCTag; Item:TOPCUAServerItem);
var
  l:TFPList;
begin
  if (ATag=nil) or not FTagItems.TryGetValue(ATag, l) then exit;
  l.Remove(Item);
  if l.Count=0 then begin
    FTagItems.Remove(ATag);
    l.Free;
    if not (csDestroying in ATag.ComponentState) then
      ATag.RemoveAllHandlersFromObject(Self);
  end;
end;

procedure TOPCUAServer.PublishItem(Item:TOPCUAServerItem);
var
  cmd:TOPCUAServerCmd;
  why:String;
begin
  if (FThread=nil) or Item.Published or (Item.Tag=nil) then exit;
  if Item.NodeIdText='' then exit;
  if not ItemType(Item, Item.FTypeIndex, Item.FIsText, why) then begin
    SetError(Format('item "%s": %s', [Item.NodeIdText, why]));
    exit;
  end;
  Inc(FNextKey);
  Item.FKey := FNextKey;
  Item.FPublishedId := Item.NodeIdText;
  if Item.FIsObject then begin
    //a structure: an object, and a variable for each of its items
    LinkTag(Item.Tag, Item);
    cmd := Default(TOPCUAServerCmd);
    cmd.Kind := sckAddObject;
    cmd.NodeId := Item.FPublishedId;
    cmd.BrowseName := Item.EffectiveNodeName;
    cmd.Description := Item.Description;
    FThread.Post(cmd);
    PublishStructItems(Item);
    exit;
  end;
  Item.FStatusTag := StatusTagOf(Item.Tag);
  //an expression has no device to write to
  Item.FCanWrite := Item.Writable and not (Item.Tag is TNumericExprTag);
  LinkTag(Item.Tag, Item);
  if Item.FStatusTag<>Item.Tag then
    LinkTag(Item.FStatusTag, Item);

  cmd := Default(TOPCUAServerCmd);
  cmd.Kind := sckAdd;
  cmd.Key := Item.FKey;
  cmd.NodeId := Item.NodeIdText;
  cmd.BrowseName := Item.EffectiveNodeName;
  cmd.Description := Item.Description;
  cmd.TypeIndex := Item.FTypeIndex;
  cmd.IsText := Item.FIsText;
  cmd.IsArray := Item.FIsArray;
  if Item.FIsArray then
    cmd.Values := Copy(TPLCBlock(Item.Tag).ValuesRaw); //the size of the array
  cmd.Writable := Item.FCanWrite;
  FThread.Post(cmd);
  PostValue(Item);
end;

//the items of a structure, among the components of its owner (where the Tag Builder puts them)
procedure TOPCUAServer.PublishStructItems(Item:TOPCUAServerItem);
var
  S:TPLCStruct;
  O:TComponent;
  found:TFPList;
  i, j:Integer;
  C:TComponent;
  child:TOPCUAServerItem;

  function IndexOf(X:TComponent):Cardinal;
  begin
    if X is TPLCStructItem then Result := TPLCStructItem(X).Index
    else Result := TPLCStructString(X).Index;
  end;

begin
  S := TPLCStruct(Item.Tag);
  O := S.Owner;
  if O=nil then exit;
  found := TFPList.Create;
  try
    for i:=0 to O.ComponentCount-1 do begin
      C := O.Components[i];
      if ((C is TPLCStructItem) and (TPLCStructItem(C).PLCBlock=S)) or
         ((C is TPLCStructString) and (TPLCStructString(C).PLCBlock=S)) then begin
        //in the order of the structure
        j := found.Count;
        while (j>0) and (IndexOf(TComponent(found[j-1]))>IndexOf(C)) do Dec(j);
        found.Insert(j, C);
      end;
    end;
    for i:=0 to found.Count-1 do begin
      C := TComponent(found[i]);
      child := FAutoItems.Add;
      child.FTag := TPLCTag(C);
      C.FreeNotification(Self);
      child.FNodeName := C.Name;
      child.FFolder := StringReplace(Item.FPublishedId, '.', '/', [rfReplaceAll]);
      child.FWritable := Item.Writable;
      child.FParentKey := Item.FKey;
      PublishItem(child);
    end;
  finally
    found.Free;
  end;
end;

procedure TOPCUAServer.UnpublishStructItems(Item:TOPCUAServerItem);
var
  i:Integer;
begin
  for i:=FAutoItems.Count-1 downto 0 do
    if (i<FAutoItems.Count) and (FAutoItems[i].FParentKey=Item.FKey) then
      FAutoItems[i].Free;
end;

procedure TOPCUAServer.UnpublishItem(Item:TOPCUAServerItem);
var
  cmd:TOPCUAServerCmd;
  key:Integer;
begin
  if not Item.Published then exit;
  key := Item.FKey;
  if Item.FIsObject then begin
    UnpublishStructItems(Item);
    Item.FKey := 0;
    UnlinkTag(Item.Tag, Item);
    if FThread<>nil then begin
      cmd := Default(TOPCUAServerCmd);
      cmd.Kind := sckRemoveObject;
      cmd.NodeId := Item.FPublishedId;
      FThread.Post(cmd);
    end;
    exit;
  end;
  FailPendingWrites(key, nil, UA_STATUSCODE_BADNODEIDUNKNOWN);
  Item.FKey := 0;
  UnlinkTag(Item.Tag, Item);
  if (Item.FStatusTag<>nil) and (Item.FStatusTag<>Item.Tag) then
    UnlinkTag(Item.FStatusTag, Item);
  Item.FStatusTag := nil;
  if FThread<>nil then begin
    cmd := Default(TOPCUAServerCmd);
    cmd.Kind := sckRemove;
    cmd.Key := key;
    FThread.Post(cmd);
  end;
end;

procedure TOPCUAServer.PostValue(Item:TOPCUAServerItem);
var
  cmd:TOPCUAServerCmd;
  T:TPLCTag;
begin
  if (FThread=nil) or (not Item.Published) or Item.FIsObject then exit;
  T := Item.Tag;
  cmd := Default(TOPCUAServerCmd);
  cmd.Kind := sckValue;
  cmd.Key := Item.FKey;
  cmd.Status := TagStatus(Item);
  cmd.TimeUTC := LocalTimeToUniversal(Now);
  try
    if Item.FIsArray then
      cmd.Values := Copy(TPLCBlock(T).ValuesRaw)
    else if Item.FIsText then
      cmd.Text := (T as ITagString).Value
    else if (Item.DataType=odtScaledDouble) or (T is TNumericExprTag) then
      cmd.Num := TPLCNumber(T).Value
    else
      cmd.Num := TPLCNumber(T).ValueRaw;
  except
    on E:Exception do begin
      cmd.Num := NaN;
      cmd.Status := UA_STATUSCODE_BADINTERNALERROR;
    end;
  end;
  Item.FLastStatus := cmd.Status;
  FThread.Post(cmd);
end;

procedure TOPCUAServer.TagChanged(Sender:TObject);
var
  l:TFPList;
  i:Integer;
begin
  if (FThread=nil) or not FTagItems.TryGetValue(TPLCTag(Sender), l) then exit;
  for i:=0 to l.Count-1 do
    PostValue(TOPCUAServerItem(l[i]));
end;

procedure TOPCUAServer.TagReadDone(Sender:TObject);
var
  l:TFPList;
  i:Integer;
  it:TOPCUAServerItem;
begin
  //every read: only a change of the quality is news (a new value comes by TagChanged)
  if (FThread=nil) or not FTagItems.TryGetValue(TPLCTag(Sender), l) then exit;
  for i:=0 to l.Count-1 do begin
    it := TOPCUAServerItem(l[i]);
    if TagStatus(it)<>it.FLastStatus then
      PostValue(it);
  end;
end;

procedure TOPCUAServer.TagRemoved(Sender:TObject);
begin
  //the tag is being destroyed: its items lose it (and their variables)
  ForEachItem(TComponent(Sender), 0);
  ForEachItem(TComponent(Sender), 1);
end;

function TOPCUAServer.ItemByKey(Key:Integer):TOPCUAServerItem;
var
  i:Integer;
begin
  for i:=0 to FItems.Count-1 do
    if FItems[i].FKey=Key then exit(FItems[i]);
  for i:=0 to FAutoItems.Count-1 do
    if FAutoItems[i].FKey=Key then exit(FAutoItems[i]);
  Result := nil;
end;

//server thread
procedure TOPCUAServer.QueueWrite(const Req:TOPCUAServerWriteRequest);
var
  wake:Boolean;
begin
  FWriteLock.Enter;
  try
    if FWriteQueueCount>=Length(FWriteQueue) then
      SetLength(FWriteQueue, 16+2*FWriteQueueCount);
    FWriteQueue[FWriteQueueCount] := Req;
    Inc(FWriteQueueCount);
    wake := not FWriteQueued;
    FWriteQueued := true;
  finally
    FWriteLock.Leave;
  end;
  if wake then
    TThread.Queue(nil, @ProcessWrites);
end;

//main thread
procedure TOPCUAServer.ProcessWrites;
var
  reqs:array of TOPCUAServerWriteRequest;
  count, i:Integer;
  it:TOPCUAServerItem;
  st:UA_StatusCode;
  cmd:TOPCUAServerCmd;
begin
  FWriteLock.Enter;
  try
    reqs := FWriteQueue;
    count := FWriteQueueCount;
    FWriteQueue := nil;
    FWriteQueueCount := 0;
    FWriteQueued := false;
  finally
    FWriteLock.Leave;
  end;
  for i:=0 to count-1 do begin
    if FThread=nil then exit;
    it := ItemByKey(reqs[i].Key);
    if (it=nil) or (it.Tag=nil) then
      st := UA_STATUSCODE_BADNODEIDUNKNOWN
    else
      try
        st := StartWrite(it, reqs[i]);
      except
        on E:Exception do begin
          SetError(Format('write to "%s": %s', [it.NodeIdText, E.Message]));
          st := UA_STATUSCODE_BADINTERNALERROR;
        end;
      end;
    if st=UA_STATUSCODE_GOODCOMPLETESASYNCHRONOUSLY then continue; //the tag answers later
    cmd := Default(TOPCUAServerCmd);
    cmd.Kind := sckWriteResult;
    cmd.WriteId := reqs[i].Id;
    cmd.Status := st;
    FThread.Post(cmd);
  end;
end;

//engineering value of a raw value, and back (through the scale processor)
function RawToEng(T:TPLCNumber; Raw:Double):Double;
begin
  if TPLCNumberAccess(T).PScaleProcessor<>nil then
    Result := TPLCNumberAccess(T).PScaleProcessor.SetInGetOut(T, Raw)
  else
    Result := Raw;
end;

function EngToRaw(T:TPLCNumber; Eng:Double):Double;
begin
  if TPLCNumberAccess(T).PScaleProcessor<>nil then
    Result := TPLCNumberAccess(T).PScaleProcessor.SetOutGetIn(T, Eng)
  else
    Result := Eng;
end;

function OutOfLimits(T:TPLCNumber; Eng:Double):Boolean;
begin
  Result := (TPLCNumberAccess(T).FEnableMin and (Eng<TPLCNumberAccess(T).FMinLimit)) or
            (TPLCNumberAccess(T).FEnableMax and (Eng>TPLCNumberAccess(T).FMaxLimit));
end;

{$IFDEF PORTUGUES}
{:
Comeca a escrita de um cliente no tag do item: converte o valor para o que o
tag recebe e pede um ScanWrite ao tag que fala com o driver. Devolve o status
final, ou GOODCOMPLETESASYNCHRONOUSLY quando o resultado vem depois.
}
{$ELSE}
{:
Starts the write of a client to the item's tag: converts the value to what the
tag takes and asks a ScanWrite of the tag that talks to the driver. Returns the
final status, or GOODCOMPLETESASYNCHRONOUSLY when the result comes later.
}
{$ENDIF}
function TOPCUAServer.StartWrite(Item:TOPCUAServerItem; const Req:TOPCUAServerWriteRequest):UA_StatusCode;
var
  T, target, resultTag:TPLCTag;
  src:TPLCNumber;
  vals:TArrayOfDouble;
  raw, eng, base:Double;
  mask, nv:QWord;
  bits, offset:Integer;
  P:TOPCUAServerPendingWrite;
  id:Int64;
  one:TArrayOfDouble;
  allowed:Boolean;
begin
  T := Item.Tag;
  if not Item.FCanWrite then exit(UA_STATUSCODE_BADNOTWRITABLE);
  if Assigned(FOnCanWrite) then begin
    allowed := true;
    FOnCanWrite(Self, Item, Req.User, allowed);
    if not allowed then begin
      if PSLogWants(lcSecurity, llWarning) then
        if Req.User='' then
          PSLog(Self, lcSecurity, llWarning, Format('OPC UA write to "%s" refused (anonymous)', [Item.NodeIdText]))
        else
          PSLog(Self, lcSecurity, llWarning, Format('OPC UA write to "%s" refused to %s', [Item.NodeIdText, Req.User]));
      exit(UA_STATUSCODE_BADUSERACCESSDENIED);
    end;
  end;
  offset := 0;

  if Item.FIsArray then begin
    //elements of a block (from Offset on); the block converts them
    vals := Req.Values;
    offset := Req.Offset;
    target := T;
  end else if T is TPLCStructString then begin
    //its bytes inside the structure
    if TPLCStructString(T).PLCBlock=nil then exit(UA_STATUSCODE_BADCONFIGURATIONERROR);
    if not TPLCStructString(T).EncodeValue(Req.Text, vals) then exit(UA_STATUSCODE_BADCONFIGURATIONERROR);
    target := TPLCStructString(T).PLCBlock;
    offset := TPLCStructString(T).Index;
  end else if T is TPLCString then begin
    vals := TPLCString(T).StringToArrayOfValues(Req.Text);
    target := T;
  end else if T is TPLCStructItem then begin
    //encoded into the bytes of the structure by the item's own type
    if TPLCStructItem(T).PLCBlock=nil then exit(UA_STATUSCODE_BADCONFIGURATIONERROR);
    if Item.DataType=odtScaledDouble then begin
      eng := Req.Num;
      raw := EngToRaw(TPLCNumber(T), eng);
    end else begin
      raw := Req.Num;
      eng := RawToEng(TPLCNumber(T), raw);
    end;
    if OutOfLimits(TPLCNumber(T), eng) then exit(UA_STATUSCODE_BADOUTOFRANGE);
    SetLength(one, 1);
    one[0] := raw;
    vals := TPLCTagAccess(T).TagValuesToPLCValues(one, 0);
    target := TPLCStructItem(T).PLCBlock;
    offset := TPLCStructItem(T).Index;
  end else if T is TTagBit then begin
    //the bits go into the word of the source, which is what gets written
    src := TTagBit(T).PLCTag;
    if src=nil then exit(UA_STATUSCODE_BADCONFIGURATIONERROR);
    bits := TTagBit(T).EndBit-TTagBit(T).StartBit+1;
    if bits>=64 then mask := High(QWord) else mask := (QWord(1) shl bits)-1;
    if (Req.Num<0) or (Req.Num>mask) then exit(UA_STATUSCODE_BADOUTOFRANGE);
    mask := mask shl TTagBit(T).StartBit;
    if TTagBit(T).UseRawValue then base := src.ValueRaw else base := src.Value;
    nv := (QWord(Trunc(base)) and not mask) or ((QWord(Trunc(Req.Num)) shl TTagBit(T).StartBit) and mask);
    if TTagBit(T).UseRawValue then
      raw := nv
    else begin
      if OutOfLimits(src, nv) then exit(UA_STATUSCODE_BADOUTOFRANGE);
      raw := EngToRaw(src, nv);
    end;
    SetLength(vals, 1);
    vals[0] := raw;
    target := src;
  end else if T is TPLCNumber then begin
    if Item.DataType=odtScaledDouble then begin
      eng := Req.Num;
      raw := EngToRaw(TPLCNumber(T), eng);
    end else begin
      raw := Req.Num;
      eng := RawToEng(TPLCNumber(T), raw);
    end;
    if OutOfLimits(TPLCNumber(T), eng) then exit(UA_STATUSCODE_BADOUTOFRANGE);
    SetLength(vals, 1);
    vals[0] := raw;
    target := T;
  end else
    exit(UA_STATUSCODE_BADNOTWRITABLE);

  //the result comes on the tag that issues the driver write: the block of an element
  resultTag := StatusTagOf(target);
  if (target is TTagBit) or (resultTag=nil) then
    exit(UA_STATUSCODE_BADCONFIGURATIONERROR);

  P := TOPCUAServerPendingWrite.Create;
  P.Id := Req.Id;
  P.Key := Item.FKey;
  P.ResultTag := resultTag;
  P.Num := Req.Num;
  P.Text := Req.Text;
  P.Values := Req.Values;
  P.Offset := Req.Offset;
  resultTag.AddCommandResultHandler(@TagCommandResult);
  resultTag.FreeNotification(Self);
  //a tag without driver (or a refused write) answers inside ScanWrite
  FCurrentWrite := P;
  try
    id := TPLCTagAccess(target).ScanWrite(vals, Length(vals), offset, true);
  finally
    FCurrentWrite := nil;
  end;
  if P.Done then begin
    Result := OPCUAServerStatusOf(P.Res);
    if Result=UA_STATUSCODE_GOOD then begin
      //the written value, ahead of the change notification
      FPendingWrites.Add(P);
      CompleteWrite(P, Result);
      exit(UA_STATUSCODE_GOODCOMPLETESASYNCHRONOUSLY); //CompleteWrite posted the result
    end;
    P.Free;
    ReleaseResultTag(resultTag);
    exit;
  end;
  if id<=0 then begin
    P.Free;
    ReleaseResultTag(resultTag);
    exit(UA_STATUSCODE_BADINTERNALERROR);
  end;
  P.ReqID := id;
  FPendingWrites.Add(P);
  Result := UA_STATUSCODE_GOODCOMPLETESASYNCHRONOUSLY;
end;

procedure TOPCUAServer.TagCommandResult(Sender:TPLCTag; TagCommand:TTagCommand; ReqID:LongWord;
  LastResult:TProtocolIOResult);
var
  i:Integer;
  P:TOPCUAServerPendingWrite;
begin
  if not (TagCommand in [tcScanWrite, tcWrite]) then exit;
  if (FCurrentWrite<>nil) and (FCurrentWrite.ResultTag=Sender) and not FCurrentWrite.Done then begin
    FCurrentWrite.Done := true;
    FCurrentWrite.Res := LastResult;
    exit;
  end;
  for i:=0 to FPendingWrites.Count-1 do begin
    P := TOPCUAServerPendingWrite(FPendingWrites[i]);
    if (P.ResultTag=Sender) and (P.ReqID=ReqID) then begin
      CompleteWrite(P, OPCUAServerStatusOf(LastResult));
      exit;
    end;
  end;
end;

{$IFDEF PORTUGUES}
//: Entrega o resultado de uma escrita pendente (e, se deu certo, o valor escrito) e a descarta.
{$ELSE}
//: Hands the result of a pending write (and, when good, the written value) over and drops it.
{$ENDIF}
procedure TOPCUAServer.CompleteWrite(P:TOPCUAServerPendingWrite; Status:UA_StatusCode);
var
  cmd:TOPCUAServerCmd;
  it:TOPCUAServerItem;
  tg:TPLCTag;
  i:Integer;
begin
  FPendingWrites.Remove(P);
  tg := P.ResultTag;
  if FThread<>nil then begin
    it := ItemByKey(P.Key);
    if (Status=UA_STATUSCODE_GOOD) and (it<>nil) then begin
      //the client that wrote reads its value back right away: the mirror gets it
      //before the result (the tag tells its change a moment later)
      cmd := Default(TOPCUAServerCmd);
      cmd.Kind := sckValue;
      cmd.Key := P.Key;
      cmd.Num := P.Num;
      cmd.Text := P.Text;
      if it.FIsArray and (it.Tag is TPLCBlock) then begin
        //the block (not updated yet) with the elements written
        cmd.Values := Copy(TPLCBlock(it.Tag).ValuesRaw);
        for i:=0 to High(P.Values) do
          if P.Offset+i<=High(cmd.Values) then
            cmd.Values[P.Offset+i] := P.Values[i];
      end;
      cmd.Status := UA_STATUSCODE_GOOD;
      cmd.TimeUTC := LocalTimeToUniversal(Now);
      it.FLastStatus := UA_STATUSCODE_GOOD;
      FThread.Post(cmd);
    end;
    cmd := Default(TOPCUAServerCmd);
    cmd.Kind := sckWriteResult;
    cmd.WriteId := P.Id;
    cmd.Status := Status;
    FThread.Post(cmd);
  end;
  P.Free;
  ReleaseResultTag(tg);
end;

procedure TOPCUAServer.FailPendingWrites(Key:Integer; ResultTag:TPLCTag; Status:UA_StatusCode);
var
  i:Integer;
  P:TOPCUAServerPendingWrite;
begin
  for i:=FPendingWrites.Count-1 downto 0 do begin
    if i>=FPendingWrites.Count then continue;
    P := TOPCUAServerPendingWrite(FPendingWrites[i]);
    if ((Key=0) and (ResultTag=nil)) or ((Key<>0) and (P.Key=Key)) or
       ((ResultTag<>nil) and (P.ResultTag=ResultTag)) then begin
      if ResultTag<>nil then P.ResultTag := nil; //being destroyed: not touched again
      CompleteWrite(P, Status);
    end;
  end;
end;

procedure TOPCUAServer.ReleaseResultTag(ATag:TPLCTag);
var
  i:Integer;
begin
  if ATag=nil then exit;
  for i:=0 to FPendingWrites.Count-1 do
    if TOPCUAServerPendingWrite(FPendingWrites[i]).ResultTag=ATag then exit;
  if csDestroying in ATag.ComponentState then exit;
  ATag.RemoveCommandResultHandler(@TagCommandResult);
  //the free notification stays when the tag is also a published one
  if not FTagItems.ContainsKey(ATag) then
    ATag.RemoveFreeNotification(Self);
end;

procedure TOPCUAServer.ItemChanged(Items:TOPCUAServerItems; Item:TOPCUAServerItem);
var
  i:Integer;
begin
  if FThread=nil then exit;
  if Item=nil then begin
    //items added (or the whole collection assigned): the new ones are published;
    //the items of the structures are handled by the server itself
    if Items.FInternal then exit;
    for i:=0 to Items.Count-1 do
      if not Items[i].Published then
        PublishItem(Items[i]);
  end else begin
    UnpublishItem(Item);
    PublishItem(Item);
  end;
end;

procedure TOPCUAServer.ItemRemoving(Item:TOPCUAServerItem);
begin
  UnpublishItem(Item);
  if (Item.FTag<>nil) and not (csDestroying in ComponentState) then
    Item.FTag.RemoveFreeNotification(Self);
end;

end.
