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

  TOPCUAServer = class;

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
  protected
    procedure Loaded; override;
    procedure Notification(AComponent:TComponent; Operation:TOperation); override;
    procedure ItemChanged(Item:TOPCUAServerItem);
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
  end;

  { server thread: internal }

  TOPCUAServerCmdKind = (sckAdd, sckRemove, sckValue);

  TOPCUAServerCmd = record
    Kind:TOPCUAServerCmdKind;
    Key:Integer;
    //add
    NodeId, BrowseName, Folder, Description:String;
    TypeIndex:Integer;
    IsText:Boolean;
    //add and value
    Num:Double;
    Text:UTF8String;
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
    Num:Double;
    Text:UTF8String;
    Status:UA_StatusCode;
    Time:UA_DateTime;
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
    procedure ApplyCmds;
    procedure AddNode(const Cmd:TOPCUAServerCmd);
    procedure RemoveNode(Key:Integer);
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
  NumExprTag, ProtocolTypes, OPCUAClient, OPCUAValues, pscadalog;

type
  TPLCTagAccess = class(TPLCTag);

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
  TypeIndex := UA_TYPES_DOUBLE;
  Why := '';
  T := Item.Tag;
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
  Result := UA_STATUSCODE_GOOD;
  try
    n := TOPCUAServerNode(nodeContext);
    value^.flags := value^.flags or UA_DATAVALUE_HASSTATUS;
    if (range<>nil) and (range^.dimensionsSize>0) then begin
      value^.status := UA_STATUSCODE_BADINDEXRANGEINVALID;
      exit;
    end;
    if n.Status=UA_STATUSCODE_BADWAITINGFORINITIALDATA then begin
      value^.status := n.Status;
      exit;
    end;
    if n.IsText then
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
      exit;
    end;
    value^.flags := value^.flags or UA_DATAVALUE_HASVALUE;
    value^.status := n.Status;
    if includeSourceTimeStamp then begin
      value^.sourceTimestamp := n.Time;
      value^.flags := value^.flags or UA_DATAVALUE_HASSOURCETIMESTAMP;
    end;
  except
    value^.status := UA_STATUSCODE_BADINTERNALERROR;
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
    TOPCUAServer(Owner).ItemChanged(TOPCUAServerItem(Item));
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
      sckValue:
        if FNodes.TryGetValue(cmds[i].Key, n) then begin
          n.Num := cmds[i].Num;
          n.Text := cmds[i].Text;
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
  n.Num := Cmd.Num;
  n.Text := Cmd.Text;
  n.Status := Cmd.Status;
  n.Time := UA_DateTimeFromDateTime(Cmd.TimeUTC);

  attr := UA_VariableAttributes_default;
  attr.dataType := UA_TYPES[Cmd.TypeIndex].typeId;
  attr.valueRank := UA_VALUERANK_SCALAR;
  attr.displayName := _UA_LOCALIZEDTEXT_ALLOC('', Cmd.BrowseName);
  if Cmd.Description<>'' then
    attr.description := _UA_LOCALIZEDTEXT_ALLOC('', Cmd.Description);
  attr.accessLevel := UA_ACCESSLEVELMASK_READ;
  attr.userAccessLevel := attr.accessLevel;
  src.read := @NodeRead;
  src.write := nil;
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
  st := UA_ServerConfig_setMinimal(conf, FPort, nil);
  if st<>UA_STATUSCODE_GOOD then
    raise Exception.Create('server configuration: '+OPCUAStatusText(st));
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
  FItems := TOPCUAServerItems.Create(Self);
end;

destructor TOPCUAServer.Destroy;
var
  l:TFPList;
begin
  Destroying;
  StopServer;
  TThread.RemoveQueuedEvents(@DeferredStart);
  FreeAndNil(FItems);
  for l in FTagItems.Values do l.Free;
  FreeAndNil(FTagItems);
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
  if (Operation=opRemove) and (FItems<>nil) then
    for i:=0 to FItems.Count-1 do
      if FItems[i].FTag=AComponent then
        FItems[i].Tag := nil;
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
  Item.FStatusTag := StatusTagOf(Item.Tag);
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
  FThread.Post(cmd);
  PostValue(Item);
end;

procedure TOPCUAServer.UnpublishItem(Item:TOPCUAServerItem);
var
  cmd:TOPCUAServerCmd;
  key:Integer;
begin
  if not Item.Published then exit;
  key := Item.FKey;
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
  if (FThread=nil) or (not Item.Published) then exit;
  T := Item.Tag;
  cmd := Default(TOPCUAServerCmd);
  cmd.Kind := sckValue;
  cmd.Key := Item.FKey;
  cmd.Status := TagStatus(Item);
  cmd.TimeUTC := LocalTimeToUniversal(Now);
  try
    if Item.FIsText then
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
var
  i:Integer;
begin
  //the tag is being destroyed: its items lose it (and their variables)
  for i:=0 to FItems.Count-1 do
    if FItems[i].FTag=Sender then
      FItems[i].Tag := nil
    else if FItems[i].FStatusTag=Sender then begin
      UnpublishItem(FItems[i]);
      PublishItem(FItems[i]);
    end;
end;

procedure TOPCUAServer.ItemChanged(Item:TOPCUAServerItem);
var
  i:Integer;
begin
  if FThread=nil then exit;
  if Item=nil then begin
    for i:=0 to FItems.Count-1 do begin
      UnpublishItem(FItems[i]);
      PublishItem(FItems[i]);
    end;
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
