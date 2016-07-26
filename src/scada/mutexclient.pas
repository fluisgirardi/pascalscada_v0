{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit que implementa um mutex de rede.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a network mutex.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)


  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Removed Extctrls unit
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
{$ENDIF}
unit MutexClient;

{$I ../common/delphiver.inc}
interface

uses
  Classes, SysUtils, socket_types, CrossEvent,
  syncobjs
  {$IF defined(WIN32) or defined(WIN64)} //delphi or lazarus over windows
    {$IFDEF FPC}
    , WinSock2,
    {$ELSE}
    , WinSock,
    {$ENDIF}
    sockets_w32_w64
  {$ELSE}
  {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
  , Sockets {$IFDEF UNIX}  , sockets_unix, netdb, Unix{$ENDIF}
            {$IFDEF WINCE} , sockets_wince {$ENDIF}
            {$IFDEF FDEBUG}, LCLProc{$ENDIF}
  {$IFEND}
  {$IFEND};

type

  { TMutexClientThread }

  TMutexClientThread = class(TCrossThread)
  private
    fConnectionBroken: TNotifyEvent;
    FOwnMutex:Boolean;
    fServerHasBeenFinished: TNotifyEvent;
    FSocket:Tsocket;
    FEnd:TCrossEvent;
    FSocketMutex:TCriticalSection;
    LastPingSent:TDateTime;
    Quit:Boolean;
  private
    procedure ConnectionIsGone;
  protected
    //called when client got the mutex
    procedure SetIntoServerMutexBehavior; virtual;
    //called when the client leaves the mutex.
    procedure SetOutServerMutexBehavior; virtual;
    //check for ping commmands when client owns the mutex.
    procedure Execute; override;
    //called when server sends a quit command.
    procedure ServerHasBeenFinished; virtual;
    //ping server
    function PingServer:Boolean;
  public
    constructor Create(CreateSuspended: Boolean; aSocket: Tsocket);
    destructor Destroy; override;
    //try enter on server mutex.
    function TryEnter:Boolean;
    //leave the server mutex.
    function Leave: Boolean;
    //send a quit command to server.
    procedure DisconnectFromServer; virtual;
    //wait the client thread ends.
    procedure WaitEnd;
  published
    property onServerHasBeenFinished:TNotifyEvent read fServerHasBeenFinished write fServerHasBeenFinished;
    property onConnectionBroken:TNotifyEvent read fConnectionBroken write fConnectionBroken;
  end;

  { TMutexClient }

  TMutexClient = class(TComponent)
  private
    FActive,
    FActiveLoaded:Boolean;
    FConnected:LongInt;
    FPort: Word;
    FServerHost:AnsiString;
    FSocket: TSocket;
    FDefaultBehavior:Boolean;
    FConnectionStatusThread:TMutexClientThread;
    procedure Connect;
    procedure Disconnect;
    procedure setActive(AValue: Boolean);
    procedure SetPort(AValue: Word);
    procedure SetServerHost(AValue: AnsiString);
    procedure ConnectionFinished(Sender:TObject);
    {$IFDEF FPC}
    function InterLockedExchangePointer(var Target: Pointer;Source : Pointer) : Pointer;
    {$ENDIF}
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    TryEnter:Boolean; overload;
    function    TryEnter(out PickedTheDefaultBehavior: Boolean): Boolean; overload;
    function Leave: Boolean;
  published
    property Active:Boolean read FActive write setActive stored true default false;
    property Host:AnsiString read FServerHost write SetServerHost stored true nodefault;
    property DefaultBehavior:Boolean read FDefaultBehavior write FDefaultBehavior stored true default false;
    property Port:Word read FPort write SetPort stored true default 52321;
  end;

implementation

uses hsstrings, dateutils, hsutils{$IFNDEF FPC}, Windows{$ENDIF};

{ TMutexClientThread }

procedure TMutexClientThread.ConnectionIsGone;
begin
  if Assigned(fConnectionBroken) then
    fConnectionBroken(Self);
end;

procedure TMutexClientThread.SetIntoServerMutexBehavior;
begin
  FOwnMutex:=true;
end;

procedure TMutexClientThread.SetOutServerMutexBehavior;
begin
  FOwnMutex:=False;
end;

procedure TMutexClientThread.Execute;
var
  serverrequest:Byte;

  function SendPingCmd:Boolean;
  var
    request:Byte;
  begin
    request:=254;
    if socket_send(FSocket,@request,1,0,1000)<1 then begin
      ConnectionIsGone;
      Result:=false;
    end else
      LastPingSent:=Now;
  end;

  function InternalPingServer:Boolean;
  begin
    Result:=true;
    if MilliSecondsBetween(now,LastPingSent)>=1000 then begin
      Result:=SendPingCmd;
    end;
  end;

begin
  FEnd.ResetEvent;
  LastPingSent:=Now;
  while (not Terminated) AND (not Quit) do begin
    FSocketMutex.Enter;
    try
      repeat
        if socket_recv(FSocket,@serverrequest,1,0,5)>=1 then begin
          case serverrequest of
            21: begin
              SetIntoServerMutexBehavior;
              exit;
            end;
            20, 30, 31, 32: begin
              SetOutServerMutexBehavior;
              exit;
            end;
            253: begin
              ServerHasBeenFinished;
            end;
            255: begin
              if not SendPingCmd then begin
                ConnectionIsGone;
                break;
              end;
            end;
          end;
        end;
      until GetNumberOfBytesInReceiveBuffer(FSocket)<=0;
      InternalPingServer;
    finally
      FSocketMutex.Leave;
    end;

    Sleep(1);
  end;
  FEnd.SetEvent;
end;

procedure TMutexClientThread.DisconnectFromServer;
var
  request:Byte;
begin
  FSocketMutex.Enter;
  request:=253;//try enter on mutex
  socket_send(FSocket,@request,1,0,1000);
  Quit:=true;
  ConnectionIsGone;
  FSocketMutex.Leave;
end;

procedure TMutexClientThread.WaitEnd;
begin
  while not (FEnd.WaitFor(10)=wrSignaled) do
    CheckSynchronize();
end;

procedure TMutexClientThread.ServerHasBeenFinished;
begin
  Quit:=true;
  ConnectionIsGone;
end;

function TMutexClientThread.PingServer: Boolean;
var
  request: byte;
begin
  Result:=false;
  request:=254;
  if socket_send(FSocket,@request,1,0,1000)<1 then
    ConnectionIsGone
  else begin
    LastPingSent:=Now;
    Result:=true;
  end;
end;

constructor TMutexClientThread.Create(CreateSuspended: Boolean; aSocket: Tsocket
  );
begin
  inherited Create(CreateSuspended);
  FSocketMutex:=TCriticalSection.Create;
  FEnd:=TCrossEvent.Create(true, false);
  FSocket:=aSocket;
  Quit:=false;
end;

destructor TMutexClientThread.Destroy;
begin
  FSocketMutex.Destroy;
  FEnd.Destroy;
  inherited Destroy;
end;

function TMutexClientThread.TryEnter: Boolean;
var
  request, response:Byte;
  ExpectedResponse:Boolean;
begin
  Result:=False;
  ExpectedResponse:=false;
  FSocketMutex.Enter;
  try
    request:=2;//try enter on mutex
    if socket_send(FSocket,@request,1,0,1000)>=1 then begin
      repeat
        if socket_recv(FSocket,@response,1,0,1000)>=1 then begin
          case response of
            20: begin
              Result:=false;
              ExpectedResponse:=true;
              SetOutServerMutexBehavior;
            end;
            21: begin
              Result:=true;
              ExpectedResponse:=true;
              SetIntoServerMutexBehavior;
            end;
            253: begin
              Result:=false;
              ExpectedResponse:=false;
              ServerHasBeenFinished;
              break;
            end;
            255: begin
              ExpectedResponse:=false;
              PingServer;
            end;
            else
              ExpectedResponse:=false;
          end;
        end;
        CheckSynchronize(1);
      until (GetNumberOfBytesInReceiveBuffer(FSocket)<=0) and ExpectedResponse;
    end else
      ConnectionIsGone;
  finally
    FSocketMutex.Leave;
  end;
end;

function TMutexClientThread.Leave:Boolean;
var
  request, response:Byte;
  ExpectedResponse:Boolean;
begin
  Result:=false;
  ExpectedResponse:=false;
  FSocketMutex.Enter;
  try
    request:=3;//try enter on mutex
    if socket_send(FSocket,@request,1,0,1000)>=1 then begin
      repeat
        if socket_recv(FSocket,@response,1,0,1000)>=1 then begin
          case response of
            30, 31, 32: begin
              Result:=true;
              ExpectedResponse:=true;
              SetOutServerMutexBehavior;
            end;
            253: begin
              ExpectedResponse:=False;
              ServerHasBeenFinished;
              break;
            end;
            255: begin
              ExpectedResponse:=false;
              PingServer;
            end;
            else
              ExpectedResponse:=false;
          end;
        end;
      until (GetNumberOfBytesInReceiveBuffer(FSocket)<=0) and ExpectedResponse;
    end else
      ConnectionIsGone;
  finally
    FSocketMutex.Leave;
  end;
end;

{ TMutexClient }

procedure TMutexClient.Connect;
var
{$IF defined(FPC) and defined(UNIX)}
  ServerAddr:THostEntry;
  channel:sockaddr_in;
{$IFEND}

{$IF defined(FPC) and defined(WINCE)}
  channel:sockaddr_in;
{$IFEND}

{$IF defined(WIN32) or defined(WIN64)}
  channel:sockaddr_in;
{$IFEND}

  flag:LongInt;
  socketOpen:boolean;
begin

  if FConnected<>0 then exit;

  socketOpen:=false;

  try
    //##########################################################################
    // RESOLUCAO DE NOMES SOBRE LINUX/FREEBSD e outros.
    // NAME RESOLUTION OVER LINUX/FREEBSD and others.
    //##########################################################################
    {$IF defined(FPC) and defined(UNIX)}
      if not GetHostByName(FServerHost,ServerAddr) then begin
        ServerAddr.Addr:=StrToHostAddr(FServerHost);
        if ServerAddr.Addr.s_addr=0 then begin
          //PActive:=false;
          //RefreshLastOSError;
          exit;
        end;
      end;
    {$IFEND}

    //##########################################################################
    // CRIA O SOCKET
    // CREATE THE SOCKET.
    //##########################################################################

    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    //UNIX and WINDOWS CE
    FSocket := fpSocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

    if FSocket<0 then begin
      //PActive:=false;
      //RefreshLastOSError;
      exit;
    end;
    {$ELSE}
    //WINDOWS
    FSocket :=   Socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

    if FSocket=INVALID_SOCKET then begin
      //PActive:=false;
      //RefreshLastOSError;
      exit;
    end;
    {$IFEND}

    socketOpen:=true;

    //##########################################################################
    //SETA O MODO DE OPERACAO DE NAO BLOQUEIO DE CHAMADA.
    //SET THE NON-BLOCKING OPERATING MODE OF THE SOCKET
    //##########################################################################
    setblockingmode(FSocket,MODE_NONBLOCKING);

    //##########################################################################
    //SETA AS OPCOES DO SOCKET
    //OPCOES DE TIMEOUT IRÃO SER FEITAS USANDO SELECT/FPSELECT
    //POIS ESTAS OPÇÕES NÃO SAO SUPORTADAS POR ALGUNS SISTEMAS OPERACIONAIS
    //
    //SOCKET OPTIONS
    //TIMEOUT OPTIONS ARE MADE USING SELECT/FPSELECT, BECAUSE THIS OPTIONS
    //AREN'T SUPPORTED BY SOME OSes LIKE WINDOWS CE
    //##########################################################################
    flag:=1;
    //UNIX AND WINDOWS CE
    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    fpsetsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY,  @flag,           sizeof(LongInt));
    {$IFEND}
    //WINDOWS
    {$IF defined(WIN32) or defined(WIN64)}
    setsockopt  (FSocket, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@flag), sizeof(LongInt));
    {$IFEND}

    //##########################################################################
    //CONFIGURA E ENDERECO QUE O SOCKET VAI CONECTAR
    //SETS THE TARGET ADDRESS TO SOCKET CONNECT
    //##########################################################################
    channel.sin_family      := AF_INET;      //FAMILY
    channel.sin_port        := htons(FPort); //PORT NUMBER

    {$IF defined(FPC) AND defined(UNIX)}
    channel.sin_addr.S_addr := longword(htonl(LongInt(ServerAddr.Addr.s_addr)));
    {$IFEND}

    {$IF defined(FPC) AND defined(WINCE)}
    channel.sin_addr := StrToNetAddr(FServerHost);
    {$IFEND}

    {$IF defined(WIN32) OR defined(WIN64)}
    channel.sin_addr.S_addr := inet_addr(PAnsiChar(FServerHost));
    {$IFEND}

    if connect_with_timeout(FSocket,@channel,sizeof(channel),2000)<>0 then begin
      exit;
    end;
    FConnected:=1;
    FConnectionStatusThread:=TMutexClientThread.Create(true, FSocket);
    FConnectionStatusThread.FreeOnTerminate:=true;
    FConnectionStatusThread.OnTerminate:=@ConnectionFinished;
    FConnectionStatusThread.onConnectionBroken:=@ConnectionFinished;
    FConnectionStatusThread.onServerHasBeenFinished:=@ConnectionFinished;


    //after setup the thread, wake up it.
    FConnectionStatusThread.WakeUp;
  finally
    if socketOpen and (FConnected=0) then
      CloseSocket(FSocket);
  end;
end;

procedure TMutexClient.Disconnect;
var
  threadinstance: TMutexClientThread;
begin
  if FConnected<>0 then begin
    if FConnectionStatusThread<>nil then begin
      threadinstance:=FConnectionStatusThread;
      with threadinstance do begin
        FreeOnTerminate:=false;
        DisconnectFromServer;
        WaitEnd;
        Destroy;
      end;
    end;
    FConnectionStatusThread:=nil;
    CloseSocket(FSocket);
    FConnected:=0;
  end;
end;

procedure TMutexClient.setActive(AValue: Boolean);
begin
  if [csLoading,csReading]*ComponentState<>[] then begin
    FActiveLoaded:=AValue;
    exit;
  end;

  if [csDesigning]*ComponentState<>[] then begin
    FActive:=AValue;
    exit;
  end;

  if AValue then
    Connect
  else
    Disconnect;

  FActive:=AValue;
end;

procedure TMutexClient.SetPort(AValue: Word);
begin
  if FActive then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if FPort=AValue then Exit;

  FPort:=AValue;
end;

procedure TMutexClient.SetServerHost(AValue: AnsiString);
var
  ip: TStringArray;
  i, ZeroCount, FFCount: Integer;
  octeto: Longint;

label
  err;
begin
  if FActive then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if (FServerHost=trim(AValue)) then exit;

  if (trim(AValue)='') then begin
    FServerHost:=trim(AValue);
    exit;
  end;

  if FServerHost<>AValue then begin
    ip:=ExplodeString('.',AValue);
    if Length(ip)<>4 then
      goto err;

    ZeroCount:=0;
    FFCount:=0;
    for i:=0 to 3 do begin
      if TryStrToInt(ip[i],octeto)=false then goto err;
      if not (octeto in [0..255]) then goto err;
      if ((i=0) or (i=3)) and ((octeto=0) or (octeto=255)) then goto err;
      if octeto=0   then ZeroCount:=ZeroCount + 1;
      if octeto=255 then FFCount  :=FFCount   + 1;
    end;
    if ZeroCount=4 then goto err;
    if FFCount=4   then goto err;

    FServerHost:=AValue;
    exit;
  end;

err:
  raise Exception.Create(Format('The address "%s" is not a valid IPv4 address',[AValue]));
end;

procedure TMutexClient.ConnectionFinished(Sender: TObject);
begin
  CloseSocket(FSocket);
  InterLockedExchange(FConnected,0);

  //TSocket 32 bits sized
  {$IF sizeof(TSocket)=4}
  InterLockedExchange(integer(FSocket),0);
  {$IFEND}

  {$IF sizeof(TSocket)=8}
  InterLockedExchange64(Int64(FSocket), 0);
  {$IFEND}

  InterlockedExchangePointer(Pointer(FConnectionStatusThread),nil);
end;

{$IFDEF FPC}
function TMutexClient.InterLockedExchangePointer(var Target: Pointer;
  Source: Pointer): Pointer;
begin
  Result := InterLockedExchange (Target, Source);
end;
{$ENDIF}

procedure TMutexClient.Loaded;
begin
  inherited Loaded;
  setActive(FActiveLoaded);
end;

constructor TMutexClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort:=51342;
  FActive:=false;
  FConnected:=0;
  FActiveLoaded:=false;
end;

destructor TMutexClient.Destroy;
begin
  setActive(false);
  inherited Destroy;
end;

function TMutexClient.TryEnter: Boolean;
var
  adefaultbehavior: Boolean;
begin
  Result:=TryEnter(adefaultbehavior);
end;

function TMutexClient.TryEnter(out PickedTheDefaultBehavior: Boolean): Boolean;
begin
  Result:=FDefaultBehavior;
  PickedTheDefaultBehavior:=true;
  if FActive then begin
    //if not connected, connect
    if FConnected=0 then
      Connect;

    //if still disconnected, exit.
    if FConnected=0 then exit;

    if FConnectionStatusThread=nil then exit;

    PickedTheDefaultBehavior:=false;
    Result:=FConnectionStatusThread.TryEnter;
  end;
end;

function TMutexClient.Leave:Boolean;
begin
  Result:=True;

  if FActive then begin
    //if not connected, connect
    if FConnected=0 then
      Connect;

    //if still disconnected, exit.
    if FConnected=0 then exit;

    if FConnectionStatusThread=nil then exit;

    Result:=FConnectionStatusThread.Leave;
  end;
end;

end.
