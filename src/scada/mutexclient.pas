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
}
{$ENDIF}
unit MutexClient;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{$I ../common/delphiver.inc}
interface

uses
  Classes, SysUtils, ExtCtrls, CommPort, commtypes, socket_types, CrossEvent,
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
    FConnected:Integer;
    FPort: Word;
    FServerHost:String;
    FSocket: TSocket;
    FDefaultBehavior:Boolean;
    FConnectionStatusThread:TMutexClientThread;
    procedure Connect;
    procedure Disconnect;
    procedure setActive(AValue: Boolean);
    procedure SetPort(AValue: Word);
    procedure SetServerHost(AValue: String);
    procedure ConnectionFinished(Sender:TObject);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    TryEnter:Boolean;
    function    TryEnter(PickedTheDefaultBehavior: Boolean): Boolean; overload;
    function Leave: Boolean;
  published
    property Active:Boolean read FActive write setActive stored true default false;
    property Host:String read FServerHost write SetServerHost stored true nodefault;
    property DefaultBehavior:Boolean read FDefaultBehavior write FDefaultBehavior stored true default false;
    property Port:Word read FPort write SetPort stored true default 52321;
  end;

implementation

uses hsstrings, dateutils;

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
  serverrequest,
  request:Byte;
  commresult: TIOResult;
  incRetries: Boolean;
  StillActive: Boolean;

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

  function PingServer:Boolean;
  begin
    Result:=true;
    if MilliSecondsBetween(now,LastPingSent)>=1000 then begin
      Result:=SendPingCmd;
    end;
  end;

begin
  FEnd.ResetEvent;
  LastPingSent:=Now;
  while (not Terminated) do begin
    FSocketMutex.Enter;
    try
      repeat
        if socket_recv(FSocket,@serverrequest,1,MSG_PEEK,5)>=1 then begin
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
              socket_recv(FSocket,@serverrequest,1,0,5);
              ServerHasBeenFinished;
            end;
            255: begin
              socket_recv(FSocket,@serverrequest,1,0,5);
              if not SendPingCmd then begin
                ConnectionIsGone;
                break;
              end;
            end;
          end;
        end;
      until GetNumberOfBytesInReceiveBuffer(FSocket)<=0;
      PingServer;
    finally
      FSocketMutex.Leave;
    end;

    Sleep(1);
  end;
  FEnd.SetEvent;
end;

procedure TMutexClientThread.DisconnectFromServer;
var
  request, response:Byte;
begin
  FSocketMutex.Enter;
  request:=253;//try enter on mutex
  socket_send(FSocket,@request,1,0,1000);
  FSocketMutex.Leave;
  Terminate;
end;

procedure TMutexClientThread.WaitEnd;
begin
  while not (FEnd.WaitFor(10)=wrSignaled) do
    CheckSynchronize();
end;

procedure TMutexClientThread.ServerHasBeenFinished;
begin
  Terminate;
end;

function TMutexClientThread.PingServer: Boolean;
var
  request: byte;
begin
  request:=254;
  if socket_send(FSocket,@request,1,0,1000)<1 then
    ConnectionIsGone
  else
    LastPingSent:=Now;
end;

constructor TMutexClientThread.Create(CreateSuspended: Boolean; aSocket: Tsocket
  );
begin
  inherited Create(CreateSuspended);
  FSocketMutex:=TCriticalSection.Create;
  FEnd:=TCrossEvent.Create(nil,true,false,'');
  FSocket:=aSocket;
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
begin
  Result:=False;
  FSocketMutex.Enter;
  try
    request:=2;//try enter on mutex
    if socket_send(FSocket,@request,1,0,1000)>=1 then begin
      repeat
        if socket_recv(FSocket,@response,1,0,1000)>=1 then begin
          case response of
            20: begin
              Result:=false;
              SetOutServerMutexBehavior;
            end;
            21: begin
              Result:=true;
              SetIntoServerMutexBehavior;
            end;
            253: begin
              Result:=false;
              ServerHasBeenFinished;
              break;
            end;
            255: begin
              PingServer;
            end;
          end;
        end else begin
          //if the program is at this line,
          //is because it send the request,
          //but don´t received a response (timeout)
          //so, release the mutex sending
          //a release command.
          request:=3; //leave mutex command.
          if socket_send(FSocket,@request,1,0,1000)<1 then
            ConnectionIsGone;
        end;
      until GetNumberOfBytesInReceiveBuffer(FSocket)<=0;
    end else
      ConnectionIsGone;
  finally
    FSocketMutex.Leave;
  end;
end;

function TMutexClientThread.Leave:Boolean;
var
  request, response:Byte;
begin
  Result:=false;
  FSocketMutex.Enter;
  try
    request:=3;//try enter on mutex
    if socket_send(FSocket,@request,1,0,1000)>=1 then begin
      repeat
        if socket_recv(FSocket,@response,1,0,1000)>=1 then begin
          case response of
            30, 31, 32: begin
              Result:=true;
              SetOutServerMutexBehavior;
            end;
            253: begin
              ServerHasBeenFinished;
              break;
            end;
            255: begin
              PingServer;
            end;
          end;
        end;
      until GetNumberOfBytesInReceiveBuffer(FSocket)<=0;
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
  ServerAddr:PHostEnt;
  channel:sockaddr_in;
{$IFEND}

  flag:Integer;
  socketOpen:boolean;
begin

  if FConnected<>0 then exit;

  socketOpen:=false;

  try
    //##########################################################################
    // RESOLUCAO DE NOMES SOBRE WINDOWS 32/64 BITS.
    // NAME RESOLUTION OVER WINDOWS 32/64 BITS.
    //##########################################################################
    {$IF defined(WIN32) or defined(WIN64)}
      //se esta usando FPC ou um Delphi abaixo da versao 2009, usa a versão
      //ansistring, caso seja uma versao delphi 2009 ou superior
      //usa a versao unicode.
      //
      //if the name resolution is being done using FPC or Delphi 2009 or older
      //uses the ansistring version, otherwise uses the unicode version.
      {$IF defined(FPC) OR (not defined(DELPHI2009_UP))}
      ServerAddr := GetHostByName(PAnsiChar(FServerHost));
      {$ELSE}
      ServerAddr := GetHostByName(PAnsiChar(AnsiString(FServerHost)));
      {$IFEND}
      if ServerAddr=nil then begin
        //PActive:=false;
        //RefreshLastOSError;
        exit;
      end;
    {$IFEND}

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
    fpsetsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY,  @flag,           sizeof(Integer));
    {$IFEND}
    //WINDOWS
    {$IF defined(WIN32) or defined(WIN64)}
    setsockopt  (FSocket, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@flag), sizeof(Integer));
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
    channel.sin_addr := StrToNetAddr(FHostName);
    {$IFEND}

    {$IF defined(WIN32) OR defined(WIN64)}
    channel.sin_addr.S_addr := PInAddr(Serveraddr.h_addr^).S_addr;
    {$IFEND}

    if connect_with_timeout(FSocket,@channel,sizeof(channel),2000)<>0 then begin
      //PActive:=false;
      //RefreshLastOSError;
      exit;
    end;
    FConnected:=1;
    FConnectionStatusThread:=TMutexClientThread.Create(true, FSocket);
    FConnectionStatusThread.FreeOnTerminate:=true;
    FConnectionStatusThread.OnTerminate:=ConnectionFinished;
    FConnectionStatusThread.onConnectionBroken:=ConnectionFinished;
    FConnectionStatusThread.onServerHasBeenFinished:=ConnectionFinished;


    //after setup the thread, wake up it.
    FConnectionStatusThread.WakeUp;
  finally
    if socketOpen and (FConnected=0) then
      CloseSocket(FSocket);
  end;
end;

procedure TMutexClient.Disconnect;
begin
  if FConnected<>0 then begin
    if FConnectionStatusThread<>nil then begin
      FConnectionStatusThread.DisconnectFromServer;
      FConnectionStatusThread.WaitEnd;
      FConnectionStatusThread.Destroy;
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

  if (AValue<1) or (AValue>65535) then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if FPort=AValue then Exit;

  FPort:=AValue;
end;

procedure TMutexClient.SetServerHost(AValue: String);
begin
  if FActive then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if FServerHost=AValue then Exit;
  FServerHost:=AValue;
end;

procedure TMutexClient.ConnectionFinished(Sender: TObject);
begin
  InterLockedExchange(FConnected,0);
  InterLockedExchange(Pointer(FConnectionStatusThread),nil);
end;

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

function TMutexClient.TryEnter(PickedTheDefaultBehavior: Boolean): Boolean; overload;
var
  request, response:Byte;
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
var
  request, response:Byte;
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
