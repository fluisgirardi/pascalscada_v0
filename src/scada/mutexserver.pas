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
unit mutexserver;

{$I ../common/delphiver.inc}
interface

uses
  Classes, SysUtils, socket_types, crossthreads, socketserver,
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
  { TAcceptThread }

  TAcceptThread = Class(TSocketAcceptThread)
  private
    FMutex:TCriticalSection;
  protected
    procedure LaunchNewThread; override;
  public
    constructor Create(CreateSuspended: Boolean;
                       ServerSocket:TSocket;
                       ServerMutex:syncobjs.TCriticalSection;
                       AddClientThread,
                       RemoveClientThread:TNotifyEvent);
  end;

  { TClientThread }

  TClientThread = Class(TSocketClientThread)
  private
    FMutex:TCriticalSection;
    FIntoCriticalSection:Boolean;
  protected
    procedure ThreadLoop; override;
  public
    constructor Create(CreateSuspended: Boolean;
                       ClientSocket:TSocket;
                       ClientSockinfo:TSockAddr;
                       ServerMutex:syncobjs.TCriticalSection;
                       RemoveClientThread:TNotifyEvent);
  end;

  { TMutexServer }

  TMutexServer = class(TComponent)
  private
    FActive,
    FActiveLoaded: Boolean;
    FPort: Word;
    FSocket: TSocket;
    FMutex:TCriticalSection;
    FAcceptThread:TAcceptThread;
    FClients,
    //threads que ja' acabaram sozinhas e ainda nao foram liberadas
    //threads that already finished on their own and were not freed yet
    FFinishedClients:Array of TClientThread;
    procedure setActive(AValue: Boolean);
    procedure ForgetClientThread(aThread:TClientThread);
    procedure FreeFinishedClients;
    procedure SetPort(AValue: Word);
    procedure AddClientThread(Sender:TObject);
    procedure RemoveClientThread(Sender:TObject);
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active:Boolean read FActive write setActive stored true default false;
    property Port:Word read FPort write SetPort stored true default 52321;

  end;

implementation

uses dateutils, hsstrings {$IF defined(WIN32) or defined(WIN64)} , Windows{$IFEND};


procedure TClientThread.ThreadLoop;
var
  ClientCmd, Response:Byte;
  FaultCount:LongInt;
  Quit:Boolean;
  LastPingSent: QWord;

  const FaultLimit = 10;

  procedure ProcClientCommand(cmd:Byte);
  begin
    case Cmd of
      2: begin
        if FIntoCriticalSection or FMutex.TryEnter then begin
          FIntoCriticalSection:=true;
          LastPingSent:=GetTickCount64;
          Response:=21;
        end else begin
          FIntoCriticalSection:=false;
          Response:=20;
        end;
        if socket_send(FSocket, @Response, 1, 0, 1000)<1 then
          FaultCount:=FaultLimit+1;
      end;
      3: begin
        try
          if FIntoCriticalSection then begin
            FMutex.Leave;
            FIntoCriticalSection:=false;
            Response:=30;
          end else
            Response:=31;
        except
          Response:=32;
        end;

        if socket_send(FSocket, @Response, 1, 0, 1000)<1 then
          FaultCount:=FaultLimit+1;
      end;

      //client was finished.
      253: Quit:=true;
      //client ping response.
      254: begin
        FaultCount:=0;
        LastPingSent:=GetTickCount64;
      end;
    end;
  end;

begin
  //command and responses id list:
  //2  - Try enter on server mutex.
  //20 - Out of server mutex
  //21 - Into server Mutex

  //3 - Leave the server mutex.
  //30 - Out server mutex
  //31 - The client dont own the mutex.

  //253 - Connection closed...
  //254 - Ping response (from client to server)
  //255 - Ping request (from server to client)
  FaultCount:=0;
  Quit:=false;

  LastPingSent:=GetTickCount64;
  while ((not Terminated) and (not Quit)) and (FaultCount<FaultLimit) do begin
    //if more than one seconds was elapsed, send a ping command.
    if (GetTickCount64 - LastPingSent)>=1000 then begin
      Response:=255;

      if socket_send(FSocket, @Response, 1, 0, 1000)<1 then begin
        FaultCount:=FaultLimit+1;
        Break;
      end;

      LastPingSent:=GetTickCount64;
      if socket_recv(FSocket, @Response, 1, 0, 1000)>=1 then
        ProcClientCommand(Response)
      else
        Inc(FaultCount);
    end;

    if socket_recv(FSocket, @ClientCmd, 1{byte to read}, 0{noflasgs}, 5{ms})=1 then begin
      ProcClientCommand(ClientCmd);
    end;
  end;

  //if server was terminated, quit the client side.
  if Terminated or (FaultCount>=FaultLimit) then begin
    Response:=253;
    socket_send(FSocket,PByte(@Response),1,0,1000);
  end;

  //leaves the mutex.
  if FIntoCriticalSection then begin
    FMutex.Leave;
    FIntoCriticalSection:=false;
  end;
end;

constructor TClientThread.Create(CreateSuspended: Boolean;
  ClientSocket: TSocket; ClientSockinfo: TSockAddr;
  ServerMutex: syncobjs.TCriticalSection; RemoveClientThread: TNotifyEvent);
begin
  inherited Create(CreateSuspended,ClientSocket,ClientSockinfo,RemoveClientThread);
  FMutex              := ServerMutex;
  FIntoCriticalSection:=false;
end;

{ TAcceptThread }

procedure TAcceptThread.LaunchNewThread;
begin
  //launch a new thread that will handle this new connection
  setblockingmode(ClientSocket,MODE_NONBLOCKING);
  FClientThread := TClientThread.Create(True, ClientSocket, ClientSockInfo, FMutex, FRemoveClientThread);
  Synchronize(@AddClientToMainThread);
  FClientThread.WakeUp;
end;

constructor TAcceptThread.Create(CreateSuspended: Boolean;
                                 ServerSocket: TSocket;
                                 ServerMutex:syncobjs.TCriticalSection;
                                 AddClientThread,
                                 RemoveClientThread:TNotifyEvent);
begin
  inherited Create(CreateSuspended,ServerSocket,AddClientThread,RemoveClientThread);
  FMutex              := ServerMutex;
end;

{ TMutexServer }

procedure TMutexServer.setActive(AValue: Boolean);
var
{$IF defined(FPC) and defined(UNIX)}
  channel:sockaddr;
{$IFEND}

{$IF defined(FPC) and defined(WINCE)}
  channel:sockaddr_in;
{$IFEND}

{$IF defined(WIN32) or defined(WIN64)}
  channel:sockaddr_in;
{$IFEND}

  reuse_addr:LongInt;
  clientthread: TClientThread;
begin
  reuse_addr:=1;

  if [csLoading,csReading]*ComponentState<>[] then begin
    FActiveLoaded:=AValue;
    exit;
  end;

  if [csDesigning]*ComponentState<>[] then begin
    FActive:=AValue;
    exit;
  end;

  if FActive=AValue then Exit;

  if AValue then begin
    //creates the socket...
    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    //UNIX and WINDOWS CE
    FSocket := fpSocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    if FSocket<0 then begin
      FActive:=false;
      //RefreshLastOSError;
      exit;
    end;
    {$ELSE}
    //WINDOWS 32 and 64 bits
    FSocket :=   Socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    if FSocket=INVALID_SOCKET then begin
      FActive:=false;
      //RefreshLastOSError;
      exit;
    end;
    {$IFEND}

    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_REUSEADDR, @reuse_addr, sizeof(reuse_addr));
    {$IFEND}
    //WINDOWS
    {$IF defined(WIN32) or defined(WIN64)}
    setsockopt(FSocket,   SOL_SOCKET,  SO_REUSEADDR, @reuse_addr, sizeof(reuse_addr));
    {$IFEND}

    //set the non-blocking mode.
    setblockingmode(FSocket, MODE_NONBLOCKING);

    channel.sin_family      := AF_INET;
    channel.sin_addr.S_addr := INADDR_ANY;
    channel.sin_port        := htons(FPort); //PORT NUMBER

    {$IF defined(FPC) AND (defined(UNIX) OR defined(WINCE))}
    if fpBind(FSocket,@channel,sizeof(channel))<>0 then begin
      CloseSocket(FSocket);
      FActive:=false;
      exit;
    end;

    if fpListen(FSocket, SOMAXCONN)<>0 then begin
      CloseSocket(FSocket);
      FActive:=false;
      exit;
    end;
    {$IFEND}

    {$IF defined(WIN32) OR defined(WIN64)}
    if bind(FSocket,channel,sizeof(channel))<>0 then begin
      CloseSocket(FSocket);
      FActive:=false;
      exit;
    end;

    if listen(FSocket, SOMAXCONN)<>0 then begin
      CloseSocket(FSocket);
      FActive:=false;
      exit;
    end;
    {$IFEND}

    //wait for connections?? must be done on another thread, because accept
    //is a blocking call...
    FAcceptThread:=TAcceptThread.Create(true, FSocket, FMutex, @AddClientThread, @RemoveClientThread);
    FAcceptThread.WakeUp;
  end else begin
    //close the socket...
    closesocket(FSocket);
    //destroy the threads from all clients and close the socket.
    FAcceptThread.Terminate;
    FAcceptThread.Destroy;
    CloseSocket(FSocket);

    //destroy all client threads...
    //Terminate so' espera a thread acabar. Sem liberar o objeto, cada estacao
    //que passou pelo servidor fica na memoria ate' o processo morrer.
    //Terminate only waits for the thread to finish. Without freeing the object,
    //every station that ever connected stays in memory until the process dies.
    while Length(FClients)>0 do begin
      clientthread:=FClients[High(FClients)];
      clientthread.Terminate;
      //durante o Terminate a propria thread pode ter se anunciado como acabada
      //e entrado na outra lista: tem que sair das duas antes de ser liberada,
      //senao a varredura do fim acha um ponteiro para memoria ja' devolvida.
      //during Terminate the thread itself may have announced it finished and
      //gone into the other list: it has to leave both before being freed, or
      //the sweep at the end finds a pointer to memory already given back.
      ForgetClientThread(clientthread);
      clientthread.Free;
    end;

    FreeFinishedClients;
  end;
  FActive:=AValue;
end;

procedure TMutexServer.SetPort(AValue: Word);
begin
  if FActive then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if FPort=AValue then Exit;

  FPort:=AValue;
end;

procedure TMutexServer.AddClientThread(Sender: TObject);
var
  i:LongInt;
  found:Boolean;
begin
  if not (sender is TClientThread) then
    raise Exception.Create(SInvalidClass);

  //find the object in object list.
  found := false;
  for i:=0 to High(FClients) do begin
    if FClients[i]=Sender then begin
      found:=true;
      break;
    end;
  end;

  if not found then begin
    i:=Length(FClients);
    SetLength(FClients, i+1);
    FClients[i]:=TClientThread(Sender);
  end;

  FreeFinishedClients;
end;

procedure TMutexServer.RemoveClientThread(Sender: TObject);
var
  i:LongInt;
  found:Boolean;
  h: LongInt;
begin
  if not (sender is TClientThread) then
    raise Exception.Create(SInvalidClass);

  //find the object in object list.
  found := false;
  for i:=0 to High(FClients) do begin
    if FClients[i]=Sender then begin
      found:=true;
      break;
    end;
  end;

  if found then begin
    h:=High(FClients);
    FClients[i]:=FClients[h];
    SetLength(FClients, h);
  end;

  //a estacao saiu sozinha: a thread some da lista, mas o objeto continua vivo.
  //Ela ainda esta' dentro do proprio Execute agora, entao so' da' para liberar
  //depois - na proxima conexao ou no desligamento do servidor.
  //the station left on its own: the thread goes off the list, but the object is
  //still alive. It is inside its own Execute right now, so it can only be freed
  //later - on the next connection or when the server is shut down.
  h:=Length(FFinishedClients);
  SetLength(FFinishedClients, h+1);
  FFinishedClients[h]:=TClientThread(Sender);
end;

procedure TMutexServer.ForgetClientThread(aThread:TClientThread);
var
  i, h: LongInt;
begin
  for i:=High(FClients) downto 0 do
    if FClients[i]=aThread then begin
      h:=High(FClients);
      FClients[i]:=FClients[h];
      SetLength(FClients, h);
    end;

  for i:=High(FFinishedClients) downto 0 do
    if FFinishedClients[i]=aThread then begin
      h:=High(FFinishedClients);
      FFinishedClients[i]:=FFinishedClients[h];
      SetLength(FFinishedClients, h);
    end;
end;

procedure TMutexServer.FreeFinishedClients;
var
  i: LongInt;
  clientthread: TClientThread;
begin
  for i:=High(FFinishedClients) downto 0 do begin
    clientthread:=FFinishedClients[i];
    //Finished e' a RTL dizendo que o Execute ja' voltou: so' ai' da' para
    //liberar o objeto da thread.
    //Finished is the RTL saying Execute already returned: only then can the
    //thread object be freed.
    if not clientthread.Finished then continue;
    ForgetClientThread(clientthread);
    clientthread.Terminate;
    clientthread.Free;
  end;
end;

procedure TMutexServer.Loaded;
begin
  inherited Loaded;
  setActive(FActiveLoaded);
end;

constructor TMutexServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //a mesma porta que a propriedade declara como "default": e' por ela que o
  //IDE decide nao gravar a propriedade no .lfm, e ai e' o construtor que
  //vale ao carregar. Com 51342 aqui e 52321 la', quem escolhia 52321 no IDE
  //carregava 51342.
  //the same port the property declares as "default": it is by that value
  //that the IDE decides not to write the property to the .lfm, and then the
  //constructor rules on load. With 51342 here and 52321 there, whoever chose
  //52321 in the IDE loaded 51342.
  FPort:=52321;
  FMutex:=syncobjs.TCriticalSection.Create;
end;

destructor TMutexServer.Destroy;
begin
  setActive(false);
  FMutex.Destroy;
  inherited Destroy;
end;

end.
