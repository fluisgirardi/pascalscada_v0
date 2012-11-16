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
unit mutexserver;

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
  , Sockets {$IFDEF UNIX}  , sockets_unix, netdb, Unix, BaseUnix{$ENDIF}
            {$IFDEF WINCE} , sockets_wince {$ENDIF}
            {$IFDEF FDEBUG}, LCLProc{$ENDIF}
  {$IFEND}
  {$IFEND};

type
  { TAcceptThread }

  TAcceptThread = Class(TCrossThread)
  private
    FServerSocket:TSocket;
    FMutex:TCriticalSection;
    FEnd:TCrossEvent;
    FClientThread:TCrossThread;
    FAddClientThread,
    FRemoveClientThread:TNotifyEvent;
    function WaitEnd(timeout:Cardinal): TWaitResult;
    procedure AddClientToMainThread;
  protected
    procedure Execute; override;
  public
    procedure Terminate;
    constructor Create(CreateSuspended: Boolean;
                       ServerSocket:TSocket;
                       ServerMutex:syncobjs.TCriticalSection;
                       AddClientThread,
                       RemoveClientThread:TNotifyEvent);
    destructor Destroy; override;
  end;

  { TClientThread }

  TClientThread = Class(TCrossThread)
  private
    FSocket:TSocket;
    FMutex:TCriticalSection;
    FEnd:TCrossEvent;
    FIntoCriticalSection:Boolean;
    FRemoveClientThread:TNotifyEvent;
    function WaitEnd(timeout:Cardinal): TWaitResult;
    procedure ClientFinished;
  protected
    procedure Execute; override;
  public
    procedure Terminate;
    constructor Create(CreateSuspended: Boolean;
                       ClientSocket:TSocket;
                       ServerMutex:syncobjs.TCriticalSection;
                       RemoveClientThread:TNotifyEvent);
    destructor Destroy; override;
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
    FClients:Array of TClientThread;
    procedure setActive(AValue: Boolean);
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

uses hsstrings {$IF defined(WIN32) or defined(WIN64)} , Windows{$ENDIF};


{ TClientThread }

function TClientThread.WaitEnd(timeout: Cardinal): TWaitResult;
begin
  Result := FEnd.WaitFor(timeout);
end;

procedure TClientThread.ClientFinished;
begin
  if Assigned(FRemoveClientThread) then
    FRemoveClientThread(Self);
end;

procedure TClientThread.Execute;
var
  ClientCmd, Response:Byte;
  FaultCount:Integer;
  Quit:Boolean;

  const FaultLimite = 30;
  procedure CheckConnectionError; forward;

  procedure ProcResponse(response:Byte);
  begin
    case response of
      2: begin //try enter...
        if FIntoCriticalSection or FMutex.TryEnter then begin
          Response:=1;
          FIntoCriticalSection:=true;
        end else
          Response:=0;

        if socket_send(FSocket,PByte(@Response),1,0,1000)<1 then
          CheckConnectionError;
      end;

      3: begin //leave the mutex.
        Response:=0;
        if FIntoCriticalSection then begin
          FMutex.Leave;
          FIntoCriticalSection:=false;
        end;
        //send the response..
        if socket_send(FSocket,PByte(@Response),1,0,1000)<1 then
          CheckConnectionError;
      end;

      253: begin//clear the fault count...
        Quit:=true;
      end;

      254: begin//clear the fault count...
        FaultCount:=0;
      end;
    end;
  end;

  procedure CheckConnectionError;
  var
    ioresult:TIOResult;
    PActive, IncFault, IncRetries:Boolean;
  begin
    Response:=255;
    IncFault:=false;
    if socket_send(FSocket,PByte(@Response),1,0,1000)<1 then
      if not CheckConnection(ioresult,IncRetries,PActive,FSocket,nil) then
        IncFault:=true;

    if socket_recv(FSocket,PByte(@ClientCmd),1,0,1000)>0 then
      ProcResponse(ClientCmd)
    else begin
      if not CheckConnection(ioresult,IncRetries,PActive,FSocket,nil) then
        IncFault:=true;
    end;

    if IncFault then inc(FaultCount);

    //fault limit, cleanup...
    if FaultCount>=FaultLimite then begin
      //leave the mutex...
      if FIntoCriticalSection then begin
        FMutex.Leave;
        FIntoCriticalSection:=false;
      end;
    end;
  end;

begin
  //command and responses id list:
  //0 - Out of server mutex
  //1 - Into server mutex
  //2 - Try Enter on server mutex
  //3 - Leave the server mutex.
  //253 - Connection closed...
  //254 - Ping response (from client to server)
  //255 - Ping request (from server to client)
  FaultCount:=0;
  Quit:=false;

  while ((not Terminated) and (not Quit)) and (FaultCount<FaultLimite) do begin
    if socket_recv(FSocket,@ClientCmd,1,0,1000)>0 then begin
      ProcResponse(ClientCmd);
    end else
      CheckConnectionError;
  end;

  //if terminated, cleanup
  if Terminated then begin
    Response:=253;
    socket_send(FSocket,PByte(@Response),1,0,1000);
  end;

  //leaves the mutex.
  if FIntoCriticalSection then begin
    FMutex.Leave;
    FIntoCriticalSection:=false;
  end;

  //Close the socket.
  CloseSocket(FSocket);

  //remove the connection thread from the main thread list.
  Synchronize(ClientFinished);

  while not FEnd.SetEvent do Sleep(1);
end;

procedure TClientThread.Terminate;
begin
  TCrossThread(self).Terminate;
  repeat
     CheckSynchronize(1);
  until WaitEnd(1)=wrSignaled;
end;

constructor TClientThread.Create(CreateSuspended: Boolean;
                                 ClientSocket: TSocket;
                                 ServerMutex: syncobjs.TCriticalSection;
                                 RemoveClientThread:TNotifyEvent);
begin
  inherited Create(CreateSuspended);
  FSocket             := ClientSocket;
  FMutex              := ServerMutex;
  FEnd                := TCrossEvent.Create(nil,true,false,'');
  FRemoveClientThread :=RemoveClientThread;
  FEnd.ResetEvent;
  FIntoCriticalSection:=false;
end;

destructor TClientThread.Destroy;
begin
  FEnd.Destroy;
  inherited Destroy;
end;

{ TAcceptThread }

procedure TAcceptThread.Execute;
var
  ClientSocket:TSocket;

  procedure LaunchNewThread;
  begin
    //launch a new thread that will handle this new connection
    setblockingmode(ClientSocket,MODE_NONBLOCKING);
    FClientThread := TClientThread.Create(True, ClientSocket, FMutex, FRemoveClientThread);
    Synchronize(AddClientToMainThread);
    FClientThread.WakeUp;
  end;

begin

  while not Terminated do begin
    //linux, bsd
    {$IF defined(FPC) AND defined(UNIX)}
    ClientSocket:=fpAccept(FServerSocket,nil,nil);

    if ClientSocket>0 then
      LaunchNewThread
    else
      Sleep(500);
    {$IFEND}

    //WINCE
    {$IF defined(FPC) AND defined(WINCE)}
    ClientSocket:=fpAccept(FServerSocket,nil,nil);

    if ClientSocket<>INVALID_SOCKET then
      LaunchNewThread
    else
      Sleep(500);
    {$IFEND}

    //WINDOWS
    {$IF defined(WIN32) or defined(WIN64)}
    ClientSocket:=  Accept(FServerSocket,nil,nil);

    if ClientSocket<>INVALID_SOCKET then
      LaunchNewThread
    else
      Sleep(500);
    {$IFEND}
  end;
  while not FEnd.SetEvent do Sleep(1);
end;

procedure TAcceptThread.Terminate;
begin
  TCrossThread(self).Terminate;
  repeat
     CheckSynchronize(1);
  until WaitEnd(1)=wrSignaled;
end;

function TAcceptThread.WaitEnd(timeout:Cardinal): TWaitResult;
begin
  Result := FEnd.WaitFor(timeout);
end;

procedure TAcceptThread.AddClientToMainThread;
begin
  if Assigned(FAddClientThread) then
    FAddClientThread(FClientThread);
end;

constructor TAcceptThread.Create(CreateSuspended: Boolean;
                                 ServerSocket: TSocket;
                                 ServerMutex:syncobjs.TCriticalSection;
                                 AddClientThread,
                                 RemoveClientThread:TNotifyEvent);
begin
  inherited Create(CreateSuspended);
  FServerSocket       := ServerSocket;
  FMutex              := ServerMutex;
  FAddClientThread    := AddClientThread;
  FRemoveClientThread := RemoveClientThread;
  FEnd                := TCrossEvent.Create(nil,true,false,'');

  FEnd.ResetEvent;
end;

destructor TAcceptThread.Destroy;
begin
  FEnd.Destroy;
  inherited Destroy;
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
  ServerAddr:PHostEnt;
  channel:sockaddr_in;
{$IFEND}

  reuse_addr:Integer;
  ct: Integer;
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
    {$ENDIF}

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
    FAcceptThread:=TAcceptThread.Create(true, FSocket, FMutex, AddClientThread, RemoveClientThread);
    FAcceptThread.WakeUp;
  end else begin
    //destroy the threads from all clients and close the socket.
    FAcceptThread.Terminate;
    FAcceptThread.Destroy;
    CloseSocket(FSocket);

    //destroy all client threads...
    for ct:=0 to High(FClients) do begin
      FClients[ct].Terminate;
      FClients[ct].Destroy;
    end;
  end;
  FActive:=AValue;
end;

procedure TMutexServer.SetPort(AValue: Word);
begin
  if FActive then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if (AValue<1) or (AValue>65535) then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if FPort=AValue then Exit;

  FPort:=AValue;
end;

procedure TMutexServer.AddClientThread(Sender: TObject);
var
  i:Integer;
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
end;

procedure TMutexServer.RemoveClientThread(Sender: TObject);
var
  i:Integer;
  found:Boolean;
  h: Integer;
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
end;

procedure TMutexServer.Loaded;
begin
  inherited Loaded;
  setActive(FActiveLoaded);
end;

constructor TMutexServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort:=51342;
  FMutex:=syncobjs.TCriticalSection.Create;
end;

destructor TMutexServer.Destroy;
begin
  setActive(false);
  FMutex.Destroy;
  inherited Destroy;
end;

end.
