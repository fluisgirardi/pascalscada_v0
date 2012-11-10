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
  , WinSock, sockets_w32_w64
  {$ELSE}
  {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
  , Sockets {$IFDEF UNIX}  , sockets_unix, netdb, Unix{$ENDIF}
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
    function WaitEnd(timeout:Cardinal): TWaitResult;
  protected
    procedure Execute; override;
  public
    procedure Terminate;
    constructor Create(CreateSuspended: Boolean; ServerSocket:TSocket; ServerMutex:syncobjs.TCriticalSection);
    destructor Destroy; override;
  end;

  { TClientThread }

  TClientThread = Class(TCrossThread)
  private
    function WaitEnd(timeout:Cardinal): TWaitResult;
  protected
    procedure Execute; override;
  public
    procedure Terminate;
    constructor Create(CreateSuspended: Boolean; ServerSocket:TSocket; ServerMutex:syncobjs.TCriticalSection);
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
    procedure setActive(AValue: Boolean);
    procedure SetPort(AValue: Word);
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

procedure Register;

implementation

uses hsstrings {$IF defined(WIN32) or defined(WIN64)} , Windows{$ENDIF};

procedure Register;
begin
  RegisterComponents(strUtilsPallete,[TMutexServer]);
end;

{ TClientThread }

function TClientThread.WaitEnd(timeout: Cardinal): TWaitResult;
begin

end;

procedure TClientThread.Execute;
begin

end;

procedure TClientThread.Terminate;
begin

end;

constructor TClientThread.Create(CreateSuspended: Boolean;
  ServerSocket: TSocket; ServerMutex: syncobjs.TCriticalSection);
begin

end;

destructor TClientThread.Destroy;
begin
  inherited Destroy;
end;

{ TAcceptThread }

procedure TAcceptThread.Execute;
var
  ClientSocket:TSocket;
  ClientAddrInfo:sockaddr;
  ClientAddrInfoSize:Integer;
begin
  while not Terminated do begin
    if WaitForConnection(FServerSocket,500) then
      {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
      ClientSocket:=fpAccept(FServerSocket,@ClientAddrInfo,@ClientAddrInfoSize);
      {$IFEND}
      //WINDOWS
      {$IF defined(WIN32) or defined(WIN64)}
      ClientSocket:=  Accept(setsockopt(FSocket,   SOL_SOCKET,  SO_REUSEADDR, @reuse_addr, sizeof(reuse_addr));
      {$IFEND}
      //launch a new thread that will handle this new connection
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

constructor TAcceptThread.Create(CreateSuspended: Boolean; ServerSocket: TSocket; ServerMutex:syncobjs.TCriticalSection);
begin
  inherited Create(CreateSuspended);
  FServerSocket := ServerSocket;
  FMutex        := ServerMutex;
  FEnd          := TCrossEvent.Create(nil,true,false,'');
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
  ServerAddr:THostEntry;
  channel:sockaddr;
{$IFEND}

{$IF defined(FPC) and defined(WINCE)}
  channel:sockaddr_in;
{$IFEND}

{$IF defined(WIN32) or defined(WIN64)}
  ServerAddr:PHostEnt;
  channel:sockaddr_in;
{$IFEND}

  flag, bufsize:Integer;
  reuse_addr:Integer;
begin
  reuse_addr:=1;

  if [csLoading,csReading]*ComponentState<>[] then begin
    FActiveLoaded:=AValue;
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
    //is a blocking calll...
    FAcceptThread:=TAcceptThread.Create(true,FSocket,FMutex);
    FAcceptThread.WakeUp;
  end else begin
    //destroy the threads from all clients and close the socket.
    FAcceptThread.Terminate;
    FAcceptThread.WaitFor;
    CloseSocket(FSocket);
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