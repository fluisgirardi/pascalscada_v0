unit redundancyctrl;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tcp_udpport, socket_types, CrossEvent, crossthreads,
  syncobjs, socketserver
  {$IF defined(WIN32) or defined(WIN64)} //delphi or lazarus over windows
    {$IFDEF FPC}
    , WinSock2,
    {$ELSE}
    , WinSock,
    {$ENDIF}
    sockets_w32_w64
  {$ELSE}
  {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
  , Sockets {$IFDEF UNIX}, ugetifaddrs, sockets_unix, netdb, Unix{$ENDIF}
            {$IFDEF WINCE} , sockets_wince {$ENDIF}
            {$IFDEF FDEBUG}, LCLProc{$ENDIF}
  {$IFEND}
  {$IFEND};

type

  TIPv4CollectionItem = class(TCollectionItem)
  private
    FIPv4Address1,
    FIPv4Address2,
    FIPv4Address3,
    FIPv4Address4: String;
    procedure SetIPv4Address1(AValue: String);
    procedure SetIPv4Address2(AValue: String);
    procedure SetIPv4Address3(AValue: String);
    procedure SetIPv4Address4(AValue: String);
  protected
    function GetDisplayName: string; override;
  public
    procedure AssignTo(Dest: TPersistent); override;
  published
    property IPv4Address1:String read FIPv4Address1 write SetIPv4Address1;
    property IPv4Address2:String read FIPv4Address2 write SetIPv4Address2;
    property IPv4Address3:String read FIPv4Address3 write SetIPv4Address3;
    property IPv4Address4:String read FIPv4Address4 write SetIPv4Address4;
  end;

  TIPv4Collection = class(TCollection)
  public
    constructor Create;
    function Add: TIPv4CollectionItem;
  end;

  { TRedundancyAcceptSocket }

  TRedundancyAcceptSocket = class(TSocketAcceptThread)
  protected
    procedure LaunchNewThread; override;
  end;

  TRedundancyClient = class (TSocketClientThread)

  end;

  { TRedundancyClientManager }

  TRedundancyClientManager = class(TpSCADACoreAffinityThreadWithLoop)
  private
    FServerPriorityMutex: TCriticalSection;
    FServerPriority: TIPv4Collection;
    procedure SetServerPriority(AValue: TIPv4Collection);
  protected
    procedure Loop; override;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
  DefaultStackSize);
    property ServerPriority:TIPv4Collection read FServerPriority write SetServerPriority;
  end;

  { TRedundancyCtrl }

  TRedundancyCtrl = class(TComponent)
  protected
    FActive,
    FActiveLoaded: Boolean;
    FPort: Word;
    FServerPriority: TIPv4Collection;
    FSocket:Tsocket;
    //FAcceptThread:TAcceptThread;
    procedure setActive(AValue: Boolean);
    procedure SetPort(AValue: Word);
    procedure SetServerPriority(AValue: TIPv4Collection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RedundancyServerActive:Boolean read FActive write setActive stored true default false;
    property Port:Word read FPort write SetPort stored true default 65432;
    property ServerPriority:TIPv4Collection read FServerPriority write SetServerPriority;
  end;

resourcestring
  FInvalidIPv4 = '(invalid)';
  FEmptyIPv4   = '(empty)';

implementation

{ TRedundancyClientManager }

procedure TRedundancyClientManager.SetServerPriority(AValue: TIPv4Collection);
begin
  try
    FServerPriorityMutex.Create;

    FServerPriority.Assign(AValue);
  finally
    FServerPriorityMutex.Leave;
  end;
end;

procedure TRedundancyClientManager.Loop;
var
  intPriority: TIPv4Collection;
begin
  intPriority:=TIPv4Collection.Create;
  try
    try
      FServerPriorityMutex.Acquire;
      intPriority.Assign(FServerPriority);
    finally
      FServerPriorityMutex.Leave;
    end;
  finally
    intPriority.Free;
  end;
end;

constructor TRedundancyClientManager.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FServerPriorityMutex:=syncobjs.TCriticalSection.Create;
  FServerPriority:=TIPv4Collection.Create;
end;

{ TRedundancyAcceptSocket }

procedure TRedundancyAcceptSocket.LaunchNewThread;
begin
  //launch a new thread that will handle this new connection
  setblockingmode(ClientSocket,MODE_NONBLOCKING);
  FClientThread := TRedundancyClient.Create(True, ClientSocket, FRemoveClientThread);
  Synchronize(@AddClientToMainThread);
  FClientThread.WakeUp;
end;

{ TRedundancyCtrl }

procedure TRedundancyCtrl.SetServerPriority(AValue: TIPv4Collection);
begin
  FServerPriority.Assign(AValue);
end;

procedure TRedundancyCtrl.setActive(AValue: Boolean);
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
  ct: LongInt;
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
    //FAcceptThread:=TAcceptThread.Create(true, FSocket, nil, nil, nil);
    //FAcceptThread.WakeUp;
  end else begin
    //close the socket...
    closesocket(FSocket);
    //destroy the threads from all clients and close the socket.
    //FAcceptThread.Terminate;
    //FAcceptThread.Destroy;
    CloseSocket(FSocket);

    //destroy all client threads...
    //for ct:=High(FClients) downto 0 do begin
    //  FClients[ct].Terminate;
    //  //FClients[ct].de;
    //end;
  end;
  FActive:=AValue;
end;

procedure TRedundancyCtrl.SetPort(AValue: Word);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
end;

constructor TRedundancyCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerPriority:=TIPv4Collection.Create;
end;

destructor TRedundancyCtrl.Destroy;
begin
  FServerPriority.Destroy;
  inherited Destroy;
end;

{ TIPv4Collection }

constructor TIPv4Collection.Create;
begin
  inherited Create(TIPv4CollectionItem);
end;

function TIPv4Collection.Add: TIPv4CollectionItem;
begin
  Result := TIPv4CollectionItem(inherited Add);
end;

{ TIPv4CollectionItem }

procedure TIPv4CollectionItem.SetIPv4Address1(AValue: String);
begin
  if FIPv4Address1=AValue then Exit;
  if (AValue.Trim()<>'') and (TTCP_UDPPort.ValidIPv4(AValue)=false) then exit;
  FIPv4Address1:=AValue;
end;

procedure TIPv4CollectionItem.SetIPv4Address2(AValue: String);
begin
  if FIPv4Address2=AValue then Exit;
  if (AValue.Trim()<>'') and (TTCP_UDPPort.ValidIPv4(AValue)=false) then exit;
  FIPv4Address2:=AValue;
end;

procedure TIPv4CollectionItem.SetIPv4Address3(AValue: String);
begin
  if FIPv4Address3=AValue then Exit;
  if (AValue.Trim()<>'') and (TTCP_UDPPort.ValidIPv4(AValue)=false) then exit;
  FIPv4Address3:=AValue;
end; 

procedure TIPv4CollectionItem.SetIPv4Address4(AValue: String);
begin
  if FIPv4Address4=AValue then Exit;
  if (AValue.Trim()<>'') and (TTCP_UDPPort.ValidIPv4(AValue)=false) then exit;
  FIPv4Address4:=AValue;
end;

function TIPv4CollectionItem.GetDisplayName: string;
begin
  if FIPv4Address1.trim<>'' then begin
    if TTCP_UDPPort.ValidIPv4(FIPv4Address1) then begin
      Result:=FIPv4Address1;
    end else begin
      Result:=FInvalidIPv4;
    end;
  end else
    Result:=FEmptyIPv4;

  if FIPv4Address2.trim<>'' then begin
    if TTCP_UDPPort.ValidIPv4(FIPv4Address2) then begin
      Result:=', '+FIPv4Address2;
    end else begin
      Result:=', '+FInvalidIPv4;
    end;
  end;

  if FIPv4Address3.trim<>'' then begin
    if TTCP_UDPPort.ValidIPv4(FIPv4Address3) then begin
      Result:=', '+FIPv4Address3;
    end else begin
      Result:=', '+FInvalidIPv4;
    end;
  end;

  if FIPv4Address4.trim<>'' then begin
    if TTCP_UDPPort.ValidIPv4(FIPv4Address4) then begin
      Result:=', '+FIPv4Address4;
    end else begin
      Result:=', '+FInvalidIPv4;
    end;
  end;
end;

procedure TIPv4CollectionItem.AssignTo(Dest: TPersistent);
var
  aDest: TIPv4CollectionItem;
begin
  if Dest is TIPv4CollectionItem then begin
     aDest:=Dest as TIPv4CollectionItem;

     inherited AssignTo(Dest);

     aDest.FIPv4Address1 := FIPv4Address1;
     aDest.FIPv4Address2 := FIPv4Address2;
     aDest.FIPv4Address3 := FIPv4Address3;
     aDest.FIPv4Address4 := FIPv4Address4;
  end else
    inherited AssignTo(Dest);
end;

end.
