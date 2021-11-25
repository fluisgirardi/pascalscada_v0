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
  TClientAddresses = packed record
    HostAddr1,
    HostAddr2,
    HostAddr3,
    HostAddr4:TIn_addr;
  end;

  TRedundancyCtrl = class;

  TClientHi = packed record
    Cmd:Word;
    HostUUID:TGuid;
  end;
  PClientHi = ^TClientHi;

  TServerQuit = TClientHi;
  PServerQuit = ^TServerQuit;

  TClientGoto = packed record
    Cmd:Word;
    HostUUID:TGuid;
    ClientAddr:TClientAddresses;
  end;

  TPingCmd = packed record
    Cmd:Word;
    HostUUID:TGuid;
    PingNr:LongWord;
  end;
  PPingCmd = ^TPingCmd;

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
    FOwner:TComponent;
    procedure LaunchNewThread; override;
  public
    constructor Create(CreateSuspended: Boolean;
                       aOwner:TRedundancyCtrl;
                       ServerSocket: TSocket;
                       AddClientThread, RemoveClientThread: TNotifyEvent);
  end;

  //server socket,

  { TRedundancyClient }
  TClientState  = (Unitialized, //new client arrives
                   HiReceived,  //new client identified said hello
                   Quit,        //Unidentified client or another error detected. Quit initiated.
                   QuitDone,    //Unidentified client or another error detected. Quit sent
                   GoodbyeGoto, //new identified client connected to a non active server. Redirecting to the correct server...
                   Ping,        //ping cmd must be done
                   PingDone,    //ping cmd done, wait for a pong
                   PongDone,    //Pong cmd received
                   WaitingPong, //waiting for a pong cmd
                   Ready,       //client is ready, waiting for a new command.
                   ReadyWaiting //client is ready, waiting to be marked as active master
                  );

  { TRedundancyServerClient }

  TRedundancyServerClient = class (TSocketClientThread)
  private
    CurState:TClientState;
    FServerPriority:TIPv4Collection;
    FOwner:TRedundancyCtrl;
    FLastCmdSentAt:TDateTime;
    FPingSeq:LongWord;
    procedure LoadServerPriority;
    function ImTheActiveServer:Boolean;
    function GetActiveServerIPv4Addr:TClientAddresses;
    function ServerGUID:TGuid;
  protected
    procedure ThreadLoop; override;
    constructor Create(CreateSuspended: Boolean;
                       aOwner:TRedundancyCtrl;
                       ClientSocket: TSocket;
                       ClientSockinfo: TSockAddr;
                       RemoveClientThread: TNotifyEvent);
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

const
  ClientHi:Word   = $0101;
  ServerQuit:Word = $DEAD;
  ClientGoto      = $6070;
  PingReq         = $1001;
  PingRply        = $0110;

resourcestring
  FInvalidIPv4 = '(invalid)';
  FEmptyIPv4   = '(empty)';

implementation

uses DateUtils;

{ TRedundancyServerClient }

procedure TRedundancyServerClient.LoadServerPriority;
begin
  if Assigned(FOwner) then
    FServerPriority.Assign(FOwner.FServerPriority);
end;

function TRedundancyServerClient.ImTheActiveServer: Boolean;
begin
  //TODO
end;

function TRedundancyServerClient.GetActiveServerIPv4Addr: TClientAddresses;
begin
  //TODO
end;

function TRedundancyServerClient.ServerGUID: TGuid;
begin
  //TODO
end;

procedure TRedundancyServerClient.ThreadLoop;
var
  buffer:array[0..1023] of Byte;
  ImAtList, found: Boolean;
  colItem: TIPv4CollectionItem;
  i: Integer;
  msgQ: TServerQuit;
  msgGoto: TClientGoto;
  msgPing: TPingCmd;
begin
  while not Terminated do begin
    case CurState of
      Unitialized: begin
        FLastCmdSentAt:=Now;
        FPingSeq:=1;
        if (socket_recv(FSocket,@buffer[0],sizeof(TClientHi),MSG_NOSIGNAL, 1000)=sizeof(TClientHi)) and
           (PClientHi(@buffer[0])^.Cmd = ClientHi) and
           (IsEqualGUID(PClientHi(@buffer[0])^.HostUUID, ServerGUID))
           then begin
             if FClientInfo.sa_family=AF_INET then begin
               Synchronize(@LoadServerPriority);
               found:=false;
               ImAtList:=false;
               for i:=0 to FServerPriority.Count-1 do begin
                 colItem:=TIPv4CollectionItem(FServerPriority.Items[i]);
                 //I'm a RedundancyServer, check if the incomming client is
                 //a redundancy server too
                 if ImAtList then begin
                   if (StrToNetAddr(colItem.FIPv4Address1).s_addr=FClientInfo.sin_addr.s_addr) or
                      (StrToNetAddr(colItem.FIPv4Address2).s_addr=FClientInfo.sin_addr.s_addr) or
                      (StrToNetAddr(colItem.FIPv4Address3).s_addr=FClientInfo.sin_addr.s_addr) or
                      (StrToNetAddr(colItem.FIPv4Address4).s_addr=FClientInfo.sin_addr.s_addr) then begin
                      found:=true;
                      break;
                   end;
                 end else begin
                   //Current machine belongs to redundancy servers set?
                   if IPv4BelongsToLocalHost(colItem.FIPv4Address1) or
                      IPv4BelongsToLocalHost(colItem.FIPv4Address2) or
                      IPv4BelongsToLocalHost(colItem.FIPv4Address3) or
                      IPv4BelongsToLocalHost(colItem.FIPv4Address4) then begin
                      ImAtList:=true;
                   end;
                 end;
               end;

               //new client is comming from a low priority server
               if found then begin
                 if ImTheActiveServer then begin
                   CurState:=Ready;
                 end else begin
                   CurState:=HiReceived;
                 end;
               end else begin
                 //I'm a redundancy partner?
                 if ImAtList then begin
                   //I'm the currenty redundancy server?
                   if ImTheActiveServer then begin
                     CurState:=Ready;
                   end else begin
                     CurState:=GoodbyeGoto;
                   end;
                 end else begin
                   CurState:=Quit;
                 end;
               end;
             end;
        end else
          CurState:=Quit;
      end;

      HiReceived:
        CurState:=ReadyWaiting;

      Quit: begin
        msgQ.Cmd:=ServerQuit;
        msgq.HostUUID:=ServerGUID;
        try
          socket_send(FSocket,@msgQ,SizeOf(msgQ),MSG_NOSIGNAL, 1000);
        except
        end;
        CurState:=QuitDone;
      end;

      QuitDone:
        Terminate;

      GoodbyeGoto: begin
        msgGoto.Cmd:=ClientGoto;
        msgGoto.HostUUID:=ServerGUID;
        msgGoto.ClientAddr:=GetActiveServerIPv4Addr;

        //if connection is dead, ignore this and mark this as quitdone, to terminate this thread;
        try
          socket_send(FSocket,@msgGoto,SizeOf(msgGoto),MSG_NOSIGNAL, 1000);
        except
        end;
        CurState:=QuitDone;
      end;

      Ping: begin
        msgPing.Cmd:=PingReq;
        msgPing.HostUUID:=ServerGUID;
        msgPing.PingNr:=FPingSeq;
        inc(FPingSeq);

        try
          if socket_send(FSocket,@msgPing,SizeOf(msgPing),MSG_NOSIGNAL, 1000)<SizeOf(msgPing) then
            CurState:=QuitDone;
        except
          CurState:=QuitDone;
        end;
        CurState:=WaitingPong;
      end;

      WaitingPong: begin
        if (socket_recv(FSocket,@buffer[0],sizeof(TPingCmd),MSG_NOSIGNAL, 10000)=sizeof(TPingCmd)) and
           (PPingCmd(@buffer[0])^.Cmd = PingRply) and
           (IsEqualGUID(PPingCmd(@buffer[0])^.HostUUID, ServerGUID)) and
           (PPingCmd(@buffer[0])^.PingNr = FPingSeq)
           then
             CurState:=ReadyWaiting
           else
             CurState:=QuitDone;
      end;

      Ready: begin
        if ImTheActiveServer then begin
          //TODO: now checks the queued updates cmds and send it to the clients
        end else
          CurState:=ReadyWaiting;
      end;

      ReadyWaiting: begin
        if ImTheActiveServer then
          CurState:=Ready
        else begin
          if MilliSecondsBetween(Now,FLastCmdSentAt)>10000 then
            CurState:=Ping;
        end;
        Sleep(10);
      end;
    end;
  end;
end;

constructor TRedundancyServerClient.Create(CreateSuspended: Boolean;
  aOwner: TRedundancyCtrl; ClientSocket: TSocket; ClientSockinfo: TSockAddr;
  RemoveClientThread: TNotifyEvent);
begin
  inherited Create(CreateSuspended, ClientSocket, ClientSockinfo,
    RemoveClientThread);
  FOwner:=aOwner;
  FServerPriority:=TIPv4Collection.Create;
end;

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
  FClientThread := TRedundancyServerClient.Create(True, FOwner as TRedundancyCtrl, ClientSocket, ClientSockInfo, FRemoveClientThread);
  Synchronize(@AddClientToMainThread);
  FClientThread.WakeUp;
end;

constructor TRedundancyAcceptSocket.Create(CreateSuspended: Boolean;
  aOwner: TRedundancyCtrl; ServerSocket: TSocket; AddClientThread,
  RemoveClientThread: TNotifyEvent);
begin
  inherited create(CreateSuspended, ServerSocket, AddClientThread, RemoveClientThread);
  FOwner:=aOwner;
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
