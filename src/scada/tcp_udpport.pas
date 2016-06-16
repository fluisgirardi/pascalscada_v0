{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit que implementa um socket TCP/UDP sobre IP cliente.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a socket client TCP/UDP over IP.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit tcp_udpport;

{$I ../common/delphiver.inc}

interface

uses
  Classes, CommPort, commtypes, socket_types, CrossEvent, MessageSpool, syncobjs
  {$IF defined(WIN32) or defined(WIN64)} //delphi or lazarus over windows
    , Windows,
    {$IFDEF FPC}
    WinSock2,
    {$ELSE}
    WinSock,
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

  TConnectEvent=procedure(var Ok:Boolean) of object;

  { TConnectThread }

  TConnectThread = class(TCrossThread)
  private
    FConnectSocket: TConnectEvent;
    FEnd,FSomethingToDo:TCrossEvent;
    FReconnectSocket: TConnectEvent;
    FMessageQueue:TMessageSpool;
    ReconnectTimerRunning:longint;
  protected
    procedure Execute; override;
  public
    constructor Create;
    procedure Terminate;
    procedure Connect;
    procedure WaitThenConnect(Interval:Integer);
    procedure WaitEnd;
    procedure DisableAutoReconnect;
  published
    property ConnectSocket:TConnectEvent read FConnectSocket write FConnectSocket;
    property ReconnectSocket:TConnectEvent read FReconnectSocket write FReconnectSocket;
  end;

  {$IFDEF PORTUGUES}
  {:
  @abstract(Driver genérico para portas TCP/UDP sobre IP. Atualmente funcionando
            para Windows, Linux e FreeBSD.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TCommPortDriver)
  }
  {$ELSE}
  {:
  @abstract(TCP/UDP over IP client port driver. Currently working on Windows,
            Linux and FreeBSD.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @seealso(TCommPortDriver)
  }
  {$ENDIF}

  { TTCP_UDPPort }

  TTCP_UDPPort = class(TCommPortDriver)
  private
    FHostName:AnsiString;
    FPortNumber:LongInt;
    FReconnectInterval,
    FTimeout:LongInt;
    FSocket:Tsocket;
    FPortType:TPortType;
    FExclusiveReaded:Boolean;
    FEnableAutoReconnect:Integer;
    FReconnectRetriesLimit:Cardinal;
    FConnectThread:TConnectThread;
    FSocketMutex:syncobjs.TCriticalSection;
    procedure DoReconnect;
    function GetEnableAutoReconect: Boolean;
    procedure SetHostname(target:Ansistring);
    procedure SetPortNumber(pn:LongInt);
    procedure SetTimeout(t:LongInt);
    procedure SetPortType(pt:TPortType);
    procedure SetExclusive(b:Boolean);

    procedure setEnableAutoReconnect(v:Boolean);
    function GetReconnectInterval:Integer;
    procedure SetReconnectInterval(v:Integer);
  protected
    //: @exclude
    FReconnectRetries:Integer;
    //: @exclude
    procedure Loaded; override;
    //: @seealso(TCommPortDriver.Read)
    procedure Read(Packet:PIOPacket); override;
    //: @seealso(TCommPortDriver.Write)
    procedure Write(Packet:PIOPacket); override;
    //: @seealso(TCommPortDriver.NeedSleepBetweenRW)
    procedure NeedSleepBetweenRW; override;
    //: @seealso(TCommPortDriver.PortStart)
    procedure PortStart(var Ok:Boolean); override;
    //: @seealso(TCommPortDriver.PortStop)
    procedure PortStop(var Ok:Boolean); override;
    //: @seealso(TCommPortDriver.ComSettingsOK)
    function  ComSettingsOK:Boolean; override;
    //: @seealso(TCommPortDriver.ClearALLBuffers)
    procedure ClearALLBuffers; override;

    //: @seealso(TCommPortDriver.DoPortDisconnected)
    procedure DoPortDisconnected(sender: TObject); override;
    //: @seealso(TCommPortDriver.DoPortOpenError)
    procedure DoPortOpenError(sender: TObject); override;

    procedure connectSocket(var Ok: Boolean);
    procedure reconnectSocket(var Ok:Boolean);
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;

    function ReallyActive: Boolean; override;
  published
    {$IFDEF PORTUGUES}
    //: Nome ou endereço do servidor onde se deseja conectar.
    {$ELSE}
    //: Hostname or address of the server to connect.
    {$ENDIF}
    property Host:AnsiString read FHostName write SetHostname nodefault;

    {$IFDEF PORTUGUES}
    //: Porta do servidor que se deseja conectar. Para Modbus TCP use 502 e para Siemens ISOTCP use 102.
    {$ELSE}
    //: Server port to connect. To use Modbus, set this to 502 and to use Siemens ISOTCP set it to 102.
    {$ENDIF}
    property Port:LongInt read FPortNumber write SetPortNumber default 102;

    {$IFDEF PORTUGUES}
    //: Timeout em milisegundos para operações de leitura/escrita.
    {$ELSE}
    //: Timeout in milliseconds to I/O operations.
    {$ENDIF}
    property Timeout:LongInt read FTimeout write SetTimeout default 1000;

    {$IFDEF PORTUGUES}
    {:
    Tipo da porta (TCP ou UDP).
    @seealso(TPortType).
    }
    {$ELSE}
    {:
    Port kind (TCP or UDP).
    @seealso(TPortType).
    }
    {$ENDIF}
    property PortType:TPortType read FPortType write SetPortType default ptTCP;


    {$IFDEF PORTUGUES}
    //: Informa se a porta é de acesso exclusivo (evita que a porta seja aberta em tempo de desenvolvimento).
    {$ELSE}
    //: Tells if the communication port is exclusive (avoid it to be opened in design time).
    {$ENDIF}
    property ExclusiveDevice:Boolean read FExclusiveDevice write SetExclusive;


    {$IFDEF PORTUGUES}
    //: Informa se a porta deve se auto reconectar após uma perda ou falha de conexão.
    {$ELSE}
    //: Enables the auto reconnection if a connection is lost or failed.
    {$ENDIF}
    property EnableAutoReconnect:Boolean read GetEnableAutoReconect write setEnableAutoReconnect  stored true default true;

    {$IFDEF PORTUGUES}
    //: Define o tempo após a perda de conexão a porta deve tentar reconectar. Tempo em milisegundos.
    {$ELSE}
    //: Time to retry a lost connection in milliseconds.
    {$ENDIF}
    property ReconnectRetryInterval:Integer read GetReconnectInterval write SetReconnectInterval stored true default 5000;

    {$IFDEF PORTUGUES}
    //: Limite máximo de tentativas de reconexão. O contador de tentativas é reiniciado após uma conexão com sucesso.
    {$ELSE}
    //: Maximum limit of connection retries.
    {$ENDIF}
    property ReconnectRetriesLimit:Cardinal read FReconnectRetriesLimit write FReconnectRetriesLimit stored true default 0;

    //: @seealso TCommPortDriver.OnCommPortOpened
    property OnCommPortOpened;
    //: @seealso TCommPortDriver.OnCommPortOpenError
    property OnCommPortOpenError;
    //: @seealso TCommPortDriver.OnCommPortClosed
    property OnCommPortClosed;
    //: @seealso TCommPortDriver.OnCommPortCloseError
    property OnCommPortCloseError;
    //: @seealso TCommPortDriver.OnCommErrorReading
    property OnCommErrorReading;
    //: @seealso TCommPortDriver.OnCommErrorWriting
    property OnCommErrorWriting;
    //: @seealso TCommPortDriver.OnCommPortDisconnected
    property OnCommPortDisconnected;
  end;

implementation

uses hsstrings, dateutils, sysutils;

{ TConnectThread }

procedure TConnectThread.Execute;
var
  msg: TMSMsg;
  Ok:Boolean;
  ReconnectStarted:TDateTime;
  ReconnectInterval:Integer;
  msbetween: Int64;
begin
  ReconnectInterval:=0;
  ReconnectTimerRunning:=0;
  while not Terminated do begin

    if ReconnectTimerRunning<>0 then begin
      FSomethingToDo.WaitFor(ReconnectInterval);
    end else
      FSomethingToDo.WaitFor($FFFFFFFF);

    while FMessageQueue.PeekMessage(msg,0,100,true) do
      case msg.MsgID of
        0: if Assigned(FConnectSocket) then FConnectSocket(Ok);
        1: begin
          ReconnectTimerRunning:=1;
          ReconnectInterval:=PtrUint(msg.wParam);
          ReconnectStarted:=Now;
        end;
        2: begin
          ReconnectTimerRunning:=0;
        end;
      end;
    msbetween:=MilliSecondsBetween(now,ReconnectStarted);
    if (ReconnectTimerRunning=1) and (msbetween>=ReconnectInterval) then begin
      ReconnectStarted:=Now;
      Ok:=false;
      if Assigned(FReconnectSocket) then FReconnectSocket(Ok);
      if Ok then
        ReconnectTimerRunning:=0;

    end;
  end;
  FEnd.SetEvent;
end;

constructor TConnectThread.Create;
begin
  inherited Create(True);
  FMessageQueue:=TMessageSpool.Create;
  FEnd:=TCrossEvent.Create(true, false);
  FSomethingToDo:=TCrossEvent.Create(false, false);
end;

procedure TConnectThread.Terminate;
begin
  inherited Terminate;
  FSomethingToDo.SetEvent;
end;

procedure TConnectThread.Connect;
begin
  FMessageQueue.PostMessage(0,nil,nil,false);
  while not FSomethingToDo.SetEvent do;
end;

procedure TConnectThread.WaitThenConnect(Interval: Integer);
var
  res:LongInt;
begin
  interlockedexchange(res,ReconnectTimerRunning);
  if res = 1 then exit;
  FMessageQueue.PostMessage(1,Pointer(PtrUInt(Interval)),nil,false);
  while not FSomethingToDo.SetEvent do;
end;

procedure TConnectThread.WaitEnd;
begin
  while not (FEnd.WaitFor(5)=wrSignaled) do
    CheckSynchronize(5);
end;

procedure TConnectThread.DisableAutoReconnect;
begin
  FMessageQueue.PostMessage(2,nil,nil,false);
  while not FSomethingToDo.SetEvent do;
end;

constructor TTCP_UDPPort.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FPortNumber:=102;
  FTimeout:=1000;
  FSocket:=0;
  FEnableAutoReconnect:=1;
  FReconnectRetriesLimit:=0;
  FReconnectInterval:=5000;

  FSocketMutex:=Syncobjs.TCriticalSection.Create;

  FConnectThread:=TConnectThread.Create;
  FConnectThread.ConnectSocket:=@connectSocket;
  FConnectThread.ReconnectSocket:=@reconnectSocket;
  FConnectThread.WakeUp;
end;

destructor  TTCP_UDPPort.Destroy;
begin
  inherited Destroy;
  FConnectThread.Terminate;
  FConnectThread.WaitEnd;
  FreeAndNil(FConnectThread);
  FreeAndNil(FSocketMutex);
end;

function TTCP_UDPPort.ReallyActive: Boolean;
var
  x:Tsocket;
begin
  {$IF defined(WINDOWS)}
    {$IF defined(CPU64)}
    InterLockedExchange64(x, FSocket);
    {$ELSE}
    InterLockedExchange(LongInt(x), LongInt(FSocket));
    {$ENDIF}
  {$ELSE}
  InterLockedExchange(x, FSocket);
  {$ENDIF}

  {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
  Result:=x>=0;
  {$ELSE}
  Result:=x<>INVALID_SOCKET;
  {$ENDIF}
end;

procedure TTCP_UDPPort.reconnectSocket(var Ok: Boolean);
begin
  InterLockedIncrement(FReconnectRetries);
  connectSocket(Ok);
end;

procedure TTCP_UDPPort.SetHostname(target:Ansistring);
begin
  DoExceptionInActive;
  FHostName:=target;
end;

procedure TTCP_UDPPort.SetPortNumber(pn:LongInt);
begin
  DoExceptionInActive;
  if (pn>=1) or (pn<=65535) then
    FPortNumber:=pn
  else
    raise Exception.Create(SportNumberRangeError);
end;

procedure TTCP_UDPPort.SetTimeout(t:LongInt);
begin
  DoExceptionInActive;
  FTimeout:=t;
end;

procedure TTCP_UDPPort.SetPortType(pt:TPortType);
begin
  DoExceptionInActive;
  FPortType:=pt;
end;

procedure TTCP_UDPPort.SetExclusive(b:Boolean);
var
  oldstate:Boolean;
begin
  if csReading in ComponentState then begin
    FExclusiveReaded:=b;
    exit;
  end;

  //only at design-time
  if csDesigning in ComponentState then begin
    //stores the old state.
    oldstate:=Active;
    //close the communication port.
    Active:=False;
    //set the new state.
    FExclusiveDevice := b;
    //restores the old state.
    Active:=oldstate;
  end else
    FExclusiveDevice := b;
end;

procedure TTCP_UDPPort.setEnableAutoReconnect(v:Boolean);
var
  x: Integer;
begin
  try
    InterLockedExchange(x, FEnableAutoReconnect);
    if (v=false) and (x<>0) then
      FConnectThread.DisableAutoReconnect;
  finally
    x:=0;
    if v then x:=1;
    InterLockedExchange(FEnableAutoReconnect,x);
  end;
end;

function  TTCP_UDPPort.GetReconnectInterval:Integer;
begin
  Result:=0;
  InterLockedExchange(Result,FReconnectInterval);
end;

procedure TTCP_UDPPort.SetReconnectInterval(v: Integer);
begin
  InterLockedExchange(FReconnectInterval,v);
end;

procedure TTCP_UDPPort.Loaded;
begin
  ExclusiveDevice:=FExclusiveReaded;
  inherited Loaded;
end;

procedure TTCP_UDPPort.Read(Packet:PIOPacket);
var
  lidos:LongInt;
  tentativas:Cardinal;
  incretries:Boolean;
begin
  tentativas := 0;
  lidos := 0;

  Packet^.Received := 0;
  while (Packet^.Received<Packet^.ToRead) and (tentativas<Packet^.ReadRetries) do begin
    try
      lidos := socket_recv(FSocket, @Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received, 0, FTimeout);
    finally
    end;

    if lidos<=0 then begin
      if not CheckConnection(Packet^.ReadIOResult, incretries, PActive, FSocket, @CommPortDisconected) then
        break;
      if incRetries then
        inc(tentativas);
    end else
      Packet^.Received := Packet^.Received + lidos;

  end;

  Packet^.ReadRetries := tentativas;
  if Packet^.ToRead>Packet^.Received then begin
    Packet^.ReadIOResult := iorTimeOut;
    if PClearBufOnErr then InternalClearALLBuffers;
  end else
    Packet^.ReadIOResult := iorOK;

  if Packet^.ReadIOResult<>iorOK then
    CommError(false, Packet^.ReadIOResult);
end;

procedure TTCP_UDPPort.Write(Packet:PIOPacket);
var
  escritos:LongInt;
  tentativas:Cardinal;
  incretries:Boolean;
begin
  tentativas := 0;
  escritos := 0;

  Packet^.Written := 0;
  while (Packet^.Written<Packet^.ToWrite) and (tentativas<Packet^.WriteRetries) do begin
    try
      escritos := socket_send(FSocket, @Packet^.BufferToWrite[Packet^.Written], Packet^.ToWrite-Packet^.Written, 0, FTimeout);
    finally
    end;

    if escritos<=0 then begin
      if not CheckConnection(Packet^.ReadIOResult, incretries, PActive, FSocket, @CommPortDisconected) then
        break;
      if incRetries then
        inc(tentativas);
    end else
      Packet^.Written := Packet^.Written + escritos;
  end;

  Packet^.WriteRetries := tentativas;
  if Packet^.ToWrite>Packet^.Written then begin
    Packet^.WriteIOResult := iorTimeOut;
    if PClearBufOnErr then
      InternalClearALLBuffers;
  end else
    Packet^.WriteIOResult := iorOK;

  if Packet^.WriteIOResult<>iorOK then
    CommError(true, Packet^.WriteIOResult);
end;

procedure TTCP_UDPPort.NeedSleepBetweenRW;
begin
  if FDelayBetweenCmds>0 then
    Sleep(FDelayBetweenCmds);
end;

procedure TTCP_UDPPort.PortStart(var Ok:Boolean);
begin
  if ([csDesigning]*ComponentState=[]) or FExclusiveDevice then
    FConnectThread.Connect;
  Ok:=true;
end;

procedure TTCP_UDPPort.PortStop(var Ok:Boolean);
var
  buffer:BYTES;
  lidos:LongInt;
begin
  FSocketMutex.Enter;
  try
    if {$IFDEF WINDOWS}FSocket<>INVALID_SOCKET{$ELSE}FSocket>=0{$ENDIF} then begin
      SetLength(buffer,5);
      {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
      fpshutdown(FSocket,SHUT_WR);
      lidos := fprecv(FSocket, @Buffer[0], 1, MSG_PEEK);
      while lidos>0 do begin
        lidos := fprecv(FSocket, @Buffer[0], 1, 0);
        lidos := fprecv(FSocket, @Buffer[0], 1, MSG_PEEK);
      end;
      {$ELSE}
      Shutdown(FSocket,1);
      lidos := Recv(FSocket, Buffer[0], 1, MSG_PEEK);
      while lidos>0 do begin
        lidos := Recv(FSocket, Buffer[0], 1, 0);
        lidos := Recv(FSocket, Buffer[0], 1, MSG_PEEK);
      end;
      {$IFEND}
      CloseSocket(FSocket);
    end;
    PActive:=false;
    Ok:=true;
    {$IF defined(WINDOWS)}
      {$IF defined(CPU64)}
      InterLockedExchange64(FSocket, INVALID_SOCKET);
      {$ELSE}
      InterLockedExchange(LongInt(FSocket), LongInt(INVALID_SOCKET));
      {$ENDIF}
    {$ELSE}
    InterLockedExchange(FSocket, -1);
    {$ENDIF}
    SetLength(Buffer,0);
  finally
    FSocketMutex.Leave;
  end;
end;

function  TTCP_UDPPort.ComSettingsOK:Boolean;
begin
  Result := (FHostName<>'') and ((FPortNumber>0) and (FPortNumber<65536));
end;

procedure TTCP_UDPPort.DoReconnect;
var
  RetriesLimit, Retries, Interval, x: Integer;
begin
  InterLockedExchange(RetriesLimit, FReconnectRetriesLimit);
  InterLockedExchange(Retries, FReconnectRetries);
  InterLockedExchange(Interval, FReconnectInterval);

  InterLockedExchange(x, FEnableAutoReconnect);
  if (x<>0) and ((RetriesLimit=0) or (Retries<=RetriesLimit)) then
    FConnectThread.WaitThenConnect(Interval);
end;

function TTCP_UDPPort.GetEnableAutoReconect: Boolean;
var
  x: Integer;
begin
  InterLockedExchange(x,FEnableAutoReconnect);
  Result:=x<>0;
end;

procedure TTCP_UDPPort.DoPortDisconnected(sender: TObject);
begin
  DoReconnect;

  inherited DoPortDisconnected(sender);
end;

procedure TTCP_UDPPort.DoPortOpenError(sender: TObject);
begin
  DoReconnect;

  inherited DoPortOpenError(sender);
end;

procedure TTCP_UDPPort.connectSocket(var Ok: Boolean);
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

  flag, bufsize, sockType, sockProto:LongInt;
  ASocket:Tsocket;
begin
  Ok:=false;
  FSocketMutex.Enter;
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
      //if the name resolution is being done using FPC or a Delphi 2009 or older
      //uses the ansistring version, otherwise uses the unicode version.
      {$IF defined(FPC) OR (not defined(DELPHI2009_UP))}
      ServerAddr := GetHostByName(PAnsiChar(FHostName));
      {$ELSE}
      ServerAddr := GetHostByName(PAnsiChar(AnsiString(FHostName)));
      {$IFEND}
      if ServerAddr=nil then begin
        PActive:=false;
        RefreshLastOSError;
        exit;
      end;
    {$IFEND}

    //##########################################################################
    // RESOLUCAO DE NOMES SOBRE LINUX/FREEBSD e outros.
    // NAME RESOLUTION OVER LINUX/FREEBSD and others.
    //##########################################################################
    {$IF defined(FPC) and defined(UNIX)}
      if not GetHostByName(FHostName,{%H-}ServerAddr) then begin
        ServerAddr.Addr:=StrToHostAddr(FHostName);
        if ServerAddr.Addr.s_addr=0 then begin
          PActive:=false;
          RefreshLastOSError;
          exit;
        end;
      end;
    {$IFEND}

    //##########################################################################
    // CRIA O SOCKET
    // CREATE THE SOCKET.
    //##########################################################################
    case FPortType of
      ptTCP:
        begin
          sockProto := IPPROTO_TCP;
          sockType  := SOCK_STREAM;
        end;
      ptUDP:
        begin
          sockProto := IPPROTO_UDP;
          sockType  := SOCK_DGRAM;
        end
      else begin
        PActive:=false;
        exit;
      end;
    end;

    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    //UNIX and WINDOWS CE
    ASocket := fpSocket(PF_INET, sockType, sockProto);

    if ASocket<0 then begin
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;
    {$ELSE}
    //WINDOWS
    ASocket :=   Socket(PF_INET, sockType, sockProto);

    if ASocket=INVALID_SOCKET then begin
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;
    {$IFEND}

    //##########################################################################
    //SETA O MODO DE OPERACAO DE NAO BLOQUEIO DE CHAMADA.
    //SET THE NON-BLOCKING OPERATING MODE OF THE SOCKET
    //##########################################################################
    setblockingmode(ASocket,MODE_NONBLOCKING);

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
    bufsize := 1024*16;
    //UNIX AND WINDOWS CE
    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    fpsetsockopt(ASocket, SOL_SOCKET,  SO_RCVBUF,    @bufsize,  sizeof(LongInt));
    fpsetsockopt(ASocket, SOL_SOCKET,  SO_SNDBUF,    @bufsize,  sizeof(LongInt));
    fpsetsockopt(ASocket, IPPROTO_TCP, TCP_NODELAY,  @flag,     sizeof(LongInt));
    {$IFEND}
    //WINDOWS
    {$IF defined(WIN32) or defined(WIN64)}
    setsockopt(ASocket, SOL_SOCKET,  SO_RCVBUF,    PAnsiChar(@bufsize), sizeof(LongInt));
    setsockopt(ASocket, SOL_SOCKET,  SO_SNDBUF,    PAnsiChar(@bufsize), sizeof(LongInt));
    setsockopt(ASocket, IPPROTO_TCP, TCP_NODELAY,  PAnsiChar(@flag),    sizeof(LongInt));
    {$IFEND}

    //##########################################################################
    //CONFIGURA E ENDERECO QUE O SOCKET VAI CONECTAR
    //SETS THE TARGET ADDRESS TO SOCKET CONNECT
    //##########################################################################
    channel.sin_family      := AF_INET;            //FAMILY
    channel.sin_port        := htons(FPortNumber); //PORT NUMBER

    {$IF defined(FPC) AND defined(UNIX)}
    channel.sin_addr.S_addr := longword(htonl(LongWord(ServerAddr.Addr.s_addr)));
    {$IFEND}

    {$IF defined(FPC) AND defined(WINCE)}
    channel.sin_addr := StrToNetAddr(FHostName);
    {$IFEND}

    {$IF defined(WIN32) OR defined(WIN64)}
    channel.sin_addr.S_addr := PInAddr(ServerAddr^.h_addr)^.S_addr;
    {$IFEND}

    if connect_with_timeout(ASocket,@channel,sizeof(channel),FTimeout)<>0 then begin
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;
    Ok:=true;
    PActive:=true;
    InterLockedExchange(FReconnectRetries, 0);
    {$IF defined(WINDOWS)}
    {$IF defined(CPU64)}
    InterLockedExchange64(FSocket,ASocket);
    {$ELSE}
    InterLockedExchange(LongInt(FSocket),LongInt(ASocket));
    {$ENDIF}
    {$ELSE}
    InterLockedExchange(FSocket,ASocket);
    {$ENDIF}
    DoPortOpened(Self);
  finally
    FSocketMutex.Leave;
    if not Ok then begin
      CloseSocket(FSocket);
      {$IF defined(WINDOWS)}
        {$IF defined(CPU64)}
        InterLockedExchange64(FSocket, INVALID_SOCKET);
        {$ELSE}
        InterLockedExchange(LongInt(FSocket), LongInt(INVALID_SOCKET));
        {$ENDIF}
      {$ELSE}
      InterLockedExchange(FSocket, -1);
      {$ENDIF}
      CommPortOpenError;
    end;
  end;
end;

procedure TTCP_UDPPort.ClearALLBuffers;
begin

end;

end.
