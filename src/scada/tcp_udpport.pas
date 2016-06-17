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

  { TConnectThread }

  TConnectThread = class(TCrossThread)
  private
    FActive:Boolean;
    FCheckSocket,
    FConnectSocket,
    FDisconnectSocket: TConnectEvent;
    FEnd:TCrossEvent;
    FReconnectSocket: TConnectEvent;
    FMessageQueue:TMessageSpool;

    FAutoReconnect,
    FReconnectInterval,
    FReconnectRetries:Integer;
    function GetEnableAutoReconnect:Boolean;

    function GetReconnectInterval:Integer;
    function GetReconnectRetries:Integer;

    procedure SetReconnectInterval(aValue:Integer);
    procedure SetEnableAutoReconnect(aValue:Boolean);
    procedure SetReconnectRetries(AValue: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create;
    procedure Connect;
    procedure Disconnect;
    procedure NotifyDisconnect;
    procedure WaitEnd;
    procedure StopAutoReconnect;
  published
    property ConnectSocket:TConnectEvent read FConnectSocket write FConnectSocket;
    property ReconnectSocket:TConnectEvent read FReconnectSocket write FReconnectSocket;
    property DisconnectSocket:TConnectEvent read FDisconnectSocket write FDisconnectSocket;
    property CheckSocket:TConnectEvent read FCheckSocket write FCheckSocket;

    property EnableAutoReconnect:Boolean read GetEnableAutoReconnect write SetEnableAutoReconnect;
    property ReconnectInterval:Integer read GetReconnectInterval write SetReconnectInterval;

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
    FTimeout:LongInt;
    FSocket:Tsocket;
    FPortType:TPortType;
    FExclusiveReaded:Boolean;

    FConnectThread:TConnectThread;
    procedure DoReconnect;

    function  GetEnableAutoReconect: Boolean;
    function  GetReconnectInterval:Integer;

    procedure setEnableAutoReconnect(v:Boolean);
    procedure SetReconnectInterval(v:Integer);

    procedure SetHostname(target:Ansistring);
    procedure SetPortNumber(pn:LongInt);
    procedure SetTimeout(t:LongInt);
    procedure SetPortType(pt:TPortType);
    procedure SetExclusive(b:Boolean);
  protected
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
    procedure CloseMySocket(var closed: Boolean);
    procedure reconnectSocket(var Ok:Boolean);
    procedure CheckSocket(var Ok:Boolean);
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

function TConnectThread.GetEnableAutoReconnect: Boolean;
var
  res: cardinal;
begin
  InterLockedExchange(res,FAutoReconnect);
  Result:=res=1;
end;

function TConnectThread.GetReconnectInterval: Integer;
begin
  Result:=0;
  InterLockedExchange(Result,FReconnectInterval);
end;

function TConnectThread.GetReconnectRetries: Integer;
begin
  Result:=0;
  InterLockedExchange(Result,FReconnectRetries);
end;

procedure TConnectThread.SetReconnectInterval(aValue: Integer);
begin
  InterLockedExchange(FReconnectInterval,aValue);
end;

procedure TConnectThread.SetEnableAutoReconnect(aValue: Boolean);
begin
  if aValue then
    InterLockedExchange(FAutoReconnect,1)
  else
    InterLockedExchange(FAutoReconnect,0);
end;

procedure TConnectThread.SetReconnectRetries(AValue: Integer);
begin
  InterLockedExchange(FReconnectRetries,AValue);
end;

procedure TConnectThread.Execute;
var
  msg: TMSMsg;
  Ok, ReconnectTimerRunning:Boolean;
  ReconnectStarted:TDateTime;
  msbetween: Int64;
begin
  ReconnectTimerRunning:=false;
  while not Terminated do begin
    while FMessageQueue.PeekMessage(msg,0,100,true) do begin
      Ok:=false;
      case msg.MsgID of
        0: begin
          FActive:=true;
          if Assigned(FConnectSocket) then begin
             FConnectSocket(Ok);
             if (Ok) then
               ReconnectTimerRunning:=false
             else
               if(FAutoReconnect=1) then begin
                  if ReconnectTimerRunning=false then
                    ReconnectStarted:=Now;
                  ReconnectTimerRunning:=true;
               end;
           end;
        end;
        1: begin
          if ReconnectTimerRunning=false then
            ReconnectStarted:=Now;
          ReconnectTimerRunning:=true;
        end;
        2: begin
          ReconnectTimerRunning:=false;
        end;
        3: begin
            FActive:=false;
            if Assigned(DisconnectSocket) then
              DisconnectSocket(Ok);
            ReconnectTimerRunning:=false;
           end;
      end;
    end;

    if FActive then begin
      OK:=true;
      if assigned(FCheckSocket) then
        FCheckSocket(Ok);
      if not ok then begin
        if ReconnectTimerRunning=false then
          ReconnectStarted:=Now;
        ReconnectTimerRunning:=true;
      end;
    end;


    msbetween:=MilliSecondsBetween(now,ReconnectStarted);
    if (FAutoReconnect=1) and (ReconnectTimerRunning) and (msbetween>=ReconnectInterval) then begin
      ReconnectStarted:=Now;
      Ok:=false;
      if Assigned(FReconnectSocket) then FReconnectSocket(Ok);
      if Ok then
        ReconnectTimerRunning:=false;
    end;

    Sleep(250);
  end;
  FEnd.SetEvent;
end;

constructor TConnectThread.Create;
begin
  inherited Create(True);
  FMessageQueue:=TMessageSpool.Create;
  FEnd:=TCrossEvent.Create(true, false);
end;

procedure TConnectThread.Connect;
begin
  FMessageQueue.PostMessage(0,nil,nil,false);
end;

procedure TConnectThread.Disconnect;
begin
  FMessageQueue.PostMessage(3,nil,nil,false);
end;

procedure TConnectThread.NotifyDisconnect;
begin
  FMessageQueue.PostMessage(1,nil,nil,false);
end;

procedure TConnectThread.WaitEnd;
begin
  while not (FEnd.WaitFor(5)=wrSignaled) do
    CheckSynchronize(5);
end;

procedure TConnectThread.StopAutoReconnect;
begin
  FMessageQueue.PostMessage(2,nil,nil,false);
end;

constructor TTCP_UDPPort.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FPortNumber:=102;
  FTimeout:=1000;
  {$IF defined(WINDOWS)}
  FSocket:=INVALID_SOCKET;
  {$ELSE}
  FSocket:=-1;
  {$ENDIF}

  FConnectThread:=TConnectThread.Create;
  FConnectThread.EnableAutoReconnect:=true;
  FConnectThread.ReconnectInterval:=5000;
  FConnectThread.ConnectSocket:=@connectSocket;
  FConnectThread.ReconnectSocket:=@reconnectSocket;
  FConnectThread.DisconnectSocket:=@CloseMySocket;
  FConnectThread.CheckSocket:=@CheckSocket;
  FConnectThread.WakeUp;
end;

destructor  TTCP_UDPPort.Destroy;
begin
  inherited Destroy;
  FConnectThread.Terminate;
  FConnectThread.WaitEnd;
  FreeAndNil(FConnectThread);
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
  CloseMySocket(Ok);
  connectSocket(Ok);
end;

procedure TTCP_UDPPort.CheckSocket(var Ok:Boolean);
begin
  Ok:=ReallyActive;
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
begin
    FConnectThread.EnableAutoReconnect:=v;
    if v=false then
      FConnectThread.StopAutoReconnect;
 end;

function  TTCP_UDPPort.GetReconnectInterval:Integer;
begin
  Result:=FConnectThread.ReconnectInterval;
end;

procedure TTCP_UDPPort.SetReconnectInterval(v: Integer);
begin
  FConnectThread.ReconnectInterval:=v;
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
  incretries, donothing:Boolean;
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
      if not CheckConnection(Packet^.ReadIOResult, incretries, FSocket, @CloseMySocket, @CommPortDisconected) then
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
  incretries, donothing:Boolean;
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
      if not CheckConnection(Packet^.ReadIOResult, incretries, FSocket, @CloseMySocket, @CommPortDisconected) then
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

procedure TTCP_UDPPort.CloseMySocket(var closed:Boolean);
var
  buffer:BYTES;
  lidos:LongInt;
  ASocket: Tsocket;
begin
  {$IF defined(WINDOWS)}
    {$IF defined(CPU64)}
    InterLockedExchange64(ASocket, FSocket);
    InterLockedExchange64(FSocket, INVALID_SOCKET);
    {$ELSE}
    InterLockedExchange(LongInt(ASocket), LongInt(FSocket));
    InterLockedExchange(LongInt(FSocket), LongInt(INVALID_SOCKET));
    {$ENDIF}
  {$ELSE}
  InterLockedExchange(ASocket, FSocket);
  InterLockedExchange(FSocket, -1);
  {$ENDIF}

  closed:=false;
  if {$IFDEF WINDOWS}ASocket<>INVALID_SOCKET{$ELSE}ASocket>=0{$ENDIF} then begin
    SetLength(buffer,5);
    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    fpshutdown(ASocket,SHUT_WR);
    lidos := fprecv(ASocket, @Buffer[0], 1, MSG_PEEK);
    while lidos>0 do begin
      lidos := fprecv(ASocket, @Buffer[0], 1, 0);
      lidos := fprecv(ASocket, @Buffer[0], 1, MSG_PEEK);
    end;
    {$ELSE}
    Shutdown(ASocket,1);
    lidos := Recv(ASocket, Buffer[0], 1, MSG_PEEK);
    while lidos>0 do begin
      lidos := Recv(ASocket, Buffer[0], 1, 0);
      lidos := Recv(ASocket, Buffer[0], 1, MSG_PEEK);
    end;
    {$IFEND}
    CloseSocket(ASocket);
  end;
  closed:=true;
  SetLength(Buffer,0);
end;

procedure TTCP_UDPPort.PortStop(var Ok:Boolean);
begin
  if ([csDesigning]*ComponentState=[]) or FExclusiveDevice then
    FConnectThread.Disconnect;
  Ok:=true;
end;

function  TTCP_UDPPort.ComSettingsOK:Boolean;
begin
  Result := (FHostName<>'') and ((FPortNumber>0) and (FPortNumber<65536));
end;

procedure TTCP_UDPPort.DoReconnect;
begin
    FConnectThread.NotifyDisconnect;
end;

function TTCP_UDPPort.GetEnableAutoReconect: Boolean;
begin
  Result:=FConnectThread.EnableAutoReconnect;
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
        exit;
      end;
    end;

    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    //UNIX and WINDOWS CE
    ASocket := fpSocket(PF_INET, sockType, sockProto);

    if ASocket<0 then begin
      RefreshLastOSError;
      exit;
    end;
    {$ELSE}
    //WINDOWS
    ASocket :=   Socket(PF_INET, sockType, sockProto);

    if ASocket=INVALID_SOCKET then begin
      RefreshLastOSError;
      exit;
    end;
    {$IFEND}

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

    if connect_without_timeout(ASocket,@channel,sizeof(channel))<>0 then begin
      RefreshLastOSError;
      exit;
    end;

    //##########################################################################
    //SETA O MODO DE OPERACAO DE NAO BLOQUEIO DE CHAMADA.
    //SET THE NON-BLOCKING OPERATING MODE OF THE SOCKET
    //##########################################################################
    setblockingmode(ASocket,MODE_NONBLOCKING);

    Ok:=true;
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
    if not Ok then begin
      CloseMySocket(Ok);
      Ok:=false;
      CommPortOpenError;
    end;
  end;
end;

procedure TTCP_UDPPort.ClearALLBuffers;
begin

end;

end.
