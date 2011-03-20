{:
  @abstract(Unit que implementa uma porta de comunicação TCP/UDP sobre IP cliente.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit tcp_udpport;

{$IFDEF FPC}
{$mode delphi}
{$IFDEF DEBUG}
  {$DEFINE FDEBUG}
{$ENDIF}
{$ENDIF}

{$I delphiver.inc}

interface

uses
  Classes, SysUtils, CommPort, commtypes
  {$IF defined(WIN32) or defined(WIN64)}
  , Windows, WinSock
  {$ELSE}
  {$IFDEF FPC}
  , Sockets {$IFDEF UNIX}, BaseUnix{$ENDIF}
            {$IFDEF FDEBUG}, LCLProc{$ENDIF}
            {$IFDEF WINCE}, WinSock{$endif}
  {$ENDIF}
  {$IFEND};

type
  {:
  Enumera os tipos de porta.
  @value ptTCP = Porta cliente do tipo TCP.
  @value ptUDP = Porta cliente do tipo UDP.
  }
  TPortType = (ptTCP, ptUDP);

  //@exclude
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400)}
  {$IFDEF UNIX}
  t_socklen = Sockets.TSockLen;
  {$ELSE}
  t_socklen = tOS_INT;
  {$ENDIF}
  {$ELSE}
  t_socklen = Integer;
  {$IFEND}
  {:
  @abstract(Driver genérico para portas TCP/UDP sobre IP. Atualmente funcionando
            para Windows, Linux e FreeBSD.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  @seealso(TCommPortDriver)
  }
  TTCP_UDPPort = class(TCommPortDriver)
  private
    FHostName:String;
    FPortNumber:Integer;
    FTimeout:Integer;
    FSocket:Tsocket;
    FPortType:TPortType;
    FExclusiveReaded:Boolean;
    procedure SetHostname(target:string);
    procedure SetPortNumber(pn:Integer);
    procedure SetTimeout(t:Integer);
    procedure SetPortType(pt:TPortType);
    procedure SetExclusive(b:Boolean);
    function  connect_with_timeout(sock:Tsocket; address:psockaddr; address_len:t_socklen; timeout:Integer):Integer;
    {$IFDEF UNIX}
    function  set_nonblock(fd:TSocket; block:boolean):Integer;
    {$ENDIF}
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
    {:
     Verifica o estado atual da conexao e sinaliza no pacote de resulta.
     @param(CommResult TIOResult. Estrutra do pedido a ser sinalizado com o novo
            estado da conexão.)
     @returns(@true caso esteja conectado.)
    }
    function  CheckConnection(var CommResult:TIOResult; var incRetries:Boolean):Boolean; virtual;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    //: Nome ou endereço do servidor onde se deseja conectar.
    property Host:String read FHostName write SetHostname nodefault;
    //: Porta do servidor que se deseja conectar. Para Modbus TCP use 502 e para Siemens ISOTCP use 102.
    property Port:Integer read FPortNumber write SetPortNumber default 102;
    //: Timeout(extra) em milisegundos para operações de leitura/escrita.
    property Timeout:Integer read FTimeout write SetTimeout default 1000;
    {:
    Tipo da porta.
    @seealso(TPortType).
    }
    property PortType:TPortType read FPortType write SetPortType default ptTCP;

    //: Porta de acesso exclusivo (evita que a porta seja aberta em tempo de desenvolvimento).
    property ExclusiveDevice:Boolean read FExclusiveDevice write SetExclusive;

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

uses hsstrings
{$ifdef fpc}
{$IFDEF UNIX}
     ,netdb,
     Unix
{$endif}
{$ENDIF}
     ;

constructor TTCP_UDPPort.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FPortNumber:=102;
  FTimeout:=1000;
  FSocket:=0;
end;

destructor  TTCP_UDPPort.Destroy;
begin
  inherited Destroy;
end;

procedure TTCP_UDPPort.SetHostname(target:string);
begin
  DoExceptionInActive;
  FHostName:=target;
end;

procedure TTCP_UDPPort.SetPortNumber(pn:Integer);
begin
  DoExceptionInActive;
  if (pn>=1) or (pn<=65535) then
    FPortNumber:=pn
  else
    raise Exception.Create(SportNumberRangeError);
end;

procedure TTCP_UDPPort.SetTimeout(t:Integer);
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

  oldstate:=PActive;
  Active:=false;
  FExclusiveDevice:=b;
  Active:=oldstate;
end;

procedure TTCP_UDPPort.Loaded;
begin
  ExclusiveDevice:=FExclusiveReaded;
  inherited Loaded;
end;

procedure TTCP_UDPPort.Read(Packet:PIOPacket);
var
  lidos:Integer;
  tentativas:Cardinal;
  incretries:Boolean;
begin
  tentativas := 0;
  lidos := 0;

  Packet^.Received := 0;
  while (Packet^.Received<Packet^.ToRead) and (tentativas<Packet^.ReadRetries) do begin
    try
      {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
      lidos := fprecv(FSocket, @Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received, 0);
      {$ELSE}
      lidos := Recv(FSocket, Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received, 0);
      {$IFEND}
    finally
    end;

    if lidos<0 then begin
      if CheckConnection(Packet^.ReadIOResult, incretries) then begin
        if incretries then inc(tentativas);
        continue;
      end else
        break;
    end else
      Packet^.Received := Packet^.Received + lidos;
    inc(tentativas);
  end;

  Packet^.ReadRetries := tentativas;
  if Packet^.ToRead>Packet^.Received then begin
    Packet^.ReadIOResult := iorTimeOut;
    if PClearBufOnErr then
      InternalClearALLBuffers;
  end else
    Packet^.ReadIOResult := iorOK;

  if Packet^.ReadIOResult<>iorOK then
    DoCommError(false, Packet^.ReadIOResult);
end;

procedure TTCP_UDPPort.Write(Packet:PIOPacket);
var
  escritos:Integer;
  tentativas:Cardinal;
  incretries:Boolean;
begin
  tentativas := 0;
  escritos := 0;

  Packet^.Wrote := 0;
  while (Packet^.Wrote<Packet^.ToWrite) and (tentativas<Packet^.WriteRetries) do begin
    try
      {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
      escritos := fpsend(FSocket, @Packet^.BufferToWrite[Packet^.Wrote], Packet^.ToWrite-Packet^.Wrote, 0);
      {$ELSE}
      escritos := Send(FSocket, Packet^.BufferToWrite[Packet^.Wrote], Packet^.ToWrite-Packet^.Wrote, 0);
      {$IFEND}
    finally
    end;

    if escritos<0 then begin
      if CheckConnection(Packet^.WriteIOResult, incretries) then begin
        if incretries then inc(tentativas);
        continue;
      end else
        break;
    end else
      Packet^.Wrote := Packet^.Wrote + escritos;
    inc(tentativas);
  end;

  Packet^.WriteRetries := tentativas;
  if Packet^.ToWrite>Packet^.Wrote then begin
    Packet^.WriteIOResult := iorTimeOut;
    if PClearBufOnErr then
      InternalClearALLBuffers;
  end else
    Packet^.WriteIOResult := iorOK;

  if Packet^.WriteIOResult<>iorOK then
    DoCommError(true, Packet^.WriteIOResult);
end;

procedure TTCP_UDPPort.NeedSleepBetweenRW;
begin
  //TCP not need sleep between Read and Write commands.
end;

procedure TTCP_UDPPort.PortStart(var Ok:Boolean);
var
{$IF defined(UNIX) and defined(FPC)}
  ServerAddr:THostEntry;
  tv:timeval;
{$ELSE}
{$IFNDEF WINCE}
  ServerAddr:PHostEnt;
{$ENDIF}
{$IFEND}
{$IFDEF WINCE}
  channel:sockets.sockaddr_in;
{$ELSE}
  channel:sockaddr_in;
{$ENDIF}
  flag, bufsize, sockType:Integer;
begin
  Ok:=false;
  try
    {$IF defined(UNIX) and defined(FPC)}
    if not GetHostByName(FHostName,ServerAddr) then begin
      ServerAddr.Addr:=StrToHostAddr(FHostName);
      if ServerAddr.Addr.s_addr=0 then begin
        PActive:=false;
        RefreshLastOSError;
        exit;
      end;
    end;
    {$ELSE}
      {$IFNDEF WINCE}
        //se esta usando FPC ou um Delphi abaixo da versao 2009
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
      {$ENDIF}
    {$IFEND}

    case FPortType of
      ptTCP:
        sockType := IPPROTO_TCP;
      ptUDP:
        sockType := IPPROTO_UDP;
      else begin
        PActive:=false;
        exit;
      end;
    end;

    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    FSocket := fpSocket(PF_INET, SOCK_STREAM, sockType);
    {$ELSE}
    FSocket :=   Socket(PF_INET, SOCK_STREAM, sockType);
    {$IFEND}

    if FSocket<0 then begin
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;

    flag:=1;
    bufsize := 1024*16;
    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    {$IFDEF UNIX}
    tv.tv_sec:=((FTimeout+1000) div 1000);
    tv.tv_usec:=((FTimeout+1000) mod 1000) * 1000;

    fpsetsockopt(FSocket, SOL_SOCKET,  SO_RCVTIMEO,  @tv,       sizeof(tv));
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_SNDTIMEO,  @tv,       sizeof(tv));
    {$ELSE}
    flag:=FTimeout+1000;
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_RCVTIMEO,  @flag, sizeof(flag));
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_SNDTIMEO,  @flag, sizeof(flag));
    flag:=1;
    {$ENDIF}
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_RCVBUF,    @bufsize,  sizeof(Integer));
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_SNDBUF,    @bufsize,  sizeof(Integer));
    fpsetsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY,  @flag,     sizeof(Integer));
    {$ELSE}
    flag:=FTimeout+1000;
    setsockopt(FSocket, SOL_SOCKET,  SO_RCVTIMEO,  PAnsiChar(@flag), sizeof(flag));
    setsockopt(FSocket, SOL_SOCKET,  SO_SNDTIMEO,  PAnsiChar(@flag), sizeof(flag));
    setsockopt(FSocket, SOL_SOCKET,  SO_RCVBUF,    PAnsiChar(@bufsize),  sizeof(Integer));
    setsockopt(FSocket, SOL_SOCKET,  SO_SNDBUF,    PAnsiChar(@bufsize),  sizeof(Integer));
    flag := 1;
    setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY,  PAnsiChar(@flag),     sizeof(Integer));
    {$IFEND}

    channel.sin_family      := AF_INET;
    channel.sin_port        := htons(FPortNumber);

    {$IF defined(FPC) AND defined(UNIX)}
    channel.sin_addr.S_addr := longword(htonl(LongInt(ServerAddr.Addr.s_addr)));
    {$ELSE}
    {$IFDEF WINCE}
    channel.sin_addr := StrToNetAddr(FHostName);
    {$ELSE}
    channel.sin_addr.S_addr := PInAddr(Serveraddr.h_addr^).S_addr;
    {$ENDIF}
    {$IFEND}

    if connect_with_timeout(FSocket,@channel,sizeof(channel),FTimeout)<>0 then begin
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;
    Ok:=true;
    PActive:=true;
  finally
    if not Ok then
      CloseSocket(FSocket);
  end;
end;

procedure TTCP_UDPPort.PortStop(var Ok:Boolean);
var
  buffer:BYTES;
  lidos:Integer;
begin
  if FSocket>0 then begin
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
  FSocket:=0;
  SetLength(Buffer,0);
end;

function  TTCP_UDPPort.ComSettingsOK:Boolean;
begin
  Result := (FHostName<>'') and ((FPortNumber>0) and (FPortNumber<65536));
end;

function TTCP_UDPPort.CheckConnection(var CommResult:TIOResult; var incRetries:Boolean):Boolean;
begin
  incRetries:=true;
  //CommResult informa o resultado da IO
  //Result informa se a acao deve ser retomada.
  {$IF defined(WIN32) or defined(WIN64) or defined(WINCE)}
  case WSAGetLastError of
    WSANOTINITIALISED,
    WSAENETDOWN,
    WSAEFAULT,
    WSAENETRESET,
    WSAENOTSOCK,
    WSAECONNABORTED,
    WSAENOTCONN,
    WSAESHUTDOWN: begin
      PActive:=false;
      CommResult := iorNotReady;
      Result := false;
    end;

    WSAEOPNOTSUPP: begin
      PActive:=false;
      CommResult:=iorPortError;
      Result:=false;
    end;

    WSAEINVAL,
    WSAEMSGSIZE: begin
      Result := false;
      CommResult := iorPortError;
    end;

    WSAEWOULDBLOCK:
      Result := true;

    WSAEINPROGRESS,    
    WSAEINTR: begin
      Result:=true;
      incRetries:=false;
    end;
    WSAETIMEDOUT: begin
      if PActive then DoCommPortDisconected;
      PActive:=false;
      {$IF defined(UNIX) and defined(FPC)}
      fpshutdown(FSocket,SHUT_RDWR);
      {$ELSE}
      Shutdown(FSocket,2);
      {$IFEND}
      CloseSocket(FSocket);
      CommResult:=iorTimeOut;
      Result:=false;
    end;
  end;
  {$ELSE}
  case socketerror of
    EsockEINVAL:
      Result:= true;

    EsockENOTCONN,
    EsockENOTSOCK,
    EsockEBADF,
    ESysECONNRESET,
    ESysECONNABORTED,
    ESysECONNREFUSED: begin
      if PActive then DoCommPortDisconected;
      PActive:=false;
      {$IF defined(UNIX) and defined(FPC)}
      fpshutdown(FSocket,SHUT_RDWR);
      {$ELSE}
      Shutdown(FSocket,2);
      {$IFEND}
      CloseSocket(FSocket);
      CommResult:=iorNotReady;
      Result:=false;
    end;

    EsockEFAULT,
    EsockEACCESS,
    EsockEMFILE,
    EsockEMSGSIZE,
    EsockENOBUFS,
    ESysEIO,
    EsockEPROTONOSUPPORT: begin
      CommResult:=iorPortError;
      Result:=false;
    end;

    EsockEINTR,
    ESysEAGAIN: begin
      Result:=true;
      incRetries:=false;
    end;
    ESysETIMEDOUT: begin
      if PActive then DoCommPortDisconected;
      PActive:=false;
      {$IF defined(UNIX) and defined(FPC)}
      fpshutdown(FSocket,SHUT_RDWR);
      {$ELSE}
      Shutdown(FSocket,2);
      {$IFEND}
      CloseSocket(FSocket);
      CommResult:=iorTimeOut;
      Result:=false;
    end;
  end;
  {$IFEND}
end;

procedure TTCP_UDPPort.ClearALLBuffers;
//var
  //lidos:Integer;
  //buffer:array[0..65535] of Byte;
begin
  //try
    //lidos:=-1;
    //while lidos>0 do begin
    //  {$IF defined(UNIX) and defined(FPC)}
    //  lidos := fprecv(FSocket, @buffer[0], 65535, MSG_PEEK);
    //  lidos := fprecv(FSocket, @buffer[0], lidos, 0);
    //  lidos := fprecv(FSocket, @buffer[0], 65535, MSG_PEEK);
    //  {$ELSE}
    //  lidos := Recv(FSocket, buffer[0], 65535, MSG_PEEK);
    //  lidos := Recv(FSocket, buffer[0], lidos, 0);
    //  lidos := Recv(FSocket, buffer[0], 65535, MSG_PEEK);
    //  {$IFEND}
    //end;
  //finally
  //end;
end;

{$IFDEF UNIX}
function  TTCP_UDPPort.set_nonblock(fd:TSocket; block:boolean):Integer;
var
  oldflags:Integer;
begin
    oldflags := FpFcntl(fd, F_GETFL, 0);
    if (oldflags < 0) then begin
      Result:= oldflags;
      exit;
    end;

    if (block) then
      oldflags := oldflags or O_NONBLOCK
    else
      oldflags := oldflags xor O_NONBLOCK;

    Result:= FpFcntl(fd, F_SETFL, oldflags);
end;
{$ENDIF}

function TTCP_UDPPort.connect_with_timeout(sock:Tsocket; address:psockaddr; address_len:t_socklen; timeout:Integer):Integer;
var
  sel:TFDSet;
  ret:Integer;
  {$IFDEF UNIX}
  mode:Integer;
  {$ELSE}
  mode:u_long;
  {$ENDIF}
  tv : TTimeVal;
  p:ptimeval;
label
  cleanup;
begin

  if timeout=-1 then
    p:=nil
  else begin
    tv.tv_Sec:=Timeout div 1000;
    tv.tv_Usec:=(Timeout mod 1000)*1000;
    p:=@tv;
  end;

  {$if defined(WIN32) or defined(WIN64) or defined(WINCE)}
  mode:=1;
  ioctlsocket(sock, FIONBIO, mode);
  {$ELSE}
  if set_nonblock(sock, true) < 0 then begin
    Result:=-1;
    exit;
  end;
  {$ifend}

  {$IF defined(UNIX) and defined(FPC)}
  if fpconnect(sock, address, address_len) <> 0 then
  {$ELSE}
  if connect(sock, address^, address_len) <> 0 then
  {$IFEND}
    {$if defined(WIN32) or defined(WIN64) or defined(WINCE)}
    if WSAGetLastError=WSAEWOULDBLOCK then begin
    {$else}
    if socketerror = ESysEINPROGRESS then begin
    {$ifend}
      {$IF defined(FPC) and defined(UNIX)}
      fpFD_ZERO(sel);
      fpFD_SET(sock, sel);
      mode := fpSelect(sock+1, nil, @sel, nil, p);
      {$else}
      FD_ZERO(sel);
      FD_SET(sock+1, sel);
      mode := select(sock+1, nil, @sel, nil, p);
      {$IFEND}

      if (mode < 0) then begin
        Result := -1;
      end else begin
        if (mode > 0) then begin
          Result := 0;
        end else begin
          if (mode = 0) then begin
            Result := -2;
          end;
        end;
      end;
    end else
      Result := -1;

cleanup:
  {$if defined(WIN32) or defined(WIN64) or defined(WINCE)}
  mode := 0;
  ioctlsocket(sock, FIONBIO, mode);
  {$else}
  if(set_nonblock(sock, false) < 0) then
    Result := -1;
  {$IFEND}
end;

{$IF defined(WIN32) or defined(WIN64)}
var
  wsaData:TWSAData;
  version:WORD;
initialization

  //inicialização Winsock
  version := MAKEWORD( 2, 0 );

  //check for error
  if WSAStartup( version, wsaData ) <> 0 then
    raise Exception.Create(SerrorInitializingWinsock);

  //check for correct version
  if (LOBYTE(wsaData.wVersion) <> 2) or (HIBYTE(wsaData.wVersion)<>0) then begin
    //incorrect WinSock version
    WSACleanup();
    raise Exception.Create(SinvalidWinSockVersion);
  end;
finalization
  WSACleanup;
{$IFEND}
end.