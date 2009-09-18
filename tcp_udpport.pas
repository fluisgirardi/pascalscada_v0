unit tcp_udpport;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, CommPort, commtypes
  {$IF defined(WIN32) or defined(WIN64)}
  , Windows, WinSock
  {$ELSE}
  {$IFDEF FPC}
  , Sockets, BaseUnix
  {$ENDIF}
  {$IFEND};

type

  TPortType = (ptTCP, ptUDP);

  TTCP_UDPPort = class(TCommPortDriver)
  private
    FHostName:String;
    FPortNumber:Integer;
    FTimeout:Integer;
    FSocket:Tsocket;
    FPortType:TPortType;
    procedure SetHostname(target:string);
    procedure SetPortNumber(pn:Integer);
    procedure SetTimeout(t:Integer);
    procedure SetPortType(pt:TPortType);
  protected
    procedure Read(Packet:PIOPacket); override;
    procedure Write(Packet:PIOPacket); override;
    procedure NeedSleepBetweenRW; override;
    procedure PortStart(var Ok:Boolean); override;
    procedure PortStop(var Ok:Boolean); override;
    function  ComSettingsOK:Boolean; override;
    procedure ClearALLBuffers; override;
    function  CheckConnection(var CommResult:TIOResult):Boolean; virtual;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
  published
    property Host:String read FHostName write SetHostname nodefault;
    property Port:Integer read FPortNumber write SetPortNumber default 102;
    property Timeout:Integer read FTimeout write SetTimeout default 1000;
    property PortType:TPortType read FPortType write SetPortType default ptTCP;
  end;

implementation

{$ifdef fpc}
{$IFDEF UNIX}
uses  netdb, Unix;
{$endif}
{$ENDIF}

constructor TTCP_UDPPort.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FPortNumber:=102;
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
    raise Exception.Create('Deve estar entre 1 e 65535!');
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

procedure TTCP_UDPPort.Read(Packet:PIOPacket);
var
  lidos:Integer;
  tentativas:Cardinal;
begin

  tentativas := 0;
  lidos := 0;

  Packet^.Received := 0;
  while (Packet^.Received<Packet^.ToRead) and (tentativas<Packet^.ReadRetries) do begin
    {$IF defined(UNIX) and defined(FPC)}
    lidos := fprecv(FSocket, @Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received, 0);
    {$ELSE}
    lidos := Recv(FSocket, Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received, 0);
    {$IFEND}

    if lidos<0 then begin
      if CheckConnection(Packet^.ReadIOResult) then begin
        inc(tentativas);
        continue;
      end;
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
end;

procedure TTCP_UDPPort.Write(Packet:PIOPacket);
var
  escritos:Integer;
  tentativas:Cardinal;
begin

  tentativas := 0;
  escritos := 0;

  Packet^.Wrote := 0;
  while (Packet^.Wrote<Packet^.ToWrite) and (tentativas<Packet^.WriteRetries) do begin
    {$IF defined(UNIX) and defined(FPC)}
    escritos := fpsend(FSocket, @Packet^.BufferToWrite[Packet^.Wrote], Packet^.ToWrite-Packet^.Wrote, 0);
    {$ELSE}
    escritos := Send(FSocket, Packet^.BufferToWrite[Packet^.Wrote], Packet^.ToWrite-Packet^.Wrote, 0);
    {$IFEND}

    if escritos<0 then begin
      if CheckConnection(Packet^.WriteIOResult) then begin
        inc(tentativas);
        continue;
      end;
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
  ServerAddr:PHostEnt;
{$IFEND}
  channel:sockaddr_in;
  flag, bufsize, sockType:Integer;
begin
  Ok:=false;
  {$IF defined(WIN32) or defined(WIN64)}
  ServerAddr:=nil;
  {$IFEND}
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
    ServerAddr := GetHostByName(PChar(FHostName));
    if ServerAddr=nil then begin
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;
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
    {$IF defined(UNIX) and defined(FPC)}
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
    {$IF defined(UNIX) and defined(FPC)}
    tv.tv_sec:=(FTimeout div 1000);
    tv.tv_usec:=(FTimeout mod 1000) * 1000;

    fpsetsockopt(FSocket, SOL_SOCKET,  SO_RCVTIMEO, @tv,      sizeof(tv));
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_SNDTIMEO, @tv,      sizeof(tv));
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_RCVBUF,   @bufsize,  sizeof(Integer));
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_SNDBUF,   @bufsize,  sizeof(Integer));
    fpsetsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, @flag,     sizeof(Integer));
    {$ELSE}
    setsockopt(FSocket, SOL_SOCKET,  SO_RCVTIMEO, PAnsiChar(@FTimeout), sizeof(FTimeout));
    setsockopt(FSocket, SOL_SOCKET,  SO_SNDTIMEO, PAnsiChar(@FTimeout), sizeof(FTimeout));
    setsockopt(FSocket, SOL_SOCKET,  SO_RCVBUF,   PAnsiChar(@bufsize),  sizeof(Integer));
    setsockopt(FSocket, SOL_SOCKET,  SO_SNDBUF,   PAnsiChar(@bufsize),  sizeof(Integer));
    setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@flag),     sizeof(Integer));
    {$IFEND}

    channel.sin_family      := AF_INET;
    {$IF defined(UNIX) and defined(FPC)}
    channel.sin_addr.S_addr := longword(htonl(LongInt(ServerAddr.Addr.s_addr)));
    {$ELSE}
    channel.sin_addr.S_addr := PInAddr(Serveraddr.h_addr^).S_addr;
    {$IFEND}
    channel.sin_port        := htons(FPortNumber);

    {$IF defined(UNIX) and defined(FPC)}
    if fpconnect(FSocket,@channel,sizeof(sockaddr_in))<>0 then begin
    {$ELSE}
    if Connect(FSocket,channel,sizeof(sockaddr_in))<>0 then begin
    {$IFEND}
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;
    Ok:=true;
    PActive:=true;
  finally
    {$IFDEF WINDOWS}
    //if ServerAddr<>nil then
    //  Freemem(ServerAddr);
    {$ENDIF}
    if not Ok then
      CloseSocket(FSocket);
  end;
end;

procedure TTCP_UDPPort.PortStop(var Ok:Boolean);
begin
  if FSocket>0 then begin
    {$IF defined(UNIX) and defined(FPC)}
    fpshutdown(FSocket,SHUT_RDWR);
    {$ELSE}
    Shutdown(FSocket,2);
    {$IFEND}
    CloseSocket(FSocket);
  end;
  PActive:=false;
  Ok:=true;
  FSocket:=0
end;

function  TTCP_UDPPort.ComSettingsOK:Boolean;
begin
  Result := (FHostName<>'') and ((FPortNumber>0) and (FPortNumber<65536));
end;

function TTCP_UDPPort.CheckConnection(var CommResult:TIOResult):Boolean;
begin
  //CommResult informa o resultado da IO
  //Result informa se a acao deve ser retomada.
  {$IF defined(WIN32) or defined(WIN64)}

  {$ELSE}
  case socketerror of
    EsockEINTR, EsockEINVAL:
      Result:= true;

    EsockENOTCONN,
    EsockENOTSOCK,
    EsockEBADF,
    ESysECONNRESET,
    ESysECONNABORTED,
    ESysECONNREFUSED: begin
      PActive:=false;
      CommResult:=iorNotReady;
      Result:=false;
    end;

    EsockEFAULT,
    EsockEACCESS,
    EsockEMFILE,
    EsockEMSGSIZE,
    EsockENOBUFS,
    ESysEIO: begin
      CommResult:=iorPortError;
      Result:=false;
    end;

    EsockEPROTONOSUPPORT: begin
      PActive:=false;
      CommResult:=iorPortError;
      Result:=false;
    end;

    ESysEAGAIN,
    ESysETIMEDOUT: begin
      CommResult:=iorTimeOut;
      Result:=true;
    end;
  end;
  {$IFEND}
end;

procedure TTCP_UDPPort.ClearALLBuffers;
begin
  //verificar se ha como limpar os buffers de uma porta TCP...
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
    raise Exception.Create('Falha inicializando WinSock!');

  //check for correct version
  if (LOBYTE(wsaData.wVersion) <> 2) or (HIBYTE(wsaData.wVersion)<>0) then begin
    //incorrect WinSock version
    WSACleanup();
    raise Exception.Create('Versao incorreta da WinSock. Requerida versao 2.0');
  end;
finalization
  WSACleanup;
{$IFEND}
end.
