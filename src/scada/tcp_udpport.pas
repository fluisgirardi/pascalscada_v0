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

{$IFDEF FPC}
{$mode delphi}
{$IFDEF DEBUG}
  {$DEFINE FDEBUG}
{$ENDIF}
{$ENDIF}

{$I ../common/delphiver.inc}

interface

uses
  Classes, SysUtils, ExtCtrls, CommPort, commtypes, socket_types
  {$IF defined(WIN32) or defined(WIN64)} //delphi or lazarus over windows
  , Windows, WinSock, sockets_w32_w64
  {$ELSE}
  {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
  , Sockets {$IFDEF UNIX}  , sockets_unix, netdb, Unix{$ENDIF}
            {$IFDEF WINCE} , sockets_wince {$ENDIF}
            {$IFDEF FDEBUG}, LCLProc{$ENDIF}
  {$IFEND}
  {$IFEND};

type

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
    FHostName:String;
    FPortNumber:Integer;
    FTimeout:Integer;
    FSocket:Tsocket;
    FPortType:TPortType;
    FExclusiveReaded:Boolean;
    freconnectTimer:TTimer;
    FEnableAutoReconnect:Boolean;
    FReconnectRetriesLimit:Cardinal;
    procedure TryReconnectTimer(Sender: TObject);
    procedure SetHostname(target:string);
    procedure SetPortNumber(pn:Integer);
    procedure SetTimeout(t:Integer);
    procedure SetPortType(pt:TPortType);
    procedure SetExclusive(b:Boolean);

    procedure setEnableAutoReconnect(v:Boolean);
    function GetReconnectInterval:Cardinal;
    procedure SetReconnectInterval(v:Cardinal);
  protected
    //: @exclude
    FReconnectRetries:Cardinal;
    //: @exclude
    procedure SetActive(v: Boolean); override;
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
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    {$IFDEF PORTUGUES}
    //: Nome ou endereço do servidor onde se deseja conectar.
    {$ELSE}
    //: Hostname or address of the server to connect.
    {$ENDIF}
    property Host:String read FHostName write SetHostname nodefault;

    {$IFDEF PORTUGUES}
    //: Porta do servidor que se deseja conectar. Para Modbus TCP use 502 e para Siemens ISOTCP use 102.
    {$ELSE}
    //: Server port to connect. To use Modbus, set this to 502 and to use Siemens ISOTCP set it to 102.
    {$ENDIF}
    property Port:Integer read FPortNumber write SetPortNumber default 102;

    {$IFDEF PORTUGUES}
    //: Timeout em milisegundos para operações de leitura/escrita.
    {$ELSE}
    //: Timeout in milliseconds to I/O operations.
    {$ENDIF}
    property Timeout:Integer read FTimeout write SetTimeout default 1000;

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
    property EnableAutoReconnect:Boolean read FEnableAutoReconnect write setEnableAutoReconnect  stored true default true;

    {$IFDEF PORTUGUES}
    //: Define o tempo após a perda de conexão a porta deve tentar reconectar. Tempo em milisegundos.
    {$ELSE}
    //: Time to retry a lost connection in milliseconds.
    {$ENDIF}
    property ReconnectRetryInterval:Cardinal read GetReconnectInterval write SetReconnectInterval stored true default 5000;

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

uses hsstrings;

constructor TTCP_UDPPort.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FPortNumber:=102;
  FTimeout:=1000;
  FSocket:=0;
  FEnableAutoReconnect:=true;
  FReconnectRetriesLimit:=0;
  freconnectTimer:=TTimer.Create(nil);
  freconnectTimer.Enabled:=false;
  freconnectTimer.Interval:=5000;
  freconnectTimer.OnTimer:=TryReconnectTimer;
end;

destructor  TTCP_UDPPort.Destroy;
begin
  inherited Destroy;
  freconnectTimer.Destroy;
end;

procedure TTCP_UDPPort.TryReconnectTimer(Sender: TObject);
begin
  freconnectTimer.Enabled:=false;
  inc(FReconnectRetries);
  Active:=true;
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
  FEnableAutoReconnect:=v;
  if (v=false) and freconnectTimer.Enabled then
    freconnectTimer.Enabled:=false;
end;

function  TTCP_UDPPort.GetReconnectInterval:Cardinal;
begin
  Result:=freconnectTimer.Interval;
end;

procedure TTCP_UDPPort.SetReconnectInterval(v:Cardinal);
begin
  freconnectTimer.Interval:=v;
end;

procedure TTCP_UDPPort.SetActive(v: Boolean);
begin
  inherited SetActive(v);
  if not v then
    freconnectTimer.Enabled:=false;
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
      lidos := socket_recv(FSocket, @Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received, 0, FTimeout);
    finally
    end;

    if lidos<=0 then begin
      if not CheckConnection(Packet^.ReadIOResult, incretries, PActive, FSocket, CommPortDisconected) then
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
  escritos:Integer;
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
      if not CheckConnection(Packet^.ReadIOResult, incretries, PActive, FSocket, CommPortDisconected) then
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

  flag, bufsize, sockType, sockProto:Integer;
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
      if not GetHostByName(FHostName,ServerAddr) then begin
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
    FSocket := fpSocket(PF_INET, sockType, sockProto);

    if FSocket<0 then begin
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;
    {$ELSE}
    //WINDOWS
    FSocket :=   Socket(PF_INET, sockType, sockProto);

    if FSocket=INVALID_SOCKET then begin
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;
    {$IFEND}

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
    bufsize := 1024*16;
    //UNIX AND WINDOWS CE
    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_RCVBUF,    @bufsize,  sizeof(Integer));
    fpsetsockopt(FSocket, SOL_SOCKET,  SO_SNDBUF,    @bufsize,  sizeof(Integer));
    fpsetsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY,  @flag,     sizeof(Integer));
    {$IFEND}
    //WINDOWS
    {$IF defined(WIN32) or defined(WIN64)}
    setsockopt(FSocket, SOL_SOCKET,  SO_RCVBUF,    PAnsiChar(@bufsize), sizeof(Integer));
    setsockopt(FSocket, SOL_SOCKET,  SO_SNDBUF,    PAnsiChar(@bufsize), sizeof(Integer));
    setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY,  PAnsiChar(@flag),    sizeof(Integer));
    {$IFEND}

    //##########################################################################
    //CONFIGURA E ENDERECO QUE O SOCKET VAI CONECTAR
    //SETS THE TARGET ADDRESS TO SOCKET CONNECT
    //##########################################################################
    channel.sin_family      := AF_INET;            //FAMILY
    channel.sin_port        := htons(FPortNumber); //PORT NUMBER

    {$IF defined(FPC) AND defined(UNIX)}
    channel.sin_addr.S_addr := longword(htonl(LongInt(ServerAddr.Addr.s_addr)));
    {$IFEND}

    {$IF defined(FPC) AND defined(WINCE)}
    channel.sin_addr := StrToNetAddr(FHostName);
    {$IFEND}

    {$IF defined(WIN32) OR defined(WIN64)}
    channel.sin_addr.S_addr := PInAddr(Serveraddr.h_addr^).S_addr;
    {$IFEND}

    if connect_with_timeout(FSocket,@channel,sizeof(channel),FTimeout)<>0 then begin
      PActive:=false;
      RefreshLastOSError;
      exit;
    end;
    Ok:=true;
    PActive:=true;
    FReconnectRetries:=0;
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

procedure TTCP_UDPPort.DoPortDisconnected(sender: TObject);
begin
  if FEnableAutoReconnect and ((FReconnectRetriesLimit=0) or (FReconnectRetries<=FReconnectRetriesLimit)) then
    freconnectTimer.Enabled:=True;

  inherited DoPortDisconnected(sender);
end;

procedure TTCP_UDPPort.DoPortOpenError(sender: TObject);
begin
  if FEnableAutoReconnect and ((FReconnectRetriesLimit=0) or (FReconnectRetries<=FReconnectRetriesLimit)) then
    freconnectTimer.Enabled:=True;

  inherited DoPortOpenError(sender);
end;

procedure TTCP_UDPPort.ClearALLBuffers;
begin

end;

end.
