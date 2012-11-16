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
unit MutexClient;

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
  , Sockets {$IFDEF UNIX}  , sockets_unix, netdb, Unix{$ENDIF}
            {$IFDEF WINCE} , sockets_wince {$ENDIF}
            {$IFDEF FDEBUG}, LCLProc{$ENDIF}
  {$IFEND}
  {$IFEND};

type

  { TMutexClient }

  TMutexClient = class(TComponent)
  private
    FActive,
    FActiveLoaded,
    FConnected: Boolean;
    FPort: Word;
    FServerHost:String;
    FSocket: TSocket;
    FPingTimer:TTimer;
    procedure Connect;
    procedure Disconnect;
    procedure SayGoodBye;
    procedure setActive(AValue: Boolean);
    procedure SetPort(AValue: Word);
    procedure SetServerHost(AValue: String);
    procedure CheckPingCmd(Sender:TObject);
    procedure PingResponse;
    procedure InternalCheckConnection;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    TryEnter:Boolean;
    procedure   Leave;
  published
    property Active:Boolean read FActive write setActive stored true default false;
    property Host:String read FServerHost write SetServerHost stored true nodefault;
    property Port:Word read FPort write SetPort stored true default 52321;
  end;

implementation

uses hsstrings;

{ TMutexClient }

procedure TMutexClient.Connect;
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

  flag:Integer;
  socketOpen:boolean;
begin

  if FConnected then exit;

  socketOpen:=false;

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
      //if the name resolution is being done using FPC or Delphi 2009 or older
      //uses the ansistring version, otherwise uses the unicode version.
      {$IF defined(FPC) OR (not defined(DELPHI2009_UP))}
      ServerAddr := GetHostByName(PAnsiChar(FServerHost));
      {$ELSE}
      ServerAddr := GetHostByName(PAnsiChar(AnsiString(FServerHost)));
      {$IFEND}
      if ServerAddr=nil then begin
        //PActive:=false;
        //RefreshLastOSError;
        exit;
      end;
    {$IFEND}

    //##########################################################################
    // RESOLUCAO DE NOMES SOBRE LINUX/FREEBSD e outros.
    // NAME RESOLUTION OVER LINUX/FREEBSD and others.
    //##########################################################################
    {$IF defined(FPC) and defined(UNIX)}
      if not GetHostByName(FServerHost,ServerAddr) then begin
        ServerAddr.Addr:=StrToHostAddr(FServerHost);
        if ServerAddr.Addr.s_addr=0 then begin
          //PActive:=false;
          //RefreshLastOSError;
          exit;
        end;
      end;
    {$IFEND}

    //##########################################################################
    // CRIA O SOCKET
    // CREATE THE SOCKET.
    //##########################################################################

    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    //UNIX and WINDOWS CE
    FSocket := fpSocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

    if FSocket<0 then begin
      //PActive:=false;
      //RefreshLastOSError;
      exit;
    end;
    {$ELSE}
    //WINDOWS
    FSocket :=   Socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

    if FSocket=INVALID_SOCKET then begin
      //PActive:=false;
      //RefreshLastOSError;
      exit;
    end;
    {$IFEND}

    socketOpen:=true;

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
    //UNIX AND WINDOWS CE
    {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
    fpsetsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY,  @flag,           sizeof(Integer));
    {$IFEND}
    //WINDOWS
    {$IF defined(WIN32) or defined(WIN64)}
    setsockopt  (FSocket, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@flag), sizeof(Integer));
    {$IFEND}

    //##########################################################################
    //CONFIGURA E ENDERECO QUE O SOCKET VAI CONECTAR
    //SETS THE TARGET ADDRESS TO SOCKET CONNECT
    //##########################################################################
    channel.sin_family      := AF_INET;      //FAMILY
    channel.sin_port        := htons(FPort); //PORT NUMBER

    {$IF defined(FPC) AND defined(UNIX)}
    channel.sin_addr.S_addr := longword(htonl(LongInt(ServerAddr.Addr.s_addr)));
    {$IFEND}

    {$IF defined(FPC) AND defined(WINCE)}
    channel.sin_addr := StrToNetAddr(FHostName);
    {$IFEND}

    {$IF defined(WIN32) OR defined(WIN64)}
    channel.sin_addr.S_addr := PInAddr(Serveraddr.h_addr^).S_addr;
    {$IFEND}

    if connect_with_timeout(FSocket,@channel,sizeof(channel),1000)<>0 then begin
      //PActive:=false;
      //RefreshLastOSError;
      exit;
    end;
    FConnected:=true;
  finally
    if socketOpen and (not FConnected) then
      CloseSocket(FSocket);
  end;
end;

procedure TMutexClient.Disconnect;
begin
  if FConnected then begin
    SayGoodBye;
    CloseSocket(FSocket);
    FConnected:=false;
  end;
end;

procedure TMutexClient.SayGoodBye;
var
  cmd:byte;
begin
  cmd:=253;
  socket_send(FSocket,@cmd,1,0,1000); //quit command.
end;

procedure TMutexClient.setActive(AValue: Boolean);
begin
  if [csLoading,csReading]*ComponentState<>[] then begin
    FActiveLoaded:=AValue;
    exit;
  end;

  if [csDesigning]*ComponentState<>[] then begin
    FActive:=AValue;
    exit;
  end;

  if AValue then
    Connect
  else
    Disconnect;

  FActive:=AValue;
end;

procedure TMutexClient.SetPort(AValue: Word);
begin
  if FActive then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if (AValue<1) or (AValue>65535) then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if FPort=AValue then Exit;

  FPort:=AValue;
end;

procedure TMutexClient.SetServerHost(AValue: String);
begin
  if FActive then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if FServerHost=AValue then Exit;
  FServerHost:=AValue;
end;

procedure TMutexClient.CheckPingCmd(Sender: TObject);
var
  servercmd:Byte;
  numofbytes, bytesread: Integer;
begin
  if FActive and FConnected then begin
    numofbytes:=GetNumberOfBytesInReceiveBuffer(FSocket);
    case numofbytes of
      -1:
        InternalCheckConnection;
      0:
        exit;
      else begin
        bytesread:=0;
        while bytesread<numofbytes do begin

          if socket_recv(FSocket,@servercmd,1,0,1000)>=1 then  begin
            case servercmd of
              255:
                PingResponse;

              253:
                Disconnect;
            end;
          end else begin
            InternalCheckConnection;
          end;

          if not FConnected then break;

          inc(bytesread);
        end;
      end;
    end;
  end;
end;

procedure TMutexClient.PingResponse;
var
  servercmd:Byte;
begin
  servercmd:=254;
  if socket_send(FSocket,@servercmd,1,0,1000)<1 then
    InternalCheckConnection;
end;

procedure TMutexClient.InternalCheckConnection;
var
  X:TIOResult;
  incRetries:Boolean;
begin
  CheckConnection(x,incRetries,FConnected,FSocket,nil);
end;

procedure TMutexClient.Loaded;
begin
  inherited Loaded;
  setActive(FActiveLoaded);
end;

constructor TMutexClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort:=51342;
  FActive:=false;
  FConnected:=false;
  FActiveLoaded:=false;
  FPingTimer:=TTimer.Create(Self);
  FPingTimer.Interval:=1000;
  FPingTimer.OnTimer:=CheckPingCmd;
end;

destructor TMutexClient.Destroy;
begin

  FPingTimer.Destroy;
  setActive(false);
  inherited Destroy;
end;

function TMutexClient.TryEnter: Boolean;
var
  request, response:Byte;
begin
  Result:=False;
  if FActive then begin
    //if not connected, connect
    if not FConnected then
      Connect;

    //if still disconnected, exit.
    if not FConnected then exit;

    //check if has some command remains on receive buffer.
    CheckPingCmd(Self);

    //if the connection has been closed, exit.
    if not FConnected then exit;

    request:=2;//try enter on mutex
    if socket_send(FSocket,@request,1,0,1000)>=1 then begin
      repeat
        if socket_recv(FSocket,@response,1,0,1000)>=1 then begin
          case response of
            0:
              Result:=false;
            1:
              Result:=true;
            253: begin
              Result:=false;
              Disconnect;
              break;
            end;
            254:
              PingResponse;
          end;
        end else
          InternalCheckConnection;

        if not FConnected then exit;
      until GetNumberOfBytesInReceiveBuffer(FSocket)<=0;
    end else
      InternalCheckConnection;
  end;

end;

procedure TMutexClient.Leave;
var
  request, response:Byte;
begin
  if FActive then begin
    //if not connected, connect
    if not FConnected then
      Connect;

    //if still disconnected, exit.
    if not FConnected then exit;

    //check if has some command remains on receive buffer.
    CheckPingCmd(Self);

    //if the connection has been closed, exit.
    if not FConnected then exit;

    request:=3; //leave mutex command.
    if socket_send(FSocket,@request,1,0,1000)>=1 then begin
      repeat
        if socket_recv(FSocket,@response,1,0,1000)>=1 then begin
          case response of
            253: begin
              Disconnect;
              break;
            end;
            254:
              PingResponse;
          end;
        end else
          InternalCheckConnection;

        if not FConnected then exit;
      until GetNumberOfBytesInReceiveBuffer(FSocket)<=0;
    end else
      InternalCheckConnection;
  end;
end;

end.
