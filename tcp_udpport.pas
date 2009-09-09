unit tcp_udpport;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CommPort,
  commtypes{$IFDEF FPC}, LResources, Sockets {$ELSE} , Windows, WinSock {$ENDIF} ;

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
    function  CheckConnection:TIOResult; virtual;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy;
  published
    property Host:String read FHostName write SetHostname nodefault;
    property Port:Integer read FPortNumber write SetPortNumber default 102;
    property Timeout:Integer read FTimeout write SetTimeout default 1000;
    property PortType:TPortType read FPortType write SetPortType default ptTCP;
  end;

implementation

{$ifdef fpc}
uses netdb {$IFDEF UNIX}, Unix {$endif};
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

  while (Packet^.Received<Packet^.ToRead) and (tentativas<Packet^.ReadRetries) do begin
    lidos := Recv(FSocket, Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received, 0);
    if lidos<0 then begin
      Packet^.ReadIOResult := CheckConnection;
      exit;
    end;

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

  while (Packet^.Wrote<Packet^.ToWrite) and (tentativas<Packet^.WriteRetries) do begin
    escritos := Send(FSocket, Packet^.BufferToWrite[Packet^.Wrote], Packet^.ToWrite-Packet^.Wrote, 0);
    if escritos<0 then begin
      Packet^.ReadIOResult := CheckConnection;
      exit;
    end;
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
{$IFDEF FPC}
  ServerAddr:THostEntry;
  {$ifdef UNIX}
  tv:timeval;
  {$ENDIF}
{$ELSE}
  ServerAddr:PHostEnt;
{$ENDIF}
  channel:sockaddr_in;
begin
  Ok:=false;
  {$IFDEF FPC}
  if not GetHostByName(FHostName,ServerAddr) then begin
  {$ELSE}
  ServerAddr := GetHostByName(PChar(FHostName));
  if ServerAddr=nil then begin
  {$ENDIF}
    PActive:=false;
    showmessage('Gethostbyname falhou');
    exit;
  end;

  case FPortType of
    ptTCP:
      FSocket := Socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    ptUDP:
      FSocket := Socket(PF_INET, SOCK_STREAM, IPPROTO_UDP);
    else begin
      PActive:=false;
      exit;
    end;
  end;

  if FSocket<0 then begin
    PActive:=false;
    ShowMessage('Socket falhou');
    exit;
  end;

  {$IFDEF UNIX}
  tv.tv_sec:=(FTimeout div 1000);
  tv.tv_usec:=(FTimeout mod 1000) * 1000;  

  SetSocketOptions(FSocket, SOL_SOCKET, SO_RCVTIMEO, tv, sizeof(tv));
  SetSocketOptions(FSocket, SOL_SOCKET, SO_SNDTIMEO, tv, sizeof(tv));
  {$ELSE}
  setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@FTimeout), sizeof(FTimeout));
  setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, PAnsiChar(@FTimeout), sizeof(FTimeout));
  {$ENDIF}

  try
    channel.sin_family      := AF_INET;
    {$IFDEF FPC}
    channel.sin_addr.S_addr := htonl(ServerAddr.Addr.s_addr);
    {$ELSE}
    channel.sin_addr.S_addr := TInAddr(Serveraddr.h_addr).S_addr;
    {$ENDIF}
    channel.sin_port        := htons(FPortNumber);

    {$IFDEF FPC}
    if not Connect(FSocket,channel,sizeof(sockaddr_in)) then begin
    {$ELSE}
    if Connect(FSocket,channel,sizeof(sockaddr_in))<>0 then begin
    {$ENDIF}
      PActive:=false;
      showmessage('Connect falhou');
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
begin
  if FSocket<0 then begin
    PActive:=false;
    exit;
  end;
  CloseSocket(FSocket);
  FSocket:=0
end;

function  TTCP_UDPPort.ComSettingsOK:Boolean;
begin
  Result := (FHostName<>'') and ((FPortNumber>0) and (FPortNumber<65536));
  if not Result then
    ShowMessage('config invalida');
end;

function TTCP_UDPPort.CheckConnection:TIOResult;
begin
  //verificar erros de porta
  Result := iorPortError;
end;

procedure TTCP_UDPPort.ClearALLBuffers;
begin
  //verificar se ha como limpar os buffers de uma porta TCP...
end;

{$IFNDEF FPC}
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
{$ENDIF}
end.
