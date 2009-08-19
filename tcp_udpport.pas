unit tcp_udpport;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CommPort,
  commtypes{$IFDEF FPC}, Sockets {$ELSE} , Windows, WinSock {$ENDIF} ;

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

uses netdb;

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
  lidos:Cardinal;
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
  escritos:Cardinal;
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
  ServerAddr:THostEntry;
  channel:sockaddr_in;
begin
  Ok:=false;

  if not GetHostByName(FHostName,ServerAddr) then begin
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
    showmessage('Socket falhou');
    exit;
  end;

  try
    channel.family          := AF_INET;
    channel.sin_addr.s_addr := ServerAddr.Addr.s_addr;
    channel.sin_port        := htons(FPortNumber);

    if not Connect(FSocket,channel,sizeof(sockaddr_in)) then begin
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
  if not REsult then
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

end.
