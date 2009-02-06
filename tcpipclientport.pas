unit TCPIPClientPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CommPort,
  lNet, commtypes;

type
  TTCPIPClientPort = class(TCommPortDriver)
  private
    FTcp:TLTcp;
    function  GetHostname:String;
    procedure SetHostname(target:string);
    function  GetPortNumber:integer;
    procedure SetPortNumber(pn:Integer);
    function  GetTimeout:Integer;
    procedure SetTimeout(t:Integer);

    procedure ConnectionOK;
  protected
    procedure Read(Packet:PIOPacket); override;
    procedure Write(Packet:PIOPacket); override;
    procedure NeedSleepBetweenRW; override;
    procedure PortStart(var Ok:Boolean); override;
    procedure PortStop(var Ok:Boolean); override;
    function  ComSettingsOK:Boolean; override;
    procedure ClearALLBuffers; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy;
  published
    property Host:String read GetHostname write SetHostname nodefault;
    property Port:Integer read GetPortNumber write SetPortNumber default 102;
    property Timeout:Integer read GetTimeout write SetTimeout default 1000;
  end;

implementation

constructor TTCPIPClientPort.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FTcp := TLTcp.Create(self);
  FTcp.Port:=102;
end;

destructor  TTCPIPClientPort.Destroy;
begin
  inherited Destroy;
  FTcp.Destroy;
end;

function  TTCPIPClientPort.GetHostname:String;
begin
  ConnectionOK;

  Result := FTcp.Host;
end;

procedure TTCPIPClientPort.SetHostname(target:string);
begin
  DoExceptionInActive;
  ConnectionOK;
  FTcp.Host:=target;
end;

function  TTCPIPClientPort.GetPortNumber:integer;
begin
  ConnectionOK;

  Result := FTcp.Port;
end;

procedure TTCPIPClientPort.SetPortNumber(pn:Integer);
begin
  DoExceptionInActive;
  ConnectionOK;
  if (pn>=1) or (pn<=65535) then
    FTcp.Port:=pn
  else
    raise Exception.Create('Deve estar entre 1 e 65535!');
end;

function  TTCPIPClientPort.GetTimeout:Integer;
begin
  ConnectionOK;

  Result := FTcp.Timeout;
end;

procedure TTCPIPClientPort.SetTimeout(t:Integer);
begin
  DoExceptionInActive;
  ConnectionOK;
  FTcp.Timeout:=t;
end;

procedure TTCPIPClientPort.ConnectionOK;
begin
  if FTcp=nil then
    raise Exception.Create('Conexão inválida!');
end;

procedure TTCPIPClientPort.Read(Packet:PIOPacket);
var
  lidos:DWORD;
  tentativas:DWORD;
begin
  ConnectionOK;

  tentativas := 0;
  lidos := 0;

  while (Packet^.Received<Packet^.ToRead) and (tentativas<Packet^.ReadRetries) do begin
    lidos := FTcp.Get(Packet^.BufferToRead[Packet^.Received], Packet^.ToRead-Packet^.Received);
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

procedure TTCPIPClientPort.Write(Packet:PIOPacket);
var
  escritos:DWORD;
  tentativas:DWORD;
begin
  ConnectionOK;

  tentativas := 0;
  escritos := 0;

  while (Packet^.Wrote<Packet^.ToWrite) and (tentativas<Packet^.WriteRetries) do begin
    escritos := FTcp.Send(Packet^.BufferToWrite[Packet^.Wrote], Packet^.ToWrite-Packet^.Wrote);
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

procedure TTCPIPClientPort.NeedSleepBetweenRW;
begin
  //TCP not need sleep between Read and Write commands.
end;

procedure TTCPIPClientPort.PortStart(var Ok:Boolean);
begin
  ConnectionOK;
  FTcp.Connect;
  PActive:=FTcp.Connected;
  ok := PActive;
end;

procedure TTCPIPClientPort.PortStop(var Ok:Boolean);
begin
  ConnectionOK;
  FTcp.Disconnect;
  PActive:=not FTcp.Connected;
  ok := PActive;
end;

function  TTCPIPClientPort.ComSettingsOK:Boolean;
begin
  Result := (FTcp.Host<>'') and (FTcp.Port in [1..65535]);
end;

procedure TTCPIPClientPort.ClearALLBuffers;
begin

end;

end.
