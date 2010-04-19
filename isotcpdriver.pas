{:
  @abstract(Implmentação do protocolo ISOTCP.)
  Este driver é baseado no driver ISOTCP da biblioteca
  LibNODAVE de ...
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit ISOTCPDriver;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  classes, sysutils, ProtocolDriver, S7Types, Tag, ProtocolTypes, CrossEvent,
  commtypes;

type
  TISOTCPDriver = class(TProtocolDriver)
  private
    FCPUs:TS7CPUs;
    FReadEvent,
    FConnectEvent:TCrossEvent;
    FConnectionWay:TISOTCPConnectionWay;
    procedure SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
  private
    procedure PrepareMsgToSend(var msg:BYTES);
    procedure PrepareMsgToExchange(var msg:BYTES);
    procedure PrepareMsgPDUHeader(var msg:BYTES; pdutype:Integer);
    procedure PrepareMsgSetParam(var mgs:BYTES; param:BYTES);
    procedure IdentifyMsg(var msg:BYTES; var MsgId:Integer);
  protected
    FConnected:Boolean;
    procedure Connect(var CPU:TS7CPU); virtual;
    procedure DoAddTag(TagObj:TTag); override;
    procedure DoDelTag(TagObj:TTag); override;
    procedure DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer); override;
    procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function  DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    property ReadSomethingAlways;
    property ConnectionWay:TISOTCPConnectionWay read FConnectionWay write SetISOConnectionWay;
  end;

implementation

uses math, syncobjs;

constructor TISOTCPDriver.Create(AOwner:TComponent);
begin

end;

procedure TISOTCPDriver.Connect(var CPU:TS7CPU);
var
  IOResult:TIOPacket;
  msg, param:BYTES;
  res:Integer;
  len:Cardinal;
  datastart:Integer;
begin
  CPU.Connected:=false;
  if PCommPort=nil then exit;

  //incializa conexao
  SetLength(msg,22);
  PrepareMsgToSend(msg);
  msg[04] := $11;  //$11,
  msg[05] := $E0;  //$E0,
  msg[06] := 0;    //0,
  msg[07] := 0;    //0,
  msg[08] := 0;    //0,
  msg[09] := 1;    //1,
  msg[10] := 0;    //0,
  msg[11] := $C1;  //$C1,
  msg[12] := 2;    //2,
  msg[13] := ifthen(FConnectionWay=ISOTCP, 1, $4D);    //'M',
  msg[14] := ifthen(FConnectionWay=ISOTCP, 0, $57);    //'W',
  msg[15] := $C2;  //$C2,
  msg[16] := 2;    //2,
  msg[17] := ifthen(FConnectionWay=ISOTCP, CPU.Rack+1, $4D);    //'M',
  msg[18] := ifthen(FConnectionWay=ISOTCP, CPU.Slot,   $57);    //'W',
  msg[19] := $C0;  //$C0,
  msg[20] := 1;    //1,
  msg[21] := 9;    //9;

  try
    FConnectEvent.ResetEvent;
    res := PCommPort.IOCommandASync(iocWriteRead,msg,4,22,DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult);
    if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
    if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>4) then exit;

    len:= IOResult.BufferToRead[2]*$100 + IOResult.BufferToRead[3];

    FConnectEvent.ResetEvent;
    res := PCommPort.IOCommandASync(iocRead,nil,len-4,0,DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult);
    if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
    if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>(len-4)) then exit;

    //negocia o tamanho da pdu

    //prepara a primeira msg
    SetLength(msg,25);
    PrepareMsgToSend(msg);
    PrepareMsgToExchange(msg);
    PrepareMsgPDUHeader(msg, 1);

    SetLength(param,8);
    param[0] := $F0;
    param[1] := 0;
    param[2] := 0;
    param[3] := 1;
    param[4] := 0;
    param[5] := 1;
    param[6] := 3;
    param[7] := $C0;

    PrepareMsgSetParam(msg,param);
    IdentifyMsg(msg, CPU.PDUId);

    FConnectEvent.ResetEvent;
    res := PCommPort.IOCommandASync(iocWriteRead,msg,4,Length(msg),DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult);
    if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
    if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>4) then exit;

    len:= IOResult.BufferToRead[2]*$100 + IOResult.BufferToRead[3];
    //As vezes o CLP manda um pacote de
    //7 bytes que não serve para nada
    //ou se serve pra algo, eu não sei.
    while len = 7 do begin
      //remove os outros 3 bytes que sobraram no buffer de leitura.
      FConnectEvent.ResetEvent;
      res := PCommPort.IOCommandASync(iocRead,nil,3,0,DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult);
      if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
      if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>3) then exit;

      //le novamente...
      FConnectEvent.ResetEvent;
      res := PCommPort.IOCommandASync(iocRead,nil,4,0,DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult);
      if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
      if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>4) then exit;

      //calcula o tamanho do pacote recebido.
      len:= IOResult.BufferToRead[2]*$100 + IOResult.BufferToRead[3];
    end;

    FConnectEvent.ResetEvent;
    res := PCommPort.IOCommandASync(iocRead,nil,len-4,0,DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult);
    if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
    //se resultado nao der ok,
    //ou não fechar com o numero de bytes a ler
    //e não ter o comprimento minimo do ISOTCP sai.
    if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>(len-4)) or ((IOResult.Received+4)<16) then exit;

    //procura o tamanho máximo da PDU no pacote recebido

    datastart:=3+10; //pdu inicia no 3 + 10 do tamanho do cabecalho da pdu
    if IOResult.BufferToRead[4] in [2,3] then //se o tipo é dois ou 3, existe uma word de erro no cabecalho
       datastart:=datastart+2;

    CPU.MaxPDULen := msg[datastart+6]*256 + msg[datastart+7];
    CPU.Connected := true;
  finally
    SetLength(param,0);
    SetLength(msg,0);
    SetLength(IOResult.BufferToRead,0);
    SetLength(IOResult.BufferToWrite,0);
  end;

end;

procedure TISOTCPDriver.PrepareMsgToSend(var msg:BYTES);
var
  len:Integer;
begin
  len := Length(msg);
  if len<4 then
    SetLength(msg, 4);
  msg[00] := 3;
  msg[01] := 0;
  msg[02] := len div $100;
  msg[03] := len mod $100;
end;

procedure TISOTCPDriver.PrepareMsgToExchange(var msg:BYTES);
begin
  if Length(msg)<7 then
    SetLength(msg, 7);
  msg[04] := $02;
  msg[05] := $F0;
  msg[06] := $80;
end;

procedure TISOTCPDriver.PrepareMsgPDUHeader(var msg:BYTES; pdutype:Integer);
var
  pkglen:integer;
begin
  pkglen := ifthen(pdutype in [2,3], 19 , 17);
  if Length(msg)<pkglen then
    SetLength(msg,pkglen);
  msg[07] := $32;
  msg[08] := pdutype;
  msg[09] := 0;
  msg[10] := 0;
  msg[11] := 0;
  msg[12] := 0;
  msg[13] := 0;
  msg[14] := 0;
  msg[15] := 0;
  msg[16] := 0;
end;

procedure TISOTCPDriver.PrepareMsgSetParam(var mgs:BYTES; param:BYTES);
var
  param_start:Integer;
  param_size:Integer;
begin
  param_size:=Length(param);
  param_start:=ifthen(msg[08] in [2,3], 19, 17);

  if Length(msg)<(param_start+param_size) then
    setLength(msg), param_start+param_size);
  Move(param[0],msg[param_start],Length(param));

  //setar o tamanho dos parametros no header...
  msg[13] := (param_size AND $FF00) div $0100;
  msg[14] := (param_size AND $FF);
end;

procedure TISOTCPDriver.IdentifyMsg(var msg:BYTES; var MsgId:Integer);
begin
  if Length(msg)<13 then
     setLength(msg,13);

  inc(MsgId);

  msg[11] := MsgId div $100;
  msg[12] := MsgId mod $100;
end;

procedure TISOTCPDriver.SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
begin
  FConnectionWay:=NewISOConWay;
end;

procedure TISOTCPDriver.DoAddTag(TagObj:TTag);
var
  plc:integer;
begin
  for plc := 0 to High(FCPUs) do begin

  end;
end;

procedure TISOTCPDriver.DoDelTag(TagObj:TTag);
begin

end;

procedure TISOTCPDriver.DoTagChange(TagObj:TTag; Change:TChangeType; oldValue, newValue:Integer);
begin

end;

procedure TISOTCPDriver.DoScanRead(Sender:TObject; var NeedSleep:Integer);
begin

end;

procedure TISOTCPDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
begin

end;

function  TISOTCPDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

function  TISOTCPDriver.DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin

end;

end.