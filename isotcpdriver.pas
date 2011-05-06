{:
  @abstract(Implmentação do protocolo ISOTCP.)
  Este driver é baseado no driver ISOTCP da biblioteca
  LibNODAVE de Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Este driver não usa Libnodave, ele é uma reescrita da mesma.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
unit ISOTCPDriver;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  classes, sysutils, S7Types, commtypes, s7family;

type
  {: Driver IsoTCP. Baseado na biblioteca LibNodave de
     Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Para endereçar uma memória, veja a documentação da classe
  TSiemensProtocolFamily.

  @seealso(TSiemensProtocolFamily).
  }

  TISOTCPDriver = class(TSiemensProtocolFamily)
  protected
    FConnectionWay:TISOTCPConnectionWay;
    procedure SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
    function NotifyThisEvents: TNotifyThisEvents; override;
    procedure PortClosed(Sender: TObject); override;
    procedure PortDisconnected(Sender: TObject); override;
  protected
    function  connectPLC(var CPU:TS7CPU):Boolean; override;
    function  exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean; override;
    function  getResponse(var msgIn:BYTES; var BytesRead:Integer):TIOResult; override;
    procedure PrepareToSend(var msg: BYTES); override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    property ReadSomethingAlways;
    property ConnectionWay:TISOTCPConnectionWay read FConnectionWay write SetISOConnectionWay;
  end;

const
  ISOTCPMinPacketLen = 16;

implementation

uses math;

constructor TISOTCPDriver.Create(AOwner:TComponent);
begin
  Inherited Create(AOwner);
  PDUIncoming:=7;
  PDUOutgoing:=7;
end;

function TISOTCPDriver.ConnectPLC(var CPU:TS7CPU):Boolean;
var
  IOResult:TIOPacket;
  msg:BYTES;
  res:Integer;
  len:Cardinal;
  retries:Integer;
begin
  CPU.Connected:=false;
  Result:=false;
  if PCommPort=nil then exit;

  //incializa conexao
  SetLength(msg,22);
  msg[04] := $11;  // $11,
  msg[05] := $E0;  // $E0,
  msg[06] := 0;    // 0,
  msg[07] := 0;    // 0,
  msg[08] := 0;    // 0,
  msg[09] := 1;    // 1,
  msg[10] := 0;    // 0,
  msg[11] := $C1;  // $C1,
  msg[12] := 2;    // 2,
  msg[13] := ifthen(FConnectionWay=ISOTCP, 1, $4D);    //'M',
  msg[14] := ifthen(FConnectionWay=ISOTCP, 0, $57);    //'W',
  msg[15] := $C2;  // $C2,
  msg[16] := 2;    // 2,
  msg[17] := ifthen(FConnectionWay=ISOTCP, CPU.Rack+1, $4D);    //'M',
  msg[18] := ifthen(FConnectionWay=ISOTCP, CPU.Slot,   $57);    //'W',
  msg[19] := $C0;  // $C0,
  msg[20] := 1;    // 1,
  msg[21] := 9;    // 9;
  PrepareToSend(msg);

  try
    res := PCommPort.IOCommandSync(iocWriteRead,msg,4,22,DriverID,ifthen(FConnectionWay=ISOTCP_VIA_CP243,1000,0),CommPortCallBack,false,nil,@IOResult);
    if (res=0) then exit;
    if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>4) then exit;

    len:= IOResult.BufferToRead[2]*$100 + IOResult.BufferToRead[3];

    res := PCommPort.IOCommandSync(iocRead,nil,len-4,0,DriverID,0,CommPortCallBack,false,nil,@IOResult);
    if (res=0) then exit;
    if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>(len-4)) then exit;

    retries := 1;
    while (len<>22) and (retries<3) do begin
      res := PCommPort.IOCommandSync(iocRead,nil,4,0,DriverID,0,CommPortCallBack,false,nil,@IOResult);
      if (res=0) then exit;
      if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>4) then exit;

      len:= IOResult.BufferToRead[2]*$100 + IOResult.BufferToRead[3];

      res := PCommPort.IOCommandSync(iocRead,nil,len-4,0,DriverID,0,CommPortCallBack,false,nil,@IOResult);
      if (res=0) then exit;
      if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>(len-4)) then exit;
    end;

    //negocia o tamanho da pdu
    if len=22 then
      CPU.Connected := NegotiatePDUSize(CPU);
  finally
    SetLength(msg,0);
    SetLength(IOResult.BufferToRead,0);
    SetLength(IOResult.BufferToWrite,0);
    Result:=CPU.Connected;
  end;
end;

function TISOTCPDriver.exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean;
var
  res:Integer;
  retries, BytesRead:Integer;
  resget:TIOResult;
begin
  if PCommPort=nil then exit;

  Result := Inherited exchange(CPU, msgOut, msgIn, IsWrite);

  if Length(msgOut)<7 then
    SetLength(msgOut, 7);
  msgOut[04] := $02;
  msgOut[05] := $F0;
  msgOut[06] := $80;

  PrepareToSend(msgOut);

  try
    res:=PCommPort.IOCommandSync(iocWrite,msgOut,0,Length(msgOut),DriverID,0,CommPortCallBack,IsWrite,nil,nil);
    if res=0 then begin
      Result:=false;
      exit;
    end;
    retries:=0;

    resget := getResponse(msgIn, BytesRead);
    while (resget<>iorOk) and (retries<3) do begin

      if resget<>iorTimeOut then
        Inc(retries)
      else
        Sleep(5);

      resget := getResponse(msgIn, BytesRead);
    end;

    Result:=BytesRead>ISOTCPMinPacketLen;
  finally

  end;
end;

function  TISOTCPDriver.getResponse(var msgIn:BYTES; var BytesRead:Integer):TIOResult;
var
  res, len:Integer;
  IOResult1, IOResult2:TIOPacket;
begin
  Result:=iorNotReady;

  try
    res := PCommPort.IOCommandSync(iocRead,nil,7,0,DriverID,0,CommPortCallBack,false,nil,@IOResult1);
    if (res=0) then begin
      BytesRead:=0;
      Result:=iorNotReady;
      exit;
    end;

    if (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>7) then begin
      BytesRead:=IOResult1.Received;
      Result:=IOResult1.ReadIOResult;
      exit;
    end;

    len := IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
    //As vezes o CLP manda um pacote de
    //7 bytes que não serve para nada
    //ou se serve pra algo, eu não sei.
    while len = 7 do begin
      //le novamente...
      res := PCommPort.IOCommandSync(iocRead,nil,7,0,DriverID,0,CommPortCallBack,false,nil,@IOResult1);
      if (res=0) then begin
        BytesRead:=0;
        Result:=iorNotReady;
        exit;
      end;

      if (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>7) then begin
        BytesRead:=IOResult1.Received;
        Result:= IOResult1.ReadIOResult;
        exit;
      end;
      //calcula o tamanho do pacote recebido.
      len:= IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
    end;

    res := PCommPort.IOCommandSync(iocRead,nil,len-7,0,DriverID,0,CommPortCallBack,false,nil,@IOResult2);
    if (res=0) then begin
      BytesRead:=0;
      Result:=iorNotReady;
      exit;
    end;
    //se resultado nao der ok,
    //ou não fechar com o numero de bytes a ler
    //e não ter o comprimento minimo do ISOTCP sai.
    if (IOResult2.ReadIOResult<>iorOK) or (IOResult2.Received<>(len-7)) then begin
      BytesRead:=IOResult2.Received;
      Result:= IOResult2.ReadIOResult;
      exit;
    end;

    SetLength(msgIn,IOResult1.ToRead + IOResult2.ToRead);

    Move(IOResult1.BufferToRead[0], msgIn[0], IOResult1.ToRead);
    Move(IOResult2.BufferToRead[0], msgIn[IOResult1.ToRead],Length(IOResult2.BufferToRead));

    BytesRead := IOResult1.Received + IOResult2.Received;
    Result:=iorOK;
  finally
    SetLength(IOResult1.BufferToRead,0);
    SetLength(IOResult1.BufferToWrite,0);
    SetLength(IOResult2.BufferToRead,0);
    SetLength(IOResult2.BufferToWrite,0);
  end;
end;

procedure TISOTCPDriver.PrepareToSend(var msg:BYTES);
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

procedure TISOTCPDriver.SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
begin
  FConnectionWay:=NewISOConWay;
end;

function TISOTCPDriver.NotifyThisEvents: TNotifyThisEvents;
begin
  Result:=[ntePortClosed, ntePortDisconnected];
end;

procedure TISOTCPDriver.PortClosed(Sender: TObject);
begin
  PortDisconnected(Self);
end;

procedure TISOTCPDriver.PortDisconnected(Sender: TObject);
var
  plc:Integer;
begin
  for plc := 0 to High(FCPUs) do
    FCPUs[plc].Connected:=false;
end;

end.