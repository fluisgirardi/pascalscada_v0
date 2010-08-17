{:
  @abstract(Implmentação do protocolo ISOTCP.)
  Este driver é baseado no driver ISOTCP da biblioteca
  LibNODAVE de Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Este driver não usa Libnodave, ele é uma reescrita da mesma.

  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit ISOTCPDriver;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  classes, sysutils, S7Types, CrossEvent, commtypes, s7family, CommPort;

type
  {: Driver IsoTCP. Baseado na biblioteca LibNodave de
     Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Para endereçar uma memória basta escrever na propriedade MemReadFunction a
  o código da área da váriavel (ver tabelas abaixo).

  Area:
  @table(
    @rowHead( @cell(Area)                       @cell(Valor) )
    @row(     @cell(Inputs, Entradas)           @cell( 1)    )
    @row(     @cell(Outputs, Saidas)            @cell( 2)    )
    @row(     @cell(Flags ou M's)               @cell( 3)    )
    @row(     @cell(DB e VM no S7-200 )         @cell( 4)    )
    @row(     @cell(Counter, S7 300/400)        @cell( 5)    )
    @row(     @cell(Timer, S7 300/400)          @cell( 6)    )

    @row(     @cell(Special Memory, SM, S7-200) @cell( 7)    )
    @row(     @cell(Entrada analógica, S7-200)  @cell( 8)    )
    @row(     @cell(Saida analógica, S7-200)    @cell( 9)    )
    @row(     @cell(Counter, S7-200)            @cell(10)    )
    @row(     @cell(Timer, S7-200)              @cell(11)    )
  )

  Logo para acessar as entradas, basta colocar na propriedade
  MemReadFunction o valor 1, para acessar a MD100 (DWord) basta
  colocar o valor 5.

  O tipo do tag fica vária com a propriedade TagType.

  }

  TISOTCPDriver = class(TSiemensProtocolFamily)
  protected
    FReadEvent,
    FConnectEvent:TCrossEvent;
    FConnectionWay:TISOTCPConnectionWay;
    procedure SetISOConnectionWay(NewISOConWay:TISOTCPConnectionWay);
  protected
    function  connectPLC(var CPU:TS7CPU):Boolean; override;
    function  exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean; override;
    function  getResponse(ComPort:TCommPortDriver; var msgIn:BYTES; var BytesRead:Integer):TIOResult; override;
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

uses math, syncobjs;

constructor TISOTCPDriver.Create(AOwner:TComponent);
begin
  Inherited Create(AOwner);
  PDUIncoming:=7;
  PDUOutgoing:=7;
  FConnectEvent:=TCrossEvent.Create(nil, true, false, Name+'_DID'+IntToStr(DriverID));
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
  PrepareToSend(msg);

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

    retries := 1;
    while (len<>22) and (retries<3) do begin
      res := PCommPort.IOCommandASync(iocRead,nil,4,0,DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult);
      if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
      if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>4) then exit;

      len:= IOResult.BufferToRead[2]*$100 + IOResult.BufferToRead[3];

      FConnectEvent.ResetEvent;
      res := PCommPort.IOCommandASync(iocRead,nil,len-4,0,DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult);
      if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
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
  x:TCrossEvent;
  res:Integer;
  retries, BytesRead:Integer;
  resget:TIOResult;
  currentPort:TCommPortDriver;
begin
  if PCommPort=nil then exit;

  currentPort:=PCommPort;

  Result := Inherited exchange(CPU, msgOut, msgIn, IsWrite);

  if Length(msgOut)<7 then
    SetLength(msgOut, 7);
  msgOut[04] := $02;
  msgOut[05] := $F0;
  msgOut[06] := $80;

  PrepareToSend(msgOut);

  x:=TCrossEvent.Create(nil, true, false, 'exchange');
  try
    InternalLeaveScanCS;
    res:=currentPort.IOCommandASync(iocWrite,msgOut,0,Length(msgOut),DriverID,0,CommPortCallBack,IsWrite,x,nil);
    if res=0 then exit;
    if x.WaitFor($FFFFFFFF)<>wrSignaled then exit;
    retries:=0;

    resget := getResponse(currentPort, msgIn, BytesRead);
    while (resget<>iorOk) and (retries<3) do begin

      if resget<>iorTimeOut then
        Inc(retries)
      else
        Sleep(5);

      resget := getResponse(currentPort, msgIn, BytesRead);
    end;

    Result:=BytesRead>ISOTCPMinPacketLen;
  finally
    InternalEnterScanCS;
    x.Destroy;
  end;
end;

function  TISOTCPDriver.getResponse(ComPort:TCommPortDriver; var msgIn:BYTES; var BytesRead:Integer):TIOResult;
var
  res, len:Integer;
  FResponseEvent:TCrossEvent;
  IOResult1, IOResult2:TIOPacket;
  curport:TCommPortDriver;
begin
  if ComPort<>nil then
    curport:=ComPort
  else begin
    if PCommPort=nil then exit;
    curport:=PCommPort;
  end;

  Result:=iorNotReady;

  FResponseEvent:=TCrossEvent.Create(nil, true, false, 'response_event');
  try
    FResponseEvent.ResetEvent;
    res := curport.IOCommandASync(iocRead,nil,4,0,DriverID,0,CommPortCallBack,false,FResponseEvent,@IOResult1);
    if (res=0) or (FResponseEvent.WaitFor($FFFFFFFF)<>wrSignaled) then begin
      BytesRead:=0;
      Result:=iorNotReady;
      exit;
    end;

    if (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>4) then begin
      BytesRead:=IOResult1.Received;
      Result:=IOResult1.ReadIOResult;
      exit;
    end;

    len := IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
    //As vezes o CLP manda um pacote de
    //7 bytes que não serve para nada
    //ou se serve pra algo, eu não sei.
    while len = 7 do begin
      //remove os outros 3 bytes que sobraram no buffer de leitura.
      FResponseEvent.ResetEvent;
      res := curport.IOCommandASync(iocRead,nil,3,0,DriverID,0,CommPortCallBack,false,FResponseEvent,@IOResult2);
      if (res=0) or (FResponseEvent.WaitFor($FFFFFFFF)<>wrSignaled) then  begin
        BytesRead:=0;
        Result:=iorNotReady;
        exit;
      end;

      if (IOResult2.ReadIOResult<>iorOK) or (IOResult2.Received<>3) then begin
        BytesRead:=IOResult2.Received;
        Result:= IOResult2.ReadIOResult;
        exit;
      end;

      //le novamente...
      FResponseEvent.ResetEvent;
      res := curport.IOCommandASync(iocRead,nil,4,0,DriverID,0,CommPortCallBack,false,FResponseEvent,@IOResult1);
      if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then begin
        BytesRead:=0;
        Result:=iorNotReady;
        exit;
      end;

      if (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>4) then begin
        BytesRead:=IOResult1.Received;
        Result:= IOResult1.ReadIOResult;
        exit;
      end;
      //calcula o tamanho do pacote recebido.
      len:= IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
    end;

    FConnectEvent.ResetEvent;
    res := curport.IOCommandASync(iocRead,nil,len-4,0,DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult2);
    if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then begin
      BytesRead:=0;
      Result:=iorNotReady;
      exit;
    end;
    //se resultado nao der ok,
    //ou não fechar com o numero de bytes a ler
    //e não ter o comprimento minimo do ISOTCP sai.
    if (IOResult2.ReadIOResult<>iorOK) or (IOResult2.Received<>(len-4)) then begin
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
    FResponseEvent.Destroy;
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

end.