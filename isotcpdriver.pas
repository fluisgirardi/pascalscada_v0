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
  commtypes, s7family;

type
  {: Driver IsoTCP. Baseado na biblioteca LibNodave de ...

  Para endereçar uma memória basta escrever na propriedade MemReadFunction a
  soma o tipo com a área da váriavel (ver tabelas abaixo).

  Tipo de dado:
  @table(
    @rowHead( @cell(Tipo de dado)                  @cell(Valor) )
    @row(     @cell(Byte, 8 bits, unsignaled)      @cell(1)     )
    @row(     @cell(Word, 16 bits, unsignaled)     @cell(2)     )
    @row(     @cell(ShortInt, 16 bits, signaled)   @cell(3)     )
    @row(     @cell(Integer, 32 bits, signaled)    @cell(4)     )
    @row(     @cell(DWord, 32 bits, unsignaled)    @cell(5)     )
    @row(     @cell(Float, 32 bits)                @cell(6)     )
  )

  Area:
  @table(
    @rowHead( @cell(Area)                       @cell(Valor) )
    @row(     @cell(Inputs, Entradas)           @cell(10)     )
    @row(     @cell(Outputs, Saidas)            @cell(20)     )
    @row(     @cell(Flags ou M's)               @cell(30)     )
    @row(     @cell(DB e VM no S7-200 )         @cell(40)     )
    @row(     @cell(DB instanciado)             @cell(50)     )
    @row(     @cell(Counter, S7 300/400)        @cell(60)     )
    @row(     @cell(Timer, S7 300/400)          @cell(70)     )

    @row(     @cell(Special Memory, SM, S7-200) @cell(80)     )
    @row(     @cell(Entrada analógica, S7-200)  @cell(90)     )
    @row(     @cell(Saida analógica, S7-200)    @cell(100)    )
    @row(     @cell(Counter, S7-200)            @cell(110)    )
    @row(     @cell(Timer, S7-200)              @cell(120)    )
  )

  Logo para acessar um byte das entradas, basta colocar na propriedade
  MemReadFunction o valor 10+1 = 11, para acessar a MD100 (DWord) basta
  colocar o valor 30+5 = 35.

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
    function  getResponse(var msgIn:BYTES):Integer; override;
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
  PDUIn:=7;
  PDUOut:=7;
end;

function TISOTCPDriver.ConnectPLC(var CPU:TS7CPU):Boolean;
var
  IOResult:TIOPacket;
  msg:BYTES;
  res:Integer;
  len:Cardinal;
begin
  CPU.Connected:=false;
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
    if (IOResult.ReadIOResult<>iorOK) or (IOResult.Received<>(len-4)) or ((IOResult.Received+4)<>22) then exit;

    //negocia o tamanho da pdu
    CPU.Connected := NegotiatePDUSize(CPU);
  finally
    SetLength(msg,0);
    SetLength(IOResult.BufferToRead,0);
    SetLength(IOResult.BufferToWrite,0);
  end;

end;

function TISOTCPDriver.exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean;
var
  x:TCrossEvent;
begin
  Result := Inherited exchange(CPU, msgOut, msgIn, IsWrite);

  if Length(msgOut)<7 then
    SetLength(msgOut, 7);
  msgOut[04] := $02;
  msgOut[05] := $F0;
  msgOut[06] := $80;

  PrepareToSend(msgOut);

  x:=TCrossEvent.Create(nil, true, false, 'exchange');
  try
    if PCommPort.IOCommandASync(iocWrite,msgOut,0,Length(msgOut),DriverID,0,CommPortCallBack,IsWrite,x,nil)=0 then exit;
    if x.WaitFor($FFFFFFFF)<>wrSignaled then exit;
    Result := getResponse(msgIn) >= ISOTCPMinPacketLen;
  finally
    x.Destroy;
  end;
end;

function  TISOTCPDriver.getResponse(var msgIn:BYTES):Integer;
var
  res, len:Integer;
  FResponseEvent:TCrossEvent;
  IOResult1, IOResult2:TIOPacket;
begin
  if PCommPort=nil then exit;

  Result:=0;

  FResponseEvent:=TCrossEvent.Create(nil, true, false, 'response_event');
  try
    FResponseEvent.ResetEvent;
    res := PCommPort.IOCommandASync(iocRead,nil,4,0,DriverID,0,CommPortCallBack,false,FResponseEvent,@IOResult1);
    if (res=0) or (FResponseEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
    if (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>4) then exit;

    len := IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
    //As vezes o CLP manda um pacote de
    //7 bytes que não serve para nada
    //ou se serve pra algo, eu não sei.
    while len = 7 do begin
      //remove os outros 3 bytes que sobraram no buffer de leitura.
      FResponseEvent.ResetEvent;
      res := PCommPort.IOCommandASync(iocRead,nil,3,0,DriverID,0,CommPortCallBack,false,FResponseEvent,@IOResult2);
      if (res=0) or (FResponseEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
      if (IOResult2.ReadIOResult<>iorOK) or (IOResult2.Received<>3) then exit;

      //le novamente...
      FResponseEvent.ResetEvent;
      res := PCommPort.IOCommandASync(iocRead,nil,4,0,DriverID,0,CommPortCallBack,false,FResponseEvent,@IOResult1);
      if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
      if (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>4) then exit;

      //calcula o tamanho do pacote recebido.
      len:= IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
    end;

    FConnectEvent.ResetEvent;
    res := PCommPort.IOCommandASync(iocRead,nil,len-4,0,DriverID,0,CommPortCallBack,false,FConnectEvent,@IOResult2);
    if (res=0) or (FConnectEvent.WaitFor($FFFFFFFF)<>wrSignaled) then exit;
    //se resultado nao der ok,
    //ou não fechar com o numero de bytes a ler
    //e não ter o comprimento minimo do ISOTCP sai.
    if (IOResult2.ReadIOResult<>iorOK) or (IOResult2.Received<>(len-4)) or ((IOResult2.Received+4)<ISOTCPMinPacketLen) then exit;

    SetLength(msgIn,IOResult1.Received + IOResult2.Received);

    Move(IOResult1.BufferToRead[0], msgIn[0], Length(IOResult1.BufferToRead));
    Move(IOResult2.BufferToRead[0], msgIn[Length(IOResult1.BufferToRead)],Length(IOResult2.BufferToRead));

    Result := IOResult1.Received + IOResult2.Received;
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