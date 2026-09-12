{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Apoio aos testes: porta de comunicacao falsa, que grava o que o
            driver escreveu e devolve respostas programadas.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Permite exercitar um driver de protocolo inteiro sem CLP e sem rede: o teste
  enfileira as respostas que o "equipamento" daria, manda o driver ler/escrever
  e depois confere byte a byte o frame que saiu.

  A porta responde sincronamente, dentro da thread que chamou - TCommPortDriver
  .IOCommandSync executa Write/Read na propria thread chamadora, sem depender de
  nenhuma thread da porta.
}
{$ELSE}
{:
  @abstract(Test support: a fake communication port that records what the driver
            wrote and replays queued responses.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Allows exercising a whole protocol driver with no PLC and no network: the test
  queues the responses the "device" would give, asks the driver to read/write and
  then checks byte by byte the frame that went out.

  The port answers synchronously, in the calling thread - TCommPortDriver
  .IOCommandSync runs Write/Read on the caller's own thread, with no dependency
  on any port thread.
}
{$ENDIF}
unit testsupport.fakeport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CommPort, commtypes;

type

  { TFakeCommPort }

  TFakeCommPort = class(TCommPortDriver)
  private
    //cada resposta enfileirada e' uma "rajada": o driver pode le-la em varias
    //leituras (o Modbus RTU le o cabecalho e depois o resto), mas uma leitura
    //nunca atravessa a fronteira entre duas rajadas.
    //each queued response is a "burst": the driver may read it in several
    //reads (Modbus RTU reads the header and then the rest), but a single read
    //never crosses the boundary between two bursts.
    FResponses:array of BYTES;
    FResponseIsTimeout:array of Boolean;
    FNextResponse:LongInt;
    FPosInResponse:LongInt;
    FWritten:array of BYTES;
    FFalharAoAbrir, FFalharAoFechar, FConfiguracaoOK:Boolean;
    FLimpezasDeBuffer:LongInt;
    function  GetPendingResponses:LongInt;
    function  GetWriteCount:LongInt;
  protected
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
  public
    constructor Create(AOwner:TComponent); override;

    {$IFDEF PORTUGUES}
    //: Enfileira a proxima resposta que a porta vai devolver numa leitura.
    {$ELSE}
    //: Queues the next response the port will give on a read.
    {$ENDIF}
    procedure QueueResponse(const aResponse:BYTES);

    {$IFDEF PORTUGUES}
    //: Enfileira um timeout - a proxima leitura volta sem dado nenhum.
    {$ELSE}
    //: Queues a timeout - the next read returns no data at all.
    {$ENDIF}
    procedure QueueTimeout;

    {$IFDEF PORTUGUES}
    //: Descarta respostas pendentes e o historico de escritas.
    {$ELSE}
    //: Drops the pending responses and the write history.
    {$ENDIF}
    procedure Reset;

    {$IFDEF PORTUGUES}
    //: Frame escrito pelo driver na enesima escrita (a primeira e' zero).
    {$ELSE}
    //: Frame written by the driver on the nth write (the first one is zero).
    {$ENDIF}
    function  WrittenFrame(aIndex:LongInt):BYTES;

    {$IFDEF PORTUGUES}
    //: Ultimo frame escrito pelo driver. Vazio se o driver nao escreveu nada.
    {$ELSE}
    //: Last frame written by the driver. Empty if the driver wrote nothing.
    {$ENDIF}
    function  LastWrittenFrame:BYTES;

    {$IFDEF PORTUGUES}
    //: Quantas vezes o driver escreveu na porta.
    {$ELSE}
    //: How many times the driver wrote to the port.
    {$ENDIF}
    property  WriteCount:LongInt read GetWriteCount;

    {$IFDEF PORTUGUES}
    //: Quantas respostas enfileiradas ainda nao foram consumidas.
    {$ELSE}
    //: How many queued responses were not consumed yet.
    {$ENDIF}
    property  PendingResponses:LongInt read GetPendingResponses;

    {$IFDEF PORTUGUES}
    //: Faz a proxima abertura da porta falhar, como um dispositivo ausente.
    {$ELSE}
    //: Makes the next port open fail, like a missing device.
    {$ENDIF}
    property  FalharAoAbrir:Boolean read FFalharAoAbrir write FFalharAoAbrir;

    {$IFDEF PORTUGUES}
    //: Faz o fechamento da porta falhar.
    {$ELSE}
    //: Makes closing the port fail.
    {$ENDIF}
    property  FalharAoFechar:Boolean read FFalharAoFechar write FFalharAoFechar;

    {$IFDEF PORTUGUES}
    //: Quando falso, a porta se declara mal configurada.
    {$ELSE}
    //: When false, the port declares itself misconfigured.
    {$ENDIF}
    property  ConfiguracaoOK:Boolean read FConfiguracaoOK write FConfiguracaoOK;

    {$IFDEF PORTUGUES}
    //: Quantas vezes os buffers foram limpos.
    {$ELSE}
    //: How many times the buffers were cleared.
    {$ENDIF}
    property  LimpezasDeBuffer:LongInt read FLimpezasDeBuffer;
  published
    //os avisos sao protegidos na classe base; cada porta concreta os republica
    //the notifications are protected in the base class; every concrete port
    //republishes them
    property OnCommPortOpened;
    property OnCommPortOpenError;
    property OnCommPortClosed;
    property OnCommPortCloseError;
    property OnCommErrorReading;
    property OnCommErrorWriting;
    property OnCommPortDisconnected;
    property ReadRetries;
    property WriteRetries;
    property ClearBuffersOnCommErrors;
  end;

implementation

constructor TFakeCommPort.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FNextResponse:=0;
  FPosInResponse:=0;
  FFalharAoAbrir:=false;
  FFalharAoFechar:=false;
  FConfiguracaoOK:=true;
  FLimpezasDeBuffer:=0;
end;

procedure TFakeCommPort.QueueResponse(const aResponse:BYTES);
begin
  SetLength(FResponses, Length(FResponses)+1);
  SetLength(FResponseIsTimeout, Length(FResponses));
  FResponses[High(FResponses)]:=Copy(aResponse, 0, Length(aResponse));
  FResponseIsTimeout[High(FResponses)]:=false;
end;

procedure TFakeCommPort.QueueTimeout;
begin
  SetLength(FResponses, Length(FResponses)+1);
  SetLength(FResponseIsTimeout, Length(FResponses));
  FResponses[High(FResponses)]:=nil;
  FResponseIsTimeout[High(FResponses)]:=true;
end;

procedure TFakeCommPort.Reset;
begin
  SetLength(FResponses,0);
  SetLength(FResponseIsTimeout,0);
  SetLength(FWritten,0);
  FNextResponse:=0;
  FPosInResponse:=0;
end;

function TFakeCommPort.WrittenFrame(aIndex:LongInt):BYTES;
begin
  if (aIndex<0) or (aIndex>High(FWritten)) then
    raise Exception.CreateFmt('TFakeCommPort: escrita %d nao existe, houve %d escrita(s)',
                              [aIndex, Length(FWritten)]);
  Result:=FWritten[aIndex];
end;

function TFakeCommPort.LastWrittenFrame:BYTES;
begin
  if Length(FWritten)=0 then
    Result:=nil
  else
    Result:=FWritten[High(FWritten)];
end;

function TFakeCommPort.GetWriteCount:LongInt;
begin
  Result:=Length(FWritten);
end;

function TFakeCommPort.GetPendingResponses:LongInt;
begin
  Result:=Length(FResponses)-FNextResponse;
  if Result<0 then
    Result:=0;
end;

procedure TFakeCommPort.Write(Packet:PIOPacket);
var
  len:LongInt;
begin
  len:=Packet^.ToWrite;
  if len>Length(Packet^.BufferToWrite) then
    len:=Length(Packet^.BufferToWrite);

  SetLength(FWritten, Length(FWritten)+1);
  FWritten[High(FWritten)]:=Copy(Packet^.BufferToWrite, 0, len);

  Packet^.Written:=len;
  Packet^.WriteIOResult:=iorOK;
end;

procedure TFakeCommPort.Read(Packet:PIOPacket);
var
  disponivel, len, i:LongInt;
begin
  //sem resposta programada, o "equipamento" ficou calado.
  //with no queued response, the "device" stayed silent.
  if FNextResponse>High(FResponses) then begin
    Packet^.Received:=0;
    Packet^.ReadIOResult:=iorTimeOut;
    exit;
  end;

  //timeout programado: consome a marca e nao entrega dado nenhum.
  //queued timeout: consumes the mark and delivers no data at all.
  if FResponseIsTimeout[FNextResponse] then begin
    inc(FNextResponse);
    FPosInResponse:=0;
    Packet^.Received:=0;
    Packet^.ReadIOResult:=iorTimeOut;
    exit;
  end;

  disponivel:=Length(FResponses[FNextResponse])-FPosInResponse;
  len:=Packet^.ToRead;
  if len>disponivel then
    len:=disponivel;

  SetLength(Packet^.BufferToRead, len);
  for i:=0 to len-1 do
    Packet^.BufferToRead[i]:=FResponses[FNextResponse][FPosInResponse+i];

  inc(FPosInResponse, len);
  if FPosInResponse>=Length(FResponses[FNextResponse]) then begin
    inc(FNextResponse);
    FPosInResponse:=0;
  end;

  Packet^.Received:=len;
  //rajada mais curta do que o driver pediu conta como timeout, que e' o que
  //uma porta real reporta quando o frame nao completa.
  //a burst shorter than what the driver asked for counts as a timeout, which
  //is what a real port reports when the frame does not complete.
  if Cardinal(len)<Packet^.ToRead then
    Packet^.ReadIOResult:=iorTimeOut
  else
    Packet^.ReadIOResult:=iorOK;
end;

procedure TFakeCommPort.NeedSleepBetweenRW;
begin
  //sem espera: o teste nao tem fio nenhum pra respeitar.
  //no wait: the test has no wire to respect.
end;

procedure TFakeCommPort.PortStart(var Ok:Boolean);
begin
  Ok:=not FFalharAoAbrir;
end;

procedure TFakeCommPort.PortStop(var Ok:Boolean);
begin
  Ok:=not FFalharAoFechar;
end;

function TFakeCommPort.ComSettingsOK:Boolean;
begin
  Result:=FConfiguracaoOK;
end;

procedure TFakeCommPort.ClearALLBuffers;
begin
  inc(FLimpezasDeBuffer);
  SetLength(FWritten,0);
end;

end.
