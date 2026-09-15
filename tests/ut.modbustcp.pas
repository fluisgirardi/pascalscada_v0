{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TModBusTCPDriver: cabecalho MBAP e interpretacao das
            respostas.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(TModBusTCPDriver tests: MBAP header and response parsing.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit ut.modbustcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, PLCTagNumber, modbustcp,
  testsupport.bytes, testsupport.protocol, testsupport.fakeport;

type

  { TModBusTCPProbe }

  TModBusTCPProbe = class(TModBusTCPDriver)
  protected
    //a varredura e' uma thread de verdade; com tags cadastrados ela sairia
    //lendo pela mesma porta de mentira. Aqui quem chama a varredura e' o
    //teste, uma volta de cada vez.
    //the scan is a real thread; with tags registered it would go reading
    //through the same fake port. Here it is the test that calls the scan,
    //one turn at a time.
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
  public
    function  Encode(aTag:TTagRec; aToWrite:TArrayOfDouble; var aResultLen:LongInt):BYTES;
    function  Decode(aPkg:TIOPacket; out aValues:TArrayOfDouble):TProtocolIOResult;
    function  ReadSync(const aTag:TTagRec; out aValues:TArrayOfDouble):TProtocolIOResult;
    function  WriteSync(const aTag:TTagRec; const aValues:TArrayOfDouble):TProtocolIOResult;
    procedure ScanOnce;
    function  Cached(const aTag:TTagRec):TScanReadRec;
  end;

  { TTestModBusTCPOverAPort }

  //o mesmo driver falando com uma porta de mentira: o caminho inteiro, com o
  //MBAP na frente e a resposta lida em duas partes
  //the same driver talking to a fake port: the whole path, with the MBAP in
  //front and the answer read in two parts
  TTestModBusTCPOverAPort = class(TTestCase)
  private
    FPort:TFakeCommPort;
    FDrv:TModBusTCPProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AReadGoesOutAndBackThroughThePort;
    procedure AWriteGoesOutAndBackThroughThePort;
    procedure WithNoAnswerTheResultIsTimeout;
    procedure AnAnswerFromAnotherUnitLeavesThePortUnlocked;
    procedure AnExceptionAnswerLeavesThePortUnlocked;
  end;

  { TTestModBusScan }

  //a varredura: os tags viram blocos, os blocos viram pedidos, e o que volta
  //fica guardado para os tags lerem
  //the scan: tags become blocks, blocks become requests, and what comes back
  //is kept for the tags to read
  TTestModBusScan = class(TTestCase)
  private
    FPort:TFakeCommPort;
    FDrv:TModBusTCPProbe;
    FTags:TList;
    function  NewTag(aStation, aAddress, aReadFunction:LongInt):TPLCTagNumber;
    function  Quantity(const aFrame:BYTES):LongInt;
    function  Address(const aFrame:BYTES):LongInt;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ARegisteredTagMakesTheScanReadItsRegister;
    procedure WhatTheScanReadIsKeptForTheTag;
    procedure TwoAdjacentTagsAreReadInOneBlock;
    procedure TagsFarApartAreReadInSeparateBlocks;
    procedure AFailedScanIsKeptAsAFailureForTheTag;
    procedure ATagWithAnUnknownFunctionIsInvalid;
    procedure ATagOnStationZeroIsInvalid;
    procedure ARemovedTagLeavesTheScan;
    procedure WithNoTagsTheScanReadsNothing;
  end;

  { TTestModBusTCP }

  TTestModBusTCP = class(TTestCase)
  private
    FDrv:TModBusTCPProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ARegisterReadBuildsTheFrameWithMBAP;
    procedure ARegisterReadPredicts13AnswerBytes;
    procedure AMultipleRegisterWriteBuildsTheFrame;
    procedure TheLengthFieldCountsTheBytesAfterIt;
    procedure ARegisterAnswerBecomesValues;
    procedure AnAnswerWithAnExceptionBecomesTheMatchingError;
    procedure AnAnswerFromAnotherUnitBecomesACommunicationError;
    procedure ATimeoutOnTheReadBecomesATimeout;
    procedure AnIncompleteAnswerWithAValidHeaderIsNotAccepted;
    procedure AnAnswerShorterThanTheHeaderIsRefused;
    procedure AByteCountThatDoesNotMatchTheRequestIsRefused;
    procedure AFrameThatDoesNotCarryTheBytesItDeclaresIsRefused;
    procedure ARequestBelowTheMinimumIsNotDecoded;
  end;

implementation

{ TModBusTCPProbe }

procedure TModBusTCPProbe.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  NeedSleep:=500;
end;

function TModBusTCPProbe.ReadSync(const aTag:TTagRec; out aValues:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DoRead(aTag, aValues, true);
end;

function TModBusTCPProbe.WriteSync(const aTag:TTagRec; const aValues:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DoWrite(aTag, aValues, true);
end;

procedure TModBusTCPProbe.ScanOnce;
var
  sleepFor:LongInt;
begin
  //como o SafeScanRead da thread: com as duas secoes criticas tomadas - a
  //leitura as solta enquanto espera a porta e as toma de volta
  //like the thread's SafeScanRead: with both critical sections held - the
  //read releases them while waiting on the port and takes them back
  sleepFor:=0;
  FWriteCS.Enter;
  FReadCS.Enter;
  try
    inherited DoScanRead(Self, sleepFor);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
  end;
end;

function TModBusTCPProbe.Cached(const aTag:TTagRec):TScanReadRec;
begin
  Result.Values:=nil;
  Result.LastQueryResult:=ioNone;
  DoGetValue(aTag, Result);
end;

{ TTestModBusTCPOverAPort }

procedure TTestModBusTCPOverAPort.SetUp;
begin
  FPort:=TFakeCommPort.Create(nil);
  FPort.Active:=true;
  FDrv:=TModBusTCPProbe.Create(nil);
  FDrv.CommunicationPort:=FPort;
end;

procedure TTestModBusTCPOverAPort.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPort);
end;

procedure TTestModBusTCPOverAPort.AReadGoesOutAndBackThroughThePort;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  FPort.QueueResponse(BytesOf('00 00 00 00 00 07 01 03 04 00 0A 00 14'));

  res:=FDrv.ReadSync(TagRecFor(1, $03, 0, 0, 2), vals);

  AssertEquals('resultado', Ord(ioOk), Ord(res));
  AssertBytesEqual('o pedido que saiu',
                   BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                   FPort.LastWrittenFrame);
  AssertEquals('dois valores',      2,  Length(vals));
  AssertEquals('primeiro registro', 10, vals[0], 0);
  AssertEquals('segundo registro',  20, vals[1], 0);
  AssertEquals('resposta consumida inteira', 0, FPort.PendingResponses);
end;

procedure TTestModBusTCPOverAPort.AWriteGoesOutAndBackThroughThePort;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  SetLength(vals,2);
  vals[0]:=10;
  vals[1]:=20;
  //o escravo confirma com endereco e quantidade
  //the slave confirms with address and quantity
  FPort.QueueResponse(BytesOf('00 00 00 00 00 06 01 10 00 00 00 02'));

  res:=FDrv.WriteSync(TagRecFor(1, 0, $10, 0, 2), vals);

  AssertEquals('resultado', Ord(ioOk), Ord(res));
  AssertBytesEqual('o pedido que saiu',
                   BytesOf('00 00 00 00 00 0B 01 10 00 00 00 02 04 00 0A 00 14'),
                   FPort.LastWrittenFrame);
end;

procedure TTestModBusTCPOverAPort.WithNoAnswerTheResultIsTimeout;
var
  vals:TArrayOfDouble;
begin
  AssertEquals(Ord(ioTimeOut), Ord(FDrv.ReadSync(TagRecFor(1, $03, 0, 0, 2), vals)));
end;

procedure TTestModBusTCPOverAPort.AnAnswerFromAnotherUnitLeavesThePortUnlocked;
var
  vals:TArrayOfDouble;
begin
  //a leitura tranca a porta e, numa resposta de outra unidade, desiste; tem
  //que destrancar mesmo assim, ou outro driver na mesma porta espera para
  //sempre
  //the read locks the port and, on another unit's answer, gives up; it has to
  //unlock all the same, or another driver on the same port waits forever
  FPort.QueueResponse(BytesOf('00 00 00 00 00 07 02 03 04 00 0A 00 14'));

  AssertEquals('erro de comunicacao', Ord(ioCommError), Ord(FDrv.ReadSync(TagRecFor(1, $03, 0, 0, 2), vals)));

  AssertTrue('outro driver consegue a porta', FPort.Lock(9999));
  FPort.Unlock(9999);
end;

procedure TTestModBusTCPOverAPort.AnExceptionAnswerLeavesThePortUnlocked;
var
  vals:TArrayOfDouble;
begin
  FPort.QueueResponse(BytesOf('00 00 00 00 00 03 01 83 02'));

  AssertEquals('endereco ilegal', Ord(ioIllegalRegAddress), Ord(FDrv.ReadSync(TagRecFor(1, $03, 0, 0, 2), vals)));

  AssertTrue('outro driver consegue a porta', FPort.Lock(9999));
  FPort.Unlock(9999);
end;

{ TTestModBusScan }

procedure TTestModBusScan.SetUp;
begin
  FPort:=TFakeCommPort.Create(nil);
  FPort.Active:=true;
  FDrv:=TModBusTCPProbe.Create(nil);
  FDrv.CommunicationPort:=FPort;
  FTags:=TList.Create;
end;

procedure TTestModBusScan.TearDown;
var
  i:Integer;
begin
  for i:=0 to FTags.Count-1 do
    TObject(FTags[i]).Free;
  FreeAndNil(FTags);
  FreeAndNil(FDrv);
  FreeAndNil(FPort);
end;

function TTestModBusScan.NewTag(aStation, aAddress, aReadFunction:LongInt):TPLCTagNumber;
begin
  Result:=TPLCTagNumber.Create(nil);
  Result.PLCStation:=aStation;
  Result.MemAddress:=aAddress;
  Result.MemReadFunction:=aReadFunction;
  Result.RefreshTime:=100;
  Result.ProtocolDriver:=FDrv;
  FTags.Add(Result);
end;

function TTestModBusScan.Quantity(const aFrame:BYTES):LongInt;
begin
  Result:=aFrame[10]*256+aFrame[11];
end;

function TTestModBusScan.Address(const aFrame:BYTES):LongInt;
begin
  Result:=aFrame[8]*256+aFrame[9];
end;

procedure TTestModBusScan.ARegisteredTagMakesTheScanReadItsRegister;
begin
  NewTag(1, 5, 3);
  FPort.QueueResponse(BytesOf('00 00 00 00 00 05 01 03 02 00 2A'));

  FDrv.ScanOnce;

  AssertEquals('uma leitura',   1, FPort.WriteCount);
  AssertEquals('no endereco 5', 5, Address(FPort.WrittenFrame(0)));
  AssertEquals('um registro',   1, Quantity(FPort.WrittenFrame(0)));
end;

procedure TTestModBusScan.WhatTheScanReadIsKeptForTheTag;
var
  guardado:TScanReadRec;
begin
  NewTag(1, 5, 3);
  FPort.QueueResponse(BytesOf('00 00 00 00 00 05 01 03 02 00 2A'));
  FDrv.ScanOnce;

  guardado:=FDrv.Cached(TagRecFor(1, 3, 0, 5, 1));

  AssertEquals('leitura ok', Ord(ioOk), Ord(guardado.LastQueryResult));
  AssertEquals('um valor',   1,  Length(guardado.Values));
  AssertEquals('42',         42, guardado.Values[0], 0);
end;

procedure TTestModBusScan.TwoAdjacentTagsAreReadInOneBlock;
begin
  //dois registros vizinhos, um pedido so'
  //two neighbouring registers, a single request
  NewTag(1, 5, 3);
  NewTag(1, 6, 3);
  FPort.QueueResponse(BytesOf('00 00 00 00 00 07 01 03 04 00 2A 00 2B'));

  FDrv.ScanOnce;

  AssertEquals('uma leitura',     1, FPort.WriteCount);
  AssertEquals('a partir do 5',   5, Address(FPort.WrittenFrame(0)));
  AssertEquals('dois registros',  2, Quantity(FPort.WrittenFrame(0)));
end;

procedure TTestModBusScan.TagsFarApartAreReadInSeparateBlocks;
var
  a, b:LongInt;
begin
  //mais longe do que o buraco permitido: dois blocos, uma volta de varredura
  //para cada
  //farther than the allowed hole: two blocks, one scan turn for each
  NewTag(1, 5, 3);
  NewTag(1, 500, 3);
  FPort.QueueResponse(BytesOf('00 00 00 00 00 05 01 03 02 00 2A'));
  FPort.QueueResponse(BytesOf('00 00 00 00 00 05 01 03 02 00 2B'));

  //um bloco recem-criado conta como lido agora; e' preciso vencer o tempo de
  //atualizacao (100 ms) para os dois pedirem leitura, e a varredura atende
  //um por volta - o mais atrasado primeiro
  //a block just created counts as read now; the refresh time (100 ms) has to
  //pass for both to ask for a read, and the scan serves one per turn - the
  //most overdue first
  Sleep(150);
  FDrv.ScanOnce;
  Sleep(150);
  FDrv.ScanOnce;

  AssertEquals('duas leituras', 2, FPort.WriteCount);
  a:=Address(FPort.WrittenFrame(0));
  b:=Address(FPort.WrittenFrame(1));
  AssertTrue(Format('uma para cada bloco (leu %d e %d)',[a,b]), ((a=5) and (b=500)) or ((a=500) and (b=5)));
  AssertEquals('um registro cada', 1, Quantity(FPort.WrittenFrame(0)));
end;

procedure TTestModBusScan.AFailedScanIsKeptAsAFailureForTheTag;
var
  guardado:TScanReadRec;
begin
  //o escravo nao respondeu: o tag tem que saber que a leitura falhou
  //the slave did not answer: the tag has to know the read failed
  NewTag(1, 5, 3);

  FDrv.ScanOnce;

  guardado:=FDrv.Cached(TagRecFor(1, 3, 0, 5, 1));
  AssertTrue('nao esta ok', guardado.LastQueryResult<>ioOk);
end;

procedure TTestModBusScan.ATagWithAnUnknownFunctionIsInvalid;
var
  tag:TPLCTagNumber;
begin
  tag:=NewTag(1, 5, 9);
  FPort.QueueResponse(BytesOf('00 00 00 00 00 05 01 03 02 00 2A'));

  FDrv.ScanOnce;

  AssertEquals('nada lido para ele', 0, FPort.WriteCount);
  AssertTrue('ainda e'' um tag do driver', tag.ProtocolDriver=FDrv);
end;

procedure TTestModBusScan.ATagOnStationZeroIsInvalid;
begin
  NewTag(0, 5, 3);

  FDrv.ScanOnce;

  AssertEquals('nada lido', 0, FPort.WriteCount);
end;

procedure TTestModBusScan.ARemovedTagLeavesTheScan;
var
  tag:TPLCTagNumber;
begin
  tag:=NewTag(1, 5, 3);

  tag.ProtocolDriver:=nil;
  FDrv.ScanOnce;

  AssertEquals('nada lido', 0, FPort.WriteCount);
end;

procedure TTestModBusScan.WithNoTagsTheScanReadsNothing;
begin
  FDrv.ScanOnce;

  AssertEquals('nada lido', 0, FPort.WriteCount);
end;

function TModBusTCPProbe.Encode(aTag:TTagRec; aToWrite:TArrayOfDouble; var aResultLen:LongInt):BYTES;
begin
  Result:=EncodePkg(aTag, aToWrite, aResultLen);
end;

function TModBusTCPProbe.Decode(aPkg:TIOPacket; out aValues:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DecodePkg(aPkg, aValues);
end;

{ TTestModBusTCP }

procedure TTestModBusTCP.SetUp;
begin
  FDrv:=TModBusTCPProbe.Create(nil);
end;

procedure TTestModBusTCP.TearDown;
begin
  FreeAndNil(FDrv);
end;

procedure TTestModBusTCP.ARegisterReadBuildsTheFrameWithMBAP;
var
  len:LongInt;
begin
  //MBAP: transacao 0000, protocolo 0000, tamanho 0006, unidade 01. Sem CRC.
  AssertBytesEqual('read request 03',
                   BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                   FDrv.Encode(TagRecFor(1, $03, 0, 0, 2), nil, len));
end;

procedure TTestModBusTCP.ARegisterReadPredicts13AnswerBytes;
var
  len:LongInt;
begin
  //6 de MBAP + unidade + funcao + contagem + 2 words = 13
  FDrv.Encode(TagRecFor(1, $03, 0, 0, 2), nil, len);
  AssertEquals('expected response size', 13, len);
end;

procedure TTestModBusTCP.AMultipleRegisterWriteBuildsTheFrame;
var
  len:LongInt;
  vals:TArrayOfDouble;
begin
  SetLength(vals,2);
  vals[0]:=10;
  vals[1]:=20;
  AssertBytesEqual('write 16',
                   BytesOf('00 00 00 00 00 0B 01 10 00 00 00 02 04 00 0A 00 14'),
                   FDrv.Encode(TagRecFor(1, 0, $10, 0, 2), vals, len));
end;

procedure TTestModBusTCP.TheLengthFieldCountsTheBytesAfterIt;
var
  len:LongInt;
  vals:TArrayOfDouble;
  frame:BYTES;
  declarado:LongInt;
begin
  SetLength(vals,2);
  vals[0]:=10;
  vals[1]:=20;
  frame:=FDrv.Encode(TagRecFor(1, 0, $10, 0, 2), vals, len);

  declarado:=(frame[4] shl 8) + frame[5];
  AssertEquals('length field vs bytes after the field',
               Length(frame)-6, declarado);
end;

procedure TTestModBusTCP.ARegisterAnswerBecomesValues;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  res:=FDrv.Decode(IOPacketFor(BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                               BytesOf('00 00 00 00 00 07 01 03 04 00 0A 00 14')), vals);
  AssertEquals('result', Ord(ioOk), Ord(res));
  AssertEquals('number of values', 2, Length(vals));
  AssertEquals('first register', 10, vals[0], 0);
  AssertEquals('second register',  20, vals[1], 0);
end;

procedure TTestModBusTCP.AnAnswerWithAnExceptionBecomesTheMatchingError;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  res:=FDrv.Decode(IOPacketFor(BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                               BytesOf('00 00 00 00 00 03 01 83 02')), vals);
  AssertEquals('exception 02', Ord(ioIllegalRegAddress), Ord(res));
end;

procedure TTestModBusTCP.AnAnswerFromAnotherUnitBecomesACommunicationError;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  //pedimos a unidade 1 e respondeu a unidade 2
  res:=FDrv.Decode(IOPacketFor(BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                               BytesOf('00 00 00 00 00 07 02 03 04 00 0A 00 14')), vals);
  AssertEquals('wrong unit', Ord(ioCommError), Ord(res));
end;

procedure TTestModBusTCP.ATimeoutOnTheReadBecomesATimeout;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
  pkg:TIOPacket;
begin
  //a porta entrega o buffer do tamanho pedido, zerado, e avisa o timeout
  pkg:=IOPacketFor(BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                   BytesOf('00 00 00 00 00 00 00 00 00 00 00 00 00'));
  pkg.ReadIOResult:=iorTimeOut;
  pkg.Received:=0;

  res:=FDrv.Decode(pkg, vals);
  AssertEquals('timeout', Ord(ioTimeOut), Ord(res));
end;

procedure TTestModBusTCP.AnIncompleteAnswerWithAValidHeaderIsNotAccepted;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
  pkg:TIOPacket;
begin
  //o pior caso do timeout: chegou o comeco da resposta, com a unidade e a
  //funcao certas, e o resto nao veio. Conferir so' os campos nao basta - o
  //driver tem que olhar o resultado da leitura, senao entrega o que sobrou
  //no buffer como se fossem registradores do equipamento
  pkg:=IOPacketFor(BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                   BytesOf('00 00 00 00 00 07 01 03 04 00 00 00 00'));
  pkg.ReadIOResult:=iorTimeOut;
  pkg.Received:=9;

  res:=FDrv.Decode(pkg, vals);
  AssertEquals('incomplete read', Ord(ioTimeOut), Ord(res));
end;

procedure TTestModBusTCP.AnAnswerShorterThanTheHeaderIsRefused;
var
  vals:TArrayOfDouble;
  resp:BYTES;
  n:LongInt;
begin
  //o cabecalho MBAP tem 7 bytes e a funcao vem no oitavo. Abaixo disso o
  //driver conferia a unidade indexando o buffer sem olhar o tamanho: lia fora
  //do vetor, e com a resposta vazia dava violacao de acesso
  for n:=0 to 7 do begin
    SetLength(resp, n);
    if n>0 then FillChar(resp[0], n, 0);

    AssertEquals('answer of '+IntToStr(n)+' bytes', Ord(ioCommError),
                 Ord(FDrv.Decode(IOPacketFor(BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                                             resp), vals)));
  end;
end;

procedure TTestModBusTCP.AByteCountThatDoesNotMatchTheRequestIsRefused;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  //pedimos 2 registradores, ou seja 4 bytes, e a resposta declara 2. Antes o
  //driver decodificava assim mesmo e o registrador que faltava virava zero
  res:=FDrv.Decode(IOPacketFor(BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                               BytesOf('00 00 00 00 00 05 01 03 02 00 0A')), vals);
  AssertEquals('count smaller than the one asked for', Ord(ioCommError), Ord(res));
end;

procedure TTestModBusTCP.AFrameThatDoesNotCarryTheBytesItDeclaresIsRefused;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  //a contagem esta certa, mas o quadro acaba antes: declara 4 bytes e traz 2
  res:=FDrv.Decode(IOPacketFor(BytesOf('00 00 00 00 00 06 01 03 00 00 00 02'),
                               BytesOf('00 00 00 00 00 07 01 03 04 00 0A')), vals);
  AssertEquals('truncated frame', Ord(ioCommError), Ord(res));
end;

procedure TTestModBusTCP.ARequestBelowTheMinimumIsNotDecoded;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  //sem o pedido nao da' para saber o que foi perguntado; o menor que o driver
  //monta tem 8 bytes
  res:=FDrv.Decode(IOPacketFor(BytesOf('00 00 00'),
                               BytesOf('00 00 00 00 00 07 01 03 04 00 0A 00 14')), vals);
  AssertEquals('incomplete request', Ord(ioDriverError), Ord(res));
end;

initialization
  RegisterTest(TTestModBusTCP);
  RegisterTest(TTestModBusTCPOverAPort);
  RegisterTest(TTestModBusScan);

end.
