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
  commtypes, Tag, modbustcp,
  testsupport.bytes, testsupport.protocol;

type

  { TModBusTCPProbe }

  TModBusTCPProbe = class(TModBusTCPDriver)
  public
    function Encode(aTag:TTagRec; aToWrite:TArrayOfDouble; var aResultLen:LongInt):BYTES;
    function Decode(aPkg:TIOPacket; out aValues:TArrayOfDouble):TProtocolIOResult;
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

end.
