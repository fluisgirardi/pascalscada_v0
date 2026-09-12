{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TModBusRTUDriver: montagem e interpretacao dos frames.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os frames esperados sao literais conferidos contra a especificacao Modbus,
  com o CRC calculado fora daqui - se o teste conferisse o CRC com a propria
  Calcul_crc da biblioteca, um erro na tabela passaria despercebido nos dois
  lados.
}
{$ELSE}
{:
  @abstract(TModBusRTUDriver tests: frame building and parsing.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The expected frames are literals checked against the Modbus specification,
  with the CRC computed outside of here - if the test checked the CRC using
  the library's own Calcul_crc, a wrong table would go unnoticed on both
  sides.
}
{$ENDIF}
unit ut.modbusrtu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, modbusserial,
  testsupport.bytes, testsupport.protocol, testsupport.fakeport;

type

  {$IFDEF PORTUGUES}
  //: Expoe os metodos protegidos do driver, que e' onde mora o protocolo.
  {$ELSE}
  //: Exposes the driver's protected methods, where the protocol lives.
  {$ENDIF}

  { TModBusRTUProbe }

  TModBusRTUProbe = class(TModBusRTUDriver)
  public
    function Encode(aTag:TTagRec; aToWrite:TArrayOfDouble; var aResultLen:LongInt):BYTES;
    function Decode(aPkg:TIOPacket; out aValues:TArrayOfDouble):TProtocolIOResult;
    function Remaining(aBuffer:BYTES):LongInt;
    function ReadSync(const aTag:TTagRec; out aValues:TArrayOfDouble):TProtocolIOResult;
    function WriteSync(const aTag:TTagRec; const aValues:TArrayOfDouble):TProtocolIOResult;
  end;

  { TTestModBusRTU }

  TTestModBusRTU = class(TTestCase)
  private
    FDrv:TModBusRTUProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //montagem de pedidos de leitura / read request building
    procedure LeituraDeRegistradoresMontaOFrame;
    procedure LeituraDeRegistradoresPrediz9BytesDeResposta;
    procedure LeituraDeCoilsMontaOFrame;
    procedure LeituraDeCoilsArredondaOTamanhoDaRespostaParaCima;
    procedure FuncaoDeLeituraDesconhecidaNaoGeraFrame;

    //montagem de pedidos de escrita / write request building
    procedure EscritaDeRegistradorUnicoMontaOFrame;
    procedure EscritaDeCoilLigadoUsaFF00;
    procedure EscritaDeCoilDesligadoUsaZeros;
    procedure EscritaDeMultiplosRegistradoresMontaOFrame;

    //interpretacao das respostas / response parsing
    procedure RespostaDeRegistradoresViraValores;
    procedure RespostaDeCoilsViraBits;
    procedure RespostaComExcecaoViraOErroCorrespondente;
    procedure RespostaComCRCErradoViraErroDeComunicacao;
    procedure RespostaDeOutroEscravoViraErroDeComunicacao;
    procedure TimeoutNaLeituraViraTimeout;

    //tamanho do que falta no buffer / remaining buffer size
    procedure ExcecaoDeixaSoOCRCNoBuffer;

    //quadros malformados / malformed frames
    procedure RespostaMenorQueOMinimoEhRecusada;
    procedure PedidoVazioNaoDerrubaODriver;
    procedure ContagemDeBytesQueNaoBateComOPedidoEhRecusada;
    procedure RelatorioDeEscravoComContagemMenorQueQuatroNaoTransborda;
  end;

  {$IFDEF PORTUGUES}
  {:
  O mesmo driver, agora falando com uma porta de verdade (falsa): exercita o
  caminho inteiro - monta o frame, escreve na porta, le o cabecalho, calcula
  quanto falta, le o resto e decodifica.
  }
  {$ELSE}
  {:
  The same driver, now talking to a real (fake) port: exercises the whole path
  - builds the frame, writes it to the port, reads the header, computes what is
  missing, reads the rest and decodes it.
  }
  {$ENDIF}

{ TTestModBusRTUComPorta }

  TTestModBusRTUComPorta = class(TTestCase)
  private
    FPort:TFakeCommPort;
    FDrv:TModBusRTUProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LeituraVaiEVoltaPelaPorta;
    procedure LeituraEhFeitaEmDuasEtapas;
    procedure EscritaVaiEVoltaPelaPorta;
    procedure SemRespostaOResultadoEhTimeout;
    procedure SemPortaOResultadoEhNullDriver;
  end;

implementation

{ TModBusRTUProbe }

function TModBusRTUProbe.Encode(aTag:TTagRec; aToWrite:TArrayOfDouble; var aResultLen:LongInt):BYTES;
begin
  Result:=EncodePkg(aTag, aToWrite, aResultLen);
end;

function TModBusRTUProbe.Decode(aPkg:TIOPacket; out aValues:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DecodePkg(aPkg, aValues);
end;

function TModBusRTUProbe.Remaining(aBuffer:BYTES):LongInt;
begin
  Result:=RemainingBytes(aBuffer);
end;

function TModBusRTUProbe.ReadSync(const aTag:TTagRec; out aValues:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DoRead(aTag, aValues, true);
end;

function TModBusRTUProbe.WriteSync(const aTag:TTagRec; const aValues:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DoWrite(aTag, aValues, true);
end;

{ TTestModBusRTU }

procedure TTestModBusRTU.SetUp;
begin
  FDrv:=TModBusRTUProbe.Create(nil);
end;

procedure TTestModBusRTU.TearDown;
begin
  FreeAndNil(FDrv);
end;

procedure TTestModBusRTU.LeituraDeRegistradoresMontaOFrame;
var
  len:LongInt;
begin
  //escravo 1, ler 2 holding registers a partir de 0
  AssertBytesEqual('pedido de leitura 03',
                   BytesOf('01 03 00 00 00 02 C4 0B'),
                   FDrv.Encode(TagRecFor(1, $03, 0, 0, 2), nil, len));
end;

procedure TTestModBusRTU.LeituraDeRegistradoresPrediz9BytesDeResposta;
var
  len:LongInt;
begin
  //endereco + funcao + contagem + 2 words + crc = 9
  FDrv.Encode(TagRecFor(1, $03, 0, 0, 2), nil, len);
  AssertEquals('tamanho previsto da resposta', 9, len);
end;

procedure TTestModBusRTU.LeituraDeCoilsMontaOFrame;
var
  len:LongInt;
begin
  AssertBytesEqual('pedido de leitura 01',
                   BytesOf('01 01 00 00 00 0A BC 0D'),
                   FDrv.Encode(TagRecFor(1, $01, 0, 0, 10), nil, len));
end;

procedure TTestModBusRTU.LeituraDeCoilsArredondaOTamanhoDaRespostaParaCima;
var
  len:LongInt;
begin
  //10 coils nao cabem em 1 byte: 5 fixos + 2 bytes de dados
  FDrv.Encode(TagRecFor(1, $01, 0, 0, 10), nil, len);
  AssertEquals('10 coils', 7, len);

  //8 coils cabem exatamente em 1 byte, sem arredondar
  FDrv.Encode(TagRecFor(1, $01, 0, 0, 8), nil, len);
  AssertEquals('8 coils', 6, len);
end;

procedure TTestModBusRTU.FuncaoDeLeituraDesconhecidaNaoGeraFrame;
var
  len:LongInt;
  frame:BYTES;
begin
  frame:=FDrv.Encode(TagRecFor(1, $42, 0, 0, 2), nil, len);
  AssertEquals('frame vazio', 0, Length(frame));
  AssertEquals('nada a receber', 0, len);
end;

procedure TTestModBusRTU.EscritaDeRegistradorUnicoMontaOFrame;
var
  len:LongInt;
  vals:TArrayOfDouble;
begin
  SetLength(vals,1);
  vals[0]:=1234; //em hexa, 04 D2
  AssertBytesEqual('escrita 06',
                   BytesOf('01 06 00 0A 04 D2 2B 55'),
                   FDrv.Encode(TagRecFor(1, 0, $06, 10, 1), vals, len));
  AssertEquals('eco da escrita tem 8 bytes', 8, len);
end;

procedure TTestModBusRTU.EscritaDeCoilLigadoUsaFF00;
var
  len:LongInt;
  vals:TArrayOfDouble;
begin
  SetLength(vals,1);
  vals[0]:=1;
  AssertBytesEqual('escrita 05 ligado',
                   BytesOf('01 05 00 03 FF 00 7C 3A'),
                   FDrv.Encode(TagRecFor(1, 0, $05, 3, 1), vals, len));
end;

procedure TTestModBusRTU.EscritaDeCoilDesligadoUsaZeros;
var
  len:LongInt;
  vals:TArrayOfDouble;
begin
  SetLength(vals,1);
  vals[0]:=0;
  AssertBytesEqual('escrita 05 desligado',
                   BytesOf('01 05 00 03 00 00 3D CA'),
                   FDrv.Encode(TagRecFor(1, 0, $05, 3, 1), vals, len));
end;

procedure TTestModBusRTU.EscritaDeMultiplosRegistradoresMontaOFrame;
var
  len:LongInt;
  vals:TArrayOfDouble;
begin
  SetLength(vals,2);
  vals[0]:=10;
  vals[1]:=20;
  AssertBytesEqual('escrita 16',
                   BytesOf('01 10 00 00 00 02 04 00 0A 00 14 D3 A2'),
                   FDrv.Encode(TagRecFor(1, 0, $10, 0, 2), vals, len));
end;

procedure TTestModBusRTU.RespostaDeRegistradoresViraValores;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  res:=FDrv.Decode(IOPacketFor(BytesOf('01 03 00 00 00 02 C4 0B'),
                               BytesOf('01 03 04 00 0A 00 14 DA 3E')), vals);
  AssertEquals('resultado', Ord(ioOk), Ord(res));
  AssertEquals('quantidade de valores', 2, Length(vals));
  AssertEquals('primeiro registro', 10, vals[0], 0);
  AssertEquals('segundo registro',  20, vals[1], 0);
end;

procedure TTestModBusRTU.RespostaDeCoilsViraBits;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
  i:LongInt;
  esperado:array[0..9] of Double = (1,0,1,0,1,1,0,0, 0,1);
begin
  //byte $35 = 0011 0101 -> bits 0,2,4,5 ligados; byte $02 -> bit 9 ligado
  res:=FDrv.Decode(IOPacketFor(BytesOf('01 01 00 00 00 0A BC 0D'),
                               BytesOf('01 01 02 35 02 2F 6D')), vals);
  AssertEquals('resultado', Ord(ioOk), Ord(res));
  AssertEquals('quantidade de bits', 10, Length(vals));
  for i:=0 to High(esperado) do
    AssertEquals('bit '+IntToStr(i), esperado[i], vals[i], 0);
end;

procedure TTestModBusRTU.RespostaComExcecaoViraOErroCorrespondente;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  //resposta com a funcao 03 marcada com o bit $80 e codigo 02 = endereco ilegal
  res:=FDrv.Decode(IOPacketFor(BytesOf('01 03 00 00 00 02 C4 0B'),
                               BytesOf('01 83 02 C0 F1')), vals);
  AssertEquals('excecao 02', Ord(ioIllegalRegAddress), Ord(res));
end;

procedure TTestModBusRTU.RespostaComCRCErradoViraErroDeComunicacao;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  //mesmo frame do teste de registradores, com o ultimo byte do CRC trocado
  res:=FDrv.Decode(IOPacketFor(BytesOf('01 03 00 00 00 02 C4 0B'),
                               BytesOf('01 03 04 00 0A 00 14 DA 3F')), vals);
  AssertEquals('crc invalido', Ord(ioCommError), Ord(res));
end;

procedure TTestModBusRTU.RespostaDeOutroEscravoViraErroDeComunicacao;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  //pedimos ao escravo 1 e respondeu o escravo 2
  res:=FDrv.Decode(IOPacketFor(BytesOf('01 03 00 00 00 02 C4 0B'),
                               BytesOf('02 03 04 00 0A 00 14 E9 3E')), vals);
  AssertEquals('escravo errado', Ord(ioCommError), Ord(res));
end;

procedure TTestModBusRTU.TimeoutNaLeituraViraTimeout;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
  pkg:TIOPacket;
begin
  //a porta entrega o buffer do tamanho pedido, zerado, e avisa o timeout
  pkg:=IOPacketFor(BytesOf('01 03 00 00 00 02 C4 0B'),
                   BytesOf('00 00 00 00 00 00 00 00 00'));
  pkg.ReadIOResult:=iorTimeOut;
  pkg.Received:=0;

  res:=FDrv.Decode(pkg, vals);
  AssertEquals('timeout', Ord(ioTimeOut), Ord(res));
end;

procedure TTestModBusRTU.ExcecaoDeixaSoOCRCNoBuffer;
begin
  //lido o cabecalho de uma excecao, so faltam os 2 bytes de CRC
  AssertEquals('resto de uma excecao', 2, FDrv.Remaining(BytesOf('01 83 02')));
end;

{ TTestModBusRTUComPorta }

procedure TTestModBusRTU.RespostaMenorQueOMinimoEhRecusada;
var
  vals:TArrayOfDouble;
  resp:BYTES;
  n:LongInt;
begin
  //um quadro RTU tem no minimo escravo, funcao e os dois bytes de CRC
  for n:=0 to 3 do begin
    SetLength(resp, n);
    if n>0 then FillChar(resp[0], n, 0);

    AssertEquals('resposta de '+IntToStr(n)+' bytes', Ord(ioCommError),
                 Ord(FDrv.Decode(IOPacketFor(BytesOf('01 03 00 00 00 02 C4 0B'), resp), vals)));
  end;
end;

procedure TTestModBusRTU.PedidoVazioNaoDerrubaODriver;
var
  vals:TArrayOfDouble;
begin
  //sem pedido nao da' para saber o que foi perguntado, mas o driver tem que
  //dizer isso, nao estourar
  AssertEquals('pedido vazio', Ord(ioDriverError),
               Ord(FDrv.Decode(IOPacketFor(nil, BytesOf('01 03 04 00 0A 00 14 DA 3E')), vals)));
end;

procedure TTestModBusRTU.ContagemDeBytesQueNaoBateComOPedidoEhRecusada;
var
  vals:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  //pedimos 2 registradores (4 bytes de dado) e a resposta traz 1, com CRC valido
  res:=FDrv.Decode(IOPacketFor(BytesOf('01 03 00 00 00 02 C4 0B'),
                               BytesOf('01 03 02 00 0A 38 43')), vals);
  AssertEquals('contagem menor que a pedida', Ord(ioCommError), Ord(res));
end;

procedure TTestModBusRTU.RelatorioDeEscravoComContagemMenorQueQuatroNaoTransborda;
var
  vals:TArrayOfDouble;
begin
  //relatorio de escravo com contagem zero
  FDrv.Decode(IOPacketFor(BytesOf('01 11 C0 2C'), BytesOf('01 11 00 2C 50')), vals);
  AssertTrue('quantidade de valores tem que ser plausivel', Length(vals)<1024);
end;

procedure TTestModBusRTUComPorta.SetUp;
begin
  FPort:=TFakeCommPort.Create(nil);
  FPort.Active:=true;

  FDrv:=TModBusRTUProbe.Create(nil);
  FDrv.CommunicationPort:=FPort;
end;

procedure TTestModBusRTUComPorta.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPort);
end;

procedure TTestModBusRTUComPorta.LeituraVaiEVoltaPelaPorta;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  FPort.QueueResponse(BytesOf('01 03 04 00 0A 00 14 DA 3E'));

  res:=FDrv.ReadSync(TagRecFor(1, $03, 0, 0, 2), vals);

  AssertEquals('resultado', Ord(ioOk), Ord(res));
  AssertBytesEqual('frame que saiu pela porta',
                   BytesOf('01 03 00 00 00 02 C4 0B'),
                   FPort.LastWrittenFrame);
  AssertEquals('quantidade de valores', 2, Length(vals));
  AssertEquals('primeiro registro', 10, vals[0], 0);
  AssertEquals('segundo registro',  20, vals[1], 0);
  AssertEquals('resposta consumida por inteiro', 0, FPort.PendingResponses);
end;

procedure TTestModBusRTUComPorta.LeituraEhFeitaEmDuasEtapas;
var
  vals:TArrayOfDouble;
begin
  //o RTU le primeiro 3 bytes de cabecalho e so depois descobre, pela contagem
  //de bytes, quanto ainda falta - uma unica escrita, duas leituras.
  FPort.QueueResponse(BytesOf('01 03 04 00 0A 00 14 DA 3E'));
  FDrv.ReadSync(TagRecFor(1, $03, 0, 0, 2), vals);
  AssertEquals('escritas na porta', 1, FPort.WriteCount);
end;

procedure TTestModBusRTUComPorta.EscritaVaiEVoltaPelaPorta;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  SetLength(vals,1);
  vals[0]:=1234;

  //o escravo ecoa o pedido de escrita
  FPort.QueueResponse(BytesOf('01 06 00 0A 04 D2 2B 55'));

  res:=FDrv.WriteSync(TagRecFor(1, 0, $06, 10, 1), vals);

  AssertEquals('resultado', Ord(ioOk), Ord(res));
  AssertBytesEqual('frame que saiu pela porta',
                   BytesOf('01 06 00 0A 04 D2 2B 55'),
                   FPort.LastWrittenFrame);
end;

procedure TTestModBusRTUComPorta.SemRespostaOResultadoEhTimeout;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
begin
  //nada enfileirado: o equipamento nao respondeu
  res:=FDrv.ReadSync(TagRecFor(1, $03, 0, 0, 2), vals);
  AssertEquals('timeout', Ord(ioTimeOut), Ord(res));
end;

procedure TTestModBusRTUComPorta.SemPortaOResultadoEhNullDriver;
var
  res:TProtocolIOResult;
  vals:TArrayOfDouble;
  semPorta:TModBusRTUProbe;
begin
  semPorta:=TModBusRTUProbe.Create(nil);
  try
    res:=semPorta.ReadSync(TagRecFor(1, $03, 0, 0, 2), vals);
    AssertEquals('driver sem porta', Ord(ioNullDriver), Ord(res));
  finally
    semPorta.Free;
  end;
end;

initialization
  RegisterTest(TTestModBusRTU);
  RegisterTest(TTestModBusRTUComPorta);

end.
