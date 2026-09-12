{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TIBoxDriver: validacao do endereco e o quadro de
            leitura com soma de verificacao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O driver so aceita um punhado de enderecos (os PIDs que o equipamento
  expoe), e recusa o resto antes mesmo de falar com a porta - por isso boa
  parte dos testes nao precisa de porta nenhuma. O que vai para o fio e' um
  quadro de quatro bytes terminado por uma soma de verificacao em complemento
  de dois, conferida tambem na resposta.
}
{$ELSE}
{:
  @abstract(TIBoxDriver tests: address validation and the read frame with its
            checksum.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The driver only accepts a handful of addresses (the PIDs the device
  exposes), refusing the rest before even talking to the port - which is why
  most of these tests need no port at all. What goes on the wire is a four
  byte frame ending in a two's complement checksum, also checked on the way
  back.
}
{$ENDIF}
unit ut.iboxdriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, iboxdriver, PLCTagNumber,
  testsupport.bytes, testsupport.protocol, testsupport.fakeport;

type

  { TIBoxProbe }

  TIBoxProbe = class(TIBoxDriver)
  public
    function  Ler(const aTag:TTagRec; out aValores:TArrayOfDouble):TProtocolIOResult;
    procedure RemoverTag(aTag:TTag);
  end;

  { TTestIBoxDriver }

  TTestIBoxDriver = class(TTestCase)
  private
    FPorta:TFakeCommPort;
    FDrv:TIBoxProbe;
    function  PedidoPara(aEstacao, aEndereco:LongInt):TTagRec;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //validacao antes de falar com a porta / validation before touching the port
    procedure EstacaoForaDaFaixaEhRecusada;
    procedure EnderecoNaoSuportadoEhRecusado;
    procedure SubelementoForaDaFaixaEhRecusado;
    procedure SemPortaDeComunicacaoNaoTentaFalar;

    //quadro enviado / frame sent
    procedure PedidoDeNivelDeCombustivelMontaOQuadro;
    procedure SomaDeVerificacaoEhOComplementoDeDois;

    //resposta / response
    procedure NivelDeCombustivelVemEmMeioPorCento;
    procedure RespostaComSomaErradaEhRecusada;
    procedure RespostaDeOutraEstacaoEhRecusada;

    //remocao de tag / tag removal
    procedure RemoverTagDeEstacaoDesconhecidaNaoFazNada;
  end;

implementation

{ TIBoxProbe }

function TIBoxProbe.Ler(const aTag:TTagRec; out aValores:TArrayOfDouble):TProtocolIOResult;
begin
  Result:=DoRead(aTag, aValores, true);
end;

procedure TIBoxProbe.RemoverTag(aTag:TTag);
begin
  DoDelTag(aTag);
end;

{ TTestIBoxDriver }

procedure TTestIBoxDriver.SetUp;
begin
  FPorta:=TFakeCommPort.Create(nil);
  FPorta.Active:=true;

  FDrv:=TIBoxProbe.Create(nil);
  FDrv.CommunicationPort:=FPorta;
end;

procedure TTestIBoxDriver.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPorta);
end;

function TTestIBoxDriver.PedidoPara(aEstacao, aEndereco:LongInt):TTagRec;
begin
  Result:=TagRecFor(aEstacao, 0, 0, aEndereco, 1);
end;

procedure TTestIBoxDriver.EstacaoForaDaFaixaEhRecusada;
var
  valores:TArrayOfDouble;
begin
  //endereco de estacao cabe num byte
  AssertEquals('estacao 300', Ord(ioIllegalStationAddress),
               Ord(FDrv.Ler(PedidoPara(300, 96), valores)));
  AssertEquals('nada foi enviado', 0, FPorta.WriteCount);
end;

procedure TTestIBoxDriver.EnderecoNaoSuportadoEhRecusado;
var
  valores:TArrayOfDouble;
begin
  //so os PIDs conhecidos sao aceitos; o resto nem chega na porta
  AssertEquals('endereco 5', Ord(ioIllegalRegAddress),
               Ord(FDrv.Ler(PedidoPara(1, 5), valores)));
  AssertEquals('nada foi enviado', 0, FPorta.WriteCount);
end;

procedure TTestIBoxDriver.SubelementoForaDaFaixaEhRecusado;
var
  valores:TArrayOfDouble;
  pedido:TTagRec;
begin
  //nos registradores 200 a 202 o subelemento escolhe o campo, de 0 a 16
  pedido:=PedidoPara(1, 200);
  pedido.SubElement:=20;

  AssertEquals('subelemento 20', Ord(ioIllegalRegAddress),
               Ord(FDrv.Ler(pedido, valores)));
end;

procedure TTestIBoxDriver.SemPortaDeComunicacaoNaoTentaFalar;
var
  valores:TArrayOfDouble;
  semPorta:TIBoxProbe;
begin
  semPorta:=TIBoxProbe.Create(nil);
  try
    AssertEquals('driver sem porta', Ord(ioNullDriver),
                 Ord(semPorta.Ler(PedidoPara(1, 96), valores)));
  finally
    semPorta.Free;
  end;
end;

procedure TTestIBoxDriver.PedidoDeNivelDeCombustivelMontaOQuadro;
var
  valores:TArrayOfDouble;
begin
  //estacao, zero, o PID pedido e a soma de verificacao
  FPorta.QueueResponse(BytesOf('01 60 64 3B'));
  FDrv.Ler(PedidoPara(1, 96), valores);

  AssertEquals('uma escrita', 1, FPorta.WriteCount);
  AssertBytesEqual('pedido do PID 96', BytesOf('01 00 60 9F'), FPorta.WrittenFrame(0));
end;

procedure TTestIBoxDriver.SomaDeVerificacaoEhOComplementoDeDois;
var
  valores:TArrayOfDouble;
begin
  //mudar a estacao muda a soma na mesma medida: $9F para a estacao 1,
  //$9E para a 2
  FPorta.QueueResponse(BytesOf('02 60 64 3A'));
  FDrv.Ler(PedidoPara(2, 96), valores);
  AssertBytesEqual('estacao 2', BytesOf('02 00 60 9E'), FPorta.WrittenFrame(0));

  FPorta.Reset;
  FPorta.QueueResponse(BytesOf('01 A8 00 57'));
  FDrv.Ler(PedidoPara(1, 168), valores);
  AssertBytesEqual('PID 168', BytesOf('01 00 A8 57'), FPorta.WrittenFrame(0));
end;

procedure TTestIBoxDriver.NivelDeCombustivelVemEmMeioPorCento;
var
  valores:TArrayOfDouble;
begin
  //o byte de dado vale meio por cento por unidade: $64 = 100 = 50%
  FPorta.QueueResponse(BytesOf('01 60 64 3B'));

  AssertEquals('leitura aceita', Ord(ioOk), Ord(FDrv.Ler(PedidoPara(1, 96), valores)));
  AssertEquals('nivel em porcento', 50, valores[0], 0);
end;

procedure TTestIBoxDriver.RespostaComSomaErradaEhRecusada;
var
  valores:TArrayOfDouble;
begin
  //mesma resposta, com o ultimo byte trocado
  FPorta.QueueResponse(BytesOf('01 60 64 3C'));

  AssertEquals('soma invalida', Ord(ioCommError),
               Ord(FDrv.Ler(PedidoPara(1, 96), valores)));
end;

procedure TTestIBoxDriver.RespostaDeOutraEstacaoEhRecusada;
var
  valores:TArrayOfDouble;
begin
  //perguntamos a estacao 1 e respondeu a 2, com soma valida
  FPorta.QueueResponse(BytesOf('02 60 64 3A'));

  AssertEquals('estacao errada', Ord(ioCommError),
               Ord(FDrv.Ler(PedidoPara(1, 96), valores)));
end;

procedure TTestIBoxDriver.RemoverTagDeEstacaoDesconhecidaNaoFazNada;
var
  tag:TPLCTagNumber;
begin
  //nenhum tag foi cadastrado, entao a lista de estacoes esta' vazia. Remover
  //um tag daqui nao pode indexar essa lista
  tag:=TPLCTagNumber.Create(nil);
  try
    tag.PLCStation:=7;
    tag.MemAddress:=96;

    FDrv.RemoverTag(tag);
  finally
    tag.Free;
  end;
end;

initialization
  RegisterTest(TTestIBoxDriver);

end.
