{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TWestASCIIDriver: o frame de presenca dos controladores
            West, indo e voltando por uma porta falsa.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O protocolo e' em ASCII legivel: "L", o endereco em dois digitos, e os
  caracteres de comando. O que da' para exercitar de fora da unit e' o
  DeviceActive, que e' publico e faz o ciclo completo - monta o frame, escreve
  na porta, le a resposta e decide. O resto do protocolo esta em metodos
  privados, fora do alcance de qualquer teste.
}
{$ELSE}
{:
  @abstract(TWestASCIIDriver tests: the West controllers' presence frame, going
            out and back through a fake port.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The protocol is readable ASCII: "L", the two digit address, and the command
  characters. What can be exercised from outside the unit is DeviceActive,
  which is public and does the whole round trip - builds the frame, writes it
  to the port, reads the answer and decides. The rest of the protocol lives in
  private methods, out of reach of any test.
}
{$ENDIF}
unit ut.westasciidriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, westasciidriver,
  testsupport.bytes, testsupport.fakeport;

type

  { TTestWestASCIIDriver }

  TTestWestASCIIDriver = class(TTestCase)
  private
    FPorta:TFakeCommPort;
    FDrv:TWestASCIIDriver;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //montagem do pedido / request building
    procedure PedidoDePresencaMontaOFrame;
    procedure EnderecoVaiSempreComDoisDigitos;

    //interpretacao da resposta / response parsing
    procedure RespostaPositivaConfirmaOEquipamento;
    procedure RespostaNoFormatoCurtoTambemEhAceita;
    procedure RespostaDeOutroEnderecoEhRecusada;
    procedure RespostaComLixoEhRecusada;
    procedure SemRespostaViraTimeout;

    //sem porta / with no port
    procedure SemPortaDeComunicacaoNaoTentaFalar;
  end;

implementation

procedure TTestWestASCIIDriver.SetUp;
begin
  FPorta:=TFakeCommPort.Create(nil);
  FPorta.Active:=true;

  FDrv:=TWestASCIIDriver.Create(nil);
  FDrv.CommunicationPort:=FPorta;
end;

procedure TTestWestASCIIDriver.TearDown;
begin
  FreeAndNil(FDrv);
  FreeAndNil(FPorta);
end;

procedure TTestWestASCIIDriver.PedidoDePresencaMontaOFrame;
begin
  //"L" (4C), endereco 01, "??" e "*" de fim
  FPorta.QueueResponse(BytesOf('4C 30 31 3F 41 2A'));
  FDrv.DeviceActive(1);

  AssertEquals('one write to the port', 1, FPorta.WriteCount);
  AssertBytesEqual('presence frame',
                   BytesOf('4C 30 31 3F 3F 2A'), FPorta.WrittenFrame(0));
end;

procedure TTestWestASCIIDriver.EnderecoVaiSempreComDoisDigitos;
begin
  //42 vira "42"; enderecos de um digito levam zero a esquerda
  FPorta.QueueResponse(BytesOf('4C 34 32 3F 41 2A'));
  FDrv.DeviceActive(42);
  AssertBytesEqual('address 42', BytesOf('4C 34 32 3F 3F 2A'), FPorta.WrittenFrame(0));

  FPorta.Reset;
  FPorta.QueueResponse(BytesOf('4C 30 39 3F 41 2A'));
  FDrv.DeviceActive(9);
  AssertBytesEqual('address 9', BytesOf('4C 30 39 3F 3F 2A'), FPorta.WrittenFrame(0));
end;

procedure TTestWestASCIIDriver.RespostaPositivaConfirmaOEquipamento;
begin
  //o equipamento devolve o mesmo endereco com "A" no lugar do segundo "?"
  FPorta.QueueResponse(BytesOf('4C 30 31 3F 41 2A'));
  AssertEquals('device present', Ord(ioOk), Ord(FDrv.DeviceActive(1)));
end;

procedure TTestWestASCIIDriver.RespostaNoFormatoCurtoTambemEhAceita;
begin
  //ha equipamentos que respondem so com o digito das unidades; o driver
  //aceita as duas formas
  FPorta.QueueResponse(BytesOf('4C 31 3F 41 2A 00'));
  AssertEquals('short format', Ord(ioOk), Ord(FDrv.DeviceActive(1)));
end;

procedure TTestWestASCIIDriver.RespostaDeOutroEnderecoEhRecusada;
begin
  //perguntamos ao 1 e respondeu o 2
  FPorta.QueueResponse(BytesOf('4C 30 32 3F 41 2A'));
  AssertEquals('wrong address', Ord(ioCommError), Ord(FDrv.DeviceActive(1)));
end;

procedure TTestWestASCIIDriver.RespostaComLixoEhRecusada;
begin
  FPorta.QueueResponse(BytesOf('FF FF FF FF FF FF'));
  AssertEquals('answer that makes no sense', Ord(ioCommError), Ord(FDrv.DeviceActive(1)));
end;

procedure TTestWestASCIIDriver.SemRespostaViraTimeout;
begin
  //nada enfileirado: o equipamento nao respondeu
  AssertEquals('timeout', Ord(ioTimeOut), Ord(FDrv.DeviceActive(1)));
  AssertEquals('but the request did get sent', 1, FPorta.WriteCount);
end;

procedure TTestWestASCIIDriver.SemPortaDeComunicacaoNaoTentaFalar;
var
  semPorta:TWestASCIIDriver;
begin
  semPorta:=TWestASCIIDriver.Create(nil);
  try
    AssertEquals('driver with no port', Ord(ioNullDriver), Ord(semPorta.DeviceActive(1)));
  finally
    semPorta.Free;
  end;
end;

initialization
  RegisterTest(TTestWestASCIIDriver);

end.
