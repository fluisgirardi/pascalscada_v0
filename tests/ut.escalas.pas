{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes das escalas: a linear e a do usuario.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Uma escala faz a ponte entre o numero que o equipamento entrega e o numero
  que o operador ve. A linear e' a de dois pontos: diz-se a faixa do
  equipamento e a faixa de engenharia, e ela interpola entre as duas. A do
  usuario nao calcula nada - chama dois ganchos e deixa a conta com quem
  escreveu o projeto.

  O que precisa valer nas duas e' que o caminho de volta desfaca o de ida:
  sem isso, o valor que o operador escreve nao e' o que chega no equipamento.
}
{$ELSE}
{:
  @abstract(Scale tests: the linear one and the user one.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A scale bridges the number the device hands over and the number the operator
  sees. The linear one is the two point kind: the device range and the
  engineering range are given, and it interpolates between them. The user one
  computes nothing - it calls two hooks and leaves the arithmetic to whoever
  wrote the project.

  What must hold in both is that the return trip undoes the outbound one:
  without that, what the operator writes is not what reaches the device.
}
{$ENDIF}
unit ut.escalas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  LinearScaleProcessor, UserScale;

type

  { TTestEscalaLinear }

  TTestEscalaLinear = class(TTestCase)
  private
    FEscala:TLinearScaleProcessor;
    //: configura as duas faixas de uma vez
    procedure Faixas(aPlcMin, aPlcMax, aSysMin, aSysMax:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PadroesDeFabrica;

    //interpolacao / interpolation
    procedure ExtremosDaFaixaBatemComOsExtremosDaEscala;
    procedure MeioDaFaixaDaMetadeDaEscala;
    procedure FaixaComDeslocamentoDeQuatroAVinte;
    procedure FaixaDoEquipamentoInvertida;
    procedure ValorForaDaFaixaEhExtrapolado;

    //ida e volta / round trip
    procedure OCaminhoDeVoltaDesfazODeIda;
    procedure OCaminhoDeVoltaDesfazODeIdaComFaixaInvertida;

    //faixas degeneradas / degenerate ranges
    procedure FaixaDoEquipamentoSemAmplitudeNaoDividePorZero;
    procedure FaixaDeEngenhariaSemAmplitudeNaoDividePorZero;
  end;

  { TTestEscalaDoUsuario }

  TTestEscalaDoUsuario = class(TTestCase)
  private
    FEscala:TUserScale;
    FChamadasDeIda, FChamadasDeVolta:LongInt;
    FEntradaVista, FResultadoQueChegou:Double;
    procedure DoEquipamentoParaOUsuario(Sender:TObject; const Entrada:Double; var Saida:Double);
    procedure DoUsuarioParaOEquipamento(Sender:TObject; const Entrada:Double; var Saida:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SemGanchoOValorPassaSemMexer;
    procedure OGanchoDeIdaEhChamado;
    procedure OGanchoDeVoltaEhChamado;
    procedure OGanchoRecebeOValorDeEntrada;
    procedure OResultadoChegaAoGanchoJaComOValorDeEntrada;
  end;

implementation

{ TTestEscalaLinear }

procedure TTestEscalaLinear.SetUp;
begin
  FEscala:=TLinearScaleProcessor.Create(nil);
end;

procedure TTestEscalaLinear.TearDown;
begin
  FreeAndNil(FEscala);
end;

procedure TTestEscalaLinear.Faixas(aPlcMin, aPlcMax, aSysMin, aSysMax:Double);
begin
  FEscala.PLCMin:=aPlcMin;
  FEscala.PLCMax:=aPlcMax;
  FEscala.SysMin:=aSysMin;
  FEscala.SysMax:=aSysMax;
end;

procedure TTestEscalaLinear.PadroesDeFabrica;
begin
  //a faixa de fabrica e' a de um conversor de 15 bits para 0 a 100 por cento
  AssertEquals('device minimum', 0,     FEscala.PLCMin, 0);
  AssertEquals('device maximum', 32000, FEscala.PLCMax, 0);
  AssertEquals('engineering minimum',  0,     FEscala.SysMin, 0);
  AssertEquals('engineering maximum',  100,   FEscala.SysMax, 0);
end;

procedure TTestEscalaLinear.ExtremosDaFaixaBatemComOsExtremosDaEscala;
begin
  AssertEquals('bottom of the scale',  0,   FEscala.SetInGetOut(nil, 0),     0.0001);
  AssertEquals('top of the scale',   100, FEscala.SetInGetOut(nil, 32000), 0.0001);
end;

procedure TTestEscalaLinear.MeioDaFaixaDaMetadeDaEscala;
begin
  AssertEquals('middle of the range', 50, FEscala.SetInGetOut(nil, 16000), 0.0001);
end;

procedure TTestEscalaLinear.FaixaComDeslocamentoDeQuatroAVinte;
begin
  //a faixa classica de corrente: 4 mA e' zero por cento, 20 mA e' cem
  Faixas(4, 20, 0, 100);

  AssertEquals('four milliamps',  0,   FEscala.SetInGetOut(nil, 4),  0.0001);
  AssertEquals('twelve milliamps',    50,  FEscala.SetInGetOut(nil, 12), 0.0001);
  AssertEquals('twenty milliamps',   100, FEscala.SetInGetOut(nil, 20), 0.0001);
end;

procedure TTestEscalaLinear.FaixaDoEquipamentoInvertida;
begin
  //ha' equipamento que entrega a faixa ao contrario
  Faixas(32000, 0, 0, 100);

  AssertEquals('the device maximum is zero', 0,   FEscala.SetInGetOut(nil, 32000), 0.0001);
  AssertEquals('the minimum is one hundred',                 100, FEscala.SetInGetOut(nil, 0),     0.0001);
end;

procedure TTestEscalaLinear.ValorForaDaFaixaEhExtrapolado;
begin
  //a escala nao corta o valor na faixa: ela prolonga a reta. Quem precisar de
  //corte usa os limites do tag
  AssertEquals('above the top',   200, FEscala.SetInGetOut(nil, 64000), 0.0001);
  AssertEquals('below the bottom', -50, FEscala.SetInGetOut(nil, -16000), 0.0001);
end;

procedure TTestEscalaLinear.OCaminhoDeVoltaDesfazODeIda;
begin
  Faixas(4, 20, -40, 120);

  AssertEquals('round trip at 7',  7,  FEscala.SetOutGetIn(nil, FEscala.SetInGetOut(nil, 7)),  0.0001);
  AssertEquals('round trip at 13', 13, FEscala.SetOutGetIn(nil, FEscala.SetInGetOut(nil, 13)), 0.0001);
end;

procedure TTestEscalaLinear.OCaminhoDeVoltaDesfazODeIdaComFaixaInvertida;
begin
  Faixas(32000, 0, 0, 100);
  AssertEquals('round trip', 12345, FEscala.SetOutGetIn(nil, FEscala.SetInGetOut(nil, 12345)), 0.0001);
end;

procedure TTestEscalaLinear.FaixaDoEquipamentoSemAmplitudeNaoDividePorZero;
begin
  //faixa mal configurada nao pode derrubar a leitura
  Faixas(100, 100, 0, 100);

  AssertEquals('on the point itself', 0, FEscala.SetInGetOut(nil, 100), 0.0001);
end;

procedure TTestEscalaLinear.FaixaDeEngenhariaSemAmplitudeNaoDividePorZero;
begin
  Faixas(0, 32000, 50, 50);

  AssertEquals('on the point itself', 0, FEscala.SetOutGetIn(nil, 50), 0.0001);
end;

{ TTestEscalaDoUsuario }

procedure TTestEscalaDoUsuario.SetUp;
begin
  FEscala:=TUserScale.Create(nil);
  FChamadasDeIda:=0;
  FChamadasDeVolta:=0;
  FEntradaVista:=0;
  FResultadoQueChegou:=0;
end;

procedure TTestEscalaDoUsuario.TearDown;
begin
  FreeAndNil(FEscala);
end;

procedure TTestEscalaDoUsuario.DoEquipamentoParaOUsuario(Sender:TObject; const Entrada:Double; var Saida:Double);
begin
  inc(FChamadasDeIda);
  FEntradaVista:=Entrada;
  FResultadoQueChegou:=Saida;
  Saida:=Entrada*10;
end;

procedure TTestEscalaDoUsuario.DoUsuarioParaOEquipamento(Sender:TObject; const Entrada:Double; var Saida:Double);
begin
  inc(FChamadasDeVolta);
  Saida:=Entrada/10;
end;

procedure TTestEscalaDoUsuario.SemGanchoOValorPassaSemMexer;
begin
  AssertEquals('there',   7, FEscala.SetInGetOut(nil, 7), 0);
  AssertEquals('back', 7, FEscala.SetOutGetIn(nil, 7), 0);
end;

procedure TTestEscalaDoUsuario.OGanchoDeIdaEhChamado;
begin
  FEscala.OnPLCToUser:=@DoEquipamentoParaOUsuario;

  AssertEquals('the hook did the maths', 50, FEscala.SetInGetOut(nil, 5), 0);
  AssertEquals('and was called once', 1, FChamadasDeIda);
  AssertEquals('without touching the other one',    0, FChamadasDeVolta);
end;

procedure TTestEscalaDoUsuario.OGanchoDeVoltaEhChamado;
begin
  FEscala.OnUserToPLC:=@DoUsuarioParaOEquipamento;

  AssertEquals('the hook did the maths', 5, FEscala.SetOutGetIn(nil, 50), 0);
  AssertEquals('and was called once', 1, FChamadasDeVolta);
  AssertEquals('without touching the other one',    0, FChamadasDeIda);
end;

procedure TTestEscalaDoUsuario.OGanchoRecebeOValorDeEntrada;
begin
  FEscala.OnPLCToUser:=@DoEquipamentoParaOUsuario;
  FEscala.SetInGetOut(nil, 42);

  AssertEquals('the hook saw the input', 42, FEntradaVista, 0);
end;

procedure TTestEscalaDoUsuario.OResultadoChegaAoGanchoJaComOValorDeEntrada;
begin
  //um gancho que nao mexe na saida deixa o valor passar: e' o que garante isso
  FEscala.OnPLCToUser:=@DoEquipamentoParaOUsuario;
  FEscala.SetInGetOut(nil, 42);

  AssertEquals('the output arrives already equal to the input', 42, FResultadoQueChegou, 0);
end;

initialization
  RegisterTest(TTestEscalaLinear);
  RegisterTest(TTestEscalaDoUsuario);

end.
