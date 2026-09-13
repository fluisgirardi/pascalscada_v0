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
    procedure Ranges(aPlcMin, aPlcMax, aSysMin, aSysMax:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FactoryDefaults;

    //interpolacao / interpolation
    procedure TheEndsOfTheRangeMatchTheEndsOfTheScale;
    procedure TheMiddleOfTheRangeGivesHalfTheScale;
    procedure ARangeWithAnOffsetFromFourToTwenty;
    procedure AnInvertedDeviceRange;
    procedure AValueOutsideTheRangeIsExtrapolated;

    //ida e volta / round trip
    procedure TheWayBackUndoesTheWayThere;
    procedure TheWayBackUndoesTheWayThereOnAnInvertedRange;

    //faixas degeneradas / degenerate ranges
    procedure ADeviceRangeWithNoSpanDoesNotDivideByZero;
    procedure AnEngineeringRangeWithNoSpanDoesNotDivideByZero;
  end;

  { TTestEscalaDoUsuario }

  TTestEscalaDoUsuario = class(TTestCase)
  private
    FEscala:TUserScale;
    FChamadasDeIda, FChamadasDeVolta:LongInt;
    FEntradaVista, FResultadoQueChegou:Double;
    procedure FromTheDeviceToTheUser(Sender:TObject; const Entrada:Double; var Saida:Double);
    procedure FromTheUserToTheDevice(Sender:TObject; const Entrada:Double; var Saida:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WithNoHookTheValueGoesThroughUntouched;
    procedure TheHookOnTheWayThereIsCalled;
    procedure TheHookOnTheWayBackIsCalled;
    procedure TheHookGetsTheInputValue;
    procedure TheResultReachesTheHookAlreadyHoldingTheInputValue;
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

procedure TTestEscalaLinear.Ranges(aPlcMin, aPlcMax, aSysMin, aSysMax:Double);
begin
  FEscala.PLCMin:=aPlcMin;
  FEscala.PLCMax:=aPlcMax;
  FEscala.SysMin:=aSysMin;
  FEscala.SysMax:=aSysMax;
end;

procedure TTestEscalaLinear.FactoryDefaults;
begin
  //a faixa de fabrica e' a de um conversor de 15 bits para 0 a 100 por cento
  AssertEquals('device minimum', 0,     FEscala.PLCMin, 0);
  AssertEquals('device maximum', 32000, FEscala.PLCMax, 0);
  AssertEquals('engineering minimum',  0,     FEscala.SysMin, 0);
  AssertEquals('engineering maximum',  100,   FEscala.SysMax, 0);
end;

procedure TTestEscalaLinear.TheEndsOfTheRangeMatchTheEndsOfTheScale;
begin
  AssertEquals('bottom of the scale',  0,   FEscala.SetInGetOut(nil, 0),     0.0001);
  AssertEquals('top of the scale',   100, FEscala.SetInGetOut(nil, 32000), 0.0001);
end;

procedure TTestEscalaLinear.TheMiddleOfTheRangeGivesHalfTheScale;
begin
  AssertEquals('middle of the range', 50, FEscala.SetInGetOut(nil, 16000), 0.0001);
end;

procedure TTestEscalaLinear.ARangeWithAnOffsetFromFourToTwenty;
begin
  //a faixa classica de corrente: 4 mA e' zero por cento, 20 mA e' cem
  Ranges(4, 20, 0, 100);

  AssertEquals('four milliamps',  0,   FEscala.SetInGetOut(nil, 4),  0.0001);
  AssertEquals('twelve milliamps',    50,  FEscala.SetInGetOut(nil, 12), 0.0001);
  AssertEquals('twenty milliamps',   100, FEscala.SetInGetOut(nil, 20), 0.0001);
end;

procedure TTestEscalaLinear.AnInvertedDeviceRange;
begin
  //ha' equipamento que entrega a faixa ao contrario
  Ranges(32000, 0, 0, 100);

  AssertEquals('the device maximum is zero', 0,   FEscala.SetInGetOut(nil, 32000), 0.0001);
  AssertEquals('the minimum is one hundred',                 100, FEscala.SetInGetOut(nil, 0),     0.0001);
end;

procedure TTestEscalaLinear.AValueOutsideTheRangeIsExtrapolated;
begin
  //a escala nao corta o valor na faixa: ela prolonga a reta. Quem precisar de
  //corte usa os limites do tag
  AssertEquals('above the top',   200, FEscala.SetInGetOut(nil, 64000), 0.0001);
  AssertEquals('below the bottom', -50, FEscala.SetInGetOut(nil, -16000), 0.0001);
end;

procedure TTestEscalaLinear.TheWayBackUndoesTheWayThere;
begin
  Ranges(4, 20, -40, 120);

  AssertEquals('round trip at 7',  7,  FEscala.SetOutGetIn(nil, FEscala.SetInGetOut(nil, 7)),  0.0001);
  AssertEquals('round trip at 13', 13, FEscala.SetOutGetIn(nil, FEscala.SetInGetOut(nil, 13)), 0.0001);
end;

procedure TTestEscalaLinear.TheWayBackUndoesTheWayThereOnAnInvertedRange;
begin
  Ranges(32000, 0, 0, 100);
  AssertEquals('round trip', 12345, FEscala.SetOutGetIn(nil, FEscala.SetInGetOut(nil, 12345)), 0.0001);
end;

procedure TTestEscalaLinear.ADeviceRangeWithNoSpanDoesNotDivideByZero;
begin
  //faixa mal configurada nao pode derrubar a leitura
  Ranges(100, 100, 0, 100);

  AssertEquals('on the point itself', 0, FEscala.SetInGetOut(nil, 100), 0.0001);
end;

procedure TTestEscalaLinear.AnEngineeringRangeWithNoSpanDoesNotDivideByZero;
begin
  Ranges(0, 32000, 50, 50);

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

procedure TTestEscalaDoUsuario.FromTheDeviceToTheUser(Sender:TObject; const Entrada:Double; var Saida:Double);
begin
  inc(FChamadasDeIda);
  FEntradaVista:=Entrada;
  FResultadoQueChegou:=Saida;
  Saida:=Entrada*10;
end;

procedure TTestEscalaDoUsuario.FromTheUserToTheDevice(Sender:TObject; const Entrada:Double; var Saida:Double);
begin
  inc(FChamadasDeVolta);
  Saida:=Entrada/10;
end;

procedure TTestEscalaDoUsuario.WithNoHookTheValueGoesThroughUntouched;
begin
  AssertEquals('there',   7, FEscala.SetInGetOut(nil, 7), 0);
  AssertEquals('back', 7, FEscala.SetOutGetIn(nil, 7), 0);
end;

procedure TTestEscalaDoUsuario.TheHookOnTheWayThereIsCalled;
begin
  FEscala.OnPLCToUser:=@FromTheDeviceToTheUser;

  AssertEquals('the hook did the maths', 50, FEscala.SetInGetOut(nil, 5), 0);
  AssertEquals('and was called once', 1, FChamadasDeIda);
  AssertEquals('without touching the other one',    0, FChamadasDeVolta);
end;

procedure TTestEscalaDoUsuario.TheHookOnTheWayBackIsCalled;
begin
  FEscala.OnUserToPLC:=@FromTheUserToTheDevice;

  AssertEquals('the hook did the maths', 5, FEscala.SetOutGetIn(nil, 50), 0);
  AssertEquals('and was called once', 1, FChamadasDeVolta);
  AssertEquals('without touching the other one',    0, FChamadasDeIda);
end;

procedure TTestEscalaDoUsuario.TheHookGetsTheInputValue;
begin
  FEscala.OnPLCToUser:=@FromTheDeviceToTheUser;
  FEscala.SetInGetOut(nil, 42);

  AssertEquals('the hook saw the input', 42, FEntradaVista, 0);
end;

procedure TTestEscalaDoUsuario.TheResultReachesTheHookAlreadyHoldingTheInputValue;
begin
  //um gancho que nao mexe na saida deixa o valor passar: e' o que garante isso
  FEscala.OnPLCToUser:=@FromTheDeviceToTheUser;
  FEscala.SetInGetOut(nil, 42);

  AssertEquals('the output arrives already equal to the input', 42, FResultadoQueChegou, 0);
end;

initialization
  RegisterTest(TTestEscalaLinear);
  RegisterTest(TTestEscalaDoUsuario);

end.
