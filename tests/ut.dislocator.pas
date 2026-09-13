{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da animacao de deslocamento:
            THMIControlDislocatorAnimation e a variante de dois tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' o componente que faz um desenho andar pela tela conforme o valor de um
  tag - a caneca subindo o elevador, o carro da ponte rolante correndo o vao.
  Por dentro e' uma escala linear por eixo: a faixa de valores do tag vira a
  faixa de pixels entre dois pontos, com limites opcionais para o desenho nao
  sair da area.

  Tudo aqui e' conta, e conta que da' para conferir na mao. O que precisa de
  tela e' so o redesenho; posicionar e' aritmetica, e e' isso que se testa.
}
{$ELSE}
{:
  @abstract(Dislocator animation tests: THMIControlDislocatorAnimation and the
            two tag variant.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This is the component that walks a drawing across the screen following a
  tag's value - the bucket climbing the elevator, the crane trolley running
  the span. Underneath it is a linear scale per axis: the tag's value range
  becomes the pixel range between two points, with optional limits so the
  drawing does not leave its area.

  All of it is arithmetic, and arithmetic one can check by hand. What needs a
  screen is only the repaint; positioning is maths, and that is what is
  tested here.
}
{$ENDIF}
unit ut.dislocator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, fpcunit, testregistry,
  HMIControlDislocatorAnimation,
  testsupport.faketag;

type

  { TDislocatorProbe }

  //MoveObject e' protegido e em producao roda pela fila assincrona da
  //aplicacao, que num programa de console nunca e' processada. A sonda o
  //chama direto, que e' o que os setters de propriedade ja fazem.
  //
  //MoveObject is protected and in production runs through the application's
  //async queue, which a console program never processes. The probe calls it
  //directly, which is what the property setters already do.
  TDislocatorProbe = class(THMIControlDislocatorAnimation)
  public
    procedure MoveNow;
  end;

  { TDislocator2Probe }

  TDislocator2Probe = class(THMIControlDislocatorAnimation2)
  public
    procedure MoveNow;
  end;

  { TTestDislocator }

  TTestDislocator = class(TTestCase)
  private
    FAnim:TDislocatorProbe;
    FControl:TControl;
    FTag:TFakeNumber;
    //: faixa de valor 0..100 indo de (10,20) ate (110,220)
    procedure SetUpAStraightTrack;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WithNoControlNothingMoves;
    procedure WithNoTagNothingMoves;

    procedure TheStartValuePutsItOnTheFirstPoint;
    procedure TheEndValuePutsItOnTheLastPoint;
    procedure HalfWayInValueIsHalfWayInPixels;
    procedure AQuarterOfTheValueIsAQuarterOfTheWay;
    procedure TheFractionOfAPixelIsTruncatedNotRounded;
    procedure AValueBeyondTheRangeKeepsGoing;

    procedure TheUpperLimitHoldsItBack;
    procedure TheLowerLimitHoldsItBack;
    procedure TheLimitsAreIgnoredUntilEnabled;
    procedure EachAxisHasItsOwnLimits;

    procedure AnAxisWithNoRangeIsLeftAlone;
    procedure ChangingAPointMovesItAtOnce;
    procedure ADestroyedTagLetsGoOfTheAnimation;
    procedure ADestroyedControlLetsGoOfTheAnimation;
  end;

  { TTestDislocator2 }

  TTestDislocator2 = class(TTestCase)
  private
    FAnim:TDislocator2Probe;
    FControl:TControl;
    FTagX, FTagY:TFakeNumber;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EachAxisFollowsItsOwnTag;
    procedure MovingOnlyTheVerticalTagMovesOnlyTheTop;
    procedure WithOnlyOneOfTheTwoTagsNothingMoves;
    procedure TheTwoAxesHaveTheirOwnValueRanges;
  end;

implementation

{ TDislocatorProbe }

procedure TDislocatorProbe.MoveNow;
begin
  MoveObject(0);
end;

{ TDislocator2Probe }

procedure TDislocator2Probe.MoveNow;
begin
  MoveObject(0);
end;

{ TTestDislocator }

procedure TTestDislocator.SetUp;
begin
  FControl:=TControl.Create(nil);
  FTag:=TFakeNumber.Create(nil);
  FAnim:=TDislocatorProbe.Create(nil);
end;

procedure TTestDislocator.TearDown;
begin
  FreeAndNil(FAnim);
  FreeAndNil(FTag);
  FreeAndNil(FControl);
end;

procedure TTestDislocator.SetUpAStraightTrack;
begin
  //valor 0 no ponto (10,20), valor 100 no ponto (110,220): cem pixels na
  //horizontal e duzentos na vertical
  FAnim.P0_X:=10;
  FAnim.P0_Y:=20;
  FAnim.P1_X:=110;
  FAnim.P1_Y:=220;
  FAnim.ValueP0:=0;
  FAnim.ValueP1:=100;
  FAnim.Control:=FControl;
  FAnim.PLCTag:=FTag;
end;

procedure TTestDislocator.WithNoControlNothingMoves;
begin
  FAnim.P0_X:=10;  FAnim.P1_X:=110;
  FAnim.ValueP0:=0; FAnim.ValueP1:=100;
  FAnim.PLCTag:=FTag;

  FTag.ChegouDoCLP(50);
  FAnim.MoveNow;

  //sem alvo nao ha o que mover, e nada pode estourar
  AssertTrue('sem controle', FAnim.Control=nil);
end;

procedure TTestDislocator.WithNoTagNothingMoves;
begin
  FAnim.P0_X:=10;  FAnim.P1_X:=110;
  FAnim.ValueP0:=0; FAnim.ValueP1:=100;
  FControl.Left:=7;
  FAnim.Control:=FControl;

  FAnim.MoveNow;

  AssertEquals('o controle ficou onde estava', 7, FControl.Left);
end;

procedure TTestDislocator.TheStartValuePutsItOnTheFirstPoint;
begin
  SetUpAStraightTrack;

  FTag.ChegouDoCLP(0);
  FAnim.MoveNow;

  AssertEquals('esquerda', 10, FControl.Left);
  AssertEquals('topo',     20, FControl.Top);
end;

procedure TTestDislocator.TheEndValuePutsItOnTheLastPoint;
begin
  SetUpAStraightTrack;

  FTag.ChegouDoCLP(100);
  FAnim.MoveNow;

  AssertEquals('esquerda', 110, FControl.Left);
  AssertEquals('topo',     220, FControl.Top);
end;

procedure TTestDislocator.HalfWayInValueIsHalfWayInPixels;
begin
  SetUpAStraightTrack;

  FTag.ChegouDoCLP(50);
  FAnim.MoveNow;

  AssertEquals('esquerda', 60,  FControl.Left);
  AssertEquals('topo',     120, FControl.Top);
end;

procedure TTestDislocator.AQuarterOfTheValueIsAQuarterOfTheWay;
begin
  SetUpAStraightTrack;

  FTag.ChegouDoCLP(25);
  FAnim.MoveNow;

  AssertEquals('esquerda', 35, FControl.Left);
  AssertEquals('topo',     70, FControl.Top);
end;

procedure TTestDislocator.TheFractionOfAPixelIsTruncatedNotRounded;
begin
  //valor 0..3 em dez pixels: o valor 2 cai em 6.666..., que vira 6, nao 7
  FAnim.P0_X:=0;
  FAnim.P1_X:=10;
  FAnim.ValueP0:=0;
  FAnim.ValueP1:=3;
  FAnim.Control:=FControl;
  FAnim.PLCTag:=FTag;

  FTag.ChegouDoCLP(2);
  FAnim.MoveNow;

  AssertEquals('truncado para baixo', 6, FControl.Left);
end;

procedure TTestDislocator.AValueBeyondTheRangeKeepsGoing;
begin
  //sem limite ligado a escala extrapola; e' o que permite usar so um pedaco
  //da faixa do tag
  SetUpAStraightTrack;

  FTag.ChegouDoCLP(150);
  FAnim.MoveNow;

  AssertEquals('passou do fim', 160, FControl.Left);
end;

procedure TTestDislocator.TheUpperLimitHoldsItBack;
begin
  SetUpAStraightTrack;
  FAnim.MaxXValue:=100;
  FAnim.EnableXMax:=true;

  FTag.ChegouDoCLP(150);
  FAnim.MoveNow;

  AssertEquals('parou no limite', 100, FControl.Left);
end;

procedure TTestDislocator.TheLowerLimitHoldsItBack;
begin
  SetUpAStraightTrack;
  FAnim.MinXValue:=30;
  FAnim.EnableXMin:=true;

  FTag.ChegouDoCLP(0);
  FAnim.MoveNow;

  AssertEquals('parou no limite', 30, FControl.Left);
end;

procedure TTestDislocator.TheLimitsAreIgnoredUntilEnabled;
begin
  SetUpAStraightTrack;
  FAnim.MaxXValue:=100;

  FTag.ChegouDoCLP(150);
  FAnim.MoveNow;

  AssertEquals('o limite nao esta ligado', 160, FControl.Left);
end;

procedure TTestDislocator.EachAxisHasItsOwnLimits;
begin
  //segurar a horizontal nao pode segurar a vertical
  SetUpAStraightTrack;
  FAnim.MaxXValue:=100;
  FAnim.EnableXMax:=true;

  FTag.ChegouDoCLP(150);
  FAnim.MoveNow;

  AssertEquals('esquerda presa', 100, FControl.Left);
  AssertEquals('topo solto',     320, FControl.Top);
end;

procedure TTestDislocator.AnAxisWithNoRangeIsLeftAlone;
begin
  //quem quer movimento so na vertical deixa P0_X igual a P1_X; a horizontal
  //do controle nao pode ser tocada
  FControl.Left:=77;
  FAnim.P0_X:=0;
  FAnim.P1_X:=0;
  FAnim.P0_Y:=20;
  FAnim.P1_Y:=220;
  FAnim.ValueP0:=0;
  FAnim.ValueP1:=100;
  FAnim.Control:=FControl;
  FAnim.PLCTag:=FTag;

  FTag.ChegouDoCLP(50);
  FAnim.MoveNow;

  AssertEquals('esquerda intocada', 77,  FControl.Left);
  AssertEquals('topo moveu',        120, FControl.Top);
end;

procedure TTestDislocator.ChangingAPointMovesItAtOnce;
begin
  //mexer numa propriedade ja reposiciona, sem esperar o proximo valor do tag
  SetUpAStraightTrack;
  FTag.ChegouDoCLP(100);
  FAnim.MoveNow;
  AssertEquals('no fim da faixa', 110, FControl.Left);

  FAnim.P1_X:=210;

  AssertEquals('seguiu o ponto novo', 210, FControl.Left);
end;

procedure TTestDislocator.ADestroyedTagLetsGoOfTheAnimation;
begin
  SetUpAStraightTrack;

  FreeAndNil(FTag);

  AssertTrue('a animacao largou o tag', FAnim.PLCTag=nil);
end;

procedure TTestDislocator.ADestroyedControlLetsGoOfTheAnimation;
begin
  //sem isso o proximo MoveObject escreveria num ponteiro solto
  SetUpAStraightTrack;

  FreeAndNil(FControl);

  AssertTrue('a animacao largou o controle', FAnim.Control=nil);
end;

{ TTestDislocator2 }

procedure TTestDislocator2.SetUp;
begin
  FControl:=TControl.Create(nil);
  FTagX:=TFakeNumber.Create(nil);
  FTagY:=TFakeNumber.Create(nil);
  FAnim:=TDislocator2Probe.Create(nil);

  FAnim.P0_X:=10;
  FAnim.P0_Y:=20;
  FAnim.P1_X:=110;
  FAnim.P1_Y:=220;
  FAnim.ValueP0x:=0;
  FAnim.ValueP1x:=100;
  FAnim.ValueP0y:=0;
  FAnim.ValueP1y:=100;
  FAnim.Control:=FControl;
end;

procedure TTestDislocator2.TearDown;
begin
  FreeAndNil(FAnim);
  FreeAndNil(FTagX);
  FreeAndNil(FTagY);
  FreeAndNil(FControl);
end;

procedure TTestDislocator2.EachAxisFollowsItsOwnTag;
begin
  FAnim.PLCTagX:=FTagX;
  FAnim.PLCTagy:=FTagY;

  FTagX.ChegouDoCLP(100);
  FTagY.ChegouDoCLP(0);
  FAnim.MoveNow;

  AssertEquals('a horizontal foi ao fim',    110, FControl.Left);
  AssertEquals('e a vertical ficou no comeco', 20, FControl.Top);
end;

procedure TTestDislocator2.MovingOnlyTheVerticalTagMovesOnlyTheTop;
begin
  FAnim.PLCTagX:=FTagX;
  FAnim.PLCTagy:=FTagY;
  FTagX.ChegouDoCLP(0);
  FTagY.ChegouDoCLP(0);
  FAnim.MoveNow;

  FTagY.ChegouDoCLP(50);
  FAnim.MoveNow;

  AssertEquals('esquerda parada', 10,  FControl.Left);
  AssertEquals('topo moveu',      120, FControl.Top);
end;

procedure TTestDislocator2.WithOnlyOneOfTheTwoTagsNothingMoves;
begin
  FControl.Left:=7;
  FControl.Top :=9;
  FAnim.PLCTagX:=FTagX;

  FTagX.ChegouDoCLP(100);
  FAnim.MoveNow;

  AssertEquals('esquerda intocada', 7, FControl.Left);
  AssertEquals('topo intocado',     9, FControl.Top);
end;

procedure TTestDislocator2.TheTwoAxesHaveTheirOwnValueRanges;
begin
  //a vertical vai de 0 a 10 em vez de 0 a 100
  FAnim.ValueP0y:=0;
  FAnim.ValueP1y:=10;
  FAnim.PLCTagX:=FTagX;
  FAnim.PLCTagy:=FTagY;

  FTagX.ChegouDoCLP(50);
  FTagY.ChegouDoCLP(5);
  FAnim.MoveNow;

  AssertEquals('meio da horizontal', 60,  FControl.Left);
  AssertEquals('meio da vertical',   120, FControl.Top);
end;

initialization
  RegisterTest(TTestDislocator);
  RegisterTest(TTestDislocator2);

end.
