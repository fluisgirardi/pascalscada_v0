{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIPolyline e do THMIFlowPolyline: a linha desenhada
            ponto a ponto e a linha que herda a cor de quem a alimenta.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O THMIPolyline e' uma lista de pontos e uma caneta. O THMIFlowPolyline
  acrescenta a isso um vinculo: ele olha a cor de outras linhas - as fontes -
  e pinta a sua com o que achar la'. E' assim que um fluxograma mostra o
  caminho que o produto esta' fazendo: o trecho a montante muda de cor e a
  mudanca desce sozinha por todos os trechos ligados a ele.

  Quando duas fontes discordam, quem decide e' MultipleColorBehavior. E' esse
  o coracao do controle e e' o que mais se testa aqui, junto com o roteamento
  automatico dos pontos do meio.
}
{$ELSE}
{:
  @abstract(THMIPolyline and THMIFlowPolyline tests: the line drawn point by
            point, and the line that inherits the colour of whatever feeds it.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  THMIPolyline is a list of points and a pen. THMIFlowPolyline adds a link to
  that: it watches the colour of other lines - its sources - and paints itself
  with whatever it finds there. That is how a flowsheet shows the path the
  product is taking: the upstream leg changes colour and the change walks down
  by itself through every leg attached to it.

  When two sources disagree, MultipleColorBehavior decides. That is the heart
  of the control and what is tested the most here, together with the automatic
  routing of the middle points.
}
{$ENDIF}
unit ut.hmipolyline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, fpcunit, testregistry,
  hmi_polyline;

type

  { TPolylineProbe }

  //InvalidateShape e' protegido e e' o unico sinal de que o controle entendeu
  //que precisa se redesenhar. A sonda conta as chamadas.
  //
  //InvalidateShape is protected and is the only sign that the control
  //understood it has to redraw itself. The probe counts the calls.
  TPolylineProbe = class(THMIPolyline)
  private
    FRedraws:LongInt;
  protected
    procedure InvalidateShape; override;
  public
    procedure ForgetRedraws;
    property Redraws:LongInt read FRedraws;
  end;

  { TFlowProbe }

  TFlowProbe = class(THMIFlowPolyline)
  private
    FRecalcs:LongInt;
  protected
    procedure RecalculateColor(WhoChanged:THMIFlowPolyline=nil); override;
  public
    procedure ForgetRecalcs;
    procedure Reroute;
    property Recalcs:LongInt read FRecalcs;
  end;

  { TTestHMIPolyline }

  TTestHMIPolyline = class(TTestCase)
  private
    FLine:TPolylineProbe;
    function NewPoint(x,y:Integer):TPointCollectionItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //os pontos / the points
    procedure ANewPolylineHasNoPoints;
    procedure TheDefaultsAreBlackTwoPixelsAndSolid;
    procedure AddedPointsKeepTheirCoordinates;
    procedure TheDisplayNameShowsTheCoordinates;
    procedure AssigningThePointsOfAnotherPolylineCopiesThem;

    //o redesenho / the redraw
    procedure AddingAPointRedrawsTheControl;
    procedure MovingAPointRedrawsTheControl;
    procedure MovingAPointToTheSamePlaceDoesNotRedraw;
    procedure DeletingAPointRedrawsTheControl;

    //a caneta / the pen
    procedure TheLineWidthIsKept;
    procedure ThePenStyleIsKept;
    procedure TheColorClNoneHidesThePolyline;
    procedure AColorShowsThePolylineAgain;
  end;

  { TTestHMIFlowPolyline }

  TTestHMIFlowPolyline = class(TTestCase)
  private
    FLine:TFlowProbe;
    FA, FB:THMIFlowPolyline;
    function  FeedWith(aSource:THMIFlowPolyline):THMIFlowSourceCollectionItem;
    procedure ColorChanged(Sender:TObject);
  private
    FColorChanges:LongInt;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //uma fonte so' / a single source
    procedure WithNoSourcesTheEmptyColorPaintsTheLine;
    procedure TheColourOfTheSourceBecomesTheColourOfTheLine;
    procedure ASourceAtTheEmptyColourLeavesTheLineEmpty;
    procedure TwoSourcesOfTheSameColourPaintThatColour;
    procedure TheColourChangeEventFiresOnTheSource;

    //fontes discordando / sources disagreeing
    procedure MixingWithCmbEmptyPaintsTheEmptyColour;
    procedure MixingWithCmbMultipleColorsReplacePaintsTheReplacement;
    procedure MixingWithCmbLastColorPaintsWhoChanged;
    procedure MixingWithCmbAndCombinesTheColours;
    procedure MixingWithCmbOrCombinesTheColours;
    procedure MixingWithCmbXorCombinesTheColours;
    procedure AnUnassignedSourceIsIgnoredWhileMixing;
    procedure ChangingTheMixBehaviorRecalculatesRightAway;
    procedure ChangingTheEmptyColourRecalculatesRightAway;

    //o vinculo / the link
    procedure TheSameSourceIsRegisteredOnlyOnce;
    procedure DroppingTheSourceStopsTheNotifications;
    procedure ADestroyedSourceLeavesTheCollection;
    procedure ADestroyedSourceDoesNotPaintAnymore;
    procedure AssigningTheSourcesOfAnotherLineCopiesThem;

    //o roteamento automatico / the automatic routing
    procedure AutoRouteBuildsSixPoints;
    procedure TheHeightDistributionDefaultsToFifty;
    procedure TheHeightDistributionIsKept;
    procedure AHeightDistributionOutOfRangeIsRefused;
    procedure TheFirstSegmentLeavesInTheStartDirection;
    procedure TheLastSegmentArrivesInTheEndDirection;
    procedure TheMiddleSegmentSplitsTheDistance;
    procedure ChangingTheStartOffsetReroutesTheLine;
  end;

implementation

function Pt(aLine:THMIPolyline; i:Integer):TPointCollectionItem;
begin
  Result:=TPointCollectionItem(aLine.PointCoordinates.Items[i]);
end;

{ TPolylineProbe }

procedure TPolylineProbe.InvalidateShape;
begin
  inc(FRedraws);
  inherited InvalidateShape;
end;

procedure TPolylineProbe.ForgetRedraws;
begin
  FRedraws:=0;
end;

{ TFlowProbe }

procedure TFlowProbe.RecalculateColor(WhoChanged:THMIFlowPolyline);
begin
  inc(FRecalcs);
  inherited RecalculateColor(WhoChanged);
end;

procedure TFlowProbe.ForgetRecalcs;
begin
  FRecalcs:=0;
end;

procedure TFlowProbe.Reroute;
begin
  RecalculateMidPoints;
end;

{ TTestHMIPolyline }

procedure TTestHMIPolyline.SetUp;
begin
  FLine:=TPolylineProbe.Create(nil);
end;

procedure TTestHMIPolyline.TearDown;
begin
  FreeAndNil(FLine);
end;

function TTestHMIPolyline.NewPoint(x,y:Integer):TPointCollectionItem;
begin
  Result:=FLine.PointCoordinates.Add;
  Result.X:=x;
  Result.Y:=y;
end;

procedure TTestHMIPolyline.ANewPolylineHasNoPoints;
begin
  AssertEquals('sem pontos', 0, FLine.PointCoordinates.Count);
end;

procedure TTestHMIPolyline.TheDefaultsAreBlackTwoPixelsAndSolid;
begin
  AssertEquals('cor da linha', clBlack, FLine.LineColor);
  AssertEquals('espessura',    2,       FLine.LineWidth);
  AssertTrue  ('caneta cheia', FLine.PenStyle=psSolid);
end;

procedure TTestHMIPolyline.AddedPointsKeepTheirCoordinates;
begin
  NewPoint(10, 20);
  NewPoint(30, 40);

  AssertEquals('dois pontos', 2,  FLine.PointCoordinates.Count);
  AssertEquals('x do 1o',     10, Pt(FLine,0).X);
  AssertEquals('y do 1o',     20, Pt(FLine,0).Y);
  AssertEquals('x do 2o',     30, Pt(FLine,1).X);
  AssertEquals('y do 2o',     40, Pt(FLine,1).Y);
end;

procedure TTestHMIPolyline.TheDisplayNameShowsTheCoordinates;
begin
  //e' o que o editor de colecao mostra na lista de pontos
  //it is what the collection editor shows on the point list
  NewPoint(7, 9);

  AssertEquals('o rotulo do item', '(x=7, y=9)', Pt(FLine,0).DisplayName);
end;

procedure TTestHMIPolyline.AssigningThePointsOfAnotherPolylineCopiesThem;
var
  outra:TPolylineProbe;
begin
  NewPoint(10, 20);
  NewPoint(30, 40);

  outra:=TPolylineProbe.Create(nil);
  try
    outra.PointCoordinates:=FLine.PointCoordinates;

    AssertEquals('copiou os dois pontos', 2,  outra.PointCoordinates.Count);
    AssertEquals('x do 1o',               10, Pt(outra,0).X);
    AssertEquals('y do 1o',               20, Pt(outra,0).Y);
    AssertEquals('x do 2o',               30, Pt(outra,1).X);
    AssertEquals('y do 2o',               40, Pt(outra,1).Y);
  finally
    outra.Free;
  end;
end;

procedure TTestHMIPolyline.AddingAPointRedrawsTheControl;
begin
  FLine.ForgetRedraws;

  FLine.PointCoordinates.Add;

  AssertTrue('o ponto novo mudou o desenho', FLine.Redraws>0);
end;

procedure TTestHMIPolyline.MovingAPointRedrawsTheControl;
begin
  NewPoint(10, 20);
  FLine.ForgetRedraws;

  Pt(FLine,0).X:=50;

  AssertTrue('mover o ponto mudou o desenho', FLine.Redraws>0);
end;

procedure TTestHMIPolyline.MovingAPointToTheSamePlaceDoesNotRedraw;
begin
  NewPoint(10, 20);
  FLine.ForgetRedraws;

  Pt(FLine,0).X:=10;
  Pt(FLine,0).Y:=20;

  AssertEquals('nada mudou, nada a redesenhar', 0, FLine.Redraws);
end;

procedure TTestHMIPolyline.DeletingAPointRedrawsTheControl;
begin
  NewPoint(10, 20);
  NewPoint(30, 40);
  FLine.ForgetRedraws;

  FLine.PointCoordinates.Delete(1);

  AssertEquals('sobrou um ponto', 1, FLine.PointCoordinates.Count);
  AssertTrue('tirar o ponto mudou o desenho', FLine.Redraws>0);
end;

procedure TTestHMIPolyline.TheLineWidthIsKept;
begin
  FLine.LineWidth:=7;

  AssertEquals('espessura', 7, FLine.LineWidth);
end;

procedure TTestHMIPolyline.ThePenStyleIsKept;
begin
  FLine.PenStyle:=psDash;

  AssertTrue('caneta tracejada', FLine.PenStyle=psDash);
end;

procedure TTestHMIPolyline.TheColorClNoneHidesThePolyline;
begin
  //clNone e' como o fluxograma apaga um trecho: sem cor, sem linha
  //clNone is how the flowsheet erases a leg: no colour, no line
  FLine.LineColor:=clNone;

  AssertFalse('sumiu da tela', FLine.Visible);
end;

procedure TTestHMIPolyline.AColorShowsThePolylineAgain;
begin
  FLine.LineColor:=clNone;

  FLine.LineColor:=clRed;

  AssertTrue  ('voltou a aparecer', FLine.Visible);
  AssertEquals('com a cor nova',    clRed, FLine.LineColor);
end;

{ TTestHMIFlowPolyline }

procedure TTestHMIFlowPolyline.SetUp;
begin
  FColorChanges:=0;
  FLine:=TFlowProbe.Create(nil);
  FA:=THMIFlowPolyline.Create(nil);
  FB:=THMIFlowPolyline.Create(nil);
end;

procedure TTestHMIFlowPolyline.TearDown;
begin
  FreeAndNil(FLine);
  FreeAndNil(FA);
  FreeAndNil(FB);
end;

function TTestHMIFlowPolyline.FeedWith(aSource:THMIFlowPolyline):THMIFlowSourceCollectionItem;
begin
  Result:=FLine.FlowSource.Add;
  Result.HMIObject:=aSource;
end;

procedure TTestHMIFlowPolyline.ColorChanged(Sender:TObject);
begin
  inc(FColorChanges);
end;

procedure TTestHMIFlowPolyline.WithNoSourcesTheEmptyColorPaintsTheLine;
begin
  FLine.EmptyColor:=clSilver;

  AssertEquals('sem fonte, cor de vazio', clSilver, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.TheColourOfTheSourceBecomesTheColourOfTheLine;
begin
  FeedWith(FA);

  FA.LineColor:=clRed;

  AssertEquals('herdou a cor da fonte', clRed, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.ASourceAtTheEmptyColourLeavesTheLineEmpty;
begin
  FLine.EmptyColor:=clSilver;
  FeedWith(FA);

  FA.LineColor:=clSilver;

  AssertEquals('fonte vazia nao pinta', clSilver, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.TwoSourcesOfTheSameColourPaintThatColour;
begin
  FeedWith(FA);
  FeedWith(FB);

  FA.LineColor:=clRed;
  FB.LineColor:=clRed;

  AssertEquals('as duas concordam', clRed, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.TheColourChangeEventFiresOnTheSource;
begin
  FA.OnColorChange:=@ColorChanged;

  FA.LineColor:=clRed;

  AssertEquals('avisou uma vez', 1, FColorChanges);
end;

procedure TTestHMIFlowPolyline.MixingWithCmbEmptyPaintsTheEmptyColour;
begin
  FLine.EmptyColor:=clSilver;
  FLine.MultipleColorBehavior:=cmbEmpty;
  FeedWith(FA);
  FeedWith(FB);

  FA.LineColor:=clRed;
  FB.LineColor:=clYellow;

  AssertEquals('na duvida, vazio', clSilver, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.MixingWithCmbMultipleColorsReplacePaintsTheReplacement;
begin
  FLine.MultipleColorBehavior:=cmbMultipleColorsReplace;
  FLine.MultipleColorsReplace:=clSilver;
  FeedWith(FA);
  FeedWith(FB);

  FA.LineColor:=clRed;
  FB.LineColor:=clYellow;

  AssertEquals('a cor combinada', clSilver, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.MixingWithCmbLastColorPaintsWhoChanged;
begin
  FLine.MultipleColorBehavior:=cmbLastColor;
  FeedWith(FA);
  FeedWith(FB);

  FA.LineColor:=clRed;
  FB.LineColor:=clYellow;

  AssertEquals('a ultima que mudou', clYellow, FLine.LineColor);

  FA.LineColor:=clLime;

  AssertEquals('e de novo a ultima', clLime, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.MixingWithCmbAndCombinesTheColours;
begin
  FLine.MultipleColorBehavior:=cmbAnd;
  FeedWith(FA);
  FeedWith(FB);

  FA.LineColor:=clRed;     //$0000FF
  FB.LineColor:=clYellow;  //$00FFFF

  AssertEquals('E das duas cores', clRed, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.MixingWithCmbOrCombinesTheColours;
begin
  FLine.MultipleColorBehavior:=cmbOr;
  FeedWith(FA);
  FeedWith(FB);

  FA.LineColor:=clRed;     //$0000FF
  FB.LineColor:=clLime;    //$00FF00

  AssertEquals('OU das duas cores', clYellow, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.MixingWithCmbXorCombinesTheColours;
begin
  FLine.MultipleColorBehavior:=cmbXor;
  FeedWith(FA);
  FeedWith(FB);

  FA.LineColor:=clRed;     //$0000FF
  FB.LineColor:=clYellow;  //$00FFFF

  AssertEquals('XOU das duas cores', clLime, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.AnUnassignedSourceIsIgnoredWhileMixing;
begin
  //um item recem criado na colecao, ou um que perdeu a linha, nao tem cor
  //nenhuma para entrar na conta
  //
  //an item just created on the collection, or one that lost its line, has no
  //colour at all to take part in the calculation
  FLine.MultipleColorBehavior:=cmbOr;
  FLine.FlowSource.Add;
  FeedWith(FA);
  FeedWith(FB);

  FA.LineColor:=clRed;     //$0000FF
  FB.LineColor:=clLime;    //$00FF00

  AssertEquals('o item sem linha nao conta', clYellow, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.ChangingTheMixBehaviorRecalculatesRightAway;
begin
  FLine.MultipleColorBehavior:=cmbLastColor;
  FLine.MultipleColorsReplace:=clSilver;
  FeedWith(FA);
  FeedWith(FB);
  FA.LineColor:=clRed;
  FB.LineColor:=clYellow;

  FLine.MultipleColorBehavior:=cmbMultipleColorsReplace;

  AssertEquals('a regra nova valeu na hora', clSilver, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.ChangingTheEmptyColourRecalculatesRightAway;
begin
  FLine.MultipleColorBehavior:=cmbEmpty;
  FeedWith(FA);
  FeedWith(FB);
  FA.LineColor:=clRed;
  FB.LineColor:=clYellow;

  FLine.EmptyColor:=clSilver;

  AssertEquals('a cor de vazio nova valeu na hora', clSilver, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.TheSameSourceIsRegisteredOnlyOnce;
begin
  //dois itens apontando para a mesma linha nao podem fazer a conta rodar duas
  //vezes a cada mudanca
  //
  //two items pointing at the same line must not make the calculation run
  //twice on every change
  FeedWith(FA);
  FeedWith(FA);
  FLine.ForgetRecalcs;

  FA.LineColor:=clRed;

  AssertEquals('recalculou uma vez so', 1, FLine.Recalcs);
end;

procedure TTestHMIFlowPolyline.DroppingTheSourceStopsTheNotifications;
var
  item:THMIFlowSourceCollectionItem;
begin
  item:=FeedWith(FA);
  item.HMIObject:=nil;
  FLine.ForgetRecalcs;

  FA.LineColor:=clRed;

  AssertEquals('nao avisa mais', 0, FLine.Recalcs);
end;

procedure TTestHMIFlowPolyline.ADestroyedSourceLeavesTheCollection;
begin
  FeedWith(FA);

  FreeAndNil(FA);

  AssertEquals('a fonte saiu da lista', 0, FLine.FlowSource.Count);
end;

procedure TTestHMIFlowPolyline.ADestroyedSourceDoesNotPaintAnymore;
begin
  FLine.EmptyColor:=clSilver;
  FeedWith(FA);
  FA.LineColor:=clRed;
  AssertEquals('pintou', clRed, FLine.LineColor);

  FreeAndNil(FA);

  AssertEquals('sem fonte, cor de vazio', clSilver, FLine.LineColor);
end;

procedure TTestHMIFlowPolyline.AssigningTheSourcesOfAnotherLineCopiesThem;
var
  outra:TFlowProbe;
begin
  FeedWith(FA);

  outra:=TFlowProbe.Create(nil);
  try
    outra.FlowSource:=FLine.FlowSource;

    AssertEquals('copiou a fonte',    1,   outra.FlowSource.Count);
    AssertTrue  ('e e a mesma linha', THMIFlowSourceCollectionItem(outra.FlowSource.Items[0]).HMIObject=FA);
  finally
    outra.Free;
  end;
end;

procedure TTestHMIFlowPolyline.AutoRouteBuildsSixPoints;
begin
  FLine.AutoRoute:=true;

  AssertEquals('seis pontos', 6, FLine.PointCoordinates.Count);
end;

procedure TTestHMIFlowPolyline.TheHeightDistributionDefaultsToFifty;
begin
  AssertEquals('metade do caminho', 50, FLine.AutoHeightDistribution, 0.0001);
end;

procedure TTestHMIFlowPolyline.TheHeightDistributionIsKept;
begin
  FLine.AutoHeightDistribution:=25;

  AssertEquals('um quarto', 25, FLine.AutoHeightDistribution, 0.0001);
end;

procedure TTestHMIFlowPolyline.AHeightDistributionOutOfRangeIsRefused;
begin
  FLine.AutoHeightDistribution:=25;

  FLine.AutoHeightDistribution:=-1;
  AssertEquals('negativo recusado', 25, FLine.AutoHeightDistribution, 0.0001);

  FLine.AutoHeightDistribution:=101;
  AssertEquals('acima de 100 recusado', 25, FLine.AutoHeightDistribution, 0.0001);
end;

procedure TTestHMIFlowPolyline.TheFirstSegmentLeavesInTheStartDirection;
begin
  FLine.AutoRoute:=true;
  FLine.AutoStartPointDirection:=pdVertical;
  FLine.AutoStartPointOffset:=10;
  Pt(FLine,0).X:=0;
  Pt(FLine,0).Y:=0;
  Pt(FLine,5).X:=100;
  Pt(FLine,5).Y:=100;

  FLine.Reroute;

  AssertEquals('sai na vertical', Pt(FLine,0).X,    Pt(FLine,1).X);
  AssertEquals('o recuo pedido',  Pt(FLine,0).Y+10, Pt(FLine,1).Y);
end;

procedure TTestHMIFlowPolyline.TheLastSegmentArrivesInTheEndDirection;
begin
  FLine.AutoRoute:=true;
  FLine.AutoEndPointDirection:=pdHorizontal;
  FLine.AutoEndPointOffset:=-10;
  Pt(FLine,0).X:=0;
  Pt(FLine,0).Y:=0;
  Pt(FLine,5).X:=100;
  Pt(FLine,5).Y:=100;

  FLine.Reroute;

  AssertEquals('chega na horizontal', Pt(FLine,5).Y,    Pt(FLine,4).Y);
  AssertEquals('o recuo pedido',      Pt(FLine,5).X-10, Pt(FLine,4).X);
end;

procedure TTestHMIFlowPolyline.TheMiddleSegmentSplitsTheDistance;
begin
  FLine.AutoRoute:=true;
  FLine.AutoStartPointDirection:=pdVertical;
  FLine.AutoEndPointDirection:=pdVertical;
  FLine.AutoHeightDistribution:=50;
  Pt(FLine,0).X:=0;
  Pt(FLine,0).Y:=0;
  Pt(FLine,5).X:=100;
  Pt(FLine,5).Y:=100;

  FLine.Reroute;

  AssertEquals('o trecho do meio e horizontal', Pt(FLine,2).Y, Pt(FLine,3).Y);
  AssertEquals('desceu a metade',
               Pt(FLine,1).Y+((Pt(FLine,5).Y-Pt(FLine,1).Y) div 2),
               Pt(FLine,2).Y);
end;

procedure TTestHMIFlowPolyline.ChangingTheStartOffsetReroutesTheLine;
var
  antes:Integer;
begin
  FLine.AutoRoute:=true;
  FLine.AutoStartPointDirection:=pdVertical;
  Pt(FLine,0).X:=0;
  Pt(FLine,0).Y:=0;
  Pt(FLine,5).X:=100;
  Pt(FLine,5).Y:=100;
  FLine.Reroute;
  antes:=Pt(FLine,1).Y;

  FLine.AutoStartPointOffset:=antes+20;

  AssertEquals('o recuo novo ja valeu', Pt(FLine,0).Y+antes+20, Pt(FLine,1).Y);
end;

initialization
  RegisterTest(TTestHMIPolyline);
  RegisterTest(TTestHMIFlowPolyline);

end.
