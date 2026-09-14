{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da fonte de dados de grafico: TTagLinkedSeriesSource.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' o que alimenta uma tendencia: cada valor novo do tag vira um ponto, e os
  pontos velhos saem quando passam da janela de tempo escolhida. Sem essa
  poda, uma tela deixada aberta a noite inteira acumula pontos ate acabar a
  memoria.

  A fonte guarda os pontos numa lista que da' para ler de fora - Count e
  Item[] - entao tudo isso se confere sem grafico nenhum na tela.
}
{$ELSE}
{:
  @abstract(Chart data source tests: TTagLinkedSeriesSource.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This is what feeds a trend: every new tag value becomes a point, and old
  points leave once they fall outside the chosen time window. Without that
  trimming, a screen left open overnight piles up points until memory runs
  out.

  The source keeps its points in a list that can be read from outside - Count
  and Item[] - so all of it can be checked with no chart on screen at all.
}
{$ENDIF}
unit ut.tagchartsource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  TagLinkedSeriesSource,
  testsupport.faketag;

type

  { TTestTagChartSource }

  TTestTagChartSource = class(TTestCase)
  private
    FSource:TTagLinkedSeriesSource;
    FTag:TFakeNumber;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //acumulo de pontos / collecting points
    procedure ANewSourceHasNoPoints;
    procedure ATagChangeAddsAPoint;
    procedure ThePointCarriesTheTagValue;
    procedure EveryChangeAddsAnotherPoint;
    procedure WithNoTagNothingIsCollected;

    //quando coletar / when to collect
    procedure OnlyTheTagChangeModeCollectsOnAChange;
    procedure TheCyclicModeDoesNotCollectOnAChange;

    //o eixo do tempo / the time axis
    procedure ByDefaultThePointIsStampedWithTheClock;
    procedure TheTagTimestampGoesInOnTheSameScale;

    //poda / trimming
    procedure OldPointsLeaveTheWindow;
    procedure TheWindowKeepsPointsCollectedWithinIt;
    procedure TheWindowTrimsOnTheTagTimestampToo;
    procedure PointsInsideTheWindowStay;
    procedure WithTheWindowOffNothingIsTrimmed;
    procedure AWindowOfZeroTrimsNothing;

    //desligamento / detaching
    procedure ChangingTagsStopsTheOldOne;
    procedure ADestroyedTagLetsGoOfTheSource;
  end;

implementation

{ TTestTagChartSource }

procedure TTestTagChartSource.SetUp;
begin
  FSource:=TTagLinkedSeriesSource.Create(nil);
  FSource.SourceUpdateType:=tlTagChange;
  FTag:=TFakeNumber.Create(nil);
  FSource.PLCTag:=FTag;
end;

procedure TTestTagChartSource.TearDown;
begin
  FreeAndNil(FSource);
  FreeAndNil(FTag);
end;

procedure TTestTagChartSource.ANewSourceHasNoPoints;
var
  nova:TTagLinkedSeriesSource;
begin
  nova:=TTagLinkedSeriesSource.Create(nil);
  try
    AssertEquals('sem pontos', 0, nova.Count);
  finally
    nova.Free;
  end;
end;

procedure TTestTagChartSource.ATagChangeAddsAPoint;
begin
  FTag.ChegouDoCLP(10);

  AssertEquals('um ponto', 1, FSource.Count);
end;

procedure TTestTagChartSource.ThePointCarriesTheTagValue;
begin
  FTag.ChegouDoCLP(42.5);

  AssertEquals('o valor do tag', 42.5, FSource.Item[0]^.Y, 0.0001);
end;

procedure TTestTagChartSource.EveryChangeAddsAnotherPoint;
begin
  FTag.ChegouDoCLP(1);
  FTag.ChegouDoCLP(2);
  FTag.ChegouDoCLP(3);

  AssertEquals('tres pontos', 3, FSource.Count);
  AssertEquals('primeiro', 1, FSource.Item[0]^.Y, 0.0001);
  AssertEquals('ultimo',   3, FSource.Item[2]^.Y, 0.0001);
end;

procedure TTestTagChartSource.WithNoTagNothingIsCollected;
var
  nova:TTagLinkedSeriesSource;
begin
  nova:=TTagLinkedSeriesSource.Create(nil);
  try
    nova.SourceUpdateType:=tlTagChange;
    FTag.ChegouDoCLP(10);

    AssertEquals('sem tag, sem pontos', 0, nova.Count);
  finally
    nova.Free;
  end;
end;

procedure TTestTagChartSource.OnlyTheTagChangeModeCollectsOnAChange;
begin
  FSource.SourceUpdateType:=tlTagUpdate;

  FTag.ChegouDoCLP(10);

  AssertEquals('o modo por atualizacao nao coleta na mudanca', 0, FSource.Count);
end;

procedure TTestTagChartSource.TheCyclicModeDoesNotCollectOnAChange;
begin
  //no modo ciclico quem coleta e o temporizador, nao o aviso do tag
  FSource.SourceUpdateType:=tlCyclic;

  FTag.ChegouDoCLP(10);

  AssertEquals('nenhum ponto', 0, FSource.Count);
end;

procedure TTestTagChartSource.ByDefaultThePointIsStampedWithTheClock;
var
  antes, depois:TDateTime;
begin
  antes:=Now;
  FTag.ChegouDoCLP(10);
  depois:=Now;

  AssertTrue('o carimbo esta entre o antes e o depois',
             (FSource.Item[0]^.X>=antes) and (FSource.Item[0]^.X<=depois));
end;

procedure TTestTagChartSource.TheTagTimestampGoesInOnTheSameScale;
var
  x:TDateTime;
begin
  //o carimbo do tag conta milissegundos desde o boot; no eixo ele entra como
  //tempo decorrido, nao como um numero de dias solto
  FSource.UseNowInsteadTagTimestamp:=false;

  FTag.ChegouDoCLP(10);

  x:=FSource.Item[0]^.X;
  AssertTrue('e tempo decorrido, nao uma data de milhares de anos',
             x<EncodeDate(1900,1,1));
  AssertTrue('e e maior que zero', x>0);
end;

procedure TTestTagChartSource.OldPointsLeaveTheWindow;
begin
  //janela de meio segundo, com o relogio de parede
  FSource.EnableXAxisMaxInterval:=true;
  FSource.XAxisMaximumInterval:=500;

  FTag.ChegouDoCLP(1);
  AssertEquals('o primeiro entrou', 1, FSource.Count);

  Sleep(700);
  FTag.ChegouDoCLP(2);

  AssertEquals('o velho saiu e so o novo ficou', 1, FSource.Count);
  AssertEquals('e e o novo', 2, FSource.Item[0]^.Y, 0.0001);
end;

procedure TTestTagChartSource.TheWindowKeepsPointsCollectedWithinIt;
begin
  //janela de um minuto, tres pontos colhidos em milissegundos
  FSource.EnableXAxisMaxInterval:=true;
  FSource.XAxisMaximumInterval:=60000;

  FTag.ChegouDoCLP(1);
  Sleep(5);
  FTag.ChegouDoCLP(2);
  Sleep(5);
  FTag.ChegouDoCLP(3);

  AssertEquals('os tres estao dentro da janela', 3, FSource.Count);
end;

procedure TTestTagChartSource.TheWindowTrimsOnTheTagTimestampToo;
begin
  //a mesma poda, agora com o carimbo do tag: as duas escalas tem que
  //concordar, senao a janela de um minuto engole tudo
  FSource.UseNowInsteadTagTimestamp:=false;
  FSource.EnableXAxisMaxInterval:=true;
  FSource.XAxisMaximumInterval:=60000;

  FTag.ChegouDoCLP(1);
  Sleep(5);
  FTag.ChegouDoCLP(2);
  Sleep(5);
  FTag.ChegouDoCLP(3);

  AssertEquals('os tres continuam dentro', 3, FSource.Count);
end;

procedure TTestTagChartSource.PointsInsideTheWindowStay;
begin
  FSource.EnableXAxisMaxInterval:=true;
  FSource.XAxisMaximumInterval:=60000;

  FTag.ChegouDoCLP(1);
  FTag.ChegouDoCLP(2);
  FTag.ChegouDoCLP(3);

  AssertEquals('os tres ficaram', 3, FSource.Count);
end;

procedure TTestTagChartSource.WithTheWindowOffNothingIsTrimmed;
begin
  FSource.EnableXAxisMaxInterval:=false;
  FSource.XAxisMaximumInterval:=500;

  FTag.ChegouDoCLP(1);
  Sleep(700);
  FTag.ChegouDoCLP(2);

  AssertEquals('os dois ficaram', 2, FSource.Count);
end;

procedure TTestTagChartSource.AWindowOfZeroTrimsNothing;
begin
  FSource.EnableXAxisMaxInterval:=true;
  FSource.XAxisMaximumInterval:=0;

  FTag.ChegouDoCLP(1);
  Sleep(50);
  FTag.ChegouDoCLP(2);

  AssertEquals('janela zero nao poda', 2, FSource.Count);
end;

procedure TTestTagChartSource.ChangingTagsStopsTheOldOne;
var
  outro:TFakeNumber;
begin
  outro:=TFakeNumber.Create(nil);
  try
    FSource.PLCTag:=outro;

    FTag.ChegouDoCLP(10);
    AssertEquals('o tag antigo nao alimenta mais', 0, FSource.Count);

    outro.ChegouDoCLP(20);
    AssertEquals('e o novo alimenta', 1, FSource.Count);
  finally
    FSource.PLCTag:=nil;
    outro.Free;
  end;
end;

procedure TTestTagChartSource.ADestroyedTagLetsGoOfTheSource;
begin
  FreeAndNil(FTag);

  AssertTrue('a fonte largou o tag', FSource.PLCTag=nil);
end;

initialization
  RegisterTest(TTestTagChartSource);

end.
