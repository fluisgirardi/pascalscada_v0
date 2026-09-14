{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIText: o valor de um tag virando uma frase escolhida.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' o rotulo que nao mostra o numero, e sim o que o numero quer dizer: 0 vira
  "parado", 1 vira "ligado", qualquer outra coisa vira "estado invalido". Cada
  frase e' uma zona com o seu criterio, o seu texto, a sua cor e o seu
  alinhamento, e quem escolhe entre elas e' a mesma conta ja testada no
  TZones.

  O que se confere aqui e' o resultado na tela: a legenda que sai, a cor que
  fica, e o que acontece quando nenhuma zona serve.
}
{$ELSE}
{:
  @abstract(THMIText tests: a tag's value turning into a chosen phrase.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It is the label that shows not the number but what the number means: 0
  becomes "stopped", 1 becomes "running", anything else becomes "invalid
  state". Each phrase is a zone with its own criterion, text, colour and
  alignment, and what picks among them is the same arithmetic already tested
  in TZones.

  What is checked here is the result on screen: the caption that comes out,
  the colour that stays, and what happens when no zone fits.
}
{$ENDIF}
unit ut.hmitext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, StdCtrls, fpcunit, testregistry,
  HMIText, hmizones,
  testsupport.faketag;

type

  { TTestHMIText }

  TTestHMIText = class(TTestCase)
  private
    FText:THMIText;
    FTag:TFakeNumber;
    procedure TagValueIs(v:Double);
    //: drena a fila da aplicacao, onde o aviso de mudanca de zona e agendado
    procedure Settle;
    function  NewZone(aValue:Double; const aText:String):TTextZone;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a frase escolhida / the chosen phrase
    procedure WithNoZonesTheCaptionIsEmpty;
    procedure TheMatchingZoneGivesItsText;
    procedure AnotherValuePicksAnotherZone;
    procedure TheFirstMatchingZoneWins;

    //quando nenhuma serve / when none fits
    procedure WithNoMatchAndNoDefaultTheCaptionIsEmpty;
    procedure TheDefaultZoneAnswersWhenNothingElseMatches;
    procedure WithNoTagTheDefaultZoneIsShown;

    //o que a zona traz junto / what the zone brings with it
    procedure TheZoneBringsItsOwnColour;
    procedure TheZoneBringsItsOwnAlignment;
    procedure AnEmptyCaptionComesTransparent;

    //prefixo e sufixo / prefix and suffix
    procedure ThePrefixAndSuffixWrapTheZoneText;

    //mudancas / changes
    procedure ChangingTheZoneTextRebuildsTheCaption;
    procedure ADestroyedTagLetsGoOfTheControl;
    procedure ClearingTheTagFallsBackToNoZone;
  end;

implementation

{ TTestHMIText }

procedure TTestHMIText.SetUp;
begin
  FText:=THMIText.Create(nil);
  FTag:=TFakeNumber.Create(nil);
end;

procedure TTestHMIText.TearDown;
begin
  FreeAndNil(FText);
  FreeAndNil(FTag);
end;

procedure TTestHMIText.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  //o aviso do tag agenda a atualizacao na fila da aplicacao
  Application.ProcessMessages;
end;

procedure TTestHMIText.Settle;
begin
  Application.ProcessMessages;
end;

function TTestHMIText.NewZone(aValue:Double; const aText:String):TTextZone;
begin
  Result:=FText.Zones.Add;
  Result.ZoneType:=ztEqual;
  Result.SetValues(aValue, aValue);
  Result.Text:=aText;
end;

procedure TTestHMIText.WithNoZonesTheCaptionIsEmpty;
begin
  FText.PLCTag:=FTag;

  TagValueIs(1);

  AssertEquals('sem zona nenhuma, nada a dizer', '', FText.Caption);
end;

procedure TTestHMIText.TheMatchingZoneGivesItsText;
begin
  NewZone(0, 'parado');
  NewZone(1, 'ligado');
  FText.PLCTag:=FTag;

  TagValueIs(1);

  AssertEquals('a frase da zona do 1', 'ligado', FText.Caption);
end;

procedure TTestHMIText.AnotherValuePicksAnotherZone;
begin
  NewZone(0, 'parado');
  NewZone(1, 'ligado');
  FText.PLCTag:=FTag;

  TagValueIs(1);
  AssertEquals('ligado', 'ligado', FText.Caption);

  TagValueIs(0);
  AssertEquals('parado', 'parado', FText.Caption);
end;

procedure TTestHMIText.TheFirstMatchingZoneWins;
var
  primeira:TTextZone;
begin
  //duas zonas casando com o mesmo valor: vale a que vem antes na colecao
  primeira:=FText.Zones.Add;
  primeira.ZoneType:=ztGreaterThan;
  primeira.SetValues(0, 0);
  primeira.Text:='acima de zero';
  NewZone(5, 'exatamente cinco');
  FText.PLCTag:=FTag;

  TagValueIs(5);

  AssertEquals('a primeira da colecao', 'acima de zero', FText.Caption);
end;

procedure TTestHMIText.WithNoMatchAndNoDefaultTheCaptionIsEmpty;
begin
  NewZone(1, 'ligado');
  FText.PLCTag:=FTag;

  TagValueIs(99);

  AssertEquals('nenhuma zona serve', '', FText.Caption);
end;

procedure TTestHMIText.TheDefaultZoneAnswersWhenNothingElseMatches;
var
  padrao:TTextZone;
begin
  NewZone(1, 'ligado');
  padrao:=NewZone(0, 'estado invalido');
  padrao.DefaultZone:=true;
  FText.PLCTag:=FTag;

  TagValueIs(99);

  AssertEquals('caiu na padrao', 'estado invalido', FText.Caption);
end;

procedure TTestHMIText.WithNoTagTheDefaultZoneIsShown;
var
  padrao:TTextZone;
begin
  //sem tag ligado o controle mostra a zona padrao, que e onde se poe o
  //"sem comunicacao"
  padrao:=NewZone(0, 'sem comunicacao');
  padrao.DefaultZone:=true;

  FText.PLCTag:=nil;
  Settle;

  AssertEquals('a zona padrao', 'sem comunicacao', FText.Caption);
end;

procedure TTestHMIText.TheZoneBringsItsOwnColour;
var
  z:TTextZone;
begin
  z:=NewZone(1, 'alarme');
  z.Color:=clRed;
  z.Transparent:=false;
  FText.PLCTag:=FTag;

  TagValueIs(1);

  AssertEquals('a cor da zona', clRed, FText.Color);
end;

procedure TTestHMIText.TheZoneBringsItsOwnAlignment;
var
  z:TTextZone;
begin
  z:=NewZone(1, 'ligado');
  z.HorizontalAlignment:=taCenter;
  FText.PLCTag:=FTag;

  TagValueIs(1);

  AssertEquals('alinhamento da zona', Ord(taCenter), Ord(TLabel(FText).Alignment));
end;

procedure TTestHMIText.AnEmptyCaptionComesTransparent;
begin
  //sem zona escolhida o controle some do fundo em vez de deixar um retangulo
  NewZone(1, 'ligado');
  FText.PLCTag:=FTag;

  TagValueIs(99);

  AssertEquals('legenda vazia', '', FText.Caption);
  AssertTrue('e transparente', TLabel(FText).Transparent);
end;

procedure TTestHMIText.ThePrefixAndSuffixWrapTheZoneText;
begin
  //o texto da zona substitui o numero; prefixo e sufixo ficam em volta dele
  NewZone(1, 'ligado');
  FText.Prefix:='motor ';
  FText.Sufix:='!';
  FText.PLCTag:=FTag;

  TagValueIs(1);

  AssertEquals('prefixo e sufixo em volta', 'motor ligado!', FText.Caption);
end;

procedure TTestHMIText.ChangingTheZoneTextRebuildsTheCaption;
var
  z:TTextZone;
begin
  z:=NewZone(1, 'ligado');
  FText.PLCTag:=FTag;
  TagValueIs(1);
  AssertEquals('antes', 'ligado', FText.Caption);

  z.Text:='em marcha';
  Settle;

  AssertEquals('a legenda acompanhou', 'em marcha', FText.Caption);
end;

procedure TTestHMIText.ClearingTheTagFallsBackToNoZone;
begin
  //sem tag nao ha' valor, e sem valor nenhuma zona responde - a nao ser a
  //zona padrao, se houver uma
  //with no tag there is no value, and with no value no zone answers - other
  //than the default zone, if there is one
  NewZone(1, 'LIGADO');
  FText.PLCTag:=FTag;
  TagValueIs(1);
  AssertEquals('mostrando a zona', 'LIGADO', FText.Caption);

  FText.PLCTag:=nil;
  Settle;

  AssertEquals('sem tag, sem zona', '', FText.Caption);
end;

procedure TTestHMIText.ADestroyedTagLetsGoOfTheControl;
begin
  NewZone(1, 'ligado');
  FText.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('o controle largou o tag', FText.PLCTag=nil);
end;

initialization
  RegisterTest(TTestHMIText);

end.
