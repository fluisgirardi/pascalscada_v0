{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da THMIBandeja: o painel cuja cor vem do CLP.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Numa linha de separacao, cada bandeja na tela e' um painel que o CLP pinta:
  um tag diz a cor do fundo, outro a cor da borda, e um terceiro o texto que
  aparece nela. A cor vem como um numero inteiro no formato RRGGBB, do jeito
  que o CLP calculou.

  Do numero saem tres coisas: a cor em si, a luminancia - que decide se o texto
  fica preto ou branco, para dar contraste - e o zero, que e' a bandeja vazia e
  some da tela.
}
{$ELSE}
{:
  @abstract(THMIBandeja tests: the panel whose colour comes from the PLC.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  On a sorting line, each tray on the screen is a panel the PLC paints: one tag
  says the background colour, another the border colour, and a third the text
  shown on it. The colour arrives as an integer in RRGGBB form, the way the PLC
  worked it out.

  Three things come out of that number: the colour itself, the luminance -
  which decides whether the text is black or white, for contrast - and zero,
  which is the empty tray and disappears from the screen.
}
{$ENDIF}
unit ut.hmibandeja;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, fpcunit, testregistry,
  HMIBandeja, testsupport.faketag;

type

  { TTestHMIBandeja }

  TTestHMIBandeja = class(TTestCase)
  private
    FBandeja:THMIBandeja;
    FCorFundo, FCorBorda:TFakeNumber;
    FConversoes:LongInt;
    FCorFixa:TColor;
    FLumFixa:LongInt;
    function  MinhaConversao(Sender:TObject; const aColorDint:LongInt; out Lum:LongInt):TColor;
    procedure BackgroundIs(aDint:LongInt);
    procedure BorderIs(aDint:LongInt);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a cor do fundo / the background colour
    procedure TheBackgroundTagPaintsTheTray;
    procedure AnotherValuePaintsItAgain;
    procedure ADarkBackgroundAsksForWhiteText;
    procedure ALightBackgroundAsksForBlackText;
    procedure AnEmptyTrayDisappears;
    procedure APaintedTrayShowsUpAgain;

    //a cor da borda / the border colour
    procedure TheBorderTagPaintsTheBevel;
    procedure TheBorderTagDoesNotTouchTheBackground;

    //a conversao da aplicacao / the application's own conversion
    procedure TheApplicationCanDecideTheBackgroundColour;
    procedure TheApplicationsConversionAlsoServesTheBorder;

    //os tags / the tags
    procedure ADestroyedTagLetsGoOfTheTray;
    procedure ChangingTagsStopsTheOldOne;
  end;

implementation

const
  //cores no formato que o CLP manda: 0x00RRGGBB
  //colours the way the PLC sends them: 0x00RRGGBB
  VERMELHO = $00FF0000;
  BRANCO   = $00FFFFFF;
  ESCURO   = $00101010;
  VERDE    = $0000FF00;

{ TTestHMIBandeja }

procedure TTestHMIBandeja.SetUp;
begin
  FConversoes:=0;
  FCorFixa:=clFuchsia;
  FLumFixa:=200;
  FBandeja:=THMIBandeja.Create(nil);
  FCorFundo:=TFakeNumber.Create(nil);
  FCorBorda:=TFakeNumber.Create(nil);
  FBandeja.BackgroundColorPLCTag:=FCorFundo;
  FBandeja.BorderColorPLCTag:=FCorBorda;
end;

procedure TTestHMIBandeja.TearDown;
begin
  FreeAndNil(FBandeja);
  FreeAndNil(FCorFundo);
  FreeAndNil(FCorBorda);
end;

function TTestHMIBandeja.MinhaConversao(Sender:TObject; const aColorDint:LongInt; out Lum:LongInt):TColor;
begin
  inc(FConversoes);
  Lum:=FLumFixa;
  Result:=FCorFixa;
end;

procedure TTestHMIBandeja.Settle;
begin
  Application.ProcessMessages;
end;

procedure TTestHMIBandeja.BackgroundIs(aDint:LongInt);
begin
  FCorFundo.ChegouDoCLP(aDint);
  Settle;
end;

procedure TTestHMIBandeja.BorderIs(aDint:LongInt);
begin
  FCorBorda.ChegouDoCLP(aDint);
  Settle;
end;

procedure TTestHMIBandeja.TheBackgroundTagPaintsTheTray;
begin
  BackgroundIs(VERMELHO);

  AssertEquals('o fundo ficou vermelho', clRed, FBandeja.Color);
end;

procedure TTestHMIBandeja.AnotherValuePaintsItAgain;
begin
  BackgroundIs(VERMELHO);

  BackgroundIs(VERDE);

  AssertEquals('o fundo ficou verde', clLime, FBandeja.Color);
end;

procedure TTestHMIBandeja.ADarkBackgroundAsksForWhiteText;
begin
  //contraste: fundo escuro pede letra clara
  //contrast: a dark background asks for light lettering
  BackgroundIs(ESCURO);

  AssertEquals('letra branca', clWhite, FBandeja.Font.Color);
end;

procedure TTestHMIBandeja.ALightBackgroundAsksForBlackText;
begin
  BackgroundIs(BRANCO);

  AssertEquals('letra preta', clBlack, FBandeja.Font.Color);
end;

procedure TTestHMIBandeja.AnEmptyTrayDisappears;
begin
  //cor zero e' a bandeja que nao existe naquela posicao
  //colour zero is the tray that does not exist in that position
  BackgroundIs(VERMELHO);
  AssertTrue('aparecendo', FBandeja.Visible);

  BackgroundIs(0);

  AssertFalse('sumiu da tela', FBandeja.Visible);
end;

procedure TTestHMIBandeja.APaintedTrayShowsUpAgain;
begin
  BackgroundIs(0);
  AssertFalse('sumiu', FBandeja.Visible);

  BackgroundIs(VERDE);

  AssertTrue('voltou a aparecer', FBandeja.Visible);
end;

procedure TTestHMIBandeja.TheBorderTagPaintsTheBevel;
begin
  BorderIs(VERDE);

  AssertEquals('a borda ficou verde', clLime, FBandeja.BevelColor);
end;

procedure TTestHMIBandeja.TheBorderTagDoesNotTouchTheBackground;
begin
  //sao dois tags para duas coisas: a borda nao pode repintar o fundo
  //two tags for two things: the border must not repaint the background
  BackgroundIs(VERMELHO);

  BorderIs(VERDE);

  AssertEquals('o fundo continua vermelho', clRed, FBandeja.Color);
end;

procedure TTestHMIBandeja.TheApplicationCanDecideTheBackgroundColour;
begin
  //quem monta a tela pode ter a sua propria tabela de cores
  //whoever builds the screen may have a colour table of their own
  FBandeja.OnConvertDintToColor:=@MinhaConversao;

  BackgroundIs(VERMELHO);

  AssertEquals('a cor que a aplicacao escolheu', FCorFixa, FBandeja.Color);
  AssertTrue  ('a aplicacao foi consultada',     FConversoes>0);
end;

procedure TTestHMIBandeja.TheApplicationsConversionAlsoServesTheBorder;
begin
  //com conversao propria ou sem ela, o tag da borda pinta a borda
  //with a conversion of its own or without it, the border tag paints the
  //border
  FBandeja.OnConvertDintToColor:=@MinhaConversao;
  BackgroundIs(VERMELHO);

  BorderIs(VERDE);

  AssertEquals('a borda usou a cor da aplicacao', FCorFixa, FBandeja.BevelColor);
  AssertEquals('e o fundo ficou como estava',     FCorFixa, FBandeja.Color);
end;

procedure TTestHMIBandeja.ADestroyedTagLetsGoOfTheTray;
begin
  BackgroundIs(VERMELHO);

  FreeAndNil(FCorFundo);

  AssertTrue('a bandeja soltou o tag', FBandeja.BackgroundColorPLCTag=nil);
end;

procedure TTestHMIBandeja.ChangingTagsStopsTheOldOne;
var
  outro:TFakeNumber;
begin
  //comparar a cor nao serve: o tratador le sempre o tag atual, entao mesmo
  //avisado pelo tag velho ele repinta com o valor do novo. O que distingue e'
  //se ele foi chamado - e a conversao da aplicacao conta as chamadas.
  //comparing the colour is no use: the handler always reads the current tag, so
  //even when told by the old tag it repaints with the new one's value. What
  //tells them apart is whether it ran at all - and the application's conversion
  //counts the calls.
  outro:=TFakeNumber.Create(nil);
  try
    FBandeja.OnConvertDintToColor:=@MinhaConversao;
    FBandeja.BackgroundColorPLCTag:=outro;
    FConversoes:=0;

    BackgroundIs(VERDE);

    AssertEquals('o tag antigo nao avisa mais', 0, FConversoes);
  finally
    outro.Free;
  end;
end;

initialization
  RegisterTest(TTestHMIBandeja);

end.
