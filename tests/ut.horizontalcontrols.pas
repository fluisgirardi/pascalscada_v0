{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes dos transportadores horizontais: a fita, o redler e a
            rosca.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os tres sao desenhos de tubulacao horizontal que carregam produto, e os tres
  saem do mesmo tronco: um controle cuja altura e' a do corpo desenhado, e nao
  qualquer altura que o usuario arraste no formulario. E' por isso que mexer
  em BodyHeight mexe na altura do controle e nos limites de tamanho dele.
}
{$ELSE}
{:
  @abstract(Horizontal conveyor tests: the belt, the redler and the screw.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  All three are drawings of horizontal piping carrying product, and all three
  come from the same trunk: a control whose height is the height of the drawn
  body, not whatever height the user drags on the form. That is why touching
  BodyHeight touches the control's height and its size limits.
}
{$ENDIF}
unit ut.horizontalcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, fpcunit, testregistry,
  hmi_draw_basic_horizontal_control, hmi_draw_fita, hmi_draw_redler,
  hmi_draw_rosca;

type

  { TFitaProbe }

  //BodyHeight e' publicada na fita e no redler, mas nao na rosca; DrawControl
  //e InvalidateDraw sao protegidos em todos. As sondas expoem o que os testes
  //precisam ver.
  //
  //BodyHeight is published on the belt and the redler but not on the screw;
  //DrawControl and InvalidateDraw are protected on all of them. The probes
  //expose what the tests need to see.
  TFitaProbe = class(THMIFitaBasica)
  private
    FRedraws:LongInt;
  protected
    procedure InvalidateDraw; override;
  public
    procedure DrawIt;
    procedure ForgetRedraws;
    function  DrawnWidth:Integer;
    function  DrawnHeight:Integer;
    property Redraws:LongInt read FRedraws;
  end;

  { TRedlerProbe }

  TRedlerProbe = class(THMIRedlerBasico)
  public
    procedure DrawIt;
    procedure SetTheBorderWidth(v:Integer);
  end;

  { TRoscaProbe }

  TRoscaProbe = class(THMIRoscaBasica)
  public
    procedure DrawIt;
  end;

  { TTestHorizontalControls }

  TTestHorizontalControls = class(TTestCase)
  private
    FForm:TForm;
    FFita:TFitaProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o corpo / the body
    procedure ANewControlIsSilverWithABlackBorder;
    procedure TheBodyHeightBecomesTheControlHeight;
    procedure TheBodyHeightFixesTheHeightOfTheControl;
    procedure TheBodyHeightSetsTheSmallestUsefulWidth;
    procedure ANarrowControlGrowsToTheSmallestWidth;
    procedure ABodyHeightBelowFiveIsRefused;

    //as cores / the colours
    procedure ChangingTheBodyColourRedraws;
    procedure SettingTheSameBodyColourDoesNotRedraw;

    //o desenho / the drawing
    procedure TheBeltDrawsWithoutComplaining;
    procedure TheRedlerDrawsWithoutComplaining;
    procedure TheScrewDrawsWithoutComplaining;
    procedure TheRedlerRefusesABodyTooShortForItsPaddles;
    procedure TheScrewKeepsItsOwnBodyHeight;
  end;

implementation

{ TFitaProbe }

procedure TFitaProbe.InvalidateDraw;
begin
  inc(FRedraws);
  inherited InvalidateDraw;
end;

procedure TFitaProbe.DrawIt;
begin
  DrawControl;
end;

procedure TFitaProbe.ForgetRedraws;
begin
  FRedraws:=0;
end;

function TFitaProbe.DrawnWidth:Integer;
begin
  Result:=FControlArea.Width;
end;

function TFitaProbe.DrawnHeight:Integer;
begin
  Result:=FControlArea.Height;
end;

{ TRedlerProbe }

procedure TRedlerProbe.DrawIt;
begin
  DrawControl;
end;

procedure TRedlerProbe.SetTheBorderWidth(v:Integer);
begin
  BorderWidth:=v;
end;

{ TRoscaProbe }

procedure TRoscaProbe.DrawIt;
begin
  DrawControl;
end;

{ TTestHorizontalControls }

procedure TTestHorizontalControls.SetUp;
begin
  //desenhar precisa de janela mae
  //painting needs a parent window
  FForm:=TForm.CreateNew(nil);
  FForm.Visible:=false;
  FForm.SetBounds(0,0,400,300);
  FFita:=TFitaProbe.Create(FForm);
  FFita.Parent:=FForm;
  FFita.SetBounds(0,0,200,12);
end;

procedure TTestHorizontalControls.TearDown;
begin
  FreeAndNil(FForm);
end;

procedure TTestHorizontalControls.ANewControlIsSilverWithABlackBorder;
begin
  AssertEquals('corpo',  clSilver, FFita.BodyColor);
  AssertEquals('borda',  clBlack,  FFita.BorderColor);
  AssertEquals('altura do corpo', 12, FFita.BodyHeight);
end;

procedure TTestHorizontalControls.TheBodyHeightBecomesTheControlHeight;
begin
  //a altura do controle nao e' livre: ela e' a do desenho
  //the control's height is not free: it is the drawing's
  FFita.BodyHeight:=20;

  AssertEquals('altura do controle', 20, FFita.Height);
end;

procedure TTestHorizontalControls.TheBodyHeightFixesTheHeightOfTheControl;
begin
  FFita.BodyHeight:=20;

  FFita.Height:=100;

  AssertEquals('continua na altura do corpo', 20, FFita.Height);
end;

procedure TTestHorizontalControls.TheBodyHeightSetsTheSmallestUsefulWidth;
begin
  //menos do que isso e o desenho nao cabe: sao os dois circulos da fita mais
  //o trecho reto entre eles
  //any narrower and the drawing does not fit: the belt's two circles plus the
  //straight run between them
  FFita.BodyHeight:=20;

  AssertEquals('largura minima', 20*2+3, FFita.Constraints.MinWidth);
end;

procedure TTestHorizontalControls.ANarrowControlGrowsToTheSmallestWidth;
begin
  FFita.SetBounds(0,0,10,12);

  FFita.BodyHeight:=20;

  AssertTrue('a largura acompanhou o minimo', FFita.Width>=FFita.Constraints.MinWidth);
end;

procedure TTestHorizontalControls.ABodyHeightBelowFiveIsRefused;
begin
  FFita.BodyHeight:=20;

  FFita.BodyHeight:=4;

  AssertEquals('continua na altura boa', 20, FFita.BodyHeight);
end;

procedure TTestHorizontalControls.ChangingTheBodyColourRedraws;
begin
  FFita.ForgetRedraws;

  FFita.BodyColor:=clLime;

  AssertTrue('pediu redesenho', FFita.Redraws>0);
end;

procedure TTestHorizontalControls.SettingTheSameBodyColourDoesNotRedraw;
begin
  FFita.BodyColor:=clLime;
  FFita.ForgetRedraws;

  FFita.BodyColor:=clLime;

  AssertEquals('nada mudou, nada a redesenhar', 0, FFita.Redraws);
end;

procedure TTestHorizontalControls.TheBeltDrawsWithoutComplaining;
begin
  FFita.BodyHeight:=16;

  FFita.DrawIt;

  AssertEquals('desenhou na largura do controle', FFita.Width,  FFita.DrawnWidth);
  AssertEquals('e na altura do controle',         FFita.Height, FFita.DrawnHeight);
end;

procedure TTestHorizontalControls.TheRedlerDrawsWithoutComplaining;
var
  redler:TRedlerProbe;
begin
  redler:=TRedlerProbe.Create(FForm);
  try
    redler.Parent:=FForm;
    redler.SetBounds(0,20,200,12);
    redler.BodyHeight:=16;

    redler.DrawIt;

    AssertEquals('altura do corpo', 16, redler.BodyHeight);
  finally
    redler.Free;
  end;
end;

procedure TTestHorizontalControls.TheScrewDrawsWithoutComplaining;
var
  rosca:TRoscaProbe;
begin
  rosca:=TRoscaProbe.Create(FForm);
  try
    rosca.Parent:=FForm;
    rosca.SetBounds(0,40,200,12);

    rosca.DrawIt;

    AssertEquals('altura do corpo', 12, rosca.BodyHeight);
  finally
    rosca.Free;
  end;
end;

procedure TTestHorizontalControls.TheRedlerRefusesABodyTooShortForItsPaddles;
var
  redler:TRedlerProbe;
begin
  //o redler precisa de espaco para as duas fileiras de pas e para o eixo
  //central, e esse espaco depende da espessura da borda: com borda de 3 o
  //minimo sobe para 9, uma altura que o tronco comum aceitaria.
  //the redler needs room for the two rows of paddles and for the central
  //shaft, and that room depends on the border width: with a border of 3 the
  //minimum goes up to 9, a height the common trunk would accept.
  redler:=TRedlerProbe.Create(FForm);
  try
    redler.Parent:=FForm;
    redler.BodyHeight:=16;
    redler.SetTheBorderWidth(3);

    redler.BodyHeight:=8;

    AssertEquals('continua na altura boa', 16, redler.BodyHeight);

    redler.BodyHeight:=9;

    AssertEquals('e aceita o menor que cabe', 9, redler.BodyHeight);
  finally
    redler.Free;
  end;
end;

procedure TTestHorizontalControls.TheScrewKeepsItsOwnBodyHeight;
var
  rosca:THMIRoscaBasica;
begin
  //pela propriedade publicada, como o formulario faz: a espessura da rosca
  //nao pode ficar presa no valor do construtor
  //through the published property, the way the form does it: the screw's
  //thickness must not stay stuck at the constructor's value
  rosca:=THMIRoscaBasica.Create(FForm);
  try
    rosca.Parent:=FForm;

    rosca.BodyHeight:=24;

    AssertEquals('altura do corpo', 24, rosca.BodyHeight);
    AssertEquals('altura do controle', 24, rosca.Height);
  finally
    rosca.Free;
  end;
end;

initialization
  RegisterTest(TTestHorizontalControls);

end.
