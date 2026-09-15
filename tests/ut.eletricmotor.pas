{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIBasicEletricMotor: o motor desenhado, com ou sem a
            bomba, deitado ou em pe', espelhado ou nao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O motor nao tem tag nem estado: e' um desenho. O que se testa e' o desenho
  mesmo - o bitmap que o controle monta antes de pintar na tela. A bomba fica
  na ponta esquerda; espelhar leva a bomba para a direita; um controle mais
  alto do que largo desenha o motor deitado e depois gira, e a bomba vai parar
  embaixo - ou em cima, se espelhado.

  Os pixels sao lidos direto do bitmap, em pontos que ficam bem dentro das
  formas, longe das bordas suavizadas.
}
{$ELSE}
{:
  @abstract(THMIBasicEletricMotor tests: the drawn motor, with or without the
            pump, lying down or standing, mirrored or not.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The motor has no tag and no state: it is a drawing. What is tested is the
  drawing itself - the bitmap the control builds before painting it on screen.
  The pump sits on the left end; mirroring takes it to the right; a control
  taller than wide draws the motor lying down and then rotates it, and the
  pump ends up at the bottom - or at the top, when mirrored.

  Pixels are read straight from the bitmap, at points that lie well inside
  the shapes, away from the antialiased edges.
}
{$ENDIF}
unit ut.eletricmotor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, fpcunit, testregistry,
  BGRABitmap, BGRABitmapTypes, HMIBasicEletricMotor;

type

  { TMotorProbe }

  //DrawControl e FControlArea sao protegidos: e' o Paint que desenha e usa. A
  //sonda desenha sem janela e deixa ler o bitmap.
  //
  //DrawControl and FControlArea are protected: Paint is what draws and uses
  //them. The probe draws with no window and lets the bitmap be read.
  TMotorProbe = class(THMIBasicEletricMotor)
  public
    procedure Draw;
    function  Area:TBGRABitmap;
    function  ShapeIsStale:Boolean;
    procedure ShapeIsFresh;
  end;

  { TTestEletricMotor }

  TTestEletricMotor = class(TTestCase)
  private
    FMotor:TMotorProbe;
    function  PixelAt(x, y:Integer):TBGRAPixel;
    function  IsBody(x, y:Integer):Boolean;
    function  IsEmpty(x, y:Integer):Boolean;
    function  IsBorder(x, y:Integer):Boolean;
    function  HasBorderColouredPixels:Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o que vem de fabrica / what comes from the factory
    procedure ANewMotorDrawsThePumpAndIsNotMirrored;

    //o bitmap / the bitmap
    procedure TheDrawingHasTheSizeOfTheControl;
    procedure AVerticalDrawingHasTheSizeOfTheControl;
    procedure TheBodyIsPaintedWithTheBodyColour;
    procedure TheBorderIsPaintedWithTheBorderColour;
    procedure TheDividingLineIsPaintedWithTheBorderColour;
    procedure AboveTheBodyThereIsNothing;

    //a bomba / the pump
    procedure ThePumpIsDrawnOnTheLeft;
    procedure ThePumpBaseIsPaintedWithTheBodyColour;
    procedure WithoutThePumpTheLeftEndIsEmpty;

    //espelhado / mirrored
    procedure MirroringMovesThePumpToTheRight;
    procedure MirroringIsAHorizontalFlip;

    //em pe' / standing
    procedure AVerticalMotorHasThePumpAtTheBottom;
    procedure AMirroredVerticalMotorHasThePumpAtTheTop;

    //redesenho / redraw
    procedure ChangingMirroredAsksForANewShape;
    procedure ChangingDrawPumpAsksForANewShape;
    procedure SettingTheSameValueAsksForNothing;

    //tamanhos / sizes
    procedure TinySizesDoNotBlowUp;
  end;

implementation

{ TMotorProbe }

procedure TMotorProbe.Draw;
begin
  DrawControl;
end;

function TMotorProbe.Area:TBGRABitmap;
begin
  Result:=FControlArea;
end;

function TMotorProbe.ShapeIsStale:Boolean;
begin
  Result:=FUpdateShape;
end;

procedure TMotorProbe.ShapeIsFresh;
begin
  FUpdateShape:=false;
end;

{ TTestEletricMotor }

procedure TTestEletricMotor.SetUp;
begin
  FMotor:=TMotorProbe.Create(nil);
  //deitado, 200 por 100: as fracoes do desenho caem em pixels inteiros
  //lying down, 200 by 100: the drawing's fractions land on whole pixels
  FMotor.SetBounds(0,0,200,100);
  FMotor.BodyColor:=clRed;
  FMotor.BorderColor:=clBlue;
end;

procedure TTestEletricMotor.TearDown;
begin
  FreeAndNil(FMotor);
end;

function TTestEletricMotor.PixelAt(x, y:Integer):TBGRAPixel;
begin
  Result:=FMotor.Area.GetPixel(x,y);
end;

function TTestEletricMotor.IsBody(x, y:Integer):Boolean;
var
  p:TBGRAPixel;
begin
  p:=PixelAt(x,y);
  Result:=(p.alpha=255) and (p.red=255) and (p.green=0) and (p.blue=0);
end;

function TTestEletricMotor.IsEmpty(x, y:Integer):Boolean;
begin
  Result:=PixelAt(x,y).alpha=0;
end;

function TTestEletricMotor.IsBorder(x, y:Integer):Boolean;
var
  p:TBGRAPixel;
begin
  p:=PixelAt(x,y);
  Result:=(p.alpha=255) and (p.red=0) and (p.green=0) and (p.blue=255);
end;

function TTestEletricMotor.HasBorderColouredPixels:Boolean;
var
  x, y:Integer;
  p:TBGRAPixel;
begin
  Result:=false;
  for y:=0 to FMotor.Area.Height-1 do
    for x:=0 to FMotor.Area.Width-1 do begin
      p:=FMotor.Area.GetPixel(x,y);
      if (p.alpha=255) and (p.red=0) and (p.green=0) and (p.blue=255) then
        exit(true);
    end;
end;

procedure TTestEletricMotor.ANewMotorDrawsThePumpAndIsNotMirrored;
begin
  AssertTrue ('com bomba',      FMotor.DrawPump);
  AssertFalse('nao espelhado',  FMotor.Mirrored);
end;

procedure TTestEletricMotor.TheDrawingHasTheSizeOfTheControl;
begin
  FMotor.Draw;

  AssertEquals('largura', 200, FMotor.Area.Width);
  AssertEquals('altura',  100, FMotor.Area.Height);
end;

procedure TTestEletricMotor.AVerticalDrawingHasTheSizeOfTheControl;
begin
  //o desenho e' feito deitado e girado; girado tem que caber no controle
  //the drawing is made lying down and rotated; rotated it has to fit the
  //control
  FMotor.SetBounds(0,0,100,200);

  FMotor.Draw;

  AssertEquals('largura', 100, FMotor.Area.Width);
  AssertEquals('altura',  200, FMotor.Area.Height);
end;

procedure TTestEletricMotor.TheBodyIsPaintedWithTheBodyColour;
begin
  FMotor.Draw;

  //o meio da caixa de ligacao, bem dentro do corpo
  //the middle of the terminal box, well inside the body
  AssertTrue('corpo vermelho', IsBody(120,50));
end;

procedure TTestEletricMotor.TheBorderIsPaintedWithTheBorderColour;
begin
  FMotor.Draw;

  AssertTrue('ha'' contorno azul', HasBorderColouredPixels);
end;

procedure TTestEletricMotor.TheDividingLineIsPaintedWithTheBorderColour;
begin
  //o risco vertical a 81% da largura, menos a borda: e' o unico traco que nao
  //e' suavizado, entao o pixel tem a cor exata
  //the vertical stroke at 81% of the width, minus the border: the one stroke
  //that is not antialiased, so the pixel carries the exact colour
  FMotor.Draw;

  AssertTrue('risco azul', IsBorder(161,50));
end;

procedure TTestEletricMotor.AboveTheBodyThereIsNothing;
begin
  //o corpo comeca em 6% da altura; acima disso o fundo aparece
  //the body starts at 6% of the height; above that the background shows
  FMotor.Draw;

  AssertTrue('nada acima do corpo', IsEmpty(120,2));
end;

procedure TTestEletricMotor.ThePumpIsDrawnOnTheLeft;
begin
  FMotor.Draw;

  //o bocal da bomba: de 7% a 16% da largura, do alto ate 10% da altura
  //the pump's nozzle: from 7% to 16% of the width, from the top to 10% of
  //the height
  AssertTrue('bocal a esquerda', IsBody(22,5));
end;

procedure TTestEletricMotor.ThePumpBaseIsPaintedWithTheBodyColour;
begin
  //a base da bomba: da borda ate 7% da largura, entre 38% e 62% da altura
  //the pump's base: from the border to 7% of the width, between 38% and 62%
  //of the height
  FMotor.Draw;

  AssertTrue('base vermelha', IsBody(5,50));
end;

procedure TTestEletricMotor.WithoutThePumpTheLeftEndIsEmpty;
begin
  FMotor.DrawPump:=false;

  FMotor.Draw;

  AssertTrue('sem bocal', IsEmpty(22,5));
end;

procedure TTestEletricMotor.MirroringMovesThePumpToTheRight;
begin
  FMotor.Mirrored:=true;

  FMotor.Draw;

  AssertTrue ('bocal a direita',   IsBody(199-22,5));
  AssertTrue ('nada a esquerda',   IsEmpty(22,5));
end;

procedure TTestEletricMotor.MirroringIsAHorizontalFlip;
var
  normal:TBGRABitmap;
  x, y:Integer;
  a, b:TBGRAPixel;
begin
  //espelhar e' so' isso: nenhum pixel muda de cor, so' de lado
  //mirroring is just that: no pixel changes colour, only side
  FMotor.Draw;
  normal:=TBGRABitmap.Create;
  try
    normal.Assign(FMotor.Area);
    FMotor.Mirrored:=true;
    FMotor.Draw;

    for y:=0 to normal.Height-1 do
      for x:=0 to normal.Width-1 do begin
        a:=normal.GetPixel(x,y);
        b:=FMotor.Area.GetPixel(normal.Width-1-x,y);
        if (a.alpha<>b.alpha) or (a.red<>b.red) or (a.green<>b.green) or (a.blue<>b.blue) then
          Fail(Format('o pixel (%d,%d) nao bate com o seu espelho',[x,y]));
      end;
  finally
    normal.Free;
  end;
end;

procedure TTestEletricMotor.AVerticalMotorHasThePumpAtTheBottom;
begin
  //deitado, o bocal esta' em (22,5); girado no sentido anti-horario o
  //pixel (x,y) vai para (y, largura-1-x)
  //lying down, the nozzle is at (22,5); rotated counter-clockwise the pixel
  //(x,y) goes to (y, width-1-x)
  FMotor.SetBounds(0,0,100,200);

  FMotor.Draw;

  AssertTrue('bocal embaixo', IsBody(5,199-22));
  AssertTrue('nada em cima',  IsEmpty(5,22));
end;

procedure TTestEletricMotor.AMirroredVerticalMotorHasThePumpAtTheTop;
begin
  //girado no sentido horario o pixel (x,y) vai para (altura-1-y, x)
  //rotated clockwise the pixel (x,y) goes to (height-1-y, x)
  FMotor.SetBounds(0,0,100,200);
  FMotor.Mirrored:=true;

  FMotor.Draw;

  AssertTrue('bocal em cima',  IsBody(99-5,22));
  AssertTrue('nada embaixo',   IsEmpty(99-5,199-22));
end;

procedure TTestEletricMotor.ChangingMirroredAsksForANewShape;
begin
  FMotor.ShapeIsFresh;

  FMotor.Mirrored:=true;

  AssertTrue('forma para refazer', FMotor.ShapeIsStale);
end;

procedure TTestEletricMotor.ChangingDrawPumpAsksForANewShape;
begin
  FMotor.ShapeIsFresh;

  FMotor.DrawPump:=false;

  AssertTrue('forma para refazer', FMotor.ShapeIsStale);
end;

procedure TTestEletricMotor.SettingTheSameValueAsksForNothing;
begin
  FMotor.ShapeIsFresh;

  FMotor.Mirrored:=false;
  FMotor.DrawPump:=true;

  AssertFalse('nada a refazer', FMotor.ShapeIsStale);
end;

procedure TTestEletricMotor.TinySizesDoNotBlowUp;
begin
  //as fracoes viram zero e os retangulos se invertem; nada disso pode
  //estourar
  //the fractions turn into zero and the rectangles invert; none of that may
  //blow up
  FMotor.SetBounds(0,0,0,0);
  FMotor.Draw;
  FMotor.SetBounds(0,0,1,1);
  FMotor.Draw;
  FMotor.SetBounds(0,0,3,2);
  FMotor.Draw;
  FMotor.SetBounds(0,0,2,3);
  FMotor.Mirrored:=true;
  FMotor.Draw;

  AssertEquals('desenhou no tamanho pedido', 3, FMotor.Area.Height);
end;

initialization
  RegisterTest(TTestEletricMotor);

end.
