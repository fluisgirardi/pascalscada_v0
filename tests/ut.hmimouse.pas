{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do mouse nos controles desenhados: o clique que cai no
            desenho e o clique que cai no vazio.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um controle desenhado ocupa um retangulo, mas o desenho quase nunca o
  preenche: uma valvula e' um losango, uma tubulacao e' uma linha. O que esta'
  fora do desenho e' transparente, e um clique ali nao e' do controle - e' de
  quem esta' atras dele: outro controle desenhado, um painel com coisas
  dentro, ou o proprio formulario.

  E' a base de todo sinotico com controles sobrepostos, e mora no
  THMIBasicControl: o teste de acerto pelo pixel do bitmap, e o repasse da
  mensagem do mouse para quem estiver por baixo.
}
{$ELSE}
{:
  @abstract(Mouse tests on the drawn controls: the click that lands on the
            drawing and the click that lands on nothing.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A drawn control occupies a rectangle, but the drawing hardly ever fills it:
  a valve is a diamond, a pipe is a line. What lies outside the drawing is
  transparent, and a click there is not the control's - it belongs to whoever
  is behind it: another drawn control, a panel with things inside, or the
  form itself.

  It is the base of every synoptic with overlapping controls, and lives in
  THMIBasicControl: the hit test by the bitmap's pixel, and the forwarding
  of the mouse message to whoever is underneath.
}
{$ENDIF}
unit ut.hmimouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls, Graphics, LMessages, LCLType,
  fpcunit, testregistry,
  BGRABitmap, BGRABitmapTypes, hmi_draw_basiccontrol;

type

  { TShapeProbe }

  //desenha um retangulo opaco na metade esquerda e deixa a direita vazia;
  //conta o que o mouse lhe entregou
  //draws an opaque rectangle on the left half and leaves the right one empty;
  //counts what the mouse delivered to it
  TShapeProbe = class(THMIBasicControl)
  private
    FFillAll:Boolean;
  protected
    procedure DrawControl; override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift:TShiftState; X, Y:Integer); override;
    procedure Click; override;
  public
    Downs, Ups, Moves, Clicks:LongInt;
    LastButton:TMouseButton;
    LastX, LastY:Integer;
    procedure Draw;
    function  Hit(X, Y:Integer):Boolean;
    property  FillAll:Boolean read FFillAll write FFillAll;
  end;

  { TTestHMIMouse }

  TTestHMIMouse = class(TTestCase)
  private
    FForm:TForm;
    FFront, FBack:TShapeProbe;
    FFormDowns:LongInt;
    FFormX, FFormY:Integer;
    procedure FormMouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X, Y:Integer);
    function  NewShape(aLeft, aTop, aWidth, aHeight:Integer; aParent:TWinControl):TShapeProbe;
    procedure Press(aOn:TControl; aMsg:Cardinal; X, Y:Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o teste de acerto / the hit test
    procedure ThePaintedPixelIsControlArea;
    procedure TheEmptyPixelIsNot;
    procedure OutsideTheBoundsIsNot;
    procedure TheHitTestMessageAnswersTheSame;

    //o clique no desenho / the click on the drawing
    procedure AClickOnTheDrawingIsTheControls;
    procedure AClickOnTheDrawingBecomesAClick;
    procedure ARightClickOnTheDrawingIsTheControls;
    procedure AMoveOverTheDrawingIsTheControls;

    //o clique no vazio / the click on nothing
    procedure AClickOnNothingGoesToTheControlBehind;
    procedure TheForwardedClickCarriesTheOtherControlsCoordinates;
    procedure AClickOnNothingWithNothingBehindGoesToTheForm;
    procedure AClickOnNothingGoesIntoAPanelBehind;
    procedure AMoveOverNothingGoesToTheControlBehind;
    procedure AfterForwardingTheControlIsEnabledAgain;
    procedure AForwardedClickDoesNotBecomeTheControlsClick;
  end;

implementation

{ TShapeProbe }

procedure TShapeProbe.DrawControl;
var
  w:Integer;
begin
  inherited DrawControl;
  if FFillAll then
    w:=Width
  else
    w:=Width div 2;
  FControlArea.FillRect(0, 0, w, Height, ColorToBGRA(clRed), dmSet);
end;

procedure TShapeProbe.MouseDown(Button:TMouseButton; Shift:TShiftState; X, Y:Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  inc(Downs);
  LastButton:=Button;
  LastX:=X;
  LastY:=Y;
end;

procedure TShapeProbe.MouseUp(Button:TMouseButton; Shift:TShiftState; X, Y:Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  inc(Ups);
end;

procedure TShapeProbe.MouseMove(Shift:TShiftState; X, Y:Integer);
begin
  inherited MouseMove(Shift, X, Y);
  inc(Moves);
  LastX:=X;
  LastY:=Y;
end;

procedure TShapeProbe.Click;
begin
  inherited Click;
  inc(Clicks);
end;

procedure TShapeProbe.Draw;
begin
  DrawControl;
end;

function TShapeProbe.Hit(X, Y:Integer):Boolean;
begin
  Result:=IsControlArea(X, Y);
end;

{ TTestHMIMouse }

procedure TTestHMIMouse.SetUp;
begin
  FFormDowns:=0;
  FForm:=TForm.CreateNew(nil);
  FForm.SetBounds(0, 0, 400, 300);
  FForm.OnMouseDown:=@FormMouseDown;
  FFront:=nil;
  FBack:=nil;
end;

procedure TTestHMIMouse.TearDown;
begin
  //os controles sao do formulario / the controls belong to the form
  FreeAndNil(FForm);
end;

procedure TTestHMIMouse.FormMouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X, Y:Integer);
begin
  inc(FFormDowns);
  FFormX:=X;
  FFormY:=Y;
end;

function TTestHMIMouse.NewShape(aLeft, aTop, aWidth, aHeight:Integer; aParent:TWinControl):TShapeProbe;
begin
  Result:=TShapeProbe.Create(FForm);
  Result.SetBounds(aLeft, aTop, aWidth, aHeight);
  Result.Parent:=aParent;
  Result.Draw;
end;

procedure TTestHMIMouse.Press(aOn:TControl; aMsg:Cardinal; X, Y:Integer);
begin
  //a mensagem do widget: as coordenadas vao no lParam, x embaixo e y em cima
  //the widget's message: the coordinates go in lParam, x low and y high
  aOn.Perform(aMsg, 0, LPARAM(X and $FFFF) or (LPARAM(Y and $FFFF) shl 16));
end;

procedure TTestHMIMouse.ThePaintedPixelIsControlArea;
begin
  FFront:=NewShape(0, 0, 200, 100, FForm);

  AssertTrue('na metade pintada', FFront.Hit(50, 50));
end;

procedure TTestHMIMouse.TheEmptyPixelIsNot;
begin
  FFront:=NewShape(0, 0, 200, 100, FForm);

  AssertFalse('na metade vazia', FFront.Hit(150, 50));
end;

procedure TTestHMIMouse.OutsideTheBoundsIsNot;
begin
  FFront:=NewShape(0, 0, 200, 100, FForm);

  AssertFalse('a esquerda', FFront.Hit(-1, 50));
  AssertFalse('acima',      FFront.Hit(50, -1));
  AssertFalse('a direita',  FFront.Hit(200, 50));
  AssertFalse('abaixo',     FFront.Hit(50, 100));
end;

procedure TTestHMIMouse.TheHitTestMessageAnswersTheSame;
begin
  FFront:=NewShape(0, 0, 200, 100, FForm);

  AssertEquals('no desenho', 1, FFront.Perform(CM_HITTEST, 0, LPARAM(50) or (LPARAM(50) shl 16)));
  AssertEquals('no vazio',   0, FFront.Perform(CM_HITTEST, 0, LPARAM(150) or (LPARAM(50) shl 16)));
end;

procedure TTestHMIMouse.AClickOnTheDrawingIsTheControls;
begin
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_LBUTTONDOWN, 50, 40);

  AssertEquals('um mouse down',   1, FFront.Downs);
  AssertEquals('botao esquerdo',  Ord(mbLeft), Ord(FFront.LastButton));
  AssertEquals('x',               50, FFront.LastX);
  AssertEquals('y',               40, FFront.LastY);
  AssertEquals('e o formulario nao viu nada', 0, FFormDowns);
end;

procedure TTestHMIMouse.AClickOnTheDrawingBecomesAClick;
begin
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_LBUTTONDOWN, 50, 40);
  Press(FFront, LM_LBUTTONUP,   50, 40);

  AssertEquals('um mouse up', 1, FFront.Ups);
  AssertEquals('um clique',   1, FFront.Clicks);
end;

procedure TTestHMIMouse.ARightClickOnTheDrawingIsTheControls;
begin
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_RBUTTONDOWN, 50, 40);

  AssertEquals('um mouse down',  1, FFront.Downs);
  AssertEquals('botao direito',  Ord(mbRight), Ord(FFront.LastButton));
end;

procedure TTestHMIMouse.AMoveOverTheDrawingIsTheControls;
begin
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_MOUSEMOVE, 50, 40);

  AssertEquals('um mouse move', 1, FFront.Moves);
  AssertEquals('x', 50, FFront.LastX);
end;

procedure TTestHMIMouse.AClickOnNothingGoesToTheControlBehind;
begin
  //o de tras cobre a area inteira; o da frente, so' a metade esquerda
  //the one behind covers the whole area; the one in front, only the left half
  FBack:=NewShape(0, 0, 200, 100, FForm);
  FBack.FillAll:=true;
  FBack.Draw;
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_LBUTTONDOWN, 150, 40);

  AssertEquals('o da frente nao viu',  0, FFront.Downs);
  AssertEquals('o de tras recebeu',    1, FBack.Downs);
end;

procedure TTestHMIMouse.TheForwardedClickCarriesTheOtherControlsCoordinates;
begin
  //o de tras esta' deslocado: o ponto tem que chegar nas coordenadas dele
  //the one behind is offset: the point has to arrive in its coordinates
  FBack:=NewShape(100, 20, 200, 100, FForm);
  FBack.FillAll:=true;
  FBack.Draw;
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_LBUTTONDOWN, 150, 40);

  AssertEquals('o de tras recebeu', 1,  FBack.Downs);
  AssertEquals('x nele',            50, FBack.LastX);
  AssertEquals('y nele',            20, FBack.LastY);
end;

procedure TTestHMIMouse.AClickOnNothingWithNothingBehindGoesToTheForm;
begin
  FFront:=NewShape(10, 10, 200, 100, FForm);

  Press(FFront, LM_LBUTTONDOWN, 150, 40);

  AssertEquals('o formulario recebeu', 1,   FFormDowns);
  AssertEquals('x no formulario',      160, FFormX);
  AssertEquals('y no formulario',      50,  FFormY);
end;

procedure TTestHMIMouse.AClickOnNothingGoesIntoAPanelBehind;
var
  painel:TPanel;
begin
  //atras ha' um painel com um controle desenhado dentro: o clique tem que
  //descer ate' ele
  //behind there is a panel with a drawn control inside: the click has to go
  //all the way down to it
  painel:=TPanel.Create(FForm);
  painel.SetBounds(100, 0, 200, 100);
  painel.Parent:=FForm;
  FBack:=NewShape(0, 0, 200, 100, painel);
  FBack.FillAll:=true;
  FBack.Draw;
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_LBUTTONDOWN, 150, 40);

  AssertEquals('o de dentro do painel recebeu', 1,  FBack.Downs);
  AssertEquals('x nele',                        50, FBack.LastX);
end;

procedure TTestHMIMouse.AMoveOverNothingGoesToTheControlBehind;
begin
  FBack:=NewShape(0, 0, 200, 100, FForm);
  FBack.FillAll:=true;
  FBack.Draw;
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_MOUSEMOVE, 150, 40);

  AssertEquals('o da frente nao viu', 0, FFront.Moves);
  AssertEquals('o de tras recebeu',   1, FBack.Moves);
end;

procedure TTestHMIMouse.AfterForwardingTheControlIsEnabledAgain;
begin
  //para o de tras ser achado, o da frente se desabilita por um instante;
  //tem que voltar como estava
  //for the one behind to be found, the one in front disables itself for an
  //instant; it has to come back as it was
  FBack:=NewShape(0, 0, 200, 100, FForm);
  FBack.FillAll:=true;
  FBack.Draw;
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_LBUTTONDOWN, 150, 40);

  AssertTrue('habilitado de novo', FFront.Enabled);
end;

procedure TTestHMIMouse.AForwardedClickDoesNotBecomeTheControlsClick;
begin
  FBack:=NewShape(0, 0, 200, 100, FForm);
  FBack.FillAll:=true;
  FBack.Draw;
  FFront:=NewShape(0, 0, 200, 100, FForm);

  Press(FFront, LM_LBUTTONDOWN, 150, 40);
  Press(FFront, LM_LBUTTONUP,   150, 40);

  AssertEquals('nenhum clique no da frente', 0, FFront.Clicks);
  AssertEquals('um clique no de tras',       1, FBack.Clicks);
end;

initialization
  RegisterTest(TTestHMIMouse);

end.
