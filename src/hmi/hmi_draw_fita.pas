unit hmi_draw_fita;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basic_horizontal_control,
  BGRABitmap;

type

  { THMIFitaBasica }

  THMIFitaBasica = class(THMIBasicHorizontalControl)
  protected
    procedure DrawControl; override;
  published
    property BodyHeight;
    property BodyColor;
    property BorderColor;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
  end;

implementation

procedure THMIFitaBasica.DrawControl;
var
  emptyArea: TBGRABitmap;
begin
  emptyArea := TBGRABitmap.Create(Width,Height);
  try
    FControlArea.Assign(emptyArea);
  finally
    FreeAndNil(emptyArea);
  end;

  //###############################################################################
  //preenchimento da cabeça do elevador, cor e diametro da linha.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:=FBodyColor;
  FControlArea.CanvasBGRA.Pen.Color  :=FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  :=FBorderWidth;

  //desenha o quadrado da fita.
  FControlArea.CanvasBGRA.Rectangle(FBodyHeight div 2,0,Width-(FBodyHeight div 2), FBodyHeight);

  //desenha o circulo da esquerda
  FControlArea.CanvasBGRA.Ellipse(0, 0, FBodyHeight, FBodyHeight);

  //desenha o circulo da esquerda
  FControlArea.CanvasBGRA.Ellipse(Width-FBodyHeight, 0, Width, FBodyHeight);
end;

end.

