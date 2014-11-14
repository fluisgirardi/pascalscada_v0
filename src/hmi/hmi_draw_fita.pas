unit hmi_draw_fita;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basic_horizontal_control,
  BGRABitmap, BGRABitmapTypes;

type

  { TFitaBasica }

  THMIFitaBasica = class(THMIBasicHorizontalControl)
  protected
    procedure Paint; override;
  end;

implementation

{ TFitaBasica }

procedure THMIFitaBasica.Paint;
begin
  if not CanRepaint then exit;

  FControlArea:=TBGRABitmap.Create(Self.Width, Self.Height);
  try
    //###############################################################################
    //preenchimento da cabeça do elevador, cor e diametro da linha.
    //###############################################################################
    FControlArea.CanvasBGRA.Brush.Color:=FBodyColor;
    FControlArea.CanvasBGRA.Pen.Color  :=FBorderColor;
    FControlArea.CanvasBGRA.Pen.Width  :=FBorderSize;

    //desenha o quadrado da fita.
    FControlArea.CanvasBGRA.Rectangle(FBodyHeight div 2,0,Width-(FBodyHeight div 2), FBodyHeight);

    //desenha o circulo da esquerda
    FControlArea.CanvasBGRA.Ellipse(0, 0, FBodyHeight, FBodyHeight);

    //desenha o circulo da esquerda
    FControlArea.CanvasBGRA.Ellipse(Width-FBodyHeight, 0, Width, FBodyHeight);

    FControlArea.Draw(Canvas,0,0, true);
    inherited Paint;
  finally
    FreeAndNil(FControlArea);
  end;
end;

end.

