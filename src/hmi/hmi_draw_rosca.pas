unit hmi_draw_rosca;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basic_horizontal_control,
  BGRABitmap, BGRABitmapTypes;

type

  { TRedlerBasico }

  { THMIRoscaBasica }

  THMIRoscaBasica = class(THMIBasicHorizontalControl)
  protected
    procedure DrawControl; override;
    procedure UpdateShape; override;

  published
    property BorderColor;
    property BodyColor;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
  end;

implementation

{ TRoscaBasica }

procedure THMIRoscaBasica.DrawControl;
var
  line_x:integer;
  x:Array Of TPoint;
  h: Integer;
  eixo_h: Integer;
  eixo_top: Integer;
  emptyArea: TBGRABitmap;
begin
  emptyArea := TBGRABitmap.Create(Width,Height);
  try
    FControlArea.Assign(emptyArea);
  finally
    FreeAndNil(emptyArea);
  end;

  //###############################################################################
  //preenchimento do redlers, cor e diametro da linha.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:= FBodyColor;
  FControlArea.CanvasBGRA.Pen.Color  := FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  := FBorderWidth;

  //desenha o quadrado da rosca.
  FControlArea.CanvasBGRA.Rectangle(0, 0, Width, FBodyHeight);

  line_x:=0;
  while line_x<(Width+FBodyHeight) do begin
    h:=Length(x);
    //adiciona os pontos a poliline da rosca..
    SetLength(x,h+1);
    x[h].X:=line_x;
    if (line_x mod (2*FBodyHeight))=0 then begin
      x[h].Y:=0;
    end else begin
      x[h].Y:=FBodyHeight;
    end;
    //
    inc(line_x, FBodyHeight);
  end;
  //desenha a "rosca"
  FControlArea.CanvasBGRA.Pen.Width  :=FBorderWidth+1;
  FControlArea.CanvasBGRA.Polyline(x);
end;

procedure THMIRoscaBasica.UpdateShape;
begin
  //evita chamar o metodo herdado
  //pois este e um controle retangular
  //e nao necessita de cortes.
end;

end.

