unit hmi_draw_redler;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basic_horizontal_control,
  BGRABitmap, BGRABitmapTypes;

type

  { TRedlerBasico }

  { THMIRedlerBasico }

  THMIRedlerBasico = class(THMIBasicHorizontalControl)
  protected
    procedure UpdateShape; override;
    procedure SetBodyHeight(AValue: Byte); override;
    procedure DrawControl; override;
  end;

implementation

{ TRedlerBasico }

procedure THMIRedlerBasico.UpdateShape;
begin
  //evita chamar o metodo herdado
  //pois este e um controle retangular
  //e nao necessita de cortes.
end;

procedure THMIRedlerBasico.SetBodyHeight(AValue: Byte);
var
  minheight: Integer;
begin
  {espaco para as pás e mais um pixel para o eixo central.}
  minheight:=2*FBorderSize+2+1;
  if AValue<minheight then exit;
  inherited SetBodyHeight(AValue);
end;

procedure THMIRedlerBasico.DrawControl;
var
  eixo_h, eixo_top, pa_h, pa_x:Integer;
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
  FControlArea.CanvasBGRA.Brush.Color:=FBodyColor;
  FControlArea.CanvasBGRA.Pen.Color  :=FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  :=FBorderSize;

  //desenha o quadrado da fita.
  FControlArea.CanvasBGRA.Rectangle(0, 0, Width, FBodyHeight);

  //evita um lado menor que o outro.
  if (FBodyHeight mod 2) = 1 then
    eixo_h:=(FBodyHeight-(2*FBorderSize)) div 3
  else
    eixo_h:=(FBodyHeight-(2*FBorderSize)) div 4;

  //menor tamanho do eixo
  if eixo_h<1 then eixo_h:=1;

  //###############################################################################
  //eixo do redler, cor e diametro da linha.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:=FBorderColor;
  FControlArea.CanvasBGRA.Pen.Color  :=FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  :=FBorderSize;

  eixo_top:=(FBodyHeight - eixo_h) div 2;

  //desenha o quadrado da fita.
  FControlArea.CanvasBGRA.Rectangle(0, eixo_top, Width, eixo_top+eixo_h);

  //desenha as pás do redler.
  pa_h := FBodyHeight - (2*FBorderSize) - eixo_h;
  pa_x:=FBodyHeight div 2;
  while (pa_x+eixo_h)<(Width-(2*FBorderSize)) do begin
    //desenha em baixo do eixo...
    if (pa_x mod 12)=0 then
      FControlArea.CanvasBGRA.Rectangle(pa_x, eixo_top+eixo_h, pa_x+eixo_h, eixo_top+eixo_h+pa_h)
    else
      FControlArea.CanvasBGRA.Rectangle(pa_x, 0, pa_x+eixo_h, pa_h-1);
    inc(pa_x, FBodyHeight div 2);
  end;
end;

end.

