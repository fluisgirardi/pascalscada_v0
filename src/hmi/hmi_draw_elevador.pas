unit hmi_draw_elevador;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basiccontrol, BGRABitmap,
  BGRABitmapTypes;

type

  { THMIElevadorBasico }

  THMIElevadorBasico = class(THMIBasicControl)
  protected
    FBodyWidth: Byte;
    FFooterColor: TColor;
    FHeadColor: TColor;
    procedure SetBodyWidth(AValue: Byte);
    procedure SetFooterColor(AValue: TColor);
    procedure SetHeadColor(AValue: TColor);
    procedure DrawControl; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HeadColor:TColor read FHeadColor write SetHeadColor default clSilver;
    property BodyColor default clSilver;
    property FooterColor:TColor read FFooterColor write SetFooterColor default clSilver;
    property BorderColor;

    property BodyWidth:Byte read FBodyWidth write SetBodyWidth default 12;
    property BorderWidth default 1;
  end;

implementation

uses Dialogs;

{ TElevadorBasico }

procedure THMIElevadorBasico.SetHeadColor(AValue: TColor);
begin
  if FHeadColor=AValue then Exit;
  FHeadColor:=AValue;
  Invalidate;
end;

procedure THMIElevadorBasico.DrawControl;
var
  x:array of TPoint;
  emptyArea: TBGRABitmap;
begin
  emptyArea := TBGRABitmap.Create(Width,Height);
  try
    FControlArea.Assign(emptyArea);
  finally
    FreeAndNil(emptyArea);
  end;

  //###############################################################################
  //coordenadas de que desenham a cabeca do elevador
  //###############################################################################
  SetLength(x,4);

  x[0].X:=BodyWidth;
  x[0].Y:=(BorderWidth div 2)+(BorderWidth mod 2);

  x[1].X:=2*BodyWidth-1;
  x[1].Y:=(BorderWidth div 2)+(BorderWidth mod 2);

  x[2].X:=3*BodyWidth-1;
  x[2].Y:=BodyWidth + (BorderWidth div 2)+(BorderWidth mod 2);

  x[3].X:=BodyWidth;
  x[3].Y:=BodyWidth + (BorderWidth div 2)+(BorderWidth mod 2);

  //###############################################################################
  //preenchimento da cabeça do elevador, cor e diametro da linha.
  //###############################################################################

  FControlArea.Canvas.Brush.Color:= FHeadColor;
  FControlArea.Canvas.Pen.Color  := FBorderColor;
  FControlArea.Canvas.Pen.Width  := FBorderWidth;

  //###############################################################################
  //desenha a cabeca do elevador.
  //###############################################################################
  FControlArea.Canvas.Polygon(x);

  //###############################################################################
  //coordenadas que desenham o pé do elevador.
  //###############################################################################
  x[0].X:=0;
  x[0].Y:=Height-BodyWidth-(BorderWidth div 2)-(BorderWidth mod 2);

  x[1].X:=3*BodyWidth-1;
  x[1].Y:=Height-BodyWidth-(BorderWidth div 2)-(BorderWidth mod 2);

  x[2].X:=2*BodyWidth-1;
  x[2].Y:=Height-(BorderWidth div 2)-(BorderWidth mod 2);

  x[3].X:=BodyWidth;
  x[3].Y:=Height-(BorderWidth div 2)-(BorderWidth mod 2);

  //###############################################################################
  //preenchimento do pé do elevador.
  //###############################################################################
  FControlArea.Canvas.Brush.Color:=FFooterColor;

  //###############################################################################
  //desenha o pé do elevador.
  //###############################################################################
  FControlArea.Canvas.Polygon(x);

  //###############################################################################
  //preenchimento do corpo do elevador.
  //###############################################################################
  FControlArea.Canvas.Brush.Color:=FBodyColor;
  FControlArea.Canvas.Rectangle(FBodyWidth,
                                (BorderWidth div 2)+(BorderWidth mod 2),
                                2*FBodyWidth,
                                Height-(BorderWidth div 2)-(BorderWidth mod 2)+1);

  FControlArea.Canvas.Line(Point(FBodyWidth, FBodyWidth+(BorderWidth div 2)+(BorderWidth mod 2)),
                           Point(2*FBodyWidth,FBodyWidth+(BorderWidth div 2)+(BorderWidth mod 2)));
  FControlArea.Canvas.Line(Point(FBodyWidth, Height-BodyWidth-(BorderWidth div 2)-(BorderWidth mod 2)),
                           Point(2*FBodyWidth, Height-BodyWidth-(BorderWidth div 2)-(BorderWidth mod 2)));
  //FControlArea.CanvasBGRA.PolylineF([PointF(FBodyWidth,  FBodyWidth+(BorderWidth div 2)+(BorderWidth mod 2)),
  //                                   PointF(2*FBodyWidth,FBodyWidth+(BorderWidth div 2)+(BorderWidth mod 2))]);
  //FControlArea.CanvasBGRA.PolylineF([PointF(FBodyWidth,   Height-BodyWidth-(BorderWidth/2)),
  //                                   PointF(2*FBodyWidth, Height-BodyWidth-(BorderWidth/2))]);
end;

constructor THMIElevadorBasico.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeginUpdate;
  HeadColor:=clSilver;
  BodyColor:=clSilver;
  FooterColor:=clSilver;
  BorderColor:=clBlack;
  BorderWidth:=1;
  BodyWidth:=12; //inicializa o desenho com 12px de largura do
  EndUpdate;
end;

procedure THMIElevadorBasico.SetBodyWidth(AValue: Byte);
begin
  if FBodyWidth=AValue then Exit;
  FBodyWidth:=AValue;
  BeginUpdate;
  Constraints.MinWidth:=FBodyWidth*3;
  Constraints.MaxWidth:=FBodyWidth*3;
  Constraints.MinHeight:=FBodyWidth*2+3;
  if Height<Constraints.MinHeight then
    Height:=Constraints.MinHeight;
  Width:=FBodyWidth*3;
  EndUpdate;
end;

procedure THMIElevadorBasico.SetFooterColor(AValue: TColor);
begin
  if FFooterColor=AValue then Exit;
  FFooterColor:=AValue;
  Invalidate;
end;

end.

