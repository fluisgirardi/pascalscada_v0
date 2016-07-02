unit hmi_draw_elevador;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basiccontrol, BGRABitmap,
  BGRABitmapTypes;

type

  THMICustomElevadorBasico = class(THMIBasicControl)
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
  protected
    property HeadColor:TColor read FHeadColor write SetHeadColor default clSilver;
    property BodyColor default clSilver;
    property FooterColor:TColor read FFooterColor write SetFooterColor default clSilver;
    property BorderColor;

    property BodyWidth:Byte read FBodyWidth write SetBodyWidth default 12;
    property BorderWidth default 1;
  end;

  THMIElevadorBasico = class(THMICustomElevadorBasico)
  published
    property HeadColor;
    property BodyColor;
    property BodyWidth;
    property FooterColor;
    property BorderColor;

    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
  end;

implementation

uses Dialogs;

{ TElevadorBasico }

procedure THMICustomElevadorBasico.SetHeadColor(AValue: TColor);
begin
  if FHeadColor=AValue then Exit;
  FHeadColor:=AValue;
  InvalidateDraw;
end;

procedure THMICustomElevadorBasico.DrawControl;
var
  x:array of TPointF;
begin
  inherited DrawControl;


  FControlArea.CanvasBGRA.AntialiasingMode:=amOff;

  //###############################################################################
  //coordenadas de que desenham a cabeca do elevador
  //###############################################################################
  SetLength(x,4);

  x[0].X:=BodyWidth;
  x[0].Y:=(BorderWidth/2);

  x[1].X:=2*BodyWidth-1;
  x[1].Y:=(BorderWidth/2);

  x[2].X:=3*BodyWidth-1;
  x[2].Y:=BodyWidth + (BorderWidth/2);

  x[3].X:=BodyWidth;
  x[3].Y:=BodyWidth + (BorderWidth/2);

  //###############################################################################
  //preenchimento da cabeça do elevador, cor e diametro da linha.
  //###############################################################################

  FControlArea.CanvasBGRA.Brush.Color:= FHeadColor;
  FControlArea.CanvasBGRA.Pen.Color  := FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  := FBorderWidth;

  //###############################################################################
  //desenha a cabeca do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.PolygonF(x);

  //###############################################################################
  //coordenadas que desenham o pé do elevador.
  //###############################################################################
  x[0].X:=0;
  x[0].Y:=Height-BodyWidth-(BorderWidth/2);

  x[1].X:=3*BodyWidth-1;
  x[1].Y:=Height-BodyWidth-(BorderWidth/2);

  x[2].X:=2*BodyWidth-1;
  x[2].Y:=Height-(BorderWidth/2);

  x[3].X:=BodyWidth;
  x[3].Y:=Height-(BorderWidth/2);

  //###############################################################################
  //preenchimento do pé do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:=FFooterColor;

  //###############################################################################
  //desenha o pé do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.PolygonF(x);

  //###############################################################################
  //preenchimento do corpo do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:=FBodyColor;

  x[0].X:=FBodyWidth;
  x[0].Y:=(BorderWidth/2);

  x[1].X:=2*BodyWidth-1;
  x[1].Y:=(BorderWidth/2);

  x[2].X:=2*BodyWidth-1;
  x[2].Y:=Height-(BorderWidth/2);

  x[3].X:=BodyWidth;
  x[3].Y:=Height-(BorderWidth/2);

  FControlArea.CanvasBGRA.PolygonF(x);

  //###############################################################################
  //Risquinhos, acabamento.
  //###############################################################################
  FControlArea.CanvasBGRA.PolylineF([PointF(FBodyWidth,
                                            FBodyWidth+(BorderWidth/2)),
                                     PointF(2*FBodyWidth-1,
                                            FBodyWidth+(BorderWidth/2))]);
  FControlArea.CanvasBGRA.PolylineF([PointF(FBodyWidth,   Height-BodyWidth-(BorderWidth/2)),
                                     PointF(2*FBodyWidth, Height-BodyWidth-(BorderWidth/2))]);
end;

constructor THMICustomElevadorBasico.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeginUpdate;
  FHeadColor:=clSilver;
  FBodyColor:=clSilver;
  FFooterColor:=clSilver;
  FBorderColor:=clBlack;
  FBorderWidth:=1;
  FBodyWidth:=12; //inicializa o desenho com 12px de largura do
  EndUpdate;
end;

procedure THMICustomElevadorBasico.SetBodyWidth(AValue: Byte);
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
  InvalidateShape;
  EndUpdate;
end;

procedure THMICustomElevadorBasico.SetFooterColor(AValue: TColor);
begin
  if FFooterColor=AValue then Exit;
  FFooterColor:=AValue;
  InvalidateDraw;
end;

end.

