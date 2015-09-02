unit hmi_draw_elevador;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basiccontrol, BGRABitmap,
  BGRABitmapTypes;

type

  { TElevadorBasico }

  { THMIElevadorBasico }

  THMIElevadorBasico = class(THMIBasicControl)
  private
    FBorderColor: TColor;
    FBorderWidth: Byte;
    procedure SetBorderWidth(AValue: Byte);
  protected
    FBodyWidth: Byte;
    FBodyColor: TColor;
    FFooterColor: TColor;
    FHeadColor: TColor;

    procedure SetBodyColor(AValue: TColor);
    procedure SetBodyWidth(AValue: Byte);
    procedure SetBorderColor(AValue: TColor); override;
    procedure SetColor(AValue: TColor); override;
    procedure SetFooterColor(AValue: TColor);
    procedure SetHeadColor(AValue: TColor);

    procedure DrawControl; override;

    property BorderColor:TColor read FBorderColor write SetBorderColor;
    property BorderWidth:Byte read FBorderWidth write SetBorderWidth;
    property BodyColor:TColor read FBodyColor write SetBodyColor;
    property BodyWidth:Byte read FBodyWidth write SetBodyWidth;
    property Color:TColor read FBodyColor Write SetColor;
    property FooterColor:TColor read FFooterColor write SetFooterColor;
    property HeadColor:TColor read FHeadColor write SetHeadColor;

  public
    constructor Create(AOwner: TComponent); override;
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
  x[0].Y:=0;

  x[1].X:=2*BodyWidth-1;
  x[1].Y:=0;

  x[2].X:=3*BodyWidth-1;
  x[2].Y:=BodyWidth;

  x[3].X:=BodyWidth;
  x[3].Y:=BodyWidth;

  //###############################################################################
  //preenchimento da cabeça do elevador, cor e diametro da linha.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:=FHeadColor;
  FControlArea.CanvasBGRA.Pen.Color  :=FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  :=FBorderWidth;

  //###############################################################################
  //desenha a cabeca do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.Polygon(x);

  //###############################################################################
  //coordenadas que desenham o pé do elevador.
  //###############################################################################
  x[0].X:=0;
  x[0].Y:=Height-BodyWidth-1;

  x[1].X:=3*BodyWidth-1;
  x[1].Y:=Height-BodyWidth-1;

  x[2].X:=2*BodyWidth-1;
  x[2].Y:=Height-1;

  x[3].X:=BodyWidth;
  x[3].Y:=Height-1;

  //###############################################################################
  //preenchimento do pé do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:=FFooterColor;

  //###############################################################################
  //desenha o pé do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.Polygon(x);

  //###############################################################################
  //preenchimento do corpo do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:=FBodyColor;
  FControlArea.CanvasBGRA.Rectangle(FBodyWidth, 0, 2*FBodyWidth, Height);

  FControlArea.CanvasBGRA.PolylineF([PointF(FBodyWidth+1, FBodyWidth), PointF(2*FBodyWidth,FBodyWidth)]);
  FControlArea.CanvasBGRA.PolylineF([PointF(FBodyWidth+1, Height-FBodyWidth-1), PointF(2*FBodyWidth, Height-FBodyWidth-1)]);
end;

constructor THMIElevadorBasico.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeginUpdate;
  Color:=clSilver;
  BorderColor:=clBlack;
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

procedure THMIElevadorBasico.SetColor(AValue: TColor);
begin
  BeginUpdate;
  SetHeadColor(AValue);
  SetBodyColor(AValue);
  SetFooterColor(AValue);
  EndUpdate;
end;

procedure THMIElevadorBasico.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  Invalidate;
end;

procedure THMIElevadorBasico.SetBorderWidth(AValue: Byte);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  Invalidate;
end;

procedure THMIElevadorBasico.SetBodyColor(AValue: TColor);
begin
  if FBodyColor=AValue then Exit;
  FBodyColor:=AValue;
  Invalidate;
end;

procedure THMIElevadorBasico.SetFooterColor(AValue: TColor);
begin
  if FFooterColor=AValue then Exit;
  FFooterColor:=AValue;
  Invalidate;
end;

end.

