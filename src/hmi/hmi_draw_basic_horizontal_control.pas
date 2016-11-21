unit hmi_draw_basic_horizontal_control;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basiccontrol, BGRABitmap,
  BGRABitmapTypes;

type

  { TControleHorizontalSimples }

  THMIBasicHorizontalControl = class(THMIBasicControl)
  protected
    FBodyHeight: Byte;

    procedure SetBodyHeight(AValue: Byte); virtual;
    procedure SetBodyColor(AValue: TColor); virtual;

    property BodyHeight:Byte read FBodyHeight write SetBodyHeight;
    property BodyColor:TColor read FBodyColor Write SetBodyColor;

  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TControleHorizontalSimples }

constructor THMIBasicHorizontalControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BodyColor:=clSilver;
  BodyHeight:=12; //inicializa o desenho com 12px de largura do
  BorderColor:=clBlack;
end;

procedure THMIBasicHorizontalControl.SetBodyHeight(AValue: Byte);
begin
  if (FBodyHeight=AValue) or (AValue<5) then Exit;
  FBodyHeight:=AValue;
  Constraints.MinHeight:=FBodyHeight;
  Constraints.MaxHeight:=FBodyHeight;
  Constraints.MinWidth :=FBodyHeight*2+3;
  if Width<Constraints.MinWidth then
    Width:=Constraints.MinHeight;
  Height:=FBodyHeight;
end;

procedure THMIBasicHorizontalControl.SetBodyColor(AValue: TColor);
begin
  if FBodyColor=AValue then Exit;
  FBodyColor:=AValue;
  InvalidateDraw;
end;

end.

