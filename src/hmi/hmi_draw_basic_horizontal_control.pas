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
    FBodyColor: TColor;

    FBorderColor: TColor;
    FBorderSize: Byte;

    procedure SetBodyHeight(AValue: Byte); virtual;
    procedure SetBodyColor(AValue: TColor); virtual;
    procedure SetBorderColor(AValue: TColor); override;
    procedure SetBorderSize(AValue: Byte); virtual;

    property BodyHeight:Byte read FBodyHeight write SetBodyHeight;
    property BodyColor:TColor read FBodyColor Write SetBodyColor;
    property BorderColor:TColor read FBorderColor write SetBorderColor;
    property BorderSize:Byte read FBorderSize write SetBorderSize;

  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TControleHorizontalSimples }

constructor THMIBasicHorizontalControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeginUpdate;
  BodyColor:=clSilver;
  BodyHeight:=12; //inicializa o desenho com 12px de largura do
  BorderColor:=clBlack;
  BorderSize:=1;
  EndUpdate;
end;

procedure THMIBasicHorizontalControl.SetBorderSize(AValue: Byte);
begin
  if FBorderSize=AValue then Exit;
  FBorderSize:=AValue;
  Invalidate;
end;

procedure THMIBasicHorizontalControl.SetBodyHeight(AValue: Byte);
begin
  if (FBodyHeight=AValue) or (AValue<5) then Exit;
  FBodyHeight:=AValue;
  BeginUpdate;
  Constraints.MinHeight:=FBodyHeight;
  Constraints.MaxHeight:=FBodyHeight;
  Constraints.MinWidth :=FBodyHeight*2+3;
  if Width<Constraints.MinWidth then
    Width:=Constraints.MinHeight;
  Height:=FBodyHeight;
  EndUpdate;
end;

procedure THMIBasicHorizontalControl.SetBodyColor(AValue: TColor);
begin
  if FBodyColor=AValue then Exit;
  FBodyColor:=AValue;
  Invalidate;
end;

procedure THMIBasicHorizontalControl.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  Invalidate;
end;

end.

