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
  //MinWidth, nao MinHeight: alargar o controle estreito usando a altura
  //minima era erro de copia. Sem efeito visivel, porque a propria restricao
  //empurrava a largura de volta - mas dizia a coisa errada.
  //MinWidth, not MinHeight: widening the narrow control with the minimum
  //height was a copy/paste slip. No visible effect, because the constraint
  //itself pushed the width back up - but it said the wrong thing.
  if Width<Constraints.MinWidth then
    Width:=Constraints.MinWidth;
  //as restricoes sozinhas nao mexem no tamanho ja' aplicado - elas valem da
  //proxima mudanca de limites em diante. E' esta atribuicao que leva o
  //controle para a altura nova na hora.
  //the constraints alone do not touch the size already applied - they hold
  //from the next bounds change on. It is this assignment that takes the
  //control to the new height right away.
  Height:=FBodyHeight;
end;

procedure THMIBasicHorizontalControl.SetBodyColor(AValue: TColor);
begin
  if FBodyColor=AValue then Exit;
  FBodyColor:=AValue;
  InvalidateDraw;
end;

end.

