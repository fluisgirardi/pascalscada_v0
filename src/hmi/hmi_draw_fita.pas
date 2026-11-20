unit hmi_draw_fita;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basic_horizontal_control;

type

  { THMIFitaBasica }

  THMIFitaBasica = class(THMIBasicHorizontalControl)
  protected
    procedure PaintControl(ACanvas:TCanvas; AWidth, AHeight:Integer); override;
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

procedure THMIFitaBasica.PaintControl(ACanvas: TCanvas; AWidth,
  AHeight: Integer);
begin
  SetupCanvas(ACanvas, FBorderColor, FBorderWidth, FBodyColor);

  ACanvas.Rectangle(FBodyHeight div 2,0,AWidth-(FBodyHeight div 2), FBodyHeight);

  ACanvas.Ellipse(0, 0, FBodyHeight, FBodyHeight);

  ACanvas.Ellipse(AWidth-FBodyHeight, 0, AWidth, FBodyHeight);
end;

end.

