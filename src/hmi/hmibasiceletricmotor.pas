unit HMIBasicEletricMotor;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  hmi_draw_basiccontrol, BGRABitmap, BGRABitmapTypes;

type

  { THMIBasicEletricMotor }

  THMIBasicEletricMotor = class(THMIBasicControl)
  private
    FMirrored: Boolean;
    procedure SetMirrored(AValue: Boolean);
  protected
    procedure DrawControl; override;

  published
    property Action;
    property OnClick;
    property OnMouseDown;
    property OnMouseLeave;
    property OnMouseMove;
    property PopupMenu;
    property Enabled;

    property BorderColor;
    property BorderWidth;
    property BodyColor;
    property Mirrored: Boolean Read FMirrored Write SetMirrored Default false;
  end;


implementation


{ THMIBasicEletricMotor }

procedure THMIBasicEletricMotor.SetMirrored(AValue: Boolean);
begin
  if FMirrored=AValue then Exit;
  FMirrored:=AValue;
  invalidateShape;
end;

procedure THMIBasicEletricMotor.DrawControl;
begin
  inherited DrawControl;

  FControlArea.CanvasBGRA.Brush.Color:= FBodyColor;
  FControlArea.CanvasBGRA.Pen.Color  := FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  := FBorderWidth;

  FControlArea.CanvasBGRA.Rectangle(Trunc(0.07*width),0+FBorderWidth,Trunc((0.07*width)+(0.09)*Width), Trunc(0.10*Height),true);
  FControlArea.CanvasBGRA.Rectangle(0+FBorderWidth,
                                    trunc(20/52*height),
                                    Trunc(0.07*Width),
                                    Trunc(32/52*Height), true);
  FControlArea.RoundRectAntialias(Trunc(0.19*Width)-1,
                         Trunc(0.06*Height),
                         Width - FBorderWidth,
                         Trunc(0.94*Height),
                         Trunc(0.2*width),
                         Trunc(0.2*width),
                         ColorToBGRA(FBorderColor),
                         FBorderWidth,
                         ColorToBGRA(FBodyColor));
  FControlArea.RoundRectAntialias(Trunc(0.04*Width),
                         Trunc(0.06*Height),
                         Trunc(0.19*Width),
                         Trunc(0.94*Height),
                         Trunc(0.075*width),
                         Trunc(0.075*width),
                         ColorToBGRA(FBorderColor),
                         FBorderWidth,
                         ColorToBGRA(FBodyColor));
  FControlArea.RoundRectAntialias(Trunc(0.39*Width),
                                  Trunc((15/52)*Height),
                                  Trunc(0.73*Width),
                                  Trunc((38/52)*Height),
                                  Trunc(0.09*Width),
                                  Trunc(0.09*Width),
                                  ColorToBGRA(FBorderColor),
                                  FBorderWidth,
                                  ColorToBGRA(FBodyColor));
  FControlArea.CanvasBGRA.Polyline([point (trunc(0.81*Width)-FBorderWidth,trunc (48/52*Height)-FBorderWidth div 2),
                                    point(trunc (0.81*Width)-FBorderWidth,trunc (3/52*Height)+FBorderWidth div 2)]);
  if FMirrored Then
    FControlArea.HorizontalFlip;
end;

end.

