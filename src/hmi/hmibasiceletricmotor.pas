unit HMIBasicEletricMotor;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  hmi_draw_basiccontrol, BGRABitmap, BGRABitmapTypes, hmi_commfaultbadge;

type

  { THMICustomBasicEletricMotor }

  THMICustomBasicEletricMotor = class(THMIBasicControl)
  private
    FDrawPump: Boolean;
    FMirrored: Boolean;
    procedure SetDrawPump(AValue: Boolean);
    procedure SetMirrored(AValue: Boolean);
  protected
    procedure DrawControl; override;
    procedure DrawFaultIcon; override;
    property Mirrored: Boolean Read FMirrored Write SetMirrored Default false;
    property DrawPump: Boolean read FDrawPump write SetDrawPump default true;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  THMIBasicEletricMotor = class(THMICustomBasicEletricMotor)
  published
    property Action;
    property DrawPump;
    property OnClick;
    property OnMouseDown;
    property OnMouseLeave;
    property OnMouseMove;
    property PopupMenu;
    property Enabled;

    property BorderColor;
    property BorderWidth;
    property BodyColor;
    property Mirrored;
  end;


implementation


{ THMIBasicEletricMotor }

procedure THMICustomBasicEletricMotor.SetMirrored(AValue: Boolean);
begin
  if FMirrored=AValue then Exit;
  FMirrored:=AValue;
  invalidateShape;
end;

procedure THMICustomBasicEletricMotor.SetDrawPump(AValue: Boolean);
begin
  if FDrawPump=AValue then Exit;
  FDrawPump:=AValue;
  InvalidateShape;
end;

procedure THMICustomBasicEletricMotor.DrawControl;
var
  Rotate: Boolean;
  w, h: Integer;
  aux: TBGRABitmap;
  aux2: TBGRACustomBitmap;
begin
  inherited DrawControl;

  FControlArea.CanvasBGRA.Brush.Color:= FBodyColor;
  FControlArea.CanvasBGRA.Pen.Color  := FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  := FBorderWidth;

  if Width>=Height then begin
    w:=Width;
    h:=Height;
    Rotate:=false;
  end else begin
    w:=Height;
    h:=Width;
    Rotate:=true;
  end;

  aux:=TBGRABitmap.Create(w,h);
  try
    if FDrawPump then begin
      aux.CanvasBGRA.Rectangle(Trunc(0.07*w),0+FBorderWidth,Trunc((0.07*w)+(0.09)*w), Trunc(0.10*h),true);

      aux.CanvasBGRA.Rectangle(0+FBorderWidth,
                                        trunc(20/52*h),
                                        Trunc(0.07*w),
                                        Trunc(32/52*h), true);
    end;
    aux.RoundRectAntialias(Trunc(0.19*w)-1,
                           Trunc(0.06*h),
                           w - FBorderWidth,
                           Trunc(0.94*h),
                           Trunc(0.2*w),
                           Trunc(0.2*w),
                           ColorToBGRA(FBorderColor),
                           FBorderWidth,
                           ColorToBGRA(FBodyColor));
    if FDrawPump then
      aux.RoundRectAntialias(Trunc(0.04*w),
                             Trunc(0.06*h),
                             Trunc(0.19*w),
                             Trunc(0.94*h),
                             Trunc(0.075*w),
                             Trunc(0.075*w),
                             ColorToBGRA(FBorderColor),
                             FBorderWidth,
                             ColorToBGRA(FBodyColor));
    aux.RoundRectAntialias(Trunc(0.39*w),
                                    Trunc((15/52)*h),
                                    Trunc(0.73*w),
                                    Trunc((38/52)*h),
                                    Trunc(0.09*w),
                                    Trunc(0.09*w),
                                    ColorToBGRA(FBorderColor),
                                    FBorderWidth,
                                    ColorToBGRA(FBodyColor));
    aux.CanvasBGRA.Polyline([point (trunc(0.81*w)-FBorderWidth,trunc (48/52*h)-FBorderWidth div 2),
                                      point(trunc (0.81*w)-FBorderWidth,trunc (3/52*h)+FBorderWidth div 2)]);

    if Rotate then begin

      if Mirrored then
        aux2:=aux.RotateCW
      else
        aux2:=aux.RotateCCW;

      try
        FControlArea.Assign(aux2);
      finally
        FreeAndNil(aux2);
      end;
    end else begin
      FControlArea.Assign(aux);
      if FMirrored Then
        FControlArea.HorizontalFlip;
    end;
  finally
    FreeAndNil(aux);
  end;


end;

procedure THMICustomBasicEletricMotor.DrawFaultIcon;
const
  //corpo do motor ocupa de x=0.19w ate x=w (ver DrawControl) - a bomba (se
  //desenhada) fica no restante, x=0..0.19w, que fica de fora do centro do
  //corpo do motor.
  //motor body spans from x=0.19w to x=w (see DrawControl) - the pump (if
  //drawn) sits in the remainder, x=0..0.19w, which is left out of the
  //motor body's center.
  MotorBodyFraction = 0.81; //1 - 0.19
var
  bodySize, bodyOffset: Integer;
begin
  if Width>=Height then begin
    bodySize := Round(MotorBodyFraction*Width);
    if bodySize<1 then bodySize := 1;
    if FMirrored then
      bodyOffset := 0
    else
      bodyOffset := Width - bodySize;
    DrawWarningIconCentered(Canvas, bodySize, Height, bodyOffset, 0);
  end else begin
    //controle vertical: o desenho e' feito na horizontal e depois rotacionado
    //(RotateCCW se nao Mirrored, RotateCW se Mirrored) - o lado que fica com
    //o corpo do motor apos a rotacao e' o melhor palpite sem poder testar
    //visualmente aqui; avise se ficar do lado errado.
    //vertical control: the drawing is done horizontally and then rotated
    //(RotateCCW if not Mirrored, RotateCW if Mirrored) - which end ends up
    //with the motor body after rotation is a best guess without being able
    //to visually test here; let me know if it lands on the wrong side.
    bodySize := Round(MotorBodyFraction*Height);
    if bodySize<1 then bodySize := 1;
    if FMirrored then
      bodyOffset := Height - bodySize
    else
      bodyOffset := 0;
    DrawWarningIconCentered(Canvas, Width, bodySize, 0, bodyOffset);
  end;
end;

constructor THMICustomBasicEletricMotor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawPump:=true;
end;

end.

