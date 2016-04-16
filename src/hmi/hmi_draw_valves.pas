unit HMI_Draw_Valves;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  hmi_draw_basiccontrol, BGRABitmap, BGRABitmapTypes;

type

  TValveType = (vtSimple, vtPneumaticOnOff, vtPneumaticProportional,vtMotorisedProportional);

  { THMIBasicValve }

  THMIBasicValve = class(THMIBasicControl)
  private
    FMirrored: Boolean;
    FValveBodyPercent: Double;
    FValveType: TValveType;
    procedure SetMirrored(AValue: Boolean);
    procedure SetValveBodyPercent(AValue: Double);
    procedure SetValveType(AValue: TValveType);
  protected
    procedure DrawControl; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BodyColor;
    property BorderColor;
    property BorderWidth;


    property Mirrored:Boolean read FMirrored write SetMirrored default false;
    property ValveBodyPercent:Double read FValveBodyPercent write SetValveBodyPercent;
    property ValveType:TValveType read FValveType write SetValveType default vtSimple;

    property OnClick;
    property Action;
  end;

implementation

uses math;

procedure THMIBasicValve.SetMirrored(AValue: Boolean);
begin
  if FMirrored=AValue then Exit;
  FMirrored:=AValue;
  InvalidateShape;
end;

procedure THMIBasicValve.SetValveBodyPercent(AValue: Double);
begin
  if FValveBodyPercent=AValue then Exit;
  if (FValveBodyPercent<0) or (FValveBodyPercent>1) then
    raise exception.Create('ValveBodyPercent accepts values between [0.0 .. 1.0]');
  FValveBodyPercent:=AValue;
  InvalidateShape;
end;

procedure THMIBasicValve.SetValveType(AValue: TValveType);
begin
  if FValveType=AValue then Exit;
  FValveType:=AValue;
  InvalidateShape;
end;

procedure THMIBasicValve.DrawControl;
var
  p:array of TPointF;
  alturaideal: real;
  larguraideal: real;
  larguradoquadrado: real;
begin
  inherited DrawControl;

  FControlArea.CanvasBGRA.Brush.Color:= FBodyColor;
  FControlArea.CanvasBGRA.Pen.Color  := FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  := FBorderWidth;

  SetLength(p, 4);
  IF Width>=Height THEN BEGIN
    p[0].x:=FBorderWidth div 2 + FBorderWidth mod 2;
    p[0].y:=(1-FValveBodyPercent)*Height;

    p[1].x:=Width-(FBorderWidth div 2) - (FBorderWidth mod 2);
    p[1].y:=Height-(FBorderWidth/2);

    p[2].x:=Width-(FBorderWidth div 2) - (FBorderWidth mod 2);
    p[2].y:=(1-FValveBodyPercent)*Height;

    p[3].x:=FBorderWidth div 2 + FBorderWidth mod 2;
    p[3].y:=Height-(FBorderWidth/2);

    FControlArea.CanvasBGRA.PolygonF(p);

    //risco
    case FValveType of
      vtPneumaticProportional,
      vtPneumaticOnOff,
      vtMotorisedProportional:
        FControlArea.CanvasBGRA.PolylineF([PointF( ifthen(((Width+FBorderWidth) mod 2)=1,Width,Width+1)/2,
                                                   (FBorderWidth)),
                                           PointF( ifthen(((Width+FBorderWidth) mod 2)=1,Width,Width+1)/2,
                                                   (1-(FValveBodyPercent/2))*Height-(FBorderWidth/2))]);
    end;

    case FValveType of
      vtPneumaticProportional, vtMotorisedProportional: begin
        larguraideal:=Width/2-Fborderwidth;
        alturaideal:=(Width/4)*(Fvalvebodypercent*Height)/Width+((1-Fvalvebodypercent)*Height)-Fborderwidth;
        larguradoquadrado:=min(larguraideal, alturaideal);
        p[0].x:=(Width-larguradoquadrado)/2;
        p[0].y:=FBorderWidth div 2 + FBorderWidth mod 2;

        p[1].x:=Width-((Width-larguradoquadrado)/2);
        p[1].y:=p[0].y;

        p[2].x:=p[1].x;
        p[2].y:=larguradoquadrado+(FBorderWidth div 2 + FBorderWidth mod 2);

        p[3].x:=p[0].x;
        p[3].y:=p[2].y;
        FControlArea.CanvasBGRA.PolygonF(p);

        if FValveType= vtMotorisedProportional then begin
          FControlArea.FontHeight := trunc(larguradoquadrado * 0.9 - 2*Fborderwidth);
          FControlArea.FontOrientation :=0;
          FControlArea.TextOut(Width/2,Trunc((larguradoquadrado-FControlArea.FontHeight)/2),'M',colortobgra(FBordercolor), taCenter);

        end;
      end;
      vtPneumaticOnOff: begin
        FControlArea.Pie(Width/2,
                         ((1-FValveBodyPercent)*Height)+(FBorderWidth/2),
                         Width/4,
                         Height*(1-FValveBodyPercent),
                         0,
                         180*0.0174532925,
                         ColorToBGRA(FBorderColor),
                         FBorderWidth,
                         ColorToBGRA(FBodyColor));
      end;
    end;
  end else begin
    p[0].x:=(1-FValveBodyPercent)*Width;
    p[0].y:=FBorderWidth div 2 + FBorderWidth mod 2;

    p[1].x:=Width-(FBorderWidth/2);
    p[1].y:=Height-(FBorderWidth div 2) - (FBorderWidth mod 2);

    p[2].x:=(1-FValveBodyPercent)*Width;
    p[2].y:=Height-(FBorderWidth div 2) - (FBorderWidth mod 2);

    p[3].x:=Width-(FBorderWidth/2);
    p[3].y:=FBorderWidth div 2 + FBorderWidth mod 2;

    FControlArea.CanvasBGRA.PolygonF(p);

    //risco
    case FValveType of
      vtPneumaticProportional,
      vtPneumaticOnOff,
      vtMotorisedProportional:
        FControlArea.CanvasBGRA.PolylineF([PointF(FBorderWidth, ifthen(((Height+FBorderWidth) mod 2)=1,Height,Height+1)/2),
                                           PointF((1-(FValveBodyPercent/2))*Width-(BorderWidth/2), ifthen(((Height+FBorderWidth) mod 2)=1,Height,Height+1)/2)]);
    end;

    case FValveType of
      vtPneumaticProportional,vtMotorisedProportional: begin
        larguraideal:=Height/2-Fborderwidth;
        alturaideal:=(Height/4)*(Fvalvebodypercent*Width)/Height+((1-Fvalvebodypercent)*Width)-Fborderwidth;
        larguradoquadrado:=min(larguraideal, alturaideal);
        p[0].x:=FBorderWidth div 2 + FBorderWidth mod 2;
        p[0].y:=(Height-larguradoquadrado)/2;

        p[1].x:=larguradoquadrado+(FBorderWidth div 2 + FBorderWidth mod 2);
        p[1].y:= p[0].y;

        p[2].x:=p[1].x;
        p[2].y:=Height-((Height-larguradoquadrado)/2);

        p[3].x:=p[0].x;
        p[3].y:=p[2].y;
        FControlArea.CanvasBGRA.PolygonF(p);

        if FValveType= vtMotorisedProportional then begin
          FControlArea.FontHeight := trunc(larguradoquadrado * 0.9 - Fborderwidth);
          FControlArea.FontOrientation :=900;
          FControlArea.TextOut(Trunc((larguradoquadrado-FControlArea.FontHeight)/2),Height/2 ,'M',colortobgra(FBordercolor), taCenter);

      end;
      end;
      vtPneumaticOnOff: begin
        FControlArea.Pie(((1-FValveBodyPercent)*Width)+(FBorderWidth/2),
                         Height/2,
                         Width*(1-FValveBodyPercent),
                         Height/4,
                         90*0.0174532925,
                         270*0.0174532925,
                         ColorToBGRA(FBorderColor),
                         FBorderWidth,
                         ColorToBGRA(FBodyColor));
      end;
    end;
  end;

  if FMirrored then begin
    if Height<=Width then
      FControlArea.VerticalFlip
    else
      FControlArea.HorizontalFlip;
  end;
end;

constructor THMIBasicValve.Create(AOwner: TComponent);
begin
  FMirrored:=false;
  FValveBodyPercent:=0.7;
  inherited Create(AOwner);
end;

end.
