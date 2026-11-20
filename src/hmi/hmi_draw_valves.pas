unit HMI_Draw_Valves;

interface

uses
  Classes, SysUtils, Types, LResources, Forms, Controls, Graphics, Dialogs,
  hmi_draw_basiccontrol;

type

  TValveType = (vtSimple, vtPneumaticOnOff, vtPneumaticProportional,vtMotorisedProportional, vtPneumaticDrawer);

  { THMIBasicValve }

  THMICustomBasicValve = class(THMIBasicControl)
  private
    FMirrored: Boolean;
    FValveBodyPercent: Double;
    FValveType: TValveType;
    procedure SetMirrored(AValue: Boolean);
    procedure SetValveBodyPercent(AValue: Double);
    procedure SetValveType(AValue: TValveType);
  protected
    procedure PaintControl(ACanvas:TCanvas; AWidth, AHeight:Integer); override;
    function  DrawingTransform:TDrawingTransform; override;

    property Mirrored:Boolean read FMirrored write SetMirrored default false;
    property ValveBodyPercent:Double read FValveBodyPercent write SetValveBodyPercent;
    property ValveType:TValveType read FValveType write SetValveType default vtSimple;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  THMIBasicValve = class(THMICustomBasicValve)
  published
    property BodyColor;
    property BorderColor;
    property BorderWidth;

    property Mirrored;
    property ValveBodyPercent;
    property ValveType;

    property OnClick;
    property Action;
  end;

implementation

uses math;

procedure THMICustomBasicValve.SetMirrored(AValue: Boolean);
begin
  if FMirrored=AValue then Exit;
  FMirrored:=AValue;
  InvalidateShape;
end;

procedure THMICustomBasicValve.SetValveBodyPercent(AValue: Double);
begin
  if FValveBodyPercent=AValue then Exit;
  //o valor que chega, nao o que ja esta guardado: conferindo o antigo, o fora
  //de faixa entrava sem reclamar e depois reprovava todos os seguintes,
  //inclusive os validos.
  //the value coming in, not the one already stored: checking the old one let
  //the out of range value in without complaining and then refused every value
  //after it, valid ones included.
  if (AValue<0) or (AValue>1) then
    raise exception.Create('ValveBodyPercent accepts values between [0.0 .. 1.0]');
  FValveBodyPercent:=AValue;
  InvalidateShape;
end;

procedure THMICustomBasicValve.SetValveType(AValue: TValveType);
begin
  if FValveType=AValue then Exit;
  FValveType:=AValue;
  InvalidateShape;
end;

function THMICustomBasicValve.DrawingTransform: TDrawingTransform;
begin
  Result:=dtNone;
  if FMirrored and (ValveType<>vtPneumaticDrawer) then begin
    if Height<=Width then
      Result:=dtFlipVertical
    else
      Result:=dtFlipHorizontal;
  end;
end;

procedure THMICustomBasicValve.PaintControl(ACanvas: TCanvas; AWidth,
  AHeight: Integer);
var
  p:array of TPointF;
  alturaideal: real;
  larguraideal: real;
  larguradoquadrado: real;

  //o 'M' do motorizado, centrado em (cx, top) ou, de lado, em (left, cy)
  //the 'M' of the motorised one, centred on (cx, top) or, sideways, on
  //(left, cy)
  procedure DrawM(aSize, aOrientation:Integer; X, Y:Double);
  var
    ts: TSize;
  begin
    if aSize<1 then exit;
    ACanvas.Font.Height     :=aSize;
    ACanvas.Font.Orientation:=aOrientation;
    ACanvas.Font.Color      :=ShapeColor(FBorderColor);
    ACanvas.Brush.Style     :=bsClear;
    ts:=ACanvas.TextExtent('M');
    if aOrientation=0 then
      ACanvas.TextOut(Round(X-ts.cx/2), Round(Y), 'M')
    else
      //girado 90 graus, o texto cresce para cima a partir do ponto
      //rotated 90 degrees, the text grows upwards from the point
      ACanvas.TextOut(Round(X), Round(Y+ts.cx/2), 'M');
    ACanvas.Font.Orientation:=0;
    ACanvas.Brush.Style     :=bsSolid;
  end;

begin
  SetupCanvas(ACanvas, FBorderColor, FBorderWidth, FBodyColor);

  SetLength(p, 4);
  IF AWidth>=AHeight THEN BEGIN
    if ValveType=vtPneumaticDrawer then begin
      p[0].x:=0 + (FBorderWidth mod 2);
      p[0].y:=0 + (FBorderWidth mod 2);

      p[1].x:=AWidth - (FBorderWidth mod 2);
      p[1].y:=0 +     (FBorderWidth mod 2);

      p[2].x:=AWidth -  (FBorderWidth mod 2);
      p[2].y:=AHeight - (FBorderWidth mod 2);

      p[3].x:=0 + (FBorderWidth mod 2);
      p[3].y:=AHeight - (FBorderWidth mod 2);

      ACanvas.Polygon(RoundPoints(p));
    end else begin
      p[0].x:=FBorderWidth div 2 + FBorderWidth mod 2;
      p[0].y:=(1-FValveBodyPercent)*AHeight;

      p[1].x:=AWidth-(FBorderWidth div 2) - (FBorderWidth mod 2);
      p[1].y:=AHeight-(FBorderWidth/2);

      p[2].x:=AWidth-(FBorderWidth div 2) - (FBorderWidth mod 2);
      p[2].y:=(1-FValveBodyPercent)*AHeight;

      p[3].x:=FBorderWidth div 2 + FBorderWidth mod 2;
      p[3].y:=AHeight-(FBorderWidth/2);

      ACanvas.Polygon(RoundPoints(p));

      //risco
      case FValveType of
        vtPneumaticProportional,
        vtPneumaticOnOff,
        vtMotorisedProportional:
          ACanvas.Polyline(RoundPoints([PointF( ifthen(((AWidth+FBorderWidth) mod 2)=1,AWidth,AWidth+1)/2,
                                                     (FBorderWidth)),
                                             PointF( ifthen(((AWidth+FBorderWidth) mod 2)=1,AWidth,AWidth+1)/2,
                                                     (1-(FValveBodyPercent/2))*AHeight-(FBorderWidth/2))]));
      end;

      case FValveType of
        vtPneumaticProportional, vtMotorisedProportional: begin
          larguraideal:=AWidth/2-Fborderwidth;
          alturaideal:=(AWidth/4)*(Fvalvebodypercent*AHeight)/AWidth+((1-Fvalvebodypercent)*AHeight)-Fborderwidth;
          larguradoquadrado:=min(larguraideal, alturaideal);
          p[0].x:=(AWidth-larguradoquadrado)/2;
          p[0].y:=FBorderWidth div 2 + FBorderWidth mod 2;

          p[1].x:=AWidth-((AWidth-larguradoquadrado)/2);
          p[1].y:=p[0].y;

          p[2].x:=p[1].x;
          p[2].y:=larguradoquadrado+(FBorderWidth div 2 + FBorderWidth mod 2);

          p[3].x:=p[0].x;
          p[3].y:=p[2].y;
          ACanvas.Polygon(RoundPoints(p));

          if FValveType= vtMotorisedProportional then begin
            DrawM(trunc(larguradoquadrado * 0.9 - 2*Fborderwidth), 0,
                  AWidth/2, Trunc((larguradoquadrado-trunc(larguradoquadrado * 0.9 - 2*Fborderwidth))/2));

          end;
        end;
        vtPneumaticOnOff: begin
          //meia elipse para cima: centro, raios e angulos em 1/16 de grau,
          //no sentido anti-horario a partir das 3 horas
          //half ellipse upwards: centre, radii and angles in 1/16 degree,
          //counter-clockwise from 3 o'clock
          ACanvas.RadialPie(Round(AWidth/2 - AWidth/4),
                            Round(((1-FValveBodyPercent)*AHeight)+(FBorderWidth/2) - AHeight*(1-FValveBodyPercent)),
                            Round(AWidth/2 + AWidth/4),
                            Round(((1-FValveBodyPercent)*AHeight)+(FBorderWidth/2) + AHeight*(1-FValveBodyPercent)),
                            0, 180*16);
        end;
      end;
    end;
  end else begin
    if ValveType=vtPneumaticDrawer then begin
      p[0].x:=0 + (FBorderWidth mod 2);
      p[0].y:=0 + (FBorderWidth mod 2);

      p[1].x:=AWidth - (FBorderWidth mod 2);
      p[1].y:=0 +     (FBorderWidth mod 2);

      p[2].x:=AWidth -  (FBorderWidth mod 2);
      p[2].y:=AHeight - (FBorderWidth mod 2);

      p[3].x:=0 + (FBorderWidth mod 2);
      p[3].y:=AHeight - (FBorderWidth mod 2);

      ACanvas.Polygon(RoundPoints(p));
    end else begin
      p[0].x:=(1-FValveBodyPercent)*AWidth;
      p[0].y:=FBorderWidth div 2 + FBorderWidth mod 2;

      p[1].x:=AWidth-(FBorderWidth/2);
      p[1].y:=AHeight-(FBorderWidth div 2) - (FBorderWidth mod 2);

      p[2].x:=(1-FValveBodyPercent)*AWidth;
      p[2].y:=AHeight-(FBorderWidth div 2) - (FBorderWidth mod 2);

      p[3].x:=AWidth-(FBorderWidth/2);
      p[3].y:=FBorderWidth div 2 + FBorderWidth mod 2;

      ACanvas.Polygon(RoundPoints(p));

      //risco
      case FValveType of
        vtPneumaticProportional,
        vtPneumaticOnOff,
        vtMotorisedProportional:
          ACanvas.Polyline(RoundPoints([PointF(FBorderWidth, ifthen(((AHeight+FBorderWidth) mod 2)=1,AHeight,AHeight+1)/2),
                                             PointF((1-(FValveBodyPercent/2))*AWidth-(BorderWidth/2), ifthen(((AHeight+FBorderWidth) mod 2)=1,AHeight,AHeight+1)/2)]));
      end;

      case FValveType of
        vtPneumaticProportional,vtMotorisedProportional: begin
          larguraideal:=AHeight/2-Fborderwidth;
          alturaideal:=(AHeight/4)*(Fvalvebodypercent*AWidth)/AHeight+((1-Fvalvebodypercent)*AWidth)-Fborderwidth;
          larguradoquadrado:=min(larguraideal, alturaideal);
          p[0].x:=FBorderWidth div 2 + FBorderWidth mod 2;
          p[0].y:=(AHeight-larguradoquadrado)/2;

          p[1].x:=larguradoquadrado+(FBorderWidth div 2 + FBorderWidth mod 2);
          p[1].y:= p[0].y;

          p[2].x:=p[1].x;
          p[2].y:=AHeight-((AHeight-larguradoquadrado)/2);

          p[3].x:=p[0].x;
          p[3].y:=p[2].y;
          ACanvas.Polygon(RoundPoints(p));

          if FValveType= vtMotorisedProportional then begin
            DrawM(trunc(larguradoquadrado * 0.9 - Fborderwidth), 900,
                  Trunc((larguradoquadrado-trunc(larguradoquadrado * 0.9 - Fborderwidth))/2), AHeight/2);

          end;
        end;
        vtPneumaticOnOff: begin
          //meia elipse para a esquerda / half ellipse to the left
          ACanvas.RadialPie(Round(((1-FValveBodyPercent)*AWidth)+(FBorderWidth/2) - AWidth*(1-FValveBodyPercent)),
                            Round(AHeight/2 - AHeight/4),
                            Round(((1-FValveBodyPercent)*AWidth)+(FBorderWidth/2) + AWidth*(1-FValveBodyPercent)),
                            Round(AHeight/2 + AHeight/4),
                            90*16, 180*16);
        end;
      end;
    end;
  end;
end;

constructor THMICustomBasicValve.Create(AOwner: TComponent);
begin
  FMirrored:=false;
  FValveBodyPercent:=0.7;
  inherited Create(AOwner);
end;

end.
