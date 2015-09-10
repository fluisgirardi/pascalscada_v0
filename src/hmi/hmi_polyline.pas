unit hmi_polyline;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basiccontrol, BGRABitmap,
  BGRABitmapTypes, hmibasiccolletion, LMessages;

type

  { TPointCollectionItem }

  TPointCollectionItem = class(THMIBasicColletionItem)
  private
    FX: Integer;
    FY: Integer;
    procedure setX(AValue: Integer);
    procedure setY(AValue: Integer);
  protected
    function GetDisplayName: string; override;
  published
    property X:Integer read FX write setX;
    property Y:Integer read FY write setY;
  end;

  TPointCollection = class(THMIBasicColletion)
  public
    constructor Create(AOwner:TComponent);
    function Add:TPointCollectionItem;
  end;


  { THMIPolyline }

  THMIPolyline = class(THMIBasicControl)
  private
    FLineColor: TColor;
    FLineWidth: Integer;
    FOnlyColorChanged:Boolean;
    procedure SetLineColor(AValue: TColor);
    procedure SetLineWidth(AValue: Integer);
    procedure setPointCoordinates(AValue: TPointCollection);
    procedure CollectionNeedsComponentState(var CurState: TComponentState);
    procedure PointChanged(Sender: TObject);
  protected
    FPointCoordinates:TPointCollection;
    FDesignDrawing:Boolean;
    FCtrlOnLastMouseMove:Boolean;
    FPointInfo:String;
    FPointInfoWidth:Integer;
    FOldAlign:TAlign;

    procedure DrawControl; override;

    procedure BeginDrawPolyline; virtual;
    procedure BeginEmptyPolyline; virtual;
    procedure EndDrawPolyline; virtual;
    procedure OptimizeDraw; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure CMDesignHitTest(var Message: TLMessage); message CM_DESIGNHITTEST;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LineColor:TColor read FLineColor write SetLineColor default clBlack;
    property LineWidth:Integer read FLineWidth write SetLineWidth default 2;
    property PointCoordinates:TPointCollection read FPointCoordinates write setPointCoordinates;
  end;

implementation

uses math, windows;

{ TPointCollection }

constructor TPointCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TPointCollectionItem);
end;

function TPointCollection.Add: TPointCollectionItem;
begin
  Result:=TPointCollectionItem(inherited Add);
end;

{ TPointCollectionItem }

procedure TPointCollectionItem.setX(AValue: Integer);
begin
  if FX=AValue then Exit;
  FX:=AValue;
  NotifyChange;
end;

procedure TPointCollectionItem.setY(AValue: Integer);
begin
  if FY=AValue then Exit;
  FY:=AValue;
  NotifyChange;
end;

function TPointCollectionItem.GetDisplayName: string;
begin
  Result:='(x='+IntToStr(FX)+', y='+IntToStr(FY)+')';
end;

{ THMIPolyline }

procedure THMIPolyline.CollectionNeedsComponentState(
  var CurState: TComponentState);
begin
  CurState:=ComponentState;
end;

procedure THMIPolyline.PointChanged(Sender: TObject);
begin
  if [csLoading, csReading]*ComponentState=[] then begin
    InvalidateShape;
  end;
end;

procedure THMIPolyline.SetLineWidth(AValue: Integer);
begin
  if FLineWidth=AValue then Exit;
  FLineWidth:=AValue;
  if [csLoading, csReading]*ComponentState=[] then begin
    InvalidateShape;
  end;
end;

procedure THMIPolyline.SetLineColor(AValue: TColor);
//var
//  NeedUpdateShape: Boolean;
begin
  if FLineColor=AValue then Exit;

  FLineColor:=AValue;
  Visible:=(AValue<>clNone);
  if not Visible then exit;

  if [csLoading, csReading]*ComponentState=[] then begin
    InvalidateDraw;
  end;
end;

procedure THMIPolyline.setPointCoordinates(AValue: TPointCollection);
begin
  FPointCoordinates.Assign(AValue);
  if [csLoading, csReading]*ComponentState=[] then begin
    InvalidateShape;
  end;
end;

procedure THMIPolyline.DrawControl;
var
  p:array of TPoint;
  aColor: TBGRAPixel;
  i: Integer;
  afillcolor: TBGRAPixel;
  abordercolor: TBGRAPixel;
begin
  inherited DrawControl;

  SetLength(p, FPointCoordinates.Count);
  for i:=0 to FPointCoordinates.Count-1 do begin
    p[i].x:=TPointCollectionItem(FPointCoordinates.Items[i]).X;
    p[i].y:=TPointCollectionItem(FPointCoordinates.Items[i]).Y;
  end;

  aColor:=ColorToBGRA(FLineColor);
  FControlArea.CanvasBGRA.Pen.Style:=psSolid;
  FControlArea.CanvasBGRA.Pen.Color:=FLineColor;
  FControlArea.CanvasBGRA.Pen.Width:=FLineWidth;
  FControlArea.CanvasBGRA.Polyline(p);

  if csDesigning in ComponentState then begin
    if FPointInfo='' then exit;
    afillcolor:=ColorToBGRA(FBodyColor);
    abordercolor:=ColorToBGRA(FBorderColor);

    afillcolor.alpha:=160;
    abordercolor.alpha:=160;

    FControlArea.Rectangle(Width-FPointInfoWidth-5,
                           0,
                           Width,
                           FControlArea.CanvasBGRA.TextHeight(FPointInfo)+5,
                           abordercolor,
                           afillcolor,
                           dmSet);
    FControlArea.TextOut(Width,0, FPointInfo,ColorToBGRA(FBorderColor),taRightJustify);
  end;
end;

procedure THMIPolyline.BeginDrawPolyline;
var
  p: Integer;
begin
  if FDesignDrawing then exit;
  BeginUpdate;
  for p:=0 to PointCoordinates.Count-1 do begin
    TPointCollectionItem(PointCoordinates.Items[p]).X:=TPointCollectionItem(PointCoordinates.Items[p]).X+Left;
    TPointCollectionItem(PointCoordinates.Items[p]).Y:=TPointCollectionItem(PointCoordinates.Items[p]).y+Top;
  end;
  FOldAlign:=Align;
  Align:=alClient;
  FDesignDrawing:=true;
  EndUpdate;
end;

procedure THMIPolyline.BeginEmptyPolyline;
begin
  if FDesignDrawing then exit;
  BeginUpdate;
  PointCoordinates.Clear;
  FOldAlign:=Align;
  Align:=alClient;
  FDesignDrawing:=true;
  EndUpdate;
end;

procedure THMIPolyline.EndDrawPolyline;
begin
  Align:=FOldAlign;
  FDesignDrawing:=False;
  OptimizeDraw;
end;

procedure THMIPolyline.OptimizeDraw;
var
  p: Integer;
  minx,
  maxx,
  miny,
  maxy: Integer;
begin
  BeginUpdate;
  for p:=0 to PointCoordinates.Count-1 do begin
    if p=0 then begin
      minx:=TPointCollectionItem(PointCoordinates.Items[p]).X;
      maxx:=minx;
      miny:=TPointCollectionItem(PointCoordinates.Items[p]).Y;
      maxy:=miny;
      continue;
    end;
    minx:=min(TPointCollectionItem(PointCoordinates.Items[p]).X, minx);
    maxx:=max(TPointCollectionItem(PointCoordinates.Items[p]).X, maxx);

    miny:=min(TPointCollectionItem(PointCoordinates.Items[p]).Y, miny);
    maxy:=max(TPointCollectionItem(PointCoordinates.Items[p]).Y, maxy);
  end;

  Left:=Max(minx-(FBorderWidth div 2),0);
  Top :=Max(miny-(FBorderWidth div 2),0);
  Width :=(maxx-minx)+(FBorderWidth div 2) + (FBorderWidth mod 2);
  Height:=(maxy-miny)+(FBorderWidth div 2) + (FBorderWidth mod 2);

  for p:=0 to PointCoordinates.Count-1 do begin
    TPointCollectionItem(PointCoordinates.Items[p]).X:=TPointCollectionItem(PointCoordinates.Items[p]).X-minx;
    TPointCollectionItem(PointCoordinates.Items[p]).Y:=TPointCollectionItem(PointCoordinates.Items[p]).Y-miny;
  end;
  EndUpdate;
end;

procedure THMIPolyline.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ponto: TPointCollectionItem;
  lastpoint: TPointCollectionItem;
  p45: LongInt;
  curDegrees: Double;


begin
  if csDesigning in ComponentState then begin
    if Shift=[ssShift, ssLeft] then begin
      EndDrawPolyline;
      exit;
    end;


    if PointCoordinates.Count=0 then
      lastpoint:=nil
    else
      lastpoint:=TPointCollectionItem(PointCoordinates.Items[PointCoordinates.Count-1]);

    if (lastpoint<>nil) AND (Shift=[ssCtrl, ssLeft]) then begin
      ponto:=FPointCoordinates.Add;
      curDegrees:=Degrees(X, lastpoint.X, Y, lastpoint.Y);
      case Trunc(curDegrees) of
        0..15: begin     //0 degrees
          ponto.Y:=lastpoint.Y;
          ponto.X:=X;
        end;
        16..37, 54..75: begin    //30 e 60 degrees
          if Y>=lastpoint.Y then
            ponto.Y:=lastpoint.Y+Trunc(sin(degtorad(curDegrees))*Hipotenusa(X, lastpoint.X, Y, lastpoint.Y))
          else
            ponto.Y:=lastpoint.Y-Trunc(sin(degtorad(curDegrees))*Hipotenusa(X, lastpoint.X, Y, lastpoint.Y));
          ponto.X:=X;
        end;
        38..53: begin    //45 degrees
          p45:=Max(Cateto(X,lastpoint.X), Cateto(Y,lastpoint.Y));
          if X>=lastpoint.X then
            ponto.X:=lastpoint.X+p45
          else
            ponto.X:=lastpoint.X-p45;

          if Y>=lastpoint.Y then
            ponto.Y:=lastpoint.Y+p45
          else
            ponto.Y:=lastpoint.Y-p45;

        end;
        76..90: begin    //30 degrees
          ponto.X:=lastpoint.X;
          ponto.Y:=Y;
        end;
      end;
    end;

    if (lastpoint=nil) OR (Shift=[ssLeft]) then begin
      ponto:=FPointCoordinates.Add;
      ponto.X:=X;
      ponto.Y:=Y;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure THMIPolyline.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lastpoint: TPointCollectionItem;
  tw: Integer;
begin
  if csDesigning in ComponentState then begin;
    if PointCoordinates.Count=0 then
      lastpoint:=nil
    else
      lastpoint:=TPointCollectionItem(PointCoordinates.Items[PointCoordinates.Count-1]);

    if Shift=[ssCtrl] then begin
      FCtrlOnLastMouseMove:=true;

      if lastpoint=nil then
        FPointInfo:=Format('x=%d, y=%d',[X,Y])
      else
        FPointInfo:=Format('x=%d, y=%d, Degrees=%d',[X,Y,Trunc(Degrees(x,lastpoint.X,Y,lastpoint.Y))]);

      tw:=FControlArea.CanvasBGRA.TextWidth(FPointInfo);
      if tw=FPointInfoWidth then
        InvalidateDraw
      else begin
        FPointInfoWidth:=tw;
        InvalidateShape;
      end;

    end else begin
      if FCtrlOnLastMouseMove then begin
        FCtrlOnLastMouseMove:=false;
        FPointInfo:='';
        FPointInfoWidth:=0;
        InvalidateShape;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure THMIPolyline.CMDesignHitTest(var Message: TLMessage);
begin
  if FDesignDrawing then
    Message.Result:=1;
end;

constructor THMIPolyline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPointCoordinates:=TPointCollection.Create(Self);
  FPointCoordinates.OnCollectionItemChange:=@PointChanged;
  FPointCoordinates.OnNeedCompState:=@CollectionNeedsComponentState;
  FLineWidth:=2;
  FCtrlOnLastMouseMove:=false;
  FLineColor:=clBlack;
  FOnlyColorChanged:=false;
end;

destructor THMIPolyline.Destroy;
begin
  FreeAndNil(FPointCoordinates);
  inherited Destroy;
end;

end.

