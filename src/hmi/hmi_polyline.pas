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
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner:TComponent);
    function Add:TPointCollectionItem;
  end;


  { THMIPolyline }

  THMIPolyline = class(THMIBasicControl)
  private
    procedure SetLineColor(AValue: TColor);
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
    property LineColor:TColor read FBorderColor write SetLineColor default clBlack;
    property LineWidth:Integer read FBorderWidth write SetBorderWidth default 2;
    property PointCoordinates:TPointCollection read FPointCoordinates write setPointCoordinates;
  end;

implementation

uses math;

{ TPointCollection }

procedure TPointCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if assigned(OnCollectionItemChange) then
    OnCollectionItemChange(Self);
end;

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

procedure THMIPolyline.SetLineColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;

  FBorderColor:=AValue;
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
  p:array of TPointF;
  i: Integer;
  afillcolor: TBGRAPixel;
  abordercolor: TBGRAPixel;
  pc: TPointCollectionItem;
begin
  inherited DrawControl;

  SetLength(p, FPointCoordinates.Count);
  for i:=0 to FPointCoordinates.Count-1 do begin
    pc:=TPointCollectionItem(FPointCoordinates.Items[i]);
    p[i].x:=pc.X + ifthen((FBorderWidth mod 2)=1, ifthen((pc.X mod 2)=0, 1), 0.5);
    p[i].y:=pc.Y + ifthen((FBorderWidth mod 2)=0, 0.5);
  end;

  FControlArea.DrawPolyLineAntialias(p, ColorToBGRA(FBorderColor),FBorderWidth);

  if csDesigning in ComponentState then begin
    if (FPointInfo='') or (FDesignDrawing=false) then exit;
    afillcolor:=ColorToBGRA(FBodyColor);
    abordercolor:=ColorToBGRA(FBorderColor);

    afillcolor.alpha:=160;
    abordercolor.alpha:=160;

    FControlArea.Rectangle(Width-FPointInfoWidth-5,
                           -1,
                           Width + 1,
                           FControlArea.CanvasBGRA.TextHeight(FPointInfo)+5,
                           abordercolor,
                           afillcolor,
                           dmDrawWithTransparency);
    FControlArea.TextOut(Width-2,1, FPointInfo,ColorToBGRA(FBorderColor),taRightJustify);
  end;
end;

procedure THMIPolyline.OptimizeDraw;
var
  p: Integer;
  minx,
  maxx,
  miny,
  maxy: Integer;
  pc: TPointCollectionItem;
begin
  BeginUpdate;
  try
    for p:=0 to PointCoordinates.Count-1 do begin
      pc:=TPointCollectionItem(PointCoordinates.Items[p]);
      if p=0 then begin
        minx:=pc.X;
        maxx:=minx;
        miny:=pc.Y;
        maxy:=miny;
        continue;
      end;
      minx:=min(pc.X, minx);
      maxx:=max(pc.X, maxx);

      miny:=min(pc.Y, miny);
      maxy:=max(pc.Y, maxy);
    end;


    DisableAutoSizing;
    Left:=Max(minx,0);
    Top :=Max(miny-ifthen((miny mod 2)=0,1),0);
    Width :=(maxx-minx)+FBorderWidth;
    Height:=(maxy-miny)+FBorderWidth;
    EnableAutoSizing;
    for p:=0 to PointCoordinates.Count-1 do begin
      pc:=TPointCollectionItem(PointCoordinates.Items[p]);
      pc.X:=pc.X-minx;
      pc.Y:=pc.Y-miny+ifthen((miny mod 2)=0,1);
    end;
  finally
    EndUpdate;
  end;
end;

procedure THMIPolyline.BeginDrawPolyline;
var
  p: Integer;
  pc: TPointCollectionItem;
begin
  if FDesignDrawing then exit;
  BeginUpdate;
  try
    for p:=0 to PointCoordinates.Count-1 do begin
      pc:=TPointCollectionItem(PointCoordinates.Items[p]);
      pc.X:=pc.X+Left;
      pc.Y:=pc.y+Top;
    end;
    FOldAlign:=Align;
    Align:=alClient;
    FDesignDrawing:=true;
  finally
    EndUpdate;
  end;

end;

procedure THMIPolyline.BeginEmptyPolyline;
begin
  if FDesignDrawing then exit;
  BeginUpdate;
  try
    PointCoordinates.Clear;
    FOldAlign:=Align;
    Align:=alClient;
    FDesignDrawing:=true;
  finally
    EndUpdate;
  end;
end;

procedure THMIPolyline.EndDrawPolyline;
begin
  Align:=FOldAlign;
  FDesignDrawing:=False;
  OptimizeDraw;
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
  FBorderWidth:=2;
  FCtrlOnLastMouseMove:=false;
  FBorderColor:=clBlack;
end;

destructor THMIPolyline.Destroy;
begin
  FreeAndNil(FPointCoordinates);
  inherited Destroy;
end;

end.

