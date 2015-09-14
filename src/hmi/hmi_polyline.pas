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

    procedure SetBorderWidth(AValue: Integer); override;
    procedure SetLineColor(AValue: TColor); virtual;

    procedure DoLineColorChange; virtual;

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

  THMIFlowPolyline = class;

  IColorChangeNotification = interface
  ['{7A61F363-CBE0-4271-8E5E-BBDCE708084E}']
    procedure AddNotifyCallback(WhoNotify:IColorChangeNotification);
    procedure RemoveNotifyCallback(WhoRemove:IColorChangeNotification);
    procedure NotifyFree(const WhoWasDestroyed:THMIFlowPolyline);
    procedure NotifyChange(const WhoChanged:THMIFlowPolyline);
  end;

  { THMIFlowSourceCollectionItem }

  THMIFlowSourceCollectionItem = class(THMIBasicColletionItem)
  private
    FHMIObject: THMIFlowPolyline;
    procedure setHMIObject(AValue: THMIFlowPolyline);
  published
    property HMIObject:THMIFlowPolyline read FHMIObject write setHMIObject;
  end;


  THMIFlowSourceCollection = Class(THMIBasicColletion)
  public
    constructor Create(AOwner:TComponent);
    function Add:THMIFlowSourceCollectionItem;
  end;

  TColorMixBehavior = (cmbLastColor, cmbAnd, cmbOr, cmbXor, cmbEmpty, cmbMultipleColorsReplace);

  { THMIFlowPolyline }

  THMIFlowPolyline = class(THMIPolyline, IColorChangeNotification)
  private
    FEmptyColor: TColor;
    FFlowSources: THMIFlowSourceCollection;

    FFlowDest:array of IColorChangeNotification;
    FMultipleColorBehavior: TColorMixBehavior;
    FMultipleColorsReplace: TColor;

    procedure AddNotifyCallback(WhoNotify:IColorChangeNotification);
    procedure RemoveNotifyCallback(WhoRemove:IColorChangeNotification);
    procedure NotifyFree(const WhoWasDestroyed:THMIFlowPolyline);
    procedure NotifyChange(const WhoChanged:THMIFlowPolyline);
    procedure SetEmptyColor(AValue: TColor);
    procedure setFlowSources(AValue: THMIFlowSourceCollection);
    procedure setMultipleColorsBehavior(AValue: TColorMixBehavior);
    procedure setMultipleColorsReplace(AValue: TColor);
  protected
    procedure DoLineColorChange; override;
    procedure RecalculateColor(WhoChanged:THMIFlowPolyline=nil); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property EmptyColor:TColor read FEmptyColor write SetEmptyColor default clBlack;
    property MultipleColorBehavior:TColorMixBehavior read FMultipleColorBehavior write setMultipleColorsBehavior;
    property MultipleColorsReplace:TColor read FMultipleColorsReplace write setMultipleColorsReplace;
    property FlowSource:THMIFlowSourceCollection read FFlowSources write setFlowSources;
  end;

implementation

uses math;

{ THMIFlowPolyline }

procedure THMIFlowPolyline.AddNotifyCallback(WhoNotify: IColorChangeNotification
  );
var
  i: Integer;
begin
  for i:=0 to High(FFlowDest) do
    if FFlowDest[i]=WhoNotify then
      exit;

  i:=Length(FFlowDest);
  SetLength(FFlowDest,i+1);
  FFlowDest[i]:=WhoNotify;
end;

procedure THMIFlowPolyline.RemoveNotifyCallback(
  WhoRemove: IColorChangeNotification);
var
  h, i: Integer;
  found:Boolean;
begin
  h:=High(FFlowDest);
  for i:=0 to h do
    if FFlowDest[i]=WhoRemove then begin
      found:=true;
      break;
    end;

  if found then begin
    FFlowDest[i]:=FFlowDest[h];
    SetLength(FFlowDest,h);
  end;
end;

procedure THMIFlowPolyline.NotifyFree(const WhoWasDestroyed: THMIFlowPolyline);
var
  i: Integer;
begin
  for i:=0 to FFlowSources.Count-1 do begin
    if THMIFlowSourceCollectionItem(FFlowSources.Items[i]).HMIObject = WhoWasDestroyed then begin
      FFlowSources.Delete(i);
      break;
    end;
  end;
end;

procedure THMIFlowPolyline.NotifyChange(const WhoChanged: THMIFlowPolyline);
begin
  RecalculateColor(WhoChanged);
end;

procedure THMIFlowPolyline.SetEmptyColor(AValue: TColor);
begin
  if FEmptyColor=AValue then Exit;
  FEmptyColor:=AValue;
  RecalculateColor;
end;

procedure THMIFlowPolyline.setFlowSources(AValue: THMIFlowSourceCollection);
begin
  if FFlowSources=AValue then Exit;
  FFlowSources.Assign(AValue);
end;

procedure THMIFlowPolyline.setMultipleColorsBehavior(AValue: TColorMixBehavior);
begin
  if FMultipleColorBehavior=AValue then Exit;
  FMultipleColorBehavior:=AValue;
  RecalculateColor;
end;

procedure THMIFlowPolyline.setMultipleColorsReplace(AValue: TColor);
begin
  if FMultipleColorsReplace=AValue then Exit;
  FMultipleColorsReplace:=AValue;
  RecalculateColor;
end;

procedure THMIFlowPolyline.DoLineColorChange;
var
  i: Integer;
begin
  for i:=0 to High(FFlowDest) do
    try
      FFlowDest[i].NotifyChange(Self);
    finally
    end;
end;

procedure THMIFlowPolyline.RecalculateColor(WhoChanged: THMIFlowPolyline);
var
  fs, validObject: THMIFlowSourceCollectionItem;
  i: Integer;
  MultipleColors: Boolean;
  CalculatedColor: TColor;
begin
  validObject:=nil;
  fs:=nil;
  MultipleColors:=false;
  for i:=0 to FFlowSources.Count-1 do begin
   fs:=THMIFlowSourceCollectionItem(FFlowSources.Items[i]);
    if fs.HMIObject.LineColor=FEmptyColor then continue;

    if validObject=nil then
      validObject:=fs
    else begin
      if validObject.HMIObject.LineColor<>fs.HMIObject.LineColor then begin
        MultipleColors:=true;
        break;
      end;
    end;
  end;

  if MultipleColors then
    case FMultipleColorBehavior of
      cmbLastColor:
        if Assigned(WhoChanged) and (WhoChanged.LineColor<>FEmptyColor) then
          LineColor:=WhoChanged.LineColor;
      cmbEmpty:
        LineColor:=FEmptyColor;
      cmbMultipleColorsReplace:
        LineColor:=FMultipleColorsReplace;
      cmbAnd,
      cmbOr,
      cmbXor: begin
        CalculatedColor:=FEmptyColor;
        for i:=0 to FFlowSources.Count-1 do begin
          fs:=THMIFlowSourceCollectionItem(FFlowSources.Items[i]);
          if fs.HMIObject.LineColor=FEmptyColor then continue;
          if CalculatedColor=FEmptyColor then
            CalculatedColor:=fs.HMIObject.LineColor
          else
            case FMultipleColorBehavior of
              cmbAnd:
                CalculatedColor:=CalculatedColor AND fs.HMIObject.LineColor;
              cmbOr:
                CalculatedColor:=CalculatedColor OR  fs.HMIObject.LineColor;
              cmbXor:
                CalculatedColor:=CalculatedColor XOR fs.HMIObject.LineColor;
            end;
        end;
        LineColor:=CalculatedColor;
      end;
    end
  else
    if Assigned(fs) and Assigned(fs.HMIObject) then
      LineColor:=fs.HMIObject.LineColor;

end;

constructor THMIFlowPolyline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlowSources:=THMIFlowSourceCollection.Create(Self);
end;

destructor THMIFlowPolyline.Destroy;
var
  i: Integer;
begin
  for i:=0 to High(FFlowDest) do
    FFlowDest[i].NotifyFree(Self);
  for i:=0 to FFlowSources.Count-1 do
    THMIFlowSourceCollectionItem(FFlowSources.Items[i]).HMIObject:=nil;
  FreeAndNil(FFlowSources);
  inherited Destroy;
end;

{ THMIFlowObjectCollection }

constructor THMIFlowSourceCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,THMIFlowSourceCollectionItem);
end;

function THMIFlowSourceCollection.Add: THMIFlowSourceCollectionItem;
begin
  Result:=THMIFlowSourceCollectionItem(inherited Add);
end;

{ THMIFlowObjectCollectionItem }

procedure THMIFlowSourceCollectionItem.setHMIObject(AValue: THMIFlowPolyline);
begin
  if FHMIObject=AValue then Exit;
  if supports(Collection.Owner, IColorChangeNotification) then begin
    if Assigned(FHMIObject) then
      (FHMIObject as IColorChangeNotification).RemoveNotifyCallback(Collection.Owner as IColorChangeNotification);

    if AValue=nil then begin
      FHMIObject:=AValue
    end else begin
      if Supports(AValue, IColorChangeNotification) then begin
        (AValue as IColorChangeNotification).AddNotifyCallback(Collection.Owner as IColorChangeNotification);
        FHMIObject:=AValue;
      end else
        raise Exception.Create('Object don´t support the IColorChangeNotification interface!');
    end;
  end;
end;

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

  try
    DoLineColorChange;
  finally
  end;

  if not Visible then exit;

  if [csLoading, csReading]*ComponentState=[] then begin
    InvalidateDraw;
  end;
end;

procedure THMIPolyline.DoLineColorChange;
begin
end;

procedure THMIPolyline.SetBorderWidth(AValue: Integer);
begin
  inherited;
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

