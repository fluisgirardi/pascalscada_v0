unit hmi_polyline;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basiccontrol, BGRABitmap,
  BGRABitmapTypes, hmibasiccolletion;

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
    procedure DrawControl; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LineColor:TColor read FLineColor write SetLineColor default clBlack;
    property LineWidth:Integer read FLineWidth write SetLineWidth default 2;
    property PointCoordinates:TPointCollection read FPointCoordinates write setPointCoordinates;
  end;

implementation

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
    DrawControl;
    UpdateShape;
    Invalidate;
  end;
end;

procedure THMIPolyline.SetLineWidth(AValue: Integer);
begin
  if FLineWidth=AValue then Exit;
  FLineWidth:=AValue;
  if [csLoading, csReading]*ComponentState=[] then begin
    DrawControl;
    UpdateShape;
    Invalidate;
  end;
end;

procedure THMIPolyline.SetLineColor(AValue: TColor);
//var
//  NeedUpdateShape: Boolean;
begin
  if FLineColor=AValue then Exit;

  //NeedUpdateShape:=(AValue=clNone) or (FLineColor=clNone);

  FLineColor:=AValue;
  Visible:=(AValue<>clNone);
  if not Visible then exit;

  if [csLoading, csReading]*ComponentState=[] then begin
    DrawControl;
    //if NeedUpdateShape then UpdateShape;
    Invalidate;
  end;
end;

procedure THMIPolyline.setPointCoordinates(AValue: TPointCollection);
begin
  FPointCoordinates.Assign(AValue);
  if [csLoading, csReading]*ComponentState=[] then begin
    DrawControl;
    UpdateShape;
    Invalidate;
  end;
end;

procedure THMIPolyline.DrawControl;
var
  p:array of TPoint;
  aColor: TBGRAPixel;
  i: Integer;
  emptyArea: TBGRABitmap;
begin
  emptyArea := TBGRABitmap.Create(Width,Height);
  try
    FControlArea.Assign(emptyArea);
  finally
    FreeAndNil(emptyArea);
  end;

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
end;

procedure THMIPolyline.Loaded;
begin
  inherited Loaded;
end;

constructor THMIPolyline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPointCoordinates:=TPointCollection.Create(Self);
  FPointCoordinates.OnCollectionItemChange:=@PointChanged;
  FPointCoordinates.OnNeedCompState:=@CollectionNeedsComponentState;
  FLineWidth:=2;
  FLineColor:=clBlack;
  FOnlyColorChanged:=false;
end;

destructor THMIPolyline.Destroy;
begin
  FreeAndNil(FPointCoordinates);
  inherited Destroy;
end;

end.

