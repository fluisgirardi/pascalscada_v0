unit HMIFlowImage;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, ExtCtrls, hmi_polyline;

type

  TPointPersistent = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FPoint:TPoint;
    function GetX: Integer;
    function GetY: Integer;
    procedure SetX(AValue: Integer);
    procedure SetY(AValue: Integer);
  protected
    procedure DoOnChange; virtual;
  public
    function Point:TPoint;
    procedure Assign(Source: TPersistent); override;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  published
    property X:Integer read GetX write SetX;
    property Y:Integer read GetY write SetY;
  end;

  { THMIFlowImage }

  THMIFlowImage = class(TImage)
  private
    procedure PointChanges(Sender: TObject);
    procedure SetInputPoint(AValue: TPointPersistent);
    procedure SetOutputPoint(AValue: TPointPersistent);
  protected
    FInputFlowPolyline: THMIFlowPolyline;
    FInputPoint: TPointPersistent;
    FOutputFlowPolyline: THMIFlowPolyline;
    FOutputPoint: TPointPersistent;
    procedure UpdateInOutLines; virtual;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer; KeepBase: Boolean
  ); override;
    procedure SetInputFlowPolyline(AValue: THMIFlowPolyline);
    procedure SetOutputFlowPolyline(AValue: THMIFlowPolyline);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property InputPoint:TPointPersistent read FInputPoint write SetInputPoint;
    property InputFlowPolyline:THMIFlowPolyline read FInputFlowPolyline write SetInputFlowPolyline;
    property OutputPoint:TPointPersistent read FOutputPoint write SetOutputPoint;
    property OutputFlowPolyline:THMIFlowPolyline read FOutputFlowPolyline write SetOutputFlowPolyline;
  end;

implementation

{ TPointPersistent }

function TPointPersistent.GetX: Integer;
begin
  Result:=FPoint.X;
end;

function TPointPersistent.GetY: Integer;
begin
  Result:=FPoint.Y;
end;

procedure TPointPersistent.SetX(AValue: Integer);
begin
  if AValue=FPoint.X then exit;
  FPoint.X:=AValue;
  DoOnChange;
end;

procedure TPointPersistent.SetY(AValue: Integer);
begin
  if AValue=FPoint.Y then exit;
  FPoint.Y:=AValue;
  DoOnChange;
end;

procedure TPointPersistent.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TPointPersistent.Point: TPoint;
begin
  Result:=FPoint;
end;

procedure TPointPersistent.Assign(Source: TPersistent);
var
  sourceAsTPP:TPointPersistent absolute Source;
begin
  if Source is TPointPersistent then begin
    if (sourceAsTPP.FPoint.X<>FPoint.X) or (sourceAsTPP.FPoint.Y<>FPoint.Y) then begin
      FPoint:=sourceAsTPP.FPoint;
      DoOnChange;
    end;
  end;
end;

{ THMIFlowImage }

procedure THMIFlowImage.PointChanges(Sender: TObject);
begin
  UpdateInOutLines;
end;

procedure THMIFlowImage.SetInputPoint(AValue: TPointPersistent);
begin
  FInputPoint.Assign(AValue);
end;

procedure THMIFlowImage.SetOutputPoint(AValue: TPointPersistent);
begin
  FOutputPoint.Assign(AValue);
end;

procedure THMIFlowImage.UpdateInOutLines;
begin
  if Assigned(FInputFlowPolyline) then
    FInputFlowPolyline.UpdateEndPoints(true,ControlToScreen(FInputPoint.Point));

  if Assigned(FOutputFlowPolyline) then
    FOutputFlowPolyline.UpdateEndPoints(false,ControlToScreen(FOutputPoint.Point));
end;

procedure THMIFlowImage.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer;
  KeepBase: Boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  UpdateInOutLines;
end;

procedure THMIFlowImage.SetInputFlowPolyline(AValue: THMIFlowPolyline);
begin
  if FInputFlowPolyline=AValue then Exit;
  if Assigned(FInputFlowPolyline) then FInputFlowPolyline.RemoveFreeNotification(Self);
  if Assigned(AValue) then AValue.FreeNotification(Self);
  FInputFlowPolyline:=AValue;
  UpdateInOutLines;
end;

procedure THMIFlowImage.SetOutputFlowPolyline(AValue: THMIFlowPolyline);
begin
  if FOutputFlowPolyline=AValue then Exit;
  if Assigned(FOutputFlowPolyline) then FOutputFlowPolyline.RemoveFreeNotification(Self);
  if Assigned(AValue) then AValue.FreeNotification(Self);
  FOutputFlowPolyline:=AValue;
  UpdateInOutLines;
end;

constructor THMIFlowImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInputFlowPolyline:=nil;
  FOutputFlowPolyline:=nil;
  FInputPoint:=TPointPersistent.Create;
  FInputPoint.OnChange:=@PointChanges;
  FOutputPoint:=TPointPersistent.Create;
  FOutputPoint.OnChange:=@PointChanges;
end;

destructor THMIFlowImage.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FInputPoint);
  FreeAndNil(FOutputPoint);
end;

end.
