unit hmi_draw_elevador;

{$mode objfpc}{$H+}

interface

uses
  Controls, sysutils, Graphics, Classes, hmi_draw_basiccontrol, BGRABitmap,
  BGRABitmapTypes, hmi_polyline, hmi_flow_zones, ExtCtrls, PLCTag, HMIZones;

type

  THMICustomElevadorBasico = class(THMIBasicControl)
  protected
    FBodyWidth: Byte;
    FFooterColor: TColor;
    FHeadColor: TColor;
    procedure SetBodyWidth(AValue: Byte);
    procedure SetFooterColor(AValue: TColor);
    procedure SetHeadColor(AValue: TColor);
    procedure DrawControl; override;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    property HeadColor:TColor read FHeadColor write SetHeadColor default clSilver;
    property BodyColor default clSilver;
    property FooterColor:TColor read FFooterColor write SetFooterColor default clSilver;
    property BorderColor;

    property BodyWidth:Byte read FBodyWidth write SetBodyWidth default 12;
    property BorderWidth default 1;
  end;

  THMIElevadorBasico = class(THMICustomElevadorBasico)
  published
    property HeadColor;
    property BodyColor;
    property BodyWidth;
    property FooterColor;
    property BorderColor;

    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
  end;

  { THMIElevatorFlowZone }

  THMIElevatorFlowZone = class(THMIFlowZone)
  private
    FEmptyColor: TColor;
    FPaintBodyWithFlowColor: Boolean;
    FPaintFooterWithFlowColor: Boolean;
    FPaintHeaderWithFlowColor: Boolean;
    procedure SetEmptyColor(AValue: TColor);
    procedure SetPaintBodyWithFlowColor(AValue: Boolean);
    procedure SetPaintFooterWithFlowColor(AValue: Boolean);
    procedure SetPaintHeaderWithFlowColor(AValue: Boolean);
  public
    constructor Create(aCollection: TCollection); override;
  published
    property EmptyColor:TColor read FEmptyColor write SetEmptyColor;
    property PaintHeaderWithFlowColor:Boolean read FPaintHeaderWithFlowColor write SetPaintHeaderWithFlowColor;
    property PaintBodyWithFlowColor:Boolean read FPaintBodyWithFlowColor write SetPaintBodyWithFlowColor;
    property PaintFooterWithFlowColor:Boolean read FPaintFooterWithFlowColor write SetPaintFooterWithFlowColor;
  end;

  { THMIElevatorFlowZones }

  THMIElevatorFlowZones = class(TZones)
    //: @exclude
    constructor Create(aOwner:TPersistent);

    {$IFDEF PORTUGUES}
    //: Adiciona uma nova zona de cor.
    {$ELSE}
    //: Adds a new color and flow zone into the collection.
    {$ENDIF}
    function Add:THMIElevatorFlowZone;
  end;


  { THMICustomFlowElevator }

  THMICustomFlowElevator = class(THMICustomElevadorBasico, IColorChangeNotification)
  private
    FUseStaticBodyColor: Boolean;
    FUseStaticFooterColor: Boolean;
    FUseStaticHeaderColor: Boolean;
    procedure SetUseStaticBodyColor(AValue: Boolean);
    procedure SetUseStaticFooterColor(AValue: Boolean);
    procedure SetUseStaticHeaderColor(AValue: Boolean);
  protected
    procedure AddNotifyCallback(WhoNotify:IColorChangeNotification);
    procedure RemoveNotifyCallback(WhoRemove:IColorChangeNotification);
    procedure NotifyFree(const WhoWasDestroyed:THMIFlowPolyline);
    procedure NotifyChange(const WhoChanged:THMIFlowPolyline);
  protected
    FInputPolyline: THMIFlowPolyline;
    FOutputPolyline: THMIFlowPolyline;
    FElevatorStates: THMIElevatorFlowZones;
    FCurrentZone,
    FOwnerZone: THMIElevatorFlowZone;
    FZoneTimer:TTimer;
    procedure SetInputPolyline(AValue: THMIFlowPolyline);
    procedure SetOutputPolyline(AValue: THMIFlowPolyline);
    procedure SetElevatorStates(AValue: THMIElevatorFlowZones);
    procedure ShowZone(aZone:THMIElevatorFlowZone);
    procedure UpdateFlow; virtual;
    property  InputPolyline:THMIFlowPolyline read FInputPolyline write SetInputPolyline;
    property  OutputPolyline:THMIFlowPolyline read FOutputPolyline write SetOutputPolyline;
    property  ColorAndFlowStates:THMIElevatorFlowZones read FElevatorStates write SetElevatorStates;
    property  UseStaticHeaderColor:Boolean read FUseStaticHeaderColor write SetUseStaticHeaderColor default false;
    property  UseStaticBodyColor:Boolean   read FUseStaticBodyColor   write SetUseStaticBodyColor   default false;
    property  UseStaticFooterColor:Boolean read FUseStaticFooterColor write SetUseStaticFooterColor default false;
    procedure StateChanged(Sender: TObject);
    procedure StatesNeedsComponentState(var CurState: TComponentState);
    procedure NextZone(Sender: TObject);
    procedure Loaded; override;
    procedure UpdateControl; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { THMICustomLinkedFlowElevator }

  THMICustomLinkedFlowElevator = class(THMICustomFlowElevator)
  private
    FOnStateChange: TNotifyEvent;
    FPLCTag: TPLCTag;
    procedure SetHMITag(AValue: TPLCTag);
    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
    procedure UpdateControlDelayed(Data: PtrInt);
  protected
    procedure UpdateControl; override;
    property PLCTag:TPLCTag read FPLCTag write SetHMITag;
    property OnStateChange:TNotifyEvent read FOnStateChange write FOnStateChange;
  public
    destructor Destroy; override;
  end;

  THMILinkedFlowElevator = class(THMICustomLinkedFlowElevator)
  published
    property InputPolyline;
    property OutputPolyline;
    property PLCTag;
    property ColorAndFlowStates;
    property CurrentBodyColor:TColor read FBodyColor;
    property BodyColor;
    property FooterColor;
    property HeadColor;
    property BorderColor;

    property Action;
    property OnClick;
    property OnMouseDown;
    property OnMouseLeave;
    property OnMouseMove;
    property PopupMenu;
    property Enabled;

    property BodyWidth;

    property SecurityCode;

    property UseStaticBodyColor;
    property UseStaticFooterColor;
    property UseStaticHeaderColor;

    property BorderWidth;
    property OnStateChange;
  end;

implementation

uses ProtocolTypes, hsstrings, forms;

{ THMIElevatorFlowZones }

constructor THMIElevatorFlowZones.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner, THMIElevatorFlowZone);
end;

function THMIElevatorFlowZones.Add: THMIElevatorFlowZone;
begin
  Result:=THMIElevatorFlowZone(inherited Add);
end;

{ THMIElevatorFlowZone }

procedure THMIElevatorFlowZone.SetPaintBodyWithFlowColor(AValue: Boolean);
begin
  if FPaintBodyWithFlowColor=AValue then Exit;
  FPaintBodyWithFlowColor:=AValue;
  NotifyChange;
end;

procedure THMIElevatorFlowZone.SetEmptyColor(AValue: TColor);
begin
  if FEmptyColor=AValue then Exit;
  FEmptyColor:=AValue;
  NotifyChange;
end;

procedure THMIElevatorFlowZone.SetPaintFooterWithFlowColor(AValue: Boolean);
begin
  if FPaintFooterWithFlowColor=AValue then Exit;
  FPaintFooterWithFlowColor:=AValue;
  NotifyChange;
end;

procedure THMIElevatorFlowZone.SetPaintHeaderWithFlowColor(AValue: Boolean);
begin
  if FPaintHeaderWithFlowColor=AValue then Exit;
  FPaintHeaderWithFlowColor:=AValue;
  NotifyChange;
end;

constructor THMIElevatorFlowZone.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  FEmptyColor:=clSilver;
end;

{ THMICustomLinkedFlowElevator }

procedure THMICustomLinkedFlowElevator.SetHMITag(AValue: TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  //
  //check if the tag is valid (only numeric tags);
  if (AValue<>nil) and (not Supports(AValue, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  //removes the old link.
  if FPLCTag<>nil then begin
    FPLCTag.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if AValue<>nil then begin
    AValue.AddWriteFaultHandler(@WriteFaultCallBack);
    AValue.AddTagChangeHandler(@TagChangeCallBack);
    AValue.AddRemoveTagHandler(@RemoveTagCallBack);
    FPLCTag := AValue;
    UpdateControl;
  end;
  FPLCTag := AValue;
end;

procedure THMICustomLinkedFlowElevator.WriteFaultCallBack(Sender: TObject);
begin
  UpdateControl;
end;

procedure THMICustomLinkedFlowElevator.TagChangeCallBack(Sender: TObject);
begin
  UpdateControl;
end;

procedure THMICustomLinkedFlowElevator.RemoveTagCallBack(Sender: TObject);
begin
  if FPLCTag=Sender then
    FPLCTag:=nil;
end;

procedure THMICustomLinkedFlowElevator.UpdateControlDelayed(Data: PtrInt);
var
  value: Double;
  zone: THMIElevatorFlowZone;
begin
  if Assigned(FPLCTag) then
    value:=(FPLCTag as ITagNumeric).GetValue;

  zone:=THMIElevatorFlowZone(FElevatorStates.GetZoneFromValue(value));
  if FOwnerZone<>zone then begin
    FOwnerZone:=zone;
    ShowZone(FOwnerZone);
    if FCurrentZone<>nil then begin
       FZoneTimer.Interval := FCurrentZone.BlinkTime;
       FZoneTimer.Enabled :=  FCurrentZone.BlinkWith<>(-1);
    end;
  end;

  if Assigned(FOnStateChange) then
    try
      FOnStateChange(Self);
    except
    end;
end;

procedure THMICustomLinkedFlowElevator.UpdateControl;
begin
  if (Application.Flags*[AppDoNotCallAsyncQueue]=[]) and (ComponentState*[csDesigning]=[]) then
    Application.QueueAsyncCall(@UpdateControlDelayed,0);
end;

destructor THMICustomLinkedFlowElevator.Destroy;
begin
  if Assigned(FPLCTag) then
    FPLCTag.RemoveAllHandlersFromObject(Self);
  Application.RemoveAsyncCalls(Self);
  inherited Destroy;
end;

{ TElevadorBasico }

procedure THMICustomElevadorBasico.SetHeadColor(AValue: TColor);
begin
  if FHeadColor=AValue then Exit;
  FHeadColor:=AValue;
  InvalidateDraw;
end;

procedure THMICustomElevadorBasico.DrawControl;
var
  x:array of TPointF;
begin
  inherited DrawControl;


  FControlArea.CanvasBGRA.AntialiasingMode:=amOff;

  //###############################################################################
  //coordenadas de que desenham a cabeca do elevador
  //###############################################################################
  SetLength(x,4);

  x[0].X:=BodyWidth;
  x[0].Y:=(BorderWidth/2);

  x[1].X:=2*BodyWidth-1;
  x[1].Y:=(BorderWidth/2);

  x[2].X:=3*BodyWidth-1;
  x[2].Y:=BodyWidth + (BorderWidth/2);

  x[3].X:=BodyWidth;
  x[3].Y:=BodyWidth + (BorderWidth/2);

  //###############################################################################
  //preenchimento da cabeça do elevador, cor e diametro da linha.
  //###############################################################################

  FControlArea.CanvasBGRA.Brush.Color:= FHeadColor;
  FControlArea.CanvasBGRA.Pen.Color  := FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  := FBorderWidth;

  //###############################################################################
  //desenha a cabeca do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.PolygonF(x);

  //###############################################################################
  //coordenadas que desenham o pé do elevador.
  //###############################################################################
  x[0].X:=0;
  x[0].Y:=Height-BodyWidth-(BorderWidth/2);

  x[1].X:=3*BodyWidth-1;
  x[1].Y:=Height-BodyWidth-(BorderWidth/2);

  x[2].X:=2*BodyWidth-1;
  x[2].Y:=Height-(BorderWidth/2);

  x[3].X:=BodyWidth;
  x[3].Y:=Height-(BorderWidth/2);

  //###############################################################################
  //preenchimento do pé do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:=FFooterColor;

  //###############################################################################
  //desenha o pé do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.PolygonF(x);

  //###############################################################################
  //preenchimento do corpo do elevador.
  //###############################################################################
  FControlArea.CanvasBGRA.Brush.Color:=FBodyColor;

  x[0].X:=FBodyWidth;
  x[0].Y:=(BorderWidth/2);

  x[1].X:=2*BodyWidth-1;
  x[1].Y:=(BorderWidth/2);

  x[2].X:=2*BodyWidth-1;
  x[2].Y:=Height-(BorderWidth/2);

  x[3].X:=BodyWidth;
  x[3].Y:=Height-(BorderWidth/2);

  FControlArea.CanvasBGRA.PolygonF(x);

  //###############################################################################
  //Risquinhos, acabamento.
  //###############################################################################
  FControlArea.CanvasBGRA.PolylineF([PointF(FBodyWidth,
                                            FBodyWidth+(BorderWidth/2)),
                                     PointF(2*FBodyWidth-1,
                                            FBodyWidth+(BorderWidth/2))]);
  FControlArea.CanvasBGRA.PolylineF([PointF(FBodyWidth,   Height-BodyWidth-(BorderWidth/2)),
                                     PointF(2*FBodyWidth, Height-BodyWidth-(BorderWidth/2))]);
end;

constructor THMICustomElevadorBasico.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeadColor:=clSilver;
  FBodyColor:=clSilver;
  FFooterColor:=clSilver;
  FBorderColor:=clBlack;
  FBorderWidth:=1;
  FBodyWidth:=12; //inicializa o desenho com 12px de largura do
end;

procedure THMICustomElevadorBasico.SetBodyWidth(AValue: Byte);
begin
  if FBodyWidth=AValue then Exit;
  FBodyWidth:=AValue;
  Constraints.MinWidth:=FBodyWidth*3;
  Constraints.MaxWidth:=FBodyWidth*3;
  Constraints.MinHeight:=FBodyWidth*2+3;
  if Height<Constraints.MinHeight then
    Height:=Constraints.MinHeight;
  Width:=FBodyWidth*3;
  InvalidateShape;
end;

procedure THMICustomElevadorBasico.SetFooterColor(AValue: TColor);
begin
  if FFooterColor=AValue then Exit;
  FFooterColor:=AValue;
  InvalidateDraw;
end;

{ TCustomFlowElevator }

procedure THMICustomFlowElevator.SetElevatorStates(AValue: THMIElevatorFlowZones
  );
begin
  FElevatorStates.Assign(AValue);
end;

procedure THMICustomFlowElevator.ShowZone(aZone: THMIElevatorFlowZone);
begin
  FCurrentZone:=aZone;
  if aZone<>nil then begin
    if (FUseStaticBodyColor=false)   and (aZone.PaintBodyWithFlowColor=false)   then SetBodyColor(aZone.Color);
    if (FUseStaticHeaderColor=false) and (aZone.PaintHeaderWithFlowColor=false) then SetHeadColor(aZone.Color);
    if (FUseStaticFooterColor=false) and (aZone.PaintFooterWithFlowColor=false) then SetFooterColor(aZone.Color);
    SetBorderColor(aZone.BorderColor);
    UpdateFlow;
    InvalidateDraw;
  end;
end;

procedure THMICustomFlowElevator.UpdateFlow;
begin
  if assigned(FCurrentZone) and Assigned(FInputPolyline) and assigned(FOutputPolyline) then begin
    if FCurrentZone.Flow then begin

      if (FUseStaticBodyColor=false) and FCurrentZone.FPaintBodyWithFlowColor   then begin
        if FInputPolyline.LineColor=FInputPolyline.EmptyColor then
          SetBodyColor(FCurrentZone.EmptyColor)
        else
          SetBodyColor(FInputPolyline.LineColor);
      end;

      if (FUseStaticHeaderColor=false) and FCurrentZone.FPaintHeaderWithFlowColor then begin
        if FInputPolyline.LineColor=FInputPolyline.EmptyColor then
          SetHeadColor(FCurrentZone.EmptyColor)
        else
          SetHeadColor(FInputPolyline.LineColor);
      end;

      if (FUseStaticFooterColor=false) and FCurrentZone.FPaintFooterWithFlowColor then  begin
        if FInputPolyline.LineColor=FInputPolyline.EmptyColor then
          SetFooterColor(FCurrentZone.EmptyColor)
        else
          SetFooterColor(FInputPolyline.LineColor);
      end;

      FOutputPolyline.LineColor:=FInputPolyline.LineColor
    end else
      FOutputPolyline.LineColor:=FOutputPolyline.EmptyColor;
  end;
end;

procedure THMICustomFlowElevator.StateChanged(Sender: TObject);
begin
  UpdateControl;
end;

procedure THMICustomFlowElevator.StatesNeedsComponentState(
  var CurState: TComponentState);
begin
  CurState:=ComponentState;
end;

procedure THMICustomFlowElevator.NextZone(Sender: TObject);
begin

end;

procedure THMICustomFlowElevator.Loaded;
begin
  inherited Loaded;
end;

procedure THMICustomFlowElevator.UpdateControl;
begin

end;

constructor THMICustomFlowElevator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FElevatorStates:=THMIElevatorFlowZones.Create(Self);
  FElevatorStates.OnCollectionItemChange:=@StateChanged;
  FElevatorStates.OnNeedCompState:=@StatesNeedsComponentState;

  FZoneTimer:=TTimer.Create(self);
  FZoneTimer.Enabled:=false;
  FZoneTimer.OnTimer:=@NextZone;
end;

destructor THMICustomFlowElevator.Destroy;
begin
  if Assigned(FInputPolyline) then
    (FInputPolyline as IColorChangeNotification).RemoveNotifyCallback(Self as IColorChangeNotification);

  FreeAndNil(FZoneTimer);
  FreeAndNil(FElevatorStates);
  inherited Destroy;
end;

procedure THMICustomFlowElevator.SetUseStaticBodyColor(AValue: Boolean);
begin
  if FUseStaticBodyColor=AValue then Exit;
  FUseStaticBodyColor:=AValue;
  ShowZone(FCurrentZone);
end;

procedure THMICustomFlowElevator.SetUseStaticFooterColor(AValue: Boolean);
begin
  if FUseStaticFooterColor=AValue then Exit;
  FUseStaticFooterColor:=AValue;
  ShowZone(FCurrentZone);
end;

procedure THMICustomFlowElevator.SetUseStaticHeaderColor(AValue: Boolean);
begin
  if FUseStaticHeaderColor=AValue then Exit;
  FUseStaticHeaderColor:=AValue;
  ShowZone(FCurrentZone);
end;

procedure THMICustomFlowElevator.AddNotifyCallback(
  WhoNotify: IColorChangeNotification);
begin

end;

procedure THMICustomFlowElevator.RemoveNotifyCallback(
  WhoRemove: IColorChangeNotification);
begin

end;

procedure THMICustomFlowElevator.NotifyFree(const WhoWasDestroyed: THMIFlowPolyline
  );
begin
  if WhoWasDestroyed=FInputPolyline then FInputPolyline:=nil;
  if WhoWasDestroyed=FOutputPolyline then FOutputPolyline:=nil;
end;

procedure THMICustomFlowElevator.NotifyChange(const WhoChanged: THMIFlowPolyline);
begin
  UpdateFlow;
end;

procedure THMICustomFlowElevator.SetInputPolyline(AValue: THMIFlowPolyline);
begin
  if FInputPolyline=AValue then
    Exit;

  if Assigned(aValue) and (not Supports(AValue, IColorChangeNotification)) then
    exit;

  if Assigned(FInputPolyline) then
    (FInputPolyline as IColorChangeNotification).RemoveNotifyCallback(Self as IColorChangeNotification);

  if Assigned(aValue) then
    (AValue as IColorChangeNotification).AddNotifyCallback(self as IColorChangeNotification);

  FInputPolyline:=AValue;
  UpdateFlow;
end;

procedure THMICustomFlowElevator.SetOutputPolyline(AValue: THMIFlowPolyline);
begin
  if FOutputPolyline=AValue then exit;
  FOutputPolyline:=AValue;
  UpdateFlow;
end;

end.

