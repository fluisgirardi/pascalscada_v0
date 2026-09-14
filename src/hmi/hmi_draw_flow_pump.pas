unit hmi_draw_flow_pump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hmi_flow_zones, HMIBasicEletricMotor, hmi_polyline, HMIZones,
  PLCTag, Tag, ExtCtrls,Graphics, hmi_commfaultbadge;

type

  { THMICustomFlowPump }

  THMICustomFlowPump = class(THMICustomBasicEletricMotor, IColorChangeNotification)
  protected
    procedure AddNotifyCallback(WhoNotify:IColorChangeNotification);
    procedure RemoveNotifyCallback(WhoRemove:IColorChangeNotification);
    procedure NotifyFree(const WhoWasDestroyed:THMIFlowPolyline);
    procedure NotifyChange(const WhoChanged:THMIFlowPolyline);
  protected
    FInputPolyline: THMIFlowPolyline;
    FOutputPolyline: THMIFlowPolyline;
    FPumpStates: THMIFlowZones;
    FCurrentZone,
    FOwnerZone: THMIFlowZone;
    FZoneTimer:TTimer;
    procedure SetInputPolyline(AValue: THMIFlowPolyline);
    procedure SetOutputPolyline(AValue: THMIFlowPolyline);
    procedure SetPumpStates(AValue: THMIFlowZones);
    procedure ShowZone(aZone:THMIFlowZone);
    procedure UpdateFlow; virtual;
    property  InputPolyline:THMIFlowPolyline read FInputPolyline write SetInputPolyline;
    property  OutputPolyline:THMIFlowPolyline read FOutputPolyline write SetOutputPolyline;
    property  ColorAndFlowStates:THMIFlowZones read FPumpStates write SetPumpStates;
    procedure PumpStateChanged(Sender: TObject);
    procedure PumpStateNeedsComponentState(var CurState: TComponentState);
    procedure NextZone(Sender: TObject);
    procedure Loaded; override;
    procedure UpdateValve; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  end;

  { THMICustomLinkedFlowPump }

  THMICustomLinkedFlowPump = class(THMICustomFlowPump)
  private
    FOnStateChange: TNotifyEvent;
    FPLCTag: TPLCTag;
    procedure SetHMITag(AValue: TPLCTag);
    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
    procedure UpdateValveDelayed(Data: PtrInt);
  protected
    procedure UpdateValve; override;
    property PLCTag:TPLCTag read FPLCTag write SetHMITag;
    property OnStateChange:TNotifyEvent read FOnStateChange write FOnStateChange;
    procedure Loaded; override;
  public
    destructor Destroy; override;
  end;

  THMILinkedFlowPump = class(THMICustomLinkedFlowPump)
  published
    property InputPolyline;
    property OutputPolyline;
    property PLCTag;
    property ColorAndFlowStates;
    property CurrentBodyColor:TColor read FBodyColor;

    property Action;
    property DrawPump;
    property OnClick;
    property OnMouseDown;
    property OnMouseLeave;
    property OnMouseMove;
    property PopupMenu;
    property Enabled;

    property SecurityCode;

    property BorderWidth;
    property Mirrored;
    property OnStateChange;
  end;

implementation

uses Forms, ProtocolTypes, hsstrings, math;

{ THMICustomLinkedFlowValve }

procedure THMICustomLinkedFlowPump.SetHMITag(AValue: TPLCTag);
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
  end;
  FPLCTag := AValue;
  //sem tag nao ha' valor: o Infinity que o recalculo usa nao casa com faixa
  //nenhuma, entao quem responde e' a zona padrao - onde mora o desenho de
  //"sem comunicacao". Ate' agora ninguem chamava o recalculo neste caminho
  //e o desenho do ultimo valor ficava na tela.
  //with no tag there is no value: the Infinity the recalculation uses matches
  //no range, so the default zone answers - which is where the "no
  //communication" drawing lives. Until now nobody called the recalculation on
  //this path and the last value's drawing stayed on screen.
  UpdateValve;
  if Assigned(FCommFaultLink) then
    FCommFaultLink.SetTag(AValue);
end;

procedure THMICustomLinkedFlowPump.WriteFaultCallBack(Sender: TObject);
begin
  UpdateValve;
end;

procedure THMICustomLinkedFlowPump.TagChangeCallBack(Sender: TObject);
begin
  UpdateValve;
end;

procedure THMICustomLinkedFlowPump.RemoveTagCallBack(Sender: TObject);
begin
  if FPLCTag=Sender then begin
    FPLCTag:=nil;
    UpdateValve;
  end;
end;

procedure THMICustomLinkedFlowPump.UpdateValve;
begin
  if (Application.Flags*[AppDoNotCallAsyncQueue]=[]) then
    Application.QueueAsyncCall(@UpdateValveDelayed,0);
end;

procedure THMICustomLinkedFlowPump.Loaded;
begin
  inherited Loaded;
  TagChangeCallBack(Self);
end;

procedure THMICustomLinkedFlowPump.UpdateValveDelayed(Data: PtrInt);
var
  zone: THMIFlowZone;
  value:Double = Infinity;
begin
  if [csReading,csLoading,csDestroying]*ComponentState<>[] then exit;
  if Assigned(FPLCTag) then
    value:=(FPLCTag as ITagNumeric).GetValue;

  zone:=THMIFlowZone(FPumpStates.GetZoneFromValue(value));
  if FOwnerZone<>zone then begin
    FOwnerZone:=zone;
    ShowZone(FOwnerZone);
    if FCurrentZone<>nil then begin
       FZoneTimer.Interval := FCurrentZone.BlinkTime;
       FZoneTimer.Enabled :=  FCurrentZone.BlinkWith<>(-1);
    end else
       //sem estado nenhum o temporizador nao pode ficar ligado: o NextZone
       //entra em cima de FCurrentZone nulo.
       //with no state at all the timer cannot stay on: NextZone would run on
       //a nil FCurrentZone.
       FZoneTimer.Enabled := false;
  end;

  if Assigned(FOnStateChange) then
    try
      FOnStateChange(Self);
    except
    end;
end;

destructor THMICustomLinkedFlowPump.Destroy;
begin
  if Assigned(FPLCTag) then
    FPLCTag.RemoveAllHandlersFromObject(Self);
  Application.RemoveAsyncCalls(Self);
  inherited Destroy;
end;

{ THMICustomFlowValve }

procedure THMICustomFlowPump.SetPumpStates(AValue: THMIFlowZones);
begin
  FPumpStates.Assign(AValue);
end;

procedure THMICustomFlowPump.ShowZone(aZone: THMIFlowZone);
begin
  FCurrentZone:=aZone;
  if aZone<>nil then begin
    SetBodyColor(aZone.Color);
    SetBorderColor(aZone.BorderColor);
    UpdateFlow;
  end;
end;

procedure THMICustomFlowPump.UpdateFlow;
begin
  if assigned(FCurrentZone) and assigned(FOutputPolyline) then begin
    //sem linha de entrada nao ha' o que passar adiante: a saida fica vazia em
    //vez de guardar a ultima cor que recebeu.
    //with no input line there is nothing to pass on: the output goes empty
    //instead of keeping the last colour it was given.
    if FCurrentZone.Flow and Assigned(FInputPolyline) then
      FOutputPolyline.LineColor:=FInputPolyline.LineColor
    else
      FOutputPolyline.LineColor:=FOutputPolyline.EmptyColor;
  end;
end;

procedure THMICustomFlowPump.AddNotifyCallback(
  WhoNotify: IColorChangeNotification);
begin

end;

procedure THMICustomFlowPump.RemoveNotifyCallback(
  WhoRemove: IColorChangeNotification);
begin

end;

procedure THMICustomFlowPump.NotifyFree(const WhoWasDestroyed: THMIFlowPolyline
  );
begin
  if WhoWasDestroyed=FInputPolyline then FInputPolyline:=nil;
  if WhoWasDestroyed=FOutputPolyline then FOutputPolyline:=nil;
  UpdateFlow;
end;

procedure THMICustomFlowPump.NotifyChange(const WhoChanged: THMIFlowPolyline);
begin
  UpdateFlow;
end;

procedure THMICustomFlowPump.SetInputPolyline(AValue: THMIFlowPolyline);
begin
  if FInputPolyline=AValue then
    Exit;

  if Assigned(aValue) and (not Supports(AValue, IColorChangeNotification)) then
    exit;

  if Assigned(FInputPolyline) then begin
    (FInputPolyline as IColorChangeNotification).RemoveNotifyCallback(Self as IColorChangeNotification);
    FInputPolyline.RemoveFreeNotification(Self);
  end;

  if Assigned(aValue) then begin
    (AValue as IColorChangeNotification).AddNotifyCallback(self as IColorChangeNotification);
    AValue.FreeNotification(Self);
  end;

  FInputPolyline:=AValue;
  UpdateFlow;
end;

procedure THMICustomFlowPump.SetOutputPolyline(AValue: THMIFlowPolyline);
begin
  if FOutputPolyline=AValue then exit;
  if Assigned(FOutputPolyline) then
    FOutputPolyline.RemoveFreeNotification(Self);
  if Assigned(AValue) then
    AValue.FreeNotification(Self);
  FOutputPolyline:=AValue;

  UpdateFlow;
end;

constructor THMICustomFlowPump.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPumpStates:=THMIFlowZones.Create(Self);
  FPumpStates.OnCollectionItemChange:=@PumpStateChanged;
  FPumpStates.OnNeedCompState:=@PumpStateNeedsComponentState;

  FZoneTimer:=TTimer.Create(self);
  FZoneTimer.Enabled:=false;
  FZoneTimer.OnTimer:=@NextZone;
end;

destructor THMICustomFlowPump.Destroy;
begin
  if Assigned(FInputPolyline) then
    (FInputPolyline as IColorChangeNotification).RemoveNotifyCallback(Self as IColorChangeNotification);

  FreeAndNil(FZoneTimer);
  FreeAndNil(FPumpStates);
  inherited Destroy;
end;

procedure THMICustomFlowPump.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent<>Self) then begin
    if AComponent=FInputPolyline then
      FInputPolyline:=nil;
    if AComponent=FOutputPolyline then
      FOutputPolyline:=nil;

    if [csDestroying]*ComponentState=[] then
      UpdateFlow;
  end;
end;

procedure THMICustomFlowPump.NextZone(Sender: TObject);
begin
  if FCurrentZone=nil then begin
    FZoneTimer.Enabled:=false;
    exit;
  end;

  if FCurrentZone.BlinkWith<0 then
    FZoneTimer.Enabled:=false
  else begin
    if FZoneTimer.Interval<>THMIFlowZone(FPumpStates.Items[FCurrentZone.BlinkWith]).BlinkTime then
      FZoneTimer.Interval := THMIFlowZone(FPumpStates.Items[FCurrentZone.BlinkWith]).BlinkTime;
    ShowZone(THMIFlowZone(FPumpStates.Items[FCurrentZone.BlinkWith]));
    if not FZoneTimer.Enabled then FZoneTimer.Enabled:=true;
  end;
end;

procedure THMICustomFlowPump.Loaded;
begin
  inherited Loaded;
  FPumpStates.Loaded;
end;

procedure THMICustomFlowPump.UpdateValve;
begin

end;

procedure THMICustomFlowPump.PumpStateChanged(Sender: TObject);
begin
  UpdateValve;
end;

procedure THMICustomFlowPump.PumpStateNeedsComponentState(
  var CurState: TComponentState);
begin
  CurState:=ComponentState;
end;

end.

