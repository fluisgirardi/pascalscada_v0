unit hmi_draw_flow_valve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hmi_flow_zones, HMI_Draw_Valves, hmi_polyline, HMIZones,
  PLCTag, Tag, ExtCtrls;

type
  THMICustomFlowValve = class(THMICustomBasicValve, IColorChangeNotification)
  protected
    procedure AddNotifyCallback(WhoNotify:IColorChangeNotification);
    procedure RemoveNotifyCallback(WhoRemove:IColorChangeNotification);
    procedure NotifyFree(const WhoWasDestroyed:THMIFlowPolyline);
    procedure NotifyChange(const WhoChanged:THMIFlowPolyline);
  protected
    FInputPolyline: THMIFlowPolyline;
    FOutputPolyline: THMIFlowPolyline;
    FValveStates: THMIFlowZones;
    FCurrentZone,
    FOwnerZone: THMIFlowZone;
    FZoneTimer:TTimer;
    procedure SetInputPolyline(AValue: THMIFlowPolyline);
    procedure SetOutputPolyline(AValue: THMIFlowPolyline);
    procedure SetValveStates(AValue: THMIFlowZones);
    procedure ShowZone(aZone:THMIFlowZone);
    procedure UpdateValve; virtual;
    procedure UpdateFlow; virtual;
    property  InputPolyline:THMIFlowPolyline read FInputPolyline write SetInputPolyline;
    property  OutputPolyline:THMIFlowPolyline read FOutputPolyline write SetOutputPolyline;
    property  ColorAndFlowStates:THMIFlowZones read FValveStates write SetValveStates;
    procedure ValveStateChanged(Sender: TObject);
    procedure ValveStateNeedsComponentState(var CurState: TComponentState);
    procedure NextZone(Sender: TObject);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  THMICustomLinkedFlowValve = class(THMICustomFlowValve)
  private
    FPLCTag: TPLCTag;
    procedure SetHMITag(AValue: TPLCTag);
    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
    procedure UpdateValveDelayed(Data: PtrInt);
  protected
    procedure UpdateValve; override;
    property PLCTag:TPLCTag read FPLCTag write SetHMITag;
  public
    destructor Destroy; override;
  end;

  THMILinkedFlowValve = class(THMICustomLinkedFlowValve)
  published
    property InputPolyline;
    property OutputPolyline;
    property PLCTag;
    property ColorAndFlowStates;

    property Mirrored;
    property ValveBodyPercent;
    property ValveType;

    property SecurityCode;

    property OnClick;
    property Action;
  end;

implementation

uses Forms, ProtocolTypes, hsstrings, math;

{ THMICustomLinkedFlowValve }

procedure THMICustomLinkedFlowValve.SetHMITag(AValue: TPLCTag);
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
    UpdateValve;
  end;
  FPLCTag := AValue;
end;

procedure THMICustomLinkedFlowValve.WriteFaultCallBack(Sender: TObject);
begin
  UpdateValve;
end;

procedure THMICustomLinkedFlowValve.TagChangeCallBack(Sender: TObject);
begin
  UpdateValve;
end;

procedure THMICustomLinkedFlowValve.RemoveTagCallBack(Sender: TObject);
begin
  if FPLCTag=Sender then
    FPLCTag:=nil;
end;

procedure THMICustomLinkedFlowValve.UpdateValve;
begin
  if (Application.Flags*[AppDoNotCallAsyncQueue]=[]) then
    Application.QueueAsyncCall(@UpdateValveDelayed,0);
end;

procedure THMICustomLinkedFlowValve.UpdateValveDelayed(Data: PtrInt);
var
  zone: THMIFlowZone;
  value:Double = Infinity;
begin
  if Assigned(FPLCTag) then
    value:=(FPLCTag as ITagNumeric).GetValue;

  zone:=THMIFlowZone(FValveStates.GetZoneFromValue(value));
  if FOwnerZone<>zone then begin
    FOwnerZone:=zone;
    ShowZone(FOwnerZone);
    if FCurrentZone<>nil then begin
       FZoneTimer.Interval := FCurrentZone.BlinkTime;
       FZoneTimer.Enabled :=  FCurrentZone.BlinkWith<>(-1);
    end;
  end;
end;

destructor THMICustomLinkedFlowValve.Destroy;
begin
  if Assigned(FPLCTag) then
    FPLCTag.RemoveAllHandlersFromObject(Self);
  Application.RemoveAsyncCalls(Self);
  inherited Destroy;
end;

{ THMICustomFlowValve }

procedure THMICustomFlowValve.SetValveStates(AValue: THMIFlowZones);
begin
  FValveStates.Assign(AValue);
end;

procedure THMICustomFlowValve.ShowZone(aZone: THMIFlowZone);
begin
  FCurrentZone:=aZone;
  if aZone<>nil then begin
    SetBodyColor(aZone.Color);
    SetBorderColor(aZone.BorderColor);
    UpdateFlow;
  end;
end;

procedure THMICustomFlowValve.UpdateValve;
begin
  //does nothing
end;

procedure THMICustomFlowValve.UpdateFlow;
begin
  if assigned(FCurrentZone) and Assigned(FInputPolyline) and assigned(FOutputPolyline) then begin
    if FCurrentZone.Flow then
      FOutputPolyline.LineColor:=FInputPolyline.LineColor
    else
      FOutputPolyline.LineColor:=FOutputPolyline.EmptyColor;
  end;
end;

procedure THMICustomFlowValve.AddNotifyCallback(
  WhoNotify: IColorChangeNotification);
begin

end;

procedure THMICustomFlowValve.RemoveNotifyCallback(
  WhoRemove: IColorChangeNotification);
begin

end;

procedure THMICustomFlowValve.NotifyFree(const WhoWasDestroyed: THMIFlowPolyline
  );
begin
  if WhoWasDestroyed=FInputPolyline then FInputPolyline:=nil;
  if WhoWasDestroyed=FOutputPolyline then FOutputPolyline:=nil;
end;

procedure THMICustomFlowValve.NotifyChange(const WhoChanged: THMIFlowPolyline);
begin
  UpdateFlow;
end;

procedure THMICustomFlowValve.SetInputPolyline(AValue: THMIFlowPolyline);
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

procedure THMICustomFlowValve.SetOutputPolyline(AValue: THMIFlowPolyline);
begin
  if FOutputPolyline=AValue then exit;
  FOutputPolyline:=AValue;
  UpdateFlow;
end;

constructor THMICustomFlowValve.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValveStates:=THMIFlowZones.Create(Self);
  FValveStates.OnCollectionItemChange:=@ValveStateChanged;
  FValveStates.OnNeedCompState:=@ValveStateNeedsComponentState;

  FZoneTimer:=TTimer.Create(self);
  FZoneTimer.Enabled:=false;
  FZoneTimer.OnTimer:=@NextZone;
end;

destructor THMICustomFlowValve.Destroy;
begin
  if Assigned(FInputPolyline) then
    (FInputPolyline as IColorChangeNotification).RemoveNotifyCallback(Self as IColorChangeNotification);

  FreeAndNil(FZoneTimer);
  FreeAndNil(FValveStates);
  inherited Destroy;
end;

procedure THMICustomFlowValve.NextZone(Sender: TObject);
begin
  if FCurrentZone.BlinkWith<0 then
    FZoneTimer.Enabled:=false
  else begin
    if FZoneTimer.Interval<>THMIFlowZone(FValveStates.Items[FCurrentZone.BlinkWith]).BlinkTime then
      FZoneTimer.Interval := THMIFlowZone(FValveStates.Items[FCurrentZone.BlinkWith]).BlinkTime;
    ShowZone(THMIFlowZone(FValveStates.Items[FCurrentZone.BlinkWith]));
    if not FZoneTimer.Enabled then FZoneTimer.Enabled:=true;
  end;
end;

procedure THMICustomFlowValve.Loaded;
begin
  inherited Loaded;
  FValveStates.Loaded;
end;

procedure THMICustomFlowValve.ValveStateChanged(Sender: TObject);
begin
  UpdateValve;
end;

procedure THMICustomFlowValve.ValveStateNeedsComponentState(
  var CurState: TComponentState);
begin
  CurState:=ComponentState;
end;

end.

