unit hmi_draw_flow_valve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hmi_flow_zones, HMI_Draw_Valves;

type
  THMICustomFlowValve = class(THMICustomBasicValve)
  private
    FValveStates: THMIFlowZones;
    procedure SetValveStates(AValue: THMIFlowZones);
  protected
    property ValveStates:THMIFlowZones read FValveStates write SetValveStates;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ THMICustomFlowValve }

procedure THMICustomFlowValve.SetValveStates(AValue: THMIFlowZones);
begin
  FValveStates.Assign(AValue);
end;

constructor THMICustomFlowValve.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValveStates:=THMIFlowZones.Create(Self);
end;

end.

