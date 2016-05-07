unit hmi_flow_zones;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hmicolorpropertyconnector, HMIZones;

type
  THMIFlowZone = class(TColorZone)
  private
    FBorderColor: TColor;
    FFlow: Boolean;
    procedure SetBorderColor(AValue: TColor);
    procedure SetFlow(AValue: Boolean);
  published
    property Flow:Boolean read FFlow write SetFlow;
    property BorderColor:TColor read FBorderColor write SetBorderColor;
  end;

  THMIFlowZones = class(TZones)
    //: @exclude
    constructor Create(aOwner:TPersistent);

    {$IFDEF PORTUGUES}
    //: Adiciona uma nova zona de cor.
    {$ELSE}
    //: Adds a new color and flow zone into the collection.
    {$ENDIF}
    function Add:THMIFlowZone;
  end;

implementation

{ THMIFlowZones }

constructor THMIFlowZones.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner, THMIFlowZone);
end;

function THMIFlowZones.Add: THMIFlowZone;
begin
  Result:=THMIFlowZone(inherited Add);
end;

{ THMIFlowZone }

procedure THMIFlowZone.SetFlow(AValue: Boolean);
begin
  if FFlow=AValue then Exit;
  FFlow:=AValue;
  NotifyChange;
end;

procedure THMIFlowZone.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  NotifyChange;
end;

end.

