unit hmibooleanpropertyconnector;

{$mode delphi}

interface

uses
  Classes, HMIZones, hmiobjectcolletion;

type
  //forward class declaration.
  TBooleanZone = class;

  {$IFDEF PORTUGUES}
  {:
  Coleção de zonas booleanas.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TBooleanZone)
  }
  {$ELSE}
  {:
  Collection of boolean zones.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TBooleanZone)
  }
  {$ENDIF}
  TBooleanZones = class(TZones)
  public
    //: @exclude
    constructor Create(Owner:TPersistent);

    {$IFDEF PORTUGUES}
    //: Adiciona uma nova zona de cor a coleção.
    {$ELSE}
    //: Adds a new color zone into the collection.
    {$ENDIF}
    function Add:TBooleanZone;
  end;

  {$IFDEF PORTUGUES}
  {:
  Implementa uma zona booleana.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TBooleanZones)
  }
  {$ELSE}
  {:
  Boolean class zone.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TBooleanZones)
  }
  {$ENDIF}
  TBooleanZone = class(TZone)
  private
    FResult: Boolean;
    procedure SetZoneResult(AValue: Boolean);
  published
    property ZoneResult:Boolean read FResult write SetZoneResult;
  end;

  TBooleanPropertyConnector = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

implementation

{ TBooleanZones}

constructor TBooleanZones.Create(Owner: TPersistent);
begin
  inherited Create(Owner, TBooleanZone);
end;

function TBooleanZones.Add: TBooleanZone;
begin
  Result:=TBooleanZone(inherited Add);
end;

{ TBooleanZone }

procedure TBooleanZone.SetZoneResult(AValue: Boolean);
begin
  if FResult=AValue then Exit;
  FResult:=AValue;
  NotifyChange;
end;

end.
