unit hmibooleanpropertyconnector;

{$mode delphi}

interface

uses
  Classes, sysutils, HMIZones, hmiobjectcolletion;

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

  TObjectWithBooleanPropetiesColletionItem = class;

  TObjectWithBooleanPropetiesColletion = class(TObjectColletion)
  public
    constructor Create(AOwner:TComponent);
    function Add: TObjectWithBooleanPropetiesColletionItem;
  end;

  { TObjectWithBooleanPropetiesColletionItem }

  TObjectWithBooleanPropetiesColletionItem = class(TObjectColletionItem)
  private
    fInvertResult,
    fModified,
    fLastResultApplied: Boolean;
    procedure SetInvertedResult(AValue: Boolean);
  public
    constructor Create(ACollection: TCollection); override;
    procedure ApplyResult(Result:Boolean); virtual;
  published
    property InvertResult:Boolean read FInvertResult write SetInvertedResult;
  end;

  { THMIBooleanPropertyConnector }

  THMIBooleanPropertyConnector = class(TComponent)
  private
    FConditionZones:TBooleanZones;
    FObjects:TObjectWithBooleanPropetiesColletion;
    function GetConditionZones: TBooleanZones;
    function GetObjects: TObjectWithBooleanPropetiesColletion;
    procedure SetConditionZones(AValue: TBooleanZones);
    procedure SetObjects(AValue: TObjectWithBooleanPropetiesColletion);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Conditions:TBooleanZones read GetConditionZones write SetConditionZones;
    property AffectedObjects:TObjectWithBooleanPropetiesColletion read GetObjects write SetObjects;
  end;

implementation

uses typinfo, rttiutils;

{ THMIBooleanPropertyConnector }

function THMIBooleanPropertyConnector.GetConditionZones: TBooleanZones;
begin
  Result:=FConditionZones;
end;

function THMIBooleanPropertyConnector.GetObjects: TObjectWithBooleanPropetiesColletion;
begin
  Result:=FObjects;
end;

procedure THMIBooleanPropertyConnector.SetConditionZones(AValue: TBooleanZones);
begin
  FConditionZones.Assign(AValue);
end;

procedure THMIBooleanPropertyConnector.SetObjects(
  AValue: TObjectWithBooleanPropetiesColletion);
begin
  FObjects.Assign(AValue);
end;

constructor THMIBooleanPropertyConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConditionZones:=TBooleanZones.Create(Self);
  FObjects:=TObjectWithBooleanPropetiesColletion.Create(Self);
end;

destructor THMIBooleanPropertyConnector.Destroy;
begin
  FreeAndNil(FConditionZones);
  FreeAndNil(FObjects);
  inherited Destroy;
end;

{ TObjectWithBooleanPropetiesColletionItem }

procedure TObjectWithBooleanPropetiesColletionItem.SetInvertedResult(
  AValue: Boolean);
begin
  if FInvertResult=AValue then Exit;
  FInvertResult:=AValue;

  if fModified then ApplyResult(fLastResultApplied);
end;

constructor TObjectWithBooleanPropetiesColletionItem.Create(
  ACollection: TCollection);
begin
  inherited Create(ACollection);
  fRequiredTypeName:=PTypeInfo(TypeInfo(Boolean)).Name ;
end;

procedure TObjectWithBooleanPropetiesColletionItem.ApplyResult(Result: Boolean);
begin
  if (not AcceptObject(TargetObject)) or
     (not AcceptObjectProperty(TargetObjectProperty)) then exit;
  fModified:=true;
  fLastResultApplied:=Result;
  SetPropValue(TargetObject,TargetObjectProperty,(Result xor fInvertResult));
end;

{ TObjectWithBooleanPropetiesColletion }

constructor TObjectWithBooleanPropetiesColletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TObjectWithBooleanPropetiesColletionItem);
end;

function TObjectWithBooleanPropetiesColletion.Add: TObjectWithBooleanPropetiesColletionItem;
begin
  Result:=TObjectWithBooleanPropetiesColletionItem(inherited Add);
end;

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
