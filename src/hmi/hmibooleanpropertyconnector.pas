unit hmibooleanpropertyconnector;

{$mode delphi}

interface

uses
  Classes, sysutils, HMIZones, hmiobjectcolletion, ProtocolTypes, HMITypes,
  Tag, PLCTag;

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
    //: Adiciona uma nova zona boolean.
    {$ELSE}
    //: Adds a new Boolean zone into the collection.
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

  { TBooleanZone }

  TBooleanZone = class(TZone)
  private
    FResult: Boolean;
    procedure SetZoneResult(AValue: Boolean);
  protected
    function GetDisplayName: string; override;
  published
    property ZoneResult:Boolean read FResult write SetZoneResult;
  end;

  //////////////////////////////////////////////////////////////////////////////

  //: @exclude
  TObjectWithBooleanPropetiesColletionItem = class;

  {$IFDEF PORTUGUES}
  {:
  Implementa uma coleção de objetos com propriedades do tipo Boolean.
  @seealso(TObjectColletion)
  }
  {$ELSE}
  {:
  Implements a collection with objects that contains boolean properties.
  @seealso(TObjectColletion)
  }
  {$ENDIF}
  TObjectWithBooleanPropetiesColletion = class(TObjectColletion)
  public
    constructor Create(AOwner:TComponent);
    function Add: TObjectWithBooleanPropetiesColletionItem;
  end;

  {$IFDEF PORTUGUES}
  {:
  Implementa um item da coleção de objetos com propriedades booleanas.
  @seealso(TObjectColletionItem)
  }
  {$ELSE}
  {:
  Implements a item of a collection with objects that contains boolean properties.
  @seealso(TObjectColletionItem)
  }
  {$ENDIF}

  { TObjectWithBooleanPropetiesColletionItem }

  TObjectWithBooleanPropetiesColletionItem = class(TObjectColletionItem)
  private
    fInvertResult,
    fModified,
    fLastResultApplied: Boolean;
    procedure SetInvertedResult(AValue: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure ApplyResult(Result:Boolean); virtual;
  published
    property InvertResult:Boolean read FInvertResult write SetInvertedResult;
  end;

  //////////////////////////////////////////////////////////////////////////////

  { THMIBooleanPropertyConnector }

  THMIBooleanPropertyConnector = class(TComponent, IHMITagInterface)
  private
    FTag:TPLCTag;
    FFirstReadOk:Boolean;
    FConditionZones:TBooleanZones;
    FObjects:TObjectWithBooleanPropetiesColletion;
    procedure ConditionItemChanged(Sender: TObject);
    procedure CollectionNeedsComponentState(var CurState: TComponentState);
    procedure ObjectItemChanged(Sender: TObject);
    function GetConditionZones: TBooleanZones;
    function GetObjects: TObjectWithBooleanPropetiesColletion;
    procedure SetConditionZones(AValue: TBooleanZones);
    procedure SetHMITag(AValue: TPLCTag);
    procedure SetObjects(AValue: TObjectWithBooleanPropetiesColletion);

    //: @seealso(IHMITagInterface.NotifyReadOk)
    procedure NotifyReadOk;
    //: @seealso(IHMITagInterface.NotifyReadFault)
    procedure NotifyReadFault;
    //: @seealso(IHMITagInterface.NotifyWriteOk)
    procedure NotifyWriteOk;
    //: @seealso(IHMITagInterface.NotifyWriteFault)
    procedure NotifyWriteFault;
    //: @seealso(IHMITagInterface.NotifyTagChange)
    procedure NotifyTagChange(Sender:TObject);
    //: @seealso(IHMITagInterface.RemoveTag)
    procedure RemoveTag(Sender:TObject);

    procedure RecalculateObjectsProperties;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Conditions:TBooleanZones read GetConditionZones write SetConditionZones;
    property AffectedObjects:TObjectWithBooleanPropetiesColletion read GetObjects write SetObjects;
    property PLCTag:TPLCTag read FTag write SetHMITag;
  end;

implementation

uses typinfo, rttiutils, pSCADA_Strings;

{ THMIBooleanPropertyConnector }

procedure THMIBooleanPropertyConnector.ConditionItemChanged(Sender: TObject);
begin
  RecalculateObjectsProperties
end;

procedure THMIBooleanPropertyConnector.CollectionNeedsComponentState(
  var CurState: TComponentState);
begin
  CurState:=ComponentState;
end;

procedure THMIBooleanPropertyConnector.ObjectItemChanged(Sender: TObject);
begin
  RecalculateObjectsProperties
end;

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

procedure THMIBooleanPropertyConnector.SetHMITag(AValue: TPLCTag);
begin
  if FTag=AValue then Exit;

  //se o tag esta entre um dos aceitos.
  //check if the tag is valid (only numeric tags)
  if (AValue<>nil) and (not Supports(AValue, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  if FTag<>nil then begin
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if AValue<>nil then begin
    AValue.AddCallBacks(Self as IHMITagInterface);
    FTag := AValue;
    RecalculateObjectsProperties;
  end;
  FTag:=AValue;
end;

procedure THMIBooleanPropertyConnector.SetObjects(
  AValue: TObjectWithBooleanPropetiesColletion);
begin
  FObjects.Assign(AValue);
end;

procedure THMIBooleanPropertyConnector.NotifyReadOk;
begin
  if FFirstReadOk then
    RecalculateObjectsProperties;
  FFirstReadOk:=false;
end;

procedure THMIBooleanPropertyConnector.NotifyReadFault;
begin
  RecalculateObjectsProperties;
end;

procedure THMIBooleanPropertyConnector.NotifyWriteOk;
begin
  RecalculateObjectsProperties;
end;

procedure THMIBooleanPropertyConnector.NotifyWriteFault;
begin
  RecalculateObjectsProperties;
end;

procedure THMIBooleanPropertyConnector.NotifyTagChange(Sender: TObject);
begin
  RecalculateObjectsProperties;
end;

procedure THMIBooleanPropertyConnector.RemoveTag(Sender: TObject);
begin
  if Sender=FTag then
     FTag:=nil;
end;

procedure THMIBooleanPropertyConnector.RecalculateObjectsProperties;
var
  x: TBooleanZone;
  o: Integer;
begin
  if csDesigning in ComponentState then exit;
  if Assigned(FTag) and Supports(FTag,ITagNumeric) then begin
    x:=TBooleanZone(FConditionZones.GetZoneFromValue((FTag as ITagNumeric).Value));
    if x=nil then exit;
    for o:=0 to AffectedObjects.Count-1 do
      TObjectWithBooleanPropetiesColletionItem(AffectedObjects.Items[o]).ApplyResult(x.ZoneResult);
  end;
end;

procedure THMIBooleanPropertyConnector.Loaded;
begin
  inherited Loaded;
  FConditionZones.Loaded;
  FObjects.Loaded;
  RecalculateObjectsProperties;
end;

procedure THMIBooleanPropertyConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    for i:=0 to FObjects.Count-1 do begin
      if TObjectWithBooleanPropetiesColletionItem(FObjects.Items[i]).TargetObject=AComponent then begin
        TObjectWithBooleanPropetiesColletionItem(FObjects.Items[i]).TargetObject:=nil;
      end;
    end;
  end;
end;

constructor THMIBooleanPropertyConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConditionZones:=TBooleanZones.Create(Self);
  FConditionZones.OnCollectionItemChange:=ConditionItemChanged;
  FConditionZones.OnNeedCompState:=CollectionNeedsComponentState;
  FObjects:=TObjectWithBooleanPropetiesColletion.Create(Self);
  FObjects.OnCollectionItemChange:=ObjectItemChanged;
  FObjects.OnNeedCompState:=CollectionNeedsComponentState;
  FFirstReadOk:=true;
end;

destructor THMIBooleanPropertyConnector.Destroy;
var
  o: Integer;
begin
  if FObjects.Owner is TComponent then
    for o:=0 to FObjects.Count-1 do begin
      if assigned(TObjectWithBooleanPropetiesColletionItem(FObjects.Items[o]).TargetObject) then
        TObjectWithBooleanPropetiesColletionItem(FObjects.Items[o]).TargetObject.RemoveFreeNotification(TComponent(FObjects.Owner));
    end;
  FreeAndNil(FConditionZones);
  FreeAndNil(FObjects);
  SetHMITag(nil);
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

function TObjectWithBooleanPropetiesColletionItem.GetDisplayName: string;
begin
  if Assigned(TargetObject) and (TargetObjectProperty<>'') then
    Result:=TargetObject.Name+'.'+TargetObjectProperty
  else
    Result:='(unused)';
end;

constructor TObjectWithBooleanPropetiesColletionItem.Create(
  ACollection: TCollection);
begin
  inherited Create(ACollection);
  fRequiredTypeName:=PTypeInfo(TypeInfo(Boolean)).Name;
  fRequiredTypeKind:=PTypeInfo(TypeInfo(Boolean)).Kind;
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

function TBooleanZone.GetDisplayName: string;
begin
  Result:=inherited GetDisplayName+', Result='+BoolToStr(ZoneResult,'True','False');
end;

end.
