unit hmicolorpropertyconnector;

interface

uses
  Classes, sysutils, HMIZones, hmiobjectcolletion, ProtocolTypes, HMITypes,
  Tag, PLCTag, Graphics;

type
  //forward class declaration.
  TColorZone = class;

  {$IFDEF PORTUGUES}
  {:
  Coleção de zonas de cores.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TColorZone)
  }
  {$ELSE}
  {:
  Collection of color zones.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TColorZone)
  }
  {$ENDIF}
  TColorZones = class(TZones)
  public
    //: @exclude
    constructor Create(aOwner:TPersistent);

    {$IFDEF PORTUGUES}
    //: Adiciona uma nova zona de cor.
    {$ELSE}
    //: Adds a new color zone into the collection.
    {$ENDIF}
    function Add:TColorZone;
  end;

  {$IFDEF PORTUGUES}
  {:
  Implementa uma zona de cor.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TColorZones)
  }
  {$ELSE}
  {:
  Color class zone.
  @seealso(TZone)
  @seealso(TZones)
  @seealso(TColorZones)
  }
  {$ENDIF}

  { TColorZone }

  TColorZone = class(TZone)
  private
    FResult: TColor;
    procedure SetZoneResult(AValue: TColor);
  protected
    function GetDisplayName: AnsiString; override;
  published
    property ZoneResult:TColor read FResult write SetZoneResult;
  end;

  //////////////////////////////////////////////////////////////////////////////

  //: @exclude
  TObjectWithColorPropetiesColletionItem = class;

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
  TObjectWithColorPropetiesColletion = class(TObjectColletion)
  public
    constructor Create(AOwner:TComponent);
    function Add: TObjectWithColorPropetiesColletionItem;
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

  { TObjectWithColorPropetiesColletionItem }

  TObjectWithColorPropetiesColletionItem = class(TObjectColletionItem, IUnknown)
  private
    FTag:TPLCTag;
    FirstReadOk:Boolean;

    function  QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (defined(WINDOWS) or defined(WIN32) or defined(WIN64)) OR ((not defined(FPC)) OR (FPC_FULLVERSION<20501)))}stdcall{$ELSE}cdecl{$IFEND};
    function _AddRef: LongInt; {$IF (defined(WINDOWS) or defined(WIN32) or defined(WIN64)) OR ((not defined(FPC)) OR (FPC_FULLVERSION<20501)))}stdcall{$ELSE}cdecl{$IFEND};
    function _Release: LongInt; {$IF (defined(WINDOWS) or defined(WIN32) or defined(WIN64)) OR ((not defined(FPC)) OR (FPC_FULLVERSION<20501)))}stdcall{$ELSE}cdecl{$IFEND};

    procedure ReadOkCallBack(Sender:TObject);
    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);

    procedure RecalculateObjectsProperties;
    procedure SetHMITag(AValue: TPLCTag);
  protected
    function GetDisplayName: AnsiString; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ApplyResult(Result:TColor); virtual;
    procedure Loaded; override;
  published
    property PLCTag:TPLCTag read FTag write SetHMITag;
  end;

  //////////////////////////////////////////////////////////////////////////////

  { THMIBooleanPropertyConnector }

  THMIColorPropertyConnector = class(TComponent)
  private
    FTag:TPLCTag;
    FConditionZones:TColorZones;
    FObjects:TObjectWithColorPropetiesColletion;
    procedure ConditionItemChanged(Sender: TObject);
    procedure CollectionNeedsComponentState(var CurState: TComponentState);
    procedure ObjectItemChanged(Sender: TObject);
    function GetConditionZones: TColorZones;
    function GetObjects: TObjectWithColorPropetiesColletion;
    procedure SetConditionZones(AValue: TColorZones);
    procedure SetHMITag(AValue: TPLCTag);
    procedure SetObjects(AValue: TObjectWithColorPropetiesColletion);

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);

    procedure RecalculateObjectsProperties;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Conditions:TColorZones read GetConditionZones write SetConditionZones;
    property AffectedObjects:TObjectWithColorPropetiesColletion read GetObjects write SetObjects;
    property PLCTag:TPLCTag read FTag write SetHMITag;
  end;

implementation

uses typinfo, rttiutils, hsstrings, hmibasiccolletion;

{ THMIBooleanPropertyConnector }

procedure THMIColorPropertyConnector.ConditionItemChanged(Sender: TObject);
begin
  RecalculateObjectsProperties
end;

procedure THMIColorPropertyConnector.CollectionNeedsComponentState(
  var CurState: TComponentState);
begin
  CurState:=ComponentState;
end;

procedure THMIColorPropertyConnector.ObjectItemChanged(Sender: TObject);
begin
  RecalculateObjectsProperties
end;

function THMIColorPropertyConnector.GetConditionZones: TColorZones;
begin
  Result:=FConditionZones;
end;

function THMIColorPropertyConnector.GetObjects: TObjectWithColorPropetiesColletion;
begin
  Result:=FObjects;
end;

procedure THMIColorPropertyConnector.SetConditionZones(AValue: TColorZones);
begin
  FConditionZones.Assign(AValue);
end;

procedure THMIColorPropertyConnector.SetHMITag(AValue: TPLCTag);
begin
  if FTag=AValue then Exit;

  //se o tag esta entre um dos aceitos.
  //check if the tag is valid (only numeric tags)
  if (AValue<>nil) and (not Supports(AValue, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  if FTag<>nil then begin
    FTag.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if AValue<>nil then begin
    AValue.AddWriteFaultHandler(@WriteFaultCallBack);
    AValue.AddTagChangeHandler(@TagChangeCallBack);
    AValue.AddRemoveTagHandler(@RemoveTagCallBack);
    FTag := AValue;
    RecalculateObjectsProperties;
  end;
  FTag:=AValue;
end;

procedure THMIColorPropertyConnector.SetObjects(
  AValue: TObjectWithColorPropetiesColletion);
begin
  FObjects.Assign(AValue);
end;

procedure THMIColorPropertyConnector.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIColorPropertyConnector.TagChangeCallBack(Sender: TObject);
begin
  RecalculateObjectsProperties;
end;

procedure THMIColorPropertyConnector.RemoveTagCallBack(Sender: TObject);
begin
  if Sender=FTag then begin
     FTag:=nil;
  end;
end;

procedure THMIColorPropertyConnector.RecalculateObjectsProperties;
var
  x: TColorZone;
  o: Integer;
begin
  if [csReading,csLoading,csDesigning]*ComponentState<>[] then exit;
  if Assigned(FTag) and Supports(FTag,ITagNumeric) then begin
    x:=TColorZone(FConditionZones.GetZoneFromValue((FTag as ITagNumeric).Value));
    if x=nil then exit;
    for o:=0 to AffectedObjects.Count-1 do begin
      if Assigned(TObjectWithColorPropetiesColletionItem(AffectedObjects.Items[o]).PLCTag) then continue;
      TObjectWithColorPropetiesColletionItem(AffectedObjects.Items[o]).ApplyResult(x.ZoneResult);
    end;
  end;
end;

procedure THMIColorPropertyConnector.Loaded;
begin
  inherited Loaded;
  FConditionZones.Loaded;
  FObjects.Loaded;
  RecalculateObjectsProperties;
end;

procedure THMIColorPropertyConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  if (Operation=opRemove) then begin
    if Assigned(FObjects) then
    for i:=0 to FObjects.Count-1 do begin
      if TObjectWithColorPropetiesColletionItem(FObjects.Items[i]).TargetObject=AComponent then begin
          TObjectWithColorPropetiesColletionItem(FObjects.Items[i]).TargetObject:=nil;
      end;
    end;
    if AComponent=FTag then begin
      FTag.RemoveAllHandlersFromObject(Self);
      FTag:=nil;
    end;
  end;
  inherited Notification(AComponent, Operation);
end;

constructor THMIColorPropertyConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConditionZones:=TColorZones.Create(Self);
  FConditionZones.OnCollectionItemChange:=@ConditionItemChanged;
  FConditionZones.OnNeedCompState:=@CollectionNeedsComponentState;
  FObjects:=TObjectWithColorPropetiesColletion.Create(Self);
  FObjects.OnCollectionItemChange:=@ObjectItemChanged;
  FObjects.OnNeedCompState:=@CollectionNeedsComponentState;
end;

destructor THMIColorPropertyConnector.Destroy;
var
  o: Integer;
begin
  if FObjects.Owner is TComponent then
    for o:=0 to FObjects.Count-1 do begin
      if assigned(TObjectWithColorPropetiesColletionItem(FObjects.Items[o]).TargetObject) then
        TObjectWithColorPropetiesColletionItem(FObjects.Items[o]).TargetObject.RemoveFreeNotification(TComponent(FObjects.Owner));
    end;
  FreeAndNil(FConditionZones);
  FreeAndNil(FObjects);
  SetHMITag(nil);
  inherited Destroy;
end;

{ TObjectWithColorPropetiesColletionItem }

function TObjectWithColorPropetiesColletionItem.QueryInterface(
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (defined(WINDOWS) or defined(WIN32) or defined(WIN64)) OR ((not defined(FPC)) OR (FPC_FULLVERSION<20501)))}stdcall{$ELSE}cdecl{$IFEND};
begin
  if GetInterface(IID, Obj) then
    result:=S_OK
  else
    result:=E_NOINTERFACE;
end;

function TObjectWithColorPropetiesColletionItem._AddRef: LongInt;{$IF (defined(WINDOWS) or defined(WIN32) or defined(WIN64)) OR ((not defined(FPC)) OR (FPC_FULLVERSION<20501)))}stdcall{$ELSE}cdecl{$IFEND};
begin
  Result:=-1;
end;

function TObjectWithColorPropetiesColletionItem._Release: LongInt; {$IF (defined(WINDOWS) or defined(WIN32) or defined(WIN64)) OR ((not defined(FPC)) OR (FPC_FULLVERSION<20501)))}stdcall{$ELSE}cdecl{$IFEND};
begin
  Result:=-1;
end;

procedure TObjectWithColorPropetiesColletionItem.ReadOkCallBack(Sender: TObject
  );
begin
  if FirstReadOk then begin
    TagChangeCallBack(Self);
    FirstReadOk:=false;
  end;
end;

procedure TObjectWithColorPropetiesColletionItem.WriteFaultCallBack(
  Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure TObjectWithColorPropetiesColletionItem.TagChangeCallBack(
  Sender: TObject);
begin
  RecalculateObjectsProperties;
end;

procedure TObjectWithColorPropetiesColletionItem.RemoveTagCallBack(
  Sender: TObject);
begin
  if Sender=FTag then
     FTag:=nil;
end;

procedure TObjectWithColorPropetiesColletionItem.RecalculateObjectsProperties;
var
  x: TColorZone;
  o: Integer;
begin
  if [csReading,csLoading,csDesigning]*THMIBasicColletion(Collection).GetComponentState<>[] then exit;
  if not (Collection.Owner is THMIColorPropertyConnector) then exit;

  if Assigned(FTag) and Supports(FTag,ITagNumeric) then begin
    x:=TColorZone(THMIColorPropertyConnector(Collection.Owner).Conditions.GetZoneFromValue((FTag as ITagNumeric).Value));
    if x<>nil then
      ApplyResult(x.ZoneResult);
  end;
end;

procedure TObjectWithColorPropetiesColletionItem.SetHMITag(AValue: TPLCTag);
begin
  if FTag=AValue then Exit;

  //se o tag esta entre um dos aceitos.
  //check if the tag is valid (only numeric tags)
  if (AValue<>nil) and (not Supports(AValue, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  if FTag<>nil then begin
    FTag.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if AValue<>nil then begin
    AValue.AddReadOkHandler(@ReadOkCallBack);
    AValue.AddWriteFaultHandler(@WriteFaultCallBack);
    AValue.AddTagChangeHandler(@TagChangeCallBack);
    AValue.AddRemoveTagHandler(@RemoveTagCallBack);
    FTag := AValue;
    RecalculateObjectsProperties;
  end;
  FTag:=AValue;
end;

function TObjectWithColorPropetiesColletionItem.GetDisplayName: AnsiString;
begin
  if Assigned(TargetObject) and (TargetObjectProperty<>'') then
    Result:=TargetObject.Name+'.'+TargetObjectProperty
  else
    Result:='(unused)';
end;

constructor TObjectWithColorPropetiesColletionItem.Create(
  ACollection: TCollection);
begin
  inherited Create(ACollection);
  fRequiredTypeName:=PTypeInfo(TypeInfo(TColor))^.Name;
  fRequiredTypeKind:=PTypeInfo(TypeInfo(TColor))^.Kind;
  FirstReadOk:=true;
end;

destructor TObjectWithColorPropetiesColletionItem.Destroy;
begin
  if Assigned(FTag) then
    FTag.RemoveAllHandlersFromObject(Self);
  inherited Destroy;
end;

procedure TObjectWithColorPropetiesColletionItem.ApplyResult(Result: TColor);
begin
  if (AcceptObject(FTargetObject)) AND (AcceptObjectProperty(FTargetObjectProperty)) then
    SetPropValue(FTargetObject,FTargetObjectProperty,Result);
end;

procedure TObjectWithColorPropetiesColletionItem.Loaded;
begin
  inherited Loaded;
  if Assigned(FTag) then
    RecalculateObjectsProperties;
end;

{ TObjectWithColorPropetiesColletion }

constructor TObjectWithColorPropetiesColletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TObjectWithColorPropetiesColletionItem);
end;

function TObjectWithColorPropetiesColletion.Add: TObjectWithColorPropetiesColletionItem;
begin
  Result:=TObjectWithColorPropetiesColletionItem(inherited Add);
end;

{ TColorZones}

constructor TColorZones.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner, TColorZone);
end;

function TColorZones.Add: TColorZone;
begin
  Result:=TColorZone(inherited Add);
end;

{ TColorZone }

procedure TColorZone.SetZoneResult(AValue: TColor);
begin
  if FResult=AValue then Exit;
  FResult:=AValue;
  NotifyChange;
end;

function TColorZone.GetDisplayName: AnsiString;
begin
  Result:=inherited GetDisplayName+', Result='+ColorToString(ZoneResult);
end;

end.
