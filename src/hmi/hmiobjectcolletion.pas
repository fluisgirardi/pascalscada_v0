unit hmiobjectcolletion;

interface

uses
  Classes, hmibasiccolletion, rttiutils, typinfo;

type
  {$IFDEF PORTUGUES}
  {:
  Implementa uma coleção de objetos.
  }
  {$ELSE}
  {:
  Object colletion class.
  }
  {$ENDIF}
  TObjectColletion = class(THMIBasicColletion);

  {$IFDEF PORTUGUES}
  {:
  Implementa um item da coleção de objetos.
  }
  {$ELSE}
  {:
  Object colletion class item.
  }
  {$ENDIF}
  TObjectColletionItem = Class(THMIBasicColletionItem)
  private
    FTag: PtrUInt;
  protected
    FTargetObject,
    FTargetObjectLoaded: TComponent;
    FTargetObjectProperty,
    FTargetObjectPropertyLoaded: AnsiString;
    procedure SetTargetObject(AValue: TComponent);
    procedure SetTargetObjectProperty(AValue: AnsiString);
  protected
    fRequiredTypeName:AnsiString;
    fRequiredTypeKind:TTypeKind;
    function AcceptObject(obj:TComponent):Boolean; virtual;
    function AcceptObjectProperty(PropertyName:AnsiString):Boolean; virtual;
  published
    property Tag:PtrUInt read FTag write FTag;
    property TargetObject:TComponent read FTargetObject write SetTargetObject;
    property TargetObjectProperty:AnsiString read FTargetObjectProperty write SetTargetObjectProperty;
  public
    procedure Loaded; override;
    constructor Create(ACollection: TCollection); override;
  end;

  TObjectColletionItemClass = class of TObjectColletionItem;

implementation

{ TObjectColletionItem }

procedure TObjectColletionItem.SetTargetObject(AValue: TComponent);
begin
  if [csReading,csLoading]*THMIBasicColletion(Collection).CollectionState<>[] then begin
    FTargetObjectLoaded:=AValue;
    exit;
  end;

  if FTargetObject=AValue    then Exit;
  if AValue=Collection.Owner then exit;

  if AValue=nil then begin
    FTargetObject:=nil;
    FTargetObjectProperty:='';
    exit;
  end;

  if not AcceptObject(AValue) then exit;
  FTargetObject:=AValue;
  if Collection.Owner is TComponent then
    FTargetObject.FreeNotification(TComponent(Collection.Owner));
end;

procedure TObjectColletionItem.SetTargetObjectProperty(AValue: AnsiString);
begin
  if [csReading,csLoading]*THMIBasicColletion(Collection).CollectionState<>[] then begin
    FTargetObjectPropertyLoaded:=AValue;
    exit;
  end;

  if FTargetObjectProperty=AValue     then Exit;

  if AValue='' then begin
    FTargetObjectProperty:='';
    exit;
  end;

  if not Assigned(FTargetObject)      then exit;
  if not AcceptObject(FTargetObject)  then exit;
  if not AcceptObjectProperty(AValue) then exit;

  FTargetObjectProperty:=AValue;
end;

function TObjectColletionItem.AcceptObject(obj: TComponent): Boolean;
var
  helper:TPropInfoList;
  pidx: Integer;
begin
  Result:=false;
  helper := TPropInfoList.Create(obj,[fRequiredTypeKind]);
  try
    for pidx:=0 to helper.Count-1 do begin
      if helper.Items[pidx]^.PropType^.Name=fRequiredTypeName then begin
        Result:=true;
        Exit;
      end;
    end;
  finally
    helper.Free;
  end;
end;

function TObjectColletionItem.AcceptObjectProperty(PropertyName: AnsiString
  ): Boolean;
var
  helper:TPropInfoList;
  pidx: Integer;
begin
  Result:=false;
  if FTargetObject=nil then exit;
  helper := TPropInfoList.Create(FTargetObject,[fRequiredTypeKind]);
  try
    for pidx:=0 to helper.Count-1 do begin
      if (lowercase(helper.Items[pidx]^.Name)=LowerCase(PropertyName)) and
         (helper.Items[pidx]^.PropType^.Name=fRequiredTypeName)  then begin
        Result:=true;
        Exit;
      end;
    end;
  finally
    helper.Free;
  end;
end;

procedure TObjectColletionItem.Loaded;
begin
  inherited Loaded;
  SetTargetObject(FTargetObjectLoaded);
  SetTargetObjectProperty(FTargetObjectPropertyLoaded);
end;

constructor TObjectColletionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fRequiredTypeName:='';
end;

end.

