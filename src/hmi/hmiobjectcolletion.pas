unit hmiobjectcolletion;

interface

uses
  Classes, hmibasiccolletion, typinfo;

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
    FTargetObject,
    FTargetObjectLoaded: TComponent;
    FTargetObjectProperty,
    FTargetObjectPropertyLoaded: String;
    procedure SetTargetObject(AValue: TComponent);
    procedure SetTargetObjectProperty(AValue: String);
  protected
    fRequiredTypeName:String;
    function AcceptObject(obj:TComponent):Boolean; virtual;
    function AcceptObjectProperty(PropertyName:String):Boolean; virtual;
  published
    property TargetObject:TComponent read FTargetObject write SetTargetObject;
    property TargetObjectProperty:String read FTargetObjectProperty write SetTargetObjectProperty;
  public
    procedure Loaded; override;
    constructor Create(ACollection: TCollection); override;
  end;

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

procedure TObjectColletionItem.SetTargetObjectProperty(AValue: String);
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
  PL: PPropList;
  tdata: PTypeData;
  nprops: Integer;
  p: Integer;
begin
  Result:=false;
  if not assigned(obj) then exit;
  tdata:=GetTypeData(obj.ClassInfo);

  GetMem(PL,tdata.PropCount*SizeOf(Pointer));
  try
    nprops:=GetPropList(obj,PL);
    for p:=0 to nprops-1 do begin
      if lowercase(PL^[p]^.PropType^.Name)=lowercase(fRequiredTypeName) then begin
        Result:=true;
        exit;
      end;
    end;
  finally
    Freemem(PL);
  end;
end;

function TObjectColletionItem.AcceptObjectProperty(PropertyName: String
  ): Boolean;
var
  PL: PPropList;
  tdata: PTypeData;
  nprops: Integer;
  p: Integer;
begin
  Result:=false;
  if not Assigned(FTargetObject) then exit;
  tdata:=GetTypeData(FTargetObject.ClassInfo);

  GetMem(PL,tdata.PropCount*SizeOf(Pointer));
  try
    nprops:=GetPropList(FTargetObject,PL);
    for p:=0 to nprops-1 do begin
      if (lowercase(PL^[p]^.Name)=lowercase(PropertyName)) and
         (lowercase(PL^[p]^.PropType^.Name)=lowercase(fRequiredTypeName)) then begin
        Result:=true;
        exit;
      end;
    end;
  finally
    Freemem(PL);
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

