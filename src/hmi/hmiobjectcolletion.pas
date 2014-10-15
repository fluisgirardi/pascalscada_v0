unit hmiobjectcolletion;

interface

uses
  Classes;

type
  {$IFDEF PORTUGUES}
  {:
  Implementa uma coleção de objetos.
  }
  {$ELSE}
  {:
  Object colletion class.
  }
  TObjectColletion = class(TCollection);

  {$IFDEF PORTUGUES}
  {:
  Implementa um item da coleção de objetos.
  }
  {$ELSE}
  {:
  Object colletion class item.
  }
  TObjectColletionItem = Class(TCollectionItem)
  private
    FTargetObject: TComponent;
    FTargetObjectProperty: String;
    procedure SetTargetObject(AValue: TComponent);
    procedure SetTargetObjectProperty(AValue: String);
  protected
    function AcceptObject(obj:TComponent):Boolean; virtual;
    function AcceptObjectProperty(PropertyName:String):Boolean; virtual;
  published
    property TargetObject:TComponent read FTargetObject write SetTargetObject;
    property TargetObjectProperty:String read FTargetObjectProperty write SetTargetObjectProperty;
  end;

implementation

{ TObjectColletionItem }

procedure TObjectColletionItem.SetTargetObject(AValue: TComponent);
begin
  if FTargetObject=AValue then Exit;
  if not AcceptObject(AValue) then exit;
  FTargetObject:=AValue;
  if Collection.Owner is TComponent then
    FTargetObject.FreeNotification(TComponent(Collection.Owner));
end;

procedure TObjectColletionItem.SetTargetObjectProperty(AValue: String);
begin
  if FTargetObjectProperty=AValue     then Exit;
  if not Assigned(FTargetObject)      then exit;
  if not AcceptObject(FTargetObject)  then exit;
  if not AcceptObjectProperty(AValue) then exit;

  FTargetObjectProperty:=AValue;
end;

function TObjectColletionItem.AcceptObject(obj: TComponent): Boolean;
begin
  Result:=false;
end;

function TObjectColletionItem.AcceptObjectProperty(PropertyName: String
  ): Boolean;
begin
  Result:=false;
end;

end.

