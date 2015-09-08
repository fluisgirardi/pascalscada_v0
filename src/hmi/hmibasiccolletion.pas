unit hmibasiccolletion;

interface

uses
  Classes, SysUtils, pscommontypes;

type

  THMIBasicColletion = class(TCollection)
  private
    FOwner:TPersistent;
    FOnColletionItemChange:TNotifyEvent;
    FOnNeedCompState:TNeedCompStateEvent;
    FComponentState:TComponentState;
 protected
    //: @exclude
    function GetOwner: TPersistent; override;
    //: @exclude
    procedure NeedCurrentCompState;
 public
   //: @exclude
   function GetComponentState:TComponentState;
 public

    {$IFDEF PORTUGUES}
    {:
    @name é o evento chamado quando há alterações de alguma propriedade de
    algum item da coleção.
    }
    {$ELSE}
    {:
    @name is called when a colletion item was changed.
    }
    {$ENDIF}
    property OnCollectionItemChange:TNotifyEvent read FOnColletionItemChange write FOnColletionItemChange;

    {$IFDEF PORTUGUES}
    {:
    @name é o evento chamado quando um item ou a coleção de itens precisa saber
    qual é o atual estado do componente. Este evento também é chamado quando o
    método NeedCurrentCompState é chamado.
    @seealso(ZonesState)
    }
    {$ELSE}
    {:
    @name is called when a colletion item or the entire collection object
    needs to know what's the current state of the owner component. It's called
    too when the procedure NeedCurrentCompState is called.
    @seealso(ZonesState)
    }
    {$ENDIF}
    property OnNeedCompState:TNeedCompStateEvent read FOnNeedCompState write FOnNeedCompState;
 public
    //: @exclude
    constructor Create(Owner:TPersistent; ItemClass: TCollectionItemClass); virtual;

    {$IFDEF PORTUGUES}
    {:
    Este método deve ser chamado através do método Loaded de seu componente para
    informar para as zonas que a partir de agora elas devem operar normalmente e
    não mais no modo de carga de configurações. @bold(Se este método não for
    chamado as zonas não vão se comportar da maneira esperada).
    }
    {$ELSE}
    {:
    This procedure must be called from Loaded procedure of your component/control
    to tell that all properties are loaded. @bold(If this procedure aren't
    called, the animation zones will not work properly).
    }
    {$ENDIF}
    procedure Loaded; virtual;

    {$IFDEF PORTUGUES}
    {:
    Propriedade que lê o estado do componente e o repassa para a coleção de
    zonas. Usa o evento OnNeedCompState para obter o atual estado.
    @seealso(OnNeedCompState)
    }
    {$ELSE}
    {:
    Property that reads the current state of the owner component/control and
    repass it to the animation collection zones. Uses the event OnNeedCompState
    to get the actual state of the owner.
    @seealso(OnNeedCompState)
    }
    {$ENDIF}
    property  CollectionState:TComponentState read GetComponentState;
  end;

  { THMIBasicColletionItem }

  THMIBasicColletionItem = class(TCollectionItem)
  protected
    {: @exclude }
    procedure NotifyChange; virtual;
  public
    {: @exclude }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    {: @exclude }
    procedure Loaded; virtual;
  end;

implementation

{ THMIBasicColletionItem }

procedure THMIBasicColletionItem.NotifyChange;
begin
  if Collection is THMIBasicColletion then
    with Collection as THMIBasicColletion do
      if Assigned(OnCollectionItemChange) then
        OnCollectionItemChange(Self);
end;

constructor THMIBasicColletionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  if Collection is THMIBasicColletion then
     THMIBasicColletion(Collection).NeedCurrentCompState;
end;

destructor THMIBasicColletionItem.Destroy;
begin
  inherited Destroy;
end;

procedure THMIBasicColletionItem.Loaded;
begin
end;

{ THMIBasicColletion }

constructor THMIBasicColletion.Create(Owner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FOwner:=Owner;
end;

function THMIBasicColletion.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;

function THMIBasicColletion.GetComponentState: TComponentState;
begin
  NeedCurrentCompState;
  Result := FComponentState;
end;

procedure THMIBasicColletion.NeedCurrentCompState;
begin
  if assigned(FOnNeedCompState) then
     FOnNeedCompState(FComponentState);
end;

procedure THMIBasicColletion.Loaded;
var
   i:LongInt;
begin
   for i:=0 to Count-1 do
     if Items[i] is THMIBasicColletionItem then
       THMIBasicColletionItem(Items[i]).Loaded;
end;

end.

