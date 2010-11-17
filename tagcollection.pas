{:
  @abstract(Implementa uma coleção de tags.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit tagcollection;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, PLCTag, HMIZones, ProtocolTypes, Tag;

type
  {:
  @abstract(Classe de um item na coleção de tags.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  }
  TTagCollectionItem=class(TCollectionItem, IUnknown, IHMITagInterface)
  private
    FTag:TPLCTag;
    procedure SetTag(t:TPLCTag);

    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

    //IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    //: Notifica a coleção de tags que um item teve alteração de valor.
    procedure NotifyChange;
    //: Descrição do item na coleção.
    function  GetDisplayName: string; override;
  public
    //: @exclude
    constructor Create(Collection: TCollection); override;
    //: @exclude
    destructor  Destroy; override;
    {:
    Chamado quando o dono da coleção foi totalmente carregado.
    Use este método para realizar ações que precisem ser feitas
    com a coleção totalmente carregada.
    }
    procedure   Loaded;
  published
    //: Tag do item da coleção.
    property PLCTag:TPLCTag read FTag write SetTag;
  end;

  {:
  @abstract(Classe que representa uma coleção de tags.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  Use este componente em lugares que são necessarios mais de um tag, como
  por exemplo históricos e receitas.
  }
  TTagCollection=class(TCollection)
  private
    FOnItemChange:TNotifyEvent;
    FOnValuesChange:TNotifyEvent;
    FOnNeedCompState:TNeedCompStateEvent;
    FComponentState:TComponentState;
  protected
    //: Retorna o estado atual do dono da coleção.
    function  GetComponentState:TComponentState;
    //: Solicita o estado atual do dono da coleção.
    procedure NeedCurrentCompState;
  published
    //: Evento que informa ao dono da coleção que um item foi alterado.
    property OnItemChange:TNotifyEvent read FOnItemChange write FOnItemChange;
    //: Evento que informa ao dono da coleção que um item da coleção teve seu valor alterado.
    property OnValuesChange:TNotifyEvent read FOnValuesChange write FOnValuesChange;
    //: Evento usado para repassar o estado do dono da coleção para a coleção.
    property OnNeedCompState:TNeedCompStateEvent read FOnNeedCompState write FOnNeedCompState;
  public
    //: @exclude
    constructor Create(ItemClass: TCollectionItemClass);
    {:
    Método chamado pelo dono da coleção para sinalizar que ele foi totalmente
    carregado.
    }
    procedure Loaded;
    //: Informa o estado atual da coleção de tags.
    property  ZonesState:TComponentState read GetComponentState;
  end;

implementation

uses hsstrings;

constructor TTagCollectionItem.Create(Collection: TCollection);
begin
  inherited create(Collection);
  FTag:=nil;
end;

destructor  TTagCollectionItem.Destroy;
begin
  if FTag<>nil then
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  Inherited Destroy;
end;

procedure   TTagCollectionItem.SetTag(t:TPLCTag);
begin
  if t=FTag then exit;

  if (t<>nil) and (not Supports(t, ITagInterface)) then
    raise Exception.Create(SinvalidTag);

  if Ftag<>nil then
    FTag.RemoveCallBacks(Self as IHMITagInterface);

  if t<>nil then
    FTag.AddCallBacks(Self as IHMITagInterface);

  FTag:=t;

  NotifyChange;
end;

procedure   TTagCollectionItem.NotifyChange;
begin
  with Collection as TTagCollection do
    if Assigned(OnItemChange) then
      OnItemChange(Self);
end;

function    TTagCollectionItem.GetDisplayName: string;
begin
  if FTag=nil then
    Result := SEmpty
  else
    Result := FTag.Name;
end;

procedure   TTagCollectionItem.Loaded;
begin
  //called when collection owner is completly loaded.
  //use this to do some actions that need to be
  //run only when object is loaded.
end;

procedure TTagCollectionItem.NotifyReadOk;
begin

end;

procedure TTagCollectionItem.NotifyReadFault;
begin

end;

procedure TTagCollectionItem.NotifyWriteOk;
begin

end;

procedure TTagCollectionItem.NotifyWriteFault;
begin
  NotifyTagChange(Self);
end;

procedure TTagCollectionItem.NotifyTagChange(Sender:TObject);
begin
  with Collection as TTagCollection do
    if Assigned(OnValuesChange) then
      OnValuesChange(Self);
end;

procedure TTagCollectionItem.RemoveTag(Sender:TObject);
begin
  if FTag=sender then
    FTag := nil;
end;

function TTagCollectionItem.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    result:=S_OK
  else
    result:=E_NOINTERFACE;
end;

function TTagCollectionItem._AddRef: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  result:=-1;
end;

function TTagCollectionItem._Release: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  result:=-1;
end;

//******************************************************************************
// TTagCollection
//******************************************************************************

constructor TTagCollection.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
end;

function    TTagCollection.GetComponentState:TComponentState;
begin
  NeedCurrentCompState;
  Result := FComponentState;
end;

procedure   TTagCollection.NeedCurrentCompState;
begin
  if assigned(FOnNeedCompState) then
    FOnNeedCompState(FComponentState);
end;

procedure   TTagCollection.Loaded;
var
   i:Integer;
begin
  for i:=0 to Count-1 do
    TZone(Items[i]).Loaded;
end;

end.

