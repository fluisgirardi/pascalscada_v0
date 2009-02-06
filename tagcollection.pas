unit tagcollection;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, PLCTag, HMIZones, ProtocolTypes;

type
  TTagCollectionItem=class(TCollectionItem)
  private
    FTag:TPLCTag;
    procedure SetTag(t:TPLCTag);
    procedure TagChanges(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    procedure NotifyChange;
    function  GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor  Destroy; override;
    procedure   Loaded;
  published
    property PLCTag:TPLCTag read FTag write SetTag;
  end;

  TTagCollection=class(TCollection)
  private
    FOnItemChange:TNotifyEvent;
    FOnValuesChange:TNotifyEvent;
    FOnNeedCompState:TNeedCompStateEvent;
    FComponentState:TComponentState;
  protected
    function  GetComponentState:TComponentState;
    procedure NeedCurrentCompState;
  published
    property OnItemChange:TNotifyEvent read FOnItemChange write FOnItemChange;
    property OnValuesChange:TNotifyEvent read FOnValuesChange write FOnValuesChange;
    property OnNeedCompState:TNeedCompStateEvent read FOnNeedCompState write FOnNeedCompState;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    procedure Loaded;
    property  ZonesState:TComponentState read GetComponentState;
  end;

implementation

constructor TTagCollectionItem.Create(Collection: TCollection);
begin
  inherited create(Collection);
  FTag:=nil;
end;

destructor  TTagCollectionItem.Destroy;
begin
  if FTag<>nil then
    FTag.RemoveChangeCallBack(TagChanges);
  Inherited Destroy;
end;

procedure   TTagCollectionItem.SetTag(t:TPLCTag);
begin
  if t=FTag then exit;

  if (t<>nil) and ((t as ITagInterface)=nil) then
    raise Exception.Create('Tag inválido!');

  if Ftag<>nil then
    FTag.RemoveChangeCallBack(TagChanges);

  if t<>nil then
    FTag.AddChangeCallBack(TagChanges,RemoveTag);

  FTag:=t;

  NotifyChange;
end;

procedure   TTagCollectionItem.TagChanges(Sender:TObject);
begin
  with Collection as TTagCollection do
    if Assigned(OnValuesChange) then
      OnValuesChange(Self);
end;

procedure   TTagCollectionItem.RemoveTag(Sender:TObject);
begin
  FTag:=nil;
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
    Result := 'Empty entry'
  else
    Result := FTag.Name;
end;

procedure   TTagCollectionItem.Loaded;
begin
  //called when collection owner is completly loaded.
  //use this to do some actions that need to be
  //run only when object is loaded.
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

