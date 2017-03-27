unit HMIComboBox;

interface

uses
  Classes, sysutils, StdCtrls, HMITypes, Tag, PLCTag, ProtocolTypes;

type

  { TComboboxItemInfo }

  TComboboxItemInfo = class
  private
    FObject: TObject;
    FTagValue: Double;
  public
    constructor Create; overload;
    constructor Create(anObject:TObject; aTagValue:Double);
  published
    property TagValue:Double read FTagValue write FTagValue;
    property ItemObject:TObject read FObject write FObject;
  end;

  { THMIComboBox }

  THMIComboBox = class(TCustomComboBox, IHMIInterface)
  private
    FAfterSendValueToTag: TAfterSendNumericValueToTagEvent;
    FBeforeSendValueToTag: TBeforeSendNumericValueToTagEvent;
  protected
    FTag:TPLCTag;
    FSecurityCode: UTF8String;
    FAllowSetIndex,
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;

    //: @exclude
    procedure Select; override;
    //: @exclude
    procedure SetEnabled(Value: Boolean); override;
    //: @exclude
    function  GetItemIndex: integer; override;
    //: @exclude
    procedure SetItemIndex(const Val: integer); override;
    procedure SetItems(const Value: TStrings); override;

    procedure InternalSetItemIndex(const Val: integer); virtual;
    procedure SetSecurityCode(AValue: UTF8String); virtual;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
    function  GetControlSecurityCode:UTF8String; virtual;
    //: @seealso(IHMIInterface.MakeUnsecure)
    procedure MakeUnsecure; virtual;
    //: @seealso(IHMIInterface.SetHMITag)
    procedure SetHMITag(t:TPLCTag); virtual;
    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag; virtual;
    //: @seealso(IHMIInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean); virtual;


    procedure WriteFaultCallBack(Sender:TObject); virtual;
    procedure TagChangeCallBack(Sender:TObject); virtual;
    procedure RemoveTagCallBack(Sender:TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshCombo(Data: PtrInt);
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;// Note: windows has a fixed height in some styles
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled read FIsEnabled write SetEnabled default True;
    property Font;
    property ItemHeight;
    property ItemIndex: integer read GetItemIndex;
    property Items;
    property ItemWidth;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;
    property PopupMenu;
    //property ReadOnly;
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;
    property ShowHint;
    property Sorted;
    //property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    {$IFDEF PORTUGUES}
    //: Evento disparado antes do HMIEdit enviar um valor ao tag associado
    {$ELSE}
    //: Event triggered before HMIEdit send a value to linked tag.
    {$ENDIF}
    property BeforeSendAValueToTag:TBeforeSendNumericValueToTagEvent read FBeforeSendValueToTag write FBeforeSendValueToTag;

    {$IFDEF PORTUGUES}
    //: Evento disparado quando o HMIEdit enviou um valor ao tag associado
    {$ELSE}
    //: Event triggered when the HMIEdit sent a value to linked tag.
    {$ENDIF}
    property AfterSendValueToTag:TAfterSendNumericValueToTagEvent read FAfterSendValueToTag write FAfterSendValueToTag;
  end;

implementation

uses ControlSecurityManager, hsstrings, forms;

constructor TComboboxItemInfo.Create;
begin
  inherited Create;
end;

constructor TComboboxItemInfo.Create(anObject: TObject; aTagValue: Double);
begin
  inherited Create;
  FObject   := anObject;
  FTagValue := aTagValue;
end;

{ THMIComboBox }

procedure THMIComboBox.SetSecurityCode(AValue: UTF8String);
begin
  if FSecurityCode=AValue then Exit;

  if Trim(AValue)='' then
    Self.CanBeAccessed(true)
  else
    with GetControlSecurityManager do begin
      ValidateSecurityCode(AValue);
      if not SecurityCodeExists(AValue) then
        RegisterSecurityCode(AValue);

      Self.CanBeAccessed(CanAccess(AValue));
    end;

  FSecurityCode:=AValue;
end;

function THMIComboBox.GetControlSecurityCode: UTF8String;
begin
  Result:=FSecurityCode;
end;

procedure THMIComboBox.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMIComboBox.SetHMITag(t: TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  //check if the tag is valid.
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SinvalidTag);

  //se ja estou associado a um tag, remove
  //if the control is linked with some tag, removes the link.
  if FTag<>nil then begin
    FTag.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link the control with the new tag.
  if t<>nil then begin
    t.AddWriteFaultHandler(@WriteFaultCallBack);
    t.AddTagChangeHandler(@TagChangeCallBack);
    t.AddRemoveTagHandler(@RemoveTagCallBack);
    FTag := t;
    RefreshCombo(0);
  end;
  FTag := t;
end;

function THMIComboBox.GetHMITag: TPLCTag;
begin
  Result:=FTag;
end;

procedure THMIComboBox.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure THMIComboBox.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIComboBox.TagChangeCallBack(Sender: TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
     Application.QueueAsyncCall(@RefreshCombo,0);
end;

procedure THMIComboBox.RemoveTagCallBack(Sender: TObject);
begin
  if Ftag=Sender then
    FTag:=nil;
end;

procedure THMIComboBox.Select;
  function GetItemValue:Double;
  begin
    if (Items.Objects[GetItemIndex]<>nil) and (Items.Objects[GetItemIndex] is TComboboxItemInfo) then
      Result := TComboboxItemInfo(Items.Objects[GetItemIndex]).TagValue
    else
      Result := GetItemIndex;
  end;

  function SendValue:Boolean;
  begin
    Result:=true;
    if Assigned(FBeforeSendValueToTag) then
      FBeforeSendValueToTag(Self,GetItemValue, Result);
  end;

  procedure AfterSendValue;
  begin
    if Assigned(FAfterSendValueToTag) then
      FAfterSendValueToTag(Self,GetItemIndex);
  end;

begin
  if (FTag<>nil) and Supports(FTag, ITagNumeric) then begin
    if SendValue then begin
      (FTag as ITagNumeric).Value:=GetItemValue;
      AfterSendValue;
    end else
      TagChangeCallBack(Self);
  end;
  inherited Select;
end;

procedure THMIComboBox.SetEnabled(Value: Boolean);
begin
  FIsEnabled:=Value;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

function THMIComboBox.GetItemIndex: integer;
begin
  Result:=inherited GetItemIndex;
end;

procedure THMIComboBox.SetItemIndex(const Val: integer);
begin
  if FAllowSetIndex then
    inherited SetItemIndex(Val);
end;

procedure THMIComboBox.SetItems(const Value: TStrings);
begin
  inherited SetItems(Value);
  RefreshCombo(0);
end;

procedure THMIComboBox.InternalSetItemIndex(const Val: integer);
begin
  FAllowSetIndex:=true;
  try
    SetItemIndex(Val);
  finally
    FAllowSetIndex:=false;
  end;
end;

constructor THMIComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetStyle(csDropDownList);
  FIsEnabled:=true;
  FAllowSetIndex:=false;
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIComboBox.Destroy;
var
  obj: Integer;
begin
  Application.RemoveAsyncCalls(Self);
  for obj:=0 to Items.Count-1 do
    if (Items.Objects[obj]<>nil) and (Items.Objects[obj] is TComboboxItemInfo) then
      Items.Objects[obj].Free;
  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

procedure THMIComboBox.RefreshCombo(Data: PtrInt);
var
  valueReal: Double;
  ValueInt: Int64;
  NewIndex: Integer;
  foundValue: Integer;
  found: Boolean;
  obj: Integer;
begin
  NewIndex:=-1;
  if (FTag<>nil) and Supports(FTag, ITagNumeric) then begin
    valueReal:=(FTag as ITagNumeric).GetValue;

    foundValue:=-1;
    found:=false;
    for obj:=0 to Items.Count-1 do begin
      if (Items.Objects[obj]<>nil) and (Items.Objects[obj] is TComboboxItemInfo) then begin
        found:=true;
        if TComboboxItemInfo(Items.Objects[obj]).TagValue=valueReal then begin
          foundValue:=obj;
          break;
        end;
      end;
    end;

    if found then begin
      NewIndex:=foundValue;
    end else begin
      ValueInt:=Trunc(valueReal);

      if ((valueReal-ValueInt)=0) and ((ValueInt>=0) and (ValueInt<Items.Count)) then
        NewIndex:=ValueInt;

    end;
  end;
  InternalSetItemIndex(NewIndex);
end;

end.
