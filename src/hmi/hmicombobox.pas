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
  published
    property TagValue:Double read FTagValue write FTagValue;
    property ItemObject:TObject read FObject write FObject;
  end;

  { THMIComboBox }

  THMIComboBox = class(TCustomComboBox, IHMIInterface, IHMITagInterface)
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

    //: @seealso(IHMITagInterface.NotifyReadOk)
    procedure NotifyReadOk; virtual;
    //: @seealso(IHMITagInterface.NotifyReadFault)
    procedure NotifyReadFault; virtual;
    //: @seealso(IHMITagInterface.NotifyWriteOk)
    procedure NotifyWriteOk; virtual;
    //: @seealso(IHMITagInterface.NotifyWriteFault)
    procedure NotifyWriteFault; virtual;
    //: @seealso(IHMITagInterface.NotifyTagChange)
    procedure NotifyTagChange(Sender:TObject); virtual;
    //: @seealso(IHMITagInterface.RemoveTag)
    procedure RemoveTag(Sender:TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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

uses ControlSecurityManager, hsstrings;

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
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  end;

  //adiona o callback para o novo tag
  //link the control with the new tag.
  if t<>nil then begin
    t.AddCallBacks(Self as IHMITagInterface);
    FTag := t;
    NotifyTagChange(Self);
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

procedure THMIComboBox.NotifyReadOk;
begin

end;

procedure THMIComboBox.NotifyReadFault;
begin

end;

procedure THMIComboBox.NotifyWriteOk;
begin

end;

procedure THMIComboBox.NotifyWriteFault;
begin
  NotifyTagChange(Self);
end;

procedure THMIComboBox.NotifyTagChange(Sender: TObject);
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

procedure THMIComboBox.RemoveTag(Sender: TObject);
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
  if (FTag<>nil) and Supports(FTag, ITagNumeric) and SendValue then begin
    (FTag as ITagNumeric).Value:=GetItemValue;
    AfterSendValue;
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
  NotifyTagChange(Self);
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
  for obj:=0 to Items.Count-1 do
    if (Items.Objects[obj]<>nil) and (Items.Objects[obj] is TComboboxItemInfo) then
      Items.Objects[obj].Free;
  if FTag<>nil then
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

end.
