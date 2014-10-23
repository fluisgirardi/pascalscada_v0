unit HMIComboBox;

{$mode delphi}

interface

uses
  Classes, StdCtrls, HMITypes, Tag, PLCTag;

type

  { THMIComboBox }

  THMIComboBox = class(TCustomComboBox, IHMIInterface, IHMITagInterface)
  protected
    FTag:TPLCTag;
    FSecurityCode: String;
    procedure Select; override;
    procedure SetSecurityCode(AValue: String); virtual;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
    function GetControlSecurityCode:String; virtual;
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
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
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
    property ReadOnly;
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
    property ShowHint;
    property Sorted;
    //property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  end;

implementation

{ THMIComboBox }

procedure THMIComboBox.SetSecurityCode(AValue: String);
begin
  if FSecurityCode=AValue then Exit;
  FSecurityCode:=AValue;
end;

function THMIComboBox.GetControlSecurityCode: String;
begin

end;

procedure THMIComboBox.MakeUnsecure;
begin

end;

procedure THMIComboBox.SetHMITag(t: TPLCTag);
begin

end;

function THMIComboBox.GetHMITag: TPLCTag;
begin

end;

procedure THMIComboBox.CanBeAccessed(a: Boolean);
begin

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

end;

procedure THMIComboBox.NotifyTagChange(Sender: TObject);
begin

end;

procedure THMIComboBox.RemoveTag(Sender: TObject);
begin

end;

procedure THMIComboBox.Select;
begin

  inherited Select;
end;

constructor THMIComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetStyle(csDropDownList);
end;

end.
