{$IFDEF PORTUGUES}
{:
  @abstract(Unit do formulário TagBuilder para a familia de drivers da Siemens.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit of Siemens TagBuilder wizard.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit us7tagbuilder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Spin, tag {$IFDEF FPC}, LCLIntf, LResources{$ENDIF}
  {$IF defined(WINDOWS) or defined(WIN32) or defined(WIN64)}, windows{$IFEND};

type
  {$IFDEF PORTUGUES}
  //: Rotina de checagem de nomes.
  {$ELSE}
  //: Name check routine.
  {$ENDIF}
  TCheckNames = procedure (Sender:TObject; NewName:String; var AcceptNewName:Boolean) of object;

  {$IFDEF PORTUGUES}
  {:
  Editor de bits de um item da estrtura.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Bit editor of a structure item.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TTagBitItemEditor = class(TPanel)
  private
    FTagName:String;
    FStartBit,
    FEndBit:Integer;
    FCheckNames:TCheckNames;
    fedtItemName:TEdit;
    lblStart,
    lblEnd:TLabel;
    spinStart,
    spinEnd:TSpinEdit;
    btnDel:TButton;
    fondelclick:TNotifyEvent;
    procedure SetTagName(newname:String);
    procedure SetStartBit(bitindex:integer);
    procedure SetEndBit(bitindex:integer);
  private
    procedure SpinEditChanges(Sender:TObject);
    procedure edtItemNameExit(Sender:TObject);
    procedure btnDelClick(Sender:TObject);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    property TagName:String read FTagName write SetTagName;
    property StartBit:Integer read FStartBit write SetStartBit;
    property EndBit:Integer read FEndBit write SetEndBit;
    property OnCheckNames:TCheckNames read FCheckNames write FCheckNames;
    property OnDelClick:TNotifyEvent read fondelclick write fondelclick;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor de itens da estrtura.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Structure item editor.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TS7TagItemEditor = class(TPanel)
  private
    FTagName:String;
    FTagType:TTagType;
    FTagScan:TRefreshTime;
    FSwapWords,
    FSwapBytes,
    FSkip:Boolean;
    edtItemName:TEdit;
    cmbItemType:TComboBox;
    spinScan:TSpinEdit;
    optSwapBytes,
    optSwapWords,
    optSkip:TCheckBox;
    btnUp,
    btnDown,
    btnDel,
    btnBits:TButton;
    TagArea,
    BitArea:TPanel;
    FCheckNames:TCheckNames;
    FUpClickEvent,
    FDownClickEvent,
    FDelClickEvent,
    FBitsClickEvent:TNotifyEvent;
    BitList:TList;
    DelTimer:TTimer;
    DelList:TList;
    FOnTypeChange,
    FOnSkipChange,
    FOnDelBitItem:TNotifyEvent;
    procedure SetTagName(newName:String);
    procedure SetTagType(newType:TTagType);
    procedure SetTagScan(newScan:TRefreshTime);
    procedure SetSwapBytes(swap:Boolean);
    procedure SetSwapWords(swap:Boolean);
    procedure SetSkipTag(Skip:Boolean);
  private
    procedure CheckNames(Sender:TObject; NewName:String; var AcceptNewName:Boolean);
    procedure btnClick(Sender:TObject);
    procedure optChange(Sender:TObject);
    procedure edtItemNameExit(Sender:TObject);
    procedure DelBitItem(Sender:TObject);
  private
    function GetBitCount:Integer;
    function GetBit(Index:Integer):TTagBitItemEditor;
    procedure OnDelTimer(Sender:TObject);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure PopulateCombo;

    procedure EnableTagType(ToEnable:Boolean);
    procedure EnableScanRate(ToEnable:Boolean);
    procedure EnableSwapBytes(ToEnable:Boolean);
    procedure EnableSwapWords(ToEnable:Boolean);

    property  Bit[index:Integer]:TTagBitItemEditor read GetBit;
  published
    property  BitCount:Integer read GetBitCount;
    function  AddBit:TTagBitItemEditor;
    procedure DelBit(index:Integer);
    function  GetIndex(tbitEditor:TTagBitItemEditor):Integer;
    function  AcceptName(TheName:String):Boolean;
  published
    property TagName:String Read FTagName write SetTagName;
    property TagType:TTagType read FTagType write SetTagType;
    property TagScan:TRefreshTime read FTagScan write SetTagScan;
    property SwapBytes:Boolean read FSwapBytes write SetSwapBytes;
    property SwapWords:Boolean read FSwapWords write SetSwapWords;
    property SkipTag:Boolean read FSkip write SetSkipTag;
    property OnCheckNames:TCheckNames read FCheckNames write FCheckNames;
    property OnUpClick:TNotifyEvent read FUpClickEvent write FUpClickEvent;
    property OnDownClickEvent:TNotifyEvent read FDownClickEvent write FDownClickEvent;
    property OnDelClickEvent:TNotifyEvent read FDelClickEvent write FDelClickEvent;
    property OnBitsClickEvent:TNotifyEvent read FBitsClickEvent write FBitsClickEvent;
    property OnTypeChange:TNotifyEvent read FOnTypeChange write FOnTypeChange;
    property OnSkipChange:TNotifyEvent read FOnSkipChange write FOnSkipChange;
    property OnDelBitItem:TNotifyEvent read FOnDelBitItem write FOnDelBitItem;
  end;

  {$IFDEF PORTUGUES}
  {:
  Tag builder da familia de protocolos da Siemens.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  TagBuilder of siemens protocol family.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TfrmS7TagBuilder = class(TForm)
    BlockName: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    MemoryArea: TRadioGroup;
    Panel3: TPanel;
    Panel4: TPanel;
    PLCAddress: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PLCStation: TSpinEdit;
    PLCSlot: TSpinEdit;
    PLCRack: TSpinEdit;
    grptagtype: TGroupBox;
    optplctagnumber: TRadioButton;
    optplcblock: TRadioButton;
    optplcStruct: TRadioButton;
    TabSheet4: TTabSheet;
    Panel5: TPanel;
    lblNumItems: TLabel;
    spinNumItens: TSpinEdit;
    spinStartAddress: TSpinEdit;
    lblStartAddress: TLabel;
    spinDBNumber: TSpinEdit;
    lblDBNumber: TLabel;
    ScrollBox1: TScrollBox;
    lblBlockType: TLabel;
    BlockType: TComboBox;
    Button1: TButton;
    BlockScan: TSpinEdit;
    lblBlockScan: TLabel;
    BlockSwapBytes: TCheckBox;
    BlockSwapWords: TCheckBox;
    StructScan: TSpinEdit;
    lblStructScan: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Timer1: TTimer;
    btnCancel: TButton;
    btnBack: TButton;
    btnNext: TButton;
    btnFinish: TButton;
    lblBlockName: TLabel;
    procedure btnFinishClick(Sender: TObject);
    procedure MemoryAreaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnBitsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TabSheet1Show(Sender: TObject);
    procedure BlockTypeChange(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject;
      var AllowChange: Boolean);
    procedure TabSheet4Show(Sender: TObject);
    procedure optplcblockClick(Sender: TObject);
    procedure spinStartAddressChange(Sender: TObject);
  private
    OldPage:TTabSheet;
    FItemId:Integer;
    FStructureModified:Boolean;
    TagList,
    ItemsToDel:TList;
    procedure UpdateStatusAndBlockName;
    procedure UpdateStructItems;
    procedure SkipChanged(Sender:TObject);
    procedure StructItemTypeChanged(Sender:TObject);
    procedure CheckNames(Sender:TObject; NewName:String; var AcceptNewName:Boolean);
    function GetStructItemsCount:Integer;
    function GetStructItem(index:Integer):TS7TagItemEditor;
    function GetStructureSizeInBytes:Integer;
    function GetRealStartOffset:Integer;
    function GetRealEndOffset:Integer;
    function GetStartOffset:Integer;
    function GetEndOffset:Integer;
    function AtLeastOneItemIsValid:Boolean;
    procedure BitItemDeleted(Sender:TObject);
    procedure UpdateFlagDBandVStrucItemName;
  public
    destructor Destroy; override;
    function GetTagType:Integer;
    function CurBlockType:TTagType;
    function GetTheLastItemOffset:Integer;
    property StructItemsCount:Integer read GetStructItemsCount;
    property StructItem[index:integer]:TS7TagItemEditor read GetStructItem;
    property StructureSizeInBytes:Integer read GetStructureSizeInBytes;
    property RealStartOffset:Integer read GetRealStartOffset;
    property RealEndOffset:Integer read GetRealEndOffset;
    property StartOffset:Integer read GetStartOffset;
    property EndOffset:Integer read GetEndOffset;
  end;

var
  frmS7TagBuilder: TfrmS7TagBuilder;

implementation

uses ubitmapper, hsstrings;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
  {$R us7tagbuilder.lfm}
  {$IFEND}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
//TagBitEditor
///////////////////////////////////////////////////////////////////////////////
constructor TTagBitItemEditor.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Align:=alTop;
  BevelOuter:=bvNone;
  Height:=25;
  Caption:='';

  fedtItemName:=TEdit.Create(Self);
  with fedtItemName do begin
    Parent:=Self;
    Left :=16;
    Top  := 2;
    Width:=137;
    OnExit:=edtItemNameExit;
  end;

  lblStart:=TLabel.Create(Self);
  with lblStart do begin
    Parent:=Self;
    AutoSize:=false;
    Alignment:=taRightJustify;
    Layout:=tlCenter;
    Caption:='Start Bit';
    Height:=21;
    Width:=49;
    Left:=168;
    Top:=2;
  end;

  lblEnd:=TLabel.Create(Self);
  with lblEnd do begin
    Parent:=Self;
    AutoSize:=false;
    Alignment:=taRightJustify;
    Layout:=tlCenter;
    Caption:='End Bit';
    Height:=21;
    Width:=49;
    Left:=272;
    Top:=2;
  end;

  spinStart:=TSpinEdit.Create(Self);
  with spinStart do begin
    Parent:=Self;
    Height:=22;
    Left:=224;
    MaxValue:=31;
    MinValue:=0;
    Top:=2;
    Width:=41;
    OnChange := SpinEditChanges;
  end;

  spinEnd:=TSpinEdit.Create(Self);
  with spinEnd do begin
    Parent:=Self;
    Height:=22;
    Left:=328;
    MaxValue:=31;
    MinValue:=0;
    Top:=2;
    Width:=41;
    OnChange := SpinEditChanges;
  end;

  btnDel:=TButton.Create(Self);
  with btnDel do begin
    Parent:=Self;
    Caption:='Del';
    Height:=22;
    Left:=503;
    Top:=3;
    Width:=33;
    OnClick:=btnDelClick;
  end;
end;

destructor  TTagBitItemEditor.Destroy;
begin
  fedtItemName.Destroy;
  lblStart.Destroy;
  lblEnd.Destroy;
  spinStart.Destroy;
  spinEnd.Destroy;
  btnDel.Destroy;
  inherited Destroy;
end;

procedure TTagBitItemEditor.SetTagName(newname:String);
var
  accept:Boolean;
begin
  accept:=true;
  if Assigned(FCheckNames) then
    FCheckNames(Self,newname,accept);

  if accept then begin
    FTagName:=newname;
    fedtItemName.Text:=newname;
    fedtItemName.Modified:=false;
  end;
end;

procedure TTagBitItemEditor.SetStartBit(bitindex:integer);
begin
  FStartBit:=bitindex;
  spinStart.Value:=bitindex;

  if FStartBit>FEndBit then begin
    FEndBit:=bitindex;
    spinEnd.Value:=FEndBit;
  end;
end;

procedure TTagBitItemEditor.SetEndBit(bitindex:integer);
begin
  FEndBit:=bitindex;
  spinEnd.Value:=bitindex;

  if FStartBit>FEndBit then begin
    FStartBit:=bitindex;
    spinStart.Value:=FStartBit;
  end;
end;

procedure TTagBitItemEditor.SpinEditChanges(Sender:TObject);
begin
  if (Sender=spinStart) then begin
    FStartBit:=spinStart.Value;
    if (spinStart.Value>spinEnd.Value) then begin
      spinEnd.Value := spinStart.Value;
      FEndBit:=spinEnd.Value;
    end;
  end;

  if (Sender=spinEnd) then begin
    FEndBit:=spinEnd.Value;
    if (spinStart.Value>spinEnd.Value) then begin
      spinStart.Value := spinEnd.Value;
      FStartBit:=spinStart.Value;
    end;
  end;
end;

procedure TTagBitItemEditor.edtItemNameExit(Sender:TObject);
var
  accept:Boolean;
begin
  if not fedtItemName.Modified then exit;
  accept:=true;
  if Assigned(FCheckNames) then
    FCheckNames(Self,fedtItemName.Text,accept);

  if accept then begin
    fedtItemName.Modified:=false;
    FTagName:=fedtItemName.Text;
  end else begin
    fedtItemName.Text:=FTagName;
    fedtItemName.Modified:=false;
  end;
end;

procedure TTagBitItemEditor.btnDelClick(Sender:TObject);
begin
  if assigned(fondelclick) then
    fondelclick(Self);
end;

///////////////////////////////////////////////////////////////////////////////
//S7TagEditor
///////////////////////////////////////////////////////////////////////////////

constructor TS7TagItemEditor.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Align:=altop;
  BevelOuter:=bvNone;
  Height:=30;
  Caption:='';
  Top:=$0FFFFFFF;

  BitList:=TList.Create;
  DelList:=TList.Create;
  DelTimer:=TTimer.Create(Self);  
  DelTimer.Enabled:=false;
  DelTimer.Interval:=10;
  DelTimer.OnTimer:=OnDelTimer;

  FSkip:=false;
  FSwapWords:=false;
  FSwapBytes:=false;
  FTagType:=pttDefault;

  TagArea:=TPanel.Create(Self);
  with TagArea do begin
    Parent:=Self;
    Align:=alTop;
    BevelOuter:=bvNone;
    Height:=25;
    Caption:='';
  end;

  BitArea:=TPanel.Create(Self);
  with BitArea do begin
    Parent:=Self;
    Align:=alClient;
    BevelOuter:=bvNone;
    Top:=31;
    Height:=0;
    Caption:='';
  end;

  edtItemName:=TEdit.Create(Self);
  with edtItemName do begin
    Parent := TagArea;
    Height:=21;
    Width:=152;
    Left:=2;
    Top:=2;
    OnExit:=edtItemNameExit;
  end;

  //correção
  //desenha um botão que não faz nada
  //para forcar a perda de foco por tab do
  //edit anterior
  //
  //FIX
  //draw a button that does nothing
  //to force the focus lost using of the previos edit
  btnUp:=TButton.Create(Self);
  with btnUp do begin
    Parent:=TagArea;
    Height:=22;
    Left:=155;
    Top:=3;
    Width:=4;
  end;

  cmbItemType:=TComboBox.Create(Self);
  with cmbItemType do begin
    Parent:=TagArea;
    Left:=160;
    top:=2;
    Style:=csDropDownList;
    Width:=78;
    OnChange:=optChange;
    OnEnter:=edtItemNameExit;
    OnClick:=edtItemNameExit;
    OnDropDown:=edtItemNameExit;
  end;

  spinScan:=TSpinEdit.Create(Self);
  with spinScan do begin
    Parent:=TagArea;
    Height:=22;
    Left:=238;
    MaxValue:=$7FFFFFFF;
    MinValue:=0;
    Value:=1000;
    Top:=2;
    Width:=55;
    OnChange:=optChange;
  end;

  optSwapBytes:=TCheckBox.Create(Self);
  with optSwapBytes do begin
    Parent:=TagArea;
    Caption:='Bytes';
    Height:=17;
    Left:=297;
    Top:=4;
    Width:=50;
    Checked:=FSwapBytes;
    Enabled:=false;
    OnClick:=optChange;
  end;

  optSwapWords:=TCheckBox.Create(Self);
  with optSwapWords do begin
    Parent:=TagArea;
    Caption:='Words';
    Height:=17;
    Left:=347;
    Top:=4;
    Width:=51;
    Checked:=FSwapWords;
    Enabled:=false;
    OnClick:=optChange;
  end;

  optSkip:=TCheckBox.Create(Self);
  with optSkip do begin
    Parent:=TagArea;
    Caption:='';
    Height:=17;
    Left:=409;
    Top:=4;
    Width:=15;
    Checked:=FSkip;
    OnClick:=optChange;
  end;

  btnUp:=TButton.Create(Self);
  with btnUp do begin
    Parent:=TagArea;
    Caption:='UP';
    Height:=22;
    Left:=437;
    Top:=3;
    Width:=33;
    OnClick:=btnClick;
  end;

  btnDown:=TButton.Create(Self);
  with btnDown do begin
    Parent:=TagArea;
    Caption:='Down';
    Height:=22;
    Left:=470;
    Top:=3;
    Width:=33;
    OnClick:=btnClick;
  end;

  btnDel:=TButton.Create(Self);
  with btnDel do begin
    Parent:=TagArea;
    Caption:='Del';
    Height:=22;
    Left:=503;
    Top:=3;
    Width:=33;
    OnClick:=btnClick;
  end;

  btnBits:=TButton.Create(Self);
  with btnBits do begin
    Parent:=TagArea;
    Caption:='Bits';
    Height:=22;
    Left:=536;
    Top:=3;
    Width:=28;
    OnClick:=btnClick;
  end;
end;

destructor TS7TagItemEditor.Destroy;
var
  c:Integer;
begin
  for c:=GetBitCount-1 downto 0 do
    DelBit(c);
  BitList.Destroy;
  inherited Destroy;
end;

procedure TS7TagItemEditor.PopulateCombo;
begin
  with cmbItemType do begin
    Items.Clear;
    Items.Clear;
    Items.Add('pttDefault');
    Items.Add('pttShortInt');
    Items.Add('pttByte');
    Items.Add('pttSmallInt');
    Items.Add('pttWord');
    Items.Add('pttInteger');
    Items.Add('pttDWord');
    Items.Add('pttFloat');
    ItemIndex:=0;
  end;
end;

procedure TS7TagItemEditor.EnableTagType(ToEnable:Boolean);
begin
  cmbItemType.Enabled:=ToEnable;
end;

procedure TS7TagItemEditor.EnableScanRate(ToEnable:Boolean);
begin
  spinScan.Enabled:=ToEnable;
end;

procedure TS7TagItemEditor.EnableSwapBytes(ToEnable:Boolean);
begin
  optSwapBytes.Enabled:=ToEnable;
  optSwapBytes.Checked:=ToEnable;
end;

procedure TS7TagItemEditor.EnableSwapWords(ToEnable:Boolean);
begin
  optSwapWords.Enabled:=ToEnable;
  optSwapWords.Checked:=ToEnable;
end;

function TS7TagItemEditor.GetBitCount:Integer;
begin
  Result:=BitList.Count;
end;

function TS7TagItemEditor.GetBit(Index:Integer):TTagBitItemEditor;
begin
  Result:=TTagBitItemEditor(BitList.Items[index])
end;

function  TS7TagItemEditor.AddBit:TTagBitItemEditor;
var
  tb:TTagBitItemEditor;
begin
  tb:=TTagBitItemEditor.Create(Self);
  tb.Parent:=BitArea;
  tb.OnDelClick:=DelBitItem;
  tb.OnCheckNames:=CheckNames;
  tb.Top:=BitList.Count*tb.Height+1;
  BitList.Add(tb);
  Self.Height:=TagArea.Height+(tb.Height*BitList.Count)+3;
  Result:=tb;
end;

procedure TS7TagItemEditor.OnDelTimer(Sender:TObject);
var
  c, i:Integer;
begin
  for c:=DelList.Count-1 downto 0 do begin
    i:=GetIndex(TTagBitItemEditor(DelList.Items[c]));
    DelBit(i);
    DelList.Delete(c);
  end;
  DelTimer.Enabled:=false;

  if Assigned(FOnDelBitItem) then
    FOnDelBitItem(Self);
end;

procedure TS7TagItemEditor.DelBit(index:Integer);
var
  tb:TTagBitItemEditor;
begin
  tb:=TTagBitItemEditor(BitList.Items[index]);
  BitList.Remove(tb);
  self.Height:=TagArea.Height+(tb.Height*BitList.Count);
  tb.Destroy;
end;

function  TS7TagItemEditor.GetIndex(tbitEditor:TTagBitItemEditor):Integer;
begin
  Result:=BitList.IndexOf(tbitEditor);
end;

function  TS7TagItemEditor.AcceptName(TheName:String):Boolean;
var
  accept1, accept2 :Boolean;
begin
  accept1:=true;
  accept2:=true;

  //checa o nome com os bits...
  //check the new name with bit names.
  CheckNames(Self, TheName, accept1);

  //checa o nome com os demais itens...
  //check the new name with other struct item names.
  if accept1 and Assigned(FCheckNames) then
    FCheckNames(Self,TheName,accept2);

  //Ok caso passe nos dois testes...
  //Ok if everything is ok.
  Result:=accept1 and accept2;
end;

procedure TS7TagItemEditor.SetTagName(newName:String);
var
  accept:Boolean;
begin
  accept:=true;
  if Assigned(FCheckNames) then
    FCheckNames(Self,newname,accept);

  if accept then begin
    FTagName:=newname;
    edtItemName.Text:=newname;
    edtItemName.Modified:=false;
  end;
end;

procedure TS7TagItemEditor.SetTagType(newType:TTagType);
begin
  FTagType:=newType;
  case newType of
    pttDefault:
      cmbItemType.ItemIndex:=0;
    pttShortInt:
      cmbItemType.ItemIndex:=1;
    pttByte:
      cmbItemType.ItemIndex:=2;
    pttSmallInt:
      cmbItemType.ItemIndex:=3;
    pttWord:
      cmbItemType.ItemIndex:=4;
    pttInteger:
      cmbItemType.ItemIndex:=5;
    pttDWord:
      cmbItemType.ItemIndex:=6;
    pttFloat:
      cmbItemType.ItemIndex:=7;
  end;
  optChange(cmbItemType);
end;

procedure TS7TagItemEditor.SetTagScan(newScan:TRefreshTime);
begin
  FTagScan:=newScan;
  spinScan.Value:=FTagScan;
end;

procedure TS7TagItemEditor.SetSwapBytes(swap:Boolean);
begin
  FSwapBytes:=swap;
  optSwapBytes.Checked:=FSwapBytes;
end;

procedure TS7TagItemEditor.SetSwapWords(swap:Boolean);
begin
  FSwapWords:=swap;
  optSwapWords.Checked:=FSwapWords;
end;

procedure TS7TagItemEditor.SetSkipTag(Skip:Boolean);
begin
  FSkip:=Skip;
  optSkip.Checked:=FSkip;
  edtItemName.Enabled:=not FSkip;
end;

procedure TS7TagItemEditor.edtItemNameExit(Sender:TObject);
var
  accept1, accept2 :Boolean;
  b:Integer;
  oldname:String;
begin
  if not edtItemName.Modified then exit;

  accept1:=true;
  accept2:=true;

  //checa o nome com os bits...
  //check the new name with bits names.
  CheckNames(Sender,edtItemName.Text,accept1);

  //checa o nome com os demais itens...
  //check the new name with other struct names.
  if accept1 and Assigned(FCheckNames) then
    FCheckNames(Self,edtItemName.Text,accept2);

  //se pelo menos um falhou, volta o nome anterior...
  //to accept the new name, everything must be ok.
  if (accept1=true) and (accept2=true) then begin
    edtItemName.Modified:=false;
    oldname:=FTagName;
    FTagName:=edtItemName.Text;

    //atualiza nome dos bits
    //update the name of the bits.
    for b:=0 to BitCount-1 do
      TTagBitItemEditor(Bit[b]).TagName := StringReplace(TTagBitItemEditor(Bit[b]).TagName,oldname,FTagName,[rfReplaceAll, rfIgnoreCase]);
      
  end else begin
    edtItemName.Text:=FTagName;
    edtItemName.Modified:=false;
  end;
end;

procedure TS7TagItemEditor.DelBitItem(Sender:TObject);
begin
  if not (Sender is TTagBitItemEditor) then exit;

  if MessageDlg(SDeleteTheItem+(Sender as TTagBitItemEditor).TagName+'"?',mtConfirmation,[mbyes,mbNo],0)=mrno then exit;

  DelList.Add(Sender);
  DelTimer.Enabled:=true;
end;

procedure TS7TagItemEditor.optChange(Sender:TObject);
begin
  if Sender=optSwapBytes then
    FSwapBytes:=optSwapBytes.Checked;

  if Sender=optSwapWords then
    FSwapWords:=optSwapWords.Checked;

  if Sender=optSkip then begin
    FSkip:=optSkip.Checked;
    edtItemName.Enabled:=not FSkip;
    if Assigned(FOnSkipChange) then
      FOnSkipChange(optSkip);
  end;

  if Sender=spinScan then
    FTagScan:=spinScan.Value;

  if Sender=cmbItemType then begin
    case cmbItemType.ItemIndex of
      0:
        FTagType:=pttDefault;
      1:
        FTagType:=pttShortInt;
      2:
        FTagType:=pttByte;
      3:
        FTagType:=pttSmallInt;
      4:
        FTagType:=pttWord;
      5:
        FTagType:=pttInteger;
      6:
        FTagType:=pttDWord;
      7:
        FTagType:=pttFloat;
    end;
    case cmbItemType.ItemIndex of
      0,1,2: begin
        optSwapBytes.Checked:=false;
        optSwapBytes.Enabled:=false;
        optSwapWords.Checked:=false;
        optSwapWords.Enabled:=false;
      end;
      3,4: begin
        optSwapBytes.Checked:=True;
        optSwapBytes.Enabled:=True;
      end;
      5,6,7: begin
        optSwapBytes.Checked:=True;
        optSwapBytes.Enabled:=True;
        optSwapWords.Checked:=True;
        optSwapWords.Enabled:=True;
      end;
    end;
    if Assigned(FOnTypeChange) then
      FOnTypeChange(Self);
  end;
end;

//evento chamado pelos bits do tag para verificar seu nome...
//event called by bit itens to check theirs names.
procedure TS7TagItemEditor.CheckNames(Sender:TObject; NewName:String; var AcceptNewName:Boolean);
var
  b:Integer;
begin
  if (Sender<>Self) and (NewName=TagName) then begin
    AcceptNewName:=false
  end else begin
    for b:=0 to GetBitCount-1 do
      if (Sender<>Bit[b]) and (Bit[b].TagName=NewName) then begin
        AcceptNewName:=false ;
        exit;
      end;

    if Assigned(FCheckNames) then
      FCheckNames(sender,NewName, AcceptNewName);
  end;
end;

procedure TS7TagItemEditor.btnClick(Sender:TObject);
begin
  if (Sender=btnUp) and Assigned(FUpClickEvent) then
    FUpClickEvent(Self);

  if (Sender=btnDown) and Assigned(FDownClickEvent) then
    FDownClickEvent(Self);

  if (Sender=btnBits) and Assigned(FBitsClickEvent) then
    FBitsClickEvent(Self);

  //esta linha tem q ficar por ultimo sempre!!!
  //this condition must be the last ALWAYS!
  if (Sender=btnDel) and Assigned(FDelClickEvent) then
    FDelClickEvent(Self);
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


procedure TfrmS7TagBuilder.MemoryAreaClick(Sender: TObject);
begin
  lblDBNumber.Enabled:=false;
  spinDBNumber.Enabled:=false;
  BlockType.Enabled:=false;
  case MemoryArea.ItemIndex of
    0: begin
     lblStartAddress.Caption:=SDigitalInputInitialByte;
     BlockType.ItemIndex:=2;
    end;
    1: begin
     lblStartAddress.Caption:=SDigitalOutputInitialByte;
     BlockType.ItemIndex:=2;
    end;
    2: begin
     lblStartAddress.Caption:=SFlagInitialAddress;
     BlockType.Enabled:=optplcblock.Checked;
    end;
    3: begin
     lblStartAddress.Caption:=SInitialAddressInsideDB;
     lblDBNumber.Enabled:=true;
     spinDBNumber.Enabled:=true;
     BlockType.Enabled:=optplcblock.Checked;
    end;
    4,9: begin
     lblStartAddress.Caption:=SCounterInitialAddress;
     BlockType.ItemIndex:=4;
    end;
    5,10: begin
     lblStartAddress.Caption:=STimerInitialAddress;
     BlockType.ItemIndex:=4;
    end;
    6: begin
     lblStartAddress.Caption:=SSMInitialByte;
     BlockType.ItemIndex:=2;
    end;
    7: begin
     lblStartAddress.Caption:=SAIWInitialAddress;
     BlockType.ItemIndex:=4;
    end;
    8: begin
     lblStartAddress.Caption:=SAQWInitialAddress;
     BlockType.ItemIndex:=4;
    end;
    11: begin
     lblStartAddress.Caption:=SPIWInitialAddress;
     BlockType.ItemIndex:=4;
    end;
    12: begin
     lblStartAddress.Caption:=SVInitialAddress;
     BlockType.Enabled:=optplcblock.Checked;
    end;
  end;

  BlockTypeChange(Sender);
  UpdateStructItems;
end;

procedure TfrmS7TagBuilder.btnFinishClick(Sender: TObject);
begin
  if (TagList.Count=0) or (not AtLeastOneItemIsValid) then
    raise Exception.Create(SYouMustHaveAtLeastOneStructureItem);
  if Trim(BlockName.Text)='' then
    raise Exception.Create(SInvalidBlockName);
end;

procedure TfrmS7TagBuilder.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  TagList:=TList.Create;
  ItemsToDel:=TList.Create;
  FStructureModified:=false;
end;

procedure TfrmS7TagBuilder.FormShow(Sender: TObject);
begin
  MemoryAreaClick(Sender);
  optplcblockClick(Sender);
end;

procedure TfrmS7TagBuilder.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TfrmS7TagBuilder.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  //onclose query
end;

procedure TfrmS7TagBuilder.Button1Click(Sender: TObject);
var
  tag, lastitem:TS7TagItemEditor;
begin
  inc(FItemId);

  tag:=TS7TagItemEditor.Create(Self);
  tag.Parent := ScrollBox1;
  tag.PopulateCombo;
  tag.OnCheckNames:=CheckNames;
  tag.OnUpClick:=btnUpClick;
  tag.OnDownClickEvent:=btnDownClick;
  tag.OnDelClickEvent:=btnDelClick;
  tag.OnBitsClickEvent:=btnBitsClick;
  tag.OnTypeChange:=StructItemTypeChanged;
  tag.OnSkipChange:=SkipChanged;
  tag.OnDelBitItem:=BitItemDeleted;
  tag.TagScan:=1000;
  tag.TagType:=pttDefault;
  tag.SwapBytes:=false;
  tag.SwapWords:=false;
  if TagList.Count>0 then begin
    lastitem:=TS7TagItemEditor(taglist.Items[TagList.Count-1]);
    tag.Top:=lastitem.Top+lastitem.Height;
  end else begin
    tag.Top:=0;
  end;

  tag.EnableScanRate(optplctagnumber.Checked);
  case MemoryArea.ItemIndex of
    0,1,6: begin
      tag.TagType:=pttByte;
    end;
    4,5,7..11: begin
      tag.TagType:=pttWord;
    end;
    2,3,12: begin
      //does nothing...
    end;
  end;
  
  tag.EnableTagType((not optplcblock.Checked) and (MemoryArea.ItemIndex in [2,3,12]));

  TagList.Add(tag);

  while not tag.AcceptName('StructItem'+IntToStr(FItemId)) do
    inc(FItemId);
  tag.TagName:='StructItem'+IntToStr(FItemId);
  UpdateStatusAndBlockName;
  FStructureModified:=true;
end;

procedure TfrmS7TagBuilder.btnUpClick(Sender: TObject);
var
  idx:Integer;
  priortop, actualTop:Integer;
  prior:TS7TagItemEditor;
begin
  if not (Sender is TS7TagItemEditor) then exit;

  idx := TagList.IndexOf(Sender);
  if idx>0 then begin
    FStructureModified:=true;
    prior:=TS7TagItemEditor(TagList.Items[idx-1]);

    priortop:=prior.Top;
    actualTop:=(Sender as TS7TagItemEditor).Top;

    TagList.Exchange(idx-1, idx);

    (Sender as TS7TagItemEditor).Top:=priortop;
    (Sender as TS7TagItemEditor).TabOrder:=prior.TabOrder;
    prior.Top:=actualTop;
  end;
  UpdateStatusAndBlockName;
end;

procedure TfrmS7TagBuilder.btnDownClick(Sender: TObject);
var
  idx:Integer;
  nexttop, actualTop:Integer;
  next:TS7TagItemEditor;
begin
  if not (Sender is TS7TagItemEditor) then exit;

  idx := TagList.IndexOf(Sender);
  if (idx<>-1) and (idx<(TagList.Count-1)) then begin
    FStructureModified:=true;  
    next:=TS7TagItemEditor(TagList.Items[idx+1]);

    nexttop:=next.Top;
    actualTop:=(Sender as TS7TagItemEditor).Top;

    TagList.Exchange(idx+1, idx);

    (Sender as TS7TagItemEditor).Top:=nexttop;
    next.TabOrder:=(Sender as TS7TagItemEditor).TabOrder;
    next.Top:=actualTop;
  end;
  UpdateStatusAndBlockName;
end;

procedure TfrmS7TagBuilder.btnDelClick(Sender: TObject);
begin
  if ItemsToDel.IndexOf(Sender)=-1 then
    if MessageDlg(SRemoveaStructItemCalled+TS7TagItemEditor(Sender).TagName+'"?', mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
      FStructureModified:=true;
      ItemsToDel.Add(Sender);
      Timer1.Enabled:=true;
    end;
end;

procedure TfrmS7TagBuilder.btnBitsClick(Sender: TObject);
var
  frmbit:TfrmBitMapper;
  ti:TTagBitItemEditor;
  s7tageditor:TS7TagItemEditor;
  bitnum,
  bytenum,
  wordnum,
  startbit,
  endbit,
  curbit:Integer;

  procedure updatenumbers;
  begin
    bitnum:=curbit;
    if frmbit.bitnamestartsfrom1.Checked then inc(bitnum);

    bytenum:=curbit div 8;
    if frmbit.bytenamestartsfrom1.Checked then inc(bytenum);

    wordnum:=curbit div 16;
    if frmbit.Wordnamestartsfrom1.Checked then inc(wordnum);
  end;

  function GetNewTagBitName:String;
  var
    n:String;
  begin
    n:=IntToStr(bitnum);
    Result:=frmbit.edtNamepattern.Text;
    Result := StringReplace(Result,'%b',n,[rfReplaceAll]);

    n:=IntToStr(bytenum);
    Result := StringReplace(Result,'%B',n,[rfReplaceAll]);

    n:=IntToStr(wordnum);
    Result := StringReplace(Result,'%w',n,[rfReplaceAll]);

    n:=(Sender as TS7TagItemEditor).TagName;
    Result := StringReplace(Result,'%t',n,[rfReplaceAll]);
  end;
begin
  if not (Sender is TS7TagItemEditor) then exit;

  s7tageditor := (Sender as TS7TagItemEditor);

  frmbit:=TfrmBitMapper.Create(Self);
  try
    if frmbit.ShowModal=mrOk then begin
      FStructureModified:=true;
      startbit:=31-frmbit.StringGrid1.Selection.Right;
      endbit:=31-frmbit.StringGrid1.Selection.Left;
      curbit:=startbit;
      if frmbit.eachbitastag.Checked then begin
        while curbit<=endbit do begin
          updatenumbers;
          ti:=s7tageditor.AddBit;
          ti.TagName:=GetNewTagBitName;
          ti.EndBit:=curbit;
          ti.StartBit:=curbit;
          inc(curbit);
        end;
      end else begin
        updatenumbers;
        ti:=s7tageditor.AddBit;
        ti.TagName:=GetNewTagBitName;
        ti.EndBit:=endbit;
        ti.StartBit:=startbit;
      end;
    end;
  finally
    frmbit.Destroy;
  end;
end;

procedure TfrmS7TagBuilder.SkipChanged(Sender:TObject);
begin
  FStructureModified:=true;
  UpdateStatusAndBlockName;
end;

procedure TfrmS7TagBuilder.StructItemTypeChanged(Sender:TObject);
begin
  if MemoryArea.ItemIndex in [2,3,12] then begin
    if not FStructureModified then begin
      UpdateFlagDBandVStrucItemName;
      FStructureModified:=false;
    end;
    UpdateStatusAndBlockName;
  end;
end;

destructor TfrmS7TagBuilder.Destroy;
var
  t,b:Integer;
begin
  for t:=TagList.Count-1 downto 0 do begin
    for b:=TS7TagItemEditor(TagList.Items[t]).BitCount-1 downto 0 do begin
      TS7TagItemEditor(TagList.Items[t]).DelBit(b);
    end;
    TS7TagItemEditor(TagList.Items[t]).Destroy;
    TagList.Delete(t);
  end;
  TagList.Destroy;
  ItemsToDel.Destroy;
  inherited Destroy;
end;

function TfrmS7TagBuilder.GetTagType:Integer;
begin
{
0  Digital Inputs, S7 200/300/400/1200        Inputs, Entradas)        @cell( 1
1  Digital Outputs, S7 200/300/400/1200       Outputs, Saidas)         @cell( 2
2  Flags, M's, S7 200/300/400/1200            Flags and M's)           @cell( 3
3  DB's, S7 300/400/1200                      DB and VM no S7-200 )    @cell( 4
4  Counter, S7 300/400/1200                   Counter, S7 300/400)     @cell( 5
5  Timer, S7 300/400/1200                     Timer, S7 300/400)       @cell( 6
6  Special Memory, SM, S7-200                 SM, S7-200)              @cell( 7
7  Analog Input, S7-200                       AIW, S7-200)             @cell( 8
8  Analog output, S7-200                      AQW, S7-200)             @cell( 9
9  Counter, S7-200                            Counter, S7-200)         @cell(10
10 Timer, S7-200                              Timer, S7-200)           @cell(11
11 Analog Input (PIW), S7-300/400/1200        PIW, S7 300/400)         @cell(12
12 VB, VW, VD, S7-200
}
  Result:=0;
  if MemoryArea.ItemIndex in [0..11] then
    Result:=MemoryArea.ItemIndex+1
  else begin
    if MemoryArea.ItemIndex=12 then Result:=4;
  end;
end;

procedure TfrmS7TagBuilder.CheckNames(Sender:TObject; NewName:String; var AcceptNewName:Boolean);
var
  t,b:Integer;
begin
  for t:=0 to TagList.Count-1 do begin
    if TagList.Items[t]=Sender then continue;
    if TS7TagItemEditor(TagList.Items[t]).TagName=NewName then begin
      AcceptNewName:=false;
      exit;
    end;
    for b:=0 to TS7TagItemEditor(TagList.Items[t]).BitCount-1 do begin
      if TS7TagItemEditor(TagList.Items[t]).Bit[b]=Sender then continue;
      if TTagBitItemEditor(TS7TagItemEditor(TagList.Items[t]).Bit[b]).TagName=NewName then begin
        AcceptNewName:=false;
        exit;
      end;
    end;
  end;
  FStructureModified:=true;
end;

function TfrmS7TagBuilder.GetStructItemsCount:Integer;
begin
  Result:=TagList.Count;
end;

function TfrmS7TagBuilder.GetStructItem(index:Integer):TS7TagItemEditor;
begin
  Result:=TS7TagItemEditor(TagList.Items[index]);
end;

function TfrmS7TagBuilder.GetStructureSizeInBytes:Integer;
var
  typesize, curitem:Integer;
begin
  if optplcblock.Checked then begin
    case BlockType.ItemIndex of
      3..4: begin
        typesize:=2;
      end;
      5..7: begin
        typesize:=4;
      end;
      else begin
        typesize:=1;
      end;
    end;
    Result:=TagList.Count*typesize
  end else begin
    Result:=0;
    for curitem:=0 to TagList.Count-1 do begin
      with TS7TagItemEditor(TagList.Items[curitem]) do begin
        case TagType of
          pttSmallInt, pttWord:
            typesize:=2;
          pttInteger, pttDWord, pttFloat:
            typesize:=4;
          else
            typesize:=1
        end;
      end;
      Inc(Result,typesize);
    end;
  end;
end;

function TfrmS7TagBuilder.GetRealStartOffset:Integer;
var
  curitem:Integer;
  curTagType:TTagType;
begin
  if AtLeastOneItemIsValid then begin
  
    if MemoryArea.ItemIndex in [4,5,9,10] then
      Result:=spinStartAddress.Value*2
    else
      Result:=spinStartAddress.Value;

    for curitem:=0 to TagList.Count-1 do
      with TS7TagItemEditor(TagList.Items[curitem]) do begin
        if optplcblock.Checked then begin
          curTagType := CurBlockType;
        end else
          curTagType := TagType;

        if not SkipTag then
          break
        else begin
          case curTagType of
            pttDefault, pttShortInt, pttByte:
              inc(Result, 1);
            pttSmallInt, pttWord:
              inc(Result, 2);
            pttInteger, pttDWord, pttFloat:
              inc(Result, 4);
          end;
        end;
      end;
  end else
    Result:=-1;
end;

function TfrmS7TagBuilder.GetRealEndOffset:Integer;
var
  curitem:Integer;
  curTagType:TTagType;
begin
  if AtLeastOneItemIsValid then begin
    Result:=EndOffset;
    for curitem:=TagList.Count-1 downto 0 do
      with TS7TagItemEditor(TagList.Items[curitem]) do begin

        if optplcblock.Checked then begin
          curTagType := CurBlockType;
        end else
          curTagType := TagType;

        if not SkipTag then
          break
        else begin
          case curTagType of
            pttDefault, pttShortInt, pttByte:
              Dec(Result, 1);
            pttSmallInt, pttWord:
              Dec(Result, 2);
            pttInteger, pttDWord, pttFloat:
              Dec(Result, 4);
          end;
        end;
      end;
  end else
    Result:=-1;
end;

function TfrmS7TagBuilder.GetTheLastItemOffset:Integer;
var
  curitem:Integer;
  curTagType:TTagType;
begin
  if AtLeastOneItemIsValid then begin
    Result:=EndOffset;
    for curitem:=TagList.Count-1 downto 0 do
      with TS7TagItemEditor(TagList.Items[curitem]) do begin

        if optplcblock.Checked then begin
          curTagType := CurBlockType;
        end else
          curTagType := TagType;

        case curTagType of
          pttSmallInt, pttWord:
            Dec(Result, 1);
          pttInteger, pttDWord, pttFloat:
            Dec(Result, 3);
        end;
        if not SkipTag then
          break;
      end;
  end else
    Result:=-1;
end;

function TfrmS7TagBuilder.GetStartOffset:Integer;
begin
  if MemoryArea.ItemIndex in [4,5,9,10] then
    Result:=(spinStartAddress.Value*2)
  else
    Result:=spinStartAddress.Value;
end;

function TfrmS7TagBuilder.GetEndOffset:Integer;
begin
  if MemoryArea.ItemIndex in [4,5,9,10] then
    Result:=(spinStartAddress.Value*2)+(spinNumItens.Value*StructureSizeInBytes)-1
  else
    Result:=spinStartAddress.Value+(spinNumItens.Value*StructureSizeInBytes)-1;
end;

function TfrmS7TagBuilder.AtLeastOneItemIsValid:Boolean;
var
  curitem:Integer;
begin
  Result:=false;
  for curitem:=0 to TagList.Count-1 do
    with TS7TagItemEditor(TagList.Items[curitem]) do
      if not SkipTag then begin
        Result:=true;
        break;
      end;
end;

function TfrmS7TagBuilder.CurBlockType:TTagType;
begin
  case BlockType.ItemIndex of
    1:
      Result:=pttShortInt;
    2:
      Result:=pttByte;
    3:
      Result:=pttSmallInt;
    4:
      Result:=pttWord;
    5:
      Result:=pttInteger;
    6:
      Result:=pttDWord;
    7:
      Result:=pttFloat;
    else
      Result:=pttDefault;
  end;
end;

procedure TfrmS7TagBuilder.BitItemDeleted(Sender:TObject);
begin
  FStructureModified:=true;
end;

procedure TfrmS7TagBuilder.UpdateFlagDBandVStrucItemName;
var
  nome, nome2:String;
  curtype:TTagType;
begin
  if TagList.Count<=0 then exit;
   
  if MemoryArea.ItemIndex=2 then
    nome:='M%s'
  else begin
    if MemoryArea.ItemIndex=3 then
      nome:='DB%d_DB%s'
    else
      nome:='V%s'
  end;

  if optplcblock.Checked then
    curtype:=CurBlockType
  else
    curtype:=TS7TagItemEditor(TagList.Items[0]).TagType;

  case curtype of
    pttDefault, pttShortInt, pttByte:
      nome2:='B';
    pttSmallInt, pttWord:
      nome2:='W';
    pttInteger, pttDWord, pttFloat:
      nome2:='D';
  end;

  if MemoryArea.ItemIndex=3 then
    nome:=Format(nome,[spinDBNumber.Value,nome2])
  else
    nome:=Format(nome,[nome2])+'%a';

  with TS7TagItemEditor(TagList.Items[0]) do begin
    TagName:=nome;
  end;
end;

procedure TfrmS7TagBuilder.Timer1Timer(Sender: TObject);
var
  c:Integer;
begin
  for c:=ItemsToDel.Count-1 downto 0 do begin
    TagList.Remove(ItemsToDel.Items[c]);
    TS7TagItemEditor(ItemsToDel.Items[c]).Destroy;
    ItemsToDel.Delete(c);
  end;
  Timer1.Enabled:=false;
  UpdateStatusAndBlockName;
  FStructureModified:=(TagList.Count<>0);
end;

procedure TfrmS7TagBuilder.TabSheet1Show(Sender: TObject);
begin
  btnBack.Enabled:=false;
  btnFinish.Enabled:=false;
  btnNext.Enabled:=true;
end;

procedure TfrmS7TagBuilder.BlockTypeChange(Sender: TObject);
begin
  if BlockType.ItemIndex in [0..2] then begin
    BlockSwapBytes.Checked:=false;
    BlockSwapWords.Checked:=false;
    BlockSwapBytes.Enabled:=false;
    BlockSwapWords.Enabled:=false;
  end;
  if BlockType.ItemIndex in [3..4] then begin
    BlockSwapBytes.Checked:=optplcblock.Checked;
    BlockSwapWords.Checked:=false;
    BlockSwapBytes.Enabled:=optplcblock.Checked;
    BlockSwapWords.Enabled:=false;
  end;
  if BlockType.ItemIndex in [5..7] then begin
    BlockSwapBytes.Checked:=optplcblock.Checked;
    BlockSwapWords.Checked:=optplcblock.Checked;
    BlockSwapBytes.Enabled:=optplcblock.Checked;
    BlockSwapWords.Enabled:=optplcblock.Checked;
  end;
  UpdateStatusAndBlockName;
end;

procedure TfrmS7TagBuilder.UpdateStructItems;
var
  c:Integer;
  toenablescan, toenabletype, toenableSwap:Boolean;
begin
  toenablescan := optplctagnumber.Checked;
  toenabletype:=(not optplcblock.Checked) and (MemoryArea.ItemIndex in [2,3,12]);
  toenableSwap:=(not optplcblock.Checked);

  for c:=0 to TagList.Count-1 do
    with TS7TagItemEditor(TagList.Items[c]) do begin
      EnableScanRate(toenablescan);
      EnableSwapBytes(toenableSwap);
      EnableSwapWords(toenableSwap);      
      case MemoryArea.ItemIndex of
        0,1,6: begin
          TagType:=pttByte;
        end;
        4,5,7..11: begin
          TagType:=pttWord;
        end;
      end;
      EnableTagType(toenabletype);
    end;
end;


procedure TfrmS7TagBuilder.btnNextClick(Sender: TObject);
var
  curitem:Integer;
  nome,nome2:String;
begin
  if (FStructureModified=false) and
     (MessageDlg('Do you want initialize the structure?', mtConfirmation, [mbYes,mbNo],0)=mrYes) then begin

    for curitem:=TagList.Count-1 downto 0 do
      TS7TagItemEditor(TagList.Items[curitem]).Destroy;

    TagList.Clear;

    Button1Click(Sender);

    case MemoryArea.ItemIndex of
      0, 1: begin
        if MemoryArea.ItemIndex=0 then begin
          nome:='IB%a';
          nome2:='I%a_';
        end else begin
          nome:='QB%a';
          nome2:='Q%a_';
        end;

        with TS7TagItemEditor(TagList.Items[0]) do begin
          TagName:=nome;
          for curitem:=0 to 7 do
            with AddBit do begin
              TagName:=nome2+IntToStr(curitem);
              StartBit:=curitem;
              EndBit:=curitem;
            end;
        end;
      end;
      2,3,12:
        UpdateFlagDBandVStrucItemName;

      4,9, 5,10: begin
        if MemoryArea.ItemIndex in [4,9] then
          nome:='C%a'
        else
          nome:='T%a';

        with TS7TagItemEditor(TagList.Items[0]) do begin
          TagName:=nome;
        end;
      end;
      6: begin
        with TS7TagItemEditor(TagList.Items[0]) do begin
          TagName:='SMB%a';
        end;
      end;
      7,8,11: begin
        if MemoryArea.ItemIndex=7 then
          nome:='AIW%a'
        else begin
          if MemoryArea.ItemIndex=8 then
            nome:='AQW%a'
          else
            nome:='PIW%a';
        end;

        with TS7TagItemEditor(TagList.Items[0]) do begin
          TagName:=nome;
        end;
      end;
    end;

    FStructureModified:=false;

  end;
  PageControl1.ActivePage:=TabSheet4;
end;

procedure TfrmS7TagBuilder.btnBackClick(Sender: TObject);
begin
  PageControl1.ActivePage:=TabSheet1;
end;

procedure TfrmS7TagBuilder.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  OldPage:=PageControl1.ActivePage;
end;

procedure TfrmS7TagBuilder.TabSheet4Show(Sender: TObject);
begin
  btnBack.Enabled:=true;
  btnFinish.Enabled:=true;
  btnNext.Enabled:=false;
end;

procedure TfrmS7TagBuilder.optplcblockClick(Sender: TObject);
begin
  lblBlockType.Enabled   :=optplcblock.Checked;
  BlockType.Enabled      :=optplcblock.Checked;
  lblBlockScan.Enabled   :=optplcblock.Checked;
  BlockScan.Enabled      :=optplcblock.Checked;
  BlockSwapBytes.Enabled :=optplcblock.Checked;
  BlockSwapWords.Enabled :=optplcblock.Checked;

  lblStructScan.Enabled:=optplcStruct.Checked;
  StructScan.Enabled:=optplcStruct.Checked;

  lblBlockName.Enabled:=optplcStruct.Checked or optplcblock.Checked;
  BlockName.Enabled:=optplcStruct.Checked or optplcblock.Checked;

  MemoryAreaClick(Sender);
end;

procedure TfrmS7TagBuilder.UpdateStatusAndBlockName;
var
  strblockname, starttype, endtype:String;
  curitem:Integer;
begin
  if BlockName.Modified then exit;
  case MemoryArea.ItemIndex of
    0: begin
     strblockname:='InputBytes_From_IB%d_to_IB%d';
    end;
    1: begin
     strblockname:='OutputBytes_From_QB%d_to_QB%d';
    end;
    2: begin
     strblockname:='Flags_From_M%s%d_to_M%s%d';
    end;
    3: begin
     strblockname:='DB%d_From_DB%s%d_to_DB%s%d'
    end;
    4,9: begin
     strblockname:='Counters_From_C%d_to_C%d'
    end;
    5,10: begin
     strblockname:='Timers_From_T%d_to_T%d'
    end;
    6: begin
     strblockname:='SM_From_SMB%d_to_SMB%d'
    end;
    7: begin
     strblockname:='AnalogInput_From_AIW%d_to_AIW%d';
    end;
    8: begin
     strblockname:='AnalogOutput_From_AQW%d_to_AQW%d';
    end;
    11: begin
     strblockname:='AnalogIW_From_PIW%d_to_PIW%d';
    end;
    12: begin
     strblockname:='Vs_From_V%s%d_to_V%s%d';
    end;
  end;

  if optplcblock.Checked then begin
    case BlockType.ItemIndex of
      0..2: begin
        starttype:='B';
      end;
      3..4: begin
        starttype:='W';
      end;
      5..7: begin
        starttype:='D';
      end;
    end;
    endtype:=starttype;
  end else begin
    starttype:='';
    endtype:='';
    if AtLeastOneItemIsValid then
      for curitem:=0 to TagList.Count-1 do begin
        with TS7TagItemEditor(TagList.Items[curitem]) do
          if (starttype='') and (not SkipTag) then begin
            case TagType of
              pttDefault, pttShortInt, pttByte:
                starttype:='B';
              pttSmallInt, pttWord:
                starttype:='W';
              pttInteger, pttDWord, pttFloat:
                starttype:='D';
            end;
          end;

        with TS7TagItemEditor(TagList.Items[(TagList.Count-1)-curitem]) do
          if (endtype='') and (not SkipTag) then begin
            case TagType of
              pttDefault, pttShortInt, pttByte:
                endtype:='B';
              pttSmallInt, pttWord:
                endtype:='W';
              pttInteger, pttDWord, pttFloat:
                endtype:='D';
            end;
          end;
      end;
  end;

  case MemoryArea.ItemIndex of
    2,12:
      BlockName.Text:=Format(strblockname,[starttype,GetRealStartOffset,endtype,GetTheLastItemOffset]);
    3:
      BlockName.Text:=Format(strblockname,[spinDBNumber.Value, starttype,GetRealStartOffset,endtype,GetTheLastItemOffset]);
    4,5,9,10:
      BlockName.Text:=Format(strblockname,[GetRealStartOffset div 2, GetTheLastItemOffset div 2]);
    else
      BlockName.Text:=Format(strblockname,[GetRealStartOffset, GetTheLastItemOffset]);
  end;
  BlockName.Modified:=false;
end;

procedure TfrmS7TagBuilder.spinStartAddressChange(Sender: TObject);
begin
  UpdateStatusAndBlockName;
end;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20400) }
initialization
  {$i us7tagbuilder.lrs}
  {$IFEND}
{$ENDIF}

end.