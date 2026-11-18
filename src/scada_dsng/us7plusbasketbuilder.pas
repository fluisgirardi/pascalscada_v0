{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Tag Builder do S7CommPlus com cesta: a árvore só navega, o que será criado fica na cesta.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Como usar: leia as variáveis do CLP, selecione na árvore (Shift/Ctrl) o que quer importar e
  clique em @italic(Adicionar à cesta), dê dois cliques ou arraste para a cesta. Cada entrada
  da cesta tem o próprio nome, scan time e, para blocos e estruturas, a opção de mapear ou não
  os itens filhos. O que a entrada vira depende só do que foi arrastado (ver S7PlusBasket):
  @unorderedList(
    @item(variável simples: TPLCTagNumber (TPLCString para STRING);)
    @item(array de variáveis simples: TPLCBlock;)
    @item(estrutura, array de estruturas ou DB: TPLCStruct.)
  )
  Com os filhos mapeados, cada BOOL vira um TTagBit sobre um item (ou elemento) de byte.

  O formulário é montado em código (não há .lfm). A cesta é uma lista virtual: aguenta milhares
  de entradas. O botão @italic(Assistente clássico) abre o assistente com marcas por modo.
}
{$ELSE}
{:
  @abstract(S7CommPlus Tag Builder with a basket: the tree only browses, what is created lives in the basket.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  How to use it: read the variables from the PLC, select on the tree (Shift/Ctrl) what you want
  to import and click @italic(Add to the basket), double click or drag into the basket. Each
  basket entry has its own name, scan time and, for blocks and structures, the option of mapping
  the child items or not. What an entry becomes depends only on what was dragged (see S7PlusBasket):
  @unorderedList(
    @item(a simple variable: TPLCTagNumber (TPLCString for a STRING);)
    @item(an array of simple variables: TPLCBlock;)
    @item(a structure, an array of structures or a DB: TPLCStruct.)
  )
  With the children mapped, each BOOL becomes a TTagBit over a byte item (or element).

  The form is built in code (there is no .lfm). The basket is a virtual list: it handles
  thousands of entries. The @italic(Classic wizard) button opens the wizard with ticks per mode.
}
{$ENDIF}
unit us7plusbasketbuilder;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Types, Contnrs, Math, Graphics, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, Masks, Spin, Tag, ProtocolTypes, S7PlusTypes, S7PlusConnection, S7PlusTypeInfo,
  S7PlusTagPlan, S7PlusBasket, S7PlusFamily;

type

  { TS7PlusBasketNodeInfo }

  //what a tree node stands for: a variable (Index>=0), the root of a DB or area (Block>=0) or
  //just a level of the path
  TS7PlusBasketNodeInfo = class
  public
    Index:LongInt;
    Block:LongInt;
    Key:String;
  end;

  //a DB or an area of the first level: its variables are only read when it is opened
  TS7PlusBasketBlock = record
    Key:String;
    IsArea:Boolean;
    Number:Cardinal;
    DBName:String;
    AreaRID, AreaTiRid:Cardinal;
    AreaName:String;
    AccessArea:Cardinal;
    Loaded:Boolean;
    First, Count:LongInt;
    Cands:TS7PlusCandidateArray;
    SizeStr:String;
  end;

  { TfrmS7PlusBasketBuilder }

  TfrmS7PlusBasketBuilder = class(TForm)
  private
    //top
    pnlTop:TPanel;
    btnBrowse, btnExpandAll, btnCollapseAll, btnClassic:TButton;
    chkSymbolicNames, chkIncludeAreas, chkShowSkipped, chkShowElements:TCheckBox;
    lblStatus, lblFilter:TLabel;
    edtFilter:TEdit;
    //tree
    pnlTree:TPanel;
    tvTags:TTreeView;
    lblTreeHint:TLabel;
    splitter:TSplitter;
    popTree:TPopupMenu;
    //basket side
    pnlRight, pnlDefaults, pnlEdit:TPanel;
    lblDefaults, lblPrefix, lblScan, lblTypes, lblEditName, lblEditScan, lblEditPrefix, lblTemplate, lblBasketInfo:TLabel;
    edtPrefix, edtName, edtTypes, edtEditPrefix, edtTemplate:TEdit;
    spinScan, spinEditScan:TSpinEdit;
    chkMapChildren, chkEditMap:TCheckBox;
    btnAdd, btnAddBelow, btnRemove, btnClear, btnApplyTemplate:TButton;
    lvBasket:TListView;
    tmrSel:TTimer;
    //bottom
    pnlBottom:TPanel;
    lblProblems:TLabel;
    pbProgress:TProgressBar;
    btnCreate, btnCancel:TButton;

    FDriver:TS7CommPlusDriver;
    FCandidates:TS7PlusCandidateArray;
    FBlocks:array of TS7PlusBasketBlock;
    FInfos:TObjectList;
    FSymbolic:Boolean;
    FLoading:Boolean;
    FDriverLog:TStringList;
    FSkippedCount:LongInt;

    FEntries:TS7PlusBasketEntries;     //capacity: Length(FEntries); the basket holds FCount
    FKindText:TStringList;             //what each entry becomes, as text (TPLCStruct...)
    FProblem:array of String;          //per entry: a name problem, or ''
    FCount:LongInt;
    FProblemCount:LongInt;
    FSel:TIntegerDynArray;             //the entries selected on the basket
    FUpdating:Boolean;
    FPlan:TS7PlusImportItems;

    procedure BuildUI;
    procedure FormResize(Sender:TObject);
    procedure DriverDebug(Sender:TObject; const Msg:String);
    function  WaitForConnection(aTimeoutMs:LongInt):Boolean;
    function  InfoOf(aNode:TTreeNode):TS7PlusBasketNodeInfo;
    function  IsPending(aNode:TTreeNode):Boolean;
    function  AreaLabel(const aArea:String):String;
    function  SizeText(const aCands:TS7PlusCandidateArray; const aPath:String; aIsDBRoot:Boolean):String;
    function  RootText(aBlock:LongInt):String;
    function  Decoration(aIndex:LongInt):String;
    function  ShowCandidate(aIndex:LongInt):Boolean;
    function  PassFilter(aIndex:LongInt):Boolean;
    procedure RebuildTree;
    procedure BuildBlock(aBlock:LongInt; aRoot:TTreeNode);
    function  LoadBlock(aBlock:LongInt; aRoot:TTreeNode):Boolean;
    procedure UpdateStatus;
    procedure SaveExpansion(aExpanded:TStringList);
    procedure RestoreExpansion(aExpanded:TStringList; aExpandAll:Boolean);

    procedure btnBrowseClick(Sender:TObject);
    procedure btnExpandAllClick(Sender:TObject);
    procedure btnCollapseAllClick(Sender:TObject);
    procedure FilterChanged(Sender:TObject);
    procedure tvTagsExpanding(Sender:TObject; Node:TTreeNode; var AllowExpansion:Boolean);
    procedure tvTagsDblClick(Sender:TObject);
    procedure AddClick(Sender:TObject);
    procedure AddBelowClick(Sender:TObject);
    procedure BasketDragOver(Sender, Source:TObject; X, Y:Integer; State:TDragState; var Accept:Boolean);
    procedure BasketDragDrop(Sender, Source:TObject; X, Y:Integer);
    procedure BasketData(Sender:TObject; Item:TListItem);
    procedure BasketSelectItem(Sender:TObject; Item:TListItem; Selected:Boolean);
    procedure BasketKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);
    procedure SelTimer(Sender:TObject);
    procedure EditNameChange(Sender:TObject);
    procedure EditScanChange(Sender:TObject);
    procedure EditPrefixChange(Sender:TObject);
    procedure ApplyTemplateClick(Sender:TObject);
    procedure EditMapClick(Sender:TObject);
    procedure RemoveClick(Sender:TObject);
    procedure ClearClick(Sender:TObject);
    procedure CreateClick(Sender:TObject);

    //the basket
    function  EntryName(i:LongInt):String;
    function  EntryKindText(const E:TS7PlusBasketEntry):String;
    function  FindEntry(const aPath:String; aIsDB:Boolean):LongInt;
    procedure PushEntry(const E:TS7PlusBasketEntry);
    function  NewEntryOfNode(aNode:TTreeNode; out E:TS7PlusBasketEntry):Boolean;
    procedure AddNodes(aBelow:Boolean);
    procedure AddBelowNode(aNode:TTreeNode; var aAdded, aSkipped:LongInt);
    procedure FinishAdd(aAdded, aSkipped:LongInt);
    procedure RefreshBasket;
    procedure RefreshProblems;
    procedure ShowSelection;
    procedure CollectSelection;
    procedure RemoveEntries(const aRemove:array of Boolean);
    function  BuildPlan:Boolean;
  public
    constructor Create(TheOwner:TComponent); override;
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    //: Driver de protocolo de onde as variáveis serão lidas.
    {$ELSE}
    //: Protocol driver from where the variables will be read.
    {$ENDIF}
    property Driver:TS7CommPlusDriver read FDriver write FDriver;

    {$IFDEF PORTUGUES}
    //: A lista de componentes a criar (depois de ShowModal=mrOK), um item por entrada da cesta.
    {$ELSE}
    //: The list of components to create (after ShowModal=mrOK), one item per basket entry.
    {$ENDIF}
    function GetImportPlan:TS7PlusImportItems;

    {$IFDEF PORTUGUES}
    //: Carrega no assistente variáveis já lidas do CLP (sem passar pelo driver): cada DB (ou área) delas vale como lido.
    {$ELSE}
    //: Loads on the wizard variables already read from the PLC (bypassing the driver): every DB (or area) on them counts as read.
    {$ENDIF}
    procedure LoadCandidates(const aCandidates:TS7PlusCandidateArray);
  end;

implementation

uses hsstrings;

{ TfrmS7PlusBasketBuilder }

constructor TfrmS7PlusBasketBuilder.Create(TheOwner:TComponent);
begin
  inherited CreateNew(TheOwner, 0);
  FInfos := TObjectList.Create(true);
  FDriverLog := TStringList.Create;
  FKindText := TStringList.Create;
  FCount := 0;
  BuildUI;
end;

destructor TfrmS7PlusBasketBuilder.Destroy;
begin
  FKindText.Free;
  FDriverLog.Free;
  FInfos.Free;
  inherited Destroy;
end;

procedure TfrmS7PlusBasketBuilder.BuildUI;

  function MakeButton(aParent:TWinControl; const aCaption:String; aLeft, aTop, aWidth:Integer;
    aClick:TNotifyEvent):TButton;
  begin
    Result := TButton.Create(Self);
    Result.Parent := aParent;
    Result.SetBounds(aLeft, aTop, aWidth, 27);
    Result.Caption := aCaption;
    Result.OnClick := aClick;
  end;

  function MakeCheck(aParent:TWinControl; const aCaption:String; aLeft, aTop, aWidth:Integer;
    aChecked:Boolean):TCheckBox;
  begin
    Result := TCheckBox.Create(Self);
    Result.Parent := aParent;
    Result.SetBounds(aLeft, aTop, aWidth, 21);
    Result.Caption := aCaption;
    Result.Checked := aChecked;
  end;

  function MakeLabel(aParent:TWinControl; const aCaption:String; aLeft, aTop:Integer):TLabel;
  begin
    Result := TLabel.Create(Self);
    Result.Parent := aParent;
    Result.SetBounds(aLeft, aTop, 100, 17);
    Result.Caption := aCaption;
  end;

  function MakePanel(aParent:TWinControl; aAlign:TAlign; aSize:Integer):TPanel;
  begin
    Result := TPanel.Create(Self);
    Result.Parent := aParent;
    Result.Align := aAlign;
    Result.BevelOuter := bvNone;
    Result.Caption := '';
    if aAlign in [alTop, alBottom] then Result.Height := aSize
    else if aAlign in [alLeft, alRight] then Result.Width := aSize;
  end;

  procedure AddColumn(const aCaption:String; aWidth:Integer);
  begin
    with lvBasket.Columns.Add do begin
      Caption := aCaption;
      Width := aWidth;
    end;
  end;

var
  mi:TMenuItem;
begin
  Caption := SS7PBKCaption;
  Width := 1100;
  Height := 800;
  Constraints.MinWidth := 900;
  Constraints.MinHeight := 640;
  Position := poScreenCenter;
  BorderIcons := [biSystemMenu, biMaximize];

  //----- bottom
  pnlBottom := MakePanel(Self, alBottom, 44);
  btnCancel := MakeButton(pnlBottom, SS7PTBCancel, 0, 8, 100, nil);
  btnCancel.ModalResult := mrCancel;
  btnCancel.Cancel := true;
  btnCreate := MakeButton(pnlBottom, SS7PBKCreate, 0, 8, 190, @CreateClick);
  btnCreate.Default := true;
  pbProgress := TProgressBar.Create(Self);
  pbProgress.Parent := pnlBottom;
  pbProgress.SetBounds(8, 28, 600, 12);
  pbProgress.Visible := false;
  lblProblems := MakeLabel(pnlBottom, '', 8, 10);
  lblProblems.AutoSize := false;
  lblProblems.Width := 600;

  //----- top
  pnlTop := MakePanel(Self, alTop, 104);
  btnBrowse := MakeButton(pnlTop, SS7PTBBrowse, 8, 8, 200, @btnBrowseClick);
  chkSymbolicNames := MakeCheck(pnlTop, SS7PTBSymbolicNames, 216, 11, 220, true);
  chkIncludeAreas := MakeCheck(pnlTop, SS7PTBIncludeAreas, 440, 11, 220, false);
  lblStatus := MakeLabel(pnlTop, SS7PTBReadFirst, 670, 13);
  lblStatus.AutoSize := false;
  lblStatus.Width := 420;
  lblFilter := MakeLabel(pnlTop, SS7PTBFilter, 8, 46);
  edtFilter := TEdit.Create(Self);
  edtFilter.Parent := pnlTop;
  edtFilter.SetBounds(56, 42, 200, 27);
  edtFilter.OnChange := @FilterChanged;
  chkShowSkipped := MakeCheck(pnlTop, SS7PTBShowSkipped, 264, 45, 300, false);
  chkShowSkipped.OnClick := @FilterChanged;
  chkShowElements := MakeCheck(pnlTop, SS7PBKShowElements, 8, 76, 420, true);
  chkShowElements.OnClick := @FilterChanged;
  btnExpandAll := MakeButton(pnlTop, SS7PTBExpandAll, 570, 42, 100, @btnExpandAllClick);
  btnCollapseAll := MakeButton(pnlTop, SS7PTBCollapseAll, 676, 42, 100, @btnCollapseAllClick);
  btnClassic := MakeButton(pnlTop, SS7PBKClassic, 0, 42, 180, nil);
  btnClassic.ModalResult := mrRetry;

  //----- tree (left)
  pnlTree := MakePanel(Self, alLeft, 340);
  lblTreeHint := TLabel.Create(Self);
  lblTreeHint.Parent := pnlTree;
  lblTreeHint.Align := alBottom;
  lblTreeHint.Caption := SS7PBKTreeHint;
  lblTreeHint.WordWrap := true;
  tvTags := TTreeView.Create(Self);
  tvTags.Parent := pnlTree;
  tvTags.Align := alClient;
  tvTags.ReadOnly := true;
  tvTags.MultiSelect := true;
  tvTags.RowSelect := true;
  tvTags.HideSelection := false;
  tvTags.DragMode := dmAutomatic;
  //without this the LCL frees the children of a node when it is collapsed
  tvTags.Items.KeepCollapsedNodes := true;
  tvTags.OnExpanding := @tvTagsExpanding;
  tvTags.OnDblClick := @tvTagsDblClick;

  popTree := TPopupMenu.Create(Self);
  mi := TMenuItem.Create(popTree);
  mi.Caption := SS7PBKAdd;
  mi.OnClick := @AddClick;
  popTree.Items.Add(mi);
  mi := TMenuItem.Create(popTree);
  mi.Caption := SS7PBKAddBelow;
  mi.OnClick := @AddBelowClick;
  popTree.Items.Add(mi);
  tvTags.PopupMenu := popTree;

  splitter := TSplitter.Create(Self);
  splitter.Parent := Self;
  splitter.Align := alLeft;
  splitter.Left := pnlTree.Width+1;

  //----- basket (right)
  pnlRight := MakePanel(Self, alClient, 0);

  pnlDefaults := MakePanel(pnlRight, alTop, 96);
  lblDefaults := MakeLabel(pnlDefaults, SS7PBKDefaults, 8, 4);
  lblDefaults.Font.Style := [fsBold];
  lblPrefix := MakeLabel(pnlDefaults, SS7PTBPrefix, 8, 28);
  lblPrefix.AutoSize := true;
  edtPrefix := TEdit.Create(Self);
  edtPrefix.Parent := pnlDefaults;
  edtPrefix.SetBounds(8, 46, 200, 27);
  lblScan := MakeLabel(pnlDefaults, SS7PTBScan, 232, 28);
  lblScan.AutoSize := true;
  spinScan := TSpinEdit.Create(Self);
  spinScan.Parent := pnlDefaults;
  spinScan.SetBounds(232, 46, 100, 27);
  spinScan.MinValue := 0;
  spinScan.MaxValue := 3600000;
  spinScan.Value := 1000;
  lblTypes := MakeLabel(pnlDefaults, SS7PBKOnlyTypes, 8, 106);
  lblTypes.AutoSize := true;
  edtTypes := TEdit.Create(Self);
  edtTypes.Parent := pnlDefaults;
  edtTypes.SetBounds(8, 124, 200, 27);
  chkMapChildren := MakeCheck(pnlDefaults, SS7PBKMapChildren, 8, 78, 640, true);
  btnAdd := MakeButton(pnlDefaults, SS7PBKAdd, 8, 160, 190, @AddClick);
  btnAddBelow := MakeButton(pnlDefaults, SS7PBKAddBelow, 206, 160, 230, @AddBelowClick);
  pnlDefaults.Height := 196;

  pnlEdit := MakePanel(pnlRight, alBottom, 206);
  lblBasketInfo := MakeLabel(pnlEdit, '', 8, 4);
  lblBasketInfo.AutoSize := false;
  lblBasketInfo.Width := 600;
  lblEditName := MakeLabel(pnlEdit, SS7PBKDetailName, 8, 26);
  lblEditName.AutoSize := true;
  edtName := TEdit.Create(Self);
  edtName.Parent := pnlEdit;
  edtName.SetBounds(8, 44, 300, 27);
  edtName.OnChange := @EditNameChange;
  lblEditScan := MakeLabel(pnlEdit, SS7PTBScan, 324, 26);
  lblEditScan.AutoSize := true;
  spinEditScan := TSpinEdit.Create(Self);
  spinEditScan.Parent := pnlEdit;
  spinEditScan.SetBounds(324, 44, 100, 27);
  spinEditScan.MinValue := 0;
  spinEditScan.MaxValue := 3600000;
  spinEditScan.OnChange := @EditScanChange;
  lblEditPrefix := MakeLabel(pnlEdit, SS7PBKEditPrefix, 540, 26);
  lblEditPrefix.AutoSize := true;
  edtEditPrefix := TEdit.Create(Self);
  edtEditPrefix.Parent := pnlEdit;
  edtEditPrefix.SetBounds(540, 44, 140, 27);
  edtEditPrefix.OnChange := @EditPrefixChange;
  chkEditMap := MakeCheck(pnlEdit, SS7PBKMapChildren, 8, 78, 640, false);
  chkEditMap.OnClick := @EditMapClick;
  lblTemplate := MakeLabel(pnlEdit, SS7PBKTemplate, 8, 108);
  lblTemplate.AutoSize := true;
  edtTemplate := TEdit.Create(Self);
  edtTemplate.Parent := pnlEdit;
  edtTemplate.SetBounds(8, 126, 300, 27);
  edtTemplate.Text := '{path}';
  btnApplyTemplate := MakeButton(pnlEdit, SS7PBKApplyTemplate, 316, 126, 190, @ApplyTemplateClick);
  btnRemove := MakeButton(pnlEdit, SS7PBKRemove, 8, 168, 190, @RemoveClick);
  btnClear := MakeButton(pnlEdit, SS7PBKClear, 206, 168, 190, @ClearClick);

  lvBasket := TListView.Create(Self);
  lvBasket.Parent := pnlRight;
  lvBasket.Align := alClient;
  lvBasket.ViewStyle := vsReport;
  lvBasket.ReadOnly := true;
  lvBasket.RowSelect := true;
  lvBasket.MultiSelect := true;
  lvBasket.HideSelection := false;
  lvBasket.OwnerData := true;
  AddColumn(SS7PBKColName, 220);
  AddColumn(SS7PBKColPath, 220);
  AddColumn(SS7PBKColBecomes, 120);
  AddColumn(SS7PBKColScan, 70);
  AddColumn(SS7PBKColChildren, 80);
  AddColumn(SS7PBKColProblem, 130);
  lvBasket.OnData := @BasketData;
  lvBasket.OnSelectItem := @BasketSelectItem;
  lvBasket.OnKeyDown := @BasketKeyDown;
  lvBasket.OnDragOver := @BasketDragOver;
  lvBasket.OnDragDrop := @BasketDragDrop;
  pnlRight.OnDragOver := @BasketDragOver;
  pnlRight.OnDragDrop := @BasketDragDrop;

  tmrSel := TTimer.Create(Self);
  tmrSel.Enabled := false;
  tmrSel.Interval := 60;
  tmrSel.OnTimer := @SelTimer;

  OnResize := @FormResize;
  FormResize(Self);
  RefreshBasket;
  ShowSelection;
end;

//what is stuck to the right edge is placed here: an anchor set while the panels still have their
//first size would keep a wrong distance
procedure TfrmS7PlusBasketBuilder.FormResize(Sender:TObject);
begin
  btnCancel.Left := pnlBottom.ClientWidth-8-btnCancel.Width;
  btnCreate.Left := btnCancel.Left-8-btnCreate.Width;
  lblProblems.Width := Max(100, btnCreate.Left-16-lblProblems.Left);
  pbProgress.Width := lblProblems.Width;
  btnClassic.Left := pnlTop.ClientWidth-8-btnClassic.Width;
  lblStatus.Width := Max(100, pnlTop.ClientWidth-8-lblStatus.Left);
end;

//------------------------------------------------------------------ driver and tree

procedure TfrmS7PlusBasketBuilder.DriverDebug(Sender:TObject; const Msg:String);
begin
  //only the last lines matter: they say why a connection or a read failed
  FDriverLog.Add(Msg);
  while FDriverLog.Count>12 do FDriverLog.Delete(0);
end;

//a port that was just opened may still be connecting
function TfrmS7PlusBasketBuilder.WaitForConnection(aTimeoutMs:LongInt):Boolean;
var
  waited:LongInt;
begin
  waited := 0;
  while (not FDriver.CommunicationPort.ReallyActive) and (waited<aTimeoutMs) do begin
    Sleep(50);
    Application.ProcessMessages;
    inc(waited, 50);
  end;
  Result := FDriver.CommunicationPort.ReallyActive;
end;

function TfrmS7PlusBasketBuilder.InfoOf(aNode:TTreeNode):TS7PlusBasketNodeInfo;
begin
  Result := TS7PlusBasketNodeInfo(aNode.Data);
end;

//a node of a block whose variables were not read yet: it has one placeholder child, to show the +
function TfrmS7PlusBasketBuilder.IsPending(aNode:TTreeNode):Boolean;
var
  info:TS7PlusBasketNodeInfo;
begin
  info := InfoOf(aNode);
  Result := Assigned(info) and (info.Block>=0) and (not FBlocks[info.Block].Loaded);
end;

function TfrmS7PlusBasketBuilder.AreaLabel(const aArea:String):String;
begin
  if SameText(aArea, 'IArea') then Result := SS7PTBAreaInputs
  else if SameText(aArea, 'QArea') then Result := SS7PTBAreaOutputs
  else if SameText(aArea, 'MArea') then Result := SS7PTBAreaFlags
  else if SameText(aArea, 'S7Timers') then Result := SS7PTBAreaTimers
  else if SameText(aArea, 'S7Counters') then Result := SS7PTBAreaCounters
  else Result := '';
end;

function TfrmS7PlusBasketBuilder.SizeText(const aCands:TS7PlusCandidateArray; const aPath:String;
  aIsDBRoot:Boolean):String;
var
  size:LongInt;
  exact:Boolean;
begin
  size := S7PlusContainerSize(aCands, aPath, aIsDBRoot, exact);
  if exact then Result := Format(SS7PTBBytes, [size])
  else Result := Format(SS7PTBAtLeastBytes, [size]);
end;

function TfrmS7PlusBasketBuilder.RootText(aBlock:LongInt):String;
begin
  Result := FBlocks[aBlock].Key;
  if FBlocks[aBlock].IsArea then
    Result := Result+' : '+AreaLabel(Result)
  else if FBlocks[aBlock].Loaded then
    Result := Result+' : '+FBlocks[aBlock].SizeStr;
end;

//what follows the name on the tree: the PLC type, and why a variable is not a tag
function TfrmS7PlusBasketBuilder.Decoration(aIndex:LongInt):String;
begin
  Result := ' : '+S7PlusSoftTypeName(FCandidates[aIndex].SoftDataType);
  if FCandidates[aIndex].IsString and (FCandidates[aIndex].StringLength>=0) then
    Result := Result+'['+IntToStr(FCandidates[aIndex].StringLength)+']';
  if FCandidates[aIndex].IsArray then
    Result := Result+'['+IntToStr(FCandidates[aIndex].ElementCount)+']';
  if FCandidates[aIndex].IsStructArray then
    Result := Result+'['+IntToStr(FCandidates[aIndex].ElementCount)+'] ('+
              Format(SS7PTBBytes, [FCandidates[aIndex].ElementCount*FCandidates[aIndex].ElementSize])+')';
  case FCandidates[aIndex].Skip of
    srUnsupportedType: Result := Result+'   ('+SS7PTBSkipUnsupportedType+')';
    srStringLength:    Result := Result+'   ('+SS7PTBSkipStringLength+')';
  end;
end;

//a variable is on the tree when it can go into the basket (or, with the option, when it is
//one the PLC lists but does not become a tag)
function TfrmS7PlusBasketBuilder.ShowCandidate(aIndex:LongInt):Boolean;
var
  k:TS7PlusBasketKind;
begin
  Result := false;
  //the elements of an array, and what is inside an element of an array of structures, are tags
  //of their own by path: the option hides them (a DB can have thousands)
  if (Pos('[', FCandidates[aIndex].Path)>0) and (not chkShowElements.Checked) then exit;
  if S7PlusBasketKindOf(FCandidates, aIndex, k) then
    Result := true
  else
    //a structure that cannot be dragged is still a level; the others only when asked
    Result := chkShowSkipped.Checked and (FCandidates[aIndex].Skip<>srNotAScalar) and
              (FCandidates[aIndex].Skip<>srArrayElement);
end;

function TfrmS7PlusBasketBuilder.PassFilter(aIndex:LongInt):Boolean;
var
  aFilter:String;
begin
  Result := ShowCandidate(aIndex);
  if not Result then exit;
  aFilter := LowerCase(Trim(edtFilter.Text));
  if aFilter='' then exit;
  //with * or ? the filter is a mask, otherwise a piece of the path
  if (Pos('*', aFilter)>0) or (Pos('?', aFilter)>0) then
    Result := MatchesMask(LowerCase(FCandidates[aIndex].Path), '*'+aFilter+'*')
  else
    Result := Pos(aFilter, LowerCase(FCandidates[aIndex].Path))>0;
end;

procedure TfrmS7PlusBasketBuilder.SaveExpansion(aExpanded:TStringList);
var
  c:LongInt;
  n:TTreeNode;
begin
  aExpanded.Clear;
  for c:=0 to tvTags.Items.Count-1 do begin
    n := tvTags.Items[c];
    if n.Expanded and Assigned(InfoOf(n)) then
      aExpanded.Add(InfoOf(n).Key);
  end;
end;

procedure TfrmS7PlusBasketBuilder.RestoreExpansion(aExpanded:TStringList; aExpandAll:Boolean);
var
  c:LongInt;
  n:TTreeNode;
begin
  for c:=0 to tvTags.Items.Count-1 do begin
    n := tvTags.Items[c];
    if (not n.HasChildren) or IsPending(n) then continue;
    if aExpandAll or (aExpanded.IndexOf(InfoOf(n).Key)>=0) then
      n.Expanded := true;
  end;
end;

procedure TfrmS7PlusBasketBuilder.RebuildTree;
var
  c:LongInt;
  info:TS7PlusBasketNodeInfo;
  node:TTreeNode;
  expanded:TStringList;
  aFilter:Boolean;
begin
  expanded := TStringList.Create;
  try
    SaveExpansion(expanded);
    aFilter := Trim(edtFilter.Text)<>'';

    tvTags.Items.BeginUpdate;
    try
      tvTags.Items.Clear;
      FInfos.Clear;

      for c:=0 to High(FBlocks) do begin
        info := TS7PlusBasketNodeInfo.Create;
        info.Key := FBlocks[c].Key;
        info.Index := -1;
        info.Block := c;
        FInfos.Add(info);

        node := tvTags.Items.AddChild(nil, RootText(c));
        node.Data := info;
        if FBlocks[c].Loaded then begin
          BuildBlock(c, node);
          //a filter shows what it found
          if aFilter and (not node.HasChildren) then node.Delete;
        end else
          tvTags.Items.AddChild(node, '...');
      end;
    finally
      tvTags.Items.EndUpdate;
    end;

    //a filter shows what it found; without one, only what the user had opened stays open
    RestoreExpansion(expanded, aFilter);
  finally
    expanded.Free;
  end;
end;

//the structures and variables of a block that was read, under its node
procedure TfrmS7PlusBasketBuilder.BuildBlock(aBlock:LongInt; aRoot:TTreeNode);
var
  c, k, p:LongInt;
  levels:TStringArray;
  candIdx:TStringList;        //path of a candidate -> its index
  nodes:TStringList;          //path of a level -> its node
  aParent, node:TTreeNode;
  key, baseKey, lvl:String;

  //the node of a level, made if it is not there yet; a level that is a variable of the list
  //(a structure, an array, an element) can be dragged too
  function NodeOf(const aKey, aLevel:String; aUnder:TTreeNode):TTreeNode;
  var
    q:LongInt;
    info:TS7PlusBasketNodeInfo;
    kind:TS7PlusBasketKind;
    aText:String;
    ci:LongInt;
  begin
    if nodes.Find(aKey, q) then begin
      Result := TTreeNode(nodes.Objects[q]);
      exit;
    end;
    info := TS7PlusBasketNodeInfo.Create;
    info.Key := aKey;
    info.Index := -1;
    info.Block := -1;
    FInfos.Add(info);

    aText := aLevel;
    if candIdx.Find(aKey, q) then begin
      ci := PtrInt(candIdx.Objects[q]);
      if S7PlusBasketKindOf(FCandidates, ci, kind) then
        info.Index := ci;
      aText := aText+Decoration(ci);
      //the size of a structure; inside an array element there can be thousands: none
      if (TS7PlusSoftDataType(FCandidates[ci].SoftDataType)=sdtSTRUCT) and
         (not FCandidates[ci].IsStructArray) and (Pos('[', aKey)=0) then
        aText := aText+' ('+SizeText(FBlocks[aBlock].Cands, aKey, false)+')';
    end;

    Result := tvTags.Items.AddChild(aUnder, aText);
    Result.Data := info;
    nodes.AddObject(aKey, Result);
  end;

begin
  candIdx := TStringList.Create;
  nodes := TStringList.Create;
  tvTags.Items.BeginUpdate;
  try
    candIdx.Sorted := true;
    candIdx.Duplicates := dupIgnore;
    nodes.Sorted := true;
    nodes.Duplicates := dupIgnore;
    nodes.AddObject(FBlocks[aBlock].Key, aRoot);

    for c:=FBlocks[aBlock].First to FBlocks[aBlock].First+FBlocks[aBlock].Count-1 do
      candIdx.AddObject(FCandidates[c].Path, TObject(PtrInt(c)));

    for c:=FBlocks[aBlock].First to FBlocks[aBlock].First+FBlocks[aBlock].Count-1 do begin
      if not PassFilter(c) then continue;

      levels := S7PlusSplitPath(FCandidates[c].Path);
      aParent := nil;
      key := '';
      for k:=0 to High(levels) do begin
        if key<>'' then key := key+'.';
        key := key+levels[k];
        lvl := levels[k];

        if nodes.Find(key, p) then
          node := TTreeNode(nodes.Objects[p])
        else begin
          //an element of an array (Arr[2]) hangs from the array (Arr), made if it is not there
          if (lvl<>'') and (lvl[Length(lvl)]=']') and (Pos('[', lvl)>1) then begin
            baseKey := Copy(key, 1, Length(key)-Length(lvl))+Copy(lvl, 1, Pos('[', lvl)-1);
            aParent := NodeOf(baseKey, Copy(lvl, 1, Pos('[', lvl)-1), aParent);
          end;
          node := NodeOf(key, lvl, aParent);
        end;
        aParent := node;
      end;
    end;
  finally
    tvTags.Items.EndUpdate;
    nodes.Free;
    candIdx.Free;
  end;
end;

//reads the variables of a block from the PLC and hangs them from its node (aRoot is the node
//being opened). False when they could not be read.
function TfrmS7PlusBasketBuilder.LoadBlock(aBlock:LongInt; aRoot:TTreeNode):Boolean;
var
  res:TProtocolIOResult;
  oldCursor:TCursor;
  oldDebug:TS7PlusDebugEvent;
  vars:TS7PlusVarInfoArray;
  local:TS7PlusCandidateArray;
  c, first:LongInt;
begin
  Result := FBlocks[aBlock].Loaded;
  if Result or FLoading or (not Assigned(FDriver)) then exit;

  FLoading := true;
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  oldDebug := FDriver.OnDebug;
  FDriverLog.Clear;
  FDriver.OnDebug := @DriverDebug;
  pbProgress.Style := pbstMarquee;
  pbProgress.Visible := true;
  btnBrowse.Enabled := false;
  try
    lblStatus.Caption := Format(SS7PTBReadingBlock, [FBlocks[aBlock].Key]);
    pbProgress.Repaint;
    Application.ProcessMessages;

    if FBlocks[aBlock].IsArea then
      res := FDriver.BrowseNativeArea(FBlocks[aBlock].AreaRID, FBlocks[aBlock].AreaTiRid,
                                      FBlocks[aBlock].AreaName, vars)
    else
      res := FDriver.BrowseDB(FBlocks[aBlock].Number, vars);

    if res<>ioOk then begin
      Screen.Cursor := oldCursor;
      if res=ioAuthError then
        MessageDlg(SS7PTBAuthRequired, mtError, [mbOK], 0)
      else
        MessageDlg(Format(SS7PTBBlockFailed, [FBlocks[aBlock].Key])+LineEnding+LineEnding+
                   SS7PTBDetails+LineEnding+FDriverLog.Text, mtError, [mbOK], 0);
      UpdateStatus;
      exit;
    end;

    SetLength(local, 0);
    S7PlusAddBrowsed(local, vars, FBlocks[aBlock].DBName, FBlocks[aBlock].AccessArea);
    S7PlusMarkArrays(local);
    //the maximum length of each STRING is in its value: read them once
    lblStatus.Caption := SS7PTBReadingStrings;
    Application.ProcessMessages;
    FDriver.ResolveStringLengths(local);

    first := Length(FCandidates);
    SetLength(FCandidates, first+Length(local));
    for c:=0 to High(local) do begin
      FCandidates[first+c] := local[c];
      if (not (local[c].Skip in [srNone, srNotAScalar])) and (not local[c].IsElement) then inc(FSkippedCount);
    end;

    FBlocks[aBlock].First := first;
    FBlocks[aBlock].Count := Length(local);
    FBlocks[aBlock].Cands := local;
    if not FBlocks[aBlock].IsArea then
      FBlocks[aBlock].SizeStr := SizeText(local, FBlocks[aBlock].Key, true);
    FBlocks[aBlock].Loaded := true;

    //the placeholder gives way to the variables
    aRoot.DeleteChildren;
    aRoot.Text := RootText(aBlock);
    BuildBlock(aBlock, aRoot);
    UpdateStatus;
    Result := true;
  finally
    pbProgress.Visible := false;
    btnBrowse.Enabled := true;
    FDriver.OnDebug := oldDebug;
    Screen.Cursor := oldCursor;
    FLoading := false;
  end;
end;

procedure TfrmS7PlusBasketBuilder.UpdateStatus;
var
  c, nRead:LongInt;
begin
  if Length(FBlocks)=0 then begin
    lblStatus.Caption := SS7PTBNothingRead;
    exit;
  end;
  nRead := 0;
  for c:=0 to High(FBlocks) do
    if FBlocks[c].Loaded then inc(nRead);

  lblStatus.Caption := Format(SS7PTBBlocksInfo, [Length(FBlocks), nRead, Length(FCandidates), FSkippedCount]);
  if nRead<Length(FBlocks) then begin
    lblStatus.Caption := lblStatus.Caption+' '+SS7PTBOpenToRead;
    if Trim(edtFilter.Text)<>'' then
      lblStatus.Caption := lblStatus.Caption+' '+SS7PTBFilterLoadedOnly;
  end;
end;

procedure TfrmS7PlusBasketBuilder.tvTagsExpanding(Sender:TObject; Node:TTreeNode;
  var AllowExpansion:Boolean);
begin
  if IsPending(Node) then
    AllowExpansion := LoadBlock(InfoOf(Node).Block, Node);
end;

procedure TfrmS7PlusBasketBuilder.btnBrowseClick(Sender:TObject);
var
  oldCursor:TCursor;
  oldDebug:TS7PlusDebugEvent;
  res:TProtocolIOResult;
  blocks:TS7PlusDataBlockInfoArray;
  c, n:LongInt;
begin
  if not Assigned(FDriver) then begin
    MessageDlg(SS7PTBWithoutDriver, mtError, [mbOK], 0);
    exit;
  end;

  if (not Assigned(FDriver.CommunicationPort)) or
     (not FDriver.CommunicationPort.Active) then begin
    MessageDlg(SS7PTBPortMustBeActive, mtError, [mbOK], 0);
    exit;
  end;

  SetLength(FCandidates, 0);
  SetLength(FBlocks, 0);
  FSkippedCount := 0;

  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  oldDebug := FDriver.OnDebug;
  FDriverLog.Clear;
  FDriver.OnDebug := @DriverDebug;
  //busy from the start: connecting and listing the DBs have no count
  pbProgress.Style := pbstMarquee;
  pbProgress.Visible := true;
  btnBrowse.Enabled := false;
  try
    lblStatus.Caption := SS7PTBReading;
    Application.ProcessMessages;

    //Active only says the port was asked to open: at design time the socket may not be there
    if not WaitForConnection(5000) then begin
      Screen.Cursor := oldCursor;
      MessageDlg(SS7PTBPortNotConnected, mtError, [mbOK], 0);
      lblStatus.Caption := SS7PTBNothingRead;
      RebuildTree;
      exit;
    end;

    //a download to the PLC since the last read adds and changes DBs and types: read it all again
    FDriver.RefreshCaches;
    res := FDriver.ListDataBlocks(blocks);
    if res<>ioOk then begin
      Screen.Cursor := oldCursor;
      if res=ioAuthError then
        MessageDlg(SS7PTBAuthRequired, mtError, [mbOK], 0)
      else
        MessageDlg(SS7PTBBrowseFailed+LineEnding+LineEnding+SS7PTBDetails+LineEnding+
                   FDriverLog.Text, mtError, [mbOK], 0);
      lblStatus.Caption := SS7PTBNothingRead;
      RebuildTree;
      exit;
    end;

    //only the first level: the variables of each one are read when it is opened
    FSymbolic := chkSymbolicNames.Checked;
    n := Length(blocks);
    if chkIncludeAreas.Checked then inc(n, 3);
    SetLength(FBlocks, n);
    for c:=0 to High(blocks) do begin
      FBlocks[c].Number := blocks[c].Number;
      FBlocks[c].AccessArea := S7PlusIds_DBAccessAreaBase+blocks[c].Number;
      if FSymbolic and (blocks[c].Name<>'') then begin
        FBlocks[c].Key := blocks[c].Name;
        FBlocks[c].DBName := blocks[c].Name;
      end else
        FBlocks[c].Key := 'DB'+IntToStr(blocks[c].Number);
    end;
    if chkIncludeAreas.Checked then begin
      n := Length(blocks);
      FBlocks[n].IsArea := true;
      FBlocks[n].Key := 'IArea';
      FBlocks[n].AreaName := 'IArea';
      FBlocks[n].AreaRID := S7PlusIds_NativeIAreaRID;
      FBlocks[n].AreaTiRid := S7PlusTiRid_IArea;
      FBlocks[n].AccessArea := S7PlusIds_NativeIAreaRID;
      FBlocks[n+1].IsArea := true;
      FBlocks[n+1].Key := 'QArea';
      FBlocks[n+1].AreaName := 'QArea';
      FBlocks[n+1].AreaRID := S7PlusIds_NativeQAreaRID;
      FBlocks[n+1].AreaTiRid := S7PlusTiRid_QArea;
      FBlocks[n+1].AccessArea := S7PlusIds_NativeQAreaRID;
      FBlocks[n+2].IsArea := true;
      FBlocks[n+2].Key := 'MArea';
      FBlocks[n+2].AreaName := 'MArea';
      FBlocks[n+2].AreaRID := S7PlusIds_NativeMAreaRID;
      FBlocks[n+2].AreaTiRid := S7PlusTiRid_MArea;
      FBlocks[n+2].AccessArea := S7PlusIds_NativeMAreaRID;
    end;
    RebuildTree;
    UpdateStatus;
  finally
    pbProgress.Visible := false;
    btnBrowse.Enabled := true;
    FDriver.OnDebug := oldDebug;
    Screen.Cursor := oldCursor;
  end;
end;

procedure TfrmS7PlusBasketBuilder.btnExpandAllClick(Sender:TObject);
var
  c:LongInt;
begin
  tvTags.Items.BeginUpdate;
  try
    for c:=0 to tvTags.Items.Count-1 do
      //an unread block is not opened by "expand all": reading every DB at once is too slow
      if tvTags.Items[c].HasChildren and (not IsPending(tvTags.Items[c])) then
        tvTags.Items[c].Expanded := true;
  finally
    tvTags.Items.EndUpdate;
  end;
end;

procedure TfrmS7PlusBasketBuilder.btnCollapseAllClick(Sender:TObject);
begin
  tvTags.FullCollapse;
end;

procedure TfrmS7PlusBasketBuilder.FilterChanged(Sender:TObject);
begin
  RebuildTree;
end;

//------------------------------------------------------------------ the basket

function TfrmS7PlusBasketBuilder.EntryName(i:LongInt):String;
begin
  Result := FEntries[i].Name;
  if Result='' then
    Result := S7PlusSuggestedName(FEntries[i].Path, FEntries[i].Prefix);
end;

function TfrmS7PlusBasketBuilder.EntryKindText(const E:TS7PlusBasketEntry):String;
var
  c:LongInt;
begin
  case E.Kind of
    bkBlock:  Result := 'TPLCBlock';
    bkStruct: Result := 'TPLCStruct';
  else
    Result := 'TPLCTagNumber';
    if not E.IsDB then
      for c:=0 to High(FCandidates) do
        if SameText(FCandidates[c].Path, E.Path) then begin
          if FCandidates[c].IsString then Result := 'TPLCString';
          break;
        end;
  end;
end;

function TfrmS7PlusBasketBuilder.FindEntry(const aPath:String; aIsDB:Boolean):LongInt;
var
  i:LongInt;
begin
  for i:=0 to FCount-1 do
    if (FEntries[i].IsDB=aIsDB) and SameText(FEntries[i].Path, aPath) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

procedure TfrmS7PlusBasketBuilder.PushEntry(const E:TS7PlusBasketEntry);
begin
  //the capacity doubles: thousands of entries are added one at a time
  if FCount>=Length(FEntries) then begin
    SetLength(FEntries, (FCount+16)*2);
    SetLength(FProblem, Length(FEntries));
  end;
  FEntries[FCount] := E;
  FProblem[FCount] := '';
  FKindText.Add(EntryKindText(E));
  inc(FCount);
end;

//the entry a tree node stands for; False when it cannot go in the basket
function TfrmS7PlusBasketBuilder.NewEntryOfNode(aNode:TTreeNode; out E:TS7PlusBasketEntry):Boolean;
var
  info:TS7PlusBasketNodeInfo;
begin
  Result := false;
  info := InfoOf(aNode);
  if info=nil then exit;
  if info.Block>=0 then begin
    //a whole DB (the areas are not one structure)
    if FBlocks[info.Block].IsArea or (not FBlocks[info.Block].Loaded) then exit;
    Result := S7PlusBasketNewDBEntry(FBlocks[info.Block].Key, edtPrefix.Text, spinScan.Value,
                                     chkMapChildren.Checked, E);
  end else if info.Index>=0 then
    Result := S7PlusBasketNewEntry(FCandidates, info.Index, edtPrefix.Text, spinScan.Value,
                                   chkMapChildren.Checked, E);
end;

procedure TfrmS7PlusBasketBuilder.FinishAdd(aAdded, aSkipped:LongInt);
begin
  RefreshBasket;
  ShowSelection;
  if (aAdded>0) or (aSkipped>0) then
    lblStatus.Caption := Format(SS7PBKAdded, [aAdded, aSkipped]);
end;

procedure TfrmS7PlusBasketBuilder.AddNodes(aBelow:Boolean);
var
  i, added, skipped:LongInt;
  node:TTreeNode;
  E:TS7PlusBasketEntry;
  info:TS7PlusBasketNodeInfo;
begin
  added := 0;
  skipped := 0;

  //a DB that was not read is read first: that rebuilds what is under its node, so it comes
  //before the selection is walked
  for i:=0 to tvTags.SelectionCount-1 do begin
    node := tvTags.Selections[i];
    info := InfoOf(node);
    if Assigned(info) and (info.Block>=0) and (not FBlocks[info.Block].IsArea) and
       (not FBlocks[info.Block].Loaded) then
      LoadBlock(info.Block, node);
  end;

  for i:=0 to tvTags.SelectionCount-1 do begin
    node := tvTags.Selections[i];
    if aBelow then begin
      AddBelowNode(node, added, skipped);
      continue;
    end;
    if NewEntryOfNode(node, E) and (FindEntry(E.Path, E.IsDB)<0) then begin
      PushEntry(E);
      inc(added);
    end else
      inc(skipped);
  end;
  FinishAdd(added, skipped);
end;

//every variable (a simple one or an array of simple ones) below a node, one entry each: the
//way to take the hundreds of variables of a DB without dragging them one by one
procedure TfrmS7PlusBasketBuilder.AddBelowNode(aNode:TTreeNode; var aAdded, aSkipped:LongInt);
var
  info:TS7PlusBasketNodeInfo;
  c, first, last:LongInt;
  kind:TS7PlusBasketKind;
  E:TS7PlusBasketEntry;
  Pfx:String;
begin
  info := InfoOf(aNode);
  if info=nil then exit;
  Pfx := info.Key+'.';
  first := 0;
  last := High(FCandidates);
  if info.Block>=0 then begin
    if not FBlocks[info.Block].Loaded then exit;
    first := FBlocks[info.Block].First;
    last := first+FBlocks[info.Block].Count-1;
  end;
  for c:=first to last do begin
    if Pos(Pfx, FCandidates[c].Path)<>1 then continue;
    //the elements of an array come with the array (and are dragged one by one when wanted)
    if Pos('[', FCandidates[c].Path)>0 then continue;
    if not PassFilter(c) then continue;
    if not S7PlusBasketKindOf(FCandidates, c, kind) then continue;
    if kind=bkStruct then continue;
    if not S7PlusTypeListMatches(edtTypes.Text, FCandidates[c].SoftDataType) then continue;
    if S7PlusBasketNewEntry(FCandidates, c, edtPrefix.Text, spinScan.Value, chkMapChildren.Checked, E) and
       (FindEntry(E.Path, E.IsDB)<0) then begin
      PushEntry(E);
      inc(aAdded);
    end else
      inc(aSkipped);
  end;
end;

procedure TfrmS7PlusBasketBuilder.AddClick(Sender:TObject);
begin
  AddNodes(false);
end;

procedure TfrmS7PlusBasketBuilder.AddBelowClick(Sender:TObject);
begin
  AddNodes(true);
end;

procedure TfrmS7PlusBasketBuilder.tvTagsDblClick(Sender:TObject);
begin
  //a node with children also expands on a double click: only add what is a leaf of the tree
  if (tvTags.Selected<>nil) and (not tvTags.Selected.HasChildren) then
    AddNodes(false);
end;

procedure TfrmS7PlusBasketBuilder.BasketDragOver(Sender, Source:TObject; X, Y:Integer;
  State:TDragState; var Accept:Boolean);
begin
  Accept := Source=tvTags;
end;

procedure TfrmS7PlusBasketBuilder.BasketDragDrop(Sender, Source:TObject; X, Y:Integer);
begin
  if Source=tvTags then AddNodes(false);
end;

procedure TfrmS7PlusBasketBuilder.RefreshBasket;
begin
  lvBasket.Items.Count := FCount;
  lvBasket.Invalidate;
  RefreshProblems;
end;

//the names of the entries themselves: invalid and repeated ones show on their rows at once.
//(The names of the children are checked when the components are created.)
procedure TfrmS7PlusBasketBuilder.RefreshProblems;
var
  i, p:LongInt;
  names:TStringList;
  nm:String;
begin
  FProblemCount := 0;
  names := TStringList.Create;
  try
    names.CaseSensitive := false;
    names.Sorted := true;
    names.Duplicates := dupAccept;
    //the first pass counts the repeats; the second marks the rows
    for i:=0 to FCount-1 do begin
      FProblem[i] := '';
      names.Add(EntryName(i));
    end;
    for i:=0 to FCount-1 do begin
      nm := EntryName(i);
      if not IsValidIdent(nm) then
        FProblem[i] := SS7PBKInvalidName
      else if names.Find(nm, p) then begin
        //Find lands on one of the equal names: the neighbours say whether there are more
        if ((p>0) and SameText(names[p-1], nm)) or ((p<names.Count-1) and SameText(names[p+1], nm)) then
          FProblem[i] := SS7PBKDuplicateName;
      end;
      if FProblem[i]<>'' then inc(FProblemCount);
    end;
  finally
    names.Free;
  end;

  if FProblemCount>0 then
    lblProblems.Caption := Format(SS7PBKNameProblems, [FProblemCount])
  else
    lblProblems.Caption := '';
  lvBasket.Invalidate;
end;

procedure TfrmS7PlusBasketBuilder.BasketData(Sender:TObject; Item:TListItem);
var
  i:LongInt;
  children:String;
begin
  i := Item.Index;
  if (i<0) or (i>=FCount) then exit;
  Item.Caption := EntryName(i);
  Item.SubItems.Clear;
  Item.SubItems.Add(FEntries[i].Path);
  Item.SubItems.Add(FKindText[i]);
  Item.SubItems.Add(IntToStr(FEntries[i].Scan));
  if FEntries[i].Kind=bkTag then
    children := '-'
  else if FEntries[i].MapChildren then
    children := SS7PBKYes
  else
    children := SS7PBKNo;
  Item.SubItems.Add(children);
  Item.SubItems.Add(FProblem[i]);
end;

procedure TfrmS7PlusBasketBuilder.BasketSelectItem(Sender:TObject; Item:TListItem; Selected:Boolean);
begin
  //one event per row of a multiple selection: wait for the last one
  tmrSel.Enabled := false;
  tmrSel.Enabled := true;
end;

procedure TfrmS7PlusBasketBuilder.SelTimer(Sender:TObject);
begin
  tmrSel.Enabled := false;
  ShowSelection;
end;

procedure TfrmS7PlusBasketBuilder.BasketKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);
begin
  if Key=46 then begin     //Delete
    RemoveClick(Sender);
    Key := 0;
  end;
end;

procedure TfrmS7PlusBasketBuilder.CollectSelection;
var
  i, n:LongInt;
begin
  SetLength(FSel, 0);
  if lvBasket.SelCount=0 then exit;
  SetLength(FSel, lvBasket.SelCount);
  n := 0;
  for i:=0 to FCount-1 do begin
    if n>=Length(FSel) then break;
    if lvBasket.Items[i].Selected then begin
      FSel[n] := i;
      inc(n);
    end;
  end;
  SetLength(FSel, n);
end;

//the edit panel shows the selected entries: with several, the fields they have in common
procedure TfrmS7PlusBasketBuilder.ShowSelection;
var
  i, kids, withKids:LongInt;
  sameScan, sameMap:Boolean;
begin
  CollectSelection;
  FUpdating := true;
  try
    lblBasketInfo.Caption := Format(SS7PBKBasketInfo, [FCount, Length(FSel)]);
    if FCount=0 then
      lblBasketInfo.Caption := SS7PBKEmptyBasket;

    edtName.Enabled := Length(FSel)=1;
    spinEditScan.Enabled := Length(FSel)>0;
    edtEditPrefix.Enabled := Length(FSel)>0;
    btnApplyTemplate.Enabled := Length(FSel)>0;
    btnRemove.Enabled := Length(FSel)>0;
    btnClear.Enabled := FCount>0;
    btnCreate.Enabled := FCount>0;

    if Length(FSel)=0 then begin
      edtName.Text := '';
      edtEditPrefix.Text := '';
      chkEditMap.Enabled := false;
      chkEditMap.Checked := false;
      exit;
    end;

    if Length(FSel)=1 then
      edtName.Text := EntryName(FSel[0])
    else
      edtName.Text := '';

    //the prefix they share; with different ones the field stays empty
    edtEditPrefix.Text := FEntries[FSel[0]].Prefix;
    for i:=1 to High(FSel) do
      if FEntries[FSel[i]].Prefix<>FEntries[FSel[0]].Prefix then edtEditPrefix.Text := '';

    sameScan := true;
    for i:=1 to High(FSel) do
      if FEntries[FSel[i]].Scan<>FEntries[FSel[0]].Scan then sameScan := false;
    if sameScan then spinEditScan.Value := FEntries[FSel[0]].Scan;

    //only blocks and structures have child items to map
    kids := 0;
    withKids := 0;
    sameMap := true;
    for i:=0 to High(FSel) do
      if FEntries[FSel[i]].Kind<>bkTag then begin
        if kids>0 then
          if FEntries[FSel[i]].MapChildren<>(withKids>0) then sameMap := false;
        inc(kids);
        if FEntries[FSel[i]].MapChildren then inc(withKids);
      end;
    chkEditMap.Enabled := kids>0;
    chkEditMap.AllowGrayed := false;
    if kids=0 then
      chkEditMap.Checked := false
    else if not sameMap then
      chkEditMap.State := cbGrayed
    else
      chkEditMap.Checked := withKids>0;
  finally
    FUpdating := false;
  end;
end;

procedure TfrmS7PlusBasketBuilder.EditNameChange(Sender:TObject);
begin
  if FUpdating or (Length(FSel)<>1) then exit;
  FEntries[FSel[0]].Name := Trim(edtName.Text);
  RefreshProblems;
end;

procedure TfrmS7PlusBasketBuilder.EditScanChange(Sender:TObject);
var
  i:LongInt;
begin
  if FUpdating then exit;
  for i:=0 to High(FSel) do
    FEntries[FSel[i]].Scan := spinEditScan.Value;
  lvBasket.Invalidate;
end;

procedure TfrmS7PlusBasketBuilder.EditPrefixChange(Sender:TObject);
var
  i:LongInt;
begin
  if FUpdating then exit;
  for i:=0 to High(FSel) do
    FEntries[FSel[i]].Prefix := edtEditPrefix.Text;
  //an entry with no name of its own is named from its prefix
  if Length(FSel)=1 then begin
    FUpdating := true;
    try
      edtName.Text := EntryName(FSel[0]);
    finally
      FUpdating := false;
    end;
  end;
  RefreshProblems;
end;

//the name of each selected entry from the template, numbered in the order of the basket
procedure TfrmS7PlusBasketBuilder.ApplyTemplateClick(Sender:TObject);
var
  i:LongInt;
begin
  CollectSelection;
  if (Length(FSel)=0) or (Trim(edtTemplate.Text)='') then exit;
  for i:=0 to High(FSel) do
    FEntries[FSel[i]].Name := Trim(S7PlusBasketApplyTemplate(edtTemplate.Text, FEntries[FSel[i]].Path, i+1));
  RefreshProblems;
  ShowSelection;
end;

procedure TfrmS7PlusBasketBuilder.EditMapClick(Sender:TObject);
var
  i:LongInt;
begin
  if FUpdating then exit;
  for i:=0 to High(FSel) do
    if FEntries[FSel[i]].Kind<>bkTag then
      FEntries[FSel[i]].MapChildren := chkEditMap.Checked;
  lvBasket.Invalidate;
end;

procedure TfrmS7PlusBasketBuilder.RemoveEntries(const aRemove:array of Boolean);
var
  i, n:LongInt;
begin
  n := 0;
  for i:=0 to FCount-1 do begin
    if (i<=High(aRemove)) and aRemove[i] then continue;
    if n<>i then begin
      FEntries[n] := FEntries[i];
      FProblem[n] := FProblem[i];
      FKindText[n] := FKindText[i];
    end;
    inc(n);
  end;
  for i:=FCount-1 downto n do
    FKindText.Delete(i);
  FCount := n;
  lvBasket.ClearSelection;
  RefreshBasket;
  ShowSelection;
end;

procedure TfrmS7PlusBasketBuilder.RemoveClick(Sender:TObject);
var
  rem:array of Boolean;
  i:LongInt;
begin
  CollectSelection;
  if Length(FSel)=0 then exit;
  SetLength(rem, FCount);
  for i:=0 to High(rem) do rem[i] := false;
  for i:=0 to High(FSel) do rem[FSel[i]] := true;
  RemoveEntries(rem);
end;

procedure TfrmS7PlusBasketBuilder.ClearClick(Sender:TObject);
var
  rem:array of Boolean;
  i:LongInt;
begin
  SetLength(rem, FCount);
  for i:=0 to High(rem) do rem[i] := true;
  RemoveEntries(rem);
end;

//------------------------------------------------------------------ creating

//One plan item per entry, with a progress bar: the plan of an entry looks at every candidate,
//so with thousands of entries it takes a while and the form must keep answering.
function TfrmS7PlusBasketBuilder.BuildPlan:Boolean;
var
  i, k, n:LongInt;
  one:TS7PlusBasketEntries;
  part:TS7PlusImportItems;
  problems:TS7PlusNameProblems;
  msg:String;
begin
  Result := false;
  SetLength(FPlan, 0);
  SetLength(one, 1);
  pbProgress.Style := pbstNormal;
  pbProgress.Min := 0;
  pbProgress.Max := FCount;
  pbProgress.Position := 0;
  pbProgress.Visible := true;
  Screen.Cursor := crHourGlass;
  try
    for i:=0 to FCount-1 do begin
      one[0] := FEntries[i];
      part := S7PlusBasketBuild(FCandidates, one);
      n := Length(FPlan);
      SetLength(FPlan, n+Length(part));
      for k:=0 to High(part) do
        FPlan[n+k] := part[k];
      if (i and 15)=0 then begin
        pbProgress.Position := i;
        lblProblems.Caption := Format(SS7PBKBuilding, [i+1, FCount]);
        Application.ProcessMessages;
      end;
    end;
  finally
    pbProgress.Visible := false;
    Screen.Cursor := crDefault;
  end;

  //the names of the children (items, elements, bits) can repeat too
  problems := S7PlusBasketNameProblems(FPlan, []);
  if Length(problems)>0 then begin
    msg := SS7PBKFixNames;
    for i:=0 to High(problems) do begin
      if i>=15 then begin
        msg := msg+LineEnding+'...';
        break;
      end;
      msg := msg+LineEnding+problems[i].Name+': ';
      case problems[i].Kind of
        npInvalid:   msg := msg+SS7PBKInvalidName;
        npDuplicate: msg := msg+SS7PBKDuplicateName;
        npExists:    msg := msg+SS7PBKExistsName;
      end;
    end;
    RefreshProblems;
    MessageDlg(msg, mtWarning, [mbOK], 0);
    exit;
  end;
  RefreshProblems;
  Result := true;
end;

procedure TfrmS7PlusBasketBuilder.CreateClick(Sender:TObject);
begin
  if FCount=0 then begin
    MessageDlg(SS7PBKEmptyBasket, mtInformation, [mbOK], 0);
    exit;
  end;
  RefreshProblems;
  if FProblemCount>0 then begin
    MessageDlg(Format(SS7PBKNameProblems, [FProblemCount]), mtWarning, [mbOK], 0);
    exit;
  end;
  if BuildPlan then
    ModalResult := mrOK;
end;

function TfrmS7PlusBasketBuilder.GetImportPlan:TS7PlusImportItems;
begin
  Result := FPlan;
end;

procedure TfrmS7PlusBasketBuilder.LoadCandidates(const aCandidates:TS7PlusCandidateArray);
var
  c, n, first:LongInt;
  levels:TStringArray;
begin
  FCandidates := aCandidates;
  S7PlusMarkArrays(FCandidates);
  //the maximum length of each STRING is in its value: read them once
  if Assigned(FDriver) then
    FDriver.ResolveStringLengths(FCandidates);

  FSkippedCount := 0;
  SetLength(FBlocks, 0);
  for c:=0 to High(FCandidates) do begin
    if (not (FCandidates[c].Skip in [srNone, srNotAScalar])) and (not FCandidates[c].IsElement) then
      inc(FSkippedCount);

    levels := S7PlusSplitPath(FCandidates[c].Path);
    if Length(levels)=0 then continue;
    n := Length(FBlocks);
    //the variables of a block come together
    if (n=0) or (FBlocks[n-1].Key<>levels[0]) then begin
      SetLength(FBlocks, n+1);
      FBlocks[n].Key := levels[0];
      FBlocks[n].IsArea := S7PlusIsAreaName(levels[0]);
      FBlocks[n].First := c;
      FBlocks[n].Loaded := true;
      n := n+1;
    end;
    inc(FBlocks[n-1].Count);
  end;
  for n:=0 to High(FBlocks) do begin
    first := FBlocks[n].First;
    SetLength(FBlocks[n].Cands, FBlocks[n].Count);
    for c:=0 to FBlocks[n].Count-1 do
      FBlocks[n].Cands[c] := FCandidates[first+c];
    if not FBlocks[n].IsArea then
      FBlocks[n].SizeStr := SizeText(FBlocks[n].Cands, FBlocks[n].Key, true);
  end;

  RebuildTree;
  UpdateStatus;
end;

end.
