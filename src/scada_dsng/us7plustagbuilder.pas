{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit do assistente de importação de tags do S7CommPlus (S7-1200/1500).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A lista de variáveis é lida @bold(online), do CLP: TS7CommPlusDriver.ListDataBlocks
  para saber quais DBs existem e TS7CommPlusDriver.BrowseDB para as variáveis de cada
  um (mais as áreas I, Q e M, se pedido). Isso tem duas consequências:

  @unorderedList(
    @item(A porta de comunicação do driver precisa estar ativa (Active=true) quando o
          assistente é aberto. Sem ela o assistente avisa e não lê nada.)
    @item(A leitura é síncrona: a IDE fica parada enquanto o CLP é navegado, o que num
          CLP com muitos DBs leva alguns segundos.)
  )

  As variáveis aparecem numa árvore: DB (ou área), depois as estruturas e as variáveis
  dentro delas, como no TIA Portal. Marcar um DB ou uma estrutura marca todas as
  variáveis que viram tag dentro dele (a marca fica parcial quando só algumas estão).

  As regras de quem vira tag estão em S7PlusTagPlan (e são testadas sem o IDE): só as
  variáveis escalares viram TPLCTagNumber, já com TagType e Swap* certos para o
  big-endian do CLP. Estruturas, STRINGs, datas e elementos de array não viram tag;
  com "Mostrar as variáveis que não viram tag" elas aparecem na lista, desabilitadas
  e com o motivo. O endereço do tag (LongAddress) é o caminho simbólico, com o nome do
  DB do TIA Portal (BlocoSimbolico_4.Var26.v1) ou o número (DB4.Var26.v1).

  Tempo de atualização 0 cria tags por mudança: o driver as entrega por subscrição.
}
{$ELSE}
{:
  @abstract(Unit of the S7CommPlus (S7-1200/1500) tag import wizard.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The variable list is read @bold(online), from the PLC: TS7CommPlusDriver.ListDataBlocks
  tells which DBs exist and TS7CommPlusDriver.BrowseDB gives the variables of each one
  (plus the I, Q and M areas, if asked). This has two consequences:

  @unorderedList(
    @item(The communication port of the driver must be active (Active=true) when the
          wizard is opened. Without it the wizard warns and reads nothing.)
    @item(The read is synchronous: the IDE is frozen while the PLC is browsed, which on
          a PLC with many DBs takes a few seconds.)
  )

  The variables show on a tree: DB (or area), then the structures and the variables
  inside them, as in TIA Portal. Checking a DB or a structure checks every variable
  that becomes a tag inside it (the check is partial when only some are).

  The rules of what becomes a tag are in S7PlusTagPlan (and are tested without the
  IDE): only scalar variables become a TPLCTagNumber, already with the right TagType and
  Swap* for the PLC's big-endian. Structures, STRINGs, dates and array elements do not
  become a tag; with "Show the variables that do not become a tag" they show on the
  list, disabled and with the reason. The tag address (LongAddress) is the symbolic
  path, with the TIA Portal DB name (BlocoSimbolico_4.Var26.v1) or the number
  (DB4.Var26.v1).

  Update rate 0 creates on-change tags: the driver delivers them by subscription.
}
{$ENDIF}
unit us7plustagbuilder;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LResources,{$ENDIF}
  Classes, SysUtils, StrUtils, Types, Contnrs, Graphics, ImgList, Forms, Controls,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Spin, Tag, ProtocolTypes, S7PlusTypes, S7PlusConnection, S7PlusTypeInfo, S7PlusTagPlan,
  S7PlusFamily;

type

  { TS7PlusNodeInfo }

  //what a tree node stands for: a variable that became a tag candidate (Index>=0) or just a
  //level of the path (a DB, a structure)
  TS7PlusNodeInfo = class
  public
    Index:LongInt;
    Key:String;      //the path up to this level, to restore what was expanded
  end;

  { TfrmS7PlusTagBuilder }

  TfrmS7PlusTagBuilder = class(TForm)
    btnBrowse: TButton;
    btnCancel: TButton;
    btnCheckAll: TButton;
    btnCollapseAll: TButton;
    btnExpandAll: TButton;
    btnInvert: TButton;
    btnOK: TButton;
    btnUncheckAll: TButton;
    chkIncludeAreas: TCheckBox;
    chkShowSkipped: TCheckBox;
    chkSymbolicNames: TCheckBox;
    edtFilter: TEdit;
    edtPrefix: TEdit;
    lblFilter: TLabel;
    lblPrefix: TLabel;
    lblScan: TLabel;
    lblSelected: TLabel;
    lblStatus: TLabel;
    pnlBottom: TPanel;
    pnlButtons: TPanel;
    pnlTop: TPanel;
    spinScan: TSpinEdit;
    tvTags: TTreeView;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnCollapseAllClick(Sender: TObject);
    procedure btnExpandAllClick(Sender: TObject);
    procedure btnInvertClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnUncheckAllClick(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvTagsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tvTagsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FDriver:TS7CommPlusDriver;
    FCandidates:TS7PlusCandidateArray;
    FChecked:array of Boolean;
    FInfos:TObjectList;          //owns the TS7PlusNodeInfo of the nodes
    FStateImages:TImageList;     //0 = unchecked, 1 = checked, 2 = partially checked
    FShownTags:LongInt;          //taggable variables on the tree now
    FBlocksRead:LongInt;
    FSkippedCount:LongInt;
    FDriverLog:TStringList;      //what the driver said while the wizard talked to the PLC
    procedure DriverDebug(Sender:TObject; const Msg:String);
    function  WaitForConnection(aTimeoutMs:LongInt):Boolean;
    procedure MakeStateImages;
    function  InfoOf(aNode:TTreeNode):TS7PlusNodeInfo;
    function  IsTaggable(aNode:TTreeNode):Boolean;
    procedure RebuildTree;
    procedure RestoreExpansion(aExpanded:TStringList; aExpandAll:Boolean);
    procedure SaveExpansion(aExpanded:TStringList);
    procedure UpdateStates;
    function  UpdateNodeState(aNode:TTreeNode):LongInt;
    procedure SetBranch(aNode:TTreeNode; aValue:Boolean);
    function  BranchAllChecked(aNode:TTreeNode):Boolean;
    procedure ToggleNode(aNode:TTreeNode);
    procedure SetAllShown(aMode:LongInt);
    procedure UpdateSelectedCount;
    function  PassFilters(aIndex:LongInt):Boolean;
    function  Decoration(aIndex:LongInt):String;
    procedure AddVars(const Vars:TS7PlusVarInfoArray; const aBlockName:String);
  public
    {$IFDEF PORTUGUES}
    //: Driver de protocolo de onde as variáveis serão lidas.
    {$ELSE}
    //: Protocol driver from where the variables will be read.
    {$ENDIF}
    property Driver:TS7CommPlusDriver read FDriver write FDriver;

    {$IFDEF PORTUGUES}
    //: Devolve a lista de tags que devem ser criados no formulário/datamodule.
    {$ELSE}
    //: Returns the list of tags that must be created on the form/datamodule.
    {$ENDIF}
    function GetImportPlan:TS7PlusImportItems;

    {$IFDEF PORTUGUES}
    //: Carrega no assistente variáveis já lidas do CLP (sem passar pelo driver).
    {$ELSE}
    //: Loads on the wizard variables already read from the PLC (bypassing the driver).
    {$ENDIF}
    procedure LoadCandidates(const aCandidates:TS7PlusCandidateArray; aBlocksRead:LongInt);
  end;

implementation

uses hsstrings;

{$IFDEF FPC}
  {$R us7plustagbuilder.lfm}
{$ENDIF}

const
  STATE_UNCHECKED = 0;
  STATE_CHECKED   = 1;
  STATE_PARTIAL   = 2;

{ TfrmS7PlusTagBuilder }

procedure TfrmS7PlusTagBuilder.FormCreate(Sender: TObject);
begin
  FInfos:=TObjectList.Create(true);
  FDriverLog:=TStringList.Create;
  FBlocksRead:=0;
  FSkippedCount:=0;
  FShownTags:=0;
  MakeStateImages;
  tvTags.StateImages:=FStateImages;

  Caption                     := SS7PTBCaption;
  btnBrowse.Caption           := SS7PTBBrowse;
  chkSymbolicNames.Caption    := SS7PTBSymbolicNames;
  chkIncludeAreas.Caption     := SS7PTBIncludeAreas;
  lblFilter.Caption           := SS7PTBFilter;
  btnExpandAll.Caption        := SS7PTBExpandAll;
  btnCollapseAll.Caption      := SS7PTBCollapseAll;
  chkShowSkipped.Caption      := SS7PTBShowSkipped;
  lblScan.Caption             := SS7PTBScan;
  lblPrefix.Caption           := SS7PTBPrefix;
  btnCheckAll.Caption         := SS7PTBCheckAll;
  btnUncheckAll.Caption       := SS7PTBUncheckAll;
  btnInvert.Caption           := SS7PTBInvert;
  btnOK.Caption               := SS7PTBImport;
  btnCancel.Caption           := SS7PTBCancel;

  lblStatus.Caption:=SS7PTBReadFirst;
  UpdateSelectedCount;
end;

procedure TfrmS7PlusTagBuilder.FormDestroy(Sender: TObject);
begin
  //the nodes still point to the infos: let go of them first
  tvTags.Items.Clear;
  FInfos.Free;
  FDriverLog.Free;
end;

//the three check box images are drawn here, so the wizard needs no resource
procedure TfrmS7PlusTagBuilder.MakeStateImages;
var
  bmp:TBitmap;
  state:LongInt;
begin
  FStateImages:=TImageList.Create(Self);
  FStateImages.Width:=16;
  FStateImages.Height:=16;

  for state:=STATE_UNCHECKED to STATE_PARTIAL do begin
    bmp:=TBitmap.Create;
    try
      bmp.SetSize(16, 16);
      bmp.Canvas.Brush.Color:=clWhite;
      bmp.Canvas.Pen.Color:=clGray;
      bmp.Canvas.Rectangle(2, 2, 14, 14);
      case state of
        STATE_CHECKED: begin
          bmp.Canvas.Pen.Color:=clBlack;
          bmp.Canvas.Pen.Width:=2;
          bmp.Canvas.MoveTo(4, 8);
          bmp.Canvas.LineTo(7, 11);
          bmp.Canvas.LineTo(12, 5);
        end;
        STATE_PARTIAL: begin
          bmp.Canvas.Brush.Color:=clGray;
          bmp.Canvas.Pen.Color:=clGray;
          bmp.Canvas.FillRect(5, 5, 11, 11);
        end;
      end;
      FStateImages.Add(bmp, nil);
    finally
      bmp.Free;
    end;
  end;
end;

function TfrmS7PlusTagBuilder.InfoOf(aNode: TTreeNode): TS7PlusNodeInfo;
begin
  Result:=TS7PlusNodeInfo(aNode.Data);
end;

//a node is something the user can check when it is a variable that becomes a tag
function TfrmS7PlusTagBuilder.IsTaggable(aNode: TTreeNode): Boolean;
var
  info:TS7PlusNodeInfo;
begin
  info:=InfoOf(aNode);
  Result:=Assigned(info) and (info.Index>=0) and (FCandidates[info.Index].Skip=srNone);
end;

function TfrmS7PlusTagBuilder.PassFilters(aIndex: LongInt): Boolean;
var
  aFilter:String;
begin
  Result:=false;

  //structures are never listed on their own: they show as the level that holds their members
  if FCandidates[aIndex].Skip=srNotAScalar then exit;
  if (FCandidates[aIndex].Skip<>srNone) and (not chkShowSkipped.Checked) then exit;

  aFilter:=Trim(edtFilter.Text);
  if (aFilter<>'') and (Pos(LowerCase(aFilter),LowerCase(FCandidates[aIndex].Path))=0) then exit;

  Result:=true;
end;

//what follows the name on the tree: the PLC type, and why a variable is not a tag
function TfrmS7PlusTagBuilder.Decoration(aIndex: LongInt): String;
begin
  Result:=' : '+S7PlusSoftTypeName(FCandidates[aIndex].SoftDataType);
  case FCandidates[aIndex].Skip of
    srUnsupportedType: Result:=Result+'   ('+SS7PTBSkipUnsupportedType+')';
    srArrayElement:    Result:=Result+'   ('+SS7PTBSkipArrayElement+')';
  end;
end;

procedure TfrmS7PlusTagBuilder.SaveExpansion(aExpanded: TStringList);
var
  c: LongInt;
  n: TTreeNode;
begin
  aExpanded.Clear;
  for c:=0 to tvTags.Items.Count-1 do begin
    n:=tvTags.Items[c];
    if n.Expanded and Assigned(InfoOf(n)) then
      aExpanded.Add(InfoOf(n).Key);
  end;
end;

procedure TfrmS7PlusTagBuilder.RestoreExpansion(aExpanded: TStringList;
  aExpandAll: Boolean);
var
  c: LongInt;
  n: TTreeNode;
begin
  for c:=0 to tvTags.Items.Count-1 do begin
    n:=tvTags.Items[c];
    if not n.HasChildren then continue;
    if aExpandAll or (aExpanded.IndexOf(InfoOf(n).Key)>=0) then
      n.Expanded:=true;
  end;
end;

procedure TfrmS7PlusTagBuilder.RebuildTree;
var
  c, k, p: LongInt;
  levels: TStringArray;
  structIdx: TStringList;     //path of a structure -> candidate index
  nodes: TStringList;         //path of a level -> its node
  aParent, node: TTreeNode;
  info: TS7PlusNodeInfo;
  key, aText: String;
  expanded: TStringList;
begin
  expanded:=TStringList.Create;
  structIdx:=TStringList.Create;
  nodes:=TStringList.Create;
  try
    structIdx.Sorted:=true;
    structIdx.Duplicates:=dupIgnore;
    nodes.Sorted:=true;
    nodes.Duplicates:=dupIgnore;

    SaveExpansion(expanded);

    tvTags.Items.BeginUpdate;
    try
      tvTags.Items.Clear;
      FInfos.Clear;
      FShownTags:=0;

      //the type of a structure is shown on its level
      for c:=0 to High(FCandidates) do
        if FCandidates[c].Skip=srNotAScalar then
          structIdx.AddObject(FCandidates[c].Path, TObject(PtrInt(c)));

      for c:=0 to High(FCandidates) do begin
        if not PassFilters(c) then continue;

        levels:=S7PlusSplitPath(FCandidates[c].Path);
        aParent:=nil;
        key:='';
        for k:=0 to High(levels) do begin
          if key<>'' then key:=key+'.';
          key:=key+levels[k];

          if nodes.Find(key, p) then
            node:=TTreeNode(nodes.Objects[p])
          else begin
            info:=TS7PlusNodeInfo.Create;
            info.Key:=key;
            info.Index:=-1;
            FInfos.Add(info);

            aText:=levels[k];
            if k<High(levels) then begin
              //a level above the variable: a DB/area or a structure
              if structIdx.Find(key, p) then
                aText:=aText+' : '+S7PlusSoftTypeName(FCandidates[PtrInt(structIdx.Objects[p])].SoftDataType);
            end else begin
              info.Index:=c;
              aText:=aText+Decoration(c);
              inc(FShownTags, Ord(FCandidates[c].Skip=srNone));
            end;

            node:=tvTags.Items.AddChild(aParent, aText);
            node.Data:=info;
            nodes.AddObject(key, node);
          end;
          aParent:=node;
        end;
      end;
    finally
      tvTags.Items.EndUpdate;
    end;

    //a filter shows what it found; without one, only what the user had opened stays open
    RestoreExpansion(expanded, Trim(edtFilter.Text)<>'');
    UpdateStates;
  finally
    nodes.Free;
    structIdx.Free;
    expanded.Free;
  end;
end;

//state of a node (and, going down, of everything under it): returns STATE_*, or -1 when
//there is nothing there the user can check
function TfrmS7PlusTagBuilder.UpdateNodeState(aNode: TTreeNode): LongInt;
var
  child: TTreeNode;
  childState, checked, unchecked: LongInt;
  info: TS7PlusNodeInfo;
begin
  info:=InfoOf(aNode);

  if not aNode.HasChildren then begin
    if IsTaggable(aNode) then begin
      if FChecked[info.Index] then Result:=STATE_CHECKED else Result:=STATE_UNCHECKED;
    end else
      Result:=-1;
    aNode.StateIndex:=Result;
    exit;
  end;

  checked:=0;
  unchecked:=0;
  child:=aNode.GetFirstChild;
  while Assigned(child) do begin
    childState:=UpdateNodeState(child);
    case childState of
      STATE_CHECKED:   inc(checked);
      STATE_UNCHECKED: inc(unchecked);
      STATE_PARTIAL:   begin inc(checked); inc(unchecked); end;
    end;
    child:=child.GetNextSibling;
  end;

  if (checked=0) and (unchecked=0) then Result:=-1
  else if unchecked=0 then Result:=STATE_CHECKED
  else if checked=0 then Result:=STATE_UNCHECKED
  else Result:=STATE_PARTIAL;

  aNode.StateIndex:=Result;
end;

procedure TfrmS7PlusTagBuilder.UpdateStates;
var
  n: TTreeNode;
begin
  n:=tvTags.Items.GetFirstNode;
  while Assigned(n) do begin
    UpdateNodeState(n);
    n:=n.GetNextSibling;
  end;
  tvTags.Invalidate;
  UpdateSelectedCount;
end;

function TfrmS7PlusTagBuilder.BranchAllChecked(aNode: TTreeNode): Boolean;
var
  child: TTreeNode;
begin
  Result:=true;
  if not aNode.HasChildren then begin
    if IsTaggable(aNode) then Result:=FChecked[InfoOf(aNode).Index];
    exit;
  end;
  child:=aNode.GetFirstChild;
  while Assigned(child) do begin
    if not BranchAllChecked(child) then begin
      Result:=false;
      exit;
    end;
    child:=child.GetNextSibling;
  end;
end;

procedure TfrmS7PlusTagBuilder.SetBranch(aNode: TTreeNode; aValue: Boolean);
var
  child: TTreeNode;
begin
  if not aNode.HasChildren then begin
    if IsTaggable(aNode) then FChecked[InfoOf(aNode).Index]:=aValue;
    exit;
  end;
  child:=aNode.GetFirstChild;
  while Assigned(child) do begin
    SetBranch(child, aValue);
    child:=child.GetNextSibling;
  end;
end;

//a variable flips; a DB or structure checks everything under it, or clears it when it
//was all checked already
procedure TfrmS7PlusTagBuilder.ToggleNode(aNode: TTreeNode);
begin
  if aNode.StateIndex<0 then exit;
  SetBranch(aNode, not BranchAllChecked(aNode));
  UpdateStates;
end;

procedure TfrmS7PlusTagBuilder.tvTagsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  n: TTreeNode;
begin
  if Button<>mbLeft then exit;
  if not (htOnStateIcon in tvTags.GetHitTestInfoAt(X, Y)) then exit;
  n:=tvTags.GetNodeAt(X, Y);
  if Assigned(n) then ToggleNode(n);
end;

procedure TfrmS7PlusTagBuilder.tvTagsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=32) and Assigned(tvTags.Selected) then begin
    ToggleNode(tvTags.Selected);
    Key:=0;
  end;
end;

//aMode: 0 = check, 1 = uncheck, 2 = invert - the variables on the tree now
procedure TfrmS7PlusTagBuilder.SetAllShown(aMode: LongInt);
var
  c: LongInt;
  n: TTreeNode;
begin
  for c:=0 to tvTags.Items.Count-1 do begin
    n:=tvTags.Items[c];
    if (not n.HasChildren) and IsTaggable(n) then
      case aMode of
        0: FChecked[InfoOf(n).Index]:=true;
        1: FChecked[InfoOf(n).Index]:=false;
        2: FChecked[InfoOf(n).Index]:=not FChecked[InfoOf(n).Index];
      end;
  end;
  UpdateStates;
end;

procedure TfrmS7PlusTagBuilder.UpdateSelectedCount;
var
  c, total: LongInt;
begin
  total:=0;
  for c:=0 to High(FChecked) do
    if FChecked[c] then inc(total);

  lblSelected.Caption:=Format(SS7PTBSelectedCount,[total, FShownTags]);
  btnOK.Enabled:=total>0;
end;

procedure TfrmS7PlusTagBuilder.AddVars(const Vars: TS7PlusVarInfoArray;
  const aBlockName: String);
var
  c, n: LongInt;
begin
  for c:=0 to High(Vars) do begin
    //the DB/area root itself ("DB4") is not a variable
    if Pos('.', Vars[c].Name)=0 then continue;
    n:=Length(FCandidates);
    SetLength(FCandidates, n+1);
    FCandidates[n]:=S7PlusClassifyVar(Vars[c], aBlockName);
    //a structure is not "skipped": its members are the tags
    if not (FCandidates[n].Skip in [srNone, srNotAScalar]) then inc(FSkippedCount);
  end;
end;

procedure TfrmS7PlusTagBuilder.DriverDebug(Sender: TObject; const Msg: String);
begin
  //only the last lines matter: they say why a connection or a read failed
  FDriverLog.Add(Msg);
  while FDriverLog.Count>12 do FDriverLog.Delete(0);
end;

//a port that was just opened (the form was loaded a moment ago) may still be connecting
function TfrmS7PlusTagBuilder.WaitForConnection(aTimeoutMs: LongInt): Boolean;
var
  waited:LongInt;
begin
  waited:=0;
  while (not FDriver.CommunicationPort.ReallyActive) and (waited<aTimeoutMs) do begin
    Sleep(50);
    Application.ProcessMessages;
    inc(waited, 50);
  end;
  Result:=FDriver.CommunicationPort.ReallyActive;
end;

procedure TfrmS7PlusTagBuilder.btnBrowseClick(Sender: TObject);
var
  oldCursor:TCursor;
  oldDebug:TS7PlusDebugEvent;
  blocks:TS7PlusDataBlockInfoArray;
  vars:TS7PlusVarInfoArray;
  c:LongInt;
  aName:String;
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
  FBlocksRead:=0;
  FSkippedCount:=0;

  oldCursor:=Screen.Cursor;
  Screen.Cursor:=crHourGlass;
  oldDebug:=FDriver.OnDebug;
  FDriverLog.Clear;
  FDriver.OnDebug:=@DriverDebug;
  try
    lblStatus.Caption:=SS7PTBReading;
    lblStatus.Update;

    //Active only says the port was asked to open: at design time the socket may not be there
    if not WaitForConnection(5000) then begin
      Screen.Cursor:=oldCursor;
      MessageDlg(SS7PTBPortNotConnected, mtError, [mbOK], 0);
      lblStatus.Caption:=SS7PTBNothingRead;
      exit;
    end;

    if FDriver.ListDataBlocks(blocks)<>ioOk then begin
      Screen.Cursor:=oldCursor;
      MessageDlg(SS7PTBBrowseFailed+LineEnding+LineEnding+SS7PTBDetails+LineEnding+
                 FDriverLog.Text, mtError, [mbOK], 0);
      lblStatus.Caption:=SS7PTBNothingRead;
      exit;
    end;

    for c:=0 to High(blocks) do begin
      if FDriver.BrowseDB(blocks[c].Number, vars)<>ioOk then continue;
      if chkSymbolicNames.Checked then aName:=blocks[c].Name else aName:='';
      AddVars(vars, aName);
      inc(FBlocksRead);
    end;

    if chkIncludeAreas.Checked then begin
      if FDriver.BrowseNativeArea(S7PlusIds_NativeIAreaRID, S7PlusTiRid_IArea, 'IArea', vars)=ioOk then begin
        AddVars(vars, ''); inc(FBlocksRead);
      end;
      if FDriver.BrowseNativeArea(S7PlusIds_NativeQAreaRID, S7PlusTiRid_QArea, 'QArea', vars)=ioOk then begin
        AddVars(vars, ''); inc(FBlocksRead);
      end;
      if FDriver.BrowseNativeArea(S7PlusIds_NativeMAreaRID, S7PlusTiRid_MArea, 'MArea', vars)=ioOk then begin
        AddVars(vars, ''); inc(FBlocksRead);
      end;
    end;
  finally
    FDriver.OnDebug:=oldDebug;
    Screen.Cursor:=oldCursor;
  end;

  LoadCandidates(FCandidates, FBlocksRead);
end;

procedure TfrmS7PlusTagBuilder.LoadCandidates(const aCandidates: TS7PlusCandidateArray;
  aBlocksRead: LongInt);
var
  c:LongInt;
begin
  FCandidates:=aCandidates;
  FBlocksRead:=aBlocksRead;

  FSkippedCount:=0;
  for c:=0 to High(FCandidates) do
    if not (FCandidates[c].Skip in [srNone, srNotAScalar]) then inc(FSkippedCount);

  SetLength(FChecked, Length(FCandidates));
  for c:=0 to High(FChecked) do
    FChecked[c]:=false;

  RebuildTree;

  if Length(FCandidates)=0 then
    lblStatus.Caption:=SS7PTBNothingRead
  else
    lblStatus.Caption:=Format(SS7PTBVarsRead,[Length(FCandidates), FBlocksRead, FSkippedCount]);
end;

procedure TfrmS7PlusTagBuilder.btnCheckAllClick(Sender: TObject);
begin
  SetAllShown(0);
end;

procedure TfrmS7PlusTagBuilder.btnUncheckAllClick(Sender: TObject);
begin
  SetAllShown(1);
end;

procedure TfrmS7PlusTagBuilder.btnInvertClick(Sender: TObject);
begin
  SetAllShown(2);
end;

procedure TfrmS7PlusTagBuilder.btnExpandAllClick(Sender: TObject);
begin
  tvTags.FullExpand;
end;

procedure TfrmS7PlusTagBuilder.btnCollapseAllClick(Sender: TObject);
begin
  tvTags.FullCollapse;
end;

procedure TfrmS7PlusTagBuilder.FilterChanged(Sender: TObject);
begin
  RebuildTree;
end;

procedure TfrmS7PlusTagBuilder.btnOKClick(Sender: TObject);
begin
  UpdateSelectedCount;
  if not btnOK.Enabled then begin
    MessageDlg(SS7PTBSelectAtLeastOne, mtInformation, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOK;
end;

function TfrmS7PlusTagBuilder.GetImportPlan: TS7PlusImportItems;
var
  c, count: LongInt;
begin
  Result:=nil;
  SetLength(Result, Length(FCandidates));
  count:=0;

  for c:=0 to High(FCandidates) do begin
    if (c>High(FChecked)) or (not FChecked[c]) then continue;
    if FCandidates[c].Skip<>srNone then continue;

    Result[count].Path      := FCandidates[c].Path;
    Result[count].CompName  := S7PlusSuggestedName(FCandidates[c].Path, edtPrefix.Text);
    Result[count].TagType   := FCandidates[c].TagType;
    Result[count].SwapBytes := FCandidates[c].SwapBytes;
    Result[count].SwapWords := FCandidates[c].SwapWords;
    Result[count].SwapDWords:= FCandidates[c].SwapDWords;
    Result[count].Scan      := spinScan.Value;
    inc(count);
  end;

  SetLength(Result, count);
end;

end.
