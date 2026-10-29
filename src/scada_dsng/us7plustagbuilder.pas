{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit do assistente de importação de tags do S7CommPlus (S7-1200/1500).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A lista de variáveis é lida @bold(online), do CLP: TS7CommPlusDriver.ListDataBlocks
  para saber quais DBs existem (mais as áreas I, Q e M, se pedido) e
  TS7CommPlusDriver.BrowseDB para as variáveis de cada um. Só o primeiro nível (os DBs e as
  áreas) é lido ao clicar em "Ler as variáveis do CLP"; as variáveis de um DB são lidas
  quando ele é aberto na árvore (no +) ou marcado. Isso tem três consequências:

  @unorderedList(
    @item(A porta de comunicação do driver precisa estar ativa (Active=true) quando o
          assistente é aberto. Sem ela o assistente avisa e não lê nada.)
    @item(A leitura é síncrona: a IDE fica parada enquanto o CLP é navegado, mas só pelo
          tempo de um DB de cada vez.)
    @item(O filtro e "Expandir tudo" valem para os DBs já carregados.)
  )

  As variáveis aparecem numa árvore: DB (ou área), depois as estruturas e as variáveis
  dentro delas, como no TIA Portal. Marcar um DB ou uma estrutura marca todas as
  variáveis que viram tag dentro dele (a marca fica parcial quando só algumas estão).

  Um array de estruturas aparece como um tag só (o array todo, um TPLCStruct) e, sob ele,
  cada elemento (Arr[0], Arr[1]...) como um nível da árvore, com os membros: cada membro de
  um elemento, em qualquer profundidade (inclusive arrays de estruturas dentro dos elementos),
  pode virar um tag solto, lido pelo caminho simbólico.

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
  tells which DBs exist (plus the I, Q and M areas, if asked) and
  TS7CommPlusDriver.BrowseDB gives the variables of each one. Only the first level (the DBs
  and the areas) is read when "Read the variables from the PLC" is pressed; the variables of
  a DB are read when it is opened on the tree (the +) or checked. This has three
  consequences:

  @unorderedList(
    @item(The communication port of the driver must be active (Active=true) when the
          wizard is opened. Without it the wizard warns and reads nothing.)
    @item(The read is synchronous: the IDE is frozen while the PLC is browsed, but only
          for the time of one DB at a time.)
    @item(The filter and "Expand all" work on the DBs already loaded.)
  )

  The variables show on a tree: DB (or area), then the structures and the variables
  inside them, as in TIA Portal. Checking a DB or a structure checks every variable
  that becomes a tag inside it (the check is partial when only some are).

  An array of structures shows as one tag (the whole array, a TPLCStruct) and, under it, each
  element (Arr[0], Arr[1]...) as a level of the tree, with its members: every member of an
  element, at any depth (arrays of structures inside the elements included), can become a
  loose tag read by its symbolic path.

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
    Block:LongInt;   //>=0: the node is the root of that block (a DB or an area)
    Key:String;      //the path up to this level, to restore what was expanded
  end;

  //a DB or an area of the first level: its variables are only read when it is opened
  TS7PlusBlockEntry = record
    Key:String;                 //first level of the paths of its variables (DB name or number, area)
    IsArea:Boolean;
    Number:Cardinal;            //DB number
    DBName:String;              //name given to the paths ('' = the DB<number> the PLC uses)
    AreaRID, AreaTiRid:Cardinal;
    AreaName:String;
    AccessArea:Cardinal;
    Loaded:Boolean;
    First, Count:LongInt;       //its variables on FCandidates
    Cands:TS7PlusCandidateArray;//the same, to measure the structures of this block only
    SizeStr:String;             //size of a DB, once loaded
  end;

  { TfrmS7PlusTagBuilder }

  TfrmS7PlusTagBuilder = class(TForm)
    btnBrowse: TButton;
    pbProgress: TProgressBar;
    btnCancel: TButton;
    btnCheckAll: TButton;
    btnCollapseAll: TButton;
    btnExpandAll: TButton;
    btnInvert: TButton;
    btnOK: TButton;
    btnUncheckAll: TButton;
    chkCreateElements: TCheckBox;
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
    procedure tvTagsExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
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
    FBlocks:array of TS7PlusBlockEntry;
    FSymbolic:Boolean;           //the DB names of the list read, as asked when it was read
    FLoading:Boolean;            //a block is being read
    FSkippedCount:LongInt;
    FDriverLog:TStringList;      //what the driver said while the wizard talked to the PLC
    procedure DriverDebug(Sender:TObject; const Msg:String);
    function  WaitForConnection(aTimeoutMs:LongInt):Boolean;
    procedure MakeStateImages;
    function  InfoOf(aNode:TTreeNode):TS7PlusNodeInfo;
    function  IsTaggable(aNode:TTreeNode):Boolean;
    procedure RebuildTree;
    procedure BuildBlock(aBlock:LongInt; aRoot:TTreeNode);
    function  LoadBlock(aBlock:LongInt; aRoot:TTreeNode):Boolean;
    function  IsPending(aNode:TTreeNode):Boolean;
    function  RootText(aBlock:LongInt):String;
    procedure UpdateStatus;
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
    function  SizeText(const aCands:TS7PlusCandidateArray; const aPath:String; aIsDBRoot:Boolean):String;
    function  AreaLabel(const aArea:String):String;
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
  FSkippedCount:=0;
  FShownTags:=0;
  MakeStateImages;
  //without this the LCL frees the children of a node when it is collapsed: the node then has
  //nothing under it (no + sign, nothing to check) until the tree is built again
  tvTags.Items.KeepCollapsedNodes:=true;
  tvTags.StateImages:=FStateImages;

  Caption                     := SS7PTBCaption;
  btnBrowse.Caption           := SS7PTBBrowse;
  chkSymbolicNames.Caption    := SS7PTBSymbolicNames;
  chkIncludeAreas.Caption     := SS7PTBIncludeAreas;
  lblFilter.Caption           := SS7PTBFilter;
  btnExpandAll.Caption        := SS7PTBExpandAll;
  btnCollapseAll.Caption      := SS7PTBCollapseAll;
  chkShowSkipped.Caption      := SS7PTBShowSkipped;
  chkCreateElements.Caption   := SS7PTBCreateElements;
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

//bytes of a DB or a structure, as the wizard shows them
function TfrmS7PlusTagBuilder.SizeText(const aCands: TS7PlusCandidateArray;
  const aPath: String; aIsDBRoot: Boolean): String;
var
  size: LongInt;
  exact: Boolean;
begin
  size:=S7PlusContainerSize(aCands, aPath, aIsDBRoot, exact);
  if exact then Result:=Format(SS7PTBBytes,[size])
  else Result:=Format(SS7PTBAtLeastBytes,[size]);
end;

function TfrmS7PlusTagBuilder.AreaLabel(const aArea: String): String;
begin
  if SameText(aArea,'IArea') then Result:=SS7PTBAreaInputs
  else if SameText(aArea,'QArea') then Result:=SS7PTBAreaOutputs
  else if SameText(aArea,'MArea') then Result:=SS7PTBAreaFlags
  else if SameText(aArea,'S7Timers') then Result:=SS7PTBAreaTimers
  else if SameText(aArea,'S7Counters') then Result:=SS7PTBAreaCounters
  else Result:='';
end;

//what follows the name on the tree: the PLC type, and why a variable is not a tag
function TfrmS7PlusTagBuilder.Decoration(aIndex: LongInt): String;
begin
  Result:=' : '+S7PlusSoftTypeName(FCandidates[aIndex].SoftDataType);
  if FCandidates[aIndex].IsString and (FCandidates[aIndex].StringLength>=0) then
    Result:=Result+'['+IntToStr(FCandidates[aIndex].StringLength)+']';
  if FCandidates[aIndex].IsArray then
    Result:=Result+'['+IntToStr(FCandidates[aIndex].ElementCount)+']';
  if FCandidates[aIndex].IsStructArray then
    Result:=Result+'['+IntToStr(FCandidates[aIndex].ElementCount)+'] ('+
            Format(SS7PTBBytes,[FCandidates[aIndex].ElementCount*FCandidates[aIndex].ElementSize])+')';
  case FCandidates[aIndex].Skip of
    srUnsupportedType: Result:=Result+'   ('+SS7PTBSkipUnsupportedType+')';
    srArrayElement:    Result:=Result+'   ('+SS7PTBSkipArrayElement+')';
    srStringLength:    Result:=Result+'   ('+SS7PTBSkipStringLength+')';
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
    if (not n.HasChildren) or IsPending(n) then continue;
    if aExpandAll or (aExpanded.IndexOf(InfoOf(n).Key)>=0) then
      n.Expanded:=true;
  end;
end;

//a node of a block whose variables were not read yet: it has one placeholder child, to show the +
function TfrmS7PlusTagBuilder.IsPending(aNode: TTreeNode): Boolean;
var
  info:TS7PlusNodeInfo;
begin
  info:=InfoOf(aNode);
  Result:=Assigned(info) and (info.Block>=0) and (not FBlocks[info.Block].Loaded);
end;

function TfrmS7PlusTagBuilder.RootText(aBlock: LongInt): String;
begin
  Result:=FBlocks[aBlock].Key;
  if FBlocks[aBlock].IsArea then
    Result:=Result+' : '+AreaLabel(Result)
  else if FBlocks[aBlock].Loaded then
    Result:=Result+' : '+FBlocks[aBlock].SizeStr;
end;

//the first level is always there; the variables of a block hang from its node once it is read
procedure TfrmS7PlusTagBuilder.RebuildTree;
var
  c: LongInt;
  info: TS7PlusNodeInfo;
  node: TTreeNode;
  expanded: TStringList;
  aFilter: Boolean;
begin
  expanded:=TStringList.Create;
  try
    SaveExpansion(expanded);
    aFilter:=Trim(edtFilter.Text)<>'';

    tvTags.Items.BeginUpdate;
    try
      tvTags.Items.Clear;
      FInfos.Clear;
      FShownTags:=0;

      for c:=0 to High(FBlocks) do begin
        info:=TS7PlusNodeInfo.Create;
        info.Key:=FBlocks[c].Key;
        info.Index:=-1;
        info.Block:=c;
        FInfos.Add(info);

        node:=tvTags.Items.AddChild(nil, RootText(c));
        node.Data:=info;
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
    UpdateStates;
  finally
    expanded.Free;
  end;
end;

//the structures and variables of a block that was read, under its node
procedure TfrmS7PlusTagBuilder.BuildBlock(aBlock: LongInt; aRoot: TTreeNode);
var
  c, k, p: LongInt;
  levels: TStringArray;
  structIdx: TStringList;     //path of a structure -> candidate index
  nodes: TStringList;         //path of a level -> its node
  aParent, node: TTreeNode;
  info: TS7PlusNodeInfo;
  key, baseKey, aText: String;
begin
  structIdx:=TStringList.Create;
  nodes:=TStringList.Create;
  tvTags.Items.BeginUpdate;
  try
    structIdx.Sorted:=true;
    structIdx.Duplicates:=dupIgnore;
    nodes.Sorted:=true;
    nodes.Duplicates:=dupIgnore;
    nodes.AddObject(FBlocks[aBlock].Key, aRoot);

    //the type of a structure is shown on its level
    for c:=FBlocks[aBlock].First to FBlocks[aBlock].First+FBlocks[aBlock].Count-1 do
      if FCandidates[c].Skip=srNotAScalar then
        structIdx.AddObject(FCandidates[c].Path, TObject(PtrInt(c)));

    for c:=FBlocks[aBlock].First to FBlocks[aBlock].First+FBlocks[aBlock].Count-1 do begin
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
          //an element of an array (Arr[2]) hangs from the array (Arr), when that is on the tree
          if (levels[k]<>'') and (levels[k][Length(levels[k])]=']') and (Pos('[', levels[k])>1) then begin
            baseKey:=Copy(key, 1, Length(key)-Length(levels[k]))+Copy(levels[k], 1, Pos('[', levels[k])-1);
            if nodes.Find(baseKey, p) then aParent:=TTreeNode(nodes.Objects[p]);
          end;
          info:=TS7PlusNodeInfo.Create;
          info.Key:=key;
          info.Index:=-1;
          info.Block:=-1;
          FInfos.Add(info);

          aText:=levels[k];
          if k<High(levels) then begin
            //a level above the variable: a structure, with its size
            if structIdx.Find(key, p) then begin
              aText:=aText+' : '+S7PlusSoftTypeName(FCandidates[PtrInt(structIdx.Objects[p])].SoftDataType);
              //inside an array element there can be thousands of levels: no size for them
              if Pos('[', key)=0 then
                aText:=aText+' ('+SizeText(FBlocks[aBlock].Cands, key, false)+')';
            end;
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
    nodes.Free;
    structIdx.Free;
  end;
end;

//reads the variables of a block from the PLC and hangs them from its node (aRoot is the node
//being opened: it is the one that cannot be rebuilt). False when they could not be read.
function TfrmS7PlusTagBuilder.LoadBlock(aBlock: LongInt; aRoot: TTreeNode): Boolean;
var
  res:TProtocolIOResult;
  oldCursor:TCursor;
  oldDebug:TS7PlusDebugEvent;
  vars:TS7PlusVarInfoArray;
  local:TS7PlusCandidateArray;
  c, first:LongInt;
begin
  Result:=FBlocks[aBlock].Loaded;
  if Result or FLoading or (not Assigned(FDriver)) then exit;

  FLoading:=true;
  oldCursor:=Screen.Cursor;
  Screen.Cursor:=crHourGlass;
  oldDebug:=FDriver.OnDebug;
  FDriverLog.Clear;
  FDriver.OnDebug:=@DriverDebug;
  pbProgress.Style:=pbstMarquee;
  pbProgress.Visible:=true;
  btnBrowse.Enabled:=false;
  try
    lblStatus.Caption:=Format(SS7PTBReadingBlock,[FBlocks[aBlock].Key]);
    pbProgress.Repaint;
    Application.ProcessMessages;

    if FBlocks[aBlock].IsArea then
      res:=FDriver.BrowseNativeArea(FBlocks[aBlock].AreaRID, FBlocks[aBlock].AreaTiRid,
                                    FBlocks[aBlock].AreaName, vars)
    else
      res:=FDriver.BrowseDB(FBlocks[aBlock].Number, vars);

    if res<>ioOk then begin
      Screen.Cursor:=oldCursor;
      if res=ioAuthError then
        MessageDlg(SS7PTBAuthRequired, mtError, [mbOK], 0)
      else
        MessageDlg(Format(SS7PTBBlockFailed,[FBlocks[aBlock].Key])+LineEnding+LineEnding+
                   SS7PTBDetails+LineEnding+FDriverLog.Text, mtError, [mbOK], 0);
      UpdateStatus;
      exit;
    end;

    SetLength(local, 0);
    S7PlusAddBrowsed(local, vars, FBlocks[aBlock].DBName, FBlocks[aBlock].AccessArea);
    S7PlusMarkArrays(local);
    //the maximum length of each STRING is in its value: read them once
    lblStatus.Caption:=SS7PTBReadingStrings;
    Application.ProcessMessages;
    FDriver.ResolveStringLengths(local);

    first:=Length(FCandidates);
    SetLength(FCandidates, first+Length(local));
    SetLength(FChecked, Length(FCandidates));
    for c:=0 to High(local) do begin
      FCandidates[first+c]:=local[c];
      FChecked[first+c]:=false;
      if not (local[c].Skip in [srNone, srNotAScalar]) then inc(FSkippedCount);
    end;

    FBlocks[aBlock].First:=first;
    FBlocks[aBlock].Count:=Length(local);
    FBlocks[aBlock].Cands:=local;
    if not FBlocks[aBlock].IsArea then
      FBlocks[aBlock].SizeStr:=SizeText(local, FBlocks[aBlock].Key, true);
    FBlocks[aBlock].Loaded:=true;

    //the placeholder gives way to the variables
    aRoot.DeleteChildren;
    aRoot.Text:=RootText(aBlock);
    BuildBlock(aBlock, aRoot);
    UpdateStates;
    UpdateStatus;
    Result:=true;
  finally
    pbProgress.Visible:=false;
    btnBrowse.Enabled:=true;
    FDriver.OnDebug:=oldDebug;
    Screen.Cursor:=oldCursor;
    FLoading:=false;
  end;
end;

procedure TfrmS7PlusTagBuilder.tvTagsExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if IsPending(Node) then
    AllowExpansion:=LoadBlock(InfoOf(Node).Block, Node);
end;

procedure TfrmS7PlusTagBuilder.UpdateStatus;
var
  c, nRead: LongInt;
begin
  if Length(FBlocks)=0 then begin
    lblStatus.Caption:=SS7PTBNothingRead;
    exit;
  end;
  nRead:=0;
  for c:=0 to High(FBlocks) do
    if FBlocks[c].Loaded then inc(nRead);

  lblStatus.Caption:=Format(SS7PTBBlocksInfo,[Length(FBlocks), nRead, Length(FCandidates), FSkippedCount]);
  if nRead<Length(FBlocks) then begin
    lblStatus.Caption:=lblStatus.Caption+' '+SS7PTBOpenToRead;
    if Trim(edtFilter.Text)<>'' then
      lblStatus.Caption:=lblStatus.Caption+' '+SS7PTBFilterLoadedOnly;
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

  //a block not read yet: unchecked until it is (ticking it reads it)
  if IsPending(aNode) then begin
    Result:=STATE_UNCHECKED;
    aNode.StateIndex:=Result;
    exit;
  end;

  //an array is one tag whatever is listed below it (its elements, when shown)
  if IsTaggable(aNode) then begin
    child:=aNode.GetFirstChild;
    while Assigned(child) do begin
      UpdateNodeState(child);
      child:=child.GetNextSibling;
    end;
    if FChecked[info.Index] then Result:=STATE_CHECKED else Result:=STATE_UNCHECKED;
    aNode.StateIndex:=Result;
    exit;
  end;

  if not aNode.HasChildren then begin
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
  if IsTaggable(aNode) then begin
    Result:=FChecked[InfoOf(aNode).Index];
    exit;
  end;
  if not aNode.HasChildren then exit;
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
  if IsTaggable(aNode) then begin
    FChecked[InfoOf(aNode).Index]:=aValue;
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
  if IsPending(aNode) and (not LoadBlock(InfoOf(aNode).Block, aNode)) then exit;
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
    if IsTaggable(n) then
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

//what the read is doing now: the bar counts the DBs and areas, the text says which one
procedure TfrmS7PlusTagBuilder.btnBrowseClick(Sender: TObject);
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
  SetLength(FChecked, 0);
  SetLength(FBlocks, 0);
  FSkippedCount:=0;

  oldCursor:=Screen.Cursor;
  Screen.Cursor:=crHourGlass;
  oldDebug:=FDriver.OnDebug;
  FDriverLog.Clear;
  FDriver.OnDebug:=@DriverDebug;
  //busy from the start: connecting and listing the DBs have no count
  pbProgress.Style:=pbstMarquee;
  pbProgress.Visible:=true;
  btnBrowse.Enabled:=false;
  try
    lblStatus.Caption:=SS7PTBReading;
    Application.ProcessMessages;

    //Active only says the port was asked to open: at design time the socket may not be there
    if not WaitForConnection(5000) then begin
      Screen.Cursor:=oldCursor;
      MessageDlg(SS7PTBPortNotConnected, mtError, [mbOK], 0);
      lblStatus.Caption:=SS7PTBNothingRead;
      RebuildTree;
      exit;
    end;

    //a download to the PLC since the last read adds and changes DBs and types: read it all again
    FDriver.RefreshCaches;
    res:=FDriver.ListDataBlocks(blocks);
    if res<>ioOk then begin
      Screen.Cursor:=oldCursor;
      if res=ioAuthError then
        MessageDlg(SS7PTBAuthRequired, mtError, [mbOK], 0)
      else
        MessageDlg(SS7PTBBrowseFailed+LineEnding+LineEnding+SS7PTBDetails+LineEnding+
                   FDriverLog.Text, mtError, [mbOK], 0);
      lblStatus.Caption:=SS7PTBNothingRead;
      RebuildTree;
      exit;
    end;

    //only the first level: the variables of each one are read when it is opened
    FSymbolic:=chkSymbolicNames.Checked;
    n:=Length(blocks);
    if chkIncludeAreas.Checked then inc(n, 3);
    SetLength(FBlocks, n);
    for c:=0 to High(blocks) do begin
      FBlocks[c].Number:=blocks[c].Number;
      FBlocks[c].AccessArea:=S7PlusIds_DBAccessAreaBase+blocks[c].Number;
      if FSymbolic and (blocks[c].Name<>'') then begin
        FBlocks[c].Key:=blocks[c].Name;
        FBlocks[c].DBName:=blocks[c].Name;
      end else
        FBlocks[c].Key:='DB'+IntToStr(blocks[c].Number);
    end;
    if chkIncludeAreas.Checked then begin
      n:=Length(blocks);
      FBlocks[n].IsArea:=true;
      FBlocks[n].Key:='IArea';
      FBlocks[n].AreaName:='IArea';
      FBlocks[n].AreaRID:=S7PlusIds_NativeIAreaRID;
      FBlocks[n].AreaTiRid:=S7PlusTiRid_IArea;
      FBlocks[n].AccessArea:=S7PlusIds_NativeIAreaRID;
      FBlocks[n+1].IsArea:=true;
      FBlocks[n+1].Key:='QArea';
      FBlocks[n+1].AreaName:='QArea';
      FBlocks[n+1].AreaRID:=S7PlusIds_NativeQAreaRID;
      FBlocks[n+1].AreaTiRid:=S7PlusTiRid_QArea;
      FBlocks[n+1].AccessArea:=S7PlusIds_NativeQAreaRID;
      FBlocks[n+2].IsArea:=true;
      FBlocks[n+2].Key:='MArea';
      FBlocks[n+2].AreaName:='MArea';
      FBlocks[n+2].AreaRID:=S7PlusIds_NativeMAreaRID;
      FBlocks[n+2].AreaTiRid:=S7PlusTiRid_MArea;
      FBlocks[n+2].AccessArea:=S7PlusIds_NativeMAreaRID;
    end;
    RebuildTree;
    UpdateStatus;
  finally
    pbProgress.Visible:=false;
    btnBrowse.Enabled:=true;
    FDriver.OnDebug:=oldDebug;
    Screen.Cursor:=oldCursor;
  end;
end;

//Variables already read elsewhere: every DB (or area) on them is taken as read.
procedure TfrmS7PlusTagBuilder.LoadCandidates(const aCandidates: TS7PlusCandidateArray;
  aBlocksRead: LongInt);
var
  c, n, first:LongInt;
  levels:TStringArray;
begin
  FCandidates:=aCandidates;
  S7PlusMarkArrays(FCandidates);
  //the maximum length of each STRING is in its value: read them once
  if Assigned(FDriver) then
    FDriver.ResolveStringLengths(FCandidates);

  FSkippedCount:=0;
  SetLength(FBlocks, 0);
  SetLength(FChecked, Length(FCandidates));
  for c:=0 to High(FCandidates) do begin
    FChecked[c]:=false;
    if not (FCandidates[c].Skip in [srNone, srNotAScalar]) then inc(FSkippedCount);

    levels:=S7PlusSplitPath(FCandidates[c].Path);
    if Length(levels)=0 then continue;
    n:=Length(FBlocks);
    //the variables of a block come together
    if (n=0) or (FBlocks[n-1].Key<>levels[0]) then begin
      SetLength(FBlocks, n+1);
      FBlocks[n].Key:=levels[0];
      FBlocks[n].IsArea:=S7PlusIsAreaName(levels[0]);
      FBlocks[n].First:=c;
      FBlocks[n].Loaded:=true;
      n:=n+1;
    end;
    inc(FBlocks[n-1].Count);
  end;
  for n:=0 to High(FBlocks) do begin
    first:=FBlocks[n].First;
    SetLength(FBlocks[n].Cands, FBlocks[n].Count);
    for c:=0 to FBlocks[n].Count-1 do
      FBlocks[n].Cands[c]:=FCandidates[first+c];
    if not FBlocks[n].IsArea then
      FBlocks[n].SizeStr:=SizeText(FBlocks[n].Cands, FBlocks[n].Key, true);
  end;

  RebuildTree;
  UpdateStatus;
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

//only the blocks already read: opening the others would read the whole PLC
procedure TfrmS7PlusTagBuilder.btnExpandAllClick(Sender: TObject);
var
  none:TStringList;
begin
  none:=TStringList.Create;
  try
    RestoreExpansion(none, true);
  finally
    none.Free;
  end;
end;

procedure TfrmS7PlusTagBuilder.btnCollapseAllClick(Sender: TObject);
begin
  tvTags.FullCollapse;
end;

procedure TfrmS7PlusTagBuilder.FilterChanged(Sender: TObject);
begin
  RebuildTree;
  if Length(FBlocks)>0 then UpdateStatus;
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
begin
  //a DB becomes one structure (with an item per ticked variable); the areas stay loose tags
  Result:=S7PlusBuildImportPlan(FCandidates, FChecked, edtPrefix.Text, spinScan.Value,
                                chkCreateElements.Checked);
end;

end.
