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
  Classes, SysUtils, StrUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Spin, Tag, ProtocolTypes, S7PlusTypes, S7PlusTypeInfo, S7PlusTagPlan,
  S7PlusFamily;

type

  { TfrmS7PlusTagBuilder }

  TfrmS7PlusTagBuilder = class(TForm)
    btnBrowse: TButton;
    btnCancel: TButton;
    btnCheckAll: TButton;
    btnInvert: TButton;
    btnOK: TButton;
    btnUncheckAll: TButton;
    chkIncludeAreas: TCheckBox;
    chkShowSkipped: TCheckBox;
    chkShowStructMembers: TCheckBox;
    chkSymbolicNames: TCheckBox;
    edtFilter: TEdit;
    edtPrefix: TEdit;
    lblFilter: TLabel;
    lblPrefix: TLabel;
    lblScan: TLabel;
    lblSelected: TLabel;
    lblStatus: TLabel;
    lvTags: TListView;
    pnlBottom: TPanel;
    pnlButtons: TPanel;
    pnlTop: TPanel;
    spinScan: TSpinEdit;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnInvertClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnUncheckAllClick(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvTagsClick(Sender: TObject);
  private
    FDriver:TS7CommPlusDriver;
    FCandidates:TS7PlusCandidateArray;
    FChecked:array of Boolean;
    FVisible:array of LongInt;   //listview index -> FCandidates index
    FBlocksRead:LongInt;
    FSkippedCount:LongInt;
    procedure HarvestChecks;
    procedure RebuildList;
    procedure UpdateSelectedCount;
    function  PassFilters(aIndex:LongInt):Boolean;
    function  CreatedAs(aIndex:LongInt):String;
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

{ TfrmS7PlusTagBuilder }

procedure TfrmS7PlusTagBuilder.FormCreate(Sender: TObject);
begin
  FBlocksRead:=0;
  FSkippedCount:=0;

  Caption                     := SS7PTBCaption;
  btnBrowse.Caption           := SS7PTBBrowse;
  chkSymbolicNames.Caption    := SS7PTBSymbolicNames;
  chkIncludeAreas.Caption     := SS7PTBIncludeAreas;
  lblFilter.Caption           := SS7PTBFilter;
  chkShowStructMembers.Caption:= SS7PTBShowStructMembers;
  chkShowSkipped.Caption      := SS7PTBShowSkipped;
  lblScan.Caption             := SS7PTBScan;
  lblPrefix.Caption           := SS7PTBPrefix;
  btnCheckAll.Caption         := SS7PTBCheckAll;
  btnUncheckAll.Caption       := SS7PTBUncheckAll;
  btnInvert.Caption           := SS7PTBInvert;
  btnOK.Caption               := SS7PTBImport;
  btnCancel.Caption           := SS7PTBCancel;

  lvTags.Columns[0].Caption   := SS7PTBColVariable;
  lvTags.Columns[1].Caption   := SS7PTBColType;
  lvTags.Columns[2].Caption   := SS7PTBColWillBeCreatedAs;

  lblStatus.Caption:=SS7PTBReadFirst;
  UpdateSelectedCount;
end;

function TfrmS7PlusTagBuilder.PassFilters(aIndex: LongInt): Boolean;
var
  aFilter:String;
begin
  Result:=false;

  if (FCandidates[aIndex].Skip<>srNone) and (not chkShowSkipped.Checked) then exit;
  if FCandidates[aIndex].Nested and (not chkShowStructMembers.Checked) then exit;

  aFilter:=Trim(edtFilter.Text);
  if (aFilter<>'') and (Pos(LowerCase(aFilter),LowerCase(FCandidates[aIndex].Path))=0) then exit;

  Result:=true;
end;

function TfrmS7PlusTagBuilder.CreatedAs(aIndex: LongInt): String;
begin
  case FCandidates[aIndex].Skip of
    srNone: begin
      Result:='TPLCTagNumber';
    end;
    srNotAScalar:      Result:=SS7PTBSkipNotAScalar;
    srUnsupportedType: Result:=SS7PTBSkipUnsupportedType;
    srArrayElement:    Result:=SS7PTBSkipArrayElement;
  else
    Result:='';
  end;
end;

procedure TfrmS7PlusTagBuilder.HarvestChecks;
var
  c: Integer;
begin
  for c:=0 to lvTags.Items.Count-1 do
    if (c<=High(FVisible)) and (FVisible[c]>=0) then
      //a variable that does not become a tag cannot be checked, whatever the list shows
      FChecked[FVisible[c]]:=lvTags.Items[c].Checked and (FCandidates[FVisible[c]].Skip=srNone);
end;

procedure TfrmS7PlusTagBuilder.RebuildList;
var
  c, visibleCount: LongInt;
  item: TListItem;
begin
  SetLength(FVisible, Length(FCandidates));
  visibleCount:=0;

  lvTags.Items.BeginUpdate;
  try
    lvTags.Items.Clear;
    for c:=0 to High(FCandidates) do begin
      if not PassFilters(c) then continue;

      item:=lvTags.Items.Add;
      item.Caption:=FCandidates[c].Path;
      item.SubItems.Add(S7PlusSoftTypeName(FCandidates[c].SoftDataType));
      item.SubItems.Add(CreatedAs(c));
      item.Checked:=FChecked[c];

      FVisible[visibleCount]:=c;
      inc(visibleCount);
    end;
  finally
    lvTags.Items.EndUpdate;
  end;

  SetLength(FVisible, visibleCount);
  UpdateSelectedCount;
end;

procedure TfrmS7PlusTagBuilder.UpdateSelectedCount;
var
  c, total: LongInt;
begin
  total:=0;
  for c:=0 to High(FChecked) do
    if FChecked[c] then inc(total);

  lblSelected.Caption:=Format(SS7PTBSelectedCount,[total, lvTags.Items.Count]);
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
    if FCandidates[n].Skip<>srNone then inc(FSkippedCount);
  end;
end;

procedure TfrmS7PlusTagBuilder.btnBrowseClick(Sender: TObject);
var
  oldCursor:TCursor;
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
  try
    lblStatus.Caption:=SS7PTBReading;
    lblStatus.Update;

    if FDriver.ListDataBlocks(blocks)<>ioOk then begin
      Screen.Cursor:=oldCursor;
      MessageDlg(SS7PTBBrowseFailed, mtError, [mbOK], 0);
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
    if FCandidates[c].Skip<>srNone then inc(FSkippedCount);

  SetLength(FChecked, Length(FCandidates));
  for c:=0 to High(FChecked) do
    FChecked[c]:=false;

  RebuildList;

  if Length(FCandidates)=0 then
    lblStatus.Caption:=SS7PTBNothingRead
  else
    lblStatus.Caption:=Format(SS7PTBVarsRead,[Length(FCandidates), FBlocksRead, FSkippedCount]);
end;

procedure TfrmS7PlusTagBuilder.btnCheckAllClick(Sender: TObject);
var
  c: Integer;
begin
  lvTags.Items.BeginUpdate;
  try
    for c:=0 to lvTags.Items.Count-1 do
      lvTags.Items[c].Checked:=true;
  finally
    lvTags.Items.EndUpdate;
  end;
  HarvestChecks;
  UpdateSelectedCount;
end;

procedure TfrmS7PlusTagBuilder.btnUncheckAllClick(Sender: TObject);
var
  c: Integer;
begin
  lvTags.Items.BeginUpdate;
  try
    for c:=0 to lvTags.Items.Count-1 do
      lvTags.Items[c].Checked:=false;
  finally
    lvTags.Items.EndUpdate;
  end;
  HarvestChecks;
  UpdateSelectedCount;
end;

procedure TfrmS7PlusTagBuilder.btnInvertClick(Sender: TObject);
var
  c: Integer;
begin
  lvTags.Items.BeginUpdate;
  try
    for c:=0 to lvTags.Items.Count-1 do
      lvTags.Items[c].Checked:=not lvTags.Items[c].Checked;
  finally
    lvTags.Items.EndUpdate;
  end;
  HarvestChecks;
  UpdateSelectedCount;
end;

procedure TfrmS7PlusTagBuilder.lvTagsClick(Sender: TObject);
begin
  HarvestChecks;
  UpdateSelectedCount;
end;

procedure TfrmS7PlusTagBuilder.FilterChanged(Sender: TObject);
begin
  HarvestChecks;
  RebuildList;
end;

procedure TfrmS7PlusTagBuilder.btnOKClick(Sender: TObject);
begin
  HarvestChecks;
  //a skipped variable that was shown and checked on the list is not counted
  RebuildList;
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
