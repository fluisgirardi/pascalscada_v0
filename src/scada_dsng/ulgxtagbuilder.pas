{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit do assistente de importação de tags de CLPs Rockwell
            Compact/ControlLogix (Ethernet/IP - CIP).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit of the tag import wizard of Rockwell Compact/ControlLogix
            PLCs (Ethernet/IP - CIP).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit ulgxtagbuilder;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LResources,{$ENDIF}
  Classes, SysUtils, StrUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Spin, Tag, LGXDriver;

type

  {$IFDEF PORTUGUES}
  //: Classe de tag do PascalSCADA que será criada para um tag do CLP.
  {$ELSE}
  //: PascalSCADA tag class that will be created for a PLC tag.
  {$ENDIF}
  TLGXTagKind = (lgxtkUnsupported, lgxtkNumber, lgxtkBlock, lgxtkString);

  {$IFDEF PORTUGUES}
  //: Descreve um tag que será criado no formulário/datamodule.
  {$ELSE}
  //: Describes a tag that will be created on the form/datamodule.
  {$ENDIF}
  TLGXImportItem = record
    //: Endereço simbólico do tag no CLP (propriedade LongAddress).
    TagPath:String;
    //: Nome sugerido do componente.
    CompName:String;
    //: Classe de tag que deve ser criada.
    Kind:TLGXTagKind;
    //: Tipo do tag, para TPLCTagNumber e TPLCBlock.
    TagType:TTagType;
    //: Elementos (TPLCBlock) ou caracteres (TPLCString).
    Size:LongInt;
    //: Tempo de atualização, em milissegundos.
    Scan:LongInt;
  end;

  //: Lista dos tags que serão criados.
  TLGXImportItems = array of TLGXImportItem;

  { TfrmLGXTagBuilder }

  TfrmLGXTagBuilder = class(TForm)
    btnBrowse: TButton;
    btnCancel: TButton;
    btnCheckAll: TButton;
    btnInvert: TButton;
    btnOK: TButton;
    btnUncheckAll: TButton;
    chkForceReload: TCheckBox;
    chkShowArrayElements: TCheckBox;
    chkShowProgramTags: TCheckBox;
    chkShowStructMembers: TCheckBox;
    chkStringsAsPLCString: TCheckBox;
    cboKindFilter: TComboBox;
    edtFilter: TEdit;
    edtPrefix: TEdit;
    lblFilter: TLabel;
    lblKindFilter: TLabel;
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
    procedure FormDestroy(Sender: TObject);
    procedure lvTagsClick(Sender: TObject);
  private
    FDriver:TLGXDriver;
    FTagList:TLGXTagInfoArray;
    FChecked:array of Boolean;
    FKind:array of TLGXTagKind;
    FStringElements:array of LongInt;
    FVisible:array of LongInt;   //indice da listview -> indice de FTagList
    FNameIndex:TStringList;      //nome em minusculas -> indice de FTagList
    FListLoaded:Boolean;
    FSkippedCount:LongInt;
    function  IsArrayElement(const aName:String):Boolean;
    function  IsStructMember(const aName:String):Boolean;
    function  IsProgramTag(const aName:String):Boolean;
    function  FindTag(const aLowerCaseName:String; out aIndex:LongInt):Boolean;
    function  StringDataElements(aTagIndex:LongInt):LongInt;
    procedure ClassifyTags;
    procedure HarvestChecks;
    procedure RebuildList;
    procedure UpdateSelectedCount;
    function  PassFilters(aTagIndex:LongInt):Boolean;
    function  KindDescription(aTagIndex:LongInt):String;
    function  SuggestedName(const aTagPath:String):String;
  public
    {$IFDEF PORTUGUES}
    //: Driver de protocolo de onde a lista de tags será lida.
    {$ELSE}
    //: Protocol driver from where the tag list will be read.
    {$ENDIF}
    property Driver:TLGXDriver read FDriver write FDriver;

    {$IFDEF PORTUGUES}
    //: Devolve a lista de tags que devem ser criados no formulário/datamodule.
    {$ELSE}
    //: Returns the list of tags that must be created on the form/datamodule.
    {$ENDIF}
    function GetImportPlan:TLGXImportItems;

    {$IFDEF PORTUGUES}
    //: Carrega no assistente uma lista de tags já lida do CLP.
    {$ELSE}
    //: Loads on the wizard a tag list already read from the PLC.
    {$ENDIF}
    procedure LoadTagList(const aTagList:TLGXTagInfoArray; aFullyLoaded:Boolean);
  end;

implementation

uses hsstrings;

{$IFDEF FPC}
  {$R ulgxtagbuilder.lfm}
{$ENDIF}

const
  KIND_FILTER_ALL     = 0;
  KIND_FILTER_NUMBER  = 1;
  KIND_FILTER_ARRAY   = 2;
  KIND_FILTER_STRING  = 3;

{ TfrmLGXTagBuilder }

procedure TfrmLGXTagBuilder.FormCreate(Sender: TObject);
begin
  FNameIndex:=TStringList.Create;
  FNameIndex.Sorted:=true;
  FNameIndex.Duplicates:=dupIgnore;
  FListLoaded:=false;
  FSkippedCount:=0;

  Caption                    := SLGXTBCaption;
  btnBrowse.Caption          := SLGXTBBrowse;
  chkForceReload.Caption     := SLGXTBForceReload;
  lblFilter.Caption          := SLGXTBFilter;
  lblKindFilter.Caption      := SLGXTBKindFilter;
  chkShowArrayElements.Caption:=SLGXTBShowArrayElements;
  chkShowStructMembers.Caption:=SLGXTBShowStructMembers;
  chkShowProgramTags.Caption := SLGXTBShowProgramTags;
  chkStringsAsPLCString.Caption:=SLGXTBStringsAsPLCString;
  lblScan.Caption            := SLGXTBScan;
  lblPrefix.Caption          := SLGXTBPrefix;
  btnCheckAll.Caption        := SLGXTBCheckAll;
  btnUncheckAll.Caption      := SLGXTBUncheckAll;
  btnInvert.Caption          := SLGXTBInvert;
  btnOK.Caption              := SLGXTBImport;
  btnCancel.Caption          := SLGXTBCancel;

  lvTags.Columns[0].Caption  := SLGXTBColTag;
  lvTags.Columns[1].Caption  := SLGXTBColType;
  lvTags.Columns[2].Caption  := SLGXTBColElements;
  lvTags.Columns[3].Caption  := SLGXTBColBytes;
  lvTags.Columns[4].Caption  := SLGXTBColWillBeCreatedAs;

  cboKindFilter.Items.Clear;
  cboKindFilter.Items.Add(SLGXTBKindAll);
  cboKindFilter.Items.Add(SLGXTBKindNumber);
  cboKindFilter.Items.Add(SLGXTBKindArray);
  cboKindFilter.Items.Add(SLGXTBKindString);
  cboKindFilter.ItemIndex:=KIND_FILTER_ALL;

  lblStatus.Caption:=SLGXTBReadTagListFirst;
  UpdateSelectedCount;
end;

procedure TfrmLGXTagBuilder.FormDestroy(Sender: TObject);
begin
  FNameIndex.Free;
end;

function TfrmLGXTagBuilder.IsArrayElement(const aName: String): Boolean;
begin
  Result:=Pos('[',aName)>0;
end;

function TfrmLGXTagBuilder.IsStructMember(const aName: String): Boolean;
var
  aux:String;
  p:Integer;
begin
  aux:=aName;

  //o "Program:<programa>." de um tag de programa não faz dele um membro de
  //estrutura.
  if IsProgramTag(aux) then begin
    p:=Pos('.',aux);
    if p>0 then
      aux:=Copy(aux,p+1,Length(aux))
    else
      aux:='';
  end;

  Result:=Pos('.',aux)>0;
end;

function TfrmLGXTagBuilder.IsProgramTag(const aName: String): Boolean;
begin
  Result:=Pos('program:',LowerCase(aName))=1;
end;

function TfrmLGXTagBuilder.FindTag(const aLowerCaseName: String; out
  aIndex: LongInt): Boolean;
var
  idx: Integer;
begin
  aIndex:=-1;
  Result:=FNameIndex.Find(aLowerCaseName, idx);
  if Result then
    aIndex:=LongInt(PtrInt(FNameIndex.Objects[idx]));
end;

function TfrmLGXTagBuilder.StringDataElements(aTagIndex: LongInt): LongInt;
var
  lname:String;
  dataIdx, lenIdx: LongInt;
begin
  //uma STRING do Logix é uma estrutura com dois membros: LEN (DINT) e
  //DATA (SINT[n]). Como o nome da UDT não vem na lista de tags, a estrutura
  //é identificada pelos seus membros.
  Result:=0;
  if not LGXTypeIsStruct(FTagList[aTagIndex].aType) then exit;

  lname:=LowerCase(FTagList[aTagIndex].name);
  if not FindTag(lname+'.data', dataIdx) then exit;
  if not FindTag(lname+'.len',  lenIdx ) then exit;

  if (FTagList[dataIdx].aType and $FF)<>TAG_CIP_TYPE_SINT then exit;

  if FTagList[dataIdx].elem_count>1 then
    Result:=FTagList[dataIdx].elem_count;
end;

procedure TfrmLGXTagBuilder.ClassifyTags;
var
  c: LongInt;
  aTagType: TTagType;
begin
  SetLength(FChecked,        Length(FTagList));
  SetLength(FKind,           Length(FTagList));
  SetLength(FStringElements, Length(FTagList));

  FNameIndex.Clear;
  FNameIndex.BeginUpdate;
  try
    for c:=0 to High(FTagList) do
      FNameIndex.AddObject(LowerCase(FTagList[c].name), TObject(PtrInt(c)));
  finally
    FNameIndex.EndUpdate;
  end;

  FSkippedCount:=0;
  for c:=0 to High(FTagList) do begin
    FChecked[c]:=false;
    FStringElements[c]:=0;
    FKind[c]:=lgxtkUnsupported;

    if LGXTypeIsSystem(FTagList[c].aType) then begin
      inc(FSkippedCount);
      continue;
    end;

    if LGXTypeIsStruct(FTagList[c].aType) then begin
      //estruturas só podem ser importadas diretamente quando são STRINGs.
      //os demais membros de uma UDT aparecem individualmente na lista.
      FStringElements[c]:=StringDataElements(c);
      if FStringElements[c]>1 then
        FKind[c]:=lgxtkString
      else
        inc(FSkippedCount);
      continue;
    end;

    if not LGXTypeToTagType(FTagList[c].aType, aTagType) then begin
      inc(FSkippedCount);
      continue;
    end;

    if (FTagList[c].num_dimensions>0) and (FTagList[c].elem_count>1) then begin
      //arrays de BOOL são empacotados em DWORDs pelo CLP, o que o driver
      //ainda não sabe desempacotar. Melhor não gerar tags quebrados.
      if (FTagList[c].aType and $FF)=TAG_CIP_TYPE_BOOL then begin
        inc(FSkippedCount);
        continue;
      end;
      FKind[c]:=lgxtkBlock;
    end else
      FKind[c]:=lgxtkNumber;
  end;
end;

function TfrmLGXTagBuilder.PassFilters(aTagIndex: LongInt): Boolean;
var
  aName, aFilter:String;
  aKind:TLGXTagKind;
  parentIdx:LongInt;
begin
  Result:=false;

  aKind:=FKind[aTagIndex];
  if aKind=lgxtkUnsupported then exit;

  if (aKind=lgxtkString) and (not chkStringsAsPLCString.Checked) then exit;

  aName:=FTagList[aTagIndex].name;

  if IsProgramTag(aName)   and (not chkShowProgramTags.Checked)   then exit;
  if IsArrayElement(aName) and (not chkShowArrayElements.Checked) then exit;

  if IsStructMember(aName) and (not chkShowStructMembers.Checked) then exit;

  //membros LEN/DATA de uma STRING importada como TPLCString não interessam.
  if chkStringsAsPLCString.Checked and IsStructMember(aName) then begin
    if AnsiEndsText('.data',aName) or AnsiEndsText('.len',aName) then begin
      if FindTag(LowerCase(Copy(aName,1,LastDelimiter('.',aName)-1)), parentIdx) then
        if FStringElements[parentIdx]>1 then exit;
    end;
  end;

  case cboKindFilter.ItemIndex of
    KIND_FILTER_NUMBER: if aKind<>lgxtkNumber then exit;
    KIND_FILTER_ARRAY : if aKind<>lgxtkBlock  then exit;
    KIND_FILTER_STRING: if aKind<>lgxtkString then exit;
  end;

  aFilter:=Trim(edtFilter.Text);
  if (aFilter<>'') and (Pos(LowerCase(aFilter),LowerCase(aName))=0) then exit;

  Result:=true;
end;

function TfrmLGXTagBuilder.KindDescription(aTagIndex: LongInt): String;
begin
  case FKind[aTagIndex] of
    lgxtkNumber: Result:='TPLCTagNumber';
    lgxtkBlock : Result:=Format('TPLCBlock[%d]',[FTagList[aTagIndex].elem_count]);
    lgxtkString: Result:=Format('TPLCString[%d]',[FStringElements[aTagIndex]-1]);
    else         Result:='';
  end;
end;

procedure TfrmLGXTagBuilder.HarvestChecks;
var
  c: Integer;
begin
  for c:=0 to lvTags.Items.Count-1 do
    if (c<=High(FVisible)) and (FVisible[c]>=0) then
      FChecked[FVisible[c]]:=lvTags.Items[c].Checked;
end;

procedure TfrmLGXTagBuilder.RebuildList;
var
  c, visibleCount: LongInt;
  item: TListItem;
begin
  SetLength(FVisible, Length(FTagList));
  visibleCount:=0;

  lvTags.Items.BeginUpdate;
  try
    lvTags.Items.Clear;
    for c:=0 to High(FTagList) do begin
      if not PassFilters(c) then continue;

      item:=lvTags.Items.Add;
      item.Caption:=FTagList[c].name;
      item.SubItems.Add(LGXTypeName(FTagList[c].aType));
      if FKind[c]=lgxtkString then begin
        item.SubItems.Add('1');
        item.SubItems.Add(IntToStr(FStringElements[c]));
      end else begin
        item.SubItems.Add(IntToStr(FTagList[c].elem_count));
        item.SubItems.Add(IntToStr(FTagList[c].elem_count*FTagList[c].elem_size));
      end;
      item.SubItems.Add(KindDescription(c));
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

procedure TfrmLGXTagBuilder.UpdateSelectedCount;
var
  c, total: LongInt;
begin
  total:=0;
  for c:=0 to High(FChecked) do
    if FChecked[c] then inc(total);

  lblSelected.Caption:=Format(SLGXTBSelectedCount,[total, lvTags.Items.Count]);
  btnOK.Enabled:=total>0;
end;

function TfrmLGXTagBuilder.SuggestedName(const aTagPath: String): String;
var
  c: Integer;
  lastWasSep:Boolean;
begin
  Result:='';
  lastWasSep:=false;
  for c:=1 to Length(aTagPath) do begin
    if aTagPath[c] in ['0'..'9','a'..'z','A'..'Z'] then begin
      Result:=Result+aTagPath[c];
      lastWasSep:=false;
    end else begin
      if not lastWasSep then
        Result:=Result+'_';
      lastWasSep:=true;
    end;
  end;

  while (Length(Result)>0) and (Result[Length(Result)]='_') do
    Delete(Result,Length(Result),1);

  Result:=Trim(edtPrefix.Text)+Result;

  //um nome de componente não pode começar com um dígito.
  if (Length(Result)>0) and (Result[1] in ['0'..'9']) then
    Result:='_'+Result;

  if Trim(Result)='' then
    Result:='tag';
end;

procedure TfrmLGXTagBuilder.btnBrowseClick(Sender: TObject);
var
  oldCursor:TCursor;
begin
  if not Assigned(FDriver) then begin
    MessageDlg(SLGXTBWithoutDriver, mtError, [mbOK], 0);
    exit;
  end;

  if (not Assigned(FDriver.CommunicationPort)) or
     (not FDriver.CommunicationPort.Active) then begin
    MessageDlg(SLGXTBPortMustBeActive, mtError, [mbOK], 0);
    exit;
  end;

  oldCursor:=Screen.Cursor;
  Screen.Cursor:=crHourGlass;
  try
    lblStatus.Caption:=SLGXTBReadingTagList;
    lblStatus.Update;

    FListLoaded:=FDriver.BrowseTagList(FTagList, chkForceReload.Checked);
  finally
    Screen.Cursor:=oldCursor;
  end;

  LoadTagList(FTagList, FListLoaded);
end;

procedure TfrmLGXTagBuilder.LoadTagList(const aTagList: TLGXTagInfoArray;
  aFullyLoaded: Boolean);
begin
  FTagList:=aTagList;
  FListLoaded:=aFullyLoaded;

  ClassifyTags;
  RebuildList;

  if Length(FTagList)=0 then
    lblStatus.Caption:=SLGXTBNothingRead
  else
    if FListLoaded then
      lblStatus.Caption:=Format(SLGXTBTagsRead,[Length(FTagList), FSkippedCount])
    else
      lblStatus.Caption:=Format(SLGXTBTagsReadPartially,[Length(FTagList), FSkippedCount]);
end;

procedure TfrmLGXTagBuilder.btnCheckAllClick(Sender: TObject);
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

procedure TfrmLGXTagBuilder.btnUncheckAllClick(Sender: TObject);
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

procedure TfrmLGXTagBuilder.btnInvertClick(Sender: TObject);
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

procedure TfrmLGXTagBuilder.lvTagsClick(Sender: TObject);
begin
  HarvestChecks;
  UpdateSelectedCount;
end;

procedure TfrmLGXTagBuilder.FilterChanged(Sender: TObject);
begin
  HarvestChecks;
  RebuildList;
end;

procedure TfrmLGXTagBuilder.btnOKClick(Sender: TObject);
begin
  HarvestChecks;
  UpdateSelectedCount;
  if not btnOK.Enabled then begin
    MessageDlg(SLGXTBSelectAtLeastOne, mtInformation, [mbOK], 0);
    exit;
  end;
  ModalResult:=mrOK;
end;

function TfrmLGXTagBuilder.GetImportPlan: TLGXImportItems;
var
  c, count, dataIdx: LongInt;
  aTagType: TTagType;
begin
  Result:=nil;
  SetLength(Result, Length(FTagList));
  count:=0;

  for c:=0 to High(FTagList) do begin
    if (c>High(FChecked)) or (not FChecked[c]) then continue;
    if FKind[c]=lgxtkUnsupported then continue;

    Result[count].Kind    :=FKind[c];
    Result[count].Scan    :=spinScan.Value;
    Result[count].TagType :=pttDefault;
    Result[count].Size    :=1;
    Result[count].TagPath :=FTagList[c].name;

    case FKind[c] of
      lgxtkString: begin
        //o TPLCString lê o membro DATA (SINT[n]) da estrutura STRING.
        if not FindTag(LowerCase(FTagList[c].name)+'.data', dataIdx) then continue;
        Result[count].TagPath:=FTagList[dataIdx].name;
        Result[count].Size   :=FStringElements[c]-1;
      end;

      lgxtkBlock: begin
        if not LGXTypeToTagType(FTagList[c].aType, aTagType) then continue;
        Result[count].TagType:=aTagType;
        Result[count].Size   :=FTagList[c].elem_count;
      end;

      lgxtkNumber: begin
        if not LGXTypeToTagType(FTagList[c].aType, aTagType) then continue;
        Result[count].TagType:=aTagType;
        Result[count].Size   :=1;
      end;
    end;

    Result[count].CompName:=SuggestedName(FTagList[c].name);
    inc(count);
  end;

  SetLength(Result, count);
end;

end.
