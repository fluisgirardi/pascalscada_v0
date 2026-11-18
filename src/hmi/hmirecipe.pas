{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Controle de receitas: uma tabela de parametros por receita, com
  biblioteca em disco e download/upload para o CLP.)

  Cada coluna da tabela e' um parametro (um campo), cada linha e' uma receita.
  A biblioteca fica num arquivo de texto simples gravado no micro, o que
  permite centenas de receitas sem gastar um byte do CLP. O botao de download
  escreve a receita selecionada nos tags vinculados as colunas; o de upload
  faz o caminho contrario.

  @author(PascalSCADA)
}
{$ELSE}
{:
  @abstract(Recipe control: one table of parameters per recipe, a library on
  disk and download/upload to the PLC.)

  Every column is a parameter (a field) and every row is a recipe. The library
  lives in a plain text file on the PC, so hundreds of recipes cost nothing in
  the PLC. Download writes the selected recipe to the tags linked to the
  columns; upload walks the other way.

  @author(PascalSCADA)
}
{$ENDIF}
unit HMIRecipe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Controls, Graphics, Grids, Tag, PLCTag, ProtocolTypes;

type
  TRecipeField = class;
  TRecipeFields = class;
  THMIRecipeGrid = class;

  {$IFDEF PORTUGUES}
  //: Chamado quando o controle recusa alguma coisa (valor fora da faixa,
  //: receita repetida, tag que nao e' numerico...).
  {$ELSE}
  //: Called when the control refuses something (value out of range, repeated
  //: recipe, a tag that is not numeric...).
  {$ENDIF}
  TRecipeErrorEvent = procedure(Sender: TObject; const aMessage: string) of object;

  {$IFDEF PORTUGUES}
  //: Chamado no fim de um download/upload, com o nome da receita usada.
  {$ELSE}
  //: Called at the end of a download/upload, with the recipe name used.
  {$ENDIF}
  TRecipeNotifyEvent = procedure(Sender: TObject; const aRecipeName: string) of object;

  {$IFDEF PORTUGUES}
  {:
    @abstract(Uma receita: um nome e um valor por campo.)
  }
  {$ELSE}
  {:
    @abstract(One recipe: a name and one value per field.)
  }
  {$ENDIF}
  TRecipe = class
  public
    Name: string;
    Values: array of Double;
    constructor Create(const aName: string; aFieldCount: Integer);
    procedure SetFieldCount(aCount: Integer);
  end;

  {$IFDEF PORTUGUES}
  {:
    @abstract(Um parametro da receita: o nome que aparece no cabecalho, o tag
    do CLP que ele escreve e como o valor e' mostrado e conferido.)
  }
  {$ELSE}
  {:
    @abstract(One recipe parameter: the name shown in the header, the PLC tag
    it writes to, and how the value is displayed and checked.)
  }
  {$ENDIF}
  TRecipeField = class(TCollectionItem)
  private
    FName: string;
    FPLCTag: TPLCTag;
    FDecimals: Integer;
    FCheckRange: Boolean;
    FMinValue: Double;
    FMaxValue: Double;
    FDefaultValue: Double;

    procedure SetName(const v: string);
    procedure SetPLCTag(const v: TPLCTag);
    procedure SetDecimals(const v: Integer);
    procedure SetCheckRange(const v: Boolean);
    procedure SetMinValue(const v: Double);
    procedure SetMaxValue(const v: Double);
    procedure SetDefaultValue(const v: Double);
    function  FieldOwner: TComponent;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy; override;

    //: limpa a referencia sem avisar o dono (tag sendo destruido).
    //: clears the reference without notifying the owner (tag being destroyed).
    procedure ClearTagRef(aTag: TComponent);
  published
    {$IFDEF PORTUGUES}
    //: Nome do parametro, mostrado no cabecalho da coluna.
    {$ELSE}
    //: Parameter name, shown in the column header.
    {$ENDIF}
    property Name: string read FName write SetName;

    {$IFDEF PORTUGUES}
    //: Tag do CLP que recebe o valor no download e fornece no upload.
    {$ELSE}
    //: PLC tag that receives the value on download and provides it on upload.
    {$ENDIF}
    property PLCTag: TPLCTag read FPLCTag write SetPLCTag;

    {$IFDEF PORTUGUES}
    //: Casas decimais usadas para mostrar e para arredondar a exibicao.
    {$ELSE}
    //: Decimal places used to display the value.
    {$ENDIF}
    property Decimals: Integer read FDecimals write SetDecimals default 2;

    {$IFDEF PORTUGUES}
    //: Liga a conferencia de faixa (MinValue/MaxValue).
    {$ELSE}
    //: Enables the range check (MinValue/MaxValue).
    {$ENDIF}
    property CheckRange: Boolean read FCheckRange write SetCheckRange default False;

    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;

    {$IFDEF PORTUGUES}
    //: Valor usado quando a receita nova e' criada e quando um campo entra
    //: numa receita que ja existia.
    {$ELSE}
    //: Value used for a new recipe and when a field is added to an existing
    //: recipe.
    {$ENDIF}
    property DefaultValue: Double read FDefaultValue write SetDefaultValue;
  end;

  {$IFDEF PORTUGUES}
  {:
    @abstract(Colecao de parametros da receita.)
  }
  {$ELSE}
  {:
    @abstract(Collection of recipe parameters.)
  }
  {$ENDIF}
  TRecipeFields = class(TCollection)
  private
    FOwner: TComponent;
    function GetItem(aIndex: Integer): TRecipeField;
    procedure SetItem(aIndex: Integer; const v: TRecipeField);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(aOwner: TComponent);
    function Add: TRecipeField;
    function IndexOfName(const aName: string): Integer;
    function ItemByName(const aName: string): TRecipeField;
    property OwnerComponent: TComponent read FOwner;
    property Items[aIndex: Integer]: TRecipeField read GetItem write SetItem; default;
  end;

  {$IFDEF PORTUGUES}
  {:
    @abstract(Tabela de receitas.)

    Uma linha por receita, uma coluna por parametro. A primeira coluna e' o
    nome da receita e pode ser editada na propria celula; as demais recebem
    numeros, conferidos contra a faixa do campo quando CheckRange esta ligado.
  }
  {$ELSE}
  {:
    @abstract(Recipe table.)

    One row per recipe, one column per parameter. The first column is the
    recipe name and can be edited in the cell itself; the others take numbers,
    checked against the field range when CheckRange is on.
  }
  {$ENDIF}
  THMIRecipeGrid = class(TStringGrid)
  private
    FFields: TRecipeFields;
    FRecipes: TList;
    FRecipeFile: string;
    FAutoLoad: Boolean;
    FNameColumnTitle: string;
    FUpdating: Boolean;
    FLastError: string;
    FOnError: TRecipeErrorEvent;
    FOnDownload: TRecipeNotifyEvent;
    FOnUpload: TRecipeNotifyEvent;
    FOnRecipeChanged: TNotifyEvent;

    procedure SetFields(const v: TRecipeFields);
    procedure SetRecipeFile(const v: string);
    procedure SetNameColumnTitle(const v: string);
    procedure RecipeChanged;

    function  GetRecipe(aIndex: Integer): TRecipe;
    function  GetRecipeCount: Integer;

    procedure DoError(const aMessage: string);

    function  LibraryFileName: string;
    procedure FillCells;
    procedure FillRecipeCells(aRow: Integer; aRecipe: TRecipe);
    function  FieldCaption(aIndex: Integer): string;
    function  FieldKey(aIndex: Integer): string;
    function  FieldIndexByKey(const aKey: string): Integer;
  protected
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    {$IFDEF PORTUGUES}
    //: Reconstroi as celulas a partir da biblioteca em memoria.
    {$ELSE}
    //: Rebuilds the cells from the in-memory library.
    {$ENDIF}
    procedure RefreshCells;

    {$IFDEF PORTUGUES}
    //: Le a biblioteca do arquivo. Erros vao para OnError e a biblioteca
    //: anterior e' mantida.
    {$ELSE}
    //: Reads the library from the file. Errors go to OnError and the previous
    //: library is kept.
    {$ENDIF}
    procedure LoadLibrary;

    {$IFDEF PORTUGUES}
    //: Grava a biblioteca no arquivo (diretorio criado se faltar).
    {$ELSE}
    //: Writes the library to the file (directory created when missing).
    {$ENDIF}
    procedure SaveLibrary;

    {$IFDEF PORTUGUES}
    //: Cria uma receita com os valores padrao e devolve o indice dela.
    {$ELSE}
    //: Creates a recipe with the default values and returns its index.
    {$ENDIF}
    function AddRecipe(const aName: string): Integer;

    {$IFDEF PORTUGUES}
    //: Apaga a receita pelo indice (0-based).
    {$ELSE}
    //: Deletes the recipe by index (0-based).
    {$ENDIF}
    procedure DeleteRecipe(aIndex: Integer); overload;
    procedure DeleteRecipe(const aName: string); overload;

    {$IFDEF PORTUGUES}
    //: Copia uma receita (valores e nome novo).
    {$ELSE}
    //: Copies a recipe (values and a new name).
    {$ENDIF}
    function DuplicateRecipe(const aName, aNewName: string): Integer;

    {$IFDEF PORTUGUES}
    //: Renomeia. Devolve @false se o nome estiver vazio ou repetido.
    {$ELSE}
    //: Renames. Returns @false when the name is empty or taken.
    {$ENDIF}
    function RenameRecipe(const aOldName, aNewName: string): Boolean;

    //: indice da receita pelo nome, -1 se nao existir / index by name, -1 if absent
    function IndexOfRecipe(const aName: string): Integer;

    function RecipeName(aIndex: Integer): string;
    function ActiveRecipeName: string;

    {$IFDEF PORTUGUES}
    //: Seleciona a linha da receita pelo nome.
    {$ELSE}
    //: Selects the recipe row by name.
    {$ENDIF}
    procedure SelectRecipe(const aName: string);

    function FieldByName(const aName: string): TRecipeField;

    {$IFDEF PORTUGUES}
    //: valor da celula (0-based, sem a coluna do nome).
    {$ELSE}
    //: value of a cell (0-based, name column excluded).
    {$ENDIF}
    function ValueOf(aRecipeIndex, aFieldIndex: Integer): Double;

    {$IFDEF PORTUGUES}
    //: Escreve a receita selecionada nos tags das colunas.
    {$ELSE}
    //: Writes the selected recipe to the tags of the columns.
    {$ENDIF}
    procedure Download;

    {$IFDEF PORTUGUES}
    //: Le os tags das colunas para a receita selecionada (nao grava o arquivo).
    {$ELSE}
    //: Reads the tags of the columns into the selected recipe (does not save
    //: the file).
    {$ENDIF}
    procedure Upload;

    //: numero de receitas na biblioteca / number of recipes in the library
    property RecipeCount: Integer read GetRecipeCount;
    //: receitas por indice (0-based) / recipes by index (0-based)
    property Recipes[aIndex: Integer]: TRecipe read GetRecipe;
    //: ultima mensagem de erro / last error message
    property LastError: string read FLastError;
  published
    {$IFDEF PORTUGUES}
    //: Parametros que viram colunas da tabela.
    {$ELSE}
    //: Parameters that become the columns of the table.
    {$ENDIF}
    property Fields: TRecipeFields read FFields write SetFields;

    {$IFDEF PORTUGUES}
    //: Arquivo da biblioteca. Vazio = recipes.ini ao lado do executavel.
    //: Caminho relativo e' resolvido a partir do executavel.
    {$ELSE}
    //: Library file. Empty = recipes.ini beside the executable. A relative
    //: path is resolved from the executable folder.
    {$ENDIF}
    property RecipeFile: string read FRecipeFile write SetRecipeFile;

    {$IFDEF PORTUGUES}
    //: Le a biblioteca quando o formulario e' criado.
    {$ELSE}
    //: Loads the library when the form is created.
    {$ENDIF}
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad default True;

    //: titulo da coluna do nome / title of the name column
    property NameColumnTitle: string read FNameColumnTitle write SetNameColumnTitle;

    property OnError: TRecipeErrorEvent read FOnError write FOnError;
    property OnDownload: TRecipeNotifyEvent read FOnDownload write FOnDownload;
    property OnUpload: TRecipeNotifyEvent read FOnUpload write FOnUpload;
    //: algo mudou na biblioteca / something changed in the library
    property OnRecipeChanged: TNotifyEvent read FOnRecipeChanged write FOnRecipeChanged;
  end;

implementation

const
  RECIPE_FILE_HEADER = '# PascalSCADA recipe library';
  RECIPE_FILE_VERSION = 'Version=1';

function IsAbsPath(const aPath: string): Boolean;
begin
  Result := (aPath <> '') and
            ((aPath[1] = '/') or (aPath[1] = '\') or
             ((Length(aPath) > 1) and (aPath[2] = ':')));
end;

function InvariantSettings: TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DecimalSeparator := '.';
  Result.ThousandSeparator := #0;
end;

function TextToValue(const aText: string; out aValue: Double): Boolean;
var
  s: string;
  fs: TFormatSettings;
begin
  s := Trim(aText);
  if s = '' then begin
    aValue := 0;
    Result := False;
    exit;
  end;
  //aceita virgula como separador decimal quando nao ha ponto.
  //accepts a comma as decimal separator when there is no dot.
  if (Pos(',', s) > 0) and (Pos('.', s) = 0) then
    s := StringReplace(s, ',', '.', [rfReplaceAll]);
  fs := InvariantSettings;
  Result := TryStrToFloat(s, aValue, fs);
end;

function ValueToText(const aValue: Double; aDecimals: Integer): string;
var
  fs: TFormatSettings;
begin
  fs := InvariantSettings;
  if aDecimals < 0 then aDecimals := 0;
  if aDecimals > 15 then aDecimals := 15;
  Result := FloatToStrF(aValue, ffFixed, 15, aDecimals, fs);
end;

{ TRecipe }

constructor TRecipe.Create(const aName: string; aFieldCount: Integer);
begin
  inherited Create;
  Name := aName;
  SetFieldCount(aFieldCount);
end;

procedure TRecipe.SetFieldCount(aCount: Integer);
begin
  if aCount < 0 then aCount := 0;
  SetLength(Values, aCount);
end;

{ TRecipeField }

constructor TRecipeField.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FDecimals := 2;
  FCheckRange := False;
  FMinValue := 0;
  FMaxValue := 100;
  FDefaultValue := 0;
end;

destructor TRecipeField.Destroy;
var
  owner: TComponent;
begin
  owner := FieldOwner;
  if (FPLCTag <> nil) and (owner <> nil) then
    FPLCTag.RemoveFreeNotification(owner);
  FPLCTag := nil;
  inherited Destroy;
end;

function TRecipeField.FieldOwner: TComponent;
begin
  if (Collection <> nil) and (Collection is TRecipeFields) then
    Result := TRecipeFields(Collection).OwnerComponent
  else
    Result := nil;
end;

function TRecipeField.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := 'Field ' + IntToStr(Index);
end;

procedure TRecipeField.SetName(const v: string);
begin
  if FName = v then exit;
  FName := v;
  Changed(False);
end;

procedure TRecipeField.SetPLCTag(const v: TPLCTag);
var
  owner: TComponent;
begin
  if FPLCTag = v then exit;
  owner := FieldOwner;
  if (FPLCTag <> nil) and (owner <> nil) then
    FPLCTag.RemoveFreeNotification(owner);
  FPLCTag := v;
  if (FPLCTag <> nil) and (owner <> nil) then
    FPLCTag.FreeNotification(owner);
  Changed(False);
end;

procedure TRecipeField.ClearTagRef(aTag: TComponent);
begin
  if FPLCTag = aTag then
    FPLCTag := nil;
end;

procedure TRecipeField.SetDecimals(const v: Integer);
begin
  if FDecimals = v then exit;
  if v < 0 then FDecimals := 0
  else if v > 15 then FDecimals := 15
  else FDecimals := v;
  Changed(False);
end;

procedure TRecipeField.SetCheckRange(const v: Boolean);
begin
  if FCheckRange = v then exit;
  FCheckRange := v;
  Changed(False);
end;

procedure TRecipeField.SetMinValue(const v: Double);
begin
  if FMinValue = v then exit;
  FMinValue := v;
  Changed(False);
end;

procedure TRecipeField.SetMaxValue(const v: Double);
begin
  if FMaxValue = v then exit;
  FMaxValue := v;
  Changed(False);
end;

procedure TRecipeField.SetDefaultValue(const v: Double);
begin
  if FDefaultValue = v then exit;
  FDefaultValue := v;
  Changed(False);
end;

{ TRecipeFields }

constructor TRecipeFields.Create(aOwner: TComponent);
begin
  inherited Create(TRecipeField);
  FOwner := aOwner;
end;

function TRecipeFields.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TRecipeFields.GetItem(aIndex: Integer): TRecipeField;
begin
  Result := TRecipeField(inherited Items[aIndex]);
end;

procedure TRecipeFields.SetItem(aIndex: Integer; const v: TRecipeField);
begin
  inherited Items[aIndex] := v;
end;

procedure TRecipeFields.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if (FOwner <> nil) and (FOwner is THMIRecipeGrid) and
     ([csDestroying, csLoading]*FOwner.ComponentState = []) then
    THMIRecipeGrid(FOwner).RefreshCells;
end;

function TRecipeFields.Add: TRecipeField;
begin
  Result := TRecipeField(inherited Add);
end;

function TRecipeFields.IndexOfName(const aName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if SameText(Items[i].Name, aName) then begin
      Result := i;
      exit;
    end;
end;

function TRecipeFields.ItemByName(const aName: string): TRecipeField;
var
  i: Integer;
begin
  i := IndexOfName(aName);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

{ THMIRecipeGrid }

constructor THMIRecipeGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFields := TRecipeFields.Create(Self);
  FRecipes := TList.Create;
  FAutoLoad := True;
  FNameColumnTitle := 'Recipe';
  FixedCols := 0;
  FixedRows := 1;
  RowCount := 2;
  ColCount := 2;
  DefaultRowHeight := 22;
  DefaultColWidth := 100;
  ColWidths[0] := 150;
  Options := Options + [goEditing, goColSizing, goTabs];
  RefreshCells;
end;

destructor THMIRecipeGrid.Destroy;
var
  i: Integer;
begin
  for i := 0 to FRecipes.Count-1 do
    TRecipe(FRecipes[i]).Free;
  FRecipes.Free;
  FFields.Free;
  inherited Destroy;
end;

procedure THMIRecipeGrid.SetFields(const v: TRecipeFields);
begin
  FFields.Assign(v);
  RefreshCells;
end;

procedure THMIRecipeGrid.SetRecipeFile(const v: string);
begin
  if FRecipeFile = v then exit;
  FRecipeFile := v;
end;

procedure THMIRecipeGrid.SetNameColumnTitle(const v: string);
begin
  if FNameColumnTitle = v then exit;
  FNameColumnTitle := v;
  RefreshCells;
end;

procedure THMIRecipeGrid.RecipeChanged;
begin
  if Assigned(FOnRecipeChanged) then
    FOnRecipeChanged(Self);
end;

procedure THMIRecipeGrid.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then begin
    RefreshCells;
    exit;
  end;
  if FAutoLoad then begin
    if FileExists(LibraryFileName) then
      LoadLibrary;
  end;
  if FRecipes.Count = 0 then
    AddRecipe('Recipe 1');
  RefreshCells;
end;

procedure THMIRecipeGrid.Notification(AComponent: TComponent; Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FFields <> nil) then
    for i := 0 to FFields.Count-1 do
      FFields[i].ClearTagRef(AComponent);
end;

function THMIRecipeGrid.GetRecipe(aIndex: Integer): TRecipe;
begin
  if (aIndex >= 0) and (aIndex < FRecipes.Count) then
    Result := TRecipe(FRecipes[aIndex])
  else
    Result := nil;
end;

function THMIRecipeGrid.GetRecipeCount: Integer;
begin
  Result := FRecipes.Count;
end;

procedure THMIRecipeGrid.DoError(const aMessage: string);
begin
  FLastError := aMessage;
  if Assigned(FOnError) then
    FOnError(Self, aMessage);
end;

function THMIRecipeGrid.LibraryFileName: string;
begin
  Result := FRecipeFile;
  if Result = '' then
    Result := 'recipes.ini';
  if not IsAbsPath(Result) then
    Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Result;
end;

function THMIRecipeGrid.FieldCaption(aIndex: Integer): string;
begin
  if (aIndex >= 0) and (aIndex < FFields.Count) then begin
    Result := FFields[aIndex].Name;
    if Result = '' then
      Result := 'Field ' + IntToStr(aIndex);
  end else
    Result := '';
end;

function THMIRecipeGrid.FieldKey(aIndex: Integer): string;
var
  s: string;
begin
  Result := FieldCaption(aIndex);
  s := StringReplace(Result, '=', '_', [rfReplaceAll]);
  s := StringReplace(s, '[', '(', [rfReplaceAll]);
  s := StringReplace(s, ']', ')', [rfReplaceAll]);
  s := StringReplace(s, #13, ' ', [rfReplaceAll]);
  s := StringReplace(s, #10, ' ', [rfReplaceAll]);
  Result := s;
end;

function THMIRecipeGrid.FieldIndexByKey(const aKey: string): Integer;
var
  i: Integer;
begin
  //primeiro pelo nome do campo; depois pelo indice gravado como $n, para o
  //caso de o campo ter sido renomeado.
  //first by field name; then by the ordinal stored as $n, for the case of a
  //field that has been renamed.
  Result := -1;
  for i := 0 to FFields.Count-1 do
    if SameText(FieldKey(i), aKey) then begin
      Result := i;
      exit;
    end;
  if (Length(aKey) > 1) and (aKey[1] = '$') then begin
    i := StrToIntDef(Copy(aKey, 2, MaxInt), -1);
    if (i >= 0) and (i < FFields.Count) then
      Result := i;
  end;
end;

procedure THMIRecipeGrid.FillRecipeCells(aRow: Integer; aRecipe: TRecipe);
var
  i: Integer;
begin
  if aRecipe = nil then exit;
  Cells[0, aRow] := aRecipe.Name;
  for i := 0 to FFields.Count-1 do begin
    if i < Length(aRecipe.Values) then
      Cells[i+1, aRow] := ValueToText(aRecipe.Values[i], FFields[i].Decimals)
    else
      Cells[i+1, aRow] := ValueToText(FFields[i].DefaultValue, FFields[i].Decimals);
  end;
end;

procedure THMIRecipeGrid.FillCells;
var
  i: Integer;
  TargetCols: Integer;
begin
  if csDestroying in ComponentState then exit;
  FUpdating := True;
  try
    //minimo duas colunas (nome + uma de parametro, vazia) e duas linhas
    //(cabecalho + uma de receita): uma grade de coluna unica faria qualquer
    //leitura de Cells[1,x] - nossa, do LCL ou do usuario - levantar
    //EGridException "Index Out of range Cell[Col=1 Row=0]".
    //
    //minimum of two columns (the name + one empty parameter column) and two
    //rows (the header + one recipe row): a single column grid makes any
    //Cells[1,x] read - ours, the LCL's or the user's - raise EGridException
    //"Index Out of range Cell[Col=1 Row=0]".
    if FFields.Count + 1 < 2 then
      TargetCols := 2
    else
      TargetCols := FFields.Count + 1;
    if ColCount <> TargetCols then
      ColCount := TargetCols;
    if RowCount <> FRecipes.Count + 1 then
      RowCount := FRecipes.Count + 1;
    if RowCount < 2 then
      RowCount := 2;

    Cells[0, 0] := FNameColumnTitle;
    for i := 0 to FFields.Count-1 do
      Cells[i+1, 0] := FieldCaption(i);

    for i := 0 to FRecipes.Count-1 do
      FillRecipeCells(i+1, TRecipe(FRecipes[i]));

    //linha vazia sobrando quando nao ha receita nenhuma.
    //empty row left over when there is no recipe at all.
    if FRecipes.Count = 0 then begin
      Cells[0, 1] := '';
      for i := 0 to ColCount-2 do
        Cells[i+1, 1] := '';
    end;
  finally
    FUpdating := False;
  end;
  Invalidate;
end;

procedure THMIRecipeGrid.RefreshCells;
var
  i, j, oldCount: Integer;
  r: TRecipe;
begin
  //campos novos entram com o valor padrao; campos removidos saem do fim.
  //new fields come in with their default value; removed fields drop from the end.
  for i := 0 to FRecipes.Count-1 do begin
    r := TRecipe(FRecipes[i]);
    oldCount := Length(r.Values);
    r.SetFieldCount(FFields.Count);
    for j := oldCount to FFields.Count-1 do
      r.Values[j] := FFields[j].DefaultValue;
  end;
  FillCells;
end;

procedure THMIRecipeGrid.SetEditText(ACol, ARow: Longint; const Value: string);
var
  field: TRecipeField;
  recipe: TRecipe;
  v: Double;
  newName: string;
begin
  if FUpdating then exit;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then begin
    inherited SetEditText(ACol, ARow, Value);
    exit;
  end;

  //cabecalho e linha inexistente nao aceitam edicao.
  //the header and a non existing row take no edit.
  if (ARow < 1) or (ARow > FRecipes.Count) then exit;

  recipe := TRecipe(FRecipes[ARow-1]);

  if ACol = 0 then begin
    newName := Trim(Value);
    if newName = '' then begin
      DoError('The recipe name cannot be empty.');
      FillRecipeCells(ARow, recipe);
      exit;
    end;
    if SameText(newName, recipe.Name) then begin
      FillRecipeCells(ARow, recipe);
      exit;
    end;
    if IndexOfRecipe(newName) >= 0 then begin
      DoError('There is already a recipe named "' + newName + '".');
      FillRecipeCells(ARow, recipe);
      exit;
    end;
    recipe.Name := newName;
    Cells[0, ARow] := newName;
    RecipeChanged;
    exit;
  end;

  if (ACol < 1) or (ACol > FFields.Count) then exit;
  field := FFields[ACol-1];

  if not TextToValue(Value, v) then begin
    DoError('"' + Value + '" is not a valid number for ' + FieldCaption(ACol-1) + '.');
    FillRecipeCells(ARow, recipe);
    exit;
  end;

  if field.CheckRange then begin
    if (v < field.MinValue) or (v > field.MaxValue) then begin
      DoError(Format('%s: %s is out of the range %s .. %s.',
        [FieldCaption(ACol-1), ValueToText(v, field.Decimals),
         ValueToText(field.MinValue, field.Decimals),
         ValueToText(field.MaxValue, field.Decimals)]));
      v := EnsureRange(v, field.MinValue, field.MaxValue);
    end;
  end;

  if ACol-1 < Length(recipe.Values) then
    recipe.Values[ACol-1] := v
  else begin
    recipe.SetFieldCount(FFields.Count);
    recipe.Values[ACol-1] := v;
  end;
  Cells[ACol, ARow] := ValueToText(v, field.Decimals);
  RecipeChanged;
end;

procedure THMIRecipeGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  oldStyle: TFontStyles;
  oldColor: TColor;
begin
  oldStyle := Canvas.Font.Style;
  oldColor := Canvas.Brush.Color;
  if ARow = 0 then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold]
  else if ACol = 0 then
    Canvas.Brush.Color := clBtnFace;
  inherited DrawCell(ACol, ARow, ARect, AState);
  Canvas.Font.Style := oldStyle;
  Canvas.Brush.Color := oldColor;
end;

function THMIRecipeGrid.AddRecipe(const aName: string): Integer;
var
  r: TRecipe;
  i: Integer;
  rname: string;
begin
  rname := Trim(aName);
  if rname = '' then
    rname := 'Recipe ' + IntToStr(FRecipes.Count+1);
  if IndexOfRecipe(rname) >= 0 then begin
    DoError('There is already a recipe named "' + rname + '".');
    Result := -1;
    exit;
  end;
  r := TRecipe.Create(rname, FFields.Count);
  for i := 0 to FFields.Count-1 do
    r.Values[i] := FFields[i].DefaultValue;
  Result := FRecipes.Add(r);
  FillCells;
  RecipeChanged;
end;

procedure THMIRecipeGrid.DeleteRecipe(aIndex: Integer);
begin
  if (aIndex < 0) or (aIndex >= FRecipes.Count) then exit;
  TRecipe(FRecipes[aIndex]).Free;
  FRecipes.Delete(aIndex);
  FillCells;
  RecipeChanged;
end;

procedure THMIRecipeGrid.DeleteRecipe(const aName: string);
begin
  DeleteRecipe(IndexOfRecipe(aName));
end;

function THMIRecipeGrid.DuplicateRecipe(const aName, aNewName: string): Integer;
var
  src, dst: TRecipe;
  i: Integer;
  rname: string;
begin
  Result := -1;
  i := IndexOfRecipe(aName);
  if i < 0 then begin
    DoError('Recipe "' + aName + '" not found.');
    exit;
  end;
  src := TRecipe(FRecipes[i]);
  rname := Trim(aNewName);
  if rname = '' then begin
    rname := aName + ' copy';
    i := 2;
    while IndexOfRecipe(rname) >= 0 do begin
      rname := aName + ' copy ' + IntToStr(i);
      Inc(i);
    end;
  end else
    if IndexOfRecipe(rname) >= 0 then begin
      DoError('There is already a recipe named "' + rname + '".');
      exit;
    end;

  dst := TRecipe.Create(rname, FFields.Count);
  for i := 0 to FFields.Count-1 do
    if i < Length(src.Values) then
      dst.Values[i] := src.Values[i]
    else
      dst.Values[i] := FFields[i].DefaultValue;
  Result := FRecipes.Add(dst);
  FillCells;
  RecipeChanged;
end;

function THMIRecipeGrid.RenameRecipe(const aOldName, aNewName: string): Boolean;
var
  i: Integer;
  rname: string;
begin
  Result := False;
  i := IndexOfRecipe(aOldName);
  if i < 0 then begin
    DoError('Recipe "' + aOldName + '" not found.');
    exit;
  end;
  rname := Trim(aNewName);
  if rname = '' then begin
    DoError('The recipe name cannot be empty.');
    exit;
  end;
  if SameText(rname, aOldName) then begin
    Result := True;
    exit;
  end;
  if IndexOfRecipe(rname) >= 0 then begin
    DoError('There is already a recipe named "' + rname + '".');
    exit;
  end;
  TRecipe(FRecipes[i]).Name := rname;
  FillCells;
  RecipeChanged;
  Result := True;
end;

function THMIRecipeGrid.IndexOfRecipe(const aName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FRecipes.Count-1 do
    if SameText(TRecipe(FRecipes[i]).Name, aName) then begin
      Result := i;
      exit;
    end;
end;

function THMIRecipeGrid.RecipeName(aIndex: Integer): string;
begin
  if (aIndex >= 0) and (aIndex < FRecipes.Count) then
    Result := TRecipe(FRecipes[aIndex]).Name
  else
    Result := '';
end;

function THMIRecipeGrid.ActiveRecipeName: string;
begin
  Result := RecipeName(Row-1);
end;

procedure THMIRecipeGrid.SelectRecipe(const aName: string);
var
  i: Integer;
begin
  i := IndexOfRecipe(aName);
  if i >= 0 then
    Row := i+1;
end;

function THMIRecipeGrid.FieldByName(const aName: string): TRecipeField;
begin
  Result := FFields.ItemByName(aName);
end;

function THMIRecipeGrid.ValueOf(aRecipeIndex, aFieldIndex: Integer): Double;
var
  r: TRecipe;
begin
  Result := 0;
  r := GetRecipe(aRecipeIndex);
  if (r = nil) or (aFieldIndex < 0) or (aFieldIndex >= Length(r.Values)) then exit;
  Result := r.Values[aFieldIndex];
end;

procedure THMIRecipeGrid.Download;
var
  i, written: Integer;
  field: TRecipeField;
  numeric: ITagNumeric;
  r: TRecipe;
begin
  if (Row < 1) or (Row > FRecipes.Count) then begin
    DoError('Select a recipe before downloading.');
    exit;
  end;
  r := TRecipe(FRecipes[Row-1]);
  written := 0;
  for i := 0 to FFields.Count-1 do begin
    field := FFields[i];
    if field.PLCTag = nil then
      Continue;
    if not Supports(field.PLCTag, ITagNumeric, numeric) then begin
      DoError(FieldCaption(i) + ': the linked tag is not numeric.');
      Continue;
    end;
    if i >= Length(r.Values) then
      Continue;
    numeric.Value := r.Values[i];
    Inc(written);
  end;
  if written = 0 then
    DoError('No column has a numeric tag linked, nothing was written.')
  else
    if Assigned(FOnDownload) then
      FOnDownload(Self, r.Name);
end;

procedure THMIRecipeGrid.Upload;
var
  i, read: Integer;
  field: TRecipeField;
  numeric: ITagNumeric;
  r: TRecipe;
begin
  if (Row < 1) or (Row > FRecipes.Count) then begin
    DoError('Select a recipe before uploading.');
    exit;
  end;
  r := TRecipe(FRecipes[Row-1]);
  if Length(r.Values) <> FFields.Count then
    r.SetFieldCount(FFields.Count);
  read := 0;
  for i := 0 to FFields.Count-1 do begin
    field := FFields[i];
    if field.PLCTag = nil then
      Continue;
    if not Supports(field.PLCTag, ITagNumeric, numeric) then begin
      DoError(FieldCaption(i) + ': the linked tag is not numeric.');
      Continue;
    end;
    r.Values[i] := numeric.Value;
    Inc(read);
  end;
  FillRecipeCells(Row, r);
  if read = 0 then
    DoError('No column has a numeric tag linked, nothing was read.')
  else begin
    RecipeChanged;
    if Assigned(FOnUpload) then
      FOnUpload(Self, r.Name);
  end;
end;

procedure THMIRecipeGrid.SaveLibrary;
var
  sl: TStringList;
  i, j: Integer;
  r: TRecipe;
  dir, line: string;
begin
  sl := TStringList.Create;
  try
    sl.Add(RECIPE_FILE_HEADER);
    sl.Add(RECIPE_FILE_VERSION);
    for i := 0 to FRecipes.Count-1 do begin
      r := TRecipe(FRecipes[i]);
      sl.Add('');
      sl.Add('[' + StringReplace(StringReplace(r.Name, '[', '(', [rfReplaceAll]),
                                 ']', ')', [rfReplaceAll]) + ']');
      for j := 0 to FFields.Count-1 do begin
        if j < Length(r.Values) then
          line := ValueToText(r.Values[j], 6)
        else
          line := ValueToText(FFields[j].DefaultValue, 6);
        sl.Add(FieldKey(j) + '=' + line);
      end;
      //guarda tambem o indice de cada campo, para o caso de renomeacao.
      //also stores each field ordinal, for the case of a renamed field.
      for j := 0 to FFields.Count-1 do begin
        if j < Length(r.Values) then
          line := ValueToText(r.Values[j], 6)
        else
          line := ValueToText(FFields[j].DefaultValue, 6);
        sl.Add('$' + IntToStr(j) + '=' + line);
      end;
    end;
    dir := ExtractFilePath(LibraryFileName);
    if (dir <> '') and not DirectoryExists(dir) then
      ForceDirectories(dir);
    try
      sl.SaveToFile(LibraryFileName);
    except
      on E: Exception do
        DoError('Could not write "' + LibraryFileName + '": ' + E.Message);
    end;
  finally
    sl.Free;
  end;
end;

procedure THMIRecipeGrid.LoadLibrary;
var
  sl: TStringList;
  temp: TList;
  i, j, f: Integer;
  line, key, value: string;
  r: TRecipe;
begin
  if not FileExists(LibraryFileName) then begin
    DoError('Recipe library "' + LibraryFileName + '" not found.');
    exit;
  end;
  sl := TStringList.Create;
  temp := TList.Create;
  r := nil;
  try
    try
      sl.LoadFromFile(LibraryFileName);
    except
      on E: Exception do begin
        DoError('Could not read "' + LibraryFileName + '": ' + E.Message);
        exit;
      end;
    end;

    //monta a biblioteca numa lista temporaria; a atual so e' trocada no fim,
    //para um arquivo invalido nao apagar o que ja estava na tela.
    //builds the library in a temporary list; the current one is swapped only
    //at the end, so a bad file does not wipe what is already on screen.
    for i := 0 to sl.Count-1 do begin
      line := Trim(sl[i]);
      if (line = '') or (line[1] = '#') then Continue;
      if line[1] = '[' then begin
        if (Length(line) > 2) and (line[Length(line)] = ']') then begin
          if r <> nil then temp.Add(r);
          r := TRecipe.Create(Copy(line, 2, Length(line)-2), FFields.Count);
        end;
        Continue;
      end;
      if r = nil then Continue;
      j := Pos('=', line);
      if j <= 0 then Continue;
      key := Trim(Copy(line, 1, j-1));
      value := Trim(Copy(line, j+1, MaxInt));
      f := FieldIndexByKey(key);
      if (f >= 0) and (f < Length(r.Values)) then
        r.Values[f] := StrToFloatDef(StringReplace(value, ',', '.', [rfReplaceAll]),
                                     0, InvariantSettings);
    end;
    if r <> nil then temp.Add(r);

    if temp.Count = 0 then begin
      DoError('No recipe found in "' + LibraryFileName + '".');
      exit;
    end;

    for i := 0 to FRecipes.Count-1 do
      TRecipe(FRecipes[i]).Free;
    FRecipes.Clear;
    for i := 0 to temp.Count-1 do
      FRecipes.Add(temp[i]);
    temp.Clear;
  finally
    for i := 0 to temp.Count-1 do
      TRecipe(temp[i]).Free;
    temp.Free;
    sl.Free;
  end;
  FillCells;
  RecipeChanged;
end;

end.
