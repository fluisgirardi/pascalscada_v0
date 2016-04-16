unit dbstructurechecker;

interface

uses
  sysutils, db, Classes;

type

  TTableMetadata = class; //forward declaration.
  TDatabaseMetadata = class; //forward declaration.

  TDatabaseObjectState = (dosUnknown, dosChanged, dosDontExists, dosOK);
  TDatabaseNameKind    = (dbkTableName, dbkFieldName, dbkIndexName); //must be improved;

  { TDatabaseObject }

  TDatabaseObject = class(TObject)
  protected
    FState:TDatabaseObjectState;
    FGenerateDDL:Boolean;
    function  ValidateName(Name:AnsiString; NameKind:TDatabaseNameKind):Boolean; virtual;
    function  GetCurrentState:TDatabaseObjectState; virtual;
    procedure ResetState; virtual;
    function  GenerateDDL:AnsiString;
  end;

  //simple index declaration (for primary and unique keys)
  TIndex = class(TDatabaseObject)
  protected
    FTableOwner:TTableMetadata;
    FIndexName:AnsiString;
    FFields:TStringList;
    procedure AddFieldToIndex(FieldName:AnsiString); virtual;
    function GetFieldCount:LongInt;
    function GetField(index:LongInt):AnsiString;
  public
    constructor Create(OwnerTable:TTableMetadata; IndexName:AnsiString);
    destructor Destroy; override;

    function GetCurrentState:TDatabaseObjectState; override;

    property IndexName:AnsiString read FIndexName;
    property FieldCount:LongInt read GetFieldCount;
    property IndexField[index:LongInt]:AnsiString read GetField;
  end;

  TUniqueIndex = class(TIndex)
  public
    procedure AddFieldToIndex(FieldName:AnsiString); override;
  end;

  TUniqueIndexClass = class of TUniqueIndex;

  TPrimaryKeyIndex = class(TIndex)
  public
    procedure AddFieldToIndex(FieldName: AnsiString); override;
  end;

  TPrimaryKeyIndexClass = class of TPrimaryKeyIndex;

  TFieldLink = record
    SourceField,
    Field:AnsiString;
  end;

  TFieldLinks = array of TFieldLink;

  TForeignKeyRestriction = (fkrNoAction, fkrRestrict, fkrCascade);

  TForeignKey = class(TIndex)
  protected
    SourceTable:TTableMetadata;
    FieldLinks:TFieldLinks;
    FUpdateAction,
    FDeleteAction:TForeignKeyRestriction;
  public
    constructor Create(OwnerTable:TTableMetadata; aIndexName, aSourceTable:AnsiString;
                       UpdateAction:TForeignKeyRestriction = fkrNoAction;
                       DeleteAction:TForeignKeyRestriction = fkrNoAction);
    destructor Destroy; override;
    procedure addFieldLink(SourceField, Field:AnsiString);
  end;

  TForeignKeyClass = class of TForeignKey;

  { TCollumnDefinition }

  TCollumnDefinition = class(TObject)
  private
    FFieldName   :AnsiString;
    FFieldType   :TFieldType;
    FNotNull     :Boolean;
    FDefaultValue:AnsiString;
    FSize        :LongInt; //string size
    FOwnerTable  :TTableMetadata;
  public
    constructor Create(OnwerTable:TTableMetadata; FieldName:AnsiString; FieldType:TFieldType; Size:LongInt = -1; Nullable:Boolean = true; DefaultValue:AnsiString = '');
    destructor Destroy;  override;
    property FieldName   :AnsiString read FFieldName;
    property FieldType   :TFieldType read FFieldType;
    property NotNull     :Boolean    read FNotNull     write FNotNull;
    property DefaultValue:AnsiString read FDefaultValue;
    property Size        :LongInt    read FSize;
  end;

  TCollumnDefinitionClass = Class of TCollumnDefinition;

  { TTableMetadata }

  TTableMetadata = class(TDatabaseObject)
  private
    FFields:array of TCollumnDefinition;
    FOwnerDatabase: TDatabaseMetadata;
    FPK:TPrimaryKeyIndex;
    FTableName: AnsiString;
    FUniqueIndexes:array of TUniqueIndex;
  public
    constructor Create(OwnerDatabase:TDatabaseMetadata; TableName:AnsiString);
    destructor Destroy; override;
    function addCollumn(FieldName:AnsiString; FieldType:TFieldType; Size:LongInt = -1; NotNull:Boolean = false; DefaultValue:AnsiString = ''):TCollumnDefinition;
    function addPrimaryKey(pkName:AnsiString):TPrimaryKeyIndex;
    function addUniqueIndex(uniquename:AnsiString):TUniqueIndex;
    function addForeignKey(IndexName, SourceTable:AnsiString;
                            UpdateAction:TForeignKeyRestriction = fkrNoAction;
                            DeleteAction:TForeignKeyRestriction = fkrNoAction):TForeignKey;
  public
    function ValidateName(Name: AnsiString; NameKind: TDatabaseNameKind): Boolean;
       override;
    function FieldExists(fieldname:AnsiString; var field:TCollumnDefinition):Boolean;
    function GetCurrentState:TDatabaseObjectState; override;
    procedure ResetState; override;
    property TableName:AnsiString read FTableName;
    property OwnerDatabase:TDatabaseMetadata read FOwnerDatabase;
  end;

  TTableMetadataClass = Class of TTableMetadata;

   { TDatabaseMetadata }

   TDatabaseMetadata = class(TDatabaseObject)
   protected
     FTables:TList;
     FTableMetadataClass:TTableMetadataClass;
   public
     constructor Create; virtual;
     function   ValidateName(Name:AnsiString; NameKind:TDatabaseNameKind):Boolean; override;
     destructor Destroy; override;
     function   AddTable(TableName:AnsiString):TTableMetadata;
     procedure  DeleteTable(TableName:AnsiString);
     function   FindTableDef(TableName:AnsiString; var index:LongInt):TTableMetadata; overload;
     function   GetCurrentState: TDatabaseObjectState; override;
     procedure  ResetState; override;
   end;

   function SortTableList(Item1, Item2: Pointer): LongInt;

implementation

{ TDatabaseMetadata }

constructor TDatabaseMetadata.Create;
begin
  FTableMetadataClass:=TTableMetadata;
end;

function TDatabaseMetadata.ValidateName(Name: AnsiString;
  NameKind: TDatabaseNameKind): Boolean;
begin
  Result:=true;
end;

destructor TDatabaseMetadata.Destroy;
var
  i:LongInt;
begin
  inherited Destroy;
  //starts from the end
  for i:=FTables.Count-1 downto 0 do begin
    TTableMetadata(FTables[i]).Destroy;
    FTables.Delete(i);
  end;
  FTables.Destroy;
end;

function TDatabaseMetadata.AddTable(TableName: AnsiString):TTableMetadata;
var
  tabledef:TTableMetadata;
  h:LongInt;
begin
  tabledef:=FindTableDef(TableName, h);
  Result:=nil;
  if tabledef=nil then begin
    tabledef:=FTableMetadataClass.Create(Self,TableName);
    FTables.Add(tabledef);
    Result:=tabledef;
    FTables.Sort(@SortTableList);
  end else
    raise exception.Create('Tabela já existe no metadados.');

end;

procedure TDatabaseMetadata.DeleteTable(TableName: AnsiString);
var
  tabledef:TTableMetadata;
  i:LongInt;
begin
  tabledef:=FindTableDef(TableName, i);

  if tabledef=nil then exit;

  tabledef.Destroy;
  FTables.Delete(i);
end;

function TDatabaseMetadata.FindTableDef(TableName: AnsiString; var index:LongInt): TTableMetadata;
var
  i:LongInt;
begin
  index:=-1;
  Result:=nil;
  //binary search here?
  for i:=0 to FTables.Count-1 do begin
    if TTableMetadata(FTables.Items[i]).TableName=TableName then begin
      Result:=TTableMetadata(FTables.Items[i]);
      index:=i;
      exit;
    end;
  end;
end;

function TDatabaseMetadata.GetCurrentState: TDatabaseObjectState;
var
  i: LongInt;
begin
  Result:=dosOK;
  for i:=0 to FTables.Count-1 do
    case TTableMetadata(FTables[i]).GetCurrentState of
      dosUnknown:
        raise exception.Create('Resposta inesperada!');

      dosChanged, dosDontExists: begin
        Result:=dosChanged;
        break;
      end;

      dosOK:
        continue;
    end;

end;

procedure TDatabaseMetadata.ResetState;
var
  i:LongInt;
begin
  inherited ResetState;
  for i:=0 to FTables.Count-1 do begin
    TTableMetadata(FTables[i]).ResetState;
  end;
end;

{ TCollumnDefinition }

constructor TCollumnDefinition.Create(OnwerTable: TTableMetadata;
  FieldName: AnsiString; FieldType: TFieldType; Size: LongInt; Nullable: Boolean;
  DefaultValue: AnsiString);
begin

end;

destructor TCollumnDefinition.Destroy;
begin
  inherited Destroy;
end;

{ TDatabaseObject }

function TDatabaseObject.ValidateName(Name: AnsiString; NameKind: TDatabaseNameKind
  ): Boolean;
begin
  Result:=true;
end;

function TDatabaseObject.GetCurrentState: TDatabaseObjectState;
begin
  Result:=dosUnknown;
end;

procedure TDatabaseObject.ResetState;
begin
  FState:=dosUnknown;
end;

function TDatabaseObject.GenerateDDL: AnsiString;
begin
  Result:='';
end;

{ TTableMetadata }

constructor TTableMetadata.Create(OwnerDatabase: TDatabaseMetadata;
  TableName: AnsiString);
begin
  inherited Create;
  if (OwnerDatabase=nil) then
    raise Exception.Create('Banco de dados inválido!');

  if (not OwnerDatabase.ValidateName(TableName,dbkTableName)) then
    raise Exception.Create('Nome invalido para a tabela');

  FTableName:=TableName;
  FOwnerDatabase:=OwnerDatabase;
end;

destructor TTableMetadata.Destroy;
begin
  inherited Destroy;
end;

function TTableMetadata.addCollumn(FieldName: AnsiString; FieldType: TFieldType;
  Size: LongInt; NotNull: Boolean; DefaultValue: AnsiString): TCollumnDefinition;
begin

end;

function TTableMetadata.addPrimaryKey(pkName: AnsiString): TPrimaryKeyIndex;
begin

end;

function TTableMetadata.addUniqueIndex(uniquename: AnsiString): TUniqueIndex;
begin

end;

function TTableMetadata.addForeignKey(IndexName, SourceTable: AnsiString;
  UpdateAction: TForeignKeyRestriction; DeleteAction: TForeignKeyRestriction
  ): TForeignKey;
begin

end;

function TTableMetadata.ValidateName(Name: AnsiString; NameKind: TDatabaseNameKind
  ): Boolean;
begin
  if FOwnerDatabase<>nil then
    Result:=FOwnerDatabase.ValidateName(Name,NameKind)
  else
    raise Exception.Create('Invalid Database');
end;

function TTableMetadata.FieldExists(fieldname: AnsiString;
  var field: TCollumnDefinition): Boolean;
begin

end;

function TTableMetadata.GetCurrentState: TDatabaseObjectState;
begin
  Result:=inherited GetCurrentState;
end;

procedure TTableMetadata.ResetState;
begin
  inherited ResetState;
end;

constructor TIndex.Create(OwnerTable:TTableMetadata; IndexName:AnsiString);
begin
  //TODO: must validate the index name first with the database driver.
  //TODO: must check if the name of the index don't already exists on schema.
  inherited Create;

  //check if index name is valid.
  if not OwnerTable.ValidateName(IndexName, dbkIndexName) then
    raise Exception.Create('Invalid index name');

  FTableOwner:=OwnerTable;
  FIndexName:=IndexName;
  FFields:=TStringList.Create;
end;

destructor TIndex.Destroy;
begin
  FFields.Destroy;
  inherited Destroy;
end;

procedure  TIndex.AddFieldToIndex(FieldName:AnsiString);
var
  ffield:TCollumnDefinition;
  found:Boolean;
  c:LongInt;
begin
  if (FTableOwner=nil) or (not FTableOwner.FieldExists(FieldName,ffield)) then
    raise Exception.Create('O Campo nao existe na tabela!');

  found:=False;
  for c:=0 to FFields.Count-1 do
    if FFields.Strings[c]=lowercase(FieldName) then begin
      found:=true;
      break;
    end;

  if found then
    raise Exception.Create('O campo já existe no indice!');

  FFields.Add(lowercase(FieldName));
end;

function TIndex.GetFieldCount:LongInt;
begin
  Result:=FFields.Count;
end;

function TIndex.GetField(index:LongInt):AnsiString;
begin
  if (index<0) or (index>=FFields.Count) then
    raise Exception.Create('Fora dos limites!');

  Result:=FFields[index];
end;

function   TIndex.GetCurrentState:TDatabaseObjectState;
begin
  Result:=dosUnknown; //TODO: must check itself with database driver.
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUniqueIndex.AddFieldToIndex(FieldName:AnsiString);
begin
  inherited AddFieldToIndex(FieldName);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPrimaryKeyIndex.AddFieldToIndex(FieldName: AnsiString);
var
  ffield:TCollumnDefinition;
begin
  inherited AddFieldToIndex(FieldName);
  if FTableOwner.FieldExists(FieldName,ffield) then
    ffield.NotNull:=true;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TForeignKey.Create(OwnerTable:TTableMetadata; aIndexName,
                               aSourceTable:AnsiString;
                               UpdateAction:TForeignKeyRestriction = fkrNoAction;
                               DeleteAction:TForeignKeyRestriction = fkrNoAction);
begin
  inherited Create(OwnerTable,IndexName);
  FDeleteAction:=DeleteAction;
  FUpdateAction:=UpdateAction;
  //must find the source table by their name.
end;

destructor  TForeignKey.Destroy;
begin
   inherited Destroy;
end;

procedure   TForeignKey.addFieldLink(SourceField, Field:AnsiString);
begin

end;

function SortTableList(Item1, Item2: Pointer): LongInt;
begin
  if TTableMetadata(item1).TableName=TTableMetadata(Item2).TableName then
    Result:=0
  else begin
    if TTableMetadata(item1).TableName<TTableMetadata(Item2).TableName then
      Result:=-1
    else
      Result:=1;
  end;
end;

end.

