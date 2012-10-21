unit dbstructurechecker;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  sysutils, db, Classes;

type

  TTableMetadata = class; //forward declaration.
  TDatabaseMetadata = class; //forward declaration.

  TDatabaseObjectState = (dosUnknown, dosChanged, dosDontExists, dosOK);
  TDatabaseNameBehavior = (dbnTableName, dbnFieldName, dbnIndexName);

  { TDatabaseObject }

  TDatabaseObject = class(TObject)
  protected
    FState:TDatabaseObjectState;
    function GetCurrentState:TDatabaseObjectState; virtual;
    procedure ResetState; virtual;
  end;

  //simple index declaration (for primary and unique keys)
  TIndex = class(TDatabaseObject)
  protected
    FTableOwner:TTableMetadata;
    FIndexName:string;
    FFields:TStringList;
    procedure AddFieldToIndex(FieldName:String); virtual;
    function GetFieldCount:Integer;
    function GetField(index:Integer):String;
  public
    constructor Create(OwnerTable:TTableMetadata; IndexName:String);
    destructor Destroy; override;

    function GetCurrentState:TDatabaseObjectState; override;

    property IndexName:String read FIndexName;
    property FieldCount:Integer read GetFieldCount;
    property IndexField[index:Integer]:String read GetField;
  end;

  TUniqueIndex = class(TIndex)
  public
    procedure AddFieldToIndex(FieldName:String); override;
  end;

  TPrimaryKeyIndex = class(TIndex)
  public
    procedure AddFieldToIndex(FieldName: String); override;
  end;

  TFieldLink = record
    SourceField,
    Field:String;
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
    constructor Create(OwnerTable:TTableMetadata; IndexName, SourceTable:String;
                       UpdateAction:TForeignKeyRestriction = fkrNoAction;
                       DeleteAction:TForeignKeyRestriction = fkrNoAction);
    destructor Destroy; override;
    procedure addFieldLink(SourceField, Field:String);
  end;

  { TCollumnDefinition }

  TCollumnDefinition = class(TObject)
  private
    FFieldName   :String;
    FFieldType   :TFieldType;
    FNullable    :Boolean;
    FDefaultValue:String;
    FSize        :Integer; //string size
    FOwnerTable  :TTableMetadata;
  public
    constructor Create(OnwerTable:TTableMetadata; FieldName:String; FieldType:TFieldType; Size:Integer = -1; Nullable:Boolean = true; DefaultValue:String = '');
    destructor Destroy;  override;
    property FieldName   :String     read FFieldName;
    property FieldType   :TFieldType read FFieldType;
    property Nullable    :Boolean    read FNullable     write FNullable;
    property DefaultValue:String     read FDefaultValue;
    property Size        :Integer    read FSize;
  end;

  { TTableMetadata }

  TTableMetadata = class(TDatabaseObject)
  private
    FFields:array of TCollumnDefinition;
    FOwnerDatabase: TDatabaseMetadata;
    FPK:TPrimaryKeyIndex;
    FTableName: String;
    FUniqueIndexes:array of TUniqueIndex;
  public
    constructor Create(OwnerDatabase:TDatabaseMetadata; TableName:String);
    destructor Destroy; override;
    procedure addCollumn(FieldName:String; FieldType:TFieldType; Size:Integer = -1; Nullable:Boolean = true; DefaultValue:String = '');
    function  addPrimaryKey(pkName:String):TPrimaryKeyIndex;
    function  addUniqueIndex(uniquename:String):TUniqueIndex;
    function  addForeignKey(IndexName, SourceTable:String;
                            UpdateAction:TForeignKeyRestriction = fkrNoAction;
                            DeleteAction:TForeignKeyRestriction = fkrNoAction):TForeignKey;
  public
    function FieldExists(fieldname:String; var field:TCollumnDefinition):Boolean;
    function GetCurrentState:TDatabaseObjectState; override;
    procedure ResetState; override;
    property TableName:String read FTableName;
    property OwnerDatabase:TDatabaseMetadata read FOwnerDatabase;
  end;

   { TDatabaseMetadata }

   TDatabaseMetadata = class(TDatabaseObject)
   protected
     FTables:TList;
   public
     destructor Destroy; override;
     function   AddTable(TableName:String):TTableMetadata;
     procedure  DeleteTable(TableName:String);
     function   FindTableDef(TableName:String; var index:Integer):TTableMetadata; overload;
     function   GetCurrentState: TDatabaseObjectState; override;
     procedure  ResetState; override;
   end;

   function SortTableList(Item1, Item2: Pointer): Integer;

implementation

{ TDatabaseMetadata }

destructor TDatabaseMetadata.Destroy;
var
  i:Integer;
begin
  inherited Destroy;
  for i:=FTables.Count-1 downto 0 do begin
    TTableMetadata(FTables[i]).Destroy;
    FTables.Delete(i);
  end;
  FTables.Destroy;
end;

function TDatabaseMetadata.AddTable(TableName: String):TTableMetadata;
var
  tabledef:TTableMetadata;
  h:Integer;
begin
  tabledef:=FindTableDef(TableName, h);
  Result:=nil;
  if tabledef=nil then begin
    tabledef:=TTableMetadata.Create(Self,TableName);
    FTables.Add(tabledef);
    Result:=tabledef;
    FTables.Sort(SortTableList);
  end else
    raise exception.Create('Tabela já existe no metadados.');

end;

procedure TDatabaseMetadata.DeleteTable(TableName: String);
var
  tabledef:TTableMetadata;
  i:Integer;
begin
  tabledef:=FindTableDef(TableName, i);

  if tabledef=nil then exit;

  tabledef.Destroy;
  FTables.Delete(i);
end;

function TDatabaseMetadata.FindTableDef(TableName: String; var index:Integer): TTableMetadata;
var
  i:Integer;
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
begin
  Result:=inherited GetCurrentState;
end;

procedure TDatabaseMetadata.ResetState;
var
  i:Integer;
begin
  inherited ResetState;
  for i:=0 to FTables.Count-1 do begin
    TTableMetadata(FTables[i]).ResetState;
  end;
end;

{ TCollumnDefinition }

constructor TCollumnDefinition.Create(OnwerTable: TTableMetadata;
  FieldName: String; FieldType: TFieldType; Size: Integer; Nullable: Boolean;
  DefaultValue: String);
begin

end;

destructor TCollumnDefinition.Destroy;
begin
  inherited Destroy;
end;

{ TDatabaseObject }

function TDatabaseObject.GetCurrentState: TDatabaseObjectState;
begin

end;

procedure TDatabaseObject.ResetState;
begin

end;

{ TTableMetadata }

constructor TTableMetadata.Create(OwnerDatabase: TDatabaseMetadata;
  TableName: String);
begin
  inherited Create;
  FTableName:=TableName;
  FOwnerDatabase:=OwnerDatabase;
end;

destructor TTableMetadata.Destroy;
begin
  inherited Destroy;
end;

procedure TTableMetadata.addCollumn(FieldName: String; FieldType: TFieldType;
  Size: Integer; Nullable: Boolean; DefaultValue: String);
begin

end;

function TTableMetadata.addPrimaryKey(pkName: String): TPrimaryKeyIndex;
begin

end;

function TTableMetadata.addUniqueIndex(uniquename: String): TUniqueIndex;
begin

end;

function TTableMetadata.addForeignKey(IndexName, SourceTable: String;
  UpdateAction: TForeignKeyRestriction; DeleteAction: TForeignKeyRestriction
  ): TForeignKey;
begin

end;

function TTableMetadata.FieldExists(fieldname: String;
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

constructor TIndex.Create(OwnerTable:TTableMetadata; IndexName:String);
begin
  //TODO: must validate the index name first with the database driver.
  //TODO: must check if the name of the index don't already exists on schema.
  inherited Create;
  FTableOwner:=OwnerTable;
  FIndexName:=IndexName;
  FFields:=TStringList.Create;
end;

destructor TIndex.Destroy;
begin
  FFields.Destroy;
  inherited Destroy;
end;

procedure  TIndex.AddFieldToIndex(FieldName:String);
var
  ffield:TCollumnDefinition;
  found:Boolean;
  c:Integer;
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

function TIndex.GetFieldCount:Integer;
begin
  Result:=FFields.Count;
end;

function TIndex.GetField(index:Integer):String;
begin
  if (index<0) or (index>=FFields.Count) then
    raise Exception.Create('Fora dos limites!');

  Result:=FFields[index];
end;

function   TIndex.GetCurrentState:TDatabaseObjectState;
begin
  //TODO: must check itself with database driver.
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUniqueIndex.AddFieldToIndex(FieldName:String);
begin
  inherited AddFieldToIndex(FieldName);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPrimaryKeyIndex.AddFieldToIndex(FieldName: String);
var
  ffield:TCollumnDefinition;
begin
  inherited AddFieldToIndex(FieldName);
  if FTableOwner.FieldExists(FieldName,ffield) then
    ffield.Nullable:=false;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TForeignKey.Create(OwnerTable:TTableMetadata; IndexName,
                               SourceTable:String;
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

procedure   TForeignKey.addFieldLink(SourceField, Field:String);
begin

end;

function SortTableList(Item1, Item2: Pointer): Integer;
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

