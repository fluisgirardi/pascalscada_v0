unit dbstructurechecker;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  sysutils, db;

type

  TTableDefinition = class; //forward declaration.
  TDatabaseDefinition = class; //forward declaration.

  TArrayOfString = array of string;
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
    FTableOwner:TTableDefinition;
    FIndexName:string;
    FFields:TArrayOfString;
    procedure AddFieldToIndex(FieldName:String); virtual;
    function GetFieldCount:Integer;
    function GetField(index:Integer):String;
  public
    constructor Create(OwnerTable:TTableDefinition; IndexName:String);
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
    SourceTable:TTableDefinition;
    FieldLinks:TFieldLinks;
    FUpdateAction,
    FDeleteAction:TForeignKeyRestriction;
  public
    constructor Create(OwnerTable:TTableDefinition; IndexName, SourceTable:String;
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
    FOwnerTable  :TTableDefinition;
  public
    constructor Create(OnwerTable:TTableDefinition; FieldName:String; FieldType:TFieldType; Size:Integer = -1; Nullable:Boolean = true; DefaultValue:String = '');
    destructor Destroy;  override;
    property FieldName   :String     read FFieldName;
    property FieldType   :TFieldType read FFieldType;
    property Nullable    :Boolean    read FNullable     write FNullable;
    property DefaultValue:String     read FDefaultValue;
    property Size        :Integer    read FSize;
  end;

  { TTableDefinition }

  TTableDefinition = class(TDatabaseObject)
  private
    FFields:array of TCollumnDefinition;
    FPK:TPrimaryKeyIndex;
    FUniqueIndexes:array of TUniqueIndex;
  public
    constructor Create(OwnerDatabase:TDatabaseDefinition);
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
  end;

   { TDatabaseDefinition }

   TDatabaseDefinition = class(TDatabaseObject)
   protected
     FTables:array of TTableDefinition;

   public
     destructor Destroy; override;
     procedure AddTable(TableName:String);
     procedure DeleteTable(TableName:String);
     function  FindTableDef(TableName:String):Boolean;
     function GetCurrentState: TDatabaseObjectState; override;
     procedure ResetState; override;
   end;

implementation

{ TDatabaseDefinition }

destructor TDatabaseDefinition.Destroy;
begin
  inherited Destroy;
end;

procedure TDatabaseDefinition.AddTable(TableName: String);
begin

end;

procedure TDatabaseDefinition.DeleteTable(TableName: String);
begin

end;

function TDatabaseDefinition.FindTableDef(TableName: String): Boolean;
begin

end;

function TDatabaseDefinition.GetCurrentState: TDatabaseObjectState;
begin
  Result:=inherited GetCurrentState;
end;

procedure TDatabaseDefinition.ResetState;
begin
  inherited ResetState;
end;

{ TCollumnDefinition }

constructor TCollumnDefinition.Create(OnwerTable: TTableDefinition;
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

{ TTableDefinition }

constructor TTableDefinition.Create(OwnerDatabase: TDatabaseDefinition);
begin

end;

destructor TTableDefinition.Destroy;
begin
  inherited Destroy;
end;

procedure TTableDefinition.addCollumn(FieldName: String; FieldType: TFieldType;
  Size: Integer; Nullable: Boolean; DefaultValue: String);
begin

end;

function TTableDefinition.addPrimaryKey(pkName: String): TPrimaryKeyIndex;
begin

end;

function TTableDefinition.addUniqueIndex(uniquename: String): TUniqueIndex;
begin

end;

function TTableDefinition.addForeignKey(IndexName, SourceTable: String;
  UpdateAction: TForeignKeyRestriction; DeleteAction: TForeignKeyRestriction
  ): TForeignKey;
begin

end;

function TTableDefinition.FieldExists(fieldname: String;
  var field: TCollumnDefinition): Boolean;
begin

end;

function TTableDefinition.GetCurrentState: TDatabaseObjectState;
begin
  Result:=inherited GetCurrentState;
end;

procedure TTableDefinition.ResetState;
begin
  inherited ResetState;
end;

constructor TIndex.Create(OwnerTable:TTableDefinition; IndexName:String);
begin
  //TODO: must validate the index name first with the database driver.
  //TODO: must check if the name of the index don't already exists on schema.
  inherited Create;
  FTableOwner:=OwnerTable;
  FIndexName:=IndexName;
end;

destructor TIndex.Destroy;
begin
  SetLength(FFields, 0);
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
  for c:=0 to High(FFields) do
    if FFields[c]=lowercase(FieldName) then begin
      found:=true;
      break;
    end;

  if found then
    raise Exception.Create('O campo já existe no indice!');

  c:=Length(FFields);
  SetLength(FFields,c+1);
  FFields[c]:=lowercase(FieldName);
end;

function TIndex.GetFieldCount:Integer;
begin
  Result:=Length(FFields);
end;

function TIndex.GetField(index:Integer):String;
begin
  if (index<0) or (index>High(FFields)) then
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

constructor TForeignKey.Create(OwnerTable:TTableDefinition; IndexName,
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

end.

