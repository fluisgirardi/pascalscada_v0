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

  //simple index declaration (primary and unique)
  TIndex = class(TObject)
  protected
    FTableOwner:TTableDefinition;
    FIndexName:string;
    FFields:TArrayOfString;
    FState:TDatabaseObjectState;
    procedure AddFieldToIndex(FieldName:String); virtual;
  public
    constructor Create(OwnerTable:TTableDefinition; IndexName:String);
    destructor Destroy; override;
    function GetCurrentState:TDatabaseObjectState; virtual;
    procedure ResetState;
    property IndexName:String read FIndexName;
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

  TTableDefinition = class(TObject)
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
    function GetCurrentState:TDatabaseObjectState; virtual;
    procedure ResetState;
  end;

   TDatabaseDefinition = class(TObject);

implementation

constructor TIndex.Create(OwnerTable:TTableDefinition; IndexName:String);
begin
  //must validate the index name first.
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

function   TIndex.GetCurrentState:TDatabaseObjectState;
begin

end;

procedure  TIndex.ResetState;
begin
  FState:=dosUnknown;
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



end.

