unit dbstructurechecker;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  db;

type

  TTableDefinition = class; //forward declaration.
  TDatabaseDefinition = class; //forward declaration.

  TArrayOfString = array of string;
  TDatabaseObjectState = (dosUnknown, dosChanged, dosDontExists, dosOK);

  //simple index declaration (primary and unique)
  TIndex = class(TObject)
  protected
    FTableOwner:TTableDefinition;
    FIndexName:string;
    FFields:TArrayOfString;
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

  TFKInfo = record
    SourceTable:String;
    FieldLinks:TFieldLinks;
  end;

  TTableLinks = array of TFKInfo;

  TForeignKey = class(TIndex)
  protected
    FTableLinks:TTableLinks;
  public
    function addForeignKey(SourceTable,SourceField, Field:String);
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
    property FieldName   :String     read FFieldName
    property FieldType   :TFieldType read FFieldType
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
  end;

implementation

constructor TIndex.Create(OwnerTable:TTableDefinition);
begin
  inherited Create;
  FTableOwner:=OwnerTable;
  FIndexName:=IndexName;
end;

destructor  TIndex.Destroy;
begin
  inherited Destroy;
end;

procedure   TIndex.AddFieldToIndex(FieldName:String);
begin

end;

function    TIndex.GetCurrentState:TDatabaseObjectState;
begin

end;

end.

