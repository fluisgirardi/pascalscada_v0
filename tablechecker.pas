unit TableChecker;

{$mode delphi}

interface

uses
  Classes, db, HMIDBConnection, memds, syncobjs;

type
  TFieldDefinition = record
    FieldName:String;
    FieldType:TFieldType;
    FieldSize:Integer;
  end;

  TFieldsDefinition = array of TFieldDefinition;

  //checks the table structure
  TBasicTableChecker = class(TComponent)
  private
    FCS:TCriticalSection;
    FSignaled:Boolean;
    FMemDS:TMemDataset;
    procedure ReturnedQuery(Sender:TObject; DS:TMemDataset);
    function  InternalExecuteSQL(SQL:String):TMemDataset;
  protected
    FDBConnection:THMIDBConnection;
    FTableName:String;
    FFields:TFieldsDefinition;
    function  ExecuteSQL(SQL:String):TMemDataset;
  public
    constructor Create(AOwner: TComponent); override;
    //constructor Create(AOwner: TComponent; DBConnection:THMIDBConnection); overload;
    //constructor Create(AOwner: TComponent; TableName:String); overload;
    //constructor Create(AOwner: TComponent; DBConnection:THMIDBConnection; TableName:String); overload;

    destructor Destroy; override;
    procedure AddFieldDefinition(fieldname:String; fieldType:TFieldType; fieldsize:Integer = -1); virtual;
    procedure DelFieldDefinition(fieldname:String; fieldType:TFieldType; fieldsize:Integer = -1); virtual;
    function  CheckTable:Boolean; virtual;
    function  DropTableCmd:String; virtual;
    procedure ExecuteDropTable; virtual;
    function  CreateTableCmd:String; virtual;
    procedure ExecuteCreateTable; virtual;
  published
    property TableName:String read FTableName write FTableName;
    property DBConnection:THMIDBConnection read FDBConnection write FDBConnection;
  end;

implementation

constructor TBasicTableChecker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCS:=TCriticalSection.Create;
end;

destructor TBasicTableChecker.Destroy;
begin
  FCS.Destroy;
  inherited Destroy;
end;

procedure TBasicTableChecker.AddFieldDefinition(fieldname:String; fieldType:TFieldType; fieldsize:Integer = -1);
var
  h:Integer;
begin
  for h:=0 to High(FFields) do
    if FFields[h].FieldName=fieldname then
      exit;

  h:=Length(FFields);
  SetLength(FFields,h+1);
  FFields[h].FieldName:=fieldname;
  FFields[h].FieldType:=fieldType;
  FFields[h].FieldSize:=fieldsize;
end;

procedure TBasicTableChecker.DelFieldDefinition(fieldname:String; fieldType:TFieldType; fieldsize:Integer = -1);
var
  c,h:Integer;
  found:Boolean;
begin
  found:=false;
  for c:=0 to High(FFields) do
    if FFields[c].FieldName=fieldname then begin
      found:=true;
      break;
    end;

  if not found then exit;

  h:=High(FFields);
  FFields[c].FieldName:=FFields[h].FieldName;
  FFields[c].FieldType:=FFields[h].FieldType;
  FFields[c].FieldSize:=FFields[h].FieldSize;
  SetLength(FFields, h);
end;

function  TBasicTableChecker.CheckTable:Boolean;
begin
  Result:=false;
end;

function  TBasicTableChecker.DropTableCmd:String;
begin
  Result:='';
end;

procedure TBasicTableChecker.ExecuteDropTable;
var
  x:TMemDataset;
begin
  x:=ExecuteSQL(DropTableCmd);
  if x<>nil then
    x.Destroy;
end;

function  TBasicTableChecker.CreateTableCmd:String;
begin
  Result:='';
end;

procedure TBasicTableChecker.ExecuteCreateTable;
var
  x:TMemDataset;
begin
  x:=ExecuteSQL(CreateTableCmd);
  if x<>nil then
    x.Destroy;
end;

procedure TBasicTableChecker.ReturnedQuery(Sender:TObject; DS:TMemDataset);
begin
  FMemDS:=DS;
  FSignaled:=true;
end;

function  TBasicTableChecker.InternalExecuteSQL(SQL:String):TMemDataset;
begin
  FCS.Enter;
  try
    FSignaled:=false;
    FMemDS:=nil;
    Result:=nil;
    if FDBConnection<>nil then begin
      FDBConnection.ExecSQL(sql,ReturnedQuery);
      while not FSignaled do
        CheckSynchronize(1);

      Result:=FMemDS;
    end;
  finally
    FCS.Leave;
  end;
end;

function  TBasicTableChecker.ExecuteSQL(SQL:String):TMemDataset;
begin
  Result:=InternalExecuteSQL(SQL);
end;


end.
