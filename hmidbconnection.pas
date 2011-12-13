{$i language.inc}
{$IFDEF PORTUGUES}
{:
  Unit que implementa a classe de conexão a vários sistemas gerenciadores de
  banco de dados.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  Unit that implements a class to connnect on many database servers.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIDBConnection;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, sysutils, ZConnection, ZPropertyEditor, MessageSpool, CrossEvent,
  syncobjs, memds, ZDataset, db {$IFNDEF FPC}, Windows{$ENDIF};

type

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Database.
  {$ELSE}
  //: Property editor of THMIDBConnection.Database property.
  {$ENDIF}
  THMIDBDatabasePropertyEditor = class(TZDatabasePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Catalog
  {$ELSE}
  //: Property editor of THMIDBConnection.Catalog property.
  {$ENDIF}
  THMIDBCatalogPropertyEditor = class(TZDatabasePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  {$IFDEF PORTUGUES}
  //: Editor da propriedade THMIDBConnection.Protocol
  {$ELSE}
  //: Property editor of THMIDBConnection.Protocol property.
  {$ENDIF}
  THMIDBProtocolPropertyEditor = class(TZProtocolPropertyEditor)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {$IFDEF PORTUGUES}
  //: Método usado pela thread para execução de uma consulta.
  {$ELSE}
  //: Procedure called by thread to execute the query.
  {$ENDIF}
  TExecSQLProc = procedure(sqlcmd:String; outputdataset:TMemDataset) of object;

  {$IFDEF PORTUGUES}
  //: Método usado pela thread para retornar um dataset após a execução da consulta.
  {$ELSE}
  //: Procedure called by thread to return the dataset after the query execution.
  {$ENDIF}
  TReturnDataSetProc = procedure(Sender:TObject; DS:TMemDataset) of object;

  {$IFDEF PORTUGUES}
  //: Inteface para interação com objetos privados do THMIDBConnection
  {$ELSE}
  //: Interface to interact with private objects of THMIDBConnection
  {$ENDIF}
  IHMIDBConnection = interface
    ['{C5AEA572-D7F8-4116-9A4B-3C3B972DC021}']
    {$IFDEF PORTUGUES}
    //: Retorna um TZConnection para os editores de propriedade.
    {$ELSE}
    //: Returns the TZConnection to be used by property editors.
    {$ENDIF}
    function GetSyncConnection:TZConnection;

    {$IFDEF PORTUGUES}
    {:
      Função que executa uma consulta assincrona.
      @param(sql String. Commando SQL. Se for uma consulta de seleção onde se
                         deseja obter os dados retornados, é necessário informar
                         uma procedure válida no parametro ReturnDatasetCallback.)
      @param(ReturnDatasetCallback TReturnDataSetProc. Ponteiro para o procedimento
                         que vai ser chamado quando para retornar os dados a aplicação.)
    }
    {$ELSE}
    {:
      Executes a assynchronous query.
      @param(sql String. SQL command. If the query is a SELECT and you want get
                         the returned data, you must supply a callback procedure
                         in ReturnDatasetCallback param.)
      @param(ReturnDatasetCallback TReturnDataSetProc. Callback that will be
                         called to return the data to the application.)
    }
    {$ENDIF}
    function ExecSQL(sql:String; ReturnDatasetCallback:TReturnDataSetProc):Integer;
  end;

  {$IFDEF PORTUGUES}
  {:
  Estrutura de comando SQL que é enviado a thread.
  @seealso(TProcessSQLCommandThread.ExecSQLWithoutResultSet)
  @seealso(TProcessSQLCommandThread.ExecSQLWithResultSet)
  }
  {$ELSE}
  {:
  SQL command message. It's queued on thread.
  @seealso(TProcessSQLCommandThread.ExecSQLWithoutResultSet)
  @seealso(TProcessSQLCommandThread.ExecSQLWithResultSet)
  }
  {$ENDIF}
  TSQLCmdRec = record
    SQLCmd:String;
    ReturnDataSetCallback:TReturnDataSetProc;
  end;

  {$IFDEF PORTUGUES}
  {:
  Pointeiro de estrutura de mensagem de comando SQL.
  @seealso(TProcessSQLCommandThread.ExecSQLWithoutResultSet)
  @seealso(TProcessSQLCommandThread.ExecSQLWithResultSet)
  }
  {$ELSE}
  {:
  Pointer of a SQL command message.
  @seealso(TProcessSQLCommandThread.ExecSQLWithoutResultSet)
  @seealso(TProcessSQLCommandThread.ExecSQLWithResultSet)
  }
  {$ENDIF}
  PSQLCmdRec = ^TSQLCmdRec;

  {$IFDEF PORTUGUES}
  {:
  Fila de execução assincrona de comandos SQL.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Asynchronous SQL command execution queue.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TProcessSQLCommandThread=class(TCrossThread)
  private
    FSpool:TMessageSpool;
    FEnd:TCrossEvent;
    cmd:PSQLCmdRec;
    fds:TMemDataset;
    fOnExecSQL:TExecSQLProc;
  protected
    //: @exclude
    procedure Execute; override;
    //: @exclude
    procedure ReturnData;
  public
    {$IFDEF PORTUGUES}
    {:
    Cria a fila de processamento assincrono.

    @param(CreateSuspended  Boolean. Se verdadeiro, a fila é criada suspensa,
                                     sendo necessário chamar o procedimento
                                     Resume posteriormente para que ela comece a
                                     trabalhar.)
    @param(ExecSQLProc TExecSQLProc. Ponteiro para o procedimento que será chamado
                                     para executar as consultas SQL.)
    }
    {$ELSE}
    {:
    Creates the assynchronous queue.

    @param(CreateSuspended  Boolean. If @true, the spool is created suspended,
                                     and you must call the procedure Resume  to
                                     start the queue.)
    @param(ExecSQLProc TExecSQLProc. Points to a procedure that will be called
                                     to executes the SQL queries.)
    }
    {$ENDIF}
    constructor Create(CreateSuspended: Boolean; ExecSQLProc:TExecSQLProc);
    //: @exclude
    destructor Destroy; override;
    {$IFDEF PORTUGUES}
    {:
    Espera determinado tempo pela finalização da thread.
    @param(Timeout Cardinal. Tempo máximo de espera.)
    @returns(Retorna wrSignaled caso a thread seja finalizada antes do tempo
             máximo de espera (Timeout). Caso a fila não termine antes do tempo
             máximo retorna wrTimeout. Caso o procedimento Destroy for chamado
             antes do método Terminate seguido @name, pode retornar wrAbandoned
             ou wrError.)
    }
    {$ELSE}
    {:
    Waits the end of the thread.
    @param(Timeout Cardinal. Maximum timeout to wait the ends.)
    @returns(Returns wrSignaled if the thread was ended before the timeout
             elapses. If the thread don't terminate before the timeout, return
             wrTimeout. If the Destroy was called before of the Terminate
             procedure, it can return wrAbandoned or wrError.)
    }
    {$ENDIF}
    function WaitEnd(Timeout:Cardinal):TWaitResult;
  public
    {$IFDEF PORTUGUES}
    {:
    Executa uma consulta SQL sem retornar um DataSet para a aplicação.
    @param(sql String. Comando SQL a executar.)
    }
    {$ELSE}
    {:
    Executes a SQL query without return a Dataset.
    @param(sql String. SQL query command.)
    }
    {$ENDIF}
    procedure ExecSQLWithoutResultSet(sql:String);

    {$IFDEF PORTUGUES}
    {:
    Executa uma consulta SQL e rotorna um DataSet para a aplicação.
    @param(sql String. Comando SQL a executar.)
    @param(ReturnDataCallback TReturnDataSetProc. Procedimento que é chamado para
                                                  retornar o dataset resultante da
                                                  consulta para a aplicação.)
    }
    {$ELSE}
    {:
    Executes a SQL query, returning a Dataset.
    @param(sql String. SQL query command.)
    @param(ReturnDataCallback TReturnDataSetProc. Procedure that will be called
                                                  to return the dataset.)
    }
    {$ENDIF}
    procedure ExecSQLWithResultSet(sql:String; ReturnDataCallback:TReturnDataSetProc);
  end;

  //: @exclude
  TBasicTableChecker = class;

  {$IFDEF PORTUGUES}
  {:
  Componente de banco de dados do PascalSCADA.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @bold(Usa o projeto ZeosLib.)
  }
  {$ELSE}
  {:
  Database component of PascalSCADA.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @bold(Uses the ZeosLib project.)
  }
  {$ENDIF}
  THMIDBConnection = class(TComponent, IHMIDBConnection)
  private
    FConnectRead:Boolean;
    FSyncConnection,
    FASyncConnection:TZConnection;
    FASyncQuery:TZQuery;
    FCS:TCriticalSection;
    FSQLSpooler:TProcessSQLCommandThread;
    FCmdID:Integer;
    function  GetSyncConnection:TZConnection;
    procedure ExecuteSQLCommand(sqlcmd:String; outputdataset:TMemDataset);
  protected
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUser: string;
    FPassword: string;
    FCatalog: string;
    procedure Loaded; override;
  protected
    function GetConnected:Boolean;

    procedure SetConnected(x:Boolean);
    procedure SetProtocol(x: String);
    procedure SetHostName(x: String);
    procedure SetPort(x: Integer);
    procedure SetDatabase(x: String);
    procedure SetUser(x: String);
    procedure SetPassword(x: String);
    procedure SetCatalog(x: String);

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    {$IFDEF PORTUGUES}
    //: Executa uma consulta assincrona.
    {$ELSE}
    //: Executes a assychronous query.
    {$ENDIF}
    function  ExecSQL(sql:String; ReturnDatasetCallback:TReturnDataSetProc):Integer;

    function CreateTableChecker:TBasicTableChecker;
  published
    {$IFDEF PORTUGUES}
    //: Caso @true, conecta ou está conectado ao banco de dados.
    {$ELSE}
    //: If true, connects or are connected on database.
    {$ENDIF}
    property Connected:Boolean read GetConnected write SetConnected;

    {$IFDEF PORTUGUES}
    //: Driver de banco de dados em uso para conexão ao banco de dados.
    {$ELSE}
    //: Database protocol driver used to connect on database.
    {$ENDIF}
    property Protocol: string  read FProtocol    write SetProtocol;

    {$IFDEF PORTUGUES}
    //: Endereço ou nome da máquina onde está o banco de dados.
    {$ELSE}
    //: Address or machine name where is the database.
    {$ENDIF}
    property HostName: string  read FHostName    write SetHostName;

    {$IFDEF PORTUGUES}
    //: Número da porta usada para conectar no banco de dados (conexões TCP/UDP)
    {$ELSE}
    //: Port number to connect on database (TCP/UDP connections)
    {$ENDIF}
    property Port:     Integer read FPort        write SetPort default 0;

    {$IFDEF PORTUGUES}
    //: Banco de dados a conectar.
    {$ELSE}
    //: Database name.
    {$ENDIF}
    property Database: string  read FDatabase    write SetDatabase;

    {$IFDEF PORTUGUES}
    //: Usuário a conectar no banco de dados.
    {$ELSE}
    //: Username to connect on database.
    {$ENDIF}
    property User:     string  read FUser        write SetUser;

    {$IFDEF PORTUGUES}
    //: Senha do usuário para conectar ao banco de dados.
    {$ELSE}
    //: Password of the user to connect on database.
    {$ENDIF}
    property Password: string  read FPassword    write SetPassword;

    {$IFDEF PORTUGUES}
    //: Verifique a documentação do TZConnection.Catalog do ZeosLib para maiores informações.
    {$ELSE}
    //: See the documentation of TZConnection.Catalog of ZeosLib for more information.
    {$ENDIF}
    property Catalog:  string  read FCatalog     write SetCatalog;
  end;

  TFKAction = ( fkNoAction, fkRestrict, fkSetNULL, fkCascade );

  TFKInfo = record
    FKTable,
    FKField:String;
    OnUpdateAction,
    OnDeleteAction:TFKAction;
  end;

  {$IFDEF PORTUGUES}
  //: Estrutura que armazena a declaração de um campo de uma tabela.
  {$ELSE}
  //: Structure that stores a table field.
  {$ENDIF}
  TFieldDefinition = record
    FieldName:String;
    FieldType:TFieldType;
    FieldSize:Integer;
    FieldFK:TFKInfo;
    FieldDefaultValue:String;
    PrimaryKey, Unique, NotNull:Boolean;
  end;

  {$IFDEF PORTUGUES}
  //: Array que contem os campos esperados em uma tabela.
  {$ELSE}
  //: Array that stores the expected fields of a table.
  {$ENDIF}
  TFieldsDefinition = array of TFieldDefinition;

  TDBNameBehavior = (nbTableName, nbFieldName, nbConstraintName);


  {$IFDEF PORTUGUES}
  //: Estados possiveis de uma tabela.
  {$ELSE}
  //: Array that stores the expected fields of a table.
  {$ENDIF}
  TTableState = (tsOk,tsDontExists, tsChanged, tsUnknown);

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
    procedure SetTableName(fname:String);
  public
    constructor Create(AOwner: TComponent); override;
    //constructor Create(AOwner: TComponent; DBConnection:THMIDBConnection); overload;
    //constructor Create(AOwner: TComponent; TableName:String); overload;
    //constructor Create(AOwner: TComponent; DBConnection:THMIDBConnection; TableName:String); overload;

    destructor Destroy; override;
    function  ValidName(fname:String; namebehavior:TDBNameBehavior):Boolean; virtual;
    procedure AddFieldDefinition(fieldname:String; fieldType:TFieldType; fieldsize:Integer = -1; pk:Boolean = false; unique:Boolean = false; notnull:Boolean = false); virtual;
    procedure DelFieldDefinition(fieldname:String; fieldType:TFieldType; fieldsize:Integer = -1; pk:Boolean = false; unique:Boolean = false; notnull:Boolean = false); virtual;
    function  CheckTable:TTableState; virtual;
    function  DropTableCmd:String; virtual;
    procedure ExecuteDropTable; virtual;
    function  CreateTableCmd:String; virtual;
    procedure ExecuteCreateTable; virtual;
  published
    property TableName:String read FTableName write SetTableName;
    property DBConnection:THMIDBConnection read FDBConnection write FDBConnection;
  end;

  TBasicTableCheckerClass = class of TBasicTableChecker;

  TPostgresTableChecker = class(TBasicTableChecker)
  public
    function  ValidName(fname:String; namebehavior:TDBNameBehavior):Boolean; virtual;

    function DropTableCmd: String; override;
    function CreateTableCmd: String; override;
  end;

  TBasicDatabaseChecker = class(TComponent)
  private
    FTableClass:TBasicTableCheckerClass;
    FTables:array of TBasicTableChecker;
  public
    constructor Create(AOwner: TComponent; tableclass:TBasicTableCheckerClass);
    destructor  Destroy; override;
    function    AddTable(tablename:String):TBasicTableChecker; virtual;
    procedure   RemoveTable(tablename:String);

    function    TableExistsOnMetadata(tablename:String):Boolean; virtual;
    function    GetTableMetadataByName(Tablename:String):TBasicTableChecker; virtual;
  end;

const
  SQLCommandMSG = 1024;

implementation

uses hsstrings;

//##############################################################################
//EDITORES DE PROPRIEDADES DA CLASSE THMIDBCONNECTION
//PROPERTY EDITORS OF THE CLASS THMIDBCONNECTION
//##############################################################################

function THMIDBDatabasePropertyEditor.GetZComponent:TPersistent;
begin
  Result:=GetComponent(0);
  if (Result is THMIDBConnection) and Supports(Result, IHMIDBConnection) then
    Result:=(THMIDBConnection(Result) as IHMIDBConnection).GetSyncConnection;
end;

function THMIDBCatalogPropertyEditor.GetZComponent:TPersistent;
begin
  Result:=GetComponent(0);
  if (Result is THMIDBConnection) and Supports(Result, IHMIDBConnection) then
    Result:=(THMIDBConnection(Result) as IHMIDBConnection).GetSyncConnection;
end;

var
  {$IFDEF WINCE}
  SupportedDBDrivers:array[0..0] of string = ('sqlite');
  {$ELSE}
  SupportedDBDrivers:array[0..3] of string = ('postgresql','sqlite','mysql','firebird');
  {$ENDIF}

//only accepted drivers are show.
procedure THMIDBProtocolPropertyEditor.GetValueList(List: TStrings);
var
  i, s:Integer;
  found:Boolean;
begin
  inherited GetValueList(List);
  for i:=List.Count-1 downto 0 do begin
    found:=false;
    for s:=0 to High(SupportedDBDrivers) do
      if pos(SupportedDBDrivers[s], List.Strings[i])<>0 then begin
        found:=true;
        break;
      end;
    if not found then
      List.Delete(i);
  end;
end;

//##############################################################################
//THREAD DE EXECUÇÃO DOS COMANDOS SQL THMIDBCONNECTION
//SQL COMMANDS QUEUE THREAD CLASS.
//##############################################################################

constructor TProcessSQLCommandThread.Create(CreateSuspended: Boolean; ExecSQLProc:TExecSQLProc);
begin
  inherited Create(CreateSuspended);
  FSpool:=TMessageSpool.Create;
  FEnd:=TCrossEvent.Create(nil,true,false,'');
  fOnExecSQL:=ExecSQLProc;
end;

destructor  TProcessSQLCommandThread.Destroy;
begin
  inherited Destroy;
  FEnd.Destroy;
end;

procedure   TProcessSQLCommandThread.Execute;
var
  msg:TMSMsg;
begin
  FEnd.ResetEvent;
  while not Terminated do begin
    while FSpool.PeekMessage(msg,0,0,true) do begin
      //executa o comando sql
      //executes the sql commmand.
      if (msg.MsgID=SQLCommandMSG) and (msg.wParam<>nil) then begin
        cmd:=PSQLCmdRec(msg.wParam);
        try
          //se é necessario retornar algo
          //cria o dataset de retorno de dados.
          //
          //if are to return the data,
          //creates the dataset.
          if Assigned(cmd^.ReturnDataSetCallback) then
            fds:=TMemDataset.Create(Nil)
          else
            fds:=nil;

          if Assigned(fOnExecSQL) then
            fOnExecSQL(cmd^.SQLCmd, fds);

          if Assigned(cmd^.ReturnDataSetCallback) then
            Synchronize(ReturnData);
        finally
          Dispose(cmd);
        end;
      end;
    end;
    Sleep(1);
  end;
  FEnd.SetEvent;
end;

procedure   TProcessSQLCommandThread.ReturnData;
begin
  cmd^.ReturnDataSetCallback(self,fds);
end;

function    TProcessSQLCommandThread.WaitEnd(Timeout:Cardinal):TWaitResult;
begin
  Result := FEnd.WaitFor(Timeout);
end;

procedure   TProcessSQLCommandThread.ExecSQLWithoutResultSet(sql:String);
begin
  ExecSQLWithResultSet(sql,nil);
end;

procedure   TProcessSQLCommandThread.ExecSQLWithResultSet(sql:String; ReturnDataCallback:TReturnDataSetProc);
var
  sqlcmd:PSQLCmdRec;
begin
  new(sqlcmd);
  sqlcmd^.SQLCmd:=sql;
  sqlcmd^.ReturnDataSetCallback:=ReturnDataCallback;
  FSpool.PostMessage(SQLCommandMSG,sqlcmd,nil,true);
end;

//##############################################################################
//CLASSE THMIDBCONNECTION
//
//THMIDBCONNECTION CLASS
//##############################################################################

constructor THMIDBConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCS:=TCriticalSection.Create;
  FSyncConnection:=TZConnection.Create(nil);
  FASyncConnection:=TZConnection.Create(nil);
  FASyncQuery:=TZQuery.Create(nil);
  FASyncQuery.Connection:=FASyncConnection;
  FCmdID:=0;

  FSQLSpooler:=TProcessSQLCommandThread.Create(true,ExecuteSQLCommand);
  FSQLSpooler.WakeUp;
end;

destructor  THMIDBConnection.Destroy;
begin
  inherited Destroy;
  //destroi a thread
  //
  //Destroys the thread.
  FSQLSpooler.Terminate;
  while FSQLSpooler.WaitEnd(1)<>wrSignaled do
    CheckSynchronize(1);
  FSQLSpooler.Destroy;

  FASyncQuery.Destroy;
  FSyncConnection.Destroy;
  FASyncConnection.Destroy;
  FCS.Destroy;
end;

procedure THMIDBConnection.Loaded;
begin
  Inherited loaded;
  Connected:=FConnectRead;
end;

function THMIDBConnection.GetSyncConnection:TZConnection;
begin
  Result:=FSyncConnection;
end;

function THMIDBConnection.ExecSQL(sql:String; ReturnDatasetCallback:TReturnDataSetProc):Integer;
begin
  FSQLSpooler.ExecSQLWithResultSet(sql,ReturnDatasetCallback);
  Result:=InterLockedIncrement(FCmdID);
  if Result=$3FFFFFFF then
    InterLockedExchange(FCmdID,0);
end;

function THMIDBConnection.CreateTableChecker:TBasicTableChecker;
begin
  if Connected then begin
    if (Protocol='mysql') or (Protocol='mysql-4.1') or (Protocol='mysql-5') or
       (Protocol='mysqld-4.1') or (Protocol='mysqld-5') then begin
      Result:=nil; //must return a MySQL table Checker.
    end;

    if (Protocol='postgresql') or (Protocol='postgresql-7') or (Protocol='postgresql-8') then begin
      Result:=TPostgresTableChecker.Create(Self);
    end;

    if (Protocol='sqlite') or (Protocol='sqlite-3') then begin
      Result:=nil; //must return a SQLite table Checker.
    end;

    if Result<>nil then
      Result.DBConnection:=Self;

  end else
    raise Exception.Create('Database must be connected!');
end;

procedure THMIDBConnection.ExecuteSQLCommand(sqlcmd:String; outputdataset:TMemDataset);
begin
  FCS.Enter;
  try
    FASyncQuery.SQL.Clear;
    FASyncQuery.SQL.Add(sqlcmd);
    if outputdataset<>nil then begin
      FASyncQuery.Open;
      outputdataset.CopyFromDataset(FASyncQuery);
    end else
      FASyncQuery.ExecSQL;
    FASyncQuery.Close;
  finally
    FCS.Leave;
  end;
end;

function  THMIDBConnection.GetConnected:Boolean;
begin
  Result:=FSyncConnection.Connected;
end;

procedure THMIDBConnection.SetProtocol(x: String);
begin
  FSyncConnection.Protocol:=x;
  FCS.Enter;
  try
    FASyncConnection.Protocol:=x;
  finally
    FCS.Leave;
  end;
  FProtocol:=FSyncConnection.Protocol;
end;

procedure THMIDBConnection.SetHostName(x: String);
begin
  FSyncConnection.HostName:=x;
  FCS.Enter;
  try
    FASyncConnection.HostName:=x;
  finally
    FCS.Leave;
  end;
  FHostName:=FSyncConnection.HostName;
end;

procedure THMIDBConnection.SetPort(x: Integer);
begin
  FSyncConnection.Port:=x;
  FCS.Enter;
  try
    FASyncConnection.Port:=x;
  finally
    FCS.Leave;
  end;
  FPort:=FSyncConnection.Port;
end;

procedure THMIDBConnection.SetDatabase(x: String);
begin
  FSyncConnection.Database:=x;
  FCS.Enter;
  try
    FASyncConnection.Database:=x;
  finally
    FCS.Leave;
  end;
  FDatabase:=FSyncConnection.Database;
end;

procedure THMIDBConnection.SetUser(x: String);
begin
  FSyncConnection.User:=x;
  FCS.Enter;
  try
    FASyncConnection.User:=x;
  finally
    FCS.Leave;
  end;
  FUser:=FSyncConnection.User;
end;

procedure THMIDBConnection.SetPassword(x: String);
begin
  FSyncConnection.Password:=x;
  FCS.Enter;
  try
    FASyncConnection.Password:=x;
  finally
    FCS.Leave;
  end;
  FPassword:=FSyncConnection.Password;
end;

procedure THMIDBConnection.SetCatalog(x: String);
begin
  FSyncConnection.Catalog:=x;
  FCS.Enter;
  try
    FASyncConnection.Catalog:=x;
  finally
    FCS.Leave;
  end;
  FCatalog:=FSyncConnection.Catalog;
end;

procedure THMIDBConnection.SetConnected(x:Boolean);
begin
  if [csReading, csLoading]*ComponentState<>[] then begin
    FConnectRead:=x;
    exit;
  end;
  FSyncConnection.Connected:=x;
  if FSyncConnection.Connected=x then begin
    FCS.Enter;
    try
      FASyncConnection.Connected:=x;
    finally
      FCS.Leave;
    end;
  end;
end;

//##############################################################################
//CLASSE TBasicTableChecker
//
//TBasicTableChecker CLASS
//##############################################################################

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

function TBasicTableChecker.ValidName(fname:String; namebehavior:TDBNameBehavior):Boolean;
var
  c:Integer;
begin
  Result:=true;
  for c:=1 to Length(fname) do
    if ((c=1) AND ((not (fname[c] in ['a'..'z'])) and (not (fname[c] in ['A'..'Z'])))) or
       ((c>1) AND ((not (fname[c] in ['a'..'z'])) and (not (fname[c] in ['A'..'Z']))  and (not (fname[c] in ['0'..'9'])) and (fname[c]<>'_'))) then begin
      Result:=false;
      exit;
    end;
end;

procedure TBasicTableChecker.AddFieldDefinition(fieldname:String; fieldType:TFieldType; fieldsize:Integer = -1; pk:Boolean = false; unique:Boolean = false; notnull:Boolean = false);
var
  h:Integer;
begin
  if not ValidName(fieldname, nbFieldName) then
    raise exception.Create(SInvalidDatabaseName);

  for h:=0 to High(FFields) do
    if FFields[h].FieldName=fieldname then
      exit;

  h:=Length(FFields);
  SetLength(FFields,h+1);
  FFields[h].FieldName :=fieldname;
  FFields[h].FieldType :=fieldType;
  FFields[h].FieldSize :=fieldsize;
  FFields[h].PrimaryKey:=pk;
  FFields[h].Unique    :=unique;
  FFields[h].NotNull   :=notnull;
end;

procedure TBasicTableChecker.DelFieldDefinition(fieldname:String; fieldType:TFieldType; fieldsize:Integer = -1; pk:Boolean = false; unique:Boolean = false; notnull:Boolean = false);
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
  FFields[c].FieldName :=FFields[h].FieldName;
  FFields[c].FieldType :=FFields[h].FieldType;
  FFields[c].FieldSize :=FFields[h].FieldSize;
  FFields[c].PrimaryKey:=FFields[h].PrimaryKey;
  FFields[c].Unique    :=FFields[h].Unique;
  FFields[c].NotNull   :=FFields[h].NotNull;
  SetLength(FFields, h);
end;

function  TBasicTableChecker.CheckTable:TTableState;
var
  ds:TMemDataset;
  f, fd:Integer;
  found:boolean;
begin
  if trim(FTableName)='' then begin
    Result:=tsDontExists;
    exit;
  end;

  ds:=ExecuteSQL('SELECT * FROM '+FTableName+' LIMIT 1;');
  if ds=nil then begin
    Result:=tsUnknown;
    exit;
  end;

  try
    for f:=0 To High(FFields) do begin
      found:=false;
      for fd:=0 to ds.FieldDefs.Count-1 do begin
        if (LowerCase(ds.FieldDefs.Items[fd].Name)=LowerCase(FFields[f].FieldName)) and
           (ds.FieldDefs.Items[fd].DataType=FFields[f].FieldType) and
           (ds.FieldDefs.Items[fd].Required=FFields[f].NotNull) and
           ((FFields[f].FieldType<>ftString) or
            ((FFields[f].FieldType=ftString) and
             (ds.FieldDefs.Items[fd].Size=FFields[f].FieldSize))) then
        begin
          found:=true;
          break;
        end;
      end;
      if not found then begin
        Result:=tsChanged;
        exit;
      end;
    end;
    Result:=tsOk;
  finally
    ds.Destroy;
  end;
end;

function  TBasicTableChecker.DropTableCmd:String;
begin
  Result:='DROP TABLE '+FTableName+';';
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

procedure TBasicTableChecker.SetTableName(fname:String);
begin
  if ValidName(fname,nbTableName) then
    FTableName:=fname
  else
    raise Exception.Create(SInvalidDatabaseName);
end;

//##############################################################################
//CLASSE TPostgreTableChecker
//
//TPostgreTableChecker CLASS
//##############################################################################

function TPostgresTableChecker.ValidName(fname: String; namebehavior:TDBNameBehavior):Boolean;
var
  c:Integer;
begin
  Result:=false;
  if namebehavior=nbTableName then
    for c:=1 to Length(fname) do
      if ((c=1) AND ((not (fname[c] in ['a'..'z'])) and (not (fname[c] in ['A'..'Z'])))) or
         ((c>1) AND ((not (fname[c] in ['a'..'z'])) and (not (fname[c] in ['A'..'Z'])) and (not (fname[c] in ['0'..'9'])) and (fname[c]<>'_') and (fname[c]<>'.'))) then begin
        Result:=false;
        exit;
      end
  else
    inherited ValidName(fname, namebehavior);
end;

function TPostgresTableChecker.DropTableCmd:String;
begin
  Result:='ALTER TABLE '+FTableName+' RENAME TO '+FTableName+'_backup_'+FormatDateTime('yyyymmddhhnnsszzz',Now);
end;

function  TPostgresTableChecker.CreateTableCmd: String;
var
  c:Integer;
begin
  Result := 'CREATE TABLE (';
  for c:=0 to High(FFields) do begin
    case FFields[c].FieldType of
      ftAutoInc:
        Result:=Result;
    end;
  end;
end;

constructor TBasicDatabaseChecker.Create(AOwner: TComponent; tableclass:TBasicTableCheckerClass);
begin
  inherited Create(AOwner);
  FTableClass:=tableclass;
end;

destructor  TBasicDatabaseChecker.Destroy;
begin
  inherited Destroy;
end;

function    TBasicDatabaseChecker.AddTable(tablename:String):TBasicTableChecker;
var
  t:Integer;
begin
  for t:=0 to High(FTables) do
    if FTables[t].TableName=lowercase(tablename) then
      raise Exception.Create(STableAlreadyexistsOnMetadata);

  Result:=FTableClass.Create(Self);

  if not Result.ValidName(tablename) then begin
    Result.Destroy;
    Result:=nil;
  end
    Result.TableName:=lowercase(tablename);
end;

procedure   TBasicDatabaseChecker.RemoveTable(tablename:String);
begin

end;

function    TBasicDatabaseChecker.TableExistsOnMetadata(tablename:String):Boolean;
var
  t:Integer;
begin
  Result:=false;
  for t:=0 to High(FTables) do
    if FTables[t].TableName=lowercase(tablename) then begin
      Result:=true
      break;
    end;
end;

function    TBasicDatabaseChecker.GetTableMetadataByName(Tablename:String):TBasicTableChecker;
var
  t:Integer;
begin
  Result:=nil;
  for t:=0 to High(FTables) do
    if FTables[t].TableName=lowercase(tablename) then begin
      Result:=FTables[t];
      break;
    end;
end;

end.
