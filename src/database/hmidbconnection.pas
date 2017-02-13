{$i ../common/language.inc}
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

interface

uses
  Classes, sysutils, ZConnection, ZPropertyEditor, MessageSpool, CrossEvent,
  syncobjs, ZDataset, psbufdataset, fgl;

type

  THMIDBConnectionStatementList = specialize TFPGList<UTF8String>;

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
  TExecSQLProc = procedure(sqlcmd:Utf8String; outputdataset:TFPSBufDataSet; out Error:Boolean) of object;

  TStartTransaction = procedure of object;

  TCommitTransaction = procedure of object;

  TRollbackTransaction = procedure of object;

  {$IFDEF PORTUGUES}
  //: Método usado pela thread para retornar um dataset após a execução da consulta.
  {$ELSE}
  //: Procedure called by thread to return the dataset after the query execution.
  {$ENDIF}
  TReturnDataSetProc = procedure(Sender:TObject; DS:TFPSBufDataSet; error:Exception) of object;

  TReturnTransactionStatementsProc = procedure(Sender:TObject; aStatements:THMIDBConnectionStatementList; Sucess:Boolean; LineOfError:Integer; Error:Exception) of object;

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
    procedure ExecSQL(sql:String; ReturnDatasetCallback:TReturnDataSetProc; ReturnSync:Boolean=true);
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
    ReturnSync:Boolean;
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

  TStatementCmdRec = record
    statements:THMIDBConnectionStatementList;
    ReturnTransactionResult:TReturnTransactionStatementsProc;
    FreeStatemensAfterExecute,
    ReturnSync:Boolean;
  end;
  PStatementCmdRec = ^TStatementCmdRec;

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
    FQueue:TMessageSpool;
    FEnd:TCrossEvent;
    cmd:PSQLCmdRec;
    statements:PStatementCmdRec;
    fds:TFPSBufDataSet;
    ferror:Exception;
    fLineError:Integer;
    FErrorOnSync:Boolean;
    fOnExecSQL:TExecSQLProc;
    fStartTransaction:TStartTransaction;
    fCommitTransaction:TCommitTransaction;
    fRollbackTransaction:TRollbackTransaction;
  protected
    //: @exclude
    procedure Execute; override;
    //: @exclude
    procedure ReturnData;
    procedure ReturnStatementsResults;
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
    constructor Create(CreateSuspended: Boolean; ExecSQLProc:TExecSQLProc;
                       StartTransactionProc:TStartTransaction;
                       CommitTransactionProc:TCommitTransaction;
                       RollbackTransactionProc:TRollbackTransaction);
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
    procedure ExecSQLWithoutResultSet(sql:String; ReturnSync:Boolean=true);

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
    procedure ExecSQLWithResultSet(sql:String; ReturnDataCallback:TReturnDataSetProc; ReturnSync:Boolean=true);

    procedure ExecTransaction(aStatements:THMIDBConnectionStatementList; ReturnTransactionResult:TReturnTransactionStatementsProc; FreeStatemensAfterExecute:Boolean; ReturnSync:Boolean=true);
  end;

  {$IFDEF PORTUGUES}
  {:
  Componente de banco de dados do PascalSCADA.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Database component of PascalSCADA.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @bold(Uses the ZeosLib project.)
  }
  {$ENDIF}

  { THMIDBConnection }

  THMIDBConnection = class(TComponent, IHMIDBConnection)
  private
    FConnectRead:Boolean;
    FLibraryLocation: String;
    FReadOnly: Boolean;
    FSyncConnection,
    FASyncConnection:TZConnection;
    FASyncQuery:TZQuery;
    FCS:TCriticalSection;
    FSQLSpooler:TProcessSQLCommandThread;
    function  getProperties: TStrings;
    function  GetSyncConnection:TZConnection;
    procedure ExecuteSQLCommand(sqlcmd:Utf8String; outputdataset:TFPSBufDataSet; out Error:Boolean);
    procedure SetLibraryLocation(AValue: String);
    procedure SetProperties(AValue: TStrings);
    procedure SetReadOnly(AValue: Boolean);
  protected
    FProtocol: string;
    FHostName: string;
    FPort: LongInt;
    FDatabase: string;
    FUser: string;
    FPassword: string;
    FCatalog: string;
    FProperties:TStringList;
    procedure Loaded; override;
  protected
    function GetConnected:Boolean;

    procedure SetConnected(x:Boolean);
    procedure SetProtocol(x: String);
    procedure SetHostName(x: String);
    procedure SetPort(x: LongInt);
    procedure SetDatabase(x: String);
    procedure SetUser(x: String);
    procedure SetPassword(x: String);
    procedure SetCatalog(x: String);
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollBackTransaction;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   ExecSQL(sql:String; ReturnDatasetCallback:TReturnDataSetProc; ReturnSync:Boolean=true);
    procedure   ExecTransaction(statements:THMIDBConnectionStatementList; ReturnTransactionResult:TReturnTransactionStatementsProc; FreeStatemensAfterExecute:Boolean; ReturnSync:Boolean=true);
  published
    {$IFDEF PORTUGUES}
    //: Caso @true, conecta ou está conectado ao banco de dados.
    {$ELSE}
    //: If true, connects or are connected on database.
    {$ENDIF}
    property Connected:Boolean read GetConnected write SetConnected;

    {$IFDEF PORTUGUES}
    //: Força o ZeosLib usar a biblioteca de acesso nativo apontada por este caminho.
    {$ELSE}
    //: If true, connects or are connected on database.
    {$ENDIF}
    property LibraryLocation:String read FLibraryLocation write SetLibraryLocation nodefault;

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
    property Port:     LongInt read FPort        write SetPort default 0;

    {$IFDEF PORTUGUES}
    //: Lista de propriedades da conexão
    {$ELSE}
    //: Connections properties.
    {$ENDIF}
    property Properties:TStrings read getProperties write SetProperties;

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

    {$IFDEF PORTUGUES}
    //: Verifique a documentação do TZConnection.ReadOnly do ZeosLib para maiores informações.
    {$ELSE}
    //: See the documentation of TZConnection.ReadOnly of ZeosLib for more information.
    {$ENDIF}
    property ReadOnly:Boolean  read FReadOnly    write SetReadOnly;
  end;

const
  SQLCommandMSG        = 0;
  StatementsCommandMSG = 1;

implementation

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
  SupportedDBDrivers:array[0..2] of string = ('postgresql','sqlite','mysql');

//only accepted drivers are show.
procedure THMIDBProtocolPropertyEditor.GetValueList(List: TStrings);
var
  i, s:LongInt;
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

constructor TProcessSQLCommandThread.Create(CreateSuspended: Boolean;
  ExecSQLProc: TExecSQLProc; StartTransactionProc: TStartTransaction;
  CommitTransactionProc: TCommitTransaction;
  RollbackTransactionProc: TRollbackTransaction);
begin
  inherited Create(CreateSuspended);
  FQueue:=TMessageSpool.Create;
  FEnd:=TCrossEvent.Create(true,false);
  fOnExecSQL:=ExecSQLProc;
  fStartTransaction:=StartTransactionProc;
  fCommitTransaction:=CommitTransactionProc;
  fRollbackTransaction:=RollbackTransactionProc;
end;

destructor  TProcessSQLCommandThread.Destroy;
begin
  inherited Destroy;
  FEnd.Destroy;
end;

procedure   TProcessSQLCommandThread.Execute;
var
  msg:TMSMsg;
  s: Integer;
  err: Boolean;
begin
  FEnd.ResetEvent;
  while not Terminated do begin
    while FQueue.PeekMessage(msg,0,0,true) do begin
      //executa o comando sql
      //executes the sql commmand.
      FErrorOnSync:=false;
      if (msg.MsgID=SQLCommandMSG) and (msg.wParam<>nil) then begin
        cmd:=PSQLCmdRec(msg.wParam);
        try
          //se é necessario retornar algo
          //cria o dataset de retorno de dados.
          //
          //if are to return the data,
          //creates the dataset.
          ferror:=nil;
          if Assigned(cmd^.ReturnDataSetCallback) then
            fds:=TFPSBufDataSet.Create(Nil)
          else
            fds:=nil;

          if Assigned(fOnExecSQL) then
            try
              fOnExecSQL(cmd^.SQLCmd, fds, err);
            finally
            end;

          try
            if Assigned(cmd^.ReturnDataSetCallback) then begin
              if cmd^.ReturnSync then begin
                FErrorOnSync:=true;
                Synchronize(@ReturnData);
              end else
                ReturnData;
            end;
          finally
            Dispose(cmd);
          end;
        except
          on e:Exception do begin
            if not FErrorOnSync then  begin
              ferror:=e;
              try
                if Assigned(cmd^.ReturnDataSetCallback) then
                  Synchronize(@ReturnData);
              finally
                Dispose(cmd);
              end;
            end;
          end;
        end;
      end;

      if (msg.MsgID=StatementsCommandMSG) and (msg.wParam<>nil) then begin
        statements:=PStatementCmdRec(msg.wParam);
        try
          if statements^.statements=nil then exit;

          if not Assigned(fStartTransaction) then exit;
          if not Assigned(fCommitTransaction) then exit;
          if not Assigned(fRollbackTransaction) then exit;

          if not Assigned(fOnExecSQL) then exit;

          fStartTransaction();
          try
            ferror:=nil;
            try
              fLineError:=0;
              for s:=0 to statements^.statements.Count-1 do begin
                fOnExecSQL(statements^.statements.Items[s], nil, err);
                if err then break;
              end;
            except
              on e:Exception do begin
                ferror:=e;
                fLineError:=s;
                exit;
              end;
            end;
          finally
            if (err=false) and (ferror=nil) then
              fCommitTransaction()
            else
              fRollbackTransaction();

            if Assigned(statements^.ReturnTransactionResult) then begin
              if statements^.ReturnSync then
                Synchronize(@ReturnStatementsResults)
              else begin
                if (err=false) and (ferror=nil) then
                  statements^.ReturnTransactionResult(self, statements^.statements, True, -1, nil)
                else
                  statements^.ReturnTransactionResult(self, statements^.statements, false, fLineError, ferror)
              end;
            end;
          end;
        finally
          if statements^.FreeStatemensAfterExecute then
            FreeAndNil(statements^.statements);
          Dispose(statements);
        end;
      end;
    end;
    Sleep(1);
  end;
  FEnd.SetEvent;
end;

procedure   TProcessSQLCommandThread.ReturnData;
begin
  if ferror<>nil then
    cmd^.ReturnDataSetCallback(self,nil,ferror)
  else
    cmd^.ReturnDataSetCallback(self,fds,nil);
end;

procedure TProcessSQLCommandThread.ReturnStatementsResults;
begin
  if ferror=nil then
    statements^.ReturnTransactionResult(self, statements^.statements, True, -1, nil)
  else
    statements^.ReturnTransactionResult(self, statements^.statements, false, fLineError, ferror)
end;

function    TProcessSQLCommandThread.WaitEnd(Timeout:Cardinal):TWaitResult;
begin
  Result := FEnd.WaitFor(Timeout);
end;

procedure   TProcessSQLCommandThread.ExecSQLWithoutResultSet(sql:String; ReturnSync:Boolean=true);
begin
  ExecSQLWithResultSet(sql, nil, ReturnSync);
end;

procedure   TProcessSQLCommandThread.ExecSQLWithResultSet(sql:String; ReturnDataCallback:TReturnDataSetProc; ReturnSync:Boolean=true);
var
  sqlcmd:PSQLCmdRec;
begin
  new(sqlcmd);
  sqlcmd^.SQLCmd:=sql;
  sqlcmd^.ReturnSync:=ReturnSync;
  sqlcmd^.ReturnDataSetCallback:=ReturnDataCallback;
  FQueue.PostMessage(SQLCommandMSG,sqlcmd,nil,true);
end;

procedure TProcessSQLCommandThread.ExecTransaction(
  aStatements: THMIDBConnectionStatementList;
  ReturnTransactionResult: TReturnTransactionStatementsProc;
  FreeStatemensAfterExecute: Boolean; ReturnSync: Boolean);
var
  statementcmd:PStatementCmdRec;
begin
  if aStatements=nil then exit;

  new(statementcmd);
  statementcmd^.statements:=aStatements;
  statementcmd^.ReturnTransactionResult:=ReturnTransactionResult;
  statementcmd^.FreeStatemensAfterExecute:=FreeStatemensAfterExecute;
  statementcmd^.ReturnSync:=ReturnSync;

  FQueue.PostMessage(StatementsCommandMSG,statementcmd,nil,true);
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
  FProperties:=TStringList.Create;

  FSQLSpooler:=TProcessSQLCommandThread.Create(true,@ExecuteSQLCommand,
                                               @StartTransaction,
                                               @CommitTransaction,
                                               @RollBackTransaction);
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

  FreeAndNil(FSQLSpooler);
  FreeAndNil(FASyncQuery);
  FreeAndNil(FSyncConnection);
  FreeAndNil(FASyncConnection);
  FreeAndNil(FProperties);
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

function THMIDBConnection.getProperties: TStrings;
begin
  Result:=FProperties;
end;

procedure THMIDBConnection.ExecSQL(sql:String; ReturnDatasetCallback:TReturnDataSetProc; ReturnSync:Boolean=true);
begin
  FSQLSpooler.ExecSQLWithResultSet(sql, ReturnDatasetCallback, ReturnSync);
end;

procedure THMIDBConnection.ExecTransaction(statements: THMIDBConnectionStatementList;
  ReturnTransactionResult: TReturnTransactionStatementsProc;
  FreeStatemensAfterExecute: Boolean; ReturnSync: Boolean);
begin
  FSQLSpooler.ExecTransaction(statements,ReturnTransactionResult,FreeStatemensAfterExecute,ReturnSync);
end;

procedure THMIDBConnection.StartTransaction;
begin
  FCS.Enter;
  try
    FASyncConnection.StartTransaction;
  finally
    FCS.Leave;
  end;
end;

procedure THMIDBConnection.CommitTransaction;
begin
  FCS.Enter;
  try
    FASyncConnection.Commit;
  finally
    FCS.Leave;
  end;
end;

procedure THMIDBConnection.RollBackTransaction;
begin
  FCS.Enter;
  try
    FASyncConnection.Rollback;
  finally
    FCS.Leave;
  end;
end;

procedure THMIDBConnection.ExecuteSQLCommand(sqlcmd: Utf8String;
  outputdataset: TFPSBufDataSet; out Error: Boolean);
begin
  FCS.Enter;
  try

    if FASyncConnection.ReadOnly then begin
      Error:=true;
      exit;
    end;

    try
      Error:=false;
      if outputdataset=nil then begin
        if not FASyncConnection.ExecuteDirect(sqlcmd) then begin
          Error := true;
        end;
      end else begin
        FASyncQuery.SQL.Clear;
        FASyncQuery.SQL.Add(sqlcmd);
        FASyncQuery.Open;
        outputdataset.CopyFromDataset(FASyncQuery);
        FASyncQuery.Close;
      end;
    except
      Error:=true;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure THMIDBConnection.SetLibraryLocation(AValue: String);
begin
  FSyncConnection.LibraryLocation:=AValue;
  FCS.Enter;
  try
    FASyncConnection.LibraryLocation:=AValue;
  finally
    FCS.Leave;
  end;
  FHostName:=FSyncConnection.LibraryLocation;
end;

procedure THMIDBConnection.SetProperties(AValue: TStrings);
begin
  FSyncConnection.Properties.Assign(AValue);
  FCS.Enter;
  try
    FASyncConnection.Properties.Assign(AValue);
  finally
    FCS.Leave;
  end;
  FProperties.Assign(AValue);
end;

procedure THMIDBConnection.SetReadOnly(AValue: Boolean);
begin
  FSyncConnection.ReadOnly:=AValue;
  FCS.Enter;
  try
    FASyncConnection.ReadOnly:=AValue;
  finally
    FCS.Leave;
  end;
  FReadOnly:=FSyncConnection.ReadOnly;
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

procedure THMIDBConnection.SetPort(x: LongInt);
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

end.
