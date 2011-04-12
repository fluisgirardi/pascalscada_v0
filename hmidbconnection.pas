unit HMIDBConnection;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, sysutils, ZConnection, ZPropertyEditor, MessageSpool, CrossEvent,
  syncobjs, memds, ZDataset;

type
  //: Editor da propriedade THMIDBConnection.Database
  THMIDBDatabasePropertyEditor = class(TZDatabasePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  //: Editor da propriedade THMIDBConnection.Catalog
  THMIDBCatalogPropertyEditor = class(TZDatabasePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  //: Editor da propriedade THMIDBConnection.Protocol
  THMIDBProtocolPropertyEditor = class(TZProtocolPropertyEditor);

  //: Método usado pela thread para execução de uma consulta.
  TExecSQLProc = procedure(sqlcmd:String; outputdataset:TMemDataset) of object;

  //: Método usado pela thread para retornar um dataset após a execução da consulta.
  TReturnDataSetProc = procedure(Sender:TObject; DS:TMemDataset) of object;

  //: Inteface para interação com objetos privados do THMIDBConnection
  IHMIDBConnection = interface
    ['{C5AEA572-D7F8-4116-9A4B-3C3B972DC021}']
    //: Retorna um TZConnection para os editores de propriedade.
    function GetSyncConnection:TZConnection;
    {:
      Função que executa uma consulta assincrona.
      @param(sql String. Commando SQL. Se for uma consulta de seleção onde se
                         deseja obter os dados retornados, é necessário informar
                         uma procedure válida no parametro ReturnDatasetCallback.)
      @param(ReturnDatasetCallback TReturnDataSetProc. Ponteiro para o procedimento
                         que vai ser chamado quando para retornar os dados a aplicação.)
    }
    function ExecSQL(sql:String; ReturnDatasetCallback:TReturnDataSetProc):Integer;
  end;

  //: Estrutura de comando SQL que é enviado a thread.
  TSQLCmdRec = record
    SQLCmd:String;
    ReturnDataSetCallback:TReturnDataSetProc;
  end;

  //: Pointeiro de estrutura de mensagem de comando SQL.
  PSQLCmdRec = ^TSQLCmdRec;

  {:
  Fila de execução assincrona de comandos SQL.
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  }
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
    {:
    Cria a fila de processamento assincrono.

    @param(CreateSuspended  Boolean. Se verdadeiro, a fila é criada suspensa,
                                     sendo necessário chamar o procedimento
                                     Resume posteriormente para que ela comece a
                                     trabalhar.)
    @param(ExecSQLProc TExecSQLProc. Ponteiro para o procedimento que será chamado
                                     para executar as consultas SQL.)
    }
    constructor Create(CreateSuspended: Boolean; ExecSQLProc:TExecSQLProc);
    //: @exclude
    destructor Destroy; override;
    {:
    Espera determinado tempo pela finalização da thread.
    @param(Timeout Cardinal. Tempo máximo de espera.)
    @returns(Retorna wrSignaled caso a thread seja finalizada antes do tempo
             máximo de espera (Timeout). Caso a fila não termine antes do tempo
             máximo retorna wrTimeout. Caso o procedimento Destroy for chamado
             antes do método Terminate seguido @name, pode retornar wrAbandoned
             ou wrError.)
    }
    function WaitEnd(Timeout:Cardinal):TWaitResult;
  public
    {:
    Executa uma consulta SQL sem retornar um DataSet para a aplicação.
    @param(sql String. Comando SQL a executar.)
    }
    procedure ExecSQLWithoutResultSet(sql:String);
    {:
    Executa uma consulta SQL e rotorna um DataSet para a aplicação.
    @param(sql String. Comando SQL a executar.)
    @param(ReturnDataCallback TReturnDataSetProc. Procedimento que é chamado para
                                                  retornar o dataset resultante da
                                                  consulta para a aplicação.)
    }
    procedure ExecSQLWithResultSet(sql:String; ReturnDataCallback:TReturnDataSetProc);
  end;

  {:
  Componente de banco de dados do PascalSCADA.
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  }
  THMIDBConnection = class(TComponent, IHMIDBConnection)
  private
    FConnectRead:Boolean;
    FSyncConnection,
    FASyncConnection:TZConnection;
    FASyncQuery:TZQuery;
    FCS:TCriticalSection;
    FSQLSpooler:TProcessSQLCommandThread;
    function  GetSyncConnection:TZConnection;
    function  ExecSQL(sql:String; ReturnDatasetCallback:TReturnDataSetProc):Integer;
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
  published
    property Connected:Boolean read GetConnected write SetConnected;
    property Protocol: string  read FProtocol    write SetProtocol;
    property HostName: string  read FHostName    write SetHostName;
    property Port:     Integer read FPort        write SetPort default 0;
    property Database: string  read FDatabase    write SetDatabase;
    property User:     string  read FUser        write SetUser;
    property Password: string  read FPassword    write SetPassword;
    property Catalog:  string  read FCatalog     write SetCatalog;
  end;

const
  SQLCommandMSG = 1024;

implementation

//##############################################################################
//EDITORES DE PROPRIEDADES DA CLASSE THMIDBCONNECTION
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

//##############################################################################
//THREAD DE EXECUÇÃO DOS COMANDOS SQL THMIDBCONNECTION
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
      if (msg.MsgID=SQLCommandMSG) and (msg.wParam<>nil) then begin
        cmd:=PSQLCmdRec(msg.wParam);
        try
          //se é necessario retornar algo
          //cria o dataset de retorno de dados.
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
//##############################################################################

constructor THMIDBConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCS:=TCriticalSection.Create;
  FSyncConnection:=TZConnection.Create(nil);
  FASyncConnection:=TZConnection.Create(nil);
  FASyncQuery:=TZQuery.Create(nil);
  FASyncQuery.Connection:=FASyncConnection;

  FSQLSpooler:=TProcessSQLCommandThread.Create(true,ExecuteSQLCommand);
  FSQLSpooler.Resume;
end;

destructor  THMIDBConnection.Destroy;
begin
  inherited Destroy;
  //destroi a thread
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
end;

procedure THMIDBConnection.ExecuteSQLCommand(sqlcmd:String; outputdataset:TMemDataset);
begin
  FCS.Enter;
  try
    FASyncQuery.SQL.Clear;
    FASyncQuery.SQL.Add(sqlcmd);
    if outputdataset=nil then begin
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

end.
