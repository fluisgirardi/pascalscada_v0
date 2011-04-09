unit HMIDBConnection;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, sysutils, ZConnection, ZPropertyEditor, MessageSpool, CrossEvent,
  syncobjs, memds, ZDataset;

type
  THMIDBDatabasePropertyEditor = class(TZDatabasePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  THMIDBCatalogPropertyEditor = class(TZDatabasePropertyEditor)
  public
    function GetZComponent: TPersistent; override;
  end;

  THMIDBProtocolPropertyEditor = class(TZProtocolPropertyEditor);

  TExecSQLProc = procedure(sqlcmd:String; outputdataset:TMemDataset) of object;

  TReturnDataSetProc = procedure(Sender:TObject; DS:TMemDataset) of object;

  IHMIDBConnection = interface
    ['{C5AEA572-D7F8-4116-9A4B-3C3B972DC021}']
    function GetSyncConnection:TZConnection;
    function ExecSQL(sql:String; ReturnDatasetCallback:TReturnDataSetProc):Integer;
  end;

  TSQLCmdRec = record
    SQLCmd:String;
    ReturnDataSetCallback:TReturnDataSetProc;
  end;

  PSQLCmdRec = ^TSQLCmdRec;

  TProcessSQLCommandThread=class(TCrossThread)
  private
    FSpool:TMessageSpool;
    FEnd:TCrossEvent;
    cmd:PSQLCmdRec;
    fds:TMemDataset;
    fOnExecSQL:TExecSQLProc;
  protected
    procedure Execute; override;
    procedure ReturnData;
  public
    constructor Create(CreateSuspended: Boolean; ExecSQLProc:TExecSQLProc);
    destructor Destroy; override;
    function WaitEnd(Timeout:Cardinal):TWaitResult;
  public
    procedure ExecSQLWithoutResultSet(sql:String);
    procedure ExecSQLWithResultSet(sql:String; ReturnDataCallback:TReturnDataSetProc);
  end;

  THMIDBConnection = class(TComponent, IHMIDBConnection)
  private
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
  FASyncQuery.Create(nil);
  FASyncQuery.Connection:=FASyncConnection;
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
  FCS.Enter
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

end;

procedure THMIDBConnection.SetHostName(x: String);
begin

end;

procedure THMIDBConnection.SetPort(x: Integer);
begin

end;

procedure THMIDBConnection.SetDatabase(x: String);
begin

end;

procedure THMIDBConnection.SetUser(x: String);
begin

end;

procedure THMIDBConnection.SetPassword(x: String);
begin

end;

procedure THMIDBConnection.SetCatalog(x: String);
begin

end;

procedure THMIDBConnection.SetConnected(x:Boolean);
begin

end;

end.
