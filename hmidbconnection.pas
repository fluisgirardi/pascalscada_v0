unit HMIDBConnection;

{$mode delphi}

interface

uses
  Classes, sysutils, ZConnection, ZPropertyEditor;

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

  IHMIDBConnection = interface
    ['{C5AEA572-D7F8-4116-9A4B-3C3B972DC021}']
    function GetSyncConnection:TZConnection;
    function ExecSQL(sql:String; NotifyAfter:TNotifyEvent):Integer;
  end;

  THMIDBConnection = class(TComponent, IHMIDBConnection)
  private
    FSyncConnection,
    FASyncConneciton:TZConnection;
    function GetSyncConnection:TZConnection;
    function ExecSQL(sql:String; NotifyAfter:TNotifyEvent):Integer;
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

    procedure SetProtocol(x: String);
    procedure SetHostName(x: String);
    procedure SetPort(x: Integer);
    procedure SetDatabase(x: String);
    procedure SetUser(x: String);
    procedure SetPassword(x: String);
    procedure SetCatalog(x: String);
    procedure SetConnected(x:Boolean);
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

implementation

function THMIDBDatabasePropertyEditor.GetZComponent:TPersistent;
begin
  Result:=GetComponent(0);
  if (Result is THMIDBConnection) and Supports(Result, IHMIDBConnection) then
    Result:=(Result as IHMIDBConnection).GetSyncConnection;
end;

function THMIDBCatalogPropertyEditor.GetZComponent:TPersistent;
begin
  Result:=GetComponent(0);
  if (Result is THMIDBConnection) and Supports(Result, IHMIDBConnection) then
    Result:=(Result as IHMIDBConnection).GetSyncConnection;
end;

//##############################################################################

constructor THMIDBConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FASyncConneciton:=TZConnection.Create(nil);
  FSyncConnection:=TZConnection.Create(nil);
end;

destructor  THMIDBConnection.Destroy;
begin
  inherited Destroy;
end;

function THMIDBConnection.GetSyncConnection:TZConnection;
begin
  Result:=FSyncConnection;
end;

function THMIDBConnection.ExecSQL(sql:String; NotifyAfter:TNotifyEvent):Integer;
begin
  //posta uma mensagem para execução assincrona.
  Result:=0;
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
