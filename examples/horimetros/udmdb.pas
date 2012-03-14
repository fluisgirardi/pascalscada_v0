unit udmdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, ExtCtrls, LR_Class, LR_DBSet, LR_E_HTM,
  ZConnection, ZDataset;

type

  { Tdmdb }

  Tdmdb = class(TDataModule)
    AlarmeDatasource: TDatasource;
    AlarmesQuery: TZQuery;
    AcionamentoQueryds_equipamento: TStringField;
    AcionamentoQuerydt_duracao1: TDateTimeField;
    AcionamentoQuerydt_fim: TDateTimeField;
    AcionamentoQuerydt_inicio: TDateTimeField;
    AlarmesAtivoscd_equipamento: TLongintField;
    AlarmesAtivosds_equipamento: TStringField;
    AlarmesAtivosds_mensagem: TStringField;
    AlarmesAtivosdt_duracao1: TDateTimeField;
    AlarmesAtivosdt_fim: TDateTimeField;
    AlarmesAtivosdt_inicio: TDateTimeField;
    AlarmesAtivosid_equipamento: TLongintField;
    AlarmesQueryds_equipamento: TStringField;
    AlarmesQuerydt_duracao1: TDateTimeField;
    AlarmesQuerydt_fim: TDateTimeField;
    AlarmesQuerydt_inicio: TDateTimeField;
    Datasource1: TDatasource;
    Datasource2: TDatasource;
    AcionamentoDatasource: TDatasource;
    SQLiteConnection: TZConnection;
    AlarmesAtivos: TZQuery;
    Timer1: TTimer;
    ZQuery1: TZQuery;
    AcionamentoQuery: TZQuery;
    ZTable1: TZTable;
    ZTable1accesslevel: TLongintField;
    ZTable1Nome: TStringField;
    ZTable1password: TStringField;
    ZTable1usuario: TStringField;
    procedure AcionamentoQueryCalcFields(DataSet: TDataSet);
procedure AlarmesAtivosCalcFields(DataSet: TDataSet);
procedure AlarmesQueryCalcFields(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
    procedure Datasource2StateChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ZTable1BeforeDelete(DataSet: TDataSet);
    procedure ZTable1BeforePost(DataSet: TDataSet);
  private
    { private declarations }
  public
    procedure Access(level:Integer);
    function  TryAccess(level:Integer):Boolean;
    function Login(username, password:String):Boolean;
    procedure Logout;
  end; 

var
  dmdb: Tdmdb;
  userlogin:String;
  userlevel:Integer;

implementation

uses Forms;

{$R *.lfm}

{ Tdmdb }

procedure Tdmdb.DataModuleCreate(Sender: TObject);
var
  sqlnow:string;
begin
  SQLiteConnection.Database:=ExtractFilePath(Application.ExeName)+'historicos.db3';
  SQLiteConnection.Connect;
  sqlnow:=FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  SQLiteConnection.ExecuteDirect('UPDATE tbl_acionamento SET dt_fim='''+sqlnow+''' WHERE dt_fim IS NULL');
  SQLiteConnection.ExecuteDirect('UPDATE tbl_alarme      SET dt_fim='''+sqlnow+''' WHERE dt_fim IS NULL');
end;

procedure Tdmdb.Datasource2StateChange(Sender: TObject);
begin

end;

procedure Tdmdb.Timer1Timer(Sender: TObject);
begin
  AlarmesAtivos.Refresh;
end;

procedure Tdmdb.ZTable1BeforeDelete(DataSet: TDataSet);
begin
  if ZTable1usuario.Value='admin' then
    raise Exception.Create('Impossível deletar o administrador do sistema');
end;

procedure Tdmdb.ZTable1BeforePost(DataSet: TDataSet);
begin
  if (ZTable1usuario.Value='admin') and (ZTable1accesslevel.Value>1) then
    raise Exception.Create('Impossível alterar o access level do administrador');
end;

procedure Tdmdb.AlarmesAtivosCalcFields(DataSet: TDataSet);
begin
  AlarmesAtivosdt_duracao1.Value:=Now-AlarmesAtivosdt_inicio.Value;
end;

procedure Tdmdb.AlarmesQueryCalcFields(DataSet: TDataSet);
begin
  if AlarmesQuerydt_fim.IsNull then
    AlarmesQuerydt_duracao1.AsDateTime:=Now-AlarmesQuerydt_inicio.AsDateTime
  else
    AlarmesQuerydt_duracao1.AsDateTime:=AlarmesQuerydt_fim.AsDateTime - AlarmesQuerydt_inicio.AsDateTime
end;

procedure Tdmdb.AcionamentoQueryCalcFields(DataSet: TDataSet);
begin
  if AcionamentoQuerydt_fim.IsNull then
    AcionamentoQuerydt_duracao1.AsDateTime:=Now-AcionamentoQuerydt_inicio.AsDateTime
  else
    AcionamentoQuerydt_duracao1.AsDateTime:=AcionamentoQuerydt_fim.AsDateTime - AcionamentoQuerydt_inicio.AsDateTime
end;

procedure Tdmdb.Access(level:Integer);
begin
  if not TryAccess(level) then
    raise Exception.Create('Acesso negado!');
end;

function  Tdmdb.TryAccess(level:Integer):Boolean;
begin
  Result:=((userlevel>0) and (userlevel>level)) or (level<1);
end;

function  Tdmdb.Login(username, password:String):Boolean;
var
  currec, rec:Integer;
begin
  Result:=false;
  currec:=ZTable1.RecNo;
  for rec:=1 to ZTable1.RecordCount do begin
    ZTable1.RecNo:=rec;
    if (ZTable1.FieldByName('usuario').AsString=username) and (ZTable1.FieldByName('password').AsString=password) then begin
      userlogin:=username;
      userlevel:=ZTable1.FieldByName('accesslevel').AsInteger;
      Result:=true;
      break;
    end;
  end;
  ZTable1.RecNo:=currec;
end;

procedure Tdmdb.Logout;
begin
  userlevel:=0;
  userlogin:='';
end;

initialization
  userlevel:=0;
  userlogin:='';

end.

