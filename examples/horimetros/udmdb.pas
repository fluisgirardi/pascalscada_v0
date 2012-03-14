unit udmdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, ExtCtrls, LR_Class, LR_DBSet, LR_E_HTM,
  ZConnection, ZDataset, CustomizedUserManagement, ControlSecurityManager;

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
    CustomizedUserManagement1: TCustomizedUserManagement;
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
    procedure CustomizedUserManagement1CanAccess(securityCode: String;
      var CanAccess: Boolean);
    procedure CustomizedUserManagement1CheckUserAndPass(user, pass: String;
      var ValidUser: Boolean);
    procedure CustomizedUserManagement1GetUserLogin(var UserInfo: String);
    procedure CustomizedUserManagement1GetUserName(var UserInfo: String);
    procedure CustomizedUserManagement1Logout(Sender: TObject);
    procedure CustomizedUserManagement1ManageUsersAndGroups(Sender: TObject);
    procedure CustomizedUserManagement1ValidadeSecurityCode(
      const securityCode: String);
    procedure DataModuleCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ZTable1BeforeDelete(DataSet: TDataSet);
    procedure ZTable1BeforePost(DataSet: TDataSet);
  private
    { private declarations }
  end;

var
  dmdb: Tdmdb;
  fullusername,
  userlogin:String;
  userlevel:Integer;

implementation

uses Forms, Dialogs, ugerenciamentousuarios;

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

procedure Tdmdb.CustomizedUserManagement1CanAccess(securityCode: String;
  var CanAccess: Boolean);
var
  level:Integer;
begin
  if not TryStrToInt(securityCode, level) then level:=0;
  CanAccess:=((userlevel>0) and (userlevel<=level)) or (level<1);
end;

procedure Tdmdb.CustomizedUserManagement1CheckUserAndPass(user, pass: String;
  var ValidUser: Boolean);
var
  rec:Integer;
  pos:TBookmark;
begin
  ValidUser:=false;
  pos:=ZTable1.GetBookmark;
  ZTable1.DisableControls;
  for rec:=1 to ZTable1.RecordCount do begin
    ZTable1.RecNo:=rec;
    if (ZTable1.FieldByName('usuario').AsString=user) and (ZTable1.FieldByName('password').AsString=pass) then begin
      userlogin:=user;
      userlevel:=ZTable1.FieldByName('accesslevel').AsInteger;
      fullusername:=ZTable1.FieldByName('Nome').AsString;
      ValidUser:=true;
      break;
    end;
  end;
  ZTable1.EnableControls;
  ZTable1.GotoBookmark(pos);
  ZTable1.FreeBookmark(pos);
end;

procedure Tdmdb.CustomizedUserManagement1GetUserLogin(var UserInfo: String);
begin
  UserInfo:=userlogin;
end;

procedure Tdmdb.CustomizedUserManagement1GetUserName(var UserInfo: String);
begin
  UserInfo:=fullusername;
end;

procedure Tdmdb.CustomizedUserManagement1Logout(Sender: TObject);
begin
  userlevel:=0;
  userlogin:='';
  fullusername:='';
end;

procedure Tdmdb.CustomizedUserManagement1ManageUsersAndGroups(Sender: TObject);
var
  um:TfrmUsuarios;
begin
  GetControlSecurityManager.TryAccess('1');

  um:=TfrmUsuarios.Create(Self);
  try
    um.ShowModal;
  finally
    um.Destroy;
  end;
end;

procedure Tdmdb.CustomizedUserManagement1ValidadeSecurityCode(
  const securityCode: String);
var
  x:Integer;
begin
  if (securityCode<>'') and (not TryStrToInt(securityCode,x)) then
    raise Exception.Create('Nivel de segurança precisa ser numérico!');
end;

procedure Tdmdb.AcionamentoQueryCalcFields(DataSet: TDataSet);
begin
  if AcionamentoQuerydt_fim.IsNull then
    AcionamentoQuerydt_duracao1.AsDateTime:=Now-AcionamentoQuerydt_inicio.AsDateTime
  else
    AcionamentoQuerydt_duracao1.AsDateTime:=AcionamentoQuerydt_fim.AsDateTime - AcionamentoQuerydt_inicio.AsDateTime
end;

initialization
  userlevel:=0;
  userlogin:='';
  fullusername:='';

end.

