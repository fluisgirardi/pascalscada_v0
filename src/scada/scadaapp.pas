//: Unit que implementa a classe TScadaApplication
unit scadaapp;

interface

uses
  SysUtils, Classes, ZConnection, db, Dialogs;

type
  TScadaApp = class(TComponent)
  private
    PZConnection:TZConnection;
    procedure DropTable(tablename:AnsiString);
    procedure CheckDBConnection;
    procedure SetDBConnection(db:TZConnection);
    function TableExists(tablename:AnsiString):Boolean;
    function FieldExists(tablename, fieldname:AnsiString):TFieldType;
    procedure CheckSystemTables;

    //procedure para criação de tabelas especificas do sistema.
    procedure Create_tbl_usuarios;
    procedure Create_tbl_integrantes;
    procedure Create_tbl_grupos;
    procedure Create_tbl_permissoes;
    procedure Create_tbl_objetos;
  protected
    { Protected }
  public
    { Public declarations }
  published
    property DBConnection:TZConnection read PZConnection write SetDBConnection;
  end;

var
  FScadaAppMgr:TScadaApp;

const
  LongIntFieldTypes = [ftSmallint, ftLongInt, ftWord, ftAutoInc, ftLargeint];
  StringFieldTypes  = [ftString, ftWideString];
  DateTimeFieldTypes= [ftDate,  ftTime, ftDateTime, ftTimeStamp];
  FloatFieldTypes   = [ftFloat, ftCurrency];
  NumericFieldTypes = [ftSmallint, ftLongInt, ftWord, ftAutoInc, ftLargeint,
                       ftFloat, ftCurrency];

implementation

uses ZDataset, Controls;

procedure TScadaApp.SetDBConnection(db:TZConnection);
var
  match:boolean;
begin
  if db=PZConnection then exit;

  //se não está em desing e esta tentando remover a conexao...
  if (db=nil) then
    raise Exception.Create(SimpossibleToRemoveWhenBusy);

  if db<>nil then begin
    match := false;

    match := match or (LowerCase(LeftStr(db.Protocol,10))='postgresql');
    match := match or (LowerCase(LeftStr(db.Protocol,6)) ='sqlite');
    match := match or (LowerCase(LeftStr(db.Protocol,5)) ='mysql');

    if not match then
      raise Exception.Create(SonlyMySQL_SQLite_PostgresSupported);
  end;

  PZConnection:=db;

  if db<>nil then
    CheckSystemTables;
end;

procedure TScadaApp.CheckDBConnection;
begin
  if PZConnection=nil then
    raise Exception.Create(SwithoutDBConnection);

  if not PZConnection.Connected then
    raise Exception.Create(SDBConnectionRequired);
end;

procedure TScadaApp.DropTable(tablename:AnsiString);
begin
   CheckDBConnection;

   PZConnection.ExecuteDirect('DROP TABLE '+tablename);
end;

function TScadaApp.TableExists(tablename:AnsiString):Boolean;
var
  affected:LongInt;
  tname,sql:AnsiString;
  found:boolean;
begin
  CheckDBConnection;

  found:=false;
  Result:=false;
  tname := PZConnection.DbcConnection.EscapeString(tablename);

  //postgres.
  if (not found) and (LowerCase(LeftStr(PZConnection.Protocol,10))='postgresql') then begin
    sql := 'select tablename from pg_tables WHERE tablename='''+tname+'''';
    found:=true;
    exit;
  end;

  //SQLITE.
  if (not found) and (LowerCase(LeftStr(PZConnection.Protocol,5))='mysql') then begin
    sql := 'SELECT name FROM sqlite_master WHERE type=''table'' AND name='''+tname+'''';
    found:=true;
    exit;
  end;

  //mysql.
  if (not found) and (LowerCase(LeftStr(PZConnection.Protocol,5))='mysql') then begin
    sql := 'SHOW TABLES WHERE Tables_in_'+PZConnection.Database+'='''+tname+'''';
    found:=true;
    exit;
  end;

  if (not found) or (not PZConnection.ExecuteDirect(sql,affected)) then exit;

  Result := (affected>0);

end;

function TScadaApp.FieldExists(tablename, fieldname:AnsiString):TFieldType;
var
  x:TZQuery;
begin
  FieldExists:=ftUnknown;
  if TableExists(tablename) then begin
    try
      x:=TZQuery.Create(self);
      x.Connection := PZConnection;
      with x.SQL do begin
        Clear;
        Add('SELECT * FROM '+tablename+' LIMIT 1');
      end;
      x.Open;
      if x.FieldByName(fieldname)<>nil then begin
        FieldExists := x.FieldByName(fieldname).DataType;
      end;
      x.Close;
    finally
      x.Destroy;
    end;
  end;
end;

procedure TScadaApp.CheckSystemTables;
var
  ok:Boolean;
begin
  if TableExists('tbl_usuarios') then begin
    ok := true;
    ok := ok and (FieldExists('tbl_usuarios','id_usuario') in LongIntFieldTypes);
    ok := ok and (FieldExists('tbl_usuarios','ds_login') in StringFieldTypes);
    ok := ok and (FieldExists('tbl_usuarios','ds_password') in StringFieldTypes);
    ok := ok and (FieldExists('tbl_usuarios','ds_fullname') in StringFieldTypes);
    ok := ok and (FieldExists('tbl_usuarios','bl_blocked') in LongIntFieldTypes);

    if not ok then begin
      if MessageDlg('A tabela de usuários está corrompida ou alterada.'+LineEnding+LineEnding+
                    'Deseja recriar a tabela',mtWarning,mbYesNo,0)<>mrYes then
        raise Exception.Create(StablesCorrupt);
      DropTable('tbl_usuarios');
      Create_tbl_usuarios;
    end;
  end else begin
    if MessageDlg('A tabela de usuários não existe.'+LineEnding+LineEnding+
                  'Deseja criar a tabela?',mtWarning,mbYesNo,0)=mrYes then
      Create_tbl_usuarios
    else
      raise Exception.Create(SusersTableNotExist);
  end;

  if TableExists('tbl_integrantes') then begin
    ok := true;
    ok := ok and (FieldExists('tbl_integrantes','cd_usuario') in LongIntFieldTypes);
    ok := ok and (FieldExists('tbl_integrantes','cd_grupo') in LongIntFieldTypes);

    if not ok then begin
      if MessageDlg('A tabela de integrantes dos grupos está corrompida ou alterada.'+LineEnding+LineEnding+
                    'Deseja recriar a tabela',mtWarning,mbYesNo,0)<>mrYes then
        raise Exception.Create(StablesCorrupt);
      DropTable('tbl_integrantes');
      Create_tbl_integrantes;
    end;
  end else begin
    if MessageDlg('A tabela de integrantes dos grupos não existe.'+LineEnding+LineEnding+
                  'Deseja criar a tabela?',mtWarning,mbYesNo,0)=mrYes then
      Create_tbl_integrantes
    else
      raise Exception.Create(SuserTableCorrupted);
  end;

  if TableExists('tbl_grupos') then begin
    ok := true;
    ok := ok and (FieldExists('tbl_grupos','cd_usuario') in LongIntFieldTypes);
    ok := ok and (FieldExists('tbl_grupos','cd_grupo') in StringFieldTypes);

    if not ok then begin
      if MessageDlg('A tabela de grupos está corrompida ou alterada.'+LineEnding+LineEnding+
                    'Deseja recriar a tabela',mtWarning,mbYesNo,0)<>mrYes then
        raise Exception.Create(StablesCorrupt);
      DropTable('tbl_grupos');
      Create_tbl_grupos;
    end;
  end else begin
    if MessageDlg('A tabela de grupos não existe.'+LineEnding+LineEnding+
                  'Deseja criar a tabela?',mtWarning,mbYesNo,0)=mrYes then
      Create_tbl_grupos
    else
      raise Exception.Create(SgroupsTableNotExist);
  end;

  if TableExists('tbl_permissoes') then begin
    ok := true;
    ok := ok and (FieldExists('tbl_permissoes','id_permissao') in LongIntFieldTypes);
    ok := ok and (FieldExists('tbl_permissoes','cd_usuario') in LongIntFieldTypes);
    ok := ok and (FieldExists('tbl_permissoes','cd_grupo') in LongIntFieldTypes);
    ok := ok and (FieldExists('tbl_permissoes','cd_objeto') in LongIntFieldTypes);

    if not ok then begin
      if MessageDlg('A tabela de permissões está corrompida ou alterada.'+LineEnding+LineEnding+
                    'Deseja recriar a tabela',mtWarning,mbYesNo,0)<>mrYes then
        raise Exception.Create(StablesCorrupt);
      DropTable('tbl_permissoes');
      Create_tbl_permissoes;
    end;
  end else begin
    if MessageDlg('A tabela de permissões não existe.'+LineEnding+LineEnding+
                  'Deseja criar a tabela?',mtWarning,mbYesNo,0)=mrYes then
      Create_tbl_permissoes
    else
      raise Exception.Create(SgroupsTableNotExist);
  end;

  if TableExists('tbl_objetos') then begin
    ok := true;
    ok := ok and (FieldExists('tbl_objetos','id_permissao') in LongIntFieldTypes);
    ok := ok and (FieldExists('tbl_objetos','cd_usuario') in LongIntFieldTypes);
    ok := ok and (FieldExists('tbl_objetos','cd_grupo') in LongIntFieldTypes);
    ok := ok and (FieldExists('tbl_objetos','cd_objeto') in LongIntFieldTypes);

    if not ok then begin
      if MessageDlg('A tabela de objetos está corrompida ou alterada.'+LineEnding+LineEnding+
                    'Deseja recriar a tabela',mtWarning,mbYesNo,0)<>mrYes then
        raise Exception.Create(StablesCorrupt);
      DropTable('tbl_objetos');
      Create_tbl_objetos;
    end;
  end else begin
    if MessageDlg('A tabela de objetos não existe.'+LineEnding+LineEnding+
                  'Deseja criar a tabela?',mtWarning,mbYesNo,0)=mrYes then
      Create_tbl_objetos
    else
      raise Exception.Create(SgroupsTableNotExist);
  end;
end;

procedure TScadaApp.Create_tbl_usuarios;
begin

end;

procedure TScadaApp.Create_tbl_integrantes;
begin

end;

procedure TScadaApp.Create_tbl_grupos;
begin

end;

procedure TScadaApp.Create_tbl_permissoes;
begin

end;

procedure TScadaApp.Create_tbl_objetos;
begin

end;


initialization
   FScadaAppMgr:=nil;
end.
