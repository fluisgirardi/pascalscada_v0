unit ugerenciamentousuarios;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  StdCtrls, Buttons;

type

  { TfrmUsuarios }

TfrmUsuarios = class(TForm)
    BitBtn1: TBitBtn;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure StateChange(Sender: TObject);
  public
    { public declarations }
  end; 

var
  frmUsuarios: TfrmUsuarios;

implementation

uses udmdb, db;

{$R *.lfm}

{ TfrmUsuarios }

procedure TfrmUsuarios.BitBtn1Click(Sender: TObject);
var
  oldpw, pw1, pw2:String;
begin
  if dmdb.Datasource2.State<>dsInsert then begin
    oldpw:=PasswordBox('Definir senha','Digite sua senha atual');
    if oldpw<>dmdb.ZTable1password.Value then
      raise Exception.Create('Falha autenticando o usuário!');
  end;

  pw1:=PasswordBox('Definir senha','Digite sua nova senha');
  pw2:=PasswordBox('Definir senha','Confirme sua senha');

  if pw1<>pw2 then
    raise Exception.Create('Erro confirmando senha!');

  if not (dmdb.Datasource2.State in dsEditModes) then
    dmdb.ZTable1.Edit;

  dmdb.ZTable1password.Value:=pw1;

  dmdb.ZTable1.Post;

  MessageDlg('Senha definida com sucesso!',mtInformation,[mbOK],0);
end;

procedure TfrmUsuarios.FormCreate(Sender: TObject);
begin
  dmdb.Datasource2.OnStateChange:=@StateChange;
  StateChange(Self);
end;

procedure TfrmUsuarios.StateChange(Sender: TObject);
begin
  DBEdit2.Enabled:=(dmdb.Datasource2.State=dsInsert);
end;

end.

