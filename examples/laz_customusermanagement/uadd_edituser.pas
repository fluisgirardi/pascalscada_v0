unit uadd_edituser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, DbCtrls;

type

  { TfrmInsertEditUser }

  TfrmInsertEditUser = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    DBCheckBox1: TDBCheckBox;
    DBEdit2: TDBEdit;
    DBText1: TDBText;
    dsUsers: TDataSource;
    DBEdit1: TDBEdit;
    edtSenha1: TEdit;
    edtSenha2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FCancelado:Boolean;
  public
    { public declarations }
  end;

var
  frmInsertEditUser: TfrmInsertEditUser;

implementation

uses udm;

{$R *.lfm}

{ TfrmInsertEditUser }

procedure TfrmInsertEditUser.FormCreate(Sender: TObject);
begin
  edtSenha1.Modified:=false;
  edtSenha2.Modified:=false;
  FCancelado:=false;
end;

procedure TfrmInsertEditUser.BitBtn1Click(Sender: TObject);
begin

end;

procedure TfrmInsertEditUser.BitBtn2Click(Sender: TObject);
begin
  FCancelado:=true;
end;

procedure TfrmInsertEditUser.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if (not FCancelado) and
     (edtSenha1.Modified or edtSenha2.Modified) and
     (edtSenha1.Text<>edtSenha2.Text) then
    raise exception.Create('A senha é diferente da contra senha. Digite a mesma senha no campo "Senha" e "Confirme senha"!');
end;

{ TfrmInsertEditUser }

end.

