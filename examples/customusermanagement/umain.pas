unit umain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, RTTIGrids, Forms, Controls, Graphics,
  Dialogs, Menus, ActnList, StdCtrls, CustomizedUserManagement, PLCTagNumber,
  HMIEdit, ControlSecurityManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    CustomizedUserManagement1: TCustomizedUserManagement;
    HMIEdit1: THMIEdit;
    HMIEdit2: THMIEdit;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    PascalSCADALoginAction1: TPascalSCADALoginAction;
    PascalSCADALogoutAction1: TPascalSCADALogoutAction;
    Action1: TPascalSCADASecureAction;
    Action2: TPascalSCADASecureAction;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
    TILabel1: TTILabel;
    TILabel2: TTILabel;
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure CustomizedUserManagement1CanAccess(securityCode: String;
      var CanAccess: Boolean);
    procedure CustomizedUserManagement1CheckUserAndPass(user, pass: String;
      var ValidUser: Boolean);
    procedure CustomizedUserManagement1GetUserLogin(var UserInfo: String);
    procedure CustomizedUserManagement1Logout(Sender: TObject);
  private
    LastValidUser:String;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CustomizedUserManagement1CheckUserAndPass(user, pass: String;
  var ValidUser: Boolean);
begin
  //check the user login and password
  ValidUser:=(((user='fabio') and (pass='123')) or ((user='user') and (pass='321')) or ((user='root') and (pass='333')));
  if ValidUser then
    LastValidUser:=user;
end;

procedure TForm1.CustomizedUserManagement1GetUserLogin(var UserInfo: String);
begin
  UserInfo:=LastValidUser; //return last logged user login.
end;

procedure TForm1.CustomizedUserManagement1Logout(Sender: TObject);
begin
  LastValidUser:='';
end;

procedure TForm1.CustomizedUserManagement1CanAccess(securityCode: String;
  var CanAccess: Boolean);
begin
  //check if the current user can access the securityCode
  CanAccess :=((LastValidUser='fabio') and (securityCode='autorizacao1')) or
              ((LastValidUser='user') and (securityCode='autorizacao2')) or
              ((LastValidUser='root') and ((securityCode='autorizacao1') or (securityCode='autorizacao2')));
end;

procedure TForm1.Action1Execute(Sender: TObject);
begin
  ShowMessage('usuário com permissao de acesso a autorizacao "autorizacao1"!'+LineEnding+
              'The current can access the authorization "autorizacao1"!');
end;

procedure TForm1.Action2Execute(Sender: TObject);
begin
  ShowMessage('usuário com permissao de acesso a autorizacao "autorizacao2"!'+LineEnding+
              'The current can access the authorization "autorizacao2"!');
end;

end.

