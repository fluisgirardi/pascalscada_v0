unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, StdCtrls, WinCCUserManagement, ControlSecurityManager, RTTICtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PascalSCADALoginAction1: TPascalSCADALoginAction;
    PascalSCADALogoutAction1: TPascalSCADALogoutAction;
    PascalSCADAManageUsersAction1: TPascalSCADAManageUsersAction;
    PascalSCADASecureAction1: TPascalSCADASecureAction;
    TILabel1: TTILabel;
    WinCCUserManagement1: TWinCCUserManagement;
    procedure PascalSCADASecureAction1Execute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.PascalSCADASecureAction1Execute(Sender: TObject);
begin
  ShowMessage('Ele tem acesso!');
end;

end.

