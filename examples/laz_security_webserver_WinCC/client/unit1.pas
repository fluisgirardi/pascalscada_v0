unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  CentralUserManagement, ControlSecurityManager, HMITrackBar;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CentralUserManagement1: TCentralUserManagement;
    HMITrackBar1: THMITrackBar;
    HMITrackBar2: THMITrackBar;
    PascalSCADALoginAction1: TPascalSCADALoginAction;
    PascalSCADALogoutAction1: TPascalSCADALogoutAction;
    PascalSCADASecureAction1: TPascalSCADASecureAction;
    PascalSCADASecureAction2: TPascalSCADASecureAction;
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PascalSCADASecureAction1Execute(Sender: TObject);
    procedure PascalSCADASecureAction2Execute(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  CentralUserManagement1.UseCentralUserAsLocalUser:=true
end;

procedure TForm1.PascalSCADASecureAction1Execute(Sender: TObject);
begin

end;

procedure TForm1.PascalSCADASecureAction2Execute(Sender: TObject);
begin

end;

end.

