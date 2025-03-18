unit umainmetodos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  CustomizedUserManagement;

type

  { TForm1 }

  TForm1 = class(TForm)
    CustomizedUserManagement1: TCustomizedUserManagement;
    procedure CustomizedUserManagement1CanAccess(securityCode: UTF8String;
      var CanAccess: Boolean);
    procedure CustomizedUserManagement1CheckUserAndPass(user, pass: UTF8String;
      out aUID: Integer; var ValidUser: Boolean; LoginAction: Boolean);
    procedure CustomizedUserManagement1CheckUserChipCard(
      aChipCardCode: UTF8String; var userlogin: UTF8String;
      var UserID: Integer; var ValidChipCard: Boolean; LoginAction: Boolean);
    procedure CustomizedUserManagement1GetUserLogin(var UserInfo: UTF8String);
    procedure CustomizedUserManagement1GetUserName(var UserInfo: UTF8String);
    procedure CustomizedUserManagement1Logout(Sender: TObject);
    procedure CustomizedUserManagement1RegisterSecurityCode(
      const securityCode: UTF8String);
    procedure CustomizedUserManagement1UIDCanAccess(aUID: Integer;
      securityCode: UTF8String; var CanAccess: Boolean);
    procedure CustomizedUserManagement1ValidadeSecurityCode(
      const securityCode: UTF8String);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.CustomizedUserManagement1CheckUserAndPass(user,
  pass: UTF8String; out aUID: Integer; var ValidUser: Boolean;
  LoginAction: Boolean);
begin

end;

procedure TForm1.CustomizedUserManagement1CheckUserChipCard(
  aChipCardCode: UTF8String; var userlogin: UTF8String; var UserID: Integer;
  var ValidChipCard: Boolean; LoginAction: Boolean);
begin

end;

procedure TForm1.CustomizedUserManagement1GetUserLogin(var UserInfo: UTF8String
  );
begin

end;

procedure TForm1.CustomizedUserManagement1GetUserName(var UserInfo: UTF8String);
begin

end;

procedure TForm1.CustomizedUserManagement1Logout(Sender: TObject);
begin

end;

procedure TForm1.CustomizedUserManagement1RegisterSecurityCode(
  const securityCode: UTF8String);
begin

end;

procedure TForm1.CustomizedUserManagement1UIDCanAccess(aUID: Integer;
  securityCode: UTF8String; var CanAccess: Boolean);
begin

end;

procedure TForm1.CustomizedUserManagement1ValidadeSecurityCode(
  const securityCode: UTF8String);
begin

end;

procedure TForm1.CustomizedUserManagement1CanAccess(securityCode: UTF8String;
  var CanAccess: Boolean);
begin

end;

end.

