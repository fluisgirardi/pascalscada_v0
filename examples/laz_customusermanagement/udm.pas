unit udm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  CustomizedUserManagement, ControlSecurityManager, uusermanagement,
  ZConnection, ZDataset, DB, md5, Variants;

type

  { Tdm }

  Tdm = class(TDataModule)
    SecurityActions: TActionList;
    CustomizedUserManagement1: TCustomizedUserManagement;
    PascalSCADALoginAction1: TPascalSCADALoginAction;
    PascalSCADALogoutAction1: TPascalSCADALogoutAction;
    PascalSCADAManageUsersAction1: TPascalSCADAManageUsersAction;
    PGConnUsers: TZConnection;
    tblGroups: TZTable;
    tblGroupsds_groupdescription: TStringField;
    tblGroupsds_groupname: TStringField;
    tblGroupsid_group: TLongintField;
    tblGroupsop_admin: TBooleanField;
    tblGroupsop_unloggedin: TBooleanField;
    tblListMembersOfGroup: TZQuery;
    tblListMembersOfGroupcd_alias_for_user: TLongintField;
    tblListMembersOfGroupds_fullname: TStringField;
    tblListMembersOfGroupds_login: TStringField;
    tblListMembersOfGroupds_password: TStringField;
    tblListMembersOfGroupid_user: TLongintField;
    tblListMembersOfGroupmember: TBooleanField;
    tblListMembersOfGroupop_blocked: TBooleanField;
    tblListPermsOfGroup: TZQuery;
    tblListPermsOfUser: TZQuery;
    tblListPermsOfUser1: TZQuery;
    tblObjects: TZTable;
    tblObjectsds_longdescription: TStringField;
    tblObjectsds_object: TStringField;
    tblObjectsid_object: TLargeintField;
    tblPermission: TZTable;
    tblUsers: TZTable;
    tblUserscd_alias_for_user: TLongintField;
    tblUsersds_automated_login_code: TStringField;
    tblUsersds_fullname: TStringField;
    tblUsersds_login: TStringField;
    tblUsersds_password: TStringField;
    tblUsersid_user: TLargeintField;
    tblUsersop_blocked: TBooleanField;
    procedure CustomizedUserManagement1CanAccess(securityCode: UTF8String;
      var CanAccess: Boolean);
    procedure CustomizedUserManagement1CheckUserAndPass(user, pass: UTF8String;
      var aUID: Integer; var ValidUser: Boolean; LoginAction: Boolean);
    procedure CustomizedUserManagement1CheckUserChipCard(
      aChipCardCode: UTF8String; var userlogin: UTF8String;
      var UserID: Integer; var ValidChipCard: Boolean; LoginAction: Boolean);
    procedure CustomizedUserManagement1FailureLogin(Sender: TObject);
    procedure CustomizedUserManagement1GetUserLogin(var UserInfo: UTF8String);
    procedure CustomizedUserManagement1GetUserName(var UserInfo: UTF8String);
    procedure CustomizedUserManagement1Logout(Sender: TObject);
    procedure CustomizedUserManagement1ManageUsersAndGroups(Sender: TObject);
    procedure CustomizedUserManagement1RegisterSecurityCode(
      const securityCode: UTF8String);
    procedure CustomizedUserManagement1SuccessfulLogin(Sender: TObject);
    procedure CustomizedUserManagement1UIDCanAccess(aUID: Integer;
      securityCode: UTF8String; var CanAccess: Boolean);
    procedure CustomizedUserManagement1ValidadeSecurityCode(
      const securityCode: UTF8String);
    procedure FormCreate(Sender: TObject);
  private
  public
    FUserLogin,
    FUserName:String;
    frmUM: TfrmUserManagement;
  end;

const
    FSecCodeManageUsers = 'Gerenciar usuários';

var
  dm: Tdm;

implementation

{$R *.lfm}


procedure Tdm.CustomizedUserManagement1CheckUserAndPass(user,
  pass: UTF8String; var aUID: Integer; var ValidUser: Boolean; LoginAction: Boolean
  );
var
  md5pass: String;
begin
  ValidUser:=false;
  {$IFDEF ReadOnly}
  exit;
  {$ENDIF}

  tblUsers.Close;
  tblUsers.Open;

  //alias de login
  md5pass:=MD5Print(MD5String(pass));
  if tblUsers.Locate('ds_login;ds_password',VarArrayOf([user, md5pass]),[loCaseInsensitive]) and (tblUserscd_alias_for_user.IsNull=false) and (tblUsersop_blocked.Value=false) then begin
    if tblUsers.Locate('id_user',tblUserscd_alias_for_user.Value,[]) and (tblUsersop_blocked.Value=false) then begin
      ValidUser:=true;
      aUID:=tblUsersid_user.Value;
      if LoginAction then begin
        FUserLogin:=tblUsersds_login.Value;
        FUserName :=tblUsersds_fullname.Value;

        tblListPermsOfUser.Close;
        tblListPermsOfUser.ParamByName('uid').Value:=aUID;
        tblListPermsOfUser.Open;
      end;
    end;
  end else begin
    if tblUsers.Locate('ds_login;ds_password',VarArrayOf([user, md5pass]),[loCaseInsensitive]) and (tblUsersop_blocked.Value=false) then begin
      ValidUser:=true;
      aUID:=tblUsersid_user.Value;
      if LoginAction then begin
        FUserLogin:=tblUsersds_login.Value;
        FUserName :=tblUsersds_fullname.Value;

        tblListPermsOfUser.Close;
        tblListPermsOfUser.ParamByName('uid').Value:=aUID;
        tblListPermsOfUser.Open;
      end;
    end;
  end;
end;

procedure Tdm.CustomizedUserManagement1FailureLogin(Sender: TObject);
begin
  //dm.LogAction('Aplicação','Tela de login','Login','Tentativa de login mal sucedida');
end;

procedure Tdm.CustomizedUserManagement1GetUserLogin(
  var UserInfo: UTF8String);
begin
  UserInfo:=FUserLogin;
end;

procedure Tdm.CustomizedUserManagement1GetUserName(var UserInfo: UTF8String);
begin
  UserInfo:=FUserName;
end;

procedure Tdm.CustomizedUserManagement1CanAccess(securityCode: UTF8String;
  var CanAccess: Boolean);
begin
  if CustomizedUserManagement1.UID=1 then begin
    CanAccess:=true;
  end else
    CanAccess:=tblListPermsOfUser.Locate('ds_object',securityCode,[loCaseInsensitive]) and
               (tblListPermsOfUser.FieldByName('AccessAllowed').AsBoolean);
end;

procedure Tdm.CustomizedUserManagement1Logout(Sender: TObject);
begin
  FUserName :='';
  FUserLogin:='';

  tblListPermsOfUser.Close;
  tblListPermsOfUser.ParamByName('uid').Value:=-1;
  tblListPermsOfUser.Open;
end;

procedure Tdm.CustomizedUserManagement1ManageUsersAndGroups(Sender: TObject);
begin
  GetControlSecurityManager.RegisterSecurityCode(FSecCodeManageUsers);
  if GetControlSecurityManager.CanAccess(FSecCodeManageUsers) then begin
    if frmUM=nil then frmUM:=TfrmUserManagement.Create(Self);
    try
      frmUM.ShowModal;
    finally
      FreeAndNil(frmUM);
    end;
  end else begin
    raise exception.Create('Você não tem permissão suficiente para gerenciar usuários!');
  end;
end;

procedure Tdm.CustomizedUserManagement1RegisterSecurityCode(
  const securityCode: UTF8String);
begin
  if Trim(securityCode)='' then exit;

  if tblObjects.Locate('ds_object',securityCode,[loCaseInsensitive]) then exit;

  if tblObjects.State in dsEditModes then
    tblObjects.Cancel;

  tblObjects.Append;
  tblObjectsds_object.Value:=securityCode;
  tblObjects.Post;
end;

procedure Tdm.CustomizedUserManagement1SuccessfulLogin(Sender: TObject);
begin
  //dm.LogAction('Aplicação','Tela de login','Login','Login bem sucedido');
end;

procedure Tdm.CustomizedUserManagement1UIDCanAccess(aUID: Integer;
  securityCode: UTF8String; var CanAccess: Boolean);
var
  oldValue:Variant;
begin
  if aUID=1 then begin
    CanAccess:=true;
    exit;
  end;

  oldValue:=tblListPermsOfUser.ParamByName('uid').Value;
  try
    tblListPermsOfUser.Close;
    tblListPermsOfUser.ParamByName('uid').Value:=aUID;
    tblListPermsOfUser.Open;

    CanAccess:=tblListPermsOfUser.Locate('ds_object',securityCode,[loCaseInsensitive]) and
               (tblListPermsOfUser.FieldByName('AccessAllowed').AsBoolean);
  finally
    tblListPermsOfUser.Close;
    tblListPermsOfUser.ParamByName('uid').Value:=oldValue;
    tblListPermsOfUser.Open;
  end;
end;

procedure Tdm.CustomizedUserManagement1ValidadeSecurityCode(
  const securityCode: UTF8String);
begin
  if Trim(securityCode)='' then exit;

  CustomizedUserManagement1RegisterSecurityCode(securityCode);
end;

procedure Tdm.FormCreate(Sender: TObject);
begin
  PGConnUsers.Connect;
  tblGroups.Open;
  tblPermission.Open;
  tblObjects.Open;
  tblUsers.Open;
end;

procedure Tdm.CustomizedUserManagement1CheckUserChipCard(
  aChipCardCode: UTF8String; var userlogin: UTF8String; var UserID: Integer;
  var ValidChipCard: Boolean; LoginAction: Boolean);
begin
  ValidChipCard:=false;
  {$IFDEF ReadOnly}
  exit;
  {$ENDIF}

  tblUsers.Close;
  tblUsers.Open;

  if tblUsers.Locate('ds_automated_login_code',VarArrayOf([aChipCardCode]),[loCaseInsensitive]) and (tblUsersop_blocked.Value=false) then begin
    ValidChipCard:=true;
    UserID:=tblUsersid_user.Value;
    userlogin:=tblUsersds_login.Value;

    if LoginAction then begin
      FUserLogin:=tblUsersds_login.Value;
      FUserName :=tblUsersds_fullname.Value;

      tblListPermsOfUser.Close;
      tblListPermsOfUser.ParamByName('uid').Value:=UserID;
      tblListPermsOfUser.Open;
    end;
  end;
end;

end.

