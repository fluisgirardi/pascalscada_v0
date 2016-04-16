unit CustomizedUserManagement;

interface

uses
  Classes, BasicUserManagement;

type
  TCheckUserAndPasswordEvent = procedure(user, pass:UTF8String; var aUID:Integer; var ValidUser:Boolean; LoginAction:Boolean) of object;
  TUserStillLoggedEvent      = procedure(var StillLogged:Boolean) of object;
  TGetUserNameAndLogin       = procedure(var UserInfo:UTF8String) of object;
  TManageUsersAndGroupsEvent = TNotifyEvent;
  TValidadeSecurityCode      = procedure(const securityCode:UTF8String) of object;
  TRegisterSecurityCode      = procedure(const securityCode:UTF8String) of object;
  TLogoutEvent               = TNotifyEvent;
  TCanAccessEvent            = procedure(securityCode:UTF8String; var CanAccess:Boolean) of object;
  TUIDCanAccessEvent         = procedure(aUID:Integer; securityCode:UTF8String; var CanAccess:Boolean) of object;

  { TCustomizedUserManagement }

  TCustomizedUserManagement = class(TBasicUserManagement)
  private
    FCheckUserAndPasswordEvent:TCheckUserAndPasswordEvent;
    FGetUserName              :TGetUserNameAndLogin;
    FGetUserLogin             :TGetUserNameAndLogin;
    FManageUsersAndGroupsEvent:TManageUsersAndGroupsEvent;
    FRegisterSecurityCode     :TRegisterSecurityCode;
    FUIDCanAccessEvent        :TUIDCanAccessEvent;
    FValidadeSecurityCode     :TValidadeSecurityCode;
    FCanAccessEvent           :TCanAccessEvent;
    FLogoutEvent              :TLogoutEvent;
  protected
    function  CheckUserAndPassword(User, Pass:UTF8String; var UserID:Integer; LoginAction:Boolean):Boolean; override;

    function  GetCurrentUserName:UTF8String; override;
    function  GetCurrentUserLogin:UTF8String; override;
    function CanAccess(sc: UTF8String; aUID: Integer): Boolean; override; overload;
  public
    procedure Logout; override;
    procedure Manage; override;

    //Security codes management
    procedure ValidateSecurityCode(sc:UTF8String); override;
    procedure RegisterSecurityCode(sc: UTF8String); override;

    function  CanAccess(sc:UTF8String):Boolean; override;
  published
    property UID;
    property CurrentUserName;
    property CurrentUserLogin;
    property LoggedSince;

    property LoginRetries;
    property LoginFrozenTime;

    property SuccessfulLogin;
    property FailureLogin;
  published
    property OnCheckUserAndPass    :TCheckUserAndPasswordEvent read FCheckUserAndPasswordEvent write FCheckUserAndPasswordEvent;
    property OnGetUserName         :TGetUserNameAndLogin       read FGetUserName               write FGetUserName;
    property OnGetUserLogin        :TGetUserNameAndLogin       read FGetUserLogin              write FGetUserLogin;
    property OnManageUsersAndGroups:TManageUsersAndGroupsEvent read FManageUsersAndGroupsEvent write FManageUsersAndGroupsEvent;
    property OnValidadeSecurityCode:TValidadeSecurityCode      read FValidadeSecurityCode      write FValidadeSecurityCode;
    property OnRegisterSecurityCode:TRegisterSecurityCode      read FRegisterSecurityCode      write FRegisterSecurityCode;
    property OnCanAccess           :TCanAccessEvent            read FCanAccessEvent            write FCanAccessEvent;
    property OnUIDCanAccess        :TUIDCanAccessEvent         read FUIDCanAccessEvent         write FUIDCanAccessEvent;
    property OnLogout              :TLogoutEvent               read FLogoutEvent               write FLogoutEvent;
  end;

implementation

uses sysutils;

function TCustomizedUserManagement.CheckUserAndPassword(User, Pass: UTF8String;
  var UserID: Integer; LoginAction: Boolean): Boolean;
begin
  Result:=false;
  try
    if Assigned(FCheckUserAndPasswordEvent) then
      FCheckUserAndPasswordEvent(user,Pass,UserID,Result,LoginAction);
  except
    Result:=false;
  end;
end;

function TCustomizedUserManagement.GetCurrentUserName: UTF8String;
begin
  Result:='';
  if FLoggedUser then
    try
      if Assigned(FGetUserName) then
        FGetUserName(Result);
    except
      Result:='';
    end;
end;

function TCustomizedUserManagement.GetCurrentUserLogin: UTF8String;
begin
  Result:='';
  if FLoggedUser then
    try
      if Assigned(FGetUserLogin) then
        FGetUserLogin(Result);
    except
      Result:='';
    end;
end;

function TCustomizedUserManagement.CanAccess(sc: UTF8String; aUID: Integer
  ): Boolean;
begin
  Result:=(Trim(sc)='');
  if aUID>=0 then
    try
      if Assigned(FUIDCanAccessEvent) then
        FUIDCanAccessEvent(aUID,sc,Result);
    except
      Result:=(Trim(sc)='');
    end;
end;

procedure TCustomizedUserManagement.Logout;
begin
  inherited Logout;
  if Assigned(FLogoutEvent) then
    try
      FLogoutEvent(self);
    except
    end;
end;

procedure TCustomizedUserManagement.Manage;
begin
  if Assigned(FManageUsersAndGroupsEvent) then
    FManageUsersAndGroupsEvent(Self);
end;

procedure TCustomizedUserManagement.ValidateSecurityCode(sc: UTF8String);
begin
  if Assigned(FValidadeSecurityCode) then
    FValidadeSecurityCode(sc);
end;

procedure TCustomizedUserManagement.RegisterSecurityCode(sc: UTF8String);
begin
  if Assigned(FRegisterSecurityCode) then
    FRegisterSecurityCode(sc);
end;

function TCustomizedUserManagement.CanAccess(sc: UTF8String): Boolean;
begin
  Result:=(Trim(sc)='');
  if FLoggedUser then
    try
      if Assigned(FCanAccessEvent) then
        FCanAccessEvent(sc,Result);
    except
      Result:=(Trim(sc)='');
    end;
end;

end.
