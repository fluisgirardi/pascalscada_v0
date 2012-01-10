unit CustomizedUserManagement;

interface

uses
  Classes, BasicUserManagement;

type
  TCheckUserAndPasswordEvent = procedure(user, pass:String; var ValidUser:Boolean) of object;
  TUserStillLoggedEvent      = procedure(var StillLogged:Boolean) of object;
  TGetUserNameAndLogin       = procedure(var UserInfo:String) of object;
  TManageUsersAndGroupsEvent = TNotifyEvent;
  TValidadeSecurityCode      = TNotifyEvent;
  TCanAccessEvent            = procedure(securityCode:String; var CanAccess:Boolean) of object;

  TCustomizedUserManagement = class(TBasicUserManagement)
  private
    FCheckUserAndPasswordEvent:TCheckUserAndPasswordEvent;
    FGetUserName              :TGetUserNameAndLogin;
    FGetUserLogin             :TGetUserNameAndLogin;
    FManageUsersAndGroupsEvent:TManageUsersAndGroupsEvent;
    FValidadeSecurityCode     :TValidadeSecurityCode;
    FCanAccessEvent           :TCanAccessEvent;
  protected
    function CheckUserAndPassword(User, Pass:String):Boolean; override;

    function GetCurrentUserName:String; override;
    function GetCurrentUserLogin:String; override;
  public
    procedure   Manage; override;

    //Security codes management
    procedure   ValidateSecurityCode(sc:String); override;

    function    CanAccess(sc:String):Boolean; override;
  published
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
    property OnCanAccess           :TCanAccessEvent            read FCanAccessEvent            write FCanAccessEvent;
  end;

implementation

uses sysutils;

function  TCustomizedUserManagement.CheckUserAndPassword(User, Pass:String):Boolean;
begin
  Result:=false;
  try
    if Assigned(FCheckUserAndPasswordEvent) then
      FCheckUserAndPasswordEvent(user,Pass,Result);
  except
    Result:=false;
  end;
end;

function  TCustomizedUserManagement.GetCurrentUserName:String;
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

function  TCustomizedUserManagement.GetCurrentUserLogin:String;
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

procedure TCustomizedUserManagement.Manage;
begin
  if Assigned(FManageUsersAndGroupsEvent) then
    FManageUsersAndGroupsEvent(Self);
end;

procedure TCustomizedUserManagement.ValidateSecurityCode(sc:String);
begin
  if Assigned(FValidadeSecurityCode) then
    FValidadeSecurityCode(Self);
end;

function  TCustomizedUserManagement.CanAccess(sc:String):Boolean;
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
