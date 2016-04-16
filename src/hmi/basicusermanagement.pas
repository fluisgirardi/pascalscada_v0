unit BasicUserManagement;

interface

uses
  SysUtils, Classes, ExtCtrls, usrmgnt_login;

type
  TVKType = (vktNone, vktAlphaNumeric, vktNumeric);

  TUserChangedEvent = procedure(Sender:TObject; const OldUsername, NewUserName:UTF8String) of object;

  { TBasicUserManagement }

  TBasicUserManagement = class(TComponent)
  protected
{}  FLoggedUser:Boolean;
{}  FCurrentUserName,
{}  FCurrentUserLogin:UTF8String;
    FUID:Integer;
{}  FLoggedSince:TDateTime;
{}  FInactiveTimeOut:Cardinal;
{}  FLoginRetries:Cardinal;
    FFrozenTime:Cardinal;
    FVirtualKeyboardType:TVKType;

    FSuccessfulLogin:TNotifyEvent;
    FFailureLogin:TNotifyEvent;
    FUserChanged:TUserChangedEvent;

    FRegisteredSecurityCodes:TStringList;

    frmLogin, frmCheckIfUserIsAllowed:TpsHMIfrmUserAuthentication;

    function  GetLoginTime:TDateTime;
    procedure SetInactiveTimeOut(t:Cardinal);
    procedure UnfreezeLogin(Sender:TObject);
    function  GetUID: Integer;
  protected
    procedure DoUserChanged; virtual;

    procedure DoSuccessfulLogin; virtual;
    procedure DoFailureLogin; virtual;

    function CheckUserAndPassword(User, Pass:UTF8String; var UserID:Integer; LoginAction:Boolean):Boolean; virtual;

    function GetLoggedUser:Boolean; virtual;
    function GetCurrentUserName:UTF8String; virtual;
    function GetCurrentUserLogin:UTF8String; virtual;

    //read only properties.
    property LoggedSince:TDateTime read GetLoginTime;

    //read-write properties.
    //property VirtualKeyboardType:TVKType read FVirtualKeyboardType write FVirtualKeyboardType;
    property InactiveTimeout:Cardinal read FInactiveTimeOut write SetInactiveTimeOut;
    property LoginRetries:Cardinal read FLoginRetries write FLoginRetries;
    property LoginFrozenTime:Cardinal read  FFrozenTime write FFrozenTime;

    property SuccessfulLogin:TNotifyEvent read FSuccessfulLogin write FSuccessfulLogin;
    property FailureLogin:TNotifyEvent read FFailureLogin write FFailureLogin;
    property UserChanged:TUserChangedEvent read FUserChanged write FUserChanged;
    function CanAccess(sc:UTF8String; aUID:Integer):Boolean; virtual; overload;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    function    Login:Boolean; virtual; overload;
    function    Login(Userlogin, userpassword: UTF8String; var UID: Integer):Boolean; virtual;
    procedure   Logout; virtual;
    procedure   Manage; virtual;

    //Security codes management
    procedure   ValidateSecurityCode(sc:UTF8String); virtual;
    function    SecurityCodeExists(sc:UTF8String):Boolean; virtual;
    procedure   RegisterSecurityCode(sc:UTF8String); virtual;
    procedure   UnregisterSecurityCode(sc:UTF8String); virtual;

    function    CanAccess(sc:UTF8String):Boolean; virtual;
    function    GetRegisteredAccessCodes:TStringList; virtual;

    function    CheckIfUserIsAllowed(sc: UTF8String; RequireUserLogin: Boolean; var userlogin: UTF8String): Boolean; virtual;

    //read only properties.
    property UID:Integer read GetUID;
    property UserLogged:Boolean read GetLoggedUser;
    property CurrentUserName:UTF8String read GetCurrentUserName;
    property CurrentUserLogin:UTF8String read GetCurrentUserLogin;
  end;

implementation

uses Controls, Forms, ControlSecurityManager, hsstrings;

constructor TBasicUserManagement.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);

  if GetControlSecurityManager.UserManagement=nil then
    GetControlSecurityManager.UserManagement:=Self
  else
    raise Exception.Create(SUserManagementIsSet);

  FLoggedUser:=false;
  FCurrentUserName:='';
  FCurrentUserLogin:='';
  FUID:=-1;
  FLoggedSince:=Now;

  FRegisteredSecurityCodes:=TStringList.Create;
end;

destructor  TBasicUserManagement.Destroy;
begin
  if GetControlSecurityManager.UserManagement=Self then
    GetControlSecurityManager.UserManagement:=nil;

  if FRegisteredSecurityCodes<>nil then
    FRegisteredSecurityCodes.Destroy;
  inherited Destroy;
end;

function    TBasicUserManagement.Login:Boolean;
var
  frozenTimer:TTimer;
  retries:LongInt;
  aborted, loggedin:Boolean;
  aUserID: Integer;
begin
  if Assigned(frmLogin) then begin
    frmLogin.ShowOnTop;
    exit;
  end;

  frozenTimer:=TTimer.Create(nil);
  frozenTimer.Enabled:=false;
  frozenTimer.Interval:=LoginFrozenTime;
  frozenTimer.Tag:=1; //login
  frozenTimer.OnTimer:=@UnfreezeLogin;
  frmLogin:=TpsHMIfrmUserAuthentication.Create(nil);
  {$IFDEF FPC}
  frmLogin.FormStyle:=fsSystemStayOnTop;
  {$ELSE}
  frmLogin.FormStyle:=fsStayOnTop;
  {$ENDIF}
  retries:=0;
  aborted:=false;
  loggedin:=False;
  Result:=false;
  try
    frmLogin.edtusername.Text:='';
    frmLogin.FocusControl:=fcUserName;
    while (not loggedin) and (not aborted) do begin
      frmLogin.edtPassword.Text:='';
      if frmLogin.ShowModal=mrOk then begin
        if CheckUserAndPassword(frmLogin.edtusername.Text, frmLogin.edtPassword.Text, aUserID, true) then begin
          FLoggedUser:=true;
          FUID:=aUserID;
          loggedin:=true;
          FCurrentUserLogin:=frmLogin.edtusername.Text;
          FLoggedSince:=Now;
          Result:=true;
          GetControlSecurityManager.UpdateControls;
          DoSuccessfulLogin;
        end else begin
          DoFailureLogin;
          inc(retries);
          if (FLoginRetries>0) and (retries>=FLoginRetries) then begin
            frmLogin.DisableEntry;
            frozenTimer.Enabled:=true;
            frmLogin.ShowModal;
            retries:=0;
          end;
        end;
      end else
        aborted:=true;
      frmLogin.FocusControl:=fcPassword;
    end;
  finally
    FreeAndNil(frmLogin);
    FreeAndNil(frozenTimer);
  end;
end;

function TBasicUserManagement.Login(Userlogin, userpassword: UTF8String; var UID: Integer): Boolean;
begin
  Result:=CheckUserAndPassword(Userlogin, userpassword, UID, true);
  if Result then begin
    FLoggedUser:=true;
    FUID:=UID;
    FCurrentUserLogin:=Userlogin;
    FLoggedSince:=Now;
    Result:=true;
    GetControlSecurityManager.UpdateControls;
    DoSuccessfulLogin;
  end;
end;

procedure   TBasicUserManagement.Logout;
begin
  FLoggedUser:=false;
  FCurrentUserName:='';
  FCurrentUserLogin:='';
  FUID:=-1;
  FLoggedSince:=Now;
  GetControlSecurityManager.UpdateControls;
end;

procedure   TBasicUserManagement.Manage;
begin
  //has nothing to do here!
end;

procedure TBasicUserManagement.ValidateSecurityCode(sc: UTF8String);
begin
  //raise a exception if the security code is invalid.
end;

function TBasicUserManagement.SecurityCodeExists(sc: UTF8String): Boolean;
begin
  Result:=FRegisteredSecurityCodes.IndexOf(sc)>=0;
end;

procedure TBasicUserManagement.RegisterSecurityCode(sc: UTF8String);
begin
  if Not SecurityCodeExists(sc) then
    FRegisteredSecurityCodes.Add(sc);
end;

procedure TBasicUserManagement.UnregisterSecurityCode(sc: UTF8String);
begin
  if SecurityCodeExists(sc) then
    FRegisteredSecurityCodes.Delete(FRegisteredSecurityCodes.IndexOf(sc));
end;

function TBasicUserManagement.CanAccess(sc: UTF8String): Boolean;
begin
  Result:=false;
end;

function    TBasicUserManagement.GetRegisteredAccessCodes:TStringList;
begin
  Result:=TStringList.Create;
  Result.Assign(FRegisteredSecurityCodes);
end;

function TBasicUserManagement.CheckIfUserIsAllowed(sc: UTF8String;
  RequireUserLogin: Boolean; var userlogin: UTF8String): Boolean;
var
  frozenTimer:TTimer;
  aUserID:Integer;

begin
  Result:=false;

  //se o usuário logado tem permissão, evita
  //abrir o dialog que irá solicitar a permissão
  //de outro usuário.
  if UserLogged and CanAccess(sc) and (RequireUserLogin=false) then begin
    userlogin:=GetCurrentUserLogin;
    Result:=true;
    exit;
  end;

  //se existe um dialogo de permissão especial aberto
  //chama ele...
  if Assigned(frmCheckIfUserIsAllowed) then begin
    frmCheckIfUserIsAllowed.ShowOnTop;
    exit;
  end;

  frozenTimer:=TTimer.Create(nil);
  frozenTimer.Enabled:=false;
  frozenTimer.Interval:=LoginFrozenTime;
  frozenTimer.Tag:=2; //Check
  frozenTimer.OnTimer:=@UnfreezeLogin;
  frmCheckIfUserIsAllowed:=TpsHMIfrmUserAuthentication.Create(nil);
  {$IFDEF FPC}
  frmCheckIfUserIsAllowed.FormStyle:=fsSystemStayOnTop;
  {$ELSE}
  frmCheckIfUserIsAllowed.FormStyle:=fsStayOnTop;
  {$ENDIF}

  frmCheckIfUserIsAllowed.Caption:=Format(SLoginCaptionToken, [sc]);
  Result:=false;
  try
    frmCheckIfUserIsAllowed.FocusControl:=fcUserName;
    frmCheckIfUserIsAllowed.edtusername.Text:='';
    frmCheckIfUserIsAllowed.edtPassword.Text:='';
    if frmCheckIfUserIsAllowed.ShowModal=mrOk then begin
      if CheckUserAndPassword(frmCheckIfUserIsAllowed.edtusername.Text, frmCheckIfUserIsAllowed.edtPassword.Text, aUserID, false) then begin
        if CanAccess(sc,aUserID) then begin
          Result:=true;
          userlogin:=frmCheckIfUserIsAllowed.edtusername.Text;
        end else
          Result:=false;
      end;
    end;
  finally
    FreeAndNil(frmCheckIfUserIsAllowed);
    FreeAndNil(frozenTimer);
  end;
end;

function TBasicUserManagement.GetUID: Integer;
begin
  Result:=FUID;
end;

function    TBasicUserManagement.GetLoginTime:TDateTime;
begin
  if FLoggedUser then
    Result:=FLoggedSince
  else
    Result:=Now;
end;

procedure   TBasicUserManagement.SetInactiveTimeOut(t:Cardinal);
begin
  //
end;

function TBasicUserManagement.CheckUserAndPassword(User, Pass: UTF8String;
  var UserID: Integer; LoginAction: Boolean): Boolean;
begin
  Result:=false;
end;

function TBasicUserManagement.GetLoggedUser:Boolean;
begin
  Result:=FLoggedUser;
end;

function TBasicUserManagement.GetCurrentUserName: UTF8String;
begin
  Result:=FCurrentUserName;
end;

function TBasicUserManagement.GetCurrentUserLogin: UTF8String;
begin
  Result:=FCurrentUserLogin;
end;

function TBasicUserManagement.CanAccess(sc: UTF8String; aUID: Integer): Boolean;
begin
  Result:=false;
end;

procedure TBasicUserManagement.DoSuccessfulLogin;
begin
  if Assigned(FSuccessfulLogin) then
    FSuccessfulLogin(Self);
end;

procedure TBasicUserManagement.DoFailureLogin;
begin
  if Assigned(FFailureLogin) then
    FFailureLogin(Self);
end;

procedure TBasicUserManagement.UnfreezeLogin(Sender:TObject);
begin
  if sender is TTimer then begin
    TTimer(sender).Enabled:=false;
    case TTimer(sender).Tag of
      1: if Assigned(frmLogin) then begin
           frmLogin.Close;
           frmLogin.EnableEntry;
         end;
      2: if Assigned(frmCheckIfUserIsAllowed) then begin
          frmCheckIfUserIsAllowed.Close;
          frmCheckIfUserIsAllowed.EnableEntry;
         end;
    end;
  end;
end;

procedure TBasicUserManagement.DoUserChanged;
begin
  if Assigned(FUserChanged) then
    try
      FUserChanged(Self, FCurrentUserLogin, GetCurrentUserLogin);
    finally
    end;
end;

end.
 
