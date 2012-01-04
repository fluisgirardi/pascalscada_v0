unit BasicUserManagement;

interface

uses
  SysUtils, Classes, ExtCtrls, usrmgnt_login;

type
  TVKType = (vktNone, vktAlphaNumeric, vktNumeric);

  TBasicUserManagement = class(TComponent)
  protected
{}  FLoggedUser:Boolean;
{}  FCurrentUserName,
{}  FCurrentUserLogin:String;
{}  FLoggedSince:TDateTime;
{}  FInactiveTimeOut:Cardinal;
{}  FLoginRetries:Cardinal;
    FFrozenTime:Cardinal;
    FVirtualKeyboardType:TVKType;

    FSuccessfulLogin:TNotifyEvent;
    FFailureLogin:TNotifyEvent;

    FRegisteredSecurityCodes:TStringList;

    frmLogin:TfrmUserAuthentication;

    function  GetLoginTime:TDateTime;
    procedure SetInactiveTimeOut(t:Cardinal);
    procedure UnfreezeLogin(Sender:TObject);
  protected

    procedure DoSuccessfulLogin; virtual;
    procedure DoFailureLogin; virtual;

    function CheckUserAndPassword(User, Pass:String):Boolean; virtual;

    function GetLoggedUser:Boolean; virtual;
    function GetCurrentUserName:String; virtual;
    function GetCurrentUserLogin:String; virtual;

    //read only properties.

    property UserLogged:Boolean read GetLoggedUser;
    property CurrentUserName:String read GetCurrentUserName;
    property CurrentUserLogin:String read GetCurrentUserLogin;
    property LoggedSince:TDateTime read GetLoginTime;

    //read-write properties.
    //property VirtualKeyboardType:TVKType read FVirtualKeyboardType write FVirtualKeyboardType;
    property InactiveTimeout:Cardinal read FInactiveTimeOut write SetInactiveTimeOut;
    property LoginRetries:Cardinal read FLoginRetries write FLoginRetries;
    property LoginFrozenTime:Cardinal read  FFrozenTime write FFrozenTime;

    property SuccessfulLogin:TNotifyEvent read FSuccessfulLogin write FSuccessfulLogin;
    property FailureLogin:TNotifyEvent read FFailureLogin write FFailureLogin;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    function    Login:Boolean; virtual;
    procedure   Logout; virtual;

    //Security codes management
    procedure   ValidateSecurityCode(sc:String); virtual;
    function    SecurityCodeExists(sc:String):Boolean; virtual;
    procedure   RegisterSecurityCode(sc:String); virtual;
    procedure   UnregisterSecurityCode(sc:String); virtual;

    function    CanAccess(sc:String):Boolean; virtual;
    function    GetRegisteredAccessCodes:TStringList; virtual;
  end;

implementation

uses Controls, ControlSecurityManager, hsstrings;

constructor TBasicUserManagement.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);

  if GetControlSecurityManager.UserManagement=nil then
    GetControlSecurityManager.UserManagement:=Self
  else
    raise Exception.Create(SUserManagementIsSet);

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
  retries:Integer;
  aborted, loggedin:Boolean;
begin
  frozenTimer:=TTimer.Create(nil);
  frozenTimer.OnTimer:=UnfreezeLogin;
  frmLogin:=TfrmUserAuthentication.Create(nil);
  retries:=0;
  aborted:=false;
  loggedin:=False;
  Result:=false;
  try
    while (not loggedin) and (not aborted) do begin
      frmLogin.edtusername.Text:='';
      frmLogin.edtPassword.Text:='';
      if frmLogin.ShowModal=mrOk then begin
        if CheckUserAndPassword(frmLogin.edtusername.Text, frmLogin.edtPassword.Text) then begin
          FLoggedUser:=true;
          loggedin:=true;
          FCurrentUserLogin:=frmLogin.edtusername.Text;
          FLoggedSince:=Now;
          Result:=true;
          DoSuccessfulLogin;
        end else begin
          DoFailureLogin;
          inc(retries);
          if (FLoginRetries>0) and (retries=FLoginRetries) then begin
            frmLogin.Enabled:=false;
            frozenTimer.Enabled:=true;
          end;
        end;
      end else
        aborted:=true;
    end;
  finally
    frmLogin.Destroy;
    frozenTimer.Destroy;
  end;
end;

procedure   TBasicUserManagement.Logout;
begin
  FLoggedUser:=false;
  FCurrentUserName:='';
  FCurrentUserLogin:='';
  FLoggedSince:=Now;
end;

procedure   TBasicUserManagement.ValidateSecurityCode(sc:String);
begin
  //raise a exception if the security code is invalid.
end;

function    TBasicUserManagement.SecurityCodeExists(sc:String):Boolean;
begin
  Result:=FRegisteredSecurityCodes.IndexOf(sc)>=0;
end;

procedure   TBasicUserManagement.RegisterSecurityCode(sc:String);
begin
  if Not SecurityCodeExists(sc) then
    FRegisteredSecurityCodes.Add(sc);
end;

procedure   TBasicUserManagement.UnregisterSecurityCode(sc:String);
begin
  if SecurityCodeExists(sc) then
    FRegisteredSecurityCodes.Delete(FRegisteredSecurityCodes.IndexOf(sc));
end;

function    TBasicUserManagement.CanAccess(sc:String):Boolean;
begin
  Result:=false;
end;

function    TBasicUserManagement.GetRegisteredAccessCodes:TStringList;
begin
  Result:=TStringList.Create;
  Result.Assign(FRegisteredSecurityCodes);
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

function    TBasicUserManagement.CheckUserAndPassword(User, Pass:String):Boolean;
begin
  Result:=false;
end;

function TBasicUserManagement.GetLoggedUser:Boolean;
begin
  Result:=FLoggedUser;
end;

function TBasicUserManagement.GetCurrentUserName:String;
begin
  Result:=FCurrentUserName;
end;

function TBasicUserManagement.GetCurrentUserLogin:String;
begin
  Result:=FCurrentUserLogin;
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
  if sender is TTimer then
    TTimer(sender).Enabled:=false;
  if frmLogin<>nil then
    frmLogin.Enabled:=true;
end;

end.
 
