{$I ../common/delphiver.inc}
unit WinCCUserManagement;

interface

uses
  Classes, sysutils, BasicUserManagement, windows, ExtCtrls;

type
  TPWRTLogin                   = function(monitor:AnsiChar)                                 :Boolean;  stdcall;
  TPWRTLogout                  = function()                                                 :Boolean;  stdcall;
  TPWRTGetCurrentUser          = function(Buffer:PAnsiChar; bufsize:Integer)                :Boolean;  stdcall;
  TPWRTGetLoginPriority        = function()                                                 :Cardinal; stdcall;
  TPWRTPermissionToString      = function(perm:Cardinal; permstr:PAnsiChar; bufsize:Integer):Boolean;  stdcall;
  TPWRTCheckPermission         = function(permlevel:Cardinal; suppress_messagebox:Cardinal) :Boolean;  stdcall;
  TPWRTCheckPermissionOnArea   = function(permlevel:Cardinal; area:PAnsiChar)               :Boolean;  stdcall;
  TPWRTCheckPermissionOnAreaID = function(permlevel:Cardinal; area:PAnsiChar)               :Boolean;  stdcall;
  TPWRTSilentLogin             = function(login:PAnsiChar; password:PAnsiChar)              :Boolean;  stdcall;

  TPermission = class(TObject)
  public
    AuthID:Integer;
  end;

  TAuthorization = class(TObject)
  public
    AuthorizationName:String;
    Valid:Boolean;
  end;

  TAuthorizations = array of TAuthorization;

  { TWinCCUserManagement }

  TWinCCUserManagement = class(TBasicUserManagement)
  private
    FCheckTimer                :TTimer;
    FInLoginProcess            :Boolean;
    procedure CheckAuthChanges(Sender:TObject);
  private
    PWRTLogin                  :TPWRTLogin;
    PWRTLogout                 :TPWRTLogout;
    PWRTGetCurrentUser         :TPWRTGetCurrentUser;
    PWRTGetLoginPriority       :TPWRTGetLoginPriority;
    PWRTPermissionToString     :TPWRTPermissionToString;
    PWRTCheckPermission        :TPWRTCheckPermission;
    PWRTCheckPermissionOnArea  :TPWRTCheckPermissionOnArea;
    PWRTCheckPermissionOnAreaID:TPWRTCheckPermissionOnAreaID;
    PWRTSilentLogin            :TPWRTSilentLogin;
    hUseAdmin:THANDLE;
    fUseAdminLoaded:Boolean;
    fAuthorizationCache:TStringList;
    procedure LoadUseAdmin;
  protected
    function CheckUserAndPassword(User, Pass: String): Boolean; override;
    function GetLoggedUser:Boolean; override;
    function GetCurrentUserLogin:String; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function Login: Boolean; override;
    procedure Logout; override;
    procedure Manage; override;

    procedure   ValidateSecurityCode(sc:String); override;
    function    SecurityCodeExists(sc:String):Boolean; override;
    procedure   RegisterSecurityCode(sc:String); override;
    procedure   UnregisterSecurityCode(sc:String); override;

    function    CanAccess(sc:String):Boolean; override;
    function    GetRegisteredAccessCodes:TStringList; override;
    procedure   ClearAuthorizationCache;
    function    CanAccessViaWinCCAuthCode(Code:Integer):Boolean;
  published
    property FailureLogin;
    property LoginRetries;
    property LoginFrozenTime;
    property SuccessfulLogin;
    property UserChanged;
  end;

implementation

uses ControlSecurityManager, hsstrings, StrUtils;

constructor TWinCCUserManagement.Create(AOwner: TComponent);
begin
  fUseAdminLoaded:=false;
  inherited Create(AOwner);

  if not fUseAdminLoaded then LoadUseAdmin;

  FCheckTimer:=TTimer.Create(Self);
  FCheckTimer.OnTimer :=CheckAuthChanges;
  FCheckTimer.Interval:=1000;
  FCheckTimer.Enabled:=false;
  fAuthorizationCache:=nil;
end;

procedure TWinCCUserManagement.AfterConstruction;
begin
  inherited AfterConstruction;
  FCheckTimer.Enabled:=true;
end;

destructor TWinCCUserManagement.Destroy;
begin
  //unload the library if it´s loaded
  if hUseAdmin<>0 then
    FreeLibrary(hUseAdmin);
  if FCheckTimer<>nil then
    FCheckTimer.Destroy;
  inherited Destroy;
end;

procedure TWinCCUserManagement.LoadUseAdmin;
begin
  hUseAdmin:=LoadLibrary('UseAdmin.dll');
  if hUseAdmin=0 then begin
    raise Exception.Create(SWinCCAreInstalled);
  end;

  //load UseAdmin functions...
  PWRTLogin                  :=TPWRTLogin(GetProcAddress(hUseAdmin,'PWRTLogin'));
  PWRTLogout                 :=TPWRTLogout(GetProcAddress(hUseAdmin,'PWRTLogout'));
  PWRTGetCurrentUser         :=TPWRTGetCurrentUser(GetProcAddress(hUseAdmin,'PWRTGetCurrentUser'));
  PWRTGetLoginPriority       :=TPWRTGetLoginPriority(GetProcAddress(hUseAdmin,'PWRTGetLoginPriority'));
  PWRTPermissionToString     :=TPWRTPermissionToString(GetProcAddress(hUseAdmin,'PWRTPermissionToString'));
  PWRTCheckPermission        :=TPWRTCheckPermission(GetProcAddress(hUseAdmin,'PWRTCheckPermission'));
  PWRTCheckPermissionOnArea  :=TPWRTCheckPermissionOnArea(GetProcAddress(hUseAdmin,'PWRTCheckPermissionOnArea'));
  PWRTCheckPermissionOnAreaID:=TPWRTCheckPermissionOnAreaID(GetProcAddress(hUseAdmin,'PWRTCheckPermissionOnAreaID'));
  PWRTSilentLogin            :=TPWRTSilentLogin(GetProcAddress(hUseAdmin,'PWRTSilentLogin'));

  fUseAdminLoaded:=true;
end;

procedure TWinCCUserManagement.CheckAuthChanges(Sender:TObject);
var
  culogin:String;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  culogin:=GetCurrentUserLogin;
  if culogin<>FCurrentUserLogin then begin
    GetControlSecurityManager.UpdateControls;
    try
      DoUserChanged;
    finally
      FCurrentUserLogin:=culogin;
    end;
  end;
end;

function TWinCCUserManagement.CheckUserAndPassword(User, Pass: String): Boolean;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  Result:=PWRTSilentLogin(PAnsiChar(AnsiString(User)),PAnsiChar(AnsiString(Pass))); //log into WinCC
end;

function TWinCCUserManagement.GetLoggedUser:Boolean;
begin
 Result := GetCurrentUserLogin<>'';
end;

function TWinCCUserManagement.GetCurrentUserLogin:String;
var
  buffer1:PAnsiChar;
  c:Integer;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  c:=PWRTGetLoginPriority(); //forces initialization...

  buffer1:=GetMemory(512);
  buffer1[0]:=#0;

  if PWRTGetCurrentUser(buffer1,510) then
    Result:=buffer1
  else
    Result:='';

  FreeMem(buffer1);
end;

function  TWinCCUserManagement.Login: Boolean;
begin
  if FInLoginProcess then exit;
  FInLoginProcess:=true;
  try
    Result := inherited Login;
  finally
    FInLoginProcess:=false;
  end;
end;

procedure TWinCCUserManagement.Logout;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  if PWRTLogout() then
    inherited Logout;
end;

procedure TWinCCUserManagement.Manage;
begin
  raise exception.Create(SUseTheWinCCUserManager);
end;

procedure   TWinCCUserManagement.ValidateSecurityCode(sc:String);
begin
  if not SecurityCodeExists(sc) then
    raise exception.Create(SUseTheWinCCUserManager);
end;

function    TWinCCUserManagement.CanAccess(sc:String):Boolean;
var
  p, p2, i:Cardinal;
  auth:TStringList;
  c:Integer;
begin
  Result:=false;

  if not fUseAdminLoaded then LoadUseAdmin;

  p:=PWRTGetLoginPriority(); //forces a initialization... I don´t know why...

  if not SecurityCodeExists(sc) then exit;

  auth:=GetRegisteredAccessCodes;

  i:=auth.IndexOf(sc);

  if (i>0) then
    p2:=Integer(Pointer(auth.Objects[i]))
  else
    p2:=0;

  auth.Destroy;

  Result:=PWRTCheckPermission(p2,1);
end;

function    TWinCCUserManagement.SecurityCodeExists(sc:String):Boolean;
var
  x:TStringList;
  c:Integer;
begin
  x:=GetRegisteredAccessCodes;
  Result:=x.IndexOf(sc)<>-1;
  x.Destroy;
end;

procedure   TWinCCUserManagement.RegisterSecurityCode(sc:String);
begin
  raise exception.Create(SUseTheWinCCUserManager);
end;

procedure   TWinCCUserManagement.UnregisterSecurityCode(sc:String);
begin
  //does nothing.
end;

function    TWinCCUserManagement.GetRegisteredAccessCodes:TStringList;
var
  buffer1:PAnsiChar;
  c:Integer;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  c:=PWRTGetLoginPriority(); //forces initialization...

  if fAuthorizationCache=nil then begin

    buffer1:=GetMemory(512);

    Result:=TStringList.Create;
    for c:=1 to 1100 do begin
      buffer1[0]:=#0;
      PWRTPermissionToString(c,buffer1,510);

      if strcomp(buffer1,'')<>0 then begin
        {$IFDEF DELPHI2009_UP}
        Result.AddObject((buffer1),TObject(Pointer(c)));
        {$ELSE}
        Result.AddObject(buffer1,TObject(Pointer(c)));
        {$ENDIF}
      end;
    end;
    fAuthorizationCache:=TStringList.Create;
    fAuthorizationCache.Assign(Result);
    FreeMem(buffer1);
  end else begin
    Result:=TStringList.Create;
    Result.Assign(fAuthorizationCache);
  end;
end;

procedure TWinCCUserManagement.ClearAuthorizationCache;
begin
  fAuthorizationCache.Destroy;
  fAuthorizationCache:=nil;
end;

function TWinCCUserManagement.CanAccessViaWinCCAuthCode(Code: Integer): Boolean;
begin
  Result := PWRTCheckPermission(code,0);
end;

end.
