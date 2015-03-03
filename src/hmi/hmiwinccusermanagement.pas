{$I ../common/delphiver.inc}
unit HMIWinCCUserManagement;

interface

uses
  Classes, sysutils, HMIBasicUserManagement, windows, ExtCtrls;

type
  THMIWCCPWRTLogin                   = function(monitor:AnsiChar)                                 :Boolean;  stdcall;
  THMIWCCPWRTLogout                  = function()                                                 :Boolean;  stdcall;
  THMIWCCPWRTGetCurrentUser          = function(Buffer:PAnsiChar; bufsize:LongInt)                :Boolean;  stdcall;
  THMIWCCPWRTGetLoginPriority        = function()                                                 :Cardinal; stdcall;
  THMIWCCPWRTPermissionToString      = function(perm:Cardinal; permstr:PAnsiChar; bufsize:LongInt):Boolean;  stdcall;
  THMIWCCPWRTCheckPermission         = function(permlevel:Cardinal; suppress_messagebox:Cardinal) :Boolean;  stdcall;
  THMIWCCPWRTCheckPermissionOnArea   = function(permlevel:Cardinal; area:PAnsiChar)               :Boolean;  stdcall;
  THMIWCCPWRTCheckPermissionOnAreaID = function(permlevel:Cardinal; area:PAnsiChar)               :Boolean;  stdcall;
  THMIWCCPWRTSilentLogin             = function(login:PAnsiChar; password:PAnsiChar)              :Boolean;  stdcall;

  THMIWCCPermission = class(TObject)
  public
    AuthID:LongInt;
  end;

  THMIWCCAuthorization = class(TObject)
  public
    AuthorizationName:String;
    Valid:Boolean;
  end;

  THMIAuthorizations = array of TAuthorization;

  { TWinCCUserManagement }

  THMIWinCCUserManagement = class(THMIBasicUserManagement)
  private
    FCheckTimer                :TTimer;
    FInLoginProcess            :Boolean;
    FAuthorizationList         :TStrings;
    procedure CheckAuthChanges(Sender:TObject);
  private
    PWRTLogin                  :THMIWCCPWRTLogin;
    PWRTLogout                 :THMIWCCPWRTLogout;
    PWRTGetCurrentUser         :THMIWCCPWRTGetCurrentUser;
    PWRTGetLoginPriority       :THMIWCCPWRTGetLoginPriority;
    PWRTPermissionToString     :THMIWCCPWRTPermissionToString;
    PWRTCheckPermission        :THMIWCCPWRTCheckPermission;
    PWRTCheckPermissionOnArea  :THMIWCCPWRTCheckPermissionOnArea;
    PWRTCheckPermissionOnAreaID:THMIWCCPWRTCheckPermissionOnAreaID;
    PWRTSilentLogin            :THMIWCCPWRTSilentLogin;
    hUseAdmin:THANDLE;
    fUseAdminLoaded:Boolean;
    fAuthorizationCache:TStringList;
    procedure LoadUseAdmin;
    procedure SetAuthorizationList(AValue: TStrings);
  protected
    function CheckUserAndPassword(User, Pass: String; var UID:Integer; LoginAction:Boolean): Boolean; override;
    function GetLoggedUser:Boolean; override;
    function GetCurrentUserLogin:String; override;
    procedure Loaded; override;
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
    function    CanAccessViaWinCCAuthCode(Code:LongInt):Boolean;
    function    CheckIfUserIsAllowed(sc: String; RequireUserLogin: Boolean; var userlogin: String): Boolean; override;
  published
    property FailureLogin;
    property LoginRetries;
    property LoginFrozenTime;
    property SuccessfulLogin;
    property UserChanged;
    property AuthorizationList:TStrings read FAuthorizationList write SetAuthorizationList stored true;
  end;

implementation

uses HMIControlSecurityManager, hsstrings, StrUtils, StdCtrls
     {$IFDEF FPC}
     , TextStrings
     {$ELSE}
     , hmitextstrings
     {$ENDIF};

constructor THMIWinCCUserManagement.Create(AOwner: TComponent);
begin
  fUseAdminLoaded:=false;
  inherited Create(AOwner);

  if not fUseAdminLoaded then LoadUseAdmin;

  FCheckTimer:=TTimer.Create(Self);
  FCheckTimer.OnTimer :=CheckAuthChanges;
  FCheckTimer.Interval:=1000;
  FCheckTimer.Enabled:=false;
  FAuthorizationList:=TTextStrings.Create;
  fAuthorizationCache:=nil;
end;

procedure THMIWinCCUserManagement.AfterConstruction;
begin
  inherited AfterConstruction;
  FCheckTimer.Enabled:=true;
end;

destructor THMIWinCCUserManagement.Destroy;
begin
  //unload the library if it´s loaded
  if hUseAdmin<>0 then
    FreeLibrary(hUseAdmin);
  FreeAndNil(FCheckTimer);
  FreeAndNil(FAuthorizationList);
  FreeAndNil(fAuthorizationCache);
  inherited Destroy;
end;

procedure THMIWinCCUserManagement.LoadUseAdmin;
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

procedure THMIWinCCUserManagement.SetAuthorizationList(AValue: TStrings);
var
  l, p, AuthNumber: LongInt;
  newAuthorizationCache:TStringList;
  ValidFormat: Boolean;
  strNum: String;
  strAuthName: String;
begin
  newAuthorizationCache:=TStringList.Create;
  try
    ValidFormat:=true;
    if AValue=nil then exit;
    for l:=0 to AValue.Count-1 do begin
      p:=Pos(':',AValue[l]);
      if p>0 then begin
        strNum:=LeftStr(AValue[l],p-1);
        strAuthName:=RightStr(AValue[l],Length(AValue[l])-p);
        if TryStrToInt(Trim(strNum), AuthNumber) then begin
          {$IFDEF DELPHI2009_UP}
          newAuthorizationCache.AddObject(strAuthName,TObject(Pointer(AuthNumber)));
          {$ELSE}
          newAuthorizationCache.AddObject(strAuthName,TObject(Pointer(PtrUInt(AuthNumber))));
          {$ENDIF}
        end else begin
          ValidFormat:=False;
          break;
        end;
      end else begin
        ValidFormat:=False;
        break;
      end;
    end;
  finally
    if ValidFormat then begin
      if fAuthorizationCache<>nil then
        fAuthorizationCache.Destroy;
      fAuthorizationCache:=newAuthorizationCache;
    end else begin
      newAuthorizationCache.Destroy;
      raise exception.Create('WinCCUserManagement: Invalid authorization list format at line '+IntToStr(l+1));
    end;
    if AValue<>nil then
      FAuthorizationList.Assign(AValue);
  end;
end;

procedure THMIWinCCUserManagement.CheckAuthChanges(Sender:TObject);
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

function THMIWinCCUserManagement.CheckUserAndPassword(User, Pass: String;
  var UID: Integer; LoginAction: Boolean): Boolean;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  Result:=PWRTSilentLogin(PAnsiChar(AnsiString(User)),PAnsiChar(AnsiString(Pass))); //log into WinCC
  if Result then
    UID:=1
  else
    UID:=-1;
end;

function THMIWinCCUserManagement.GetLoggedUser:Boolean;
begin
 Result := GetCurrentUserLogin<>'';
end;

function THMIWinCCUserManagement.GetCurrentUserLogin:String;
var
  buffer1:PAnsiChar;
  c:LongInt;
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

procedure THMIWinCCUserManagement.Loaded;
begin
  inherited Loaded;
  if FAuthorizationList.Count>0 then
    SetAuthorizationList(FAuthorizationList);
end;

function  THMIWinCCUserManagement.Login: Boolean;
begin
  if FInLoginProcess then exit;
  FInLoginProcess:=true;
  try
    Result := inherited Login;
  finally
    FInLoginProcess:=false;
  end;
end;

procedure THMIWinCCUserManagement.Logout;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  if PWRTLogout() then
    inherited Logout;
end;

procedure THMIWinCCUserManagement.Manage;
begin
  raise exception.Create(SUseTheWinCCUserManager);
end;

procedure   THMIWinCCUserManagement.ValidateSecurityCode(sc:String);
begin
  if not SecurityCodeExists(sc) then
    raise exception.Create(SUseTheWinCCUserManager);
end;

function    THMIWinCCUserManagement.CanAccess(sc:String):Boolean;
var
  p, p2, i:Cardinal;
  auth:TStringList;
  c:LongInt;
begin
  Result:=false;

  if not fUseAdminLoaded then LoadUseAdmin;

  p:=PWRTGetLoginPriority(); //forces a initialization... I don´t know why...

  if not SecurityCodeExists(sc) then exit;

  auth:=GetRegisteredAccessCodes;

  i:=auth.IndexOf(sc);

  if (i>0) then
    p2:=LongInt(Pointer(auth.Objects[i]))
  else
    p2:=0;

  auth.Destroy;

  Result:=PWRTCheckPermission(p2,1);
end;

function    THMIWinCCUserManagement.SecurityCodeExists(sc:String):Boolean;
var
  x:TStringList;
  c:LongInt;
begin
  x:=GetRegisteredAccessCodes;
  Result:=x.IndexOf(sc)<>-1;
  x.Destroy;
end;

procedure   THMIWinCCUserManagement.RegisterSecurityCode(sc:String);
begin
  raise exception.Create(SUseTheWinCCUserManager);
end;

procedure   THMIWinCCUserManagement.UnregisterSecurityCode(sc:String);
begin
  //does nothing.
end;

function    THMIWinCCUserManagement.GetRegisteredAccessCodes:TStringList;
var
  buffer1:PAnsiChar;
  c:LongInt;
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
        Result.AddObject(buffer1,TObject(Pointer(PtrUInt(c))));
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

procedure THMIWinCCUserManagement.ClearAuthorizationCache;
begin
  fAuthorizationCache.Destroy;
  fAuthorizationCache:=nil;
end;

function THMIWinCCUserManagement.CanAccessViaWinCCAuthCode(Code: LongInt): Boolean;
begin
  Result := PWRTCheckPermission(code,0);
end;

function THMIWinCCUserManagement.CheckIfUserIsAllowed(sc: String;
  RequireUserLogin: Boolean; var userlogin: String): Boolean;
begin
  raise exception.Create(SWCCNotSupportCheckUserAuth);
end;

end.
