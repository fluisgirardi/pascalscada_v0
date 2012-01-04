unit WinCCUserManagement;

interface

uses
  Classes, sysutils, BasicUserManagement, dynlibs, ExtCtrls;

type
  TPWRTLogin                   = function (monitor:char)                                    :Boolean;  stdcall;
  TPWRTLogout                  = function ()                                                :Boolean;  stdcall;
  TPWRTGetCurrentUser          = function (Buffer:PChar; bufsize:Integer)                   :Boolean;  stdcall;
  TPWRTGetLoginPriority        = function ()                                                :Cardinal; stdcall;
  TPWRTPermissionToString      = function (perm:Cardinal; permstr:PChar; bufsize:Integer)   :Boolean;  stdcall;
  TPWRTCheckPermission         = function (permlevel:Cardinal; suppress_messagebox:Cardinal):Boolean;  stdcall;
  TPWRTCheckPermissionOnArea   = function (permlevel:Cardinal; area:PChar)                  :Boolean;  stdcall;
  TPWRTCheckPermissionOnAreaID = function (permlevel:Cardinal; area:PChar)                  :Boolean;  stdcall;
  TPWRTSilentLogin             = function (login:PChar; password:PChar)                     :Boolean;  stdcall;

  TPermission = class(TObject)
  public
    AuthID:Integer;
  end;

  TWinCCUserManagement = class(TBasicUserManagement)
  private
    FCheckTimer                :TTimer;
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
    hUseAdmin:TLibHandle;
  protected
    function CheckUserAndPassword(User, Pass: String): Boolean; override;
    function GetLoggedUser:Boolean; virtual;
    function GetCurrentUserLogin:String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Logout; override;

    procedure   ValidateSecurityCode(sc:String); override;
    function    SecurityCodeExists(sc:String):Boolean; virtual;
    procedure   RegisterSecurityCode(sc:String); virtual;
    procedure   UnregisterSecurityCode(sc:String); virtual;

    function    CanAccess(sc:String):Boolean; override;
    function    GetRegisteredAccessCodes:TStringList; virtual;
  published
    property CurrentUserLogin;
    property FailureLogin;
    property LoginRetries;
    property LoginFrozenTime;
    property SuccessfulLogin;
  end;

implementation

uses ControlSecurityManager;

constructor TWinCCUserManagement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  hUseAdmin:=LoadLibrary('UseAdmin.dll');
  if hUseAdmin=0 then begin
    raise Exception.Create('WinCC está instalado?');
  end;

  //load UseAdmin functions...
  PWRTLogin                  :=TPWRTLogin(GetProcedureAddress(hUseAdmin,'PWRTLogin'));
  PWRTLogout                 :=TPWRTLogout(GetProcedureAddress(hUseAdmin,'PWRTLogout'));
  PWRTGetCurrentUser         :=TPWRTGetCurrentUser(GetProcedureAddress(hUseAdmin,'PWRTGetCurrentUser'));
  PWRTGetLoginPriority       :=TPWRTGetLoginPriority(GetProcedureAddress(hUseAdmin,'PWRTGetLoginPriority'));
  PWRTPermissionToString     :=TPWRTPermissionToString(GetProcedureAddress(hUseAdmin,'PWRTPermissionToString'));
  PWRTCheckPermission        :=TPWRTCheckPermission(GetProcedureAddress(hUseAdmin,'PWRTCheckPermission'));
  PWRTCheckPermissionOnArea  :=TPWRTCheckPermissionOnArea(GetProcedureAddress(hUseAdmin,'PWRTCheckPermissionOnArea'));
  PWRTCheckPermissionOnAreaID:=TPWRTCheckPermissionOnAreaID(GetProcedureAddress(hUseAdmin,'PWRTCheckPermissionOnAreaID'));
  PWRTSilentLogin            :=TPWRTSilentLogin(GetProcedureAddress(hUseAdmin,'PWRTSilentLogin'));

  FCheckTimer:=TTimer.Create(Self);
  FCheckTimer.OnTimer :=CheckAuthChanges;
  FCheckTimer.Interval:=1000;
  FCheckTimer.Enabled:=false;
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
    UnloadLibrary(hUseAdmin);
  if FCheckTimer<>nil then
    FCheckTimer.Destroy;
  inherited Destroy;
end;

procedure TWinCCUserManagement.CheckAuthChanges(Sender:TObject);
var
  culogin:String;
begin
  culogin:=GetCurrentUserLogin;
  if culogin<>FCurrentUserLogin then begin
    GetControlSecurityManager.UpdateControls;
    FCurrentUserLogin:=culogin;
  end;
end;

function TWinCCUserManagement.CheckUserAndPassword(User, Pass: String): Boolean;
begin
  Result:=PWRTSilentLogin(PChar(User),PChar(Pass)); //log into WinCC
end;

function TWinCCUserManagement.GetLoggedUser:Boolean;
begin
 Result := GetCurrentUserLogin<>'';
end;

function TWinCCUserManagement.GetCurrentUserLogin:String;
var
  buffer1:PChar;
  c:Integer;
begin
  c:=PWRTGetLoginPriority(); //forces initialization...

  buffer1:=StrAlloc(512);
  buffer1[0]:=#0;

  if PWRTGetCurrentUser(buffer1,510) then
    Result:=buffer1
  else
    Result:='';

  StrDispose(buffer1);
end;

procedure TWinCCUserManagement.Logout;
begin
  if PWRTLogout() then
    FLoggedUser:=false;
end;

procedure   TWinCCUserManagement.ValidateSecurityCode(sc:String);
begin
  if not SecurityCodeExists(sc) then
    raise exception.Create('Cadastre a informação de segurança usando o Security Manager do WinCC');
end;

function    TWinCCUserManagement.CanAccess(sc:String):Boolean;
var
  p, p2, i:Cardinal;
  auth:TStringList;
  c:Integer;
begin
  Result:=false;

  p:=PWRTGetLoginPriority(); //forces a initialization... I don´t know why...

  if not SecurityCodeExists(sc) then exit;

  auth:=GetRegisteredAccessCodes;

  i:=auth.IndexOf(sc);

  if (i>0) and (auth.Objects[i] is TPermission) then
    p2:=TPermission(auth.Objects[i]).AuthID
  else
    p2:=0;

  for c:=auth.Count-1 downto 0 do begin
    auth.Objects[c].Destroy;
    auth.Objects[c]:=Nil;
  end;

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
  for c:=x.Count-1 downto 0 do begin
    x.Objects[c].Destroy;
    x.Objects[c]:=Nil;
  end;
  x.Destroy;
end;

procedure   TWinCCUserManagement.RegisterSecurityCode(sc:String);
begin
  raise exception.Create('Cadastre a informação de segurança usando o Security Manager do WinCC');
end;

procedure   TWinCCUserManagement.UnregisterSecurityCode(sc:String);
begin
  //does nothing.
end;

function    TWinCCUserManagement.GetRegisteredAccessCodes:TStringList;
var
  buffer1:PChar;
  c:Integer;
  authobj:TPermission;
begin
  c:=PWRTGetLoginPriority(); //forces initialization...

  buffer1:=StrAlloc(512);

  Result:=TStringList.Create;

  for c:=1 to 1100 do begin
    buffer1[0]:=#0;
    PWRTPermissionToString(c,buffer1,510);

    if strcomp(buffer1,'')<>0 then begin
      authobj:=TPermission.Create;
      authobj.AuthID:=c;
      Result.AddObject(buffer1,authobj);
    end;
  end;
  StrDispose(buffer1);
end;

end.
