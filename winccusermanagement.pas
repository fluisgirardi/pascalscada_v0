unit WinCCUserManagement;

{$mode delphi}

interface

uses
  BasicUserManagement;

type
  TWinCCUserManagement = class(TBasicUserManagement)
  public
    function Login: Boolean; override;
    procedure Logout; override;

    procedure   ValidateSecurityCode(sc:String); override;
    function    CanAccess(sc:String):Boolean; override;
  published
    property LoggedSince;

    property SuccessfulLogin;
    property FailureLogin;

  end;

  function PWRTLogin                  (monitor:WideChar)                            :Boolean; cdecl; external 'UseAdmin.dll';
  function PWRTLogout                 ()                                            :Boolean; cdecl; external 'UseAdmin.dll';
  function PWRTGetCurrentUser         (Buffer:PChar; bufsize:Integer)               :Boolean; cdecl; external 'UseAdmin.dll';
  function PWRTGetLoginPriority       ()                                            :Integer; cdecl; external 'UseAdmin.dll';
  function PWRTPermissionToString     (perm:Integer; permstr:PChar; bufsize:Integer):Boolean; cdecl; external 'UseAdmin.dll';
  function PWRTCheckPermission        (permlevel:DWord; suppress_messagebox:DWord)  :Boolean; cdecl; external 'UseAdmin.dll';
  function PWRTCheckPermissionOnArea  (permlevel:DWord; area:PWideChar)             :Boolean; cdecl; external 'UseAdmin.dll';
  function PWRTCheckPermissionOnAreaID(permlevel:DWord; area:PWideChar)             :Boolean; cdecl; external 'UseAdmin.dll';

implementation

uses sysutils;

function  TWinCCUserManagement.Login: Boolean;
begin
  Result:=PWRTLogin('1');
  FLoggedUser:=Result;
  if FLoggedUser then
    FLoggedSince:=Now;
end;

procedure TWinCCUserManagement.Logout;
begin
  if PWRTLogout() then
    FLoggedUser:=false;
end;

procedure   TWinCCUserManagement.ValidateSecurityCode(sc:String);
begin
  StrToInt(sc); //only integer numbers are accepted
end;

function    TWinCCUserManagement.CanAccess(sc:String):Boolean;
var
  p:Integer;
begin
  Result:=false;

  p:=PWRTGetLoginPriority(); //forces a initialization... I don´t know why...

  if not TryStrToInt(sc,p) then exit;

  if PWRTCheckPermission(p,1) then
    Result:=true;
end;

end.
