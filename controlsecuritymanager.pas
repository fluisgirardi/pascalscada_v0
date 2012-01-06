unit ControlSecurityManager;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, sysutils, HMITypes, ActnList, PLCTag;

type
  TControlSecurityManager = class(TComponent)
  private
    FControls:array of IHMIInterface;
    FUserManagement:TComponent;
    procedure SetUserManagement(um:TComponent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function   Login:Boolean;
    procedure  Logout;
    procedure  Manage;
    procedure  TryAccess(sc:String);
    procedure  RegisterControl(control:IHMIInterface);
    procedure  UnRegisterControl(control:IHMIInterface);
    procedure  UpdateControls;
    function   CanAccess(sc:String):Boolean;
    procedure  ValidateSecurityCode(sc:String);
    procedure  RegisterSecurityCode(sc:String);
    procedure  UnregisterSecurityCode(sc:String);
    function   SecurityCodeExists(sc:String):Boolean;
    function   GetRegisteredAccessCodes:TStringList;
  published
    property UserManagement:TComponent read FUserManagement write SetUserManagement;
  end;

  TPascalSCADAUserManagementAction = class(TAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

  TPascalSCADALoginAction = class(TPascalSCADAUserManagementAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TPascalSCADALogoutAction = class(TPascalSCADAUserManagementAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TPascalSCADAManageUsersAction = class(TPascalSCADAUserManagementAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TPascalSCADASecureAction = class(TPascalSCADAUserManagementAction, IHMIInterface)
  private
    FCanBeAccessed:Boolean;
    FSecurityCode:String;
    procedure SetSecurityCode(sc:String);

    function  GetControlSecurityCode:String;
    procedure MakeUnsecure;
    procedure CanBeAccessed(a:Boolean);

    //unused procedures
    procedure SetHMITag(t:TPLCTag); virtual;
    function  GetHMITag:TPLCTag;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTarget(Target: TObject); override;
  published
    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
  end;

  function GetControlSecurityManager:TControlSecurityManager;

implementation

uses BasicUserManagement, hsstrings, Dialogs, Controls;

constructor TControlSecurityManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUserManagement:=nil;
  SetLength(FControls,0);
end;

destructor TControlSecurityManager.Destroy;
begin
  if Length(FControls)>0 then
    raise Exception.Create(SSecurityControlBusy);
  inherited Destroy;
end;

function   TControlSecurityManager.Login:Boolean;
begin
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).Login
  else
    Result:=false;
end;

procedure  TControlSecurityManager.Logout;
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).Logout
end;

procedure  TControlSecurityManager.Manage;
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).Manage;
end;

procedure  TControlSecurityManager.TryAccess(sc:String);
begin
  if FUserManagement<>nil then
    if not TBasicUserManagement(FUserManagement).CanAccess(sc) then
      raise Exception.Create(SAccessDenied);
end;

procedure TControlSecurityManager.SetUserManagement(um:TComponent);
begin
  if (um<>nil) and (not (um is TBasicUserManagement)) then
    raise Exception.Create(SInvalidUserManager);

  if (um<>nil) and (FUserManagement<>nil) then
    raise Exception.Create(SUserManagementIsSet);

  FUserManagement:=um;
  UpdateControls;
end;

procedure  TControlSecurityManager.RegisterControl(control:IHMIInterface);
var
  h:Integer;
begin
  h:=Length(FControls);
  SetLength(FControls,h+1);
  FControls[h]:=control;
  control.CanBeAccessed(CanAccess(control.GetControlSecurityCode));
end;

procedure  TControlSecurityManager.UnRegisterControl(control:IHMIInterface);
var
  c, h:Integer;
begin
  h:=High(FControls);
  for c:=0 to h do
    if FControls[c]=control then begin
      FControls[c]:=FControls[h];
      SetLength(FControls,h);
      break;
    end;
end;

procedure  TControlSecurityManager.UpdateControls;
var
  c:Integer;
begin
  for c:=0 to High(FControls) do
    FControls[c].CanBeAccessed(CanAccess(FControls[c].GetControlSecurityCode));
end;

function   TControlSecurityManager.CanAccess(sc:String):Boolean;
begin
  Result:=true;

  if sc='' then exit;

  if (FUserManagement<>nil) and (FUserManagement is TBasicUserManagement) then
    Result:=TBasicUserManagement(FUserManagement).CanAccess(sc);
end;

procedure  TControlSecurityManager.ValidateSecurityCode(sc:String);
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).ValidateSecurityCode(sc);
end;

procedure  TControlSecurityManager.RegisterSecurityCode(sc:String);
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).RegisterSecurityCode(sc);
end;

procedure  TControlSecurityManager.UnregisterSecurityCode(sc:String);
var
  being_used:Boolean;
  c:Integer;
begin
  being_used:=false;
  for c:=0 to Length(FControls) do
    being_used:=being_used or (FControls[c].GetControlSecurityCode=sc);

  if being_used then begin
    case MessageDlg(SSecurityCodeBusyWantRemove,mtConfirmation,mbYesNoCancel,0) of
      mrYes:
        for c:=0 to Length(FControls) do
          if FControls[c].GetControlSecurityCode=sc then
            FControls[c].MakeUnsecure;
      mrNo:
        raise Exception.Create(SSecurityCodeStillBusy);
      mrCancel:
        exit;
    end;
  end;

  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).UnregisterSecurityCode(sc);
end;

function   TControlSecurityManager.SecurityCodeExists(sc:String):Boolean;
begin
  Result:=false;
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).SecurityCodeExists(sc);
end;

function   TControlSecurityManager.GetRegisteredAccessCodes:TStringList;
begin
  if FUserManagement=nil then begin
    Result:=TStringList.Create
  end else
    Result:=TBasicUserManagement(FUserManagement).GetRegisteredAccessCodes;
end;

////////////////////////////////////////////////////////////////////////////////
//PascaSCADA user management Standart actions
////////////////////////////////////////////////////////////////////////////////

function TPascalSCADAUserManagementAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result:=true;
end;

procedure TPascalSCADALoginAction.UpdateTarget(Target: TObject);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    with GetControlSecurityManager.UserManagement as TBasicUserManagement do
      Enabled:=not UserLogged;
end;

procedure TPascalSCADALoginAction.ExecuteTarget(Target: TObject);
begin
  GetControlSecurityManager.Login;
end;

procedure TPascalSCADALogoutAction.UpdateTarget(Target: TObject);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    with GetControlSecurityManager.UserManagement as TBasicUserManagement do
      Enabled:=UserLogged;
end;

procedure TPascalSCADALogoutAction.ExecuteTarget(Target: TObject);
begin
  GetControlSecurityManager.Logout;
end;

procedure TPascalSCADAManageUsersAction.UpdateTarget(Target: TObject);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    with GetControlSecurityManager.UserManagement as TBasicUserManagement do
      Enabled:=UserLogged;
end;

procedure TPascalSCADAManageUsersAction.ExecuteTarget(Target: TObject);
begin
  GetControlSecurityManager.Manage;
end;

//

constructor TPascalSCADASecureAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetControlSecurityManager.RegisterControl(self as IHMIInterface);
end;

destructor  TPascalSCADASecureAction.Destroy;
begin
  GetControlSecurityManager.UnRegisterControl(self as IHMIInterface);
  inherited Destroy;
end;

procedure   TPascalSCADASecureAction.UpdateTarget(Target: TObject);
begin
  Enabled:=FCanBeAccessed;
end;

procedure TPascalSCADASecureAction.SetSecurityCode(sc:String);
begin
  if Trim(sc)='' then
    Self.CanBeAccessed(true)
  else
    with GetControlSecurityManager do begin
      ValidateSecurityCode(sc);
      if not SecurityCodeExists(sc) then
        RegisterSecurityCode(sc);

      Self.CanBeAccessed(CanAccess(sc));
    end;

  FSecurityCode:=sc;
end;

function  TPascalSCADASecureAction.GetControlSecurityCode:String;
begin
  Result:=FSecurityCode;
end;

procedure TPascalSCADASecureAction.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TPascalSCADASecureAction.CanBeAccessed(a:Boolean);
begin
  FCanBeAccessed:=a;
  Enabled:=a;
end;

//unused procedures
procedure TPascalSCADASecureAction.SetHMITag(t:TPLCTag);
begin

end;

function  TPascalSCADASecureAction.GetHMITag:TPLCTag;
begin

end;


////////////////////////////////////////////////////////////////////////////////
//END PascaSCADA user management Standart actions
////////////////////////////////////////////////////////////////////////////////

var
  QPascalControlSecurityManager:TControlSecurityManager;

function GetControlSecurityManager:TControlSecurityManager;
begin
  Result:=QPascalControlSecurityManager;
end;

initialization
  QPascalControlSecurityManager:=TControlSecurityManager.Create(nil);
finalization
  QPascalControlSecurityManager.Destroy;

end.
