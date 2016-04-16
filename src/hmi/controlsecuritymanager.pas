unit ControlSecurityManager;

interface

uses
  Classes, sysutils, HMITypes, ActnList, PLCTag, BasicUserManagement, Controls,
  LCLType;

type


  { TControlSecurityManager }

  TControlSecurityManager = class(TComponent)
  private
    FControls:array of IHMIInterface;
    FUserManagement:TBasicUserManagement;
    procedure SetUserManagement(um:TBasicUserManagement);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function   Login(Userlogin, Userpassword: UTF8String; var UID: Integer):Boolean; overload;
    function   Login:Boolean; overload;

    procedure  Logout;
    procedure  Manage;
    function   GetCurrentUserlogin:UTF8String;
    procedure  TryAccess(sc:UTF8String);
    procedure  RegisterControl(control:IHMIInterface);
    procedure  UnRegisterControl(control:IHMIInterface);
    procedure  UpdateControls;
    function   CanAccess(sc:UTF8String):Boolean;
    procedure  ValidateSecurityCode(sc:UTF8String);
    procedure  RegisterSecurityCode(sc:UTF8String);
    procedure  UnregisterSecurityCode(sc:UTF8String);
    function   SecurityCodeExists(sc:UTF8String):Boolean;
    function   GetRegisteredAccessCodes:TStringList;
    function   CheckIfUserIsAllowed(sc:UTF8String; RequireUserLogin:Boolean; var userlogin:UTF8String):Boolean;
  published
    property UserManagement:TBasicUserManagement read FUserManagement write SetUserManagement;
  end;

  //actions...

  { TPascalSCADACheckSpecialTokenAction }

  TPascalSCADACheckSpecialTokenAction = class(TAction)
  private
    FAuthorizedBy: UTF8String;
    FRequireLoginAlways: Boolean;
    FSecurityCode: UTF8String;
    procedure SetSecurityCode(AValue: UTF8String);
  public
    function Execute: Boolean; override;
    function HandlesTarget(Target: TObject): Boolean; override;
  published
    property AuthorizedBy:UTF8String read FAuthorizedBy;
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;
    property RequireLoginAlways:Boolean read FRequireLoginAlways write FRequireLoginAlways;
  end;

  { TPascalSCADAUserManagementAction }

  TPascalSCADAUserManagementAction = class(TAction, IHMIInterface)
  private
    FDisableIfNotAuthorized: Boolean;
    procedure SetDisableIfNotAuthorized(AValue: Boolean);
  protected
    FEnabled,
    FAccessAllowed:Boolean;
    FSecurityCode:UTF8String;

    procedure SetEnabled(AValue: Boolean); virtual;

    function  GetControlSecurityCode:UTF8String; virtual;
    procedure MakeUnsecure; virtual;
    procedure CanBeAccessed(a:Boolean); virtual;

    //unused procedures
    procedure SetHMITag(t:TPLCTag);
    function  GetHMITag:TPLCTag;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default true;
    property DisableIfNotAuthorized:Boolean read FDisableIfNotAuthorized write SetDisableIfNotAuthorized default true;
  end;

  { TPascalSCADALoginAction }

  TPascalSCADALoginAction = class(TPascalSCADAUserManagementAction)
  protected
    procedure CanBeAccessed(a: Boolean); override;
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  { TPascalSCADALogoutAction }

  TPascalSCADALogoutAction = class(TPascalSCADAUserManagementAction)
  protected
    procedure CanBeAccessed(a: Boolean); override;
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  { TPascalSCADALogin_LogoutAction }

  TPascalSCADALogin_LogoutAction = class(TPascalSCADAUserManagementAction)
  private
    FAfterLogin: TNotifyEvent;
    FBeforeLogin: TNotifyEvent;
    FWithUserLoggedInImageIndex,
    FWithoutUserLoggedInImageIndex:LongInt;
    FWithUserLoggedInCaption,
    FWithoutUserLoggedInCaption:TCaption;
    FWithUserLoggedInHint,
    FWithoutUserLoggedInHint:TTranslateString;
    function GetCurrentCaption: TCaption;
    function GetCurrentHintMessage: TTranslateString;
    function GetCurrentImageIndex: LongInt;
    procedure SetWithUserLoggedInCaption(const AValue: TCaption);
    procedure SetWithUserLoggedInHint(const AValue: TTranslateString);
    procedure SetWithUserLoggedInImageIndex(const AValue: LongInt);
    procedure SetWithoutUserLoggedInCaption(const AValue: TCaption);
    procedure SetWithoutUserLoggedInHint(const AValue: TTranslateString);
    procedure SetWithoutUserLoggedInImageIndex(const AValue: LongInt);
    procedure UpdateMyState;
  protected
    procedure CanBeAccessed(a: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property Caption:TCaption read GetCurrentCaption;
    property Hint:TTranslateString read GetCurrentHintMessage;
    property ImageIndex:LongInt read GetCurrentImageIndex;
    property WithUserLoggedInCaption:TCaption         read FWithUserLoggedInCaption    write SetWithUserLoggedInCaption;
    property WithUserLoggedInHint:TTranslateString    read FWithUserLoggedInHint       write SetWithUserLoggedInHint;
    property WithUserLoggedInImageIndex:LongInt       read FWithUserLoggedInImageIndex write SetWithUserLoggedInImageIndex;

    property WithoutUserLoggedInCaption:TCaption      read FWithoutUserLoggedInCaption    write SetWithoutUserLoggedInCaption;
    property WithoutUserLoggedInHint:TTranslateString read FWithoutUserLoggedInHint       write SetWithoutUserLoggedInHint;
    property WithoutUserLoggedInImageIndex:LongInt    read FWithoutUserLoggedInImageIndex write SetWithoutUserLoggedInImageIndex;
    property BeforeLogin:TNotifyEvent                 read FBeforeLogin                   write FBeforeLogin;
    property AfterLogin:TNotifyEvent                  read FAfterLogin                    write FAfterLogin;
  end;

  { TPascalSCADAManageUsersAction }

  TPascalSCADAManageUsersAction = class(TPascalSCADAUserManagementAction)
  protected
    procedure CanBeAccessed(a: Boolean); override;
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  { TPascalSCADASecureAction }

  TPascalSCADASecureAction = class(TPascalSCADAUserManagementAction)
  protected
    procedure SetSecurityCode(sc:UTF8String);
  public
    procedure UpdateTarget(Target: TObject); override;
    function Execute: Boolean; override;
  published
    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;
  end;

  function GetControlSecurityManager:TControlSecurityManager;

implementation

uses hsstrings, Dialogs;

{ TPascalSCADACheckSpecialTokenAction }

procedure TPascalSCADACheckSpecialTokenAction.SetSecurityCode(AValue: UTF8String);
begin
  if FSecurityCode=AValue then Exit;

  if Trim(AValue)<>'' then
    with GetControlSecurityManager do begin
      ValidateSecurityCode(AValue);
      if not SecurityCodeExists(AValue) then
        RegisterSecurityCode(AValue);
    end;

  FSecurityCode:=AValue;
end;

function TPascalSCADACheckSpecialTokenAction.Execute: Boolean;
begin
  if GetControlSecurityManager.CheckIfUserIsAllowed(FSecurityCode, FRequireLoginAlways, FAuthorizedBy) then
    Result:=inherited Execute
  else
    Result:=false;
end;

function TPascalSCADACheckSpecialTokenAction.HandlesTarget(Target: TObject
  ): Boolean;
begin
  Result:=true;
end;

{ TPascalSCADALogin_LogoutAction }

function TPascalSCADALogin_LogoutAction.GetCurrentCaption: TCaption;
begin
  Result:=inherited Caption;
end;

function TPascalSCADALogin_LogoutAction.GetCurrentHintMessage: TTranslateString;
begin
  Result:=inherited Hint;
end;

function TPascalSCADALogin_LogoutAction.GetCurrentImageIndex: LongInt;
begin
  Result:=inherited ImageIndex;
end;

procedure TPascalSCADALogin_LogoutAction.SetWithUserLoggedInCaption(
  const AValue: TCaption);
begin
  if FWithUserLoggedInCaption=AValue then exit;
  FWithUserLoggedInCaption:=AValue;
  UpdateMyState;
end;

procedure TPascalSCADALogin_LogoutAction.SetWithUserLoggedInHint(
  const AValue: TTranslateString);
begin
  if FWithUserLoggedInHint=AValue then exit;
  FWithUserLoggedInHint:=AValue;
  UpdateMyState;
end;

procedure TPascalSCADALogin_LogoutAction.SetWithUserLoggedInImageIndex(
  const AValue: LongInt);
begin
  if FWithUserLoggedInImageIndex=AValue then exit;
  FWithUserLoggedInImageIndex:=AValue;
  UpdateMyState;
end;

procedure TPascalSCADALogin_LogoutAction.SetWithoutUserLoggedInCaption(
  const AValue: TCaption);
begin
  if FWithoutUserLoggedInCaption=AValue then exit;
  FWithoutUserLoggedInCaption:=AValue;
  UpdateMyState;
end;

procedure TPascalSCADALogin_LogoutAction.SetWithoutUserLoggedInHint(
  const AValue: TTranslateString);
begin
  if FWithoutUserLoggedInHint=AValue then exit;
  FWithoutUserLoggedInHint:=AValue;
  UpdateMyState;
end;

procedure TPascalSCADALogin_LogoutAction.SetWithoutUserLoggedInImageIndex(
  const AValue: LongInt);
begin
  if FWithoutUserLoggedInImageIndex=AValue then exit;
  FWithoutUserLoggedInImageIndex:=AValue;
  UpdateMyState;
end;

procedure TPascalSCADALogin_LogoutAction.UpdateMyState;
begin
  if GetControlSecurityManager.UserManagement<>nil then
    if TBasicUserManagement(GetControlSecurityManager.UserManagement).UserLogged then begin
      inherited Caption   :=FWithUserLoggedInCaption;
      inherited Hint      :=FWithUserLoggedInHint;
      inherited ImageIndex:=FWithUserLoggedInImageIndex;
    end else begin
      inherited Caption   :=FWithoutUserLoggedInCaption;
      inherited Hint      :=FWithoutUserLoggedInHint;
      inherited ImageIndex:=FWithoutUserLoggedInImageIndex;
    end;
end;

procedure TPascalSCADALogin_LogoutAction.CanBeAccessed(a: Boolean);
begin
  inherited CanBeAccessed(true); //it can be accessed always.
  UpdateMyState;
end;

constructor TPascalSCADALogin_LogoutAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWithUserLoggedInImageIndex:=-1;
  FWithoutUserLoggedInImageIndex:=-1;
end;

procedure TPascalSCADALogin_LogoutAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TPascalSCADALogin_LogoutAction.ExecuteTarget(Target: TObject);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    if TBasicUserManagement(GetControlSecurityManager.UserManagement).UserLogged then begin
      GetControlSecurityManager.Logout;
    end else begin
      if Assigned(FBeforeLogin) then
        FBeforeLogin(Self);

      GetControlSecurityManager.Login;

      if Assigned(FAfterLogin) then
        FAfterLogin(Self);
    end;
  UpdateMyState;
end;

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

function TControlSecurityManager.Login(Userlogin, Userpassword: UTF8String; var UID:Integer): Boolean; overload;
begin
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).Login(Userlogin,Userpassword,UID)
  else
    Result:=false;
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

function TControlSecurityManager.GetCurrentUserlogin: UTF8String;
begin
  Result:='';
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).CurrentUserLogin;
end;

procedure  TControlSecurityManager.TryAccess(sc:UTF8String);
begin
  if FUserManagement<>nil then
    if not TBasicUserManagement(FUserManagement).CanAccess(sc) then
      raise Exception.Create(SAccessDenied);
end;

procedure TControlSecurityManager.SetUserManagement(um:TBasicUserManagement);
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
  h:LongInt;
begin
  h:=Length(FControls);
  SetLength(FControls,h+1);
  FControls[h]:=control;
  control.CanBeAccessed(CanAccess(control.GetControlSecurityCode));
end;

procedure  TControlSecurityManager.UnRegisterControl(control:IHMIInterface);
var
  c, h:LongInt;
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
  c:LongInt;
begin
  for c:=0 to High(FControls) do
    FControls[c].CanBeAccessed(CanAccess(FControls[c].GetControlSecurityCode));
end;

function   TControlSecurityManager.CanAccess(sc:UTF8String):Boolean;
begin
  Result:=true;

  if sc='' then exit;

  if (FUserManagement<>nil) and (FUserManagement is TBasicUserManagement) then
    Result:=TBasicUserManagement(FUserManagement).CanAccess(sc);
end;

procedure  TControlSecurityManager.ValidateSecurityCode(sc:UTF8String);
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).ValidateSecurityCode(sc);
end;

procedure  TControlSecurityManager.RegisterSecurityCode(sc:UTF8String);
begin
  if FUserManagement<>nil then
    TBasicUserManagement(FUserManagement).RegisterSecurityCode(sc);
end;

procedure  TControlSecurityManager.UnregisterSecurityCode(sc:UTF8String);
var
  being_used:Boolean;
  c:LongInt;
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

function   TControlSecurityManager.SecurityCodeExists(sc:UTF8String):Boolean;
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

function TControlSecurityManager.CheckIfUserIsAllowed(sc: UTF8String;
  RequireUserLogin: Boolean; var userlogin: UTF8String): Boolean;
begin
  Result:=false;
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).CheckIfUserIsAllowed(sc, RequireUserLogin, userlogin);
end;

////////////////////////////////////////////////////////////////////////////////
//PascaSCADA user management Standart actions
////////////////////////////////////////////////////////////////////////////////

procedure TPascalSCADAUserManagementAction.SetDisableIfNotAuthorized(
  AValue: Boolean);
begin
  if FDisableIfNotAuthorized=AValue then Exit;
  FDisableIfNotAuthorized:=AValue;
  CanBeAccessed(GetControlSecurityManager.CanAccess(FSecurityCode));
end;

procedure TPascalSCADAUserManagementAction.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  inherited Enabled:=FEnabled and FAccessAllowed;
end;

function TPascalSCADAUserManagementAction.GetControlSecurityCode: UTF8String;
begin
  Result:=FSecurityCode;
end;

procedure TPascalSCADAUserManagementAction.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure TPascalSCADAUserManagementAction.CanBeAccessed(a: Boolean);
begin
  FAccessAllowed:=a or (FDisableIfNotAuthorized=false);
  inherited Enabled:=FEnabled and FAccessAllowed;
end;

procedure TPascalSCADAUserManagementAction.SetHMITag(t: TPLCTag);
begin
  //does nothing
end;

function TPascalSCADAUserManagementAction.GetHMITag: TPLCTag;
begin
  Result:=nil;
  //does nothing
end;

function TPascalSCADAUserManagementAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result:=true;
end;

constructor TPascalSCADAUserManagementAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled:=true;
  FDisableIfNotAuthorized:=true;
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor TPascalSCADAUserManagementAction.Destroy;
begin
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////
//PascaSCADA Login Action
////////////////////////////////////////////////////////////////////////////////

procedure TPascalSCADALoginAction.CanBeAccessed(a: Boolean);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    with GetControlSecurityManager.UserManagement as TBasicUserManagement do
      inherited CanBeAccessed(not UserLogged);
end;

procedure TPascalSCADALoginAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TPascalSCADALoginAction.ExecuteTarget(Target: TObject);
begin
  GetControlSecurityManager.Login;
end;

////////////////////////////////////////////////////////////////////////////////
//PascaSCADA Logout Action
////////////////////////////////////////////////////////////////////////////////

procedure TPascalSCADALogoutAction.CanBeAccessed(a: Boolean);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    with GetControlSecurityManager.UserManagement as TBasicUserManagement do
      inherited CanBeAccessed(UserLogged);
end;

procedure TPascalSCADALogoutAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(true);
end;

procedure TPascalSCADALogoutAction.ExecuteTarget(Target: TObject);
begin
  GetControlSecurityManager.Logout;
end;

////////////////////////////////////////////////////////////////////////////////
//PascaSCADA User management action
////////////////////////////////////////////////////////////////////////////////

procedure TPascalSCADAManageUsersAction.CanBeAccessed(a: Boolean);
begin
  if GetControlSecurityManager.UserManagement<>nil then
    with GetControlSecurityManager.UserManagement as TBasicUserManagement do
      inherited CanBeAccessed(UserLogged);
end;

procedure TPascalSCADAManageUsersAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(false);
end;

procedure TPascalSCADAManageUsersAction.ExecuteTarget(Target: TObject);
begin
  GetControlSecurityManager.Manage;
end;

////////////////////////////////////////////////////////////////////////////////
//PascaSCADA General purpose secure action
////////////////////////////////////////////////////////////////////////////////

procedure   TPascalSCADASecureAction.UpdateTarget(Target: TObject);
begin
  CanBeAccessed(FAccessAllowed);
  inherited UpdateTarget(Target);
end;

function TPascalSCADASecureAction.Execute: Boolean;
begin
  if GetControlSecurityManager.CanAccess(FSecurityCode) then
    Result:=inherited Execute
  else begin
    MessageDlg('Error','Access denied!',mtInformation,[mbOK],0);
    Result:=false;
  end;
end;

procedure TPascalSCADASecureAction.SetSecurityCode(sc: UTF8String);
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
