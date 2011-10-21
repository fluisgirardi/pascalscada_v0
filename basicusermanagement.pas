unit BasicUserManagement;

interface

uses
  SysUtils, Classes, ExtCtrls;

type
  TVKType = (vktNone, vktAlphaNumeric, vktNumeric);

  TBasicUserManagement = class(TComponent)
  private
{}  FLoggedUser:Boolean;
{}  FCurrentUserName,
{}  FCurrentUserLogin:String;
{}  FLoggedSince:TDateTime;
{}  FInactiveTimeOut:Cardinal;
{}  FLoginRetries:Cardinal;
    FFrozenTime:Cardinal;
    FVirtualKeyboardType:TVKType;

    function GetLoginTime:TDateTime;
    procedure SetInactiveTimeOut(t:Cardinal);
  protected

    function CheckUserAndPassword(User, Pass:String):Boolean; virtual;

    //read only properties.

    property UserLogged:Boolean read FLoggedUser;
    property CurrentUserName:String read FCurrentUserName;
    property CurrentUserLogin:String read FCurrentUserLogin;
    property LoggedSince:TDateTime read GetLoginTime;

    //read-write properties.
    //property VirtualKeyboardType:TVKType read FVirtualKeyboardType write FVirtualKeyboardType;
    property InactiveTimeout:Cardinal read FInactiveTimeOut write SetInactiveTimeOut;
    property LoginRetries:Cardinal read FLoginRetries write FLoginRetries;
    property LoginFrozenTime:Cardinal read  FFrozenTime write FFrozenTime;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    function    Login:Boolean; virtual;
    procedure   Logout; virtual;

    procedure   ValidateSecurityCode(sc:String); virtual;
    function    CanAccess(sc:String):Boolean; virtual;
  end;

implementation

uses keyboard;

constructor TBasicUserManagement.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
end;

destructor  TBasicUserManagement.Destroy;
begin
  inherited Destroy;
end;

function    TBasicUserManagement.Login:Boolean;
begin

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
  //does nothing.
end;

function    TBasicUserManagement.CanAccess(sc:String):Boolean;
begin
  Result:=false;
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

end.
 
