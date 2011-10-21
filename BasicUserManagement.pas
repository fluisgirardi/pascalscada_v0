unit BasicUserManagement;

interface

uses
  SysUtils, Classes;

type
  TVKType = (vktAlphaNumeric, vktNumeric);

  TBasicUserManagement = class(TComponent)
  private
    FLoggedUser:Boolean;
    FCurrentUserName,
    FCurrentUserLogin:String;
    FLoggedSince:TDateTime;
    FInactiveTimeOut:Cardinal;
    FLoginRetries:Cardinal;
    FFreezeTime:Cardinal;
    FNumberOfRetriesToFreeze:Cardinal;
    FUseVirtualKeyBoard:Boolean;
    FVirtualKeyboardType:TVKType;
    function GetLoginTime:TDateTime;
  protected

    function CheckUserAndPassword(User, Pass:String):Boolean; virtual;

    property UserLogged:Boolean read FLoggedUser;
    property CurrentUserName:String read FCurrentUserName;
    property CurrentUserLogin:String read FCurrentUserLogin;
    property LoggedSince:TDateTime read GetLoginTime;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    function    Login:Boolean; virtual;
    procedure   Logout; virtual;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PascalSCADA User Management', [TBasicUserManagement]);
end;

end.
 