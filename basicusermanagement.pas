unit BasicUserManagement;

interface

uses
  SysUtils, Classes;

type
  TVKType = (vktAlphaNumeric, vktNumeric);

  TBasicUserManagement = class(TComponent)
  private
{}  FLoggedUser:Boolean;
{}  FCurrentUserName,
{}  FCurrentUserLogin:String;
{}  FLoggedSince:TDateTime;
{}  FInactiveTimeOut:Cardinal;
{}  FLoginRetries:Cardinal;
    FFreezeTime:Cardinal;
    FUseVirtualKeyBoard:Boolean;
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
    property UseVirtualKeyBoard:Boolean read FUseVirtualKeyBoard write FUseVirtualKeyBoard;
    property VirtualKeyboardType:TVKType read FVirtualKeyboardType write FVirtualKeyboardType;
    property InactiveTimeout:Cardinal read FInactiveTimeOut write SetInactiveTimeOut;
    property LoginRetries:Cardinal read FLoginRetries write FLoginRetries;
    property LoginFrozenTime:Cardinal read  FFreezeTime write FFreezeTime;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    function    Login:Boolean; virtual;
    procedure   Logout; virtual;

    procedure   ValidateSecurityCode(sc:String); virtual;
    procedure   CanAccess(sc:String):Boolean; virtual;
  end;

procedure Register;

implementation


end.
 