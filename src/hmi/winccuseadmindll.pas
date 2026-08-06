unit WinCCUseAdminDLL;

{ Esta unit isola todo o acesso a UseAdmin.dll (WinCC) em uma unica classe.
  A classe TUseAdminDLL carrega a DLL no Create e a libera no Destroy,
  expondo metodos publicos que encapsulam cada chamada da API nativa. }

interface

uses
  Classes, sysutils, windows, ctypes;

{$ALIGN 1}

type
  TPWRTLogin                   = function(monitor:AnsiChar)                                 :Boolean;  stdcall;
  TPWRTLogout                  = function()                                                 :Boolean;  stdcall;
  TPWRTGetCurrentUser          = function(Buffer:PAnsiChar; bufsize:LongInt)                :Boolean;  stdcall;
  TPWRTGetLoginPriority        = function()                                                 :Cardinal; stdcall;
  TPWRTPermissionToString      = function(perm:clong; permstr:LPSTR;     bufsize:cint)      :Boolean;  stdcall;
  TPWRTCheckPermission         = function(permlevel:Cardinal; suppress_messagebox:Cardinal) :Boolean;  stdcall;
  TPWRTCheckPermissionOnArea   = function(permlevel:Cardinal; area:PAnsiChar)               :Boolean;  stdcall;
  TPWRTCheckPermissionOnAreaID = function(permlevel:Cardinal; area:PAnsiChar)               :Boolean;  stdcall;
  TPWRTSilentLogin             = function(login:PAnsiChar; password:PAnsiChar)              :Boolean;  stdcall;

  { TUseAdminDLL }

  { Encapsula o carregamento/descarregamento da UseAdmin.dll e todas as
    chamadas expostas por ela. A DLL e carregada no construtor e liberada
    no destrutor - nao ha carregamento tardio (lazy load) aqui. }
  TUseAdminDLL = class(TObject)
  private
    FHandle                    :THANDLE;
    PWRTLogin                  :TPWRTLogin;
    PWRTLogout                 :TPWRTLogout;
    PWRTGetCurrentUser         :TPWRTGetCurrentUser;
    PWRTGetLoginPriority       :TPWRTGetLoginPriority;
    PWRTPermissionToString     :TPWRTPermissionToString;
    PWRTCheckPermission        :TPWRTCheckPermission;
    PWRTCheckPermissionOnArea  :TPWRTCheckPermissionOnArea;
    PWRTCheckPermissionOnAreaID:TPWRTCheckPermissionOnAreaID;
    PWRTSilentLogin             :TPWRTSilentLogin;
    procedure LoadProcs;
  public
    constructor Create(const LibraryFileName:AnsiString = 'UseAdmin.dll');
    destructor Destroy; override;

    function Login(Monitor:AnsiChar):Boolean;
    function Logout:Boolean;
    function GetCurrentUser(Buffer:PAnsiChar; BufSize:LongInt):Boolean;
    function GetLoginPriority:Cardinal;
    function PermissionToString(Perm:clong; PermStr:LPSTR; BufSize:cint):Boolean;
    function CheckPermission(PermLevel:Cardinal; SuppressMessageBox:Cardinal):Boolean;
    function CheckPermissionOnArea(PermLevel:Cardinal; Area:PAnsiChar):Boolean;
    function CheckPermissionOnAreaID(PermLevel:Cardinal; Area:PAnsiChar):Boolean;
    function SilentLogin(aLogin, aPassword:PAnsiChar):Boolean;

    property Handle:THANDLE read FHandle;
  end;

implementation

uses hsstrings;

{ TUseAdminDLL }

constructor TUseAdminDLL.Create(const LibraryFileName:AnsiString = 'UseAdmin.dll');
begin
  inherited Create;

  FHandle:=LoadLibrary(PAnsiChar(LibraryFileName));
  if FHandle=0 then
    raise Exception.Create(SWinCCAreInstalled);

  LoadProcs;
end;

destructor TUseAdminDLL.Destroy;
begin
  if FHandle<>0 then begin
    FreeLibrary(FHandle);
    FHandle:=0;
  end;
  inherited Destroy;
end;

procedure TUseAdminDLL.LoadProcs;
begin
  PWRTLogin                  :=TPWRTLogin(GetProcAddress(FHandle,'PWRTLogin'));
  PWRTLogout                 :=TPWRTLogout(GetProcAddress(FHandle,'PWRTLogout'));
  PWRTGetCurrentUser         :=TPWRTGetCurrentUser(GetProcAddress(FHandle,'PWRTGetCurrentUser'));
  PWRTGetLoginPriority       :=TPWRTGetLoginPriority(GetProcAddress(FHandle,'PWRTGetLoginPriority'));
  PWRTPermissionToString     :=TPWRTPermissionToString(GetProcAddress(FHandle,'PWRTPermissionToStringA'));
  PWRTCheckPermission        :=TPWRTCheckPermission(GetProcAddress(FHandle,'PWRTCheckPermission'));
  PWRTCheckPermissionOnArea  :=TPWRTCheckPermissionOnArea(GetProcAddress(FHandle,'PWRTCheckPermissionOnArea'));
  PWRTCheckPermissionOnAreaID:=TPWRTCheckPermissionOnAreaID(GetProcAddress(FHandle,'PWRTCheckPermissionOnAreaID'));
  PWRTSilentLogin            :=TPWRTSilentLogin(GetProcAddress(FHandle,'PWRTSilentLogin'));
end;

function TUseAdminDLL.Login(Monitor:AnsiChar):Boolean;
begin
  Result:=PWRTLogin(Monitor);
end;

function TUseAdminDLL.Logout:Boolean;
begin
  Result:=PWRTLogout();
end;

function TUseAdminDLL.GetCurrentUser(Buffer:PAnsiChar; BufSize:LongInt):Boolean;
begin
  Result:=PWRTGetCurrentUser(Buffer,BufSize);
end;

function TUseAdminDLL.GetLoginPriority:Cardinal;
begin
  Result:=PWRTGetLoginPriority();
end;

function TUseAdminDLL.PermissionToString(Perm:clong; PermStr:LPSTR; BufSize:cint):Boolean;
begin
  Result:=PWRTPermissionToString(Perm,PermStr,BufSize);
end;

function TUseAdminDLL.CheckPermission(PermLevel:Cardinal; SuppressMessageBox:Cardinal):Boolean;
begin
  Result:=PWRTCheckPermission(PermLevel,SuppressMessageBox);
end;

function TUseAdminDLL.CheckPermissionOnArea(PermLevel:Cardinal; Area:PAnsiChar):Boolean;
begin
  Result:=PWRTCheckPermissionOnArea(PermLevel,Area);
end;

function TUseAdminDLL.CheckPermissionOnAreaID(PermLevel:Cardinal; Area:PAnsiChar):Boolean;
begin
  Result:=PWRTCheckPermissionOnAreaID(PermLevel,Area);
end;

function TUseAdminDLL.SilentLogin(aLogin, aPassword: PAnsiChar): Boolean;
begin
  Result:=PWRTSilentLogin(aLogin,aPassword);
end;

end.
