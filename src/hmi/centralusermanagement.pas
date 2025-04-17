unit CentralUserManagement;

{$mode ObjFPC}

interface

uses
  BasicUserManagement, ControlSecurityManager, fpjson, jsonparser, Classes,
  fphttpclient, SysUtils, Forms;

type

  { TCentralUserManagement }

  TCentralUserManagement = class(TBasicUserManagement)
  private
    FAuthServer: String;
    FAuthServerPort: Word;
    FRaiseExceptOnConnFailure: Boolean;
    FUseCentralUserAsLocalUser: Boolean;
    function PostMethod(aAPIEndpoint: String; aJsonData: TJSONData;
      var ReturnData: UTF8String): Boolean;
    function PostMethodInt(aAPIEndpoint: String; aJsonData: TJSONData;
      var ReturnData: UTF8String): Integer;
    procedure setAuthServer(AValue: String);
    procedure SetAuthServerPort(AValue: Word);
    procedure SetUseCentralUserAsLocalUser(AValue: Boolean);
    procedure UpdateControlSecureState(Data: PtrInt);

  protected
    function CheckUserAndPassword(User, Pass:UTF8String; out UserID:Integer; LoginAction:Boolean):Boolean; override;
    function CanAccess(sc: UTF8String; aUID: Integer): Boolean; override; overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetRegisteredAccessCodes: TStringList; override;
    procedure Manage; override;

    //Security codes management
    procedure ValidateSecurityCode(sc:UTF8String); override;
    procedure RegisterSecurityCode(sc: UTF8String); override;
    function SecurityCodeExists(sc: UTF8String): Boolean; override;

    function  CanAccess(sc:UTF8String):Boolean; override;
  published
    property UID;
    property ChipCardReader;
    property CurrentUserName;
    property CurrentUserLogin;
    property LoggedSince;

    property LoginRetries;
    property LoginFrozenTime;

    property SuccessfulLogin;
    property FailureLogin;
    property UserChanged;
  published
    property RaiseExceptOnConnFailure:Boolean read FRaiseExceptOnConnFailure write FRaiseExceptOnConnFailure;
    property AuthServer:String read FAuthServer write setAuthServer;
    property AuthServerPort:Word read FAuthServerPort Write SetAuthServerPort;
    property UseCentralUserAsLocalUser:Boolean read FUseCentralUserAsLocalUser write SetUseCentralUserAsLocalUser;
  end;

const
  _User = 'user';
  _Password = 'password';
  _checkuserpwd = 'checkuserpwd';
  _enumsecuritycodes = 'enumsecuritycodes';
  _ManagementNotAvailable = 'User management not available, check the user management on the server!';
  _ManagementValidateSC = 'User can not access Security Code';
  _ManagementValidateRSC = 'Security Code is not found';
  _CannotConnectOnSecServer = 'Conneciton to security server failed';
  _SecurityServerRejectedSC = 'Security server has been rejected this security code';
  _SecurityServerCannotRegisterSC = 'Security server cannot register this security code';
  _uidcanaccess = 'uidcanaccess';
  _securitycode = 'securitycode';
  _uid = 'uid';
  _login = 'login';
  _registersecuritycode = 'registersecuritycode';
  _validadesecuritycode = 'validadesecuritycode';
  _FUseCentralUserAsLocalUser = 'UseCentralUserAsLocalUser';


implementation

{ TCentralUserManagement }

function TCentralUserManagement.PostMethod(aAPIEndpoint: String;
  aJsonData: TJSONData; var ReturnData: UTF8String): Boolean;
begin
  exit(PostMethodInt(aAPIEndpoint,aJsonData,ReturnData)=200);
end;

function TCentralUserManagement.PostMethodInt(aAPIEndpoint: String;
  aJsonData: TJSONData; var ReturnData: UTF8String): Integer;
var
  wc: TFPHTTPClient;
  ms, sl: TStringStream;
begin
  wc:=TFPHTTPClient.Create(Self);
  try
    ms:=TStringStream.Create(aJsonData.AsJSON);
    try
      ms.Position:=0;
      wc.RequestBody:=ms;
      sl:=TStringStream.Create;
      try
        try
          wc.Post('http://'+FAuthServer+':'+FAuthServerPort.ToString+'/'+aAPIEndpoint, sl);
        except
        end;
        sl.Position:=0;
        ReturnData:=sl.DataString;
        //Clipboard.AsText:=ReturnData
      finally
        FreeAndNil(sl)
      end;
      Exit(wc.ResponseStatusCode);
    finally
      FreeAndNil(ms);
    end;
  finally
    FreeAndNil(wc);
  end;
end;

procedure TCentralUserManagement.setAuthServer(AValue: String);
begin
  if FAuthServer=AValue then Exit;
  FAuthServer:=AValue;
  Logout;
end;

procedure TCentralUserManagement.SetAuthServerPort(AValue: Word);
begin
  if FAuthServerPort=AValue then Exit;
  FAuthServerPort:=AValue;
  Logout;
end;

procedure TCentralUserManagement.SetUseCentralUserAsLocalUser(AValue: Boolean);
begin
  if FUseCentralUserAsLocalUser=AValue then Exit;
  FUseCentralUserAsLocalUser:=AValue;
  GetControlSecurityManager.UpdateControls;
end;

procedure TCentralUserManagement.UpdateControlSecureState(Data: PtrInt);
begin
  GetControlSecurityManager.UpdateControls;
end;

function TCentralUserManagement.CheckUserAndPassword(User, Pass: UTF8String;
  out UserID: Integer; LoginAction: Boolean): Boolean;
var
  jobj: TJSONObject;
  UserInfo: UTF8String;
  UserData: TJSONData;
  aUID:TJSONNumber;
begin
  jobj:=TJSONObject.Create;
  try
    jobj.Add(_User, User);
    jobj.Add(_Password, Pass);
    if PostMethod(_checkuserpwd, jobj, UserInfo) then begin
      try
        UserData:=GetJSON(UserInfo);
      except
        exit(false);
      end;
      try
        if (UserData is TJSONObject) and TJSONObject(UserData).Find(_uid,aUID) then begin
          UserID:=aUID.AsInteger;
          exit(True);
        end else
          exit(false);
      finally
        if Assigned(UserData) then
          FreeAndNil(UserData);
      end;
    end else
      exit(false);
  finally
    FreeAndNil(jobj);
  end;
end;

function TCentralUserManagement.CanAccess(sc: UTF8String; aUID: Integer
  ): Boolean;
var
  jobj: TJSONObject;
  aux: UTF8String;
  CentralData: TJSONData;
  jUID:TJSONNumber;
  jLogin:TJSONString;
begin
  jobj:=TJSONObject.Create;
  try
    jobj.Add(_uid, aUID);
    jobj.Add(_securitycode, sc);
    jobj.Add(_FUseCentralUserAsLocalUser, FUseCentralUserAsLocalUser);
    Result:=PostMethod(_uidcanaccess, jobj, aux);
    if FUseCentralUserAsLocalUser then begin
      try
        CentralData:=GetJSON(aux);
      except
        exit;
      end;

      if (CentralData is TJSONObject) then begin
        if TJSONObject(CentralData).Find(_uid,jUID) then begin
          if FUID<>jUID.AsInteger then begin
            //delay a Control security refresh
            if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
              Application.QueueAsyncCall(@UpdateControlSecureState,0);
          end;
          FUID:=jUID.AsInteger;
        end;

        if TJSONObject(CentralData).Find(_login,jLogin) then begin
          FCurrentUserLogin:=jLogin.AsString;
        end;

        exit;
      end;
    end;
  finally
    FreeAndNil(jobj);
  end;
end;

constructor TCentralUserManagement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRaiseExceptOnConnFailure:=false;
end;

destructor TCentralUserManagement.Destroy;
begin
  if Assigned(Application) then
    Application.RemoveAllHandlersOfObject(Self);
  inherited Destroy;
end;

function TCentralUserManagement.GetRegisteredAccessCodes: TStringList;
var
  SecurityCodeList: UTF8String;
  SecCodeJArr: TJSONData;
  jobj: TJSONObject;
  i: Integer;
  aux: TJSONParser;
begin
  Result:=TStringList.Create;
  jobj:=TJSONObject.Create;
  try
    if PostMethod(_enumsecuritycodes, jobj, SecurityCodeList) then begin
      try
        aux:=TJSONParser.Create(SecurityCodeList);
        try
          SecCodeJArr:=aux.Parse;
        finally
          FreeAndNil(aux);
        end;
      except
        exit;
      end;

      try
        if (SecCodeJArr is TJSONArray) then begin

          for i:=0 to SecCodeJArr.Count-1 do
            Result.Add(SecCodeJArr.Items[i].AsString);
        end else
          exit;
      finally
        if Assigned(SecCodeJArr) then
          FreeAndNil(SecCodeJArr);
      end;
    end else
      exit;
  finally
    FreeAndNil(jobj);
  end;
end;

procedure TCentralUserManagement.Manage;
begin
  raise Exception.Create(_ManagementNotAvailable);
end;

procedure TCentralUserManagement.ValidateSecurityCode(sc: UTF8String);
var
  jobj: TJSONObject;
  aux: UTF8String;
begin
  jobj:=TJSONObject.Create;
  try
    jobj.Add(_securitycode, sc);
    case PostMethodInt(_validadesecuritycode, jobj, aux) of
      0: if FRaiseExceptOnConnFailure then
        raise Exception.Create(_CannotConnectOnSecServer);
      200: ;
      404: ;
      405:
        raise Exception.Create(_SecurityServerRejectedSC);
      else begin

      end;
    end;
  finally
    FreeAndNil(jobj);
  end;
end;

procedure TCentralUserManagement.RegisterSecurityCode(sc: UTF8String);
var
  jobj: TJSONObject;
  aux: UTF8String;
begin
  jobj:=TJSONObject.Create;
  try
    jobj.Add(_securitycode, sc);
    case PostMethodInt(_registersecuritycode, jobj, aux) of
      0:
        if FRaiseExceptOnConnFailure then
          raise Exception.Create(_CannotConnectOnSecServer);
      200: ;
      404: ;
      405:
        raise Exception.Create(_SecurityServerCannotRegisterSC);
      else begin

      end;
    end;
  finally
    FreeAndNil(jobj);
  end;
end;

function TCentralUserManagement.SecurityCodeExists(sc: UTF8String): Boolean;
var
  x:TStringList;
  c:LongInt;
begin
  x:=GetRegisteredAccessCodes;
  try
    Result:=x.IndexOf(sc)<>-1;
  finally
    x.Destroy;
  end;
end;

function TCentralUserManagement.CanAccess(sc: UTF8String): Boolean;
begin
  Result:=CanAccess(sc,FUID);
end;

end.
