unit CentralUserManagement;

{$mode ObjFPC}

interface

uses
  BasicUserManagement, fpjson, jsonparser, Classes, fphttpclient, SysUtils;

type

  { TCentralUserManagement }

  TCentralUserManagement = class(TBasicUserManagement)
  private
    FAuthServer: String;
    FAuthServerPort: Word;
    function PostMethod(aAPIEndpoint: String; aJsonData: TJSONData;
      var ReturnData: UTF8String): Boolean;
    procedure setAuthServer(AValue: String);
    procedure SetAuthServerPort(AValue: Word);

  protected
    function CheckUserAndPassword(User, Pass:UTF8String; out UserID:Integer; LoginAction:Boolean):Boolean; override;
    function CheckUserChipCard(aChipCardCode: UTF8String; var userlogin: UTF8String; var UserID: Integer; LoginAction: Boolean): Boolean; override;

    function GetCurrentUserName:UTF8String; override;
    function GetCurrentUserLogin:UTF8String; override;
    function CanAccess(sc: UTF8String; aUID: Integer): Boolean; override; overload;
    function GetRegisteredAccessCodes: TStringList; override;
  public
    procedure Manage; override;

    //Security codes management
    procedure ValidateSecurityCode(sc:UTF8String); override;
    procedure RegisterSecurityCode(sc: UTF8String); override;

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
  published
    property AuthServer:String read FAuthServer write setAuthServer;
    property AuthServerPort:Word read FAuthServerPort Write SetAuthServerPort;
  end;

const
  _User = 'user';
  _Password = 'password';
  _checkuserpwd = 'checkuserpwd';
  _enumsecuritycodes = 'enumsecuritycodes';
  _ManagementNotAvailable = 'User management not available, check the user management on the server!';
  _ManagementValidateSC = 'User can not access Security Code';
  _ManagementValidateRSC = 'Security Code is not found';
  _uidcanaccess = 'uidcanaccess';
  _securitycode = 'securitycode';
  _uid = 'uid';
  _registersecuritycode = 'registersecuritycode';
  _validadesecuritycode = 'validadesecuritycode';


implementation

{ TCentralUserManagement }

function TCentralUserManagement.PostMethod(aAPIEndpoint: String;
  aJsonData: TJSONData; var ReturnData:UTF8String): Boolean;
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
      Exit(wc.ResponseStatusCode=200);
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
        if (UserData is TJSONObject) and TJSONObject(UserData).Find('UID',aUID) then begin
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

function TCentralUserManagement.CheckUserChipCard(aChipCardCode: UTF8String;
  var userlogin: UTF8String;
  var UserID: Integer; LoginAction: Boolean): Boolean;
begin
  Result:=inherited CheckUserChipCard(aChipCardCode, userlogin, UserID,
    LoginAction);
end;

function TCentralUserManagement.GetCurrentUserName: UTF8String;
begin
  Result:=inherited GetCurrentUserName;
end;

function TCentralUserManagement.GetCurrentUserLogin: UTF8String;
begin
  Result:=inherited GetCurrentUserLogin;
end;

function TCentralUserManagement.CanAccess(sc: UTF8String; aUID: Integer
  ): Boolean;
var
  jobj: TJSONObject;
  aux: UTF8String;
begin
  jobj:=TJSONObject.Create;
  try
    jobj.Add(_uid, aUID);
    jobj.Add(_securitycode, sc);
    exit(PostMethod(_uidcanaccess, jobj, aux));
  finally
    FreeAndNil(jobj);
  end;
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
    if not PostMethod(_validadesecuritycode, jobj, aux) then
      raise Exception.Create(_ManagementValidateSC);
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
    if not PostMethod(_registersecuritycode, jobj, aux) then
      raise Exception.Create(_ManagementValidateRSC);
  finally
    FreeAndNil(jobj);
  end;
end;

function TCentralUserManagement.CanAccess(sc: UTF8String): Boolean;
begin
  Result:=CanAccess(sc,FUID);
end;

end.
