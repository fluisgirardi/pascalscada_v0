unit BasicUserManagement;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ExtCtrls, Dialogs, Controls, Forms, StdCtrls, Graphics,
  usrmgnt_login, ChipCardReader;

type
  TVKType = (vktNone, vktAlphaNumeric, vktNumeric);

  TUserChangedEvent = procedure(Sender:TObject; const OldUsername, NewUserName:UTF8String) of object;

  { TBasicUserManagement }

  TBasicUserManagement = class(TComponent)
  private
    FChipCardReader: TChipCardReader;
    procedure CheckIfCardHasBeenRemoved(Sender: TObject);
    procedure ChipCardReaderProc(Data: PtrInt);
    procedure SetChipCardReader(AValue: TChipCardReader);
    procedure WarningCanBeClosed(Sender: TObject; var CanClose: Boolean);
  protected
{}  FLoggedUser:Boolean;
{}  FCurrentUserName,
{}  FCurrentUserLogin:UTF8String;
    FUID:Integer;
{}  FLoggedSince:TDateTime;
{}  FInactiveTimeOut:Cardinal;
{}  FLoginRetries:Cardinal;
    FFrozenTime:Cardinal;
    FVirtualKeyboardType:TVKType;

    FSuccessfulLogin:TNotifyEvent;
    FFailureLogin:TNotifyEvent;
    FUserChanged:TUserChangedEvent;

    FRegisteredSecurityCodes:TStringList;

    frmLoginDlg:TpsHMIfrmUserAuthentication;

    function  GetLoginTime:TDateTime;
    procedure SetInactiveTimeOut({%H-}t:Cardinal);
    procedure UnfreezeLogin(Sender:TObject);
    function  GetUID: Integer;
  protected
    procedure DoUserChanged; virtual;

    procedure DoSuccessfulLogin; virtual;
    procedure DoFailureLogin; virtual;

    function CheckUserAndPassword({%H-}User, {%H-}Pass:UTF8String; out {%H-}UserID:Integer; {%H-}LoginAction:Boolean):Boolean; virtual;
    function CheckUserChipCard(aChipCardCode:UTF8String; var userlogin:UTF8String; var {%H-}UserID:Integer; {%H-}LoginAction:Boolean):Boolean; virtual;

    function GetLoggedUser:Boolean; virtual;
    function GetCurrentUserName:UTF8String; virtual;
    function GetCurrentUserLogin:UTF8String; virtual;

    //read only properties.
    property LoggedSince:TDateTime read GetLoginTime;

    //read-write properties.
    //property VirtualKeyboardType:TVKType read FVirtualKeyboardType write FVirtualKeyboardType;
    property InactiveTimeout:Cardinal read FInactiveTimeOut write SetInactiveTimeOut;
    property LoginRetries:Cardinal read FLoginRetries write FLoginRetries;
    property LoginFrozenTime:Cardinal read  FFrozenTime write FFrozenTime;

    property SuccessfulLogin:TNotifyEvent read FSuccessfulLogin write FSuccessfulLogin;
    property FailureLogin:TNotifyEvent read FFailureLogin write FFailureLogin;
    property UserChanged:TUserChangedEvent read FUserChanged write FUserChanged;
    function CanAccess({%H-}sc:UTF8String; {%H-}aUID:Integer):Boolean; virtual; overload;
    property ChipCardReader:TChipCardReader read FChipCardReader write SetChipCardReader;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WaitForEmptyChipCard; virtual;
    procedure StartDelayedChipCardRead;
    procedure StopDelayedChipCardRead;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    function    Login:Boolean; virtual; overload;
    function    Login(Userlogin, userpassword: UTF8String; var UID: Integer):Boolean; virtual;
    procedure   Logout; virtual;
    procedure   Manage; virtual;

    //Security codes management
    procedure   ValidateSecurityCode({%H-}sc:UTF8String); virtual;
    function    SecurityCodeExists({%H-}sc:UTF8String):Boolean; virtual;
    procedure   RegisterSecurityCode({%H-}sc:UTF8String); virtual;
    procedure   UnregisterSecurityCode({%H-}sc:UTF8String); virtual;

    function    CanAccess({%H-}sc:UTF8String):Boolean; virtual;
    function    GetRegisteredAccessCodes:TStringList; virtual;

    function    CheckIfUserIsAllowed({%H-}sc: UTF8String; RequireUserLogin: Boolean; var userlogin: UTF8String; const UserHint:UTF8String): Boolean; virtual;

    //read only properties.
    property UID:Integer read GetUID;
    property UserLogged:Boolean read GetLoggedUser;
    property CurrentUserName:UTF8String read GetCurrentUserName;
    property CurrentUserLogin:UTF8String read GetCurrentUserLogin;
  end;

const
  mrOKChipCard = mrLast + 1;

implementation

uses ControlSecurityManager, hsstrings;

constructor TBasicUserManagement.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);

  if GetControlSecurityManager.UserManagement=nil then
    GetControlSecurityManager.UserManagement:=Self
  else
    raise Exception.Create(SUserManagementIsSet);

  FLoggedUser:=false;
  FCurrentUserName:='';
  FCurrentUserLogin:='';
  FUID:=-1;
  FLoggedSince:=Now;

  FRegisteredSecurityCodes:=TStringList.Create;
end;

destructor  TBasicUserManagement.Destroy;
begin
  Application.RemoveAllHandlersOfObject(Self);
  if GetControlSecurityManager.UserManagement=Self then
    GetControlSecurityManager.UserManagement:=nil;

  if FRegisteredSecurityCodes<>nil then
    FRegisteredSecurityCodes.Destroy;
  inherited Destroy;
end;

function    TBasicUserManagement.Login:Boolean;
var
  frozenTimer:TTimer;
  retries:LongInt;
  aborted, loggedin:Boolean;
  aUserIDCC, PreferredWidth, PreferredHeight, res, aUserIDDlg: Integer;
  aUserLogin: UTF8String;
begin
  if Assigned(frmLoginDlg) then begin
    frmLoginDlg.ShowOnTop;
    exit;
  end;

  frozenTimer:=TTimer.Create(nil);
  frozenTimer.Enabled:=false;
  frozenTimer.Interval:=LoginFrozenTime;
  frozenTimer.Tag:=1; //login
  frozenTimer.OnTimer:=@UnfreezeLogin;

  retries:=0;
  aborted:=false;
  loggedin:=False;
  Result:=false;

  frmLoginDlg:=TpsHMIfrmUserAuthentication.Create(nil);
  try
    frmLoginDlg.edtusername.Text:='';
    frmLoginDlg.FocusControl:=fcUserName;

    frmLoginDlg.AutoSize:=true;
    frmLoginDlg.HandleNeeded;
    frmLoginDlg.GetPreferredSize(PreferredWidth,PreferredHeight);

    while (not loggedin) and (not aborted) do begin
      frmLoginDlg.edtPassword.Text:='';
      try
        if Assigned(FChipCardReader) then begin
          if not FChipCardReader.ChipCardReady then
            FChipCardReader.InitializeChipCard;
          if FChipCardReader.ChipCardReady then begin
            WaitForEmptyChipCard;
            StartDelayedChipCardRead;
          end;
        end;

        res:=frmLoginDlg.ShowModal;
        if res in [mrOK, mrOKChipCard] then begin
          if ((res=mrOk)         and CheckUserAndPassword(frmLoginDlg.edtusername.Text, frmLoginDlg.edtPassword.Text, aUserIDDlg, true)) or
             ((res=mrOKChipCard) and CheckUserChipCard(frmLoginDlg.ChipCardCode, aUserLogin, aUserIDCC, true)) then begin
            FLoggedUser:=true;
            loggedin:=true;

            if res=mrOK then begin
              FUID:=aUserIDDlg;
              FCurrentUserLogin:=frmLoginDlg.edtusername.Text
            end else begin
              FUID:=aUserIDCC;
              FCurrentUserLogin:=aUserLogin;
            end;

            FLoggedSince:=Now;
            Result:=true;
            GetControlSecurityManager.UpdateControls;
            DoSuccessfulLogin;
          end else begin
            DoFailureLogin;
            inc(retries);
            if (FLoginRetries>0) and (retries>=FLoginRetries) then begin
              frmLoginDlg.DisableEntry;
              frozenTimer.Enabled:=true;
              frmLoginDlg.ShowModal;
              retries:=0;
            end;
          end;
        end else
          aborted:=true;

        frmLoginDlg.FocusControl:=fcPassword;
      finally
        if Assigned(FChipCardReader) then begin
          if FChipCardReader.ChipCardReady then begin
            StopDelayedChipCardRead;
            FChipCardReader.FinishChipCard;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(frmLoginDlg);
    FreeAndNil(frozenTimer);
  end;
end;

function TBasicUserManagement.Login(Userlogin, userpassword: UTF8String; var UID: Integer): Boolean;
begin
  Result:=CheckUserAndPassword(Userlogin, userpassword, UID, true);
  if Result then begin
    FLoggedUser:=true;
    FUID:=UID;
    FCurrentUserLogin:=Userlogin;
    FLoggedSince:=Now;
    Result:=true;
    GetControlSecurityManager.UpdateControls;
    DoSuccessfulLogin;
  end;
end;

procedure   TBasicUserManagement.Logout;
begin
  FLoggedUser:=false;
  FCurrentUserName:='';
  FCurrentUserLogin:='';
  FUID:=-1;
  FLoggedSince:=Now;
  GetControlSecurityManager.UpdateControls;
end;

procedure   TBasicUserManagement.Manage;
begin
  //has nothing to do here!
end;

procedure TBasicUserManagement.ValidateSecurityCode(sc: UTF8String);
begin
  //raise a exception if the security code is invalid.
end;

function TBasicUserManagement.SecurityCodeExists(sc: UTF8String): Boolean;
begin
  Result:=FRegisteredSecurityCodes.IndexOf(sc)>=0;
end;

procedure TBasicUserManagement.RegisterSecurityCode(sc: UTF8String);
begin
  if Not SecurityCodeExists(sc) then
    FRegisteredSecurityCodes.Add(sc);
end;

procedure TBasicUserManagement.UnregisterSecurityCode(sc: UTF8String);
begin
  if SecurityCodeExists(sc) then
    FRegisteredSecurityCodes.Delete(FRegisteredSecurityCodes.IndexOf(sc));
end;

function TBasicUserManagement.CanAccess(sc: UTF8String): Boolean;
begin
  Result:=false;
end;

function    TBasicUserManagement.GetRegisteredAccessCodes:TStringList;
begin
  Result:=TStringList.Create;
  Result.Assign(FRegisteredSecurityCodes);
end;

function TBasicUserManagement.CheckIfUserIsAllowed(sc: UTF8String;
  RequireUserLogin: Boolean; var userlogin: UTF8String;
  const UserHint: UTF8String): Boolean;
var
  frozenTimer:TTimer;
  aUserID, PreferredWidth, PreferredHeight, res:Integer;
  aUserLogin: UTF8String;

begin
  Result:=false;

  //se o usuário logado tem permissão, evita
  //abrir o dialog que irá solicitar a permissão
  //de outro usuário.
  if UserLogged and CanAccess(sc) and (RequireUserLogin=false) then begin
    userlogin:=GetCurrentUserLogin;
    Result:=true;
    exit;
  end;

  //se existe um dialogo de permissão especial aberto
  //chama ele...
  if Assigned(frmLoginDlg) then begin
    frmLoginDlg.ShowOnTop;
    exit;
  end;

  frozenTimer:=TTimer.Create(nil);
  frozenTimer.Enabled:=false;
  frozenTimer.Interval:=LoginFrozenTime;
  frozenTimer.Tag:=2; //Check
  frozenTimer.OnTimer:=@UnfreezeLogin;

  Result:=false;

  frmLoginDlg:=TpsHMIfrmUserAuthentication.Create(nil);
  try
    frmLoginDlg.FormStyle:=fsSystemStayOnTop;

    frmLoginDlg.Caption:=SLoginRequired;
    if trim(sc)<>'' then begin
      frmLoginDlg.lblRequiredPerm.BorderSpacing.Around:=3;
      frmLoginDlg.lblRequiredPerm.Caption:=format(SRequiredPerm,[sc]);
    end;
    if trim(UserHint)<>'' then begin
      frmLoginDlg.lblHint.BorderSpacing.Around:=3;
      frmLoginDlg.lblHint.BorderSpacing.Top:=8;
      frmLoginDlg.lblHint.Caption:=UserHint;
    end;

    frmLoginDlg.FocusControl:=fcUserName;
    frmLoginDlg.edtusername.Text:='';
    frmLoginDlg.edtPassword.Text:='';

    frmLoginDlg.AutoSize:=true;
    frmLoginDlg.HandleNeeded;
    frmLoginDlg.GetPreferredSize(PreferredWidth,PreferredHeight);

    try
      if Assigned(FChipCardReader) then begin
        if not FChipCardReader.ChipCardReady then
          FChipCardReader.InitializeChipCard;
        if FChipCardReader.ChipCardReady then begin
          WaitForEmptyChipCard;
          StartDelayedChipCardRead;
        end;
      end;

      res := frmLoginDlg.ShowModal;
      if res in [mrOk, mrOKChipCard] then begin
        if ((res=mrOk)         and CheckUserAndPassword(frmLoginDlg.edtusername.Text, frmLoginDlg.edtPassword.Text, aUserID, false)) or
           ((res=mrOKChipCard) and CheckUserChipCard(frmLoginDlg.ChipCardCode, aUserLogin, aUserID, false)) then begin
          if CanAccess(sc,aUserID) then begin
            Result:=true;

            if res=mrOK then
              userlogin:=frmLoginDlg.edtusername.Text
            else
              userlogin:=aUserLogin;

          end else
            Result:=false;
        end;
      end;

    finally
      if Assigned(FChipCardReader) then begin
        if FChipCardReader.ChipCardReady then begin
          StopDelayedChipCardRead;
          FChipCardReader.FinishChipCard;
        end;
      end;
    end;
  finally
    FreeAndNil(frmLoginDlg);
    FreeAndNil(frozenTimer);
  end;
end;

function TBasicUserManagement.GetUID: Integer;
begin
  Result:=FUID;
end;

procedure TBasicUserManagement.SetChipCardReader(AValue: TChipCardReader);
begin
  if FChipCardReader=AValue then Exit;

  if Assigned(FChipCardReader) then
    FChipCardReader.RemoveFreeNotification(Self);

  if Assigned(AValue) then
    AValue.FreeNotification(Self);

  FChipCardReader:=AValue;
end;

procedure TBasicUserManagement.CheckIfCardHasBeenRemoved(Sender: TObject);
begin
  if assigned(FChipCardReader) and (Sender is TTimer) and (TTimer(sender).Owner is TForm) then begin
    if FChipCardReader.IsEmptyChipCard then begin
      TForm(TTimer(sender).Owner).ModalResult:=mrOK;
      TTimer(sender).Enabled:=false;
    end;
  end;
end;

procedure TBasicUserManagement.ChipCardReaderProc(Data: PtrInt);
begin
  if assigned(FChipCardReader) and Assigned(frmLoginDlg) then begin
    if FChipCardReader.ChipCardRead(frmLoginDlg.ChipCardCode) then begin
      frmLoginDlg.ModalResult:=mrOKChipCard;
    end else
      StartDelayedChipCardRead;
  end;
end;

procedure TBasicUserManagement.WarningCanBeClosed(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(FChipCardReader) then
    canClose:=FChipCardReader.IsEmptyChipCard
  else
    CanClose:=true;
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

function TBasicUserManagement.CheckUserAndPassword(User, Pass: UTF8String; out
  UserID: Integer; LoginAction: Boolean): Boolean;
begin
  Result:=false;
end;

function TBasicUserManagement.CheckUserChipCard(aChipCardCode: UTF8String;
  var userlogin: UTF8String; var UserID: Integer; LoginAction: Boolean
  ): Boolean;
begin
  Result:=false;
end;

function TBasicUserManagement.GetLoggedUser:Boolean;
begin
  Result:=FLoggedUser;
end;

function TBasicUserManagement.GetCurrentUserName: UTF8String;
begin
  Result:=FCurrentUserName;
end;

function TBasicUserManagement.GetCurrentUserLogin: UTF8String;
begin
  Result:=FCurrentUserLogin;
end;

function TBasicUserManagement.CanAccess(sc: UTF8String; aUID: Integer): Boolean;
begin
  Result:=false;
end;

procedure TBasicUserManagement.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FChipCardReader) then
    FChipCardReader:=nil;
end;

procedure TBasicUserManagement.WaitForEmptyChipCard;
var
  frm: TForm;
  lbl: TLabel;
  tmr: TTimer;
begin
  if Assigned(FChipCardReader) then begin
    frm:=TForm.CreateNew(Self);
    try
      frm.SetBounds(0,0,320,240);
      frm.Position:=poScreenCenter;
      frm.BorderStyle:=bsNone;
      lbl:=TLabel.Create(frm);
      lbl.Align:=alClient;
      lbl.WordWrap:=true;
      lbl.Alignment:=taCenter;
      lbl.Layout:=tlCenter;
      lbl.font.Style:=[fsBold];
      lbl.font.Height:=16;
      lbl.Caption:=SRemoveChipCard;
      lbl.Parent:=frm;
      tmr:=TTimer.Create(frm);
      tmr.Interval:=100;
      tmr.OnTimer:=@CheckIfCardHasBeenRemoved;
      frm.OnCloseQuery:=@WarningCanBeClosed;
      repeat
        if not FChipCardReader.IsEmptyChipCard then
          frm.ShowModal;
      until FChipCardReader.IsEmptyChipCard;
    finally
      FreeAndNil(frm);
    end;
  end;
end;

procedure TBasicUserManagement.StartDelayedChipCardRead;
begin
  if assigned(FChipCardReader) and ((Application.Flags*[AppDoNotCallAsyncQueue])=[]) then begin
    Application.QueueAsyncCall(@ChipCardReaderProc,0);
  end;
end;

procedure TBasicUserManagement.StopDelayedChipCardRead;
begin
  Application.RemoveAsyncCalls(Self);
end;

procedure TBasicUserManagement.DoSuccessfulLogin;
begin
  if Assigned(FSuccessfulLogin) then
    FSuccessfulLogin(Self);
end;

procedure TBasicUserManagement.DoFailureLogin;
begin
  if Assigned(FFailureLogin) then
    FFailureLogin(Self);
end;

procedure TBasicUserManagement.UnfreezeLogin(Sender:TObject);
begin
  if sender is TTimer then begin
    TTimer(sender).Enabled:=false;
    case TTimer(sender).Tag of
      1,2: if Assigned(frmLoginDlg) then begin
           frmLoginDlg.Close;
           frmLoginDlg.EnableEntry;
         end;
    end;
  end;
end;

procedure TBasicUserManagement.DoUserChanged;
begin
  if Assigned(FUserChanged) then
    try
      FUserChanged(Self, FCurrentUserLogin, GetCurrentUserLogin);
    finally
    end;
end;

end.
 
