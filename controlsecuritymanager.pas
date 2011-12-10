unit ControlSecurityManager;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, sysutils, HMITypes;

type
  TControlSecurityManager = class(TComponent)
  private
    FControls:array of IHMIInterface;
    FUserManagement:TComponent;

  protected
    { Protected declarations }
  public
    destructor Destroy; override;
    procedure  RegisterControl(control:IHMIInterface);
    procedure  UnRegisterControl(control:IHMIInterface);
    procedure  UpdateControls;
    function   CanAccess(sc:String):Boolean;
    procedure  ValidateSecurityCode(sc:String);
    procedure  RegisterSecurityCode(sc:String);
    procedure  UnregisterSecurityCode(sc:String);
    function   SecurityCodeExists(sc:String):Boolean;
  published
    property UserManagement:TComponent read FUserManagement write FUserManagement;
  end;

    function GetControlSecurityManager:TControlSecurityManager;

implementation

uses BasicUserManagement, hsstrings, Dialogs, Controls;

destructor TControlSecurityManager.Destroy;
begin
  if Length(FControls)>0 then
    raise Exception.Create(SSecurityControlBusy);
  inherited Destroy;
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
  for c:=0 to Length(FControls) do
    FControls[c].CanBeAccessed(CanAccess(FControls[c].GetControlSecurityCode));
end;

function   TControlSecurityManager.CanAccess(sc:String):Boolean;
begin
  Result:=true;
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
    case MessageDlg('O codigo de segurança ainda está sendo usado por alguns controles, deseja remover o a ligação com eles?',mtConfirmation,mbYesNoCancel,0) of
      mrYes:
        for c:=0 to Length(FControls) do
          if FControls[c].GetControlSecurityCode=sc then
            FControls[c].MakeUnsecure;
      mrNo:
        raise Exception.Create('Remova o codigo dos controles que o estão usando!');
      mrCancel:
        exit;
    end;
  end;

  TBasicUserManagement(FUserManagement).UnregisterSecurityCode(sc);
end;

function   TControlSecurityManager.SecurityCodeExists(sc:String):Boolean;
begin
  Result:=false;
  if FUserManagement<>nil then
    Result:=TBasicUserManagement(FUserManagement).SecurityCodeExists(sc);
end;

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