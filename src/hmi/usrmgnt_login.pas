{$i ../common/language.inc}
unit usrmgnt_login;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources,{$ENDIF} Forms, StdCtrls, Buttons, ExtCtrls, Classes;

type

  TFocusedControl = (fcUserName, fcPassword);

  { TfrmUserAuthentication }

  TfrmUserAuthentication = class(TForm)
    edtusername: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtPassword: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn4: TBitBtn;
    BitBtn1: TBitBtn;
    procedure FormShow(Sender: TObject);
  private
    FFocusControl:TFocusedControl;
  public
    { Public declarations }
  published
    property FocusControl:TFocusedControl read FFocusControl write FFocusControl;
  end;

var
  frmUserAuthentication: TfrmUserAuthentication;

implementation

{$IFDEF FPC}
  {$IF defined(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20400) }
    {$R usrmgnt_login.lfm}
  {$IFEND}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

{ TfrmUserAuthentication }

procedure TfrmUserAuthentication.FormShow(Sender: TObject);
begin
  case FFocusControl of
    fcUserName: edtusername.SetFocus;
    fcPassword: edtPassword.SetFocus;
  end;
end;

{$IFDEF FPC}
  {$IF defined(FPC_FULLVERSION) AND (FPC_FULLVERSION < 20400) }
  initialization
  {$i usrmgnt_login.lrs}
  {$IFEND}
{$ENDIF}

end.
