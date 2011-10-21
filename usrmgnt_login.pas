{$i language.inc}
unit usrmgnt_login;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources,{$ENDIF} Forms, StdCtrls, Buttons, ExtCtrls;

type
  TfrmUserAuthentication = class(TForm)
    edtusername: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtPassword: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn4: TBitBtn;
    BitBtn1: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUserAuthentication: TfrmUserAuthentication;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
    {$R usrmgnt_login.lfm}
  {$ELSE}
  initialization
  {$i usrmgnt_login.lrs}
  {$IFEND}
{$ENDIF}

end.
