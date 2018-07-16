unit comptagedt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, Spin;

type
  TfrmTComponentTagEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Edit1: TEdit;
    procedure Edit1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    fValue: PtrInt;
  public
    property Value:PtrInt read fValue;
  end;

var
  frmTComponentTagEditor: TfrmTComponentTagEditor;

implementation

{$R *.lfm}


procedure TfrmTComponentTagEditor.Edit1Change(Sender: TObject);
begin
  {$IFDEF CPU64}
  ButtonPanel1.OKButton.Enabled:=TryStrToInt64(Edit1.Text,fValue);
  {$ELSE}
  ButtonPanel1.OKButton.Enabled:=TryStrToInt(Edit1.Text,fValue);
  {$ENDIF}
end;

procedure TfrmTComponentTagEditor.FormActivate(Sender: TObject);
begin
  Edit1.SetFocus;
end;

end.

