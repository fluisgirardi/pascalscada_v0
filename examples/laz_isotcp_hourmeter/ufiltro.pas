unit ufiltro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, Buttons;

type

  { TfrmFiltro }

TfrmFiltro = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    DateFinal: TDateEdit;
    DateInicial: TDateEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmFiltro: TfrmFiltro;

implementation

{$R *.lfm}

{ TfrmFiltro }

procedure TfrmFiltro.FormShow(Sender: TObject);
begin
  DateInicial.Date:=Now;
  DateFinal.Date:=now;
end;

end.

