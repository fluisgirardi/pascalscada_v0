unit uaddgroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, DBCtrls;

type

  { TfrmAddGroup }

  TfrmAddGroup = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    dsGroups: TDataSource;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
  private

  public

  end;

var
  frmAddGroup: TfrmAddGroup;

implementation

{$R *.lfm}

end.

