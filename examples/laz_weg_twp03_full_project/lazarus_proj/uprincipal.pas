unit uPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics,
  Dialogs, StdCtrls,  ExtCtrls,  DbCtrls, uDM, HMIEdit,HMICheckBox;

type

  { TfPrincipal }

  TfPrincipal = class(TForm)
    HMICheckBox1: THMICheckBox;
    HMICheckBox10: THMICheckBox;
    HMICheckBox2: THMICheckBox;
    HMICheckBox3: THMICheckBox;
    HMICheckBox4: THMICheckBox;
    HMICheckBox5: THMICheckBox;
    HMICheckBox6: THMICheckBox;
    HMICheckBox7: THMICheckBox;
    HMICheckBox8: THMICheckBox;
    HMICheckBox9: THMICheckBox;
    HMIEdit1: THMIEdit;
    HMIEdit10: THMIEdit;
    HMIEdit11: THMIEdit;
    HMIEdit12: THMIEdit;
    HMIEdit13: THMIEdit;
    HMIEdit2: THMIEdit;
    HMIEdit3: THMIEdit;
    HMIEdit4: THMIEdit;
    HMIEdit5: THMIEdit;
    HMIEdit6: THMIEdit;
    HMIEdit7: THMIEdit;
    HMIEdit8: THMIEdit;
    HMIEdit9: THMIEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    RefreshTTI: TTimer;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure RefreshTTITimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fPrincipal: TfPrincipal;

implementation

{$R *.lfm}

{ TfPrincipal }

procedure TfPrincipal.RefreshTTITimer(Sender: TObject);
begin
  TIPropertyGrid1.RefreshPropertyValues;
  DM.D8436.Read;
  DM.C251.Read;
  DM.Saidas.Read;
  DM.Entradas.Read;
  DM.D8013.Read;
end;


end.

