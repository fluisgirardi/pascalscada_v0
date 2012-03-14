unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, HMILabel, PLCTagNumber, PLCBlock;

type

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    HMILabel1: THMILabel;
    HMILabel10: THMILabel;
    HMILabel11: THMILabel;
    HMILabel12: THMILabel;
    HMILabel13: THMILabel;
    HMILabel14: THMILabel;
    HMILabel15: THMILabel;
    HMILabel16: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMILabel4: THMILabel;
    HMILabel5: THMILabel;
    HMILabel6: THMILabel;
    HMILabel7: THMILabel;
    HMILabel8: THMILabel;
    HMILabel9: THMILabel;
    Timer1: TTimer;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses Unit2;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  c:Integer;
begin
  ComboBox1.items.Clear;
  for c:=0 to DataModule1.ComponentCount-1 do begin
    if (DataModule1.Components[c] is TPLCTagNumber) or (DataModule1.Components[c] is TPLCBlock) then
      ComboBox1.items.AddObject(DataModule1.Components[c].Name, DataModule1.Components[c]);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  TIPropertyGrid1.RefreshPropertyValues;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex in [0..(ComboBox1.Items.Count-1)] then
    TIPropertyGrid1.TIObject:=TComponent(ComboBox1.Items.Objects[ComboBox1.ItemIndex]);
end;

end.

