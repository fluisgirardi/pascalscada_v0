unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  SerialPort, WestASCIIDriver, PLCTagNumber, HMILabel, HMIUpDown, HMIEdit,
  HMITrackBar, HMIProgressBar, HMIRadioGroup, HMIScrollBar, HMIAnimation,
  HMIText, StdCtrls, HMICheckBox, HMIRadioButton;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    HMIAnimation1: THMIAnimation;
    HMICheckBox2: THMICheckBox;
    HMIEdit1: THMIEdit;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMIProgressBar1: THMIProgressBar;
    HMIRadioButton1: THMIRadioButton;
    HMIRadioButton2: THMIRadioButton;
    HMIRadioButton3: THMIRadioButton;
    HMIRadioButton4: THMIRadioButton;
    HMIRadioGroup1: THMIRadioGroup;
    HMIScrollBar1: THMIScrollBar;
    HMIText1: THMIText;
    HMITrackBar1: THMITrackBar;
    HMIUpDown1: THMIUpDown;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
    SerialPortDriver1: TSerialPortDriver;
    WestASCIIDriver1: TWestASCIIDriver;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  x:THMICheckBox;
begin
  x:=THMICheckBox.Create(self);
  x.Parent:=Form1;
  x.CaptionTrue := 'teste';
  x.CaptionFalse := 'teste';
  x.CaptionGrayed := 'teste';

end;

initialization
  {$I unit1.lrs}

end.

