unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, numexprtag,
  PLCTagNumber, HMITrackBar, HMILabel, hmi_draw_basic_vector_control;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMITrackBar1: THMITrackBar;
    HMITrackBar2: THMITrackBar;
    NumericExprTag1: TNumericExprTag;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

