unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  hmi_draw_basic_vector_control, HMITrackBar, PLCTagNumber;

type
  TForm1 = class(TForm)
    HMIFlowVectorControl1: THMIFlowVectorControl;
    HMITrackBar1: THMITrackBar;
    PLCTagNumber1: TPLCTagNumber;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

