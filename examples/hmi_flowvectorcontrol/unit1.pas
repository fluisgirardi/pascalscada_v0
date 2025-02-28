unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PLCTagNumber,
  hmi_draw_basic_vector_control, HMITrackBar, hmi_polyline;

type

  { TForm1 }

  TForm1 = class(TForm)
    SaidaSinalVerde: THMIFlowPolyline;
    RuaComFluxo: THMIFlowPolyline;
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

