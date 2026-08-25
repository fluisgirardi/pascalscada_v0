unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  RTTIGrids, tcp_udpport, S7PlusFamily, PLCTagNumber, ModBusTCP, HMILabel,
  HMIEdit, HMIRadioGroup, HMIAnimation, HMICheckBox, HMIUpDown, HMIComboBox,
  hmi_draw_basic_vector_control, hmi_draw_elevador, hmi_draw_flow_pump,
  hmi_draw_flow_valve, HMIProgressBar, HMITrackBar, HMIBandeja, HMIFlowImage,
  HMIScrollBar, HMIText, HMIRadioButton, hmi_draw_fita, hmi_draw_redler,
  hmi_draw_rosca, hmi_polyline, HMI_Draw_Valves, HMIBasicEletricMotor,
  HMITransparentButton, HMIControlDislocatorAnimation,
  hmibooleanpropertyconnector, hmicolorpropertyconnector;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMIAnimation1: THMIAnimation;
    HMIBandeja1: THMIBandeja;
    HMIBasicEletricMotor1: THMIBasicEletricMotor;
    HMIBasicValve1: THMIBasicValve;
    HMIBasicVectorControl1: THMIBasicVectorControl;
    HMICheckBox1: THMICheckBox;
    HMIColorPropertyConnector1: THMIColorPropertyConnector;
    HMIComboBox1: THMIComboBox;
    HMIControlDislocatorAnimation2_1: THMIControlDislocatorAnimation2;
    HMIEdit1: THMIEdit;
    HMIElevadorBasico1: THMIElevadorBasico;
    HMIFitaBasica1: THMIFitaBasica;
    HMIFlowImage1: THMIFlowImage;
    HMIFlowPolyline1: THMIFlowPolyline;
    HMIFlowVectorControl1: THMIFlowVectorControl;
    HMIFlowVectorControl2: THMIFlowVectorControl;
    HMIForkedFlowValve1: THMIForkedFlowValve;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILinkedFlowElevator1: THMILinkedFlowElevator;
    HMILinkedFlowPump1: THMILinkedFlowPump;
    HMILinkedFlowPump2: THMILinkedFlowPump;
    HMILinkedFlowValve1: THMILinkedFlowValve;
    HMIPolyline1: THMIPolyline;
    HMIProgressBar1: THMIProgressBar;
    HMIRadioButton1: THMIRadioButton;
    HMIRadioGroup1: THMIRadioGroup;
    HMIRedlerBasico1: THMIRedlerBasico;
    HMIRoscaBasica1: THMIRoscaBasica;
    HMIScrollBar1: THMIScrollBar;
    HMIScrollBar2: THMIScrollBar;
    HMIText1: THMIText;
    HMIText2: THMIText;
    HMIThreeWayFlowValve1: THMIThreeWayFlowValve;
    HMITrackBar1: THMITrackBar;
    HMITransparentButton1: THMITransparentButton;
    HMIUpDown1: THMIUpDown;
    ModBusTCPDriver1: TModBusTCPDriver;
    Panel1: TPanel;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
    StaticText1: TStaticText;
    BrokenTCPConnection: TTCP_UDPPort;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //HMIEdit1.;
end;

end.

