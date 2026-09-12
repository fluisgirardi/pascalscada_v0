unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  RTTIGrids, tcp_udpport, S7PlusFamily, PLCTagNumber, ModBusTCP, HMILabel,
  HMIEdit, HMIRadioGroup, HMIAnimation, HMICheckBox, HMIUpDown, HMIComboBox,
  hmi_draw_basic_vector_control, hmi_draw_elevador, hmi_draw_flow_pump,
  hmi_draw_flow_valve, HMIProgressBar, HMITrackBar,
  HMIScrollBar, HMIText, HMIRadioButton, HMIControlDislocatorAnimation,
  hmibooleanpropertyconnector, hmicolorpropertyconnector;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMIAnimation1: THMIAnimation;
    HMICheckBox1: THMICheckBox;
    HMIColorPropertyConnector1: THMIColorPropertyConnector;
    HMIComboBox1: THMIComboBox;
    HMIControlDislocatorAnimation2_1: THMIControlDislocatorAnimation2;
    HMIEdit1: THMIEdit;
    HMIFlowVectorControl1: THMIFlowVectorControl;
    HMIFlowVectorControl2: THMIFlowVectorControl;
    HMIForkedFlowValve1: THMIForkedFlowValve;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMIForkedFlowValve2: THMIForkedFlowValve;
    HMILinkedFlowElevator1: THMILinkedFlowElevator;
    HMILinkedFlowPump1: THMILinkedFlowPump;
    HMILinkedFlowPump2: THMILinkedFlowPump;
    HMILinkedFlowPump3: THMILinkedFlowPump;
    HMILinkedFlowValve1: THMILinkedFlowValve;
    HMILinkedFlowValve2: THMILinkedFlowValve;
    HMILinkedFlowValve3: THMILinkedFlowValve;
    HMILinkedFlowValve4: THMILinkedFlowValve;
    HMIProgressBar1: THMIProgressBar;
    HMIRadioButton1: THMIRadioButton;
    HMIRadioGroup1: THMIRadioGroup;
    HMIScrollBar1: THMIScrollBar;
    HMIScrollBar2: THMIScrollBar;
    HMIText1: THMIText;
    HMIText2: THMIText;
    HMIThreeWayFlowValve1: THMIThreeWayFlowValve;
    HMITrackBar1: THMITrackBar;
    HMIUpDown1: THMIUpDown;
    ModBusTCPDriver1: TModBusTCPDriver;
    Panel1: TPanel;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
    BrokenTCPConnection: TTCP_UDPPort;
    TIPropertyGrid1: TTIPropertyGrid;
    Lbl_HMILabel1: TLabel;
    Lbl_HMILabel2: TLabel;
    Lbl_HMIEdit1: TLabel;
    Lbl_HMIRadioGroup1: TLabel;
    Lbl_HMIAnimation1: TLabel;
    Lbl_HMICheckBox1: TLabel;
    Lbl_HMIRadioButton1: TLabel;
    Lbl_HMIUpDown1: TLabel;
    Lbl_HMIComboBox1: TLabel;
    Lbl_HMIProgressBar1: TLabel;
    Lbl_HMITrackBar1: TLabel;
    Lbl_HMIScrollBar1: TLabel;
    Lbl_HMIScrollBar2: TLabel;
    Lbl_HMIText1: TLabel;
    Lbl_HMIText2: TLabel;
    Lbl_HMILinkedFlowValve1: TLabel;
    Lbl_HMILinkedFlowValve2: TLabel;
    Lbl_HMILinkedFlowValve3: TLabel;
    Lbl_HMILinkedFlowValve4: TLabel;
    Lbl_HMILinkedFlowPump1: TLabel;
    Lbl_HMILinkedFlowPump2: TLabel;
    Lbl_HMILinkedFlowPump3: TLabel;
    Lbl_HMILinkedFlowElevator1: TLabel;
    Lbl_HMIForkedFlowValve1: TLabel;
    Lbl_HMIForkedFlowValve2: TLabel;
    Lbl_HMIThreeWayFlowValve1: TLabel;
    Lbl_HMIFlowVectorControl1: TLabel;
    Lbl_HMIFlowVectorControl2: TLabel;
    Lbl_Panel1: TLabel;
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
  Left:=0;
  //ModBusTCPDriver1.Free;
  BrokenTCPConnection.Free;
end;

end.

