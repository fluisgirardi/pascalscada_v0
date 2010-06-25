unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  tcp_udpport, ModBusTCP, PLCTagNumber, HMIEdit, HMILabel, PLCBlock,
  PLCBlockElement, PLCString, HMICheckBox, HMITrackBar, HMIProgressBar,
  HMIRadioGroup, HMIUpDown, HMIScrollBar, HMIAnimation, HMIText;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    HMIEdit1: THMIEdit;
    HMIEdit2: THMIEdit;
    HMIEdit3: THMIEdit;
    HMIEdit4: THMIEdit;
    HMIEdit5: THMIEdit;
    HMIEdit6: THMIEdit;
    HMIEdit7: THMIEdit;
    HMIEdit8: THMIEdit;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMILabel4: THMILabel;
    HMILabel5: THMILabel;
    HMILabel6: THMILabel;
    HMILabel7: THMILabel;
    HMILabel8: THMILabel;
    ModBusTCPDriver1: TModBusTCPDriver;
    PLCBlock1: TPLCBlock;
    PLCBlock2: TPLCBlock;
    PLCBlock3: TPLCBlock;
    PLCBlockElement1: TPLCBlockElement;
    PLCBlockElement2: TPLCBlockElement;
    PLCBlockElement3: TPLCBlockElement;
    PLCBlockElement4: TPLCBlockElement;
    PLCBlockElement5: TPLCBlockElement;
    PLCBlockElement6: TPLCBlockElement;
    PLCBlockElement7: TPLCBlockElement;
    PLCTagNumber1: TPLCTagNumber;
    TCP_UDPPort1: TTCP_UDPPort;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

