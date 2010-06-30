unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  tcp_udpport, ModBusTCP, PLCTagNumber, HMIEdit, HMILabel, PLCBlock,
  PLCBlockElement, PLCString, HMICheckBox, HMITrackBar, HMIProgressBar,
  HMIRadioGroup, HMIUpDown, HMIScrollBar, HMIAnimation, HMIText, PLCStruct,
  PLCStructElement;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    HMIEdit1: THMIEdit;
    HMIEdit10: THMIEdit;
    HMIEdit11: THMIEdit;
    HMIEdit12: THMIEdit;
    HMIEdit13: THMIEdit;
    HMIEdit14: THMIEdit;
    HMIEdit15: THMIEdit;
    HMIEdit2: THMIEdit;
    HMIEdit3: THMIEdit;
    HMIEdit4: THMIEdit;
    HMIEdit5: THMIEdit;
    HMIEdit6: THMIEdit;
    HMIEdit7: THMIEdit;
    HMIEdit8: THMIEdit;
    HMIEdit9: THMIEdit;
    HMILabel1: THMILabel;
    HMILabel10: THMILabel;
    HMILabel11: THMILabel;
    HMILabel12: THMILabel;
    HMILabel13: THMILabel;
    HMILabel14: THMILabel;
    HMILabel15: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMILabel4: THMILabel;
    HMILabel5: THMILabel;
    HMILabel6: THMILabel;
    HMILabel7: THMILabel;
    HMILabel8: THMILabel;
    HMILabel9: THMILabel;
    ModBusTCPDriver1: TModBusTCPDriver;
    PLCBlock1: TPLCBlock;
    PLCBlock2: TPLCBlock;
    PLCBlockElement1: TPLCBlockElement;
    PLCBlockElement2: TPLCBlockElement;
    PLCBlockElement3: TPLCBlockElement;
    PLCBlockElement4: TPLCBlockElement;
    PLCBlockElement5: TPLCBlockElement;
    PLCBlockElement6: TPLCBlockElement;
    PLCBlockElement7: TPLCBlockElement;
    PLCStruct1: TPLCStruct;
    PLCStructItem1: TPLCStructItem;
    PLCStructItem2: TPLCStructItem;
    PLCStructItem3: TPLCStructItem;
    PLCStructItem4: TPLCStructItem;
    PLCStructItem5: TPLCStructItem;
    PLCStructItem6: TPLCStructItem;
    PLCStructItem7: TPLCStructItem;
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

