unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, tcp_udpport, ModBusTCP, PLCBlock, PLCBlockElement, PLCTagNumber,
  LinearScaleProcessor, HMIEdit, HMILabel, HMICheckBox, HMIText;

type
  TForm1 = class(TForm)
    AnalogReg1: TPLCBlockElement;
    AnalogReg2: TPLCBlockElement;
    AnalogReg3: TPLCBlockElement;
    AnalogRegBlk1: TPLCBlock;
    Bloco1: TPLCBlock;
    ckbox_Coil1: THMICheckBox;
    ckbox_Coil2: THMICheckBox;
    ckbox_Coil3: THMICheckBox;
    ckbox_Coil4: THMICheckBox;
    ckbox_Coil5: THMICheckBox;
    Coil0: TPLCBlockElement;
    Coil1: TPLCBlockElement;
    Coil2: TPLCBlockElement;
    Coil3: TPLCBlockElement;
    Coil4: TPLCBlockElement;
    CoilsBlk1: TPLCBlock;
    ckbox_Coil0: THMICheckBox;
    DigitalInput1: TPLCBlockElement;
    DigitalInput2: TPLCBlockElement;
    DigitalInput3: TPLCBlockElement;
    HMIEdit1: THMIEdit;
    HMIEdit10: THMIEdit;
    HMIEdit11: THMIEdit;
    HMIEdit2: THMIEdit;
    HMIEdit3: THMIEdit;
    HMIEdit4: THMIEdit;
    HMIEdit5: THMIEdit;
    HMIEdit6: THMIEdit;
    HMIEdit7: THMIEdit;
    HMIEdit8: THMIEdit;
    HMIEdit9: THMIEdit;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMIText1: THMIText;
    HMIText2: THMIText;
    HMIText3: THMIText;
    InputsBlk1: TPLCBlock;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LinearScaleProcessor1: TLinearScaleProcessor;
    ModBusTCPDriver1: TModBusTCPDriver;
    SingleCoil1: TPLCTagNumber;
    SingleTag1: TPLCTagNumber;
    Tag1: TPLCBlockElement;
    Tag10: TPLCBlockElement;
    Tag2: TPLCBlockElement;
    Tag3: TPLCBlockElement;
    Tag4: TPLCBlockElement;
    Tag5: TPLCBlockElement;
    Tag6: TPLCBlockElement;
    Tag7: TPLCBlockElement;
    Tag8: TPLCBlockElement;
    Tag9: TPLCBlockElement;
    TCP_UDPPort1: TTCP_UDPPort;
    TILabel1: TTILabel;
    TILabel2: TTILabel;
    TILabel3: TTILabel;
    TILabel4: TTILabel;
    TILabel5: TTILabel;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

