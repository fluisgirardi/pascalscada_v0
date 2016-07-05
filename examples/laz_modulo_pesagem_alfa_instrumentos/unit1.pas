unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, SerialPort,
  ModBusSerial, PLCBlock, PLCBlockElement, TagBit, HMILabel;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bloco1: TPLCBlock;
    HMILabel1: THMILabel;
    HMILabel10: THMILabel;
    HMILabel11: THMILabel;
    HMILabel12: THMILabel;
    HMILabel13: THMILabel;
    HMILabel14: THMILabel;
    HMILabel15: THMILabel;
    HMILabel16: THMILabel;
    HMILabel17: THMILabel;
    HMILabel18: THMILabel;
    HMILabel19: THMILabel;
    HMILabel2: THMILabel;
    HMILabel20: THMILabel;
    HMILabel21: THMILabel;
    HMILabel22: THMILabel;
    HMILabel3: THMILabel;
    HMILabel4: THMILabel;
    HMILabel5: THMILabel;
    HMILabel6: THMILabel;
    HMILabel7: THMILabel;
    HMILabel8: THMILabel;
    HMILabel9: THMILabel;
    ModBusRTUDriver1: TModBusRTUDriver;
    SerialPortDriver1: TSerialPortDriver;
    Tag1: TPLCBlockElement;
    Tag1_bit0: TTagBit;
    Tag1_bit1: TTagBit;
    Tag1_bit10: TTagBit;
    Tag1_bit11: TTagBit;
    Tag1_bit12: TTagBit;
    Tag1_bit13: TTagBit;
    Tag1_bit14: TTagBit;
    Tag1_bit15: TTagBit;
    Tag1_bit2: TTagBit;
    Tag1_bit3: TTagBit;
    Tag1_bit4: TTagBit;
    Tag1_bit5: TTagBit;
    Tag1_bit6: TTagBit;
    Tag1_bit7: TTagBit;
    Tag1_bit8: TTagBit;
    Tag1_bit9: TTagBit;
    Tag2: TPLCBlockElement;
    Tag3: TPLCBlockElement;
    Tag4: TPLCBlockElement;
    Tag5: TPLCBlockElement;
    Tag6: TPLCBlockElement;
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

