unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  tcp_udpport, ISOTCPDriver, PLCTagNumber, TagBit, HMILabel;

type

  { TForm1 }

  TForm1 = class(TForm)
    DB1_DBB0: TPLCTagNumber;
    DB1_DBB0_bit0: TTagBit;
    DB1_DBB0_bit1: TTagBit;
    DB1_DBB0_bit2: TTagBit;
    DB1_DBB0_bit3: TTagBit;
    DB1_DBB0_bit4: TTagBit;
    DB1_DBB0_bit5: TTagBit;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMILabel4: THMILabel;
    HMILabel5: THMILabel;
    HMILabel6: THMILabel;
    ISOTCPDriver1: TISOTCPDriver;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TCP_UDPPort1: TTCP_UDPPort;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

