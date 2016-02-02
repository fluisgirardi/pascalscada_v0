unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  tcp_udpport, ISOTCPDriver, PLCBlock, PLCBlockElement, PLCTagNumber, HMILabel,
  HMIUpDown;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMIUpDown1: THMIUpDown;
    HMIUpDown2: THMIUpDown;
    ISOTCPDriver1: TISOTCPDriver;
    Label1: TLabel;
    TCP_UDPPort1: TTCP_UDPPort;
    VW2: TPLCTagNumber;
    MW0: TPLCTagNumber;
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

