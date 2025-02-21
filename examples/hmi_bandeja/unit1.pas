unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, HMIBandeja, HMILabel,
  tcp_udpport, ISOTCPDriver, PLCStruct, PLCStructElement, PLCBlockElement,
  PLCTagNumber, PLCString, plcstructstring;

type

  { TForm1 }

  TForm1 = class(TForm)
    BackgroundTag1: TPLCStructItem;
    BorderTag1: TPLCStructItem;
    HMIBandeja1: THMIBandeja;
    CLPVirtual: TISOTCPDriver;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    PLCStruct1: TPLCStruct;
    CaptionTag: TPLCStructString;
    TCP_UDPPort1: TTCP_UDPPort;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

