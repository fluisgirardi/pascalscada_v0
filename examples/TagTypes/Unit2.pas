unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PLCNumber, PLCBlockElement, StdCtrls, HMIEdit, Tag, PLCTag, TagBlock,
  PLCBlock, ProtocolDriver, ModBusDriver, ModBusTCP, CommPort, tcp_udpport;

type
  TForm2 = class(TForm)
    TCP_UDPPort1: TTCP_UDPPort;
    ModBusTCPDriver1: TModBusTCPDriver;
    PLCBlock1: TPLCBlock;
    HMIEdit1: THMIEdit;
    HMIEdit2: THMIEdit;
    PLCBlockElement1: TPLCBlockElement;
    PLCBlockElement2: TPLCBlockElement;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.
