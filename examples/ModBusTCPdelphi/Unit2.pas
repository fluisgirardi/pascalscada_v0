unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CommPort, tcp_udpport, PLCNumber, PLCBlockElement, Tag, PLCTag,
  TagBlock, PLCBlock, ComCtrls, HMIUpDown, StdCtrls, HMILabel, ExtCtrls,
  ProtocolDriver, ModBusDriver, ModBusTCP, HMIScrollBar, HMIEdit, TeEngine,
  Series, TeeProcs, Chart;

type
  TForm2 = class(TForm)
    TCP_UDPPort1: TTCP_UDPPort;
    ModBusTCPDriver1: TModBusTCPDriver;
    Panel1: TPanel;
    HMILabel1: THMILabel;
    HMIUpDown1: THMIUpDown;
    PLCBlock1: TPLCBlock;
    PLCBlock2: TPLCBlock;
    PLCBlock3: TPLCBlock;
    PLCBlock4: TPLCBlock;
    PLCBlock5: TPLCBlock;
    PLCBlock6: TPLCBlock;
    PLCBlock7: TPLCBlock;
    PLCBlock8: TPLCBlock;
    PLCBlock9: TPLCBlock;
    PLCBlock10: TPLCBlock;
    PLCBlock11: TPLCBlock;
    PLCBlock12: TPLCBlock;
    PLCBlock13: TPLCBlock;
    PLCBlock14: TPLCBlock;
    PLCBlock15: TPLCBlock;
    PLCBlock16: TPLCBlock;
    PLCBlock17: TPLCBlock;
    PLCBlock18: TPLCBlock;
    PLCBlock19: TPLCBlock;
    PLCBlock20: TPLCBlock;
    PLCBlockElement1: TPLCBlockElement;
    PLCBlockElement2: TPLCBlockElement;
    PLCBlockElement3: TPLCBlockElement;
    HMILabel2: THMILabel;
    HMIEdit1: THMIEdit;
    HMILabel3: THMILabel;
    HMIEdit2: THMIEdit;
    HMIScrollBar1: THMIScrollBar;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Chart1: TChart;
    Series1: TFastLineSeries;
    procedure Timer1Timer(Sender: TObject);
  private
    point:Integer;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  label1.Caption := FormatFloat('#0',TCP_UDPPort1.CommandsPerSecond);
  label2.Caption := 'RX: '+FormatFloat('#0.0',TCP_UDPPort1.RXBytesSecond/1024)+' KB/s';
  label3.Caption := 'TX: '+FormatFloat('#0.0',TCP_UDPPort1.TXBytesSecond/1024)+' KB/s';
  inc(point);
  Series1.AddXY(point,TCP_UDPPort1.CommandsPerSecond);
  if Series1.Count>300 then
    Series1.Delete(0);
end;

end.
