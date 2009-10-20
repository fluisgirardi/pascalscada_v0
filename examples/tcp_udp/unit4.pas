unit Unit4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, RTTIGrids, TAGraph, TASources, TASeries, tcp_udpport,
  ModBusTCP, PLCTagNumber, PLCBlock, HMILabel, HMIUpDown, HMIScrollBar, HMIEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1Line1: TLine;
    Chart1Line2: TLine;
    Chart1LineSeries1: TLineSeries;
    HMIEdit1: THMIEdit;
    HMIEdit2: THMIEdit;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMIScrollBar1: THMIScrollBar;
    HMIUpDown1: THMIUpDown;
    Label1: TLabel;
    ListChartSource1: TListChartSource;
    ModBusTCPDriver1: TModBusTCPDriver;
    Panel1: TPanel;
    PLCBlock1: TPLCBlock;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
    PLCTagNumber3: TPLCTagNumber;
    TCP_UDPPort1: TTCP_UDPPort;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    px:Integer;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses LCLProc;

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if ListChartSource1.Count>300 then
    ListChartSource1.Delete(0);
  inc(px);
  ListChartSource1.Add(px,TCP_UDPPort1.CommandsPerSecond,IntToStr(px), clGreen);
  Label1.Caption:=FormatFloat('#0',TCP_UDPPort1.CommandsPerSecond);
end;

initialization
  {$I unit4.lrs}

end.

