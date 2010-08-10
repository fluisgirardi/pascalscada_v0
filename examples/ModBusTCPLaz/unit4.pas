unit Unit4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, RTTIGrids, TAGraph, TASources, TASeries, tcp_udpport,
  ModBusTCP, PLCTagNumber, PLCBlock, HMILabel, HMIUpDown, HMIScrollBar, HMIEdit,
  PLCBlockElement, sdl;

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
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListChartSource1: TListChartSource;
    ModBusTCPDriver1: TModBusTCPDriver;
    Panel1: TPanel;
    PLCBlock1: TPLCBlock;
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
    PLCBlock2: TPLCBlock;
    PLCBlock20: TPLCBlock;
    PLCBlock3: TPLCBlock;
    PLCBlock4: TPLCBlock;
    PLCBlock5: TPLCBlock;
    PLCBlock6: TPLCBlock;
    PLCBlock7: TPLCBlock;
    PLCBlock8: TPLCBlock;
    PLCBlock9: TPLCBlock;
    PLCBlockElement1: TPLCBlockElement;
    PLCBlockElement2: TPLCBlockElement;
    PLCBlockElement3: TPLCBlockElement;
    TCP_UDPPort1: TTCP_UDPPort;
    Timer1: TTimer;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure FormCreate(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    px:Integer;
    cmdmedios, cmdcount:Integer;
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
  Inc(cmdmedios, TCP_UDPPort1.CommandsPerSecond);
  inc(cmdcount);
  Label1.Caption:=FormatFloat('#0',TCP_UDPPort1.CommandsPerSecond);
  Label2.Caption:='TX: '+FormatFloat('#0.0',TCP_UDPPort1.TXBytesSecond/1024)+' kb/s';
  Label3.Caption:='RX: '+FormatFloat('#0.0',TCP_UDPPort1.RXBytesSecond/1024)+' kb/s';
  Label4.Caption:=FormatFloat('#0',cmdmedios/cmdcount);
  TIPropertyGrid1.RefreshPropertyValues;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SDL_Init(SDL_INIT_EVERYTHING);
end;

initialization
  {$I unit4.lrs}

end.

