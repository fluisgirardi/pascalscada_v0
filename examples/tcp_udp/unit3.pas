unit Unit3; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, RTTICtrls, TAGraph, TASources, TASeries, tcp_udpport, ModBusTCP,
  PLCTagNumber, HMILabel, HMIUpDown, PLCBlock, ComCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1Line1: TLine;
    Chart1Line2: TLine;
    Chart1LineSeries1: TLineSeries;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMIUpDown1: THMIUpDown;
    ListChartSource1: TListChartSource;
    ModBusTCPDriver1: TModBusTCPDriver;
    PLCBlock1: TPLCBlock;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
    PLCTagNumber3: TPLCTagNumber;
    TCP_UDPPort1: TTCP_UDPPort;
    TILabel1: TTILabel;
    TILabel2: TTILabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure HMIUpDown1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure HMIUpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure PLCTagNumber1ValueChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    x:Integer;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
end;

procedure TForm1.HMIUpDown1Changing(Sender: TObject; var AllowChange: Boolean);
begin

end;

procedure TForm1.HMIUpDown1Click(Sender: TObject; Button: TUDBtnType);
begin

end;

procedure TForm1.PLCTagNumber1ValueChange(Sender: TObject);
begin
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  IF ListChartSource1.Count>180 THEN
    ListChartSource1.Delete(0);
  INC(X);
  ListChartSource1.Add(X,TCP_UDPPort1.CommandsPerSecond,IntToStr(X),clRed);
end;

initialization
  {$I unit3.lrs}

end.

