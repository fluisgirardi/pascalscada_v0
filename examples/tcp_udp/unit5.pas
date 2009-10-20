unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, tcp_udpport, ModBusTCP, PLCTagNumber, HMILabel,
  HMIScrollBar, HMIUpDown;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    HMIUpDown1: THMIUpDown;
    Label1: TLabel;
    ModBusTCPDriver1: TModBusTCPDriver;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
    PLCTagNumber3: TPLCTagNumber;
    TCP_UDPPort1: TTCP_UDPPort;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label1.Caption:=FormatFloat('#0',TCP_UDPPort1.CommandsPerSecond);
end;

initialization
  {$I unit5.lrs}

end.

