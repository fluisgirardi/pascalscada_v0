unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, tcp_udpport, ModBusTCP, PLCTagNumber, HMILabel, PLCBlock;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
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
    HMILabel3: THMILabel;
    HMILabel4: THMILabel;
    HMILabel5: THMILabel;
    HMILabel6: THMILabel;
    HMILabel7: THMILabel;
    HMILabel8: THMILabel;
    HMILabel9: THMILabel;
    ModBusTCPDriver1: TModBusTCPDriver;
    PLCBlock1: TPLCBlock;
    Tag1: TPLCTagNumber;
    Tag10: TPLCTagNumber;
    Tag11: TPLCTagNumber;
    Tag12: TPLCTagNumber;
    Tag13: TPLCTagNumber;
    Tag14: TPLCTagNumber;
    Tag15: TPLCTagNumber;
    Tag16: TPLCTagNumber;
    Tag17: TPLCTagNumber;
    Tag18: TPLCTagNumber;
    Tag19: TPLCTagNumber;
    Tag2: TPLCTagNumber;
    Tag20: TPLCTagNumber;
    Tag3: TPLCTagNumber;
    Tag4: TPLCTagNumber;
    Tag5: TPLCTagNumber;
    Tag6: TPLCTagNumber;
    Tag7: TPLCTagNumber;
    Tag8: TPLCTagNumber;
    Tag9: TPLCTagNumber;
    TCP_UDPPort1: TTCP_UDPPort;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    mult:integer;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  x:array[0..29] of double;
  c:Integer;
begin
  for c:=0 to 29 do
    x[c] := c*mult;

  inc(mult);

  PLCBlock1.ValuesRaw:=x;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  mult:=1;
end;

end.

