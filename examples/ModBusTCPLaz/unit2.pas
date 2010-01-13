unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  RTTICtrls, tcp_udpport, ModBusMasterDriver, PLCTagNumber;

type

  { TForm1 }

  TForm1 = class(TForm)
    ModBusMasterDriver1: TModBusMasterDriver;
    PLCTagNumber1: TPLCTagNumber;
    TCP_UDPPort1: TTCP_UDPPort;
    TICheckBox1: TTICheckBox;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
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

initialization
  {$I unit2.lrs}

end.

