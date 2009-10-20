unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  tcp_udpport, WestASCIIDriver, PLCTagNumber, ModBusMasterDriver;

type

  { TForm1 }

  TForm1 = class(TForm)
    ModBusMasterDriver1: TModBusMasterDriver;
    PLCTagNumber1: TPLCTagNumber;
    TCP_UDPPort1: TTCP_UDPPort;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

initialization
  {$I unit1.lrs}

end.

