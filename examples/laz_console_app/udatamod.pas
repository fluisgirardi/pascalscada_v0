unit udatamod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, tcp_udpport, ModBusTCP, PLCBlock,
  PLCBlockElement;

type
  TDataModule1 = class(TDataModule)
    ModBusTCPDriver1: TModBusTCPDriver;
    PLCBlock1: TPLCBlock;
    PLCBlock1_e0: TPLCBlockElement;
    PLCBlock1_e1: TPLCBlockElement;
    PLCBlock1_e2: TPLCBlockElement;
    PLCBlock1_e3: TPLCBlockElement;
    PLCBlock1_e4: TPLCBlockElement;
    PLCBlock1_e5: TPLCBlockElement;
    PLCBlock1_e6: TPLCBlockElement;
    PLCBlock1_e7: TPLCBlockElement;
    PLCBlock1_e8: TPLCBlockElement;
    PLCBlock1_e9: TPLCBlockElement;
    TCP_UDPPort1: TTCP_UDPPort;
  private

  public

  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

end.

