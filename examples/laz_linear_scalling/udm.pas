unit udm; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ValueProcessor, tcp_udpport, ModBusTCP, PLCBlock,
  PLCBlockElement, LinearScaleProcessor;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    LinearScale_1_is_100: TLinearScaleProcessor;
    ModBusTCPDriver1: TModBusTCPDriver;
    Linear1_is_100_on_PLC: TPIPE;
    PLCBlock1: TPLCBlock;
    TCP_UDPPort1: TTCP_UDPPort;
    Word1: TPLCBlockElement;
    Word10: TPLCBlockElement;
    Word4_ws: TPLCBlockElement;
    Word2_ws: TPLCBlockElement;
    Word2: TPLCBlockElement;
    Word3: TPLCBlockElement;
    Word4: TPLCBlockElement;
    Word5: TPLCBlockElement;
    Word6: TPLCBlockElement;
    Word7: TPLCBlockElement;
    Word8: TPLCBlockElement;
    Word9: TPLCBlockElement;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DataModule1: TDataModule1; 

implementation

{$R *.lfm}

end.

