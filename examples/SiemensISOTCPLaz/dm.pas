unit dm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Dialogs,
  tcp_udpport, ISOTCPDriver, PLCBlock, PLCBlockElement, PLCTagNumber, TagBit, commtypes;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    DB1: TPLCBlock;
    I0_1: TTagBit;
    Counters: TPLCBlock;
    C0: TPLCBlockElement;
    C3: TPLCBlockElement;
    C4: TPLCBlockElement;
    C5: TPLCBlockElement;
    C6: TPLCBlockElement;
    C7: TPLCBlockElement;
    C8: TPLCBlockElement;
    C9: TPLCBlockElement;
    C1: TPLCBlockElement;
    C2: TPLCBlockElement;
    Q0_2: TTagBit;
    Q0_3: TTagBit;
    Q0_4: TTagBit;
    Q0_5: TTagBit;
    Q0_6: TTagBit;
    Q0_7: TTagBit;
    I0_2: TTagBit;
    I0_3: TTagBit;
    I0_4: TTagBit;
    I0_5: TTagBit;
    I0_6: TTagBit;
    I0_7: TTagBit;
    Q0_0: TTagBit;
    Q0_1: TTagBit;
    OutputsBYTE_01: TPLCTagNumber;
    MD00: TPLCBlockElement;
    MD12: TPLCBlockElement;
    MD16: TPLCBlockElement;
    MD20: TPLCBlockElement;
    MD24: TPLCBlockElement;
    DB1_DBD28: TPLCBlockElement;
    MD28: TPLCBlockElement;
    DB1_DBD32: TPLCBlockElement;
    MD32: TPLCBlockElement;
    DB1_DBD36: TPLCBlockElement;
    DB1_DBD04: TPLCBlockElement;
    DB1_DBD08: TPLCBlockElement;
    DB1_DBD12: TPLCBlockElement;
    DB1_DBD16: TPLCBlockElement;
    DB1_DBD20: TPLCBlockElement;
    DB1_DBD24: TPLCBlockElement;
    MD36: TPLCBlockElement;
    MD04: TPLCBlockElement;
    MD08: TPLCBlockElement;
    ISOTCPDriver1: TISOTCPDriver;
    MD0_MD40: TPLCBlock;
    DB1_DBD00: TPLCBlockElement;
    InputsBYTE_01: TPLCTagNumber;
    I0_0: TTagBit;
    TCP_UDPPort1: TTCP_UDPPort;
    procedure CountersValueChange(Sender: TObject);
    procedure TCP_UDPPort1CommPortOpenError(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DataModule1: TDataModule1; 

implementation

uses Unit1;

{ TDataModule1 }

procedure TDataModule1.TCP_UDPPort1CommPortOpenError(Sender: TObject);
begin
  Form1.Label1.Caption:='lascou...';
end;

procedure TDataModule1.CountersValueChange(Sender: TObject);
begin

end;

initialization
  {$I dm.lrs}

end.

