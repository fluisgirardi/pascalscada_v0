unit uDM;

interface

uses
  SysUtils, Classes, PLCNumber, PLCBlockElement, Tag, PLCTag, TagBlock,
  PLCBlock, ProtocolDriver, s7family, ISOTCPDriver, CommPort, tcp_udpport,
  ExtCtrls, TagBit, PLCTagNumber;

type
  TDataModule2 = class(TDataModule)
    TCP_UDPPort1: TTCP_UDPPort;
    ISOTCPDriver1: TISOTCPDriver;
    DB1: TPLCBlock;
    MD0_MD40: TPLCBlock;
    DB1_DBD00: TPLCBlockElement;
    DB1_DBD04: TPLCBlockElement;
    DB1_DBD08: TPLCBlockElement;
    DB1_DBD12: TPLCBlockElement;
    DB1_DBD16: TPLCBlockElement;
    DB1_DBD20: TPLCBlockElement;
    DB1_DBD24: TPLCBlockElement;
    DB1_DBD28: TPLCBlockElement;
    DB1_DBD32: TPLCBlockElement;
    DB1_DBD36: TPLCBlockElement;
    MD00: TPLCBlockElement;
    MD04: TPLCBlockElement;
    MD08: TPLCBlockElement;
    MD12: TPLCBlockElement;
    MD16: TPLCBlockElement;
    MD20: TPLCBlockElement;
    MD24: TPLCBlockElement;
    MD28: TPLCBlockElement;
    MD32: TPLCBlockElement;
    MD36: TPLCBlockElement;
    InputsBYTE_01: TPLCTagNumber;
    I0_0: TTagBit;
    I0_1: TTagBit;
    I0_2: TTagBit;
    I0_3: TTagBit;
    I0_4: TTagBit;
    I0_5: TTagBit;
    I0_6: TTagBit;
    I0_7: TTagBit;
    OutputsBYTE_01: TPLCTagNumber;
    Q0_0: TTagBit;
    Q0_1: TTagBit;
    Q0_2: TTagBit;
    Q0_3: TTagBit;
    Q0_4: TTagBit;
    Q0_5: TTagBit;
    Q0_6: TTagBit;
    Q0_7: TTagBit;
    Counters: TPLCBlock;
    C0: TPLCBlockElement;
    C1: TPLCBlockElement;
    C2: TPLCBlockElement;
    C3: TPLCBlockElement;
    C4: TPLCBlockElement;
    C5: TPLCBlockElement;
    C6: TPLCBlockElement;
    C7: TPLCBlockElement;
    C8: TPLCBlockElement;
    C9: TPLCBlockElement;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule2: TDataModule2;

implementation

{$R *.dfm}

procedure TDataModule2.Timer1Timer(Sender: TObject);
begin
  DB1.Read;
  DB1_DBD16.Value:=DB1_DBD16.Value+3;
  DB1_DBD24.Value:=DB1_DBD24.Value+5;
  DB1.WriteDirect;
end;

end.
