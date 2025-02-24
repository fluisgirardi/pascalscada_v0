unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, HMIBandeja,
  HMILabel, tcp_udpport, ISOTCPDriver, PLCStruct, PLCStructElement,
  PLCBlockElement, PLCTagNumber, PLCString, plcstructstring;

type

  { TForm1 }

  TForm1 = class(TForm)
    BackgroundTag1: TPLCStructItem;
    BorderTag1: TPLCStructItem;
    HMIBandeja1: THMIBandeja;
    CLPVirtual: TISOTCPDriver;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    Panel1: TPanel;
    PLCStruct1: TPLCStruct;
    CaptionTag: TPLCStructString;
    TCP_UDPPort1: TTCP_UDPPort;
    function HMIBandeja1ConvertDintToColor(Sender: TObject;
      const aColorDint: LongInt; out Lum: LongInt): TColor;

  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


function TForm1.HMIBandeja1ConvertDintToColor(Sender: TObject;
  const aColorDint: LongInt; out Lum: LongInt): TColor;
var
  B, G, R: LongInt;
  Lumin: Int64;
begin
  B:=(aColorDint and $00ff0000) div $10000;
  G:=(aColorDint and $FF000000) div $1000000;
  R:=(aColorDint and $000000ff);
  Lum:=trunc((R*0.3) + (G*0.59) + (B*0.11));
  Result:=RGBToColor(R,G,B);
end;

end.

