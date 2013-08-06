unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, SerialPort, ModBusSerial, PLCTagNumber;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    ModBusRTUDriver1: TModBusRTUDriver;
    Panel1: TPanel;
    Panel2: TPanel;
    SerialPortDriver1: TSerialPortDriver;
    startAddress: TSpinEdit;
    endAddress: TSpinEdit;
    Tag1: TPLCTagNumber;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses typinfo, Tag;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  node: Integer;
  function NodeStatus:String;
  begin
    //codes returned by TModBusRTUDriver.DecodePkg function.
    case Tag1.LastSyncReadStatus of
      ioIllegalFunction,
      ioIllegalRegAddress,
      ioIllegalValue,
      ioPLCError,
      ioOk:
        Result:='Station UP!';
      ioTimeOut,
      ioDriverError,
      ioCommError:
        Result:='Station down...';
      ioNone:
        Result:='Serial port is configured?';
    end;
  end;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss',Now)+' Starting Modbus RTU network scan...');
  for node:=startAddress.Value to endAddress.Value do begin
    Tag1.PLCStation:=node;
    Tag1.Read;
    Memo1.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss',Now)+
                    ', Address: '+FormatFloat('000', node)+
                    ', LastSyncReadStatus='+GetEnumName(TypeInfo(Tag1.LastSyncReadStatus), Integer(Tag1.LastSyncReadStatus))+
                    ' Diagnostic: '+NodeStatus);
  end;
end;

end.

