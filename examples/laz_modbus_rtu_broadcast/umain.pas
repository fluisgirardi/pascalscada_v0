unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, SerialPort, ModBusSerial, PLCTagNumber;

type

  { TForm1 }

  TForm1 = class(TForm)
    ModBusRTUDriver1: TModBusRTUDriver;
    PLCTagNumber1: TPLCTagNumber;
    SerialPortDriver1: TSerialPortDriver;
    Timer1: TTimer;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    curValue:Integer;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  PLCTagNumber1.Value:=curValue;
  inc(curValue);
end;

end.

