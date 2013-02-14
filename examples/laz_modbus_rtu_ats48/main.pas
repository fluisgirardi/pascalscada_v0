unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  SerialPort, ModBusSerial, PLCTagNumber, HMILabel, StdCtrls, hsutils;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    HMILabel1: THMILabel;
    Memo1: TMemo;
    ModBusMasterDriver1: TModBusRTUDriver;
    ATS48CMD: TPLCTagNumber;
    ATS48Status: TPLCTagNumber;
    SerialPortDriver1: TSerialPortDriver;
    procedure ATS48StatusValueChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    PowerOn, First:boolean;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ATS48StatusValueChange(Sender: TObject);
begin
  if Sender is TPLCTagNumber then
    with Sender as TPLCTagNumber do
       Memo1.Lines.Add(Name+'='+FormatFloat('#0',trunc(Value) and $FF));

  if (Sender is TPLCTagNumber) and (TPLCTagNumber(Sender).Name='ATS48Status') then begin
    if First then begin
      if TPLCTagNumber(Sender).Value=$27 then
        PowerOn:=true;
      First:=false;
    end;
    if PowerOn then
      with TPLCTagNumber(Sender) do begin
        case Trunc(Value) and $FF of
          $60:
            ATS48CMD.Value:=$06;
          $21:
            ATS48CMD.Value:=$07;
          $23:
            ATS48CMD.Value:=$0F;
        end;
      end
    else
      with TPLCTagNumber(Sender) do begin
        case Trunc(Value) and $FF of
          $27:
            ATS48CMD.Value:=$07;
          $23:
            ATS48CMD.Value:=$06;
          $21:
            ATS48CMD.Value:=$00;
        end;
      end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PowerOn:=true;
  ATS48CMD.Value:=$80;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  PowerOn:=false;
  ATS48CMD.Value:=$80;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  First:=false;
  MessageDlg('This example demonstrate a simple ModBus RTU that '+LineEnding+
             'turn on or turn off a motor linked with a Telemecanique'+LineEnding+
             'Altistart 48 (Softstarter).'+LineEnding+LineEnding+
             'Please setup a valid serial port before get a functional'+LineEnding+
             'example. All components are stored on form1.',
             mtInformation,[mbok],0);
end;

initialization
  {$I main.lrs}

end.

