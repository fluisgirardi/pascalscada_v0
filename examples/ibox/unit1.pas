unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  SerialPort, IBoxDriver, PLCTagNumber, HMILabel;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMILabel3: THMILabel;
    IBoxDriver1: TIBoxDriver;
    PLCTagNumber1: TPLCTagNumber;
    PLCTagNumber2: TPLCTagNumber;
    PLCTagNumber3: TPLCTagNumber;
    SerialPortDriver1: TSerialPortDriver;
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
  {$I unit1.lrs}

end.

