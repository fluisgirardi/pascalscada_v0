unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SerialPort;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    SerialPortDriver1: TSerialPortDriver;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses IniFiles;



{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  inifile:TIniFile;
begin
  inifile:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'serialsettings.ini');
  try
    //properties below must be saved on a config file.
    case SerialPortDriver1.BaudRate of
      br110:
        inifile.WriteInteger('Serial','Baudrate',0);
      br300:
        inifile.WriteInteger('Serial','Baudrate',1);
      br600:
        inifile.WriteInteger('Serial','Baudrate',2);
      br1200:
        inifile.WriteInteger('Serial','Baudrate',3);
      br2400:
        inifile.WriteInteger('Serial','Baudrate',4) ;
      br4800:
        inifile.WriteInteger('Serial','Baudrate',5);
      br9600:
        inifile.WriteInteger('Serial','Baudrate',6);
      br19200:
        inifile.WriteInteger('Serial','Baudrate',7);
      br38400:
        inifile.WriteInteger('Serial','Baudrate',8);
      br57600:
        inifile.WriteInteger('Serial','Baudrate',9);
      br115200:
        inifile.WriteInteger('Serial','Baudrate',10);
    end;

    case SerialPortDriver1.DataBits of
      db5:
          inifile.WriteInteger('Serial','DataBits',5);
      db6:
          inifile.WriteInteger('Serial','DataBits',6);
      db7:
          inifile.WriteInteger('Serial','DataBits',7);
      db8:
          inifile.WriteInteger('Serial','DataBits',8);
    end;

    case SerialPortDriver1.StopBits of
      sb1:
          inifile.WriteInteger('Serial','StopBits',1);
      sb2:
          inifile.WriteInteger('Serial','StopBits',2);
    end;

    case SerialPortDriver1.Paridade of
      spNone:
          inifile.WriteInteger('Serial','Parity',0);
      spOdd:
          inifile.WriteInteger('Serial','Parity',1);
      spEven:
          inifile.WriteInteger('Serial','Parity',2);
    end;

    inifile.WriteInteger('Serial','Timeout',SerialPortDriver1.Timeout);
    inifile.WriteInteger('Serial','WriteReadDelay',SerialPortDriver1.WriteReadDelay);
    inifile.WriteInteger('Serial','WriteRetries',SerialPortDriver1.WriteRetries);
    inifile.WriteInteger('Serial','ReadRetries',SerialPortDriver1.ReadRetries);
    inifile.WriteBool   ('Serial','ClearBuffersOnCommErrors',SerialPortDriver1.ClearBuffersOnCommErrors);
    inifile.WriteString ('Serial','COMPort',SerialPortDriver1.COMPort);
    inifile.WriteBool   ('Serial','Active',SerialPortDriver1.Active);
  finally
    inifile.Destroy;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  inifile:TIniFile;
begin
  inifile:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'serialsettings.ini');
  try
    case inifile.ReadInteger('Serial','Baudrate', 7) of //if the config is invalid or not exists, loads 7 as default value (19200 bps)
     0:  SerialPortDriver1.BaudRate := br110 ;
     1:  SerialPortDriver1.BaudRate := br300 ;
     2:  SerialPortDriver1.BaudRate := br600 ;
     3:  SerialPortDriver1.BaudRate := br1200 ;
     4:  SerialPortDriver1.BaudRate := br2400 ;
     5:  SerialPortDriver1.BaudRate := br4800 ;
     6:  SerialPortDriver1.BaudRate := br9600 ;
     //7 will be treated on else.
     8:  SerialPortDriver1.BaudRate := br38400 ;
     9:  SerialPortDriver1.BaudRate := br57600 ;
     10: SerialPortDriver1.BaudRate := br115200 ;
     else
         SerialPortDriver1.BaudRate := br19200 ;
    end;

    case inifile.ReadInteger('Serial','DataBits',8) of //if the config is invalid or not exists, loads 8 as default value (8 data bits)
      5: SerialPortDriver1.DataBits := db5;
      6: SerialPortDriver1.DataBits := db6;
      7: SerialPortDriver1.DataBits := db7;
      else
         SerialPortDriver1.DataBits := db8;
    end;

    case inifile.ReadInteger('Serial','StopBits',1) of
      2: SerialPortDriver1.StopBits := sb2;
      else
         SerialPortDriver1.StopBits := sb1;
    end;

    case inifile.ReadInteger('Serial','Parity',0) of
      1: SerialPortDriver1.Paridade := spOdd;
      2: SerialPortDriver1.Paridade := spEven;
      else
         SerialPortDriver1.Paridade := spNone;
    end;

    SerialPortDriver1.Timeout                 :=inifile.ReadInteger('Serial','Timeout',                 100);
    SerialPortDriver1.WriteReadDelay          :=inifile.ReadInteger('Serial','WriteReadDelay',          20);
    SerialPortDriver1.WriteRetries            :=inifile.ReadInteger('Serial','WriteRetries',            3);
    SerialPortDriver1.ReadRetries             :=inifile.ReadInteger('Serial','ReadRetries',             3);
    SerialPortDriver1.ClearBuffersOnCommErrors:=inifile.ReadBool   ('Serial','ClearBuffersOnCommErrors',true);
    SerialPortDriver1.COMPort                 :=inifile.ReadString ('Serial','COMPort',                 '');
    SerialPortDriver1.Active                  :=inifile.ReadBool   ('Serial','Active',                  false); //this must be always the last line when restoring serial port properties.
  finally
    inifile.Destroy;
  end;
end;

end.

