unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, PropEdits,
  SerialPort;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    SerialPortDriver1: TSerialPortDriver;
    procedure Button1Click(Sender: TObject);
  private
    procedure AdicionaNoCombo(const S: string);

  public

  end;

var
  Form1: TForm1;

implementation

uses scadapropeditor;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  x:TPortPropertyEditor;
  y: TPropertyEditorHook;
begin
  ComboBox1.Clear;
  try
    y:=TPropertyEditorHook.Create(SerialPortDriver1);
    try
      x:=TPortPropertyEditor.Create(y,0);
      x.GetValues(@AdicionaNoCombo);
    finally
      x.Free;
    end;
  finally
    y.Free;
  end;
end;

procedure TForm1.AdicionaNoCombo(const S: string);
begin
  ComboBox1.AddItem(s,nil);
end;

end.

