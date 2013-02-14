unit client_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MutexClient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    MutexClient1: TMutexClient;
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

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if MutexClient1.TryEnter then
    ShowMessage('This application own the mutex!')
  else
    ShowMessage('Another application own the mutex!');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MutexClient1.Leave;
end;

end.

