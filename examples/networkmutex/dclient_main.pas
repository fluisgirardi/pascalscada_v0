unit dclient_main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MutexClient;

type
  TForm2 = class(TForm)
    MutexClient1: TMutexClient;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  if MutexClient1.TryEnter then
    ShowMessage('This application own the mutex!')
  else
    ShowMessage('Another application own the mutex!');
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  MutexClient1.Leave
end;

end.
