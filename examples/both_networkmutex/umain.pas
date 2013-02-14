unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  mutexserver, MutexClient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    MutexClient1: TMutexClient;
    MutexClient2: TMutexClient;
    MutexClient3: TMutexClient;
    MutexServer1: TMutexServer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  //MutexServer1.Active:=true;
  //MutexClient1.Active:=true;
  //MutexClient2.Active:=true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if MutexClient1.TryEnter then
    ShowMessage('Mutex Client 1 own the mutex')
  else
    ShowMessage('The Mutex is being used!');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if MutexClient2.TryEnter then
    ShowMessage('Mutex Client 2 own the mutex')
  else
    ShowMessage('The Mutex is being used!');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  MutexClient1.Leave;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  MutexClient2.Leave;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if MutexClient3.TryEnter then
    ShowMessage('Mutex Client 3 own the mutex')
  else
    ShowMessage('The Mutex is being used!');
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  MutexClient3.Leave;
end;

end.

