unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, mutexserver, MutexClient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TPanel;
    Button2: TPanel;
    MutexClient1: TMutexClient;
    MutexClient2: TMutexClient;
    MutexServer1: TMutexServer;
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
  if Button1.Color=clLime then begin
    MutexClient1.Leave;
    Button1.Color:=clDefault;
  end else begin
    if MutexClient1.TryEnter then begin
      Button1.Color:=clLime;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Button2.Color=clLime then begin
    MutexClient2.Leave;
    Button2.Color:=clDefault;
  end else begin
    if MutexClient2.TryEnter then begin
      Button2.Color:=clLime;
    end;
  end;
end;

end.

