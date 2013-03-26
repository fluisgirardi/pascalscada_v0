unit client_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MutexClient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    MutexClient1: TMutexClient;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    favoidrecreate:Boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; avoidrecreation:Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if MutexClient1.TryEnter then begin
    Panel1.Visible:=true;
  end else
    ShowMessage('Another application own the mutex!');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MutexClient1.Leave;
  Panel1.Visible:=false;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  x:TForm1;
begin
  if favoidrecreate then exit;

  x:=TForm1.Create(Application, True);
  x.Left:=Left+Width;
  x.Show;

  x:=TForm1.Create(Application, True);
  x.Top:=Top+Height;
  x.Show;

  x:=TForm1.Create(Application, True);
  x.Left:=Left+Width;
  x.Top:=Top+Height;
  x.Show;
end;

constructor TForm1.Create(TheOwner: TComponent; avoidrecreation: Boolean);
begin
  inherited Create(TheOwner);
  favoidrecreate:=avoidrecreation;
end;

end.

