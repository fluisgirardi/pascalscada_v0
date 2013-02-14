unit uMain;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ImgList, HMIAnimation, Tag, PLCTag, PLCNumber,
  PLCTagNumber, StdCtrls, Grids, DBGrids, Buttons,
  {$IFDEF FPC}LResources,{$ELSE} jpeg, {$ENDIF} HMILabel, HMIText, uDM;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMIAnimation1: THMIAnimation;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image7: TImage;
    Image8: TImage;
    HMIAnimation2: THMIAnimation;
    HMIAnimation3: THMIAnimation;
    Label1: TLabel;
    HMILabel1: THMILabel;
    Label2: TLabel;
    HMILabel2: THMILabel;
    Label3: TLabel;
    HMILabel3: THMILabel;
    Timer1: TTimer;
    DBGrid1: TDBGrid;
    HMIText1: THMIText;
    HMIText2: THMIText;
    HMIText3: THMIText;
    SpeedButton1: TSpeedButton;
    Image9: TImage;
    HMIAnimation4: THMIAnimation;
    Timer2: TTimer;
    Label4: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses uDMImg, hsutils{, ShellAPI};

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false//MessageDlg('Deseja mesmo sair da aplicação?',mtConfirmation,[mbYes,mbNo],0)=mrYes;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  //ShellExecute(0,PAnsiChar('open'),PAnsiChar('http://localhost:18080'),PAnsiChar(''),PAnsiChar(''),SW_MAXIMIZE)
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Image4.Height :=(73*(1000-FloatToInteger(dm.PesoBalanca.Value))) div 1000;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Label4.Caption:=FormatFloat('#0',DM.TCP_UDPPort1.CommandsPerSecond);
end;
{$IFDEF FPC}
initialization
  {$i uMain.lrs}
{$ENDIF}
end.


