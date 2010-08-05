unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, RTTIGrids, tcp_udpport, ISOTCPDriver, PLCBlock,
  PLCBlockElement, HMIEdit, HMIText, LinearScaleProcessor,
  HMIControlDislocatorAnimation, HMIAnimation, sdl;

type

  { TForm1 }

  TForm1 = class(TForm)
    HMIControlDislocatorAnimation1: THMIControlDislocatorAnimation;
    HMIEdit1: THMIEdit;
    HMIEdit10: THMIEdit;
    HMIEdit11: THMIEdit;
    HMIEdit12: THMIEdit;
    HMIEdit13: THMIEdit;
    HMIEdit14: THMIEdit;
    HMIEdit15: THMIEdit;
    HMIEdit16: THMIEdit;
    HMIEdit17: THMIEdit;
    HMIEdit18: THMIEdit;
    HMIEdit19: THMIEdit;
    HMIEdit2: THMIEdit;
    HMIEdit20: THMIEdit;
    HMIEdit21: THMIEdit;
    HMIEdit22: THMIEdit;
    HMIEdit23: THMIEdit;
    HMIEdit24: THMIEdit;
    HMIEdit25: THMIEdit;
    HMIEdit26: THMIEdit;
    HMIEdit27: THMIEdit;
    HMIEdit28: THMIEdit;
    HMIEdit29: THMIEdit;
    HMIEdit3: THMIEdit;
    HMIEdit30: THMIEdit;
    HMIEdit4: THMIEdit;
    HMIEdit5: THMIEdit;
    HMIEdit6: THMIEdit;
    HMIEdit7: THMIEdit;
    HMIEdit8: THMIEdit;
    HMIEdit9: THMIEdit;
    HMIText1: THMIText;
    HMIText10: THMIText;
    HMIText11: THMIText;
    HMIText12: THMIText;
    HMIText13: THMIText;
    HMIText14: THMIText;
    HMIText15: THMIText;
    HMIText16: THMIText;
    HMIText2: THMIText;
    HMIText3: THMIText;
    HMIText4: THMIText;
    HMIText5: THMIText;
    HMIText6: THMIText;
    HMIText7: THMIText;
    HMIText8: THMIText;
    HMIText9: THMIText;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    Timer1: TTimer;
    TIPropertyGrid1: TTIPropertyGrid;
    ToggleBox1: TToggleBox;
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses dm;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SDL_Init(SDL_INIT_EVERYTHING);
end;

procedure TForm1.RadioButton1Change(Sender: TObject);
begin

end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  if not (Sender is TCheckBox) then exit;
  with Sender as TCheckBox do
    case Tag of
      0:  TIPropertyGrid1.TIObject:=DataModule1.DB1;
      1:  TIPropertyGrid1.TIObject:=DataModule1.MD0_MD40;
      2:  TIPropertyGrid1.TIObject:=DataModule1.Counters;
      3:  TIPropertyGrid1.TIObject:=DataModule1.InputsBYTE_01;
      4:  TIPropertyGrid1.TIObject:=DataModule1.OutputsBYTE_01;
    end;
end;

procedure TForm1.RadioButton2Change(Sender: TObject);
begin

end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label26.Caption:=IntToStr(DataModule1.TCP_UDPPort1.CommandsPerSecond);
  TIPropertyGrid1.RefreshPropertyValues;
end;

initialization
  {$I unit1.lrs}

end.

