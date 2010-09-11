unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HMIEdit, ExtCtrls, HMIControlDislocatorAnimation,
  HMILabel, HMIText;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    HMIEdit1: THMIEdit;
    Label3: TLabel;
    HMIEdit2: THMIEdit;
    HMIEdit3: THMIEdit;
    HMIEdit4: THMIEdit;
    HMIEdit5: THMIEdit;
    HMIEdit6: THMIEdit;
    HMIEdit7: THMIEdit;
    HMIEdit8: THMIEdit;
    HMIEdit9: THMIEdit;
    HMIEdit10: THMIEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    HMIEdit11: THMIEdit;
    Label24: TLabel;
    HMIEdit12: THMIEdit;
    HMIEdit13: THMIEdit;
    HMIEdit14: THMIEdit;
    HMIEdit15: THMIEdit;
    HMIEdit16: THMIEdit;
    HMIEdit17: THMIEdit;
    HMIEdit18: THMIEdit;
    HMIEdit19: THMIEdit;
    HMIEdit20: THMIEdit;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    HMIText1: THMIText;
    HMIText2: THMIText;
    HMIText3: THMIText;
    HMIText4: THMIText;
    HMIText5: THMIText;
    HMIText6: THMIText;
    HMIText7: THMIText;
    HMIText8: THMIText;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    HMIText9: THMIText;
    HMIText10: THMIText;
    HMIText11: THMIText;
    HMIText12: THMIText;
    HMIText13: THMIText;
    HMIText14: THMIText;
    HMIText15: THMIText;
    HMIText16: THMIText;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
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
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    HMIEdit21: THMIEdit;
    Label54: TLabel;
    HMIEdit22: THMIEdit;
    HMIEdit23: THMIEdit;
    HMIEdit24: THMIEdit;
    HMIEdit25: THMIEdit;
    HMIEdit26: THMIEdit;
    HMIEdit27: THMIEdit;
    HMIEdit28: THMIEdit;
    HMIEdit29: THMIEdit;
    HMIEdit30: THMIEdit;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Timer1: TTimer;
    HMIControlDislocatorAnimation1: THMIControlDislocatorAnimation;
    Button1: TButton;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses uDM;

{$R *.dfm}

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label26.Caption := IntToStr(DataModule2.TCP_UDPPort1.CommandsPerSecond);
end;

end.
