unit upopuphorimetro; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, HMIText, PLCNumber, HMILabel, HMIEdit;

type

  { TfrmPopup }

TfrmPopup = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    HMIEdit1: THMIEdit;
    HMILabel1: THMILabel;
    HMILabel2: THMILabel;
    HMIText1: THMIText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    TagTEMPO_DECORRIDO,
    TagSETPOINT_HORIMETRO,
    TagTEMPO_ACUMULADO,
    TagRESET_HORIMETRO,
    TagBIT_MOTOR_LIGADO,
    TagALARME_RESET_ALARME:TPLCNumber;
  public
    constructor Create(TheOwner: TComponent; pTagTEMPO_DECORRIDO,
                       pTagSETPOINT_HORIMETRO, pTagTEMPO_ACUMULADO, pTagRESET_HORIMETRO,
                       pTagBIT_MOTOR_LIGADO, pTagALARME_RESET_ALARME:TPLCNumber);
  end; 

var
  frmPopup: TfrmPopup;

implementation

{$R *.lfm}

procedure TfrmPopup.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TfrmPopup.BitBtn1Click(Sender: TObject);
begin
  if TagRESET_HORIMETRO<>nil then begin
    TagRESET_HORIMETRO.Value:=1;
    Sleep(50);
    TagRESET_HORIMETRO.Value:=0;
  end;
end;

procedure TfrmPopup.BitBtn2Click(Sender: TObject);
begin
  if TagALARME_RESET_ALARME<>nil then begin
    TagALARME_RESET_ALARME.Value:=1;
    Sleep(50);
    TagALARME_RESET_ALARME.Value:=0;
  end;
end;

procedure TfrmPopup.FormCreate(Sender: TObject);
begin
  HMIText1.PLCTag :=TagBIT_MOTOR_LIGADO;
  HMIEdit1.PLCTag :=TagSETPOINT_HORIMETRO;
  HMILabel1.PLCTag:=TagTEMPO_DECORRIDO;
  HMILabel2.PLCTag:=TagTEMPO_ACUMULADO;
end;

constructor TfrmPopup.Create(TheOwner: TComponent; pTagTEMPO_DECORRIDO,
                   pTagSETPOINT_HORIMETRO, pTagTEMPO_ACUMULADO, pTagRESET_HORIMETRO,
                   pTagBIT_MOTOR_LIGADO, pTagALARME_RESET_ALARME:TPLCNumber);
begin
  inherited Create(TheOwner);
  TagTEMPO_DECORRIDO     := pTagTEMPO_DECORRIDO;
  TagSETPOINT_HORIMETRO  := pTagSETPOINT_HORIMETRO;
  TagTEMPO_ACUMULADO     := pTagTEMPO_ACUMULADO;
  TagRESET_HORIMETRO     := pTagRESET_HORIMETRO;
  TagBIT_MOTOR_LIGADO    := pTagBIT_MOTOR_LIGADO;
  TagALARME_RESET_ALARME := pTagALARME_RESET_ALARME;
end;

end.

