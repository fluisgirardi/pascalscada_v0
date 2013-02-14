unit urelatorios;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, RLReport,
  RLRichFilter, RLHTMLFilter, RLPDFFilter, RLXLSFilter, RLSaveDialog, LR_Class,
  LR_DBSet, udmdb;

type

  { TfrmRelatorios }

TfrmRelatorios = class(TForm)
    RLBand1: TRLBand;
    RLBand2: TRLBand;
    RLDBText1: TRLDBText;
    RLDBText2: TRLDBText;
    RLDBText3: TRLDBText;
    RLDBText4: TRLDBText;
    RLHTMLFilter1: TRLHTMLFilter;
    RLLabel1: TRLLabel;
    RLLabel2: TRLLabel;
    RLLabel3: TRLLabel;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLPDFFilter1: TRLPDFFilter;
    RLReport1: TRLReport;
    RLRichFilter1: TRLRichFilter;
    RLXLSFilter1: TRLXLSFilter;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmRelatorios: TfrmRelatorios;

implementation

{$R *.lfm}

{ TfrmRelatorios }

procedure TfrmRelatorios.FormCreate(Sender: TObject);
begin

end;

end.

