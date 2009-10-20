program Pesagens;

uses
  Forms,
  uDM in 'uDM.pas' {DM: TDataModule},
  uMain in 'uMain.pas' {Form1},
  uDMImg in 'uDMImg.pas' {Imagens: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TImagens, Imagens);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
