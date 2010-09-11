program isotcp;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  uDM in 'uDM.pas' {DataModule2: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataModule2, DataModule2);
  Application.Run;
end.
