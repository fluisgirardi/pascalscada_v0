program Pesagens;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Forms, Interfaces,
  uDM in 'uDM.pas' {DM: TDataModule},
  uMain in 'uMain.pas' {Form1},
  uDMImg in 'uDMImg.pas', pascalscada, zcomponent {Imagens: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TImagens, Imagens);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
