program horimetros;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, lazreport, umain, udm, pascalscada, zcomponent,
  udmdb, upopuphorimetro, ugraficos, ufiltro,
  ugerenciamentousuarios;

{$R *.res}

begin
  Application.Title:='Monitoramento e controle de horimetros';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tdmdb, dmdb);
  Application.CreateForm(Tdmtags, dmtags);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

