program project3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit3, scada, LResources, RunTimeTypeInfoControls, TAChartLazarusPkg
  { you can add units after this };

{$IFDEF WINDOWS}{$R project3.rc}{$ENDIF}

begin
  {$I project3.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

