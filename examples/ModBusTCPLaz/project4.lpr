program project4;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, scada, Unit4, LResources, TAChartLazarusPkg
  { you can add units after this };

{$IFDEF WINDOWS}{$R project4.rc}{$ENDIF}

begin
  {$I project4.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

