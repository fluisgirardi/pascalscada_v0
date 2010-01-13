program project2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit2, scada, LResources, RunTimeTypeInfoControls
  { you can add units after this };

{$IFDEF WINDOWS}{$R project2.rc}{$ENDIF}

begin
  {$I project2.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

