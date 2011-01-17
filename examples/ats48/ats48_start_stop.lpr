program ats48_start_stop;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, main, pascalscada, LResources;

{$IFDEF WINDOWS}{$R ats48_start_stop.rc}{$ENDIF}

begin
  {$I ats48_start_stop.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

