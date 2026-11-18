program laz_opcua_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, //the server works in its own thread
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
