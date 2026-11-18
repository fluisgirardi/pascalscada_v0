program laz_opcua_hmi;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, //the driver works in its own threads
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  //the test server is up before the form (and its tags) are loaded: the driver connects
  //on its first try instead of waiting for the reconnect interval
  StartLocalServer;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
  StopLocalServer;
end.
