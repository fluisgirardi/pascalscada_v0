program laz_opcua_client;

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
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
