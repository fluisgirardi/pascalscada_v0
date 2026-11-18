program laz_pscadalogger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, //the logger and the burst work in their own threads
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
