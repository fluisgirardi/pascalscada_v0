program pascalscada_live_tests;

{$mode objfpc}{$H+}

{: Testes AO VIVO do driver S7CommPlus (precisam de um PLC / PLCSim com os DBs de tests/plc).
   Veja lt.s7plus.pas: S7PLUS_HOST, S7PLUS_PORT, S7PLUS_USER, S7PLUS_PASSWORD.
   LIVE tests of the S7CommPlus driver (they need a PLC / PLCSim with the DBs of tests/plc).
   See lt.s7plus.pas: S7PLUS_HOST, S7PLUS_PORT, S7PLUS_USER, S7PLUS_PASSWORD. }

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, consoletestrunner,
  lt.s7plus;

type
  TPascalSCADALiveRunner = class(TTestRunner)
  end;

var
  App: TPascalSCADALiveRunner;

begin
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.ThousandSeparator:=',';

  App := TPascalSCADALiveRunner.Create(nil);
  App.Initialize;
  App.Title := 'PascalSCADA 0.x - testes ao vivo (S7CommPlus)';
  App.Run;
  App.Free;
end.
