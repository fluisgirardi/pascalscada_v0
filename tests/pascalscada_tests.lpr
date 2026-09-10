program pascalscada_tests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, consoletestrunner,
  ut.crc16utils,
  ut.hsutils,
  ut.plcmemorymanager;

type
  TPascalSCADATestRunner = class(TTestRunner)
  end;

var
  App: TPascalSCADATestRunner;

begin
  App := TPascalSCADATestRunner.Create(nil);
  App.Initialize;
  App.Title := 'PascalSCADA 0.x - testes automatizados';
  App.Run;
  App.Free;
end.
