program pascalscada_tests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, consoletestrunner,
  //apoio aos testes / test support
  testsupport.bytes,
  testsupport.protocol,
  testsupport.fakeport,
  //casos de teste / test cases
  ut.crc16utils,
  ut.hsutils,
  ut.plcmemorymanager,
  ut.modbusrtu,
  ut.modbustcp;

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
