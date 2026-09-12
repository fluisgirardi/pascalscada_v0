program pascalscada_tests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, consoletestrunner,
  //apoio aos testes / test support
  testsupport.bytes,
  testsupport.protocol,
  testsupport.fakeport,
  testsupport.faketag,
  //casos de teste / test cases
  ut.crc16utils,
  ut.hsutils,
  ut.plcmemorymanager,
  ut.modbusrtu,
  ut.modbustcp,
  ut.s7plusvlq,
  ut.s7pluscodec,
  ut.s7plustypeinfo,
  ut.s7family,
  ut.protocoldriver,
  ut.plcstring,
  ut.messagespool,
  ut.plcmemorymanager2,
  ut.crossevent,
  ut.commtypes,
  ut.lgxdriver,
  ut.tcp_udpport,
  ut.westasciidriver,
  ut.melsectcp,
  ut.iboxdriver,
  ut.s7familyresposta,
  ut.melsecresposta,
  ut.tagbit;

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
