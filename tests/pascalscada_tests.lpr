program pascalscada_tests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, consoletestrunner,
  //apoio aos testes / test support
  testsupport.bytes,
  testsupport.protocol,
  testsupport.fakeport,
  testsupport.faketag,
  testsupport.fakedriver,
  testsupport.fakeserver,
  testsupport.fakeserial,
  //casos de teste / test cases
  ut.crc16utils,
  ut.hsutils,
  ut.propvalueparsing,
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
  ut.tagbit,
  ut.commport,
  ut.tag,
  ut.valueprocessor,
  ut.crossthreads,
  ut.serialport,
  ut.plcblock,
  ut.plctagnumber,
  ut.numexprtag,
  ut.escalas,
  ut.protscanupdate,
  ut.plcstruct,
  ut.mutex,
  ut.tagcollection,
  ut.isotcp,
  ut.scanthread,
  ut.socketserver,
  ut.mtpcpu;

type
  TPascalSCADATestRunner = class(TTestRunner)
  end;

var
  App: TPascalSCADATestRunner;

begin
  //os testes conferem texto formatado (FormatFloat, FloatToStr) contra valores
  //escritos com ponto. No Windows o FPC le a configuracao regional da maquina,
  //entao numa maquina configurada em pt-BR o mesmo teste que passa no CI - que
  //roda en-US - falharia so' pela virgula. O separador fica fixo aqui.
  //the tests check formatted text (FormatFloat, FloatToStr) against values
  //written with a dot. On Windows FPC reads the machine's regional settings, so
  //on a machine set to pt-BR the very test that passes on CI - which runs en-US
  //- would fail over the comma alone. The separator is pinned here.
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.ThousandSeparator:=',';

  App := TPascalSCADATestRunner.Create(nil);
  App.Initialize;
  App.Title := 'PascalSCADA 0.x - testes automatizados';
  App.Run;
  App.Free;
end.
