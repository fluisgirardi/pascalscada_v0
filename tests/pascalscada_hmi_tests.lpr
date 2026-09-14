program pascalscada_hmi_tests;

{$mode objfpc}{$H+}

//A camada de HMI vive num binario separado porque arrasta o BGRABitmap junto,
//e so o job "desktop" do CI o instala. Os jobs de 32 bits, ARM e FreeBSD
//continuam rodando pascalscada_tests, que nao depende de nada grafico.
//
//The HMI layer lives in a separate binary because it drags BGRABitmap along,
//and only the CI "desktop" job installs it. The 32 bit, ARM and FreeBSD jobs
//keep running pascalscada_tests, which depends on nothing graphical.

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  //Interfaces puxa a implementacao do widgetset; sem ela o LCL fica sem os
  //WSRegister* e o binario nao liga.
  //Interfaces pulls in the widgetset implementation; without it the LCL is
  //left with no WSRegister* and the binary does not link.
  Interfaces,
  Classes, consoletestrunner,
  //casos de teste / test cases
  //apoio aos testes / test support
  testsupport.faketag,
  //casos de teste / test cases
  ut.usermanagement,
  ut.hmizones,
  ut.propertyconnector,
  ut.colorconnector,
  ut.commfaultbadge,
  ut.dislocator,
  ut.eventlogger,
  ut.alarmlogger,
  ut.tagchartsource,
  ut.hmicheckbox,
  ut.hmilabel,
  ut.hmitext,
  ut.hmianimation,
  ut.hmiprogressbar,
  ut.hmibasiccontrol,
  ut.hmipolyline;

type
  TPascalSCADAHMITestRunner = class(TTestRunner)
  end;

var
  App: TPascalSCADAHMITestRunner;

begin
  App := TPascalSCADAHMITestRunner.Create(nil);
  App.Initialize;
  App.Title := 'PascalSCADA 0.x - testes automatizados da camada HMI';
  App.Run;
  App.Free;
end.
