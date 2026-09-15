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
  Forms,
  Classes, SysUtils, consoletestrunner,
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
  ut.hmipolyline,
  ut.vectorcontrol,
  ut.flowvalve,
  ut.flowelevator,
  ut.numericcontrols,
  ut.hmiedit,
  ut.hmibutton,
  ut.hmicombobox,
  ut.smallcontrols,
  ut.hmiflowimage,
  ut.horizontalcontrols,
  ut.animationtimers;

type
  TPascalSCADAHMITestRunner = class(TTestRunner)
  end;

var
  App: TPascalSCADAHMITestRunner;

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

  //os controles comuns do Windows - barra deslizante, incrementador, barra de
  //progresso - so' existem depois que o widgetset chama InitCommonControlsEx,
  //e quem chama isso e' o TWin32WidgetSet.AppInit, a partir do
  //Forms.Application.Initialize. O runner de console e' um TCustomApplication
  //e nunca passava por ali: criar um TTrackBar de verdade falhava com
  //"Cannot find window class" no CI do Windows e "Classe inexistente" no wine.
  //Sem efeito no gtk2, onde o AppInit nao registra classe nenhuma.
  //Windows' common controls - track bar, up/down, progress bar - only exist
  //after the widgetset calls InitCommonControlsEx, and what calls it is
  //TWin32WidgetSet.AppInit, reached from Forms.Application.Initialize. The
  //console runner is a TCustomApplication and never went through it: creating
  //a real TTrackBar failed with "Cannot find window class" on the Windows CI
  //and "Classe inexistente" under wine. No effect on gtk2, where AppInit
  //registers no classes.
  Forms.Application.Initialize;

  App := TPascalSCADAHMITestRunner.Create(nil);
  App.Initialize;
  App.Title := 'PascalSCADA 0.x - testes automatizados da camada HMI';
  App.Run;
  App.Free;
end.
