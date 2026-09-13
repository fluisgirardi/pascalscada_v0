{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TCrossEvent: o evento de sincronizacao entre threads.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' o primitivo que as threads de scan usam para dormir ate ter trabalho.
  Os testes usam tempos folgados de proposito: o CI roda em ARM emulado, onde
  uma espera de 100 ms pode demorar bem mais - a assercao e' sobre o resultado
  da espera, nao sobre o relogio.
}
{$ELSE}
{:
  @abstract(TCrossEvent tests: the inter-thread synchronization event.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This is the primitive the scan threads use to sleep until there is work.
  The tests use generous timings on purpose: CI runs on emulated ARM, where a
  100 ms wait can take a good deal longer - the assertion is about the wait's
  result, not about the clock.
}
{$ENDIF}
unit ut.crossevent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fpcunit, testregistry,
  CrossEvent;

type

  {$IFDEF PORTUGUES}
  //: Thread que sinaliza o evento depois de um tempo.
  {$ELSE}
  //: Thread that signals the event after a while.
  {$ENDIF}

  { TSignaller }

  TSignaller = class(TThread)
  private
    FEvento:TCrossEvent;
    FEspera:Cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create(aEvento:TCrossEvent; aEsperaMs:Cardinal);
  end;

  { TTestCrossEvent }

  TTestCrossEvent = class(TTestCase)
  private
    FEvento:TCrossEvent;
    function  NameOf(aResultado:TWaitResult):String;
  protected
    procedure TearDown; override;
  published
    //evento manual / manual reset event
    procedure AManualEventCreatedSignalledAlreadyReleases;
    procedure AManualEventDoesNotResetItself;
    procedure ResetHoldsTheWaitAgain;
    procedure SignallingLaterReleases;

    //espera sem sinal / waiting with no signal
    procedure WithNoSignalTheWaitTimesOut;
    procedure TheTimeoutDoesNotReturnBeforeTheTimeAskedFor;

    //evento automatico / auto reset event
    procedure AnAutoResetEventResetsItselfAfterReleasing;
    procedure AnAutoResetEventCreatedSignalledSaysSignalled;
    procedure AnAutoResetEventSignalledLaterAlsoSaysSignalled;

    //entre threads / across threads
    procedure ASignalFromAnotherThreadWakesTheWait;

    //retornos / return values
    procedure SetEventAndResetEventAgree;
  end;

implementation

{ TSignaller }

constructor TSignaller.Create(aEvento:TCrossEvent; aEsperaMs:Cardinal);
begin
  FEvento:=aEvento;
  FEspera:=aEsperaMs;
  FreeOnTerminate:=false;
  inherited Create(false);
end;

procedure TSignaller.Execute;
begin
  Sleep(FEspera);
  FEvento.SetEvent;
end;

{ TTestCrossEvent }

procedure TTestCrossEvent.TearDown;
begin
  FreeAndNil(FEvento);
end;

function TTestCrossEvent.NameOf(aResultado:TWaitResult):String;
begin
  case aResultado of
    wrSignaled:  Result:='wrSignaled';
    wrTimeout:   Result:='wrTimeout';
    wrAbandoned: Result:='wrAbandoned';
    wrError:     Result:='wrError';
  else
    Result:='resultado desconhecido';
  end;
end;

procedure TTestCrossEvent.AManualEventCreatedSignalledAlreadyReleases;
begin
  //criado ja sinalizado: quem espera nao espera nada
  FEvento:=TCrossEvent.Create(true, true);
  AssertEquals('waiting on a signalled event', 'wrSignaled', NameOf(FEvento.WaitFor(1000)));
end;

procedure TTestCrossEvent.AManualEventDoesNotResetItself;
begin
  //no evento manual o sinal fica de pe ate alguem resetar
  FEvento:=TCrossEvent.Create(true, true);

  AssertEquals('first wait', 'wrSignaled', NameOf(FEvento.WaitFor(1000)));
  AssertEquals('second wait',  'wrSignaled', NameOf(FEvento.WaitFor(1000)));
end;

procedure TTestCrossEvent.ResetHoldsTheWaitAgain;
begin
  FEvento:=TCrossEvent.Create(true, true);
  FEvento.ResetEvent;

  AssertEquals('after the reset', 'wrTimeout', NameOf(FEvento.WaitFor(200)));
end;

procedure TTestCrossEvent.SignallingLaterReleases;
begin
  //criado sem sinal, sinalizado na sequencia
  FEvento:=TCrossEvent.Create(true, false);
  AssertEquals('before the signal', 'wrTimeout', NameOf(FEvento.WaitFor(100)));

  FEvento.SetEvent;
  AssertEquals('after the signal', 'wrSignaled', NameOf(FEvento.WaitFor(1000)));
end;

procedure TTestCrossEvent.WithNoSignalTheWaitTimesOut;
begin
  FEvento:=TCrossEvent.Create(true, false);
  AssertEquals('with no signal at all', 'wrTimeout', NameOf(FEvento.WaitFor(100)));
end;

procedure TTestCrossEvent.TheTimeoutDoesNotReturnBeforeTheTimeAskedFor;
var
  inicio, decorrido:QWord;
begin
  //o limite de baixo e' o que importa: voltar antes seria espera quebrada.
  //nao ha limite de cima porque o CI roda em ARM emulado.
  FEvento:=TCrossEvent.Create(true, false);

  inicio:=GetTickCount64;
  FEvento.WaitFor(300);
  decorrido:=GetTickCount64-inicio;

  AssertTrue('waited less than asked: '+IntToStr(decorrido)+' ms', decorrido>=250);
end;

procedure TTestCrossEvent.AnAutoResetEventResetsItselfAfterReleasing;
begin
  //no evento automatico o sinal e' consumido por quem esperou
  FEvento:=TCrossEvent.Create(false, true);
  FEvento.WaitFor(1000);

  AssertEquals('the signal was consumed', 'wrTimeout', NameOf(FEvento.WaitFor(200)));
end;

procedure TTestCrossEvent.AnAutoResetEventCreatedSignalledSaysSignalled;
begin
  //criado ja sinalizado: o auto-reset consome o sinal, mas quem esperou tem
  //que saber que foi sinal, e nao erro
  FEvento:=TCrossEvent.Create(false, true);
  AssertEquals('auto reset, signalled', 'wrSignaled', NameOf(FEvento.WaitFor(1000)));
end;

procedure TTestCrossEvent.AnAutoResetEventSignalledLaterAlsoSaysSignalled;
begin
  //e o mesmo pelo caminho do SetEvent, que e' como as threads usam
  FEvento:=TCrossEvent.Create(false, false);
  AssertEquals('before the signal', 'wrTimeout', NameOf(FEvento.WaitFor(100)));

  FEvento.SetEvent;
  AssertEquals('after the signal', 'wrSignaled', NameOf(FEvento.WaitFor(1000)));
  AssertEquals('and the signal was consumed', 'wrTimeout', NameOf(FEvento.WaitFor(200)));
end;

procedure TTestCrossEvent.ASignalFromAnotherThreadWakesTheWait;
var
  sinalizador:TSignaller;
  resultado:TWaitResult;
begin
  //o uso de verdade: uma thread dorme no evento ate outra ter trabalho pra ela
  FEvento:=TCrossEvent.Create(true, false);

  sinalizador:=TSignaller.Create(FEvento, 50);
  try
    resultado:=FEvento.WaitFor(10000);
    AssertEquals('the wait must be woken up', 'wrSignaled', NameOf(resultado));
  finally
    sinalizador.WaitFor;
    sinalizador.Free;
  end;
end;

procedure TTestCrossEvent.SetEventAndResetEventAgree;
begin
  FEvento:=TCrossEvent.Create(true, false);

  AssertTrue('SetEvent',   FEvento.SetEvent);
  AssertTrue('ResetEvent', FEvento.ResetEvent);
end;

initialization
  RegisterTest(TTestCrossEvent);

end.
