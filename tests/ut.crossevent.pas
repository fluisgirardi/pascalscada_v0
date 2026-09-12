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

  { TSinalizador }

  TSinalizador = class(TThread)
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
    function  NomeDo(aResultado:TWaitResult):String;
  protected
    procedure TearDown; override;
  published
    //evento manual / manual reset event
    procedure ManualCriadoSinalizadoJaLibera;
    procedure ManualNaoSeResetaSozinho;
    procedure ResetVoltaASegurarAEspera;
    procedure SinalizarDepoisLibera;

    //espera sem sinal / waiting with no signal
    procedure SemSinalAEsperaDaTimeout;
    procedure TimeoutNaoRetornaAntesDoTempoPedido;

    //evento automatico / auto reset event
    procedure AutomaticoSeResetaDepoisDeLiberar;
    procedure AutomaticoSinalizadoDizSinalizado;
    procedure AutomaticoSinalizadoDepoisTambemDizSinalizado;

    //entre threads / across threads
    procedure SinalDeOutraThreadAcordaAEspera;

    //retornos / return values
    procedure SetEventEResetEventConfirmam;
  end;

implementation

{ TSinalizador }

constructor TSinalizador.Create(aEvento:TCrossEvent; aEsperaMs:Cardinal);
begin
  FEvento:=aEvento;
  FEspera:=aEsperaMs;
  FreeOnTerminate:=false;
  inherited Create(false);
end;

procedure TSinalizador.Execute;
begin
  Sleep(FEspera);
  FEvento.SetEvent;
end;

{ TTestCrossEvent }

procedure TTestCrossEvent.TearDown;
begin
  FreeAndNil(FEvento);
end;

function TTestCrossEvent.NomeDo(aResultado:TWaitResult):String;
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

procedure TTestCrossEvent.ManualCriadoSinalizadoJaLibera;
begin
  //criado ja sinalizado: quem espera nao espera nada
  FEvento:=TCrossEvent.Create(true, true);
  AssertEquals('espera em evento sinalizado', 'wrSignaled', NomeDo(FEvento.WaitFor(1000)));
end;

procedure TTestCrossEvent.ManualNaoSeResetaSozinho;
begin
  //no evento manual o sinal fica de pe ate alguem resetar
  FEvento:=TCrossEvent.Create(true, true);

  AssertEquals('primeira espera', 'wrSignaled', NomeDo(FEvento.WaitFor(1000)));
  AssertEquals('segunda espera',  'wrSignaled', NomeDo(FEvento.WaitFor(1000)));
end;

procedure TTestCrossEvent.ResetVoltaASegurarAEspera;
begin
  FEvento:=TCrossEvent.Create(true, true);
  FEvento.ResetEvent;

  AssertEquals('depois do reset', 'wrTimeout', NomeDo(FEvento.WaitFor(200)));
end;

procedure TTestCrossEvent.SinalizarDepoisLibera;
begin
  //criado sem sinal, sinalizado na sequencia
  FEvento:=TCrossEvent.Create(true, false);
  AssertEquals('antes do sinal', 'wrTimeout', NomeDo(FEvento.WaitFor(100)));

  FEvento.SetEvent;
  AssertEquals('depois do sinal', 'wrSignaled', NomeDo(FEvento.WaitFor(1000)));
end;

procedure TTestCrossEvent.SemSinalAEsperaDaTimeout;
begin
  FEvento:=TCrossEvent.Create(true, false);
  AssertEquals('sem sinal nenhum', 'wrTimeout', NomeDo(FEvento.WaitFor(100)));
end;

procedure TTestCrossEvent.TimeoutNaoRetornaAntesDoTempoPedido;
var
  inicio, decorrido:QWord;
begin
  //o limite de baixo e' o que importa: voltar antes seria espera quebrada.
  //nao ha limite de cima porque o CI roda em ARM emulado.
  FEvento:=TCrossEvent.Create(true, false);

  inicio:=GetTickCount64;
  FEvento.WaitFor(300);
  decorrido:=GetTickCount64-inicio;

  AssertTrue('esperou menos do que pediu: '+IntToStr(decorrido)+' ms', decorrido>=250);
end;

procedure TTestCrossEvent.AutomaticoSeResetaDepoisDeLiberar;
begin
  //no evento automatico o sinal e' consumido por quem esperou
  FEvento:=TCrossEvent.Create(false, true);
  FEvento.WaitFor(1000);

  AssertEquals('o sinal foi consumido', 'wrTimeout', NomeDo(FEvento.WaitFor(200)));
end;

procedure TTestCrossEvent.AutomaticoSinalizadoDizSinalizado;
begin
  //criado ja sinalizado: o auto-reset consome o sinal, mas quem esperou tem
  //que saber que foi sinal, e nao erro
  FEvento:=TCrossEvent.Create(false, true);
  AssertEquals('automatico sinalizado', 'wrSignaled', NomeDo(FEvento.WaitFor(1000)));
end;

procedure TTestCrossEvent.AutomaticoSinalizadoDepoisTambemDizSinalizado;
begin
  //e o mesmo pelo caminho do SetEvent, que e' como as threads usam
  FEvento:=TCrossEvent.Create(false, false);
  AssertEquals('antes do sinal', 'wrTimeout', NomeDo(FEvento.WaitFor(100)));

  FEvento.SetEvent;
  AssertEquals('depois do sinal', 'wrSignaled', NomeDo(FEvento.WaitFor(1000)));
  AssertEquals('e o sinal foi consumido', 'wrTimeout', NomeDo(FEvento.WaitFor(200)));
end;

procedure TTestCrossEvent.SinalDeOutraThreadAcordaAEspera;
var
  sinalizador:TSinalizador;
  resultado:TWaitResult;
begin
  //o uso de verdade: uma thread dorme no evento ate outra ter trabalho pra ela
  FEvento:=TCrossEvent.Create(true, false);

  sinalizador:=TSinalizador.Create(FEvento, 50);
  try
    resultado:=FEvento.WaitFor(10000);
    AssertEquals('a espera tem que ser acordada', 'wrSignaled', NomeDo(resultado));
  finally
    sinalizador.WaitFor;
    sinalizador.Free;
  end;
end;

procedure TTestCrossEvent.SetEventEResetEventConfirmam;
begin
  FEvento:=TCrossEvent.Create(true, false);

  AssertTrue('SetEvent',   FEvento.SetEvent);
  AssertTrue('ResetEvent', FEvento.ResetEvent);
end;

initialization
  RegisterTest(TTestCrossEvent);

end.
