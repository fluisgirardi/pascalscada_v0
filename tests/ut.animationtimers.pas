{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do relogio compartilhado das animacoes: um temporizador por
            intervalo, com a lista de quem quer ser avisado.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Uma tela com trinta alarmes piscando nao pode ter trinta temporizadores: o
  gerenciador mantem um por intervalo e chama, a cada batida, todos os
  controles cadastrados naquele intervalo. Piscar junto e' consequencia disso
  - dois controles no mesmo intervalo piscam no mesmo compasso.

  O que se testa aqui e' a lista: quem entra, quem sai, e - principalmente -
  o que acontece quando alguem sai de dentro da propria chamada, que e'
  exatamente o que um controle faz quando para de piscar.
}
{$ELSE}
{:
  @abstract(Tests of the shared animation clock: one timer per interval, with
            the list of whoever wants to be told.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A screen with thirty blinking alarms cannot have thirty timers: the manager
  keeps one per interval and calls, on every tick, every control registered on
  that interval. Blinking in step is a consequence - two controls on the same
  interval blink on the same beat.

  What is tested here is the list: who comes in, who goes out and - above all -
  what happens when someone leaves from inside their own call, which is exactly
  what a control does when it stops blinking.
}
{$ENDIF}
unit ut.animationtimers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fpcunit, testregistry,
  hmi_animation_timers;

type

  { TBlinkerForTest }

  //um objeto qualquer com dois metodos de aviso, para testar a remocao por
  //objeto
  //any object with two notification methods, to test removal by object
  TBlinkerForTest = class(TObject)
  public
    First, Second:LongInt;
    procedure FirstCallback(Sender:TObject);
    procedure SecondCallback(Sender:TObject);
  end;

  { TTestAnimationTimers }

  TTestAnimationTimers = class(TTestCase)
  private
    FCountA, FCountB:LongInt;
    FSelfRemoving:LongInt;
    FBoom:LongInt;
    procedure CallbackA(Sender:TObject);
    procedure CallbackB(Sender:TObject);
    procedure CallbackThatRemovesItself(Sender:TObject);
    procedure CallbackThatRemovesTheOther(Sender:TObject);
    procedure CallbackThatBlowsUp(Sender:TObject);
    //: espera ate' o contador chegar ao valor pedido, drenando a fila
    //: waits until the counter reaches the asked value, draining the queue
    function  WaitUntil(const counter:PLongInt; wanted:LongInt):Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a lista / the list
    procedure ACallbackIsCalledOnEveryTick;
    procedure TwoCallbacksOnTheSameIntervalAreBothCalled;
    procedure TheSameCallbackTwiceIsCalledOnlyOnce;

    //quem sai / who leaves
    procedure RemovingACallbackStopsIt;
    procedure RemovingByIntervalOnlyTouchesThatInterval;
    procedure RemovingEveryCallbackOfAnObjectStopsThemAll;

    //o temporizador em si / the timer itself
    procedure AnIntervalWithNobodyLeftDropsItsTimer;
    procedure DroppingEveryCallbackOfAnObjectAlsoDropsTheTimer;

    //os casos dificeis / the hard cases
    procedure ACallbackThatRemovesItselfDoesNotBreakTheOthers;
    procedure ACallbackRemovedByAnotherIsNotCalledInTheSameRound;
    procedure AnExceptionInOneCallbackDoesNotStopTheOthers;
  end;

implementation

const
  //intervalos proprios de cada teste, para um teste nao herdar a lista do
  //outro: o gerenciador e' unico no processo
  //an interval of its own for each test, so one test does not inherit the
  //other's list: the manager is a single instance in the process
  UM_TICK        = 31;
  DOIS_TICKS     = 32;
  MESMO_DUAS     = 33;
  SAI            = 34;
  SAI_POR_INTERVALO_A = 35;
  SAI_POR_INTERVALO_B = 36;
  SAI_POR_OBJETO = 37;
  SAI_DE_DENTRO  = 38;
  EXPLODE        = 39;
  SAI_NO_MEIO    = 40;
  SEM_NINGUEM    = 41;
  SEM_NINGUEM2   = 42;

{ TBlinkerForTest }

procedure TBlinkerForTest.FirstCallback(Sender:TObject);
begin
  inc(First);
end;

procedure TBlinkerForTest.SecondCallback(Sender:TObject);
begin
  inc(Second);
end;

{ TTestAnimationTimers }

procedure TTestAnimationTimers.SetUp;
begin
  FCountA:=0;
  FCountB:=0;
  FSelfRemoving:=0;
  FBoom:=0;
end;

procedure TTestAnimationTimers.TearDown;
begin
  //o gerenciador vive no processo inteiro: o que este teste cadastrou tem que
  //sair, ou continua batendo durante os proximos
  //the manager lives for the whole process: whatever this test registered has
  //to go, or it keeps ticking during the next ones
  GetAnimationTimer.RemoveCallbacksFromObject(Self);
end;

procedure TTestAnimationTimers.CallbackA(Sender:TObject);
begin
  inc(FCountA);
end;

procedure TTestAnimationTimers.CallbackB(Sender:TObject);
begin
  inc(FCountB);
end;

procedure TTestAnimationTimers.CallbackThatRemovesItself(Sender:TObject);
begin
  inc(FSelfRemoving);
  //e' o que um controle faz quando para de piscar
  //it is what a control does when it stops blinking
  GetAnimationTimer.RemoveCallback(@CallbackThatRemovesItself);
end;

procedure TTestAnimationTimers.CallbackThatRemovesTheOther(Sender:TObject);
begin
  inc(FSelfRemoving);
  //um controle que solta o pisca de outro de dentro da propria chamada - e' o
  //que acontece quando um controle e' destruido durante a batida
  //a control that drops another's blink from inside its own call - which is
  //what happens when a control is destroyed during the tick
  GetAnimationTimer.RemoveCallback(@CallbackB);
end;

procedure TTestAnimationTimers.CallbackThatBlowsUp(Sender:TObject);
begin
  inc(FBoom);
  raise Exception.Create('o aviso de um controle quebrou');
end;

function TTestAnimationTimers.WaitUntil(const counter:PLongInt; wanted:LongInt):Boolean;
var
  limite:QWord;
begin
  limite:=GetTickCount64+3000;
  while (counter^<wanted) and (GetTickCount64<limite) do begin
    Application.ProcessMessages;
    Sleep(2);
  end;
  Result:=counter^>=wanted;
end;

procedure TTestAnimationTimers.ACallbackIsCalledOnEveryTick;
begin
  GetAnimationTimer.AddTimerCallback(UM_TICK, @CallbackA);

  AssertTrue('bateu a primeira vez', WaitUntil(@FCountA, 1));
  AssertTrue('e continua batendo',   WaitUntil(@FCountA, 3));
end;

procedure TTestAnimationTimers.TwoCallbacksOnTheSameIntervalAreBothCalled;
begin
  //um temporizador so', dois avisados: e' o que faz dois controles piscarem
  //no mesmo compasso
  //a single timer, two listeners: it is what makes two controls blink on the
  //same beat
  GetAnimationTimer.AddTimerCallback(DOIS_TICKS, @CallbackA);
  GetAnimationTimer.AddTimerCallback(DOIS_TICKS, @CallbackB);

  AssertTrue('o primeiro bateu', WaitUntil(@FCountA, 2));
  AssertTrue('o segundo tambem', WaitUntil(@FCountB, 2));
end;

procedure TTestAnimationTimers.TheSameCallbackTwiceIsCalledOnlyOnce;
begin
  //cadastrar duas vezes nao pode dobrar o ritmo de quem pisca
  //registering twice must not double the beat of whoever blinks
  GetAnimationTimer.AddTimerCallback(MESMO_DUAS, @CallbackA);
  GetAnimationTimer.AddTimerCallback(MESMO_DUAS, @CallbackA);
  GetAnimationTimer.AddTimerCallback(MESMO_DUAS, @CallbackB);

  AssertTrue('bateu o bastante para comparar', WaitUntil(@FCountB, 5));

  AssertTrue('no mesmo ritmo do outro, e nao no dobro', FCountA<=FCountB+1);
end;

procedure TTestAnimationTimers.RemovingACallbackStopsIt;
var
  ondeParou:LongInt;
begin
  GetAnimationTimer.AddTimerCallback(SAI, @CallbackA);
  GetAnimationTimer.AddTimerCallback(SAI, @CallbackB);
  AssertTrue('comecou a bater', WaitUntil(@FCountA, 2));

  GetAnimationTimer.RemoveCallback(@CallbackA);
  ondeParou:=FCountA;

  AssertTrue('o outro continua', WaitUntil(@FCountB, FCountB+3));
  AssertEquals('e o removido parou', ondeParou, FCountA);
end;

procedure TTestAnimationTimers.RemovingByIntervalOnlyTouchesThatInterval;
var
  ondeParou:LongInt;
begin
  GetAnimationTimer.AddTimerCallback(SAI_POR_INTERVALO_A, @CallbackA);
  GetAnimationTimer.AddTimerCallback(SAI_POR_INTERVALO_B, @CallbackA);
  GetAnimationTimer.AddTimerCallback(SAI_POR_INTERVALO_B, @CallbackB);
  AssertTrue('comecou a bater', WaitUntil(@FCountB, 2));

  GetAnimationTimer.RemoveTimerCallback(SAI_POR_INTERVALO_A, @CallbackA);
  ondeParou:=FCountA;

  AssertTrue('o outro intervalo continua', WaitUntil(@FCountB, FCountB+3));
  AssertTrue('e o cadastro do outro intervalo tambem', FCountA>ondeParou);
end;

procedure TTestAnimationTimers.RemovingEveryCallbackOfAnObjectStopsThemAll;
var
  piscador:TBlinkerForTest;
  paradoEm, paradoEm2:LongInt;
begin
  //e' o que o destrutor de um controle faz: solta tudo que ele cadastrou
  //it is what a control's destructor does: drops everything it registered
  piscador:=TBlinkerForTest.Create;
  try
    GetAnimationTimer.AddTimerCallback(SAI_POR_OBJETO, @piscador.FirstCallback);
    GetAnimationTimer.AddTimerCallback(SAI_POR_OBJETO, @piscador.SecondCallback);
    GetAnimationTimer.AddTimerCallback(SAI_POR_OBJETO, @CallbackA);
    AssertTrue('os dois bateram', WaitUntil(@piscador.Second, 2));

    GetAnimationTimer.RemoveCallbacksFromObject(piscador);
    paradoEm :=piscador.First;
    paradoEm2:=piscador.Second;

    AssertTrue('o de fora continua', WaitUntil(@FCountA, FCountA+3));
    AssertEquals('o primeiro parou', paradoEm,  piscador.First);
    AssertEquals('o segundo tambem', paradoEm2, piscador.Second);
  finally
    GetAnimationTimer.RemoveCallbacksFromObject(piscador);
    piscador.Free;
  end;
end;

procedure TTestAnimationTimers.AnIntervalWithNobodyLeftDropsItsTimer;
var
  antes:Integer;
begin
  //um temporizador sem ninguem para avisar acorda o programa a cada batida
  //para percorrer uma lista vazia, ate' o fim do processo
  //a timer with nobody to tell wakes the program on every tick to walk an
  //empty list, until the process ends
  antes:=GetAnimationTimer.TimerCount;

  GetAnimationTimer.AddTimerCallback(SEM_NINGUEM, @CallbackA);
  AssertEquals('um temporizador a mais', antes+1, GetAnimationTimer.TimerCount);

  GetAnimationTimer.RemoveCallback(@CallbackA);

  AssertEquals('e ele sai quando fica sem ninguem', antes, GetAnimationTimer.TimerCount);
end;

procedure TTestAnimationTimers.DroppingEveryCallbackOfAnObjectAlsoDropsTheTimer;
var
  piscador:TBlinkerForTest;
  antes:Integer;
begin
  //e' o caminho do destrutor de um controle
  //it is the path a control's destructor takes
  piscador:=TBlinkerForTest.Create;
  try
    antes:=GetAnimationTimer.TimerCount;
    GetAnimationTimer.AddTimerCallback(SEM_NINGUEM2, @piscador.FirstCallback);
    GetAnimationTimer.AddTimerCallback(SEM_NINGUEM2, @piscador.SecondCallback);
    AssertEquals('um temporizador a mais', antes+1, GetAnimationTimer.TimerCount);

    GetAnimationTimer.RemoveCallbacksFromObject(piscador);

    AssertEquals('e ele sai com o ultimo avisado', antes, GetAnimationTimer.TimerCount);
  finally
    piscador.Free;
  end;
end;

procedure TTestAnimationTimers.ACallbackThatRemovesItselfDoesNotBreakTheOthers;
begin
  //um controle que para de piscar se remove de dentro da propria chamada, e a
  //lista encolhe no meio da volta do temporizador. Quem vem depois dele na
  //lista nao pode ser perdido - nem a volta pode passar do fim.
  //a control that stops blinking removes itself from inside its own call, and
  //the list shrinks in the middle of the timer's round. Whoever comes after it
  //on the list must not be lost - nor may the round run past the end.
  GetAnimationTimer.AddTimerCallback(SAI_DE_DENTRO, @CallbackThatRemovesItself);
  GetAnimationTimer.AddTimerCallback(SAI_DE_DENTRO, @CallbackA);

  AssertTrue('quem sai foi chamado', WaitUntil(@FSelfRemoving, 1));

  AssertTrue('e quem ficou continua sendo chamado', WaitUntil(@FCountA, FCountA+3));
  AssertEquals('quem saiu nao foi chamado de novo', 1, FSelfRemoving);
end;

procedure TTestAnimationTimers.ACallbackRemovedByAnotherIsNotCalledInTheSameRound;
begin
  //a lista encolhe no meio da volta: quem foi removido nao pode ser chamado
  //depois de sair, nem a volta pode ler alem do fim da lista
  //the list shrinks in the middle of the round: whoever was removed must not be
  //called after leaving, nor may the round read past the end of the list
  GetAnimationTimer.AddTimerCallback(SAI_NO_MEIO, @CallbackThatRemovesTheOther);
  GetAnimationTimer.AddTimerCallback(SAI_NO_MEIO, @CallbackB);

  AssertTrue('quem remove foi chamado', WaitUntil(@FSelfRemoving, 1));

  AssertEquals('o removido nao foi chamado', 0, FCountB);
end;

procedure TTestAnimationTimers.AnExceptionInOneCallbackDoesNotStopTheOthers;
begin
  //um controle com defeito nao pode parar o pisca da tela inteira
  //one broken control must not stop the whole screen from blinking
  GetAnimationTimer.AddTimerCallback(EXPLODE, @CallbackThatBlowsUp);
  GetAnimationTimer.AddTimerCallback(EXPLODE, @CallbackA);

  AssertTrue('o que quebra foi chamado', WaitUntil(@FBoom, 1));
  AssertTrue('e o outro continua sendo chamado', WaitUntil(@FCountA, 3));
end;

initialization
  RegisterTest(TTestAnimationTimers);

end.
