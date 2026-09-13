{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do ciclo de vida das threads de laco.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Quase tudo que corre sozinho nesta biblioteca deriva de
  TpSCADACoreAffinityThreadWithLoop: a thread de avisos de cada porta, as de
  varredura de cada driver, a de chamadas adiadas dos tags. Todas sao criadas
  suspensas e iniciadas em seguida, e quem as destroi espera pelo fim.

  A espera e' a parte delicada. O FPC nao chama Execute quando a thread foi
  criada suspensa e ja' esta terminada na hora em que enfim e' escalonada, e e'
  dentro do Execute que os eventos de "comecei" e "terminei" sao sinalizados.
  Destruir a thread antes que ela chegue a rodar, entao, nao pode depender
  desses eventos.
}
{$ELSE}
{:
  @abstract(Loop thread lifecycle tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Almost everything that runs on its own in this library descends from
  TpSCADACoreAffinityThreadWithLoop: each port's notification thread, each
  driver's scan threads, the tags' delayed call thread. All of them are created
  suspended and started right after, and whoever destroys them waits for the
  end.

  The waiting is the delicate part. FPC does not call Execute when the thread
  was created suspended and is already terminated by the time it finally gets
  scheduled, and it is inside Execute that the "started" and "finished" events
  are signaled. Destroying the thread before it ever runs, then, cannot depend
  on those events.
}
{$ENDIF}
unit ut.crossthreads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, crossthreads;

type

  { TTestLoop }

  //: thread de laco que so' conta voltas
  TTestLoop = class(TpSCADACoreAffinityThreadWithLoop)
  private
    FVoltas:LongInt;
  protected
    procedure Loop; override;
  public
    property Voltas:LongInt read FVoltas;
  end;

  { TTestCrossThreads }

  TTestCrossThreads = class(TTestCase)
  published
    procedure DestroyingRightAfterStartingMustNotHang;
    procedure DestroyingAfterTheLoopStartedMustNotHang;
    procedure WaitingForTheLoopToStartDoesNotHangOnAFinishedThread;
  end;

implementation

{ TTestLoop }

procedure TTestLoop.Loop;
begin
  inc(FVoltas);
  Sleep(1);
end;

{ TTestCrossThreads }

procedure TTestCrossThreads.DestroyingRightAfterStartingMustNotHang;
var
  c:LongInt;
  t:TTestLoop;
begin
  //ATENCAO: se a correcao for desfeita este teste nao falha - ele TRAVA a
  //rodada, porque a espera pelo fim da thread nao termina mais.
  //WARNING: if the fix is undone this test does not fail - it HANGS the run,
  //because the wait for the thread to finish never ends.
  for c:=1 to 50 do begin
    t:=TTestLoop.Create(true);
    t.Start;
    t.Free;
  end;

  AssertTrue('fifty threads created and destroyed in a row', true);
end;

procedure TTestCrossThreads.DestroyingAfterTheLoopStartedMustNotHang;
var
  t:TTestLoop;
begin
  //o caminho normal: a thread roda um pouco antes de ser destruida
  t:=TTestLoop.Create(true);
  try
    t.Start;
    t.WaitLoopStarts;
    Sleep(20);

    AssertTrue('the loop must have gone round', t.Voltas>0);
  finally
    t.Free;
  end;
end;

procedure TTestCrossThreads.WaitingForTheLoopToStartDoesNotHangOnAFinishedThread;
var
  t:TTestLoop;
begin
  //esperar pelo inicio de um laco que nunca vai comecar tem que voltar, nao
  //ficar preso: a thread acabou sem nunca ter rodado
  t:=TTestLoop.Create(true);
  try
    t.Start;
    t.Terminate;
    t.WaitLoopStarts;

    AssertTrue('the wait for the start must return', true);
  finally
    t.Free;
  end;
end;

initialization
  RegisterTest(TTestCrossThreads);

end.
