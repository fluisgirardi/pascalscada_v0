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

  { TLacoDeTeste }

  //: thread de laco que so' conta voltas
  TLacoDeTeste = class(TpSCADACoreAffinityThreadWithLoop)
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
    procedure DestruirLogoDepoisDeIniciarNaoPodeTravar;
    procedure DestruirDepoisDoLacoComecarNaoPodeTravar;
    procedure EsperarOLacoComecarNaoTravaComAThreadJaTerminada;
  end;

implementation

{ TLacoDeTeste }

procedure TLacoDeTeste.Loop;
begin
  inc(FVoltas);
  Sleep(1);
end;

{ TTestCrossThreads }

procedure TTestCrossThreads.DestruirLogoDepoisDeIniciarNaoPodeTravar;
var
  c:LongInt;
  t:TLacoDeTeste;
begin
  //ATENCAO: se a correcao for desfeita este teste nao falha - ele TRAVA a
  //rodada, porque a espera pelo fim da thread nao termina mais.
  //WARNING: if the fix is undone this test does not fail - it HANGS the run,
  //because the wait for the thread to finish never ends.
  for c:=1 to 50 do begin
    t:=TLacoDeTeste.Create(true);
    t.Start;
    t.Free;
  end;

  AssertTrue('cinquenta threads criadas e destruidas em sequencia', true);
end;

procedure TTestCrossThreads.DestruirDepoisDoLacoComecarNaoPodeTravar;
var
  t:TLacoDeTeste;
begin
  //o caminho normal: a thread roda um pouco antes de ser destruida
  t:=TLacoDeTeste.Create(true);
  try
    t.Start;
    t.WaitLoopStarts;
    Sleep(20);

    AssertTrue('o laco tem que ter dado voltas', t.Voltas>0);
  finally
    t.Free;
  end;
end;

procedure TTestCrossThreads.EsperarOLacoComecarNaoTravaComAThreadJaTerminada;
var
  t:TLacoDeTeste;
begin
  //esperar pelo inicio de um laco que nunca vai comecar tem que voltar, nao
  //ficar preso: a thread acabou sem nunca ter rodado
  t:=TLacoDeTeste.Create(true);
  try
    t.Start;
    t.Terminate;
    t.WaitLoopStarts;

    AssertTrue('a espera pelo inicio tem que voltar', true);
  finally
    t.Free;
  end;
end;

initialization
  RegisterTest(TTestCrossThreads);

end.
