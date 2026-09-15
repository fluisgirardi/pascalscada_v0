{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes de pascalScadaMTPCPU: quantos processadores ha', e a troca
            de thread que depende disso.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Sao duas rotinas pequenas, mas chamadas nos lacos mais quentes da
  biblioteca: cada espera por um evento roda CrossThreadSwitch ate' ele vir.
  O numero de processadores tem que ser um numero de verdade - nunca zero ou
  negativo, que e' o que uma chamada de sistema falhando devolve - e a troca
  tem que continuar barata.
}
{$ELSE}
{:
  @abstract(pascalScadaMTPCPU tests: how many processors there are, and the
            thread switch that depends on it.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Two small routines, but called in the library's hottest loops: every wait
  for an event runs CrossThreadSwitch until it arrives. The processor count
  has to be a real number - never zero or negative, which is what a failing
  system call gives back - and the switch has to stay cheap.
}
{$ENDIF}
unit ut.mtpcpu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, pascalScadaMTPCPU;

type

  { TTestMTPCPU }

  TTestMTPCPU = class(TTestCase)
  published
    procedure ThereIsAtLeastOneProcessor;
    procedure TheCountIsPlausible;
    procedure TheCountIsStable;
    procedure ASwitchComesBack;
    procedure TenThousandSwitchesAreCheap;
  end;

implementation

procedure TTestMTPCPU.ThereIsAtLeastOneProcessor;
begin
  //uma chamada de sistema que falha devolve -1; isso nao pode chegar a quem
  //decide como esperar
  //a failing system call gives back -1; that must not reach whoever decides
  //how to wait
  AssertTrue('pelo menos um', GetSystemThreadCount>=1);
end;

procedure TTestMTPCPU.TheCountIsPlausible;
begin
  AssertTrue('nao mais que 4096', GetSystemThreadCount<=4096);
end;

procedure TTestMTPCPU.TheCountIsStable;
var
  primeiro, c:LongInt;
begin
  primeiro:=GetSystemThreadCount;
  for c:=1 to 100 do
    AssertEquals('sempre o mesmo', primeiro, GetSystemThreadCount);
end;

procedure TTestMTPCPU.ASwitchComesBack;
begin
  CrossThreadSwitch;
  AssertTrue('voltou', true);
end;

procedure TTestMTPCPU.TenThousandSwitchesAreCheap;
var
  c:LongInt;
  inicio:QWord;
begin
  //e' o que um laco de espera faz: a troca pergunta ao sistema quantos
  //processadores ha' a cada volta, e isso tem que continuar barato
  //it is what a wait loop does: the switch asks the system how many
  //processors there are on every turn, and that has to stay cheap
  inicio:=GetTickCount64;
  for c:=1 to 10000 do
    CrossThreadSwitch;

  AssertTrue('menos de um segundo', GetTickCount64-inicio<1000);
end;

initialization
  RegisterTest(TTestMTPCPU);

end.
