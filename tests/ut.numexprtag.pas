{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TNumericExprTag: o tag calculado por expressao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Este tag nao le nada do equipamento: o valor dele sai de uma expressao sobre
  ate' dez outros tags, chamados de A a J. A cada mudanca em qualquer um
  deles a expressao e' avaliada de novo.

  E' um tag de leitura: escrever nele nao faz nada alem de avisar falha de
  escrita. Quando a expressao nao pode ser avaliada, o erro fica guardado em
  LastEvalutionError e o valor anterior permanece.
}
{$ELSE}
{:
  @abstract(TNumericExprTag tests: the expression computed tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This tag reads nothing from the device: its value comes from an expression
  over up to ten other tags, named A to J. On every change of any of them the
  expression is evaluated again.

  It is a read tag: writing to it does nothing but raise a write fault. When
  the expression cannot be evaluated, the error is kept in LastEvalutionError
  and the previous value stays.
}
{$ENDIF}
unit ut.numexprtag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, NumExprTag,
  testsupport.faketag;

type

  { TTestNumExprTag }

  TTestNumExprTag = class(TTestCase)
  private
    FExpr:TNumericExprTag;
    FA, FB:TFakeNumber;
    FAvisos, FFalhasDeLeitura, FFalhasDeEscrita:LongInt;
    procedure CountNotification(Sender:TObject);
    procedure CountReadFailure(Sender:TObject);
    procedure CountWriteFailure(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //avaliacao / evaluation
    procedure AConstantExpressionIsEvaluated;
    procedure AnExpressionWithOneVariable;
    procedure AnExpressionWithTwoVariables;
    procedure AMathFunctionIsAccepted;
    procedure TheIfThenFunctionPicksTheValue;
    procedure AnIntegerResultDoesNotTurnIntoGarbage;
    procedure ADirectComparisonIsOneOrZero;
    procedure ATextResultIsAnError;

    //recalculo / recalculation
    procedure ChangingTheSourceTagRecomputes;
    procedure ChangingTheExpressionRecomputes;
    procedure AChangeInTheValueNotifiesTheListener;

    //erros / errors
    procedure ANewExpressionStartsWithNoError;
    procedure AnInvalidExpressionKeepsTheError;
    procedure AnInvalidExpressionDoesNotChangeTheValue;
    procedure AnInvalidExpressionReportsAReadFailure;
    procedure AGoodExpressionAfterABadOneClearsTheError;
    procedure AnUnboundVariableIsAnError;

    //escrita / writing
    procedure WritingToTheComputedTagIsRefused;

    //ciclo de vida / lifecycle
    procedure ADestroyedSourceTagBreaksTheLink;
  end;

implementation

procedure TTestNumExprTag.SetUp;
begin
  FExpr:=TNumericExprTag.Create(nil);
  FA   :=TFakeNumber.Create(nil);
  FB   :=TFakeNumber.Create(nil);
  FAvisos:=0;
  FFalhasDeLeitura:=0;
  FFalhasDeEscrita:=0;
end;

procedure TTestNumExprTag.TearDown;
begin
  FreeAndNil(FExpr);
  FreeAndNil(FA);
  FreeAndNil(FB);
end;

procedure TTestNumExprTag.CountNotification(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestNumExprTag.CountReadFailure(Sender:TObject);
begin
  inc(FFalhasDeLeitura);
end;

procedure TTestNumExprTag.CountWriteFailure(Sender:TObject);
begin
  inc(FFalhasDeEscrita);
end;

procedure TTestNumExprTag.AConstantExpressionIsEvaluated;
begin
  FExpr.Expression:='2+3';
  AssertEquals('constant', 5, FExpr.Value, 0);
end;

procedure TTestNumExprTag.AnExpressionWithOneVariable;
begin
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='A*2';

  AssertEquals('one variable', 20, FExpr.Value, 0);
end;

procedure TTestNumExprTag.AnExpressionWithTwoVariables;
begin
  FA.ChegouDoCLP(10);
  FB.ChegouDoCLP(4);
  FExpr.A:=FA;
  FExpr.B:=FB;
  FExpr.Expression:='A-B';

  AssertEquals('two variables', 6, FExpr.Value, 0);
end;

procedure TTestNumExprTag.AMathFunctionIsAccepted;
begin
  FA.ChegouDoCLP(9);
  FExpr.A:=FA;
  FExpr.Expression:='sqrt(A)';

  AssertEquals('square root', 3, FExpr.Value, 0.0001);
end;

procedure TTestNumExprTag.TheIfThenFunctionPicksTheValue;
begin
  //ifthen e' acrescentada pela propria unit, nao vem do avaliador
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='ifthen(A>5, 100, 200)';
  AssertEquals('true condition', 100, FExpr.Value, 0);

  FA.ChegouDoCLP(1);
  AssertEquals('false condition', 200, FExpr.Value, 0);
end;

procedure TTestNumExprTag.AnIntegerResultDoesNotTurnIntoGarbage;
begin
  //o avaliador devolve um registro variante: ler o campo de ponto flutuante
  //de um resultado inteiro devolve os bits do inteiro
  FA.ChegouDoCLP(7);
  FExpr.A:=FA;

  FExpr.Expression:='10';
  AssertEquals('integer constant', 10, FExpr.Value, 0);

  FExpr.Expression:='round(A/2)';
  AssertEquals('rounding',     4, FExpr.Value, 0);

  FExpr.Expression:='trunc(A/2)';
  AssertEquals('truncation',        3, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ADirectComparisonIsOneOrZero;
begin
  //escrever a comparacao direto e' natural num tag de supervisao
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;

  FExpr.Expression:='A>5';
  AssertEquals('true is one', 1, FExpr.Value, 0);

  FA.ChegouDoCLP(1);
  AssertEquals('false is zero',    0, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ATextResultIsAnError;
begin
  //texto nao vira valor de tag: tem que virar erro, nao numero estranho
  FExpr.Expression:='5';
  AssertEquals('good value before', 5, FExpr.Value, 0);

  FExpr.Expression:='"abc"';
  AssertTrue  ('a text result is an error', FExpr.LastEvalutionError<>'OK');
  AssertEquals('and the previous value stays',   5, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ChangingTheSourceTagRecomputes;
begin
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='A*2';
  AssertEquals('before', 20, FExpr.Value, 0);

  FA.ChegouDoCLP(30);
  AssertEquals('after the change at the source', 60, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ChangingTheExpressionRecomputes;
begin
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='A*2';
  AssertEquals('before', 20, FExpr.Value, 0);

  FExpr.Expression:='A+1';
  AssertEquals('after changing the expression', 11, FExpr.Value, 0);
end;

procedure TTestNumExprTag.AChangeInTheValueNotifiesTheListener;
begin
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='A*2';

  FExpr.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  FA.ChegouDoCLP(30);
  AssertTrue('the value changed, it must notify', FAvisos>0);
end;

procedure TTestNumExprTag.ANewExpressionStartsWithNoError;
begin
  FExpr.Expression:='1+1';
  AssertEquals('no error', 'OK', FExpr.LastEvalutionError);
end;

procedure TTestNumExprTag.AnInvalidExpressionKeepsTheError;
begin
  FExpr.Expression:='2+*3';
  AssertTrue('the error must be kept', FExpr.LastEvalutionError<>'OK');
end;

procedure TTestNumExprTag.AnInvalidExpressionDoesNotChangeTheValue;
begin
  FExpr.Expression:='7';
  AssertEquals('good value', 7, FExpr.Value, 0);

  FExpr.Expression:='2+*3';
  AssertEquals('the previous value stands', 7, FExpr.Value, 0);
end;

procedure TTestNumExprTag.AnInvalidExpressionReportsAReadFailure;
begin
  FExpr.AddReadFaultHandler(@CountReadFailure);
  FExpr.Expression:='2+*3';

  AssertTrue('the listener must be told', FFalhasDeLeitura>0);
end;

procedure TTestNumExprTag.AGoodExpressionAfterABadOneClearsTheError;
begin
  FExpr.Expression:='2+*3';
  AssertTrue('error kept', FExpr.LastEvalutionError<>'OK');

  FExpr.Expression:='4+4';
  AssertEquals('error cleared', 'OK', FExpr.LastEvalutionError);
  AssertEquals('and the new value', 8, FExpr.Value, 0);
end;

procedure TTestNumExprTag.AnUnboundVariableIsAnError;
begin
  //A so' existe na expressao se houver um tag ligado nela
  FExpr.Expression:='A*2';
  AssertTrue('variable with no tag', FExpr.LastEvalutionError<>'OK');
end;

procedure TTestNumExprTag.WritingToTheComputedTagIsRefused;
begin
  FExpr.Expression:='7';
  FExpr.AddWriteFaultHandler(@CountWriteFailure);

  FExpr.Value:=99;

  AssertEquals('the computed value does not change', 7, FExpr.Value, 0);
  AssertTrue  ('and the write is refused',     FFalhasDeEscrita>0);
end;

procedure TTestNumExprTag.ADestroyedSourceTagBreaksTheLink;
var
  origem:TFakeNumber;
begin
  origem:=TFakeNumber.Create(nil);
  origem.ChegouDoCLP(5);
  FExpr.A:=origem;
  FExpr.Expression:='A';
  AssertEquals('reading from the source', 5, FExpr.Value, 0);

  FreeAndNil(origem);

  AssertTrue('the link must have been broken', FExpr.A=nil);
end;

initialization
  RegisterTest(TTestNumExprTag);

end.
