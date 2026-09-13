{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TMessageSpool: a fila de mensagens entre threads.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A fila e' duplamente encadeada e mantem tres ponteiros - primeira, ultima e
  a primeira mensagem normal - que precisam continuar coerentes depois de
  cada insercao e remocao. Os testes verificam a ordem observavel (que e' o
  contrato) e, de quebra, o heaptrc acusa qualquer no' que ficar para tras.
}
{$ELSE}
{:
  @abstract(TMessageSpool tests: the inter-thread message queue.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The queue is doubly linked and keeps three pointers - first, last and the
  first normal message - that must stay consistent after every insertion and
  removal. The tests check the observable order (which is the contract) and,
  as a bonus, heaptrc catches any node left behind.
}
{$ENDIF}
unit ut.messagespool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  MessageSpool;

type

  { TTestMessageSpool }

  TTestMessageSpool = class(TTestCase)
  private
    FFila:TMessageSpool;
    //: tira a proxima mensagem da fila e devolve o seu id (0 se a fila estiver vazia)
    function  NextId:Cardinal;
    procedure PostIt(aId:Cardinal; aPrioritaria:Boolean = false);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //fila vazia / empty queue
    procedure ANewQueueIsEmpty;
    procedure ReadingAnEmptyQueueFindsNothing;

    //ida e volta / round trip
    procedure APostedMessageIsFound;
    procedure ReadingWithoutRemovingLeavesTheMessageInTheQueue;
    procedure ReadingWithRemovalTakesItOutOfTheQueue;
    procedure TheParametersAreKept;

    //ordem / ordering
    procedure NormalMessagesComeOutInTheOrderTheyWentIn;
    procedure APriorityMessageJumpsTheQueue;
    procedure PriorityMessagesKeepTheirOrderAmongThemselves;
    procedure APriorityMessageInAnEmptyQueueIsStillFirst;
    procedure APriorityMessageAfterEverythingWasReadWorksAgain;

    procedure APriorityMessageJumpsAQueueOfASingleMessage;
    procedure APriorityMessageGoesToTheEndOfThePriorityQueue;

    //filtro por faixa de id / id range filter
    procedure TheFilterTakesOnlyTheMessageInTheRange;
    procedure AFilterWithNoMatchRemovesNothing;
    procedure AZeroedRangeTakesTheFirstMessage;

    //contagem / count
    procedure TheCountFollowsTheOperations;
  end;

implementation

procedure TTestMessageSpool.SetUp;
begin
  FFila:=TMessageSpool.Create;
end;

procedure TTestMessageSpool.TearDown;
begin
  //de proposito sem esvaziar: o destrutor tem que liberar o que sobrou, e o
  //heaptrc reclama se nao liberar.
  FreeAndNil(FFila);
end;

procedure TTestMessageSpool.PostIt(aId:Cardinal; aPrioritaria:Boolean);
begin
  FFila.PostMessage(aId, nil, nil, aPrioritaria);
end;

function TTestMessageSpool.NextId:Cardinal;
var
  msg:TMSMsg;
begin
  if FFila.PeekMessage(msg, 0, 0, true) then
    Result:=msg.MsgID
  else
    Result:=0;
end;

procedure TTestMessageSpool.ANewQueueIsEmpty;
begin
  AssertEquals('queue just created', 0, FFila.GetMsgCount);
end;

procedure TTestMessageSpool.ReadingAnEmptyQueueFindsNothing;
var
  msg:TMSMsg;
begin
  AssertFalse('there is nothing to read', FFila.PeekMessage(msg, 0, 0, true));
end;

procedure TTestMessageSpool.APostedMessageIsFound;
var
  msg:TMSMsg;
begin
  PostIt(42);
  AssertTrue  ('the message must be found', FFila.PeekMessage(msg, 0, 0, false));
  AssertEquals('identifier', 42, msg.MsgID);
end;

procedure TTestMessageSpool.ReadingWithoutRemovingLeavesTheMessageInTheQueue;
var
  msg:TMSMsg;
begin
  PostIt(42);
  FFila.PeekMessage(msg, 0, 0, false);

  AssertEquals('the message is still in the queue', 1, FFila.GetMsgCount);
  AssertTrue  ('and can be read again', FFila.PeekMessage(msg, 0, 0, false));
end;

procedure TTestMessageSpool.ReadingWithRemovalTakesItOutOfTheQueue;
var
  msg:TMSMsg;
begin
  PostIt(42);
  AssertTrue  ('the first read finds it', FFila.PeekMessage(msg, 0, 0, true));
  AssertEquals('queue emptied', 0, FFila.GetMsgCount);
  AssertFalse ('the second read does not find it', FFila.PeekMessage(msg, 0, 0, true));
end;

procedure TTestMessageSpool.TheParametersAreKept;
var
  msg:TMSMsg;
  w, l:Pointer;
begin
  //ponteiros nao sao desreferenciados pela fila: ela so os carrega
  w:=Pointer(PtrUInt($1234));
  l:=Pointer(PtrUInt($5678));

  FFila.PostMessage(7, w, l, true);
  FFila.PeekMessage(msg, 0, 0, true);

  AssertEquals('identifier', 7, msg.MsgID);
  AssertTrue  ('wParam', msg.wParam=w);
  AssertTrue  ('lParam', msg.lParam=l);
  AssertTrue  ('priority flag', msg.Priority);
end;

procedure TTestMessageSpool.NormalMessagesComeOutInTheOrderTheyWentIn;
begin
  PostIt(1);
  PostIt(2);
  PostIt(3);

  AssertEquals('first', 1, NextId);
  AssertEquals('second',  2, NextId);
  AssertEquals('third', 3, NextId);
  AssertEquals('queue empty at the end', 0, FFila.GetMsgCount);
end;

procedure TTestMessageSpool.APriorityMessageJumpsTheQueue;
begin
  PostIt(1);
  PostIt(2);
  PostIt(9, true);

  //a prioritaria passa na frente das normais que ja estavam esperando
  AssertEquals('the priority one first', 9, NextId);
  AssertEquals('then the oldest one', 1, NextId);
  AssertEquals('and the next one',         2, NextId);
end;

procedure TTestMessageSpool.PriorityMessagesKeepTheirOrderAmongThemselves;
begin
  PostIt(1);
  PostIt(2);
  PostIt(8, true);
  PostIt(9, true);

  //entre prioritarias vale a ordem de chegada; as normais ficam depois,
  //tambem na ordem em que entraram
  AssertEquals('first priority one', 8, NextId);
  AssertEquals('second priority one',  9, NextId);
  AssertEquals('first normal one',      1, NextId);
  AssertEquals('second normal one',       2, NextId);
end;

procedure TTestMessageSpool.APriorityMessageInAnEmptyQueueIsStillFirst;
begin
  PostIt(9, true);
  PostIt(1);

  AssertEquals('priority', 9, NextId);
  AssertEquals('normal',      1, NextId);
end;

procedure TTestMessageSpool.APriorityMessageAfterEverythingWasReadWorksAgain;
begin
  //esvaziar a fila tem que deixar os tres ponteiros internos coerentes:
  //se a marca da primeira mensagem normal ficar para tras, a proxima
  //prioritaria e' inserida no lugar errado.
  PostIt(1);
  PostIt(2);
  AssertEquals('first', 1, NextId);
  AssertEquals('second',  2, NextId);
  AssertEquals('empty queue', 0, FFila.GetMsgCount);

  PostIt(3);
  PostIt(4);
  PostIt(9, true);

  AssertEquals('the priority one goes first', 9, NextId);
  AssertEquals('and then the normal ones',   3, NextId);
  AssertEquals('in arrival order',   4, NextId);
end;

procedure TTestMessageSpool.APriorityMessageJumpsAQueueOfASingleMessage;
begin
  //com UMA mensagem na fila o caminho era outro, e ignorava a prioridade
  PostIt(1);
  PostIt(9, true);

  AssertEquals('the priority one must go to the front', 9, NextId);
  AssertEquals('and the normal one comes after',                  1, NextId);
end;

procedure TTestMessageSpool.APriorityMessageGoesToTheEndOfThePriorityQueue;
begin
  //com uma prioritaria sozinha na fila, outra prioritaria vai depois dela -
  //nao ha normal nenhuma para furar
  PostIt(8, true);
  PostIt(9, true);
  PostIt(1);

  AssertEquals('the first priority one', 8, NextId);
  AssertEquals('the second priority one',  9, NextId);
  AssertEquals('and only then the normal one',    1, NextId);
end;

procedure TTestMessageSpool.TheFilterTakesOnlyTheMessageInTheRange;
var
  msg:TMSMsg;
begin
  PostIt(10);
  PostIt(20);
  PostIt(30);

  AssertTrue  ('the range must find the middle one', FFila.PeekMessage(msg, 20, 20, true));
  AssertEquals('identifier', 20, msg.MsgID);
  AssertEquals('two are left', 2, FFila.GetMsgCount);

  //e as outras continuam na ordem, com a fila religada corretamente
  AssertEquals('first one left', 10, NextId);
  AssertEquals('second one left',  30, NextId);
end;

procedure TTestMessageSpool.AFilterWithNoMatchRemovesNothing;
var
  msg:TMSMsg;
begin
  PostIt(10);

  AssertFalse ('range outside what exists', FFila.PeekMessage(msg, 100, 200, true));
  AssertEquals('the message is still there', 1, FFila.GetMsgCount);
end;

procedure TTestMessageSpool.AZeroedRangeTakesTheFirstMessage;
var
  msg:TMSMsg;
begin
  //faixa 0..0 significa "qualquer mensagem"
  PostIt(10);
  PostIt(20);

  AssertTrue  ('no filter', FFila.PeekMessage(msg, 0, 0, true));
  AssertEquals('the first one in the queue', 10, msg.MsgID);
end;

procedure TTestMessageSpool.TheCountFollowsTheOperations;
var
  msg:TMSMsg;
begin
  AssertEquals('empty', 0, FFila.GetMsgCount);

  PostIt(1);
  PostIt(2, true);
  AssertEquals('two posted', 2, FFila.GetMsgCount);

  FFila.PeekMessage(msg, 0, 0, false);
  AssertEquals('reading without removing changes nothing', 2, FFila.GetMsgCount);

  FFila.PeekMessage(msg, 0, 0, true);
  AssertEquals('one removed', 1, FFila.GetMsgCount);
end;

initialization
  RegisterTest(TTestMessageSpool);

end.
