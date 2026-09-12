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
    function  ProximoId:Cardinal;
    procedure Posta(aId:Cardinal; aPrioritaria:Boolean = false);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //fila vazia / empty queue
    procedure FilaNovaEstaVazia;
    procedure LeituraDeFilaVaziaNaoAchaNada;

    //ida e volta / round trip
    procedure MensagemPostadaEhEncontrada;
    procedure LeituraSemRemoverDeixaAMensagemNaFila;
    procedure LeituraComRemocaoTiraDaFila;
    procedure ParametrosSaoPreservados;

    //ordem / ordering
    procedure MensagensNormaisSaemNaOrdemEmQueEntraram;
    procedure MensagemPrioritariaFuraAFila;
    procedure PrioritariasMantemAOrdemEntreSi;
    procedure PrioritariaNaFilaVaziaContinuaPrimeira;
    procedure PrioritariaDepoisDeTudoLidoVoltaAFuncionar;

    procedure PrioritariaFuraAFilaComUmaMensagemSo;
    procedure PrioritariaEntraNoFimDaFilaDePrioritarias;

    //filtro por faixa de id / id range filter
    procedure FiltroPegaSoAMensagemDaFaixa;
    procedure FiltroSemCorrespondenciaNaoRemoveNada;
    procedure FaixaZeradaPegaAPrimeiraMensagem;

    //contagem / count
    procedure ContagemAcompanhaAsOperacoes;
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

procedure TTestMessageSpool.Posta(aId:Cardinal; aPrioritaria:Boolean);
begin
  FFila.PostMessage(aId, nil, nil, aPrioritaria);
end;

function TTestMessageSpool.ProximoId:Cardinal;
var
  msg:TMSMsg;
begin
  if FFila.PeekMessage(msg, 0, 0, true) then
    Result:=msg.MsgID
  else
    Result:=0;
end;

procedure TTestMessageSpool.FilaNovaEstaVazia;
begin
  AssertEquals('fila recem criada', 0, FFila.GetMsgCount);
end;

procedure TTestMessageSpool.LeituraDeFilaVaziaNaoAchaNada;
var
  msg:TMSMsg;
begin
  AssertFalse('nao ha o que ler', FFila.PeekMessage(msg, 0, 0, true));
end;

procedure TTestMessageSpool.MensagemPostadaEhEncontrada;
var
  msg:TMSMsg;
begin
  Posta(42);
  AssertTrue  ('a mensagem tem que ser achada', FFila.PeekMessage(msg, 0, 0, false));
  AssertEquals('identificador', 42, msg.MsgID);
end;

procedure TTestMessageSpool.LeituraSemRemoverDeixaAMensagemNaFila;
var
  msg:TMSMsg;
begin
  Posta(42);
  FFila.PeekMessage(msg, 0, 0, false);

  AssertEquals('a mensagem continua na fila', 1, FFila.GetMsgCount);
  AssertTrue  ('e pode ser lida de novo', FFila.PeekMessage(msg, 0, 0, false));
end;

procedure TTestMessageSpool.LeituraComRemocaoTiraDaFila;
var
  msg:TMSMsg;
begin
  Posta(42);
  AssertTrue  ('primeira leitura acha', FFila.PeekMessage(msg, 0, 0, true));
  AssertEquals('fila esvaziada', 0, FFila.GetMsgCount);
  AssertFalse ('segunda leitura nao acha', FFila.PeekMessage(msg, 0, 0, true));
end;

procedure TTestMessageSpool.ParametrosSaoPreservados;
var
  msg:TMSMsg;
  w, l:Pointer;
begin
  //ponteiros nao sao desreferenciados pela fila: ela so os carrega
  w:=Pointer(PtrUInt($1234));
  l:=Pointer(PtrUInt($5678));

  FFila.PostMessage(7, w, l, true);
  FFila.PeekMessage(msg, 0, 0, true);

  AssertEquals('identificador', 7, msg.MsgID);
  AssertTrue  ('wParam', msg.wParam=w);
  AssertTrue  ('lParam', msg.lParam=l);
  AssertTrue  ('marca de prioridade', msg.Priority);
end;

procedure TTestMessageSpool.MensagensNormaisSaemNaOrdemEmQueEntraram;
begin
  Posta(1);
  Posta(2);
  Posta(3);

  AssertEquals('primeira', 1, ProximoId);
  AssertEquals('segunda',  2, ProximoId);
  AssertEquals('terceira', 3, ProximoId);
  AssertEquals('fila vazia ao fim', 0, FFila.GetMsgCount);
end;

procedure TTestMessageSpool.MensagemPrioritariaFuraAFila;
begin
  Posta(1);
  Posta(2);
  Posta(9, true);

  //a prioritaria passa na frente das normais que ja estavam esperando
  AssertEquals('prioritaria primeiro', 9, ProximoId);
  AssertEquals('depois a mais antiga', 1, ProximoId);
  AssertEquals('e a seguinte',         2, ProximoId);
end;

procedure TTestMessageSpool.PrioritariasMantemAOrdemEntreSi;
begin
  Posta(1);
  Posta(2);
  Posta(8, true);
  Posta(9, true);

  //entre prioritarias vale a ordem de chegada; as normais ficam depois,
  //tambem na ordem em que entraram
  AssertEquals('primeira prioritaria', 8, ProximoId);
  AssertEquals('segunda prioritaria',  9, ProximoId);
  AssertEquals('primeira normal',      1, ProximoId);
  AssertEquals('segunda normal',       2, ProximoId);
end;

procedure TTestMessageSpool.PrioritariaNaFilaVaziaContinuaPrimeira;
begin
  Posta(9, true);
  Posta(1);

  AssertEquals('prioritaria', 9, ProximoId);
  AssertEquals('normal',      1, ProximoId);
end;

procedure TTestMessageSpool.PrioritariaDepoisDeTudoLidoVoltaAFuncionar;
begin
  //esvaziar a fila tem que deixar os tres ponteiros internos coerentes:
  //se a marca da primeira mensagem normal ficar para tras, a proxima
  //prioritaria e' inserida no lugar errado.
  Posta(1);
  Posta(2);
  AssertEquals('primeira', 1, ProximoId);
  AssertEquals('segunda',  2, ProximoId);
  AssertEquals('fila vazia', 0, FFila.GetMsgCount);

  Posta(3);
  Posta(4);
  Posta(9, true);

  AssertEquals('prioritaria na frente', 9, ProximoId);
  AssertEquals('e depois as normais',   3, ProximoId);
  AssertEquals('na ordem de chegada',   4, ProximoId);
end;

procedure TTestMessageSpool.PrioritariaFuraAFilaComUmaMensagemSo;
begin
  //com UMA mensagem na fila o caminho era outro, e ignorava a prioridade
  Posta(1);
  Posta(9, true);

  AssertEquals('a prioritaria tem que passar na frente', 9, ProximoId);
  AssertEquals('e a normal vem depois',                  1, ProximoId);
end;

procedure TTestMessageSpool.PrioritariaEntraNoFimDaFilaDePrioritarias;
begin
  //com uma prioritaria sozinha na fila, outra prioritaria vai depois dela -
  //nao ha normal nenhuma para furar
  Posta(8, true);
  Posta(9, true);
  Posta(1);

  AssertEquals('a primeira prioritaria', 8, ProximoId);
  AssertEquals('a segunda prioritaria',  9, ProximoId);
  AssertEquals('e so entao a normal',    1, ProximoId);
end;

procedure TTestMessageSpool.FiltroPegaSoAMensagemDaFaixa;
var
  msg:TMSMsg;
begin
  Posta(10);
  Posta(20);
  Posta(30);

  AssertTrue  ('a faixa tem que achar a do meio', FFila.PeekMessage(msg, 20, 20, true));
  AssertEquals('identificador', 20, msg.MsgID);
  AssertEquals('sobraram duas', 2, FFila.GetMsgCount);

  //e as outras continuam na ordem, com a fila religada corretamente
  AssertEquals('primeira restante', 10, ProximoId);
  AssertEquals('segunda restante',  30, ProximoId);
end;

procedure TTestMessageSpool.FiltroSemCorrespondenciaNaoRemoveNada;
var
  msg:TMSMsg;
begin
  Posta(10);

  AssertFalse ('faixa fora do que existe', FFila.PeekMessage(msg, 100, 200, true));
  AssertEquals('a mensagem continua la', 1, FFila.GetMsgCount);
end;

procedure TTestMessageSpool.FaixaZeradaPegaAPrimeiraMensagem;
var
  msg:TMSMsg;
begin
  //faixa 0..0 significa "qualquer mensagem"
  Posta(10);
  Posta(20);

  AssertTrue  ('sem filtro', FFila.PeekMessage(msg, 0, 0, true));
  AssertEquals('a primeira da fila', 10, msg.MsgID);
end;

procedure TTestMessageSpool.ContagemAcompanhaAsOperacoes;
var
  msg:TMSMsg;
begin
  AssertEquals('vazia', 0, FFila.GetMsgCount);

  Posta(1);
  Posta(2, true);
  AssertEquals('duas postadas', 2, FFila.GetMsgCount);

  FFila.PeekMessage(msg, 0, 0, false);
  AssertEquals('ler sem remover nao muda', 2, FFila.GetMsgCount);

  FFila.PeekMessage(msg, 0, 0, true);
  AssertEquals('uma removida', 1, FFila.GetMsgCount);
end;

initialization
  RegisterTest(TTestMessageSpool);

end.
