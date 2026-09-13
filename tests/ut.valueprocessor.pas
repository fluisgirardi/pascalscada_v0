{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da fila de processadores de escala.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Uma escala transforma o valor puro que veio do equipamento no valor que o
  usuario ve, e desfaz a transformacao no caminho de volta. A fila encadeia
  varias: indo do equipamento para o usuario ela percorre os itens do primeiro
  ao ultimo, e voltando percorre ao contrario, para que o caminho de volta
  desfaca exatamente o de ida.

  Cada item da fila aponta para um processador de escala, e esse apontamento
  pode ser trocado ou limpo em tempo de projeto.
}
{$ELSE}
{:
  @abstract(Scale processor queue tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A scale turns the raw value that came from the device into the value the
  user sees, and undoes the transformation on the way back. The queue chains
  several of them: going from device to user it walks the items first to last,
  and coming back it walks them in reverse, so that the return trip undoes
  exactly the outbound one.

  Each queue item points at a scale processor, and that link can be changed or
  cleared at design time.
}
{$ENDIF}
unit ut.valueprocessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ValueProcessor;

type

  { TEscalaQueDobra }

  //: escala de teste: dobra na ida, divide na volta
  TEscalaQueDobra = class(TScaleProcessor)
  public
    function SetInGetOut(Sender:TComponent; aInput:Double):Double; override;
    function SetOutGetIn(Sender:TComponent; aOutput:Double):Double; override;
  end;

  { TEscalaQueSoma10 }

  TEscalaQueSoma10 = class(TScaleProcessor)
  public
    function SetInGetOut(Sender:TComponent; aInput:Double):Double; override;
    function SetOutGetIn(Sender:TComponent; aOutput:Double):Double; override;
  end;

  { TTestValueProcessor }

  TTestValueProcessor = class(TTestCase)
  private
    FFila:TScalesQueue;
    FDobra:TEscalaQueDobra;
    FSoma:TEscalaQueSoma10;
    function  NovoItem(aEscala:TScaleProcessor):TScaleQueueItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //encadeamento / chaining
    procedure FilaVaziaDevolveOValorSemMexer;
    procedure ItemSemEscalaNaoMexeNoValor;
    procedure UmaEscalaNaFilaEhAplicada;
    procedure DuasEscalasSaoAplicadasNaOrdemDaFila;
    procedure OCaminhoDeVoltaDesfazODeIda;

    //o vinculo do item com a escala / the item's link to its scale
    procedure ItemNovoNaoTemEscala;
    procedure TrocarAEscalaDoItem;
    procedure LimparAEscalaDoItem;
    procedure AFilaNaoPodeApontarParaOProprioDono;
  end;

implementation

{ TEscalaQueDobra }

function TEscalaQueDobra.SetInGetOut(Sender:TComponent; aInput:Double):Double;
begin
  Result:=aInput*2;
end;

function TEscalaQueDobra.SetOutGetIn(Sender:TComponent; aOutput:Double):Double;
begin
  Result:=aOutput/2;
end;

{ TEscalaQueSoma10 }

function TEscalaQueSoma10.SetInGetOut(Sender:TComponent; aInput:Double):Double;
begin
  Result:=aInput+10;
end;

function TEscalaQueSoma10.SetOutGetIn(Sender:TComponent; aOutput:Double):Double;
begin
  Result:=aOutput-10;
end;

{ TTestValueProcessor }

procedure TTestValueProcessor.SetUp;
begin
  FFila :=TScalesQueue.Create(nil);
  FDobra:=TEscalaQueDobra.Create(nil);
  FSoma :=TEscalaQueSoma10.Create(nil);
end;

procedure TTestValueProcessor.TearDown;
begin
  FreeAndNil(FFila);
  FreeAndNil(FDobra);
  FreeAndNil(FSoma);
end;

function TTestValueProcessor.NovoItem(aEscala:TScaleProcessor):TScaleQueueItem;
begin
  Result:=FFila.ScalesQueue.Add;
  if aEscala<>nil then
    Result.ScaleProcessor:=aEscala;
end;

procedure TTestValueProcessor.FilaVaziaDevolveOValorSemMexer;
begin
  AssertEquals('there',   7, FFila.SetInGetOut(nil, 7), 0);
  AssertEquals('back', 7, FFila.SetOutGetIn(nil, 7), 0);
end;

procedure TTestValueProcessor.ItemSemEscalaNaoMexeNoValor;
begin
  //item na fila sem processador associado e' um elo neutro
  NovoItem(nil);
  AssertEquals('there',   7, FFila.SetInGetOut(nil, 7), 0);
  AssertEquals('back', 7, FFila.SetOutGetIn(nil, 7), 0);
end;

procedure TTestValueProcessor.UmaEscalaNaFilaEhAplicada;
begin
  NovoItem(FDobra);
  AssertEquals('there',   14, FFila.SetInGetOut(nil, 7), 0);
  AssertEquals('back',  7, FFila.SetOutGetIn(nil, 14), 0);
end;

procedure TTestValueProcessor.DuasEscalasSaoAplicadasNaOrdemDaFila;
begin
  //do equipamento para o usuario: primeiro item primeiro. 7 dobra para 14 e
  //depois soma 10, dando 24
  NovoItem(FDobra);
  NovoItem(FSoma);

  AssertEquals('there', 24, FFila.SetInGetOut(nil, 7), 0);
end;

procedure TTestValueProcessor.OCaminhoDeVoltaDesfazODeIda;
begin
  //a volta percorre a fila ao contrario: sem isso ela nao desfaria a ida
  NovoItem(FDobra);
  NovoItem(FSoma);

  AssertEquals('the way back undoes the way there', 7, FFila.SetOutGetIn(nil, FFila.SetInGetOut(nil, 7)), 0);
end;

procedure TTestValueProcessor.ItemNovoNaoTemEscala;
var
  item:TScaleQueueItem;
begin
  item:=FFila.ScalesQueue.Add;
  AssertTrue('item just created', item.ScaleProcessor=nil);
end;

procedure TTestValueProcessor.TrocarAEscalaDoItem;
var
  item:TScaleQueueItem;
begin
  item:=NovoItem(FDobra);
  item.ScaleProcessor:=FSoma;

  AssertTrue  ('the scale changed', item.ScaleProcessor=FSoma);
  AssertEquals('and the value follows the new one', 17, FFila.SetInGetOut(nil, 7), 0);
end;

procedure TTestValueProcessor.LimparAEscalaDoItem;
var
  item:TScaleQueueItem;
begin
  //desfazer o vinculo e' operacao valida: o item volta a ser um elo neutro
  item:=NovoItem(FDobra);
  item.ScaleProcessor:=nil;

  AssertTrue  ('no scale',            item.ScaleProcessor=nil);
  AssertEquals('the value goes through untouched', 7, FFila.SetInGetOut(nil, 7), 0);
end;

procedure TTestValueProcessor.AFilaNaoPodeApontarParaOProprioDono;
var
  item:TScaleQueueItem;
  recusou:Boolean;
begin
  //uma fila que contem a si mesma entraria em recursao sem fim
  item:=FFila.ScalesQueue.Add;
  recusou:=false;
  try
    item.ScaleProcessor:=FFila;
  except
    on E:Exception do
      recusou:=true;
  end;

  AssertTrue('the queue itself must be refused', recusou);
end;

initialization
  RegisterTest(TTestValueProcessor);

end.
