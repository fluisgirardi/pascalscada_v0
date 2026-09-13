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
    procedure ContarAviso(Sender:TObject);
    procedure ContarFalhaDeLeitura(Sender:TObject);
    procedure ContarFalhaDeEscrita(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //avaliacao / evaluation
    procedure ExpressaoConstanteEhCalculada;
    procedure ExpressaoComUmaVariavel;
    procedure ExpressaoComDuasVariaveis;
    procedure FuncaoMatematicaEhAceita;
    procedure FuncaoIfThenEscolheOValor;
    procedure ResultadoInteiroNaoViraLixo;
    procedure ComparacaoDiretaValeUmOuZero;
    procedure ResultadoDeTextoEhErro;

    //recalculo / recalculation
    procedure MudarOTagDeOrigemRecalcula;
    procedure MudarAExpressaoRecalcula;
    procedure MudancaNoValorAvisaQuemEscuta;

    //erros / errors
    procedure ExpressaoNovaComecaSemErro;
    procedure ExpressaoInvalidaGuardaOErro;
    procedure ExpressaoInvalidaNaoMudaOValor;
    procedure ExpressaoInvalidaAvisaFalhaDeLeitura;
    procedure ExpressaoBoaDepoisDeUmaRuimLimpaOErro;
    procedure VariavelNaoLigadaEhErro;

    //escrita / writing
    procedure EscreverNoTagCalculadoEhRecusado;

    //ciclo de vida / lifecycle
    procedure TagDeOrigemDestruidoDesligaOVinculo;
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

procedure TTestNumExprTag.ContarAviso(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestNumExprTag.ContarFalhaDeLeitura(Sender:TObject);
begin
  inc(FFalhasDeLeitura);
end;

procedure TTestNumExprTag.ContarFalhaDeEscrita(Sender:TObject);
begin
  inc(FFalhasDeEscrita);
end;

procedure TTestNumExprTag.ExpressaoConstanteEhCalculada;
begin
  FExpr.Expression:='2+3';
  AssertEquals('constante', 5, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ExpressaoComUmaVariavel;
begin
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='A*2';

  AssertEquals('uma variavel', 20, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ExpressaoComDuasVariaveis;
begin
  FA.ChegouDoCLP(10);
  FB.ChegouDoCLP(4);
  FExpr.A:=FA;
  FExpr.B:=FB;
  FExpr.Expression:='A-B';

  AssertEquals('duas variaveis', 6, FExpr.Value, 0);
end;

procedure TTestNumExprTag.FuncaoMatematicaEhAceita;
begin
  FA.ChegouDoCLP(9);
  FExpr.A:=FA;
  FExpr.Expression:='sqrt(A)';

  AssertEquals('raiz quadrada', 3, FExpr.Value, 0.0001);
end;

procedure TTestNumExprTag.FuncaoIfThenEscolheOValor;
begin
  //ifthen e' acrescentada pela propria unit, nao vem do avaliador
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='ifthen(A>5, 100, 200)';
  AssertEquals('condicao verdadeira', 100, FExpr.Value, 0);

  FA.ChegouDoCLP(1);
  AssertEquals('condicao falsa', 200, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ResultadoInteiroNaoViraLixo;
begin
  //o avaliador devolve um registro variante: ler o campo de ponto flutuante
  //de um resultado inteiro devolve os bits do inteiro
  FA.ChegouDoCLP(7);
  FExpr.A:=FA;

  FExpr.Expression:='10';
  AssertEquals('constante inteira', 10, FExpr.Value, 0);

  FExpr.Expression:='round(A/2)';
  AssertEquals('arredondamento',     4, FExpr.Value, 0);

  FExpr.Expression:='trunc(A/2)';
  AssertEquals('truncamento',        3, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ComparacaoDiretaValeUmOuZero;
begin
  //escrever a comparacao direto e' natural num tag de supervisao
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;

  FExpr.Expression:='A>5';
  AssertEquals('verdadeiro vale um', 1, FExpr.Value, 0);

  FA.ChegouDoCLP(1);
  AssertEquals('falso vale zero',    0, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ResultadoDeTextoEhErro;
begin
  //texto nao vira valor de tag: tem que virar erro, nao numero estranho
  FExpr.Expression:='5';
  AssertEquals('valor bom antes', 5, FExpr.Value, 0);

  FExpr.Expression:='"abc"';
  AssertTrue  ('resultado de texto e erro', FExpr.LastEvalutionError<>'OK');
  AssertEquals('e o valor anterior fica',   5, FExpr.Value, 0);
end;

procedure TTestNumExprTag.MudarOTagDeOrigemRecalcula;
begin
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='A*2';
  AssertEquals('antes', 20, FExpr.Value, 0);

  FA.ChegouDoCLP(30);
  AssertEquals('depois da mudanca na origem', 60, FExpr.Value, 0);
end;

procedure TTestNumExprTag.MudarAExpressaoRecalcula;
begin
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='A*2';
  AssertEquals('antes', 20, FExpr.Value, 0);

  FExpr.Expression:='A+1';
  AssertEquals('depois de mudar a expressao', 11, FExpr.Value, 0);
end;

procedure TTestNumExprTag.MudancaNoValorAvisaQuemEscuta;
begin
  FA.ChegouDoCLP(10);
  FExpr.A:=FA;
  FExpr.Expression:='A*2';

  FExpr.AddTagChangeHandler(@ContarAviso);
  FAvisos:=0;

  FA.ChegouDoCLP(30);
  AssertTrue('o valor mudou, tem que avisar', FAvisos>0);
end;

procedure TTestNumExprTag.ExpressaoNovaComecaSemErro;
begin
  FExpr.Expression:='1+1';
  AssertEquals('sem erro', 'OK', FExpr.LastEvalutionError);
end;

procedure TTestNumExprTag.ExpressaoInvalidaGuardaOErro;
begin
  FExpr.Expression:='2+*3';
  AssertTrue('o erro tem que ficar guardado', FExpr.LastEvalutionError<>'OK');
end;

procedure TTestNumExprTag.ExpressaoInvalidaNaoMudaOValor;
begin
  FExpr.Expression:='7';
  AssertEquals('valor bom', 7, FExpr.Value, 0);

  FExpr.Expression:='2+*3';
  AssertEquals('o valor anterior fica de pe', 7, FExpr.Value, 0);
end;

procedure TTestNumExprTag.ExpressaoInvalidaAvisaFalhaDeLeitura;
begin
  FExpr.AddReadFaultHandler(@ContarFalhaDeLeitura);
  FExpr.Expression:='2+*3';

  AssertTrue('quem escuta tem que saber', FFalhasDeLeitura>0);
end;

procedure TTestNumExprTag.ExpressaoBoaDepoisDeUmaRuimLimpaOErro;
begin
  FExpr.Expression:='2+*3';
  AssertTrue('erro guardado', FExpr.LastEvalutionError<>'OK');

  FExpr.Expression:='4+4';
  AssertEquals('erro limpo', 'OK', FExpr.LastEvalutionError);
  AssertEquals('e o valor novo', 8, FExpr.Value, 0);
end;

procedure TTestNumExprTag.VariavelNaoLigadaEhErro;
begin
  //A so' existe na expressao se houver um tag ligado nela
  FExpr.Expression:='A*2';
  AssertTrue('variavel sem tag', FExpr.LastEvalutionError<>'OK');
end;

procedure TTestNumExprTag.EscreverNoTagCalculadoEhRecusado;
begin
  FExpr.Expression:='7';
  FExpr.AddWriteFaultHandler(@ContarFalhaDeEscrita);

  FExpr.Value:=99;

  AssertEquals('o valor calculado nao muda', 7, FExpr.Value, 0);
  AssertTrue  ('e a escrita e recusada',     FFalhasDeEscrita>0);
end;

procedure TTestNumExprTag.TagDeOrigemDestruidoDesligaOVinculo;
var
  origem:TFakeNumber;
begin
  origem:=TFakeNumber.Create(nil);
  origem.ChegouDoCLP(5);
  FExpr.A:=origem;
  FExpr.Expression:='A';
  AssertEquals('lendo da origem', 5, FExpr.Value, 0);

  FreeAndNil(origem);

  AssertTrue('o vinculo tem que ter sido desfeito', FExpr.A=nil);
end;

initialization
  RegisterTest(TTestNumExprTag);

end.
