{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TPLCBlock e do TPLCBlockElement.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um bloco e' um tag de varios valores lidos de uma vez, e o elemento e' uma
  janela de um valor so' dentro dele. E' o mesmo arranjo pai-filho do tag de
  bits, com uma diferenca: aqui o filho nao recorta, ele indexa.

  O bloco recebe os valores da varredura por TagCommandCallBack, que e'
  protegido - uma sonda o expoe, e com isso os testes nao precisam de driver
  nenhum, so' entregam os valores como um driver entregaria.
}
{$ELSE}
{:
  @abstract(TPLCBlock and TPLCBlockElement tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A block is a tag of several values read in one go, and an element is a one
  value window into it. It is the same parent-child arrangement as the bit
  tag, with one difference: here the child does not slice, it indexes.

  The block takes scanned values through TagCommandCallBack, which is
  protected - a probe exposes it, and with that the tests need no driver at
  all, they just hand over the values a driver would.
}
{$ENDIF}
unit ut.plcblock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, PLCBlock, PLCBlockElement;

type

  { TBlocoProbe }

  TBlocoProbe = class(TPLCBlock)
  public
    //: entrega valores ao bloco como uma varredura faria
    procedure ChegouDaVarredura(const aValores:TArrayOfDouble; aDeslocamento:LongInt;
                                aResultado:TProtocolIOResult = ioOk);
  end;

  { TTestPLCBlock }

  TTestPLCBlock = class(TTestCase)
  private
    FBloco:TBlocoProbe;
    FAvisos:LongInt;
    procedure ContarAviso(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //tamanho / size
    procedure TamanhoMudaAQuantidadeDeValores;
    procedure TamanhoZeroEhIgnorado;
    procedure ValoresNovosComecamEmZero;

    //acesso aos valores / value access
    procedure IndiceNegativoEhRecusado;
    procedure IndiceAlemDoFimEhRecusado;

    //valores vindos da varredura / values coming from a scan
    procedure VarreduraGuardaOsValores;
    procedure VarreduraComDeslocamentoGuardaNoLugarCerto;
    procedure MaisValoresDoQueCabeNaoTransborda;
    procedure FalhaNaLeituraNaoMudaOsValores;

    //avisos / notifications
    procedure MudancaDeValorAvisaQuemEscuta;
    procedure LeituraComOsMesmosValoresNaoAvisa;
  end;

  { TTestPLCBlockElement }

  TTestPLCBlockElement = class(TTestCase)
  private
    FBloco:TBlocoProbe;
    FElemento:TPLCBlockElement;
    FAvisos:LongInt;
    procedure ContarAviso(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ElementoLeOValorDoBloco;
    procedure ElementoAcompanhaAMudancaDoBloco;
    procedure EscreverNoElementoMudaOBloco;
    procedure IndiceAlemDoBlocoEhRecusado;
    procedure IndiceEscolhidoAntesDoBlocoEhConferidoDepois;
    procedure BlocoDestruidoDesligaOVinculo;
    procedure SemBlocoGuardaOValorLocalmente;
  end;

implementation

//: monta o vetor que um driver entregaria
function Valores(const aValores:array of Double):TArrayOfDouble;
var
  c:LongInt;
begin
  Result:=nil;
  SetLength(Result, Length(aValores));
  for c:=0 to High(aValores) do
    Result[c]:=aValores[c];
end;

{ TBlocoProbe }

procedure TBlocoProbe.ChegouDaVarredura(const aValores:TArrayOfDouble; aDeslocamento:LongInt;
                                        aResultado:TProtocolIOResult = ioOk);
begin
  TagCommandCallBack(0, aValores, GetTickCount64, tcScanRead, aResultado, aDeslocamento);
end;

{ TTestPLCBlock }

procedure TTestPLCBlock.SetUp;
begin
  FBloco:=TBlocoProbe.Create(nil);
  FBloco.Size:=4;
  FAvisos:=0;
end;

procedure TTestPLCBlock.TearDown;
begin
  FreeAndNil(FBloco);
end;

procedure TTestPLCBlock.ContarAviso(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestPLCBlock.TamanhoMudaAQuantidadeDeValores;
begin
  AssertEquals('tamanho inicial', 4, FBloco.Size);

  FBloco.Size:=7;
  AssertEquals('tamanho novo',    7, FBloco.Size);
  AssertEquals('valores',         7, Length(FBloco.ValuesRaw));
end;

procedure TTestPLCBlock.TamanhoZeroEhIgnorado;
begin
  //um bloco de tamanho zero nao le nada: o tamanho anterior fica de pe
  FBloco.Size:=0;
  AssertEquals('tamanho continua', 4, FBloco.Size);
end;

procedure TTestPLCBlock.ValoresNovosComecamEmZero;
begin
  FBloco.Size:=6;
  AssertEquals('valor novo', 0, FBloco.ValueRaw[5], 0);
end;

procedure TTestPLCBlock.IndiceNegativoEhRecusado;
var
  recusou:Boolean;
  lixo:Double;
begin
  recusou:=false;
  try
    lixo:=FBloco.ValueRaw[-1];
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('indice negativo', recusou);
end;

procedure TTestPLCBlock.IndiceAlemDoFimEhRecusado;
var
  recusou:Boolean;
  lixo:Double;
begin
  recusou:=false;
  try
    lixo:=FBloco.ValueRaw[4];
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('indice alem do fim', recusou);
end;

procedure TTestPLCBlock.VarreduraGuardaOsValores;
begin
  FBloco.ChegouDaVarredura(Valores([10, 20, 30, 40]), 0);

  AssertEquals('primeiro', 10, FBloco.ValueRaw[0], 0);
  AssertEquals('segundo',  20, FBloco.ValueRaw[1], 0);
  AssertEquals('terceiro', 30, FBloco.ValueRaw[2], 0);
  AssertEquals('quarto',   40, FBloco.ValueRaw[3], 0);
end;

procedure TTestPLCBlock.VarreduraComDeslocamentoGuardaNoLugarCerto;
begin
  //um driver pode entregar so' um pedaco do bloco, dizendo de onde ele comeca
  FBloco.ChegouDaVarredura(Valores([77, 88]), 2);

  AssertEquals('nao mexeu no comeco', 0,  FBloco.ValueRaw[0], 0);
  AssertEquals('terceiro',            77, FBloco.ValueRaw[2], 0);
  AssertEquals('quarto',              88, FBloco.ValueRaw[3], 0);
end;

procedure TTestPLCBlock.MaisValoresDoQueCabeNaoTransborda;
begin
  //o driver mandou mais do que o bloco comporta: o que cabe entra, o resto
  //nao pode escrever fora do vetor
  FBloco.ChegouDaVarredura(Valores([1, 2, 3, 4, 5, 6]), 0);

  AssertEquals('o que coube', 4, FBloco.ValueRaw[3], 0);
  AssertEquals('tamanho intacto', 4, FBloco.Size);
end;

procedure TTestPLCBlock.FalhaNaLeituraNaoMudaOsValores;
begin
  FBloco.ChegouDaVarredura(Valores([10, 20, 30, 40]), 0);
  FBloco.ChegouDaVarredura(Valores([99, 99, 99, 99]), 0, ioTimeOut);

  AssertEquals('valor antigo fica', 10, FBloco.ValueRaw[0], 0);
end;

procedure TTestPLCBlock.MudancaDeValorAvisaQuemEscuta;
begin
  FBloco.AddTagChangeHandler(@ContarAviso);
  FBloco.ChegouDaVarredura(Valores([10, 20, 30, 40]), 0);

  AssertTrue('mudanca tem que avisar', FAvisos>0);
end;

procedure TTestPLCBlock.LeituraComOsMesmosValoresNaoAvisa;
begin
  FBloco.ChegouDaVarredura(Valores([10, 20, 30, 40]), 0);
  FBloco.AddTagChangeHandler(@ContarAviso);
  FAvisos:=0;

  FBloco.ChegouDaVarredura(Valores([10, 20, 30, 40]), 0);
  AssertEquals('nada mudou, nada a avisar', 0, FAvisos);
end;

{ TTestPLCBlockElement }

procedure TTestPLCBlockElement.SetUp;
begin
  FBloco:=TBlocoProbe.Create(nil);
  FBloco.Size:=4;

  FElemento:=TPLCBlockElement.Create(nil);
  FAvisos:=0;
end;

procedure TTestPLCBlockElement.TearDown;
begin
  FreeAndNil(FElemento);
  FreeAndNil(FBloco);
end;

procedure TTestPLCBlockElement.ContarAviso(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestPLCBlockElement.ElementoLeOValorDoBloco;
begin
  FBloco.ChegouDaVarredura(nil, 0);
  FElemento.PLCBlock:=FBloco;
  FElemento.Index   :=2;

  FBloco.ValueRaw[2]:=55;
  AssertEquals('o elemento le do bloco', 55, FElemento.Value, 0);
end;

procedure TTestPLCBlockElement.ElementoAcompanhaAMudancaDoBloco;
begin
  FElemento.PLCBlock:=FBloco;
  FElemento.Index   :=1;
  FElemento.AddTagChangeHandler(@ContarAviso);
  FAvisos:=0;

  FBloco.ChegouDaVarredura(Valores([0, 42, 0, 0]), 0);

  AssertEquals('valor novo',        42, FElemento.Value, 0);
  AssertTrue  ('e avisou a mudanca', FAvisos>0);
end;

procedure TTestPLCBlockElement.EscreverNoElementoMudaOBloco;
begin
  FElemento.PLCBlock:=FBloco;
  FElemento.Index   :=3;

  FElemento.Value:=17;
  AssertEquals('o bloco recebeu', 17, FBloco.ValueRaw[3], 0);
end;

procedure TTestPLCBlockElement.IndiceAlemDoBlocoEhRecusado;
var
  recusou:Boolean;
begin
  FElemento.PLCBlock:=FBloco;

  recusou:=false;
  try
    FElemento.Index:=4;
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('indice alem do bloco', recusou);
end;

procedure TTestPLCBlockElement.IndiceEscolhidoAntesDoBlocoEhConferidoDepois;
begin
  //sem bloco qualquer indice e' aceito; ao vincular um bloco menor que ele, o
  //elemento fica apontando para fora
  FElemento.Index   :=9;
  FElemento.PLCBlock:=FBloco;

  AssertTrue('o indice tem que caber no bloco', FElemento.Index<FBloco.Size);
end;

procedure TTestPLCBlockElement.BlocoDestruidoDesligaOVinculo;
var
  bloco:TBlocoProbe;
begin
  bloco:=TBlocoProbe.Create(nil);
  bloco.Size:=2;
  FElemento.PLCBlock:=bloco;
  FElemento.Index   :=1;

  FreeAndNil(bloco);

  AssertTrue('o vinculo tem que ter sido desfeito', FElemento.PLCBlock=nil);
end;

procedure TTestPLCBlockElement.SemBlocoGuardaOValorLocalmente;
begin
  FElemento.Value:=8;
  AssertEquals('valor guardado', 8, FElemento.Value, 0);
end;

initialization
  RegisterTest(TTestPLCBlock);
  RegisterTest(TTestPLCBlockElement);

end.
