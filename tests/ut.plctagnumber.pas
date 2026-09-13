{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TPLCTagNumber e do contrato de TPLCNumber.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Este e' o tag numerico comum, o que a maioria dos projetos usa. Sem driver
  ligado ele nao fica inutil: escrever nele cai no proprio TagCommandCallBack
  com ioNullDriver, entao o valor fica guardado. E' por esse caminho que estes
  testes exercitam tudo sem precisar de CLP, porta ou driver.

  A parte que vem de TPLCNumber - limites de faixa, escala de engenharia e a
  formatacao do valor em texto - vale para todo tag numerico da biblioteca.
}
{$ELSE}
{:
  @abstract(TPLCTagNumber tests, and the TPLCNumber contract.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This is the ordinary numeric tag, the one most projects use. With no driver
  attached it is not useless: writing to it lands on its own TagCommandCallBack
  with ioNullDriver, so the value is kept. That is the path these tests use to
  exercise everything with no PLC, port or driver.

  What comes from TPLCNumber - range limits, engineering scale and formatting
  the value as text - holds for every numeric tag in the library.
}
{$ENDIF}
unit ut.plctagnumber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, PLCTagNumber, ValueProcessor;

type

  { TEscalaQueDobra }

  //: escala de teste: o valor do CLP vale o dobro para o usuario
  TEscalaQueDobra = class(TScaleProcessor)
  public
    function SetInGetOut(Sender:TComponent; aInput:Double):Double; override;
    function SetOutGetIn(Sender:TComponent; aOutput:Double):Double; override;
  end;

  { TTagProbe }

  TTagProbe = class(TPLCTagNumber)
  public
    //: entrega um valor ao tag como uma varredura faria
    procedure ChegouDaVarredura(aValor:Double; aResultado:TProtocolIOResult = ioOk);
  end;

  { TTestPLCTagNumber }

  TTestPLCTagNumber = class(TTestCase)
  private
    FTag:TTagProbe;
    FEscala:TEscalaQueDobra;
    FAvisos, FFalhasDeEscrita:LongInt;
    procedure ContarAviso(Sender:TObject);
    procedure ContarFalhaDeEscrita(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //valores vindos da varredura / values coming from a scan
    procedure ValorDaVarreduraFicaGuardado;
    procedure FalhaNaLeituraNaoMudaOValor;
    procedure MudancaDeValorAvisaQuemEscuta;
    procedure LeituraComOMesmoValorNaoAvisa;

    //escrita sem driver / writing with no driver
    procedure SemDriverOValorEscritoFicaGuardado;

    //limites de faixa / range limits
    procedure LimitesDesligadosDeixamPassarQualquerValor;
    procedure ValorAbaixoDoMinimoEhRecusado;
    procedure ValorAcimaDoMaximoEhRecusado;
    procedure ValorNaFaixaEhAceito;
    procedure ValorRecusadoAvisaFalhaDeEscrita;
    procedure ValorRecusadoNaoChegaAMudarOTag;
    procedure MinimoMaiorQueOMaximoEhRecusado;

    //escala de engenharia / engineering scale
    procedure AEscalaEhAplicadaNaLeitura;
    procedure AEscalaEhDesfeitaNaEscrita;
    procedure OValorPuroNaoPassaPelaEscala;
    procedure OsLimitesValemNaEscalaDeEngenharia;

    //valor em texto / value as text
    procedure TextoComPrefixoESufixo;
    procedure TextoComFormatoNumerico;
    procedure TextoSemFormatoUsaOPadrao;
    procedure TextoComFormatoDeHoraUsaOValorEmMilissegundos;
  end;

implementation

{ TEscalaQueDobra }

function TEscalaQueDobra.SetInGetOut(Sender:TComponent; aInput:Double):Double;
begin
  //do equipamento para o usuario
  Result:=aInput*2;
end;

function TEscalaQueDobra.SetOutGetIn(Sender:TComponent; aOutput:Double):Double;
begin
  //do usuario para o equipamento
  Result:=aOutput/2;
end;

{ TTagProbe }

procedure TTagProbe.ChegouDaVarredura(aValor:Double; aResultado:TProtocolIOResult = ioOk);
var
  valores:TArrayOfDouble;
begin
  SetLength(valores, 1);
  valores[0]:=aValor;
  TagCommandCallBack(0, valores, GetTickCount64, tcScanRead, aResultado, 0);
end;

{ TTestPLCTagNumber }

procedure TTestPLCTagNumber.SetUp;
begin
  FTag:=TTagProbe.Create(nil);
  FEscala:=TEscalaQueDobra.Create(nil);
  FAvisos:=0;
  FFalhasDeEscrita:=0;
end;

procedure TTestPLCTagNumber.TearDown;
begin
  FreeAndNil(FTag);
  FreeAndNil(FEscala);
end;

procedure TTestPLCTagNumber.ContarAviso(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestPLCTagNumber.ContarFalhaDeEscrita(Sender:TObject);
begin
  inc(FFalhasDeEscrita);
end;

procedure TTestPLCTagNumber.ValorDaVarreduraFicaGuardado;
begin
  FTag.ChegouDaVarredura(42);
  AssertEquals('valor da varredura', 42, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.FalhaNaLeituraNaoMudaOValor;
begin
  FTag.ChegouDaVarredura(42);
  FTag.ChegouDaVarredura(99, ioTimeOut);

  AssertEquals('o valor bom fica', 42, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.MudancaDeValorAvisaQuemEscuta;
begin
  FTag.AddTagChangeHandler(@ContarAviso);
  FTag.ChegouDaVarredura(42);

  AssertTrue('mudanca tem que avisar', FAvisos>0);
end;

procedure TTestPLCTagNumber.LeituraComOMesmoValorNaoAvisa;
begin
  FTag.ChegouDaVarredura(42);
  FTag.AddTagChangeHandler(@ContarAviso);
  FAvisos:=0;

  FTag.ChegouDaVarredura(42);
  AssertEquals('nada mudou, nada a avisar', 0, FAvisos);
end;

procedure TTestPLCTagNumber.SemDriverOValorEscritoFicaGuardado;
begin
  //sem driver a escrita volta pelo proprio callback, com ioNullDriver
  FTag.Value:=17;
  AssertEquals('valor guardado', 17, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.LimitesDesligadosDeixamPassarQualquerValor;
begin
  FTag.SetMinMaxValues(0, 10);
  FTag.Value:=1000;
  AssertEquals('sem limites ligados', 1000, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.ValorAbaixoDoMinimoEhRecusado;
var
  recusou:Boolean;
begin
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMinValue:=true;

  recusou:=false;
  try
    FTag.Value:=-1;
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('abaixo do minimo', recusou);
end;

procedure TTestPLCTagNumber.ValorAcimaDoMaximoEhRecusado;
var
  recusou:Boolean;
begin
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMaxValue:=true;

  recusou:=false;
  try
    FTag.Value:=101;
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('acima do maximo', recusou);
end;

procedure TTestPLCTagNumber.ValorNaFaixaEhAceito;
begin
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMinValue:=true;
  FTag.EnableMaxValue:=true;

  FTag.Value:=50;
  AssertEquals('valor na faixa', 50, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.ValorRecusadoAvisaFalhaDeEscrita;
begin
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMaxValue:=true;
  FTag.AddWriteFaultHandler(@ContarFalhaDeEscrita);

  try
    FTag.Value:=200;
  except
    on E:Exception do ;
  end;

  AssertTrue('quem escuta tem que saber da recusa', FFalhasDeEscrita>0);
end;

procedure TTestPLCTagNumber.ValorRecusadoNaoChegaAMudarOTag;
begin
  FTag.ChegouDaVarredura(10);
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMaxValue:=true;

  try
    FTag.Value:=200;
  except
    on E:Exception do ;
  end;

  AssertEquals('o valor anterior fica de pe', 10, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.MinimoMaiorQueOMaximoEhRecusado;
var
  recusou:Boolean;
begin
  recusou:=false;
  try
    FTag.SetMinMaxValues(100, 0);
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('faixa invertida', recusou);
end;

procedure TTestPLCTagNumber.AEscalaEhAplicadaNaLeitura;
begin
  //o equipamento manda 21, o usuario ve 42
  FTag.ScaleProcessor:=FEscala;
  FTag.ChegouDaVarredura(21);

  AssertEquals('valor em engenharia', 42, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.AEscalaEhDesfeitaNaEscrita;
begin
  //o usuario escreve 42, o equipamento recebe 21
  FTag.ScaleProcessor:=FEscala;
  FTag.Value:=42;

  AssertEquals('valor puro', 21, FTag.ValueRaw, 0);
end;

procedure TTestPLCTagNumber.OValorPuroNaoPassaPelaEscala;
begin
  FTag.ScaleProcessor:=FEscala;
  FTag.ChegouDaVarredura(21);

  AssertEquals('o valor puro e o que veio do equipamento', 21, FTag.ValueRaw, 0);
end;

procedure TTestPLCTagNumber.OsLimitesValemNaEscalaDeEngenharia;
var
  recusou:Boolean;
begin
  //os limites sao conferidos antes da escala: sao a faixa que o usuario ve
  FTag.ScaleProcessor:=FEscala;
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMaxValue:=true;

  recusou:=false;
  try
    FTag.Value:=150;
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('150 em engenharia passa do limite', recusou);

  //e 100 em engenharia passa, mesmo virando 50 no equipamento
  FTag.Value:=100;
  AssertEquals('no limite', 50, FTag.ValueRaw, 0);
end;

procedure TTestPLCTagNumber.TextoComPrefixoESufixo;
begin
  FTag.ChegouDaVarredura(25);
  AssertEquals('com prefixo e sufixo', 'T= 25 C', FTag.GetValueAsText('T= ', ' C', ''));
end;

procedure TTestPLCTagNumber.TextoComFormatoNumerico;
begin
  FTag.ChegouDaVarredura(3.14159);
  AssertEquals('duas casas', '3.14', FTag.GetValueAsText('', '', '0.00'));
end;

procedure TTestPLCTagNumber.TextoSemFormatoUsaOPadrao;
begin
  FTag.ChegouDaVarredura(7);
  AssertEquals('sem formato', '7', FTag.GetValueAsText('', '', ''));
end;

procedure TTestPLCTagNumber.TextoComFormatoDeHoraUsaOValorEmMilissegundos;
begin
  //com formato de data ou hora o valor e' lido como milissegundos
  FTag.ChegouDaVarredura(3661000);
  AssertEquals('uma hora, um minuto e um segundo', '01:01:01',
               FTag.GetValueAsText('', '', 'hh:nn:ss'));
end;

initialization
  RegisterTest(TTestPLCTagNumber);

end.
