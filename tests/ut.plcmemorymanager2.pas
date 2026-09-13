{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TPLCMemoryManager: montagem dos blocos de scan e o
            caminho de dados que todo driver usa.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Esta e' a peca que decide quantos pedidos vao para o CLP a cada varredura -
  enderecos vizinhos viram um bloco so, buracos pequenos sao engolidos para
  poupar pedidos, e blocos grandes demais sao fatiados. Depois da leitura, e'
  ela tambem que distribui os bytes recebidos nos blocos certos.

  Os testes complementam os de ut.plcmemorymanager, que cobrem a formacao
  basica dos blocos.
}
{$ELSE}
{:
  @abstract(TPLCMemoryManager tests: scan block building and the data path
            every driver uses.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This is the piece that decides how many requests go to the PLC on each scan
  - neighbouring addresses become a single block, small holes are swallowed to
  save requests, and oversized blocks are sliced. After the read, it is also
  what spreads the received bytes over the right blocks.

  These complement ut.plcmemorymanager, which covers the basic block forming.
}
{$ENDIF}
unit ut.plcmemorymanager2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, PLCMemoryManager;

type

  { TTestGerenciadorDeMemoria }

  TTestGerenciadorDeMemoria = class(TTestCase)
  private
    FMM:TPLCMemoryManager;
    function  Valores(const aValores:array of Double):TArrayOfDouble;
    function  LeBloco(aInicio, aTamanho:LongInt; out aResultado:TProtocolIOResult):TArrayOfDouble;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //formacao e desmonte dos blocos / block forming and unforming
    procedure EnderecosRepetidosNaoDuplicam;
    procedure RemoverTudoDesfazOsBlocos;
    procedure RemocaoNoMeioQuebraOBlocoEmDois;
    procedure BuracoDoTamanhoDoLimiteAindaUne;
    procedure BuracoUmAlemDoLimiteSepara;
    procedure TamanhoContaOBuracoEngolido;

    //o RegSize / the RegSize parameter
    procedure RegSizeMultiplicaAQuantidadeDeEnderecos;
    procedure RemocaoPrecisaDoMesmoRegSize;

    //caminho de dados / data path
    procedure ValoresGravadosSaoLidosDeVolta;
    procedure EscritaCobrindoDoisBlocosPreencheOsDois;
    procedure EscritaForaDosBlocosNaoDerruba;
    procedure LeituraParcialAvisaPeloRetorno;
    procedure FalhaFicaGuardadaNoBloco;
    procedure TamanhoDoSetValuesVemDoVetorNaoDoParametro;

    //varredura / scanning
    procedure BlocoRecemCriadoJaNasceCarimbado;
    procedure MenorTempoDeScanVenceEntreOsBlocos;
  end;

implementation

procedure TTestGerenciadorDeMemoria.SetUp;
begin
  FMM:=TPLCMemoryManager.Create;
  FMM.MaxHole:=5;
  FMM.MaxBlockItems:=100;
end;

procedure TTestGerenciadorDeMemoria.TearDown;
begin
  FreeAndNil(FMM);
end;

function TTestGerenciadorDeMemoria.Valores(const aValores:array of Double):TArrayOfDouble;
var
  i:LongInt;
begin
  SetLength(Result, Length(aValores));
  for i:=0 to High(aValores) do
    Result[i]:=aValores[i];
end;

function TTestGerenciadorDeMemoria.LeBloco(aInicio, aTamanho:LongInt; out aResultado:TProtocolIOResult):TArrayOfDouble;
var
  carimbo:QWord;
begin
  SetLength(Result, aTamanho);
  aResultado:=ioNone;
  carimbo:=0;
  FMM.GetValues(aInicio, aTamanho, 1, Result, aResultado, carimbo);
end;

procedure TTestGerenciadorDeMemoria.EnderecosRepetidosNaoDuplicam;
begin
  //dois tags pedindo a mesma faixa nao podem dobrar o que se le do CLP
  FMM.AddAddress(0, 10, 1, 1000);
  FMM.AddAddress(0, 10, 1, 1000);

  AssertEquals('a single block', 1, Length(FMM.Blocks));
  AssertEquals('and the same size', 10, FMM.Size);
end;

procedure TTestGerenciadorDeMemoria.RemoverTudoDesfazOsBlocos;
begin
  FMM.AddAddress(0, 10, 1, 1000);
  FMM.RemoveAddress(0, 10, 1);

  AssertEquals('no blocks', 0, Length(FMM.Blocks));
  AssertEquals('size zeroed', 0, FMM.Size);
end;

procedure TTestGerenciadorDeMemoria.RemocaoNoMeioQuebraOBlocoEmDois;
begin
  //tirar o miolo tem que abrir um buraco maior que o limite
  FMM.AddAddress(0, 20, 1, 1000);
  FMM.RemoveAddress(6, 8, 1);

  AssertEquals('two blocks', 2, Length(FMM.Blocks));
  AssertEquals('end of the first one',   5,  FMM.Blocks[0].AddressEnd);
  AssertEquals('start of the second one', 14, FMM.Blocks[1].AddressStart);
end;

procedure TTestGerenciadorDeMemoria.BuracoDoTamanhoDoLimiteAindaUne;
begin
  //MaxHole=5: enderecos 0..9 e 15..16 deixam um buraco de exatamente 5
  FMM.AddAddress(0, 10, 1, 1000);
  FMM.AddAddress(15, 2, 1, 1000);

  AssertEquals('a hole of 5 must join', 1, Length(FMM.Blocks));
  AssertEquals('end of the block', 16, FMM.Blocks[0].AddressEnd);
end;

procedure TTestGerenciadorDeMemoria.BuracoUmAlemDoLimiteSepara;
begin
  //um a mais e o bloco se parte
  FMM.AddAddress(0, 10, 1, 1000);
  FMM.AddAddress(16, 2, 1, 1000);

  AssertEquals('a hole of 6 must split', 2, Length(FMM.Blocks));
end;

procedure TTestGerenciadorDeMemoria.TamanhoContaOBuracoEngolido;
begin
  //Size soma a extensao dos blocos, nao os enderecos pedidos: o buraco
  //engolido tambem e' lido do CLP, entao conta como trafego.
  FMM.AddAddress(0, 10, 1, 1000);
  FMM.AddAddress(13, 2, 1, 1000);

  AssertEquals('one block', 1, Length(FMM.Blocks));
  AssertEquals('the block goes from 0 to 14', 14, FMM.Blocks[0].AddressEnd);
  AssertEquals('12 addresses asked for, 15 read', 15, FMM.Size);
end;

procedure TTestGerenciadorDeMemoria.RegSizeMultiplicaAQuantidadeDeEnderecos;
begin
  //RegSize e' quantas palavras minimas do equipamento cada variavel ocupa:
  //2 variaveis de 2 bytes = 4 enderecos, nao 2.
  FMM.AddAddress(0, 2, 2, 1000);

  AssertEquals('one block', 1, Length(FMM.Blocks));
  AssertEquals('four addresses', 4, FMM.Size);
  AssertEquals('from 0 to 3', 3, FMM.Blocks[0].AddressEnd);
end;

procedure TTestGerenciadorDeMemoria.RemocaoPrecisaDoMesmoRegSize;
begin
  //quem removeu com RegSize menor deixa sobra para tras
  FMM.AddAddress(0, 2, 2, 1000);
  FMM.RemoveAddress(0, 2, 1);

  AssertEquals('two addresses are left', 2, FMM.Size);

  FMM.RemoveAddress(2, 2, 1);
  AssertEquals('now it is empty', 0, FMM.Size);
end;

procedure TTestGerenciadorDeMemoria.ValoresGravadosSaoLidosDeVolta;
var
  lidos:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  FMM.AddAddress(0, 4, 1, 1000);
  FMM.SetValues(0, 4, 1, Valores([10, 20, 30, 40]), ioOk);

  lidos:=LeBloco(0, 4, res);

  AssertEquals('result kept', Ord(ioOk), Ord(res));
  AssertEquals('first value', 10, lidos[0], 0);
  AssertEquals('last value',   40, lidos[3], 0);
end;

procedure TTestGerenciadorDeMemoria.EscritaCobrindoDoisBlocosPreencheOsDois;
var
  lidos:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  //dois blocos separados, e uma resposta que cobre os dois
  FMM.AddAddress(0, 4, 1, 1000);
  FMM.AddAddress(20, 4, 1, 1000);
  AssertEquals('two blocks', 2, Length(FMM.Blocks));

  FMM.SetValues(0, 24, 1,
                Valores([1,2,3,4, 0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0, 21,22,23,24]),
                ioOk);

  lidos:=LeBloco(0, 4, res);
  AssertEquals('first block', 1, lidos[0], 0);

  lidos:=LeBloco(20, 4, res);
  AssertEquals('second block, first value', 21, lidos[0], 0);
  AssertEquals('second block, last value',   24, lidos[3], 0);
end;

procedure TTestGerenciadorDeMemoria.EscritaForaDosBlocosNaoDerruba;
var
  lidos:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  FMM.AddAddress(0, 4, 1, 1000);
  FMM.SetValues(0, 4, 1, Valores([10, 20, 30, 40]), ioOk);

  //uma resposta para uma faixa que ninguem pediu tem que ser inocua
  FMM.SetValues(500, 4, 1, Valores([1,2,3,4]), ioOk);

  lidos:=LeBloco(0, 4, res);
  AssertEquals('the existing block stays untouched', 10, lidos[0], 0);
end;

procedure TTestGerenciadorDeMemoria.LeituraParcialAvisaPeloRetorno;
var
  lidos:TArrayOfDouble;
  res:TProtocolIOResult;
  carimbo:QWord;
  retorno:LongInt;
begin
  FMM.AddAddress(0, 4, 1, 1000);
  FMM.SetValues(0, 4, 1, Valores([10, 20, 30, 40]), ioOk);

  //pedir 4 enderecos que existem: cobertura total
  SetLength(lidos, 4);
  res:=ioNone;
  carimbo:=0;
  retorno:=FMM.GetValues(0, 4, 1, lidos, res, carimbo);
  AssertEquals('full coverage', 0, retorno);

  //pedir alem do que existe: cobertura parcial
  SetLength(lidos, 8);
  retorno:=FMM.GetValues(0, 8, 1, lidos, res, carimbo);
  AssertTrue('partial coverage must be flagged', retorno<>0);
end;

procedure TTestGerenciadorDeMemoria.FalhaFicaGuardadaNoBloco;
var
  lidos:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  FMM.AddAddress(0, 4, 1, 1000);
  FMM.SetValues(0, 4, 1, Valores([10, 20, 30, 40]), ioOk);

  FMM.SetFault(0, 4, 1, ioTimeOut);

  lidos:=LeBloco(0, 4, res);
  AssertEquals('whoever reads later needs to know about the failure', Ord(ioTimeOut), Ord(res));
end;

procedure TTestGerenciadorDeMemoria.TamanhoDoSetValuesVemDoVetorNaoDoParametro;
var
  lidos:TArrayOfDouble;
  res:TProtocolIOResult;
begin
  //surpresa da API: o SetValues usa Length(Values) para saber ate onde grava;
  //o parametro Len e o RegSize nao entram nessa conta. Quem chamar com Len
  //menor que o vetor grava tudo assim mesmo.
  FMM.AddAddress(0, 4, 1, 1000);
  FMM.SetValues(0, 1, 1, Valores([10, 20, 30, 40]), ioOk);

  lidos:=LeBloco(0, 4, res);
  AssertEquals('the fourth value was stored too', 40, lidos[3], 0);
end;

procedure TTestGerenciadorDeMemoria.BlocoRecemCriadoJaNasceCarimbado;
begin
  //um bloco recem montado ja vem com o carimbo de atualizado, entao nao pede
  //leitura imediata: a primeira vem depois de um periodo de scan. Vale saber
  //disso ao acrescentar um tag com o driver ja rodando.
  FMM.AddAddress(0, 4, 1, 1000);

  AssertFalse ('does not ask for an immediate read', FMM.Blocks[0].NeedRefresh);
  AssertTrue  ('the timestamp is recent', FMM.Blocks[0].MilisecondsFromLastUpdate<1000);
  AssertEquals('and it inherits the scan time asked for', 1000, FMM.Blocks[0].ScanTime);
end;

procedure TTestGerenciadorDeMemoria.MenorTempoDeScanVenceEntreOsBlocos;
begin
  //o driver varre no ritmo do tag mais exigente
  FMM.AddAddress(0, 4, 1, 1000);
  FMM.AddAddress(20, 4, 1, 250);

  AssertEquals('shortest scan time', 250, FMM.MinScanTime);
end;

initialization
  RegisterTest(TTestGerenciadorDeMemoria);

end.
