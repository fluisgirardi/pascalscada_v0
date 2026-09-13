{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TPLCStruct: leitura tipada sobre um bloco de bytes.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A estrutura guarda bytes crus e oferece leitura tipada por deslocamento:
  palavra, palavra dupla, ponto flutuante, texto. Como equipamento nenhum
  concorda sobre a ordem dos bytes, cada leitura aceita as trocas de bytes, de
  palavras e de palavras duplas - e e' justamente ai' que mora o erro caro,
  porque um valor lido na ordem errada nao parece errado, parece outro valor.

  Os bytes entram na estrutura pelo mesmo caminho de uma varredura, por
  TagCommandCallBack, que uma sonda expoe.
}
{$ELSE}
{:
  @abstract(TPLCStruct tests: typed reading over a block of bytes.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The struct keeps raw bytes and offers typed reading by offset: word, double
  word, floating point, text. Since no two devices agree on byte order, every
  read takes the byte, word and double word swaps - and that is exactly where
  the expensive mistake lives, because a value read in the wrong order does not
  look wrong, it looks like another value.

  The bytes enter the struct the same way a scan delivers them, through
  TagCommandCallBack, which a probe exposes.
}
{$ENDIF}
unit ut.plcstruct;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, PLCStruct, PLCStructElement;

type

  { TEstruturaProbe }

  TEstruturaProbe = class(TPLCStruct)
  public
    //: poe os bytes na estrutura como uma varredura faria
    procedure ChegouDaVarredura(const aBytes:array of Byte);
  end;

  { TTestPLCStruct }

  TTestPLCStruct = class(TTestCase)
  private
    FEstrutura:TEstruturaProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //leitura crua / raw reading
    procedure ByteNoDeslocamentoPedido;
    procedure ByteAlemDoFimEhRecusado;

    //palavra / word
    procedure PalavraNaOrdemDoProcessador;
    procedure PalavraComBytesTrocados;
    procedure PalavraComSinal;
    procedure PalavraAlemDoFimEhRecusada;

    //palavra dupla / double word
    procedure PalavraDuplaNaOrdemDoProcessador;
    procedure PalavraDuplaComPalavrasTrocadas;
    procedure PalavraDuplaComBytesEPalavrasTrocados;
    procedure InteiroComSinalDePalavraDupla;

    //ponto flutuante / floating point
    procedure PontoFlutuanteSimples;
    procedure PontoFlutuanteSimplesComTrocaCompleta;
    procedure PontoFlutuanteDuplo;
    procedure TrocaDePalavrasEmSessentaEQuatroBitsEhRespeitada;

    //texto no formato siemens / siemens string format
    procedure TextoSiemensTemTamanhoMaximoEAtual;
    procedure TextoSiemensParaNoTamanhoAtual;
    procedure TextoSiemensNoFimDaEstruturaNaoEstoura;
  end;

  {$IFDEF PORTUGUES}
  {:
  O item da estrutura: uma janela tipada num deslocamento dela. E' ele que
  transforma os bytes crus no valor que o projeto usa, e quem diz quantos
  bytes ler e' o tipo escolhido no item, nao a estrutura.
  }
  {$ELSE}
  {:
  The struct item: a typed window at an offset of it. It is what turns the raw
  bytes into the value the project uses, and what says how many bytes to read
  is the type chosen on the item, not the struct.
  }
  {$ENDIF}

  { TTestPLCStructItem }

  TTestPLCStructItem = class(TTestCase)
  private
    FEstrutura:TEstruturaProbe;
    FItem:TPLCStructItem;
    FAvisos:LongInt;
    procedure ContarAviso(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //leitura tipada / typed reading
    procedure ItemDeByteLeUmByte;
    procedure ItemDePalavraLeDoisBytes;
    procedure ItemDePalavraDuplaLeQuatroBytes;
    procedure ItemDePontoFlutuanteLeQuatroBytes;
    procedure ItemLeNoDeslocamentoEscolhido;
    procedure TrocarOTipoTrocaQuantosBytesSaoLidos;

    //limites / bounds
    procedure DeslocamentoQueNaoCabeNaEstruturaEhRecusado;
    procedure UltimoDeslocamentoQueCabeEhAceito;

    //ciclo de vida / lifecycle
    procedure ItemAcompanhaAMudancaDaEstrutura;
    procedure EstruturaDestruidaDesligaOVinculo;
    procedure SemEstruturaGuardaOValorLocalmente;
  end;

implementation

{ TEstruturaProbe }

procedure TEstruturaProbe.ChegouDaVarredura(const aBytes:array of Byte);
var
  valores:TArrayOfDouble;
  c:LongInt;
begin
  valores:=nil;
  SetLength(valores, Length(aBytes));
  for c:=0 to High(aBytes) do
    valores[c]:=aBytes[c];

  TagCommandCallBack(0, valores, GetTickCount64, tcScanRead, ioOk, 0);
end;

{ TTestPLCStruct }

procedure TTestPLCStruct.SetUp;
begin
  FEstrutura:=TEstruturaProbe.Create(nil);
  FEstrutura.Size:=16;
end;

procedure TTestPLCStruct.TearDown;
begin
  FreeAndNil(FEstrutura);
end;

procedure TTestPLCStruct.ByteNoDeslocamentoPedido;
begin
  FEstrutura.ChegouDaVarredura([$0A, $0B, $0C, $0D]);

  AssertEquals('primeiro', $0A, FEstrutura.GetByte(0));
  AssertEquals('terceiro', $0C, FEstrutura.GetByte(2));
end;

procedure TTestPLCStruct.ByteAlemDoFimEhRecusado;
var
  recusou:Boolean;
  lixo:Byte;
begin
  recusou:=false;
  try
    lixo:=FEstrutura.GetByte(16);
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('byte alem do fim', recusou);
end;

procedure TTestPLCStruct.PalavraNaOrdemDoProcessador;
begin
  //sem troca, o primeiro byte e' o menos significativo
  FEstrutura.ChegouDaVarredura([$34, $12]);
  AssertEquals('palavra', $1234, FEstrutura.GetWord(0, false));
end;

procedure TTestPLCStruct.PalavraComBytesTrocados;
begin
  //com troca, o primeiro byte e' o mais significativo - a ordem do S7
  FEstrutura.ChegouDaVarredura([$12, $34]);
  AssertEquals('palavra trocada', $1234, FEstrutura.GetWord(0, true));
end;

procedure TTestPLCStruct.PalavraComSinal;
begin
  //$FFFF vale menos um quando lido com sinal
  FEstrutura.ChegouDaVarredura([$FF, $FF]);
  AssertEquals('menos um', -1, FEstrutura.GetSmallInt(0, false));
end;

procedure TTestPLCStruct.PalavraAlemDoFimEhRecusada;
var
  recusou:Boolean;
  lixo:Word;
begin
  //ler dois bytes precisa dos dois: o ultimo byte sozinho nao serve
  recusou:=false;
  try
    lixo:=FEstrutura.GetWord(15, false);
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('palavra alem do fim', recusou);
end;

procedure TTestPLCStruct.PalavraDuplaNaOrdemDoProcessador;
begin
  FEstrutura.ChegouDaVarredura([$78, $56, $34, $12]);
  AssertEquals('palavra dupla', $12345678, FEstrutura.GetLongWord(0, false, false));
end;

procedure TTestPLCStruct.PalavraDuplaComPalavrasTrocadas;
begin
  //so' as palavras trocadas: cada metade fica na ordem do processador
  FEstrutura.ChegouDaVarredura([$34, $12, $78, $56]);
  AssertEquals('palavras trocadas', $12345678, FEstrutura.GetLongWord(0, false, true));
end;

procedure TTestPLCStruct.PalavraDuplaComBytesEPalavrasTrocados;
begin
  //tudo trocado e' a ordem de rede, byte mais significativo primeiro
  FEstrutura.ChegouDaVarredura([$12, $34, $56, $78]);
  AssertEquals('ordem de rede', $12345678, FEstrutura.GetLongWord(0, true, true));
end;

procedure TTestPLCStruct.InteiroComSinalDePalavraDupla;
begin
  FEstrutura.ChegouDaVarredura([$FF, $FF, $FF, $FF]);
  AssertEquals('menos um', -1, FEstrutura.GetLongInt(0, false, false));
end;

procedure TTestPLCStruct.PontoFlutuanteSimples;
begin
  //1.0 em ponto flutuante simples e' $3F800000
  FEstrutura.ChegouDaVarredura([$00, $00, $80, $3F]);
  AssertEquals('um', 1, FEstrutura.GetSingle(0, false, false), 0.0001);
end;

procedure TTestPLCStruct.PontoFlutuanteSimplesComTrocaCompleta;
begin
  //o mesmo 1.0 na ordem de rede, que e' como o S7 entrega
  FEstrutura.ChegouDaVarredura([$3F, $80, $00, $00]);
  AssertEquals('um', 1, FEstrutura.GetSingle(0, true, true), 0.0001);
end;

procedure TTestPLCStruct.PontoFlutuanteDuplo;
begin
  //1.0 em ponto flutuante duplo e' $3FF0000000000000
  FEstrutura.ChegouDaVarredura([$00, $00, $00, $00, $00, $00, $F0, $3F]);
  AssertEquals('um', 1, FEstrutura.GetDouble(0, false, false, false), 0.0001);
end;

procedure TTestPLCStruct.TrocaDePalavrasEmSessentaEQuatroBitsEhRespeitada;
begin
  //a troca de palavras e' pedida na chamada. A propriedade SwapWords do tag
  //nao serve aqui: numa estrutura ela e' presa em falso de proposito, porque o
  //bloco e' de bytes crus e cada leitura escolhe a sua ordem
  FEstrutura.ChegouDaVarredura([$01, $02, $03, $04, $05, $06, $07, $08]);

  AssertTrue('sem troca de palavras',
             QWord($0807060504030201) = FEstrutura.GetQWord(0, false, false, false));
  AssertTrue('com troca de palavras',
             QWord($0605080702010403) = FEstrutura.GetQWord(0, false, true, false));
end;

procedure TTestPLCStruct.TextoSiemensTemTamanhoMaximoEAtual;
begin
  //o formato do S7: tamanho maximo, tamanho atual e os caracteres
  FEstrutura.ChegouDaVarredura([10, 3, Ord('a'), Ord('b'), Ord('c'), 0, 0, 0]);
  AssertEquals('texto', 'abc', FEstrutura.GetSiemensString(0));
end;

procedure TTestPLCStruct.TextoSiemensParaNoTamanhoAtual;
begin
  //o que vem depois do tamanho atual e' resto de escrita anterior, nao texto
  FEstrutura.ChegouDaVarredura([10, 2, Ord('a'), Ord('b'), Ord('c'), Ord('d')]);
  AssertEquals('so o que foi declarado', 'ab', FEstrutura.GetSiemensString(0));
end;

procedure TTestPLCStruct.TextoSiemensNoFimDaEstruturaNaoEstoura;
var
  texto:String;
  estourou:Boolean;
begin
  //a estrutura tem 16 bytes; o texto comeca no 12 e declara 10 caracteres, que
  //nao cabem. Tem que devolver o que cabe, nao derrubar a leitura
  FEstrutura.ChegouDaVarredura([0,0,0,0,0,0,0,0,0,0,0,0, 10, 10, Ord('a'), Ord('b')]);

  estourou:=false;
  texto:='';
  try
    texto:=FEstrutura.GetSiemensString(12);
  except
    on E:Exception do estourou:=true;
  end;

  AssertFalse('a leitura nao pode estourar', estourou);
  AssertEquals('o que cabe', 'ab', texto);
end;

{ TTestPLCStructItem }

procedure TTestPLCStructItem.SetUp;
begin
  FEstrutura:=TEstruturaProbe.Create(nil);
  FEstrutura.Size:=16;

  FItem:=TPLCStructItem.Create(nil);
  FAvisos:=0;
end;

procedure TTestPLCStructItem.TearDown;
begin
  FreeAndNil(FItem);
  FreeAndNil(FEstrutura);
end;

procedure TTestPLCStructItem.ContarAviso(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestPLCStructItem.ItemDeByteLeUmByte;
begin
  FEstrutura.ChegouDaVarredura([$0A, $0B, $0C, $0D]);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttByte;
  FItem.Index   :=0;

  AssertEquals('um byte', $0A, FItem.Value, 0);
end;

procedure TTestPLCStructItem.ItemDePalavraLeDoisBytes;
begin
  FEstrutura.ChegouDaVarredura([$34, $12, $00, $00]);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttWord;
  FItem.Index   :=0;

  AssertEquals('uma palavra', $1234, FItem.Value, 0);
end;

procedure TTestPLCStructItem.ItemDePalavraDuplaLeQuatroBytes;
begin
  FEstrutura.ChegouDaVarredura([$78, $56, $34, $12]);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttDWord;
  FItem.Index   :=0;

  AssertEquals('uma palavra dupla', $12345678, FItem.Value, 0);
end;

procedure TTestPLCStructItem.ItemDePontoFlutuanteLeQuatroBytes;
begin
  //1.0 em ponto flutuante simples
  FEstrutura.ChegouDaVarredura([$00, $00, $80, $3F]);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttFloat;
  FItem.Index   :=0;

  AssertEquals('um', 1, FItem.Value, 0.0001);
end;

procedure TTestPLCStructItem.ItemLeNoDeslocamentoEscolhido;
begin
  //e' assim que se mapeia um membro no meio da estrutura
  FEstrutura.ChegouDaVarredura([$00, $00, $00, $00, $EF, $BE]);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttWord;
  FItem.Index   :=4;

  AssertEquals('no deslocamento quatro', $BEEF, FItem.Value, 0);
end;

procedure TTestPLCStructItem.TrocarOTipoTrocaQuantosBytesSaoLidos;
begin
  FEstrutura.ChegouDaVarredura([$34, $12, $00, $00]);
  FItem.PLCBlock:=FEstrutura;
  FItem.Index   :=0;

  FItem.TagType:=pttByte;
  AssertEquals('como byte',   $34,   FItem.Value, 0);

  FItem.TagType:=pttWord;
  AssertEquals('como palavra', $1234, FItem.Value, 0);
end;

procedure TTestPLCStructItem.DeslocamentoQueNaoCabeNaEstruturaEhRecusado;
var
  recusou:Boolean;
begin
  //uma palavra dupla no byte 14 de uma estrutura de 16 passa do fim
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttDWord;

  recusou:=false;
  try
    FItem.Index:=14;
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('deslocamento que nao cabe', recusou);
end;

procedure TTestPLCStructItem.UltimoDeslocamentoQueCabeEhAceito;
begin
  //a mesma palavra dupla no byte 12 cabe exatamente
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttDWord;
  FItem.Index   :=12;

  AssertEquals('ultimo que cabe', 12, FItem.Index);
end;

procedure TTestPLCStructItem.ItemAcompanhaAMudancaDaEstrutura;
begin
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttByte;
  FItem.Index   :=1;

  FEstrutura.ChegouDaVarredura([$00, $42]);
  AssertEquals('valor novo', $42, FItem.Value, 0);
end;

procedure TTestPLCStructItem.EstruturaDestruidaDesligaOVinculo;
var
  estrutura:TEstruturaProbe;
begin
  estrutura:=TEstruturaProbe.Create(nil);
  estrutura.Size:=8;
  FItem.PLCBlock:=estrutura;
  FItem.TagType :=pttByte;
  FItem.Index   :=0;

  FreeAndNil(estrutura);

  AssertTrue('o vinculo tem que ter sido desfeito', FItem.PLCBlock=nil);
end;

procedure TTestPLCStructItem.SemEstruturaGuardaOValorLocalmente;
begin
  //sem estrutura o item ainda e' um tag: guarda o que escrevem nele
  FItem.TagType:=pttByte;
  FItem.Value  :=9;

  AssertEquals('valor guardado', 9, FItem.Value, 0);
end;

initialization
  RegisterTest(TTestPLCStruct);
  RegisterTest(TTestPLCStructItem);

end.
