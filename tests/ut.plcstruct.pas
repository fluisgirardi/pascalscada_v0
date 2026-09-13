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
  Tag, ProtocolTypes, PLCStruct, PLCStructElement, PLCStructString, PLCString;

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

  {$IFDEF PORTUGUES}
  {:
  O texto dentro da estrutura. Cada fabricante guarda texto do seu jeito: o C
  termina no zero, o Siemens poe tamanho maximo e atual na frente, o Rockwell
  poe o tamanho numa palavra dupla. O tag precisa saber qual e', e reservar o
  cabecalho de cada um dentro da estrutura.
  }
  {$ELSE}
  {:
  Text inside the struct. Every maker stores text its own way: the C one ends
  at the zero byte, the Siemens one puts maximum and current length up front,
  the Rockwell one puts the length in a double word. The tag has to know which,
  and to reserve each one's header inside the struct.
  }
  {$ENDIF}

  { TTestPLCStructString }

  TTestPLCStructString = class(TTestCase)
  private
    FEstrutura:TEstruturaProbe;
    FTexto:TPLCStructString;
    FAvisos:LongInt;
    procedure ContarAviso(Sender:TObject);
    procedure Configurar(aTipo:TPLCStringTypes; aIndice, aTamanho:Cardinal);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //os tres formatos / the three formats
    procedure TextoNoFormatoCTerminaNoZero;
    procedure TextoNoFormatoCRespeitaOTamanhoMaximo;
    procedure TextoNoFormatoSiemensEhLido;
    procedure TextoNoFormatoRockwellUsaOTamanhoDaFrente;

    //deslocamento e mudanca / offset and change
    procedure TextoEhLidoNoDeslocamentoEscolhido;
    procedure MudancaNaEstruturaAtualizaOTexto;

    //o que cabe / what fits
    procedure CadaFormatoReservaOSeuCabecalho;
    procedure DeslocamentoQueNaoCabeEhIgnoradoEmSilencio;

    //ciclo de vida / lifecycle
    procedure EstruturaDestruidaDesligaOVinculo;
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

  AssertEquals('first', $0A, FEstrutura.GetByte(0));
  AssertEquals('third', $0C, FEstrutura.GetByte(2));
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
  AssertTrue('byte past the end', recusou);
end;

procedure TTestPLCStruct.PalavraNaOrdemDoProcessador;
begin
  //sem troca, o primeiro byte e' o menos significativo
  FEstrutura.ChegouDaVarredura([$34, $12]);
  AssertEquals('word', $1234, FEstrutura.GetWord(0, false));
end;

procedure TTestPLCStruct.PalavraComBytesTrocados;
begin
  //com troca, o primeiro byte e' o mais significativo - a ordem do S7
  FEstrutura.ChegouDaVarredura([$12, $34]);
  AssertEquals('word swapped', $1234, FEstrutura.GetWord(0, true));
end;

procedure TTestPLCStruct.PalavraComSinal;
begin
  //$FFFF vale menos um quando lido com sinal
  FEstrutura.ChegouDaVarredura([$FF, $FF]);
  AssertEquals('minus one', -1, FEstrutura.GetSmallInt(0, false));
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
  AssertTrue('word past the end', recusou);
end;

procedure TTestPLCStruct.PalavraDuplaNaOrdemDoProcessador;
begin
  FEstrutura.ChegouDaVarredura([$78, $56, $34, $12]);
  AssertEquals('double word', $12345678, FEstrutura.GetLongWord(0, false, false));
end;

procedure TTestPLCStruct.PalavraDuplaComPalavrasTrocadas;
begin
  //so' as palavras trocadas: cada metade fica na ordem do processador
  FEstrutura.ChegouDaVarredura([$34, $12, $78, $56]);
  AssertEquals('words swapped', $12345678, FEstrutura.GetLongWord(0, false, true));
end;

procedure TTestPLCStruct.PalavraDuplaComBytesEPalavrasTrocados;
begin
  //tudo trocado e' a ordem de rede, byte mais significativo primeiro
  FEstrutura.ChegouDaVarredura([$12, $34, $56, $78]);
  AssertEquals('network order', $12345678, FEstrutura.GetLongWord(0, true, true));
end;

procedure TTestPLCStruct.InteiroComSinalDePalavraDupla;
begin
  FEstrutura.ChegouDaVarredura([$FF, $FF, $FF, $FF]);
  AssertEquals('minus one', -1, FEstrutura.GetLongInt(0, false, false));
end;

procedure TTestPLCStruct.PontoFlutuanteSimples;
begin
  //1.0 em ponto flutuante simples e' $3F800000
  FEstrutura.ChegouDaVarredura([$00, $00, $80, $3F]);
  AssertEquals('one', 1, FEstrutura.GetSingle(0, false, false), 0.0001);
end;

procedure TTestPLCStruct.PontoFlutuanteSimplesComTrocaCompleta;
begin
  //o mesmo 1.0 na ordem de rede, que e' como o S7 entrega
  FEstrutura.ChegouDaVarredura([$3F, $80, $00, $00]);
  AssertEquals('one', 1, FEstrutura.GetSingle(0, true, true), 0.0001);
end;

procedure TTestPLCStruct.PontoFlutuanteDuplo;
begin
  //1.0 em ponto flutuante duplo e' $3FF0000000000000
  FEstrutura.ChegouDaVarredura([$00, $00, $00, $00, $00, $00, $F0, $3F]);
  AssertEquals('one', 1, FEstrutura.GetDouble(0, false, false, false), 0.0001);
end;

procedure TTestPLCStruct.TrocaDePalavrasEmSessentaEQuatroBitsEhRespeitada;
begin
  //a troca de palavras e' pedida na chamada. A propriedade SwapWords do tag
  //nao serve aqui: numa estrutura ela e' presa em falso de proposito, porque o
  //bloco e' de bytes crus e cada leitura escolhe a sua ordem
  FEstrutura.ChegouDaVarredura([$01, $02, $03, $04, $05, $06, $07, $08]);

  AssertTrue('no word swapping',
             QWord($0807060504030201) = FEstrutura.GetQWord(0, false, false, false));
  AssertTrue('with word swapping',
             QWord($0605080702010403) = FEstrutura.GetQWord(0, false, true, false));
end;

procedure TTestPLCStruct.TextoSiemensTemTamanhoMaximoEAtual;
begin
  //o formato do S7: tamanho maximo, tamanho atual e os caracteres
  FEstrutura.ChegouDaVarredura([10, 3, Ord('a'), Ord('b'), Ord('c'), 0, 0, 0]);
  AssertEquals('text', 'abc', FEstrutura.GetSiemensString(0));
end;

procedure TTestPLCStruct.TextoSiemensParaNoTamanhoAtual;
begin
  //o que vem depois do tamanho atual e' resto de escrita anterior, nao texto
  FEstrutura.ChegouDaVarredura([10, 2, Ord('a'), Ord('b'), Ord('c'), Ord('d')]);
  AssertEquals('only what was declared', 'ab', FEstrutura.GetSiemensString(0));
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

  AssertFalse('the read must not overrun', estourou);
  AssertEquals('what fits', 'ab', texto);
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

  AssertEquals('one byte', $0A, FItem.Value, 0);
end;

procedure TTestPLCStructItem.ItemDePalavraLeDoisBytes;
begin
  FEstrutura.ChegouDaVarredura([$34, $12, $00, $00]);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttWord;
  FItem.Index   :=0;

  AssertEquals('one word', $1234, FItem.Value, 0);
end;

procedure TTestPLCStructItem.ItemDePalavraDuplaLeQuatroBytes;
begin
  FEstrutura.ChegouDaVarredura([$78, $56, $34, $12]);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttDWord;
  FItem.Index   :=0;

  AssertEquals('one double word', $12345678, FItem.Value, 0);
end;

procedure TTestPLCStructItem.ItemDePontoFlutuanteLeQuatroBytes;
begin
  //1.0 em ponto flutuante simples
  FEstrutura.ChegouDaVarredura([$00, $00, $80, $3F]);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttFloat;
  FItem.Index   :=0;

  AssertEquals('one', 1, FItem.Value, 0.0001);
end;

procedure TTestPLCStructItem.ItemLeNoDeslocamentoEscolhido;
begin
  //e' assim que se mapeia um membro no meio da estrutura
  FEstrutura.ChegouDaVarredura([$00, $00, $00, $00, $EF, $BE]);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttWord;
  FItem.Index   :=4;

  AssertEquals('at offset four', $BEEF, FItem.Value, 0);
end;

procedure TTestPLCStructItem.TrocarOTipoTrocaQuantosBytesSaoLidos;
begin
  FEstrutura.ChegouDaVarredura([$34, $12, $00, $00]);
  FItem.PLCBlock:=FEstrutura;
  FItem.Index   :=0;

  FItem.TagType:=pttByte;
  AssertEquals('as a byte',   $34,   FItem.Value, 0);

  FItem.TagType:=pttWord;
  AssertEquals('as a word', $1234, FItem.Value, 0);
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
  AssertTrue('an offset that does not fit', recusou);
end;

procedure TTestPLCStructItem.UltimoDeslocamentoQueCabeEhAceito;
begin
  //a mesma palavra dupla no byte 12 cabe exatamente
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttDWord;
  FItem.Index   :=12;

  AssertEquals('the last one that fits', 12, FItem.Index);
end;

procedure TTestPLCStructItem.ItemAcompanhaAMudancaDaEstrutura;
begin
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttByte;
  FItem.Index   :=1;

  FEstrutura.ChegouDaVarredura([$00, $42]);
  AssertEquals('new value', $42, FItem.Value, 0);
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

  AssertTrue('the link must have been broken', FItem.PLCBlock=nil);
end;

procedure TTestPLCStructItem.SemEstruturaGuardaOValorLocalmente;
begin
  //sem estrutura o item ainda e' um tag: guarda o que escrevem nele
  FItem.TagType:=pttByte;
  FItem.Value  :=9;

  AssertEquals('value kept', 9, FItem.Value, 0);
end;

{ TTestPLCStructString }

procedure TTestPLCStructString.SetUp;
begin
  FEstrutura:=TEstruturaProbe.Create(nil);
  FEstrutura.Size:=16;

  FTexto:=TPLCStructString.Create(nil);
  FAvisos:=0;
end;

procedure TTestPLCStructString.TearDown;
begin
  FreeAndNil(FTexto);
  FreeAndNil(FEstrutura);
end;

procedure TTestPLCStructString.ContarAviso(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestPLCStructString.Configurar(aTipo:TPLCStringTypes; aIndice, aTamanho:Cardinal);
begin
  FTexto.PLCBlock  :=FEstrutura;
  FTexto.StringType:=aTipo;
  FTexto.StringSize:=aTamanho;
  FTexto.Index     :=aIndice;
end;

procedure TTestPLCStructString.TextoNoFormatoCTerminaNoZero;
begin
  Configurar(stC, 0, 8);
  FEstrutura.ChegouDaVarredura([Ord('o'), Ord('l'), Ord('a'), 0, Ord('x'), Ord('y')]);

  AssertEquals('stops at the zero', 'ola', FTexto.Value);
end;

procedure TTestPLCStructString.TextoNoFormatoCRespeitaOTamanhoMaximo;
begin
  //sem zero nenhum, o tamanho declarado e' o que limita
  Configurar(stC, 0, 3);
  FEstrutura.ChegouDaVarredura([Ord('a'), Ord('b'), Ord('c'), Ord('d'), Ord('e')]);

  AssertEquals('three characters', 'abc', FTexto.Value);
end;

procedure TTestPLCStructString.TextoNoFormatoSiemensEhLido;
begin
  //tamanho maximo, tamanho atual, caracteres
  Configurar(stSIEMENS, 0, 10);
  FEstrutura.ChegouDaVarredura([10, 3, Ord('a'), Ord('b'), Ord('c'), 0, 0]);

  AssertEquals('siemens text', 'abc', FTexto.Value);
end;

procedure TTestPLCStructString.TextoNoFormatoRockwellUsaOTamanhoDaFrente;
begin
  //palavra dupla com o tamanho, depois os caracteres
  Configurar(stROCKWELL, 0, 8);
  FEstrutura.ChegouDaVarredura([4, 0, 0, 0, Ord('c'), Ord('a'), Ord('s'), Ord('a'), Ord('x')]);

  AssertEquals('rockwell text', 'casa', FTexto.Value);
end;

procedure TTestPLCStructString.TextoEhLidoNoDeslocamentoEscolhido;
begin
  Configurar(stC, 4, 8);
  FEstrutura.ChegouDaVarredura([0, 0, 0, 0, Ord('l'), Ord('a'), Ord('h'), 0]);

  AssertEquals('at offset four', 'lah', FTexto.Value);
end;

procedure TTestPLCStructString.MudancaNaEstruturaAtualizaOTexto;
begin
  Configurar(stC, 0, 8);
  FEstrutura.ChegouDaVarredura([Ord('u'), Ord('m'), 0]);
  AssertEquals('before', 'um', FTexto.Value);

  FTexto.AddTagChangeHandler(@ContarAviso);
  FAvisos:=0;

  FEstrutura.ChegouDaVarredura([Ord('d'), Ord('o'), Ord('i'), Ord('s'), 0]);

  AssertEquals('after',  'dois', FTexto.Value);
  AssertTrue  ('and it notified', FAvisos>0);
end;

procedure TTestPLCStructString.CadaFormatoReservaOSeuCabecalho;
begin
  //numa estrutura de 16 bytes: o C gasta um byte com o zero final, o Siemens
  //dois com os tamanhos, o Rockwell quatro com a palavra dupla
  FTexto.PLCBlock:=FEstrutura;

  FTexto.StringType:=stC;
  FTexto.StringSize:=15;
  AssertEquals('C fits with 15', 15, FTexto.StringSize);

  FTexto.StringType:=stSIEMENS;
  FTexto.StringSize:=14;
  AssertEquals('siemens fits with 14', 14, FTexto.StringSize);

  FTexto.StringType:=stROCKWELL;
  FTexto.StringSize:=12;
  AssertEquals('rockwell fits with 12', 12, FTexto.StringSize);
end;

procedure TTestPLCStructString.DeslocamentoQueNaoCabeEhIgnoradoEmSilencio;
begin
  //o que nao cabe nao levanta erro: a propriedade simplesmente nao muda
  Configurar(stC, 0, 8);

  FTexto.Index:=12;
  AssertEquals('the previous offset stays', 0, FTexto.Index);
end;

procedure TTestPLCStructString.EstruturaDestruidaDesligaOVinculo;
var
  estrutura:TEstruturaProbe;
begin
  estrutura:=TEstruturaProbe.Create(nil);
  estrutura.Size:=16;

  FTexto.PLCBlock  :=estrutura;
  FTexto.StringType:=stC;
  FTexto.StringSize:=8;

  FreeAndNil(estrutura);

  AssertTrue('the link must have been broken', FTexto.PLCBlock=nil);
end;

initialization
  RegisterTest(TTestPLCStruct);
  RegisterTest(TTestPLCStructItem);
  RegisterTest(TTestPLCStructString);

end.
