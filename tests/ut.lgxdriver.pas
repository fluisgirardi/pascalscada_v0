{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do LGXDriver: caminho simbolico CIP e classificacao dos
            tipos do ControlLogix.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  No protocolo dos CLPs Rockwell nao se le por endereco: pede-se pelo NOME do
  tag, codificado em segmentos CIP. O codificador e' funcao interna da unit,
  entao os testes chegam nele pela porta publica - atribuindo RequestPath num
  pedido e lendo o ReqPathData que sai.

  Os frames esperados seguem a especificacao CIP: $91 abre um segmento
  simbolico ANSI (tamanho, caracteres, e um byte de enchimento quando o nome
  tem tamanho impar); $28 e' indice de ate um byte e $29 indice de dois.
}
{$ELSE}
{:
  @abstract(LGXDriver tests: CIP symbolic path and ControlLogix type
            classification.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  On Rockwell PLCs you do not read by address: you ask by the tag NAME, encoded
  as CIP segments. The encoder is internal to the unit, so the tests reach it
  through the public door - setting RequestPath on a request and reading the
  ReqPathData that comes out.

  The expected frames follow the CIP specification: $91 opens an ANSI symbolic
  segment (length, characters, and a pad byte when the name has an odd length);
  $28 is a one byte index and $29 a two byte one.
}
{$ENDIF}
unit ut.lgxdriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, Tag, ProtocolTypes, LGXDriver,
  testsupport.bytes;

type

  { TTestLGXDriver }

  TTestLGXDriver = class(TTestCase)
  private
    FPedido:TCIPReadTagFragReq;
    //: codifica o caminho pela API publica e devolve os bytes gerados
    function  Caminho(const aTag:String):BYTES;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //caminho simbolico / symbolic path
    procedure NomeSimplesViraSegmentoAnsi;
    procedure NomeDeTamanhoParNaoLevaEnchimento;
    procedure MembroDeEstruturaGeraUmSegmentoPorParte;
    procedure IndiceAteDuzentosECinquentaECincoCabeEmUmByte;
    procedure IndiceMaiorUsaSegmentoDeDoisBytes;
    procedure ArrayDeVariasDimensoesGeraUmIndicePorDimensao;
    procedure TagDeProgramaEhUmSegmentoSo;
    procedure EnchimentoEhZeroQuandoOAnteriorEhMenor;
    procedure CaminhoLongoDemaisEhRecusado;

    //classificacao de tipos CIP / CIP type classification
    procedure EstruturaEhReconhecidaPeloBitAlto;
    procedure TipoDeSistemaEhReconhecido;
    procedure DimensoesVemDosDoisBitsAcimaDoTipo;
    procedure NomeDoTipoSegueOStudio;
    procedure TipoCipViraTipoDeTagDoPascalSCADA;
    procedure EstruturaNaoTemTipoDeTagEquivalente;
    procedure TamanhoEmBytesPorTipo;

    procedure EnchimentoDoSegmentoEhSempreZero;

    //lacuna conhecida / known gap
    procedure CaminhoCodificadoDeveriaSerLegivelDeVolta;
  end;

implementation

procedure TTestLGXDriver.SetUp;
begin
  FPedido:=TCIPReadTagFragReq.Create;
end;

procedure TTestLGXDriver.TearDown;
begin
  FreeAndNil(FPedido);
end;

function TTestLGXDriver.Caminho(const aTag:String):BYTES;
begin
  FPedido.RequestPath:=aTag;
  Result:=FPedido.ReqPathData;
end;

procedure TTestLGXDriver.NomeSimplesViraSegmentoAnsi;
begin
  //$91, tamanho 5, os caracteres de "MyTag", e um byte de enchimento porque
  //o segmento tem que terminar em fronteira de palavra
  AssertBytesEqual('MyTag', BytesOf('91 05 4D 79 54 61 67 00'), Caminho('MyTag'));
end;

procedure TTestLGXDriver.NomeDeTamanhoParNaoLevaEnchimento;
begin
  AssertBytesEqual('Ab', BytesOf('91 02 41 62'), Caminho('Ab'));
end;

procedure TTestLGXDriver.MembroDeEstruturaGeraUmSegmentoPorParte;
begin
  //cada parte separada por ponto vira o seu proprio segmento
  AssertBytesEqual('A.B', BytesOf('91 01 41 00 91 01 42 00'), Caminho('A.B'));
end;

procedure TTestLGXDriver.IndiceAteDuzentosECinquentaECincoCabeEmUmByte;
begin
  //$28 = indice de um byte
  AssertBytesEqual('Tag[5]', BytesOf('91 03 54 61 67 00 28 05'), Caminho('Tag[5]'));
  AssertBytesEqual('Tag[255]', BytesOf('91 03 54 61 67 00 28 FF'), Caminho('Tag[255]'));
end;

procedure TTestLGXDriver.IndiceMaiorUsaSegmentoDeDoisBytes;
begin
  //$29 = indice de dois bytes, e o valor vai em little-endian (300 = $012C)
  AssertBytesEqual('Tag[300]', BytesOf('91 03 54 61 67 00 29 00 2C 01'), Caminho('Tag[300]'));
  AssertBytesEqual('Tag[256]', BytesOf('91 03 54 61 67 00 29 00 00 01'), Caminho('Tag[256]'));
end;

procedure TTestLGXDriver.ArrayDeVariasDimensoesGeraUmIndicePorDimensao;
begin
  //array de duas dimensoes: um segmento de indice para cada
  AssertBytesEqual('Tag[1,2]', BytesOf('91 03 54 61 67 00 28 01 28 02'), Caminho('Tag[1,2]'));
end;

procedure TTestLGXDriver.TagDeProgramaEhUmSegmentoSo;
begin
  //"Program:MainProgram" tem 19 caracteres e vale como um nome unico - os dois
  //pontos nao separam nada aqui, so o ponto separa.
  AssertBytesEqual('tag de programa',
                   BytesOf('91 13' +                                   //segmento de 19 caracteres
                           '50 72 6F 67 72 61 6D 3A' +                 //"Program:"
                           '4D 61 69 6E 50 72 6F 67 72 61 6D' +        //"MainProgram"
                           '00' +                                      //enchimento
                           '91 02 41 62'),                             //"Ab"
                   Caminho('Program:MainProgram.Ab'));
end;

procedure TTestLGXDriver.EnchimentoEhZeroQuandoOAnteriorEhMenor;
begin
  //o lado que funciona: com o segmento anterior menor, o enchimento sai zerado
  AssertBytesEqual('A.Tag', BytesOf('91 01 41 00 91 03 54 61 67 00'), Caminho('A.Tag'));
end;

procedure TTestLGXDriver.CaminhoLongoDemaisEhRecusado;
var
  recusou:Boolean;
begin
  //acima de 200 caracteres o codificador recusa em vez de gerar lixo
  recusou:=false;
  try
    Caminho(StringOfChar('A', 250));
  except
    on E:Exception do
      recusou:=true;
  end;
  AssertTrue('caminho acima do limite deve ser recusado', recusou);
end;

procedure TTestLGXDriver.EstruturaEhReconhecidaPeloBitAlto;
begin
  //bit $8000 ligado = estrutura/UDT
  AssertTrue ('com o bit de estrutura', LGXTypeIsStruct($8FCE));
  AssertFalse('DINT nao e estrutura',   LGXTypeIsStruct($00C4));
end;

procedure TTestLGXDriver.TipoDeSistemaEhReconhecido;
begin
  //bit $1000 = tipo interno do CLP, que o construtor de tags ignora
  AssertTrue ('com o bit de sistema', LGXTypeIsSystem($10C4));
  AssertFalse('DINT comum',           LGXTypeIsSystem($00C4));
end;

procedure TTestLGXDriver.DimensoesVemDosDoisBitsAcimaDoTipo;
begin
  //bits 13 e 14 guardam de 0 a 3 dimensoes
  AssertEquals('escalar',      0, LGXTypeDimensions($00C4));
  AssertEquals('uma dimensao', 1, LGXTypeDimensions($20C4));
  AssertEquals('duas',         2, LGXTypeDimensions($40C4));
  AssertEquals('tres',         3, LGXTypeDimensions($60C4));
end;

procedure TTestLGXDriver.NomeDoTipoSegueOStudio;
begin
  AssertEquals('bool',  'BOOL',  LGXTypeName($00C1));
  AssertEquals('dint',  'DINT',  LGXTypeName($00C4));
  AssertEquals('real',  'REAL',  LGXTypeName($00CA));
  AssertEquals('lreal', 'LREAL', LGXTypeName($00CB));

  //estrutura tem nome proprio, independente do resto do codigo
  AssertEquals('estrutura', 'STRUCT/UDT', LGXTypeName($8FCE));

  //tipo que o driver nao conhece aparece em hexa, para nao mentir um nome
  AssertEquals('desconhecido', '0x00EE', LGXTypeName($00EE));
end;

procedure TTestLGXDriver.TipoCipViraTipoDeTagDoPascalSCADA;
var
  tipo:TTagType;
begin
  AssertTrue  ('dint convertido',  LGXTypeToTagType($00C4, tipo));
  AssertEquals('dint', Ord(pttLongInt), Ord(tipo));

  AssertTrue  ('real convertido',  LGXTypeToTagType($00CA, tipo));
  AssertEquals('real', Ord(pttFloat), Ord(tipo));

  AssertTrue  ('lint convertido',  LGXTypeToTagType($00C5, tipo));
  AssertEquals('lint', Ord(pttInt64), Ord(tipo));

  //as dimensoes nao atrapalham: o tipo continua sendo lido dos 8 bits baixos
  AssertTrue  ('dint em array',    LGXTypeToTagType($20C4, tipo));
  AssertEquals('dint em array', Ord(pttLongInt), Ord(tipo));
end;

procedure TTestLGXDriver.EstruturaNaoTemTipoDeTagEquivalente;
var
  tipo:TTagType;
begin
  //estrutura nao tem valor numerico unico: quem chama precisa saber disso
  AssertFalse('estrutura', LGXTypeToTagType($8FCE, tipo));
end;

procedure TTestLGXDriver.TamanhoEmBytesPorTipo;
begin
  AssertEquals('bool',  1, LGXTypeSizeInBytes($00C1));
  AssertEquals('sint',  1, LGXTypeSizeInBytes($00C2));
  AssertEquals('int',   2, LGXTypeSizeInBytes($00C3));
  AssertEquals('dint',  4, LGXTypeSizeInBytes($00C4));
  AssertEquals('real',  4, LGXTypeSizeInBytes($00CA));
  AssertEquals('lint',  8, LGXTypeSizeInBytes($00C5));
  AssertEquals('lreal', 8, LGXTypeSizeInBytes($00CB));

  //tipo desconhecido cai em 1 byte - inclusive STRING, que na pratica ocupa
  //bem mais; quem trata string nao passa por aqui
  AssertEquals('desconhecido', 1, LGXTypeSizeInBytes($00EE));
end;

procedure TTestLGXDriver.EnchimentoDoSegmentoEhSempreZero;
begin
  //nome impar depois de um nome maior: o enchimento nao pode herdar nada do
  //segmento anterior
  AssertBytesEqual('AAAA.B', BytesOf('91 04 41 41 41 41 91 01 42 00'), Caminho('AAAA.B'));
  AssertBytesEqual('tag de programa com membro impar',
                   BytesOf('91 13 50 72 6F 67 72 61 6D 3A 4D 61 69 6E 50 72 6F 67 72 61 6D 00' +
                           '91 03 54 61 67 00'),
                   Caminho('Program:MainProgram.Tag'));
end;

procedure TTestLGXDriver.CaminhoCodificadoDeveriaSerLegivelDeVolta;
begin
  Ignore('lacuna conhecida: DecodeTagPath e'#39' um esqueleto - nao atribui o ' +
         'Result e devolve string vazia (o proprio codigo tem o comentario ' +
         '"TODO: Decodificar o indecodificavel?"). Ela e'#39' o leitor da ' +
         'propriedade RequestPath em quatro classes de pedido ' +
         '(TCIPReadTagReq, TCIPReadTagFragReq, TCIPWriteTagReq e ' +
         'TCIPWriteTagFragReq), entao ler esse caminho de volta sempre devolve ' +
         'vazio. Remova este Ignore quando o decodificador for escrito.');

  FPedido.RequestPath:='MyTag';
  AssertEquals('ida e volta do caminho', 'MyTag', FPedido.RequestPath);
end;

initialization
  RegisterTest(TTestLGXDriver);

end.
