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
    function  PathOf(const aTag:String):BYTES;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //caminho simbolico / symbolic path
    procedure APlainNameBecomesAnAnsiSegment;
    procedure AnEvenLengthNameTakesNoPadding;
    procedure AStructureMemberGivesOneSegmentPerPart;
    procedure AnIndexUpToTwoHundredAndFiftyFiveFitsInOneByte;
    procedure ABiggerIndexUsesATwoByteSegment;
    procedure AMultiDimensionalArrayGivesOneIndexPerDimension;
    procedure AProgramTagIsASingleSegment;
    procedure ThePaddingIsZeroWhenThePreviousOneIsSmaller;
    procedure APathThatIsTooLongIsRefused;

    //classificacao de tipos CIP / CIP type classification
    procedure AStructureIsRecognisedByTheHighBit;
    procedure ASystemTypeIsRecognised;
    procedure TheDimensionsComeFromTheTwoBitsAboveTheType;
    procedure TheTypeNameFollowsTheStudio;
    procedure ACipTypeBecomesAPascalSCADATagType;
    procedure AStructureHasNoMatchingTagType;
    procedure SizeInBytesPerType;

    procedure TheSegmentPaddingIsAlwaysZero;

    //lacuna conhecida / known gap
    procedure AnEncodedPathShouldBeReadableBack;
    procedure EveryKindOfPathComesBackAsItWent;
    procedure ATruncatedPathDecodesWhatIsWhole;
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

function TTestLGXDriver.PathOf(const aTag:String):BYTES;
begin
  FPedido.RequestPath:=aTag;
  Result:=FPedido.ReqPathData;
end;

procedure TTestLGXDriver.APlainNameBecomesAnAnsiSegment;
begin
  //$91, tamanho 5, os caracteres de "MyTag", e um byte de enchimento porque
  //o segmento tem que terminar em fronteira de palavra
  AssertBytesEqual('MyTag', BytesOf('91 05 4D 79 54 61 67 00'), PathOf('MyTag'));
end;

procedure TTestLGXDriver.AnEvenLengthNameTakesNoPadding;
begin
  AssertBytesEqual('Ab', BytesOf('91 02 41 62'), PathOf('Ab'));
end;

procedure TTestLGXDriver.AStructureMemberGivesOneSegmentPerPart;
begin
  //cada parte separada por ponto vira o seu proprio segmento
  AssertBytesEqual('A.B', BytesOf('91 01 41 00 91 01 42 00'), PathOf('A.B'));
end;

procedure TTestLGXDriver.AnIndexUpToTwoHundredAndFiftyFiveFitsInOneByte;
begin
  //$28 = indice de um byte
  AssertBytesEqual('Tag[5]', BytesOf('91 03 54 61 67 00 28 05'), PathOf('Tag[5]'));
  AssertBytesEqual('Tag[255]', BytesOf('91 03 54 61 67 00 28 FF'), PathOf('Tag[255]'));
end;

procedure TTestLGXDriver.ABiggerIndexUsesATwoByteSegment;
begin
  //$29 = indice de dois bytes, e o valor vai em little-endian (300 = $012C)
  AssertBytesEqual('Tag[300]', BytesOf('91 03 54 61 67 00 29 00 2C 01'), PathOf('Tag[300]'));
  AssertBytesEqual('Tag[256]', BytesOf('91 03 54 61 67 00 29 00 00 01'), PathOf('Tag[256]'));
end;

procedure TTestLGXDriver.AMultiDimensionalArrayGivesOneIndexPerDimension;
begin
  //array de duas dimensoes: um segmento de indice para cada
  AssertBytesEqual('Tag[1,2]', BytesOf('91 03 54 61 67 00 28 01 28 02'), PathOf('Tag[1,2]'));
end;

procedure TTestLGXDriver.AProgramTagIsASingleSegment;
begin
  //"Program:MainProgram" tem 19 caracteres e vale como um nome unico - os dois
  //pontos nao separam nada aqui, so o ponto separa.
  AssertBytesEqual('program tag',
                   BytesOf('91 13' +                                   //segmento de 19 caracteres
                           '50 72 6F 67 72 61 6D 3A' +                 //"Program:"
                           '4D 61 69 6E 50 72 6F 67 72 61 6D' +        //"MainProgram"
                           '00' +                                      //enchimento
                           '91 02 41 62'),                             //"Ab"
                   PathOf('Program:MainProgram.Ab'));
end;

procedure TTestLGXDriver.ThePaddingIsZeroWhenThePreviousOneIsSmaller;
begin
  //o lado que funciona: com o segmento anterior menor, o enchimento sai zerado
  AssertBytesEqual('A.Tag', BytesOf('91 01 41 00 91 03 54 61 67 00'), PathOf('A.Tag'));
end;

procedure TTestLGXDriver.APathThatIsTooLongIsRefused;
var
  recusou:Boolean;
begin
  //acima de 200 caracteres o codificador recusa em vez de gerar lixo
  recusou:=false;
  try
    PathOf(StringOfChar('A', 250));
  except
    on E:Exception do
      recusou:=true;
  end;
  AssertTrue('a path over the limit must be refused', recusou);
end;

procedure TTestLGXDriver.AStructureIsRecognisedByTheHighBit;
begin
  //bit $8000 ligado = estrutura/UDT
  AssertTrue ('with the structure bit', LGXTypeIsStruct($8FCE));
  AssertFalse('DINT is not a structure',   LGXTypeIsStruct($00C4));
end;

procedure TTestLGXDriver.ASystemTypeIsRecognised;
begin
  //bit $1000 = tipo interno do CLP, que o construtor de tags ignora
  AssertTrue ('with the system bit', LGXTypeIsSystem($10C4));
  AssertFalse('plain DINT',           LGXTypeIsSystem($00C4));
end;

procedure TTestLGXDriver.TheDimensionsComeFromTheTwoBitsAboveTheType;
begin
  //bits 13 e 14 guardam de 0 a 3 dimensoes
  AssertEquals('scalar',      0, LGXTypeDimensions($00C4));
  AssertEquals('one dimension', 1, LGXTypeDimensions($20C4));
  AssertEquals('two',         2, LGXTypeDimensions($40C4));
  AssertEquals('three',         3, LGXTypeDimensions($60C4));
end;

procedure TTestLGXDriver.TheTypeNameFollowsTheStudio;
begin
  AssertEquals('bool',  'BOOL',  LGXTypeName($00C1));
  AssertEquals('dint',  'DINT',  LGXTypeName($00C4));
  AssertEquals('real',  'REAL',  LGXTypeName($00CA));
  AssertEquals('lreal', 'LREAL', LGXTypeName($00CB));

  //estrutura tem nome proprio, independente do resto do codigo
  AssertEquals('structure', 'STRUCT/UDT', LGXTypeName($8FCE));

  //tipo que o driver nao conhece aparece em hexa, para nao mentir um nome
  AssertEquals('unknown', '0x00EE', LGXTypeName($00EE));
end;

procedure TTestLGXDriver.ACipTypeBecomesAPascalSCADATagType;
var
  tipo:TTagType;
begin
  AssertTrue  ('dint converted',  LGXTypeToTagType($00C4, tipo));
  AssertEquals('dint', Ord(pttLongInt), Ord(tipo));

  AssertTrue  ('real converted',  LGXTypeToTagType($00CA, tipo));
  AssertEquals('real', Ord(pttFloat), Ord(tipo));

  AssertTrue  ('lint converted',  LGXTypeToTagType($00C5, tipo));
  AssertEquals('lint', Ord(pttInt64), Ord(tipo));

  //as dimensoes nao atrapalham: o tipo continua sendo lido dos 8 bits baixos
  AssertTrue  ('dint in an array',    LGXTypeToTagType($20C4, tipo));
  AssertEquals('dint in an array', Ord(pttLongInt), Ord(tipo));
end;

procedure TTestLGXDriver.AStructureHasNoMatchingTagType;
var
  tipo:TTagType;
begin
  //estrutura nao tem valor numerico unico: quem chama precisa saber disso
  AssertFalse('structure', LGXTypeToTagType($8FCE, tipo));
end;

procedure TTestLGXDriver.SizeInBytesPerType;
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
  AssertEquals('unknown', 1, LGXTypeSizeInBytes($00EE));
end;

procedure TTestLGXDriver.TheSegmentPaddingIsAlwaysZero;
begin
  //nome impar depois de um nome maior: o enchimento nao pode herdar nada do
  //segmento anterior
  AssertBytesEqual('AAAA.B', BytesOf('91 04 41 41 41 41 91 01 42 00'), PathOf('AAAA.B'));
  AssertBytesEqual('program tag with an odd member',
                   BytesOf('91 13 50 72 6F 67 72 61 6D 3A 4D 61 69 6E 50 72 6F 67 72 61 6D 00' +
                           '91 03 54 61 67 00'),
                   PathOf('Program:MainProgram.Tag'));
end;

procedure TTestLGXDriver.AnEncodedPathShouldBeReadableBack;
begin
  //e' o caminho que o IDE e o depurador leem de volta dos pedidos CIP
  //it is the path the IDE and the debugger read back from the CIP requests
  FPedido.RequestPath:='MyTag';
  AssertEquals('round trip of the path', 'MyTag', FPedido.RequestPath);
end;

procedure TTestLGXDriver.EveryKindOfPathComesBackAsItWent;
const
  Caminhos: array[0..5] of String = ('A.B', 'Tag[5]', 'Tag[300]', 'Tag[256]', 'Tag[1,2]', 'Program:MainProgram.Motor.Speed[3]');
var
  c:Integer;
begin
  for c:=0 to High(Caminhos) do begin
    FPedido.RequestPath:=Caminhos[c];
    AssertEquals(Caminhos[c], Caminhos[c], FPedido.RequestPath);
  end;
end;

procedure TTestLGXDriver.ATruncatedPathDecodesWhatIsWhole;
begin
  //um segmento cortado no meio nao pode ler alem do buffer: para no que
  //estava inteiro
  //a segment cut in the middle must not read past the buffer: it stops at
  //what was whole
  FPedido.ReqPathData:=BytesOf('91 01 41 00 91 05 42');
  AssertEquals('so o primeiro', 'A', FPedido.RequestPath);
end;

initialization
  RegisterTest(TTestLGXDriver);

end.
