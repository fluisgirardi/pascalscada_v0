{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do S7PlusTypeInfo: tabela de tipos, campos de bits e os
            parsers da arvore de tipos.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Esta e' a parte do S7CommPlus que mistura as duas convencoes: a arvore de
  tipos e' little-endian enquanto o resto do protocolo e' big-endian, e num
  mesmo registro (o VartypeElement) convivem as duas. Os testes escrevem os
  buffers literalmente pra fixar exatamente onde cada uma vale.
}
{$ELSE}
{:
  @abstract(S7PlusTypeInfo tests: the type table, the bit fields and the type
            tree parsers.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This is the part of S7CommPlus that mixes both conventions: the type tree is
  little-endian while the rest of the protocol is big-endian, and a single
  record (the VartypeElement) carries both. The tests spell the buffers out
  literally to pin down exactly where each one applies.
}
{$ENDIF}
unit ut.s7plustypeinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, S7PlusTypes, S7PlusCodec, S7PlusTypeInfo,
  testsupport.bytes;

type

  { TTestS7PlusTypeInfo }

  TTestS7PlusTypeInfo = class(TTestCase)
  published
    //tabela de tipos / type table
    procedure KnownScalarTypesAreSupported;
    procedure StructuralTypesAreNotSupported;
    procedure TheSizeOfTheScalarTypes;
    procedure StringAddsTwoToTheDeclaredLength;
    procedure ATypeWithNoKnownSizeGivesZero;

    //campos de bits do VartypeElement / VartypeElement bit fields
    procedure TheOffsetInfoTypeComesFromTheFourHighBits;
    procedure TheOptimisedBitOffsetComesFromTheThreeLowBits;
    procedure BitOffsetsAndClassicModeComeFromTheFlagsByte;

    //offset info (little-endian) / offset info (little endian)
    procedure OffsetInfoType8ReadsTheOptimisedOneFirst;
    procedure OffsetInfoType1ReversesTheOrder;
    procedure ArrayOffsetInfoCarriesTheLowerBoundAndTheCount;

    //elementos e listas / elements and lists
    procedure AVartypeElementMixesLittleAndBigEndian;
    procedure AVartypeListSkipsTheStartingIdOfTheFirstBlock;
    procedure ANameListReadsTheLengthAndTheTrailingZero;

    //objetos / objects
    procedure AnObjectWithNoOpeningComplains;
    procedure TheObjectAttributesAreRead;
    procedure AMissingAttributeInTheObjectReturnsFalse;
    procedure ANestedObjectIsFoundByItsClass;
    procedure SiblingObjectsAreWalked;

    //formatacao / formatting
    procedure LidsAreFormattedInHexSeparatedByDots;
  end;

implementation

//codigos de "software datatype" do CLP, os mesmos numeros que o TIA usa
const
  SDT_BOOL    = 1;
  SDT_BYTE    = 2;
  SDT_WORD    = 4;
  SDT_INT     = 5;
  SDT_DINT    = 7;
  SDT_REAL    = 8;
  SDT_ARRAY   = 16;
  SDT_STRUCT  = 17;
  SDT_STRING  = 19;
  SDT_POINTER = 20;
  SDT_ANY     = 22;
  SDT_LREAL   = 48;
  SDT_UDINT   = 54;
  SDT_WSTRING = 62;
  SDT_VARIANT = 63;
  SDT_DTL     = 67;
  SDT_UNKNOWN = 255;

procedure TTestS7PlusTypeInfo.KnownScalarTypesAreSupported;
begin
  AssertTrue('bool',   IsSoftDataTypeSupported(SDT_BOOL));
  AssertTrue('int',    IsSoftDataTypeSupported(SDT_INT));
  AssertTrue('real',   IsSoftDataTypeSupported(SDT_REAL));
  AssertTrue('string', IsSoftDataTypeSupported(SDT_STRING));
  AssertTrue('lreal',  IsSoftDataTypeSupported(SDT_LREAL));
  AssertTrue('udint',  IsSoftDataTypeSupported(SDT_UDINT));
end;

procedure TTestS7PlusTypeInfo.StructuralTypesAreNotSupported;
begin
  //array e struct nao sao lidos como valor: sao nos da arvore, e quem os
  //atravessa e' o construtor da lista achatada.
  AssertFalse('array',        IsSoftDataTypeSupported(SDT_ARRAY));
  AssertFalse('struct',       IsSoftDataTypeSupported(SDT_STRUCT));
  AssertFalse('variant',      IsSoftDataTypeSupported(SDT_VARIANT));
  AssertFalse('dtl',          IsSoftDataTypeSupported(SDT_DTL));
  AssertFalse('void',         IsSoftDataTypeSupported(0));
  AssertFalse('unknown', IsSoftDataTypeSupported(SDT_UNKNOWN));
end;

procedure TTestS7PlusTypeInfo.TheSizeOfTheScalarTypes;
begin
  AssertEquals('bool',    1,  S7PlusDataTypeSize(SDT_BOOL));
  AssertEquals('byte',    1,  S7PlusDataTypeSize(SDT_BYTE));
  AssertEquals('word',    2,  S7PlusDataTypeSize(SDT_WORD));
  AssertEquals('int',     2,  S7PlusDataTypeSize(SDT_INT));
  AssertEquals('dint',    4,  S7PlusDataTypeSize(SDT_DINT));
  AssertEquals('real',    4,  S7PlusDataTypeSize(SDT_REAL));
  AssertEquals('lreal',   8,  S7PlusDataTypeSize(SDT_LREAL));
  AssertEquals('pointer', 6,  S7PlusDataTypeSize(SDT_POINTER));
  AssertEquals('any',     10, S7PlusDataTypeSize(SDT_ANY));
  AssertEquals('dtl',     12, S7PlusDataTypeSize(SDT_DTL));
end;

procedure TTestS7PlusTypeInfo.StringAddsTwoToTheDeclaredLength;
begin
  //os dois bytes a mais sao o cabecalho classico do S7: tamanho maximo e atual
  AssertEquals('string[10]',  12, S7PlusDataTypeSize(SDT_STRING, 10));
  AssertEquals('wstring[4]',   6, S7PlusDataTypeSize(SDT_WSTRING, 4));
  AssertEquals('string with no length', 2, S7PlusDataTypeSize(SDT_STRING));
end;

procedure TTestS7PlusTypeInfo.ATypeWithNoKnownSizeGivesZero;
begin
  //zero aqui e' o sinal de "nao sei medir isso", nao um tipo de zero byte
  AssertEquals('array',        0, S7PlusDataTypeSize(SDT_ARRAY));
  AssertEquals('struct',       0, S7PlusDataTypeSize(SDT_STRUCT));
  AssertEquals('unknown', 0, S7PlusDataTypeSize(SDT_UNKNOWN));
end;

procedure TTestS7PlusTypeInfo.TheOffsetInfoTypeComesFromTheFourHighBits;
var
  el:TS7PlusVartypeElement;
begin
  FillChar(el, SizeOf(el), 0);

  el.AttributeFlags:=$8000;
  AssertEquals('type 8', 8, S7PlusVteOffsetInfoType(el));

  el.AttributeFlags:=$3ABC;
  AssertEquals('type 3', 3, S7PlusVteOffsetInfoType(el));

  el.AttributeFlags:=$0FFF;
  AssertEquals('type 0', 0, S7PlusVteOffsetInfoType(el));
end;

procedure TTestS7PlusTypeInfo.TheOptimisedBitOffsetComesFromTheThreeLowBits;
var
  el:TS7PlusVartypeElement;
begin
  FillChar(el, SizeOf(el), 0);

  el.AttributeFlags:=$8005;
  AssertEquals('bit 5', 5, S7PlusVteAttributeBitOffset(el));

  //so os tres bits baixos entram: o resto e' outro campo
  el.AttributeFlags:=$80F8;
  AssertEquals('bit 0', 0, S7PlusVteAttributeBitOffset(el));
end;

procedure TTestS7PlusTypeInfo.BitOffsetsAndClassicModeComeFromTheFlagsByte;
var
  el:TS7PlusVartypeElement;
begin
  FillChar(el, SizeOf(el), 0);

  //o $53 = 0101 0011: nao otimizado 5, otimizado 3, sem o bit de classico
  el.BitOffsetInfoFlags:=$53;
  AssertEquals('not optimised', 5, S7PlusVteNonOptBitOffset(el));
  AssertEquals('optimised',     3, S7PlusVteOptBitOffset(el));
  AssertFalse ('classic',         S7PlusVteClassic(el));

  //o $5B = 0101 1011: mesmos offsets, agora com o bit $08 de bloco classico
  el.BitOffsetInfoFlags:=$5B;
  AssertEquals('not optimised', 5, S7PlusVteNonOptBitOffset(el));
  AssertEquals('optimised',     3, S7PlusVteOptBitOffset(el));
  AssertTrue  ('classic',         S7PlusVteClassic(el));
end;

procedure TTestS7PlusTypeInfo.OffsetInfoType8ReadsTheOptimisedOneFirst;
var
  info:TS7PlusOffsetInfo;
  fim:Integer;
begin
  //little-endian: 01 00 = 1, 02 00 = 2
  fim:=ParseOffsetInfo(BytesOf('01 00 02 00'), 0, 8, info);
  AssertEquals('optimised',     1, info.OptAddr);
  AssertEquals('not optimised', 2, info.NonOptAddr);
  AssertEquals('bytes read',   4, fim);
  AssertEquals('code',        8, info.Code);
end;

procedure TTestS7PlusTypeInfo.OffsetInfoType1ReversesTheOrder;
var
  info:TS7PlusOffsetInfo;
begin
  //mesmos bytes do teste anterior: no tipo 1 (legado) a ordem e' a inversa
  ParseOffsetInfo(BytesOf('01 00 02 00'), 0, 1, info);
  AssertEquals('not optimised', 1, info.NonOptAddr);
  AssertEquals('optimised',     2, info.OptAddr);
end;

procedure TTestS7PlusTypeInfo.ArrayOffsetInfoCarriesTheLowerBoundAndTheCount;
var
  info:TS7PlusOffsetInfo;
  fim:Integer;
begin
  //2 nao usados, 2 nao usados, otimizado=10, nao otimizado=20,
  //limite inferior=-5 (FB FF FF FF em complemento de dois), contagem=10
  fim:=ParseOffsetInfo(BytesOf('00 00 00 00 0A 00 00 00 14 00 00 00 FB FF FF FF 0A 00 00 00'),
                       0, 3, info);
  AssertEquals('optimised',        10, info.OptAddr);
  AssertEquals('not optimised',    20, info.NonOptAddr);
  AssertEquals('lower bound',  -5, info.ArrayLowerBound);
  AssertEquals('how many elements',10, info.ArrayElementCount);
  AssertTrue  ('marked as one dimension', info.Is1Dim);
  AssertEquals('bytes read',      20, fim);
end;

procedure TTestS7PlusTypeInfo.AVartypeElementMixesLittleAndBigEndian;
var
  el:TS7PlusVartypeElement;
  fim:Integer;
begin
  //lid e crc em little-endian, flags de atributo em BIG-endian, e o offset
  //info (aqui tipo 8, vindo do $8... das flags) de novo em little-endian.
  fim:=ParseVartypeElement(BytesOf('05 00 00 00' +   //lid = 5 (LE)
                                   '78 56 34 12' +   //crc = $12345678 (LE)
                                   '05' +            //tipo INT
                                   '80 05' +         //flags $8005 (BE): offsetinfo 8, bit 5
                                   '53' +            //nao otimizado 5, otimizado 3
                                   '0A 00 14 00'),   //otimizado 10, nao otimizado 20 (LE)
                           0, el);

  AssertEquals('lid',            5, el.Lid);
  AssertEquals('crc',            Int64($12345678), Int64(el.SymbolCrc));
  AssertEquals('type',           SDT_INT, el.SoftDataType);
  AssertEquals('offset type', 8, S7PlusVteOffsetInfoType(el));
  AssertEquals('optimised bit',  5, S7PlusVteAttributeBitOffset(el));
  AssertEquals('optimised',      10, el.OffsetInfo.OptAddr);
  AssertEquals('not optimised',  20, el.OffsetInfo.NonOptAddr);
  AssertEquals('bytes read',    16, fim);
end;

procedure TTestS7PlusTypeInfo.AVartypeListSkipsTheStartingIdOfTheFirstBlock;
var
  elementos:TS7PlusVartypeElementArray;
begin
  //bloco de $14 = 20 bytes (4 do id inicial + 16 do elemento), tamanho em
  //big-endian, terminado por um bloco de tamanho zero.
  ParseVartypeList(BytesOf('00 14' +
                           '01 00 00 00' +                 //id inicial - nao e' contagem
                           '05 00 00 00 78 56 34 12 05 80 05 53 0A 00 14 00' +
                           '00 00'),                       //bloco vazio encerra a lista
                   0, elementos);

  AssertEquals('number of elements', 1, Length(elementos));
  AssertEquals('lid of the element', 5, elementos[0].Lid);
end;

procedure TTestS7PlusTypeInfo.ANameListReadsTheLengthAndTheTrailingZero;
var
  nomes:TStringArray;
begin
  //cada nome e' [tamanho][caracteres][zero]; o bloco tem 9 bytes
  ParseVarnameList(BytesOf('00 09' +
                           '02 41 42 00' +      //"AB"
                           '03 43 44 45 00' +   //"CDE"
                           '00 00'),
                   0, nomes);

  AssertEquals('number of names', 2, Length(nomes));
  AssertEquals('first name', 'AB',  nomes[0]);
  AssertEquals('second name',  'CDE', nomes[1]);
end;

procedure TTestS7PlusTypeInfo.AnObjectWithNoOpeningComplains;
var
  obj:TS7PlusObject;
  reclamou:Boolean;
begin
  reclamou:=false;
  try
    ParseS7PlusObject(BytesOf('A3 01 00 04 00'), 0, obj);
  except
    on E:Exception do
      reclamou:=true;
  end;
  AssertTrue('without the opening $A1 it must complain', reclamou);
end;

procedure TTestS7PlusTypeInfo.TheObjectAttributesAreRead;
var
  obj:TS7PlusObject;
  atributos:TS7PlusPObjectAttributeArray;
  valor:TBytes;
begin
  SetLength(atributos, 2);
  atributos[0].AttrId:=10;
  atributos[0].Value :=EncodeValuePUSInt(7);
  atributos[1].AttrId:=1234;
  atributos[1].Value :=EncodeValuePUDInt(300);

  ParseS7PlusObject(EncodePObject($00000001, 534, 0, 0, atributos), 0, obj);

  AssertEquals('relation id', 1, obj.RelationId);
  AssertEquals('class', 534, obj.ClassId);
  AssertEquals('number of attributes', 2, Length(obj.AttrIds));

  AssertTrue('attribute 1234 found', S7PlusObjectAttr(obj, 1234, valor));
  AssertBytesEqual('attribute value', BytesOf('00 00 01 2C'), valor);
end;

procedure TTestS7PlusTypeInfo.AMissingAttributeInTheObjectReturnsFalse;
var
  obj:TS7PlusObject;
  atributos:TS7PlusPObjectAttributeArray;
  valor:TBytes;
begin
  SetLength(atributos, 1);
  atributos[0].AttrId:=10;
  atributos[0].Value :=EncodeValuePUSInt(7);

  ParseS7PlusObject(EncodePObject($00000001, 534, 0, 0, atributos), 0, obj);
  AssertFalse('attribute that does not exist', S7PlusObjectAttr(obj, 999, valor));
end;

procedure TTestS7PlusTypeInfo.ANestedObjectIsFoundByItsClass;
var
  objetos:TS7PlusObjectArray;
  achado:TS7PlusObject;
  atributos:TS7PlusPObjectAttributeArray;
  interno, externo:BYTES;
begin
  SetLength(atributos, 1);
  atributos[0].AttrId:=10;
  atributos[0].Value :=EncodeValuePUSInt(7);

  //um objeto de classe 534 (o container de type info) dentro de outro
  interno:=EncodePObject($00000002, 534, 0, 0, atributos);
  externo:=BytesCat(BytesCat(BytesOf('A1 00 00 00 01 82 2C 00 00'), interno),
                    BytesOf('A2'));

  ParseS7PlusObjectList(externo, 0, objetos);
  AssertEquals('one object at the root', 1, Length(objetos));
  AssertEquals('one child', 1, Length(objetos[0].Objects));

  //a busca precisa descer na arvore, nao so olhar a raiz
  AssertTrue('class 534 found', S7PlusFindContainer(objetos, 534, achado));
  AssertEquals('relation id of the one found', 2, achado.RelationId);

  AssertFalse('class that does not exist', S7PlusFindContainer(objetos, 999, achado));
end;

procedure TTestS7PlusTypeInfo.SiblingObjectsAreWalked;
var
  objetos:TS7PlusObjectArray;
  atributos:TS7PlusPObjectAttributeArray;
  fim:Integer;
  dados:BYTES;
begin
  SetLength(atributos, 0);
  dados:=BytesCat(EncodePObject($00000001, 300, 0, 0, atributos),
                  EncodePObject($00000002, 534, 0, 0, atributos));

  fim:=ParseS7PlusObjectList(dados, 0, objetos);

  AssertEquals('two sibling objects', 2, Length(objetos));
  AssertEquals('class of the first one', 300, objetos[0].ClassId);
  AssertEquals('class of the second one',  534, objetos[1].ClassId);
  AssertEquals('consumed everything', Length(dados), fim);
end;

procedure TTestS7PlusTypeInfo.LidsAreFormattedInHexSeparatedByDots;
var
  lids:TS7PlusLIDArray;
  vazio:TS7PlusLIDArray;
begin
  SetLength(lids, 3);
  lids[0]:=1;
  lids[1]:=10;
  lids[2]:=255;
  AssertEquals('path of lids', '1.A.FF', S7PlusFormatLids(lids));

  SetLength(vazio, 0);
  AssertEquals('empty path', '', S7PlusFormatLids(vazio));
end;

initialization
  RegisterTest(TTestS7PlusTypeInfo);

end.
