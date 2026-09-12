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
    procedure TiposEscalaresConhecidosSaoSuportados;
    procedure TiposEstruturaisNaoSaoSuportados;
    procedure TamanhoDosTiposEscalares;
    procedure StringSomaDoisAoTamanhoDeclarado;
    procedure TipoSemTamanhoConhecidoDaZero;

    //campos de bits do VartypeElement / VartypeElement bit fields
    procedure TipoDeOffsetInfoVemDosQuatroBitsAltos;
    procedure BitOffsetOtimizadoVemDosTresBitsBaixos;
    procedure BitOffsetsEModoClassicoVemDoByteDeFlags;

    //offset info (little-endian) / offset info (little endian)
    procedure OffsetInfoTipo8LeOtimizadoPrimeiro;
    procedure OffsetInfoTipo1InverteAOrdem;
    procedure OffsetInfoDeArrayTrazLimiteInferiorEContagem;

    //elementos e listas / elements and lists
    procedure ElementoDeVartypeMisturaLittleEBigEndian;
    procedure ListaDeVartypePulaOIdInicialDoPrimeiroBloco;
    procedure ListaDeNomesLeTamanhoEZeroFinal;

    //objetos / objects
    procedure ObjetoSemAberturaReclama;
    procedure AtributosDoObjetoSaoLidos;
    procedure AtributoAusenteNoObjetoDevolveFalso;
    procedure ObjetoAninhadoEhEncontradoPelaClasse;
    procedure ObjetosIrmaosSaoPercorridos;

    //formatacao / formatting
    procedure LidsSaoFormatadosEmHexaSeparadosPorPonto;
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

procedure TTestS7PlusTypeInfo.TiposEscalaresConhecidosSaoSuportados;
begin
  AssertTrue('bool',   IsSoftDataTypeSupported(SDT_BOOL));
  AssertTrue('int',    IsSoftDataTypeSupported(SDT_INT));
  AssertTrue('real',   IsSoftDataTypeSupported(SDT_REAL));
  AssertTrue('string', IsSoftDataTypeSupported(SDT_STRING));
  AssertTrue('lreal',  IsSoftDataTypeSupported(SDT_LREAL));
  AssertTrue('udint',  IsSoftDataTypeSupported(SDT_UDINT));
end;

procedure TTestS7PlusTypeInfo.TiposEstruturaisNaoSaoSuportados;
begin
  //array e struct nao sao lidos como valor: sao nos da arvore, e quem os
  //atravessa e' o construtor da lista achatada.
  AssertFalse('array',        IsSoftDataTypeSupported(SDT_ARRAY));
  AssertFalse('struct',       IsSoftDataTypeSupported(SDT_STRUCT));
  AssertFalse('variant',      IsSoftDataTypeSupported(SDT_VARIANT));
  AssertFalse('dtl',          IsSoftDataTypeSupported(SDT_DTL));
  AssertFalse('void',         IsSoftDataTypeSupported(0));
  AssertFalse('desconhecido', IsSoftDataTypeSupported(SDT_UNKNOWN));
end;

procedure TTestS7PlusTypeInfo.TamanhoDosTiposEscalares;
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

procedure TTestS7PlusTypeInfo.StringSomaDoisAoTamanhoDeclarado;
begin
  //os dois bytes a mais sao o cabecalho classico do S7: tamanho maximo e atual
  AssertEquals('string[10]',  12, S7PlusDataTypeSize(SDT_STRING, 10));
  AssertEquals('wstring[4]',   6, S7PlusDataTypeSize(SDT_WSTRING, 4));
  AssertEquals('string sem tamanho', 2, S7PlusDataTypeSize(SDT_STRING));
end;

procedure TTestS7PlusTypeInfo.TipoSemTamanhoConhecidoDaZero;
begin
  //zero aqui e' o sinal de "nao sei medir isso", nao um tipo de zero byte
  AssertEquals('array',        0, S7PlusDataTypeSize(SDT_ARRAY));
  AssertEquals('struct',       0, S7PlusDataTypeSize(SDT_STRUCT));
  AssertEquals('desconhecido', 0, S7PlusDataTypeSize(SDT_UNKNOWN));
end;

procedure TTestS7PlusTypeInfo.TipoDeOffsetInfoVemDosQuatroBitsAltos;
var
  el:TS7PlusVartypeElement;
begin
  FillChar(el, SizeOf(el), 0);

  el.AttributeFlags:=$8000;
  AssertEquals('tipo 8', 8, S7PlusVteOffsetInfoType(el));

  el.AttributeFlags:=$3ABC;
  AssertEquals('tipo 3', 3, S7PlusVteOffsetInfoType(el));

  el.AttributeFlags:=$0FFF;
  AssertEquals('tipo 0', 0, S7PlusVteOffsetInfoType(el));
end;

procedure TTestS7PlusTypeInfo.BitOffsetOtimizadoVemDosTresBitsBaixos;
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

procedure TTestS7PlusTypeInfo.BitOffsetsEModoClassicoVemDoByteDeFlags;
var
  el:TS7PlusVartypeElement;
begin
  FillChar(el, SizeOf(el), 0);

  //o $53 = 0101 0011: nao otimizado 5, otimizado 3, sem o bit de classico
  el.BitOffsetInfoFlags:=$53;
  AssertEquals('nao otimizado', 5, S7PlusVteNonOptBitOffset(el));
  AssertEquals('otimizado',     3, S7PlusVteOptBitOffset(el));
  AssertFalse ('classico',         S7PlusVteClassic(el));

  //o $5B = 0101 1011: mesmos offsets, agora com o bit $08 de bloco classico
  el.BitOffsetInfoFlags:=$5B;
  AssertEquals('nao otimizado', 5, S7PlusVteNonOptBitOffset(el));
  AssertEquals('otimizado',     3, S7PlusVteOptBitOffset(el));
  AssertTrue  ('classico',         S7PlusVteClassic(el));
end;

procedure TTestS7PlusTypeInfo.OffsetInfoTipo8LeOtimizadoPrimeiro;
var
  info:TS7PlusOffsetInfo;
  fim:Integer;
begin
  //little-endian: 01 00 = 1, 02 00 = 2
  fim:=ParseOffsetInfo(BytesOf('01 00 02 00'), 0, 8, info);
  AssertEquals('otimizado',     1, info.OptAddr);
  AssertEquals('nao otimizado', 2, info.NonOptAddr);
  AssertEquals('bytes lidos',   4, fim);
  AssertEquals('codigo',        8, info.Code);
end;

procedure TTestS7PlusTypeInfo.OffsetInfoTipo1InverteAOrdem;
var
  info:TS7PlusOffsetInfo;
begin
  //mesmos bytes do teste anterior: no tipo 1 (legado) a ordem e' a inversa
  ParseOffsetInfo(BytesOf('01 00 02 00'), 0, 1, info);
  AssertEquals('nao otimizado', 1, info.NonOptAddr);
  AssertEquals('otimizado',     2, info.OptAddr);
end;

procedure TTestS7PlusTypeInfo.OffsetInfoDeArrayTrazLimiteInferiorEContagem;
var
  info:TS7PlusOffsetInfo;
  fim:Integer;
begin
  //2 nao usados, 2 nao usados, otimizado=10, nao otimizado=20,
  //limite inferior=-5 (FB FF FF FF em complemento de dois), contagem=10
  fim:=ParseOffsetInfo(BytesOf('00 00 00 00 0A 00 00 00 14 00 00 00 FB FF FF FF 0A 00 00 00'),
                       0, 3, info);
  AssertEquals('otimizado',        10, info.OptAddr);
  AssertEquals('nao otimizado',    20, info.NonOptAddr);
  AssertEquals('limite inferior',  -5, info.ArrayLowerBound);
  AssertEquals('quantos elementos',10, info.ArrayElementCount);
  AssertTrue  ('marcado como uma dimensao', info.Is1Dim);
  AssertEquals('bytes lidos',      20, fim);
end;

procedure TTestS7PlusTypeInfo.ElementoDeVartypeMisturaLittleEBigEndian;
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
  AssertEquals('tipo',           SDT_INT, el.SoftDataType);
  AssertEquals('tipo de offset', 8, S7PlusVteOffsetInfoType(el));
  AssertEquals('bit otimizado',  5, S7PlusVteAttributeBitOffset(el));
  AssertEquals('otimizado',      10, el.OffsetInfo.OptAddr);
  AssertEquals('nao otimizado',  20, el.OffsetInfo.NonOptAddr);
  AssertEquals('bytes lidos',    16, fim);
end;

procedure TTestS7PlusTypeInfo.ListaDeVartypePulaOIdInicialDoPrimeiroBloco;
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

  AssertEquals('quantidade de elementos', 1, Length(elementos));
  AssertEquals('lid do elemento', 5, elementos[0].Lid);
end;

procedure TTestS7PlusTypeInfo.ListaDeNomesLeTamanhoEZeroFinal;
var
  nomes:TStringArray;
begin
  //cada nome e' [tamanho][caracteres][zero]; o bloco tem 9 bytes
  ParseVarnameList(BytesOf('00 09' +
                           '02 41 42 00' +      //"AB"
                           '03 43 44 45 00' +   //"CDE"
                           '00 00'),
                   0, nomes);

  AssertEquals('quantidade de nomes', 2, Length(nomes));
  AssertEquals('primeiro nome', 'AB',  nomes[0]);
  AssertEquals('segundo nome',  'CDE', nomes[1]);
end;

procedure TTestS7PlusTypeInfo.ObjetoSemAberturaReclama;
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
  AssertTrue('sem o $A1 de abertura deve reclamar', reclamou);
end;

procedure TTestS7PlusTypeInfo.AtributosDoObjetoSaoLidos;
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

  AssertEquals('id de relacao', 1, obj.RelationId);
  AssertEquals('classe', 534, obj.ClassId);
  AssertEquals('quantidade de atributos', 2, Length(obj.AttrIds));

  AssertTrue('atributo 1234 encontrado', S7PlusObjectAttr(obj, 1234, valor));
  AssertBytesEqual('valor do atributo', BytesOf('00 00 01 2C'), valor);
end;

procedure TTestS7PlusTypeInfo.AtributoAusenteNoObjetoDevolveFalso;
var
  obj:TS7PlusObject;
  atributos:TS7PlusPObjectAttributeArray;
  valor:TBytes;
begin
  SetLength(atributos, 1);
  atributos[0].AttrId:=10;
  atributos[0].Value :=EncodeValuePUSInt(7);

  ParseS7PlusObject(EncodePObject($00000001, 534, 0, 0, atributos), 0, obj);
  AssertFalse('atributo inexistente', S7PlusObjectAttr(obj, 999, valor));
end;

procedure TTestS7PlusTypeInfo.ObjetoAninhadoEhEncontradoPelaClasse;
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
  AssertEquals('um objeto na raiz', 1, Length(objetos));
  AssertEquals('um filho', 1, Length(objetos[0].Objects));

  //a busca precisa descer na arvore, nao so olhar a raiz
  AssertTrue('classe 534 encontrada', S7PlusFindContainer(objetos, 534, achado));
  AssertEquals('id de relacao do achado', 2, achado.RelationId);

  AssertFalse('classe inexistente', S7PlusFindContainer(objetos, 999, achado));
end;

procedure TTestS7PlusTypeInfo.ObjetosIrmaosSaoPercorridos;
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

  AssertEquals('dois objetos irmaos', 2, Length(objetos));
  AssertEquals('classe do primeiro', 300, objetos[0].ClassId);
  AssertEquals('classe do segundo',  534, objetos[1].ClassId);
  AssertEquals('consumiu tudo', Length(dados), fim);
end;

procedure TTestS7PlusTypeInfo.LidsSaoFormatadosEmHexaSeparadosPorPonto;
var
  lids:TS7PlusLIDArray;
  vazio:TS7PlusLIDArray;
begin
  SetLength(lids, 3);
  lids[0]:=1;
  lids[1]:=10;
  lids[2]:=255;
  AssertEquals('caminho de lids', '1.A.FF', S7PlusFormatLids(lids));

  SetLength(vazio, 0);
  AssertEquals('caminho vazio', '', S7PlusFormatLids(vazio));
end;

initialization
  RegisterTest(TTestS7PlusTypeInfo);

end.
