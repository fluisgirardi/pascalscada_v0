{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do S7PlusCodec: cabecalhos, primitivas e PValues do
            S7CommPlus.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Alguns testes aqui sao cerca em volta de erro que ja custou depuracao ao
  vivo e esta anotado no proprio codigo: UINT/INT sao dois bytes fixos (nao
  VLQ) e WSTRING e' um byte por caractere (nao UTF-16). Os dois desalinhavam
  todos os atributos seguintes da mensagem, com sintoma generico e longe da
  causa - exatamente o tipo de regressao que um teste de bytes pega na hora.
}
{$ELSE}
{:
  @abstract(S7PlusCodec tests: S7CommPlus headers, primitives and PValues.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Some tests here are a fence around mistakes that already cost live
  debugging and are documented in the code itself: UINT/INT are fixed two
  byte values (not VLQ) and WSTRING is one byte per character (not UTF-16).
  Both desynchronized every following attribute of the message, with a
  generic symptom far from the cause - exactly the kind of regression a byte
  level test catches immediately.
}
{$ENDIF}
unit ut.s7pluscodec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  S7PlusCodec,
  testsupport.bytes;

type

  { TTestS7PlusCodec }

  TTestS7PlusCodec = class(TTestCase)
  published
    //cabecalho do quadro / frame header
    procedure TheHeaderHasIdVersionAndLength;
    procedure TheHeaderComesBackWithVersionAndLength;
    procedure AHeaderWithTheWrongIdComplains;
    procedure ATruncatedHeaderComplains;

    //cabecalhos de pedido e resposta / request and response headers
    procedure ARequestHeaderIsFourteenBytes;
    procedure AnAnswerHeaderIsReadBack;
    procedure ATruncatedAnswerHeaderComplains;

    //primitivas de tamanho fixo / fixed width primitives
    procedure IntegersAreBigEndian;
    procedure IntegersComeBackFromBigEndian;
    procedure FloatsFollowIEEE754BigEndian;
    procedure FloatsGoOutAndComeBack;

    //valores tipados / typed values
    procedure ATypedUDIntValueUsesVLQ;
    procedure ATypedDWordValueUsesFourFixedBytes;
    procedure ATypedBoolValueIsNormalisedToZeroOrOne;
    procedure ATypedValueWithAnUnsupportedTypeComplains;
    procedure ARIDIsFourFixedBytes;

    //PValues de escrita / write PValues
    procedure ABlobCarriesRootZeroAndLength;
    procedure AByteArrayUsesTheArrayFlag;
    procedure WritingADIntBecomesASignedVLQ;
    procedure WritingANegativeDIntBecomesASignedVLQ;
    procedure WritingARealPassesTheBytesStraightThrough;
    procedure WritingABoolUsesTheLastByte;
    procedure WritingAStringUsesAnArrayOfUSInt;
    procedure WritingAnUnknownTypeFallsBackToAByteArray;

    //PValues escalares / scalar PValues
    procedure UIntAndIntAreTwoFixedBytes;
    procedure UDIntUsesVLQ;
    procedure AWStringIsOneBytePerCharacter;
    procedure AnArrayOfUDIntHonoursTheFlagAskedFor;
    procedure ALIntGoesOutAndBackThroughTheDecoder;

    //objetos / objects
    procedure AnObjectHasAnOpeningAttributesAndAClosing;
    procedure AnAttributeIsFoundInTheObject;
    procedure AMissingAttributeReturnsFalse;
    procedure AV1QualifierEndsWithAFixedSizeUDInt;
    procedure AnItemAddressCountsTheFields;

    //decodificacao de PValue / PValue decoding
    procedure AUDIntPValueComesBackWithFourBytes;
  end;

implementation

procedure TTestS7PlusCodec.TheHeaderHasIdVersionAndLength;
begin
  //o $72 = id do protocolo S7CommPlus
  AssertBytesEqual('header', BytesOf('72 01 01 23'), EncodeS7PlusHeader($01, $0123));
end;

procedure TTestS7PlusCodec.TheHeaderComesBackWithVersionAndLength;
var
  versao:Byte;
  tamanho:Word;
  consumido:Integer;
begin
  consumido:=DecodeS7PlusHeader(BytesOf('72 02 00 10 AA BB'), 0, versao, tamanho);
  AssertEquals('bytes consumed', 4, consumido);
  AssertEquals('version', 2, versao);
  AssertEquals('size', 16, tamanho);
end;

procedure TTestS7PlusCodec.AHeaderWithTheWrongIdComplains;
var
  versao:Byte;
  tamanho:Word;
  reclamou:Boolean;
begin
  reclamou:=false;
  try
    DecodeS7PlusHeader(BytesOf('73 01 00 10'), 0, versao, tamanho);
  except
    on E:Exception do
      reclamou:=true;
  end;
  AssertTrue('a protocol id other than $72 must complain', reclamou);
end;

procedure TTestS7PlusCodec.ATruncatedHeaderComplains;
var
  versao:Byte;
  tamanho:Word;
  reclamou:Boolean;
begin
  reclamou:=false;
  try
    DecodeS7PlusHeader(BytesOf('72 01'), 0, versao, tamanho);
  except
    on E:Exception do
      reclamou:=true;
  end;
  AssertTrue('a header shorter than 4 bytes must complain', reclamou);
end;

procedure TTestS7PlusCodec.ARequestHeaderIsFourteenBytes;
begin
  //opcode $31 (request), funcao $04BB, sequencia 2, sessao $12345678, flags $36
  AssertBytesEqual('request header',
                   BytesOf('31 00 00 04 BB 00 00 00 02 12 34 56 78 36'),
                   EncodeRequestHeader($04BB, $0002, $12345678, $36));
end;

procedure TTestS7PlusCodec.AnAnswerHeaderIsReadBack;
var
  cab:TS7PlusResponseHeader;
begin
  cab:=DecodeResponseHeader(BytesOf('32 00 00 04 BB 00 00 00 07 36'), 0);
  AssertEquals('opcode', $32, cab.Opcode);
  AssertEquals('function', $04BB, cab.FunctionCode);
  AssertEquals('sequence', 7, cab.SequenceNumber);
  AssertEquals('transport flags', $36, cab.TransportFlags);
  //a resposta nao repete o id de sessao
  AssertEquals('session id', 0, cab.SessionId);
end;

procedure TTestS7PlusCodec.ATruncatedAnswerHeaderComplains;
var
  reclamou:Boolean;
begin
  reclamou:=false;
  try
    DecodeResponseHeader(BytesOf('32 00 00 04 BB'), 0);
  except
    on E:Exception do
      reclamou:=true;
  end;
  AssertTrue('an answer shorter than 10 bytes must complain', reclamou);
end;

procedure TTestS7PlusCodec.IntegersAreBigEndian;
begin
  AssertBytesEqual('uint8',  BytesOf('7F'), EncodeUInt8($7F));
  AssertBytesEqual('uint16', BytesOf('12 34'), EncodeUInt16($1234));
  AssertBytesEqual('uint32', BytesOf('12 34 56 78'), EncodeUInt32($12345678));
  AssertBytesEqual('uint64', BytesOf('01 02 03 04 05 06 07 08'),
                   EncodeUInt64(QWord($0102030405060708)));
end;

procedure TTestS7PlusCodec.IntegersComeBackFromBigEndian;
begin
  AssertEquals('uint16', $1234, DecodeUInt16(BytesOf('12 34'), 0));
  AssertEquals('uint32', Int64($12345678), Int64(DecodeUInt32(BytesOf('12 34 56 78'), 0)));
  AssertEquals('uint64', QWord($0102030405060708),
               DecodeUInt64(BytesOf('01 02 03 04 05 06 07 08'), 0));

  //com deslocamento, que e' como o codec anda pela mensagem
  AssertEquals('uint16 with an offset', $5678, DecodeUInt16(BytesOf('12 34 56 78'), 2));
end;

procedure TTestS7PlusCodec.FloatsFollowIEEE754BigEndian;
begin
  AssertBytesEqual('single 1.0',  BytesOf('3F 80 00 00'), EncodeFloat32(1.0));
  AssertBytesEqual('single -12.5', BytesOf('C1 48 00 00'), EncodeFloat32(-12.5));
  AssertBytesEqual('double 1.0',  BytesOf('3F F0 00 00 00 00 00 00'), EncodeFloat64(1.0));
end;

procedure TTestS7PlusCodec.FloatsGoOutAndComeBack;
begin
  AssertEquals('single', 3.5, DecodeFloat32(EncodeFloat32(3.5), 0), 0);
  AssertEquals('double', -0.125, DecodeFloat64(EncodeFloat64(-0.125), 0), 0);
end;

procedure TTestS7PlusCodec.ATypedUDIntValueUsesVLQ;
begin
  //o $04 = UDInt, valor 300 em VLQ
  AssertBytesEqual('udint', BytesOf('04 82 2C'), EncodeTypedValueUInt($04, 300));
end;

procedure TTestS7PlusCodec.ATypedDWordValueUsesFourFixedBytes;
begin
  //o $0C = DWord: mesmo valor, largura fixa
  AssertBytesEqual('dword', BytesOf('0C 00 00 01 2C'), EncodeTypedValueUInt($0C, 300));
end;

procedure TTestS7PlusCodec.ATypedBoolValueIsNormalisedToZeroOrOne;
begin
  AssertBytesEqual('bool true', BytesOf('01 01'), EncodeTypedValueUInt($01, 7));
  AssertBytesEqual('bool false',      BytesOf('01 00'), EncodeTypedValueUInt($01, 0));
end;

procedure TTestS7PlusCodec.ATypedValueWithAnUnsupportedTypeComplains;
var
  reclamou:Boolean;
begin
  //o $0E = Real: nao e' um inteiro, nao pode ser codificado por aqui
  reclamou:=false;
  try
    EncodeTypedValueUInt($0E, 1);
  except
    on E:Exception do
      reclamou:=true;
  end;
  AssertTrue('an unsupported type must complain', reclamou);
end;

procedure TTestS7PlusCodec.ARIDIsFourFixedBytes;
begin
  //o $12 = RID
  AssertBytesEqual('rid', BytesOf('12 12 34 56 78'), EncodeTypedValueRID($12345678));
end;

procedure TTestS7PlusCodec.ABlobCarriesRootZeroAndLength;
begin
  //flags $00, tipo $14 (Blob), raiz 0, tamanho 2, dados
  AssertBytesEqual('blob', BytesOf('00 14 00 02 AA BB'), EncodePValueBlob(BytesOf('AA BB')));
end;

procedure TTestS7PlusCodec.AByteArrayUsesTheArrayFlag;
begin
  //flags $10 (array), tipo $0A (Byte), contagem 2, dados
  AssertBytesEqual('array of bytes', BytesOf('10 0A 02 AA BB'),
                   EncodePValueByteArray(BytesOf('AA BB')));
end;

procedure TTestS7PlusCodec.WritingADIntBecomesASignedVLQ;
begin
  //SoftDataType 7 = DINT; 300 cru em big-endian vira tipo $08 + VLQ assinado
  AssertBytesEqual('dint 300', BytesOf('00 08 82 2C'),
                   EncodeTypedWriteValue(7, BytesOf('00 00 01 2C')));
end;

procedure TTestS7PlusCodec.WritingANegativeDIntBecomesASignedVLQ;
begin
  //-1 cru = FF FF FF FF; o VLQ assinado de -1 e' um unico byte $7F
  AssertBytesEqual('dint -1', BytesOf('00 08 7F'),
                   EncodeTypedWriteValue(7, BytesOf('FF FF FF FF')));
end;

procedure TTestS7PlusCodec.WritingARealPassesTheBytesStraightThrough;
begin
  //SoftDataType 8 = REAL: o padrao IEEE-754 vai como esta
  AssertBytesEqual('real 1.0', BytesOf('00 0E 3F 80 00 00'),
                   EncodeTypedWriteValue(8, BytesOf('3F 80 00 00')));
end;

procedure TTestS7PlusCodec.WritingABoolUsesTheLastByte;
begin
  //SoftDataType 1 = BOOL: aproveita so o byte menos significativo
  AssertBytesEqual('bool', BytesOf('00 01 01'),
                   EncodeTypedWriteValue(1, BytesOf('00 01')));
end;

procedure TTestS7PlusCodec.WritingAStringUsesAnArrayOfUSInt;
begin
  //SoftDataType 19 = STRING: array de USInt ($10 $02) com a sequencia
  //classica do S7 [tamanho maximo][tamanho atual][caracteres]
  AssertBytesEqual('string', BytesOf('10 02 04 FF 02 41 42'),
                   EncodeTypedWriteValue(19, BytesOf('FF 02 41 42')));
end;

procedure TTestS7PlusCodec.WritingAnUnknownTypeFallsBackToAByteArray;
begin
  //SoftDataType 0 (desconhecido) usa o array de bytes generico
  AssertBytesEqual('unknown type', BytesOf('10 0A 02 AA BB'),
                   EncodeTypedWriteValue(0, BytesOf('AA BB')));
end;

procedure TTestS7PlusCodec.UIntAndIntAreTwoFixedBytes;
begin
  //cerca de regressao: um VLQ aqui gastaria 1 byte para o zero e desalinharia
  //todos os atributos seguintes do objeto.
  AssertBytesEqual('uint 0',  BytesOf('00 03 00 00'), EncodeValuePUInt(0));
  AssertBytesEqual('uint 300',BytesOf('00 03 01 2C'), EncodeValuePUInt(300));
  AssertBytesEqual('int -2',  BytesOf('00 07 FF FE'), EncodeValuePInt(-2));
end;

procedure TTestS7PlusCodec.UDIntUsesVLQ;
begin
  AssertBytesEqual('udint 300', BytesOf('00 04 82 2C'), EncodeValuePUDInt(300));
end;

procedure TTestS7PlusCodec.AWStringIsOneBytePerCharacter;
begin
  //cerca de regressao: apesar do nome, aqui WSTRING e' um byte por caractere;
  //em UTF-16 o tamanho declarado (2) nao bateria com os 4 bytes enviados.
  AssertBytesEqual('wstring AB', BytesOf('00 15 02 41 42'), EncodeValuePWString('AB'));
end;

procedure TTestS7PlusCodec.AnArrayOfUDIntHonoursTheFlagAskedFor;
begin
  //flags $20 = "Addressarray", usado pelo GetVarSubstreamed e pela Subscription
  AssertBytesEqual('array of udint', BytesOf('20 04 02 01 82 2C'),
                   EncodeValuePUDIntArray([1, 300], $20));
end;

procedure TTestS7PlusCodec.ALIntGoesOutAndBackThroughTheDecoder;
var
  consumido:Integer;
begin
  //LInt vai em VLQ assinado e volta como 8 bytes big-endian
  AssertBytesEqual('lint -1',
                   BytesOf('FF FF FF FF FF FF FF FF'),
                   DecodePValueToBytes(EncodeValuePLInt(-1), 0, consumido));
end;

procedure TTestS7PlusCodec.AnObjectHasAnOpeningAttributesAndAClosing;
var
  atributos:TS7PlusPObjectAttributeArray;
begin
  SetLength(atributos, 1);
  atributos[0].AttrId:=1234;
  atributos[0].Value :=EncodeValuePUSInt(7);

  //o $A1 abre, $A3 marca atributo, $A2 fecha. ClassId 300 e o atributo 1234
  //vao em VLQ; o RelationId e' fixo de 4 bytes.
  AssertBytesEqual('object',
                   BytesOf('A1 12 34 56 78 82 2C 00 00 A3 89 52 00 02 07 A2'),
                   EncodePObject($12345678, 300, 0, 0, atributos));
end;

procedure TTestS7PlusCodec.AnAttributeIsFoundInTheObject;
var
  objeto, valor:TBytes;
  atributos:TS7PlusPObjectAttributeArray;
begin
  SetLength(atributos, 2);
  atributos[0].AttrId:=10;
  atributos[0].Value :=EncodeValuePUSInt(7);
  atributos[1].AttrId:=1234;
  atributos[1].Value :=EncodeValuePUDInt(300);

  objeto:=EncodePObject($00000001, 300, 0, 0, atributos);

  //achar o segundo atributo exige pular corretamente o primeiro
  AssertTrue('attribute 1234 must be found',
             ParseAttributeRawValue(objeto, 1234, valor));
  AssertBytesEqual('attribute value', BytesOf('00 00 01 2C'), valor);
end;

procedure TTestS7PlusCodec.AMissingAttributeReturnsFalse;
var
  objeto, valor:TBytes;
  atributos:TS7PlusPObjectAttributeArray;
begin
  SetLength(atributos, 1);
  atributos[0].AttrId:=10;
  atributos[0].Value :=EncodeValuePUSInt(7);

  objeto:=EncodePObject($00000001, 300, 0, 0, atributos);
  AssertFalse('attribute that does not exist', ParseAttributeRawValue(objeto, 999, valor));
  AssertEquals('the value must come back empty', 0, Length(valor));
end;

procedure TTestS7PlusCodec.AV1QualifierEndsWithAFixedSizeUDInt;
begin
  //ObjectQualifier 1256, ParentRID 1257, CompositionAID 1258, KeyQualifier 1259.
  //Na variante V1 o KeyQualifier vai em 4 bytes fixos e sem terminador.
  AssertBytesEqual('v1 qualifier',
                   BytesOf('00 00 04 E8 89 69 00 12 00 00 00 00 89 6A 00 13 00 89 6B 00 04 00 00 00 05'),
                   EncodeObjectQualifierV1(5));
end;

procedure TTestS7PlusCodec.AnItemAddressCountsTheFields;
var
  ender:TS7PlusItemAddress;
begin
  //crc 0, area 300, (numero de lids + 1), subarea 1, lid 2
  ender:=EncodeItemAddress(300, 1, [2], 0);
  AssertBytesEqual('address', BytesOf('00 82 2C 02 01 02'), ender.Data);
  //o contador informado ao PLC inclui crc, area, contagem, subarea e cada lid
  AssertEquals('fields', 5, ender.FieldCount);
end;

procedure TTestS7PlusCodec.AUDIntPValueComesBackWithFourBytes;
var
  consumido:Integer;
begin
  AssertBytesEqual('udint decoded', BytesOf('00 00 01 2C'),
                   DecodePValueToBytes(BytesOf('00 04 82 2C'), 0, consumido));
  AssertEquals('bytes consumed', 4, consumido);
end;

initialization
  RegisterTest(TTestS7PlusCodec);

end.
