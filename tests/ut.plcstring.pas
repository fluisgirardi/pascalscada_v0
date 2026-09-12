{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TPLCString: conversao entre codepages e dimensionamento
            do bloco.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  As strings de teste sao montadas byte a byte, nunca por literal acentuado:
  um literal depende do codepage com que o fonte foi compilado, e os alvos do
  CI nao concordam nisso - o teste estaria medindo o compilador, nao a
  conversao.
}
{$ELSE}
{:
  @abstract(TPLCString tests: codepage conversion and block sizing.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Test strings are built byte by byte, never as accented literals: a literal
  depends on the codepage the source was compiled with, and the CI targets do
  not agree on that - the test would be measuring the compiler, not the
  conversion.
}
{$ENDIF}
unit ut.plcstring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, PLCString,
  testsupport.bytes;

type

  { TTestPLCString }

  TTestPLCString = class(TTestCase)
  private
    FTag:TPLCString;
    function  Bruto(const aHex:String):RawByteString;
    function  ComoUtf8(const aHex:String):UTF8String;
    function  HexDa(const aTexto:RawByteString):String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //conversao de codepage para UTF-8 / codepage to UTF-8 conversion
    procedure Utf8PassaOsBytesAdiante;
    procedure CP1252ViraUtf8;
    procedure Latin1ViraUtf8;
    procedure CP437ViraUtf8;
    procedure CP866ViraUtf8;
    procedure TextoVazioContinuaVazio;

    //conversao de UTF-8 para byte / UTF-8 to byte conversion
    procedure CaractereUtf8ViraByteDoCodepage;
    procedure IdaEVoltaPreservaOByte;
    procedure EmUtf8APosicaoEhDeByteNaoDeCaractere;

    //dimensionamento do bloco / block sizing
    procedure PadraoEhStringCComOnzeBytes;
    procedure SiemensGastaDoisBytesDeCabecalho;
    procedure ByteSizeDeSeteEncolheOBloco;
    procedure ByteSizeInvalidoEhRecusado;
    procedure ByteSizeDeSeteLimitaOTamanhoDaString;

    procedure LimiteDeTamanhoOlhaOValorPedido;

    //defeito conhecido / known defect
    procedure RockwellDeveDimensionarOBloco;
  end;

implementation

function TTestPLCString.Bruto(const aHex:String):RawByteString;
var
  b:BYTES;
  i:LongInt;
begin
  b:=BytesOf(aHex);
  SetLength(Result, Length(b));
  for i:=0 to High(b) do
    Result[i+1]:=Chr(b[i]);
end;

function TTestPLCString.ComoUtf8(const aHex:String):UTF8String;
var
  b:BYTES;
  i:LongInt;
begin
  b:=BytesOf(aHex);
  SetLength(Result, Length(b));
  for i:=0 to High(b) do
    Result[i+1]:=Chr(b[i]);
end;

function TTestPLCString.HexDa(const aTexto:RawByteString):String;
var
  i:LongInt;
begin
  Result:='';
  for i:=1 to Length(aTexto) do begin
    if Result<>'' then
      Result:=Result+' ';
    Result:=Result+IntToHex(Byte(aTexto[i]),2);
  end;
end;

procedure TTestPLCString.SetUp;
begin
  FTag:=TPLCString.Create(nil);
end;

procedure TTestPLCString.TearDown;
begin
  FreeAndNil(FTag);
end;

procedure TTestPLCString.Utf8PassaOsBytesAdiante;
begin
  //ja' esta' em UTF-8: nada a converter
  AssertEquals('sem conversao', '41 42 43',
               HexDa(TPLCString.ConvertRawByteStringToUTF8(Bruto('41 42 43'), UTF_8)));
end;

procedure TTestPLCString.CP1252ViraUtf8;
begin
  //no CP1252 o byte C9 e' o E maiusculo com acento agudo (U+00C9)
  AssertEquals('E com acento', 'C3 89',
               HexDa(TPLCString.ConvertRawByteStringToUTF8(Bruto('C9'), CP1252)));
end;

procedure TTestPLCString.Latin1ViraUtf8;
begin
  //latin-1: o byte E7 e' o c cedilha (U+00E7)
  AssertEquals('c cedilha', 'C3 A7',
               HexDa(TPLCString.ConvertRawByteStringToUTF8(Bruto('E7'), CP8859_1)));
end;

procedure TTestPLCString.CP437ViraUtf8;
begin
  //CP437 (DOS): o byte 80 e' o C cedilha maiusculo (U+00C7)
  AssertEquals('C cedilha', 'C3 87',
               HexDa(TPLCString.ConvertRawByteStringToUTF8(Bruto('80'), CP437)));
end;

procedure TTestPLCString.CP866ViraUtf8;
begin
  //CP866 (cirilico): o byte 80 e' o A cirilico (U+0410)
  AssertEquals('A cirilico', 'D0 90',
               HexDa(TPLCString.ConvertRawByteStringToUTF8(Bruto('80'), CP866)));
end;

procedure TTestPLCString.TextoVazioContinuaVazio;
begin
  AssertEquals('vazio em UTF-8', '',
               HexDa(TPLCString.ConvertRawByteStringToUTF8('', UTF_8)));
  AssertEquals('vazio em CP1252', '',
               HexDa(TPLCString.ConvertRawByteStringToUTF8('', CP1252)));
end;

procedure TTestPLCString.CaractereUtf8ViraByteDoCodepage;
begin
  //o caminho de volta: o E com acento (C3 89 em UTF-8) vira o byte C9 do CP1252
  AssertEquals('byte no CP1252', $C9,
               TPLCString.ConvertUTF8CharToByte(ComoUtf8('C3 89'), CP1252, 1));
end;

procedure TTestPLCString.IdaEVoltaPreservaOByte;
var
  emUtf8:UTF8String;
begin
  emUtf8:=TPLCString.ConvertRawByteStringToUTF8(Bruto('C9'), CP1252);
  AssertEquals('ida e volta pelo CP1252', $C9,
               TPLCString.ConvertUTF8CharToByte(emUtf8, CP1252, 1));
end;

procedure TTestPLCString.EmUtf8APosicaoEhDeByteNaoDeCaractere;
begin
  //em UTF_8 a funcao indexa BYTES: no C3 89 a posicao 2 e' o segundo byte do
  //mesmo caractere, nao um caractere seguinte. Quem chama precisa saber disso.
  AssertEquals('primeiro byte',  $C3, TPLCString.ConvertUTF8CharToByte(ComoUtf8('C3 89'), UTF_8, 1));
  AssertEquals('segundo byte',   $89, TPLCString.ConvertUTF8CharToByte(ComoUtf8('C3 89'), UTF_8, 2));
end;

procedure TTestPLCString.PadraoEhStringCComOnzeBytes;
begin
  //stC: os caracteres mais o terminador
  AssertEquals('tamanho da string', 10, FTag.StringSize);
  AssertEquals('bits por caractere', 8, FTag.ByteSize);
  AssertEquals('bytes do bloco',    11, FTag.Size);
end;

procedure TTestPLCString.SiemensGastaDoisBytesDeCabecalho;
begin
  //stSIEMENS: tamanho maximo e tamanho atual antes dos caracteres
  FTag.StringType:=stSIEMENS;
  AssertEquals('bytes do bloco', 12, FTag.Size);
end;

procedure TTestPLCString.ByteSizeDeSeteEncolheOBloco;
begin
  //11 caracteres de 7 bits = 77 bits = 10 bytes (arredondando para cima)
  FTag.ByteSize:=7;
  AssertEquals('bytes do bloco', 10, FTag.Size);
end;

procedure TTestPLCString.ByteSizeInvalidoEhRecusado;
var
  recusouMenor, recusouMaior:Boolean;
begin
  recusouMenor:=false;
  try
    FTag.ByteSize:=6;
  except
    on E:Exception do recusouMenor:=true;
  end;

  recusouMaior:=false;
  try
    FTag.ByteSize:=9;
  except
    on E:Exception do recusouMaior:=true;
  end;

  AssertTrue('6 bits deve ser recusado', recusouMenor);
  AssertTrue('9 bits deve ser recusado', recusouMaior);
  AssertEquals('e o valor anterior fica', 8, FTag.ByteSize);
end;

procedure TTestPLCString.ByteSizeDeSeteLimitaOTamanhoDaString;
begin
  //com 7 bits o maior indice representavel e' 127
  FTag.StringSize:=200;
  FTag.ByteSize:=7;
  AssertEquals('tamanho limitado', 127, FTag.StringSize);
end;

procedure TTestPLCString.LimiteDeTamanhoOlhaOValorPedido;
var
  aceitou:Boolean;
begin
  //o limite tem que olhar o valor pedido, nao o tamanho que o bloco ja tem
  aceitou:=true;
  try
    FTag.StringSize:=1000;
  except
    on E:Exception do aceitou:=false;
  end;
  AssertFalse('com 8 bits por caractere o maximo e 255', aceitou);

  AssertEquals('e o tamanho anterior fica de pe', 10, FTag.StringSize);

  //o segundo sintoma: com o limite conferido contra o bloco, um valor valido
  //passava a ser recusado depois que um absurdo era aceito.
  FTag.StringSize:=200;
  AssertEquals('valor valido continua aceito', 200, FTag.StringSize);
  AssertEquals('e o bloco acompanha',          201, FTag.Size);
end;

procedure TTestPLCString.RockwellDeveDimensionarOBloco;
begin
  Ignore('limitacao ja anotada no proprio codigo (TODO em CalcBlockSize): ' +
         'stROCKWELL nao esta implementado, cai no "else" e o bloco fica com 1 ' +
         'byte, fazendo o tag devolver sempre string vazia. Enquanto isso, o ' +
         'contorno documentado e usar stC apontando para o membro DATA. Remova ' +
         'este Ignore quando a estrutura STRING (LEN:DINT + DATA:SINT[n]) for ' +
         'codificada.');

  FTag.StringType:=stROCKWELL;
  AssertTrue('o bloco precisa comportar os caracteres', FTag.Size>10);
end;

initialization
  RegisterTest(TTestPLCString);

end.
