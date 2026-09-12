{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Apoio aos testes: leitura/escrita de vetores de bytes em hexa e
            comparacao com mensagem legivel.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Testes de protocolo sao escritos em cima de frames literais. Comparar
  BYTES com AssertEquals byte a byte produz mensagens inuteis quando falha
  ("esperado 3, veio 4" - em que posicao? de que frame?), entao aqui o frame
  inteiro aparece dos dois lados na mensagem de erro.
}
{$ELSE}
{:
  @abstract(Test support: hex literals to/from byte arrays and a comparison
            with a readable failure message.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Protocol tests are written on top of literal frames. Comparing BYTES with
  AssertEquals byte by byte gives useless messages on failure ("expected 3,
  got 4" - at which position? of which frame?), so here the whole frame shows
  up on both sides of the error message.
}
{$ENDIF}
unit testsupport.bytes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, commtypes;

{$IFDEF PORTUGUES}
{:
Converte uma string hexa num vetor de bytes. Espaco, tabulacao, quebra de
linha, "-", ":" e "," sao ignorados, entao '01 03 00 00' e '0103-0000' dao
o mesmo resultado.
@raises(Exception se aparecer um caractere que nao e hexa nem separador, ou
        se o numero de digitos for impar.)
}
{$ELSE}
{:
Converts a hex string into a byte array. Space, tab, line break, "-", ":" and
"," are ignored, so '01 03 00 00' and '0103-0000' give the same result.
@raises(Exception if a character is neither a hex digit nor a separator, or
        if the digit count is odd.)
}
{$ENDIF}
function  BytesOf(const aHex:String):BYTES;

{$IFDEF PORTUGUES}
//: Converte um vetor de bytes em string hexa, um byte por grupo ('01 03 00').
{$ELSE}
//: Converts a byte array into a hex string, one byte per group ('01 03 00').
{$ENDIF}
function  HexOf(const aBytes:BYTES):String;

{$IFDEF PORTUGUES}
//: Concatena dois vetores de bytes.
{$ELSE}
//: Concatenates two byte arrays.
{$ENDIF}
function  BytesCat(const aLeft, aRight:BYTES):BYTES;

{$IFDEF PORTUGUES}
{:
Compara dois vetores de bytes. Em caso de diferenca, falha o teste mostrando
os dois frames inteiros em hexa e a posicao do primeiro byte diferente.
}
{$ELSE}
{:
Compares two byte arrays. On a difference, fails the test showing both frames
in hex and the position of the first differing byte.
}
{$ENDIF}
procedure AssertBytesEqual(const aMsg:String; const aExpected, aActual:BYTES);

implementation

uses
  fpcunit;

function BytesOf(const aHex:String):BYTES;
var
  digits:String;
  i:LongInt;
begin
  digits:='';
  for i:=1 to Length(aHex) do
    case aHex[i] of
      '0'..'9','A'..'F','a'..'f':
        digits:=digits+aHex[i];
      ' ',#9,#10,#13,'-',':',',':
        ; //separadores, ignora / separators, skip
      else
        raise Exception.CreateFmt('BytesOf: caractere invalido "%s" em "%s"',[aHex[i], aHex]);
    end;

  if Odd(Length(digits)) then
    raise Exception.CreateFmt('BytesOf: numero impar de digitos hexa em "%s"',[aHex]);

  SetLength(Result, Length(digits) div 2);
  for i:=0 to High(Result) do
    Result[i]:=StrToInt('$'+Copy(digits, (i*2)+1, 2));
end;

function HexOf(const aBytes:BYTES):String;
var
  i:LongInt;
begin
  Result:='';
  for i:=0 to High(aBytes) do begin
    if Result<>'' then
      Result:=Result+' ';
    Result:=Result+IntToHex(aBytes[i],2);
  end;
end;

function BytesCat(const aLeft, aRight:BYTES):BYTES;
var
  i:LongInt;
begin
  SetLength(Result, Length(aLeft)+Length(aRight));
  for i:=0 to High(aLeft) do
    Result[i]:=aLeft[i];
  for i:=0 to High(aRight) do
    Result[Length(aLeft)+i]:=aRight[i];
end;

procedure AssertBytesEqual(const aMsg:String; const aExpected, aActual:BYTES);
var
  i:LongInt;
begin
  if Length(aExpected)<>Length(aActual) then
    TAssert.Fail(Format('%s: esperados %d bytes [%s], vieram %d bytes [%s]',
                        [aMsg, Length(aExpected), HexOf(aExpected),
                               Length(aActual),   HexOf(aActual)]));

  for i:=0 to High(aExpected) do
    if aExpected[i]<>aActual[i] then
      TAssert.Fail(Format('%s: difere no byte %d (esperado $%s, veio $%s) - esperado [%s], veio [%s]',
                          [aMsg, i, IntToHex(aExpected[i],2), IntToHex(aActual[i],2),
                                 HexOf(aExpected), HexOf(aActual)]));
end;

end.
