unit ut.propvalueparsing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PropValueParsing;

type

  { TTestPropValueParsing }

  { Reconhecimento do texto do editor de propriedades inteiras (com suporte
    a expressoes) do Object Inspector - o defeito real era um Tag comum (ex.:
    3000000000, valido num PtrInt=Int64 de 64 bits) sendo recusado no Windows
    com "Value must be between 0 and 2147483647", porque o editor só sabia
    testar "isto e' um literal simples?" com TryStrToInt (32 bits): falhando
    esse teste, o valor caia no caminho do "inherited SetValue" do Lazarus,
    cujo TIntegerPropertyEditor grava com SetOrdValue(integer(L)) - trunca
    para 32 bits e le os limites errados de uma propriedade de 64 bits. }
  TTestPropValueParsing = class(TTestCase)
  published
    procedure APlainSmallNumberIsRecognized;
    procedure ANumberBeyond32BitsIsStillRecognized;
    procedure ANumberNearInt64MaxIsRecognized;
    procedure AnEmptyStringIsNotALiteral;
    procedure AnAlgebraicExpressionIsNotALiteral;
    procedure PlusIsARelativeExpressionNotALiteral;
    procedure MinusIsARelativeExpressionNotALiteral;
    procedure TimesAndDivideAreRelativeExpressions;
    procedure APlainNumberIsNotARelativeExpression;
    procedure AnEmptyStringIsNotARelativeExpression;
  end;

implementation

{ O defeito em si: um valor de Tag comum, maior que High(Longint)
  (2147483647), tem que ser reconhecido como literal simples e chegar
  inteiro - e' exatamente o caso que TryStrToInt (32 bits) recusava. }
procedure TTestPropValueParsing.ANumberBeyond32BitsIsStillRecognized;
var
  resolved: Int64;
begin
  AssertTrue('"3000000000" must be a plain literal, not an expression',
             TryParsePlainIntegerLiteral('3000000000', resolved));
  AssertEquals('the Int64 value must come through whole', Int64(3000000000), resolved);
end;

procedure TTestPropValueParsing.ANumberNearInt64MaxIsRecognized;
var
  resolved: Int64;
begin
  AssertTrue('a value near High(Int64) must still be a plain literal',
             TryParsePlainIntegerLiteral('9223372036854775807', resolved));
  AssertEquals('the Int64 value must come through whole', High(Int64), resolved);
end;

procedure TTestPropValueParsing.APlainSmallNumberIsRecognized;
var
  resolved: Int64;
begin
  AssertTrue('"42" must be a plain literal', TryParsePlainIntegerLiteral('42', resolved));
  AssertEquals(Int64(42), resolved);
end;

procedure TTestPropValueParsing.AnEmptyStringIsNotALiteral;
var
  resolved: Int64;
begin
  { NewValue[1] numa string vazia nao pode derrubar o editor: tem que
    devolver False sem levantar excecao. }
  AssertFalse('an empty string is not a literal', TryParsePlainIntegerLiteral('', resolved));
  AssertEquals('Resolved must stay at 0 on failure', Int64(0), resolved);
end;

procedure TTestPropValueParsing.AnAlgebraicExpressionIsNotALiteral;
var
  resolved: Int64;
begin
  AssertFalse('"A+B" is an expression, not a literal', TryParsePlainIntegerLiteral('A+B', resolved));
end;

procedure TTestPropValueParsing.PlusIsARelativeExpressionNotALiteral;
var
  resolved: Int64;
begin
  { Documentado: "+2" soma ao valor atual da propriedade, nao grava 2. }
  AssertFalse('"+2" is the relative-expression form, not a literal',
              TryParsePlainIntegerLiteral('+2', resolved));
end;

procedure TTestPropValueParsing.MinusIsARelativeExpressionNotALiteral;
var
  resolved: Int64;
begin
  { Documentado: "-5" subtrai do valor atual, nao grava -5 - e' por isso
    que um Tag negativo se digita como expressao (ex.: "0-5"), nao "-5". }
  AssertFalse('"-5" is the relative-expression form, not a literal',
              TryParsePlainIntegerLiteral('-5', resolved));
end;

procedure TTestPropValueParsing.TimesAndDivideAreRelativeExpressions;
begin
  AssertTrue('*2', IsRelativeExpression('*2'));
  AssertTrue('/2', IsRelativeExpression('/2'));
end;

procedure TTestPropValueParsing.APlainNumberIsNotARelativeExpression;
begin
  AssertFalse('"42" has no leading operator', IsRelativeExpression('42'));
end;

procedure TTestPropValueParsing.AnEmptyStringIsNotARelativeExpression;
begin
  AssertFalse('an empty string has no leading operator', IsRelativeExpression(''));
end;

initialization
  RegisterTest(TTestPropValueParsing);

end.
