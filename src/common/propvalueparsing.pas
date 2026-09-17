{$i language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Reconhecimento do texto digitado nos editores de propriedades
  inteiras (com suporte a expressões) do Object Inspector.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Separado do editor de propriedades (que depende do IDEIntf, e por isso não
  pode ser testado nos testes automatizados de console) justamente para
  poder ser testado: aqui mora só a decisão "isto é um literal simples, uma
  expressão relativa (+/-*/) ou uma expressão de verdade?", sem nenhuma
  dependência de IDE.

  Existe por causa de um defeito real: o editor usava @code(TryStrToInt)
  (32 bits) só para decidir se o texto era um número simples, e nesse caso
  repassava para o @code(inherited SetValue) do Lazarus, cujo
  @code(TIntegerPropertyEditor.SetValue) grava o valor com
  @code(SetOrdValue(integer(L))) — trunca para 32 bits e, em propriedades de
  64 bits (como @code(TComponent.Tag), @code(PtrInt=Int64) em plataformas de
  64 bits), lê os limites errados do RTTI. Resultado: digitar um Tag comum no
  Windows podia ser recusado com "Value must be between 0 and 2147483647".
  Usando @code(TryStrToInt64) aqui, o valor nunca passa pelo caminho de 32
  bits do Lazarus.
}
{$ELSE}
{:
  @abstract(Recognizes the text typed into the integer property editors
  (with expression support) of the Object Inspector.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Split out of the property editor (which depends on IDEIntf, and so cannot
  be exercised by the console automated tests) precisely so it can be
  tested: this unit holds only the decision "is this a plain literal, a
  relative expression (+/-*/) or a real expression?", with no IDE
  dependency at all.

  It exists because of a real defect: the editor used @code(TryStrToInt)
  (32 bits) just to decide whether the text was a plain number, and in that
  case forwarded to Lazarus' @code(inherited SetValue), whose
  @code(TIntegerPropertyEditor.SetValue) stores the value with
  @code(SetOrdValue(integer(L))) — truncating to 32 bits and, on 64-bit
  properties (such as @code(TComponent.Tag), @code(PtrInt=Int64) on 64-bit
  platforms), reading the wrong RTTI bounds. Result: typing an ordinary Tag
  value on Windows could be refused with "Value must be between 0 and
  2147483647". Using @code(TryStrToInt64) here, the value never goes
  through Lazarus' 32-bit path.
}
{$ENDIF}
unit PropValueParsing;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

{$IFDEF PORTUGUES}
//: @name diz se @code(Value) começa com um operador (+ - * /), o que o torna
//: uma expressão relativa ao valor atual da propriedade, e não um literal.
{$ELSE}
//: @name tells whether @code(Value) starts with an operator (+ - * /),
//: which makes it an expression relative to the property's current value,
//: not a literal.
{$ENDIF}
function IsRelativeExpression(const Value: AnsiString): Boolean;

{$IFDEF PORTUGUES}
{:
  Tenta reconhecer @code(Value) como um literal inteiro simples (sem
  operador na frente). Se conseguir, devolve @true e o valor em
  @code(Resolved) — em Int64, largura suficiente para qualquer propriedade
  ordinal suportada pelo editor, incluindo as de 64 bits. Se @code(Value)
  for uma expressão relativa ou não for um número válido, devolve @false e
  @code(Resolved) fica em 0; nesses casos cabe ao chamador usar o avaliador
  de expressões completo.
}
{$ELSE}
{:
  Tries to recognize @code(Value) as a plain integer literal (no operator in
  front). When it succeeds, returns @true and the value in @code(Resolved)
  — as an Int64, wide enough for any ordinal property the editor supports,
  64-bit ones included. When @code(Value) is a relative expression or not a
  valid number, returns @false and @code(Resolved) is left at 0; the caller
  is then expected to fall back to the full expression evaluator.
}
{$ENDIF}
function TryParsePlainIntegerLiteral(const Value: AnsiString; out Resolved: Int64): Boolean;

implementation

function IsRelativeExpression(const Value: AnsiString): Boolean;
begin
  Result := (Length(Value) > 0) and (Value[1] in ['+', '-', '*', '/']);
end;

function TryParsePlainIntegerLiteral(const Value: AnsiString; out Resolved: Int64): Boolean;
begin
  Resolved := 0;
  Result := (not IsRelativeExpression(Value)) and TryStrToInt64(Value, Resolved);
end;

end.
