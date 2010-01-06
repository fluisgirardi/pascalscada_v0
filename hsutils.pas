{:
  Implementa funções uteis a todo o sistema.
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit hsutils;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses SysUtils;
{:
  Converte um valor Float para Int64, descartando a parte decimal.
  @returns(O valor convertido para Int64.)
}
function FloatToInt64(Value:Extended):Int64;
{:
  Realiza o cálculo de exponenciação.
  @returns(O resultado da exponenciação em Cardinal.)
}
function Power(Base, Expoent:Integer):Cardinal;
{:
  Converte um valor Float para Integer, descartando a parte decimal.
  @returns(O valor convertido para Integer.)
}
function FloatToInteger(Value:Extended):Integer;

implementation

function Power(Base, Expoent:Integer):Cardinal;
var
  c:Integer;
begin
  if Expoent=0 then begin
    Result:=1;
    exit;
  end;
  Result:=Base;
  for c:=2 to Expoent do
    Result := Result*Base;
end;

function FloatToInteger(Value:Extended):Integer;
var
  c, pos, t:Integer;
  x:TFloatRec;
begin
  c:=0;
  result := 0;
  {$IFDEF FPC}
  FloatToDecimal(x,Value,10,0);
  {$ELSE}
  FloatToDecimal(x,Value,fvExtended,10,0);
  {$ENDIF}
  while (c<10) and (x.Digits[c]<>#0) do
    inc(c);
  t := c; //armazena o tamanho
  dec(c);
  pos := c;
  while ((c>=0) and (c<10)) and ((x.Digits[c]>='0') and (x.Digits[c]<='9')) do begin
    Result := Result + (Byte(x.Digits[c])-48) * power(10,pos - c);
    Dec(c);
  end;
  Result := Result * Power(10,x.Exponent-t);
  if x.Negative then
    Result := Result * (-1);
end;

function FloatToInt64(Value:Extended):Int64;
var
  c, pos, t:Integer;
  x:TFloatRec;
begin
  c:=0;
  result := 0;
  {$IFDEF FPC}
  FloatToDecimal(x,Value,19,0);
  {$ELSE}
  FloatToDecimal(x,Value,fvExtended,19,0);
  {$ENDIF}
  while (c<19) and (x.Digits[c]<>#0) do
    inc(c);
  t := c; //armazena o tamanho
  dec(c);
  pos := c;
  while ((c>=0) and (c<19)) and ((x.Digits[c]>='0') and (x.Digits[c]<='9')) do begin
    Result := Result + (Byte(x.Digits[c])-48) * power(10,pos - c);
    Dec(c);
  end;
  Result := Result * Power(10,x.Exponent-t);
  if x.Negative then
    Result := Result * (-1);
end;

end.
