{:
  Implementa funções uteis a todo o sistema.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
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
begin
  Result := trunc(Value);
end;

function FloatToInt64(Value:Extended):Int64;
begin
  Result := trunc(Value);
end;

end.
