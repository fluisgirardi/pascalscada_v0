{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  Implementa funções uteis a todo o sistema.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  Miscelaneous functions.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit hsutils;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses SysUtils;

{$IFDEF PORTUGUES}
{:
  Realiza o cálculo de exponenciação.
  @returns(O resultado da exponenciação em Cardinal.)
}
{$ELSE}
{:
  Computes the exponentiation.
  @returns(Cardinal. Returns Base^Expoent calculation.)
}
{$ENDIF}
function Power(Base:LongInt; Expoent:Cardinal):Cardinal;

implementation

function Power(Base:LongInt; Expoent:Cardinal):Cardinal;
var
  c:LongInt;
begin
  if Expoent=0 then begin
    Result:=1;
    exit;
  end;
  Result:=Base;
  for c:=2 to Expoent do
    Result := Result*Base;
end;

end.
