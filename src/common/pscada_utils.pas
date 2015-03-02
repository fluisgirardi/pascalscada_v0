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
unit pSCADA_utils;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

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
function BitToDec(Bit:Byte):QWord;

implementation

function BitToDec(Bit:Byte):QWord;
begin
  if not (byte in [0..63]) then 
    raise Exception.Create('Fora do range de 64 bits (0..63)');
  Result:=1 shl Bit;
end;

end.
