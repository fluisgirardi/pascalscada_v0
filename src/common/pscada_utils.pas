{$i ../common/pscada_settings.inc}
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

uses SysUtils, pSCADA_Types;

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

{$IFDEF PORTUGUES}
{:
Concatena dois buffers de bytes.
@seealso(BYTES)
}
{$ELSE}
{:
Concatenate two @noAutoLink(bytes) buffers.
@seealso(BYTES)
}
{$ENDIF}
function ConcatenateBYTES(const a,b:BYTES):BYTES;

implementation

function BitToDec(Bit:Byte):QWord;
begin
  if not (Bit in [0..63]) then
    raise Exception.Create('Fora do range de 64 bits (0..63)');
  Result:=1 shl Bit;
end;

//concatenate two buffers of bytes.
function ConcatenateBYTES(const a,b:BYTES):BYTES;
var
c:LongInt;
begin
SetLength(Result,Length(a)+Length(b));
for c:=0 to High(a) do
  Result[c]:=a[c];
for c:=0 to High(b) do
  Result[c+Length(a)]:=b[c];
end;

end.
