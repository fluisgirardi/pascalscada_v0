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

interface

uses SysUtils;

type
  TStringArray = array of String;

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
function ExplodeString(delimiter:string; str:string; limit:integer=MaxInt):TStringArray;

implementation

uses math;

function Power(Base:LongInt; Expoent:Cardinal):Cardinal;
var
  c:LongInt;
begin
  //don't use math.power and trunc together,
  //conversions from real to integer make it slower!
  Result := 1;
  while (Expoent<>0) do begin
    if (Expoent and 1)=1 then
      Result := Result * base;
    Expoent:=Expoent shr 1;
    Base := Base * Base;
  end;
end;

function ExplodeString(delimiter:string; str:string; limit:integer=MaxInt):TStringArray;
var
  p,cc,dsize:integer;
begin
  cc := 0;
  dsize := length(delimiter);
  if dsize = 0 then
  begin
    setlength(result,1);
    result[0] := str;
    exit;
  end;
  while cc+1 < limit do
  begin
    p := pos(delimiter,str);
    if p > 0 then
    begin
      inc(cc);
      setlength(result,cc);
      result[cc-1] := copy(str,1,p-1);
      delete(str,1,p+dsize-1);
    end else break;
  end;
  inc(cc);
  setlength(result,cc);
  result[cc-1] := str;
end;



end.
