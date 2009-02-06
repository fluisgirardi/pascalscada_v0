unit crc16utils;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses commtypes;

function Test_crc(const Pkg:BYTES):Boolean;
function Calcul_crc(var Pkg:BYTES):Cardinal;

implementation

function Test_crc(const Pkg:BYTES):Boolean;
var
  crc,j,carry_flag,a:Cardinal;
  i,n:Integer;
begin
  n := Length(Pkg)-2;
  crc := $FFFF;
  i := 0;
  while (i<n) do begin
    crc := crc xor Cardinal(pkg[i]);
    for j:=0 to 7 do begin
      a := crc;
      carry_flag := a and $0001;
      crc := crc shr 1;
      if (carry_flag=1) then
        crc := crc xor $A001;
    end;
    inc(i);
  end;
  
  Result := ((n+2)<=Length(Pkg)) and ((Cardinal(pkg[n+1])=(crc shr 8)) or (Cardinal(pkg[n])=(crc and 255)));

end;

function Calcul_crc(var Pkg:BYTES):Cardinal;
var
  crc,j,carry_flag,a:Cardinal;
  i,n:Integer;  
begin
  n:=Length(Pkg)-2;
  crc := $FFFF;
  i := 0;
  while (i<n) do begin
    crc :=crc xor Cardinal(pkg[i]);
    for j:=0 to 7 do begin
      a := crc;
      carry_flag := a and $0001;
      crc := crc shr 1;
      if (carry_flag=1) then
        crc := crc xor $A001;
    end;
    inc(i);
  end;
  pkg[n+1] := ((crc and $FF00) shr 8);
  pkg[n]   := (crc and 255);
  result := crc;
end;


end.
 
