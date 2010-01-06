{:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  @abstract(Unit das funções CRC de 16 bits)
}
unit crc16utils;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses commtypes;

{:
Testa um pacote de BYTES até o tamanho de Pkg - 2
e retorna @true caso o calculo retorne o mesmo resultado
com o que deve estar armazenado nos últimos dois bytes
do buffer.

Caso o pacote tenha 10 bytes, ele irá calcular com os
oito primeiros bytes e vai verificar se o calculo resultante
é igual aos bytes 9 e 10 de Pkg. Byte 9 é o mais significativo.

@param(Pkg BYTES. Buffer que contem os dados que irá ser verificado
       o CRC16. Deve ter pelo menos 2 bytes de tamanho.)

@returns(@true caso o pacote feche o CRC16 com o CRC16 que
         está armazenado nos últimos dois bytes.)
}
function Test_crc(const Pkg:BYTES):Boolean;

{:
Calcula o CRC-16 do pacote até seu tamanho - 2.
Caso um pacote tenha 10 bytes, ele irá pegar para
o calculo os 8 primeiros e irá armazenar nos bytes
9 e 10 o calculo resultante. Byte 9 é o mais significativo.

@param(Pkg BYTES. Buffer que contem os dados que irá ser calculado
       o CRC16. Deve ter pelo menos 2 bytes de tamanho.)

@returns(Uma Cardinal com o valor CRC16 do calculo feito com
         o tamanho de pkg - 2)
}
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
 
