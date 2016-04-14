{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit das funções CRC-16)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit that implement CRC-16 functions)
}
{$ENDIF}
unit crc16utils;

interface

uses commtypes;

{$IFDEF PORTUGUES}
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
{$ELSE}
{:
Verifies an package of BYTES ultil their length - 2 and returns @true
if the value calculated is the same on that is stored in the last 2 bytes
of the package.

If the Pkg has 10 bytes of length, it will calculate the CRC using the
first 8 bytes and it will check the result with bytes 9 and 10 of Pkg.
The byte number 9 is the most significative.

@param(Pkg BYTES. Buffer that stores the data that will be checked the CRC-16.
       Must have at least 2 bytes of length.)

@returns(@true if the CRC-16 calculated is the same that is stored on the last 2
         bytes of Pkg.)
}
{$ENDIF}
function Test_crc(const Pkg:BYTES):Boolean;

{$IFDEF PORTUGUES}
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
{$ELSE}
{:
Calculate the CRC-16 of the package until their size - 2. If the package has
10 bytes of length, it will use to calculate the first 8 bytes and will store
the result of the calculation on bytes 9 and 10. The byte number 9 is the most
significative.

@param(Pkg BYTES. Buffer that contains the data that will be used to calculate
       the CRC-16 and that will store the result on the last 2 bytes. Must have
       at least 2 bytes of length.)

@returns(A Cardinal number with the CRC-16 calculated with length of Pkg - 2.)
}
{$ENDIF}
function Calcul_crc(var Pkg:BYTES):Cardinal;

implementation

function Test_crc(const Pkg:BYTES):Boolean;
var
  crc,j,carry_flag,a:Cardinal;
  i,n:LongInt;
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
  i,n:LongInt;
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
 
