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
function Calculate_CRC8(var data:BYTES):Byte;
function Fast_CRC_Cal8Bits(data:BYTES):Byte;

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
  
  //CRC must matches the two bytes, so should be a AND, not a OR
  Result := ((n+2)<=Length(Pkg)) and ((Cardinal(pkg[n+1])=(crc shr 8)) AND (Cardinal(pkg[n])=(crc and 255)));

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

function Calculate_CRC8(var data:BYTES):Byte;
var
  crc:Byte;
  i, j: Integer;
begin
  crc := $ff;
  for i := 0 to high(data) do begin
    crc := crc xor data[i];
    for j := 0 to 7 do begin
      if ((crc and $80) <> 0) then
        crc := byte(((Word(crc) shl 1) xor $31))
      else
        crc := byte(Word(crc) shl 1);
    end;
  end;
  exit(crc);
end;

function Fast_CRC_Cal8Bits(data:BYTES):Byte;
const
  CrcTable:array[0..255] of byte = ( // 0x97 Polynomial Table, 8-bit, sourcer32@gmail.com
                                    $00,$97,$B9,$2E,$E5,$72,$5C,$CB,
                                    $5D,$CA,$E4,$73,$B8,$2F,$01,$96,
                                    $BA,$2D,$03,$94,$5F,$C8,$E6,$71,
                                    $E7,$70,$5E,$C9,$02,$95,$BB,$2C,
                                    $E3,$74,$5A,$CD,$06,$91,$BF,$28,
                                    $BE,$29,$07,$90,$5B,$CC,$E2,$75,
                                    $59,$CE,$E0,$77,$BC,$2B,$05,$92,
                                    $04,$93,$BD,$2A,$E1,$76,$58,$CF,
                                    $51,$C6,$E8,$7F,$B4,$23,$0D,$9A,
                                    $0C,$9B,$B5,$22,$E9,$7E,$50,$C7,
                                    $EB,$7C,$52,$C5,$0E,$99,$B7,$20,
                                    $B6,$21,$0F,$98,$53,$C4,$EA,$7D,
                                    $B2,$25,$0B,$9C,$57,$C0,$EE,$79,
                                    $EF,$78,$56,$C1,$0A,$9D,$B3,$24,
                                    $08,$9F,$B1,$26,$ED,$7A,$54,$C3,
                                    $55,$C2,$EC,$7B,$B0,$27,$09,$9E,
                                    $A2,$35,$1B,$8C,$47,$D0,$FE,$69,
                                    $FF,$68,$46,$D1,$1A,$8D,$A3,$34,
                                    $18,$8F,$A1,$36,$FD,$6A,$44,$D3,
                                    $45,$D2,$FC,$6B,$A0,$37,$19,$8E,
                                    $41,$D6,$F8,$6F,$A4,$33,$1D,$8A,
                                    $1C,$8B,$A5,$32,$F9,$6E,$40,$D7,
                                    $FB,$6C,$42,$D5,$1E,$89,$A7,$30,
                                    $A6,$31,$1F,$88,$43,$D4,$FA,$6D,
                                    $F3,$64,$4A,$DD,$16,$81,$AF,$38,
                                    $AE,$39,$17,$80,$4B,$DC,$F2,$65,
                                    $49,$DE,$F0,$67,$AC,$3B,$15,$82,
                                    $14,$83,$AD,$3A,$F1,$66,$48,$DF,
                                    $10,$87,$A9,$3E,$F5,$62,$4C,$DB,
                                    $4D,$DA,$F4,$63,$A8,$3F,$11,$86,
                                    $AA,$3D,$13,$84,$4F,$D8,$F6,$61,
                                    $F7,$60,$4E,$D9,$12,$85,$AB,$3C
                                   );
var
  i: Integer;
  crc:Byte;
begin
  crc:=0;
  for i:=0 to High(data) do begin
    crc := crc xor data[i]; // Apply Byte
    crc := CrcTable[crc and $FF]; // One round of 8-bits
  end;

  exit(crc);
end;

end.
 
