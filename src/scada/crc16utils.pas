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

type
  TCRC8_Table = array[0..255] of Byte;

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
function Fast_CRC_Cal8Bits(data:BYTES; const Init:Byte):Byte;
function CRC8_DVBS2(const data:BYTES; const Init:Byte): Byte;
function CRC8_Default(const data:BYTES; const Init:Byte): Byte;
function CRC8_Maxim(const data:BYTES; const Init:Byte): Byte;
function CRC8_ITU(const data:BYTES; const Init:Byte): Byte;

function CRC8_DARC(const data: BYTES; const Init: Byte): Byte;
function CRC8_BLUETOOTH(const data: BYTES; const Init: Byte): Byte;
function CRC8_CDMA2000(const data: BYTES; const Init: Byte): Byte;
function CRC8_ROHC(const data: BYTES; const Init: Byte): Byte;
function CRC8_WCDMA(const data: BYTES; const Init: Byte): Byte;
function CRC8_OPENSAFETY(const data: BYTES; const Init: Byte): Byte;
function CRC8_AUTOSAR(const data: BYTES; const Init: Byte): Byte;
function CRC8_ICODE(const Data: Bytes; const Init: Byte): Byte;

procedure CRC8_GenerateTable(var Table: TCRC8_Table;
                             Poly: Byte;
                             RefIn: Boolean); inline;

function CRC8_Calc(const Data: BYTES;
                   const Table: TCRC8_Table;
                   Init: Byte;
                   RefOut: Boolean;
                   XorOut: Byte): Byte; inline;

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

function Fast_CRC_Cal8Bits(data: BYTES; const Init: Byte): Byte;
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
  crc:=Init;
  for i:=0 to High(data) do begin
    crc := crc xor data[i]; // Apply Byte
    crc := CrcTable[crc and $FF]; // One round of 8-bits
  end;

  exit(crc);
end;

function CRC8_DVBS2(const data: BYTES; const Init: Byte): Byte;
var
  i: Integer;
  crc: Byte;
const
  CRC8_DVBS2_Table: array[0..255] of Byte = (
    $00,$D5,$7F,$AA,$FE,$2B,$81,$54,$29,$FC,$56,$83,$D7,$02,$A8,$7D,
    $52,$87,$2D,$F8,$AC,$79,$D3,$06,$7B,$AE,$04,$D1,$85,$50,$FA,$2F,
    $A4,$71,$DB,$0E,$5A,$8F,$25,$F0,$8D,$58,$F2,$27,$73,$A6,$0C,$D9,
    $F6,$23,$89,$5C,$08,$DD,$77,$A2,$DF,$0A,$A0,$75,$21,$F4,$5E,$8B,
    $9D,$48,$E2,$37,$63,$B6,$1C,$C9,$B4,$61,$CB,$1E,$4A,$9F,$35,$E0,
    $CF,$1A,$B0,$65,$31,$E4,$4E,$9B,$E6,$33,$99,$4C,$18,$CD,$67,$B2,
    $39,$EC,$46,$93,$C7,$12,$B8,$6D,$10,$C5,$6F,$BA,$EE,$3B,$91,$44,
    $6B,$BE,$14,$C1,$95,$40,$EA,$3F,$42,$97,$3D,$E8,$BC,$69,$C3,$16,
    $EF,$3A,$90,$45,$11,$C4,$6E,$BB,$C6,$13,$B9,$6C,$38,$ED,$47,$92,
    $BD,$68,$C2,$17,$43,$96,$3C,$E9,$94,$41,$EB,$3E,$6A,$BF,$15,$C0,
    $4B,$9E,$34,$E1,$B5,$60,$CA,$1F,$62,$B7,$1D,$C8,$9C,$49,$E3,$36,
    $19,$CC,$66,$B3,$E7,$32,$98,$4D,$30,$E5,$4F,$9A,$CE,$1B,$B1,$64,
    $72,$A7,$0D,$D8,$8C,$59,$F3,$26,$5B,$8E,$24,$F1,$A5,$70,$DA,$0F,
    $20,$F5,$5F,$8A,$DE,$0B,$A1,$74,$09,$DC,$76,$A3,$F7,$22,$88,$5D,
    $D6,$03,$A9,$7C,$28,$FD,$57,$82,$FF,$2A,$80,$55,$01,$D4,$7E,$AB,
    $84,$51,$FB,$2E,$7A,$AF,$05,$D0,$AD,$78,$D2,$07,$53,$86,$2C,$F9
  );

begin
  crc := Init;  // init = 0

  for i := 0 to High(data) do
    crc := CRC8_DVBS2_Table[crc xor data[i]];

  Result := crc;  // xorout = 0
end;

function CRC8_Default(const data: BYTES; const Init: Byte): Byte;
var
  i: Integer;
  crc: Byte;

const
  CRC8_Table: array[0..255] of Byte = (
    $00,$07,$0E,$09,$1C,$1B,$12,$15,$38,$3F,$36,$31,$24,$23,$2A,$2D,
    $70,$77,$7E,$79,$6C,$6B,$62,$65,$48,$4F,$46,$41,$54,$53,$5A,$5D,
    $E0,$E7,$EE,$E9,$FC,$FB,$F2,$F5,$D8,$DF,$D6,$D1,$C4,$C3,$CA,$CD,
    $90,$97,$9E,$99,$8C,$8B,$82,$85,$A8,$AF,$A6,$A1,$B4,$B3,$BA,$BD,
    $C7,$C0,$C9,$CE,$DB,$DC,$D5,$D2,$FF,$F8,$F1,$F6,$E3,$E4,$ED,$EA,
    $B7,$B0,$B9,$BE,$AB,$AC,$A5,$A2,$8F,$88,$81,$86,$93,$94,$9D,$9A,
    $27,$20,$29,$2E,$3B,$3C,$35,$32,$1F,$18,$11,$16,$03,$04,$0D,$0A,
    $57,$50,$59,$5E,$4B,$4C,$45,$42,$6F,$68,$61,$66,$73,$74,$7D,$7A,
    $89,$8E,$87,$80,$95,$92,$9B,$9C,$B1,$B6,$BF,$B8,$AD,$AA,$A3,$A4,
    $F9,$FE,$F7,$F0,$E5,$E2,$EB,$EC,$C1,$C6,$CF,$C8,$DD,$DA,$D3,$D4,
    $69,$6E,$67,$60,$75,$72,$7B,$7C,$51,$56,$5F,$58,$4D,$4A,$43,$44,
    $19,$1E,$17,$10,$05,$02,$0B,$0C,$21,$26,$2F,$28,$3D,$3A,$33,$34,
    $4E,$49,$40,$47,$52,$55,$5C,$5B,$76,$71,$78,$7F,$6A,$6D,$64,$63,
    $3E,$39,$30,$37,$22,$25,$2C,$2B,$06,$01,$08,$0F,$1A,$1D,$14,$13,
    $AE,$A9,$A0,$A7,$B2,$B5,$BC,$BB,$96,$91,$98,$9F,$8A,$8D,$84,$83,
    $DE,$D9,$D0,$D7,$C2,$C5,$CC,$CB,$E6,$E1,$E8,$EF,$FA,$FD,$F4,$F3
  );

begin
  crc := Init;
  for i := 0 to High(data) do
    crc := CRC8_Table[crc xor data[i]];
  Result := crc;
end;

function CRC8_Maxim(const data: BYTES; const Init: Byte): Byte;
var
  i: Integer;
  crc: Byte;

const
  CRC8_Maxim_Table: array[0..255] of Byte = (
    $00,$31,$62,$53,$C4,$F5,$A6,$97,$B9,$88,$DB,$EA,$7D,$4C,$1F,$2E,
    $43,$72,$21,$10,$87,$B6,$E5,$D4,$FA,$CB,$98,$A9,$3E,$0F,$5C,$6D,
    $86,$B7,$E4,$D5,$42,$73,$20,$11,$3F,$0E,$5D,$6C,$FB,$CA,$99,$A8,
    $C5,$F4,$A7,$96,$01,$30,$63,$52,$7C,$4D,$1E,$2F,$B8,$89,$DA,$EB,
    $3D,$0C,$5F,$6E,$F9,$C8,$9B,$AA,$84,$B5,$E6,$D7,$40,$71,$22,$13,
    $7E,$4F,$1C,$2D,$BA,$8B,$D8,$E9,$C7,$F6,$A5,$94,$03,$32,$61,$50,
    $BB,$8A,$D9,$E8,$7F,$4E,$1D,$2C,$02,$33,$60,$51,$C6,$F7,$A4,$95,
    $F8,$C9,$9A,$AB,$3C,$0D,$5E,$6F,$41,$70,$23,$12,$85,$B4,$E7,$D6,
    $7A,$4B,$18,$29,$BE,$8F,$DC,$ED,$C3,$F2,$A1,$90,$07,$36,$65,$54,
    $39,$08,$5B,$6A,$FD,$CC,$9F,$AE,$80,$B1,$E2,$D3,$44,$75,$26,$17,
    $FC,$CD,$9E,$AF,$38,$09,$5A,$6B,$45,$74,$27,$16,$81,$B0,$E3,$D2,
    $BF,$8E,$DD,$EC,$7B,$4A,$19,$28,$06,$37,$64,$55,$C2,$F3,$A0,$91,
    $47,$76,$25,$14,$83,$B2,$E1,$D0,$FE,$CF,$9C,$AD,$3A,$0B,$58,$69,
    $04,$35,$66,$57,$C0,$F1,$A2,$93,$BD,$8C,$DF,$EE,$79,$48,$1B,$2A,
    $C1,$F0,$A3,$92,$05,$34,$67,$56,$78,$49,$1A,$2B,$BC,$8D,$DE,$EF,
    $82,$B3,$E0,$D1,$46,$77,$24,$15,$3B,$0A,$59,$68,$FF,$CE,$9D,$AC
  );

begin
  crc := Init;
  for i := 0 to High(data) do
    crc := CRC8_Maxim_Table[crc xor data[i]];
  Result := crc;
end;

function CRC8_ITU(const data: BYTES; const Init: Byte): Byte;
var
  crc, i, j: Byte;
begin
  crc := Init;

  for i := 0 to High(data) do
  begin
    crc := crc xor data[i];
    for j := 0 to 7 do
      if (crc and $80) <> 0 then
        crc := (crc shl 1) xor $07
      else
        crc := crc shl 1;
  end;

  Result := crc xor $55; // xorout
end;

(*
 ============================================================================
    CRC-8 / DARC
    Poly=0x39  Init=0x00  RefIn/RefOut=TRUE  XorOut=0x00
 ============================================================================
*)
const CRC8_DARC_Table: array[0..255] of Byte = (
  $00,$39,$72,$4B,$E4,$DD,$96,$AF,$D3,$EA,$A1,$98,$37,$0E,$45,$7C,
  $AB,$92,$D9,$E0,$4F,$76,$3D,$04,$78,$41,$0A,$33,$9C,$A5,$EE,$D7,
  $9A,$A3,$E8,$D1,$7E,$47,$0C,$35,$49,$70,$3B,$02,$AD,$94,$DF,$E6,
  $31,$08,$43,$7A,$D5,$EC,$A7,$9E,$E2,$DB,$90,$A9,$06,$3F,$74,$4D,
  $65,$5C,$17,$2E,$81,$B8,$F3,$CA,$B6,$8F,$C4,$FD,$52,$6B,$20,$19,
  $CE,$F7,$BC,$85,$2A,$13,$58,$61,$1D,$24,$6F,$56,$F9,$C0,$8B,$B2,
  $FF,$C6,$8D,$B4,$1B,$22,$69,$50,$2C,$15,$5E,$67,$C8,$F1,$BA,$83,
  $54,$6D,$26,$1F,$B0,$89,$C2,$FB,$87,$BE,$F5,$CC,$63,$5A,$11,$28,
  $CB,$F2,$B9,$80,$2F,$16,$5D,$64,$18,$21,$6A,$53,$FC,$C5,$8E,$B7,
  $60,$59,$12,$2B,$84,$BD,$F6,$CF,$B3,$8A,$C1,$F8,$57,$6E,$25,$1C,
  $51,$68,$23,$1A,$B5,$8C,$C7,$FE,$82,$BB,$F0,$C9,$66,$5F,$14,$2D,
  $FA,$C3,$88,$B1,$1E,$27,$6C,$55,$29,$10,$5B,$62,$CD,$F4,$BF,$86,
  $AE,$97,$DC,$E5,$4A,$73,$38,$01,$7D,$44,$0F,$36,$99,$A0,$EB,$D2,
  $05,$3C,$77,$4E,$E1,$D8,$93,$AA,$D6,$EF,$A4,$9D,$32,$0B,$40,$79,
  $34,$0D,$46,$7F,$D0,$E9,$A2,$9B,$E7,$DE,$95,$AC,$03,$3A,$71,$48,
  $9F,$A6,$ED,$D4,$7B,$42,$09,$30,$4C,$75,$3E,$07,$A8,$91,$DA,$E3
);

function CRC8_DARC(const data: BYTES; const Init: Byte): Byte;
var i: Integer; crc: Byte;
begin
  crc := Init;
  for i := 0 to High(data) do
    crc := CRC8_DARC_Table[crc xor data[i]];
  Result := crc;
end;

(*
 ============================================================================
    CRC-8 / BLUETOOTH
    Poly=0xA7  Init=0x00  RefIn/RefOut=TRUE  XorOut=0x00
 ============================================================================
*)
const CRC8_BLUETOOTH_Table: array[0..255] of Byte = (
  $00,$A7,$5D,$FA,$BA,$1D,$E7,$40,$63,$C4,$3E,$99,$D9,$7E,$84,$23,
  $C6,$61,$9B,$3C,$7C,$DB,$21,$86,$A5,$02,$F8,$5F,$1F,$B8,$42,$E5,
  $3B,$9C,$66,$C1,$81,$26,$DC,$7B,$58,$FF,$05,$A2,$E2,$45,$BF,$18,
  $FD,$5A,$A0,$07,$47,$E0,$1A,$BD,$9E,$39,$C3,$64,$24,$83,$79,$DE,
  $76,$D1,$2B,$8C,$CC,$6B,$91,$36,$15,$B2,$48,$EF,$AF,$08,$F2,$55,
  $B0,$17,$ED,$4A,$0A,$AD,$57,$F0,$D3,$74,$8E,$29,$69,$CE,$34,$93,
  $4D,$EA,$10,$B7,$F7,$50,$AA,$0D,$2E,$89,$73,$D4,$94,$33,$C9,$6E,
  $8B,$2C,$D6,$71,$31,$96,$6C,$CB,$E8,$4F,$B5,$12,$52,$F5,$0F,$A8,
  $EC,$4B,$B1,$16,$56,$F1,$0B,$AC,$8F,$28,$D2,$75,$35,$92,$68,$CF,
  $2A,$8D,$77,$D0,$90,$37,$CD,$6A,$49,$EE,$14,$B3,$F3,$54,$AE,$09,
  $D7,$70,$8A,$2D,$6D,$CA,$30,$97,$B4,$13,$E9,$4E,$0E,$A9,$53,$F4,
  $11,$B6,$4C,$EB,$AB,$0C,$F6,$51,$72,$D5,$2F,$88,$C8,$6F,$95,$32,
  $9A,$3D,$C7,$60,$20,$87,$7D,$DA,$F9,$5E,$A4,$03,$43,$E4,$1E,$B9,
  $5C,$FB,$01,$A6,$E6,$41,$BB,$1C,$3F,$98,$62,$C5,$85,$22,$D8,$7F,
  $A1,$06,$FC,$5B,$1B,$BC,$46,$E1,$C2,$65,$9F,$38,$78,$DF,$25,$82,
  $67,$C0,$3A,$9D,$DD,$7A,$80,$27,$04,$A3,$59,$FE,$BE,$19,$E3,$44
);

function CRC8_BLUETOOTH(const data: BYTES; const Init: Byte): Byte;
var i: Integer; crc: Byte;
begin
  crc := Init;
  for i := 0 to High(data) do
    crc := CRC8_BLUETOOTH_Table[crc xor data[i]];
  Result := crc;
end;

(*
 ============================================================================
    CRC-8 / CDMA2000
    Poly=0x9B  Init=0xFF  RefIn/RefOut=FALSE  XorOut=0x00
 ============================================================================
*)
const CRC8_CDMA2000_Table: array[0..255] of Byte = (
  $00,$9B,$AD,$36,$C1,$5A,$6C,$F7,$19,$82,$B4,$2F,$D8,$43,$75,$EE,
  $32,$A9,$9F,$04,$F3,$68,$5E,$C5,$2B,$B0,$86,$1D,$EA,$71,$47,$DC,
  $64,$FF,$C9,$52,$A5,$3E,$08,$93,$7D,$E6,$D0,$4B,$BC,$27,$11,$8A,
  $56,$CD,$FB,$60,$97,$0C,$3A,$A1,$4F,$D4,$E2,$79,$8E,$15,$23,$B8,
  $C8,$53,$65,$FE,$09,$92,$A4,$3F,$D1,$4A,$7C,$E7,$10,$8B,$BD,$26,
  $FA,$61,$57,$CC,$3B,$A0,$96,$0D,$E3,$78,$4E,$D5,$22,$B9,$8F,$14,
  $AC,$37,$01,$9A,$6D,$F6,$C0,$5B,$B5,$2E,$18,$83,$74,$EF,$D9,$42,
  $9E,$05,$33,$A8,$5F,$C4,$F2,$69,$87,$1C,$2A,$B1,$46,$DD,$EB,$70,
  $8D,$16,$20,$BB,$4C,$D7,$E1,$7A,$94,$0F,$39,$A2,$55,$CE,$F8,$63,
  $BF,$24,$12,$89,$7E,$E5,$D3,$48,$A6,$3D,$0B,$90,$67,$FC,$CA,$51,
  $E9,$72,$44,$DF,$28,$B3,$85,$1E,$F0,$6B,$5D,$C6,$31,$AA,$9C,$07,
  $DB,$40,$76,$ED,$1A,$81,$B7,$2C,$C2,$59,$6F,$F4,$03,$98,$AE,$35,
  $45,$DE,$E8,$73,$84,$1F,$29,$B2,$5C,$C7,$F1,$6A,$9D,$06,$30,$AB,
  $77,$EC,$DA,$41,$B6,$2D,$1B,$80,$6E,$F5,$C3,$58,$AF,$34,$02,$99,
  $21,$BA,$8C,$17,$E0,$7B,$4D,$D6,$38,$A3,$95,$0E,$F9,$62,$54,$CF,
  $13,$88,$BE,$25,$D2,$49,$7F,$E4,$0A,$91,$A7,$3C,$CB,$50,$66,$FD
);

function CRC8_CDMA2000(const data: BYTES; const Init: Byte): Byte;
var i: Integer; crc: Byte;
begin
  crc := Init;
  for i := 0 to High(data) do
    crc := CRC8_CDMA2000_Table[crc xor data[i]];
  Result := crc;
end;

(*
 ============================================================================
    CRC-8 / ROHC
    Poly=0x07  Init=0xFF  RefIn/RefOut=TRUE  XorOut=0x00
 ============================================================================
*)
const CRC8_ROHC_Table: array[0..255] of Byte = (
  $00,$07,$0E,$09,$1C,$1B,$12,$15,$38,$3F,$36,$31,$24,$23,$2A,$2D,
  $70,$77,$7E,$79,$6C,$6B,$62,$65,$48,$4F,$46,$41,$54,$53,$5A,$5D,
  $E0,$E7,$EE,$E9,$FC,$FB,$F2,$F5,$D8,$DF,$D6,$D1,$C4,$C3,$CA,$CD,
  $90,$97,$9E,$99,$8C,$8B,$82,$85,$A8,$AF,$A6,$A1,$B4,$B3,$BA,$BD,
  $C7,$C0,$C9,$CE,$DB,$DC,$D5,$D2,$FF,$F8,$F1,$F6,$E3,$E4,$ED,$EA,
  $B7,$B0,$B9,$BE,$AB,$AC,$A5,$A2,$8F,$88,$81,$86,$93,$94,$9D,$9A,
  $27,$20,$29,$2E,$3B,$3C,$35,$32,$1F,$18,$11,$16,$03,$04,$0D,$0A,
  $57,$50,$59,$5E,$4B,$4C,$45,$42,$6F,$68,$61,$66,$73,$74,$7D,$7A,
  $89,$8E,$87,$80,$95,$92,$9B,$9C,$B1,$B6,$BF,$B8,$AD,$AA,$A3,$A4,
  $F9,$FE,$F7,$F0,$E5,$E2,$EB,$EC,$C1,$C6,$CF,$C8,$DD,$DA,$D3,$D4,
  $69,$6E,$67,$60,$75,$72,$7B,$7C,$51,$56,$5F,$58,$4D,$4A,$43,$44,
  $19,$1E,$17,$10,$05,$02,$0B,$0C,$21,$26,$2F,$28,$3D,$3A,$33,$34,
  $4E,$49,$40,$47,$52,$55,$5C,$5B,$76,$71,$78,$7F,$6A,$6D,$64,$63,
  $3E,$39,$30,$37,$22,$25,$2C,$2B,$06,$01,$08,$0F,$1A,$1D,$14,$13,
  $AE,$A9,$A0,$A7,$B2,$B5,$BC,$BB,$96,$91,$98,$9F,$8A,$8D,$84,$83,
  $DE,$D9,$D0,$D7,$C2,$C1,$C8,$CF,$E2,$E1,$E8,$EF,$FA,$FD,$F4,$F3
);

function CRC8_ROHC(const data: BYTES; const Init: Byte): Byte;
var i: Integer; crc: Byte;
begin
  crc := Init;
  for i := 0 to High(data) do
    crc := CRC8_ROHC_Table[crc xor data[i]];
  Result := crc;
end;

(*
 ============================================================================
    CRC-8 / WCDMA
    Poly=0x9B  Init=0x00  RefIn/RefOut=FALSE  XorOut=0x00
 ============================================================================
*)
const CRC8_WCDMA_Table: array[0..255] of Byte = (
  $00,$9B,$AD,$36,$C1,$5A,$6C,$F7,$19,$82,$B4,$2F,$D8,$43,$75,$EE,
  $32,$A9,$9F,$04,$F3,$68,$5E,$C5,$2B,$B0,$86,$1D,$EA,$71,$47,$DC,
  $64,$FF,$C9,$52,$A5,$3E,$08,$93,$7D,$E6,$D0,$4B,$BC,$27,$11,$8A,
  $56,$CD,$FB,$60,$97,$0C,$3A,$A1,$4F,$D4,$E2,$79,$8E,$15,$23,$B8,
  $C8,$53,$65,$FE,$09,$92,$A4,$3F,$D1,$4A,$7C,$E7,$10,$8B,$BD,$26,
  $FA,$61,$57,$CC,$3B,$A0,$96,$0D,$E3,$78,$4E,$D5,$22,$B9,$8F,$14,
  $AC,$37,$01,$9A,$6D,$F6,$C0,$5B,$B5,$2E,$18,$83,$74,$EF,$D9,$42,
  $9E,$05,$33,$A8,$5F,$C4,$F2,$69,$87,$1C,$2A,$B1,$46,$DD,$EB,$70,
  $8D,$16,$20,$BB,$4C,$D7,$E1,$7A,$94,$0F,$39,$A2,$55,$CE,$F8,$63,
  $BF,$24,$12,$89,$7E,$E5,$D3,$48,$A6,$3D,$0B,$90,$67,$FC,$CA,$51,
  $E9,$72,$44,$DF,$28,$B3,$85,$1E,$F0,$6B,$5D,$C6,$31,$AA,$9C,$07,
  $DB,$40,$76,$ED,$1A,$81,$B7,$2C,$C2,$59,$6F,$F4,$03,$98,$AE,$35,
  $45,$DE,$E8,$73,$84,$1F,$29,$B2,$5C,$C7,$F1,$6A,$9D,$06,$30,$AB,
  $77,$EC,$DA,$41,$B6,$2D,$1B,$80,$6E,$F5,$C3,$58,$AF,$34,$02,$99,
  $21,$BA,$8C,$17,$E0,$7B,$4D,$D6,$38,$A3,$95,$0E,$F9,$62,$54,$CF,
  $13,$88,$BE,$25,$D2,$49,$7F,$E4,$0A,$91,$A7,$3C,$CB,$50,$66,$FD
);

function CRC8_WCDMA(const data: BYTES; const Init: Byte): Byte;
var i: Integer; crc: Byte;
begin
  crc := Init;
  for i := 0 to High(data) do
    crc := CRC8_WCDMA_Table[crc xor data[i]];
  Result := crc;
end;

(*
 ============================================================================
    CRC-8 / OPENSAFETY
    Poly=0x2F  Init=0x00  RefIn/RefOut=FALSE  XorOut=0x00
 ============================================================================
*)
const CRC8_OPENSafety_Table: array[0..255] of Byte = (
  $00,$2F,$5E,$71,$BC,$93,$E2,$CD,$57,$78,$09,$26,$EB,$C4,$B5,$9A,
  $AE,$81,$F0,$DF,$12,$3D,$4C,$63,$F9,$D6,$A7,$88,$45,$6A,$1B,$34,
  $73,$5C,$2D,$02,$CF,$E0,$91,$BE,$24,$0B,$7A,$55,$98,$B7,$C6,$E9,
  $DD,$F2,$83,$AC,$61,$4E,$3F,$10,$8A,$A5,$D4,$FB,$36,$19,$68,$47,
  $E6,$C9,$B8,$97,$5A,$75,$04,$2B,$B1,$9E,$EF,$C0,$0D,$22,$53,$7C,
  $48,$67,$16,$39,$F4,$DB,$AA,$85,$1F,$30,$41,$6E,$A3,$8C,$FD,$D2,
  $95,$BA,$CB,$E4,$29,$06,$77,$58,$C2,$ED,$9C,$B3,$7E,$51,$20,$0F,
  $3B,$14,$65,$4A,$87,$A8,$D9,$F6,$6C,$43,$32,$1D,$D0,$FF,$8E,$A1,
  $CD,$E2,$93,$BC,$71,$5E,$2F,$00,$9A,$B5,$C4,$EB,$26,$09,$78,$57,
  $63,$4C,$3D,$12,$DF,$F0,$81,$AE,$34,$1B,$6A,$45,$88,$A7,$D6,$F9,
  $BE,$91,$E0,$CF,$02,$2D,$5C,$73,$E9,$C6,$B7,$98,$55,$7A,$0B,$24,
  $10,$3F,$4E,$61,$AC,$83,$F2,$DD,$47,$68,$19,$36,$FB,$D4,$A5,$8A,
  $2B,$04,$75,$5A,$97,$B8,$C9,$E6,$7C,$53,$22,$0D,$C0,$EF,$9E,$B1,
  $85,$AA,$DB,$F4,$39,$16,$67,$48,$D2,$FD,$8C,$A3,$6E,$41,$30,$1F,
  $58,$77,$06,$29,$E4,$CB,$BA,$95,$0F,$20,$51,$7C,$B3,$9C,$ED,$C2,
  $F6,$D9,$A8,$87,$4A,$65,$14,$3B,$A1,$8E,$FF,$D0,$1D,$32,$43,$6C
);

function CRC8_OPENSAFETY(const data: BYTES; const Init: Byte): Byte;
var i: Integer; crc: Byte;
begin
  crc := Init;
  for i := 0 to High(data) do
    crc := CRC8_OPENSafety_Table[crc xor data[i]];
  Result := crc;
end;

(*
 ============================================================================
    CRC-8 / AUTOSAR
    Poly=0x2F  Init=0xFF  RefIn/RefOut=FALSE  XorOut=0xFF
 ============================================================================
*)
const CRC8_AUTOSAR_Table: array[0..255] of Byte = (
  $00,$2F,$5E,$71,$BC,$93,$E2,$CD,$57,$78,$09,$26,$EB,$C4,$B5,$9A,
  $AE,$81,$F0,$DF,$12,$3D,$4C,$63,$F9,$D6,$A7,$88,$45,$6A,$1B,$34,
  $73,$5C,$2D,$02,$CF,$E0,$91,$BE,$24,$0B,$7A,$55,$98,$B7,$C6,$E9,
  $DD,$F2,$83,$AC,$61,$4E,$3F,$10,$8A,$A5,$D4,$FB,$36,$19,$68,$47,
  $E6,$C9,$B8,$97,$5A,$75,$04,$2B,$B1,$9E,$EF,$C0,$0D,$22,$53,$7C,
  $48,$67,$16,$39,$F4,$DB,$AA,$85,$1F,$30,$41,$6E,$A3,$8C,$FD,$D2,
  $95,$BA,$CB,$E4,$29,$06,$77,$58,$C2,$ED,$9C,$B3,$7E,$51,$20,$0F,
  $3B,$14,$65,$4A,$87,$A8,$D9,$F6,$6C,$43,$32,$1D,$D0,$FF,$8E,$A1,
  $CD,$E2,$93,$BC,$71,$5E,$2F,$00,$9A,$B5,$C4,$EB,$26,$09,$78,$57,
  $63,$4C,$3D,$12,$DF,$F0,$81,$AE,$34,$1B,$6A,$45,$88,$A7,$D6,$F9,
  $BE,$91,$E0,$CF,$02,$2D,$5C,$73,$E9,$C6,$B7,$98,$55,$7A,$0B,$24,
  $10,$3F,$4E,$61,$AC,$83,$F2,$DD,$47,$68,$19,$36,$FB,$D4,$A5,$8A,
  $2B,$04,$75,$5A,$97,$B8,$C9,$E6,$7C,$53,$22,$0D,$C0,$EF,$9E,$B1,
  $85,$AA,$DB,$F4,$39,$16,$67,$48,$D2,$FD,$8C,$A3,$6E,$41,$30,$1F,
  $58,$77,$06,$29,$E4,$CB,$BA,$95,$0F,$20,$51,$7C,$B3,$9C,$ED,$C2,
  $F6,$D9,$A8,$87,$4A,$65,$14,$3B,$A1,$8E,$FF,$D0,$1D,$32,$43,$6C
);

function CRC8_AUTOSAR(const data: BYTES; const Init: Byte): Byte;
var i: Integer; crc: Byte;
begin
  crc := Init;
  for i := 0 to High(data) do
    crc := CRC8_AUTOSAR_Table[crc xor data[i]];
  Result := crc xor $FF;  // XOR-OUT
end;

function CRC8_ICODE(const Data: Bytes; const Init: Byte): Byte;
const
  POLY   = $1D;   // Polinômio CRC-8 I-CODE
  //INIT   = $FD;   // Valor inicial
  XOROUT = $00;   // XOR final
var
  crc: Byte;
  b: Byte;
  i: Integer;
  bit: Integer;
begin
  crc := INIT;

  for i := 0 to High(Data) do
  begin
    b := Data[i];
    crc := crc xor b;

    // shift bit a bit, MSB first
    for bit := 0 to 7 do
    begin
      if (crc and $80) <> 0 then
        crc := (crc shl 1) xor POLY
      else
        crc := (crc shl 1);
    end;

    crc := crc and $FF; // garantir 8 bits
  end;

  Result := crc xor XOROUT;
end;


// ---------------------------------------------------------
// Reflect8 via lookup table (fast)
// ---------------------------------------------------------
var
  Reflect8Table: array[0..255] of Byte;
  Reflect8Table_Inited: Boolean = False;

procedure Init_Reflect8Table;
var
  i, bit: Integer;
  b, r: Byte;
begin
  if Reflect8Table_Inited then Exit;
  for i := 0 to 255 do
  begin
    b := Byte(i);
    r := 0;
    for bit := 0 to 7 do
      if (b and (1 shl bit)) <> 0 then
        r := r or (1 shl (7 - bit));
    Reflect8Table[i] := r;
  end;
  Reflect8Table_Inited := True;
end;

function Reflect8(b: Byte): Byte;
begin
  // garante tabela inicializada (chamada barata após init)
  if not Reflect8Table_Inited then
    Init_Reflect8Table;
  Result := Reflect8Table[b];
end;

// ---------------------------------------------------------
// Generate 256-entry CRC-8 lookup table
// ---------------------------------------------------------
procedure CRC8_GenerateTable(var Table: TCRC8_Table;
                             Poly: Byte;
                             RefIn: Boolean);
var
  i, j: Integer;
  crc: Byte;
  P: Byte;
begin
  // If reflected algorithm, reflect polynomial
  if RefIn then
    P := Reflect8(Poly)
  else
    P := Poly;

  for i := 0 to 255 do
  begin
    crc := i;

    for j := 0 to 7 do
    begin
      if RefIn then
      begin
        // LSB-first (reflected)
        if (crc and $01) <> 0 then
          crc := (crc shr 1) xor P
        else
          crc := crc shr 1;
      end
      else
      begin
        // MSB-first (normal)
        if (crc and $80) <> 0 then
          crc := (crc shl 1) xor P
        else
          crc := crc shl 1;
      end;
    end;

    Table[i] := crc;
  end;
end;

// ---------------------------------------------------------
// Generic CRC-8 calculation using lookup table
// ---------------------------------------------------------
function CRC8_Calc(const Data: BYTES;
                   const Table: TCRC8_Table;
                   Init: Byte;
                   RefOut: Boolean;
                   XorOut: Byte): Byte; inline;
var
  crc: Byte;
  i: Integer;
begin
  crc := Init;

  for i := 0 to High(Data) do
    crc := Table[(crc xor Data[i])];

  if RefOut then
    crc := Reflect8(crc);

  Result := crc xor XorOut;
end;

end.
 
