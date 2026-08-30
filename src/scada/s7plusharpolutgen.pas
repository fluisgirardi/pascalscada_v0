unit S7PlusHarpoLutGen;

{$mode objfpc}{$H+}
{$Q-}{$R-}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/lut_generator.py. Builds a 4KB table of 256 UInt128 entries
//: from a 16-byte seed key (used by ChecksumTransform) - each entry is the seed
//: multiplied by i over GF(2^128) under the canonical AES-GCM polynomial
//: x^128+x^7+x^2+x+1.

interface

uses SysUtils;

const
  LUTGEN_SOURCE_SIZE = $10;
  LUTGEN_DESTINATION_SIZE = $1000;

procedure LutGeneratorExecute(var Destination:TBytes; const Source:TBytes);

implementation

type
  //128-bit value as two QWords (Lo=bits 0..63, Hi=bits 64..127).
  TU128 = record
    Lo, Hi:QWord;
  end;

function U128FromBytesLE(const B:TBytes; Off:Integer):TU128;
var
  i:Integer;
begin
  Result.Lo := 0; Result.Hi := 0;
  for i:=7 downto 0 do Result.Lo := (Result.Lo shl 8) or B[Off+i];
  for i:=15 downto 8 do Result.Hi := (Result.Hi shl 8) or B[Off+i];
end;

procedure U128ToBytesLE(const V:TU128; var B:TBytes; Off:Integer);
var
  i:Integer;
  Lo, Hi:QWord;
begin
  Lo := V.Lo; Hi := V.Hi;
  for i:=0 to 7 do begin B[Off+i] := Byte(Lo and $FF); Lo := Lo shr 8; end;
  for i:=8 to 15 do begin B[Off+i] := Byte(Hi and $FF); Hi := Hi shr 8; end;
end;

function U128Xor(const A, B:TU128):TU128;
begin
  Result.Lo := A.Lo xor B.Lo;
  Result.Hi := A.Hi xor B.Hi;
end;

//: value*2 mod 2^128 (just a 128-bit shift-left-by-1; the field reduction, if the
//: shifted-out top bit was set, is applied by the caller - mirrors Python's
//: `(multiplicand*2) & _U128` followed by a conditional XOR with _REDUCTION).
function U128ShiftLeft1(const A:TU128; out TopBitWasSet:Boolean):TU128;
begin
  TopBitWasSet := (A.Hi and $8000000000000000)<>0;
  Result.Hi := ((A.Hi shl 1) or (A.Lo shr 63));
  Result.Lo := A.Lo shl 1;
end;

procedure LutGeneratorExecute(var Destination:TBytes; const Source:TBytes);
const
  REDUCTION_LO = QWord($100008005); //x^128+x^7+x^2+x+1, low 33 bits - entirely within bits 0..63
  REDUCTION_HI = QWord(0);
var
  Quads:array[0..255] of TU128;
  i, j, ProductIndex:Integer;
  Multiplicand, Product:TU128;
  TopBitSet:Boolean;
  Reduction:TU128;
begin
  if Length(Destination)<LUTGEN_DESTINATION_SIZE then
    raise Exception.CreateFmt('destination must be at least %d bytes, got %d', [LUTGEN_DESTINATION_SIZE, Length(Destination)]);
  if Length(Source)<LUTGEN_SOURCE_SIZE then
    raise Exception.CreateFmt('source must be at least %d bytes, got %d', [LUTGEN_SOURCE_SIZE, Length(Source)]);

  Reduction.Lo := REDUCTION_LO;
  Reduction.Hi := REDUCTION_HI;

  for i:=0 to 255 do begin Quads[i].Lo := 0; Quads[i].Hi := 0; end;
  Quads[1] := U128FromBytesLE(Source, 0);

  i := 1;
  while i<128 do begin
    Multiplicand := Quads[i];
    Product := U128ShiftLeft1(Multiplicand, TopBitSet);
    if TopBitSet then Product := U128Xor(Product, Reduction);

    ProductIndex := i*2;
    Quads[ProductIndex] := Product;

    for j:=1 to ProductIndex-1 do
      Quads[ProductIndex+j] := U128Xor(Quads[j], Product);

    i := i*2;
  end;

  for i:=0 to 255 do U128ToBytesLE(Quads[i], Destination, i*16);
end;

end.
