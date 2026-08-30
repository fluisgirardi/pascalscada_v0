unit S7PlusHarpoBigInt;

{$mode objfpc}{$H+}
{$Q-}{$R-}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/big_int_operations.py. BigInt-style operations on 5/6-uint32
//: arrays, used by the seed/key-derivation transforms to pack/unpack 192-bit integers
//: into the proprietary curve's 30-bits-per-limb / 26-bits-per-limb internal format.

interface

uses SysUtils, Math;

const
  PREPARE_DESTINATION_SIZE = $05*4;
  PREPARE_SOURCE_SIZE = $06*4;
  FINALIZE_DESTINATION_SIZE = $06*4;
  FINALIZE_SOURCE_SIZE = $05*4;

//: Reads 24 bytes (6 uint32 LE) from Source, writes 20 bytes (5 uint32 LE) to Destination.
procedure Prepare(var Destination:TBytes; const Source:TBytes);
//: Reads 20 bytes (5 uint32 LE) from Source, writes 24 bytes (6 uint32 LE) to Destination.
//: Inverse of Prepare's packing.
procedure Finalize_(var Destination:TBytes; const Source:TBytes);
//: In-place Prepare followed by Finalize on the same 24-byte buffer.
procedure PrepareFinalize(var Buffer:TBytes);
//: Rotates a 6-uint32 buffer right by 30 bits, in place.
procedure RotateRight30(var Buffer:TBytes);
//: Rotates the leading 4 uint32s of a buffer left by 31 bits, with a custom
//: polynomial reduction on overflow, in place.
procedure RotateLeft31(var Buffer:TBytes);

implementation

function CarryHelper(A, B:Cardinal):Cardinal;
begin
  if A<B then Result := 1 else Result := 0;
end;

function BytesToU32LE(const B:TBytes; Off:Integer):Cardinal;
begin
  Result := Cardinal(B[Off]) or (Cardinal(B[Off+1]) shl 8) or
            (Cardinal(B[Off+2]) shl 16) or (Cardinal(B[Off+3]) shl 24);
end;

procedure U32ToBytesLE(V:Cardinal; var B:TBytes; Off:Integer);
begin
  B[Off]   := Byte(V and $FF);
  B[Off+1] := Byte((V shr 8) and $FF);
  B[Off+2] := Byte((V shr 16) and $FF);
  B[Off+3] := Byte((V shr 24) and $FF);
end;

procedure Prepare(var Destination:TBytes; const Source:TBytes);
var
  Src:array[0..5] of Cardinal;
  Dst:array[0..4] of Cardinal;
  Temp0, Temp1:Cardinal;
  i:Integer;
begin
  if Length(Destination)<PREPARE_DESTINATION_SIZE then
    raise Exception.CreateFmt('destination must be at least %d bytes, got %d', [PREPARE_DESTINATION_SIZE, Length(Destination)]);
  if Length(Source)<PREPARE_SOURCE_SIZE then
    raise Exception.CreateFmt('source must be at least %d bytes, got %d', [PREPARE_SOURCE_SIZE, Length(Source)]);

  for i:=0 to 5 do Src[i] := BytesToU32LE(Source, i*4);

  Temp0 := Src[0];
  Temp1 := ((Src[1] shl $1A) + (Temp0 shr 2)) and $FFFFFFFF;
  Dst[0] := Temp1;

  Temp1 := ((CarryHelper(Temp1, Temp0) shr 2) + (Src[1] shr 6)) and $FFFFFFFF;
  Temp0 := (Src[2]*$400000 + Temp1) and $FFFFFFFF;
  Dst[1] := Temp0;

  Temp1 := (CarryHelper(Temp0, Temp1) + (Src[2] shr 10)) and $FFFFFFFF;
  Temp0 := (Src[3]*$40000 + Temp1) and $FFFFFFFF;
  Dst[2] := Temp0;

  Temp1 := (CarryHelper(Temp0, Temp1) + (Src[3] shr $E)) and $FFFFFFFF;
  Temp0 := (Src[4]*$4000 + Temp1) and $FFFFFFFF;
  Dst[3] := Temp0;

  Temp1 := (CarryHelper(Temp0, Temp1) + (Src[4] shr $12)) and $FFFFFFFF;
  Temp0 := (Src[5]*$400 + Temp1) and $FFFFFFFF;
  Dst[4] := Temp0;

  Temp0 := ((CarryHelper(Temp0, Temp1) + (Src[5] shr $16)) * $2F) and $FFFFFFFF;

  if Temp0<>0 then begin
    Dst[0] := (Dst[0] + Temp0) and $FFFFFFFF;
    Dst[1] := (Dst[1] + CarryHelper(Dst[0], Temp0)) and $FFFFFFFF;

    Temp0 := CarryHelper(Dst[1], CarryHelper(Dst[0], Temp0));
    Dst[2] := (Dst[2] + Temp0) and $FFFFFFFF;

    Temp0 := CarryHelper(Dst[2], Temp0);
    Dst[3] := (Dst[3] + Temp0) and $FFFFFFFF;

    Temp0 := CarryHelper(Dst[3], Temp0);
    Dst[4] := (Dst[4] + Temp0) and $FFFFFFFF;

    Dst[0] := (Dst[0] + CarryHelper(Dst[4], Temp0) * $2F) and $FFFFFFFF;
  end;

  for i:=0 to 4 do U32ToBytesLE(Dst[i], Destination, i*4);
end;

procedure Finalize_(var Destination:TBytes; const Source:TBytes);
var
  Real_:TBytes;
  n, i:Integer;
  Src:array[0..4] of Cardinal;
  Dst:array[0..5] of Cardinal;
begin
  if Length(Destination)<FINALIZE_DESTINATION_SIZE then
    raise Exception.CreateFmt('destination must be at least %d bytes, got %d', [FINALIZE_DESTINATION_SIZE, Length(Destination)]);
  if Length(Source)=0 then
    raise Exception.Create('source must not be empty');

  SetLength(Real_, FINALIZE_SOURCE_SIZE);
  FillChar(Real_[0], FINALIZE_SOURCE_SIZE, 0);
  n := Min(Length(Source), FINALIZE_SOURCE_SIZE);
  for i:=0 to n-1 do Real_[i] := Source[i];

  for i:=0 to 4 do Src[i] := BytesToU32LE(Real_, i*4);

  Dst[0] := ((Src[0] and $0FFFFFFF) shl 2) and $FFFFFFFF;
  Dst[1] := (((Src[1] shl $06) or (Src[0] shr $1A)) and $3FFFFFFC) and $FFFFFFFF;
  Dst[2] := (((Src[2] shl $0A) or (Src[1] shr $16)) and $3FFFFFFC) and $FFFFFFFF;
  Dst[3] := (((Src[2] shr $12) or (Src[3] shl $0E)) and $3FFFFFFC) and $FFFFFFFF;
  Dst[4] := (((Src[4] shl $12) or (Src[3] shr $0E)) and $3FFFFFFC) and $FFFFFFFF;
  Dst[5] := ((Src[4] shr $0A) and $3FFFFC) and $FFFFFFFF;

  for i:=0 to 5 do U32ToBytesLE(Dst[i], Destination, i*4);
end;

procedure PrepareFinalize(var Buffer:TBytes);
var
  Ds:array[0..5] of Cardinal;
  Temp0, Temp1, Temp2, Temp3, Temp4, Temp5, Temp6:Cardinal;
  Out_:array[0..5] of Cardinal;
  i:Integer;
begin
  if Length(Buffer)<FINALIZE_DESTINATION_SIZE then
    raise Exception.CreateFmt('buffer must be at least %d bytes, got %d', [FINALIZE_DESTINATION_SIZE, Length(Buffer)]);

  for i:=0 to 5 do Ds[i] := BytesToU32LE(Buffer, i*4);

  Temp0 := (Ds[1]*$4000000 + (Ds[0] shr 2)) and $FFFFFFFF;
  Temp1 := (CarryHelper(Temp0, Ds[0] shr 2) + (Ds[1] shr 6)) and $FFFFFFFF;

  Temp2 := (Ds[2]*$400000 + Temp1) and $FFFFFFFF;
  Temp1 := (CarryHelper(Temp2, Temp1) + (Ds[2] shr 10)) and $FFFFFFFF;

  Temp3 := (Ds[3]*$40000 + Temp1) and $FFFFFFFF;
  Temp1 := (CarryHelper(Temp3, Temp1) + (Ds[3] shr $E)) and $FFFFFFFF;

  Temp4 := (Ds[4]*$4000 + Temp1) and $FFFFFFFF;
  Temp1 := (CarryHelper(Temp4, Temp1) + (Ds[4] shr $12)) and $FFFFFFFF;

  Temp5 := (Ds[5]*$400 + Temp1) and $FFFFFFFF;
  Temp1 := ((CarryHelper(Temp5, Temp1) + (Ds[5] shr $16)) * $2F) and $FFFFFFFF;

  if Temp1<>0 then begin
    Temp6 := CarryHelper((Temp0+Temp1) and $FFFFFFFF, Temp1);

    Temp2 := (Temp2+Temp6) and $FFFFFFFF;
    Temp6 := CarryHelper(Temp2, Temp6);

    Temp3 := (Temp3+Temp6) and $FFFFFFFF;
    Temp6 := CarryHelper(Temp3, Temp6);

    Temp4 := (Temp4+Temp6) and $FFFFFFFF;
    Temp6 := CarryHelper(Temp4, Temp6);

    Temp5 := (Temp5+Temp6) and $FFFFFFFF;
    Temp0 := (Temp0 + Temp1 + CarryHelper(Temp5, Temp6) * $2F) and $FFFFFFFF;
  end;

  Out_[0] := ((Temp0 and $FFFFFFF) shl 2) and $FFFFFFFF;
  Out_[1] := (((Temp2 shl 6) or (Temp0 shr $1A)) and $3FFFFFFC) and $FFFFFFFF;
  Out_[2] := (((Temp3 shl 10) or (Temp2 shr $16)) and $3FFFFFFC) and $FFFFFFFF;
  Out_[3] := (((Temp4 shl $E) or (Temp3 shr $12)) and $3FFFFFFC) and $FFFFFFFF;
  Out_[4] := (((Temp5 shl $12) or (Temp4 shr $E)) and $3FFFFFFC) and $FFFFFFFF;
  Out_[5] := ((Temp5 shr 10) and $3FFFFC) and $FFFFFFFF;

  for i:=0 to 5 do U32ToBytesLE(Out_[i], Buffer, i*4);
end;

procedure RotateRight30(var Buffer:TBytes);
var
  Ds:array[0..5] of Cardinal;
  i:Integer;
begin
  for i:=0 to 5 do Ds[i] := BytesToU32LE(Buffer, i*4);
  Ds[5] := (Ds[4] shr $1E) and $FFFFFFFF;
  for i:=4 downto 1 do
    Ds[i] := ((Ds[i-1] shr $1E) or ((Ds[i] shl 2) and $FFFFFFFF)) and $FFFFFFFF;
  Ds[0] := (Ds[0] shl 2) and $FFFFFFFF;
  for i:=0 to 5 do U32ToBytesLE(Ds[i], Buffer, i*4);
end;

procedure RotateLeft31(var Buffer:TBytes);
var
  Ds:array[0..3] of Cardinal;
  First_:Cardinal;
  OverflowMask:Cardinal;
  i:Integer;
begin
  for i:=0 to 3 do Ds[i] := BytesToU32LE(Buffer, i*4);
  First_ := Ds[0];
  for i:=0 to 2 do
    Ds[i] := (((Ds[i+1] shl $1F) and $FFFFFFFF) or (Ds[i] shr 1)) and $FFFFFFFF;
  if (First_ and 1)<>0 then OverflowMask := $E1000000 else OverflowMask := 0;
  Ds[3] := ((Ds[3] shr 1) xor OverflowMask) and $FFFFFFFF;
  for i:=0 to 3 do U32ToBytesLE(Ds[i], Buffer, i*4);
end;

end.
