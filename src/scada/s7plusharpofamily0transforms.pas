unit S7PlusHarpoFamily0Transforms;

{$mode objfpc}{$H+}
{$Q-}{$R-}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/{pre_seed_transform,key_derivation_transform,
//: checksum_transform,transform13}.py.

interface

uses SysUtils, S7PlusHarpoMonolith9, S7PlusHarpoMonolith10, S7PlusHarpoFamily0Data;

const
  PRESEED_SOURCE_SIZE = $18;
  PRESEED_DESTINATION_SIZE = $3C;
  KEYDERIV_SOURCE_SIZE = $3C;
  KEYDERIV_DESTINATION_SIZE = $30;
  CHECKSUM_KEY_SIZE = $10;
  CHECKSUM_DESTINATION_SIZE = $10;
  CHECKSUM_LOOKUP_TABLE_SIZE = $1000;
  TRANSFORM13_DESTINATION_SIZE = $3C;
  TRANSFORM13_SOURCE_SIZE = $3C;

procedure PreSeedTransformExecute(var Destination:TBytes; const Source:TBytes);
procedure KeyDerivationTransformExecute(var Destination:TBytes; const Source:TBytes);
procedure ChecksumTransformExecute(var Destination:TBytes; const Key, LookupTable:TBytes);
procedure Transform13Execute(var Destination:TBytes; const Source:TBytes);

implementation

function U32(const B:TBytes; Off:Integer):Cardinal;
begin
  Result := Cardinal(B[Off]) or (Cardinal(B[Off+1]) shl 8) or
            (Cardinal(B[Off+2]) shl 16) or (Cardinal(B[Off+3]) shl 24);
end;

procedure PutU32(var B:TBytes; Off:Integer; V:Cardinal);
begin
  B[Off]   := Byte(V and $FF);
  B[Off+1] := Byte((V shr 8) and $FF);
  B[Off+2] := Byte((V shr 16) and $FF);
  B[Off+3] := Byte((V shr 24) and $FF);
end;

procedure PreSeedTransformExecute(var Destination:TBytes; const Source:TBytes);
const
  MagicPostfix:array[0..11] of Byte = ($79,$B3,$5B,$4F, $5F,$72,$BA,$90, $BB,$D7,$A4,$36);
var
  Work, M9Dst:TBytes;
  i, CopyLen:Integer;
begin
  if Length(Destination)<PRESEED_DESTINATION_SIZE then
    raise Exception.CreateFmt('destination too small (%d, need %d)', [Length(Destination), PRESEED_DESTINATION_SIZE]);
  if Length(Source)<PRESEED_SOURCE_SIZE then
    raise Exception.CreateFmt('source too small (%d, need %d)', [Length(Source), PRESEED_SOURCE_SIZE]);

  SetLength(Work, $C5*4);
  FillChar(Work[0], Length(Work), 0);
  Move(Transform1Data[0], Work[0], Length(Transform1Data));
  Move(MagicPostfix[0], Work[$C2*4], 12);

  SetLength(M9Dst, 24);
  for i:=0 to 2 do begin
    PutU32(Work, $C0*4, U32(Source, i*8));
    PutU32(Work, $C0*4+4, U32(Source, i*8+4));

    S7PlusHarpoMonolith9.Execute(Work, M9Dst);

    if i<2 then CopyLen := 24 else CopyLen := 12;
    Move(M9Dst[0], Destination[i*24], CopyLen);
  end;
end;

procedure KeyDerivationTransformExecute(var Destination:TBytes; const Source:TBytes);
var
  Buf1, Buf2, M9Dst:TBytes;
  i:Integer;
  M9Dw0, M9Dw1:Cardinal;
begin
  if Length(Destination)<KEYDERIV_DESTINATION_SIZE then
    raise Exception.CreateFmt('destination too small (%d, need %d)', [Length(Destination), KEYDERIV_DESTINATION_SIZE]);
  if Length(Source)<KEYDERIV_SOURCE_SIZE then
    raise Exception.CreateFmt('source too small (%d, need %d)', [Length(Source), KEYDERIV_SOURCE_SIZE]);

  SetLength(Buf1, $18*4);
  FillChar(Buf1[0], Length(Buf1), 0);
  PutU32(Buf1, 0, $FFFFFFFF);
  PutU32(Buf1, 4, $FFFFFFFF);
  PutU32(Buf1, 8, $0000FFFF);
  Move(Source[0], Buf1[3*4], KEYDERIV_SOURCE_SIZE);

  SetLength(Buf2, $C5*4);
  S7PlusHarpoMonolith10.Execute(Buf1, Buf2);

  SetLength(M9Dst, 24);

  for i:=0 to 5 do begin
    PutU32(Buf2, $C0*4,   U32(SharedData, (i*2)*4));
    PutU32(Buf2, $C0*4+4, U32(SharedData, (i*2+1)*4));
    PutU32(Buf2, $C2*4,   U32(SharedData, ($12+i*3)*4));
    PutU32(Buf2, $C2*4+4, U32(SharedData, ($12+i*3+1)*4));
    PutU32(Buf2, $C2*4+8, U32(SharedData, ($12+i*3+2)*4));

    S7PlusHarpoMonolith9.Execute(Buf2, M9Dst);

    M9Dw0 := U32(M9Dst, 0);
    M9Dw1 := U32(M9Dst, 4);
    PutU32(Destination, (i*2)*4, M9Dw0);
    PutU32(Destination, (i*2+1)*4, M9Dw1);

    if i=2 then begin
      FillChar(Buf1[0], 12, 0); //dwords 0,1,2 := 0
      S7PlusHarpoMonolith10.Execute(Buf1, Buf2);
    end;
  end;
end;

procedure XOR128(var Work:array of Cardinal; Offset:Integer; const Lut:array of Cardinal; LutIndex:Integer);
var i:Integer;
begin
  for i:=0 to 3 do Work[Offset+i] := Work[Offset+i] xor Lut[LutIndex+i];
end;

procedure ChecksumTransformExecute(var Destination:TBytes; const Key, LookupTable:TBytes);
const
  //NOTE: NOT a `for RoundShift in [$18,$10,$08] do` - FPC's for-in over a set
  //literal iterates in ascending numeric order (8,16,24), silently reversing
  //the round order the Python source actually specifies (24,16,8). Use an
  //explicit ordered array instead.
  RoundShifts:array[0..2] of Integer = ($18, $10, $08);
var
  Work:array[0..7] of Cardinal;
  KeyDwords:array[0..3] of Cardinal;
  LutDwords:array of Cardinal;
  i, j, k, LutIndex, RoundShift:Integer;
  Temp:Cardinal;
  Dst:array[0..3] of Cardinal;
begin
  if Length(Destination)<CHECKSUM_DESTINATION_SIZE then
    raise Exception.CreateFmt('destination must be at least %d bytes', [CHECKSUM_DESTINATION_SIZE]);
  if Length(Key)<CHECKSUM_KEY_SIZE then
    raise Exception.CreateFmt('key must be at least %d bytes', [CHECKSUM_KEY_SIZE]);
  if Length(LookupTable)<CHECKSUM_LOOKUP_TABLE_SIZE then
    raise Exception.CreateFmt('lookup_table must be at least %d bytes', [CHECKSUM_LOOKUP_TABLE_SIZE]);

  for i:=0 to 7 do Work[i] := 0;
  for i:=0 to 3 do KeyDwords[i] := U32(Key, i*4);
  SetLength(LutDwords, CHECKSUM_LOOKUP_TABLE_SIZE div 4);
  for i:=0 to High(LutDwords) do LutDwords[i] := U32(LookupTable, i*4);

  for k:=0 to 2 do begin
    RoundShift := RoundShifts[k];
    for j:=0 to 3 do begin
      LutIndex := Integer((KeyDwords[j] shr RoundShift) and $FF) shl 2;
      XOR128(Work, j, LutDwords, LutIndex);
    end;
    for j:=7 downto 1 do
      Work[j] := ((Work[j-1] shr $18) or ((Work[j] shl $08) and $FFFFFFFF)) and $FFFFFFFF;
    Work[0] := (Work[0] shl $08) and $FFFFFFFF;
  end;

  for i:=0 to 3 do begin
    LutIndex := Integer(KeyDwords[i] and $FF) shl 2;
    XOR128(Work, i, LutDwords, LutIndex);
  end;

  //NOTE: these 5 expressions are generated via transpile_to_pascal.py's expr_to_pas
  //(fully, redundantly parenthesized) rather than hand-typed - a hand-typed version of
  //Dst[0] here was missing one closing paren before "xor Work[0]" and, relying on
  //Pascal's `and` binding tighter than `xor` to compensate, silently computed the wrong
  //value (caught only by cross-checking against a live HarpoS7 Python trace, since the
  //transform4 fixture vector didn't happen to expose it).
  Temp := ((((((Work[7] shr $D) xor Work[7]) shr $11) xor Work[4]) xor Work[7]) and $FFFFFFFF);

  Dst[0] := ((((((((Temp shl $D) and $FFFFFFFF) xor Temp) shl $2) and $FFFFFFFF) xor Work[0]) xor Temp) and $FFFFFFFF);
  Dst[1] := ((((((((Temp shr $D) xor Temp) shr $11) xor (((((Work[5] shl $D) and $FFFFFFFF) xor Work[5]) shl $2) and $FFFFFFFF)) xor Work[1]) xor Temp) xor Work[5]) and $FFFFFFFF);
  Dst[2] := ((((((((Work[5] shr $D) xor Work[5]) shr $11) xor (((((Work[6] shl $D) and $FFFFFFFF) xor Work[6]) shl $2) and $FFFFFFFF)) xor Work[2]) xor Work[5]) xor Work[6]) and $FFFFFFFF);
  Dst[3] := ((((((((Work[6] shr $D) xor Work[6]) shr $11) xor (((((Work[7] shl $D) and $FFFFFFFF) xor Work[7]) shl $2) and $FFFFFFFF)) xor Work[3]) xor Work[6]) xor Work[7]) and $FFFFFFFF);

  for i:=0 to 3 do PutU32(Destination, i*4, Dst[i]);
end;

procedure Transform13Execute(var Destination:TBytes; const Source:TBytes);
const
  StaticMask:array[0..11] of Byte = ($FF,$FF,$FF,$FF, $FF,$FF,$FF,$FF, $FF,$FF,$FF,$00);
var
  M10Mask, M10Dst, M9Dst:TBytes;
  i, DstIndex, CopyLen:Integer;
begin
  if Length(Destination)<TRANSFORM13_DESTINATION_SIZE then
    raise Exception.Create('destination too small');
  if Length(Source)<TRANSFORM13_SOURCE_SIZE then
    raise Exception.Create('source too small');

  SetLength(M10Mask, 12+TRANSFORM13_SOURCE_SIZE);
  Move(StaticMask[0], M10Mask[0], 12);
  Move(Source[0], M10Mask[12], TRANSFORM13_SOURCE_SIZE);

  SetLength(M10Dst, $C0*4+5*4);
  S7PlusHarpoMonolith10.Execute(M10Mask, M10Dst);

  SetLength(M9Dst, 24);

  for i:=6 to 8 do begin
    PutU32(M10Dst, $C0*4,   U32(SharedData, (i*2)*4));
    PutU32(M10Dst, $C0*4+4, U32(SharedData, (i*2+1)*4));
    PutU32(M10Dst, $C2*4,   $4F5BB379);
    PutU32(M10Dst, $C2*4+4, $90BA725F);
    PutU32(M10Dst, $C2*4+8, $36A4D7BB);

    S7PlusHarpoMonolith9.Execute(M10Dst, M9Dst);

    DstIndex := $18*(i-6);
    if i<8 then CopyLen := $18 else CopyLen := $0C;
    Move(M9Dst[0], Destination[DstIndex], CopyLen);

    if i=7 then begin
      FillChar(M10Mask[0], 12, 0);
      S7PlusHarpoMonolith10.Execute(M10Mask, M10Dst);
    end;
  end;
end;

end.
