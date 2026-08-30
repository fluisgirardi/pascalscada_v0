unit S7PlusHarpoHash;

{$mode objfpc}{$H+}
{$Q-}{$R-}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/harpo_hash.py. "HarpoHash" is NOT a cryptographic hash - it's a
//: proprietary pseudo-AES round function Siemens layered on top of AES-ECB in
//: OMSp_core_managed.dll, used both to encrypt the SessionKey blob's random seed and
//: to compute HarpoAesCtr's integrity MAC.

interface

uses SysUtils;

const
  LUT_SEED_WORD_COUNT = 256;

//: LutSeed/HarpoHashSeed's 512 bytes, reinterpreted as 256 little-endian uint16 words
//: (both primitives only ever read it 2 bytes at a time, at even offsets).
var
  LutSeedWords:array[0..LUT_SEED_WORD_COUNT-1] of Word;

//: One HarpoHash round on 16 bytes, via a fixed 512-byte table. Raises on wrong size.
function Lut1(const State:TBytes):TBytes;
//: Derives the 4KB working table (1024 little-endian uint32s) from a 16-byte seed key.
function GenerateLookupTable(const Key:TBytes):TBytes;
//: Runs the working table over a 16-byte input, producing 16 bytes - the per-block
//: primitive HarpoAesCtr uses for its integrity-MAC computation.
function HashBlock(const Data, Lut:TBytes):TBytes;

implementation

procedure InitLutSeedWords;
const
  LutSeedBytes:array[0..511] of Byte = (
    $00, $00, $01, $C2, $03, $84, $02, $46, $07, $08, $06, $CA, $04, $8C, $05, $4E,
    $0E, $10, $0F, $D2, $0D, $94, $0C, $56, $09, $18, $08, $DA, $0A, $9C, $0B, $5E,
    $1C, $20, $1D, $E2, $1F, $A4, $1E, $66, $1B, $28, $1A, $EA, $18, $AC, $19, $6E,
    $12, $30, $13, $F2, $11, $B4, $10, $76, $15, $38, $14, $FA, $16, $BC, $17, $7E,
    $38, $40, $39, $82, $3B, $C4, $3A, $06, $3F, $48, $3E, $8A, $3C, $CC, $3D, $0E,
    $36, $50, $37, $92, $35, $D4, $34, $16, $31, $58, $30, $9A, $32, $DC, $33, $1E,
    $24, $60, $25, $A2, $27, $E4, $26, $26, $23, $68, $22, $AA, $20, $EC, $21, $2E,
    $2A, $70, $2B, $B2, $29, $F4, $28, $36, $2D, $78, $2C, $BA, $2E, $FC, $2F, $3E,
    $70, $80, $71, $42, $73, $04, $72, $C6, $77, $88, $76, $4A, $74, $0C, $75, $CE,
    $7E, $90, $7F, $52, $7D, $14, $7C, $D6, $79, $98, $78, $5A, $7A, $1C, $7B, $DE,
    $6C, $A0, $6D, $62, $6F, $24, $6E, $E6, $6B, $A8, $6A, $6A, $68, $2C, $69, $EE,
    $62, $B0, $63, $72, $61, $34, $60, $F6, $65, $B8, $64, $7A, $66, $3C, $67, $FE,
    $48, $C0, $49, $02, $4B, $44, $4A, $86, $4F, $C8, $4E, $0A, $4C, $4C, $4D, $8E,
    $46, $D0, $47, $12, $45, $54, $44, $96, $41, $D8, $40, $1A, $42, $5C, $43, $9E,
    $54, $E0, $55, $22, $57, $64, $56, $A6, $53, $E8, $52, $2A, $50, $6C, $51, $AE,
    $5A, $F0, $5B, $32, $59, $74, $58, $B6, $5D, $F8, $5C, $3A, $5E, $7C, $5F, $BE,
    $E1, $00, $E0, $C2, $E2, $84, $E3, $46, $E6, $08, $E7, $CA, $E5, $8C, $E4, $4E,
    $EF, $10, $EE, $D2, $EC, $94, $ED, $56, $E8, $18, $E9, $DA, $EB, $9C, $EA, $5E,
    $FD, $20, $FC, $E2, $FE, $A4, $FF, $66, $FA, $28, $FB, $EA, $F9, $AC, $F8, $6E,
    $F3, $30, $F2, $F2, $F0, $B4, $F1, $76, $F4, $38, $F5, $FA, $F7, $BC, $F6, $7E,
    $D9, $40, $D8, $82, $DA, $C4, $DB, $06, $DE, $48, $DF, $8A, $DD, $CC, $DC, $0E,
    $D7, $50, $D6, $92, $D4, $D4, $D5, $16, $D0, $58, $D1, $9A, $D3, $DC, $D2, $1E,
    $C5, $60, $C4, $A2, $C6, $E4, $C7, $26, $C2, $68, $C3, $AA, $C1, $EC, $C0, $2E,
    $CB, $70, $CA, $B2, $C8, $F4, $C9, $36, $CC, $78, $CD, $BA, $CF, $FC, $CE, $3E,
    $91, $80, $90, $42, $92, $04, $93, $C6, $96, $88, $97, $4A, $95, $0C, $94, $CE,
    $9F, $90, $9E, $52, $9C, $14, $9D, $D6, $98, $98, $99, $5A, $9B, $1C, $9A, $DE,
    $8D, $A0, $8C, $62, $8E, $24, $8F, $E6, $8A, $A8, $8B, $6A, $89, $2C, $88, $EE,
    $83, $B0, $82, $72, $80, $34, $81, $F6, $84, $B8, $85, $7A, $87, $3C, $86, $FE,
    $A9, $C0, $A8, $02, $AA, $44, $AB, $86, $AE, $C8, $AF, $0A, $AD, $4C, $AC, $8E,
    $A7, $D0, $A6, $12, $A4, $54, $A5, $96, $A0, $D8, $A1, $1A, $A3, $5C, $A2, $9E,
    $B5, $E0, $B4, $22, $B6, $64, $B7, $A6, $B2, $E8, $B3, $2A, $B1, $6C, $B0, $AE,
    $BB, $F0, $BA, $32, $B8, $74, $B9, $B6, $BC, $F8, $BD, $3A, $BF, $7C, $BE, $BE
  );
var
  i:Integer;
begin
  for i:=0 to LUT_SEED_WORD_COUNT-1 do
    LutSeedWords[i] := Word(LutSeedBytes[i*2]) or (Word(LutSeedBytes[i*2+1]) shl 8);
end;

//: One Lut1 step: reads 4 uint32s from State[SrcOff..SrcOff+3], writes 4 to
//: State[DstOff..DstOff+3]. Writes happen after all reads, so overlapping/in-place
//: ranges (SrcOff=DstOff) are safe - mirrors HarpoS7's KeyExtensions.Lut1Inplace.
procedure Lut1Inplace(var State:array of Cardinal; DstOff, SrcOff:Integer);
var
  A0, A1, A2, A3:Cardinal;
  T1Initial, SeedIndex:Cardinal;
  T2:Word;
  Out0, Out1, Out2, Out3:Cardinal;
  T1, Val_, Shifted:Cardinal;
begin
  A0 := State[SrcOff];
  A1 := State[SrcOff+1];
  A2 := State[SrcOff+2];
  A3 := State[SrcOff+3];

  T1Initial := A3;
  SeedIndex := (((T1Initial shr $11) and $80808080) * 2) and $FFFFFFFF;
  T2 := LutSeedWords[(SeedIndex shr 1) and $FF]; // SeedIndex is always an even byte offset < 512

  //slot 3: val_idx=2 (A2), t1_idx=3 (A3)
  T1 := A3; Val_ := A2;
  Shifted := (Val_ shr $11) or ((T1 shl $F) and $FFFFFFFF);
  Out3 := ((Shifted xor (T1 shr 1)) and $7F7F7F7F) xor Shifted;

  //slot 2: val_idx=1 (A1), t1_idx=2 (A2)
  T1 := A2; Val_ := A1;
  Shifted := (Val_ shr $11) or ((T1 shl $F) and $FFFFFFFF);
  Out2 := ((Shifted xor (T1 shr 1)) and $7F7F7F7F) xor Shifted;

  //slot 1: val_idx=0 (A0), t1_idx=1 (A1)
  T1 := A1; Val_ := A0;
  Shifted := (Val_ shr $11) or ((T1 shl $F) and $FFFFFFFF);
  Out1 := ((Shifted xor (T1 shr 1)) and $7F7F7F7F) xor Shifted;

  T1 := A0;
  Out0 := ((((T1 shl $F) and $FFFFFFFF) xor (T1 shr 1)) and $7F7F7F7F) xor ((T1 shl $F) and $FFFFFFFF) xor T2;

  State[DstOff]   := Out0;
  State[DstOff+1] := Out1;
  State[DstOff+2] := Out2;
  State[DstOff+3] := Out3;
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

function Lut1(const State:TBytes):TBytes;
var
  Uints:array[0..7] of Cardinal;
  i:Integer;
begin
  if Length(State)<>16 then
    raise Exception.Create('state must be 16 bytes');
  for i:=0 to 3 do Uints[i] := BytesToU32LE(State, i*4);
  Uints[4] := 0; Uints[5] := 0; Uints[6] := 0; Uints[7] := 0;
  Lut1Inplace(Uints, 4, 0);
  SetLength(Result, 16);
  for i:=0 to 3 do U32ToBytesLE(Uints[4+i], Result, i*4);
end;

function GenerateLookupTable(const Key:TBytes):TBytes;
var
  State:array[0..1023] of Cardinal;
  i, Index, j, k, Dest1, Dest2, Src, n:Integer;
begin
  if Length(Key)<>16 then
    raise Exception.Create('key must be 16 bytes');
  for i:=0 to 1023 do State[i] := 0;
  for i:=0 to 3 do State[$200+i] := BytesToU32LE(Key, i*4);

  //First loop: index = 64,32,16,8,4,2,1 - Lut1 reduces from offset (index*8) into
  //offset (index*4), cascading the 16-byte key at uint-index 0x200 back through the
  //table to fill uints 4..8.
  Index := $40;
  while Index<>0 do begin
    Lut1Inplace(State, Index*4, Index*8);
    Index := Index shr 1;
  end;

  //Second loop: index = 2,4,8,...,128 - XORs uint-aligned regions of the table with
  //the 16-byte block at uint indices 4..8 to fill out the rest of the table.
  Index := 2;
  while Index<$100 do begin
    if Index>1 then begin
      j := Index shl 4; //bytes
      k := Index-1;
      Dest1 := (($18+j) div 4); //uint indices
      Dest2 := $18 div 4;
      Src := j div 4;
      for n:=1 to k do begin
        State[Dest1-2] := State[Dest2-2] xor State[Src];
        State[Dest1-1] := State[Dest2-1] xor State[Src+1];
        State[Dest1]   := State[Dest2]   xor State[Src+2];
        State[Dest1+1] := State[Dest2+1] xor State[Src+3];
        Dest1 := Dest1+4;
        Dest2 := Dest2+4;
      end;
    end;
    Index := Index*2;
  end;

  SetLength(Result, 4096);
  for i:=0 to 1023 do U32ToBytesLE(State[i], Result, i*4);
end;

function HashBlock(const Data, Lut:TBytes):TBytes;
var
  LutU:array[0..1023] of Cardinal;
  i, V5:Integer;
  T1, T2, T3, T4:Cardinal;
  V1, V2, V3, V4:Cardinal;
  SeedVal:Word;
begin
  if Length(Data)<>16 then
    raise Exception.Create('data must be 16 bytes');
  if Length(Lut)<>4096 then
    raise Exception.Create('lut must be 4096 bytes');

  for i:=0 to 1023 do LutU[i] := BytesToU32LE(Lut, i*4);

  T1 := 0; T2 := 0; T3 := 0; T4 := 0;
  for i:=15 downto 0 do begin
    V1 := T3 shr $18;
    V2 := T2 shr $18;
    V3 := T1 shr $18;
    V4 := Data[i];
    V5 := (V4*4) and $FFFFFFFF;

    SeedVal := LutSeedWords[(T4 shr $18) and $FF];

    T1 := (((T1 shl 8) and $FFFFFFFF) xor SeedVal xor LutU[V5]) and $FFFFFFFF;
    T2 := ((((T2 shl 8) and $FFFFFFFF) or V3) xor LutU[V5+1]) and $FFFFFFFF;
    T3 := ((((T3 shl 8) and $FFFFFFFF) or V2) xor LutU[V5+2]) and $FFFFFFFF;
    T4 := ((((T4 shl 8) and $FFFFFFFF) or V1) xor LutU[V5+3]) and $FFFFFFFF;
  end;

  SetLength(Result, 16);
  U32ToBytesLE(T1, Result, 0);
  U32ToBytesLE(T2, Result, 4);
  U32ToBytesLE(T3, Result, 8);
  U32ToBytesLE(T4, Result, 12);
end;

initialization
  InitLutSeedWords;

end.
