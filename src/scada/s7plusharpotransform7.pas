unit S7PlusHarpoTransform7;

{$mode objfpc}{$H+}
{$Q-}{$R-}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/transform7.py - the core elliptic-curve-like scalar
//: multiplication, orchestrating MonolithWithCopy wrappers (3-7), Transform12
//: dispatch, BigIntAddition, and a 600-uint work buffer. No direct unit-test vector
//: exists upstream either (see ARCHITECTURE.md) - validated instead by the end-to-end
//: SessionKey handshake against real hardware.

interface

uses SysUtils, S7PlusHarpoFamily0Data, S7PlusHarpoMonolithWrappers, S7PlusHarpoBigInt,
     S7PlusHarpoBigIntX, S7PlusHarpoTransform12;

const
  TRANSFORM7_DESTINATION_SIZE = 72;

procedure Transform7Execute(var Destination:TBytes; const Prng1, Prng2, Source:TBytes);

implementation

const
  WORK_SIZE = 600*4; //2400 bytes

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

function I32(const B:TBytes; Off:Integer):LongInt;
begin
  Result := LongInt(U32(B, Off));
end;

//: big_int_addition(dst_view, bytes(dst_view_current_24), bytes(src_view_24)) - reads
//: 24-byte snapshots at the given offsets, computes, writes the 24-byte result back
//: into Buf at DstOff (mirrors the memoryview-slice call pattern transform7.py uses
//: throughout - Source1 is always the CURRENT content of the destination slot itself).
procedure BigAddAt(var Buf:TBytes; DstOff:Integer; const Src:TBytes; SrcOff:Integer);
var
  S1, S2, D:TBytes;
begin
  SetLength(S1, 24); Move(Buf[DstOff], S1[0], 24);
  SetLength(S2, 24); Move(Src[SrcOff], S2[0], 24);
  SetLength(D, DESTINATION_SIZE);
  BigIntAddition(D, S1, S2);
  Move(D[0], Buf[DstOff], 24);
end;

procedure PrepFinAt(var Buf:TBytes; Off:Integer);
var
  Tmp:TBytes;
begin
  SetLength(Tmp, 24);
  Move(Buf[Off], Tmp[0], 24);
  PrepareFinalize(Tmp);
  Move(Tmp[0], Buf[Off], 24);
end;

procedure Transform7Execute(var Destination:TBytes; const Prng1, Prng2, Source:TBytes);
var
  Prng1Dwords, Prng2Dwords:array[0..4] of Cardinal;
  W, Ctx:TBytes;
  i:Integer;
  WordIdx, BitPos:Integer;
  T12Idx:Integer;
  WDword:Cardinal;
begin
  for i:=0 to 4 do Prng1Dwords[i] := U32(Prng1, i*4);
  for i:=0 to 4 do Prng2Dwords[i] := U32(Prng2, i*4);

  SetLength(W, WORK_SIZE);
  FillChar(W[0], WORK_SIZE, 0);

  //prng1[0:5] -> work dwords [0xC:0x11], then OR 4 into [0xC]
  for i:=0 to 4 do PutU32(W, ($C+i)*4, Prng1Dwords[i]);
  PutU32(W, $C*4, U32(W, $C*4) or 4);

  //source[0:5] -> work dwords [0:5]
  for i:=0 to 4 do PutU32(W, i*4, U32(Source, i*4));

  //source[5:10] -> work dwords [0x12:0x17], then OR 4 into [0x12]
  for i:=0 to 4 do PutU32(W, ($12+i)*4, U32(Source, (5+i)*4));
  PutU32(W, $12*4, U32(W, $12*4) or 4);

  //-- Monolith3 chain --
  Monolith3WithCopy(W,$A8, W,$60, Transform7Data,0, Transform7Data,$48, W,$30);
  Monolith3WithCopy(W,$2A0, W,$210, Transform7Data,0, Transform7Data,$48, W,$48);
  Monolith3WithCopy(W,$1C8, W,$F0, W,$A8, W,$60, W,0);
  Monolith3WithCopy(W,$180, W,$138, W,$1C8, W,$F0, W,$48);

  Monolith4WithCopy(W,$4E0, W,$180, W,$138);

  SetLength(Ctx, CONTEXT_SIZE);
  FillChar(Ctx[0], CONTEXT_SIZE, 0);

  Monolith5WithCopy(Ctx,$450, W,$18, W,$4E0, W,$180, W,$138);
  BigAddAt(Ctx, $450, W, $18);

  Monolith3WithCopy(W,$498, W,$768, W,$180, W,$138, W,$30);

  Monolith4WithCopy(W,$648, Transform7Data,0, Transform7Data,$48);
  Monolith6WithCopy(W,$8D0, W,$888, W,$648, W,$A8, W,$60);

  Monolith4WithCopy(W,$570, W,$498, W,$768);
  Monolith6WithCopy(W,$330, W,$690, W,$570, W,$2A0, W,$210);

  Monolith4WithCopy(W,$840, W,$A8, W,$60);
  Monolith6WithCopy(W,$7F8, W,$6D8, W,$840, W,$1C8, W,$F0);

  Monolith4WithCopy(W,$5B8, W,$7F8, W,$6D8);
  Monolith6WithCopy(W,$3C0, W,$450, W,$5B8, W,$2A0, W,$210);

  Monolith4WithCopy(W,$600, W,$3C0, W,$450);
  Monolith5WithCopy(Ctx,$690, W,$18, W,$600, W,$3C0, W,$450);
  BigAddAt(Ctx, $690, W, $18);

  Monolith4WithCopy(W,$378, W,$330, W,$690);
  Monolith6WithCopy(W,$2E8, W,$258, W,$378, W,$3C0, W,$450);

  Monolith4WithCopy(W,$408, W,$2E8, W,$258);
  Monolith5WithCopy(Ctx,$480, W,$18, W,$408, W,$2E8, W,$258);
  BigAddAt(Ctx, $480, W, $18);

  //RotateRight30 on first 6 uints of work buffer
  RotateRight30(W);

  Monolith3WithCopy(W,$A8, W,$60, Transform7Data,0, Transform7Data,0, W,0);
  Monolith5WithCopy(Ctx,$8D0, W,$18, Transform7Data,0, W,$A8, W,$60);
  BigAddAt(Ctx, $8D0, W, $18);

  //-- Transform12 dispatch loop 1: i = 0..0x9F (prng2 bits) --
  for i:=0 to $9F do begin
    WordIdx := ($9F-i) shr 5;
    BitPos := ($FFFFFFFF-Cardinal(i)) and $1F;
    T12Idx := Integer(((Prng2Dwords[WordIdx] shr BitPos) and 1)) + i*2;
    Transform12Execute(Ctx, I32(Transform7Indexes, T12Idx*4), I32(Transform7Counts, T12Idx*4));
  end;

  //-- Transform12 dispatch loop 2: i = 0xA0..0xF8 (prng1 bits from work buffer) --
  for i:=$A0 to $F8 do begin
    WordIdx := ((i-$A0) shr 5) + $C;
    WDword := U32(W, WordIdx*4);
    T12Idx := Integer((WDword shr Cardinal(i and $1F)) and 1) + i*2;
    Transform12Execute(Ctx, I32(Transform7Indexes, T12Idx*4), I32(Transform7Counts, T12Idx*4));
  end;

  PrepFinAt(Ctx, $918);
  PrepFinAt(Ctx, $6A8);
  PrepFinAt(Ctx, $5B8);
  PrepFinAt(Ctx, $288);

  Monolith7WithCopy(W,$7B0, W,$720, Ctx,$288, Transform7Data,$90);
  Monolith4WithCopy(W,$528, W,$7B0, W,$720);

  Monolith7WithCopy(W,$A8, W,$60, Ctx,$288, W,$528);
  Monolith4WithCopy(W,$2A0, W,$A8, W,$60);

  Monolith7WithCopy(W,$210, W,$1C8, Ctx,$5B8, W,$2A0);
  Monolith4WithCopy(W,$F0, W,$210, W,$1C8);

  Monolith7WithCopy(W,$180, W,$138, Ctx,$918, W,$F0);
  Monolith7WithCopy(W,$4E0, W,$498, Ctx,$6A8, W,$F0);

  Monolith4WithCopy(W,$330, W,$180, W,$138);

  Monolith6WithCopy(W,$2E8, W,$258, W,$330, W,$A8, W,$60);
  Monolith6WithCopy(W,$378, W,$408, W,$528, W,$2E8, W,$258);

  Monolith4WithCopy(Destination, 0, W,$378, W,$408);
end;

end.
