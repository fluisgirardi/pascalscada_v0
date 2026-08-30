unit S7PlusHarpoFingerprint;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/fingerprint.py - HarpoFingerprint challenge fingerprinting,
//: producing an 8-byte fingerprint from a PLC challenge (used by DeriveSessionKey to
//: build the HMAC-SHA256 input for the 24-byte session key).

interface

uses SysUtils, S7PlusHarpoFamily0Data;

const
  FINGERPRINT_LENGTH = 8;

procedure FingerprintChallenge(var Destination:TBytes; const Challenge:TBytes);

implementation

const
  SMALL_CTX_LEN = 272;
  NUM_MUTATIONS = 20;

type
  TU16Array = array of Word;

var
  Data1, Data2:array[0..NUM_MUTATIONS-1] of TU16Array;
  DataLoaded:Boolean = false;

function U32FromBytes(const B:TBytes; Off:Integer):Cardinal;
begin
  Result := Cardinal(B[Off]) or (Cardinal(B[Off+1]) shl 8) or
            (Cardinal(B[Off+2]) shl 16) or (Cardinal(B[Off+3]) shl 24);
end;

procedure LoadCollection(const Data:TBytes; var Coll:array of TU16Array);
var
  Lengths:array[0..19] of Cardinal;
  i, j, Offset_, Len_:Integer;
begin
  for i:=0 to 19 do
    Lengths[i] := Cardinal(Data[i*4]) or (Cardinal(Data[i*4+1]) shl 8) or
                  (Cardinal(Data[i*4+2]) shl 16) or (Cardinal(Data[i*4+3]) shl 24);
  Offset_ := 80;
  for i:=0 to 19 do begin
    Len_ := Lengths[i];
    SetLength(Coll[i], Len_);
    for j:=0 to Len_-1 do
      Coll[i][j] := Word(Data[Offset_+j*2]) or (Word(Data[Offset_+j*2+1]) shl 8);
    Inc(Offset_, Len_*2);
  end;
end;

procedure EnsureDataLoaded;
begin
  if DataLoaded then exit;
  LoadCollection(FPData1, Data1);
  LoadCollection(FPData2, Data2);
  DataLoaded := true;
end;

function PWVarMask(Value:Cardinal):Cardinal;
begin
  Result := (((Value and 1) * Cardinal(-4)) + 4) and $1F;
end;

function PWVarRead(Value:Cardinal; const SmallCtx:TBytes):Byte;
begin
  Result := Byte((SmallCtx[Value shr 1] shr PWVarMask(Value and $FF)) and $FF);
end;

procedure SubProcedure(const D1:TU16Array; XorMagic:Cardinal; const D2:TU16Array;
                       var SmallCtx:TBytes; var BigCtx:array of Cardinal);
var
  Data2Bytes:TBytes;
  i:Integer;
  Index_, CtxOffset:Integer;
  PwVar0, PwVar1:Byte;
  PwVar2:Word;
  Static3:Byte;
  T1, Mod_:Integer;
  T3, Static6:Cardinal;
  T4:Integer;
  Static5:Byte;
  BVar3:Byte;
  CtxBufferIndex:Integer;
  T5:Integer;
  Data2Index:Integer;
  Data2Byte:Byte;
  FVal:Byte;
begin
  SetLength(Data2Bytes, Length(D2)*2);
  for i:=0 to High(D2) do begin
    Data2Bytes[i*2]   := Byte(D2[i] and $FF);
    Data2Bytes[i*2+1] := Byte((D2[i] shr 8) and $FF);
  end;

  Index_ := 0;
  CtxOffset := 0;

  while Index_<Length(D1) do begin
    PwVar0 := PWVarRead(D1[Index_], SmallCtx); Inc(Index_);
    PwVar1 := PWVarRead(D1[Index_], SmallCtx); Inc(Index_);
    PwVar2 := D1[Index_]; Inc(Index_);

    Static3 := Byte(((PwVar1 and $F) or (PwVar0 shl 4)) and $FF);

    T1 := (Static3 shr 3) + (CtxOffset shr 2);
    Mod_ := T1 mod $2F;
    T3 := BigCtx[Mod_];
    Static6 := (T3 xor XorMagic) and $FFFFFFFF;

    T4 := (($7 - (PwVar1 and $7)) * $04) and $1F;
    Static5 := Byte((Static6 shr T4) and $FF);

    BVar3 := Byte((((PwVar2 and $FF) and 1) * Cardinal(-4) + 4) and $FF);
    CtxBufferIndex := PwVar2 shr 1;

    T5 := BVar3 and $1F;
    Data2Index := ((Static3 shr 1) + CtxOffset);

    if (Data2Index>=0) and (Data2Index<Length(Data2Bytes)) then
      Data2Byte := Data2Bytes[Data2Index]
    else
      Data2Byte := 0;

    FVal := Byte(
      ((((Data2Byte shr PWVarMask(PwVar1 and $FF)) xor Static5) and $F) shl T5) or
      (($F0 shr T5) and SmallCtx[CtxBufferIndex])
    ) and $FF;

    SmallCtx[CtxBufferIndex] := FVal;

    Inc(CtxOffset, $80);
  end;
end;

procedure MutateContext(var BigCtx:array of Cardinal; MutationIndex:Integer);
var
  j:Integer;
begin
  for j:=0 to High(FPMutationGroups[MutationIndex]) do begin
    case FPMutationGroups[MutationIndex][j].Op of
      fpmoAdd: BigCtx[FPMutationGroups[MutationIndex][j].Index_] :=
                 (BigCtx[FPMutationGroups[MutationIndex][j].Index_] + FPMutationGroups[MutationIndex][j].Value) and $FFFFFFFF;
      fpmoMul: BigCtx[FPMutationGroups[MutationIndex][j].Index_] :=
                 (BigCtx[FPMutationGroups[MutationIndex][j].Index_] * FPMutationGroups[MutationIndex][j].Value) and $FFFFFFFF;
      fpmoXor: BigCtx[FPMutationGroups[MutationIndex][j].Index_] :=
                 (BigCtx[FPMutationGroups[MutationIndex][j].Index_] xor FPMutationGroups[MutationIndex][j].Value) and $FFFFFFFF;
    end;
  end;
end;

procedure FinalFingerprint(var FP:TBytes; const SC:TBytes);
begin
  FP[0] := Byte(((SC[93] shl 4) or (SC[224] shr 4)) and $FF);
  FP[1] := Byte(((((FP[1] xor SC[189]) and $F xor SC[189]) xor SC[53]) and $F xor ((FP[1] xor SC[189]) and $F xor SC[189])) and $FF);
  FP[2] := Byte((((FP[2] and $F or (SC[119] shl 4)) xor SC[86]) and $F xor (FP[2] and $F or (SC[119] shl 4))) and $FF);
  FP[3] := Byte((((FP[3] xor SC[83]) and $F xor SC[83]) and $F0 or (SC[33] shr 4)) and $FF);
  FP[4] := Byte(((((FP[4] xor SC[229]) and $F xor SC[229]) xor SC[58]) and $F xor ((FP[4] xor SC[229]) and $F xor SC[229])) and $FF);
  FP[5] := Byte(((((FP[5] xor SC[69]) and $F xor SC[69]) xor SC[165]) and $F xor ((FP[5] xor SC[69]) and $F xor SC[69])) and $FF);
  FP[6] := Byte((((FP[6] xor SC[63]) and $F xor SC[63]) and $F0 or (SC[89] shr 4)) and $FF);
  FP[7] := Byte(((((FP[7] xor SC[172]) and $F xor SC[172]) xor SC[247]) and $F xor ((FP[7] xor SC[172]) and $F xor SC[172])) and $FF);
end;

procedure FingerprintChallenge(var Destination:TBytes; const Challenge:TBytes);
var
  SmallCtx:TBytes;
  BigCtx:array[0..46] of Cardinal;
  i:Integer;
begin
  if Length(Destination)<FINGERPRINT_LENGTH then
    raise Exception.Create('destination must be at least 8 bytes');
  if Length(Challenge)<18 then
    raise Exception.Create('challenge must be at least 18 bytes');

  EnsureDataLoaded;

  SetLength(SmallCtx, SMALL_CTX_LEN);
  FillChar(SmallCtx[0], SMALL_CTX_LEN, 0);
  for i:=0 to 46 do BigCtx[i] := U32FromBytes(FPBigContextInit, i*4);

  Move(Challenge[2], SmallCtx[0], 16);

  for i:=0 to NUM_MUTATIONS-1 do begin
    SubProcedure(Data1[i], U32FromBytes(FPXorMagic, i*4), Data2[i], SmallCtx, BigCtx);
    MutateContext(BigCtx, i);
  end;

  FinalFingerprint(Destination, SmallCtx);
end;

end.
