unit S7PlusHarpoSeedTransform;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/seed_transform.py - generates the encrypted seed blob:
//: Transform7 (twice) with random PRNG buffers, then Monolith1.Loop -> Monolith2
//: (zero check) -> Monolith8 -> Transform13 -> Monolith11.

interface

uses SysUtils, S7PlusSSL, S7PlusHarpoRandom, S7PlusHarpoTransform7, S7PlusHarpoFamily0Transforms,
     S7PlusHarpoMonolith1, S7PlusHarpoMonolith2, S7PlusHarpoMonolith8, S7PlusHarpoMonolith11,
     S7PlusHarpoFamily0Data;

const
  SEED_TRANSFORM_DESTINATION_SIZE = $3C;
  SEED_TRANSFORM_PUBLIC_KEY_LENGTH = $28;

procedure SeedTransformExecute(var Destination:TBytes; const PublicKey, Transform1:TBytes);

implementation

function U32(const B:TBytes; Off:Integer):Cardinal;
begin
  Result := Cardinal(B[Off]) or (Cardinal(B[Off+1]) shl 8) or
            (Cardinal(B[Off+2]) shl 16) or (Cardinal(B[Off+3]) shl 24);
end;

//: Monolith1.Loop: execute until the result is non-zero (Monolith1 is the one monolith
//: with a uint return value - see S7PlusHarpoMonolith1).
procedure Monolith1Loop(var Buf:TBytes);
var
  Src:TBytes;
  Res:Cardinal;
begin
  SetLength(Src, $48);
  Move(Buf[0], Src[0], $48);
  Res := S7PlusHarpoMonolith1.Execute(Src, Buf);
  while Res=0 do begin
    Move(Buf[0], Src[0], $48);
    Res := S7PlusHarpoMonolith1.Execute(Src, Buf);
  end;
end;

procedure SeedTransformExecute(var Destination:TBytes; const PublicKey, Transform1:TBytes);
var
  Prng1, Prng2, T7Dst, Work:TBytes;
  T7Loop:Cardinal;
  i:Integer;
  M8Out, M11Src, M11Dst, T13Out:TBytes;
begin
  if Length(Destination)<SEED_TRANSFORM_DESTINATION_SIZE then
    raise Exception.CreateFmt('destination too small (%d, need %d)', [Length(Destination), SEED_TRANSFORM_DESTINATION_SIZE]);
  if Length(PublicKey)<SEED_TRANSFORM_PUBLIC_KEY_LENGTH then
    raise Exception.CreateFmt('publicKey too small (%d, need %d)', [Length(PublicKey), SEED_TRANSFORM_PUBLIC_KEY_LENGTH]);
  if Length(Transform1)<PRESEED_DESTINATION_SIZE then
    raise Exception.CreateFmt('transform1 too small (%d, need %d)', [Length(Transform1), PRESEED_DESTINATION_SIZE]);

  Prng1 := HarpoRandomBytes($14);
  SetLength(T7Dst, TRANSFORM7_DESTINATION_SIZE);
  SetLength(Work, 5*4); FillChar(Work[0], Length(Work), 0);

  T7Loop := 0;
  repeat
    Prng2 := HarpoRandomBytes($14);
    Transform7Execute(T7Dst, Prng1, Prng2, Copy(Transform7Data, $D8, Length(Transform7Data)-$D8));

    Monolith1Loop(T7Dst);
    S7PlusHarpoMonolith2.Execute(T7Dst, Work);

    T7Loop := 0;
    for i:=0 to 4 do T7Loop := T7Loop or U32(Work, i*4);
  until T7Loop<>0;

  Move(Work[0], Destination[$14], $14);
  Move(Prng1[0], Destination[$28], $14);

  Transform7Execute(T7Dst, Prng1, Prng2, PublicKey);
  Monolith1Loop(T7Dst);

  //Monolith8: src=72 bytes (t7_dst) -> dst=60 significant bytes (72-byte buffer, matching
  //the reference's oversized "m8_buf[20:]" slice - only the first 60 bytes are written).
  SetLength(M8Out, 72); FillChar(M8Out[0], 72, 0);
  S7PlusHarpoMonolith8.Execute(T7Dst, M8Out);

  //Transform13 output -> m11_src[0x3C:0x3C+24]; transform1 data -> m11_src[0:0x3C].
  SetLength(M11Src, $1E*4); FillChar(M11Src[0], Length(M11Src), 0);
  Move(Transform1[0], M11Src[0], $3C);
  SetLength(T13Out, $1E*4-$3C); FillChar(T13Out[0], Length(T13Out), 0);
  Transform13Execute(T13Out, M8Out);
  Move(T13Out[0], M11Src[$3C], Length(T13Out));

  //Monolith11: src=120 bytes -> dst=20 significant bytes (92-byte buffer, matching the
  //reference's oversized "m8_buf" reuse as both Monolith8's and Monolith11's destination).
  SetLength(M11Dst, 92); FillChar(M11Dst[0], 92, 0);
  Move(M8Out[0], M11Dst[20], 72);
  S7PlusHarpoMonolith11.Execute(M11Src, M11Dst);

  Move(M11Dst[0], Destination[0], $14);
end;

end.
