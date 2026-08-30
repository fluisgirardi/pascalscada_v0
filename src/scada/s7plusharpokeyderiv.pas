unit S7PlusHarpoKeyDeriv;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/key_derivation.py - SHA-256-based KDFs for the various symmetric keys
//: and IVs the SessionKey handshake feeds into AES-CTR.

interface

uses SysUtils, Math, S7PlusSSL, S7PlusHarpoFingerprint;

//: 16-byte AES-128 key used to encrypt the PLC challenge:
//: SHA-256(RandomKey[:24] || 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 00)[:16].
function DeriveChallengeEncryptionKey(const RandomKey:TBytes):TBytes;
//: 48 bytes (32-byte AES-256 key + 16-byte IV) for encrypting the random seed, via a
//: counter-mode SHA-256 chain over reverse(A2[:32])+A3[:64]+counter-byte.
function DeriveSeedEncryptionKeyAndIV(const A2, A3:TBytes):TBytes;
//: 24-byte key for encrypting the legitimation (password) challenge:
//: SHA-256(SessionKey[:24] || "MISTRUST")[4:28].
function DeriveLegitimationChallengeKey(const SessionKey:TBytes):TBytes;
//: 24-byte session key from Key2 and the PLC challenge:
//: HMAC-SHA256(Key[:24], HarpoFingerprint(Challenge) || Challenge[2:18])[:24].
function DeriveSessionKey(const Key, Challenge:TBytes):TBytes;

implementation

function DeriveChallengeEncryptionKey(const RandomKey:TBytes):TBytes;
const
  Magic:array[0..15] of Byte = (
    $01,$02,$03,$04, $05,$06,$07,$08, $09,$0A,$0B,$0C, $0D,$0E,$0F,$00
  );
var
  Buf, Digest:TBytes;
  i:Integer;
begin
  if Length(RandomKey)<24 then
    raise Exception.CreateFmt('random_key must be at least 24 bytes, got %d', [Length(RandomKey)]);
  SetLength(Buf, 24+16);
  for i:=0 to 23 do Buf[i] := RandomKey[i];
  for i:=0 to 15 do Buf[24+i] := Magic[i];
  Digest := S7PlusSHA256(Buf);
  SetLength(Result, 16);
  for i:=0 to 15 do Result[i] := Digest[i];
end;

function DeriveSeedEncryptionKeyAndIV(const A2, A3:TBytes):TBytes;
var
  A2Reversed:array[0..31] of Byte;
  Buf, Digest:TBytes;
  Offset_, Size_, i:Integer;
begin
  if Length(A2)<32 then
    raise Exception.CreateFmt('a2 must be at least 32 bytes, got %d', [Length(A2)]);
  if Length(A3)<64 then
    raise Exception.CreateFmt('a3 must be at least 64 bytes, got %d', [Length(A3)]);

  for i:=0 to 31 do A2Reversed[i] := A2[31-i];

  SetLength(Result, 48);
  Offset_ := 0;
  while Offset_<48 do begin
    SetLength(Buf, 32+64+1);
    for i:=0 to 31 do Buf[i] := A2Reversed[i];
    for i:=0 to 63 do Buf[32+i] := A3[i];
    Buf[96] := Byte(Offset_);
    Digest := S7PlusSHA256(Buf);
    Size_ := Min($30-Offset_, $20);
    for i:=0 to Size_-1 do Result[Offset_+i] := Digest[i];
    Inc(Offset_, $20);
  end;
end;

function DeriveLegitimationChallengeKey(const SessionKey:TBytes):TBytes;
const
  Magic:array[0..7] of Byte = (Ord('M'),Ord('I'),Ord('S'),Ord('T'),Ord('R'),Ord('U'),Ord('S'),Ord('T'));
var
  Buf, Digest:TBytes;
  i:Integer;
begin
  if Length(SessionKey)<24 then
    raise Exception.CreateFmt('session_key must be at least 24 bytes, got %d', [Length(SessionKey)]);
  SetLength(Buf, 24+8);
  for i:=0 to 23 do Buf[i] := SessionKey[i];
  for i:=0 to 7 do Buf[24+i] := Magic[i];
  Digest := S7PlusSHA256(Buf);
  SetLength(Result, 24);
  for i:=0 to 23 do Result[i] := Digest[4+i];
end;

function DeriveSessionKey(const Key, Challenge:TBytes):TBytes;
var
  Fp, Source, Full:TBytes;
  KeyPrefix:TBytes;
  i:Integer;
begin
  if Length(Key)<24 then
    raise Exception.CreateFmt('key must be at least 24 bytes, got %d', [Length(Key)]);
  if Length(Challenge)<18 then
    raise Exception.CreateFmt('challenge must be at least 18 bytes, got %d', [Length(Challenge)]);

  SetLength(Fp, 8);
  FingerprintChallenge(Fp, Challenge);

  SetLength(Source, 8+16);
  Move(Fp[0], Source[0], 8);
  Move(Challenge[2], Source[8], 16);

  SetLength(KeyPrefix, 24);
  Move(Key[0], KeyPrefix[0], 24);

  Full := S7PlusHMACSHA256(KeyPrefix, Source);
  SetLength(Result, 24);
  for i:=0 to 23 do Result[i] := Full[i];
end;

end.
