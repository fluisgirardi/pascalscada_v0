unit S7PlusHarpoAuthenticator;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/authenticator.py - RealPlcAuthenticator, which orchestrates
//: PreSeedTransform, SeedTransform, KeyDerivationTransform, LutGenerator,
//: ChecksumTransform, BigIntOperations and AES-ECB to build the 180-byte
//: SecurityKeyEncryptedKey blob.

interface

uses SysUtils, S7PlusSSL, S7PlusHarpoRandom, S7PlusHarpoBlobMeta, S7PlusHarpoKeys,
     S7PlusHarpoFamily0Transforms, S7PlusHarpoLutGen, S7PlusHarpoSeedTransform,
     S7PlusHarpoBigInt;

type
  TS7PlusHarpoAuthenticator = class
  private
    FKey1, FKey2, FIV:TBytes;
    FLookupTable:TBytes;
    FChecksum:TBytes;
    FChallengeKey, FChecksumKey:TBytes;
    FEncryptedBytes:Integer;
    procedure DeriveKeysAndLut(const T1:TBytes);
    function AesEcbEncrypt(const Plaintext:TBytes):TBytes;
    procedure UpdateChecksum(const CtBlock:TBytes);
  public
    //: Key1/Key2/nil - pass nil to auto-generate via HarpoRandomBytes (matches the
    //: Python constructor's `key1: bytes|None = None` optional params).
    constructor Create(Key1:TBytes=nil; Key2:TBytes=nil);
    function Key2LeftoverLength:Integer;
    function WriteMetadata(var Blob:TBytes; const PublicKey:TBytes; Family:TS7PlusHarpoKeyFamily):Integer;
    //: Writes the encrypted seed at Blob[BlobOffset:], returns SeedTransform's
    //: DESTINATION_SIZE (60) as the number of bytes written - mirrors write_seed(blob,
    //: public_key) where the Python caller already sliced Blob to the right starting
    //: offset (bv[offset:]); we pass that offset explicitly instead, since Pascal
    //: dynamic arrays don't have re-based-index slice views.
    function WriteSeed(var Blob:TBytes; BlobOffset:Integer; const PublicKey:TBytes):Integer;
    function EncryptFullBlocks(var Blob:TBytes; BlobOffset:Integer; const Challenge:TBytes):Integer;
    function EncryptFinalBlock(var Blob:TBytes; BlobOffset:Integer):Integer;
    function ExtractKey2:TBytes;
  end;

implementation

constructor TS7PlusHarpoAuthenticator.Create(Key1:TBytes; Key2:TBytes);
begin
  inherited Create;
  if Key2<>nil then FKey2 := Copy(Key2, 0, Length(Key2))
  else FKey2 := HarpoRandomBytes(24);

  if Key1<>nil then FKey1 := Copy(Key1, 0, Length(Key1))
  else FKey1 := HarpoRandomBytes(24);

  FIV := HarpoRandomBytes(16);

  SetLength(FLookupTable, LUTGEN_DESTINATION_SIZE);
  FillChar(FLookupTable[0], Length(FLookupTable), 0);
  SetLength(FChecksum, 16); FillChar(FChecksum[0], 16, 0);
  SetLength(FChallengeKey, 16); FillChar(FChallengeKey[0], 16, 0);
  SetLength(FChecksumKey, 16); FillChar(FChecksumKey[0], 16, 0);
  FEncryptedBytes := 0;
end;

function TS7PlusHarpoAuthenticator.Key2LeftoverLength:Integer;
begin
  Result := Length(FKey2) mod 16;
end;

function TS7PlusHarpoAuthenticator.WriteMetadata(var Blob:TBytes; const PublicKey:TBytes; Family:TS7PlusHarpoKeyFamily):Integer;
begin
  if not (Family in [hkfS7_1200, hkfS7_1500]) then
    raise Exception.Create('family is not supported by this authenticator');
  Result := S7PlusHarpoBlobMeta.WriteMetadata(Blob, PublicKey, FKey2, Family);
end;

procedure TS7PlusHarpoAuthenticator.DeriveKeysAndLut(const T1:TBytes);
var
  KdBuf, KdTail:TBytes;
begin
  SetLength(KdBuf, KEYDERIV_DESTINATION_SIZE);
  KeyDerivationTransformExecute(KdBuf, T1);

  Move(KdBuf[0], FChallengeKey[0], 16);
  Move(KdBuf[16], FChecksumKey[0], 16);

  SetLength(KdTail, Length(KdBuf)-32);
  Move(KdBuf[32], KdTail[0], Length(KdTail));
  LutGeneratorExecute(FLookupTable, KdTail);
end;

function TS7PlusHarpoAuthenticator.WriteSeed(var Blob:TBytes; BlobOffset:Integer; const PublicKey:TBytes):Integer;
var
  T1, SeedOut:TBytes;
begin
  SetLength(T1, PRESEED_DESTINATION_SIZE);
  PreSeedTransformExecute(T1, FKey1);

  SetLength(SeedOut, SEED_TRANSFORM_DESTINATION_SIZE);
  SeedTransformExecute(SeedOut, PublicKey, T1);
  Move(SeedOut[0], Blob[BlobOffset], SEED_TRANSFORM_DESTINATION_SIZE);
  Result := SEED_TRANSFORM_DESTINATION_SIZE;

  DeriveKeysAndLut(T1);

  ChecksumTransformExecute(FChecksum, FIV, FLookupTable);
end;

function TS7PlusHarpoAuthenticator.AesEcbEncrypt(const Plaintext:TBytes):TBytes;
var
  PT16:TBytes;
begin
  SetLength(PT16, 16);
  Move(Plaintext[0], PT16[0], 16);
  Result := S7PlusAES128ECBEncrypt(FChallengeKey, PT16);
end;

procedure TS7PlusHarpoAuthenticator.UpdateChecksum(const CtBlock:TBytes);
var
  i:Integer;
  NewChecksum:TBytes;
begin
  SetLength(NewChecksum, 16);
  for i:=0 to 15 do NewChecksum[i] := FChecksum[i] xor CtBlock[i];
  FChecksum := NewChecksum;
  ChecksumTransformExecute(FChecksum, FChecksum, FLookupTable);
end;

function TS7PlusHarpoAuthenticator.EncryptFullBlocks(var Blob:TBytes; BlobOffset:Integer; const Challenge:TBytes):Integer;
var
  Offset_, i, j, NumBlocks:Integer;
  CtBlock:TBytes;
begin
  Offset_ := 0;

  Move(FIV[0], Blob[BlobOffset+Offset_], 16);
  Inc(Offset_, 16);

  CtBlock := AesEcbEncrypt(FIV);
  for i:=0 to 15 do CtBlock[i] := CtBlock[i] xor Challenge[2+i];
  Move(CtBlock[0], Blob[BlobOffset+Offset_], 16);
  Inc(Offset_, 16);
  Inc(FEncryptedBytes, 16);

  RotateLeft31(FIV);
  UpdateChecksum(CtBlock);

  NumBlocks := Length(FKey2) div 16;
  for j:=0 to NumBlocks-1 do begin
    CtBlock := AesEcbEncrypt(FIV);
    for i:=0 to 15 do CtBlock[i] := CtBlock[i] xor FKey2[j*16+i];
    Move(CtBlock[0], Blob[BlobOffset+Offset_], 16);
    Inc(Offset_, 16);
    Inc(FEncryptedBytes, 16);

    RotateLeft31(FIV);
    UpdateChecksum(CtBlock);
  end;

  Result := Offset_;
end;

function TS7PlusHarpoAuthenticator.EncryptFinalBlock(var Blob:TBytes; BlobOffset:Integer):Integer;
var
  Leftover, LeftoverStart, Offset_, i:Integer;
  CtBlock, CtPadded, EncryptedChecksum:TBytes;
  ChkDwords:array[0..3] of Cardinal;
begin
  Leftover := Key2LeftoverLength;
  LeftoverStart := Length(FKey2)-Leftover;

  CtBlock := AesEcbEncrypt(FIV);
  for i:=0 to Leftover-1 do CtBlock[i] := CtBlock[i] xor FKey2[LeftoverStart+i];

  if Leftover>0 then Move(CtBlock[0], Blob[BlobOffset], Leftover);
  Inc(FEncryptedBytes, Leftover);
  Offset_ := Leftover;

  SetLength(CtPadded, 16); FillChar(CtPadded[0], 16, 0);
  if Leftover>0 then Move(CtBlock[0], CtPadded[0], Leftover);

  UpdateChecksum(CtPadded);

  for i:=0 to 3 do
    ChkDwords[i] := Cardinal(FChecksum[i*4]) or (Cardinal(FChecksum[i*4+1]) shl 8) or
                    (Cardinal(FChecksum[i*4+2]) shl 16) or (Cardinal(FChecksum[i*4+3]) shl 24);
  ChkDwords[3] := ChkDwords[3] xor Cardinal(FEncryptedBytes);
  for i:=0 to 3 do begin
    FChecksum[i*4]   := Byte(ChkDwords[i] and $FF);
    FChecksum[i*4+1] := Byte((ChkDwords[i] shr 8) and $FF);
    FChecksum[i*4+2] := Byte((ChkDwords[i] shr 16) and $FF);
    FChecksum[i*4+3] := Byte((ChkDwords[i] shr 24) and $FF);
  end;

  ChecksumTransformExecute(FChecksum, FChecksum, FLookupTable);

  EncryptedChecksum := S7PlusAES128ECBEncrypt(FChecksumKey, FChecksum);
  Move(EncryptedChecksum[0], Blob[BlobOffset+Offset_], 16);
  Inc(Offset_, 16);

  Result := Offset_;
end;

function TS7PlusHarpoAuthenticator.ExtractKey2:TBytes;
begin
  Result := Copy(FKey2, 0, Length(FKey2));
end;

end.
