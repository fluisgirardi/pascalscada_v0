unit S7PlusHarpoBlobMeta;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/blob_metadata.py. Produces the leading 48-byte metadata header of the
//: SecurityKeyEncryptedKey blob (180 bytes for real PLCs, 216 for PlcSim) TIA Portal
//: sends as the "SessionKey" value in its session-setup write.

interface

uses SysUtils, S7PlusHarpoKeys, S7PlusHarpoAuth;

const
  ENCRYPTED_BLOB_LENGTH_REAL_PLC = 180;
  ENCRYPTED_BLOB_LENGTH_PLCSIM = 216;

function GetBlobLength(Family:TS7PlusHarpoKeyFamily):Cardinal;
function GetSymmetricKeyFlags(Family:TS7PlusHarpoKeyFamily):Cardinal;
function GetPublicKeyFlags(Family:TS7PlusHarpoKeyFamily):Cardinal;

//: Writes the 48-byte metadata header at the start of Blob (which must already be at
//: least 48 bytes). PublicKey is the PLC's key (40 bytes real PLC / 64 PlcSim, only the
//: first 24 used); SymmetricKey is the 24-byte client-generated session key. Returns
//: the next writable offset (always 48).
function WriteMetadata(var Blob:TBytes; const PublicKey, SymmetricKey:TBytes; Family:TS7PlusHarpoKeyFamily):Integer;

implementation

const
  BLOB_MAGIC = $FEE1DEAD;
  METADATA_LENGTH = 48;

function PublicKeyLengthByFamily(Family:TS7PlusHarpoKeyFamily):Cardinal;
begin
  if Family=hkfPLCSIM then Result := PUBLIC_KEY_LENGTH_PLCSIM
  else Result := PUBLIC_KEY_LENGTH_REAL_PLC;
end;

function GetBlobLength(Family:TS7PlusHarpoKeyFamily):Cardinal;
begin
  if Family=hkfPLCSIM then Result := ENCRYPTED_BLOB_LENGTH_PLCSIM
  else Result := ENCRYPTED_BLOB_LENGTH_REAL_PLC;
end;

function GetSymmetricKeyFlags(Family:TS7PlusHarpoKeyFamily):Cardinal;
begin
  case Family of
    hkfS7_1500: Result := $001;
    hkfS7_1200: Result := $101;
    hkfPLCSIM:  Result := $301;
  else Result := 0; end;
end;

function GetPublicKeyFlags(Family:TS7PlusHarpoKeyFamily):Cardinal;
begin
  case Family of
    hkfS7_1500: Result := $010;
    hkfS7_1200: Result := $110;
    hkfPLCSIM:  Result := $310;
  else Result := 0; end;
end;

procedure PutU32LE(var Buf:TBytes; Off:Integer; V:Cardinal);
begin
  Buf[Off]   := Byte(V and $FF);
  Buf[Off+1] := Byte((V shr 8) and $FF);
  Buf[Off+2] := Byte((V shr 16) and $FF);
  Buf[Off+3] := Byte((V shr 24) and $FF);
end;

function WriteMetadata(var Blob:TBytes; const PublicKey, SymmetricKey:TBytes; Family:TS7PlusHarpoKeyFamily):Integer;
var
  ExpectedPubkeyLength, BlobLength_, SymFlags, PubFlags:Cardinal;
  KeyIdBytes:TBytes;
  i:Integer;
begin
  if Length(Blob)<METADATA_LENGTH then
    raise Exception.CreateFmt('blob must be at least %d bytes, got %d', [METADATA_LENGTH, Length(Blob)]);

  ExpectedPubkeyLength := PublicKeyLengthByFamily(Family);
  if Cardinal(Length(PublicKey))<ExpectedPubkeyLength then
    raise Exception.CreateFmt('public_key must be at least %d bytes for this family, got %d',
                               [ExpectedPubkeyLength, Length(PublicKey)]);

  BlobLength_ := GetBlobLength(Family);
  SymFlags := GetSymmetricKeyFlags(Family);
  PubFlags := GetPublicKeyFlags(Family);

  PutU32LE(Blob, 0, BLOB_MAGIC);
  PutU32LE(Blob, 4, BlobLength_);
  PutU32LE(Blob, 8, 1);
  PutU32LE(Blob, 12, 1);

  KeyIdBytes := DeriveKeyId(SymmetricKey);
  for i:=0 to KEY_ID_LENGTH-1 do Blob[16+i] := KeyIdBytes[i];
  PutU32LE(Blob, 24, SymFlags);
  PutU32LE(Blob, 28, 0);

  KeyIdBytes := DeriveKeyId(PublicKey);
  for i:=0 to KEY_ID_LENGTH-1 do Blob[32+i] := KeyIdBytes[i];
  PutU32LE(Blob, 40, PubFlags);
  PutU32LE(Blob, 44, 0);

  Result := METADATA_LENGTH;
end;

end.
