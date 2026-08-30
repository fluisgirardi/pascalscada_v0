unit S7PlusHarpoAuth;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's session_auth/utils.py.

interface

uses SysUtils, S7PlusSSL;

const
  KEY_ID_LENGTH = 8;

//: Computes the 8-byte key fingerprint used in the encrypted key blob (HarpoS7's
//: "DeriveKeyId"): SHA-256 over the first 24 bytes of Key concatenated with the
//: literal ASCII string "DERIVE", truncated to 8 bytes. Key must be at least 24
//: bytes (longer keys are silently truncated, matching upstream).
function DeriveKeyId(const Key:TBytes):TBytes;

implementation

function DeriveKeyId(const Key:TBytes):TBytes;
const
  MAGIC:array[0..5] of Byte = (Ord('D'),Ord('E'),Ord('R'),Ord('I'),Ord('V'),Ord('E'));
var
  Buf:TBytes;
  Digest:TBytes;
  i:Integer;
begin
  if Length(Key)<24 then
    raise Exception.CreateFmt('key must be at least 24 bytes, got %d', [Length(Key)]);
  SetLength(Buf, 24+6);
  for i:=0 to 23 do Buf[i] := Key[i];
  for i:=0 to 5 do Buf[24+i] := MAGIC[i];
  Digest := S7PlusSHA256(Buf);
  SetLength(Result, KEY_ID_LENGTH);
  for i:=0 to KEY_ID_LENGTH-1 do Result[i] := Digest[i];
end;

end.
