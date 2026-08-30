unit S7PlusHarpoLegacyAuth;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/legacy_auth.py - entry point for the S7CommPlus V1 SessionKey
//: handshake (V1-initial S7-1200/1500 firmware, pre-TLS): builds the 180-byte
//: SecurityKeyEncryptedKey blob and derives the 24-byte session key used for packet
//: integrity (HMAC).

interface

uses SysUtils, S7PlusHarpoBlobMeta, S7PlusHarpoKeys, S7PlusHarpoAuthenticator,
     S7PlusHarpoKeyDeriv;

//: Challenge is the PLC's 20-byte challenge from the CreateObject response;
//: PublicKey is the PLC's 40-byte public key. Returns (Blob, SessionKey) - 180-byte
//: blob and 24-byte key. Only S7-1200/S7-1500 families are supported.
procedure AuthenticateRealPlc(const Challenge, PublicKey:TBytes; Family:TS7PlusHarpoKeyFamily;
                               out Blob, SessionKey:TBytes);

implementation

procedure AuthenticateRealPlc(const Challenge, PublicKey:TBytes; Family:TS7PlusHarpoKeyFamily;
                               out Blob, SessionKey:TBytes);
var
  Auth:TS7PlusHarpoAuthenticator;
  Offset_:Integer;
  Key2:TBytes;
begin
  if not (Family in [hkfS7_1200, hkfS7_1500]) then
    raise Exception.Create('Only S7_1200 and S7_1500 families are supported');

  SetLength(Blob, ENCRYPTED_BLOB_LENGTH_REAL_PLC);
  FillChar(Blob[0], Length(Blob), 0);

  Auth := TS7PlusHarpoAuthenticator.Create;
  try
    Offset_ := Auth.WriteMetadata(Blob, PublicKey, Family);
    Inc(Offset_, Auth.WriteSeed(Blob, Offset_, PublicKey));
    Inc(Offset_, Auth.EncryptFullBlocks(Blob, Offset_, Challenge));
    Auth.EncryptFinalBlock(Blob, Offset_);

    Key2 := Auth.ExtractKey2;
    SessionKey := DeriveSessionKey(Key2, Challenge);
  finally
    Auth.Free;
  end;
end;

end.
