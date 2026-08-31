unit S7PlusHarpoLegitimate;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/legitimate.py - the "LegitimateScheme" that solves the post-SessionKey
//: legitimation challenge (password authentication) V1-initial PLCs require after
//: SetupSession's SecurityKey blob is accepted, before they'll allow data operations.

interface

uses SysUtils, S7PlusSSL, S7PlusHarpoKeys, S7PlusHarpoAuth, S7PlusHarpoBlobMeta,
     S7PlusHarpoAuthenticator, S7PlusHarpoKeyDeriv;

const
  OUTPUT_BLOB_LENGTH_REAL_PLC = 180+68; //: 248 bytes.

//: Challenge is the 20-byte legitimation challenge (read from address 303 after
//: SetupSession succeeds); PublicKey is the PLC's public key; SessionKey is the 24-byte
//: key AuthenticateRealPlc returned. Returns the 248-byte blob to write to address 1846.
function SolveLegitimateChallengeRealPlc(const Challenge, PublicKey:TBytes;
                                          Family:TS7PlusHarpoKeyFamily;
                                          const SessionKey:TBytes;
                                          const Password:AnsiString):TBytes;

implementation

function StrToBytesLatin1(const S:AnsiString):TBytes;
begin
  SetLength(Result, Length(S));
  if Length(S)>0 then Move(S[1], Result[0], Length(S));
end;

procedure PutU32LE(var Buf:TBytes; Off:Integer; V:Cardinal);
begin
  Buf[Off]   := Byte(V and $FF);
  Buf[Off+1] := Byte((V shr 8) and $FF);
  Buf[Off+2] := Byte((V shr 16) and $FF);
  Buf[Off+3] := Byte((V shr 24) and $FF);
end;

procedure WriteFragmentMetadata(var Buf:TBytes; Offset, Index, Length_:Integer);
begin
  PutU32LE(Buf, Offset,   $DEADBEEF);
  PutU32LE(Buf, Offset+4, Cardinal(Index));
  PutU32LE(Buf, Offset+8, Cardinal(Length_));
end;

procedure WriteSeedBeefMetadata(var Buf:TBytes; const PublicKey:TBytes;
                                 Family:TS7PlusHarpoKeyFamily; const SymmetricKey:TBytes);
const
  SeedFragLen = $40+$3C; //: BEEF_SEED_METADATA_LENGTH + encrypted seed length.
var
  KeyId:TBytes;
  i:Integer;
begin
  PutU32LE(Buf, 0, $DEADBEEF);
  PutU32LE(Buf, 4, SeedFragLen);
  PutU32LE(Buf, 8, 1);
  PutU32LE(Buf, 12, 2);
  Buf[$15] := $04;

  KeyId := DeriveKeyId(PublicKey);
  for i:=0 to 7 do Buf[$1C+i] := KeyId[i];

  PutU32LE(Buf, $24, S7PlusHarpoBlobMeta.GetPublicKeyFlags(Family));
  PutU32LE(Buf, $28, 0);

  KeyId := DeriveKeyId(SymmetricKey);
  for i:=0 to 7 do Buf[$2C+i] := KeyId[i];

  PutU32LE(Buf, $34, 1); //: symmetric key flags for legitimation - always 1 for real PLCs.
  PutU32LE(Buf, $38, 0);

  PutU32LE(Buf, $3C, $3C); //: encrypted seed length.
end;

function SolveLegitimateChallengeRealPlc(const Challenge, PublicKey:TBytes;
                                          Family:TS7PlusHarpoKeyFamily;
                                          const SessionKey:TBytes;
                                          const Password:AnsiString):TBytes;
var
  PasswordHash, ChallengeKey, Key2, ZeroChallenge:TBytes;
  Auth:TS7PlusHarpoAuthenticator;
  Offset_, Leftover:Integer;
begin
  PasswordHash := S7PlusSHA1(StrToBytesLatin1(Password));
  ChallengeKey := DeriveLegitimationChallengeKey(SessionKey);

  SetLength(Key2, 20+20);
  Move(PasswordHash[0], Key2[0], 20);
  Move(Challenge[0], Key2[20], 20);

  SetLength(Result, OUTPUT_BLOB_LENGTH_REAL_PLC);
  FillChar(Result[0], Length(Result), 0);

  WriteSeedBeefMetadata(Result, PublicKey, Family, ChallengeKey);

  Offset_ := $40;
  Auth := TS7PlusHarpoAuthenticator.Create(ChallengeKey, Key2);
  try
    Inc(Offset_, Auth.WriteSeed(Result, Offset_, PublicKey));

    WriteFragmentMetadata(Result, Offset_, 0, $10+$30);
    Inc(Offset_, 12);

    SetLength(ZeroChallenge, 20);
    FillChar(ZeroChallenge[0], 20, 0);
    Inc(Offset_, Auth.EncryptFullBlocks(Result, Offset_, ZeroChallenge));

    Leftover := Auth.Key2LeftoverLength;
    WriteFragmentMetadata(Result, Offset_, 1, Leftover+16);
    Inc(Offset_, 12);
    Inc(Offset_, Auth.EncryptFinalBlock(Result, Offset_));

    WriteFragmentMetadata(Result, Offset_, 2, 0);
  finally
    Auth.Free;
  end;
end;

end.
