unit S7PlusHarpoAesCtr;

{$mode objfpc}{$H+}
{$Q-}{$R-}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/harpo_aes_ctr.py. NOT standard AES-CTR: layers a proprietary
//: HarpoHash-based transform on top of AES-128-ECB to produce ciphertext and a
//: running integrity MAC in one pass. Used to encrypt the SessionKey blob's random
//: seed and challenge.

interface

uses SysUtils, S7PlusSSL, S7PlusHarpoHash;

const
  HARPO_AES_BLOCK_SIZE = 16;
  HARPO_AES_KEY_LENGTH = 16;
  HARPO_LUT_SIZE = 4096;

type
  TS7PlusHarpoAesCtr = class
  private
    FKey:TBytes;
    FCounter, FAes2, FAes3, FAes4, FIvExtension:array[0..15] of Byte;
    FLut:TBytes;
    FVar1, FVar2:Integer;
    procedure IncrementCounter;
  public
    constructor Create(const Key:TBytes);
    //: Sets up the working table and counter from an IV (must be a non-zero multiple
    //: of 16 bytes - 12-byte and partial-tail paths are not implemented, matching
    //: HarpoS7/the python-snap7 port, since the SessionKey handshake never needs them).
    procedure Init(const IV:TBytes);
    //: Encrypts arbitrary-length plaintext, accumulating the running MAC. Output
    //: length matches input length. Call Init again to reset state.
    function EncryptCtr(const Plaintext:TBytes):TBytes;
    //: Finalises the running MAC and emits Length checksum bytes (1..16).
    function CalculateChecksum(Length_:Integer=HARPO_AES_BLOCK_SIZE):TBytes;
    //: Internal counter state, exposed read-only for vector-test parity.
    function GetCounter:TBytes;
    //: Test-only access to internal state (used by CalculateChecksum's own vector
    //: test, which pre-fills state directly instead of running Init/EncryptCtr).
    procedure PokeStateForTest(const Lut:TBytes; const Aes3, Aes2, IvExtension:TBytes; Var1, Var2:Integer);
  end;

implementation

function EncryptEcbBlock(const Key:TBytes; const PT:array of Byte):TBytes;
var
  PTBytes:TBytes;
  i:Integer;
begin
  SetLength(PTBytes, 16);
  for i:=0 to 15 do PTBytes[i] := PT[i];
  Result := S7PlusAES128ECBEncrypt(Key, PTBytes);
end;

constructor TS7PlusHarpoAesCtr.Create(const Key:TBytes);
begin
  inherited Create;
  if Length(Key)<>HARPO_AES_KEY_LENGTH then
    raise Exception.CreateFmt('key must be %d bytes, got %d', [HARPO_AES_KEY_LENGTH, Length(Key)]);
  FKey := Key;
  SetLength(FLut, HARPO_LUT_SIZE);
  FVar1 := 0;
  FVar2 := 0;
end;

procedure TS7PlusHarpoAesCtr.Init(const IV:TBytes);
var
  Zero16:array[0..15] of Byte;
  EncZero, ChunkStart:TBytes;
  i, ChunkOff:Integer;
  IvBitLen:Cardinal;
  Chunk:array[0..15] of Byte;
  IvExtBytes:TBytes;
begin
  if Length(IV)=0 then
    raise Exception.Create('iv must not be empty');
  if Length(IV)=$C then
    raise Exception.Create('12-byte IV path not implemented');
  if (Length(IV) mod HARPO_AES_BLOCK_SIZE)<>0 then
    raise Exception.Create('non-multiple-of-16 IV tail not implemented');

  //1. AES-ECB encrypt 16 zero bytes - the seed that drives working-table generation.
  FillChar(Zero16, 16, 0);
  EncZero := EncryptEcbBlock(FKey, Zero16);
  for i:=0 to 15 do FAes4[i] := EncZero[i];

  //2. Generate the working LUT from that seed.
  SetLength(IvExtBytes, 16);
  for i:=0 to 15 do IvExtBytes[i] := FAes4[i];
  FLut := S7PlusHarpoHash.GenerateLookupTable(IvExtBytes);

  //3. XOR + hash each 16-byte chunk of IV into IvExtension.
  FillChar(FIvExtension, 16, 0);
  ChunkOff := 0;
  while ChunkOff<Length(IV) do begin
    for i:=0 to 15 do Chunk[i] := IV[ChunkOff+i];
    for i:=0 to 15 do FIvExtension[i] := FIvExtension[i] xor Chunk[i];
    SetLength(IvExtBytes, 16);
    for i:=0 to 15 do IvExtBytes[i] := FIvExtension[i];
    IvExtBytes := S7PlusHarpoHash.HashBlock(IvExtBytes, FLut);
    for i:=0 to 15 do FIvExtension[i] := IvExtBytes[i];
    Inc(ChunkOff, HARPO_AES_BLOCK_SIZE);
  end;

  //4. XOR the IV bit-length and a derived high-bits byte into fixed positions, then
  //hash one more time.
  IvBitLen := Cardinal(Length(IV)) shl 3;
  FIvExtension[$F] := FIvExtension[$F] xor Byte(IvBitLen and $FF);
  FIvExtension[$E] := FIvExtension[$E] xor Byte((IvBitLen shr 8) and $FF);
  FIvExtension[$D] := FIvExtension[$D] xor Byte((IvBitLen shr 16) and $FF);
  FIvExtension[$C] := FIvExtension[$C] xor Byte((IvBitLen shr 24) and $FF);
  FIvExtension[$B] := FIvExtension[$B] xor Byte((Cardinal(Length(IV)) shr 29) and $FF);

  SetLength(IvExtBytes, 16);
  for i:=0 to 15 do IvExtBytes[i] := FIvExtension[i];
  IvExtBytes := S7PlusHarpoHash.HashBlock(IvExtBytes, FLut);
  for i:=0 to 15 do FIvExtension[i] := IvExtBytes[i];

  //6. Counter starts as a copy of the finalised IV extension.
  FCounter := FIvExtension;

  //7. Reset the MAC accumulator and byte counters.
  FillChar(FAes3, 16, 0);
  FVar1 := 0;
  FVar2 := 0;
  if False then ChunkStart := nil; //silence unused-var warning
end;

procedure TS7PlusHarpoAesCtr.IncrementCounter;
var
  V2:Integer;
begin
  //Only bytes 0xD..0xF act as a counter - the upper 13 bytes are a fixed nonce.
  V2 := $10;
  while V2>=$D+1 do begin
    Dec(V2);
    FCounter[V2] := (FCounter[V2]+1) and $FF;
    if FCounter[V2]<>0 then break;
  end;
end;

function TS7PlusHarpoAesCtr.EncryptCtr(const Plaintext:TBytes):TBytes;
var
  V1, V3, V4, AesIdx, TailLen, i:Integer;
  CT:Byte;
  Buf16:array[0..15] of Byte;
  Tmp:TBytes;
begin
  SetLength(Result, Length(Plaintext));
  V1 := FVar2 and $F;

  if (FVar2=0) and (FVar1<>0) and ((FVar1 and $F)<>0) then begin
    SetLength(Tmp, 16); for i:=0 to 15 do Tmp[i] := FAes3[i];
    Tmp := S7PlusHarpoHash.HashBlock(Tmp, FLut);
    for i:=0 to 15 do FAes3[i] := Tmp[i];
  end;

  V4 := 0;
  if V1<>0 then begin
    if Length(Plaintext)<>0 then begin
      while True do begin
        if V1>$F then break;
        V3 := (FAes2[V1] xor Plaintext[V4]) and $FF;
        Result[V4] := V3;
        FAes3[V1] := FAes3[V1] xor V3;
        Inc(V1);
        Inc(V4);
        if V4>=Length(Plaintext) then break;
      end;
    end;
    if V1=$10 then begin
      SetLength(Tmp, 16); for i:=0 to 15 do Tmp[i] := FAes3[i];
      Tmp := S7PlusHarpoHash.HashBlock(Tmp, FLut);
      for i:=0 to 15 do FAes3[i] := Tmp[i];
      V1 := 0;
    end;
  end;

  while V4+HARPO_AES_BLOCK_SIZE<=Length(Plaintext) do begin
    IncrementCounter;
    for i:=0 to 15 do Buf16[i] := FCounter[i];
    Tmp := EncryptEcbBlock(FKey, Buf16);
    for i:=0 to 15 do FAes2[i] := Tmp[i];
    for i:=0 to 15 do begin
      CT := (Plaintext[V4+i] xor FAes2[i]) and $FF;
      Result[V4+i] := CT;
      FAes3[i] := FAes3[i] xor CT;
    end;
    SetLength(Tmp, 16); for i:=0 to 15 do Tmp[i] := FAes3[i];
    Tmp := S7PlusHarpoHash.HashBlock(Tmp, FLut);
    for i:=0 to 15 do FAes3[i] := Tmp[i];
    Inc(V4, HARPO_AES_BLOCK_SIZE);
  end;

  if V4<Length(Plaintext) then begin
    IncrementCounter;
    for i:=0 to 15 do Buf16[i] := FCounter[i];
    Tmp := EncryptEcbBlock(FKey, Buf16);
    for i:=0 to 15 do FAes2[i] := Tmp[i];
    TailLen := Length(Plaintext)-V4;
    AesIdx := V1;
    for i:=0 to TailLen-1 do begin
      CT := (FAes2[AesIdx] xor Plaintext[V4+i]) and $FF;
      Result[V4+i] := CT;
      FAes3[AesIdx] := FAes3[AesIdx] xor CT;
      Inc(AesIdx);
    end;
    V4 := Length(Plaintext);
  end;

  Inc(FVar2, V4);
end;

function TS7PlusHarpoAesCtr.CalculateChecksum(Length_:Integer):TBytes;
var
  V1, V2, i:Cardinal;
  Tmp:TBytes;
begin
  if (Length_<1) or (Length_>HARPO_AES_BLOCK_SIZE) then
    raise Exception.CreateFmt('length must be 1..%d, got %d', [HARPO_AES_BLOCK_SIZE, Length_]);

  if (FVar2=0) and (FVar1<>0) and ((FVar1 and $F)<>0) then begin
    SetLength(Tmp, 16); for i:=0 to 15 do Tmp[i] := FAes3[i];
    Tmp := S7PlusHarpoHash.HashBlock(Tmp, FLut);
    for i:=0 to 15 do FAes3[i] := Tmp[i];
  end;
  if (FVar2<>0) and ((FVar2 and $F)<>0) then begin
    SetLength(Tmp, 16); for i:=0 to 15 do Tmp[i] := FAes3[i];
    Tmp := S7PlusHarpoHash.HashBlock(Tmp, FLut);
    for i:=0 to 15 do FAes3[i] := Tmp[i];
  end;

  V1 := (Cardinal(FVar2) shl 3) and $FFFFFFFF;
  for i:=$F downto $C do
    FAes3[i] := FAes3[i] xor Byte((V1 shr ((($F-i)*8))) and $FF);
  FAes3[$B] := FAes3[$B] xor Byte((Cardinal(FVar2) shr 29) and $FF);

  V2 := (Cardinal(FVar1) shl 3) and $FFFFFFFF;
  for i:=7 downto 4 do
    FAes3[i] := FAes3[i] xor Byte((V2 shr (((7-i)*8))) and $FF);
  FAes3[3] := FAes3[3] xor Byte((Cardinal(FVar1) shr 29) and $FF);

  SetLength(Tmp, 16); for i:=0 to 15 do Tmp[i] := FAes3[i];
  Tmp := S7PlusHarpoHash.HashBlock(Tmp, FLut);
  for i:=0 to 15 do FAes3[i] := Tmp[i];

  SetLength(Tmp, 16); for i:=0 to 15 do Tmp[i] := FIvExtension[i];
  Tmp := EncryptEcbBlock(FKey, FIvExtension);
  for i:=0 to 15 do FAes2[i] := Tmp[i];

  SetLength(Result, Length_);
  for i:=0 to Cardinal(Length_)-1 do
    Result[i] := FAes2[i] xor FAes3[i];
end;

function TS7PlusHarpoAesCtr.GetCounter:TBytes;
var i:Integer;
begin
  SetLength(Result, 16);
  for i:=0 to 15 do Result[i] := FCounter[i];
end;

procedure TS7PlusHarpoAesCtr.PokeStateForTest(const Lut:TBytes; const Aes3, Aes2, IvExtension:TBytes; Var1, Var2:Integer);
var i:Integer;
begin
  FLut := Lut;
  for i:=0 to 15 do begin
    FAes3[i] := Aes3[i];
    FAes2[i] := Aes2[i];
    FIvExtension[i] := IvExtension[i];
  end;
  FVar1 := Var1;
  FVar2 := Var2;
end;

end.
