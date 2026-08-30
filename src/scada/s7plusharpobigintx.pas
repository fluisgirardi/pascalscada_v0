unit S7PlusHarpoBigIntX;

{$mode objfpc}{$H+}
{$Q-}{$R-}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/big_int_transforms.py - big-integer arithmetic (add/sub/mul/
//: square) on the "prepared" 20-byte operands, mirroring Python's arbitrary-precision
//: int semantics with fixed-width little-endian byte-array arithmetic (our operands
//: never exceed a few dozen bytes, so a generous fixed width has ample headroom).

interface

uses SysUtils, Math, S7PlusHarpoBigInt;

const
  DESTINATION_SIZE = FINALIZE_DESTINATION_SIZE; //6 uints = 24 bytes
  SOURCE_SIZE = PREPARE_SOURCE_SIZE; //6 uints = 24 bytes

procedure BigIntAddition(var Destination:TBytes; const Source1, Source2:TBytes);
procedure BigIntSubtraction(var Destination:TBytes; const Minuend, Subtrahend:TBytes);
procedure BigIntMultiplication(var Destination:TBytes; const Source1, Source2:TBytes);
procedure BigIntSquare(var Destination:TBytes; const Source:TBytes);

implementation

const
  BIGWIDTH = 128; //generous fixed width (bytes) - our real operands never exceed ~40 bytes

//===========================================================================
// Fixed-width unsigned bignum helpers (little-endian, BIGWIDTH bytes)
//===========================================================================

function BigNatFromBytes(const Data:TBytes):TBytes;
var
  i, n:Integer;
begin
  SetLength(Result, BIGWIDTH);
  FillChar(Result[0], BIGWIDTH, 0);
  n := Min(Length(Data), BIGWIDTH);
  for i:=0 to n-1 do Result[i] := Data[i];
end;

//: Highest set bit + 1 (Python's int.bit_length()); 0 if the value is zero.
function BigNatBitLength(const N:TBytes):Integer;
var
  i, b:Integer;
  v:Byte;
begin
  Result := 0;
  for i:=High(N) downto 0 do begin
    if N[i]<>0 then begin
      v := N[i];
      b := 0;
      while v<>0 do begin Inc(b); v := v shr 1; end;
      Result := i*8+b;
      exit;
    end;
  end;
end;

//: Python's `_to_bytes(value, max(1,(value.bit_length()+7)//8))` - the natural minimal
//: unsigned little-endian length.
function BigNatToMinimalBytes(const N:TBytes):TBytes;
var
  Len_:Integer;
begin
  Len_ := Max(1, (BigNatBitLength(N)+7) div 8);
  SetLength(Result, Len_);
  Move(N[0], Result[0], Len_);
end;

function BigNatCompare(const A, B:TBytes):Integer;
var
  i:Integer;
begin
  Result := 0;
  for i:=BIGWIDTH-1 downto 0 do begin
    if A[i]<>B[i] then begin
      if A[i]<B[i] then Result := -1 else Result := 1;
      exit;
    end;
  end;
end;

function BigNatAdd(const A, B:TBytes):TBytes;
var
  i:Integer;
  Carry, Sum_:Word;
begin
  SetLength(Result, BIGWIDTH);
  Carry := 0;
  for i:=0 to BIGWIDTH-1 do begin
    Sum_ := Word(A[i]) + Word(B[i]) + Carry;
    Result[i] := Byte(Sum_ and $FF);
    Carry := Sum_ shr 8;
  end;
  //Overflow beyond BIGWIDTH bytes is not expected for our operand sizes - silently
  //dropped, matching a fixed-width assumption that's always safe here.
end;

//: Assumes A>=B (as unsigned magnitudes) - computes A-B.
function BigNatSub(const A, B:TBytes):TBytes;
var
  i:Integer;
  Borrow:Integer;
  Diff:Integer;
begin
  SetLength(Result, BIGWIDTH);
  Borrow := 0;
  for i:=0 to BIGWIDTH-1 do begin
    Diff := Integer(A[i]) - Integer(B[i]) - Borrow;
    if Diff<0 then begin
      Diff := Diff+256;
      Borrow := 1;
    end else
      Borrow := 0;
    Result[i] := Byte(Diff);
  end;
end;

function BigNatMul(const A, B:TBytes):TBytes;
var
  i, j:Integer;
  Carry, P:QWord;
  Acc:array[0..BIGWIDTH-1] of QWord;
begin
  SetLength(Result, BIGWIDTH);
  for i:=0 to BIGWIDTH-1 do Acc[i] := 0;
  for i:=0 to BIGWIDTH-1 do begin
    if A[i]=0 then continue;
    Carry := 0;
    for j:=0 to BIGWIDTH-1 do begin
      if i+j>=BIGWIDTH then break;
      P := QWord(A[i])*QWord(B[j]) + Acc[i+j] + Carry;
      Acc[i+j] := P and $FF;
      Carry := P shr 8;
    end;
    //propagate remaining carry into higher limbs (dropped past BIGWIDTH, never
    //happens for our operand sizes)
    j := i+BIGWIDTH;
    while (Carry<>0) and (j<BIGWIDTH) do begin
      Acc[j] := Acc[j]+Carry;
      Carry := Acc[j] shr 8;
      Acc[j] := Acc[j] and $FF;
      Inc(j);
    end;
  end;
  for i:=0 to BIGWIDTH-1 do Result[i] := Byte(Acc[i]);
end;

function BigNatIsZero(const N:TBytes):Boolean;
var i:Integer;
begin
  Result := true;
  for i:=0 to High(N) do
    if N[i]<>0 then begin Result := false; exit; end;
end;

//===========================================================================

function Prepared(const Buf:TBytes):TBytes;
var
  Out_:TBytes;
begin
  SetLength(Out_, PREPARE_DESTINATION_SIZE);
  Prepare(Out_, Buf);
  Result := Out_;
end;

//: Mirrors _compress: returns True in NeedsMore if the value (Buf, first Len_ bytes
//: significant) still doesn't fit in FINALIZE_SOURCE_SIZE bytes after folding the
//: overflow*0x2F back in. Buf/Len_ are updated in place (Buf padded to BIGWIDTH).
procedure Compress(var Buf:TBytes; var Len_:Integer; out NeedsMore:Boolean);
var
  Overflow, Compressed, Product, Factor:TBytes;
  NewBytes:TBytes;
  ActualLength, i:Integer;
begin
  NeedsMore := false;
  if Len_<=FINALIZE_SOURCE_SIZE then exit;

  SetLength(Overflow, BIGWIDTH); FillChar(Overflow[0],BIGWIDTH,0);
  Move(Buf[FINALIZE_SOURCE_SIZE], Overflow[0], Len_-FINALIZE_SOURCE_SIZE);

  SetLength(Compressed, BIGWIDTH); FillChar(Compressed[0],BIGWIDTH,0);
  Move(Buf[0], Compressed[0], FINALIZE_SOURCE_SIZE);

  SetLength(Factor, BIGWIDTH); FillChar(Factor[0],BIGWIDTH,0);
  Factor[0] := $2F;

  Product := BigNatAdd(BigNatMul(Overflow, Factor), Compressed);

  //new_bytes = product.to_bytes(length) where length = old Len_ (Python keeps the same
  //buffer width, then rstrips trailing zero bytes for the "actual" length).
  SetLength(NewBytes, Len_);
  Move(Product[0], NewBytes[0], Len_);

  ActualLength := Len_;
  while (ActualLength>1) and (NewBytes[ActualLength-1]=0) do Dec(ActualLength);

  SetLength(Buf, ActualLength);
  for i:=0 to ActualLength-1 do Buf[i] := NewBytes[i];
  Len_ := ActualLength;

  NeedsMore := Len_>FINALIZE_SOURCE_SIZE;
end;

//: Adds 0x2F to the leading uint32 of Buf, in place.
procedure FinalCompress(var Buf:TBytes);
var
  Leading:Cardinal;
begin
  Leading := (Cardinal(Buf[0]) or (Cardinal(Buf[1]) shl 8) or (Cardinal(Buf[2]) shl 16) or (Cardinal(Buf[3]) shl 24));
  Leading := (Leading + $2F) and $FFFFFFFF;
  Buf[0] := Byte(Leading and $FF);
  Buf[1] := Byte((Leading shr 8) and $FF);
  Buf[2] := Byte((Leading shr 16) and $FF);
  Buf[3] := Byte((Leading shr 24) and $FF);
end;

procedure FinalizeCompressed(var Buf:TBytes; Len_:Integer; var Destination:TBytes);
var
  NeedsMore:Boolean;
  Trimmed:TBytes;
begin
  Compress(Buf, Len_, NeedsMore);
  if NeedsMore then begin
    Compress(Buf, Len_, NeedsMore);
    if NeedsMore then
      FinalCompress(Buf);
  end;
  SetLength(Trimmed, Len_);
  Move(Buf[0], Trimmed[0], Len_);
  Finalize_(Destination, Trimmed);
end;

procedure BigIntAddition(var Destination:TBytes; const Source1, Source2:TBytes);
var
  A, B, Sum_, SumMin:TBytes;
  Length_, i:Integer;
  Leading:QWord;
  Words:array[0..3] of Cardinal;
  Carry:Cardinal;
begin
  if Length(Destination)<DESTINATION_SIZE then
    raise Exception.CreateFmt('destination must be at least %d bytes', [DESTINATION_SIZE]);
  if (Length(Source1)<SOURCE_SIZE) or (Length(Source2)<SOURCE_SIZE) then
    raise Exception.CreateFmt('each source must be at least %d bytes', [SOURCE_SIZE]);

  A := BigNatFromBytes(Prepared(Source1));
  B := BigNatFromBytes(Prepared(Source2));
  Sum_ := BigNatAdd(A, B);
  SumMin := BigNatToMinimalBytes(Sum_);
  Length_ := Length(SumMin);

  if Length_>FINALIZE_SOURCE_SIZE then begin
    //C# does an in-place overflow correction on a uint32 view of the leading 16
    //bytes only when length overruns.
    for i:=0 to 3 do
      Words[i] := Cardinal(SumMin[i*4]) or (Cardinal(SumMin[i*4+1]) shl 8) or
                  (Cardinal(SumMin[i*4+2]) shl 16) or (Cardinal(SumMin[i*4+3]) shl 24);
    Words[0] := (Words[0] + $2F) and $FFFFFFFF;
    if Words[0]<$2F then Carry := 1 else Carry := 0;
    if Carry<>0 then begin
      for i:=1 to 3 do begin
        Leading := QWord(Words[i]) + Carry;
        Words[i] := Cardinal(Leading and $FFFFFFFF);
        if Words[i]<Carry then Carry := 1 else Carry := 0;
      end;
      if Carry<>0 then
        Words[0] := (Words[0] + $5E) and $FFFFFFFF;
    end;
    for i:=0 to 3 do begin
      SumMin[i*4]   := Byte(Words[i] and $FF);
      SumMin[i*4+1] := Byte((Words[i] shr 8) and $FF);
      SumMin[i*4+2] := Byte((Words[i] shr 16) and $FF);
      SumMin[i*4+3] := Byte((Words[i] shr 24) and $FF);
    end;
  end;

  Finalize_(Destination, SumMin);
end;

//: Mirrors _signed_byte_count: minimal byte count for Value's signed two's-complement
//: representation. Value/NegMagnitude describe a signed value via (IsNeg, Magnitude) -
//: Magnitude is the absolute value as an unsigned BigNat (BIGWIDTH bytes).
function SignedByteCount(IsNeg:Boolean; const Magnitude:TBytes):Integer;
var
  M:TBytes;
begin
  if BigNatIsZero(Magnitude) then begin Result := 1; exit; end;
  if not IsNeg then
    Result := Max(1, (BigNatBitLength(Magnitude)+8) div 8)
  else begin
    //-value-1, i.e. Magnitude-1 (Magnitude=abs(value)>0 here)
    SetLength(M, BIGWIDTH); FillChar(M[0],BIGWIDTH,0); M[0] := 1;
    M := BigNatSub(Magnitude, M);
    Result := Max(1, (BigNatBitLength(M)+8) div 8);
  end;
end;

//: Encodes a signed value (IsNeg,Magnitude) as Length_ bytes, little-endian two's
//: complement - mirrors Python's `value.to_bytes(length, 'little', signed=True)`.
function SignedToBytes(IsNeg:Boolean; const Magnitude:TBytes; Length_:Integer):TBytes;
var
  Full:TBytes;
  One:TBytes;
begin
  if not IsNeg then begin
    SetLength(Result, Length_);
    Move(Magnitude[0], Result[0], Length_);
  end else begin
    //two's complement: (2^(8*Length_) - Magnitude) truncated to Length_ bytes, i.e.
    //((not Magnitude)+1) truncated - compute via BigNatSub on a BIGWIDTH-wide "zero
    //minus Magnitude" using wraparound (two's complement negation naturally emerges
    //from unsigned subtraction with borrow propagating as 0xFF past the top).
    SetLength(One, BIGWIDTH); FillChar(One[0],BIGWIDTH,0); One[0] := 1;
    Full := BigNatSub(BigNatFromBytes(nil), Magnitude); //0 - Magnitude, wraps mod 2^(8*BIGWIDTH)
    SetLength(Result, Length_);
    Move(Full[0], Result[0], Length_);
  end;
end;

procedure BigIntSubtraction(var Destination:TBytes; const Minuend, Subtrahend:TBytes);
var
  A, B:TBytes;
  Cmp:Integer;
  IsNegative:Boolean;
  Magnitude, Two47:TBytes;
  SignedCount, BufferSize, DiffLength, i:Integer;
  DiffBytes:TBytes;
begin
  if Length(Destination)<DESTINATION_SIZE then
    raise Exception.CreateFmt('destination must be at least %d bytes', [DESTINATION_SIZE]);
  if (Length(Minuend)<SOURCE_SIZE) or (Length(Subtrahend)<SOURCE_SIZE) then
    raise Exception.CreateFmt('both inputs must be at least %d bytes', [SOURCE_SIZE]);

  A := BigNatFromBytes(Prepared(Minuend));
  B := BigNatFromBytes(Prepared(Subtrahend));
  Cmp := BigNatCompare(A, B);

  if Cmp>=0 then begin
    IsNegative := false;
    Magnitude := BigNatSub(A, B);
  end else begin
    IsNegative := true;
    Magnitude := BigNatSub(B, A); //|diff|
    //diff -= 0x2F (diff is negative, so magnitude += 0x2F)
    SetLength(Two47, BIGWIDTH); FillChar(Two47[0],BIGWIDTH,0); Two47[0] := $2F;
    Magnitude := BigNatAdd(Magnitude, Two47);
  end;

  SignedCount := SignedByteCount(IsNegative, Magnitude);
  BufferSize := Max(SignedCount, FINALIZE_SOURCE_SIZE);
  DiffBytes := SignedToBytes(IsNegative, Magnitude, SignedCount);
  SetLength(DiffBytes, BufferSize);
  for i:=SignedCount to BufferSize-1 do DiffBytes[i] := 0; //explicit zero-extend (matches Python's bytearray.extend(b"\x00"*n) - don't rely on SetLength's own zero-init guarantees)
  DiffLength := SignedCount;

  if DiffLength>FINALIZE_SOURCE_SIZE then
    DiffLength := FINALIZE_SOURCE_SIZE
  else if (DiffLength<FINALIZE_SOURCE_SIZE) and IsNegative then begin
    for i:=DiffLength to FINALIZE_SOURCE_SIZE-1 do DiffBytes[i] := $FF;
    DiffLength := FINALIZE_SOURCE_SIZE;
  end;

  SetLength(DiffBytes, DiffLength);
  Finalize_(Destination, DiffBytes);
end;

procedure BigIntMultiplication(var Destination:TBytes; const Source1, Source2:TBytes);
var
  A, B, Product, ProductMin:TBytes;
  Length_:Integer;
begin
  if Length(Destination)<DESTINATION_SIZE then
    raise Exception.CreateFmt('destination must be at least %d bytes', [DESTINATION_SIZE]);
  if (Length(Source1)<SOURCE_SIZE) or (Length(Source2)<SOURCE_SIZE) then
    raise Exception.CreateFmt('each source must be at least %d bytes', [SOURCE_SIZE]);

  A := BigNatFromBytes(Prepared(Source1));
  B := BigNatFromBytes(Prepared(Source2));
  Product := BigNatMul(A, B);
  ProductMin := BigNatToMinimalBytes(Product);
  Length_ := Length(ProductMin);
  FinalizeCompressed(ProductMin, Length_, Destination);
end;

procedure BigIntSquare(var Destination:TBytes; const Source:TBytes);
var
  Base_, Result_, ResultMin:TBytes;
  Length_:Integer;
begin
  if Length(Destination)<DESTINATION_SIZE then
    raise Exception.CreateFmt('destination must be at least %d bytes', [DESTINATION_SIZE]);
  if Length(Source)<SOURCE_SIZE then
    raise Exception.CreateFmt('source must be at least %d bytes', [SOURCE_SIZE]);

  Base_ := BigNatFromBytes(Prepared(Source));
  Result_ := BigNatMul(Base_, Base_);
  ResultMin := BigNatToMinimalBytes(Result_);
  Length_ := Length(ResultMin);
  FinalizeCompressed(ResultMin, Length_, Destination);
end;

end.
