unit S7PlusHarpoTransform12;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/transform12.py - an opcode-driven dispatcher over
//: BigIntTransforms, reading a metadata tape one uint32 at a time.

interface

uses SysUtils, S7PlusHarpoBigIntX, S7PlusHarpoFamily0Data;

const
  CONTEXT_SIZE = 4*894; //3576 bytes
  BIGINT_SLOT_BYTES = 24;

//: Runs Count opcodes starting from metadata word Index, mutating Context in place.
procedure Transform12Execute(var Context:TBytes; Index, Count:Integer);

implementation

function SlotView(const Data:TBytes; Index:Integer):TBytes;
begin
  SetLength(Result, BIGINT_SLOT_BYTES);
  Move(Data[Index*BIGINT_SLOT_BYTES], Result[0], BIGINT_SLOT_BYTES);
end;

procedure Transform12Execute(var Context:TBytes; Index, Count:Integer);
var
  i:Integer;
  WordOffset:Integer;
  MetaWord:Cardinal;
  DstSlot, Src1Slot, Src2Slot, Opcode:Cardinal;
  DstOffset:Integer;
  Src1Buf, Src2Buf, Result_:TBytes;
begin
  if Count=0 then exit;
  if Length(Context)<CONTEXT_SIZE then
    raise Exception.CreateFmt('context must be at least %d bytes, got %d', [CONTEXT_SIZE, Length(Context)]);

  for i:=0 to Count-1 do begin
    WordOffset := (Index+i)*4;
    if WordOffset+4>Length(Transform12Metadata) then
      raise Exception.CreateFmt('metadata index %d out of range', [Index+i]);
    MetaWord := Cardinal(Transform12Metadata[WordOffset]) or (Cardinal(Transform12Metadata[WordOffset+1]) shl 8) or
                (Cardinal(Transform12Metadata[WordOffset+2]) shl 16) or (Cardinal(Transform12Metadata[WordOffset+3]) shl 24);

    DstSlot := (MetaWord shr $16) and $FF;
    Src1Slot := (MetaWord shr $0B) and $3FF;
    Src2Slot := MetaWord and $3FF;
    Opcode := MetaWord shr $1E;

    DstOffset := Integer(DstSlot)*BIGINT_SLOT_BYTES;

    if Src1Slot<$100 then Src1Buf := SlotView(Context, Src1Slot)
    else Src1Buf := SlotView(Transform12BigIntData, Integer(Src1Slot)-$100);
    if Src2Slot<$100 then Src2Buf := SlotView(Context, Src2Slot)
    else Src2Buf := SlotView(Transform12BigIntData, Integer(Src2Slot)-$100);

    SetLength(Result_, BIGINT_SLOT_BYTES);
    case Opcode of
      0: BigIntMultiplication(Result_, Src1Buf, Src2Buf);
      1: BigIntSquare(Result_, Src1Buf);
      2: BigIntAddition(Result_, Src1Buf, Src2Buf);
    else
      BigIntSubtraction(Result_, Src1Buf, Src2Buf);
    end;

    Move(Result_[0], Context[DstOffset], BIGINT_SLOT_BYTES);
  end;
end;

end.
