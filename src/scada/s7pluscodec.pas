{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Codificação/decodificação das mensagens do protocolo S7CommPlus.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Portado de python-snap7/s7commplus/codec.py (referência: thomas-v2/S7CommPlusDriver/Core/S7p.cs).
}
{$ELSE}
{:
  @abstract(S7CommPlus message encoding/decoding.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Ported from python-snap7/s7commplus/codec.py (reference: thomas-v2/S7CommPlusDriver/Core/S7p.cs).
}
{$ENDIF}
unit S7PlusCodec;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, S7PlusTypes, S7PlusVLQ;

type
  TS7PlusResponseHeader = record
    Opcode:Byte;
    FunctionCode:Word;
    SequenceNumber:Word;
    SessionId:Cardinal;
    TransportFlags:Byte;
  end;

  //: Result of TS7PlusItemAddress encoding.
  TS7PlusItemAddress = record
    Data:TBytes;
    FieldCount:Integer; //: SymbolCrc + AccessArea + NumLIDs + AccessSubArea + LIDs
  end;

//-- Frame header ($72 <ver> <len:2>) -----------------------------------------
function EncodeS7PlusHeader(Version:Byte; DataLen:Word):TBytes;
function DecodeS7PlusHeader(const Data:TBytes; Offset:Integer; out Version:Byte; out DataLen:Word):Integer;

//-- Request/response headers --------------------------------------------------
//: 14-byte request header (Opcode+Reserved+FuncCode+Reserved+Seq+SessionId+TransportFlags).
function EncodeRequestHeader(FunctionCode:Word; SequenceNumber:Word; SessionId:Cardinal=0; TransportFlags:Byte=$36):TBytes;
//: 10-byte response header (Opcode+Reserved+FuncCode+Reserved+Seq+TransportFlags, no SessionId).
function DecodeResponseHeader(const Data:TBytes; Offset:Integer):TS7PlusResponseHeader;

//-- Fixed-width big-endian primitives ------------------------------------------
function EncodeUInt8(Value:Byte):TBytes;
function EncodeUInt16(Value:Word):TBytes;
function DecodeUInt16(const Data:TBytes; Offset:Integer):Word;
function EncodeUInt32(Value:Cardinal):TBytes;
function DecodeUInt32(const Data:TBytes; Offset:Integer):Cardinal;
function EncodeUInt64(Value:QWord):TBytes;
function DecodeUInt64(const Data:TBytes; Offset:Integer):QWord;
function EncodeFloat32(Value:Single):TBytes;
function DecodeFloat32(const Data:TBytes; Offset:Integer):Single;
function EncodeFloat64(Value:Double):TBytes;
function DecodeFloat64(const Data:TBytes; Offset:Integer):Double;

//-- Typed value / object model helpers -----------------------------------------

//: Prepends the DataType byte and encodes Value accordingly (no leading flags byte).
function EncodeTypedValueUInt(DataType:Byte; Value:QWord):TBytes;
function EncodeTypedValueRID(Value:Cardinal):TBytes;

//: Fixed ObjectQualifier structure appended to GetMultiVariables/SetMultiVariables requests.
function EncodeObjectQualifier:TBytes;

//: Encodes an ItemAddress (variable address) used by GetMultiVariables/SetMultiVariables.
function EncodeItemAddress(AccessArea, AccessSubArea:Cardinal; const Lids:array of Cardinal; SymbolCrc:Cardinal=0):TS7PlusItemAddress;

//: Encodes raw bytes as a BLOB PValue: [flags=$00][datatype=BLOB][len:VLQ][data].
function EncodePValueBlob(const Data:TBytes):TBytes;
//: Encodes raw bytes as a plain BYTE array PValue (Flags=$10/array, DataType=Byte,
//: VLQ(count), raw bytes) - matches the reference's ValueByteArray.Serialize(). Used as the
//: fallback for write values whose SoftDataType isn't a plain scalar (arrays, strings,
//: structs, or SoftDataType=0/unknown).
function EncodePValueByteArray(const Data:TBytes):TBytes;
//: Encodes a write value as its own type-specific scalar PValue (e.g. ValueDInt, ValueReal),
//: matching the target variable's SoftDataType (as returned by BrowseDB) - required for
//: SetMultiVariables writes to be accepted at all: a generic byte-array/blob value is
//: rejected by the PLC regardless of session/legitimation state, confirmed against real
//: hardware. RawData is the tag's raw big-endian byte buffer (same layout ReadSymbolic
//: returns). Falls back to EncodePValueByteArray for any SoftDataType without a known
//: scalar mapping (arrays, strings, structs, SoftDataType=0).
//: SoftDataType values (see TS7PlusSoftDataType in S7PlusTypeInfo - duplicated here as
//: raw numbers to avoid a circular unit dependency, since S7PlusTypeInfo already uses
//: S7PlusCodec): 1=BOOL 2=BYTE 3=CHAR 4=WORD 5=INT 6=DWORD 7=DINT 8=REAL 48=LREAL
//: 49=ULINT 50=LINT 51=LWORD 52=USINT 53=UINT 54=UDINT 55=SINT.
function EncodeTypedWriteValue(SoftDataType:Byte; const RawData:TBytes):TBytes;

//-- Small scalar PValue builders (Core/PValue.cs's ValueBool/ValueUSInt/.../ValueWString),
//-- used to build ad-hoc object attributes (e.g. Subscription) rather than tag write values.
function EncodeValuePBool(V:Boolean):TBytes;
function EncodeValuePUSInt(V:Byte):TBytes;
function EncodeValuePUInt(V:Word):TBytes;
function EncodeValuePInt(V:SmallInt):TBytes;
function EncodeValuePUDInt(V:Cardinal):TBytes;
function EncodeValuePLInt(V:Int64):TBytes;
//: UTF-16BE WSTRING PValue (matches the protocol's big-endian convention elsewhere).
function EncodeValuePWString(const S:UnicodeString):TBytes;
//: Array-of-UDInt PValue with a caller-chosen Flags byte (the reference reuses this same
//: PValue shape with Flags=$20 - "Addressarray" - for GetVarSubstreamedRequest's address
//: and for Subscription's SubscriptionReferenceList, not just the usual Flags=$10 array).
function EncodeValuePUDIntArray(const Values:array of Cardinal; Flags:Byte):TBytes;

//: Encodes a generic tagged object (Core/PObject.cs's PObject.Serialize()): StartOfObject,
//: RelationId (fixed UInt32), ClassId/ClassFlags/AttributeId (VLQ), each attribute as
//: [Attribute tag][AttrId VLQ][already-encoded PValue bytes], TerminatingObject. Used to
//: build CreateObject request bodies for objects other than the session itself (e.g. a
//: Subscription).
type
  TS7PlusPObjectAttribute = record
    AttrId:Cardinal;
    Value:TBytes; //an already-encoded PValue (e.g. from one of the EncodeValueP* helpers)
  end;
  TS7PlusPObjectAttributeArray = array of TS7PlusPObjectAttribute;
function EncodePObject(RelationId, ClassId, ClassFlags, AttributeId:Cardinal; const Attributes:TS7PlusPObjectAttributeArray):TBytes;

//: Decodes a PValue (flags+datatype+value) to its raw big-endian bytes, regardless of type.
function DecodePValueToBytes(const Data:TBytes; Offset:Integer; out Consumed:Integer):TBytes;

//: Skips over a typed value (best-effort), returning the offset just past it.
function SkipTypedValue(const Data:TBytes; Offset:Integer; DataType, Flags:Byte):Integer;

//: Parses a CreateObject response body (after the 10-byte response header).
//: Returns the (first, usable) session id and the offset just past the ObjectIds list.
function ParseCreateObjectSessionId(const Body:TBytes; out SessionId:Cardinal; out BodyOffset:Integer; out ReturnValue:QWord):Boolean;

//: Scans a CreateObject response payload for the ServerSessionVersion (306) attribute.
//: Returns the raw typed value (flags+datatype+data) to echo back verbatim, or nil if absent.
function ParseServerSessionVersion(const Payload:TBytes):TBytes;

implementation

function BytesConcat(const A, B:TBytes):TBytes;
begin
  SetLength(Result, Length(A)+Length(B));
  if Length(A)>0 then Move(A[0], Result[0], Length(A));
  if Length(B)>0 then Move(B[0], Result[Length(A)], Length(B));
end;

function BytesOf(const B:array of Byte):TBytes;
begin
  SetLength(Result, Length(B));
  if Length(B)>0 then
    Move(B[0], Result[0], Length(B));
end;

function EncodeS7PlusHeader(Version:Byte; DataLen:Word):TBytes;
begin
  SetLength(Result, 4);
  Result[0] := S7Plus_PROTOCOL_ID;
  Result[1] := Version;
  Result[2] := Hi(DataLen);
  Result[3] := Lo(DataLen);
end;

function DecodeS7PlusHeader(const Data:TBytes; Offset:Integer; out Version:Byte; out DataLen:Word):Integer;
begin
  if (Length(Data)-Offset)<4 then
    raise Exception.Create('S7PlusCodec: not enough data for S7CommPlus header');
  if Data[Offset]<>S7Plus_PROTOCOL_ID then
    raise Exception.CreateFmt('S7PlusCodec: invalid protocol id: $%.2x, expected $%.2x',[Data[Offset], S7Plus_PROTOCOL_ID]);
  Version := Data[Offset+1];
  DataLen := (Data[Offset+2] shl 8) or Data[Offset+3];
  Result := 4;
end;

function EncodeRequestHeader(FunctionCode:Word; SequenceNumber:Word; SessionId:Cardinal; TransportFlags:Byte):TBytes;
begin
  SetLength(Result, 14);
  Result[0] := S7PlusOpcode_Request;
  Result[1] := 0; Result[2] := 0; //reserved
  Result[3] := Hi(FunctionCode); Result[4] := Lo(FunctionCode);
  Result[5] := 0; Result[6] := 0; //reserved
  Result[7] := Hi(SequenceNumber); Result[8] := Lo(SequenceNumber);
  Result[9]  := (SessionId shr 24) and $FF;
  Result[10] := (SessionId shr 16) and $FF;
  Result[11] := (SessionId shr  8) and $FF;
  Result[12] :=  SessionId         and $FF;
  Result[13] := TransportFlags;
end;

function DecodeResponseHeader(const Data:TBytes; Offset:Integer):TS7PlusResponseHeader;
begin
  if (Length(Data)-Offset)<10 then
    raise Exception.Create('S7PlusCodec: not enough data for response header');
  Result.Opcode := Data[Offset];
  Result.FunctionCode := (Data[Offset+3] shl 8) or Data[Offset+4];
  Result.SequenceNumber := (Data[Offset+7] shl 8) or Data[Offset+8];
  Result.SessionId := 0;
  Result.TransportFlags := Data[Offset+9];
end;

function EncodeUInt8(Value:Byte):TBytes;
begin
  SetLength(Result,1);
  Result[0] := Value;
end;

function EncodeUInt16(Value:Word):TBytes;
begin
  SetLength(Result,2);
  Result[0] := Hi(Value);
  Result[1] := Lo(Value);
end;

function DecodeUInt16(const Data:TBytes; Offset:Integer):Word;
begin
  Result := (Data[Offset] shl 8) or Data[Offset+1];
end;

function EncodeUInt32(Value:Cardinal):TBytes;
begin
  SetLength(Result,4);
  Result[0] := (Value shr 24) and $FF;
  Result[1] := (Value shr 16) and $FF;
  Result[2] := (Value shr  8) and $FF;
  Result[3] :=  Value         and $FF;
end;

function DecodeUInt32(const Data:TBytes; Offset:Integer):Cardinal;
begin
  Result := (Cardinal(Data[Offset]) shl 24) or (Cardinal(Data[Offset+1]) shl 16) or
            (Cardinal(Data[Offset+2]) shl 8) or Cardinal(Data[Offset+3]);
end;

function EncodeUInt64(Value:QWord):TBytes;
var
  i:Integer;
begin
  SetLength(Result,8);
  for i:=0 to 7 do
    Result[i] := (Value shr ((7-i)*8)) and $FF;
end;

function DecodeUInt64(const Data:TBytes; Offset:Integer):QWord;
var
  i:Integer;
begin
  Result := 0;
  for i:=0 to 7 do
    Result := (Result shl 8) or Data[Offset+i];
end;

function EncodeFloat32(Value:Single):TBytes;
var
  d:Cardinal absolute Value;
begin
  Result := EncodeUInt32(d);
end;

function DecodeFloat32(const Data:TBytes; Offset:Integer):Single;
var
  d:Cardinal;
begin
  d := DecodeUInt32(Data, Offset);
  Result := PSingle(@d)^;
end;

function EncodeFloat64(Value:Double):TBytes;
var
  d:QWord absolute Value;
begin
  Result := EncodeUInt64(d);
end;

function DecodeFloat64(const Data:TBytes; Offset:Integer):Double;
var
  d:QWord;
begin
  d := DecodeUInt64(Data, Offset);
  Result := PDouble(@d)^;
end;

function EncodeTypedValueUInt(DataType:Byte; Value:QWord):TBytes;
begin
  case DataType of
    S7PlusType_BOOL:  Result := BytesConcat(EncodeUInt8(DataType), EncodeUInt8(Ord(Value<>0)));
    S7PlusType_USINT,
    S7PlusType_BYTE:  Result := BytesConcat(EncodeUInt8(DataType), EncodeUInt8(Value and $FF));
    S7PlusType_UINT,
    S7PlusType_WORD:  Result := BytesConcat(EncodeUInt8(DataType), EncodeUInt16(Value and $FFFF));
    S7PlusType_UDINT: Result := BytesConcat(EncodeUInt8(DataType), EncodeUInt32VLQ(Value and $FFFFFFFF));
    S7PlusType_DWORD: Result := BytesConcat(EncodeUInt8(DataType), EncodeUInt32(Value and $FFFFFFFF));
    S7PlusType_ULINT: Result := BytesConcat(EncodeUInt8(DataType), EncodeUInt64VLQ(Value));
    S7PlusType_LWORD: Result := BytesConcat(EncodeUInt8(DataType), EncodeUInt64(Value));
  else
    raise Exception.CreateFmt('S7PlusCodec: unsupported DataType for EncodeTypedValueUInt: $%.2x',[DataType]);
  end;
end;

function EncodeTypedValueRID(Value:Cardinal):TBytes;
begin
  Result := BytesConcat(EncodeUInt8(S7PlusType_RID), EncodeUInt32(Value));
end;

function EncodeObjectQualifier:TBytes;
begin
  Result := EncodeUInt32(S7PlusIds_ObjectQualifier);
  //ParentRID = RID(0)
  Result := BytesConcat(Result, EncodeUInt32VLQ(S7PlusIds_ParentRID));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_RID]));
  Result := BytesConcat(Result, EncodeUInt32(0));
  //CompositionAID = AID(0)
  Result := BytesConcat(Result, EncodeUInt32VLQ(S7PlusIds_CompositionAID));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_AID]));
  Result := BytesConcat(Result, EncodeUInt32VLQ(0));
  //KeyQualifier = UDInt(0)
  Result := BytesConcat(Result, EncodeUInt32VLQ(S7PlusIds_KeyQualifier));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_UDINT]));
  Result := BytesConcat(Result, EncodeUInt32VLQ(0));
  //Terminator
  Result := BytesConcat(Result, BytesOf([$00]));
end;

function EncodeItemAddress(AccessArea, AccessSubArea:Cardinal; const Lids:array of Cardinal; SymbolCrc:Cardinal):TS7PlusItemAddress;
var
  i:Integer;
begin
  Result.Data := EncodeUInt32VLQ(SymbolCrc);
  Result.Data := BytesConcat(Result.Data, EncodeUInt32VLQ(AccessArea));
  Result.Data := BytesConcat(Result.Data, EncodeUInt32VLQ(Length(Lids)+1)); //+1 for AccessSubArea
  Result.Data := BytesConcat(Result.Data, EncodeUInt32VLQ(AccessSubArea));
  for i:=0 to High(Lids) do
    Result.Data := BytesConcat(Result.Data, EncodeUInt32VLQ(Lids[i]));
  Result.FieldCount := 4 + Length(Lids);
end;

function EncodePValueBlob(const Data:TBytes):TBytes;
begin
  Result := BytesOf([$00, S7PlusType_BLOB]);
  Result := BytesConcat(Result, EncodeUInt32VLQ(0)); //BlobRootId - 0 (ad-hoc blob, no root object)
  Result := BytesConcat(Result, EncodeUInt32VLQ(Length(Data)));
  Result := BytesConcat(Result, Data);
end;

function EncodePValueByteArray(const Data:TBytes):TBytes;
begin
  Result := BytesOf([$10, S7PlusType_BYTE]); //Flags=$10 (array), DataType=Byte
  Result := BytesConcat(Result, EncodeUInt32VLQ(Length(Data)));
  Result := BytesConcat(Result, Data);
end;

function BEBytesToUInt64(const Data:TBytes):QWord;
var
  i:Integer;
begin
  Result := 0;
  for i:=0 to High(Data) do
    Result := (Result shl 8) or Data[i];
end;

function BEBytesToInt64(const Data:TBytes):Int64;
var
  u:QWord;
  Bits:Integer;
begin
  u := BEBytesToUInt64(Data);
  Bits := Length(Data)*8;
  if (Bits>0) and (Bits<64) and ((u and (QWord(1) shl (Bits-1)))<>0) then
    Result := Int64(u) - (Int64(1) shl Bits) //sign-extend
  else
    Result := Int64(u);
end;

function EncodeTypedWriteValue(SoftDataType:Byte; const RawData:TBytes):TBytes;
var
  WireType:Byte;
  Kind:(wkNone, wkRawByte, wkRawFloat, wkUnsignedVLQ, wkSignedVLQ, wkUSIntArray);
begin
  Kind := wkNone;
  case SoftDataType of
    1{sdtBOOL}:  begin WireType := S7PlusType_BOOL;  Kind := wkRawByte; end;
    2{sdtBYTE}:  begin WireType := S7PlusType_BYTE;  Kind := wkRawByte; end;
    3{sdtCHAR}:  begin WireType := S7PlusType_USINT; Kind := wkRawByte; end;
    52{sdtUSINT}:begin WireType := S7PlusType_USINT; Kind := wkRawByte; end;
    55{sdtSINT}: begin WireType := S7PlusType_SINT;  Kind := wkRawByte; end;
    4{sdtWORD}:  begin WireType := S7PlusType_WORD;  Kind := wkUnsignedVLQ; end;
    53{sdtUINT}: begin WireType := S7PlusType_UINT;  Kind := wkUnsignedVLQ; end;
    6{sdtDWORD}: begin WireType := S7PlusType_DWORD; Kind := wkUnsignedVLQ; end;
    54{sdtUDINT}:begin WireType := S7PlusType_UDINT; Kind := wkUnsignedVLQ; end;
    49{sdtULINT}:begin WireType := S7PlusType_ULINT; Kind := wkUnsignedVLQ; end;
    51{sdtLWORD}:begin WireType := S7PlusType_LWORD; Kind := wkUnsignedVLQ; end;
    5{sdtINT}:   begin WireType := S7PlusType_INT;   Kind := wkSignedVLQ; end;
    7{sdtDINT}:  begin WireType := S7PlusType_DINT;  Kind := wkSignedVLQ; end;
    50{sdtLINT}: begin WireType := S7PlusType_LINT;  Kind := wkSignedVLQ; end;
    8{sdtREAL}:  begin WireType := S7PlusType_REAL;  Kind := wkRawFloat; end;
    48{sdtLREAL}:begin WireType := S7PlusType_LREAL; Kind := wkRawFloat; end;
    //TPLCString.StringToArrayOfValues already builds the classic S7 [MaxLen][CurLen][chars]
    //byte sequence itself (confirmed in plcstring.pas), matching the reference's
    //ValueUSIntArray (DataType=USInt) exactly - it just needs the right wire DataType, not
    //the generic Byte-typed fallback EncodePValueByteArray uses.
    19{sdtSTRING}: Kind := wkUSIntArray;
  end;

  case Kind of
    wkRawByte: begin
      Result := BytesOf([$00, WireType]);
      if Length(RawData)>0 then
        Result := BytesConcat(Result, BytesOf([RawData[High(RawData)]]))
      else
        Result := BytesConcat(Result, BytesOf([0]));
    end;
    wkRawFloat: begin
      Result := BytesOf([$00, WireType]);
      Result := BytesConcat(Result, RawData); //IEEE-754 bit pattern, passed through as-is
    end;
    wkUnsignedVLQ: begin
      Result := BytesOf([$00, WireType]);
      if Length(RawData)>4 then
        Result := BytesConcat(Result, EncodeUInt64VLQ(BEBytesToUInt64(RawData)))
      else
        Result := BytesConcat(Result, EncodeUInt32VLQ(Cardinal(BEBytesToUInt64(RawData))));
    end;
    wkSignedVLQ: begin
      Result := BytesOf([$00, WireType]);
      if Length(RawData)>4 then
        Result := BytesConcat(Result, EncodeInt64VLQ(BEBytesToInt64(RawData)))
      else
        Result := BytesConcat(Result, EncodeInt32VLQ(LongInt(BEBytesToInt64(RawData))));
    end;
    wkUSIntArray: begin
      Result := BytesOf([$10, S7PlusType_USINT]);
      Result := BytesConcat(Result, EncodeUInt32VLQ(Length(RawData)));
      Result := BytesConcat(Result, RawData);
    end;
  else
    Result := EncodePValueByteArray(RawData);
  end;
end;

function EncodeValuePBool(V:Boolean):TBytes;
begin
  Result := BytesOf([$00, S7PlusType_BOOL, Ord(V)]);
end;

function EncodeValuePUSInt(V:Byte):TBytes;
begin
  Result := BytesOf([$00, S7PlusType_USINT, V]);
end;

function EncodeValuePUInt(V:Word):TBytes;
begin
  Result := BytesConcat(BytesOf([$00, S7PlusType_UINT]), EncodeUInt32VLQ(V));
end;

function EncodeValuePInt(V:SmallInt):TBytes;
begin
  Result := BytesConcat(BytesOf([$00, S7PlusType_INT]), EncodeInt32VLQ(V));
end;

function EncodeValuePUDInt(V:Cardinal):TBytes;
begin
  Result := BytesConcat(BytesOf([$00, S7PlusType_UDINT]), EncodeUInt32VLQ(V));
end;

function EncodeValuePLInt(V:Int64):TBytes;
begin
  Result := BytesConcat(BytesOf([$00, S7PlusType_LINT]), EncodeInt64VLQ(V));
end;

function EncodeValuePWString(const S:UnicodeString):TBytes;
var
  i:Integer;
  Chars:TBytes;
begin
  SetLength(Chars, Length(S)*2);
  for i:=1 to Length(S) do begin
    Chars[(i-1)*2]   := Hi(Word(S[i]));
    Chars[(i-1)*2+1] := Lo(Word(S[i]));
  end;
  Result := BytesOf([$00, S7PlusType_WSTRING]);
  Result := BytesConcat(Result, EncodeUInt32VLQ(Length(S)));
  Result := BytesConcat(Result, Chars);
end;

function EncodeValuePUDIntArray(const Values:array of Cardinal; Flags:Byte):TBytes;
var
  i:Integer;
begin
  Result := BytesOf([Flags, S7PlusType_UDINT]);
  Result := BytesConcat(Result, EncodeUInt32VLQ(Length(Values)));
  for i:=0 to High(Values) do
    Result := BytesConcat(Result, EncodeUInt32VLQ(Values[i]));
end;

function EncodePObject(RelationId, ClassId, ClassFlags, AttributeId:Cardinal; const Attributes:TS7PlusPObjectAttributeArray):TBytes;
var
  i:Integer;
begin
  Result := BytesOf([S7PlusElement_StartOfObject]);
  Result := BytesConcat(Result, EncodeUInt32(RelationId));
  Result := BytesConcat(Result, EncodeUInt32VLQ(ClassId));
  Result := BytesConcat(Result, EncodeUInt32VLQ(ClassFlags));
  Result := BytesConcat(Result, EncodeUInt32VLQ(AttributeId));
  for i:=0 to High(Attributes) do begin
    Result := BytesConcat(Result, BytesOf([S7PlusElement_Attribute]));
    Result := BytesConcat(Result, EncodeUInt32VLQ(Attributes[i].AttrId));
    Result := BytesConcat(Result, Attributes[i].Value);
  end;
  Result := BytesConcat(Result, BytesOf([S7PlusElement_TerminatingObject]));
end;

function PValueElementSize(DataType:Byte):Integer;
begin
  case DataType of
    S7PlusType_BOOL, S7PlusType_USINT, S7PlusType_BYTE, S7PlusType_SINT: Result := 1;
    S7PlusType_UINT, S7PlusType_WORD, S7PlusType_INT: Result := 2;
    S7PlusType_REAL: Result := 4;
    S7PlusType_LREAL, S7PlusType_TIMESTAMP: Result := 8;
    S7PlusType_RID: Result := 4;
  else
    Result := 0; //variable length (VLQ encoded)
  end;
end;

function DecodePValueToBytes(const Data:TBytes; Offset:Integer; out Consumed:Integer):TBytes;
var
  Flags, DataType:Byte;
  IsArray:Boolean;
  Count, c, ElemSize, i:Integer;
  Len:Cardinal;
  StructId:Cardinal;
  TransportFlags, Key:Cardinal;
  val32:Cardinal;
  val64:QWord;
  sval32:LongInt;
  sval64:Int64;
  TmpResult:TBytes;
begin
  if (Offset+2)>Length(Data) then
    raise Exception.Create('S7PlusCodec: not enough data for PValue header');

  Flags := Data[Offset];
  DataType := Data[Offset+1];
  Consumed := 2;

  IsArray := (Flags and $10)<>0;

  if IsArray then begin
    Count := DecodeUInt32VLQ(Data, Offset+Consumed, c);
    Consumed := Consumed + c;
    ElemSize := PValueElementSize(DataType);
    if ElemSize>0 then begin
      SetLength(Result, Count*ElemSize);
      if Count*ElemSize>0 then
        Move(Data[Offset+Consumed], Result[0], Count*ElemSize);
      Consumed := Consumed + Count*ElemSize;
    end else begin
      SetLength(Result, 0);
      for i:=1 to Count do begin
        val32 := DecodeUInt32VLQ(Data, Offset+Consumed, c);
        Consumed := Consumed + c;
        Result := BytesConcat(Result, EncodeUInt32VLQ(val32));
      end;
    end;
    exit;
  end;

  case DataType of
    S7PlusType_NULL: begin
      SetLength(Result,0);
    end;
    S7PlusType_BOOL,
    S7PlusType_USINT, S7PlusType_BYTE, S7PlusType_SINT: begin
      SetLength(Result,1);
      Result[0] := Data[Offset+Consumed];
      Consumed := Consumed+1;
    end;
    S7PlusType_UINT, S7PlusType_WORD, S7PlusType_INT: begin
      SetLength(Result,2);
      Move(Data[Offset+Consumed], Result[0], 2);
      Consumed := Consumed+2;
    end;
    S7PlusType_UDINT: begin
      val32 := DecodeUInt32VLQ(Data, Offset+Consumed, c);
      Consumed := Consumed+c;
      Result := EncodeUInt32(val32);
    end;
    S7PlusType_DWORD: begin
      SetLength(Result,4);
      Move(Data[Offset+Consumed], Result[0], 4);
      Consumed := Consumed+4;
    end;
    S7PlusType_DINT: begin
      sval32 := DecodeInt32VLQ(Data, Offset+Consumed, c);
      Consumed := Consumed+c;
      Result := EncodeUInt32(Cardinal(sval32));
    end;
    S7PlusType_REAL: begin
      SetLength(Result,4);
      Move(Data[Offset+Consumed], Result[0], 4);
      Consumed := Consumed+4;
    end;
    S7PlusType_LREAL: begin
      SetLength(Result,8);
      Move(Data[Offset+Consumed], Result[0], 8);
      Consumed := Consumed+8;
    end;
    S7PlusType_ULINT: begin
      val64 := DecodeUInt64VLQ(Data, Offset+Consumed, c);
      Consumed := Consumed+c;
      Result := EncodeUInt64(val64);
    end;
    S7PlusType_LWORD: begin
      SetLength(Result,8);
      Move(Data[Offset+Consumed], Result[0], 8);
      Consumed := Consumed+8;
    end;
    S7PlusType_LINT: begin
      sval64 := DecodeInt64VLQ(Data, Offset+Consumed, c);
      Consumed := Consumed+c;
      Result := EncodeUInt64(QWord(sval64));
    end;
    S7PlusType_TIMESTAMP: begin
      SetLength(Result,8);
      Move(Data[Offset+Consumed], Result[0], 8);
      Consumed := Consumed+8;
    end;
    S7PlusType_TIMESPAN: begin
      sval64 := DecodeInt64VLQ(Data, Offset+Consumed, c);
      Consumed := Consumed+c;
      Result := EncodeUInt64(QWord(sval64));
    end;
    S7PlusType_RID: begin
      SetLength(Result,4);
      Move(Data[Offset+Consumed], Result[0], 4);
      Consumed := Consumed+4;
    end;
    S7PlusType_AID: begin
      val32 := DecodeUInt32VLQ(Data, Offset+Consumed, c);
      Consumed := Consumed+c;
      Result := EncodeUInt32(val32);
    end;
    S7PlusType_BLOB: begin
      DecodeUInt32VLQ(Data, Offset+Consumed, c); //BlobRootId - not needed here, just advancing
      Consumed := Consumed+c;
      Len := DecodeUInt32VLQ(Data, Offset+Consumed, c);
      Consumed := Consumed+c;
      SetLength(Result, Len);
      if Len>0 then
        Move(Data[Offset+Consumed], Result[0], Len);
      Consumed := Consumed+Integer(Len);
    end;
    S7PlusType_WSTRING: begin
      Len := DecodeUInt32VLQ(Data, Offset+Consumed, c);
      Consumed := Consumed+c;
      SetLength(Result, Len);
      if Len>0 then
        Move(Data[Offset+Consumed], Result[0], Len);
      Consumed := Consumed+Integer(Len);
    end;
    S7PlusType_STRUCT: begin
      StructId := DecodeUInt32(Data, Offset+Consumed);
      Consumed := Consumed+4;

      if ((StructId>$90000000) and (StructId<$9FFFFFFF)) or
         ((StructId>$02000000) and (StructId<$02FFFFFF)) then begin
        Consumed := Consumed+8; //PackedStructInterfaceTimestamp (fixed UInt64)
        TransportFlags := DecodeUInt32VLQ(Data, Offset+Consumed, c);
        Consumed := Consumed+c;
        Len := DecodeUInt32VLQ(Data, Offset+Consumed, c);
        Consumed := Consumed+c;
        if (TransportFlags and $400)<>0 then begin //Count2Present
          Len := DecodeUInt32VLQ(Data, Offset+Consumed, c);
          Consumed := Consumed+c;
        end;
        SetLength(Result, Len);
        if Len>0 then
          Move(Data[Offset+Consumed], Result[0], Len);
        Consumed := Consumed+Integer(Len);
        exit;
      end;

      //Normal struct: concatenate member values, stopping at the 0 key terminator.
      SetLength(TmpResult,0);
      Key := DecodeUInt32VLQ(Data, Offset+Consumed, c);
      Consumed := Consumed+c;
      while Key>0 do begin
        TmpResult := BytesConcat(TmpResult, DecodePValueToBytes(Data, Offset+Consumed, c));
        Consumed := Consumed+c;
        Key := DecodeUInt32VLQ(Data, Offset+Consumed, c);
        Consumed := Consumed+c;
      end;
      Result := TmpResult;
    end;
  else
    raise Exception.CreateFmt('S7PlusCodec: unsupported PValue datatype: $%.2x',[DataType]);
  end;
end;

function SkipTypedValue(const Data:TBytes; Offset:Integer; DataType, Flags:Byte):Integer;
var
  IsArray:Boolean;
  Count, c, ElemSize, i:Integer;
  Len:Cardinal;
  SubFlags, SubType:Byte;
begin
  Result := Offset;
  IsArray := (Flags and $10)<>0;

  if IsArray then begin
    if Result>=Length(Data) then exit;
    Count := DecodeUInt32VLQ(Data, Result, c);
    Result := Result+c;
    ElemSize := PValueElementSize(DataType);
    if ElemSize>0 then
      Result := Result + Count*ElemSize
    else
      for i:=1 to Count do begin
        if Result>=Length(Data) then break;
        DecodeUInt32VLQ(Data, Result, c);
        Result := Result+c;
      end;
    exit;
  end;

  case DataType of
    S7PlusType_NULL: ;
    S7PlusType_BOOL, S7PlusType_USINT, S7PlusType_BYTE, S7PlusType_SINT:
      Result := Result+1;
    S7PlusType_UINT, S7PlusType_WORD, S7PlusType_INT:
      Result := Result+2;
    S7PlusType_UDINT, S7PlusType_AID, S7PlusType_DINT: begin
      DecodeUInt32VLQ(Data, Result, c);
      Result := Result+c;
    end;
    S7PlusType_DWORD:
      Result := Result+4;
    S7PlusType_ULINT, S7PlusType_LINT: begin
      DecodeUInt64VLQ(Data, Result, c);
      Result := Result+c;
    end;
    S7PlusType_LWORD:
      Result := Result+8;
    S7PlusType_REAL:
      Result := Result+4;
    S7PlusType_LREAL:
      Result := Result+8;
    S7PlusType_TIMESTAMP:
      Result := Result+8;
    S7PlusType_TIMESPAN: begin
      DecodeUInt64VLQ(Data, Result, c);
      Result := Result+c;
    end;
    S7PlusType_RID:
      Result := Result+4;
    S7PlusType_BLOB: begin
      DecodeUInt32VLQ(Data, Result, c); //BlobRootId
      Result := Result+c;
      Len := DecodeUInt32VLQ(Data, Result, c);
      Result := Result+c+Integer(Len);
    end;
    S7PlusType_WSTRING: begin
      Len := DecodeUInt32VLQ(Data, Result, c);
      Result := Result+c+Integer(Len);
    end;
    S7PlusType_STRUCT: begin
      //Normal-mode struct: UInt32 struct-id, then members [VLQ key][typed value],
      //terminated by a $00 list-terminator byte.
      Result := Result+4;
      while Result<Length(Data) do begin
        if Data[Result]=$00 then begin
          inc(Result);
          break;
        end;
        DecodeUInt32VLQ(Data, Result, c); //key
        Result := Result+c;
        if (Result+2)>Length(Data) then break;
        SubFlags := Data[Result];
        SubType := Data[Result+1];
        Result := Result+2;
        Result := SkipTypedValue(Data, Result, SubType, SubFlags);
      end;
    end;
  else
    //unknown type - can't skip reliably.
  end;
end;

function ParseCreateObjectSessionId(const Body:TBytes; out SessionId:Cardinal; out BodyOffset:Integer; out ReturnValue:QWord):Boolean;
var
  c, ObjCount, i:Integer;
  ObjId:Cardinal;
  FirstObjId:Cardinal;
  HasObj:Boolean;
begin
  ReturnValue := DecodeUInt64VLQ(Body, 0, c);
  BodyOffset := c;

  if BodyOffset<Length(Body) then
    ObjCount := Body[BodyOffset]
  else
    ObjCount := 0;
  inc(BodyOffset);

  HasObj := false;
  FirstObjId := 0;
  for i:=1 to ObjCount do begin
    ObjId := DecodeUInt32VLQ(Body, BodyOffset, c);
    BodyOffset := BodyOffset+c;
    if not HasObj then begin
      FirstObjId := ObjId;
      HasObj := true;
    end;
  end;

  SessionId := FirstObjId;
  Result := HasObj;
end;

function ParseServerSessionVersion(const Payload:TBytes):TBytes;
var
  Offset, c, i, ValueStart, EndOff:Integer;
  Tag:Byte;
  AttrId:Cardinal;
  Flags, DataType:Byte;
begin
  Result := nil;
  Offset := 0;
  while Offset<Length(Payload) do begin
    Tag := Payload[Offset];

    if Tag=S7PlusElement_Attribute then begin
      inc(Offset);
      if Offset>=Length(Payload) then break;
      AttrId := DecodeUInt32VLQ(Payload, Offset, c);
      Offset := Offset+c;

      if (Offset+2)>Length(Payload) then break;
      Flags := Payload[Offset];
      DataType := Payload[Offset+1];

      if AttrId=S7PlusObjId_ServerSessionVersion then begin
        ValueStart := Offset;
        EndOff := SkipTypedValue(Payload, Offset+2, DataType, Flags);
        SetLength(Result, EndOff-ValueStart);
        if Length(Result)>0 then
          Move(Payload[ValueStart], Result[0], Length(Result));
        exit;
      end else
        Offset := SkipTypedValue(Payload, Offset+2, DataType, Flags);

    end else if Tag=S7PlusElement_StartOfObject then begin
      inc(Offset);
      if (Offset+4)>Length(Payload) then break;
      Offset := Offset+4; //RelationId (fixed)
      for i:=1 to 3 do begin //ClassId, ClassFlags, AttributeId (each VLQ)
        DecodeUInt32VLQ(Payload, Offset, c);
        Offset := Offset+c;
      end;

    end else if Tag=S7PlusElement_TerminatingObject then
      inc(Offset)
    else if Tag=$00 then
      inc(Offset)
    else
      inc(Offset); //unknown tag - skip
  end;
end;

end.
