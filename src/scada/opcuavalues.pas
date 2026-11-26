{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Conversões entre os valores do OPC UA (UA_Variant) e os valores dos tags do PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um tag numérico (TPLCTagNumber, TPLCBlock) recebe o valor como número, um por elemento:
  o tipo vem do servidor, então nenhum TagType/SwapBytes é necessário. Booleanos viram 0/1,
  DateTime vira um TDateTime na hora local, enumerações viram o seu valor inteiro.

  Um tag de texto (TPLCString) recebe os bytes do texto em UTF-8, um por valor (use
  StringType=stC). Um tag de texto num nó numérico recebe o número escrito como texto.
}
{$ELSE}
{:
  @abstract(Conversions between OPC UA values (UA_Variant) and PascalSCADA tag values.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A numeric tag (TPLCTagNumber, TPLCBlock) gets the value as a number, one per element:
  the type comes from the server, so no TagType/SwapBytes is needed. Booleans become 0/1,
  DateTime becomes a TDateTime in local time, enumerations become their integer value.

  A text tag (TPLCString) gets the bytes of the text in UTF-8, one per value (use
  StringType=stC). A text tag on a numeric node gets the number written as text.
}
{$ENDIF}
unit OPCUAValues;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, Tag, open62541;

type
  //: How the values of a node are handed to the tags.
  TOPCUAValueKind = (ovkNumber, ovkText);

//: True for the builtin types whose values are text (String, ByteString, XmlElement, LocalizedText, QualifiedName, NodeId, Guid).
function OPCUAIsTextType(DataType:PUA_DataType):Boolean;
//: True for the builtin types that are handed to numeric tags.
function OPCUAIsNumericType(DataType:PUA_DataType):Boolean;

//: The builtin type of a node data type: itself for a builtin, Int32 for an enumeration,
//: nil when it is not known (structures and others).
function OPCUABuiltinTypeOf(DataType:PUA_DataType):PUA_DataType;

{:
  Converts a variant to the values of a numeric tag: one per element (scalars give one).
  Returns ioOk, ioEmptyPacket (no value) or ioIllegalValue (a type that is not a number).
}
function OPCUAVariantToNumbers(const V:UA_Variant; out Values:TArrayOfDouble):TProtocolIOResult;

{:
  Converts a variant to the bytes of a text tag (UTF-8, one byte per value). Numbers become
  their text, an array of text values becomes its first element.
}
function OPCUAVariantToText(const V:UA_Variant; out Text:UTF8String):TProtocolIOResult;
function OPCUATextToValues(const Text:UTF8String; Size:Integer):TArrayOfDouble;
function OPCUAValuesToText(const Values:TArrayOfDouble):UTF8String;

{:
  Builds a variant of DataType from the values of a numeric tag: a scalar when AsArray is
  false (only Values[0] is used), an array of Length(Values) elements otherwise.
  The variant owns its data (UA_Variant_clear it). Returns ioIllegalValue when a value does
  not fit in the type.
}
function OPCUANumbersToVariant(DataType:PUA_DataType; AsArray:Boolean; const Values:TArrayOfDouble; out V:UA_Variant):TProtocolIOResult;

//: Builds a variant of DataType (a text type or a number, parsed from the text) from a text.
function OPCUATextToVariant(DataType:PUA_DataType; const Text:UTF8String; out V:UA_Variant):TProtocolIOResult;

//: The TProtocolIOResult of an OPC UA status code (Good and Uncertain are ioOk).
function OPCUAStatusToIOResult(Status:UA_StatusCode):TProtocolIOResult;
//: True for the status codes that mean the connection/session is gone.
function OPCUAIsConnectionLost(Status:UA_StatusCode):Boolean;

{:
  Splits a tag address in the NodeId and the optional index range written after a "|":
  @code('ns=3;s="DB1"."Arr"|2') is element 2, @code('ns=2;i=7|0:3') the first four.
}
procedure OPCUASplitAddress(const Address:AnsiString; out NodeIdText, IndexRange:AnsiString);

implementation

uses
  Math, DateUtils;

//the bytes of an UA_String (UTF-8) as an UTF8String, without code page conversions
function UAToUTF8(const S:UA_String):UTF8String;
var
  raw:RawByteString;
begin
  SetLength(raw, S.length);
  if S.length>0 then
    Move(S.data^, raw[1], S.length);
  SetCodePage(raw, CP_UTF8, false);
  Result := raw;
end;

//the bytes of an UTF8String in an AnsiString, without code page conversions
function UTF8Bytes(const S:UTF8String):AnsiString;
begin
  SetLength(Result, Length(S));
  if Length(S)>0 then
    Move(S[1], Result[1], Length(S));
end;

function TypeIs(DataType:PUA_DataType; Index:Integer):Boolean; inline;
begin
  Result := DataType=@UA_TYPES[Index];
end;

function OPCUAIsTextType(DataType:PUA_DataType):Boolean;
begin
  Result := (DataType<>nil) and
            (TypeIs(DataType, UA_TYPES_STRING) or TypeIs(DataType, UA_TYPES_BYTESTRING) or
             TypeIs(DataType, UA_TYPES_XMLELEMENT) or TypeIs(DataType, UA_TYPES_LOCALIZEDTEXT) or
             TypeIs(DataType, UA_TYPES_QUALIFIEDNAME) or TypeIs(DataType, UA_TYPES_NODEID) or
             TypeIs(DataType, UA_TYPES_GUID));
end;

function OPCUAIsNumericType(DataType:PUA_DataType):Boolean;
begin
  if DataType=nil then exit(false);
  case UA_DataType_typeKind(DataType) of
    UA_DATATYPEKIND_BOOLEAN, UA_DATATYPEKIND_SBYTE, UA_DATATYPEKIND_BYTE,
    UA_DATATYPEKIND_INT16, UA_DATATYPEKIND_UINT16, UA_DATATYPEKIND_INT32,
    UA_DATATYPEKIND_UINT32, UA_DATATYPEKIND_INT64, UA_DATATYPEKIND_UINT64,
    UA_DATATYPEKIND_FLOAT, UA_DATATYPEKIND_DOUBLE, UA_DATATYPEKIND_DATETIME,
    UA_DATATYPEKIND_STATUSCODE, UA_DATATYPEKIND_ENUM:
      Result := true;
    else
      Result := false;
  end;
end;

function OPCUABuiltinTypeOf(DataType:PUA_DataType):PUA_DataType;
begin
  Result := nil;
  if DataType=nil then exit;
  case UA_DataType_typeKind(DataType) of
    UA_DATATYPEKIND_ENUM:
      Result := @UA_TYPES[UA_TYPES_INT32];
    UA_DATATYPEKIND_STRUCTURE, UA_DATATYPEKIND_OPTSTRUCT, UA_DATATYPEKIND_UNION,
    UA_DATATYPEKIND_BITFIELDCLUSTER, UA_DATATYPEKIND_DECIMAL:
      Result := nil;
    else
      Result := DataType;
  end;
end;

function ElementToDouble(DataType:PUA_DataType; P:Pointer; out D:Double):Boolean;
begin
  Result := true;
  case UA_DataType_typeKind(DataType) of
    UA_DATATYPEKIND_BOOLEAN:    D := Ord(PUA_Boolean(P)^ );
    UA_DATATYPEKIND_SBYTE:      D := PUA_SByte(P)^;
    UA_DATATYPEKIND_BYTE:       D := PUA_Byte(P)^;
    UA_DATATYPEKIND_INT16:      D := PUA_Int16(P)^;
    UA_DATATYPEKIND_UINT16:     D := PUA_UInt16(P)^;
    UA_DATATYPEKIND_INT32,
    UA_DATATYPEKIND_ENUM:       D := PUA_Int32(P)^;
    UA_DATATYPEKIND_UINT32,
    UA_DATATYPEKIND_STATUSCODE: D := PUA_UInt32(P)^;
    UA_DATATYPEKIND_INT64:      D := PUA_Int64(P)^;
    UA_DATATYPEKIND_UINT64:     D := PUA_UInt64(P)^;
    UA_DATATYPEKIND_FLOAT:      D := PUA_Float(P)^;
    UA_DATATYPEKIND_DOUBLE:     D := PUA_Double(P)^;
    UA_DATATYPEKIND_DATETIME:   D := UniversalTimeToLocal(UA_DateTimeToDateTime(PUA_DateTime(P)^));
    else begin
      D := 0;
      Result := false;
    end;
  end;
end;

function OPCUAVariantToNumbers(const V:UA_Variant; out Values:TArrayOfDouble):TProtocolIOResult;
var
  i, n:Integer;
  sz:UA_UInt32;
  P:PByte;
begin
  SetLength(Values, 0);
  if (V._type=nil) or (V.data=nil) then exit(ioEmptyPacket);
  if not OPCUAIsNumericType(V._type) then exit(ioIllegalValue);
  if UA_Variant_isScalar(@V) then
    n := 1
  else
    n := V.arrayLength;
  sz := UA_DataType_memSize(V._type);
  SetLength(Values, n);
  P := V.data;
  for i:=0 to n-1 do begin
    ElementToDouble(V._type, P, Values[i]);
    Inc(P, sz);
  end;
  Result := ioOk;
end;

function GuidToText(const G:UA_Guid):AnsiString;
begin
  Result := LowerCase(Format('%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',
            [G.data1, G.data2, G.data3, G.data4[0], G.data4[1], G.data4[2], G.data4[3],
             G.data4[4], G.data4[5], G.data4[6], G.data4[7]]));
end;

function ElementToText(DataType:PUA_DataType; P:Pointer; out Text:UTF8String):Boolean;
var
  D:Double;
  fs:TFormatSettings;
begin
  Result := true;
  if TypeIs(DataType, UA_TYPES_STRING) or TypeIs(DataType, UA_TYPES_BYTESTRING) or TypeIs(DataType, UA_TYPES_XMLELEMENT) then
    Text := UAToUTF8(PUA_String(P)^)
  else if TypeIs(DataType, UA_TYPES_LOCALIZEDTEXT) then
    Text := UAToUTF8(PUA_LocalizedText(P)^.text)
  else if TypeIs(DataType, UA_TYPES_QUALIFIEDNAME) then
    Text := UAToUTF8(PUA_QualifiedName(P)^.name)
  else if TypeIs(DataType, UA_TYPES_NODEID) then
    Text := UTF8String(UA_NodeIdToStr(PUA_NodeId(P)^))
  else if TypeIs(DataType, UA_TYPES_GUID) then
    Text := UTF8String(GuidToText(PUA_Guid(P)^))
  else if TypeIs(DataType, UA_TYPES_DATETIME) then
    Text := UTF8String(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', UniversalTimeToLocal(UA_DateTimeToDateTime(PUA_DateTime(P)^))))
  else if TypeIs(DataType, UA_TYPES_BOOLEAN) then
    Text := UTF8String(BoolToStr(PUA_Boolean(P)^, true))
  else if ElementToDouble(DataType, P, D) then begin
    fs := DefaultFormatSettings;
    fs.DecimalSeparator := '.';
    Text := UTF8String(FloatToStr(D, fs));
  end else begin
    Text := '';
    Result := false;
  end;
end;

function OPCUAVariantToText(const V:UA_Variant; out Text:UTF8String):TProtocolIOResult;
begin
  Text := '';
  if (V._type=nil) then exit(ioEmptyPacket);
  if V.data=nil then exit(ioEmptyPacket);
  if (not UA_Variant_isScalar(@V)) and (V.arrayLength=0) then exit(ioOk);
  if PByte(V.data)=PByte(UA_EMPTY_ARRAY_SENTINEL) then exit(ioOk);
  if ElementToText(V._type, V.data, Text) then
    Result := ioOk
  else
    Result := ioIllegalValue;
end;

function OPCUATextToValues(const Text:UTF8String; Size:Integer):TArrayOfDouble;
var
  i, n:Integer;
begin
  n := Length(Text);
  if Size<=0 then Size := n;
  SetLength(Result, Size);
  for i:=0 to Size-1 do
    if i<n then
      Result[i] := Ord(Text[i+1])
    else
      Result[i] := 0;
end;

function OPCUAValuesToText(const Values:TArrayOfDouble):UTF8String;
var
  i, n:Integer;
  raw:RawByteString;
begin
  //byte by byte, without any code page conversion: the values are UTF-8 already
  n := 0;
  while (n<=High(Values)) and (not IsNan(Values[n])) and ((Trunc(Values[n]) and $FF)<>0) do
    Inc(n);
  SetLength(raw, n);
  for i:=0 to n-1 do
    raw[i+1] := AnsiChar(Trunc(Values[i]) and $FF);
  SetCodePage(raw, CP_UTF8, false);
  Result := raw;
end;

function DoubleToElement(DataType:PUA_DataType; D:Double; P:Pointer):Boolean;
  function InRange(lo, hi:Double):Boolean;
  begin
    Result := (not IsNan(D)) and (D>=lo) and (D<=hi);
  end;
var
  r:Double;
begin
  Result := false;
  if IsNan(D) then exit;
  r := Round(D);
  case UA_DataType_typeKind(DataType) of
    UA_DATATYPEKIND_BOOLEAN: begin PUA_Boolean(P)^ := D<>0; Result := true; end;
    UA_DATATYPEKIND_SBYTE:  if InRange(-128, 127) then begin PUA_SByte(P)^ := Trunc(r); Result := true; end;
    UA_DATATYPEKIND_BYTE:   if InRange(0, 255) then begin PUA_Byte(P)^ := Trunc(r); Result := true; end;
    UA_DATATYPEKIND_INT16:  if InRange(-32768, 32767) then begin PUA_Int16(P)^ := Trunc(r); Result := true; end;
    UA_DATATYPEKIND_UINT16: if InRange(0, 65535) then begin PUA_UInt16(P)^ := Trunc(r); Result := true; end;
    UA_DATATYPEKIND_INT32,
    UA_DATATYPEKIND_ENUM:   if InRange(-2147483648.0, 2147483647.0) then begin PUA_Int32(P)^ := Trunc(r); Result := true; end;
    UA_DATATYPEKIND_UINT32,
    UA_DATATYPEKIND_STATUSCODE:
                            if InRange(0, 4294967295.0) then begin PUA_UInt32(P)^ := Trunc(r); Result := true; end;
    UA_DATATYPEKIND_INT64:  if InRange(-9223372036854775808.0, 9223372036854774784.0) then begin PUA_Int64(P)^ := Trunc(r); Result := true; end;
    UA_DATATYPEKIND_UINT64: if InRange(0, 18446744073709549568.0) then begin PUA_UInt64(P)^ := UInt64(Trunc(r)); Result := true; end;
    UA_DATATYPEKIND_FLOAT:  if IsInfinite(D) or (Abs(D)<=MaxSingle) then begin PUA_Float(P)^ := D; Result := true; end;
    UA_DATATYPEKIND_DOUBLE: begin PUA_Double(P)^ := D; Result := true; end;
    UA_DATATYPEKIND_DATETIME: begin PUA_DateTime(P)^ := UA_DateTimeFromDateTime(LocalTimeToUniversal(D)); Result := true; end;
  end;
end;

function OPCUANumbersToVariant(DataType:PUA_DataType; AsArray:Boolean; const Values:TArrayOfDouble; out V:UA_Variant):TProtocolIOResult;
var
  i, n:Integer;
  sz:UA_UInt32;
  buf, P:PByte;
begin
  UA_Variant_init(V);
  if (DataType=nil) or (not OPCUAIsNumericType(DataType)) then exit(ioIllegalValue);
  if UA_DataType_typeKind(DataType)=UA_DATATYPEKIND_ENUM then
    DataType := @UA_TYPES[UA_TYPES_INT32];
  if AsArray then n := Length(Values) else n := Min(1, Length(Values));
  if n=0 then exit(ioIllegalValue);
  sz := UA_DataType_memSize(DataType);
  buf := UA_Array_new(n, DataType);
  if buf=nil then exit(ioDriverError);
  P := buf;
  for i:=0 to n-1 do begin
    if not DoubleToElement(DataType, Values[i], P) then begin
      UA_Array_delete(buf, n, DataType);
      exit(ioIllegalValue);
    end;
    Inc(P, sz);
  end;
  if AsArray then
    UA_Variant_setArray(@V, buf, n, DataType)
  else
    UA_Variant_setScalar(@V, buf, DataType);
  Result := ioOk;
end;

function OPCUATextToVariant(DataType:PUA_DataType; const Text:UTF8String; out V:UA_Variant):TProtocolIOResult;
var
  tmp:AnsiString;
  s:UA_String;
  lt:UA_LocalizedText;
  qn:UA_QualifiedName;
  id:UA_NodeId;
  d:Double;
  vals:TArrayOfDouble;
  fs:TFormatSettings;
  dt:TDateTime;
begin
  UA_Variant_init(V);
  if DataType=nil then exit(ioIllegalValue);
  tmp := UTF8Bytes(Text);
  if TypeIs(DataType, UA_TYPES_STRING) or TypeIs(DataType, UA_TYPES_BYTESTRING) or TypeIs(DataType, UA_TYPES_XMLELEMENT) then begin
    s := _UA_STRING(tmp);
    Result := OPCUAStatusToIOResult(UA_Variant_setScalarCopy(@V, @s, DataType));
  end else if TypeIs(DataType, UA_TYPES_LOCALIZEDTEXT) then begin
    FillChar(lt, SizeOf(lt), 0);
    lt.text := _UA_STRING(tmp);
    Result := OPCUAStatusToIOResult(UA_Variant_setScalarCopy(@V, @lt, DataType));
  end else if TypeIs(DataType, UA_TYPES_QUALIFIEDNAME) then begin
    FillChar(qn, SizeOf(qn), 0);
    qn.name := _UA_STRING(tmp);
    Result := OPCUAStatusToIOResult(UA_Variant_setScalarCopy(@V, @qn, DataType));
  end else if TypeIs(DataType, UA_TYPES_NODEID) then begin
    if UA_StrToNodeId(tmp, id)<>UA_STATUSCODE_GOOD then exit(ioIllegalValue);
    Result := OPCUAStatusToIOResult(UA_Variant_setScalarCopy(@V, @id, DataType));
    UA_NodeId_clear(id);
  end else if TypeIs(DataType, UA_TYPES_DATETIME) then begin
    if not TryStrToDateTime(tmp, dt) then exit(ioIllegalValue);
    SetLength(vals, 1);
    vals[0] := dt;
    Result := OPCUANumbersToVariant(DataType, false, vals, V);
  end else if TypeIs(DataType, UA_TYPES_BOOLEAN) and (SameText(Trim(tmp),'true') or SameText(Trim(tmp),'false')) then begin
    SetLength(vals, 1);
    vals[0] := Ord(SameText(Trim(tmp),'true'));
    Result := OPCUANumbersToVariant(DataType, false, vals, V);
  end else if OPCUAIsNumericType(DataType) then begin
    fs := DefaultFormatSettings;
    fs.DecimalSeparator := '.';
    if not TryStrToFloat(Trim(tmp), d, fs) then
      if not TryStrToFloat(Trim(tmp), d) then exit(ioIllegalValue);
    SetLength(vals, 1);
    vals[0] := d;
    Result := OPCUANumbersToVariant(DataType, false, vals, V);
  end else
    Result := ioIllegalValue;
end;

function OPCUAIsConnectionLost(Status:UA_StatusCode):Boolean;
begin
  case Status of
    UA_STATUSCODE_BADCONNECTIONCLOSED, UA_STATUSCODE_BADSECURECHANNELCLOSED,
    UA_STATUSCODE_BADSECURECHANNELIDINVALID, UA_STATUSCODE_BADSESSIONCLOSED,
    UA_STATUSCODE_BADSESSIONIDINVALID, UA_STATUSCODE_BADSESSIONNOTACTIVATED,
    UA_STATUSCODE_BADSERVERNOTCONNECTED, UA_STATUSCODE_BADCOMMUNICATIONERROR,
    UA_STATUSCODE_BADDISCONNECT, UA_STATUSCODE_BADCONNECTIONREJECTED,
    UA_STATUSCODE_BADNOTCONNECTED, UA_STATUSCODE_BADSHUTDOWN,
    UA_STATUSCODE_BADSERVERHALTED:
      Result := true;
    else
      Result := false;
  end;
end;

function OPCUAStatusToIOResult(Status:UA_StatusCode):TProtocolIOResult;
begin
  if not UA_StatusCode_isBad(Status) then exit(ioOk);
  case Status and $FFFF0000 of
    UA_STATUSCODE_BADNODEIDUNKNOWN, UA_STATUSCODE_BADNODEIDINVALID,
    UA_STATUSCODE_BADATTRIBUTEIDINVALID, UA_STATUSCODE_BADNOTFOUND,
    UA_STATUSCODE_BADBROWSENAMEINVALID:
      Result := ioObjectNotExists;
    UA_STATUSCODE_BADUSERACCESSDENIED, UA_STATUSCODE_BADNOTREADABLE,
    UA_STATUSCODE_BADNOTWRITABLE, UA_STATUSCODE_BADWRITENOTSUPPORTED,
    UA_STATUSCODE_BADSECURITYMODEINSUFFICIENT:
      Result := ioObjectAccessNotAllowed;
    UA_STATUSCODE_BADTYPEMISMATCH, UA_STATUSCODE_BADOUTOFRANGE,
    UA_STATUSCODE_BADDATATYPEIDUNKNOWN, UA_STATUSCODE_BADDECODINGERROR,
    UA_STATUSCODE_BADENCODINGERROR:
      Result := ioIllegalValue;
    UA_STATUSCODE_BADINDEXRANGEINVALID, UA_STATUSCODE_BADINDEXRANGENODATA:
      Result := ioIllegalMemoryAddress;
    UA_STATUSCODE_BADTIMEOUT, UA_STATUSCODE_BADREQUESTTIMEOUT:
      Result := ioTimeOut;
    UA_STATUSCODE_BADIDENTITYTOKENINVALID, UA_STATUSCODE_BADIDENTITYTOKENREJECTED,
    UA_STATUSCODE_BADUSERSIGNATUREINVALID:
      Result := ioAuthError;
    UA_STATUSCODE_BADSECURITYCHECKSFAILED, UA_STATUSCODE_BADCERTIFICATEINVALID,
    UA_STATUSCODE_BADCERTIFICATEUNTRUSTED, UA_STATUSCODE_BADCERTIFICATEURIINVALID,
    UA_STATUSCODE_BADCERTIFICATETIMEINVALID, UA_STATUSCODE_BADSECURITYPOLICYREJECTED,
    UA_STATUSCODE_BADTCPENDPOINTURLINVALID, UA_STATUSCODE_BADCONNECTIONREJECTED:
      Result := ioConnectPLCFailed;
    UA_STATUSCODE_BADOUTOFMEMORY, UA_STATUSCODE_BADINTERNALERROR:
      Result := ioDriverError;
    UA_STATUSCODE_BADTOOMANYOPERATIONS, UA_STATUSCODE_BADNOTSUPPORTED,
    UA_STATUSCODE_BADSERVICEUNSUPPORTED, UA_STATUSCODE_BADNOTIMPLEMENTED:
      Result := ioIllegalRequest;
    else
      if OPCUAIsConnectionLost(Status and $FFFF0000) then
        Result := ioCommError
      else
        Result := ioPLCError;
  end;
end;

procedure OPCUASplitAddress(const Address:AnsiString; out NodeIdText, IndexRange:AnsiString);
var
  p, i:Integer;
  r:AnsiString;
  ok:Boolean;
begin
  NodeIdText := Trim(Address);
  IndexRange := '';
  p := LastDelimiter('|', NodeIdText);
  if p<=0 then exit;
  r := Trim(Copy(NodeIdText, p+1, MaxInt));
  //an index range is digits, ':' and ',' only; anything else belongs to the NodeId
  ok := r<>'';
  for i:=1 to Length(r) do
    if not (r[i] in ['0'..'9', ':', ',']) then begin
      ok := false;
      break;
    end;
  if not ok then exit;
  IndexRange := r;
  NodeIdText := Trim(Copy(NodeIdText, 1, p-1));
end;

end.
