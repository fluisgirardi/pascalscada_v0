{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Codificação/decodificação VLQ (Variable-Length Quantity) usada pelo S7CommPlus.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Cada byte usa 7 bits de dado + 1 bit de continuação (MSB). Bit de continuação = 1
  significa que há mais bytes; = 0 é o último byte. Ordem big-endian (grupo mais
  significativo primeiro). Valores com sinal usam o bit 6 do primeiro byte como
  indicador de sinal. Para 64 bits, 8 grupos de 7 bits somam 56 bits (menos que 64),
  então o 9º byte usa todos os 8 bits (sem bit de continuação), evitando um 10º byte.

  Portado de python-snap7/s7commplus/vlq.py (referência: thomas-v2/S7CommPlusDriver/Core/S7p.cs).
}
{$ELSE}
{:
  @abstract(Variable-Length Quantity (VLQ) encoding/decoding used by S7CommPlus.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Each byte uses 7 data bits + 1 continuation bit (MSB). continuation=1 means more
  bytes follow, continuation=0 means this is the last byte. Big-endian byte order
  (most significant group first). Signed values use bit 6 of the first byte as a
  sign flag. For 64-bit values, 8 groups of 7 bits sum to 56 bits (less than 64), so
  the 9th byte uses all 8 bits (no continuation flag), avoiding the need of a 10th byte.

  Ported from python-snap7/s7commplus/vlq.py (reference: thomas-v2/S7CommPlusDriver/Core/S7p.cs).
}
{$ENDIF}
unit S7PlusVLQ;

{$mode Delphi}{$H+}

interface

uses
  SysUtils;

//: Encodes an unsigned 32-bit integer as VLQ (1 to 5 bytes).
function EncodeUInt32VLQ(Value:Cardinal):TBytes;
//: Decodes a VLQ-encoded unsigned 32-bit integer. @param(Consumed Number of bytes read.)
function DecodeUInt32VLQ(const Data:TBytes; Offset:Integer; out Consumed:Integer):Cardinal;

//: Encodes a signed 32-bit integer as VLQ (1 to 5 bytes).
function EncodeInt32VLQ(Value:LongInt):TBytes;
//: Decodes a VLQ-encoded signed 32-bit integer. @param(Consumed Number of bytes read.)
function DecodeInt32VLQ(const Data:TBytes; Offset:Integer; out Consumed:Integer):LongInt;

//: Encodes an unsigned 64-bit integer as VLQ (1 to 9 bytes).
function EncodeUInt64VLQ(Value:QWord):TBytes;
//: Decodes a VLQ-encoded unsigned 64-bit integer. @param(Consumed Number of bytes read.)
function DecodeUInt64VLQ(const Data:TBytes; Offset:Integer; out Consumed:Integer):QWord;

//: Encodes a signed 64-bit integer as VLQ (1 to 9 bytes).
function EncodeInt64VLQ(Value:Int64):TBytes;
//: Decodes a VLQ-encoded signed 64-bit integer. @param(Consumed Number of bytes read.)
function DecodeInt64VLQ(const Data:TBytes; Offset:Integer; out Consumed:Integer):Int64;

implementation

function EncodeUInt32VLQ(Value:Cardinal):TBytes;
var
  NumGroups, i:Integer;
  b:array[0..4] of Byte;
  len:Integer;
begin
  NumGroups := 1;
  for i:=4 downto 1 do
    if (Value and (Cardinal($7F) shl (i*7)))<>0 then begin
      NumGroups := i+1;
      break;
    end;

  len := 0;
  for i:=NumGroups-1 downto 0 do begin
    b[len] := (Value shr (i*7)) and $7F;
    if i>0 then
      b[len] := b[len] or $80;
    inc(len);
  end;

  SetLength(Result, len);
  Move(b[0], Result[0], len);
end;

function DecodeUInt32VLQ(const Data:TBytes; Offset:Integer; out Consumed:Integer):Cardinal;
var
  Octet:Byte;
  i:Integer;
begin
  Result := 0;
  Consumed := 0;
  for i:=1 to 5 do begin
    if (Offset+Consumed)>=Length(Data) then
      raise Exception.Create('S7PlusVLQ: unexpected end of VLQ data (uint32)');
    Octet := Data[Offset+Consumed];
    inc(Consumed);
    Result := (Result shl 7) or (Octet and $7F);
    if (Octet and $80)=0 then
      break;
  end;
end;

function EncodeInt32VLQ(Value:LongInt):TBytes;
var
  AbsV:Cardinal;
  b:array[0..4] of Byte;
  len,i:Integer;
  v:LongInt;
begin
  if Value=LongInt($80000000) then
    AbsV := $80000000
  else
    AbsV := Abs(Value);

  v := Value;
  b[0] := v and $7F;
  len := 1;

  for i:=1 to 4 do begin
    if AbsV>=$40 then begin
      inc(len);
      AbsV := AbsV shr 7;
      v := v shr 7;
      b[i] := ((v and $7F) + $80) and $FF;
    end else
      break;
  end;

  SetLength(Result, len);
  for i:=0 to len-1 do
    Result[i] := b[len-1-i];
end;

function DecodeInt32VLQ(const Data:TBytes; Offset:Integer; out Consumed:Integer):LongInt;
var
  Octet:Byte;
  Counter:Integer;
begin
  Result := 0;
  Consumed := 0;
  for Counter:=1 to 5 do begin
    if (Offset+Consumed)>=Length(Data) then
      raise Exception.Create('S7PlusVLQ: unexpected end of VLQ data (int32)');
    Octet := Data[Offset+Consumed];
    inc(Consumed);

    if (Counter=1) and ((Octet and $40)<>0) then begin
      Octet := Octet and $BF;
      Result := -64;
    end else
      Result := Result shl 7;

    Result := Result + (Octet and $7F);

    if (Octet and $80)=0 then
      break;
  end;
end;

function EncodeUInt64VLQ(Value:QWord):TBytes;
var
  Special:Boolean;
  b:array[0..8] of Byte;
  len,i:Integer;
  v:QWord;
begin
  Special := Value > QWord($00FFFFFFFFFFFFFF);
  v := Value;

  if Special then
    b[0] := v and $FF
  else
    b[0] := v and $7F;

  len := 1;
  for i:=1 to 8 do begin
    if v>=$80 then begin
      inc(len);
      if (i=1) and Special then
        v := v shr 8
      else
        v := v shr 7;
      b[i] := ((v and $7F) + $80) and $FF;
    end else
      break;
  end;

  if Special and (len=8) then begin
    inc(len);
    b[8] := $80;
  end;

  SetLength(Result, len);
  for i:=0 to len-1 do
    Result[i] := b[len-1-i];
end;

function DecodeUInt64VLQ(const Data:TBytes; Offset:Integer; out Consumed:Integer):QWord;
var
  Octet:Byte;
  Counter:Integer;
  Cont:Boolean;
begin
  Result := 0;
  Consumed := 0;
  Cont := false;

  for Counter:=1 to 8 do begin
    if (Offset+Consumed)>=Length(Data) then
      raise Exception.Create('S7PlusVLQ: unexpected end of VLQ data (uint64)');
    Octet := Data[Offset+Consumed];
    inc(Consumed);

    Result := (Result shl 7) or (Octet and $7F);
    Cont := (Octet and $80)<>0;

    if not Cont then
      break;
  end;

  if Cont then begin
    if (Offset+Consumed)>=Length(Data) then
      raise Exception.Create('S7PlusVLQ: unexpected end of VLQ data (uint64, 9th byte)');
    Octet := Data[Offset+Consumed];
    inc(Consumed);
    Result := (Result shl 8) or Octet;
  end;
end;

function EncodeInt64VLQ(Value:Int64):TBytes;
var
  AbsV:QWord;
  Special:Boolean;
  b:array[0..8] of Byte;
  len,i:Integer;
  v:Int64;
begin
  if Value=Int64($8000000000000000) then
    AbsV := QWord($8000000000000000)
  else
    AbsV := Abs(Value);

  Special := AbsV > QWord($007FFFFFFFFFFFFF);
  v := Value;

  if Special then
    b[0] := v and $FF
  else
    b[0] := v and $7F;

  len := 1;
  for i:=1 to 8 do begin
    if AbsV>=$40 then begin
      inc(len);
      if (i=1) and Special then begin
        AbsV := AbsV shr 8;
        v := v shr 8;
      end else begin
        AbsV := AbsV shr 7;
        v := v shr 7;
      end;
      b[i] := ((v and $7F) + $80) and $FF;
    end else
      break;
  end;

  if Special and (len=8) then begin
    inc(len);
    if v>=0 then
      b[8] := $80
    else
      b[8] := $FF;
  end;

  SetLength(Result, len);
  for i:=0 to len-1 do
    Result[i] := b[len-1-i];
end;

function DecodeInt64VLQ(const Data:TBytes; Offset:Integer; out Consumed:Integer):Int64;
var
  Octet:Byte;
  Counter:Integer;
  Cont:Boolean;
begin
  Result := 0;
  Consumed := 0;
  Cont := false;

  for Counter:=1 to 8 do begin
    if (Offset+Consumed)>=Length(Data) then
      raise Exception.Create('S7PlusVLQ: unexpected end of VLQ data (int64)');
    Octet := Data[Offset+Consumed];
    inc(Consumed);

    if (Counter=1) and ((Octet and $40)<>0) then begin
      Octet := Octet and $BF;
      Result := -64;
    end else
      Result := Result shl 7;

    Cont := (Octet and $80)<>0;
    Result := Result + (Octet and $7F);

    if not Cont then
      break;
  end;

  if Cont then begin
    if (Offset+Consumed)>=Length(Data) then
      raise Exception.Create('S7PlusVLQ: unexpected end of VLQ data (int64, 9th byte)');
    Octet := Data[Offset+Consumed];
    inc(Consumed);
    Result := (Result shl 8) or Octet;
  end;
end;

end.
