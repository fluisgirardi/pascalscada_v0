unit S7PlusHarpoUtil;

{$mode objfpc}{$H+}

interface

uses SysUtils;

//: Little-endian: reads Count 32-bit words from the front of Buf into Arr[0..Count-1].
//: Buf must have at least Count*4 bytes (mirrors HarpoS7._to_uints, which just drops
//: any bytes beyond the last complete word rather than erroring).
procedure BytesToUInt32Array(const Buf:TBytes; var Arr:array of Cardinal; Count:Integer);
//: Little-endian: writes Arr[0..Count-1] into the front Count*4 bytes of Buf, in place -
//: Buf's own length is never changed (mirrors HarpoS7._from_uints's same-length slice
//: assignment onto the destination bytearray).
procedure UInt32ArrayToBytes(const Arr:array of Cardinal; var Buf:TBytes; Count:Integer);

implementation

procedure BytesToUInt32Array(const Buf:TBytes; var Arr:array of Cardinal; Count:Integer);
var
  i:Integer;
begin
  for i:=0 to Count-1 do
    Arr[i] := Cardinal(Buf[i*4]) or (Cardinal(Buf[i*4+1]) shl 8) or
              (Cardinal(Buf[i*4+2]) shl 16) or (Cardinal(Buf[i*4+3]) shl 24);
end;

procedure UInt32ArrayToBytes(const Arr:array of Cardinal; var Buf:TBytes; Count:Integer);
var
  i:Integer;
  v:Cardinal;
begin
  for i:=0 to Count-1 do begin
    v := Arr[i];
    Buf[i*4]   := Byte(v and $FF);
    Buf[i*4+1] := Byte((v shr 8) and $FF);
    Buf[i*4+2] := Byte((v shr 16) and $FF);
    Buf[i*4+3] := Byte((v shr 24) and $FF);
  end;
end;

end.
