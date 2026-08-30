unit S7PlusHarpoMonolithWrappers;

{$mode objfpc}{$H+}

//: Ported from HarpoS7 (bonk-dev/HarpoS7, MIT) via python-snap7's
//: session_auth/family0/monolith_wrappers.py. "WithCopy" wrappers for Monoliths 3-7:
//: concatenate multiple input spans into the monolith's flat source buffer, run
//: Execute, then split the destination back into multiple output spans. Used by
//: Transform7.
//:
//: Each Src/Dst parameter pairs a buffer with a byte offset into it (mirroring
//: Python's `buf[offset:]` memoryview slicing) - writes through a TBytes parameter
//: mutate the caller's own array in place (FPC dynamic arrays share their backing
//: store on ordinary assignment/parameter-passing; only SetLength on the local copies
//: made inside a callee, which none of these do, would detach it).

interface

uses SysUtils, S7PlusHarpoMonolith3, S7PlusHarpoMonolith4, S7PlusHarpoMonolith5,
     S7PlusHarpoMonolith6, S7PlusHarpoMonolith7;

//: Monolith3.WithCopy: 3 inputs (0x48+0x48+0x18) -> 2 outputs (0x48+0x48).
procedure Monolith3WithCopy(var Dst1:TBytes; Dst1Off:Integer; var Dst2:TBytes; Dst2Off:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer;
                            const Src3:TBytes; Src3Off:Integer);
//: Monolith4.WithCopy: 2 inputs (0x48+0x48) -> 1 output (72 bytes).
procedure Monolith4WithCopy(var Dst:TBytes; DstOff:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer);
//: Monolith5.WithCopy: 3 inputs (0x48+0x48+0x48) -> 2 outputs (0x18+0x18).
procedure Monolith5WithCopy(var Dst1:TBytes; Dst1Off:Integer; var Dst2:TBytes; Dst2Off:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer;
                            const Src3:TBytes; Src3Off:Integer);
//: Monolith6.WithCopy: 3 inputs (0x48+0x48+0x48) -> 2 outputs (0x48+0x48).
procedure Monolith6WithCopy(var Dst1:TBytes; Dst1Off:Integer; var Dst2:TBytes; Dst2Off:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer;
                            const Src3:TBytes; Src3Off:Integer);
//: Monolith7.WithCopy: 2 inputs (0x18+0x48) -> 2 outputs (0x48+0x48).
procedure Monolith7WithCopy(var Dst1:TBytes; Dst1Off:Integer; var Dst2:TBytes; Dst2Off:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer);

implementation

procedure Monolith3WithCopy(var Dst1:TBytes; Dst1Off:Integer; var Dst2:TBytes; Dst2Off:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer;
                            const Src3:TBytes; Src3Off:Integer);
var
  MonoSrc, MonoDst:TBytes;
begin
  SetLength(MonoSrc, $48+$48+$18);
  Move(Src1[Src1Off], MonoSrc[0], $48);
  Move(Src2[Src2Off], MonoSrc[$48], $48);
  Move(Src3[Src3Off], MonoSrc[$90], $18);

  SetLength(MonoDst, $90);
  S7PlusHarpoMonolith3.Execute(MonoSrc, MonoDst);

  Move(MonoDst[0], Dst1[Dst1Off], $48);
  Move(MonoDst[$48], Dst2[Dst2Off], $48);
end;

procedure Monolith4WithCopy(var Dst:TBytes; DstOff:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer);
var
  MonoSrc, MonoDst:TBytes;
begin
  SetLength(MonoSrc, $48+$48);
  Move(Src1[Src1Off], MonoSrc[0], $48);
  Move(Src2[Src2Off], MonoSrc[$48], $48);

  SetLength(MonoDst, 72);
  S7PlusHarpoMonolith4.Execute(MonoSrc, MonoDst);

  Move(MonoDst[0], Dst[DstOff], 72);
end;

procedure Monolith5WithCopy(var Dst1:TBytes; Dst1Off:Integer; var Dst2:TBytes; Dst2Off:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer;
                            const Src3:TBytes; Src3Off:Integer);
var
  MonoSrc, MonoDst:TBytes;
begin
  SetLength(MonoSrc, $48*3);
  Move(Src1[Src1Off], MonoSrc[0], $48);
  Move(Src2[Src2Off], MonoSrc[$48], $48);
  Move(Src3[Src3Off], MonoSrc[$90], $48);

  SetLength(MonoDst, 48);
  S7PlusHarpoMonolith5.Execute(MonoSrc, MonoDst);

  Move(MonoDst[0], Dst1[Dst1Off], $18);
  Move(MonoDst[$18], Dst2[Dst2Off], $18);
end;

procedure Monolith6WithCopy(var Dst1:TBytes; Dst1Off:Integer; var Dst2:TBytes; Dst2Off:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer;
                            const Src3:TBytes; Src3Off:Integer);
var
  MonoSrc, MonoDst:TBytes;
begin
  SetLength(MonoSrc, $48*3);
  Move(Src1[Src1Off], MonoSrc[0], $48);
  Move(Src2[Src2Off], MonoSrc[$48], $48);
  Move(Src3[Src3Off], MonoSrc[$90], $48);

  SetLength(MonoDst, $90);
  S7PlusHarpoMonolith6.Execute(MonoSrc, MonoDst);

  Move(MonoDst[0], Dst1[Dst1Off], $48);
  Move(MonoDst[$48], Dst2[Dst2Off], $48);
end;

procedure Monolith7WithCopy(var Dst1:TBytes; Dst1Off:Integer; var Dst2:TBytes; Dst2Off:Integer;
                            const Src1:TBytes; Src1Off:Integer; const Src2:TBytes; Src2Off:Integer);
var
  MonoSrc, MonoDst:TBytes;
begin
  SetLength(MonoSrc, $18+$48);
  Move(Src1[Src1Off], MonoSrc[0], $18);
  Move(Src2[Src2Off], MonoSrc[$18], $48);

  SetLength(MonoDst, $90);
  S7PlusHarpoMonolith7.Execute(MonoSrc, MonoDst);

  Move(MonoDst[0], Dst1[Dst1Off], $48);
  Move(MonoDst[$48], Dst2[Dst2Off], $48);
end;

end.
