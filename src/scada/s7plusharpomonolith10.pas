unit S7PlusHarpoMonolith10;

{$mode objfpc}{$H+}

//: Hand-written orchestrator mirroring HarpoS7's Monolith10 (C# delegates to 3 Part
//: subprograms sharing a 301-element locals scratch array) - see
//: session_auth/family0/_generated/monolith10.py (also hand-written there, same reason).

interface

uses SysUtils;

procedure Execute(const Source:TBytes; var Destination:TBytes);

implementation

uses S7PlusHarpoTenPart1, S7PlusHarpoTenPart2, S7PlusHarpoTenPart3;

procedure Execute(const Source:TBytes; var Destination:TBytes);
var
  Locals_:array[0..300] of Cardinal;
  i:Integer;
begin
  for i:=0 to High(Locals_) do Locals_[i] := 0;
  S7PlusHarpoTenPart1.Execute(Source, Locals_);
  S7PlusHarpoTenPart2.Execute(Locals_);
  S7PlusHarpoTenPart3.Execute(Destination, Locals_);
end;

end.
