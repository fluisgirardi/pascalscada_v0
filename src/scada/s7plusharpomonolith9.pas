unit S7PlusHarpoMonolith9;

{$mode objfpc}{$H+}

//: Hand-written orchestrator mirroring HarpoS7's Monolith9 (C# delegates to 11 Part
//: subprograms sharing an 831-element locals scratch array) - see
//: session_auth/family0/_generated/monolith9.py (also hand-written there, same reason).

interface

uses SysUtils;

procedure Execute(const Source:TBytes; var Destination:TBytes);

implementation

uses S7PlusHarpoNinePart1, S7PlusHarpoNinePart2, S7PlusHarpoNinePart3, S7PlusHarpoNinePart4,
     S7PlusHarpoNinePart5, S7PlusHarpoNinePart6, S7PlusHarpoNinePart7, S7PlusHarpoNinePart8,
     S7PlusHarpoNinePart9, S7PlusHarpoNinePart10, S7PlusHarpoNinePart11;

procedure Execute(const Source:TBytes; var Destination:TBytes);
var
  Locals_:array[0..830] of Cardinal;
  i:Integer;
begin
  for i:=0 to High(Locals_) do Locals_[i] := 0;
  S7PlusHarpoNinePart1.Execute(Source, Locals_);
  S7PlusHarpoNinePart2.Execute(Source, Locals_);
  S7PlusHarpoNinePart3.Execute(Locals_);
  S7PlusHarpoNinePart4.Execute(Locals_);
  S7PlusHarpoNinePart5.Execute(Locals_);
  S7PlusHarpoNinePart6.Execute(Locals_);
  S7PlusHarpoNinePart7.Execute(Locals_);
  S7PlusHarpoNinePart8.Execute(Locals_);
  S7PlusHarpoNinePart9.Execute(Locals_);
  S7PlusHarpoNinePart10.Execute(Locals_);
  S7PlusHarpoNinePart11.Execute(Destination, Locals_);
end;

end.
