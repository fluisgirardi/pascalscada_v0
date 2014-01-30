unit fpsdbf_prssupp;

// parse support

{$I fpsdbf_common.inc}

interface

uses
  Classes;

type

  {TOCollection interfaces between OWL TCollection and VCL TList}

  TOCollection = class(TList)
  public
    procedure AtFree(Index: LongInt);
    procedure FreeAll;
    procedure DoFree(Item: Pointer);
    procedure FreeItem(Item: Pointer); virtual;
    destructor Destroy; override;
  end;

  TNoOwnerCollection = class(TOCollection)
  public
    procedure FreeItem(Item: Pointer); override;
  end;

  { TSortedCollection object }

  TSortedCollection = class(TOCollection)
  public
    function Compare(Key1, Key2: Pointer): LongInt; virtual; abstract;
    function IndexOf(Item: Pointer): LongInt; virtual;
    procedure Add(Item: Pointer); virtual;
    procedure AddReplace(Item: Pointer); virtual;
    procedure AddList(Source: TList; FromIndex, ToIndex: LongInt);
    {if duplicate then replace the duplicate else add}
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: LongInt): Boolean; virtual;
  end;

  { TStrCollection object }

  TStrCollection = class(TSortedCollection)
  public
    function Compare(Key1, Key2: Pointer): LongInt; override;
    procedure FreeItem(Item: Pointer); override;
  end;

function GetStrFromInt(Val: LongInt; const Dst: PChar): LongInt;
procedure GetStrFromInt_Width(Val: LongInt; const Width: LongInt; const Dst: PChar; const PadChar: Char);
{$ifdef SUPPORT_INT64}
function  GetStrFromInt64(Val: Int64; const Dst: PChar): LongInt;
procedure GetStrFromInt64_Width(Val: Int64; const Width: LongInt; const Dst: PChar; const PadChar: Char);
{$endif}

implementation

uses SysUtils;

destructor TOCollection.Destroy;
begin
  FreeAll;
  inherited Destroy;
end;

procedure TOCollection.AtFree(Index: LongInt);
var
  Item: Pointer;
begin
  Item := Items[Index];
  Delete(Index);
  FreeItem(Item);
end;


procedure TOCollection.FreeAll;
var
  I: LongInt;
begin
  try
    for I := 0 to Count - 1 do
      FreeItem(Items[I]);
  finally
    Count := 0;
  end;
end;

procedure TOCollection.DoFree(Item: Pointer);
begin
  AtFree(IndexOf(Item));
end;

procedure TOCollection.FreeItem(Item: Pointer);
begin
  if (Item <> nil) then
    with TObject(Item) as TObject do
      Free;
end;

{----------------------------------------------------------------virtual;
  Implementing TNoOwnerCollection
  -----------------------------------------------------------------}

procedure TNoOwnerCollection.FreeItem(Item: Pointer);
begin
end;

{ TSortedCollection }

function TSortedCollection.IndexOf(Item: Pointer): LongInt;
var
  I: LongInt;
begin
  IndexOf := -1;
  if Search(KeyOf(Item), I) then
  begin
    while (I < Count) and (Item <> Items[I]) do
      Inc(I);
    if I < Count then IndexOf := I;
  end;
end;

procedure TSortedCollection.AddReplace(Item: Pointer);
var
  Index: LongInt;
begin
  if Search(KeyOf(Item), Index) then
    Delete(Index);
  Add(Item);
end;

procedure TSortedCollection.Add(Item: Pointer);
var
  I: LongInt;
begin
  Search(KeyOf(Item), I);
  Insert(I, Item);
end;

procedure TSortedCollection.AddList(Source: TList; FromIndex, ToIndex: LongInt);
var
  I: LongInt;
begin
  for I := FromIndex to ToIndex do
    Add(Source.Items[I]);
end;

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
begin
  Result := Item;
end;

function TSortedCollection.Search(Key: Pointer; var Index: LongInt): Boolean;
var
  L, H, I, C: LongInt;
begin
  Result := false;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) div 2;
    C := Compare(KeyOf(Items[I]), Key);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      Result := C = 0;
    end;
  end;
  Index := L;
end;

{ TStrCollection }

function TStrCollection.Compare(Key1, Key2: Pointer): LongInt;
begin
  Compare := StrComp(PWideChar(Key1), PWideChar(Key2));
end;

procedure TStrCollection.FreeItem(Item: Pointer);
begin
  StrDispose(PWideChar(Item));
end;

// it seems there is no pascal function to convert an LongInt into a PChar???
// NOTE: in dbf_dbffile.pas there is also a convert routine, but is slightly different

function GetStrFromInt(Val: LongInt; const Dst: PChar): LongInt;
var
  Temp: array[0..10] of Char;
  I, J: LongInt;
begin
  Val := Abs(Val);
  // we'll have to store characters backwards first
  I := 0;
  J := 0;
  repeat
    Temp[I] := Chr((Val mod 10) + Ord('0'));
    Val := Val div 10;
    Inc(I);
  until Val = 0;

  // remember number of digits
  Result := I;
  // copy value, remember: stored backwards
  repeat
    Dst[J] := Temp[I-1];
    Inc(J);
    Dec(I);
  until I = 0;
  // done!
end;

// it seems there is no pascal function to convert an LongInt into a PChar???

procedure GetStrFromInt_Width(Val: LongInt; const Width: LongInt; const Dst: PChar; const PadChar: Char);
var
  Temp: array[0..10] of Char;
  I, J: LongInt;
  NegSign: boolean;
begin
  {$I fpsgetstrfromint.inc}
end;

{$ifdef SUPPORT_INT64}

procedure GetStrFromInt64_Width(Val: Int64; const Width: LongInt; const Dst: PChar; const PadChar: Char);
var
  Temp: array[0..19] of Char;
  I, J: LongInt;
  NegSign: boolean;
begin
  {$I fpsgetstrfromint.inc}
end;

function GetStrFromInt64(Val: Int64; const Dst: PChar): LongInt;
var
  Temp: array[0..19] of Char;
  I, J: LongInt;
begin
  Val := Abs(Val);
  // we'll have to store characters backwards first
  I := 0;
  J := 0;
  repeat
    Temp[I] := Chr((Val mod 10) + Ord('0'));
    Val := Val div 10;
    Inc(I);
  until Val = 0;

  // remember number of digits
  Result := I;
  // copy value, remember: stored backwards
  repeat
    Dst[J] := Temp[I-1];
    inc(J);
    dec(I);
  until I = 0;
  // done!
end;

{$endif}

end.

