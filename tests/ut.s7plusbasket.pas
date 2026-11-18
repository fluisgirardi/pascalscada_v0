{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da cesta do Tag Builder do S7CommPlus: o que cada entrada vira.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Nao precisa de CLP nem do IDE: S7PlusBasket so decide sobre candidatos de S7PlusTagPlan.
}
{$ELSE}
{:
  @abstract(Tests of the S7CommPlus Tag Builder basket: what each entry becomes.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Needs neither a PLC nor the IDE: S7PlusBasket only decides over S7PlusTagPlan candidates.
}
{$ENDIF}
unit ut.s7plusbasket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fpcunit, testregistry, Tag, S7PlusTypeInfo, S7PlusTagPlan, S7PlusBasket;

type

  { TTestS7PlusBasket }

  TTestS7PlusBasket = class(TTestCase)
  published
    procedure ADraggedNodeBecomesWhatItIs;
    procedure WhatCannotBeBuiltCannotBeDragged;
    procedure ATagHasNoChildrenToMap;
    procedure EachEntryKeepsItsOwnScanAndName;
    procedure ABlockMapsItsElementsOrNot;
    procedure AStructMapsItsItemsOrNot;
    procedure ADraggedStructDoesNotBringTheStructsInsideIt;
    procedure ABoolInAStructBecomesABitOverAByteItem;
    procedure BoolsOfTheSameByteShareTheByteItem;
    procedure AnArrayOfBoolsBecomesByteElementsAndBits;
    procedure AnArrayOfStructsIsAlwaysOneStruct;
    procedure TheBoolOfAnArrayOfStructsHasABytePerIndex;
    procedure AWholeDBIsOneStruct;
    procedure AnAreaIsNotAnEntry;
    procedure AnEntryWhoseVariableIsGoneIsLeftOut;
    procedure TheSameNodeInTwoEntriesIsAllowed;
    procedure TheSameNameTwiceIsAProblem;
    procedure AnInvalidNameIsAProblem;
    procedure AnExistingNameIsAProblem;
    procedure TheNamesOfTheChildrenAreChecked;
    procedure ACleanBasketHasNoProblems;
    procedure ATemplateFillsTheNameFromThePath;
    procedure ATypeListFiltersByThePLCType;
    procedure AnElementOfAnArrayOfStructsIsAStructOfItsOwn;
    procedure AnElementOfAnArrayOfStructsMapsItsMembersOrNot;
    procedure AnArrayElementIsATagByItsPath;
    procedure AMemberInsideAStructArrayElementIsATagByItsPath;
  end;

implementation

function ListSized(const Names:array of String; const Types:array of Byte;
  const ByteOffs, ImageOffs:array of Integer):TS7PlusCandidateArray;
var
  i:Integer;
  V:TS7PlusVarInfo;
begin
  SetLength(Result, Length(Names));
  for i:=0 to High(Names) do begin
    FillChar(V, SizeOf(V), 0);
    V.Name := Names[i];
    V.SoftDataType := Types[i];
    V.ByteOffset := ByteOffs[i];
    V.ImageOffset := ImageOffs[i];
    Result[i] := S7PlusClassifyVar(V, '');
  end;
  S7PlusMarkArrays(Result);
end;

//DB4 of the test PLC: Var25 DInt, Var26 Struct{v1 Byte, v2 Word, v3 DWord, v4 Bool, Static_1 Real,
//Static_2..4 Bool}, optimized (the PLC lays Var26 out by size, a Bool in a byte of its own)
function Db4Like:TS7PlusCandidateArray;
begin
  Result := ListSized(
    ['DB4.Var1', 'DB4.Var25', 'DB4.Var26', 'DB4.Var26.v1', 'DB4.Var26.v2', 'DB4.Var26.v3', 'DB4.Var26.v4',
     'DB4.Var26.Static_1', 'DB4.Var26.Static_2', 'DB4.Var26.Static_3', 'DB4.Var26.Static_4'],
    [7, 7, 17, 2, 4, 6, 40, 8, 40, 40, 40],
    [0, 96, 100, 100, 102, 104, 108, 110, 114, 114, 114],
    [4, 100, 104, 114, 112, 104, 115, 108, 116, 117, 118]);
end;

//DB4.var28: Array[0..1] of Struct (Dint1 DInt, aBool Bool, aInt Int), as the PLC lists it
function StructArrayList:TS7PlusCandidateArray;
begin
  Result := ListSized(['DB4.var28', 'DB4.var28[0]', 'DB4.var28[0].Dint1', 'DB4.var28[0].aBool', 'DB4.var28[0].aInt',
                       'DB4.var28[1]', 'DB4.var28[1].Dint1', 'DB4.var28[1].aBool', 'DB4.var28[1].aInt'],
                      [17, 17, 7, 40, 5, 17, 7, 40, 5],
                      [526, 526, 526, 530, 532, 534, 534, 538, 540],
                      [276, -1, -1, -1, -1, -1, -1, -1, -1]);
end;

//DB5.S: Struct{a DWord, b1, b2, b3 Bool}; the Bools share the byte at 4, at bits 0, 1 and 2
function SharedBoolList:TS7PlusCandidateArray;
begin
  Result := ListSized(['DB5.S', 'DB5.S.a', 'DB5.S.b1', 'DB5.S.b2', 'DB5.S.b3'],
                      [17, 6, 40, 40, 40],
                      [0, 0, 4, 4, 4],
                      [0, 0, 4, 4, 4]);
  Result[3].BitOffset := 1;
  Result[4].BitOffset := 2;
end;

//DB6.F: Array[0..2] of Bool and DB6.W: Array[0..1] of Word
function ArrayList:TS7PlusCandidateArray;
begin
  Result := ListSized(['DB6.F', 'DB6.F[0]', 'DB6.F[1]', 'DB6.F[2]', 'DB6.W', 'DB6.W[0]', 'DB6.W[1]'],
                      [40, 40, 40, 40, 4, 4, 4],
                      [0, -1, -1, -1, 4, -1, -1],
                      [0, -1, -1, -1, 4, -1, -1]);
end;

function Entry(const L:TS7PlusCandidateArray; Idx:Integer; const Prefix:String; Scan:LongInt;
  Map:Boolean):TS7PlusBasketEntry;
begin
  TAssert.AssertTrue(L[Idx].Path+' can be dragged', S7PlusBasketNewEntry(L, Idx, Prefix, Scan, Map, Result));
end;

function One(const L:TS7PlusCandidateArray; Idx:Integer; const Prefix:String; Scan:LongInt;
  Map:Boolean):TS7PlusImportItem;
var
  E:TS7PlusBasketEntries;
  P:TS7PlusImportItems;
begin
  SetLength(E, 1);
  E[0] := Entry(L, Idx, Prefix, Scan, Map);
  P := S7PlusBasketBuild(L, E);
  TAssert.AssertEquals('one item for one entry', 1, Length(P));
  Result := P[0];
end;

function CountBoolItems(const I:TS7PlusImportItem):Integer;
var
  k:Integer;
begin
  Result := 0;
  for k:=0 to High(I.StructItems) do
    if I.StructItems[k].IsBool then Inc(Result);
end;

function FindItem(const I:TS7PlusImportItem; const AName:String):Integer;
var
  k:Integer;
begin
  Result := -1;
  for k:=0 to High(I.StructItems) do
    if I.StructItems[k].CompName=AName then Result := k;
end;

procedure TTestS7PlusBasket.ADraggedNodeBecomesWhatItIs;
var
  L:TS7PlusCandidateArray;
  K:TS7PlusBasketKind;
begin
  L := Db4Like;
  AssertTrue(S7PlusBasketKindOf(L, 0, K));
  AssertTrue('a simple variable is a tag', K=bkTag);
  AssertTrue(S7PlusBasketKindOf(L, 2, K));
  AssertTrue('a structure is a struct', K=bkStruct);
  AssertTrue(S7PlusBasketKindOf(L, 3, K));
  AssertTrue('a member of it is a tag', K=bkTag);
  L := ArrayList;
  AssertTrue(S7PlusBasketKindOf(L, 4, K));
  AssertTrue('an array of simple variables is a block', K=bkBlock);
  L := StructArrayList;
  AssertTrue(S7PlusBasketKindOf(L, 0, K));
  AssertTrue('an array of structures is a struct, whatever the user asks', K=bkStruct);
end;

procedure TTestS7PlusBasket.WhatCannotBeBuiltCannotBeDragged;
var
  L:TS7PlusCandidateArray;
  K:TS7PlusBasketKind;
begin
  L := StructArrayList;
  AssertFalse('the element of an array of scalars is a tag, not a struct', S7PlusBasketKindOf(L, 99, K));
  L := ListSized(['DB7.Nome', 'DB7.Quando'], [19, Ord(sdtDTL)], [0, 0], [0, 0]);
  AssertFalse('a STRING whose length was not read', S7PlusBasketKindOf(L, 0, K));
  L[0].StringLength := 10;
  AssertTrue('and once it was', S7PlusBasketKindOf(L, 0, K));
  AssertFalse('a date/time type', S7PlusBasketKindOf(L, 1, K));
  AssertFalse('an index out of the list', S7PlusBasketKindOf(L, 5, K));
end;

procedure TTestS7PlusBasket.ATagHasNoChildrenToMap;
var
  L:TS7PlusCandidateArray;
  E:TS7PlusBasketEntry;
begin
  L := Db4Like;
  AssertTrue(S7PlusBasketNewEntry(L, 0, 'p', 0, true, E));
  AssertFalse('nothing to map on a tag', E.MapChildren);
  AssertTrue(S7PlusBasketNewEntry(L, 2, 'p', 0, true, E));
  AssertTrue('but there is on a struct', E.MapChildren);
end;

procedure TTestS7PlusBasket.EachEntryKeepsItsOwnScanAndName;
var
  L:TS7PlusCandidateArray;
  E:TS7PlusBasketEntries;
  P:TS7PlusImportItems;
begin
  L := Db4Like;
  SetLength(E, 3);
  E[0] := Entry(L, 0, 'a', 500, false);
  E[1] := Entry(L, 1, 'b', 0, false);
  E[1].Name := 'MyDInt';
  E[2] := Entry(L, 2, 'c', 1000, false);
  P := S7PlusBasketBuild(L, E);
  AssertEquals(3, Length(P));
  AssertEquals('scan of the first', 500, P[0].Scan);
  AssertEquals('on change for the second', 0, P[1].Scan);
  AssertEquals('and 1 s for the third', 1000, P[2].Scan);
  AssertEquals('the prefix names it', 'aDB4_Var1', P[0].CompName);
  AssertEquals('the given name wins', 'MyDInt', P[1].CompName);
  AssertFalse('a tag', P[0].IsBlock or P[0].IsStruct);
  AssertTrue(P[2].IsStruct);
end;

procedure TTestS7PlusBasket.ABlockMapsItsElementsOrNot;
var
  I:TS7PlusImportItem;
  L:TS7PlusCandidateArray;
begin
  L := ArrayList;
  I := One(L, 4, 'p', 0, true);
  AssertTrue('a TPLCBlock', I.IsBlock);
  AssertEquals(2, I.Size);
  AssertEquals('with its elements', 2, Length(I.ElementNames));
  AssertEquals('pDB6_W_0', I.ElementNames[0]);
  I := One(L, 4, 'p', 0, false);
  AssertTrue('still a block', I.IsBlock);
  AssertEquals('without the elements', 0, Length(I.ElementNames));
  AssertEquals('and as big as before', 2, I.Size);
end;

procedure TTestS7PlusBasket.AStructMapsItsItemsOrNot;
var
  I:TS7PlusImportItem;
  L:TS7PlusCandidateArray;
begin
  L := Db4Like;
  I := One(L, 2, 'p', 0, true);
  AssertTrue(I.IsStruct);
  AssertEquals('DB4.Var26', I.Path);
  AssertEquals('v3 at the start of the image', 0, I.StructItems[FindItem(I, 'pDB4_Var26_v3')].Offset);
  I := One(L, 2, 'p', 0, false);
  AssertTrue('still a struct', I.IsStruct);
  AssertEquals('without the items', 0, Length(I.StructItems));
  AssertEquals('with the size of a read', 15, I.Size);
  AssertEquals('and no bits', 0, Length(I.Bits));
end;

procedure TTestS7PlusBasket.ADraggedStructDoesNotBringTheStructsInsideIt;
var
  L:TS7PlusCandidateArray;
  E:TS7PlusBasketEntries;
  P:TS7PlusImportItems;
begin
  L := ListSized(['DB8.A', 'DB8.A.B', 'DB8.A.B.x', 'DB8.A.y'], [17, 17, 7, 7], [0, 4, 4, 0], [0, 4, 4, 0]);
  SetLength(E, 1);
  E[0] := Entry(L, 0, 'p', 0, true);
  P := S7PlusBasketBuild(L, E);
  AssertEquals('only what was dragged', 1, Length(P));
  AssertEquals('DB8.A', P[0].Path);
  AssertEquals('it holds the variables of the inner one too', 2, Length(P[0].StructItems));
end;

procedure TTestS7PlusBasket.ABoolInAStructBecomesABitOverAByteItem;
var
  I:TS7PlusImportItem;
  L:TS7PlusCandidateArray;
  k:Integer;
begin
  L := Db4Like;
  I := One(L, 2, 'p', 0, true);
  AssertEquals('the Bool items are gone', 0, CountBoolItems(I));
  AssertEquals('one bit per Bool', 4, Length(I.Bits));
  //v3, Static_1, v2, v1 and a byte item per Bool (each has a byte of its own here)
  AssertEquals('items', 8, Length(I.StructItems));
  AssertEquals('pDB4_Var26_v4', I.Bits[0].CompName);
  AssertEquals('named by the structure and the byte', 'pDB4_Var26_Byte11', I.Bits[0].SupportName);
  AssertEquals(0, I.Bits[0].BitIndex);
  k := FindItem(I, I.Bits[0].SupportName);
  AssertTrue('the byte item exists', k>=0);
  AssertEquals('at byte 11', 11, I.StructItems[k].Offset);
  AssertTrue('as a byte', I.StructItems[k].TagType=pttByte);
  AssertEquals('the size still covers the last byte', 15, I.Size);
end;

procedure TTestS7PlusBasket.BoolsOfTheSameByteShareTheByteItem;
var
  I:TS7PlusImportItem;
  L:TS7PlusCandidateArray;
begin
  L := SharedBoolList;
  I := One(L, 0, 'p', 0, true);
  AssertEquals('a and the byte', 2, Length(I.StructItems));
  AssertEquals('three bits', 3, Length(I.Bits));
  AssertEquals(I.Bits[0].SupportName, I.Bits[1].SupportName);
  AssertEquals(I.Bits[0].SupportName, I.Bits[2].SupportName);
  AssertEquals('bit of b1', 0, I.Bits[0].BitIndex);
  AssertEquals('bit of b2', 1, I.Bits[1].BitIndex);
  AssertEquals('bit of b3', 2, I.Bits[2].BitIndex);
  AssertEquals('pDB5_S_b2', I.Bits[1].CompName);
end;

procedure TTestS7PlusBasket.AnArrayOfBoolsBecomesByteElementsAndBits;
var
  I:TS7PlusImportItem;
  L:TS7PlusCandidateArray;
begin
  L := ArrayList;
  I := One(L, 0, 'p', 0, true);
  AssertTrue('a block', I.IsBlock);
  AssertEquals('of bytes', 3, I.Size);
  AssertEquals('an element per Bool', 3, Length(I.ElementNames));
  AssertEquals('pDB6_F_0_Byte', I.ElementNames[0]);
  AssertEquals('and a bit per element', 3, Length(I.Bits));
  AssertEquals('pDB6_F_0', I.Bits[0].CompName);
  AssertEquals(I.ElementNames[0], I.Bits[0].SupportName);
  AssertEquals(0, I.Bits[0].BitIndex);
  I := One(L, 0, 'p', 0, false);
  AssertEquals('no elements, no bits', 0, Length(I.ElementNames)+Length(I.Bits));
end;

procedure TTestS7PlusBasket.AnArrayOfStructsIsAlwaysOneStruct;
var
  I:TS7PlusImportItem;
  L:TS7PlusCandidateArray;
begin
  L := StructArrayList;
  I := One(L, 0, 'p', 250, false);
  AssertTrue('a struct', I.IsStruct);
  AssertEquals('DB4.var28', I.Path);
  AssertEquals('the whole array: 2 x 8', 16, I.Size);
  AssertEquals('without the items', 0, Length(I.StructItems));
  AssertEquals(250, I.Scan);
  I := One(L, 0, 'p', 250, true);
  AssertTrue('the same struct', I.IsStruct);
  AssertEquals('the whole array', 16, I.Size);
  //Dint1 and aInt in two indexes, and a byte item for each aBool
  AssertEquals('items', 6, Length(I.StructItems));
  AssertEquals('a bit for each aBool', 2, Length(I.Bits));
end;

procedure TTestS7PlusBasket.TheBoolOfAnArrayOfStructsHasABytePerIndex;
var
  I:TS7PlusImportItem;
  L:TS7PlusCandidateArray;
  k0, k1:Integer;
begin
  L := StructArrayList;
  I := One(L, 0, 'p', 0, true);
  AssertEquals('pDB4_var28_0_aBool', I.Bits[0].CompName);
  AssertEquals('pDB4_var28_1_aBool', I.Bits[1].CompName);
  k0 := FindItem(I, I.Bits[0].SupportName);
  k1 := FindItem(I, I.Bits[1].SupportName);
  AssertTrue('each has its byte', (k0>=0) and (k1>=0) and (k0<>k1));
  AssertEquals('aBool at 4 in the first', 4, I.StructItems[k0].Offset);
  AssertEquals('and at 8 + 4 in the second', 12, I.StructItems[k1].Offset);
end;

procedure TTestS7PlusBasket.AWholeDBIsOneStruct;
var
  E:TS7PlusBasketEntries;
  P:TS7PlusImportItems;
  L:TS7PlusCandidateArray;
begin
  L := Db4Like;
  SetLength(E, 1);
  AssertTrue(S7PlusBasketNewDBEntry('DB4', 'p', 100, false, E[0]));
  P := S7PlusBasketBuild(L, E);
  AssertEquals(1, Length(P));
  AssertTrue(P[0].IsStruct);
  AssertEquals('DB4', P[0].Path);
  AssertEquals('the whole DB read', 115, P[0].Size);
  AssertEquals(100, P[0].Scan);
  AssertEquals('no items', 0, Length(P[0].StructItems));
  AssertTrue(S7PlusBasketNewDBEntry('DB4', 'p', 100, true, E[0]));
  P := S7PlusBasketBuild(L, E);
  AssertEquals('the Bools of the DB become bits too', 4, Length(P[0].Bits));
  AssertEquals(0, CountBoolItems(P[0]));
end;

procedure TTestS7PlusBasket.AnAreaIsNotAnEntry;
var
  E:TS7PlusBasketEntry;
begin
  AssertFalse(S7PlusBasketNewDBEntry('MArea', 'p', 0, false, E));
  AssertFalse(S7PlusBasketNewDBEntry('', 'p', 0, false, E));
end;

procedure TTestS7PlusBasket.AnEntryWhoseVariableIsGoneIsLeftOut;
var
  L:TS7PlusCandidateArray;
  E:TS7PlusBasketEntries;
  P:TS7PlusImportItems;
begin
  L := Db4Like;
  SetLength(E, 2);
  E[0] := Entry(L, 0, 'p', 0, false);
  E[1] := Entry(L, 1, 'p', 0, false);
  E[0].Path := 'DB4.Removed';
  P := S7PlusBasketBuild(L, E);
  AssertEquals(1, Length(P));
  AssertEquals('DB4.Var25', P[0].Path);
end;

procedure TTestS7PlusBasket.TheSameNodeInTwoEntriesIsAllowed;
var
  L:TS7PlusCandidateArray;
  E:TS7PlusBasketEntries;
  P:TS7PlusImportItems;
  Problems:TS7PlusNameProblems;
begin
  L := Db4Like;
  SetLength(E, 2);
  //the whole struct and one of its members as a tag of its own
  E[0] := Entry(L, 2, 'p', 0, false);
  E[1] := Entry(L, 3, 'p', 500, false);
  P := S7PlusBasketBuild(L, E);
  AssertEquals(2, Length(P));
  Problems := S7PlusBasketNameProblems(P, []);
  AssertEquals('different names, nothing to complain about', 0, Length(Problems));
end;

procedure TTestS7PlusBasket.TheSameNameTwiceIsAProblem;
var
  L:TS7PlusCandidateArray;
  E:TS7PlusBasketEntries;
  Problems:TS7PlusNameProblems;
begin
  L := Db4Like;
  SetLength(E, 2);
  E[0] := Entry(L, 0, 'p', 0, false);
  E[1] := Entry(L, 1, 'p', 0, false);
  E[1].Name := 'PDB4_VAR1';
  Problems := S7PlusBasketNameProblems(S7PlusBasketBuild(L, E), []);
  AssertEquals(1, Length(Problems));
  AssertTrue(Problems[0].Kind=npDuplicate);
  AssertEquals('the comparison ignores case', 'PDB4_VAR1', Problems[0].Name);
end;

procedure TTestS7PlusBasket.AnInvalidNameIsAProblem;
var
  L:TS7PlusCandidateArray;
  E:TS7PlusBasketEntries;
  Problems:TS7PlusNameProblems;
begin
  L := Db4Like;
  SetLength(E, 1);
  E[0] := Entry(L, 0, 'p', 0, false);
  E[0].Name := '1 my tag';
  Problems := S7PlusBasketNameProblems(S7PlusBasketBuild(L, E), []);
  AssertEquals(1, Length(Problems));
  AssertTrue(Problems[0].Kind=npInvalid);
end;

procedure TTestS7PlusBasket.AnExistingNameIsAProblem;
var
  L:TS7PlusCandidateArray;
  E:TS7PlusBasketEntries;
  Problems:TS7PlusNameProblems;
begin
  L := Db4Like;
  SetLength(E, 1);
  E[0] := Entry(L, 0, 'p', 0, false);
  Problems := S7PlusBasketNameProblems(S7PlusBasketBuild(L, E), ['Button1', 'pdb4_var1']);
  AssertEquals(1, Length(Problems));
  AssertTrue(Problems[0].Kind=npExists);
  AssertEquals('pDB4_Var1', Problems[0].Name);
end;

procedure TTestS7PlusBasket.TheNamesOfTheChildrenAreChecked;
var
  L:TS7PlusCandidateArray;
  E:TS7PlusBasketEntries;
  Problems:TS7PlusNameProblems;
begin
  L := Db4Like;
  //two structs with the same prefix and the children mapped: every child name collides
  SetLength(E, 2);
  E[0] := Entry(L, 2, 'p', 0, true);
  E[1] := Entry(L, 2, 'p', 0, true);
  E[1].Name := 'Other';
  Problems := S7PlusBasketNameProblems(S7PlusBasketBuild(L, E), []);
  AssertTrue('the items and the bits repeat', Length(Problems)>=8);
  //with a different prefix the children differ
  E[1] := Entry(L, 2, 'q', 0, true);
  Problems := S7PlusBasketNameProblems(S7PlusBasketBuild(L, E), []);
  AssertEquals(0, Length(Problems));
end;

procedure TTestS7PlusBasket.ACleanBasketHasNoProblems;
var
  L:TS7PlusCandidateArray;
begin
  L := Db4Like;
  AssertEquals(0, Length(S7PlusBasketNameProblems(S7PlusBasketBuild(L, nil), ['Button1'])));
end;

procedure TTestS7PlusBasket.ATemplateFillsTheNameFromThePath;
begin
  AssertEquals('DB4_Var26_v1', S7PlusBasketApplyTemplate('{path}', 'DB4.Var26.v1', 1));
  AssertEquals('pv1_1', S7PlusBasketApplyTemplate('p{name}_{n}', 'DB4.Var26.v1', 1));
  AssertEquals('DB4x', S7PlusBasketApplyTemplate('{DB}x', 'DB4.Var26.v1', 1));
  AssertEquals('a template with no token is the name', 'fixed', S7PlusBasketApplyTemplate('fixed', 'DB4.Var1', 3));
  AssertEquals('an index of an array stays in the name', 'tagArr_2', S7PlusBasketApplyTemplate('tag{name}', 'DB4.Arr[2]', 1));
end;

procedure TTestS7PlusBasket.ATypeListFiltersByThePLCType;
begin
  AssertTrue('an empty list takes all', S7PlusTypeListMatches('', 8));
  AssertTrue('Real', S7PlusTypeListMatches('Real', 8));
  AssertTrue('the comparison ignores case and spaces', S7PlusTypeListMatches('dint, REAL', 8));
  AssertFalse('Int is not Real', S7PlusTypeListMatches('Int,DInt', 8));
  AssertTrue('DInt', S7PlusTypeListMatches('Int,DInt', 7));
end;

procedure TTestS7PlusBasket.AnElementOfAnArrayOfStructsIsAStructOfItsOwn;
var
  L:TS7PlusCandidateArray;
  K:TS7PlusBasketKind;
  I:TS7PlusImportItem;
begin
  L := StructArrayList;
  AssertTrue('var28[1] can be dragged', S7PlusBasketKindOf(L, 5, K));
  AssertTrue('as a struct', K=bkStruct);
  I := One(L, 5, 'p', 500, false);
  AssertTrue(I.IsStruct);
  AssertEquals('read by the path of the element', 'DB4.var28[1]', I.Path);
  AssertEquals('as big as an element', 8, I.Size);
  AssertEquals('pDB4_var28_1', I.CompName);
  AssertEquals(500, I.Scan);
end;

procedure TTestS7PlusBasket.AnElementOfAnArrayOfStructsMapsItsMembersOrNot;
var
  L:TS7PlusCandidateArray;
  I:TS7PlusImportItem;
begin
  L := StructArrayList;
  I := One(L, 5, 'p', 0, true);
  //Dint1, the byte of aBool and aInt
  AssertEquals('items', 3, Length(I.StructItems));
  AssertEquals('Dint1 at 0', 0, I.StructItems[0].Offset);
  AssertEquals('aInt at 6', 6, I.StructItems[2].Offset);
  AssertEquals('a bit for aBool', 1, Length(I.Bits));
  AssertEquals('pDB4_var28_1_aBool', I.Bits[0].CompName);
  AssertEquals('its byte item is at 4', 4, I.StructItems[1].Offset);
  I := One(L, 5, 'p', 0, false);
  AssertEquals('without the items', 0, Length(I.StructItems));
  AssertEquals('and no bits', 0, Length(I.Bits));
end;

procedure TTestS7PlusBasket.AnArrayElementIsATagByItsPath;
var
  L:TS7PlusCandidateArray;
  K:TS7PlusBasketKind;
  I:TS7PlusImportItem;
begin
  L := ArrayList;
  AssertTrue('Arr[0] can be dragged', S7PlusBasketKindOf(L, 5, K));
  AssertTrue('as a tag', K=bkTag);
  I := One(L, 5, 'p', 0, false);
  AssertEquals('DB6.W[0]', I.Path);
  AssertFalse('not a block', I.IsBlock);
  AssertEquals('pDB6_W_0', I.CompName);
end;

procedure TTestS7PlusBasket.AMemberInsideAStructArrayElementIsATagByItsPath;
var
  L:TS7PlusCandidateArray;
  K:TS7PlusBasketKind;
  I:TS7PlusImportItem;
begin
  L := StructArrayList;
  AssertTrue('the element itself is a struct', S7PlusBasketKindOf(L, 1, K) and (K=bkStruct));
  AssertTrue('a member of it is a tag', S7PlusBasketKindOf(L, 2, K) and (K=bkTag));
  I := One(L, 2, 'p', 0, false);
  AssertEquals('DB4.var28[0].Dint1', I.Path);
  AssertTrue(I.TagType=pttLongInt);
end;

initialization
  RegisterTest(TTestS7PlusBasket);

end.
