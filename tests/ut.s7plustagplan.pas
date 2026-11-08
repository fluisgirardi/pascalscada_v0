{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes das regras do Tag Builder do S7CommPlus: que variavel vira que tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Nao precisa de CLP nem do IDE: S7PlusTagPlan so decide a partir de TS7PlusVarInfo.
}
{$ELSE}
{:
  @abstract(Tests of the S7CommPlus Tag Builder rules: which variable becomes which tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Needs neither a PLC nor the IDE: S7PlusTagPlan only decides from TS7PlusVarInfo.
}
{$ENDIF}
unit ut.s7plustagplan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fpcunit, testregistry, Tag, ProtocolTypes, S7PlusTypeInfo, S7PlusTagPlan;

type

  { TTestS7PlusTagPlan }

  TTestS7PlusTagPlan = class(TTestCase)
  private
    function Var_(const AName:String; ASoftType:Byte):TS7PlusVarInfo;
    procedure AssertType(const What:String; ASoftType:Byte; AType:TTagType; ASwapB, ASwapW, ASwapD:Boolean);
  published
    procedure OneByteTypesNeedNoSwap;
    procedure TwoByteTypesSwapBytes;
    procedure FourByteTypesSwapBytesAndWords;
    procedure EightByteTypesSwapEverything;
    procedure SignAndFloatTypesFollowTheirSoftType;
    procedure StringsDatesAndStructsAreNotScalars;
    procedure AScalarBecomesATag;
    procedure AStructIsNotATagButItsMembersAre;
    procedure AnArrayElementIsSkipped;
    procedure AnUnsupportedTypeIsSkippedForItsOwnReason;
    procedure TheDBNumberIsReplacedByTheSymbolicName;
    procedure ADBNumberThatIsOnlyAPrefixOfAnotherNameIsNotReplaced;
    procedure AnAreaPathKeepsItsName;
    procedure OnlyMembersBelowTheFirstLevelAreNested;
    procedure TypeNamesFollowTia;
    procedure NamesBecomeValidComponentNames;
    procedure ANameThatStartsWithADigitGetsAnUnderscore;
    procedure AnEmptyPathGivesTag;
    procedure ThePrefixIsKept;
    procedure APathSplitsIntoItsLevels;
    procedure ADotInsideBracketsDoesNotSplit;
    procedure ASingleLevelIsOneSegment;
    procedure AnEmptyPathHasNoLevels;
    procedure AnArrayOfScalarsIsMarkedWithItsElementCount;
    procedure ElementsOfAnArrayStaySkipped;
    procedure AScalarWithoutElementsIsNotAnArray;
    procedure ASecondArrayDoesNotTakeTheElementsOfTheFirst;
    procedure AMultiDimensionalArrayCountsEveryElement;
    procedure AnArrayKnowsThePathOfEachElementInListingOrder;
    procedure AStructSizeIsWhatAReadOfItReturns;
    procedure ADBSizeIsTheWholeDBReadWithClassicOffsets;
    procedure AnArrayInsideAStructCountsAllItsElements;
    procedure AStringMakesTheSizeInexact;
    procedure AnAreaTagNameKeepsItsDots;
    procedure AnAreaPathIsTwoLevels;
    procedure TagInfosStartWithTheDBsAsStructures;
    procedure AScalarKeepsItsTypeAndSwapsInTheCommonFormat;
    procedure AStructureIsListedWithItsSizeAndItsMembersAfterIt;
    procedure AnArrayIsOneEntryAndItsElementsAreLeftOut;
    procedure AStringIsATkStringWithUnknownLength;
    procedure AStringBecomesATagOnceItsLengthIsKnown;
    procedure TheLengthOfAStringIsItsFirstByte;
    procedure AValueThatDoesNotFitTheLengthGivesNoLength;
    procedure AStringKeepsItsAddressToBeRead;
    procedure ASizedStringKeepsTheSizeExact;
    procedure AStringWithNoLengthReadIsListedAsAString;
    procedure AnArrayOfStringsIsNotATag;
    procedure AnArrayOfStructuresBecomesOneStructTag;
    procedure TheElementsOfAnArrayOfStructuresStayHidden;
    procedure TheMembersInsideAnElementAreTagsByPath;
    procedure TheMembersInsideAnElementAreNotInTheCommonList;
    procedure AnArrayOfStructuresHasItsSizeInTheCommonList;
    procedure AWholeDBCountsTheArrayOfStructures;
    procedure AStructureInsideTheElementIsHandledByItsLeaves;
    procedure AnArrayInsideTheElementIsHandledByItsLeaves;
    procedure OneElementIsSizedFromItsMembers;
    procedure AStringMemberOfTheElementIsMarked;
    procedure MarkingTwiceKeepsTheArrayOfStructuresAsIs;
    procedure SimpleModeMakesOneTagPerTickedScalar;
    procedure SimpleModeLeavesOutArraysStructuresAndArraysOfStructures;
    procedure SimpleModeKeepsTheMembersInsideAnElementAsTagsByPath;
    procedure SimpleModeMakesATextTagOfAString;
    procedure AnAreaVariableIsASimpleTag;
    procedure ArrayModeMakesABlockPerTickedArray;
    procedure ArrayModeNamesTheElementsOnlyWhenAsked;
    procedure ArrayModeOffersOnlyArraysOfScalars;
    procedure StructModeMakesOneStructPerTickedStructure;
    procedure StructModePlacesTheItemsByTheirDistanceInTheImage;
    procedure StructModeWithoutItemsMakesOnlyTheStructure;
    procedure StructModeOffersOnlyStructures;
    procedure StructModeItemsIncludeTheElementsOfAnArrayAndTheStrings;
    procedure StructArrayModeOffersOnlyTheMembersOfTheFirstElement;
    procedure StructArrayModeMapsTheTickedMembersToEveryIndex;
    procedure StructArrayModeNeedsAMemberTicked;
    procedure StructArrayModeLeavesOutAStringWithoutLength;
    procedure TheNamesCarryThePrefix;
    procedure AWholeDBIsOneStructureWithAnItemPerVariable;
    procedure AMemberInsideAStructureOfTheDBIsPlacedByItsDistanceInTheImage;
    procedure TheItemsOfADBAreOnlyMadeWhenAsked;
    procedure TheStructureOfADBIsTheWholeDBRead;
    procedure AnAreaIsNotADBStructure;
    procedure ADBStructureHasItsStringsAndTheElementsOfItsArrays;
    procedure ADBStructureHasTheMembersOfEveryIndexOfAnArrayOfStructures;
    procedure ATickedParentCreatesAllItsSubItems;
    procedure AParentTickedAsAGroupCreatesWhatIsUnderIt;
    procedure TheElementLevelOfAnArrayOfStructuresIsAGroup;
    procedure ATickedArrayOfStructuresMapsEveryMember;
    procedure TickedSubItemsDoNotTickTheParent;
    procedure AGroupThatIsAnArrayCoversItsElements;
    procedure ANegativeIndexIsNotTheSameNameAsThePositiveOne;
    procedure AnAreaTagHasTheAreaAsItsParent;
  end;

implementation

function TTestS7PlusTagPlan.Var_(const AName:String; ASoftType:Byte):TS7PlusVarInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Name := AName;
  Result.SoftDataType := ASoftType;
end;

procedure TTestS7PlusTagPlan.AssertType(const What:String; ASoftType:Byte; AType:TTagType;
  ASwapB, ASwapW, ASwapD:Boolean);
var
  T:TTagType;
  B, W, D:Boolean;
begin
  AssertTrue(What+' maps', S7PlusTagTypeOf(ASoftType, T, B, W, D));
  AssertTrue(What+' type', T=AType);
  AssertEquals(What+' SwapBytes', ASwapB, B);
  AssertEquals(What+' SwapWords', ASwapW, W);
  AssertEquals(What+' SwapDWords', ASwapD, D);
end;

procedure TTestS7PlusTagPlan.OneByteTypesNeedNoSwap;
begin
  AssertType('Bool', 1, pttByte, false, false, false);
  AssertType('BBool (optimized DB)', 40, pttByte, false, false, false);
  AssertType('Byte', 2, pttByte, false, false, false);
  AssertType('Char', 3, pttByte, false, false, false);
  AssertType('USInt', 52, pttByte, false, false, false);
  AssertType('SInt', 55, pttShortInt, false, false, false);
end;

procedure TTestS7PlusTagPlan.TwoByteTypesSwapBytes;
begin
  AssertType('Word', 4, pttWord, true, false, false);
  AssertType('UInt', 53, pttWord, true, false, false);
  AssertType('Int', 5, pttSmallInt, true, false, false);
end;

procedure TTestS7PlusTagPlan.FourByteTypesSwapBytesAndWords;
begin
  AssertType('DWord', 6, pttDWord, true, true, false);
  AssertType('UDInt', 54, pttDWord, true, true, false);
  AssertType('DInt', 7, pttLongInt, true, true, false);
  AssertType('Real', 8, pttFloat, true, true, false);
end;

procedure TTestS7PlusTagPlan.EightByteTypesSwapEverything;
begin
  AssertType('LWord', 51, pttQWord, true, true, true);
  AssertType('ULInt', 49, pttQWord, true, true, true);
  AssertType('LInt', 50, pttInt64, true, true, true);
  AssertType('LReal', 48, pttDouble, true, true, true);
end;

procedure TTestS7PlusTagPlan.SignAndFloatTypesFollowTheirSoftType;
var
  T:TTagType;
  B, W, D:Boolean;
begin
  //the unsigned/signed split is what makes a 32-bit value come out right
  S7PlusTagTypeOf(7, T, B, W, D);
  AssertTrue('DInt is signed', T=pttLongInt);
  S7PlusTagTypeOf(54, T, B, W, D);
  AssertTrue('UDInt is unsigned', T=pttDWord);
  S7PlusTagTypeOf(8, T, B, W, D);
  AssertTrue('Real is a float', T=pttFloat);
end;

procedure TTestS7PlusTagPlan.StringsDatesAndStructsAreNotScalars;
var
  T:TTagType;
  B, W, D:Boolean;
begin
  AssertFalse('String', S7PlusTagTypeOf(19, T, B, W, D));
  AssertFalse('WString', S7PlusTagTypeOf(62, T, B, W, D));
  AssertFalse('Date', S7PlusTagTypeOf(9, T, B, W, D));
  AssertFalse('Time', S7PlusTagTypeOf(11, T, B, W, D));
  AssertFalse('DTL', S7PlusTagTypeOf(67, T, B, W, D));
  AssertFalse('Struct', S7PlusTagTypeOf(17, T, B, W, D));
  AssertFalse('Void', S7PlusTagTypeOf(0, T, B, W, D));
end;

procedure TTestS7PlusTagPlan.AScalarBecomesATag;
var
  C:TS7PlusCandidate;
begin
  C := S7PlusClassifyVar(Var_('DB4.Var1', 7), '');
  AssertTrue('no skip', C.Skip=srNone);
  AssertEquals('path', 'DB4.Var1', C.Path);
  AssertTrue('type', C.TagType=pttLongInt);
  AssertTrue('swap bytes', C.SwapBytes);
  AssertTrue('swap words', C.SwapWords);
  AssertFalse('not nested', C.Nested);
end;

procedure TTestS7PlusTagPlan.AStructIsNotATagButItsMembersAre;
var
  C:TS7PlusCandidate;
begin
  C := S7PlusClassifyVar(Var_('DB4.Var26', 17), '');
  AssertTrue('the struct is not a scalar', C.Skip=srNotAScalar);
  C := S7PlusClassifyVar(Var_('DB4.Var26.v2', 4), '');
  AssertTrue('its member is', C.Skip=srNone);
  AssertTrue('and is nested', C.Nested);
end;

procedure TTestS7PlusTagPlan.AnArrayElementIsSkipped;
begin
  AssertTrue(S7PlusClassifyVar(Var_('DB2.Tabela[3]', 7), '').Skip=srArrayElement);
  AssertTrue(S7PlusClassifyVar(Var_('DB2.Tabela[1,2]', 7), '').Skip=srArrayElement);
end;

procedure TTestS7PlusTagPlan.AnUnsupportedTypeIsSkippedForItsOwnReason;
begin
  AssertTrue('wstring', S7PlusClassifyVar(Var_('DB4.Nome', 62), '').Skip=srUnsupportedType);
  AssertTrue('date', S7PlusClassifyVar(Var_('DB4.Quando', 9), '').Skip=srUnsupportedType);
end;

procedure TTestS7PlusTagPlan.TheDBNumberIsReplacedByTheSymbolicName;
begin
  AssertEquals('member', 'BlocoSimbolico_4.Var26.v1',
    S7PlusClassifyVar(Var_('DB4.Var26.v1', 2), 'BlocoSimbolico_4').Path);
  AssertEquals('the DB alone', 'BlocoSimbolico_4',
    S7PlusClassifyVar(Var_('DB4', 17), 'BlocoSimbolico_4').Path);
  AssertEquals('empty name keeps the number', 'DB4.Var1',
    S7PlusClassifyVar(Var_('DB4.Var1', 7), '').Path);
end;

procedure TTestS7PlusTagPlan.ADBNumberThatIsOnlyAPrefixOfAnotherNameIsNotReplaced;
begin
  //"DBx" that is not DB<number>. is left alone
  AssertEquals('DBabc', 'DBabc.Var1', S7PlusClassifyVar(Var_('DBabc.Var1', 7), 'X').Path);
  AssertEquals('DB4x', 'DB4x.Var1', S7PlusClassifyVar(Var_('DB4x.Var1', 7), 'X').Path);
end;

procedure TTestS7PlusTagPlan.AnAreaPathKeepsItsName;
var
  C:TS7PlusCandidate;
begin
  C := S7PlusClassifyVar(Var_('MArea.Clock_10Hz', 1), 'IgnoredForAreas');
  AssertEquals('path', 'MArea.Clock_10Hz', C.Path);
  AssertTrue('a Bool is a tag', C.Skip=srNone);
end;

procedure TTestS7PlusTagPlan.OnlyMembersBelowTheFirstLevelAreNested;
begin
  AssertFalse('DB4.Var1', S7PlusClassifyVar(Var_('DB4.Var1', 7), '').Nested);
  AssertTrue('DB4.Var26.v1', S7PlusClassifyVar(Var_('DB4.Var26.v1', 2), '').Nested);
  AssertTrue('DB2.itemteste.teste', S7PlusClassifyVar(Var_('DB2.itemteste.teste', 7), '').Nested);
end;

procedure TTestS7PlusTagPlan.TypeNamesFollowTia;
begin
  AssertEquals('DInt', 'DInt', S7PlusSoftTypeName(7));
  AssertEquals('Real', 'Real', S7PlusSoftTypeName(8));
  AssertEquals('Bool', 'Bool', S7PlusSoftTypeName(40));
  AssertEquals('Struct', 'Struct', S7PlusSoftTypeName(17));
  AssertEquals('unknown', '?200', S7PlusSoftTypeName(200));
end;

procedure TTestS7PlusTagPlan.NamesBecomeValidComponentNames;
begin
  AssertEquals('dots', 'DB4_Var26_v1', S7PlusSuggestedName('DB4.Var26.v1', ''));
  AssertEquals('runs of separators collapse', 'MArea_Clock_10Hz', S7PlusSuggestedName('MArea..Clock_10Hz', ''));
  AssertEquals('brackets', 'DB2_Tabela_3', S7PlusSuggestedName('DB2.Tabela[3]', ''));
  AssertTrue('valid identifier', IsValidIdent(S7PlusSuggestedName('BlocoSimbolico_4.Var26.Static_1', 'tag')));
end;

procedure TTestS7PlusTagPlan.ANameThatStartsWithADigitGetsAnUnderscore;
begin
  AssertEquals('digit first', '_4abc_x', S7PlusSuggestedName('4abc.x', ''));
end;

procedure TTestS7PlusTagPlan.AnEmptyPathGivesTag;
begin
  AssertEquals('empty', 'tag', S7PlusSuggestedName('', ''));
  AssertEquals('only separators', 'tag', S7PlusSuggestedName('...', ''));
end;

procedure TTestS7PlusTagPlan.ThePrefixIsKept;
begin
  AssertEquals('prefix', 's7_DB4_Var1', S7PlusSuggestedName('DB4.Var1', 's7_'));
end;

procedure TTestS7PlusTagPlan.APathSplitsIntoItsLevels;
var
  L:TStringArray;
begin
  L := S7PlusSplitPath('BlocoSimbolico_4.Var26.v1');
  AssertEquals('count', 3, Length(L));
  AssertEquals('db', 'BlocoSimbolico_4', L[0]);
  AssertEquals('struct', 'Var26', L[1]);
  AssertEquals('member', 'v1', L[2]);
end;

procedure TTestS7PlusTagPlan.ADotInsideBracketsDoesNotSplit;
var
  L:TStringArray;
begin
  L := S7PlusSplitPath('DB2.Tabela[1.5].x');
  AssertEquals('count', 3, Length(L));
  AssertEquals('array level', 'Tabela[1.5]', L[1]);
  AssertEquals('member', 'x', L[2]);
end;

procedure TTestS7PlusTagPlan.ASingleLevelIsOneSegment;
var
  L:TStringArray;
begin
  L := S7PlusSplitPath('DB4');
  AssertEquals('count', 1, Length(L));
  AssertEquals('segment', 'DB4', L[0]);
end;

procedure TTestS7PlusTagPlan.AnEmptyPathHasNoLevels;
begin
  AssertEquals('count', 0, Length(S7PlusSplitPath('')));
end;

function ListOf(const Names:array of String; const Types:array of Byte; ASubst:Boolean):TS7PlusCandidateArray;
var
  i:Integer;
  V:TS7PlusVarInfo;
begin
  SetLength(Result, Length(Names));
  for i:=0 to High(Names) do begin
    FillChar(V, SizeOf(V), 0);
    V.Name := Names[i];
    V.SoftDataType := Types[i];
    Result[i] := S7PlusClassifyVar(V, '');
  end;
  S7PlusMarkArrays(Result);
end;

procedure TTestS7PlusTagPlan.AnArrayOfScalarsIsMarkedWithItsElementCount;
var
  L:TS7PlusCandidateArray;
begin
  //DB5 of the test PLC: Teste, Array[0..3] of DWord (the PLC lists the array, then its elements)
  L := ListOf(['DB5.Teste', 'DB5.Teste[0]', 'DB5.Teste[1]', 'DB5.Teste[2]', 'DB5.Teste[3]'],
              [6, 6, 6, 6, 6], false);
  AssertTrue('the array is one tag', L[0].Skip=srNone);
  AssertTrue('and is an array', L[0].IsArray);
  AssertEquals('four elements', 4, L[0].ElementCount);
end;

procedure TTestS7PlusTagPlan.ElementsOfAnArrayStaySkipped;
var
  L:TS7PlusCandidateArray;
begin
  L := ListOf(['DB5.Teste', 'DB5.Teste[0]', 'DB5.Teste[1]'], [6, 6, 6], false);
  AssertTrue('element 0', L[1].Skip=srArrayElement);
  AssertTrue('element 1', L[2].Skip=srArrayElement);
  AssertFalse('an element is not an array', L[1].IsArray);
end;

procedure TTestS7PlusTagPlan.AScalarWithoutElementsIsNotAnArray;
var
  L:TS7PlusCandidateArray;
begin
  L := ListOf(['DB4.Var1', 'DB4.Var2'], [7, 7], false);
  AssertFalse('Var1', L[0].IsArray);
  AssertEquals('no elements', 0, L[0].ElementCount);
end;

procedure TTestS7PlusTagPlan.ASecondArrayDoesNotTakeTheElementsOfTheFirst;
var
  L:TS7PlusCandidateArray;
begin
  L := ListOf(['DB5.A', 'DB5.A[0]', 'DB5.A[1]', 'DB5.B', 'DB5.B[0]', 'DB5.B[1]', 'DB5.B[2]'],
              [7, 7, 7, 8, 8, 8, 8], false);
  AssertEquals('A', 2, L[0].ElementCount);
  AssertEquals('B', 3, L[3].ElementCount);
end;

procedure TTestS7PlusTagPlan.AMultiDimensionalArrayCountsEveryElement;
var
  L:TS7PlusCandidateArray;
begin
  L := ListOf(['DB5.M', 'DB5.M[0,0]', 'DB5.M[0,1]', 'DB5.M[1,0]', 'DB5.M[1,1]'],
              [5, 5, 5, 5, 5], false);
  AssertTrue('array', L[0].IsArray);
  AssertEquals('2 x 2 elements', 4, L[0].ElementCount);
end;

procedure TTestS7PlusTagPlan.AnArrayKnowsThePathOfEachElementInListingOrder;
var
  L:TS7PlusCandidateArray;
begin
  L := ListOf(['DB5.Teste', 'DB5.Teste[0]', 'DB5.Teste[1]', 'DB5.Teste[2]'], [6, 6, 6, 6], false);
  AssertEquals('count', 3, Length(L[0].ElementPaths));
  AssertEquals('first', 'DB5.Teste[0]', L[0].ElementPaths[0]);
  AssertEquals('last', 'DB5.Teste[2]', L[0].ElementPaths[2]);
  //what the wizard turns them into
  AssertEquals('component name', 'tag_DB5_Teste_2', S7PlusSuggestedName(L[0].ElementPaths[2], 'tag_'));
  AssertEquals('multi dimensional', 'DB5_M_1_0', S7PlusSuggestedName('DB5.M[1,0]', ''));
end;

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

//DB4 of the test PLC (Var25 DInt, then Var26 Struct{v1 Byte, v2 Word, v3 DWord, v4 Bool,
//Static_1 Real, Static_2..4 Bool}), optimized: the PLC lays Var26 out by size
function Db4Like:TS7PlusCandidateArray;
begin
  Result := ListSized(
    ['DB4.Var1', 'DB4.Var25', 'DB4.Var26', 'DB4.Var26.v1', 'DB4.Var26.v2', 'DB4.Var26.v3', 'DB4.Var26.v4',
     'DB4.Var26.Static_1', 'DB4.Var26.Static_2', 'DB4.Var26.Static_3', 'DB4.Var26.Static_4'],
    [7, 7, 17, 2, 4, 6, 40, 8, 40, 40, 40],
    [0, 96, 100, 100, 102, 104, 108, 110, 114, 114, 114],
    [4, 100, 104, 114, 112, 104, 115, 108, 116, 117, 118]);
end;

procedure TTestS7PlusTagPlan.AStructSizeIsWhatAReadOfItReturns;
var
  Exact:Boolean;
begin
  //v3 DWord @0, Static_1 Real @4, v2 Word @8, v1 Byte @10, four Bool @11..14 = 15 bytes
  AssertEquals('Var26', 15, S7PlusContainerSize(Db4Like, 'DB4.Var26', false, Exact));
  AssertTrue('exact', Exact);
end;

procedure TTestS7PlusTagPlan.ADBSizeIsTheWholeDBReadWithClassicOffsets;
var
  Exact:Boolean;
begin
  //Var25 @96 (4) and Var26 @100 (15 bytes): 115, what the TPLCStruct of the whole DB needs
  AssertEquals('DB4', 115, S7PlusContainerSize(Db4Like, 'DB4', true, Exact));
  AssertTrue('exact', Exact);
end;

procedure TTestS7PlusTagPlan.AnArrayInsideAStructCountsAllItsElements;
var
  L:TS7PlusCandidateArray;
  Exact:Boolean;
begin
  //S: Struct{ a: Array[0..2] of DWord, b: Byte } - the elements are covered by the array
  L := ListSized(['DB5.S', 'DB5.S.a', 'DB5.S.a[0]', 'DB5.S.a[1]', 'DB5.S.a[2]', 'DB5.S.b'],
                 [17, 6, 6, 6, 6, 2],
                 [0, 0, -1, -1, -1, 12],
                 [0, 0, -1, -1, -1, 12]);
  AssertEquals('3 x 4 + 1', 13, S7PlusContainerSize(L, 'DB5.S', false, Exact));
  AssertTrue('exact', Exact);
end;

procedure TTestS7PlusTagPlan.AStringMakesTheSizeInexact;
var
  L:TS7PlusCandidateArray;
  Exact:Boolean;
begin
  L := ListSized(['DB5.S', 'DB5.S.n', 'DB5.S.x'], [17, 19, 7], [0, 0, 256], [0, 0, 256]);
  S7PlusContainerSize(L, 'DB5.S', false, Exact);
  AssertFalse('the length of a String is not on the list', Exact);
end;

procedure TTestS7PlusTagPlan.AnAreaTagNameKeepsItsDots;
var
  L:TStringArray;
begin
  L := S7PlusSplitPath('MArea.Clock_2.5Hz');
  AssertEquals('two levels', 2, Length(L));
  AssertEquals('area', 'MArea', L[0]);
  AssertEquals('the whole name', 'Clock_2.5Hz', L[1]);
end;

procedure TTestS7PlusTagPlan.AnAreaPathIsTwoLevels;
var
  L:TStringArray;
begin
  L := S7PlusSplitPath('IArea.D01SS1~Ready (automatic)');
  AssertEquals('two levels', 2, Length(L));
  AssertEquals('name', 'D01SS1~Ready (automatic)', L[1]);
  //a DB is not an area: its members split as before
  AssertEquals('DB', 3, Length(S7PlusSplitPath('DB4.Var26.v1')));
end;

function FindInfo(const A:TPLCTagInfoArray; const APath:String):Integer;
var
  i:Integer;
begin
  Result := -1;
  for i:=0 to High(A) do
    if A[i].Path=APath then Result := i;
end;

procedure TTestS7PlusTagPlan.TagInfosStartWithTheDBsAsStructures;
var
  A:TPLCTagInfoArray;
begin
  A := S7PlusTagInfosOf(Db4Like);
  AssertTrue('the first entry is the DB', A[0].Kind=tkStructure);
  AssertEquals('path', 'DB4', A[0].Path);
  AssertEquals('group', 'DB4', A[0].Group);
  AssertEquals('what the TPLCStruct of the whole DB needs', 115, A[0].SizeInBytes);
  AssertEquals('listed once', 1, Length(A)-Length(S7PlusTagInfosOf(Db4Like))+1);
end;

procedure TTestS7PlusTagPlan.AScalarKeepsItsTypeAndSwapsInTheCommonFormat;
var
  A:TPLCTagInfoArray;
  i:Integer;
begin
  A := S7PlusTagInfosOf(Db4Like);
  i := FindInfo(A, 'DB4.Var1');
  AssertTrue('scalar', A[i].Kind=tkScalar);
  AssertTrue('DInt', A[i].TagType=pttLongInt);
  AssertTrue('big-endian PLC', A[i].SwapBytes and A[i].SwapWords);
  AssertEquals('type name', 'DInt', A[i].NativeType);
  AssertEquals('four bytes', 4, A[i].SizeInBytes);
  AssertEquals('parent is the DB', 'DB4', A[i].ParentPath);
  AssertEquals('name', 'Var1', A[i].Name);
end;

procedure TTestS7PlusTagPlan.AStructureIsListedWithItsSizeAndItsMembersAfterIt;
var
  A:TPLCTagInfoArray;
  s, m:Integer;
begin
  A := S7PlusTagInfosOf(Db4Like);
  s := FindInfo(A, 'DB4.Var26');
  m := FindInfo(A, 'DB4.Var26.v2');
  AssertTrue('structure', A[s].Kind=tkStructure);
  AssertEquals('what a read of it returns', 15, A[s].SizeInBytes);
  AssertTrue('the member comes after', m>s);
  AssertEquals('member parent', 'DB4.Var26', A[m].ParentPath);
  AssertTrue('member is a word', A[m].TagType=pttWord);
end;

procedure TTestS7PlusTagPlan.AnArrayIsOneEntryAndItsElementsAreLeftOut;
var
  L:TS7PlusCandidateArray;
  A:TPLCTagInfoArray;
  i:Integer;
begin
  L := ListOf(['DB5.Teste', 'DB5.Teste[0]', 'DB5.Teste[1]', 'DB5.Teste[2]'], [6, 6, 6, 6], false);
  A := S7PlusTagInfosOf(L);
  i := FindInfo(A, 'DB5.Teste');
  AssertTrue('array', A[i].Kind=tkArray);
  AssertEquals('three elements', 3, A[i].ElementCount);
  AssertEquals('twelve bytes', 12, A[i].SizeInBytes);
  AssertTrue('DWord', A[i].TagType=pttDWord);
  AssertEquals('the elements are not listed', -1, FindInfo(A, 'DB5.Teste[1]'));
end;

procedure TTestS7PlusTagPlan.AStringIsATkStringWithUnknownLength;
var
  L:TS7PlusCandidateArray;
  A:TPLCTagInfoArray;
  i:Integer;
begin
  L := ListOf(['DB5.Nome', 'DB5.Data'], [19, 9], false);
  A := S7PlusTagInfosOf(L);
  i := FindInfo(A, 'DB5.Nome');
  AssertTrue('string', A[i].Kind=tkString);
  AssertEquals('the maximum length is unknown', 0, A[i].ElementCount);
  AssertTrue('a date has no tag type', A[FindInfo(A, 'DB5.Data')].Kind=tkUnsupported);
end;

procedure TTestS7PlusTagPlan.AnAreaTagHasTheAreaAsItsParent;
var
  L:TS7PlusCandidateArray;
  A:TPLCTagInfoArray;
  i:Integer;
begin
  L := ListOf(['MArea.Clock_2.5Hz'], [1], false);
  A := S7PlusTagInfosOf(L);
  AssertEquals('no DB entry for an area', 1, Length(A));
  AssertEquals('name keeps its dot', 'Clock_2.5Hz', A[0].Name);
  AssertEquals('parent', 'MArea', A[0].ParentPath);
  AssertEquals('group', 'MArea', A[0].Group);
  AssertTrue('a bool reads as a byte', A[0].TagType=pttByte);
end;

procedure TTestS7PlusTagPlan.AStringBecomesATagOnceItsLengthIsKnown;
var
  C:TS7PlusCandidate;
  L:TS7PlusCandidateArray;
  A:TPLCTagInfoArray;
  i:Integer;
begin
  C := S7PlusClassifyVar(Var_('DB4.Nome', 19), '');
  AssertTrue('a string is a tag', C.Skip=srNone);
  AssertTrue('a TPLCString', C.IsString);
  AssertEquals('length not read yet', -1, C.StringLength);
  L := ListOf(['DB5.Nome'], [19], false);
  L[0].StringLength := 50;
  A := S7PlusTagInfosOf(L);
  i := FindInfo(A, 'DB5.Nome');
  AssertTrue('string', A[i].Kind=tkString);
  AssertEquals('maximum length', 50, A[i].ElementCount);
  AssertEquals('maximum, current length and the characters', 52, A[i].SizeInBytes);
end;

procedure TTestS7PlusTagPlan.TheLengthOfAStringIsItsFirstByte;
var
  D:TBytes;
  Len:LongInt;
begin
  //the value of a String[50]: maximum, current length, 50 characters
  SetLength(D, 52);
  FillChar(D[0], 52, 0);
  D[0] := 50;
  D[1] := 10;
  AssertTrue(S7PlusStringLengthOf(D, Len));
  AssertEquals(50, Len);
  //the default String[254]
  SetLength(D, 256);
  FillChar(D[0], 256, 0);
  D[0] := 254;
  AssertTrue(S7PlusStringLengthOf(D, Len));
  AssertEquals(254, Len);
end;

procedure TTestS7PlusTagPlan.AValueThatDoesNotFitTheLengthGivesNoLength;
var
  D:TBytes;
  Len:LongInt;
begin
  SetLength(D, 0);
  AssertFalse('nothing', S7PlusStringLengthOf(D, Len));
  AssertEquals(-1, Len);
  SetLength(D, 1);
  D[0] := 5;
  AssertFalse('one byte', S7PlusStringLengthOf(D, Len));
  SetLength(D, 10);
  FillChar(D[0], 10, 0);
  D[0] := 50;
  AssertFalse('claims 50 characters in 10 bytes', S7PlusStringLengthOf(D, Len));
end;

procedure TTestS7PlusTagPlan.AStringKeepsItsAddressToBeRead;
var
  V:TS7PlusVarInfo;
  C:TS7PlusCandidate;
begin
  V := Var_('DB4.Var26.ItemStruct', 19);
  SetLength(V.Lids, 2);
  V.Lids[0] := $1A;
  V.Lids[1] := $16;
  C := S7PlusClassifyVar(V, '', $8A0E0004);
  AssertEquals($8A0E0004, C.AccessArea);
  AssertEquals(2, Length(C.Lids));
  AssertEquals($16, C.Lids[1]);
end;

procedure TTestS7PlusTagPlan.ASizedStringKeepsTheSizeExact;
var
  L:TS7PlusCandidateArray;
  Exact:Boolean;
  Size:LongInt;
begin
  L := ListSized(['DB5.S', 'DB5.S.n', 'DB5.S.x'], [17, 19, 7], [0, 0, 56], [0, 0, 56]);
  L[1].StringLength := 50;
  Size := S7PlusContainerSize(L, 'DB5.S', false, Exact);
  AssertTrue('the length is known now', Exact);
  AssertEquals('the string (52) and a DInt after it', 56+4, Size);
end;

procedure TTestS7PlusTagPlan.AStringWithNoLengthReadIsListedAsAString;
var
  L:TS7PlusCandidateArray;
  A:TPLCTagInfoArray;
  i:Integer;
begin
  L := ListOf(['DB5.Nome'], [19], false);
  L[0].IsString := false;
  L[0].Skip := srStringLength;
  A := S7PlusTagInfosOf(L);
  i := FindInfo(A, 'DB5.Nome');
  AssertTrue(A[i].Kind=tkString);
  AssertEquals(0, A[i].ElementCount);
end;

procedure TTestS7PlusTagPlan.AnArrayOfStringsIsNotATag;
var
  L:TS7PlusCandidateArray;
begin
  L := ListOf(['DB5.Nomes', 'DB5.Nomes[0]', 'DB5.Nomes[1]'], [19, 19, 19], false);
  S7PlusMarkArrays(L);
  AssertTrue('the array of strings is not a tag', L[0].Skip=srUnsupportedType);
  AssertFalse('and not a TPLCString', L[0].IsString);
  AssertFalse('nor a block', L[0].IsArray);
end;

function StructArrayList(WithString:Boolean):TS7PlusCandidateArray;
begin
  //the way the PLC lists DB4.var28: Array[0..1] of Struct (Dint1 DInt, aBool Bool, aInt Int)
  if WithString then
    Result := ListSized(['DB4.var28', 'DB4.var28[0]', 'DB4.var28[0].Dint1', 'DB4.var28[0].Nome',
                         'DB4.var28[1]', 'DB4.var28[1].Dint1', 'DB4.var28[1].Nome'],
                        [17, 17, 7, 19, 17, 7, 19],
                        [526, 526, 526, 530, 584, 584, 588],
                        [276, -1, -1, -1, -1, -1, -1])
  else
    Result := ListSized(['DB4.var28', 'DB4.var28[0]', 'DB4.var28[0].Dint1', 'DB4.var28[0].aBool', 'DB4.var28[0].aInt',
                         'DB4.var28[1]', 'DB4.var28[1].Dint1', 'DB4.var28[1].aBool', 'DB4.var28[1].aInt'],
                        [17, 17, 7, 40, 5, 17, 7, 40, 5],
                        [526, 526, 526, 530, 532, 534, 534, 538, 540],
                        [276, -1, -1, -1, -1, -1, -1, -1, -1]);
end;

procedure TTestS7PlusTagPlan.AnArrayOfStructuresBecomesOneStructTag;
var
  L:TS7PlusCandidateArray;
begin
  L := StructArrayList(false);
  AssertTrue('the array is a struct tag', L[0].IsStructArray);
  AssertTrue('and a tag', L[0].Skip=srNone);
  AssertEquals('elements', 2, L[0].ElementCount);
  AssertEquals('the PLC stride between two elements', 8, L[0].ElementSize);
  AssertEquals('members of one element', 3, Length(L[0].Members));
  AssertEquals('Dint1 at 0', 0, L[0].Members[0].Offset);
  AssertTrue('Dint1 is a DInt', L[0].Members[0].TagType=pttLongInt);
  AssertTrue('and is swapped', L[0].Members[0].SwapBytes and L[0].Members[0].SwapWords);
  AssertEquals('aBool at 4', 4, L[0].Members[1].Offset);
  AssertTrue('a Bool is a byte', L[0].Members[1].TagType=pttByte);
  AssertEquals('aInt at 6', 6, L[0].Members[2].Offset);
  AssertTrue('aInt is an Int', L[0].Members[2].TagType=pttSmallInt);
  AssertEquals('the path of each element', 'DB4.var28[1]', L[0].ElementPaths[1]);
end;

procedure TTestS7PlusTagPlan.TheElementsOfAnArrayOfStructuresStayHidden;
var
  L:TS7PlusCandidateArray;
  i:Integer;
begin
  L := StructArrayList(false);
  for i:=1 to High(L) do
    if L[i].Path[Length(L[i].Path)]=']' then
      AssertTrue(L[i].Path+' is not a tag of its own', L[i].Skip=srArrayElement);
end;

procedure TTestS7PlusTagPlan.TheMembersInsideAnElementAreTagsByPath;
var
  L:TS7PlusCandidateArray;
begin
  L := StructArrayList(false);
  AssertTrue('a member', L[2].Skip=srNone);
  AssertTrue('read by its path', L[2].ViaPath);
  AssertTrue('with its type', L[2].TagType=pttLongInt);
  AssertFalse('the array itself is not', L[0].ViaPath);
  AssertTrue('a STRING inside an element is not offered (its length would need a read of each)',
             S7PlusClassifyVar(Var_('DB4.arr[0].Nome', 19), '').Skip=srUnsupportedType);
  AssertTrue('a structure inside an element is only a level',
             S7PlusClassifyVar(Var_('DB4.arr[0].in', 17), '').Skip=srNotAScalar);
end;

procedure TTestS7PlusTagPlan.TheMembersInsideAnElementAreNotInTheCommonList;
var
  A:TPLCTagInfoArray;
  i:Integer;
begin
  A := S7PlusTagInfosOf(StructArrayList(false));
  for i:=0 to High(A) do
    AssertTrue(A[i].Path+' is not listed', Pos('[', A[i].Path)=0);
end;

procedure TTestS7PlusTagPlan.AnArrayOfStructuresHasItsSizeInTheCommonList;
var
  L:TS7PlusCandidateArray;
  A:TPLCTagInfoArray;
  i:Integer;
begin
  L := StructArrayList(false);
  A := S7PlusTagInfosOf(L);
  i := FindInfo(A, 'DB4.var28');
  AssertTrue('a structure', A[i].Kind=tkStructure);
  AssertEquals('elements', 2, A[i].ElementCount);
  AssertEquals('bytes', 16, A[i].SizeInBytes);
end;

procedure TTestS7PlusTagPlan.AWholeDBCountsTheArrayOfStructures;
var
  L:TS7PlusCandidateArray;
  Exact:Boolean;
begin
  L := StructArrayList(false);
  AssertEquals('the array starts at 526 and has 2 x 8 bytes', 526+16, S7PlusContainerSize(L, 'DB4', true, Exact));
  AssertTrue(Exact);
end;

procedure TTestS7PlusTagPlan.AStructureInsideTheElementIsHandledByItsLeaves;
var
  L:TS7PlusCandidateArray;
begin
  //arr[0] at 10 and arr[1] at 22: the stride is 12; the structure "in" holds x at 14
  L := ListSized(['DB4.arr', 'DB4.arr[0]', 'DB4.arr[0].a', 'DB4.arr[0].in', 'DB4.arr[0].in.x',
                  'DB4.arr[1]', 'DB4.arr[1].a', 'DB4.arr[1].in', 'DB4.arr[1].in.x'],
                 [17, 17, 7, 17, 7, 17, 7, 17, 7],
                 [10, 10, 10, 14, 14, 22, 22, 26, 26], [0, -1, -1, -1, -1, -1, -1, -1, -1]);
  AssertTrue('now an array of structures', L[0].IsStructArray);
  AssertEquals('stride', 12, L[0].ElementSize);
  AssertEquals('the leaves are the members, the structure is not', 2, Length(L[0].Members));
  AssertEquals('a', 'a', L[0].Members[0].Name);
  AssertEquals('in.x', 'in.x', L[0].Members[1].Name);
  AssertEquals('in.x is 4 bytes after a', 4, L[0].Members[1].Offset);
end;

procedure TTestS7PlusTagPlan.AnArrayInsideTheElementIsHandledByItsLeaves;
var
  L:TS7PlusCandidateArray;
begin
  //the shape of a recipe DB: Proc[0..1], each with Rec[1..2] of {w DInt, f Bool}, then an Int
  L := ListSized(['DB4.Proc', 'DB4.Proc[0]', 'DB4.Proc[0].Rec', 'DB4.Proc[0].Rec[1]', 'DB4.Proc[0].Rec[1].w',
                  'DB4.Proc[0].Rec[1].f', 'DB4.Proc[0].Rec[2]', 'DB4.Proc[0].Rec[2].w', 'DB4.Proc[0].Rec[2].f',
                  'DB4.Proc[0].n',
                  'DB4.Proc[1]', 'DB4.Proc[1].Rec', 'DB4.Proc[1].Rec[1]', 'DB4.Proc[1].Rec[1].w',
                  'DB4.Proc[1].Rec[1].f', 'DB4.Proc[1].Rec[2]', 'DB4.Proc[1].Rec[2].w', 'DB4.Proc[1].Rec[2].f',
                  'DB4.Proc[1].n'],
                 [17, 17, 17, 17, 7, 40, 17, 7, 40, 5, 17, 17, 17, 7, 40, 17, 7, 40, 5],
                 [0, 0, 0, 0, 0, 4, 6, 6, 10, 12, 14, 14, 14, 14, 18, 20, 20, 24, 26],
                 [0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1]);
  AssertTrue(L[0].IsStructArray);
  AssertEquals('elements', 2, L[0].ElementCount);
  AssertEquals('stride', 14, L[0].ElementSize);
  AssertEquals('leaves of one element', 5, Length(L[0].Members));
  AssertEquals('Rec[2].f', 'Rec[2].f', L[0].Members[3].Name);
  AssertEquals('at 10', 10, L[0].Members[3].Offset);
  AssertEquals('n', 12, L[0].Members[4].Offset);
end;

procedure TTestS7PlusTagPlan.OneElementIsSizedFromItsMembers;
var
  L:TS7PlusCandidateArray;
begin
  L := ListSized(['DB4.one', 'DB4.one[0]', 'DB4.one[0].Dint1', 'DB4.one[0].aBool', 'DB4.one[0].aInt'],
                 [17, 17, 7, 40, 5], [100, 100, 100, 104, 106], [0, -1, -1, -1, -1]);
  AssertTrue(L[0].IsStructArray);
  AssertEquals('the end of aInt (8), already even', 8, L[0].ElementSize);
  L := ListSized(['DB4.one', 'DB4.one[0]', 'DB4.one[0].Dint1', 'DB4.one[0].aBool'],
                 [17, 17, 7, 40], [100, 100, 100, 104], [0, -1, -1, -1]);
  AssertEquals('5 bytes rounded up to even', 6, L[0].ElementSize);
end;

procedure TTestS7PlusTagPlan.AStringMemberOfTheElementIsMarked;
var
  L:TS7PlusCandidateArray;
begin
  L := StructArrayList(true);
  AssertTrue(L[0].IsStructArray);
  AssertEquals('the stride is what separates the elements', 58, L[0].ElementSize);
  AssertEquals(2, Length(L[0].Members));
  AssertTrue('Nome is a string', L[0].Members[1].IsString);
  AssertTrue('and an item', L[0].Members[1].Supported);
  AssertEquals('its length is read from the PLC', -1, L[0].Members[1].StringLength);
  AssertEquals('at 4', 4, L[0].Members[1].Offset);
end;

procedure TTestS7PlusTagPlan.MarkingTwiceKeepsTheArrayOfStructuresAsIs;
var
  L:TS7PlusCandidateArray;
begin
  L := StructArrayList(false);
  S7PlusMarkArrays(L);
  AssertTrue('still a struct array', L[0].IsStructArray);
  AssertFalse('and not a block of scalars', L[0].IsArray);
  AssertEquals(2, L[0].ElementCount);
end;

function AllChecked(N:Integer):TBooleanDynArray;
var
  i:Integer;
begin
  SetLength(Result, N);
  for i:=0 to N-1 do
    Result[i] := true;
end;

function Ticked(N:Integer; const Idx:array of Integer):TBooleanDynArray;
var
  i:Integer;
begin
  SetLength(Result, N);
  for i:=0 to N-1 do
    Result[i] := false;
  for i:=0 to High(Idx) do
    Result[Idx[i]] := true;
end;

procedure TTestS7PlusTagPlan.SimpleModeMakesOneTagPerTickedScalar;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := Db4Like;
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [0, 3]), imSimple, 'tag', 500, true);
  AssertEquals('two loose tags, no structure around them', 2, Length(P));
  AssertFalse(P[0].IsStruct);
  AssertFalse(P[0].IsBlock);
  AssertEquals('DB4.Var1', P[0].Path);
  AssertTrue('a DInt', P[0].TagType=pttLongInt);
  AssertTrue('big-endian from the PLC', P[0].SwapBytes and P[0].SwapWords);
  AssertEquals('by its path, also inside a structure', 'DB4.Var26.v1', P[1].Path);
  AssertEquals('scan', 500, P[0].Scan);
end;

procedure TTestS7PlusTagPlan.SimpleModeLeavesOutArraysStructuresAndArraysOfStructures;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
  i:Integer;
begin
  L := ListSized(['DB5.Teste', 'DB5.Teste[0]', 'DB5.Teste[1]', 'DB5.S', 'DB5.S.a', 'DB5.x'],
                 [6, 6, 6, 17, 7, 7], [0, -1, -1, 8, 8, 12], [0, -1, -1, 8, 8, 12]);
  P := S7PlusBuildImportPlan(L, AllChecked(Length(L)), imSimple, 'tag', 0, true);
  AssertEquals('only S.a and x', 2, Length(P));
  AssertEquals('DB5.S.a', P[0].Path);
  AssertEquals('DB5.x', P[1].Path);
  L := StructArrayList(false);
  P := S7PlusBuildImportPlan(L, AllChecked(Length(L)), imSimple, 'tag', 0, true);
  for i:=0 to High(P) do
    AssertTrue(P[i].Path+' is not the array', P[i].Path<>'DB4.var28');
end;

procedure TTestS7PlusTagPlan.SimpleModeKeepsTheMembersInsideAnElementAsTagsByPath;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := StructArrayList(false);
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [6]), imSimple, 'tag', 500, true);
  AssertEquals('one tag', 1, Length(P));
  AssertFalse(P[0].IsStruct);
  AssertEquals('by its path', 'DB4.var28[1].Dint1', P[0].Path);
  AssertTrue('a DInt', P[0].TagType=pttLongInt);
  AssertEquals('scan', 500, P[0].Scan);
end;

procedure TTestS7PlusTagPlan.SimpleModeMakesATextTagOfAString;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := ListSized(['DB4.Nome', 'DB4.Outra'], [19, 19], [10, 70], [10, 70]);
  L[0].StringLength := 50;
  P := S7PlusBuildImportPlan(L, AllChecked(2), imSimple, 'tag', 0, true);
  AssertEquals('the one with no length read yet is left out', 1, Length(P));
  AssertTrue('a text tag', P[0].IsString);
  AssertEquals('of 50 characters', 50, P[0].StringSize);
end;

procedure TTestS7PlusTagPlan.AnAreaVariableIsASimpleTag;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := ListSized(['MArea.Clock_10Hz'], [1], [0], [0]);
  P := S7PlusBuildImportPlan(L, AllChecked(1), imSimple, 'tag', 0, true);
  AssertEquals(1, Length(P));
  AssertEquals('MArea.Clock_10Hz', P[0].Path);
  AssertFalse(P[0].IsStruct);
end;

procedure TTestS7PlusTagPlan.ArrayModeMakesABlockPerTickedArray;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := ListSized(['DB5.Teste', 'DB5.Teste[0]', 'DB5.Teste[1]', 'DB5.Teste[2]', 'DB5.x'],
                 [6, 6, 6, 6, 7], [0, -1, -1, -1, 12], [0, -1, -1, -1, 12]);
  P := S7PlusBuildImportPlan(L, AllChecked(Length(L)), imArray, 'tag', 250, true);
  AssertEquals('one block; the DInt is not an array', 1, Length(P));
  AssertTrue(P[0].IsBlock);
  AssertEquals('DB5.Teste', P[0].Path);
  AssertEquals('three elements', 3, P[0].Size);
  AssertEquals(250, P[0].Scan);
  AssertEquals('an element name each', 3, Length(P[0].ElementNames));
end;

procedure TTestS7PlusTagPlan.ArrayModeNamesTheElementsOnlyWhenAsked;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := ListSized(['DB5.Teste', 'DB5.Teste[0]', 'DB5.Teste[1]'], [6, 6, 6], [0, -1, -1], [0, -1, -1]);
  P := S7PlusBuildImportPlan(L, AllChecked(3), imArray, 'tag', 0, false);
  AssertEquals(1, Length(P));
  AssertEquals('no elements', 0, Length(P[0].ElementNames));
  AssertEquals('but the block has its size', 2, P[0].Size);
end;

procedure TTestS7PlusTagPlan.ArrayModeOffersOnlyArraysOfScalars;
var
  L:TS7PlusCandidateArray;
begin
  L := ListSized(['DB5.Teste', 'DB5.Teste[0]', 'DB5.Teste[1]', 'DB5.x'], [6, 6, 6, 7], [0, -1, -1, 12], [0, -1, -1, 12]);
  AssertTrue('the array', S7PlusSelectable(L[0], imArray, false));
  AssertFalse('an element', S7PlusSelectable(L[1], imArray, false));
  AssertFalse('a scalar', S7PlusSelectable(L[3], imArray, false));
  AssertFalse('and the array is not a simple tag', S7PlusSelectable(L[0], imSimple, false));
  AssertTrue('the scalar is', S7PlusSelectable(L[3], imSimple, false));
end;

procedure TTestS7PlusTagPlan.StructModeMakesOneStructPerTickedStructure;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
  Exact:Boolean;
begin
  L := Db4Like;
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [2]), imStruct, 'tag', 0, true);
  AssertEquals('one structure', 1, Length(P));
  AssertTrue(P[0].IsStruct);
  AssertEquals('read by the path of the structure', 'DB4.Var26', P[0].Path);
  AssertEquals('what a read of it returns', S7PlusContainerSize(L, 'DB4.Var26', false, Exact), P[0].Size);
  AssertEquals('an item per member', 8, Length(P[0].StructItems));
end;

procedure TTestS7PlusTagPlan.StructModePlacesTheItemsByTheirDistanceInTheImage;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
  i:Integer;
begin
  //Var26 is at image 104; v3 (DWord) is the first of it, v1 (Byte) comes after the others
  L := Db4Like;
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [2]), imStruct, 'tag', 0, true);
  for i:=0 to High(P[0].StructItems) do
    if P[0].StructItems[i].CompName='tagDB4_Var26_v1' then AssertEquals('v1: 114 - 104', 10, P[0].StructItems[i].Offset)
    else if P[0].StructItems[i].CompName='tagDB4_Var26_v3' then AssertEquals('v3: 104 - 104', 0, P[0].StructItems[i].Offset);
end;

procedure TTestS7PlusTagPlan.StructModeWithoutItemsMakesOnlyTheStructure;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := Db4Like;
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [2]), imStruct, 'tag', 0, false);
  AssertEquals(1, Length(P));
  AssertEquals('no items', 0, Length(P[0].StructItems));
  AssertTrue('but it still has its size', P[0].Size>0);
end;

procedure TTestS7PlusTagPlan.StructModeOffersOnlyStructures;
var
  L:TS7PlusCandidateArray;
begin
  L := Db4Like;
  AssertFalse('a scalar', S7PlusSelectable(L[0], imStruct, false));
  AssertTrue('the structure', S7PlusSelectable(L[2], imStruct, false));
  AssertFalse('a member', S7PlusSelectable(L[3], imStruct, false));
  L := StructArrayList(false);
  AssertFalse('an array of structures is another mode', S7PlusSelectable(L[0], imStruct, false));
  AssertFalse('and so is an element of it', S7PlusSelectable(L[1], imStruct, false));
end;

procedure TTestS7PlusTagPlan.StructModeItemsIncludeTheElementsOfAnArrayAndTheStrings;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := ListSized(['DB5.S', 'DB5.S.a', 'DB5.S.Arr', 'DB5.S.Arr[0]', 'DB5.S.Arr[1]', 'DB5.S.Nome'],
                 [17, 7, 6, 6, 6, 19], [20, 20, 24, -1, -1, 32], [20, 20, 24, -1, -1, 32]);
  L[5].StringLength := 10;
  P := S7PlusBuildImportPlan(L, AllChecked(Length(L)), imStruct, 'tag', 0, true);
  AssertEquals('a, two elements, the text', 4, Length(P[0].StructItems));
  AssertEquals('Arr[1] at 4 + 4', 8, P[0].StructItems[2].Offset);
  AssertTrue('the text', P[0].StructItems[3].IsString);
  AssertEquals('at 12', 12, P[0].StructItems[3].Offset);
  AssertEquals('and the structure holds it (12 + 10 + 2)', 24, P[0].Size);
  P := S7PlusBuildImportPlan(L, AllChecked(Length(L)), imStruct, 'tag', 0, false);
  AssertEquals('without the option, no items', 0, Length(P[0].StructItems));
end;

procedure TTestS7PlusTagPlan.StructArrayModeOffersOnlyTheMembersOfTheFirstElement;
var
  L:TS7PlusCandidateArray;
  ArrOf, MemOf:TIntegerDynArray;
begin
  L := StructArrayList(false);
  S7PlusMapStructArrayMembers(L, ArrOf, MemOf);
  AssertEquals('the array itself is the parent, not a member', -1, MemOf[0]);
  AssertEquals('var28[0].Dint1', 0, MemOf[2]);
  AssertEquals('var28[0].aBool', 1, MemOf[3]);
  AssertEquals('var28[0].aInt', 2, MemOf[4]);
  AssertEquals('of the array 0', 0, ArrOf[4]);
  AssertEquals('var28[1] is not offered', -1, MemOf[6]);
  AssertTrue('a member is selectable', S7PlusSelectable(L[2], imStructArray, MemOf[2]>=0));
  AssertFalse('a member of the second element is not', S7PlusSelectable(L[6], imStructArray, MemOf[6]>=0));
  AssertFalse('the array is not', S7PlusSelectable(L[0], imStructArray, false));
  AssertTrue('but it is the head to show', S7PlusIsStructArrayHead(L[0]));
end;

procedure TTestS7PlusTagPlan.StructArrayModeMapsTheTickedMembersToEveryIndex;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := StructArrayList(false);
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [2, 4]), imStructArray, 'tag', 0, false);
  AssertEquals('one structure for the array', 1, Length(P));
  AssertTrue(P[0].IsStruct);
  AssertEquals('DB4.var28', P[0].Path);
  AssertEquals('the whole array: 2 x 8', 16, P[0].Size);
  AssertEquals('2 members x 2 indexes', 4, Length(P[0].StructItems));
  AssertEquals('[0].Dint1', 0, P[0].StructItems[0].Offset);
  AssertEquals('[0].aInt', 6, P[0].StructItems[1].Offset);
  AssertEquals('[1].Dint1', 8, P[0].StructItems[2].Offset);
  AssertEquals('[1].aInt', 14, P[0].StructItems[3].Offset);
  AssertEquals('named by the element', 'tagDB4_var28_1_Dint1', P[0].StructItems[2].CompName);
  AssertTrue('typed from the member', P[0].StructItems[1].TagType=pttSmallInt);
end;

procedure TTestS7PlusTagPlan.StructArrayModeNeedsAMemberTicked;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := StructArrayList(false);
  //the second element is not offered, and the element is only a level: nothing to map
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [5, 6]), imStructArray, 'tag', 0, true);
  AssertEquals(0, Length(P));
end;

procedure TTestS7PlusTagPlan.StructArrayModeLeavesOutAStringWithoutLength;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := StructArrayList(true);
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [3]), imStructArray, 'tag', 0, true);
  AssertEquals('only a text whose length is unknown: nothing to read', 0, Length(P));
  L[0].Members[1].StringLength := 20;
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [3]), imStructArray, 'tag', 0, true);
  AssertEquals(1, Length(P));
  AssertEquals(2, Length(P[0].StructItems));
  AssertTrue(P[0].StructItems[0].IsString);
  AssertEquals(20, P[0].StructItems[0].StringSize);
  AssertEquals('Nome at 4 of the second element too: 58 + 4', 62, P[0].StructItems[1].Offset);
end;

procedure TTestS7PlusTagPlan.TheNamesCarryThePrefix;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := Db4Like;
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [2]), imStruct, 'meu', 0, true);
  AssertEquals('the structure', 'meuDB4_Var26', P[0].CompName);
  AssertEquals('the item', 'meuDB4_Var26_v1', P[0].StructItems[0].CompName);
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [0]), imSimple, 'meu', 0, true);
  AssertEquals('the tag', 'meuDB4_Var1', P[0].CompName);
end;

procedure TTestS7PlusTagPlan.AWholeDBIsOneStructureWithAnItemPerVariable;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := ListSized(['DB4.Var1', 'DB4.Var2', 'DB4.Var3'], [7, 7, 7], [0, 4, 8], [4, 8, 12]);
  P := S7PlusBuildDBStructPlan(L, 'DB4', 'tag', 0, true);
  AssertEquals(1, Length(P));
  AssertTrue('a structure', P[0].IsStruct);
  AssertEquals('read by the DB name', 'DB4', P[0].Path);
  AssertEquals('an item per variable', 3, Length(P[0].StructItems));
  AssertEquals('Var2 at byte 4', 4, P[0].StructItems[1].Offset);
  AssertTrue('a DInt', P[0].StructItems[1].TagType=pttLongInt);
  AssertTrue('big-endian from the PLC', P[0].StructItems[1].SwapBytes and P[0].StructItems[1].SwapWords);
  AssertEquals('scanned as asked', 0, P[0].Scan);
  AssertEquals('the whole DB read', 12, P[0].Size);
end;

procedure TTestS7PlusTagPlan.AMemberInsideAStructureOfTheDBIsPlacedByItsDistanceInTheImage;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  //Var26 at the classic byte 100 and image 104; v3 (DWord) is the first of it, v1 (Byte) after the others
  L := ListSized(['DB4.Var26', 'DB4.Var26.v1', 'DB4.Var26.v3'], [17, 2, 6], [100, 100, 104], [104, 114, 104]);
  P := S7PlusBuildDBStructPlan(L, 'DB4', 'tag', 0, true);
  AssertEquals('the structure itself is not an item', 2, Length(P[0].StructItems));
  AssertEquals('v1: 100 + (114 - 104)', 110, P[0].StructItems[0].Offset);
  AssertEquals('v3: 100 + 0', 100, P[0].StructItems[1].Offset);
end;

procedure TTestS7PlusTagPlan.TheItemsOfADBAreOnlyMadeWhenAsked;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := ListSized(['DB4.Var1', 'DB4.Var2'], [7, 7], [0, 4], [4, 8]);
  P := S7PlusBuildDBStructPlan(L, 'DB4', 'tag', 0, false);
  AssertEquals(1, Length(P));
  AssertEquals('no items', 0, Length(P[0].StructItems));
  AssertEquals('but the whole DB is read', 8, P[0].Size);
end;

procedure TTestS7PlusTagPlan.TheStructureOfADBIsTheWholeDBRead;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := ListSized(['DB4.Var1', 'DB4.Var2', 'DB5.Var1'], [7, 7, 7], [0, 4, 0], [4, 8, 4]);
  P := S7PlusBuildDBStructPlan(L, 'DB4', 'meu', 0, true);
  AssertEquals('only the variables of DB4', 2, Length(P[0].StructItems));
  AssertEquals('meuDB4', P[0].CompName);
  AssertEquals('meuDB4_Var2', P[0].StructItems[1].CompName);
end;

procedure TTestS7PlusTagPlan.AnAreaIsNotADBStructure;
var
  L:TS7PlusCandidateArray;
begin
  L := ListSized(['MArea.Clock_10Hz'], [1], [0], [0]);
  AssertEquals(0, Length(S7PlusBuildDBStructPlan(L, 'MArea', 'tag', 0, true)));
end;

procedure TTestS7PlusTagPlan.ADBStructureHasItsStringsAndTheElementsOfItsArrays;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := ListSized(['DB4.Nome', 'DB4.Outra', 'DB4.Teste', 'DB4.Teste[0]', 'DB4.Teste[1]'], [19, 19, 6, 6, 6],
                 [10, 70, 100, -1, -1], [10, 70, 100, -1, -1]);
  L[0].StringLength := 50;
  P := S7PlusBuildDBStructPlan(L, 'DB4', 'tag', 0, true);
  AssertEquals('the string with no length read is left out; the two elements are in', 3, Length(P[0].StructItems));
  AssertTrue('a text item', P[0].StructItems[0].IsString);
  AssertEquals(50, P[0].StructItems[0].StringSize);
  AssertEquals('Teste[1] at 100 + 4', 104, P[0].StructItems[2].Offset);
end;

procedure TTestS7PlusTagPlan.ADBStructureHasTheMembersOfEveryIndexOfAnArrayOfStructures;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := StructArrayList(false);
  P := S7PlusBuildDBStructPlan(L, 'DB4', 'tag', 0, true);
  AssertEquals('2 elements x 3 members', 6, Length(P[0].StructItems));
  AssertEquals('var28[0].Dint1', 526, P[0].StructItems[0].Offset);
  AssertEquals('var28[1].aInt', 540, P[0].StructItems[5].Offset);
  AssertEquals('the DB read covers the array', 526+16, P[0].Size);
end;

procedure TTestS7PlusTagPlan.ATickedParentCreatesAllItsSubItems;
var
  L:TS7PlusCandidateArray;
  E:TBooleanDynArray;
begin
  L := Db4Like;
  //only the structure ticked: every member under it counts as ticked, the rest does not
  E := S7PlusEffectiveChecked(L, Ticked(Length(L), [2]), []);
  AssertFalse('Var1 is not under it', E[0]);
  AssertTrue('the structure', E[2]);
  AssertTrue('v1', E[3]);
  AssertTrue('Static_4', E[10]);
end;

procedure TTestS7PlusTagPlan.AParentTickedAsAGroupCreatesWhatIsUnderIt;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := Db4Like;
  //Var26 is a level of the tree (not a tag in the simple mode): ticked as a group
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), []), ['DB4.Var26'], imSimple, 'tag', 0, true);
  AssertEquals('the 8 members of Var26', 8, Length(P));
  AssertEquals('DB4.Var26.v1', P[0].Path);
  //the whole DB ticked
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), []), ['DB4'], imSimple, 'tag', 0, true);
  AssertEquals('every scalar of the DB', 10, Length(P));
end;

procedure TTestS7PlusTagPlan.TheElementLevelOfAnArrayOfStructuresIsAGroup;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := StructArrayList(false);
  //the level "var28[1]" of the tree: the simple tags of that element only
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), []), ['DB4.var28[1]'], imSimple, 'tag', 0, true);
  AssertEquals(3, Length(P));
  AssertEquals('DB4.var28[1].Dint1', P[0].Path);
end;

procedure TTestS7PlusTagPlan.ATickedArrayOfStructuresMapsEveryMember;
var
  L:TS7PlusCandidateArray;
  P:TS7PlusImportItems;
begin
  L := StructArrayList(false);
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), [0]), imStructArray, 'tag', 0, true);
  AssertEquals(1, Length(P));
  AssertEquals('3 members x 2 indexes', 6, Length(P[0].StructItems));
  //and the same when the array is covered by its DB
  P := S7PlusBuildImportPlan(L, Ticked(Length(L), []), ['DB4'], imStructArray, 'tag', 0, true);
  AssertEquals(6, Length(P[0].StructItems));
end;

procedure TTestS7PlusTagPlan.TickedSubItemsDoNotTickTheParent;
var
  L:TS7PlusCandidateArray;
  E:TBooleanDynArray;
begin
  L := Db4Like;
  E := S7PlusEffectiveChecked(L, Ticked(Length(L), [3]), []);
  AssertTrue(E[3]);
  AssertFalse('the structure above is not ticked', E[2]);
  AssertFalse('nor the siblings', E[4]);
end;

procedure TTestS7PlusTagPlan.AGroupThatIsAnArrayCoversItsElements;
var
  L:TS7PlusCandidateArray;
  E:TBooleanDynArray;
begin
  L := ListSized(['DB5.Teste', 'DB5.Teste[0]', 'DB5.Teste[1]', 'DB5.x'], [6, 6, 6, 7], [0, -1, -1, 12], [0, -1, -1, 12]);
  E := S7PlusEffectiveChecked(L, Ticked(Length(L), [0]), []);
  AssertTrue('the elements are under the array', E[1] and E[2]);
  AssertFalse('the other variable is not', E[3]);
end;

procedure TTestS7PlusTagPlan.ANegativeIndexIsNotTheSameNameAsThePositiveOne;
begin
  AssertEquals('pArr_NegLow_m2', S7PlusSuggestedName('Arr_NegLow[-2]', 'p'));
  AssertEquals('pArr_NegLow_2', S7PlusSuggestedName('Arr_NegLow[2]', 'p'));
  AssertEquals('a dash that is not an index stays a separator', 'pA_B', S7PlusSuggestedName('A-B', 'p'));
  AssertEquals('second dimension', 'pArr_1_m3', S7PlusSuggestedName('Arr[1,-3]', 'p'));
end;

initialization
  RegisterTest(TTestS7PlusTagPlan);

end.
