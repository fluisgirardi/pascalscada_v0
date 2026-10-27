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
  Classes, SysUtils, Types, fpcunit, testregistry, Tag, S7PlusTypeInfo, S7PlusTagPlan;

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
  AssertTrue(S7PlusClassifyVar(Var_('DB2.Tabela[1,2].x', 7), '').Skip=srArrayElement);
end;

procedure TTestS7PlusTagPlan.AnUnsupportedTypeIsSkippedForItsOwnReason;
begin
  AssertTrue('string', S7PlusClassifyVar(Var_('DB4.Nome', 19), '').Skip=srUnsupportedType);
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

initialization
  RegisterTest(TTestS7PlusTagPlan);

end.
