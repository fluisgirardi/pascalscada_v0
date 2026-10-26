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
  Classes, SysUtils, fpcunit, testregistry, Tag, S7PlusTypeInfo, S7PlusTagPlan;

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

initialization
  RegisterTest(TTestS7PlusTagPlan);

end.
