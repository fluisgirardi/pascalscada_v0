{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes dos hashes de bloco (7949 e 7951) e da leitura de atributos de um EXPLORE.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Nao precisa de CLP. Os valores esperados do fluxo foram montados a mao a partir das regras; que eles
  batam com um CLP ainda nao foi conferido (veja tests/live).
}
{$ELSE}
{:
  @abstract(Tests of the block hashes (7949 and 7951) and of the attribute reading of an EXPLORE.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Needs no PLC. The expected stream values were put together by hand from the rules; whether they match a
  PLC has not been checked yet (see tests/live).
}
{$ENDIF}
unit ut.s7plusblockhash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fpcunit, testregistry, S7PlusTypes, S7PlusAlarms, S7PlusBlockHash;

type

  { TTestS7PlusBlockHash }

  TTestS7PlusBlockHash = class(TTestCase)
  published
    procedure Sha1OfABC;
    procedure ABlockWithNoAlarmsHasTheKnownAlarmHash;
    procedure AlarmsAndTextsAreHashedByIdAndByLanguage;
    procedure TheClassListComesFirstWhateverItsPlace;
    procedure TheStreamOfABlockFollowsTheFixedOrder;
    procedure ADictionaryGoesInKeyOrderAndTheHiddenBodyKeysAreLeftOut;
    procedure AUDTHasOnlyTheDataInterfaceAttributes;
    procedure TheInputIsBuiltFromTheAttributesRead;
    procedure TheAttributesOfAnObjectAreReadFromAReply;
    procedure AnObjectThatIsNotInTheReplyIsAnError;
    procedure EveryAttributeOfAnObjectCanBeListed;
  end;

implementation

function B(const V:array of Byte):TBytes;
var
  i:Integer;
begin
  SetLength(Result, Length(V));
  for i:=0 to High(V) do Result[i] := V[i];
end;

function Item(AKey:Cardinal; const V:array of Byte):TS7PlusDictItem;
begin
  Result.Key := AKey;
  Result.Value := B(V);
end;

function TextBytes(const S:String):TBytes;
begin
  SetLength(Result, Length(S));
  if Length(S)>0 then Move(S[1], Result[0], Length(S));
end;

function Dict(const Items:array of TS7PlusDictItem):TS7PlusDict;
var
  i:Integer;
begin
  SetLength(Result, Length(Items));
  for i:=0 to High(Items) do Result[i] := Items[i];
end;

procedure TTestS7PlusBlockHash.Sha1OfABC;
begin
  AssertEquals('A9993E364706816ABA3E25717850C26C9CD0D89D', S7PlusHex(S7PlusSha1(TextBytes('abc'))));
  AssertEquals('empty', 'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709', S7PlusHex(S7PlusSha1(nil)));
end;

procedure TTestS7PlusBlockHash.ABlockWithNoAlarmsHasTheKnownAlarmHash;
begin
  //the SHA-1 of '<?xml version="1.0" encoding="utf-8"?><AlarmClassList />' + CRLF
  AssertEquals('6610F3B5B089454CA1C582423CA2E79A4930A770',
               S7PlusHex(S7PlusAlarmIfrHash(nil, nil)));
end;

procedure TTestS7PlusBlockHash.AlarmsAndTextsAreHashedByIdAndByLanguage;
var
  Desc, Texts:TS7PlusDict;
  T:UTF8String;
begin
  //given out of order: the alarms go by id and the texts by LCID
  Desc := Dict([Item(7, [Ord('B')]), Item(0, [Ord('C')]), Item(3, [Ord('A')])]);
  Texts := Dict([Item(1046, [Ord('Y')]), Item(1033, [Ord('X')])]);
  T := S7PlusAlarmIfrText(Desc, Texts);
  AssertEquals('class list, alarm 3, alarm 7, LCID 1033, LCID 1046, each with CRLF',
               'C'#13#10'A'#13#10'B'#13#10'X'#13#10'Y'#13#10, T);
  AssertEquals(S7PlusHex(S7PlusSha1(TextBytes(T))), S7PlusHex(S7PlusAlarmIfrHash(Desc, Texts)));
end;

procedure TTestS7PlusBlockHash.TheClassListComesFirstWhateverItsPlace;
var
  Desc:TS7PlusDict;
begin
  Desc := Dict([Item(9, [Ord('Z')]), Item(0, [Ord('K')])]);
  AssertEquals('K'#13#10'Z'#13#10, S7PlusAlarmIfrText(Desc, nil));
  //alarms but no class list: the empty list is used
  Desc := Dict([Item(9, [Ord('Z')])]);
  AssertEquals('<?xml version="1.0" encoding="utf-8"?><AlarmClassList />'#13#10'Z'#13#10, S7PlusAlarmIfrText(Desc, nil));
end;

function SampleInput(IsBlock:Boolean):TS7PlusBlockHashInput;
begin
  Finalize(Result);
  FillChar(Result, SizeOf(Result), 0);
  Result.IsBlock := IsBlock;
  Result.Name := 'A';
  Result.Comment := Dict([Item(1033, [$42])]);
  Result.Designators := B([1]);
  Result.IdentES := B([2]);
  Result.CompilerSwitches := B([3]);
  Result.InterfaceDescription := B([4]);
  Result.LineComments := Dict([Item(2, [5]), Item(1, [6])]);
  Result.Title := Dict([Item(1, [$54])]);
  Result.BodyDescription := Dict([Item($80000001, [$22]), Item(1, [$11])]);
  Result.BlockLanguage := 4;
  Result.BlockNumber := 1;
  Result.KnowhowMode := 0;
  Result.KnowhowPassword := B([7]);
  Result.OptimizeInfo := B([8]);
  Result.TypeInfo := B([9]);
end;

procedure TTestS7PlusBlockHash.TheStreamOfABlockFollowsTheFixedOrder;
begin
  //name, comment, designators, IdentES, switches, interface, line comments (key order), title, body (public key),
  //language, number, know-how mode (UInt16 little-endian), password, optimize info, type info
  AssertEquals('41 42 01 02 03 04 06 05 54 11 0400 0100 0000 07 08 09 without the spaces',
               '414201020304060554'+'11'+'040001000000'+'070809', S7PlusHex(S7PlusBlockIfrStream(SampleInput(true))));
  AssertEquals('the hash is the SHA-1 of the stream', S7PlusHex(S7PlusSha1(S7PlusBlockIfrStream(SampleInput(true)))),
               S7PlusHex(S7PlusBlockIfrHash(SampleInput(true))));
end;

procedure TTestS7PlusBlockHash.ADictionaryGoesInKeyOrderAndTheHiddenBodyKeysAreLeftOut;
var
  I:TS7PlusBlockHashInput;
begin
  I := SampleInput(true);
  I.LineComments := Dict([Item(30, [3]), Item(10, [1]), Item(20, [2])]);
  AssertEquals('by key, not by position', '41420102030401020354'+'11'+'040001000000'+'070809',
               S7PlusHex(S7PlusBlockIfrStream(I)));
  I.BodyDescription := Dict([Item($80000005, [$AA])]);
  AssertEquals('a body with only hidden keys adds nothing', '41420102030401020354'+'040001000000'+'070809',
               S7PlusHex(S7PlusBlockIfrStream(I)));
end;

procedure TTestS7PlusBlockHash.AUDTHasOnlyTheDataInterfaceAttributes;
begin
  //no body, language, number, protection, optimize info and type info: up to the cross references
  AssertEquals('414201020304060554', S7PlusHex(S7PlusBlockIfrStream(SampleInput(false))));
end;

procedure TTestS7PlusBlockHash.TheInputIsBuiltFromTheAttributesRead;
var
  V:TS7PlusAttrValues;
  I:TS7PlusBlockHashInput;
begin
  SetLength(V, Length(S7PlusBlockHashAttrIds));
  V[1].Found := true; V[1].Kind := akList;
  SetLength(V[1].Items, 1);
  V[1].Items[0].Key := 1033; V[1].Items[0].Kind := akText; V[1].Items[0].Bytes := B([$42]);
  V[3].Found := true; V[3].Kind := akBlob; V[3].Bytes := B([2]);
  V[10].Found := true; V[10].Kind := akNumber; V[10].Num := 4;
  V[11].Found := true; V[11].Kind := akNumber; V[11].Num := 1;
  V[12].Found := true; V[12].Kind := akStruct;
  SetLength(V[12].Items, 2);
  V[12].Items[0].Key := 3448; V[12].Items[0].Kind := akNumber; V[12].Items[0].Num := 0;
  V[12].Items[1].Key := 3449; V[12].Items[1].Kind := akList; V[12].Items[1].Bytes := B([7]);
  I := S7PlusBlockHashInputFromAttrs('A', V, true);
  AssertEquals('A', I.Name);
  AssertEquals(1, Length(I.Comment));
  AssertEquals(1033, I.Comment[0].Key);
  AssertEquals('02', S7PlusHex(I.IdentES));
  AssertEquals(4, I.BlockLanguage);
  AssertEquals(1, I.BlockNumber);
  AssertEquals('07', S7PlusHex(I.KnowhowPassword));
  AssertEquals('an attribute the PLC lacks is empty', 0, Length(I.TypeInfo));
end;

//a reply as the PLC sends it: ReturnValue 0, ExploreId, an IntegrityId, then the object
function Reply(RelationId:Cardinal; const Body:array of Byte):TBytes;
var
  n, i:Integer;
begin
  SetLength(Result, 0);
  n := 0;
  SetLength(Result, 1+4+1+1+4+3+Length(Body)+1);
  Result[n] := 0; inc(n);                                //ReturnValue
  Result[n] := 0; Result[n+1] := 0; Result[n+2] := 0; Result[n+3] := 3; inc(n, 4);   //ExploreId
  Result[n] := 1; inc(n);                                //IntegrityId
  Result[n] := S7PlusElement_StartOfObject; inc(n);
  Result[n] := RelationId shr 24; Result[n+1] := (RelationId shr 16) and $FF;
  Result[n+2] := (RelationId shr 8) and $FF; Result[n+3] := RelationId and $FF; inc(n, 4);
  Result[n] := 1; Result[n+1] := 0; Result[n+2] := 0; inc(n, 3);   //ClassId, ClassFlags, AttributeId
  for i:=0 to High(Body) do begin Result[n] := Body[i]; inc(n); end;
  Result[n] := S7PlusElement_TerminatingObject;
end;

procedure TTestS7PlusBlockHash.TheAttributesOfAnObjectAreReadFromAReply;
var
  R:TBytes;
  V:TS7PlusAttrValues;
  Ids:array[0..3] of Cardinal;
begin
  //attribute 2523 = UInt16 4; attribute 4288 = a sparse array of texts {1033: 'AB'}; attribute 2449 = a blob
  R := Reply($8A0E0004, [
    S7PlusElement_Attribute, $93, $5B, $00, S7PlusType_UINT, $00, $04,
    S7PlusElement_Attribute, $A1, $40, $40, S7PlusType_WSTRING, $88, $09, $02, $41, $42, $00]);
  Ids[0] := 2523; Ids[1] := 4288; Ids[2] := 7949; Ids[3] := 2449;
  AssertTrue(S7PlusParseObjectAttrs(R, $8A0E0004, Ids, V));
  AssertEquals(4, Length(V));
  AssertTrue('the UInt16', V[0].Found and (V[0].Kind=akNumber));
  AssertEquals(4, V[0].Num);
  AssertTrue('the sparse array of texts', V[1].Found and (V[1].Kind=akList));
  AssertEquals(1, Length(V[1].Items));
  AssertEquals(1033, V[1].Items[0].Key);
  AssertEquals('4142', S7PlusHex(V[1].Items[0].Bytes));
  AssertFalse('an attribute that is not there', V[2].Found);
end;

procedure TTestS7PlusBlockHash.AnObjectThatIsNotInTheReplyIsAnError;
var
  R:TBytes;
  V:TS7PlusAttrValues;
  Ids:array[0..0] of Cardinal;
begin
  R := Reply($8A0E0004, []);
  Ids[0] := 2523;
  AssertFalse(S7PlusParseObjectAttrs(R, $8A0E0009, Ids, V));
  AssertFalse('an error reply', S7PlusParseObjectAttrs(B([$81, $00]), $8A0E0004, Ids, V));
end;

procedure TTestS7PlusBlockHash.EveryAttributeOfAnObjectCanBeListed;
var
  R:TBytes;
  V:TS7PlusAttrValues;
  Ids:TCardinalDynArray;
begin
  R := Reply($8A0E0004, [
    S7PlusElement_Attribute, $93, $5B, $00, S7PlusType_UINT, $00, $04,
    S7PlusElement_Attribute, $A1, $40, $40, S7PlusType_WSTRING, $88, $09, $02, $41, $42, $00]);
  AssertTrue(S7PlusParseObjectAllAttrs(R, $8A0E0004, Ids, V));
  AssertEquals(2, Length(Ids));
  AssertEquals(2523, Ids[0]);
  AssertEquals(4288, Ids[1]);
  AssertEquals(4, V[0].Num);
  AssertEquals('4142', S7PlusHex(V[1].Items[0].Bytes));
  AssertFalse('another object', S7PlusParseObjectAllAttrs(R, $8A0E0009, Ids, V));
end;

initialization
  RegisterTest(TTestS7PlusBlockHash);

end.
