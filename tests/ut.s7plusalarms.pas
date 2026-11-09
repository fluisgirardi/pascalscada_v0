{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes dos alarmes ativos do S7CommPlus: decodificação da resposta do EXPLORE do subsistema de alarmes.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Não precisa de CLP: a resposta é montada byte a byte, no formato que as bibliotecas de referência descrevem.
}
{$ELSE}
{:
  @abstract(Tests of the S7CommPlus active alarms: decoding the reply of the EXPLORE of the alarm subsystem.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Needs no PLC: the reply is built byte by byte, in the format the reference libraries describe.
}
{$ENDIF}
unit ut.s7plusalarms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fpcunit, testregistry, S7PlusTypes, S7PlusVLQ, S7PlusCodec, S7PlusAlarms;

type

  { TTestS7PlusAlarms }

  TTestS7PlusAlarms = class(TTestCase)
  private
    function Cat(const A, B:TBytes):TBytes;
    function Attr(AId:Cardinal; const AValue:TBytes):TBytes;
    function WStr(const S:String):TBytes;
    function Blob(ARoot:Cardinal; const AData:TBytes):TBytes;
    function TypedBlob(ARoot:Cardinal; const AData:TBytes):TBytes;
    function Text(const S:String):TBytes;
    function Dai(AComing:Boolean; ASeq:Cardinal; const AName:String):TBytes;
    function Reply(const Objects:TBytes):TBytes;
    //Notification frame body (from the opcode on): header, empty data item list, alarm part
    function Notif(ASubId, ATail:Cardinal; ATick:Byte; AWithTimestamp:Boolean; const Objects:TBytes):TBytes;
    function Contains(const Data, Pattern:TBytes):Boolean;
  published
    procedure ListOfAttributesIsTheOneOfTheNotifications;
    procedure NoObjectsIsNoActiveAlarm;
    procedure AnErrorReturnValueFails;
    procedure ATruncatedReplyFails;
    procedure AnAlarmIsDecoded;
    procedure TheEventTimesAreConverted;
    procedure AGoingAlarmIsGoing;
    procedure TheHmiInfoIsDecoded;
    procedure TheTextsQuoteTheAssociatedValues;
    procedure OnlyTheLanguagesAskedForAreKept;
    procedure SeveralAlarmsComeInOrder;
    procedure ObjectsThatAreNotAlarmsAreIgnored;
    procedure TheIntegrityIdMayStartWithTheObjectTag;
    procedure InterpolationFormats;
    procedure InterpolationKeepsWhatItCannotResolve;
    procedure TheTimestampOfZeroIsNoTime;
    procedure TheShortDescription;
    procedure ANotificationCarriesItsAlarms;
    procedure ANotificationWithATimestampHeaderIsRead;
    procedure TheSubscriptionIdComesFromTheAlarmPartWhenTheHeaderHasNone;
    procedure ANotificationWithoutAlarmPartIsNotAnAlarmOne;
    procedure ANotificationWithDataItemsIsNotAnAlarmOne;
    procedure AFrameThatIsNotANotificationFails;
    procedure ATruncatedNotificationFails;
    procedure TheLanguageFilterAppliesToNotifications;
    procedure TheSubscriptionObjectHasTheRelationToTheAlarmSubsystem;
    procedure TheSubscriptionAsksForTextsAndDomainsOnlyWhenTold;
  end;

implementation

function Bx(const A:array of Byte):TBytes;
begin
  SetLength(Result, Length(A));
  if Length(A)>0 then Move(A[0], Result[0], Length(A));
end;

function StrBytes(const S:String):TBytes;
begin
  SetLength(Result, Length(S));
  if Length(S)>0 then Move(S[1], Result[0], Length(S));
end;

const
  CLASS_DAI = 2681;
  //2020-01-01 00:00:00 UTC in ns
  NS_2020 = QWord(1577836800)*QWord(1000000000);

function TTestS7PlusAlarms.Cat(const A, B:TBytes):TBytes;
begin
  SetLength(Result, Length(A)+Length(B));
  if Length(A)>0 then Move(A[0], Result[0], Length(A));
  if Length(B)>0 then Move(B[0], Result[Length(A)], Length(B));
end;

function TTestS7PlusAlarms.Attr(AId:Cardinal; const AValue:TBytes):TBytes;
begin
  Result := Cat(Bx([S7PlusElement_Attribute]), EncodeUInt32VLQ(AId));
  Result := Cat(Result, AValue);
end;

function TTestS7PlusAlarms.WStr(const S:String):TBytes;
begin
  Result := Cat(Bx([0, S7PlusType_WSTRING]), EncodeUInt32VLQ(Length(S)));
  Result := Cat(Result, StrBytes(S));
end;

function TTestS7PlusAlarms.Blob(ARoot:Cardinal; const AData:TBytes):TBytes;
begin
  Result := Cat(EncodeUInt32VLQ(ARoot), EncodeUInt32VLQ(Length(AData)));
  Result := Cat(Result, AData);
end;

//a blob whose root is a type id: 8 bytes of type timestamp + type byte 2 come before the size
function TTestS7PlusAlarms.TypedBlob(ARoot:Cardinal; const AData:TBytes):TBytes;
begin
  Result := Cat(EncodeUInt32VLQ(ARoot), Bx([0,0,0,0,0,0,0,0, 2]));
  Result := Cat(Result, EncodeUInt32VLQ(Length(AData)));
  Result := Cat(Result, AData);
end;

function TTestS7PlusAlarms.Text(const S:String):TBytes;
begin
  Result := StrBytes(S);
end;

//One DAI object: ID 4711, domain 3, type 1, a Coming/Going struct, HmiInfo, texts in de-DE and pt-BR.
function TTestS7PlusAlarms.Dai(AComing:Boolean; ASeq:Cardinal; const AName:String):TBytes;
var
  Ev, Hmi, Assoc, Texts:TBytes;
  EventId:Cardinal;
begin
  //SD_1 = DINT 42 (index 1), index 0 = type info
  Assoc := Cat(Bx([$20, S7PlusType_BLOB]), EncodeUInt32VLQ(2));
  Assoc := Cat(Assoc, Blob(0, Bx([0,0,0,0])));
  Assoc := Cat(Assoc, TypedBlob($02000007, Bx([0,0,0,42])));

  Ev := EncodeUInt32(0);                                                                        //struct id
  Ev := Cat(Ev, EncodeUInt32VLQ(3474));  Ev := Cat(Ev, Bx([0, S7PlusType_USINT, 1]));     //AllStatesInfo
  Ev := Cat(Ev, EncodeUInt32VLQ(3475));  Ev := Cat(Ev, Bx([0, S7PlusType_TIMESTAMP]));     //Timestamp
  Ev := Cat(Ev, EncodeUInt64(NS_2020 + QWord(5)*1000000000));
  Ev := Cat(Ev, EncodeUInt32VLQ(3476));  Ev := Cat(Ev, Assoc);                                  //associated values
  Ev := Cat(Ev, EncodeUInt32VLQ(3646));  Ev := Cat(Ev, Bx([0, S7PlusType_TIMESTAMP]));     //ack timestamp
  Ev := Cat(Ev, EncodeUInt64(NS_2020 + QWord(9)*1000000000));
  Ev := Cat(Ev, Bx([0]));                                                                   //end of members

  //HmiInfo: syntax 258, version 1, client id 77, priority 5, 3 reserved, class 12, producer 2, group 3, flags 4
  Hmi := Bx([1,2, 0,1, 0,0,0,77, 5, 0,0,0, 0,12, 2, 3, 4]);

  Texts := Cat(Bx([$40, S7PlusType_BLOB]), EncodeUInt32VLQ((1031 shl 16) or 2));
  Texts := Cat(Texts, Blob(0, Text('Wert @1%d@ zu hoch')));
  Texts := Cat(Texts, EncodeUInt32VLQ((1046 shl 16) or 2));
  Texts := Cat(Texts, Blob(0, Text('Valor @1%04d@ alto @1%3d@|@2%d@')));
  Texts := Cat(Texts, EncodeUInt32VLQ((1046 shl 16) or 1));
  Texts := Cat(Texts, Blob(0, Text('Info pt')));
  Texts := Cat(Texts, EncodeUInt32VLQ((1046 shl 16) or 3));
  Texts := Cat(Texts, Blob(0, Text('Extra 1')));
  Texts := Cat(Texts, Bx([0]));                                                             //end of keys

  Result := Bx([S7PlusElement_StartOfObject, $12,$34,$56,$78]);
  Result := Cat(Result, EncodeUInt32VLQ(CLASS_DAI));
  Result := Cat(Result, Bx([0, 0]));
  Result := Cat(Result, Attr(S7PlusIds_ObjectVariableTypeName, WStr(AName)));
  Result := Cat(Result, Attr(2670, Cat(Bx([0, S7PlusType_LWORD]), EncodeUInt64(4711))));
  Result := Cat(Result, Attr(2671, Bx([0, S7PlusType_USINT, 3])));
  Result := Cat(Result, Attr(2672, Bx([0, S7PlusType_UINT, 0, 3])));
  Result := Cat(Result, Attr(4079, Cat(Bx([0, S7PlusType_DINT]), EncodeInt32VLQ(-2))));
  Result := Cat(Result, Attr(7917, Cat(Bx([0, S7PlusType_UDINT]), EncodeUInt32VLQ(ASeq))));
  Result := Cat(Result, Attr(7813, Cat(Bx([0, S7PlusType_BLOB]), Blob(0, Hmi))));
  if AComing then EventId := 2673 else EventId := 2677;
  Result := Cat(Result, Attr(EventId, Cat(Bx([0, S7PlusType_STRUCT]), Ev)));
  Result := Cat(Result, Attr(2715, Texts));
  Result := Cat(Result, Bx([S7PlusElement_TerminatingObject]));
end;

//ReturnValue 0, ExploreId 8, IntegrityId, then the objects
function TTestS7PlusAlarms.Reply(const Objects:TBytes):TBytes;
begin
  Result := Cat(Bx([0, 0,0,0,8, 5]), Objects);
end;

procedure TTestS7PlusAlarms.ListOfAttributesIsTheOneOfTheNotifications;
var
  A:TCardinalDynArray;
begin
  A := S7PlusAlarmExploreAttributes;
  AssertEquals(10, Length(A));
  AssertEquals(2670, A[0]);
  AssertEquals(2715, A[9]);
end;

procedure TTestS7PlusAlarms.NoObjectsIsNoActiveAlarm;
var
  Al:TS7PlusAlarmArray;
begin
  AssertTrue(S7PlusParseAlarmExplore(Reply(nil), [], Al));
  AssertEquals(0, Length(Al));
end;

procedure TTestS7PlusAlarms.AnErrorReturnValueFails;
var
  Al:TS7PlusAlarmArray;
  R:TBytes;
begin
  R := Cat(Bx([$81, $00]), Cat(Bx([0,0,0,8,5]), Dai(true, 1, 'A')));
  AssertFalse(S7PlusParseAlarmExplore(R, [], Al));
  AssertEquals(0, Length(Al));
  AssertFalse(S7PlusParseAlarmExplore(nil, [], Al));
end;

procedure TTestS7PlusAlarms.ATruncatedReplyFails;
var
  Al:TS7PlusAlarmArray;
  R:TBytes;
begin
  R := Reply(Dai(true, 1, 'A'));
  SetLength(R, Length(R)-20);
  AssertFalse(S7PlusParseAlarmExplore(R, [], Al));
end;

procedure TTestS7PlusAlarms.AnAlarmIsDecoded;
var
  Al:TS7PlusAlarmArray;
begin
  AssertTrue(S7PlusParseAlarmExplore(Reply(Dai(true, 99, 'Alarm_1')), [], Al));
  AssertEquals(1, Length(Al));
  AssertEquals('Alarm_1', String(Al[0].Name));
  AssertEquals(4711, Int64(Al[0].CpuAlarmId));
  AssertEquals(3, Al[0].AllStatesInfo);
  AssertEquals(3, Al[0].Domain);
  AssertEquals(-2, Al[0].MessageType);
  AssertEquals(99, Al[0].SequenceCounter);
  AssertTrue('coming', Al[0].Event=aeComing);
  AssertEquals(2, Length(Al[0].AssociatedValues)); //the type info and SD_1
  AssertEquals('42', String(Al[0].AssociatedValues[1]));
  AssertEquals('', String(Al[0].AssociatedValues[0]));
end;

procedure TTestS7PlusAlarms.TheEventTimesAreConverted;
var
  Al:TS7PlusAlarmArray;
begin
  AssertTrue(S7PlusParseAlarmExplore(Reply(Dai(true, 1, 'A')), [], Al));
  AssertTrue(Al[0].TimestampNs=NS_2020 + QWord(5)*1000000000);
  AssertEquals('2020-01-01 00:00:05', FormatDateTime('yyyy-mm-dd hh:nn:ss', Al[0].Timestamp));
  AssertEquals('2020-01-01 00:00:09', FormatDateTime('yyyy-mm-dd hh:nn:ss', Al[0].AckTimestamp));
end;

procedure TTestS7PlusAlarms.AGoingAlarmIsGoing;
var
  Al:TS7PlusAlarmArray;
begin
  AssertTrue(S7PlusParseAlarmExplore(Reply(Dai(false, 1, 'A')), [], Al));
  AssertTrue('going', Al[0].Event=aeGoing);
  AssertEquals('2020-01-01 00:00:05', FormatDateTime('yyyy-mm-dd hh:nn:ss', Al[0].Timestamp));
end;

procedure TTestS7PlusAlarms.TheHmiInfoIsDecoded;
var
  Al:TS7PlusAlarmArray;
begin
  AssertTrue(S7PlusParseAlarmExplore(Reply(Dai(true, 1, 'A')), [], Al));
  AssertTrue(Al[0].HmiInfo.Present);
  AssertEquals(258, Al[0].HmiInfo.SyntaxId);
  AssertEquals(1, Al[0].HmiInfo.Version);
  AssertEquals(77, Al[0].HmiInfo.ClientAlarmId);
  AssertEquals(5, Al[0].HmiInfo.Priority);
  AssertEquals(12, Al[0].HmiInfo.AlarmClass);
  AssertEquals(2, Al[0].HmiInfo.Producer);
  AssertEquals(3, Al[0].HmiInfo.GroupId);
  AssertEquals(4, Al[0].HmiInfo.Flags);
end;

procedure TTestS7PlusAlarms.TheTextsQuoteTheAssociatedValues;
var
  Al:TS7PlusAlarmArray;
  T:TS7PlusAlarmText;
begin
  AssertTrue(S7PlusParseAlarmExplore(Reply(Dai(true, 1, 'A')), [], Al));
  AssertEquals(2, Length(Al[0].Texts));
  AssertTrue(S7PlusAlarmTextOf(Al[0], 1031, T));
  AssertEquals('Wert 42 zu hoch', String(T.AlarmText));
  AssertTrue(S7PlusAlarmTextOf(Al[0], 1046, T));
  //@1%04d@ -> 0042, @1%3d@ -> " 42", @2%d@ has no value: stays
  AssertEquals('Valor 0042 alto  42|@2%d@', String(T.AlarmText));
  AssertEquals('Info pt', String(T.InfoText));
  AssertEquals('Extra 1', String(T.AdditionalText[0]));
  AssertEquals('', String(T.AdditionalText[1]));
  //a language that is not there gives the first one
  AssertTrue(S7PlusAlarmTextOf(Al[0], 1033, T));
end;

procedure TTestS7PlusAlarms.OnlyTheLanguagesAskedForAreKept;
var
  Al:TS7PlusAlarmArray;
  T:TS7PlusAlarmText;
begin
  AssertTrue(S7PlusParseAlarmExplore(Reply(Dai(true, 1, 'A')), [1046], Al));
  AssertEquals(1, Length(Al[0].Texts));
  AssertEquals(1046, Al[0].Texts[0].LanguageId);
  AssertTrue(S7PlusParseAlarmExplore(Reply(Dai(true, 1, 'A')), [1033], Al));
  AssertEquals(0, Length(Al[0].Texts));
  AssertFalse(S7PlusAlarmTextOf(Al[0], 1033, T));
end;

procedure TTestS7PlusAlarms.SeveralAlarmsComeInOrder;
var
  Al:TS7PlusAlarmArray;
begin
  AssertTrue(S7PlusParseAlarmExplore(Reply(Cat(Dai(true, 10, 'First'), Dai(false, 20, 'Second'))), [], Al));
  AssertEquals(2, Length(Al));
  AssertEquals('First', String(Al[0].Name));
  AssertEquals(10, Al[0].SequenceCounter);
  AssertEquals('Second', String(Al[1].Name));
  AssertEquals(20, Al[1].SequenceCounter);
  AssertTrue(Al[1].Event=aeGoing);
end;

procedure TTestS7PlusAlarms.ObjectsThatAreNotAlarmsAreIgnored;
var
  Other:TBytes;
  Al:TS7PlusAlarmArray;
begin
  Other := Bx([S7PlusElement_StartOfObject, 0,0,0,1]);
  Other := Cat(Other, EncodeUInt32VLQ(1234));
  Other := Cat(Other, Bx([0, 0, S7PlusElement_TerminatingObject]));
  AssertTrue(S7PlusParseAlarmExplore(Reply(Cat(Other, Dai(true, 7, 'Mine'))), [], Al));
  AssertEquals(1, Length(Al));
  AssertEquals('Mine', String(Al[0].Name));
end;

procedure TTestS7PlusAlarms.TheIntegrityIdMayStartWithTheObjectTag;
var
  Al:TS7PlusAlarmArray;
  R:TBytes;
begin
  //an IntegrityId whose first VLQ byte is $A1 (two bytes: $A1 $05) looks like the start of an object
  R := Cat(Bx([0, 0,0,0,8, $A1, $05]), Dai(true, 3, 'X'));
  AssertTrue(S7PlusParseAlarmExplore(R, [], Al));
  AssertEquals(1, Length(Al));
  AssertEquals('X', String(Al[0].Name));
end;

procedure TTestS7PlusAlarms.InterpolationFormats;
begin
  AssertEquals('7', String(S7PlusInterpolateAssociatedValues('@1%d@', ['', '7'])));
  AssertEquals('007', String(S7PlusInterpolateAssociatedValues('@1%03d@', ['', '7'])));
  AssertEquals('  7', String(S7PlusInterpolateAssociatedValues('@1%3d@', ['', '7'])));
  AssertEquals('7  |', String(S7PlusInterpolateAssociatedValues('@1%-3d@|', ['', '7'])));
  AssertEquals('1.50', String(S7PlusInterpolateAssociatedValues('@1%.2f@', ['', '1.5'])));
  AssertEquals('ff', String(S7PlusInterpolateAssociatedValues('@1%x@', ['', '255'])));
  AssertEquals('FF', String(S7PlusInterpolateAssociatedValues('@1%X@', ['', '255'])));
  AssertEquals('Motor 3 ok', String(S7PlusInterpolateAssociatedValues('Motor @1%d@ @2%s@', ['', '3', 'ok'])));
  AssertEquals('Temp 21,5', 'Temp ' + StringReplace(String(S7PlusInterpolateAssociatedValues('@1%.1f@', ['', '21.5'])), '.', ',', []));
  AssertEquals('no token', String(S7PlusInterpolateAssociatedValues('no token', [])));
end;

procedure TTestS7PlusAlarms.InterpolationKeepsWhatItCannotResolve;
begin
  AssertEquals('@5%d@', String(S7PlusInterpolateAssociatedValues('@5%d@', ['', '1'])));
  AssertEquals('@1%q@', String(S7PlusInterpolateAssociatedValues('@1%q@', ['', '1'])));
  AssertEquals('@1%d@', String(S7PlusInterpolateAssociatedValues('@1%d@', ['', 'abc'])));
  AssertEquals('a@b', String(S7PlusInterpolateAssociatedValues('a@b', ['', '1'])));
  AssertEquals('@1', String(S7PlusInterpolateAssociatedValues('@1', ['', '1'])));
end;

procedure TTestS7PlusAlarms.TheTimestampOfZeroIsNoTime;
begin
  AssertTrue(S7PlusTimestampToDateTime(0)=0);
  AssertEquals('1970-01-01 00:00:01', FormatDateTime('yyyy-mm-dd hh:nn:ss', S7PlusTimestampToDateTime(1000000000)));
end;

procedure TTestS7PlusAlarms.TheShortDescription;
var
  Al:TS7PlusAlarmArray;
begin
  AssertTrue(S7PlusParseAlarmExplore(Reply(Dai(true, 99, 'A')), [1046], Al));
  AssertEquals('#4711 [coming] seq=99 2020-01-01 00:00:05 Valor 0042 alto  42|@2%d@', S7PlusAlarmToString(Al[0], 1046));
end;

function TTestS7PlusAlarms.Notif(ASubId, ATail:Cardinal; ATick:Byte; AWithTimestamp:Boolean; const Objects:TBytes):TBytes;
begin
  Result := Cat(Bx([S7PlusOpcode_Notification]), EncodeUInt32(ASubId));
  Result := Cat(Result, Bx([0,4, 0,0, 0,0]));         //three unknown UInt16
  Result := Cat(Result, Bx([ATick]));
  Result := Cat(Result, EncodeUInt32VLQ(7));           //sequence number
  if AWithTimestamp then begin
    Result := Cat(Result, EncodeUInt64(NS_2020 div 1000));
    Result := Cat(Result, Bx([1]));                    //additional change counter
  end else
    Result := Cat(Result, Bx([1]));                    //change counter
  Result := Cat(Result, Bx([0]));                      //end of the data item list
  if Length(Objects)>0 then begin
    Result := Cat(Result, EncodeUInt32(ATail));
    Result := Cat(Result, Bx([0, 1, $81]));
    Result := Cat(Result, Objects);
  end;
  Result := Cat(Result, Bx([0]));
end;

function TTestS7PlusAlarms.Contains(const Data, Pattern:TBytes):Boolean;
var
  i:Integer;
begin
  Result := false;
  if Length(Pattern)=0 then exit;
  for i:=0 to Length(Data)-Length(Pattern) do
    if CompareByte(Data[i], Pattern[0], Length(Pattern))=0 then begin
      Result := true;
      exit;
    end;
end;

procedure TTestS7PlusAlarms.ANotificationCarriesItsAlarms;
var
  Al:TS7PlusAlarmArray;
  Sub:Cardinal;
  Tick:Byte;
begin
  AssertTrue(S7PlusParseAlarmNotification(Notif($11223344, $7FFFC055, 3, false, Cat(Dai(true, 10, 'A1'), Dai(false, 11, 'A2'))), [], Sub, Tick, Al));
  AssertEquals($11223344, Int64(Sub));
  AssertEquals(3, Tick);
  AssertEquals(2, Length(Al));
  AssertEquals('A1', String(Al[0].Name));
  AssertTrue(Al[0].Event=aeComing);
  AssertEquals(10, Al[0].SequenceCounter);
  AssertTrue(Al[1].Event=aeGoing);
  AssertEquals('42', String(Al[0].AssociatedValues[1]));
end;

procedure TTestS7PlusAlarms.ANotificationWithATimestampHeaderIsRead;
var
  Al:TS7PlusAlarmArray;
  Sub:Cardinal;
  Tick:Byte;
begin
  AssertTrue(S7PlusParseAlarmNotification(Notif(5, $7FFFC006, 1, true, Dai(true, 10, 'A1')), [], Sub, Tick, Al));
  AssertEquals(1, Length(Al));
  AssertEquals('A1', String(Al[0].Name));
end;

procedure TTestS7PlusAlarms.TheSubscriptionIdComesFromTheAlarmPartWhenTheHeaderHasNone;
var
  Al:TS7PlusAlarmArray;
  Sub:Cardinal;
  Tick:Byte;
begin
  AssertTrue(S7PlusParseAlarmNotification(Notif(0, $0A0B0C0D, 1, false, Dai(true, 10, 'A1')), [], Sub, Tick, Al));
  AssertEquals($0A0B0C0D, Int64(Sub));
end;

procedure TTestS7PlusAlarms.ANotificationWithoutAlarmPartIsNotAnAlarmOne;
var
  Al:TS7PlusAlarmArray;
  Sub:Cardinal;
  Tick:Byte;
begin
  AssertFalse(S7PlusParseAlarmNotification(Notif(5, 0, 1, false, Bx([])), [], Sub, Tick, Al));
  AssertEquals(0, Length(Al));
end;

procedure TTestS7PlusAlarms.ANotificationWithDataItemsIsNotAnAlarmOne;
var
  Al:TS7PlusAlarmArray;
  Sub:Cardinal;
  Tick:Byte;
  F:TBytes;
begin
  //the data item list is not empty: a $9B item (ref id, then a value)
  F := Cat(Bx([S7PlusOpcode_Notification]), EncodeUInt32(5));
  F := Cat(F, Bx([0,4, 0,0, 0,0, 1]));
  F := Cat(F, EncodeUInt32VLQ(7));
  F := Cat(F, Bx([1, $9B]));
  F := Cat(F, EncodeUInt32VLQ(1));
  F := Cat(F, Bx([0, S7PlusType_USINT, 1, 0, 0]));
  AssertFalse(S7PlusParseAlarmNotification(F, [], Sub, Tick, Al));
end;

procedure TTestS7PlusAlarms.AFrameThatIsNotANotificationFails;
var
  Al:TS7PlusAlarmArray;
  Sub:Cardinal;
  Tick:Byte;
  F:TBytes;
begin
  F := Notif(5, $7FFFC006, 1, false, Dai(true, 10, 'A1'));
  F[0] := S7PlusOpcode_Response;
  AssertFalse(S7PlusParseAlarmNotification(F, [], Sub, Tick, Al));
  AssertFalse(S7PlusParseAlarmNotification(Bx([]), [], Sub, Tick, Al));
end;

procedure TTestS7PlusAlarms.ATruncatedNotificationFails;
var
  Al:TS7PlusAlarmArray;
  Sub:Cardinal;
  Tick:Byte;
  F, T:TBytes;
begin
  F := Notif(5, $7FFFC006, 1, false, Dai(true, 10, 'A1'));
  SetLength(T, Length(F)-30);
  Move(F[0], T[0], Length(T));
  AssertFalse(S7PlusParseAlarmNotification(T, [], Sub, Tick, Al));
  AssertEquals(0, Length(Al));
end;

procedure TTestS7PlusAlarms.TheLanguageFilterAppliesToNotifications;
var
  Al:TS7PlusAlarmArray;
  Sub:Cardinal;
  Tick:Byte;
begin
  AssertTrue(S7PlusParseAlarmNotification(Notif(5, $7FFFC006, 1, false, Dai(true, 10, 'A1')), [1031], Sub, Tick, Al));
  AssertEquals(1, Length(Al[0].Texts));
  AssertEquals(1031, Al[0].Texts[0].LanguageId);
end;

procedure TTestS7PlusAlarms.TheSubscriptionObjectHasTheRelationToTheAlarmSubsystem;
var
  B:TBytes;
begin
  B := S7PlusBuildAlarmSubscriptionObject($7FFFC001, 10, true, [1046], []);
  AssertEquals(S7PlusElement_StartOfObject, B[0]);
  AssertEquals($7F, B[1]);
  AssertEquals($C0, B[3]);
  //the end: the child and then the subscription close
  AssertEquals(S7PlusElement_TerminatingObject, B[High(B)]);
  AssertEquals(S7PlusElement_TerminatingObject, B[High(B)-1]);
  //the relation to the alarm subsystem (RID 8) closes the child, before the two terminators
  AssertTrue('relation', Contains(B, Cat(Cat(Bx([S7PlusElement_Relation]), EncodeUInt32VLQ(2660)), EncodeUInt32(8))));
  //the child object (class 2662, relation id $51010001) is inside it
  AssertTrue('child', Contains(B, Cat(Cat(Bx([S7PlusElement_StartOfObject]), EncodeUInt32($51010001)), EncodeUInt32VLQ(2662))));
  //alarm function class (USInt 2) and route mode (USInt 2)
  AssertTrue('function class', Contains(B, Cat(Cat(Bx([S7PlusElement_Attribute]), EncodeUInt32VLQ(S7PlusObjId_SubscriptionFunctionClassId)), Bx([0, S7PlusType_USINT, 2]))));
  AssertTrue('route mode', Contains(B, Cat(Cat(Bx([S7PlusElement_Attribute]), EncodeUInt32VLQ(S7PlusObjId_SubscriptionRouteMode)), Bx([0, S7PlusType_USINT, 2]))));
  //the credit limit, an INT of 10
  AssertTrue('credit', Contains(B, Cat(Cat(Bx([S7PlusElement_Attribute]), EncodeUInt32VLQ(S7PlusObjId_SubscriptionCreditLimit)), Bx([0, S7PlusType_INT, 0, 10]))));
end;

procedure TTestS7PlusAlarms.TheSubscriptionAsksForTextsAndDomainsOnlyWhenTold;
var
  Full, Plain:TBytes;
  FilterAttr, LangAttr, TextsAttr:TBytes;
begin
  FilterAttr := Cat(Bx([S7PlusElement_Attribute]), EncodeUInt32VLQ(7731));
  LangAttr := Cat(Bx([S7PlusElement_Attribute]), EncodeUInt32VLQ(8181));
  TextsAttr := Cat(Bx([S7PlusElement_Attribute]), EncodeUInt32VLQ(8173));
  Full := S7PlusBuildAlarmSubscriptionObject($7FFFC001, 10, true, [1046], []);
  Plain := S7PlusBuildAlarmSubscriptionObject($7FFFC001, 10, false, [], []);
  AssertTrue('filter', Contains(Full, Cat(FilterAttr, Bx([$20, S7PlusType_UINT, 1, $FF, $FF]))));
  AssertTrue('languages', Contains(Full, Cat(Cat(LangAttr, Bx([$20, S7PlusType_UDINT, 1])), EncodeUInt32VLQ(1046))));
  AssertTrue('send texts', Contains(Full, Cat(TextsAttr, Bx([0, S7PlusType_BOOL, 1]))));
  AssertFalse('no filter', Contains(Plain, FilterAttr));
  AssertFalse('no languages', Contains(Plain, LangAttr));
  AssertFalse('no send texts', Contains(Plain, TextsAttr));
  //explicit domains replace the all-domains value
  Full := S7PlusBuildAlarmSubscriptionObject($7FFFC001, 10, true, [], [1, 256]);
  AssertTrue('domains', Contains(Full, Cat(FilterAttr, Bx([$20, S7PlusType_UINT, 2, 0, 1, 1, 0]))));
end;

initialization
  RegisterTest(TTestS7PlusAlarms);

end.
