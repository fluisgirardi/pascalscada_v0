{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes das notificacoes de subscricao do S7CommPlus
            (TS7PlusConnection.ParseNotification / StripV3HashPrefix).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os frames aqui foram conferidos byte a byte contra o parser do python-snap7
  (s7commplus). O frame "TIA Portal" e' uma captura real (issue 710 do
  python-snap7), com o prefixo de hash do V3. Dois defeitos ja custaram
  depuracao ao vivo e estao cercados: o prefixo de hash do V3 nao era removido
  antes de ParseNotification, e o status de erro por item ($13/$03) era
  descartado, deixando "subscricao criada mas nenhum valor chega" sem pista.
}
{$ELSE}
{:
  @abstract(S7CommPlus subscription notification tests
            (TS7PlusConnection.ParseNotification / StripV3HashPrefix).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The frames here were checked byte by byte against python-snap7's
  (s7commplus) parser. The "TIA Portal" frame is a real capture (python-snap7
  issue 710), carrying the V3 hash prefix. Two defects that already cost live
  debugging are fenced: the V3 hash prefix was not stripped before
  ParseNotification, and the per item error status ($13/$03) was dropped,
  leaving "subscription created but no value arrives" without a clue.
}
{$ENDIF}
unit ut.s7plusconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Types, Tag, S7PlusConnection, S7PlusFamily, S7PlusTypes, S7PlusTypeInfo,
  testsupport.bytes, testsupport.fakeport;

type

  { TS7ConnectionProbe }

  //A connection that never handshakes: frames come from a fake port, as if the session
  //were already up (no TLS: LogicalRecv then reads the TPKT frames straight from the port).
  TS7ConnectionProbe = class(TS7PlusConnection)
  public
    function NextReply(out Frame:TBytes):Boolean;
    procedure SessionIsUp;
    procedure CountIntegrityIds;
    function SendBody(Version:Byte; const Body:TBytes):Boolean;
    function WritePayload(const Items:TS7PlusMultiWriteItemArray):TBytes;
    function WriteReply(const Response:TBytes; N:Integer; out Results:TS7PlusMultiWriteResultArray):Boolean;
  end;

  { TTestS7PlusNotification }

  TTestS7PlusNotification = class(TTestCase)
  private
    FConnection:TS7PlusConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ValuesAndErrorAreReportedSeparately;
    procedure TheOldOverloadStillReturnsTheValues;
    procedure ErrorsStartEmptyWhenTheParseFails;
    procedure TimestampedNotificationParsesTheSame;
    procedure UnsupportedItemTagFailsTheParse;
    procedure NotificationWithTheV3HashPrefixIsNotParsedAsIs;
    procedure StrippingTheV3HashPrefixMakesItParse;
    procedure StripLeavesOtherVersionsAlone;
    procedure StripLeavesABodyThatAlreadyStartsAtTheOpcode;
    procedure RealTiaPortalNotificationParses;
  end;

  { TTestS7PlusNotificationQueue }

  TTestS7PlusNotificationQueue = class(TTestCase)
  private
    FPort:TFakeCommPort;
    FConn:TS7ConnectionProbe;
    procedure Feed(Version:Byte; const BodyHex:String);
    procedure Poll(out Got:Boolean; out Credit:Byte; out Values:TS7PlusNotificationValueArray; out Errors:TS7PlusNotificationErrorArray);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ANotificationBeforeTheReplyIsQueuedNotReturned;
    procedure QueuedNotificationsComeOutInArrivalOrder;
    procedure AV3NotificationWithTheHashPrefixIsQueued;
    procedure PollingReadsANotificationStraightFromTheWire;
    procedure PollingAFrameThatIsNotANotificationDiscardsIt;
    procedure PollingWithNothingToReadReportsNoNotification;
    procedure PollingAReadThatFailsIsReportedAsAFailure;
    procedure DisconnectingForgetsTheQueue;
  end;

  { TTestS7PlusFragments }

  TTestS7PlusFragments = class(TTestCase)
  private
    FPort:TFakeCommPort;
    FConn:TS7ConnectionProbe;
    function Pattern(N:Integer):TBytes;
    function ReadItems(N:Integer):TS7PlusMultiReadItemArray;
    procedure QueueReadReply(FirstItem, Count:Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ABodyThatFitsGoesOutAsOnePacketWithItsTrailer;
    procedure ABiggerBodyIsSplitAndOnlyTheLastFragmentHasTheTrailer;
    procedure TheFragmentsCarryTheWholeBodyInOrder;
    procedure ABodyOfExactlyOneFragmentIsNotSplit;
    procedure ARequestOfMoreItemsThanOnePacketCanHoldIsSentInFragments;
    procedure AFragmentedReplyIsHandedBackAsOneFrame;
    procedure AShortFrameWithNoTrailerIsNotWaitedOn;
    procedure ABigFrameThatHasItsTrailerIsLeftAlone;
    procedure AFragmentedNotificationIsReassembledByPolling;
    procedure ReadingMoreItemsThanTheLimitIsSplitIntoRequests;
    procedure TheLimitIsHonouredWhateverItsValue;
    procedure AFailingRequestKeepsWhatTheEarlierOnesRead;
    procedure TheDefaultLimitIsTheMeasuredOne;
    procedure TheLimitCannotBeLessThanOne;
    procedure ABatchOfBigValuesIsSplitByTheSizeOfTheReply;
    procedure AnItemBiggerThanTheReplyGoesOutAlone;
    procedure ItemsOfUnknownSizeAreCountedAsThirtyTwoBytes;
    procedure SmallItemsStillFillARequestUpToTheItemLimit;
    procedure TheRoomLeftInTheLastRequest;
    procedure FillersAreTheOldestThatFit;
    procedure FillersNeverCutATagAndRespectTheLimits;
  end;

  { TTestS7PlusDriverFlags }

  TTestS7PlusDriverFlags = class(TTestCase)
  published
    procedure ReadSomethingAlwaysIsOnByDefault;
    procedure ReadOnlyBlocksTheDirectWrites;
  end;

  { TTestS7PlusMultiWrite }

  TTestS7PlusMultiWrite = class(TTestCase)
  private
    FPort:TFakeCommPort;
    FConn:TS7ConnectionProbe;
    function Item(DB, Start:Integer; const DataHex:String; SoftType:Byte):TS7PlusMultiWriteItem;
    procedure QueueReply(const PayloadHex:String);
    function RequestsSent:Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure OneItemIsTheSingleWritePayloadByteForByte;
    procedure MixedItemsMatchTheUpstreamEncoderByteForByte;
    procedure WithIntegrityCountingTheOneItemPayloadIsUpstreamsByteForByte;
    procedure WithIntegrityCountingTheMixedPayloadIsUpstreamsByteForByte;
    procedure EveryWriteOfASessionCarriesItsOwnCountingIntegrityId;
    procedure WithoutIntegrityCountingTheWriteKeepsItsStandInId;
    procedure AReplyWithNoErrorListTakesEveryItem;
    procedure ErrorsInTheReplyMarkOnlyTheirItems;
    procedure ASetReturnValueWithNoItemListFailsTheWholeBatch;
    procedure ASetReturnValueWithAnItemListIsAPartialSuccess;
    procedure TheRealPLCReplyForOneBadItemIsAPartialSuccess;
    procedure AnErrorTextObjectAfterTheReturnValueIsNotAnItemList;
    procedure ErrorsForItemsOutOfRangeAreIgnored;
    procedure ATruncatedReplyDoesNotRaise;
    procedure AnEmptyBatchSendsNothing;
    procedure WritingGoesOutInOneRequestAndReportsPerItem;
    procedure MoreItemsThanTheLimitGoOutInSeveralRequests;
    procedure AFailedLaterRequestKeepsWhatEarlierOnesDid;
    procedure TheWriteLimitIsTheConfiguredOne;
  end;

implementation

{ TS7ConnectionProbe }

function TS7ConnectionProbe.NextReply(out Frame:TBytes):Boolean;
begin
  Result := RecvApplicationFrame(Frame);
end;

procedure TS7ConnectionProbe.SessionIsUp;
begin
  AssumeConnected;
end;

function TS7ConnectionProbe.SendBody(Version:Byte; const Body:TBytes):Boolean;
begin
  Result := SendFragmented(Version, Body);
end;

procedure TS7ConnectionProbe.CountIntegrityIds;
begin
  AssumeIntegrityTracking;
end;

function TS7ConnectionProbe.WritePayload(const Items:TS7PlusMultiWriteItemArray):TBytes;
begin
  Result := BuildMultiSymbolicWritePayload(Items);
end;

function TS7ConnectionProbe.WriteReply(const Response:TBytes; N:Integer; out Results:TS7PlusMultiWriteResultArray):Boolean;
begin
  Result := ParseMultiWriteResponse(Response, N, Results);
end;

const
  //opcode $33, id da subscricao, 3x UInt16, credito 3, seq 9, change counter 1;
  //item $92 ref 7 = BLOB 1234 (BlobRootId 0), item $9B ref 8 = USInt $2A,
  //erro $13 ref 9, terminador, 1 byte de sobra
  V2_NOTIFICATION =
    '33 70400025 0004 0000 0000 03 09 01 ' +
    '92 00000007 00 14 00 02 1234 ' +
    '9B 08 00 02 2A ' +
    '13 00000009 ' +
    '00 AA';
  //igual, mas com change counter 0 => 8 bytes de timestamp e depois o contador
  V2_TIMESTAMPED =
    '33 70400025 0004 0000 0000 03 09 ' +
    '00060A24181E4000 01 ' +
    '92 00000007 00 14 00 02 1234 ' +
    '9B 08 00 02 2A ' +
    '13 00000009 ' +
    '00 AA';
  V3_HASH_PREFIX = '20 0000000000000000000000000000000000000000000000000000000000000000 ';
  //captura real do TIA Portal (python-snap7 issue 710), corpo apos o cabecalho de 4 bytes
  TIA_NOTIFICATION =
    '20db9b8947109b14bc56e8bd25032cda2dd3a9488cbd1807017a69f90af8982371' +
    '337040002504000000000000050192000000070014001801000006278ab08c18c456364dfc0d' +
    'd0800000000f000000920000000800020092000000090001000000000000';

procedure TTestS7PlusNotification.SetUp;
begin
  inherited SetUp;
  FConnection := TS7PlusConnection.Create(nil, 0);
end;

procedure TTestS7PlusNotification.TearDown;
begin
  FConnection.Free;
  inherited TearDown;
end;

procedure TTestS7PlusNotification.ValuesAndErrorAreReportedSeparately;
var
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  AssertTrue('parse', FConnection.ParseNotification(BytesOf(V2_NOTIFICATION), Credit, Values, Errors));
  AssertEquals('credit tick', 3, Credit);
  AssertEquals('value count', 2, Length(Values));
  AssertEquals('value 1 ref', 7, Values[0].RefId);
  AssertBytesEqual('value 1 data', BytesOf('12 34'), Values[0].Data);
  AssertEquals('value 2 ref', 8, Values[1].RefId);
  AssertBytesEqual('value 2 data', BytesOf('2A'), Values[1].Data);
  AssertEquals('error count', 1, Length(Errors));
  AssertEquals('error ref', 9, Errors[0].RefId);
  AssertEquals('error status', $13, Errors[0].Status);
end;

procedure TTestS7PlusNotification.TheOldOverloadStillReturnsTheValues;
var
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
begin
  AssertTrue('parse', FConnection.ParseNotification(BytesOf(V2_NOTIFICATION), Credit, Values));
  AssertEquals('value count', 2, Length(Values));
end;

procedure TTestS7PlusNotification.ErrorsStartEmptyWhenTheParseFails;
var
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  SetLength(Errors, 3);
  AssertFalse('empty frame', FConnection.ParseNotification(BytesOf(''), Credit, Values, Errors));
  AssertEquals('errors', 0, Length(Errors));
end;

procedure TTestS7PlusNotification.TimestampedNotificationParsesTheSame;
var
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  AssertTrue('parse', FConnection.ParseNotification(BytesOf(V2_TIMESTAMPED), Credit, Values, Errors));
  AssertEquals('credit tick', 3, Credit);
  AssertEquals('value count', 2, Length(Values));
  AssertEquals('error count', 1, Length(Errors));
  AssertEquals('error status', $13, Errors[0].Status);
end;

procedure TTestS7PlusNotification.UnsupportedItemTagFailsTheParse;
var
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  AssertFalse('tag $55', FConnection.ParseNotification(
    BytesOf('33 70400025 0004 0000 0000 03 09 01 55 00000001'), Credit, Values, Errors));
end;

procedure TTestS7PlusNotification.NotificationWithTheV3HashPrefixIsNotParsedAsIs;
var
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
begin
  //e' exatamente o que WaitForFrame entregava antes do conserto
  AssertFalse('prefixed', FConnection.ParseNotification(BytesOf(V3_HASH_PREFIX + V2_NOTIFICATION), Credit, Values));
end;

procedure TTestS7PlusNotification.StrippingTheV3HashPrefixMakesItParse;
var
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
  Body:TBytes;
begin
  Body := TS7PlusConnection.StripV3HashPrefix(S7PlusVersion_V3, BytesOf(V3_HASH_PREFIX + V2_NOTIFICATION));
  AssertBytesEqual('stripped', BytesOf(V2_NOTIFICATION), Body);
  AssertTrue('parse', FConnection.ParseNotification(Body, Credit, Values, Errors));
  AssertEquals('value count', 2, Length(Values));
  AssertEquals('error count', 1, Length(Errors));
end;

procedure TTestS7PlusNotification.StripLeavesOtherVersionsAlone;
begin
  AssertBytesEqual('v2', BytesOf(V3_HASH_PREFIX + V2_NOTIFICATION),
    TS7PlusConnection.StripV3HashPrefix(S7PlusVersion_V2, BytesOf(V3_HASH_PREFIX + V2_NOTIFICATION)));
end;

procedure TTestS7PlusNotification.StripLeavesABodyThatAlreadyStartsAtTheOpcode;
begin
  AssertBytesEqual('v3 without prefix', BytesOf(V2_NOTIFICATION),
    TS7PlusConnection.StripV3HashPrefix(S7PlusVersion_V3, BytesOf(V2_NOTIFICATION)));
end;

procedure TTestS7PlusNotification.RealTiaPortalNotificationParses;
var
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  AssertTrue('parse', FConnection.ParseNotification(
    TS7PlusConnection.StripV3HashPrefix(S7PlusVersion_V3, BytesOf(TIA_NOTIFICATION)), Credit, Values, Errors));
  AssertEquals('value count', 3, Length(Values));
  AssertEquals('error count', 0, Length(Errors));
  AssertBytesEqual('ref 7', BytesOf('01000006278ab08c18c456364dfc0dd0800000000f000000'), Values[0].Data);
  AssertBytesEqual('ref 8', BytesOf('00'), Values[1].Data);
  AssertBytesEqual('ref 9', BytesOf('00'), Values[2].Data);
end;

//One TPKT/COTP frame carrying S7CommPlus data of the given version: [header][body] and, when
//WithTrailer, the trailer - the last (or only) fragment of a message has it, earlier ones do not.
function TpktPart(Version:Byte; const Body:TBytes; WithTrailer:Boolean):TBytes;
var
  Msg:TBytes;
  Len, i, Extra:Integer;
begin
  Len := Length(Body);
  Extra := 0;
  if WithTrailer then Extra := 4;
  SetLength(Msg, 4+Len+Extra);
  Msg[0] := $72; Msg[1] := Version; Msg[2] := Len shr 8; Msg[3] := Len and $FF;
  for i:=0 to Len-1 do
    Msg[4+i] := Body[i];
  if WithTrailer then begin
    Msg[4+Len] := $72; Msg[5+Len] := Version; Msg[6+Len] := 0; Msg[7+Len] := 0;
  end;
  SetLength(Result, 7+Length(Msg));
  Result[0] := 3; Result[1] := 0; Result[2] := Length(Result) shr 8; Result[3] := Length(Result) and $FF;
  Result[4] := 2; Result[5] := $F0; Result[6] := $80;
  for i:=0 to High(Msg) do
    Result[7+i] := Msg[i];
end;

function TpktOf(Version:Byte; const Body:TBytes):TBytes;
begin
  Result := TpktPart(Version, Body, true);
end;

const
  //corpo de uma resposta qualquer (opcode $32): so o primeiro byte importa para separar
  //resposta de notificacao
  REPLY_BODY = '32 0000 0000 0000 0005 34 00';
  //a mesma notificacao, mas com o valor da ref 7 diferente, para distinguir a ordem
  V2_NOTIFICATION_SECOND =
    '33 70400025 0004 0000 0000 03 0A 01 ' +
    '92 00000007 00 14 00 02 5678 ' +
    '00';

{ TTestS7PlusNotificationQueue }

procedure TTestS7PlusNotificationQueue.SetUp;
begin
  inherited SetUp;
  FPort := TFakeCommPort.Create(nil);
  FPort.Active := true;
  FConn := TS7ConnectionProbe.Create(FPort, 1);
  FConn.UseTLS := false;
  FConn.SessionIsUp;
end;

procedure TTestS7PlusNotificationQueue.TearDown;
begin
  FConn.Free;
  FPort.Free;
  inherited TearDown;
end;

//Queues one TPKT/COTP frame carrying one S7CommPlus message of the given version.
procedure TTestS7PlusNotificationQueue.Feed(Version:Byte; const BodyHex:String);
var
  Body, Msg, Pkt:TBytes;
  Len, i:Integer;
begin
  Body := BytesOf(BodyHex);
  Len := Length(Body);
  SetLength(Msg, 4+Len+4);
  Msg[0] := $72; Msg[1] := Version; Msg[2] := Len shr 8; Msg[3] := Len and $FF;
  for i:=0 to Len-1 do
    Msg[4+i] := Body[i];
  Msg[4+Len] := $72; Msg[5+Len] := Version; Msg[6+Len] := 0; Msg[7+Len] := 0;
  SetLength(Pkt, 7+Length(Msg));
  Pkt[0] := 3; Pkt[1] := 0; Pkt[2] := Length(Pkt) shr 8; Pkt[3] := Length(Pkt) and $FF;
  Pkt[4] := 2; Pkt[5] := $F0; Pkt[6] := $80;
  for i:=0 to High(Msg) do
    Pkt[7+i] := Msg[i];
  FPort.QueueResponse(Pkt);
end;

procedure TTestS7PlusNotificationQueue.Poll(out Got:Boolean; out Credit:Byte;
  out Values:TS7PlusNotificationValueArray; out Errors:TS7PlusNotificationErrorArray);
begin
  AssertTrue('poll itself succeeds', FConn.PollNotification(0, Got, Credit, Values, Errors));
end;

procedure TTestS7PlusNotificationQueue.ANotificationBeforeTheReplyIsQueuedNotReturned;
var
  Frame:TBytes;
  Got:Boolean;
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  Feed(S7PlusVersion_V2, V2_NOTIFICATION);
  Feed(S7PlusVersion_V2, REPLY_BODY);
  AssertTrue('reply received', FConn.NextReply(Frame));
  //the frame handed back is the reply, not the notification that came first
  AssertEquals('reply opcode', $32, Frame[4]);
  AssertEquals('one notification waiting', 1, FConn.PendingNotificationCount);
  Poll(Got, Credit, Values, Errors);
  AssertTrue('notification delivered', Got);
  AssertEquals('credit tick', 3, Credit);
  AssertEquals('values', 2, Length(Values));
  AssertEquals('errors', 1, Length(Errors));
  AssertEquals('queue drained', 0, FConn.PendingNotificationCount);
end;

procedure TTestS7PlusNotificationQueue.QueuedNotificationsComeOutInArrivalOrder;
var
  Frame:TBytes;
  Got:Boolean;
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  Feed(S7PlusVersion_V2, V2_NOTIFICATION);
  Feed(S7PlusVersion_V2, V2_NOTIFICATION_SECOND);
  Feed(S7PlusVersion_V2, REPLY_BODY);
  AssertTrue('reply received', FConn.NextReply(Frame));
  AssertEquals('two waiting', 2, FConn.PendingNotificationCount);
  Poll(Got, Credit, Values, Errors);
  AssertTrue('first', Got);
  AssertEquals('first has the credit tick of the first', 3, Credit);
  Poll(Got, Credit, Values, Errors);
  AssertTrue('second', Got);
  AssertEquals('second values', 1, Length(Values));
  AssertBytesEqual('second data', BytesOf('56 78'), Values[0].Data);
end;

procedure TTestS7PlusNotificationQueue.AV3NotificationWithTheHashPrefixIsQueued;
var
  Frame:TBytes;
  Got:Boolean;
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  Feed(S7PlusVersion_V3, V3_HASH_PREFIX + V2_NOTIFICATION);
  Feed(S7PlusVersion_V3, V3_HASH_PREFIX + REPLY_BODY);
  AssertTrue('reply received', FConn.NextReply(Frame));
  AssertEquals('one waiting', 1, FConn.PendingNotificationCount);
  Poll(Got, Credit, Values, Errors);
  AssertTrue('delivered', Got);
  AssertEquals('values', 2, Length(Values));
end;

procedure TTestS7PlusNotificationQueue.PollingReadsANotificationStraightFromTheWire;
var
  Got:Boolean;
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  Feed(S7PlusVersion_V2, V2_NOTIFICATION);
  Poll(Got, Credit, Values, Errors);
  AssertTrue('delivered', Got);
  AssertEquals('values', 2, Length(Values));
  AssertEquals('subscription id', $70400025, FConn.LastNotificationSubscriptionId);
  AssertEquals('sequence', 9, FConn.LastNotificationSequence);
end;

procedure TTestS7PlusNotificationQueue.PollingAFrameThatIsNotANotificationDiscardsIt;
var
  Got:Boolean;
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  Feed(S7PlusVersion_V2, REPLY_BODY);
  Poll(Got, Credit, Values, Errors);
  AssertFalse('not a notification', Got);
  AssertEquals('nothing queued', 0, FConn.PendingNotificationCount);
end;

procedure TTestS7PlusNotificationQueue.PollingWithNothingToReadReportsNoNotification;
var
  Got:Boolean;
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  //nothing on the wire is not an error: the poll succeeds and says there is nothing
  Poll(Got, Credit, Values, Errors);
  AssertFalse('nothing delivered', Got);
end;

procedure TTestS7PlusNotificationQueue.PollingAReadThatFailsIsReportedAsAFailure;
var
  Got:Boolean;
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
begin
  FPort.QueueTimeout;
  AssertFalse('read failure is reported', FConn.PollNotification(0, Got, Credit, Values, Errors));
  AssertFalse('nothing delivered', Got);
end;

procedure TTestS7PlusNotificationQueue.DisconnectingForgetsTheQueue;
var
  Frame:TBytes;
begin
  Feed(S7PlusVersion_V2, V2_NOTIFICATION);
  Feed(S7PlusVersion_V2, REPLY_BODY);
  AssertTrue('reply received', FConn.NextReply(Frame));
  AssertEquals('one waiting', 1, FConn.PendingNotificationCount);
  FConn.Disconnect;
  AssertEquals('forgotten', 0, FConn.PendingNotificationCount);
end;

const
  //Payloads built by python-snap7's _build_write_payload (the last version before the
  //S7CommPlus module moved to its own library), for items (db, start, data, type):
  //  one:   (4, 0, 4242 as DINT)
  //  mixed: (4,0,-5 DINT) (4,4,300 INT) (2,8,1.5 REAL) (4,12,TRUE BOOL) (1,16,70000 UDINT) (4,20,127 USINT)
  //Upstream's tail is  <object qualifier><4 zero bytes> and the real IntegrityId goes between the
  //two. A session that does not count IntegrityIds still gets a fixed VLQ(1) there (OUR_TAIL).
  UPSTREAM_ONE =
    '0000000001060088d0b880040393760104010008a112';
  UPSTREAM_MIXED =
    '0000000006240088d0b8800403937601040088d0b8800403937605020088d0b8800203937609040088d0b880040393760d01' +
    '0088d0b8800103937611040088d0b88004039376150101' +
    '00087b020007012c03000e3fc000000400010105000484a2700600027f';
  //(the closing $00 of the value list is the first byte of the tails below)
  //object qualifier + trailing padding, as upstream ends the payload
  UPSTREAM_TAIL = '00000004e88969001200000000896a001300896b0004000000000000';
  //ours: the same qualifier, then VLQ(1), then the padding
  OUR_TAIL = '00000004e88969001200000000896a001300896b00040000' + '01' + '00000000';

{ TTestS7PlusFragments }

procedure TTestS7PlusFragments.SetUp;
begin
  inherited SetUp;
  FPort := TFakeCommPort.Create(nil);
  FPort.Active := true;
  FConn := TS7ConnectionProbe.Create(FPort, 1);
  FConn.UseTLS := false;
  FConn.SessionIsUp;
end;

procedure TTestS7PlusFragments.TearDown;
begin
  FConn.Free;
  FPort.Free;
  inherited TearDown;
end;

function TTestS7PlusFragments.Pattern(N:Integer):TBytes;
var
  i:Integer;
begin
  SetLength(Result, N);
  for i:=0 to N-1 do
    Result[i] := (i*7+3) and $FF;
end;

function TTestS7PlusFragments.ReadItems(N:Integer):TS7PlusMultiReadItemArray;
var
  i:Integer;
begin
  SetLength(Result, N);
  for i:=0 to N-1 do begin
    Result[i].AccessArea := $8A0E0004;
    SetLength(Result[i].Lids, 2);
    Result[i].Lids[0] := 1+i;
    Result[i].Lids[1] := 4;
    Result[i].ExpectedSize := 1;       //the replies the tests queue carry one byte per item
  end;
end;

//The reply of a read of Count items whose first is numbered 1: ReturnValue 0, then
//[item number][USInt value] for each, closing zeros of the values and of the error list.
procedure TTestS7PlusFragments.QueueReadReply(FirstItem, Count:Integer);
var
  Hex:String;
  i:Integer;
begin
  Hex := '32 0000 0000 0000 0001 34 00 ';
  for i:=1 to Count do
    Hex := Hex + IntToHex(i, 2) + ' 00 02 ' + IntToHex((FirstItem+i-1) and $FF, 2) + ' ';
  Hex := Hex + '00 00';
  FPort.QueueResponse(TpktOf(S7PlusVersion_V2, BytesOf(Hex)));
end;

procedure TTestS7PlusFragments.ABodyThatFitsGoesOutAsOnePacketWithItsTrailer;
var
  Body, Sent:TBytes;
begin
  Body := Pattern(500);
  AssertTrue('sent', FConn.SendBody(S7PlusVersion_V2, Body));
  AssertEquals('one packet', 1, FPort.WriteCount);
  Sent := FPort.WrittenFrame(0);
  //TPKT+COTP (7), header (4), data, trailer (4)
  AssertEquals('size', 7+4+500+4, Length(Sent));
  AssertEquals('header length', 500, Sent[9]*256+Sent[10]);
  AssertEquals('trailer id', $72, Sent[Length(Sent)-4]);
  AssertEquals('trailer length', 0, Sent[Length(Sent)-1]+Sent[Length(Sent)-2]);
end;

procedure TTestS7PlusFragments.ABodyOfExactlyOneFragmentIsNotSplit;
begin
  AssertTrue('sent', FConn.SendBody(S7PlusVersion_V2, Pattern(987)));
  AssertEquals('one packet', 1, FPort.WriteCount);
  AssertTrue('sent bigger by one', FConn.SendBody(S7PlusVersion_V2, Pattern(988)));
  AssertEquals('that one needs two', 3, FPort.WriteCount);
end;

procedure TTestS7PlusFragments.ABiggerBodyIsSplitAndOnlyTheLastFragmentHasTheTrailer;
var
  F0, F1, F2:TBytes;
begin
  AssertTrue('sent', FConn.SendBody(S7PlusVersion_V2, Pattern(2500)));
  AssertEquals('three packets (987+987+526)', 3, FPort.WriteCount);
  F0 := FPort.WrittenFrame(0);
  F1 := FPort.WrittenFrame(1);
  F2 := FPort.WrittenFrame(2);
  AssertEquals('first fragment',  987, F0[9]*256+F0[10]);
  AssertEquals('second fragment', 987, F1[9]*256+F1[10]);
  AssertEquals('third fragment',  526, F2[9]*256+F2[10]);
  //only the last one is followed by the trailer
  AssertEquals('first has no trailer',  7+4+987, Length(F0));
  AssertEquals('second has no trailer', 7+4+987, Length(F1));
  AssertEquals('last has it',           7+4+526+4, Length(F2));
  AssertEquals('version of each', S7PlusVersion_V2, F1[8]);
end;

procedure TTestS7PlusFragments.TheFragmentsCarryTheWholeBodyInOrder;
var
  Body, Joined, F:TBytes;
  i, k:Integer;
begin
  Body := Pattern(2500);
  AssertTrue('sent', FConn.SendBody(S7PlusVersion_V2, Body));
  SetLength(Joined, 0);
  for k:=0 to FPort.WriteCount-1 do begin
    F := FPort.WrittenFrame(k);
    Joined := BytesCat(Joined, Copy(F, 11, F[9]*256+F[10]));
  end;
  AssertEquals('same length', Length(Body), Length(Joined));
  for i:=0 to High(Body) do
    if Body[i]<>Joined[i] then
      Fail('byte '+IntToStr(i)+' differs');
end;

procedure TTestS7PlusFragments.ARequestOfMoreItemsThanOnePacketCanHoldIsSentInFragments;
var
  Items:TS7PlusMultiReadItemArray;
  R:TS7PlusMultiReadResultArray;
begin
  //100 items are ~1 KB of payload: more than a 987 byte fragment holds
  Items := ReadItems(100);
  QueueReadReply(0, 100);
  AssertTrue('read', FConn.ReadMultipleSymbolic(Items, R));
  AssertTrue('sent in more than one packet', FPort.WriteCount>=2);
  AssertEquals('all items answered', 100, Length(R));
  AssertTrue('first', R[0].Ok);
  AssertTrue('last', R[99].Ok);
end;

procedure TTestS7PlusFragments.AFragmentedReplyIsHandedBackAsOneFrame;
var
  First, Second, Frame:TBytes;
  Hex:String;
  i:Integer;
begin
  //a reply bigger than a PDU: 950 bytes in a first fragment with no trailer, then the rest
  //with the trailer
  Hex := '32 0000 0000 0000 0001 34 ';
  for i:=1 to 940 do
    Hex := Hex + '5A ';
  First := BytesOf(Hex);
  Second := BytesOf('01 02 03 04 05 06 07 08 09 0A');
  FPort.QueueResponse(TpktPart(S7PlusVersion_V2, First, false));
  FPort.QueueResponse(TpktPart(S7PlusVersion_V2, Second, true));

  AssertTrue('reply received', FConn.NextReply(Frame));
  //one frame: header with the TOTAL length, all the data
  AssertEquals('total length', Length(First)+Length(Second), Frame[2]*256+Frame[3]);
  AssertEquals('begins with the first fragment', $32, Frame[4]);
  AssertEquals('ends with the second', $0A, Frame[Length(Frame)-1]);
  AssertEquals('nothing left to read', 0, FPort.PendingResponses);
end;

procedure TTestS7PlusFragments.AShortFrameWithNoTrailerIsNotWaitedOn;
var
  Frame:TBytes;
begin
  //only a BIG fragment can be the start of a longer message: a short one is a whole frame, and
  //waiting for more would just time out
  FPort.QueueResponse(TpktPart(S7PlusVersion_V2, BytesOf('32 0000 0000 0000 0001 34 00 00'), false));
  AssertTrue('reply received', FConn.NextReply(Frame));
  AssertEquals('as it came', 4+12, Length(Frame));
end;

procedure TTestS7PlusFragments.ABigFrameThatHasItsTrailerIsLeftAlone;
var
  Frame:TBytes;
  Hex:String;
  i:Integer;
begin
  Hex := '32 0000 0000 0000 0001 34 ';
  for i:=1 to 940 do
    Hex := Hex + '5A ';
  FPort.QueueResponse(TpktOf(S7PlusVersion_V2, BytesOf(Hex)));
  FPort.QueueResponse(TpktOf(S7PlusVersion_V2, BytesOf('32 0000 0000 0000 0002 34 00 00')));
  AssertTrue('first reply', FConn.NextReply(Frame));
  AssertEquals('only the first was taken', 1, FPort.PendingResponses);
end;

procedure TTestS7PlusFragments.AFragmentedNotificationIsReassembledByPolling;
var
  Hex:String;
  i:Integer;
  Got:Boolean;
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
  Whole:TBytes;
  Cut:Integer;
begin
  //a notification carrying 130 USInt values: ~1 KB, so the PLC sends it in two fragments
  Hex := '33 70400025 0004 0000 0000 03 09 01 ';
  for i:=1 to 130 do
    Hex := Hex + '92 ' + IntToHex(i, 8) + ' 00 02 ' + IntToHex(i and $FF, 2) + ' ';
  Hex := Hex + '00';
  Whole := BytesOf(Hex);
  Cut := 920;
  FPort.QueueResponse(TpktPart(S7PlusVersion_V2, Copy(Whole, 0, Cut), false));
  FPort.QueueResponse(TpktPart(S7PlusVersion_V2, Copy(Whole, Cut, Length(Whole)-Cut), true));

  AssertTrue('poll', FConn.PollNotification(0, Got, Credit, Values, Errors));
  AssertTrue('delivered', Got);
  AssertEquals('every value', 130, Length(Values));
  AssertEquals('first ref', 1, Values[0].RefId);
  AssertEquals('last ref', 130, Values[129].RefId);
end;

procedure TTestS7PlusFragments.ReadingMoreItemsThanTheLimitIsSplitIntoRequests;
var
  Items:TS7PlusMultiReadItemArray;
  R:TS7PlusMultiReadResultArray;
  i:Integer;
begin
  Items := ReadItems(250);
  QueueReadReply(0, 100);
  QueueReadReply(100, 100);
  QueueReadReply(200, 50);
  AssertTrue('read', FConn.ReadMultipleSymbolic(Items, R));
  AssertEquals('results for every item', 250, Length(R));
  for i:=0 to 249 do
    AssertTrue('item '+IntToStr(i), R[i].Ok);
  //each reply numbers its own items from 1: the value says which request it came from
  AssertEquals('first request',  0,   Trunc(R[0].Data[0]));
  AssertEquals('second request', 100, Trunc(R[100].Data[0]));
  AssertEquals('third request',  200 and $FF, Trunc(R[200].Data[0]));
  AssertEquals('last item', (200+49) and $FF, Trunc(R[249].Data[0]));
end;

procedure TTestS7PlusFragments.TheLimitIsHonouredWhateverItsValue;
var
  Items:TS7PlusMultiReadItemArray;
  R:TS7PlusMultiReadResultArray;
  F:TBytes;
begin
  FConn.MaxItemsPerRequest := 7;
  Items := ReadItems(15);
  QueueReadReply(0, 7);
  QueueReadReply(7, 7);
  QueueReadReply(14, 1);
  AssertTrue('read', FConn.ReadMultipleSymbolic(Items, R));
  AssertEquals('three requests (7+7+1)', 3, FPort.WriteCount);
  //the item count of each request: the byte after the 4 byte InObjectId of the payload, which
  //follows TPKT+COTP (7), the S7CommPlus header (4) and the 14 byte request header
  F := FPort.WrittenFrame(0);
  AssertEquals('first request item count', 7, F[7+4+14+4]);
  F := FPort.WrittenFrame(2);
  AssertEquals('last request item count', 1, F[7+4+14+4]);
end;

procedure TTestS7PlusFragments.AFailingRequestKeepsWhatTheEarlierOnesRead;
var
  Items:TS7PlusMultiReadItemArray;
  R:TS7PlusMultiReadResultArray;
  i:Integer;
begin
  Items := ReadItems(150);
  QueueReadReply(0, 100);
  FPort.QueueResponse(TpktOf(S7PlusVersion_V2, BytesOf('32 0000 0000 0000 0001 34 81 00'))); //refused
  AssertFalse('failed as a whole', FConn.ReadMultipleSymbolic(Items, R));
  AssertEquals('results for every item', 150, Length(R));
  for i:=0 to 99 do
    AssertTrue('read before the failure '+IntToStr(i), R[i].Ok);
  for i:=100 to 149 do
    AssertFalse('not read '+IntToStr(i), R[i].Ok);
end;

procedure TTestS7PlusFragments.TheDefaultLimitIsTheMeasuredOne;
begin
  AssertEquals('the limit measured on a PLCSim', 100, S7PlusDefaultMaxItemsPerRequest);
  AssertEquals('the connection starts with it', 100, FConn.MaxItemsPerRequest);
end;

procedure TTestS7PlusFragments.TheLimitCannotBeLessThanOne;
begin
  FConn.MaxItemsPerRequest := 0;
  AssertEquals('zero becomes one', 1, FConn.MaxItemsPerRequest);
  FConn.MaxItemsPerRequest := -5;
  AssertEquals('negative becomes one', 1, FConn.MaxItemsPerRequest);
end;

{ TTestS7PlusMultiWrite }

procedure TTestS7PlusMultiWrite.SetUp;
begin
  inherited SetUp;
  FPort := TFakeCommPort.Create(nil);
  FPort.Active := true;
  FConn := TS7ConnectionProbe.Create(FPort, 1);
  FConn.UseTLS := false;
  FConn.SessionIsUp;
end;

procedure TTestS7PlusMultiWrite.TearDown;
begin
  FConn.Free;
  FPort.Free;
  inherited TearDown;
end;

function TTestS7PlusMultiWrite.Item(DB, Start:Integer; const DataHex:String; SoftType:Byte):TS7PlusMultiWriteItem;
begin
  Result.AccessArea := $8A0E0000 + Cardinal(DB);
  SetLength(Result.Lids, 2);
  Result.Lids[0] := Start+1;
  Result.Lids[1] := Length(BytesOf(DataHex));
  Result.Data := BytesOf(DataHex);
  Result.SoftDataType := SoftType;
end;

//How many REQUESTS went out: a request bigger than a PDU is several packets, and only its last
//packet ends with the trailer [$72][version][0][0].
function TTestS7PlusMultiWrite.RequestsSent:Integer;
var
  k:Integer;
  F:TBytes;
begin
  Result := 0;
  for k:=0 to FPort.WriteCount-1 do begin
    F := FPort.WrittenFrame(k);
    if (Length(F)>=4) and (F[Length(F)-4]=$72) and (F[Length(F)-2]=0) and (F[Length(F)-1]=0) then
      Inc(Result);
  end;
end;

//A reply: the 10 byte response header the connection strips, then the payload.
procedure TTestS7PlusMultiWrite.QueueReply(const PayloadHex:String);
begin
  FPort.QueueResponse(TpktOf(S7PlusVersion_V2, BytesOf('32 0000 0000 0000 0001 34 ' + PayloadHex)));
end;

procedure TTestS7PlusMultiWrite.OneItemIsTheSingleWritePayloadByteForByte;
var
  Items:TS7PlusMultiWriteItemArray;
begin
  SetLength(Items, 1);
  Items[0] := Item(4, 0, '00 00 10 92', 7{DINT});
  AssertBytesEqual('one item', BytesOf(UPSTREAM_ONE + OUR_TAIL), FConn.WritePayload(Items));
end;

procedure TTestS7PlusMultiWrite.MixedItemsMatchTheUpstreamEncoderByteForByte;
var
  Items:TS7PlusMultiWriteItemArray;
begin
  SetLength(Items, 6);
  Items[0] := Item(4, 0,  'FF FF FF FB', 7{DINT});
  Items[1] := Item(4, 4,  '01 2C',       5{INT});
  Items[2] := Item(2, 8,  '3F C0 00 00', 8{REAL});
  Items[3] := Item(4, 12, '01',          1{BOOL});
  Items[4] := Item(1, 16, '00 01 11 70', 54{UDINT});
  Items[5] := Item(4, 20, '7F',          52{USINT});
  AssertBytesEqual('mixed', BytesOf(UPSTREAM_MIXED + OUR_TAIL), FConn.WritePayload(Items));
end;

procedure TTestS7PlusMultiWrite.WithIntegrityCountingTheOneItemPayloadIsUpstreamsByteForByte;
var
  Items:TS7PlusMultiWriteItemArray;
begin
  //On a V2 session SendRequest splices the real IntegrityId in, so the payload itself must NOT
  //carry one: with a fixed "1" of its own, the PLC took that for the IntegrityId, and every
  //write after the first one repeated it and got the session dropped (seen on a PLCSim).
  FConn.CountIntegrityIds;
  SetLength(Items, 1);
  Items[0] := Item(4, 0, '00 00 10 92', 7{DINT});
  AssertBytesEqual('one item', BytesOf(UPSTREAM_ONE + UPSTREAM_TAIL), FConn.WritePayload(Items));
end;

procedure TTestS7PlusMultiWrite.WithIntegrityCountingTheMixedPayloadIsUpstreamsByteForByte;
var
  Items:TS7PlusMultiWriteItemArray;
begin
  FConn.CountIntegrityIds;
  SetLength(Items, 6);
  Items[0] := Item(4, 0,  'FF FF FF FB', 7{DINT});
  Items[1] := Item(4, 4,  '01 2C',       5{INT});
  Items[2] := Item(2, 8,  '3F C0 00 00', 8{REAL});
  Items[3] := Item(4, 12, '01',          1{BOOL});
  Items[4] := Item(1, 16, '00 01 11 70', 54{UDINT});
  Items[5] := Item(4, 20, '7F',          52{USINT});
  AssertBytesEqual('mixed', BytesOf(UPSTREAM_MIXED + UPSTREAM_TAIL), FConn.WritePayload(Items));
end;

procedure TTestS7PlusMultiWrite.EveryWriteOfASessionCarriesItsOwnCountingIntegrityId;
var
  R:TS7PlusMultiWriteResultArray;
  Items:TS7PlusMultiWriteItemArray;
  F:TBytes;
begin
  FConn.CountIntegrityIds;
  SetLength(Items, 1);
  Items[0] := Item(4, 0, '00 00 10 92', 7);
  QueueReply('00 00');
  QueueReply('00 00');
  QueueReply('00 00');

  AssertTrue('write 1', FConn.WriteMultipleSymbolic(Items, R));
  AssertTrue('write 2', FConn.WriteMultipleSymbolic(Items, R));
  AssertTrue('write 3', FConn.WriteMultipleSymbolic(Items, R));

  //the frame ends with  <IntegrityId> 00 00 00 00 <trailer 72 02 00 00>; the byte before the
  //id is the end of the object qualifier ($00), not a stand-in $01
  AssertEquals('three requests', 3, FPort.WriteCount);
  F := FPort.WrittenFrame(0);
  AssertEquals('first id',  0, F[Length(F)-9]);
  AssertEquals('no stand-in before it (1)', 0, F[Length(F)-10]);
  F := FPort.WrittenFrame(1);
  AssertEquals('second id', 1, F[Length(F)-9]);
  AssertEquals('no stand-in before it (2)', 0, F[Length(F)-10]);
  F := FPort.WrittenFrame(2);
  AssertEquals('third id',  2, F[Length(F)-9]);
end;

procedure TTestS7PlusMultiWrite.WithoutIntegrityCountingTheWriteKeepsItsStandInId;
var
  R:TS7PlusMultiWriteResultArray;
  Items:TS7PlusMultiWriteItemArray;
  F:TBytes;
begin
  //sessions that do not count (V1) still need the id the payload carries itself
  SetLength(Items, 1);
  Items[0] := Item(4, 0, '00 00 10 92', 7);
  QueueReply('00 00');
  AssertTrue('write', FConn.WriteMultipleSymbolic(Items, R));
  F := FPort.WrittenFrame(0);
  AssertEquals('stand-in id', 1, F[Length(F)-9]);
end;

procedure TTestS7PlusMultiWrite.AReplyWithNoErrorListTakesEveryItem;
var
  R:TS7PlusMultiWriteResultArray;
  i:Integer;
begin
  AssertTrue('parsed', FConn.WriteReply(BytesOf('00 00'), 3, R));
  AssertEquals('count', 3, Length(R));
  for i:=0 to 2 do
    AssertTrue('item '+IntToStr(i), R[i].Ok);
end;

procedure TTestS7PlusMultiWrite.ErrorsInTheReplyMarkOnlyTheirItems;
var
  R:TS7PlusMultiWriteResultArray;
begin
  //ReturnValue 0, then item 2 failed with error 5, item 3 with error 300 ($82 $2C), end
  AssertTrue('parsed', FConn.WriteReply(BytesOf('00 02 05 03 82 2C 00'), 4, R));
  AssertTrue('item 1 ok', R[0].Ok);
  AssertFalse('item 2 failed', R[1].Ok);
  AssertEquals('item 2 error', 5, R[1].ErrorValue);
  AssertFalse('item 3 failed', R[2].Ok);
  AssertEquals('item 3 error', 300, R[2].ErrorValue);
  AssertTrue('item 4 ok', R[3].Ok);
end;

procedure TTestS7PlusMultiWrite.ASetReturnValueWithNoItemListFailsTheWholeBatch;
var
  R:TS7PlusMultiWriteResultArray;
  i:Integer;
begin
  AssertFalse('rejected as a whole', FConn.WriteReply(BytesOf('81 00'), 2, R));
  for i:=0 to 1 do
    AssertFalse('item '+IntToStr(i)+' not taken', R[i].Ok);
end;

procedure TTestS7PlusMultiWrite.ASetReturnValueWithAnItemListIsAPartialSuccess;
var
  R:TS7PlusMultiWriteResultArray;
begin
  //ReturnValue 128, then item 2 refused with error 5, end of list
  AssertTrue('partial success', FConn.WriteReply(BytesOf('81 00 02 05 00'), 3, R));
  AssertTrue('item 1 written', R[0].Ok);
  AssertFalse('item 2 refused', R[1].Ok);
  AssertEquals('item 2 error', 5, R[1].ErrorValue);
  AssertTrue('item 3 written', R[2].Ok);
end;

procedure TTestS7PlusMultiWrite.TheRealPLCReplyForOneBadItemIsAPartialSuccess;
var
  R:TS7PlusMultiWriteResultArray;
  i:Integer;
begin
  //Captured from a PLCSim: a batch of 6 whose 6th item had a LID that does not exist. The
  //ReturnValue is an error, the list names item 6, and items 1-5 had been written.
  AssertTrue('partial success', FConn.WriteReply(
    BytesOf('90 8B 90 80 80 83 C8 80 13 06 C0 D3 D1 80 80 A6 B9 FF 01 00 06 00 00 00 00'), 6, R));
  for i:=0 to 4 do
    AssertTrue('item '+IntToStr(i+1)+' written', R[i].Ok);
  AssertFalse('item 6 refused', R[5].Ok);
  AssertTrue('with an error value', R[5].ErrorValue<>0);
end;

procedure TTestS7PlusMultiWrite.AnErrorTextObjectAfterTheReturnValueIsNotAnItemList;
var
  R:TS7PlusMultiWriteResultArray;
  i:Integer;
begin
  //ReturnValue, then a StartOfObject ($A1...) like the error texts PLCs send: not a list, so
  //the whole request failed
  AssertFalse('whole failure', FConn.WriteReply(BytesOf('81 00 A1 00 00 00 66 90 2B 10'), 3, R));
  for i:=0 to 2 do
    AssertFalse('item '+IntToStr(i+1), R[i].Ok);
end;

procedure TTestS7PlusMultiWrite.ErrorsForItemsOutOfRangeAreIgnored;
var
  R:TS7PlusMultiWriteResultArray;
begin
  AssertTrue('parsed', FConn.WriteReply(BytesOf('00 09 01 00'), 2, R));
  AssertTrue('item 1', R[0].Ok);
  AssertTrue('item 2', R[1].Ok);
end;

procedure TTestS7PlusMultiWrite.ATruncatedReplyDoesNotRaise;
var
  R:TS7PlusMultiWriteResultArray;
begin
  //item number with no error value after it
  AssertTrue('parsed', FConn.WriteReply(BytesOf('00 02'), 3, R));
  AssertEquals('count', 3, Length(R));
  AssertFalse('empty reply is a failure', FConn.WriteReply(BytesOf(''), 3, R));
end;

procedure TTestS7PlusMultiWrite.AnEmptyBatchSendsNothing;
var
  R:TS7PlusMultiWriteResultArray;
  Items:TS7PlusMultiWriteItemArray;
begin
  SetLength(Items, 0);
  AssertTrue('ok', FConn.WriteMultipleSymbolic(Items, R));
  AssertEquals('no results', 0, Length(R));
  AssertEquals('nothing sent', 0, FPort.WriteCount);
end;

procedure TTestS7PlusMultiWrite.WritingGoesOutInOneRequestAndReportsPerItem;
var
  R:TS7PlusMultiWriteResultArray;
  Items:TS7PlusMultiWriteItemArray;
begin
  SetLength(Items, 3);
  Items[0] := Item(4, 0, '00 00 00 01', 7);
  Items[1] := Item(4, 4, '00 00 00 02', 7);
  Items[2] := Item(4, 8, '00 00 00 03', 7);
  QueueReply('00 02 07 00'); //item 2 refused
  AssertTrue('request ok', FConn.WriteMultipleSymbolic(Items, R));
  AssertEquals('one request', 1, FPort.WriteCount);
  AssertTrue('item 1', R[0].Ok);
  AssertFalse('item 2', R[1].Ok);
  AssertEquals('item 2 error', 7, R[1].ErrorValue);
  AssertTrue('item 3', R[2].Ok);
end;

procedure TTestS7PlusMultiWrite.MoreItemsThanTheLimitGoOutInSeveralRequests;
var
  R:TS7PlusMultiWriteResultArray;
  Items:TS7PlusMultiWriteItemArray;
  i:Integer;
begin
  FConn.MaxItemsPerRequest := 50; //the batches below are 50+50+20
  SetLength(Items, 120);
  for i:=0 to 119 do
    Items[i] := Item(4, i*4, '00 00 00 01', 7);
  QueueReply('00 00');
  QueueReply('00 03 09 51 04 00'); //second request: its item 3 refused (error 9); "$51" is out of range
  QueueReply('00 00');
  AssertTrue('ok', FConn.WriteMultipleSymbolic(Items, R));
  AssertEquals('three requests (50+50+20)', 3, FPort.WriteCount);
  AssertEquals('results', 120, Length(R));
  AssertTrue('item 1', R[0].Ok);
  AssertTrue('item 50', R[49].Ok);
  //the error list of the second request numbers items within THAT request: its item 3 is
  //item 53 of the batch; the out of range number 81 must not touch anything
  AssertFalse('item 3 of the second request', R[52].Ok);
  AssertEquals('its error', 9, R[52].ErrorValue);
  AssertTrue('neighbour before', R[51].Ok);
  AssertTrue('neighbour after', R[53].Ok);
  AssertTrue('item 100', R[99].Ok);
  AssertTrue('last', R[119].Ok);
end;

procedure TTestS7PlusMultiWrite.AFailedLaterRequestKeepsWhatEarlierOnesDid;
var
  R:TS7PlusMultiWriteResultArray;
  Items:TS7PlusMultiWriteItemArray;
  i:Integer;
begin
  FConn.MaxItemsPerRequest := 50;
  SetLength(Items, 60);
  for i:=0 to 59 do
    Items[i] := Item(4, i*4, '00 00 00 01', 7);
  QueueReply('00 00');
  QueueReply('81 00'); //the second request is rejected as a whole
  AssertFalse('failed as a whole', FConn.WriteMultipleSymbolic(Items, R));
  AssertEquals('results kept', 60, Length(R));
  AssertTrue('first request took its items', R[0].Ok);
  AssertTrue('first request took its items (last)', R[49].Ok);
  AssertFalse('second request did not', R[50].Ok);
end;

procedure TTestS7PlusMultiWrite.TheWriteLimitIsTheConfiguredOne;
var
  R:TS7PlusMultiWriteResultArray;
  Items:TS7PlusMultiWriteItemArray;
  i:Integer;
begin
  //default 100: 100 items are one request, 101 are two
  SetLength(Items, 100);
  for i:=0 to 99 do
    Items[i] := Item(4, i*4, '00 00 00 01', 7);
  QueueReply('00 00');
  AssertTrue('100 items', FConn.WriteMultipleSymbolic(Items, R));
  AssertEquals('one request', 1, RequestsSent);

  SetLength(Items, 101);
  Items[100] := Item(4, 400, '00 00 00 01', 7);
  QueueReply('00 00');
  QueueReply('00 00');
  AssertTrue('101 items', FConn.WriteMultipleSymbolic(Items, R));
  AssertEquals('two more requests', 3, RequestsSent);
end;

procedure TTestS7PlusFragments.ABatchOfBigValuesIsSplitByTheSizeOfTheReply;
var
  Items:TS7PlusMultiReadItemArray;
  R:TS7PlusMultiReadResultArray;
  F:TBytes;
  i:Integer;
begin
  //a reply that does not fit one PDU (~990 bytes on a PLCSim) is answered with a fatal system
  //event that ends the session: 10 items of 102 bytes must not go in one request
  Items := ReadItems(10);
  for i:=0 to 9 do
    Items[i].ExpectedSize := 102;
  QueueReadReply(0, 8);
  QueueReadReply(8, 2);
  AssertTrue('read', FConn.ReadMultipleSymbolic(Items, R));
  AssertEquals('two requests (8+2)', 2, FPort.WriteCount);
  F := FPort.WrittenFrame(0);
  AssertEquals('first request item count', 8, F[7+4+14+4]);
  F := FPort.WrittenFrame(1);
  AssertEquals('second request item count', 2, F[7+4+14+4]);
  AssertEquals('results for every item', 10, Length(R));
end;

procedure TTestS7PlusFragments.AnItemBiggerThanTheReplyGoesOutAlone;
var
  Items:TS7PlusMultiReadItemArray;
  R:TS7PlusMultiReadResultArray;
  F:TBytes;
  i:Integer;
begin
  Items := ReadItems(3);
  for i:=0 to 2 do
    Items[i].ExpectedSize := 2000;
  QueueReadReply(0, 1);
  QueueReadReply(1, 1);
  QueueReadReply(2, 1);
  AssertTrue('read', FConn.ReadMultipleSymbolic(Items, R));
  AssertEquals('one request each', 3, FPort.WriteCount);
  F := FPort.WrittenFrame(1);
  AssertEquals('one item', 1, F[7+4+14+4]);
end;

procedure TTestS7PlusFragments.ItemsOfUnknownSizeAreCountedAsThirtyTwoBytes;
var
  Items:TS7PlusMultiReadItemArray;
  R:TS7PlusMultiReadResultArray;
  F:TBytes;
  i:Integer;
begin
  //32 + 8 of overhead = 40 bytes each: 24 fit in the 971 left of a PDU
  Items := ReadItems(40);
  for i:=0 to 39 do
    Items[i].ExpectedSize := 0;
  QueueReadReply(0, 24);
  QueueReadReply(24, 16);
  AssertTrue('read', FConn.ReadMultipleSymbolic(Items, R));
  AssertEquals('two requests', 2, FPort.WriteCount);
  F := FPort.WrittenFrame(0);
  AssertEquals('24 in the first', 24, F[7+4+14+4]);
end;

procedure TTestS7PlusFragments.SmallItemsStillFillARequestUpToTheItemLimit;
var
  Items:TS7PlusMultiReadItemArray;
  R:TS7PlusMultiReadResultArray;
  F:TBytes;
  i:Integer;
begin
  Items := ReadItems(100);
  for i:=0 to 99 do
    Items[i].ExpectedSize := 4;       //12 bytes each: 100 of them are 1200 > 971, so 80 fit
  QueueReadReply(0, 80);
  QueueReadReply(80, 20);
  AssertTrue('read', FConn.ReadMultipleSymbolic(Items, R));
  F := FPort.WrittenFrame(0);
  AssertEquals('80 4-byte DInts fit one reply', 80, F[7+4+14+4]);
end;

procedure TTestS7PlusFragments.TheRoomLeftInTheLastRequest;
var
  Items:TS7PlusMultiReadItemArray;
  ItemsLeft, BytesLeft, i:Integer;
begin
  //nothing to read: a whole empty request is left (971 bytes of reply, 100 items)
  SetLength(Items, 0);
  FConn.ReadRoomLeft(Items, ItemsLeft, BytesLeft);
  AssertEquals('items', 100, ItemsLeft);
  AssertEquals('bytes', 971, BytesLeft);

  //10 items of 4 bytes take 12 each
  Items := ReadItems(10);
  for i:=0 to 9 do Items[i].ExpectedSize := 4;
  FConn.ReadRoomLeft(Items, ItemsLeft, BytesLeft);
  AssertEquals('items', 90, ItemsLeft);
  AssertEquals('bytes', 971-120, BytesLeft);

  //the room is the one of the LAST request: 100 items of 4 bytes are 80 + 20
  Items := ReadItems(100);
  for i:=0 to 99 do Items[i].ExpectedSize := 4;
  FConn.ReadRoomLeft(Items, ItemsLeft, BytesLeft);
  AssertEquals('items of the second request', 80, ItemsLeft);
  AssertEquals('bytes of the second request', 971-240, BytesLeft);

  //an item of unknown size counts 32+8
  Items := ReadItems(1);
  Items[0].ExpectedSize := 0;
  FConn.ReadRoomLeft(Items, ItemsLeft, BytesLeft);
  AssertEquals('unknown', 971-40, BytesLeft);

  //an item bigger than the reply goes alone: nothing is left
  Items := ReadItems(1);
  Items[0].ExpectedSize := 2000;
  FConn.ReadRoomLeft(Items, ItemsLeft, BytesLeft);
  AssertEquals('items', 99, ItemsLeft);
  AssertEquals('bytes', 0, BytesLeft);
end;

function Cand(AIndex:Integer; AAge:Int64; AItems, ABytes:Integer):TS7PlusFillCandidate;
begin
  Result.Index := AIndex;
  Result.Age := AAge;
  Result.Items := AItems;
  Result.Bytes := ABytes;
end;

procedure TTestS7PlusFragments.FillersAreTheOldestThatFit;
var
  C:TS7PlusFillCandidateArray;
  R:TIntegerDynArray;
begin
  SetLength(C, 4);
  C[0] := Cand(10, 100, 1, 12);
  C[1] := Cand(11, 900, 1, 12);
  C[2] := Cand(12, 500, 1, 12);
  C[3] := Cand(13, 500, 1, 12);       //ties with 12: the list order stays
  R := S7PlusPickFillers(C, 100, 971, 100);
  AssertEquals('all', 4, Length(R));
  AssertEquals('oldest', 11, R[0]);
  AssertEquals('tie 1', 12, R[1]);
  AssertEquals('tie 2', 13, R[2]);
  AssertEquals('youngest', 10, R[3]);
  R := S7PlusPickFillers(C, 100, 971, 2);
  AssertEquals('MaxCount', 2, Length(R));
  AssertEquals('first', 11, R[0]);
  R := S7PlusPickFillers(C, 100, 24, 100);
  AssertEquals('only two fit in 24 bytes', 2, Length(R));
  AssertEquals('oldest first', 11, R[0]);
  AssertEquals('then the next', 12, R[1]);
  AssertEquals('none', 0, Length(S7PlusPickFillers(nil, 100, 971, 100)));
end;

procedure TTestS7PlusFragments.FillersNeverCutATagAndRespectTheLimits;
var
  C:TS7PlusFillCandidateArray;
  R:TIntegerDynArray;
begin
  SetLength(C, 3);
  C[0] := Cand(1, 900, 5, 600);       //the oldest is too big for the room: skipped
  C[1] := Cand(2, 800, 1, 100);
  C[2] := Cand(3, 700, 3, 200);       //fits in bytes but not in items
  R := S7PlusPickFillers(C, 2, 300, 100);
  AssertEquals('one fits', 1, Length(R));
  AssertEquals('the younger that fits goes', 2, R[0]);
  AssertEquals('no item room', 0, Length(S7PlusPickFillers(C, 0, 971, 100)));
  AssertEquals('no byte room', 0, Length(S7PlusPickFillers(C, 100, 0, 100)));
  AssertEquals('no count', 0, Length(S7PlusPickFillers(C, 100, 971, 0)));
  R := S7PlusPickFillers(C, 100, 971, 100);
  AssertEquals('all', 3, Length(R));
end;

procedure TTestS7PlusDriverFlags.ReadSomethingAlwaysIsOnByDefault;
var
  D:TS7CommPlusDriver;
begin
  D := TS7CommPlusDriver.Create(nil);
  try
    AssertTrue(D.ReadSomethingAlways);
    AssertFalse(D.ReadOnly);
  finally
    D.Free;
  end;
end;

procedure TTestS7PlusDriverFlags.ReadOnlyBlocksTheDirectWrites;
var
  D:TS7CommPlusDriver;
  Lids:TS7PlusLIDArray;
  Items:TS7PlusMultiWriteItemArray;
  R:TS7PlusMultiWriteResultArray;
begin
  D := TS7CommPlusDriver.Create(nil);
  try
    D.ReadOnly := true;
    SetLength(Lids, 0);
    SetLength(Items, 0);
    AssertTrue('symbolic', D.WriteSymbolic(0, Lids, nil, 0)=ioReadOnlyProtocol);
    AssertTrue('multiple', D.WriteMultipleSymbolic(Items, R)=ioReadOnlyProtocol);
    AssertTrue('DB', D.WriteDB(1, 0, nil)=ioReadOnlyProtocol);
    AssertTrue('area', D.WriteArea(0, 0, nil)=ioReadOnlyProtocol);
  finally
    D.Free;
  end;
end;

initialization
  RegisterTest(TTestS7PlusDriverFlags);
  RegisterTest(TTestS7PlusNotification);
  RegisterTest(TTestS7PlusNotificationQueue);
  RegisterTest(TTestS7PlusFragments);
  RegisterTest(TTestS7PlusMultiWrite);

end.
