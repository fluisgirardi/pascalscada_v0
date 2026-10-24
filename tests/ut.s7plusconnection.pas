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
  S7PlusConnection, S7PlusTypes,
  testsupport.bytes, testsupport.fakeport;

type

  { TS7ConnectionProbe }

  //A connection that never handshakes: frames come from a fake port, as if the session
  //were already up (no TLS: LogicalRecv then reads the TPKT frames straight from the port).
  TS7ConnectionProbe = class(TS7PlusConnection)
  public
    function NextReply(out Frame:TBytes):Boolean;
    procedure SessionIsUp;
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

initialization
  RegisterTest(TTestS7PlusNotification);
  RegisterTest(TTestS7PlusNotificationQueue);

end.
