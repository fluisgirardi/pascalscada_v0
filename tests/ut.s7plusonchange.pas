{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes dos tags "on change" (RefreshTime=0) do TS7CommPlusDriver: como as
            notificacoes de uma subscricao viram o valor do tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Nao precisa de CLP: o que se testa e' a contabilidade - qual tag um id de referencia
  designa, o que uma notificacao faz com os dados e com o carimbo de tempo (o carimbo e'
  o que faz o framework entregar o valor ao tag), e o que acontece quando a sessao se
  perde. A parte de rede (criar a subscricao, receber, renovar o credito) so se valida
  contra um CLP ou simulador.
}
{$ELSE}
{:
  @abstract(Tests for the on change (RefreshTime=0) tags of TS7CommPlusDriver: how the
            notifications of a subscription become the tag's value.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  No PLC needed: what is tested is the bookkeeping - which tag a reference id
  designates, what a notification does to the data and to the timestamp (the timestamp
  is what makes the framework deliver the value to the tag), and what happens when the
  session is lost. The network side (creating the subscription, receiving, renewing the
  credit) can only be validated against a PLC or simulator.
}
{$ENDIF}
unit ut.s7plusonchange;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, PLCTagNumber,
  S7PlusFamily, S7PlusConnection,
  testsupport.bytes;

type

  { TS7DriverProbe }

  //Opens the protected steps of the driver; everything else is the real driver.
  TS7DriverProbe = class(TS7CommPlusDriver)
  public
    function Attach(const Path:AnsiString; RefBase:Cardinal):Boolean;
    function Resolved(const Path:AnsiString; const Composite:TS7PlusCompositeItemArray):Boolean;
    procedure Apply(const Values:TS7PlusNotificationValueArray; const Errors:TS7PlusNotificationErrorArray; Now_:QWord);
    procedure LoseSession;
    function ValueOf(const Path:AnsiString; Size:LongInt = 0):TScanReadRec;
  end;

  { TTestS7PlusOnChange }

  TTestS7PlusOnChange = class(TTestCase)
  private
    FDriver:TS7DriverProbe;
    FTags:array of TPLCTagNumber;
    function NewTag(const Path:AnsiString; RefreshTime:Integer):TPLCTagNumber;
    function Value(RefId:Cardinal; const HexData:String):TS7PlusNotificationValue;
    function Err(RefId:Cardinal; Status:Byte):TS7PlusNotificationError;
    procedure Notify(const V:array of TS7PlusNotificationValue; const E:array of TS7PlusNotificationError; Now_:QWord);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ATagIsRegisteredWithTheDriverByItsPath;
    procedure AValueBecomesTheDataAndStampsTheEntry;
    procedure AnErrorMarksTheTagAsFailed;
    procedure AnErrorOutweighsAValueOfTheSameTagInOneNotification;
    procedure AValueAfterAnErrorRecoversTheTag;
    procedure AnUnknownReferenceIdIsIgnored;
    procedure NothingIsAppliedToATagThatIsNotSubscribed;
    procedure EachTagReceivesOnlyItsOwnReferenceId;
    procedure ACompositeMemberOnlyChangesItsOwnBytes;
    procedure ACompositeKeepsTheMembersThatDidNotChange;
    procedure LosingTheSessionFlagsTheTagAndForgetsTheIds;
    procedure TheRequestAndScanLimitsHaveTheirDefaults;
    procedure TheLimitsCannotBeSetBelowOne;
  end;

implementation

{ TS7DriverProbe }

function TS7DriverProbe.Attach(const Path:AnsiString; RefBase:Cardinal):Boolean;
begin
  Result := MarkSubscribed(Path, RefBase);
end;

function TS7DriverProbe.Resolved(const Path:AnsiString; const Composite:TS7PlusCompositeItemArray):Boolean;
begin
  Result := MarkResolved(Path, Composite);
end;

procedure TS7DriverProbe.Apply(const Values:TS7PlusNotificationValueArray; const Errors:TS7PlusNotificationErrorArray; Now_:QWord);
begin
  ProcessNotification(Values, Errors, Now_);
end;

procedure TS7DriverProbe.LoseSession;
begin
  ResetSubscriptions;
end;

function TS7DriverProbe.ValueOf(const Path:AnsiString; Size:LongInt):TScanReadRec;
var
  tr:TTagRec;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.Path := Path;
  tr.Size := Size;
  FillChar(Result, SizeOf(Result), 0);
  DoGetValue(tr, Result);
end;

{ TTestS7PlusOnChange }

procedure TTestS7PlusOnChange.SetUp;
begin
  inherited SetUp;
  FDriver := TS7DriverProbe.Create(nil);
end;

procedure TTestS7PlusOnChange.TearDown;
var
  i:Integer;
begin
  for i:=0 to High(FTags) do
    FTags[i].Free;
  FDriver.Free;
  inherited TearDown;
end;

function TTestS7PlusOnChange.NewTag(const Path:AnsiString; RefreshTime:Integer):TPLCTagNumber;
begin
  Result := TPLCTagNumber.Create(nil);
  Result.RefreshTime := RefreshTime;
  Result.LongAddress := Path;
  Result.ProtocolDriver := FDriver;
  SetLength(FTags, Length(FTags)+1);
  FTags[High(FTags)] := Result;
end;

function TTestS7PlusOnChange.Value(RefId:Cardinal; const HexData:String):TS7PlusNotificationValue;
begin
  Result.RefId := RefId;
  Result.Data := BytesOf(HexData);
end;

function TTestS7PlusOnChange.Err(RefId:Cardinal; Status:Byte):TS7PlusNotificationError;
begin
  Result.RefId := RefId;
  Result.Status := Status;
end;

procedure TTestS7PlusOnChange.Notify(const V:array of TS7PlusNotificationValue;
                                     const E:array of TS7PlusNotificationError; Now_:QWord);
var
  Vs:TS7PlusNotificationValueArray;
  Es:TS7PlusNotificationErrorArray;
  i:Integer;
begin
  SetLength(Vs, Length(V));
  for i:=0 to High(V) do
    Vs[i] := V[i];
  SetLength(Es, Length(E));
  for i:=0 to High(E) do
    Es[i] := E[i];
  FDriver.Apply(Vs, Es, Now_);
end;

procedure TTestS7PlusOnChange.ATagIsRegisteredWithTheDriverByItsPath;
begin
  NewTag('DB4.Var1', 0);
  AssertTrue('attached', FDriver.Attach('DB4.Var1', 1));
end;

procedure TTestS7PlusOnChange.AValueBecomesTheDataAndStampsTheEntry;
var
  R:TScanReadRec;
begin
  NewTag('DB4.Var1', 0);
  FDriver.Attach('DB4.Var1', 1);
  Notify([Value(1, '12 34')], [], 5000);
  R := FDriver.ValueOf('DB4.Var1');
  AssertEquals('result', Ord(ioOk), Ord(R.LastQueryResult));
  AssertEquals('stamp', 5000, R.ClkMonotonicTStamp);
  AssertEquals('length', 2, Length(R.Values));
  AssertEquals('byte 0', $12, Trunc(R.Values[0]));
  AssertEquals('byte 1', $34, Trunc(R.Values[1]));
end;

procedure TTestS7PlusOnChange.AnErrorMarksTheTagAsFailed;
var
  R:TScanReadRec;
begin
  NewTag('DB4.Var1', 0);
  FDriver.Attach('DB4.Var1', 1);
  Notify([], [Err(1, $13)], 7000);
  R := FDriver.ValueOf('DB4.Var1');
  AssertEquals('result', Ord(ioCommError), Ord(R.LastQueryResult));
  AssertEquals('stamp', 7000, R.ClkMonotonicTStamp);
  AssertEquals('faults', 1, R.ReadFaults);
end;

procedure TTestS7PlusOnChange.AnErrorOutweighsAValueOfTheSameTagInOneNotification;
var
  R:TScanReadRec;
begin
  NewTag('DB4.Var1', 0);
  FDriver.Attach('DB4.Var1', 1);
  Notify([Value(1, '01')], [Err(1, $03)], 100);
  R := FDriver.ValueOf('DB4.Var1');
  AssertEquals('result', Ord(ioCommError), Ord(R.LastQueryResult));
end;

procedure TTestS7PlusOnChange.AValueAfterAnErrorRecoversTheTag;
var
  R:TScanReadRec;
begin
  NewTag('DB4.Var1', 0);
  FDriver.Attach('DB4.Var1', 1);
  Notify([], [Err(1, $13)], 100);
  Notify([Value(1, '2A')], [], 200);
  R := FDriver.ValueOf('DB4.Var1');
  AssertEquals('result', Ord(ioOk), Ord(R.LastQueryResult));
  AssertEquals('stamp', 200, R.ClkMonotonicTStamp);
  AssertEquals('value', $2A, Trunc(R.Values[0]));
end;

procedure TTestS7PlusOnChange.AnUnknownReferenceIdIsIgnored;
var
  R:TScanReadRec;
begin
  NewTag('DB4.Var1', 0);
  FDriver.Attach('DB4.Var1', 1);
  //id 99 belonged to a tag that was removed while its subscription lives on
  Notify([Value(99, 'FF')], [Err(98, $13)], 300);
  R := FDriver.ValueOf('DB4.Var1');
  AssertEquals('stamp untouched', 0, R.ClkMonotonicTStamp);
  AssertEquals('no data', 0, Length(R.Values));
end;

procedure TTestS7PlusOnChange.NothingIsAppliedToATagThatIsNotSubscribed;
var
  R:TScanReadRec;
begin
  NewTag('DB4.Var1', 0);
  Notify([Value(1, 'FF')], [], 300);
  R := FDriver.ValueOf('DB4.Var1');
  AssertEquals('stamp untouched', 0, R.ClkMonotonicTStamp);
end;

procedure TTestS7PlusOnChange.EachTagReceivesOnlyItsOwnReferenceId;
var
  A, B:TScanReadRec;
begin
  NewTag('DB4.A', 0);
  NewTag('DB4.B', 0);
  FDriver.Attach('DB4.A', 1);
  FDriver.Attach('DB4.B', 2);
  Notify([Value(2, '0B')], [], 400);
  A := FDriver.ValueOf('DB4.A');
  B := FDriver.ValueOf('DB4.B');
  AssertEquals('A untouched', 0, A.ClkMonotonicTStamp);
  AssertEquals('B stamp', 400, B.ClkMonotonicTStamp);
  AssertEquals('B value', $0B, Trunc(B.Values[0]));
end;

procedure TTestS7PlusOnChange.ACompositeMemberOnlyChangesItsOwnBytes;
var
  Members:TS7PlusCompositeItemArray;
  R:TScanReadRec;
begin
  SetLength(Members, 2);
  Members[0].ByteOffset := 0;
  Members[1].ByteOffset := 2;
  NewTag('DB4', 0);
  FDriver.Resolved('DB4', Members);
  //a composite of two members owns ids 10 and 11
  FDriver.Attach('DB4', 10);
  Notify([Value(11, 'CC DD')], [], 500);
  R := FDriver.ValueOf('DB4');
  AssertEquals('length', 4, Length(R.Values));
  AssertEquals('member 1 byte 0', $CC, Trunc(R.Values[2]));
  AssertEquals('member 1 byte 1', $DD, Trunc(R.Values[3]));
end;

procedure TTestS7PlusOnChange.ACompositeKeepsTheMembersThatDidNotChange;
var
  Members:TS7PlusCompositeItemArray;
  R:TScanReadRec;
begin
  SetLength(Members, 2);
  Members[0].ByteOffset := 0;
  Members[1].ByteOffset := 2;
  NewTag('DB4', 0);
  FDriver.Resolved('DB4', Members);
  FDriver.Attach('DB4', 10);
  Notify([Value(10, 'AA BB'), Value(11, 'CC DD')], [], 600);
  Notify([Value(11, 'EE FF')], [], 700);
  R := FDriver.ValueOf('DB4');
  AssertEquals('stamp', 700, R.ClkMonotonicTStamp);
  AssertEquals('member 0 kept', $AA, Trunc(R.Values[0]));
  AssertEquals('member 0 kept (2)', $BB, Trunc(R.Values[1]));
  AssertEquals('member 1 new', $EE, Trunc(R.Values[2]));
  AssertEquals('member 1 new (2)', $FF, Trunc(R.Values[3]));
end;

procedure TTestS7PlusOnChange.LosingTheSessionFlagsTheTagAndForgetsTheIds;
var
  R:TScanReadRec;
begin
  NewTag('DB4.Var1', 0);
  FDriver.Attach('DB4.Var1', 1);
  Notify([Value(1, '01')], [], 800);
  FDriver.LoseSession;
  R := FDriver.ValueOf('DB4.Var1');
  AssertEquals('flagged as failed', Ord(ioCommError), Ord(R.LastQueryResult));
  //the old id means nothing now: a stray late notification must not revive the tag
  Notify([Value(1, '02')], [], 900);
  R := FDriver.ValueOf('DB4.Var1');
  AssertEquals('still failed', Ord(ioCommError), Ord(R.LastQueryResult));
end;

procedure TTestS7PlusOnChange.TheRequestAndScanLimitsHaveTheirDefaults;
begin
  //100 items per request is what a PLC was measured to accept; a subscription is bounded by
  //size (~2000 items measured) so it gets a conservative 500
  AssertEquals('items per request', 100, FDriver.MaxItemsPerRequest);
  AssertEquals('tags per scan round', 500, FDriver.MaxTagsPerScan);
  AssertEquals('items per subscription', 500, FDriver.MaxItemsPerSubscription);
end;

procedure TTestS7PlusOnChange.TheLimitsCannotBeSetBelowOne;
begin
  FDriver.MaxItemsPerRequest := 0;
  FDriver.MaxTagsPerScan := -3;
  FDriver.MaxItemsPerSubscription := 0;
  AssertEquals('items per request', 1, FDriver.MaxItemsPerRequest);
  AssertEquals('tags per scan round', 1, FDriver.MaxTagsPerScan);
  AssertEquals('items per subscription', 1, FDriver.MaxItemsPerSubscription);
  FDriver.MaxItemsPerRequest := 250;
  AssertEquals('and a larger one is kept', 250, FDriver.MaxItemsPerRequest);
end;

initialization
  RegisterTest(TTestS7PlusOnChange);

end.
