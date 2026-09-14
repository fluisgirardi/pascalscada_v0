{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do registrador de alarmes: THMIAlarmLogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Cada alarme e' um criterio numerico sobre um tag - acima de tal valor, fora
  de tal faixa, com tal bit ligado. Quando o criterio passa a valer nasce um
  alarme com identificador proprio; quando deixa de valer, o mesmo
  identificador e' fechado. Entre uma coisa e outra a varredura repete o mesmo
  valor centenas de vezes, e nada disso pode virar alarme novo.

  Errar aqui e' caro dos dois lados: alarme que nao entra e' operador sem
  aviso; alarme que entra de novo a cada varredura enche a tela e o banco, e o
  que interessa se perde no meio.

  O criterio e' uma copia da conta que o TZones.GetZoneFromValue faz, escrita
  de novo dentro do logger - por isso vale conferir os sete tipos aqui
  tambem, e nao so la'.
}
{$ELSE}
{:
  @abstract(Alarm logger tests: THMIAlarmLogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Every alarm is a numeric criterion over a tag - above such a value, outside
  such a range, with such a bit set. When the criterion starts holding an
  alarm is born with its own id; when it stops, that same id is closed. In
  between, the scan repeats the same value hundreds of times, and none of that
  may turn into a new alarm.

  Getting it wrong is expensive both ways: an alarm that never comes in is an
  operator with no warning; an alarm that comes in again on every scan fills
  the screen and the database, and what matters is lost in the noise.

  The criterion is a copy of the arithmetic TZones.GetZoneFromValue does,
  written again inside the logger - which is why the seven types are worth
  checking here too, not only there.
}
{$ENDIF}
unit ut.alarmlogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  hmizones, HMIAlarmLogger,
  testsupport.faketag;

type

  { TTestAlarmLogger }

  TTestAlarmLogger = class(TTestCase)
  private
    FLogger:THMIAlarmLogger;
    FTag:TFakeNumber;
    FItem:TAlarmItem;
    FInCount, FOutCount, FIdCount:LongInt;
    FLastInID, FLastOutID:Int64;
    FLastMsgItem:TAlarmItem;
    FIdToGive:Int64;
    FIdAnswer:Boolean;

    procedure OnIn(Sender:TObject; aTimeStamp:TDateTime; AlarmMsgItem:TAlarmItem;
                   AlarmIntID:Int64; AlarmGUID:TGuid; var AlarmIncommingSQL:UTF8String);
    procedure OnOut(Sender:TObject; aTimeStamp:TDateTime; AlamrIntID:Int64;
                    AlarmGUID:TGuid; var OutgoingAlarmSQL:UTF8String);
    function  OnNewId(var AlarmIntID:Int64; var AlarmGUID:TGuid):Boolean;
    //: prepara o alarme do teste com um tipo e um valor principal
    procedure AlarmOn(aType:TZoneTypes; v:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //estado inicial / starting state
    procedure ANewLoggerHasNoAlarms;
    procedure AnAddedAlarmKeepsItsMessageAndExtraInfo;
    procedure ANewAlarmIsNotActive;

    //os sete criterios / the seven criteria
    procedure AnEqualAlarmComesInOnTheExactValue;
    procedure ANotEqualAlarmComesInOnAnythingElse;
    procedure ARangeAlarmLeavesTheBoundsOutByDefault;
    procedure ARangeAlarmTakesTheBoundsWhenAsked;
    procedure AnOutOfRangeAlarmComesInOutside;
    procedure AGreaterThanAlarmComesInAbove;
    procedure ALessThanAlarmComesInBelow;
    procedure ABitAlarmComesInWithThatBitSet;
    procedure ABitAlarmCanAlsoWatchForTheBitClear;
    procedure EveryBitFromZeroToThirtyOneIsUsable;

    //entrada e saida / in and out
    procedure TheAlarmComesInOnlyOnce;
    procedure LeavingTheConditionClosesTheAlarm;
    procedure TheAlarmGoesOutOnlyOnce;
    procedure TheClosedAlarmCarriesTheIdThatCameIn;
    procedure ComingBackRaisesANewId;
    procedure AlarmActiveFollowsTheState;
    procedure AValueThatNeverMatchesRaisesNothing;

    //identificadores / identifiers
    procedure TheInternalCounterGivesGrowingIds;
    procedure TheIdHookTakesOverWhenItIsSet;
    procedure AnIdHookThatRefusesStopsTheAlarm;

    //varios alarmes / several alarms
    procedure TwoAlarmsOnTheSameTagAreBothJudged;
    procedure AnAlarmOnAnotherTagIsLeftAlone;

    //desligamento / detaching
    procedure ClearingTheTagStopsTheNotifications;
  end;

implementation

{ TTestAlarmLogger }

procedure TTestAlarmLogger.SetUp;
begin
  FInCount:=0;
  FOutCount:=0;
  FIdCount:=0;
  FLastInID:=0;
  FLastOutID:=0;
  FLastMsgItem:=nil;
  FIdToGive:=1000;
  FIdAnswer:=true;

  FLogger:=THMIAlarmLogger.Create(nil);
  FLogger.OnIncomingAlarm:=@OnIn;
  FLogger.OnOutgoingAlarm:=@OnOut;

  FTag:=TFakeNumber.Create(nil);
  FItem:=FLogger.AlarmMessages.Add;
  FItem.PLCTag:=FTag;
end;

procedure TTestAlarmLogger.TearDown;
begin
  FreeAndNil(FLogger);
  FreeAndNil(FTag);
end;

procedure TTestAlarmLogger.OnIn(Sender:TObject; aTimeStamp:TDateTime;
  AlarmMsgItem:TAlarmItem; AlarmIntID:Int64; AlarmGUID:TGuid;
  var AlarmIncommingSQL:UTF8String);
begin
  inc(FInCount);
  FLastInID:=AlarmIntID;
  FLastMsgItem:=AlarmMsgItem;
end;

procedure TTestAlarmLogger.OnOut(Sender:TObject; aTimeStamp:TDateTime;
  AlamrIntID:Int64; AlarmGUID:TGuid; var OutgoingAlarmSQL:UTF8String);
begin
  inc(FOutCount);
  FLastOutID:=AlamrIntID;
end;

function TTestAlarmLogger.OnNewId(var AlarmIntID:Int64; var AlarmGUID:TGuid):Boolean;
begin
  inc(FIdCount);
  AlarmIntID:=FIdToGive;
  CreateGUID(AlarmGUID);
  Result:=FIdAnswer;
end;

procedure TTestAlarmLogger.AlarmOn(aType:TZoneTypes; v:Double);
begin
  FItem.ZoneType:=aType;
  if aType=ztBit then
    FItem.Value1:=v      //ztBit nao passa pela ordenacao de Value1/Value2
  else
    FItem.SetValues(v, v);
end;

procedure TTestAlarmLogger.ANewLoggerHasNoAlarms;
var
  vazio:THMIAlarmLogger;
begin
  vazio:=THMIAlarmLogger.Create(nil);
  try
    AssertEquals('sem alarmes', 0, vazio.AlarmMessages.Count);
  finally
    vazio.Free;
  end;
end;

procedure TTestAlarmLogger.AnAddedAlarmKeepsItsMessageAndExtraInfo;
begin
  FItem.AlarmMessage:='temperatura do mancal acima do limite';
  FItem.ExtraInfo:='parar o moinho e chamar a manutencao';

  AssertEquals('mensagem', 'temperatura do mancal acima do limite', FItem.AlarmMessage);
  AssertEquals('informacao extra', 'parar o moinho e chamar a manutencao', FItem.ExtraInfo);
end;

procedure TTestAlarmLogger.ANewAlarmIsNotActive;
begin
  AssertFalse('nasce apagado', FItem.AlarmActive);
end;

procedure TTestAlarmLogger.AnEqualAlarmComesInOnTheExactValue;
begin
  AlarmOn(ztEqual, 10);

  FTag.ChegouDoCLP(10);
  AssertEquals('entrou em 10', 1, FInCount);

  FTag.ChegouDoCLP(11);
  AssertEquals('e saiu em 11', 1, FOutCount);
end;

procedure TTestAlarmLogger.ANotEqualAlarmComesInOnAnythingElse;
begin
  AlarmOn(ztNotEqual, 10);

  FTag.ChegouDoCLP(11);
  AssertEquals('entrou em 11', 1, FInCount);

  FTag.ChegouDoCLP(10);
  AssertEquals('e saiu em 10', 1, FOutCount);
end;

procedure TTestAlarmLogger.ARangeAlarmLeavesTheBoundsOutByDefault;
begin
  FItem.ZoneType:=ztRange;
  FItem.SetValues(10, 20);

  FTag.ChegouDoCLP(15);
  AssertEquals('15 esta dentro', 1, FInCount);

  FTag.ChegouDoCLP(20);
  AssertEquals('20 esta fora',   1, FOutCount);
end;

procedure TTestAlarmLogger.ARangeAlarmTakesTheBoundsWhenAsked;
begin
  FItem.ZoneType:=ztRange;
  FItem.SetValues(10, 20);
  FItem.IncludeValue1:=true;
  FItem.IncludeValue2:=true;

  FTag.ChegouDoCLP(20);

  AssertEquals('20 entra com o limite', 1, FInCount);
end;

procedure TTestAlarmLogger.AnOutOfRangeAlarmComesInOutside;
begin
  FItem.ZoneType:=ztOutOfRange;
  FItem.SetValues(10, 20);

  FTag.ChegouDoCLP(25);
  AssertEquals('25 esta fora da faixa', 1, FInCount);

  FTag.ChegouDoCLP(15);
  AssertEquals('e 15 esta dentro',      1, FOutCount);
end;

procedure TTestAlarmLogger.AGreaterThanAlarmComesInAbove;
begin
  AlarmOn(ztGreaterThan, 10);

  FTag.ChegouDoCLP(11);
  AssertEquals('11 passa do limite', 1, FInCount);

  FTag.ChegouDoCLP(10);
  AssertEquals('10 nao, sem o limite incluso', 1, FOutCount);
end;

procedure TTestAlarmLogger.ALessThanAlarmComesInBelow;
begin
  AlarmOn(ztLessThan, 10);

  FTag.ChegouDoCLP(9);
  AssertEquals('9 esta abaixo', 1, FInCount);

  FTag.ChegouDoCLP(10);
  AssertEquals('10 nao',        1, FOutCount);
end;

procedure TTestAlarmLogger.ABitAlarmComesInWithThatBitSet;
begin
  AlarmOn(ztBit, 3);
  FItem.IncludeValue1:=true;

  FTag.ChegouDoCLP(8);
  AssertEquals('8 tem o bit 3', 1, FInCount);

  FTag.ChegouDoCLP(7);
  AssertEquals('7 nao tem',     1, FOutCount);
end;

procedure TTestAlarmLogger.ABitAlarmCanAlsoWatchForTheBitClear;
begin
  AlarmOn(ztBit, 3);
  FItem.IncludeValue1:=false;

  FTag.ChegouDoCLP(7);
  AssertEquals('7 esta com o bit 3 desligado', 1, FInCount);
end;

procedure TTestAlarmLogger.EveryBitFromZeroToThirtyOneIsUsable;
var
  b:LongInt;
  valor:Int64;
begin
  //o bit vira mascara por Trunc(Power(2,bit)); se a potencia em ponto
  //flutuante escorregar um fio, a mascara sai errada e o alarme nao entra
  FItem.IncludeValue1:=true;
  for b:=0 to 31 do begin
    FInCount:=0;
    FOutCount:=0;
    FItem.LastAlarmGUID :=GUID_NULL;
    FItem.LastAlarmIntID:=0;
    AlarmOn(ztBit, b);

    valor:=Int64(1) shl b;
    FTag.ChegouDoCLP(0);
    FTag.ChegouDoCLP(valor);

    AssertEquals('bit '+IntToStr(b)+' ligado', 1, FInCount);
  end;
end;

procedure TTestAlarmLogger.TheAlarmComesInOnlyOnce;
begin
  //a varredura repete o mesmo valor o tempo todo
  AlarmOn(ztGreaterThan, 10);

  FTag.ChegouDoCLP(50);
  FTag.ChegouDoCLP(51);
  FTag.ChegouDoCLP(52);

  AssertEquals('um alarme so', 1, FInCount);
end;

procedure TTestAlarmLogger.LeavingTheConditionClosesTheAlarm;
begin
  AlarmOn(ztGreaterThan, 10);

  FTag.ChegouDoCLP(50);
  FTag.ChegouDoCLP(5);

  AssertEquals('fechou', 1, FOutCount);
end;

procedure TTestAlarmLogger.TheAlarmGoesOutOnlyOnce;
begin
  AlarmOn(ztGreaterThan, 10);

  FTag.ChegouDoCLP(50);
  FTag.ChegouDoCLP(5);
  FTag.ChegouDoCLP(4);
  FTag.ChegouDoCLP(3);

  AssertEquals('uma saida so', 1, FOutCount);
end;

procedure TTestAlarmLogger.TheClosedAlarmCarriesTheIdThatCameIn;
begin
  AlarmOn(ztGreaterThan, 10);

  FTag.ChegouDoCLP(50);
  FTag.ChegouDoCLP(5);

  AssertEquals('mesmo identificador', FLastInID, FLastOutID);
end;

procedure TTestAlarmLogger.ComingBackRaisesANewId;
var
  primeiro:Int64;
begin
  AlarmOn(ztGreaterThan, 10);

  FTag.ChegouDoCLP(50);
  primeiro:=FLastInID;
  FTag.ChegouDoCLP(5);
  FTag.ChegouDoCLP(60);

  AssertEquals('dois alarmes', 2, FInCount);
  AssertTrue('com identificadores diferentes', FLastInID<>primeiro);
end;

procedure TTestAlarmLogger.AlarmActiveFollowsTheState;
begin
  AlarmOn(ztGreaterThan, 10);

  FTag.ChegouDoCLP(50);
  AssertTrue('aceso', FItem.AlarmActive);

  FTag.ChegouDoCLP(5);
  AssertFalse('apagado', FItem.AlarmActive);
end;

procedure TTestAlarmLogger.AValueThatNeverMatchesRaisesNothing;
begin
  AlarmOn(ztGreaterThan, 100);

  FTag.ChegouDoCLP(5);
  FTag.ChegouDoCLP(6);

  AssertEquals('nenhum alarme', 0, FInCount);
  AssertEquals('nenhuma saida', 0, FOutCount);
end;

procedure TTestAlarmLogger.TheInternalCounterGivesGrowingIds;
var
  primeiro:Int64;
begin
  AlarmOn(ztGreaterThan, 10);

  FTag.ChegouDoCLP(50);
  primeiro:=FLastInID;
  FTag.ChegouDoCLP(5);
  FTag.ChegouDoCLP(60);

  AssertTrue('o identificador cresceu', FLastInID>primeiro);
end;

procedure TTestAlarmLogger.TheIdHookTakesOverWhenItIsSet;
begin
  AlarmOn(ztGreaterThan, 10);
  FLogger.OnGenerateNewAlarmID:=@OnNewId;
  FIdToGive:=777;

  FTag.ChegouDoCLP(50);

  AssertEquals('o gancho foi consultado', 1, FIdCount);
  AssertEquals('e o identificador veio dele', 777, FLastInID);
end;

procedure TTestAlarmLogger.AnIdHookThatRefusesStopsTheAlarm;
begin
  AlarmOn(ztGreaterThan, 10);
  FLogger.OnGenerateNewAlarmID:=@OnNewId;
  FIdAnswer:=false;

  FTag.ChegouDoCLP(50);

  AssertEquals('o gancho foi consultado', 1, FIdCount);
  AssertEquals('mas nenhum alarme entrou', 0, FInCount);
  AssertFalse('e o item continua apagado', FItem.AlarmActive);
end;

procedure TTestAlarmLogger.TwoAlarmsOnTheSameTagAreBothJudged;
var
  segundo:TAlarmItem;
begin
  //o mesmo tag pode ter alarme de alta e de muito alta
  AlarmOn(ztGreaterThan, 10);

  segundo:=FLogger.AlarmMessages.Add;
  segundo.ZoneType:=ztGreaterThan;
  segundo.SetValues(40, 40);
  segundo.PLCTag:=FTag;

  FTag.ChegouDoCLP(50);

  AssertEquals('os dois entraram', 2, FInCount);
end;

procedure TTestAlarmLogger.AnAlarmOnAnotherTagIsLeftAlone;
var
  outroTag:TFakeNumber;
  outroItem:TAlarmItem;
begin
  outroTag:=TFakeNumber.Create(nil);
  try
    AlarmOn(ztGreaterThan, 10);

    outroItem:=FLogger.AlarmMessages.Add;
    outroItem.ZoneType:=ztGreaterThan;
    outroItem.SetValues(10, 10);
    outroItem.PLCTag:=outroTag;

    FTag.ChegouDoCLP(50);

    AssertEquals('so o alarme do tag que mudou', 1, FInCount);
    AssertFalse('o outro continua apagado', outroItem.AlarmActive);
  finally
    outroTag.Free;
  end;
end;

procedure TTestAlarmLogger.ClearingTheTagStopsTheNotifications;
begin
  AlarmOn(ztGreaterThan, 10);

  FItem.PLCTag:=nil;
  FTag.ChegouDoCLP(50);

  AssertEquals('nenhum alarme', 0, FInCount);
end;

initialization
  RegisterTest(TTestAlarmLogger);

end.
