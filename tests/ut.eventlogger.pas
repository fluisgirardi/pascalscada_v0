{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do registrador de eventos: THMIEventLogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' o componente que transforma mudanca de tag em linha de historico: uma
  lista de tags vigiados, uma lista de descricoes por valor, e a cada valor
  novo que casa com uma descricao nasce um evento com identificador proprio,
  fechando o evento anterior daquele tag. O SQL em si e' de quem escuta os
  ganchos - o componente so decide quando e com que dados.

  Essa decisao e' o que se testa aqui, e nao precisa de banco nenhum: os
  ganchos entregam tudo por parametro.
}
{$ELSE}
{:
  @abstract(Event logger tests: THMIEventLogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This is the component that turns a tag change into a history row: a list of
  watched tags, a list of descriptions by value, and on every new value that
  matches a description an event is born with its own id, closing that tag's
  previous event. The SQL itself belongs to whoever listens to the hooks - the
  component only decides when, and with what.

  That decision is what is tested here, and it needs no database: the hooks
  hand everything over as parameters.
}
{$ENDIF}
unit ut.eventlogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testregistry,
  HMIEventLogger, HMIDBConnection,
  testsupport.faketag;

type

  { TTestEventDescriptions }

  TTestEventDescriptions = class(TTestCase)
  private
    FLogger:THMIEventLogger;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ANewLoggerHasNoDescriptionsAndNoTags;
    procedure AnAddedDescriptionKeepsWhatWasPutInIt;
    procedure DescriptionsAreIndependentOfEachOther;
    procedure AnAddedTagItemKeepsWhatWasPutInIt;
  end;

  { TTestEventLogger }

  TTestEventLogger = class(TTestCase)
  private
    FLogger:THMIEventLogger;
    FTag:TFakeNumber;
    FItem:TEventTagColletionItem;
    //contadores e ultimos valores vistos pelos ganchos
    FNewCount, FFinishedCount, FIdCount:LongInt;
    FLastDesc:TEventCollectionItem;
    FLastIntID, FLastFinishedIntID:Int64;
    FIdToGive:Int64;
    FIdAnswer:Boolean;

    procedure OnNew(Sender:TObject; TagItem:TEventTagColletionItem;
                    EventIntID:Int64; EventGUID:TGuid;
                    EventDesc:TEventCollectionItem;
                    var NewTagEventSQL:THMIDBConnectionStatementList);
    procedure OnFinished(Sender:TObject; EventIntID:Int64; EventGUID:TGuid;
                         var FinishEventSQL:String);
    function  OnNewId(var EventIntID:Int64; var EventGUID:TGuid):Boolean;
    function  NewDescription(aValue:LongInt; const aText:String):TEventCollectionItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o caminho normal / the ordinary path
    procedure AMatchingValueRaisesAnEvent;
    procedure TheMatchingDescriptionIsHandedToTheHook;
    procedure AValueWithNoDescriptionRaisesNothing;
    procedure TheSameValueTwiceRaisesOnlyOneEvent;
    procedure AValueIsTruncatedBeforeItIsMatched;
    procedure IgnoringTheDescriptionListRaisesOnAnyValue;
    procedure TheTimestampFollowsTheEvent;

    //identificadores / identifiers
    procedure TheInternalCounterGivesGrowingIds;
    procedure TheIdHookTakesOverWhenItIsSet;
    procedure AnIdHookThatRefusesStopsTheEvent;

    //fechamento / closing
    procedure ANewEventClosesThePreviousOne;
    procedure TheFirstEventOfATagClosesNothing;

    //desligamento / detaching
    procedure ClearingTheTagStopsTheNotifications;
    procedure ADestroyedTagIsDroppedFromTheList;

    //sem banco / with no database
    procedure WithNoDatabaseConnectionAnEventDoesNotCrash;
  end;

implementation

{ TTestEventDescriptions }

procedure TTestEventDescriptions.SetUp;
begin
  FLogger:=THMIEventLogger.Create(nil);
end;

procedure TTestEventDescriptions.TearDown;
begin
  FreeAndNil(FLogger);
end;

procedure TTestEventDescriptions.ANewLoggerHasNoDescriptionsAndNoTags;
begin
  AssertEquals('sem descricoes', 0, FLogger.EventDescriptions.Count);
  AssertEquals('sem tags',       0, FLogger.EventTags.Count);
end;

procedure TTestEventDescriptions.AnAddedDescriptionKeepsWhatWasPutInIt;
var
  desc:TEventCollectionItem;
begin
  desc:=FLogger.EventDescriptions.Add;
  desc.EventValue:=3;
  desc.EventDescription:='motor parado por falta de fase';
  desc.EventColor:=clRed;

  AssertEquals('valor', 3, desc.EventValue);
  AssertEquals('texto', 'motor parado por falta de fase', desc.EventDescription);
  AssertEquals('cor',   clRed, desc.EventColor);
end;

procedure TTestEventDescriptions.DescriptionsAreIndependentOfEachOther;
var
  a, b:TEventCollectionItem;
begin
  a:=FLogger.EventDescriptions.Add;
  a.EventValue:=1;
  b:=FLogger.EventDescriptions.Add;
  b.EventValue:=2;

  AssertEquals('duas descricoes', 2, FLogger.EventDescriptions.Count);
  AssertEquals('a primeira', 1, a.EventValue);
  AssertEquals('a segunda',  2, b.EventValue);
end;

procedure TTestEventDescriptions.AnAddedTagItemKeepsWhatWasPutInIt;
var
  item:TEventTagColletionItem;
begin
  item:=FLogger.EventTags.Add;
  item.TagID:=42;
  item.TagPath:='planta/moinho/motor1';
  item.TagDesc:='motor do moinho';

  AssertEquals('identificador', 42, item.TagID);
  AssertEquals('caminho', 'planta/moinho/motor1', item.TagPath);
  AssertEquals('descricao', 'motor do moinho', item.TagDesc);
end;

{ TTestEventLogger }

procedure TTestEventLogger.SetUp;
begin
  FNewCount:=0;
  FFinishedCount:=0;
  FIdCount:=0;
  FLastDesc:=nil;
  FLastIntID:=0;
  FLastFinishedIntID:=0;
  FIdToGive:=1000;
  FIdAnswer:=true;

  FLogger:=THMIEventLogger.Create(nil);
  FLogger.OnNewTagEvent:=@OnNew;
  FLogger.OnTagEventFinished:=@OnFinished;

  //sem banco: o componente decide o evento inteiro antes de ter o que gravar,
  //e os ganchos entregam a decisao por parametro
  FTag:=TFakeNumber.Create(nil);
  FItem:=FLogger.EventTags.Add;
  FItem.PLCTag:=FTag;
end;

procedure TTestEventLogger.TearDown;
begin
  FreeAndNil(FLogger);
  FreeAndNil(FTag);
end;

procedure TTestEventLogger.OnNew(Sender:TObject; TagItem:TEventTagColletionItem;
  EventIntID:Int64; EventGUID:TGuid; EventDesc:TEventCollectionItem;
  var NewTagEventSQL:THMIDBConnectionStatementList);
begin
  inc(FNewCount);
  FLastDesc:=EventDesc;
  FLastIntID:=EventIntID;
end;

procedure TTestEventLogger.OnFinished(Sender:TObject; EventIntID:Int64;
  EventGUID:TGuid; var FinishEventSQL:String);
begin
  inc(FFinishedCount);
  FLastFinishedIntID:=EventIntID;
end;

function TTestEventLogger.OnNewId(var EventIntID:Int64; var EventGUID:TGuid):Boolean;
begin
  inc(FIdCount);
  EventIntID:=FIdToGive;
  CreateGUID(EventGUID);
  Result:=FIdAnswer;
end;

function TTestEventLogger.NewDescription(aValue:LongInt; const aText:String):TEventCollectionItem;
begin
  Result:=FLogger.EventDescriptions.Add;
  Result.EventValue:=aValue;
  Result.EventDescription:=aText;
end;

procedure TTestEventLogger.AMatchingValueRaisesAnEvent;
begin
  NewDescription(3, 'falta de fase');

  FTag.ChegouDoCLP(3);

  AssertEquals('um evento', 1, FNewCount);
end;

procedure TTestEventLogger.TheMatchingDescriptionIsHandedToTheHook;
var
  faltaDeFase, sobrecarga:TEventCollectionItem;
begin
  faltaDeFase:=NewDescription(3, 'falta de fase');
  sobrecarga :=NewDescription(7, 'sobrecarga');

  FTag.ChegouDoCLP(7);

  AssertSame('a descricao do valor 7', sobrecarga, FLastDesc);
end;

procedure TTestEventLogger.AValueWithNoDescriptionRaisesNothing;
begin
  NewDescription(3, 'falta de fase');

  FTag.ChegouDoCLP(99);

  AssertEquals('nenhum evento', 0, FNewCount);
end;

procedure TTestEventLogger.TheSameValueTwiceRaisesOnlyOneEvent;
begin
  //a varredura repete o mesmo valor o tempo todo; so a mudanca vira evento
  NewDescription(3, 'falta de fase');

  FTag.ChegouDoCLP(3);
  FTag.ChegouDoCLP(3);
  FTag.ChegouDoCLP(3);

  AssertEquals('um evento so', 1, FNewCount);
end;

procedure TTestEventLogger.AValueIsTruncatedBeforeItIsMatched;
begin
  //a descricao casa por inteiro; o valor do tag e' real
  NewDescription(3, 'falta de fase');

  FTag.ChegouDoCLP(3.7);

  AssertEquals('3.7 casou com a descricao do 3', 1, FNewCount);
end;

procedure TTestEventLogger.IgnoringTheDescriptionListRaisesOnAnyValue;
begin
  NewDescription(3, 'falta de fase');
  FItem.IgnoreDescriptionList:=true;

  FTag.ChegouDoCLP(99);

  AssertEquals('valor sem descricao tambem virou evento', 1, FNewCount);
end;

procedure TTestEventLogger.TheTimestampFollowsTheEvent;
var
  antes:TDateTime;
begin
  NewDescription(3, 'falta de fase');
  antes:=FLogger.CurrentEventTimestamp;

  FTag.ChegouDoCLP(3);

  AssertTrue('o carimbo avancou', FLogger.CurrentEventTimestamp>=antes);
  AssertTrue('e nao e vazio', FLogger.CurrentEventTimestamp>0);
end;

procedure TTestEventLogger.TheInternalCounterGivesGrowingIds;
var
  primeiro:Int64;
begin
  NewDescription(3, 'falta de fase');
  NewDescription(7, 'sobrecarga');

  FTag.ChegouDoCLP(3);
  primeiro:=FLastIntID;

  FTag.ChegouDoCLP(7);

  AssertTrue('o identificador cresceu', FLastIntID>primeiro);
end;

procedure TTestEventLogger.TheIdHookTakesOverWhenItIsSet;
begin
  NewDescription(3, 'falta de fase');
  FLogger.OnGenerateNewEventID:=@OnNewId;
  FIdToGive:=555;

  FTag.ChegouDoCLP(3);

  AssertEquals('o gancho foi consultado', 1, FIdCount);
  AssertEquals('e o identificador veio dele', 555, FLastIntID);
end;

procedure TTestEventLogger.AnIdHookThatRefusesStopsTheEvent;
begin
  //sem identificador nao ha o que gravar
  NewDescription(3, 'falta de fase');
  FLogger.OnGenerateNewEventID:=@OnNewId;
  FIdAnswer:=false;

  FTag.ChegouDoCLP(3);

  AssertEquals('o gancho foi consultado', 1, FIdCount);
  AssertEquals('mas nenhum evento nasceu', 0, FNewCount);
end;

procedure TTestEventLogger.ANewEventClosesThePreviousOne;
var
  primeiro:Int64;
begin
  NewDescription(3, 'falta de fase');
  NewDescription(7, 'sobrecarga');

  FTag.ChegouDoCLP(3);
  primeiro:=FLastIntID;

  FTag.ChegouDoCLP(7);

  AssertEquals('fechou um evento', 1, FFinishedCount);
  AssertEquals('e foi o anterior', primeiro, FLastFinishedIntID);
end;

procedure TTestEventLogger.TheFirstEventOfATagClosesNothing;
begin
  NewDescription(3, 'falta de fase');

  FTag.ChegouDoCLP(3);

  AssertEquals('nada a fechar', 0, FFinishedCount);
end;

procedure TTestEventLogger.ClearingTheTagStopsTheNotifications;
begin
  NewDescription(3, 'falta de fase');

  FItem.PLCTag:=nil;
  FTag.ChegouDoCLP(3);

  AssertEquals('nenhum evento', 0, FNewCount);
end;

procedure TTestEventLogger.ADestroyedTagIsDroppedFromTheList;
begin
  AssertEquals('um tag na lista', 1, FLogger.EventTags.Count);

  FreeAndNil(FTag);

  AssertEquals('a entrada saiu da lista', 0, FLogger.EventTags.Count);
end;

procedure TTestEventLogger.WithNoDatabaseConnectionAnEventDoesNotCrash;
begin
  //sem banco ligado o evento tem que nascer do mesmo jeito
  FLogger.AsyncDBConnection:=nil;
  NewDescription(3, 'falta de fase');

  FTag.ChegouDoCLP(3);

  AssertEquals('o evento nasceu mesmo sem banco', 1, FNewCount);
end;

initialization
  RegisterTest(TTestEventDescriptions);
  RegisterTest(TTestEventLogger);

end.
