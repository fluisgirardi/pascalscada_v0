{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes das colecoes de HMI e das zonas de animacao:
            THMIBasicColletion, TZone e TZones.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Uma zona e' um criterio numerico com um desenho pendurado nele - "entre 10 e
  20 mostre amarelo". Quem escolhe qual zona vale e' o GetZoneFromValue, e ele
  e' computacao pura: nao toca em tela nenhuma, so percorre a colecao aplicando
  o criterio de cada uma. Se escolher errado, o operador ve verde num tanque
  que esta' transbordando.

  A colecao por baixo avisa o controle dono a cada mudanca, e e' esse aviso
  que faz a tela se redesenhar. Os testes conferem tanto o criterio quanto o
  aviso.
}
{$ELSE}
{:
  @abstract(HMI collection and animation zone tests: THMIBasicColletion,
            TZone and TZones.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A zone is a numeric criterion with a drawing hanging from it - "between 10
  and 20 show yellow". What picks the winning zone is GetZoneFromValue, and it
  is pure computation: it touches no screen, it just walks the collection
  applying each criterion. Picking the wrong one shows the operator green on a
  tank that is overflowing.

  The collection underneath notifies the owning control on every change, and
  it is that notification that makes the screen redraw. These tests cover both
  the criterion and the notification.
}
{$ENDIF}
unit ut.hmizones;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  hmibasiccolletion, hmizones;

type

  { TTestHMICollection }

  TTestHMICollection = class(TTestCase)
  private
    FDono:TComponent;
    FZones:TZones;
    FChangeCount:LongInt;
    FLastChanged:TObject;
    FState:TComponentState;
    procedure CountChange(Sender:TObject);
    procedure GiveTheState(var CurState:TComponentState);
    function  NewZone(aType:TZoneTypes; aV1:Double):TAnimationZone;
    //: poe o valor principal sem cair na troca Value1/Value2
    procedure PutMainValue(aZone:TAnimationZone; v:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ANewCollectionIsEmpty;
    procedure AddingAnItemNotifies;
    procedure TheNotificationCarriesTheItemThatChanged;
    procedure ChangingAValueNotifies;
    procedure SettingTheSameValueDoesNotNotify;
    procedure BeginUpdateHoldsTheNotificationBack;
    procedure WhileReadingNothingIsNotified;
    procedure TheOwnerIsTheOneGivenToTheConstructor;
    procedure AssignCopiesEveryItemAcross;
    procedure AssignReplacesWhatWasThere;
    procedure ACollectionWithNoOwningComponentStillTakesItems;
  end;

  { TTestZones }

  TTestZones = class(TTestCase)
  private
    FDono:TComponent;
    FZones:TZones;
    FChangeCount:LongInt;
    procedure CountChange(Sender:TObject);
    function  NewZone(aType:TZoneTypes):TAnimationZone;
    function  NewZoneWith(aType:TZoneTypes; v:Double):TAnimationZone;
    function  NewRange(aFrom, aTo:Double; aIncFrom, aIncTo:Boolean):TAnimationZone;
    //: numero do item escolhido para o valor, ou -1 / index chosen for the value, or -1
    function  ChosenFor(v:Double):LongInt;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //valores guardados / stored values
    procedure TheSettersKeepValue1BelowValue2;
    procedure SettingBothValuesAtOnceLeavesThemWhereTheyWerePut;
    procedure SettingBothValuesStillKeepsTheOrder;
    procedure SettingBothValuesNotifiesOnlyOnce;
    procedure ABitZoneOnlyTakesValue1BetweenZeroAndThirtyOne;
    procedure ChangingToBitWithAnImpossibleValueIsRefused;

    //criterios / criteria
    procedure AnEqualZoneTakesTheExactValue;
    procedure ANotEqualZoneTakesEverythingElse;
    procedure ARangeLeavesTheBoundsOutByDefault;
    procedure ARangeTakesTheBoundsWhenAsked;
    procedure AnOutOfRangeZoneTakesWhatIsOutside;
    procedure AGreaterThanZoneTakesWhatIsAbove;
    procedure ALessThanZoneTakesWhatIsBelow;
    procedure ABitZoneTakesTheValueWithThatBitSet;
    procedure ABitZoneCanAlsoLookForTheBitClear;
    procedure TheHighestAllowedBitIsUsable;

    //escolha entre varias / choosing among many
    procedure TheFirstMatchingZoneWins;
    procedure WithNoMatchAndNoDefaultNobodyIsChosen;
    procedure TheDefaultZoneAnswersWhenNothingElseMatches;
    procedure AMatchingZoneBeatsTheDefault;
    procedure TheDefaultZoneIsNotJudgedByItsOwnCriterion;
    procedure OnlyOneZoneCanBeTheDefault;
    procedure GetDefaultZoneFindsIt;
    procedure GetDefaultZoneGivesNilWhenThereIsNone;
    procedure AnEmptyCollectionChoosesNobody;
  end;

implementation

{ TTestHMICollection }

procedure TTestHMICollection.SetUp;
begin
  FChangeCount:=0;
  FLastChanged:=nil;
  FState:=[];

  //a zona le o estado do componente dono na criacao, entao a colecao precisa
  //de um dono de verdade
  FDono:=TComponent.Create(nil);
  FZones:=TZones.Create(FDono, TAnimationZone);
  FZones.OnNeedCompState:=@GiveTheState;
  FZones.OnCollectionItemChange:=@CountChange;
end;

procedure TTestHMICollection.TearDown;
begin
  FreeAndNil(FZones);
  FreeAndNil(FDono);
end;

procedure TTestHMICollection.CountChange(Sender:TObject);
begin
  inc(FChangeCount);
  FLastChanged:=Sender;
end;

procedure TTestHMICollection.GiveTheState(var CurState:TComponentState);
begin
  CurState:=FState;
end;

procedure TTestHMICollection.PutMainValue(aZone:TAnimationZone; v:Double);
begin
  //Value1 sozinho seria empurrado para o Value2 pela ordenacao; SetValues poe
  //os dois de uma vez
  aZone.SetValues(v, v);
end;

function TTestHMICollection.NewZone(aType:TZoneTypes; aV1:Double):TAnimationZone;
begin
  Result:=TAnimationZone(FZones.Add);
  Result.ZoneType:=aType;
  PutMainValue(Result, aV1);
end;

procedure TTestHMICollection.ANewCollectionIsEmpty;
begin
  AssertEquals('sem itens', 0, FZones.Count);
  AssertTrue('sem zona padrao', FZones.GetDefaultZone=nil);
end;

procedure TTestHMICollection.AddingAnItemNotifies;
begin
  FZones.Add;

  AssertEquals('um item', 1, FZones.Count);
  AssertEquals('um aviso', 1, FChangeCount);
end;

procedure TTestHMICollection.TheNotificationCarriesTheItemThatChanged;
var
  zona:TAnimationZone;
begin
  zona:=TAnimationZone(FZones.Add);

  AssertSame('o aviso trouxe o item', zona, FLastChanged);
end;

procedure TTestHMICollection.ChangingAValueNotifies;
var
  zona:TAnimationZone;
begin
  zona:=NewZone(ztEqual, 0);
  FChangeCount:=0;

  zona.Value1:=10;

  AssertEquals('a mudanca avisou', 1, FChangeCount);
  AssertSame('e trouxe a zona', zona, FLastChanged);
end;

procedure TTestHMICollection.SettingTheSameValueDoesNotNotify;
var
  zona:TAnimationZone;
begin
  zona:=NewZone(ztEqual, 10);
  FChangeCount:=0;

  zona.Value1:=10;

  AssertEquals('nada mudou, nada a avisar', 0, FChangeCount);
end;

procedure TTestHMICollection.BeginUpdateHoldsTheNotificationBack;
begin
  //quem vai mexer em varias zonas de uma vez nao quer a tela redesenhando a
  //cada uma
  FZones.BeginUpdate;
  try
    FZones.Add;
    FZones.Add;
    AssertEquals('nenhum aviso durante a atualizacao', 0, FChangeCount);
  finally
    FZones.EndUpdate;
  end;

  AssertEquals('as duas entraram', 2, FZones.Count);
end;

procedure TTestHMICollection.WhileReadingNothingIsNotified;
var
  zona:TAnimationZone;
begin
  //carregando o .lfm o controle ainda esta se montando; avisar agora faria a
  //tela se desenhar pela metade
  zona:=NewZone(ztEqual, 0);
  FChangeCount:=0;
  FState:=[csReading];

  zona.Value1:=10;
  FZones.Add;

  AssertEquals('silencio durante a leitura', 0, FChangeCount);
  AssertEquals('mas o valor foi guardado', 10, zona.Value1, 0);
end;

procedure TTestHMICollection.TheOwnerIsTheOneGivenToTheConstructor;
var
  dono:TComponent;
  zonas:TZones;
begin
  dono:=TComponent.Create(nil);
  try
    zonas:=TZones.Create(dono, TAnimationZone);
    try
      AssertSame('o dono informado', dono, zonas.Owner);
    finally
      zonas.Free;
    end;
  finally
    dono.Free;
  end;
end;

procedure TTestHMICollection.AssignCopiesEveryItemAcross;
var
  outras:TZones;
begin
  NewZone(ztEqual, 10);
  NewZone(ztGreaterThan, 20);

  outras:=TZones.Create(FDono, TAnimationZone);
  try
    outras.Assign(FZones);

    AssertEquals('mesma quantidade', 2, outras.Count);
    AssertEquals('valor do primeiro', 10, TAnimationZone(outras.Items[0]).Value1, 0);
    AssertEquals('tipo do segundo', Ord(ztGreaterThan),
                 Ord(TAnimationZone(outras.Items[1]).ZoneType));
  finally
    outras.Free;
  end;
end;

procedure TTestHMICollection.AssignReplacesWhatWasThere;
var
  outras:TZones;
begin
  NewZone(ztEqual, 10);

  outras:=TZones.Create(FDono, TAnimationZone);
  try
    outras.Add;
    outras.Add;
    outras.Add;

    outras.Assign(FZones);

    AssertEquals('o que havia saiu', 1, outras.Count);
  finally
    outras.Free;
  end;
end;

procedure TTestHMICollection.ACollectionWithNoOwningComponentStillTakesItems;
var
  zonas:TZones;
begin
  //a colecao aceita qualquer TPersistent como dono, inclusive nenhum
  zonas:=TZones.Create(nil, TAnimationZone);
  try
    zonas.Add;
    AssertEquals('um item', 1, zonas.Count);
  finally
    zonas.Free;
  end;
end;

{ TTestZones }

procedure TTestZones.SetUp;
begin
  FChangeCount:=0;
  FDono:=TComponent.Create(nil);
  FZones:=TZones.Create(FDono, TAnimationZone);
end;

procedure TTestZones.TearDown;
begin
  FreeAndNil(FZones);
  FreeAndNil(FDono);
end;

procedure TTestZones.CountChange(Sender:TObject);
begin
  inc(FChangeCount);
end;

function TTestZones.NewZone(aType:TZoneTypes):TAnimationZone;
begin
  Result:=TAnimationZone(FZones.Add);
  Result.ZoneType:=aType;
end;

function TTestZones.NewZoneWith(aType:TZoneTypes; v:Double):TAnimationZone;
begin
  //SetValues poe os dois de uma vez, sem passar pela ordenacao de Value1
  //sozinho
  Result:=NewZone(aType);
  Result.SetValues(v, v);
end;

function TTestZones.NewRange(aFrom, aTo:Double; aIncFrom, aIncTo:Boolean):TAnimationZone;
begin
  //Value2 primeiro: os setters trocam os dois para manter Value1<=Value2, e
  //numa zona recem criada o Value2 vale zero
  Result:=NewZone(ztRange);
  Result.SetValues(aFrom, aTo);
  Result.IncludeValue1:=aIncFrom;
  Result.IncludeValue2:=aIncTo;
end;

function TTestZones.ChosenFor(v:Double):LongInt;
var
  escolhida:TZone;
  c:LongInt;
begin
  Result:=-1;
  escolhida:=FZones.GetZoneFromValue(v);
  if escolhida=nil then exit;
  for c:=0 to FZones.Count-1 do
    if FZones.Items[c]=escolhida then begin
      Result:=c;
      exit;
    end;
end;

procedure TTestZones.TheSettersKeepValue1BelowValue2;
var
  zona:TAnimationZone;
begin
  //quem escreve Value1 maior que Value2 acaba com os dois trocados, e nao com
  //uma faixa invertida que nunca casa
  zona:=NewZone(ztRange);

  zona.Value1:=10;

  AssertEquals('o maior foi para o Value2', 10, zona.Value2, 0);
  AssertEquals('e o Value1 ficou com o menor', 0, zona.Value1, 0);
end;

procedure TTestZones.SettingBothValuesAtOnceLeavesThemWhereTheyWerePut;
var
  zona:TAnimationZone;
begin
  //Value1 sozinho e empurrado para o Value2 quando passa dele, e numa zona
  //nova o Value2 vale zero. SetValues e o caminho para quem quer os dois
  zona:=NewZone(ztEqual);

  zona.SetValues(10, 10);

  AssertEquals('Value1 ficou onde foi posto', 10, zona.Value1, 0);
  AssertEquals('bate com 10', 0, ChosenFor(10));
end;

procedure TTestZones.SettingBothValuesStillKeepsTheOrder;
var
  zona:TAnimationZone;
begin
  //a ordenacao continua valendo, so que aplicada uma vez so
  zona:=NewZone(ztRange);

  zona.SetValues(20, 10);

  AssertEquals('o menor foi para o Value1', 10, zona.Value1, 0);
  AssertEquals('e o maior para o Value2',   20, zona.Value2, 0);
end;

procedure TTestZones.SettingBothValuesNotifiesOnlyOnce;
var
  zona:TAnimationZone;
begin
  //dois valores, um aviso: o ponto de pedir os dois de uma vez e' nao fazer a
  //tela se redesenhar no meio do caminho
  zona:=NewZone(ztRange);
  FChangeCount:=0;
  FZones.OnCollectionItemChange:=@CountChange;
  try
    zona.SetValues(10, 20);
    AssertEquals('um aviso so', 1, FChangeCount);

    zona.SetValues(10, 20);
    AssertEquals('repetir os mesmos valores nao avisa', 1, FChangeCount);
  finally
    FZones.OnCollectionItemChange:=nil;
  end;
end;

procedure TTestZones.ABitZoneOnlyTakesValue1BetweenZeroAndThirtyOne;
var
  zona:TAnimationZone;
begin
  zona:=NewZone(ztBit);

  try
    zona.Value1:=32;
    Fail('bit 32 tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  try
    zona.Value1:=-1;
    Fail('bit negativo tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;
end;

procedure TTestZones.ChangingToBitWithAnImpossibleValueIsRefused;
var
  zona:TAnimationZone;
begin
  //a zona ja' tem um valor fora da faixa de bit; virar bit agora nao pode
  zona:=NewZoneWith(ztEqual, 100);

  try
    zona.ZoneType:=ztBit;
    Fail('virar bit com Value1 igual a 100 tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('o tipo continua o anterior', Ord(ztEqual), Ord(zona.ZoneType));
end;

procedure TTestZones.AnEqualZoneTakesTheExactValue;
var
  zona:TAnimationZone;
begin
  zona:=NewZoneWith(ztEqual, 10);

  AssertEquals('bate em 10',       0, ChosenFor(10));
  AssertEquals('nao bate em 9.99', -1, ChosenFor(9.99));
  AssertEquals('nao bate em 11',   -1, ChosenFor(11));
end;

procedure TTestZones.ANotEqualZoneTakesEverythingElse;
var
  zona:TAnimationZone;
begin
  zona:=NewZoneWith(ztNotEqual, 10);

  AssertEquals('nao bate em 10', -1, ChosenFor(10));
  AssertEquals('bate em 9',       0, ChosenFor(9));
  AssertEquals('bate em 11',      0, ChosenFor(11));
end;

procedure TTestZones.ARangeLeavesTheBoundsOutByDefault;
begin
  NewRange(10, 20, false, false);

  AssertEquals('15 esta dentro',   0, ChosenFor(15));
  AssertEquals('10 fica de fora', -1, ChosenFor(10));
  AssertEquals('20 fica de fora', -1, ChosenFor(20));
end;

procedure TTestZones.ARangeTakesTheBoundsWhenAsked;
begin
  NewRange(10, 20, true, true);

  AssertEquals('10 entra',         0, ChosenFor(10));
  AssertEquals('20 entra',         0, ChosenFor(20));
  AssertEquals('9.99 continua fora', -1, ChosenFor(9.99));
  AssertEquals('20.01 continua fora', -1, ChosenFor(20.01));
end;

procedure TTestZones.AnOutOfRangeZoneTakesWhatIsOutside;
var
  zona:TAnimationZone;
begin
  zona:=NewZone(ztOutOfRange);
  zona.Value2:=20;
  zona.Value1:=10;

  AssertEquals('5 esta fora',      0, ChosenFor(5));
  AssertEquals('25 esta fora',     0, ChosenFor(25));
  AssertEquals('15 esta dentro',  -1, ChosenFor(15));
end;

procedure TTestZones.AGreaterThanZoneTakesWhatIsAbove;
var
  zona:TAnimationZone;
begin
  zona:=NewZoneWith(ztGreaterThan, 10);

  AssertEquals('11 passa',            0, ChosenFor(11));
  AssertEquals('10 nao, sem o limite', -1, ChosenFor(10));

  zona.IncludeValue1:=true;
  AssertEquals('10 passa com o limite', 0, ChosenFor(10));
end;

procedure TTestZones.ALessThanZoneTakesWhatIsBelow;
var
  zona:TAnimationZone;
begin
  zona:=NewZoneWith(ztLessThan, 10);

  AssertEquals('9 passa',              0, ChosenFor(9));
  AssertEquals('10 nao, sem o limite', -1, ChosenFor(10));

  zona.IncludeValue1:=true;
  AssertEquals('10 passa com o limite', 0, ChosenFor(10));
end;

procedure TTestZones.ABitZoneTakesTheValueWithThatBitSet;
var
  zona:TAnimationZone;
begin
  zona:=NewZone(ztBit);
  zona.Value1:=3;   //ztBit nao passa pela troca
  zona.IncludeValue1:=true;

  AssertEquals('8 tem o bit 3',         0, ChosenFor(8));
  AssertEquals('9 tambem',              0, ChosenFor(9));
  AssertEquals('7 nao tem o bit 3',    -1, ChosenFor(7));
end;

procedure TTestZones.ABitZoneCanAlsoLookForTheBitClear;
var
  zona:TAnimationZone;
begin
  //IncludeValue1 falso inverte o criterio: casa quando o bit esta desligado
  zona:=NewZone(ztBit);
  zona.Value1:=3;
  zona.IncludeValue1:=false;

  AssertEquals('7 nao tem o bit 3',  0, ChosenFor(7));
  AssertEquals('8 tem',             -1, ChosenFor(8));
end;

procedure TTestZones.TheHighestAllowedBitIsUsable;
var
  zona:TAnimationZone;
begin
  //o setter aceita ate' 31; a selecao tem que dar conta do mesmo 31
  zona:=NewZone(ztBit);
  zona.Value1:=31;
  zona.IncludeValue1:=true;

  AssertEquals('com o bit 31 ligado',  0, ChosenFor(-2147483648));
  AssertEquals('sem o bit 31',        -1, ChosenFor(1));
end;

procedure TTestZones.TheFirstMatchingZoneWins;
var
  a, b:TAnimationZone;
begin
  a:=NewZoneWith(ztGreaterThan, 0);
  b:=NewZoneWith(ztGreaterThan, 5);

  //as duas casam com 10; a primeira da colecao e' que vale
  AssertEquals('a primeira', 0, ChosenFor(10));
end;

procedure TTestZones.WithNoMatchAndNoDefaultNobodyIsChosen;
var
  zona:TAnimationZone;
begin
  zona:=NewZoneWith(ztEqual, 10);

  AssertTrue('ninguem escolhido', FZones.GetZoneFromValue(99)=nil);
end;

procedure TTestZones.TheDefaultZoneAnswersWhenNothingElseMatches;
var
  a, padrao:TAnimationZone;
begin
  a:=NewZoneWith(ztEqual, 10);
  padrao:=NewZoneWith(ztEqual, 99);
  padrao.DefaultZone:=true;

  AssertEquals('caiu na padrao', 1, ChosenFor(5));
end;

procedure TTestZones.AMatchingZoneBeatsTheDefault;
var
  padrao, a:TAnimationZone;
begin
  //a padrao vem primeiro na colecao, e ainda assim quem casa e' que ganha
  padrao:=NewZoneWith(ztEqual, 99);
  padrao.DefaultZone:=true;
  a:=NewZoneWith(ztEqual, 10);

  AssertEquals('quem casou', 1, ChosenFor(10));
end;

procedure TTestZones.TheDefaultZoneIsNotJudgedByItsOwnCriterion;
var
  padrao:TAnimationZone;
begin
  //a zona padrao e' a rede de seguranca; o criterio dela nao e' avaliado
  padrao:=NewZoneWith(ztEqual, 10);
  padrao.DefaultZone:=true;

  AssertEquals('respondeu como padrao', 0, ChosenFor(10));
  AssertEquals('e tambem para outro valor', 0, ChosenFor(5));
end;

procedure TTestZones.OnlyOneZoneCanBeTheDefault;
var
  a, b:TAnimationZone;
begin
  a:=TAnimationZone(FZones.Add);
  b:=TAnimationZone(FZones.Add);

  a.DefaultZone:=true;
  b.DefaultZone:=true;

  AssertFalse('a primeira deixou de ser padrao', a.DefaultZone);
  AssertTrue('a segunda e a padrao', b.DefaultZone);
  AssertSame('e e ela que responde', b, FZones.GetDefaultZone);
end;

procedure TTestZones.GetDefaultZoneFindsIt;
var
  a, padrao:TAnimationZone;
begin
  a:=TAnimationZone(FZones.Add);
  padrao:=TAnimationZone(FZones.Add);
  padrao.DefaultZone:=true;

  AssertSame('achou a padrao', padrao, FZones.GetDefaultZone);
end;

procedure TTestZones.GetDefaultZoneGivesNilWhenThereIsNone;
begin
  FZones.Add;
  FZones.Add;

  AssertTrue('nenhuma padrao', FZones.GetDefaultZone=nil);
end;

procedure TTestZones.AnEmptyCollectionChoosesNobody;
begin
  AssertTrue('colecao vazia', FZones.GetZoneFromValue(10)=nil);
end;

initialization
  RegisterTest(TTestHMICollection);
  RegisterTest(TTestZones);

end.
