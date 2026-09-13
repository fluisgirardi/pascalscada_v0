{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do vinculo objeto/propriedade e do conector booleano:
            TObjectColletionItem e THMIBooleanPropertyConnector.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O conector e' o que liga um tag a uma propriedade de um controle qualquer
  sem escrever uma linha de codigo: escolhe-se o objeto, a propriedade, e uma
  lista de faixas dizendo que valor do tag resulta em verdadeiro. Quem procura
  a propriedade e' o RTTI, em tempo de execucao, e por isso tudo aqui e'
  acordo por nome e por tipo - errar significa escrever num lugar que nao era
  para ser escrito, ou nao escrever em lugar nenhum e a tela mentir.

  Nada disso precisa de tela: o alvo do vinculo e' um TComponent com
  propriedades publicadas, e e' isso que os testes usam.
}
{$ELSE}
{:
  @abstract(Object/property binding and boolean connector tests:
            TObjectColletionItem and THMIBooleanPropertyConnector.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The connector is what ties a tag to a property of any control without
  writing a line of code: you pick the object, the property, and a list of
  ranges saying which tag value comes out true. What looks the property up is
  RTTI, at run time, so everything here is agreement by name and by type -
  getting it wrong means writing somewhere that was not meant to be written,
  or writing nowhere at all and letting the screen lie.

  None of it needs a screen: the binding target is a TComponent with published
  properties, and that is what these tests use.
}
{$ENDIF}
unit ut.propertyconnector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  hmizones, hmiobjectcolletion, hmibooleanpropertyconnector,
  testsupport.faketag;

type

  { TTargetForTest }

  //alvo do vinculo: um componente com propriedades publicadas de varios tipos
  //binding target: a component with published properties of several types
  TTargetForTest = class(TComponent)
  private
    FOn, FLocked:Boolean;
    FLabel:AnsiString;
    FAmount:LongInt;
  published
    property IsOn:Boolean read FOn write FOn;
    property Locked:Boolean read FLocked write FLocked;
    property Caption:AnsiString read FLabel write FLabel;
    property Amount:LongInt read FAmount write FAmount;
  end;

  { TTestPropertyBinding }

  TTestPropertyBinding = class(TTestCase)
  private
    FConnector:THMIBooleanPropertyConnector;
    FTarget:TTargetForTest;
    FItem:TObjectWithBooleanPropetiesColletionItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //escolha do objeto / picking the object
    procedure AnObjectWithABooleanPropertyIsAccepted;
    procedure AnObjectWithoutABooleanPropertyIsRefused;
    procedure TheOwnerOfTheCollectionCannotBeTheTarget;
    procedure TheConnectorItselfHasNoBooleanPropertyToBind;
    procedure ClearingTheTargetAlsoClearsTheProperty;

    //escolha da propriedade / picking the property
    procedure APropertyOfTheRightTypeIsAccepted;
    procedure APropertyOfAnotherTypeIsRefused;
    procedure APropertyThatDoesNotExistIsRefused;
    procedure ThePropertyNameIsCaseInsensitive;
    procedure WithNoTargetObjectNoPropertyIsAccepted;
    procedure AnEmptyPropertyNameClearsIt;

    //aplicacao do resultado / applying the result
    procedure ApplyingTrueWritesTheProperty;
    procedure ApplyingFalseWritesTheProperty;
    procedure InvertResultFlipsWhatIsWritten;
    procedure ApplyingWithNoTargetWritesNowhere;
    procedure ApplyingOnlyTouchesTheChosenProperty;
  end;

  { TTestBooleanConnector }

  TTestBooleanConnector = class(TTestCase)
  private
    FConnector:THMIBooleanPropertyConnector;
    FTarget:TTargetForTest;
    FTag:TFakeNumber;
    procedure BindTo(const aProperty:AnsiString);
    function  NewCondition(v:Double; aResult:Boolean):TBooleanZone;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ANewConnectorHasNoConditionsAndNoObjects;
    procedure TheTagValuePicksTheZoneAndWritesTheProperty;
    procedure AnotherTagValuePicksTheOtherZone;
    procedure WithNoMatchingZoneThePropertyIsLeftAlone;
    procedure TheDefaultZoneAnswersForWhatIsLeft;
    procedure EveryBoundObjectIsWritten;
    procedure WithNoTagNothingIsWritten;
    procedure ADestroyedTagLetsGoOfTheConnector;
  end;

implementation

uses hmitypes, PLCTag;

{ TTestPropertyBinding }

procedure TTestPropertyBinding.SetUp;
begin
  FConnector:=THMIBooleanPropertyConnector.Create(nil);
  FTarget:=TTargetForTest.Create(nil);
  FItem:=FConnector.AffectedObjects.Add;
end;

procedure TTestPropertyBinding.TearDown;
begin
  FreeAndNil(FConnector);
  FreeAndNil(FTarget);
end;

procedure TTestPropertyBinding.AnObjectWithABooleanPropertyIsAccepted;
begin
  FItem.TargetObject:=FTarget;

  AssertSame('o alvo entrou', FTarget, FItem.TargetObject);
end;

procedure TTestPropertyBinding.AnObjectWithoutABooleanPropertyIsRefused;
var
  semBooleano:TComponent;
begin
  //um TComponent puro publica Name e Tag, nenhum booleano
  semBooleano:=TComponent.Create(nil);
  try
    FItem.TargetObject:=semBooleano;

    AssertTrue('nao ha o que vincular', FItem.TargetObject=nil);
  finally
    semBooleano.Free;
  end;
end;

procedure TTestPropertyBinding.TheOwnerOfTheCollectionCannotBeTheTarget;
var
  colecao:TObjectWithBooleanPropetiesColletion;
  item:TObjectWithBooleanPropetiesColletionItem;
begin
  //apontar o item para o proprio dono da colecao daria uma volta sem fim: o
  //dono e' quem reage a mudanca que ele mesmo acabou de causar. O alvo aqui
  //seria aceito por qualquer outro criterio - ele tem a propriedade booleana
  //que o conector procura - e ainda assim tem que ser recusado
  colecao:=TObjectWithBooleanPropetiesColletion.Create(FTarget);
  try
    item:=colecao.Add;
    item.TargetObject:=FTarget;

    AssertTrue('o dono foi recusado', item.TargetObject=nil);
  finally
    colecao.Free;
  end;
end;

procedure TTestPropertyBinding.TheConnectorItselfHasNoBooleanPropertyToBind;
begin
  //o conector publica Conditions, AffectedObjects e PLCTag, nenhum booleano
  FItem.TargetObject:=FConnector;

  AssertTrue('nao ha o que vincular', FItem.TargetObject=nil);
end;

procedure TTestPropertyBinding.ClearingTheTargetAlsoClearsTheProperty;
begin
  FItem.TargetObject:=FTarget;
  FItem.TargetObjectProperty:='IsOn';

  FItem.TargetObject:=nil;

  AssertTrue('sem alvo', FItem.TargetObject=nil);
  AssertEquals('e sem propriedade', '', FItem.TargetObjectProperty);
end;

procedure TTestPropertyBinding.APropertyOfTheRightTypeIsAccepted;
begin
  FItem.TargetObject:=FTarget;

  FItem.TargetObjectProperty:='IsOn';

  AssertEquals('propriedade booleana', 'IsOn', FItem.TargetObjectProperty);
end;

procedure TTestPropertyBinding.APropertyOfAnotherTypeIsRefused;
begin
  //Caption e texto e Amount e inteiro; o conector booleano nao escreve neles
  FItem.TargetObject:=FTarget;

  FItem.TargetObjectProperty:='Caption';
  AssertEquals('texto recusado', '', FItem.TargetObjectProperty);

  FItem.TargetObjectProperty:='Amount';
  AssertEquals('inteiro recusado', '', FItem.TargetObjectProperty);
end;

procedure TTestPropertyBinding.APropertyThatDoesNotExistIsRefused;
begin
  FItem.TargetObject:=FTarget;

  FItem.TargetObjectProperty:='NaoExiste';

  AssertEquals('nome desconhecido', '', FItem.TargetObjectProperty);
end;

procedure TTestPropertyBinding.ThePropertyNameIsCaseInsensitive;
begin
  //no object inspector o nome vem com a caixa que o usuario digitou
  FItem.TargetObject:=FTarget;

  FItem.TargetObjectProperty:='ison';

  AssertEquals('aceito assim mesmo', 'ison', FItem.TargetObjectProperty);
end;

procedure TTestPropertyBinding.WithNoTargetObjectNoPropertyIsAccepted;
begin
  FItem.TargetObjectProperty:='IsOn';

  AssertEquals('sem objeto nao ha propriedade', '', FItem.TargetObjectProperty);
end;

procedure TTestPropertyBinding.AnEmptyPropertyNameClearsIt;
begin
  FItem.TargetObject:=FTarget;
  FItem.TargetObjectProperty:='IsOn';

  FItem.TargetObjectProperty:='';

  AssertEquals('limpou', '', FItem.TargetObjectProperty);
end;

procedure TTestPropertyBinding.ApplyingTrueWritesTheProperty;
begin
  FItem.TargetObject:=FTarget;
  FItem.TargetObjectProperty:='IsOn';

  FItem.ApplyResult(true);

  AssertTrue('a propriedade foi escrita', FTarget.IsOn);
end;

procedure TTestPropertyBinding.ApplyingFalseWritesTheProperty;
begin
  FTarget.IsOn:=true;
  FItem.TargetObject:=FTarget;
  FItem.TargetObjectProperty:='IsOn';

  FItem.ApplyResult(false);

  AssertFalse('a propriedade foi apagada', FTarget.IsOn);
end;

procedure TTestPropertyBinding.InvertResultFlipsWhatIsWritten;
begin
  FItem.TargetObject:=FTarget;
  FItem.TargetObjectProperty:='IsOn';
  FItem.InvertResult:=true;

  FItem.ApplyResult(true);
  AssertFalse('verdadeiro virou falso', FTarget.IsOn);

  FItem.ApplyResult(false);
  AssertTrue('e falso virou verdadeiro', FTarget.IsOn);
end;

procedure TTestPropertyBinding.ApplyingWithNoTargetWritesNowhere;
begin
  //sem alvo nem propriedade escolhidos, aplicar o resultado nao pode estourar
  FItem.ApplyResult(true);
end;

procedure TTestPropertyBinding.ApplyingOnlyTouchesTheChosenProperty;
begin
  FItem.TargetObject:=FTarget;
  FItem.TargetObjectProperty:='IsOn';

  FItem.ApplyResult(true);

  AssertTrue('a escolhida mudou',      FTarget.IsOn);
  AssertFalse('a outra ficou parada',  FTarget.Locked);
end;

{ TTestBooleanConnector }

procedure TTestBooleanConnector.SetUp;
begin
  FConnector:=THMIBooleanPropertyConnector.Create(nil);
  FTarget:=TTargetForTest.Create(nil);
  FTag:=TFakeNumber.Create(nil);
end;

procedure TTestBooleanConnector.TearDown;
begin
  FreeAndNil(FConnector);
  FreeAndNil(FTag);
  FreeAndNil(FTarget);
end;

procedure TTestBooleanConnector.BindTo(const aProperty:AnsiString);
var
  item:TObjectWithBooleanPropetiesColletionItem;
begin
  item:=FConnector.AffectedObjects.Add;
  item.TargetObject:=FTarget;
  item.TargetObjectProperty:=aProperty;
end;

function TTestBooleanConnector.NewCondition(v:Double; aResult:Boolean):TBooleanZone;
begin
  Result:=FConnector.Conditions.Add;
  Result.ZoneType:=ztEqual;
  Result.SetValues(v, v);
  Result.ZoneResult:=aResult;
end;

procedure TTestBooleanConnector.ANewConnectorHasNoConditionsAndNoObjects;
begin
  AssertEquals('sem condicoes', 0, FConnector.Conditions.Count);
  AssertEquals('sem objetos',   0, FConnector.AffectedObjects.Count);
  AssertTrue('sem tag', FConnector.PLCTag=nil);
end;

procedure TTestBooleanConnector.TheTagValuePicksTheZoneAndWritesTheProperty;
begin
  NewCondition(1, true);
  BindTo('IsOn');
  FConnector.PLCTag:=FTag;

  FTag.ChegouDoCLP(1);

  AssertTrue('a zona do 1 resulta verdadeiro', FTarget.IsOn);
end;

procedure TTestBooleanConnector.AnotherTagValuePicksTheOtherZone;
begin
  NewCondition(1, true);
  NewCondition(2, false);
  BindTo('IsOn');
  FConnector.PLCTag:=FTag;

  FTag.ChegouDoCLP(1);
  AssertTrue('valor 1', FTarget.IsOn);

  FTag.ChegouDoCLP(2);
  AssertFalse('valor 2', FTarget.IsOn);
end;

procedure TTestBooleanConnector.WithNoMatchingZoneThePropertyIsLeftAlone;
begin
  //nenhuma zona casa e nao ha zona padrao: o que estava fica
  NewCondition(1, true);
  BindTo('IsOn');
  FConnector.PLCTag:=FTag;

  FTag.ChegouDoCLP(1);
  AssertTrue('ficou verdadeiro', FTarget.IsOn);

  FTag.ChegouDoCLP(99);
  AssertTrue('e continua, porque nenhuma zona respondeu', FTarget.IsOn);
end;

procedure TTestBooleanConnector.TheDefaultZoneAnswersForWhatIsLeft;
var
  padrao:TBooleanZone;
begin
  NewCondition(1, true);
  padrao:=NewCondition(0, false);
  padrao.DefaultZone:=true;
  BindTo('IsOn');
  FConnector.PLCTag:=FTag;

  FTag.ChegouDoCLP(1);
  AssertTrue('a zona do 1', FTarget.IsOn);

  FTag.ChegouDoCLP(99);
  AssertFalse('caiu na padrao', FTarget.IsOn);
end;

procedure TTestBooleanConnector.EveryBoundObjectIsWritten;
var
  outro:TTargetForTest;
  item:TObjectWithBooleanPropetiesColletionItem;
begin
  outro:=TTargetForTest.Create(nil);
  try
    NewCondition(1, true);
    BindTo('IsOn');

    item:=FConnector.AffectedObjects.Add;
    item.TargetObject:=outro;
    item.TargetObjectProperty:='Locked';

    FConnector.PLCTag:=FTag;
    FTag.ChegouDoCLP(1);

    AssertTrue('o primeiro alvo', FTarget.IsOn);
    AssertTrue('e o segundo',     outro.Locked);
  finally
    outro.Free;
  end;
end;

procedure TTestBooleanConnector.WithNoTagNothingIsWritten;
begin
  NewCondition(1, true);
  BindTo('IsOn');

  //sem tag ligado nao ha valor para consultar
  AssertFalse('nada foi escrito', FTarget.IsOn);
end;

procedure TTestBooleanConnector.ADestroyedTagLetsGoOfTheConnector;
begin
  NewCondition(1, true);
  BindTo('IsOn');
  FConnector.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('o conector largou o tag', FConnector.PLCTag=nil);
end;

initialization
  RegisterTest(TTestPropertyBinding);
  RegisterTest(TTestBooleanConnector);

end.
