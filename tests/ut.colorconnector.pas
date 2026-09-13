{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do conector de cor: THMIColorPropertyConnector.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Mesmo desenho do conector booleano - faixas de valor levando a um resultado,
  aplicado por RTTI numa propriedade de outro objeto - com duas diferencas que
  valem teste proprio: o resultado e' uma TColor, e cada item da lista pode ter
  o seu proprio tag. Quando tem, e' esse tag que manda nele, e a varredura do
  conector passa por cima sem tocar - as faixas continuam sendo as do conector.

  E' assim que uma tela mostra dez motores pintados pela mesma regra, cada um
  lendo o seu proprio estado.
}
{$ELSE}
{:
  @abstract(Colour connector tests: THMIColorPropertyConnector.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Same design as the boolean connector - value ranges leading to a result,
  applied through RTTI onto another object's property - with two differences
  worth their own tests: the result is a TColor, and every item in the list may
  carry its own tag. When it does, that tag is what drives it and the
  connector's own sweep steps over it without touching - the ranges are still
  the connector's.

  That is how one screen paints ten motors by the same rule, each reading its
  own state.
}
{$ENDIF}
unit ut.colorconnector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testregistry,
  hmizones, hmiobjectcolletion, hmicolorpropertyconnector,
  testsupport.faketag;

type

  { TColorTargetForTest }

  TColorTargetForTest = class(TComponent)
  private
    FBack, FFore:TColor;
    FAmount:LongInt;
    FOn:Boolean;
  published
    property Background:TColor read FBack write FBack;
    property Foreground:TColor read FFore write FFore;
    property Amount:LongInt read FAmount write FAmount;
    property IsOn:Boolean read FOn write FOn;
  end;

  { TTestColorBinding }

  TTestColorBinding = class(TTestCase)
  private
    FConnector:THMIColorPropertyConnector;
    FTarget:TColorTargetForTest;
    FItem:TObjectWithColorPropetiesColletionItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AnObjectWithAColorPropertyIsAccepted;
    procedure AColorPropertyIsAccepted;
    procedure APlainIntegerPropertyIsRefused;
    procedure ABooleanPropertyIsRefused;
    procedure APropertyThatDoesNotExistIsRefused;
    procedure ApplyingAColourWritesTheProperty;
    procedure ApplyingOnlyTouchesTheChosenProperty;
    procedure ApplyingWithNoTargetWritesNowhere;
  end;

  { TTestColorConnector }

  TTestColorConnector = class(TTestCase)
  private
    FConnector:THMIColorPropertyConnector;
    FTarget:TColorTargetForTest;
    FTag:TFakeNumber;
    function  BindTo(aTarget:TColorTargetForTest; const aProperty:AnsiString):TObjectWithColorPropetiesColletionItem;
    function  NewCondition(v:Double; aColour:TColor):TColorZone;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ANewConnectorHasNoConditionsAndNoObjects;
    procedure TheTagValuePicksTheZoneAndPaintsTheProperty;
    procedure AnotherTagValuePicksTheOtherColour;
    procedure TheDefaultZoneAnswersForWhatIsLeft;
    procedure WithNoMatchingZoneThePropertyIsLeftAlone;
    procedure EveryBoundObjectIsPainted;
    procedure ADestroyedTagLetsGoOfTheConnector;

    //tag por item / per item tag
    procedure AnItemWithItsOwnTagIsPaintedByIt;
    procedure AnItemWithItsOwnTagIsSkippedByTheConnectorSweep;
    procedure TheItemWithItsOwnTagStillUsesTheSharedConditions;
    procedure ADestroyedItemTagLetsGoOfTheItem;
  end;

implementation

{ TTestColorBinding }

procedure TTestColorBinding.SetUp;
begin
  FConnector:=THMIColorPropertyConnector.Create(nil);
  FTarget:=TColorTargetForTest.Create(nil);
  FItem:=FConnector.AffectedObjects.Add;
end;

procedure TTestColorBinding.TearDown;
begin
  FreeAndNil(FConnector);
  FreeAndNil(FTarget);
end;

procedure TTestColorBinding.AnObjectWithAColorPropertyIsAccepted;
begin
  FItem.TargetObject:=FTarget;

  AssertSame('o alvo entrou', FTarget, FItem.TargetObject);
end;

procedure TTestColorBinding.AColorPropertyIsAccepted;
begin
  FItem.TargetObject:=FTarget;

  FItem.TargetObjectProperty:='Background';

  AssertEquals('propriedade de cor', 'Background', FItem.TargetObjectProperty);
end;

procedure TTestColorBinding.APlainIntegerPropertyIsRefused;
begin
  //TColor e um LongInt com nome proprio; o conector confere o nome do tipo,
  //nao o tamanho, entao um inteiro qualquer nao serve
  FItem.TargetObject:=FTarget;

  FItem.TargetObjectProperty:='Amount';

  AssertEquals('inteiro sem nome de cor', '', FItem.TargetObjectProperty);
end;

procedure TTestColorBinding.ABooleanPropertyIsRefused;
begin
  FItem.TargetObject:=FTarget;

  FItem.TargetObjectProperty:='IsOn';

  AssertEquals('booleano recusado', '', FItem.TargetObjectProperty);
end;

procedure TTestColorBinding.APropertyThatDoesNotExistIsRefused;
begin
  FItem.TargetObject:=FTarget;

  FItem.TargetObjectProperty:='NaoExiste';

  AssertEquals('nome desconhecido', '', FItem.TargetObjectProperty);
end;

procedure TTestColorBinding.ApplyingAColourWritesTheProperty;
begin
  FItem.TargetObject:=FTarget;
  FItem.TargetObjectProperty:='Background';

  FItem.ApplyResult(clRed);

  AssertEquals('a cor foi escrita', clRed, FTarget.Background);
end;

procedure TTestColorBinding.ApplyingOnlyTouchesTheChosenProperty;
begin
  FTarget.Foreground:=clBlack;
  FItem.TargetObject:=FTarget;
  FItem.TargetObjectProperty:='Background';

  FItem.ApplyResult(clRed);

  AssertEquals('a escolhida mudou',    clRed,   FTarget.Background);
  AssertEquals('a outra ficou parada', clBlack, FTarget.Foreground);
end;

procedure TTestColorBinding.ApplyingWithNoTargetWritesNowhere;
begin
  //sem alvo nem propriedade, aplicar a cor nao pode estourar
  FItem.ApplyResult(clRed);
end;

{ TTestColorConnector }

procedure TTestColorConnector.SetUp;
begin
  FConnector:=THMIColorPropertyConnector.Create(nil);
  FTarget:=TColorTargetForTest.Create(nil);
  FTag:=TFakeNumber.Create(nil);
end;

procedure TTestColorConnector.TearDown;
begin
  FreeAndNil(FConnector);
  FreeAndNil(FTag);
  FreeAndNil(FTarget);
end;

function TTestColorConnector.BindTo(aTarget:TColorTargetForTest;
  const aProperty:AnsiString):TObjectWithColorPropetiesColletionItem;
begin
  Result:=FConnector.AffectedObjects.Add;
  Result.TargetObject:=aTarget;
  Result.TargetObjectProperty:=aProperty;
end;

function TTestColorConnector.NewCondition(v:Double; aColour:TColor):TColorZone;
begin
  Result:=FConnector.Conditions.Add;
  Result.ZoneType:=ztEqual;
  Result.SetValues(v, v);
  Result.ZoneResult:=aColour;
end;

procedure TTestColorConnector.ANewConnectorHasNoConditionsAndNoObjects;
begin
  AssertEquals('sem condicoes', 0, FConnector.Conditions.Count);
  AssertEquals('sem objetos',   0, FConnector.AffectedObjects.Count);
  AssertTrue('sem tag', FConnector.PLCTag=nil);
end;

procedure TTestColorConnector.TheTagValuePicksTheZoneAndPaintsTheProperty;
begin
  NewCondition(1, clRed);
  BindTo(FTarget, 'Background');
  FConnector.PLCTag:=FTag;

  FTag.ChegouDoCLP(1);

  AssertEquals('pintou de vermelho', clRed, FTarget.Background);
end;

procedure TTestColorConnector.AnotherTagValuePicksTheOtherColour;
begin
  NewCondition(1, clRed);
  NewCondition(2, clLime);
  BindTo(FTarget, 'Background');
  FConnector.PLCTag:=FTag;

  FTag.ChegouDoCLP(1);
  AssertEquals('valor 1', clRed, FTarget.Background);

  FTag.ChegouDoCLP(2);
  AssertEquals('valor 2', clLime, FTarget.Background);
end;

procedure TTestColorConnector.TheDefaultZoneAnswersForWhatIsLeft;
var
  padrao:TColorZone;
begin
  NewCondition(1, clRed);
  padrao:=NewCondition(0, clGray);
  padrao.DefaultZone:=true;
  BindTo(FTarget, 'Background');
  FConnector.PLCTag:=FTag;

  FTag.ChegouDoCLP(1);
  AssertEquals('a zona do 1', clRed, FTarget.Background);

  FTag.ChegouDoCLP(99);
  AssertEquals('caiu na padrao', clGray, FTarget.Background);
end;

procedure TTestColorConnector.WithNoMatchingZoneThePropertyIsLeftAlone;
begin
  NewCondition(1, clRed);
  BindTo(FTarget, 'Background');
  FConnector.PLCTag:=FTag;

  FTag.ChegouDoCLP(1);
  FTag.ChegouDoCLP(99);

  AssertEquals('a cor anterior ficou', clRed, FTarget.Background);
end;

procedure TTestColorConnector.EveryBoundObjectIsPainted;
var
  outro:TColorTargetForTest;
begin
  outro:=TColorTargetForTest.Create(nil);
  try
    NewCondition(1, clRed);
    BindTo(FTarget, 'Background');
    BindTo(outro, 'Foreground');
    FConnector.PLCTag:=FTag;

    FTag.ChegouDoCLP(1);

    AssertEquals('o primeiro alvo', clRed, FTarget.Background);
    AssertEquals('e o segundo',     clRed, outro.Foreground);
  finally
    outro.Free;
  end;
end;

procedure TTestColorConnector.ADestroyedTagLetsGoOfTheConnector;
begin
  NewCondition(1, clRed);
  BindTo(FTarget, 'Background');
  FConnector.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('o conector largou o tag', FConnector.PLCTag=nil);
end;

procedure TTestColorConnector.AnItemWithItsOwnTagIsPaintedByIt;
var
  item:TObjectWithColorPropetiesColletionItem;
  tagDoItem:TFakeNumber;
begin
  tagDoItem:=TFakeNumber.Create(nil);
  try
    NewCondition(1, clRed);
    item:=BindTo(FTarget, 'Background');
    item.PLCTag:=tagDoItem;

    tagDoItem.ChegouDoCLP(1);

    AssertEquals('o tag do item pintou', clRed, FTarget.Background);
  finally
    tagDoItem.Free;
  end;
end;

procedure TTestColorConnector.AnItemWithItsOwnTagIsSkippedByTheConnectorSweep;
var
  item:TObjectWithColorPropetiesColletionItem;
  tagDoItem:TFakeNumber;
begin
  //o item ja tem quem mande nele; o tag do conector nao pode atropelar
  tagDoItem:=TFakeNumber.Create(nil);
  try
    NewCondition(1, clRed);
    NewCondition(2, clLime);
    item:=BindTo(FTarget, 'Background');
    item.PLCTag:=tagDoItem;
    FConnector.PLCTag:=FTag;

    tagDoItem.ChegouDoCLP(1);
    AssertEquals('pintado pelo tag do item', clRed, FTarget.Background);

    FTag.ChegouDoCLP(2);
    AssertEquals('o tag do conector nao mexeu', clRed, FTarget.Background);
  finally
    tagDoItem.Free;
  end;
end;

procedure TTestColorConnector.TheItemWithItsOwnTagStillUsesTheSharedConditions;
var
  item:TObjectWithColorPropetiesColletionItem;
  tagDoItem:TFakeNumber;
begin
  //as faixas sao uma so por conector, mesmo quando cada item le o seu tag
  tagDoItem:=TFakeNumber.Create(nil);
  try
    NewCondition(1, clRed);
    NewCondition(2, clLime);
    item:=BindTo(FTarget, 'Background');
    item.PLCTag:=tagDoItem;

    tagDoItem.ChegouDoCLP(2);

    AssertEquals('usou a faixa do conector', clLime, FTarget.Background);
  finally
    tagDoItem.Free;
  end;
end;

procedure TTestColorConnector.ADestroyedItemTagLetsGoOfTheItem;
var
  item:TObjectWithColorPropetiesColletionItem;
  tagDoItem:TFakeNumber;
begin
  NewCondition(1, clRed);
  item:=BindTo(FTarget, 'Background');
  tagDoItem:=TFakeNumber.Create(nil);
  item.PLCTag:=tagDoItem;

  FreeAndNil(tagDoItem);

  AssertTrue('o item largou o tag', item.PLCTag=nil);
end;

initialization
  RegisterTest(TTestColorBinding);
  RegisterTest(TTestColorConnector);

end.
