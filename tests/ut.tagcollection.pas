{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da TTagCollection: a lista de tags que um componente
            carrega, e os avisos que ela repassa ao dono.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Uma receita, um historico, qualquer coisa que precise de mais de um tag
  guarda uma colecao destas. Cada item aponta para um tag; a colecao avisa o
  dono de duas coisas: um item mudou (ganhou, trocou ou perdeu o tag) e um
  valor mudou (o tag de algum item mudou de valor, ou falhou ao escrever).

  O item tem que soltar o tag quando o tag morre, e o tag tem que soltar o
  item quando o item morre - senao sobra um ponteiro para alguem que ja' nao
  existe.
}
{$ELSE}
{:
  @abstract(TTagCollection tests: the list of tags a component carries, and
            the notices it passes on to the owner.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A recipe, a historian, anything that needs more than one tag keeps one of
  these collections. Each item points to a tag; the collection tells the owner
  two things: an item changed (it gained, swapped or lost its tag) and a value
  changed (some item's tag changed value, or failed to write).

  The item has to let go of the tag when the tag dies, and the tag has to let
  go of the item when the item dies - or a pointer to someone who no longer
  exists is left behind.
}
{$ENDIF}
unit ut.tagcollection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  tagcollection, PLCTag, PLCBlock, hsstrings, testsupport.faketag;

type

  { TFaultyNumber }

  //um tag que consegue falhar ao escrever, para o aviso de falha ter origem
  //a tag able to fail a write, so the fault notice has a source
  TFaultyNumber = class(TFakeNumber)
  public
    procedure FailAWrite;
  end;

  { TTestTagCollection }

  TTestTagCollection = class(TTestCase)
  private
    FCollection:TTagCollection;
    FTag:TFaultyNumber;
    FItemChanges, FValueChanges:LongInt;
    FLastItem, FLastValueItem:TObject;
    FStateAsked:LongInt;
    procedure ItemChanged(Sender:TObject);
    procedure ValuesChanged(Sender:TObject);
    procedure NeedState(var CurState:TComponentState);
    function  NewItem:TTagCollectionItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o item e o tag / the item and the tag
    procedure ANewItemHasNoTag;
    procedure AttachingATagKeepsIt;
    procedure AttachingATagTellsTheOwnerWhichItemChanged;
    procedure AttachingTheSameTagAgainTellsNothing;
    procedure ClearingTheTagTellsTheOwner;
    procedure ATagWithoutTheTagInterfaceIsRefused;

    //os valores / the values
    procedure ATagValueChangeReachesTheOwnerThroughTheItem;
    procedure AWriteFaultReachesTheOwnerToo;
    procedure EveryItemReportsItsOwnTag;
    procedure AClearedTagNoLongerReports;
    procedure AReplacedTagNoLongerReportsAndTheNewOneDoes;

    //quem morre solta quem / who dies lets go of whom
    procedure ADestroyedTagLetsGoOfTheItem;
    procedure ADestroyedTagTellsTheOwnerTheItemChanged;
    procedure ADestroyedItemLetsGoOfTheTag;
    procedure ADestroyedCollectionLetsGoOfTheTags;

    //o nome / the name
    procedure AnEmptyItemIsCalledEmpty;
    procedure AnItemIsCalledAfterItsTag;

    //o estado do dono / the owner's state
    procedure TheStateIsAskedFromTheOwner;
    procedure WithNoOwnerTheStateIsEmpty;
  end;

implementation

{ TFaultyNumber }

procedure TFaultyNumber.FailAWrite;
begin
  NotifyWriteFault;
end;

{ TTestTagCollection }

procedure TTestTagCollection.SetUp;
begin
  FItemChanges:=0;
  FValueChanges:=0;
  FLastItem:=nil;
  FLastValueItem:=nil;
  FStateAsked:=0;
  FCollection:=TTagCollection.Create(TTagCollectionItem);
  FCollection.OnItemChange:=@ItemChanged;
  FCollection.OnValuesChange:=@ValuesChanged;
  FTag:=TFaultyNumber.Create(nil);
  FTag.Name:='nivel_tanque';
end;

procedure TTestTagCollection.TearDown;
begin
  FreeAndNil(FCollection);
  FreeAndNil(FTag);
end;

procedure TTestTagCollection.ItemChanged(Sender:TObject);
begin
  inc(FItemChanges);
  FLastItem:=Sender;
end;

procedure TTestTagCollection.ValuesChanged(Sender:TObject);
begin
  inc(FValueChanges);
  FLastValueItem:=Sender;
end;

procedure TTestTagCollection.NeedState(var CurState:TComponentState);
begin
  inc(FStateAsked);
  CurState:=[csLoading];
end;

function TTestTagCollection.NewItem:TTagCollectionItem;
begin
  Result:=TTagCollectionItem(FCollection.Add);
end;

procedure TTestTagCollection.ANewItemHasNoTag;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;

  AssertTrue('sem tag', item.PLCTag=nil);
end;

procedure TTestTagCollection.AttachingATagKeepsIt;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;

  item.PLCTag:=FTag;

  AssertTrue('o tag ficou', item.PLCTag=FTag);
end;

procedure TTestTagCollection.AttachingATagTellsTheOwnerWhichItemChanged;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;

  item.PLCTag:=FTag;

  AssertEquals('um aviso',      1, FItemChanges);
  AssertTrue  ('sobre o item',  FLastItem=item);
end;

procedure TTestTagCollection.AttachingTheSameTagAgainTellsNothing;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;
  item.PLCTag:=FTag;
  FItemChanges:=0;

  item.PLCTag:=FTag;

  AssertEquals('nada mudou, nada avisado', 0, FItemChanges);
end;

procedure TTestTagCollection.ClearingTheTagTellsTheOwner;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;
  item.PLCTag:=FTag;
  FItemChanges:=0;

  item.PLCTag:=nil;

  AssertTrue  ('sem tag',   item.PLCTag=nil);
  AssertEquals('um aviso',  1, FItemChanges);
end;

procedure TTestTagCollection.ATagWithoutTheTagInterfaceIsRefused;
var
  item:TTagCollectionItem;
  bloco:TPLCBlock;
begin
  //um bloco nao tem valor unico para reportar: o item nao saberia o que
  //dizer ao dono
  //a block has no single value to report: the item would not know what to
  //tell the owner
  item:=NewItem;
  item.PLCTag:=FTag;
  bloco:=TPLCBlock.Create(nil);
  try
    try
      item.PLCTag:=bloco;
      Fail('um bloco tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e o tag anterior continua', item.PLCTag=FTag);
  finally
    bloco.Free;
  end;
end;

procedure TTestTagCollection.ATagValueChangeReachesTheOwnerThroughTheItem;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;
  item.PLCTag:=FTag;

  FTag.ChegouDoCLP(42);

  AssertEquals('um aviso de valor', 1, FValueChanges);
  AssertTrue  ('vindo do item',     FLastValueItem=item);
end;

procedure TTestTagCollection.AWriteFaultReachesTheOwnerToo;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;
  item.PLCTag:=FTag;

  FTag.FailAWrite;

  AssertEquals('um aviso de valor', 1, FValueChanges);
  AssertTrue  ('vindo do item',     FLastValueItem=item);
end;

procedure TTestTagCollection.EveryItemReportsItsOwnTag;
var
  primeiro, segundo:TTagCollectionItem;
  outro:TFakeNumber;
begin
  outro:=TFakeNumber.Create(nil);
  try
    primeiro:=NewItem;
    primeiro.PLCTag:=FTag;
    segundo:=NewItem;
    segundo.PLCTag:=outro;

    outro.ChegouDoCLP(7);

    AssertEquals('um aviso',           1, FValueChanges);
    AssertTrue  ('do segundo item',    FLastValueItem=segundo);
  finally
    outro.Free;
  end;
end;

procedure TTestTagCollection.AClearedTagNoLongerReports;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;
  item.PLCTag:=FTag;
  item.PLCTag:=nil;

  FTag.ChegouDoCLP(42);

  AssertEquals('nenhum aviso', 0, FValueChanges);
end;

procedure TTestTagCollection.AReplacedTagNoLongerReportsAndTheNewOneDoes;
var
  item:TTagCollectionItem;
  novo:TFakeNumber;
begin
  novo:=TFakeNumber.Create(nil);
  try
    item:=NewItem;
    item.PLCTag:=FTag;

    item.PLCTag:=novo;
    FTag.ChegouDoCLP(1);
    AssertEquals('o antigo ja nao fala pelo item', 0, FValueChanges);

    novo.ChegouDoCLP(2);
    AssertEquals('o novo fala', 1, FValueChanges);
  finally
    novo.Free;
  end;
end;

procedure TTestTagCollection.ADestroyedTagLetsGoOfTheItem;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;
  item.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('o item soltou o tag', item.PLCTag=nil);
end;

procedure TTestTagCollection.ADestroyedTagTellsTheOwnerTheItemChanged;
var
  item:TTagCollectionItem;
begin
  //perder o tag e' uma mudanca no item tanto quanto ganhar um: uma receita
  //que guarda os nomes precisa saber que aquela linha ficou vazia
  //losing the tag is as much a change to the item as gaining one: a recipe
  //keeping the names needs to know that line went empty
  item:=NewItem;
  item.PLCTag:=FTag;
  FItemChanges:=0;

  FreeAndNil(FTag);

  AssertEquals('um aviso',     1, FItemChanges);
  AssertTrue  ('sobre o item', FLastItem=item);
end;

procedure TTestTagCollection.ADestroyedItemLetsGoOfTheTag;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;
  item.PLCTag:=FTag;

  item.Free;
  FTag.ChegouDoCLP(42);

  AssertEquals('ninguem mais escuta', 0, FValueChanges);
end;

procedure TTestTagCollection.ADestroyedCollectionLetsGoOfTheTags;
begin
  NewItem.PLCTag:=FTag;

  FreeAndNil(FCollection);
  FTag.ChegouDoCLP(42);

  AssertEquals('ninguem mais escuta', 0, FValueChanges);
end;

procedure TTestTagCollection.AnEmptyItemIsCalledEmpty;
begin
  AssertEquals(SEmpty, NewItem.DisplayName);
end;

procedure TTestTagCollection.AnItemIsCalledAfterItsTag;
var
  item:TTagCollectionItem;
begin
  item:=NewItem;

  item.PLCTag:=FTag;

  AssertEquals('nivel_tanque', item.DisplayName);
end;

procedure TTestTagCollection.TheStateIsAskedFromTheOwner;
var
  estado:TComponentState;
begin
  FCollection.OnNeedCompState:=@NeedState;

  estado:=FCollection.ZonesState;

  AssertEquals('perguntou ao dono',   1, FStateAsked);
  AssertTrue  ('e trouxe a resposta', csLoading in estado);
end;

procedure TTestTagCollection.WithNoOwnerTheStateIsEmpty;
begin
  AssertTrue('nenhum estado', FCollection.ZonesState=[]);
end;

initialization
  RegisterTest(TTestTagCollection);

end.
