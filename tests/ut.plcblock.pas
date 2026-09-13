{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TPLCBlock e do TPLCBlockElement.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um bloco e' um tag de varios valores lidos de uma vez, e o elemento e' uma
  janela de um valor so' dentro dele. E' o mesmo arranjo pai-filho do tag de
  bits, com uma diferenca: aqui o filho nao recorta, ele indexa.

  O bloco recebe os valores da varredura por TagCommandCallBack, que e'
  protegido - uma sonda o expoe, e com isso os testes nao precisam de driver
  nenhum, so' entregam os valores como um driver entregaria.
}
{$ELSE}
{:
  @abstract(TPLCBlock and TPLCBlockElement tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A block is a tag of several values read in one go, and an element is a one
  value window into it. It is the same parent-child arrangement as the bit
  tag, with one difference: here the child does not slice, it indexes.

  The block takes scanned values through TagCommandCallBack, which is
  protected - a probe exposes it, and with that the tests need no driver at
  all, they just hand over the values a driver would.
}
{$ENDIF}
unit ut.plcblock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, PLCBlock, PLCBlockElement;

type

  { TBlocoProbe }

  TBlocoProbe = class(TPLCBlock)
  public
    //: entrega valores ao bloco como uma varredura faria
    procedure CameFromTheScan(const aValores:TArrayOfDouble; aDeslocamento:LongInt;
                                aResultado:TProtocolIOResult = ioOk);
  end;

  { TTestPLCBlock }

  TTestPLCBlock = class(TTestCase)
  private
    FBloco:TBlocoProbe;
    FAvisos:LongInt;
    procedure CountNotification(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //tamanho / size
    procedure TheSizeChangesHowManyValuesThereAre;
    procedure ASizeOfZeroIsIgnored;
    procedure NewValuesStartAtZero;

    //acesso aos valores / value access
    procedure ANegativeIndexIsRefused;
    procedure AnIndexPastTheEndIsRefused;

    //valores vindos da varredura / values coming from a scan
    procedure TheScanKeepsTheValues;
    procedure AScanWithAnOffsetStoresInTheRightPlace;
    procedure MoreValuesThanFitDoNotOverflow;
    procedure AFailedReadDoesNotChangeTheValues;

    //avisos / notifications
    procedure AValueChangeNotifiesTheListener;
    procedure AReadWithTheSameValuesDoesNotNotify;

    //os comandos sem driver ligado / the commands with no driver attached
    procedure AScanReadWithNoDriverHasNoIdentifier;
    procedure AReadWithNoDriverDoesNothing;
    procedure AWriteWithNoDriverComesBackThroughTheCallback;
    procedure WritingZeroValuesDoesNothing;
  end;

  { TTestPLCBlockElement }

  TTestPLCBlockElement = class(TTestCase)
  private
    FBloco:TBlocoProbe;
    FElemento:TPLCBlockElement;
    FAvisos:LongInt;
    procedure CountNotification(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheElementReadsTheValueFromTheBlock;
    procedure TheElementFollowsTheBlockChange;
    procedure WritingToTheElementChangesTheBlock;
    procedure AnIndexPastTheBlockIsRefused;
    procedure AnIndexChosenBeforeTheBlockIsCheckedLater;
    procedure ADestroyedBlockBreaksTheLink;
    procedure WithNoBlockItKeepsTheValueLocally;
  end;

implementation

//: monta o vetor que um driver entregaria
function ValuesOf(const aValores:array of Double):TArrayOfDouble;
var
  c:LongInt;
begin
  Result:=nil;
  SetLength(Result, Length(aValores));
  for c:=0 to High(aValores) do
    Result[c]:=aValores[c];
end;

{ TBlocoProbe }

procedure TBlocoProbe.CameFromTheScan(const aValores:TArrayOfDouble; aDeslocamento:LongInt;
                                        aResultado:TProtocolIOResult = ioOk);
begin
  TagCommandCallBack(0, aValores, GetTickCount64, tcScanRead, aResultado, aDeslocamento);
end;

{ TTestPLCBlock }

procedure TTestPLCBlock.SetUp;
begin
  FBloco:=TBlocoProbe.Create(nil);
  FBloco.Size:=4;
  FAvisos:=0;
end;

procedure TTestPLCBlock.TearDown;
begin
  FreeAndNil(FBloco);
end;

procedure TTestPLCBlock.CountNotification(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestPLCBlock.TheSizeChangesHowManyValuesThereAre;
begin
  AssertEquals('initial size', 4, FBloco.Size);

  FBloco.Size:=7;
  AssertEquals('new size',    7, FBloco.Size);
  AssertEquals('values',         7, Length(FBloco.ValuesRaw));
end;

procedure TTestPLCBlock.ASizeOfZeroIsIgnored;
begin
  //um bloco de tamanho zero nao le nada: o tamanho anterior fica de pe
  FBloco.Size:=0;
  AssertEquals('the size is still', 4, FBloco.Size);
end;

procedure TTestPLCBlock.NewValuesStartAtZero;
begin
  FBloco.Size:=6;
  AssertEquals('new value', 0, FBloco.ValueRaw[5], 0);
end;

procedure TTestPLCBlock.ANegativeIndexIsRefused;
var
  recusou:Boolean;
  lixo:Double;
begin
  recusou:=false;
  try
    lixo:=FBloco.ValueRaw[-1];
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('negative index', recusou);
end;

procedure TTestPLCBlock.AnIndexPastTheEndIsRefused;
var
  recusou:Boolean;
  lixo:Double;
begin
  recusou:=false;
  try
    lixo:=FBloco.ValueRaw[4];
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('index past the end', recusou);
end;

procedure TTestPLCBlock.TheScanKeepsTheValues;
begin
  FBloco.CameFromTheScan(ValuesOf([10, 20, 30, 40]), 0);

  AssertEquals('first', 10, FBloco.ValueRaw[0], 0);
  AssertEquals('second',  20, FBloco.ValueRaw[1], 0);
  AssertEquals('third', 30, FBloco.ValueRaw[2], 0);
  AssertEquals('fourth',   40, FBloco.ValueRaw[3], 0);
end;

procedure TTestPLCBlock.AScanWithAnOffsetStoresInTheRightPlace;
begin
  //um driver pode entregar so' um pedaco do bloco, dizendo de onde ele comeca
  FBloco.CameFromTheScan(ValuesOf([77, 88]), 2);

  AssertEquals('did not touch the beginning', 0,  FBloco.ValueRaw[0], 0);
  AssertEquals('third',            77, FBloco.ValueRaw[2], 0);
  AssertEquals('fourth',              88, FBloco.ValueRaw[3], 0);
end;

procedure TTestPLCBlock.MoreValuesThanFitDoNotOverflow;
begin
  //o driver mandou mais do que o bloco comporta: o que cabe entra, o resto
  //nao pode escrever fora do vetor
  FBloco.CameFromTheScan(ValuesOf([1, 2, 3, 4, 5, 6]), 0);

  AssertEquals('what fitted', 4, FBloco.ValueRaw[3], 0);
  AssertEquals('size untouched', 4, FBloco.Size);
end;

procedure TTestPLCBlock.AFailedReadDoesNotChangeTheValues;
begin
  FBloco.CameFromTheScan(ValuesOf([10, 20, 30, 40]), 0);
  FBloco.CameFromTheScan(ValuesOf([99, 99, 99, 99]), 0, ioTimeOut);

  AssertEquals('the old value stays', 10, FBloco.ValueRaw[0], 0);
end;

procedure TTestPLCBlock.AValueChangeNotifiesTheListener;
begin
  FBloco.AddTagChangeHandler(@CountNotification);
  FBloco.CameFromTheScan(ValuesOf([10, 20, 30, 40]), 0);

  AssertTrue('a change must notify', FAvisos>0);
end;

procedure TTestPLCBlock.AReadWithTheSameValuesDoesNotNotify;
begin
  FBloco.CameFromTheScan(ValuesOf([10, 20, 30, 40]), 0);
  FBloco.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  FBloco.CameFromTheScan(ValuesOf([10, 20, 30, 40]), 0);
  AssertEquals('nothing changed, nothing to notify', 0, FAvisos);
end;

{ TTestPLCBlockElement }

procedure TTestPLCBlock.AScanReadWithNoDriverHasNoIdentifier;
begin
  //sem driver nao ha pedido a numerar: menos um e' o "nao fiz nada" que os
  //tags conferem
  AssertEquals('no driver', -1, FBloco.ScanRead);
end;

procedure TTestPLCBlock.AReadWithNoDriverDoesNothing;
begin
  //nao ha o que entregar, e o bloco nao pode inventar valor nenhum
  FBloco.CameFromTheScan(ValuesOf([10, 20, 30, 40]), 0);

  FBloco.Read;

  AssertEquals('the values stay as they were', 10, FBloco.ValueRaw[0], 0);
end;

procedure TTestPLCBlock.AWriteWithNoDriverComesBackThroughTheCallback;
begin
  //sem driver a escrita volta pelo proprio callback, com ioNullDriver, e o
  //bloco guarda o que foi escrito
  FBloco.Write(ValuesOf([7, 8]), 2, 0);

  AssertEquals('first', 7, FBloco.ValueRaw[0], 0);
  AssertEquals('second',  8, FBloco.ValueRaw[1], 0);
end;

procedure TTestPLCBlock.WritingZeroValuesDoesNothing;
begin
  FBloco.CameFromTheScan(ValuesOf([10, 20, 30, 40]), 0);

  FBloco.Write(ValuesOf([99]), 0, 0);
  AssertEquals('nothing was written', 10, FBloco.ValueRaw[0], 0);

  AssertEquals('and neither was the scan', -1, FBloco.ScanWrite(ValuesOf([99]), 0, 0));
end;

procedure TTestPLCBlockElement.SetUp;
begin
  FBloco:=TBlocoProbe.Create(nil);
  FBloco.Size:=4;

  FElemento:=TPLCBlockElement.Create(nil);
  FAvisos:=0;
end;

procedure TTestPLCBlockElement.TearDown;
begin
  FreeAndNil(FElemento);
  FreeAndNil(FBloco);
end;

procedure TTestPLCBlockElement.CountNotification(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestPLCBlockElement.TheElementReadsTheValueFromTheBlock;
begin
  FBloco.CameFromTheScan(nil, 0);
  FElemento.PLCBlock:=FBloco;
  FElemento.Index   :=2;

  FBloco.ValueRaw[2]:=55;
  AssertEquals('the element reads from the block', 55, FElemento.Value, 0);
end;

procedure TTestPLCBlockElement.TheElementFollowsTheBlockChange;
begin
  FElemento.PLCBlock:=FBloco;
  FElemento.Index   :=1;
  FElemento.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  FBloco.CameFromTheScan(ValuesOf([0, 42, 0, 0]), 0);

  AssertEquals('new value',        42, FElemento.Value, 0);
  AssertTrue  ('and reported the change', FAvisos>0);
end;

procedure TTestPLCBlockElement.WritingToTheElementChangesTheBlock;
begin
  FElemento.PLCBlock:=FBloco;
  FElemento.Index   :=3;

  FElemento.Value:=17;
  AssertEquals('the block got', 17, FBloco.ValueRaw[3], 0);
end;

procedure TTestPLCBlockElement.AnIndexPastTheBlockIsRefused;
var
  recusou:Boolean;
begin
  FElemento.PLCBlock:=FBloco;

  recusou:=false;
  try
    FElemento.Index:=4;
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('index past the block', recusou);
end;

procedure TTestPLCBlockElement.AnIndexChosenBeforeTheBlockIsCheckedLater;
begin
  //sem bloco qualquer indice e' aceito; ao vincular um bloco menor que ele, o
  //elemento fica apontando para fora
  FElemento.Index   :=9;
  FElemento.PLCBlock:=FBloco;

  AssertTrue('the index must fit in the block', FElemento.Index<FBloco.Size);
end;

procedure TTestPLCBlockElement.ADestroyedBlockBreaksTheLink;
var
  bloco:TBlocoProbe;
begin
  bloco:=TBlocoProbe.Create(nil);
  bloco.Size:=2;
  FElemento.PLCBlock:=bloco;
  FElemento.Index   :=1;

  FreeAndNil(bloco);

  AssertTrue('the link must have been broken', FElemento.PLCBlock=nil);
end;

procedure TTestPLCBlockElement.WithNoBlockItKeepsTheValueLocally;
begin
  FElemento.Value:=8;
  AssertEquals('value kept', 8, FElemento.Value, 0);
end;

initialization
  RegisterTest(TTestPLCBlock);
  RegisterTest(TTestPLCBlockElement);

end.
