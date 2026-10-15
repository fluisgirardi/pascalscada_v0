{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes de regressao para valores NaN vindos do CLP.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  NaN<>NaN e' sempre verdadeiro. Todo codigo que decide "o valor mudou?" com
  um simples <> notifica a cada leitura quando o CLP entrega NaN, e quem relia
  o valor no aviso (THMILabel) entra em laco infinito, congelando o programa.

  Estes testes cobrem todos os lugares que fazem essa comparacao: TPLCTagNumber,
  TPLCBlock, TPLCBlockElement e TPLCStructItem. Para cada um vale o mesmo
  contrato: NaN repetido nao notifica; ir de numero para NaN (e vice-versa)
  notifica; e reler o valor dentro do aviso nao pode realimentar o aviso.
}
{$ELSE}
{:
  @abstract(Regression tests for NaN values coming from the PLC.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  NaN<>NaN is always true. Any code that decides "did the value change?" with a
  plain <> notifies on every read when the PLC delivers NaN, and whoever reads
  the value back on the notification (THMILabel) loops forever, freezing the
  program.

  These tests cover every place that makes that comparison: TPLCTagNumber,
  TPLCBlock, TPLCBlockElement and TPLCStructItem. The same contract holds for
  each: a repeated NaN does not notify; going from number to NaN (and back)
  does; and reading the value back inside the notification must not feed the
  notification again.
}
{$ENDIF}
unit ut.nanvalues;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, fpcunit, testregistry,
  Tag, ProtocolTypes, PLCTagNumber, PLCBlock, PLCBlockElement,
  PLCStruct, PLCStructElement;

type

  { TNanTagProbe }

  TNanTagProbe = class(TPLCTagNumber)
  public
    procedure CameFromTheScan(aValor:Double);
  end;

  { TNanBlockProbe }

  TNanBlockProbe = class(TPLCBlock)
  public
    procedure CameFromTheScan(const aValores:array of Double);
  end;

  { TNanStructProbe }

  TNanStructProbe = class(TPLCStruct)
  public
    procedure CameFromTheScan(const aBytes:array of Byte);
  end;

  { TTestNanValues }

  TTestNanValues = class(TTestCase)
  private
    FTag:TNanTagProbe;
    FBloco:TNanBlockProbe;
    FElemento:TPLCBlockElement;
    FEstrutura:TNanStructProbe;
    FItem:TPLCStructItem;
    FAvisos:LongInt;
    FUltimoLido:Double;
    procedure CountNotification(Sender:TObject);
    procedure ReadBackNotification(Sender:TObject);
    procedure ScanTag(aValor:Double);
    procedure ScanBlock(aValor:Double);
    procedure ScanStruct(aNaN:Boolean; aValor:Byte);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //TPLCTagNumber
    procedure TagFirstNanNotifies;
    procedure TagRepeatedNanDoesNotNotify;
    procedure TagNumberToNanNotifies;
    procedure TagNanToNumberNotifies;
    procedure TagNanValueIsReadAsNan;
    procedure TagReadingBackInsideTheNotificationDoesNotLoop;

    //TPLCBlock
    procedure BlockFirstNanNotifies;
    procedure BlockRepeatedNanDoesNotNotify;
    procedure BlockNanInOneSlotOfSeveralDoesNotNotifyAgain;
    procedure BlockNumberToNanNotifies;
    procedure BlockNanToNumberNotifies;

    //TPLCBlockElement
    procedure ElementRepeatedNanDoesNotNotify;
    procedure ElementNumberToNanNotifies;
    procedure ElementNanToNumberNotifies;
    procedure ElementReadingBackInsideTheNotificationDoesNotLoop;

    //TPLCStructItem
    procedure StructItemRepeatedNanDoesNotNotify;
    procedure StructItemNumberToNanNotifies;
    procedure StructItemNanToNumberNotifies;
    procedure StructItemReadingBackInsideTheNotificationDoesNotLoop;
  end;

implementation

const
  //quantas notificacoes seguidas consideramos laco infinito
  //how many notifications in a row we consider an endless loop
  LOOP_LIMIT = 50;

{ TNanTagProbe }

procedure TNanTagProbe.CameFromTheScan(aValor:Double);
var
  valores:TArrayOfDouble;
begin
  valores:=nil;
  SetLength(valores, 1);
  valores[0]:=aValor;
  TagCommandCallBack(0, valores, GetTickCount64, tcScanRead, ioOk, 0);
end;

{ TNanBlockProbe }

procedure TNanBlockProbe.CameFromTheScan(const aValores:array of Double);
var
  valores:TArrayOfDouble;
  c:LongInt;
begin
  valores:=nil;
  SetLength(valores, Length(aValores));
  for c:=0 to High(aValores) do
    valores[c]:=aValores[c];
  TagCommandCallBack(0, valores, GetTickCount64, tcScanRead, ioOk, 0);
end;

{ TNanStructProbe }

procedure TNanStructProbe.CameFromTheScan(const aBytes:array of Byte);
var
  valores:TArrayOfDouble;
  c:LongInt;
begin
  valores:=nil;
  SetLength(valores, Length(aBytes));
  for c:=0 to High(aBytes) do
    valores[c]:=aBytes[c];
  TagCommandCallBack(0, valores, GetTickCount64, tcScanRead, ioOk, 0);
end;

{ TTestNanValues }

procedure TTestNanValues.SetUp;
begin
  FTag      :=TNanTagProbe.Create(nil);

  FBloco    :=TNanBlockProbe.Create(nil);
  FBloco.Size:=4;
  FElemento :=TPLCBlockElement.Create(nil);
  FElemento.PLCBlock:=FBloco;
  FElemento.Index   :=1;

  FEstrutura:=TNanStructProbe.Create(nil);
  FEstrutura.Size:=8;
  FItem     :=TPLCStructItem.Create(nil);
  FItem.PLCBlock:=FEstrutura;
  FItem.TagType :=pttFloat;
  FItem.Index   :=0;

  FAvisos    :=0;
  FUltimoLido:=0;
end;

procedure TTestNanValues.TearDown;
begin
  FreeAndNil(FItem);
  FreeAndNil(FEstrutura);
  FreeAndNil(FElemento);
  FreeAndNil(FBloco);
  FreeAndNil(FTag);
end;

procedure TTestNanValues.CountNotification(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestNanValues.ReadBackNotification(Sender:TObject);
begin
  //o que o THMILabel faz: le o valor de volta dentro do aviso
  inc(FAvisos);
  if FAvisos>LOOP_LIMIT then
    Fail('endless notification loop with a NaN value');
  if Sender is TPLCTagNumber then
    FUltimoLido:=TPLCTagNumber(Sender).Value
  else if Sender is TPLCBlockElement then
    FUltimoLido:=TPLCBlockElement(Sender).Value
  else if Sender is TPLCStructItem then
    FUltimoLido:=TPLCStructItem(Sender).Value;
end;

procedure TTestNanValues.ScanTag(aValor:Double);
begin
  FTag.CameFromTheScan(aValor);
end;

procedure TTestNanValues.ScanBlock(aValor:Double);
begin
  //so' o slot 1 interessa ao elemento / only slot 1 matters to the element
  FBloco.CameFromTheScan([0, aValor, 0, 0]);
end;

procedure TTestNanValues.ScanStruct(aNaN:Boolean; aValor:Byte);
begin
  if aNaN then
    FEstrutura.CameFromTheScan([$00, $00, $C0, $7F, 0, 0, 0, 0])   //NaN simples
  else
    FEstrutura.CameFromTheScan([$00, $00, $80, $3F, 0, 0, 0, aValor]); //1.0
end;

{ TPLCTagNumber }

procedure TTestNanValues.TagFirstNanNotifies;
begin
  ScanTag(42);
  FTag.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanTag(NaN);
  AssertTrue('42 -> NaN is a change', FAvisos>0);
end;

procedure TTestNanValues.TagRepeatedNanDoesNotNotify;
var
  c:LongInt;
begin
  ScanTag(NaN);
  FTag.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  for c:=1 to 10 do ScanTag(NaN);
  AssertEquals('NaN -> NaN is not a change', 0, FAvisos);
end;

procedure TTestNanValues.TagNumberToNanNotifies;
begin
  ScanTag(1);
  FTag.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanTag(NaN);
  AssertEquals('exactly one notification', 1, FAvisos);
end;

procedure TTestNanValues.TagNanToNumberNotifies;
begin
  ScanTag(NaN);
  FTag.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanTag(1);
  AssertEquals('exactly one notification', 1, FAvisos);
end;

procedure TTestNanValues.TagNanValueIsReadAsNan;
begin
  ScanTag(NaN);
  AssertTrue('the tag must hold the NaN', IsNan(FTag.ValueRaw));
end;

procedure TTestNanValues.TagReadingBackInsideTheNotificationDoesNotLoop;
var
  c:LongInt;
begin
  FTag.AddTagChangeHandler(@ReadBackNotification);
  for c:=1 to 10 do ScanTag(NaN);
  AssertTrue('few notifications for ten identical scans', FAvisos<=2);
end;

{ TPLCBlock }

procedure TTestNanValues.BlockFirstNanNotifies;
begin
  ScanBlock(5);
  FBloco.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanBlock(NaN);
  AssertTrue('5 -> NaN is a change', FAvisos>0);
end;

procedure TTestNanValues.BlockRepeatedNanDoesNotNotify;
var
  c:LongInt;
begin
  ScanBlock(NaN);
  FBloco.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  for c:=1 to 10 do ScanBlock(NaN);
  AssertEquals('NaN -> NaN is not a change', 0, FAvisos);
end;

procedure TTestNanValues.BlockNanInOneSlotOfSeveralDoesNotNotifyAgain;
var
  c:LongInt;
begin
  //um NaN entre varios valores normais tambem nao pode ficar notificando
  FBloco.CameFromTheScan([1, NaN, 3, 4]);
  FBloco.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  for c:=1 to 10 do FBloco.CameFromTheScan([1, NaN, 3, 4]);
  AssertEquals('same values, NaN included: nothing to notify', 0, FAvisos);
end;

procedure TTestNanValues.BlockNumberToNanNotifies;
begin
  ScanBlock(1);
  FBloco.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanBlock(NaN);
  AssertEquals('exactly one notification', 1, FAvisos);
end;

procedure TTestNanValues.BlockNanToNumberNotifies;
begin
  ScanBlock(NaN);
  FBloco.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanBlock(1);
  AssertEquals('exactly one notification', 1, FAvisos);
end;

{ TPLCBlockElement }

procedure TTestNanValues.ElementRepeatedNanDoesNotNotify;
var
  c:LongInt;
begin
  ScanBlock(NaN);
  FElemento.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  for c:=1 to 10 do ScanBlock(NaN);
  AssertEquals('NaN -> NaN is not a change', 0, FAvisos);
end;

procedure TTestNanValues.ElementNumberToNanNotifies;
begin
  ScanBlock(1);
  FElemento.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanBlock(NaN);
  AssertEquals('exactly one notification', 1, FAvisos);
end;

procedure TTestNanValues.ElementNanToNumberNotifies;
begin
  ScanBlock(NaN);
  FElemento.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanBlock(1);
  AssertEquals('exactly one notification', 1, FAvisos);
end;

procedure TTestNanValues.ElementReadingBackInsideTheNotificationDoesNotLoop;
var
  c:LongInt;
begin
  FElemento.AddTagChangeHandler(@ReadBackNotification);
  for c:=1 to 10 do ScanBlock(NaN);
  AssertTrue('few notifications for ten identical scans', FAvisos<=2);
end;

{ TPLCStructItem }

procedure TTestNanValues.StructItemRepeatedNanDoesNotNotify;
var
  c:LongInt;
begin
  ScanStruct(True, 0);
  FItem.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  for c:=1 to 10 do ScanStruct(True, 0);
  AssertEquals('NaN -> NaN is not a change', 0, FAvisos);
end;

procedure TTestNanValues.StructItemNumberToNanNotifies;
begin
  ScanStruct(False, 0);
  FItem.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanStruct(True, 0);
  AssertEquals('exactly one notification', 1, FAvisos);
end;

procedure TTestNanValues.StructItemNanToNumberNotifies;
begin
  ScanStruct(True, 0);
  FItem.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  ScanStruct(False, 0);
  AssertEquals('exactly one notification', 1, FAvisos);
end;

procedure TTestNanValues.StructItemReadingBackInsideTheNotificationDoesNotLoop;
var
  c:LongInt;
begin
  FItem.AddTagChangeHandler(@ReadBackNotification);
  for c:=1 to 10 do ScanStruct(True, 0);
  AssertTrue('few notifications for ten identical scans', FAvisos<=2);
end;

initialization
  RegisterTest(TTestNanValues);

end.
