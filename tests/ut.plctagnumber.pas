{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TPLCTagNumber e do contrato de TPLCNumber.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Este e' o tag numerico comum, o que a maioria dos projetos usa. Sem driver
  ligado ele nao fica inutil: escrever nele cai no proprio TagCommandCallBack
  com ioNullDriver, entao o valor fica guardado. E' por esse caminho que estes
  testes exercitam tudo sem precisar de CLP, porta ou driver.

  A parte que vem de TPLCNumber - limites de faixa, escala de engenharia e a
  formatacao do valor em texto - vale para todo tag numerico da biblioteca.
}
{$ELSE}
{:
  @abstract(TPLCTagNumber tests, and the TPLCNumber contract.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This is the ordinary numeric tag, the one most projects use. With no driver
  attached it is not useless: writing to it lands on its own TagCommandCallBack
  with ioNullDriver, so the value is kept. That is the path these tests use to
  exercise everything with no PLC, port or driver.

  What comes from TPLCNumber - range limits, engineering scale and formatting
  the value as text - holds for every numeric tag in the library.
}
{$ENDIF}
unit ut.plctagnumber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, PLCTagNumber, ValueProcessor;

type

  { TEscalaQueDobra }

  //: escala de teste: o valor do CLP vale o dobro para o usuario
  TEscalaQueDobra = class(TScaleProcessor)
  public
    function SetInGetOut(Sender:TComponent; aInput:Double):Double; override;
    function SetOutGetIn(Sender:TComponent; aOutput:Double):Double; override;
  end;

  { TTagProbe }

  TTagProbe = class(TPLCTagNumber)
  public
    //: entrega um valor ao tag como uma varredura faria
    procedure CameFromTheScan(aValor:Double; aResultado:TProtocolIOResult = ioOk);
  end;

  { TTestPLCTagNumber }

  TTestPLCTagNumber = class(TTestCase)
  private
    FTag:TTagProbe;
    FEscala:TEscalaQueDobra;
    FAvisos, FFalhasDeEscrita:LongInt;
    procedure CountNotification(Sender:TObject);
    procedure CountWriteFailure(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //valores vindos da varredura / values coming from a scan
    procedure TheValueFromTheScanIsKept;
    procedure AFailedReadDoesNotChangeTheValue;
    procedure AValueChangeNotifiesTheListener;
    procedure AReadWithTheSameValueDoesNotNotify;

    //escrita sem driver / writing with no driver
    procedure WithNoDriverTheValueWrittenIsKept;

    //limites de faixa / range limits
    procedure WithTheLimitsOffAnyValueGoesThrough;
    procedure AValueBelowTheMinimumIsRefused;
    procedure AValueAboveTheMaximumIsRefused;
    procedure AValueInsideTheRangeIsAccepted;
    procedure ARefusedValueReportsAWriteFailure;
    procedure ARefusedValueNeverChangesTheTag;
    procedure AMinimumAboveTheMaximumIsRefused;

    //escala de engenharia / engineering scale
    procedure TheScaleIsAppliedOnTheRead;
    procedure TheScaleIsUndoneOnTheWrite;
    procedure TheRawValueDoesNotGoThroughTheScale;
    procedure TheLimitsApplyInEngineeringUnits;

    //valor em texto / value as text
    procedure TextWithAPrefixAndASuffix;
    procedure TextWithANumericFormat;
    procedure TextWithNoFormatUsesTheDefault;
    procedure TextWithATimeFormatUsesTheValueInMilliseconds;
  end;

implementation

{ TEscalaQueDobra }

function TEscalaQueDobra.SetInGetOut(Sender:TComponent; aInput:Double):Double;
begin
  //do equipamento para o usuario
  Result:=aInput*2;
end;

function TEscalaQueDobra.SetOutGetIn(Sender:TComponent; aOutput:Double):Double;
begin
  //do usuario para o equipamento
  Result:=aOutput/2;
end;

{ TTagProbe }

procedure TTagProbe.CameFromTheScan(aValor:Double; aResultado:TProtocolIOResult = ioOk);
var
  valores:TArrayOfDouble;
begin
  SetLength(valores, 1);
  valores[0]:=aValor;
  TagCommandCallBack(0, valores, GetTickCount64, tcScanRead, aResultado, 0);
end;

{ TTestPLCTagNumber }

procedure TTestPLCTagNumber.SetUp;
begin
  FTag:=TTagProbe.Create(nil);
  FEscala:=TEscalaQueDobra.Create(nil);
  FAvisos:=0;
  FFalhasDeEscrita:=0;
end;

procedure TTestPLCTagNumber.TearDown;
begin
  FreeAndNil(FTag);
  FreeAndNil(FEscala);
end;

procedure TTestPLCTagNumber.CountNotification(Sender:TObject);
begin
  inc(FAvisos);
end;

procedure TTestPLCTagNumber.CountWriteFailure(Sender:TObject);
begin
  inc(FFalhasDeEscrita);
end;

procedure TTestPLCTagNumber.TheValueFromTheScanIsKept;
begin
  FTag.CameFromTheScan(42);
  AssertEquals('value from the scan', 42, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.AFailedReadDoesNotChangeTheValue;
begin
  FTag.CameFromTheScan(42);
  FTag.CameFromTheScan(99, ioTimeOut);

  AssertEquals('the good value stays', 42, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.AValueChangeNotifiesTheListener;
begin
  FTag.AddTagChangeHandler(@CountNotification);
  FTag.CameFromTheScan(42);

  AssertTrue('a change must notify', FAvisos>0);
end;

procedure TTestPLCTagNumber.AReadWithTheSameValueDoesNotNotify;
begin
  FTag.CameFromTheScan(42);
  FTag.AddTagChangeHandler(@CountNotification);
  FAvisos:=0;

  FTag.CameFromTheScan(42);
  AssertEquals('nothing changed, nothing to notify', 0, FAvisos);
end;

procedure TTestPLCTagNumber.WithNoDriverTheValueWrittenIsKept;
begin
  //sem driver a escrita volta pelo proprio callback, com ioNullDriver
  FTag.Value:=17;
  AssertEquals('value kept', 17, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.WithTheLimitsOffAnyValueGoesThrough;
begin
  FTag.SetMinMaxValues(0, 10);
  FTag.Value:=1000;
  AssertEquals('with the limits off', 1000, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.AValueBelowTheMinimumIsRefused;
var
  recusou:Boolean;
begin
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMinValue:=true;

  recusou:=false;
  try
    FTag.Value:=-1;
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('below the minimum', recusou);
end;

procedure TTestPLCTagNumber.AValueAboveTheMaximumIsRefused;
var
  recusou:Boolean;
begin
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMaxValue:=true;

  recusou:=false;
  try
    FTag.Value:=101;
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('above the maximum', recusou);
end;

procedure TTestPLCTagNumber.AValueInsideTheRangeIsAccepted;
begin
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMinValue:=true;
  FTag.EnableMaxValue:=true;

  FTag.Value:=50;
  AssertEquals('value inside the range', 50, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.ARefusedValueReportsAWriteFailure;
begin
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMaxValue:=true;
  FTag.AddWriteFaultHandler(@CountWriteFailure);

  try
    FTag.Value:=200;
  except
    on E:Exception do ;
  end;

  AssertTrue('the listener must be told about the refusal', FFalhasDeEscrita>0);
end;

procedure TTestPLCTagNumber.ARefusedValueNeverChangesTheTag;
begin
  FTag.CameFromTheScan(10);
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMaxValue:=true;

  try
    FTag.Value:=200;
  except
    on E:Exception do ;
  end;

  AssertEquals('the previous value stands', 10, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.AMinimumAboveTheMaximumIsRefused;
var
  recusou:Boolean;
begin
  recusou:=false;
  try
    FTag.SetMinMaxValues(100, 0);
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('inverted range', recusou);
end;

procedure TTestPLCTagNumber.TheScaleIsAppliedOnTheRead;
begin
  //o equipamento manda 21, o usuario ve 42
  FTag.ScaleProcessor:=FEscala;
  FTag.CameFromTheScan(21);

  AssertEquals('value in engineering units', 42, FTag.Value, 0);
end;

procedure TTestPLCTagNumber.TheScaleIsUndoneOnTheWrite;
begin
  //o usuario escreve 42, o equipamento recebe 21
  FTag.ScaleProcessor:=FEscala;
  FTag.Value:=42;

  AssertEquals('raw value', 21, FTag.ValueRaw, 0);
end;

procedure TTestPLCTagNumber.TheRawValueDoesNotGoThroughTheScale;
begin
  FTag.ScaleProcessor:=FEscala;
  FTag.CameFromTheScan(21);

  AssertEquals('the raw value is what came from the device', 21, FTag.ValueRaw, 0);
end;

procedure TTestPLCTagNumber.TheLimitsApplyInEngineeringUnits;
var
  recusou:Boolean;
begin
  //os limites sao conferidos antes da escala: sao a faixa que o usuario ve
  FTag.ScaleProcessor:=FEscala;
  FTag.SetMinMaxValues(0, 100);
  FTag.EnableMaxValue:=true;

  recusou:=false;
  try
    FTag.Value:=150;
  except
    on E:Exception do recusou:=true;
  end;
  AssertTrue('150 in engineering units is over the limit', recusou);

  //e 100 em engenharia passa, mesmo virando 50 no equipamento
  FTag.Value:=100;
  AssertEquals('at the limit', 50, FTag.ValueRaw, 0);
end;

procedure TTestPLCTagNumber.TextWithAPrefixAndASuffix;
begin
  FTag.CameFromTheScan(25);
  AssertEquals('with a prefix and a suffix', 'T= 25 C', FTag.GetValueAsText('T= ', ' C', ''));
end;

procedure TTestPLCTagNumber.TextWithANumericFormat;
begin
  FTag.CameFromTheScan(3.14159);
  AssertEquals('two decimal places', '3.14', FTag.GetValueAsText('', '', '0.00'));
end;

procedure TTestPLCTagNumber.TextWithNoFormatUsesTheDefault;
begin
  FTag.CameFromTheScan(7);
  AssertEquals('no format', '7', FTag.GetValueAsText('', '', ''));
end;

procedure TTestPLCTagNumber.TextWithATimeFormatUsesTheValueInMilliseconds;
begin
  //com formato de data ou hora o valor e' lido como milissegundos
  FTag.CameFromTheScan(3661000);
  AssertEquals('one hour, one minute and one second', '01:01:01',
               FTag.GetValueAsText('', '', 'hh:nn:ss'));
end;

initialization
  RegisterTest(TTestPLCTagNumber);

end.
