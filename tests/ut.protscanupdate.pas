{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TScanUpdate: a ponte entre o driver e os tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Quando um tag pede uma leitura de varredura, quem atende nao e' o driver
  diretamente: o pedido e' enfileirado nesta thread, que chama o driver fora
  da thread principal e depois devolve o resultado ao tag de volta na thread
  principal, em lote. E' esse vai-e-vem que faz a supervisao nao travar a
  interface enquanto conversa com o equipamento.

  Os testes montam os dois lados - o gancho que o driver preencheria e a
  chamada de volta que o tag receberia - e conferem o que atravessa a ponte.
}
{$ELSE}
{:
  @abstract(TScanUpdate tests: the bridge between the driver and the tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  When a tag asks for a scan read, it is not the driver that answers directly:
  the request is queued on this thread, which calls the driver off the main
  thread and then hands the result back to the tag on the main thread, in
  batches. That round trip is what keeps the supervision from freezing the
  interface while it talks to the device.

  The tests set up both sides - the hook the driver would fill in and the
  callback the tag would get - and check what crosses the bridge.
}
{$ENDIF}
unit ut.protscanupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, ProtScanUpdate,
  testsupport.fakedriver, testsupport.protocol;

type

  { TTestScanUpdate }

  TTestScanUpdate = class(TTestCase)
  private
    FPonte:TScanUpdate;
    //o que o driver responderia / what the driver would answer
    FPedidos:LongInt;
    FTagRecVisto:TTagRec;
    FValorAEntregar:Double;
    FResultadoAEntregar:TProtocolIOResult;
    //o que o tag recebeu / what the tag got
    FEntregas:LongInt;
    FUltimoReqID:LongWord;
    FUltimoComando:TTagCommand;
    FUltimoResultado:TProtocolIOResult;
    FUltimoDeslocamento:LongInt;
    FUltimosValores:TArrayOfDouble;

    procedure FetchValues(const aTagRec:TTagRec; var aValores:TScanReadRec);
    procedure DeliverTo(const ReqID:LongWord; Values:TArrayOfDouble;
                       ValuesTimeStamp:QWord; TagCommand:TTagCommand;
                       LastResult:TProtocolIOResult; OffSet:LongInt);
    function  WaitForDeliveries(aQuantas, aPrazoMs:LongInt):Boolean;
    function  TestRequest:TTagRec;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheRequestReachesTheDriverOffTheMainThread;
    procedure TheDriverValueIsDeliveredToTheTag;
    procedure TheCommandDeliveredIsAScanCommand;
    procedure TheDriverResultIsPassedOn;
    procedure WithNoHookTheResultIsADriverError;
    procedure TheRequestIdentifierIsPassedOn;
    procedure TheRealOffsetIsPassedOn;
    procedure ManyRequestsAreAllDelivered;
  end;

implementation

//Criar um driver de protocolo custa caro para destruir: um so' atende a classe
//inteira. Ele existe aqui porque o TScanUpdate exige um dono que seja driver.
var
  DonoCompartilhado:TFakeProtocolDriver = nil;

{ TTestScanUpdate }

procedure TTestScanUpdate.SetUp;
begin
  if DonoCompartilhado=nil then
    DonoCompartilhado:=TFakeProtocolDriver.Create(nil);

  FPedidos:=0;
  FEntregas:=0;
  FValorAEntregar:=0;
  FResultadoAEntregar:=ioOk;
  FUltimoReqID:=0;
  FUltimoDeslocamento:=-1;
  FUltimosValores:=nil;

  FPonte:=TScanUpdate.Create(true, DonoCompartilhado, nil);
  FPonte.OnGetValue:=@FetchValues;
  FPonte.WakeUp;
  FPonte.WaitLoopStarts;
end;

procedure TTestScanUpdate.TearDown;
begin
  if FPonte<>nil then begin
    FPonte.Terminate;
    FreeAndNil(FPonte);
  end;
end;

procedure TTestScanUpdate.FetchValues(const aTagRec:TTagRec; var aValores:TScanReadRec);
begin
  inc(FPedidos);
  FTagRecVisto:=aTagRec;

  SetLength(aValores.Values, 1);
  aValores.Values[0]      :=FValorAEntregar;
  aValores.LastQueryResult:=FResultadoAEntregar;
  aValores.ClkMonotonicTStamp:=GetTickCount64;
end;

procedure TTestScanUpdate.DeliverTo(const ReqID:LongWord; Values:TArrayOfDouble;
                                   ValuesTimeStamp:QWord; TagCommand:TTagCommand;
                                   LastResult:TProtocolIOResult; OffSet:LongInt);
begin
  inc(FEntregas);
  FUltimoReqID       :=ReqID;
  FUltimoComando     :=TagCommand;
  FUltimoResultado   :=LastResult;
  FUltimoDeslocamento:=OffSet;
  FUltimosValores    :=Copy(Values, 0, Length(Values));
end;

function TTestScanUpdate.WaitForDeliveries(aQuantas, aPrazoMs:LongInt):Boolean;
var
  gasto:LongInt;
begin
  //a entrega acontece na thread principal: sem bombear a fila de sincronizacao
  //ela nunca chega
  gasto:=0;
  while (FEntregas<aQuantas) and (gasto<aPrazoMs) do begin
    CheckSynchronize(5);
    inc(gasto, 5);
  end;
  Result:=FEntregas>=aQuantas;
end;

function TTestScanUpdate.TestRequest:TTagRec;
begin
  Result:=TagRecFor(1, 3, 0, 100, 1);
  Result.ID        :=77;
  Result.RealOffset:=0;
  Result.CallBack  :=@DeliverTo;
end;

procedure TTestScanUpdate.TheRequestReachesTheDriverOffTheMainThread;
var
  pedido:TTagRec;
begin
  pedido:=TestRequest;
  FPonte.ScanRead(pedido);

  AssertTrue  ('the request must be served', WaitForDeliveries(1, 3000));
  AssertEquals('the driver was called once',   1,   FPedidos);
  AssertEquals('and with the address asked for',        100, FTagRecVisto.Address);
end;

procedure TTestScanUpdate.TheDriverValueIsDeliveredToTheTag;
var
  pedido:TTagRec;
begin
  FValorAEntregar:=42;
  pedido:=TestRequest;
  FPonte.ScanRead(pedido);

  AssertTrue  ('delivered',        WaitForDeliveries(1, 3000));
  AssertEquals('one value',        1,  Length(FUltimosValores));
  AssertEquals('the value from the driver', 42, FUltimosValores[0], 0);
end;

procedure TTestScanUpdate.TheCommandDeliveredIsAScanCommand;
var
  pedido:TTagRec;
begin
  pedido:=TestRequest;
  FPonte.ScanRead(pedido);

  AssertTrue  ('delivered', WaitForDeliveries(1, 3000));
  AssertEquals('command',  Ord(tcScanRead), Ord(FUltimoComando));
end;

procedure TTestScanUpdate.TheDriverResultIsPassedOn;
var
  pedido:TTagRec;
begin
  //o tag precisa saber que a leitura falhou, nao so' nao receber valor
  FResultadoAEntregar:=ioTimeOut;
  pedido:=TestRequest;
  FPonte.ScanRead(pedido);

  AssertTrue  ('delivered',  WaitForDeliveries(1, 3000));
  AssertEquals('result', Ord(ioTimeOut), Ord(FUltimoResultado));
end;

procedure TTestScanUpdate.WithNoHookTheResultIsADriverError;
var
  pedido:TTagRec;
begin
  //sem ninguem para atender, a ponte tem que dizer que nao ha driver
  FPonte.OnGetValue:=nil;
  pedido:=TestRequest;
  FPonte.ScanRead(pedido);

  AssertTrue  ('delivered all the same', WaitForDeliveries(1, 3000));
  AssertEquals('driver error',       Ord(ioDriverError), Ord(FUltimoResultado));
  AssertEquals('and the driver was not called', 0, FPedidos);
end;

procedure TTestScanUpdate.TheRequestIdentifierIsPassedOn;
var
  pedido:TTagRec;
begin
  //e' por ele que o tag reconhece a resposta do seu proprio pedido
  pedido:=TestRequest;
  FPonte.ScanRead(pedido);

  AssertTrue  ('delivered', WaitForDeliveries(1, 3000));
  AssertEquals('identifier', 77, FUltimoReqID);
end;

procedure TTestScanUpdate.TheRealOffsetIsPassedOn;
var
  pedido:TTagRec;
begin
  //o deslocamento diz em que ponto do tag os valores entram
  pedido:=TestRequest;
  pedido.RealOffset:=5;
  FPonte.ScanRead(pedido);

  AssertTrue  ('delivered',      WaitForDeliveries(1, 3000));
  AssertEquals('offset',  5, FUltimoDeslocamento);
end;

procedure TTestScanUpdate.ManyRequestsAreAllDelivered;
var
  pedido:TTagRec;
  c:LongInt;
begin
  //a ponte junta os pendentes numa ida so' a thread principal: nenhum pode
  //ficar pelo caminho
  pedido:=TestRequest;
  for c:=1 to 5 do
    FPonte.ScanRead(pedido);

  AssertTrue  ('the five deliveries', WaitForDeliveries(5, 5000));
  AssertEquals('the driver was called five times', 5, FPedidos);
end;

initialization
  RegisterTest(TTestScanUpdate);

finalization
  FreeAndNil(DonoCompartilhado);

end.
