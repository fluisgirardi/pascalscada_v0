{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da TScanThread: a thread de varredura do driver de
            protocolo.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' a thread que faz o driver andar: a cada volta chama a varredura de
  leitura, e antes dela atende os pedidos que chegaram pela fila - escritas
  por scan e leituras avulsas. Os pedidos vem de outra thread, num registro
  alocado por quem pediu, e a thread fica dona dele.

  O que se testa: a varredura roda e continua rodando, mesmo depois de uma
  excecao; os ritmos - o minimo entre voltas e o que a varredura pede;
  cada pedido chega ao seu tratador, fora da thread principal, com o que foi
  pedido e na ordem em que foi pedido; e nenhum registro fica para tras,
  atendido ou nao.
}
{$ELSE}
{:
  @abstract(TScanThread tests: the protocol driver's scan thread.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It is the thread that keeps the driver going: on every turn it calls the
  read scan, and before that it serves the requests that arrived through the
  queue - scan writes and single reads. Requests come from another thread, in
  a record allocated by whoever asked, and the thread takes ownership of it.

  What is tested: the scan runs and keeps running, even after an exception;
  the pacing - the minimum between turns and what the scan asks for; each
  request reaches its handler, off the main thread, with what was asked and
  in the order it was asked; and no record is left behind, served or not.
}
{$ENDIF}
unit ut.scanthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, ProtocolTypes, protscan, testsupport.protocol;

type

  { TTestScanThread }

  TTestScanThread = class(TTestCase)
  private
    FThread:TScanThread;
    FScans:LongInt;
    FSleepToAsk:LongInt;
    FRaiseOnFirstScan:Boolean;
    FWrites, FReads:LongInt;
    FWriteAddresses:array of LongInt;
    FLastWriteValues:TArrayOfDouble;
    FLastReadAddress:LongInt;
    FHandlerThread:TThreadID;
    procedure ScanRead(Sender:TObject; var NeedSleep:LongInt);
    function  ScanWrite(const Tag:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
    function  SingleRead(var Tag:TTagRec; var values:TArrayOfDouble):TProtocolIOResult;
    function  NewRequest(aAddress:LongInt; const aValues:array of Double):PScanReqRec;
    function  WaitUntil(var aCounter:LongInt; aAtLeast, aDeadlineMs:LongInt):Boolean;
    procedure Start;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a varredura / the scan
    procedure TheLoopCallsTheScanRead;
    procedure TheScanReadKeepsBeingCalled;
    procedure AnExceptionInTheScanReadDoesNotStopTheThread;
    procedure WithNoScanReadTheThreadStillRuns;

    //o ritmo / the pacing
    procedure MinTimeOfScanPacesTheLoop;
    procedure TheSleepAskedByTheScanPacesTheLoop;

    //os pedidos / the requests
    procedure AScanWriteReachesTheWriteHandler;
    procedure TheWriteHandlerRunsOffTheMainThread;
    procedure ASingleReadReachesTheReadHandler;
    procedure ManyWritesAreServedInOrder;
    procedure WithNoWriteHandlerTheRequestIsDropped;
    procedure WithNoReadHandlerTheRequestIsDropped;

    //o fim / the end
    procedure TerminateStopsTheLoop;
  end;

implementation

{ TTestScanThread }

procedure TTestScanThread.SetUp;
begin
  FScans:=0;
  FSleepToAsk:=0;
  FRaiseOnFirstScan:=false;
  FWrites:=0;
  FReads:=0;
  FWriteAddresses:=nil;
  FLastWriteValues:=nil;
  FLastReadAddress:=-1;
  //TThreadID e' um ponteiro em alguns sistemas (FreeBSD): zero so' com o cast
  //TThreadID is a pointer on some systems (FreeBSD): zero only through the cast
  FHandlerThread:=TThreadID(0);
  //sem atualizador: quem responde aos pedidos, aqui, sao os tratadores
  //with no updater: here it is the handlers that answer the requests
  FThread:=TScanThread.Create(true, nil);
  FThread.OnDoScanRead:=@ScanRead;
  FThread.OnDoScanWrite:=@ScanWrite;
  FThread.OnDoSingleScanRead:=@SingleRead;
end;

procedure TTestScanThread.TearDown;
begin
  if FThread<>nil then begin
    FThread.Terminate;
    FreeAndNil(FThread);
  end;
end;

procedure TTestScanThread.Start;
begin
  FThread.WakeUp;
  FThread.WaitLoopStarts;
end;

procedure TTestScanThread.ScanRead(Sender:TObject; var NeedSleep:LongInt);
begin
  InterLockedIncrement(FScans);
  NeedSleep:=FSleepToAsk;
  if FRaiseOnFirstScan and (FScans=1) then
    raise Exception.Create('a varredura tropecou');
end;

function TTestScanThread.ScanWrite(const Tag:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
var
  n:LongInt;
begin
  FHandlerThread:=GetCurrentThreadId;
  n:=Length(FWriteAddresses);
  SetLength(FWriteAddresses, n+1);
  FWriteAddresses[n]:=Tag.Address;
  FLastWriteValues:=Copy(values, 0, Length(values));
  InterLockedIncrement(FWrites);
  Result:=ioOk;
end;

function TTestScanThread.SingleRead(var Tag:TTagRec; var values:TArrayOfDouble):TProtocolIOResult;
begin
  FHandlerThread:=GetCurrentThreadId;
  FLastReadAddress:=Tag.Address;
  SetLength(values, 1);
  values[0]:=7;
  InterLockedIncrement(FReads);
  Result:=ioOk;
end;

function TTestScanThread.NewRequest(aAddress:LongInt; const aValues:array of Double):PScanReqRec;
var
  i:LongInt;
begin
  //o registro e' de quem pede; a thread fica dona dele e o libera
  //the record is the requester's; the thread takes it over and releases it
  New(Result);
  Result^.Tag:=TagRecFor(1, 3, 16, aAddress, Length(aValues));
  SetLength(Result^.Values, Length(aValues));
  for i:=0 to High(aValues) do
    Result^.Values[i]:=aValues[i];
  Result^.RequestResult:=ioNone;
  Result^.ClkMonotonicTStamp:=0;
end;

function TTestScanThread.WaitUntil(var aCounter:LongInt; aAtLeast, aDeadlineMs:LongInt):Boolean;
var
  gasto:LongInt;
begin
  gasto:=0;
  while (aCounter<aAtLeast) and (gasto<aDeadlineMs) do begin
    Sleep(5);
    inc(gasto, 5);
  end;
  Result:=aCounter>=aAtLeast;
end;

procedure TTestScanThread.TheLoopCallsTheScanRead;
begin
  Start;

  AssertTrue('a varredura rodou', WaitUntil(FScans, 1, 2000));
end;

procedure TTestScanThread.TheScanReadKeepsBeingCalled;
begin
  Start;

  AssertTrue('mais de uma volta', WaitUntil(FScans, 5, 2000));
end;

procedure TTestScanThread.AnExceptionInTheScanReadDoesNotStopTheThread;
begin
  //um driver que tropeca numa volta nao pode derrubar a varredura inteira
  //a driver that trips on one turn must not bring the whole scan down
  FRaiseOnFirstScan:=true;
  Start;

  AssertTrue ('continuou rodando', WaitUntil(FScans, 5, 2000));
  AssertFalse('e a thread esta viva', FThread.Finished);
end;

procedure TTestScanThread.WithNoScanReadTheThreadStillRuns;
var
  pedido:PScanReqRec;
begin
  //sem varredura a thread so' atende a fila
  //with no scan the thread only serves the queue
  FThread.OnDoScanRead:=nil;
  Start;

  pedido:=NewRequest(10, [1]);
  FThread.ScanWrite(pedido);

  AssertTrue('atendeu a fila', WaitUntil(FWrites, 1, 2000));
end;

procedure TTestScanThread.MinTimeOfScanPacesTheLoop;
begin
  //100 ms entre voltas: em meio segundo cabem umas cinco, nunca dezenas
  //100 ms between turns: half a second fits about five, never dozens
  FThread.MinTimeOfScan:=100;
  Start;

  Sleep(550);

  AssertTrue('rodou',              FScans>=2);
  AssertTrue('mas nao desenfreada', FScans<=8);
end;

procedure TTestScanThread.TheSleepAskedByTheScanPacesTheLoop;
begin
  //a varredura devolve quanto quer dormir ate' a proxima volta
  //the scan gives back how long it wants to sleep until the next turn
  FSleepToAsk:=100;
  Start;

  Sleep(550);

  AssertTrue('rodou',              FScans>=2);
  AssertTrue('mas nao desenfreada', FScans<=8);
end;

procedure TTestScanThread.AScanWriteReachesTheWriteHandler;
var
  pedido:PScanReqRec;
begin
  Start;
  pedido:=NewRequest(100, [1.5, 2.5]);

  FThread.ScanWrite(pedido);

  AssertTrue  ('atendido',           WaitUntil(FWrites, 1, 2000));
  AssertEquals('no endereco pedido', 100, FWriteAddresses[0]);
  AssertEquals('dois valores',       2,   Length(FLastWriteValues));
  AssertEquals('o primeiro',         1.5, FLastWriteValues[0], 0);
  AssertEquals('o segundo',          2.5, FLastWriteValues[1], 0);
end;

procedure TTestScanThread.TheWriteHandlerRunsOffTheMainThread;
var
  pedido:PScanReqRec;
begin
  //e' o ponto de tudo: a escrita no equipamento nao pode travar a tela
  //it is the whole point: writing to the device must not freeze the screen
  Start;
  pedido:=NewRequest(100, [1]);

  FThread.ScanWrite(pedido);

  AssertTrue('atendido', WaitUntil(FWrites, 1, 2000));
  AssertTrue('fora da thread principal', FHandlerThread<>MainThreadID);
end;

procedure TTestScanThread.ASingleReadReachesTheReadHandler;
var
  pedido:PScanReqRec;
begin
  Start;
  pedido:=NewRequest(200, []);

  FThread.SingleScanRead(pedido);

  AssertTrue  ('atendido',           WaitUntil(FReads, 1, 2000));
  AssertEquals('no endereco pedido', 200, FLastReadAddress);
  AssertTrue  ('fora da thread principal', FHandlerThread<>MainThreadID);
end;

procedure TTestScanThread.ManyWritesAreServedInOrder;
var
  c:LongInt;
begin
  Start;

  for c:=1 to 5 do
    FThread.ScanWrite(NewRequest(c, [c]));

  AssertTrue('todas atendidas', WaitUntil(FWrites, 5, 3000));
  for c:=1 to 5 do
    AssertEquals(Format('a %d-a na sua vez',[c]), c, FWriteAddresses[c-1]);
end;

procedure TTestScanThread.WithNoWriteHandlerTheRequestIsDropped;
var
  pedido:PScanReqRec;
begin
  //ninguem para atender: o registro tem que ser liberado mesmo assim - o
  //heaptrc no fim da suite e' quem confere
  //nobody to serve it: the record has to be released all the same - the
  //heaptrc at the end of the suite is what checks it
  FThread.OnDoScanWrite:=nil;
  Start;
  pedido:=NewRequest(100, [1]);

  FThread.ScanWrite(pedido);

  WaitUntil(FScans, 5, 2000);
  AssertEquals('ninguem atendeu', 0, FWrites);
end;

procedure TTestScanThread.WithNoReadHandlerTheRequestIsDropped;
var
  pedido:PScanReqRec;
begin
  FThread.OnDoSingleScanRead:=nil;
  Start;
  pedido:=NewRequest(200, []);

  FThread.SingleScanRead(pedido);

  WaitUntil(FScans, 5, 2000);
  AssertEquals('ninguem atendeu', 0, FReads);
end;

procedure TTestScanThread.TerminateStopsTheLoop;
var
  antes:LongInt;
begin
  Start;
  WaitUntil(FScans, 2, 2000);

  FThread.Terminate;
  antes:=FScans;
  Sleep(100);

  AssertTrue  ('acabou',              FThread.Finished);
  AssertEquals('e nao deu mais volta', antes, FScans);
end;

initialization
  RegisterTest(TTestScanThread);

end.
