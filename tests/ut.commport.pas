{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TCommPortDriver, a classe base de todas as portas.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Nenhum driver de protocolo fala com o sistema operacional: todos passam por
  aqui. Esta classe abre e fecha a porta, avisa quem escuta, serializa os
  comandos de um driver contra os dos outros e entrega os pacotes prontos as
  primitivas que cada porta concreta implementa.

  O TFakeCommPort e' uma porta concreta como qualquer outra - implementa as
  seis primitivas e nada mais - entao o que se exercita aqui e' a classe base
  de verdade, nao uma imitacao dela.
}
{$ELSE}
{:
  @abstract(TCommPortDriver tests, the base class of every port.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  No protocol driver talks to the operating system: they all go through here.
  This class opens and closes the port, notifies the listeners, serializes one
  driver's commands against the others' and hands the finished packets to the
  primitives each concrete port implements.

  TFakeCommPort is a concrete port like any other - it implements the six
  primitives and nothing else - so what is exercised here is the real base
  class, not an imitation of it.
}
{$ENDIF}
unit ut.commport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  CommPort, commtypes,
  testsupport.bytes, testsupport.fakeport, testsupport.fakedriver;

type

  { TTestCommPort }

  TTestCommPort = class(TTestCase)
  private
    FPorta:TFakeCommPort;
    FAberta, FErroAoAbrir, FFechada, FErroAoFechar:LongInt;
    FComecos, FFins:LongInt;
    FChamadas:LongInt;
    FPacoteRecebido:TIOPacket;
    FErrosDeLeitura, FErrosDeEscrita, FDesconexoes:LongInt;
    FUltimoErro:TIOResult;
    FArquivoDeLog:String;
    procedure CountReadError(Error:TIOResult);
    procedure CountWriteError(Error:TIOResult);
    procedure CountDisconnection(Sender:TObject);
    function  LogContents:String;
    procedure CountOpened(Sender:TObject);
    procedure CountOpenError(Sender:TObject);
    procedure CountClosed(Sender:TObject);
    procedure CountCloseError(Sender:TObject);
    procedure CountStart(Sender:TObject);
    procedure CountEnd(Sender:TObject);
    procedure HookUpTheCounters;
    //: recebe o pacote pronto na sobrecarga que devolve por chamada de volta
    procedure KeepPacket(var aPacote:TIOPacket);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //travamento entre drivers / locking between drivers
    procedure ANewPortIsNotLocked;
    procedure LockingMarksTheOwner;
    procedure AnotherDriverCannotLock;
    procedure LockingTwiceDoesNotStack;
    procedure UnlockingWithAnotherIdDoesNotUnlock;
    procedure UnlockingWithTheRightIdReleases;
    procedure UnlockingAFreePortDoesNotComplain;

    //abrir e fechar / opening and closing
    procedure ANewPortIsClosed;
    procedure OpenAndClose;
    procedure AFailedOpenDoesNotLeaveThePortActive;
    procedure AFailedCloseLeavesThePortActive;

    //avisos / notifications
    procedure OpeningNotifiesTheListener;
    procedure ClosingNotifiesTheListener;
    procedure AFailedOpenReportsTheError;
    procedure AFailedCloseReportsTheError;

    //comandos de entrada e saida / io commands
    procedure ACommandOnAClosedPortReturnsZero;
    procedure WritesAndReadsInASingleCall;
    procedure TheCommandIdGrowsOnEveryCall;
    procedure BytesWrittenAndReadAreCounted;
    procedure WithNoAnswerTheReadResultIsTimeout;
    procedure ACommandOnAClosedPortDoesNotTouchTheBuffers;

    //os avisos de operacao demorada / the long operation notifications
    procedure ALongOperationReportsStartAndEnd;
    procedure ALongOperationOnAClosedPortStaysBalanced;

    //a sobrecarga que devolve por chamada de volta / the callback overload
    procedure TheCallbackGetsTheFinishedPacket;
    procedure NoCallbackHappensOnAClosedPort;
    procedure ACallbackOnAClosedPortStaysBalanced;

    //drivers pendurados na porta / drivers attached to the port
    procedure AnAttachedDriverRegistersWithThePort;
    procedure AttachingADriverTwiceDoesNotDuplicateIt;
    procedure ChangingPortRemovesItFromThePreviousOne;
    procedure ADestroyedPortReleasesItsOnlyDriver;
    procedure ADestroyedPortReleasesBothDrivers;
    procedure ADestroyedPortReleasesEveryDriver;

    //contadores de trafego / traffic counters
    procedure TheCountersStartAtZero;
    procedure BytesAddUpAcrossCommands;
    procedure ACommandThatDidNotHappenCountsNoBytes;

    //ultimo quadro, para diagnostico / last frame, for diagnostics
    procedure TheLastFrameIsKeptInHex;
    procedure TheLastFrameDoesNotMixInEarlierCommands;
    procedure TheLastFrameSentFollowsAPureWrite;

    //limpeza de buffers / buffer clearing
    procedure FlushingTheBuffersReachesTheConcretePort;
    procedure FlushingOnFailureIsOnByDefault;

    //erros de entrada e saida / io errors
    procedure AReadErrorNotifiesTheListener;
    procedure AWriteErrorNotifiesTheListener;
    procedure ADisconnectionNotifiesTheListener;

    //em tempo de projeto / at design time
    procedure AnExclusivePortDoesNotOpenAtDesignTime;
    procedure ANonExclusivePortOpensAtDesignTime;

    //registro do trafego em arquivo / traffic log file
    procedure TurningLoggingOnCreatesTheFile;
    procedure EveryCommandLeavesALineInTheLog;
    procedure LoggingOffWritesNothing;
    procedure ChangingFileWithLoggingOn;
    procedure ADestroyedPortWithLoggingOnClosesTheFile;
  end;

implementation

const
  //dois "drivers" quaisquer, so' para separar um dono do outro
  DRIVER_A = 101;
  DRIVER_B = 202;

procedure TTestCommPort.SetUp;
begin
  FPorta:=TFakeCommPort.Create(nil);
  FAberta:=0;      FErroAoAbrir:=0;
  FFechada:=0;     FErroAoFechar:=0;
  FComecos:=0;     FFins:=0;
  FChamadas:=0;
  FErrosDeLeitura:=0; FErrosDeEscrita:=0; FDesconexoes:=0;
  FUltimoErro:=iorNone;
  FArquivoDeLog:=GetTempDir+'pascalscada_log_teste.txt';
  if FileExists(FArquivoDeLog) then DeleteFile(FArquivoDeLog);
end;

procedure TTestCommPort.TearDown;
begin
  FreeAndNil(FPorta);
  if (FArquivoDeLog<>'') and FileExists(FArquivoDeLog) then
    DeleteFile(FArquivoDeLog);
end;

procedure TTestCommPort.CountOpened(Sender:TObject);      begin inc(FAberta);      end;
procedure TTestCommPort.CountOpenError(Sender:TObject); begin inc(FErroAoAbrir); end;
procedure TTestCommPort.CountClosed(Sender:TObject);     begin inc(FFechada);     end;
procedure TTestCommPort.CountCloseError(Sender:TObject);begin inc(FErroAoFechar);end;
procedure TTestCommPort.CountStart(Sender:TObject);      begin inc(FComecos);     end;
procedure TTestCommPort.CountEnd(Sender:TObject);         begin inc(FFins);        end;

procedure TTestCommPort.KeepPacket(var aPacote:TIOPacket);
begin
  inc(FChamadas);
  FPacoteRecebido:=aPacote;
end;

procedure TTestCommPort.HookUpTheCounters;
begin
  FPorta.OnCommPortOpened    :=@CountOpened;
  FPorta.OnCommPortOpenError :=@CountOpenError;
  FPorta.OnCommPortClosed    :=@CountClosed;
  FPorta.OnCommPortCloseError:=@CountCloseError;
end;

procedure TTestCommPort.ANewPortIsNotLocked;
begin
  AssertFalse('port just created', FPorta.Locked);
  AssertEquals('no owner', 0, FPorta.LockedBy);
end;

procedure TTestCommPort.LockingMarksTheOwner;
begin
  AssertTrue ('locked',      FPorta.Lock(DRIVER_A));
  AssertTrue ('is locked', FPorta.Locked);
  AssertEquals('owner',        DRIVER_A, FPorta.LockedBy);
end;

procedure TTestCommPort.AnotherDriverCannotLock;
begin
  //e' disso que depende nao embaralhar quadros de dois protocolos na mesma
  //porta
  FPorta.Lock(DRIVER_A);
  AssertFalse('the second driver cannot lock', FPorta.Lock(DRIVER_B));
  AssertEquals('the owner is still the first one', DRIVER_A, FPorta.LockedBy);
end;

procedure TTestCommPort.LockingTwiceDoesNotStack;
begin
  //travar nao e' reentrante: quem ja' travou e pede de novo recebe nao
  FPorta.Lock(DRIVER_A);
  AssertFalse('second lock by the same owner', FPorta.Lock(DRIVER_A));
end;

procedure TTestCommPort.UnlockingWithAnotherIdDoesNotUnlock;
begin
  FPorta.Lock(DRIVER_A);
  AssertFalse('another driver unlocking', FPorta.Unlock(DRIVER_B));
  AssertTrue ('still locked',         FPorta.Locked);
end;

procedure TTestCommPort.UnlockingWithTheRightIdReleases;
begin
  FPorta.Lock(DRIVER_A);
  AssertTrue ('unlocked',        FPorta.Unlock(DRIVER_A));
  AssertFalse('is not locked', FPorta.Locked);
  AssertTrue ('and another one can lock now', FPorta.Lock(DRIVER_B));
end;

procedure TTestCommPort.UnlockingAFreePortDoesNotComplain;
begin
  AssertTrue('unlocking a free port', FPorta.Unlock(DRIVER_A));
end;

procedure TTestCommPort.ANewPortIsClosed;
begin
  AssertFalse('Active',       FPorta.Active);
  AssertFalse('ReallyActive', FPorta.ReallyActive);
end;

procedure TTestCommPort.OpenAndClose;
begin
  FPorta.Active:=true;
  AssertTrue('open', FPorta.Active);
  AssertTrue('really open', FPorta.ReallyActive);

  FPorta.Active:=false;
  AssertFalse('closed', FPorta.Active);
end;

procedure TTestCommPort.AFailedOpenDoesNotLeaveThePortActive;
begin
  //dispositivo ausente: a porta nao pode ficar se dizendo aberta
  FPorta.FalharAoAbrir:=true;
  FPorta.Active:=true;

  AssertFalse('Active',       FPorta.Active);
  AssertFalse('ReallyActive', FPorta.ReallyActive);
end;

procedure TTestCommPort.AFailedCloseLeavesThePortActive;
begin
  FPorta.Active:=true;
  FPorta.FalharAoFechar:=true;
  FPorta.Active:=false;

  AssertTrue('closing failed, so it is still open', FPorta.Active);
end;

procedure TTestCommPort.OpeningNotifiesTheListener;
begin
  HookUpTheCounters;
  FPorta.Active:=true;

  AssertEquals('reported the open', 1, FAberta);
  AssertEquals('no error reported', 0, FErroAoAbrir);
end;

procedure TTestCommPort.ClosingNotifiesTheListener;
begin
  FPorta.Active:=true;
  HookUpTheCounters;
  FPorta.Active:=false;

  AssertEquals('reported the close', 1, FFechada);
  AssertEquals('no error reported', 0, FErroAoFechar);
end;

procedure TTestCommPort.AFailedOpenReportsTheError;
begin
  HookUpTheCounters;
  FPorta.FalharAoAbrir:=true;
  FPorta.Active:=true;

  AssertEquals('reported the error',      1, FErroAoAbrir);
  AssertEquals('and did not report an open', 0, FAberta);
end;

procedure TTestCommPort.AFailedCloseReportsTheError;
begin
  FPorta.Active:=true;
  HookUpTheCounters;
  FPorta.FalharAoFechar:=true;
  FPorta.Active:=false;

  AssertEquals('reported the error',        1, FErroAoFechar);
  AssertEquals('and did not report a close', 0, FFechada);
end;

procedure TTestCommPort.ACommandOnAClosedPortReturnsZero;
var
  pkg:TIOPacket;
begin
  //zero e' o "nao fiz nada" que os drivers conferem antes de olhar o pacote
  AssertEquals('port closed', 0,
               FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 4,
                                    DRIVER_A, 0, @pkg));
end;

procedure TTestCommPort.WritesAndReadsInASingleCall;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB CC DD'));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 4, DRIVER_A, 0, @pkg);

  AssertBytesEqual('what was written', BytesOf('01 02'), FPorta.LastWrittenFrame);
  AssertBytesEqual('what was read',    BytesOf('AA BB CC DD'), pkg.BufferToRead);
end;

procedure TTestCommPort.TheCommandIdGrowsOnEveryCall;
var
  pkg:TIOPacket;
  primeiro, segundo:Cardinal;
begin
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA'));
  FPorta.QueueResponse(BytesOf('BB'));

  primeiro:=FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 1, DRIVER_A, 0, @pkg);
  segundo :=FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 1, DRIVER_A, 0, @pkg);

  AssertTrue('the first id cannot be zero', primeiro<>0);
  AssertTrue('each command has its own id',       segundo>primeiro);
end;

procedure TTestCommPort.BytesWrittenAndReadAreCounted;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB CC'));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_A, 0, @pkg);

  AssertEquals('bytes asked to write', 2, pkg.ToWrite);
  AssertEquals('bytes written',              2, pkg.Written);
  AssertEquals('bytes asked to read',      3, pkg.ToRead);
  AssertEquals('bytes read',                 3, pkg.Received);
  AssertEquals('write ok', Ord(iorOK), Ord(pkg.WriteIOResult));
  AssertEquals('read ok', Ord(iorOK), Ord(pkg.ReadIOResult));
end;

procedure TTestCommPort.WithNoAnswerTheReadResultIsTimeout;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;

  FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 4, DRIVER_A, 0, @pkg);

  AssertEquals('the write went out',    Ord(iorOK),      Ord(pkg.WriteIOResult));
  AssertEquals('the answer did not come', Ord(iorTimeOut), Ord(pkg.ReadIOResult));
  AssertEquals('nothing read', 0, pkg.Received);
end;

procedure TTestCommPort.ACommandOnAClosedPortDoesNotTouchTheBuffers;
var
  pkg:TIOPacket;
begin
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 4, DRIVER_A, 0, @pkg);

  AssertEquals('nothing written to the port', 0, FPorta.WriteCount);
  AssertEquals('write with no result', Ord(iorNone), Ord(pkg.WriteIOResult));
  AssertEquals('read with no result', Ord(iorNone), Ord(pkg.ReadIOResult));
end;

procedure TTestCommPort.ALongOperationReportsStartAndEnd;
var
  pkg:TIOPacket;
begin
  //os drivers usam este par para soltar e retomar a sua secao critica
  //enquanto a porta faz a parte lenta
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA'));

  FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 1, DRIVER_A, 0, @pkg,
                       @CountStart, @CountEnd);

  AssertEquals('reported the start', 1, FComecos);
  AssertEquals('reported the end',    1, FFins);
end;

procedure TTestCommPort.ALongOperationOnAClosedPortStaysBalanced;
var
  pkg:TIOPacket;
begin
  //o par tem que fechar sempre. Quem o usa solta uma secao critica no comeco e
  //a retoma no fim: um comeco sem fim deixa a secao solta para sempre
  FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 1, DRIVER_A, 0, @pkg,
                       @CountStart, @CountEnd);

  AssertEquals('a start must have an end', FComecos, FFins);
end;

procedure TTestCommPort.TheCallbackGetsTheFinishedPacket;
begin
  //a outra sobrecarga nao devolve o pacote pelo parametro: entrega por chamada
  //de volta, com tudo ja' preenchido
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));

  FPorta.IOCommandSync(iocWriteRead, BytesOf('01 02'), 2, 2, DRIVER_A, 0,
                       @KeepPacket, nil, nil);

  AssertEquals('called once', 1, FChamadas);
  AssertBytesEqual('what was read', BytesOf('AA BB'), FPacoteRecebido.BufferToRead);
  AssertEquals('read ok', Ord(iorOK), Ord(FPacoteRecebido.ReadIOResult));
end;

procedure TTestCommPort.NoCallbackHappensOnAClosedPort;
begin
  FPorta.IOCommandSync(iocWriteRead, BytesOf('01 02'), 2, 2, DRIVER_A, 0,
                       @KeepPacket, nil, nil);

  AssertEquals('port closed, nothing to deliver', 0, FChamadas);
end;

procedure TTestCommPort.ACallbackOnAClosedPortStaysBalanced;
begin
  FPorta.IOCommandSync(iocWriteRead, BytesOf('01 02'), 2, 2, DRIVER_A, 0,
                       @KeepPacket, nil, nil, @CountStart, @CountEnd);

  AssertEquals('a start must have an end', FComecos, FFins);
end;

procedure TTestCommPort.AnAttachedDriverRegistersWithThePort;
var
  drv:TFakeProtocolDriver;
begin
  drv:=TFakeProtocolDriver.Create(nil);
  try
    drv.CommunicationPort:=FPorta;
    AssertTrue('the driver kept the port', drv.CommunicationPort=FPorta);
  finally
    drv.Free;
  end;
end;

procedure TTestCommPort.AttachingADriverTwiceDoesNotDuplicateIt;
var
  drv:TFakeProtocolDriver;
begin
  //atribuir a mesma porta de novo nao pode criar um segundo registro
  drv:=TFakeProtocolDriver.Create(nil);
  try
    drv.CommunicationPort:=FPorta;
    drv.CommunicationPort:=FPorta;
    AssertTrue('still on the same port', drv.CommunicationPort=FPorta);
  finally
    drv.Free;
  end;
end;

procedure TTestCommPort.ChangingPortRemovesItFromThePreviousOne;
var
  drv:TFakeProtocolDriver;
  outra:TFakeCommPort;
begin
  outra:=TFakeCommPort.Create(nil);
  drv  :=TFakeProtocolDriver.Create(nil);
  try
    drv.CommunicationPort:=FPorta;
    drv.CommunicationPort:=outra;
    AssertTrue('changed port', drv.CommunicationPort=outra);

    //a porta antiga nao pode mais achar que tem este driver: destrui-la nao
    //pode mexer em quem ja' saiu
    FreeAndNil(FPorta);
    AssertTrue('the old port left without taking the driver', drv.CommunicationPort=outra);
  finally
    drv.Free;
    outra.Free;
  end;
end;

procedure TTestCommPort.ADestroyedPortReleasesItsOnlyDriver;
var
  drv:TFakeProtocolDriver;
begin
  drv:=TFakeProtocolDriver.Create(nil);
  try
    drv.CommunicationPort:=FPorta;
    FreeAndNil(FPorta);

    AssertTrue('the driver must have dropped the destroyed port',
               drv.CommunicationPort=nil);
  finally
    drv.Free;
  end;
end;

procedure TTestCommPort.ADestroyedPortReleasesBothDrivers;
var
  a, b:TFakeProtocolDriver;
begin
  a:=TFakeProtocolDriver.Create(nil);
  b:=TFakeProtocolDriver.Create(nil);
  try
    a.CommunicationPort:=FPorta;
    b.CommunicationPort:=FPorta;

    FreeAndNil(FPorta);

    AssertTrue('first driver', a.CommunicationPort=nil);
    AssertTrue('second driver',  b.CommunicationPort=nil);
  finally
    a.Free;
    b.Free;
  end;
end;

procedure TTestCommPort.ADestroyedPortReleasesEveryDriver;
var
  a, b, c:TFakeProtocolDriver;
begin
  //uma porta com mais de um protocolo pendurado e' arranjo comum. Nenhum deles
  //pode sobrar com um ponteiro para a porta que acabou de ser destruida
  a:=TFakeProtocolDriver.Create(nil);
  b:=TFakeProtocolDriver.Create(nil);
  c:=TFakeProtocolDriver.Create(nil);
  try
    a.CommunicationPort:=FPorta;
    b.CommunicationPort:=FPorta;
    c.CommunicationPort:=FPorta;

    FreeAndNil(FPorta);

    AssertTrue('first driver',  a.CommunicationPort=nil);
    AssertTrue('second driver',   b.CommunicationPort=nil);
    AssertTrue('third driver',  c.CommunicationPort=nil);
  finally
    a.Free;
    b.Free;
    c.Free;
  end;
end;

procedure TTestCommPort.CountReadError(Error:TIOResult);
begin
  inc(FErrosDeLeitura);
  FUltimoErro:=Error;
end;

procedure TTestCommPort.CountWriteError(Error:TIOResult);
begin
  inc(FErrosDeEscrita);
  FUltimoErro:=Error;
end;

procedure TTestCommPort.CountDisconnection(Sender:TObject);
begin
  inc(FDesconexoes);
end;

function TTestCommPort.LogContents:String;
var
  arq:TStringList;
begin
  Result:='';
  if (FArquivoDeLog='') or (not FileExists(FArquivoDeLog)) then exit;

  arq:=TStringList.Create;
  try
    arq.LoadFromFile(FArquivoDeLog);
    Result:=arq.Text;
  finally
    arq.Free;
  end;
end;

procedure TTestCommPort.TheCountersStartAtZero;
begin
  AssertEquals('received',   0, FPorta.RXBytes);
  AssertEquals('transmitted', 0, FPorta.TXBytes);
end;

procedure TTestCommPort.BytesAddUpAcrossCommands;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB CC'));
  FPorta.QueueResponse(BytesOf('DD'));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_A, 0, @pkg);
  AssertEquals('after the first one, transmitted', 2, FPorta.TXBytes);
  AssertEquals('after the first one, received',    3, FPorta.RXBytes);

  FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('03'), 1, DRIVER_A, 0, @pkg);
  AssertEquals('the counters add up', 3, FPorta.TXBytes);
  AssertEquals('the counters add up', 4, FPorta.RXBytes);
end;

procedure TTestCommPort.ACommandThatDidNotHappenCountsNoBytes;
var
  pkg:TIOPacket;
begin
  //porta fechada: nada foi para o fio, nada pode ser contado
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_A, 0, @pkg);

  AssertEquals('transmitted', 0, FPorta.TXBytes);
  AssertEquals('received',    0, FPorta.RXBytes);
end;

procedure TTestCommPort.TheLastFrameIsKeptInHex;
var
  pkg:TIOPacket;
begin
  //as duas propriedades existem para a interface grafica mostrar o que passou
  //pelo fio no ultimo comando
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);

  AssertEquals('sent',  '01 02 ', FPorta.Traffic_send);
  AssertEquals('received', 'AA BB ', FPorta.Traffic_receiver);
end;

procedure TTestCommPort.TheLastFrameDoesNotMixInEarlierCommands;
var
  pkg:TIOPacket;
begin
  //uma leitura pura e depois um escreve-le: o que se ve tem que ser o quadro
  //do ultimo comando, nao os dois emendados
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('11 22'));
  FPorta.QueueResponse(BytesOf('AA BB'));

  FPorta.IOCommandSync(iocRead, 0, nil, 2, DRIVER_A, 0, @pkg);
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);

  AssertEquals('received on the last command', 'AA BB ', FPorta.Traffic_receiver);
end;

procedure TTestCommPort.TheLastFrameSentFollowsAPureWrite;
var
  pkg:TIOPacket;
begin
  //uma escrita pura tambem poe bytes no fio: o quadro enviado tem que ser o
  //dela, e nao o do comando anterior
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);

  FPorta.IOCommandSync(iocWrite, 2, BytesOf('03 04'), 0, DRIVER_A, 0, @pkg);

  AssertEquals('sent on a pure write', '03 04 ', FPorta.Traffic_send);
end;

procedure TTestCommPort.FlushingTheBuffersReachesTheConcretePort;
begin
  //a classe base nao limpa nada por conta propria: ela repassa a limpeza para
  //a porta concreta, que e' quem sabe o que ha para esvaziar
  AssertEquals('no flush yet', 0, FPorta.LimpezasDeBuffer);

  FPorta.LimparBuffers;
  AssertEquals('the concrete port was called', 1, FPorta.LimpezasDeBuffer);
end;

procedure TTestCommPort.FlushingOnFailureIsOnByDefault;
begin
  AssertTrue('flush the buffers on a communication error', FPorta.ClearBuffersOnCommErrors);
end;

procedure TTestCommPort.AReadErrorNotifiesTheListener;
begin
  FPorta.OnCommErrorReading:=@CountReadError;
  FPorta.OnCommErrorWriting:=@CountWriteError;

  FPorta.AvisarErroDeEntradaESaida(false, iorTimeOut);

  AssertEquals('reported the read',        1, FErrosDeLeitura);
  AssertEquals('and not the write',         0, FErrosDeEscrita);
  AssertEquals('with the error that happened', Ord(iorTimeOut), Ord(FUltimoErro));
end;

procedure TTestCommPort.AWriteErrorNotifiesTheListener;
begin
  FPorta.OnCommErrorReading:=@CountReadError;
  FPorta.OnCommErrorWriting:=@CountWriteError;

  FPorta.AvisarErroDeEntradaESaida(true, iorPortError);

  AssertEquals('reported the write',        1, FErrosDeEscrita);
  AssertEquals('and not the read',         0, FErrosDeLeitura);
  AssertEquals('with the error that happened', Ord(iorPortError), Ord(FUltimoErro));
end;

procedure TTestCommPort.ADisconnectionNotifiesTheListener;
begin
  FPorta.OnCommPortDisconnected:=@CountDisconnection;
  FPorta.AvisarDesconexao;

  AssertEquals('reported the disconnection', 1, FDesconexoes);
end;

procedure TTestCommPort.AnExclusivePortDoesNotOpenAtDesignTime;
begin
  //uma porta serial nao pode ser aberta pelo ambiente de desenvolvimento: ela
  //tomaria o equipamento de quem esta' rodando
  FPorta.MarcarComoExclusiva;
  FPorta.MarcarComoEmProjeto;

  FPorta.Active:=true;

  AssertTrue ('the property takes the value',  FPorta.Active);
  AssertFalse('but the port is not really open', FPorta.ReallyActive);
end;

procedure TTestCommPort.ANonExclusivePortOpensAtDesignTime;
begin
  //uma porta de rede pode, porque nao impede ninguem de usar o equipamento
  FPorta.MarcarComoEmProjeto;
  FPorta.Active:=true;

  AssertTrue('really open', FPorta.ReallyActive);
end;

procedure TTestCommPort.TurningLoggingOnCreatesTheFile;
begin
  FPorta.LogFile:=FArquivoDeLog;
  FPorta.LogIOActions:=true;

  AssertTrue ('the file must exist', FileExists(FArquivoDeLog));
  AssertTrue ('and logging stayed on', FPorta.LogIOActions);
end;

procedure TTestCommPort.EveryCommandLeavesALineInTheLog;
var
  pkg:TIOPacket;
begin
  //e' por aqui que se captura o trafego de um equipamento de verdade para
  //depois alimentar os testes de decodificacao
  FPorta.LogFile:=FArquivoDeLog;
  FPorta.LogIOActions:=true;
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);
  FPorta.LogIOActions:=false;

  AssertTrue('what was written is in the log', Pos('01 02', LogContents)>0);
  AssertTrue('what was read too',              Pos('AA BB', LogContents)>0);
end;

procedure TTestCommPort.LoggingOffWritesNothing;
var
  pkg:TIOPacket;
begin
  FPorta.LogFile:=FArquivoDeLog;
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);

  AssertFalse('with logging off there is no file either', FileExists(FArquivoDeLog));
end;

procedure TTestCommPort.ChangingFileWithLoggingOn;
var
  segundo:String;
  pkg:TIOPacket;
begin
  segundo:=FArquivoDeLog+'.2';
  FPorta.LogFile:=FArquivoDeLog;
  FPorta.LogIOActions:=true;
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));

  try
    //trocar o arquivo tem que fechar o anterior e seguir registrando no novo
    FPorta.LogFile:=segundo;
    AssertTrue('logging is still on', FPorta.LogIOActions);

    FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);
    FPorta.LogIOActions:=false;

    AssertTrue('the new file was created', FileExists(segundo));
  finally
    if FileExists(segundo) then DeleteFile(segundo);
  end;
end;

procedure TTestCommPort.ADestroyedPortWithLoggingOnClosesTheFile;
var
  porta:TFakeCommPort;
begin
  porta:=TFakeCommPort.Create(nil);
  try
    porta.LogFile:=FArquivoDeLog+'.destruir';
    porta.LogIOActions:=true;
  finally
    //sem desligar o registro antes: e' exatamente o que o destrutor tem que
    //dar conta sozinho
    porta.Free;
  end;

  if FileExists(FArquivoDeLog+'.destruir') then
    DeleteFile(FArquivoDeLog+'.destruir');

  //o que prova isto e' o contador de blocos nao liberados no fim da rodada:
  //com o destrutor abandonando o arquivo, esta porta deixava dois blocos para
  //tras. O teste existe para que a porta seja destruida assim.
  AssertTrue('the port was destroyed with logging on', true);
end;

initialization
  RegisterTest(TTestCommPort);

end.
