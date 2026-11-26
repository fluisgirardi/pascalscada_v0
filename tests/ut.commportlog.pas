{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do log de comunicacao: o que as portas e os drivers publicam no hub.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  As portas publicam os bytes de cada comando (lcComm, llTrace), os erros de
  leitura/escrita (lcComm, llWarning) e o ciclo de vida da conexao
  (lcConnection). O S7CommPlus repassa as mensagens de depuracao da conexao
  como lcProtocol/llDebug, com o driver como fonte.
}
{$ELSE}
{:
  @abstract(Communication logging tests: what ports and drivers post to the hub.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Ports post the bytes of each command (lcComm, llTrace), read/write errors
  (lcComm, llWarning) and the connection life cycle (lcConnection). S7CommPlus
  forwards the debug messages of its connection as lcProtocol/llDebug, with
  the driver as the source.
}
{$ENDIF}
unit ut.commportlog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  commtypes, CommPort, socket_types, tcp_udpport, serialport, S7PlusFamily, pscadalog,
  testsupport.bytes, testsupport.fakeport, testsupport.fakeserver,
  testsupport.logcollector;

type

  { TTestCommPortLog }

  TTestCommPortLog = class(TTestCase)
  private
    FOwner:TComponent;
    FPort:TFakeCommPort;
    FLog:TLogCollector;
    procedure NewLog(Categories:TpSCADALogCategories; MinLevel:TpSCADALogLevel);
    procedure CheckEntry(Index:Integer; Category:TpSCADALogCategory;
                         Level:TpSCADALogLevel; const Msg:String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WriteReadPostsTheBytesSentAndReceived;
    procedure ReadWritePostsTheReadFirst;
    procedure ACommandOnAClosedPortPostsNothing;
    procedure TrafficIsOnlyPostedAtTrace;
    procedure ReadAndWriteErrorsAreWarnings;
    procedure StartingAndStoppingThePortArePosted;
    procedure AFailedOpenIsAnError;
    procedure ALostConnectionIsAWarning;
    procedure TheSerialPortDescribesItsSettings;
    procedure TheS7CommPlusDriverForwardsItsDebugMessages;
  end;

  { TTestTcpPortLog }

  TTestTcpPortLog = class(TTestCase)
  private
    FServer:TServidorDeTeste;
    FPort:TTCP_UDPPort;
    FLog:TLogCollector;
    function WaitFor(Connected:Boolean; Ms:Integer):Boolean;
    function WaitForEntry(const Prefix:String; From, Ms:Integer):Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheConnectionIsPostedWithHostAndPort;
    procedure ADropAndTheReconnectionArePostedInOrder;
  end;

implementation

const
  TEST_DRIVER = 77;

type
  TSerialPortAccess = class(TSerialPortDriver);
  TS7PlusDriverAccess = class(TS7CommPlusDriver);

{ TTestCommPortLog }

procedure TTestCommPortLog.SetUp;
begin
  FOwner := TComponent.Create(nil);
  FOwner.Name := 'Form1';
  FPort := TFakeCommPort.Create(FOwner);
  FPort.Name := 'Port1';
end;

procedure TTestCommPortLog.TearDown;
begin
  FreeAndNil(FLog);
  FreeAndNil(FOwner);
end;

procedure TTestCommPortLog.NewLog(Categories:TpSCADALogCategories; MinLevel:TpSCADALogLevel);
begin
  FLog := TLogCollector.Create(Categories, MinLevel);
end;

procedure TTestCommPortLog.CheckEntry(Index:Integer; Category:TpSCADALogCategory;
  Level:TpSCADALogLevel; const Msg:String);
begin
  AssertTrue('entry '+IntToStr(Index)+' exists'+FLog.Dump, Index<FLog.Count);
  AssertEquals('category of '+IntToStr(Index), PSLogCategoryName(Category), PSLogCategoryName(FLog[Index].Category));
  AssertEquals('level of '+IntToStr(Index), PSLogLevelName(Level), PSLogLevelName(FLog[Index].Level));
  AssertEquals('message of '+IntToStr(Index), Msg, FLog[Index].Msg);
end;

procedure TTestCommPortLog.WriteReadPostsTheBytesSentAndReceived;
var
  pkg:TIOPacket;
begin
  FPort.Active := true;
  NewLog([lcComm], llTrace);
  FPort.QueueResponse(BytesOf('AA BB CC'));
  FPort.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, TEST_DRIVER, 0, @pkg);
  AssertTrue('flush', FLog.Flush);
  AssertEquals('two entries'+FLog.Dump, 2, FLog.Count);
  CheckEntry(0, lcComm, llTrace, 'TX 2/2 iorOK');
  AssertEquals('bytes sent', '01 02', PSLogHex(FLog[0].Data));
  CheckEntry(1, lcComm, llTrace, 'RX 3/3 iorOK');
  AssertEquals('bytes received', 'AA BB CC', PSLogHex(FLog[1].Data));
  AssertEquals('source', 'Form1.Port1', FLog[0].Source);
  AssertTrue('class', FLog[0].SourceClass=TFakeCommPort);
end;

procedure TTestCommPortLog.ReadWritePostsTheReadFirst;
var
  pkg:TIOPacket;
begin
  FPort.Active := true;
  NewLog([lcComm], llTrace);
  FPort.QueueResponse(BytesOf('10'));
  FPort.IOCommandSync(iocReadWrite, 1, BytesOf('20'), 1, TEST_DRIVER, 0, @pkg);
  AssertTrue('flush', FLog.Flush);
  CheckEntry(0, lcComm, llTrace, 'RX 1/1 iorOK');
  CheckEntry(1, lcComm, llTrace, 'TX 1/1 iorOK');
end;

procedure TTestCommPortLog.ACommandOnAClosedPortPostsNothing;
var
  pkg:TIOPacket;
begin
  //IOCommandSync gives up before any I/O: posting it would repeat once per
  //scan for as long as the device is down; the drop is in lcConnection
  NewLog([lcComm], llTrace);
  AssertEquals('no command', 0,
    FPort.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, TEST_DRIVER, 0, @pkg));
  AssertTrue('flush', FLog.Flush);
  AssertEquals('no traffic'+FLog.Dump, 0, FLog.Count);
end;

procedure TTestCommPortLog.TrafficIsOnlyPostedAtTrace;
var
  pkg:TIOPacket;
begin
  FPort.Active := true;
  NewLog([lcComm], llDebug);
  FPort.QueueResponse(BytesOf('AA'));
  FPort.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 1, TEST_DRIVER, 0, @pkg);
  AssertTrue('flush', FLog.Flush);
  AssertEquals('no traffic below trace'+FLog.Dump, 0, FLog.Count);
end;

procedure TTestCommPortLog.ReadAndWriteErrorsAreWarnings;
begin
  NewLog([lcComm], llWarning);
  FPort.AvisarErroDeEntradaESaida(false, iorTimeOut);
  FPort.AvisarErroDeEntradaESaida(true, iorPortError);
  AssertTrue('flush', FLog.Flush);
  CheckEntry(0, lcComm, llWarning, 'read failed: iorTimeOut');
  CheckEntry(1, lcComm, llWarning, 'write failed: iorPortError');
end;

procedure TTestCommPortLog.StartingAndStoppingThePortArePosted;
begin
  NewLog([lcConnection], llInfo);
  FPort.Active := true;
  FPort.Active := false;
  AssertTrue('flush', FLog.Flush);
  CheckEntry(0, lcConnection, llInfo, 'port started');
  CheckEntry(1, lcConnection, llInfo, 'port stopped');
end;

procedure TTestCommPortLog.AFailedOpenIsAnError;
begin
  NewLog([lcConnection], llInfo);
  FPort.FalharAoAbrir := true;
  FPort.Active := true;
  AssertTrue('flush', FLog.Flush);
  AssertEquals('one entry'+FLog.Dump, 1, FLog.Count);
  AssertEquals('error', PSLogLevelName(llError), PSLogLevelName(FLog[0].Level));
  AssertEquals('message', 'open failed', Copy(FLog[0].Msg, 1, 11));
end;

procedure TTestCommPortLog.ALostConnectionIsAWarning;
begin
  FPort.Active := true;
  NewLog([lcConnection], llInfo);
  FPort.AvisarDesconexao;
  AssertTrue('flush', FLog.Flush);
  AssertEquals('one entry'+FLog.Dump, 1, FLog.Count);
  AssertEquals('warning', PSLogLevelName(llWarning), PSLogLevelName(FLog[0].Level));
  AssertEquals('message', 'connection lost', Copy(FLog[0].Msg, 1, 15));
end;

procedure TTestCommPortLog.TheSerialPortDescribesItsSettings;
var
  S:TSerialPortDriver;
begin
  S := TSerialPortDriver.Create(nil);
  try
    //the name is kept only for a port that exists (Linux has /dev/ttyS9, Windows has no
    //such COM port) unless any name is accepted
    S.AcceptAnyPortName := true;
    S.COMPort := 'ttyS9';
    S.BaudRate := br9600;
    S.DataBits := db7;
    S.Paridade := spEven;
    S.StopBits := sb2;
    AssertEquals('ttyS9 9600 7E2', TSerialPortAccess(S).LogDescription);
    S.BaudRate := br115200;
    S.DataBits := db8;
    S.Paridade := spNone;
    S.StopBits := sb1;
    AssertEquals('ttyS9 115200 8N1', TSerialPortAccess(S).LogDescription);
  finally
    S.Free;
  end;
end;

procedure TTestCommPortLog.TheS7CommPlusDriverForwardsItsDebugMessages;
var
  D:TS7CommPlusDriver;
  i:Integer;
begin
  //the fake port stays silent: the COTP connection request goes out and
  //nothing comes back, which the connection reports through its debug messages
  D := TS7CommPlusDriver.Create(FOwner);
  D.Name := 'S7Plus1';
  FPort.Active := true;
  D.CommunicationPort := FPort;
  NewLog([lcProtocol, lcComm], llTrace);
  //under the driver's IO lock, as its public API does: the scan thread runs EnsureConnected on
  //every tick, and two threads finding no connection both made one - the one overwritten
  //leaked (208 bytes on linux32)
  TS7PlusDriverAccess(D).LockDriverIO;
  try
    AssertFalse('no PLC, no connection', TS7PlusDriverAccess(D).EnsureConnected);
  finally
    TS7PlusDriverAccess(D).UnlockDriverIO;
  end;
  AssertTrue('flush', FLog.Flush);
  i := FLog.IndexOf(lcProtocol, '');
  AssertTrue('a protocol message was posted'+FLog.Dump, i>=0);
  AssertEquals('debug', PSLogLevelName(llDebug), PSLogLevelName(FLog[i].Level));
  AssertEquals('the driver is the source', 'Form1.S7Plus1', FLog[i].Source);
  AssertTrue('driver class', FLog[i].SourceClass=TS7CommPlusDriver);
  AssertTrue('and the request went out by the port'+FLog.Dump, FLog.IndexOf(lcComm, 'TX ')>=0);
end;

{ TTestTcpPortLog }

procedure TTestTcpPortLog.SetUp;
begin
  FServer := TServidorDeTeste.Create;
  FPort := TTCP_UDPPort.Create(nil);
  FPort.Name := 'TcpPort1';
  FPort.Host := '127.0.0.1';
  FPort.Port := FServer.Porta;
  FPort.PortType := ptTCP;
  FPort.Timeout := 300;
  FPort.ReconnectRetryInterval := 200;
  FLog := TLogCollector.Create([lcConnection], llInfo);
end;

procedure TTestTcpPortLog.TearDown;
begin
  FreeAndNil(FPort);
  FreeAndNil(FServer);
  FreeAndNil(FLog);
end;

function TTestTcpPortLog.WaitFor(Connected:Boolean; Ms:Integer):Boolean;
var
  spent:Integer;
begin
  spent := 0;
  while (FPort.ReallyActive<>Connected) and (spent<Ms) do begin
    Sleep(5);
    Inc(spent, 5);
  end;
  Result := FPort.ReallyActive=Connected;
end;

function TTestTcpPortLog.WaitForEntry(const Prefix:String; From, Ms:Integer):Integer;
var
  spent:Integer;
begin
  spent := 0;
  repeat
    FLog.Flush;
    Result := FLog.IndexOf(lcConnection, Prefix, From);
    if Result>=0 then exit;
    Sleep(20);
    Inc(spent, 20);
  until spent>=Ms;
end;

procedure TTestTcpPortLog.TheConnectionIsPostedWithHostAndPort;
var
  i:Integer;
  where:String;
begin
  where := Format('(127.0.0.1:%d TCP)', [FServer.Porta]);
  FPort.Active := true;
  AssertTrue('connected', WaitFor(true, 3000));
  i := WaitForEntry('connected', 0, 2000);
  AssertTrue('connection posted'+FLog.Dump, i>=0);
  AssertEquals('with host and port', 'connected '+where, FLog[i].Msg);
  AssertEquals('source', 'TcpPort1', FLog[i].Source);
  AssertTrue('port started before'+FLog.Dump,
             FLog.IndexOf(lcConnection, 'port started '+where)>=0);
end;

procedure TTestTcpPortLog.ADropAndTheReconnectionArePostedInOrder;
var
  pkg:TIOPacket;
  lost, again, back:Integer;
begin
  FPort.Active := true;
  AssertTrue('connected', WaitFor(true, 3000));

  FServer.SoltarAConexao;
  //the socket only notices it dropped when it is used
  FPort.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, TEST_DRIVER, 0, @pkg);
  FPort.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, TEST_DRIVER, 0, @pkg);
  AssertTrue('dropped', WaitFor(false, 3000));
  AssertTrue('came back', WaitFor(true, 5000));

  lost := WaitForEntry('connection lost', 0, 2000);
  AssertTrue('the drop was posted'+FLog.Dump, lost>=0);
  AssertEquals('as a warning', PSLogLevelName(llWarning), PSLogLevelName(FLog[lost].Level));
  again := WaitForEntry('reconnecting', lost, 2000);
  AssertTrue('then the reconnection'+FLog.Dump, again>lost);
  back := WaitForEntry('connected', again, 2000);
  AssertTrue('then connected again'+FLog.Dump, back>again);
end;

initialization
  RegisterTest(TTestCommPortLog);
  RegisterTest(TTestTcpPortLog);

end.
