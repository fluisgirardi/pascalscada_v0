{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TTCP_UDPPort: validacao de endereco e identificacao
            unica da porta.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O identificador da porta nao e' enfeite: o LGXDriver o usa como chave do mapa
  que separa os dados de cada CLP. Duas portas distintas com o mesmo id
  misturariam dados de equipamentos diferentes, entao o que os testes garantem
  e' que endereco, porta e tipo entram todos na conta.
}
{$ELSE}
{:
  @abstract(TTCP_UDPPort tests: address validation and the port's unique id.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The port id is not decoration: LGXDriver uses it as the key of the map that
  keeps each PLC's data apart. Two distinct ports sharing an id would mix data
  from different devices, so what the tests guarantee is that address, port
  number and type all take part in it.
}
{$ENDIF}
unit ut.tcp_udpport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  CommPort, commtypes, socket_types, tcp_udpport,
  testsupport.bytes, testsupport.fakeserver;

type

  { TTestTcpUdpPort }

  TTestTcpUdpPort = class(TTestCase)
  private
    FPorta:TTCP_UDPPort;
    function  IdOf(const aEndereco:String; aPorta:LongInt; aTipo:TPortType):TPortUniqueID;
    function  TypeByteOf(aId:TPortUniqueID):Byte;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //validacao de endereco / address validation
    procedure ValidAddressesAreAccepted;
    procedure TheFirstOctetCannotBeZeroOr255;
    procedure TheLastOctetCannotBeZeroOr255;
    procedure ItMustHaveFourOctets;
    procedure AnOctetOutOfRangeIsRefused;
    procedure ANonNumericOctetIsRefused;
    procedure EmptyTextIsNotAValidAddress;

    //atribuicao do endereco / assigning the address
    procedure AHostNameIsRefused;
    procedure ARefusedAddressDoesNotEraseThePreviousOne;
    procedure AnEmptyAddressIsAccepted;

    //identificador da porta / the port id
    procedure PortDefaults;
    procedure TheIdChangesWithTheAddress;
    procedure TheIdChangesWithThePortNumber;
    procedure TheIdChangesWithThePortType;
    procedure TheIdIsStableForTheSameSettings;
    procedure APortWithNoAddressIsMarkedIncomplete;
  end;

  {$IFDEF PORTUGUES}
  {:
  A mesma porta, agora falando com um equipamento de verdade: um servidor de
  teste ouvindo numa porta efemera de 127.0.0.1. E' o unico jeito de exercitar
  o que a porta de rede realmente faz - conectar, mandar, receber, perceber que
  o outro lado sumiu e voltar sozinha.
  }
  {$ELSE}
  {:
  The same port, now talking to a real device: a test server listening on an
  ephemeral port of 127.0.0.1. It is the only way to exercise what the network
  port actually does - connect, send, receive, notice the other end is gone and
  come back on its own.
  }
  {$ENDIF}

  { TTestTcpUdpPortOverAServer }

  TTestTcpUdpPortOverAServer = class(TTestCase)
  private
    FServidor:TServidorDeTeste;
    FPorta:TTCP_UDPPort;
    //: espera a porta conectar, ou desistir no prazo
    function  WaitForConnection(aPrazoMs:LongInt):Boolean;
    function  WaitForDisconnection(aPrazoMs:LongInt):Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //conexao / connecting
    procedure ConnectToTheServer;
    procedure APortNeverOpenedIsNotConnected;
    procedure WithNobodyListeningItDoesNotConnect;
    procedure ClosingThePortEndsTheConnection;

    //ida e volta / round trip
    procedure WhatTheDriverWritesReachesTheServer;
    procedure TheServerAnswerComesBackToTheDriver;
    procedure WithNoAnswerTheResultIsTimeout;

    //o equipamento some / the device goes away
    procedure ADeviceThatDisappearsDropsTheConnection;
    procedure AfterDroppingThePortComesBackOnItsOwn;

    //ciclo de vida / lifecycle
    procedure DestroyingRightAfterCreatingMustNotHang;
  end;

  {$IFDEF PORTUGUES}
  {:
  A mesma porta em datagrama. Nao ha' conexao para cair nem para refazer: o
  soquete e' ligado ao destino e os datagramas vao e voltam. O que muda de
  verdade e' que o equipamento nunca "some" - some o datagrama.
  }
  {$ELSE}
  {:
  The same port over datagrams. There is no connection to drop nor to rebuild:
  the socket is bound to the destination and datagrams go and come back. What
  really changes is that the device never "goes away" - the datagram does.
  }
  {$ENDIF}

  { TTestTcpUdpPortOnDatagram }

  TTestTcpUdpPortOnDatagram = class(TTestCase)
  private
    FServidor:TServidorUDPDeTeste;
    FPorta:TTCP_UDPPort;
    function  WaitForConnection(aPrazoMs:LongInt):Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ADatagramPortBecomesActive;
    procedure WhatTheDriverWritesReachesTheServer;
    procedure TheServerAnswerComesBackToTheDriver;
    procedure WithNoAnswerTheResultIsTimeout;
    procedure ThePortTypeSeparatesTheIdentifiers;
  end;

implementation

//Cada TTCP_UDPPort sobe uma thread de reconexao, e o destrutor espera ela
//encerrar - criar uma por teste levava a suite inteira a dezenas de segundos.
//Os metodos exercitados aqui nao guardam estado entre testes (cada um atribui
//o endereco, a porta e o tipo que precisa), entao uma instancia atende a
//classe toda.
var
  PortaCompartilhada:TTCP_UDPPort = nil;

procedure TTestTcpUdpPort.SetUp;
begin
  if PortaCompartilhada=nil then
    PortaCompartilhada:=TTCP_UDPPort.Create(nil);
  FPorta:=PortaCompartilhada;
end;

procedure TTestTcpUdpPort.TearDown;
begin
  FPorta:=nil;
end;

function TTestTcpUdpPort.IdOf(const aEndereco:String; aPorta:LongInt; aTipo:TPortType):TPortUniqueID;
begin
  FPorta.Host:=aEndereco;
  FPorta.Port:=aPorta;
  FPorta.PortType:=aTipo;
  Result:=FPorta.getPortId;
end;

function TTestTcpUdpPort.TypeByteOf(aId:TPortUniqueID):Byte;
begin
  //o byte mais significativo guarda o tipo da porta e a marca de incompleta
  Result:=(aId shr 56) and $FF;
end;

procedure TTestTcpUdpPort.ValidAddressesAreAccepted;
begin
  AssertTrue('an ordinary address',        TTCP_UDPPort.ValidIPv4('192.168.0.10'));
  AssertTrue('zeros in the middle are fine',   TTCP_UDPPort.ValidIPv4('10.0.0.1'));
  AssertTrue('255 in the middle is fine',      TTCP_UDPPort.ValidIPv4('1.255.255.1'));
  AssertTrue('per octet bounds',    TTCP_UDPPort.ValidIPv4('1.0.255.254'));
end;

procedure TTestTcpUdpPort.TheFirstOctetCannotBeZeroOr255;
begin
  //rede 0 e broadcast nao endereçam equipamento nenhum
  AssertFalse('starts with 0',   TTCP_UDPPort.ValidIPv4('0.168.0.10'));
  AssertFalse('starts with 255', TTCP_UDPPort.ValidIPv4('255.168.0.10'));
end;

procedure TTestTcpUdpPort.TheLastOctetCannotBeZeroOr255;
begin
  //.0 e' a propria rede e .255 e' o broadcast dela
  AssertFalse('ends in 0',   TTCP_UDPPort.ValidIPv4('192.168.0.0'));
  AssertFalse('ends in 255', TTCP_UDPPort.ValidIPv4('192.168.0.255'));
end;

procedure TTestTcpUdpPort.ItMustHaveFourOctets;
begin
  AssertFalse('only three',  TTCP_UDPPort.ValidIPv4('192.168.1'));
  AssertFalse('five',    TTCP_UDPPort.ValidIPv4('192.168.1.1.1'));
  AssertFalse('only one',    TTCP_UDPPort.ValidIPv4('192'));
end;

procedure TTestTcpUdpPort.AnOctetOutOfRangeIsRefused;
begin
  AssertFalse('over 255', TTCP_UDPPort.ValidIPv4('192.168.1.300'));
  AssertFalse('negative',     TTCP_UDPPort.ValidIPv4('192.168.-1.10'));
end;

procedure TTestTcpUdpPort.ANonNumericOctetIsRefused;
begin
  AssertFalse('a letter',     TTCP_UDPPort.ValidIPv4('192.168.1.a'));
  AssertFalse('empty',     TTCP_UDPPort.ValidIPv4('192.168..1'));
end;

procedure TTestTcpUdpPort.EmptyTextIsNotAValidAddress;
begin
  AssertFalse('empty', TTCP_UDPPort.ValidIPv4(''));
end;

procedure TTestTcpUdpPort.AHostNameIsRefused;
var
  recusou:Boolean;
begin
  //o componente so trabalha com IPv4 literal: nome depende de resolucao de
  //nomes, que ele nao faz
  recusou:=false;
  try
    FPorta.Host:='clp1.local';
  except
    on E:Exception do
      recusou:=true;
  end;
  AssertTrue('a host name must be refused', recusou);
end;

procedure TTestTcpUdpPort.ARefusedAddressDoesNotEraseThePreviousOne;
begin
  FPorta.Host:='192.168.0.10';
  try
    FPorta.Host:='nao.e.um.ip';
  except
    on E:Exception do ;
  end;
  AssertEquals('the previous address stands', '192.168.0.10', FPorta.Host);
end;

procedure TTestTcpUdpPort.AnEmptyAddressIsAccepted;
begin
  //porta recem criada, ou limpa, e' estado valido: so nao da' para conectar
  FPorta.Host:='192.168.0.10';
  FPorta.Host:='';
  AssertEquals('address cleared', '', FPorta.Host);
end;

procedure TTestTcpUdpPort.PortDefaults;
begin
  AssertEquals('the S7 default port', 102, FPorta.Port);
  AssertEquals('default type',        Ord(ptTCP), Ord(FPorta.PortType));
end;

procedure TTestTcpUdpPort.TheIdChangesWithTheAddress;
begin
  AssertTrue('different addresses, different ids',
             IdOf('192.168.0.10', 102, ptTCP) <> IdOf('192.168.0.11', 102, ptTCP));
end;

procedure TTestTcpUdpPort.TheIdChangesWithThePortNumber;
begin
  //mesmo equipamento, servicos diferentes
  AssertTrue('different ports, different ids',
             IdOf('192.168.0.10', 102, ptTCP) <> IdOf('192.168.0.10', 502, ptTCP));
end;

procedure TTestTcpUdpPort.TheIdChangesWithThePortType;
begin
  AssertTrue('TCP and UDP on the same address and port are distinct ports',
             IdOf('192.168.0.10', 102, ptTCP) <> IdOf('192.168.0.10', 102, ptUDP));

  AssertEquals('the TCP mark', 2, TypeByteOf(IdOf('192.168.0.10', 102, ptTCP)));
  AssertEquals('the UDP mark', 3, TypeByteOf(IdOf('192.168.0.10', 102, ptUDP)));
end;

procedure TTestTcpUdpPort.TheIdIsStableForTheSameSettings;
var
  primeiro:TPortUniqueID;
begin
  //ler duas vezes tem que dar o mesmo valor - e' chave de mapa
  primeiro:=IdOf('192.168.0.10', 102, ptTCP);
  AssertTrue('same reading', primeiro = FPorta.getPortId);
  AssertTrue('same settings', primeiro = IdOf('192.168.0.10', 102, ptTCP));
end;

procedure TTestTcpUdpPort.APortWithNoAddressIsMarkedIncomplete;
begin
  //sem endereco o id leva o bit alto ligado, distinguindo-o de qualquer porta
  //configurada de verdade
  FPorta.Host:='';
  FPorta.PortType:=ptTCP;

  AssertEquals('TCP type with the incomplete mark', $82, TypeByteOf(FPorta.getPortId));
  AssertTrue  ('and it differs from a configured port',
               FPorta.getPortId <> IdOf('192.168.0.10', 102, ptTCP));
end;

{ TTestTcpUdpPortOverAServer }

const
  DRIVER_DE_TESTE = 77;

procedure TTestTcpUdpPortOverAServer.SetUp;
begin
  FServidor:=TServidorDeTeste.Create;

  FPorta:=TTCP_UDPPort.Create(nil);
  FPorta.Host                 :='127.0.0.1';
  FPorta.Port                 :=FServidor.Porta;
  FPorta.PortType             :=ptTCP;
  FPorta.Timeout              :=300;
  FPorta.ReconnectRetryInterval:=200;
end;

procedure TTestTcpUdpPortOverAServer.TearDown;
begin
  FreeAndNil(FPorta);
  FreeAndNil(FServidor);
end;

function TTestTcpUdpPortOverAServer.WaitForConnection(aPrazoMs:LongInt):Boolean;
var
  gasto:LongInt;
begin
  gasto:=0;
  while (not FPorta.ReallyActive) and (gasto<aPrazoMs) do begin
    Sleep(5);
    inc(gasto, 5);
  end;
  Result:=FPorta.ReallyActive;
end;

function TTestTcpUdpPortOverAServer.WaitForDisconnection(aPrazoMs:LongInt):Boolean;
var
  gasto:LongInt;
begin
  gasto:=0;
  while FPorta.ReallyActive and (gasto<aPrazoMs) do begin
    Sleep(5);
    inc(gasto, 5);
  end;
  Result:=not FPorta.ReallyActive;
end;

procedure TTestTcpUdpPortOverAServer.ConnectToTheServer;
begin
  FPorta.Active:=true;

  AssertTrue('the port must connect',        WaitForConnection(3000));
  AssertTrue('and the server must see someone', FServidor.EsperarConexoes(1, 1000));
end;

procedure TTestTcpUdpPortOverAServer.APortNeverOpenedIsNotConnected;
begin
  AssertFalse('without opening there is no socket', FPorta.ReallyActive);
  AssertEquals('and the server saw nobody', 0, FServidor.Conexoes);
end;

procedure TTestTcpUdpPortOverAServer.WithNobodyListeningItDoesNotConnect;
begin
  //aponta para a porta do servidor depois de derruba-lo: nao ha quem atenda
  FreeAndNil(FServidor);

  FPorta.Active:=true;

  AssertFalse('it cannot claim to be connected', WaitForConnection(700));
end;

procedure TTestTcpUdpPortOverAServer.ClosingThePortEndsTheConnection;
begin
  FPorta.Active:=true;
  AssertTrue('connected', WaitForConnection(3000));

  FPorta.Active:=false;

  //PortStop nao fecha o soquete: posta um pedido para a thread de conexao, que
  //fecha quando chegar a vez dela. O que importa e' que feche
  AssertTrue('the connection must be closed', WaitForDisconnection(3000));
  AssertFalse('and the port stays closed',         FPorta.Active);
end;

procedure TTestTcpUdpPortOverAServer.WhatTheDriverWritesReachesTheServer;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;
  AssertTrue('connected', WaitForConnection(3000));

  FPorta.IOCommandSync(iocWrite, 4, BytesOf('01 02 03 04'), 0, DRIVER_DE_TESTE, 0, @pkg);

  AssertTrue('the bytes arrived', FServidor.EsperarBytes(4, 1000));
  AssertBytesEqual('and they are the same', BytesOf('01 02 03 04'), FServidor.Recebido);
end;

procedure TTestTcpUdpPortOverAServer.TheServerAnswerComesBackToTheDriver;
var
  pkg:TIOPacket;
begin
  FServidor.EnfileirarResposta(BytesOf('AA BB CC'));

  FPorta.Active:=true;
  AssertTrue('connected', WaitForConnection(3000));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertEquals('read ok',      Ord(iorOK), Ord(pkg.ReadIOResult));
  AssertEquals('three bytes',      3, pkg.Received);
  AssertBytesEqual('the answer',  BytesOf('AA BB CC'), pkg.BufferToRead);
end;

procedure TTestTcpUdpPortOverAServer.WithNoAnswerTheResultIsTimeout;
var
  pkg:TIOPacket;
begin
  //o servidor recebe mas nao responde nada
  FPorta.Active:=true;
  AssertTrue('connected', WaitForConnection(3000));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertEquals('the write went out',      Ord(iorOK),      Ord(pkg.WriteIOResult));
  AssertEquals('the answer did not come', Ord(iorTimeOut), Ord(pkg.ReadIOResult));
end;

procedure TTestTcpUdpPortOverAServer.ADeviceThatDisappearsDropsTheConnection;
var
  pkg:TIOPacket;
begin
  //sem reconexao automatica, para medir so' a queda
  FPorta.EnableAutoReconnect:=false;
  FPorta.Active:=true;
  AssertTrue('connected', WaitForConnection(3000));

  FServidor.SoltarAConexao;

  //o soquete so' descobre que caiu quando tenta usar
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertTrue('the port must notice it dropped', WaitForDisconnection(3000));
end;

procedure TTestTcpUdpPortOverAServer.AfterDroppingThePortComesBackOnItsOwn;
var
  pkg:TIOPacket;
begin
  //e' o que mantem a supervisao viva quando o equipamento reinicia
  FPorta.Active:=true;
  AssertTrue('connected', WaitForConnection(3000));

  FServidor.SoltarAConexao;
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);
  AssertTrue('dropped', WaitForDisconnection(3000));

  AssertTrue('and it came back on its own',      WaitForConnection(5000));
  AssertTrue('with a new connection',  FServidor.EsperarConexoes(2, 1000));
end;

procedure TTestTcpUdpPortOverAServer.DestroyingRightAfterCreatingMustNotHang;
var
  c:LongInt;
  porta:TTCP_UDPPort;
begin
  //criar e destruir em sequencia, que e' o que um programa faz ao reconfigurar
  //a comunicacao
  for c:=1 to 20 do begin
    porta:=TTCP_UDPPort.Create(nil);
    porta.Host:='127.0.0.1';
    porta.Port:=59000;
    porta.Free;
  end;

  AssertTrue('twenty ports created and destroyed in a row', true);
end;

{ TTestTcpUdpPortOnDatagram }

procedure TTestTcpUdpPortOnDatagram.SetUp;
begin
  FServidor:=TServidorUDPDeTeste.Create;

  FPorta:=TTCP_UDPPort.Create(nil);
  FPorta.Host                 :='127.0.0.1';
  FPorta.Port                 :=FServidor.Porta;
  FPorta.PortType             :=ptUDP;
  FPorta.Timeout              :=300;
  FPorta.ReconnectRetryInterval:=200;
end;

procedure TTestTcpUdpPortOnDatagram.TearDown;
begin
  FreeAndNil(FPorta);
  FreeAndNil(FServidor);
end;

function TTestTcpUdpPortOnDatagram.WaitForConnection(aPrazoMs:LongInt):Boolean;
var
  gasto:LongInt;
begin
  gasto:=0;
  while (not FPorta.ReallyActive) and (gasto<aPrazoMs) do begin
    Sleep(5);
    inc(gasto, 5);
  end;
  Result:=FPorta.ReallyActive;
end;

procedure TTestTcpUdpPortOnDatagram.ADatagramPortBecomesActive;
begin
  FPorta.Active:=true;
  AssertTrue('the datagram port must become active', WaitForConnection(3000));
end;

procedure TTestTcpUdpPortOnDatagram.WhatTheDriverWritesReachesTheServer;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;
  AssertTrue('active', WaitForConnection(3000));

  FPorta.IOCommandSync(iocWrite, 4, BytesOf('01 02 03 04'), 0, DRIVER_DE_TESTE, 0, @pkg);

  AssertTrue('the datagram arrived', FServidor.EsperarBytes(4, 1000));
  AssertBytesEqual('and it is the same one', BytesOf('01 02 03 04'), FServidor.Recebido);
end;

procedure TTestTcpUdpPortOnDatagram.TheServerAnswerComesBackToTheDriver;
var
  pkg:TIOPacket;
begin
  FServidor.EnfileirarResposta(BytesOf('AA BB CC'));

  FPorta.Active:=true;
  AssertTrue('active', WaitForConnection(3000));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertEquals('read ok',     Ord(iorOK), Ord(pkg.ReadIOResult));
  AssertEquals('three bytes',     3, pkg.Received);
  AssertBytesEqual('the answer', BytesOf('AA BB CC'), pkg.BufferToRead);
end;

procedure TTestTcpUdpPortOnDatagram.WithNoAnswerTheResultIsTimeout;
var
  pkg:TIOPacket;
begin
  //em datagrama a perda e' o caso comum, nao a excecao
  FPorta.Active:=true;
  AssertTrue('active', WaitForConnection(3000));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertEquals('the write went out',      Ord(iorOK),      Ord(pkg.WriteIOResult));
  AssertEquals('the answer did not come', Ord(iorTimeOut), Ord(pkg.ReadIOResult));
end;

procedure TTestTcpUdpPortOnDatagram.ThePortTypeSeparatesTheIdentifiers;
var
  emTcp:TPortUniqueID;
begin
  //mesmo endereco e mesma porta, protocolos diferentes: sao dois destinos
  FPorta.PortType:=ptTCP;
  emTcp:=FPorta.getPortId;

  FPorta.PortType:=ptUDP;
  AssertTrue('TCP and UDP to the same destination are distinct ports', emTcp<>FPorta.getPortId);
end;

initialization
  RegisterTest(TTestTcpUdpPort);
  RegisterTest(TTestTcpUdpPortOverAServer);
  RegisterTest(TTestTcpUdpPortOnDatagram);

finalization
  FreeAndNil(PortaCompartilhada);

end.
