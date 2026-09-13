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
    function  IdDe(const aEndereco:String; aPorta:LongInt; aTipo:TPortType):TPortUniqueID;
    function  ByteDeTipo(aId:TPortUniqueID):Byte;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //validacao de endereco / address validation
    procedure EnderecosValidosSaoAceitos;
    procedure PrimeiroOctetoNaoPodeSerZeroNem255;
    procedure UltimoOctetoNaoPodeSerZeroNem255;
    procedure PrecisaTerQuatroOctetos;
    procedure OctetoForaDaFaixaEhRecusado;
    procedure OctetoNaoNumericoEhRecusado;
    procedure TextoVazioNaoEhEnderecoValido;

    //atribuicao do endereco / assigning the address
    procedure NomeDeHostEhRecusado;
    procedure EnderecoRecusadoNaoApagaOAnterior;
    procedure EnderecoVazioEhAceito;

    //identificador da porta / the port id
    procedure PadroesDaPorta;
    procedure IdMudaComOEndereco;
    procedure IdMudaComONumeroDaPorta;
    procedure IdMudaComOTipoDePorta;
    procedure IdEhEstavelParaAMesmaConfiguracao;
    procedure PortaSemEnderecoEhMarcadaComoIncompleta;
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

  { TTestTcpUdpPortComServidor }

  TTestTcpUdpPortComServidor = class(TTestCase)
  private
    FServidor:TServidorDeTeste;
    FPorta:TTCP_UDPPort;
    //: espera a porta conectar, ou desistir no prazo
    function  EsperarConexao(aPrazoMs:LongInt):Boolean;
    function  EsperarDesconexao(aPrazoMs:LongInt):Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //conexao / connecting
    procedure ConectaNoServidor;
    procedure PortaNuncaAbertaNaoEstaConectada;
    procedure SemNinguemOuvindoNaoConecta;
    procedure FecharAPortaEncerraAConexao;

    //ida e volta / round trip
    procedure OQueOMotoristaEscreveChegaNoServidor;
    procedure ARespostaDoServidorVoltaParaOMotorista;
    procedure SemRespostaOResultadoEhTimeout;

    //o equipamento some / the device goes away
    procedure EquipamentoQueSomeDerrubaAConexao;
    procedure DepoisDeCairAPortaVoltaSozinha;

    //ciclo de vida / lifecycle
    procedure DestruirLogoDepoisDeCriarNaoPodeTravar;
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

function TTestTcpUdpPort.IdDe(const aEndereco:String; aPorta:LongInt; aTipo:TPortType):TPortUniqueID;
begin
  FPorta.Host:=aEndereco;
  FPorta.Port:=aPorta;
  FPorta.PortType:=aTipo;
  Result:=FPorta.getPortId;
end;

function TTestTcpUdpPort.ByteDeTipo(aId:TPortUniqueID):Byte;
begin
  //o byte mais significativo guarda o tipo da porta e a marca de incompleta
  Result:=(aId shr 56) and $FF;
end;

procedure TTestTcpUdpPort.EnderecosValidosSaoAceitos;
begin
  AssertTrue('endereco comum',        TTCP_UDPPort.ValidIPv4('192.168.0.10'));
  AssertTrue('zeros no meio valem',   TTCP_UDPPort.ValidIPv4('10.0.0.1'));
  AssertTrue('255 no meio vale',      TTCP_UDPPort.ValidIPv4('1.255.255.1'));
  AssertTrue('limites por octeto',    TTCP_UDPPort.ValidIPv4('1.0.255.254'));
end;

procedure TTestTcpUdpPort.PrimeiroOctetoNaoPodeSerZeroNem255;
begin
  //rede 0 e broadcast nao endereçam equipamento nenhum
  AssertFalse('comeca com 0',   TTCP_UDPPort.ValidIPv4('0.168.0.10'));
  AssertFalse('comeca com 255', TTCP_UDPPort.ValidIPv4('255.168.0.10'));
end;

procedure TTestTcpUdpPort.UltimoOctetoNaoPodeSerZeroNem255;
begin
  //.0 e' a propria rede e .255 e' o broadcast dela
  AssertFalse('termina em 0',   TTCP_UDPPort.ValidIPv4('192.168.0.0'));
  AssertFalse('termina em 255', TTCP_UDPPort.ValidIPv4('192.168.0.255'));
end;

procedure TTestTcpUdpPort.PrecisaTerQuatroOctetos;
begin
  AssertFalse('so tres',  TTCP_UDPPort.ValidIPv4('192.168.1'));
  AssertFalse('cinco',    TTCP_UDPPort.ValidIPv4('192.168.1.1.1'));
  AssertFalse('um so',    TTCP_UDPPort.ValidIPv4('192'));
end;

procedure TTestTcpUdpPort.OctetoForaDaFaixaEhRecusado;
begin
  AssertFalse('acima de 255', TTCP_UDPPort.ValidIPv4('192.168.1.300'));
  AssertFalse('negativo',     TTCP_UDPPort.ValidIPv4('192.168.-1.10'));
end;

procedure TTestTcpUdpPort.OctetoNaoNumericoEhRecusado;
begin
  AssertFalse('letra',     TTCP_UDPPort.ValidIPv4('192.168.1.a'));
  AssertFalse('vazio',     TTCP_UDPPort.ValidIPv4('192.168..1'));
end;

procedure TTestTcpUdpPort.TextoVazioNaoEhEnderecoValido;
begin
  AssertFalse('vazio', TTCP_UDPPort.ValidIPv4(''));
end;

procedure TTestTcpUdpPort.NomeDeHostEhRecusado;
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
  AssertTrue('nome de host deve ser recusado', recusou);
end;

procedure TTestTcpUdpPort.EnderecoRecusadoNaoApagaOAnterior;
begin
  FPorta.Host:='192.168.0.10';
  try
    FPorta.Host:='nao.e.um.ip';
  except
    on E:Exception do ;
  end;
  AssertEquals('o endereco anterior fica de pe', '192.168.0.10', FPorta.Host);
end;

procedure TTestTcpUdpPort.EnderecoVazioEhAceito;
begin
  //porta recem criada, ou limpa, e' estado valido: so nao da' para conectar
  FPorta.Host:='192.168.0.10';
  FPorta.Host:='';
  AssertEquals('endereco limpo', '', FPorta.Host);
end;

procedure TTestTcpUdpPort.PadroesDaPorta;
begin
  AssertEquals('porta padrao do S7', 102, FPorta.Port);
  AssertEquals('tipo padrao',        Ord(ptTCP), Ord(FPorta.PortType));
end;

procedure TTestTcpUdpPort.IdMudaComOEndereco;
begin
  AssertTrue('enderecos diferentes, ids diferentes',
             IdDe('192.168.0.10', 102, ptTCP) <> IdDe('192.168.0.11', 102, ptTCP));
end;

procedure TTestTcpUdpPort.IdMudaComONumeroDaPorta;
begin
  //mesmo equipamento, servicos diferentes
  AssertTrue('portas diferentes, ids diferentes',
             IdDe('192.168.0.10', 102, ptTCP) <> IdDe('192.168.0.10', 502, ptTCP));
end;

procedure TTestTcpUdpPort.IdMudaComOTipoDePorta;
begin
  AssertTrue('TCP e UDP no mesmo endereco e porta sao portas distintas',
             IdDe('192.168.0.10', 102, ptTCP) <> IdDe('192.168.0.10', 102, ptUDP));

  AssertEquals('marca do TCP', 2, ByteDeTipo(IdDe('192.168.0.10', 102, ptTCP)));
  AssertEquals('marca do UDP', 3, ByteDeTipo(IdDe('192.168.0.10', 102, ptUDP)));
end;

procedure TTestTcpUdpPort.IdEhEstavelParaAMesmaConfiguracao;
var
  primeiro:TPortUniqueID;
begin
  //ler duas vezes tem que dar o mesmo valor - e' chave de mapa
  primeiro:=IdDe('192.168.0.10', 102, ptTCP);
  AssertTrue('mesma leitura', primeiro = FPorta.getPortId);
  AssertTrue('mesma configuracao', primeiro = IdDe('192.168.0.10', 102, ptTCP));
end;

procedure TTestTcpUdpPort.PortaSemEnderecoEhMarcadaComoIncompleta;
begin
  //sem endereco o id leva o bit alto ligado, distinguindo-o de qualquer porta
  //configurada de verdade
  FPorta.Host:='';
  FPorta.PortType:=ptTCP;

  AssertEquals('tipo TCP com a marca de incompleta', $82, ByteDeTipo(FPorta.getPortId));
  AssertTrue  ('e difere de uma porta configurada',
               FPorta.getPortId <> IdDe('192.168.0.10', 102, ptTCP));
end;

{ TTestTcpUdpPortComServidor }

const
  DRIVER_DE_TESTE = 77;

procedure TTestTcpUdpPortComServidor.SetUp;
begin
  FServidor:=TServidorDeTeste.Create;

  FPorta:=TTCP_UDPPort.Create(nil);
  FPorta.Host                 :='127.0.0.1';
  FPorta.Port                 :=FServidor.Porta;
  FPorta.PortType             :=ptTCP;
  FPorta.Timeout              :=300;
  FPorta.ReconnectRetryInterval:=200;
end;

procedure TTestTcpUdpPortComServidor.TearDown;
begin
  FreeAndNil(FPorta);
  FreeAndNil(FServidor);
end;

function TTestTcpUdpPortComServidor.EsperarConexao(aPrazoMs:LongInt):Boolean;
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

function TTestTcpUdpPortComServidor.EsperarDesconexao(aPrazoMs:LongInt):Boolean;
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

procedure TTestTcpUdpPortComServidor.ConectaNoServidor;
begin
  FPorta.Active:=true;

  AssertTrue('a porta tem que conectar',        EsperarConexao(3000));
  AssertTrue('e o servidor tem que ver alguem', FServidor.EsperarConexoes(1, 1000));
end;

procedure TTestTcpUdpPortComServidor.PortaNuncaAbertaNaoEstaConectada;
begin
  AssertFalse('sem abrir, nao ha soquete', FPorta.ReallyActive);
  AssertEquals('e o servidor nao viu ninguem', 0, FServidor.Conexoes);
end;

procedure TTestTcpUdpPortComServidor.SemNinguemOuvindoNaoConecta;
begin
  //aponta para a porta do servidor depois de derruba-lo: nao ha quem atenda
  FreeAndNil(FServidor);

  FPorta.Active:=true;

  AssertFalse('nao pode se dizer conectada', EsperarConexao(700));
end;

procedure TTestTcpUdpPortComServidor.FecharAPortaEncerraAConexao;
begin
  FPorta.Active:=true;
  AssertTrue('conectou', EsperarConexao(3000));

  FPorta.Active:=false;

  //PortStop nao fecha o soquete: posta um pedido para a thread de conexao, que
  //fecha quando chegar a vez dela. O que importa e' que feche
  AssertTrue('a conexao tem que ser encerrada', EsperarDesconexao(3000));
  AssertFalse('e a porta fica fechada',         FPorta.Active);
end;

procedure TTestTcpUdpPortComServidor.OQueOMotoristaEscreveChegaNoServidor;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;
  AssertTrue('conectou', EsperarConexao(3000));

  FPorta.IOCommandSync(iocWrite, 4, BytesOf('01 02 03 04'), 0, DRIVER_DE_TESTE, 0, @pkg);

  AssertTrue('os bytes chegaram', FServidor.EsperarBytes(4, 1000));
  AssertBytesEqual('e sao os mesmos', BytesOf('01 02 03 04'), FServidor.Recebido);
end;

procedure TTestTcpUdpPortComServidor.ARespostaDoServidorVoltaParaOMotorista;
var
  pkg:TIOPacket;
begin
  FServidor.EnfileirarResposta(BytesOf('AA BB CC'));

  FPorta.Active:=true;
  AssertTrue('conectou', EsperarConexao(3000));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertEquals('leitura ok',      Ord(iorOK), Ord(pkg.ReadIOResult));
  AssertEquals('tres bytes',      3, pkg.Received);
  AssertBytesEqual('a resposta',  BytesOf('AA BB CC'), pkg.BufferToRead);
end;

procedure TTestTcpUdpPortComServidor.SemRespostaOResultadoEhTimeout;
var
  pkg:TIOPacket;
begin
  //o servidor recebe mas nao responde nada
  FPorta.Active:=true;
  AssertTrue('conectou', EsperarConexao(3000));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertEquals('a escrita saiu',      Ord(iorOK),      Ord(pkg.WriteIOResult));
  AssertEquals('a resposta nao veio', Ord(iorTimeOut), Ord(pkg.ReadIOResult));
end;

procedure TTestTcpUdpPortComServidor.EquipamentoQueSomeDerrubaAConexao;
var
  pkg:TIOPacket;
begin
  //sem reconexao automatica, para medir so' a queda
  FPorta.EnableAutoReconnect:=false;
  FPorta.Active:=true;
  AssertTrue('conectou', EsperarConexao(3000));

  FServidor.SoltarAConexao;

  //o soquete so' descobre que caiu quando tenta usar
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);

  AssertTrue('a porta tem que perceber que caiu', EsperarDesconexao(3000));
end;

procedure TTestTcpUdpPortComServidor.DepoisDeCairAPortaVoltaSozinha;
var
  pkg:TIOPacket;
begin
  //e' o que mantem a supervisao viva quando o equipamento reinicia
  FPorta.Active:=true;
  AssertTrue('conectou', EsperarConexao(3000));

  FServidor.SoltarAConexao;
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_DE_TESTE, 0, @pkg);
  AssertTrue('caiu', EsperarDesconexao(3000));

  AssertTrue('e voltou sozinha',      EsperarConexao(5000));
  AssertTrue('com uma nova conexao',  FServidor.EsperarConexoes(2, 1000));
end;

procedure TTestTcpUdpPortComServidor.DestruirLogoDepoisDeCriarNaoPodeTravar;
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

  AssertTrue('vinte portas criadas e destruidas em sequencia', true);
end;

initialization
  RegisterTest(TTestTcpUdpPort);
  RegisterTest(TTestTcpUdpPortComServidor);

finalization
  FreeAndNil(PortaCompartilhada);

end.
