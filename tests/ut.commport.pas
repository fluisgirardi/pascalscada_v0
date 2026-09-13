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
    procedure ContarErroDeLeitura(Error:TIOResult);
    procedure ContarErroDeEscrita(Error:TIOResult);
    procedure ContarDesconexao(Sender:TObject);
    function  ConteudoDoLog:String;
    procedure ContarAberta(Sender:TObject);
    procedure ContarErroAoAbrir(Sender:TObject);
    procedure ContarFechada(Sender:TObject);
    procedure ContarErroAoFechar(Sender:TObject);
    procedure ContarComeco(Sender:TObject);
    procedure ContarFim(Sender:TObject);
    procedure LigarOsContadores;
    //: recebe o pacote pronto na sobrecarga que devolve por chamada de volta
    procedure GuardarPacote(var aPacote:TIOPacket);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //travamento entre drivers / locking between drivers
    procedure PortaNovaNaoEstaTravada;
    procedure TravarMarcaODono;
    procedure OutroDriverNaoConsegueTravar;
    procedure TravarDuasVezesNaoEmpilha;
    procedure DestravarComOutroIdNaoDestrava;
    procedure DestravarComOIdCertoLibera;
    procedure DestravarPortaLivreNaoReclama;

    //abrir e fechar / opening and closing
    procedure PortaNovaEstaFechada;
    procedure AbrirEFechar;
    procedure AberturaQueFalhaNaoDeixaAPortaAtiva;
    procedure FechamentoQueFalhaDeixaAPortaAtiva;

    //avisos / notifications
    procedure AbrirAvisaQuemEscuta;
    procedure FecharAvisaQuemEscuta;
    procedure AberturaQueFalhaAvisaOErro;
    procedure FechamentoQueFalhaAvisaOErro;

    //comandos de entrada e saida / io commands
    procedure ComandoComAPortaFechadaDevolveZero;
    procedure EscreveELeNumaChamadaSo;
    procedure OIdDoComandoCresceACadaChamada;
    procedure BytesEscritosELidosSaoContados;
    procedure SemRespostaOResultadoDeLeituraEhTimeout;
    procedure ComandoComAPortaFechadaNaoTocaNosBuffers;

    //os avisos de operacao demorada / the long operation notifications
    procedure OperacaoDemoradaAvisaComecoEFim;
    procedure OperacaoDemoradaComAPortaFechadaNaoDesequilibra;

    //a sobrecarga que devolve por chamada de volta / the callback overload
    procedure ChamadaDeVoltaRecebeOPacotePronto;
    procedure ChamadaDeVoltaNaoAconteceComAPortaFechada;
    procedure ChamadaDeVoltaComAPortaFechadaNaoDesequilibra;

    //drivers pendurados na porta / drivers attached to the port
    procedure DriverPenduradoSeRegistraNaPorta;
    procedure DriverPenduradoDuasVezesNaoDuplica;
    procedure TrocarDePortaTiraDaAnterior;
    procedure PortaDestruidaSoltaOUnicoDriver;
    procedure PortaDestruidaSoltaOsDoisDrivers;
    procedure PortaDestruidaSoltaTodosOsDrivers;

    //contadores de trafego / traffic counters
    procedure ContadoresComecamZerados;
    procedure BytesSaoAcumuladosEntreComandos;
    procedure ComandoQueNaoAconteceNaoContaBytes;

    //ultimo quadro, para diagnostico / last frame, for diagnostics
    procedure UltimoQuadroFicaGuardadoEmHexa;
    procedure UltimoQuadroNaoMisturaComandosAnteriores;
    procedure UltimoQuadroEnviadoAcompanhaAEscritaPura;

    //limpeza de buffers / buffer clearing
    procedure LimparBuffersChegaNaPortaConcreta;
    procedure LimparNaFalhaVemLigadoDeFabrica;

    //erros de entrada e saida / io errors
    procedure ErroDeLeituraAvisaQuemEscuta;
    procedure ErroDeEscritaAvisaQuemEscuta;
    procedure DesconexaoAvisaQuemEscuta;

    //em tempo de projeto / at design time
    procedure PortaExclusivaNaoAbreEmTempoDeProjeto;
    procedure PortaNaoExclusivaAbreEmTempoDeProjeto;

    //registro do trafego em arquivo / traffic log file
    procedure LigarORegistroCriaOArquivo;
    procedure CadaComandoDeixaUmaLinhaNoRegistro;
    procedure RegistroDesligadoNaoEscreveNada;
    procedure TrocarDeArquivoComORegistroLigado;
    procedure PortaDestruidaComORegistroLigadoFechaOArquivo;
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

procedure TTestCommPort.ContarAberta(Sender:TObject);      begin inc(FAberta);      end;
procedure TTestCommPort.ContarErroAoAbrir(Sender:TObject); begin inc(FErroAoAbrir); end;
procedure TTestCommPort.ContarFechada(Sender:TObject);     begin inc(FFechada);     end;
procedure TTestCommPort.ContarErroAoFechar(Sender:TObject);begin inc(FErroAoFechar);end;
procedure TTestCommPort.ContarComeco(Sender:TObject);      begin inc(FComecos);     end;
procedure TTestCommPort.ContarFim(Sender:TObject);         begin inc(FFins);        end;

procedure TTestCommPort.GuardarPacote(var aPacote:TIOPacket);
begin
  inc(FChamadas);
  FPacoteRecebido:=aPacote;
end;

procedure TTestCommPort.LigarOsContadores;
begin
  FPorta.OnCommPortOpened    :=@ContarAberta;
  FPorta.OnCommPortOpenError :=@ContarErroAoAbrir;
  FPorta.OnCommPortClosed    :=@ContarFechada;
  FPorta.OnCommPortCloseError:=@ContarErroAoFechar;
end;

procedure TTestCommPort.PortaNovaNaoEstaTravada;
begin
  AssertFalse('porta recem criada', FPorta.Locked);
  AssertEquals('sem dono', 0, FPorta.LockedBy);
end;

procedure TTestCommPort.TravarMarcaODono;
begin
  AssertTrue ('travou',      FPorta.Lock(DRIVER_A));
  AssertTrue ('esta travada', FPorta.Locked);
  AssertEquals('dono',        DRIVER_A, FPorta.LockedBy);
end;

procedure TTestCommPort.OutroDriverNaoConsegueTravar;
begin
  //e' disso que depende nao embaralhar quadros de dois protocolos na mesma
  //porta
  FPorta.Lock(DRIVER_A);
  AssertFalse('o segundo driver nao pode travar', FPorta.Lock(DRIVER_B));
  AssertEquals('o dono continua o primeiro', DRIVER_A, FPorta.LockedBy);
end;

procedure TTestCommPort.TravarDuasVezesNaoEmpilha;
begin
  //travar nao e' reentrante: quem ja' travou e pede de novo recebe nao
  FPorta.Lock(DRIVER_A);
  AssertFalse('segunda trava do mesmo dono', FPorta.Lock(DRIVER_A));
end;

procedure TTestCommPort.DestravarComOutroIdNaoDestrava;
begin
  FPorta.Lock(DRIVER_A);
  AssertFalse('outro driver destravando', FPorta.Unlock(DRIVER_B));
  AssertTrue ('continua travada',         FPorta.Locked);
end;

procedure TTestCommPort.DestravarComOIdCertoLibera;
begin
  FPorta.Lock(DRIVER_A);
  AssertTrue ('destravou',        FPorta.Unlock(DRIVER_A));
  AssertFalse('nao esta travada', FPorta.Locked);
  AssertTrue ('e outro ja pode travar', FPorta.Lock(DRIVER_B));
end;

procedure TTestCommPort.DestravarPortaLivreNaoReclama;
begin
  AssertTrue('destravar porta livre', FPorta.Unlock(DRIVER_A));
end;

procedure TTestCommPort.PortaNovaEstaFechada;
begin
  AssertFalse('Active',       FPorta.Active);
  AssertFalse('ReallyActive', FPorta.ReallyActive);
end;

procedure TTestCommPort.AbrirEFechar;
begin
  FPorta.Active:=true;
  AssertTrue('aberta', FPorta.Active);
  AssertTrue('de fato aberta', FPorta.ReallyActive);

  FPorta.Active:=false;
  AssertFalse('fechada', FPorta.Active);
end;

procedure TTestCommPort.AberturaQueFalhaNaoDeixaAPortaAtiva;
begin
  //dispositivo ausente: a porta nao pode ficar se dizendo aberta
  FPorta.FalharAoAbrir:=true;
  FPorta.Active:=true;

  AssertFalse('Active',       FPorta.Active);
  AssertFalse('ReallyActive', FPorta.ReallyActive);
end;

procedure TTestCommPort.FechamentoQueFalhaDeixaAPortaAtiva;
begin
  FPorta.Active:=true;
  FPorta.FalharAoFechar:=true;
  FPorta.Active:=false;

  AssertTrue('fechar falhou, entao continua aberta', FPorta.Active);
end;

procedure TTestCommPort.AbrirAvisaQuemEscuta;
begin
  LigarOsContadores;
  FPorta.Active:=true;

  AssertEquals('avisou que abriu', 1, FAberta);
  AssertEquals('sem aviso de erro', 0, FErroAoAbrir);
end;

procedure TTestCommPort.FecharAvisaQuemEscuta;
begin
  FPorta.Active:=true;
  LigarOsContadores;
  FPorta.Active:=false;

  AssertEquals('avisou que fechou', 1, FFechada);
  AssertEquals('sem aviso de erro', 0, FErroAoFechar);
end;

procedure TTestCommPort.AberturaQueFalhaAvisaOErro;
begin
  LigarOsContadores;
  FPorta.FalharAoAbrir:=true;
  FPorta.Active:=true;

  AssertEquals('avisou o erro',      1, FErroAoAbrir);
  AssertEquals('e nao disse que abriu', 0, FAberta);
end;

procedure TTestCommPort.FechamentoQueFalhaAvisaOErro;
begin
  FPorta.Active:=true;
  LigarOsContadores;
  FPorta.FalharAoFechar:=true;
  FPorta.Active:=false;

  AssertEquals('avisou o erro',        1, FErroAoFechar);
  AssertEquals('e nao disse que fechou', 0, FFechada);
end;

procedure TTestCommPort.ComandoComAPortaFechadaDevolveZero;
var
  pkg:TIOPacket;
begin
  //zero e' o "nao fiz nada" que os drivers conferem antes de olhar o pacote
  AssertEquals('porta fechada', 0,
               FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 4,
                                    DRIVER_A, 0, @pkg));
end;

procedure TTestCommPort.EscreveELeNumaChamadaSo;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB CC DD'));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 4, DRIVER_A, 0, @pkg);

  AssertBytesEqual('o que foi escrito', BytesOf('01 02'), FPorta.LastWrittenFrame);
  AssertBytesEqual('o que foi lido',    BytesOf('AA BB CC DD'), pkg.BufferToRead);
end;

procedure TTestCommPort.OIdDoComandoCresceACadaChamada;
var
  pkg:TIOPacket;
  primeiro, segundo:Cardinal;
begin
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA'));
  FPorta.QueueResponse(BytesOf('BB'));

  primeiro:=FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 1, DRIVER_A, 0, @pkg);
  segundo :=FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 1, DRIVER_A, 0, @pkg);

  AssertTrue('o primeiro id nao pode ser zero', primeiro<>0);
  AssertTrue('cada comando tem o seu id',       segundo>primeiro);
end;

procedure TTestCommPort.BytesEscritosELidosSaoContados;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB CC'));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_A, 0, @pkg);

  AssertEquals('pedidos para escrever', 2, pkg.ToWrite);
  AssertEquals('escritos',              2, pkg.Written);
  AssertEquals('pedidos para ler',      3, pkg.ToRead);
  AssertEquals('lidos',                 3, pkg.Received);
  AssertEquals('escrita ok', Ord(iorOK), Ord(pkg.WriteIOResult));
  AssertEquals('leitura ok', Ord(iorOK), Ord(pkg.ReadIOResult));
end;

procedure TTestCommPort.SemRespostaOResultadoDeLeituraEhTimeout;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;

  FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 4, DRIVER_A, 0, @pkg);

  AssertEquals('a escrita saiu',    Ord(iorOK),      Ord(pkg.WriteIOResult));
  AssertEquals('a resposta nao veio', Ord(iorTimeOut), Ord(pkg.ReadIOResult));
  AssertEquals('nada lido', 0, pkg.Received);
end;

procedure TTestCommPort.ComandoComAPortaFechadaNaoTocaNosBuffers;
var
  pkg:TIOPacket;
begin
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 4, DRIVER_A, 0, @pkg);

  AssertEquals('nada escrito na porta', 0, FPorta.WriteCount);
  AssertEquals('escrita sem resultado', Ord(iorNone), Ord(pkg.WriteIOResult));
  AssertEquals('leitura sem resultado', Ord(iorNone), Ord(pkg.ReadIOResult));
end;

procedure TTestCommPort.OperacaoDemoradaAvisaComecoEFim;
var
  pkg:TIOPacket;
begin
  //os drivers usam este par para soltar e retomar a sua secao critica
  //enquanto a porta faz a parte lenta
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA'));

  FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 1, DRIVER_A, 0, @pkg,
                       @ContarComeco, @ContarFim);

  AssertEquals('avisou o comeco', 1, FComecos);
  AssertEquals('avisou o fim',    1, FFins);
end;

procedure TTestCommPort.OperacaoDemoradaComAPortaFechadaNaoDesequilibra;
var
  pkg:TIOPacket;
begin
  //o par tem que fechar sempre. Quem o usa solta uma secao critica no comeco e
  //a retoma no fim: um comeco sem fim deixa a secao solta para sempre
  FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('01'), 1, DRIVER_A, 0, @pkg,
                       @ContarComeco, @ContarFim);

  AssertEquals('um comeco tem que ter um fim', FComecos, FFins);
end;

procedure TTestCommPort.ChamadaDeVoltaRecebeOPacotePronto;
begin
  //a outra sobrecarga nao devolve o pacote pelo parametro: entrega por chamada
  //de volta, com tudo ja' preenchido
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));

  FPorta.IOCommandSync(iocWriteRead, BytesOf('01 02'), 2, 2, DRIVER_A, 0,
                       @GuardarPacote, nil, nil);

  AssertEquals('chamou uma vez', 1, FChamadas);
  AssertBytesEqual('o que foi lido', BytesOf('AA BB'), FPacoteRecebido.BufferToRead);
  AssertEquals('leitura ok', Ord(iorOK), Ord(FPacoteRecebido.ReadIOResult));
end;

procedure TTestCommPort.ChamadaDeVoltaNaoAconteceComAPortaFechada;
begin
  FPorta.IOCommandSync(iocWriteRead, BytesOf('01 02'), 2, 2, DRIVER_A, 0,
                       @GuardarPacote, nil, nil);

  AssertEquals('porta fechada, nada a entregar', 0, FChamadas);
end;

procedure TTestCommPort.ChamadaDeVoltaComAPortaFechadaNaoDesequilibra;
begin
  FPorta.IOCommandSync(iocWriteRead, BytesOf('01 02'), 2, 2, DRIVER_A, 0,
                       @GuardarPacote, nil, nil, @ContarComeco, @ContarFim);

  AssertEquals('um comeco tem que ter um fim', FComecos, FFins);
end;

procedure TTestCommPort.DriverPenduradoSeRegistraNaPorta;
var
  drv:TFakeProtocolDriver;
begin
  drv:=TFakeProtocolDriver.Create(nil);
  try
    drv.CommunicationPort:=FPorta;
    AssertTrue('o driver ficou com a porta', drv.CommunicationPort=FPorta);
  finally
    drv.Free;
  end;
end;

procedure TTestCommPort.DriverPenduradoDuasVezesNaoDuplica;
var
  drv:TFakeProtocolDriver;
begin
  //atribuir a mesma porta de novo nao pode criar um segundo registro
  drv:=TFakeProtocolDriver.Create(nil);
  try
    drv.CommunicationPort:=FPorta;
    drv.CommunicationPort:=FPorta;
    AssertTrue('continua na mesma porta', drv.CommunicationPort=FPorta);
  finally
    drv.Free;
  end;
end;

procedure TTestCommPort.TrocarDePortaTiraDaAnterior;
var
  drv:TFakeProtocolDriver;
  outra:TFakeCommPort;
begin
  outra:=TFakeCommPort.Create(nil);
  drv  :=TFakeProtocolDriver.Create(nil);
  try
    drv.CommunicationPort:=FPorta;
    drv.CommunicationPort:=outra;
    AssertTrue('trocou de porta', drv.CommunicationPort=outra);

    //a porta antiga nao pode mais achar que tem este driver: destrui-la nao
    //pode mexer em quem ja' saiu
    FreeAndNil(FPorta);
    AssertTrue('a porta antiga saiu sem levar o driver', drv.CommunicationPort=outra);
  finally
    drv.Free;
    outra.Free;
  end;
end;

procedure TTestCommPort.PortaDestruidaSoltaOUnicoDriver;
var
  drv:TFakeProtocolDriver;
begin
  drv:=TFakeProtocolDriver.Create(nil);
  try
    drv.CommunicationPort:=FPorta;
    FreeAndNil(FPorta);

    AssertTrue('o driver tem que ter largado a porta destruida',
               drv.CommunicationPort=nil);
  finally
    drv.Free;
  end;
end;

procedure TTestCommPort.PortaDestruidaSoltaOsDoisDrivers;
var
  a, b:TFakeProtocolDriver;
begin
  a:=TFakeProtocolDriver.Create(nil);
  b:=TFakeProtocolDriver.Create(nil);
  try
    a.CommunicationPort:=FPorta;
    b.CommunicationPort:=FPorta;

    FreeAndNil(FPorta);

    AssertTrue('primeiro driver', a.CommunicationPort=nil);
    AssertTrue('segundo driver',  b.CommunicationPort=nil);
  finally
    a.Free;
    b.Free;
  end;
end;

procedure TTestCommPort.PortaDestruidaSoltaTodosOsDrivers;
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

    AssertTrue('primeiro driver',  a.CommunicationPort=nil);
    AssertTrue('segundo driver',   b.CommunicationPort=nil);
    AssertTrue('terceiro driver',  c.CommunicationPort=nil);
  finally
    a.Free;
    b.Free;
    c.Free;
  end;
end;

procedure TTestCommPort.ContarErroDeLeitura(Error:TIOResult);
begin
  inc(FErrosDeLeitura);
  FUltimoErro:=Error;
end;

procedure TTestCommPort.ContarErroDeEscrita(Error:TIOResult);
begin
  inc(FErrosDeEscrita);
  FUltimoErro:=Error;
end;

procedure TTestCommPort.ContarDesconexao(Sender:TObject);
begin
  inc(FDesconexoes);
end;

function TTestCommPort.ConteudoDoLog:String;
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

procedure TTestCommPort.ContadoresComecamZerados;
begin
  AssertEquals('recebidos',   0, FPorta.RXBytes);
  AssertEquals('transmitidos', 0, FPorta.TXBytes);
end;

procedure TTestCommPort.BytesSaoAcumuladosEntreComandos;
var
  pkg:TIOPacket;
begin
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB CC'));
  FPorta.QueueResponse(BytesOf('DD'));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_A, 0, @pkg);
  AssertEquals('depois do primeiro, transmitidos', 2, FPorta.TXBytes);
  AssertEquals('depois do primeiro, recebidos',    3, FPorta.RXBytes);

  FPorta.IOCommandSync(iocWriteRead, 1, BytesOf('03'), 1, DRIVER_A, 0, @pkg);
  AssertEquals('os contadores somam', 3, FPorta.TXBytes);
  AssertEquals('os contadores somam', 4, FPorta.RXBytes);
end;

procedure TTestCommPort.ComandoQueNaoAconteceNaoContaBytes;
var
  pkg:TIOPacket;
begin
  //porta fechada: nada foi para o fio, nada pode ser contado
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 3, DRIVER_A, 0, @pkg);

  AssertEquals('transmitidos', 0, FPorta.TXBytes);
  AssertEquals('recebidos',    0, FPorta.RXBytes);
end;

procedure TTestCommPort.UltimoQuadroFicaGuardadoEmHexa;
var
  pkg:TIOPacket;
begin
  //as duas propriedades existem para a interface grafica mostrar o que passou
  //pelo fio no ultimo comando
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);

  AssertEquals('enviado',  '01 02 ', FPorta.Traffic_send);
  AssertEquals('recebido', 'AA BB ', FPorta.Traffic_receiver);
end;

procedure TTestCommPort.UltimoQuadroNaoMisturaComandosAnteriores;
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

  AssertEquals('recebido no ultimo comando', 'AA BB ', FPorta.Traffic_receiver);
end;

procedure TTestCommPort.UltimoQuadroEnviadoAcompanhaAEscritaPura;
var
  pkg:TIOPacket;
begin
  //uma escrita pura tambem poe bytes no fio: o quadro enviado tem que ser o
  //dela, e nao o do comando anterior
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));
  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);

  FPorta.IOCommandSync(iocWrite, 2, BytesOf('03 04'), 0, DRIVER_A, 0, @pkg);

  AssertEquals('enviado na escrita pura', '03 04 ', FPorta.Traffic_send);
end;

procedure TTestCommPort.LimparBuffersChegaNaPortaConcreta;
begin
  //a classe base nao limpa nada por conta propria: ela repassa a limpeza para
  //a porta concreta, que e' quem sabe o que ha para esvaziar
  AssertEquals('nenhuma limpeza ainda', 0, FPorta.LimpezasDeBuffer);

  FPorta.LimparBuffers;
  AssertEquals('a porta concreta foi chamada', 1, FPorta.LimpezasDeBuffer);
end;

procedure TTestCommPort.LimparNaFalhaVemLigadoDeFabrica;
begin
  AssertTrue('limpar os buffers em erro de comunicacao', FPorta.ClearBuffersOnCommErrors);
end;

procedure TTestCommPort.ErroDeLeituraAvisaQuemEscuta;
begin
  FPorta.OnCommErrorReading:=@ContarErroDeLeitura;
  FPorta.OnCommErrorWriting:=@ContarErroDeEscrita;

  FPorta.AvisarErroDeEntradaESaida(false, iorTimeOut);

  AssertEquals('avisou a leitura',        1, FErrosDeLeitura);
  AssertEquals('e nao a escrita',         0, FErrosDeEscrita);
  AssertEquals('com o erro que aconteceu', Ord(iorTimeOut), Ord(FUltimoErro));
end;

procedure TTestCommPort.ErroDeEscritaAvisaQuemEscuta;
begin
  FPorta.OnCommErrorReading:=@ContarErroDeLeitura;
  FPorta.OnCommErrorWriting:=@ContarErroDeEscrita;

  FPorta.AvisarErroDeEntradaESaida(true, iorPortError);

  AssertEquals('avisou a escrita',        1, FErrosDeEscrita);
  AssertEquals('e nao a leitura',         0, FErrosDeLeitura);
  AssertEquals('com o erro que aconteceu', Ord(iorPortError), Ord(FUltimoErro));
end;

procedure TTestCommPort.DesconexaoAvisaQuemEscuta;
begin
  FPorta.OnCommPortDisconnected:=@ContarDesconexao;
  FPorta.AvisarDesconexao;

  AssertEquals('avisou a desconexao', 1, FDesconexoes);
end;

procedure TTestCommPort.PortaExclusivaNaoAbreEmTempoDeProjeto;
begin
  //uma porta serial nao pode ser aberta pelo ambiente de desenvolvimento: ela
  //tomaria o equipamento de quem esta' rodando
  FPorta.MarcarComoExclusiva;
  FPorta.MarcarComoEmProjeto;

  FPorta.Active:=true;

  AssertTrue ('a propriedade aceita o valor',  FPorta.Active);
  AssertFalse('mas a porta nao esta de fato aberta', FPorta.ReallyActive);
end;

procedure TTestCommPort.PortaNaoExclusivaAbreEmTempoDeProjeto;
begin
  //uma porta de rede pode, porque nao impede ninguem de usar o equipamento
  FPorta.MarcarComoEmProjeto;
  FPorta.Active:=true;

  AssertTrue('aberta de fato', FPorta.ReallyActive);
end;

procedure TTestCommPort.LigarORegistroCriaOArquivo;
begin
  FPorta.LogFile:=FArquivoDeLog;
  FPorta.LogIOActions:=true;

  AssertTrue ('o arquivo tem que existir', FileExists(FArquivoDeLog));
  AssertTrue ('e o registro ficou ligado', FPorta.LogIOActions);
end;

procedure TTestCommPort.CadaComandoDeixaUmaLinhaNoRegistro;
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

  AssertTrue('o que foi escrito esta no registro', Pos('01 02', ConteudoDoLog)>0);
  AssertTrue('o que foi lido tambem',              Pos('AA BB', ConteudoDoLog)>0);
end;

procedure TTestCommPort.RegistroDesligadoNaoEscreveNada;
var
  pkg:TIOPacket;
begin
  FPorta.LogFile:=FArquivoDeLog;
  FPorta.Active:=true;
  FPorta.QueueResponse(BytesOf('AA BB'));

  FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);

  AssertFalse('sem registro ligado, nem arquivo existe', FileExists(FArquivoDeLog));
end;

procedure TTestCommPort.TrocarDeArquivoComORegistroLigado;
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
    AssertTrue('o registro continua ligado', FPorta.LogIOActions);

    FPorta.IOCommandSync(iocWriteRead, 2, BytesOf('01 02'), 2, DRIVER_A, 0, @pkg);
    FPorta.LogIOActions:=false;

    AssertTrue('o arquivo novo foi criado', FileExists(segundo));
  finally
    if FileExists(segundo) then DeleteFile(segundo);
  end;
end;

procedure TTestCommPort.PortaDestruidaComORegistroLigadoFechaOArquivo;
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
  AssertTrue('a porta foi destruida com o registro ligado', true);
end;

initialization
  RegisterTest(TTestCommPort);

end.
