{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do mutex de rede: quem manda quando duas estacoes disputam
            o mesmo processo.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Em redundancia, duas estacoes rodam a mesma aplicacao e so' uma pode estar no
  comando. Quem decide qual e' um mutex que mora na rede: a estacao que
  conseguir entrar assume, a outra espera. Quando a que estava no comando cai,
  o mutex tem que ficar livre sozinho - senao a reserva nunca assume, que e' o
  oposto do que a redundancia existe para fazer.

  E ha' o caso em que o arbitro nao responde: ai' cada estacao decide pelo que
  esta' configurado nela, e saber se a resposta veio do servidor ou desse
  padrao e' o que evita duas estacoes no comando ao mesmo tempo.
}
{$ELSE}
{:
  @abstract(Network mutex tests: what decides which of two stations is in
            charge of the same process.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  In a redundant setup two stations run the same application and only one may
  be in command. What decides which one is a mutex living on the network: the
  station that manages to enter takes over, the other waits. When the one in
  command goes down, the mutex has to free itself - otherwise the standby never
  takes over, which is the opposite of what redundancy is there for.

  And there is the case where the arbiter does not answer: then each station
  decides by what is configured on it, and knowing whether the answer came from
  the server or from that default is what keeps two stations from being in
  command at once.
}
{$ENDIF}
unit ut.mutex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Sockets, socket_types,
  {$IF defined(WIN32) or defined(WIN64)}sockets_w32_w64{$ELSE}sockets_unix{$IFEND},
  MutexClient, MutexServer;

type

  { TArbitroDeMentira }

  {$IFDEF PORTUGUES}
  {:
    Um arbitro de mentira: aceita uma conexao e manda o byte que o teste pedir,
    sem ninguem ter perguntado nada. Serve para o caso da resposta fora de hora,
    que um servidor de verdade nao deixa acontecer sob demanda.
  }
  {$ELSE}
  {:
    A pretend arbiter: it takes one connection and sends whatever byte the test
    asks for, with nobody having asked anything. It is there for the answer that
    arrives out of turn, which a real server will not produce on demand.
  }
  {$ENDIF}
  TArbitroDeMentira = class
  private
    FOuvinte:LongInt;
    FCliente:LongInt;
    FPorta:Word;
  public
    constructor Create;
    destructor  Destroy; override;
    //: aceita a conexao que estiver esperando / takes the waiting connection
    function  Atender:Boolean;
    //: manda um byte solto para o cliente / sends a loose byte to the client
    procedure Enviar(aByte:Byte);
    property  Porta:Word read FPorta;
  end;

  { TEstacaoDeMentira }

  {$IFDEF PORTUGUES}
  {:
    Uma estacao de mentira: fala o protocolo do mutex no osso, para poder morrer
    sem se despedir - que e' o que uma estacao de verdade faz quando cai.
  }
  {$ELSE}
  {:
    A pretend station: it speaks the bare mutex protocol, so that it can die with
    no goodbye - which is what a real station does when it goes down.
  }
  {$ENDIF}
  TEstacaoDeMentira = class
  private
    FSocket:LongInt;
  public
    constructor Create(aPorta:Word);
    destructor  Destroy; override;
    //: pede o comando e diz se conseguiu / asks for command and says if it got it
    function  Assumir:Boolean;
    //: fecha o socket sem avisar ninguem / closes the socket telling nobody
    procedure Morrer;
  end;

  { TTestMutexDeRede }

  TMutexClientProbe = class(TMutexClient)
  public
    procedure BeginLoading;
    procedure EndLoading;
  end;

  TTestMutexDeRede = class(TTestCase)
  private
    FServidor:TMutexServer;
    FPorta:Word;
    FA, FB:TMutexClient;
    function  NovoCliente(aPorta:Word; aPadrao:Boolean):TMutexClient;
    procedure LigarServidor;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //com arbitro / with the arbiter
    procedure AStationCanTakeCommand;
    procedure TheSecondStationWaitsWhileTheFirstIsInCommand;
    procedure OnceTheFirstLeavesTheSecondTakesOver;
    procedure TheSameStationAskingTwiceStaysInCommand;
    procedure AStationThatShutsDownFreesTheCommand;
    procedure AStationThatDiesWithNoGoodbyeFreesTheCommand;

    //sem arbitro / with no arbiter
    procedure WithNoServerTheConfiguredDefaultDecides;
    procedure TheDefaultCanBeToStayOut;
    procedure TheAnswerSaysWhetherItCameFromTheServer;

    //desligado / switched off
    procedure AnInactiveClientAnswersItsDefault;

    //o componente cliente / the client component
    procedure ANewClientIsInactiveOnTheDeclaredDefaultPort;
    procedure TheServerAlsoStartsOnItsDeclaredDefaultPort;
    procedure AnInvalidHostIsRefused;
    procedure AValidHostIsKeptAndAnEmptyOneClearsIt;
    procedure HostAndPortCannotChangeWhileActive;
    procedure LeavingWithNoServerSaysTrue;
    procedure TheActiveFlagLoadedFromTheFormIsAppliedAfterLoading;
    procedure WhenTheServerShutsDownTheClientFallsBackToItsDefault;

    //resposta fora de hora / an answer out of turn
    procedure AnAnswerOutOfTurnDoesNotHangTheStation;
  end;

implementation

{ TArbitroDeMentira }

constructor TArbitroDeMentira.Create;
var
  endereco:TInetSockAddr;
  tam, reusar:LongInt;
begin
  inherited Create;
  FCliente:=-1;

  FOuvinte:=fpSocket(AF_INET, SOCK_STREAM, 0);
  if FOuvinte<0 then
    raise Exception.Create('arbitro de mentira: nao consegui criar o socket');

  reusar:=1;
  fpSetSockOpt(FOuvinte, SOL_SOCKET, SO_REUSEADDR, @reusar, SizeOf(reusar));

  endereco.sin_family     :=AF_INET;
  endereco.sin_port       :=htons(0);
  endereco.sin_addr.s_addr:=htonl($7F000001);

  if fpBind(FOuvinte, @endereco, SizeOf(endereco))<>0 then
    raise Exception.Create('arbitro de mentira: nao consegui reservar a porta');

  if fpListen(FOuvinte, 4)<>0 then
    raise Exception.Create('arbitro de mentira: nao consegui ouvir a porta');

  tam:=SizeOf(endereco);
  if fpGetSockName(FOuvinte, @endereco, @tam)<>0 then
    raise Exception.Create('arbitro de mentira: nao achei a porta que me deram');
  FPorta:=htons(endereco.sin_port);
end;

destructor TArbitroDeMentira.Destroy;
begin
  if FCliente>=0 then CloseSocket(FCliente);
  if FOuvinte>=0 then CloseSocket(FOuvinte);
  inherited Destroy;
end;

function TArbitroDeMentira.Atender:Boolean;
var
  endereco:TInetSockAddr;
  tam:LongInt;
begin
  tam:=SizeOf(endereco);
  FCliente:=fpAccept(FOuvinte, @endereco, @tam);
  Result:=FCliente>=0;
end;

procedure TArbitroDeMentira.Enviar(aByte:Byte);
begin
  if FCliente>=0 then
    fpSend(FCliente, @aByte, 1, 0);
end;

{ TEstacaoDeMentira }

constructor TEstacaoDeMentira.Create(aPorta:Word);
var
  endereco:TInetSockAddr;
begin
  inherited Create;
  FSocket:=fpSocket(AF_INET, SOCK_STREAM, 0);
  if FSocket<0 then
    raise Exception.Create('estacao de mentira: nao consegui criar o socket');

  endereco.sin_family     :=AF_INET;
  endereco.sin_port       :=htons(aPorta);
  endereco.sin_addr.s_addr:=htonl($7F000001);

  if fpConnect(FSocket, @endereco, SizeOf(endereco))<>0 then
    raise Exception.Create('estacao de mentira: nao consegui falar com o arbitro');

  //sem isso socket_recv nao tem como respeitar prazo nenhum
  //without this socket_recv has no way of honouring any deadline
  setblockingmode(FSocket, MODE_NONBLOCKING);
end;

destructor TEstacaoDeMentira.Destroy;
begin
  Morrer;
  inherited Destroy;
end;

function TEstacaoDeMentira.Assumir:Boolean;
var
  pedido, resposta:Byte;
  limite:QWord;
begin
  Result:=false;
  pedido:=2; //2 e' "quero entrar" / 2 is "I want in"
  if socket_send(FSocket, @pedido, 1, 0, 1000)<1 then exit;

  limite:=GetTickCount64+5000;
  while GetTickCount64<limite do begin
    //o servidor so' poe uma conexao nova para rodar quando alguem bombeia o
    //Synchronize dele. Numa aplicacao quem faz isso e' o laco principal; aqui,
    //que roda tudo no mesmo processo, quem faz e' o teste.
    //the server only puts a new connection to work when somebody pumps its
    //Synchronize. In an application that is the main loop; here, where it all
    //runs in one process, it is the test.
    CheckSynchronize(1);

    //o servidor tambem manda pings (255) por conta propria: e' preciso passar
    //por eles ate' achar a resposta do pedido.
    //the server also sends pings (255) on its own: they have to be walked past
    //until the answer to the request shows up.
    if socket_recv(FSocket, @resposta, 1, 0, 50)=1 then begin
      if resposta=21 then begin Result:=true; exit; end;
      if resposta=20 then exit;
      if resposta=255 then begin
        pedido:=254; //responde o ping / answers the ping
        socket_send(FSocket, @pedido, 1, 0, 1000);
      end;
    end;
  end;
end;

procedure TEstacaoDeMentira.Morrer;
begin
  if FSocket>=0 then
    CloseSocket(FSocket);
  FSocket:=-1;
end;

{ TMutexClientProbe }

procedure TMutexClientProbe.BeginLoading;
begin
  Loading;
end;

procedure TMutexClientProbe.EndLoading;
begin
  Loaded;
end;

{ TTestMutexDeRede }

procedure TTestMutexDeRede.SetUp;
begin
  FServidor:=nil;
  FA:=nil;
  FB:=nil;
  //porta alta e fixa por execucao: o servidor nao oferece porta efemera
  //a high, per-run fixed port: the server does not offer an ephemeral one
  FPorta:=52321+(GetTickCount64 mod 400);
end;

procedure TTestMutexDeRede.TearDown;
begin
  FreeAndNil(FA);
  FreeAndNil(FB);
  FreeAndNil(FServidor);
end;

procedure TTestMutexDeRede.LigarServidor;
begin
  FServidor:=TMutexServer.Create(nil);
  FServidor.Port:=FPorta;
  FServidor.Active:=true;
end;

function TTestMutexDeRede.NovoCliente(aPorta:Word; aPadrao:Boolean):TMutexClient;
begin
  Result:=TMutexClient.Create(nil);
  Result.Host:='127.0.0.1';
  Result.Port:=aPorta;
  Result.DefaultBehavior:=aPadrao;
  Result.Active:=true;
end;

procedure TTestMutexDeRede.AStationCanTakeCommand;
begin
  LigarServidor;
  FA:=NovoCliente(FPorta, false);

  AssertTrue('assumiu o comando', FA.TryEnter);
end;

procedure TTestMutexDeRede.TheSecondStationWaitsWhileTheFirstIsInCommand;
begin
  //duas estacoes no comando ao mesmo tempo e' o acidente que a redundancia
  //existe para evitar
  //two stations in command at once is the accident redundancy exists to avoid
  LigarServidor;
  FA:=NovoCliente(FPorta, false);
  FB:=NovoCliente(FPorta, false);

  AssertTrue ('a primeira assumiu', FA.TryEnter);
  AssertFalse('a segunda espera',   FB.TryEnter);
end;

procedure TTestMutexDeRede.OnceTheFirstLeavesTheSecondTakesOver;
begin
  LigarServidor;
  FA:=NovoCliente(FPorta, false);
  FB:=NovoCliente(FPorta, false);
  AssertTrue ('a primeira assumiu', FA.TryEnter);
  AssertFalse('a segunda espera',   FB.TryEnter);

  AssertTrue('a primeira saiu', FA.Leave);

  AssertTrue('a segunda assumiu', FB.TryEnter);
end;

procedure TTestMutexDeRede.TheSameStationAskingTwiceStaysInCommand;
begin
  //quem ja' esta' no comando continua no comando ao perguntar de novo
  //whoever is already in command stays in command when asking again
  LigarServidor;
  FA:=NovoCliente(FPorta, false);

  AssertTrue('assumiu', FA.TryEnter);
  AssertTrue('continua no comando', FA.TryEnter);
end;

procedure TTestMutexDeRede.AStationThatShutsDownFreesTheCommand;
var
  limite:QWord;
begin
  //e' o caso que a redundancia existe para atender: a estacao no comando cai,
  //e a reserva tem que conseguir assumir
  //this is the case redundancy exists for: the station in command goes down,
  //and the standby has to be able to take over
  LigarServidor;
  FA:=NovoCliente(FPorta, false);
  FB:=NovoCliente(FPorta, false);
  AssertTrue ('a primeira assumiu', FA.TryEnter);
  AssertFalse('a segunda espera',   FB.TryEnter);

  FreeAndNil(FA);

  limite:=GetTickCount64+5000;
  while (not FB.TryEnter) and (GetTickCount64<limite) do
    Sleep(50);

  AssertTrue('a reserva assumiu', FB.TryEnter);
end;

procedure TTestMutexDeRede.AStationThatDiesWithNoGoodbyeFreesTheCommand;
var
  caida:TEstacaoDeMentira;
  limite:QWord;
begin
  //queda de verdade nao manda aviso nenhum: o cabo sai, a maquina desliga, o
  //processo morre. Quem estava no comando some sem devolver o mutex, e cabe ao
  //arbitro notar a ausencia - senao a reserva espera para sempre por uma estacao
  //que nao existe mais.
  //a real crash sends no warning at all: the cable is pulled, the machine goes
  //off, the process dies. Whoever was in command vanishes without handing the
  //mutex back, and it is up to the arbiter to notice the absence - otherwise the
  //standby waits forever on a station that is not there any more.
  LigarServidor;
  caida:=TEstacaoDeMentira.Create(FPorta);
  try
    AssertTrue('a que caiu estava no comando', caida.Assumir);

    FB:=NovoCliente(FPorta, false);
    AssertFalse('a reserva espera', FB.TryEnter);

    caida.Morrer;

    limite:=GetTickCount64+20000;
    while (not FB.TryEnter) and (GetTickCount64<limite) do
      Sleep(100);

    AssertTrue('a reserva assumiu depois da queda', FB.TryEnter);
  finally
    caida.Free;
  end;
end;

procedure TTestMutexDeRede.WithNoServerTheConfiguredDefaultDecides;
begin
  //sem arbitro, a estacao decide sozinha pelo que foi configurado nela
  //with no arbiter, the station decides on its own by what was configured on it
  FA:=NovoCliente(FPorta, true);

  AssertTrue('assumiu pelo padrao configurado', FA.TryEnter);
end;

procedure TTestMutexDeRede.TheDefaultCanBeToStayOut;
begin
  FA:=NovoCliente(FPorta, false);

  AssertFalse('ficou de fora pelo padrao configurado', FA.TryEnter);
end;

procedure TTestMutexDeRede.TheAnswerSaysWhetherItCameFromTheServer;
var
  peloPadrao:Boolean;
begin
  //saber a origem da resposta e' o que permite a aplicacao tratar "assumi
  //porque o arbitro deixou" de forma diferente de "assumi porque nao achei o
  //arbitro"
  //knowing where the answer came from is what lets the application tell "I took
  //over because the arbiter let me" from "I took over because I could not find
  //the arbiter"
  FA:=NovoCliente(FPorta, true);
  peloPadrao:=false;

  FA.TryEnter(peloPadrao);
  AssertTrue('sem servidor, veio do padrao', peloPadrao);

  LigarServidor;
  FB:=NovoCliente(FPorta, true);
  peloPadrao:=true;

  FB.TryEnter(peloPadrao);
  AssertFalse('com servidor, veio do servidor', peloPadrao);
end;

procedure TTestMutexDeRede.AnInactiveClientAnswersItsDefault;
begin
  //desligado, o cliente nem procura o servidor
  //switched off, the client does not even look for the server
  LigarServidor;
  FA:=TMutexClient.Create(nil);
  FA.Host:='127.0.0.1';
  FA.Port:=FPorta;
  FA.DefaultBehavior:=true;
  FA.Active:=false;

  AssertTrue('respondeu o padrao', FA.TryEnter);
end;

procedure TTestMutexDeRede.AnAnswerOutOfTurnDoesNotHangTheStation;
var
  arbitro:TArbitroDeMentira;
begin
  //quando a rede atrasa, a resposta de um pedido antigo chega depois que quem
  //pediu ja' desistiu, e quem a acha e' a thread que cuida da conexao. Se ela
  //sair de cena sem avisar que acabou, desligar a estacao nao termina nunca -
  //e uma estacao que nao consegue se desligar tambem nao consegue reassumir.
  //when the network lags, the answer to an old request arrives after whoever
  //asked has given up, and what finds it is the thread minding the connection.
  //If that thread leaves without saying it ended, shutting the station down
  //never finishes - and a station that cannot shut down cannot take over again.
  arbitro:=TArbitroDeMentira.Create;
  try
    FA:=TMutexClient.Create(nil);
    FA.Host:='127.0.0.1';
    FA.Port:=arbitro.Porta;
    FA.DefaultBehavior:=false;
    FA.Active:=true;

    AssertTrue('o arbitro pegou a conexao', arbitro.Atender);

    //21 e' "pode entrar", so' que ninguem pediu para entrar
    //21 is "you may enter", except nobody asked to enter
    arbitro.Enviar(21);
    Sleep(300);

    FreeAndNil(FA);
    AssertTrue('a estacao se desligou', true);
  finally
    arbitro.Free;
  end;
end;

procedure TTestMutexDeRede.ANewClientIsInactiveOnTheDeclaredDefaultPort;
begin
  //a porta que a propriedade declara como padrao e a que o construtor poe
  //tem que ser a mesma: e' pelo "default" que o IDE decide nao gravar a
  //propriedade no .lfm - e ai o construtor e' quem vale ao carregar
  //the port the property declares as default and the one the constructor
  //sets have to be the same: it is by the "default" that the IDE decides not
  //to write the property to the .lfm - and then the constructor rules on load
  FA:=TMutexClient.Create(nil);

  AssertFalse ('inativo',          FA.Active);
  AssertEquals('porta padrao',     52321, FA.Port);
  AssertFalse ('sem comando por padrao', FA.DefaultBehavior);
  AssertEquals('sem servidor',     '', FA.Host);
end;

procedure TTestMutexDeRede.TheServerAlsoStartsOnItsDeclaredDefaultPort;
begin
  FServidor:=TMutexServer.Create(nil);

  AssertEquals('porta padrao', 52321, FServidor.Port);
end;

procedure TTestMutexDeRede.AnInvalidHostIsRefused;
begin
  FA:=TMutexClient.Create(nil);
  FA.Host:='10.0.0.1';

  try
    FA.Host:='servidor';
    Fail('um nome nao e'' um IPv4');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('o anterior ficou', '10.0.0.1', FA.Host);
end;

procedure TTestMutexDeRede.AValidHostIsKeptAndAnEmptyOneClearsIt;
begin
  FA:=TMutexClient.Create(nil);

  FA.Host:='192.168.0.10';
  AssertEquals('guardado', '192.168.0.10', FA.Host);

  FA.Host:='';
  AssertEquals('limpo', '', FA.Host);
end;

procedure TTestMutexDeRede.HostAndPortCannotChangeWhileActive;
begin
  //ativo sem servidor: continua ativo, tentando; mudar o alvo por baixo dele
  //e' recusado
  //active with no server: it stays active, trying; changing the target under
  //it is refused
  FA:=NovoCliente(FPorta, true);

  try
    FA.Port:=FPorta+1;
    Fail('a porta nao muda com o cliente ativo');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;
  try
    FA.Host:='10.0.0.1';
    Fail('o host nao muda com o cliente ativo');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  FA.Active:=false;
  FA.Port:=FPorta+1;
  AssertEquals('inativo, muda', FPorta+1, FA.Port);
end;

procedure TTestMutexDeRede.LeavingWithNoServerSaysTrue;
begin
  //sem arbitro nao ha' o que soltar; a estacao nao pode ficar presa nisso
  //with no arbiter there is nothing to release; the station must not get
  //stuck on it
  FA:=NovoCliente(FPorta, true);

  AssertTrue(FA.Leave);
end;

procedure TTestMutexDeRede.TheActiveFlagLoadedFromTheFormIsAppliedAfterLoading;
var
  sonda:TMutexClientProbe;
begin
  //durante a carga do .lfm a ativacao espera o Loaded - a porta e o host
  //podem nem ter sido lidos ainda
  //while the .lfm loads the activation waits for Loaded - the port and the
  //host may not even have been read yet
  sonda:=TMutexClientProbe.Create(nil);
  try
    sonda.BeginLoading;
    sonda.Host:='127.0.0.1';
    sonda.Port:=FPorta;
    sonda.Active:=true;
    AssertFalse('carregando, ainda inativo', sonda.Active);

    sonda.EndLoading;

    AssertTrue('carregado, ativo', sonda.Active);
  finally
    sonda.Free;
  end;
end;

procedure TTestMutexDeRede.WhenTheServerShutsDownTheClientFallsBackToItsDefault;
var
  peloPadrao:Boolean;
  limite:QWord;
begin
  //o arbitro desliga: a estacao tem que perceber e voltar a decidir pelo
  //padrao configurado, em vez de ficar presa numa conexao morta
  //the arbiter shuts down: the station has to notice and go back to deciding
  //by the configured default, instead of hanging on a dead connection
  LigarServidor;
  FA:=NovoCliente(FPorta, false);
  peloPadrao:=true;
  AssertTrue ('assumiu pelo servidor', FA.TryEnter(peloPadrao));
  AssertFalse('e foi o servidor',      peloPadrao);

  FreeAndNil(FServidor);

  limite:=GetTickCount64+5000;
  repeat
    Sleep(50);
    FA.TryEnter(peloPadrao);
  until peloPadrao or (GetTickCount64>limite);

  AssertTrue ('voltou ao padrao',          peloPadrao);
  AssertFalse('e o padrao e'' ficar de fora', FA.TryEnter);
end;

initialization
  RegisterTest(TTestMutexDeRede);

end.
