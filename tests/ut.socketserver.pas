{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do servidor socket: a thread que aceita conexoes e a
            thread que atende cada uma.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' a base do servidor do mutex de rede e do controle de redundancia: uma
  thread fica no accept e, a cada conexao, lanca outra para atender o
  cliente, avisando a thread principal na entrada e na saida.

  Os testes usam a rede de verdade, em 127.0.0.1 e porta efemera: um socket
  ouvinte, uma thread de accept com um atendente que devolve o que recebe, e
  clientes de mentira do lado de ca'.
}
{$ELSE}
{:
  @abstract(Socket server tests: the thread that accepts connections and the
            thread that serves each one.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It is the base of the network mutex server and of the redundancy control:
  one thread sits in accept and, for each connection, launches another to
  serve the client, telling the main thread on the way in and on the way out.

  The tests use the real network, on 127.0.0.1 and an ephemeral port: a
  listening socket, an accept thread with a server that echoes what it gets,
  and pretend clients on this side.
}
{$ENDIF}
unit ut.socketserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Sockets, socket_types,
  {$IF defined(WIN32) or defined(WIN64)}sockets_w32_w64{$ELSE}sockets_unix{$IFEND},
  syncobjs, crossthreads, socketserver;

type

  { TEchoThread }

  //atende um cliente devolvendo cada byte que ele manda, ate' ele desligar
  //serves a client giving back every byte it sends, until it hangs up
  TEchoThread = class(TSocketClientThread)
  protected
    procedure ThreadLoop; override;
  public
    function PeerAddress:LongWord;
    function WaitEnd(aTimeout:Cardinal):TWaitResult;
  end;

  { TAcceptProbe }

  //a cada conexao aceita lanca um TEchoThread, como o servidor do mutex faz
  //for every accepted connection launches a TEchoThread, as the mutex server
  //does
  TAcceptProbe = class(TSocketAcceptThread)
  protected
    procedure LaunchNewThread; override;
  public
    //encena a corrida: o atendente ja' nasce terminado, e a RTL pula o
    //Execute dele
    //stages the race: the server thread is born terminated, and the RTL skips
    //its Execute
    TerminateBeforeRunning:Boolean;
  end;

  { TTestSocketServer }

  TTestSocketServer = class(TTestCase)
  private
    FListener:TSocket;
    FPort:Word;
    FAccept:TAcceptProbe;
    FClients:TList;
    FAdded, FRemoved:LongInt;
    FAddedOn:TThreadID;
    procedure ClientAdded(Sender:TObject);
    procedure ClientRemoved(Sender:TObject);
    procedure StartServer;
    function  Connect:TSocket;
    function  WaitUntil(var aCounter:LongInt; aAtLeast, aDeadlineMs:LongInt):Boolean;
    function  FirstClient:TEchoThread;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //as conexoes / the connections
    procedure AConnectionLaunchesAClientThread;
    procedure TheAddNoticeComesOnTheMainThread;
    procedure TwoConnectionsLaunchTwoThreads;
    procedure TheClientThreadCarriesThePeerAddress;

    //a conversa / the talk
    procedure TheClientThreadTalksThroughTheSocket;
    procedure WhenThePeerHangsUpTheClientThreadEndsAndSaysSo;
    procedure TerminatingTheClientThreadClosesTheSocket;
    procedure AClientThreadThatNeverRanStillClosesTheSocket;

    //o fim / the end
    procedure TerminatingTheAcceptThreadStopsAccepting;
    procedure TerminatingTwiceIsHarmless;
    procedure DestroyingWithoutTerminatingDoesNotLeak;
  end;

implementation

{ TEchoThread }

procedure TEchoThread.ThreadLoop;
var
  b:Byte;
  n:LongInt;
begin
  while not Terminated do begin
    n:=socket_recv(FSocket, @b, 1, 0, 20);
    if n=1 then
      socket_send(FSocket, @b, 1, 0, 1000)
    else if n=0 then
      exit; //o cliente desligou / the client hung up
  end;
end;

function TEchoThread.WaitEnd(aTimeout:Cardinal):TWaitResult;
begin
  Result:=inherited WaitEnd(aTimeout);
end;

function TEchoThread.PeerAddress:LongWord;
begin
  Result:=ntohl(TInetSockAddr(FClientInfo).sin_addr.s_addr);
end;

{ TAcceptProbe }

procedure TAcceptProbe.LaunchNewThread;
begin
  setblockingmode(ClientSocket, MODE_NONBLOCKING);
  FClientThread:=TEchoThread.Create(true, ClientSocket, ClientSockInfo, FRemoveClientThread);
  Synchronize(@AddClientToMainThread);
  if TerminateBeforeRunning then
    TpSCADACoreAffinityThread(FClientThread).Terminate;
  FClientThread.WakeUp;
end;

{ TTestSocketServer }

procedure TTestSocketServer.SetUp;
var
  endereco:TInetSockAddr;
  tam, reusar:LongInt;
begin
  FAccept:=nil;
  FClients:=TList.Create;
  FAdded:=0;
  FRemoved:=0;
  FAddedOn:=0;

  FListener:=fpSocket(AF_INET, SOCK_STREAM, 0);
  if FListener<0 then
    raise Exception.Create('nao consegui criar o socket ouvinte');
  reusar:=1;
  fpSetSockOpt(FListener, SOL_SOCKET, SO_REUSEADDR, @reusar, SizeOf(reusar));

  endereco.sin_family     :=AF_INET;
  endereco.sin_port       :=htons(0);
  endereco.sin_addr.s_addr:=htonl($7F000001);
  if fpBind(FListener, @endereco, SizeOf(endereco))<>0 then
    raise Exception.Create('nao consegui reservar a porta');
  if fpListen(FListener, 4)<>0 then
    raise Exception.Create('nao consegui ouvir a porta');
  tam:=SizeOf(endereco);
  if fpGetSockName(FListener, @endereco, @tam)<>0 then
    raise Exception.Create('nao achei a porta que me deram');
  FPort:=ntohs(endereco.sin_port);

  //como o servidor de verdade: o accept nao bloqueia, a thread dorme entre
  //as tentativas e o Terminate consegue alcanca-la
  //like the real server: accept does not block, the thread sleeps between
  //attempts and Terminate can reach it
  setblockingmode(FListener, MODE_NONBLOCKING);
end;

procedure TTestSocketServer.TearDown;
var
  i:LongInt;
begin
  if FAccept<>nil then begin
    FAccept.Terminate;
    FreeAndNil(FAccept);
  end;
  for i:=0 to FClients.Count-1 do begin
    TEchoThread(FClients[i]).Terminate;
    TEchoThread(FClients[i]).Free;
  end;
  FreeAndNil(FClients);
  if FListener>=0 then
    CloseSocket(FListener);
end;

procedure TTestSocketServer.ClientAdded(Sender:TObject);
begin
  FClients.Add(Sender);
  FAddedOn:=GetCurrentThreadId;
  inc(FAdded);
end;

procedure TTestSocketServer.ClientRemoved(Sender:TObject);
begin
  inc(FRemoved);
end;

procedure TTestSocketServer.StartServer;
begin
  FAccept:=TAcceptProbe.Create(true, FListener, @ClientAdded, @ClientRemoved);
  FAccept.WakeUp;
end;

function TTestSocketServer.Connect:TSocket;
var
  endereco:TInetSockAddr;
begin
  Result:=fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result<0 then
    raise Exception.Create('cliente: nao consegui criar o socket');
  endereco.sin_family     :=AF_INET;
  endereco.sin_port       :=htons(FPort);
  endereco.sin_addr.s_addr:=htonl($7F000001);
  if fpConnect(Result, @endereco, SizeOf(endereco))<>0 then
    raise Exception.Create('cliente: nao consegui conectar');
  setblockingmode(Result, MODE_NONBLOCKING);
end;

function TTestSocketServer.WaitUntil(var aCounter:LongInt; aAtLeast, aDeadlineMs:LongInt):Boolean;
var
  gasto:LongInt;
begin
  //os avisos vem por Synchronize: sem bombear a fila eles nunca chegam
  //the notices come through Synchronize: with no pumping they never arrive
  gasto:=0;
  while (aCounter<aAtLeast) and (gasto<aDeadlineMs) do begin
    CheckSynchronize(5);
    inc(gasto, 5);
  end;
  Result:=aCounter>=aAtLeast;
end;

function TTestSocketServer.FirstClient:TEchoThread;
begin
  Result:=TEchoThread(FClients[0]);
end;

procedure TTestSocketServer.AConnectionLaunchesAClientThread;
var
  s:TSocket;
begin
  StartServer;

  s:=Connect;
  try
    AssertTrue  ('avisou a entrada', WaitUntil(FAdded, 1, 3000));
    AssertEquals('uma thread',       1, FClients.Count);
    AssertTrue  ('e e'' a de atender', TObject(FClients[0]) is TEchoThread);
  finally
    CloseSocket(s);
  end;
end;

procedure TTestSocketServer.TheAddNoticeComesOnTheMainThread;
var
  s:TSocket;
begin
  //quem recebe o aviso mexe em listas e componentes da aplicacao
  //whoever gets the notice touches the application's lists and components
  StartServer;

  s:=Connect;
  try
    AssertTrue('avisou', WaitUntil(FAdded, 1, 3000));
    AssertTrue('na thread principal', FAddedOn=MainThreadID);
  finally
    CloseSocket(s);
  end;
end;

procedure TTestSocketServer.TwoConnectionsLaunchTwoThreads;
var
  a, b:TSocket;
begin
  StartServer;

  a:=Connect;
  b:=Connect;
  try
    AssertTrue  ('duas entradas', WaitUntil(FAdded, 2, 3000));
    AssertEquals('duas threads',  2, FClients.Count);
    AssertTrue  ('distintas',     FClients[0]<>FClients[1]);
  finally
    CloseSocket(a);
    CloseSocket(b);
  end;
end;

procedure TTestSocketServer.TheClientThreadCarriesThePeerAddress;
var
  s:TSocket;
begin
  StartServer;

  s:=Connect;
  try
    AssertTrue  ('avisou',     WaitUntil(FAdded, 1, 3000));
    AssertEquals('127.0.0.1',  $7F000001, FirstClient.PeerAddress);
  finally
    CloseSocket(s);
  end;
end;

procedure TTestSocketServer.TheClientThreadTalksThroughTheSocket;
var
  s:TSocket;
  fora, volta:Byte;
begin
  StartServer;

  s:=Connect;
  try
    AssertTrue('avisou', WaitUntil(FAdded, 1, 3000));

    fora:=$5A;
    AssertEquals('mandou',   1, socket_send(s, @fora, 1, 0, 1000));
    AssertEquals('voltou',   1, socket_recv(s, @volta, 1, 0, 3000));
    AssertEquals('o mesmo',  $5A, volta);
  finally
    CloseSocket(s);
  end;
end;

procedure TTestSocketServer.WhenThePeerHangsUpTheClientThreadEndsAndSaysSo;
var
  s:TSocket;
begin
  StartServer;
  s:=Connect;
  AssertTrue('avisou a entrada', WaitUntil(FAdded, 1, 3000));

  CloseSocket(s);

  AssertTrue('avisou a saida',    WaitUntil(FRemoved, 1, 3000));
  //o aviso sai de dentro da thread, que ainda tem uns passos ate' acabar
  //the notice leaves from inside the thread, which still has a few steps to
  //go before it ends
  AssertTrue('e a thread acabou', FirstClient.WaitEnd(3000)=wrSignaled);
end;

procedure TTestSocketServer.TerminatingTheClientThreadClosesTheSocket;
var
  s:TSocket;
  b:Byte;
begin
  StartServer;
  s:=Connect;
  try
    AssertTrue('avisou a entrada', WaitUntil(FAdded, 1, 3000));
    //uma ida e volta garante que o atendente esta' rodando
    //a round trip makes sure the server thread is running
    b:=1;
    socket_send(s, @b, 1, 0, 1000);
    AssertEquals('atendente vivo', 1, socket_recv(s, @b, 1, 0, 3000));

    FirstClient.Terminate;

    //do lado de ca' a conexao acabou: a leitura devolve fim de arquivo
    //on this side the connection is over: the read gives end of file
    AssertEquals('fim da conexao', 0, socket_recv(s, @b, 1, 0, 3000));
  finally
    CloseSocket(s);
  end;
end;

procedure TTestSocketServer.AClientThreadThatNeverRanStillClosesTheSocket;
var
  s:TSocket;
  b:Byte;
begin
  //o cliente que conecta no instante em que o servidor desliga: a thread
  //dele nasce terminada e a RTL pula o Execute - onde o socket era fechado.
  //O cliente nao pode ficar pendurado numa conexao que ninguem atende.
  //the client connecting the instant the server shuts down: its thread is
  //born terminated and the RTL skips Execute - where the socket was closed.
  //The client must not be left hanging on a connection nobody serves.
  StartServer;
  FAccept.TerminateBeforeRunning:=true;
  s:=Connect;
  try
    AssertTrue('avisou a entrada', WaitUntil(FAdded, 1, 3000));

    FirstClient.Terminate;

    AssertEquals('fim da conexao', 0, socket_recv(s, @b, 1, 0, 3000));
  finally
    CloseSocket(s);
  end;
end;

procedure TTestSocketServer.TerminatingTheAcceptThreadStopsAccepting;
var
  s:TSocket;
begin
  StartServer;

  FAccept.Terminate;
  AssertTrue('a thread de accept acabou', FAccept.Finished);

  //o TCP ainda aceita a conexao na fila do ouvinte, mas ninguem a atende
  //TCP still takes the connection into the listener's backlog, but nobody
  //serves it
  s:=Connect;
  try
    AssertFalse('nenhuma thread lancada', WaitUntil(FAdded, 1, 300));
  finally
    CloseSocket(s);
  end;
end;

procedure TTestSocketServer.TerminatingTwiceIsHarmless;
begin
  StartServer;

  FAccept.Terminate;
  FAccept.Terminate;

  AssertTrue('acabou', FAccept.Finished);
end;

procedure TTestSocketServer.DestroyingWithoutTerminatingDoesNotLeak;
var
  s:TSocket;
begin
  //um servidor que some sem passar pelo Terminate: o heaptrc no fim da
  //suite e' quem confere que nada ficou para tras
  //a server that goes away without going through Terminate: the heaptrc at
  //the end of the suite is what checks nothing was left behind
  StartServer;
  s:=Connect;
  try
    AssertTrue('avisou a entrada', WaitUntil(FAdded, 1, 3000));

    FreeAndNil(FAccept);
    FirstClient.Free;
    FClients.Clear;
  finally
    CloseSocket(s);
  end;
end;

initialization
  RegisterTest(TTestSocketServer);

end.
