{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Servidor de testes: um equipamento de mentira ouvindo em 127.0.0.1.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A porta de rede e' a unica da biblioteca que nao da' para exercitar com uma
  porta falsa: o que interessa nela e' justamente o soquete - conectar,
  perceber que o outro lado sumiu, reconectar. Entao aqui ha um servidor de
  verdade, ouvindo numa porta efemera do endereco de retorno, que responde o
  que os testes mandarem responder e sabe soltar a conexao na hora que o teste
  pedir.

  Os soquetes vem da unit Sockets do FPC, que oferece a mesma familia fp* em
  todos os alvos. Para nao deixar a thread presa num accept sem fim, o
  encerramento conecta um cliente descartavel na propria porta.
}
{$ELSE}
{:
  @abstract(A test server: a fake device listening on 127.0.0.1.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The network port is the only one in the library that cannot be exercised
  with a fake port: what matters in it is the socket itself - connecting,
  noticing the other end is gone, reconnecting. So here is a real server,
  listening on an ephemeral port of the loopback address, which answers
  whatever the tests tell it to answer and knows how to drop the connection
  when the test asks.

  The sockets come from FPC's Sockets unit, which offers the same fp* family
  on every target. To avoid leaving the thread stuck in an endless accept,
  shutting down connects a throwaway client to the port itself.
}
{$ENDIF}
unit testsupport.fakeserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, Sockets, commtypes;

type

  { TServidorDeTeste }

  TServidorDeTeste = class
  private
    FOuvinte:LongInt;
    FCliente:LongInt;
    FPorta:LongInt;
    FCS:TCriticalSection;
    FThread:TThread;
    FRecebido:BYTES;
    FRespostas:array of BYTES;
    FProximaResposta:LongInt;
    FConexoes:LongInt;
    FTerminou:Boolean;
    function  Parando:Boolean;
    function  GetConexoes:LongInt;
    function  GetRecebido:BYTES;
    function  ProximaResposta(out aResposta:BYTES):Boolean;
    procedure GuardarRecebido(const aBuf; aTamanho:LongInt);
    procedure NovaConexao(aSocket:LongInt);
    procedure Servir;
  public
    constructor Create;
    destructor  Destroy; override;

    {$IFDEF PORTUGUES}
    //: Enfileira o que o equipamento vai responder na proxima mensagem.
    {$ELSE}
    //: Queues what the device will answer on the next incoming message.
    {$ENDIF}
    procedure EnfileirarResposta(const aResposta:BYTES);

    {$IFDEF PORTUGUES}
    //: Solta a conexao, como um equipamento que perdeu energia.
    {$ELSE}
    //: Drops the connection, like a device that lost power.
    {$ENDIF}
    procedure SoltarAConexao;

    {$IFDEF PORTUGUES}
    //: Espera ate' haver aQuantas conexoes, ou ate' o prazo acabar.
    {$ELSE}
    //: Waits until there are aQuantas connections, or the deadline passes.
    {$ENDIF}
    function EsperarConexoes(aQuantas, aPrazoMs:LongInt):Boolean;

    {$IFDEF PORTUGUES}
    //: Espera ate' ter recebido aQuantos bytes, ou ate' o prazo acabar.
    {$ELSE}
    //: Waits until aQuantos bytes have arrived, or the deadline passes.
    {$ENDIF}
    function EsperarBytes(aQuantos, aPrazoMs:LongInt):Boolean;

    //: porta efemera que o sistema escolheu
    property Porta:LongInt read FPorta;
    //: tudo o que o cliente mandou, desde o inicio
    property Recebido:BYTES read GetRecebido;
    //: quantas conexoes foram aceitas
    property Conexoes:LongInt read GetConexoes;
  end;

  { TServidorUDPDeTeste }

  {$IFDEF PORTUGUES}
  {:
  O mesmo equipamento de mentira, agora em datagrama. Nao ha' conexao a
  aceitar: o servidor fica esperando datagramas e responde para quem mandou.
  }
  {$ELSE}
  {:
  The same fake device, now over datagrams. There is no connection to accept:
  the server waits for datagrams and answers whoever sent them.
  }
  {$ENDIF}
  TServidorUDPDeTeste = class
  private
    FSocket:LongInt;
    FPorta:LongInt;
    FCS:TCriticalSection;
    FThread:TThread;
    FRecebido:BYTES;
    FRespostas:array of BYTES;
    FProximaResposta:LongInt;
    FTerminou:Boolean;
    function  Parando:Boolean;
    function  GetRecebido:BYTES;
    function  ProximaResposta(out aResposta:BYTES):Boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Servir;
    procedure EnfileirarResposta(const aResposta:BYTES);
    function  EsperarBytes(aQuantos, aPrazoMs:LongInt):Boolean;
    property  Porta:LongInt read FPorta;
    property  Recebido:BYTES read GetRecebido;
  end;

implementation

type

  { TThreadDoServidorUDP }

  TThreadDoServidorUDP = class(TThread)
  private
    FDono:TServidorUDPDeTeste;
  protected
    procedure Execute; override;
  public
    constructor Create(aDono:TServidorUDPDeTeste);
  end;

  { TThreadDoServidor }

  TThreadDoServidor = class(TThread)
  private
    FDono:TServidorDeTeste;
  protected
    procedure Execute; override;
  public
    constructor Create(aDono:TServidorDeTeste);
  end;

constructor TThreadDoServidor.Create(aDono:TServidorDeTeste);
begin
  FDono:=aDono;
  FreeOnTerminate:=false;
  inherited Create(false);
end;

procedure TThreadDoServidor.Execute;
begin
  FDono.Servir;
end;

constructor TThreadDoServidorUDP.Create(aDono:TServidorUDPDeTeste);
begin
  FDono:=aDono;
  FreeOnTerminate:=false;
  inherited Create(false);
end;

procedure TThreadDoServidorUDP.Execute;
begin
  FDono.Servir;
end;

{ TServidorUDPDeTeste }

constructor TServidorUDPDeTeste.Create;
var
  endereco:TInetSockAddr;
  tam:LongInt;
begin
  inherited Create;
  FCS:=TCriticalSection.Create;
  FProximaResposta:=0;
  FTerminou:=false;

  FSocket:=fpSocket(AF_INET, SOCK_DGRAM, 0);
  if FSocket<0 then
    raise Exception.Create('servidor udp de teste: nao consegui criar o soquete');

  endereco.sin_family     :=AF_INET;
  endereco.sin_port       :=htons(0);
  endereco.sin_addr.s_addr:=htonl($7F000001);

  if fpBind(FSocket, @endereco, SizeOf(endereco))<>0 then
    raise Exception.Create('servidor udp de teste: nao consegui reservar a porta');

  tam:=SizeOf(endereco);
  if fpGetSockName(FSocket, @endereco, @tam)<>0 then
    raise Exception.Create('servidor udp de teste: nao descobri a porta escolhida');
  FPorta:=htons(endereco.sin_port);

  FThread:=TThreadDoServidorUDP.Create(Self);
end;

destructor TServidorUDPDeTeste.Destroy;
var
  acordar:LongInt;
  endereco:TInetSockAddr;
  nada:Byte;
begin
  FCS.Enter;
  try
    FTerminou:=true;
  finally
    FCS.Leave;
  end;

  if FThread<>nil then begin
    FThread.Terminate;

    //a thread esta' parada esperando datagrama: um datagrama a acorda
    //the thread is sitting on a recvfrom: a datagram wakes it up
    acordar:=fpSocket(AF_INET, SOCK_DGRAM, 0);
    if acordar>=0 then begin
      endereco.sin_family     :=AF_INET;
      endereco.sin_port       :=htons(FPorta);
      endereco.sin_addr.s_addr:=htonl($7F000001);
      nada:=0;
      fpSendTo(acordar, @nada, 1, 0, @endereco, SizeOf(endereco));
      CloseSocket(acordar);
    end;

    FThread.WaitFor;
    FreeAndNil(FThread);
  end;

  if FSocket>=0 then
    CloseSocket(FSocket);

  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TServidorUDPDeTeste.Servir;
var
  buf:array[0..1023] of Byte;
  quem:TInetSockAddr;
  tam, lidos, antes:LongInt;
  resposta:BYTES;
begin
  while not Parando do begin
    tam:=SizeOf(quem);
    lidos:=fpRecvFrom(FSocket, @buf[0], SizeOf(buf), 0, @quem, @tam);

    if Parando or (lidos<=0) then break;

    FCS.Enter;
    try
      antes:=Length(FRecebido);
      SetLength(FRecebido, antes+lidos);
      Move(buf[0], FRecebido[antes], lidos);
    finally
      FCS.Leave;
    end;

    if ProximaResposta(resposta) and (Length(resposta)>0) then
      fpSendTo(FSocket, @resposta[0], Length(resposta), 0, @quem, tam);
  end;
end;

function TServidorUDPDeTeste.Parando:Boolean;
begin
  FCS.Enter;
  try
    Result:=FTerminou;
  finally
    FCS.Leave;
  end;
end;

function TServidorUDPDeTeste.GetRecebido:BYTES;
begin
  FCS.Enter;
  try
    Result:=Copy(FRecebido, 0, Length(FRecebido));
  finally
    FCS.Leave;
  end;
end;

function TServidorUDPDeTeste.ProximaResposta(out aResposta:BYTES):Boolean;
begin
  aResposta:=nil;
  FCS.Enter;
  try
    Result:=FProximaResposta<=High(FRespostas);
    if Result then begin
      aResposta:=Copy(FRespostas[FProximaResposta], 0, Length(FRespostas[FProximaResposta]));
      inc(FProximaResposta);
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TServidorUDPDeTeste.EnfileirarResposta(const aResposta:BYTES);
begin
  FCS.Enter;
  try
    SetLength(FRespostas, Length(FRespostas)+1);
    FRespostas[High(FRespostas)]:=Copy(aResposta, 0, Length(aResposta));
  finally
    FCS.Leave;
  end;
end;

function TServidorUDPDeTeste.EsperarBytes(aQuantos, aPrazoMs:LongInt):Boolean;
var
  gasto:LongInt;
begin
  gasto:=0;
  while (Length(GetRecebido)<aQuantos) and (gasto<aPrazoMs) do begin
    Sleep(5);
    inc(gasto, 5);
  end;
  Result:=Length(GetRecebido)>=aQuantos;
end;

{ TServidorDeTeste }

constructor TServidorDeTeste.Create;
var
  endereco:TInetSockAddr;
  tam:LongInt;
  reusar:LongInt;
begin
  inherited Create;
  FCS:=TCriticalSection.Create;
  FCliente:=-1;
  FProximaResposta:=0;
  FConexoes:=0;
  FTerminou:=false;

  FOuvinte:=fpSocket(AF_INET, SOCK_STREAM, 0);
  if FOuvinte<0 then
    raise Exception.Create('servidor de teste: nao consegui criar o soquete');

  reusar:=1;
  fpSetSockOpt(FOuvinte, SOL_SOCKET, SO_REUSEADDR, @reusar, SizeOf(reusar));

  endereco.sin_family     :=AF_INET;
  endereco.sin_port       :=htons(0);          //porta efemera / ephemeral port
  endereco.sin_addr.s_addr:=htonl($7F000001);  //127.0.0.1

  if fpBind(FOuvinte, @endereco, SizeOf(endereco))<>0 then
    raise Exception.Create('servidor de teste: nao consegui reservar a porta');

  if fpListen(FOuvinte, 4)<>0 then
    raise Exception.Create('servidor de teste: nao consegui ouvir a porta');

  tam:=SizeOf(endereco);
  if fpGetSockName(FOuvinte, @endereco, @tam)<>0 then
    raise Exception.Create('servidor de teste: nao descobri a porta escolhida');
  FPorta:=htons(endereco.sin_port);

  FThread:=TThreadDoServidor.Create(Self);
end;

destructor TServidorDeTeste.Destroy;
var
  acordar:LongInt;
  endereco:TInetSockAddr;
begin
  FCS.Enter;
  try
    FTerminou:=true;
  finally
    FCS.Leave;
  end;

  SoltarAConexao;

  if FThread<>nil then begin
    FThread.Terminate;

    //a thread pode estar parada num accept: um cliente descartavel a acorda
    //the thread may be sitting in an accept: a throwaway client wakes it up
    acordar:=fpSocket(AF_INET, SOCK_STREAM, 0);
    if acordar>=0 then begin
      endereco.sin_family     :=AF_INET;
      endereco.sin_port       :=htons(FPorta);
      endereco.sin_addr.s_addr:=htonl($7F000001);
      fpConnect(acordar, @endereco, SizeOf(endereco));
      CloseSocket(acordar);
    end;

    FThread.WaitFor;
    FreeAndNil(FThread);
  end;

  if FOuvinte>=0 then
    CloseSocket(FOuvinte);

  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TServidorDeTeste.Servir;
var
  endereco:TInetSockAddr;
  tam, novo:LongInt;
begin
  while not Parando do begin
    tam:=SizeOf(endereco);
    novo:=fpAccept(FOuvinte, @endereco, @tam);

    if Parando then begin
      if novo>=0 then CloseSocket(novo);
      break;
    end;

    if novo<0 then break;

    NovaConexao(novo);
  end;
end;

procedure TServidorDeTeste.NovaConexao(aSocket:LongInt);
var
  buf:array[0..1023] of Byte;
  lidos:LongInt;
  resposta:BYTES;
begin
  FCS.Enter;
  try
    FCliente:=aSocket;
    inc(FConexoes);
  finally
    FCS.Leave;
  end;

  repeat
    lidos:=fpRecv(aSocket, @buf[0], SizeOf(buf), 0);
    if lidos<=0 then break;

    GuardarRecebido(buf, lidos);

    if ProximaResposta(resposta) and (Length(resposta)>0) then
      fpSend(aSocket, @resposta[0], Length(resposta), 0);
  until Parando;

  FCS.Enter;
  try
    if FCliente=aSocket then
      FCliente:=-1;
  finally
    FCS.Leave;
  end;
  CloseSocket(aSocket);
end;

procedure TServidorDeTeste.GuardarRecebido(const aBuf; aTamanho:LongInt);
var
  antes:LongInt;
begin
  FCS.Enter;
  try
    antes:=Length(FRecebido);
    SetLength(FRecebido, antes+aTamanho);
    Move(aBuf, FRecebido[antes], aTamanho);
  finally
    FCS.Leave;
  end;
end;

function TServidorDeTeste.ProximaResposta(out aResposta:BYTES):Boolean;
begin
  aResposta:=nil;
  FCS.Enter;
  try
    Result:=FProximaResposta<=High(FRespostas);
    if Result then begin
      aResposta:=Copy(FRespostas[FProximaResposta], 0, Length(FRespostas[FProximaResposta]));
      inc(FProximaResposta);
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TServidorDeTeste.EnfileirarResposta(const aResposta:BYTES);
begin
  FCS.Enter;
  try
    SetLength(FRespostas, Length(FRespostas)+1);
    FRespostas[High(FRespostas)]:=Copy(aResposta, 0, Length(aResposta));
  finally
    FCS.Leave;
  end;
end;

procedure TServidorDeTeste.SoltarAConexao;
var
  cliente:LongInt;
begin
  FCS.Enter;
  try
    cliente:=FCliente;
    FCliente:=-1;
  finally
    FCS.Leave;
  end;

  if cliente>=0 then begin
    fpShutdown(cliente, 2);
    CloseSocket(cliente);
  end;
end;

function TServidorDeTeste.Parando:Boolean;
begin
  FCS.Enter;
  try
    Result:=FTerminou;
  finally
    FCS.Leave;
  end;
end;

function TServidorDeTeste.GetConexoes:LongInt;
begin
  FCS.Enter;
  try
    Result:=FConexoes;
  finally
    FCS.Leave;
  end;
end;

function TServidorDeTeste.GetRecebido:BYTES;
begin
  FCS.Enter;
  try
    Result:=Copy(FRecebido, 0, Length(FRecebido));
  finally
    FCS.Leave;
  end;
end;

function TServidorDeTeste.EsperarConexoes(aQuantas, aPrazoMs:LongInt):Boolean;
var
  gasto:LongInt;
begin
  gasto:=0;
  while (GetConexoes<aQuantas) and (gasto<aPrazoMs) do begin
    Sleep(5);
    inc(gasto, 5);
  end;
  Result:=GetConexoes>=aQuantas;
end;

function TServidorDeTeste.EsperarBytes(aQuantos, aPrazoMs:LongInt):Boolean;
var
  gasto:LongInt;
begin
  gasto:=0;
  while (Length(GetRecebido)<aQuantos) and (gasto<aPrazoMs) do begin
    Sleep(5);
    inc(gasto, 5);
  end;
  Result:=Length(GetRecebido)>=aQuantos;
end;

end.
