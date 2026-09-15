{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Servidor HTTP de mentira, para testar quem fala com um servidor de
            autenticacao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Aceita uma conexao por vez, le a requisicao inteira - cabecalho mais corpo,
  pelo Content-Length - guarda o caminho e o corpo que chegaram, responde o que
  o teste mandou responder e fecha. E' o bastante para um cliente de API JSON.
}
{$ELSE}
{:
  @abstract(A pretend HTTP server, to test whoever talks to an authentication
            server.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It takes one connection at a time, reads the whole request - headers plus
  body, by Content-Length - records the path and the body that arrived, answers
  whatever the test told it to answer and closes. That is enough for a JSON API
  client.
}
{$ENDIF}
unit testsupport.httpstub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, Sockets;

type

  { TServidorHTTPDeTeste }

  TServidorHTTPDeTeste = class
  private
    FOuvinte:LongInt;
    FPorta:LongInt;
    FCS:TCriticalSection;
    FThread:TThread;
    FTerminou:Boolean;
    FStatus:Integer;
    FCorpoResposta:UTF8String;
    FUltimoCaminho:UTF8String;
    FUltimoCorpo:UTF8String;
    FRequisicoes:LongInt;
    function  Parando:Boolean;
    function  GetRequisicoes:LongInt;
    function  GetUltimoCaminho:UTF8String;
    function  GetUltimoCorpo:UTF8String;
    procedure Atender(aSocket:LongInt);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Servir;

    {$IFDEF PORTUGUES}
    //: define o que o servidor vai responder daqui em diante.
    {$ELSE}
    //: sets what the server will answer from now on.
    {$ENDIF}
    procedure Responder(aStatus:Integer; const aCorpo:UTF8String);

    {$IFDEF PORTUGUES}
    //: espera ate' ter atendido aQuantas requisicoes, ou ate' o prazo acabar.
    {$ELSE}
    //: waits until aQuantas requests have been served, or the deadline passes.
    {$ENDIF}
    function EsperarRequisicoes(aQuantas, aPrazoMs:LongInt):Boolean;

    property Porta:LongInt read FPorta;
    property Requisicoes:LongInt read GetRequisicoes;
    property UltimoCaminho:UTF8String read GetUltimoCaminho;
    property UltimoCorpo:UTF8String read GetUltimoCorpo;
  end;

implementation

type

  { TThreadDoServidorHTTP }

  TThreadDoServidorHTTP = class(TThread)
  private
    FDono:TServidorHTTPDeTeste;
  protected
    procedure Execute; override;
  public
    constructor Create(aDono:TServidorHTTPDeTeste);
  end;

constructor TThreadDoServidorHTTP.Create(aDono:TServidorHTTPDeTeste);
begin
  FDono:=aDono;
  FreeOnTerminate:=false;
  inherited Create(false);
end;

procedure TThreadDoServidorHTTP.Execute;
begin
  FDono.Servir;
end;

{ TServidorHTTPDeTeste }

constructor TServidorHTTPDeTeste.Create;
var
  endereco:TInetSockAddr;
  tam, reusar:LongInt;
begin
  inherited Create;
  FCS:=TCriticalSection.Create;
  FTerminou:=false;
  FStatus:=200;
  FCorpoResposta:='{}';
  FRequisicoes:=0;

  FOuvinte:=fpSocket(AF_INET, SOCK_STREAM, 0);
  if FOuvinte<0 then
    raise Exception.Create('http stub: could not create the socket');

  reusar:=1;
  fpSetSockOpt(FOuvinte, SOL_SOCKET, SO_REUSEADDR, @reusar, SizeOf(reusar));

  endereco.sin_family     :=AF_INET;
  endereco.sin_port       :=htons(0);
  endereco.sin_addr.s_addr:=htonl($7F000001);

  if fpBind(FOuvinte, @endereco, SizeOf(endereco))<>0 then
    raise Exception.Create('http stub: could not bind the port');

  if fpListen(FOuvinte, 4)<>0 then
    raise Exception.Create('http stub: could not listen on the port');

  tam:=SizeOf(endereco);
  if fpGetSockName(FOuvinte, @endereco, @tam)<>0 then
    raise Exception.Create('http stub: could not find the port it was given');
  FPorta:=htons(endereco.sin_port);

  FThread:=TThreadDoServidorHTTP.Create(Self);
end;

destructor TServidorHTTPDeTeste.Destroy;
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

function TServidorHTTPDeTeste.Parando:Boolean;
begin
  FCS.Enter;
  try
    Result:=FTerminou;
  finally
    FCS.Leave;
  end;
end;

function TServidorHTTPDeTeste.GetRequisicoes:LongInt;
begin
  FCS.Enter;
  try
    Result:=FRequisicoes;
  finally
    FCS.Leave;
  end;
end;

function TServidorHTTPDeTeste.GetUltimoCaminho:UTF8String;
begin
  FCS.Enter;
  try
    Result:=FUltimoCaminho;
  finally
    FCS.Leave;
  end;
end;

function TServidorHTTPDeTeste.GetUltimoCorpo:UTF8String;
begin
  FCS.Enter;
  try
    Result:=FUltimoCorpo;
  finally
    FCS.Leave;
  end;
end;

procedure TServidorHTTPDeTeste.Responder(aStatus:Integer; const aCorpo:UTF8String);
begin
  FCS.Enter;
  try
    FStatus:=aStatus;
    FCorpoResposta:=aCorpo;
  finally
    FCS.Leave;
  end;
end;

function TServidorHTTPDeTeste.EsperarRequisicoes(aQuantas, aPrazoMs:LongInt):Boolean;
var
  limite:QWord;
begin
  limite:=GetTickCount64+aPrazoMs;
  while (GetRequisicoes<aQuantas) and (GetTickCount64<limite) do
    Sleep(2);
  Result:=GetRequisicoes>=aQuantas;
end;

procedure TServidorHTTPDeTeste.Atender(aSocket:LongInt);
var
  buf:array[0..2047] of Byte;
  lidos, fimCabecalho, tamanhoCorpo, p:LongInt;
  recebido, cabecalho, corpo, linha, resposta, status:UTF8String;
begin
  recebido:='';
  fimCabecalho:=0;

  //cabecalho ate' a linha em branco / headers up to the blank line
  while not Parando do begin
    lidos:=fpRecv(aSocket, @buf[0], SizeOf(buf), 0);
    if lidos<=0 then break;
    SetLength(linha, lidos);
    Move(buf[0], linha[1], lidos);
    recebido:=recebido+linha;
    fimCabecalho:=Pos(#13#10#13#10, recebido);
    if fimCabecalho>0 then break;
  end;

  if fimCabecalho=0 then begin
    CloseSocket(aSocket);
    exit;
  end;

  cabecalho:=Copy(recebido, 1, fimCabecalho+3);
  corpo    :=Copy(recebido, fimCabecalho+4, Length(recebido));

  //o corpo que faltar, pelo Content-Length / whatever body is missing, by
  //Content-Length
  tamanhoCorpo:=0;
  p:=Pos('content-length:', LowerCase(cabecalho));
  if p>0 then begin
    linha:=Copy(cabecalho, p+15, 20);
    tamanhoCorpo:=StrToIntDef(Trim(Copy(linha, 1, Pos(#13, linha+#13)-1)), 0);
  end;

  while (Length(corpo)<tamanhoCorpo) and (not Parando) do begin
    lidos:=fpRecv(aSocket, @buf[0], SizeOf(buf), 0);
    if lidos<=0 then break;
    SetLength(linha, lidos);
    Move(buf[0], linha[1], lidos);
    corpo:=corpo+linha;
  end;

  //o caminho pedido, da primeira linha / the requested path, from the first
  //line
  linha:=Copy(cabecalho, 1, Pos(#13#10, cabecalho)-1);
  p:=Pos(' ', linha);
  linha:=Copy(linha, p+1, Length(linha));
  p:=Pos(' ', linha);
  if p>0 then linha:=Copy(linha, 1, p-1);

  FCS.Enter;
  try
    FUltimoCaminho:=linha;
    FUltimoCorpo:=corpo;
    inc(FRequisicoes);
    case FStatus of
      200: status:='200 OK';
      401: status:='401 Unauthorized';
      403: status:='403 Forbidden';
      404: status:='404 Not Found';
      405: status:='405 Method Not Allowed';
      500: status:='500 Internal Server Error';
      else status:=IntToStr(FStatus)+' Status';
    end;
    resposta:='HTTP/1.1 '+status+#13#10+
              'Content-Type: application/json'#13#10+
              'Content-Length: '+IntToStr(Length(FCorpoResposta))+#13#10+
              'Connection: close'#13#10#13#10+
              FCorpoResposta;
  finally
    FCS.Leave;
  end;

  if Length(resposta)>0 then
    fpSend(aSocket, @resposta[1], Length(resposta), 0);

  CloseSocket(aSocket);
end;

procedure TServidorHTTPDeTeste.Servir;
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

    Atender(novo);
  end;
end;

end.
