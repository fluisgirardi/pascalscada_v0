{$i ../common/language.inc}
{$IFDEF PORTUGUES}
//: Implementa funções de socket para unix.
{$ELSE}
//: Unix socket functions.
{$ENDIF}
unit sockets_unix;

interface

uses
  unix, baseunix, Sockets, socket_types, commtypes, termio;

  {$IFDEF PORTUGUES}
  {:
  Seta o modo de operação de socket.
  @seealso(MODE_NONBLOCKING)
  @seealso(MODE_BLOCKING)
  }
  {$ELSE}
  {:
  Sets the socket operation mode.
  @seealso(MODE_NONBLOCKING)
  @seealso(MODE_BLOCKING)
  }
  {$ENDIF}
  function setblockingmode(fd:TSocket; mode:LongInt):LongInt;

  {$IFDEF PORTUGUES}
  {:
  Função de conexão com timeout. Seus parametros são identicos a função
  connect/fpconnect, porem adicionado o tempo máximo de espera pelo estabelecimento
  da conexão em milisegundos.
  @returns(0 se a conexão foi estabelecida com sucesso.)
  }
  {$ELSE}
  {:
  Connect function with timeout. Their parameters are the same of the functions
  connect/fpconnect, with a extra parameter that is the maximum timeout of the
  connection establishment in milliseconds.
  @returns(0 if the connection was estabilished successful.)
  }
  {$ENDIF}
  function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen; timeout:LongInt):LongInt;
  function connect_without_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen):LongInt;

  {$IFDEF PORTUGUES}
  {:
  Função que recebe dados do socket. Seus parametros são identicos a da função
  recv/fprecv, adicionado do parametro timeout, que diz o tempo máximo para
  receber todos os dados solicitados.
  }
  {$ELSE}
  {:
  Function that receive data of a socket. Their parameters are the same of the
  function recv/fprecv, with a extra parameter that is the maximum timout to
  receive all requested data on socket.
  }
  {$ENDIF}
  function socket_recv(sock:Tsocket; buf:PByte; len: Cardinal; flags, timeout: LongInt):LongInt;

  {$IFDEF PORTUGUES}
  {:
  Função que envia dados ao socket. Seus parametros são identicos a da função
  send/fpsend, adicionado do parametro timeout, que diz o tempo máximo para
  enviar todos os dados.
  }
  {$ELSE}
  {:
  Function that sends data through the socket. Their parameters are the same of
  the function send/fpsend, with a extra parameter that is the maximum timout to
  send all requested data.
  }
  {$ENDIF}
  function socket_send(sock:Tsocket; buf:PByte; len: Cardinal; flags, timeout: LongInt):LongInt;

  {$IFDEF PORTUGUES}
  {:
  Verifica o estado da conexão e atualiza o estado da porta da comunicação.
  @returns(@True se ainda está conectado)
  }
  {$ELSE}
  {:
  Check the current connection state and updates the state of the communication port.
  @returns(@True if stills connected.)
  }
  {$ENDIF}
  function CheckConnection(var CommResult:TIOResult; var incRetries:Boolean; var FSocket:TSocket; CloseSocketProc:TConnectEvent; DoCommPortDisconected:TDisconnectNotifierProc):Boolean;

  {$IFDEF PORTUGUES}
  {:
  Espera por uma conexao de entrada
  @returns(@True se uma conexao de entrada foi realizada)
  }
  {$ELSE}
  {:
  Waits for a incoming connection.
  @returns(@True if a incoming connection was done.)
  }
  {$ENDIF}
  function WaitForConnection(FListenerSocket:TSocket; timeout:LongInt):Boolean;

  {$IFDEF PORTUGUES}
  {:
  Função que informa quantos bytes estão disponíveis para serem lidos.
  @returns(Um valor maior que zero caso existir dados disponíveis no buffer,
           zero caso não exista ou -1 em caso de erro.)
  }
  {$ELSE}
  {:
  Rerturn how many bytes are available on receive buffer.
  @returns(A value bigger than zero if data are available on the receive
           buffer, zero if no data on the receive buffer and -1 on error.)
  }
  {$ENDIF}
  function GetNumberOfBytesInReceiveBuffer(socket:Tsocket):LongInt;
implementation

function setblockingmode(fd:TSocket; mode:LongInt):LongInt;
var
  oldflags:LongInt;
begin
  oldflags := FpFcntl(fd, F_GETFL, 0);
  if (oldflags < 0) then begin
    Result:= oldflags;
    exit;
  end;

  if mode=MODE_NONBLOCKING then
    oldflags := oldflags or O_NONBLOCK
  else
    oldflags := oldflags xor O_NONBLOCK;

  Result:= FpFcntl(fd, F_SETFL, oldflags);
end;

function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen; timeout:LongInt):LongInt;
var
  sel:tpollfd;
  mode:LongInt;
begin
  Result:=0;

  if fpconnect(sock, address, address_len) <> 0 then begin
    if fpGetErrno = ESysEINPROGRESS then begin

      sel.fd:=sock;
      sel.events:=POLLIN or POLLPRI or POLLOUT;
      sel.revents:=0;

      mode := FpPoll(@sel,1,timeout);

      if (mode > 0) then begin
        if ((sel.revents and POLLERR)=POLLERR) or ((sel.revents and POLLHUP)=POLLHUP) or ((sel.revents and POLLNVAL)=POLLNVAL) then
          Result:=-1  //error
        else
          Result:=0;  //connection is fine.
      end else begin
        if mode=0 then
          Result:=-2  //timeout?
        else
          Result:=-1; //error.
      end;
    end else
      Result := -1;   //error.
  end;
end;

function connect_without_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen):LongInt;
var
  sel:tpollfd;
  mode:LongInt;
begin
  Result:=0;

  if fpconnect(sock, address, address_len) <> 0 then begin
    Result := -1;   //error.
  end;
end;

function socket_recv(sock:Tsocket; buf:PByte; len: Cardinal; flags, timeout: LongInt):LongInt;
var
  sel:tpollfd;
  mode:LongInt;
begin
  Result:=fprecv(sock, buf, len, flags or msg_nosignal);

  if  Result < 0 then begin
    if fpGetErrno in [ESysEINTR, ESysEAGAIN] then begin
      sel.fd:=sock;
      sel.events:=POLLIN;

      mode := FpPoll(@sel, 1, timeout);

      if (mode > 0) then begin
        if ((sel.revents and POLLERR)=POLLERR) or ((sel.revents and POLLHUP)=POLLHUP) or ((sel.revents and POLLNVAL)=POLLNVAL) then
          Result:=-1  //error
        else
          Result:=fprecv(sock, buf, len, flags);  //connection is fine.
      end else begin
        if mode=0 then
          Result:=-2  //timeout?
        else
          Result:=-1; //error.
      end;
    end else
      Result := -1;
  end;
end;

function socket_send(sock:Tsocket; buf:PByte; len: Cardinal; flags, timeout: LongInt):LongInt;
var
  sel:tpollfd;
  mode:LongInt;
begin
  Result:=fpsend(sock, buf, len, flags or msg_nosignal);

  if Result < 0 then begin
    if fpGetErrno in [ESysEINTR, ESysEAGAIN] then begin
      sel.fd:=sock;
      sel.events:=POLLOUT;

      mode := FpPoll(@sel, 1, timeout);

      if (mode > 0) then begin
        if ((sel.revents and POLLERR)=POLLERR) or ((sel.revents and POLLHUP)=POLLHUP) or ((sel.revents and POLLNVAL)=POLLNVAL) then
          Result:=-1  //error
        else
          Result:=fpsend(sock, buf, len, flags);  //connection is fine.
      end else begin
        if mode=0 then
          Result:=-2  //timeout?
        else
          Result:=-1; //error.
      end;
    end else
      Result := -1;
  end;
end;

function CheckConnection(var CommResult: TIOResult; var incRetries: Boolean;
  var FSocket: TSocket; CloseSocketProc: TConnectEvent;
  DoCommPortDisconected: TDisconnectNotifierProc): Boolean;
var
  retval, nbytes:LongInt;
  sel:tpollfd;
  closed: Boolean;
begin
  Result:=true;

  retval:=0;
  nbytes:=0;
  retval:=FpIOCtl(FSocket,FIONREAD,@nbytes);

  if retval<>0 then begin
    if Assigned(CloseSocketProc) then CloseSocketProc(closed);
    if Assigned(DoCommPortDisconected) then
      DoCommPortDisconected();
    CommResult:=iorPortError;
    Result:=false;
    exit;
  end;

  if (nbytes>0) then begin   // there is something in receive buffer, it doesn't seem the socket has been closed
    Result:=true;
    exit;
  end;


  sel.fd:=FSocket;
  sel.events:=POLLIN or POLLOUT or POLLPRI;
  sel.revents:=0;

  retval:=FpPoll(@sel,1,1);

  if (retval=0) then begin   //timeout, appears to be ok...
    Result:=true;
    CommResult:=iorTimeOut;
    incRetries:=true;
    exit;
  end;

  if (retval<0) then begin //error on socket...
    if Assigned(CloseSocketProc) then CloseSocketProc(closed);
    if Assigned(DoCommPortDisconected) then
      DoCommPortDisconected();
    CommResult:=iorPortError;
    Result:=false;
    exit;
  end;

  if (retval=1) then begin  // seems there is something in our receive buffer!!
    if ((sel.revents and POLLERR)=POLLERR) or ((sel.revents and POLLHUP)=POLLHUP) or ((sel.revents and POLLNVAL)=POLLNVAL) then begin
      if Assigned(CloseSocketProc) then CloseSocketProc(closed);
      if Assigned(DoCommPortDisconected) then
        DoCommPortDisconected();
      CommResult:=iorPortError;
      Result:=false;
      exit;
    end;

    // now we check how many bytes are in receive buffer
    retval:=FpIOCtl(FSocket,FIONREAD,@nbytes);

    if (retval<>0) then begin  // some error occured
      if Assigned(CloseSocketProc) then CloseSocketProc(closed);
      if Assigned(DoCommPortDisconected) then
        DoCommPortDisconected();
      CommResult:=iorPortError;
      Result:=false;
      exit;
    end;

    if (nbytes=0) then begin
      if Assigned(CloseSocketProc) then CloseSocketProc(closed);
      if Assigned(DoCommPortDisconected) then
        DoCommPortDisconected();
      CommResult:=iorNotReady;
      Result:=false;
      exit;
    end;

    incRetries:=true;
  end;
end;

function WaitForConnection(FListenerSocket:TSocket; timeout:LongInt):Boolean;
var
  sel:tpollfd;
  mode:LongInt;
begin
  Result := false;

  sel.fd:=FListenerSocket;
  sel.events:=POLLIN or POLLPRI or POLLOUT;
  sel.revents:=0;

  mode := FpPoll(@sel,1,timeout);

  if (mode > 0) then begin
    if ((sel.revents and POLLERR)=POLLERR) or ((sel.revents and POLLHUP)=POLLHUP) or ((sel.revents and POLLNVAL)=POLLNVAL) then
      Result:=False  //error
    else
      Result:=True;  //connection is fine.
  end else begin
    if mode=0 then
      Result:=false  //timeout?
    else
      Result:=false; //error.
  end;
end;

function GetNumberOfBytesInReceiveBuffer(socket: Tsocket): LongInt;
var
  retval, nbytes:LongInt;
begin
  Result:=0;

  retval:=FpIOCtl(socket,FIONREAD,@nbytes);

  if retval<>0 then begin
    Result:=-1;
    exit;
  end;

  if (nbytes>0) then
    Result:=nbytes;
end;

end.
