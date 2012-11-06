{$i ../common/language.inc}
{$IFDEF PORTUGUES}
//: Implementa funções de socket para Windows.
{$ELSE}
//: Windows socket functions.
{$ENDIF}
unit sockets_w32_w64;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  windows, winsock, socket_types, hsstrings, sysutils, commtypes;

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
  function socket_recv(sock:Tsocket; buf:PByte; len: Cardinal; flags, timeout: Integer):Integer;

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
  function socket_send(sock:Tsocket; buf:PByte; len: Cardinal; flags, timeout: Integer):Integer;

  {$IFDEF PORTUGUES}
  //: Seta o modo de operação de socket.
  {$ELSE}
  //: Sets the socket operation mode.
  {$ENDIF}
  function setblockingmode(fd:TSocket; mode:u_long):Integer;

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
  function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen; timeout:Integer):Integer;

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
  function CheckConnection(var CommResult:TIOResult; var incRetries:Boolean; var PActive:Boolean; var FSocket:TSocket; DoCommPortDisconected:TDisconnectNotifierProc):Boolean;

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
  function WaitForConnection(FListenerSocket:TSocket; timeout:Integer):Boolean;

implementation

function setblockingmode(fd:TSocket; mode:u_long):Integer;
begin
  if ioctlsocket(fd, FIONBIO, mode)=SOCKET_ERROR then
    Result:=-1
  else
    Result:=0;
end;

function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen; timeout:Integer):Integer;
var
  sel:TFDSet;
  mode:u_long;
  tv : TTimeVal;
  p:ptimeval;
begin

  if timeout=-1 then
    p:=nil
  else begin
    tv.tv_Sec:=Timeout div 1000;
    tv.tv_Usec:=(Timeout mod 1000)*1000;
    p:=@tv;
  end;

  Result:=0;

  if connect(sock, address^, address_len) <> 0 then begin
    if WSAGetLastError=WSAEWOULDBLOCK then begin
      FD_ZERO(sel);
      FD_SET(sock, sel);
      mode := select(sock, nil, @sel, nil, p);

      if (mode < 0) then begin
        Result := -1;
      end else begin
        if (mode > 0) then begin
          Result := 0;
        end else begin
          if (mode = 0) then begin
            Result := -2;
          end;
        end;
      end;
    end else
      Result := -1;
  end;
end;

function socket_recv(sock:Tsocket; buf:PByte; len: Cardinal; flags, timeout: Integer):Integer;
var
  sel:TFDSet;
  mode:u_long;
  tv : TTimeVal;
  p:ptimeval;
begin

  if timeout=-1 then
    p:=nil
  else begin
    tv.tv_Sec:=Timeout div 1000;
    tv.tv_Usec:=(Timeout mod 1000)*1000;
    p:=@tv;
  end;

  Result:=recv(sock, buf^, len, flags);

  if Result = SOCKET_ERROR then begin
    if (WSAGetLastError=WSAEWOULDBLOCK) then begin
      FD_ZERO(sel);
      FD_SET(sock, sel);
      mode := select(sock, @sel, nil, nil, p);

      if (mode < 0) then begin
        Result := -1;
      end else begin
        if (mode > 0) then begin
          Result := recv(sock, buf^, len, flags);
        end else begin
          if (mode = 0) then begin
            Result := -2;
          end;
        end;
      end;
    end else
      Result := -1;
  end;
end;

function socket_send(sock:Tsocket; buf:PByte; len: Cardinal; flags, timeout: Integer):Integer;
var
  sel:TFDSet;
  mode:u_long;
  tv : TTimeVal;
  p:ptimeval;
begin

  if timeout=-1 then
    p:=nil
  else begin
    tv.tv_Sec:=Timeout div 1000;
    tv.tv_Usec:=(Timeout mod 1000)*1000;
    p:=@tv;
  end;

  Result:=send(sock, buf^, len, flags);

  if Result = SOCKET_ERROR then begin
    if WSAGetLastError=WSAEWOULDBLOCK then begin
      FD_ZERO(sel);
      FD_SET(sock, sel);
      mode := select(sock, nil, @sel, nil, p);

      if (mode < 0) then begin
        Result := -1;
      end else begin
        if (mode > 0) then begin
          Result := send(sock, buf^, len, flags);
        end else begin
          if (mode = 0) then begin
            Result := -2;
          end;
        end;
      end;
    end else
      Result := -1;
  end;
end;

function CheckConnection(var CommResult:TIOResult; var incRetries:Boolean; var PActive:Boolean; var FSocket:TSocket; DoCommPortDisconected:TDisconnectNotifierProc):Boolean;
var
  retval, nbytes:Integer;
  t:TTimeVal;
  readset:TFDSet;
begin
  Result:=true;

  retval:=0;
  nbytes:=0;
  {$IFDEF FPC}
  retval:=ioctlsocket(FSocket,FIONREAD,@nbytes);
  {$ELSE}
  retval:=ioctlsocket(FSocket,FIONREAD,nbytes);
  {$ENDIF}
  if retval<>0 then begin
    DoCommPortDisconected();
    CommResult:=iorPortError;
    PActive:=false;
    Result:=false;
    exit;
  end;

  if (nbytes>0) then begin   // there is something in receive buffer, it doesn't seem the socket has been closed
    Result:=true;
    exit;
  end;

  t.tv_usec:=1;
  t.tv_sec:=0;

  FD_ZERO(readset);
  FD_SET(FSocket,readset);
  retval:=Select(FSocket,@readset,nil,nil,@t);

  if (retval=0) then begin   //timeout, appears to be ok...
    Result:=true;
    CommResult:=iorTimeOut;
    incRetries:=true;
    exit;
  end;

  if (retval<0) then begin //error on socket...
    DoCommPortDisconected();
    CommResult:=iorPortError;
    PActive:=false;
    Result:=false;
    exit;
  end;

  if (retval=1) then begin  // seems there is something in our receive buffer!!
    // now we check how many bytes are in receive buffer
    {$IFDEF FPC}
    retval:=ioctlsocket(FSocket,FIONREAD,@nbytes);
    {$ELSE}
    retval:=ioctlsocket(FSocket,FIONREAD,nbytes);
    {$ENDIF};

    if (retval<>0) then begin  // some error occured
      DoCommPortDisconected();
      CommResult:=iorPortError;
      PActive:=false;
      Result:=false;
      exit;
    end;

    if (nbytes=0) then begin
      DoCommPortDisconected();
      CommResult:=iorNotReady;
      PActive:=false;
      Result:=false;
      exit;
    end;

    incRetries:=true;
  end;
end;

function WaitForConnection(FListenerSocket:TSocket; timeout:Integer):Boolean;
var
  sel:TFDSet;
  mode:u_long;
  tv : TTimeVal;
  p:ptimeval;
begin

  if timeout=-1 then
    p:=nil
  else begin
    tv.tv_Sec:=Timeout div 1000;
    tv.tv_Usec:=(Timeout mod 1000)*1000;
    p:=@tv;
  end;


  FD_ZERO(sel);
  FD_SET(sock, sel);
  mode := select(sock, @sel, nil, nil, p);

  if (mode <= 0) then begin
    Result := false;
  end else
    if (mode > 0) then begin
      Result := true;
    end;
end;

{$IF defined(WIN32) or defined(WIN64)}
var
  wsaData:TWSAData;
  version:WORD;
initialization

  //inicialização Winsock
  version := MAKEWORD( 2, 0 );

  //check for error
  if WSAStartup( version, wsaData ) <> 0 then
    raise Exception.Create(SerrorInitializingWinsock);

  //check for correct version
  if (LOBYTE(wsaData.wVersion) <> 2) or (HIBYTE(wsaData.wVersion)<>0) then begin
    //incorrect WinSock version
    WSACleanup();
    raise Exception.Create(SinvalidWinSockVersion);
  end;
finalization
  WSACleanup;
{$IFEND}
end.

