unit sockets_wince;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  windows, Sockets, socket_types, commtypes;

  function socket_recv(sock:Tsocket; buf: pointer; len: Cardinal; flags, timeout: Integer):Integer;
  function socket_send(sock:Tsocket; buf: pointer; len: Cardinal; flags, timeout: Integer):Integer;
  function setblockingmode(fd:TSocket; mode:dword):Integer;
  function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen; timeout:Integer):Integer;
  function CheckConnection(var CommResult:TIOResult; var incRetries:Boolean; var PActive:Boolean; var FSocket:TSocket; DoCommPortDisconected:TDisconnectNotifierProc):Boolean;

implementation

uses winsock;

function setblockingmode(fd:sockets.Tsocket; mode:u_long):Integer;
begin
  if ioctlsocket(fd, FIONBIO, mode)=SOCKET_ERROR then
    Result:=-1
  else
    Result:=0;
end;

function connect_with_timeout(sock:sockets.Tsocket; address:sockets.psockaddr; address_len:t_socklen; timeout:Integer):Integer;
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

  if fpconnect(sock, address, address_len) <> 0 then begin
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

function socket_recv(sock:sockets.Tsocket; buf: pointer; len: Cardinal; flags, timeout: Integer):Integer;
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

  Result:=fprecv(sock, buf, len, flags);

  if Result = SOCKET_ERROR then begin
    if WSAGetLastError=WSAEWOULDBLOCK then begin
      FD_ZERO(sel);
      FD_SET(sock, sel);
      mode := select(sock, @sel, nil, nil, p);

      if (mode < 0) then begin
        Result := -1;
      end else begin
        if (mode > 0) then begin
          Result := fprecv(sock, buf, len, flags);
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

function socket_send(sock:sockets.Tsocket; buf: pointer; len: Cardinal; flags, timeout: Integer):Integer;
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

  Result:=fpsend(sock, buf, len, flags);

  if Result = SOCKET_ERROR then begin
    if WSAGetLastError=WSAEWOULDBLOCK then begin
      FD_ZERO(sel);
      FD_SET(sock, sel);
      mode := select(sock, nil, @sel, nil, p);

      if (mode < 0) then begin
        Result := -1;
      end else begin
        if (mode > 0) then begin
          Result := fpsend(sock, buf, len, flags);
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
begin
  case WSAGetLastError of
    WSANOTINITIALISED,
    WSAENETDOWN,
    WSAEFAULT,
    WSAENETRESET,
    WSAENOTSOCK,
    WSAECONNABORTED,
    WSAENOTCONN,
    WSAESHUTDOWN: begin
      PActive:=false;
      CommResult := iorNotReady;
      Result := false;
    end;

    WSAEOPNOTSUPP: begin
      PActive:=false;
      CommResult:=iorPortError;
      Result:=false;
    end;

    WSAEINVAL,
    WSAEMSGSIZE: begin
      Result := false;
      CommResult := iorPortError;
    end;

    WSAEWOULDBLOCK:
      Result := true;

    WSAEINPROGRESS,
    WSAEINTR: begin
      Result:=true;
      incRetries:=false;
    end;
    WSAETIMEDOUT: begin
      if PActive then DoCommPortDisconected;
      PActive:=false;
      Shutdown(FSocket,2);
      CloseSocket(FSocket);
      CommResult:=iorTimeOut;
      Result:=false;
    end;
  end;
end;

end.
