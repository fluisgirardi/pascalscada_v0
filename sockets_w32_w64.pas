unit sockets_w32_w64;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  windows, winsock, socket_types, hsstrings, sysutils;

  function socket_recv(sock:Tsocket; buf: pointer; len: Cardinal; flags, timeout: Integer):Integer;
  function socket_send(sock:Tsocket; buf: pointer; len: Cardinal; flags, timeout: Integer):Integer;
  function setblockingmode(fd:TSocket; mode:u_long):Integer;
  function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen; timeout:Integer):Integer;

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
      FD_SET(sock+1, sel);
      mode := select(sock+1, nil, @sel, nil, p);

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

function socket_recv(sock:Tsocket; buf: pointer; len: Cardinal; flags, timeout: Integer):Integer;
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

  Result:=recv(sock, buf, len, flags);

  if Result = SOCKET_ERROR then begin
    if (WSAGetLastError=WSAEWOULDBLOCK) then begin
      FD_ZERO(sel);
      FD_SET(sock, sel);
      mode := select(sock, @sel, nil, nil, p);

      if (mode < 0) then begin
        Result := -1;
      end else begin
        if (mode > 0) then begin
          Result := recv(sock, buf, len, flags);
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

function socket_send(sock:Tsocket; buf: pointer; len: Cardinal; flags, timeout: Integer):Integer;
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

  Result:=send(sock, buf, len, flags);

  if Result = SOCKET_ERROR then begin
    if WSAGetLastError=WSAEWOULDBLOCK then begin
      FD_ZERO(sel);
      FD_SET(sock, sel);
      mode := select(sock, nil, @sel, nil, p);

      if (mode < 0) then begin
        Result := -1;
      end else begin
        if (mode > 0) then begin
          Result := send(sock, buf, len, flags);
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

