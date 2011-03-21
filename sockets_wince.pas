unit sockets_wince;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  winsock, socket_types;

  function setblockingmode(fd:TSocket; mode:u_long):Integer;
  function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen; timeout:Integer):Integer;

implementation

function setblockingmode(fd:TSocket; mode:u_long):Integer;
begin
  if ioctlsocket(sock, FIONBIO, mode)=SOCKET_ERROR then
    Result:=-1;
  else
    Result:=0;
end;

function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen; timeout:Integer):Integer;
var
  sel:TFDSet;
  ret:Integer;
  mode:u_long;
  tv : TTimeVal;
  p:ptimeval;
label
  cleanup;
begin

  if timeout=-1 then
    p:=nil
  else begin
    tv.tv_Sec:=Timeout div 1000;
    tv.tv_Usec:=(Timeout mod 1000)*1000;
    p:=@tv;
  end;

  if connect(sock, address^, address_len) <> 0 then
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


end.

