unit sockets_unix;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Sockets, socket_types;

  function setblockingmode(fd:TSocket; block:boolean):Integer;
  function connect_with_timeout(sock:Tsocket; address:PSockAddr; address_len:t_socklen; timeout:Integer):Integer;

implementation

function setblockingmode(fd:TSocket; mode:Integer):Integer;
var
  oldflags:Integer;
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

