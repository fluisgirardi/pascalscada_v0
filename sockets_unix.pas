unit sockets_unix;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  unix, baseunix, Sockets, socket_types;

  function setblockingmode(fd:TSocket; mode:Integer):Integer;
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
  mode:Integer;
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
    if socketerror = ESysEINPROGRESS then begin
      fpFD_ZERO(sel);
      fpFD_SET(sock, sel);
      mode := fpSelect(sock+1, nil, @sel, nil, p);

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


end.

