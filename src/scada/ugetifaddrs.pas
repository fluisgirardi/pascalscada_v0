unit ugetifaddrs;

{$mode objfpc}{$H+}

interface

uses ctypes, Sockets;

type
  Pifaddrs = ^ifaddrs;
  ifaddrs = record
    ifa_next: Pifaddrs;
    ifa_name: PAnsiChar;
    ifa_flags: cuint;
    ifa_addr: Psockaddr;
    ifa_netmask: Psockaddr;
    ifa_dstaddr: Psockaddr;
    ifa_data: Pointer;
  end platform;
  PPifaddrs = ^Pifaddrs;

  {$EXTERNALSYM ifaddrs}

 function getifaddrs(ifap: PPifaddrs): Integer; cdecl; external 'libc.so' name 'getifaddrs';
 procedure freeifaddrs(ifap: pifaddrs); cdecl; external 'libc.so' name 'freeifaddrs';

 function IPv4BelongsToLocalHost(aIPv4Addr:String):Boolean;

implementation

uses sysutils;

function IPv4BelongsToLocalHost(aIPv4Addr: String): Boolean;
var
  iadr: pifaddrs;
  curif: Pifaddrs;
  res: Integer;
begin
  res:=getifaddrs(@iadr);
  if res>=0 then begin
    try
      curif:=iadr;
      repeat
        if curif^.ifa_addr<>nil then begin
          if curif^.ifa_addr^.sa_family = AF_INET then begin
            if aIPv4Addr=Format('%d.%d.%d.%d', [curif^.ifa_addr^.sin_addr.s_bytes[1],
                                                curif^.ifa_addr^.sin_addr.s_bytes[2],
                                                curif^.ifa_addr^.sin_addr.s_bytes[3],
                                                curif^.ifa_addr^.sin_addr.s_bytes[4]]) then begin
              exit(true);
            end;
          end;
        end;
        curif:=curif^.ifa_next;
      until curif=nil;
    finally
      freeifaddrs(iadr);
    end;
  end;

  exit(false);
end;

end.
