unit socket_types;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  {$IF defined(WIN32) or defined(WIN64)} //delphi ou lazarus sobre windows
  WinSock;
  {$ELSE}
  {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
  Sockets;
  {$IFEND}
  {$IFEND}

type

  //@exclude
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400)}
  {$IF defined(WIN32) or defined(WIN64)}
  t_socklen = tOS_INT;
  {$ELSE}
  t_socklen = TSockLen;
  {$IFEND}
  {$ELSE}
  t_socklen = Integer;
  {$IFEND}

  {:
  Enumera os tipos de porta.
  @value ptTCP = Porta cliente do tipo TCP.
  @value ptUDP = Porta cliente do tipo UDP.
  }
  TPortType = (ptTCP, ptUDP);

const
  MODE_NONBLOCKING = 1;
  MODE_BLOCKING = 0;

implementation

end.

