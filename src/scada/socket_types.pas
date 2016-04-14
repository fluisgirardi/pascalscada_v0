{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
@abstract(Tipos usados por sockets.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
@abstract(Some types used by sockets.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ENDIF}
unit socket_types;

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
  t_socklen = LongInt;
  {$IFEND}

  {$IFDEF PORTUGUES}
  {:
  Enumera os tipos de porta.
  @value ptTCP = Porta cliente do tipo TCP.
  @value ptUDP = Porta cliente do tipo UDP.
  }
  {$ELSE}
  {:
  Enumerates all kinds of client ports.
  @value ptTCP = TCP port.
  @value ptUDP = UDP port.
  }
  {$ENDIF}
  TPortType = (ptTCP, ptUDP);

  TDisconnectNotifierProc = procedure of object;

const
  {$IFDEF PORTUGUES}
  //: Define o modo de não bloqueio do socket (não espera a ação terminar).
  {$ELSE}
  //: Defines the non-blocking mode of socket (don't waits the end of the action).
  {$ENDIF}
  MODE_NONBLOCKING = 1;

  {$IFDEF PORTUGUES}
  //: Define o modo de bloqueio do socket (espera a ação terminar).
  {$ELSE}
  //: Defines the blocking mode of socket (waits the end of the action).
  {$ENDIF}
  MODE_BLOCKING = 0;

implementation

end.

