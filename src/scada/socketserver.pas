{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit que implementa servidor socket multithread.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a threaded socket server .)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit socketserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CrossEvent, crossthreads,
  syncobjs
  {$IF defined(WIN32) or defined(WIN64)} //delphi or lazarus over windows
    {$IFDEF FPC}
    , WinSock2,
    {$ELSE}
    , WinSock,
    {$ENDIF}
    sockets_w32_w64
  {$ELSE}
  {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
  , Sockets {$IFDEF UNIX}  , netdb, Unix{$ENDIF}
            {$IFDEF WINCE} , sockets_wince {$ENDIF}
            {$IFDEF FDEBUG}, LCLProc{$ENDIF}
  {$IFEND}
  {$IFEND};

type

  { TSocketAcceptThread }

  TSocketAcceptThread = Class(TpSCADACoreAffinityThread)
  protected
    FServerSocket:TSocket;
    FEnd:TCrossEvent;
    FClientThread:TpSCADACoreAffinityThread;
    FAddClientThread,
    FRemoveClientThread:TNotifyEvent;
    function WaitEnd(timeout:Cardinal): TWaitResult;
    procedure AddClientToMainThread;
  protected
    {$IF defined(FPC) AND defined(UNIX)}
    ClientSockInfoLen:TSocklen;
    {$ENDIF}
    ClientSockInfo:TSockAddr;
    ClientSocket:TSocket;
    procedure Execute; override;
    procedure LaunchNewThread; virtual;
  public
    procedure Terminate;
    constructor Create(CreateSuspended: Boolean;
                       ServerSocket:TSocket;
                       AddClientThread,
                       RemoveClientThread:TNotifyEvent);
  end;

  { TSocketClientThread }

  TSocketClientThread = Class(TpSCADACoreAffinityThread)
  protected
    FClientInfo:TSockAddr;
    FSocket:TSocket;
    FEnd:TCrossEvent;
    FRemoveClientThread:TNotifyEvent;
    function WaitEnd(timeout:Cardinal): TWaitResult;
    procedure ClientFinished;
  protected
    procedure Execute; override;
    procedure ThreadLoop; virtual;
  public
    procedure Terminate;
    constructor Create(CreateSuspended: Boolean;
                       ClientSocket:TSocket;
                       ClientSockinfo:TSockAddr;
                       RemoveClientThread:TNotifyEvent); virtual;
  end;

implementation

{ TSocketClientThread }

function TSocketClientThread.WaitEnd(timeout: Cardinal): TWaitResult;
begin
  Result := FEnd.WaitFor(timeout);
end;

procedure TSocketClientThread.ClientFinished;
begin
  if Assigned(FRemoveClientThread) then
    FRemoveClientThread(Self);
end;

procedure TSocketClientThread.Execute;
begin
  ThreadLoop;

  //Close the socket.
  CloseSocket(FSocket);

  //remove the connection thread from the main thread list.
  Synchronize(@ClientFinished);

  while not FEnd.SetEvent do Sleep(1);
end;

procedure TSocketClientThread.ThreadLoop;
begin

end;

procedure TSocketClientThread.Terminate;
begin
  TpSCADACoreAffinityThread(self).Terminate;
  //FEnd so' e' sinalizado dentro do Execute, e a RTL nao chama Execute quando a
  //thread criada suspensa ja' esta' terminada na hora em que enfim e' escalonada
  //- ver "if not(LThread.FTerminated)" em rtl/unix/tthread.inc. Sem olhar
  //Finished, que a RTL marca com o Execute tendo rodado ou nao, desligar o
  //servidor logo depois de ligar trava para sempre aqui.
  //FEnd is only signaled inside Execute, and the RTL does not call Execute when
  //a thread created suspended is already terminated by the time it finally gets
  //scheduled - see "if not(LThread.FTerminated)" in rtl/unix/tthread.inc.
  //Without looking at Finished, which the RTL sets whether Execute ran or not,
  //shutting the server down right after starting it hangs here forever.
  repeat
     CheckSynchronize(1);
  until (WaitEnd(1)=wrSignaled) or Finished;
  FEnd.Destroy;
end;

constructor TSocketClientThread.Create(CreateSuspended: Boolean;
  ClientSocket: TSocket; ClientSockinfo: TSockAddr;
  RemoveClientThread: TNotifyEvent);
begin
  inherited Create(CreateSuspended);
  FSocket             := ClientSocket;
  FClientInfo         := ClientSockinfo;
  FEnd                := TCrossEvent.Create(true, false);
  FRemoveClientThread :=RemoveClientThread;
  FEnd.ResetEvent;
end;

{ TSocketAcceptThread }

function TSocketAcceptThread.WaitEnd(timeout: Cardinal): TWaitResult;
begin
  Result := FEnd.WaitFor(timeout);
end;

procedure TSocketAcceptThread.AddClientToMainThread;
begin
  if Assigned(FAddClientThread) then
    FAddClientThread(FClientThread);
end;

procedure TSocketAcceptThread.Execute;
begin
  while not Terminated do begin
    //Linux, BSDs
    {$IF defined(FPC) AND defined(UNIX)}
    ClientSockInfoLen:=sizeof(ClientSockInfo);
    ClientSocket:=fpAccept(FServerSocket, @ClientSockInfo, @ClientSockInfoLen);

    if ClientSocket>0 then
      LaunchNewThread
    else
      Sleep(5);
    {$IFEND}

    //WINCE
    {$IF defined(FPC) AND defined(WINCE)}
    ClientSocket:=fpAccept(FServerSocket,nil,nil);

    if ClientSocket<>INVALID_SOCKET then
      LaunchNewThread
    else
      Sleep(5);
    {$IFEND}

    //WINDOWS
    {$IF defined(WIN32) or defined(WIN64)}
    ClientSocket:=  Accept(FServerSocket,nil,nil);

    if ClientSocket<>INVALID_SOCKET then
      LaunchNewThread
    else
      Sleep(5);
    {$IFEND}
  end;
  while not FEnd.SetEvent do Sleep(1);
end;

procedure TSocketAcceptThread.LaunchNewThread;
begin

end;

procedure TSocketAcceptThread.Terminate;
begin
  TpSCADACoreAffinityThread(self).Terminate;
  //: @seealso(TSocketClientThread.Terminate) para o porque do teste de Finished
  repeat
     CheckSynchronize(1);
  until (WaitEnd(1)=wrSignaled) or Finished;
  FEnd.Destroy;
end;

constructor TSocketAcceptThread.Create(CreateSuspended: Boolean;
  ServerSocket: TSocket; AddClientThread, RemoveClientThread: TNotifyEvent);
begin
  inherited Create(CreateSuspended);
  FServerSocket       := ServerSocket;
  FAddClientThread    := AddClientThread;
  FRemoveClientThread := RemoveClientThread;
  FEnd                := TCrossEvent.Create(true, false);

  FEnd.ResetEvent;
end;

end.

