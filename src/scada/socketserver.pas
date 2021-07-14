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
  Classes, SysUtils, socket_types, CrossEvent, crossthreads,
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
  , Sockets {$IFDEF UNIX}  , sockets_unix, netdb, Unix{$ENDIF}
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
  repeat
     CheckSynchronize(1);
  until WaitEnd(1)=wrSignaled;
  FEnd.Destroy;
end;

constructor TSocketClientThread.Create(CreateSuspended: Boolean;
  ClientSocket: TSocket; RemoveClientThread: TNotifyEvent);
begin
  inherited Create(CreateSuspended);
  FSocket             := ClientSocket;
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
    ClientSocket:=fpAccept(FServerSocket,nil,nil);

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
  repeat
     CheckSynchronize(1);
  until WaitEnd(1)=wrSignaled;
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

