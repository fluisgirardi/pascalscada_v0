{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit que implementa um mutex de rede.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a network mutex.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit mutexserver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{$I ../common/delphiver.inc}
interface

uses
  Classes, SysUtils, ExtCtrls, CommPort, commtypes, socket_types
  {$IF defined(WIN32) or defined(WIN64)} //delphi or lazarus over windows
  , Windows, WinSock, sockets_w32_w64
  {$ELSE}
  {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
  , Sockets {$IFDEF UNIX}  , sockets_unix, netdb, Unix{$ENDIF}
            {$IFDEF WINCE} , sockets_wince {$ENDIF}
            {$IFDEF FDEBUG}, LCLProc{$ENDIF}
  {$IFEND}
  {$IFEND};

type

  { TMutexServer }

  TMutexServer = class(TComponent)
  private
    FActive,
    FActiveLoaded: Boolean;
    FPort: Word;
    FSocket: TSocket;
    procedure setActive(AValue: Boolean);
    procedure SetPort(AValue: Word);
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    { Public declarations }
  published
    property Active:Boolean read FActive write setActive stored true default false;
    property Port:Word read FPort write SetPort stored true default 52321;

  end;

procedure Register;

implementation

uses hsstrings;

procedure Register;
begin
  RegisterComponents(strUtilsPallete,[TMutexServer]);
end;

{ TMutexServer }

procedure TMutexServer.setActive(AValue: Boolean);
var
{$IF defined(FPC) and defined(UNIX)}
  ServerAddr:THostEntry;
  channel:sockaddr_in;
{$IFEND}

{$IF defined(FPC) and defined(WINCE)}
  channel:sockaddr_in;
{$IFEND}

{$IF defined(WIN32) or defined(WIN64)}
  ServerAddr:PHostEnt;
  channel:sockaddr_in;
{$IFEND}

  flag, bufsize:Integer;
begin

  if [csLoading,csReading]*ComponentState<>[] then begin
    FActiveLoaded:=AValue;
    exit;
  end;

  if FActive=AValue then Exit;

  if AValue then begin
    try
      {$IF defined(FPC) AND (defined(UNIX) or defined(WINCE))}
      //UNIX and WINDOWS CE
      FSocket := fpSocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

      if FSocket<0 then begin
        FActive:=false;
        //RefreshLastOSError;
        exit;
      end;

      {$ELSE}
      //WINDOWS
      FSocket :=   Socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
      if FSocket=INVALID_SOCKET then begin
        FActive:=false;
        //RefreshLastOSError;
        exit;
      end;
      {$ENDIF}

      channel.sin_family      := AF_INET;
      channel.sin_addr.S_addr := INADDR_ANY;
      channel.sin_port        := htons(FPort); //PORT NUMBER


      {$IF defined(FPC) AND (defined(UNIX) OR defined(WINCE))}
      if fpbind(FSocket,channel,sizeof(channel))<>0 then begin
        CloseSocket(FSocket);
        FActive:=false;
        exit;
      end;

      if listen(FSocket, SOMAXCONN)<>0 then begin
        CloseSocket(FSocket);
        FActive:=false;
        exit;
      end;
      {$IFEND}

      {$IF defined(WIN32) OR defined(WIN64)}
      if bind(FSocket,channel,sizeof(channel))<>0 then begin
        CloseSocket(FSocket);
        FActive:=false;
        exit;
      end;

      if listen(FSocket, SOMAXCONN)<>0 then begin
        CloseSocket(FSocket);
        FActive:=false;
        exit;
      end;

      //espera pela conexao???
      {$IFEND}
    finally

    end;
  end;
  FActive:=AValue;
end;

procedure TMutexServer.SetPort(AValue: Word);
begin
  if FActive then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if (AValue<1) or (AValue>65535) then
    raise exception.Create(SimpossibleToChangeWhenActive);

  if FPort=AValue then Exit;

  FPort:=AValue;
end;

procedure TMutexServer.Loaded;
begin
  inherited Loaded;
  setActive(FActiveLoaded);
end;

end.
