program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Sockets, errors
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  sin,        // STANDARD IN
  sout: text; // STANDARD OUT
  socketABC:longint;
  addr: TInetSockAddr;
  line: string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  Addr.family:=AF_INET;
  addr.port := $4E04;
  addr.addr := ((1 shl 24) or 127);
  socketABC:= socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if not Connect (socketABC,ADDR,SIN,SOUT) then
  begin
    Writeln ('Couldn''t connect to localhost');
    Writeln ('Socket error : ', strerror(SocketError));
    halt(1);
  end else
    Writeln ('Connected to localhost');
  rewrite (sout);
  reset(sin);
  writeln(sout, 'HELO YourDomainName');
  writeln(sout, 'QUIT');
  flush(sout);
  while not eof(sin) do
  begin
    readln (Sin,line);
    writeln (line);
  end;
  close(sin);
  close(sout);
  writeln ('hit enter to exit');
  readln;

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMyApplication;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

