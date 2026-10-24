program s7commplus_sub211_rawread_test;

{: Teste isolado: tenta ler DB4 por endereco bruto (offset de bytes), sem passar por
   Explore/BrowseDB (que falha no .211 com SystemEvent fatal) e sem usar LIDs simbolicos -
   testa a hipotese de que o TisMonitorJob que o TIA usa para monitorar a DB4 no editor
   endereca por offset bruto (nao por LID resolvido via TypeInfo), e que talvez esse
   caminho de leitura nao passe pelo mesmo bloqueio.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, tcp_udpport, S7PlusConnection, S7PlusTypes;

type
  TDebugSink = class
    procedure OnDebug(Sender:TObject; const Msg:String);
  end;

procedure TDebugSink.OnDebug(Sender:TObject; const Msg:String);
begin
  WriteLn('  [debug] ', Msg);
end;

var
  Port:TTCP_UDPPort;
  Conn:TS7PlusConnection;
  DebugSink:TDebugSink;
  Host:String;
  WaitMs, k:Integer;
  Data:TBytes;
  Ok:Boolean;
begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> [notls]');
    Halt(1);
  end;

  Host := ParamStr(1);

  Port := TTCP_UDPPort.Create(nil);
  DebugSink := TDebugSink.Create;
  try
    Port.Host := Host;
    Port.Port := 102;
    Port.Timeout := 8000;
    Port.EnableAutoReconnect := false;

    WriteLn('Abrindo porta TCP com ', Host, ':102 ...');
    Port.Active := true;
    WaitMs := 0;
    while (not Port.ReallyActive) and (WaitMs<5000) do begin
      Sleep(100);
      Inc(WaitMs, 100);
    end;

    if not Port.ReallyActive then begin
      WriteLn('Falha ao abrir a porta TCP (timeout).');
      Halt(2);
    end;
    WriteLn('Porta TCP aberta.');

    Conn := TS7PlusConnection.Create(Port, 1);
    try
      Conn.OnDebug := @DebugSink.OnDebug;
      if (ParamCount>=2) and (ParamStr(2)='notls') then
        Conn.UseTLS := false;

      if not Conn.Connect('') then begin
        WriteLn('Falha ao conectar/montar sessao.');
        Halt(3);
      end;

      WriteLn('=== DBRead(4, offset=0, size=4) bruto ===');
      Ok := Conn.DBRead(4, 0, 4, Data);
      WriteLn('  Resultado: ', Ok, ' len=', Length(Data));
      for k:=0 to High(Data) do Write(Format('%.2x ',[Data[k]]));
      WriteLn;
    finally
      Conn.Free;
    end;

  finally
    Port.Free;
    DebugSink.Free;
  end;
end.
