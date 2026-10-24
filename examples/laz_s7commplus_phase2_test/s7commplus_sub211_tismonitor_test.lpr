program s7commplus_sub211_tismonitor_test;

{: Testa o mecanismo real que o TIA Portal usa pra monitorar DB4 nesse firmware
   V1-initial/SecurityKey (.211): CreateTisMonitorJob, em vez de SubscriptionCreate
   (classe 1001, que e rejeitada aqui com um SystemEvent fatal constante independente
   de como o LID/RID e endereçado - confirmado ao vivo). Usa os LIDs de DB4.Var1/2/3
   ja conhecidos (12,13,14 - obtidos via BrowseDB(4) contra o .210, mesmo projeto TIA).
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
  Port, Port2:TTCP_UDPPort;
  Conn, Conn2:TS7PlusConnection;
  DebugSink:TDebugSink;
  Host:String;
  WaitMs:Integer;
  Lids:array[0..2] of Cardinal;
  JobRID:Cardinal;
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

      WriteLn('=== Abrindo SEGUNDA conexao/sessao (hipotese: DB4 so e acessivel numa 2a sessao) ===');
      Port2 := TTCP_UDPPort.Create(nil);
      try
        Port2.Host := Host;
        Port2.Port := 102;
        Port2.Timeout := 8000;
        Port2.EnableAutoReconnect := false;
        Port2.Active := true;
        WaitMs := 0;
        while (not Port2.ReallyActive) and (WaitMs<5000) do begin
          Sleep(100);
          Inc(WaitMs, 100);
        end;
        if not Port2.ReallyActive then begin
          WriteLn('Falha ao abrir a 2a porta TCP (timeout).');
          Halt(4);
        end;

        Conn2 := TS7PlusConnection.Create(Port2, 2);
        try
          Conn2.OnDebug := @DebugSink.OnDebug;
          if (ParamCount>=2) and (ParamStr(2)='notls') then
            Conn2.UseTLS := false;

          if not Conn2.Connect('') then begin
            WriteLn('Falha ao conectar/montar a 2a sessao.');
            Halt(5);
          end;

          WriteLn('=== CreateTisMonitorJob na 2a sessao (DB4.Var1/2/3 via LIDs [12,13,14]) ===');
          Lids[0] := 12; Lids[1] := 13; Lids[2] := 14;
          Ok := Conn2.CreateTisMonitorJob(Lids, JobRID);
          WriteLn('  Resultado: ', Ok, ' JobRID=0x', IntToHex(JobRID,8));

          if Ok then begin
            WriteLn('Aguardando 3s por notificacoes assincronas (nao tratadas ainda)...');
            Sleep(3000);
          end;
        finally
          Conn2.Free;
        end;
      finally
        Port2.Free;
      end;

    finally
      Conn.Free;
    end;

  finally
    Port.Free;
    DebugSink.Free;
  end;
end.
