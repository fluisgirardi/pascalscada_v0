program s7commplus_dl_replay_test;

{: EXPERIMENTO DIAGNOSTICO: replay da transacao de download de DB capturada da TIA
   (arquivo replay_bodies.txt, seq 48-62 = BeginSequence..EndSequence + finalizacao).
   Objetivo: descobrir se os checksums do compilador sao a UNICA parede, ou se ha
   outras condicoes que bloqueiam o download. Reenvia os corpos capturados verbatim
   pela nossa propria sessao (header+V3/HMAC re-gerados por SendReplayRaw).

   ATENCAO: escreve na memoria de programa do CLP. Usar SOMENTE contra o .211 (CLP de teste).

   Uso: s7commplus_dl_replay_test <ip> <arquivo-bodies> [notls]
}
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}cthreads,{$ENDIF}{$ENDIF}
  Classes, SysUtils, tcp_udpport, S7PlusConnection, S7PlusTypes;

type
  TDbgSink = class
    procedure OnDebug(Sender:TObject; const Msg:String);
  end;

procedure TDbgSink.OnDebug(Sender:TObject; const Msg:String);
begin
  WriteLn('  [dbg] ', Msg);
end;

function HexToBytes(const S:AnsiString):TBytes;
var i,n:Integer;
begin
  n := Length(S) div 2; SetLength(Result, n);
  for i:=0 to n-1 do Result[i] := StrToInt('$'+Copy(S,i*2+1,2));
end;

var
  Port:TTCP_UDPPort;
  Conn:TS7PlusConnection;
  Dbg:TDbgSink;
  Host, Fn, Line, HexStr:String;
  Lst:TStringList;
  i, sp1, sp2, WaitMs:Integer;
  Seq, Fc:Integer;
  Body, Resp:TBytes;
  Reass:Boolean;
  ok:Boolean;
begin
  if ParamCount<2 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip> <arquivo-bodies> [notls]'); Halt(1);
  end;
  Host := ParamStr(1); Fn := ParamStr(2);

  Port := TTCP_UDPPort.Create(nil);
  Dbg := TDbgSink.Create;
  try
    Port.Host := Host; Port.Port := 102; Port.Timeout := 8000; Port.EnableAutoReconnect := false;
    Port.Active := true;
    WaitMs := 0;
    while (not Port.ReallyActive) and (WaitMs<5000) do begin Sleep(100); Inc(WaitMs,100); end;
    if not Port.ReallyActive then begin WriteLn('Falha TCP'); Halt(2); end;

    Conn := TS7PlusConnection.Create(Port, 1);
    try
      Conn.OnDebug := @Dbg.OnDebug;
      if (ParamCount>=3) and (ParamStr(3)='notls') then Conn.UseTLS := false;
      if not Conn.Connect('') then begin WriteLn('Falha ao conectar'); Halt(3); end;
      WriteLn('=== Conectado. Iniciando replay da transacao de download ===');

      Lst := TStringList.Create;
      try
        Lst.LoadFromFile(Fn);
        for i:=0 to Lst.Count-1 do begin
          Line := Trim(Lst[i]);
          if Line='' then continue;
          sp1 := Pos(' ', Line); Seq := StrToInt(Copy(Line,1,sp1-1));
          Line := Copy(Line, sp1+1, MaxInt);
          sp2 := Pos(' ', Line); Fc := StrToInt('$'+Copy(Line,1,sp2-1));
          HexStr := Trim(Copy(Line, sp2+1, MaxInt));
          Body := HexToBytes(HexStr);
          Reass := (Fc=S7PlusFunc_Explore);
          WriteLn(Format('--- replay seq=%d func=$%.4x len=%d ---',[Seq,Fc,Length(Body)]));
          ok := Conn.SendReplayRaw(Fc, Body, -1, Resp, Reass);
          if ok then
            WriteLn(Format('    OK resp[%d]=%s',[Length(Resp), S7PlusHexStr(Copy(Resp,0,24))]))
          else begin
            WriteLn('    *** FALHOU nesta etapa (seq '+IntToStr(Seq)+') - parando replay ***');
            break;
          end;
        end;
      finally
        Lst.Free;
      end;
      WriteLn('=== Replay concluido ===');
    finally
      Conn.Free;
    end;
  finally
    Port.Free; Dbg.Free;
  end;
end.
