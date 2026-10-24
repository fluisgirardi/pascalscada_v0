program s7commplus_listdbs_test;

{: Valida TS7PlusConnection.ListDataBlocks: deve listar as DBs do programa com nome
   simbolico (TIA Portal) + numero, ex. "BlocoSimbolico_4" -> DB4.

   Uso: s7commplus_listdbs_test <ip-do-clp>
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, tcp_udpport, S7PlusFamily, S7PlusConnection, S7PlusTypeInfo;

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
  Driver:TS7CommPlusDriver;
  DebugSink:TDebugSink;
  Host:String;
  WaitMs, i:Integer;
  Blocks:TS7PlusDataBlockInfoArray;
  Res:TProtocolIOResult;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp>');
    Halt(1);
  end;

  Host := ParamStr(1);

  Port := TTCP_UDPPort.Create(nil);
  Driver := TS7CommPlusDriver.Create(nil);
  DebugSink := TDebugSink.Create;
  try
    Port.Host := Host;
    Port.Port := 102;
    Port.Timeout := 8000;
    Port.EnableAutoReconnect := false;
    Driver.CommunicationPort := Port;
    Driver.OnDebug := @DebugSink.OnDebug;

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

    WriteLn('=== ListDataBlocks ===');
    Res := Driver.ListDataBlocks(Blocks);
    if Res=ioOk then begin
      WriteLn(Format('%d DBs encontradas:',[Length(Blocks)]));
      for i:=0 to High(Blocks) do
        WriteLn(Format('  name="%s"  number=%d  rid=$%.8x',[Blocks[i].Name, Blocks[i].Number, Blocks[i].Rid]));
    end else
      WriteLn('Falha ao listar DBs: ', Ord(Res));
  finally
    Driver.Free;
    Port.Free;
    DebugSink.Free;
  end;
end.
