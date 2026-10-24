program s7commplus_struct_test;

{: Valida a leitura de uma struct inteira via TPLCBlock + LongAddress simbolico,
   passando pelo caminho normal do framework (DoScanRead/DoGetValue/SizeOfTag),
   nao pela chamada direta ReadSymbolic.

   Uso: s7commplus_struct_test <ip-do-clp> <path> [refresh-ms]
   Exemplo: s7commplus_struct_test 192.168.137.210 DB4.Var26 500
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, PLCBlock, tcp_udpport, S7PlusFamily, S7PlusConnection;

type
  TTestHost = class
    UpdateCount:Integer;
    Blk:TPLCBlock;
    procedure OnDebug(Sender:TObject; const Msg:String);
    procedure BlockUpdated(Sender:TObject);
  end;

procedure TTestHost.OnDebug(Sender:TObject; const Msg:String);
begin
  WriteLn('  [debug] ', Msg);
end;

procedure TTestHost.BlockUpdated(Sender:TObject);
var
  i:Integer;
  s:String;
begin
  Inc(UpdateCount);
  s := '';
  for i:=0 to Blk.Size-1 do
    s := s + Format('%.2x ', [Trunc(Blk.ValueRaw[i])]);
  WriteLn(Format('  >> update #%d: %s (size=%d) = %s', [UpdateCount, Blk.Name, Blk.Size, s]));
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  Host_:TTestHost;
  Host, TagPath:String;
  RefreshMs, WaitMs, i:Integer;

begin
  if ParamCount<2 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> <path> [refresh-ms]');
    Halt(1);
  end;

  Host := ParamStr(1);
  TagPath := ParamStr(2);
  RefreshMs := 500;
  if ParamCount>=3 then RefreshMs := StrToIntDef(ParamStr(3), RefreshMs);

  Port := TTCP_UDPPort.Create(nil);
  Driver := TS7CommPlusDriver.Create(nil);
  Host_ := TTestHost.Create;
  Host_.UpdateCount := 0;
  try
    Port.Host := Host;
    Port.Port := 102;
    Port.Timeout := 8000;
    Port.EnableAutoReconnect := false;
    Driver.CommunicationPort := Port;
    Driver.OnDebug := @Host_.OnDebug;

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

    Host_.Blk := TPLCBlock.Create(nil);
    Host_.Blk.Name := 'Blk_'+StringReplace(TagPath,'.','_',[rfReplaceAll]);
    Host_.Blk.Size := 16; //upper-bound guess; actual read length may differ (driver returns whatever the PLC sends)
    Host_.Blk.OnUpdate := @Host_.BlockUpdated;
    Host_.Blk.RefreshTime := RefreshMs;
    Host_.Blk.LongAddress := TagPath;
    Host_.Blk.ProtocolDriver := Driver;

    WriteLn(Format('=== Block "%s" criado, Path=%s, RefreshTime=%dms ===',[Host_.Blk.Name, TagPath, RefreshMs]));

    for i:=1 to 30 do begin
      Sleep(100);
      CheckSynchronize;
      Sleep(400);
      CheckSynchronize;
      if Host_.UpdateCount>=2 then break;
    end;

    if Host_.UpdateCount>0 then
      WriteLn(Format('=== OK: block atualizou automaticamente %d vez(es) via Path simbolico ===',[Host_.UpdateCount]))
    else
      WriteLn('=== FALHA: block nunca atualizou ===');
  finally
    Host_.Blk.Free;
    Driver.Free;
    Port.Free;
    Host_.Free;
  end;
end.
