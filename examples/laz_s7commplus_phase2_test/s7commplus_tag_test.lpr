program s7commplus_tag_test;

{: Fase 2 (conclusao): TPLCTagNumber generico com LongAddress simbolico ("DB4.Var1",
   "MArea.Clock_10Hz", etc.) atualizando sozinho via AutoRead/RefreshTime, atraves do
   TS7CommPlusDriver.DoScanRead/DoGetValue.

   Uso: s7commplus_tag_test <ip-do-clp> <path> [refresh-ms]
   Exemplo: s7commplus_tag_test 192.168.137.210 DB4.Var1 500
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, PLCTagNumber, tcp_udpport, S7PlusFamily, S7PlusConnection;

type
  TTestHost = class
    UpdateCount:Integer;
    LastValue:Double;
    Tag_:TPLCTagNumber;
    procedure OnDebug(Sender:TObject; const Msg:String);
    procedure TagUpdated(Sender:TObject);
  end;

procedure TTestHost.OnDebug(Sender:TObject; const Msg:String);
begin
  WriteLn('  [debug] ', Msg);
end;

procedure TTestHost.TagUpdated(Sender:TObject);
begin
  Inc(UpdateCount);
  WriteLn(Format('  >> update #%d: %s = %g (t=%dms)', [UpdateCount, Tag_.Name, Tag_.Value, GetTickCount64]));
  LastValue := Tag_.Value;
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  Host_:TTestHost;
  MyTag:TPLCTagNumber;
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
  MyTag := TPLCTagNumber.Create(nil);
  Host_.UpdateCount := 0;
  Host_.Tag_ := MyTag;
  try
    Port.Host := Host;
    Port.Port := 102;
    Port.Timeout := 8000;
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

    MyTag.Name := 'Tag_'+StringReplace(TagPath,'.','_',[rfReplaceAll]);
    MyTag.TagType := pttLongInt;
    MyTag.SwapBytes := true;
    MyTag.SwapWords := true;
    MyTag.OnUpdate := @Host_.TagUpdated;
    MyTag.RefreshTime := RefreshMs;
    MyTag.LongAddress := TagPath;
    MyTag.ProtocolDriver := Driver;

    WriteLn(Format('=== Tag "%s" criado, Path=%s, RefreshTime=%dms, aguardando updates automaticos ===',[MyTag.Name, TagPath, RefreshMs]));

    WriteLn('=== Escrevendo um novo valor (4242), pra provar que o scan automatico pega a mudanca ===');
    MyTag.SyncWrites := true; //Value:= ja escreve sozinho (SetValueRaw); so precisamos de UMA escrita.
    MyTag.Value := 4242;
    WriteLn(Format('  Write concluido. Valor logo apos escrever (deve ja ser 4242): %g',[MyTag.Value]));

    //The framework's scan-update thread delivers tag updates via TThread.Synchronize,
    //which a GUI app pumps automatically (Application.ProcessMessages). This is a plain
    //console app, so it must call CheckSynchronize itself - otherwise updates queue up
    //and never get delivered.
    for i:=1 to 40 do begin
      Sleep(100);
      CheckSynchronize;
      Sleep(400);
      CheckSynchronize;
      if Host_.UpdateCount>0 then begin
        WriteLn(Format('  (tick %d) ultimo valor: %g, leituras OK=%d, falhas=%d',[i, Host_.LastValue, MyTag.CommReadsOK, MyTag.CommReadErrors]));
        if Host_.UpdateCount>=3 then break;
      end else
        WriteLn(Format('  (tick %d) ainda sem update. leituras OK=%d, falhas=%d',[i, MyTag.CommReadsOK, MyTag.CommReadErrors]));
    end;

    if Host_.UpdateCount>0 then
      WriteLn(Format('=== OK: tag atualizou automaticamente %d vez(es) via Path simbolico ===',[Host_.UpdateCount]))
    else
      WriteLn('=== FALHA: tag nunca atualizou ===');

    WriteLn('=== Teste sincrono: MyTag.Read (Tag.Read direto, fora do scan) ===');
    MyTag.Read;
    WriteLn(Format('Valor apos Read sincrono: %g',[MyTag.Value]));
  finally
    MyTag.Free;
    Driver.Free;
    Port.Free;
    Host_.Free;
  end;
end.
