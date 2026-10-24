program s7commplus_sub211_direct_test;

{: Teste direcionado: pula BrowseDB/Explore inteiramente (que falha no .211 com
   SystemEvent fatal) e chama SubscriptionCreate direto, com os LIDs de DB4.Var1/2/3
   ja conhecidos (obtidos via BrowseDB(4) contra o .210, que roda o MESMO projeto TIA
   "teste" - LIDs sao atribuidos deterministicamente na compilacao do bloco, entao devem
   ser identicos em ambos os CLPs). Testa a hipotese de que CreateObject (que ja funciona
   no .211, confirmado com ChangeCounter/Subscription vazia) tambem funciona com uma
   Subscription REAL referenciando DB4, mesmo sem BrowseDB/Explore terem funcionado antes.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, tcp_udpport, S7PlusFamily, S7PlusConnection, S7PlusTypeInfo, S7PlusTypes;

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
  WaitMs, i, k:Integer;
  Res:TProtocolIOResult;
  Items:TS7PlusMultiReadItemArray;
  CreditTick:Byte;
  Values:TS7PlusNotificationValueArray;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> [notls]');
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
    if (ParamCount>=2) and (ParamStr(2)='notls') then
      Driver.UseTLS := false;

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

    //LIDs de DB4.Var1/2/3 obtidos via BrowseDB(4) contra o .210 (mesmo projeto TIA "teste").
    SetLength(Items, 3);
    Items[0].AccessArea := S7PlusIds_DBAccessAreaBase+4; SetLength(Items[0].Lids,1); Items[0].Lids[0] := 12; Items[0].SymbolCrc := 0;
    Items[1].AccessArea := S7PlusIds_DBAccessAreaBase+4; SetLength(Items[1].Lids,1); Items[1].Lids[0] := 13; Items[1].SymbolCrc := 0;
    Items[2].AccessArea := S7PlusIds_DBAccessAreaBase+4; SetLength(Items[2].Lids,1); Items[2].Lids[0] := 14; Items[2].SymbolCrc := 0;

    WriteLn('=== SubscriptionCreate direto (DB4.Var1/2/3 via LIDs fixos [12,13,14], cycle=200ms) ===');
    Res := Driver.SubscriptionCreate(Items, 200);
    WriteLn('  Resultado: ', Ord(Res));
    if Res<>ioOk then Halt(4);

    WriteLn('=== Esperando ate 5 Notifications ===');
    for i:=1 to 5 do begin
      WriteLn(Format('--- Notification #%d ---',[i]));
      Res := Driver.SubscriptionWaitNotification(CreditTick, Values);
      WriteLn(Format('  Resultado=%d CreditTick=%d Values=%d',[Ord(Res), CreditTick, Length(Values)]));
      if Res=ioOk then
        for k:=0 to High(Values) do begin
          Write(Format('    RefId=%d Data=',[Values[k].RefId]));
          for WaitMs:=0 to High(Values[k].Data) do
            Write(Format('%.2x ',[Values[k].Data[WaitMs]]));
          WriteLn;
        end
      else
        break;
    end;

    WriteLn('=== SubscriptionDelete ===');
    Res := Driver.SubscriptionDelete;
    WriteLn('  Resultado: ', Ord(Res));

  finally
    Driver.Free;
    Port.Free;
    DebugSink.Free;
  end;
end.
