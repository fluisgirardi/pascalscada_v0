program s7commplus_subscription_test;

{: Testa TS7CommPlusDriver.SubscriptionCreate/SubscriptionWaitNotification/SubscriptionDelete
   diretamente (baixo nivel, sem integracao com o framework de tags ainda) - cria uma
   subscription para DB4.Var1/Var2/Var3 e espera algumas Notifications chegarem.

   Uso: s7commplus_subscription_test <ip-do-clp>
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
  DBVars:TS7PlusVarInfoArray;
  Res:TProtocolIOResult;
  Items:TS7PlusMultiReadItemArray;
  Var1Idx, Var2Idx, Var3Idx:Integer;
  CreditTick:Byte;
  Values:TS7PlusNotificationValueArray;

  function FindVar(const Name:String):Integer;
  var k2:Integer;
  begin
    Result := -1;
    for k2:=0 to High(DBVars) do
      if DBVars[k2].Name=Name then exit(k2);
  end;

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

    WriteLn('=== BrowseDB(4) ===');
    Res := Driver.BrowseDB(4, DBVars);
    if Res<>ioOk then begin
      WriteLn('Falha ao explorar DB4: ', Ord(Res));
      Halt(3);
    end;
    for i:=0 to High(DBVars) do begin
      Write(Format('  DBVar[%d] Name=%s SymbolCrc=%.8x Lids=[',[i,DBVars[i].Name,DBVars[i].SymbolCrc]));
      for k:=0 to High(DBVars[i].Lids) do Write(DBVars[i].Lids[k],' ');
      WriteLn(']');
    end;
    Var1Idx := FindVar('DB4.Var1');
    Var2Idx := FindVar('DB4.Var2');
    Var3Idx := FindVar('DB4.Var3');
    if (Var1Idx<0) or (Var2Idx<0) or (Var3Idx<0) then begin
      WriteLn('DB4.Var1/2/3 nao encontrados.');
      Halt(3);
    end;

    SetLength(Items, 3);
    Items[0].AccessArea := S7PlusIds_DBAccessAreaBase+4; Items[0].Lids := DBVars[Var1Idx].Lids; Items[0].SymbolCrc := DBVars[Var1Idx].SymbolCrc;
    Items[1].AccessArea := S7PlusIds_DBAccessAreaBase+4; Items[1].Lids := DBVars[Var2Idx].Lids; Items[1].SymbolCrc := DBVars[Var2Idx].SymbolCrc;
    Items[2].AccessArea := S7PlusIds_DBAccessAreaBase+4; Items[2].Lids := DBVars[Var3Idx].Lids; Items[2].SymbolCrc := DBVars[Var3Idx].SymbolCrc;

    WriteLn('=== SubscriptionCreate (DB4.Var1/2/3, cycle=200ms) ===');
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
