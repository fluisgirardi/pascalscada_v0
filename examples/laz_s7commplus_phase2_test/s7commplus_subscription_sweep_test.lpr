program s7commplus_subscription_sweep_test;

{: Varre combinacoes de RouteMode/RelationId/atributo 1055/CreditLimit no
   SubscriptionCreate contra o CLP real, tentando sair do "Download error (IDs & states
   [main, sub, TI, next])!" que toda combinacao testada ate agora retornou.

   Cada combinacao roda numa conexao (sessao) totalmente nova - descobrimos que chamar
   SubscriptionDelete apos uma falha destroi o container ClassSubscriptions criado junto
   com a sessao (FSubscriptionsContainerObjectId), quebrando QUALQUER tentativa seguinte
   na mesma sessao com um erro diferente ("download: no parent object!"). Reconectar do
   zero a cada combinacao evita esse efeito colateral e garante que cada uma seja testada
   de forma limpa/independente.

   Uso: s7commplus_subscription_sweep_test <ip-do-clp>
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
    Enabled:Boolean;
    procedure OnDebug(Sender:TObject; const Msg:String);
  end;

  TCombo = record
    Name:String;
    RouteMode:Byte;
    UseFixedRelationId:Boolean;
    IncludeAttr1055:Boolean;
    CreditLimit:SmallInt;
  end;

procedure TDebugSink.OnDebug(Sender:TObject; const Msg:String);
begin
  if Enabled then WriteLn('  [debug] ', Msg);
end;

var
  Host:String;
  Combos:array of TCombo;
  ci:Integer;

  procedure AddCombo(const Name:String; RouteMode:Byte; UseFixedRelationId, IncludeAttr1055:Boolean; CreditLimit:SmallInt);
  var n:Integer;
  begin
    n := Length(Combos);
    SetLength(Combos, n+1);
    Combos[n].Name := Name;
    Combos[n].RouteMode := RouteMode;
    Combos[n].UseFixedRelationId := UseFixedRelationId;
    Combos[n].IncludeAttr1055 := IncludeAttr1055;
    Combos[n].CreditLimit := CreditLimit;
  end;

  function TryCombo(const Combo:TCombo):TProtocolIOResult;
  var
    Port:TTCP_UDPPort;
    Driver:TS7CommPlusDriver;
    DebugSink:TDebugSink;
    WaitMs:Integer;
    DBVars:TS7PlusVarInfoArray;
    Items:TS7PlusMultiReadItemArray;
    Var1Idx, k2:Integer;
  begin
    Result := ioCommError;
    Port := TTCP_UDPPort.Create(nil);
    Driver := TS7CommPlusDriver.Create(nil);
    DebugSink := TDebugSink.Create;
    DebugSink.Enabled := true;
    try
      Port.Host := Host;
      Port.Port := 102;
      Port.Timeout := 8000;
      Port.EnableAutoReconnect := false;
      Driver.CommunicationPort := Port;
      Driver.OnDebug := @DebugSink.OnDebug;

      Port.Active := true;
      WaitMs := 0;
      while (not Port.ReallyActive) and (WaitMs<5000) do begin
        Sleep(100);
        Inc(WaitMs, 100);
      end;
      if not Port.ReallyActive then begin
        WriteLn('  Falha ao abrir a porta TCP (timeout).');
        exit;
      end;

      DebugSink.Enabled := false;
      if Driver.BrowseDB(4, DBVars)<>ioOk then begin
        WriteLn('  Falha ao explorar DB4.');
        exit;
      end;
      Var1Idx := -1;
      for k2:=0 to High(DBVars) do
        if DBVars[k2].Name='DB4.Var1' then begin Var1Idx := k2; break; end;
      if Var1Idx<0 then begin
        WriteLn('  DB4.Var1 nao encontrado.');
        exit;
      end;

      SetLength(Items, 1);
      Items[0].AccessArea := S7PlusIds_DBAccessAreaBase+4;
      Items[0].Lids := DBVars[Var1Idx].Lids;
      Items[0].SymbolCrc := DBVars[Var1Idx].SymbolCrc;

      DebugSink.Enabled := true;
      Result := Driver.SubscriptionCreate(Items, 200, Combo.RouteMode, Combo.UseFixedRelationId,
                                           Combo.IncludeAttr1055, Combo.CreditLimit);
      if Result=ioOk then
        Driver.SubscriptionDelete; //so' limpa em caso de sucesso - a sessao inteira cai fora de qualquer jeito.

    finally
      Driver.Free;
      Port.Free;
      DebugSink.Free;
    end;
  end;

var
  Res:TProtocolIOResult;
begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp>');
    Halt(1);
  end;

  Host := ParamStr(1);

  AddCombo('baseline ($14, GetNewRID, attr1055=on, credit=10)', $14, false, true, 10);
  AddCombo('RouteMode=$00', $00, false, true, 10);
  AddCombo('RouteMode=$04', $04, false, true, 10);
  AddCombo('RouteMode=$20', $20, false, true, 10);
  AddCombo('RelationId fixo 0x7fffc001 (igual ao C#)', $14, true, true, 10);
  AddCombo('sem atributo 1055', $14, false, false, 10);
  AddCombo('CreditLimit=-1 (ilimitado)', $14, false, true, -1);
  AddCombo('tudo junto: $00, RID fixo, sem 1055, credit=-1', $00, true, false, -1);

  for ci:=0 to High(Combos) do begin
    WriteLn;
    WriteLn(Format('=== [%d/%d] %s (sessao nova) ===',[ci+1, Length(Combos), Combos[ci].Name]));
    Res := TryCombo(Combos[ci]);
    WriteLn(Format('  Resultado: %d',[Ord(Res)]));
    if Res=ioOk then begin
      WriteLn('  *** SUCESSO - esta combinacao funcionou! ***');
      break;
    end;
  end;
end.
