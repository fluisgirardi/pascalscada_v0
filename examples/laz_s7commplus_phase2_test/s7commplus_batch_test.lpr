program s7commplus_batch_test;

{: Valida ReadMultipleSymbolic: le varias variaveis (DB4.Var1..Var5, DB4.Var26.v1,
   MArea.System_Byte) num unico GetMultiVariables, misturando DBs/areas diferentes no
   mesmo request.

   Uso: s7commplus_batch_test <ip-do-clp>
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

procedure DumpBytes(const Caption:String; const B:TBytes);
var
  j:Integer;
begin
  Write('  ', Caption, ' = ');
  for j:=0 to High(B) do
    Write(Format('%.2x ',[B[j]]));
  WriteLn;
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  DebugSink:TDebugSink;
  Host:String;
  WaitMs, i:Integer;
  DBVars, MVars:TS7PlusVarInfoArray;
  Res:TProtocolIOResult;
  Items:TS7PlusMultiReadItemArray;
  Results:TS7PlusMultiReadResultArray;
  DBAccessArea:Cardinal;

  function FindVar(const Vars:TS7PlusVarInfoArray; const Name:String):Integer;
  var
    k:Integer;
  begin
    Result := -1;
    for k:=0 to High(Vars) do
      if Vars[k].Name=Name then exit(k);
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

    WriteLn('=== Explorando DB4 e MArea (para pegar os LIDs) ===');
    Res := Driver.BrowseDB(4, DBVars);
    if Res<>ioOk then begin
      WriteLn('Falha ao explorar DB4: ', Ord(Res));
      Halt(3);
    end;
    Res := Driver.BrowseNativeArea(S7PlusIds_NativeMAreaRID, S7PlusTiRid_MArea, 'MArea', MVars);
    if Res<>ioOk then begin
      WriteLn('Falha ao explorar MArea: ', Ord(Res));
      Halt(3);
    end;

    DBAccessArea := S7PlusIds_DBAccessAreaBase + 4;
    SetLength(Items, 7);
    Items[0].AccessArea := DBAccessArea; Items[0].Lids := DBVars[FindVar(DBVars,'DB4.Var1')].Lids;
    Items[1].AccessArea := DBAccessArea; Items[1].Lids := DBVars[FindVar(DBVars,'DB4.Var2')].Lids;
    Items[2].AccessArea := DBAccessArea; Items[2].Lids := DBVars[FindVar(DBVars,'DB4.Var3')].Lids;
    Items[3].AccessArea := DBAccessArea; Items[3].Lids := DBVars[FindVar(DBVars,'DB4.Var4')].Lids;
    Items[4].AccessArea := DBAccessArea; Items[4].Lids := DBVars[FindVar(DBVars,'DB4.Var5')].Lids;
    Items[5].AccessArea := DBAccessArea; Items[5].Lids := DBVars[FindVar(DBVars,'DB4.Var26.v1')].Lids;
    Items[6].AccessArea := S7PlusIds_NativeMAreaRID; Items[6].Lids := MVars[FindVar(MVars,'MArea.System_Byte')].Lids;

    WriteLn('=== ReadMultipleSymbolic: 7 itens (DB4.Var1..5, DB4.Var26.v1, MArea.System_Byte) em UM request ===');
    Res := Driver.ReadMultipleSymbolic(Items, Results);
    if Res<>ioOk then begin
      WriteLn('Falha na leitura em lote: ', Ord(Res));
      Halt(4);
    end;

    WriteLn(Format('Resultado: %d itens',[Length(Results)]));
    DumpBytes('DB4.Var1     ', Results[0].Data);
    DumpBytes('DB4.Var2     ', Results[1].Data);
    DumpBytes('DB4.Var3     ', Results[2].Data);
    DumpBytes('DB4.Var4     ', Results[3].Data);
    DumpBytes('DB4.Var5     ', Results[4].Data);
    DumpBytes('DB4.Var26.v1 ', Results[5].Data);
    DumpBytes('MArea.SysByte', Results[6].Data);
    for i:=0 to High(Results) do
      if not Results[i].Ok then
        WriteLn(Format('  item %d: FALHOU',[i]));

    if Results[0].Ok and Results[6].Ok then
      WriteLn('=== OK: batching funcionando (multiplos itens, DBs/areas diferentes, um request) ===')
    else
      WriteLn('=== FALHA ===');
  finally
    Driver.Free;
    Port.Free;
    DebugSink.Free;
  end;
end.
