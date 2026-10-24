program s7commplus_phase2_test;

{: Fase 2 do driver S7CommPlus: resolucao de Path simbolico via EXPLORE + arvore de
   tipos (TypeInfo), e leitura/escrita real de variaveis de DB e da area M por LID
   (nao mais por offset bruto - S7CommPlus nao suporta isso, confirmado contra CLP real).

   Uso: s7commplus_phase2_test <ip-do-clp> <db> [timeout-ms] [explore-delay-ms]
   Exemplo: s7commplus_phase2_test 192.168.137.210 4 15000
   O 4o parametro (opcional) insere uma pausa, em ms, imediatamente antes do Explore do
   container de tipos OMS - diagnostico para testar se o CLP precisa de um intervalo maior
   entre a leitura do LID=1 da DB e o Explore.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, tcp_udpport, S7PlusFamily, S7PlusConnection, S7PlusTypes, S7PlusTypeInfo;

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
  DBNumber, TimeoutMs, WaitMs, i, ExploreDelayMs:Integer;
  Host:String;
  DBVars, MVars:TS7PlusVarInfoArray;
  Res:TProtocolIOResult;
  Data, WriteBuf:TBytes;
  AccessArea:Cardinal;

procedure DumpBytes(const Caption:String; const B:TBytes);
var
  j:Integer;
begin
  Write(Caption, ' = ');
  for j:=0 to High(B) do
    Write(Format('%.2x ',[B[j]]));
  WriteLn;
end;

procedure PrintVars(const Vars:TS7PlusVarInfoArray; Limit:Integer);
var
  i:Integer;
begin
  for i:=0 to High(Vars) do begin
    if i>=Limit then begin
      WriteLn(Format('  ... e mais %d variaveis',[Length(Vars)-Limit]));
      break;
    end;
    WriteLn(Format('  %s  lids=%s  softdatatype=%d  byteoffset=%d',
            [Vars[i].Name, S7PlusFormatLids(Vars[i].Lids), Vars[i].SoftDataType, Vars[i].ByteOffset]));
  end;
end;

begin
  if ParamCount<2 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> <db> [timeout-ms]');
    Halt(1);
  end;

  Host := ParamStr(1);
  DBNumber := StrToIntDef(ParamStr(2), 4);
  TimeoutMs := 8000;
  if ParamCount>=3 then TimeoutMs := StrToIntDef(ParamStr(3), TimeoutMs);
  ExploreDelayMs := 0;
  if ParamCount>=4 then ExploreDelayMs := StrToIntDef(ParamStr(4), ExploreDelayMs);

  Port := TTCP_UDPPort.Create(nil);
  Driver := TS7CommPlusDriver.Create(nil);
  DebugSink := TDebugSink.Create;
  try
    Port.Host := Host;
    Port.Port := 102;
    Port.Timeout := TimeoutMs;
    Port.EnableAutoReconnect := false;
    Driver.CommunicationPort := Port;
    Driver.OnDebug := @DebugSink.OnDebug;
    Driver.ExploreDelayMs := ExploreDelayMs;

    WriteLn('Abrindo porta TCP com ', Host, ':102 ...');
    Port.Active := true;
    WaitMs := 0;
    while (not Port.ReallyActive) and (WaitMs<5000) do begin
      Sleep(100);
      Inc(WaitMs, 100);
    end;
    if not Port.ReallyActive then begin
      WriteLn('Falha ao abrir a porta TCP (timeout).');
      ExitCode := 2;
    end else begin
    WriteLn('Porta TCP aberta.');

    WriteLn(Format('=== Explorando DB%d (dispara a conexao) ===',[DBNumber]));
    Res := Driver.BrowseDB(DBNumber, DBVars);
    if Res<>ioOk then begin
      WriteLn('Falha ao explorar a DB: ', Ord(Res));
      ExitCode := 3;
    end else begin
    WriteLn(Format('DB%d: %d variaveis encontradas:',[DBNumber, Length(DBVars)]));
    PrintVars(DBVars, 30);

    for i:=0 to High(DBVars) do
      if DBVars[i].Name=Format('DB%d',[DBNumber]) then begin
        WriteLn(Format('=== Lendo "%s" (DB inteira, Lids vazio) via LID simbolico ===',[DBVars[i].Name]));
        AccessArea := S7PlusIds_DBAccessAreaBase + Cardinal(DBNumber);
        Res := Driver.ReadSymbolic(AccessArea, DBVars[i].Lids, Data);
        if Res=ioOk then
          DumpBytes(DBVars[i].Name, Data)
        else
          WriteLn('Falha na leitura da DB inteira: ', Ord(Res));
        break;
      end;

    if Length(DBVars)>0 then begin
      AccessArea := S7PlusIds_DBAccessAreaBase + Cardinal(DBNumber);
      WriteLn(Format('=== Lendo "%s" via LID simbolico (%s) ===',[DBVars[0].Name, S7PlusFormatLids(DBVars[0].Lids)]));
      Res := Driver.ReadSymbolic(AccessArea, DBVars[0].Lids, Data);
      if Res=ioOk then begin
        DumpBytes(DBVars[0].Name, Data);
        WriteLn('=== Escrevendo de volta o mesmo valor em DB4.Var1 (round-trip de teste) ===');
        SetLength(WriteBuf, Length(Data));
        for i:=0 to High(Data) do WriteBuf[i] := Data[i];
        Res := Driver.WriteSymbolic(AccessArea, DBVars[0].Lids, WriteBuf);
        if Res=ioOk then begin
          WriteLn('Escrita OK.');
          Res := Driver.ReadSymbolic(AccessArea, DBVars[0].Lids, Data);
          if Res=ioOk then
            DumpBytes('DB4.Var1 (relido apos escrita)', Data)
          else
            WriteLn('Falha ao reler apos escrita: ', Ord(Res));
        end else
          WriteLn('Falha na escrita: ', Ord(Res));
      end else
        WriteLn('Falha na leitura simbolica: ', Ord(Res));
    end;

    for i:=0 to High(DBVars) do
      if DBVars[i].Name='DB4.Var26' then begin
        WriteLn(Format('=== Lendo "%s" (struct inteira) via LID simbolico (%s) ===',[DBVars[i].Name, S7PlusFormatLids(DBVars[i].Lids)]));
        Res := Driver.ReadSymbolic(AccessArea, DBVars[i].Lids, Data);
        if Res=ioOk then
          DumpBytes(DBVars[i].Name, Data)
        else
          WriteLn('Falha na leitura da struct: ', Ord(Res));
        break;
      end;

    WriteLn('=== Explorando area M (Merker) ===');
    Res := Driver.BrowseNativeArea(S7PlusIds_NativeMAreaRID, S7PlusTiRid_MArea, 'MArea', MVars);
    if Res<>ioOk then
      WriteLn('Falha ao explorar a area M: ', Ord(Res))
    else begin
      WriteLn(Format('MArea: %d variaveis encontradas:',[Length(MVars)]));
      PrintVars(MVars, 15);

      if Length(MVars)>0 then begin
        WriteLn(Format('=== Lendo "%s" via LID simbolico (%s) ===',[MVars[0].Name, S7PlusFormatLids(MVars[0].Lids)]));
        Res := Driver.ReadSymbolic(S7PlusIds_NativeMAreaRID, MVars[0].Lids, Data);
        if Res=ioOk then begin
          DumpBytes(MVars[0].Name, Data);
          WriteLn('=== Escrevendo de volta o mesmo valor (round-trip de teste) ===');
          SetLength(WriteBuf, Length(Data));
          for i:=0 to High(Data) do WriteBuf[i] := Data[i];
          Res := Driver.WriteSymbolic(S7PlusIds_NativeMAreaRID, MVars[0].Lids, WriteBuf);
          if Res=ioOk then
            WriteLn('Escrita OK.')
          else
            WriteLn('Falha na escrita: ', Ord(Res));
        end else
          WriteLn('Falha na leitura simbolica de M: ', Ord(Res));
      end;
    end;

    WriteLn('Sessao conectada: ', BoolToStr(Driver.Connected, true));
    end; //Res<>ioOk (BrowseDB)
    end; //Port.ReallyActive
  finally
    Driver.Free;
    Port.Free;
    DebugSink.Free;
  end;
end.
