program s7commplus_multiwrite_test;

{: Escrita em lote (TS7CommPlusDriver.WriteMultipleSymbolic): varios itens numa so requisicao
   SetMultiVariables, conferidos depois lendo de volta.

   Escreve (de proposito em variaveis que o programa de simulacao do CLP nao toca):
     DB4.Var10..Var13 com valores novos (tipo conforme o browse da DB), DB4.Var26.v1, e
     mais um item INVALIDO (LID inexistente) para ver como o CLP reporta erro por item.

   Uso: s7commplus_multiwrite_test <ip-do-clp> [valido]
   "valido": nao inclui o item invalido (isola o lote bom do erro por item).
   Sem "valido": o PLC responde com erro mas lista so o item invalido, e os itens bons SAO
   escritos (sucesso parcial); o teste confere isso lendo de volta.
   Os valores mudam a cada execucao, para a conferencia nao ser enganada por valores antigos.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, StrUtils,
  Tag, tcp_udpport, S7PlusFamily, S7PlusConnection, S7PlusTypeInfo, S7PlusTypes;

type
  TDebugSink = class
    procedure OnDebug(Sender:TObject; const Msg:String);
  end;

procedure TDebugSink.OnDebug(Sender:TObject; const Msg:String);
begin
  if (Pos('ParseMultiWriteResponse', Msg)>0) or (Pos('respPayload', Msg)>0) or (Pos('functionCode=$0542', Msg)>0) then
    WriteLn('  [debug] ', Copy(Msg, 1, 220));
end;

function Hex(const B:TBytes):String;
var
  j:Integer;
begin
  Result := '';
  for j:=0 to High(B) do
    Result := Result + Format('%.2x ',[B[j]]);
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  DebugSink:TDebugSink;
  Host:String;
  OnlyValid:Boolean;
  Token:Integer;
  WaitMs, i, Bad, Failed:Integer;
  DBVars:TS7PlusVarInfoArray;
  Res:TProtocolIOResult;
  WItems:TS7PlusMultiWriteItemArray;
  WResults:TS7PlusMultiWriteResultArray;
  RItems:TS7PlusMultiReadItemArray;
  RResults:TS7PlusMultiReadResultArray;
  Names:array[0..4] of String = ('DB4.Var10','DB4.Var11','DB4.Var12','DB4.Var13','DB4.Var26.v1');
  Wanted:array[0..4] of TBytes;
  DBAccessArea:Cardinal;

  function FindVar(const Name:String):Integer;
  var
    k:Integer;
  begin
    Result := -1;
    for k:=0 to High(DBVars) do
      if DBVars[k].Name=Name then exit(k);
  end;

  //Raw big-endian bytes for a distinctive value of the variable's own type.
  function ValueFor(SoftDataType:Byte; Seed:Integer):TBytes;
  begin
    case SoftDataType of
      1, 2, 3, 52, 55: Result := TBytes.Create(Byte(Seed and $7F));           //BOOL BYTE CHAR USINT SINT
      4, 53, 5:        Result := TBytes.Create(Byte(Seed shr 8), Byte(Seed)); //WORD UINT INT
      8:               Result := TBytes.Create($3F, $80, 0, Byte(Seed));       //REAL (a bit pattern close to 1.0)
    else
      Result := TBytes.Create(0, 0, Byte(Seed shr 8), Byte(Seed));            //DINT / UDINT / DWORD
    end;
  end;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp>');
    Halt(1);
  end;
  Host := ParamStr(1);
  OnlyValid := (ParamCount>=2) and (ParamStr(2)='valido');
  Token := Integer(GetTickCount64 mod 5000);

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

    if Driver.BrowseDB(4, DBVars)<>ioOk then begin
      WriteLn('Falha ao explorar DB4.');
      Halt(3);
    end;

    DBAccessArea := S7PlusIds_DBAccessAreaBase + 4;
    SetLength(WItems, Length(Names)+Ord(not OnlyValid));
    SetLength(RItems, Length(Names));
    for i:=0 to High(Names) do begin
      Bad := FindVar(Names[i]);
      if Bad<0 then begin
        WriteLn(Names[i], ' nao encontrada no browse.');
        Halt(3);
      end;
      Wanted[i] := ValueFor(DBVars[Bad].SoftDataType, 100+Token+i*37);
      WItems[i].AccessArea := DBAccessArea;
      WItems[i].Lids := DBVars[Bad].Lids;
      WItems[i].SoftDataType := DBVars[Bad].SoftDataType;
      WItems[i].Data := Wanted[i];
      RItems[i].AccessArea := DBAccessArea;
      RItems[i].Lids := DBVars[Bad].Lids;
      WriteLn(Format('  %-14s softtype=%-3d  vai escrever %s', [Names[i], DBVars[Bad].SoftDataType, Hex(Wanted[i])]));
    end;
    if not OnlyValid then begin
      //item invalido: um LID que nao existe na DB4
      i := Length(Names);
      SetLength(WItems[i].Lids, 1);
      WItems[i].Lids[0] := $7FFF0;
      WItems[i].AccessArea := DBAccessArea;
      WItems[i].SoftDataType := 7;
      WItems[i].Data := TBytes.Create(0, 0, 0, 1);
      WriteLn('  (item invalido)  LID $7FFF0 inexistente');
    end;

    WriteLn('=== WriteMultipleSymbolic: ', Length(WItems), ' itens em UMA requisicao ===');
    Res := Driver.WriteMultipleSymbolic(WItems, WResults);
    WriteLn('  resultado do driver: ', Ord(Res), IfThen(Res=ioOk, ' (ioOk)', ' (FALHA)'));
    if Res<>ioOk then Halt(4);
    for i:=0 to High(WResults) do
      if WResults[i].Ok then
        WriteLn(Format('  item %d: aceito', [i+1]))
      else
        WriteLn(Format('  item %d: RECUSADO pelo CLP (erro %d)', [i+1, WResults[i].ErrorValue]));

    WriteLn('=== Lendo de volta (ReadMultipleSymbolic) ===');
    if Driver.ReadMultipleSymbolic(RItems, RResults)<>ioOk then begin
      WriteLn('Falha na leitura de conferencia.');
      Halt(5);
    end;
    Failed := 0;
    for i:=0 to High(Names) do begin
      Bad := 0;
      if (not RResults[i].Ok) or (Length(RResults[i].Data)<>Length(Wanted[i])) then
        Bad := 1
      else
        for WaitMs:=0 to High(Wanted[i]) do
          if RResults[i].Data[WaitMs]<>Wanted[i][WaitMs] then
            Bad := 1;
      if Bad=0 then
        WriteLn(Format('  %-14s OK   %s', [Names[i], Hex(RResults[i].Data)]))
      else begin
        WriteLn(Format('  %-14s DIFERENTE: li %s, esperava %s', [Names[i], Hex(RResults[i].Data), Hex(Wanted[i])]));
        Inc(Failed);
      end;
    end;
    if (not OnlyValid) and (Res=ioOk) and WResults[High(WResults)].Ok then begin
      WriteLn('  (!) o item invalido foi ACEITO - inesperado');
      Inc(Failed);
    end;
    if OnlyValid then begin
      if Failed=0 then
        WriteLn('=== OK: escrita em lote confere (todos os itens aceitos e lidos de volta iguais) ===')
      else
        WriteLn('=== FALHA: ', Failed, ' problema(s) ===');
    end else begin
      if Res<>ioOk then begin
        WriteLn('=== FALHA: o lote falhou como um todo ===');
      end else if (Failed=0) and (not WResults[High(WResults)].Ok) then
        WriteLn('=== OK: so o item invalido foi recusado; os itens bons foram escritos e conferem ===')
      else
        WriteLn('=== FALHA: ', Failed, ' problema(s) ===');
    end;
  finally
    Driver.Free;
    Port.Free;
    DebugSink.Free;
  end;
end.
