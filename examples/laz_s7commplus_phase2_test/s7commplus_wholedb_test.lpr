program s7commplus_wholedb_test;

{: Valida a leitura de "DB inteiro" via um TPLCStruct com LongAddress = nome da DB (bare
   root, sem ponto) - ex. "DB4" ou "BlocoSimbolico_4". Em vez do blob de metadados vazio
   (LID vazio), o driver agora monta a struct lendo (em lote) cada membro de topo da DB e
   colando os bytes na posicao (ByteOffset) de cada um - ver ResolvePath/DoScanRead em
   s7plusfamily.pas. Compara com uma leitura direta de DB4.Var1 (BrowseDB + ReadSymbolic)
   para confirmar que o byte no offset certo bate.

   Uso: s7commplus_wholedb_test <ip-do-clp>
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, StrUtils,
  Tag, PLCStruct, tcp_udpport, S7PlusFamily, S7PlusConnection, S7PlusTypeInfo, S7PlusTypes;

type
  TTestHost = class
    Updates, Updates2, Updates3:Integer;
    Struct1, Struct2, Struct3:TPLCStruct;
    procedure OnDebug(Sender:TObject; const Msg:String);
    procedure StructUpdated(Sender:TObject);
    procedure Struct2Updated(Sender:TObject);
    procedure Struct3Updated(Sender:TObject);
  end;

procedure TTestHost.OnDebug(Sender:TObject; const Msg:String);
begin
  //silencioso - so queremos o resultado final
end;

procedure TTestHost.StructUpdated(Sender:TObject);
begin
  Inc(Updates);
end;

procedure TTestHost.Struct2Updated(Sender:TObject);
begin
  Inc(Updates2);
end;

procedure TTestHost.Struct3Updated(Sender:TObject);
begin
  Inc(Updates3);
end;

procedure DumpBytes(const Caption:String; const B:TBytes; MaxLen:Integer);
var
  j, n:Integer;
begin
  n := Length(B);
  if n>MaxLen then n := MaxLen;
  Write('  ', Caption, ' (', Length(B), ' bytes) = ');
  for j:=0 to n-1 do
    Write(Format('%.2x ',[B[j]]));
  WriteLn;
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  Host_:TTestHost;
  Host:String;
  WaitMs, i:Integer;
  DBVars:TS7PlusVarInfoArray;
  Res:TProtocolIOResult;
  DirectVar1, DirectMember:TBytes;
  Var1Idx, Var1Offset, Checked:Integer;
  StructBytes, StructBytes2, StructBytes3:TBytes;
  Rest:String;
  AllMatch:Boolean;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp>');
    Halt(1);
  end;

  Host := ParamStr(1);

  Port := TTCP_UDPPort.Create(nil);
  Driver := TS7CommPlusDriver.Create(nil);
  Host_ := TTestHost.Create;
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

    WriteLn('=== Leitura direta de DB4.Var1 (referencia) ===');
    Res := Driver.BrowseDB(4, DBVars);
    if Res<>ioOk then begin
      WriteLn('Falha ao explorar DB4: ', Ord(Res));
      Halt(3);
    end;
    Var1Idx := -1;
    for i:=0 to High(DBVars) do
      if DBVars[i].Name='DB4.Var1' then Var1Idx := i;
    if Var1Idx<0 then begin
      WriteLn('DB4.Var1 nao encontrado no browse.');
      Halt(3);
    end;
    Var1Offset := DBVars[Var1Idx].ByteOffset;
    WriteLn(Format('  DB4.Var1: ByteOffset=%d', [Var1Offset]));

    Res := Driver.ReadSymbolic(S7PlusIds_DBAccessAreaBase+4, DBVars[Var1Idx].Lids, DirectVar1);
    if Res<>ioOk then begin
      WriteLn('Falha ao ler DB4.Var1 diretamente: ', Ord(Res));
      Halt(3);
    end;
    DumpBytes('DB4.Var1 direto', DirectVar1, 16);

    WriteLn('=== Criando TPLCStruct com LongAddress=DB4 (bare root, "DB inteiro") ===');
    Host_.Struct1 := TPLCStruct.Create(nil);
    Host_.Struct1.Name := 'Struct_DB4_Whole';
    Host_.Struct1.Size := 512;
    Host_.Struct1.OnUpdate := @Host_.StructUpdated;
    Host_.Struct1.RefreshTime := 1000;
    Host_.Struct1.LongAddress := 'DB4';
    Host_.Struct1.ProtocolDriver := Driver;

    WaitMs := 0;
    while (Host_.Updates<1) and (WaitMs<15000) do begin
      Sleep(100);
      CheckSynchronize;
      Inc(WaitMs, 100);
    end;

    if Host_.Updates<1 then begin
      WriteLn('=== FALHA: TPLCStruct(DB4) nunca atualizou ===');
      Halt(4);
    end;

    SetLength(StructBytes, Host_.Struct1.Size);
    for i:=0 to Host_.Struct1.Size-1 do
      StructBytes[i] := Host_.Struct1.GetByte(i);
    DumpBytes('Struct(DB4) montada ', StructBytes, 32);

    WriteLn(Format('=== Comparando byte(s) no offset %d (DB4.Var1, %d bytes) ===', [Var1Offset, Length(DirectVar1)]));
    if (Var1Offset+Length(DirectVar1)<=Length(StructBytes)) then begin
      Write('  Struct bytes nesse offset = ');
      for i:=0 to High(DirectVar1) do
        Write(Format('%.2x ',[StructBytes[Var1Offset+i]]));
      WriteLn;

      if CompareMem(@StructBytes[Var1Offset], @DirectVar1[0], Length(DirectVar1)) then
        WriteLn('=== OK: bytes batem - a struct montada tem o conteudo real da DB ===')
      else
        WriteLn('=== FALHA: bytes NAO batem ===');
    end else
      WriteLn('=== FALHA: struct montada eh menor que o esperado ===');

    WriteLn('=== Comparando TODOS os membros de topo de DB4 contra a struct montada ===');
    AllMatch := true; Checked := 0;
    for i:=0 to High(DBVars) do begin
      if DBVars[i].Name='DB4' then continue; //raiz, nao eh membro
      if DBVars[i].ByteOffset=-1 then continue; //offset desconhecido (ex. dentro de array)
      Rest := Copy(DBVars[i].Name, Length('DB4')+2, MaxInt);
      if (Rest='') or (Pos('.', Rest)>0) then continue; //nao eh membro de topo (campo aninhado)

      Res := Driver.ReadSymbolic(S7PlusIds_DBAccessAreaBase+4, DBVars[i].Lids, DirectMember);
      if Res<>ioOk then begin
        WriteLn(Format('  %s: falha ao ler diretamente', [DBVars[i].Name]));
        AllMatch := false;
        continue;
      end;
      Inc(Checked);
      if (DBVars[i].ByteOffset+Length(DirectMember)>Length(StructBytes)) then begin
        WriteLn(Format('  %s: fora do tamanho da struct (offset=%d len=%d)', [DBVars[i].Name, DBVars[i].ByteOffset, Length(DirectMember)]));
        continue;
      end;
      if CompareMem(@StructBytes[DBVars[i].ByteOffset], @DirectMember[0], Length(DirectMember)) then
        WriteLn(Format('  %s: OK (offset=%d len=%d)', [DBVars[i].Name, DBVars[i].ByteOffset, Length(DirectMember)]))
      else begin
        WriteLn(Format('  %s: DIVERGIU (offset=%d len=%d)', [DBVars[i].Name, DBVars[i].ByteOffset, Length(DirectMember)]));
        AllMatch := false;
      end;
    end;
    WriteLn(Format('=== %d membros verificados; %s ===', [Checked, IfThen(AllMatch,'TODOS BATEM','ALGUM DIVERGIU')]));

    WriteLn('=== TPLCStruct com LongAddress=BlocoSimbolico_4 (nome simbolico, bare root) ===');
    Host_.Struct2 := TPLCStruct.Create(nil);
    Host_.Struct2.Name := 'Struct_DB4_BySymbolicName';
    Host_.Struct2.Size := 512;
    Host_.Struct2.OnUpdate := @Host_.Struct2Updated;
    Host_.Struct2.RefreshTime := 1000;
    Host_.Struct2.LongAddress := 'BlocoSimbolico_4';
    Host_.Struct2.ProtocolDriver := Driver;

    WaitMs := 0;
    while (Host_.Updates2<1) and (WaitMs<15000) do begin
      Sleep(100);
      CheckSynchronize;
      Inc(WaitMs, 100);
    end;

    if Host_.Updates2<1 then
      WriteLn('=== FALHA: TPLCStruct(BlocoSimbolico_4) nunca atualizou ===')
    else begin
      SetLength(StructBytes2, Host_.Struct2.Size);
      for i:=0 to Host_.Struct2.Size-1 do
        StructBytes2[i] := Host_.Struct2.GetByte(i);
      if (Var1Offset+Length(DirectVar1)<=Length(StructBytes2)) and
         CompareMem(@StructBytes2[Var1Offset], @DirectVar1[0], Length(DirectVar1)) then
        WriteLn('=== OK: resolucao por nome simbolico tambem monta o DB inteiro corretamente ===')
      else
        WriteLn('=== FALHA: resolucao por nome simbolico nao bateu ===');
    end;

    WriteLn('=== TPLCStruct com Size menor que a DB inteira (trunca para Size) ===');
    Host_.Struct3 := TPLCStruct.Create(nil);
    Host_.Struct3.Name := 'Struct_DB4_Truncated';
    Host_.Struct3.Size := 10; //so cobre ate o meio de DB4.Var2 (offset 8, 4 bytes)
    Host_.Struct3.OnUpdate := @Host_.Struct3Updated;
    Host_.Struct3.RefreshTime := 1000;
    Host_.Struct3.LongAddress := 'DB4';
    Host_.Struct3.ProtocolDriver := Driver;

    WaitMs := 0;
    while (Host_.Updates3<1) and (WaitMs<15000) do begin
      Sleep(100);
      CheckSynchronize;
      Inc(WaitMs, 100);
    end;

    if Host_.Updates3<1 then
      WriteLn('=== FALHA: TPLCStruct(DB4, Size=10) nunca atualizou ===')
    else begin
      SetLength(StructBytes3, Host_.Struct3.Size);
      for i:=0 to Host_.Struct3.Size-1 do
        StructBytes3[i] := Host_.Struct3.GetByte(i);
      DumpBytes('Struct(DB4, Size=10) ', StructBytes3, 10);
      if CompareMem(@StructBytes3[0], @StructBytes[0], 10) then
        WriteLn('=== OK: truncado para Size=10 bate com o inicio da montagem completa ===')
      else
        WriteLn('=== FALHA: truncamento nao bateu com o inicio da montagem completa ===');
    end;

  finally
    if Host_.Struct1<>nil then Host_.Struct1.Free;
    if Host_.Struct2<>nil then Host_.Struct2.Free;
    if Host_.Struct3<>nil then Host_.Struct3.Free;
    Driver.Free;
    Port.Free;
    Host_.Free;
  end;
end.
