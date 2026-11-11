program s7commplus_dumpblocks_test;

{: DIAGNOSTICO (somente leitura): despeja em arquivos texto os atributos que o CLP devolve para o objeto de
   cada DB (o que o TIA enviaria num CreateObject de download): InterfaceDescription, TypeInfo, OptimizeInfo,
   IdentES, valores iniciais, hashes... Serve para comparar DBs e aprender o formato dos blobs do compilador.

   Uso: s7commplus_dumpblocks_test <ip> <usuario> <senha> <pasta-de-saida> [nome-do-DB]
   (a senha so na linha de comando: nao grave em arquivo)

   DIAGNOSTIC (read only): dumps to text files the attributes the PLC returns for the object of each DB
   (what TIA would send in a download CreateObject), to compare DBs and learn the compiler blob formats. }

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Types, Tag, tcp_udpport, ProtocolTypes, S7PlusFamily, S7PlusTypeInfo, S7PlusTypes, S7PlusAlarms,
  S7PlusBlockHash;

const
  KindNames:array[TS7PlusAttrKind] of String = ('none', 'number', 'text', 'blob', 'struct', 'list');

function Hex(const B:TBytes):String;
begin
  Result := S7PlusHex(B);
end;

procedure DumpValue(L:TStrings; AId:Cardinal; const V:TS7PlusAttrValue);
var
  i:Integer;
begin
  if not V.Found then begin
    L.Add(Format('attr %d: (no value)', [AId]));
    exit;
  end;
  L.Add(Format('attr %d: kind=%s root=%d num=%d (0x%x) bytes=%d items=%d',
    [AId, KindNames[V.Kind], V.Root, V.Num, V.Num, Length(V.Bytes), Length(V.Items)]));
  if Length(V.Bytes)>0 then L.Add('  bytes: '+Hex(V.Bytes));
  for i:=0 to High(V.Items) do begin
    L.Add(Format('  item key=%d kind=%s num=%d (0x%x) bytes=%d', [V.Items[i].Key, KindNames[V.Items[i].Kind],
      V.Items[i].Num, V.Items[i].Num, Length(V.Items[i].Bytes)]));
    if Length(V.Items[i].Bytes)>0 then L.Add('    bytes: '+Hex(V.Items[i].Bytes));
  end;
end;

var
  Port:TTCP_UDPPort;
  Drv:TS7CommPlusDriver;
  Blocks:TS7PlusDataBlockInfoArray;
  Ids:TCardinalDynArray;
  Vals:TS7PlusAttrValues;
  Vars:TS7PlusVarInfoArray;
  L:TStringList;
  Out_, Only_, FName:String;
  i, k, w:Integer;
begin
  if ParamCount<4 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip> <usuario> <senha> <pasta-de-saida> [nome-do-DB]');
    Halt(1);
  end;
  Out_ := IncludeTrailingPathDelimiter(ParamStr(4));
  ForceDirectories(Out_);
  Only_ := '';
  if ParamCount>=5 then Only_ := ParamStr(5);

  Port := TTCP_UDPPort.Create(nil);
  Drv := TS7CommPlusDriver.Create(nil);
  L := TStringList.Create;
  try
    Port.Host := ParamStr(1);
    Port.Port := 102;
    Port.Timeout := 8000;
    Port.EnableAutoReconnect := true;
    Drv.CommunicationPort := Port;
    Drv.Username := ParamStr(2);
    Drv.Password := ParamStr(3);
    Port.Active := true;
    w := 0;
    while (not Port.ReallyActive) and (w<6000) do begin Sleep(100); Inc(w, 100); end;
    if not Port.ReallyActive then begin WriteLn('no connection'); Halt(2); end;

    if Drv.ListDataBlocks(Blocks)<>ioOk then begin WriteLn('ListDataBlocks failed'); Halt(3); end;
    WriteLn(Length(Blocks), ' DBs');
    for i:=0 to High(Blocks) do begin
      if (Only_<>'') and (not SameText(Only_, Blocks[i].Name)) then continue;
      L.Clear;
      L.Add(Format('DB %d "%s" relation $%.8x', [Blocks[i].Number, Blocks[i].Name,
        S7PlusIds_DBAccessAreaBase+Cardinal(Blocks[i].Number)]));
      if Drv.ReadObjectAllAttributes(S7PlusIds_DBAccessAreaBase+Cardinal(Blocks[i].Number), Ids, Vals)<>ioOk then
        L.Add('(attributes not read)')
      else begin
        L.Add(Format('%d attributes returned by default', [Length(Ids)]));
        for k:=0 to High(Ids) do DumpValue(L, Ids[k], Vals[k]);
      end;
      //the variables of the DB with their offsets (the interface the layout signature is made from)
      if Drv.BrowseDB(Blocks[i].Number, Vars)=ioOk then begin
        L.Add(Format('%d variables', [Length(Vars)]));
        for k:=0 to High(Vars) do
          L.Add(Format('var %s type=%d byteoff=%d imageoff=%d bit=%d size=%d',
            [Vars[k].Name, Vars[k].SoftDataType, Vars[k].ByteOffset, Vars[k].ImageOffset, Vars[k].BitOffset, Vars[k].DataSize]));
      end;
      FName := Out_+Format('DB%d_%s.txt', [Blocks[i].Number, StringReplace(Blocks[i].Name, '/', '_', [rfReplaceAll])]);
      L.SaveToFile(FName);
      WriteLn(FName, ': ', Length(Ids), ' attributes');
    end;
  finally
    L.Free;
    Drv.Free;
    Port.Free;
  end;
end.
