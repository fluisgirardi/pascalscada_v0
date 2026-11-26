program s7commplus_dumpobject_test;
{ Read-only: dumps ALL attributes of one or more OMS objects addressed by RID (hex),
  e.g. a UDT by its BlockRID (0x9100xxxx), to read its 7733 StructureModified and the
  other attributes. Companion to s7commplus_dumpblocks_test (which only lists DB objects).

  Credentials come from the environment (S7PLUS_USER / S7PLUS_PASSWORD) so they are never
  written to a file nor shown in the process arguments.

  Uso: S7PLUS_USER=.. S7PLUS_PASSWORD=.. s7commplus_dumpobject_test <ip> <pasta-saida> <ridhex> [<ridhex> ...]
  Ex.: ... 192.168.56.3 /tmp/udts 9100005E 91000020 9100003F }
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Types, Tag, tcp_udpport, ProtocolTypes, S7PlusFamily, S7PlusTypes, S7PlusAlarms, S7PlusBlockHash;

function Hex(const B:TBytes):String;
var i:Integer;
begin Result:=''; for i:=0 to High(B) do Result:=Result+IntToHex(B[i],2); end;

const
  KindNames:array[TS7PlusAttrKind] of String = ('none','number','text','blob','struct','list');

procedure DumpValue(L:TStringList; AId:Cardinal; const V:TS7PlusAttrValue);
var i:Integer;
begin
  L.Add(Format('attr %d: kind=%s root=%d num=%d (0x%x) bytes=%d items=%d',
    [AId, KindNames[V.Kind], V.Root, V.Num, V.Num, Length(V.Bytes), Length(V.Items)]));
  if Length(V.Bytes)>0 then L.Add('  bytes: '+Hex(V.Bytes));
  for i:=0 to High(V.Items) do begin
    L.Add(Format('  item key=%d kind=%s num=%d (0x%x) bytes=%d',
      [V.Items[i].Key, KindNames[V.Items[i].Kind], V.Items[i].Num, V.Items[i].Num, Length(V.Items[i].Bytes)]));
    if Length(V.Items[i].Bytes)>0 then L.Add('    bytes: '+Hex(V.Items[i].Bytes));
  end;
end;

var
  Port:TTCP_UDPPort;
  Drv:TS7CommPlusDriver;
  Ids:TCardinalDynArray;
  Vals:TS7PlusAttrValues;
  L:TStringList;
  Out_, FName, U, P:String;
  i, k, w, a:Integer;
  RID:Cardinal;
begin
  if ParamCount<3 then begin
    WriteLn('Uso: S7PLUS_USER=.. S7PLUS_PASSWORD=.. ', ExtractFileName(ParamStr(0)), ' <ip> <pasta-saida> <ridhex> [<ridhex> ...]');
    Halt(1);
  end;
  U := GetEnvironmentVariable('S7PLUS_USER');
  P := GetEnvironmentVariable('S7PLUS_PASSWORD');
  if (U='') then begin WriteLn('set S7PLUS_USER / S7PLUS_PASSWORD'); Halt(1); end;
  Out_ := IncludeTrailingPathDelimiter(ParamStr(2));
  ForceDirectories(Out_);

  Port := TTCP_UDPPort.Create(nil);
  Drv := TS7CommPlusDriver.Create(nil);
  L := TStringList.Create;
  try
    Port.Host := ParamStr(1);
    Port.Port := 102;
    Port.Timeout := 8000;
    Port.EnableAutoReconnect := true;
    Drv.CommunicationPort := Port;
    Drv.Username := U;
    Drv.Password := P;
    Port.Active := true;
    w := 0;
    while (not Port.ReallyActive) and (w<6000) do begin Sleep(100); Inc(w, 100); end;
    if not Port.ReallyActive then begin WriteLn('no connection'); Halt(2); end;

    for a:=3 to ParamCount do begin
      RID := Cardinal(StrToInt64('$'+ParamStr(a)));
      L.Clear;
      L.Add(Format('object relation $%.8x', [RID]));
      if Drv.ReadObjectAllAttributes(RID, Ids, Vals)<>ioOk then
        L.Add('(attributes not read)')
      else begin
        L.Add(Format('%d attributes returned by default', [Length(Ids)]));
        for k:=0 to High(Ids) do DumpValue(L, Ids[k], Vals[k]);
      end;
      FName := Out_+Format('OBJ_%.8x.txt', [RID]);
      L.SaveToFile(FName);
      WriteLn(FName, ': ', Length(Ids), ' attributes');
    end;
  finally
    L.Free;
    Drv.Free;
    Port.Free;
  end;
end.
