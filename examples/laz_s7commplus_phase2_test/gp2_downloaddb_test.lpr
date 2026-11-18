program gp2_downloaddb_test;
{ TESTE de TGP2ES7CommPlusDriver.DownloadDataBlock contra um PLCSim de teste.
  Monta a imagem do dbSimples (blobs 2449/2544 lidos de um dump, hashes 7949/7951 do dump e
  7733 computado do layout) e chama DownloadDataBlock. EXPERIMENTAL - ver a unit.
  Credenciais via ambiente (S7PLUS_USER / S7PLUS_PASSWORD).
  Uso: S7PLUS_USER=.. S7PLUS_PASSWORD=.. gp2_downloaddb_test <ip> <dumpfile-do-dbSimples> }
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, TypInfo, Types, Tag, tcp_udpport, ProtocolTypes,
  s7plustypes, GP2ES7CommPlusDriver;

function DumpAttrBytes(const FileName:String; Aid:Integer):TBytes;
var
  L:TStringList;
  i, cur:Integer;
  s, hex:String;
begin
  SetLength(Result,0); cur:=-1;
  L := TStringList.Create;
  try
    L.LoadFromFile(FileName);
    for i:=0 to L.Count-1 do begin
      s := Trim(L[i]);
      if Copy(s,1,5)='attr ' then
        cur := StrToIntDef(Trim(Copy(s,6,Pos(':',s)-6)),-1)
      else if (Copy(s,1,6)='bytes:') and (cur=Aid) then begin
        hex := Trim(Copy(s,7,Length(s)));
        SetLength(Result, Length(hex) div 2);
        for cur:=0 to High(Result) do Result[cur] := StrToInt('$'+Copy(hex,2*cur+1,2));
        exit;
      end;
    end;
  finally
    L.Free;
  end;
end;

var
  Port:TTCP_UDPPort;
  Drv:TGP2ES7CommPlusDriver;
  Img:TGP2DataBlockImage;
  U,P:String;
  w:Integer;
  r:TProtocolIOResult;
begin
  if ParamCount<2 then begin
    WriteLn('Uso: S7PLUS_USER=.. S7PLUS_PASSWORD=.. ',ExtractFileName(ParamStr(0)),' <ip> <dumpfile>');
    Halt(1);
  end;
  U := GetEnvironmentVariable('S7PLUS_USER'); P := GetEnvironmentVariable('S7PLUS_PASSWORD');

  Img := Default(TGP2DataBlockImage);
  Img.Name := 'dbSimples'; Img.Number := 172; Img.Language := 5;
  Img.IdentES := DumpAttrBytes(ParamStr(2), 2449);
  Img.InterfaceDescription := DumpAttrBytes(ParamStr(2), 2544);
  Img.LineComments := DumpAttrBytes(ParamStr(2), 2546);
  Img.IfrHash := DumpAttrBytes(ParamStr(2), 7949);
  Img.AlarmIfrHash := DumpAttrBytes(ParamStr(2), 7951);
  //layout para o 7733: otimizado, Volatile 864; aInt (OperandType 5) opt 32 / std 0, Volatile(3)
  Img.Layout.OptimizedAccess := true;
  Img.Layout.VolatileAreaSize := 864;
  SetLength(Img.Layout.Lines, 1);
  Img.Layout.Lines[0] := Default(TGP2LayoutLine);
  Img.Layout.Lines[0].Name := 'aInt';
  Img.Layout.Lines[0].TypeClass := 5;
  Img.Layout.Lines[0].Remanence := 3;
  Img.Layout.Lines[0].OptimizedOffset := 32;
  Img.Layout.Lines[0].StandardOffset := 0;

  WriteLn('imagem: IdentES=',Length(Img.IdentES),'B IntfDesc=',Length(Img.InterfaceDescription),
          'B 7949=',Length(Img.IfrHash),'B 7951=',Length(Img.AlarmIfrHash),'B');
  WriteLn('7733 computado = 0x', IntToHex(GP2DbStructureSignature(Img.Layout), 16));

  Port := TTCP_UDPPort.Create(nil);
  Drv := TGP2ES7CommPlusDriver.Create(nil);
  try
    Port.Host := ParamStr(1); Port.Port := 102; Port.Timeout := 8000; Port.EnableAutoReconnect := true;
    Drv.CommunicationPort := Port; Drv.Username := U; Drv.Password := P;
    Port.Active := true;
    w := 0; while (not Port.ReallyActive) and (w<6000) do begin Sleep(100); Inc(w,100); end;
    if not Port.ReallyActive then begin WriteLn('sem conexao'); Halt(2); end;
    WriteLn('conectado ao ', ParamStr(1), ' - chamando DownloadDataBlock...');
    r := Drv.DownloadDataBlock(Img);
    WriteLn('DownloadDataBlock -> ', GetEnumName(TypeInfo(TProtocolIOResult), Ord(r)));
  finally
    Drv.Free; Port.Free;
  end;
end.
