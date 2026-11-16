program gp2_orchestrated_test;
{ TESTE de TGP2ES7CommPlusDriver.DownloadBlockOrchestrated: re-toca a arvore nativa serializada
  (stream interno remontado da captura em claro) sobre a sessao viva do .4, com remap de ids.
  EXPERIMENTAL - o download real cruza varias conexoes com GetLink/Explore interdependentes;
  este teste exercita o motor e mostra ate onde o CLP processa.
  Credenciais via ambiente. Uso: S7PLUS_USER=.. S7PLUS_PASSWORD=.. gp2_orchestrated_test <ip> <inner.bin> }
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, TypInfo, Types, Tag, tcp_udpport, ProtocolTypes,
  s7plustypes, s7plustypeinfo, s7plusfamily, GP2ES7CommPlusDriver;

function LoadFile(const fn:String):TBytes;
var f:TFileStream;
begin
  f := TFileStream.Create(fn, fmOpenRead);
  try SetLength(Result, f.Size); if f.Size>0 then f.ReadBuffer(Result[0], f.Size);
  finally f.Free; end;
end;

//distinct 4-byte ids referenced by a 0x70 tag in the inner stream (object ids to remap)
function CollectIds(const B:TBytes):TCardinalDynArray;
var i,n,k:Integer; id:Cardinal; dup:Boolean;
begin
  SetLength(Result,0); n:=0; i:=0;
  while i<=Length(B)-5 do begin
    if B[i]=$70 then begin
      id := (B[i+1] shl 24) or (B[i+2] shl 16) or (B[i+3] shl 8) or B[i+4];
      if (id and $FFFF0000)=$00100000 then begin  //0x0010xxxx look like session object ids
        dup:=false; for k:=0 to n-1 do if Result[k]=id then dup:=true;
        if not dup then begin SetLength(Result,n+1); Result[n]:=id; Inc(n); end;
      end;
    end;
    Inc(i);
  end;
end;

var
  Port:TTCP_UDPPort;
  Drv:TGP2ES7CommPlusDriver;
  U,P:String; w,i:Integer;
  inner:TBytes; ids:TCardinalDynArray;
  Steps:TGP2StepResults; r:TProtocolIOResult;
  Blocks:TS7PlusDataBlockInfoArray;
begin
  if ParamCount<2 then begin WriteLn('Uso: ... <ip> <inner.bin>'); Halt(1); end;
  U:=GetEnvironmentVariable('S7PLUS_USER'); P:=GetEnvironmentVariable('S7PLUS_PASSWORD');
  inner := LoadFile(ParamStr(2));
  ids := CollectIds(inner);
  WriteLn('inner=',Length(inner),'B  ids candidatos=',Length(ids));
  for i:=0 to High(ids) do WriteLn('  captured id[',i,']=0x',IntToHex(ids[i],8));

  Port := TTCP_UDPPort.Create(nil);
  Drv := TGP2ES7CommPlusDriver.Create(nil);
  try
    Port.Host:=ParamStr(1); Port.Port:=102; Port.Timeout:=8000; Port.EnableAutoReconnect:=true;
    Drv.CommunicationPort:=Port; Drv.Username:=U; Drv.Password:=P;
    Port.Active:=true;
    w:=0; while (not Port.ReallyActive) and (w<6000) do begin Sleep(100); Inc(w,100); end;
    if not Port.ReallyActive then begin WriteLn('sem conexao'); Halt(2); end;
    //estabelece a sessão S7plus via um método público conhecido antes de orquestrar
    WriteLn('ListDataBlocks (abre sessão) -> ', GetEnumName(TypeInfo(TProtocolIOResult), Ord(Drv.ListDataBlocks(Blocks))), ' (', Length(Blocks), ' DBs)');
    //O download ESCREVE no CLP: só roda com GP2_DO_DOWNLOAD=1 (proteção p/ CLPs de programa real).
    if GetEnvironmentVariable('GP2_DO_DOWNLOAD')<>'1' then begin
      WriteLn('(modo somente-leitura: defina GP2_DO_DOWNLOAD=1 para disparar o DownloadBlockOrchestrated)');
    end else begin
      WriteLn('conectado ao ',ParamStr(1),' - DownloadBlockOrchestrated...');
      r := Drv.DownloadBlockOrchestrated(0, inner, ids, 0, Steps);
      WriteLn('resultado=', GetEnumName(TypeInfo(TProtocolIOResult), Ord(r)));
      WriteLn('passos executados=', Length(Steps));
      for i:=0 to High(Steps) do
        WriteLn(Format('  #%d func=$%.4x ok=%s retval=$%.2x resp=%dB',
          [i, Steps[i].Func, BoolToStr(Steps[i].Ok,true), Steps[i].RetVal, Length(Steps[i].Resp)]));
    end;
  finally
    Drv.Free; Port.Free;
  end;
end.
