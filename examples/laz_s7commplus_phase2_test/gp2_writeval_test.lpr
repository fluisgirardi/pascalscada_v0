program gp2_writeval_test;
{ Teste de ESCRITA SEGURA e REVERSÍVEL (valor de variável) num DB de teste, para confirmar o
  caminho de escrita ao vivo. Lê o valor atual, escreve um novo, confere, e RESTAURA o original.
  Não cria/apaga objetos (nada destrutivo). Credenciais via ambiente.
  Uso: S7PLUS_USER=.. S7PLUS_PASSWORD=.. gp2_writeval_test <ip> <DBnumber> <VarName> }
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, TypInfo, Types, Tag, tcp_udpport, ProtocolTypes,
  s7plustypes, s7plustypeinfo, s7plusfamily;

function Hex(const B:TBytes):String; var i:Integer; begin Result:=''; for i:=0 to High(B) do Result:=Result+IntToHex(B[i],2); end;

var
  Port:TTCP_UDPPort; Drv:TS7CommPlusDriver;
  Vars:TS7PlusVarInfoArray; U,P,VarName:String; DBn,w,i,vi:Integer;
  area:Cardinal; lids:TS7PlusLIDArray; orig,neu,chk:TBytes; sdt:Byte;
begin
  if ParamCount<3 then begin WriteLn('Uso: ... <ip> <DBnumber> <VarName>'); Halt(1); end;
  U:=GetEnvironmentVariable('S7PLUS_USER'); P:=GetEnvironmentVariable('S7PLUS_PASSWORD');
  DBn:=StrToInt(ParamStr(2)); VarName:=ParamStr(3);
  Port:=TTCP_UDPPort.Create(nil); Drv:=TS7CommPlusDriver.Create(nil);
  try
    Port.Host:=ParamStr(1); Port.Port:=102; Port.Timeout:=8000; Port.EnableAutoReconnect:=true;
    Drv.CommunicationPort:=Port; Drv.Username:=U; Drv.Password:=P; Port.Active:=true;
    w:=0; while (not Port.ReallyActive) and (w<6000) do begin Sleep(100); Inc(w,100); end;
    if not Port.ReallyActive then begin WriteLn('sem conexao'); Halt(2); end;
    if Drv.BrowseDB(DBn, Vars)<>ioOk then begin WriteLn('BrowseDB falhou'); Halt(3); end;
    vi:=-1;
    for i:=0 to High(Vars) do if SameText(Vars[i].Name, VarName) then begin vi:=i; break; end;
    if vi<0 then begin
      WriteLn('variavel "',VarName,'" nao encontrada no DB',DBn,'. Variaveis (ate 20):');
      for i:=0 to High(Vars) do begin
        WriteLn(Format('  %s (type=%d size=%d)', [Vars[i].Name, Vars[i].SoftDataType, Vars[i].DataSize]));
        if i>=19 then break;
      end;
      Halt(4);
    end;
    area:=S7PlusIds_DBAccessAreaBase+Cardinal(DBn);
    lids:=Vars[vi].Lids; sdt:=Vars[vi].SoftDataType;
    WriteLn(Format('%s  type=%d size=%d', [Vars[vi].Name, sdt, Vars[vi].DataSize]));
    if Drv.ReadSymbolic(area, lids, orig)<>ioOk then begin WriteLn('read falhou'); Halt(5); end;
    WriteLn('valor atual = ', Hex(orig));
    //novo valor = original com o 1o byte invertido (mudança mínima, reversível)
    neu:=Copy(orig); if Length(neu)>0 then neu[0]:=neu[0] xor $01;
    WriteLn('escrevendo    ', Hex(neu), ' ...');
    if Drv.WriteSymbolic(area, lids, neu, sdt)<>ioOk then begin WriteLn('WRITE falhou (CLP recusou)'); end
    else begin
      if Drv.ReadSymbolic(area, lids, chk)=ioOk then
        WriteLn('releitura     ', Hex(chk), '  -> escrita ', BoolToStr(Hex(chk)=Hex(neu), 'CONFIRMADA','NAO confirmada'));
      //RESTAURA o valor original
      if Drv.WriteSymbolic(area, lids, orig, sdt)=ioOk then WriteLn('valor original RESTAURADO: ', Hex(orig))
      else WriteLn('ATENCAO: falha ao restaurar o valor original!');
    end;
  finally
    Drv.Free; Port.Free;
  end;
end.
