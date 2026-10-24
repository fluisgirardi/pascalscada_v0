program s7commplus_write_diag;

{: Diagnostico da rejeicao de escrita (WriteSymbolic) - ver se o CLP devolve um returnValue/
   texto de erro especifico, e compara o payload de uma leitura (que funciona) com o de uma
   escrita (que falha), para achar a causa raiz. Se uma senha for passada, tenta Authenticate
   (legitimacao) antes da escrita, para validar se isso destrava o SetMultiVariables.

   Uso: s7commplus_write_diag <ip-do-clp> [senha]
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

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  DebugSink:TDebugSink;
  Host, Password:String;
  WaitMs, i:Integer;
  DBVars:TS7PlusVarInfoArray;
  Res:TProtocolIOResult;
  Var1Idx:Integer;
  Data, ReadBack:TBytes;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp>');
    Halt(1);
  end;

  Host := ParamStr(1);
  if ParamCount>=2 then Password := ParamStr(2) else Password := '';

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

    WriteLn('=== BrowseDB(4) ===');
    Res := Driver.BrowseDB(4, DBVars);
    if Res<>ioOk then begin
      WriteLn('Falha ao explorar DB4: ', Ord(Res));
      Halt(3);
    end;
    Var1Idx := -1;
    for i:=0 to High(DBVars) do
      if DBVars[i].Name='DB4.Var1' then Var1Idx := i;
    if Var1Idx<0 then begin
      WriteLn('DB4.Var1 nao encontrado.');
      Halt(3);
    end;
    WriteLn(Format('  DB4.Var1: Lids=%s SoftDataType=%d ByteOffset=%d',
      [S7PlusFormatLids(DBVars[Var1Idx].Lids), DBVars[Var1Idx].SoftDataType, DBVars[Var1Idx].ByteOffset]));

    WriteLn('=== ReadSymbolic (deve funcionar) ===');
    Res := Driver.ReadSymbolic(S7PlusIds_DBAccessAreaBase+4, DBVars[Var1Idx].Lids, Data);
    WriteLn('  Resultado leitura: ', Ord(Res));

    if Password<>'' then begin
      WriteLn('=== Authenticate (legitimacao) ===');
      Res := Driver.Authenticate(Password);
      WriteLn('  Resultado autenticacao: ', Ord(Res));
    end;

    WriteLn('=== WriteSymbolic (o que esta falhando) ===');
    SetLength(Data, 4);
    Data[0] := 0; Data[1] := 0; Data[2] := 0; Data[3] := 7;
    Res := Driver.WriteSymbolic(S7PlusIds_DBAccessAreaBase+4, DBVars[Var1Idx].Lids, Data, DBVars[Var1Idx].SoftDataType);
    WriteLn('  Resultado escrita: ', Ord(Res));

    WriteLn('=== ReadSymbolic apos a tentativa de escrita (o valor mudou?) ===');
    Res := Driver.ReadSymbolic(S7PlusIds_DBAccessAreaBase+4, DBVars[Var1Idx].Lids, ReadBack);
    if Res=ioOk then begin
      Write('  Valor lido = ');
      for i:=0 to High(ReadBack) do
        Write(Format('%.2x ',[ReadBack[i]]));
      WriteLn;
    end else
      WriteLn('  Falha na releitura: ', Ord(Res));

  finally
    Driver.Free;
    Port.Free;
    DebugSink.Free;
  end;
end.
