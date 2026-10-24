program s7commplus_scanbatch_test;

{: Valida que DoScanRead agora bate varios tags independentes (sem relacao de
   dedup entre eles) num unico GetMultiVariables por ciclo, em vez de um por vez.
   Cria 4 tags independentes (DB4.Var1..3, MArea.System_Byte) com o mesmo
   RefreshTime e conta quantos GetMultiVariables realmente saem no fio.

   Uso: s7commplus_scanbatch_test <ip-do-clp>
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, PLCTagNumber, tcp_udpport, S7PlusFamily, S7PlusConnection;

type
  TTestHost = class
    Updates:array[0..3] of Integer;
    ReadCount:Integer;
    Tags:array[0..3] of TPLCTagNumber;
    procedure OnDebug(Sender:TObject; const Msg:String);
    procedure Tag0Updated(Sender:TObject);
    procedure Tag1Updated(Sender:TObject);
    procedure Tag2Updated(Sender:TObject);
    procedure Tag3Updated(Sender:TObject);
  end;

procedure TTestHost.OnDebug(Sender:TObject; const Msg:String);
begin
  if Pos('functionCode=$054C', Msg)>0 then
    Inc(ReadCount);
  WriteLn('  [debug] ', Msg);
end;

procedure TTestHost.Tag0Updated(Sender:TObject);
begin
  Inc(Updates[0]);
  WriteLn(Format('  >> %s update #%d = %g',[Tags[0].Name, Updates[0], Tags[0].Value]));
end;

procedure TTestHost.Tag1Updated(Sender:TObject);
begin
  Inc(Updates[1]);
  WriteLn(Format('  >> %s update #%d = %g',[Tags[1].Name, Updates[1], Tags[1].Value]));
end;

procedure TTestHost.Tag2Updated(Sender:TObject);
begin
  Inc(Updates[2]);
  WriteLn(Format('  >> %s update #%d = %g',[Tags[2].Name, Updates[2], Tags[2].Value]));
end;

procedure TTestHost.Tag3Updated(Sender:TObject);
begin
  Inc(Updates[3]);
  WriteLn(Format('  >> %s update #%d = %g',[Tags[3].Name, Updates[3], Tags[3].Value]));
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  Host_:TTestHost;
  Host:String;
  WaitMs, i, TotalUpdates:Integer;

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

    for i:=0 to 3 do
      Host_.Tags[i] := TPLCTagNumber.Create(nil);

    Host_.Tags[0].Name := 'Tag_DB4_Var1';
    Host_.Tags[0].TagType := pttLongInt;
    Host_.Tags[0].OnUpdate := @Host_.Tag0Updated;
    Host_.Tags[0].RefreshTime := 1000;
    Host_.Tags[0].LongAddress := 'DB4.Var1';
    Host_.Tags[0].ProtocolDriver := Driver;

    Host_.Tags[1].Name := 'Tag_DB4_Var2';
    Host_.Tags[1].TagType := pttLongInt;
    Host_.Tags[1].OnUpdate := @Host_.Tag1Updated;
    Host_.Tags[1].RefreshTime := 1000;
    Host_.Tags[1].LongAddress := 'DB4.Var2';
    Host_.Tags[1].ProtocolDriver := Driver;

    Host_.Tags[2].Name := 'Tag_DB4_Var3';
    Host_.Tags[2].TagType := pttLongInt;
    Host_.Tags[2].OnUpdate := @Host_.Tag2Updated;
    Host_.Tags[2].RefreshTime := 1000;
    Host_.Tags[2].LongAddress := 'DB4.Var3';
    Host_.Tags[2].ProtocolDriver := Driver;

    Host_.Tags[3].Name := 'Tag_MArea_SysByte';
    Host_.Tags[3].TagType := pttByte;
    Host_.Tags[3].OnUpdate := @Host_.Tag3Updated;
    Host_.Tags[3].RefreshTime := 1000;
    Host_.Tags[3].LongAddress := 'MArea.System_Byte';
    Host_.Tags[3].ProtocolDriver := Driver;

    WriteLn('=== 4 tags independentes criados (DB4.Var1..3, MArea.System_Byte), RefreshTime=1000ms ===');

    for i:=1 to 40 do begin
      Sleep(100);
      CheckSynchronize;
      Sleep(400);
      CheckSynchronize;
      TotalUpdates := Host_.Updates[0]+Host_.Updates[1]+Host_.Updates[2]+Host_.Updates[3];
      if TotalUpdates>=12 then break; //3 rounds x 4 tags
    end;

    WriteLn(Format('=== RESULTADO: updates=[%d,%d,%d,%d]  GetMultiVariables enviados=%d ===',
      [Host_.Updates[0], Host_.Updates[1], Host_.Updates[2], Host_.Updates[3], Host_.ReadCount]));

    //Se batching estiver funcionando, o numero de requests deve ser proximo do numero de
    //RODADAS de scan (cada request atualiza os 4 de uma vez), nao 4x o numero de rodadas.
    if (Host_.Updates[0]>0) and (Host_.Updates[1]>0) and (Host_.Updates[2]>0) and (Host_.Updates[3]>0) then begin
      if Host_.ReadCount<=(Host_.Updates[0]+2) then
        WriteLn('=== OK: batching funcionando (1 request atualiza os 4 tags de uma vez) ===')
      else
        WriteLn('=== ATENCAO: mais requests que o esperado - parece que NAO esta batchando ===');
    end else
      WriteLn('=== FALHA: nem todos os tags atualizaram ===');
  finally
    for i:=0 to 3 do
      Host_.Tags[i].Free;
    Driver.Free;
    Port.Free;
    Host_.Free;
  end;
end.
