program s7commplus_dedup_test;

{: Valida o dedup/overlap-detection: cria um TPLCBlock lendo a struct inteira
   (DB4.Var26) e um TPLCTagNumber lendo so um campo dela (DB4.Var26.v2), e confirma
   que so UMA leitura GetMultiVariables acontece por ciclo (a do campo deve vir "de
   graca", fatiada do blob da struct).

   Uso: s7commplus_dedup_test <ip-do-clp>
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, PLCBlock, PLCTagNumber, tcp_udpport, S7PlusFamily, S7PlusConnection;

type
  TTestHost = class
    BlockUpdates, FieldUpdates, ReadCount:Integer;
    Blk:TPLCBlock;
    FieldTag:TPLCTagNumber;
    procedure OnDebug(Sender:TObject; const Msg:String);
    procedure BlockUpdated(Sender:TObject);
    procedure FieldUpdated(Sender:TObject);
  end;

procedure TTestHost.OnDebug(Sender:TObject; const Msg:String);
begin
  if Pos('functionCode=$054C', Msg)>0 then //GetMultiVariables request
    Inc(ReadCount);
  WriteLn('  [debug] ', Msg);
end;

procedure TTestHost.BlockUpdated(Sender:TObject);
var
  i:Integer;
  s:String;
begin
  Inc(BlockUpdates);
  s := '';
  for i:=0 to Blk.Size-1 do
    s := s + Format('%.2x ', [Trunc(Blk.ValueRaw[i])]);
  WriteLn(Format('  >> BLOCK update #%d: %s = %s', [BlockUpdates, Blk.Name, s]));
end;

procedure TTestHost.FieldUpdated(Sender:TObject);
begin
  Inc(FieldUpdates);
  WriteLn(Format('  >> FIELD update #%d: %s = %g', [FieldUpdates, FieldTag.Name, FieldTag.Value]));
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  Host_:TTestHost;
  Host:String;
  WaitMs, i:Integer;

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

    //Struct inteira primeiro (vira o "container").
    Host_.Blk := TPLCBlock.Create(nil);
    Host_.Blk.Name := 'Blk_DB4_Var26';
    Host_.Blk.Size := 7;
    Host_.Blk.OnUpdate := @Host_.BlockUpdated;
    Host_.Blk.RefreshTime := 1000;
    Host_.Blk.LongAddress := 'DB4.Var26';
    Host_.Blk.ProtocolDriver := Driver;

    //Um campo dela - deve virar "derivado" e nunca gerar sua propria leitura.
    Host_.FieldTag := TPLCTagNumber.Create(nil);
    Host_.FieldTag.Name := 'Tag_DB4_Var26_v2';
    Host_.FieldTag.TagType := pttWord;
    Host_.FieldTag.OnUpdate := @Host_.FieldUpdated;
    Host_.FieldTag.RefreshTime := 1000;
    Host_.FieldTag.LongAddress := 'DB4.Var26.v2';
    Host_.FieldTag.ProtocolDriver := Driver;

    WriteLn('=== Block + Tag criados, aguardando updates automaticos ===');

    for i:=1 to 30 do begin
      Sleep(100);
      CheckSynchronize;
      Sleep(400);
      CheckSynchronize;
      if (Host_.BlockUpdates>=2) and (Host_.FieldUpdates>=2) then break;
    end;

    WriteLn(Format('=== RESULTADO: BlockUpdates=%d FieldUpdates=%d GetMultiVariables enviados=%d ===',
      [Host_.BlockUpdates, Host_.FieldUpdates, Host_.ReadCount]));
    if (Host_.BlockUpdates>0) and (Host_.FieldUpdates>0) then begin
      if Host_.ReadCount<=(Host_.BlockUpdates+2) then
        WriteLn('=== OK: dedup funcionando (numero de leituras bate com so o Block sendo lido) ===')
      else
        WriteLn('=== ATENCAO: mais leituras do que esperado - dedup pode nao estar funcionando ===');
    end else
      WriteLn('=== FALHA: algum dos dois nunca atualizou ===');
  finally
    Host_.Blk.Free;
    Host_.FieldTag.Free;
    Driver.Free;
    Port.Free;
    Host_.Free;
  end;
end.
