program s7commplus_onchange_test;

{: Tags "on change": TPLCTagNumber com RefreshTime=0 recebem o valor por NOTIFICACAO de uma
   subscricao do CLP, sem polling. O driver cria a subscricao sozinho assim que resolve o Path.

   Uso: s7commplus_onchange_test <ip-do-clp> <path>[:tipo] [<path>[:tipo] ...] [-t segundos]
   tipo: dint (padrao), int, byte, real
   Exemplo: s7commplus_onchange_test 192.168.137.210 DB4.Var1 DB4.Var2:int DB4.Var3:real -t 120

   Mude os valores no CLP/simulador (tabela de observacao do TIA, ou um programa) enquanto
   roda: cada mudanca deve aparecer aqui UMA vez, logo depois de acontecer, e valores que nao
   mudam nao devem aparecer de novo. Ao final, um resumo por tag.

   Nota: um tag que nao conseguir subscricao cai para polling de 1 s (veja as linhas
   [debug] "ManageSubscriptions").
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, PLCTagNumber, tcp_udpport, S7PlusFamily, S7PlusConnection;

type
  TWatched = class
    Tag_:TPLCTagNumber;
    Updates:Integer;
    FirstAt:QWord;
    procedure TagUpdated(Sender:TObject);
  end;

  TDebugSink = class
    procedure OnDebug(Sender:TObject; const Msg:String);
  end;

var
  StartedAt:QWord;

procedure TWatched.TagUpdated(Sender:TObject);
begin
  Inc(Updates);
  if Updates=1 then FirstAt := GetTickCount64;
  WriteLn(Format('  >> %6dms  %-16s = %g   (update #%d)', [GetTickCount64-StartedAt, Tag_.Name, Tag_.Value, Updates]));
end;

procedure TDebugSink.OnDebug(Sender:TObject; const Msg:String);
begin
  //so o que interessa a subscricoes; o resto e' ruido neste teste
  if (Pos('ManageSubscriptions', Msg)>0) or (Pos('Subscricao', Msg)>0) or (Pos('PumpNotifications', Msg)>0) or
     (Pos('QueueNotification', Msg)>0) or (Pos('PollNotification', Msg)>0) or (Pos('intercalada', Msg)>0) then
    WriteLn('  [debug] ', Msg);
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  Sink:TDebugSink;
  Watched:array of TWatched;
  Host, Spec, PathPart, TypePart:String;
  Seconds, WaitMs, i, p:Integer;
  Silent:Boolean;

begin
  if ParamCount<2 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> <path>[:tipo] [<path>[:tipo] ...] [-t segundos]');
    Halt(1);
  end;

  Host := ParamStr(1);
  Seconds := 60;
  SetLength(Watched, 0);
  i := 2;
  while i<=ParamCount do begin
    if ParamStr(i)='-t' then begin
      Seconds := StrToIntDef(ParamStr(i+1), Seconds);
      Inc(i, 2);
      continue;
    end;
    SetLength(Watched, Length(Watched)+1);
    Watched[High(Watched)] := TWatched.Create;
    Watched[High(Watched)].Tag_ := TPLCTagNumber.Create(nil);
    Spec := ParamStr(i);
    p := Pos(':', Spec);
    if p>0 then begin
      PathPart := Copy(Spec, 1, p-1);
      TypePart := LowerCase(Copy(Spec, p+1, MaxInt));
    end else begin
      PathPart := Spec;
      TypePart := 'dint';
    end;
    with Watched[High(Watched)].Tag_ do begin
      Name := 'Tag_'+StringReplace(PathPart, '.', '_', [rfReplaceAll]);
      if TypePart='int' then begin TagType := pttSmallInt; SwapBytes := true; end
      else if TypePart='byte' then TagType := pttByte
      else if TypePart='real' then begin TagType := pttFloat; SwapBytes := true; SwapWords := true; end
      else begin TagType := pttLongInt; SwapBytes := true; SwapWords := true; end;
      OnUpdate := @Watched[High(Watched)].TagUpdated;
      RefreshTime := 0; //<- on change
      LongAddress := PathPart;
    end;
    Inc(i);
  end;
  if Length(Watched)=0 then begin
    WriteLn('Nenhum path informado.');
    Halt(1);
  end;

  Port := TTCP_UDPPort.Create(nil);
  Driver := TS7CommPlusDriver.Create(nil);
  Sink := TDebugSink.Create;
  try
    Port.Host := Host;
    Port.Port := 102;
    Port.Timeout := 8000;
    Driver.CommunicationPort := Port;
    Driver.OnDebug := @Sink.OnDebug;

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

    StartedAt := GetTickCount64;
    for i:=0 to High(Watched) do begin
      Watched[i].Tag_.ProtocolDriver := Driver;
      WriteLn(Format('Tag %s (Path=%s, RefreshTime=0) ligado ao driver.', [Watched[i].Tag_.Name, Watched[i].Tag_.LongAddress]));
    end;
    WriteLn(Format('=== Observando por %d s. Mude os valores no CLP. ===', [Seconds]));

    //Console app: the framework delivers tag updates through Synchronize, which has to be pumped.
    while GetTickCount64-StartedAt < QWord(Seconds)*1000 do begin
      CheckSynchronize;
      Sleep(20);
    end;
    CheckSynchronize;

    WriteLn;
    WriteLn('=== Resumo ===');
    Silent := true;
    for i:=0 to High(Watched) do begin
      WriteLn(Format('  %-16s updates=%d  primeiro em %dms  ultimo valor=%g  leituras OK=%d falhas=%d',
        [Watched[i].Tag_.Name, Watched[i].Updates,
         Int64(Watched[i].FirstAt)-Int64(StartedAt), Watched[i].Tag_.Value,
         Watched[i].Tag_.CommReadsOK, Watched[i].Tag_.CommReadErrors]));
      if Watched[i].Updates>0 then Silent := false;
    end;
    if Silent then
      WriteLn('=== FALHA: nenhum tag recebeu valor ===')
    else
      WriteLn('=== OK: houve entrega de valores sem polling (confira se so as mudancas apareceram) ===');
  finally
    for i:=0 to High(Watched) do begin
      Watched[i].Tag_.Free;
      Watched[i].Free;
    end;
    Driver.Free;
    Port.Free;
    Sink.Free;
  end;
end.
