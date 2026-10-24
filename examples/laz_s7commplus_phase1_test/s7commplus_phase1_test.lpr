program s7commplus_phase1_test;

{: Fase 1 do driver S7CommPlus: conecta em um CLP S7-1200/1500 (ou PLCSim Advanced),
   estabelece a sessão V1 (sem TLS) e faz leitura/escrita de uma DB e da área M por
   endereço numérico (sem resolução de Path simbólico - isso é a Fase 2).

   Uso: s7commplus_phase1_test <ip-do-clp> [db] [start] [size]
   Exemplo: s7commplus_phase1_test 192.168.0.1 1 0 16
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, tcp_udpport, S7PlusFamily, S7PlusConnection;

type
  //: Recebe o rastro de depuracao do driver (COTP/InitSSL/CreateObject/SetupSession/
  //: GetMultiVariables/SetMultiVariables) e imprime no console.
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
  Data, WriteBuf:TBytes;
  Res:TProtocolIOResult;
  i, DBNumber, StartAddr, ReadSize, WaitMs, TimeoutMs:Integer;
  Host:String;

procedure DumpBytes(const Caption:String; const B:TBytes);
var
  j:Integer;
begin
  Write(Caption, ' = ');
  for j:=0 to High(B) do
    Write(Format('%.2x ',[B[j]]));
  WriteLn;
end;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> [db] [start] [size] [timeout-ms]');
    Halt(1);
  end;

  Host := ParamStr(1);
  DBNumber := 1;
  StartAddr := 0;
  ReadSize := 16;
  TimeoutMs := 8000; //handshake TLS (troca de certificado) pode demorar mais que o default de 1000ms
  if ParamCount>=2 then DBNumber := StrToIntDef(ParamStr(2), DBNumber);
  if ParamCount>=3 then StartAddr := StrToIntDef(ParamStr(3), StartAddr);
  if ParamCount>=4 then ReadSize := StrToIntDef(ParamStr(4), ReadSize);
  if ParamCount>=5 then TimeoutMs := StrToIntDef(ParamStr(5), TimeoutMs);

  Port := TTCP_UDPPort.Create(nil);
  Driver := TS7CommPlusDriver.Create(nil);
  DebugSink := TDebugSink.Create;
  try
    Port.Host := Host;
    Port.Port := 102;
    Port.Timeout := TimeoutMs;
    WriteLn('Timeout da porta: ', TimeoutMs, 'ms');
    Driver.CommunicationPort := Port;
    Driver.OnDebug := @DebugSink.OnDebug;

    WriteLn('Abrindo porta TCP com ', Host, ':102 ...');
    Port.Active := true;

    //Port.Active dispara a conexao numa thread de fundo (TConnectThread) e retorna
    //na hora; ReallyActive so fica true quando a thread termina de conectar. Espera
    //ate 5s por isso antes de desistir.
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

    WriteLn('Conectando sessao S7CommPlus (V1, sem TLS)... (a 1a leitura abaixo dispara a conexao)');

    WriteLn(Format('Lendo DB%d[%d..%d]...',[DBNumber, StartAddr, StartAddr+ReadSize-1]));
    Res := Driver.ReadDB(DBNumber, StartAddr, ReadSize, Data);
    if Res=ioOk then
      DumpBytes(Format('DB%d[%d..%d]',[DBNumber, StartAddr, StartAddr+ReadSize-1]), Data)
    else
      WriteLn('Falha na leitura da DB: ', Ord(Res));

    WriteLn(Format('Lendo M%d.0..M%d.0 (%d bytes, area Flags)...',[StartAddr, StartAddr+ReadSize-1, ReadSize]));
    Res := Driver.ReadArea(S7PlusRF_Flags, StartAddr, ReadSize, Data);
    if Res=ioOk then
      DumpBytes('M', Data)
    else
      WriteLn('Falha na leitura de M: ', Ord(Res));

    if Res=ioOk then begin
      SetLength(WriteBuf, Length(Data));
      for i:=0 to High(Data) do
        WriteBuf[i] := Data[i]; //escreve de volta o mesmo valor lido (round-trip de teste)
      WriteLn('Escrevendo de volta os mesmos bytes em M (round-trip de teste)...');
      Res := Driver.WriteArea(S7PlusRF_Flags, StartAddr, WriteBuf);
      if Res=ioOk then
        WriteLn('Escrita em M OK.')
      else
        WriteLn('Falha na escrita em M: ', Ord(Res));
    end;

    WriteLn('Sessao conectada: ', BoolToStr(Driver.Connected, true));
  finally
    Driver.Free;
    Port.Free;
    DebugSink.Free;
  end;
end.
