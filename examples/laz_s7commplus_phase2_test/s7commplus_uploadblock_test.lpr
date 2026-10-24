program s7commplus_uploadblock_test;

{: Testa TS7CommPlusDriver.UploadBlock (leitura do corpo compilado de um bloco via
   GetVarSubStreamed, endereco de 2 elementos [BlockType,BlockNumber]) contra DB4
   (BlockType=25=BLOCK_DB per S7PlusSoftDataType).

   Uso: s7commplus_uploadblock_test <ip-do-clp> [blocktype] [blocknumber]
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Math,
  Tag, tcp_udpport, S7PlusFamily, S7PlusConnection;

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
  Host:String;
  WaitMs, i:Integer;
  Res:TProtocolIOResult;
  Data:TBytes;
  BlockType, BlockNumber:Cardinal;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> [blocktype] [blocknumber]');
    Halt(1);
  end;

  Host := ParamStr(1);
  if ParamCount>=2 then BlockType := StrToInt(ParamStr(2)) else BlockType := 25; //BLOCK_DB
  if ParamCount>=3 then BlockNumber := StrToInt(ParamStr(3)) else BlockNumber := 4;

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

    WriteLn(Format('=== UploadBlock(BlockType=%d, BlockNumber=%d) ===',[BlockType, BlockNumber]));
    Res := Driver.UploadBlock(BlockType, BlockNumber, Data);
    WriteLn('  Resultado: ', Ord(Res));
    WriteLn('  Bytes recebidos: ', Length(Data));
    if Length(Data)>0 then begin
      Write('  Primeiros bytes: ');
      for i:=0 to Min(63,High(Data)) do
        Write(Format('%.2x ',[Data[i]]));
      WriteLn;
    end;

  finally
    Driver.Free;
    Port.Free;
    DebugSink.Free;
  end;
end.
