program s7commplus_structitems_test;

{: Le um DB inteiro (ex. DB4) com um TPLCStruct e le cada membro por um TPLCStructItem.

   O TPLCStruct (LongAddress = nome do DB, sem ponto) traz o DB todo numa leitura so; cada
   TPLCStructItem aponta para ele (PLCBlock), diz em que byte comeca (Index) e de que tipo e'
   (TagType), e passa a valer o que a struct leu - sem pedir nada mais ao CLP.

   Os membros do DB4 de teste sao Var1..Var25, DInt de 4 bytes em sequencia (Var<n> no byte
   (n-1)*4). O CLP guarda em big-endian, por isso os itens usam SwapBytes e SwapWords.
   Cada valor do item e' conferido contra os mesmos 4 bytes da struct montados a mao.

   Uso: s7commplus_structitems_test <ip-do-clp> [DB] [refresh-ms] [segundos]
   Exemplo: s7commplus_structitems_test 192.168.56.2 DB4 500 10
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, PLCStruct, PLCStructElement, tcp_udpport, S7PlusFamily;

const
  MemberCount = 25;  //Var1..Var25 (Var26 e' outra coisa, de 15 bytes)
  MemberSize  = 4;

type
  THost = class
    Struct:TPLCStruct;
    Items:array[1..MemberCount] of TPLCStructItem;
    Updates:Integer;
    Mismatches:Integer;
    procedure StructUpdated(Sender:TObject);
    function ExpectedOf(N:Integer):LongInt;
  end;

function THost.ExpectedOf(N:Integer):LongInt;
var
  Off:Integer;
begin
  Off := (N-1)*MemberSize;
  Result := LongInt((Cardinal(Struct.GetByte(Off)) shl 24) or (Cardinal(Struct.GetByte(Off+1)) shl 16) or
                    (Cardinal(Struct.GetByte(Off+2)) shl 8) or Cardinal(Struct.GetByte(Off+3)));
end;

procedure THost.StructUpdated(Sender:TObject);
var
  n:Integer;
  s:String;
  Bad:Boolean;
begin
  Inc(Updates);
  s := '';
  Bad := false;
  for n:=1 to MemberCount do begin
    s := s + Format('Var%d=%d ', [n, Trunc(Items[n].Value)]);
    if Trunc(Items[n].Value)<>ExpectedOf(n) then begin
      Bad := true;
      Inc(Mismatches);
    end;
  end;
  WriteLn(Format('update #%d (%d bytes)%s', [Updates, Struct.Size, BoolToStr(Bad, '  <<< ITEM DIVERGE DOS BYTES', '')]));
  if Updates<=2 then
    WriteLn('  ', s);
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  H:THost;
  Ip, DbName:String;
  RefreshMs, Seconds, WaitMs, n:Integer;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> [DB] [refresh-ms] [segundos]');
    Halt(1);
  end;
  Ip := ParamStr(1);
  DbName := 'DB4';
  RefreshMs := 500;
  Seconds := 10;
  if ParamCount>=2 then DbName := ParamStr(2);
  if ParamCount>=3 then RefreshMs := StrToIntDef(ParamStr(3), RefreshMs);
  if ParamCount>=4 then Seconds := StrToIntDef(ParamStr(4), Seconds);

  Port := TTCP_UDPPort.Create(nil);
  Driver := TS7CommPlusDriver.Create(nil);
  H := THost.Create;
  try
    Port.Host := Ip;
    Port.Port := 102;
    Port.Timeout := 8000;
    Port.EnableAutoReconnect := false;
    Driver.CommunicationPort := Port;

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

    //a struct cobre os membros que vamos ler (Size trunca o DB ao inicio dele)
    H.Struct := TPLCStruct.Create(nil);
    H.Struct.Name := 'Struct_'+DbName;
    H.Struct.Size := MemberCount*MemberSize;
    H.Struct.OnUpdate := @H.StructUpdated;
    H.Struct.RefreshTime := RefreshMs;
    H.Struct.LongAddress := DbName;
    H.Struct.ProtocolDriver := Driver;

    for n:=1 to MemberCount do begin
      H.Items[n] := TPLCStructItem.Create(nil);
      H.Items[n].Name := Format('Var%d', [n]);
      H.Items[n].PLCBlock := H.Struct;
      H.Items[n].TagType := pttLongInt;  //DInt
      H.Items[n].SwapBytes := true;      //big-endian do CLP
      H.Items[n].SwapWords := true;
      H.Items[n].Index := (n-1)*MemberSize;
    end;
    WriteLn(Format('=== Struct "%s" (%d bytes) com %d TPLCStructItem, RefreshTime=%dms, por %ds ===',
                   [DbName, H.Struct.Size, MemberCount, RefreshMs, Seconds]));

    for n:=1 to Seconds*10 do begin
      Sleep(100);
      CheckSynchronize;
    end;

    if (H.Updates>0) and (H.Mismatches=0) then
      WriteLn(Format('=== OK: %d atualizacoes, todos os itens batem com os bytes da struct ===', [H.Updates]))
    else if H.Updates=0 then
      WriteLn('=== FALHA: a struct nunca atualizou ===')
    else
      WriteLn(Format('=== FALHA: %d itens divergiram dos bytes ===', [H.Mismatches]));
    if (H.Updates=0) or (H.Mismatches<>0) then Halt(1);
  finally
    for n:=1 to MemberCount do
      H.Items[n].Free;
    H.Struct.Free;
    Driver.Free;
    Port.Free;
    H.Free;
  end;
end.
