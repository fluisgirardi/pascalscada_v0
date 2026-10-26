program s7commplus_wholedb_write_test;

{: Escrita de um DB inteiro (Path "bare root", sem ponto) pelo S7CommPlus.

   O tag cobre o DB todo; a escrita corta os bytes em cada membro escalar (pelo ByteOffset)
   e manda tudo em SetMultiVariables. BOOL, STRING e structs nao sao escritos.

   Roteiro:
     1. le o DB inteiro (guarda o original)
     2. altera os 4 bytes de cada membro DB4.Var10..Var13 (DInt) no buffer e escreve o DB todo
     3. le cada DB4.VarN pelo Path simbolico proprio e confere o valor escrito
     4. escreve o original de volta e confere de novo
   Var1..Var8 mudam sozinhas (programa do CLP): ao escrever o DB inteiro elas recebem o
   valor lido no passo 1, que a logica do CLP volta a incrementar - inofensivo no PLCSim.

   Uso: s7commplus_wholedb_write_test <ip-do-clp> [DB] [-v]     (DB padrao: DB4)
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, ProtocolTypes, tcp_udpport, S7PlusFamily;

type
  THost = class
    Verbose:Boolean;
    Last:TProtocolIOResult;
    Vals:TArrayOfDouble;
    procedure OnDebug(Sender:TObject; const Msg:String);
    procedure Told(const ReqID:LongWord; Values:TArrayOfDouble; ValuesTimeStamp:QWord;
                   TagCommand:TTagCommand; LastResult:TProtocolIOResult; OffSet:LongInt);
  end;

procedure THost.OnDebug(Sender:TObject; const Msg:String);
begin
  if Verbose and (Pos('TPKT', Msg)=0) and (Pos('SSL_read', Msg)=0) and (Pos('COTP', Msg)=0) and
     (Pos('LogicalRecv', Msg)=0) and (Pos('Reassembled', Msg)=0) then
    WriteLn('  [debug] ', Copy(Msg, 1, 200));
end;

procedure THost.Told(const ReqID:LongWord; Values:TArrayOfDouble; ValuesTimeStamp:QWord;
  TagCommand:TTagCommand; LastResult:TProtocolIOResult; OffSet:LongInt);
begin
  Last := LastResult;
  Vals := Values;
end;

var
  Port:TTCP_UDPPort;
  Drv:TS7CommPlusDriver;
  H:THost;
  DbName:String;
  Whole, Member, Saved:TArrayOfDouble;
  Wr:TMultiWriteArray;
  tr:TTagRec;
  i, n, W, Fails:Integer;

function ReadPath(const Path:String; out Data:TArrayOfDouble):Boolean;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.Path := Path;
  tr.Size := 0;
  tr.CallBack := @H.Told;
  H.Last := ioNone;
  Drv.Read(tr);
  Data := H.Vals;
  Result := H.Last=ioOk;
end;

function MemberValue(N:Integer):Int64;
var
  Data:TArrayOfDouble;
begin
  Result := -1;
  if ReadPath(DbName+'.Var'+IntToStr(N), Data) and (Length(Data)>=4) then
    Result := (Trunc(Data[0]) shl 24) or (Trunc(Data[1]) shl 16) or (Trunc(Data[2]) shl 8) or Trunc(Data[3]);
end;

function WriteWhole(const Data:TArrayOfDouble):TProtocolIOResult;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.Path := DbName;
  tr.CallBack := @H.Told;
  SetLength(Wr, 1);
  FillChar(Wr[0], SizeOf(Wr[0]), 0);
  Wr[0].Tag := tr;
  Wr[0].Values := Data;
  Drv.MultiWrite(Wr);
  Result := Wr[0].Result;
end;

procedure Check(const What:String; Ok:Boolean);
begin
  if Ok then
    WriteLn('  OK     ', What)
  else begin
    WriteLn('  FALHOU ', What);
    Inc(Fails);
  end;
end;

begin
  DbName := 'DB4';
  H := THost.Create;
  Fails := 0;
  for i:=2 to ParamCount do
    if ParamStr(i)='-v' then H.Verbose := true
    else DbName := ParamStr(i);
  Port := TTCP_UDPPort.Create(nil);
  Drv := TS7CommPlusDriver.Create(nil);
  try
    Port.Host := ParamStr(1);
    Port.Port := 102;
    Port.Timeout := 6000;
    Port.EnableAutoReconnect := false;
    Drv.CommunicationPort := Port;
    Drv.OnDebug := @H.OnDebug;
    Port.Active := true;
    W := 0;
    while (not Port.ReallyActive) and (W<5000) do begin Sleep(100); Inc(W, 100); end;

    WriteLn('--- 1. leitura de "', DbName, '"');
    if not ReadPath(DbName, Saved) then begin
      WriteLn('leitura do DB inteiro falhou');
      Halt(2);
    end;
    WriteLn('  ', Length(Saved), ' bytes');
    Whole := Copy(Saved, 0, Length(Saved));

    WriteLn('--- 2. escrita do DB inteiro com Var10..Var13 alteradas');
    //Var10..Var13: 4 bytes cada, offsets 36, 40, 44, 48 (membros de 4 bytes em sequencia)
    for n:=10 to 13 do begin
      i := (n-1)*4;
      Whole[i]   := $01;
      Whole[i+1] := $02;
      Whole[i+2] := $00;
      Whole[i+3] := n;  //0x0102000N
    end;
    Check('MultiWrite do DB inteiro = ioOk', WriteWhole(Whole)=ioOk);

    WriteLn('--- 3. confere cada membro pelo Path proprio');
    for n:=10 to 13 do
      Check('Var'+IntToStr(n)+' = '+IntToStr($01020000+n), MemberValue(n)=$01020000+n);

    WriteLn('--- 4. escreve o original de volta');
    Check('MultiWrite do original = ioOk', WriteWhole(Saved)=ioOk);
    for n:=10 to 13 do begin
      i := (n-1)*4;
      Check('Var'+IntToStr(n)+' voltou ao original',
            MemberValue(n)=((Trunc(Saved[i]) shl 24) or (Trunc(Saved[i+1]) shl 16) or (Trunc(Saved[i+2]) shl 8) or Trunc(Saved[i+3])));
    end;

    WriteLn;
    if Fails=0 then WriteLn('=== TUDO OK ===') else WriteLn('=== ', Fails, ' FALHA(S) ===');
  finally
    Drv.Free;
    Port.Free;
    H.Free;
  end;
  if Fails<>0 then Halt(1);
end.
