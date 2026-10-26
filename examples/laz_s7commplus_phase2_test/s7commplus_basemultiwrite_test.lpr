program s7commplus_basemultiwrite_test;

{: Escrita em lote pela API da BASE (TProtocolDriver.MultiWrite / ScanMultiWrite), com o
   override do S7CommPlus (DoMultiWrite) reunindo as escritas simbolicas numa requisicao.

   Escreve DB4.Var10..Var13 (DInt) e DB4.Var26.v1 (Byte), mais um item com Path que nao
   existe (DB4.NaoExiste), nas duas formas:
     1. sincrona  (MultiWrite)     - callbacks tcWrite
     2. assincrona (ScanMultiWrite) - callbacks tcScanWrite, via a thread de scan
   e confere cada valor lendo de volta (Driver.Read). Cada item deve ter o seu resultado:
   os cinco bons ioOk, o inexistente ioCommError.

   Uso: s7commplus_basemultiwrite_test <ip-do-clp>
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, ProtocolTypes, tcp_udpport, S7PlusFamily;

const
  Paths:array[0..5] of String = ('DB4.Var10','DB4.Var11','DB4.Var12','DB4.Var13','DB4.Var26.v1','DB4.NaoExiste');
  SizesInBytes:array[0..5] of Integer = (4,4,4,4,1,4);

type
  THost = class
    Verbose:Boolean;
    Callbacks:Integer;
    Results:array[0..5] of TProtocolIOResult;
    Command:TTagCommand;
    LastRead:TArrayOfDouble;
    procedure OnDebug(Sender:TObject; const Msg:String);
    procedure Told(const ReqID:LongWord; Values:TArrayOfDouble; ValuesTimeStamp:QWord;
                   TagCommand:TTagCommand; LastResult:TProtocolIOResult; OffSet:LongInt);
    procedure ReadTold(const ReqID:LongWord; Values:TArrayOfDouble; ValuesTimeStamp:QWord;
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
  //callbacks come in the order the items were given
  if Callbacks<=High(Results) then
    Results[Callbacks] := LastResult;
  Command := TagCommand;
  Inc(Callbacks);
end;

procedure THost.ReadTold(const ReqID:LongWord; Values:TArrayOfDouble; ValuesTimeStamp:QWord;
  TagCommand:TTagCommand; LastResult:TProtocolIOResult; OffSet:LongInt);
begin
  LastRead := Values;
end;

var
  Port:TTCP_UDPPort;
  Driver:TS7CommPlusDriver;
  Host:THost;
  HostName:String;
  W:Integer;

function NewBatch(Token:Integer; ByCallback:TTagCommandCallBack; out Wanted:TArrayOfDouble):TMultiWriteArray;
var
  i, b:Integer;
begin
  SetLength(Result, Length(Paths));
  SetLength(Wanted, Length(Paths));
  for i:=0 to High(Paths) do begin
    FillChar(Result[i], SizeOf(Result[i]), 0);
    Result[i].Tag.Path := Paths[i];
    Result[i].Tag.Size := SizesInBytes[i];
    Result[i].Tag.CallBack := ByCallback;
    SetLength(Result[i].Values, SizesInBytes[i]);
    for b:=0 to SizesInBytes[i]-1 do
      Result[i].Values[b] := 0;
    //distinctive value: the low two bytes carry Token+i
    Result[i].Values[SizesInBytes[i]-1] := (Token+i*7) and $7F;
    if SizesInBytes[i]>1 then
      Result[i].Values[SizesInBytes[i]-2] := (Token+i*7) shr 7;
    Wanted[i] := Result[i].Values[SizesInBytes[i]-1];
  end;
end;

function IfThen_(C:Boolean; const A, B:String):String;
begin
  if C then Result := A else Result := B;
end;

function Name(R:TProtocolIOResult):String;
begin
  if R=ioOk then Result := 'ioOk'
  else if R=ioCommError then Result := 'ioCommError'
  else Result := 'io#'+IntToStr(Ord(R));
end;

function RunCase(const Title:String; Async:Boolean; Token:Integer):Integer;
var
  Batch:TMultiWriteArray;
  Wanted:TArrayOfDouble;
  i, b, Problems:Integer;
  tr:TTagRec;
  Read_:TProtocolIOResult;
  Match:Boolean;
  Started:QWord;
begin
  WriteLn('=== ', Title, ' ===');
  Problems := 0;
  Host.Callbacks := 0;
  for i:=0 to High(Host.Results) do Host.Results[i] := ioNone;
  Batch := NewBatch(Token, @Host.Told, Wanted);

  if Async then begin
    WriteLn('  ScanMultiWrite devolveu o pedido #', Driver.ScanMultiWrite(Batch));
    Started := GetTickCount64;
    while (Host.Callbacks<Length(Paths)) and (GetTickCount64-Started<8000) do begin
      CheckSynchronize(5);
      Sleep(2);
    end;
  end else
    Driver.MultiWrite(Batch);

  WriteLn(Format('  callbacks recebidos: %d de %d  (comando: %s)',
    [Host.Callbacks, Length(Paths), IfThen_(Host.Command=tcScanWrite, 'tcScanWrite', 'tcWrite')]));
  if Host.Callbacks<>Length(Paths) then Inc(Problems);

  for i:=0 to High(Paths) do begin
    if (Paths[i]='DB4.NaoExiste') then begin
      if Host.Results[i]=ioCommError then
        WriteLn(Format('  %-14s %s  (esperado: recusado)', [Paths[i], Name(Host.Results[i])]))
      else begin
        WriteLn(Format('  %-14s %s  <-- ESPERAVA ioCommError', [Paths[i], Name(Host.Results[i])]));
        Inc(Problems);
      end;
      continue;
    end;

    FillChar(tr, SizeOf(tr), 0);
    tr.Path := Paths[i];
    tr.Size := SizesInBytes[i];
    tr.CallBack := @Host.ReadTold;
    SetLength(Host.LastRead, 0);
    Driver.Read(tr);
    Match := (Length(Host.LastRead)=SizesInBytes[i]);
    if Match then
      for b:=0 to SizesInBytes[i]-1 do
        if Host.LastRead[b]<>Batch[i].Values[b] then Match := false;
    if (Host.Results[i]=ioOk) and Match then
      WriteLn(Format('  %-14s ioOk   leitura de volta confere', [Paths[i]]))
    else begin
      WriteLn(Format('  %-14s %s   %s  <-- PROBLEMA', [Paths[i], Name(Host.Results[i]), IfThen_(Match, 'confere', 'NAO confere')]));
      Inc(Problems);
    end;
  end;
  Result := Problems;
end;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp>');
    Halt(1);
  end;
  HostName := ParamStr(1);

  Port := TTCP_UDPPort.Create(nil);
  Driver := TS7CommPlusDriver.Create(nil);
  Host := THost.Create;
  try
    Port.Host := HostName;
    Port.Port := 102;
    Port.Timeout := 8000;
    Port.EnableAutoReconnect := false;
    Driver.CommunicationPort := Port;
    Host.Verbose := (ParamCount>=2) and (ParamStr(2)='-v');
    Driver.OnDebug := @Host.OnDebug;

    Port.Active := true;
    W := 0;
    while (not Port.ReallyActive) and (W<5000) do begin
      Sleep(100);
      Inc(W, 100);
    end;
    if not Port.ReallyActive then begin
      WriteLn('Falha ao abrir a porta TCP (timeout).');
      Halt(2);
    end;
    WriteLn('Porta TCP aberta.');

    W := RunCase('1. MultiWrite (sincrono)', false, Integer(GetTickCount64 mod 600));
    W := W + RunCase('2. MultiWrite de novo na mesma sessao (sincrono)', false, Integer(GetTickCount64 mod 600)+1);
    W := W + RunCase('3. ScanMultiWrite (assincrono, pela thread de scan)', true, Integer(GetTickCount64 mod 600)+3);

    if W=0 then
      WriteLn('=== OK: lote pela API da base - um resultado por item, escrita conferida ===')
    else
      WriteLn('=== FALHA: ', W, ' problema(s) ===');
  finally
    Driver.Free;
    Port.Free;
    Host.Free;
  end;
end.
