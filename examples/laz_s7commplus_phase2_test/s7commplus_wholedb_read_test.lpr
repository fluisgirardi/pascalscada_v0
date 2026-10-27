program s7commplus_wholedb_read_test;

{: Leitura de um DB inteiro pelo nome (Path sem ponto, ex. "DB4"), conferida membro a membro.

   O driver monta o DB lendo cada membro de primeiro nivel e colando os bytes no offset
   classico dele (ByteOffset). Aqui o DB e' lido uma vez pelo nome e, depois, cada membro de
   primeiro nivel e' lido sozinho (BrowseDB + ReadSymbolic) e comparado com os bytes que
   ficaram no mesmo offset do DB inteiro. Membros de tamanho conhecido que mudam sozinhos
   (programa do CLP) sao lidos de novo algumas vezes antes de contar como divergencia.
   BOOL e STRING/structs nao sao comparados byte a byte (BOOL divide o byte; o conteudo de uma
   struct segue o layout real, nao o classico) e aparecem como "pulado".

   Uso: s7commplus_wholedb_read_test <ip-do-clp> [DB] [-u usuario -p senha] [-v]
   Exemplo: s7commplus_wholedb_read_test 192.168.56.2 DB5
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Tag, ProtocolTypes, tcp_udpport, S7PlusFamily, S7PlusTypeInfo, S7PlusTypes;

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

const
  MaxTries = 5;

var
  Port:TTCP_UDPPort;
  Drv:TS7CommPlusDriver;
  H:THost;
  DbName, User, Pass:String;
  DbNumber:Integer;
  Whole:TArrayOfDouble;
  Vars:TS7PlusVarInfoArray;
  Member:TBytes;
  tr:TTagRec;
  i, j, k, W, Ok, Bad, Skipped, Tries, Dot:Integer;
  Same:Boolean;
  Rest, Numeric:String;

function ReadWhole:Boolean;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.Path := DbName;
  tr.CallBack := @H.Told;
  H.Last := ioNone;
  Drv.Read(tr);
  Whole := H.Vals;
  Result := H.Last=ioOk;
end;

function SameBytes(Offset:Integer; const Data:TBytes):Boolean;
var
  n:Integer;
begin
  Result := (Offset>=0) and (Offset+Length(Data)<=Length(Whole));
  if not Result then exit;
  for n:=0 to High(Data) do
    if Trunc(Whole[Offset+n])<>Data[n] then begin
      Result := false;
      exit;
    end;
end;

function Hex(const Data:TBytes; Max:Integer):String;
var
  n:Integer;
begin
  Result := '';
  for n:=0 to High(Data) do begin
    if n>=Max then begin Result := Result+'...'; break; end;
    Result := Result+IntToHex(Data[n], 2)+' ';
  end;
end;

begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> [DB] [-u usuario -p senha] [-v]');
    Halt(1);
  end;
  DbName := 'DB4';
  User := '';
  Pass := '';
  H := THost.Create;
  i := 2;
  while i<=ParamCount do begin
    if ParamStr(i)='-v' then H.Verbose := true
    else if (ParamStr(i)='-u') and (i<ParamCount) then begin Inc(i); User := ParamStr(i); end
    else if (ParamStr(i)='-p') and (i<ParamCount) then begin Inc(i); Pass := ParamStr(i); end
    else DbName := ParamStr(i);
    Inc(i);
  end;

  Port := TTCP_UDPPort.Create(nil);
  Drv := TS7CommPlusDriver.Create(nil);
  Ok := 0; Bad := 0; Skipped := 0;
  try
    Port.Host := ParamStr(1);
    Port.Port := 102;
    Port.Timeout := 8000;
    Port.EnableAutoReconnect := false;
    Drv.CommunicationPort := Port;
    Drv.OnDebug := @H.OnDebug;
    if User<>'' then begin
      Drv.Username := User;
      Drv.Password := Pass;
    end;
    Port.Active := true;
    W := 0;
    while (not Port.ReallyActive) and (W<5000) do begin Sleep(100); Inc(W, 100); end;
    if not Port.ReallyActive then begin
      WriteLn('Falha ao abrir a porta TCP (timeout).');
      Halt(2);
    end;

    //numero do DB: "DB4" -> 4; um nome simbolico vem de GetDBNumberByName (ListDataBlocks)
    Numeric := UpperCase(DbName);
    if (Copy(Numeric, 1, 2)='DB') and (StrToIntDef(Copy(Numeric, 3, 10), -1)>=0) then
      DbNumber := StrToInt(Copy(Numeric, 3, 10))
    else
      DbNumber := -1;

    WriteLn('--- leitura de "', DbName, '" pelo nome');
    if not ReadWhole then begin
      WriteLn('leitura do DB inteiro falhou (', Ord(H.Last), ')');
      Halt(2);
    end;
    WriteLn('  ', Length(Whole), ' bytes');

    if DbNumber<0 then begin
      WriteLn('Para conferir os membros passe o DB como DB<numero>.');
      Halt(0);
    end;
    if Drv.BrowseDB(DbNumber, Vars)<>ioOk then begin
      WriteLn('BrowseDB(', DbNumber, ') falhou');
      Halt(2);
    end;

    WriteLn('--- membros de primeiro nivel x bytes do DB inteiro');
    for j:=0 to High(Vars) do begin
      Dot := Pos('.', Vars[j].Name);
      Rest := Copy(Vars[j].Name, Dot+1, Length(Vars[j].Name));
      //so o primeiro nivel: sem outro ponto nem indice de array no resto do nome
      if (Dot=0) or (Pos('.', Rest)>0) or (Pos('[', Rest)>0) then Continue;
      if Vars[j].ByteOffset<0 then Continue;

      //BOOL (divide o byte), STRING/WSTRING e structs (layout real) nao sao comparados
      if (TS7PlusSoftDataType(Vars[j].SoftDataType) in [sdtBOOL, sdtSTRING, sdtWSTRING, sdtSTRUCT, sdtARRAY]) then begin
        WriteLn(Format('  pulado %-28s (tipo %d, byte %d)', [Rest, Vars[j].SoftDataType, Vars[j].ByteOffset]));
        Inc(Skipped);
        Continue;
      end;

      Same := false;
      for Tries:=1 to MaxTries do begin
        if Drv.ReadSymbolic(S7PlusIds_DBAccessAreaBase+Cardinal(DbNumber), Vars[j].Lids, Member)<>ioOk then Break;
        Same := SameBytes(Vars[j].ByteOffset, Member);
        if Same then Break;
        //o valor pode ter mudado entre as duas leituras: le o DB inteiro de novo
        if not ReadWhole then Break;
      end;
      if Same then begin
        Inc(Ok);
        WriteLn(Format('  ok     %-28s byte %4d  %3d bytes  %s', [Rest, Vars[j].ByteOffset, Length(Member), Hex(Member, 8)]));
      end else begin
        Inc(Bad);
        k := Vars[j].ByteOffset;
        WriteLn(Format('  DIVERGE %-27s byte %4d  membro: %s', [Rest, k, Hex(Member, 8)]));
      end;
    end;

    WriteLn;
    WriteLn(Format('=== %d conferem, %d divergem, %d pulados ===', [Ok, Bad, Skipped]));
  finally
    Drv.Free;
    Port.Free;
    H.Free;
  end;
  if Bad<>0 then Halt(1);
end.
