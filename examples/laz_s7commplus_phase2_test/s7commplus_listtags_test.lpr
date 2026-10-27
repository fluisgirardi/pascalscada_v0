program s7commplus_listtags_test;

{: Lista todos os tags do CLP pelo metodo COMUM da base, TProtocolDriver.ListTags - o mesmo
   codigo serve para qualquer driver que saiba listar (hoje o S7CommPlus e o Rockwell): a
   variavel e' um TProtocolDriver, nao um TS7CommPlusDriver.

   Uso: s7commplus_listtags_test <ip-do-clp> [filtro]
   Sem filtro mostra so um resumo e os DBs; com filtro (texto contido no Path), cada tag.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, TypInfo,
  Tag, ProtocolTypes, ProtocolDriver, tcp_udpport, S7PlusFamily;

const
  KindNames:array[TPLCTagKind] of String = ('scalar', 'array', 'structure', 'string', 'unsupported');

var
  Port:TTCP_UDPPort;
  Driver:TProtocolDriver;     //any driver: only the base class is used from here on
  Tags:TPLCTagInfoArray;
  Counts:array[TPLCTagKind] of Integer;
  Res:TProtocolIOResult;
  Filter:String;
  i, W:Integer;
begin
  if ParamCount<1 then begin
    WriteLn('Uso: ', ExtractFileName(ParamStr(0)), ' <ip-do-clp> [filtro]');
    Halt(1);
  end;
  Filter := LowerCase(ParamStr(2));

  Port := TTCP_UDPPort.Create(nil);
  Driver := TS7CommPlusDriver.Create(nil);
  try
    Port.Host := ParamStr(1);
    Port.Port := 102;
    Port.Timeout := 8000;
    Port.EnableAutoReconnect := false;
    Driver.CommunicationPort := Port;
    Port.Active := true;
    W := 0;
    while (not Port.ReallyActive) and (W<5000) do begin
      Sleep(100);
      Inc(W, 100);
    end;

    if not Driver.SupportsListTags then begin
      WriteLn('Este driver nao sabe listar os tags.');
      Halt(2);
    end;

    Res := Driver.ListTags(Tags);
    WriteLn('ListTags: resultado=', Ord(Res), ' (1=ok, 19=parcial)   tags=', Length(Tags));
    if Length(Tags)=0 then Halt(3);

    FillChar(Counts, SizeOf(Counts), 0);
    for i:=0 to High(Tags) do
      Inc(Counts[Tags[i].Kind]);
    WriteLn(Format('  scalar=%d array=%d structure=%d string=%d unsupported=%d',
                   [Counts[tkScalar], Counts[tkArray], Counts[tkStructure], Counts[tkString], Counts[tkUnsupported]]));
    WriteLn;

    for i:=0 to High(Tags) do begin
      if Filter='' then begin
        //only the DBs and the structures
        if Tags[i].Kind<>tkStructure then continue;
      end else if Pos(Filter, LowerCase(Tags[i].Path))=0 then
        continue;
      WriteLn(Format('%-44s %-10s %-12s %-7s n=%-4d %5d bytes  grupo=%s',
                     [Tags[i].Path, KindNames[Tags[i].Kind], Tags[i].NativeType,
                      GetEnumName(TypeInfo(TTagType), Ord(Tags[i].TagType)),
                      Tags[i].ElementCount, Tags[i].SizeInBytes, Tags[i].Group]));
    end;
  finally
    Driver.Free;
    Port.Free;
  end;
end.
