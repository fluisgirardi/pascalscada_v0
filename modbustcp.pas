{:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  @abstract(Implementa o driver ModBus TCP.)
}
unit ModBusTCP;

{$IFDEF FPC}
{$mode delphi}
{$IFDEF DEBUG}
  {$DEFINE FDEBUG}
{$ENDIF}
{$ENDIF}

interface

uses
  ModBusDriver, ProtocolTypes, commtypes;

type
  {:
  @abstract(Classe driver ModBus para TCP/IP.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  @bold(Para informações de como endereçar seus tags veja a classe TModBusDriver.)

  @seealso(TModBusDriver)
  }
  TModBusTCPDriver = class(TModBusDriver)
  protected
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES; override;
    function  DecodePkg(pkg:TIOPacket; var values:TArrayOfDouble):TProtocolIOResult; override;
  published
    property ReadSomethingAlways;
    property OutputMaxHole;
    property InputMaxHole;
    property RegisterMaxHole;
  end;

implementation

uses Math, hsutils, PLCMemoryManager, SysUtils{$IFDEF FDEBUG}, LCLProc{$ENDIF};

function  TModBusTCPDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES;
var
  i, c, c2, size:Integer;
begin
  //checa se é um pacote de escrita de valores ou de leitura
  //que está sendo codificado.

  //de leitura de valores...
  if ToWrite=nil then begin
    case TagObj.ReadFunction of
      $01,$02,$03,$04: begin
        //codifica pedido de leitura de entradas, saidas,
        //bloco de registradores e registrador simples.
        SetLength(Result,12);
        Result[00] := 0;
        Result[01] := 0;
        Result[02] := 0;
        Result[03] := 0;
        Result[04] := 0; //tamanho restante da mensagem, bHi
        Result[05] := 6; //tamanho restante da mensagem, bLo
        Result[06] := TagObj.Station and $FF;
        Result[07] := TagObj.ReadFunction and $FF;
        Result[08] := (TagObj.Address and $FF00) shr 8;
        Result[09] := TagObj.Address and $FF;
        Result[10] := (TagObj.Size and $FF00) shr 8;
        Result[11] := TagObj.Size and $FF;
      end;

      $07: begin
        // Lê o Status
        SetLength(Result,8);
        Result[00] := 0;
        Result[01] := 0;
        Result[02] := 0;
        Result[03] := 0;
        Result[04] := 0; //tamanho restante da mensagem, bHi
        Result[05] := 2; //tamanho restante da mensagem, bLo
        Result[06] := TagObj.Station and $FF;
        Result[07] := $07;
      end;

      $08: begin
        // Teste de Linha...
        SetLength(Result,12);
        Result[00] := 0;
        Result[01] := 0;
        Result[02] := 0;
        Result[03] := 0;
        Result[04] := 0; //tamanho restante da mensagem, bHi
        Result[05] := 6; //tamanho restante da mensagem, bLo
        Result[06] := TagObj.Station and $FF;
        Result[07] := $08;
        Result[08] := 0;
        Result[09] := 0;
        Result[10] := 0;
        Result[11] := 0;
      end;
      else begin
        SetLength(Result,0);
      end;
    end;

    // Calcula o tamanho do pacote resposta
    case TagObj.ReadFunction of
      $01..$02:
        ResultLen := 9 +(TagObj.Size div 8)+IfThen((TagObj.Size mod 8)<>0,1,0);
      $03..$04:
        ResultLen := 9+(TagObj.Size*2);
      $07:
        ResultLen := 9;
      $08:
        ResultLen := 12;
      else
      begin
        ResultLen := 0;
      end;
    end;
  end else begin
    case TagObj.WriteFunction of
      $05: begin
        //escreve uma saida...
        SetLength(Result,12);
        Result[00] := 0;
        Result[01] := 0;
        Result[02] := 0;
        Result[03] := 0;
        Result[04] := 0; //tamanho restante da mensagem, bHi
        Result[05] := 6; //tamanho restante da mensagem, bLo
        Result[06] := TagObj.Station and $FF;
        Result[07] := $05;
        Result[08] := (TagObj.Address and $FF00) shr 8;
        Result[09] := TagObj.Address and $FF;

        if (ToWrite[0]=0) then begin
           Result[10] := $00;
           Result[11] := $00;
        end else begin
           Result[10] := $FF;
           Result[11] := $00;
        end;
      end;

      $06: begin
        // escreve 1 registro
        SetLength(Result,12);
        Result[00] := 0;
        Result[01] := 0;
        Result[02] := 0;
        Result[03] := 0;
        Result[04] := 0; //tamanho restante da mensagem, bHi
        Result[05] := 6; //tamanho restante da mensagem, bLo
        Result[06] := TagObj.Station and $FF;
        Result[07] := $06;
        Result[08] := (TagObj.Address and $FF00) shr 8;
        Result[09] := TagObj.Address and $FF;
        Result[10] := (FloatToInteger(ToWrite[0]) and $FF00) shr 8;
        Result[11] := FloatToInteger(ToWrite[0]) and $FF;
      end;

      $0F: begin
        //Num de saidas em bytes + 9 bytes fixos.
        size:=(TagObj.Size div 8)+IfThen((TagObj.Size mod 8)>0,1,0);
        SetLength(Result,size+13); //13 = end+func+address 2b,num bits 2b, pkg len 1b + 6 modbus tcp

        Result[00] := 0;
        Result[01] := 0;
        Result[02] := 0;
        Result[03] := 0;
        Result[04] := ((size + 7) and $FF00) div $100; //tamanho restante da mensagem, bHi
        Result[05] := ((size + 7) and $FF);            //tamanho restante da mensagem, bLo
        Result[06] := TagObj.Station and $FF;
        Result[07] := $0F;
        Result[08] := ((TagObj.Address+TagObj.OffSet) and $FF00) shr 8;
        Result[09] := (TagObj.Address+TagObj.OffSet) and $FF;
        Result[10] := (Min(TagObj.Size,Length(ToWrite)) and $FF00) shr 8;
        Result[11] := Min(TagObj.Size,Length(ToWrite)) and $FF;
        Result[12] := (TagObj.Size div 8)+IfThen((TagObj.Size mod 8)>0,1,0);

        i := 0;
        c := 0;
        c2:= 13;
        Result[13] := 0;

        for c := 0 to Min(TagObj.Size,Length(ToWrite))-1 do begin
          if ToWrite[c]<>0 then begin
            Result[c2] := Result[c2]+ (1 shl i);
          end;

          inc(i);
          if i>7 then begin
            i:=0;
            inc(c2);
            Result[c2] := 0;
          end;
        end;
      end;

      $10: begin
        // Escreve X bytes
        size := (TagObj.Size*2);
        SetLength(Result,size+13); //13 = end+func+address 2b,num bits 2b, pkg len 1b + 6 modbus tcp

        Result[00] := 0;
        Result[01] := 0;
        Result[02] := 0;
        Result[03] := 0;
        Result[04] := ((size + 7) and $FF00) div $100; //tamanho restante da mensagem, bHi
        Result[05] := ((size + 7) and $FF);            //tamanho restante da mensagem, bLo

        Result[06] := TagObj.Station and $FF;
        Result[07] := $10;
        Result[08] := ((TagObj.Address+TagObj.OffSet) and $FF00) shr 8;
        Result[09] := (TagObj.Address+TagObj.OffSet) and $FF;
        Result[10] := ((TagObj.Size and $FF00) shr 8);
        Result[11] := TagObj.Size and $FF;
        Result[12] := (TagObj.Size*2) and $FF;
        i := 0;
        while (i<TagObj.Size) do begin
            Result[13+i*2] := ((FloatToInteger(ToWrite[i]) and $FF00) shr 8);
            Result[14+i*2] := FloatToInteger(ToWrite[i]) and $FF;
            inc(i);
        end;
      end;
      else begin
        SetLength(Result,0);
      end;
    end;
    // Calcula o tamanho do pacote resposta
    case TagObj.WriteFunction of
      $05,$06,$0F,$10:
        ResultLen := 12;
      else
      begin
        ResultLen := 0;
      end;
    end;
  end;
end;

function TModBusTCPDriver.DecodePkg(pkg:TIOPacket; var values:TArrayOfDouble):TProtocolIOResult;
var
   i,c,c2,plc:integer;
   address, len:Cardinal;
   foundPLC:Boolean;
   aux:TPLCMemoryManager;
   {$IFDEF FDEBUG}
   debug:string;
   {$ENDIF}
begin
  //se algumas das IOs falhou,
  Result:=ioOk;
  case pkg.WriteIOResult of
    iorTimeOut:
      Result:=ioTimeOut;
    iorNotReady,
    iorNone:
      Result:=ioDriverError;
    iorPortError:
      Result := ioCommError;
  end;

  if (Result<>ioOk)then
    case pkg.ReadIOResult of
      iorTimeOut:
        Result:=ioTimeOut;
      iorNotReady,
      iorNone:
        Result:=ioDriverError;
      iorPortError:
        Result := ioCommError;
    end;
  
  //se o endereco retornado nao conferem com o selecionado...
  if (Result=ioOk) AND (pkg.BufferToWrite[6]<>pkg.BufferToRead[6]) then begin
    {$IFDEF FDEBUG}
    debug := '';
    for c := 0 to High(pkg.BufferToRead) do
      debug := debug + IntToHex(pkg.BufferToRead[c], 2);
    DebugLn('Pacotes diferem no endereço. Hex do pacote recebido:');
    DebugLn(debug);
    {$ENDIF}    
    Result := ioCommError;
  end;

  //procura o plc
  foundPLC := false;
  for plc := 0 to High(PModbusPLC) do
    if PModbusPLC[plc].Station = pkg.BufferToWrite[6] then begin
      foundPLC := true;
      break;
    end;

  //comeca a decodificar o pacote...

  //leitura de bits das entradas ou saidas
  case pkg.BufferToRead[7] of
    $01,$02: begin
      //acerta onde vao ser colocados os valores decodificados...
      if foundPLC then begin
        if pkg.BufferToWrite[7]=$01 then
          aux := PModbusPLC[plc].OutPuts
        else
          aux := PModbusPLC[plc].Inputs;
      end;

      if Result=ioOk then begin
        address := (pkg.BufferToWrite[08] shl 8) + pkg.BufferToWrite[09];
        len     := (pkg.BufferToWrite[10] shl 8) + pkg.BufferToWrite[11];
        SetLength(Values,len);

        i := 0;
        c := 0;
        c2:= 9;
        while (i<len) and (c2<Length(pkg.BufferToRead)) do begin
          if (c=8) then begin
            c:=0;
            inc(c2);
          end;
          Values[i]:=IfThen(((Integer(pkg.BufferToRead[c2]) and (1 shl c))=(1 shl c)),1,0);
          inc(i);
          inc(c);
        end;
        if foundPLC then
          aux.SetValues(address,len,1,Values,Result);
      end else
        if foundPLC then
          aux.SetFault(address,len,1,Result);
    end;

    //leitura de words dos registradores ou das entradas analogicas
    $03,$04: begin
      //acerta onde vao ser colocados os valores decodificados...
      if foundPLC then begin
        if pkg.BufferToWrite[7]=$03 then
          aux := PModbusPLC[plc].Registers
        else
          aux := PModbusPLC[plc].AnalogReg;
      end;

      if Result=ioOk then begin
        address := (pkg.BufferToWrite[08] shl 8) + pkg.BufferToWrite[09];
        len     := (pkg.BufferToWrite[10] shl 8) + pkg.BufferToWrite[11];
        SetLength(Values,len);

        // data are ok
        for i:=0 to Len-1 do begin
          Values[i]:=(Integer(pkg.BufferToRead[9+(i*2)]) shl 8) + Integer(pkg.BufferToRead[10+i*2]);
        end;

        if foundPLC then
          aux.SetValues(address,len,1,Values,Result);
      end else
        if foundPLC then
          aux.SetFault(address,len,1,Result);
    end;

    $05: begin
      if Result=ioOk then begin
        address := (pkg.BufferToWrite[8] * 256) + pkg.BufferToWrite[9];
        SetLength(values,1);

        if (pkg.BufferToWrite[10]=0) and (pkg.BufferToWrite[11]=0) then
           values[0] := 0
        else
           values[0] := 1;

        if foundPLC then
          PModbusPLC[plc].OutPuts.SetValues(address,1,1,values,Result);
      end else
        if foundPLC then
          PModbusPLC[plc].OutPuts.SetFault(address,1,1,Result);
    end;
    $06: begin
      if Result=ioOk then begin
        address := (pkg.BufferToWrite[8] * 256) + pkg.BufferToWrite[9];
        SetLength(values,1);

        values[0] := pkg.BufferToWrite[10]*256+pkg.BufferToWrite[11];

        if foundPLC then
          PModbusPLC[plc].Registers.SetValues(address,1,1,values,Result);
      end else
        if foundPLC then
          PModbusPLC[plc].Registers.SetFault(address,1,1,Result);
    end;
    $07: begin
      if foundPLC then begin
        if Result=ioOk then begin
          PModbusPLC[plc].Status07Value :=Integer(pkg.BufferToRead[8]);
          PModbusPLC[plc].Status07TimeStamp := Now;
        end;
        PModbusPLC[plc].Status07LastError := Result;
      end;
    end;
    $0F: begin
      if Result=ioOk then begin
        address := (pkg.BufferToWrite[08] * 256) + pkg.BufferToWrite[09];
        len     := (pkg.BufferToWrite[10] * 256) + pkg.BufferToWrite[11];

        SetLength(values,len);

        i := 0;
        c := 0;
        c2:= 13;
        while (i<len) and (c2<Length(pkg.BufferToRead)) do begin
          if (c=8) then begin
            c:=0;
            inc(c2);
          end;
          Values[i]:=IfThen(((Integer(pkg.BufferToWrite[c2]) and (1 shl c))=(1 shl c)),1,0);
          inc(i);
          inc(c);
        end;

        if foundPLC then
          PModbusPLC[plc].OutPuts.SetValues(address,len,1,values,Result);
      end else
        if foundPLC then
          PModbusPLC[plc].OutPuts.SetFault(address,len,1,Result);
    end;
    $10: begin
      if Result=ioOk then begin
        address := (pkg.BufferToWrite[08] * 256) + pkg.BufferToWrite[09];
        len     := (pkg.BufferToWrite[10] * 256) + pkg.BufferToWrite[11];

        SetLength(values,len);

        i := 0;
        while (i<len) do begin
           values[i] := pkg.BufferToWrite[13+i*2]*256 + pkg.BufferToWrite[14+i*2];
           inc(i);
        end;

        if foundPLC then
          PModbusPLC[plc].Registers.SetValues(address,len,1,values,Result);
      end else
        if foundPLC then
          PModbusPLC[plc].Registers.SetFault(address,len,1,Result);
    end;
    else begin
      case pkg.BufferToRead[7] of
        $81:
          Result := ioIllegalFunction;
        $82:
          Result := ioIllegalRegAddress;
        $83:
          Result := ioIllegalValue;
        $84,$85,$86,$87,$88:
          Result := ioPLCError;
        else
          Result := ioCommError;
      end;

      address := (pkg.BufferToWrite[08] shl 8) + pkg.BufferToWrite[09];
      len     := (pkg.BufferToWrite[10] shl 8) + pkg.BufferToWrite[11];

      case pkg.BufferToWrite[7] of
        $01: begin
          if foundPLC then
            PModbusPLC[plc].OutPuts.SetFault(address,len,1,Result);
        end;
        $02: begin
          if foundPLC then
            PModbusPLC[plc].Inputs.SetFault(address,len,1,Result);
        end;
        $03: begin
          if foundPLC then
            PModbusPLC[plc].Registers.SetFault(address,len,1,Result);
        end;
        $04: begin
          if foundPLC then
            PModbusPLC[plc].AnalogReg.SetFault(address,len,1,Result);
        end;
      end;
    end;
  end;
end;

end.
