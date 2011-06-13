{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementa o driver ModBus RTU.)
}
unit ModBusSerial;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  ModBusDriver, Tag, commtypes, crc16utils;

type
  {:
  @abstract(Classe driver ModBus RTU.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @bold(Para informações de como endereçar seus tags veja a classe TModBusDriver.)

  @seealso(TModBusDriver)
  }
  TModBusRTUDriver = class(TModBusDriver)
  protected
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES; override;
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; override;
  published
    property ReadSomethingAlways;
    property OutputMaxHole;
    property InputMaxHole;
    property RegisterMaxHole;
  end;

implementation

uses Math, PLCMemoryManager, SysUtils;

function  TModBusRTUDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:Integer):BYTES;
var
  i, c, c2:Integer;
begin
  //checa se é um pacote de escrita de valores ou de leitura
  //que está sendo codificado.

  //de leitura de valores...
  if ToWrite=nil then begin
    case TagObj.ReadFunction of
      $01,$02,$03,$04: begin
        //codifica pedido de leitura de entradas, saidas,
        //bloco de registradores e registrador simples.
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := TagObj.ReadFunction and $FF;
        Result[2] := (TagObj.Address and $FF00) shr 8;
        Result[3] := TagObj.Address and $FF;
        Result[4] := (TagObj.Size and $FF00) shr 8;
        Result[5] := TagObj.Size and $FF;
        // Calcula CRC
        Calcul_crc(Result);
      end;

      $07: begin
        // Lê o Status
        SetLength(Result,4);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $07;
        // Calcula o CRC
        Calcul_crc(Result);
      end;

      $08: begin
        // Teste de Linha...
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $08;
        Result[2] := 0;
        Result[3] := 0;
        Result[4] := 0;
        Result[5] := 0;
        Calcul_crc(Result);
      end;
      else begin
        SetLength(Result,0);
      end;
    end;

    // Calcula o tamanho do pacote resposta
    case TagObj.ReadFunction of
      $01..$02:
        ResultLen := 5 +(TagObj.Size div 8)+IfThen((TagObj.Size mod 8)<>0,1,0);
      $03..$04:
        ResultLen := 5+(TagObj.Size*2);
      $07:
        ResultLen := 5;
      $08:
        ResultLen := 8;
      else
      begin
        ResultLen := 0;
      end;
    end;
  end else begin
    case TagObj.WriteFunction of
      $05: begin
        //escreve uma saida...
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $05;
        Result[2] := (TagObj.Address and $FF00) shr 8;
        Result[3] := TagObj.Address and $FF;

        if (ToWrite[0]=0) then begin
           Result[4] := $00;
           Result[5] := $00;
        end else begin
           Result[4] := $FF;
           Result[5] := $00;
        end;
        // Calcula CRC
        Calcul_crc(Result);
      end;

      $06: begin
        // escreve 1 registro
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $06;
        Result[2] := (TagObj.Address and $FF00) shr 8;
        Result[3] := TagObj.Address and $FF;
        Result[4] := (Trunc(ToWrite[0]) and $FF00) shr 8;
        Result[5] := Trunc(ToWrite[0]) and $FF;
        // Calcula o CRC
        Calcul_crc(Result);
      end;

      $0F: begin
        //Num de saidas em bytes + 9 bytes fixos.
        SetLength(Result,(TagObj.Size div 8)+IfThen((TagObj.Size mod 8)>0,1,0)+9);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $0F;
        Result[2] := ((TagObj.Address+TagObj.OffSet) and $FF00) shr 8;
        Result[3] := (TagObj.Address+TagObj.OffSet) and $FF;
        Result[4] := (Min(TagObj.Size,Length(ToWrite)) and $FF00) shr 8;
        Result[5] := Min(TagObj.Size,Length(ToWrite)) and $FF;
        Result[6] := (TagObj.Size div 8)+IfThen((TagObj.Size mod 8)>0,1,0);

        i := 0;
        c := 0;
        c2:= 7;
        Result[7] := 0;

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

        // Calcula CRC
        Calcul_crc(Result);
      end;

      $10: begin
        // Escreve X bytes
        SetLength(Result,(TagObj.Size*2)+9);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $10;
        Result[2] := ((TagObj.Address+TagObj.OffSet) and $FF00) shr 8;
        Result[3] := (TagObj.Address+TagObj.OffSet) and $FF;
        Result[4] := ((TagObj.Size and $FF00) shr 8);
        Result[5] := TagObj.Size and $FF;
        Result[6] := (TagObj.Size*2) and $FF;
        i := 0;
        while (i<TagObj.Size) do begin
            Result[7+i*2] := ((Trunc(ToWrite[i]) and $FF00) shr 8);
            Result[8+i*2] := Trunc(ToWrite[i]) and $FF;
            inc(i);
        end;
        // Calcula o CRC
        Calcul_crc(Result);
      end;
      else begin
        SetLength(Result,0);
      end;
    end;
    // Calcula o tamanho do pacote resposta
    case TagObj.WriteFunction of
      $05,$06,$0F,$10:
        ResultLen := 8;
      else
      begin
        ResultLen := 0;
      end;
    end;
  end;
end;

function TModBusRTUDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
var
   i,c,c2,plc:integer;
   address, len:Cardinal;
   foundPLC:Boolean;
   aux:TPLCMemoryManager;
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

  if (Result=ioOk)then
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
  if (Result=ioOk) and (pkg.BufferToWrite[0]<>pkg.BufferToRead[0]) then begin
    Result := ioCommError;
  end;

  //se a checagem crc nao bate, sai
  if (Result=ioOk) and ((not Test_crc(pkg.BufferToWrite)) or (not Test_crc(pkg.BufferToRead))) then begin
    Result := ioCommError;
    exit;
  end;

  //procura o plc
  foundPLC := false;
  for plc := 0 to High(PModbusPLC) do
    if PModbusPLC[plc].Station = pkg.BufferToWrite[0] then begin
      foundPLC := true;
      break;
    end;

  //comeca a decodificar o pacote...

  //leitura de bits das entradas ou saidas
  case pkg.BufferToRead[1] of
    $01,$02: begin
      //acerta onde vao ser colocados os valores decodificados...
      if foundPLC then begin
        if pkg.BufferToWrite[1]=$01 then
          aux := PModbusPLC[plc].OutPuts
        else
          aux := PModbusPLC[plc].Inputs;
      end;

      if Result=ioOk then begin
        address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
        len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
        SetLength(Values,len);

        i := 0;
        c := 0;
        c2:= 3;
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
          aux.SetValues(address,len,1,Values, Result);
      end else
        if foundPLC then
          aux.SetFault(address,len,1,Result);
    end;

    //leitura de words dos registradores ou das entradas analogicas
    $03,$04: begin
      //acerta onde vao ser colocados os valores decodificados...
      if foundPLC then begin
        if pkg.BufferToWrite[1]=$03 then
          aux := PModbusPLC[plc].Registers
        else
          aux := PModbusPLC[plc].AnalogReg;
      end;

      if Result=ioOk then begin
        address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
        len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
        SetLength(Values,len);

        // data are ok
        for i:=0 to Len-1 do begin
          Values[i]:=(Integer(pkg.BufferToRead[3+(i*2)]) shl 8) + Integer(pkg.BufferToRead[4+i*2]);
        end;

        if foundPLC then
          aux.SetValues(address,len,1,Values, Result);
      end else
        if foundPLC then
          aux.SetFault(address,len,1,Result);
    end;

    $05: begin
      if Result=ioOk then begin
        address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
        SetLength(values,1);

        if (pkg.BufferToWrite[4]=0) and (pkg.BufferToWrite[5]=0) then
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
        address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
        SetLength(values,1);

        values[0] := pkg.BufferToWrite[4]*256+pkg.BufferToWrite[5];
        if foundPLC then
          PModbusPLC[plc].Registers.SetValues(address,1,1,values,Result);
      end else
        if foundPLC then
          PModbusPLC[plc].Registers.SetFault(address,1,1,Result)
    end;
    $07: begin
      if foundPLC then begin
         if Result=ioOk then begin
           PModbusPLC[plc].Status07Value :=Integer(pkg.BufferToRead[2]);
           PModbusPLC[plc].Status07TimeStamp := Now;
         end;
         PModbusPLC[plc].Status07LastError := Result;
      end;
    end;
    $0F: begin
      if Result=ioOk then begin
        address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
        len     := (pkg.BufferToWrite[4] * 256) + pkg.BufferToWrite[5];

        SetLength(values,len);

        i := 0;
        c := 0;
        c2:= 7;
        while i<len do begin
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
        address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
        len     := (pkg.BufferToWrite[4] * 256) + pkg.BufferToWrite[5];

        SetLength(values,len);

        i := 0;
        while (i<len) do begin
           values[i] := pkg.BufferToWrite[7+i*2]*256 + pkg.BufferToWrite[8+i*2];
           inc(i);
        end;
        if foundPLC then
          PModbusPLC[plc].Registers.SetValues(address,len,1,values,Result);
      end else
        if foundPLC then
          PModbusPLC[plc].Registers.SetFault(address,len,1,Result);
    end;
    else begin
      case pkg.BufferToRead[1] of
        $81:
          Result := ioIllegalFunction;
        $82:
          Result := ioIllegalRegAddress;
        $83:
          Result := ioIllegalValue;
        $84,$85,$86,$87,$88:
          Result := ioPLCError;
      end;

      address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
      len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];

      case pkg.BufferToWrite[1] of
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
