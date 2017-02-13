{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementa o driver ModBus RTU.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Unit that implements the ModBus RTU protocol driver.)
}
{$ENDIF}
unit ModBusSerial;

interface

uses
  Classes, ModBusDriver, Tag, commtypes, crc16utils;

type

  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe driver ModBus RTU.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @bold(Para informações de como endereçar seus tags veja a classe TModBusDriver.)

  @seealso(TModBusDriver)
  }
  {$ELSE}
  {:
  @abstract(Class of ModBus RTU protocol driver.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @bold(For more information, see the documentation of TModBusDriver class.)

  @seealso(TModBusDriver)
  }
  {$ENDIF}

  { TModBusRTUDriver }

  TModBusRTUDriver = class(TModBusDriver)
  protected
    function AllowBroadCast: Boolean; override;
    //:  @seealso(TModBusDriver.EncodePkg)
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES; override;
    //:  @seealso(TModBusDriver.DecodePkg)
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; override;
    //:  @seealso(TModBusDriver.RemainingBytes)
    function RemainingBytes(buffer: BYTES): LongInt; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    //:  @seealso(TModBusDriver.ReadSomethingAlways)
    property ReadSomethingAlways;
    //:  @seealso(TModBusDriver.OutputMaxHole)
    property OutputMaxHole;
    //:  @seealso(TModBusDriver.InputMaxHole)
    property InputMaxHole;
    //:  @seealso(TModBusDriver.RegisterMaxHole)
    property RegisterMaxHole;

    property ReadOnly;
  end;

implementation

uses Math, PLCMemoryManager, SysUtils, crossdatetime;

constructor TModBusRTUDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PFirstRequestLen:=3;
  PFuncByteOffset:=1;
  PCRCLen:=2;
end;

function TModBusRTUDriver.AllowBroadCast: Boolean;
begin
  Result:=true;
end;

function  TModBusRTUDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES;
var
  i, c, c2:LongInt;
begin
  //checa se é um pacote de escrita de valores ou de leitura
  //que está sendo codificado.
  //
  //Verify if is packet to write data on device that is being encoded.

  //de leitura de valores...
  //read data from slave.
  if ToWrite=nil then begin
    case TagObj.ReadFunction of
      $01,$02,$03,$04: begin
        //codifica pedido de leitura de entradas, saidas,
        //bloco de registradores e registrador simples.
        //
        //encode a packet to read input, outputs, register or analog registers
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := TagObj.ReadFunction and $FF;
        Result[2] := (TagObj.Address and $FF00) shr 8;
        Result[3] := TagObj.Address and $FF;
        Result[4] := (TagObj.Size and $FF00) shr 8;
        Result[5] := TagObj.Size and $FF;
        // Calcula CRC
        // computes the CRC
        Calcul_crc(Result);
      end;

      $07: begin
        // Lê o Status
        //encode a packet to read the device status.
        SetLength(Result,4);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $07;
        // Calcula o CRC
        // computes the CRC
        Calcul_crc(Result);
      end;

      $08: begin
        // Teste de Linha...
        // line test
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
    // computes the size of the incoming packet.
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
        //encodes a packet to write a single coil.
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
        // computes the CRC
        Calcul_crc(Result);
      end;

      $06: begin
        // escreve 1 registro
        // encodes a packet to write a single register.
        SetLength(Result,8);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $06;
        Result[2] := (TagObj.Address and $FF00) shr 8;
        Result[3] := TagObj.Address and $FF;
        Result[4] := (Trunc(ToWrite[0]) and $FF00) shr 8;
        Result[5] := Trunc(ToWrite[0]) and $FF;
        // Calcula o CRC
        // computes the CRC
        Calcul_crc(Result);
      end;

      $0F: begin
        //Num de saidas em bytes + 9 bytes fixos.
        //encodes a packet to write multiple coils.
        SetLength(Result,(TagObj.Size div 8)+IfThen((TagObj.Size mod 8)>0,1,0)+9);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $0F;
        Result[2] := ((TagObj.Address+TagObj.OffSet) and $FF00) shr 8;        //endereco
        Result[3] := (TagObj.Address+TagObj.OffSet) and $FF;                  //endereco
        Result[4] := (Min(TagObj.Size,Length(ToWrite)) and $FF00) shr 8;      //num de coils
        Result[5] := Min(TagObj.Size,Length(ToWrite)) and $FF;                //num de coils
        Result[6] := (TagObj.Size div 8)+IfThen((TagObj.Size mod 8)>0,1,0);   //num de bytes que seguem

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
        // computes the CRC
        Calcul_crc(Result);
      end;

      $10: begin
        // Escreve X bytes
        // encodes a packet to write multiple registers
        SetLength(Result,(TagObj.Size*2)+9);
        Result[0] := TagObj.Station and $FF;
        Result[1] := $10;
        Result[2] := ((TagObj.Address+TagObj.OffSet) and $FF00) shr 8; //endereco
        Result[3] := (TagObj.Address+TagObj.OffSet) and $FF;           //endereco
        Result[4] := ((TagObj.Size and $FF00) shr 8);                  //num de words
        Result[5] := TagObj.Size and $FF;                              //num de words
        Result[6] := (TagObj.Size*2) and $FF;                          //num de bytes que seguem = num de words * 2
        i := 0;
        while (i<TagObj.Size) do begin
            Result[7+i*2] := ((Trunc(ToWrite[i]) and $FF00) shr 8);
            Result[8+i*2] := Trunc(ToWrite[i]) and $FF;
            inc(i);
        end;
        // Calcula o CRC
        // computes the CRC
        Calcul_crc(Result);
      end;
      else begin
        SetLength(Result,0);
      end;
    end;
    // Calcula o tamanho do pacote resposta
    // computes the size of the incoming packet.
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
   i,c,c2,plc:LongInt;
   address, len:Cardinal;
   foundPLC:Boolean;
   aux:TPLCMemoryManager;
begin
  //se algumas das IOs falhou,
  //if some IO fail.
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

  if (Length(pkg.BufferToWrite)=0) or (Length(pkg.BufferToRead)=0) then
    Result:=ioDriverError;
  
  //se o endereco retornado nao conferem com o selecionado...
  //if the address in the incoming packet is different of the requested
  if (Result=ioOk) and (pkg.BufferToWrite[0]<>pkg.BufferToRead[0]) then begin
    Result := ioCommError;
  end;

  //se a checagem crc nao bate, sai
  //verify the CRC of incoming packet.
  if (Result=ioOk) and ((not Test_crc(pkg.BufferToWrite)) or (not Test_crc(pkg.BufferToRead))) then begin
    Result := ioCommError;
    exit;
  end;

  //procura o plc
  //search de PLC
  foundPLC := false;
  for plc := 0 to High(PModbusPLC) do
    if PModbusPLC[plc].Station = pkg.BufferToWrite[0] then begin
      foundPLC := true;
      break;
    end;

  //comeca a decodificar o pacote...
  //decodes the packet.

  //leitura de bits das entradas ou saidas
  //request to read the digital input/outpus (coils)
  case pkg.BufferToRead[1] of
    $01,$02: begin
      //acerta onde vao ser colocados os valores decodificados...
      //where the data decoded will be stored.
      if foundPLC then begin
        if pkg.BufferToWrite[1]=$01 then
          aux := PModbusPLC[plc].OutPuts
        else
          aux := PModbusPLC[plc].Inputs;
      end;

      address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
      len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
      if Result=ioOk then begin
        SetLength(Values,len);

        i := 0;
        c := 0;
        c2:= 3;
        while (i<len) and (c2<Length(pkg.BufferToRead)) do begin
          if (c=8) then begin
            c:=0;
            inc(c2);
          end;
          Values[i]:=IfThen(((LongInt(pkg.BufferToRead[c2]) and (1 shl c))=(1 shl c)),1,0);
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
    //request to read registers/analog registers
    $03,$04: begin
      //acerta onde vao ser colocados os valores decodificados...
      //where the data decoded will be stored.
      if foundPLC then begin
        if pkg.BufferToWrite[1]=$03 then
          aux := PModbusPLC[plc].Registers
        else
          aux := PModbusPLC[plc].AnalogReg;
      end;

      address := (pkg.BufferToWrite[2] shl 8) + pkg.BufferToWrite[3];
      len     := (pkg.BufferToWrite[4] shl 8) + pkg.BufferToWrite[5];
      if Result=ioOk then begin
        SetLength(Values,len);

        // data are ok
        for i:=0 to Len-1 do begin
          Values[i]:=(LongInt(pkg.BufferToRead[3+(i*2)]) shl 8) + LongInt(pkg.BufferToRead[4+i*2]);
        end;

        if foundPLC then
          aux.SetValues(address,len,1,Values, Result);
      end else
        if foundPLC then
          aux.SetFault(address,len,1,Result);
    end;

    // decodifica a escrita de uma saida digital
    // decodes a write to a single coil
    $05: begin
      address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
      if Result=ioOk then begin

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

    // decodifica a escrita de um registro
    // decodes a write to a single register
    $06: begin
      address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
      if Result=ioOk then begin
        SetLength(values,1);

        values[0] := pkg.BufferToWrite[4]*256+pkg.BufferToWrite[5];
        if foundPLC then
          PModbusPLC[plc].Registers.SetValues(address,1,1,values,Result);
      end else
        if foundPLC then
          PModbusPLC[plc].Registers.SetFault(address,1,1,Result)
    end;

    // decodifica o status do escravo
    // decodes a the current state of the slave
    $07: begin
      if foundPLC then begin
         if Result=ioOk then begin
           PModbusPLC[plc].Status07Value :=LongInt(pkg.BufferToRead[2]);
           PModbusPLC[plc].Status07TimeStamp := CrossNow;
         end;
         PModbusPLC[plc].Status07LastError := Result;
      end;
    end;

    // decodifica a escrita de multiplos saidas digitais
    // decodes a write to multiple coils.
    $0F: begin
      address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
      len     := (pkg.BufferToWrite[4] * 256) + pkg.BufferToWrite[5];
      if Result=ioOk then begin
        SetLength(values,len);

        i := 0;
        c := 0;
        c2:= 7;
        while i<len do begin
          if (c=8) then begin
            c:=0;
            inc(c2);
          end;
          Values[i]:=IfThen(((LongInt(pkg.BufferToWrite[c2]) and (1 shl c))=(1 shl c)),1,0);
          inc(i);
          inc(c);
        end;
        if foundPLC then
          PModbusPLC[plc].OutPuts.SetValues(address,len,1,values,Result);
      end else
        if foundPLC then
          PModbusPLC[plc].OutPuts.SetFault(address,len,1,Result);
    end;

    // decodifica a escrita de multiplos registros
    // decodes a write to a multiple registers
    $10: begin
      address := (pkg.BufferToWrite[2] * 256) + pkg.BufferToWrite[3];
      len     := (pkg.BufferToWrite[4] * 256) + pkg.BufferToWrite[5];
      if Result=ioOk then begin

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
      //tratamento de erros modbus
      //modbus error handling.
      case pkg.BufferToRead[2] of
        $01:
          Result := ioIllegalFunction;
        $02:
          Result := ioIllegalRegAddress;
        $03:
          Result := ioIllegalValue;
        $04:
          Result := ioPLCError;
        $05:
          Result := ioAcknowledge;
        $06:
          Result := ioBusy;
        $07:
          Result := ioNACK;
        $08:
          Result := ioMemoryParityError;
        $0A:
          Result := ioGatewayUnavailable;
        $0B:
          Result := ioDeviceGatewayFailedToRespond;
        else
          Result := ioCommError;
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

function TModBusRTUDriver.RemainingBytes(buffer: BYTES): LongInt;
begin
  Result:=255; //if some communication error happens, set the next read
               //to read 255 bytes and clear the remaining buffer.
  if Length(buffer)>=3 then begin
    if (buffer[PFuncByteOffset] and $80)=$80 then begin
      Result:=2; //is remaining the CRC at the buffer.
    end else begin
      case buffer[PFuncByteOffset] of
        1,2,3,4:
          Result:=buffer[PFuncByteOffset+1]+2;
        5,6,15,16:
          Result:=5;
      end;
    end;
  end;
end;

end.
