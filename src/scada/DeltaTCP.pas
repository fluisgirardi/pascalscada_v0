{$i ../common/language.inc}
{:
  @abstract(Unit that implements the Delta (台达) TCP protocol driver.)

  The Delta PLCs with Ethernet (DVP-ES2/SE, AS series, ...) speak standard
  Modbus TCP on port 502, so the wire frame is the plain Modbus TCP frame:

  @code(
    request:  [txH][txL][00][00][lenH][lenL][unit][fc][address...][quantity...]
    answer :  [txH][txL][00][00][lenH][lenL][unit][fc][bytecount][data...]
    error  :  [txH][txL][00][00][00][03][unit][fc|$80][exception]
  )

  Function codes: 01 read coils, 02 read discrete inputs (X), 03 read holding
  registers, 04 read input registers, 05 write single coil, 06 write single
  register, 15 write multiple coils, 16 write multiple registers. The Delta
  addresses (M100, D200...) are translated to the Modbus address space by
  @link(DeltaDriver.TDeltaDriver).

  @seealso(DeltaDriver.TDeltaDriver)
}
unit DeltaTCP;

interface

uses
  DeltaDriver, Tag, commtypes, Classes;

type
  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe driver do protocolo Delta (台达) sobre TCP/IP.)
  }
  {$ELSE}
  {:
  @abstract(Class of the Delta (台达) protocol driver over TCP/IP.)

  Connect it to a @link(TTCP_UDPPort) configured with the IP address of the PLC
  (port 502). See @link(DeltaDriver.TDeltaDriver) for the address table of the
  tags and the @code(Series) property to select between the DVP and the AS
  series address map.

  @seealso(DeltaDriver.TDeltaDriver)
  @seealso(TTCP_UDPPort)
  }
  {$ENDIF}
  TDeltaTCPDriver = class(TDeltaDriver)
  private
    FMessageID:Word;
    function ReadRemaining(buffer:BYTES):LongInt;
  protected
    //: @seealso(TDeltaDriver.EncodePkg)
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES; override;
    //: @seealso(TDeltaDriver.DecodePkg)
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; override;
    //: @seealso(TDeltaDriver.RemainingBytesRead)
    function RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt; override;
    //: @seealso(TDeltaDriver.RemainingBytesWrite)
    function RemainingBytesWrite(buffer:BYTES):LongInt; override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    //: @seealso(TDeltaDriver.Series)
    property Series;
    //: @seealso(TDeltaDriver.IOAddressFormat)
    property IOAddressFormat;
    //: @seealso(TDeltaDriver.ReadSomethingAlways)
    property ReadSomethingAlways;
    //: @seealso(TDeltaDriver.BitMaxHole)
    property BitMaxHole;
    //: @seealso(TDeltaDriver.WordMaxHole)
    property WordMaxHole;
    //: @seealso(TDeltaDriver.BitMaxBlockSize)
    property BitMaxBlockSize;
    //: @seealso(TDeltaDriver.WordMaxBlockSize)
    property WordMaxBlockSize;
    //: @seealso(TProtocolDriver.ReadOnly)
    property ReadOnly;
  end;

implementation

uses SysUtils, Math;

constructor TDeltaTCPDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FMessageID := 0;
  PInternalDelayBetweenCmds := 0;
  PFirstRequestLen := 8;   //6 bytes of the MBAP header + unit id + function
  PFuncByteOffset := 7;
  PCRCLen := 0;
end;

function TDeltaTCPDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES;
var
  dev, fc, size, byteLen, i, c, c2, coreLen:LongInt;
  isWrite, isBit:Boolean;
begin
  ResultLen := 0;
  Result := nil;

  if ToWrite = nil then
    dev := DeviceIndex(TagObj.ReadFunction)
  else
    dev := DeviceIndex(TagObj.WriteFunction);

  //elemento de memoria desconhecido ou nao suportado pela serie: nao ha o que
  //montar.
  //unknown memory area or not supported by the series: there is nothing to
  //build.
  if (dev < 0) or (not DeviceSupportedBySeries(dev)) then
    exit;

  isWrite := (ToWrite <> nil);
  isBit := DELTA_DEV_ISBIT[dev];

  //X e' uma entrada fisica: nao pode ser escrita.
  //X is a physical input: it can't be written.
  if isWrite and (DELTA_DEV_FUNC[dev] = DELTA_FUNC_X) then
    exit;

  size := TagObj.Size;
  if (size <= 0) or (TagObj.Address < 0) or (TagObj.Address > $FFFF) then
    exit;

  //codigo de funcao modbus do pedido.
  //modbus function code of the request.
  if not isWrite then begin
    fc := DeviceReadFuncCode(dev);
  end else begin
    if isBit then begin
      if size = 1 then
        fc := 5     //write single coil
      else
        fc := 15;   //write multiple coils
    end else begin
      if size = 1 then
        fc := 6     //write single register
      else
        fc := 16;   //write multiple registers
    end;
  end;

  if not isWrite then begin
    //leitura de bits ou de words.
    //read of bits or of words.
    if isBit then begin
      if size > 1920 then
        exit;
      byteLen := (size + 7) div 8;
    end else begin
      if size > 120 then
        exit;
      byteLen := size * 2;
    end;

    coreLen := 6;
    SetLength(Result,6 + coreLen);
    for i:=0 to High(Result) do
      Result[i] := 0;
    Result[00] := (FMessageID shr 8) and $FF;
    Result[01] := FMessageID and $FF;
    Result[02] := 0;
    Result[03] := 0;
    Result[04] := (coreLen shr 8) and $FF;
    Result[05] := coreLen and $FF;
    Result[06] := TagObj.Station and $FF;
    Result[07] := fc;
    Result[08] := (TagObj.Address shr 8) and $FF;
    Result[09] := TagObj.Address and $FF;
    Result[10] := (size shr 8) and $FF;
    Result[11] := size and $FF;

    //resposta: 7 bytes de cabecalho MBAP+unit, funcao, bytecount e dados.
    //answer: 7 bytes of MBAP+unit header, function, bytecount and data.
    ResultLen := 9 + byteLen;
  end else begin
    //escrita de bits ou de words.
    //write of bits or of words.
    if Length(ToWrite) < size then
      size := Length(ToWrite);
    if size <= 0 then
      exit;

    if fc = 5 then begin
      //escrita de uma bobina: o valor vem nos 16 bits do campo de dados.
      //write of a single coil: the value comes on the 16 data bits.
      coreLen := 6;
      SetLength(Result,6 + coreLen);
      for i:=0 to High(Result) do
        Result[i] := 0;
      Result[00] := (FMessageID shr 8) and $FF;
      Result[01] := FMessageID and $FF;
      Result[02] := 0;
      Result[03] := 0;
      Result[04] := (coreLen shr 8) and $FF;
      Result[05] := coreLen and $FF;
      Result[06] := TagObj.Station and $FF;
      Result[07] := fc;
      Result[08] := (TagObj.Address shr 8) and $FF;
      Result[09] := TagObj.Address and $FF;
      if ToWrite[0] <> 0 then
        Result[10] := $FF;

      ResultLen := 12;
    end else
    if fc = 6 then begin
      //escrita de um registrador.
      //write of a single register.
      coreLen := 6;
      SetLength(Result,6 + coreLen);
      for i:=0 to High(Result) do
        Result[i] := 0;
      Result[00] := (FMessageID shr 8) and $FF;
      Result[01] := FMessageID and $FF;
      Result[02] := 0;
      Result[03] := 0;
      Result[04] := (coreLen shr 8) and $FF;
      Result[05] := coreLen and $FF;
      Result[06] := TagObj.Station and $FF;
      Result[07] := fc;
      Result[08] := (TagObj.Address shr 8) and $FF;
      Result[09] := TagObj.Address and $FF;
      Result[10] := (Trunc(ToWrite[0]) shr 8) and $FF;
      Result[11] := Trunc(ToWrite[0]) and $FF;

      ResultLen := 12;
    end else begin
      //escrita multipla de bits (15) ou de words (16).
      //multiple write of bits (15) or words (16).
      if isBit then
        byteLen := (size + 7) div 8
      else
        byteLen := size * 2;

      coreLen := 7 + byteLen;
      SetLength(Result,6 + coreLen);
      for i:=0 to High(Result) do
        Result[i] := 0;
      Result[00] := (FMessageID shr 8) and $FF;
      Result[01] := FMessageID and $FF;
      Result[02] := 0;
      Result[03] := 0;
      Result[04] := (coreLen shr 8) and $FF;
      Result[05] := coreLen and $FF;
      Result[06] := TagObj.Station and $FF;
      Result[07] := fc;
      Result[08] := (TagObj.Address shr 8) and $FF;
      Result[09] := TagObj.Address and $FF;
      Result[10] := (size shr 8) and $FF;
      Result[11] := size and $FF;
      Result[12] := byteLen;

      if isBit then begin
        i := 0;
        c := 0;
        c2 := 13;
        while (i < size) do begin
          if ToWrite[i] <> 0 then
            Result[c2] := Result[c2] or (1 shl c);
          inc(i);
          inc(c);
          if c > 7 then begin
            c := 0;
            inc(c2);
          end;
        end;
      end else begin
        for i:=0 to size-1 do begin
          if i < Length(ToWrite) then begin
            Result[13+i*2] := (Trunc(ToWrite[i]) shr 8) and $FF;
            Result[14+i*2] := Trunc(ToWrite[i]) and $FF;
          end else begin
            Result[13+i*2] := 0;
            Result[14+i*2] := 0;
          end;
        end;
      end;

      //a resposta de escrita ecoa o pedido: 7 bytes de cabecalho+unit, funcao,
      //endereco e quantidade.
      //the write answer echoes the request: 7 bytes of header+unit, function,
      //address and quantity.
      ResultLen := 12;
    end;
  end;

  FMessageID := (FMessageID + 1) and $FFFF;
end;

function TDeltaTCPDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
var
  i, c, c2, plc, dev, reqFC, respFC, address, len, byteLen:LongInt;
  reqIsBit, ok:Boolean;
  foundPLC:Boolean;
begin
  SetLength(values,0);

  //sem o pedido nao ha como saber o que foi perguntado: uma leitura tem 12
  //bytes, uma escrita nunca tem menos que isso.
  //without the request there is no way to know what was asked: a read has 12
  //bytes and a write never has less than that.
  if Length(pkg.BufferToWrite) < 12 then begin
    Result := ioDriverError;
    exit;
  end;

  //se alguma das IOs falhou,
  //if some IO failed,
  Result := ioOk;
  case pkg.WriteIOResult of
    iorTimeOut:
      Result := ioTimeOut;
    iorNotReady,
    iorNone:
      Result := ioDriverError;
    iorPortError:
      Result := ioCommError;
  end;

  if (Result = ioOk) then
    case pkg.ReadIOResult of
      iorTimeOut:
        Result := ioTimeOut;
      iorNotReady,
      iorNone:
        Result := ioDriverError;
      iorPortError:
        Result := ioCommError;
    end;

  //a resposta tem 6 bytes de cabecalho MBAP, unit id e funcao: abaixo disso
  //nao ha o que conferir.
  //the answer has 6 bytes of MBAP header, unit id and function: below that
  //there is nothing to check.
  if (Result = ioOk) and (Length(pkg.BufferToRead) < 8) then
    Result := ioCommError;

  foundPLC := false;
  for plc:=0 to High(PDeltaPLC) do
    if PDeltaPLC[plc].Station = pkg.BufferToWrite[6] then begin
      foundPLC := true;
      break;
    end;

  reqFC := pkg.BufferToWrite[7] and $FF;
  reqIsBit := reqFC in [1, 2, 5, 15];

  address := (LongInt(pkg.BufferToWrite[8]) shl 8) or
              LongInt(pkg.BufferToWrite[9]);
  len := (LongInt(pkg.BufferToWrite[10]) shl 8) or
          LongInt(pkg.BufferToWrite[11]);

  dev := DeviceIndexByWireAddress(reqFC, address);

  //se a resposta nao confere com o pedido...
  //if the answer doesn't match the request...
  if (Result = ioOk) then begin
    respFC := pkg.BufferToRead[7] and $FF;
    if (respFC = ((reqFC or $80) and $FF)) and (Length(pkg.BufferToRead) < 9) then
      Result := ioCommError
    else if respFC = ((reqFC or $80) and $FF) then begin
      //codigo de excecao modbus no nono byte.
      //modbus exception code on the ninth byte.
      case pkg.BufferToRead[8] of
        $01: Result := ioIllegalFunction;
        $02: Result := ioIllegalRegAddress;
        $03: Result := ioIllegalValue;
        $04: Result := ioPLCError;
        $05: Result := ioAcknowledge;
        $06: Result := ioBusy;
        $07: Result := ioNACK;
        $08: Result := ioMemoryParityError;
        $0A: Result := ioCommError;
        $0B: Result := ioCommError;
        else Result := ioPLCError;
      end;
    end else
      if respFC <> reqFC then
        Result := ioCommError;
  end;

  //confere o numero de bytes de dado declarado na resposta contra o pedido:
  //um quadro pela metade nao pode virar valor. Vale apenas para as leituras:
  //nas escritas (05/06) os bytes 10-11 carregam valor, nao quantidade.
  //check the declared number of data bytes against the request: a partial
  //frame must not become values. Applies to reads only: on single writes
  //(05/06) bytes 10-11 carry the value, not the quantity.
  if (Result = ioOk) and (reqFC in [1, 2]) then begin
    byteLen := (len + 7) div 8;
    if Length(pkg.BufferToRead) < (9 + byteLen) then
      Result := ioCommError;
  end;
  if (Result = ioOk) and (reqFC = 3) then begin
    byteLen := len * 2;
    if Length(pkg.BufferToRead) < (9 + byteLen) then
      Result := ioCommError;
  end;

  //leitura de bits (01 coils, 02 discrete inputs).
  //read of bits (01 coils, 02 discrete inputs).
  if reqFC in [1, 2] then begin
    if (Result = ioOk) then begin
      SetLength(values,len);
      c2 := 9;
      c := 0;
      for i:=0 to len-1 do begin
        if c = 8 then begin
          c := 0;
          inc(c2);
        end;
        if (c2 < Length(pkg.BufferToRead)) and
           ((LongInt(pkg.BufferToRead[c2]) and (1 shl c)) <> 0) then
          values[i] := 1
        else
          values[i] := 0;
        inc(c);
      end;
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetValues(address,len,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetFault(address,len,1,Result);
  end;

  //leitura de words (03 holding registers).
  //read of words (03 holding registers).
  if reqFC = 3 then begin
    if (Result = ioOk) then begin
      SetLength(values,len);
      for i:=0 to len-1 do
        values[i] := (LongInt(pkg.BufferToRead[9+i*2]) shl 8) or
                      LongInt(pkg.BufferToRead[10+i*2]);
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetValues(address,len,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetFault(address,len,1,Result);
  end;

  //escrita de uma bobina (05): o valor sai do proprio pedido.
  //write of a single coil (05): the value comes from the request itself.
  if reqFC = 5 then begin
    if (Result = ioOk) then begin
      SetLength(values,1);
      if (Length(pkg.BufferToWrite) > 10) and (pkg.BufferToWrite[10] = $FF) then
        values[0] := 1
      else
        values[0] := 0;
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetValues(address,1,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetFault(address,1,1,Result);
  end;

  //escrita de um registrador (06): o valor sai do proprio pedido.
  //write of a single register (06): the value comes from the request itself.
  if reqFC = 6 then begin
    if (Result = ioOk) then begin
      SetLength(values,1);
      if Length(pkg.BufferToWrite) > 11 then
        values[0] := (LongInt(pkg.BufferToWrite[10]) shl 8) or
                      LongInt(pkg.BufferToWrite[11])
      else
        values[0] := 0;
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetValues(address,1,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetFault(address,1,1,Result);
  end;

  //escrita multipla de bits (15): os valores saem do proprio pedido.
  //multiple write of bits (15): the values come from the request itself.
  if reqFC = 15 then begin
    if (Result = ioOk) then begin
      SetLength(values,len);
      c2 := 13;
      c := 0;
      for i:=0 to len-1 do begin
        if c = 8 then begin
          c := 0;
          inc(c2);
        end;
        if (c2 < Length(pkg.BufferToWrite)) and
           ((LongInt(pkg.BufferToWrite[c2]) and (1 shl c)) <> 0) then
          values[i] := 1
        else
          values[i] := 0;
        inc(c);
      end;
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetValues(address,len,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetFault(address,len,1,Result);
  end;

  //escrita multipla de words (16): os valores saem do proprio pedido.
  //multiple write of words (16): the values come from the request itself.
  if reqFC = 16 then begin
    if (Result = ioOk) then begin
      SetLength(values,len);
      for i:=0 to len-1 do begin
        if (14+i*2) < Length(pkg.BufferToWrite) then
          values[i] := (LongInt(pkg.BufferToWrite[13+i*2]) shl 8) or
                        LongInt(pkg.BufferToWrite[14+i*2])
        else
          values[i] := 0;
      end;
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetValues(address,len,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].SetFault(address,len,1,Result);
  end;
end;

function TDeltaTCPDriver.ReadRemaining(buffer:BYTES):LongInt;
var
  total:LongInt;
begin
  Result := 0;
  if Length(buffer) < 6 then
    exit;
  //o tamanho declarado no MBAP cobre unit id, funcao e dados; ja' foram lidos
  //cabecalho(6) + unit id + funcao.
  //the length declared on the MBAP covers unit id, function and data; header
  //(6) + unit id + function were already read.
  total := (LongInt(buffer[4]) shl 8) or LongInt(buffer[5]);
  if total > 2 then
    Result := total - 2;
end;

function TDeltaTCPDriver.RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt;
begin
  Result := ReadRemaining(buffer);
end;

function TDeltaTCPDriver.RemainingBytesWrite(buffer:BYTES):LongInt;
begin
  Result := ReadRemaining(buffer);
end;

end.
