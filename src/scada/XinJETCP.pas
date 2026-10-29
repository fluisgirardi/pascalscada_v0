{$i ../common/language.inc}
{:
  @abstract(Unit that implements the Xinje (信捷) TCP protocol driver.)

  The frame is the one used by the Xinje internal protocol, which is the modbus
  TCP frame with the function code replaced by a command byte:

  @code(
    request:  [idH][idL][00][00][lenH][lenL][station][cmd][code][a2][a1][a0][nH][nL]
    answer :  [idH][idL][00][00][lenH][lenL][station][cmd][size][data...]
  )

  Commands: $1E read bits, $1F write bits, $20 read words, $21 write words.
  The byte @code(code) is the data code of the soft element (D=128, M=3, ...)
  and @code(a2..a0) is a 24 bit big endian address.

  @seealso(XinJEDriver.TXinJEDriver)
}
unit XinJETCP;

interface

uses
  XinJEDriver, Tag, commtypes, Classes;

type
  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe driver do protocolo Xinje (信捷) sobre TCP/IP.)
  }
  {$ELSE}
  {:
  @abstract(Class of the Xinje (信捷) protocol driver over TCP/IP.)

  Connect it to a @link(TTCP_UDPPort) configured with the IP address of the PLC
  (port 502). See @link(XinJEDriver.TXinJEDriver) for the address table of the
  tags.

  @seealso(XinJEDriver.TXinJEDriver)
  @seealso(TTCP_UDPPort)
  }
  {$ENDIF}
  TXinJETCPDriver = class(TXinJEDriver)
  private
    FMessageID:Word;
    function ReadRemaining(buffer:BYTES):LongInt;
  protected
    //: @seealso(TXinJEDriver.EncodePkg)
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES; override;
    //: @seealso(TXinJEDriver.DecodePkg)
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; override;
    //: @seealso(TXinJEDriver.RemainingBytesRead)
    function RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt; override;
    //: @seealso(TXinJEDriver.RemainingBytesWrite)
    function RemainingBytesWrite(buffer:BYTES):LongInt; override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    //: @seealso(TXinJEDriver.ReadSomethingAlways)
    property ReadSomethingAlways;
    //: @seealso(TXinJEDriver.BitMaxHole)
    property BitMaxHole;
    //: @seealso(TXinJEDriver.WordMaxHole)
    property WordMaxHole;
    //: @seealso(TXinJEDriver.BitMaxBlockSize)
    property BitMaxBlockSize;
    //: @seealso(TXinJEDriver.WordMaxBlockSize)
    property WordMaxBlockSize;
    //: @seealso(TXinJEDriver.IOAddressFormat)
    property IOAddressFormat;
    //: @seealso(TProtocolDriver.ReadOnly)
    property ReadOnly;
  end;

implementation

uses SysUtils, Math;

constructor TXinJETCPDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FMessageID := 0;
  PInternalDelayBetweenCmds := 0;
  PFirstRequestLen := 8;   //6 bytes of the header + station + command
  PFuncByteOffset := 7;
  PCRCLen := 0;
end;

function TXinJETCPDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES;
var
  dev, cmd, code, size, byteLen, coreLen, i, c, c2:LongInt;
begin
  ResultLen := 0;
  Result := nil;

  if ToWrite = nil then
    dev := DeviceIndex(TagObj.ReadFunction)
  else
    dev := DeviceIndex(TagObj.WriteFunction);

  //elemento de memoria desconhecido: nao ha o que montar.
  //unknown memory area: there is nothing to build.
  if dev < 0 then
    exit;

  size := TagObj.Size;
  if (size <= 0) or (TagObj.Address < 0) or (TagObj.Address > $FFFFFF) then
    exit;

  cmd := XINJE_CMD_READ_WORDS;
  if XINJE_DEV_ISBIT[dev] then
    cmd := XINJE_CMD_READ_BITS;
  if ToWrite <> nil then begin
    if XINJE_DEV_ISBIT[dev] then
      cmd := XINJE_CMD_WRITE_BITS
    else
      cmd := XINJE_CMD_WRITE_WORDS;
  end;

  code := XINJE_DEV_DATACODE[dev];

  if ToWrite = nil then begin
    //leitura de bits ou de words.
    //read of bits or of words.
    if XINJE_DEV_ISBIT[dev] then begin
      if size > 1920 then
        exit;
      byteLen := (size + 7) div 8;
    end else begin
      if size > 120 then
        exit;
      byteLen := size * 2;
    end;

    coreLen := 8;
    SetLength(Result,XINJE_TCP_HEADER_LEN + coreLen);
    for i:=0 to High(Result) do
      Result[i] := 0;
    Result[00] := (FMessageID shr 8) and $FF;
    Result[01] := FMessageID and $FF;
    Result[02] := 0;
    Result[03] := 0;
    Result[04] := (coreLen shr 8) and $FF;
    Result[05] := coreLen and $FF;
    Result[06] := TagObj.Station and $FF;
    Result[07] := cmd;
    Result[08] := code;
    Result[09] := (TagObj.Address shr 16) and $FF;
    Result[10] := (TagObj.Address shr 8) and $FF;
    Result[11] := TagObj.Address and $FF;
    Result[12] := (size shr 8) and $FF;
    Result[13] := size and $FF;

    ResultLen := 9 + byteLen;
  end else begin
    //escrita de bits ou de words.
    //write of bits or of words.
    if Length(ToWrite) < size then
      size := Length(ToWrite);
    if size <= 0 then
      exit;

    if XINJE_DEV_ISBIT[dev] then
      byteLen := (size + 7) div 8
    else
      byteLen := size * 2;

    coreLen := 9 + byteLen;
    SetLength(Result,XINJE_TCP_HEADER_LEN + coreLen);
    for i:=0 to High(Result) do
      Result[i] := 0;
    Result[00] := (FMessageID shr 8) and $FF;
    Result[01] := FMessageID and $FF;
    Result[02] := 0;
    Result[03] := 0;
    Result[04] := (coreLen shr 8) and $FF;
    Result[05] := coreLen and $FF;
    Result[06] := TagObj.Station and $FF;
    Result[07] := cmd;
    Result[08] := code;
    Result[09] := (TagObj.Address shr 16) and $FF;
    Result[10] := (TagObj.Address shr 8) and $FF;
    Result[11] := TagObj.Address and $FF;

    if XINJE_DEV_ISBIT[dev] then begin
      //numero de bits a escrever
      //number of bits being written
      Result[12] := (size shr 8) and $FF;
      Result[13] := size and $FF;
      Result[14] := byteLen;
      i := 0;
      c := 0;
      c2 := 15;
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
      Result[12] := (size shr 8) and $FF;
      Result[13] := size and $FF;
      Result[14] := byteLen;
      for i:=0 to size-1 do begin
        if i < Length(ToWrite) then begin
          Result[15+i*2] := (Trunc(ToWrite[i]) shr 8) and $FF;
          Result[16+i*2] := Trunc(ToWrite[i]) and $FF;
        end else begin
          Result[15+i*2] := 0;
          Result[16+i*2] := 0;
        end;
      end;
    end;

    //a resposta de escrita so' confirma o pedido: 6 bytes de cabecalho mais
    //station, comando e tamanho.
    //the write answer only acknowledges the request: 6 bytes of header plus
    //station, command and size.
    ResultLen := 9;
  end;

  FMessageID := (FMessageID + 1) and $FFFF;
end;

function TXinJETCPDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
var
  i, c, c2, plc, dev, reqCmd, respCmd, address, len, byteLen:LongInt;
  foundPLC:Boolean;
begin
  SetLength(values,0);

  //sem o pedido nao ha como saber o que foi perguntado: leitura tem 14 bytes,
  //escrita nunca tem menos que isso.
  //without the request there is no way to know what was asked: a read has 14
  //bytes and a write never has less than that.
  if Length(pkg.BufferToWrite) < 14 then begin
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

  //a resposta tem 6 bytes de cabecalho, station e comando: abaixo disso nao ha
  //o que conferir.
  //the answer has 6 bytes of header, station and command: below that there is
  //nothing to check.
  if (Result = ioOk) and (Length(pkg.BufferToRead) < 8) then
    Result := ioCommError;

  foundPLC := false;
  for plc:=0 to High(PXinJEPLC) do
    if PXinJEPLC[plc].Station = pkg.BufferToWrite[6] then begin
      foundPLC := true;
      break;
    end;

  reqCmd := pkg.BufferToWrite[7] and $FF;
  dev := DeviceIndexByDataCode(pkg.BufferToWrite[8] and $FF);

  address := (LongInt(pkg.BufferToWrite[9]) shl 16) or
             (LongInt(pkg.BufferToWrite[10]) shl 8) or
              LongInt(pkg.BufferToWrite[11]);
  len := (LongInt(pkg.BufferToWrite[12]) shl 8) or
          LongInt(pkg.BufferToWrite[13]);

  //se a resposta nao confere com o pedido...
  //if the answer doesn't match the request...
  if (Result = ioOk) then begin
    respCmd := pkg.BufferToRead[7] and $FF;
    if (respCmd = ((reqCmd or $80) and $FF)) and (Length(pkg.BufferToRead) < 9) then
      Result := ioCommError
    else if respCmd = ((reqCmd or $80) and $FF) then begin
      //codigo de erro do PLC no nono byte.
      //PLC error code on the ninth byte.
      case pkg.BufferToRead[8] of
        $01: Result := ioIllegalFunction;
        $02: Result := ioIllegalRegAddress;
        $03: Result := ioIllegalValue;
        $04: Result := ioPLCError;
        $05: Result := ioAcknowledge;
        $06: Result := ioBusy;
        $07: Result := ioNACK;
        $08: Result := ioMemoryParityError;
        else Result := ioPLCError;
      end;
    end else
      if respCmd <> reqCmd then
        Result := ioCommError;
  end;

  //confere o numero de bytes de dado declarado na resposta contra o pedido:
  //um quadro pela metade nao pode virar valor.
  //check the declared number of data bytes against the request: a partial
  //frame must not become values.
  if (Result = ioOk) and (reqCmd = XINJE_CMD_READ_BITS) then begin
    byteLen := (len + 7) div 8;
    if Length(pkg.BufferToRead) < (9 + byteLen) then
      Result := ioCommError;
  end;
  if (Result = ioOk) and (reqCmd = XINJE_CMD_READ_WORDS) then begin
    byteLen := len * 2;
    if Length(pkg.BufferToRead) < (9 + byteLen) then
      Result := ioCommError;
  end;

  //leitura de bits das entradas/saidas/relays
  //read of the bits (inputs, outputs, relays)
  if (reqCmd = XINJE_CMD_READ_BITS) then begin
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
        PXinJEPLC[plc].Devices[dev].SetValues(address,len,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PXinJEPLC[plc].Devices[dev].SetFault(address,len,1,Result);
  end;

  //leitura de words dos registradores
  //read of the words (registers)
  if (reqCmd = XINJE_CMD_READ_WORDS) then begin
    if (Result = ioOk) then begin
      SetLength(values,len);
      for i:=0 to len-1 do
        values[i] := (LongInt(pkg.BufferToRead[9+i*2]) shl 8) or
                      LongInt(pkg.BufferToRead[10+i*2]);
      if foundPLC and (dev >= 0) then
        PXinJEPLC[plc].Devices[dev].SetValues(address,len,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PXinJEPLC[plc].Devices[dev].SetFault(address,len,1,Result);
  end;

  //escrita de bits: os valores saem do proprio pedido.
  //write of bits: the values come from the request itself.
  if (reqCmd = XINJE_CMD_WRITE_BITS) then begin
    if (Result = ioOk) then begin
      SetLength(values,len);
      c2 := 15;
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
        PXinJEPLC[plc].Devices[dev].SetValues(address,len,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PXinJEPLC[plc].Devices[dev].SetFault(address,len,1,Result);
  end;

  //escrita de words: os valores saem do proprio pedido.
  //write of words: the values come from the request itself.
  if (reqCmd = XINJE_CMD_WRITE_WORDS) then begin
    if (Result = ioOk) then begin
      SetLength(values,len);
      for i:=0 to len-1 do begin
        if (16+i*2) < Length(pkg.BufferToWrite) then
          values[i] := (LongInt(pkg.BufferToWrite[15+i*2]) shl 8) or
                        LongInt(pkg.BufferToWrite[16+i*2])
        else
          values[i] := 0;
      end;
      if foundPLC and (dev >= 0) then
        PXinJEPLC[plc].Devices[dev].SetValues(address,len,1,values,Result);
    end else
      if foundPLC and (dev >= 0) then
        PXinJEPLC[plc].Devices[dev].SetFault(address,len,1,Result);
  end;
end;

function TXinJETCPDriver.ReadRemaining(buffer:BYTES):LongInt;
var
  total:LongInt;
begin
  Result := 0;
  if Length(buffer) < 6 then
    exit;
  //o tamanho declarado cobre station, comando e dados; ja' foram lidos
  //cabecalho(6) + station + comando.
  //the declared length covers station, command and data; header(6) + station +
  //command were already read.
  total := (LongInt(buffer[4]) shl 8) or LongInt(buffer[5]);
  if total > 2 then
    Result := total - 2;
end;

function TXinJETCPDriver.RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt;
begin
  Result := ReadRemaining(buffer);
end;

function TXinJETCPDriver.RemainingBytesWrite(buffer:BYTES):LongInt;
begin
  Result := ReadRemaining(buffer);
end;

end.
