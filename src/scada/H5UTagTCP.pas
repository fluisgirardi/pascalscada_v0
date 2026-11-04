{$i ../common/language.inc}
{:
  @abstract(Unit that implements the Inovance (汇川) H5U tag area protocol over TCP/IP.)

  The H5U keeps the variables of the program in a flat bit addressed variable
  area that is served on the TCP port 12939 by what Inovance calls the
  @bold(H5U TCP Monitor Protocol) - the protocol behind "导出 HMI 监视变量表"
  of AutoShop and behind the tag import of the Inovance IT7000, EasyBuilder and
  MCGS HMIs. It is @bold(not) part of the Modbus map of the port 502: the
  official Modbus manual of the H5U only maps M/B/S/X/Y (coils, 0..65535) and
  D/R (registers, 0..65535), while the variable area goes up to $4C4B400.

  Inovance does not publish the frame layout of the port 12939, so this unit
  implements the two layouts seen in the field and lets the layout be chosen at
  design time through @code(FrameFormat):

  @bold(hffMbapExt) - Modbus TCP frame with a 32 bit address (default)

  @code(
    request : [txH][txL][00][00][lenH][lenL][unit][fc][addr:4][count:2][data]
    answer  : [txH][txL][00][00][lenH][lenL][unit][fc][bytecount][data]
    error   : [txH][txL][00][00][lenH][lenL][unit][fc|$80][exception]
  )

  @bold(hffHzExt) - "HZ" framed variant, a 10 byte header before the command

  @code(
    request : [48][5A][seqH][seqL][00][00][00][00][lenH][lenL][fc][addr:4][count:2][data]
    answer  : [48][5A][seqH][seqL][00][00][00][00][lenH][lenL][fc][bytecount][data]
  )

  On both layouts the address is the 32 bit value of the variable area written
  big endian, the function codes are the Modbus ones (01 read bits, 03 read
  words, 05/06 write one, 0F/10 write many) and the exception codes are the
  Modbus ones.

  @bold(Calibrating the layout against a real PLC)

  Fill the @code(TraceFile) property with a file name and every request and
  answer is appended to it as hexadecimal. A capture of the port 12939 taken
  while AutoShop or the HMI is online gives the exact layout; if it differs from
  the two above, only @code(EncodePkg) / @code(DecodePkg) of this unit have to
  be adjusted - the tag engine of @link(H5UTagDriver.TH5UTagDriver) doesn't
  change.

  @seealso(H5UTagDriver.TH5UTagDriver)
}

unit H5UTagTCP;

interface

uses
  H5UTagDriver, Tag, commtypes, Classes, SysUtils;

type
  {: Frame layout used on the port 12939. }
  TH5UTagFrameFormat = (hffMbapExt, hffHzExt);

  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe driver do protocolo de tags do H5U (汇川) sobre TCP/IP.)
  }
  {$ELSE}
  {:
  @abstract(Class of the Inovance (汇川) H5U tag protocol driver over TCP/IP.)

  Connect it to a @link(TTCP_UDPPort) configured with the IP address of the PLC
  and the port @bold(12939). See @link(H5UTagDriver.TH5UTagDriver) for the
  address of the tags ('UB' bit area, 'UW' word area, MemAddress = the
  hexadecimal number of the AutoShop CSV written in decimal).

  @seealso(H5UTagDriver.TH5UTagDriver)
  @seealso(TTCP_UDPPort)
  }
  {$ENDIF}
  TH5UTagTCPDriver = class(TH5UTagDriver)
  private
    FMessageID:Word;
    FFrameFormat:TH5UTagFrameFormat;
    FTraceFile:string;
    procedure SetFrameFormat(v:TH5UTagFrameFormat);
    procedure Trace(ADirection:AnsiString; buffer:BYTES);
    function  ReadRemaining(buffer:BYTES):LongInt;
  protected
    //: @seealso(TH5UTagDriver.EncodePkg)
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES; override;
    //: @seealso(TH5UTagDriver.DecodePkg)
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; override;
    //: @seealso(TH5UTagDriver.RemainingBytesRead)
    function RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt; override;
    //: @seealso(TH5UTagDriver.RemainingBytesWrite)
    function RemainingBytesWrite(buffer:BYTES):LongInt; override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    {: Layout of the frame sent to the port 12939. Default @code(hffMbapExt). }
    property FrameFormat:TH5UTagFrameFormat read FFrameFormat write SetFrameFormat default hffMbapExt;
    {: When not empty, every frame sent and received is appended to this file as
       hexadecimal, to calibrate the layout against a capture. }
    property TraceFile:string read FTraceFile write FTraceFile;
    //: @seealso(TH5UTagDriver.WordAddressInBits)
    property WordAddressInBits;
    //: @seealso(TH5UTagDriver.ReadSomethingAlways)
    property ReadSomethingAlways;
    //: @seealso(TH5UTagDriver.BitMaxHole)
    property BitMaxHole;
    //: @seealso(TH5UTagDriver.WordMaxHole)
    property WordMaxHole;
    //: @seealso(TH5UTagDriver.BitMaxBlockSize)
    property BitMaxBlockSize;
    //: @seealso(TH5UTagDriver.WordMaxBlockSize)
    property WordMaxBlockSize;
  end;

implementation

constructor TH5UTagTCPDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FMessageID := 0;
  FFrameFormat := hffMbapExt;
  FTraceFile := '';
  //cabecalho + funcao: o resto do quadro e' lido com o tamanho declarado.
  //header + function: the rest of the frame is read with the declared length.
  PFirstRequestLen := 8;
  PFuncByteOffset := 7;
end;

procedure TH5UTagTCPDriver.SetFrameFormat(v:TH5UTagFrameFormat);
begin
  FFrameFormat := v;
  case FFrameFormat of
    hffMbapExt: begin
      //[txid:2][proto:2][len:2][unit:1][fc]
      PFirstRequestLen := 8;
      PFuncByteOffset := 7;
    end;
    hffHzExt: begin
      //[48][5A][seq:2][reserved:4][len:2][fc]
      PFirstRequestLen := 11;
      PFuncByteOffset := 10;
    end;
  end;
end;

procedure TH5UTagTCPDriver.Trace(ADirection:AnsiString; buffer:BYTES);
var
  f:TextFile;
  i:LongInt;
  s:AnsiString;
begin
  if FTraceFile = '' then
    exit;
  s := '';
  for i:=0 to High(buffer) do
    s := s + IntToHex(buffer[i],2) + ' ';
  try
    AssignFile(f,FTraceFile);
    if FileExists(FTraceFile) then
      Append(f)
    else
      Rewrite(f);
    try
      WriteLn(f,FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now),' ',ADirection,' ',
               Length(buffer),' bytes: ',s);
    finally
      CloseFile(f);
    end;
  except
    //falha de log nunca pode derrubar a comunicacao.
    //a log failure never can break the communication.
  end;
end;

{ Number of header bytes before the function code and the offsets of the length
  field depend on the layout:
  hffMbapExt: total = 6 + lenField, fc at 7, addr at 8, count at 12
  hffHzExt  : total = 10 + lenField, fc at 10, addr at 11, count at 15 }
function TH5UTagTCPDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES;
var
  dev, fc, size, byteLen, i, c, coreLen, hdrLen, baseLen, addrOff, cntOff:LongInt;
  isWrite, isBit:Boolean;
  value:Word;
begin
  ResultLen := 0;
  Result := nil;

  if ToWrite = nil then
    dev := DeviceIndex(TagObj.ReadFunction)
  else
    dev := DeviceIndex(TagObj.WriteFunction);

  if dev < 0 then
    exit;

  isWrite := (ToWrite <> nil);
  isBit := H5UTAG_DEV_ISBIT[dev];

  size := TagObj.Size;
  if (size <= 0) or (TagObj.Address < 0) or (TagObj.Address > H5UTAG_MAX_ADDRESS) then
    exit;

  if isBit then begin
    if size > 1920 then
      exit;
    byteLen := (size + 7) div 8;
  end else begin
    if size > 120 then
      exit;
    byteLen := size * 2;
  end;

  //codigo de funcao do pedido.
  //function code of the request.
  if not isWrite then begin
    if isBit then
      fc := 1
    else
      fc := 3;
  end else begin
    if isBit then begin
      if size = 1 then
        fc := 5
      else
        fc := 15;
    end else begin
      if size = 1 then
        fc := 6
      else
        fc := 16;
    end;
  end;

  //tamanho do PDU.
  //size of the PDU.
  coreLen := 1 + 4 + 2;         //fc + endereco de 32 bits + quantidade
  if isWrite and (fc in [15,16]) then
    coreLen := coreLen + 1 + byteLen;

  case FFrameFormat of
    hffMbapExt: begin
      hdrLen  := 7;   //txid, proto, len, unit
      baseLen := 6;
      addrOff := 8;
      cntOff  := 12;
    end;
    else begin
      hdrLen  := 10;  //48 5A, seq, reserved, len
      baseLen := 10;
      addrOff := 11;
      cntOff  := 15;
    end;
  end;

  SetLength(Result, hdrLen + coreLen);
  for i:=0 to High(Result) do
    Result[i] := 0;

  if FFrameFormat = hffMbapExt then begin
    Result[0] := (FMessageID shr 8) and $FF;
    Result[1] := FMessageID and $FF;
    Result[2] := 0;
    Result[3] := 0;
    Result[4] := ((coreLen + 1) shr 8) and $FF;   //+1 = unit id
    Result[5] := (coreLen + 1) and $FF;
    Result[6] := TagObj.Station and $FF;
  end else begin
    Result[0] := $48;
    Result[1] := $5A;
    Result[2] := (FMessageID shr 8) and $FF;
    Result[3] := FMessageID and $FF;
    Result[4] := 0;
    Result[5] := 0;
    Result[6] := 0;
    Result[7] := 0;
    Result[8] := (coreLen shr 8) and $FF;
    Result[9] := coreLen and $FF;
  end;

  inc(FMessageID);

  //funcao + endereco de 32 bits + quantidade.
  //function + 32 bit address + quantity.
  Result[hdrLen] := fc;
  Result[addrOff]   := (TagObj.Address shr 24) and $FF;
  Result[addrOff+1] := (TagObj.Address shr 16) and $FF;
  Result[addrOff+2] := (TagObj.Address shr 8) and $FF;
  Result[addrOff+3] := TagObj.Address and $FF;
  Result[cntOff]    := (size shr 8) and $FF;
  Result[cntOff+1]  := size and $FF;

  if isWrite then begin
    if fc = 5 then begin
      //escrita de um bit: 0xFF00 liga, 0x0000 desliga.
      //single bit write: 0xFF00 on, 0x0000 off.
      if (Length(ToWrite) > 0) and (ToWrite[0] <> 0) then
        value := $FF00
      else
        value := $0000;
      Result[cntOff]   := (value shr 8) and $FF;
      Result[cntOff+1] := value and $FF;
    end else if fc = 6 then begin
      if Length(ToWrite) > 0 then
        value := Trunc(ToWrite[0]) and $FFFF
      else
        value := 0;
      Result[cntOff]   := (value shr 8) and $FF;
      Result[cntOff+1] := value and $FF;
    end else begin
      //escrita multipla: bytecount + dados.
      //multiple write: bytecount + data.
      Result[cntOff+2] := byteLen and $FF;
      if isBit then begin
        for c:=0 to size-1 do
          if (c < Length(ToWrite)) and (ToWrite[c] <> 0) then
            Result[cntOff+3+(c div 8)] := Result[cntOff+3+(c div 8)] or (1 shl (c mod 8));
      end else begin
        c := 0;
        for i:=0 to size-1 do begin
          if i < Length(ToWrite) then
            value := Trunc(ToWrite[i]) and $FFFF
          else
            value := 0;
          Result[cntOff+3+c]   := (value shr 8) and $FF;
          Result[cntOff+3+c+1] := value and $FF;
          inc(c,2);
        end;
      end;
    end;
  end;

  //bytes esperados na resposta: cabecalho + funcao + bytecount + dados.
  //bytes expected on the answer: header + function + bytecount + data.
  if not isWrite then begin
    case FFrameFormat of
      hffMbapExt: ResultLen := 9 + byteLen;
      else        ResultLen := 12 + byteLen;
    end;
  end else begin
    case FFrameFormat of
      hffMbapExt: ResultLen := 8;
      else        ResultLen := 11;
    end;
  end;

  Trace('TX',Result);
end;

function TH5UTagTCPDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
var
  i, c, c2, reqFC, respFC, len, byteLen, hdrLen, addrOff, cntOff:LongInt;
  reqIsBit:Boolean;
  dataOff:LongInt;
begin
  SetLength(values,0);

  Trace('RX',pkg.BufferToRead);

  if Length(pkg.BufferToWrite) < PFirstRequestLen then begin
    Result := ioDriverError;
    exit;
  end;

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

  case FFrameFormat of
    hffMbapExt: begin
      hdrLen  := 7;
      addrOff := 8;
      cntOff  := 12;
    end;
    else begin
      hdrLen  := 10;
      addrOff := 11;
      cntOff  := 15;
    end;
  end;

  //a resposta precisa ter pelo menos o cabecalho + funcao.
  //the answer needs at least the header + function.
  if (Result = ioOk) and (Length(pkg.BufferToRead) < hdrLen+1) then
    Result := ioCommError;

  if Result <> ioOk then
    exit;

  reqFC := pkg.BufferToWrite[hdrLen] and $FF;
  reqIsBit := reqFC in [1, 2, 5, 15];
  len := (LongInt(pkg.BufferToWrite[cntOff]) shl 8) or LongInt(pkg.BufferToWrite[cntOff+1]);

  respFC := pkg.BufferToRead[hdrLen] and $FF;

  //resposta de erro: funcao com o bit 7 ligado + codigo de excecao.
  //error answer: function with the bit 7 set + exception code.
  if respFC = ((reqFC or $80) and $FF) then begin
    if Length(pkg.BufferToRead) < hdrLen+2 then begin
      Result := ioCommError;
      exit;
    end;
    case pkg.BufferToRead[hdrLen+1] of
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
    exit;
  end;

  if respFC <> reqFC then begin
    Result := ioCommError;
    exit;
  end;

  //escrita: a resposta nao traz dados.
  //write: the answer carries no data.
  if reqFC in [5, 6, 15, 16] then
    exit;

  //leitura: bytecount + dados.
  //read: bytecount + data.
  dataOff := hdrLen + 2;
  if Length(pkg.BufferToRead) < dataOff then begin
    Result := ioCommError;
    exit;
  end;

  byteLen := pkg.BufferToRead[hdrLen+1] and $FF;
  if Length(pkg.BufferToRead) < dataOff + byteLen then begin
    Result := ioCommError;
    exit;
  end;

  if len <= 0 then
    exit;

  if reqIsBit then begin
    if byteLen < ((len + 7) div 8) then begin
      Result := ioCommError;
      exit;
    end;
    SetLength(values,len);
    c2 := 0;
    for i:=0 to len-1 do begin
      c := i div 8;
      if (LongInt(pkg.BufferToRead[dataOff+c]) and (1 shl (i mod 8))) <> 0 then
        values[i] := 1
      else
        values[i] := 0;
    end;
  end else begin
    if byteLen < len*2 then begin
      Result := ioCommError;
      exit;
    end;
    SetLength(values,len);
    c2 := 0;
    for i:=0 to len-1 do begin
      values[i] := (LongInt(pkg.BufferToRead[dataOff+c2]) shl 8) or
                    LongInt(pkg.BufferToRead[dataOff+c2+1]);
      inc(c2,2);
    end;
  end;
end;

function TH5UTagTCPDriver.ReadRemaining(buffer:BYTES):LongInt;

  function DeclaredLen:LongInt;
  var
    baseLen, lenOff:LongInt;
  begin
    Result := 0;
    case FFrameFormat of
      hffMbapExt: begin
        baseLen := 6;   //[txid][proto][len][unit] = 6 bytes antes do unit
        lenOff  := 4;
      end;
      else begin
        baseLen := 10;  //[48][5A][seq][reserved][len] = 10 bytes antes da funcao
        lenOff  := 8;
      end;
    end;
    if Length(buffer) < lenOff+2 then
      exit;
    Result := baseLen + ((LongInt(buffer[lenOff]) shl 8) or LongInt(buffer[lenOff+1]));
  end;

var
  total:LongInt;
begin
  Result := 0;
  total := DeclaredLen;
  if total > Length(buffer) then
    Result := total - Length(buffer)
  else
    Result := 0;
end;

function TH5UTagTCPDriver.RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt;
begin
  Result := ReadRemaining(buffer);
end;

function TH5UTagTCPDriver.RemainingBytesWrite(buffer:BYTES):LongInt;
begin
  //a resposta de uma escrita tambem declara o proprio tamanho no cabecalho: o
  //resto precisa ser drenado do socket, senao sobra lixo para a proxima leitura.
  //the answer of a write also declares its own size on the header: the rest has
  //to be drained from the socket, otherwise it spoils the next read.
  Result := ReadRemaining(buffer);
end;

end.
