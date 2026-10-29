{$i ../common/language.inc}
{:
  @abstract(Base unit of the Inovance (汇川) H5U tag area protocol driver.)

  The AutoShop IDE keeps every variable of a H5U program inside a flat,
  bit addressed "variable area" (变量区). The variable monitor table exported
  by AutoShop ("导出 HMI 监视变量表", the CSV that EasyBuilder, MCGS and the
  IT7000 HMIs import) lists one line per variable with the address of the
  variable inside that area, written in hexadecimal:

  @code(
    "旋转伺服操作.Jog_Vel","","Float","4","1","UW 21004F0","100ms","1",,,,"点动速度",1
    "旋转伺服操作.Jog_Forward","","Bool","1","1","UB 21004E1","100ms","1",,,,"点动进",1
  )

  UB is a bit address of the variable area, UW is the address of a word of the
  same area written in bits (word n starts at bit 16*n), so the pair
  @code(UW 21004F0) / @code(UW 2100510) of two consecutive floats are two words
  apart, exactly like the CSV shows.

  This unit implements everything protocol independent: the two devices
  (UB = bit, UW = word), the 27 bit address space (0..$4C4B400), the memory
  managers, the scan engine and the tag registration. The wire format is
  implemented by @link(H5UTagTCP.TH5UTagTCPDriver).

  @bold(How to address a tag)

  @unorderedList(
    @item(@bold(PLCStation): station (unit) number sent on the frame, usually 1.)
    @item(@bold(MemReadFunction) and @bold(MemWriteFunction): 'UB' or 'UW'.)
    @item(@bold(MemAddress): the hexadecimal number of the CSV, written as a
          decimal value: @code(UW 21004F0) is MemAddress = $21004F0.
          The driver accepts the number as it comes from AutoShop.)
  )

  Tag type -> size: a Bool tag on UB takes 1 bit, a 16 bit tag on UW takes one
  word, a 32 bit tag (Float, Int32) takes two words, a 64 bit tag four.

  @bold(Note about the wire format)

  The H5U serves this area only on the TCP port 12939 ("H5U TCP Monitor
  Protocol", the one used by the Inovance HMIs and by EasyBuilder), it is not
  part of the Modbus map of the port 502. Inovance does not publish the frame
  layout of that protocol, so @link(H5UTagTCP.TH5UTagTCPDriver) ships two
  layouts selectable at design time (@code(FrameFormat)) and can dump every
  frame to a log file (@code(TraceFile)) so the layout can be confirmed
  against a capture of the port 12939 taken from AutoShop or from an HMI.

  @seealso(H5UTagTCP.TH5UTagTCPDriver)
}

unit H5UTagDriver;

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver, ProtocolTypes, Tag, PLCTagNumber,
  PLCMemoryManager, PLCBlock, PLCString
  {$IFNDEF FPC}, Windows{$ENDIF};

const
  //: Number of memory areas (devices) handled by this driver.
  H5UTAG_DEV_COUNT = 2;

  //: MemReadFunction/MemWriteFunction of the bit area (变量区位地址).
  H5UTAG_FUNC_UB = 1;
  //: MemReadFunction/MemWriteFunction of the word area (变量区字地址).
  H5UTAG_FUNC_UW = 17;

  //: Last valid address of the variable area (bit 80000000 of the H5U).
  H5UTAG_MAX_ADDRESS = $04C4B400;

type
  {:
    Table of the memory areas. All arrays below are indexed by the device index
    (0..H5UTAG_DEV_COUNT-1), not by the MemReadFunction value.
  }
  TH5UTagDevice = (h5dUB, h5dUW);

const
  H5UTAG_DEV_FUNC: array[0..H5UTAG_DEV_COUNT-1] of LongInt = (
    H5UTAG_FUNC_UB, H5UTAG_FUNC_UW
  );

  H5UTAG_DEV_NAME: array[0..H5UTAG_DEV_COUNT-1] of string[2] = (
    'UB','UW'
  );

  H5UTAG_DEV_ISBIT: array[0..H5UTAG_DEV_COUNT-1] of Boolean = (
    True, False
  );

type

  TH5UTagStation = record
    Station: LongInt;
    //: One memory manager for each memory area of the PLC.
    Devices: array[0..H5UTAG_DEV_COUNT-1] of TPLCMemoryManager;
  end;

  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe base do driver de comunicacao por tags do H5U (汇川).)
  }
  {$ELSE}
  {:
  @abstract(Base class of the Inovance (汇川) H5U tag area drivers.)

  The descendants only translate the requests built here into bytes of the
  wire (@link(EncodePkg)) and the answer back into values (@link(DecodePkg)).
  Everything else - tag registration, scan, write queue, block building - is
  done here.

  @seealso(H5UTagTCP.TH5UTagTCPDriver)
  }
  {$ENDIF}
  TH5UTagDriver = class(TProtocolDriver)
  private
    FMustReleaseResources:Boolean;
    FWordAddressInBits:Boolean;
    PBitMaxHole:Cardinal;
    PWordMaxHole:Cardinal;
    PBitMaxBlockSize:Cardinal;
    PWordMaxBlockSize:Cardinal;
    procedure SetWordAddressInBits(v:Boolean);
    procedure SetBitMaxHole(v:Cardinal);
    procedure SetWordMaxHole(v:Cardinal);
    procedure SetBitMaxBlockSize(v:Cardinal);
    procedure SetWordMaxBlockSize(v:Cardinal);
  protected
    PFirstRequestLen,
    PFuncByteOffset,
    PCRCLen:LongInt;

    PInternalDelayBetweenCmds:Cardinal;

    //: State of every PLC (station) known by the driver.
    PH5UTagPLC:array of TH5UTagStation;

    function  GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
    procedure BuildTagRec(plc,func,startaddress,size:LongInt; out tr:TTagRec);

    {: Index in the device tables of the memory area selected by a
       MemReadFunction/MemWriteFunction value. Returns -1 when the value
       doesn't belong to this driver. }
    function  DeviceIndex(func:LongInt):LongInt;
    {: Converts the address typed on the tag into the address sent on the wire.
       By default the number is sent untouched (the variable area is bit
       addressed); with @code(WordAddressInBits) = false a UW address is
       divided by 16 and becomes a word index. }
    function  DeviceAddress(dev:LongInt; address:LongInt):LongInt;
    procedure NormalizeTagRec(var tr:TTagRec);

    {$IFDEF PORTUGUES}
    //: Codifica um pedido para o dispositivo.
    {$ELSE}
    //: Encodes a request to the device.
    {$ENDIF}
    function  EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES; virtual;
    {$IFDEF PORTUGUES}
    //: Decodifica a resposta do dispositivo.
    {$ELSE}
    //: Decodes the answer of the device.
    {$ENDIF}
    function  DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult; virtual;

    {: Bytes still pending on the RX buffer after the first read of a response
       to a read request. }
    function RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt; virtual;
    {: Bytes still pending on the RX buffer after the first read of a response
       to a write request. }
    function RemainingBytesWrite(buffer:BYTES):LongInt; virtual;

    {: Sends one read frame (a block that fits in a single frame) and stores
       the values on the memory manager of the device. }
    function SendRead(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult; virtual;
    {: Sends one write frame and stores the values on the memory manager of the
       device. }
    function SendWrite(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult; virtual;

    //: Reads a range, splitting it when it is longer than a frame.
    function ReadTagRec(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
    //: Writes a range, splitting it when it is longer than a frame.
    function WriteTagRec(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;

    //: @seealso(TProtocolDriver.DoAddTag)
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    //: @seealso(TProtocolDriver.DoDelTag)
    procedure DoDelTag(TagObj:TTag); override;
    //: @seealso(TProtocolDriver.DoScanRead)
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    //: @seealso(TProtocolDriver.DoGetValue)
    procedure DoGetValue(TagObj:TTagRec; var values:TScanReadRec); override;
    //: @seealso(TProtocolDriver.DoWrite)
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    //: @seealso(TProtocolDriver.DoRead)
    function  DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;

    {: When TRUE (default) a UW address is sent to the PLC as it is written on
       the AutoShop CSV, that is, as a bit offset of the variable area. When
       FALSE it is divided by 16 and sent as a word index. }
    property WordAddressInBits:Boolean read FWordAddressInBits write SetWordAddressInBits default true;

    {$IFDEF PORTUGUES}
    {:
    Informa quantos bits podem ficar sem serem declarados para manter um bloco
    continuo.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ELSE}
    {:
    How many bits can be undeclared to keep a continuous block.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ENDIF}
    property BitMaxHole:Cardinal read PBitMaxHole write SetBitMaxHole default 50;

    {$IFDEF PORTUGUES}
    {:
    Informa quantos words podem ficar sem serem declarados para manter um bloco
    continuo.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ELSE}
    {:
    How many words can be undeclared to keep a continuous block.
    @seealso(TPLCMemoryManager.MaxHole)
    }
    {$ENDIF}
    property WordMaxHole:Cardinal read PWordMaxHole write SetWordMaxHole default 10;

    {$IFDEF PORTUGUES}
    {:
    Informa quantos bits podem ser lidos no mesmo pacote.
    @seealso(TPLCMemoryManager.MaxBlockItems)
    }
    {$ELSE}
    {:
    How many bits can be read inside a single frame.
    @seealso(TPLCMemoryManager.MaxBlockItems)
    }
    {$ENDIF}
    property BitMaxBlockSize:Cardinal read PBitMaxBlockSize write SetBitMaxBlockSize default 1920;

    {$IFDEF PORTUGUES}
    {:
    Informa quantos words podem ser lidos no mesmo pacote.
    @seealso(TPLCMemoryManager.MaxBlockItems)
    }
    {$ELSE}
    {:
    How many words can be read inside a single frame.
    @seealso(TPLCMemoryManager.MaxBlockItems)
    }
    {$ENDIF}
    property WordMaxBlockSize:Cardinal read PWordMaxBlockSize write SetWordMaxBlockSize default 120;

  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
    //: @seealso(TProtocolDriver.SizeOfTag)
    function  SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
    //: Builds the address as it is written on the AutoShop CSV (UB 21004E0, UW 21004F0...).
    function  LiteralTagAddress(aTag:TTag; aBlockTag:TTag=nil):AnsiString; override;
  end;

implementation

uses pascalScadaMTPCPU;

constructor TH5UTagDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FProtocolReady := false;

  FWordAddressInBits := true;
  PBitMaxHole := 50;
  PWordMaxHole := 10;
  PBitMaxBlockSize := 1920;
  PWordMaxBlockSize := 120;

  PReadSomethingAlways := true;
  PInternalDelayBetweenCmds := 0;

  //o cabecalho do protocolo tem 7 bytes (6 de cabecalho + unit id) e a funcao
  //vem no oitavo byte. O descendente ajusta estes valores quando o formato de
  //quadro for outro.
  //the protocol header has 7 bytes (6 header + unit id) and the function byte
  //comes on the eighth byte. The descendant adjusts these values when the
  //frame format is different.
  PFirstRequestLen := 8;
  PFuncByteOffset := 7;
  PCRCLen := 0;

  SetLength(PH5UTagPLC,0);
end;

destructor TH5UTagDriver.Destroy;
var
  plc, dev:LongInt;
begin
  inherited Destroy;
  for plc:=0 to High(PH5UTagPLC) do
    for dev:=0 to H5UTAG_DEV_COUNT-1 do
      if Assigned(PH5UTagPLC[plc].Devices[dev]) then begin
        PH5UTagPLC[plc].Devices[dev].Destroy;
        PH5UTagPLC[plc].Devices[dev] := nil;
      end;
  SetLength(PH5UTagPLC,0);
end;

function TH5UTagDriver.DeviceIndex(func:LongInt):LongInt;
var
  c:LongInt;
begin
  Result := -1;
  for c:=0 to H5UTAG_DEV_COUNT-1 do
    if H5UTAG_DEV_FUNC[c] = func then begin
      Result := c;
      exit;
    end;
end;

function TH5UTagDriver.DeviceAddress(dev:LongInt; address:LongInt):LongInt;
begin
  Result := address;
  if (dev < 0) or (dev >= H5UTAG_DEV_COUNT) then
    exit;
  //o endereco da area de variaveis e' um deslocamento de bit: a word n comeca
  //no bit 16*n. Se o PLC esperar o indice da word na linha, divide por 16.
  //the variable area address is a bit offset: word n starts at bit 16*n.
  //When the PLC expects the word index on the wire, divide by 16.
  if (not H5UTAG_DEV_ISBIT[dev]) and (not FWordAddressInBits) then
    Result := address div 16;
end;

procedure TH5UTagDriver.NormalizeTagRec(var tr:TTagRec);
var
  dev:LongInt;
begin
  dev := DeviceIndex(tr.ReadFunction);
  if dev < 0 then
    dev := DeviceIndex(tr.WriteFunction);
  if dev < 0 then
    exit;
  tr.Address := DeviceAddress(dev, tr.Address);
end;

function TH5UTagDriver.GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
var
  found:Boolean;
begin
  found := false;
  Result := false;
  //PLCTagNumber
  if (not found) and (TagObj is TPLCTagNumber) then begin
    found := true;
    Station := TPLCTagNumber(TagObj).PLCStation;
    Address := TPLCTagNumber(TagObj).MemAddress;
    Size    := TPLCTagNumber(TagObj).TagSizeOnProtocol;
    RegType := TPLCTagNumber(TagObj).MemReadFunction;
    ScanTime:= TPLCTagNumber(TagObj).RefreshTime;
    Result  := found;
  end;

  //TPLCBlock and TPLCStruct
  if (not found) and (TagObj is TPLCBlock) then begin
    found   := true;
    Station := TPLCBlock(TagObj).PLCStation;
    Address := TPLCBlock(TagObj).MemAddress;
    Size    := TPLCBlock(TagObj).TagSizeOnProtocol;
    RegType := TPLCBlock(TagObj).MemReadFunction;
    ScanTime:= TPLCBlock(TagObj).RefreshTime;
    Result  := found;
  end;

  //TPLCString
  if (not found) and (TagObj is TPLCString) then begin
    found   := true;
    Station := TPLCString(TagObj).PLCStation;
    Address := TPLCString(TagObj).MemAddress;
    Size    := TPLCString(TagObj).Size;
    RegType := TPLCString(TagObj).MemReadFunction;
    ScanTime:= TPLCString(TagObj).RefreshTime;
    Result  := found;
  end;
end;

procedure TH5UTagDriver.BuildTagRec(plc, func, startaddress, size: LongInt; out tr: TTagRec);
begin
  with tr do begin
    ID := 0;
    Station := plc;
    Rack := 0;
    Slot := 0;
    Address := startaddress;
    ReadFunction := func;
    WriteFunction := func;
    OffSet := 0;
    File_DB := 0;
    SubElement := 0;
    Count := 0;
    RealOffset := 0;
    Retries := 0;
    UpdateTime := 0;
    Path := '';
    CallBack := nil;
  end;
  tr.Size := size;
end;

procedure TH5UTagDriver.SetWordAddressInBits(v:Boolean);
begin
  //a conversao acontece quando o tag e' registrado: mude antes de ligar os tags.
  //the conversion happens when the tag is registered: change it before linking
  //the tags to the driver.
  FWordAddressInBits := v;
end;

procedure TH5UTagDriver.SetBitMaxHole(v:Cardinal);
var
  plc:LongInt;
begin
  if v = PBitMaxHole then
    exit;
  PBitMaxHole := v;
  for plc:=0 to High(PH5UTagPLC) do
    if Assigned(PH5UTagPLC[plc].Devices[Ord(h5dUB)]) then
      PH5UTagPLC[plc].Devices[Ord(h5dUB)].MaxHole := v;
end;

procedure TH5UTagDriver.SetWordMaxHole(v:Cardinal);
var
  plc:LongInt;
begin
  if v = PWordMaxHole then
    exit;
  PWordMaxHole := v;
  for plc:=0 to High(PH5UTagPLC) do
    if Assigned(PH5UTagPLC[plc].Devices[Ord(h5dUW)]) then
      PH5UTagPLC[plc].Devices[Ord(h5dUW)].MaxHole := v;
end;

procedure TH5UTagDriver.SetBitMaxBlockSize(v:Cardinal);
var
  plc:LongInt;
begin
  if v = PBitMaxBlockSize then
    exit;
  PBitMaxBlockSize := v;
  for plc:=0 to High(PH5UTagPLC) do
    if Assigned(PH5UTagPLC[plc].Devices[Ord(h5dUB)]) then
      PH5UTagPLC[plc].Devices[Ord(h5dUB)].MaxBlockItems := v;
end;

procedure TH5UTagDriver.SetWordMaxBlockSize(v:Cardinal);
var
  plc:LongInt;
begin
  if v = PWordMaxBlockSize then
    exit;
  PWordMaxBlockSize := v;
  for plc:=0 to High(PH5UTagPLC) do
    if Assigned(PH5UTagPLC[plc].Devices[Ord(h5dUW)]) then
      PH5UTagPLC[plc].Devices[Ord(h5dUW)].MaxBlockItems := v;
end;

procedure TH5UTagDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
var
  station, mem, size, memtype, scantime:LongInt;
  found, valido:Boolean;
  plc, dev, c:LongInt;
begin
  station := 0;
  mem := 0;
  size := 0;
  memtype := 0;
  scantime := 0;
  valido := false;

  found := GetTagProperts(TagObj,station,mem,size,memtype,scantime);

  if found then
    if (station >= 0) and (station <= 255) then begin
      dev := DeviceIndex(memtype);
      if (dev >= 0) and (mem >= 0) and (mem <= H5UTAG_MAX_ADDRESS) then begin
        found := false;
        for plc:=0 to High(PH5UTagPLC) do
          if PH5UTagPLC[plc].Station = station then begin
            found := true;
            break;
          end;

        //if the slave was not found, add it.
        if not found then begin
          plc := Length(PH5UTagPLC);
          SetLength(PH5UTagPLC,plc+1);
          PH5UTagPLC[plc].Station := station;
          for c:=0 to H5UTAG_DEV_COUNT-1 do begin
            PH5UTagPLC[plc].Devices[c] := TPLCMemoryManager.Create;
            if H5UTAG_DEV_ISBIT[c] then begin
              PH5UTagPLC[plc].Devices[c].MaxBlockItems := PBitMaxBlockSize;
              PH5UTagPLC[plc].Devices[c].MaxHole := PBitMaxHole;
            end else begin
              PH5UTagPLC[plc].Devices[c].MaxBlockItems := PWordMaxBlockSize;
              PH5UTagPLC[plc].Devices[c].MaxHole := PWordMaxHole;
            end;
          end;
        end;

        valido := (size > 0);
        if valido then
          PH5UTagPLC[plc].Devices[dev].AddAddress(DeviceAddress(dev,mem),size,1,scantime);
      end;
    end;
  inherited DoAddTag(TagObj, valido);
end;

procedure TH5UTagDriver.DoDelTag(TagObj:TTag);
var
  station, mem, size, memtype, scantime:LongInt;
  found:Boolean;
  plc, dev:LongInt;
begin
  station := 0;
  mem := 0;
  size := 0;
  memtype := 0;
  scantime := 0;
  found := GetTagProperts(TagObj,station,mem,size,memtype,scantime);

  if found then
    if (station >= 0) and (station <= 255) then begin
      dev := DeviceIndex(memtype);
      found := false;
      for plc:=0 to High(PH5UTagPLC) do
        if PH5UTagPLC[plc].Station = station then begin
          found := true;
          break;
        end;

      if found and (dev >= 0) then
        PH5UTagPLC[plc].Devices[dev].RemoveAddress(DeviceAddress(dev,mem),size,1);
    end;
  inherited DoDelTag(TagObj);
end;

procedure TH5UTagDriver.DoGetValue(TagObj:TTagRec; var values:TScanReadRec);
var
  plc, c, dev:LongInt;
  found:Boolean;
begin
  if Length(values.Values)<TagObj.Size then
    SetLength(values.Values,TagObj.Size);

  for c:=0 to Length(values.Values)-1 do
    values.Values[c] := 0;

  dev := DeviceIndex(TagObj.ReadFunction);
  found := false;
  if dev >= 0 then
    for plc:=0 to High(PH5UTagPLC) do
      if PH5UTagPLC[plc].Station = TagObj.Station then begin
        found := true;
        break;
      end;

  if not found then begin
    values.ClkMonotonicTStamp := GetTickCount64;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
    values.LastQueryResult := ioDriverError;
    SetLength(values.Values,0);
    exit;
  end;

  PH5UTagPLC[plc].Devices[dev].GetValues(DeviceAddress(dev,TagObj.Address),TagObj.Size,1,
                                        values.Values, values.LastQueryResult,
                                        values.ClkMonotonicTStamp);

  if values.LastQueryResult=ioOk then begin
    values.ReadsOK := 1;
    values.ReadFaults := 0;
  end else begin
    values.ReadsOK := 0;
    values.ReadFaults := 1;
  end;
end;

function TH5UTagDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
var
  FunctionCode:Cardinal;
  dev:LongInt;
begin
  FunctionCode := 0;
  if (aTag is TPLCTagNumber) then begin
    if (isWrite) then
      FunctionCode := TPLCTagNumber(aTag).MemWriteFunction
    else
      FunctionCode := TPLCTagNumber(aTag).MemReadFunction;
  end;

  if (aTag is TPLCBlock) then begin
    if (isWrite) then
      FunctionCode := TPLCBlock(aTag).MemWriteFunction
    else
      FunctionCode := TPLCBlock(aTag).MemReadFunction;
  end;

  if (aTag is TPLCString) then begin
    if (isWrite) then
      FunctionCode := TPLCString(aTag).MemWriteFunction
    else
      FunctionCode := TPLCString(aTag).MemReadFunction;
  end;

  //retorna o tamanho em bits de cada tipo de elemento de memoria.
  //returns the size in bits of the element read/written by each device.
  dev := DeviceIndex(FunctionCode);
  if (dev >= 0) and (not H5UTAG_DEV_ISBIT[dev]) then begin
    Result := 16;
    ProtocolTagType := ptWord;
  end else begin
    Result := 1;
    ProtocolTagType := ptBit;
  end;
end;

function TH5UTagDriver.LiteralTagAddress(aTag:TTag; aBlockTag:TTag):AnsiString;
var
  func:LongInt;
  address:Cardinal;
  dev:LongInt;
begin
  Result := '';
  func := 0;
  address := 0;
  if aTag is TPLCTagNumber then begin
    func := TPLCTagNumber(aTag).MemReadFunction;
    address := TPLCTagNumber(aTag).MemAddress;
  end;
  if aTag is TPLCBlock then begin
    func := TPLCBlock(aTag).MemReadFunction;
    address := TPLCBlock(aTag).MemAddress;
  end;
  if aTag is TPLCString then begin
    func := TPLCString(aTag).MemReadFunction;
    address := TPLCString(aTag).MemAddress;
  end;

  dev := DeviceIndex(func);
  if dev >= 0 then
    //escrito como no CSV do AutoShop: UB 21004E0 / UW 21004F0.
    //written like the AutoShop CSV: UB 21004E0 / UW 21004F0.
    Result := H5UTAG_DEV_NAME[dev] + ' ' + IntToHex(address,1)
  else
    Result := inherited LiteralTagAddress(aTag, aBlockTag);
end;

function TH5UTagDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES;
begin
  ResultLen := 0;
  Result := nil;
end;

function TH5UTagDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
begin
  values := nil;
  Result := ioDriverError;
end;

function TH5UTagDriver.RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt;
begin
  Result := 0;
end;

function TH5UTagDriver.RemainingBytesWrite(buffer:BYTES):LongInt;
begin
  Result := 0;
end;

function TH5UTagDriver.SendRead(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
var
  IOResult1, IOResult2:TIOPacket;
  FRemainingBytes:LongInt;
  pkg:BYTES;
  rl, res:LongInt;
  starts, ends:TNotifyEvent;
  locked:Boolean;
begin
  SetLength(Values,0);
  Result := ioNullDriver;
  locked := false;
  try
    if FMustReleaseResources then begin
      starts := @HighLatencyOperationWillBegin;
      ends   := @HighLatencyOperationWasEnded;
    end else begin
      starts := nil;
      ends   := nil;
    end;

    pkg := EncodePkg(tr,nil,rl);
    if Length(pkg)=0 then begin
      Result := ioIllegalFunction;
      exit;
    end;

    if (PCommPort<>nil) and PCommPort.ReallyActive then begin
      PCommPort.Lock(DriverID);
      locked := true;
      res := PCommPort.IOCommandSync(iocWriteRead,Length(pkg),pkg,PFirstRequestLen,DriverID,PInternalDelayBetweenCmds,@IOResult1,starts,ends);

      //if the IO result is OK, reads the remaining packet...
      if (res<>0) and (IOResult1.ReadIOResult=iorOK) then begin
        FRemainingBytes := RemainingBytesRead(IOResult1.BufferToRead, tr);

        if FRemainingBytes>0 then begin
          res := PCommPort.IOCommandSync(iocRead,0,nil,FRemainingBytes,DriverID,0,@IOResult2,starts,ends);
          if res<>0 then begin
            IOResult1.BufferToRead := ConcatenateBYTES(IOResult1.BufferToRead, IOResult2.BufferToRead);
            IOResult1.Received := IOResult1.Received + IOResult2.Received;
            if IOResult2.ReadIOResult<>iorOK then
              IOResult1.ReadIOResult := IOResult2.ReadIOResult;
          end else
            IOResult1.ReadIOResult := iorNotReady;
        end;
        Result := DecodePkg(IOResult1,Values);
      end else
        Result := ioEmptyPacket;
    end;
  finally
    if locked then
      PCommPort.Unlock(DriverID);
    SetLength(pkg,0);
    SetLength(IOResult1.BufferToRead,0);
    SetLength(IOResult1.BufferToWrite,0);
    SetLength(IOResult2.BufferToRead,0);
    SetLength(IOResult2.BufferToWrite,0);
  end;
end;

function TH5UTagDriver.SendWrite(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
var
  IOResult1, IOResult2:TIOPacket;
  FRemainingBytes:LongInt;
  pkg:BYTES;
  rl, res:LongInt;
  tempValues:TArrayOfDouble;
  locked:Boolean;
begin
  Result := ioNullDriver;
  locked := false;
  try
    pkg := EncodePkg(tr,Values,rl);
    if Length(pkg)=0 then begin
      Result := ioIllegalFunction;
      exit;
    end;

    if (PCommPort<>nil) and PCommPort.ReallyActive then begin
      PCommPort.Lock(DriverID);
      locked := true;
      res := PCommPort.IOCommandSync(iocWriteRead,Length(pkg),pkg,PFirstRequestLen,DriverID,PInternalDelayBetweenCmds,@IOResult1);

      if (res<>0) and (IOResult1.ReadIOResult=iorOK) then begin
        FRemainingBytes := RemainingBytesWrite(IOResult1.BufferToRead);
        if FRemainingBytes>0 then begin
          res := PCommPort.IOCommandSync(iocRead,0,nil,FRemainingBytes,DriverID,0,@IOResult2);
          if res<>0 then begin
            IOResult1.BufferToRead := ConcatenateBYTES(IOResult1.BufferToRead, IOResult2.BufferToRead);
            IOResult1.Received := IOResult1.Received + IOResult2.Received;
            if IOResult2.ReadIOResult<>iorOK then
              IOResult1.ReadIOResult := IOResult2.ReadIOResult;
          end else
            IOResult1.ReadIOResult := iorNotReady;
        end;
        Result := DecodePkg(IOResult1,tempValues);
      end else
        Result := ioEmptyPacket;
    end;
  finally
    if locked then
      PCommPort.Unlock(DriverID);
    SetLength(pkg,0);
    SetLength(tempValues,0);
    SetLength(IOResult1.BufferToRead,0);
    SetLength(IOResult1.BufferToWrite,0);
    SetLength(IOResult2.BufferToRead,0);
    SetLength(IOResult2.BufferToWrite,0);
  end;
end;

function TH5UTagDriver.ReadTagRec(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
var
  dev, segLen, done, c, base, maxSeg:LongInt;
  seg:TTagRec;
  part:TArrayOfDouble;
begin
  SetLength(Values,0);
  Result := ioOk;

  dev := DeviceIndex(tr.ReadFunction);
  if dev < 0 then begin
    Result := ioIllegalFunction;
    exit;
  end;

  if tr.Size <= 0 then
    exit;

  SetLength(Values,tr.Size);
  for c:=0 to High(Values) do
    Values[c] := 0;

  //um pedido nao pode passar do tamanho maximo de um quadro.
  //a single request can't be longer than a frame.
  if H5UTAG_DEV_ISBIT[dev] then
    maxSeg := PBitMaxBlockSize
  else
    maxSeg := PWordMaxBlockSize;
  if maxSeg <= 0 then
    maxSeg := tr.Size;

  done := 0;
  while done < tr.Size do begin
    base := tr.Address + done;
    segLen := tr.Size - done;
    if segLen > maxSeg then
      segLen := maxSeg;
    if segLen <= 0 then
      segLen := tr.Size - done;

    seg := tr;
    seg.Address := base;
    seg.Size := segLen;

    part := nil;
    Result := SendRead(seg,part);
    if Result <> ioOk then
      break;
    if Length(part) < segLen then begin
      Result := ioCommError;
      break;
    end;

    for c:=0 to segLen-1 do
      Values[done+c] := part[c];

    inc(done,segLen);
  end;
end;

function TH5UTagDriver.WriteTagRec(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
var
  dev, segLen, done, c, base, available, maxSeg:LongInt;
  seg:TTagRec;
  part:TArrayOfDouble;
begin
  Result := ioOk;

  dev := DeviceIndex(tr.WriteFunction);
  if dev < 0 then begin
    Result := ioIllegalFunction;
    exit;
  end;

  available := Length(Values);
  if (tr.Size <= 0) or (available <= 0) then
    exit;

  if available > tr.Size then
    available := tr.Size;

  if H5UTAG_DEV_ISBIT[dev] then
    maxSeg := PBitMaxBlockSize
  else
    maxSeg := PWordMaxBlockSize;
  if maxSeg <= 0 then
    maxSeg := available;

  done := 0;
  while done < available do begin
    base := tr.Address + done;
    segLen := available - done;
    if segLen > maxSeg then
      segLen := maxSeg;
    if segLen <= 0 then
      segLen := available - done;

    seg := tr;
    seg.Address := base;
    seg.Size := segLen;

    SetLength(part,segLen);
    for c:=0 to segLen-1 do
      part[c] := Values[done+c];

    Result := SendWrite(seg,part);
    if Result <> ioOk then
      break;

    inc(done,segLen);
  end;
end;

function TH5UTagDriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  tr:TTagRec;
begin
  tr := tagrec;
  NormalizeTagRec(tr);
  Result := ReadTagRec(tr,Values);
end;

function TH5UTagDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  tr:TTagRec;
begin
  tr := tagrec;
  NormalizeTagRec(tr);
  Result := WriteTagRec(tr,Values);
end;

procedure TH5UTagDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
var
  plc, block, dev:LongInt;
  found, first:Boolean;
  minScan:Int64;
  lastPLC, lastAddr, lastSize, lastFunc:LongInt;
  tr:TTagRec;
  values:TArrayOfDouble;
begin
  try
    if ([csDestroying]*ComponentState<>[]) then begin
      CrossThreadSwitch;
      exit;
    end;

    //avoid high cpu consumption with tags linked and without a communication port.
    if (not Assigned(PCommPort)) or (PCommPort.ReallyActive=false) then begin
      NeedSleep := 1;
      exit;
    end;

    found := false;
    first := true;
    minScan := 0;
    lastPLC := 0;
    lastAddr := 0;
    lastSize := 0;
    lastFunc := 0;

    for plc:=0 to High(PH5UTagPLC) do
      for dev:=0 to H5UTAG_DEV_COUNT-1 do
        if Assigned(PH5UTagPLC[plc].Devices[dev]) then
          for block:=0 to High(PH5UTagPLC[plc].Devices[dev].Blocks) do
            if PH5UTagPLC[plc].Devices[dev].Blocks[block].NeedRefresh then begin
              found := true;
              BuildTagRec(PH5UTagPLC[plc].Station, H5UTAG_DEV_FUNC[dev],
                          PH5UTagPLC[plc].Devices[dev].Blocks[block].AddressStart,
                          PH5UTagPLC[plc].Devices[dev].Blocks[block].Size, tr);
              FMustReleaseResources := true;
              ReadTagRec(tr,values);
              FMustReleaseResources := false;
            end else begin
              if first then begin
                first := false;
                minScan := PH5UTagPLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate;
                lastPLC := plc;
                lastAddr := PH5UTagPLC[plc].Devices[dev].Blocks[block].AddressStart;
                lastSize := PH5UTagPLC[plc].Devices[dev].Blocks[block].Size;
                lastFunc := H5UTAG_DEV_FUNC[dev];
              end else
                if PH5UTagPLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate > minScan then begin
                  minScan := PH5UTagPLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate;
                  lastPLC := plc;
                  lastAddr := PH5UTagPLC[plc].Devices[dev].Blocks[block].AddressStart;
                  lastSize := PH5UTagPLC[plc].Devices[dev].Blocks[block].Size;
                  lastFunc := H5UTAG_DEV_FUNC[dev];
                end;
            end;

    //nothing is due: read the block that is waiting for the longest time to
    //keep the link alive.
    if (not found) then
      if (not first) and PReadSomethingAlways then begin
        BuildTagRec(PH5UTagPLC[lastPLC].Station, lastFunc, lastAddr, lastSize, tr);
        FMustReleaseResources := true;
        ReadTagRec(tr,values);
        FMustReleaseResources := false;
      end else
        NeedSleep := 1;
  finally
    FProtocolReady := true;
    SetLength(values,0);
  end;
end;

end.
