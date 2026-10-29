{$i ../common/language.inc}
{:
  @abstract(Base unit of the Xinje (信捷) PLC protocol driver family.)

  This unit holds everything that is protocol independent: the device table
  (soft element -> data code), the memory managers, the scan engine and the
  block splitting rules. The wire format is implemented by the descendants
  (see @link(XinJETCP.TXinJETCPDriver)).

  @bold(How to address a tag)

  A tag is configured through the properties of @link(TPLCTagNumber):

  @unorderedList(
    @item(@bold(PLCStation): station number of the PLC (1..255).)
    @item(@bold(MemReadFunction) and @bold(MemWriteFunction): the soft element
          (device) that will be read/written. Both must be set to the same
          value.)
    @item(@bold(MemAddress): the address inside the soft element, written the
          same way you write it on XDPPro.)
  )

  @table(
    @rowHead( @cell(Soft element) @cell(MemReadFunction/MemWriteFunction) @cell(MemAddress) )
    @row(     @cell(M)             @cell(1)   @cell(M0, M100, ...) )
    @row(     @cell(X)             @cell(2)   @cell(X0, X10, ... - octal) )
    @row(     @cell(Y)             @cell(3)   @cell(Y0, Y10, ... - octal) )
    @row(     @cell(S)             @cell(4)   @cell(S0, S100, ...) )
    @row(     @cell(SM)            @cell(5)   @cell(SM0, SM3, ...) )
    @row(     @cell(T)             @cell(6)   @cell(T0, T100, ... - bit) )
    @row(     @cell(C)             @cell(7)   @cell(C0, C100, ... - bit) )
    @row(     @cell(ET)            @cell(8)   @cell(ET0, ...) )
    @row(     @cell(HM)            @cell(9)   @cell(HM0, ...) )
    @row(     @cell(HS)            @cell(10)  @cell(HS0, ...) )
    @row(     @cell(HT)            @cell(11)  @cell(HT0, ...) )
    @row(     @cell(HC)            @cell(12)  @cell(HC0, ...) )
    @row(     @cell(HSC)           @cell(13)  @cell(HSC0, ...) )
    @row(     @cell(D)             @cell(17)  @cell(D0, D100, ...) )
    @row(     @cell(SD)            @cell(18)  @cell(SD0, ...) )
    @row(     @cell(TD)            @cell(19)  @cell(TD0, ... - word) )
    @row(     @cell(CD)            @cell(20)  @cell(CD0, ... - word) )
    @row(     @cell(HD)            @cell(21)  @cell(HD0, ...) )
    @row(     @cell(FD)            @cell(22)  @cell(FD0, ...) )
    @row(     @cell(ID)            @cell(23)  @cell(ID0, ...) )
    @row(     @cell(QD)            @cell(24)  @cell(QD0, ...) )
    @row(     @cell(ETD)           @cell(25)  @cell(ETD0, ...) )
    @row(     @cell(HSD)           @cell(26)  @cell(HSD0, ...) )
    @row(     @cell(HTD)           @cell(27)  @cell(HTD0, ...) )
    @row(     @cell(HCD)           @cell(28)  @cell(HCD0, ...) )
    @row(     @cell(HSCD)          @cell(29)  @cell(HSCD0, ...) )
    @row(     @cell(SFD)           @cell(30)  @cell(SFD0, ...) )
  )

  X and Y are numbered in octal by the PLC itself: X10 means the eleventh
  input. Because MemAddress is a number, the driver reads its digits as octal
  digits (X10 -> 10 -> 8) when IOAddressFormat is xiofOctal, which is the
  default. Set IOAddressFormat to xiofDecimal if you want to type the plain
  bit index instead.
}

unit XinJEDriver;

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver, ProtocolTypes, Tag, PLCTagNumber,
  PLCMemoryManager, PLCBlock, PLCString
  {$IFNDEF FPC}, Windows{$ENDIF};

const
  //: Number of soft elements (memory areas) handled by this driver family.
  XINJE_DEV_COUNT = 27;

  //: MemReadFunction/MemWriteFunction of the bit devices.
  XINJE_FUNC_M    = 1;
  XINJE_FUNC_X    = 2;
  XINJE_FUNC_Y    = 3;
  XINJE_FUNC_S    = 4;
  XINJE_FUNC_SM   = 5;
  XINJE_FUNC_T    = 6;
  XINJE_FUNC_C    = 7;
  XINJE_FUNC_ET   = 8;
  XINJE_FUNC_HM   = 9;
  XINJE_FUNC_HS   = 10;
  XINJE_FUNC_HT   = 11;
  XINJE_FUNC_HC   = 12;
  XINJE_FUNC_HSC  = 13;

  //: MemReadFunction/MemWriteFunction of the word devices.
  XINJE_FUNC_D    = 17;
  XINJE_FUNC_SD   = 18;
  XINJE_FUNC_TD   = 19;
  XINJE_FUNC_CD   = 20;
  XINJE_FUNC_HD   = 21;
  XINJE_FUNC_FD   = 22;
  XINJE_FUNC_ID   = 23;
  XINJE_FUNC_QD   = 24;
  XINJE_FUNC_ETD  = 25;
  XINJE_FUNC_HSD  = 26;
  XINJE_FUNC_HTD  = 27;
  XINJE_FUNC_HCD  = 28;
  XINJE_FUNC_HSCD = 29;
  XINJE_FUNC_SFD  = 30;

  //: Command bytes of the Xinje internal protocol (the modbus function code
  //: position of the frame).
  XINJE_CMD_READ_BITS   = $1E;
  XINJE_CMD_WRITE_BITS  = $1F;
  XINJE_CMD_READ_WORDS  = $20;
  XINJE_CMD_WRITE_WORDS = $21;

  //: Size of the frame header added by the TCP transport (message id,
  //: protocol id and length).
  XINJE_TCP_HEADER_LEN = 6;

{:
  Table of the soft elements. All the arrays below are indexed by the device
  index (0..XINJE_DEV_COUNT-1), not by the function code.
}
const
  XINJE_DEV_FUNC: array[0..XINJE_DEV_COUNT-1] of LongInt = (
    XINJE_FUNC_M,  XINJE_FUNC_X,  XINJE_FUNC_Y,  XINJE_FUNC_S,  XINJE_FUNC_SM,
    XINJE_FUNC_T,  XINJE_FUNC_C,  XINJE_FUNC_ET, XINJE_FUNC_HM, XINJE_FUNC_HS,
    XINJE_FUNC_HT, XINJE_FUNC_HC, XINJE_FUNC_HSC,
    XINJE_FUNC_D,  XINJE_FUNC_SD, XINJE_FUNC_TD, XINJE_FUNC_CD, XINJE_FUNC_HD,
    XINJE_FUNC_FD, XINJE_FUNC_ID, XINJE_FUNC_QD, XINJE_FUNC_ETD, XINJE_FUNC_HSD,
    XINJE_FUNC_HTD, XINJE_FUNC_HCD, XINJE_FUNC_HSCD, XINJE_FUNC_SFD
  );

  XINJE_DEV_NAME: array[0..XINJE_DEV_COUNT-1] of string[4] = (
    'M','X','Y','S','SM','T','C','ET','HM','HS','HT','HC','HSC',
    'D','SD','TD','CD','HD','FD','ID','QD','ETD','HSD','HTD','HCD','HSCD','SFD'
  );

  XINJE_DEV_DATACODE: array[0..XINJE_DEV_COUNT-1] of Byte = (
    3, 1, 2, 4, 13, 5, 6, 7, 8, 9, 10, 11, 12,
    128, 131, 129, 130, 136, 141, 134, 135, 133, 140, 137, 138, 139, 142
  );

  XINJE_DEV_ISBIT: array[0..XINJE_DEV_COUNT-1] of Boolean = (
    True, True, True, True, True, True, True, True, True, True, True, True, True,
    False, False, False, False, False, False, False, False, False, False, False,
    False, False, False
  );

  {: Addresses above this limit belong to the extension of the address range of
     the soft element: a single frame must not cross it, so long blocks are
     split in two requests. Zero means "the whole range is contiguous". }
  XINJE_DEV_CRITICAL: array[0..XINJE_DEV_COUNT-1] of LongInt = (
    20480, 0, 0, 8000, 4096, 4096, 4096, 0, 6144, 0, 1024, 1024, 0,
    20480, 4096, 4096, 4096, 6144, 8192, 0, 0, 0, 1024, 1024, 1024, 0, 4096
  );

type

  {: How the digits typed on MemAddress are understood for X and Y. }
  TXinJEIOFormat = (xiofOctal, xiofDecimal);

  TXinJEStation = record
    Station: LongInt;
    //: One memory manager for each soft element of the PLC.
    Devices: array[0..XINJE_DEV_COUNT-1] of TPLCMemoryManager;
  end;

  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe base dos drivers do protocolo Xinje (信捷).)
  }
  {$ELSE}
  {:
  @abstract(Base class of the Xinje (信捷) protocol drivers.)

  The descendants only have to translate the requests built here into bytes of
  the wire (@link(EncodePkg)) and the answer back into values
  (@link(DecodePkg)). Everything else - tag registration, scan, write queue,
  address translation - is done here.

  @seealso(XinJETCP.TXinJETCPDriver)
  }
  {$ENDIF}
  TXinJEDriver = class(TProtocolDriver)
  private
    FMustReleaseResources:Boolean;
    FIOAddressFormat:TXinJEIOFormat;
    PBitMaxHole:Cardinal;
    PWordMaxHole:Cardinal;
    PBitMaxBlockSize:Cardinal;
    PWordMaxBlockSize:Cardinal;
    procedure SetIOAddressFormat(v:TXinJEIOFormat);
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
    PXinJEPLC:array of TXinJEStation;

    function  GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
    procedure BuildTagRec(plc,func,startaddress,size:LongInt; out tr:TTagRec);

    {: Index in the device tables of the soft element selected by a
       MemReadFunction/MemWriteFunction value. Returns -1 when the function
       code doesn't belong to the Xinje family. }
    function  DeviceIndex(func:LongInt):LongInt;
    {: Index in the device tables of the soft element that owns a data code.
       Returns -1 when the code is unknown. }
    function  DeviceIndexByDataCode(code:LongInt):LongInt;
    {: Converts the address typed on the tag (tag address space) into the
       address used on the wire. Only X/Y need a translation (octal). }
    function  DeviceAddress(dev:LongInt; address:LongInt):LongInt;
    {: Converts a tag record from the tag address space into the wire address
       space. }
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

    {: Reads a range, splitting it at the critical address of the device when
       needed. Addresses must be already in the wire address space. }
    function ReadTagRec(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
    {: Writes a range, splitting it at the critical address of the device when
       needed. Addresses must be already in the wire address space. }
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

    {: How the digits typed on MemAddress are read for the X and Y soft
       elements. Xinje numbers them in octal, so X10 with xiofOctal reaches the
       bit 8 of the PLC, exactly like the address written on the ladder. }
    property IOAddressFormat:TXinJEIOFormat read FIOAddressFormat write SetIOAddressFormat default xiofOctal;

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
    //: Builds the address as it is written on the PLC program (D100, X10...).
    function  LiteralTagAddress(aTag:TTag; aBlockTag:TTag=nil):AnsiString; override;
  end;

implementation

uses pascalScadaMTPCPU;

{ Returns the value of the digits of Address read as octal digits. When the
  number has a digit that doesn't exist in octal the number is returned
  untouched, so a tag written in decimal still reaches the address it says. }
function OctalAddress(address:LongInt):LongInt;
var
  s:AnsiString;
  i, d, v:LongInt;
begin
  Result := address;
  if address <= 0 then
    exit;
  s := IntToStr(address);
  v := 0;
  for i:=1 to Length(s) do begin
    d := Ord(s[i]) - Ord('0');
    if (d < 0) or (d > 7) then
      exit;
    v := (v * 8) + d;
  end;
  Result := v;
end;

constructor TXinJEDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FProtocolReady := false;

  FIOAddressFormat := xiofOctal;
  PBitMaxHole := 50;
  PWordMaxHole := 10;
  PBitMaxBlockSize := 1920;
  PWordMaxBlockSize := 120;

  PReadSomethingAlways := true;
  PInternalDelayBetweenCmds := 0;
  //o driver le primeiro o cabecalho de 6 bytes mais station e comando (8) e
  //depois o resto, cujo tamanho vem declarado no cabecalho. Assim uma resposta
  //de escrita minima (8 bytes) nao fica esperando bytes que nunca vem.
  //the driver reads first the 6 byte header plus station and command (8) and
  //then the rest, whose size is declared on the header. That way a minimal
  //write answer (8 bytes) doesn't wait for bytes that never arrive.
  PFirstRequestLen := 8;
  PFuncByteOffset := 7;
  PCRCLen := 0;

  SetLength(PXinJEPLC,0);
end;

destructor TXinJEDriver.Destroy;
var
  plc, dev:LongInt;
begin
  inherited Destroy;
  for plc:=0 to High(PXinJEPLC) do
    for dev:=0 to XINJE_DEV_COUNT-1 do
      if Assigned(PXinJEPLC[plc].Devices[dev]) then begin
        PXinJEPLC[plc].Devices[dev].Destroy;
        PXinJEPLC[plc].Devices[dev] := nil;
      end;
  SetLength(PXinJEPLC,0);
end;

function TXinJEDriver.DeviceIndex(func:LongInt):LongInt;
var
  c:LongInt;
begin
  Result := -1;
  for c:=0 to XINJE_DEV_COUNT-1 do
    if XINJE_DEV_FUNC[c] = func then begin
      Result := c;
      exit;
    end;
end;

function TXinJEDriver.DeviceIndexByDataCode(code:LongInt):LongInt;
var
  c:LongInt;
begin
  Result := -1;
  for c:=0 to XINJE_DEV_COUNT-1 do
    if XINJE_DEV_DATACODE[c] = code then begin
      Result := c;
      exit;
    end;
end;

function TXinJEDriver.DeviceAddress(dev:LongInt; address:LongInt):LongInt;
begin
  Result := address;
  if (dev < 0) or (dev >= XINJE_DEV_COUNT) then
    exit;
  if (FIOAddressFormat = xiofOctal) and
     ((XINJE_DEV_FUNC[dev] = XINJE_FUNC_X) or (XINJE_DEV_FUNC[dev] = XINJE_FUNC_Y)) then
    Result := OctalAddress(address);
end;

procedure TXinJEDriver.NormalizeTagRec(var tr:TTagRec);
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

function TXinJEDriver.GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
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

procedure TXinJEDriver.BuildTagRec(plc, func, startaddress, size: LongInt; out tr: TTagRec);
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

procedure TXinJEDriver.SetIOAddressFormat(v:TXinJEIOFormat);
begin
  //a conversao acontece quando o tag e' registrado: mude o formato antes de
  //ligar os tags ao driver.
  //the conversion happens when the tag is registered: change the format before
  //linking the tags to the driver.
  FIOAddressFormat := v;
end;

procedure TXinJEDriver.SetBitMaxHole(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PBitMaxHole then
    exit;
  PBitMaxHole := v;
  for plc:=0 to High(PXinJEPLC) do
    for dev:=0 to XINJE_DEV_COUNT-1 do
      if Assigned(PXinJEPLC[plc].Devices[dev]) and XINJE_DEV_ISBIT[dev] then
        PXinJEPLC[plc].Devices[dev].MaxHole := v;
end;

procedure TXinJEDriver.SetWordMaxHole(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PWordMaxHole then
    exit;
  PWordMaxHole := v;
  for plc:=0 to High(PXinJEPLC) do
    for dev:=0 to XINJE_DEV_COUNT-1 do
      if Assigned(PXinJEPLC[plc].Devices[dev]) and (not XINJE_DEV_ISBIT[dev]) then
        PXinJEPLC[plc].Devices[dev].MaxHole := v;
end;

procedure TXinJEDriver.SetBitMaxBlockSize(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PBitMaxBlockSize then
    exit;
  PBitMaxBlockSize := v;
  for plc:=0 to High(PXinJEPLC) do
    for dev:=0 to XINJE_DEV_COUNT-1 do
      if Assigned(PXinJEPLC[plc].Devices[dev]) and XINJE_DEV_ISBIT[dev] then
        PXinJEPLC[plc].Devices[dev].MaxBlockItems := v;
end;

procedure TXinJEDriver.SetWordMaxBlockSize(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PWordMaxBlockSize then
    exit;
  PWordMaxBlockSize := v;
  for plc:=0 to High(PXinJEPLC) do
    for dev:=0 to XINJE_DEV_COUNT-1 do
      if Assigned(PXinJEPLC[plc].Devices[dev]) and (not XINJE_DEV_ISBIT[dev]) then
        PXinJEPLC[plc].Devices[dev].MaxBlockItems := v;
end;

procedure TXinJEDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
var
  station, mem, size, memtype, scantime:LongInt;
  found, valido:Boolean;
  plc, dev, c:LongInt;
begin
  //Recupera as informacoes do tag;
  //retrieve informations of the tag.
  station := 0;
  mem := 0;
  size := 0;
  memtype := 0;
  scantime := 0;
  valido := false;

  found := GetTagProperts(TagObj,station,mem,size,memtype,scantime);

  if found then
    //se o endereco do plc esta numa faixa valida procura nos blocos de memoria.
    //check if the address of the slave is valid.
    if (station in [1..255]) then begin
      dev := DeviceIndex(memtype);
      if dev >= 0 then begin
        found := false;
        for plc:=0 to High(PXinJEPLC) do
          if PXinJEPLC[plc].Station = station then begin
            found := true;
            break;
          end;

        //se nao encontrou o plc, adiciona!
        //if the slave was not found, add it.
        if not found then begin
          plc := Length(PXinJEPLC);
          SetLength(PXinJEPLC,plc+1);
          PXinJEPLC[plc].Station := station;
          for c:=0 to XINJE_DEV_COUNT-1 do begin
            PXinJEPLC[plc].Devices[c] := TPLCMemoryManager.Create;
            if XINJE_DEV_ISBIT[c] then begin
              PXinJEPLC[plc].Devices[c].MaxBlockItems := PBitMaxBlockSize;
              PXinJEPLC[plc].Devices[c].MaxHole := PBitMaxHole;
            end else begin
              PXinJEPLC[plc].Devices[c].MaxBlockItems := PWordMaxBlockSize;
              PXinJEPLC[plc].Devices[c].MaxHole := PWordMaxHole;
            end;
          end;
        end;

        valido := (size > 0);
        if valido then
          PXinJEPLC[plc].Devices[dev].AddAddress(DeviceAddress(dev,mem),size,1,scantime);
      end;
    end;
  inherited DoAddTag(TagObj, valido);
end;

procedure TXinJEDriver.DoDelTag(TagObj:TTag);
var
  station, mem, size, memtype, scantime:LongInt;
  found:Boolean;
  plc, dev:LongInt;
begin
  //Recupera as informacoes do tag;
  //retrieve informations about the tag.
  station := 0;
  mem := 0;
  size := 0;
  memtype := 0;
  scantime := 0;
  found := GetTagProperts(TagObj,station,mem,size,memtype,scantime);

  if found then
    //se o endereco do plc esta numa faixa valida procura nos blocos de memoria.
    //check if the slave address is valid.
    if (station in [1..255]) then begin
      dev := DeviceIndex(memtype);
      found := false;
      for plc:=0 to High(PXinJEPLC) do
        if PXinJEPLC[plc].Station = station then begin
          found := true;
          break;
        end;

      //se encontrou o plc remove a memoria que estou lendo dele.
      //if the slave was found, remove the tag from its memory.
      if found and (dev >= 0) then
        PXinJEPLC[plc].Devices[dev].RemoveAddress(DeviceAddress(dev,mem),size,1);
    end;
  inherited DoDelTag(TagObj);
end;

procedure TXinJEDriver.DoGetValue(TagObj:TTagRec; var values:TScanReadRec);
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
    for plc:=0 to High(PXinJEPLC) do
      if PXinJEPLC[plc].Station = TagObj.Station then begin
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

  PXinJEPLC[plc].Devices[dev].GetValues(DeviceAddress(dev,TagObj.Address),TagObj.Size,1,
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

function TXinJEDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
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

  //TPLCBlock and TPLCStruct
  if (aTag is TPLCBlock) then begin
    if (isWrite) then
      FunctionCode := TPLCBlock(aTag).MemWriteFunction
    else
      FunctionCode := TPLCBlock(aTag).MemReadFunction;
  end;

  //TPLCString
  if (aTag is TPLCString) then begin
    if (isWrite) then
      FunctionCode := TPLCString(aTag).MemWriteFunction
    else
      FunctionCode := TPLCString(aTag).MemReadFunction;
  end;

  //retorna o tamanho em bits das variaveis lidas/escritas por
  //cada tipo de elemento de memoria
  //
  //return the size in bits of the element read/written by each function code
  dev := DeviceIndex(FunctionCode);
  if (dev >= 0) and (not XINJE_DEV_ISBIT[dev]) then begin
    Result := 16;
    ProtocolTagType := ptWord;
  end else begin
    Result := 1;
    ProtocolTagType := ptBit;
  end;
end;

function TXinJEDriver.LiteralTagAddress(aTag:TTag; aBlockTag:TTag):AnsiString;
var
  func, address:LongInt;
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

  if DeviceIndex(func) >= 0 then
    Result := XINJE_DEV_NAME[DeviceIndex(func)] + IntToStr(address)
  else
    Result := inherited LiteralTagAddress(aTag, aBlockTag);
end;

function TXinJEDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES;
begin
  ResultLen := 0;
  Result := nil;
end;

function TXinJEDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
begin
  values := nil;
  Result := ioDriverError;
end;

function TXinJEDriver.RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt;
begin
  Result := 0;
end;

function TXinJEDriver.RemainingBytesWrite(buffer:BYTES):LongInt;
begin
  Result := 0;
end;

function TXinJEDriver.SendRead(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
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

      //se o resultado de leitura deu ok, le o resto do pacote.
      //if the IO result is OK, reads the remaining packet...
      if (res<>0) and (IOResult1.ReadIOResult=iorOK) then begin
        //retorna o numero de bytes que esta aguardando ser lido no buffer da porta de comunicacao.
        //calculates the remaining package length at the communication buffer.
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

function TXinJEDriver.SendWrite(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
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

function TXinJEDriver.ReadTagRec(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
var
  dev, crit, segLen, done, c, base, maxSeg:LongInt;
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

  //um pedido nao pode atravessar o endereco critico do elemento de memoria:
  //blocos longos sao divididos em duas leituras.
  //a single request must not cross the critical address of the memory area:
  //long blocks are split in two reads.
  crit := XINJE_DEV_CRITICAL[dev];
  if XINJE_DEV_ISBIT[dev] then
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
    if (crit > 0) and (base < crit) and ((base + segLen) > crit) then
      segLen := crit - base;
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

function TXinJEDriver.WriteTagRec(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
var
  dev, crit, segLen, done, c, base, available, maxSeg:LongInt;
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

  crit := XINJE_DEV_CRITICAL[dev];
  if XINJE_DEV_ISBIT[dev] then
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
    if (crit > 0) and (base < crit) and ((base + segLen) > crit) then
      segLen := crit - base;
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

function TXinJEDriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  tr:TTagRec;
begin
  tr := tagrec;
  NormalizeTagRec(tr);
  Result := ReadTagRec(tr,Values);
end;

function TXinJEDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  tr:TTagRec;
begin
  tr := tagrec;
  NormalizeTagRec(tr);
  Result := WriteTagRec(tr,Values);
end;

procedure TXinJEDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
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

    //evita alto consumo de CPU com tags ligados e sem porta de comunicacao.
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

    for plc:=0 to High(PXinJEPLC) do
      for dev:=0 to XINJE_DEV_COUNT-1 do
        if Assigned(PXinJEPLC[plc].Devices[dev]) then
          for block:=0 to High(PXinJEPLC[plc].Devices[dev].Blocks) do
            if PXinJEPLC[plc].Devices[dev].Blocks[block].NeedRefresh then begin
              found := true;
              BuildTagRec(PXinJEPLC[plc].Station, XINJE_DEV_FUNC[dev],
                          PXinJEPLC[plc].Devices[dev].Blocks[block].AddressStart,
                          PXinJEPLC[plc].Devices[dev].Blocks[block].Size, tr);
              FMustReleaseResources := true;
              ReadTagRec(tr,values);
              FMustReleaseResources := false;
            end else begin
              if first then begin
                first := false;
                minScan := PXinJEPLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate;
                lastPLC := plc;
                lastAddr := PXinJEPLC[plc].Devices[dev].Blocks[block].AddressStart;
                lastSize := PXinJEPLC[plc].Devices[dev].Blocks[block].Size;
                lastFunc := XINJE_DEV_FUNC[dev];
              end else
                if PXinJEPLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate > minScan then begin
                  minScan := PXinJEPLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate;
                  lastPLC := plc;
                  lastAddr := PXinJEPLC[plc].Devices[dev].Blocks[block].AddressStart;
                  lastSize := PXinJEPLC[plc].Devices[dev].Blocks[block].Size;
                  lastFunc := XINJE_DEV_FUNC[dev];
                end;
            end;

    //nada venceu o tempo de varredura: le o bloco que esta a mais tempo sem
    //atualizacao para manter a comunicacao viva.
    //nothing is due: read the block that is waiting for the longest time to
    //keep the link alive.
    if (not found) then
      if (not first) and PReadSomethingAlways then begin
        BuildTagRec(PXinJEPLC[lastPLC].Station, lastFunc, lastAddr, lastSize, tr);
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
