{$i ../common/language.inc}
{:
  @abstract(Base unit of the Inovance (汇川) PLC protocol driver family.)

  The Inovance PLCs (H5U/Easy, H3U/XP, AM400/600/800, AC800) speak standard
  Modbus over Ethernet (TCP 502); what makes them "Inovance" is the address
  translation between the Inovance soft elements (M, X, Y, S, B, Q, I,
  D/MW/MD, R, SD, T, C) and the Modbus data model. Everything protocol
  independent lives here: the device table, the address translation for the
  three series, the memory managers, the scan engine and the block splitting
  rules. The wire format is implemented by the descendants (see
  @link(InovanceTCP.TInovanceTCPDriver)).

  @bold(How to address a tag)

  A tag is configured through the properties of @link(TPLCTagNumber):

  @unorderedList(
    @item(@bold(PLCStation): Modbus unit id of the PLC (1..255).)
    @item(@bold(MemReadFunction) and @bold(MemWriteFunction): the soft element
          (device) that will be read/written. Both must be set to the same
          value.)
    @item(@bold(MemAddress): the address inside the soft element, written as a
          decimal bit offset / word number. See the mapping table below.)
  )

  @table(
    @rowHead( @cell(Soft element) @cell(Func) @cell(Type) @cell(AM) @cell(H3U) @cell(H5U) )
    @row( @cell(M)   @cell(1)  @cell(bit) @cell(yes, offset 0)   @cell(yes, offset 0)   @cell(yes, offset 0) )
    @row( @cell(Q)   @cell(2)  @cell(bit) @cell(yes, bit offset) @cell(-) @cell(-) )
    @row( @cell(I)   @cell(3)  @cell(bit) @cell(yes, bit offset, read only) @cell(-) @cell(-) )
    @row( @cell(X)   @cell(4)  @cell(bit) @cell(-) @cell(yes, octal bit offset, read only) @cell(yes, octal bit offset, read only) )
    @row( @cell(Y)   @cell(5)  @cell(bit) @cell(-) @cell(yes, octal bit offset) @cell(yes, octal bit offset) )
    @row( @cell(S)   @cell(6)  @cell(bit) @cell(-) @cell(yes, +57344) @cell(yes, +57344) )
    @row( @cell(B)   @cell(7)  @cell(bit) @cell(-) @cell(-) @cell(yes, +12288) )
    @row( @cell(D)   @cell(17) @cell(word) @cell(-) @cell(yes, offset 0) @cell(yes, offset 0) )
    @row( @cell(MW)  @cell(18) @cell(word) @cell(yes, offset 0) @cell(-) @cell(-) )
    @row( @cell(MD)  @cell(19) @cell(word, takes 2 registers) @cell(yes, x2) @cell(-) @cell(-) )
    @row( @cell(R)   @cell(20) @cell(word) @cell(-) @cell(yes, +12288) @cell(yes, +12288) )
    @row( @cell(SD)  @cell(21) @cell(word) @cell(-) @cell(yes, +9216) @cell(-) )
    @row( @cell(T)   @cell(22) @cell(word) @cell(-) @cell(yes, +61440) @cell(-) )
    @row( @cell(C)   @cell(23) @cell(word) @cell(-) @cell(yes, see below) @cell(-) )
  )

  Bit offsets explained:
  @unorderedList(
    @item(AM Q and I are typed as "byte.bit" on AutoShop: Q10.3 means the bit 3
          of the byte 10. MemAddress takes the decimal bit offset: 10*8+3 = 83.)
    @item(H3U/H5U X and Y are octal numbered like Mitsubishi. MemAddress takes
          the digits as octal (X17 -> bit 15). The dotted form X10.3 becomes
          octal(10)*8+3 = 67.)
    @item(M is always typed as plain decimal.)
  )

  The word counter C of the H3U uses one register below C200 and two registers
  from C200 on: the driver splits blocks that would cross the C200 boundary.
  32 bit data (MD, DINT) is low word first (CDAB), like on the Delta PLCs.

  The I and X soft elements are physical inputs: they can only be read, never
  written. The AM series SM/SD extensions (non standard Modbus function codes
  0x31/0x33/0x3F) are not supported.

  @seealso(InovanceTCP.TInovanceTCPDriver)
}

unit InovanceDriver;

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver, ProtocolTypes, Tag, PLCTagNumber,
  PLCMemoryManager, PLCBlock, PLCString
  {$IFNDEF FPC}, Windows{$ENDIF};

const
  //: Number of soft elements (memory areas) handled by this driver family.
  INOVANCE_DEV_COUNT = 14;

  //: MemReadFunction/MemWriteFunction of the bit devices.
  INOVANCE_FUNC_M = 1;
  INOVANCE_FUNC_Q = 2;
  INOVANCE_FUNC_I = 3;
  INOVANCE_FUNC_X = 4;
  INOVANCE_FUNC_Y = 5;
  INOVANCE_FUNC_S = 6;
  INOVANCE_FUNC_B = 7;

  //: MemReadFunction/MemWriteFunction of the word devices.
  INOVANCE_FUNC_D  = 17;
  INOVANCE_FUNC_MW = 18;
  INOVANCE_FUNC_MD = 19;
  INOVANCE_FUNC_R  = 20;
  INOVANCE_FUNC_SD = 21;
  INOVANCE_FUNC_T  = 22;
  INOVANCE_FUNC_C  = 23;

{:
  Table of the soft elements. All the arrays below are indexed by the device
  index (0..INOVANCE_DEV_COUNT-1), not by the function code.
}
const
  INOVANCE_DEV_FUNC: array[0..INOVANCE_DEV_COUNT-1] of LongInt = (
    INOVANCE_FUNC_M, INOVANCE_FUNC_Q, INOVANCE_FUNC_I, INOVANCE_FUNC_X,
    INOVANCE_FUNC_Y, INOVANCE_FUNC_S, INOVANCE_FUNC_B,
    INOVANCE_FUNC_D, INOVANCE_FUNC_MW, INOVANCE_FUNC_MD, INOVANCE_FUNC_R,
    INOVANCE_FUNC_SD, INOVANCE_FUNC_T, INOVANCE_FUNC_C
  );

  INOVANCE_DEV_NAME: array[0..INOVANCE_DEV_COUNT-1] of string[3] = (
    'M','Q','I','X','Y','S','B',
    'D','MW','MD','R','SD','T','C'
  );

  INOVANCE_DEV_ISBIT: array[0..INOVANCE_DEV_COUNT-1] of Boolean = (
    True, True, True, True, True, True, True,
    False, False, False, False, False, False, False
  );

type

  {: PLC family handled by the driver: the address translation differs. }
  TInovanceSeries = (ivsAM, ivsH3U, ivsH5U);

  {: How the digits typed on MemAddress are understood for X and Y. }
  TInovanceIOFormat = (ivoOctal, ivoDecimal);

  TInovanceStation = record
    Station: LongInt;
    //: One memory manager for each soft element of the PLC.
    Devices: array[0..INOVANCE_DEV_COUNT-1] of TPLCMemoryManager;
  end;

  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe base dos drivers do protocolo Inovance (汇川).)
  }
  {$ELSE}
  {:
  @abstract(Base class of the Inovance (汇川) protocol drivers.)

  The descendants only have to translate the requests built here into bytes of
  the wire (@link(EncodePkg)) and the answer back into values
  (@link(DecodePkg)). Everything else - tag registration, scan, write queue,
  address translation, block splitting - is done here.

  @seealso(InovanceTCP.TInovanceTCPDriver)
  }
  {$ENDIF}
  TInovanceDriver = class(TProtocolDriver)
  private
    FMustReleaseResources:Boolean;
    FSeries:TInovanceSeries;
    FIOAddressFormat:TInovanceIOFormat;
    PBitMaxHole:Cardinal;
    PWordMaxHole:Cardinal;
    PBitMaxBlockSize:Cardinal;
    PWordMaxBlockSize:Cardinal;
    procedure SetIOAddressFormat(v:TInovanceIOFormat);
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
    PInovancePLC:array of TInovanceStation;

    {: On the Inovance PLCs two soft elements can share the same Modbus wire
       address (AM: Q and M both start at coil 0), so the answer cannot be
       traced back by the wire address. The descendants register here, for
       each Modbus transaction id, the soft element that was requested. }
    PWireDevMap:array[0..255] of LongInt;

    function  GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
    procedure BuildTagRec(plc,func,startaddress,size:LongInt; out tr:TTagRec);

    {: Index in the device tables of the soft element selected by a
       MemReadFunction/MemWriteFunction value. Returns -1 when the function
       code doesn't belong to the Inovance family. }
    function  DeviceIndex(func:LongInt):LongInt;
    {: Converts the address typed on the tag (tag address space) into the
       Modbus address used on the wire. Implements the Inovance address map. }
    function  DeviceAddress(dev:LongInt; address:LongInt):LongInt;
    {: Converts a tag record from the tag address space into the wire address
       space. }
    procedure NormalizeTagRec(var tr:TTagRec);

    {: TRUE when the soft element exists on the current series. }
    function  DeviceSupportedBySeries(dev:LongInt):Boolean;
    {: Modbus read function code of a soft element: 1 (coil), 2 (discrete
       input, I and X) or 3 (holding register). }
    function  DeviceReadFuncCode(dev:LongInt):Byte;

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

    {: PLC family handled by the driver. Set it before adding the tags: the
       address translation happens when a tag is registered. }
    property Series:TInovanceSeries read FSeries write FSeries default ivsH5U;

    {: How the digits typed on MemAddress are read for the X and Y soft
       elements. Inovance numbers them in octal, so X17 with ivoOctal reaches
       the bit 15 of the PLC, exactly like the address written on the ladder. }
    property IOAddressFormat:TInovanceIOFormat read FIOAddressFormat write SetIOAddressFormat default ivoOctal;

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
    //: Builds the address as it is written on the PLC program (D100, X17...).
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

constructor TInovanceDriver.Create(AOwner:TComponent);
var
  c:LongInt;
begin
  inherited Create(AOwner);
  FProtocolReady := false;

  FSeries := ivsH5U;
  FIOAddressFormat := ivoOctal;
  PBitMaxHole := 50;
  PWordMaxHole := 10;
  PBitMaxBlockSize := 1920;
  PWordMaxBlockSize := 120;

  PReadSomethingAlways := true;
  PInternalDelayBetweenCmds := 0;
  //o cabecalho MBAP do modbus TCP tem 7 bytes (6 de cabecalho + unit id) e a
  //funcao vem no oitavo byte. O resto do pacote e' lido com o tamanho
  //declarado no cabecalho.
  //the modbus TCP MBAP header has 7 bytes (6 header + unit id) and the
  //function byte comes on the eighth byte. The rest of the packet is read
  //with the size declared on the header.
  PFirstRequestLen := 8;
  PFuncByteOffset := 7;
  PCRCLen := 0;

  for c:=0 to 255 do
    PWireDevMap[c] := -1;

  SetLength(PInovancePLC,0);
end;

destructor TInovanceDriver.Destroy;
var
  plc, dev:LongInt;
begin
  inherited Destroy;
  for plc:=0 to High(PInovancePLC) do
    for dev:=0 to INOVANCE_DEV_COUNT-1 do
      if Assigned(PInovancePLC[plc].Devices[dev]) then begin
        PInovancePLC[plc].Devices[dev].Destroy;
        PInovancePLC[plc].Devices[dev] := nil;
      end;
  SetLength(PInovancePLC,0);
end;

function TInovanceDriver.DeviceIndex(func:LongInt):LongInt;
var
  c:LongInt;
begin
  Result := -1;
  for c:=0 to INOVANCE_DEV_COUNT-1 do
    if INOVANCE_DEV_FUNC[c] = func then begin
      Result := c;
      exit;
    end;
end;

function TInovanceDriver.DeviceSupportedBySeries(dev:LongInt):Boolean;
begin
  Result := true;
  if (dev < 0) or (dev >= INOVANCE_DEV_COUNT) then begin
    Result := false;
    exit;
  end;

  case FSeries of
    ivsAM:
      //AM has Q, I, M (bit) and MW, MD (word). D, R, SD, T, C and the
      //H3U/H5U physical inputs/outputs are not mapped on the wire.
      Result := INOVANCE_DEV_FUNC[dev] in [INOVANCE_FUNC_M, INOVANCE_FUNC_Q,
                  INOVANCE_FUNC_I, INOVANCE_FUNC_MW, INOVANCE_FUNC_MD];
    ivsH3U:
      Result := INOVANCE_DEV_FUNC[dev] in [INOVANCE_FUNC_M, INOVANCE_FUNC_X,
                  INOVANCE_FUNC_Y, INOVANCE_FUNC_S,
                  INOVANCE_FUNC_D, INOVANCE_FUNC_R, INOVANCE_FUNC_SD,
                  INOVANCE_FUNC_T, INOVANCE_FUNC_C];
    ivsH5U:
      Result := INOVANCE_DEV_FUNC[dev] in [INOVANCE_FUNC_M, INOVANCE_FUNC_X,
                  INOVANCE_FUNC_Y, INOVANCE_FUNC_S, INOVANCE_FUNC_B,
                  INOVANCE_FUNC_D, INOVANCE_FUNC_R];
  end;
end;

function TInovanceDriver.DeviceReadFuncCode(dev:LongInt):Byte;
begin
  //I e X sao entradas fisicas: lidas com a funcao 02 (discrete input).
  //I and X are physical inputs: read with the function 02 (discrete input).
  Result := 1;
  if (dev >= 0) and (dev < INOVANCE_DEV_COUNT) then begin
    if INOVANCE_DEV_ISBIT[dev] then
      Result := 1
    else
      Result := 3;
    if INOVANCE_DEV_FUNC[dev] in [INOVANCE_FUNC_I, INOVANCE_FUNC_X] then
      Result := 2;
  end;
end;

function TInovanceDriver.DeviceAddress(dev:LongInt; address:LongInt):LongInt;
begin
  Result := address;
  if (dev < 0) or (dev >= INOVANCE_DEV_COUNT) then
    exit;

  case FSeries of
    ivsAM:
      case INOVANCE_DEV_FUNC[dev] of
        //Q e I sao enderecos de bit decimal: Q10.3 -> 10*8+3 = 83.
        INOVANCE_FUNC_Q : Result := address;
        INOVANCE_FUNC_I : Result := address;
        INOVANCE_FUNC_M : Result := address;
        INOVANCE_FUNC_MW: Result := address;
        INOVANCE_FUNC_MD: Result := address * 2;
      end;
    ivsH3U:
      case INOVANCE_DEV_FUNC[dev] of
        INOVANCE_FUNC_M : Result := address;
        INOVANCE_FUNC_X :
          if FIOAddressFormat = ivoOctal then
            Result := OctalAddress(address) + 63488
          else
            Result := address + 63488;
        INOVANCE_FUNC_Y :
          if FIOAddressFormat = ivoOctal then
            Result := OctalAddress(address) + 64512
          else
            Result := address + 64512;
        INOVANCE_FUNC_S : Result := address + 57344;
        INOVANCE_FUNC_D : Result := address;
        INOVANCE_FUNC_R : Result := address + 12288;
        INOVANCE_FUNC_SD: Result := address + 9216;
        INOVANCE_FUNC_T : Result := address + 61440;
        INOVANCE_FUNC_C :
          //C0..C199 = 16 bits counters, C200+ = 32 bits (two registers).
          if address >= 200 then
            Result := (address - 200) * 2 + 63232
          else
            Result := address + 62464;
      end;
    ivsH5U:
      case INOVANCE_DEV_FUNC[dev] of
        INOVANCE_FUNC_M : Result := address;
        INOVANCE_FUNC_X :
          if FIOAddressFormat = ivoOctal then
            Result := OctalAddress(address) + 63488
          else
            Result := address + 63488;
        INOVANCE_FUNC_Y :
          if FIOAddressFormat = ivoOctal then
            Result := OctalAddress(address) + 64512
          else
            Result := address + 64512;
        INOVANCE_FUNC_S : Result := address + 57344;
        INOVANCE_FUNC_B : Result := address + 12288;
        INOVANCE_FUNC_D : Result := address;
        INOVANCE_FUNC_R : Result := address + 12288;
      end;
  end;
end;

procedure TInovanceDriver.NormalizeTagRec(var tr:TTagRec);
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

function TInovanceDriver.GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
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

procedure TInovanceDriver.BuildTagRec(plc, func, startaddress, size: LongInt; out tr: TTagRec);
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

procedure TInovanceDriver.SetIOAddressFormat(v:TInovanceIOFormat);
begin
  //a conversao acontece quando o tag e' registrado: mude o formato antes de
  //ligar os tags ao driver.
  //the conversion happens when the tag is registered: change the format before
  //linking the tags to the driver.
  FIOAddressFormat := v;
end;

procedure TInovanceDriver.SetBitMaxHole(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PBitMaxHole then
    exit;
  PBitMaxHole := v;
  for plc:=0 to High(PInovancePLC) do
    for dev:=0 to INOVANCE_DEV_COUNT-1 do
      if Assigned(PInovancePLC[plc].Devices[dev]) and INOVANCE_DEV_ISBIT[dev] then
        PInovancePLC[plc].Devices[dev].MaxHole := v;
end;

procedure TInovanceDriver.SetWordMaxHole(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PWordMaxHole then
    exit;
  PWordMaxHole := v;
  for plc:=0 to High(PInovancePLC) do
    for dev:=0 to INOVANCE_DEV_COUNT-1 do
      if Assigned(PInovancePLC[plc].Devices[dev]) and (not INOVANCE_DEV_ISBIT[dev]) then
        PInovancePLC[plc].Devices[dev].MaxHole := v;
end;

procedure TInovanceDriver.SetBitMaxBlockSize(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PBitMaxBlockSize then
    exit;
  PBitMaxBlockSize := v;
  for plc:=0 to High(PInovancePLC) do
    for dev:=0 to INOVANCE_DEV_COUNT-1 do
      if Assigned(PInovancePLC[plc].Devices[dev]) and INOVANCE_DEV_ISBIT[dev] then
        PInovancePLC[plc].Devices[dev].MaxBlockItems := v;
end;

procedure TInovanceDriver.SetWordMaxBlockSize(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PWordMaxBlockSize then
    exit;
  PWordMaxBlockSize := v;
  for plc:=0 to High(PInovancePLC) do
    for dev:=0 to INOVANCE_DEV_COUNT-1 do
      if Assigned(PInovancePLC[plc].Devices[dev]) and (not INOVANCE_DEV_ISBIT[dev]) then
        PInovancePLC[plc].Devices[dev].MaxBlockItems := v;
end;

procedure TInovanceDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
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
      if (dev >= 0) and DeviceSupportedBySeries(dev) then begin
        found := false;
        for plc:=0 to High(PInovancePLC) do
          if PInovancePLC[plc].Station = station then begin
            found := true;
            break;
          end;

        //se nao encontrou o plc, adiciona!
        //if the slave was not found, add it.
        if not found then begin
          plc := Length(PInovancePLC);
          SetLength(PInovancePLC,plc+1);
          PInovancePLC[plc].Station := station;
          for c:=0 to INOVANCE_DEV_COUNT-1 do begin
            PInovancePLC[plc].Devices[c] := TPLCMemoryManager.Create;
            if INOVANCE_DEV_ISBIT[c] then begin
              PInovancePLC[plc].Devices[c].MaxBlockItems := PBitMaxBlockSize;
              PInovancePLC[plc].Devices[c].MaxHole := PBitMaxHole;
            end else begin
              PInovancePLC[plc].Devices[c].MaxBlockItems := PWordMaxBlockSize;
              PInovancePLC[plc].Devices[c].MaxHole := PWordMaxHole;
            end;
          end;
        end;

        valido := (size > 0);
        if valido then
          PInovancePLC[plc].Devices[dev].AddAddress(DeviceAddress(dev,mem),size,1,scantime);
      end;
    end;
  inherited DoAddTag(TagObj, valido);
end;

procedure TInovanceDriver.DoDelTag(TagObj:TTag);
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
      for plc:=0 to High(PInovancePLC) do
        if PInovancePLC[plc].Station = station then begin
          found := true;
          break;
        end;

      //se encontrou o plc remove a memoria que estou lendo dele.
      //if the slave was found, remove the tag from its memory.
      if found and (dev >= 0) then
        PInovancePLC[plc].Devices[dev].RemoveAddress(DeviceAddress(dev,mem),size,1);
    end;
  inherited DoDelTag(TagObj);
end;

procedure TInovanceDriver.DoGetValue(TagObj:TTagRec; var values:TScanReadRec);
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
    for plc:=0 to High(PInovancePLC) do
      if PInovancePLC[plc].Station = TagObj.Station then begin
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

  PInovancePLC[plc].Devices[dev].GetValues(DeviceAddress(dev,TagObj.Address),TagObj.Size,1,
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

function TInovanceDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
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
  if (dev >= 0) and (not INOVANCE_DEV_ISBIT[dev]) then begin
    Result := 16;
    ProtocolTagType := ptWord;
  end else begin
    Result := 1;
    ProtocolTagType := ptBit;
  end;
end;

function TInovanceDriver.LiteralTagAddress(aTag:TTag; aBlockTag:TTag):AnsiString;
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
    Result := INOVANCE_DEV_NAME[DeviceIndex(func)] + IntToStr(address)
  else
    Result := inherited LiteralTagAddress(aTag, aBlockTag);
end;

function TInovanceDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES;
begin
  ResultLen := 0;
  Result := nil;
end;

function TInovanceDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
begin
  values := nil;
  Result := ioDriverError;
end;

function TInovanceDriver.RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt;
begin
  Result := 0;
end;

function TInovanceDriver.RemainingBytesWrite(buffer:BYTES):LongInt;
begin
  Result := 0;
end;

function TInovanceDriver.SendRead(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
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

function TInovanceDriver.SendWrite(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
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

function TInovanceDriver.ReadTagRec(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
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
  //H3U: o contador C comeca a ocupar dois registradores a partir de C200.
  //H3U: the counter C takes two registers from C200 on.
  crit := 0;
  if (FSeries = ivsH3U) and (INOVANCE_DEV_FUNC[dev] = INOVANCE_FUNC_C) then
    crit := 63232;

  if INOVANCE_DEV_ISBIT[dev] then
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

function TInovanceDriver.WriteTagRec(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
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

  crit := 0;
  if (FSeries = ivsH3U) and (INOVANCE_DEV_FUNC[dev] = INOVANCE_FUNC_C) then
    crit := 63232;

  if INOVANCE_DEV_ISBIT[dev] then
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

function TInovanceDriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  tr:TTagRec;
begin
  tr := tagrec;
  NormalizeTagRec(tr);
  Result := ReadTagRec(tr,Values);
end;

function TInovanceDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  tr:TTagRec;
begin
  tr := tagrec;
  NormalizeTagRec(tr);
  Result := WriteTagRec(tr,Values);
end;

procedure TInovanceDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
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

    for plc:=0 to High(PInovancePLC) do
      for dev:=0 to INOVANCE_DEV_COUNT-1 do
        if Assigned(PInovancePLC[plc].Devices[dev]) then
          for block:=0 to High(PInovancePLC[plc].Devices[dev].Blocks) do
            if PInovancePLC[plc].Devices[dev].Blocks[block].NeedRefresh then begin
              found := true;
              BuildTagRec(PInovancePLC[plc].Station, INOVANCE_DEV_FUNC[dev],
                          PInovancePLC[plc].Devices[dev].Blocks[block].AddressStart,
                          PInovancePLC[plc].Devices[dev].Blocks[block].Size, tr);
              FMustReleaseResources := true;
              ReadTagRec(tr,values);
              FMustReleaseResources := false;
            end else begin
              if first then begin
                first := false;
                minScan := PInovancePLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate;
                lastPLC := plc;
                lastAddr := PInovancePLC[plc].Devices[dev].Blocks[block].AddressStart;
                lastSize := PInovancePLC[plc].Devices[dev].Blocks[block].Size;
                lastFunc := INOVANCE_DEV_FUNC[dev];
              end else
                if PInovancePLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate > minScan then begin
                  minScan := PInovancePLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate;
                  lastPLC := plc;
                  lastAddr := PInovancePLC[plc].Devices[dev].Blocks[block].AddressStart;
                  lastSize := PInovancePLC[plc].Devices[dev].Blocks[block].Size;
                  lastFunc := INOVANCE_DEV_FUNC[dev];
                end;
            end;

    //nada venceu o tempo de varredura: le o bloco que esta a mais tempo sem
    //atualizacao para manter a comunicacao viva.
    //nothing is due: read the block that is waiting for the longest time to
    //keep the link alive.
    if (not found) then
      if (not first) and PReadSomethingAlways then begin
        BuildTagRec(PInovancePLC[lastPLC].Station, lastFunc, lastAddr, lastSize, tr);
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
