{$i ../common/language.inc}
{:
  @abstract(Base unit of the Delta (台达) PLC protocol driver family.)

  The Delta PLC family talks standard Modbus; what makes it "Delta" is the
  address translation between the Delta soft elements (M, X, Y, S, T, C, D...)
  and the Modbus data model. Everything protocol independent lives here: the
  device table, the address translation for the DVP and AS series, the memory
  managers, the scan engine and the block splitting rules. The wire format is
  implemented by the descendants (see @link(DeltaTCP.TDeltaTCPDriver)).

  @bold(How to address a tag)

  A tag is configured through the properties of @link(TPLCTagNumber):

  @unorderedList(
    @item(@bold(PLCStation): Modbus unit id of the PLC (1..255).)
    @item(@bold(MemReadFunction) and @bold(MemWriteFunction): the soft element
          (device) that will be read/written. Both must be set to the same
          value.)
    @item(@bold(MemAddress): the address inside the soft element, written the
          same way you write it on WPLSoft/ASDA Soft.)
  )

  @table(
    @rowHead( @cell(Soft element) @cell(MemReadFunction/MemWriteFunction) @cell(MemAddress) @cell(DVP) @cell(AS) )
    @row(     @cell(M)   @cell(1)  @cell(M0, M100, ...)      @cell(yes) @cell(yes) )
    @row(     @cell(X)   @cell(2)  @cell(X0, X17, ... octal) @cell(yes) @cell(yes) )
    @row(     @cell(Y)   @cell(3)  @cell(Y0, Y17, ... octal) @cell(yes) @cell(yes) )
    @row(     @cell(S)   @cell(4)  @cell(S0, S100, ...)      @cell(yes) @cell(yes) )
    @row(     @cell(T)   @cell(5)  @cell(T0, T100, ... bit)  @cell(yes) @cell(yes) )
    @row(     @cell(C)   @cell(6)  @cell(C0, C100, ... bit)  @cell(yes) @cell(yes) )
    @row(     @cell(SM)  @cell(7)  @cell(SM0, SM100, ...)    @cell(-)   @cell(yes) )
    @row(     @cell(HC)  @cell(8)  @cell(HC0, ... bit)       @cell(-)   @cell(yes) )
    @row(     @cell(D)   @cell(17) @cell(D0, D100, ...)      @cell(yes) @cell(yes) )
    @row(     @cell(TD)  @cell(18) @cell(TD0, ... word)      @cell(yes) @cell(yes) )
    @row(     @cell(CD)  @cell(19) @cell(CD0, ... word)      @cell(yes) @cell(yes) )
    @row(     @cell(SR)  @cell(20) @cell(SR0, ... word)      @cell(-)   @cell(yes) )
    @row(     @cell(HCD) @cell(21) @cell(HCD0, ... word)     @cell(-)   @cell(yes) )
    @row(     @cell(E)   @cell(22) @cell(E0, ... word)       @cell(-)   @cell(yes) )
  )

  For the DVP series X and Y are numbered in octal by the PLC itself: X17 means
  the fifteenth input. Because MemAddress is a number, the driver reads its
  digits as octal digits (X17 -> 17 -> 15) when IOAddressFormat is dvfOctal,
  which is the default. Set IOAddressFormat to dvfDecimal to type the plain bit
  index instead. For the AS series X and Y use decimal bit offsets: X10.5 is
  typed as 10*16+5 = 165.

  The X soft element is an input: it can only be read, never written. 32 bit
  values use the CDAB word order of the Delta PLCs: the low word is stored on
  the first register.

  @seealso(DeltaTCP.TDeltaTCPDriver)
}

unit DeltaDriver;

interface

uses
  SysUtils, Classes, CommTypes, ProtocolDriver, ProtocolTypes, Tag, PLCTagNumber,
  PLCMemoryManager, PLCBlock, PLCString
  {$IFNDEF FPC}, Windows{$ENDIF};

const
  //: Number of soft elements (memory areas) handled by this driver family.
  DELTA_DEV_COUNT = 14;

  //: MemReadFunction/MemWriteFunction of the bit devices.
  DELTA_FUNC_M  = 1;
  DELTA_FUNC_X  = 2;
  DELTA_FUNC_Y  = 3;
  DELTA_FUNC_S  = 4;
  DELTA_FUNC_T  = 5;
  DELTA_FUNC_C  = 6;
  DELTA_FUNC_SM = 7;
  DELTA_FUNC_HC = 8;

  //: MemReadFunction/MemWriteFunction of the word devices.
  DELTA_FUNC_D   = 17;
  DELTA_FUNC_TD  = 18;
  DELTA_FUNC_CD  = 19;
  DELTA_FUNC_SR  = 20;
  DELTA_FUNC_HCD = 21;
  DELTA_FUNC_E   = 22;

{:
  Table of the soft elements. All the arrays below are indexed by the device
  index (0..DELTA_DEV_COUNT-1), not by the function code.
}
const
  DELTA_DEV_FUNC: array[0..DELTA_DEV_COUNT-1] of LongInt = (
    DELTA_FUNC_M, DELTA_FUNC_X, DELTA_FUNC_Y, DELTA_FUNC_S,
    DELTA_FUNC_T, DELTA_FUNC_C, DELTA_FUNC_SM, DELTA_FUNC_HC,
    DELTA_FUNC_D, DELTA_FUNC_TD, DELTA_FUNC_CD, DELTA_FUNC_SR,
    DELTA_FUNC_HCD, DELTA_FUNC_E
  );

  DELTA_DEV_NAME: array[0..DELTA_DEV_COUNT-1] of string[3] = (
    'M','X','Y','S','T','C','SM','HC',
    'D','TD','CD','SR','HCD','E'
  );

  DELTA_DEV_ISBIT: array[0..DELTA_DEV_COUNT-1] of Boolean = (
    True, True, True, True, True, True, True, True,
    False, False, False, False, False, False
  );

type

  {: PLC family handled by the driver: the address translation differs. }
  TDeltaSeries = (dvsDvp, dvsAS);

  {: How the digits typed on MemAddress are understood for X and Y (DVP). }
  TDeltaIOFormat = (dvfOctal, dvfDecimal);

  TDeltaStation = record
    Station: LongInt;
    //: One memory manager for each soft element of the PLC.
    Devices: array[0..DELTA_DEV_COUNT-1] of TPLCMemoryManager;
  end;

  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe base dos drivers do protocolo Delta (台达).)
  }
  {$ELSE}
  {:
  @abstract(Base class of the Delta (台达) protocol drivers.)

  The descendants only have to translate the requests built here into bytes of
  the wire (@link(EncodePkg)) and the answer back into values
  (@link(DecodePkg)). Everything else - tag registration, scan, write queue,
  address translation, block splitting - is done here.

  @seealso(DeltaTCP.TDeltaTCPDriver)
  }
  {$ENDIF}
  TDeltaDriver = class(TProtocolDriver)
  private
    FMustReleaseResources:Boolean;
    FSeries:TDeltaSeries;
    FIOAddressFormat:TDeltaIOFormat;
    PBitMaxHole:Cardinal;
    PWordMaxHole:Cardinal;
    PBitMaxBlockSize:Cardinal;
    PWordMaxBlockSize:Cardinal;
    procedure SetIOAddressFormat(v:TDeltaIOFormat);
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
    PDeltaPLC:array of TDeltaStation;

    function  GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
    procedure BuildTagRec(plc,func,startaddress,size:LongInt; out tr:TTagRec);

    {: Index in the device tables of the soft element selected by a
       MemReadFunction/MemWriteFunction value. Returns -1 when the function
       code doesn't belong to the Delta family. }
    function  DeviceIndex(func:LongInt):LongInt;
    {: Converts the address typed on the tag (tag address space) into the
       Modbus address used on the wire. Implements the Delta address map. }
    function  DeviceAddress(dev:LongInt; address:LongInt):LongInt;
    {: Converts a tag record from the tag address space into the wire address
       space. }
    procedure NormalizeTagRec(var tr:TTagRec);

    {: First Modbus wire address of the soft element for the current series.
       Used to find back the soft element on the answers. Returns -1 when the
       device doesn't exist on the current series. }
    function  DeviceBaseWire(dev:LongInt):LongInt;
    {: Modbus address where a single request must not cross. Zero means "the
       whole range is contiguous". Depends on the series. }
    function  DeviceCritical(dev:LongInt):LongInt;
    {: TRUE when the soft element exists on the current series. }
    function  DeviceSupportedBySeries(dev:LongInt):Boolean;
    {: Modbus read function code of a soft element: 1 (coil), 2 (discrete
       input, X), 3 (holding register) or 4 (input register). }
    function  DeviceReadFuncCode(dev:LongInt):Byte;
    {: Index in the device tables of the soft element that owns a wire address,
       found back from the Modbus function code of the request. Returns -1 when
       the address belongs to no soft element. }
    function  DeviceIndexByWireAddress(func:Byte; address:LongInt):LongInt;

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
    property Series:TDeltaSeries read FSeries write FSeries default dvsDvp;

    {: How the digits typed on MemAddress are read for the X and Y soft
       elements. Delta numbers them in octal, so X17 with dvfOctal reaches the
       bit 15 of the PLC, exactly like the address written on the ladder. }
    property IOAddressFormat:TDeltaIOFormat read FIOAddressFormat write SetIOAddressFormat default dvfOctal;

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

constructor TDeltaDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FProtocolReady := false;

  FSeries := dvsDvp;
  FIOAddressFormat := dvfOctal;
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

  SetLength(PDeltaPLC,0);
end;

destructor TDeltaDriver.Destroy;
var
  plc, dev:LongInt;
begin
  inherited Destroy;
  for plc:=0 to High(PDeltaPLC) do
    for dev:=0 to DELTA_DEV_COUNT-1 do
      if Assigned(PDeltaPLC[plc].Devices[dev]) then begin
        PDeltaPLC[plc].Devices[dev].Destroy;
        PDeltaPLC[plc].Devices[dev] := nil;
      end;
  SetLength(PDeltaPLC,0);
end;

function TDeltaDriver.DeviceIndex(func:LongInt):LongInt;
var
  c:LongInt;
begin
  Result := -1;
  for c:=0 to DELTA_DEV_COUNT-1 do
    if DELTA_DEV_FUNC[c] = func then begin
      Result := c;
      exit;
    end;
end;

function TDeltaDriver.DeviceSupportedBySeries(dev:LongInt):Boolean;
begin
  Result := true;
  case FSeries of
    dvsDvp:
      //SM, HC, SR, HCD and E only exist on the AS series.
      Result := not (DELTA_DEV_FUNC[dev] in [DELTA_FUNC_SM, DELTA_FUNC_HC,
                    DELTA_FUNC_SR, DELTA_FUNC_HCD, DELTA_FUNC_E]);
    dvsAS:
      Result := true;
  end;
end;

function TDeltaDriver.DeviceBaseWire(dev:LongInt):LongInt;
begin
  Result := -1;
  if (dev < 0) or (dev >= DELTA_DEV_COUNT) then
    exit;

  case FSeries of
    dvsDvp:
      case DELTA_DEV_FUNC[dev] of
        DELTA_FUNC_M : Result := 2048;   //M0..M1535 -> 2048..3583
        DELTA_FUNC_X : Result := 1024;   //X0.. -> 1024..
        DELTA_FUNC_Y : Result := 1280;   //Y0.. -> 1280..
        DELTA_FUNC_S : Result := 0;      //S0.. -> 0..
        DELTA_FUNC_T : Result := 1536;   //T0.. -> 1536..
        DELTA_FUNC_C : Result := 3584;   //C0.. -> 3584..
        DELTA_FUNC_D : Result := 4096;   //D0..D4095 -> 4096..8191
        DELTA_FUNC_TD: Result := 1536;
        DELTA_FUNC_CD: Result := 3584;
      end;
    dvsAS:
      case DELTA_DEV_FUNC[dev] of
        DELTA_FUNC_M : Result := 0;
        DELTA_FUNC_X : Result := 24576;
        DELTA_FUNC_Y : Result := 40960;
        DELTA_FUNC_S : Result := 20480;
        DELTA_FUNC_T : Result := 57344;
        DELTA_FUNC_C : Result := 61440;
        DELTA_FUNC_SM: Result := 16384;
        DELTA_FUNC_HC: Result := 64512;
        DELTA_FUNC_D : Result := 0;
        DELTA_FUNC_TD: Result := 57344;
        DELTA_FUNC_CD: Result := 61440;
        DELTA_FUNC_SR: Result := 49152;
        DELTA_FUNC_HCD: Result := 64512;
        DELTA_FUNC_E : Result := 65024;
      end;
  end;
end;

function TDeltaDriver.DeviceCritical(dev:LongInt):LongInt;
begin
  Result := 0;
  if (dev < 0) or (dev >= DELTA_DEV_COUNT) then
    exit;

  //na serie DVP o M salta de 3583 para 45056 e o D de 8191 para 36864: um
  //pedido nao pode atravessar esses enderecos.
  //on the DVP series M jumps from 3583 to 45056 and D from 8191 to 36864: a
  //single request must not cross those addresses.
  if FSeries = dvsDvp then
    case DELTA_DEV_FUNC[dev] of
      DELTA_FUNC_M : Result := 3584;
      DELTA_FUNC_D : Result := 8192;
    end;
end;

function TDeltaDriver.DeviceReadFuncCode(dev:LongInt):Byte;
begin
  //X e' uma entrada fisica: lida com a funcao 02 (discrete input).
  //X is a physical input: read with the function 02 (discrete input).
  Result := 1;
  if (dev >= 0) and (dev < DELTA_DEV_COUNT) then begin
    if DELTA_DEV_ISBIT[dev] then
      Result := 1
    else
      Result := 3;
    if DELTA_DEV_FUNC[dev] = DELTA_FUNC_X then
      Result := 2;
  end;
end;

function TDeltaDriver.DeviceAddress(dev:LongInt; address:LongInt):LongInt;
begin
  Result := address;
  if (dev < 0) or (dev >= DELTA_DEV_COUNT) then
    exit;

  case FSeries of
    dvsDvp:
      case DELTA_DEV_FUNC[dev] of
        DELTA_FUNC_M :
          if address >= 1536 then
            Result := address - 1536 + 45056
          else
            Result := address + 2048;
        DELTA_FUNC_X :
          if FIOAddressFormat = dvfOctal then
            Result := OctalAddress(address) + 1024
          else
            Result := address + 1024;
        DELTA_FUNC_Y :
          if FIOAddressFormat = dvfOctal then
            Result := OctalAddress(address) + 1280
          else
            Result := address + 1280;
        DELTA_FUNC_S : Result := address;
        DELTA_FUNC_T : Result := address + 1536;
        DELTA_FUNC_C : Result := address + 3584;
        DELTA_FUNC_D :
          if address >= 4096 then
            Result := address - 4096 + 36864
          else
            Result := address + 4096;
        DELTA_FUNC_TD: Result := address + 1536;
        DELTA_FUNC_CD: Result := address + 3584;
      end;
    dvsAS:
      case DELTA_DEV_FUNC[dev] of
        DELTA_FUNC_M  : Result := address;
        DELTA_FUNC_X  : Result := address + 24576;
        DELTA_FUNC_Y  : Result := address + 40960;
        DELTA_FUNC_S  : Result := address + 20480;
        DELTA_FUNC_T  : Result := address + 57344;
        DELTA_FUNC_C  : Result := address + 61440;
        DELTA_FUNC_SM : Result := address + 16384;
        DELTA_FUNC_HC : Result := address + 64512;
        DELTA_FUNC_D  : Result := address;
        DELTA_FUNC_TD : Result := address + 57344;
        DELTA_FUNC_CD : Result := address + 61440;
        DELTA_FUNC_SR : Result := address + 49152;
        DELTA_FUNC_HCD: Result := address + 64512;
        DELTA_FUNC_E  : Result := address + 65024;
      end;
  end;
end;

function TDeltaDriver.DeviceIndexByWireAddress(func:Byte; address:LongInt):LongInt;
var
  c, base, bestBase:LongInt;
  isBit:Boolean;
begin
  Result := -1;
  isBit := func in [1, 2, 5, 15];

  //na serie DVP o M salta para 45056 e o D para 36864: nessas regioes a regra
  //"maior base antes do endereco" escolheria o elemento errado, porque o C
  //(bit) comeca em 3584. Trate as extensoes explicitamente.
  //on the DVP series M jumps to 45056 and D to 36864: on those regions the
  //"largest base before the address" rule would pick the wrong element,
  //because C (bit) starts at 3584. Handle the extensions explicitly.
  if FSeries = dvsDvp then begin
    if isBit and (address >= 45056) then begin
      Result := DeviceIndex(DELTA_FUNC_M);
      exit;
    end;
    if (not isBit) and (address >= 36864) and (address < 45056) then begin
      Result := DeviceIndex(DELTA_FUNC_D);
      exit;
    end;
  end;

  bestBase := -1;
  for c:=0 to DELTA_DEV_COUNT-1 do begin
    if DELTA_DEV_ISBIT[c] <> isBit then
      continue;
    if not DeviceSupportedBySeries(c) then
      continue;
    base := DeviceBaseWire(c);
    if base < 0 then
      continue;
    if (address >= base) and (base > bestBase) then begin
      bestBase := base;
      Result := c;
    end;
  end;
end;

procedure TDeltaDriver.NormalizeTagRec(var tr:TTagRec);
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

function TDeltaDriver.GetTagProperts(TagObj:TTag; var Station, Address, Size, RegType, ScanTime:LongInt):Boolean;
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

procedure TDeltaDriver.BuildTagRec(plc, func, startaddress, size: LongInt; out tr: TTagRec);
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

procedure TDeltaDriver.SetIOAddressFormat(v:TDeltaIOFormat);
begin
  //a conversao acontece quando o tag e' registrado: mude o formato antes de
  //ligar os tags ao driver.
  //the conversion happens when the tag is registered: change the format before
  //linking the tags to the driver.
  FIOAddressFormat := v;
end;

procedure TDeltaDriver.SetBitMaxHole(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PBitMaxHole then
    exit;
  PBitMaxHole := v;
  for plc:=0 to High(PDeltaPLC) do
    for dev:=0 to DELTA_DEV_COUNT-1 do
      if Assigned(PDeltaPLC[plc].Devices[dev]) and DELTA_DEV_ISBIT[dev] then
        PDeltaPLC[plc].Devices[dev].MaxHole := v;
end;

procedure TDeltaDriver.SetWordMaxHole(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PWordMaxHole then
    exit;
  PWordMaxHole := v;
  for plc:=0 to High(PDeltaPLC) do
    for dev:=0 to DELTA_DEV_COUNT-1 do
      if Assigned(PDeltaPLC[plc].Devices[dev]) and (not DELTA_DEV_ISBIT[dev]) then
        PDeltaPLC[plc].Devices[dev].MaxHole := v;
end;

procedure TDeltaDriver.SetBitMaxBlockSize(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PBitMaxBlockSize then
    exit;
  PBitMaxBlockSize := v;
  for plc:=0 to High(PDeltaPLC) do
    for dev:=0 to DELTA_DEV_COUNT-1 do
      if Assigned(PDeltaPLC[plc].Devices[dev]) and DELTA_DEV_ISBIT[dev] then
        PDeltaPLC[plc].Devices[dev].MaxBlockItems := v;
end;

procedure TDeltaDriver.SetWordMaxBlockSize(v:Cardinal);
var
  plc, dev:LongInt;
begin
  if v = PWordMaxBlockSize then
    exit;
  PWordMaxBlockSize := v;
  for plc:=0 to High(PDeltaPLC) do
    for dev:=0 to DELTA_DEV_COUNT-1 do
      if Assigned(PDeltaPLC[plc].Devices[dev]) and (not DELTA_DEV_ISBIT[dev]) then
        PDeltaPLC[plc].Devices[dev].MaxBlockItems := v;
end;

procedure TDeltaDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
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
        for plc:=0 to High(PDeltaPLC) do
          if PDeltaPLC[plc].Station = station then begin
            found := true;
            break;
          end;

        //se nao encontrou o plc, adiciona!
        //if the slave was not found, add it.
        if not found then begin
          plc := Length(PDeltaPLC);
          SetLength(PDeltaPLC,plc+1);
          PDeltaPLC[plc].Station := station;
          for c:=0 to DELTA_DEV_COUNT-1 do begin
            PDeltaPLC[plc].Devices[c] := TPLCMemoryManager.Create;
            if DELTA_DEV_ISBIT[c] then begin
              PDeltaPLC[plc].Devices[c].MaxBlockItems := PBitMaxBlockSize;
              PDeltaPLC[plc].Devices[c].MaxHole := PBitMaxHole;
            end else begin
              PDeltaPLC[plc].Devices[c].MaxBlockItems := PWordMaxBlockSize;
              PDeltaPLC[plc].Devices[c].MaxHole := PWordMaxHole;
            end;
          end;
        end;

        valido := (size > 0);
        if valido then
          PDeltaPLC[plc].Devices[dev].AddAddress(DeviceAddress(dev,mem),size,1,scantime);
      end;
    end;
  inherited DoAddTag(TagObj, valido);
end;

procedure TDeltaDriver.DoDelTag(TagObj:TTag);
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
      for plc:=0 to High(PDeltaPLC) do
        if PDeltaPLC[plc].Station = station then begin
          found := true;
          break;
        end;

      //se encontrou o plc remove a memoria que estou lendo dele.
      //if the slave was found, remove the tag from its memory.
      if found and (dev >= 0) then
        PDeltaPLC[plc].Devices[dev].RemoveAddress(DeviceAddress(dev,mem),size,1);
    end;
  inherited DoDelTag(TagObj);
end;

procedure TDeltaDriver.DoGetValue(TagObj:TTagRec; var values:TScanReadRec);
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
    for plc:=0 to High(PDeltaPLC) do
      if PDeltaPLC[plc].Station = TagObj.Station then begin
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

  PDeltaPLC[plc].Devices[dev].GetValues(DeviceAddress(dev,TagObj.Address),TagObj.Size,1,
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

function TDeltaDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
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
  if (dev >= 0) and (not DELTA_DEV_ISBIT[dev]) then begin
    Result := 16;
    ProtocolTagType := ptWord;
  end else begin
    Result := 1;
    ProtocolTagType := ptBit;
  end;
end;

function TDeltaDriver.LiteralTagAddress(aTag:TTag; aBlockTag:TTag):AnsiString;
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
    Result := DELTA_DEV_NAME[DeviceIndex(func)] + IntToStr(address)
  else
    Result := inherited LiteralTagAddress(aTag, aBlockTag);
end;

function TDeltaDriver.EncodePkg(TagObj:TTagRec; ToWrite:TArrayOfDouble; var ResultLen:LongInt):BYTES;
begin
  ResultLen := 0;
  Result := nil;
end;

function TDeltaDriver.DecodePkg(pkg:TIOPacket; out values:TArrayOfDouble):TProtocolIOResult;
begin
  values := nil;
  Result := ioDriverError;
end;

function TDeltaDriver.RemainingBytesRead(buffer:BYTES; TagObj:TTagRec):LongInt;
begin
  Result := 0;
end;

function TDeltaDriver.RemainingBytesWrite(buffer:BYTES):LongInt;
begin
  Result := 0;
end;

function TDeltaDriver.SendRead(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
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

function TDeltaDriver.SendWrite(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
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

function TDeltaDriver.ReadTagRec(const tr:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
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
  crit := DeviceCritical(dev);
  if DELTA_DEV_ISBIT[dev] then
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

function TDeltaDriver.WriteTagRec(const tr:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
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

  crit := DeviceCritical(dev);
  if DELTA_DEV_ISBIT[dev] then
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

function TDeltaDriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  tr:TTagRec;
begin
  tr := tagrec;
  NormalizeTagRec(tr);
  Result := ReadTagRec(tr,Values);
end;

function TDeltaDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  tr:TTagRec;
begin
  tr := tagrec;
  NormalizeTagRec(tr);
  Result := WriteTagRec(tr,Values);
end;

procedure TDeltaDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
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

    for plc:=0 to High(PDeltaPLC) do
      for dev:=0 to DELTA_DEV_COUNT-1 do
        if Assigned(PDeltaPLC[plc].Devices[dev]) then
          for block:=0 to High(PDeltaPLC[plc].Devices[dev].Blocks) do
            if PDeltaPLC[plc].Devices[dev].Blocks[block].NeedRefresh then begin
              found := true;
              BuildTagRec(PDeltaPLC[plc].Station, DELTA_DEV_FUNC[dev],
                          PDeltaPLC[plc].Devices[dev].Blocks[block].AddressStart,
                          PDeltaPLC[plc].Devices[dev].Blocks[block].Size, tr);
              FMustReleaseResources := true;
              ReadTagRec(tr,values);
              FMustReleaseResources := false;
            end else begin
              if first then begin
                first := false;
                minScan := PDeltaPLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate;
                lastPLC := plc;
                lastAddr := PDeltaPLC[plc].Devices[dev].Blocks[block].AddressStart;
                lastSize := PDeltaPLC[plc].Devices[dev].Blocks[block].Size;
                lastFunc := DELTA_DEV_FUNC[dev];
              end else
                if PDeltaPLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate > minScan then begin
                  minScan := PDeltaPLC[plc].Devices[dev].Blocks[block].MilisecondsFromLastUpdate;
                  lastPLC := plc;
                  lastAddr := PDeltaPLC[plc].Devices[dev].Blocks[block].AddressStart;
                  lastSize := PDeltaPLC[plc].Devices[dev].Blocks[block].Size;
                  lastFunc := DELTA_DEV_FUNC[dev];
                end;
            end;

    //nada venceu o tempo de varredura: le o bloco que esta a mais tempo sem
    //atualizacao para manter a comunicacao viva.
    //nothing is due: read the block that is waiting for the longest time to
    //keep the link alive.
    if (not found) then
      if (not first) and PReadSomethingAlways then begin
        BuildTagRec(PDeltaPLC[lastPLC].Station, lastFunc, lastAddr, lastSize, tr);
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
