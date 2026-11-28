{$i ../common/language.inc}
{:
  @abstract(Modbus RTU driver pre-configured for Xinje (信捷) XD/XL PLCs.)

  The PLCs of the XD/XL series expose their soft elements over Modbus RTU as
  standard coils (function 1/5) and holding registers (function 3/6). This unit
  keeps the Modbus RTU protocol stack of @link(ModBusSerial.TModBusRTUDriver)
  and adds the address translation described in chapter 6 of the XD/XL basic
  instruction manual.

  Drop @link(TXinJEModbusRTUDriver) on a form, connect it to a
  @link(SerialPort.TSerialPortDriver) configured as 19200, 8 data bits, 1 stop
  bit, even parity, then use @link(TXinJEModbusRTUDriver.SetupTag) (or
  @link(TXinJEModbusRTUDriver.SoftElementToModbus)) to configure each tag from
  its XDPPro name, for example @code(D100), @code(Y0) or @code(HD0).

  @bold(Important:) the X/Y elements are octal in the PLC. @code(Y10) means the
  ninth output, whose Modbus address is 24584 (H6008), not 24586.

  @seealso(ModBusSerial.TModBusRTUDriver)
  @seealso(XinJETCP.TXinJETCPDriver)
}
unit XinJEModbusRTU;

interface

uses
  Classes, SysUtils, ModBusSerial, ModBusDriver, Tag, PLCTagNumber;

type
  {: Address map used by the PLC family. }
  TXinJEModbusFamily = (
    xmXD3,   //: XD1, XD2, XD3, XD3E, XL1, XL3
    xmXD5,   //: XD5, XDM, XDC, XD5E, XDME, XL5, XL5E, XL5N, XL5H, XLME
    xmXDH    //: XDH, XLH
  );

  {$IFDEF PORTUGUES}
  {:
  @abstract(Driver Modbus RTU para CLPs Xinje XD/XL.)
  @seealso(ModBusSerial.TModBusRTUDriver)
  }
  {$ELSE}
  {:
  @abstract(Modbus RTU driver for Xinje XD/XL PLCs.)

  Connect it to a @link(TSerialPortDriver) and set @bold(PLCFamily) to match
  the PLC. Use @bold(SetupTag) to turn an XDPPro soft element name into the
  Modbus function/address of a @link(TPLCTagNumber).
  @seealso(ModBusSerial.TModBusRTUDriver)
  }
  {$ENDIF}
  TXinJEModbusRTUDriver = class(TModBusRTUDriver)
  private
    FFamily: TXinJEModbusFamily;
    procedure SetFamily(AValue: TXinJEModbusFamily);
  public
    constructor Create(AOwner: TComponent); override;

    {: Guesses the address family from a series name such as "XD3", "XD5E",
       "XLH". Unknown names default to xmXD3. }
    class function FamilyFromName(const AName: string): TXinJEModbusFamily;
    {: Human readable name of a family. }
    class function FamilyName(AFamily: TXinJEModbusFamily): string;

    {: Converts an XDPPro soft element name (D100, Y10, HD0, SM3, ...) into
       the Modbus function code and address used by this driver.
       @param ASoftElement name as written in XDPPro, e.g. @code(Y10).
       @param AFunc 1 for coils, 3 for holding registers.
       @param AAddress zero based Modbus address.
       @param AIsBit True for bit devices. }
    function SoftElementToModbus(const ASoftElement: string;
      out AFunc: LongInt; out AAddress: LongInt; out AIsBit: Boolean): Boolean;

    {: Configures an existing TPLCTagNumber from an XDPPro soft element name.
       Sets PLCStation, MemAddress, MemReadFunction, MemWriteFunction and the
       initial tag type. Returns False when the name is unknown. }
    function SetupTag(ATag: TPLCTagNumber; const ASoftElement: string;
      AStation: LongInt = 1): Boolean;
  published
    property PLCFamily: TXinJEModbusFamily read FFamily write SetFamily default xmXD3;
  end;

implementation

type
  TXinJEDevMap = record
    Name: string[4];
    IsBit: Boolean;
    Base: array[0..2] of LongInt;
    Count: array[0..2] of LongInt;
  end;

const
  FAM_SMALL = 0;
  FAM_LARGE = 1;
  FAM_HIGH  = 2;

  { Soft elements of chapter 6 of the XD/XL basic instruction manual.
    Count is the number of addresses in the continuous block that starts at
    Base. }
  XINJE_MODBUS_DEVICES: array[0..28] of TXinJEDevMap = (
    (Name:'M';    IsBit:True;  Base:(0,     0,     0    ); Count:(8000, 20480, 20480)),
    (Name:'X';    IsBit:True;  Base:(20480, 20480, 20480); Count:(64,   64,    64   )),
    (Name:'Y';    IsBit:True;  Base:(24576, 24576, 24576); Count:(64,   64,    64   )),
    (Name:'S';    IsBit:True;  Base:(28672, 28672, 28672); Count:(1024, 8000,  8000 )),
    (Name:'SM';   IsBit:True;  Base:(36864, 36864, 36864); Count:(2048, 4096,  4096 )),
    (Name:'T';    IsBit:True;  Base:(40960, 40960, 40960); Count:(576,  4096,  4096 )),
    (Name:'C';    IsBit:True;  Base:(45056, 45056, 45056); Count:(576,  4096,  4096 )),
    (Name:'ET';   IsBit:True;  Base:(49152, 49152, 49152); Count:(32,   40,    40   )),
    (Name:'SEM';  IsBit:True;  Base:(49280, 49280, 49280); Count:(32,   128,   128  )),
    (Name:'HM';   IsBit:True;  Base:(49408, 49408, 49408); Count:(960,  6144,  6144 )),
    (Name:'HS';   IsBit:True;  Base:(55552, 55552, 55552); Count:(128,  1000,  1000 )),
    (Name:'HT';   IsBit:True;  Base:(57600, 57600, 57600); Count:(96,   1024,  1024 )),
    (Name:'HC';   IsBit:True;  Base:(58624, 58624, 58624); Count:(96,   1024,  1024 )),
    (Name:'HSC';  IsBit:True;  Base:(59648, 59648, 59648); Count:(32,   40,    40   )),
    (Name:'D';    IsBit:False; Base:(0,     0,     0    ); Count:(8000, 20480, 20480)),
    (Name:'ID';   IsBit:False; Base:(20480, 20480, 20480); Count:(100,  100,   100  )),
    (Name:'QD';   IsBit:False; Base:(24576, 24576, 24576); Count:(100,  100,   100  )),
    (Name:'SD';   IsBit:False; Base:(28672, 28672, 28672); Count:(2048, 4096,  4096 )),
    (Name:'TD';   IsBit:False; Base:(32768, 32768, 32768); Count:(576,  4096,  4096 )),
    (Name:'CD';   IsBit:False; Base:(36864, 36864, 36864); Count:(576,  4096,  4096 )),
    (Name:'ETD';  IsBit:False; Base:(40960, 40960, 40960); Count:(32,   40,    40   )),
    (Name:'HD';   IsBit:False; Base:(41088, 41088, 41088); Count:(1000, 6144,  6144 )),
    (Name:'HSD';  IsBit:False; Base:(47232, 47232, 47232); Count:(500,  1024,  1024 )),
    (Name:'HTD';  IsBit:False; Base:(48256, 48256, 48256); Count:(96,   1024,  1024 )),
    (Name:'HCD';  IsBit:False; Base:(49280, 49280, 49280); Count:(96,   1024,  1024 )),
    (Name:'HSCD'; IsBit:False; Base:(50304, 50304, 50304); Count:(32,   40,    40   )),
    (Name:'FD';   IsBit:False; Base:(50368, 50368, 50368); Count:(5120, 8192,  8192 )),
    (Name:'SFD';  IsBit:False; Base:(58560, 58560, 58560); Count:(2000, 4096,  4096 )),
    (Name:'FS';   IsBit:False; Base:(62656, 62656, 62656); Count:(48,   48,    48   ))
  );

function FamilyIndex(AFamily: TXinJEModbusFamily): LongInt;
begin
  case AFamily of
    xmXD5: Result := FAM_LARGE;
    xmXDH: Result := FAM_HIGH;
  else
    Result := FAM_SMALL;
  end;
end;

function DecimalToInt(const S: string): LongInt;
var
  i, d: LongInt;
begin
  Result := 0;
  if S = '' then
  begin
    Result := -1;
    exit;
  end;
  for i := 1 to Length(S) do begin
    d := Ord(S[i]) - Ord('0');
    if (d < 0) or (d > 9) then
    begin
      Result := -1;
      exit;
    end;
    Result := Result * 10 + d;
  end;
end;

function OctalToInt(const S: string): LongInt;
var
  i, d: LongInt;
begin
  Result := 0;
  if S = '' then
  begin
    Result := -1;
    exit;
  end;
  for i := 1 to Length(S) do begin
    d := Ord(S[i]) - Ord('0');
    if (d < 0) or (d > 7) then
    begin
      Result := -1;
      exit;
    end;
    Result := Result * 8 + d;
  end;
end;

function FindDevice(const AName: string; out AIndex: LongInt; out ADigits: string): Boolean;
var
  s: string;
  i, n, best: LongInt;
begin
  Result := False;
  AIndex := -1;
  ADigits := '';
  s := UpperCase(Trim(AName));
  best := 0;
  for i := 0 to High(XINJE_MODBUS_DEVICES) do begin
    n := Length(XINJE_MODBUS_DEVICES[i].Name);
    if (Length(s) <= n) then
      continue;
    if not SameText(Copy(s, 1, n), XINJE_MODBUS_DEVICES[i].Name) then
      continue;
    if not (s[n+1] in ['0'..'9']) then
      continue;
    if n > best then begin
      best := n;
      AIndex := i;
    end;
  end;
  if AIndex >= 0 then begin
    ADigits := Copy(s, best + 1, Length(s) - best);
    Result := True;
  end;
end;

{ IR addresses such as X10000, X20100, Y11000, ID30000. The first three
  digits are octal and encode the expansion board; the last two digits are
  the bit/word index inside that board. }
function ExpandedAddress(const ADigits: string; AModuleBase, ABDBase, AEDBase: LongInt;
  AStep, AMaxModule: LongInt; out AValue: LongInt): Boolean;
var
  m3, sub: LongInt;
begin
  Result := False;
  AValue := -1;
  if Length(ADigits) <> 5 then
    exit;
  m3 := OctalToInt(Copy(ADigits, 1, 3));
  sub := OctalToInt(Copy(ADigits, 4, 2));
  if (m3 < 0) or (sub < 0) then
    exit;

  // octal 100..117 = module 1..16
  if (m3 >= 64) and (m3 <= 79) then begin
    if (m3 - 63) > AMaxModule then
      exit;
    AValue := AModuleBase + (m3 - 64) * AStep + sub;
    Result := True;
  end
  // octal 200..201 = BD board 1..2
  else if (m3 >= 128) and (m3 <= 129) then begin
    AValue := ABDBase + (m3 - 128) * AStep + sub;
    Result := True;
  end
  // octal 300 = ED board 1
  else if m3 = 192 then begin
    AValue := AEDBase + sub;
    Result := True;
  end;
end;

constructor TXinJEModbusRTUDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFamily := xmXD3;
  // Chapter 6 limits: 125 holding/input registers, 2000 coils.
  HoldingRegsMaxBlockSize := 125;
  AnalogRegsMaxBlockSize := 125;
  InputsMaxBlockSize := 2000;
  OutputsMaxBlockSize := 2000;
  PInternalDelayBetweenCmds := 0;
end;

procedure TXinJEModbusRTUDriver.SetFamily(AValue: TXinJEModbusFamily);
begin
  if FFamily = AValue then
    exit;
  FFamily := AValue;
end;

class function TXinJEModbusRTUDriver.FamilyFromName(const AName: string): TXinJEModbusFamily;
var
  s: string;
begin
  s := UpperCase(Trim(AName));
  if (Copy(s, 1, 3) = 'XDH') or (Copy(s, 1, 3) = 'XLH') then
    Result := xmXDH
  else if (Copy(s, 1, 3) = 'XD5') or (Copy(s, 1, 3) = 'XDM') or
          (Copy(s, 1, 3) = 'XDC') or (Copy(s, 1, 3) = 'XL5') or
          (Copy(s, 1, 5) = 'XLME') then
    Result := xmXD5
  else
    Result := xmXD3;
end;

class function TXinJEModbusRTUDriver.FamilyName(AFamily: TXinJEModbusFamily): string;
begin
  case AFamily of
    xmXD5: Result := 'XD5/XDM/XDC/XD5E/XDME/XL5/XL5E/XL5N/XL5H/XLME';
    xmXDH: Result := 'XDH/XLH';
  else
    Result := 'XD1/XD2/XD3/XD3E/XL1/XL3';
  end;
end;

function TXinJEModbusRTUDriver.SoftElementToModbus(const ASoftElement: string;
  out AFunc: LongInt; out AAddress: LongInt; out AIsBit: Boolean): Boolean;
var
  dev, fi, v, moduleBase, bdBase, edBase, step, maxModule: LongInt;
  digits: string;
begin
  Result := False;
  AFunc := 0;
  AAddress := -1;
  AIsBit := False;
  fi := FamilyIndex(FFamily);

  if not FindDevice(ASoftElement, dev, digits) then
    exit;

  AIsBit := XINJE_MODBUS_DEVICES[dev].IsBit;
  AFunc := 1;
  if not AIsBit then
    AFunc := 3;

  // Extension boards of X/Y/ID/QD use their own address map.
  if (Length(digits) = 5) and
     ((XINJE_MODBUS_DEVICES[dev].Name = 'X') or
      (XINJE_MODBUS_DEVICES[dev].Name = 'Y') or
      (XINJE_MODBUS_DEVICES[dev].Name = 'ID') or
      (XINJE_MODBUS_DEVICES[dev].Name = 'QD')) then begin
    if (XINJE_MODBUS_DEVICES[dev].Name = 'X') or (XINJE_MODBUS_DEVICES[dev].Name = 'ID') then begin
      moduleBase := $5100;
      bdBase := $58D0;
      edBase := $5BF0;
    end else if (XINJE_MODBUS_DEVICES[dev].Name = 'Y') or (XINJE_MODBUS_DEVICES[dev].Name = 'QD') then begin
      moduleBase := $6100;
      bdBase := $68D0;
      edBase := $6BF0;
    end else begin
      moduleBase := $5100;
      bdBase := $58D0;
      edBase := $5BF0;
    end;

    if (XINJE_MODBUS_DEVICES[dev].Name = 'ID') or (XINJE_MODBUS_DEVICES[dev].Name = 'QD') then
      step := 100
    else
      step := 64;

    if FFamily = xmXD3 then
      maxModule := 8
    else
      maxModule := 16;

    if not ExpandedAddress(digits, moduleBase, bdBase, edBase, step, maxModule, v) then
      exit;
    AAddress := v;
    Result := True;
    exit;
  end;

  if XINJE_MODBUS_DEVICES[dev].IsBit and
     ((XINJE_MODBUS_DEVICES[dev].Name = 'X') or (XINJE_MODBUS_DEVICES[dev].Name = 'Y')) then
    v := OctalToInt(digits)
  else
    v := DecimalToInt(digits);

  if (v < 0) or (v >= XINJE_MODBUS_DEVICES[dev].Count[fi]) then
    exit;

  AAddress := XINJE_MODBUS_DEVICES[dev].Base[fi] + v;
  Result := True;
end;

function TXinJEModbusRTUDriver.SetupTag(ATag: TPLCTagNumber;
  const ASoftElement: string; AStation: LongInt): Boolean;
var
  func, address: LongInt;
  isBit: Boolean;
begin
  Result := False;
  if ATag = nil then
    exit;
  if not SoftElementToModbus(ASoftElement, func, address, isBit) then
    exit;

  ATag.PLCStation := AStation;
  ATag.MemAddress := address;
  ATag.MemReadFunction := func;
  if isBit then
    ATag.MemWriteFunction := 5
  else
    ATag.MemWriteFunction := 6;
  Result := True;
end;

end.
