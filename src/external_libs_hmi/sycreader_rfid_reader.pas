unit sycreader_rfid_reader;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLType,
  ChipCardReader, crossthreads, {$IFNDEF WINDOWS}hidapi,{$ENDIF} syncobjs, cp1250;

type
  TStrRec = record
    StrBuffer:String;
  end;
  PStrRec = ^TStrRec;

{$IFDEF WINDOWS}
  { TSycreaderRFID_RawInputReader

    Windows backend. Windows opens HID keyboards exclusively, so libhidapi
    can't read the reader there. The keystrokes of the reader are taken from
    the Raw Input API instead (only the ones of the device that matches
    VID/PID/SN) and, while the reader is started, a keyboard hook of the main
    thread drops them before they reach the controls of the application, like
    the udev rule does on Linux.

    Must be started and stopped from the main thread: the Raw Input window and
    the keyboard hook live there and depend on its message loop. }

  TSycreaderRFID_RawInputReader = class
  private
    FVID,
    FPID:Word;
    FSN:String;
    FStarted,
    FReady,
    FHasCur:Boolean;
    FNextBuffer,
    FCurBuffer:String;
    procedure ConfigChanged;
    procedure SetPID(AValue: Word);
    procedure SetSN(AValue: String);
    procedure SetVID(AValue: Word);
    procedure ProcessKey(aMakeCode, aFlags:Word);
  public
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function getLastHIDRead(out strBuffer:UTF8String):Boolean;
    function ReadPending:Boolean;
    property VID:Word read FVID write SetVID;
    property PID:Word read FPID write SetPID;
    property SN:String read FSN write SetSN;
    property Initialized:Boolean read FReady;
  end;
{$ELSE}
  { TSycreaderRFID_USBHIDReader }

  TSycreaderRFID_USBHIDReader = class(TpSCADACoreAffinityThreadWithLoop)
  private
    FVID,
    FLastVID,
    FPID,
    FLastPID,
    FReady:LongWord;
    FSN,
    FLastSN:String;
    FSNMutex:TCriticalSection;
    Device : PHidDevice;
    FNextBuffer,
    FCurBuffer:PStrRec;
    function GetPID: Word;
    function GetReady: Boolean;
    function GetSN: String;
    function GetVID: Word;
    procedure SetPID(AValue: Word);
    procedure SetSN(AValue: String);
    procedure SetVID(AValue: Word);
  protected
    procedure Loop; override;
    procedure LoopTerminated; override;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
  DefaultStackSize);
    destructor Destroy; override;
    function getLastHIDRead(out strBuffer:UTF8String):Boolean;
    function ReadPending:Boolean;
    property VID:Word read GetVID write SetVID;
    property PID:Word read GetPID write SetPID;
    property SN:String read GetSN write SetSN;
    property Initialized:Boolean read GetReady;
  end;
{$ENDIF}

  { TSycRFIDReader }

  TSycRFIDReader = class(TChipCardReader)
  private
    FSycRFIDReader:{$IFDEF WINDOWS}TSycreaderRFID_RawInputReader{$ELSE}TSycreaderRFID_USBHIDReader{$ENDIF};
    function GetPID: WORD;
    function GetSN: String;
    function GetVID: WORD;
    procedure SetPID(AValue: WORD);
    procedure SetSN(AValue: String);
    procedure SetVID(AValue: WORD);
  protected
    procedure Loaded; override;
  public
    function InitializeChipCard:Boolean; override;
    function IsEmptyChipCard:Boolean; override;
    function ChipCardReady: Boolean; override;
    function ChipCardRead(var aChipCardCode:UTF8String):Boolean; override;
    function FinishChipCard:Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property VendorID:WORD read GetVID write SetVID;
    property ProductID:WORD read GetPID write SetPID;
    property SerialNumber:String read GetSN write SetSN;
  end;

var
  map:array[0..511] of char;

procedure Register;

implementation

uses hsstrings{$IFDEF WINDOWS}, Windows{$ENDIF};

procedure Register;
begin
  RegisterComponents(strUserManagement,[TSycRFIDReader]);
end;

{$IFDEF WINDOWS}
// Raw Input gives scan codes (set 1), while the Linux backend gets HID usage
// codes straight from the keyboard reports. Converting the scan code to the
// HID usage keeps one decoding (map[] and the terminator rule) for both, and
// doesn't depend on the keyboard layout of Windows. Zero means "ignore" (the
// modifiers and the lock keys, that the Linux backend never sees in byte 2).
const
  SycScanToHIDUsage:array[$00..$58] of Byte = (
    $00,$29,$1E,$1F,$20,$21,$22,$23,$24,$25,$26,$27,$2D,$2E,$2A,$2B, // row 00
    $14,$1A,$08,$15,$17,$1C,$18,$0C,$12,$13,$2F,$30,$28,$00,$04,$16, // row 10
    $07,$09,$0A,$0B,$0D,$0E,$0F,$33,$34,$35,$00,$31,$1D,$1B,$06,$19, // row 20
    $05,$11,$10,$36,$37,$38,$00,$55,$00,$2C,$00,$3A,$3B,$3C,$3D,$3E, // row 30
    $3F,$40,$41,$42,$43,$00,$00,$5F,$60,$61,$56,$5C,$5D,$5E,$57,$59, // row 40
    $5A,$5B,$62,$63,$00,$00,$64,$44,$45); // row 50

  // keystrokes of the reader not seen by the keyboard hook (the application
  // wasn't in foreground) are forgotten after this time.
  SycSwallowTimeout = 1000;

  SycHWND_MESSAGE = HWND(-3);
  SycWndClassName:UnicodeString = 'PascalSCADA_SycRFIDRawInput';

function HidD_GetSerialNumberString(HidDeviceObject:THandle; Buffer:Pointer;
  BufferLength:ULONG):ByteBool; stdcall; external 'hid.dll';

function ScanCodeToHIDUsage(aMakeCode:Word; aE0:Boolean):Byte;
begin
  Result:=0;
  if aE0 then begin
    case aMakeCode of
      $1C: Result:=$58; //keypad enter
      $35: Result:=$54; //keypad /
      $47: Result:=$4A; //home
      $48: Result:=$52; //up
      $49: Result:=$4B; //page up
      $4B: Result:=$50; //left
      $4D: Result:=$4F; //right
      $4F: Result:=$4D; //end
      $50: Result:=$51; //down
      $51: Result:=$4E; //page down
      $52: Result:=$49; //insert
      $53: Result:=$4C; //delete
    end;
  end else
    if aMakeCode<=High(SycScanToHIDUsage) then
      Result:=SycScanToHIDUsage[aMakeCode];
end;

type
  TSycRawDevInfo = record
    Handle:THandle;
    VID,
    PID:Word;
    Name,
    SN:UnicodeString;
    SNRead:Boolean;
  end;

  TSycSwallowEntry = record
    Scan:Word;
    E0,
    Up:Boolean;
    Tick:QWord;
  end;

  { TSycRawInputDispatcher

    Only one Raw Input target is allowed per process for the keyboards, so a
    single message-only window serves all TSycreaderRFID_RawInputReader
    instances. It exists while at least one reader is started. }

  TSycRawInputDispatcher = class
  private
    FWnd:HWND;
    FHook:HHOOK;
    FReaders:TFPList;
    FDevs:array of TSycRawDevInfo;
    FSwallow:array of TSycSwallowEntry;
    function DevIndex(aDev:THandle):Integer;
    function DeviceMatches(aDev:THandle; aReader:TSycreaderRFID_RawInputReader):Boolean;
    procedure PruneSwallow;
    function TakeSwallow(aScan:Word; aE0, aUp:Boolean):Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(aReader:TSycreaderRFID_RawInputReader);
    function Detach(aReader:TSycreaderRFID_RawInputReader):Boolean;
    procedure RefreshReady;
    procedure HandleRawInput(aRawInput:HRAWINPUT);
    procedure HandleDeviceChange(aChange:WPARAM; aDev:THandle);
    function HookKey(aScan:Word; aE0, aUp:Boolean):Boolean;
  end;

var
  SycDispatcher:TSycRawInputDispatcher = nil;
  SycWndClassRegistered:Boolean = false;

function SycRawInputWndProc(aWnd:HWND; aMsg:UINT; aWParam:WPARAM;
  aLParam:LPARAM):LRESULT; stdcall;
begin
  if Assigned(SycDispatcher) and (aWnd=SycDispatcher.FWnd) then
    case aMsg of
      WM_INPUT:
        SycDispatcher.HandleRawInput(HRAWINPUT(aLParam));
      WM_INPUT_DEVICE_CHANGE: begin
        SycDispatcher.HandleDeviceChange(aWParam, THandle(aLParam));
        exit(0);
      end;
    end;
  //WM_INPUT also needs DefWindowProc, for the cleanup of the raw input.
  Result:=DefWindowProcW(aWnd, aMsg, aWParam, aLParam);
end;

function SycKeyboardHookProc(aCode:longint; aWParam:WPARAM;
  aLParam:LPARAM):LRESULT; stdcall;
var
  flags:PtrUInt;
begin
  if (aCode=HC_ACTION) and Assigned(SycDispatcher) then begin
    flags:=PtrUInt(aLParam);
    if SycDispatcher.HookKey((flags shr 16) and $FF,
                             (flags and (1 shl 24))<>0,
                             (flags and (PtrUInt(1) shl 31))<>0) then
      exit(1);
  end;
  Result:=CallNextHookEx(0, aCode, aWParam, aLParam);
end;

{ TSycRawInputDispatcher }

constructor TSycRawInputDispatcher.Create;
var
  wc:WNDCLASSW;
  rid:RAWINPUTDEVICE;
begin
  inherited Create;
  FReaders:=TFPList.Create;

  if not SycWndClassRegistered then begin
    FillChar(wc, SizeOf(wc), 0);
    wc.lpfnWndProc:=@SycRawInputWndProc;
    wc.hInstance:=HInstance;
    wc.lpszClassName:=PWideChar(SycWndClassName);
    SycWndClassRegistered:=RegisterClassW(wc)<>0;
  end;

  FWnd:=CreateWindowExW(0, PWideChar(SycWndClassName), nil, 0, 0, 0, 0, 0,
                        SycHWND_MESSAGE, 0, HInstance, nil);
  if FWnd<>0 then begin
    rid.usUsagePage:=1; //generic desktop
    rid.usUsage:=6;     //keyboard
    rid.dwFlags:=RIDEV_INPUTSINK or RIDEV_DEVNOTIFY;
    rid.hwndTarget:=FWnd;
    if not RegisterRawInputDevices(@rid, 1, SizeOf(rid)) then begin
      DestroyWindow(FWnd);
      FWnd:=0;
    end;
  end;

  if FWnd<>0 then
    FHook:=SetWindowsHookExW(WH_KEYBOARD, @SycKeyboardHookProc, 0, GetCurrentThreadId);
end;

destructor TSycRawInputDispatcher.Destroy;
var
  rid:RAWINPUTDEVICE;
begin
  if FHook<>0 then
    UnhookWindowsHookEx(FHook);
  if FWnd<>0 then begin
    rid.usUsagePage:=1;
    rid.usUsage:=6;
    rid.dwFlags:=RIDEV_REMOVE;
    rid.hwndTarget:=0;
    RegisterRawInputDevices(@rid, 1, SizeOf(rid));
    DestroyWindow(FWnd);
  end;
  FreeAndNil(FReaders);
  inherited Destroy;
end;

procedure TSycRawInputDispatcher.Attach(aReader: TSycreaderRFID_RawInputReader);
begin
  if FReaders.IndexOf(aReader)<0 then
    FReaders.Add(aReader);
  RefreshReady;
end;

function TSycRawInputDispatcher.Detach(aReader: TSycreaderRFID_RawInputReader
  ): Boolean;
begin
  FReaders.Remove(aReader);
  aReader.FReady:=false;
  Result:=FReaders.Count=0;
end;

function TSycRawInputDispatcher.DevIndex(aDev: THandle): Integer;
var
  len:UINT;
  upName:String;
  p:Integer;
begin
  for Result:=0 to High(FDevs) do
    if FDevs[Result].Handle=aDev then exit;

  Result:=Length(FDevs);
  SetLength(FDevs, Result+1);
  FDevs[Result].Handle:=aDev;
  FDevs[Result].VID:=0;
  FDevs[Result].PID:=0;
  FDevs[Result].Name:='';
  FDevs[Result].SN:='';
  FDevs[Result].SNRead:=false;

  // \\?\HID#VID_FFFF&PID_0035#7&2a1c...&0&0000#{884b96c3-...}
  len:=0;
  if (GetRawInputDeviceInfoW(aDev, RIDI_DEVICENAME, nil, @len)=0) and (len>0) then begin
    SetLength(FDevs[Result].Name, len);
    if GetRawInputDeviceInfoW(aDev, RIDI_DEVICENAME, PWideChar(FDevs[Result].Name), @len)<>UINT(-1) then begin
      FDevs[Result].Name:=PWideChar(FDevs[Result].Name);
      upName:=UpperCase(String(FDevs[Result].Name));
      p:=Pos('VID_', upName);
      if p>0 then FDevs[Result].VID:=StrToIntDef('$'+Copy(upName, p+4, 4), 0);
      p:=Pos('PID_', upName);
      if p>0 then FDevs[Result].PID:=StrToIntDef('$'+Copy(upName, p+4, 4), 0);
    end else
      FDevs[Result].Name:='';
  end;
end;

function TSycRawInputDispatcher.DeviceMatches(aDev: THandle;
  aReader: TSycreaderRFID_RawInputReader): Boolean;
var
  i:Integer;
  h:THandle;
  path:UnicodeString;
  buf:array[0..127] of WideChar;
begin
  if aDev=0 then exit(false); //injected (SendInput) keystrokes
  i:=DevIndex(aDev);
  if (FDevs[i].VID<>aReader.FVID) or (FDevs[i].PID<>aReader.FPID) then
    exit(false);
  if aReader.FSN='' then
    exit(true);

  if not FDevs[i].SNRead then begin
    FDevs[i].SNRead:=true;
    path:=FDevs[i].Name;
    if Copy(path, 1, 4)='\??\' then path[2]:='\'; //Windows XP
    //no access rights needed to query the strings, even of a keyboard.
    h:=CreateFileW(PWideChar(path), 0, FILE_SHARE_READ or FILE_SHARE_WRITE,
                   nil, OPEN_EXISTING, 0, 0);
    if h<>INVALID_HANDLE_VALUE then begin
      FillChar(buf, SizeOf(buf), 0);
      if HidD_GetSerialNumberString(h, @buf, SizeOf(buf)-SizeOf(WideChar)) then
        FDevs[i].SN:=PWideChar(@buf);
      CloseHandle(h);
    end;
  end;
  Result:=FDevs[i].SN=UnicodeString(aReader.FSN);
end;

procedure TSycRawInputDispatcher.RefreshReady;
var
  list:array of RAWINPUTDEVICELIST;
  n:UINT;
  i, d, r:Integer;
  found:Boolean;
  reader:TSycreaderRFID_RawInputReader;
begin
  list:=nil;
  n:=0;
  if GetRawInputDeviceList(nil, @n, SizeOf(RAWINPUTDEVICELIST))=0 then begin
    SetLength(list, n);
    if (n>0) and (GetRawInputDeviceList(@list[0], @n, SizeOf(RAWINPUTDEVICELIST))=UINT(-1)) then
      n:=0;
    SetLength(list, n);
  end;

  //forget the devices that are gone (handles can be reused).
  for d:=High(FDevs) downto 0 do begin
    found:=false;
    for i:=0 to High(list) do
      if list[i].hDevice=FDevs[d].Handle then begin
        found:=true;
        break;
      end;
    if not found then
      Delete(FDevs, d, 1);
  end;

  for r:=0 to FReaders.Count-1 do begin
    reader:=TSycreaderRFID_RawInputReader(FReaders[r]);
    reader.FReady:=false;
    if FWnd=0 then continue;
    for i:=0 to High(list) do
      if (list[i].dwType=RIM_TYPEKEYBOARD) and DeviceMatches(list[i].hDevice, reader) then begin
        reader.FReady:=true;
        break;
      end;
  end;
end;

procedure TSycRawInputDispatcher.HandleRawInput(aRawInput: HRAWINPUT);
var
  ri:RAWINPUT;
  sz:UINT;
  r:Integer;
  claimed:Boolean;
  reader:TSycreaderRFID_RawInputReader;
begin
  sz:=SizeOf(ri);
  if GetRawInputData(aRawInput, RID_INPUT, @ri, @sz, SizeOf(RAWINPUTHEADER))=UINT(-1) then
    exit;
  if ri.header.dwType<>RIM_TYPEKEYBOARD then
    exit;

  claimed:=false;
  for r:=0 to FReaders.Count-1 do begin
    reader:=TSycreaderRFID_RawInputReader(FReaders[r]);
    if DeviceMatches(ri.header.hDevice, reader) then begin
      //a keystroke means the device is there, even if the arrival was missed.
      reader.FReady:=true;
      reader.ProcessKey(ri.data.keyboard.MakeCode, ri.data.keyboard.Flags);
      claimed:=true;
    end;
  end;

  if claimed then begin
    PruneSwallow;
    r:=Length(FSwallow);
    SetLength(FSwallow, r+1);
    FSwallow[r].Scan:=ri.data.keyboard.MakeCode;
    FSwallow[r].E0:=(ri.data.keyboard.Flags and RI_KEY_E0)<>0;
    FSwallow[r].Up:=(ri.data.keyboard.Flags and RI_KEY_BREAK)<>0;
    FSwallow[r].Tick:=GetTickCount64;
  end;
end;

procedure TSycRawInputDispatcher.HandleDeviceChange(aChange: WPARAM;
  aDev: THandle);
var
  i:Integer;
begin
  if aChange=GIDC_REMOVAL then
    for i:=High(FDevs) downto 0 do
      if FDevs[i].Handle=aDev then
        Delete(FDevs, i, 1);
  RefreshReady;
end;

procedure TSycRawInputDispatcher.PruneSwallow;
var
  now_:QWord;
  i:Integer;
begin
  now_:=GetTickCount64;
  for i:=High(FSwallow) downto 0 do
    if now_-FSwallow[i].Tick>SycSwallowTimeout then
      Delete(FSwallow, i, 1);
end;

function TSycRawInputDispatcher.TakeSwallow(aScan: Word; aE0, aUp: Boolean
  ): Boolean;
var
  i:Integer;
begin
  for i:=0 to High(FSwallow) do
    if (FSwallow[i].Scan=aScan) and (FSwallow[i].E0=aE0) and (FSwallow[i].Up=aUp) then begin
      Delete(FSwallow, i, 1);
      exit(true);
    end;
  Result:=false;
end;

function TSycRawInputDispatcher.HookKey(aScan: Word; aE0, aUp: Boolean
  ): Boolean;
var
  msg:TMsg;
begin
  PruneSwallow;
  Result:=TakeSwallow(aScan, aE0, aUp);
  if Result or (FWnd=0) then exit;

  //the WM_INPUT of this keystroke may still be queued behind it.
  while PeekMessageW(msg, FWnd, WM_INPUT, WM_INPUT, PM_REMOVE) do
    DispatchMessageW(msg);
  Result:=TakeSwallow(aScan, aE0, aUp);
end;

{ TSycreaderRFID_RawInputReader }

procedure TSycreaderRFID_RawInputReader.ConfigChanged;
begin
  FNextBuffer:='';
  if FStarted and Assigned(SycDispatcher) then
    SycDispatcher.RefreshReady;
end;

procedure TSycreaderRFID_RawInputReader.SetPID(AValue: Word);
begin
  if FPID=AValue then Exit;
  FPID:=AValue;
  ConfigChanged;
end;

procedure TSycreaderRFID_RawInputReader.SetSN(AValue: String);
begin
  if FSN=AValue then Exit;
  FSN:=AValue;
  ConfigChanged;
end;

procedure TSycreaderRFID_RawInputReader.SetVID(AValue: Word);
begin
  if FVID=AValue then Exit;
  FVID:=AValue;
  ConfigChanged;
end;

procedure TSycreaderRFID_RawInputReader.ProcessKey(aMakeCode, aFlags: Word);
var
  usage:Byte;
begin
  //only the key presses, like the Linux backend (a release report has no key).
  if (aFlags and (RI_KEY_BREAK or RI_KEY_E1))<>0 then exit;

  usage:=ScanCodeToHIDUsage(aMakeCode, (aFlags and RI_KEY_E0)<>0);
  if usage=0 then exit;

  if usage in [$04..$27,$2c..$38,$59..$62] then
    FNextBuffer:=FNextBuffer+map[usage]
  else begin
    //any other key (the Enter sent by the reader) ends the code.
    if FNextBuffer<>'' then begin
      FCurBuffer:=FNextBuffer;
      FHasCur:=true;
    end;
    FNextBuffer:='';
  end;
end;

destructor TSycreaderRFID_RawInputReader.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TSycreaderRFID_RawInputReader.Start;
begin
  if FStarted then exit;
  if SycDispatcher=nil then
    SycDispatcher:=TSycRawInputDispatcher.Create;
  FStarted:=true;
  FNextBuffer:='';
  SycDispatcher.Attach(Self);
end;

procedure TSycreaderRFID_RawInputReader.Stop;
begin
  if not FStarted then exit;
  FStarted:=false;
  if Assigned(SycDispatcher) and SycDispatcher.Detach(Self) then
    FreeAndNil(SycDispatcher);
end;

function TSycreaderRFID_RawInputReader.getLastHIDRead(out
  strBuffer: UTF8String): Boolean;
begin
  strBuffer:='';
  Result:=FHasCur;
  if Result then begin
    strBuffer:=FCurBuffer;
    FCurBuffer:='';
    FHasCur:=false;
  end;
end;

function TSycreaderRFID_RawInputReader.ReadPending: Boolean;
begin
  Result:=FHasCur;
end;

{$ELSE}
{ TSycreaderRFID_USBHIDReader }

function TSycreaderRFID_USBHIDReader.GetPID: Word;
var
  aux:LongWord;
begin
  InterlockedExchange(aux,FPID);
  Result:=aux;
end;

function TSycreaderRFID_USBHIDReader.GetReady: Boolean;
var
  aux:LongWord;
begin
  InterlockedExchange(aux,FReady);
  Result:=aux<>0;
end;

function TSycreaderRFID_USBHIDReader.GetSN: String;
begin
  try
    FSNMutex.Enter;
    Result:=FSN;
  finally
    FSNMutex.Leave;
  end;
end;

function TSycreaderRFID_USBHIDReader.GetVID: Word;
var
  aux:LongWord;
begin
  InterlockedExchange(aux,FVID);
  Result:=aux;
end;

procedure TSycreaderRFID_USBHIDReader.SetPID(AValue: Word);
var
  aux:LongWord;
begin
  aux:=AValue;
  InterlockedExchange(FPID,aux);
end;

procedure TSycreaderRFID_USBHIDReader.SetSN(AValue: String);
begin
  try
    FSNMutex.Enter;
    FSN:=AValue;
  finally
    FSNMutex.Leave;
  end;
end;

procedure TSycreaderRFID_USBHIDReader.SetVID(AValue: Word);
var
  aux:LongWord;
begin
  aux:=AValue;
  InterlockedExchange(FVID,aux);
end;

procedure TSycreaderRFID_USBHIDReader.Loop;
var
  aux:LongWord;
  ShouldReopen: Boolean = false;
  Buffer: array[0..511] of Byte;
  Num: SizeInt;
  auxptr: PStrRec;
begin
  InterlockedExchange(aux, FPID);
  ShouldReopen:=ShouldReopen or (aux<>FLastPID);
  FLastPID:=aux;

  InterlockedExchange(aux, FVID);
  ShouldReopen:=ShouldReopen or (aux<>FLastVID);
  FLastVID:=aux;

  try
    FSNMutex.Enter;
    ShouldReopen:=ShouldReopen or (FSN<>FLastSN);
    FLastSN:=FSN;
  finally
    FSNMutex.Leave;
  end;

  if ShouldReopen or (Device=nil) then begin
    InterlockedExchange(FReady, 0);

    if assigned(Device) then begin
      Device^.Close;
      Device:=nil;
    end;

    Device := THidDevice.Open(FLastVID, FLastPID, FLastSN);
  end;

  if Assigned(Device) then begin
    InterlockedExchange(FReady, 1);
    Num := Device^.ReadTimeout(Buffer, SizeOf(Buffer),1000);

    if num<0 then begin
      Device^.Close;
      Device:=nil
    end;

    if Num<>8 then exit;
    if FNextBuffer=nil  then begin
      New(FNextBuffer);
      FNextBuffer^.StrBuffer:='';
    end;

    if buffer[2] in [0,1,$04..$27,$2c..$38,$59..$62]  then begin
      if buffer[2]>=$04 then begin
        FNextBuffer^.StrBuffer:=FNextBuffer^.StrBuffer+map[buffer[2]];
      end;
    end else begin
      auxptr := InterlockedExchange(FCurBuffer, FNextBuffer);
      if Assigned(auxptr) then
        Dispose(auxptr);
      FNextBuffer:=nil;
    end;
  end else begin
    Sleep(50);
  end;
end;

procedure TSycreaderRFID_USBHIDReader.LoopTerminated;
begin
  inherited LoopTerminated;
  if Assigned(Device) then begin
    Device^.Close;
    Device:=nil;
  end;
end;

constructor TSycreaderRFID_USBHIDReader.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FSNMutex:=TCriticalSection.Create;
  FReady:=0;
end;

destructor TSycreaderRFID_USBHIDReader.Destroy;
begin
  FreeAndNil(FSNMutex);
  inherited Destroy;
end;

function TSycreaderRFID_USBHIDReader.getLastHIDRead(out strBuffer: UTF8String
  ): Boolean;
var
  auxptr: PStrRec;
begin
  Result:=false;
  auxptr:=InterlockedExchange(FCurBuffer,nil);
  if Assigned(auxptr) then begin
    strBuffer:=auxptr^.StrBuffer;
    dispose(auxptr);
    exit(true);
  end;
end;

function TSycreaderRFID_USBHIDReader.ReadPending: Boolean;
var
  auxptr: PStrRec;
begin
  auxptr:=InterlockedExchange(FCurBuffer,FCurBuffer);
  Result:=Assigned(auxptr);
end;
{$ENDIF}

{ TSycRFIDReader }

function TSycRFIDReader.GetPID: WORD;
begin
  Result:=FSycRFIDReader.PID;
end;

function TSycRFIDReader.GetSN: String;
begin
  Result:=FSycRFIDReader.SN;
end;

function TSycRFIDReader.GetVID: WORD;
begin
  Result:=FSycRFIDReader.VID;
end;

procedure TSycRFIDReader.SetPID(AValue: WORD);
begin
  FSycRFIDReader.PID:=AValue;
end;

procedure TSycRFIDReader.SetSN(AValue: String);
begin
  FSycRFIDReader.SN:=AValue;
end;

procedure TSycRFIDReader.SetVID(AValue: WORD);
begin
  FSycRFIDReader.VID:=aValue;
end;

procedure TSycRFIDReader.Loaded;
begin
  inherited Loaded;
  if ([csDesigning]*ComponentState)=[] then
    {$IFDEF WINDOWS}
    FSycRFIDReader.Start;
    {$ELSE}
    FSycRFIDReader.WakeUp;
    {$ENDIF}
end;

function TSycRFIDReader.InitializeChipCard: Boolean;
begin
  if ([csDesigning]*ComponentState)=[] then begin
    {$IFDEF WINDOWS}
    FSycRFIDReader.Start;
    {$ELSE}
    FSycRFIDReader.WakeUp;
    FSycRFIDReader.LoopStarted(1000);
    {$ENDIF}
    Result:=FSycRFIDReader.Initialized;
  end else
    exit(true);
end;

function TSycRFIDReader.IsEmptyChipCard: Boolean;
var
  aux: UTF8String;
begin
  Result:=false;
  repeat
    ;
  until FSycRFIDReader.getLastHIDRead(aux)=false;
  Result:=true;
end;

function TSycRFIDReader.ChipCardReady: Boolean;
begin
  Result:=FSycRFIDReader.Initialized;
end;

function TSycRFIDReader.ChipCardRead(var aChipCardCode: UTF8String): Boolean;
begin
  Result:=FSycRFIDReader.getLastHIDRead(aChipCardCode);
end;

function TSycRFIDReader.FinishChipCard: Boolean;
begin
  Result:=true;
end;

constructor TSycRFIDReader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF WINDOWS}
  FSycRFIDReader:=TSycreaderRFID_RawInputReader.Create;
  {$ELSE}
  FSycRFIDReader:=TSycreaderRFID_USBHIDReader.Create(true);
  {$ENDIF}
end;

destructor TSycRFIDReader.Destroy;
begin
  {$IFDEF WINDOWS}
  FreeAndNil(FSycRFIDReader);
  {$ELSE}
  if FSycRFIDReader.Suspended then begin
    FSycRFIDReader.WakeUp;
    FSycRFIDReader.LoopStarted(1000);
  end;

  FSycRFIDReader.Terminate;

  repeat
    CheckSynchronize(10);
  until FSycRFIDReader.WaitEnd(10)=wrSignaled;
  {$ENDIF}

  inherited Destroy;
end;

initialization
map[$0] := #0; //KEY_NONE 0x00 // No key pressed
map[$1] := #1; //KEY_ERR_OVF 0x01 //  Keyboard Error Roll Over - used for all slots if too many keys are pressed ("Phantom key")
map[$004] := 'a'; //KEY_A 0x04 // Keyboard a and A
map[$005] := 'b'; //KEY_B 0x05 // Keyboard b and B
map[$006] := 'c'; //KEY_C 0x06 // Keyboard c and C
map[$007] := 'd'; //KEY_D 0x07 // Keyboard d and D
map[$008] := 'e'; //KEY_E 0x08 // Keyboard e and E
map[$009] := 'f'; //KEY_F 0x09 // Keyboard f and F
map[$00a] := 'g'; //KEY_G 0x0a // Keyboard g and G
map[$00b] := 'h'; //KEY_H 0x0b // Keyboard h and H
map[$00c] := 'i'; //KEY_I 0x0c // Keyboard i and I
map[$00d] := 'j'; //KEY_J 0x0d // Keyboard j and J
map[$00e] := 'k'; //KEY_K 0x0e // Keyboard k and K
map[$00f] := 'l'; //KEY_L 0x0f // Keyboard l and L
map[$010] := 'm'; //KEY_M 0x10 // Keyboard m and M
map[$011] := 'n'; //KEY_N 0x11 // Keyboard n and N
map[$012] := 'o'; //KEY_O 0x12 // Keyboard o and O
map[$013] := 'p'; //KEY_P 0x13 // Keyboard p and P
map[$014] := 'q'; //KEY_Q 0x14 // Keyboard q and Q
map[$015] := 'r'; //KEY_R 0x15 // Keyboard r and R
map[$016] := 's'; //KEY_S 0x16 // Keyboard s and S
map[$017] := 't'; //KEY_T 0x17 // Keyboard t and T
map[$018] := 'u'; //KEY_U 0x18 // Keyboard u and U
map[$019] := 'v'; //KEY_V 0x19 // Keyboard v and V
map[$01a] := 'w'; //KEY_W 0x1a // Keyboard w and W
map[$01b] := 'x'; //KEY_X 0x1b // Keyboard x and X
map[$01c] := 'y'; //KEY_Y 0x1c // Keyboard y and Y
map[$01d] := 'z'; //KEY_Z 0x1d // Keyboard z and Z
map[$104] := 'A'; //KEY_A 0x04 // Keyboard a and A
map[$105] := 'B'; //KEY_B 0x05 // Keyboard b and B
map[$106] := 'C'; //KEY_C 0x06 // Keyboard c and C
map[$107] := 'D'; //KEY_D 0x07 // Keyboard d and D
map[$108] := 'E'; //KEY_E 0x08 // Keyboard e and E
map[$109] := 'F'; //KEY_F 0x09 // Keyboard f and F
map[$10a] := 'G'; //KEY_G 0x0a // Keyboard g and G
map[$10b] := 'H'; //KEY_H 0x0b // Keyboard h and H
map[$10c] := 'I'; //KEY_I 0x0c // Keyboard i and I
map[$10d] := 'J'; //KEY_J 0x0d // Keyboard j and J
map[$10e] := 'K'; //KEY_K 0x0e // Keyboard k and K
map[$10f] := 'L'; //KEY_L 0x0f // Keyboard l and L
map[$110] := 'M'; //KEY_M 0x10 // Keyboard m and M
map[$111] := 'N'; //KEY_N 0x11 // Keyboard n and N
map[$112] := 'O'; //KEY_O 0x12 // Keyboard o and O
map[$113] := 'P'; //KEY_P 0x13 // Keyboard p and P
map[$114] := 'Q'; //KEY_Q 0x14 // Keyboard q and Q
map[$115] := 'R'; //KEY_R 0x15 // Keyboard r and R
map[$116] := 'S'; //KEY_S 0x16 // Keyboard s and S
map[$117] := 'T'; //KEY_T 0x17 // Keyboard t and T
map[$118] := 'U'; //KEY_U 0x18 // Keyboard u and U
map[$119] := 'V'; //KEY_V 0x19 // Keyboard v and V
map[$11a] := 'W'; //KEY_W 0x1a // Keyboard w and W
map[$11b] := 'X'; //KEY_X 0x1b // Keyboard x and X
map[$11c] := 'Y'; //KEY_Y 0x1c // Keyboard y and Y
map[$11d] := 'Z'; //KEY_Z 0x1d // Keyboard z and Z

map[$01e] := '1'; //KEY_1 0x1e // Keyboard 1 and !
map[$01f] := '2'; //KEY_2 0x1f // Keyboard 2 and @
map[$020] := '3'; //KEY_3 0x20 // Keyboard 3 and #
map[$021] := '4'; //KEY_4 0x21 // Keyboard 4 and $
map[$022] := '5'; //KEY_5 0x22 // Keyboard 5 and %
map[$023] := '6'; //KEY_6 0x23 // Keyboard 6 and ^
map[$024] := '7'; //KEY_7 0x24 // Keyboard 7 and &
map[$025] := '8'; //KEY_8 0x25 // Keyboard 8 and *
map[$026] := '9'; //KEY_9 0x26 // Keyboard 9 and (
map[$027] := '0'; //KEY_0 0x27 // Keyboard 0 and )
map[$11e] := '!'; //KEY_1 0x1e // Keyboard 1 and !
map[$11f] := '@'; //KEY_2 0x1f // Keyboard 2 and @
map[$120] := '#'; //KEY_3 0x20 // Keyboard 3 and #
map[$121] := '$'; //KEY_4 0x21 // Keyboard 4 and $
map[$122] := '%'; //KEY_5 0x22 // Keyboard 5 and %
map[$123] := '^'; //KEY_6 0x23 // Keyboard 6 and ^
map[$124] := '&'; //KEY_7 0x24 // Keyboard 7 and &
map[$125] := '*'; //KEY_8 0x25 // Keyboard 8 and *
map[$126] := '('; //KEY_9 0x26 // Keyboard 9 and (
map[$127] := ')'; //KEY_0 0x27 // Keyboard 0 and )

map[$28] :=  char(VK_RETURN); //KEY_ENTER 0x28 // Keyboard Return (ENTER)
map[$29] :=  char(VK_ESCAPE); //KEY_ESC 0x29 // Keyboard ESCAPE
map[$2a] :=  char(VK_BACK  );   //KEY_BACKSPACE 0x2a // Keyboard DELETE (Backspace)
map[$2b] :=  char(VK_TAB   );    //KEY_TAB 0x2b // Keyboard Tab

map[$2c] :=  ' ';       //KEY_SPACE 0x2c // Keyboard Spacebar

map[$02d] :=  '-'; //KEY_MINUS 0x2d // Keyboard - and _
map[$02e] :=  '='; //KEY_EQUAL 0x2e // Keyboard = and +
map[$02f] :=  '['; //KEY_LEFTBRACE 0x2f // Keyboard [ and {
map[$030] :=  ']'; //KEY_RIGHTBRACE 0x30 // Keyboard ] and }
map[$031] :=  '\'; //KEY_BACKSLASH 0x31 // Keyboard \ and |
map[$032] :=  '''';//KEY_HASHTILDE 0x32 // Keyboard Non-US # and ~
map[$033] :=  ';'; //KEY_SEMICOLON 0x33 // Keyboard ; and :
map[$034] :=  ''''; //KEY_APOSTROPHE 0x34 // Keyboard ' and "
map[$035] :=  '~'; //KEY_GRAVE 0x35 // Keyboard ` and ~
map[$036] :=  ','; //KEY_COMMA 0x36 // Keyboard , and <
map[$037] :=  '.'; //KEY_DOT 0x37 // Keyboard . and >
map[$038] :=  '/'; //KEY_SLASH 0x38 // Keyboard / and ?
map[$12d] :=  '_'; //KEY_MINUS 0x2d // Keyboard - and _
map[$12e] :=  '+'; //KEY_EQUAL 0x2e // Keyboard = and +
map[$12f] :=  '{'; //KEY_LEFTBRACE 0x2f // Keyboard [ and {
map[$130] :=  '}'; //KEY_RIGHTBRACE 0x30 // Keyboard ] and }
map[$131] :=  '|'; //KEY_BACKSLASH 0x31 // Keyboard \ and |
map[$132] :=  '`';//KEY_HASHTILDE 0x32 // Keyboard Non-US # and ~
map[$133] :=  ':'; //KEY_SEMICOLON 0x33 // Keyboard ; and :
map[$134] :=  '"'; //KEY_APOSTROPHE 0x34 // Keyboard ' and "
map[$135] :=  '^'; //KEY_GRAVE 0x35 // Keyboard ` and ~
map[$136] :=  '<'; //KEY_COMMA 0x36 // Keyboard , and <
map[$137] :=  '>'; //KEY_DOT 0x37 // Keyboard . and >
map[$138] :=  '?'; //KEY_SLASH 0x38 // Keyboard / and ?

map[$039] := Char(VK_CAPITAL); //KEY_CAPSLOCK 0x39 // Keyboard Caps Lock

map[$03a] := Char(VK_F1 ); //KEY_F1 0x3a // Keyboard F1
map[$03b] := Char(VK_F2 ); //KEY_F2 0x3b // Keyboard F2
map[$03c] := Char(VK_F3 ); //KEY_F3 0x3c // Keyboard F3
map[$03d] := Char(VK_F4 ); //KEY_F4 0x3d // Keyboard F4
map[$03e] := Char(VK_F5 ); //KEY_F5 0x3e // Keyboard F5
map[$03f] := Char(VK_F6 ); //KEY_F6 0x3f // Keyboard F6
map[$040] := Char(VK_F7 ); //KEY_F7 0x40 // Keyboard F7
map[$041] := Char(VK_F8 ); //KEY_F8 0x41 // Keyboard F8
map[$042] := Char(VK_F9 ); //KEY_F9 0x42 // Keyboard F9
map[$043] := Char(VK_F10); //KEY_F10 0x43 // Keyboard F10
map[$044] := Char(VK_F11); //KEY_F11 0x44 // Keyboard F11
map[$045] := Char(VK_F12); //KEY_F12 0x45 // Keyboard F12

map[$046] := Char(VK_PRINT);  //KEY_SYSRQ 0x46 // Keyboard Print Screen
map[$047] := Char(VK_SCROLL); //KEY_SCROLLLOCK 0x47 // Keyboard Scroll Lock
map[$048] := Char(VK_PAUSE);  //KEY_PAUSE 0x48 // Keyboard Pause
map[$049] := Char(VK_INSERT); //KEY_INSERT 0x49 // Keyboard Insert
map[$04a] := Char(VK_HOME);   //KEY_HOME 0x4a // Keyboard Home
map[$04b] := Char(VK_PRIOR);  //KEY_PAGEUP 0x4b // Keyboard Page Up
map[$04c] := Char(VK_DELETE); //KEY_DELETE 0x4c // Keyboard Delete Forward
map[$04d] := Char(VK_END);    //KEY_END 0x4d // Keyboard End
map[$04e] := Char(VK_NEXT);   //KEY_PAGEDOWN 0x4e // Keyboard Page Down
map[$04f] := Char(VK_RIGHT);  //KEY_RIGHT 0x4f // Keyboard Right Arrow
map[$050] := Char(VK_LEFT);   //KEY_LEFT 0x50 // Keyboard Left Arrow
map[$051] := Char(VK_DOWN);   //KEY_DOWN 0x51 // Keyboard Down Arrow
map[$052] := Char(VK_UP);     //KEY_UP 0x52 // Keyboard Up Arrow

map[$053] := Char(VK_NUMLOCK); //KEY_NUMLOCK 0x53 // Keyboard Num Lock and Clear
map[$054] := Char(VK_DIVIDE); //KEY_KPSLASH 0x54 // Keypad /
map[$055] := Char(VK_MULTIPLY); //KEY_KPASTERISK 0x55 // Keypad *
map[$056] := Char(VK_SUBTRACT); //KEY_KPMINUS 0x56 // Keypad -
map[$057] := Char(VK_ADD); //KEY_KPPLUS 0x57 // Keypad +
map[$058] := Char(VK_RETURN); //KEY_KPENTER 0x58 // Keypad ENTER

map[$059] := '1'; //KEY_KP1 0x59 // Keypad 1 and End
map[$05a] := '2'; //KEY_KP2 0x5a // Keypad 2 and Down Arrow
map[$05b] := '3'; //KEY_KP3 0x5b // Keypad 3 and PageDn
map[$05c] := '4'; //KEY_KP4 0x5c // Keypad 4 and Left Arrow
map[$05d] := '5'; //KEY_KP5 0x5d // Keypad 5
map[$05e] := '6'; //KEY_KP6 0x5e // Keypad 6 and Right Arrow
map[$05f] := '7'; //KEY_KP7 0x5f // Keypad 7 and Home
map[$060] := '8'; //KEY_KP8 0x60 // Keypad 8 and Up Arrow
map[$061] := '9'; //KEY_KP9 0x61 // Keypad 9 and Page Up
map[$062] := '0'; //KEY_KP0 0x62 // Keypad 0 and Insert
map[$063] := char(VK_DECIMAL); //KEY_KPDOT 0x63 // Keypad . and Delete

map[$159] := char(VK_END); //KEY_KP1 0x59 // Keypad 1 and End
map[$15a] := char(VK_DOWN); //KEY_KP2 0x5a // Keypad 2 and Down Arrow
map[$15b] := char(VK_NEXT); //KEY_KP3 0x5b // Keypad 3 and PageDn
map[$15c] := char(VK_LEFT); //KEY_KP4 0x5c // Keypad 4 and Left Arrow
map[$15e] := char(VK_RIGHT); //KEY_KP6 0x5e // Keypad 6 and Right Arrow
map[$15f] := char(VK_HOME); //KEY_KP7 0x5f // Keypad 7 and Home
map[$160] := char(VK_UP); //KEY_KP8 0x60 // Keypad 8 and Up Arrow
map[$161] := char(VK_PRIOR); //KEY_KP9 0x61 // Keypad 9 and Page Up
map[$162] := char(VK_INSERT); //KEY_KP0 0x62 // Keypad 0 and Insert
map[$163] := char(VK_DELETE); //KEY_KPDOT 0x63 // Keypad . and Delete

//map[$64] =  KEY_102ND 0x64 // Keyboard Non-US \ and |
//map[$65] =  KEY_COMPOSE 0x65 // Keyboard Application
//map[$66] =  KEY_POWER 0x66 // Keyboard Power
//map[$67] =  KEY_KPEQUAL 0x67 // Keypad =
//
//map[$68] =  KEY_F13 0x68 // Keyboard F13
//map[$69] =  KEY_F14 0x69 // Keyboard F14
//map[$6a] =  KEY_F15 0x6a // Keyboard F15
//map[$6b] =  KEY_F16 0x6b // Keyboard F16
//map[$6c] =  KEY_F17 0x6c // Keyboard F17
//map[$6d] =  KEY_F18 0x6d // Keyboard F18
//map[$6e] =  KEY_F19 0x6e // Keyboard F19
//map[$6f] =  KEY_F20 0x6f // Keyboard F20
//map[$70] =  KEY_F21 0x70 // Keyboard F21
//map[$71] =  KEY_F22 0x71 // Keyboard F22
//map[$72] =  KEY_F23 0x72 // Keyboard F23
//map[$73] =  KEY_F24 0x73 // Keyboard F24
//
//map[$74] =  KEY_OPEN 0x74 // Keyboard Execute
//map[$75] =  KEY_HELP 0x75 // Keyboard Help
//map[$76] =  KEY_PROPS 0x76 // Keyboard Menu
//map[$77] =  KEY_FRONT 0x77 // Keyboard Select
//map[$78] =  KEY_STOP 0x78 // Keyboard Stop
//map[$79] =  KEY_AGAIN 0x79 // Keyboard Again
//map[$7a] =  KEY_UNDO 0x7a // Keyboard Undo
//map[$7b] =  KEY_CUT 0x7b // Keyboard Cut
//map[$7c] =  KEY_COPY 0x7c // Keyboard Copy
//map[$7d] =  KEY_PASTE 0x7d // Keyboard Paste
//map[$7e] =  KEY_FIND 0x7e // Keyboard Find
//map[$7f] =  KEY_MUTE 0x7f // Keyboard Mute
//map[$80] =  KEY_VOLUMEUP 0x80 // Keyboard Volume Up
//map[$81] =  KEY_VOLUMEDOWN 0x81 // Keyboard Volume Down
//map[$85] =  KEY_KPCOMMA 0x85 // Keypad Comma
//map[$87] =  KEY_RO 0x87 // Keyboard International1
//map[$88] =  KEY_KATAKANAHIRAGANA 0x88 // Keyboard International2
//map[$89] =  KEY_YEN 0x89 // Keyboard International3
//map[$8a] =  KEY_HENKAN 0x8a // Keyboard International4
//map[$8b] =  KEY_MUHENKAN 0x8b // Keyboard International5
//map[$8c] =  KEY_KPJPCOMMA 0x8c // Keyboard International6
//map[$90] =  KEY_HANGEUL 0x90 // Keyboard LANG1
//map[$91] =  KEY_HANJA 0x91 // Keyboard LANG2
//map[$92] =  KEY_KATAKANA 0x92 // Keyboard LANG3
//map[$93] =  KEY_HIRAGANA 0x93 // Keyboard LANG4
//map[$94] =  KEY_ZENKAKUHANKAKU 0x94 // Keyboard LANG5
//
//map[$b6] =  KEY_KPLEFTPAREN 0xb6 // Keypad (
//map[$b7] =  KEY_KPRIGHTPAREN 0xb7 // Keypad )
//
//map[$e0] =  KEY_LEFTCTRL 0xe0 // Keyboard Left Control
//map[$e1] =  KEY_LEFTSHIFT 0xe1 // Keyboard Left Shift
//map[$e2] =  KEY_LEFTALT 0xe2 // Keyboard Left Alt
//map[$e3] =  KEY_LEFTMETA 0xe3 // Keyboard Left GUI
//map[$e4] =  KEY_RIGHTCTRL 0xe4 // Keyboard Right Control
//map[$e5] =  KEY_RIGHTSHIFT 0xe5 // Keyboard Right Shift
//map[$e6] =  KEY_RIGHTALT 0xe6 // Keyboard Right Alt
//map[$e7] =  KEY_RIGHTMETA 0xe7 // Keyboard Right GUI
//
//map[$e8] =  KEY_MEDIA_PLAYPAUSE 0xe8
//map[$e9] =  KEY_MEDIA_STOPCD 0xe9
//map[$ea] =  KEY_MEDIA_PREVIOUSSONG 0xea
//map[$eb] =  KEY_MEDIA_NEXTSONG 0xeb
//map[$ec] =  KEY_MEDIA_EJECTCD 0xec
//map[$ed] =  KEY_MEDIA_VOLUMEUP 0xed
//map[$ee] =  KEY_MEDIA_VOLUMEDOWN 0xee
//map[$ef] =  KEY_MEDIA_MUTE 0xef
//map[$f0] =  KEY_MEDIA_WWW 0xf0
//map[$f1] =  KEY_MEDIA_BACK 0xf1
//map[$f2] =  KEY_MEDIA_FORWARD 0xf2
//map[$f3] =  KEY_MEDIA_STOP 0xf3
//map[$f4] =  KEY_MEDIA_FIND 0xf4
//map[$f5] =  KEY_MEDIA_SCROLLUP 0xf5
//map[$f6] =  KEY_MEDIA_SCROLLDOWN 0xf6
//map[$f7] =  KEY_MEDIA_EDIT 0xf7
//map[$f8] =  KEY_MEDIA_SLEEP 0xf8
//map[$f9] =  KEY_MEDIA_COFFEE 0xf9
//map[$fa] =  KEY_MEDIA_REFRESH 0xfa
//map[$fb] =  KEY_MEDIA_CALC 0xfb

{$IFDEF WINDOWS}
finalization
  FreeAndNil(SycDispatcher);
  if SycWndClassRegistered then
    UnregisterClassW(PWideChar(SycWndClassName), HInstance);
{$ENDIF}
end.
