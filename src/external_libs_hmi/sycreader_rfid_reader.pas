unit sycreader_rfid_reader;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLType,
  ChipCardReader, crossthreads, {$IFNDEF WINDOWS}hidapi,{$ENDIF} syncobjs, cp1250,
  sycreader_keydecoder;

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
    FDecoder:TSycKeyDecoder;
    FCurBuffer:String;
    procedure ConfigChanged;
    procedure SetPID(AValue: Word);
    procedure SetSN(AValue: String);
    procedure SetVID(AValue: Word);
    procedure ProcessKey(aMakeCode, aFlags:Word);
  public
    constructor Create;
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
    FDecoder:TSycKeyDecoder;
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
  //kept for compatibility, the table lives in sycreader_keydecoder.
  map:array[0..511] of char absolute sycreader_keydecoder.map;

procedure Register;

implementation

uses hsstrings{$IFDEF WINDOWS}, Windows{$ENDIF};

procedure Register;
begin
  RegisterComponents(strUserManagement,[TSycRFIDReader]);
end;

{$IFDEF WINDOWS}
const
  SycHWND_MESSAGE = HWND(-3);

  //QS_ALLINPUT of FPC (255) lacks QS_RAWINPUT ($400), and the LCL waits for
  //messages with it (AppProcessMessages, AppWaitMessage): a WM_INPUT alone
  //never wakes the application, and stays queued until some other message
  //comes. The timer wakes the loop and drains the WM_INPUT of the window.
  SycPumpTimerID = 1;
  SycPumpInterval = 50;
  SycWndClassName:UnicodeString = 'PascalSCADA_SycRFIDRawInput';

function HidD_GetSerialNumberString(HidDeviceObject:THandle; Buffer:Pointer;
  BufferLength:ULONG):ByteBool; stdcall; external 'hid.dll';

type
  TSycRawDevInfo = record
    Handle:THandle;
    VID,
    PID:Word;
    Name,
    SN:UnicodeString;
    SNRead:Boolean;
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
    FSwallow:TSycSwallowQueue;
    function DevIndex(aDev:THandle):Integer;
    function DeviceMatches(aDev:THandle; aReader:TSycreaderRFID_RawInputReader):Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(aReader:TSycreaderRFID_RawInputReader);
    function Detach(aReader:TSycreaderRFID_RawInputReader):Boolean;
    procedure RefreshReady;
    procedure HandleRawInput(aRawInput:HRAWINPUT);
    procedure HandleDeviceChange(aChange:WPARAM; aDev:THandle);
    procedure DrainRawInput;
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
      WM_TIMER: begin
        SycDispatcher.DrainRawInput;
        exit(0);
      end;
    end;
  //WM_INPUT also needs DefWindowProc, for the cleanup of the raw input.
  Result:=DefWindowProcW(aWnd, aMsg, aWParam, aLParam);
end;

function SycKeyboardHookProc(aCode:longint; aWParam:WPARAM;
  aLParam:LPARAM):LRESULT; stdcall;
var
  scan:Word;
  e0, up:Boolean;
begin
  if (aCode=HC_ACTION) and Assigned(SycDispatcher) then begin
    SycDecodeKeyboardHookLParam(PtrUInt(aLParam), scan, e0, up);
    if SycDispatcher.HookKey(scan, e0, up) then
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
  FSwallow:=TSycSwallowQueue.Create;

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
    SetTimer(FWnd, SycPumpTimerID, SycPumpInterval, nil);

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
    KillTimer(FWnd, SycPumpTimerID);
    DestroyWindow(FWnd);
  end;
  FreeAndNil(FReaders);
  FreeAndNil(FSwallow);
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
      SycParseHIDDeviceName(String(FDevs[Result].Name), FDevs[Result].VID, FDevs[Result].PID);
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

  if claimed then
    FSwallow.Add(ri.data.keyboard.MakeCode,
                 (ri.data.keyboard.Flags and RI_KEY_E0)<>0,
                 (ri.data.keyboard.Flags and RI_KEY_BREAK)<>0,
                 GetTickCount64);
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

procedure TSycRawInputDispatcher.DrainRawInput;
var
  msg:TMsg;
begin
  if FWnd=0 then exit;
  while PeekMessageW(msg, FWnd, WM_INPUT, WM_INPUT, PM_REMOVE) do
    DispatchMessageW(msg);
end;

function TSycRawInputDispatcher.HookKey(aScan: Word; aE0, aUp: Boolean
  ): Boolean;
begin
  Result:=FSwallow.Take(aScan, aE0, aUp, GetTickCount64);
  if Result or (FWnd=0) then exit;

  //the WM_INPUT of this keystroke may still be queued behind it.
  DrainRawInput;
  Result:=FSwallow.Take(aScan, aE0, aUp, GetTickCount64);
end;

{ TSycreaderRFID_RawInputReader }

procedure TSycreaderRFID_RawInputReader.ConfigChanged;
begin
  FDecoder.Reset;
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
  code:AnsiString;
begin
  if FDecoder.FeedScanCode(aMakeCode, (aFlags and RI_KEY_E0)<>0,
                           (aFlags and RI_KEY_BREAK)<>0,
                           (aFlags and RI_KEY_E1)<>0, code) then begin
    FCurBuffer:=code;
    FHasCur:=true;
  end;
end;

constructor TSycreaderRFID_RawInputReader.Create;
begin
  inherited Create;
  FDecoder:=TSycKeyDecoder.Create;
end;

destructor TSycreaderRFID_RawInputReader.Destroy;
begin
  Stop;
  FreeAndNil(FDecoder);
  inherited Destroy;
end;

procedure TSycreaderRFID_RawInputReader.Start;
begin
  if FStarted then exit;
  if SycDispatcher=nil then
    SycDispatcher:=TSycRawInputDispatcher.Create;
  FStarted:=true;
  FDecoder.Reset;
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
  code: AnsiString;
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

    FDecoder.Reset;
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
    if FDecoder.FeedHIDUsage(buffer[2], code) then begin
      New(auxptr);
      auxptr^.StrBuffer:=code;
      auxptr := InterlockedExchange(FCurBuffer, auxptr);
      if Assigned(auxptr) then
        Dispose(auxptr);
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
  FDecoder:=TSycKeyDecoder.Create;
  FReady:=0;
end;

destructor TSycreaderRFID_USBHIDReader.Destroy;
var
  auxptr: PStrRec;
begin
  FreeAndNil(FSNMutex);
  FreeAndNil(FDecoder);
  auxptr:=InterlockedExchange(FCurBuffer, nil);
  if Assigned(auxptr) then
    Dispose(auxptr);
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

{$IFDEF WINDOWS}
finalization
  FreeAndNil(SycDispatcher);
  if SycWndClassRegistered then
    UnregisterClassW(PWideChar(SycWndClassName), HInstance);
{$ENDIF}
end.
