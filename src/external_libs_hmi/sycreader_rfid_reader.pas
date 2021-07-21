unit sycreader_rfid_reader;

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLType,
  ChipCardReader, crossthreads, hidapi, syncobjs, cp1250;

type
  TStrRec = record
    StrBuffer:String;
  end;
  PStrRec = ^TStrRec;

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

  { TSycRFIDReader }

  TSycRFIDReader = class(TChipCardReader)
  private
    FSycRFIDReader:TSycreaderRFID_USBHIDReader;
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

uses hsstrings;

procedure Register;
begin
  RegisterComponents(strUserManagement,[TSycRFIDReader]);
end;

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

  if ShouldReopen then begin
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
    FSycRFIDReader.WakeUp;
end;

function TSycRFIDReader.InitializeChipCard: Boolean;
begin
  if ([csDesigning]*ComponentState)=[] then begin
    FSycRFIDReader.WakeUp;
    FSycRFIDReader.LoopStarted(1000);
    Result:=FSycRFIDReader.GetReady;
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
  Result:=FSycRFIDReader.GetReady;
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
  FSycRFIDReader:=TSycreaderRFID_USBHIDReader.Create(true);
end;

destructor TSycRFIDReader.Destroy;
begin
  FSycRFIDReader.Terminate;

  repeat
    CheckSynchronize(10);
  until FSycRFIDReader.WaitEnd(10)=wrSignaled;

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

end.
