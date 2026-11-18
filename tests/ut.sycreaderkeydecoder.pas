{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da parte independente de plataforma do leitor RFID
            Sycreader: o que transforma as teclas digitadas pelo leitor no
            codigo do cartao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O leitor e' um emulador de teclado: digita o codigo do cartao e um Enter. No
  Linux chegam codigos de uso HID (relatorios do teclado, via libhidapi); no
  Windows chegam scan codes (Raw Input), convertidos para o mesmo codigo de uso
  HID. A partir dai' a decodificacao e' uma so', e e' ela que se testa aqui,
  junto com o que o backend do Windows usa para decidir quais teclas engolir.
}
{$ELSE}
{:
  @abstract(Tests of the platform independent part of the Sycreader RFID
            reader: what turns the keys typed by the reader into the code of
            the card.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The reader is a keyboard emulator: it types the code of the card and an
  Enter. On Linux HID usage codes arrive (keyboard reports, through libhidapi);
  on Windows scan codes arrive (Raw Input), converted to the same HID usage.
  From there on the decoding is a single one, and that is what is tested here,
  along with what the Windows backend uses to decide which keys to swallow.
}
{$ENDIF}
unit ut.sycreaderkeydecoder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, sycreader_keydecoder;

type

  { TTestSycKeyDecoder }

  TTestSycKeyDecoder = class(TTestCase)
  private
    FDec:TSycKeyDecoder;
    function FeedUsages(const aUsages:array of Byte; out aCode:String):Boolean;
    function FeedScans(const aScans:array of Word; out aCode:String):Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //usos HID (Linux) / HID usages (Linux)
    procedure TheCodeEndsOnEnter;
    procedure CharactersAloneDeliverNothing;
    procedure NoKeyAndRollOverAreIgnored;
    procedure AnEnterWithNoCodeDeliversNothing;
    procedure AnyUsageOutOfTheCodeEndsIt;
    procedure KeypadDigitsAreDigits;
    procedure EveryCodeUsageHasACharacter;
    procedure TheNextCodeStartsEmpty;
    procedure ResetDropsWhatWasTyped;
    //scan codes (Windows)
    procedure ScanCodesGiveTheSameCode;
    procedure ReleasesAreIgnored;
    procedure ModifiersDoNotEndTheCode;
    procedure E1KeysAreIgnored;
    procedure KeypadEnterEndsTheCode;
  end;

  { TTestSycScanCodeTable }

  TTestSycScanCodeTable = class(TTestCase)
  private
    procedure CheckRow(aFirstScan:Word; const aChars:String);
  published
    procedure DigitRowIsUS;
    procedure LetterRowsAreUS;
    procedure PunctuationIsUS;
    procedure KeypadDigitsWithoutE0;
    procedure E0KeypadKeysAreNavigation;
    procedure ModifiersAndLocksAreIgnored;
    procedure OutOfTableIsIgnored;
    procedure NoTwoScanCodesShareAUsage;
  end;

  { TTestSycDeviceName }

  TTestSycDeviceName = class(TTestCase)
  published
    procedure KeyboardPath;
    procedure LowerCasePath;
    procedure CompositeDevicePath;
    procedure PathWithoutVIDPID;
    procedure TruncatedVID;
    procedure NonHexPID;
  end;

  { TTestSycHookLParam }

  TTestSycHookLParam = class(TTestCase)
  published
    procedure KeyPress;
    procedure KeyRelease;
    procedure ExtendedKey;
    {$IFDEF CPU64}
    procedure SignExtendedLParam;
    {$ENDIF}
  end;

  { TTestSycSwallowQueue }

  TTestSycSwallowQueue = class(TTestCase)
  private
    FQ:TSycSwallowQueue;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TakeNeedsScanE0AndDirection;
    procedure EachEntryIsTakenOnce;
    procedure RepeatedKeysAreQueuedApart;
    procedure TakeSkipsEntriesThatDoNotMatch;
    procedure EntriesExpireAfterTheTimeout;
    procedure AddForgetsExpiredEntries;
    procedure ClockGoingBackForgetsEntries;
    procedure ClearEmptiesTheQueue;
  end;

implementation

const
  //usos HID / HID usages
  U_A = $04; U_B = $05; U_Z = $1D; U_1 = $1E; U_2 = $1F; U_0 = $27;
  U_ENTER = $28; U_ESC = $29; U_TAB = $2B; U_SPACE = $2C; U_MINUS = $2D;
  U_SLASH = $38; U_CAPS = $39; U_KPENTER = $58; U_KP1 = $59; U_KP0 = $62;
  U_RIGHT = $4F;

  //scan codes (set 1)
  S_A = $1E; S_B = $30; S_1 = $02; S_2 = $03; S_ENTER = $1C; S_LSHIFT = $2A;

{ TTestSycKeyDecoder }

function TTestSycKeyDecoder.FeedUsages(const aUsages: array of Byte; out
  aCode: String): Boolean;
var
  i:Integer;
  c:String;
begin
  Result:=false;
  aCode:='';
  for i:=0 to High(aUsages) do
    if FDec.FeedHIDUsage(aUsages[i], c) then begin
      AssertFalse('only one code expected', Result);
      Result:=true;
      aCode:=c;
    end;
end;

function TTestSycKeyDecoder.FeedScans(const aScans: array of Word; out
  aCode: String): Boolean;
var
  i:Integer;
  c:String;
begin
  //cada tecla e' pressionada e solta / each key is pressed and released
  Result:=false;
  aCode:='';
  for i:=0 to High(aScans) do begin
    if FDec.FeedScanCode(aScans[i], false, false, false, c) then begin
      AssertFalse('only one code expected', Result);
      Result:=true;
      aCode:=c;
    end;
    AssertFalse('a release never ends a code',
                FDec.FeedScanCode(aScans[i], false, true, false, c));
  end;
end;

procedure TTestSycKeyDecoder.SetUp;
begin
  FDec:=TSycKeyDecoder.Create;
end;

procedure TTestSycKeyDecoder.TearDown;
begin
  FreeAndNil(FDec);
end;

procedure TTestSycKeyDecoder.TheCodeEndsOnEnter;
var
  code:String;
begin
  AssertTrue(FeedUsages([U_A, U_1, U_B, U_2, U_Z, U_0, U_ENTER], code));
  AssertEquals('a1b2z0', code);
end;

procedure TTestSycKeyDecoder.CharactersAloneDeliverNothing;
var
  code:String;
begin
  AssertFalse(FeedUsages([U_1, U_2, U_A], code));
  AssertEquals('', code);
  AssertEquals('12a', FDec.Pending);
end;

procedure TTestSycKeyDecoder.NoKeyAndRollOverAreIgnored;
var
  code:String;
begin
  //o relatorio de "tecla solta" tem uso 0 / the "key released" report has usage 0
  AssertTrue(FeedUsages([U_1, 0, U_1, 1, 0, U_2, 0, U_ENTER, 0], code));
  AssertEquals('112', code);
end;

procedure TTestSycKeyDecoder.AnEnterWithNoCodeDeliversNothing;
var
  code:String;
begin
  AssertFalse(FeedUsages([0, U_ENTER, 0, U_ENTER], code));
  AssertFalse(FDec.FeedHIDUsage(U_ENTER, code));
  AssertEquals('', code);
end;

procedure TTestSycKeyDecoder.AnyUsageOutOfTheCodeEndsIt;
var
  code:String;
  u:Byte;
begin
  for u in [U_ENTER, U_ESC, U_TAB, U_CAPS, U_KPENTER, U_RIGHT, $E1] do begin
    AssertTrue('usage $'+IntToHex(u, 2), FeedUsages([U_A, U_1, u], code));
    AssertEquals('usage $'+IntToHex(u, 2), 'a1', code);
  end;
end;

procedure TTestSycKeyDecoder.KeypadDigitsAreDigits;
var
  code:String;
  u:Byte;
begin
  for u:=U_KP1 to U_KP0 do
    FDec.FeedHIDUsage(u, code);
  AssertTrue(FDec.FeedHIDUsage(U_ENTER, code));
  AssertEquals('1234567890', code);
end;

procedure TTestSycKeyDecoder.EveryCodeUsageHasACharacter;
var
  u:Byte;
  code:String;
begin
  for u:=0 to 255 do
    if u in SycCodeUsages then begin
      AssertTrue('usage $'+IntToHex(u, 2), map[u]>=' ');
      FDec.Reset;
      AssertFalse(FDec.FeedHIDUsage(u, code));
      AssertEquals('usage $'+IntToHex(u, 2), map[u], FDec.Pending);
    end;
  AssertEquals(' ', map[U_SPACE]);
  AssertEquals('-', map[U_MINUS]);
  AssertEquals('/', map[U_SLASH]);
end;

procedure TTestSycKeyDecoder.TheNextCodeStartsEmpty;
var
  code:String;
begin
  AssertTrue(FeedUsages([U_1, U_2, U_ENTER], code));
  AssertEquals('12', code);
  AssertEquals('', FDec.Pending);
  AssertTrue(FeedUsages([U_A, U_ENTER], code));
  AssertEquals('a', code);
end;

procedure TTestSycKeyDecoder.ResetDropsWhatWasTyped;
var
  code:String;
begin
  FeedUsages([U_1, U_2], code);
  FDec.Reset;
  AssertEquals('', FDec.Pending);
  AssertTrue(FeedUsages([U_A, U_ENTER], code));
  AssertEquals('a', code);
end;

procedure TTestSycKeyDecoder.ScanCodesGiveTheSameCode;
var
  code:String;
begin
  AssertTrue(FeedScans([S_A, S_1, S_B, S_2, S_ENTER], code));
  AssertEquals('a1b2', code);
end;

procedure TTestSycKeyDecoder.ReleasesAreIgnored;
var
  code:String;
begin
  //so' solturas: nada digitado / releases only: nothing typed
  AssertFalse(FDec.FeedScanCode(S_A, false, true, false, code));
  AssertFalse(FDec.FeedScanCode(S_ENTER, false, true, false, code));
  AssertEquals('', FDec.Pending);
end;

procedure TTestSycKeyDecoder.ModifiersDoNotEndTheCode;
var
  code:String;
begin
  //no Linux o shift vem no byte de modificadores, nunca no byte 2
  //on Linux shift comes in the modifier byte, never in byte 2
  AssertTrue(FeedScans([S_A, S_LSHIFT, S_B, $1D, $38, S_1, S_ENTER], code));
  AssertEquals('ab1', code);
end;

procedure TTestSycKeyDecoder.E1KeysAreIgnored;
var
  code:String;
begin
  FeedScans([S_A], code);
  //Pause/Break: E1 1D 45
  AssertFalse(FDec.FeedScanCode($1D, false, false, true, code));
  AssertFalse(FDec.FeedScanCode($45, false, false, true, code));
  AssertEquals('a', FDec.Pending);
end;

procedure TTestSycKeyDecoder.KeypadEnterEndsTheCode;
var
  code:String;
begin
  FeedScans([S_1, S_2], code);
  AssertTrue(FDec.FeedScanCode(S_ENTER, true, false, false, code));
  AssertEquals('12', code);
end;

{ TTestSycScanCodeTable }

procedure TTestSycScanCodeTable.CheckRow(aFirstScan: Word; const aChars: String);
var
  i:Integer;
  u:Byte;
begin
  for i:=1 to Length(aChars) do begin
    u:=SycScanCodeToHIDUsage(aFirstScan+i-1, false);
    AssertTrue('scan $'+IntToHex(aFirstScan+i-1, 2)+' is a code key', u in SycCodeUsages);
    AssertEquals('scan $'+IntToHex(aFirstScan+i-1, 2), aChars[i], map[u]);
  end;
end;

procedure TTestSycScanCodeTable.DigitRowIsUS;
begin
  CheckRow($02, '1234567890-=');
end;

procedure TTestSycScanCodeTable.LetterRowsAreUS;
begin
  CheckRow($10, 'qwertyuiop[]');
  CheckRow($1E, 'asdfghjkl;''');
  CheckRow($2C, 'zxcvbnm,./');
end;

procedure TTestSycScanCodeTable.PunctuationIsUS;
begin
  CheckRow($2B, '\');
  CheckRow($39, ' ');
  //o mapa de usos do Linux poe "~" no acento grave (0x35)
  //the Linux usage map puts "~" on the grave accent (0x35)
  CheckRow($29, '~');
end;

procedure TTestSycScanCodeTable.KeypadDigitsWithoutE0;
begin
  CheckRow($47, '789');
  CheckRow($4B, '456');
  CheckRow($4F, '1230');
end;

procedure TTestSycScanCodeTable.E0KeypadKeysAreNavigation;
var
  s:Word;
  u:Byte;
begin
  for s in [$47,$48,$49,$4B,$4D,$4F,$50,$51,$52,$53] do begin
    u:=SycScanCodeToHIDUsage(s, true);
    AssertTrue('E0 scan $'+IntToHex(s, 2)+' has an usage', u<>0);
    AssertFalse('E0 scan $'+IntToHex(s, 2)+' is not a code key', u in SycCodeUsages);
  end;
  AssertEquals('keypad enter', $58, SycScanCodeToHIDUsage($1C, true));
  AssertEquals('keypad /', $54, SycScanCodeToHIDUsage($35, true));
  //E0 1D/38 sao o Ctrl/Alt da direita / E0 1D/38 are the right Ctrl/Alt
  AssertEquals(0, SycScanCodeToHIDUsage($1D, true));
  AssertEquals(0, SycScanCodeToHIDUsage($38, true));
  //"fake shift" de algumas teclas E0 / "fake shift" of some E0 keys
  AssertEquals(0, SycScanCodeToHIDUsage($2A, true));
end;

procedure TTestSycScanCodeTable.ModifiersAndLocksAreIgnored;
var
  s:Word;
begin
  //ctrl, shifts, alt, caps, num, scroll
  for s in [$1D,$2A,$36,$38,$3A,$45,$46] do
    AssertEquals('scan $'+IntToHex(s, 2), 0, SycScanCodeToHIDUsage(s, false));
end;

procedure TTestSycScanCodeTable.OutOfTableIsIgnored;
begin
  AssertEquals(0, SycScanCodeToHIDUsage($00, false));
  AssertEquals(0, SycScanCodeToHIDUsage($59, false));
  AssertEquals(0, SycScanCodeToHIDUsage($FF, false)); //overrun
  AssertEquals(0, SycScanCodeToHIDUsage($100, false));
  AssertEquals(0, SycScanCodeToHIDUsage($02, true));
end;

procedure TTestSycScanCodeTable.NoTwoScanCodesShareAUsage;
var
  seen:array[Byte] of Integer;
  s:Integer;
  e0:Boolean;
  u:Byte;
begin
  for u:=Low(seen) to High(seen) do
    seen[u]:=-1;
  for e0:=false to true do
    for s:=0 to $FF do begin
      u:=SycScanCodeToHIDUsage(s, e0);
      if u=0 then continue;
      AssertEquals('usage $'+IntToHex(u, 2)+' of scan $'+IntToHex(s, 2), -1, seen[u]);
      seen[u]:=s+Ord(e0)*$100;
    end;
end;

{ TTestSycDeviceName }

procedure TTestSycDeviceName.KeyboardPath;
var
  v, p:Word;
begin
  AssertTrue(SycParseHIDDeviceName('\\?\HID#VID_FFFF&PID_0035#7&2a1c3f&0&0000#{884b96c3-56ef-11d1-bc8c-00a0c91405dd}', v, p));
  AssertEquals($FFFF, v);
  AssertEquals($0035, p);
end;

procedure TTestSycDeviceName.LowerCasePath;
var
  v, p:Word;
begin
  AssertTrue(SycParseHIDDeviceName('\\?\hid#vid_04d9&pid_1603#7&1&0&0000#{884b96c3-56ef-11d1-bc8c-00a0c91405dd}', v, p));
  AssertEquals($04D9, v);
  AssertEquals($1603, p);
end;

procedure TTestSycDeviceName.CompositeDevicePath;
var
  v, p:Word;
begin
  AssertTrue(SycParseHIDDeviceName('\\?\HID#VID_046D&PID_C52B&MI_00#8&5a0f&0&0000#{884b96c3-56ef-11d1-bc8c-00a0c91405dd}', v, p));
  AssertEquals($046D, v);
  AssertEquals($C52B, p);
end;

procedure TTestSycDeviceName.PathWithoutVIDPID;
var
  v, p:Word;
begin
  //teclado PS/2 / PS/2 keyboard
  AssertFalse(SycParseHIDDeviceName('\\?\ACPI#PNP0303#4&1d401fb5&0#{884b96c3-56ef-11d1-bc8c-00a0c91405dd}', v, p));
  AssertEquals(0, v);
  AssertEquals(0, p);
  AssertFalse(SycParseHIDDeviceName('', v, p));
end;

procedure TTestSycDeviceName.TruncatedVID;
var
  v, p:Word;
begin
  AssertFalse(SycParseHIDDeviceName('\\?\HID#PID_0035&VID_FF', v, p));
  AssertEquals(0, v);
  AssertEquals(0, p);
end;

procedure TTestSycDeviceName.NonHexPID;
var
  v, p:Word;
begin
  AssertFalse(SycParseHIDDeviceName('\\?\HID#VID_FFFF&PID_00ZZ#7', v, p));
  AssertEquals(0, v);
  AssertEquals(0, p);
end;

{ TTestSycHookLParam }

procedure TTestSycHookLParam.KeyPress;
var
  s:Word;
  e0, up:Boolean;
begin
  //repeticao 1, scan 1E (A) / repeat count 1, scan 1E (A)
  SycDecodeKeyboardHookLParam($001E0001, s, e0, up);
  AssertEquals($1E, s);
  AssertFalse(e0);
  AssertFalse(up);
end;

procedure TTestSycHookLParam.KeyRelease;
var
  s:Word;
  e0, up:Boolean;
begin
  //bits 30 (estava pressionada) e 31 (soltando) / bits 30 (was down) and 31 (releasing)
  SycDecodeKeyboardHookLParam($C01E0001, s, e0, up);
  AssertEquals($1E, s);
  AssertFalse(e0);
  AssertTrue(up);
end;

procedure TTestSycHookLParam.ExtendedKey;
var
  s:Word;
  e0, up:Boolean;
begin
  //seta para a direita / right arrow
  SycDecodeKeyboardHookLParam($014D0001, s, e0, up);
  AssertEquals($4D, s);
  AssertTrue(e0);
  AssertFalse(up);
  //com o bit 29 (alt) / with bit 29 (alt)
  SycDecodeKeyboardHookLParam($E11C0001, s, e0, up);
  AssertEquals($1C, s);
  AssertTrue(e0);
  AssertTrue(up);
end;

{$IFDEF CPU64}
procedure TTestSycHookLParam.SignExtendedLParam;
var
  s:Word;
  e0, up:Boolean;
begin
  //LPARAM e' com sinal: no win64 a soltura pode chegar estendida
  //LPARAM is signed: on win64 a release may come sign-extended
  SycDecodeKeyboardHookLParam(PtrUInt($FFFFFFFFC01E0001), s, e0, up);
  AssertEquals($1E, s);
  AssertFalse(e0);
  AssertTrue(up);
end;
{$ENDIF}

{ TTestSycSwallowQueue }

procedure TTestSycSwallowQueue.SetUp;
begin
  FQ:=TSycSwallowQueue.Create(1000);
end;

procedure TTestSycSwallowQueue.TearDown;
begin
  FreeAndNil(FQ);
end;

procedure TTestSycSwallowQueue.TakeNeedsScanE0AndDirection;
begin
  FQ.Add($1E, false, false, 5000);
  AssertFalse('release', FQ.Take($1E, false, true, 5000));
  AssertFalse('extended', FQ.Take($1E, true, false, 5000));
  AssertFalse('other key', FQ.Take($30, false, false, 5000));
  AssertEquals(1, FQ.Count);
  AssertTrue(FQ.Take($1E, false, false, 5000));
  AssertEquals(0, FQ.Count);
end;

procedure TTestSycSwallowQueue.EachEntryIsTakenOnce;
begin
  FQ.Add($1E, false, false, 5000);
  AssertTrue(FQ.Take($1E, false, false, 5001));
  //a mesma tecla vinda do teclado de verdade passa
  //the same key coming from the real keyboard goes through
  AssertFalse(FQ.Take($1E, false, false, 5002));
end;

procedure TTestSycSwallowQueue.RepeatedKeysAreQueuedApart;
begin
  //codigo "11": duas descidas e duas subidas / code "11": two presses and two releases
  FQ.Add($02, false, false, 5000);
  FQ.Add($02, false, true, 5001);
  FQ.Add($02, false, false, 5002);
  FQ.Add($02, false, true, 5003);
  AssertTrue(FQ.Take($02, false, false, 5010));
  AssertTrue(FQ.Take($02, false, true, 5010));
  AssertTrue(FQ.Take($02, false, false, 5010));
  AssertTrue(FQ.Take($02, false, true, 5010));
  AssertFalse(FQ.Take($02, false, false, 5010));
end;

procedure TTestSycSwallowQueue.TakeSkipsEntriesThatDoNotMatch;
begin
  FQ.Add($1E, false, false, 5000);
  FQ.Add($30, false, false, 5000);
  FQ.Add($1C, false, false, 5000);
  AssertTrue(FQ.Take($30, false, false, 5000));
  AssertEquals(2, FQ.Count);
  AssertTrue(FQ.Take($1E, false, false, 5000));
  AssertTrue(FQ.Take($1C, false, false, 5000));
end;

procedure TTestSycSwallowQueue.EntriesExpireAfterTheTimeout;
begin
  FQ.Add($1E, false, false, 5000);
  AssertTrue('at the timeout', FQ.Take($1E, false, false, 6000));
  FQ.Add($1E, false, false, 5000);
  AssertFalse('after the timeout', FQ.Take($1E, false, false, 6001));
  AssertEquals(0, FQ.Count);
end;

procedure TTestSycSwallowQueue.AddForgetsExpiredEntries;
begin
  //a aplicacao nao estava em primeiro plano: o hook nunca viu estas teclas
  //the application wasn't in foreground: the hook never saw these keys
  FQ.Add($1E, false, false, 5000);
  FQ.Add($1E, false, true, 5000);
  FQ.Add($30, false, false, 7000);
  AssertEquals(1, FQ.Count);
end;

procedure TTestSycSwallowQueue.ClockGoingBackForgetsEntries;
begin
  FQ.Add($1E, false, false, 5000);
  AssertFalse(FQ.Take($1E, false, false, 4000));
  AssertEquals(0, FQ.Count);
end;

procedure TTestSycSwallowQueue.ClearEmptiesTheQueue;
begin
  FQ.Add($1E, false, false, 5000);
  FQ.Add($30, false, false, 5000);
  FQ.Clear;
  AssertEquals(0, FQ.Count);
  AssertFalse(FQ.Take($1E, false, false, 5000));
end;

initialization
  RegisterTest('sycreader', TTestSycKeyDecoder);
  RegisterTest('sycreader', TTestSycScanCodeTable);
  RegisterTest('sycreader', TTestSycDeviceName);
  RegisterTest('sycreader', TTestSycHookLParam);
  RegisterTest('sycreader', TTestSycSwallowQueue);
end.
