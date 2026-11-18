{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TSycRFIDReader no Windows: o backend de Raw Input com o
            componente de verdade.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Sem o leitor fisico, o que da' para conferir e' o ciclo de vida (a janela de
  Raw Input e o hook sao compartilhados por todos os leitores do processo) e a
  identificacao do dispositivo. A leitura de ponta a ponta usa teclas
  injetadas com SendInput atribuidas ao teclado do sistema: o wine faz isso,
  o Windows de verdade entrega as injetadas sem dispositivo, e ai' o teste e'
  ignorado.
}
{$ELSE}
{:
  @abstract(Tests of TSycRFIDReader on Windows: the Raw Input backend with the
            real component.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Without the physical reader, what can be checked is the life cycle (the Raw
  Input window and the hook are shared by all readers of the process) and the
  identification of the device. The end to end read uses keys injected with
  SendInput attributed to the system keyboard: wine does that, a real Windows
  delivers injected keys with no device, and then the test is ignored.
}
{$ENDIF}
unit ut.sycrfidreaderwin;

{$mode objfpc}{$H+}

interface

{$IFDEF WINDOWS}
uses
  Classes, SysUtils, Forms, Windows, fpcunit, testregistry,
  sycreader_rfid_reader, sycreader_keydecoder;

type

  { TTestSycRFIDReaderWin }

  TTestSycRFIDReaderWin = class(TTestCase)
  private
    FReaders:array of TSycRFIDReader;
    function NewReader(aVID, aPID:Word; const aSN:String = ''):TSycRFIDReader;
    function FindHIDKeyboard(out aVID, aPID:Word):Boolean;
    procedure Pump(aMs:Integer);
    procedure SendScan(aScan:Word; aUp:Boolean);
  protected
    procedure TearDown; override;
  published
    procedure AbsentDeviceIsNotReady;
    procedure NothingToReadAtStart;
    procedure TwoReadersShareTheRawInput;
    procedure StartsAgainAfterTheLastReaderIsFreed;
    procedure ChangingVIDPIDUpdatesReady;
    procedure PresentKeyboardIsReady;
    procedure WrongSerialNumberIsNotReady;
    procedure InjectedCodeIsRead;
  end;
{$ENDIF}

implementation

{$IFDEF WINDOWS}

{ TTestSycRFIDReaderWin }

function TTestSycRFIDReaderWin.NewReader(aVID, aPID: Word; const aSN: String
  ): TSycRFIDReader;
begin
  Result:=TSycRFIDReader.Create(nil);
  Result.VendorID:=aVID;
  Result.ProductID:=aPID;
  Result.SerialNumber:=aSN;
  SetLength(FReaders, Length(FReaders)+1);
  FReaders[High(FReaders)]:=Result;
end;

function TTestSycRFIDReaderWin.FindHIDKeyboard(out aVID, aPID: Word): Boolean;
var
  list:array of RAWINPUTDEVICELIST;
  n, len:UINT;
  i:Integer;
  name:array[0..1023] of WideChar;
begin
  Result:=false;
  aVID:=0;
  aPID:=0;
  n:=0;
  list:=nil;
  if (GetRawInputDeviceList(nil, @n, SizeOf(RAWINPUTDEVICELIST))<>0) or (n=0) then
    exit;
  SetLength(list, n);
  if GetRawInputDeviceList(@list[0], @n, SizeOf(RAWINPUTDEVICELIST))=UINT(-1) then
    exit;
  for i:=0 to Integer(n)-1 do
    if list[i].dwType=RIM_TYPEKEYBOARD then begin
      len:=Length(name);
      FillChar(name, SizeOf(name), 0);
      if GetRawInputDeviceInfoW(list[i].hDevice, RIDI_DEVICENAME, @name, @len)=UINT(-1) then
        continue;
      if SycParseHIDDeviceName(String(UnicodeString(PWideChar(@name))), aVID, aPID) then
        exit(true);
    end;
end;

procedure TTestSycRFIDReaderWin.Pump(aMs: Integer);
var
  t:QWord;
begin
  t:=GetTickCount64;
  repeat
    Application.ProcessMessages;
    Sleep(5);
  until GetTickCount64-t>=QWord(aMs);
end;

procedure TTestSycRFIDReaderWin.SendScan(aScan: Word; aUp: Boolean);
var
  inp:TInput;
begin
  FillChar(inp, SizeOf(inp), 0);
  inp._type:=INPUT_KEYBOARD;
  inp.ki.wScan:=aScan;
  inp.ki.dwFlags:=KEYEVENTF_SCANCODE;
  if aUp then
    inp.ki.dwFlags:=inp.ki.dwFlags or KEYEVENTF_KEYUP;
  SendInput(1, @inp, SizeOf(inp));
end;

procedure TTestSycRFIDReaderWin.TearDown;
var
  i:Integer;
begin
  for i:=High(FReaders) downto 0 do
    FReaders[i].Free;
  SetLength(FReaders, 0);
end;

procedure TTestSycRFIDReaderWin.AbsentDeviceIsNotReady;
var
  r:TSycRFIDReader;
begin
  //nenhum dispositivo usa VID 0 / no device uses VID 0
  r:=NewReader($0000, $0035);
  AssertFalse(r.InitializeChipCard);
  AssertFalse(r.ChipCardReady);
end;

procedure TTestSycRFIDReaderWin.NothingToReadAtStart;
var
  r:TSycRFIDReader;
  code:UTF8String;
begin
  r:=NewReader($0000, $0035);
  r.InitializeChipCard;
  code:='x';
  AssertFalse(r.ChipCardRead(code));
  AssertTrue(r.IsEmptyChipCard);
  AssertTrue(r.FinishChipCard);
end;

procedure TTestSycRFIDReaderWin.TwoReadersShareTheRawInput;
var
  r1, r2:TSycRFIDReader;
  vid, pid:Word;
begin
  if not FindHIDKeyboard(vid, pid) then
    Ignore('no HID keyboard with VID/PID on this machine');
  r1:=NewReader(vid, pid);
  r2:=NewReader(vid, pid);
  AssertTrue('r1', r1.InitializeChipCard);
  AssertTrue('r2', r2.InitializeChipCard);
  //liberar um nao derruba o outro / freeing one doesn't bring the other down
  r1.Free;
  FReaders[0]:=nil;
  Pump(50);
  AssertTrue('r2 after r1 freed', r2.ChipCardReady);
end;

procedure TTestSycRFIDReaderWin.StartsAgainAfterTheLastReaderIsFreed;
var
  r:TSycRFIDReader;
  vid, pid:Word;
begin
  if not FindHIDKeyboard(vid, pid) then
    Ignore('no HID keyboard with VID/PID on this machine');
  r:=NewReader(vid, pid);
  AssertTrue(r.InitializeChipCard);
  r.Free;
  FReaders[0]:=nil;
  //o dispatcher foi liberado; um novo leitor cria outro
  //the dispatcher was freed; a new reader creates another one
  r:=NewReader(vid, pid);
  AssertTrue(r.InitializeChipCard);
end;

procedure TTestSycRFIDReaderWin.ChangingVIDPIDUpdatesReady;
var
  r:TSycRFIDReader;
  vid, pid:Word;
begin
  if not FindHIDKeyboard(vid, pid) then
    Ignore('no HID keyboard with VID/PID on this machine');
  r:=NewReader($0000, $0035);
  AssertFalse(r.InitializeChipCard);
  r.VendorID:=vid;
  r.ProductID:=pid;
  AssertTrue('now pointing to a present keyboard', r.ChipCardReady);
  r.ProductID:=pid xor $FFFF;
  AssertFalse('pointing away again', r.ChipCardReady);
end;

procedure TTestSycRFIDReaderWin.PresentKeyboardIsReady;
var
  vid, pid:Word;
begin
  if not FindHIDKeyboard(vid, pid) then
    Ignore('no HID keyboard with VID/PID on this machine');
  AssertTrue(NewReader(vid, pid).InitializeChipCard);
end;

procedure TTestSycRFIDReaderWin.WrongSerialNumberIsNotReady;
var
  vid, pid:Word;
begin
  if not FindHIDKeyboard(vid, pid) then
    Ignore('no HID keyboard with VID/PID on this machine');
  AssertFalse(NewReader(vid, pid, 'PascalSCADA-no-such-serial').InitializeChipCard);
end;

procedure TTestSycRFIDReaderWin.InjectedCodeIsRead;
const
  //termina com Esc, nao Enter: no Windows de verdade as injetadas vao para a
  //janela em foco (o console do runner), e um Enter executaria a linha.
  //ends with Esc, not Enter: on a real Windows the injected keys go to the
  //focused window (the runner's console), and an Enter would run the line.
  seq:array[0..6] of Word = ($1E,$02,$30,$03,$2C,$0B,$01); //a1b2z0 Esc
var
  r:TSycRFIDReader;
  vid, pid:Word;
  code:UTF8String;
  i:Integer;
  t:QWord;
begin
  if not FindHIDKeyboard(vid, pid) then
    Ignore('no HID keyboard with VID/PID on this machine');
  r:=NewReader(vid, pid);
  AssertTrue(r.InitializeChipCard);
  r.IsEmptyChipCard;
  for i:=0 to High(seq) do begin
    SendScan(seq[i], false);
    SendScan(seq[i], true);
  end;
  code:='';
  t:=GetTickCount64;
  repeat
    Pump(10);
    if r.ChipCardRead(code) then break;
  until GetTickCount64-t>1500;
  if code='' then
    //Windows de verdade: injetadas chegam sem dispositivo. wine: atribui ao
    //teclado dele, mas so' com o foco/estado de teclado limpo - na suite HMI
    //completa algum teste anterior o desvia.
    //real Windows: injected keys come with no device. wine: attributes them
    //to its keyboard, but only with a clean focus/keyboard state - in the full
    //HMI suite some earlier test steers it away.
    Ignore('injected keystrokes did not reach the reader (no device on a real Windows; under wine, run --suite=sycreader)');
  AssertEquals('a1b2z0', code);
end;

initialization
  RegisterTest('sycreader', TTestSycRFIDReaderWin);
{$ENDIF}
end.
