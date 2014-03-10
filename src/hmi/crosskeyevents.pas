{$i ../common/language.inc}
{$i ../common/delphiver.inc}
{$IFDEF PORTUGUES}
{:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    @abstract(Unit de teclado virtual multi plataforma.)
}
{$ELSE}
{:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    @abstract(Multi-platform virtual keyboard unit.)
}
{$ENDIF}
unit crosskeyevents;

interface

uses
  Controls, Classes, SysUtils;

type
  {$IFDEF PORTUGUES}
  {:
      @abstract(Classe te base de emulação de eventos de teclado.)
      @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
      @abstract(Base class of keyboard events emulation.)
      @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TCrossKeyEvents = class
  protected
    {$IFDEF PORTUGUES}
    //: Controle alvo dos eventos de teclado.
    {$ELSE}
    //: Target control of keyboard events.
    {$ENDIF}
    FTarget:TWinControl;

    FShitfState:TShiftState;

    {$IFDEF PORTUGUES}
    //: Emula o pressionamento de uma tecla.
    {$ELSE}
    //: Emulates the key press event.
    {$ENDIF}
    procedure DoDown(Key: LongWord); virtual; abstract;

    {$IFDEF PORTUGUES}
    //: Emula o liberar de uma tecla.
    {$ELSE}
    //: Emulates a key release event.
    {$ENDIF}
    procedure DoUp(Key: LongWord); virtual; abstract;

    {$IFDEF PORTUGUES}
    //: Traduz o codigo da tecla para o codigo da tecla no widgetset.
    {$ELSE}
    //: Translate the key code to the widgetset key code.
    {$ENDIF}
    function  TranlateVirtualKey(Key:Word):LongWord; virtual; abstract;
  public
    {$IFDEF PORTUGUES}
     //: Cria o emulador de eventos de tecla.
    {$ELSE}
    //: Creates the key event emulator.
    {$ENDIF}
    constructor Create(Target:TWinControl);

    {$IFDEF PORTUGUES}
    //: Configura o controle alvo dos eventos de tecla.
    {$ELSE}
    //: Sets the target control of key events.
    {$ENDIF}
    procedure SetTarget(Target:TWinControl);

    {$IFDEF PORTUGUES}
    //: Traduz e emula o pressionamento de uma tecla.
    {$ELSE}
    //: Translate and send the key press event.
    {$ENDIF}
    procedure Down(Key: Word);

    {$IFDEF PORTUGUES}
    //: Traduz e emula o liberar de uma tecla.
    {$ELSE}
    //: Translate and send a key release event.
    {$ENDIF}
    procedure Up(Key: Word);

    {$IFDEF PORTUGUES}
    //: Traduz e emula o presionar e o liberar de uma tecla.
    {$ELSE}
    //: Translate and send a key press and a key release events.
    {$ENDIF}
    procedure Press(Key: Word);

    {$IFDEF PORTUGUES}
    //: Aplica modificadores de evento de tecla (Ctrl, Shift e Alt).
    {$ELSE}
    //: Apply key modification events (Ctrl, Shift and Alt).
    {$ENDIF}
    procedure Apply(Shift: TShiftState); virtual;
    {$IFDEF PORTUGUES}
    //: Remove modificadores de evento de tecla (Ctrl, Shift e Alt).
    {$ELSE}
    //: Removes key modification events (Ctrl, Shift and Alt).
    {$ENDIF}
    procedure Unapply(Shift: TShiftState); virtual;
  end;

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

  {$IF defined(FPC) and defined(LCL)}
  TLazarusLCLKeyEvents = class(TCrossKeyEvents)
    protected
      //: @seealso(TCrossKeyEvents.DoDown)
      procedure DoDown(Key: LongWord); override;
      //: @seealso(TCrossKeyEvents.DoUp)
      procedure DoUp(Key: LongWord); override;
      //: @seealso(TCrossKeyEvents.TranlateVirtualKey)
      function TranlateVirtualKey(Key: Word): LongWord; override;
  end;
  {$IFEND}

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

  {$IF (not defined(FPC)) and defined(DELPHI2009_UP)}
  TDelphiVCLKeyEvents = class(TCrossKeyEvents)
    protected
      //: @seealso(TCrossKeyEvents.DoDown)
      procedure DoDown(Key: LongWord); override;
      //: @seealso(TCrossKeyEvents.DoUp)
      procedure DoUp(Key: LongWord); override;
      //: @seealso(TCrossKeyEvents.TranlateVirtualKey)
      function TranlateVirtualKey(Key: Word): LongWord; override;
  end;
  {$IFEND}

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////


  {$IFDEF PORTUGUES}
  //: Cria o emulador de eventos de tecla de acordo com o widgetset em uso.
  {$ELSE}
  //: Creates the key event emulator of the currently widgetset.
  {$ENDIF}
  function CreateCrossKeyEvents(Target:TWinControl):TCrossKeyEvents;

var
  PSVK_BACK, PSVK_DECIMAL, PSVK_SUBTRACT:Byte;

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

{$IF (not defined(FPC)) and (defined(WIN32) or defined(WIN64) or defined(WINDOWS))}
const
  //Constantes e teclas ausentes.
  //missing key constants
  VK_0 = 48;
  VK_1 = 49;
  VK_2 = 50;
  VK_3 = 51;
  VK_4 = 52;
  VK_5 = 53;
  VK_6 = 54;
  VK_7 = 55;
  VK_8 = 56;
  VK_9 = 57;

  //these constants are taken from FPC RTL.
  {$IFNDEF FPC}
  VK_LBUTTON = 1;
  VK_RBUTTON = 2;
  VK_CANCEL = 3;
  VK_MBUTTON = 4;
  VK_BACK = 8;
  VK_TAB = 9;
  VK_CLEAR = 12;
  VK_SHIFT = 16;
  VK_CONTROL = 17;
  VK_MENU = 18;
  VK_PAUSE = 19;
  VK_CAPITAL = 20;
  VK_SPACE = 32;
  VK_PRIOR = 33;
  VK_NEXT = 34;
  VK_END = 35;
  VK_HOME = 36;
  VK_SELECT = 41;
  VK_PRINT = 42;
  VK_EXECUTE = 43;
  VK_SNAPSHOT = 44;
  VK_INSERT = 45;
  VK_HELP = 47;
  VK_A = 65;
  VK_B = 66;
  VK_C = 67;
  VK_D = 68;
  VK_E = 69;
  VK_F = 70;
  VK_G = 71;
  VK_H = 72;
  VK_I = 73;
  VK_J = 74;
  VK_K = 75;
  VK_L = 76;
  VK_M = 77;
  VK_N = 78;
  VK_O = 79;
  VK_P = 80;
  VK_Q = 81;
  VK_R = 82;
  VK_S = 83;
  VK_T = 84;
  VK_U = 85;
  VK_V = 86;
  VK_W = 87;
  VK_X = 88;
  VK_Y = 89;
  VK_Z = 90;
  VK_LWIN = 91;
  VK_RWIN = 92;
  VK_APPS = 93;
  VK_NUMPAD0 = 96;
  VK_NUMPAD1 = 97;
  VK_NUMPAD2 = 98;
  VK_NUMPAD3 = 99;
  VK_NUMPAD4 = 100;
  VK_NUMPAD5 = 101;
  VK_NUMPAD6 = 102;
  VK_NUMPAD7 = 103;
  VK_NUMPAD8 = 104;
  VK_NUMPAD9 = 105;
  VK_MULTIPLY = 106;
  VK_ADD = 107;
  VK_SEPARATOR = 108;
  VK_SUBTRACT = 109;
  VK_DECIMAL = 110;
  VK_DIVIDE = 111;
  VK_F1 = 112;
  VK_F2 = 113;
  VK_F3 = 114;
  VK_F4 = 115;
  VK_F5 = 116;
  VK_F6 = 117;
  VK_F7 = 118;
  VK_F8 = 119;
  VK_F9 = 120;
  VK_F10 = 121;
  VK_F11 = 122;
  VK_F12 = 123;
  VK_F13 = 124;
  VK_F14 = 125;
  VK_F15 = 126;
  VK_F16 = 127;
  VK_F17 = 128;
  VK_F18 = 129;
  VK_F19 = 130;
  VK_F20 = 131;
  VK_F21 = 132;
  VK_F22 = 133;
  VK_F23 = 134;
  VK_F24 = 135;
{ GetAsyncKeyState  }
  VK_NUMLOCK = 144;
  VK_SCROLL = 145;
  VK_LSHIFT = 160;
  VK_LCONTROL = 162;
  VK_LMENU = 164;
  VK_RSHIFT = 161;
  VK_RCONTROL = 163;
  VK_RMENU = 165;
{ ImmGetVirtualKey  }
  VK_PROCESSKEY = 229;
  {$ENDIF}

  //VK_SPACE  = $20;
  //VK_PRIOR  = $21;
  //VK_NEXT   = $22;
  //VK_END    = $23;
  //VK_HOME   = $24;
  //VK_SELECT = $2;
  VK_LEFT   = $25;
  VK_UP     = $26;
  VK_RIGHT  = $27;
  VK_DOWN   = $28;
  VK_ESCAPE = $1B;
  VK_DELETE = $2E;
  VK_RETURN = $0D;

{$IFEND}

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

implementation

//se não esta definido FPC (consequentemente não estará definida LCLwin32),
//estou usando Delphi, consequentemente, Windows...
//
//if isn't set FPC (consequently will not defined LCLwin32),
//so, I'm using Delphi, consequently Windows.

{$IF defined(LCLwin32) OR defined(LCLwin64) OR defined(LCLwince) OR (not defined(FPC))}
  uses windows{$IFDEF FPC}, LCLMessageGlue, LCLType {$ELSE}, Messages {$ENDIF};
{$ELSE}
  uses LCLMessageGlue, LCLType;
{$IFEND}

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

constructor TCrossKeyEvents.Create(Target:TWinControl);
begin
  FTarget:=Target;
end;

procedure TCrossKeyEvents.SetTarget(Target:TWinControl);
begin
  FTarget:=Target;
end;

procedure TCrossKeyEvents.Down(Key: Word);
begin
  DoDown(TranlateVirtualKey(key));
end;

procedure TCrossKeyEvents.Up(Key: Word);
begin
  DoUp(TranlateVirtualKey(key));
end;

procedure TCrossKeyEvents.Press(Key: Word);
begin
  Down(Key);
  Up(Key);
end;

procedure TCrossKeyEvents.Apply(Shift: TShiftState);
begin
  if ssCtrl in Shift then Down(VK_CONTROL);
  if ssAlt in Shift then Down(VK_MENU);
  if ssShift in Shift then Down(VK_SHIFT);
end;

procedure TCrossKeyEvents.Unapply(Shift: TShiftState);
begin
  if ssCtrl in Shift then Up(VK_CONTROL);
  if ssAlt in Shift then Up(VK_MENU);
  if ssShift in Shift then Up(VK_SHIFT);
end;

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

{$IF defined(FPC) and defined(LCL)}
procedure TLazarusLCLKeyEvents.DoDown(Key: LongWord);
var
  aKey:Word;
begin
  aKey:=Key;
  LCLSendKeyDownEvent(FTarget,aKey,0,true,false);
end;

procedure TLazarusLCLKeyEvents.DoUp(Key: LongWord);
var
  aKey:Word;
begin
  aKey:=Key;
  LCLSendKeyUpEvent(FTarget,aKey,0,true,false);
end;

function TLazarusLCLKeyEvents.TranlateVirtualKey(Key: Word): LongWord;
begin
  Result:=Key;
end;
{$IFEND}

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

{$IF (not defined(FPC)) and defined(DELPHI2009_UP)}
procedure TDelphiVCLKeyEvents.DoDown(Key: LongWord);
begin
  SendMessage(FTarget.Handle,WM_KEYDOWN,Key,0);
  if (Key<>VK_DELETE) AND (Key in [VK_0..VK_9,PSVK_DECIMAL,PSVK_BACK,PSVK_SUBTRACT]) then
    SendMessage(FTarget.Handle,WM_CHAR,Key,0);
end;

procedure TDelphiVCLKeyEvents.DoUp(Key: LongWord);
begin
  SendMessage(FTarget.Handle,WM_KEYUP,Key,0);
end;

function TDelphiVCLKeyEvents.TranlateVirtualKey(Key: Word): LongWord;
begin
  Result := key;
end;
{$IFEND}


//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

function CreateCrossKeyEvents(Target:TWinControl):TCrossKeyEvents;
begin
  {$IF (not defined(FPC)) and defined(DELPHI2009_UP)}
  Result:=TDelphiVCLKeyEvents.Create(Target);
  {$IFEND}

  {$IF defined(FPC) and defined(LCL)}
  Result:=TLazarusLCLKeyEvents.Create(Target);
  {$IFEND}
end;

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

initialization

{$IFDEF WINDOWS}
PSVK_BACK     := 8;
PSVK_DECIMAL  := Ord(DecimalSeparator);
PSVK_SUBTRACT := Ord('-');
{$ELSE}
PSVK_BACK     := VK_BACK;
PSVK_DECIMAL  := VK_DECIMAL;
PSVK_SUBTRACT := VK_SUBTRACT;
{$ENDIF}

end.
