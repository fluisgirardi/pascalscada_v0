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

    {$IFDEF PORTUGUES}
    //: Emula o pressionamento de uma tecla.
    {$ELSE}
    //: Emulates the key press event.
    {$ENDIF}
    procedure DoDown(Key: LongWord); dynamic; abstract;

    {$IFDEF PORTUGUES}
    //: Emula o liberar de uma tecla.
    {$ELSE}
    //: Emulates a key release event.
    {$ENDIF}
    procedure DoUp(Key: LongWord); dynamic; abstract;

    {$IFDEF PORTUGUES}
    //: Traduz o codigo da tecla para o codigo da tecla no widgetset.
    {$ELSE}
    //: Translate the key code to the widgetset key code.
    {$ENDIF}
    function  TranlateVirtualKey(Key:Word):LongWord; dynamic; abstract;
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
    procedure Apply(Shift: TShiftState);
    {$IFDEF PORTUGUES}
    //: Remove modificadores de evento de tecla (Ctrl, Shift e Alt).
    {$ELSE}
    //: Removes key modification events (Ctrl, Shift and Alt).
    {$ENDIF}
    procedure Unapply(Shift: TShiftState);
  end;

  {$IF defined(LCLgtk2)}
  TGTK2KeyEvents = class(TCrossKeyEvents)
    protected
      // @seealso(TCrossKeyEvents.DoDown)
      procedure DoDown(Key: LongWord); override;
      // @seealso(TCrossKeyEvents.DoUp)
      procedure DoUp(Key: LongWord); override;
      // @seealso(TCrossKeyEvents.TranlateVirtualKey)
      function TranlateVirtualKey(Key: Word): LongWord; override;
  end;
  {$IFEND}

  {$IF defined(LCLqt)}
  TQT4KeyEvents = class(TCrossKeyEvents)
    protected
      // @seealso(TCrossKeyEvents.DoDown)
      procedure DoDown(Key: LongWord); override;
      // @seealso(TCrossKeyEvents.DoUp)
      procedure DoUp(Key: LongWord); override;
      // @seealso(TCrossKeyEvents.TranlateVirtualKey)
      function TranlateVirtualKey(Key: Word): LongWord; override;
  end;
  {$IFEND}

  {$IF defined(LCLwin32) OR (not defined(FPC))}
  TWindowsKeyEvents = class(TCrossKeyEvents)
    protected
      // @seealso(TCrossKeyEvents.DoDown)
      procedure DoDown(Key: LongWord); override;
      // @seealso(TCrossKeyEvents.DoUp)
      procedure DoUp(Key: LongWord); override;
      // @seealso(TCrossKeyEvents.TranlateVirtualKey)
      function TranlateVirtualKey(Key: Word): LongWord; override;
  end;
  {$IFEND}

  {$IFDEF PORTUGUES}
  //: Cria o emulador de eventos de tecla de acordo com o widgetset em uso.
  {$ELSE}
  //: Creates the key event emulator of the currently widgetset.
  {$ENDIF}
  function CreateCrossKeyEvents(Target:TWinControl):TCrossKeyEvents;

var
  PSVK_BACK, PSVK_DECIMAL, PSVK_SUBTRACT:Byte;

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


implementation

{$IF defined(LCLgtk2)}
uses gdk2, gtk2, gdk2x, glib2, LCLType, ctypes;
{$IFEND}

{$IF defined(LCLqt)}
uses {$IFDEF FPC}qt4, qtwidgets, qtobjects, LCLType{$ENDIF};
{$IFEND}

//se não esta definido FPC (consequentemente não estará definida LCLwin32),
//estou usando Delphi, consequentemente, Windows...
//
//if isn't set FPC (consequently will not defined LCLwin32),
//so, I'm using Delphi, consequently Windows.
{$IF defined(LCLwin32) OR defined(LCLwin64) OR defined(LCLwince) OR (not defined(FPC))}
uses windows{$IFDEF FPC}, LCLType {$ELSE}, Messages {$ENDIF};
{$IFEND}

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

{$IF defined(LCLgtk2)}
procedure TGTK2KeyEvents.DoDown(Key: LongWord);
var
  gev:TGdkEvent;
  keys:PGdkKeymapKey;
  nkeys:cint;
begin
  if FTarget=nil then exit;
  gev.key.window:=PGtkWidget(Ftarget.Handle)^.window;
  gev.key._type:=GDK_KEY_PRESS;
  gev.key.send_event:=1;
  gev.key.time:=10;
  gev.key.state:=0;
  gev.key.keyval:=key;
  gev.key.length:=1;
  gev.key._string:=gdk_keyval_name(key);

  gdk_keymap_get_entries_for_keyval(nil,gev.key.keyval,keys,@nkeys);

  if keys=nil then begin
    gev.key.hardware_keycode:=GDK_KEY_a;
    gev.key.group:=1;
  end else begin
    gev.key.hardware_keycode:=keys^.keycode;
    gev.key.group:=keys^.group;
    g_free(keys);
  end;

  gdk_event_put(@gev);
end;

procedure TGTK2KeyEvents.DoUp(Key: LongWord);
var
  gev:TGdkEvent;
  keys:PGdkKeymapKey;
  nkeys:cint;
begin
  if FTarget=nil then exit;
  gev.key.window:=PGtkWidget(Ftarget.Handle)^.window;
  gev.key._type:=GDK_KEY_RELEASE;
  gev.key.send_event:=1;
  gev.key.time:=10;
  gev.key.state:=0;
  gev.key.keyval:=key;
  gev.key.length:=1;
  gev.key._string:=gdk_keyval_name(key);

  gdk_keymap_get_entries_for_keyval(nil,gev.key.keyval,keys,@nkeys);

  if keys=nil then begin
    gev.key.hardware_keycode:=GDK_KEY_a;
    gev.key.group:=1;
  end else begin
    gev.key.hardware_keycode:=keys^.keycode;
    gev.key.group:=keys^.group;
    g_free(keys);
  end;

  gdk_event_put(@gev);
end;

function TGTK2KeyEvents.TranlateVirtualKey(Key: Word): LongWord;
begin
  case Key of
    VK_BACK: Result := GDK_KEY_BackSpace;
    VK_TAB: Result := GDK_KEY_Tab;
    VK_CLEAR: Result := GDK_KEY_Clear;
    VK_RETURN: Result := GDK_KEY_Return;
    VK_SHIFT: Result := GDK_KEY_Shift_L;
    VK_CONTROL: Result := GDK_KEY_Control_L;
    VK_MENU: Result := GDK_KEY_Menu; // alt key crashes app, GDK_KEY_Alt_R;
    VK_CAPITAL: Result := GDK_KEY_Caps_Lock;

    VK_ESCAPE: Result := GDK_KEY_Escape;
    VK_SPACE: Result := GDK_KEY_space;
    VK_PRIOR: Result := GDK_KEY_Prior;
    VK_NEXT: Result := GDK_KEY_Next;
    VK_END: Result := GDK_KEY_End;
    VK_HOME: Result := GDK_KEY_Home;
    VK_LEFT: Result := GDK_KEY_Left;
    VK_UP: Result := GDK_KEY_Up;
    VK_RIGHT: Result := GDK_KEY_Right;
    VK_DOWN: Result := GDK_KEY_Down;
    VK_SELECT: Result := GDK_KEY_Select;
    VK_PRINT: Result := GDK_KEY_Print;
    VK_EXECUTE: Result := GDK_KEY_Execute;

    VK_INSERT: Result := GDK_KEY_Insert;
    VK_DELETE: Result := GDK_KEY_Delete;
    VK_HELP: Result := GDK_KEY_Help;
    VK_0: Result := GDK_KEY_0;
    VK_1: Result := GDK_KEY_1;
    VK_2: Result := GDK_KEY_2;
    VK_3: Result := GDK_KEY_3;
    VK_4: Result := GDK_KEY_4;
    VK_5: Result := GDK_KEY_5;
    VK_6: Result := GDK_KEY_6;
    VK_7: Result := GDK_KEY_7;
    VK_8: Result := GDK_KEY_8;
    VK_9: Result := GDK_KEY_9;

    VK_A: Result := GDK_KEY_a;
    VK_B: Result := GDK_KEY_b;
    VK_C: Result := GDK_KEY_c;
    VK_D: Result := GDK_KEY_d;
    VK_E: Result := GDK_KEY_e;
    VK_F: Result := GDK_KEY_f;
    VK_G: Result := GDK_KEY_g;
    VK_H: Result := GDK_KEY_h;
    VK_I: Result := GDK_KEY_i;
    VK_J: Result := GDK_KEY_j;
    VK_K: Result := GDK_KEY_k;
    VK_L: Result := GDK_KEY_l;
    VK_M: Result := GDK_KEY_m;
    VK_N: Result := GDK_KEY_n;
    VK_O: Result := GDK_KEY_o;
    VK_P: Result := GDK_KEY_p;
    VK_Q: Result := GDK_KEY_q;
    VK_R: Result := GDK_KEY_r;
    VK_S: Result := GDK_KEY_s;
    VK_T: Result := GDK_KEY_t;
    VK_U: Result := GDK_KEY_u;
    VK_V: Result := GDK_KEY_v;
    VK_W: Result := GDK_KEY_w;
    VK_X: Result := GDK_KEY_x;
    VK_Y: Result := GDK_KEY_y;
    VK_Z: Result := GDK_KEY_z;

    VK_NUMPAD0: Result := GDK_KEY_KP_0;
    VK_NUMPAD1: Result := GDK_KEY_KP_1;
    VK_NUMPAD2: Result := GDK_KEY_KP_2;
    VK_NUMPAD3: Result := GDK_KEY_KP_3;
    VK_NUMPAD4: Result := GDK_KEY_KP_4;
    VK_NUMPAD5: Result := GDK_KEY_KP_5;
    VK_NUMPAD6: Result := GDK_KEY_KP_6;
    VK_NUMPAD7: Result := GDK_KEY_KP_7;
    VK_NUMPAD8: Result := GDK_KEY_KP_8;
    VK_NUMPAD9: Result := GDK_KEY_KP_9;
    VK_MULTIPLY: Result := GDK_KEY_KP_Multiply;
    VK_ADD: Result := GDK_KEY_KP_Add;
    VK_SEPARATOR: Result := GDK_KEY_KP_Separator;
    VK_SUBTRACT: Result := GDK_KEY_KP_Subtract;
    VK_DECIMAL: Result := GDK_KEY_KP_Decimal;
    VK_DIVIDE: Result := GDK_KEY_KP_Divide;
    VK_F1: Result := GDK_KEY_F1;
    VK_F2: Result := GDK_KEY_F2;
    VK_F3: Result := GDK_KEY_F3;
    VK_F4: Result := GDK_KEY_F4;
    VK_F5: Result := GDK_KEY_F5;
    VK_F6: Result := GDK_KEY_F6;
    VK_F7: Result := GDK_KEY_F7;
    VK_F8: Result := GDK_KEY_F8;
    VK_F9: Result := GDK_KEY_F9;
    VK_F10: Result := GDK_KEY_F10;
    VK_F11: Result := GDK_KEY_F11;
    VK_F12: Result := GDK_KEY_F12;
    VK_F13: Result := GDK_KEY_F13;
    VK_F14: Result := GDK_KEY_F14;
    VK_F15: Result := GDK_KEY_F15;
    VK_F16: Result := GDK_KEY_F16;
    VK_F17: Result := GDK_KEY_F17;
    VK_F18: Result := GDK_KEY_F18;
    VK_F19: Result := GDK_KEY_F19;
    VK_F20: Result := GDK_KEY_F20;
    VK_F21: Result := GDK_KEY_F21;
    VK_F22: Result := GDK_KEY_F22;
    VK_F23: Result := GDK_KEY_F23;
    VK_F24: Result := GDK_KEY_F24;
    VK_NUMLOCK: Result := GDK_KEY_Num_Lock;
    VK_SCROLL: Result := GDK_KEY_Scroll_Lock;
  else
    Result := GDK_KEY_VoidSymbol;
  end;
end;
{$IFEND}

{$IF defined(LCLqt)}
procedure TQT4KeyEvents.DoDown(Key: LongWord);
var
  qevt:QKeyEventH;
  ktxt:WideString;
begin
  ktxt:=chr(key);

  qevt:=QKeyEvent_create(QEventKeyPress, key, QtNoModifier, @ktxt, false, 1);

  QCoreApplication_sendEvent(TQtWidget(FTarget.Handle).Widget,qevt);
  QKeyEvent_destroy(qevt);
end;

procedure TQT4KeyEvents.DoUp(Key: LongWord);
var
  qevt:QKeyEventH;
begin
  qevt:=QKeyEvent_create(QEventKeyRelease, key, QtNoModifier, nil, false, 1);

  QCoreApplication_sendEvent(TQtWidget(FTarget.Handle).Widget,qevt);
  QKeyEvent_destroy(qevt);
end;

function TQT4KeyEvents.TranlateVirtualKey(Key: Word): LongWord;
begin
  case Key of
    VK_BACK: Result := QtKey_Backspace;
    VK_TAB: Result := QtKey_Tab;
    VK_CLEAR: Result := QtKey_Clear;
    VK_RETURN: Result := QtKey_Return;
    VK_SHIFT: Result := QtKey_Shift;
    VK_CONTROL: Result := QtKey_Control;
    VK_MENU: Result := QtKey_Menu; // alt key crashes app, QtKey_Alt_R;
    VK_CAPITAL: Result := QtKey_CapsLock;

    VK_ESCAPE: Result := QtKey_Escape;
    VK_SPACE: Result := QtKey_space;
    VK_PRIOR: Result := QtKey_PageUp;
    VK_NEXT: Result := QtKey_PageDown;
    VK_END: Result := QtKey_End;
    VK_HOME: Result := QtKey_Home;
    VK_LEFT: Result := QtKey_Left;
    VK_UP: Result := QtKey_Up;
    VK_RIGHT: Result := QtKey_Right;
    VK_DOWN: Result := QtKey_Down;
    VK_SELECT: Result := QtKey_Select;
    VK_PRINT: Result := QtKey_Print;
    VK_EXECUTE: Result := QtKey_Execute;

    VK_INSERT: Result := QtKey_Insert;
    VK_DELETE: Result := QtKey_Delete;
    VK_HELP: Result := QtKey_Help;
    VK_0: Result := QtKey_0;
    VK_1: Result := QtKey_1;
    VK_2: Result := QtKey_2;
    VK_3: Result := QtKey_3;
    VK_4: Result := QtKey_4;
    VK_5: Result := QtKey_5;
    VK_6: Result := QtKey_6;
    VK_7: Result := QtKey_7;
    VK_8: Result := QtKey_8;
    VK_9: Result := QtKey_9;

    VK_A: Result := QtKey_a;
    VK_B: Result := QtKey_b;
    VK_C: Result := QtKey_c;
    VK_D: Result := QtKey_d;
    VK_E: Result := QtKey_e;
    VK_F: Result := QtKey_f;
    VK_G: Result := QtKey_g;
    VK_H: Result := QtKey_h;
    VK_I: Result := QtKey_i;
    VK_J: Result := QtKey_j;
    VK_K: Result := QtKey_k;
    VK_L: Result := QtKey_l;
    VK_M: Result := QtKey_m;
    VK_N: Result := QtKey_n;
    VK_O: Result := QtKey_o;
    VK_P: Result := QtKey_p;
    VK_Q: Result := QtKey_q;
    VK_R: Result := QtKey_r;
    VK_S: Result := QtKey_s;
    VK_T: Result := QtKey_t;
    VK_U: Result := QtKey_u;
    VK_V: Result := QtKey_v;
    VK_W: Result := QtKey_w;
    VK_X: Result := QtKey_x;
    VK_Y: Result := QtKey_y;
    VK_Z: Result := QtKey_z;

    VK_NUMPAD0: Result := QtKey_0;
    VK_NUMPAD1: Result := QtKey_1;
    VK_NUMPAD2: Result := QtKey_2;
    VK_NUMPAD3: Result := QtKey_3;
    VK_NUMPAD4: Result := QtKey_4;
    VK_NUMPAD5: Result := QtKey_5;
    VK_NUMPAD6: Result := QtKey_6;
    VK_NUMPAD7: Result := QtKey_7;
    VK_NUMPAD8: Result := QtKey_8;
    VK_NUMPAD9: Result := QtKey_9;
    VK_MULTIPLY: Result := QtKey_Asterisk;
    VK_ADD: Result := QtKey_Plus;
    VK_SEPARATOR: Result := QtKey_Comma;
    VK_SUBTRACT: Result := QtKey_Minus;
    VK_DECIMAL: Result := QtKey_Period;
    VK_DIVIDE: Result := QtKey_Slash;
    VK_F1: Result := QtKey_F1;
    VK_F2: Result := QtKey_F2;
    VK_F3: Result := QtKey_F3;
    VK_F4: Result := QtKey_F4;
    VK_F5: Result := QtKey_F5;
    VK_F6: Result := QtKey_F6;
    VK_F7: Result := QtKey_F7;
    VK_F8: Result := QtKey_F8;
    VK_F9: Result := QtKey_F9;
    VK_F10: Result := QtKey_F10;
    VK_F11: Result := QtKey_F11;
    VK_F12: Result := QtKey_F12;
    VK_F13: Result := QtKey_F13;
    VK_F14: Result := QtKey_F14;
    VK_F15: Result := QtKey_F15;
    VK_F16: Result := QtKey_F16;
    VK_F17: Result := QtKey_F17;
    VK_F18: Result := QtKey_F18;
    VK_F19: Result := QtKey_F19;
    VK_F20: Result := QtKey_F20;
    VK_F21: Result := QtKey_F21;
    VK_F22: Result := QtKey_F22;
    VK_F23: Result := QtKey_F23;
    VK_F24: Result := QtKey_F24;
    VK_NUMLOCK: Result := QtKey_NumLock;
    VK_SCROLL: Result := QtKey_ScrollLock;
  else
    Result := QtKey_0;
  end;
end;
{$IFEND}

{$IF defined(LCLwin32) OR (not defined(FPC))}
procedure TWindowsKeyEvents.DoDown(Key: LongWord);
begin
  SendMessage(FTarget.Handle,WM_KEYDOWN,Key,0);
  if (Key<>VK_DELETE) AND (Key in [VK_0..VK_9,PSVK_DECIMAL,PSVK_BACK,PSVK_SUBTRACT]) then
    SendMessage(FTarget.Handle,WM_CHAR,Key,0);
end;

procedure TWindowsKeyEvents.DoUp(Key: LongWord);
begin
  SendMessage(FTarget.Handle,WM_KEYUP,Key,0);
end;

function TWindowsKeyEvents.TranlateVirtualKey(Key: Word): LongWord;
begin
  Result := key;
end;

{$IFEND}

function CreateCrossKeyEvents(Target:TWinControl):TCrossKeyEvents;
begin
  {$IF defined(LCLgtk2)}
  Result:=TGtk2KeyEvents.Create(Target);
  {$IFEND}

  {$IF defined(LCLqt)}
  Result:=TQt4KeyEvents.Create(Target);
  {$IFEND}

  {$IF defined(LCLwin32) OR (not defined(FPC))}
  Result:=TWindowsKeyEvents.Create(Target);
  {$IFEND}
end;

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
