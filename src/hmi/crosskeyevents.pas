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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  { TWindowsVirtualKeyboard }

  TWindowsVirtualKeyboard = class(TCrossKeyEvents)
  protected
    procedure DoDown(Key: LongWord); override;
    procedure DoUp(Key: LongWord); override;
    function TranlateVirtualKey(Key: Word): LongWord; override;
  end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


  {$IFDEF PORTUGUES}
  //: Cria o emulador de eventos de tecla de acordo com o widgetset em uso.
  {$ELSE}
  //: Creates the key event emulator of the currently widgetset.
  {$ENDIF}
  function CreateCrossKeyEvents(Target:TWinControl):TCrossKeyEvents;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

implementation

//se não esta definido FPC (consequentemente não estará definida LCLwin32),
//estou usando Delphi, consequentemente, Windows...
//
//if isn't set FPC (consequently will not defined LCLwin32),
//so, I'm using Delphi, consequently Windows.

{$IF defined(win32) OR defined(win64) OR defined(wince) OR defined(windows)}
  uses windows{$IFDEF FPC}, JwaWinUser{$ENDIF};
{$IFEND}

{ TWindowsVirtualKeyboard }

procedure TWindowsVirtualKeyboard.DoDown(Key: LongWord);
var
  Input: TInput;
begin
  FillChar(Input, SizeOf(Input), 0);
  Input.type_ := INPUT_KEYBOARD;
  Input.ki.dwFlags := 0;
  Input.ki.wVk := Key;

  SendInput(1, @Input, SizeOf(Input));
end;

procedure TWindowsVirtualKeyboard.DoUp(Key: LongWord);
var
  Input: TInput;
begin
  FillChar(Input, SizeOf(Input), 0);
  Input.type_ := INPUT_KEYBOARD;
  Input.ki.dwFlags := KEYEVENTF_KEYUP;
  Input.ki.wVk := Key;

  SendInput(1, @Input, SizeOf(Input));
end;

function TWindowsVirtualKeyboard.TranlateVirtualKey(Key: Word): LongWord;
begin
  Result:=Key;
end;



////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function CreateCrossKeyEvents(Target:TWinControl):TCrossKeyEvents;
begin
  {$IF defined(win32) OR defined(win64) OR defined(wince) OR defined(windows)}
  Result:=TWindowsVirtualKeyboard.Create(Target);
  {$IFEND}
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

initialization


end.

