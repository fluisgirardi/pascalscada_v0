{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do emulador de teclado: o que o teclado de tela usa para
            digitar no controle que esta' em foco.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Num painel sem teclado fisico, o operador digita numa imagem de teclado na
  propria tela. Cada tecla desenhada vira um evento de tecla de verdade,
  entregue ao controle alvo pelo caminho do sistema - e' esta unit que
  constroi esse evento, um jeito para cada widgetset.

  A parte comum - traduzir, mandar a descida e a subida, aplicar e tirar os
  modificadores - e' o que se testa aqui de forma igual em toda plataforma.
}
{$ELSE}
{:
  @abstract(Keyboard emulator tests: what the on-screen keyboard uses to type
            into the focused control.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  On a panel with no physical keyboard, the operator types on a picture of a
  keyboard on the screen itself. Each drawn key becomes a real key event,
  delivered to the target control through the system's own path - this unit is
  what builds that event, one way per widgetset.

  The common part - translating, sending the press and the release, applying
  and removing the modifiers - is what is tested here, the same way on every
  platform.
}
{$ENDIF}
unit ut.crosskeyevents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, LCLType, fpcunit, testregistry,
  {$IF defined(LCLgtk2)}gdk2,{$IFEND}
  CrossKeyEvents;

type

  { TKeyEventsProbe }

  //DoDown, DoUp e TranlateVirtualKey sao abstratos: cada widgetset os
  //implementa do seu jeito. A sonda os implementa anotando o que foi pedido,
  //para a logica comum poder ser conferida em qualquer plataforma.
  //
  //DoDown, DoUp and TranlateVirtualKey are abstract: each widgetset implements
  //them its own way. The probe implements them by writing down what was asked,
  //so the common logic can be checked on any platform.
  TKeyEventsProbe = class(TCrossKeyEvents)
  private
    FLog:AnsiString;
  protected
    procedure DoDown(Key: LongWord); override;
    procedure DoUp(Key: LongWord); override;
    function  TranlateVirtualKey(Key:Word):LongWord; override;
  public
    procedure Forget;
    function  TargetIs:TWinControl;
    property Log:AnsiString read FLog;
  end;

  { TTestCrossKeyEvents }

  TTestCrossKeyEvents = class(TTestCase)
  private
    FProbe:TKeyEventsProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a tecla / the key
    procedure ThePressGoesDownAndThenUp;
    procedure EveryEventGoesThroughTheTranslation;
    procedure DownAndUpCanBeSentOnTheirOwn;

    //os modificadores / the modifiers
    procedure ApplyPressesOnlyTheModifiersAsked;
    procedure ApplyPressesTheThreeModifiers;
    procedure UnapplyReleasesTheModifiersAsked;
    procedure AnEmptyShiftStateTouchesNothing;

    //o alvo / the target
    procedure TheTargetComesFromTheConstructor;
    procedure TheTargetCanBeChangedLater;
  end;

  { TTestKeyEventsForThisWidgetset }

  TTestKeyEventsForThisWidgetset = class(TTestCase)
  private
    FForm:TForm;
    FEdit:TEdit;
    procedure FocusTheField;
    procedure TypeAndWait(aKeyboard:TCrossKeyEvents; aKey:Word; const aExpected:String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ThisWidgetsetHasAnEmulator;
    procedure TypingALetterReachesTheFocusedField;
    procedure TypingADigitReachesTheField;
    procedure TypingAKeypadDigitReachesTheField;
    procedure TypingTheKeypadMinusReachesTheField;
    procedure TypingTheDecimalSeparatorReachesTheField;
    procedure BackspaceErasesTheLastCharacter;
    procedure ShiftMakesTheLetterUpperCase;
  end;

  {$IF defined(LCLgtk2)}
  { TGTK2TranslationProbe }

  //a tabela de traducao e' protegida: a sonda a expoe
  //the translation table is protected: the probe exposes it
  TGTK2TranslationProbe = class(TGTK2KeyEvents)
  public
    function Translate(aKey:Word):LongWord;
  end;

  { TTestGTK2Translation }

  //do codigo de tecla virtual do LCL para o keysym do GDK: e' o que decide
  //qual tecla o sistema ve quando o teclado de tela e' tocado
  //from the LCL virtual key code to the GDK keysym: it is what decides which
  //key the system sees when the on-screen keyboard is touched
  TTestGTK2Translation = class(TTestCase)
  private
    FProbe:TGTK2TranslationProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheDigitsAreTranslated;
    procedure TheLettersAreTranslatedToLowerCase;
    procedure TheKeypadDigitsAreTranslated;
    procedure TheKeypadOperatorsAreTranslated;
    procedure TheEditingKeysAreTranslated;
    procedure TheFunctionKeysAreTranslated;
    procedure AnUnknownKeyIsVoid;
  end;
  {$IFEND}

implementation

{ TKeyEventsProbe }

procedure TKeyEventsProbe.DoDown(Key: LongWord);
begin
  FLog:=FLog+'v'+IntToStr(Key)+' ';
end;

procedure TKeyEventsProbe.DoUp(Key: LongWord);
begin
  FLog:=FLog+'^'+IntToStr(Key)+' ';
end;

function TKeyEventsProbe.TranlateVirtualKey(Key:Word):LongWord;
begin
  //uma traducao boba e reconhecivel: some 1000 ao codigo da tecla
  //a silly, recognisable translation: add 1000 to the key code
  Result:=Key+1000;
end;

procedure TKeyEventsProbe.Forget;
begin
  FLog:='';
end;

function TKeyEventsProbe.TargetIs:TWinControl;
begin
  Result:=FTarget;
end;

{ TTestCrossKeyEvents }

procedure TTestCrossKeyEvents.SetUp;
begin
  FProbe:=TKeyEventsProbe.Create(nil);
end;

procedure TTestCrossKeyEvents.TearDown;
begin
  FreeAndNil(FProbe);
end;

procedure TTestCrossKeyEvents.ThePressGoesDownAndThenUp;
begin
  //uma tecla apertada e' uma descida seguida de uma subida, nessa ordem
  //a pressed key is a press followed by a release, in that order
  FProbe.Press(VK_A);

  AssertEquals('desceu e subiu', 'v1065 ^1065 ', FProbe.Log);
end;

procedure TTestCrossKeyEvents.EveryEventGoesThroughTheTranslation;
begin
  //o codigo que chega e' o virtual do LCL; o que sai e' o do widgetset
  //the code coming in is the LCL virtual one; what goes out is the
  //widgetset's
  FProbe.Down(VK_RETURN);

  AssertEquals('traduzido', 'v'+IntToStr(VK_RETURN+1000)+' ', FProbe.Log);
end;

procedure TTestCrossKeyEvents.DownAndUpCanBeSentOnTheirOwn;
begin
  //e' o que uma tecla mantida pressionada precisa: descer agora, subir depois
  //it is what a held key needs: press now, release later
  FProbe.Down(VK_SHIFT);
  FProbe.Forget;

  FProbe.Up(VK_SHIFT);

  AssertEquals('so a subida', '^'+IntToStr(VK_SHIFT+1000)+' ', FProbe.Log);
end;

procedure TTestCrossKeyEvents.ApplyPressesOnlyTheModifiersAsked;
begin
  FProbe.Apply([ssCtrl]);

  AssertEquals('so o controle', 'v'+IntToStr(VK_CONTROL+1000)+' ', FProbe.Log);
end;

procedure TTestCrossKeyEvents.ApplyPressesTheThreeModifiers;
begin
  FProbe.Apply([ssCtrl, ssAlt, ssShift]);

  AssertEquals('os tres, nesta ordem',
               'v'+IntToStr(VK_CONTROL+1000)+' '+
               'v'+IntToStr(VK_MENU+1000)+' '+
               'v'+IntToStr(VK_SHIFT+1000)+' ', FProbe.Log);
end;

procedure TTestCrossKeyEvents.UnapplyReleasesTheModifiersAsked;
begin
  FProbe.Apply([ssCtrl, ssShift]);
  FProbe.Forget;

  FProbe.Unapply([ssCtrl, ssShift]);

  AssertEquals('soltou os dois',
               '^'+IntToStr(VK_CONTROL+1000)+' '+
               '^'+IntToStr(VK_SHIFT+1000)+' ', FProbe.Log);
end;

procedure TTestCrossKeyEvents.AnEmptyShiftStateTouchesNothing;
begin
  FProbe.Apply([]);
  FProbe.Unapply([]);

  AssertEquals('nenhum evento', '', FProbe.Log);
end;

procedure TTestCrossKeyEvents.TheTargetComesFromTheConstructor;
var
  form:TForm;
  outro:TKeyEventsProbe;
begin
  form:=TForm.CreateNew(nil);
  try
    outro:=TKeyEventsProbe.Create(form);
    try
      AssertSame('o alvo dado na criacao', form, outro.TargetIs);
    finally
      outro.Free;
    end;
  finally
    form.Free;
  end;
end;

procedure TTestCrossKeyEvents.TheTargetCanBeChangedLater;
var
  form:TForm;
begin
  //o teclado de tela troca de alvo a cada campo que ganha o foco
  //the on-screen keyboard changes target on every field that takes focus
  form:=TForm.CreateNew(nil);
  try
    FProbe.SetTarget(form);

    AssertSame('o alvo novo', form, FProbe.TargetIs);
  finally
    form.Free;
  end;
end;

{ TTestKeyEventsForThisWidgetset }

procedure TTestKeyEventsForThisWidgetset.SetUp;
begin
  FForm:=TForm.CreateNew(nil);
  FForm.Visible:=false;
  FForm.SetBounds(0,0,300,200);
  FEdit:=TEdit.Create(FForm);
  FEdit.Parent:=FForm;
end;

procedure TTestKeyEventsForThisWidgetset.TearDown;
begin
  FreeAndNil(FForm);
end;

procedure TTestKeyEventsForThisWidgetset.ThisWidgetsetHasAnEmulator;
var
  teclado:TCrossKeyEvents;
begin
  //sem emulador para o widgetset em uso, o teclado de tela nao existe - e a
  //fabrica avisa isso levantando excecao, em vez de devolver nada
  //with no emulator for the widgetset in use there is no on-screen keyboard -
  //and the factory says so by raising, rather than handing back nothing
  teclado:=CreateCrossKeyEvents(FEdit);
  try
    AssertNotNull('a fabrica entregou um emulador', teclado);
  finally
    teclado.Free;
  end;
end;

procedure TTestKeyEventsForThisWidgetset.FocusTheField;
begin
  FForm.Show;
  FEdit.SetFocus;
  Application.ProcessMessages;
end;

procedure TTestKeyEventsForThisWidgetset.TypeAndWait(aKeyboard:TCrossKeyEvents; aKey:Word; const aExpected:String);
var
  limite:QWord;
begin
  //o evento vai para a fila do sistema; e' preciso bombear ate' ele voltar
  //the event goes to the system's queue; it has to be pumped until it comes back
  aKeyboard.Press(aKey);
  limite:=GetTickCount64+2000;
  while (FEdit.Text<>aExpected) and (GetTickCount64<limite) do begin
    Application.ProcessMessages;
    Sleep(2);
  end;
end;

procedure TTestKeyEventsForThisWidgetset.TypingALetterReachesTheFocusedField;
var
  teclado:TCrossKeyEvents;
begin
  //o caminho inteiro: a tecla desenhada vira evento do sistema e chega ao
  //campo como se tivesse vindo de um teclado de verdade
  //the whole path: the drawn key becomes a system event and reaches the field
  //as if it had come from a real keyboard
  FocusTheField;

  teclado:=CreateCrossKeyEvents(FEdit);
  try
    TypeAndWait(teclado, VK_A, 'a');

    AssertEquals('a letra chegou ao campo', 'a', LowerCase(FEdit.Text));
  finally
    teclado.Free;
  end;
end;

procedure TTestKeyEventsForThisWidgetset.TypingADigitReachesTheField;
var
  teclado:TCrossKeyEvents;
begin
  FocusTheField;
  teclado:=CreateCrossKeyEvents(FEdit);
  try
    TypeAndWait(teclado, VK_5, '5');

    AssertEquals('o digito chegou', '5', FEdit.Text);
  finally
    teclado.Free;
  end;
end;

procedure TTestKeyEventsForThisWidgetset.TypingAKeypadDigitReachesTheField;
var
  teclado:TCrossKeyEvents;
begin
  //e' o que o teclado numerico de tela manda
  //it is what the numeric on-screen keyboard sends
  FocusTheField;
  teclado:=CreateCrossKeyEvents(FEdit);
  try
    TypeAndWait(teclado, VK_NUMPAD7, '7');

    AssertEquals('o digito do teclado numerico chegou', '7', FEdit.Text);
  finally
    teclado.Free;
  end;
end;

procedure TTestKeyEventsForThisWidgetset.TypingTheKeypadMinusReachesTheField;
var
  teclado:TCrossKeyEvents;
begin
  FocusTheField;
  teclado:=CreateCrossKeyEvents(FEdit);
  try
    TypeAndWait(teclado, VK_SUBTRACT, '-');

    AssertEquals('o sinal chegou', '-', FEdit.Text);
  finally
    teclado.Free;
  end;
end;

procedure TTestKeyEventsForThisWidgetset.TypingTheDecimalSeparatorReachesTheField;
var
  teclado:TCrossKeyEvents;
  limite:QWord;
begin
  //e' a tecla do separador decimal do teclado numerico de tela; o caractere
  //que sai segue o layout da maquina - ponto ou virgula - mas nunca pode
  //virar um Delete, que era o que acontecia com o Num Lock desligado
  //it is the decimal separator key of the numeric on-screen keyboard; the
  //character that comes out follows the machine's layout - point or comma -
  //but must never turn into a Delete, which is what happened with Num Lock off
  FocusTheField;
  teclado:=CreateCrossKeyEvents(FEdit);
  try
    teclado.Press(VK_OEM_PERIOD);
    limite:=GetTickCount64+2000;
    while (FEdit.Text='') and (GetTickCount64<limite) do begin
      Application.ProcessMessages;
      Sleep(2);
    end;

    AssertTrue('ponto ou virgula: "'+FEdit.Text+'"', (FEdit.Text='.') or (FEdit.Text=','));
  finally
    teclado.Free;
  end;
end;

procedure TTestKeyEventsForThisWidgetset.BackspaceErasesTheLastCharacter;
var
  teclado:TCrossKeyEvents;
begin
  FocusTheField;
  teclado:=CreateCrossKeyEvents(FEdit);
  try
    TypeAndWait(teclado, VK_5, '5');
    TypeAndWait(teclado, VK_6, '56');
    AssertEquals('dois digitos', '56', FEdit.Text);

    TypeAndWait(teclado, VK_BACK, '5');

    AssertEquals('o ultimo foi apagado', '5', FEdit.Text);
  finally
    teclado.Free;
  end;
end;

procedure TTestKeyEventsForThisWidgetset.ShiftMakesTheLetterUpperCase;
var
  teclado:TCrossKeyEvents;
begin
  //os modificadores aplicados entram no estado do evento
  //the applied modifiers go into the event's state
  FocusTheField;
  teclado:=CreateCrossKeyEvents(FEdit);
  try
    teclado.Apply([ssShift]);
    TypeAndWait(teclado, VK_A, 'A');
    teclado.Unapply([ssShift]);

    AssertEquals('maiuscula', 'A', FEdit.Text);
  finally
    teclado.Free;
  end;
end;

{$IF defined(LCLgtk2)}

{ TGTK2TranslationProbe }

function TGTK2TranslationProbe.Translate(aKey:Word):LongWord;
begin
  Result:=TranlateVirtualKey(aKey);
end;

{ TTestGTK2Translation }

procedure TTestGTK2Translation.SetUp;
begin
  FProbe:=TGTK2TranslationProbe.Create(nil);
end;

procedure TTestGTK2Translation.TearDown;
begin
  FreeAndNil(FProbe);
end;

procedure TTestGTK2Translation.TheDigitsAreTranslated;
begin
  AssertEquals('0', GDK_KEY_0, FProbe.Translate(VK_0));
  AssertEquals('5', GDK_KEY_5, FProbe.Translate(VK_5));
  AssertEquals('9', GDK_KEY_9, FProbe.Translate(VK_9));
end;

procedure TTestGTK2Translation.TheLettersAreTranslatedToLowerCase;
begin
  //a caixa vem do shift no evento, nao da tecla
  //the case comes from the shift in the event, not from the key
  AssertEquals('a', GDK_KEY_a, FProbe.Translate(VK_A));
  AssertEquals('z', GDK_KEY_z, FProbe.Translate(VK_Z));
end;

procedure TTestGTK2Translation.TheKeypadDigitsAreTranslated;
begin
  AssertEquals('KP 0', GDK_KEY_KP_0, FProbe.Translate(VK_NUMPAD0));
  AssertEquals('KP 9', GDK_KEY_KP_9, FProbe.Translate(VK_NUMPAD9));
end;

procedure TTestGTK2Translation.TheKeypadOperatorsAreTranslated;
begin
  //o teclado numerico de tela usa o menos e o ponto do teclado numerico
  //the numeric on-screen keyboard uses the keypad minus and decimal
  AssertEquals('menos',   GDK_KEY_KP_Subtract, FProbe.Translate(VK_SUBTRACT));
  AssertEquals('decimal', GDK_KEY_KP_Decimal,  FProbe.Translate(VK_DECIMAL));
  AssertEquals('mais',    GDK_KEY_KP_Add,      FProbe.Translate(VK_ADD));
end;

procedure TTestGTK2Translation.TheEditingKeysAreTranslated;
begin
  AssertEquals('backspace', GDK_KEY_BackSpace, FProbe.Translate(VK_BACK));
  AssertEquals('return',    GDK_KEY_Return,    FProbe.Translate(VK_RETURN));
  AssertEquals('delete',    GDK_KEY_Delete,    FProbe.Translate(VK_DELETE));
  AssertEquals('tab',       GDK_KEY_Tab,       FProbe.Translate(VK_TAB));
  AssertEquals('escape',    GDK_KEY_Escape,    FProbe.Translate(VK_ESCAPE));
  AssertEquals('espaco',    GDK_KEY_space,     FProbe.Translate(VK_SPACE));
end;

procedure TTestGTK2Translation.TheFunctionKeysAreTranslated;
begin
  AssertEquals('F1',  GDK_KEY_F1,  FProbe.Translate(VK_F1));
  AssertEquals('F12', GDK_KEY_F12, FProbe.Translate(VK_F12));
end;

procedure TTestGTK2Translation.AnUnknownKeyIsVoid;
begin
  //uma tecla sem traducao nao pode virar uma tecla qualquer
  //a key with no translation must not turn into some other key
  AssertEquals('void', GDK_KEY_VoidSymbol, FProbe.Translate(VK_OEM_1));
end;
{$IFEND}

initialization
  RegisterTest(TTestCrossKeyEvents);
  RegisterTest(TTestKeyEventsForThisWidgetset);
  {$IF defined(LCLgtk2)}
  RegisterTest(TTestGTK2Translation);
  {$IFEND}

end.
