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
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ThisWidgetsetHasAnEmulator;
    procedure TypingALetterReachesTheFocusedField;
  end;

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

procedure TTestKeyEventsForThisWidgetset.TypingALetterReachesTheFocusedField;
var
  teclado:TCrossKeyEvents;
  limite:QWord;
begin
  //o caminho inteiro: a tecla desenhada vira evento do sistema e chega ao
  //campo como se tivesse vindo de um teclado de verdade
  //the whole path: the drawn key becomes a system event and reaches the field
  //as if it had come from a real keyboard
  FForm.Show;
  FEdit.SetFocus;
  Application.ProcessMessages;

  teclado:=CreateCrossKeyEvents(FEdit);
  try
    teclado.Press(VK_A);

    limite:=GetTickCount64+2000;
    while (FEdit.Text='') and (GetTickCount64<limite) do begin
      Application.ProcessMessages;
      Sleep(2);
    end;

    AssertEquals('a letra chegou ao campo', 'a', LowerCase(FEdit.Text));
  finally
    teclado.Free;
  end;
end;

initialization
  RegisterTest(TTestCrossKeyEvents);
  RegisterTest(TTestKeyEventsForThisWidgetset);

end.
