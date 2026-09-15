{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIKeyboardManager: quem decide, a cada campo focado,
            se aparece teclado de tela e qual deles.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Num painel sem teclado fisico, o gerenciador escuta a troca de foco da tela
  inteira e pergunta a aplicacao, por evento, que teclado aquele campo quer -
  nenhum, numerico ou alfanumerico. Para o campo que quer teclado, ele assume
  os eventos de clique, entrada e saida do controle, e devolve os originais
  quando o foco sai.

  Essa troca e' o que se testa aqui: assumir sem perder o que era do campo, e
  devolver inteiro ao sair.
}
{$ELSE}
{:
  @abstract(THMIKeyboardManager tests: what decides, on every focused field,
            whether an on-screen keyboard shows up and which one.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  On a panel with no physical keyboard, the manager listens to the focus
  changes of the whole screen and asks the application, through an event, which
  keyboard that field wants - none, numeric or alphanumeric. For a field that
  wants one, it takes over the control's click, enter and exit events, and
  hands the originals back when focus leaves.

  That swap is what is tested here: taking over without losing what belonged to
  the field, and handing it back whole on the way out.
}
{$ENDIF}
unit ut.keyboardmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, fpcunit, testregistry,
  HMIKeyboardManager;

type

  { TTestKeyboardManager }

  TTestKeyboardManager = class(TTestCase)
  private
    FForm:TForm;
    FEdit1, FEdit2:TEdit;
    FManager:THMIKeyboardManager;
    FAsked:LongInt;
    FLastAsked:TControl;
    FAnswer:TOnScreenKeyboard;
    FMyClicks:LongInt;
    procedure FocusChange(FocusedControl:TControl;
                          var KeyboarTypeForControl:TOnScreenKeyboard;
                          var NumericKBOptions:TNumericScreenKeyboardOptions;
                          var AlphaNumKBOptions:TAlphaNumericScreenKeyBoardOptions;
                          var ShowKeyboardNow:Boolean);
    procedure MyClick(Sender:TObject);
    procedure FocusOn(aControl:TWinControl);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a pergunta / the question
    procedure ANewManagerDoesNotShowKeyboardsOnEnter;
    procedure FocusingAFieldAsksWhichKeyboardItWants;
    procedure TheQuestionCarriesTheFocusedField;
    procedure EveryFieldIsAskedOnItsOwn;

    //a troca de eventos / the swap of handlers
    procedure AFieldThatWantsNoKeyboardKeepsItsOwnHandlers;
    procedure AFieldThatWantsAKeyboardHasItsHandlersTakenOver;
    procedure LeavingTheFieldGivesTheHandlersBack;
    procedure TheOriginalHandlerStillRunsWhileTakenOver;

    //o que some / what goes away
    procedure ADestroyedFieldIsForgotten;
  end;

implementation

{ TTestKeyboardManager }

procedure TTestKeyboardManager.SetUp;
begin
  FAsked:=0;
  FLastAsked:=nil;
  FAnswer:=oskNone;
  FMyClicks:=0;

  FForm:=TForm.CreateNew(nil);
  FForm.SetBounds(0,0,300,200);
  FEdit1:=TEdit.Create(FForm);
  FEdit1.Parent:=FForm;
  FEdit1.SetBounds(10,10,100,24);
  FEdit1.OnClick:=@MyClick;
  FEdit2:=TEdit.Create(FForm);
  FEdit2.Parent:=FForm;
  FEdit2.SetBounds(10,50,100,24);
  FForm.Show;
  Application.ProcessMessages;

  FManager:=THMIKeyboardManager.Create(FForm);
  FManager.OnFocusChange:=@FocusChange;

  //o formulario mostrado ja' entrega o foco ao primeiro campo; parar no
  //segundo faz com que focar o primeiro, no teste, seja uma troca de verdade -
  //o gerenciador so' e' avisado quando o controle ativo muda.
  //showing the form already hands focus to the first field; parking on the
  //second makes focusing the first, in the test, a real change - the manager is
  //only told when the active control changes.
  FEdit2.SetFocus;
  Application.ProcessMessages;
  FAsked:=0;
  FLastAsked:=nil;
end;

procedure TTestKeyboardManager.TearDown;
begin
  FreeAndNil(FManager);
  FreeAndNil(FForm);
  Application.ProcessMessages;
end;

procedure TTestKeyboardManager.FocusChange(FocusedControl:TControl;
  var KeyboarTypeForControl:TOnScreenKeyboard;
  var NumericKBOptions:TNumericScreenKeyboardOptions;
  var AlphaNumKBOptions:TAlphaNumericScreenKeyBoardOptions;
  var ShowKeyboardNow:Boolean);
begin
  inc(FAsked);
  FLastAsked:=FocusedControl;
  KeyboarTypeForControl:=FAnswer;
  ShowKeyboardNow:=false;
end;

procedure TTestKeyboardManager.MyClick(Sender:TObject);
begin
  inc(FMyClicks);
end;

procedure TTestKeyboardManager.FocusOn(aControl:TWinControl);
begin
  aControl.SetFocus;
  Application.ProcessMessages;
end;

procedure TTestKeyboardManager.ANewManagerDoesNotShowKeyboardsOnEnter;
begin
  //por padrao o teclado so' aparece no clique; aparecer ao entrar no campo e'
  //escolha de quem monta a tela
  //by default the keyboard only shows on a click; showing it on entering the
  //field is a choice of whoever builds the screen
  AssertFalse('nao aparece ao entrar', FManager.ShowKeyboardOnEnter);
end;

procedure TTestKeyboardManager.FocusingAFieldAsksWhichKeyboardItWants;
begin
  FocusOn(FEdit1);

  AssertTrue('a aplicacao foi consultada', FAsked>0);
end;

procedure TTestKeyboardManager.TheQuestionCarriesTheFocusedField;
begin
  FocusOn(FEdit1);

  AssertSame('sobre o campo que ganhou o foco', FEdit1, FLastAsked);
end;

procedure TTestKeyboardManager.EveryFieldIsAskedOnItsOwn;
begin
  FocusOn(FEdit1);

  FocusOn(FEdit2);

  AssertSame('a ultima pergunta foi do segundo campo', FEdit2, FLastAsked);
end;

procedure TTestKeyboardManager.AFieldThatWantsNoKeyboardKeepsItsOwnHandlers;
begin
  //campo que nao quer teclado nao pode ter os eventos dele tomados
  //a field that wants no keyboard must not have its events taken over
  FAnswer:=oskNone;

  FocusOn(FEdit1);

  AssertTrue('o clique continua sendo o da tela', FEdit1.OnClick=@MyClick);
end;

procedure TTestKeyboardManager.AFieldThatWantsAKeyboardHasItsHandlersTakenOver;
begin
  //e' assim que o clique no campo abre o teclado
  //it is how clicking the field opens the keyboard
  FAnswer:=oskNumeric;

  FocusOn(FEdit1);

  AssertFalse('o clique passou a ser do gerenciador', FEdit1.OnClick=@MyClick);
  AssertTrue ('e existe',                             Assigned(FEdit1.OnClick));
end;

procedure TTestKeyboardManager.LeavingTheFieldGivesTheHandlersBack;
begin
  //o campo tem que sair da mao do gerenciador com o que era dele
  //the field has to leave the manager's hands with what belonged to it
  FAnswer:=oskNumeric;
  FocusOn(FEdit1);

  FocusOn(FEdit2);

  AssertTrue('o clique voltou a ser o da tela', FEdit1.OnClick=@MyClick);
end;

procedure TTestKeyboardManager.TheOriginalHandlerStillRunsWhileTakenOver;
begin
  //assumir o evento nao pode calar o que a tela ja' fazia no clique
  //taking the event over must not silence what the screen already did on the
  //click
  FAnswer:=oskNumeric;
  FocusOn(FEdit1);

  FEdit1.OnClick(FEdit1);

  AssertEquals('o clique da tela rodou', 1, FMyClicks);
end;

procedure TTestKeyboardManager.ADestroyedFieldIsForgotten;
begin
  //o gerenciador guarda o ultimo campo focado; se ele for destruido, o
  //ponteiro tem que sumir junto
  //the manager keeps the last focused field; if it is destroyed, the pointer
  //has to go with it
  FAnswer:=oskNumeric;
  FocusOn(FEdit1);

  FreeAndNil(FEdit1);
  Application.ProcessMessages;

  FocusOn(FEdit2);

  AssertSame('e o proximo campo e atendido normalmente', FEdit2, FLastAsked);
end;

initialization
  RegisterTest(TTestKeyboardManager);

end.
