{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do teclado numerico de tela.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' a janelinha que aparece ao lado do campo num painel sem teclado fisico.
  Cada tecla desenhada carrega o codigo da tecla de verdade que ela representa,
  e apertar uma delas manda esse codigo ao campo alvo pelo caminho do sistema.

  A janela e' uma so': pedir teclado para o mesmo campo devolve a mesma, e
  pedir para outro campo fecha a anterior - duas janelas de teclado abertas ao
  mesmo tempo seriam duas fontes de digitacao sobre o mesmo processo.
}
{$ELSE}
{:
  @abstract(On-screen numeric keyboard tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It is the little window that shows up beside the field on a panel with no
  physical keyboard. Each drawn key carries the code of the real key it stands
  for, and pressing one sends that code to the target field through the
  system's own path.

  There is only ever one window: asking for a keyboard for the same field hands
  back the same one, and asking for another field closes the previous - two open
  keyboard windows would be two sources of typing over the same process.
}
{$ENDIF}
unit ut.numerickeyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, LCLType, fpcunit, testregistry,
  unumerickeyboard, ualfakeyboard;

type

  { TTestNumericKeyboard }

  TTestNumericKeyboard = class(TTestCase)
  private
    FForm:TForm;
    FEdit, FOutroEdit:TEdit;
    FKeyboard:TpsHMIfrmNumericKeyBoard;
    FClosed:LongInt;
    procedure KeyboardClosed(Sender:TObject; var CloseAction:TCloseAction);
    function  NewKeyboardFor(aTarget:TWinControl; showMinus, showDecimal:Boolean):TpsHMIfrmNumericKeyBoard;
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a criacao / creating it
    procedure ANilTargetIsRefused;
    procedure TheAlphanumericKeyboardAlsoRefusesANilTarget;
    procedure TheKeyboardRemembersItsTarget;
    procedure AskingTwiceForTheSameFieldGivesTheSameKeyboard;
    procedure AskingForAnotherFieldGivesAnotherKeyboard;

    //as teclas que aparecem / the keys that show up
    procedure TheMinusKeyShowsOnlyWhenAsked;
    procedure TheDecimalKeyShowsOnlyWhenAsked;
    procedure AskingAgainChangesWhichKeysShow;

    //o que cada tecla vale / what each key is worth
    procedure TheDigitKeysCarryTheirOwnCodes;
    procedure TheCommandKeysCarryTheirCodes;

    //digitar / typing
    procedure PressingADigitTypesItIntoTheField;
    procedure PressingSeveralDigitsBuildsTheNumber;

    //o alvo que some / the target that goes away
    procedure ADestroyedTargetClosesTheKeyboard;
  end;

implementation

{ TTestNumericKeyboard }

procedure TTestNumericKeyboard.KeyboardClosed(Sender:TObject; var CloseAction:TCloseAction);
begin
  inc(FClosed);
end;

procedure TTestNumericKeyboard.SetUp;
begin
  FKeyboard:=nil;
  FClosed:=0;
  FForm:=TForm.CreateNew(nil);
  FForm.SetBounds(0,0,300,200);
  FEdit:=TEdit.Create(FForm);
  FEdit.Parent:=FForm;
  FEdit.SetBounds(10,10,120,24);
  FOutroEdit:=TEdit.Create(FForm);
  FOutroEdit.Parent:=FForm;
  FOutroEdit.SetBounds(10,50,120,24);
  FForm.Show;
  Application.ProcessMessages;
end;

procedure TTestNumericKeyboard.TearDown;
begin
  //o teclado se destroi ao fechar (caFree); fechar antes do formulario evita
  //deixar janela viva entre um teste e outro
  //the keyboard frees itself on close (caFree); closing it before the form
  //keeps no window alive between one test and the next
  if Assigned(FKeyboard) then begin
    FKeyboard.Close;
    Application.ProcessMessages;
    FKeyboard:=nil;
  end;
  FreeAndNil(FForm);
  Application.ProcessMessages;
end;

function TTestNumericKeyboard.NewKeyboardFor(aTarget:TWinControl; showMinus, showDecimal:Boolean):TpsHMIfrmNumericKeyBoard;
begin
  Result:=TpsHMIfrmNumericKeyBoard.CreateOrGetLast(FForm, aTarget, showMinus, showDecimal);
  FKeyboard:=Result;
end;

procedure TTestNumericKeyboard.Settle;
begin
  Application.ProcessMessages;
end;

procedure TTestNumericKeyboard.ANilTargetIsRefused;
var
  kb:TpsHMIfrmNumericKeyBoard;
begin
  //teclado sem campo para digitar nao faz sentido, e a recusa tem que ser
  //limpa - nao um estouro no meio da construcao
  //a keyboard with no field to type into makes no sense, and the refusal has to
  //be clean - not a crash in the middle of the construction
  kb:=nil;
  try
    try
      kb:=TpsHMIfrmNumericKeyBoard.Create(FForm, nil, false, false);
      Fail('alvo nulo tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on E:Exception do
        AssertTrue('a recusa diz o motivo', Pos('target', LowerCase(E.Message))>0);
    end;
  finally
    kb.Free;
  end;
end;

procedure TTestNumericKeyboard.TheAlphanumericKeyboardAlsoRefusesANilTarget;
var
  kb:TpsHMIfrmAlphaKeyboard;
begin
  //o teclado alfanumerico e' gemeo do numerico e tinha o mesmo caminho de
  //recusa quebrado
  //the alphanumeric keyboard is the numeric one's twin and had the same broken
  //refusal path
  kb:=nil;
  try
    try
      kb:=TpsHMIfrmAlphaKeyboard.Create(FForm, nil,
                                        true, true, true, true, true, true,
                                        true, true, true, true, true);
      Fail('alvo nulo tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on E:Exception do
        AssertTrue('a recusa diz o motivo', Pos('target', LowerCase(E.Message))>0);
    end;
  finally
    kb.Free;
  end;
end;

procedure TTestNumericKeyboard.TheKeyboardRemembersItsTarget;
begin
  NewKeyboardFor(FEdit, false, false);

  AssertSame('o campo alvo', FEdit, FKeyboard.Target);
end;

procedure TTestNumericKeyboard.AskingTwiceForTheSameFieldGivesTheSameKeyboard;
begin
  //duas janelas de teclado abertas ao mesmo tempo seriam duas fontes de
  //digitacao sobre o mesmo processo.
  //
  //comparar ponteiros nao serve: a janela liberada e a nova caem no mesmo
  //endereco. Uma marca deixada na primeira e' o que distingue reaproveitar de
  //recriar.
  //two keyboard windows open at once would be two sources of typing over the
  //same process.
  //
  //comparing pointers is no use: the freed window and the new one land on the
  //same address. A mark left on the first is what tells reusing from
  //rebuilding apart.
  NewKeyboardFor(FEdit, false, false);
  FKeyboard.Tag:=4242;

  NewKeyboardFor(FEdit, false, false);

  AssertEquals('a mesma janela de antes', 4242, FKeyboard.Tag);
end;

procedure TTestNumericKeyboard.AskingForAnotherFieldGivesAnotherKeyboard;
begin
  //nao da' para comparar os ponteiros: a janela antiga e' liberada antes da
  //nova ser criada, e a nova costuma cair no mesmo endereco. O que se afirma e'
  //que a janela viva passou a ser a do campo novo.
  //the pointers cannot be compared: the old window is freed before the new one
  //is built, and the new one usually lands on the same address. What is
  //asserted is that the live window is now the new field's.
  NewKeyboardFor(FEdit, false, false);

  NewKeyboardFor(FOutroEdit, false, false);

  AssertSame('a janela viva e a do campo novo', FOutroEdit, FKeyboard.Target);
end;

procedure TTestNumericKeyboard.TheMinusKeyShowsOnlyWhenAsked;
begin
  //campo que nao aceita negativo nao mostra a tecla de menos
  //a field that takes no negative value does not show the minus key
  NewKeyboardFor(FEdit, false, false);
  AssertFalse('sem sinal', FKeyboard.Btn_Minus.Visible);

  FKeyboard.Close;
  Settle;

  NewKeyboardFor(FEdit, true, false);
  AssertTrue('com sinal', FKeyboard.Btn_Minus.Visible);
end;

procedure TTestNumericKeyboard.TheDecimalKeyShowsOnlyWhenAsked;
begin
  NewKeyboardFor(FEdit, false, false);
  AssertFalse('sem decimal', FKeyboard.Btn_DecSeparator.Visible);

  FKeyboard.Close;
  Settle;

  NewKeyboardFor(FEdit, false, true);
  AssertTrue('com decimal', FKeyboard.Btn_DecSeparator.Visible);
end;

procedure TTestNumericKeyboard.AskingAgainChangesWhichKeysShow;
begin
  //o mesmo campo pode mudar de regra - de inteiro para real, por exemplo - e a
  //janela reaproveitada tem que acompanhar
  //the same field may change its rule - from integer to real, say - and the
  //reused window has to follow
  NewKeyboardFor(FEdit, false, false);

  NewKeyboardFor(FEdit, true, true);

  AssertTrue('o sinal apareceu',   FKeyboard.Btn_Minus.Visible);
  AssertTrue('o decimal tambem',   FKeyboard.Btn_DecSeparator.Visible);
end;

procedure TTestNumericKeyboard.TheDigitKeysCarryTheirOwnCodes;
begin
  //e' o codigo da tecla de verdade que a tecla desenhada representa
  //it is the code of the real key the drawn key stands for
  NewKeyboardFor(FEdit, false, false);

  AssertEquals('zero',  VK_0, FKeyboard.Btn_0.Tag);
  AssertEquals('um',    VK_1, FKeyboard.Btn_1.Tag);
  AssertEquals('cinco', VK_5, FKeyboard.Btn_5.Tag);
  AssertEquals('nove',  VK_9, FKeyboard.Btn_9.Tag);
end;

procedure TTestNumericKeyboard.TheCommandKeysCarryTheirCodes;
begin
  NewKeyboardFor(FEdit, true, true);

  AssertEquals('confirma',  VK_RETURN, FKeyboard.Btn_Ok.Tag);
  AssertEquals('cancela',   VK_ESCAPE, FKeyboard.Btn_Esc.Tag);
  AssertEquals('apaga',     VK_BACK,   FKeyboard.Btn_Back.Tag);
  AssertEquals('deleta',    VK_DELETE, FKeyboard.Btn_Del.Tag);
  AssertEquals('esquerda',  VK_LEFT,   FKeyboard.Btn_Left.Tag);
  AssertEquals('direita',   VK_RIGHT,  FKeyboard.Btn_Rigth.Tag);
end;

procedure TTestNumericKeyboard.PressingADigitTypesItIntoTheField;
var
  limite:QWord;
begin
  //o caminho inteiro: a tecla desenhada vira evento do sistema e chega ao
  //campo
  //the whole path: the drawn key becomes a system event and reaches the field
  FEdit.SetFocus;
  Settle;
  NewKeyboardFor(FEdit, false, false);

  FKeyboard.Btn_7.Click;

  limite:=GetTickCount64+2000;
  while (FEdit.Text='') and (GetTickCount64<limite) do begin
    Application.ProcessMessages;
    Sleep(2);
  end;

  AssertEquals('o digito chegou ao campo', '7', FEdit.Text);
end;

procedure TTestNumericKeyboard.PressingSeveralDigitsBuildsTheNumber;
var
  limite:QWord;
begin
  FEdit.SetFocus;
  Settle;
  NewKeyboardFor(FEdit, false, false);

  FKeyboard.Btn_4.Click;
  FKeyboard.Btn_2.Click;

  limite:=GetTickCount64+2000;
  while (Length(FEdit.Text)<2) and (GetTickCount64<limite) do begin
    Application.ProcessMessages;
    Sleep(2);
  end;

  AssertEquals('os dois digitos, na ordem', '42', FEdit.Text);
end;

procedure TTestNumericKeyboard.ADestroyedTargetClosesTheKeyboard;
begin
  //sem campo alvo o teclado nao tem para onde digitar. A janela se fecha e se
  //destroi (caFree), entao nao da' para le-la depois: o que se observa e' o
  //aviso de fechamento.
  //with no target field the keyboard has nowhere to type. The window closes and
  //frees itself (caFree), so it cannot be read afterwards: what is observed is
  //the close notification.
  NewKeyboardFor(FOutroEdit, false, false);
  FKeyboard.OnClose:=@KeyboardClosed;

  FreeAndNil(FOutroEdit);
  Settle;

  AssertEquals('a janela se fechou', 1, FClosed);
  FKeyboard:=nil;
end;

initialization
  RegisterTest(TTestNumericKeyboard);

end.
