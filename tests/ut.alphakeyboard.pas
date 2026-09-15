{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do teclado alfanumerico de tela.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' o irmao maior do teclado numerico: teclas de letra, de simbolo, de funcao
  e de navegacao, mais as modificadoras - Shift, Ctrl, Alt - que ficam
  pressionadas esperando a proxima tecla.

  Quem monta a tela escolhe quais grupos aparecem para cada campo: um campo de
  senha nao precisa das teclas de funcao, um campo de receita talvez precise
  dos numeros. Essa escolha vale tambem quando a mesma janela e' reaproveitada
  - e e' ai' que este teste bate.
}
{$ELSE}
{:
  @abstract(On-screen alphanumeric keyboard tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It is the numeric keyboard's bigger brother: letter, symbol, function and
  navigation keys, plus the modifiers - Shift, Ctrl, Alt - that stay pressed
  waiting for the next key.

  Whoever builds the screen chooses which groups show up for each field: a
  password field has no use for the function keys, a recipe field may need the
  numbers. That choice has to hold when the same window is reused as well - and
  that is where this suite hits.
}
{$ENDIF}
unit ut.alphakeyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Buttons, Forms, LCLType, fpcunit,
  testregistry, ualfakeyboard;

type

  { TTestAlphaKeyboard }

  TTestAlphaKeyboard = class(TTestCase)
  private
    FForm:TForm;
    FEdit:TEdit;
    FKeyboard:TpsHMIfrmAlphaKeyboard;
    function  NewKeyboard(showFxx, showSymbols, showNumbers, closeOnEnter:Boolean):TpsHMIfrmAlphaKeyboard;
    procedure Settle;
    function  WaitForText(const wanted:AnsiString):Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a janela / the window
    procedure TheKeyboardRemembersItsTarget;
    procedure AskingTwiceForTheSameFieldGivesTheSameKeyboard;

    //os grupos de teclas / the key groups
    procedure TheFunctionKeysCanBeLeftOut;
    procedure TheNumberKeysCanBeLeftOut;
    procedure AReusedKeyboardCanGetItsKeysBack;

    //o que cada tecla vale / what each key is worth
    procedure TheLetterKeysCarryTheirCodes;
    procedure TheCommandKeysCarryTheirCodes;

    //digitar / typing
    procedure PressingALetterTypesItIntoTheField;
  end;

implementation

{ TTestAlphaKeyboard }

procedure TTestAlphaKeyboard.SetUp;
begin
  FKeyboard:=nil;
  FForm:=TForm.CreateNew(nil);
  FForm.SetBounds(0,0,400,300);
  FEdit:=TEdit.Create(FForm);
  FEdit.Parent:=FForm;
  FEdit.SetBounds(10,10,200,24);
  FForm.Show;
  Application.ProcessMessages;
end;

procedure TTestAlphaKeyboard.TearDown;
begin
  if Assigned(FKeyboard) then begin
    FKeyboard.Close;
    Application.ProcessMessages;
    FKeyboard:=nil;
  end;
  FreeAndNil(FForm);
  Application.ProcessMessages;
end;

function TTestAlphaKeyboard.NewKeyboard(showFxx, showSymbols, showNumbers, closeOnEnter:Boolean):TpsHMIfrmAlphaKeyboard;
begin
  Result:=TpsHMIfrmAlphaKeyboard.CreateOrGetLast(FForm, FEdit,
            showFxx,      //ShowFxxKeys
            true,         //ShowTab
            true,         //ShowCaps
            true,         //ShowShift
            true,         //ShowCtrl
            true,         //ShowAlt
            showSymbols,  //ShowSymbols
            showNumbers,  //ShowNumbers
            true,         //ShowFastNavigation
            true,         //ShowNavigation
            closeOnEnter);
  FKeyboard:=Result;
end;

procedure TTestAlphaKeyboard.Settle;
begin
  Application.ProcessMessages;
end;

function TTestAlphaKeyboard.WaitForText(const wanted:AnsiString):Boolean;
var
  limite:QWord;
begin
  limite:=GetTickCount64+2000;
  while (FEdit.Text<>wanted) and (GetTickCount64<limite) do begin
    Application.ProcessMessages;
    Sleep(2);
  end;
  Result:=FEdit.Text=wanted;
end;

procedure TTestAlphaKeyboard.TheKeyboardRemembersItsTarget;
begin
  NewKeyboard(true, true, true, false);

  AssertSame('o campo alvo', FEdit, FKeyboard.Target);
end;

procedure TTestAlphaKeyboard.AskingTwiceForTheSameFieldGivesTheSameKeyboard;
begin
  //marca na primeira janela: comparar ponteiros nao serve, a liberada e a nova
  //caem no mesmo endereco
  //a mark on the first window: comparing pointers is no use, the freed one and
  //the new one land on the same address
  NewKeyboard(true, true, true, false);
  FKeyboard.Tag:=4242;

  NewKeyboard(true, true, true, false);

  AssertEquals('a mesma janela de antes', 4242, FKeyboard.Tag);
end;

procedure TTestAlphaKeyboard.TheFunctionKeysCanBeLeftOut;
begin
  //um campo de texto comum nao precisa de F1 a F12
  //an ordinary text field has no use for F1 to F12
  NewKeyboard(false, true, true, false);

  AssertFalse('F1 fora',  FKeyboard.Btn_F1.Enabled);
  AssertFalse('F12 fora', FKeyboard.Btn_F12.Enabled);
end;

procedure TTestAlphaKeyboard.TheNumberKeysCanBeLeftOut;
begin
  NewKeyboard(true, true, false, false);

  AssertFalse('os numeros ficaram fora', FKeyboard.Btn_5.Enabled);
end;

procedure TTestAlphaKeyboard.AReusedKeyboardCanGetItsKeysBack;
begin
  //o mesmo campo pode mudar de regra, e a janela reaproveitada tem que
  //acompanhar nos dois sentidos - tirar teclas e devolver teclas
  //the same field may change its rule, and the reused window has to follow both
  //ways - taking keys away and giving them back
  NewKeyboard(false, true, false, false);
  AssertFalse('sem funcao', FKeyboard.Btn_F1.Enabled);
  AssertFalse('sem numeros', FKeyboard.Btn_5.Enabled);

  NewKeyboard(true, true, true, false);

  AssertTrue('as de funcao voltaram', FKeyboard.Btn_F1.Enabled);
  AssertTrue('e os numeros tambem',   FKeyboard.Btn_5.Enabled);
end;

procedure TTestAlphaKeyboard.TheLetterKeysCarryTheirCodes;
begin
  NewKeyboard(true, true, true, false);

  AssertEquals('a', VK_A, FKeyboard.Btn_A.Tag);
  AssertEquals('m', VK_M, FKeyboard.Btn_M.Tag);
  AssertEquals('z', VK_Z, FKeyboard.Btn_Z.Tag);
end;

procedure TTestAlphaKeyboard.TheCommandKeysCarryTheirCodes;
begin
  NewKeyboard(true, true, true, false);

  AssertEquals('apaga',     VK_BACK,   FKeyboard.Btn_Back.Tag);
  AssertEquals('deleta',    VK_DELETE, FKeyboard.Btn_Del.Tag);
  AssertEquals('cancela',   VK_ESCAPE, FKeyboard.Btn_Esc.Tag);
  AssertEquals('esquerda',  VK_LEFT,   FKeyboard.Btn_Left.Tag);
  AssertEquals('inicio',    VK_HOME,   FKeyboard.Btn_Home.Tag);
  AssertEquals('fim',       VK_END,    FKeyboard.Btn_End.Tag);
end;

procedure TTestAlphaKeyboard.PressingALetterTypesItIntoTheField;
begin
  //o caminho inteiro, como no teclado numerico
  //the whole path, as on the numeric keyboard
  FEdit.SetFocus;
  Settle;
  NewKeyboard(true, true, true, false);

  FKeyboard.Btn_K.Click;

  AssertTrue('a letra chegou ao campo', WaitForText('k'));
end;

initialization
  RegisterTest(TTestAlphaKeyboard);

end.
