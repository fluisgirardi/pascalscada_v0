{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIEdit: a caixa em que o operador digita o valor que
            vai para o CLP.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' o controle de entrada mais perigoso da biblioteca: qualquer coisa que o
  operador digitar pode virar um valor escrito no processo. Por isso ele tem
  tres linhas de defesa - o que o tag aceita (IsValidValue) enquanto se
  digita, os limites de minimo e maximo na hora de enviar, e o evento
  BeforeSendAValueToTag para a aplicacao dar a ultima palavra.

  E tem a outra metade: o que vem do CLP tem que aparecer formatado, com
  prefixo e sufixo, mas nao pode sobrescrever o que o operador esta' digitando
  no exato momento em que ele digita.
}
{$ELSE}
{:
  @abstract(THMIEdit tests: the box where the operator types the value that
            goes to the PLC.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It is the most dangerous input control in the library: anything the operator
  types can become a value written into the process. That is why it has three
  lines of defence - what the tag accepts (IsValidValue) while typing, the
  minimum and maximum limits at send time, and the BeforeSendAValueToTag event
  for the application to have the last word.

  And there is the other half: what comes from the PLC has to show up
  formatted, with prefix and suffix, but must not overwrite what the operator
  is typing at the very moment they type it.
}
{$ENDIF}
unit ut.hmiedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, LCLType, fpcunit, testregistry,
  HMIEdit, HMITypes, PLCTag, PLCString,
  ControlSecurityManager, CustomizedUserManagement, testsupport.faketag;

type

  { TEditProbe }

  //sem janela de verdade os caminhos do widget - foco, tecla, mudanca de
  //texto - nao acontecem sozinhos. A sonda chama cada um deles pelo nome.
  //
  //with no real window the widget paths - focus, key, text change - do not
  //happen on their own. The probe calls each of them by name.
  TEditProbe = class(THMIEdit)
  public
    procedure TypeText(const t:AnsiString);
    procedure PressEnter;
    procedure PressEsc;
    procedure EnterTheField;
    procedure LeaveTheField;
  end;

  { TTestHMIEdit }

  TTestHMIEdit = class(TTestCase)
  private
    FForm:TForm;
    FEdit:TEditProbe;
    FTag:TFakeNumber;
    FBlock:Boolean;
    FSent:AnsiString;
    FSendCount:LongInt;
    FAskCount:LongInt;
    procedure BeforeSend(Sender:TObject; Value:TTranslateString; var SendIt:Boolean);
    procedure AfterSend(Sender:TObject; Value:TTranslateString);
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //criado por codigo / created by code
    procedure EnablingARuntimeCreatedControlKeepsItEnabled;
    //o que vem do CLP / what comes from the PLC
    procedure TheTagValueShowsUpFormatted;
    procedure TheFormatChangesHowItIsShown;
    procedure ThePrefixAndTheSufixDressTheValue;
    procedure ANewValueFromThePLCIsShown;

    //o que o operador digita / what the operator types
    procedure WhatWasTypedIsNotOverwrittenByThePLC;
    procedure LeavingTheFieldShowsThePLCValueAgain;
    procedure WhileFocusedTheValueIsFrozenByDefault;
    procedure WithoutFreezingTheFocusedFieldFollowsThePLC;

    //o envio / the send
    procedure PressingEnterSendsWhatWasTyped;
    procedure PressingEnterWithoutTheSettingDoesNotSend;
    procedure LeavingTheFieldSendsWhatWasTyped;
    procedure TextThatIsNotANumberIsRefusedWhileTyping;
    procedure AValueAboveTheMaximumIsRefused;
    procedure AValueBelowTheMinimumIsRefused;
    procedure WithTheLimitsOffAnyValueGoesThrough;
    procedure ARefusedValueDoesNotFreezeTheBoxOnEnter;
    procedure ARefusedValueDoesNotFreezeTheBoxOnExit;

    //os avisos / the notifications
    procedure TheBeforeEventIsNotAskedAboutAValueOutOfRange;
    procedure TheBeforeEventCanRefuseTheWrite;
    procedure TheAfterEventTellsWhatWasSent;

    //envio a cada tecla / send on every keystroke
    procedure SendingOnEveryChangeWritesWhatIsTyped;
    procedure ANumberBeingTypedDoesNotRaiseWhilePassingOutOfRange;

    //os limites / the limits
    procedure TheMinimumMustBeLessThanTheMaximum;
    procedure TheMaximumMustBeGreaterThanTheMinimum;

    //o tag / the tag
    procedure ADestroyedTagLetsGoOfTheEdit;
    procedure ClearingTheTagEmptiesTheBox;
    procedure ADestroyedTagEmptiesTheBox;
    procedure WithNoTagNothingIsSent;

    //seguranca / security
    procedure WithoutPermissionTheEditIsDisabled;
    procedure PermissionDoesNotOverrideTheProgramsEnabled;
  end;

implementation

type

  { TUserManagementForTest }

  TUserManagementForTest = class(TCustomizedUserManagement)
  private
    FGranted:UTF8String;
  protected
    function CanAccess(sc:UTF8String):Boolean; override;
  public
    property Granted:UTF8String read FGranted write FGranted;
  end;

function TUserManagementForTest.CanAccess(sc:UTF8String):Boolean;
begin
  Result:=(Trim(sc)='') or (sc=FGranted);
end;

{ TEditProbe }

procedure TEditProbe.TypeText(const t:AnsiString);
begin
  TEdit(Self).Text:=t;
  Modified:=true;
  Change;
end;

procedure TEditProbe.PressEnter;
var
  k:Word;
begin
  k:=VK_RETURN;
  KeyDown(k, []);
end;

procedure TEditProbe.PressEsc;
var
  k:Word;
begin
  k:=VK_ESCAPE;
  KeyDown(k, []);
end;

procedure TEditProbe.EnterTheField;
begin
  DoEnter;
end;

procedure TEditProbe.LeaveTheField;
begin
  DoExit;
end;

{ TTestHMIEdit }

procedure TTestHMIEdit.SetUp;
begin
  FBlock:=false;
  FSent:='';
  FSendCount:=0;
  FAskCount:=0;
  //um formulario de verdade: o Edit precisa de janela para o texto, o foco e
  //a marca de modificado se comportarem como em producao
  //a real form: the Edit needs a window for the text, the focus and the
  //modified mark to behave as they do in production
  FForm:=TForm.CreateNew(nil);
  FForm.Visible:=false;
  FEdit:=TEditProbe.Create(FForm);
  FEdit.Parent:=FForm;
  FTag:=TFakeNumber.Create(nil);
  FEdit.PLCTag:=FTag;
end;

procedure TTestHMIEdit.TearDown;
begin
  FreeAndNil(FForm);
  FreeAndNil(FTag);
end;

procedure TTestHMIEdit.BeforeSend(Sender:TObject; Value:TTranslateString; var SendIt:Boolean);
begin
  inc(FAskCount);
  SendIt:=not FBlock;
end;

procedure TTestHMIEdit.AfterSend(Sender:TObject; Value:TTranslateString);
begin
  inc(FSendCount);
  FSent:=Value;
end;

procedure TTestHMIEdit.Settle;
begin
  //a atualizacao do texto e' agendada na fila da aplicacao
  //the text refresh is scheduled on the application queue
  Application.ProcessMessages;
end;

procedure TTestHMIEdit.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestHMIEdit.EnablingARuntimeCreatedControlKeepsItEnabled;
begin
  //sem .lfm e sem codigo de seguranca: desabilitar e reabilitar tem que
  //deixa-lo habilitado. A flag de seguranca nascia falsa, e o E logico
  //com ela desabilitava o controle no primeiro Enabled:=true.
  //with no .lfm and no security code: disabling and re-enabling has to
  //leave it enabled. The security flag was born false, and the logical
  //AND with it disabled the control on the first Enabled:=true.
  FEdit.Enabled:=false;

  FEdit.Enabled:=true;

  AssertTrue('habilitado', TControl(FEdit).Enabled);
end;

procedure TTestHMIEdit.TheTagValueShowsUpFormatted;
begin
  TagValueIs(42);

  AssertEquals('o valor com o formato padrao', '42.0', FEdit.Text);
end;

procedure TTestHMIEdit.TheFormatChangesHowItIsShown;
begin
  FEdit.NumberFormat:='#0.000';

  TagValueIs(42);

  AssertEquals('tres casas', '42.000', FEdit.Text);
end;

procedure TTestHMIEdit.ThePrefixAndTheSufixDressTheValue;
begin
  FEdit.Prefix:='R$ ';
  FEdit.Sufix:=' /kg';

  TagValueIs(42);

  AssertEquals('vestido', 'R$ 42.0 /kg', FEdit.Text);
end;

procedure TTestHMIEdit.ANewValueFromThePLCIsShown;
begin
  TagValueIs(42);

  TagValueIs(7);

  AssertEquals('acompanhou', '7.0', FEdit.Text);
end;

procedure TTestHMIEdit.WhatWasTypedIsNotOverwrittenByThePLC;
begin
  //o operador esta' digitando: o CLP nao pode apagar o que ele escreveu.
  //Sem congelamento, para a marca de modificado ser a unica coisa segurando
  //a leitura - que e' o que este teste mede.
  //the operator is typing: the PLC must not wipe what they wrote. With
  //freezing off, so the modified mark is the only thing holding the refresh
  //back - which is what this test measures.
  FEdit.FreezeValueOnFocus:=false;
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('99');

  TagValueIs(7);

  AssertEquals('o que foi digitado continua la', '99', FEdit.Text);
end;

procedure TTestHMIEdit.LeavingTheFieldShowsThePLCValueAgain;
begin
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('99');

  FEdit.LeaveTheField;
  Settle;

  AssertEquals('voltou a mostrar o tag', '99.0', FEdit.Text);
end;

procedure TTestHMIEdit.WhileFocusedTheValueIsFrozenByDefault;
begin
  //com o campo em foco, o valor mostrado congela na primeira leitura para o
  //numero nao dancar embaixo do cursor
  //with the field focused, the shown value freezes on the first reading so
  //the number does not dance under the cursor
  TagValueIs(42);
  FEdit.EnterTheField;

  TagValueIs(7);

  AssertEquals('congelado', '42.0', FEdit.Text);
end;

procedure TTestHMIEdit.WithoutFreezingTheFocusedFieldFollowsThePLC;
begin
  FEdit.FreezeValueOnFocus:=false;
  TagValueIs(42);
  FEdit.EnterTheField;

  TagValueIs(7);

  AssertEquals('acompanhou mesmo em foco', '7.0', FEdit.Text);
end;

procedure TTestHMIEdit.PressingEnterSendsWhatWasTyped;
begin
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('99');

  FEdit.PressEnter;

  AssertEquals('o tag recebeu', 99, FTag.Value, 0.0001);
end;

procedure TTestHMIEdit.PressingEnterWithoutTheSettingDoesNotSend;
begin
  FEdit.SendValueWhen:=[scLostFocus];
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('99');

  FEdit.PressEnter;

  AssertEquals('o tag ficou como estava', 42, FTag.Value, 0.0001);
end;

procedure TTestHMIEdit.LeavingTheFieldSendsWhatWasTyped;
begin
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('99');

  FEdit.LeaveTheField;

  AssertEquals('o tag recebeu', 99, FTag.Value, 0.0001);
end;

procedure TTestHMIEdit.TextThatIsNotANumberIsRefusedWhileTyping;
begin
  //a primeira linha de defesa e' o proprio tag: o que ele nao aceita como
  //valor e' desfeito na hora em que se digita, e nunca chega a ser enviado
  //the first line of defence is the tag itself: what it does not accept as a
  //value is undone as it is typed, and never gets sent at all
  TagValueIs(42);
  FEdit.EnterTheField;

  FEdit.TypeText('abc');

  AssertEquals('o texto voltou ao ultimo valor bom', '42.0', FEdit.Text);
end;

procedure TTestHMIEdit.AValueAboveTheMaximumIsRefused;
begin
  FEdit.MaxValue:=100;
  FEdit.EnableMaxValue:=true;
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('150');

  //a recusa e' por excecao: o operador tem que ver o erro, nao um valor que
  //sumiu sem explicacao
  //the refusal comes as an exception: the operator has to see the error, not
  //a value that vanished with no explanation
  try
    FEdit.PressEnter;
    Fail('valor acima do maximo tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('o tag ficou como estava', 42, FTag.Value, 0.0001);
end;

procedure TTestHMIEdit.AValueBelowTheMinimumIsRefused;
begin
  FEdit.MaxValue:=100;
  FEdit.MinValue:=10;
  FEdit.EnableMinValue:=true;
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('5');

  //a recusa e' por excecao: o operador tem que ver o erro, nao um valor que
  //sumiu sem explicacao
  //the refusal comes as an exception: the operator has to see the error, not
  //a value that vanished with no explanation
  try
    FEdit.PressEnter;
    Fail('valor abaixo do minimo tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('o tag ficou como estava', 42, FTag.Value, 0.0001);
end;

procedure TTestHMIEdit.WithTheLimitsOffAnyValueGoesThrough;
begin
  FEdit.MaxValue:=100;
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('150');

  FEdit.PressEnter;

  AssertEquals('o limite desligado nao segura', 150, FTag.Value, 0.0001);
end;

procedure TTestHMIEdit.ARefusedValueDoesNotFreezeTheBoxOnEnter;
begin
  //a recusa no Enter, com o campo ainda em foco: se a marca de modificado
  //ficar de pe', a caixa para de acompanhar o processo ali mesmo
  //the refusal on Enter, with the field still focused: if the modified mark
  //stays up, the box stops following the process right there
  FEdit.FreezeValueOnFocus:=false;
  FEdit.MaxValue:=100;
  FEdit.EnableMaxValue:=true;
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('150');

  try
    FEdit.PressEnter;
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  TagValueIs(7);

  AssertEquals('voltou a mostrar o CLP', '7.0', FEdit.Text);
end;

procedure TTestHMIEdit.ARefusedValueDoesNotFreezeTheBoxOnExit;
begin
  //o outro caminho de envio: sair do campo sem teclar Enter. A recusa nao
  //pode levar embora a volta da caixa ao valor do processo.
  //the other send path: leaving the field without pressing Enter. The refusal
  //must not take away the box going back to the process value.
  FEdit.MaxValue:=100;
  FEdit.EnableMaxValue:=true;
  TagValueIs(42);
  FEdit.EnterTheField;
  FEdit.TypeText('150');

  try
    FEdit.LeaveTheField;
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  TagValueIs(7);

  AssertEquals('voltou a mostrar o CLP', '7.0', FEdit.Text);
end;

procedure TTestHMIEdit.TheBeforeEventIsNotAskedAboutAValueOutOfRange;
begin
  FEdit.MaxValue:=100;
  FEdit.EnableMaxValue:=true;
  TagValueIs(42);
  FEdit.BeforeSendAValueToTag:=@BeforeSend;
  FEdit.EnterTheField;
  FEdit.TypeText('150');

  try
    FEdit.PressEnter;
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('nao perguntou nada', 0, FAskCount);
end;

procedure TTestHMIEdit.SendingOnEveryChangeWritesWhatIsTyped;
begin
  FEdit.SendValueWhen:=[scAnyChange];
  TagValueIs(42);
  FEdit.EnterTheField;

  FEdit.TypeText('99');

  AssertEquals('foi na hora', 99, FTag.Value, 0.0001);
end;

procedure TTestHMIEdit.ANumberBeingTypedDoesNotRaiseWhilePassingOutOfRange;
begin
  //digitando "150" num campo de ate' 100, o numero passa por 1 e 15 antes de
  //chegar a 150; nenhum desses passos pode abrir caixa de erro na cara do
  //operador
  //typing "150" on a field capped at 100, the number goes through 1 and 15
  //before reaching 150; none of those steps may throw an error box at the
  //operator
  FEdit.SendValueWhen:=[scAnyChange];
  FEdit.MaxValue:=100;
  FEdit.MinValue:=50;
  FEdit.EnableMaxValue:=true;
  FEdit.EnableMinValue:=true;
  TagValueIs(60);
  FEdit.EnterTheField;

  FEdit.TypeText('1');
  FEdit.TypeText('15');
  FEdit.TypeText('150');

  AssertEquals('nada fora da faixa foi escrito', 60, FTag.Value, 0.0001);

  FEdit.TypeText('75');

  AssertEquals('e o valor valido foi', 75, FTag.Value, 0.0001);
end;

procedure TTestHMIEdit.TheBeforeEventCanRefuseTheWrite;
begin
  TagValueIs(42);
  FEdit.BeforeSendAValueToTag:=@BeforeSend;
  FBlock:=true;
  FEdit.EnterTheField;
  FEdit.TypeText('99');

  FEdit.PressEnter;

  AssertEquals('o tag ficou como estava', 42, FTag.Value, 0.0001);
end;

procedure TTestHMIEdit.TheAfterEventTellsWhatWasSent;
begin
  TagValueIs(42);
  FEdit.BeforeSendAValueToTag:=@BeforeSend;
  FEdit.AfterSendValueToTag:=@AfterSend;
  FEdit.EnterTheField;
  FEdit.TypeText('99');

  FEdit.PressEnter;

  AssertEquals('avisou uma vez',   1,    FSendCount);
  AssertEquals('com o que foi',    '99', FSent);
end;

procedure TTestHMIEdit.TheMinimumMustBeLessThanTheMaximum;
begin
  FEdit.MaxValue:=100;

  try
    FEdit.MinValue:=150;
    Fail('minimo acima do maximo tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('o minimo continua onde estava', 0, FEdit.MinValue, 0.0001);
end;

procedure TTestHMIEdit.TheMaximumMustBeGreaterThanTheMinimum;
begin
  FEdit.MaxValue:=100;
  FEdit.MinValue:=10;

  try
    FEdit.MaxValue:=5;
    Fail('maximo abaixo do minimo tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('o maximo continua onde estava', 100, FEdit.MaxValue, 0.0001);
end;

procedure TTestHMIEdit.ClearingTheTagEmptiesTheBox;
begin
  TagValueIs(42);
  AssertEquals('mostrando o valor', '42.0', FEdit.Text);

  FEdit.PLCTag:=nil;

  AssertEquals('sem tag, sem valor', '', FEdit.Text);
end;

procedure TTestHMIEdit.ADestroyedTagEmptiesTheBox;
begin
  TagValueIs(42);

  FreeAndNil(FTag);

  AssertEquals('sem tag, sem valor', '', FEdit.Text);
end;

procedure TTestHMIEdit.ADestroyedTagLetsGoOfTheEdit;
begin
  FreeAndNil(FTag);

  AssertTrue('a caixa soltou o tag', FEdit.PLCTag=nil);
end;

procedure TTestHMIEdit.WithNoTagNothingIsSent;
begin
  FEdit.PLCTag:=nil;
  FEdit.AfterSendValueToTag:=@AfterSend;
  FEdit.EnterTheField;
  FEdit.TypeText('99');

  FEdit.PressEnter;

  AssertEquals('nao mandou nada', 0, FSendCount);
end;

procedure TTestHMIEdit.WithoutPermissionTheEditIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='digitar_receita';

    FEdit.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitada', TControl(FEdit).Enabled);
  finally
    users.Free;
  end;
end;

procedure TTestHMIEdit.PermissionDoesNotOverrideTheProgramsEnabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='digitar_receita';
    FEdit.SecurityCode:='digitar_receita';

    FEdit.Enabled:=false;

    AssertFalse('o programa desabilitou', TControl(FEdit).Enabled);
  finally
    users.Free;
  end;
end;

initialization
  RegisterTest(TTestHMIEdit);

end.
