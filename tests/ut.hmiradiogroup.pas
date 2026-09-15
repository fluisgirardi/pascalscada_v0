{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIRadioGroup: o grupo de opcoes ligado a um tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Parado, Manual, Automatico. Cada opcao do grupo vale a sua posicao: a
  primeira e' zero, a segunda e' um. Diferente da lista de opcoes, aqui nao ha'
  numero proprio por linha - o que o CLP guarda e' a posicao.

  O tag manda: o valor que chega marca a opcao daquela posicao, e um valor que
  nao e' posicao nenhuma - negativo, alem da lista, ou tag nenhum - cai na
  opcao padrao. O operador manda: marcar uma opcao escreve a posicao dela.
}
{$ELSE}
{:
  @abstract(THMIRadioGroup tests: the group of options linked to a tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Stopped, Manual, Automatic. Each option in the group is worth its position:
  the first is zero, the second is one. Unlike the combo box, here there is no
  number of its own per line - what the PLC holds is the position.

  The tag rules: the value that arrives marks the option at that position, and
  a value that is no position at all - negative, beyond the list, or no tag -
  falls on the default option. The operator rules: marking an option writes
  its position.
}
{$ENDIF}
unit ut.hmiradiogroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Forms, fpcunit, testregistry,
  HMIRadioGroup, PLCTag, PLCString,
  ControlSecurityManager, CustomizedUserManagement, testsupport.faketag;

type

  { TRadioGroupProbe }

  //Nao ha' mouse: marcar um dos botoes do grupo e' o que o widget faz quando
  //o operador clica, e o grupo fica sabendo pelo OnClick do botao.
  //
  //There is no mouse: checking one of the group's buttons is what the widget
  //does when the operator clicks, and the group hears of it through the
  //button's OnClick.
  TRadioGroupProbe = class(THMIRadioGroup)
  public
    procedure Pick(aIndex:Integer);
  end;

  { TRefusingNumber }

  //Um tag cuja escrita nunca chega: guarda o valor que tinha e avisa a falha,
  //como faria um tag de verdade com o CLP fora do ar.
  //
  //A tag whose write never lands: it keeps the value it had and reports the
  //fault, as a real tag would with the PLC offline.
  TRefusingNumber = class(TFakeNumber)
  protected
    procedure SetValueRaw(aValue:Double); override;
  end;

  { TTestHMIRadioGroup }

  TTestHMIRadioGroup = class(TTestCase)
  private
    FGroup:TRadioGroupProbe;
    FTag:TFakeNumber;
    procedure ThreeModes;
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o tag escolhe a opcao / the tag picks the option
    procedure TheTagValuePicksTheOptionAtThatPosition;
    procedure AnotherValuePicksAnotherOption;
    procedure AFractionalValueIsTruncatedToAPosition;
    procedure AValueBeyondTheListFallsOnTheDefault;
    procedure ANegativeValueFallsOnTheDefault;
    procedure WithNoDefaultAnUnknownValueLeavesNothingSelected;
    procedure ARefreshFromTheTagWritesNothingBack;

    //a opcao padrao / the default option
    procedure ChangingTheDefaultAppliesAtOnce;
    procedure ADefaultBelowMinusOneBecomesMinusOne;

    //o operador escolhe / the operator picks
    procedure PickingAnOptionWritesItsPosition;
    procedure PickingAnotherOptionWritesTheNewPosition;
    procedure WithNoTagPickingWritesNothing;
    procedure ARefusedWritePutsTheTagValueBack;

    //o programa escolhe / the program picks
    procedure SettingItemIndexWritesToTheTag;
    procedure SettingItemIndexWithNoTagMovesTheSelection;

    //o tag / the tag
    procedure ATagThatIsNotNumericIsRefused;
    procedure ADestroyedTagLetsGoOfTheGroup;
    procedure WithNoTagTheFirstOptionIsNotSelected;
    procedure ClearingTheTagFallsOnTheDefault;
    procedure ADestroyedTagFallsOnTheDefault;

    //seguranca / security
    procedure WithoutPermissionTheGroupIsDisabled;
    procedure WithPermissionTheGroupIsEnabled;
    procedure PermissionDoesNotOverrideEnabledFalse;
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

{ TRadioGroupProbe }

procedure TRadioGroupProbe.Pick(aIndex:Integer);
begin
  Buttons[aIndex].Checked:=true;
end;

{ TRefusingNumber }

procedure TRefusingNumber.SetValueRaw(aValue:Double);
begin
  NotifyWriteFault;
end;

{ TTestHMIRadioGroup }

procedure TTestHMIRadioGroup.SetUp;
begin
  FGroup:=TRadioGroupProbe.Create(nil);
  FTag:=TFakeNumber.Create(nil);
end;

procedure TTestHMIRadioGroup.TearDown;
begin
  FreeAndNil(FGroup);
  FreeAndNil(FTag);
end;

procedure TTestHMIRadioGroup.ThreeModes;
begin
  FGroup.Items.Add('Parado');
  FGroup.Items.Add('Manual');
  FGroup.Items.Add('Automatico');
end;

procedure TTestHMIRadioGroup.Settle;
begin
  //a atualizacao do grupo e' agendada na fila da aplicacao
  //the group refresh is scheduled on the application queue
  Application.ProcessMessages;
end;

procedure TTestHMIRadioGroup.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestHMIRadioGroup.TheTagValuePicksTheOptionAtThatPosition;
begin
  ThreeModes;
  FGroup.PLCTag:=FTag;

  TagValueIs(1);

  AssertEquals('a segunda opcao', 1, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.AnotherValuePicksAnotherOption;
begin
  ThreeModes;
  FGroup.PLCTag:=FTag;
  TagValueIs(1);

  TagValueIs(2);

  AssertEquals('a terceira opcao', 2, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.AFractionalValueIsTruncatedToAPosition;
begin
  //um tag real pode carregar 1.7; a opcao e' a da parte inteira
  //a real tag may carry 1.7; the option is the one of the integer part
  ThreeModes;
  FGroup.PLCTag:=FTag;

  TagValueIs(1.7);

  AssertEquals('a segunda opcao', 1, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.AValueBeyondTheListFallsOnTheDefault;
begin
  ThreeModes;
  FGroup.DefaultIndex:=0;
  FGroup.PLCTag:=FTag;
  TagValueIs(2);

  TagValueIs(3);

  AssertEquals('a opcao padrao', 0, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.ANegativeValueFallsOnTheDefault;
begin
  ThreeModes;
  FGroup.DefaultIndex:=0;
  FGroup.PLCTag:=FTag;
  TagValueIs(2);

  TagValueIs(-1);

  AssertEquals('a opcao padrao', 0, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.WithNoDefaultAnUnknownValueLeavesNothingSelected;
begin
  //e' o mais honesto: o CLP esta' num modo que a tela nao conhece
  //it is the honest answer: the PLC is in a mode the screen does not know
  ThreeModes;
  FGroup.PLCTag:=FTag;
  TagValueIs(1);

  TagValueIs(99);

  AssertEquals('nenhuma opcao marcada', -1, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.ARefreshFromTheTagWritesNothingBack;
begin
  //mostrar o valor que chegou nao pode virar uma escrita de volta ao CLP
  //showing the value that arrived must not turn into a write back to the PLC
  ThreeModes;
  FGroup.PLCTag:=FTag;

  TagValueIs(1);
  TagValueIs(2);

  AssertEquals('nenhuma escrita', 0, FTag.Escritas);
end;

procedure TTestHMIRadioGroup.ChangingTheDefaultAppliesAtOnce;
begin
  ThreeModes;
  FGroup.PLCTag:=FTag;
  TagValueIs(99);
  AssertEquals('antes, nada marcado', -1, FGroup.ItemIndex);

  FGroup.DefaultIndex:=2;

  AssertEquals('a nova opcao padrao, sem esperar', 2, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.ADefaultBelowMinusOneBecomesMinusOne;
begin
  FGroup.DefaultIndex:=-7;

  AssertEquals('-1 e'' o menor que existe', -1, FGroup.DefaultIndex);
end;

procedure TTestHMIRadioGroup.PickingAnOptionWritesItsPosition;
begin
  ThreeModes;
  FGroup.PLCTag:=FTag;

  FGroup.Pick(2);

  AssertEquals('uma escrita',       1, FTag.Escritas);
  AssertEquals('com a posicao',     2, FTag.Value, 0.0001);
end;

procedure TTestHMIRadioGroup.PickingAnotherOptionWritesTheNewPosition;
begin
  ThreeModes;
  FGroup.PLCTag:=FTag;
  FGroup.Pick(2);
  Settle;

  FGroup.Pick(1);

  AssertEquals('duas escritas',     2, FTag.Escritas);
  AssertEquals('com a nova posicao',1, FTag.Value, 0.0001);
end;

procedure TTestHMIRadioGroup.WithNoTagPickingWritesNothing;
begin
  ThreeModes;

  FGroup.Pick(2);

  AssertEquals('a opcao marcou',     2, FGroup.ItemIndex);
  AssertEquals('e ninguem foi escrito', 0, FTag.Escritas);
end;

procedure TTestHMIRadioGroup.ARefusedWritePutsTheTagValueBack;
var
  teimoso:TRefusingNumber;
begin
  //a escrita falhou: a opcao marcada tem que voltar a ser a que o processo
  //diz, senao a tela mostra um modo em que o CLP nunca entrou
  //the write failed: the marked option has to go back to what the process
  //says, or the screen shows a mode the PLC never entered
  teimoso:=TRefusingNumber.Create(nil);
  try
    ThreeModes;
    FGroup.PLCTag:=teimoso;
    teimoso.ChegouDoCLP(0);
    Settle;

    FGroup.Pick(2);
    Settle;

    AssertEquals('de volta a opcao do tag', 0, FGroup.ItemIndex);
  finally
    FGroup.PLCTag:=nil;
    teimoso.Free;
  end;
end;

procedure TTestHMIRadioGroup.SettingItemIndexWritesToTheTag;
begin
  //o programa marcando uma opcao vale o mesmo que o operador marcando: a
  //posicao vai para o tag, uma vez so'
  //the program marking an option is worth the same as the operator marking it:
  //the position goes to the tag, once
  ThreeModes;
  FGroup.PLCTag:=FTag;

  FGroup.ItemIndex:=1;

  AssertEquals('uma escrita',   1, FTag.Escritas);
  AssertEquals('com a posicao', 1, FTag.Value, 0.0001);
  AssertEquals('e a opcao marcada', 1, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.SettingItemIndexWithNoTagMovesTheSelection;
begin
  ThreeModes;

  FGroup.ItemIndex:=1;

  AssertEquals('a opcao marcada', 1, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  FGroup.PLCTag:=FTag;
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FGroup.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FGroup.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestHMIRadioGroup.ADestroyedTagLetsGoOfTheGroup;
begin
  FGroup.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('o grupo soltou o tag', FGroup.PLCTag=nil);
end;

procedure TTestHMIRadioGroup.WithNoTagTheFirstOptionIsNotSelected;
begin
  //sem tag nao ha' leitura; a primeira opcao marcada diria que o processo
  //esta' "Parado" sem ninguem ter dito isso. Qualquer atualizacao do grupo -
  //aqui, mexer na opcao padrao - passa pelo mesmo caminho que o Loaded.
  //with no tag there is no reading; the first option marked would say the
  //process is "Stopped" with nobody having said so. Any refresh of the group -
  //here, touching the default option - goes down the same path Loaded does.
  ThreeModes;

  FGroup.DefaultIndex:=-1;
  Settle;

  AssertEquals('nenhuma opcao marcada', -1, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.ClearingTheTagFallsOnTheDefault;
begin
  ThreeModes;
  FGroup.DefaultIndex:=0;
  FGroup.PLCTag:=FTag;
  TagValueIs(2);
  AssertEquals('na opcao do tag', 2, FGroup.ItemIndex);

  FGroup.PLCTag:=nil;

  AssertEquals('sem tag, a opcao padrao', 0, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.ADestroyedTagFallsOnTheDefault;
begin
  ThreeModes;
  FGroup.DefaultIndex:=0;
  FGroup.PLCTag:=FTag;
  TagValueIs(2);

  FreeAndNil(FTag);

  AssertEquals('sem tag, a opcao padrao', 0, FGroup.ItemIndex);
end;

procedure TTestHMIRadioGroup.WithoutPermissionTheGroupIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='trocar_receita';

    FGroup.SecurityCode:='trocar_modo';

    AssertFalse('sem permissao, desabilitado', TControl(FGroup).Enabled);
  finally
    users.Free;
  end;
end;

procedure TTestHMIRadioGroup.WithPermissionTheGroupIsEnabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='trocar_modo';

    FGroup.SecurityCode:='trocar_modo';

    AssertTrue('com permissao, habilitado', TControl(FGroup).Enabled);
  finally
    users.Free;
  end;
end;

procedure TTestHMIRadioGroup.PermissionDoesNotOverrideEnabledFalse;
var
  users:TUserManagementForTest;
begin
  //o programa desabilitou; a permissao nao pode religar por cima
  //the program disabled it; the permission cannot switch it back on over that
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='trocar_modo';
    FGroup.Enabled:=false;

    FGroup.SecurityCode:='trocar_modo';

    AssertFalse('continua desabilitado', TControl(FGroup).Enabled);
  finally
    users.Free;
  end;
end;

initialization
  RegisterTest(TTestHMIRadioGroup);

end.
