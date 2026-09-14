{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIComboBox: a lista de opcoes ligada a um tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Uma lista de receitas, de modos de operacao, de velocidades. Cada linha da
  lista vale um numero no CLP - e esse numero nao e' necessariamente a posicao
  da linha: o item "Manual" pode valer 10 e o "Automatico" valer 20.

  Os dois sentidos importam. O tag manda: o valor que chega escolhe qual linha
  aparece selecionada, e nenhuma linha fica marcada se o valor nao corresponde
  a nenhuma. E o operador manda: escolher uma linha escreve o numero dela.
}
{$ELSE}
{:
  @abstract(THMIComboBox tests: the list of options linked to a tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A list of recipes, of operating modes, of speeds. Each line on the list is
  worth a number in the PLC - and that number is not necessarily the line's
  position: the "Manual" item may be worth 10 and "Automatic" 20.

  Both directions matter. The tag rules: the value that arrives picks which
  line shows as selected, and no line stays marked if the value matches none.
  And the operator rules: picking a line writes its number.
}
{$ENDIF}
unit ut.hmicombobox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, fpcunit, testregistry,
  HMIComboBox, PLCTag, PLCString,
  ControlSecurityManager, CustomizedUserManagement, testsupport.faketag;

type

  { TComboProbe }

  //Select e' protegido: e' o widget que o chama quando o operador escolhe uma
  //linha. A sonda encena a escolha sem precisar de mouse.
  //
  //Select is protected: the widget calls it when the operator picks a line.
  //The probe stages the choice with no mouse involved.
  TComboProbe = class(THMIComboBox)
  public
    procedure Pick(aIndex:Integer);
  end;

  { TTestHMIComboBox }

  TTestHMIComboBox = class(TTestCase)
  private
    FCombo:TComboProbe;
    FTag:TFakeNumber;
    FBlock:Boolean;
    FSent:Double;
    FSendCount:LongInt;
    procedure BeforeSend(Sender:TObject; Value:Double; var SendIt:Boolean);
    procedure AfterSend(Sender:TObject; Value:Double);
    procedure ThreeModes;
    procedure ThreePlainLines;
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o tag escolhe a linha / the tag picks the line
    procedure TheTagValuePicksTheLineThatCarriesIt;
    procedure AnotherValuePicksAnotherLine;
    procedure AValueThatMatchesNoLineLeavesNothingSelected;
    procedure WithPlainLinesTheValueIsTheLinePosition;
    procedure APositionOutsideTheListLeavesNothingSelected;

    //o operador escolhe / the operator picks
    procedure PickingALineWritesItsValue;
    procedure PickingAPlainLineWritesItsPosition;
    procedure TheBeforeEventCanRefuseTheWrite;
    procedure ARefusedChoiceGoesBackToTheTagValue;
    procedure TheAfterEventTellsWhatWasSent;
    procedure WithNoTagPickingWritesNothing;

    //o programa nao escolhe / the program does not pick
    procedure TheProgramCannotChangeTheSelection;

    //o tag / the tag
    procedure ATagThatIsNotNumericIsRefused;
    procedure ADestroyedTagLetsGoOfTheCombo;
    procedure ClearingTheTagLeavesNothingSelected;
    procedure ADestroyedTagLeavesNothingSelected;

    //a lista / the list
    procedure ReplacingTheListDoesNotLeakTheOldValues;

    //seguranca / security
    procedure WithoutPermissionTheComboIsDisabled;
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

{ TComboProbe }

procedure TComboProbe.Pick(aIndex:Integer);
begin
  //o widget move o indice e so' depois chama o Select
  //the widget moves the index and only then calls Select
  InternalSetItemIndex(aIndex);
  Select;
end;

{ TTestHMIComboBox }

procedure TTestHMIComboBox.SetUp;
begin
  FBlock:=false;
  FSent:=-1;
  FSendCount:=0;
  FCombo:=TComboProbe.Create(nil);
  FTag:=TFakeNumber.Create(nil);
end;

procedure TTestHMIComboBox.TearDown;
begin
  FreeAndNil(FCombo);
  FreeAndNil(FTag);
end;

procedure TTestHMIComboBox.BeforeSend(Sender:TObject; Value:Double; var SendIt:Boolean);
begin
  SendIt:=not FBlock;
end;

procedure TTestHMIComboBox.AfterSend(Sender:TObject; Value:Double);
begin
  inc(FSendCount);
  FSent:=Value;
end;

procedure TTestHMIComboBox.ThreeModes;
begin
  //tres linhas, cada uma valendo um numero que nao e' a sua posicao
  //three lines, each worth a number that is not its position
  FCombo.Items.AddObject('Parado',     TComboboxItemInfo.Create(nil, 10));
  FCombo.Items.AddObject('Manual',     TComboboxItemInfo.Create(nil, 20));
  FCombo.Items.AddObject('Automatico', TComboboxItemInfo.Create(nil, 30));
end;

procedure TTestHMIComboBox.ThreePlainLines;
begin
  //sem numero proprio: quem vale e' a posicao
  //with no number of their own: the position is what counts
  FCombo.Items.Add('zero');
  FCombo.Items.Add('um');
  FCombo.Items.Add('dois');
end;

procedure TTestHMIComboBox.Settle;
begin
  //a atualizacao da lista e' agendada na fila da aplicacao
  //the list refresh is scheduled on the application queue
  Application.ProcessMessages;
end;

procedure TTestHMIComboBox.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestHMIComboBox.TheTagValuePicksTheLineThatCarriesIt;
begin
  ThreeModes;
  FCombo.PLCTag:=FTag;

  TagValueIs(20);

  AssertEquals('a linha do valor 20', 1, FCombo.ItemIndex);
end;

procedure TTestHMIComboBox.AnotherValuePicksAnotherLine;
begin
  ThreeModes;
  FCombo.PLCTag:=FTag;
  TagValueIs(20);

  TagValueIs(30);

  AssertEquals('a linha do valor 30', 2, FCombo.ItemIndex);
end;

procedure TTestHMIComboBox.AValueThatMatchesNoLineLeavesNothingSelected;
begin
  //e' o mais honesto: o CLP esta' num modo que a tela nao conhece
  //it is the honest answer: the PLC is in a mode the screen does not know
  ThreeModes;
  FCombo.PLCTag:=FTag;
  TagValueIs(20);

  TagValueIs(99);

  AssertEquals('nenhuma linha marcada', -1, FCombo.ItemIndex);
end;

procedure TTestHMIComboBox.WithPlainLinesTheValueIsTheLinePosition;
begin
  ThreePlainLines;
  FCombo.PLCTag:=FTag;

  TagValueIs(2);

  AssertEquals('a terceira linha', 2, FCombo.ItemIndex);
end;

procedure TTestHMIComboBox.APositionOutsideTheListLeavesNothingSelected;
begin
  ThreePlainLines;
  FCombo.PLCTag:=FTag;
  TagValueIs(1);

  TagValueIs(7);

  AssertEquals('nenhuma linha marcada', -1, FCombo.ItemIndex);
end;

procedure TTestHMIComboBox.PickingALineWritesItsValue;
begin
  ThreeModes;
  FCombo.PLCTag:=FTag;

  FCombo.Pick(2);

  AssertEquals('escreveu o valor da linha', 30, FTag.Value, 0.0001);
end;

procedure TTestHMIComboBox.PickingAPlainLineWritesItsPosition;
begin
  ThreePlainLines;
  FCombo.PLCTag:=FTag;

  FCombo.Pick(1);

  AssertEquals('escreveu a posicao', 1, FTag.Value, 0.0001);
end;

procedure TTestHMIComboBox.TheBeforeEventCanRefuseTheWrite;
begin
  ThreeModes;
  FCombo.PLCTag:=FTag;
  TagValueIs(10);
  FCombo.BeforeSendAValueToTag:=@BeforeSend;
  FBlock:=true;

  FCombo.Pick(2);

  AssertEquals('o tag ficou como estava', 10, FTag.Value, 0.0001);
end;

procedure TTestHMIComboBox.ARefusedChoiceGoesBackToTheTagValue;
begin
  //recusada a escrita, a lista tem que voltar a mostrar o que o processo diz
  //once the write is refused, the list has to go back to showing what the
  //process says
  ThreeModes;
  FCombo.PLCTag:=FTag;
  TagValueIs(10);
  FCombo.BeforeSendAValueToTag:=@BeforeSend;
  FBlock:=true;

  FCombo.Pick(2);
  Settle;

  AssertEquals('de volta a linha do tag', 0, FCombo.ItemIndex);
end;

procedure TTestHMIComboBox.TheAfterEventTellsWhatWasSent;
begin
  ThreeModes;
  FCombo.PLCTag:=FTag;
  FCombo.BeforeSendAValueToTag:=@BeforeSend;
  FCombo.AfterSendValueToTag:=@AfterSend;

  FCombo.Pick(2);

  AssertEquals('avisou uma vez',  1,  FSendCount);
  AssertEquals('com o que foi',   30, FSent, 0.0001);
end;

procedure TTestHMIComboBox.WithNoTagPickingWritesNothing;
begin
  ThreeModes;
  FCombo.AfterSendValueToTag:=@AfterSend;

  FCombo.Pick(2);

  AssertEquals('nao mandou nada', 0, FSendCount);
end;

procedure TTestHMIComboBox.TheProgramCannotChangeTheSelection;
begin
  //quem manda na linha selecionada e' o tag; o programa mudar isso por fora
  //faria a tela mentir sobre o processo
  //the tag rules which line is selected; the program changing it from outside
  //would make the screen lie about the process
  ThreeModes;
  FCombo.PLCTag:=FTag;
  TagValueIs(10);

  TComboBox(FCombo).ItemIndex:=2;

  AssertEquals('continua na linha do tag', 0, FCombo.ItemIndex);
end;

procedure TTestHMIComboBox.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  FCombo.PLCTag:=FTag;
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FCombo.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FCombo.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestHMIComboBox.ADestroyedTagLetsGoOfTheCombo;
begin
  FCombo.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('a lista soltou o tag', FCombo.PLCTag=nil);
end;

procedure TTestHMIComboBox.ClearingTheTagLeavesNothingSelected;
begin
  //sem tag nao ha' leitura: a linha que ficou marcada continuaria parecendo o
  //modo em que o processo esta'
  //with no tag there is no reading: the line left marked would go on looking
  //like the mode the process is in
  ThreeModes;
  FCombo.PLCTag:=FTag;
  TagValueIs(20);
  AssertEquals('na linha do tag', 1, FCombo.ItemIndex);

  FCombo.PLCTag:=nil;

  AssertEquals('sem tag, nenhuma linha', -1, FCombo.ItemIndex);
end;

procedure TTestHMIComboBox.ADestroyedTagLeavesNothingSelected;
begin
  ThreeModes;
  FCombo.PLCTag:=FTag;
  TagValueIs(20);

  FreeAndNil(FTag);

  AssertEquals('sem tag, nenhuma linha', -1, FCombo.ItemIndex);
end;

procedure TTestHMIComboBox.ReplacingTheListDoesNotLeakTheOldValues;
var
  nova:TStringList;
begin
  //os numeros das linhas sao objetos que a lista carrega; trocar a lista tem
  //que levar os antigos junto
  //the lines' numbers are objects the list carries; replacing the list has to
  //take the old ones with it
  ThreeModes;

  nova:=TStringList.Create;
  try
    nova.AddObject('Ligado',   TComboboxItemInfo.Create(nil, 1));
    nova.AddObject('Desligado',TComboboxItemInfo.Create(nil, 0));
    FCombo.Items:=nova;
  finally
    nova.Free;
  end;

  AssertEquals('a lista nova', 2, FCombo.Items.Count);
end;

procedure TTestHMIComboBox.WithoutPermissionTheComboIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='trocar_receita';

    FCombo.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitada', TControl(FCombo).Enabled);
  finally
    users.Free;
  end;
end;

initialization
  RegisterTest(TTestHMIComboBox);

end.
