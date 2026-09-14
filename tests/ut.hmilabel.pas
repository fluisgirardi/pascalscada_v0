{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMILabel: o valor de um tag virando texto na tela.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' o controle mais usado de uma tela de supervisorio e o unico que so le:
  pega o valor do tag, passa pelo formato, pelo prefixo e pelo sufixo, e
  entrega uma legenda. Nao tem caminho de escrita, nao tem clique - o que se
  testa e' a legenda que sai, e ela e' legivel de fora.

  A formatacao em si e' do tag; o que o rotulo acrescenta e' quando refazer a
  legenda, o que mostrar quando nao ha tag, e o controle de acesso.
}
{$ELSE}
{:
  @abstract(THMILabel tests: a tag's value turning into text on screen.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It is the most used control on a supervisory screen and the only one that
  merely reads: it takes the tag's value, runs it through the format, the
  prefix and the suffix, and hands over a caption. No write path, no click -
  what is tested is the caption that comes out, and it is readable from
  outside.

  The formatting itself belongs to the tag; what the label adds is when to
  rebuild the caption, what to show with no tag, and access control.
}
{$ENDIF}
unit ut.hmilabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, fpcunit, testregistry,
  HMILabel, PLCTag, ControlSecurityManager, CustomizedUserManagement,
  testsupport.faketag;

type

  { TTestHMILabel }

  TTestHMILabel = class(TTestCase)
  private
    FLabel:THMILabel;
    FTag:TFakeNumber;
    procedure TagValueIs(v:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a legenda / the caption
    procedure WithNoTagTheCaptionIsEmpty;
    procedure TheCaptionShowsTheTagValue;
    procedure TheCaptionFollowsTheTagValue;
    procedure ThePrefixComesBeforeTheValue;
    procedure TheSuffixComesAfterTheValue;
    procedure WithNoFormatTheValueComesWithOneDecimal;
    procedure TheNumberFormatShapesTheValue;
    procedure PrefixFormatAndSuffixComeTogether;

    //quando a legenda e refeita / when the caption is rebuilt
    procedure ChangingThePrefixRebuildsTheCaptionAtOnce;
    procedure ChangingTheFormatRebuildsTheCaptionAtOnce;

    //o tag / the tag
    procedure ATagThatIsNotReadableIsRefused;
    procedure ChangingTagsFollowsTheNewOne;
    procedure ADestroyedTagLetsGoOfTheLabel;

    //seguranca / security
    procedure ANewLabelIsEnabled;
    procedure WithoutPermissionTheLabelIsDisabled;
    procedure TheEnabledPropertyAnswersForTheProgramNotForTheControl;
  end;

implementation

uses HMITypes;

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

{ TTestHMILabel }

procedure TTestHMILabel.SetUp;
begin
  FLabel:=THMILabel.Create(nil);
  FTag:=TFakeNumber.Create(nil);
end;

procedure TTestHMILabel.TearDown;
begin
  FreeAndNil(FLabel);
  FreeAndNil(FTag);
end;

procedure TTestHMILabel.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  //o aviso do tag agenda a atualizacao na fila da aplicacao
  Application.ProcessMessages;
end;

procedure TTestHMILabel.WithNoTagTheCaptionIsEmpty;
begin
  //em tempo de execucao um rotulo sem tag nao inventa texto
  AssertEquals('legenda vazia', '', FLabel.Caption);
end;

procedure TTestHMILabel.TheCaptionShowsTheTagValue;
begin
  FLabel.PLCTag:=FTag;

  TagValueIs(42);

  AssertEquals('valor na legenda', '42.0', FLabel.Caption);
end;

procedure TTestHMILabel.TheCaptionFollowsTheTagValue;
begin
  FLabel.PLCTag:=FTag;

  TagValueIs(10);
  AssertEquals('primeiro valor', '10.0', FLabel.Caption);

  TagValueIs(20);
  AssertEquals('segundo valor',  '20.0', FLabel.Caption);
end;

procedure TTestHMILabel.ThePrefixComesBeforeTheValue;
begin
  FLabel.PLCTag:=FTag;
  FLabel.Prefix:='nivel: ';

  TagValueIs(42);

  AssertEquals('prefixo antes', 'nivel: 42.0', FLabel.Caption);
end;

procedure TTestHMILabel.TheSuffixComesAfterTheValue;
begin
  FLabel.PLCTag:=FTag;
  FLabel.Sufix:=' m';

  TagValueIs(42);

  AssertEquals('sufixo depois', '42.0 m', FLabel.Caption);
end;

procedure TTestHMILabel.WithNoFormatTheValueComesWithOneDecimal;
begin
  //quem nao escolhe formato recebe uma casa decimal, nao o inteiro cru
  FLabel.PLCTag:=FTag;

  TagValueIs(42);

  AssertEquals('uma casa decimal', '42.0', FLabel.Caption);
end;

procedure TTestHMILabel.TheNumberFormatShapesTheValue;
begin
  FLabel.PLCTag:=FTag;
  FLabel.NumberFormat:='0.00';

  TagValueIs(42.5);

  AssertEquals('duas casas', '42.50', FLabel.Caption);
end;

procedure TTestHMILabel.PrefixFormatAndSuffixComeTogether;
begin
  FLabel.PLCTag:=FTag;
  FLabel.Prefix:='nivel: ';
  FLabel.NumberFormat:='0.0';
  FLabel.Sufix:=' m';

  TagValueIs(3.75);

  AssertEquals('tudo junto', 'nivel: 3.8 m', FLabel.Caption);
end;

procedure TTestHMILabel.ChangingThePrefixRebuildsTheCaptionAtOnce;
begin
  //mexer no prefixo nao pode esperar a proxima varredura para aparecer
  FLabel.PLCTag:=FTag;
  TagValueIs(42);

  FLabel.Prefix:='nivel: ';

  AssertEquals('legenda refeita na hora', 'nivel: 42.0', FLabel.Caption);
end;

procedure TTestHMILabel.ChangingTheFormatRebuildsTheCaptionAtOnce;
begin
  FLabel.PLCTag:=FTag;
  TagValueIs(42);

  FLabel.NumberFormat:='0.00';

  AssertEquals('legenda refeita na hora', '42.00', FLabel.Caption);
end;

procedure TTestHMILabel.ATagThatIsNotReadableIsRefused;
var
  semLeitura:TComponent;
begin
  //o rotulo precisa de algo que saiba se descrever em texto
  semLeitura:=TComponent.Create(nil);
  try
    try
      FLabel.PLCTag:=TPLCTag(semLeitura);
      Fail('um componente que nao e tag tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;
  finally
    semLeitura.Free;
  end;
end;

procedure TTestHMILabel.ChangingTagsFollowsTheNewOne;
var
  outro:TFakeNumber;
begin
  outro:=TFakeNumber.Create(nil);
  try
    FLabel.PLCTag:=FTag;
    TagValueIs(10);

    FLabel.PLCTag:=outro;
    outro.ChegouDoCLP(99);
    Application.ProcessMessages;
    AssertEquals('segue o tag novo', '99.0', FLabel.Caption);

    TagValueIs(11);
    AssertEquals('e o antigo nao mexe mais', '99.0', FLabel.Caption);
  finally
    FLabel.PLCTag:=nil;
    outro.Free;
  end;
end;

procedure TTestHMILabel.ADestroyedTagLetsGoOfTheLabel;
begin
  FLabel.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('o rotulo largou o tag', FLabel.PLCTag=nil);
end;

procedure TTestHMILabel.ANewLabelIsEnabled;
begin
  AssertTrue('sem codigo de seguranca, habilitado', TControl(FLabel).Enabled);
end;

procedure TTestHMILabel.WithoutPermissionTheLabelIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='ver_producao';

    FLabel.SecurityCode:='ver_receita';

    AssertFalse('sem permissao, desabilitado', TControl(FLabel).Enabled);
  finally
    users.Free;
  end;
end;

procedure TTestHMILabel.TheEnabledPropertyAnswersForTheProgramNotForTheControl;
var
  users:TUserManagementForTest;
begin
  //THMILabel redeclara Enabled lendo o campo do proprio controle, que guarda
  //o que o PROGRAMA pediu. O estado que vale e o E logico disso com a
  //permissao, e so se le pelo ancestral
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='ver_producao';
    FLabel.SecurityCode:='ver_receita';

    AssertTrue('a propriedade diz o que o programa pediu', FLabel.Enabled);
    AssertFalse('mas o controle esta desabilitado',        TControl(FLabel).Enabled);
  finally
    users.Free;
  end;
end;

initialization
  RegisterTest(TTestHMILabel);

end.
