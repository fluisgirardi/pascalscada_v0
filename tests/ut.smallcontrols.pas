{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes dos dois controles menores da camada: o radio ligado a um
            tag e o botao transparente.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O THMIRadioButton e' uma caixa de marcar com duas regras a mais: um clique
  sempre marca - nunca desmarca - e qualquer valor que nao seja o de marcado
  deixa o radio solto. E' assim que um grupo de opcoes mostra em qual modo o
  processo esta'.

  O THMITransparentButton e' uma area clicavel sem desenho proprio, usada por
  cima de sinoticos. Nao tem tag; o que se testa nele e' nao estragar a area
  de desenho que a base mantem.
}
{$ELSE}
{:
  @abstract(Tests of the two smallest controls in the layer: the radio button
            linked to a tag and the transparent button.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  THMIRadioButton is a check box with two extra rules: a click always checks -
  never unchecks - and any value other than the checked one leaves the radio
  released. That is how a group of options shows which mode the process is in.

  THMITransparentButton is a clickable area with no drawing of its own, used on
  top of synoptics. It has no tag; what is tested on it is that it does not
  wreck the drawing area the base class keeps.
}
{$ENDIF}
unit ut.smallcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, fpcunit, testregistry,
  HMIRadioButton, HMITransparentButton, HMITypes, PLCTag,
  ControlSecurityManager, CustomizedUserManagement, testsupport.faketag;

type

  { TRadioProbe }

  TRadioProbe = class(THMIRadioButton)
  public
    procedure PressIt;
  end;

  { TTransparentProbe }

  //FControlArea e' a area de desenho que a base cria no construtor e usa em
  //todo o resto; a sonda so' diz se ela ainda esta' la'.
  //
  //FControlArea is the drawing area the base class creates in the constructor
  //and uses everywhere else; the probe only says whether it is still there.
  TTransparentProbe = class(THMITransparentButton)
  public
    procedure PaintIt;
    function  HasDrawingArea:Boolean;
    function  DrawingAreaWidth:Integer;
    function  DrawingAreaHeight:Integer;
  end;

  { TUserManagementForTest }

  TUserManagementForTest = class(TCustomizedUserManagement)
  private
    FGranted:UTF8String;
    FAsked:LongInt;
    FWatched:UTF8String;
  protected
    function CanAccess(sc:UTF8String):Boolean; override;
  public
    property Granted:UTF8String read FGranted write FGranted;
    property Watched:UTF8String read FWatched write FWatched;
    property Asked:LongInt read FAsked write FAsked;
  end;

  { TTestHMIRadioButton }

  TTestHMIRadioButton = class(TTestCase)
  private
    FRadio:TRadioProbe;
    FTag:TFakeNumber;
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o que o processo responde / what the process answers
    procedure ANewRadioLeavesUnknownValuesUnchecked;
    procedure TheTrueValueChecksTheRadio;
    procedure TheFalseValueLeavesItUnchecked;
    procedure AnUnknownValueLeavesItUnchecked;
    procedure LosingTheTagLeavesItUnchecked;

    //o que o operador manda / what the operator sends
    procedure ClickingChecksTheRadio;
    procedure ClickingWritesTheTrueValue;
    procedure ClickingAnAlreadyCheckedRadioSaysItAgain;

    //cadastro na seguranca / registering with security
    procedure TheRadioIsRegisteredOnlyOnce;
    procedure WithoutPermissionTheRadioIsDisabled;
  end;

  { TTestHMITransparentButton }

  TTestHMITransparentButton = class(TTestCase)
  private
    FForm:TForm;
    FButton:TTransparentProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ANewButtonHasADrawingArea;
    procedure PaintingKeepsTheDrawingArea;
    procedure PaintingTwiceKeepsTheDrawingArea;
    procedure PaintingSizesTheDrawingAreaToTheControl;
  end;

implementation

function TUserManagementForTest.CanAccess(sc:UTF8String):Boolean;
begin
  if (FWatched<>'') and (sc=FWatched) then
    inc(FAsked);
  Result:=(Trim(sc)='') or (sc=FGranted);
end;

{ TRadioProbe }

procedure TRadioProbe.PressIt;
begin
  Click;
end;

{ TTransparentProbe }

procedure TTransparentProbe.PaintIt;
begin
  Paint;
end;

function TTransparentProbe.HasDrawingArea:Boolean;
begin
  Result:=Assigned(FControlArea);
end;

function TTransparentProbe.DrawingAreaWidth:Integer;
begin
  Result:=FControlArea.Width;
end;

function TTransparentProbe.DrawingAreaHeight:Integer;
begin
  Result:=FControlArea.Height;
end;

{ TTestHMIRadioButton }

procedure TTestHMIRadioButton.SetUp;
begin
  FRadio:=TRadioProbe.Create(nil);
  FRadio.ValueTrue:=1;
  FRadio.ValueFalse:=0;
  FTag:=TFakeNumber.Create(nil);
  FRadio.PLCTag:=FTag;
end;

procedure TTestHMIRadioButton.TearDown;
begin
  FreeAndNil(FRadio);
  FreeAndNil(FTag);
end;

procedure TTestHMIRadioButton.Settle;
begin
  Application.ProcessMessages;
end;

procedure TTestHMIRadioButton.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestHMIRadioButton.ANewRadioLeavesUnknownValuesUnchecked;
begin
  //um radio nao tem estado cinza: ou e' esta opcao, ou nao e'
  //a radio has no grey state: either it is this option, or it is not
  AssertTrue('valores desconhecidos deixam solto', FRadio.OtherValuesIS=isUnchecked);
end;

procedure TTestHMIRadioButton.TheTrueValueChecksTheRadio;
begin
  TagValueIs(1);

  AssertTrue('marcado', FRadio.Checked);
end;

procedure TTestHMIRadioButton.TheFalseValueLeavesItUnchecked;
begin
  TagValueIs(1);

  TagValueIs(0);

  AssertFalse('solto', FRadio.Checked);
end;

procedure TTestHMIRadioButton.AnUnknownValueLeavesItUnchecked;
begin
  TagValueIs(1);

  TagValueIs(99);

  AssertFalse('solto', FRadio.Checked);
end;

procedure TTestHMIRadioButton.LosingTheTagLeavesItUnchecked;
begin
  TagValueIs(1);
  AssertTrue('marcado pelo tag', FRadio.Checked);

  FRadio.PLCTag:=nil;

  AssertFalse('sem tag, solto', FRadio.Checked);
end;

procedure TTestHMIRadioButton.ClickingChecksTheRadio;
begin
  //clicar num radio ja' marcado nao o desmarca - quem desmarca e' o processo,
  //ou o clique em outra opcao do grupo
  //clicking an already checked radio does not uncheck it - what unchecks it is
  //the process, or a click on another option of the group
  TagValueIs(0);
  AssertFalse('comeca solto', FRadio.Checked);

  FRadio.PressIt;

  AssertTrue('marcou', FRadio.Checked);
end;

procedure TTestHMIRadioButton.ClickingWritesTheTrueValue;
begin
  TagValueIs(0);

  FRadio.PressIt;

  AssertEquals('mandou o valor de marcado', 1, FTag.Value, 0.0001);
end;

procedure TTestHMIRadioButton.ClickingAnAlreadyCheckedRadioSaysItAgain;
var
  antes:LongInt;
begin
  //reafirmar a opcao e' util: e' o operador insistindo no modo que ele quer
  //saying it again is useful: it is the operator insisting on the mode they
  //want
  TagValueIs(1);
  antes:=FTag.Escritas;

  FRadio.PressIt;

  AssertTrue('escreveu de novo', FTag.Escritas>antes);
end;

procedure TTestHMIRadioButton.TheRadioIsRegisteredOnlyOnce;
var
  users:TUserManagementForTest;
begin
  //o gerenciador percorre os controles cadastrados; um controle cadastrado
  //duas vezes e' reavaliado duas vezes a cada troca de usuario
  //the manager sweeps the registered controls; a control registered twice is
  //re-evaluated twice on every change of user
  users:=TUserManagementForTest.Create(nil);
  try
    FRadio.SecurityCode:='parar_motor';
    users.Watched:='parar_motor';
    users.Asked:=0;

    GetControlSecurityManager.UpdateControls;

    AssertEquals('perguntou uma vez so', 1, users.Asked);
  finally
    users.Free;
  end;
end;

procedure TTestHMIRadioButton.WithoutPermissionTheRadioIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='ligar_bomba';

    FRadio.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitado', TControl(FRadio).Enabled);
  finally
    users.Free;
  end;
end;

{ TTestHMITransparentButton }

procedure TTestHMITransparentButton.SetUp;
begin
  //desenhar exige janela de verdade: sem formulario mae o Canvas nao existe
  //painting needs a real window: with no parent form there is no Canvas
  FForm:=TForm.CreateNew(nil);
  FForm.Visible:=false;
  FButton:=TTransparentProbe.Create(FForm);
  FButton.Parent:=FForm;
  FButton.SetBounds(0,0,40,20);
end;

procedure TTestHMITransparentButton.TearDown;
begin
  FreeAndNil(FForm);
end;

procedure TTestHMITransparentButton.ANewButtonHasADrawingArea;
begin
  AssertTrue('a area de desenho existe', FButton.HasDrawingArea);
end;

procedure TTestHMITransparentButton.PaintingKeepsTheDrawingArea;
begin
  //a area de desenho e' da base, que a cria no construtor e conta com ela em
  //todo o resto: desenhar nao pode leva-la embora
  //the drawing area belongs to the base class, which creates it in the
  //constructor and relies on it everywhere else: painting must not take it
  //away
  FButton.PaintIt;

  AssertTrue('a area de desenho continua la', FButton.HasDrawingArea);
end;

procedure TTestHMITransparentButton.PaintingTwiceKeepsTheDrawingArea;
begin
  FButton.PaintIt;
  FButton.PaintIt;

  AssertTrue('a area de desenho continua la', FButton.HasDrawingArea);
end;

procedure TTestHMITransparentButton.PaintingSizesTheDrawingAreaToTheControl;
begin
  //de nada adianta a area existir se ela nao tem o tamanho do controle
  //an area that exists is no use if it does not have the control's size
  FButton.SetBounds(0,0,60,30);

  FButton.PaintIt;

  AssertEquals('largura', 60, FButton.DrawingAreaWidth);
  AssertEquals('altura',  30, FButton.DrawingAreaHeight);
end;

initialization
  RegisterTest(TTestHMIRadioButton);
  RegisterTest(TTestHMITransparentButton);

end.
