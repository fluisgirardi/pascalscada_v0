{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIButton: o botao que liga e desliga coisas no
            processo.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um botao de tela tem duas metades. A de saida: ao ser pressionado ele
  escreve um valor no tag - qual valor depende do tipo (liga/desliga,
  momentaneo, pulso enquanto pressionado). E a de entrada: a aparencia dele -
  texto, cor e afundado ou nao - tem que dizer o que o processo respondeu, nao
  o que o operador clicou.

  E' essa segunda metade que costuma mentir: um botao que fica afundado porque
  foi clicado, e nao porque o motor ligou, e' um instrumento quebrado.
}
{$ELSE}
{:
  @abstract(THMIButton tests: the button that switches things on and off in
            the process.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A screen button has two halves. The output one: when pressed it writes a
  value to the tag - which value depends on the type (on/off, momentary, pulse
  while held). And the input one: its looks - text, colour and pressed or not -
  have to say what the process answered, not what the operator clicked.

  It is that second half that tends to lie: a button that stays pressed because
  it was clicked, rather than because the motor started, is a broken
  instrument.
}
{$ENDIF}
unit ut.hmibutton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Buttons, Graphics, Forms, fpcunit, testregistry,
  HMIButton, HMITypes, PLCTag, PLCString,
  ControlSecurityManager, CustomizedUserManagement, testsupport.faketag;

type

  { TButtonProbe }

  //o estado do botao (FState) e' atualizado pelo widget no clique do mouse e
  //so' depois o Click e' chamado - e' o contrato que o proprio codigo do
  //controle documenta. A sonda encena esse passo sem precisar de mouse.
  //
  //the button state (FState) is updated by the widget on the mouse click and
  //only then is Click called - the contract the control's own code documents.
  //The probe stages that step with no mouse involved.
  TButtonProbe = class(THMIButton)
  public
    procedure PressTo(bs:TButtonState);
    procedure ReleaseTheMouse;
    function  StateIs:TButtonState;
  end;

  { TTestHMIButton }

  TTestHMIButton = class(TTestCase)
  private
    FButton:TButtonProbe;
    FTag:TFakeNumber;
    FTag2:TFakeNumber;
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //criado por codigo / created by code
    procedure EnablingARuntimeCreatedControlKeepsItEnabled;
    //o que o processo responde / what the process answers
    procedure TheTagValueAtLinkTimeSetsTheLook;
    procedure TheButtonFollowsTheTagValue;
    procedure ACaptionAndAColourForEachState;
    procedure AnUnknownValueCanShowItDown;
    procedure AnUnknownValueCanShowItUp;
    procedure AnUnknownValueCanShowItGrayed;
    //retroalimentacao / feedback tag
    procedure AFeedbackTagDrivesTheLook;
    procedure TheWriteTagDoesNotDriveTheLookWhenThereIsAFeedbackTag;
    procedure AToggleButtonWithFeedbackFollowsTheFeedback;
    procedure ASingleTagOnBothRolesKeepsWorking;
    procedure AFeedbackTagThatDiesLetsGoOfTheButton;
    //imagens / bitmaps
    procedure ABitmapForEachState;

    //o que o operador manda / what the operator sends
    procedure PressingAnOnOffButtonDownWritesTheDownValue;
    procedure PressingAnOnOffButtonUpWritesTheUpValue;
    procedure AMomentaryButtonGoesDownAndComesBack;
    procedure AJogButtonWritesTheDownValueWhilePressed;
    procedure ReleasingAJogButtonWritesTheUpValue;
    procedure AToggleButtonInvertsTheTagValue;
    procedure AToggleButtonNeverLooksPressed;
    procedure WithNoTagAClickWritesNothing;

    //os valores / the values
    procedure ANewButtonComesWithUsableValues;

    //o tag / the tag
    procedure ATagThatIsNotNumericIsRefused;
    procedure ADestroyedTagLetsGoOfTheButton;
    procedure ClearingTheTagShowsTheUnknownState;
    procedure ADestroyedTagShowsTheUnknownState;

    //seguranca / security
    procedure WithoutPermissionTheButtonIsDisabled;
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

{ TButtonProbe }

procedure TButtonProbe.PressTo(bs:TButtonState);
begin
  FState:=bs;
  Click;
end;

procedure TButtonProbe.ReleaseTheMouse;
begin
  MouseUp(mbLeft, [], 0, 0);
end;

function TButtonProbe.StateIs:TButtonState;
begin
  Result:=FState;
end;

{ TTestHMIButton }

procedure TTestHMIButton.SetUp;
begin
  FButton:=TButtonProbe.Create(nil);
  FButton.ValueDown:=1;
  FButton.ValueUp:=0;
  FButton.CaptionDown:='LIGADO';
  FButton.CaptionUp:='DESLIGADO';
  FButton.CaptionGrayed:='?';
  FButton.ColorDown:=clLime;
  FButton.ColorUp:=clRed;
  FButton.ColorGrayed:=clGray;
  FTag:=TFakeNumber.Create(nil);
  FTag2:=TFakeNumber.Create(nil);
end;

procedure TTestHMIButton.TearDown;
begin
  FreeAndNil(FButton);
  FreeAndNil(FTag);
  FreeAndNil(FTag2);
end;

procedure TTestHMIButton.Settle;
begin
  Application.ProcessMessages;
end;

procedure TTestHMIButton.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestHMIButton.EnablingARuntimeCreatedControlKeepsItEnabled;
begin
  //sem .lfm e sem codigo de seguranca: desabilitar e reabilitar tem que
  //deixa-lo habilitado. A flag de seguranca nascia falsa, e o E logico
  //com ela desabilitava o controle no primeiro Enabled:=true.
  //with no .lfm and no security code: disabling and re-enabling has to
  //leave it enabled. The security flag was born false, and the logical
  //AND with it disabled the control on the first Enabled:=true.
  FButton.Enabled:=false;

  FButton.Enabled:=true;

  AssertTrue('habilitado', TControl(FButton).Enabled);
end;

procedure TTestHMIButton.TheTagValueAtLinkTimeSetsTheLook;
begin
  FTag.ChegouDoCLP(1);

  FButton.PLCTag:=FTag;

  AssertTrue  ('afundado',  FButton.Down);
  AssertEquals('com o texto de ligado', 'LIGADO', FButton.Caption);
end;

procedure TTestHMIButton.TheButtonFollowsTheTagValue;
begin
  //o processo mudou sozinho - por outra tela, por intertravamento, pelo
  //proprio CLP. O botao tem que mostrar o que aconteceu.
  //the process changed on its own - from another screen, from an interlock,
  //from the PLC itself. The button has to show what happened.
  FButton.PLCTag:=FTag;
  TagValueIs(0);
  AssertFalse('comeca solto', FButton.Down);

  TagValueIs(1);

  AssertTrue  ('acompanhou o tag',      FButton.Down);
  AssertEquals('e trocou o texto',      'LIGADO', FButton.Caption);
end;

procedure TTestHMIButton.ACaptionAndAColourForEachState;
begin
  FButton.PLCTag:=FTag;

  TagValueIs(0);
  AssertEquals('texto de desligado', 'DESLIGADO', FButton.Caption);
  AssertEquals('cor de desligado',   clRed,       FButton.Color);

  TagValueIs(1);
  AssertEquals('texto de ligado', 'LIGADO', FButton.Caption);
  AssertEquals('cor de ligado',   clLime,   FButton.Color);
end;

procedure TTestHMIButton.AnUnknownValueCanShowItDown;
begin
  //valor que nao e' nem o de ligado nem o de desligado: quem decide o que
  //mostrar e' OtherValuesIs
  //a value that is neither the on nor the off one: OtherValuesIs decides what
  //to show
  FButton.OtherValuesIs:=isChecked;
  FButton.PLCTag:=FTag;

  TagValueIs(7);

  AssertTrue('afundado', FButton.Down);
end;

procedure TTestHMIButton.AnUnknownValueCanShowItUp;
begin
  FButton.OtherValuesIs:=isUnchecked;
  FButton.PLCTag:=FTag;
  TagValueIs(1);

  TagValueIs(7);

  AssertFalse('solto', FButton.Down);
end;

procedure TTestHMIButton.AnUnknownValueCanShowItGrayed;
begin
  FButton.OtherValuesIs:=IsGrayed;
  FButton.PLCTag:=FTag;

  TagValueIs(7);

  AssertEquals('texto de indefinido', '?', FButton.Caption);
  AssertEquals('cor de indefinido',   clGray, FButton.Color);
end;

procedure TTestHMIButton.PressingAnOnOffButtonDownWritesTheDownValue;
begin
  FButton.ButtonType:=btOnOff;
  FButton.PLCTag:=FTag;

  FButton.PressTo(bsDown);

  AssertEquals('mandou o valor de ligado', 1, FTag.Value, 0.0001);
end;

procedure TTestHMIButton.PressingAnOnOffButtonUpWritesTheUpValue;
begin
  FButton.ButtonType:=btOnOff;
  FButton.PLCTag:=FTag;
  FTag.ChegouDoCLP(1);

  FButton.PressTo(bsUp);

  AssertEquals('mandou o valor de desligado', 0, FTag.Value, 0.0001);
end;

procedure TTestHMIButton.AMomentaryButtonGoesDownAndComesBack;
begin
  //botao de pulso: manda o valor de ligado e, na mesma batida, o de desligado
  //momentary button: sends the on value and, in the same beat, the off one
  FButton.ButtonType:=btMomentary;
  FButton.PLCTag:=FTag;

  FButton.PressTo(bsDown);

  AssertEquals('terminou desligado', 0, FTag.Value, 0.0001);
  AssertEquals('duas escritas',      2, FTag.Escritas);
end;

procedure TTestHMIButton.AJogButtonWritesTheDownValueWhilePressed;
begin
  FButton.ButtonType:=btJog;
  FButton.PLCTag:=FTag;

  FButton.PressTo(bsDown);

  AssertEquals('ligado enquanto pressionado', 1, FTag.Value, 0.0001);
end;

procedure TTestHMIButton.ReleasingAJogButtonWritesTheUpValue;
begin
  FButton.ButtonType:=btJog;
  FButton.PLCTag:=FTag;
  FButton.PressTo(bsDown);

  FButton.ReleaseTheMouse;

  AssertEquals('desligou ao soltar', 0, FTag.Value, 0.0001);
end;

procedure TTestHMIButton.AToggleButtonInvertsTheTagValue;
begin
  //btToogle, como esta' documentado no TButtonType: inverte o valor do tag
  //btToogle, as documented on TButtonType: inverts the tag's value
  FButton.ButtonType:=btToogle;
  FButton.PLCTag:=FTag;
  TagValueIs(0);

  FButton.PressTo(bsDown);

  AssertEquals('inverteu para ligado', 1, FTag.Value, 0.0001);

  FButton.PressTo(bsUp);

  AssertEquals('e de volta para desligado', 0, FTag.Value, 0.0001);
end;

procedure TTestHMIButton.AToggleButtonNeverLooksPressed;
begin
  //"inverte o valor do tag e mantem a aparencia solta", diz o TButtonType
  //"inverts the tag value and keeps the released look", says TButtonType
  FButton.ButtonType:=btToogle;
  FButton.PLCTag:=FTag;

  TagValueIs(1);

  AssertFalse('continua solto', FButton.Down);
end;

procedure TTestHMIButton.WithNoTagAClickWritesNothing;
begin
  FButton.ButtonType:=btOnOff;

  FButton.PressTo(bsDown);

  AssertEquals('nada escrito', 0, FTag.Escritas);
end;

procedure TTestHMIButton.ANewButtonComesWithUsableValues;
var
  novo:TButtonProbe;
begin
  //criado por codigo: com ValueDown igual a ValueUp o botao fica
  //permanentemente afundado e todo clique escreve o mesmo valor
  //created from code: with ValueDown equal to ValueUp the button stays pressed
  //forever and every click writes the same value
  novo:=TButtonProbe.Create(nil);
  try
    AssertTrue('os dois valores sao diferentes', novo.ValueDown<>novo.ValueUp);
  finally
    novo.Free;
  end;
end;

procedure TTestHMIButton.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  FButton.PLCTag:=FTag;
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FButton.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FButton.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestHMIButton.ClearingTheTagShowsTheUnknownState;
begin
  //sem tag, o valor nao e' nem o de ligado nem o de desligado: e' o mesmo
  //caso de OtherValuesIs
  //with no tag the value is neither the on nor the off one: it is the same
  //case as OtherValuesIs
  FButton.OtherValuesIs:=IsGrayed;
  FButton.PLCTag:=FTag;
  TagValueIs(1);
  AssertEquals('mostrando ligado', 'LIGADO', FButton.Caption);

  FButton.PLCTag:=nil;

  AssertEquals('sem tag, indefinido', '?', FButton.Caption);
end;

procedure TTestHMIButton.ADestroyedTagShowsTheUnknownState;
begin
  FButton.OtherValuesIs:=IsGrayed;
  FButton.PLCTag:=FTag;
  TagValueIs(1);

  FreeAndNil(FTag);

  AssertEquals('sem tag, indefinido', '?', FButton.Caption);
end;

procedure TTestHMIButton.ADestroyedTagLetsGoOfTheButton;
begin
  FButton.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('o botao soltou o tag', FButton.PLCTag=nil);
end;

procedure TTestHMIButton.AFeedbackTagDrivesTheLook;
begin
  //o CLP devolve o estado real em outra variavel: e' dela que vem a aparencia
  //the PLC gives the real state back on another variable: the looks come from it
  FButton.PLCTag:=FTag;
  FButton.FeedbackTag:=FTag2;

  FTag2.ChegouDoCLP(1);
  AssertEquals('retroalimentacao em 1, afundado', 'LIGADO', FButton.Caption);

  FTag2.ChegouDoCLP(0);
  AssertEquals('retroalimentacao em 0, solto', 'DESLIGADO', FButton.Caption);
end;

procedure TTestHMIButton.TheWriteTagDoesNotDriveTheLookWhenThereIsAFeedbackTag;
begin
  //com retroalimentacao, o valor escrito nao manda na aparencia: o clique
  //manda 1, mas ate' o CLP devolver, o botao mostra o que o processo respondeu
  //with a feedback tag, the written value does not rule the looks: the click
  //sends 1, but until the PLC answers, the button shows what the process said
  FButton.PLCTag:=FTag;
  FButton.FeedbackTag:=FTag2;
  FTag2.ChegouDoCLP(0);

  FTag.ChegouDoCLP(1);

  AssertEquals('continua solto', 'DESLIGADO', FButton.Caption);
end;

procedure TTestHMIButton.AToggleButtonWithFeedbackFollowsTheFeedback;
begin
  //o botao de inversao com retroalimentacao: cada clique inverte o valor
  //escrito e a aparencia segue o retorno do CLP
  //the toggle button with feedback: every click inverts the written value and
  //the looks follow the PLC's answer
  FButton.ButtonType:=btToogle;
  FButton.PLCTag:=FTag;
  FButton.FeedbackTag:=FTag2;
  FTag2.ChegouDoCLP(0);

  FButton.PressTo(bsDown);
  AssertEquals('primeiro clique escreve 1', 1, FTag.Value, 0.0001);
  AssertEquals('CLP ainda nao respondeu, solto', 'DESLIGADO', FButton.Caption);

  FTag2.ChegouDoCLP(1);
  AssertEquals('CLP ligou, afundado', 'LIGADO', FButton.Caption);

  FButton.PressTo(bsUp);
  AssertEquals('segundo clique escreve 0', 0, FTag.Value, 0.0001);
end;

procedure TTestHMIButton.ASingleTagOnBothRolesKeepsWorking;
begin
  //o mesmo tag escrevendo e retroalimentando tem que continuar funcionando
  //the same tag writing and feeding back must keep working
  FButton.OtherValuesIs:=IsGrayed;
  FButton.PLCTag:=FTag;
  FButton.FeedbackTag:=FTag;

  FTag.ChegouDoCLP(1);
  AssertEquals('um, afundado', 'LIGADO', FButton.Caption);

  FButton.PLCTag:=nil;

  AssertTrue('soltou o papel de escrita', FButton.PLCTag=nil);
  AssertTrue('a retroalimentacao continua ligada', FButton.FeedbackTag=FTag);

  //e o aviso de mudanca do tag ainda alimenta a aparencia
  //and the tag's change notice still drives the looks
  FTag.ChegouDoCLP(0);
  AssertEquals('zero, solto', 'DESLIGADO', FButton.Caption);
end;

procedure TTestHMIButton.AFeedbackTagThatDiesLetsGoOfTheButton;
begin
  FButton.PLCTag:=FTag;
  FButton.FeedbackTag:=FTag2;

  FreeAndNil(FTag2);

  AssertTrue('o botao soltou a retroalimentacao', FButton.FeedbackTag=nil);
end;

procedure TTestHMIButton.ABitmapForEachState;
var
  up, down:TBitmap;
begin
  //cada estado com a sua imagem
  //each state with its own image
  up:=TBitmap.Create;
  up.SetSize(8,8);
  up.Canvas.Brush.Color:=clRed;
  up.Canvas.FillRect(0,0,8,8);
  down:=TBitmap.Create;
  down.SetSize(8,8);
  down.Canvas.Brush.Color:=clBlue;
  down.Canvas.FillRect(0,0,8,8);
  try
    FButton.PLCTag:=FTag;
    FButton.GlyphUp:=up;
    FButton.GlyphDown:=down;

    TagValueIs(0);
    AssertTrue('estado 0 usa a imagem de solto', FButton.Glyph.Canvas.Pixels[4,4]=clRed);

    TagValueIs(1);
    AssertTrue('estado 1 usa a imagem de afundado', FButton.Glyph.Canvas.Pixels[4,4]=clBlue);
  finally
    up.Free;
    down.Free;
  end;
end;

procedure TTestHMIButton.WithoutPermissionTheButtonIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='ligar_motor';

    FButton.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitado', TControl(FButton).Enabled);
  finally
    users.Free;
  end;
end;

initialization
  RegisterTest(TTestHMIButton);

end.
