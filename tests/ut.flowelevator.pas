{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do elevador desenhado: o estado que o tag manda, as tres
            partes que ele pinta e o fluxo que passa - ou nao - pela saida.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O elevador e' primo da valvula e da bomba: os mesmos estados vindos do tag,
  a mesma linha de entrada e de saida. O que ele tem de diferente sao tres
  partes pintadas em separado - cabeca, corpo e pe' - e, para cada uma, a
  escolha entre a cor do estado, a cor do produto que esta' passando ou uma
  cor fixa que o estado nao mexe.

  E' essa escolha, e o fluxo que ela acompanha, que se testa aqui.
}
{$ELSE}
{:
  @abstract(Drawn elevator tests: the state the tag chooses, the three parts
            it paints and the flow that does - or does not - leave through the
            output.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The elevator is a cousin of the valve and the pump: the same states coming
  from the tag, the same input and output lines. What it has of its own are
  three parts painted separately - head, body and footer - and, for each one,
  the choice between the state's colour, the colour of the product going
  through, or a fixed colour the state does not touch.

  That choice, and the flow it goes along with, is what is tested here.
}
{$ENDIF}
unit ut.flowelevator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, fpcunit, testregistry,
  hmi_draw_elevador, hmi_polyline, HMIZones, PLCTag, PLCString,
  ControlSecurityManager, CustomizedUserManagement, testsupport.faketag;

type

  { TElevatorProbe }

  //o temporizador do pisca e o passo para o proximo estado sao protegidos.
  //A sonda os expoe para o pisca poder ser conferido sem esperar o relogio.
  //
  //the blink timer and the step to the next state are protected. The probe
  //exposes them so the blink can be checked without waiting for the clock.
  TElevatorProbe = class(THMILinkedFlowElevator)
  public
    function  Blinking:Boolean;
    procedure FireBlink;
  end;

  { TTestBasicElevator }

  TTestBasicElevator = class(TTestCase)
  private
    FElevator:THMIElevadorBasico;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ANewElevatorIsSilverWithTheHeadOnTheRight;
    procedure TheHeadSideIsKept;
    procedure TheColoursOfTheThreePartsAreKept;
    procedure TheBodyWidthIsKept;
  end;

  { TTestFlowElevator }

  TTestFlowElevator = class(TTestCase)
  private
    FElevator:TElevatorProbe;
    FTag:TFakeNumber;
    FInput,
    FOutput:THMIFlowPolyline;
    FStateChanges:LongInt;
    procedure OnStateChanged(Sender:TObject);
    function  NewState(aValue:Double; aFlow:Boolean; aColor, aBorder:TColor):THMIElevatorFlowZone;
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //as tres partes / the three parts
    procedure TheStateColoursHeadBodyAndFooter;
    procedure TheStateColoursTheBorder;
    procedure AStaticPartIsNotPaintedByTheState;

    //a cor do produto / the colour of the product
    procedure TheFlowColourPaintsTheBodyWhenTheStateAsksForIt;
    procedure TheFlowColourPaintsTheHeadWhenTheStateAsksForIt;
    procedure AnEmptyInputPaintsTheBodyWithTheStateEmptyColour;
    procedure AStaticPartIsNotPaintedByTheFlowEither;

    //o fluxo / the flow
    procedure AnOpenStatePassesTheInputColourToTheOutput;
    procedure AClosedStateLeavesTheOutputEmpty;
    procedure TheOutputFollowsTheInputColourChange;
    procedure ADestroyedInputStopsFeedingTheOutput;
    procedure DroppingTheInputStopsFeedingTheOutput;

    //o tag / the tag
    procedure TheTagValueChoosesTheState;
    procedure ATagThatIsNotNumericIsRefused;
    procedure ADestroyedTagLetsGoOfTheElevator;
    procedure TheStateChangeEventFires;

    //o pisca / the blink
    procedure AStateWithoutBlinkLeavesTheTimerOff;
    procedure AStateWithBlinkTurnsTheTimerOn;
    procedure TheBlinkShowsTheOtherState;
    procedure LeavingEveryStateStopsTheBlink;

    //os estados / the states
    procedure AssigningTheStatesOfAnotherElevatorCopiesThem;

    //seguranca / security
    procedure WithoutPermissionTheElevatorIsDisabled;
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

{ TElevatorProbe }

function TElevatorProbe.Blinking:Boolean;
begin
  Result:=Assigned(FZoneTimer) and FZoneTimer.Enabled;
end;

procedure TElevatorProbe.FireBlink;
begin
  NextZone(Self);
end;

{ TTestBasicElevator }

procedure TTestBasicElevator.SetUp;
begin
  FElevator:=THMIElevadorBasico.Create(nil);
end;

procedure TTestBasicElevator.TearDown;
begin
  FreeAndNil(FElevator);
end;

procedure TTestBasicElevator.ANewElevatorIsSilverWithTheHeadOnTheRight;
begin
  AssertFalse ('cabeca a direita', FElevator.HeadAtLeft);
  AssertEquals('cabeca',           clSilver, FElevator.HeadColor);
  AssertEquals('corpo',            clSilver, FElevator.BodyColor);
  AssertEquals('pe',               clSilver, FElevator.FooterColor);
  AssertEquals('largura do corpo', 12,       FElevator.BodyWidth);
end;

procedure TTestBasicElevator.TheHeadSideIsKept;
begin
  FElevator.HeadAtLeft:=true;

  AssertTrue('cabeca a esquerda', FElevator.HeadAtLeft);
end;

procedure TTestBasicElevator.TheColoursOfTheThreePartsAreKept;
begin
  FElevator.HeadColor  :=clRed;
  FElevator.BodyColor  :=clLime;
  FElevator.FooterColor:=clBlue;

  AssertEquals('cabeca', clRed,  FElevator.HeadColor);
  AssertEquals('corpo',  clLime, FElevator.BodyColor);
  AssertEquals('pe',     clBlue, FElevator.FooterColor);
end;

procedure TTestBasicElevator.TheBodyWidthIsKept;
begin
  FElevator.BodyWidth:=30;

  AssertEquals('largura do corpo', 30, FElevator.BodyWidth);
end;

{ TTestFlowElevator }

procedure TTestFlowElevator.SetUp;
begin
  FStateChanges:=0;
  FElevator:=TElevatorProbe.Create(nil);
  FTag:=TFakeNumber.Create(nil);
  FInput:=THMIFlowPolyline.Create(nil);
  FOutput:=THMIFlowPolyline.Create(nil);
  FElevator.InputPolyline:=FInput;
  FElevator.OutputPolyline:=FOutput;
  FElevator.PLCTag:=FTag;
end;

procedure TTestFlowElevator.TearDown;
begin
  FreeAndNil(FElevator);
  FreeAndNil(FInput);
  FreeAndNil(FOutput);
  FreeAndNil(FTag);
end;

procedure TTestFlowElevator.OnStateChanged(Sender:TObject);
begin
  inc(FStateChanges);
end;

function TTestFlowElevator.NewState(aValue:Double; aFlow:Boolean; aColor, aBorder:TColor):THMIElevatorFlowZone;
begin
  Result:=FElevator.ColorAndFlowStates.Add;
  Result.ZoneType:=ztEqual;
  Result.SetValues(aValue, aValue);
  Result.Flow:=aFlow;
  Result.Color:=aColor;
  Result.BorderColor:=aBorder;
end;

procedure TTestFlowElevator.Settle;
begin
  //o recalculo do estado e' agendado na fila da aplicacao
  //the state recalculation is scheduled on the application queue
  Application.ProcessMessages;
end;

procedure TTestFlowElevator.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestFlowElevator.TheStateColoursHeadBodyAndFooter;
begin
  NewState(1, true, clLime, clGreen);

  TagValueIs(1);

  AssertEquals('cabeca', clLime, FElevator.HeadColor);
  AssertEquals('corpo',  clLime, FElevator.BodyColor);
  AssertEquals('pe',     clLime, FElevator.FooterColor);
end;

procedure TTestFlowElevator.TheStateColoursTheBorder;
begin
  NewState(1, true, clLime, clGreen);

  TagValueIs(1);

  AssertEquals('a borda', clGreen, FElevator.BorderColor);
end;

procedure TTestFlowElevator.AStaticPartIsNotPaintedByTheState;
begin
  //cor fixa e' a maneira de dizer "essa parte e' assim e o estado nao mexe"
  //a static colour is the way of saying "this part is like this and the state
  //does not touch it"
  FElevator.UseStaticBodyColor:=true;
  FElevator.BodyColor:=clNavy;
  NewState(1, true, clLime, clGreen);

  TagValueIs(1);

  AssertEquals('o corpo ficou como estava', clNavy, FElevator.BodyColor);
  AssertEquals('e a cabeca seguiu o estado', clLime, FElevator.HeadColor);
end;

procedure TTestFlowElevator.TheFlowColourPaintsTheBodyWhenTheStateAsksForIt;
var
  z:THMIElevatorFlowZone;
begin
  z:=NewState(1, true, clLime, clGreen);
  z.PaintBodyWithFlowColor:=true;
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('o corpo com a cor do produto', clRed,  FElevator.BodyColor);
  AssertEquals('e a cabeca com a do estado',   clLime, FElevator.HeadColor);
end;

procedure TTestFlowElevator.TheFlowColourPaintsTheHeadWhenTheStateAsksForIt;
var
  z:THMIElevatorFlowZone;
begin
  z:=NewState(1, true, clLime, clGreen);
  z.PaintHeaderWithFlowColor:=true;
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('a cabeca com a cor do produto', clRed,  FElevator.HeadColor);
  AssertEquals('e o corpo com a do estado',     clLime, FElevator.BodyColor);
end;

procedure TTestFlowElevator.AnEmptyInputPaintsTheBodyWithTheStateEmptyColour;
var
  z:THMIElevatorFlowZone;
begin
  //elevador rodando sem produto: a parte que mostraria o produto fica com a
  //cor de vazio do estado
  //elevator running with no product: the part that would show the product
  //takes the state's empty colour
  z:=NewState(1, true, clLime, clGreen);
  z.PaintBodyWithFlowColor:=true;
  z.EmptyColor:=clGray;

  TagValueIs(1);

  AssertEquals('o corpo com a cor de vazio', clGray, FElevator.BodyColor);
end;

procedure TTestFlowElevator.AStaticPartIsNotPaintedByTheFlowEither;
var
  z:THMIElevatorFlowZone;
begin
  FElevator.UseStaticBodyColor:=true;
  FElevator.BodyColor:=clNavy;
  z:=NewState(1, true, clLime, clGreen);
  z.PaintBodyWithFlowColor:=true;
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('o corpo ficou como estava', clNavy, FElevator.BodyColor);
end;

procedure TTestFlowElevator.AnOpenStatePassesTheInputColourToTheOutput;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('a saida recebeu o fluxo', clRed, FOutput.LineColor);
end;

procedure TTestFlowElevator.AClosedStateLeavesTheOutputEmpty;
begin
  NewState(0, false, clRed, clMaroon);
  FInput.LineColor:=clRed;

  TagValueIs(0);

  AssertEquals('parado, saida vazia', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowElevator.TheOutputFollowsTheInputColourChange;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;
  TagValueIs(1);

  FInput.LineColor:=clYellow;
  Settle;

  AssertEquals('a saida acompanhou', clYellow, FOutput.LineColor);
end;

procedure TTestFlowElevator.ADestroyedInputStopsFeedingTheOutput;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;
  TagValueIs(1);
  AssertEquals('passou', clRed, FOutput.LineColor);

  FreeAndNil(FInput);
  Settle;

  AssertEquals('sem entrada, saida vazia', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowElevator.DroppingTheInputStopsFeedingTheOutput;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;
  TagValueIs(1);

  FElevator.InputPolyline:=nil;
  Settle;

  AssertEquals('sem entrada, saida vazia', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowElevator.TheTagValueChoosesTheState;
begin
  NewState(0, false, clRed,  clMaroon);
  NewState(1, true,  clLime, clGreen);
  FInput.LineColor:=clRed;

  TagValueIs(1);
  AssertEquals('rodando', clLime, FElevator.BodyColor);
  AssertEquals('e passando', clRed, FOutput.LineColor);

  TagValueIs(0);
  AssertEquals('parado', clRed, FElevator.BodyColor);
  AssertEquals('e sem passar', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowElevator.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  //um elevador escolhe o estado por um numero. O tag de texto e' um tag de
  //verdade; o unico motivo de recusa possivel e' nao ser numerico
  //
  //an elevator picks its state by a number. The text tag is a real tag; the
  //only possible reason to refuse it is not being numeric
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FElevator.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FElevator.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestFlowElevator.ADestroyedTagLetsGoOfTheElevator;
begin
  FreeAndNil(FTag);

  AssertTrue('o elevador soltou o tag', FElevator.PLCTag=nil);
end;

procedure TTestFlowElevator.TheStateChangeEventFires;
begin
  FElevator.OnStateChange:=@OnStateChanged;
  NewState(0, false, clRed,  clMaroon);
  NewState(1, true,  clLime, clGreen);

  TagValueIs(1);

  AssertTrue('avisou a troca de estado', FStateChanges>0);
end;

procedure TTestFlowElevator.AStateWithoutBlinkLeavesTheTimerOff;
begin
  NewState(1, true, clLime, clGreen);

  TagValueIs(1);

  AssertFalse('estado parado nao pisca', FElevator.Blinking);
end;

procedure TTestFlowElevator.AStateWithBlinkTurnsTheTimerOn;
var
  z:THMIElevatorFlowZone;
begin
  z:=NewState(1, true, clLime, clGreen);
  NewState(2, true, clYellow, clOlive);
  z.BlinkTime:=100;
  z.BlinkWith:=1;

  TagValueIs(1);

  AssertTrue('estado que pisca liga o temporizador', FElevator.Blinking);
end;

procedure TTestFlowElevator.TheBlinkShowsTheOtherState;
var
  z:THMIElevatorFlowZone;
begin
  //cada batida do relogio troca o desenho para o estado apontado por
  //BlinkWith
  //every tick swaps the drawing to the state BlinkWith points at
  z:=NewState(1, true, clLime, clGreen);
  NewState(2, true, clYellow, clOlive);
  z.BlinkTime:=100;
  z.BlinkWith:=1;
  TagValueIs(1);
  AssertEquals('mostrando o estado do tag', clLime, FElevator.BodyColor);

  FElevator.FireBlink;

  AssertEquals('mostrando o estado do pisca', clYellow, FElevator.BodyColor);
end;

procedure TTestFlowElevator.LeavingEveryStateStopsTheBlink;
var
  z:THMIElevatorFlowZone;
begin
  //sair para um valor sem estado nenhum deixa o estado atual em nil; o
  //temporizador nao pode continuar ligado chamando o pisca em cima de nada
  //
  //moving to a value with no state at all leaves the current state nil; the
  //timer cannot stay on, calling the blink on nothing
  z:=NewState(1, true, clLime, clGreen);
  NewState(2, true, clYellow, clOlive);
  z.BlinkTime:=100;
  z.BlinkWith:=1;
  TagValueIs(1);
  AssertTrue('piscando', FElevator.Blinking);

  TagValueIs(99);

  AssertFalse('sem estado, sem pisca', FElevator.Blinking);
end;

procedure TTestFlowElevator.AssigningTheStatesOfAnotherElevatorCopiesThem;
var
  outro:THMILinkedFlowElevator;
  z:THMIElevatorFlowZone;
begin
  z:=NewState(1, true, clLime, clGreen);
  z.EmptyColor:=clGray;
  z.PaintBodyWithFlowColor:=true;

  outro:=THMILinkedFlowElevator.Create(nil);
  try
    outro.ColorAndFlowStates:=FElevator.ColorAndFlowStates;

    AssertEquals('um estado', 1,       outro.ColorAndFlowStates.Count);
    AssertEquals('a cor',     clLime,  THMIElevatorFlowZone(outro.ColorAndFlowStates.Items[0]).Color);
    AssertEquals('a borda',   clGreen, THMIElevatorFlowZone(outro.ColorAndFlowStates.Items[0]).BorderColor);
    AssertTrue  ('o fluxo',   THMIElevatorFlowZone(outro.ColorAndFlowStates.Items[0]).Flow);
    AssertEquals('a cor de vazio', clGray, THMIElevatorFlowZone(outro.ColorAndFlowStates.Items[0]).EmptyColor);
    AssertTrue  ('e o corpo pela cor do produto',
                 THMIElevatorFlowZone(outro.ColorAndFlowStates.Items[0]).PaintBodyWithFlowColor);
  finally
    outro.Free;
  end;
end;

procedure TTestFlowElevator.WithoutPermissionTheElevatorIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='abrir_valvula';

    FElevator.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitado', TControl(FElevator).Enabled);
  finally
    users.Free;
  end;
end;

initialization
  RegisterTest(TTestBasicElevator);
  RegisterTest(TTestFlowElevator);

end.
