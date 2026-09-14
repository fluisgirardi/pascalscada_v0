{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da valvula e da bomba desenhadas: o estado que o tag manda,
            a cor que ele pinta e o fluxo que ele deixa - ou nao - passar.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A valvula e a bomba sao o mesmo mecanismo com desenhos diferentes: uma
  colecao de estados, cada um com uma faixa de valores, uma cor de corpo, uma
  cor de borda e um sim/nao chamado Flow. O tag escolhe o estado; o estado
  pinta o desenho e diz se a cor que chega pela linha de entrada segue pela
  linha de saida ou se a saida fica vazia.

  E' o mesmo par entrada/saida do THMIFlowPolyline, so que agora quem decide
  se o fluxo passa e' o equipamento no meio do caminho.
}
{$ELSE}
{:
  @abstract(Drawn valve and pump tests: the state the tag chooses, the colour
            it paints and the flow it does - or does not - let through.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The valve and the pump are the same mechanism with different drawings: a
  collection of states, each with a range of values, a body colour, a border
  colour and a yes/no called Flow. The tag picks the state; the state paints
  the drawing and says whether the colour arriving through the input line
  carries on through the output line or the output goes empty.

  It is the same input/output pair as THMIFlowPolyline, except that now what
  decides whether the flow passes is the equipment in the middle of the way.
}
{$ENDIF}
unit ut.flowvalve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, fpcunit, testregistry,
  HMI_Draw_Valves, hmi_draw_flow_valve, hmi_draw_flow_pump, hmi_flow_zones,
  hmi_polyline, HMIZones, PLCTag, PLCString,
  ControlSecurityManager, CustomizedUserManagement, testsupport.faketag;

type

  { TValveProbe }

  //BodyColor, BorderColor e o temporizador do pisca sao protegidos: em
  //producao so o desenho os consulta. A sonda os expoe para conferir o que o
  //estado escolhido fez com a valvula.
  //
  //BodyColor, BorderColor and the blink timer are protected: in production
  //only the drawing asks for them. The probe exposes them so what the chosen
  //state did to the valve can be checked.
  TValveProbe = class(THMILinkedFlowValve)
  public
    function  BodyColorIs:TColor;
    function  BorderColorIs:TColor;
    function  Blinking:Boolean;
  end;

  { TPumpProbe }

  TPumpProbe = class(THMILinkedFlowPump)
  public
    function  BorderColorIs:TColor;
    function  Blinking:Boolean;
  end;

  { TTestBasicValve }

  TTestBasicValve = class(TTestCase)
  private
    FValve:THMIBasicValve;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ANewValveIsSimpleAndNotMirrored;
    procedure TheValveTypeIsKept;
    procedure MirroringIsKept;
    procedure TheBodyPercentAcceptsTheWholeRange;
    procedure ABodyPercentAboveOneIsRefused;
    procedure ABodyPercentBelowZeroIsRefused;
    procedure AGoodPercentIsStillAcceptedAfterARefusedOne;
  end;

  { TTestFlowValve }

  TTestFlowValve = class(TTestCase)
  private
    FValve:TValveProbe;
    FTag:TFakeNumber;
    FInput,
    FOutput:THMIFlowPolyline;
    function  NewState(aValue:Double; aFlow:Boolean; aColor, aBorder:TColor):THMIFlowZone;
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o fluxo / the flow
    procedure AnOpenStatePassesTheInputColourToTheOutput;
    procedure AClosedStateLeavesTheOutputEmpty;
    procedure TheOutputFollowsTheInputColourChange;
    procedure TheTagValueOpensAndClosesTheValve;
    procedure ADestroyedInputStopsFeedingTheOutput;
    procedure DroppingTheInputStopsFeedingTheOutput;

    //o desenho / the drawing
    procedure TheStateColoursTheValve;

    //o pisca / the blink
    procedure AStateWithoutBlinkLeavesTheTimerOff;
    procedure AStateWithBlinkTurnsTheTimerOn;
    procedure LeavingEveryStateStopsTheBlink;

    //o tag / the tag
    procedure ATagThatIsNotNumericIsRefused;
    procedure ADestroyedTagLetsGoOfTheValve;

    //os estados / the states
    procedure AssigningTheStatesOfAnotherValveCopiesThem;

    //seguranca / security
    procedure WithoutPermissionTheValveIsDisabled;
  end;

  { TTestFlowPump }

  TTestFlowPump = class(TTestCase)
  private
    FPump:TPumpProbe;
    FTag:TFakeNumber;
    FInput,
    FOutput:THMIFlowPolyline;
    FStateChanges:LongInt;
    procedure OnStateChanged(Sender:TObject);
    function  NewState(aValue:Double; aFlow:Boolean; aColor, aBorder:TColor):THMIFlowZone;
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ARunningPumpPassesTheInputColourToTheOutput;
    procedure AStoppedPumpLeavesTheOutputEmpty;
    procedure TheStateColoursThePump;
    procedure ADestroyedInputStopsFeedingTheOutput;
    procedure LeavingEveryStateStopsTheBlink;
    procedure TheStateChangeEventFires;
    procedure ATagThatIsNotNumericIsRefused;
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

{ TValveProbe }

function TValveProbe.BodyColorIs:TColor;
begin
  Result:=BodyColor;
end;

function TValveProbe.BorderColorIs:TColor;
begin
  Result:=BorderColor;
end;

function TValveProbe.Blinking:Boolean;
begin
  Result:=Assigned(FZoneTimer) and FZoneTimer.Enabled;
end;

{ TPumpProbe }

function TPumpProbe.BorderColorIs:TColor;
begin
  Result:=BorderColor;
end;

function TPumpProbe.Blinking:Boolean;
begin
  Result:=Assigned(FZoneTimer) and FZoneTimer.Enabled;
end;

{ TTestBasicValve }

procedure TTestBasicValve.SetUp;
begin
  FValve:=THMIBasicValve.Create(nil);
end;

procedure TTestBasicValve.TearDown;
begin
  FreeAndNil(FValve);
end;

procedure TTestBasicValve.ANewValveIsSimpleAndNotMirrored;
begin
  AssertTrue ('valvula simples', FValve.ValveType=vtSimple);
  AssertFalse('sem espelhar',    FValve.Mirrored);
end;

procedure TTestBasicValve.TheValveTypeIsKept;
begin
  FValve.ValveType:=vtPneumaticOnOff;

  AssertTrue('pneumatica on/off', FValve.ValveType=vtPneumaticOnOff);
end;

procedure TTestBasicValve.MirroringIsKept;
begin
  FValve.Mirrored:=true;

  AssertTrue('espelhada', FValve.Mirrored);
end;

procedure TTestBasicValve.TheBodyPercentAcceptsTheWholeRange;
begin
  FValve.ValveBodyPercent:=0;
  AssertEquals('zero', 0, FValve.ValveBodyPercent, 0.0001);

  FValve.ValveBodyPercent:=0.5;
  AssertEquals('meio', 0.5, FValve.ValveBodyPercent, 0.0001);

  FValve.ValveBodyPercent:=1;
  AssertEquals('um', 1, FValve.ValveBodyPercent, 0.0001);
end;

procedure TTestBasicValve.ABodyPercentAboveOneIsRefused;
begin
  FValve.ValveBodyPercent:=0.5;

  try
    FValve.ValveBodyPercent:=1.5;
    Fail('acima de 1 tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('e o valor bom continua la', 0.5, FValve.ValveBodyPercent, 0.0001);
end;

procedure TTestBasicValve.ABodyPercentBelowZeroIsRefused;
begin
  FValve.ValveBodyPercent:=0.5;

  try
    FValve.ValveBodyPercent:=-0.5;
    Fail('abaixo de 0 tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('e o valor bom continua la', 0.5, FValve.ValveBodyPercent, 0.0001);
end;

procedure TTestBasicValve.AGoodPercentIsStillAcceptedAfterARefusedOne;
begin
  //recusar um valor nao pode deixar a propriedade travada
  //refusing a value must not leave the property stuck
  try
    FValve.ValveBodyPercent:=2;
  except
    on Exception do ;
  end;

  FValve.ValveBodyPercent:=0.25;

  AssertEquals('aceitou o valor bom', 0.25, FValve.ValveBodyPercent, 0.0001);
end;

{ TTestFlowValve }

procedure TTestFlowValve.SetUp;
begin
  FValve:=TValveProbe.Create(nil);
  FTag:=TFakeNumber.Create(nil);
  FInput:=THMIFlowPolyline.Create(nil);
  FOutput:=THMIFlowPolyline.Create(nil);
  FValve.InputPolyline:=FInput;
  FValve.OutputPolyline:=FOutput;
  FValve.PLCTag:=FTag;
end;

procedure TTestFlowValve.TearDown;
begin
  FreeAndNil(FValve);
  FreeAndNil(FInput);
  FreeAndNil(FOutput);
  FreeAndNil(FTag);
end;

function TTestFlowValve.NewState(aValue:Double; aFlow:Boolean; aColor, aBorder:TColor):THMIFlowZone;
begin
  Result:=FValve.ColorAndFlowStates.Add;
  Result.ZoneType:=ztEqual;
  Result.SetValues(aValue, aValue);
  Result.Flow:=aFlow;
  Result.Color:=aColor;
  Result.BorderColor:=aBorder;
end;

procedure TTestFlowValve.Settle;
begin
  //o recalculo do estado e' agendado na fila da aplicacao
  //the state recalculation is scheduled on the application queue
  Application.ProcessMessages;
end;

procedure TTestFlowValve.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestFlowValve.AnOpenStatePassesTheInputColourToTheOutput;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('a saida recebeu o fluxo', clRed, FOutput.LineColor);
end;

procedure TTestFlowValve.AClosedStateLeavesTheOutputEmpty;
begin
  NewState(0, false, clRed, clMaroon);
  FInput.LineColor:=clRed;

  TagValueIs(0);

  AssertEquals('valvula fechada, saida vazia', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowValve.TheOutputFollowsTheInputColourChange;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;
  TagValueIs(1);

  FInput.LineColor:=clYellow;
  Settle;

  AssertEquals('a saida acompanhou', clYellow, FOutput.LineColor);
end;

procedure TTestFlowValve.TheTagValueOpensAndClosesTheValve;
begin
  NewState(0, false, clRed,  clMaroon);
  NewState(1, true,  clLime, clGreen);
  FInput.LineColor:=clRed;

  TagValueIs(1);
  AssertEquals('aberta', clRed, FOutput.LineColor);

  TagValueIs(0);
  AssertEquals('fechada', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowValve.ADestroyedInputStopsFeedingTheOutput;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;
  TagValueIs(1);
  AssertEquals('passou', clRed, FOutput.LineColor);

  FreeAndNil(FInput);
  Settle;

  AssertEquals('sem entrada, saida vazia', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowValve.DroppingTheInputStopsFeedingTheOutput;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;
  TagValueIs(1);

  FValve.InputPolyline:=nil;
  Settle;

  AssertEquals('sem entrada, saida vazia', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowValve.TheStateColoursTheValve;
begin
  NewState(1, true, clLime, clGreen);

  TagValueIs(1);

  AssertEquals('o corpo', clLime,  FValve.BodyColorIs);
  AssertEquals('a borda', clGreen, FValve.BorderColorIs);
end;

procedure TTestFlowValve.AStateWithoutBlinkLeavesTheTimerOff;
begin
  NewState(1, true, clLime, clGreen);

  TagValueIs(1);

  AssertFalse('estado parado nao pisca', FValve.Blinking);
end;

procedure TTestFlowValve.AStateWithBlinkTurnsTheTimerOn;
var
  z:THMIFlowZone;
begin
  z:=NewState(1, true, clLime, clGreen);
  NewState(2, true, clYellow, clOlive);
  z.BlinkTime:=100;
  z.BlinkWith:=1;

  TagValueIs(1);

  AssertTrue('estado que pisca liga o temporizador', FValve.Blinking);
end;

procedure TTestFlowValve.LeavingEveryStateStopsTheBlink;
var
  z:THMIFlowZone;
begin
  //sair para um valor sem estado nenhum deixa FCurrentZone em nil; o
  //temporizador nao pode continuar ligado chamando o pisca em cima de nada
  //
  //moving to a value with no state at all leaves FCurrentZone nil; the timer
  //cannot stay on, calling the blink on nothing
  z:=NewState(1, true, clLime, clGreen);
  NewState(2, true, clYellow, clOlive);
  z.BlinkTime:=100;
  z.BlinkWith:=1;
  TagValueIs(1);
  AssertTrue('piscando', FValve.Blinking);

  TagValueIs(99);

  AssertFalse('sem estado, sem pisca', FValve.Blinking);
end;

procedure TTestFlowValve.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  //uma valvula escolhe o estado por um numero. O tag de texto e' um tag de
  //verdade; o unico motivo de recusa possivel e' nao ser numerico
  //
  //a valve picks its state by a number. The text tag is a real tag; the only
  //possible reason to refuse it is not being numeric
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FValve.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FValve.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestFlowValve.ADestroyedTagLetsGoOfTheValve;
begin
  FreeAndNil(FTag);

  AssertTrue('a valvula soltou o tag', FValve.PLCTag=nil);
end;

procedure TTestFlowValve.AssigningTheStatesOfAnotherValveCopiesThem;
var
  outra:TValveProbe;
begin
  NewState(1, true, clLime, clGreen);

  outra:=TValveProbe.Create(nil);
  try
    outra.ColorAndFlowStates:=FValve.ColorAndFlowStates;

    AssertEquals('um estado',  1,       outra.ColorAndFlowStates.Count);
    AssertEquals('a cor',      clLime,  THMIFlowZone(outra.ColorAndFlowStates.Items[0]).Color);
    AssertEquals('a borda',    clGreen, THMIFlowZone(outra.ColorAndFlowStates.Items[0]).BorderColor);
    AssertTrue  ('e o fluxo',  THMIFlowZone(outra.ColorAndFlowStates.Items[0]).Flow);
  finally
    outra.Free;
  end;
end;

procedure TTestFlowValve.WithoutPermissionTheValveIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='abrir_valvula';

    FValve.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitada', TControl(FValve).Enabled);
  finally
    users.Free;
  end;
end;

{ TTestFlowPump }

procedure TTestFlowPump.SetUp;
begin
  FStateChanges:=0;
  FPump:=TPumpProbe.Create(nil);
  FTag:=TFakeNumber.Create(nil);
  FInput:=THMIFlowPolyline.Create(nil);
  FOutput:=THMIFlowPolyline.Create(nil);
  FPump.InputPolyline:=FInput;
  FPump.OutputPolyline:=FOutput;
  FPump.PLCTag:=FTag;
end;

procedure TTestFlowPump.TearDown;
begin
  FreeAndNil(FPump);
  FreeAndNil(FInput);
  FreeAndNil(FOutput);
  FreeAndNil(FTag);
end;

procedure TTestFlowPump.OnStateChanged(Sender:TObject);
begin
  inc(FStateChanges);
end;

function TTestFlowPump.NewState(aValue:Double; aFlow:Boolean; aColor, aBorder:TColor):THMIFlowZone;
begin
  Result:=FPump.ColorAndFlowStates.Add;
  Result.ZoneType:=ztEqual;
  Result.SetValues(aValue, aValue);
  Result.Flow:=aFlow;
  Result.Color:=aColor;
  Result.BorderColor:=aBorder;
end;

procedure TTestFlowPump.Settle;
begin
  Application.ProcessMessages;
end;

procedure TTestFlowPump.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestFlowPump.ARunningPumpPassesTheInputColourToTheOutput;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('a saida recebeu o fluxo', clRed, FOutput.LineColor);
end;

procedure TTestFlowPump.AStoppedPumpLeavesTheOutputEmpty;
begin
  NewState(0, false, clRed, clMaroon);
  FInput.LineColor:=clRed;

  TagValueIs(0);

  AssertEquals('bomba parada, saida vazia', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowPump.TheStateColoursThePump;
begin
  NewState(1, true, clLime, clGreen);

  TagValueIs(1);

  AssertEquals('o corpo', clLime,  FPump.CurrentBodyColor);
  AssertEquals('a borda', clGreen, FPump.BorderColorIs);
end;

procedure TTestFlowPump.ADestroyedInputStopsFeedingTheOutput;
begin
  NewState(1, true, clLime, clGreen);
  FInput.LineColor:=clRed;
  TagValueIs(1);
  AssertEquals('passou', clRed, FOutput.LineColor);

  FreeAndNil(FInput);
  Settle;

  AssertEquals('sem entrada, saida vazia', FOutput.EmptyColor, FOutput.LineColor);
end;

procedure TTestFlowPump.LeavingEveryStateStopsTheBlink;
var
  z:THMIFlowZone;
begin
  z:=NewState(1, true, clLime, clGreen);
  NewState(2, true, clYellow, clOlive);
  z.BlinkTime:=100;
  z.BlinkWith:=1;
  TagValueIs(1);
  AssertTrue('piscando', FPump.Blinking);

  TagValueIs(99);

  AssertFalse('sem estado, sem pisca', FPump.Blinking);
end;

procedure TTestFlowPump.TheStateChangeEventFires;
begin
  FPump.OnStateChange:=@OnStateChanged;
  NewState(0, false, clRed,  clMaroon);
  NewState(1, true,  clLime, clGreen);

  TagValueIs(1);

  AssertTrue('avisou a troca de estado', FStateChanges>0);
end;

procedure TTestFlowPump.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FPump.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FPump.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

initialization
  RegisterTest(TTestBasicValve);
  RegisterTest(TTestFlowValve);
  RegisterTest(TTestFlowPump);

end.
