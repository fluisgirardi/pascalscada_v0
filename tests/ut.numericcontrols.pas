{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes dos tres controles numericos de entrada: a barra, a barra
            de rolagem e o incrementador.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Sao os tres controles em que o operador mexe para mandar um numero ao CLP.
  Os tres tem o mesmo esqueleto: o valor do tag vai para a posicao do
  controle, a posicao volta para o tag quando o operador solta, e o codigo de
  seguranca decide se ele pode mexer.

  O que muda e' o momento do envio - a barra manda quando solta a tecla ou o
  mouse, a barra de rolagem manda durante o arrasto se UpdateOnMove estiver
  ligado, e o incrementador manda a cada clique, somando ou subtraindo o
  incremento do valor que veio do tag.
}
{$ELSE}
{:
  @abstract(Tests of the three numeric input controls: the track bar, the
            scroll bar and the up/down.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  These are the three controls the operator moves to send a number to the PLC.
  All three share the same skeleton: the tag's value goes to the control's
  position, the position goes back to the tag when the operator lets go, and
  the security code decides whether they may touch it at all.

  What differs is when the value is sent - the track bar sends when the key or
  the mouse is released, the scroll bar sends during the drag if UpdateOnMove
  is on, and the up/down sends on every click, adding or subtracting the
  increment from the value that came from the tag.
}
{$ENDIF}
unit ut.numericcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, ComCtrls, StdCtrls, LMessages, LCLType,
  fpcunit, testregistry,
  HMITrackBar, HMIScrollBar, HMIUpDown, PLCTag, PLCString,
  ControlSecurityManager, CustomizedUserManagement, testsupport.faketag;

type

  { TTrackBarProbe }

  //WriteValue e' protegido e so' e' chamado pelo KeyUp/MouseUp do widget. A
  //sonda o expoe para o envio poder ser testado sem um clique de verdade.
  //
  //WriteValue is protected and is only called from the widget's KeyUp/MouseUp.
  //The probe exposes it so the send can be tested without a real click.
  TTrackBarProbe = class(THMITrackBar)
  public
    procedure MoveTo(p:LongInt);
    procedure Touch;
    procedure Send;
  end;

  { TScrollBarProbe }

  TScrollBarProbe = class(THMIScrollBar)
  public
    procedure DragTo(p:LongInt);
    procedure LetGo(p:LongInt);
  end;

  { TUpDownProbe }

  TUpDownProbe = class(THMIUpDown)
  public
    procedure ClickUp;
    procedure ClickDown;
  end;

  { TTestHMITrackBar }

  TTestHMITrackBar = class(TTestCase)
  private
    FBar:TTrackBarProbe;
    FTag:TFakeNumber;
    FBlock:Boolean;
    FSent:Double;
    FSendCount:LongInt;
    procedure BeforeSend(Sender:TObject; Value:Double; var SendIt:Boolean);
    procedure AfterSend(Sender:TObject; Value:Double);
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o que vem do CLP / what comes from the PLC
    procedure TheTagValueMovesTheBar;
    procedure ANewValueFromThePLCMovesTheBarAgain;
    procedure MovingTheBarMarksItAsModified;
    procedure AfterARefreshTheBarIsNotModified;

    //o que vai para o CLP / what goes to the PLC
    procedure SendingWritesThePositionToTheTag;
    procedure TheBeforeEventCanRefuseTheWrite;
    procedure TheAfterEventTellsWhatWasSent;
    procedure WithNoTagSendingWritesNothing;

    //o tag / the tag
    procedure ATagThatIsNotNumericIsRefused;
    procedure ADestroyedTagLetsGoOfTheBar;

    //seguranca / security
    procedure WithoutPermissionTheBarIsDisabled;
    procedure PermissionDoesNotOverrideTheProgramsEnabled;
  end;

  { TTestHMIScrollBar }

  TTestHMIScrollBar = class(TTestCase)
  private
    FBar:TScrollBarProbe;
    FTag:TFakeNumber;
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheTagValueMovesTheScrollBar;
    procedure LettingGoWritesThePositionToTheTag;
    procedure WhileDraggingNothingIsWrittenByDefault;
    procedure WithUpdateOnMoveTheDragItselfWrites;
    procedure WhileDraggingThePLCDoesNotMoveTheThumb;
    procedure AfterLettingGoThePLCMovesItAgain;
    procedure ATagThatIsNotNumericIsRefused;
    procedure ADestroyedTagLetsGoOfTheScrollBar;
    procedure WithoutPermissionTheScrollBarIsDisabled;
  end;

  { TTestHMIUpDown }

  TTestHMIUpDown = class(TTestCase)
  private
    FUpDown:TUpDownProbe;
    FTag:TFakeNumber;
    FBlock:Boolean;
    FSent:Double;
    FSendCount:LongInt;
    procedure BeforeSend(Sender:TObject; Value:Double; var SendIt:Boolean);
    procedure AfterSend(Sender:TObject; Value:Double);
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o incremento / the increment
    procedure TheIncrementIsKept;
    procedure AnIncrementOfZeroIsRefused;
    procedure AnIncrementBelowZeroIsRefused;

    //os cliques / the clicks
    procedure TheTagValueBecomesThePosition;
    procedure ClickingUpWritesTheValuePlusTheIncrement;
    procedure ClickingDownWritesTheValueMinusTheIncrement;
    procedure TheMaximumLimitsWhatIsWritten;
    procedure TheMinimumLimitsWhatIsWritten;
    procedure WithoutTheLimitsTheValueGoesThrough;

    //os avisos / the notifications
    procedure TheBeforeEventCanRefuseTheWrite;
    procedure TheAfterEventTellsWhatWasSent;

    //o tag / the tag
    procedure WithNoTagAClickWritesNothing;
    procedure ATagThatIsNotNumericIsRefused;
    procedure ADestroyedTagLetsGoOfTheUpDown;

    //seguranca / security
    procedure WithoutPermissionTheUpDownIsDisabled;
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

{ TTrackBarProbe }

procedure TTrackBarProbe.MoveTo(p:LongInt);
begin
  //Position do THMITrackBar e' so' de leitura; a do widget e' quem o operador
  //move.
  //THMITrackBar's Position is read only; the widget's is the one the operator
  //moves.
  TTrackBar(Self).Position:=p;
end;

procedure TTrackBarProbe.Touch;
var
  msg:TLMessage;
begin
  //e' o aviso que o widget manda quando o operador move a barra; sem janela
  //de verdade ele nao chega sozinho, e sem ele nada marca Modified.
  //this is the notification the widget sends when the operator moves the bar;
  //with no real window it does not arrive on its own, and without it nothing
  //marks Modified.
  FillChar(msg, SizeOf(msg), 0);
  msg.msg:=LM_CHANGED;
  DoChange(msg);
end;

procedure TTrackBarProbe.Send;
begin
  WriteValue;
end;

{ TScrollBarProbe }

procedure TScrollBarProbe.DragTo(p:LongInt);
var
  pos:LongInt;
begin
  pos:=p;
  Scroll(scPosition, pos);
end;

procedure TScrollBarProbe.LetGo(p:LongInt);
var
  pos:LongInt;
begin
  pos:=p;
  {$IF defined(WIN32) or defined(WIN64)}
  //no Windows quem fecha o arrasto e' o proprio scroll
  //on Windows it is the scroll itself that closes the drag
  Scroll(scEndScroll, pos);
  {$ELSE}
  Scroll(scPosition, pos);
  MouseUp(mbLeft, [], 0, 0);
  {$IFEND}
end;

{ TUpDownProbe }

procedure TUpDownProbe.ClickUp;
begin
  Click(btNext);
end;

procedure TUpDownProbe.ClickDown;
begin
  Click(btPrev);
end;

{ TTestHMITrackBar }

procedure TTestHMITrackBar.SetUp;
begin
  FBlock:=false;
  FSent:=-1;
  FSendCount:=0;
  FBar:=TTrackBarProbe.Create(nil);
  TTrackBar(FBar).Min:=0;
  TTrackBar(FBar).Max:=100;
  FTag:=TFakeNumber.Create(nil);
  FBar.PLCTag:=FTag;
end;

procedure TTestHMITrackBar.TearDown;
begin
  FreeAndNil(FBar);
  FreeAndNil(FTag);
end;

procedure TTestHMITrackBar.BeforeSend(Sender:TObject; Value:Double; var SendIt:Boolean);
begin
  SendIt:=not FBlock;
end;

procedure TTestHMITrackBar.AfterSend(Sender:TObject; Value:Double);
begin
  inc(FSendCount);
  FSent:=Value;
end;

procedure TTestHMITrackBar.Settle;
begin
  //a atualizacao do controle e' agendada na fila da aplicacao
  //the control refresh is scheduled on the application queue
  Application.ProcessMessages;
end;

procedure TTestHMITrackBar.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestHMITrackBar.TheTagValueMovesTheBar;
begin
  TagValueIs(42);

  AssertEquals('a barra foi para o valor do tag', 42, FBar.Position);
end;

procedure TTestHMITrackBar.ANewValueFromThePLCMovesTheBarAgain;
begin
  TagValueIs(42);

  TagValueIs(7);

  AssertEquals('acompanhou o valor novo', 7, FBar.Position);
end;

procedure TTestHMITrackBar.MovingTheBarMarksItAsModified;
begin
  //e' a marca que o KeyUp e o MouseUp consultam para decidir se ha' algo para
  //mandar ao CLP
  //it is the mark KeyUp and MouseUp look at to decide whether there is
  //anything to send to the PLC
  FBar.MoveTo(30);
  FBar.Touch;

  AssertTrue('esta modificada', FBar.Modified);
end;

procedure TTestHMITrackBar.AfterARefreshTheBarIsNotModified;
begin
  //o que veio do CLP nao e' mexida do operador: nao pode voltar como escrita
  //what came from the PLC is not the operator moving it: it must not go back
  //as a write
  FBar.MoveTo(30);
  FBar.Touch;
  AssertTrue('modificada pela mexida', FBar.Modified);

  TagValueIs(42);

  AssertFalse('e nao mais depois da leitura', FBar.Modified);
end;

procedure TTestHMITrackBar.SendingWritesThePositionToTheTag;
begin
  FBar.MoveTo(30);

  FBar.Send;

  AssertEquals('o tag recebeu a posicao', 30, FTag.Value, 0.0001);
end;

procedure TTestHMITrackBar.TheBeforeEventCanRefuseTheWrite;
begin
  TagValueIs(5);
  FBar.BeforeSendAValueToTag:=@BeforeSend;
  FBlock:=true;
  FBar.MoveTo(30);

  FBar.Send;

  AssertEquals('o tag ficou como estava', 5, FTag.Value, 0.0001);
end;

procedure TTestHMITrackBar.TheAfterEventTellsWhatWasSent;
begin
  FBar.BeforeSendAValueToTag:=@BeforeSend;
  FBar.AfterSendValueToTag:=@AfterSend;
  FBar.MoveTo(30);

  FBar.Send;

  AssertEquals('avisou uma vez',    1,  FSendCount);
  AssertEquals('com o que foi sem', 30, FSent, 0.0001);
end;

procedure TTestHMITrackBar.WithNoTagSendingWritesNothing;
begin
  FBar.PLCTag:=nil;
  FBar.AfterSendValueToTag:=@AfterSend;
  FBar.MoveTo(30);

  FBar.Send;

  AssertEquals('nao mandou nada', 0, FSendCount);
end;

procedure TTestHMITrackBar.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FBar.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FBar.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestHMITrackBar.ADestroyedTagLetsGoOfTheBar;
begin
  FreeAndNil(FTag);

  AssertTrue('a barra soltou o tag', FBar.PLCTag=nil);
end;

procedure TTestHMITrackBar.WithoutPermissionTheBarIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='mexer_na_barra';

    FBar.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitada', TControl(FBar).Enabled);
  finally
    users.Free;
  end;
end;

procedure TTestHMITrackBar.PermissionDoesNotOverrideTheProgramsEnabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='mexer_na_barra';
    FBar.SecurityCode:='mexer_na_barra';

    FBar.Enabled:=false;

    AssertFalse('o programa desabilitou', TControl(FBar).Enabled);
  finally
    users.Free;
  end;
end;

{ TTestHMIScrollBar }

procedure TTestHMIScrollBar.SetUp;
begin
  FBar:=TScrollBarProbe.Create(nil);
  TScrollBar(FBar).Min:=0;
  TScrollBar(FBar).Max:=100;
  FTag:=TFakeNumber.Create(nil);
  FBar.PLCTag:=FTag;
end;

procedure TTestHMIScrollBar.TearDown;
begin
  FreeAndNil(FBar);
  FreeAndNil(FTag);
end;

procedure TTestHMIScrollBar.Settle;
begin
  Application.ProcessMessages;
end;

procedure TTestHMIScrollBar.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestHMIScrollBar.TheTagValueMovesTheScrollBar;
begin
  TagValueIs(42);

  AssertEquals('a barra foi para o valor do tag', 42, TScrollBar(FBar).Position);
end;

procedure TTestHMIScrollBar.LettingGoWritesThePositionToTheTag;
begin
  FBar.LetGo(30);

  AssertEquals('o tag recebeu a posicao', 30, FTag.Value, 0.0001);
end;

procedure TTestHMIScrollBar.WhileDraggingNothingIsWrittenByDefault;
var
  i:Integer;
begin
  //sem UpdateOnMove o CLP so' ve o valor quando o operador solta
  //without UpdateOnMove the PLC only sees the value when the operator lets go
  TagValueIs(5);

  for i:=1 to 10 do
    FBar.DragTo(30);

  AssertEquals('o tag ficou como estava', 5, FTag.Value, 0.0001);
end;

procedure TTestHMIScrollBar.WithUpdateOnMoveTheDragItselfWrites;
var
  i:Integer;
begin
  TagValueIs(5);
  FBar.UpdateOnMove:=true;

  for i:=1 to 6 do
    FBar.DragTo(30);

  AssertEquals('o tag acompanhou o arrasto', 30, FTag.Value, 0.0001);
end;

procedure TTestHMIScrollBar.WhileDraggingThePLCDoesNotMoveTheThumb;
begin
  //a barra esta na mao do operador: o valor que chega do CLP nao pode puxar o
  //cursor de onde ele esta' arrastando
  //the bar is in the operator's hand: the value arriving from the PLC must
  //not pull the thumb away from where they are dragging
  TScrollBar(FBar).Position:=30;
  FBar.DragTo(30);

  TagValueIs(5);

  AssertEquals('ficou onde o operador deixou', 30, TScrollBar(FBar).Position);
end;

procedure TTestHMIScrollBar.AfterLettingGoThePLCMovesItAgain;
begin
  //e' o outro lado: a barra nao pode ficar congelada depois do arrasto
  //the other side of it: the bar must not stay frozen after the drag
  TScrollBar(FBar).Position:=30;
  FBar.DragTo(30);
  FBar.LetGo(30);

  TagValueIs(5);

  AssertEquals('voltou a seguir o CLP', 5, TScrollBar(FBar).Position);
end;

procedure TTestHMIScrollBar.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FBar.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FBar.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestHMIScrollBar.ADestroyedTagLetsGoOfTheScrollBar;
begin
  FreeAndNil(FTag);

  AssertTrue('a barra soltou o tag', FBar.PLCTag=nil);
end;

procedure TTestHMIScrollBar.WithoutPermissionTheScrollBarIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='mexer_na_barra';

    FBar.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitada', TControl(FBar).Enabled);
  finally
    users.Free;
  end;
end;

{ TTestHMIUpDown }

procedure TTestHMIUpDown.SetUp;
begin
  FBlock:=false;
  FSent:=-1;
  FSendCount:=0;
  FUpDown:=TUpDownProbe.Create(nil);
  FTag:=TFakeNumber.Create(nil);
  FUpDown.PLCTag:=FTag;
end;

procedure TTestHMIUpDown.TearDown;
begin
  FreeAndNil(FUpDown);
  FreeAndNil(FTag);
end;

procedure TTestHMIUpDown.BeforeSend(Sender:TObject; Value:Double; var SendIt:Boolean);
begin
  SendIt:=not FBlock;
end;

procedure TTestHMIUpDown.AfterSend(Sender:TObject; Value:Double);
begin
  inc(FSendCount);
  FSent:=Value;
end;

procedure TTestHMIUpDown.Settle;
begin
  Application.ProcessMessages;
end;

procedure TTestHMIUpDown.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestHMIUpDown.TheIncrementIsKept;
begin
  FUpDown.Increment:=5;

  AssertEquals('o incremento', 5, FUpDown.Increment, 0.0001);
end;

procedure TTestHMIUpDown.AnIncrementOfZeroIsRefused;
begin
  //um incremento de zero deixa os dois botoes sem efeito nenhum
  //an increment of zero leaves both buttons with no effect at all
  FUpDown.Increment:=5;

  try
    FUpDown.Increment:=0;
    Fail('incremento zero tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('e o incremento bom continua la', 5, FUpDown.Increment, 0.0001);
end;

procedure TTestHMIUpDown.AnIncrementBelowZeroIsRefused;
begin
  FUpDown.Increment:=5;

  try
    FUpDown.Increment:=-2;
    Fail('incremento negativo tem que ser recusado');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;

  AssertEquals('e o incremento bom continua la', 5, FUpDown.Increment, 0.0001);
end;

procedure TTestHMIUpDown.TheTagValueBecomesThePosition;
begin
  TagValueIs(10);

  AssertEquals('a posicao e o valor do tag', 10, FUpDown.Position, 0.0001);
end;

procedure TTestHMIUpDown.ClickingUpWritesTheValuePlusTheIncrement;
begin
  FUpDown.Increment:=5;
  TagValueIs(10);

  FUpDown.ClickUp;

  AssertEquals('somou o incremento', 15, FTag.Value, 0.0001);
end;

procedure TTestHMIUpDown.ClickingDownWritesTheValueMinusTheIncrement;
begin
  FUpDown.Increment:=5;
  TagValueIs(10);

  FUpDown.ClickDown;

  AssertEquals('subtraiu o incremento', 5, FTag.Value, 0.0001);
end;

procedure TTestHMIUpDown.TheMaximumLimitsWhatIsWritten;
begin
  FUpDown.Increment:=5;
  FUpDown.Max:=12;
  FUpDown.EnableMax:=true;
  TagValueIs(10);

  FUpDown.ClickUp;

  AssertEquals('parou no maximo', 12, FTag.Value, 0.0001);
end;

procedure TTestHMIUpDown.TheMinimumLimitsWhatIsWritten;
begin
  FUpDown.Increment:=5;
  FUpDown.Max:=100;
  FUpDown.Min:=8;
  FUpDown.EnableMin:=true;
  TagValueIs(10);

  FUpDown.ClickDown;

  AssertEquals('parou no minimo', 8, FTag.Value, 0.0001);
end;

procedure TTestHMIUpDown.WithoutTheLimitsTheValueGoesThrough;
begin
  FUpDown.Increment:=5;
  FUpDown.Max:=12;
  TagValueIs(10);

  FUpDown.ClickUp;

  AssertEquals('o limite desligado nao segura', 15, FTag.Value, 0.0001);
end;

procedure TTestHMIUpDown.TheBeforeEventCanRefuseTheWrite;
begin
  FUpDown.Increment:=5;
  TagValueIs(10);
  FUpDown.BeforeSendAValueToTag:=@BeforeSend;
  FBlock:=true;

  FUpDown.ClickUp;

  AssertEquals('o tag ficou como estava', 10, FTag.Value, 0.0001);
end;

procedure TTestHMIUpDown.TheAfterEventTellsWhatWasSent;
begin
  FUpDown.Increment:=5;
  TagValueIs(10);
  FUpDown.BeforeSendAValueToTag:=@BeforeSend;
  FUpDown.AfterSendValueToTag:=@AfterSend;

  FUpDown.ClickUp;

  AssertEquals('avisou uma vez',     1,  FSendCount);
  AssertEquals('com o que foi sem',  15, FSent, 0.0001);
end;

procedure TTestHMIUpDown.WithNoTagAClickWritesNothing;
begin
  FUpDown.Increment:=5;
  FUpDown.PLCTag:=nil;
  FUpDown.AfterSendValueToTag:=@AfterSend;

  FUpDown.ClickUp;

  AssertEquals('nao mandou nada', 0, FSendCount);
end;

procedure TTestHMIUpDown.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FUpDown.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FUpDown.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestHMIUpDown.ADestroyedTagLetsGoOfTheUpDown;
begin
  FreeAndNil(FTag);

  AssertTrue('o controle soltou o tag', FUpDown.PLCTag=nil);
end;

procedure TTestHMIUpDown.WithoutPermissionTheUpDownIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='mexer_no_controle';

    FUpDown.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitado', TControl(FUpDown).Enabled);
  finally
    users.Free;
  end;
end;

initialization
  RegisterTest(TTestHMITrackBar);
  RegisterTest(TTestHMIScrollBar);
  RegisterTest(TTestHMIUpDown);

end.
