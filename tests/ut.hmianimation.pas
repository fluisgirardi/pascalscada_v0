{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIAnimation: o valor de um tag escolhendo o desenho
            que aparece na tela.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Uma valvula aberta, uma valvula fechada, uma valvula em falha: tres imagens,
  tres zonas, e o valor do tag dizendo qual vale. E' o mesmo mecanismo de
  zonas do THMIText, so que o que muda e' a figura em vez da frase.

  O desenho em si precisa de arquivo em disco ou de lista de imagens, mas a
  escolha - que e' o que pode dar errado - e' legivel pela propriedade
  CurrentAnimationZone e pelo evento ZoneChanged. E' isso que se testa aqui.
}
{$ELSE}
{:
  @abstract(THMIAnimation tests: a tag's value choosing the drawing that shows
            on screen.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  An open valve, a closed valve, a faulted valve: three images, three zones,
  and the tag's value saying which one holds. It is the same zone mechanism
  as THMIText, except what changes is the picture rather than the phrase.

  The drawing itself needs a file on disk or an image list, but the choice -
  which is what can go wrong - is readable through the CurrentAnimationZone
  property and the ZoneChanged event. That is what is tested here.
}
{$ENDIF}
unit ut.hmianimation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, fpcunit, testregistry,
  HMIAnimation, hmizones, ControlSecurityManager, CustomizedUserManagement,
  testsupport.faketag;

type

  { TTestHMIAnimation }

  TTestHMIAnimation = class(TTestCase)
  private
    FAnim:THMIAnimation;
    FTag:TFakeNumber;
    FChangeCount:LongInt;
    FLastZoneIndex:Integer;
    procedure OnZoneChanged(Sender:TObject; ZoneIndex:Integer);
    procedure TagValueIs(v:Double);
    procedure Settle;
    function  NewZone(aValue:Double):TGraphicZone;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //criado por codigo / created by code
    procedure EnablingARuntimeCreatedControlKeepsItEnabled;
    //a escolha da zona / choosing the zone
    procedure ANewControlHasNoZoneChosen;
    procedure TheMatchingZoneBecomesTheCurrentOne;
    procedure AnotherValuePicksAnotherZone;
    procedure WithNoMatchAndNoDefaultNoZoneIsChosen;
    procedure TheDefaultZoneAnswersWhenNothingElseMatches;
    procedure WithNoTagTheDefaultZoneIsShown;

    //o aviso de troca / the change notification
    procedure ChangingZoneNotifiesWithTheZoneIndex;
    procedure FallingToNoZoneNotifiesWithMinusOne;

    //o tag / the tag
    procedure ChangingTagsFollowsTheNewOne;
    procedure ADestroyedTagLetsGoOfTheControl;
    procedure ClearingTheTagFallsBackToTheDefaultZone;

    //seguranca / security
    procedure WithoutPermissionTheControlIsDisabled;

    //o encerramento do app / app shutdown
    procedure AQueuedRefreshDoesNothingDuringAppShutdown;
  end;

implementation

type

  { TAnimationAccess }

  //: so' eleva a visibilidade de SimulateAppShutdown (protected em
  //: THMIAnimation) pra publica, pro teste conseguir setar / just raises
  //: SimulateAppShutdown's visibility (protected in THMIAnimation) to
  //: public, so the test can set it
  TAnimationAccess = class(THMIAnimation)
  public
    property SimulateAppShutdown;
  end;

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

{ TTestHMIAnimation }

procedure TTestHMIAnimation.SetUp;
begin
  FChangeCount:=0;
  FLastZoneIndex:=-99;
  FAnim:=THMIAnimation.Create(nil);
  FTag:=TFakeNumber.Create(nil);
end;

procedure TTestHMIAnimation.TearDown;
begin
  FreeAndNil(FAnim);
  FreeAndNil(FTag);
end;

procedure TTestHMIAnimation.OnZoneChanged(Sender:TObject; ZoneIndex:Integer);
begin
  inc(FChangeCount);
  FLastZoneIndex:=ZoneIndex;
end;

procedure TTestHMIAnimation.Settle;
begin
  //o aviso do tag e o da colecao sao agendados na fila da aplicacao
  Application.ProcessMessages;
end;

procedure TTestHMIAnimation.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

function TTestHMIAnimation.NewZone(aValue:Double):TGraphicZone;
begin
  Result:=FAnim.Zones.Add;
  Result.ZoneType:=ztEqual;
  Result.SetValues(aValue, aValue);
end;

procedure TTestHMIAnimation.EnablingARuntimeCreatedControlKeepsItEnabled;
begin
  //sem .lfm e sem codigo de seguranca: desabilitar e reabilitar tem que
  //deixa-lo habilitado. A flag de seguranca nascia falsa, e o E logico
  //com ela desabilitava o controle no primeiro Enabled:=true.
  //with no .lfm and no security code: disabling and re-enabling has to
  //leave it enabled. The security flag was born false, and the logical
  //AND with it disabled the control on the first Enabled:=true.
  FAnim.Enabled:=false;

  FAnim.Enabled:=true;

  AssertTrue('habilitado', TControl(FAnim).Enabled);
end;

procedure TTestHMIAnimation.ANewControlHasNoZoneChosen;
begin
  AssertTrue('nenhuma zona escolhida', FAnim.CurrentAnimationZone=nil);
end;

procedure TTestHMIAnimation.TheMatchingZoneBecomesTheCurrentOne;
var
  fechada, aberta:TGraphicZone;
begin
  fechada:=NewZone(0);
  aberta :=NewZone(1);
  FAnim.PLCTag:=FTag;

  TagValueIs(1);

  AssertSame('a zona do valor 1', aberta, FAnim.CurrentAnimationZone);
end;

procedure TTestHMIAnimation.AnotherValuePicksAnotherZone;
var
  fechada, aberta:TGraphicZone;
begin
  fechada:=NewZone(0);
  aberta :=NewZone(1);
  FAnim.PLCTag:=FTag;

  TagValueIs(1);
  AssertSame('aberta', aberta, FAnim.CurrentAnimationZone);

  TagValueIs(0);
  AssertSame('fechada', fechada, FAnim.CurrentAnimationZone);
end;

procedure TTestHMIAnimation.WithNoMatchAndNoDefaultNoZoneIsChosen;
begin
  NewZone(1);
  FAnim.PLCTag:=FTag;

  TagValueIs(99);

  AssertTrue('nenhuma zona serve', FAnim.CurrentAnimationZone=nil);
end;

procedure TTestHMIAnimation.TheDefaultZoneAnswersWhenNothingElseMatches;
var
  padrao:TGraphicZone;
begin
  NewZone(1);
  padrao:=NewZone(0);
  padrao.DefaultZone:=true;
  FAnim.PLCTag:=FTag;

  TagValueIs(99);

  AssertSame('caiu na padrao', padrao, FAnim.CurrentAnimationZone);
end;

procedure TTestHMIAnimation.WithNoTagTheDefaultZoneIsShown;
var
  padrao:TGraphicZone;
begin
  //sem tag ligado o controle mostra a zona padrao, que e onde se poe o
  //desenho de "sem comunicacao"
  padrao:=NewZone(0);
  padrao.DefaultZone:=true;
  Settle;

  AssertSame('a zona padrao', padrao, FAnim.CurrentAnimationZone);
end;

procedure TTestHMIAnimation.ChangingZoneNotifiesWithTheZoneIndex;
var
  aberta:TGraphicZone;
begin
  NewZone(0);
  aberta:=NewZone(1);
  FAnim.PLCTag:=FTag;
  TagValueIs(0);

  FChangeCount:=0;
  FAnim.ZoneChanged:=@OnZoneChanged;
  TagValueIs(1);

  AssertTrue('avisou a troca', FChangeCount>0);
  AssertEquals('com o indice da zona nova', aberta.Index, FLastZoneIndex);
end;

procedure TTestHMIAnimation.FallingToNoZoneNotifiesWithMinusOne;
begin
  //valor que nao casa com nada e sem zona padrao: o aviso vem com menos um,
  //que e como quem escuta sabe que nao ha desenho a mostrar
  NewZone(1);
  FAnim.PLCTag:=FTag;
  TagValueIs(1);

  FChangeCount:=0;
  FAnim.ZoneChanged:=@OnZoneChanged;
  TagValueIs(99);

  AssertTrue('avisou', FChangeCount>0);
  AssertEquals('menos um', -1, FLastZoneIndex);
end;

procedure TTestHMIAnimation.ChangingTagsFollowsTheNewOne;
var
  outro:TFakeNumber;
  aberta:TGraphicZone;
begin
  outro:=TFakeNumber.Create(nil);
  try
    NewZone(0);
    aberta:=NewZone(1);
    FAnim.PLCTag:=FTag;
    TagValueIs(0);

    FAnim.PLCTag:=outro;
    outro.ChegouDoCLP(1);
    Settle;
    AssertSame('segue o tag novo', aberta, FAnim.CurrentAnimationZone);

    TagValueIs(0);
    AssertSame('e o antigo nao mexe mais', aberta, FAnim.CurrentAnimationZone);
  finally
    FAnim.PLCTag:=nil;
    outro.Free;
  end;
end;

procedure TTestHMIAnimation.ClearingTheTagFallsBackToTheDefaultZone;
var
  aberta, padrao:TGraphicZone;
begin
  //sem tag nao ha' valor: quem responde e' a zona padrao, onde mora o desenho
  //de "sem comunicacao"
  //with no tag there is no value: the default zone answers, which is where the
  //"no communication" drawing lives
  aberta:=NewZone(1);
  padrao:=NewZone(9);
  padrao.DefaultZone:=true;
  FAnim.PLCTag:=FTag;
  TagValueIs(1);
  AssertSame('mostrando a zona do valor', aberta, FAnim.CurrentAnimationZone);

  FAnim.PLCTag:=nil;
  Settle;

  AssertSame('sem tag, a zona padrao', padrao, FAnim.CurrentAnimationZone);
end;

procedure TTestHMIAnimation.ADestroyedTagLetsGoOfTheControl;
begin
  NewZone(1);
  FAnim.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('o controle largou o tag', FAnim.PLCTag=nil);
end;

procedure TTestHMIAnimation.WithoutPermissionTheControlIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='abrir_valvula';

    FAnim.SecurityCode:='parar_motor';

    AssertFalse('sem permissao, desabilitado', TControl(FAnim).Enabled);
  finally
    users.Free;
  end;
end;

procedure TTestHMIAnimation.AQueuedRefreshDoesNothingDuringAppShutdown;
var
  fechada:TGraphicZone;
begin
  //TApplication.Destroy inclui AppDoNotCallAsyncQueue em Flags ANTES de
  //esvaziar a fila de Application.QueueAsyncCall pendente (ver
  //lcl/include/application.inc: Include(FFlags,AppDoNotCallAsyncQueue);
  //ProcessAsyncCallQueue;). E' exatamente essa janela que faz um
  //RefreshAnimation enfileirado durante o funcionamento normal - e nunca
  //processado a tempo - ser atendido so' durante o encerramento do app,
  //quando hmi_animation_timers ja' liberou seu gerenciador de timers
  //global (GetAnimationTimer volta nil) e ShowDefaultZone/SetValue
  //derrubariam o processo. RefreshAnimation tem que virar um no-op nessa
  //janela, sem tocar zona nem temporizador nenhum.
  //
  //Application.Terminated (sem setter publico) e AppDoNotCallAsyncQueue em
  //Application.Flags (TApplication.SetFlags filtra e so' deixa
  //AppNoExceptionMessages passar por Application.Flags:=...) nao tem como
  //ser simulados daqui de fora - por isso o teste usa
  //TAnimationAccess(FAnim).SimulateAppShutdown (protected em THMIAnimation,
  //elevada a public so' pela subclasse TAnimationAccess acima).
  //TApplication.Destroy includes AppDoNotCallAsyncQueue in Flags BEFORE
  //flushing the pending Application.QueueAsyncCall queue (see
  //lcl/include/application.inc: Include(FFlags,AppDoNotCallAsyncQueue);
  //ProcessAsyncCallQueue;). That is exactly the window that lets a
  //RefreshAnimation queued during normal operation - and never processed in
  //time - only get handled during app shutdown, when hmi_animation_timers
  //has already released its global timer manager (GetAnimationTimer comes
  //back nil) and ShowDefaultZone/SetValue would crash the process.
  //RefreshAnimation must become a no-op in that window, touching neither
  //zone nor timer.
  //
  //Application.Terminated (no public setter) and AppDoNotCallAsyncQueue in
  //Application.Flags (TApplication.SetFlags filters and only lets
  //AppNoExceptionMessages through Application.Flags:=...) can't be
  //simulated from out here - that is why the test uses
  //TAnimationAccess(FAnim).SimulateAppShutdown (protected in THMIAnimation,
  //raised to public only by the TAnimationAccess subclass above).
  fechada:=NewZone(0);
  NewZone(1); //aberta - a zona que RefreshAnimation escolheria se nao fosse barrado / the zone RefreshAnimation would pick if not blocked
  FAnim.PLCTag:=FTag;
  TagValueIs(0);
  AssertSame('zona antes do encerramento', fechada, FAnim.CurrentAnimationZone);

  //o valor muda, mas o aviso que levaria a um RefreshAnimation fica
  //pendente - Settle (Application.ProcessMessages) nao e' chamado aqui de
  //proposito.
  //the value changes, but the notification that would lead to a
  //RefreshAnimation stays pending - Settle (Application.ProcessMessages) is
  //deliberately not called here.
  FTag.ChegouDoCLP(1);

  FChangeCount:=0;
  FAnim.ZoneChanged:=@OnZoneChanged;

  TAnimationAccess(FAnim).SimulateAppShutdown:=true;
  try
    //e' o que TApplication.Destroy faz com qualquer chamada ainda pendente
    //na fila: executa-la diretamente, ja' no meio do encerramento.
    //this is what TApplication.Destroy does with any call still pending in
    //the queue: run it directly, already in the middle of shutting down.
    FAnim.RefreshAnimation(0);
  finally
    TAnimationAccess(FAnim).SimulateAppShutdown:=false;
  end;

  AssertSame('a zona nao mudou', fechada, FAnim.CurrentAnimationZone);
  AssertEquals('nenhum aviso disparado', 0, FChangeCount);
end;

initialization
  RegisterTest(TTestHMIAnimation);

end.
