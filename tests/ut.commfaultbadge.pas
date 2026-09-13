{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do contador de falha de comunicacao:
            THMITagFaultBadgeLink.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O selo de falha que aparece sobre um controle nao e' ligado e desligado
  diretamente: existe um contador, e cada vinculo tag-controle sobe ou desce
  esse contador uma unica vez por transicao. E' o que permite um controle
  vinculado a varios tags mostrar o selo enquanto qualquer um deles estiver
  ruim, e so apagar quando o ultimo voltar.

  A conta tem que fechar exatamente. Um IncFault a mais e o selo fica aceso
  para sempre num equipamento que esta bom; um a menos e o operador deixa de
  ver que perdeu a comunicacao. Nada disso desenha nada - o desenho e' de
  quem le o contador.
}
{$ELSE}
{:
  @abstract(Communication fault counter tests: THMITagFaultBadgeLink.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The fault badge shown over a control is not switched on and off directly:
  there is a counter, and every tag-to-control link raises or lowers it
  exactly once per transition. That is what lets a control bound to several
  tags show the badge while any one of them is unhealthy, and clear it only
  when the last one recovers.

  The arithmetic has to balance exactly. One IncFault too many and the badge
  stays lit over healthy equipment; one too few and the operator never sees
  that communication was lost. None of this draws anything - drawing belongs
  to whoever reads the counter.
}
{$ENDIF}
unit ut.commfaultbadge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Tag, hmi_commfaultbadge,
  testsupport.faketag;

type

  { TCounterProbe }

  //conta as subidas e descidas no lugar do selo de verdade
  //counts the ups and downs in place of the real badge
  TCounterProbe = class(THMIFaultCounter)
  private
    FUps, FDowns:LongInt;
  public
    procedure IncFault; override;
    procedure DecFault; override;
    //: quantas vezes subiu menos quantas desceu / ups minus downs
    function  Balance:LongInt;
    property  Ups:LongInt read FUps;
    property  Downs:LongInt read FDowns;
  end;

  { TTagWithFaults }

  //tag falso com os avisos de falha na mao, e com o estado de leitura e
  //escrita que o vinculo consulta ao se pendurar
  //fake tag with the fault notifications at hand, and with the read and write
  //status the link asks for when it attaches
  TTagWithFaults = class(TFakeNumber)
  private
    FReadStatus, FWriteStatus:TProtocolIOResult;
  protected
    function GetLastAsyncReadStatus: TProtocolIOResult;  override;
    function GetLastAsyncWriteStatus: TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
    procedure FireReadFault;
    procedure FireReadOk;
    procedure FireWriteFault;
    procedure FireWriteOk;
    property  ReadStatus:TProtocolIOResult  read FReadStatus  write FReadStatus;
    property  WriteStatus:TProtocolIOResult read FWriteStatus write FWriteStatus;
  end;

  { TTestCommFaultBadge }

  TTestCommFaultBadge = class(TTestCase)
  private
    FCounter:TCounterProbe;
    FLink:THMITagFaultBadgeLink;
    FTag:TTagWithFaults;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //vinculo / attaching
    procedure ANewLinkTouchesNoCounter;
    procedure AttachingAHealthyTagRaisesNothing;
    procedure AttachingAnAlreadyFaultedTagRaisesItAtOnce;
    procedure AttachingATagWithAWriteFaultAlsoRaisesIt;
    procedure AttachingTheSameTagTwiceDoesNothing;

    //transicoes / transitions
    procedure AReadFaultRaisesTheCounter;
    procedure ASecondReadFaultDoesNotRaiseItAgain;
    procedure AReadOkLowersItBack;
    procedure AReadOkWithNoFaultRaisesNothing;
    procedure AWriteFaultRaisesTheCounterToo;
    procedure WithBothSidesFaultedAReadOkKeepsItRaised;
    procedure AndTheWriteOkThenLowersIt;
    procedure ARoundTripLeavesTheCounterBalanced;

    //desligamento / detaching
    procedure ChangingTagsLetsGoOfTheOldOne;
    procedure ADetachedTagStopsCountingAltogether;
    procedure ADestroyedTagClearsTheFault;
    procedure DestroyingTheLinkWhileFaultedLowersTheCounter;
    procedure ALinkWithNoCounterDoesNotCrash;
  end;

implementation

{ TCounterProbe }

procedure TCounterProbe.IncFault;
begin
  inc(FUps);
end;

procedure TCounterProbe.DecFault;
begin
  inc(FDowns);
end;

function TCounterProbe.Balance:LongInt;
begin
  Result:=FUps-FDowns;
end;

{ TTagWithFaults }

constructor TTagWithFaults.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FReadStatus :=ioOk;
  FWriteStatus:=ioOk;
end;

function TTagWithFaults.GetLastAsyncReadStatus: TProtocolIOResult;
begin
  Result:=FReadStatus;
end;

function TTagWithFaults.GetLastAsyncWriteStatus: TProtocolIOResult;
begin
  Result:=FWriteStatus;
end;

procedure TTagWithFaults.FireReadFault;
begin
  FReadStatus:=ioTimeOut;
  NotifyReadFault;
end;

procedure TTagWithFaults.FireReadOk;
begin
  FReadStatus:=ioOk;
  NotifyReadOk;
end;

procedure TTagWithFaults.FireWriteFault;
begin
  FWriteStatus:=ioTimeOut;
  NotifyWriteFault;
end;

procedure TTagWithFaults.FireWriteOk;
begin
  FWriteStatus:=ioOk;
  NotifyWriteOk;
end;

{ TTestCommFaultBadge }

procedure TTestCommFaultBadge.SetUp;
begin
  FCounter:=TCounterProbe.Create;
  FLink:=THMITagFaultBadgeLink.Create(FCounter);
  FTag:=TTagWithFaults.Create(nil);
end;

procedure TTestCommFaultBadge.TearDown;
begin
  FreeAndNil(FLink);
  FreeAndNil(FTag);
  FreeAndNil(FCounter);
end;

procedure TTestCommFaultBadge.ANewLinkTouchesNoCounter;
begin
  AssertEquals('nada subiu',  0, FCounter.Ups);
  AssertEquals('nada desceu', 0, FCounter.Downs);
end;

procedure TTestCommFaultBadge.AttachingAHealthyTagRaisesNothing;
begin
  FLink.SetTag(FTag);

  AssertEquals('tag bom nao acende o selo', 0, FCounter.Balance);
  AssertEquals('e nao chamou ninguem',      0, FCounter.Ups);
end;

procedure TTestCommFaultBadge.AttachingAnAlreadyFaultedTagRaisesItAtOnce;
begin
  //o controle pode nascer depois do equipamento ja ter caido; o selo tem que
  //aparecer sem esperar o proximo aviso
  FTag.ReadStatus:=ioTimeOut;

  FLink.SetTag(FTag);

  AssertEquals('o selo subiu na hora', 1, FCounter.Balance);
end;

procedure TTestCommFaultBadge.AttachingATagWithAWriteFaultAlsoRaisesIt;
begin
  FTag.WriteStatus:=ioTimeOut;

  FLink.SetTag(FTag);

  AssertEquals('falha de escrita tambem conta', 1, FCounter.Balance);
end;

procedure TTestCommFaultBadge.AttachingTheSameTagTwiceDoesNothing;
begin
  FTag.ReadStatus:=ioTimeOut;
  FLink.SetTag(FTag);

  FLink.SetTag(FTag);

  AssertEquals('nao contou duas vezes', 1, FCounter.Ups);
end;

procedure TTestCommFaultBadge.AReadFaultRaisesTheCounter;
begin
  FLink.SetTag(FTag);

  FTag.FireReadFault;

  AssertEquals('subiu uma vez', 1, FCounter.Ups);
  AssertEquals('e o selo esta aceso', 1, FCounter.Balance);
end;

procedure TTestCommFaultBadge.ASecondReadFaultDoesNotRaiseItAgain;
begin
  //cada varredura que falha manda um aviso; se cada um subisse o contador, o
  //selo nunca mais apagaria
  FLink.SetTag(FTag);

  FTag.FireReadFault;
  FTag.FireReadFault;
  FTag.FireReadFault;

  AssertEquals('subiu uma vez so', 1, FCounter.Ups);
end;

procedure TTestCommFaultBadge.AReadOkLowersItBack;
begin
  FLink.SetTag(FTag);
  FTag.FireReadFault;

  FTag.FireReadOk;

  AssertEquals('desceu uma vez', 1, FCounter.Downs);
  AssertEquals('e o selo apagou', 0, FCounter.Balance);
end;

procedure TTestCommFaultBadge.AReadOkWithNoFaultRaisesNothing;
begin
  FLink.SetTag(FTag);

  FTag.FireReadOk;
  FTag.FireReadOk;

  AssertEquals('nada desceu', 0, FCounter.Downs);
end;

procedure TTestCommFaultBadge.AWriteFaultRaisesTheCounterToo;
begin
  FLink.SetTag(FTag);

  FTag.FireWriteFault;

  AssertEquals('o selo acendeu', 1, FCounter.Balance);
end;

procedure TTestCommFaultBadge.WithBothSidesFaultedAReadOkKeepsItRaised;
begin
  //o selo aceso vale para leitura OU escrita ruim; uma voltar nao basta
  FLink.SetTag(FTag);
  FTag.FireReadFault;
  FTag.FireWriteFault;

  FTag.FireReadOk;

  AssertEquals('subiu uma vez so',       1, FCounter.Ups);
  AssertEquals('e continua aceso',       1, FCounter.Balance);
end;

procedure TTestCommFaultBadge.AndTheWriteOkThenLowersIt;
begin
  FLink.SetTag(FTag);
  FTag.FireReadFault;
  FTag.FireWriteFault;
  FTag.FireReadOk;

  FTag.FireWriteOk;

  AssertEquals('agora apagou', 0, FCounter.Balance);
  AssertEquals('e desceu uma vez so', 1, FCounter.Downs);
end;

procedure TTestCommFaultBadge.ARoundTripLeavesTheCounterBalanced;
var
  c:LongInt;
begin
  //dez idas e voltas, e a conta tem que fechar em zero
  FLink.SetTag(FTag);

  for c:=1 to 10 do begin
    FTag.FireReadFault;
    FTag.FireReadOk;
  end;

  AssertEquals('subiu dez',  10, FCounter.Ups);
  AssertEquals('desceu dez', 10, FCounter.Downs);
  AssertEquals('e fechou',    0, FCounter.Balance);
end;

procedure TTestCommFaultBadge.ChangingTagsLetsGoOfTheOldOne;
var
  outro:TTagWithFaults;
begin
  outro:=TTagWithFaults.Create(nil);
  try
    FLink.SetTag(FTag);
    FTag.FireReadFault;
    AssertEquals('aceso pelo primeiro', 1, FCounter.Balance);

    FLink.SetTag(outro);

    AssertEquals('o tag novo esta bom, entao apagou', 0, FCounter.Balance);
  finally
    outro.Free;
  end;
end;

procedure TTestCommFaultBadge.ADetachedTagStopsCountingAltogether;
var
  outro:TTagWithFaults;
begin
  outro:=TTagWithFaults.Create(nil);
  try
    FLink.SetTag(FTag);
    FLink.SetTag(outro);

    //o antigo nao manda mais em nada
    FTag.FireReadFault;

    AssertEquals('o tag antigo nao conta mais', 0, FCounter.Balance);
  finally
    outro.Free;
  end;
end;

procedure TTestCommFaultBadge.ADestroyedTagClearsTheFault;
begin
  FLink.SetTag(FTag);
  FTag.FireReadFault;

  FreeAndNil(FTag);

  AssertEquals('o selo apagou com o tag', 0, FCounter.Balance);
end;

procedure TTestCommFaultBadge.DestroyingTheLinkWhileFaultedLowersTheCounter;
begin
  //o vinculo morrendo com o selo aceso deixaria o contador preso acima de
  //zero, e o selo aceso para sempre
  FLink.SetTag(FTag);
  FTag.FireReadFault;
  AssertEquals('aceso', 1, FCounter.Balance);

  FreeAndNil(FLink);

  AssertEquals('a conta fechou', 0, FCounter.Balance);
end;

procedure TTestCommFaultBadge.ALinkWithNoCounterDoesNotCrash;
var
  solto:THMITagFaultBadgeLink;
begin
  solto:=THMITagFaultBadgeLink.Create(nil);
  try
    solto.SetTag(FTag);
    FTag.FireReadFault;
    FTag.FireReadOk;
  finally
    solto.Free;
  end;
end;

initialization
  RegisterTest(TTestCommFaultBadge);

end.
