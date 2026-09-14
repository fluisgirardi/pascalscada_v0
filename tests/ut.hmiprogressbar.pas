{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIProgressBar: a barra que mostra o valor de um tag
            dentro de uma faixa.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um nivel de silo, a abertura de uma valvula, a carga de um motor: a barra
  pega o valor do tag e o coloca entre um minimo e um maximo. So le, nao
  escreve, e nao tem clique.

  O que da' para conferir de fora e' o valor que a barra esta' representando e
  a faixa em que ele cai. A conta que normaliza esse valor para o comprimento
  desenhado vive dentro da rotina de desenho, numa funcao aninhada, e nao
  alcanca teste nenhum - fica registrado aqui para nao se procurar por ela.
}
{$ELSE}
{:
  @abstract(THMIProgressBar tests: the bar showing a tag's value inside a
            range.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A silo level, a valve opening, a motor load: the bar takes the tag's value
  and places it between a minimum and a maximum. It only reads, never writes,
  and has no click.

  What can be checked from outside is the value the bar is representing and
  the range it falls in. The arithmetic normalising that value into the drawn
  length lives inside the paint routine, in a nested function, and is beyond
  the reach of any test - noted here so nobody goes looking for it.
}
{$ENDIF}
unit ut.hmiprogressbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, fpcunit, testregistry,
  HMIProgressBar, PLCTag, PLCString, ControlSecurityManager, CustomizedUserManagement,
  testsupport.faketag;

type

  { TProgressBarProbe }

  //Progress e' protegido: em producao so a rotina de desenho o consulta. A
  //sonda o expoe para que o valor representado possa ser conferido.
  //
  //Progress is protected: in production only the paint routine asks for it.
  //The probe exposes it so the value being shown can be checked.
  TProgressBarProbe = class(THMIProgressBar)
  public
    function CurrentProgress:Double;
  end;

  { TTestHMIProgressBar }

  TTestHMIProgressBar = class(TTestCase)
  private
    FBar:TProgressBarProbe;
    FTag:TFakeNumber;
    procedure TagValueIs(v:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o valor representado / the value being shown
    procedure WithNoTagTheProgressSitsAtTheMinimum;
    procedure WithNoTagTheProgressFollowsTheMinimum;
    procedure TheProgressIsTheTagValue;
    procedure TheProgressFollowsTheTagValue;
    procedure AValueOutsideTheRangeIsStillReported;

    //a faixa / the range
    procedure TheRangeKeepsWhatWasPutInIt;
    procedure TheRangeMayBeInverted;

    //o tag / the tag
    procedure ATagThatIsNotNumericIsRefused;
    procedure ChangingTagsFollowsTheNewOne;
    procedure ADestroyedTagLetsGoOfTheBar;

    //seguranca / security
    procedure WithoutPermissionTheBarIsDisabled;
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

{ TProgressBarProbe }

function TProgressBarProbe.CurrentProgress:Double;
begin
  Result:=Progress;
end;

{ TTestHMIProgressBar }

procedure TTestHMIProgressBar.SetUp;
begin
  FBar:=TProgressBarProbe.Create(nil);
  FBar.Min:=0;
  FBar.Max:=100;
  FTag:=TFakeNumber.Create(nil);
end;

procedure TTestHMIProgressBar.TearDown;
begin
  FreeAndNil(FBar);
  FreeAndNil(FTag);
end;

procedure TTestHMIProgressBar.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  //o aviso do tag e agendado na fila da aplicacao
  Application.ProcessMessages;
end;

procedure TTestHMIProgressBar.WithNoTagTheProgressSitsAtTheMinimum;
begin
  //barra sem tag fica no fundo da escala, nao num valor qualquer
  AssertEquals('no minimo', 0, FBar.CurrentProgress, 0.0001);
end;

procedure TTestHMIProgressBar.WithNoTagTheProgressFollowsTheMinimum;
begin
  FBar.Min:=20;

  AssertEquals('acompanha o minimo novo', 20, FBar.CurrentProgress, 0.0001);
end;

procedure TTestHMIProgressBar.TheProgressIsTheTagValue;
begin
  FBar.PLCTag:=FTag;

  TagValueIs(42.5);

  AssertEquals('o valor do tag', 42.5, FBar.CurrentProgress, 0.0001);
end;

procedure TTestHMIProgressBar.TheProgressFollowsTheTagValue;
begin
  FBar.PLCTag:=FTag;

  TagValueIs(10);
  AssertEquals('primeiro valor', 10, FBar.CurrentProgress, 0.0001);

  TagValueIs(90);
  AssertEquals('segundo valor',  90, FBar.CurrentProgress, 0.0001);
end;

procedure TTestHMIProgressBar.AValueOutsideTheRangeIsStillReported;
begin
  //a barra nao mente sobre o valor: quem limita o desenho e a rotina de
  //pintura, nao a leitura
  FBar.PLCTag:=FTag;

  TagValueIs(150);

  AssertEquals('o valor cru', 150, FBar.CurrentProgress, 0.0001);
end;

procedure TTestHMIProgressBar.TheRangeKeepsWhatWasPutInIt;
begin
  FBar.Min:=-50;
  FBar.Max:=250;

  AssertEquals('minimo', -50, FBar.Min, 0.0001);
  AssertEquals('maximo', 250, FBar.Max, 0.0001);
end;

procedure TTestHMIProgressBar.TheRangeMayBeInverted;
begin
  //nada impede um maximo menor que o minimo; a barra guarda o que se pediu
  FBar.Min:=100;
  FBar.Max:=0;

  AssertEquals('minimo', 100, FBar.Min, 0.0001);
  AssertEquals('maximo', 0,   FBar.Max, 0.0001);
end;

procedure TTestHMIProgressBar.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  //uma barra so sabe desenhar numero. O tag de texto e um tag de verdade, com
  //tudo no lugar - o unico motivo de recusa possivel e nao ser numerico
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FBar.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FBar.PLCTag=nil);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestHMIProgressBar.ChangingTagsFollowsTheNewOne;
var
  outro:TFakeNumber;
begin
  outro:=TFakeNumber.Create(nil);
  try
    FBar.PLCTag:=FTag;
    TagValueIs(10);

    FBar.PLCTag:=outro;
    outro.ChegouDoCLP(80);
    Application.ProcessMessages;
    AssertEquals('segue o tag novo', 80, FBar.CurrentProgress, 0.0001);

    TagValueIs(11);
    AssertEquals('e o antigo nao mexe mais', 80, FBar.CurrentProgress, 0.0001);
  finally
    FBar.PLCTag:=nil;
    outro.Free;
  end;
end;

procedure TTestHMIProgressBar.ADestroyedTagLetsGoOfTheBar;
begin
  FBar.PLCTag:=FTag;

  FreeAndNil(FTag);

  AssertTrue('a barra largou o tag', FBar.PLCTag=nil);
end;

procedure TTestHMIProgressBar.WithoutPermissionTheBarIsDisabled;
var
  users:TUserManagementForTest;
begin
  users:=TUserManagementForTest.Create(nil);
  try
    users.Granted:='ver_producao';

    FBar.SecurityCode:='ver_receita';

    AssertFalse('sem permissao, desabilitado', TControl(FBar).Enabled);
  finally
    users.Free;
  end;
end;

initialization
  RegisterTest(TTestHMIProgressBar);

end.
