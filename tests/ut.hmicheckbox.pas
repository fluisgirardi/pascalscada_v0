{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMICheckBox: o vinculo de duas maos entre um tag e uma
            caixa de marcacao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  E' o controle mais simples que escreve no equipamento, e por isso o que mais
  merece cuidado: o operador clica e um valor sai para o CLP. Dois valores
  ficam configurados - o que representa marcado e o que representa
  desmarcado - e tudo que nao for nenhum dos dois cai numa terceira regra,
  escolhida em OtherValues.

  As duas maos sao independentes: da' para ter uma caixa que mostra o estado
  mas nao escreve ao ser clicada, desligando WriteTrue ou WriteFalse. E ha um
  gancho de veto antes de cada escrita, que e' onde um programa pede
  confirmacao ao operador.
}
{$ELSE}
{:
  @abstract(THMICheckBox tests: the two way link between a tag and a check
            box.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It is the simplest control that writes to the equipment, and for that
  reason the one that deserves most care: the operator clicks and a value
  leaves for the PLC. Two values are configured - the one that means checked
  and the one that means unchecked - and anything that is neither falls to a
  third rule, chosen in OtherValues.

  The two directions are independent: a box can show the state without
  writing when clicked, by turning WriteTrue or WriteFalse off. And there is a
  veto hook before every write, which is where a program asks the operator to
  confirm.
}
{$ENDIF}
unit ut.hmicheckbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, fpcunit, testregistry,
  HMICheckBox, HMITypes,
  testsupport.faketag;

type

  { TTestHMICheckBox }

  TTestHMICheckBox = class(TTestCase)
  private
    FBox:THMICheckBox;
    FTag:TFakeNumber;
    FBeforeCount, FAfterCount:LongInt;
    FLastSent:Double;
    FLetItGo:Boolean;

    procedure BeforeSend(Sender:TObject; aValue:Double; var aAllow:Boolean);
    procedure AfterSend(Sender:TObject; aValue:Double);
    //: entrega o valor ao controle como a varredura faria
    procedure TagValueIs(v:Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //criado por codigo / created by code
    procedure EnablingARuntimeCreatedControlKeepsItEnabled;
    //leitura: do tag para a caixa / reading: from the tag to the box
    procedure TheTrueValueChecksTheBox;
    procedure TheFalseValueUnchecksTheBox;
    procedure CheckedReadsTheTagNotTheControl;
    procedure WithNoTagTheBoxKeepsItsOwnState;
    procedure LosingTheTagShowsTheUnknownState;
    procedure ADestroyedTagShowsTheUnknownState;

    //os outros valores / the other values
    procedure AnotherValueCanCheckTheBox;
    procedure AnotherValueCanUncheckTheBox;
    procedure AnotherValueCanGreyTheBox;

    //aparencia por estado / look per state
    procedure EachStateBringsItsOwnCaption;
    procedure EachStateBringsItsOwnColour;

    //escrita: da caixa para o tag / writing: from the box to the tag
    procedure CheckingTheBoxWritesTheTrueValue;
    procedure UncheckingTheBoxWritesTheFalseValue;
    procedure WithWriteTrueOffCheckingWritesNothing;
    procedure WithWriteFalseOffUncheckingWritesNothing;

    //ganchos de escrita / write hooks
    procedure TheBeforeHookSeesTheValueAboutToGo;
    procedure TheBeforeHookCanVetoTheWrite;
    procedure TheAfterHookRunsOnceTheValueWentOut;
    procedure ASingleChangeSendsTheValueOnce;

    //configuracao / configuration
    procedure TheTwoValuesCannotBeTheSame;
    procedure OutOfTheDesignerBothValuesStartAtZero;

    //desligamento / detaching
    procedure ADestroyedTagLetsGoOfTheBox;
  end;

implementation

{ TTestHMICheckBox }

procedure TTestHMICheckBox.SetUp;
begin
  FBeforeCount:=0;
  FAfterCount:=0;
  FLastSent:=0;
  FLetItGo:=true;

  FBox:=THMICheckBox.Create(nil);
  //fora do designer os dois nascem valendo zero, e um setter recusa deixa-los
  //iguais; o ValueTrue tem que sair do zero antes
  FBox.ValueTrue:=1;
  FBox.ValueFalse:=0;
  FTag:=TFakeNumber.Create(nil);
  FBox.PLCTag:=FTag;
end;

procedure TTestHMICheckBox.TearDown;
begin
  FreeAndNil(FBox);
  FreeAndNil(FTag);
end;

procedure TTestHMICheckBox.BeforeSend(Sender:TObject; aValue:Double; var aAllow:Boolean);
begin
  inc(FBeforeCount);
  FLastSent:=aValue;
  aAllow:=FLetItGo;
end;

procedure TTestHMICheckBox.AfterSend(Sender:TObject; aValue:Double);
begin
  inc(FAfterCount);
  FLastSent:=aValue;
end;

procedure TTestHMICheckBox.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  //o aviso do tag agenda a atualizacao na fila da aplicacao; num teste de
  //console e' preciso drenar essa fila na mao
  Application.ProcessMessages;
end;

procedure TTestHMICheckBox.EnablingARuntimeCreatedControlKeepsItEnabled;
begin
  //sem .lfm e sem codigo de seguranca: desabilitar e reabilitar tem que
  //deixa-lo habilitado. A flag de seguranca nascia falsa, e o E logico
  //com ela desabilitava o controle no primeiro Enabled:=true.
  //with no .lfm and no security code: disabling and re-enabling has to
  //leave it enabled. The security flag was born false, and the logical
  //AND with it disabled the control on the first Enabled:=true.
  FBox.Enabled:=false;

  FBox.Enabled:=true;

  AssertTrue('habilitado', TControl(FBox).Enabled);
end;

procedure TTestHMICheckBox.TheTrueValueChecksTheBox;
begin
  TagValueIs(1);

  AssertEquals('marcada', Ord(cbChecked), Ord(FBox.State));
end;

procedure TTestHMICheckBox.TheFalseValueUnchecksTheBox;
begin
  TagValueIs(1);
  TagValueIs(0);

  AssertEquals('desmarcada', Ord(cbUnchecked), Ord(FBox.State));
end;

procedure TTestHMICheckBox.CheckedReadsTheTagNotTheControl;
begin
  //com tag ligado, quem responde pelo Checked e o valor do tag
  TagValueIs(1);
  AssertTrue('o tag vale o valor de marcado', FBox.Checked);

  TagValueIs(0);
  AssertFalse('e agora o de desmarcado', FBox.Checked);
end;

procedure TTestHMICheckBox.WithNoTagTheBoxKeepsItsOwnState;
var
  solta:THMICheckBox;
begin
  solta:=THMICheckBox.Create(nil);
  try
    solta.Checked:=true;
    AssertTrue('sem tag, o estado e o do proprio controle', solta.Checked);
  finally
    solta.Free;
  end;
end;

procedure TTestHMICheckBox.LosingTheTagShowsTheUnknownState;
begin
  //sem tag nao ha' leitura: o estado passa a ser o mesmo de um valor que nao
  //e' nem o de marcado nem o de desmarcado
  //with no tag there is no reading: the state becomes the same as a value that
  //is neither the checked nor the unchecked one
  FBox.OtherValuesIS:=IsGrayed;
  TagValueIs(1);
  AssertEquals('marcado pelo tag', Ord(cbChecked), Ord(FBox.State));

  FBox.PLCTag:=nil;

  AssertEquals('sem tag, indefinido', Ord(cbGrayed), Ord(FBox.State));
end;

procedure TTestHMICheckBox.ADestroyedTagShowsTheUnknownState;
begin
  FBox.OtherValuesIS:=IsGrayed;
  TagValueIs(1);

  FreeAndNil(FTag);

  AssertEquals('sem tag, indefinido', Ord(cbGrayed), Ord(FBox.State));
end;

procedure TTestHMICheckBox.AnotherValueCanCheckTheBox;
begin
  FBox.OtherValuesIS:=isChecked;

  TagValueIs(99);

  AssertEquals('valor desconhecido marca', Ord(cbChecked), Ord(FBox.State));
end;

procedure TTestHMICheckBox.AnotherValueCanUncheckTheBox;
begin
  FBox.OtherValuesIS:=isUnchecked;

  TagValueIs(99);

  AssertEquals('valor desconhecido desmarca', Ord(cbUnchecked), Ord(FBox.State));
end;

procedure TTestHMICheckBox.AnotherValueCanGreyTheBox;
begin
  //o estado cinza e o jeito de dizer "nao sei", que e o mais honesto quando o
  //valor nao e nenhum dos dois esperados
  FBox.OtherValuesIS:=IsGrayed;

  TagValueIs(99);

  AssertEquals('valor desconhecido fica cinza', Ord(cbGrayed), Ord(FBox.State));
end;

procedure TTestHMICheckBox.EachStateBringsItsOwnCaption;
begin
  FBox.CaptionTrue :='ligado';
  FBox.CaptionFalse:='desligado';

  TagValueIs(1);
  AssertEquals('texto de marcado', 'ligado', FBox.Caption);

  TagValueIs(0);
  AssertEquals('texto de desmarcado', 'desligado', FBox.Caption);
end;

procedure TTestHMICheckBox.EachStateBringsItsOwnColour;
begin
  FBox.ColorTrue :=clLime;
  FBox.ColorFalse:=clRed;

  TagValueIs(1);
  AssertEquals('cor de marcado', clLime, FBox.Color);

  TagValueIs(0);
  AssertEquals('cor de desmarcado', clRed, FBox.Color);
end;

procedure TTestHMICheckBox.CheckingTheBoxWritesTheTrueValue;
begin
  FBox.Checked:=true;

  AssertEquals('o valor de marcado saiu para o tag', 1, FTag.Value, 0.0001);
end;

procedure TTestHMICheckBox.UncheckingTheBoxWritesTheFalseValue;
begin
  FBox.Checked:=true;
  FBox.Checked:=false;

  AssertEquals('o valor de desmarcado saiu', 0, FTag.Value, 0.0001);
end;

procedure TTestHMICheckBox.WithWriteTrueOffCheckingWritesNothing;
begin
  //caixa que mostra mas nao comanda na subida
  FBox.WriteTrueValue:=false;
  TagValueIs(0);

  FBox.Checked:=true;

  AssertEquals('o tag ficou como estava', 0, FTag.Value, 0.0001);
end;

procedure TTestHMICheckBox.WithWriteFalseOffUncheckingWritesNothing;
begin
  FBox.WriteFalseValue:=false;
  TagValueIs(1);

  FBox.Checked:=false;

  AssertEquals('o tag ficou como estava', 1, FTag.Value, 0.0001);
end;

procedure TTestHMICheckBox.TheBeforeHookSeesTheValueAboutToGo;
begin
  FBox.BeforeSendAValueToTag:=@BeforeSend;

  FBox.Checked:=true;

  AssertEquals('o gancho foi chamado uma vez', 1, FBeforeCount);
  AssertEquals('e viu o valor de marcado', 1, FLastSent, 0.0001);
end;

procedure TTestHMICheckBox.TheBeforeHookCanVetoTheWrite;
begin
  //e onde um programa pede confirmacao antes de mexer no equipamento
  FBox.BeforeSendAValueToTag:=@BeforeSend;
  FLetItGo:=false;
  TagValueIs(0);

  FBox.Checked:=true;

  AssertEquals('o gancho foi consultado uma vez', 1, FBeforeCount);
  AssertEquals('e o valor nao saiu', 0, FTag.Value, 0.0001);
end;

procedure TTestHMICheckBox.TheAfterHookRunsOnceTheValueWentOut;
begin
  FBox.AfterSendValueToTag:=@AfterSend;

  FBox.Checked:=true;

  AssertEquals('o gancho de depois foi chamado uma vez', 1, FAfterCount);
  AssertEquals('com o valor que saiu', 1, FLastSent, 0.0001);
end;

procedure TTestHMICheckBox.ASingleChangeSendsTheValueOnce;
begin
  //o operador que pede confirmacao nao pode ser perguntado duas vezes, e o
  //CLP nao pode receber o comando em dobro
  FBox.BeforeSendAValueToTag:=@BeforeSend;
  FBox.AfterSendValueToTag:=@AfterSend;

  FBox.Checked:=true;

  AssertEquals('um pedido de confirmacao', 1, FBeforeCount);
  AssertEquals('e um aviso de envio',      1, FAfterCount);
end;

procedure TTestHMICheckBox.TheTwoValuesCannotBeTheSame;
begin
  //dois valores iguais deixariam o controle sem como distinguir os estados
  try
    FBox.ValueTrue:=FBox.ValueFalse;
    Fail('valores iguais tem que ser recusados');
  except
    on EAssertionFailedError do raise;
    on Exception do ;
  end;
end;

procedure TTestHMICheckBox.OutOfTheDesignerBothValuesStartAtZero;
var
  nova:THMICheckBox;
begin
  //no designer o controle nasce com 1 e 0; criado por codigo nasce com os
  //dois zerados, e ai' a ordem de configuracao importa - por o ValueFalse
  //primeiro esbarra no proprio zero do ValueTrue
  nova:=THMICheckBox.Create(nil);
  try
    AssertEquals('marcado nasce zero',   0, nova.ValueTrue,  0.0001);
    AssertEquals('desmarcado tambem',    0, nova.ValueFalse, 0.0001);

    try
      nova.ValueFalse:=0;
      Fail('por o ValueFalse igual ao ValueTrue tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    nova.ValueTrue:=1;
    nova.ValueFalse:=0;
    AssertEquals('configurado na ordem certa', 1, nova.ValueTrue, 0.0001);
  finally
    nova.Free;
  end;
end;

procedure TTestHMICheckBox.ADestroyedTagLetsGoOfTheBox;
begin
  FreeAndNil(FTag);

  AssertTrue('a caixa largou o tag', FBox.PLCTag=nil);
end;

initialization
  RegisterTest(TTestHMICheckBox);

end.
