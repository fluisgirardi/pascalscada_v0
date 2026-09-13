{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TTag: as listas de ouvintes e o aviso de remocao.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Todo tag guarda seis listas de ouvintes - leitura ok, falha de leitura,
  escrita ok, falha de escrita, mudanca de valor e remocao do tag. A ultima e'
  a delicada: e' avisada de dentro do destrutor, e o que um ouvinte
  costumeiramente faz ao receber esse aviso e' largar os seus ganchos, o que
  encurta a propria lista que esta' sendo percorrida.
}
{$ELSE}
{:
  @abstract(TTag tests: the listener lists and the removal notification.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Every tag keeps six listener lists - read ok, read fault, write ok, write
  fault, value change and tag removal. The last one is the delicate one: it is
  fired from inside the destructor, and what a listener customarily does on
  receiving it is to drop its handlers, which shrinks the very list being
  walked.
}
{$ENDIF}
unit ut.tag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Tag;

type

  { TSimpleTag }

  //: um tag concreto qualquer: TTag nao tem nada de abstrato a implementar
  TSimpleTag = class(TTag);

  { TListener }

  {$IFDEF PORTUGUES}
  //: Ouvinte que, ao saber que o tag vai embora, larga os seus ganchos - que
  //  e' o que RemoveAllHandlersFromObject existe para fazer.
  {$ELSE}
  //: A listener that, on learning the tag is going away, drops its handlers -
  //  which is what RemoveAllHandlersFromObject is there for.
  {$ENDIF}
  TListener = class
  private
    FTag:TTag;
    FAvisado:Boolean;
    FLargarOsGanchos:Boolean;
  public
    constructor Create(aTag:TTag; aLargarOsGanchos:Boolean);
    procedure RemovalNotice(Sender:TObject);
    property  Avisado:Boolean read FAvisado;
  end;

  { TTestTag }

  TTestTag = class(TTestCase)
  published
    procedure TheRemovalNoticeReachesTheListener;
    procedure AListenerThatDropsItsHooksDoesNotHideTheOthers;
    procedure HooksDroppedEarlierAreNotNotified;
  end;

implementation

{ TListener }

constructor TListener.Create(aTag:TTag; aLargarOsGanchos:Boolean);
begin
  inherited Create;
  FTag:=aTag;
  FAvisado:=false;
  FLargarOsGanchos:=aLargarOsGanchos;
  FTag.AddRemoveTagHandler(@RemovalNotice);
end;

procedure TListener.RemovalNotice(Sender:TObject);
begin
  FAvisado:=true;
  if FLargarOsGanchos and (FTag<>nil) then begin
    FTag.RemoveAllHandlersFromObject(Self);
    FTag:=nil;
  end;
end;

{ TTestTag }

procedure TTestTag.TheRemovalNoticeReachesTheListener;
var
  tag:TSimpleTag;
  ouvinte:TListener;
begin
  tag:=TSimpleTag.Create(nil);
  ouvinte:=TListener.Create(tag, false);
  try
    FreeAndNil(tag);
    AssertTrue('the listener must have been told', ouvinte.Avisado);
  finally
    ouvinte.Free;
    tag.Free;
  end;
end;

procedure TTestTag.AListenerThatDropsItsHooksDoesNotHideTheOthers;
var
  tag:TSimpleTag;
  primeiro, segundo, terceiro:TListener;
begin
  //o primeiro ouvinte larga os ganchos ao ser avisado, o que tira a entrada
  //dele da lista que o destrutor esta' percorrendo. Os outros dois nao podem
  //ficar sem aviso por causa disso
  tag:=TSimpleTag.Create(nil);
  primeiro:=TListener.Create(tag, true);
  segundo :=TListener.Create(tag, false);
  terceiro:=TListener.Create(tag, false);
  try
    FreeAndNil(tag);

    AssertTrue('first listener', primeiro.Avisado);
    AssertTrue('second listener',  segundo.Avisado);
    AssertTrue('third listener', terceiro.Avisado);
  finally
    primeiro.Free;
    segundo.Free;
    terceiro.Free;
    tag.Free;
  end;
end;

procedure TTestTag.HooksDroppedEarlierAreNotNotified;
var
  tag:TSimpleTag;
  ouvinte:TListener;
begin
  //quem se desligou antes da hora nao pode receber o aviso
  tag:=TSimpleTag.Create(nil);
  ouvinte:=TListener.Create(tag, false);
  try
    tag.RemoveAllHandlersFromObject(ouvinte);
    FreeAndNil(tag);

    AssertFalse('a detached listener gets no notification', ouvinte.Avisado);
  finally
    ouvinte.Free;
    tag.Free;
  end;
end;

initialization
  RegisterTest(TTestTag);

end.
