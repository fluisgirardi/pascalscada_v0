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

  { TTagSimples }

  //: um tag concreto qualquer: TTag nao tem nada de abstrato a implementar
  TTagSimples = class(TTag);

  { TOuvinte }

  {$IFDEF PORTUGUES}
  //: Ouvinte que, ao saber que o tag vai embora, larga os seus ganchos - que
  //  e' o que RemoveAllHandlersFromObject existe para fazer.
  {$ELSE}
  //: A listener that, on learning the tag is going away, drops its handlers -
  //  which is what RemoveAllHandlersFromObject is there for.
  {$ENDIF}
  TOuvinte = class
  private
    FTag:TTag;
    FAvisado:Boolean;
    FLargarOsGanchos:Boolean;
  public
    constructor Create(aTag:TTag; aLargarOsGanchos:Boolean);
    procedure AvisoDeRemocao(Sender:TObject);
    property  Avisado:Boolean read FAvisado;
  end;

  { TTestTag }

  TTestTag = class(TTestCase)
  published
    procedure AvisoDeRemocaoChegaAoOuvinte;
    procedure OuvinteQueLargaOsGanchosNaoEsconderOsOutros;
    procedure GanchosLargadosAntesNaoSaoAvisados;
  end;

implementation

{ TOuvinte }

constructor TOuvinte.Create(aTag:TTag; aLargarOsGanchos:Boolean);
begin
  inherited Create;
  FTag:=aTag;
  FAvisado:=false;
  FLargarOsGanchos:=aLargarOsGanchos;
  FTag.AddRemoveTagHandler(@AvisoDeRemocao);
end;

procedure TOuvinte.AvisoDeRemocao(Sender:TObject);
begin
  FAvisado:=true;
  if FLargarOsGanchos and (FTag<>nil) then begin
    FTag.RemoveAllHandlersFromObject(Self);
    FTag:=nil;
  end;
end;

{ TTestTag }

procedure TTestTag.AvisoDeRemocaoChegaAoOuvinte;
var
  tag:TTagSimples;
  ouvinte:TOuvinte;
begin
  tag:=TTagSimples.Create(nil);
  ouvinte:=TOuvinte.Create(tag, false);
  try
    FreeAndNil(tag);
    AssertTrue('o ouvinte tem que ter sido avisado', ouvinte.Avisado);
  finally
    ouvinte.Free;
    tag.Free;
  end;
end;

procedure TTestTag.OuvinteQueLargaOsGanchosNaoEsconderOsOutros;
var
  tag:TTagSimples;
  primeiro, segundo, terceiro:TOuvinte;
begin
  //o primeiro ouvinte larga os ganchos ao ser avisado, o que tira a entrada
  //dele da lista que o destrutor esta' percorrendo. Os outros dois nao podem
  //ficar sem aviso por causa disso
  tag:=TTagSimples.Create(nil);
  primeiro:=TOuvinte.Create(tag, true);
  segundo :=TOuvinte.Create(tag, false);
  terceiro:=TOuvinte.Create(tag, false);
  try
    FreeAndNil(tag);

    AssertTrue('primeiro ouvinte', primeiro.Avisado);
    AssertTrue('segundo ouvinte',  segundo.Avisado);
    AssertTrue('terceiro ouvinte', terceiro.Avisado);
  finally
    primeiro.Free;
    segundo.Free;
    terceiro.Free;
    tag.Free;
  end;
end;

procedure TTestTag.GanchosLargadosAntesNaoSaoAvisados;
var
  tag:TTagSimples;
  ouvinte:TOuvinte;
begin
  //quem se desligou antes da hora nao pode receber o aviso
  tag:=TTagSimples.Create(nil);
  ouvinte:=TOuvinte.Create(tag, false);
  try
    tag.RemoveAllHandlersFromObject(ouvinte);
    FreeAndNil(tag);

    AssertFalse('ouvinte desligado nao recebe aviso', ouvinte.Avisado);
  finally
    ouvinte.Free;
    tag.Free;
  end;
end;

initialization
  RegisterTest(TTestTag);

end.
