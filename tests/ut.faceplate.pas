{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do TFaceplateFrame: o quadro que recebe uma estrutura e a
            reparte entre os seus itens.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Um faceplate e' um quadro desenhado no IDE - um .lfm - com itens de
  estrutura dentro: nivel, setpoint, nome. Na tela, cada instancia recebe uma
  estrutura diferente (o motor 1, o motor 2) e todos os itens do quadro
  passam a ler dela.

  O quadro de teste vem do seu proprio .lfm, como um faceplate de verdade: e'
  o unico jeito de passar pelo caminho que o usuario passa - o Create que
  carrega o recurso, o Loaded, o OnLoaded.
}
{$ELSE}
{:
  @abstract(TFaceplateFrame tests: the frame that receives a struct and hands
            it out to its items.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A faceplate is a frame drawn in the IDE - a .lfm - with struct items inside:
  level, setpoint, name. On screen, each instance receives a different struct
  (motor 1, motor 2) and every item in the frame starts reading from it.

  The test frame comes from its own .lfm, like a real faceplate: it is the
  only way to go down the path the user goes down - the Create that loads the
  resource, Loaded, OnLoaded.
}
{$ENDIF}
unit ut.faceplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, LMessages, fpcunit, testregistry,
  hmifaceplatecontainer, PLCStruct, PLCStructElement, plcstructstring;

type

  { TTestFaceplate }

  //o quadro do .lfm: dois itens numericos, um texto, e dois controles comuns
  //the frame from the .lfm: two numeric items, one string, two plain controls
  TTestFaceplate = class(TFaceplateFrame)
    Rotulo:TLabel;
    Caixa:TEdit;
    Nivel:TPLCStructItem;
    Setpoint:TPLCStructItem;
    Nome:TPLCStructString;
    procedure LoadedHappened(Sender:TObject);
  public
    LoadedCount:LongInt;
    function ItIsControlArea:Boolean;
  end;

  { TTestFaceplateFrame }

  TTestFaceplateFrame = class(TTestCase)
  private
    FPlate:TTestFaceplate;
    FStruct:TPLCStruct;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //a carga / loading
    procedure AFaceplateCanBeCreatedByCodeBeforeItHasAParent;
    procedure OnLoadedFiresOnceWhenTheResourceIsLoaded;
    procedure TheChildrenAreNotSelectableInTheDesigner;

    //a estrutura / the struct
    procedure ANewFaceplateHasNoStruct;
    procedure TheStructIsHandedToEveryItem;
    procedure TheStructCanBeReadBack;
    procedure ClearingTheStructClearsEveryItem;
    procedure ADestroyedStructLetsGoOfTheFaceplate;
    procedure TheItemsReadFromTheStruct;

    //transparencia / transparency
    procedure ANewFaceplateIsNotTransparent;
    procedure AnOpaqueFaceplateAnswersTheHitTest;
    procedure ATransparentFaceplateLetsTheHitTestThrough;

    //na tela / on screen
    procedure AFaceplateOnAFormGetsItsWindow;
  end;

implementation

{$R *.lfm}

{ TTestFaceplate }

procedure TTestFaceplate.LoadedHappened(Sender:TObject);
begin
  inc(LoadedCount);
end;

function TTestFaceplate.ItIsControlArea:Boolean;
begin
  Result:=IsControlArea(1,1);
end;

{ TTestFaceplateFrame }

procedure TTestFaceplateFrame.SetUp;
begin
  FPlate:=nil;
  FStruct:=TPLCStruct.Create(nil);
  //dois bytes numericos e um texto C de 5 (+1 do terminador)
  //two numeric bytes and a C string of 5 (+1 for the terminator)
  FStruct.Size:=8;
end;

procedure TTestFaceplateFrame.TearDown;
begin
  FreeAndNil(FPlate);
  FreeAndNil(FStruct);
end;

procedure TTestFaceplateFrame.AFaceplateCanBeCreatedByCodeBeforeItHasAParent;
begin
  //e' o jeito comum de usar um quadro: Create, e so' depois Parent. O Loaded
  //roda no meio do Create, sem janela nenhuma para se apoiar.
  //it is the usual way to use a frame: Create, and only then Parent. Loaded
  //runs in the middle of Create, with no window at all to lean on.
  FPlate:=TTestFaceplate.Create(nil);

  AssertNotNull('o quadro existe', FPlate);
  AssertEquals ('com os seus itens', 3+2, FPlate.ComponentCount);
end;

procedure TTestFaceplateFrame.OnLoadedFiresOnceWhenTheResourceIsLoaded;
begin
  FPlate:=TTestFaceplate.Create(nil);

  AssertEquals('OnLoaded uma vez', 1, FPlate.LoadedCount);
end;

procedure TTestFaceplateFrame.TheChildrenAreNotSelectableInTheDesigner;
begin
  //quem se seleciona no IDE e' o faceplate inteiro, nao os controles de dentro
  //what gets selected in the IDE is the whole faceplate, not the controls in it
  FPlate:=TTestFaceplate.Create(nil);

  AssertTrue('rotulo',  csNoDesignSelectable in FPlate.Rotulo.ControlStyle);
  AssertTrue('caixa',   csNoDesignSelectable in FPlate.Caixa.ControlStyle);
end;

procedure TTestFaceplateFrame.ANewFaceplateHasNoStruct;
begin
  FPlate:=TTestFaceplate.Create(nil);

  AssertTrue('sem estrutura', FPlate.FaceplatePLCTag=nil);
  AssertTrue('e os itens tambem', FPlate.Nivel.PLCBlock=nil);
end;

procedure TTestFaceplateFrame.TheStructIsHandedToEveryItem;
begin
  FPlate:=TTestFaceplate.Create(nil);

  FPlate.FaceplatePLCTag:=FStruct;

  AssertTrue('nivel',    FPlate.Nivel.PLCBlock=FStruct);
  AssertTrue('setpoint', FPlate.Setpoint.PLCBlock=FStruct);
  AssertTrue('nome',     FPlate.Nome.PLCBlock=FStruct);
end;

procedure TTestFaceplateFrame.TheStructCanBeReadBack;
begin
  //e' o que o IDE grava no .lfm da tela - lendo a propriedade
  //it is what the IDE writes to the screen's .lfm - by reading the property
  FPlate:=TTestFaceplate.Create(nil);

  FPlate.FaceplatePLCTag:=FStruct;

  AssertTrue('a estrutura que foi dada', FPlate.FaceplatePLCTag=FStruct);
end;

procedure TTestFaceplateFrame.ClearingTheStructClearsEveryItem;
begin
  FPlate:=TTestFaceplate.Create(nil);
  FPlate.FaceplatePLCTag:=FStruct;

  FPlate.FaceplatePLCTag:=nil;

  AssertTrue('quadro sem estrutura', FPlate.FaceplatePLCTag=nil);
  AssertTrue('nivel',    FPlate.Nivel.PLCBlock=nil);
  AssertTrue('setpoint', FPlate.Setpoint.PLCBlock=nil);
  AssertTrue('nome',     FPlate.Nome.PLCBlock=nil);
end;

procedure TTestFaceplateFrame.ADestroyedStructLetsGoOfTheFaceplate;
begin
  FPlate:=TTestFaceplate.Create(nil);
  FPlate.FaceplatePLCTag:=FStruct;

  FreeAndNil(FStruct);

  AssertTrue('o quadro soltou a estrutura', FPlate.FaceplatePLCTag=nil);
  AssertTrue('e os itens tambem',           FPlate.Nivel.PLCBlock=nil);
end;

procedure TTestFaceplateFrame.TheItemsReadFromTheStruct;
begin
  FPlate:=TTestFaceplate.Create(nil);
  FPlate.FaceplatePLCTag:=FStruct;

  FStruct.ValueRaw[1]:=42;

  AssertEquals('o setpoint le o byte 1', 42, FPlate.Setpoint.Value, 0.0001);
end;

procedure TTestFaceplateFrame.ANewFaceplateIsNotTransparent;
begin
  FPlate:=TTestFaceplate.Create(nil);

  AssertFalse('opaco',            FPlate.Transparent);
  AssertTrue ('e'' area de controle', FPlate.ItIsControlArea);
end;

procedure TTestFaceplateFrame.AnOpaqueFaceplateAnswersTheHitTest;
begin
  FPlate:=TTestFaceplate.Create(nil);

  AssertEquals('acertou o quadro', 1, FPlate.Perform(CM_HITTEST, 0, 0));
end;

procedure TTestFaceplateFrame.ATransparentFaceplateLetsTheHitTestThrough;
begin
  //transparente, o clique entre os controles vai para quem esta' atras
  //transparent, a click between the controls goes to whoever is behind
  FPlate:=TTestFaceplate.Create(nil);

  FPlate.Transparent:=true;

  AssertEquals('passou direto', 0, FPlate.Perform(CM_HITTEST, 0, 0));
  AssertFalse ('nao e'' area de controle', FPlate.ItIsControlArea);
end;

procedure TTestFaceplateFrame.AFaceplateOnAFormGetsItsWindow;
var
  form:TForm;
begin
  //a janela do quadro e' recortada pela regiao dos seus controles; isso so'
  //pode acontecer quando ha' janela - dentro de um formulario
  //the frame's window is clipped by the region of its controls; that can only
  //happen once there is a window - inside a form
  form:=TForm.CreateNew(nil);
  try
    FPlate:=TTestFaceplate.Create(form);
    FPlate.Parent:=form;

    FPlate.HandleNeeded;

    AssertTrue('tem janela', FPlate.HandleAllocated);
    FPlate:=nil; //o formulario e' o dono / the form owns it
  finally
    form.Free;
  end;
end;

initialization
  RegisterTest(TTestFaceplateFrame);

end.
