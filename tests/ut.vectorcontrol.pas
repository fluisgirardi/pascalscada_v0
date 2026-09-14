{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes da familia de desenhos vetoriais: o SVG que muda de cor
            conforme o tag e deixa - ou nao - o fluxo passar para as saidas.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Uma valvula na tela e' um SVG. O tag diz em que estado ela esta', o estado
  escolhe uma zona, e a zona traz uma lista de instrucoes em texto dizendo o
  que muda no desenho: "corpo.fill=#ff0000" pinta o corpo de vermelho,
  "corpo.fill=fill" manda usar a cor da propria zona, "flow.output=1" manda o
  fluxo que chegou pela linha de entrada seguir pela primeira saida.

  Sao duas coisas para testar: o interpretador dessas instrucoes, que precisa
  recusar o que esta' errado antes de guardar, e o efeito delas - a cor que
  aparece no SVG e a cor que sai pelas linhas de saida.
}
{$ELSE}
{:
  @abstract(Vector drawing family tests: the SVG that changes colour according
            to the tag and lets - or does not let - the flow reach the outputs.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A valve on the screen is an SVG. The tag says which state it is in, the state
  picks a zone, and the zone carries a list of text statements saying what
  changes on the drawing: "body.fill=#ff0000" paints the body red,
  "body.fill=fill" tells it to use the zone's own colour, "flow.output=1" tells
  the flow that arrived through the input line to carry on through the first
  output.

  There are two things to test: the interpreter of those statements, which has
  to refuse what is wrong before storing it, and their effect - the colour that
  shows on the SVG and the colour that leaves through the output lines.
}
{$ENDIF}
unit ut.vectorcontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, fpcunit, testregistry,
  BGRABitmapTypes, BGRASVGType,
  hmi_draw_basic_vector_control, hmi_flow_zones, hmi_polyline, HMIZones,
  PLCTag, PLCString, testsupport.faketag;

type

  { TVectorProbe }

  //FSVGDrawing e' protegido: em producao so a rotina de desenho o consulta.
  //A sonda le a cor que sobrou no elemento do SVG depois das instrucoes da
  //zona terem sido aplicadas.
  //
  //FSVGDrawing is protected: in production only the paint routine asks for it.
  //The probe reads the colour left on the SVG element after the zone's
  //statements have been applied.
  TVectorProbe = class(THMIFlowVectorControl)
  private
    function  Find(const aId:AnsiString):TSVGElement;
  public
    function FillOf(const aId:AnsiString):TColor;
    function BorderOf(const aId:AnsiString):TColor;
  end;

  { TTestVectorFlowStatements }

  TTestVectorFlowStatements = class(TTestCase)
  private
    FStates:THMIVectorFlowZones;
    FZone:THMIVectorFlowZone;
    function  ObjectOf(const aLine:AnsiString):TSVGChange;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o que e' aceito / what is accepted
    procedure TheBareFlowStatementIsValid;
    procedure AFillWithAnRGBColourIsValid;
    procedure AFillTakingTheZoneColourIsValid;
    procedure ABorderStatementIsValid;
    procedure AStopColorStatementIsValid;
    procedure TheStatementIsCaseInsensitive;
    procedure TheOutputStatementIsValid;
    procedure TheStopOpacityIsValidBetweenZeroAndOneHundred;

    //o que e' recusado / what is refused
    procedure AnIdWithASpaceIsRefused;
    procedure AnRGBWithoutTheHashIsRefused;
    procedure AnRGBStartingWithAnotherSignIsRefused;
    procedure AShortRGBIsRefused;
    procedure AnRGBWithLettersOutOfTheHexRangeIsRefused;
    procedure AnUnknownPropertyIsRefused;
    procedure AnOutputThatIsNotANumberIsRefused;
    procedure AnOutputBelowOneIsRefused;
    procedure AStopOpacityOutOfRangeIsRefused;
    procedure ALineWithNoValueIsRefused;

    //a leitura da cor / reading the colour
    procedure TheHexColourIsReadChannelByChannel;

    //os objetos criados / the objects that are built
    procedure AFillStatementBecomesAFillChange;
    procedure ABorderStatementBecomesABorderChange;
    procedure AStopColorStatementBecomesAStopColorChange;
    procedure AnOutputStatementBecomesAnOutputChange;
    procedure AStatementTakingTheFlowColourKeepsTheFlowAsTheSource;

    //a lista da zona / the zone's list
    procedure TheStatementsOfTheZoneAreKept;
    procedure AnInvalidStatementIsRefusedBeforeBeingStored;
  end;

  { TTestFlowVectorControl }

  TTestFlowVectorControl = class(TTestCase)
  private
    FCtrl:TVectorProbe;
    FTag:TFakeNumber;
    FInput,
    FOut1,
    FOut2:THMIFlowPolyline;
    FZoneChanges:LongInt;
    procedure OnZoneChanged(Sender:TObject);
    function  NewState(aValue:Double; const aChanges:AnsiString):THMIVectorFlowZone;
    procedure TagValueIs(v:Double);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //o fluxo / the flow
    procedure TheOutputTakesTheColourOfTheInput;
    procedure AStateWithNoFlowStatementLeavesTheOutputEmpty;
    procedure AnEmptyInputLeavesTheOutputEmpty;
    procedure TheOutputFollowsTheInputColourChange;
    procedure OnlyTheChosenOutputCarriesTheFlow;
    procedure TheTagValueChoosesWhichOutputCarriesTheFlow;
    procedure TheBareFlowStatementFeedsTheFirstOutput;
    procedure ADestroyedInputStopsFeedingTheOutputs;

    //o desenho / the drawing
    procedure ACustomColourPaintsTheElement;
    procedure TheZoneColourPaintsTheElement;
    procedure TheZoneBorderColourPaintsTheBorder;
    procedure TheFlowColourPaintsTheElement;
    procedure ChangingStateRepaintsWithTheNewStatements;

    //os avisos / the notifications
    procedure ChangingZoneNotifies;

    //o tag / the tag
    procedure ATagThatIsNotNumericIsRefused;
    procedure ADestroyedTagLetsGoOfTheControl;

    //as colecoes / the collections
    procedure AForkedValveComesWithTwoOutputs;
    procedure AThreeWayValveComesWithThreeOutputs;
    procedure AssigningTheOutputsOfAnotherControlCopiesThem;
    procedure AssigningTheStatesOfAnotherControlCopiesThem;
  end;

implementation

const
  UmSVG =
    '<?xml version="1.0" encoding="UTF-8"?>'+LineEnding+
    '<svg xmlns="http://www.w3.org/2000/svg" width="40" height="40">'+LineEnding+
    '  <rect id="corpo" x="0" y="0" width="40" height="40" fill="#ffffff" stroke="#000000" stroke-width="1"/>'+LineEnding+
    '</svg>';

{ TVectorProbe }

function TVectorProbe.Find(const aId:AnsiString):TSVGElement;
var
  i: Integer;
begin
  Result:=nil;
  if FSVGDrawing=nil then exit;
  for i:=0 to FSVGDrawing.Content.ElementCount-1 do
    if FSVGDrawing.Content.IsSVGElement[i] and
       FSVGDrawing.Content.Element[i].HasAttribute('id') and
       (LowerCase(FSVGDrawing.Content.Element[i].Attribute['id'])=aId) then begin
      Result:=FSVGDrawing.Content.Element[i];
      exit;
    end;
end;

function TVectorProbe.FillOf(const aId:AnsiString):TColor;
var
  el:TSVGElement;
begin
  el:=Find(aId);
  if el=nil then
    Result:=clNone
  else
    Result:=BGRAToColor(el.fillColor);
end;

function TVectorProbe.BorderOf(const aId:AnsiString):TColor;
var
  el:TSVGElement;
begin
  el:=Find(aId);
  if el=nil then
    Result:=clNone
  else
    Result:=BGRAToColor(el.strokeColor);
end;

{ TTestVectorFlowStatements }

procedure TTestVectorFlowStatements.SetUp;
begin
  FStates:=THMIVectorFlowZones.Create(nil);
  FZone:=FStates.Add;
end;

procedure TTestVectorFlowStatements.TearDown;
begin
  FreeAndNil(FStates);
end;

function TTestVectorFlowStatements.ObjectOf(const aLine:AnsiString):TSVGChange;
begin
  Result:=FZone.CreateStatementObject(aLine);
end;

procedure TTestVectorFlowStatements.TheBareFlowStatementIsValid;
begin
  AssertTrue('flow', THMIVectorFlowZone.ValidStatement('flow'));
end;

procedure TTestVectorFlowStatements.AFillWithAnRGBColourIsValid;
begin
  AssertTrue('cor em hexa', THMIVectorFlowZone.ValidStatement('corpo.fill=#ff0000'));
end;

procedure TTestVectorFlowStatements.AFillTakingTheZoneColourIsValid;
begin
  AssertTrue('cor da zona',  THMIVectorFlowZone.ValidStatement('corpo.fill=fill'));
  AssertTrue('cor da borda', THMIVectorFlowZone.ValidStatement('corpo.fill=border'));
  AssertTrue('cor do fluxo', THMIVectorFlowZone.ValidStatement('corpo.fill=flow'));
end;

procedure TTestVectorFlowStatements.ABorderStatementIsValid;
begin
  AssertTrue('borda', THMIVectorFlowZone.ValidStatement('corpo.border=#00ff00'));
end;

procedure TTestVectorFlowStatements.AStopColorStatementIsValid;
begin
  AssertTrue('parada do degrade', THMIVectorFlowZone.ValidStatement('parada1.stop-color=#0000ff'));
end;

procedure TTestVectorFlowStatements.TheStatementIsCaseInsensitive;
begin
  AssertTrue('tudo em maiuscula', THMIVectorFlowZone.ValidStatement('CORPO.FILL=#FF0000'));
end;

procedure TTestVectorFlowStatements.TheOutputStatementIsValid;
begin
  AssertTrue('saida 2', THMIVectorFlowZone.ValidStatement('flow.output=2'));
end;

procedure TTestVectorFlowStatements.TheStopOpacityIsValidBetweenZeroAndOneHundred;
begin
  AssertTrue('0',   THMIVectorFlowZone.ValidStatement('parada1.stop-opacity=0'));
  AssertTrue('100', THMIVectorFlowZone.ValidStatement('parada1.stop-opacity=100'));
end;

procedure TTestVectorFlowStatements.AnIdWithASpaceIsRefused;
begin
  AssertFalse('id com espaco', THMIVectorFlowZone.ValidStatement('corpo da valvula.fill=#ff0000'));
end;

procedure TTestVectorFlowStatements.AnRGBWithoutTheHashIsRefused;
begin
  AssertFalse('sem o #', THMIVectorFlowZone.ValidStatement('corpo.fill=ff0000'));
end;

procedure TTestVectorFlowStatements.AnRGBStartingWithAnotherSignIsRefused;
begin
  //do tamanho certo, mas nao comeca com #
  //the right length, but it does not start with #
  AssertFalse('comeca com outro sinal', THMIVectorFlowZone.ValidStatement('corpo.fill=*ff0000'));
end;

procedure TTestVectorFlowStatements.AShortRGBIsRefused;
begin
  AssertFalse('hexa curto', THMIVectorFlowZone.ValidStatement('corpo.fill=#f00'));
end;

procedure TTestVectorFlowStatements.AnRGBWithLettersOutOfTheHexRangeIsRefused;
begin
  AssertFalse('letra fora do hexa', THMIVectorFlowZone.ValidStatement('corpo.fill=#ffzz00'));
end;

procedure TTestVectorFlowStatements.AnUnknownPropertyIsRefused;
begin
  AssertFalse('propriedade que nao existe', THMIVectorFlowZone.ValidStatement('corpo.width=10'));
end;

procedure TTestVectorFlowStatements.AnOutputThatIsNotANumberIsRefused;
begin
  AssertFalse('saida sem numero', THMIVectorFlowZone.ValidStatement('flow.output=primeira'));
end;

procedure TTestVectorFlowStatements.AnOutputBelowOneIsRefused;
begin
  //as saidas sao contadas a partir de 1; o que nao aponta para saida nenhuma
  //tem que ser recusado na hora de digitar, e nao virar uma linha calada
  //outputs are counted from 1; what points at no output at all has to be
  //refused while it is typed, instead of becoming a silent line
  AssertFalse('saida zero',     THMIVectorFlowZone.ValidStatement('flow.output=0'));
  AssertFalse('saida negativa', THMIVectorFlowZone.ValidStatement('flow.output=-1'));
end;

procedure TTestVectorFlowStatements.AStopOpacityOutOfRangeIsRefused;
begin
  AssertFalse('acima de 100', THMIVectorFlowZone.ValidStatement('parada1.stop-opacity=101'));
  AssertFalse('negativo',     THMIVectorFlowZone.ValidStatement('parada1.stop-opacity=-1'));
end;

procedure TTestVectorFlowStatements.ALineWithNoValueIsRefused;
begin
  AssertFalse('sem o igual', THMIVectorFlowZone.ValidStatement('corpo.fill'));
end;

procedure TTestVectorFlowStatements.TheHexColourIsReadChannelByChannel;
begin
  AssertEquals('vermelho', clRed,  THMIVectorFlowZone.RGBHexToColor('#ff0000'));
  AssertEquals('verde',    clLime, THMIVectorFlowZone.RGBHexToColor('#00ff00'));
  AssertEquals('azul',     clBlue, THMIVectorFlowZone.RGBHexToColor('#0000ff'));
end;

procedure TTestVectorFlowStatements.AFillStatementBecomesAFillChange;
var
  o:TSVGChange;
begin
  o:=ObjectOf('corpo.fill=#ff0000');
  try
    AssertTrue  ('e uma mudanca de preenchimento', o is TSVGFillChange);
    AssertEquals('do elemento corpo', 'corpo', TSVGColorChange(o).SVGID);
    AssertTrue  ('com cor propria',   TSVGColorChange(o).ColorSource=csCustom);
    AssertEquals('vermelha',          clRed, TSVGColorChange(o).CustomColor);
  finally
    o.Free;
  end;
end;

procedure TTestVectorFlowStatements.ABorderStatementBecomesABorderChange;
var
  o:TSVGChange;
begin
  o:=ObjectOf('corpo.border=#00ff00');
  try
    AssertTrue  ('e uma mudanca de borda', o is TSVGBorderChange);
    AssertEquals('verde', clLime, TSVGColorChange(o).CustomColor);
  finally
    o.Free;
  end;
end;

procedure TTestVectorFlowStatements.AStopColorStatementBecomesAStopColorChange;
var
  o:TSVGChange;
begin
  o:=ObjectOf('parada1.stop-color=#0000ff');
  try
    AssertTrue('e uma mudanca de parada do degrade', o is TSVGStopColorChange);
  finally
    o.Free;
  end;
end;

procedure TTestVectorFlowStatements.AnOutputStatementBecomesAnOutputChange;
var
  o:TSVGChange;
begin
  o:=ObjectOf('flow.output=2');
  try
    AssertTrue  ('e uma escolha de saida', o is TOutputChange);
    AssertEquals('a segunda', 2, TOutputChange(o).OutputIndex);
  finally
    o.Free;
  end;
end;

procedure TTestVectorFlowStatements.AStatementTakingTheFlowColourKeepsTheFlowAsTheSource;
var
  o:TSVGChange;
begin
  o:=ObjectOf('corpo.fill=flow');
  try
    AssertTrue('a cor vem do fluxo', TSVGColorChange(o).ColorSource=csFlow);
  finally
    o.Free;
  end;
end;

procedure TTestVectorFlowStatements.TheStatementsOfTheZoneAreKept;
var
  sl:TStringList;
begin
  sl:=TStringList.Create;
  try
    sl.Add('corpo.fill=#ff0000');
    sl.Add('flow.output=1');
    FZone.SVGChanges:=sl;
  finally
    sl.Free;
  end;

  AssertEquals('duas instrucoes', 2, FZone.SVGChanges.Count);
  AssertTrue('a primeira virou objeto', FZone.SVGChanges.Objects[0] is TSVGFillChange);
  AssertTrue('a segunda tambem',        FZone.SVGChanges.Objects[1] is TOutputChange);
end;

procedure TTestVectorFlowStatements.AnInvalidStatementIsRefusedBeforeBeingStored;
var
  sl:TStringList;
begin
  //recusar na hora de guardar e' o que da' o erro no lugar certo - na
  //propriedade que o usuario digitou - em vez de um desenho errado depois
  //
  //refusing it while storing is what puts the error in the right place - on
  //the property the user typed - instead of a wrong drawing later on
  sl:=TStringList.Create;
  try
    sl.Add('corpo.fill=#ff0000');
    sl.Add('isso nao e uma instrucao');
    try
      FZone.SVGChanges:=sl;
      Fail('a instrucao invalida tinha que ser recusada');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertEquals('e nada foi guardado', 0, FZone.SVGChanges.Count);
  finally
    sl.Free;
  end;
end;

{ TTestFlowVectorControl }

procedure TTestFlowVectorControl.SetUp;
begin
  FZoneChanges:=0;
  FCtrl:=TVectorProbe.Create(nil);
  FCtrl.SVGContents.Text:=UmSVG;
  FTag:=TFakeNumber.Create(nil);
  FInput:=THMIFlowPolyline.Create(nil);
  FOut1:=THMIFlowPolyline.Create(nil);
  FOut2:=THMIFlowPolyline.Create(nil);

  //vazio e' preto em todas elas; o fluxo e' qualquer cor diferente disso
  //empty is black on all of them; the flow is any colour other than that
  FCtrl.FlowOutputPolylines.Add.OutputPolyline:=FOut1;
  FCtrl.FlowOutputPolylines.Add.OutputPolyline:=FOut2;
  FCtrl.InputFlowPolyline:=FInput;
  FCtrl.PLCTag:=FTag;
end;

procedure TTestFlowVectorControl.TearDown;
begin
  FreeAndNil(FCtrl);
  FreeAndNil(FInput);
  FreeAndNil(FOut1);
  FreeAndNil(FOut2);
  FreeAndNil(FTag);
end;

procedure TTestFlowVectorControl.OnZoneChanged(Sender:TObject);
begin
  inc(FZoneChanges);
end;

function TTestFlowVectorControl.NewState(aValue:Double; const aChanges:AnsiString):THMIVectorFlowZone;
var
  sl:TStringList;
begin
  Result:=FCtrl.ColorAndFlowStates.Add;
  Result.ZoneType:=ztEqual;
  Result.SetValues(aValue, aValue);
  if aChanges<>'' then begin
    sl:=TStringList.Create;
    try
      sl.Text:=aChanges;
      Result.SVGChanges:=sl;
    finally
      sl.Free;
    end;
  end;
end;

procedure TTestFlowVectorControl.Settle;
begin
  //o recalculo do desenho e do fluxo e' agendado na fila da aplicacao
  //the redraw and the flow are scheduled on the application queue
  Application.ProcessMessages;
end;

procedure TTestFlowVectorControl.TagValueIs(v:Double);
begin
  FTag.ChegouDoCLP(v);
  Settle;
end;

procedure TTestFlowVectorControl.TheOutputTakesTheColourOfTheInput;
begin
  NewState(1, 'flow.output=1');
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('a saida 1 recebeu o fluxo', clRed, FOut1.LineColor);
end;

procedure TTestFlowVectorControl.AStateWithNoFlowStatementLeavesTheOutputEmpty;
begin
  //valvula fechada: o estado nao manda o fluxo para saida nenhuma
  //closed valve: the state sends the flow to no output at all
  NewState(0, 'corpo.fill=#ff0000');
  FInput.LineColor:=clRed;

  TagValueIs(0);

  AssertEquals('a saida 1 ficou vazia', FOut1.EmptyColor, FOut1.LineColor);
  AssertEquals('a saida 2 tambem',      FOut2.EmptyColor, FOut2.LineColor);
end;

procedure TTestFlowVectorControl.AnEmptyInputLeavesTheOutputEmpty;
begin
  //valvula aberta, mas nada chegando nela
  //open valve, but nothing arriving at it
  NewState(1, 'flow.output=1');

  TagValueIs(1);

  AssertEquals('nada entrou, nada saiu', FOut1.EmptyColor, FOut1.LineColor);
end;

procedure TTestFlowVectorControl.TheOutputFollowsTheInputColourChange;
begin
  NewState(1, 'flow.output=1');
  FInput.LineColor:=clRed;
  TagValueIs(1);

  FInput.LineColor:=clLime;
  Settle;

  AssertEquals('a saida acompanhou', clLime, FOut1.LineColor);
end;

procedure TTestFlowVectorControl.OnlyTheChosenOutputCarriesTheFlow;
begin
  NewState(1, 'flow.output=1');
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('a escolhida',     clRed,            FOut1.LineColor);
  AssertEquals('e so a escolhida', FOut2.EmptyColor, FOut2.LineColor);
end;

procedure TTestFlowVectorControl.TheTagValueChoosesWhichOutputCarriesTheFlow;
begin
  //e' o que uma valvula de tres vias faz: o mesmo fluxo, outra saida
  //it is what a three way valve does: the same flow, another output
  NewState(1, 'flow.output=1');
  NewState(2, 'flow.output=2');
  FInput.LineColor:=clRed;

  TagValueIs(1);
  AssertEquals('pela saida 1', clRed,            FOut1.LineColor);
  AssertEquals('e nao pela 2', FOut2.EmptyColor, FOut2.LineColor);

  TagValueIs(2);
  AssertEquals('agora pela 2', clRed,            FOut2.LineColor);
  AssertEquals('e nao pela 1', FOut1.EmptyColor, FOut1.LineColor);
end;

procedure TTestFlowVectorControl.TheBareFlowStatementFeedsTheFirstOutput;
begin
  //"flow" sem numero e' a forma curta de "flow.output=1"
  //"flow" with no number is the short form of "flow.output=1"
  NewState(1, 'flow');
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('a primeira saida recebeu o fluxo', clRed, FOut1.LineColor);
end;

procedure TTestFlowVectorControl.ADestroyedInputStopsFeedingTheOutputs;
begin
  NewState(1, 'flow.output=1');
  FInput.LineColor:=clRed;
  TagValueIs(1);
  AssertEquals('pintou', clRed, FOut1.LineColor);

  FreeAndNil(FInput);
  Settle;

  AssertEquals('sem entrada, sem fluxo', FOut1.EmptyColor, FOut1.LineColor);
end;

procedure TTestFlowVectorControl.ACustomColourPaintsTheElement;
begin
  NewState(1, 'corpo.fill=#ff0000');

  TagValueIs(1);

  AssertEquals('o corpo ficou vermelho', clRed, FCtrl.FillOf('corpo'));
end;

procedure TTestFlowVectorControl.TheZoneColourPaintsTheElement;
var
  z:THMIVectorFlowZone;
begin
  z:=NewState(1, 'corpo.fill=fill');
  z.Color:=clLime;

  TagValueIs(1);

  AssertEquals('o corpo ficou com a cor da zona', clLime, FCtrl.FillOf('corpo'));
end;

procedure TTestFlowVectorControl.TheZoneBorderColourPaintsTheBorder;
var
  z:THMIVectorFlowZone;
begin
  z:=NewState(1, 'corpo.border=border');
  z.BorderColor:=clBlue;

  TagValueIs(1);

  AssertEquals('a borda ficou com a cor da zona', clBlue, FCtrl.BorderOf('corpo'));
end;

procedure TTestFlowVectorControl.TheFlowColourPaintsTheElement;
begin
  //o corpo da valvula pintado com a cor do produto que esta passando
  //the valve body painted with the colour of the product going through
  NewState(1, 'corpo.fill=flow');
  FInput.LineColor:=clRed;

  TagValueIs(1);

  AssertEquals('o corpo ficou com a cor do fluxo', clRed, FCtrl.FillOf('corpo'));
end;

procedure TTestFlowVectorControl.ChangingStateRepaintsWithTheNewStatements;
begin
  NewState(0, 'corpo.fill=#ff0000');
  NewState(1, 'corpo.fill=#00ff00');

  TagValueIs(0);
  AssertEquals('fechada', clRed, FCtrl.FillOf('corpo'));

  TagValueIs(1);
  AssertEquals('aberta', clLime, FCtrl.FillOf('corpo'));
end;

procedure TTestFlowVectorControl.ChangingZoneNotifies;
begin
  FCtrl.ZoneChanged:=@OnZoneChanged;
  NewState(0, 'corpo.fill=#ff0000');
  NewState(1, 'corpo.fill=#00ff00');

  TagValueIs(0);
  TagValueIs(1);

  AssertTrue('avisou a troca de estado', FZoneChanges>0);
end;

procedure TTestFlowVectorControl.ATagThatIsNotNumericIsRefused;
var
  tagDeTexto:TPLCString;
begin
  //um desenho vetorial escolhe o estado por um numero. O tag de texto e' um
  //tag de verdade; o unico motivo de recusa possivel e' nao ser numerico
  //
  //a vector drawing picks its state by a number. The text tag is a real tag;
  //the only possible reason to refuse it is not being numeric
  tagDeTexto:=TPLCString.Create(nil);
  try
    try
      FCtrl.PLCTag:=tagDeTexto;
      Fail('um tag de texto tem que ser recusado');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;

    AssertTrue('e nao pode ter sido ligado', FCtrl.PLCTag=FTag);
  finally
    tagDeTexto.Free;
  end;
end;

procedure TTestFlowVectorControl.ADestroyedTagLetsGoOfTheControl;
begin
  FreeAndNil(FTag);

  AssertTrue('o controle soltou o tag', FCtrl.PLCTag=nil);
end;

procedure TTestFlowVectorControl.AForkedValveComesWithTwoOutputs;
var
  v:THMIForkedFlowValve;
begin
  v:=THMIForkedFlowValve.Create(nil);
  try
    v.OutputPolylineLeft :=FOut1;
    v.OutputPolylineRight:=FOut2;

    AssertTrue('a esquerda', v.OutputPolylineLeft =FOut1);
    AssertTrue('a direita',  v.OutputPolylineRight=FOut2);
  finally
    v.Free;
  end;
end;

procedure TTestFlowVectorControl.AThreeWayValveComesWithThreeOutputs;
var
  v:THMIThreeWayFlowValve;
begin
  v:=THMIThreeWayFlowValve.Create(nil);
  try
    v.OutputPolylineLeft  :=FOut1;
    v.OutputPolylineMiddle:=FOut2;

    AssertTrue('a esquerda', v.OutputPolylineLeft  =FOut1);
    AssertTrue('a do meio',  v.OutputPolylineMiddle=FOut2);
    AssertTrue('a direita ainda sem linha', v.OutputPolylineRight=nil);
  finally
    v.Free;
  end;
end;

procedure TTestFlowVectorControl.AssigningTheOutputsOfAnotherControlCopiesThem;
var
  outro:TVectorProbe;
begin
  outro:=TVectorProbe.Create(nil);
  try
    outro.FlowOutputPolylines:=FCtrl.FlowOutputPolylines;

    AssertEquals('duas saidas', 2, outro.FlowOutputPolylines.Count);
    AssertTrue('a primeira e a mesma linha',
               THMIOutputCollectionItem(outro.FlowOutputPolylines.Items[0]).OutputPolyline=FOut1);
  finally
    outro.Free;
  end;
end;

procedure TTestFlowVectorControl.AssigningTheStatesOfAnotherControlCopiesThem;
var
  outro:TVectorProbe;
  z:THMIVectorFlowZone;
begin
  z:=NewState(1, 'flow.output=1');
  z.Color:=clLime;
  z.BorderColor:=clBlue;
  z.Flow:=true;

  outro:=TVectorProbe.Create(nil);
  try
    outro.ColorAndFlowStates:=FCtrl.ColorAndFlowStates;

    AssertEquals('um estado',    1,      outro.ColorAndFlowStates.Count);
    AssertEquals('a cor',        clLime, THMIVectorFlowZone(outro.ColorAndFlowStates.Items[0]).Color);
    AssertEquals('a cor da borda', clBlue, THMIVectorFlowZone(outro.ColorAndFlowStates.Items[0]).BorderColor);
    AssertEquals('as instrucoes', 1,     THMIVectorFlowZone(outro.ColorAndFlowStates.Items[0]).SVGChanges.Count);
  finally
    outro.Free;
  end;
end;

initialization
  RegisterTest(TTestVectorFlowStatements);
  RegisterTest(TTestFlowVectorControl);

end.
