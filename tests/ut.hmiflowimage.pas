{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Testes do THMIFlowImage: a figura que ancora as pontas de duas
            linhas de fluxo.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Num sinotico, um equipamento desenhado como figura - um tanque, um filtro,
  uma peneira - tem tubulacao entrando de um lado e saindo do outro. Este
  controle diz em que ponto da figura cada ponta se encaixa, e mantem as duas
  linhas grudadas ali quando a figura e' movida ou redimensionada.
}
{$ELSE}
{:
  @abstract(THMIFlowImage tests: the picture that anchors the ends of two flow
            lines.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  On a synoptic, a piece of equipment drawn as a picture - a tank, a filter, a
  screen - has piping coming in on one side and going out on the other. This
  control says at which point of the picture each end fits, and keeps both
  lines stuck there when the picture is moved or resized.
}
{$ENDIF}
unit ut.hmiflowimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, fpcunit, testregistry,
  HMIFlowImage, hmi_polyline;

type

  { TTestPointPersistent }

  TTestPointPersistent = class(TTestCase)
  private
    FPoint:TPointPersistent;
    FChanges:LongInt;
    procedure Changed(Sender:TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheCoordinatesAreKept;
    procedure ChangingACoordinateNotifies;
    procedure SettingTheSameCoordinateDoesNotNotify;
    procedure AssigningAnotherPointCopiesBothCoordinates;
    procedure AssigningTheSamePlaceDoesNotNotify;
    procedure AssigningSomethingElseIsRefused;
  end;

  { TTestHMIFlowImage }

  TTestHMIFlowImage = class(TTestCase)
  private
    FForm:TForm;
    FImage:THMIFlowImage;
    FIn, FOut:THMIFlowPolyline;
    function  LastPointOf(aLine:THMIFlowPolyline):TPointCollectionItem;
    function  FirstPointOf(aLine:THMIFlowPolyline):TPointCollectionItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TheInputLineEndsAtTheInputPoint;
    procedure TheOutputLineStartsAtTheOutputPoint;
    procedure MovingThePointDragsTheLineEnd;
    procedure MovingThePictureDragsBothLines;
    procedure ADestroyedLineIsNotTouchedAgain;
  end;

implementation

{ TTestPointPersistent }

procedure TTestPointPersistent.SetUp;
begin
  FChanges:=0;
  FPoint:=TPointPersistent.Create;
  FPoint.OnChange:=@Changed;
end;

procedure TTestPointPersistent.TearDown;
begin
  FreeAndNil(FPoint);
end;

procedure TTestPointPersistent.Changed(Sender:TObject);
begin
  inc(FChanges);
end;

procedure TTestPointPersistent.TheCoordinatesAreKept;
begin
  FPoint.X:=10;
  FPoint.Y:=20;

  AssertEquals('x', 10, FPoint.X);
  AssertEquals('y', 20, FPoint.Y);
  AssertEquals('e o ponto', 10, FPoint.Point.X);
end;

procedure TTestPointPersistent.ChangingACoordinateNotifies;
begin
  FPoint.X:=10;

  AssertEquals('avisou', 1, FChanges);
end;

procedure TTestPointPersistent.SettingTheSameCoordinateDoesNotNotify;
begin
  FPoint.X:=10;
  FChanges:=0;

  FPoint.X:=10;

  AssertEquals('nada mudou, nada a avisar', 0, FChanges);
end;

procedure TTestPointPersistent.AssigningAnotherPointCopiesBothCoordinates;
var
  outro:TPointPersistent;
begin
  outro:=TPointPersistent.Create;
  try
    outro.X:=30;
    outro.Y:=40;

    FPoint.Assign(outro);

    AssertEquals('x', 30, FPoint.X);
    AssertEquals('y', 40, FPoint.Y);
    AssertEquals('avisou uma vez so', 1, FChanges);
  finally
    outro.Free;
  end;
end;

procedure TTestPointPersistent.AssigningTheSamePlaceDoesNotNotify;
var
  outro:TPointPersistent;
begin
  FPoint.X:=30;
  FPoint.Y:=40;
  outro:=TPointPersistent.Create;
  try
    outro.X:=30;
    outro.Y:=40;
    FChanges:=0;

    FPoint.Assign(outro);

    AssertEquals('mesmo lugar, nada a avisar', 0, FChanges);
  finally
    outro.Free;
  end;
end;

procedure TTestPointPersistent.AssigningSomethingElseIsRefused;
var
  outro:TStringList;
begin
  //copiar de outro tipo nao pode passar calado: o erro diz o que se tentou
  //copiar em que
  //copying from another type must not pass silently: the error says what was
  //being copied into what
  outro:=TStringList.Create;
  try
    try
      FPoint.Assign(outro);
      Fail('fonte de outro tipo tem que ser recusada');
    except
      on EAssertionFailedError do raise;
      on Exception do ;
    end;
  finally
    outro.Free;
  end;
end;

{ TTestHMIFlowImage }

procedure TTestHMIFlowImage.SetUp;
begin
  //ancorar ponta de linha usa ControlToScreen, que precisa de janela mae
  //anchoring a line end uses ControlToScreen, which needs a parent window
  FForm:=TForm.CreateNew(nil);
  FForm.Visible:=false;
  FForm.SetBounds(0,0,400,300);
  FImage:=THMIFlowImage.Create(FForm);
  FImage.Parent:=FForm;
  FImage.SetBounds(50,60,100,80);

  FIn:=THMIFlowPolyline.Create(FForm);
  FIn.Parent:=FForm;
  FIn.AutoRoute:=true;
  FOut:=THMIFlowPolyline.Create(FForm);
  FOut.Parent:=FForm;
  FOut.AutoRoute:=true;
end;

procedure TTestHMIFlowImage.TearDown;
begin
  FreeAndNil(FForm);
end;

function TTestHMIFlowImage.LastPointOf(aLine:THMIFlowPolyline):TPointCollectionItem;
begin
  Result:=TPointCollectionItem(aLine.PointCoordinates.Items[aLine.PointCoordinates.Count-1]);
end;

function TTestHMIFlowImage.FirstPointOf(aLine:THMIFlowPolyline):TPointCollectionItem;
begin
  Result:=TPointCollectionItem(aLine.PointCoordinates.Items[0]);
end;

procedure TTestHMIFlowImage.TheInputLineEndsAtTheInputPoint;
var
  antesX:Integer;
begin
  //a linha que chega termina no ponto de entrada da figura
  //the line that arrives ends at the picture's input point
  FImage.InputPoint.X:=10;
  FImage.InputPoint.Y:=20;
  antesX:=LastPointOf(FIn).X;

  FImage.InputFlowPolyline:=FIn;

  AssertTrue('a ponta da linha se mexeu', LastPointOf(FIn).X<>antesX);
end;

procedure TTestHMIFlowImage.TheOutputLineStartsAtTheOutputPoint;
var
  antesX:Integer;
begin
  FImage.OutputPoint.X:=90;
  FImage.OutputPoint.Y:=20;
  antesX:=FirstPointOf(FOut).X;

  FImage.OutputFlowPolyline:=FOut;

  AssertTrue('o comeco da linha se mexeu', FirstPointOf(FOut).X<>antesX);
end;

procedure TTestHMIFlowImage.MovingThePointDragsTheLineEnd;
var
  antesX:Integer;
begin
  //mudar o ponto de encaixe na figura arrasta a ponta da linha junto
  //moving the anchor point on the picture drags the line end along
  FImage.InputFlowPolyline:=FIn;
  FImage.InputPoint.X:=10;
  antesX:=LastPointOf(FIn).X;

  FImage.InputPoint.X:=60;

  AssertTrue('a ponta acompanhou', LastPointOf(FIn).X<>antesX);
end;

procedure TTestHMIFlowImage.MovingThePictureDragsBothLines;
var
  antesEntrada, antesSaida:Integer;
begin
  FImage.InputFlowPolyline:=FIn;
  FImage.OutputFlowPolyline:=FOut;
  antesEntrada:=LastPointOf(FIn).X;
  antesSaida:=FirstPointOf(FOut).X;

  FImage.SetBounds(150,60,100,80);

  AssertTrue('a entrada acompanhou', LastPointOf(FIn).X<>antesEntrada);
  AssertTrue('a saida acompanhou',   FirstPointOf(FOut).X<>antesSaida);
end;

procedure TTestHMIFlowImage.ADestroyedLineIsNotTouchedAgain;
begin
  //a figura pede aviso de destruicao das linhas; se esse aviso nao zerar o
  //ponteiro, o proximo movimento da figura escreve em memoria liberada
  //the picture asks to be told when the lines are destroyed; if that
  //notification does not clear the pointer, the next move writes into freed
  //memory
  FImage.InputFlowPolyline:=FIn;
  FImage.OutputFlowPolyline:=FOut;

  FreeAndNil(FIn);

  FImage.SetBounds(150,60,100,80);

  AssertTrue('a figura soltou a linha destruida', FImage.InputFlowPolyline=nil);
end;

initialization
  RegisterTest(TTestPointPersistent);
  RegisterTest(TTestHMIFlowImage);

end.
