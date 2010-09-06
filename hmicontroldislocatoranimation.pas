{:
  @abstract(Controle de deslocamento de controles (animação) nos eixos X e Y da janela.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit HMIControlDislocatorAnimation;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Controls, LinearScaleProcessor, PLCNumber, Tag,
  ProtocolTypes;

type

  {:
  @abstract(Controle de deslocamento de controles (animação) nos eixos X e Y da janela.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
  }
  THMIControlDislocatorAnimation = class(TComponent, IHMITagInterface)
  private
    FStartLeft,
    FEndLeft:Integer;
    FStartTop,
    FEndTop:Integer;
    FStartValue,
    FEndValue:Double;
    FTarget:TControl;
    FXLinearScale,
    FYLinearScale:TLinearScaleProcessor;
    FTag:TPLCNumber;
    FMinX, FMaxX,
    FMinY, FMaxY:Boolean;
    FMinXValue, FMaxXValue,
    FMinYValue, FMaxyValue:Integer;

    FGetPositionP0,
    FGetPositionP1,
    FGoToP0:String;

    procedure MoveObject;
    procedure SetStartLeft(v:Integer);
    procedure SetStartTop(v:Integer);
    procedure SetEndLeft(v:Integer);
    procedure SetEndTop(v:Integer);
    procedure SetValueStart(v:Double);
    procedure SetValueEnd(v:Double);
    procedure SetPLCTag(t:TPLCNumber);
    procedure SetControl(t:TControl);

    procedure PropertyDoesNothing(v:String);

    procedure SetEnableMinX(v:Boolean);
    procedure SetEnableMaxX(v:Boolean);
    procedure SetEnableMinY(v:Boolean);
    procedure SetEnableMaxY(v:Boolean);

    procedure SetMinX(v:Integer);
    procedure SetMaxX(v:Integer);
    procedure SetMinY(v:Integer);
    procedure SetMaxY(v:Integer);

    //metodos da interface ihmiTagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    //@exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    //: Posição inicial no eixo X (propriedade Left do controle)
    property P0_X:Integer read FStartLeft write SetStartLeft;
    //: Posição inicial no eixo Y (propriedade Top do controle)
    property P0_Y:Integer read FStartTop write SetStartTop;
    //: Posição final no eixo X (propriedade Left do controle)
    property P1_X:Integer read FEndLeft write SetEndLeft;
    //: Posição final no eixo y (propriedade Top do controle)
    property P1_Y:Integer read FEndTop write SetEndTop;
    //: Valor do tag que irá fazer o controle ir para as coordenadas (P0_X; P0_Y);
    property ValueP0:Double read FStartValue write SetValueStart;
    //: Valor do tag que irá fazer o controle ir para as coordenadas (P1_X; P1_Y);
    property ValueP1:Double read FEndValue write SetValueEnd;
    //: Tag que irá controlar a animação.
    property PLCTag:TPLCNumber read FTag write SetPLCTag;
    //: Controle que será manipulado.
    property Control:TControl read FTarget write SetControl;

    //: Habilita o valor mínimo do eixo X.
    property EnableXMin:Boolean read FMinX write SetEnableMinX;
    //: Habilita o valor máximo do eixo X.
    property EnableXMax:Boolean read FMaxX write SetEnableMaxX;
    //: Habilita o valor mínimo do eixo Y.
    property EnableYMin:Boolean read FMinY write SetEnableMinY;
    //: Habilita o valor máximo do eixo Y.
    property EnableYMax:Boolean read FMaxY write SetEnableMaxY;

    //: Caso valor mínimo de X caso EnableXMin esteja habilitado.
    property MinXValue:Integer read FMinXValue write SetMinX;
    //: Caso valor máximo de X caso EnableXMax esteja habilitado.
    property MaxXValue:Integer read FMaxXValue write SetMaxX;
    //: Caso valor mínimo de Y caso EnableYMin esteja habilitado.
    property MinYValue:Integer read FMinYValue write SetMinY;
    //: Caso valor máximo de Y caso EnableYMax esteja habilitado.
    property MaxYValue:Integer read FMaxyValue write SetMaxY;

    //: Pega a posição atual do controle como posição inicial (P0).
    property Gets_P0_Position:String read FGetPositionP0 write PropertyDoesNothing;
    //: Pega a posição atual do controle como posição final (P1).
    property Gets_P1_Position:String read FGetPositionP1 write PropertyDoesNothing;
     //: Reposiciona o controle para a posição inicial definida.
    property GoTo_P0_Position:String read FGoToP0        write PropertyDoesNothing;
  end;


implementation

uses hsstrings;

constructor THMIControlDislocatorAnimation.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FGetPositionP0:=SGetP0;
  FGetPositionP1:=SGetP1;
  FGoToP0:=SGotoP0;
  FXLinearScale:=TLinearScaleProcessor.Create(Self);
  FYLinearScale:=TLinearScaleProcessor.Create(Self);
end;

destructor  THMIControlDislocatorAnimation.Destroy;
begin

  if FTag<>nil then begin
    FTag.RemoveCallBacks(self as IHMITagInterface);
  end;

  FXLinearScale.Destroy;
  FYLinearScale.Destroy;
  inherited Destroy;
end;

procedure THMIControlDislocatorAnimation.Loaded;
begin
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.MoveObject;
var
  outX, outY:Double;
begin
  if (FTarget=nil) or (FTag=nil) then exit;

  FXLinearScale.Input:=FTag.Value;
  FYLinearScale.Input:=FTag.Value;

  if FMinX and (FXLinearScale.Output<FMinXValue) then
     outx:=FMinXValue
  else
     outx:=FXLinearScale.Output;

  if FMaxX and (FXLinearScale.Output>FMaxXValue) then
     outx:=FMaxXValue
  else
     outx:=FXLinearScale.Output;


  if FMinY and (FYLinearScale.Output<FMinYValue) then
     outY:=FMinYValue
  else
     outY:=FYLinearScale.Output;

  if FMaxY and (FYLinearScale.Output>FMaxYValue) then
     outY:=FMaxYValue
  else
     outY:=FYLinearScale.Output;

  FTarget.Left:=trunc(outX);
  FTarget.Top :=trunc(outY);
end;

procedure THMIControlDislocatorAnimation.SetStartLeft(v:Integer);
begin
  FStartLeft:=v;
  FXLinearScale.SysMin:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetStartTop(v:Integer);
begin
  FStartTop:=v;
  FYLinearScale.SysMin:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetEndLeft(v:Integer);
begin
  FEndLeft:=v;
  FXLinearScale.SysMax:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetEndTop(v:Integer);
begin
  FEndTop:=v;
  FYLinearScale.SysMax:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetValueStart(v:Double);
begin
  FStartValue:=v;
  FXLinearScale.PLCMin:=v;
  FYLinearScale.PLCMin:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetValueEnd(v:Double);
begin
  FEndValue:=v;
  FXLinearScale.PLCMax:=v;
  FYLinearScale.PLCMax:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetPLCTag(t:TPLCNumber);
begin
  //se o tag esta entre um dos aceitos.
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  if FTag<>nil then begin
    FTag.RemoveCallBacks(self as IHMITagInterface);
  end;

  //adiona o callback para o novo tag
  if t<>nil then begin
    t.AddCallBacks(self as IHMITagInterface);
    FTag := t;
    MoveObject;
  end;
  FTag := t;
end;

procedure THMIControlDislocatorAnimation.SetControl(t:TControl);
begin
  if t=FTarget then exit;

  if FTarget<>nil then begin
    FTarget.Left:=FStartLeft;
    FTarget.Top:=FStartTop;
  end;

  FTarget:=t;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.PropertyDoesNothing(v:String);
begin

end;

procedure THMIControlDislocatorAnimation.NotifyReadOk;
begin

end;

procedure THMIControlDislocatorAnimation.NotifyReadFault;
begin

end;

procedure THMIControlDislocatorAnimation.NotifyWriteOk;
begin

end;

procedure THMIControlDislocatorAnimation.NotifyWriteFault;
begin

end;

procedure THMIControlDislocatorAnimation.NotifyTagChange(Sender:TObject);
begin
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.RemoveTag(Sender:TObject);
begin
  if FTag=Sender then
     FTag:=nil;
end;

procedure THMIControlDislocatorAnimation.SetEnableMinX(v:Boolean);
begin
  FMinX:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetEnableMaxX(v:Boolean);
begin
  FMaxX:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetEnableMinY(v:Boolean);
begin
  FMinY:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetEnableMaxY(v:Boolean);
begin
  FMinY:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetMinX(v:Integer);
begin
  FMinXValue:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetMaxX(v:Integer);
begin
  FMaxXValue:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetMinY(v:Integer);
begin
  FMinYValue:=v;
  MoveObject;
end;

procedure THMIControlDislocatorAnimation.SetMaxY(v:Integer);
begin
  FMaxYValue:=v;
  MoveObject;
end;

end.
