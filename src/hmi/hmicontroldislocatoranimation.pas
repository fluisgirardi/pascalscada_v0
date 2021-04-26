{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Controle de deslocamento de controles (animação) nos eixos X e Y da janela.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a component that moves controls on axis X and Y
  of the window.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIControlDislocatorAnimation;

interface

uses
  Classes, SysUtils, Controls, LinearScaleProcessor, PLCNumber, Tag,
  ProtocolTypes;

type

  {$IFDEF PORTUGUES}
  {:
  @abstract(Controle de deslocamento de controles (animação) nos eixos X e Y da janela.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  @abstract(Class of component that moves controls on axis X and Y of the window.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}

  { THMIControlDislocatorAnimation }

  THMICustomControlDislocatorAnimation = class(TComponent)
  private
    FStartLeft,
    FEndLeft:LongInt;
    FStartTop,
    FEndTop:LongInt;
    FStartValue,
    FEndValue:Double;
    FTarget:TControl;
    FXLinearScale,
    FYLinearScale:TLinearScaleProcessor;
    FTag, FTagLoaded:TPLCNumber;
    FMinX, FMaxX,
    FMinY, FMaxY:Boolean;
    FMinXValue, FMaxXValue,
    FMinYValue, FMaxyValue:LongInt;

    FGetPositionP0,
    FGetPositionP1,
    FGoToP0:UTF8String;

    procedure SetStartLeft(v:LongInt);
    procedure SetStartTop(v:LongInt);
    procedure SetEndLeft(v:LongInt);
    procedure SetEndTop(v:LongInt);
    procedure SetControl(t:TControl);

    procedure PropertyDoesNothing(v:UTF8String);

    procedure SetEnableMinX(v:Boolean);
    procedure SetEnableMaxX(v:Boolean);
    procedure SetEnableMinY(v:Boolean);
    procedure SetEnableMaxY(v:Boolean);

    procedure SetMinX(v:LongInt);
    procedure SetMaxX(v:LongInt);
    procedure SetMinY(v:LongInt);
    procedure SetMaxY(v:LongInt);

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
  protected
    //@exclude
    procedure MoveObject(DataPtr:PtrInt); virtual;
    //@exclude
    procedure Loaded; override;
    procedure SetPLCTag(t:TPLCNumber);

    procedure SetValueStart(v:Double); virtual;
    procedure SetValueEnd(v:Double); virtual;

    {$IFDEF PORTUGUES}
    //: Tag numérico que irá controlar a animação.
    {$ELSE}
    //: Numeric tag that will control the animation.
    {$ENDIF}
    property PLCTag:TPLCNumber read FTag write SetPLCTag;
    {$IFDEF PORTUGUES}
    //: Valor do tag que irá fazer o controle ir para as coordenadas Inicias (P0_X; P0_Y);
    {$ELSE}
    //: Value of the tag that will move the control to the Initial coordinates (P0_X; P0_Y);
    {$ENDIF}
    property ValueP0:Double read FStartValue write SetValueStart;

    {$IFDEF PORTUGUES}
    //: Valor do tag que irá fazer o controle ir para as coordenadas finais (P1_X; P1_Y);
    {$ELSE}
    //: Value of the tag that will move the control to the final coordinates (P1_X; P1_Y);
    {$ENDIF}
    property ValueP1:Double read FEndValue write SetValueEnd;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    procedure SetPositionsAndMinMax(vP0X,vP0Y,vP1X,vP1Y: LongInt);
  published
    {$IFDEF PORTUGUES}
    //: Posição inicial no eixo X (propriedade Left do controle)
    {$ELSE}
    //: Initial position on X axis (Left property of the control)
    {$ENDIF}
    property P0_X:LongInt read FStartLeft write SetStartLeft;

    {$IFDEF PORTUGUES}
    //: Posição inicial no eixo Y (propriedade Top do controle)
    {$ELSE}
    //: Initial position on Y axis (Top property of the control)
    {$ENDIF}
    property P0_Y:LongInt read FStartTop write SetStartTop;

    {$IFDEF PORTUGUES}
    //: Posição final no eixo X (propriedade Left do controle)
    {$ELSE}
    //: Final position on X axis (Left property of the control)
    {$ENDIF}
    property P1_X:LongInt read FEndLeft write SetEndLeft;

    {$IFDEF PORTUGUES}
    //: Posição final no eixo y (propriedade Top do controle)
    {$ELSE}
    //: Final position on Y axis (Top property of the control)
    {$ENDIF}
    property P1_Y:LongInt read FEndTop write SetEndTop;

    {$IFDEF PORTUGUES}
    //: Controle que será manipulado.
    {$ELSE}
    //: Control that will be manipulated.
    {$ENDIF}
    property Control:TControl read FTarget write SetControl;

    {$IFDEF PORTUGUES}
    //: Habilita o valor mínimo no eixo X.
    {$ELSE}
    //: Enables a minimum value on X axis.
    {$ENDIF}
    property EnableXMin:Boolean read FMinX write SetEnableMinX;

    {$IFDEF PORTUGUES}
    //: Habilita o valor máximo no eixo X.
    {$ELSE}
    //: Enables a maximum value on X axis.
    {$ENDIF}
    property EnableXMax:Boolean read FMaxX write SetEnableMaxX;

    {$IFDEF PORTUGUES}
    //: Habilita o valor mínimo no eixo Y.
    {$ELSE}
    //: Enables a minimum value on Y axis.
    {$ENDIF}
    property EnableYMin:Boolean read FMinY write SetEnableMinY;

    {$IFDEF PORTUGUES}
    //: Habilita o valor máximo no eixo Y.
    {$ELSE}
    //: Enables a maximum value on Y axis.
    {$ENDIF}
    property EnableYMax:Boolean read FMaxY write SetEnableMaxY;

    {$IFDEF PORTUGUES}
    //: Caso valor mínimo de X caso EnableXMin seja @true.
    {$ELSE}
    //: Minimum value of X axis if EnableXMin is @true.
    {$ENDIF}
    property MinXValue:LongInt read FMinXValue write SetMinX;

    {$IFDEF PORTUGUES}
    //: Caso valor máximo de X caso EnableXMax seja @true.
    {$ELSE}
    //: Minimum value of X axis if EnableXMax is @true.
    {$ENDIF}
    property MaxXValue:LongInt read FMaxXValue write SetMaxX;

    {$IFDEF PORTUGUES}
    //: Caso valor mínimo de Y caso EnableyMin seja @true.
    {$ELSE}
    //: Minimum value of y axis if EnableYMin is @true.
    {$ENDIF}
    property MinYValue:LongInt read FMinYValue write SetMinY;

    {$IFDEF PORTUGUES}
    //: Caso valor máximo de Y caso EnableYMax seja @true.
    {$ELSE}
    //: Minimum value of Y axis if EnableYMax is @true.
    {$ENDIF}
    property MaxYValue:LongInt read FMaxyValue write SetMaxY;

    {$IFDEF PORTUGUES}
    //: Pega a posição atual do controle como posição inicial (P0).
    {$ELSE}
    //: Gets the actual position of the control as the initial position (P0).
    {$ENDIF}
    property Gets_P0_Position:UTF8String read FGetPositionP0 write PropertyDoesNothing;
    {$IFDEF PORTUGUES}
    //: Pega a posição atual do controle como posição final (P1).
    {$ELSE}
    //: Gets the actual position of the control as the final position (P1).
    {$ENDIF}
    property Gets_P1_Position:UTF8String read FGetPositionP1 write PropertyDoesNothing;

    {$IFDEF PORTUGUES}
    //: Reposiciona o controle para a posição inicial definida.
    {$ELSE}
    //: Puts the control in the initial position defined.
    {$ENDIF}
    property GoTo_P0_Position:UTF8String read FGoToP0        write PropertyDoesNothing;
  end;

  THMIControlDislocatorAnimation = class(THMICustomControlDislocatorAnimation)
  published
    property PLCTag;
    property ValueP0;
    property ValueP1;
  end;

  { THMIControlDislocatorAnimation2 }

  THMIControlDislocatorAnimation2 = Class(THMICustomControlDislocatorAnimation)
  private
    FEndValueY: Double;
    FStartValueY: Double;
    FTagY, FTagYLoaded: TPLCNumber;
    procedure SetValueEndY(AValue: Double);
    procedure SetValueStartY(AValue: Double);
  protected
    procedure MoveObject(DataPtr: PtrInt); override;
    procedure SetPLCTagY(t: TPLCNumber);
    procedure Loaded; override;
    procedure SetValueStart(v:Double); override;
    procedure SetValueEnd(v:Double); override;
  public
    destructor Destroy; override;
  published
    {$IFDEF PORTUGUES}
    //: Tag numérico que irá controlar a animação.
    {$ELSE}
    //: Numeric tag that will control the animation.
    {$ENDIF}
    property PLCTagX:TPLCNumber read FTag write SetPLCTag;
    {$IFDEF PORTUGUES}
    //: Tag numérico que irá controlar a animação.
    {$ELSE}
    //: Numeric tag that will control the animation.
    {$ENDIF}
    property PLCTagy:TPLCNumber read FTagY write SetPLCTagY;

    {$IFDEF PORTUGUES}
    //: Valor do tag que irá fazer o controle ir para as coordenadas Inicias (P0_X; P0_Y);
    {$ELSE}
    //: Value of the tag that will move the control to the Initial coordinates (P0_X; P0_Y);
    {$ENDIF}
    property ValueP0x:Double read FStartValue write SetValueStart;

    {$IFDEF PORTUGUES}
    //: Valor do tag que irá fazer o controle ir para as coordenadas finais (P1_X; P1_Y);
    {$ELSE}
    //: Value of the tag that will move the control to the final coordinates (P1_X; P1_Y);
    {$ENDIF}
    property ValueP1x:Double read FEndValue write SetValueEnd;

    {$IFDEF PORTUGUES}
    //: Valor do tag que irá fazer o controle ir para as coordenadas Inicias (P0_X; P0_Y);
    {$ELSE}
    //: Value of the tag that will move the control to the Initial coordinates (P0_X; P0_Y);
    {$ENDIF}
    property ValueP0y:Double read FStartValueY write SetValueStartY;

    {$IFDEF PORTUGUES}
    //: Valor do tag que irá fazer o controle ir para as coordenadas finais (P1_X; P1_Y);
    {$ELSE}
    //: Value of the tag that will move the control to the final coordinates (P1_X; P1_Y);
    {$ENDIF}
    property ValueP1y:Double read FEndValueY write SetValueEndY;
  end;


implementation

uses hsstrings, Forms;

{ THMIControlDislocatorAnimation2 }

procedure THMIControlDislocatorAnimation2.SetValueEndY(AValue: Double);
begin
  FEndValueY:=AValue;
  FYLinearScale.PLCMax:=AValue;
  MoveObject(0);
end;

procedure THMIControlDislocatorAnimation2.SetValueStartY(AValue: Double);
begin
  FStartValueY:=AValue;
  FYLinearScale.PLCMin:=AValue;
  MoveObject(0);
end;

procedure THMIControlDislocatorAnimation2.MoveObject(DataPtr: PtrInt);
var
  outX, outY:Double;
begin
  if [csReading,csLoading,csDestroying]*ComponentState<>[] then exit;
  if (FTarget=nil) or (FTag=nil) or (FTagY=nil) then exit;

  FXLinearScale.Input:=FTag.Value;
  FYLinearScale.Input:=FTagY.Value;

  if FMinX and (FXLinearScale.Output<FMinXValue) then
    outx:=FMinXValue
  else begin
    if FMaxX and (FXLinearScale.Output>FMaxXValue) then
      outx:=FMaxXValue
    else
      outx:=FXLinearScale.Output;
  end;


  if FMinY and (FYLinearScale.Output<FMinYValue) then
     outY:=FMinYValue
  else begin
    if FMaxY and (FYLinearScale.Output>FMaxYValue) then
      outY:=FMaxYValue
    else
      outY:=FYLinearScale.Output;
  end;

  if FStartLeft<>FEndLeft then
    FTarget.Left:=trunc(outX);
  if FStartTop<>FEndTop then
    FTarget.Top :=trunc(outY);
end;

procedure THMIControlDislocatorAnimation2.SetPLCTagY(t: TPLCNumber);
begin
  if [csReading,csLoading]*ComponentState<>[] then begin
    FTagYLoaded:=t;
    exit;
  end;

  //se o tag esta entre um dos aceitos.
  //Checks if the tag is valid (only numeric tag are acceptable)
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  //if the control is linked with some tag, remove the old link.
  if FTagY<>nil then begin
    FTagY.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if t<>nil then begin
    t.AddWriteFaultHandler(@WriteFaultCallBack);
    t.AddTagChangeHandler(@TagChangeCallBack);
    t.AddRemoveTagHandler(@RemoveTagCallBack);
    FTagY := t;
    MoveObject(0);
  end;
  FTagY := t;
end;

procedure THMIControlDislocatorAnimation2.Loaded;
begin
  inherited Loaded;
end;

procedure THMIControlDislocatorAnimation2.SetValueStart(v: Double);
begin
  FStartValue:=v;
  FXLinearScale.PLCMin:=v;
  MoveObject(0);
end;

procedure THMIControlDislocatorAnimation2.SetValueEnd(v: Double);
begin
  FEndValue:=v;
  FXLinearScale.PLCMax:=v;
  MoveObject(0);
end;

destructor THMIControlDislocatorAnimation2.Destroy;
begin
  if Assigned(FTagY) then
     FTagY.RemoveAllHandlersFromObject(Self);
  inherited Destroy;
end;

constructor THMICustomControlDislocatorAnimation.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FGetPositionP0:=SGetP0;
  FGetPositionP1:=SGetP1;
  FGoToP0:=SGotoP0;
  FXLinearScale:=TLinearScaleProcessor.Create(Self);
  FYLinearScale:=TLinearScaleProcessor.Create(Self);
end;

destructor  THMICustomControlDislocatorAnimation.Destroy;
begin
  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);

  FXLinearScale.Destroy;
  FYLinearScale.Destroy;
  Application.RemoveAsyncCalls(Self);
  inherited Destroy;
end;

procedure THMICustomControlDislocatorAnimation.SetPositionsAndMinMax(vP0X, vP0Y,
  vP1X, vP1Y: LongInt);
begin

  if vP0X>vP1X then begin
    SetMaxX(vP0X);
    SetMinX(vP1X);
  end else begin
    SetMaxX(vP1X);
    SetMinX(vP0X);
  end;

  if vP0Y>vP1Y then begin
    SetMaxY(vP0Y);
    SetMinY(vP1Y);
  end else begin
    SetMaxY(vP1Y);
    SetMinY(vP0Y);
  end;

  SetStartLeft(vP0X);
  SetEndLeft(vP1X);
  SetStartTop(vP0Y);
  SetEndTop(vP1Y);

  EnableXMax:=True;
  EnableXMin:=True;
  EnableYMax:=True;
  EnableYMin:=True;

end;

procedure THMICustomControlDislocatorAnimation.Loaded;
begin
  inherited Loaded;
  SetPLCTag(FTagLoaded);
  TagChangeCallBack(Self);
end;

procedure THMICustomControlDislocatorAnimation.MoveObject(DataPtr: PtrInt);
var
  outX, outY:Double;
begin
  if [csReading,csLoading,csDestroying]*ComponentState<>[] then exit;
  if (FTarget=nil) or (FTag=nil) then exit;

  FXLinearScale.Input:=FTag.Value;
  FYLinearScale.Input:=FTag.Value;

  if FMinX and (FXLinearScale.Output<FMinXValue) then
    outx:=FMinXValue
  else begin
    if FMaxX and (FXLinearScale.Output>FMaxXValue) then
      outx:=FMaxXValue
    else
      outx:=FXLinearScale.Output;
  end;


  if FMinY and (FYLinearScale.Output<FMinYValue) then
     outY:=FMinYValue
  else begin
    if FMaxY and (FYLinearScale.Output>FMaxYValue) then
      outY:=FMaxYValue
    else
      outY:=FYLinearScale.Output;
  end;

  if FStartLeft<>FEndLeft then
    FTarget.Left:=trunc(outX);
  if FStartTop<>FEndTop then
    FTarget.Top :=trunc(outY);
end;

procedure THMICustomControlDislocatorAnimation.SetStartLeft(v:LongInt);
begin
  FStartLeft:=v;
  FXLinearScale.SysMin:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetStartTop(v:LongInt);
begin
  FStartTop:=v;
  FYLinearScale.SysMin:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetEndLeft(v:LongInt);
begin
  FEndLeft:=v;
  FXLinearScale.SysMax:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetEndTop(v:LongInt);
begin
  FEndTop:=v;
  FYLinearScale.SysMax:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetValueStart(v:Double);
begin
  FStartValue:=v;
  FXLinearScale.PLCMin:=v;
  FYLinearScale.PLCMin:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetValueEnd(v:Double);
begin
  FEndValue:=v;
  FXLinearScale.PLCMax:=v;
  FYLinearScale.PLCMax:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetPLCTag(t:TPLCNumber);
begin
  if [csReading,csLoading]*ComponentState<>[] then begin
    FTagLoaded:=t;
    exit;
  end;
  //se o tag esta entre um dos aceitos.
  //Checks if the tag is valid (only numeric tag are acceptable)
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  //if the control is linked with some tag, remove the old link.
  if FTag<>nil then begin
    FTag.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if t<>nil then begin
    t.AddWriteFaultHandler(@WriteFaultCallBack);
    t.AddTagChangeHandler(@TagChangeCallBack);
    t.AddRemoveTagHandler(@RemoveTagCallBack);
    FTag := t;
    MoveObject(0);
  end;
  FTag := t;
end;

procedure THMICustomControlDislocatorAnimation.SetControl(t:TControl);
begin
  if t=FTarget then exit;

  if FTarget<>nil then begin
    FTarget.Left:=FStartLeft;
    FTarget.Top:=FStartTop;
  end;

  FTarget:=t;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.PropertyDoesNothing(v: UTF8String);
begin

end;

procedure THMICustomControlDislocatorAnimation.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMICustomControlDislocatorAnimation.TagChangeCallBack(Sender: TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    Application.QueueAsyncCall(@MoveObject,0);
end;

procedure THMICustomControlDislocatorAnimation.RemoveTagCallBack(Sender: TObject);
begin
  if FTag=Sender then
     FTag:=nil;
end;

procedure THMICustomControlDislocatorAnimation.SetEnableMinX(v:Boolean);
begin
  FMinX:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetEnableMaxX(v:Boolean);
begin
  FMaxX:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetEnableMinY(v:Boolean);
begin
  FMinY:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetEnableMaxY(v:Boolean);
begin
  FMaxY:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetMinX(v:LongInt);
begin
  FMinXValue:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetMaxX(v:LongInt);
begin
  FMaxXValue:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetMinY(v:LongInt);
begin
  FMinYValue:=v;
  MoveObject(0);
end;

procedure THMICustomControlDislocatorAnimation.SetMaxY(v:LongInt);
begin
  FMaxYValue:=v;
  MoveObject(0);
end;

end.
