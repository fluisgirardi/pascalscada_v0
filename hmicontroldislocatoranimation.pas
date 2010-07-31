{:
  @abstract(Controle de deslocamento de controles (animação) nos eixos X e Y da tela.)
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
    procedure Loaded; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
  published
    property P0_X:Integer read FStartLeft write SetStartLeft;
    property P0_Y:Integer read FStartTop write SetStartTop;
    property P1_X:Integer read FEndLeft write SetEndLeft;
    property P1_Y:Integer read FEndTop write SetEndTop;
    property ValueP0:Double read FStartValue write SetValueStart;
    property ValueP1:Double read FEndValue write SetValueEnd;
    property PLCTag:TPLCNumber read FTag write SetPLCTag;
    property Control:TControl read FTarget write SetControl;

    property EnableXMin:Boolean read FMinX write SetEnableMinX;
    property EnableXMax:Boolean read FMaxX write SetEnableMaxX;
    property EnableYMin:Boolean read FMinY write SetEnableMinY;
    property EnableYMax:Boolean read FMaxY write SetEnableMaxY;

    property MinXValue:Integer read FMinXValue write SetMinX;
    property MaxXValue:Integer read FMaxXValue write SetMaxX;
    property MinYValue:Integer read FMinYValue write SetMinY;
    property MaxYValue:Integer read FMaxyValue write SetMaxY;

    property Gets_P0_Position:String read FGetPositionP0 write PropertyDoesNothing;
    property Gets_P1_Position:String read FGetPositionP1 write PropertyDoesNothing;
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
