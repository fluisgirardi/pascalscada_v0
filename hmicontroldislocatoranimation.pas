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

    procedure MoveObject;
    procedure SetStartLeft(v:Integer);
    procedure SetStartTop(v:Integer);
    procedure SetEndLeft(v:Integer);
    procedure SetEndTop(v:Integer);
    procedure SetValueStart(v:Double);
    procedure SetValueEnd(v:Double);
    procedure SetPLCTag(t:TPLCNumber);
    procedure SetControl(t:TControl);

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
    property StartX:Integer read FStartLeft write SetStartLeft;
    property StartY:Integer read FStartTop write SetStartTop;
    property EndX:Integer read FEndLeft write SetEndLeft;
    property EndY:Integer read FEndTop write SetEndTop;
    property ValueStart:Double read FStartValue write SetValueStart;
    property ValueEnd:Double read FEndValue write SetValueEnd;
    property PLCTag:TPLCNumber read FTag write SetPLCTag;
    property Control:TControl read FTarget write SetControl;
  end;


implementation

uses hsstrings;

constructor THMIControlDislocatorAnimation.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
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
begin
  if (FTarget=nil) or (FTag=nil) then exit;

  FXLinearScale.Input:=FTag.Value;
  FYLinearScale.Input:=FTag.Value;

  FTarget.Left:=trunc(FXLinearScale.Output);
  FTarget.Top :=trunc(FYLinearScale.Output);
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
    FTarget.Left:=StartX;
    FTarget.Top:=StartY;
  end;

  FTarget:=t;
  MoveObject;
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

end.
