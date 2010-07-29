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
  Classes, SysUtils, Controls, LinearScaleProcessor;

type
  TControlDislocatorAnimation = class(TComponent)
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
  end;


implementation

end.
