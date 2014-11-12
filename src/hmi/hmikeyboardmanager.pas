unit HMIKeyboardManager;

{$mode delphi}

interface

uses
  Classes, Forms, Controls;

type

  THMIFocusChangeEvent = procedure(oldFocusedControl, newFocusedControl:TControl) of object;

  { THMIKeyboardManager }

  THMIKeyboardManager = class(TComponent)
  private
    FOnFocusChange: THMIFocusChangeEvent;
    procedure ControlFocusChanged(Sender: TObject; LastControl: TControl);
  protected
    FLastFocusedControl:TControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnFocusChange:THMIFocusChangeEvent read FOnFocusChange write FOnFocusChange;
  end;

implementation

{ THMIKeyboardManager }

procedure THMIKeyboardManager.ControlFocusChanged(Sender: TObject;
  LastControl: TControl);
begin
  try
    if Assigned(FOnFocusChange) then
      OnFocusChange(FLastFocusedControl,LastControl);
  finally
    FLastFocusedControl:=LastControl
  end;
end;

constructor THMIKeyboardManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Screen.AddHandlerActiveControlChanged(ControlFocusChanged);
end;

destructor THMIKeyboardManager.Destroy;
begin
  Screen.RemoveHandlerActiveControlChanged(ControlFocusChanged);
  inherited Destroy;
end;

end.
