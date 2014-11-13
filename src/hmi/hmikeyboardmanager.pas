unit HMIKeyboardManager;

{$mode delphi}

interface

uses
  Classes, Forms, Controls, unumerickeyboard, ualfakeyboard;

type

  TOnScreenKeyboard = (oskNone, oskNumeric, oskAlphaNumeric);

  TNumericScreenKeyboardOption = (nskoShowMinus, nskoShowDecimalPoint);
  TNumericScreenKeyboardOptions = set of TNumericScreenKeyboardOption;

  TAlphaNumericScreenKeyBoardOption = (askoShowFxxKeys,
                                       askoShowTab,
                                       askoShowCaps,
                                       askoShowShift,
                                       askoShowCtrl,
                                       askoShowAlt,
                                       askoShowSymbols,
                                       askoShowFastNavigation,
                                       askoShowNavigation,
                                       askoCloseOnPressEnter);
  TAlphaNumericScreenKeyBoardOptions = set of TAlphaNumericScreenKeyBoardOption;


  THMIFocusChangeEvent = procedure(oldFocusedControl,
                                   newFocusedControl:TControl;
                                   const ClickEvent, ExitEvent:TNotifyEvent;
                                   var KeyboarTypeForControl:TOnScreenKeyboard;
                                   var NumericKBOptions:TNumericScreenKeyboardOptions;
                                   var AlphaNumKBOptions:TAlphaNumericScreenKeyBoardOptions) of object;

  { THMIKeyboardManager }

  THMIKeyboardManager = class(TComponent)
  protected
    FNumericKeyBoard:TpsHMIfrmNumericKeyBoard;
    FAlphaNumericKeyboard:TpsHMIfrmAlphaKeyboard;
    FOnFocusChange: THMIFocusChangeEvent;
    FLastFocusedControl:TWinControl;
    FKeyboarTypeForControl:TOnScreenKeyboard;
    FNumericKBOptions:TNumericScreenKeyboardOptions;
    FAlphaNumKBOptions:TAlphaNumericScreenKeyBoardOptions;
    procedure ControlFocusChanged(Sender: TObject; LastControl: TControl);
    procedure CloseNumKB(Sender: TObject; var CloseAction: TCloseAction);
    procedure CloseAlphaKB(Sender: TObject; var CloseAction: TCloseAction);
    procedure ClickEvent(Sender:TObject);
    procedure ExitEvent(Sender:TObject);
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
var
  fLastControl: TWinControl;
begin
  if assigned(LastControl) and (LastControl is TWinControl) then
    fLastControl:=TWinControl(LastControl)
  else
    fLastControl:=nil;

  try
    if Assigned(FOnFocusChange) then
      OnFocusChange(FLastFocusedControl,
                    fLastControl,
                    ClickEvent,
                    ExitEvent,
                    FKeyboarTypeForControl,
                    FNumericKBOptions,
                    FAlphaNumKBOptions);
  finally
    FLastFocusedControl:=fLastControl;
  end;
end;

procedure THMIKeyboardManager.CloseNumKB(Sender: TObject; var CloseAction: TCloseAction);
begin
  FNumericKeyBoard:=nil;
end;

procedure THMIKeyboardManager.CloseAlphaKB(Sender: TObject; var CloseAction: TCloseAction);
begin
  FAlphaNumericKeyboard:=nil;
end;

procedure THMIKeyboardManager.ClickEvent(Sender: TObject);
begin
  case FKeyboarTypeForControl of
    oskNone:
      ExitEvent(nil);
    oskNumeric: begin
      FNumericKeyBoard:=TpsHMIfrmNumericKeyBoard.Create(FLastFocusedControl,
                                                        FLastFocusedControl,
                                                        nskoShowMinus in FNumericKBOptions,
                                                        nskoShowDecimalPoint in FNumericKBOptions);
      FNumericKeyBoard.OnClose:=CloseNumKB;
      FNumericKeyBoard.ShowAlongsideOfTheTarget;
    end;
    oskAlphaNumeric: begin
      FAlphaNumericKeyboard:=TpsHMIfrmAlphaKeyboard.Create(FLastFocusedControl,
                                                           FLastFocusedControl,
                                                           askoShowFxxKeys in FAlphaNumKBOptions,
                                                           askoShowTab in FAlphaNumKBOptions,
                                                           askoShowCaps in FAlphaNumKBOptions,
                                                           askoShowShift in FAlphaNumKBOptions,
                                                           askoShowCtrl in FAlphaNumKBOptions,
                                                           askoShowAlt in FAlphaNumKBOptions,
                                                           askoShowSymbols in FAlphaNumKBOptions,
                                                           askoShowFastNavigation in FAlphaNumKBOptions,
                                                           askoShowNavigation in FAlphaNumKBOptions,
                                                           askoCloseOnPressEnter in FAlphaNumKBOptions);
      FAlphaNumericKeyboard.OnClose:=CloseAlphaKB;
      FAlphaNumericKeyboard.ShowAlongsideOfTheTarget;
    end;
  end;
end;

procedure THMIKeyboardManager.ExitEvent(Sender: TObject);
begin
  if Assigned(FNumericKeyBoard) then begin
    FNumericKeyBoard.Close;
    FNumericKeyBoard:=nil;
  end;
  if Assigned(FAlphaNumericKeyboard) then begin
    FAlphaNumericKeyboard.Close;
    FAlphaNumericKeyboard:=nil;
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
