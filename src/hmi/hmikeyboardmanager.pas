unit HMIKeyboardManager;

{$mode delphi}

interface

uses
  Classes, Forms, Controls, sysutils, HMINumericKeyboard, HMIAlfaKeyboard;

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
                                       askoShowNumbers,
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
    FAlphaNumericKeyboard:TpSCADAfrmAlphaKeyboard;
    FOnFocusChange: THMIFocusChangeEvent;
    FLastFocusedControl:TWinControl;
    FKeyboarTypeForControl:TOnScreenKeyboard;
    FNumericKBOptions:TNumericScreenKeyboardOptions;
    FAlphaNumKBOptions:TAlphaNumericScreenKeyBoardOptions;
    procedure ControlFocusChanged(Sender: TObject; LastControl: TControl);
    procedure NumKBClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure AlphaKBClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure ClickEvent(Sender:TObject);
    procedure ExitEvent(Sender:TObject);
    procedure CloseNumKB;
    procedure CloseAlphaKB;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
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

  if (fLastControl<>FLastFocusedControl) and
     (fLastControl<>FNumericKeyBoard) and
     (fLastControl<>FAlphaNumericKeyboard) then begin
     CloseAlphaKB;
     CloseNumKB;
   end;

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
    if ((fLastControl<>FNumericKeyBoard) and (fLastControl<>FAlphaNumericKeyboard)) or (fLastControl=nil) then
      FLastFocusedControl:=fLastControl;
  end;
end;

procedure THMIKeyboardManager.NumKBClosed(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FNumericKeyBoard:=nil;
end;

procedure THMIKeyboardManager.AlphaKBClosed(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FAlphaNumericKeyboard:=nil;
end;

procedure THMIKeyboardManager.ClickEvent(Sender: TObject);
begin
  case FKeyboarTypeForControl of
    oskNone:
      ExitEvent(nil);
    oskNumeric: begin
      CloseAlphaKB;
      FNumericKeyBoard:=TpsHMIfrmNumericKeyBoard.Create(Self,
                                                        FLastFocusedControl,
                                                        nskoShowMinus in FNumericKBOptions,
                                                        nskoShowDecimalPoint in FNumericKBOptions);
      FNumericKeyBoard.OnClose:=NumKBClosed;
      FNumericKeyBoard.ShowAlongsideOfTheTarget;
    end;
    oskAlphaNumeric: begin
      CloseNumKB;
      FAlphaNumericKeyboard:=TpSCADAfrmAlphaKeyboard.Create(Self,
                                                            FLastFocusedControl,
                                                            askoShowFxxKeys in FAlphaNumKBOptions,
                                                            askoShowTab in FAlphaNumKBOptions,
                                                            askoShowCaps in FAlphaNumKBOptions,
                                                            askoShowShift in FAlphaNumKBOptions,
                                                            askoShowCtrl in FAlphaNumKBOptions,
                                                            askoShowAlt in FAlphaNumKBOptions,
                                                            askoShowSymbols in FAlphaNumKBOptions,
                                                            askoShowNumbers in FAlphaNumKBOptions,
                                                            askoShowFastNavigation in FAlphaNumKBOptions,
                                                            askoShowNavigation in FAlphaNumKBOptions,
                                                            askoCloseOnPressEnter in FAlphaNumKBOptions);
      FAlphaNumericKeyboard.OnClose:=AlphaKBClosed;
      FAlphaNumericKeyboard.ShowAlongsideOfTheTarget;
    end;
  end;
end;

procedure THMIKeyboardManager.ExitEvent(Sender: TObject);
begin
 CloseAlphaKB;
 CloseNumKB;
end;

procedure THMIKeyboardManager.CloseNumKB;
begin
  if Assigned(FNumericKeyBoard) then begin
    FNumericKeyBoard.Close;
    FNumericKeyBoard:=nil;
  end;
end;

procedure THMIKeyboardManager.CloseAlphaKB;
begin
  if Assigned(FAlphaNumericKeyboard) then begin
    FAlphaNumericKeyboard.Close;
    FAlphaNumericKeyboard:=nil;
  end;
end;

procedure THMIKeyboardManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (FLastFocusedControl=AComponent) then begin
    FLastFocusedControl:=nil;
    ExitEvent(nil);
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
