unit HMIKeyboardManager;

{$mode delphi}

interface

uses
  Classes, Forms, Controls, sysutils, unumerickeyboard, ualfakeyboard;

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


  THMIFocusChangeEvent = procedure(FocusedControl:TControl;
                                   var KeyboarTypeForControl:TOnScreenKeyboard;
                                   var NumericKBOptions:TNumericScreenKeyboardOptions;
                                   var AlphaNumKBOptions:TAlphaNumericScreenKeyBoardOptions) of object;

  { THMIKeyboardManager }

  THMIKeyboardManager = class(TComponent)
  private
    function SameMethod(AMethod1, AMethod2: TNotifyEvent): boolean;
  protected
    FOldOnEnterEvent,
    FOldOnClickEvent,
    FOldOnExitEvent:TNotifyEvent;
    FNumericKeyBoard:TpsHMIfrmNumericKeyBoard;
    FAlphaNumericKeyboard:TpsHMIfrmAlphaKeyboard;
    FOnFocusChange: THMIFocusChangeEvent;
    FLastFocusedControl:TWinControl;
    FKeyboarTypeForControl:TOnScreenKeyboard;
    FNumericKBOptions:TNumericScreenKeyboardOptions;
    FAlphaNumKBOptions:TAlphaNumericScreenKeyBoardOptions;
    procedure ControlFocusChanged(Sender: TObject; LastControl: TControl);
    procedure NumKBClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure AlphaKBClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure ShowKeyboard(sender:TObject);
    procedure ClickEvent(Sender:TObject);
    procedure EnterEvent(Sender:TObject);
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

  if FLastFocusedControl<>nil then begin
     if SameMethod(FLastFocusedControl.OnClick, EnterEvent) then FLastFocusedControl.OnClick:=FOldOnClickEvent;
     if SameMethod(FLastFocusedControl.OnEnter, EnterEvent) then FLastFocusedControl.OnEnter:=FOldOnEnterEvent;
     if SameMethod(FLastFocusedControl.OnExit,  ExitEvent)  then FLastFocusedControl.OnExit :=FOldOnExitEvent;
  end;

  try
    if fLastControl<>nil then begin
      FOldOnClickEvent := fLastControl.OnClick;
      FOldOnEnterEvent := fLastControl.OnEnter;
      FOldOnExitEvent  := fLastControl.OnExit;
      if Assigned(FOnFocusChange) then
        OnFocusChange(fLastControl,
                      FKeyboarTypeForControl,
                      FNumericKBOptions,
                      FAlphaNumKBOptions);
      if FKeyboarTypeForControl<>oskNone then begin
        fLastControl.OnClick:=ClickEvent;
        fLastControl.OnEnter:=EnterEvent;
        fLastControl.OnExit :=ExitEvent;
      end;
    end;
  finally
    if ((fLastControl<>FNumericKeyBoard) and (fLastControl<>FAlphaNumericKeyboard)) or (fLastControl=nil) then
      FLastFocusedControl:=fLastControl;
  end;
end;

function THMIKeyboardManager.SameMethod(AMethod1, AMethod2: TNotifyEvent): boolean;
begin
  result := (TMethod(AMethod1).Code = TMethod(AMethod2).Code)
            and (TMethod(AMethod1).Data = TMethod(AMethod2).Data);
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

procedure THMIKeyboardManager.ShowKeyboard(sender: TObject);
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
      FAlphaNumericKeyboard:=TpsHMIfrmAlphaKeyboard.Create(Self,
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

procedure THMIKeyboardManager.ClickEvent(Sender: TObject);
begin
  ShowKeyboard(Sender);

  if Assigned(FOldOnClickEvent) then
    FOldOnClickEvent(Sender);
end;

procedure THMIKeyboardManager.EnterEvent(Sender: TObject);
begin
  ShowKeyboard(Sender);

  if Assigned(FOldOnEnterEvent) then
    FOldOnEnterEvent(Sender);
end;

procedure THMIKeyboardManager.ExitEvent(Sender: TObject);
begin
 CloseAlphaKB;
 CloseNumKB;

 if Assigned(FOldOnExitEvent) then
   FOldOnExitEvent(Sender);
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
  FAlphaNumericKeyboard:=nil;
  FNumericKeyBoard:=nil;
  Screen.AddHandlerActiveControlChanged(ControlFocusChanged);
end;

destructor THMIKeyboardManager.Destroy;
begin
  Screen.RemoveHandlerActiveControlChanged(ControlFocusChanged);
  inherited Destroy;
end;

end.
