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
    FShowKeyboardOnEnter: Boolean;
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
    procedure PassFocusToLastValidControl(Data: PtrInt);
  published
    property ShowKeyboardOnEnter:Boolean read FShowKeyboardOnEnter write FShowKeyboardOnEnter default false;
    property OnFocusChange:THMIFocusChangeEvent read FOnFocusChange write FOnFocusChange;
  end;

implementation

uses LCLProc;

{ THMIKeyboardManager }

procedure THMIKeyboardManager.ControlFocusChanged(Sender: TObject;
  LastControl: TControl);
var
  fLastControl: TWinControl;
begin
  DebugLn('=====================================================================');
  if FLastFocusedControl<>nil then
    DebugLn('Last control name:', FLastFocusedControl.Name)
  else
    DebugLn('Last Focused Control IS NULL (FLastFocusedControl=nil)');

  if LastControl<>nil then
    DebugLn('Current control name:', LastControl.Name)
  else
    DebugLn('Current focused control IS NULL (LastControl=nil)');

  if LastControl=FNumericKeyBoard then begin
    DebugLn('LastControl=FNumericKeyBoard EXITING!');
    exit;
  end;

  if LastControl=FAlphaNumericKeyboard then begin
    DebugLn('LastControl=FAlphaNumericKeyboard EXITING!');
    exit;
  end;

  if LastControl=FLastFocusedControl then begin
    DebugLn('LastControl=FLastFocusedControl EXITING!');
    exit;
  end;

  if assigned(LastControl) and (LastControl is TWinControl) then begin
    DebugLn('Cur focused control is TWinControl');
    if (not SameMethod(TWinControl(LastControl).OnClick,ClickEvent)) or
       (not SameMethod(TWinControl(LastControl).OnEnter,EnterEvent)) or
       (not SameMethod(TWinControl(LastControl).OnExit, ExitEvent)) then begin
      fLastControl:=TWinControl(LastControl);
      DebugLn('fLastControl:=TWinControl(LastControl)');
    end else begin
      DebugLn('Cur focused control has all events assigned to current handler?!?');
      if not SameMethod(TWinControl(LastControl).OnClick,ClickEvent) then DebugLn('TWinControl(LastControl).OnClick<>ClickEvent');
      if not SameMethod(TWinControl(LastControl).OnEnter,EnterEvent) then DebugLn('TWinControl(LastControl).OnEnter<>EnterEvent');
      if not SameMethod(TWinControl(LastControl).OnExit, ExitEvent)  then DebugLn('TWinControl(LastControl).OnExit<>ExitEvent');
      exit;
    end;
  end else begin
    fLastControl:=nil;
    DebugLn('Cur focused control IS NOT a TWinControl');
  end;

  if (fLastControl<>FLastFocusedControl) and (fLastControl<>FNumericKeyBoard) and (fLastControl<>FAlphaNumericKeyboard) then begin
    DebugLn('Closing all keyboards...');
    CloseAlphaKB;
    CloseNumKB;
  end;

  if FLastFocusedControl<>nil then begin
    DebugLn('Restoring the default event handles for control ',FLastFocusedControl.Name);
    if SameMethod(FLastFocusedControl.OnClick, ClickEvent) then begin
      DebugLn('Restoring OnClick');
      FLastFocusedControl.OnClick:=FOldOnClickEvent;
    end;

    if SameMethod(FLastFocusedControl.OnEnter, EnterEvent) then begin
      DebugLn('Restoring OnEnter');
      FLastFocusedControl.OnEnter:=FOldOnEnterEvent;
    end;

    if SameMethod(FLastFocusedControl.OnExit,  ExitEvent)  then begin
      DebugLn('Restoring OnEnter');
      FLastFocusedControl.OnExit :=FOldOnExitEvent;
    end;
  end;

  try
    if fLastControl<>nil then begin
      DebugLn('Backup of event handlers of the new focused control ',fLastControl.Name);
      FOldOnClickEvent := fLastControl.OnClick;
      FOldOnEnterEvent := fLastControl.OnEnter;
      FOldOnExitEvent  := fLastControl.OnExit;
      if Assigned(FOnFocusChange) then begin
        OnFocusChange(fLastControl,
                      FKeyboarTypeForControl,
                      FNumericKBOptions,
                      FAlphaNumKBOptions);
        DebugLn('OnFocusChange fired');
      end else
        DebugLn('FOnFocusChange event is NULL');
      if FKeyboarTypeForControl=oskNone then begin
        DebugLn('FKeyboarTypeForControl=oskNone');
        fLastControl:=nil;
        FLastFocusedControl:=Nil;
      end else begin
        DebugLn('FKeyboarTypeForControl<>oskNone');
        fLastControl.OnClick:=ClickEvent;
        fLastControl.OnEnter:=EnterEvent;
        fLastControl.OnExit :=ExitEvent;
        DebugLn('setup up of new event handlers...');
      end;
    end else
      DebugLn('fLastControl=nil');
  finally
    if ((fLastControl<>FNumericKeyBoard) and (fLastControl<>FAlphaNumericKeyboard)) or (fLastControl=nil) then begin
      if fLastControl=nil then
        DebugLn('FLastFocusedControl:=fLastControl(NULL)')
      else
        DebugLn('FLastFocusedControl:=fLastControl');
      FLastFocusedControl:=fLastControl;
    end;
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

  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    application.QueueAsyncCall(PassFocusToLastValidControl,0);

  if Assigned(FOldOnClickEvent) then
    FOldOnClickEvent(Sender);
end;

procedure THMIKeyboardManager.EnterEvent(Sender: TObject);
begin
  if FShowKeyboardOnEnter then begin
    ShowKeyboard(Sender);

    if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
      application.QueueAsyncCall(PassFocusToLastValidControl,0);
  end;

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
  FLastFocusedControl:=nil;
  FShowKeyboardOnEnter:=false;
  Screen.AddHandlerActiveControlChanged(ControlFocusChanged);
end;

destructor THMIKeyboardManager.Destroy;
begin
  Screen.RemoveHandlerActiveControlChanged(ControlFocusChanged);
  inherited Destroy;
end;

procedure THMIKeyboardManager.PassFocusToLastValidControl(Data: PtrInt);
begin
  if FLastFocusedControl<>nil then begin
    GetParentForm(FLastFocusedControl).ShowOnTop;
  end;
end;

end.
