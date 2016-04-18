unit ualfakeyboard;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Buttons, ExtCtrls,
  crosskeyevents, pscada_common;

type

  { TpsHMIfrmAlphaKeyboard }

  TpsHMIfrmAlphaKeyboard = class(TForm)
    Btn_0: TSpeedButton;
    Btn_1: TSpeedButton;
    Btn_Q: TSpeedButton;
    Btn_W: TSpeedButton;
    Btn_E: TSpeedButton;
    Btn_R: TSpeedButton;
    Btn_T: TSpeedButton;
    Btn_Y: TSpeedButton;
    Btn_U: TSpeedButton;
    Btn_I: TSpeedButton;
    Btn_O: TSpeedButton;
    Btn_P: TSpeedButton;
    Btn_2: TSpeedButton;
    Btn_A: TSpeedButton;
    Btn_S: TSpeedButton;
    Btn_D: TSpeedButton;
    Btn_F: TSpeedButton;
    Btn_G: TSpeedButton;
    Btn_H: TSpeedButton;
    Btn_J: TSpeedButton;
    Btn_K: TSpeedButton;
    Btn_L: TSpeedButton;
    Btn_Cedilla: TSpeedButton;
    Btn_3: TSpeedButton;
    Btn_Z: TSpeedButton;
    Btn_X: TSpeedButton;
    Btn_C: TSpeedButton;
    Btn_V: TSpeedButton;
    Btn_B: TSpeedButton;
    Btn_N: TSpeedButton;
    Btn_M: TSpeedButton;
    Btn_Comma: TSpeedButton;
    Btn_Dot: TSpeedButton;
    Btn_Semicolon: TSpeedButton;
    Btn_4: TSpeedButton;
    Btn_BackSlash: TSpeedButton;
    Btn_Quote: TSpeedButton;
    Btn_Hyphen: TSpeedButton;
    Btn_Equal: TSpeedButton;
    Btn_SingleQuote: TSpeedButton;
    Btn_BracketOpen: TSpeedButton;
    Btn_Tilde: TSpeedButton;
    Btn_BracketClose: TSpeedButton;
    Btn_Slash: TSpeedButton;
    Btn_F1: TSpeedButton;
    Btn_5: TSpeedButton;
    Btn_F2: TSpeedButton;
    Btn_F3: TSpeedButton;
    Btn_F4: TSpeedButton;
    Btn_F5: TSpeedButton;
    Btn_F6: TSpeedButton;
    Btn_F7: TSpeedButton;
    Btn_F8: TSpeedButton;
    Btn_F9: TSpeedButton;
    Btn_F10: TSpeedButton;
    Btn_F11: TSpeedButton;
    Btn_6: TSpeedButton;
    Btn_F12: TSpeedButton;
    Btn_7: TSpeedButton;
    Btn_8: TSpeedButton;
    Btn_9: TSpeedButton;
    Btn_Back: TSpeedButton;
    Btn_Del: TSpeedButton;
    Btn_Ins: TSpeedButton;
    Btn_End: TSpeedButton;
    Btn_Home: TSpeedButton;
    Btn_PgUp: TSpeedButton;
    Btn_PgDown: TSpeedButton;
    Btn_Esc: TSpeedButton;
    Btn_Tab: TSpeedButton;
    Btn_CtrlR: TSpeedButton;
    Btn_Caps: TSpeedButton;
    Btn_Shift: TSpeedButton;
    Btn_Ctrl: TSpeedButton;
    Btn_Space: TSpeedButton;
    Btn_Alt: TSpeedButton;
    Btn_AltGR: TSpeedButton;
    Btn_Left: TSpeedButton;
    Btn_Ok: TSpeedButton;
    Btn_Rigth: TSpeedButton;
    Btn_Down: TSpeedButton;
    Btn_Up: TSpeedButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: LongInt);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: LongInt);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: LongInt);
    procedure FormPaint(Sender: TObject);
    procedure ModifierPress(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BtnPress(Sender: TObject);
  private
    //move operations
    OffsetX, OffsetY:LongInt;
    CurX,CurY:LongInt;
    MoveOperation:Boolean;

    FTarget:TWinControl;
    FFormOwner:TCustomForm;
    FKeyboard:TCrossKeyEvents;

    CurrentState:TShiftState;
    procedure BringToFrontWithoutActivate;
    procedure GotoBetterPosition;
    procedure ModifierRelease();
    procedure ReturnFocusToTarget;
  protected
    FFxxKeyGroup,
    FNumbersKeyGroup,
    FSymbolsKeyGroup,
    FNavigationKeyGroup,
    FFastNavigationKeyGroup:TList;
    FReturnCloseKeyBoard:Boolean;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor Create(TheOwner: TComponent;
                       Target:TWinControl;
                       ShowFxxKeys,
                       ShowTab,
                       ShowCaps,
                       ShowShift,
                       ShowCtrl,
                       ShowAlt,
                       ShowSymbols,
                       ShowNumbers,
                       ShowFastNavigation,
                       ShowNavigation,

                       CloseOnPressEnter:Boolean); overload;
    destructor Destroy; override;
    procedure ShowAlongsideOfTheTarget;
  published
    property Target:TWinControl read FTarget;
  end; 

var
  psHMIfrmAlphaKeyboard: TpsHMIfrmAlphaKeyboard;

implementation

uses strutils,
     {$IFDEF FPC}InterfaceBase, keyboard, LResources, LCLIntf, LCLType{$ENDIF}
     {$IF defined(WINDOWS) or defined(WIN32) or defined(WIN64) or defined(MSWINDOWS)}
     , windows
     {$IFEND};

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
    {$R ualfakeyboard.lfm}
  {$IFEND}
{$ENDIF}

{ TpsHMIfrmAlphaKeyboard }

var
  LastAlphaKeyboard:TpsHMIfrmAlphaKeyboard;

constructor TpsHMIfrmAlphaKeyboard.Create(TheOwner: TComponent;
  Target: TWinControl; ShowFxxKeys, ShowTab, ShowCaps, ShowShift, ShowCtrl,
  ShowAlt, ShowSymbols, ShowNumbers, ShowFastNavigation, ShowNavigation,
  CloseOnPressEnter: Boolean);
var
  curcontrol:TControl;
  k: TKeyEvent;

  procedure EnableGroup(Group:TList; EnableGroup:Boolean);
  var
    i: Integer;
  begin
    for i:=0 to Group.Count-1 do begin
      TSpeedButton(Group.Items[i]).Enabled:=TSpeedButton(Group.Items[i]).Enabled and EnableGroup;
    end;
  end;

begin
  inherited Create(TheOwner);

  if Assigned(LastAlphaKeyboard) then begin
    LastAlphaKeyboard.Close;
    LastAlphaKeyboard:=nil;
  end;

  FTarget:=Target;
  FKeyboard:=CreateCrossKeyEvents(Target);
  {$IFDEF LCL}
  FormStyle:=fsSystemStayOnTop;
  {$ENDIF}

  curcontrol:=FTarget;
  FFormOwner:=nil;
  ControlStyle:=ControlStyle+[csNoFocus];
  while (curcontrol<>nil) and (FFormOwner=nil) do begin
    if (curcontrol.Parent=nil) and (curcontrol is TCustomForm) then
      FFormOwner:=curcontrol as TCustomForm;

    curcontrol:=curcontrol.Parent;
  end;
  LastAlphaKeyboard:=Self;

  CurrentState:=[ssCtrl, ssAlt, ssShift];
  ModifierRelease();
  CurrentState:=[];

  FReturnCloseKeyBoard:=CloseOnPressEnter;

  FFxxKeyGroup:=TList.Create;
  FFxxKeyGroup.Add(Btn_F1);
  FFxxKeyGroup.Add(Btn_F2);
  FFxxKeyGroup.Add(Btn_F3);
  FFxxKeyGroup.Add(Btn_F4);
  FFxxKeyGroup.Add(Btn_F5);
  FFxxKeyGroup.Add(Btn_F6);
  FFxxKeyGroup.Add(Btn_F7);
  FFxxKeyGroup.Add(Btn_F8);
  FFxxKeyGroup.Add(Btn_F9);
  FFxxKeyGroup.Add(Btn_F10);
  FFxxKeyGroup.Add(Btn_F11);
  FFxxKeyGroup.Add(Btn_F12);

  FNumbersKeyGroup:=TList.Create;
  FNumbersKeyGroup.Add(Btn_0);
  FNumbersKeyGroup.Add(Btn_1);
  FNumbersKeyGroup.Add(Btn_2);
  FNumbersKeyGroup.Add(Btn_3);
  FNumbersKeyGroup.Add(Btn_4);
  FNumbersKeyGroup.Add(Btn_5);
  FNumbersKeyGroup.Add(Btn_6);
  FNumbersKeyGroup.Add(Btn_7);
  FNumbersKeyGroup.Add(Btn_8);
  FNumbersKeyGroup.Add(Btn_9);

  FSymbolsKeyGroup:=TList.Create;
  FSymbolsKeyGroup.Add(Btn_Quote);
  FSymbolsKeyGroup.Add(Btn_BackSlash);
  FSymbolsKeyGroup.Add(Btn_Hyphen);
  FSymbolsKeyGroup.Add(Btn_Equal);
  FSymbolsKeyGroup.Add(Btn_SingleQuote);
  FSymbolsKeyGroup.Add(Btn_BracketOpen);
  FSymbolsKeyGroup.Add(Btn_Tilde);
  FSymbolsKeyGroup.Add(Btn_BracketClose);
  FSymbolsKeyGroup.Add(Btn_Comma);
  FSymbolsKeyGroup.Add(Btn_Dot);
  FSymbolsKeyGroup.Add(Btn_Semicolon);
  FSymbolsKeyGroup.Add(Btn_Slash);

  FFastNavigationKeyGroup:=TList.Create;
  FFastNavigationKeyGroup.Add(Btn_PgUp);
  FFastNavigationKeyGroup.Add(Btn_PgDown);
  FFastNavigationKeyGroup.Add(Btn_End);
  FFastNavigationKeyGroup.Add(Btn_Home);

  FNavigationKeyGroup:=TList.Create;
  FNavigationKeyGroup.Add(Btn_Up);
  FNavigationKeyGroup.Add(Btn_Down);
  FNavigationKeyGroup.Add(Btn_Left);
  FNavigationKeyGroup.Add(Btn_Rigth);

  Btn_Caps.Enabled:=Btn_Caps.Enabled and ShowCaps;
  Btn_Tab.Enabled:=Btn_Tab.Enabled and ShowTab;
  Btn_Shift.Enabled:=Btn_Shift.Enabled and ShowShift and ShowSymbols;
  Btn_Ctrl.Enabled:=Btn_Ctrl.Enabled and ShowCtrl;
  Btn_Alt.Enabled:=Btn_Alt.Enabled and ShowAlt;

  EnableGroup(FNavigationKeyGroup, ShowNavigation);
  EnableGroup(FFastNavigationKeyGroup, ShowFastNavigation);
  EnableGroup(FFxxKeyGroup, ShowFxxKeys);
  EnableGroup(FSymbolsKeyGroup, ShowSymbols);
  EnableGroup(FNumbersKeyGroup, ShowNumbers);
end;

destructor TpsHMIfrmAlphaKeyboard.Destroy;
begin
  ModifierRelease;
  Fkeyboard.destroy;
  if LastAlphaKeyboard=Self then
    LastAlphaKeyboard:=nil;

  FreeAndNil(FFxxKeyGroup);
  FreeAndNil(FNumbersKeyGroup);
  FreeAndNil(FSymbolsKeyGroup);
  FreeAndNil(FNavigationKeyGroup);
  FreeAndNil(FFastNavigationKeyGroup);
  inherited Destroy;

end;

procedure TpsHMIfrmAlphaKeyboard.ShowAlongsideOfTheTarget;
begin
  GotoBetterPosition;
  Show;
  GetParentForm(FTarget).ShowOnTop;
end;

procedure TpsHMIfrmAlphaKeyboard.FormCreate(Sender: TObject);
var
  c:LongInt;
begin
  Timer1.Enabled:=false;
  MoveOperation:=False;
  for c:=0 to ControlCount-1 do
    if Controls[c] is TSpeedButton then begin
      TSpeedButton(Controls[c]).Caption:=StringReplace(TSpeedButton(Controls[c]).Caption,' ',''+LineEnding+'',[rfReplaceAll]);
    end;

  Btn_0.Tag:=VK_0;
  Btn_1.Tag:=VK_1;
  Btn_2.Tag:=VK_2;
  Btn_3.Tag:=VK_3;
  Btn_4.Tag:=VK_4;
  Btn_5.Tag:=VK_5;
  Btn_6.Tag:=VK_6;
  Btn_7.Tag:=VK_7;
  Btn_8.Tag:=VK_8;
  Btn_9.Tag:=VK_9;
  Btn_A.Tag:=VK_A;
  //Btn_Alt.Tag:=VK_MENU;
  //Btn_AltGR.Tag:=VK_RMENU;
  Btn_B.Tag:=VK_B;
  Btn_Back.Tag:=VK_BACK;
  Btn_BackSlash.Tag:=VK_OEM_102;
  Btn_BracketClose.Tag:=VK_OEM_5;
  Btn_BracketOpen.Tag:=VK_OEM_6;
  Btn_C.Tag:=VK_C;
  //Btn_Caps.Tag:=VK_CAPITAL;
  Btn_Cedilla.Tag:=VK_OEM_1;
  Btn_Comma.Tag:=VK_OEM_COMMA;
  //Btn_Ctrl.Tag:=VK_CONTROL;
  //Btn_CtrlR.Tag:=VK_RCONTROL;
  Btn_D.Tag:=VK_D;
  Btn_Del.Tag:=VK_DELETE;
  Btn_Dot.Tag:=VK_OEM_PERIOD;
  Btn_Down.Tag:=VK_DOWN;
  Btn_E.Tag:=VK_E;
  Btn_End.Tag:=VK_END;
  Btn_Equal.Tag:=VK_OEM_PLUS;
  Btn_Esc.Tag:=VK_ESCAPE;
  Btn_F.Tag:=VK_F;
  Btn_F1.Tag:=VK_F1;
  Btn_F10.Tag:=VK_F10;
  Btn_F11.Tag:=VK_F11;
  Btn_F12.Tag:=VK_F12;
  Btn_F2.Tag:=VK_F2;
  Btn_F3.Tag:=VK_F3;
  Btn_F4.Tag:=VK_F4;
  Btn_F5.Tag:=VK_F5;
  Btn_F6.Tag:=VK_F6;
  Btn_F7.Tag:=VK_F7;
  Btn_F8.Tag:=VK_F8;
  Btn_F9.Tag:=VK_F9;
  Btn_G.Tag:=VK_G;
  Btn_H.Tag:=VK_H;
  Btn_Home.Tag:=VK_HOME;
  Btn_Hyphen.Tag:=VK_OEM_MINUS;
  Btn_I.Tag:=VK_I;
  Btn_Ins.Tag:=VK_INSERT;
  Btn_J.Tag:=VK_J;
  Btn_K.Tag:=VK_K;
  Btn_L.Tag:=VK_L;
  Btn_Left.Tag:=VK_LEFT;
  Btn_M.Tag:=VK_M;
  Btn_N.Tag:=VK_N;
  Btn_O.Tag:=VK_O;
  Btn_Ok.Tag:=VK_RETURN;
  Btn_P.Tag:=VK_P;
  Btn_PgDown.Tag:=VK_NEXT;
  Btn_PgUp.Tag:=VK_PRIOR;
  Btn_Q.Tag:=VK_Q;
  Btn_Quote.Tag:=VK_OEM_3;
  Btn_R.Tag:=VK_R;
  Btn_Rigth.Tag:=VK_RIGHT;
  Btn_S.Tag:=VK_S;
  Btn_Semicolon.Tag:=VK_OEM_2;
  //Btn_Shift.Tag:=VK_SHIFT;
  Btn_SingleQuote.Tag:=VK_OEM_4;
  //Btn_Slash.Tag:=VK_UNKNOWN;
  Btn_Space.Tag:=VK_SPACE;
  Btn_T.Tag:=VK_T;
  Btn_Tab.Tag:=VK_TAB;
  Btn_Tilde.Tag:=VK_OEM_7;
  Btn_U.Tag:=VK_U;
  Btn_Up.Tag:=VK_UP;
  Btn_V.Tag:=VK_V;
  Btn_W.Tag:=VK_W;
  Btn_X.Tag:=VK_X;
  Btn_Y.Tag:=VK_Y;
  Btn_Z.Tag:=VK_Z;
end;

procedure TpsHMIfrmAlphaKeyboard.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: LongInt);
begin
  OffsetX:=X;
  OffsetY:=Y;
  Timer1.Enabled:=true;
end;

procedure TpsHMIfrmAlphaKeyboard.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: LongInt);
begin
  CurX:=X;
  CurY:=Y;
  MoveOperation:=True;
end;

procedure TpsHMIfrmAlphaKeyboard.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: LongInt);
begin
  Timer1.Enabled:=false;
  MoveOperation:=False;
end;

procedure TpsHMIfrmAlphaKeyboard.FormPaint(Sender: TObject);
begin
  Canvas.Font.Size:=8;
  Canvas.Font.Style:=[fsBold];

  canvas.TextOut(62,233,'Click here');
  canvas.TextOut(62,245,'to move');
  canvas.TextOut(62,257,'the keyboard');
end;

procedure TpsHMIfrmAlphaKeyboard.ModifierPress(Sender: TObject);
var
  x:TSpeedButton;
begin
  if not (Sender is TSpeedButton) then exit;

  x:=TSpeedButton.Create(Self);
  x.GroupIndex:=TSpeedButton(Sender).GroupIndex;
  x.Parent:=TSpeedButton(Sender).Parent;
  x.Left:=-1000;
  x.Top :=-1000;

  FKeyboard.Unapply(CurrentState);

  case TSpeedButton(Sender).Tag of
    1:
      if ssShift in CurrentState then begin
        CurrentState:=CurrentState-[ssShift];
        //TSpeedButton(Sender).Down:=false;
        x.Down:=true;
        TSpeedButton(Sender).Down:=false;
      end else begin
        CurrentState:=CurrentState+[ssShift];
        TSpeedButton(Sender).Down:=true;
      end;
    2:
      if ssAlt in CurrentState then begin
        CurrentState:=CurrentState-[ssAlt];
        //TSpeedButton(Sender).Down:=false;
        x.Down:=true;
        TSpeedButton(Sender).Down:=false;
      end else begin
        CurrentState:=CurrentState+[ssAlt];
        TSpeedButton(Sender).Down:=true;
      end;
    3:
      if ssCtrl in CurrentState then begin
        CurrentState:=CurrentState-[ssCtrl];
        //TSpeedButton(Sender).Down:=false;
        x.Down:=true;
        TSpeedButton(Sender).Down:=false;
      end else begin
        CurrentState:=CurrentState+[ssCtrl];
        TSpeedButton(Sender).Down:=true;
      end;
    {$IFDEF FPC}
    4:
      if ssAltGr in CurrentState then begin
        CurrentState:=CurrentState-[ssAltGr];
        //TSpeedButton(Sender).Down:=false;
        x.Down:=true;
        TSpeedButton(Sender).Down:=false;
      end else begin
        CurrentState:=CurrentState+[ssAltGr];
        TSpeedButton(Sender).Down:=true;
      end;
    {$ENDIF}
  end;
  Fkeyboard.Apply(CurrentState);
  Application.ProcessMessages;
  x.Destroy;
end;

procedure TpsHMIfrmAlphaKeyboard.ModifierRelease();
begin
  Fkeyboard.Unapply(CurrentState);
  Application.ProcessMessages;
end;

procedure TpsHMIfrmAlphaKeyboard.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  CloseAction:=caFree;
end;

procedure TpsHMIfrmAlphaKeyboard.Timer1Timer(Sender: TObject);
begin
  if not MoveOperation then exit;
  MoveOperation:=False;

  if CurX>OffsetX then
    Left:=Left+(CurX-OffsetX)
  else
    Left:=Left-(OffsetX-CurX);

  if CurY>OffsetY then
    Top:=Top+(CurY-OffsetY)
  else
    Top:=Top-(OffsetY-CurY);
end;

procedure TpsHMIfrmAlphaKeyboard.BtnPress(Sender: TObject);
begin
  if FTarget=nil then exit;

  ReturnFocusToTarget;

  with Sender as TSpeedButton do begin
    FKeyboard.Press(Tag);
    //Application.ProcessMessages;
    if (tag=VK_ESCAPE) or (FReturnCloseKeyBoard and (tag=VK_RETURN)) then
      Close;
  end;
  BringToFrontWithoutActivate;
end;

procedure TpsHMIfrmAlphaKeyboard.BringToFrontWithoutActivate;
begin
  WidgetSet.SetWindowPos(Self.Handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE+SWP_NOSIZE+SWP_NOACTIVATE);
end;

procedure TpsHMIfrmAlphaKeyboard.ReturnFocusToTarget;
begin
  FFormOwner.Show;
  FTarget.SetFocus;
  Application.ProcessMessages;
  BringToFrontWithoutActivate;
end;


procedure TpsHMIfrmAlphaKeyboard.GotoBetterPosition;
var
  sw, sh:Integer;
  frect, t_rect: TRect;
begin
  //auto posicionamento do popup.
  //t_point:=FTarget.ClientOrigin;
  WidgetSet.GetWindowRect(FTarget.Handle,t_rect);
  WidgetSet.GetWindowRect(Self.Handle,frect);
  sw:=Screen.Width;
  sh:=Screen.Height;

  if (t_rect.Top+(frect.Bottom-frect.Top)+FTarget.Height)<=sh then
    Top:=t_rect.Top+FTarget.Height   //borda superior do form com borda inferior do target
  else begin
    if (t_rect.Top - (frect.Bottom - frect.Top))>=0 then
      Top:=t_rect.Top - (frect.Bottom - frect.Top)  //borda inferior do form com borda superior do target
    else begin
      Top:= (t_rect.Top+((t_rect.Bottom-t_rect.Top) div 2) - ((frect.Bottom-frect.Top) div 2)); //meio
      if Top<0 then Top:=0;
      if (Top+(frect.Bottom - frect.Top))>Screen.Height then Top:=Screen.Height - (frect.Bottom - frect.Top);
    end;
  end;

  if ((t_rect.Left+FTarget.Width)-(frect.Right-frect.Left))>=0 then
    Left:=((t_rect.Left+FTarget.Width)-(frect.Right-frect.Left))  //borda direita do form com
                                                                //borda direita do target
  else begin
    if (t_rect.Left+(frect.Right-frect.Left))<=sw then
      Left:=t_rect.Left   //borda esquerda do form com borda esquerda do target
    else begin
      Left:= (t_rect.Left+((t_rect.Right-t_rect.Left) div 2) - ((frect.Right-frect.Left) div 2)); //meio
      if Left<0 then Left:=0;
      if (Left+(frect.Right - frect.Left))>Screen.Width then Left:=Screen.Width - (frect.Right - frect.Left);
    end;
  end;
end;

end.

