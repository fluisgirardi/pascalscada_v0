unit ualfakeyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Buttons, ExtCtrls,
  crosskeyevents;

type

  { TForm1 }

  TForm1 = class(TForm)
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
    Btn_ShiftR: TSpeedButton;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure ModifierPress(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BtnPress(Sender: TObject);
  private
    //move operations
    OffsetX, OffsetY:Integer;
    CurX,CurY:Integer;
    MoveOperation:Boolean;

    FTarget:TWinControl;
    FFormOwner:TForm;
    FKeyboard:TCrossKeyEvents;

    CurrentState:TShiftState;
    procedure ModifierRelease();
  public
    constructor Create(TheOwner: TComponent; Target:TWinControl; ShowMinus, ShowDecimal:Boolean); overload;
    destructor Destroy; override;
  end; 

var
  Form1: TForm1; 

implementation

uses strutils, keyboard
     {$IFDEF FPC}, LResources, LCLIntf, LCLType{$ENDIF}
     {$IFDEF WINDOWS}
     , windows
     {$ENDIF};

{$R *.lfm}

{ TForm1 }

constructor TForm1.Create(TheOwner: TComponent; Target:TWinControl; ShowMinus, ShowDecimal:Boolean);
var
  curcontrol:TControl;
begin
  inherited Create(TheOwner);
  FTarget:=Target;
  Fkeyboard:=CreateCrossKeyEvents(Target);

  curcontrol:=FTarget;
  FFormOwner:=nil;
  while (curcontrol<>nil) and (FFormOwner=nil) do begin
    if curcontrol is TForm then
      FFormOwner:=curcontrol as TForm
    else
      curcontrol:=curcontrol.Parent;
  end;
end;

destructor TForm1.Destroy;
begin
  inherited Destroy;
  Fkeyboard.destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  c:Integer;
begin
  Timer1.Enabled:=false;
  MoveOperation:=False;
  for c:=0 to ControlCount-1 do
    if Controls[c] is TSpeedButton then begin
      TSpeedButton(Controls[c]).Caption:=StringReplace(TSpeedButton(Controls[c]).Caption,' ',''+LineEnding+'',[rfReplaceAll]);
    end;

  Btn_Back.Tag:=PSVK_BACK;

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
  Btn_Alt.Tag:=VK_MENU;
  Btn_AltGR.Tag:=VK_RMENU;
  Btn_B.Tag:=VK_B;
  Btn_Back.Tag:=PSVK_BACK;
  Btn_BackSlash.Tag:=VK_OEM_5;
  Btn_BracketClose.Tag:=VK_OEM_6;
  Btn_BracketOpen.Tag:=VK_OEM_4;
  Btn_C.Tag:=VK_C;
  //Btn_Caps.Tag:=VK_;
  //Btn_Cedilla.Tag:=VK_;
  Btn_Comma.Tag:=VK_OEM_COMMA;
  Btn_Ctrl.Tag:=VK_CONTROL;
  Btn_CtrlR.Tag:=VK_RCONTROL;
  Btn_D.Tag:=VK_D;
  Btn_Del.Tag:=VK_DELETE;
  Btn_Dot.Tag:=VK_OEM_PERIOD;
  Btn_Down.Tag:=VK_DOWN;
  Btn_E.Tag:=VK_E;
  Btn_End.Tag:=VK_END;
  //Btn_Equal.Tag:=VK_;
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
  //Btn_Hyphen.Tag:=VK_MINUS;
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
  Btn_Quote.Tag:=VK_OEM_7;
  Btn_R.Tag:=VK_R;
  Btn_Rigth.Tag:=VK_RIGHT;
  Btn_S.Tag:=VK_S;
  //Btn_Semicolon.Tag:=VK_se;
  //Btn_Shift.Tag:=VK_SHIFT;
  //Btn_ShiftR.Tag:=VK_SHIFT;
  //Btn_SingleQuote.Tag:=VK_;
  //Btn_Slash.Tag:=VK_;
  Btn_Space.Tag:=VK_SPACE;
  Btn_T.Tag:=VK_T;
  Btn_Tab.Tag:=VK_TAB;
  //Btn_Tilde.Tag:=VK_TILDE;
  Btn_U.Tag:=VK_U;
  Btn_Up.Tag:=VK_UP;
  Btn_V.Tag:=VK_V;
  Btn_W.Tag:=VK_W;
  Btn_X.Tag:=VK_X;
  Btn_Y.Tag:=VK_Y;
  Btn_Z.Tag:=VK_Z;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  OffsetX:=X;
  OffsetY:=Y;
  Timer1.Enabled:=true;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  CurX:=X;
  CurY:=Y;
  MoveOperation:=True;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Timer1.Enabled:=false;
  MoveOperation:=False;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Font.Size:=8;
  Canvas.Font.Style:=[fsBold];

  canvas.TextOut(62,248,'Click here');
  canvas.TextOut(62,260,'to move');
  canvas.TextOut(62,272,'the keyboard');
end;

procedure TForm1.ModifierPress(Sender: TObject);
var
  x:TSpeedButton;
begin
  if not (Sender is TSpeedButton) then exit;

  x:=TSpeedButton.Create(Self);
  x.GroupIndex:=TSpeedButton(Sender).GroupIndex;

  case TSpeedButton(Sender).Tag of
    1:
      if ssShift in CurrentState then begin
        CurrentState:=CurrentState-[ssShift];
        TSpeedButton(Sender).Down:=false;
        x.Down:=true;
      end else begin
        CurrentState:=CurrentState+[ssShift];
        TSpeedButton(Sender).Down:=true;
      end;
    2:
      if ssAlt in CurrentState then begin
        CurrentState:=CurrentState-[ssAlt];
        TSpeedButton(Sender).Down:=false;
        x.Down:=true;
      end else begin
        CurrentState:=CurrentState+[ssAlt];
        TSpeedButton(Sender).Down:=true;
      end;
    3:
      if ssCtrl in CurrentState then begin
        CurrentState:=CurrentState-[ssCtrl];
        TSpeedButton(Sender).Down:=false;
        x.Down:=true;
      end else begin
        CurrentState:=CurrentState+[ssCtrl];
        TSpeedButton(Sender).Down:=true;
      end;
    4:
      if ssAltGr in CurrentState then begin
        CurrentState:=CurrentState-[ssAltGr];
        TSpeedButton(Sender).Down:=false;
        x.Down:=true;
      end else begin
        CurrentState:=CurrentState+[ssAltGr];
        TSpeedButton(Sender).Down:=true;
      end;
  end;
  Fkeyboard.Apply(CurrentState);
  x.Destroy;
end;

procedure TForm1.ModifierRelease();
begin
  Fkeyboard.Unapply(CurrentState);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
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

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TForm1.BtnPress(Sender: TObject);
begin
  if FTarget=nil then exit;
  with Sender as TSpeedButton do begin
    Fkeyboard.Press(Tag);
    //ModifierRelease();
    if (tag=VK_ESCAPE) or (tag=VK_RETURN) then
      close;
  end;
  FFormOwner.Show;
end;

end.

