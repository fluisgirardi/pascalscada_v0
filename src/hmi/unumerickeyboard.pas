{$i ../common/language.inc}
{$IFDEF PORTUGUES}
//: Teclado numérico.
{$ELSE}
//: Vitual numeric keyboard.
{$ENDIF}
unit unumerickeyboard;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Forms, Classes, types, Controls, Buttons, crosskeyevents
  {$IFDEF FPC}, LResources, LCLIntf, LCLType{$ENDIF};

type

  { TNumericKeyBoard }

  TNumericKeyBoard = class(TForm)
    Btn_1: TSpeedButton;
    Btn_2: TSpeedButton;
    Btn_3: TSpeedButton;
    Btn_4: TSpeedButton;
    Btn_5: TSpeedButton;
    Btn_6: TSpeedButton;
    Btn_7: TSpeedButton;
    Btn_8: TSpeedButton;
    Btn_9: TSpeedButton;
    Btn_0: TSpeedButton;
    Btn_Minus: TSpeedButton;
    Btn_DecSeparator: TSpeedButton;
    Btn_Del: TSpeedButton;
    Btn_Esc: TSpeedButton;
    Btn_Back: TSpeedButton;
    Btn_Ok: TSpeedButton;
    Btn_Left: TSpeedButton;
    Btn_Rigth: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnPress(Sender: TObject);
  private
    FShowMinus,
    FShowDecimal:Boolean;
    FTarget:TWinControl;
    FFormOwner:TCustomForm;
    keyboard:TCrossKeyEvents;
    procedure GotoBetterPosition;
    procedure ReturnFocusToTarget;
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure BringToFrontWithoutActivate;
  public
    constructor Create(TheOwner: TComponent; Target:TWinControl; ShowMinus, ShowDecimal:Boolean); overload;
    destructor Destroy; override;
    procedure ShowAlongsideOfTheTarget;
  published
    property Target:TWinControl read FTarget;
  end;

var
  NumericKeyBoard: TNumericKeyBoard;

implementation

uses InterfaceBase;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
    {$R unumerickeyboard.lfm}
  {$IFEND}
{$ENDIF}

var
  LastNumericKeyBoard: TNumericKeyBoard;

constructor TNumericKeyBoard.Create(TheOwner: TComponent; Target:TWinControl; ShowMinus, ShowDecimal:Boolean);
var
  curcontrol:TControl;
begin
  inherited Create(TheOwner);

  if Assigned(LastNumericKeyBoard) then begin
    LastNumericKeyBoard.Close;
    LastNumericKeyBoard:=nil;
  end;

  FShowDecimal:=ShowDecimal;
  FShowMinus:=ShowMinus;
  FTarget:=Target;
  keyboard:=CreateCrossKeyEvents(Target);
  {$IFDEF LCL}
  FormStyle:=fsSystemStayOnTop;
  {$ENDIF}

  curcontrol:=FTarget;
  FFormOwner:=nil;
  while (curcontrol<>nil) and (FFormOwner=nil) do begin
    if (curcontrol.Parent=nil) and (curcontrol is TCustomForm) then
      FFormOwner:=curcontrol as TCustomForm;

    curcontrol:=curcontrol.Parent;
  end;
  LastNumericKeyBoard:=Self;
end;

destructor TNumericKeyBoard.Destroy;
begin
  inherited Destroy;
  if LastNumericKeyBoard=Self then
    LastNumericKeyBoard:=nil;
  keyboard.Destroy;
end;

procedure TNumericKeyBoard.GotoBetterPosition;
var
  sw, sh:Integer;
  frect, t_rect: TRect;
begin
  //auto posicionamento do popup.
  //t_point:=FTarget.ClientOrigin;
  WidgetSet.GetWindowRect(Target.Handle,t_rect);
  WidgetSet.GetWindowRect(Self.Handle,frect);
  sw:=Screen.Width;
  sh:=Screen.Height;

  if (t_rect.Top+(frect.Bottom-frect.Top)+FTarget.Height)<=sh then
    Top:=t_rect.Top+FTarget.Height   //borda superior do form com borda inferior do target
  else begin
    if (t_rect.Top - (frect.Bottom - frect.Top))>=0 then
      Top:=t_rect.Top - (frect.Bottom - frect.Top)  //borda inferior do form com
                                                                    //borda superior do target
    else
      Top:=(t_rect.Top-(frect.Bottom-frect.Top)-FTarget.Height) div 2; //meio
  end;

  if ((t_rect.Left+FTarget.Width)-(frect.Right-frect.Left))>=0 then
    Left:=((t_rect.Left+FTarget.Width)-(frect.Right-frect.Left))  //borda direita do form com
                                                                //borda direita do target
  else begin
    if (t_rect.Left+(frect.Right-frect.Left))<=sw then
      Left:=t_rect.Left   //borda esquerda do form com borda esquerda do target
    else
      Left:=(t_rect.Left-(frect.Right-frect.Left)-FTarget.Width) div 2; //meio
  end;
end;

procedure TNumericKeyBoard.ReturnFocusToTarget;
begin
  FFormOwner.Show;
  FTarget.SetFocus;
  Application.ProcessMessages;
  BringToFrontWithoutActivate;
end;

procedure TNumericKeyBoard.ShowAlongsideOfTheTarget;
begin
  GotoBetterPosition;
  //BringToFrontWithoutActivate;
  ShowOnTop;
end;

procedure TNumericKeyBoard.FormCreate(Sender: TObject);
begin
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

  Btn_Left.tag:=VK_LEFT;
  Btn_Rigth.Tag:=VK_RIGHT;
  Btn_Esc.Tag:=VK_ESCAPE;
  Btn_Del.Tag:=VK_DELETE;
  Btn_Ok.Tag:=VK_RETURN;

  Btn_Back.Tag:=VK_BACK;
  Btn_DecSeparator.Tag:=VK_OEM_PERIOD; //Who is in delphi?
  Btn_Minus.Tag:=VK_SUBTRACT;

  Btn_Minus.Visible:=FShowMinus;
  Btn_DecSeparator.Visible:=FShowDecimal;
end;

procedure TNumericKeyBoard.BtnPress(Sender: TObject);
begin
  if FTarget=nil then exit;

  ReturnFocusToTarget;

  with Sender as TSpeedButton do begin
    keyboard.Press(Tag);
    Application.ProcessMessages;
    if (tag=VK_ESCAPE) or (tag=VK_RETURN) then
      close;
  end;
  BringToFrontWithoutActivate;
end;

procedure TNumericKeyBoard.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  if LastNumericKeyBoard=Self then
    LastNumericKeyBoard:=nil;
  CloseAction:=caFree;
end;

procedure TNumericKeyBoard.BringToFrontWithoutActivate;
begin
  WidgetSet.SetWindowPos(Self.Handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE+SWP_NOSIZE+SWP_NOACTIVATE);
end;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20400) }
    initialization
    {$i unumerickeyboard.lrs}
  {$IFEND}
{$ENDIF}

end.
