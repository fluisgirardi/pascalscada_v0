{$i ../common/language.inc}
{$IFDEF PORTUGUES}
//: Teclado numérico.
{$ELSE}
//: Vitual numeric keyboard.
{$ENDIF}
unit unumerickeyboard;

interface

uses
  SysUtils, Forms, Classes, types, Controls, Buttons, crosskeyevents
  {$IFDEF FPC}, LResources, LCLIntf, LCLType{$ENDIF};

type

  { TpsHMIfrmNumericKeyBoard }

  TpsHMIfrmNumericKeyBoard = class(TForm)
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
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor Create(TheOwner: TComponent; Target:TWinControl; ShowMinus, ShowDecimal:Boolean); overload;
    destructor Destroy; override;
    procedure ShowAlongsideOfTheTarget;
  published
    property Target:TWinControl read FTarget;
  end;

var
  psHMIfrmNumericKeyBoard: TpsHMIfrmNumericKeyBoard;

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
  LastNumericKeyBoard: TpsHMIfrmNumericKeyBoard;

constructor TpsHMIfrmNumericKeyBoard.Create(TheOwner: TComponent; Target:TWinControl; ShowMinus, ShowDecimal:Boolean);
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

  ControlStyle:=ControlStyle+[csNoFocus];
  FFormOwner:=nil;
  FFormOwner:=GetParentForm(Target);
  LastNumericKeyBoard:=Self;
end;

destructor TpsHMIfrmNumericKeyBoard.Destroy;
begin
  inherited Destroy;
  if LastNumericKeyBoard=Self then
    LastNumericKeyBoard:=nil;
  keyboard.Destroy;
end;

procedure TpsHMIfrmNumericKeyBoard.GotoBetterPosition;
var
  sw, sh:Integer;
  numkeyboard_rect, target_rect: TRect;
begin
  //auto posicionamento do popup.
  //t_point:=FTarget.ClientOrigin;
  WidgetSet.GetWindowRect(Target.Handle,target_rect);
  WidgetSet.GetWindowRect(Self.Handle,numkeyboard_rect);
  sw:=Screen.Width;
  sh:=Screen.Height;

  if (target_rect.Top+(numkeyboard_rect.Bottom-numkeyboard_rect.Top)+FTarget.Height)<=sh then
    Top:=target_rect.Top+FTarget.Height   //borda superior do form com borda inferior do target
  else begin
    if (target_rect.Top - (numkeyboard_rect.Bottom - numkeyboard_rect.Top))>=0 then
      Top:=target_rect.Top - (numkeyboard_rect.Bottom - numkeyboard_rect.Top)  //borda inferior do form com borda superior do target
    else begin
      Top:= (target_rect.Top+((target_rect.Bottom-target_rect.Top) div 2) - ((numkeyboard_rect.Bottom-numkeyboard_rect.Top) div 2)); //meio
      if Top<0 then Top:=0;
      if (Top+(numkeyboard_rect.Bottom - numkeyboard_rect.Top))>Screen.Height then Top:=Screen.Height - (numkeyboard_rect.Bottom - numkeyboard_rect.Top);
    end;
  end;

  if ((target_rect.Left+FTarget.Width)-(numkeyboard_rect.Right-numkeyboard_rect.Left))>=0 then
    Left:=((target_rect.Left+FTarget.Width)-(numkeyboard_rect.Right-numkeyboard_rect.Left))  //borda direita do form com
                                                                //borda direita do target
  else begin
    if (target_rect.Left+(numkeyboard_rect.Right-numkeyboard_rect.Left))<=sw then
      Left:=target_rect.Left   //borda esquerda do form com borda esquerda do target
    else begin
      Left:= (target_rect.Left+((target_rect.Right-target_rect.Left) div 2) - ((numkeyboard_rect.Right-numkeyboard_rect.Left) div 2)); //meio
      if Left<0 then Left:=0;
      if (Left+(numkeyboard_rect.Right - numkeyboard_rect.Left))>Screen.Width then Left:=Screen.Width - (numkeyboard_rect.Right - numkeyboard_rect.Left);
    end;
  end;
end;

procedure TpsHMIfrmNumericKeyBoard.ShowAlongsideOfTheTarget;
begin
  GotoBetterPosition;
  Show;
  GetParentForm(FTarget).ShowOnTop;
end;

procedure TpsHMIfrmNumericKeyBoard.FormCreate(Sender: TObject);
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

procedure TpsHMIfrmNumericKeyBoard.BtnPress(Sender: TObject);
begin
  if FTarget=nil then exit;

  with Sender as TSpeedButton do begin
    keyboard.Press(Tag);
    if (tag=VK_ESCAPE) or (tag=VK_RETURN) then
      close;
  end;
end;

procedure TpsHMIfrmNumericKeyBoard.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  if LastNumericKeyBoard=Self then
    LastNumericKeyBoard:=nil;
  CloseAction:=caFree;
end;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20400) }
    initialization
    {$i unumerickeyboard.lrs}
  {$IFEND}
{$ENDIF}

end.
