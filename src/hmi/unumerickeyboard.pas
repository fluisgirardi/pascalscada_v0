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
  SysUtils, Forms, Classes, Controls, Buttons, crosskeyevents
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BtnPress(Sender: TObject);
  private
    FShowMinus,
    FShowDecimal:Boolean;
    FTarget:TWinControl;
    FFormOwner:TForm;
    keyboard:TCrossKeyEvents;
  public
    constructor Create(TheOwner: TComponent; Target:TWinControl; ShowMinus, ShowDecimal:Boolean); overload;
    destructor Destroy; override;
  published
    property Target:TWinControl read FTarget;
  end;

var
  NumericKeyBoard: TNumericKeyBoard;

implementation

{$IFDEF WINDOWS}
uses windows;
{$ENDIF}

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
    {$R unumerickeyboard.lfm}
  {$IFEND}
{$ENDIF}

constructor TNumericKeyBoard.Create(TheOwner: TComponent; Target:TWinControl; ShowMinus, ShowDecimal:Boolean);
var
  curcontrol:TControl;
begin
  inherited Create(TheOwner);
  FShowDecimal:=ShowDecimal;
  FShowMinus:=ShowMinus;
  FTarget:=Target;
  keyboard:=CreateCrossKeyEvents(Target);

  curcontrol:=FTarget;
  FFormOwner:=nil;
  while (curcontrol<>nil) and (FFormOwner=nil) do begin
    if curcontrol is TForm then
      FFormOwner:=curcontrol as TForm
    else
      curcontrol:=curcontrol.Parent;
  end;

end;

destructor TNumericKeyBoard.Destroy;
begin
  inherited Destroy;
  keyboard.Destroy;
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
  Btn_DecSeparator.Tag:=VK_DECIMAL;
  Btn_Minus.Tag:=VK_SUBTRACT;

  OnClose:=FormClose;

  Btn_Minus.Visible:=FShowMinus;
  Btn_DecSeparator.Visible:=FShowDecimal;
end;

procedure TNumericKeyBoard.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TNumericKeyBoard.BtnPress(Sender: TObject);
begin
  if FTarget=nil then exit;
  with Sender as TSpeedButton do begin
    keyboard.Press(Tag);
    if (tag=VK_ESCAPE) or (tag=VK_RETURN) then
      close;
  end;
  FFormOwner.Show;
end;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20400) }
    initialization
    {$i unumerickeyboard.lrs}
  {$IFEND}
{$ENDIF}

end.
