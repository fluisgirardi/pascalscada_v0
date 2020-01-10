unit hmi_draw_basiccontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Controls, Graphics, BGRABitmap, BGRABitmapTypes, LCLIntf,
  LMessages, ControlSecurityManager, HMITypes, PLCTag;

type

  TDragManagerAccess = Class(TDragManager);

  {$DEFINE ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE}

  { THMIBasicControl }

  THMIBasicControl = class(TCustomControl, IHMIInterface)

  protected
    FSecurityCode:UTF8String;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    //: @seealso(IHMIInterface.SetHMITag)
    procedure SetHMITag({%H-}t:TPLCTag); virtual;                    //seta um tag
    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag; virtual;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
     function GetControlSecurityCode:UTF8String;
    //: @seealso(IHMIInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean);
    //: @seealso(IHMIInterface.MakeUnsecure)
    procedure MakeUnsecure;

    //: @exclude
    procedure SetEnabled(e:Boolean); override;
    procedure SetSecurityCode(sc: UTF8String);

    procedure DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
      Shift: TShiftState); virtual;
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton); virtual;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;
  protected
    FBorderColor:TColor;
    FBodyColor: TColor;
    FUpdateShape,
    FShouldRedraw:Boolean;

    FBorderWidth:Integer;
    FControlArea:TBGRABitmap;
    FUpdatingCount:Cardinal;

    FOldWidth, FOldHeight:Integer;

    function  IsControlArea(X,Y:Integer):Boolean; virtual;
    procedure ForwardMouseMessage(var Message:TLMMouse); virtual;

    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TLMRButtonDown); message LM_RBUTTONDOWN;
    procedure WMMButtonDown(var Message: TLMMButtonDown); message LM_MBUTTONDOWN;
    procedure WMXButtonDown(var Message: TLMXButtonDown); message LM_XBUTTONDOWN;

    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TLMRButtonUp); message LM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TLMMButtonUp); message LM_MBUTTONUP;
    procedure WMXButtonUp(var Message: TLMXButtonUp); message LM_XBUTTONUP;

    procedure WMLButtonDBLCLK(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMRButtonDBLCLK(var Message: TLMRButtonDblClk); message LM_RBUTTONDBLCLK;
    procedure WMMButtonDBLCLK(var Message: TLMMButtonDblClk); message LM_MBUTTONDBLCLK;
    procedure WMXButtonDBLCLK(var Message: TLMXButtonDblClk); message LM_XBUTTONDBLCLK;
    procedure WMLButtonTripleCLK(var Message: TLMLButtonTripleClk); message LM_LBUTTONTRIPLECLK;
    procedure WMRButtonTripleCLK(var Message: TLMRButtonTripleClk); message LM_RBUTTONTRIPLECLK;
    procedure WMMButtonTripleCLK(var Message: TLMMButtonTripleClk); message LM_MBUTTONTRIPLECLK;
    procedure WMXButtonTripleCLK(var Message: TLMXButtonTripleClk); message LM_XBUTTONTRIPLECLK;
    procedure WMLButtonQuadCLK(var Message: TLMLButtonQuadClk); message LM_LBUTTONQUADCLK;
    procedure WMRButtonQuadCLK(var Message: TLMRButtonQuadClk); message LM_RBUTTONQUADCLK;
    procedure WMMButtonQuadCLK(var Message: TLMMButtonQuadClk); message LM_MBUTTONQUADCLK;
    procedure WMXButtonQuadCLK(var Message: TLMXButtonQuadClk); message LM_XBUTTONQUADCLK;

    procedure WMMouseMove(var Message: TLMMouseMove); message LM_MOUSEMOVE;

    function Cateto(p0, p1: Integer): Integer;
    function Degrees(x0, x1, y0, y1: Integer): Double;
    function Hipotenusa(x0, x1, y0, y1: Integer): Double;
    function Seno(x0, x1, y0, y1: Integer): Double;

    function  ControlArea(pixel: TBGRAPixel): Boolean; virtual;
    procedure InvalidateDraw; virtual;
    procedure InvalidateShape; virtual;
    procedure DrawControl; virtual;
    procedure UpdateShape; virtual;
    procedure Paint; override;
    procedure Resize; override;
    procedure CMHitTest(var Message: TCMHittest) ; message CM_HITTEST;
    procedure CMDesignHitTest(var Message: TCMHittest) ; message CM_DESIGNHITTEST;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetBodyColor(AValue: TColor); virtual;
    procedure SetBorderColor(AValue: TColor); virtual;
    procedure SetBorderWidth(AValue: Integer); virtual;
    procedure Loaded; override;
    property BorderColor:TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderWidth:Integer read FBorderWidth write SetBorderWidth default 1;
    property BodyColor:TColor read FBodyColor write SetBodyColor default clSilver;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
  end;

implementation

uses math, LCLType, Forms;

{ TBasicSCADAControl }

procedure THMIBasicControl.InvalidateDraw;
begin
  FShouldRedraw:=true;
  Invalidate;
end;

procedure THMIBasicControl.InvalidateShape;
var
  emptyArea: TBGRABitmap;
begin
  if [csDestroying]*ComponentState<>[] then exit;
  FUpdateShape:=true;
  emptyArea := TBGRABitmap.Create();
  try
    FControlArea.Assign(emptyArea);
  finally
    FreeAndNil(emptyArea);
  end;
  Invalidate;
end;

procedure THMIBasicControl.DrawControl;
var
  emptyArea: TBGRABitmap;
begin
  emptyArea := TBGRABitmap.Create(Width,Height);
  try
    FControlArea.Assign(emptyArea);
  finally
    FreeAndNil(emptyArea);
  end;
end;

procedure THMIBasicControl.SetBorderWidth(AValue: Integer);
begin
  if FBorderWidth=AValue then Exit;
  if AValue<0 then exit;
  FBorderWidth:=AValue;
  if ComponentState*[csReading, csLoading]=[] then
    InvalidateShape;
end;

procedure THMIBasicControl.SetBodyColor(AValue: TColor);
begin
  if FBodyColor=AValue then Exit;
  FBodyColor:=AValue;

  if ComponentState*[csReading, csLoading]=[] then
    InvalidateDraw;
end;

procedure THMIBasicControl.SetSecurityCode(sc: UTF8String);
begin
  if Trim(sc)='' then
    Self.CanBeAccessed(true)
  else
    with GetControlSecurityManager do begin
      ValidateSecurityCode(sc);
      if not SecurityCodeExists(sc) then
        RegisterSecurityCode(sc);

      Self.CanBeAccessed(CanAccess(sc));
    end;

  FSecurityCode:=sc;
end;

function THMIBasicControl.IsControlArea(X, Y:Integer): Boolean;
begin
  Result:=Assigned(FControlArea) and
          (x>=0) and
          (x<FControlArea.Width) and
          (y>=0) and
          (y<FControlArea.Height) and
          ControlArea(FControlArea.ScanAt(X, Y));
end;

procedure THMIBasicControl.ForwardMouseMessage(var Message: TLMMouse);
var
  oldEnableState: Boolean;
  ClientPoint, GlobalPoint:TPoint;
  aControl: TControl;
begin
  oldEnableState:=Enabled;
  Enabled:=false;
  try
    ClientPoint.x:=Message.XPos;
    ClientPoint.y:=Message.YPos;
    GlobalPoint:=ClientToScreen(ClientPoint);
    ClientPoint:=ClientToParent(ClientPoint);
    if (Name='LinhaCondensado') then
      aControl:=nil;

    aControl:= Parent.ControlAtPos(ClientPoint, false, true);
    repeat
      if Assigned(aControl) and (aControl is TWinControl) and (TWinControl(aControl).ControlCount>0) then begin
        aControl:=TWinControl(aControl).ControlAtPos(aControl.ScreenToClient(GlobalPoint),false,true);
      end;
    until (Assigned(aControl)=false) or (not (aControl is TWinControl)) or (TWinControl(aControl).ControlCount=0);

    if Assigned(aControl) then begin
      ClientPoint:= aControl.ScreenToClient(GlobalPoint);
      Message.XPos:=ClientPoint.x;
      Message.YPos:=ClientPoint.y;
      aControl.Dispatch(Message)
    end else
      if Assigned(Parent) then begin
        ClientPoint:= Parent.ScreenToClient(GlobalPoint);
        Message.XPos:=ClientPoint.x;
        Message.YPos:=ClientPoint.y;
        Parent.Dispatch(Message);
      end;
  finally
    Enabled:=oldEnableState;
  end;
end;

procedure THMIBasicControl.DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
  Shift: TShiftState);
var
  MP: TPoint;
begin
  if not (csNoStdEvents in ControlStyle) then
  begin
    MP := GetMousePosFromMessage(Message.Pos);
    MouseDown(Button, KeysToShiftState(Message.Keys) + Shift, MP.X, MP.Y);
  end;
end;

procedure THMIBasicControl.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
var
  P, MP: TPoint;
begin
  if not (csNoStdEvents in ControlStyle) then
  begin
    MP := GetMousePosFromMessage(Message.Pos);
    if (Button in [mbLeft, mbRight]) and DragManager.IsDragging then
    begin
      P := ClientToScreen(MP);
      TDragManagerAccess(DragManager).MouseUp(Button, KeysToShiftState(Message.Keys), P.X, P.Y);
      Message.Result := 1;
    end;
    MouseUp(Button, KeysToShiftState(Message.Keys), MP.X, MP.Y);
  end;
end;

procedure THMIBasicControl.WMLButtonDown(var Message: TLMLButtonDown);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := True;

    if csClickEvents in ControlStyle then
      Include(FControlState, csClicked);
    DoMouseDown(Message, mbLeft, []);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonDown(var Message: TLMRButtonDown);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := True;
    DoMouseDown(Message, mbRight, []);
  end else
    ForwardMouseMessage(Message);
end;


procedure THMIBasicControl.WMMButtonDown(var Message: TLMMButtonDown);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin

    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
        MouseCapture := True;

    DoMouseDown(Message, mbMiddle, []);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonDown(var Message: TLMXButtonDown);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := True;


    DoMouseDown(Message, Btn, []);
  end else
    ForwardMouseMessage(Message);
end;


procedure THMIBasicControl.WMLButtonUp(var Message: TLMLButtonUp);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin

    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := False;

    if csClicked in ControlState then  begin
      Exclude(FControlState, csClicked);
      Click;
    end;

    DoMouseUp(Message, mbLeft);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonUp(var Message: TLMRButtonUp);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := False;

    //MouseUp event is independent of return values of contextmenu
    DoMouseUp(Message, mbRight);
  end else
    ForwardMouseMessage(Message);
end;

{------------------------------------------------------------------------------
  Method: THMIBasicControl.WMMButtonUp
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure THMIBasicControl.WMMButtonUp(var Message: TLMMButtonUp);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
      MouseCapture := False;

    DoMouseUp(Message, mbMiddle);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonUp(var Message: TLMXButtonUp);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := False;

    DoMouseUp(Message, Btn);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMLButtonDBLCLK(var Message: TLMLButtonDblClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    //TODO: SendCancelMode(self);
    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := True;

    // first send a mouse down
    DoMouseDown(Message, mbLeft ,[ssDouble]);
    // then send the double click
    if csClickEvents in ControlStyle then
      DblClick;
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonDBLCLK(var Message: TLMRButtonDblClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := True;
    DoMouseDown(Message, mbRight ,[ssDouble]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMMButtonDBLCLK(var Message: TLMMButtonDblClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbMiddle ,[ssDouble]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonDBLCLK(var Message: TLMXButtonDblClk);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, Btn, [ssDouble]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMLButtonTripleCLK(var Message: TLMLButtonTripleClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := True;

    if csClickEvents in ControlStyle then TripleClick;
    DoMouseDown(Message, mbLeft ,[ssTriple]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonTripleCLK(var Message: TLMRButtonTripleClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbRight ,[ssTriple]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMMButtonTripleCLK(var Message: TLMMButtonTripleClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbMiddle ,[ssTriple]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonTripleCLK(var Message: TLMXButtonTripleClk);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, Btn, [ssTriple]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMLButtonQuadCLK(var Message: TLMLButtonQuadClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := True;

    if csClickEvents in ControlStyle then QuadClick;
    DoMouseDown(Message, mbLeft ,[ssQuad]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonQuadCLK(var Message: TLMRButtonQuadClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbRight ,[ssQuad]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMMButtonQuadCLK(var Message: TLMMButtonQuadClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbMiddle ,[ssQuad]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonQuadCLK(var Message: TLMXButtonQuadClk);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, Btn, [ssQuad]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMMouseMove(var Message: TLMMouseMove);
var
  MP: TPoint;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    MP := GetMousePosFromMessage(Message.Pos);
    UpdateMouseCursor(MP.X,MP.Y);
    if not (csNoStdEvents in ControlStyle) then
      MouseMove(KeystoShiftState(Word(Message.Keys)), MP.X, MP.Y);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.SetHMITag(t: TPLCTag);
begin

end;

function THMIBasicControl.GetHMITag: TPLCTag;
begin
  Result:=nil;
end;

function THMIBasicControl.GetControlSecurityCode: UTF8String;
begin
  Result:=FSecurityCode;
end;

procedure THMIBasicControl.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity :=a;
  SetEnabled(FIsEnabled);
end;

procedure THMIBasicControl.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMIBasicControl.SetEnabled(e: Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

function THMIBasicControl.Cateto(p0,p1:Integer):Integer;
begin
  if p1>=p0 then
    Result:=p1-p0
  else
    Result:=p0-p1;
end;

function THMIBasicControl.Hipotenusa(x0, x1, y0, y1:Integer):Double;
begin
  Result:=sqrt(Power(Cateto(x0,x1),2)+
               Power(Cateto(y0,y1),2));
end;

function THMIBasicControl.Seno(x0,x1,y0,y1:Integer):Double;
var
  hip: Double;
begin
  hip:=Hipotenusa(x0,x1,y0,y1);
  if hip=0 then
    Result:=0
  else
    Result:=Cateto(y0,y1)/hip;
end;

function THMIBasicControl.Degrees(x0,x1,y0,y1:Integer):Double;
var
  CatetoAdj: Integer;
  CatetoOposto: Integer;
  SomaCatetos: Integer;
begin
  CatetoAdj:=Cateto(x0,x1);
  CatetoOposto:=Cateto(y0,y1);
  SomaCatetos:=CatetoAdj+CatetoOposto;
  if SomaCatetos=0 then
    Result:=0
  else
    Result:=CatetoOposto/SomaCatetos*90;
end;

function THMIBasicControl.ControlArea(pixel:TBGRAPixel):Boolean;
begin
  Result:=pixel.alpha>127;
end;

procedure THMIBasicControl.UpdateShape;
var
  p:PBGRAPixel;
  x, y:Integer;
  arect:TRect;

  {$IF defined(RGN_CONTINUOUS_ROW_AS_RECTANGLE) OR defined(ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE)}
  started:boolean;
  x0, x1:Integer;
  {$IFDEF ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE}
  fbmp: TBitmap;
  pb:PByte;

  procedure Draw1BitBMPLine(abmp:TBitmap; FromX, ToX, Y:Integer);
  var
    sl: PByte;
    aBit: Word;
    aX: Integer;
  begin
    sl:=abmp.ScanLine[y];
    inc(sl,FromX div 8);
    {$IFNDEF LCLGtk2}
    aBit:=128;
    aBit:=aBit shr (FromX mod 8);
    {$ELSE}
    abit:=1;
    aBit:=aBit shl (FromX mod 8);
    {$ENDIF}

    for aX:=FromX to ToX-1 do begin
      sl^:=sl^+aBit;

      {$IFNDEF LCLGtk2}
      aBit:=aBit shr 1;
      {$ELSE}
      aBit:=aBit shl 1;
      {$ENDIF}

      {$IFNDEF LCLGtk2}
      if aBit=0 then begin
        aBit:=128;
        inc(sl);
      end;
      {$ELSE}
      if aBit=256 then begin
        aBit:=1;
        inc(sl);
      end;
      {$ENDIF}
    end;

  end;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF RGN_DETECT_RECTANGLES}
  xa, x1:Integer;
  y1:Integer;
  invalidline:boolean;
  {$ENDIF}

  {$IF (not defined(RGN_PIXEL_BY_PIXEL)) AND (not defined(RGN_CONTINUOUS_ROW_AS_RECTANGLE)) AND (not defined(RGN_DETECT_RECTANGLES)) AND (not defined(ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE))}
  pb:PByte;
  bit:PtrInt;
  fbmp: TBitmap;
  {$ELSE}
  {$IFNDEF ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE}
  frgn: TRegion;
  {$ENDIF}
  {$ENDIF}
begin
  {$IF defined(LCLqt) or defined(LCLQt5)}
  Color:=clBackground;
  exit;
  {$IFEND}
  if Parent=nil then exit;

  {$IFDEF RGN_PIXEL_BY_PIXEL}
  frgn:=TRegion.Create;
  for y:=0 to FControlArea.Height-1 do begin
    p:=FControlArea.ScanLine[y];
    for x:=0 to FControlArea.Width-1 do begin
      if (p^.alpha>0) then
        frgn.AddRectangle(x,y,x+1,y+1);

      inc(p);
    end;
  end;
  SetShape(frgn);
  FreeAndNil(frgn);
  {$ENDIF}

  {$IFDEF RGN_CONTINUOUS_ROW_AS_RECTANGLE}
  frgn:=TRegion.Create;
  for y:=0 to FControlArea.Height-1 do begin
    p:=FControlArea.ScanLine[y];
    started:=false;
    for x:=0 to FControlArea.Width-1 do begin
      if (p^.alpha=0) then begin
        if started then begin
          if x0=x1 then
            frgn.AddRectangle(x0,y,x0+1,y+1)
          else
            frgn.AddRectangle(x0,y,x1,y+1);
        end;
        started:=false;
      end else begin
        if started then begin
          x1:=x+1;
        end else begin
          x0:=x;
          x1:=x+1;
          started:=true;
        end;
      end;
      inc(p);
    end;

    //the
    if started then begin
      if x0=x1 then
        frgn.AddRectangle(x0,y,x0+1,y+1)
      else
        frgn.AddRectangle(x0,y,x1,y+1);
    end;
  end;
  SetShape(frgn);
  FreeAndNil(frgn);
  {$ENDIF}

  {$IFDEF RGN_DETECT_RECTANGLES}
  frgn:=TRegion.Create;
  try
    for y:=0 to FControlArea.Height-1 do begin
      for x:=0 to FControlArea.Width-1 do begin
        if ControlArea(FControlArea.ScanLine[y][x]) and (PtInRegion(frgn.Handle, x, y)=false) then begin
          for x1:=x to FControlArea.Width-2 do begin
            if ControlArea(FControlArea.ScanLine[y][x1+1])=false then break;
            if PtInRegion(frgn.Handle, x1+1, y) then break;
          end;

          invalidline:=false;
          for y1:=y to FControlArea.Height-2 do begin
            for xa:=x to x1 do begin
              if ControlArea(FControlArea.ScanLine[y1+1][xa])=false then begin
                invalidline:=true;
                break;
              end;
              if PtInRegion(frgn.Handle, xa, y1+1) then begin
                invalidline:=true;
                break;
              end;
            end;
            if invalidline then break;
          end;
          frgn.AddRectangle(x,y,x1+1,y1+1);
        end;
      end;
    end;
    SetShape(frgn);
  finally
    FreeAndNil(frgn);
  end;
  {$ENDIF}


  {$IFDEF ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE}
  fbmp:=TBitmap.Create;
  fbmp.Monochrome :=true;
  fbmp.PixelFormat:=pf1bit;
  fbmp.Width:=FControlArea.Width;
  fbmp.Height:=FControlArea.Height;
  fbmp.Canvas.Pen.Color:=clBlackOpaque;

  for y:=0 to FControlArea.Height-1 do begin
    p:=FControlArea.ScanLine[y];
    started:=false;
    for x:=0 to FControlArea.Width-1 do begin
      if ControlArea(p^) then begin
        if started then begin
          x1:=x+1;
        end else begin
          x0:=x;
          x1:=x+1;
          started:=true;
        end;
      end else begin
        if started then begin
          if x0=x1 then
            Draw1BitBMPLine(fbmp, x0, x0+1,y)
          else
            Draw1BitBMPLine(fbmp, x0, x1,  y);
        end;
        started:=false;
      end;
      inc(p);
    end;

    //the
    if started then begin
      if x0=x1 then
        Draw1BitBMPLine(fbmp, x0, x0+1,y)
      else
        Draw1BitBMPLine(fbmp, x0, x1,  y);
    end;
  end;
  SetShape(fbmp);
  FreeAndNil(fbmp);
  {$ENDIF}

  {$IF (not defined(RGN_PIXEL_BY_PIXEL)) AND (not defined(RGN_CONTINUOUS_ROW_AS_RECTANGLE)) AND (not defined(RGN_DETECT_RECTANGLES)) AND (not defined(ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE))}
  //////////////////////////////////////////////////////////////////////////////
  fbmp:=TBitmap.Create;
  fbmp.Monochrome :=true;
  fbmp.PixelFormat:=pf1bit;
  fbmp.Width:=FControlArea.Width;
  fbmp.Height:=FControlArea.Height;


  //build shape
  for y:=0 to FControlArea.Height-1 do begin
    pb:=PByte(fbmp.ScanLine[y]);
    {$IFNDEF LCLGtk2}
    bit:=128;
    {$ELSE}
    bit:=1;
    {$ENDIF}
    for x:=0 to FControlArea.Width-1 do begin
      if ControlArea(FControlArea.ScanLine[y][x]) then
        pb^:=pb^+bit;

      {$IFNDEF LCLGtk2}
      bit:=bit shr 1;
      {$ELSE}
      bit:=bit shl 1;
      {$ENDIF}

      {$IFNDEF LCLGtk2}
      if bit=0 then begin
        bit:=128;
        inc(pb);
      end;
      {$ELSE}
      if bit=256 then begin
        bit:=1;
        inc(pb);
      end;
      {$ENDIF}
    end;
  end;
  SetShape(fbmp);
  FreeAndNil(fbmp);
  {$ENDIF}
end;

procedure THMIBasicControl.Paint;
begin
  if assigned(FControlArea) then begin
    if FControlArea.Empty Or FUpdateShape then begin
      DrawControl;
      {$IF defined(LCLqt) or defined(LCLQt5)}
      Color:=clBackground;
      {$ELSE}
      UpdateShape;
      {$IFEND}
      FShouldRedraw:=false;
      FUpdateShape:=false;
    end;

    if FShouldRedraw then begin
      DrawControl;
      FShouldRedraw:=false;
    end;

    if Visible then
      FControlArea.Draw(Canvas, 0, 0, False);
  end;
  inherited Paint;
end;

procedure THMIBasicControl.Resize;
begin
  try
    inherited Resize;
  finally
    if (Width<>FOldWidth) or (FOldHeight<>Height) then
      InvalidateShape;
    FOldWidth:=Width;
    FOldHeight:=Height;
  end;
end;

procedure THMIBasicControl.Invalidate;
begin
  //GTK2 bug: if control is empty and setshape was applied,
  //Invalidate will not call Paint, because all control area
  //is "transparent", so Paint don't have anything to do...
  {$IF defined(LCLGtk2)} //maybe more need this trick...
  if ([csDestroying]*ComponentState=[]) and Assigned(FControlArea) and FControlArea.Empty then begin
    DrawControl;
    UpdateShape;
  end;
  {$IFEND}
  inherited Invalidate;
end;

procedure THMIBasicControl.CMHitTest(var Message: TCMHittest);
begin
  if IsControlArea(Message.XPos, Message.YPos) then
    Message.Result:=1
  else
    Message.Result:=0;
end;

procedure THMIBasicControl.CMDesignHitTest(var Message: TCMHittest);
begin
  if IsControlArea(Message.XPos, Message.YPos) then
    Message.Result:=1
  else
    Message.Result:=0;
end;

//procedure THMIBasicControl.TransparentHitTest(var Message: TCMHitTest);
//begin
//  //if (not Assigned(FControlArea)) or (not ControlArea(FControlArea.ScanAt(Message.Pos.X,Message.Pos.Y))) then
//  //  Message.Result:=HTTRANSPARENT
//  //else
//    Message.Result:=0;
//end;

procedure THMIBasicControl.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if (NewParent<>nil) and (ComponentState*[csReading, csLoading]=[]) then begin
    DrawControl;
    UpdateShape;
    Invalidate;
  end;
end;

procedure THMIBasicControl.SetBorderColor(AValue: TColor);
begin
  if AValue=FBorderColor then exit;
  FBorderColor:=AValue;
  if ComponentState*[csReading, csLoading]=[] then
    InvalidateDraw;
end;

procedure THMIBasicControl.Loaded;
begin
  inherited Loaded;
  InvalidateShape;
end;

constructor THMIBasicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color:=clBackground;
  FShouldRedraw:=false;
  FUpdateShape:=false;
  FBodyColor:=clSilver;
  FBorderColor:=clBlack;
  FBorderWidth:=1;
  FControlArea:=TBGRABitmap.Create;
  FIsEnabled:=true;
  FOldHeight:=0;
  FOldWidth:=0;
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIBasicControl.Destroy;
begin
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  FreeAndNil(FControlArea);
  inherited Destroy;
end;

end.

