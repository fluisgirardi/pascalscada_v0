unit hmi_draw_basiccontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Controls, Graphics, BGRABitmap, BGRABitmapTypes, LCLIntf,
  LMessages, ControlSecurityManager, HMITypes, PLCTag;

type

  {$DEFINE RGN_DETECT_RECTANGLES}

  { THMIBasicControl }

  THMIBasicControl = class(TCustomControl, IHMIInterface)
  protected
    FSecurityCode:UTF8String;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    //: @seealso(IHMIInterface.SetHMITag)
    procedure SetHMITag(t:TPLCTag); virtual;                    //seta um tag
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
  end;

implementation

uses math;

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

  {$IFDEF RGN_CONTINUOUS_ROW_AS_RECTANGLE}
  started:boolean;
  x0, x1:Integer;
  {$ENDIF}

  {$IFDEF RGN_DETECT_RECTANGLES}
  xa, x1:Integer;
  y1:Integer;
  invalidline:boolean;
  {$ENDIF}

  {$IF (not defined(RGN_PIXEL_BY_PIXEL)) AND (not defined(RGN_CONTINUOUS_ROW_AS_RECTANGLE)) AND (not defined(RGN_DETECT_RECTANGLES))}
  pb:PByte;
  bit:PtrInt;
  fbmp: TBitmap;
  {$ELSE}
  frgn: TRegion;
  {$ENDIF}

begin
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

  {$IF (not defined(RGN_PIXEL_BY_PIXEL)) AND (not defined(RGN_CONTINUOUS_ROW_AS_RECTANGLE)) AND (not defined(RGN_DETECT_RECTANGLES))}
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
      UpdateShape;
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

procedure THMIBasicControl.CMHitTest(var Message: TCMHittest);
begin
  if Assigned(FControlArea) and ControlArea(FControlArea.ScanAt(Message.Pos.X,Message.Pos.Y)) then
    Message.Result:=1
  else
    Message.Result:=0;
end;

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

