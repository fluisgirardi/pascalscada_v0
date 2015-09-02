unit hmi_draw_basiccontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Controls, Graphics, BGRABitmap, BGRABitmapTypes, LCLIntf,
  LMessages, LCLProc;

type

  {$DEFINE RGN_DETECT_RECTANGLES}

  { THMIBasicControl }

  THMIBasicControl = class(TCustomControl)
  private
    //{$IFDEF WINDOWS}
    UpdatingShape,
    UpdateShapeOnPaint:Boolean;
    //{$ENDIF}
  protected
    FControlArea:TBGRABitmap;
    FUpdatingCount:Cardinal;
    function ControlArea(pixel: TBGRAPixel): Boolean;
    function  CanRepaint:Boolean; virtual;
    procedure DrawControl; virtual;
    procedure UpdateShape; virtual;
    procedure Paint; override;
    procedure CMHitTest(var Message: TCMHittest) ; message CM_HITTEST;
  protected
    procedure SetBorderColor(AValue: TColor); virtual; abstract;
    procedure Loaded; override;
    procedure Loading; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  end;

implementation

uses math {$IFDEF DEBUG}, LCLProc{$ENDIF};

{ TBasicSCADAControl }

function THMIBasicControl.CanRepaint: Boolean;
begin
  Result:=FUpdatingCount=0;
end;

procedure THMIBasicControl.DrawControl;
begin

end;

function THMIBasicControl.ControlArea(pixel:TBGRAPixel):Boolean;
begin
  Result:=pixel.alpha>0;
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
  if (not UpdatingShape) then begin
    UpdateShapeOnPaint:=true;
    exit;
  end;

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
    //{$ifdef WINDOWS}
    UpdatingShape:=True;
    try
      if UpdateShapeOnPaint then begin
        UpdateShape;
        UpdateShapeOnPaint:=false;
      end;
    finally
      UpdatingShape:=false;
    end;
    //{$endif}
    FControlArea.Draw(Canvas, 0, 0, False);
  end;
  inherited Paint;
end;

procedure THMIBasicControl.CMHitTest(var Message: TCMHittest);
begin
  if Assigned(FControlArea) and ControlArea(FControlArea.ScanAt(Message.Pos.X,Message.Pos.Y)) then
    Message.Result:=1
  else
    Message.Result:=0;
end;

procedure THMIBasicControl.Loaded;
begin
  DebugLn('Loaded');
  inherited Loaded;
  DrawControl;
  UpdateShape;
  EndUpdate;
  InvalidateControl(true,false);
  DebugLn('END: Loaded');
end;

procedure THMIBasicControl.Loading;
begin
  inherited Loading;
  DebugLn('THMIBasicControl.Loading...');
end;

constructor THMIBasicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlArea:=TBGRABitmap.Create;
  if [csLoading, csReading]*ComponentState<>[] then
    BeginUpdate;

  DebugLn('THMIBasicControl.Create...');

end;

destructor THMIBasicControl.Destroy;
begin
  FreeAndNil(FControlArea);
  inherited Destroy;
end;

procedure THMIBasicControl.BeginUpdate;
begin
  if FUpdatingCount<Cardinal(-1) then
   inc(FUpdatingCount);
end;

procedure THMIBasicControl.EndUpdate;
begin
  if FUpdatingCount>0 then
    Dec(FUpdatingCount);

  if FUpdatingCount=0 then
    Invalidate;

end;

end.

