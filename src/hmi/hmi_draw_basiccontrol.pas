unit hmi_draw_basiccontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Controls, Graphics, BGRABitmap, BGRABitmapTypes, LCLIntf,
  LMessages;

type

  { TBasicSCADAControl }

  { THMIBasicControl }

  THMIBasicControl = class(TCustomControl)
  private
    frgn:TRegion;
  protected
    FControlArea:TBGRABitmap;
    FUpdatingCount:Cardinal;
    function CanRepaint:Boolean; virtual;
    procedure DrawControl; virtual;
    procedure UpdateShape; virtual;
    procedure SetBorderColor(AValue: TColor); virtual;
    procedure Paint; override;
    procedure CMHitTest(var Message: TCMHittest) ; message CM_HITTEST;
  public
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

{$DEFINE DETECT_RECTANGLES}
procedure THMIBasicControl.UpdateShape;
var
  p:PBGRAPixel;
  x, y:Integer;

  {$IFDEF CONTINUOUS_ROW_AS_RECTANGLE}
  started:boolean;
  x0, x1:Integer;
  {$ENDIF}

  {$IFDEF DETECT_RECTANGLES}
  xa, x1:Integer;
  y1:Integer;
  invalidline:boolean;
  {$ENDIF}

  function ControlArea(pixel:TBGRAPixel):Boolean;
  begin
    Result:=pixel.alpha>0;
  end;

begin
  if assigned(frgn) then FreeAndNil(frgn);

  frgn:=TRegion.Create;
  {$IFDEF PIXEL_BY_PIXEL}
  for y:=0 to FControlArea.Height-1 do begin
    p:=FControlArea.ScanLine[y];
    for x:=0 to FControlArea.Width-1 do begin
      if (p^.alpha>0) then
        frgn.AddRectangle(x,y,x+1,y+1);

      inc(p);
    end;
  end;
  {$ENDIF}

  {$IFDEF CONTINUOUS_ROW_AS_RECTANGLE}
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
  {$ENDIF}

  {$IFDEF DETECT_RECTANGLES}
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
  {$ENDIF}

  if ParentHandlesAllocated then
    SetShape(frgn);

end;

procedure THMIBasicControl.SetBorderColor(AValue: TColor);
begin

end;

procedure THMIBasicControl.Paint;
begin
  if assigned(FControlArea) then begin
    FControlArea.Draw(Canvas, 0, 0, False);
    {$IFDEF WINDOWS}
    //UpdateShape;
    {$ENDIF}
  end;
end;

procedure THMIBasicControl.CMHitTest(var Message: TCMHittest);
begin
  if assigned(frgn) and PtInRegion(frgn.Handle, Message.Pos.X, Message.Pos.Y) then
    Message.Result:=1
  else
    Message.Result:=0;
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
end;

end.

