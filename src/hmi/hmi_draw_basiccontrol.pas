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
    procedure SetBorderColor(AValue: TColor); virtual;
    procedure SetColor(AValue: TColor); virtual;
    procedure Paint; override;
    procedure CMHitTest(var Message: TCMHittest) ; message CM_HITTEST;
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  end;

implementation

uses math;

{ TBasicSCADAControl }

function THMIBasicControl.CanRepaint: Boolean;
begin
  Result:=FUpdatingCount=0;
end;

procedure THMIBasicControl.SetBorderColor(AValue: TColor);
begin

end;

procedure THMIBasicControl.SetColor(AValue: TColor);
begin

end;

//{$DEFINE PIXEL_BY_PIXEL}

procedure THMIBasicControl.Paint;
var
  p:PBGRAPixel;
  x, y:Integer;
  {$IFNDEF PIXEL_BY_PIXEL}
  started:boolean;
  x0, x1:Integer;
  {$ENDIF}
begin
  if assigned(frgn) then FreeAndNil(frgn);

  frgn:=TRegion.Create;
  for y:=0 to FControlArea.Height-1 do begin
    p:=FControlArea.ScanLine[y];
    {$IFNDEF PIXEL_BY_PIXEL}
    started:=false;
    {$ENDIF}
    for x:=0 to FControlArea.Width-1 do begin
      {$IFDEF PIXEL_BY_PIXEL}
      if (p^.alpha>0) then
        frgn.AddRectangle(x,y,x+1,y+1);

      {$ELSE}
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
      {$ENDIF}
      inc(p);
    end;
    {$IFNDEF PIXEL_BY_PIXEL}
    //
    if started then begin
      if x0=x1 then
        frgn.AddRectangle(x0,y,x0+1,y+1)
      else
        frgn.AddRectangle(x0,y,x1,y+1);
    end;
    {$ENDIF}
  end;
  SetShape(frgn);

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

  if FUpdatingCount=0 then
    Invalidate;

end;

end.

