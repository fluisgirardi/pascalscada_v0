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
    fbmp:TBitmap;
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

procedure THMIBasicControl.Paint;
var
  p:PBGRAPixel;
  x, y:Integer;
  pb: PByte;
  bit: Integer;
begin
  if assigned(fbmp) then FreeAndNil(fbmp);

  fbmp:=TBitmap.Create;
  try
    fbmp.Width:=FControlArea.Width;
    fbmp.Height:=FControlArea.Height;
    fbmp.Monochrome :=true;
    fbmp.PixelFormat:=pf1bit;

    for y:=0 to FControlArea.Height-1 do begin
      p:=FControlArea.ScanLine[y];
      pb:=PByte(fbmp.ScanLine[y]);
      bit:=128;
      for x:=0 to FControlArea.Width-1 do begin
        {$IFDEF WINDOWS}
        if (p^.alpha>0) then begin
        {$ELSE}
        if (p^.alpha=0) then begin
        {$ENDIF}
          pb^:=pb^+bit;
        end;

        bit:=bit shr 1;

        if bit=0 then begin
          bit:=128;
          inc(pb);
        end;

        inc(p);
      end;
    end;
    SetShape(fbmp);
  finally
    //FreeAndNil(bmp);
  end;
end;

procedure THMIBasicControl.CMHitTest(var Message: TCMHittest);
begin
  if assigned(fbmp) and PtInRegion(fbmp.Handle, Message.Pos.X, Message.Pos.Y) then
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
