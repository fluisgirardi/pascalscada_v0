unit hmi_draw_basiccontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Controls, Graphics, BGRABitmap, BGRABitmapTypes, BMPcomn;

type

  { TBasicSCADAControl }

  THMIBasicControl = class(TCustomControl)
  protected
    FControlArea:TBGRABitmap;
    FUpdatingCount:Cardinal;
    function CanRepaint:Boolean; virtual;
    procedure SetBorderColor(AValue: TColor); virtual;
    procedure SetColor(AValue: TColor); virtual;
    procedure Paint; override;
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
  bmp:TBitmap;
  p:PBGRAPixel;
  x, y:Integer;
  pb: PByte;
  bit: Integer;
begin
  bmp:=TBitmap.Create;
  try
    bmp.Width:=FControlArea.Width;
    bmp.Height:=FControlArea.Height;
    bmp.Monochrome :=true;
    bmp.PixelFormat:=pf1bit;

    for y:=0 to FControlArea.Height-1 do begin
      p:=FControlArea.ScanLine[y];
      pb:=PByte(bmp.ScanLine[y]);
      bit:=7;
      for x:=0 to FControlArea.Width-1 do begin
        {$IFDEF WINDOWS}
        if (p^.alpha>0) then begin
        {$ELSE}
        if (p^.alpha=0) then begin
        {$ENDIF}
          pb^:=pb^+trunc(power(2,bit));
        end;

        dec(bit);

        if bit<0 then begin
          bit:=7;
          inc(pb);
        end;

        inc(p);
      end;
    end;
    SetShape(bmp);
  finally
    FreeAndNil(bmp);
  end;
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

