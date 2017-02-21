unit hmi_draw_basic_vector_control;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hmi_draw_basiccontrol, BGRASVG, BGRASVGShapes, BGRASVGType,
  TextStrings, BGRAUnits, BGRABitmapTypes;

type

  { THMICustomVectorControl }

  THMICustomVectorControl = class(THMIBasicControl)
  private
    FProportional: Boolean;
    FStretch: Boolean;

    FSVGContents: TStrings;
    procedure SetProportional(AValue: Boolean);
    procedure SetStretch(AValue: Boolean);
    procedure SetSVGContents(AValue: TStrings);
  protected
    FSVGDrawing: TBGRASVG;
    procedure DrawControl; override;
    procedure ReloadDrawing; virtual;
    property  SVGContents:TStrings read FSVGContents write SetSVGContents;
    property  Stretch:Boolean read FStretch write SetStretch;
    property  Proportional:Boolean read FProportional write SetProportional;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  THMIBasicVectorControl = class(THMICustomVectorControl)
  published
    property SVGContents;
    property Stretch;
  end;

implementation

{ THMIBasicVectorControl }

procedure THMICustomVectorControl.SetSVGContents(AValue: TStrings);
begin
  if Assigned(AValue) then begin
    FSVGContents.Assign(AValue);
    ReloadDrawing;
    InvalidateShape;
  end;
end;

procedure THMICustomVectorControl.SetStretch(AValue: Boolean);
begin
  if FStretch=AValue then Exit;
  FStretch:=AValue;
  InvalidateShape;
end;

procedure THMICustomVectorControl.SetProportional(AValue: Boolean);
begin
  if FProportional=AValue then Exit;
  FProportional:=AValue;
  InvalidateShape;
end;

procedure THMICustomVectorControl.DrawControl;
begin
  inherited DrawControl;
  if FStretch then begin
    if FProportional then
      FSVGDrawing.StretchDraw(FControlArea.Canvas2D,
                              taLeftJustify,
                              tlTop,
                              0,
                              0,
                              FControlArea.Canvas2D.Width,
                              FControlArea.Canvas2D.Height)
    else
      FSVGDrawing.StretchDraw(FControlArea.Canvas2D,
                              0,
                              0,
                              FControlArea.Canvas2D.Width,
                              FControlArea.Canvas2D.Height)
  end else
    FSVGDrawing.Draw(FControlArea.Canvas2D, 0, 0, cuPixel);
end;

procedure THMICustomVectorControl.ReloadDrawing;
var
  SVGStream: TMemoryStream;
begin
  if FSVGContents.Count<=0 then exit;

  SVGStream:=TMemoryStream.Create;
  try
    FSVGContents.SaveToStream(SVGStream);
    SVGStream.Position:=0;
    FSVGDrawing.LoadFromStream(SVGStream);
  finally
    FreeAndNil(SVGStream);
  end;
end;

constructor THMICustomVectorControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSVGDrawing:=TBGRASVG.Create;
  FSVGContents:=TTextStrings.Create;
end;

destructor THMICustomVectorControl.Destroy;
begin
  FreeAndNil(FSVGDrawing);
  FreeAndNil(FSVGContents);
  inherited Destroy;
end;

end.

