unit hmi_draw_basic_vector_control;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hmi_draw_basiccontrol, BGRASVG, BGRASVGShapes, BGRASVGType,
  TextStrings, BGRAUnits, BGRABitmapTypes, hmi_polyline, hmi_flow_zones,
  HMIZones, hsutils, PLCTag;

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
    procedure Loaded; override;
    property  SVGContents:TStrings read FSVGContents write SetSVGContents;
    property  Stretch:Boolean read FStretch write SetStretch;
    property  Proportional:Boolean read FProportional write SetProportional;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { THMIVectorFlowZone }

  THMIVectorFlowZone = class(THMIFlowZone)
  private
    FSVGChanges: TStrings;
    procedure SetSVGChanges(AValue: TStrings);
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    class function ValidStatement(aLine: String): Boolean;
  published
    property SVGChanges:TStrings read FSVGChanges write SetSVGChanges;
  end;

  { THMIVectorFlowZones }

  THMIVectorFlowZones = class(TZones)
    constructor Create(aOwner: TPersistent);
    function Add: THMIVectorFlowZone;
  end;

  { THMICustomFlowVectorControl }

  THMICustomFlowVectorControl = class(THMICustomVectorControl)
  private
    FFlowOutputs: THMIFlowPolylinesCollection;
    FInputFlowPolyline: THMIFlowPolyline;
    FPLCTag: TPLCTag;
    FStates: THMIVectorFlowZones;
    procedure RemoveTagCallBack(Sender: TObject);
    procedure SetFlowOutputs(AValue: THMIFlowPolylinesCollection);
    procedure SetInputFlowPolyline(AValue: THMIFlowPolyline);
    procedure SetStates(AValue: THMIVectorFlowZones);
    procedure TagChangeCallBack(Sender: TObject);
    procedure UpdateDrawAndFlowDelayed(Data: PtrInt);
    procedure WriteFaultCallBack(Sender: TObject);
  protected
    procedure UpdateDrawAndFlow;
    procedure SetHMITag(t: TPLCTag); override;
    property ColorAndFlowStates:THMIVectorFlowZones read FStates write SetStates;
    property InputFlowPolyline:THMIFlowPolyline read FInputFlowPolyline write SetInputFlowPolyline;
    property FlowOutputPolylines:THMIFlowPolylinesCollection read FFlowOutputs write SetFlowOutputs;
    property PLCTag:TPLCTag read FPLCTag write SetHMITag;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  THMIBasicVectorControl = class(THMICustomVectorControl)
  published
    property SVGContents;
    property Stretch;
    property Proportional;
  end;

implementation

uses strutils, ProtocolTypes, hsstrings, Forms, hmi_animation_timers;

{ THMIVectorFlowZone }

procedure THMIVectorFlowZone.SetSVGChanges(AValue: TStrings);
var
  l: Integer;
  line: String;
begin
  if assigned(AValue) then begin
    for l:=0 to AValue.Count-1 do begin
      line:=AValue.Strings[l];
      if ValidStatement(line)=false then begin
        raise Exception.Create(Format('Invalid statement at line %d: %s%s',[l,LineEnding,line]));
      end;
    end;
    FSVGChanges.Assign(AValue);
  end;
end;

constructor THMIVectorFlowZone.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  FSVGChanges:=TTextStrings.Create;
end;

destructor THMIVectorFlowZone.Destroy;
begin
  FreeAndNil(FSVGChanges);
  inherited Destroy;
end;

class function THMIVectorFlowZone.ValidStatement(aLine: String): Boolean;
var
  LineParts1, LineParts2: TStringArray;
  aCleanLine: String;
  aux: Longint;
begin
  Result:=false;
  aCleanLine:=LowerCase(aLine);

  //default flow output.
  if aCleanLine='flow' then begin
    Result:=true;
    exit;
  end;

  LineParts1 := ExplodeString('=', aCleanLine);
  //checks if some_id.someprop=somevalue is valid
  if Length(LineParts1)=2 then begin
    LineParts2 := ExplodeString('.',LineParts2[0]);
    //checks if some_id.someprop is valid
    if Length(LineParts2)=2 then begin
      //checks if some_id is valid.
      if pos(' ',LineParts2[0])>0 then exit;
      //some_id.fill=... or some_id.border=...
      if (LineParts2[1]='fill') or (LineParts2[1]='border') then begin
        if (LineParts1[1]='fill') or (LineParts1[1]='border') or (LineParts1[1]='flow') then begin
          Result:=true;
        end else begin
          Result:=TryStrToInt(LineParts1[1],aux);
        end;
      end else begin
        if (LineParts2[0]='flow') and (LineParts2[1]='output') then begin
          result := TryStrToInt(LineParts1[1],aux);
        end;
      end;
    end;
  end;
end;

{ THMIVectorFlowZones }

constructor THMIVectorFlowZones.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner,THMIVectorFlowZone);
end;

function THMIVectorFlowZones.Add: THMIVectorFlowZone;
begin
  Result:=THMIVectorFlowZone(inherited Add);
end;

{ THMICustomFlowVectorControl }

procedure THMICustomFlowVectorControl.SetFlowOutputs(
  AValue: THMIFlowPolylinesCollection);
begin
  if AValue=nil then Exit;

  FFlowOutputs.Assign(AValue);
end;

procedure THMICustomFlowVectorControl.RemoveTagCallBack(Sender: TObject);
begin
  if FPLCTag=Sender then
    FPLCTag:=nil;
end;

procedure THMICustomFlowVectorControl.SetInputFlowPolyline(
  AValue: THMIFlowPolyline);
begin
  if FInputFlowPolyline=AValue then
    Exit;

  if Assigned(aValue) and (not Supports(AValue, IColorChangeNotification)) then
    exit;

  if Assigned(FInputFlowPolyline) then
    (FInputFlowPolyline as IColorChangeNotification).RemoveNotifyCallback(Self as IColorChangeNotification);

  if Assigned(aValue) then
    (AValue as IColorChangeNotification).AddNotifyCallback(self as IColorChangeNotification);

  FInputFlowPolyline:=AValue;
  UpdateDrawAndFlow;
end;

procedure THMICustomFlowVectorControl.SetStates(AValue: THMIVectorFlowZones);
begin
  if AValue=nil then Exit;
  FStates.Assign(AValue);
end;

procedure THMICustomFlowVectorControl.TagChangeCallBack(Sender: TObject);
begin
  UpdateDrawAndFlow;
end;

procedure THMICustomFlowVectorControl.UpdateDrawAndFlowDelayed(Data: PtrInt);
begin

end;

procedure THMICustomFlowVectorControl.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Sender);
end;

procedure THMICustomFlowVectorControl.UpdateDrawAndFlow;
begin
  if (Application.Flags*[AppDoNotCallAsyncQueue])=[] then begin
    Application.QueueAsyncCall(@UpdateDrawAndFlowDelayed,0);
  end;
end;

procedure THMICustomFlowVectorControl.SetHMITag(t: TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  //check if the tag is valid (only numeric tags);
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  //removes the old link.
  if FPLCTag<>nil then begin
    FPLCTag.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if t<>nil then begin
    t.AddWriteFaultHandler(@WriteFaultCallBack);
    t.AddTagChangeHandler(@TagChangeCallBack);
    t.AddRemoveTagHandler(@RemoveTagCallBack);
    FPLCTag := t;
    UpdateDrawAndFlow;
  end;
  FPLCTag := t;
end;

constructor THMICustomFlowVectorControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlowOutputs:=THMIFlowPolylinesCollection.Create(Self);
  FStates:=THMIVectorFlowZones.Create(Self);
end;

destructor THMICustomFlowVectorControl.Destroy;
begin
  if Assigned(FInputFlowPolyline) then
    (FInputFlowPolyline as IColorChangeNotification).RemoveNotifyCallback(Self as IColorChangeNotification);

  Application.RemoveAsyncCalls(Self);

  FreeAndNil(FFlowOutputs);
  FreeAndNil(FStates);
  inherited Destroy;
end;

{ THMIBasicVectorControl }

procedure THMICustomVectorControl.SetSVGContents(AValue: TStrings);
begin
  if Assigned(AValue) then begin
    FSVGContents.Assign(AValue);
    if ([csReading,csLoading]*ComponentState)=[] then begin
      ReloadDrawing;
      InvalidateShape;
    end;
  end;
end;

procedure THMICustomVectorControl.SetStretch(AValue: Boolean);
begin
  if FStretch=AValue then Exit;
  FStretch:=AValue;
  if ([csReading,csLoading]*ComponentState)=[] then
    InvalidateShape;
end;

procedure THMICustomVectorControl.SetProportional(AValue: Boolean);
begin
  if FProportional=AValue then Exit;
  FProportional:=AValue;
  if ([csReading,csLoading]*ComponentState)=[] then
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
                              FControlArea.Canvas2D.Width-1,
                              FControlArea.Canvas2D.Height-1)
    else
      FSVGDrawing.StretchDraw(FControlArea.Canvas2D,
                              0,
                              0,
                              FControlArea.Canvas2D.Width-1,
                              FControlArea.Canvas2D.Height-1)
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

procedure THMICustomVectorControl.Loaded;
begin
  inherited Loaded;
  ReloadDrawing;
  InvalidateShape;
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

