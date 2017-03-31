unit hmi_draw_basic_vector_control;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hmi_draw_basiccontrol, BGRASVG, BGRASVGShapes, BGRASVGType,
  TextStrings, Graphics, BGRAUnits, BGRABitmapTypes, hmi_polyline,
  hmi_flow_zones, hmibasiccolletion, HMIZones, hmi_animation_timers, hsutils,
  PLCTag;

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
    procedure CheckAutoSize;
    procedure DrawControl; override;
    procedure ReloadDrawing; virtual;
    procedure Loaded; override;
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    property  SVGContents:TStrings read FSVGContents write SetSVGContents;
    property  Stretch:Boolean read FStretch write SetStretch;
    property  Proportional:Boolean read FProportional write SetProportional;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TColorSource = (csCustom, csFill, csBorder, csFlow);

  TSVGChange = class(TObject);

  { TSVGColorChange }

  TSVGColorChange = class(TSVGChange)
  private
    FSVGId      : AnsiString;
    FCustomColor: TColor;
    FColorSource: TColorSource;
  public
    constructor Create(aSVGId:AnsiString; aColorSource:TColorSource; aCustomColor:TColor);
    property SVGID:AnsiString read FSVGId;
    property CustomColor:TColor read FCustomColor;
    property ColorSource:TColorSource read FColorSource;
  end;

  TSVGColorChangeClass = class of TSVGColorChange;

  TSVGFillChange   = class(TSVGColorChange);
  TSVGBorderChange = class(TSVGColorChange);

  { TOutputChange }

  TOutputChange = class(TSVGChange)
  private
    FOutputIndex: Integer;
  public
    constructor Create(aOutputIndex:Integer = 0);
    property OutputIndex:Integer read FOutputIndex;
  end;

  { THMIVectorFlowZone }

  THMIVectorFlowZone = class(THMIFlowZone)
  private
    FSVGChanges: TStrings;
    procedure SetSVGChanges(AValue: TStrings);
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Loaded; override;
    class function ValidRGBHex(aRGBStr:String):Boolean;
    class function RGBHexToColor(aRGBStr:String):TColor;
    class function ValidStatement(aLine: String): Boolean;
    function CreateStatementObject(aLine: String):TSVGChange;
  published
    property SVGChanges:TStrings read FSVGChanges write SetSVGChanges;
  end;

  { THMIVectorFlowZones }

  THMIVectorFlowZones = class(TZones)
    constructor Create(aOwner: TPersistent);
    function Add: THMIVectorFlowZone;
  end;

  { THMIOutputCollectionItem }

  THMIOutputCollectionItem = class(THMIBasicColletionItem)
  private
    FOuputPolyline: THMIFlowPolyline;
    procedure SetOuputPolyline(AValue: THMIFlowPolyline);
  protected
    function GetDisplayName: string; override;
  published
    property OutputPolyline:THMIFlowPolyline read FOuputPolyline write SetOuputPolyline;
  end;

  THMIOutputCollection = class(THMIBasicColletion)
    constructor Create(aOwner:TPersistent);
    function Add: THMIOutputCollectionItem;
  end;

  { THMICustomFlowVectorControl }

  THMICustomFlowVectorControl = class(THMICustomVectorControl, IColorChangeNotification)
  private
    FFlowOutputs: THMIOutputCollection;
    FInputFlowPolyline: THMIFlowPolyline;
    FOnDrawChanges: TNotifyEvent;
    FPLCTag: TPLCTag;
    FStates: THMIVectorFlowZones;
    FCurrentZone,
    FOwnerZone: THMIVectorFlowZone;
    procedure BlinkTimer(Sender: TObject);
    procedure RemoveTagCallBack(Sender: TObject);
    procedure SetFlowOutputs(AValue: THMIOutputCollection);
    procedure SetInputFlowPolyline(AValue: THMIFlowPolyline);
    procedure SetStates(AValue: THMIVectorFlowZones);
    procedure TagChangeCallBack(Sender: TObject);
    procedure UpdateDrawAndFlowDelayed(Data: PtrInt);
    procedure WriteFaultCallBack(Sender: TObject);

    //IColorChangeNotification
    procedure AddNotifyCallback(WhoNotify:IColorChangeNotification);
    procedure RemoveNotifyCallback(WhoRemove:IColorChangeNotification);
    procedure NotifyFree(const WhoWasDestroyed:THMIFlowPolyline);
    procedure NotifyChange(const WhoChanged:THMIFlowPolyline);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure ShowZone(aZone:THMIVectorFlowZone);
    procedure UpdateDrawAndFlow;
    procedure SetHMITag(t: TPLCTag); override;
    procedure Loaded; override;
    property ColorAndFlowStates:THMIVectorFlowZones read FStates write SetStates;
    property InputFlowPolyline:THMIFlowPolyline read FInputFlowPolyline write SetInputFlowPolyline;
    property FlowOutputPolylines:THMIOutputCollection read FFlowOutputs write SetFlowOutputs;
    property PLCTag:TPLCTag read FPLCTag write SetHMITag;
    property OnDrawChanges:TNotifyEvent read FOnDrawChanges write FOnDrawChanges;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  THMI2OutFlowVectorControl = class(THMICustomFlowVectorControl)
  protected
    function GetOutput1: THMIFlowPolyline;
    function GetOutput2: THMIFlowPolyline;
    procedure SetOutput1(AValue: THMIFlowPolyline);
    procedure SetOutput2(AValue: THMIFlowPolyline);

    property Output1:THMIFlowPolyline read GetOutput1 write SetOutput1;
    property Output2:THMIFlowPolyline read GetOutput2 write SetOutput2;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  THMI3OutFlowVectorControl = class(THMI2OutFlowVectorControl)
  private
    function GetOutput3: THMIFlowPolyline;
    procedure SetOutput3(AValue: THMIFlowPolyline);
  protected
    property Output3:THMIFlowPolyline read GetOutput3 write SetOutput3;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TForkedFlowValveOrientation = (ffvRight, ffvLeft);

  { THMIForkedFlowValve }

  THMIForkedFlowValve = class(THMI2OutFlowVectorControl)
  private
    FOrientation,
    FOrientationLoaded: TForkedFlowValveOrientation;
    procedure SetOrientation(AValue: TForkedFlowValveOrientation);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSize;
    property ColorAndFlowStates;
    property InputFlowPolyline;
    property PLCTag;
    property Proportional;
    property Stretch;
    property OutputPolylineLeft:THMIFlowPolyline read GetOutput1 write SetOutput1;
    property OutputPolylineRight:THMIFlowPolyline read GetOutput2 write SetOutput2;
    property Orientation:TForkedFlowValveOrientation read FOrientation write SetOrientation;
  end;

  { THMIThreeWayFlowValve }

  THMIThreeWayFlowValve = class(THMI3OutFlowVectorControl)
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSize;
    property ColorAndFlowStates;
    property InputFlowPolyline;
    property PLCTag;
    property Proportional;
    property Stretch;
    property OutputPolylineLeft:THMIFlowPolyline read GetOutput1 write SetOutput1;
    property OutputPolylineMiddle:THMIFlowPolyline read GetOutput2 write SetOutput2;
    property OutputPolylineRight:THMIFlowPolyline read GetOutput3 write SetOutput3;
  end;

  THMIBasicVectorControl = class(THMICustomVectorControl)
  published
    property AutoSize;
    property SVGContents;
    property Stretch;
    property Proportional;
  end;

  THMIFlowVectorControl = class(THMICustomFlowVectorControl)
  published
    property AutoSize;
    property ColorAndFlowStates;
    property FlowOutputPolylines;
    property InputFlowPolyline;
    property PLCTag;
    property Proportional;
    property Stretch;
    property SVGContents;
  end;

implementation

uses strutils, math, ProtocolTypes, hsstrings, Forms, LResources;

{ THMIThreeWayFlowValve }

procedure THMIThreeWayFlowValve.Loaded;
begin
  inherited Loaded;
  CheckAutoSize;
end;

constructor THMIThreeWayFlowValve.Create(AOwner: TComponent);
var
  lrs: TLResource;
begin
  inherited Create(AOwner);

  lrs:=LazarusResources.Find('three-way-flow-valve');

  FSVGContents.Clear;
  if Assigned(lrs) then
   FSVGContents.Text:=lrs.Value;
end;

{ THMIForkedFlowValve }

procedure THMIForkedFlowValve.SetOrientation(AValue: TForkedFlowValveOrientation
  );
var
  //ms: TLazarusResourceStream;
  lrs: TLResource;
  sz: Int64;
begin
  if (ComponentState*[csReading,csLoading])<>[] then begin
    FOrientationLoaded:=AValue;
    exit;
  end;

  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  CheckAutoSize;

  case FOrientation of
    ffvRight: lrs:=LazarusResources.Find('forked_valve_right');
    ffvLeft:  lrs:=LazarusResources.Find('forked_valve_left');
  end;

  FSVGContents.Clear;
  if Assigned(lrs) then
     FSVGContents.Text:=lrs.Value;

  ShowZone(FCurrentZone);
  InvalidateShape;
end;

procedure THMIForkedFlowValve.Loaded;
begin
  inherited Loaded;
  case FOrientationLoaded of
    ffvRight: FOrientation:=ffvLeft;
    ffvLeft:  FOrientation:=ffvRight;
  end;
  SetOrientation(FOrientationLoaded);
end;

constructor THMIForkedFlowValve.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrientation:=ffvRight;
  if ([csDesigning]*ComponentState)<>[] then AdjustSize;
end;

{ THMI3OutFlowVectorControl }

function THMI3OutFlowVectorControl.GetOutput3: THMIFlowPolyline;
begin
  if FFlowOutputs.Count>2 then
    Result:=THMIOutputCollectionItem(FFlowOutputs.Items[2]).OutputPolyline
  else
    Result:=nil;
end;

procedure THMI3OutFlowVectorControl.SetOutput3(AValue: THMIFlowPolyline);
begin
  if FFlowOutputs.Count>2 then
    THMIOutputCollectionItem(FFlowOutputs.Items[2]).OutputPolyline:=AValue;
end;

constructor THMI3OutFlowVectorControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  while FFlowOutputs.Count<3 do
    FFlowOutputs.Add;
end;

{ THMI2OutFlowVectorControl }

function THMI2OutFlowVectorControl.GetOutput1: THMIFlowPolyline;
begin
  if FFlowOutputs.Count>0 then
    Result:=THMIOutputCollectionItem(FFlowOutputs.Items[0]).OutputPolyline
  else
    Result:=nil;
end;

function THMI2OutFlowVectorControl.GetOutput2: THMIFlowPolyline;
begin
  if FFlowOutputs.Count>1 then
    Result:=THMIOutputCollectionItem(FFlowOutputs.Items[1]).OutputPolyline
  else
    Result:=nil;
end;

procedure THMI2OutFlowVectorControl.SetOutput1(AValue: THMIFlowPolyline);
begin
  if FFlowOutputs.Count>0 then
    THMIOutputCollectionItem(FFlowOutputs.Items[0]).OutputPolyline:=AValue;
end;

procedure THMI2OutFlowVectorControl.SetOutput2(AValue: THMIFlowPolyline);
begin
  if FFlowOutputs.Count>1 then
    THMIOutputCollectionItem(FFlowOutputs.Items[1]).OutputPolyline:=AValue;
end;

constructor THMI2OutFlowVectorControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  while FFlowOutputs.Count<2 do
    FFlowOutputs.Add;
end;

{ TOutputChange }

constructor TOutputChange.Create(aOutputIndex: Integer);
begin
  inherited Create;
  FOutputIndex:=aOutputIndex;
end;

{ TSVGFillChange }

constructor TSVGColorChange.Create(aSVGId: AnsiString;
  aColorSource: TColorSource; aCustomColor: TColor);
begin
  inherited Create;
  FSVGId       := aSVGId;
  FCustomColor := aCustomColor;
  FColorSource := aColorSource;
end;

{ THMIOutputCollection }

constructor THMIOutputCollection.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner, THMIOutputCollectionItem);
end;

function THMIOutputCollection.Add: THMIOutputCollectionItem;
begin
  Result:=THMIOutputCollectionItem(inherited Add);
end;

{ THMIOutputCollectionItem }

procedure THMIOutputCollectionItem.SetOuputPolyline(AValue: THMIFlowPolyline);
begin
  if FOuputPolyline=AValue then Exit;

  if Assigned(AValue) then
    AValue.FreeNotification(TComponent(Collection.Owner));

  if Assigned(FOuputPolyline) then
    FOuputPolyline.RemoveFreeNotification(TComponent(Collection.Owner));

  FOuputPolyline:=AValue;

  NotifyChange;
end;

function THMIOutputCollectionItem.GetDisplayName: string;
begin
  if Assigned(FOuputPolyline) then
    Result:=FOuputPolyline.Name
  else
    Result:='(unassigned)';
end;

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

    for l:=0 to FSVGChanges.Count-1 do
      if Assigned(FSVGChanges.Objects[l]) then begin
        FSVGChanges.Objects[l].Destroy;
        FSVGChanges.Objects[l]:=nil;
      end;

    FSVGChanges.Assign(AValue);
    for l:=0 to FSVGChanges.Count-1 do begin
      FSVGChanges.Objects[l]:=CreateStatementObject(FSVGChanges.Strings[l]);
    end;
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

procedure THMIVectorFlowZone.Loaded;
var
  l: Integer;
begin
  inherited Loaded;
  for l:=0 to FSVGChanges.Count-1 do begin
    FSVGChanges.Objects[l]:=CreateStatementObject(FSVGChanges.Strings[l]);
  end;
end;

class function THMIVectorFlowZone.ValidRGBHex(aRGBStr: String): Boolean;
var
  i: Integer;
begin
  //#FFFFFF
  //1234567
  Result:=false;
  aRGBStr:=LowerCase(aRGBStr);
  if Length(aRGBStr)<>7 then exit;
  if aRGBStr[1]<>'#' then exit;
  for i:=2 to 7 do
    if (not (aRGBStr[i] in ['0'..'9'])) and (not (aRGBStr[i] in ['a'..'f'])) then
      exit;

  Result:=true;
end;

class function THMIVectorFlowZone.RGBHexToColor(aRGBStr: String): TColor;
begin
  Result:=RGBToColor(StrToInt('$'+MidStr(aRGBStr,2,2)),
                     StrToInt('$'+MidStr(aRGBStr,4,2)),
                     StrToInt('$'+MidStr(aRGBStr,6,2)));
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
    LineParts2 := ExplodeString('.',LineParts1[0]);
    //checks if some_id.someprop is valid
    if Length(LineParts2)=2 then begin
      //checks if some_id is valid.
      if pos(' ',LineParts2[0])>0 then exit;
      //some_id.fill=... or some_id.border=...
      if (LineParts2[1]='fill') or (LineParts2[1]='border') then begin
        if (LineParts1[1]='fill') or (LineParts1[1]='border') or (LineParts1[1]='flow') then begin
          Result:=true;
        end else begin
          Result:=ValidRGBHex(LineParts1[1]);
        end;
      end else begin
        if (LineParts2[0]='flow') and (LineParts2[1]='output') then begin
          result := TryStrToInt(LineParts1[1],aux);
        end;
      end;
    end;
  end;
end;

function THMIVectorFlowZone.CreateStatementObject(aLine: String
  ): TSVGChange;
var
  LineParts1, LineParts2: TStringArray;
  aCleanLine: String;
  aux: Longint;
  aClass: Integer;
  aColor: TColor;
  aColorSource: TColorSource;
begin
  Result:=Nil;
  aCleanLine:=LowerCase(aLine);

  //default flow output.
  if aCleanLine='flow' then begin
    Result:=TOutputChange.Create;
    exit;
  end;

  LineParts1 := ExplodeString('=', aCleanLine);
  //checks if some_id.someprop=somevalue is valid
  if Length(LineParts1)=2 then begin
    LineParts2 := ExplodeString('.',LineParts1[0]);
    //checks if some_id.someprop is valid
    if Length(LineParts2)=2 then begin
      //checks if some_id is valid.
      if pos(' ',LineParts2[0])>0 then exit;
      //some_id.fill=... or some_id.border=...
      aClass:=0;
      if (LineParts2[1]='fill')   then AClass:=1;
      if (LineParts2[1]='border') then AClass:=2;
      if (LineParts2[0]='flow') and (LineParts2[1]='output') then AClass:=3;

      if aClass in [1..2] then
        if (LineParts1[1]='fill') then begin
          aColor:=clBlack;
          aColorSource:=csFill;
        end else
          if (LineParts1[1]='border') then begin
            aColor:=clBlack;
            aColorSource:=csBorder;
          end else
            if (LineParts1[1]='flow') then begin
              aColor:=clBlack;
              aColorSource:=csFlow;
            end else begin
              if ValidRGBHex(LineParts1[1]) then begin
                aColor:=RGBHexToColor(LineParts1[1]);
                aColorSource:=csCustom;
              end else
                aClass:=0;
            end;

      case aClass of
        1: Result:=TSVGFillChange.Create  (LineParts2[0], aColorSource, aColor);
        2: Result:=TSVGBorderChange.Create(LineParts2[0], aColorSource, aColor);
        3: if TryStrToInt(LineParts1[1],aux) then
             Result:=TOutputChange.Create(aux);
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
  AValue: THMIOutputCollection);
begin
  if AValue=nil then Exit;

  FFlowOutputs.Assign(AValue);
end;

procedure THMICustomFlowVectorControl.RemoveTagCallBack(Sender: TObject);
begin
  if FPLCTag=Sender then
    FPLCTag:=nil;
end;

procedure THMICustomFlowVectorControl.BlinkTimer(Sender: TObject);
begin
  if (FCurrentZone.BlinkWith<0) or (THMIVectorFlowZone(FStates.Items[FCurrentZone.BlinkWith]).BlinkTime<>FCurrentZone.BlinkTime) then
    GetAnimationTimer.RemoveCallback(@BlinkTimer);

  if (FCurrentZone.BlinkWith>=0) AND (THMIVectorFlowZone(FStates.Items[FCurrentZone.BlinkWith]).BlinkTime<>FCurrentZone.BlinkTime) and (THMIVectorFlowZone(FStates.Items[FCurrentZone.BlinkWith]).BlinkTime>0) then
      GetAnimationTimer.AddTimerCallback(THMIVectorFlowZone(FStates.Items[FCurrentZone.BlinkWith]).BlinkTime, @BlinkTimer);

  ShowZone(THMIVectorFlowZone(FStates.Items[FCurrentZone.BlinkWith]));
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
var
  zone: THMIVectorFlowZone;
  value:Double = Infinity;
begin
  if Assigned(FPLCTag) and Supports(FPLCTag, ITagNumeric) then
    value:=(FPLCTag as ITagNumeric).GetValue;

  zone:=THMIVectorFlowZone(FStates.GetZoneFromValue(value));
  if FOwnerZone<>zone then begin
    FOwnerZone:=zone;
    ShowZone(FOwnerZone);
    if (FCurrentZone<>nil) and (FCurrentZone.BlinkWith<>(-1)) and (FCurrentZone.BlinkTime>0) then begin
      GetAnimationTimer.AddTimerCallback(FCurrentZone.BlinkTime,@BlinkTimer);
    end;
  end else
    ShowZone(FCurrentZone);

  if Assigned(FOnDrawChanges) then
    try
      FOnDrawChanges(Self);
    except
    end;
end;

procedure THMICustomFlowVectorControl.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Sender);
end;

procedure THMICustomFlowVectorControl.AddNotifyCallback(
  WhoNotify: IColorChangeNotification);
begin

end;

procedure THMICustomFlowVectorControl.RemoveNotifyCallback(
  WhoRemove: IColorChangeNotification);
begin

end;

procedure THMICustomFlowVectorControl.NotifyFree(
  const WhoWasDestroyed: THMIFlowPolyline);
begin
  if WhoWasDestroyed=FInputFlowPolyline then begin
    FInputFlowPolyline:=nil;
    exit;
  end;
end;

procedure THMICustomFlowVectorControl.NotifyChange(
  const WhoChanged: THMIFlowPolyline);
begin
  UpdateDrawAndFlow;
end;

procedure THMICustomFlowVectorControl.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent<>Self) then begin
    if AComponent=FInputFlowPolyline then
      FInputFlowPolyline:=nil;
    for i:=0 to FFlowOutputs.Count-1 do
      if THMIOutputCollectionItem(FFlowOutputs.Items[i]).FOuputPolyline=AComponent then
        THMIOutputCollectionItem(FFlowOutputs.Items[i]).FOuputPolyline:=nil;
  end;
end;

procedure THMICustomFlowVectorControl.ShowZone(aZone: THMIVectorFlowZone);
var
  v: Double;
  l, FlowOutput, o: Integer;
  aSVGElement: TSVGElement       = nil;
  ColorObj: TSVGColorChange      = nil;
  outputChangeObj: TOutputChange = nil;

  function FindSVGElement(const aSVGId:AnsiString; const SVGContents:TSVGContent; out SVGElement:TSVGElement):Boolean;
  var
    i: Integer;
  begin
    Result:=False;
    if SVGContents=nil then exit;
    for i:=0 to SVGContents.ElementCount-1 do begin
      if LowerCase(SVGContents.Element[i].Attribute['id'])=aSVGId then begin
        Result:=true;
        SVGElement:=SVGContents.Element[i];
        break;
      end else
        if SVGContents.Element[i] is TSVGGroup then begin
          if FindSVGElement(aSVGId,TSVGGroup(SVGContents.Element[i]).Content,SVGElement) then begin
            Result:=true;
            Break;
          end;
        end;
    end;
  end;

  procedure ApplyColorToSVGElement(const aColorChange:TSVGColorChange; aSVGItem:TSVGElement; const AColor:TColor);
  begin
    if aColorChange is TSVGFillChange then
      aSVGItem.fillColor:=ColorToBGRA(AColor);
    if aColorChange is TSVGBorderChange then
      aSVGItem.strokeColor:=ColorToBGRA(AColor);
  end;
begin
  ReloadDrawing;
  FCurrentZone:=aZone;
  if aZone<>nil then begin
    for l:=0 to fCurrentZone.FSVGChanges.Count-1 do begin
      if fCurrentZone.FSVGChanges.Objects[l]=nil then continue;

      //fill and border color changes
      if fCurrentZone.FSVGChanges.Objects[l] is TSVGColorChange then begin
        ColorObj:=TSVGFillChange(fCurrentZone.FSVGChanges.Objects[l]);
        if (Assigned(aSVGElement) and (aSVGElement.Attribute['id']=ColorObj.SVGID)) or
           FindSVGElement(ColorObj.SVGID, FSVGDrawing.Content, aSVGElement) then begin

          case ColorObj.ColorSource of
            csCustom: ApplyColorToSVGElement(ColorObj, aSVGElement, ColorObj.CustomColor);
            csFill:   ApplyColorToSVGElement(ColorObj, aSVGElement, fCurrentZone.Color);
            csBorder: ApplyColorToSVGElement(ColorObj, aSVGElement, fCurrentZone.BorderColor);
            csFlow:
              if Assigned(FInputFlowPolyline) and (FInputFlowPolyline.LineColor<>FInputFlowPolyline.EmptyColor) then
                ApplyColorToSVGElement(ColorObj, aSVGElement, FInputFlowPolyline.LineColor);
          end;
        end;
      end;

      //border color changes
      if fCurrentZone.FSVGChanges.Objects[l] is TOutputChange then
        outputChangeObj:=TOutputChange(fCurrentZone.FSVGChanges.Objects[l]);
    end;
  end;

  FlowOutput:=-1;
  if Assigned(outputChangeObj) then FlowOutput:=outputChangeObj.OutputIndex;

  for o:=0 to FFlowOutputs.Count-1 do
    with FFlowOutputs.Items[o] as THMIOutputCollectionItem do begin
      if (o=FlowOutput) and Assigned(OutputPolyline) and Assigned(FInputFlowPolyline) then
        OutputPolyline.LineColor:=FInputFlowPolyline.LineColor
      else
        if Assigned(OutputPolyline) then
          OutputPolyline.LineColor:=OutputPolyline.EmptyColor;
    end;

  InvalidateDraw;
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

procedure THMICustomFlowVectorControl.Loaded;
begin
  inherited Loaded;
  FStates.Loaded;
  CheckAutoSize;
end;

constructor THMICustomFlowVectorControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlowOutputs:=THMIOutputCollection.Create(Self);
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

procedure THMICustomVectorControl.CheckAutoSize;
begin
  if AutoSize then begin
    InvalidatePreferredSize;
    AdjustSize;
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
  CheckAutoSize;
end;

procedure THMICustomVectorControl.Loaded;
begin
  inherited Loaded;
  ReloadDrawing;
  InvalidateShape;
end;

procedure THMICustomVectorControl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if (FSVGDrawing=nil) or ((FSVGDrawing.WidthAsCm=0) and (FSVGDrawing.HeightAsCm=0)) then
    ReloadDrawing;

  if FSVGDrawing<>nil then begin
    PreferredWidth  := trunc(FSVGDrawing.Units.Convert(FSVGDrawing.Width.value,  FSVGDrawing.Width.CSSUnit,  cuPixel, 96))+1;
    PreferredHeight := trunc(FSVGDrawing.Units.Convert(FSVGDrawing.Height.value, FSVGDrawing.Height.CSSUnit, cuPixel, 96))+1;
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

initialization

{$I hmi_draw_basic_vector_control.lrs}

end.

