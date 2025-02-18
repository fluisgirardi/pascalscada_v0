{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementa um controle para exibição de valores numéricos em forma de barra de progresso.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a control to show numeric values in a progress bar.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIProgressBar;

interface

uses
  SysUtils, Classes, Controls, ComCtrls, HMITypes, PLCTag, ProtocolTypes, Tag,
  hmi_draw_basiccontrol, Graphics;

type

  {$IFDEF PORTUGUES}
  {:
  Implementa um controle para exibição de valores numéricos em forma de barra de
  progresso.

  @bold(Para maiores informações consulte a documentação da classe TProgressBar
  de seu ambiente de desenvolvimento.)
  }
  {$ELSE}
  {:
  Implements a control to show numeric values of tags in a progress bar.

  @bold(To get more information see the documentation of the class TProgressBar
  of your development environment.)
  }
  {$ENDIF}

  { THMIProgressBar }

  THMIProgressBar = class(THMIBasicControl, IHMIInterface)
  private
    FBackgroundColor: TColor;
    FRegInSecMan:Boolean;
    FMax: Double;
    FMin: Double;
    FOrientation: TProgressBarOrientation;
    FTag:TPLCTag;
    FRefreshPending:Boolean;

    procedure SetBackgroundColor(AValue: TColor);
    procedure SetMax(AValue: Double);
    procedure SetMin(AValue: Double);
    procedure SetOrientation(AValue: TProgressBarOrientation);

    function  GetPosition:Double;

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
  protected
    function Progress:Double;
    //: @exclude
    function GetHMITag: TPLCTag; override;
    procedure SetHMITag(t: TPLCTag); override;
    //: @exclude
    procedure Loaded; override;
    procedure UpdateShape; override;
    procedure DrawControl; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshProgress(Data: PtrInt);
  published
    //: @exclude
    property Enabled:Boolean read FIsEnabled write SetEnabled;

    {$IFDEF PORTUGUES}
    {:
    Tag numérico que será usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    }
    {$ELSE}
    {:
    Numeric tag that will be used by the control.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    }
    {$ENDIF}
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode;
    property Min: Double read FMin write SetMin;
    property Max: Double read FMax write SetMax;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
    property BackgroundColor:TColor read FBackgroundColor write SetBackgroundColor default clSilver;
    property Color: TColor read FBodyColor write SetBodyColor default clGreen;
    property Cursor;
    property Align;
    property OnClick;
    property BorderColor;
  end;

implementation

uses hsstrings, ControlSecurityManager, Math, BGRABitmap, BGRABitmapTypes,
  Forms;

constructor THMIProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegInSecMan:=GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
  if not FRegInSecMan then begin
    {$IFNDEF WINDOWS}
    writeln('FIX-ME: Failed to register class ',ClassName,' instace with name="',Name,'" in the ControlSecurityManager?',{$i %FILE%},':',{$i %LINE%});
    {$ENDIF}
  end;
  FIsEnabled:=true;
  FBodyColor:=clGreen;
  FBackgroundColor:=clSilver;
end;

destructor THMIProgressBar.Destroy;
begin
  if FRegInSecMan then
    GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface)
  else begin
    {$IFNDEF WINDOWS}
    writeln('FIX-ME: Why class ',ClassName,', instace name="',Name,'" ins''t registered in ControlSecurityManager?',{$i %FILE%},':',{$i %LINE%});
    {$ENDIF}
  end;

  Application.RemoveAsyncCalls(Self);
  if Assigned(FTag) then
    Ftag.RemoveAllHandlersFromObject(Self);
  inherited Destroy;
end;

procedure THMIProgressBar.RefreshProgress(Data: PtrInt);
begin
  FRefreshPending:=false;
  if ([csReading,csLoading,csDestroying]*ComponentState<>[]) or (FTag=nil) then
    exit;

  if (FTag<>nil) and (Supports(FTag, ITagNumeric)) then
    InvalidateDraw;
end;

procedure THMIProgressBar.Loaded;
begin
  inherited Loaded;
  CanBeAccessed(GetControlSecurityManager.CanAccess(GetControlSecurityCode));
  TagChangeCallBack(GetHMITag);
end;

procedure THMIProgressBar.UpdateShape;
begin
  //this control is a rect, so, it will not be shaped.
  //inherited UpdateShape;
end;

procedure THMIProgressBar.DrawControl;
var
  aux: Double;

function ProgressNormalized:Double;
begin
  Result:=((Progress-FMin)/ifthen((FMax-FMin)=0,1,(FMax-FMin)));
  if Result<0 then Result:=0;
  if Result>1 then Result:=1;
end;

begin
  inherited DrawControl;
  FControlArea.CanvasBGRA.Brush.Color:= FBackgroundColor;
  FControlArea.CanvasBGRA.Pen.Color  := FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  := FBorderWidth;
  FControlArea.CanvasBGRA.Rectangle(0,0,Width,Height);

  case Orientation of
    pbVertical: begin
      FControlArea.CanvasBGRA.Pen.Width  :=0;
      FControlArea.CanvasBGRA.Brush.Color:= FBodyColor;
      aux:=Height*((FMax-Progress)/ifthen((FMax-FMin)=0,1,(FMax-FMin)));
      FControlArea.RectangleAntialias(FBorderWidth,
                                      Height-(Height*ProgressNormalized),
                                      Width-(FBorderWidth*2),
                                      Height-FBorderWidth,
                                      ColorToBGRA(FBorderColor),
                                      0,
                                      ColorToBGRA(FBodyColor));

    end;

    pbHorizontal: begin
      FControlArea.CanvasBGRA.Pen.Width  :=0;
      FControlArea.RectangleAntialias(FBorderWidth,
                                      FBorderWidth,
                                      Width*ProgressNormalized,
                                      height-(FBorderWidth*2),
                                      ColorToBGRA(FBorderColor),
                                      0,
                                      ColorToBGRA(FBodyColor));

    end;

    pbRightToLeft: begin
      FControlArea.CanvasBGRA.Pen.Width  :=0;
      FControlArea.CanvasBGRA.Brush.Color:= clDefault;
      FControlArea.RectangleAntialias(Width-(Width*ProgressNormalized),
                                      FBorderWidth,
                                      Width-FBorderWidth,
                                      height-(FBorderWidth*2),
                                      ColorToBGRA(FBorderColor),
                                      0,
                                      ColorToBGRA(FBodyColor));

    end;
    pbTopDown: begin
      FControlArea.CanvasBGRA.Pen.Width  :=0;
      FControlArea.CanvasBGRA.Brush.Color:= FBodyColor;
      aux:=Height*((FMax-Progress)/ifthen((FMax-FMin)=0,1,(FMax-FMin)));
      FControlArea.RectangleAntialias(FBorderWidth,
                                      FBorderWidth,
                                      Width-(FBorderWidth*2),
                                      Height*ProgressNormalized,
                                      ColorToBGRA(FBorderColor),
                                      0,
                                      ColorToBGRA(FBodyColor));

    end;
  end;
end;

procedure THMIProgressBar.SetMin(AValue: Double);
begin
  if FMin=AValue then Exit;
  FMin:=AValue;
  InvalidateDraw;
end;

procedure THMIProgressBar.SetOrientation(AValue: TProgressBarOrientation);
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  InvalidateDraw;
end;

procedure THMIProgressBar.SetMax(AValue: Double);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;
  InvalidateDraw;
end;

procedure THMIProgressBar.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor=AValue then Exit;
  FBackgroundColor:=AValue;

  if ComponentState*[csReading, csLoading]=[] then
    InvalidateDraw;
end;

procedure THMIProgressBar.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  //check if the tag is valid.
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  //if the control is linked with some tag, remove the old link.
  if FTag<>nil then begin
    FTag.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if t<>nil then begin
    t.AddWriteFaultHandler(@WriteFaultCallBack);
    t.AddTagChangeHandler(@TagChangeCallBack);
    t.AddRemoveTagHandler(@RemoveTagCallBack);
    FTag := t;
    RefreshProgress(0);
  end;
  FTag := t;
  InvalidateDraw;
end;

function  THMIProgressBar.GetHMITag:TPLCTag;
begin
  Result := FTag;
end;

function  THMIProgressBar.GetPosition:Double;
begin
   Result := 0;
   if (FTag<>nil) AND Supports(FTag, ITagNumeric) then begin
      Result := (FTag as ITagNumeric).Value;
   end;
end;

procedure THMIProgressBar.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIProgressBar.TagChangeCallBack(Sender: TObject);
begin
  if (Application.Flags*[AppDoNotCallAsyncQueue]=[]) and (FRefreshPending=false) then begin
    FRefreshPending:=true;
    Application.QueueAsyncCall(@RefreshProgress, 0);
  end;
end;

procedure THMIProgressBar.RemoveTagCallBack(Sender: TObject);
begin
  if FTag=Sender then begin
    FTag := nil;
    InvalidateDraw;
  end;
end;

function THMIProgressBar.Progress: Double;
begin
  if (FTag<>nil) and (Supports(FTag,ITagNumeric)) then
    Result:=(FTag as ITagNumeric).GetValue
  else
    Result:=FMin;
end;

end.
