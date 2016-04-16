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

  THMIProgressBar = class(THMIBasicControl, IHMIInterface, IHMITagInterface)
  private
    FMax: Double;
    FMin: Double;
    FOrientation: TProgressBarOrientation;
    FTag:TPLCTag;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;

    FSecurityCode:String;
    procedure SetMax(AValue: Double);
    procedure SetMin(AValue: Double);
    procedure SetOrientation(AValue: TProgressBarOrientation);
    procedure SetSecurityCode(sc:String);

    //: @seealso(IHMIInterface.SetHMITag)
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
     function GetControlSecurityCode:String;
    //: @seealso(IHMIInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean);
    //: @seealso(IHMIInterface.MakeUnsecure)
    procedure MakeUnsecure;

    function  GetPosition:Double;

    //implements the IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    function Progress:Double;
    //: @exclude
    procedure SetEnabled(e:Boolean); override;
    //: @exclude
    procedure Loaded; override;
    procedure UpdateShape; override;
    procedure DrawControl; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
    property Min: Double read FMin write SetMin;
    property Max: Double read FMax write SetMax;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
    property Color: TColor read FBodyColor write SetBodyColor default clGreen;
    property Cursor;
    property Align;
    property OnClick;
    property BorderColor;
  end;

implementation

uses hsstrings, ControlSecurityManager, BGRABitmap, BGRABitmapTypes;

constructor THMIProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsEnabled:=true;
  FBodyColor:=clGreen;
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIProgressBar.Destroy;
begin
  if Assigned(FTag) then
    Ftag.RemoveCallBacks(Self as IHMITagInterface);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

procedure THMIProgressBar.Loaded;
begin
   inherited Loaded;
   NotifyTagChange(Self);
end;

procedure THMIProgressBar.UpdateShape;
begin
  //this control is a rect, so, it will not be shaped.
  //inherited UpdateShape;
end;

procedure THMIProgressBar.DrawControl;
begin
  inherited DrawControl;
  FControlArea.CanvasBGRA.Brush.Color:= FBodyColor;
  FControlArea.CanvasBGRA.Pen.Color  := FBorderColor;
  FControlArea.CanvasBGRA.Pen.Width  := FBorderWidth;


  case Orientation of
    pbVertical: begin
      FControlArea.CanvasBGRA.Rectangle(0,0,Width,Height);
      FControlArea.CanvasBGRA.Pen.Width  :=0;
      FControlArea.CanvasBGRA.Brush.Color:= clDefault;
      FControlArea.RectangleAntialias(FBorderWidth/2,
                                      FBorderWidth/2,
                                      Width-FBorderWidth,
                                      Height/(FMax-FMin)*(FMax-Progress),
                                      ColorToBGRA(FBorderColor),
                                      0,
                                      ColorToBGRA(clSilver));

    end;

    pbHorizontal: begin
      FControlArea.CanvasBGRA.Brush.Color:= clSilver;
      FControlArea.CanvasBGRA.Rectangle(0,0,Width,Height);
      FControlArea.CanvasBGRA.Pen.Width  :=0;
      FControlArea.RectangleAntialias(FBorderWidth/2,
                                      FBorderWidth/2,
                                      width/(FMax-FMin)*(Progress-FMin),
                                      height-FBorderWidth,
                                      ColorToBGRA(FBorderColor),
                                      0,
                                      ColorToBGRA(FBodyColor));

    end;

    pbRightToLeft: begin
      FControlArea.CanvasBGRA.Rectangle(0,0,Width,Height);
      FControlArea.CanvasBGRA.Pen.Width  :=0;
      FControlArea.CanvasBGRA.Brush.Color:= clDefault;
      FControlArea.RectangleAntialias(FBorderWidth/2,
                                      FBorderWidth/2,
                                       width/(FMax-FMin)*(FMax-Progress),
                                       height-FBorderWidth,
                                      ColorToBGRA(FBorderColor),
                                      0,
                                      ColorToBGRA(clSilver));

    end;
    pbTopDown: begin
      FControlArea.CanvasBGRA.Brush.Color:= clSilver;
      FControlArea.CanvasBGRA.Rectangle(0,0,Width,Height);
      FControlArea.CanvasBGRA.Pen.Width  :=0;
      FControlArea.RectangleAntialias(FBorderWidth/2,
                                      FBorderWidth/2,
                                      width-FBorderWidth,
                                      height/(FMax-FMin)*(Progress-FMin),
                                      ColorToBGRA(FBorderColor),
                                      0,
                                      ColorToBGRA(FBodycolor));

    end;



  end;






end;

function THMIProgressBar.GetControlSecurityCode:String;
begin
   Result:=FSecurityCode;
end;

procedure THMIProgressBar.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure THMIProgressBar.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMIProgressBar.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure THMIProgressBar.SetSecurityCode(sc: String);
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

procedure THMIProgressBar.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  //check if the tag is valid.
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  //if the control is linked with some tag, remove the old link.
  if FTag<>nil then begin
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if t<>nil then begin
    t.AddCallBacks(Self as IHMITagInterface);
    FTag := t;
    NotifyTagChange(self);
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

procedure THMIProgressBar.NotifyReadOk;
begin

end;

procedure THMIProgressBar.NotifyReadFault;
begin

end;

procedure THMIProgressBar.NotifyWriteOk;
begin

end;

procedure THMIProgressBar.NotifyWriteFault;
begin
  NotifyTagChange(Self);
end;

procedure THMIProgressBar.NotifyTagChange(Sender:TObject);
begin
  if ([csReading]*ComponentState<>[]) or (FTag=nil) then
    exit;

  if (FTag<>nil) and (Supports(FTag, ITagNumeric)) then
    InvalidateDraw;
end;

procedure THMIProgressBar.RemoveTag(Sender:TObject);
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
