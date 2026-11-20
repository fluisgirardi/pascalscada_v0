unit hmi_draw_basiccontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Types, Controls, Graphics, IntfGraphics, GraphType, FPImage,
  LCLIntf, LMessages, ControlSecurityManager, HMITypes, PLCTag;

{$IF defined(CM_DESIGNERHITTEST)}

{$ELSE}
const
  CM_DESIGNERHITTEST = CM_BASE + 100;
{$ENDIF}

type

  TDragManagerAccess = Class(TDragManager);

  {$IFDEF PORTUGUES}
  //: Um pixel da area do controle (FControlArea): 32 bits, azul primeiro - o
  //: formato B8G8R8A8 do TLazIntfImage. alpha=0 e' fora do desenho.
  {$ELSE}
  //: One pixel of the control area (FControlArea): 32 bits, blue first - the
  //: B8G8R8A8 layout of TLazIntfImage. alpha=0 is outside the drawing.
  {$ENDIF}
  TControlPixel = packed record
    blue, green, red, alpha: Byte;
  end;
  PControlPixel = ^TControlPixel;

  {$IFDEF PORTUGUES}
  //: O que a base faz com o desenho antes de usa-lo: nada, espelhar ou girar
  //: (girado, o desenho tem a largura e a altura trocadas, veja GetDrawingSize).
  {$ELSE}
  //: What the base does with the drawing before using it: nothing, mirror or
  //: rotate (rotated, the drawing has width and height swapped, see
  //: GetDrawingSize).
  {$ENDIF}
  TDrawingTransform = (dtNone, dtFlipHorizontal, dtFlipVertical, dtRotateCW, dtRotateCCW);

  {$DEFINE ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE}

  { THMIBasicControl }

  THMIBasicControl = class(TCustomControl, IHMIInterface)
  private
    FRegInSecMan:Boolean;
    //as duas camadas do desenho: a colorida e a mascara (preto = controle)
    //the two layers of the drawing: the coloured one and the mask (black =
    //control)
    FColorLayer,
    FMaskLayer:TBitmap;
    //a mascara so' e' redesenhada quando a forma muda; a cor muda sozinha
    //the mask is redrawn only when the shape changes; colour changes alone
    FMaskValid,
    FColorsOnly:Boolean;
    FMaskAlpha:array of Byte;
    FMaskWidth, FMaskHeight:Integer;
    //o bitmap que o Paint desenha, feito da FControlArea
    //the bitmap Paint draws, made from FControlArea
    FPaintBitmap:TBitmap;
    FPaintBitmapValid:Boolean;
    FDrawingMask:Boolean;
    procedure PrepareLayer(var ALayer:TBitmap; AWidth, AHeight:Integer);
    procedure ReadMask(AWidth, AHeight:Integer);
    procedure ComposeControlArea(AWidth, AHeight:Integer);
    procedure BuildShapeOnly;
    procedure RebuildShape;
  protected
    FSecurityCode:UTF8String;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    //: @seealso(IHMIInterface.SetHMITag)
    procedure SetHMITag({%H-}t:TPLCTag); virtual;                    //seta um tag
    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag; virtual;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
     function GetControlSecurityCode:UTF8String;
    //: @seealso(IHMIInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean);
    //: @seealso(IHMIInterface.MakeUnsecure)
    procedure MakeUnsecure;

    //: @exclude
    procedure SetEnabled(e:Boolean); override;
    procedure SetSecurityCode(sc: UTF8String);

    procedure DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
      Shift: TShiftState); virtual;
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton); virtual;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;
  protected
    FBorderColor:TColor;
    FBodyColor: TColor;
    FUpdateShape,
    FShouldRedraw:Boolean;

    FBorderWidth:Integer;
    {$IFDEF PORTUGUES}
    //: O desenho pronto do controle, RGBA de 32 bits (veja TControlPixel): o
    //: que o Paint mostra, o que o teste de acerto e o SetShape consultam.
    {$ELSE}
    //: The finished drawing of the control, 32-bit RGBA (see TControlPixel):
    //: what Paint shows, what the hit test and SetShape look at.
    {$ENDIF}
    FControlArea:TLazIntfImage;
    FUpdatingCount:Cardinal;

    FOldWidth, FOldHeight:Integer;

    //: @exclude
    FSimulateAppShutdown: Boolean;

    function  IsControlArea(X,Y:Integer):Boolean; virtual;
    procedure ForwardMouseMessage(var Message:TLMMouse); virtual;

    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TLMRButtonDown); message LM_RBUTTONDOWN;
    procedure WMMButtonDown(var Message: TLMMButtonDown); message LM_MBUTTONDOWN;
    procedure WMXButtonDown(var Message: TLMXButtonDown); message LM_XBUTTONDOWN;

    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TLMRButtonUp); message LM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TLMMButtonUp); message LM_MBUTTONUP;
    procedure WMXButtonUp(var Message: TLMXButtonUp); message LM_XBUTTONUP;

    procedure WMLButtonDBLCLK(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMRButtonDBLCLK(var Message: TLMRButtonDblClk); message LM_RBUTTONDBLCLK;
    procedure WMMButtonDBLCLK(var Message: TLMMButtonDblClk); message LM_MBUTTONDBLCLK;
    procedure WMXButtonDBLCLK(var Message: TLMXButtonDblClk); message LM_XBUTTONDBLCLK;
    procedure WMLButtonTripleCLK(var Message: TLMLButtonTripleClk); message LM_LBUTTONTRIPLECLK;
    procedure WMRButtonTripleCLK(var Message: TLMRButtonTripleClk); message LM_RBUTTONTRIPLECLK;
    procedure WMMButtonTripleCLK(var Message: TLMMButtonTripleClk); message LM_MBUTTONTRIPLECLK;
    procedure WMXButtonTripleCLK(var Message: TLMXButtonTripleClk); message LM_XBUTTONTRIPLECLK;
    procedure WMLButtonQuadCLK(var Message: TLMLButtonQuadClk); message LM_LBUTTONQUADCLK;
    procedure WMRButtonQuadCLK(var Message: TLMRButtonQuadClk); message LM_RBUTTONQUADCLK;
    procedure WMMButtonQuadCLK(var Message: TLMMButtonQuadClk); message LM_MBUTTONQUADCLK;
    procedure WMXButtonQuadCLK(var Message: TLMXButtonQuadClk); message LM_XBUTTONQUADCLK;

    procedure WMMouseMove(var Message: TLMMouseMove); message LM_MOUSEMOVE;

    function Cateto(p0, p1: Integer): Integer;
    function Degrees(x0, x1, y0, y1: Integer): Double;
    function Hipotenusa(x0, x1, y0, y1: Integer): Double;
    function Seno(x0, x1, y0, y1: Integer): Double;

    function  ControlArea(const pixel: TControlPixel): Boolean; virtual;
    procedure InvalidateDraw; virtual;
    procedure InvalidateShape; virtual;
    {$IFDEF PORTUGUES}
    //: Monta a FControlArea: chama o PaintControl uma vez com as cores e outra
    //: (so' se a forma mudou) em preto sobre branco, que vira a mascara, e
    //: aplica o DrawingTransform. Quem desenha de outro jeito (o controle
    //: vetorial) sobrescreve este metodo e preenche a FControlArea por conta
    //: propria, com ClearControlArea e ControlAreaChanged.
    {$ELSE}
    //: Builds FControlArea: calls PaintControl once with the colours and once
    //: more (only if the shape changed) in black on white, which becomes the
    //: mask, and applies DrawingTransform. Whoever draws some other way (the
    //: vector control) overrides this method and fills FControlArea on its
    //: own, with ClearControlArea and ControlAreaChanged.
    {$ENDIF}
    procedure DrawControl; virtual;
    {$IFDEF PORTUGUES}
    //: Desenha o controle no ACanvas (AWidth x AHeight, antes do
    //: DrawingTransform). E' chamado duas vezes: use ShapeColor (ou
    //: SetupCanvas) em toda cor, para que na passada da mascara tudo saia
    //: preto. O fundo ja' vem pintado; o que ficar sem pintar e' transparente.
    {$ELSE}
    //: Draws the control on ACanvas (AWidth x AHeight, before
    //: DrawingTransform). It is called twice: use ShapeColor (or SetupCanvas)
    //: for every colour, so that on the mask pass everything comes out black.
    //: The background is already painted; whatever is left unpainted is
    //: transparent.
    {$ENDIF}
    procedure PaintControl(ACanvas:TCanvas; AWidth, AHeight:Integer); virtual;
    //: @seealso(PaintControl)
    procedure GetDrawingSize(out AWidth, AHeight:Integer); virtual;
    //: @seealso(TDrawingTransform)
    function  DrawingTransform:TDrawingTransform; virtual;
    {$IFDEF PORTUGUES}
    //: @true quando o Paint pode chamar o PaintControl direto no Canvas do
    //: controle: quem recorta o desenho na tela e' o SetShape, entao so' a
    //: mascara precisa ser lida de volta, e so' quando a forma muda. Nesse
    //: modo a FControlArea que o Paint deixa tem so' o alfa (as cores ficam
    //: zeradas); o DrawControl, chamado direto, continua montando tudo.
    //: Padrao: sem DrawingTransform. Quem sobrescreve o DrawControl (o
    //: controle vetorial) responde @false.
    {$ELSE}
    //: @true when Paint may call PaintControl straight on the control's
    //: Canvas: what clips the drawing on screen is SetShape, so only the mask
    //: has to be read back, and only when the shape changes. In this mode the
    //: FControlArea Paint leaves holds only the alpha (colours stay zero);
    //: DrawControl, called directly, still builds everything. Default: no
    //: DrawingTransform. Whoever overrides DrawControl (the vector control)
    //: answers @false.
    {$ENDIF}
    function  PaintsDirectly:Boolean; virtual;
    {$IFDEF PORTUGUES}
    //: A cor a usar no PaintControl: a propria, ou preto na passada da mascara.
    {$ELSE}
    //: The colour to use in PaintControl: the colour itself, or black on the
    //: mask pass.
    {$ENDIF}
    function  ShapeColor(AColor:TColor):TColor;
    {$IFDEF PORTUGUES}
    //: Caneta e pincel de uma vez, ja' pelo ShapeColor. APenWidth<=0: sem borda.
    {$ELSE}
    //: Pen and brush at once, already through ShapeColor. APenWidth<=0: no
    //: border.
    {$ENDIF}
    procedure SetupCanvas(ACanvas:TCanvas; APenColor:TColor; APenWidth:Integer; ABrushColor:TColor);
    //: @true durante a passada da mascara / during the mask pass.
    property  DrawingMask:Boolean read FDrawingMask;
    //: Tamanho de um texto na fonte em que o PaintControl desenha.
    //: Size of a text in the font PaintControl draws with.
    function  MeasureText(const AText:String):TSize;
    //: Pontos com coordenadas reais arredondados para o TCanvas.
    //: Points with real coordinates rounded for TCanvas.
    class function RoundPoints(const APoints:array of TPointF):TPointArray;
    function  ControlAreaEmpty:Boolean;
    function  ControlAreaRow(Y:Integer):PControlPixel;
    //: FControlArea com AWidth x AHeight pixels transparentes.
    //: FControlArea with AWidth x AHeight transparent pixels.
    procedure ClearControlArea(AWidth, AHeight:Integer);
    //: Avisa que a FControlArea mudou (o Paint refaz o seu bitmap).
    //: Tells that FControlArea changed (Paint rebuilds its bitmap).
    procedure ControlAreaChanged;
    procedure UpdateShape; virtual;
    procedure Paint; override;
    procedure Resize; override;
    procedure CMHitTest(var Message: TCMHittest) ; message CM_HITTEST;
    procedure CMDesigerHitTest(var Message: TCMHittest) ; message CM_DESIGNERHITTEST;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetBodyColor(AValue: TColor); virtual;
    procedure SetBorderColor(AValue: TColor); virtual;
    procedure SetBorderWidth(AValue: Integer); virtual;
    procedure Loaded; override;
    property BorderColor:TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderWidth:Integer read FBorderWidth write SetBorderWidth default 1;
    property BodyColor:TColor read FBodyColor write SetBodyColor default clSilver;
    {$IFDEF PORTUGUES}
    //: simula, so' para testes automatizados, a janela de encerramento do
    //: app que callbacks enfileirados via Application.QueueAsyncCall
    //: precisam detectar (Application.Terminated nao tem setter publico, e
    //: TApplication.SetFlags filtra e so' deixa AppNoExceptionMessages
    //: passar por Application.Flags:=...). Fica falso o tempo todo fora de
    //: teste.
    {$ELSE}
    //: simulates, for automated tests only, the app-shutdown window that
    //: callbacks queued via Application.QueueAsyncCall need to detect
    //: (Application.Terminated has no public setter, and
    //: TApplication.SetFlags filters and only lets AppNoExceptionMessages
    //: through Application.Flags:=...). Stays false at all times outside of
    //: tests.
    {$ENDIF}
    property SimulateAppShutdown: Boolean read FSimulateAppShutdown write FSimulateAppShutdown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
  end;

implementation

uses math, LCLType, Forms;

{ TBasicSCADAControl }

procedure THMIBasicControl.InvalidateDraw;
begin
  FShouldRedraw:=true;
  Invalidate;
end;

procedure THMIBasicControl.InvalidateShape;
begin
  if [csDestroying]*ComponentState<>[] then exit;
  FUpdateShape:=true;
  FMaskValid:=false;
  //a area antiga fica ate' o Paint refazer: esvazia-la fazia o Invalidate do
  //gtk2 reconstruir a forma na hora, a cada chamada - um SetBounds chama
  //varias - em vez de uma vez so', no Paint
  //the old area stays until Paint rebuilds it: emptying it made the gtk2
  //Invalidate rebuild the shape right away, on every call - a SetBounds
  //makes several - instead of once, in Paint
  Invalidate;
end;

procedure THMIBasicControl.PrepareLayer(var ALayer: TBitmap; AWidth,
  AHeight: Integer);
begin
  //a camada so' cresce, com folga, e quem desenha e le usa so' AWidth x
  //AHeight dela: mudar o tamanho de um TBitmap (SetSize) no gtk2 custa caro
  //- uns 18 ms em 1200x800, mais 12 ms no primeiro desenho - e um
  //redimensionamento faria isso a cada pixel. Sobrando mais que o dobro da
  //area, ela e' refeita menor, para nao ler de volta pixels a toa.
  //the layer only grows, with some slack, and whoever draws and reads uses
  //only AWidth x AHeight of it: resizing a TBitmap (SetSize) on gtk2 is
  //expensive - about 18 ms at 1200x800, plus 12 ms on the first drawing -
  //and a resize would pay it on every pixel. With more than twice the area
  //to spare, it is rebuilt smaller, so as not to read pixels back for
  //nothing.
  if (ALayer=nil) or
     (ALayer.Width<AWidth) or (ALayer.Height<AHeight) or
     (Int64(ALayer.Width)*ALayer.Height > 2*Int64(Max(AWidth,1))*Max(AHeight,1)+4096) then begin
    FreeAndNil(ALayer);
    ALayer:=TBitmap.Create;
    ALayer.PixelFormat:=pf24bit;
    ALayer.SetSize(AWidth+Max(16, AWidth div 8), AHeight+Max(16, AHeight div 8));
  end;
  ALayer.Canvas.Pen.Style:=psSolid;
  ALayer.Canvas.Pen.Width:=1;
  ALayer.Canvas.Pen.Color:=clWhite;
  ALayer.Canvas.Brush.Style:=bsSolid;
  ALayer.Canvas.Brush.Color:=clWhite;
  ALayer.Canvas.FillRect(0, 0, AWidth, AHeight);
  //a mesma fonte do Canvas do controle, onde o Paint desenha direto
  //the same font as the control's Canvas, where Paint draws directly
  ALayer.Canvas.Font.Assign(Font);
end;

type
  //acesso direto aos bytes de uma camada, quando o formato permite
  //direct access to the bytes of a layer, when the format allows it
  TLayerAccess = record
    Img: TLazIntfImage;
    Fast: Boolean;
    Step, OfsR, OfsG, OfsB: Integer;
  end;

procedure OpenLayer(ABitmap: TBitmap; out ALayer: TLayerAccess);
var
  d: TRawImageDescription;
begin
  ALayer.Img:=ABitmap.CreateIntfImage;
  d:=ALayer.Img.DataDescription;
  //o caso comum (gtk2, win32, qt): 24 ou 32 bits, 8 por canal, cada canal
  //num byte inteiro. O resto vai pelo Colors[], mais lento, mas certo.
  //the usual case (gtk2, win32, qt): 24 or 32 bits, 8 per channel, each
  //channel on a whole byte. Anything else goes through Colors[], slower but
  //right.
  ALayer.Fast:=(d.Format=ricfRGBA) and (d.ByteOrder=riboLSBFirst) and
               (d.BitsPerPixel in [24,32]) and
               (d.RedPrec=8) and (d.GreenPrec=8) and (d.BluePrec=8) and
               (d.RedShift mod 8=0) and (d.GreenShift mod 8=0) and (d.BlueShift mod 8=0) and
               (d.LineOrder=riloTopToBottom);
  if ALayer.Fast then begin
    ALayer.Step:=d.BitsPerPixel div 8;
    ALayer.OfsR:=d.RedShift   div 8;
    ALayer.OfsG:=d.GreenShift div 8;
    ALayer.OfsB:=d.BlueShift  div 8;
  end;
end;

procedure LayerPixel(const ALayer: TLayerAccess; X, Y: Integer; out R, G, B: Byte); inline;
var
  p: PByte;
  c: TFPColor;
begin
  if ALayer.Fast then begin
    p:=PByte(ALayer.Img.GetDataLineStart(Y))+X*ALayer.Step;
    R:=p[ALayer.OfsR];
    G:=p[ALayer.OfsG];
    B:=p[ALayer.OfsB];
  end else begin
    c:=ALayer.Img.Colors[X,Y];
    R:=c.red   shr 8;
    G:=c.green shr 8;
    B:=c.blue  shr 8;
  end;
end;

//le a mascara: o quanto cada pixel tem de preto vira o alfa (um desenho
//suavizado da' bordas suaves). As cores vem do TLazIntfImage, que converte
//qualquer formato que o widgetset tenha usado no bitmap.
//reads the mask: how black each pixel is becomes the alpha (an antialiased
//drawing gives soft edges). The colours come from TLazIntfImage, which
//converts whatever format the widgetset used for the bitmap.
procedure THMIBasicControl.ReadMask(AWidth, AHeight: Integer);
var
  la: TLayerAccess;
  x, y, i: Integer;
  r, g, b: Byte;
begin
  SetLength(FMaskAlpha, AWidth*AHeight);
  OpenLayer(FMaskLayer, la);
  try
    i:=0;
    for y:=0 to AHeight-1 do
      for x:=0 to AWidth-1 do begin
        LayerPixel(la, x, y, r, g, b);
        FMaskAlpha[i]:=255-(r+g+b) div 3;
        inc(i);
      end;
  finally
    la.Img.Free;
  end;
  FMaskWidth:=AWidth;
  FMaskHeight:=AHeight;
end;

procedure THMIBasicControl.ComposeControlArea(AWidth, AHeight: Integer);
var
  la: TLayerAccess;
  x, y, i, dx, dy: Integer;
  t: TDrawingTransform;
  px: PControlPixel;
begin
  t:=DrawingTransform;
  if t in [dtRotateCW, dtRotateCCW] then
    ClearControlArea(AHeight, AWidth)
  else
    ClearControlArea(AWidth, AHeight);

  OpenLayer(FColorLayer, la);
  try
    i:=0;
    for y:=0 to AHeight-1 do
      for x:=0 to AWidth-1 do begin
        case t of
          dtFlipHorizontal: begin dx:=AWidth-1-x;  dy:=y;            end;
          dtFlipVertical:   begin dx:=x;           dy:=AHeight-1-y;  end;
          dtRotateCW:       begin dx:=AHeight-1-y; dy:=x;            end;
          dtRotateCCW:      begin dx:=y;           dy:=AWidth-1-x;   end;
          else              begin dx:=x;           dy:=y;            end;
        end;
        if FMaskAlpha[i]>0 then begin
          px:=ControlAreaRow(dy);
          inc(px, dx);
          LayerPixel(la, x, y, px^.red, px^.green, px^.blue);
          px^.alpha:=FMaskAlpha[i];
        end;
        inc(i);
      end;
  finally
    la.Img.Free;
  end;
end;

procedure THMIBasicControl.DrawControl;
var
  w, h: Integer;
begin
  GetDrawingSize(w, h);
  if (w<=0) or (h<=0) then begin
    ClearControlArea(Max(Width,0), Max(Height,0));
    ControlAreaChanged;
    exit;
  end;

  //a mascara primeiro. So' o Paint, quando mudou apenas a cor
  //(InvalidateDraw), reaproveita a que ja' tem; quem chama o DrawControl
  //direto sempre tem a forma refeita.
  //the mask first. Only Paint, when just the colour changed
  //(InvalidateDraw), reuses the one it has; whoever calls DrawControl
  //directly always gets the shape rebuilt.
  if (not FColorsOnly) or (not FMaskValid) or (FMaskWidth<>w) or (FMaskHeight<>h) then begin
    PrepareLayer(FMaskLayer, w, h);
    FDrawingMask:=true;
    try
      PaintControl(FMaskLayer.Canvas, w, h);
    finally
      FDrawingMask:=false;
    end;
    ReadMask(w, h);
    FMaskValid:=true;
  end;

  PrepareLayer(FColorLayer, w, h);
  PaintControl(FColorLayer.Canvas, w, h);

  ComposeControlArea(w, h);
  ControlAreaChanged;
end;

procedure THMIBasicControl.BuildShapeOnly;
var
  w, h, x, y, i: Integer;
  px: PControlPixel;
begin
  w:=Width;
  h:=Height;
  if (w<=0) or (h<=0) then begin
    ClearControlArea(Max(w,0), Max(h,0));
    ControlAreaChanged;
    exit;
  end;

  PrepareLayer(FMaskLayer, w, h);
  FDrawingMask:=true;
  try
    PaintControl(FMaskLayer.Canvas, w, h);
  finally
    FDrawingMask:=false;
  end;
  ReadMask(w, h);
  FMaskValid:=true;

  ClearControlArea(w, h);
  i:=0;
  for y:=0 to h-1 do begin
    px:=ControlAreaRow(y);
    for x:=0 to w-1 do begin
      px^.alpha:=FMaskAlpha[i];
      inc(px);
      inc(i);
    end;
  end;
  ControlAreaChanged;
end;

procedure THMIBasicControl.RebuildShape;
begin
  if PaintsDirectly then
    BuildShapeOnly
  else
    DrawControl;
end;

function THMIBasicControl.PaintsDirectly: Boolean;
begin
  Result:=DrawingTransform=dtNone;
end;

procedure THMIBasicControl.PaintControl(ACanvas: TCanvas; AWidth,
  AHeight: Integer);
begin
  //nada: a base e' transparente / nothing: the base is transparent
end;

procedure THMIBasicControl.GetDrawingSize(out AWidth, AHeight: Integer);
begin
  if DrawingTransform in [dtRotateCW, dtRotateCCW] then begin
    AWidth :=Height;
    AHeight:=Width;
  end else begin
    AWidth :=Width;
    AHeight:=Height;
  end;
end;

function THMIBasicControl.DrawingTransform: TDrawingTransform;
begin
  Result:=dtNone;
end;

function THMIBasicControl.ShapeColor(AColor: TColor): TColor;
begin
  if FDrawingMask then
    Result:=clBlack
  else
    Result:=AColor;
end;

procedure THMIBasicControl.SetupCanvas(ACanvas: TCanvas; APenColor: TColor;
  APenWidth: Integer; ABrushColor: TColor);
begin
  if APenWidth<=0 then
    ACanvas.Pen.Style:=psClear
  else begin
    ACanvas.Pen.Style:=psSolid;
    ACanvas.Pen.Width:=APenWidth;
  end;
  ACanvas.Pen.Color:=ShapeColor(APenColor);
  ACanvas.Brush.Style:=bsSolid;
  ACanvas.Brush.Color:=ShapeColor(ABrushColor);
end;

function THMIBasicControl.MeasureText(const AText: String): TSize;
begin
  if FColorLayer=nil then
    PrepareLayer(FColorLayer, 1, 1);
  Result:=FColorLayer.Canvas.TextExtent(AText);
end;

class function THMIBasicControl.RoundPoints(const APoints: array of TPointF
  ): TPointArray;
var
  i: Integer;
begin
  Result:=nil;
  SetLength(Result, Length(APoints));
  for i:=0 to High(APoints) do
    Result[i]:=Point(Round(APoints[i].x), Round(APoints[i].y));
end;

function THMIBasicControl.ControlAreaEmpty: Boolean;
begin
  Result:=(FControlArea.Width<=0) or (FControlArea.Height<=0);
end;

function THMIBasicControl.ControlAreaRow(Y: Integer): PControlPixel;
begin
  Result:=PControlPixel(FControlArea.GetDataLineStart(Y));
end;

procedure THMIBasicControl.ClearControlArea(AWidth, AHeight: Integer);
var
  desc: TRawImageDescription;
begin
  if (FControlArea.Width<>AWidth) or (FControlArea.Height<>AHeight) then begin
    desc.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
    FControlArea.DataDescription:=desc;
  end;
  if (AWidth>0) and (AHeight>0) then
    FillChar(FControlArea.PixelData^, AWidth*AHeight*SizeOf(TControlPixel), 0);
end;

procedure THMIBasicControl.ControlAreaChanged;
begin
  FPaintBitmapValid:=false;
end;

procedure THMIBasicControl.SetBorderWidth(AValue: Integer);
begin
  if FBorderWidth=AValue then Exit;
  if AValue<0 then exit;
  FBorderWidth:=AValue;
  if ComponentState*[csReading, csLoading]=[] then
    InvalidateShape;
end;

procedure THMIBasicControl.SetBodyColor(AValue: TColor);
begin
  if FBodyColor=AValue then Exit;
  FBodyColor:=AValue;

  if ComponentState*[csReading, csLoading]=[] then
    InvalidateDraw;
end;

procedure THMIBasicControl.SetSecurityCode(sc: UTF8String);
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

function THMIBasicControl.IsControlArea(X, Y:Integer): Boolean;
begin
  Result:=Assigned(FControlArea) and
          (x>=0) and
          (x<FControlArea.Width) and
          (y>=0) and
          (y<FControlArea.Height) and
          ControlArea(ControlAreaRow(Y)[X]);
end;

procedure THMIBasicControl.ForwardMouseMessage(var Message: TLMMouse);
var
  oldEnableState: Boolean;
  ClientPoint, GlobalPoint:TPoint;
  aControl: TControl;
begin
  oldEnableState:=Enabled;
  Enabled:=false;
  try
    ClientPoint.x:=Message.XPos;
    ClientPoint.y:=Message.YPos;
    GlobalPoint:=ClientToScreen(ClientPoint);
    ClientPoint:=ClientToParent(ClientPoint);

    aControl:= Parent.ControlAtPos(ClientPoint, false, true);
    repeat
      if Assigned(aControl) and (aControl is TWinControl) and (TWinControl(aControl).ControlCount>0) then begin
        aControl:=TWinControl(aControl).ControlAtPos(aControl.ScreenToClient(GlobalPoint),false,true);
      end;
    until (Assigned(aControl)=false) or (not (aControl is TWinControl)) or (TWinControl(aControl).ControlCount=0);

    if Assigned(aControl) then begin
      ClientPoint:= aControl.ScreenToClient(GlobalPoint);
      Message.XPos:=ClientPoint.x;
      Message.YPos:=ClientPoint.y;
      aControl.Dispatch(Message)
    end else
      if Assigned(Parent) then begin
        ClientPoint:= Parent.ScreenToClient(GlobalPoint);
        Message.XPos:=ClientPoint.x;
        Message.YPos:=ClientPoint.y;
        Parent.Dispatch(Message);
      end;
  finally
    Enabled:=oldEnableState;
  end;
end;

procedure THMIBasicControl.DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
  Shift: TShiftState);
var
  MP: TPoint;
begin
  if not (csNoStdEvents in ControlStyle) then
  begin
    MP := GetMousePosFromMessage(Message.Pos);
    MouseDown(Button, KeysToShiftState(Message.Keys) + Shift, MP.X, MP.Y);
  end;
end;

procedure THMIBasicControl.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
var
  P, MP: TPoint;
begin
  if not (csNoStdEvents in ControlStyle) then
  begin
    MP := GetMousePosFromMessage(Message.Pos);
    if (Button in [mbLeft, mbRight]) and DragManager.IsDragging then
    begin
      P := ClientToScreen(MP);
      TDragManagerAccess(DragManager).MouseUp(Button, KeysToShiftState(Message.Keys), P.X, P.Y);
      Message.Result := 1;
    end;
    MouseUp(Button, KeysToShiftState(Message.Keys), MP.X, MP.Y);
  end;
end;

procedure THMIBasicControl.WMLButtonDown(var Message: TLMLButtonDown);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := True;

    if csClickEvents in ControlStyle then
      Include(FControlState, csClicked);
    DoMouseDown(Message, mbLeft, []);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonDown(var Message: TLMRButtonDown);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := True;
    DoMouseDown(Message, mbRight, []);
  end else
    ForwardMouseMessage(Message);
end;


procedure THMIBasicControl.WMMButtonDown(var Message: TLMMButtonDown);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin

    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
        MouseCapture := True;

    DoMouseDown(Message, mbMiddle, []);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonDown(var Message: TLMXButtonDown);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := True;


    DoMouseDown(Message, Btn, []);
  end else
    ForwardMouseMessage(Message);
end;


procedure THMIBasicControl.WMLButtonUp(var Message: TLMLButtonUp);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin

    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := False;

    if csClicked in ControlState then  begin
      Exclude(FControlState, csClicked);
      Click;
    end;

    DoMouseUp(Message, mbLeft);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonUp(var Message: TLMRButtonUp);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := False;

    //MouseUp event is independent of return values of contextmenu
    DoMouseUp(Message, mbRight);
  end else
    ForwardMouseMessage(Message);
end;

{------------------------------------------------------------------------------
  Method: THMIBasicControl.WMMButtonUp
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure THMIBasicControl.WMMButtonUp(var Message: TLMMButtonUp);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
      MouseCapture := False;

    DoMouseUp(Message, mbMiddle);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonUp(var Message: TLMXButtonUp);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := False;

    DoMouseUp(Message, Btn);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMLButtonDBLCLK(var Message: TLMLButtonDblClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    //TODO: SendCancelMode(self);
    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := True;

    // first send a mouse down
    DoMouseDown(Message, mbLeft ,[ssDouble]);
    // then send the double click
    if csClickEvents in ControlStyle then
      DblClick;
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonDBLCLK(var Message: TLMRButtonDblClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := True;
    DoMouseDown(Message, mbRight ,[ssDouble]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMMButtonDBLCLK(var Message: TLMMButtonDblClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbMiddle ,[ssDouble]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonDBLCLK(var Message: TLMXButtonDblClk);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, Btn, [ssDouble]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMLButtonTripleCLK(var Message: TLMLButtonTripleClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := True;

    if csClickEvents in ControlStyle then TripleClick;
    DoMouseDown(Message, mbLeft ,[ssTriple]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonTripleCLK(var Message: TLMRButtonTripleClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbRight ,[ssTriple]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMMButtonTripleCLK(var Message: TLMMButtonTripleClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbMiddle ,[ssTriple]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonTripleCLK(var Message: TLMXButtonTripleClk);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, Btn, [ssTriple]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMLButtonQuadCLK(var Message: TLMLButtonQuadClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
      MouseCapture := True;

    if csClickEvents in ControlStyle then QuadClick;
    DoMouseDown(Message, mbLeft ,[ssQuad]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMRButtonQuadCLK(var Message: TLMRButtonQuadClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbRight ,[ssQuad]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMMButtonQuadCLK(var Message: TLMMButtonQuadClk);
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, mbMiddle ,[ssQuad]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMXButtonQuadCLK(var Message: TLMXButtonQuadClk);
var
  Btn: TMouseButton;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
    else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
    else Exit;

    if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
      MouseCapture := True;

    DoMouseDown(Message, Btn, [ssQuad]);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.WMMouseMove(var Message: TLMMouseMove);
var
  MP: TPoint;
begin
  if IsControlArea(Message.XPos, Message.YPos) then begin
    MP := GetMousePosFromMessage(Message.Pos);
    UpdateMouseCursor(MP.X,MP.Y);
    if not (csNoStdEvents in ControlStyle) then
      MouseMove(KeystoShiftState(Word(Message.Keys)), MP.X, MP.Y);
  end else
    ForwardMouseMessage(Message);
end;

procedure THMIBasicControl.SetHMITag(t: TPLCTag);
begin

end;

function THMIBasicControl.GetHMITag: TPLCTag;
begin
  Result:=nil;
end;

function THMIBasicControl.GetControlSecurityCode: UTF8String;
begin
  Result:=FSecurityCode;
end;

procedure THMIBasicControl.CanBeAccessed(a: Boolean);
begin
  FIsEnabledBySecurity :=a;
  SetEnabled(FIsEnabled);
end;

procedure THMIBasicControl.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMIBasicControl.SetEnabled(e: Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

function THMIBasicControl.Cateto(p0,p1:Integer):Integer;
begin
  if p1>=p0 then
    Result:=p1-p0
  else
    Result:=p0-p1;
end;

function THMIBasicControl.Hipotenusa(x0, x1, y0, y1:Integer):Double;
begin
  Result:=sqrt(Power(Cateto(x0,x1),2)+
               Power(Cateto(y0,y1),2));
end;

function THMIBasicControl.Seno(x0,x1,y0,y1:Integer):Double;
var
  hip: Double;
begin
  hip:=Hipotenusa(x0,x1,y0,y1);
  if hip=0 then
    Result:=0
  else
    Result:=Cateto(y0,y1)/hip;
end;

function THMIBasicControl.Degrees(x0,x1,y0,y1:Integer):Double;
var
  CatetoAdj: Integer;
  CatetoOposto: Integer;
  SomaCatetos: Integer;
begin
  CatetoAdj:=Cateto(x0,x1);
  CatetoOposto:=Cateto(y0,y1);
  SomaCatetos:=CatetoAdj+CatetoOposto;
  if SomaCatetos=0 then
    Result:=0
  else
    Result:=CatetoOposto/SomaCatetos*90;
end;

function THMIBasicControl.ControlArea(const pixel: TControlPixel): Boolean;
begin
  Result:=pixel.alpha>127;
end;

procedure THMIBasicControl.UpdateShape;
var
  p:PControlPixel;
  x, y:Integer;
  arect:TRect;

  {$IF defined(RGN_CONTINUOUS_ROW_AS_RECTANGLE) OR defined(ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE)}
  started:boolean;
  x0, x1:Integer;
  {$IFDEF ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE}
  fbmp: TBitmap;
  pb:PByte;

  procedure Draw1BitBMPLine(abmp:TBitmap; FromX, ToX, Y:Integer);
  var
    sl: PByte;
    aBit: Word;
    aX: Integer;
  begin
    sl:=abmp.ScanLine[y];
    inc(sl,FromX div 8);
    {$IFNDEF LCLGtk2}
    aBit:=128;
    aBit:=aBit shr (FromX mod 8);
    {$ELSE}
    abit:=1;
    aBit:=aBit shl (FromX mod 8);
    {$ENDIF}

    for aX:=FromX to ToX-1 do begin
      sl^:=sl^+aBit;

      {$IFNDEF LCLGtk2}
      aBit:=aBit shr 1;
      {$ELSE}
      aBit:=aBit shl 1;
      {$ENDIF}

      {$IFNDEF LCLGtk2}
      if aBit=0 then begin
        aBit:=128;
        inc(sl);
      end;
      {$ELSE}
      if aBit=256 then begin
        aBit:=1;
        inc(sl);
      end;
      {$ENDIF}
    end;

  end;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF RGN_DETECT_RECTANGLES}
  xa, x1:Integer;
  y1:Integer;
  invalidline:boolean;
  {$ENDIF}

  {$IF (not defined(RGN_PIXEL_BY_PIXEL)) AND (not defined(RGN_CONTINUOUS_ROW_AS_RECTANGLE)) AND (not defined(RGN_DETECT_RECTANGLES)) AND (not defined(ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE))}
  pb:PByte;
  bit:PtrInt;
  fbmp: TBitmap;
  {$ELSE}
  {$IFNDEF ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE}
  frgn: TRegion;
  {$ENDIF}
  {$ENDIF}
begin
  {$IF defined(LCLqt) or defined(LCLQt5)}
  Color:=clBackground;
  exit;
  {$IFEND}
  if Parent=nil then exit;

  {$IFDEF RGN_PIXEL_BY_PIXEL}
  frgn:=TRegion.Create;
  for y:=0 to FControlArea.Height-1 do begin
    p:=ControlAreaRow(y);
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
    p:=ControlAreaRow(y);
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
        if ControlArea(ControlAreaRow(y)[x]) and (PtInRegion(frgn.Handle, x, y)=false) then begin
          for x1:=x to FControlArea.Width-2 do begin
            if ControlArea(ControlAreaRow(y)[x1+1])=false then break;
            if PtInRegion(frgn.Handle, x1+1, y) then break;
          end;

          invalidline:=false;
          for y1:=y to FControlArea.Height-2 do begin
            for xa:=x to x1 do begin
              if ControlArea(ControlAreaRow(y1+1)[xa])=false then begin
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


  {$IFDEF ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE}
  fbmp:=TBitmap.Create;
  fbmp.Monochrome :=true;
  fbmp.PixelFormat:=pf1bit;
  fbmp.Width:=FControlArea.Width;
  fbmp.Height:=FControlArea.Height;
  fbmp.Canvas.Pen.Color:=clBlack;

  for y:=0 to FControlArea.Height-1 do begin
    p:=ControlAreaRow(y);
    started:=false;
    for x:=0 to FControlArea.Width-1 do begin
      if ControlArea(p^) then begin
        if started then begin
          x1:=x+1;
        end else begin
          x0:=x;
          x1:=x+1;
          started:=true;
        end;
      end else begin
        if started then begin
          if x0=x1 then
            Draw1BitBMPLine(fbmp, x0, x0+1,y)
          else
            Draw1BitBMPLine(fbmp, x0, x1,  y);
        end;
        started:=false;
      end;
      inc(p);
    end;

    //the
    if started then begin
      if x0=x1 then
        Draw1BitBMPLine(fbmp, x0, x0+1,y)
      else
        Draw1BitBMPLine(fbmp, x0, x1,  y);
    end;
  end;
  SetShape(fbmp);
  FreeAndNil(fbmp);
  {$ENDIF}

  {$IF (not defined(RGN_PIXEL_BY_PIXEL)) AND (not defined(RGN_CONTINUOUS_ROW_AS_RECTANGLE)) AND (not defined(RGN_DETECT_RECTANGLES)) AND (not defined(ONE_BIT_BMP_CONTINUOUS_ROW_AS_RECTANGLE))}
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
      if ControlArea(ControlAreaRow(y)[x]) then
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
  if assigned(FControlArea) and PaintsDirectly then begin
    //so' a forma precisa da mascara lida de volta; a cor vai direto na tela
    //only the shape needs the mask read back; the colour goes straight to the
    //screen
    if ControlAreaEmpty or FUpdateShape or (not FMaskValid) or
       (FMaskWidth<>Width) or (FMaskHeight<>Height) then begin
      BuildShapeOnly;
      {$IF defined(LCLqt) or defined(LCLQt5)}
      Color:=clBackground;
      {$ELSE}
      UpdateShape;
      {$IFEND}
      FUpdateShape:=false;
    end;
    FShouldRedraw:=false;
    if Visible and not ControlAreaEmpty then begin
      Canvas.Font.Assign(Font);
      PaintControl(Canvas, Width, Height);
    end;
  end else if assigned(FControlArea) then begin
    if ControlAreaEmpty Or FUpdateShape then begin
      DrawControl;
      {$IF defined(LCLqt) or defined(LCLQt5)}
      Color:=clBackground;
      {$ELSE}
      UpdateShape;
      {$IFEND}
      FShouldRedraw:=false;
      FUpdateShape:=false;
    end;

    if FShouldRedraw then begin
      FColorsOnly:=true;
      try
        DrawControl;
      finally
        FColorsOnly:=false;
      end;
      FShouldRedraw:=false;
    end;

    if Visible and not ControlAreaEmpty then begin
      if not FPaintBitmapValid then begin
        FPaintBitmap.LoadFromIntfImage(FControlArea);
        FPaintBitmapValid:=true;
      end;
      Canvas.Draw(0, 0, FPaintBitmap);
    end;
  end;
  inherited Paint;
end;

procedure THMIBasicControl.Resize;
begin
  try
    inherited Resize;
  finally
    if (Width<>FOldWidth) or (FOldHeight<>Height) then
      InvalidateShape;
    FOldWidth:=Width;
    FOldHeight:=Height;
  end;
end;

procedure THMIBasicControl.Invalidate;
begin
  //GTK2 bug: if control is empty and setshape was applied,
  //Invalidate will not call Paint, because all control area
  //is "transparent", so Paint don't have anything to do...
  {$IF defined(LCLGtk2)} //maybe more need this trick...
  //sem janela nao ha' Paint para destravar / with no window there is no Paint to unblock
  if HandleAllocated and ([csDestroying]*ComponentState=[]) and Assigned(FControlArea) and ControlAreaEmpty then begin
    RebuildShape;
    UpdateShape;
    //a forma acabou de ser refeita: o Paint que vem nao precisa refaze-la
    //the shape was just rebuilt: the coming Paint need not rebuild it
    FUpdateShape:=false;
  end;
  {$IFEND}
  inherited Invalidate;
end;

procedure THMIBasicControl.CMHitTest(var Message: TCMHittest);
begin
  if IsControlArea(Message.XPos, Message.YPos) then
    Message.Result:=1
  else
    Message.Result:=0;
end;

procedure THMIBasicControl.CMDesigerHitTest(var Message: TCMHittest);
var
  p: TPoint;
begin
  //sem writeln aqui: a IDE manda esta mensagem a cada movimento do mouse no
  //designer e, aberta sem terminal, nao tem saida padrao - o writeln dava
  //EInOutError "Disk Full" (erro 101 de E/S)
  //no writeln here: the IDE sends this message on every mouse move in the
  //designer and, started with no terminal, has no standard output - the
  //writeln raised EInOutError "Disk Full" (I/O error 101)
  p:=ParentToClient(Point(Message.XPos, Message.YPos),GetDesignerForm(Self));

  if IsControlArea(p.X,p.Y) then
    Message.Result:=0
  else
    Message.Result:=1;
end;

//procedure THMIBasicControl.TransparentHitTest(var Message: TCMHitTest);
//begin
//  //if (not Assigned(FControlArea)) or (not ControlArea(FControlArea.ScanAt(Message.Pos.X,Message.Pos.Y))) then
//  //  Message.Result:=HTTRANSPARENT
//  //else
//    Message.Result:=0;
//end;

procedure THMIBasicControl.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if (NewParent<>nil) and (ComponentState*[csReading, csLoading]=[]) then begin
    //so' a forma: quem pinta direto nao precisa da imagem colorida montada
    //only the shape: whoever paints directly does not need the coloured image
    RebuildShape;
    UpdateShape;
    //acabou de ser refeita: o primeiro Paint nao precisa refaze-la de novo
    //just rebuilt: the first Paint need not rebuild it again
    FUpdateShape:=false;
    Invalidate;
  end;
end;

procedure THMIBasicControl.SetBorderColor(AValue: TColor);
begin
  if AValue=FBorderColor then exit;
  FBorderColor:=AValue;
  if ComponentState*[csReading, csLoading]=[] then
    InvalidateDraw;
end;

procedure THMIBasicControl.Loaded;
begin
  inherited Loaded;
  CanBeAccessed(GetControlSecurityManager.CanAccess(GetControlSecurityCode));
  InvalidateShape;
end;

constructor THMIBasicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegInSecMan:=GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
  if not FRegInSecMan then begin
    {$IFNDEF WINDOWS}
    writeln('FIX-ME: Failed to register class ',ClassName,' instace with name="',Name,'" in the ControlSecurityManager?',{$i %FILE%},':',{$i %LINE%});
    {$ENDIF}
  end;
  Color:=clBackground;
  FShouldRedraw:=false;
  FUpdateShape:=false;
  FBodyColor:=clSilver;
  FBorderColor:=clBlack;
  FBorderWidth:=1;
  FControlArea:=TLazIntfImage.Create(0,0);
  ClearControlArea(0,0);
  FPaintBitmap:=TBitmap.Create;
  FPaintBitmapValid:=false;
  FMaskValid:=false;
  FColorsOnly:=false;
  FDrawingMask:=false;
  FIsEnabled:=true;
  //sem codigo de seguranca ninguem barra o controle. Comecando em falso, o
  //primeiro Enabled:=true de um controle criado por codigo - ou o repasse de
  //um clique, que desabilita e reabilita - o deixava desabilitado: o E logico
  //com a seguranca dava falso. Quem carrega de um .lfm nunca viu isso porque
  //o Loaded pergunta ao gerenciador e acerta a flag.
  //with no security code nobody bars the control. Starting at false, the
  //first Enabled:=true of a control created by code - or the forwarding of a
  //click, which disables and re-enables - left it disabled: the logical AND
  //with security gave false. Whatever loads from a .lfm never saw it because
  //Loaded asks the manager and sets the flag.
  FIsEnabledBySecurity:=true;
  FOldHeight:=0;
  FOldWidth:=0;
end;

destructor THMIBasicControl.Destroy;
begin
  if FRegInSecMan then
    GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface)
  else begin
    {$IFNDEF WINDOWS}
    writeln('FIX-ME: Why class ',ClassName,', instace name="',Name,'" ins''t registered in ControlSecurityManager?',{$i %FILE%},':',{$i %LINE%});
    {$ENDIF}
  end;

  FreeAndNil(FControlArea);
  FreeAndNil(FPaintBitmap);
  FreeAndNil(FColorLayer);
  FreeAndNil(FMaskLayer);
  inherited Destroy;
end;

end.
