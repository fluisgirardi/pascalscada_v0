unit hmi_draw_basiccontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Types, Controls, Graphics, IntfGraphics, GraphType, FPImage,
  FPCanvas, LazCanvas, LCLIntf, LMessages, ControlSecurityManager, HMITypes,
  PLCTag;

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

  { THMIBasicControl }

  THMIBasicControl = class(TGraphicControl, IHMIInterface)
  private
    FRegInSecMan:Boolean;
    //o bitmap que o Paint desenha, feito da FControlArea
    //the bitmap Paint draws, made from FControlArea
    FPaintBitmap:TBitmap;
    FPaintBitmapValid:Boolean;
    //a mascara de 1 bit tirada do alfa da FControlArea (bit 1 = fundo)
    //the 1-bit mask taken from the alpha of FControlArea (bit 1 = background)
    FPaintMask:array of Byte;
    //onde se desenha quando ha' DrawingTransform, antes de girar/espelhar
    //where the drawing goes when there is a DrawingTransform, before
    //rotating/mirroring
    FWorkImage:TLazIntfImage;
    procedure UpdatePaintBitmap;
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
    //: que o Paint mostra, o que o teste de acerto consulta.
    {$ELSE}
    //: The finished drawing of the control, 32-bit RGBA (see TControlPixel):
    //: what Paint shows, what the hit test looks at.
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
    //: Monta a FControlArea: limpa a imagem (transparente), chama o
    //: PaintControl com um TLazCanvas sobre ela e aplica o DrawingTransform.
    //: Tudo em memoria (fcl-image): o que o PaintControl pinta fica opaco, o
    //: resto transparente, e e' essa mesma imagem que o Paint mostra e que o
    //: teste de clique consulta. Quem desenha de outro jeito (o
    //: controle vetorial) sobrescreve este metodo e preenche a FControlArea
    //: por conta propria, com ClearControlArea e ControlAreaChanged.
    {$ELSE}
    //: Builds FControlArea: clears the image (transparent), calls
    //: PaintControl with a TLazCanvas over it and applies DrawingTransform.
    //: All in memory (fcl-image): whatever PaintControl paints becomes
    //: opaque, the rest transparent, and this same image is what Paint shows
    //: and what the hit test looks at. Whoever draws some other
    //: way (the vector control) overrides this method and fills FControlArea
    //: on its own, with ClearControlArea and ControlAreaChanged.
    {$ENDIF}
    procedure DrawControl; virtual;
    {$IFDEF PORTUGUES}
    //: Desenha o controle no ACanvas (AWidth x AHeight, antes do
    //: DrawingTransform). As cores vao pelo FPColor (veja SetupCanvas e
    //: FPColorOf). Alem das primitivas do TFPCustomCanvas, use RoundRect,
    //: RadialPie e StrokeText desta classe, que o fcl-image nao tem.
    {$ELSE}
    //: Draws the control on ACanvas (AWidth x AHeight, before
    //: DrawingTransform). Colours go through FPColor (see SetupCanvas and
    //: FPColorOf). Besides the TFPCustomCanvas primitives, use RoundRect,
    //: RadialPie and StrokeText of this class, which fcl-image lacks.
    {$ENDIF}
    procedure PaintControl(ACanvas:TFPCustomCanvas; AWidth, AHeight:Integer); virtual;
    //: @seealso(PaintControl)
    procedure GetDrawingSize(out AWidth, AHeight:Integer); virtual;
    //: @seealso(TDrawingTransform)
    function  DrawingTransform:TDrawingTransform; virtual;
    //: Uma TColor (inclusive as do sistema) como TFPColor opaca.
    //: A TColor (system ones included) as an opaque TFPColor.
    class function FPColorOf(AColor:TColor):TFPColor;
    {$IFDEF PORTUGUES}
    //: Caneta e pincel de uma vez. APenWidth<=0: sem borda.
    {$ELSE}
    //: Pen and brush at once. APenWidth<=0: no border.
    {$ENDIF}
    procedure SetupCanvas(ACanvas:TFPCustomCanvas; APenColor:TColor; APenWidth:Integer; ABrushColor:TColor);
    {$IFDEF PORTUGUES}
    //: Retangulo de cantos arredondados, como o TCanvas.RoundRect (RX e RY sao
    //: os diametros da elipse do canto), feito como poligono.
    {$ELSE}
    //: Rounded rectangle, as TCanvas.RoundRect (RX and RY are the diameters of
    //: the corner's ellipse), made as a polygon.
    {$ENDIF}
    procedure RoundRect(ACanvas:TFPCustomCanvas; X1, Y1, X2, Y2, RX, RY:Integer);
    {$IFDEF PORTUGUES}
    //: Fatia de elipse, como o TCanvas.RadialPie (angulos em 1/16 de grau, no
    //: sentido anti-horario a partir das 3 horas), feita como poligono.
    {$ELSE}
    //: Ellipse slice, as TCanvas.RadialPie (angles in 1/16 degree,
    //: counter-clockwise from 3 o'clock), made as a polygon.
    {$ENDIF}
    procedure RadialPie(ACanvas:TFPCustomCanvas; X1, Y1, X2, Y2, StartAngle16Deg, Angle16DegLength:Integer);
    {$IFDEF PORTUGUES}
    //: Texto desenhado com polylines (uma fonte de tracos simples, sem
    //: depender de fonte do sistema), com a caneta atual. (X,Y) e' o canto
    //: superior esquerdo; ASize, a altura das maiusculas em pixels;
    //: AOrientation, 0 ou 900 (de baixo para cima, como o Font.Orientation).
    //: Cobre digitos, '-', '=', ',', '.', ':', espaco e as letras D, M, e, g,
    //: r, s, x e y; o resto sai como um retangulo.
    {$ELSE}
    //: Text drawn with polylines (a simple stroke font, with no system font
    //: involved), with the current pen. (X,Y) is the top left corner; ASize,
    //: the height of the capitals in pixels; AOrientation, 0 or 900 (bottom to
    //: top, as Font.Orientation). Covers digits, '-', '=', ',', '.', ':',
    //: space and the letters D, M, e, g, r, s, x and y; anything else comes
    //: out as a rectangle.
    {$ENDIF}
    procedure StrokeText(ACanvas:TFPCustomCanvas; X, Y, ASize:Integer; const AText:String; AOrientation:Integer = 0);
    //: Largura e altura (com as descendentes) de um StrokeText deitado.
    //: Width and height (descenders included) of a StrokeText lying down.
    class function StrokeTextExtent(const AText:String; ASize:Integer):TSize;
    //: Pontos com coordenadas reais arredondados.
    //: Points with real coordinates rounded.
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
  //a area antiga fica ate' o Paint refazer: esvazia-la fazia o Invalidate do
  //gtk2 reconstruir a forma na hora, a cada chamada - um SetBounds chama
  //varias - em vez de uma vez so', no Paint
  //the old area stays until Paint rebuilds it: emptying it made the gtk2
  //Invalidate rebuild the shape right away, on every call - a SetBounds
  //makes several - instead of once, in Paint
  Invalidate;
end;

procedure ClearImage(AImage: TLazIntfImage; AWidth, AHeight: Integer);
var
  desc: TRawImageDescription;
begin
  if (AImage.Width<>AWidth) or (AImage.Height<>AHeight) then begin
    desc.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
    AImage.DataDescription:=desc;
  end;
  if (AWidth>0) and (AHeight>0) then
    FillChar(AImage.PixelData^, AWidth*AHeight*SizeOf(TControlPixel), 0);
end;

procedure THMIBasicControl.DrawControl;
var
  w, h, x, y, dx, dy: Integer;
  t: TDrawingTransform;
  target: TLazIntfImage;
  cv: TLazCanvas;
  src, dst: PControlPixel;
begin
  GetDrawingSize(w, h);
  if (w<=0) or (h<=0) then begin
    ClearControlArea(Max(Width,0), Max(Height,0));
    ControlAreaChanged;
    exit;
  end;

  t:=DrawingTransform;
  if t=dtNone then begin
    ClearControlArea(w, h);
    target:=FControlArea;
  end else begin
    if FWorkImage=nil then
      FWorkImage:=TLazIntfImage.Create(0,0);
    ClearImage(FWorkImage, w, h);
    target:=FWorkImage;
  end;

  cv:=TLazCanvas.Create(target);
  try
    PaintControl(cv, w, h);
  finally
    cv.Free;
  end;

  if t<>dtNone then begin
    if t in [dtRotateCW, dtRotateCCW] then
      ClearControlArea(h, w)
    else
      ClearControlArea(w, h);
    for y:=0 to h-1 do begin
      src:=PControlPixel(FWorkImage.GetDataLineStart(y));
      for x:=0 to w-1 do begin
        case t of
          dtFlipHorizontal: begin dx:=w-1-x; dy:=y;     end;
          dtFlipVertical:   begin dx:=x;     dy:=h-1-y; end;
          dtRotateCW:       begin dx:=h-1-y; dy:=x;     end;
          else              begin dx:=y;     dy:=w-1-x; end; //dtRotateCCW
        end;
        dst:=ControlAreaRow(dy);
        inc(dst, dx);
        dst^:=src^;
        inc(src);
      end;
    end;
  end;
  ControlAreaChanged;
end;

procedure THMIBasicControl.PaintControl(ACanvas: TFPCustomCanvas; AWidth,
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

class function THMIBasicControl.FPColorOf(AColor: TColor): TFPColor;
begin
  Result:=TColorToFPColor(ColorToRGB(AColor));
  Result.Alpha:=alphaOpaque;
end;

procedure THMIBasicControl.SetupCanvas(ACanvas: TFPCustomCanvas;
  APenColor: TColor; APenWidth: Integer; ABrushColor: TColor);
begin
  if APenWidth<=0 then
    ACanvas.Pen.Style:=psClear
  else begin
    ACanvas.Pen.Style:=psSolid;
    ACanvas.Pen.Width:=APenWidth;
  end;
  ACanvas.Pen.FPColor:=FPColorOf(APenColor);
  ACanvas.Brush.Style:=bsSolid;
  ACanvas.Brush.FPColor:=FPColorOf(ABrushColor);
end;

procedure THMIBasicControl.RoundRect(ACanvas: TFPCustomCanvas; X1, Y1, X2,
  Y2, RX, RY: Integer);
var
  pts: array of TPoint;
  a, b, l, r, tp, bt, cx, cy: Double;
  n, i, k, c: Integer;
  ang: Double;
const
  //centro de cada canto e o angulo onde o arco comeca
  //centre of each corner and the angle where the arc starts
  StartAng: array[0..3] of Double = (0, 90, 180, 270);
begin
  if (X2<=X1) or (Y2<=Y1) then exit;
  //como no TCanvas, X2 e Y2 ficam fora / as on TCanvas, X2 and Y2 stay out
  l:=X1; tp:=Y1; r:=X2-1; bt:=Y2-1;
  a:=Min(Abs(RX)/2, (r-l)/2);
  b:=Min(Abs(RY)/2, (bt-tp)/2);
  if (a<1) or (b<1) then begin
    ACanvas.Rectangle(X1, Y1, X2, Y2);
    exit;
  end;
  n:=Max(2, Min(16, Round(Max(a,b)/2)));
  pts:=nil;
  SetLength(pts, 4*(n+1));
  k:=0;
  for c:=0 to 3 do begin
    case c of
      0: begin cx:=r-a; cy:=tp+b; end;  //superior direito / top right
      1: begin cx:=l+a; cy:=tp+b; end;  //superior esquerdo / top left
      2: begin cx:=l+a; cy:=bt-b; end;  //inferior esquerdo / bottom left
      else begin cx:=r-a; cy:=bt-b; end; //inferior direito / bottom right
    end;
    for i:=0 to n do begin
      ang:=(StartAng[c]+90*i/n)*Pi/180;
      pts[k]:=Point(Round(cx+a*Cos(ang)), Round(cy-b*Sin(ang)));
      inc(k);
    end;
  end;
  ACanvas.Polygon(pts);
end;

procedure THMIBasicControl.RadialPie(ACanvas: TFPCustomCanvas; X1, Y1, X2,
  Y2, StartAngle16Deg, Angle16DegLength: Integer);
var
  pts: array of TPoint;
  cx, cy, a, b, ang, a0, len: Double;
  n, i: Integer;
begin
  cx:=(X1+X2)/2;
  cy:=(Y1+Y2)/2;
  a:=Abs(X2-X1)/2;
  b:=Abs(Y2-Y1)/2;
  a0 :=StartAngle16Deg/16;
  len:=Angle16DegLength/16;
  n:=Max(4, Min(64, Round(Abs(len)/360*Pi*(a+b)/3)));
  pts:=nil;
  SetLength(pts, n+2);
  pts[0]:=Point(Round(cx), Round(cy));
  for i:=0 to n do begin
    ang:=(a0+len*i/n)*Pi/180;
    pts[i+1]:=Point(Round(cx+a*Cos(ang)), Round(cy-b*Sin(ang)));
  end;
  ACanvas.Polygon(pts);
end;

//a fonte de tracos: cada caractere sao polylines numa grade de 5 colunas
//(0..4) por 9 linhas (0 = topo das maiusculas, 6 = linha de base, ate' 8
//para as descendentes); cada ponto sao dois digitos "xy" e as polylines
//sao separadas por espaco
//the stroke font: each character is polylines on a grid of 5 columns
//(0..4) by 9 rows (0 = top of the capitals, 6 = baseline, down to 8 for
//descenders); each point is two digits "xy" and the polylines are
//separated by a space
function StrokeGlyph(ch: Char): String;
begin
  case ch of
    '0': Result:='0040460600 0640';
    '1': Result:='1120 2026 1636';
    '2': Result:='004043030646';
    '3': Result:='00404606 1343';
    '4': Result:='000343 4046';
    '5': Result:='400003434606';
    '6': Result:='400006464303';
    '7': Result:='004016';
    '8': Result:='0040460600 0343';
    '9': Result:='430300404606';
    '-': Result:='0343';
    '=': Result:='0242 0444';
    ',': Result:='252617';
    '.': Result:='2526';
    ':': Result:='2122 2526';
    ' ': Result:='';
    'D': Result:='00304145360600';
    'M': Result:='0600234046';
    'e': Result:='044443321203051646';
    'g': Result:='421203051646 42473808';
    'r': Result:='0206 042242';
    's': Result:='420204444606';
    'x': Result:='0246 4206';
    'y': Result:='0225 4218';
  else
    Result:='0040460600';
  end;
end;

class function THMIBasicControl.StrokeTextExtent(const AText: String;
  ASize: Integer): TSize;
var
  u: Double;
begin
  u:=Max(ASize,1)/6;
  if AText='' then
    Result.cx:=0
  else
    Result.cx:=Round((Length(AText)*6-2)*u);
  Result.cy:=Round(8*u)+1;
end;

procedure THMIBasicControl.StrokeText(ACanvas: TFPCustomCanvas; X, Y,
  ASize: Integer; const AText: String; AOrientation: Integer);
var
  u: Double;
  ci, i, n: Integer;
  g: String;
  pts: array of TPoint;
  gx, gy: Double;
  oldStyle: TFPPenStyle;
  oldWidth: Integer;

  function Place(px, py: Double): TPoint;
  begin
    //px ao longo da linha de base, py para baixo
    //px along the baseline, py downwards
    if AOrientation=900 then
      Result:=Point(X+Round(py), Y-Round(px))
    else
      Result:=Point(X+Round(px), Y+Round(py));
  end;

begin
  if AText='' then exit;
  u:=Max(ASize,1)/6;
  oldStyle:=ACanvas.Pen.Style;
  oldWidth:=ACanvas.Pen.Width;
  ACanvas.Pen.Style:=psSolid;
  ACanvas.Pen.Width:=Max(1, Round(ASize/8));
  pts:=nil;
  try
    for ci:=1 to Length(AText) do begin
      g:=StrokeGlyph(AText[ci])+' ';
      n:=0;
      i:=1;
      while i<=Length(g) do begin
        if g[i]=' ' then begin
          if n>=2 then
            ACanvas.Polyline(Copy(pts, 0, n));
          n:=0;
          inc(i);
        end else begin
          gx:=(ci-1)*6+(Ord(g[i])-Ord('0'));
          gy:=Ord(g[i+1])-Ord('0');
          if n>=Length(pts) then
            SetLength(pts, n+8);
          pts[n]:=Place(gx*u, gy*u);
          inc(n);
          inc(i, 2);
        end;
      end;
    end;
  finally
    ACanvas.Pen.Style:=oldStyle;
    ACanvas.Pen.Width:=oldWidth;
  end;
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
begin
  //sem janela nao ha' forma nativa: quem recorta e' a mascara do Paint e quem
  //deixa o clique passar e' o CM_HITTEST. Fica como gancho para quem precisa
  //saber que a forma mudou.
  //with no window there is no native shape: the Paint mask clips and
  //CM_HITTEST lets the click through. Kept as a hook for whoever needs to
  //know the shape changed.
end;

procedure THMIBasicControl.Paint;
begin
  if assigned(FControlArea) then begin
    if ControlAreaEmpty Or FUpdateShape then begin
      DrawControl;
      UpdateShape;
      FShouldRedraw:=false;
      FUpdateShape:=false;
    end;

    //so' a cor mudou: o desenho em memoria e' barato
    //only the colour changed: drawing in memory is cheap
    if FShouldRedraw then begin
      DrawControl;
      FShouldRedraw:=false;
    end;

    if Visible and not ControlAreaEmpty then begin
      if not FPaintBitmapValid then
        UpdatePaintBitmap;
      Canvas.Draw(0, 0, FPaintBitmap);
    end;
  end;
  inherited Paint;
end;

type
  TBitmapAccess = class(TBitmap);

procedure THMIBasicControl.UpdatePaintBitmap;
var
  raw: TRawImage;
  bmp, msk: HBITMAP;
  w, h, x, y, mbpl: Integer;
  p: PControlPixel;
  m: PByte;
  b: Byte;
  maskChanged: Boolean;
begin
  //sem copia: o TRawImage aponta para a propria memoria da FControlArea,
  //vista como 32 bits sem alfa (BGRX - sobe sem mistura pixel a pixel), e o
  //alfa vira uma mascara de 1 bit, que recorta o desenho sobre o pai
  //no copy: the TRawImage points at the very memory of FControlArea, seen as
  //32 bits with no alpha (BGRX - uploaded with no per-pixel blending), and
  //the alpha becomes a 1-bit mask, which clips the drawing over the parent
  w:=FControlArea.Width;
  h:=FControlArea.Height;
  raw.Init;
  raw.Description.Init_BPP32_B8G8R8_M1_BIO_TTB(w, h);
  //linhas da mascara alinhadas em byte: o gtk2 a cria com
  //gdk_bitmap_create_from_data, que le assim e ignora o MaskLineEnd
  //mask rows aligned to a byte: gtk2 creates it with
  //gdk_bitmap_create_from_data, which reads it so and ignores MaskLineEnd
  raw.Description.MaskLineEnd:=rileByteBoundary;
  mbpl:=raw.Description.MaskBytesPerLine;
  //a mascara nova e' comparada com a anterior enquanto e' montada: quando
  //so' a cor muda ela sai igual e o pixmap dela e' reaproveitado - so' o
  //bitmap de cor sobe. Compara em vez de confiar em quem pediu o redesenho
  //porque nem todo InvalidateDraw mantem a forma (o texto da polyline muda
  //os pixels opacos).
  //the new mask is compared with the previous one while it is built: when
  //only the colour changes it comes out the same and its pixmap is reused -
  //only the colour bitmap is uploaded. It compares instead of trusting
  //whoever asked for the redraw because not every InvalidateDraw keeps the
  //shape (the polyline text changes the opaque pixels).
  maskChanged:=(Length(FPaintMask)<>mbpl*h) or not FPaintBitmap.MaskHandleAllocated;
  if Length(FPaintMask)<>mbpl*h then
    SetLength(FPaintMask, mbpl*h);
  for y:=0 to h-1 do begin
    p:=ControlAreaRow(y);
    m:=@FPaintMask[y*mbpl];
    x:=0;
    while x<w do begin
      //bit 1 = mascarado (fundo); bit 0 e' o pixel mais a esquerda
      //bit 1 = masked (background); bit 0 is the leftmost pixel
      b:=0;
      repeat
        if not ControlArea(p^) then
          b:=b or (1 shl (x and 7));
        inc(p);
        inc(x);
      until (x>=w) or ((x and 7)=0);
      if m^<>b then begin
        m^:=b;
        maskChanged:=true;
      end;
      inc(m);
    end;
  end;
  raw.Data:=FControlArea.PixelData;
  raw.DataSize:=PtrUInt(w)*PtrUInt(h)*SizeOf(TControlPixel);
  raw.Mask:=@FPaintMask[0];
  raw.MaskSize:=Length(FPaintMask);
  if maskChanged then begin
    if RawImage_CreateBitmaps(raw, bmp, msk, false) then
      TBitmapAccess(FPaintBitmap).SetHandles(bmp, msk);
  end else
    //mesma mascara: o SetHandles mantem o handle dela
    //same mask: SetHandles keeps its handle
    if RawImage_CreateBitmaps(raw, bmp, msk, true) then
      TBitmapAccess(FPaintBitmap).SetHandles(bmp, FPaintBitmap.MaskHandle);
  FPaintBitmapValid:=true;
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
    //o desenho fica para o Paint; aqui so' a area, para o teste de clique
    //the drawing is left to Paint; here only the area, for the hit test
    DrawControl;
    UpdateShape;
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
  FWorkImage:=nil;
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
  FreeAndNil(FWorkImage);
  inherited Destroy;
end;

end.
