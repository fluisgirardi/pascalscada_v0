{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit que implementa o selo de aviso de falha de comunicação exibido
  sobre controles vinculados a tags com problema de leitura ou escrita.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements the communication-fault warning badge shown
  over controls linked to tags with a read or write problem.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit hmi_commfaultbadge;

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, Tag, PLCTag, BGRABitmap,
  BGRABitmapTypes, base64, Math, Forms, ExtCtrls,
  {$IFDEF FPC}LCLIntf, LCLType{$ELSE}Windows{$ENDIF};

type
  {$IFDEF PORTUGUES}
  {:
  Base comum de "contador de fontes de falha" - THMICommBadgeController (selo
  em janela separada, para controles de terceiros/nativos sem Paint próprio)
  e THMIInlineFaultIndicator (sem janela, para controles que desenham o ícone
  no próprio Paint) implementam essa mesma interface, permitindo que
  THMITagFaultBadgeLink funcione com qualquer um dos dois.
  }
  {$ELSE}
  {:
  Common "fault source counter" base - THMICommBadgeController (badge in a
  separate window, for third-party/native controls with no Paint of their
  own) and THMIInlineFaultIndicator (no window, for controls that draw the
  icon in their own Paint) both implement this, so THMITagFaultBadgeLink can
  work with either one.
  }
  {$ENDIF}
  THMIFaultCounter = class(TObject)
  public
    procedure IncFault; virtual; abstract;
    procedure DecFault; virtual; abstract;
  end;

  {$IFDEF PORTUGUES}
  {:
  Onde o THMIWarningBadge se posiciona em relacao ao controle-alvo. wbaCenter
  e' o comportamento original (quadrado centralizado sobre o alvo inteiro).
  wbaLeftEdge/wbaRightEdge encostam o selo numa faixa vertical numa lateral
  do alvo, do tamanho de WarningIconMarginWidth - usado pelo THMIEdit, que
  reserva esse mesmo espaco via gtk_entry_set_inner_border e precisa do
  icone numa janela separada porque a faixa reservada fica sujeita a
  repinturas internas do GtkEntry que nao passam pelo nosso hook de paint.
  }
  {$ELSE}
  {:
  Where THMIWarningBadge positions itself relative to the target control.
  wbaCenter is the original behavior (square centered over the whole
  target). wbaLeftEdge/wbaRightEdge flush the badge against a vertical
  strip on one side of the target, sized to WarningIconMarginWidth - used
  by THMIEdit, which reserves that same space via
  gtk_entry_set_inner_border and needs the icon in a separate window
  because that reserved strip is subject to internal GtkEntry repaints
  that don't go through our paint hook.
  }
  {$ENDIF}
  TWarningBadgeAnchor = (wbaCenter, wbaLeftEdge, wbaRightEdge);

  {$IFDEF PORTUGUES}
  {:
  Pequeno selo de aviso (ícone PNG com transparência, carregado via BGRABitmap)
  desenhado como irmão do controle-alvo (mesmo Parent), centralizado (X,Y)
  sobre seus limites. Não depende do controle-alvo ter um Paint próprio
  acessível - funciona sobre qualquer TControl, inclusive de terceiros. É um
  TCustomControl (janela própria), não um TGraphicControl, porque um controle
  sem janela nunca fica visualmente por cima de um irmão com janela nativa
  (ex.: TEdit, TGroupBox) - o BringToFront só reordena de verdade entre
  controles com janela.
  }
  {$ELSE}
  {:
  Small warning badge (a transparent PNG icon, loaded via BGRABitmap) painted
  as a sibling of the target control (same Parent), centered (X,Y) over its
  bounds. Does not depend on the target control having an accessible Paint of
  its own - works over any TControl, including third-party ones. It's a
  TCustomControl (has its own window handle), not a TGraphicControl, because
  a handle-less control can never visually sit on top of a sibling that has a
  native window (e.g. TEdit, TGroupBox) - BringToFront only really reorders
  among windowed controls.
  }
  {$ENDIF}
  THMIWarningBadge = class(TCustomControl)
  private
    FTargetControl: TControl;
    FAnchor: TWarningBadgeAnchor;
    procedure TargetBoundsChanged(Sender: TObject);
    procedure TargetVisibleChanged(Sender: TObject);
    procedure TargetBeforeDestruction(Sender: TObject);
    procedure Reposition;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AttachTo(ATarget: TControl);
    procedure DetachTarget;
    procedure SetAnchor(AAnchor: TWarningBadgeAnchor);
    procedure ShowBadge;
    procedure HideBadge;
  end;

  {$IFDEF PORTUGUES}
  {:
  Controla o selo de aviso de UM alvo visual. Mantém uma contagem de quantas
  fontes de falha (tags) estão ativas no momento, de forma que um controle
  vinculado a mais de um tag mostre o selo se qualquer um deles estiver com
  problema, sem precisar de lógica extra em quem o usa.
  }
  {$ELSE}
  {:
  Owns the warning badge for ONE visual target. Keeps a count of how many
  fault sources (tags) are currently active, so a control linked to more than
  one tag shows the badge if any of them has a problem, without extra logic
  in whoever uses it.
  }
  {$ENDIF}
  THMICommBadgeController = class(THMIFaultCounter)
  private
    FTarget: TControl;
    FBadge: THMIWarningBadge;
    FFaultCount: Integer;
    FAnchor: TWarningBadgeAnchor;
    FColor: TColor;
    FHasColor: Boolean;
    procedure EnsureBadge;
    procedure UpdateBadgeVisibility;
  public
    destructor Destroy; override;
    procedure SetTarget(ATarget: TControl);
    //: @seealso(THMIWarningBadge)
    procedure SetAnchor(AAnchor: TWarningBadgeAnchor);
    {$IFDEF PORTUGUES}
    //: Cor de fundo do selo - use a mesma cor do controle-alvo pra reforcar a aparencia de que o selo "faz parte" dele.
    {$ELSE}
    //: Badge background color - use the same color as the target control to reinforce the look of the badge "belonging" to it.
    {$ENDIF}
    procedure SetColor(AColor: TColor);
    //: uma fonte de falha passou de OK para falha / a fault source went from OK to faulted
    procedure IncFault; override;
    //: uma fonte de falha passou de falha para OK / a fault source went from faulted to OK
    procedure DecFault; override;
    //: @true se ao menos uma fonte de falha esta ativa no momento / @true if at least one fault source is currently active
    function Faulted: Boolean;
  end;

  {$IFDEF PORTUGUES}
  {:
  Variante sem janela: em vez de mostrar um selo separado, apenas invalida
  (força um repaint de) o controle-dono quando o estado de falha muda. Usada
  por controles que têm um Paint próprio acessível (LCL-desenhados) e
  preferem desenhar o ícone diretamente no seu Canvas - dá transparência
  perfeita, sem o problema de janela separada sobre controle nativo.
  }
  {$ELSE}
  {:
  Windowless variant: instead of showing a separate badge, it just
  invalidates (forces a repaint of) the owner control when the fault state
  changes. Used by controls that have their own accessible Paint
  (LCL-drawn) and prefer to draw the icon directly on their own Canvas -
  gives perfect transparency, without the separate-window-over-native-control
  problem.
  }
  {$ENDIF}
  THMIInlineFaultIndicator = class(THMIFaultCounter)
  private
    FOwnerControl: TControl;
    FFaultCount: Integer;
    //: repaint de reforco, com atraso real (nao so "proxima folga do loop")
    //: - ver comentario em IncFault.
    //: backup repaint, with a real time delay (not just "next loop idle
    //: slot") - see comment on IncFault.
    FDeferredRepaintTimer: TTimer;
    procedure DeferredRepaintTimer(Sender: TObject);
  public
    constructor Create(AOwnerControl: TControl);
    destructor Destroy; override;
    procedure IncFault; override;
    procedure DecFault; override;
    function Faulted: Boolean;
  end;

  {$IFDEF PORTUGUES}
  {:
  Assina os eventos de falha/OK de leitura e escrita de UM tag e reporta
  TRANSIÇÕES de estado (não o estado bruto) a um THMIFaultCounter
  compartilhado (selo em janela ou indicador inline). Isso é o que faz o
  contador funcionar corretamente quando mais de um tag está vinculado ao
  mesmo controle, e evita reagir a cada ciclo de scan (os eventos de
  falha/OK do tag disparam a cada ciclo, não só na transição).
  }
  {$ELSE}
  {:
  Subscribes to one tag's read/write fault/OK events and reports state
  TRANSITIONS (not the raw state) to a shared THMIFaultCounter (windowed
  badge or inline indicator). This is what makes the counter work correctly
  when more than one tag is linked to the same control, and avoids reacting
  on every scan cycle (the tag's fault/OK events fire every cycle, not just
  on the transition).
  }
  {$ENDIF}
  THMITagFaultBadgeLink = class(TObject)
  private
    FTag: TPLCTag;
    FController: THMIFaultCounter;
    FReadFaulted, FWriteFaulted, FReportedFaulted: Boolean;
    procedure ReadFaultCallBack(Sender: TObject);
    procedure ReadOkCallBack(Sender: TObject);
    procedure WriteFaultCallBack(Sender: TObject);
    procedure WriteOkCallBack(Sender: TObject);
    procedure RemoveTagCallBack(Sender: TObject);
    procedure Recompute;
  public
    constructor Create(AController: THMIFaultCounter);
    destructor Destroy; override;
    procedure SetTag(ATag: TPLCTag);
  end;

{$IFDEF PORTUGUES}
{:
Desenha o ícone de aviso centralizado, do tamanho da menor dimensão de
ACanvas, direto num Canvas qualquer (uso pelos controles com Paint
próprio). Se AEraseBackground for @true, preenche primeiro o retângulo do
ícone com o Brush atual de ACanvas (cor/estilo já setados pelo chamador) -
necessário em controles cujo Paint não recobre sempre o mesmo pixel
inteiramente a cada repintura (ex.: THMIAnimation com imagem transparente
ou menor que o controle), senão o ícone (desenhado com transparência real,
não opaco) vai acumulando opacidade a cada repintura até virar um bloco
sólido.
}
{$ELSE}
{:
Draws the warning icon centered, sized to the smallest dimension, directly
on any Canvas (used by controls that draw it in their own Paint). If
AEraseBackground is @true, first fills the icon's rectangle with
ACanvas's current Brush (color/style already set by the caller) -
necessary on controls whose Paint doesn't always fully cover that same
pixel on every repaint (e.g. THMIAnimation with a transparent or
smaller-than-control image), otherwise the icon (drawn with real
transparency, not opaque) keeps accumulating opacity on every repaint
until it turns into a solid block.
}
{$ENDIF}
procedure DrawWarningIcon(ACanvas: TCanvas; AWidth, AHeight: Integer; AEraseBackground: Boolean = False);

{$IFDEF PORTUGUES}
{:
Igual a DrawWarningIcon (centralizado, do tamanho da menor dimensão), mas
dentro de um sub-retângulo [AOffsetX, AOffsetX+AWidth] x [AOffsetY,
AOffsetY+AHeight] em vez de sempre a partir de (0,0) - use quando a area
que deve exibir o icone e' uma REGIAO do controle (ex.: o corpo do motor
sem a parte da bomba, ou a coluna central de um elevador de canecas), nao
o controle inteiro.
}
{$ELSE}
{:
Same as DrawWarningIcon (centered, sized to the smallest dimension), but
within a sub-rectangle [AOffsetX, AOffsetX+AWidth] x [AOffsetY,
AOffsetY+AHeight] instead of always starting at (0,0) - use it when the
area that should show the icon is a REGION of the control (e.g. the motor
body without the pump part, or a bucket elevator's center column), not
the whole control.
}
{$ENDIF}
procedure DrawWarningIconCentered(ACanvas: TCanvas; AWidth, AHeight: Integer; AOffsetX: Integer = 0; AOffsetY: Integer = 0);

{$IFDEF PORTUGUES}
{:
Igual a DrawWarningIcon (centralizado, do tamanho da menor dimensão), mas
desenha DIRETO num TBGRABitmap (ex.: FControlArea) via CanvasBGRA, em vez
de num TCanvas comum. Use quando precisar compor o ícone com transparência
de verdade ANTES do SetShape calcular a máscara - .Canvas (TBitmap de
compatibilidade) não preserva a transparência do ícone nesse cenário.
}
{$ELSE}
{:
Same as DrawWarningIcon (centered, sized to the smallest dimension), but
draws DIRECTLY onto a TBGRABitmap (e.g. FControlArea) via CanvasBGRA,
instead of a plain TCanvas. Use it when you need to compose the icon with
real transparency BEFORE SetShape computes the mask - .Canvas (the
compatibility TBitmap) doesn't preserve the icon's transparency in that
scenario.
}
{$ENDIF}
procedure DrawWarningIconOnBitmap(ADest: TBGRABitmap; AWidth, AHeight: Integer);

{$IFDEF PORTUGUES}
{:
Desenha o ícone encostado numa lateral (esquerda ou direita) em vez de
centralizado sobre todo o controle - usado por controles como o THMIEdit,
onde um ícone centralizado ficaria por cima do texto. O ícone é dimensionado
pela altura disponível (menos uma pequena margem) e alinhado verticalmente
ao centro. AVerticalOffset desloca o resultado pra baixo - use quando a
area disponivel (AHeight) e' uma FAIXA dentro do controle, nao o controle
inteiro (ex.: a faixa "corpo" de uma valvula, que nao comeca em y=0).
}
{$ELSE}
{:
Draws the icon flush against one side (left or right) instead of centered
over the whole control - used by controls like THMIEdit, where a centered
icon would sit on top of the text. The icon is sized to the available
height (minus a small margin) and centered vertically. AVerticalOffset
shifts the result down - use it when the available area (AHeight) is a
STRIP within the control, not the whole control (e.g. a valve's "body"
strip, which doesn't start at y=0).
}
{$ENDIF}
procedure DrawWarningIconAt(ACanvas: TCanvas; AWidth, AHeight: Integer; AtRight: Boolean; AVerticalOffset: Integer = 0);

{$IFDEF PORTUGUES}
{:
Igual a DrawWarningIconAt, mas encostado no topo ou na base (nao lateral) -
dimensionado pela LARGURA disponivel, deslocado horizontalmente por
AHorizontalOffset. Uso simetrico ao de AVerticalOffset em DrawWarningIconAt,
mas pro caso em que a faixa disponivel corre no eixo X (ex.: valvula
desenhada na vertical).
}
{$ELSE}
{:
Same as DrawWarningIconAt, but flush against the top or bottom (not a
side) - sized to the available WIDTH, shifted horizontally by
AHorizontalOffset. Symmetric use case to AVerticalOffset in
DrawWarningIconAt, for when the available strip runs along the X axis
(e.g. a vertically-drawn valve).
}
{$ENDIF}
procedure DrawWarningIconAtTB(ACanvas: TCanvas; AWidth, AHeight: Integer; ABottom: Boolean; AHorizontalOffset: Integer = 0);

{$IFDEF PORTUGUES}
//: Espaço (ícone + margens) que DrawWarningIconAt ocupa para uma dada altura de controle - use para reservar espaço de texto ao lado do ícone.
{$ELSE}
//: Space (icon + margins) that DrawWarningIconAt takes up for a given control height - use it to reserve room for text next to the icon.
{$ENDIF}
function WarningIconMarginWidth(AHeight: Integer): Integer;

{$IFDEF PORTUGUES}
{:
Desenha o icone de aviso centralizado sobre AControl, usando a DC recebida
num WMPaint (LM_PAINT). Alguns widgets GTK2 simples (ex.: TCheckBox,
TScrollBar, TTrackBar em certos temas) nao tem janela (GdkWindow) propria -
compartilham a do pai - e nesse caso a DC do WMPaint tem origem no PAI, nao
no controle, fazendo um desenho ingenuo em (0,0) cair na origem do
formulario em vez de sobre o controle. Esta rotina detecta esse caso
(Parent.Handle=Handle) e compensa deslocando a origem da DC antes de
desenhar.
}
{$ELSE}
{:
Draws the warning icon centered over AControl, using the DC received in a
WMPaint (LM_PAINT). Some simple GTK2 widgets (e.g. TCheckBox, TScrollBar,
TTrackBar under certain themes) have no window of their own (GdkWindow) -
they share their parent's - and in that case the WMPaint's DC has its
origin at the PARENT, not the control, so a naive draw at (0,0) lands at
the form's origin instead of over the control. This routine detects that
case (Parent.Handle=Handle) and compensates by shifting the DC's origin
before drawing.
}
{$ENDIF}
procedure DrawWarningIconOnControlDC(AControl: TWinControl; ADC: HDC);

implementation

const
  BadgeSize = 14;
  IconSideMargin = 2;
  //espaco deixado pra fora do selo, nos lados sem prender no icone, pra
  //borda do controle-alvo (ex.: THMIEdit) continuar aparecendo por baixo -
  //sem isso o selo tapa a borda inteira do lado onde fica.
  //space left outside the badge, on the sides not anchored to the icon, so
  //the target control's own border (e.g. THMIEdit) still shows through -
  //without it the badge covers the whole border on the side it's on.
  BadgeEdgeInset = 2;
  //ioNullDriver NAO e' saudavel - significa "porta sem driver realmente
  //ativo agora" (TModBusDriver.DoRead/DoWrite retornam isso quando
  //PCommPort.ReallyActive=False, ex.: durante reconexao) - e alterna com
  //ioCommError/ioTimeOut a cada ciclo quando a conexao esta genuinamente
  //fora do ar, fazendo o selo entrar e sair repetidamente se fosse tratado
  //como OK aqui.
  //ioNullDriver is NOT healthy - it means "port with no really active
  //driver right now" (TModBusDriver.DoRead/DoWrite return this when
  //PCommPort.ReallyActive=False, e.g. while reconnecting) - and it
  //alternates with ioCommError/ioTimeOut every cycle when the connection
  //is genuinely down, making the badge flap in and out if treated as OK
  //here.
  HealthyResults = [ioNone, ioOk];

  //icone de aviso (triangulo amarelo com "!"), 128x128, PNG com transparencia,
  //codificado em base64 para ficar embutido no binario (sem depender de um
  //arquivo externo em tempo de execucao). gerado a partir de
  //src/hmi/hmi_commfaultbadge_icon.png.
  //warning icon (yellow triangle with "!"), 128x128, PNG with transparency,
  //base64-encoded so it's baked into the binary (no external file needed at
  //runtime). generated from src/hmi/hmi_commfaultbadge_icon.png.
  //AnsiString explicito (nao "String" generico): a package tem
  //UseAnsiStrings=False e este arquivo nao declara {$mode objfpc}/{$H+}
  //proprio, entao "String" aqui viraria ShortString (limite de 255
  //caracteres) e truncaria silenciosamente este literal de +5000
  //caracteres, corrompendo o PNG embutido.
  //explicit AnsiString (not generic "String"): the package has
  //UseAnsiStrings=False and this file declares no {$mode objfpc}/{$H+} of
  //its own, so "String" here would become ShortString (255-char limit) and
  //silently truncate this 5000+ char literal, corrupting the embedded PNG.
  WarningIconPNGBase64: AnsiString =
    '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';

  FWarningIcon: TBGRABitmap = nil;

function GetWarningIcon: TBGRABitmap;
var
  RawBytes: AnsiString;
  st: TStringStream;
begin
  if FWarningIcon=nil then begin
    RawBytes := DecodeStringBase64(WarningIconPNGBase64);
    st := TStringStream.Create(RawBytes);
    try
      FWarningIcon := TBGRABitmap.Create;
      FWarningIcon.LoadFromStream(st);
    finally
      FreeAndNil(st);
    end;
  end;
  Result := FWarningIcon;
end;

procedure DrawWarningIcon(ACanvas: TCanvas; AWidth, AHeight: Integer; AEraseBackground: Boolean);
var
  Icon: TBGRABitmap;
  sz, offX, offY: Integer;
  IconRect: TRect;
begin
  Icon := GetWarningIcon;
  if (Icon=nil) or Icon.Empty then exit;

  //mesmo calculo usado por THMIWarningBadge.Reposition - centralizado, do
  //tamanho da menor dimensao.
  //same calculation used by THMIWarningBadge.Reposition - centered, sized to
  //the smallest dimension.
  sz := AWidth;
  if AHeight<sz then sz := AHeight;
  if sz<1 then exit;

  offX := (AWidth  - sz) div 2;
  offY := (AHeight - sz) div 2;
  IconRect := Rect(offX, offY, offX + sz, offY + sz);

  if AEraseBackground then
    ACanvas.FillRect(IconRect);

  Icon.Draw(ACanvas, IconRect, False);
end;

procedure DrawWarningIconCentered(ACanvas: TCanvas; AWidth, AHeight: Integer; AOffsetX: Integer; AOffsetY: Integer);
var
  Icon: TBGRABitmap;
  sz, offX, offY: Integer;
begin
  Icon := GetWarningIcon;
  if (Icon=nil) or Icon.Empty then exit;

  sz := AWidth;
  if AHeight<sz then sz := AHeight;
  if sz<1 then exit;

  offX := AOffsetX + (AWidth  - sz) div 2;
  offY := AOffsetY + (AHeight - sz) div 2;
  Icon.Draw(ACanvas, Rect(offX, offY, offX + sz, offY + sz), False);
end;

procedure DrawWarningIconOnBitmap(ADest: TBGRABitmap; AWidth, AHeight: Integer);
var
  Icon: TBGRABitmap;
  sz, offX, offY: Integer;
begin
  Icon := GetWarningIcon;
  if (Icon=nil) or Icon.Empty then exit;

  sz := AWidth;
  if AHeight<sz then sz := AHeight;
  if sz<1 then exit;

  offX := (AWidth  - sz) div 2;
  offY := (AHeight - sz) div 2;
  ADest.CanvasBGRA.StretchDraw(Rect(offX, offY, offX + sz, offY + sz), Icon);
end;

procedure DrawWarningIconAt(ACanvas: TCanvas; AWidth, AHeight: Integer; AtRight: Boolean; AVerticalOffset: Integer);
var
  Icon: TBGRABitmap;
  sz, offX, offY: Integer;
begin
  Icon := GetWarningIcon;
  if (Icon=nil) or Icon.Empty then exit;

  sz := AHeight - IconSideMargin*2;
  if sz > AWidth - IconSideMargin*2 then
    sz := AWidth - IconSideMargin*2;
  if sz<1 then exit;

  offY := AVerticalOffset + (AHeight - sz) div 2;
  if AtRight then
    offX := AWidth - sz - IconSideMargin
  else
    offX := IconSideMargin;

  Icon.Draw(ACanvas, Rect(offX, offY, offX + sz, offY + sz), False);
end;

procedure DrawWarningIconAtTB(ACanvas: TCanvas; AWidth, AHeight: Integer; ABottom: Boolean; AHorizontalOffset: Integer);
var
  Icon: TBGRABitmap;
  sz, offX, offY: Integer;
begin
  Icon := GetWarningIcon;
  if (Icon=nil) or Icon.Empty then exit;

  sz := AWidth - IconSideMargin*2;
  if sz > AHeight - IconSideMargin*2 then
    sz := AHeight - IconSideMargin*2;
  if sz<1 then exit;

  offX := AHorizontalOffset + (AWidth - sz) div 2;
  if ABottom then
    offY := AHeight - sz - IconSideMargin
  else
    offY := IconSideMargin;

  Icon.Draw(ACanvas, Rect(offX, offY, offX + sz, offY + sz), False);
end;

function WarningIconMarginWidth(AHeight: Integer): Integer;
var
  sz: Integer;
begin
  sz := AHeight - IconSideMargin*2;
  if sz<1 then sz := 0;
  Result := sz + IconSideMargin*2;
end;

procedure DrawWarningIconOnControlDC(AControl: TWinControl; ADC: HDC);
var
  cnv: TCanvas;
  Icon: TBGRABitmap;
  sz, BaseX, BaseY, offX, offY: Integer;
begin
  Icon := GetWarningIcon;
  if (Icon=nil) or Icon.Empty then exit;

  //SetWindowOrgEx nao surtia efeito (ou surtia efeito errado) porque
  //TBGRABitmap.Draw desenha direto na superficie GDK/Cairo, sem passar
  //pela transformacao logica->device que a LCL emula pro resto do GDI -
  //entao calculamos o retangulo final ja deslocado nos mesmos, em vez de
  //depender de transformacao nenhuma da DC.
  //SetWindowOrgEx had no effect (or the wrong effect) because
  //TBGRABitmap.Draw draws straight onto the GDK/Cairo surface, without
  //going through the logical->device transform the LCL emulates for the
  //rest of the GDI - so we compute the final, already-shifted rect
  //ourselves instead of relying on any DC transform.
  //
  //Nao da' pra confiar em "o controle tem janela propria" (Handle<>
  //Parent.Handle e' sempre verdade - Handle e' so o ponteiro do GtkWidget,
  //nao indica se ele tem GdkWindow propria) nem em csOpaque isoladamente
  //(consertou o THMIScrollBar, mas THMICheckBox/THMITrackBar continuaram
  //desenhando relativo ao formulario mesmo sem csOpaque) - testes ao vivo
  //confirmaram que os tres controles que chamam esta rotina tem esse
  //comportamento (a DC do WMPaint acaba sendo relativa ao formulario, nao
  //ao controle), entao aplicamos o deslocamento sempre.
  //Can't rely on "the control has its own window" (Handle<>Parent.Handle
  //is always true - Handle is just the GtkWidget pointer, it doesn't
  //indicate whether it has its own GdkWindow) nor on csOpaque alone (fixed
  //THMIScrollBar, but THMICheckBox/THMITrackBar kept drawing relative to
  //the form even without csOpaque) - live testing confirmed all three
  //controls that call this routine have this behavior (the WMPaint's DC
  //ends up relative to the form, not the control), so we always apply the
  //offset.
  BaseX := AControl.Left;
  BaseY := AControl.Top;

  sz := AControl.ClientWidth;
  if AControl.ClientHeight<sz then sz := AControl.ClientHeight;
  if sz<1 then exit;

  offX := BaseX + (AControl.ClientWidth  - sz) div 2;
  offY := BaseY + (AControl.ClientHeight - sz) div 2;

  cnv := TCanvas.Create;
  try
    cnv.Handle := ADC;
    Icon.Draw(cnv, Rect(offX, offY, offX + sz, offY + sz), False);
  finally
    cnv.Free;
  end;
end;

{ THMIInlineFaultIndicator }

constructor THMIInlineFaultIndicator.Create(AOwnerControl: TControl);
begin
  inherited Create;
  FOwnerControl := AOwnerControl;
end;

destructor THMIInlineFaultIndicator.Destroy;
begin
  FreeAndNil(FDeferredRepaintTimer);
  inherited Destroy;
end;

procedure THMIInlineFaultIndicator.DeferredRepaintTimer(Sender: TObject);
begin
  FDeferredRepaintTimer.Enabled := False;
  if Assigned(FOwnerControl) and ([csDestroying]*FOwnerControl.ComponentState=[]) then
    FOwnerControl.Repaint;
end;

procedure THMIInlineFaultIndicator.IncFault;
begin
  inc(FFaultCount);
  //Repaint (Invalidate + Update), nao so Invalidate: Invalidate apenas
  //agenda a repintura pro proximo ciclo ocioso do loop de eventos - sem
  //mais nada acontecendo na tela, isso podia ficar pendente ate um evento
  //qualquer (ex.: clique do mouse) "cutucar" o loop e processa-la. Repaint
  //forca a repintura de verdade, imediatamente - funciona pra maioria dos
  //controles.
  //
  //Mas pelo menos o THMIAnimation tem sua propria troca assincrona de
  //imagem (ShowZone: Picture.Clear + reload) disparada pela MESMA janela
  //de tempo - um teste com log mostrou nosso Paint rodando com
  //HasGraphic=False (pousando bem no meio dessa troca) e depois NUNCA MAIS
  //sendo chamado, mesmo com a falha continuando ativa por varios segundos
  //(nada mais invalida o controle nesse meio tempo). Ou seja, um unico
  //repaint de reforco pode colidir com essa troca de imagem e "congelar"
  //um resultado ruim na tela. Por isso o repaint de reforco usa um TTimer
  //com atraso real (nao so "proxima folga do loop"), dando tempo de
  //verdade pra essa troca terminar antes da segunda tentativa.
  //Repaint (Invalidate + Update), not just Invalidate: Invalidate only
  //schedules the repaint for the event loop's next idle cycle - with
  //nothing else happening on screen, that could sit pending until some
  //unrelated event (e.g. a mouse click) "nudged" the loop into processing
  //it. Repaint forces the actual repaint right away - works for most
  //controls.
  //
  //But at least THMIAnimation has its own async picture swap (ShowZone:
  //Picture.Clear + reload) triggered in the same time window - a logged
  //test showed our Paint running with HasGraphic=False (landing right in
  //the middle of that swap) and then NEVER being called again, even with
  //the fault staying active for several seconds (nothing else invalidates
  //the control in the meantime). So a single backup repaint can collide
  //with that picture swap and "freeze" a bad result on screen. That's why
  //the backup repaint uses a TTimer with a real time delay (not just "next
  //loop idle slot"), giving that swap real time to finish before the
  //second attempt.
  if (FFaultCount=1) and Assigned(FOwnerControl) then begin
    FOwnerControl.Repaint;
    if FDeferredRepaintTimer=nil then begin
      FDeferredRepaintTimer := TTimer.Create(nil);
      FDeferredRepaintTimer.Interval := 300;
      FDeferredRepaintTimer.OnTimer := @DeferredRepaintTimer;
      FDeferredRepaintTimer.Enabled := False;
    end;
    FDeferredRepaintTimer.Enabled := False;
    FDeferredRepaintTimer.Enabled := True;
  end;
end;

procedure THMIInlineFaultIndicator.DecFault;
begin
  if FFaultCount>0 then
    dec(FFaultCount);
  if (FFaultCount=0) and Assigned(FOwnerControl) then begin
    if Assigned(FDeferredRepaintTimer) then
      FDeferredRepaintTimer.Enabled := False;
    FOwnerControl.Repaint;
  end;
end;

function THMIInlineFaultIndicator.Faulted: Boolean;
begin
  Result := FFaultCount>0;
end;

{ THMIWarningBadge }

constructor THMIWarningBadge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Paint agora sempre pinta o retangulo inteiro (fundo capturado do alvo +
  //icone), entao csOpaque evita que a LCL perca tempo apagando o fundo antes.
  //Paint now always paints the whole rect (backdrop captured from the target
  //+ icon), so csOpaque avoids the LCL wasting time erasing the background
  //first.
  ControlStyle := ControlStyle + [csOpaque];
  SetInitialBounds(0, 0, BadgeSize, BadgeSize);
  Visible := False;
end;

destructor THMIWarningBadge.Destroy;
begin
  DetachTarget;
  inherited Destroy;
end;

procedure THMIWarningBadge.AttachTo(ATarget: TControl);
begin
  if FTargetControl=ATarget then exit;

  DetachTarget;
  FTargetControl := ATarget;
  if FTargetControl<>nil then begin
    Parent := FTargetControl.Parent;
    FTargetControl.AddHandlerOnChangeBounds(@TargetBoundsChanged);
    FTargetControl.AddHandlerOnVisibleChanged(@TargetVisibleChanged);
    FTargetControl.AddHandlerOnBeforeDestruction(@TargetBeforeDestruction);
    Reposition;
  end;
end;

procedure THMIWarningBadge.DetachTarget;
begin
  if FTargetControl<>nil then begin
    FTargetControl.RemoveAllHandlersOfObject(Self);
    FTargetControl := nil;
  end;
end;

procedure THMIWarningBadge.TargetBoundsChanged(Sender: TObject);
begin
  Reposition;
end;

procedure THMIWarningBadge.TargetVisibleChanged(Sender: TObject);
begin
  if (FTargetControl<>nil) and (not FTargetControl.Visible) then
    Visible := False
  else
    Reposition;
end;

procedure THMIWarningBadge.TargetBeforeDestruction(Sender: TObject);
begin
  DetachTarget;
end;

procedure THMIWarningBadge.Reposition;
var
  sz, w: Integer;
begin
  if FTargetControl=nil then exit;

  case FAnchor of
    wbaLeftEdge, wbaRightEdge: begin
      //faixa vertical do lado do alvo, do tamanho reservado por
      //gtk_entry_set_inner_border (WarningIconMarginWidth) - a mesma
      //largura que o THMIEdit pede pra reservar espaco de texto. Encolhida
      //por BadgeEdgeInset no lado externo (topo, base e a lateral que NAO
      //encosta no icone) pra deixar a borda do proprio controle-alvo
      //aparecer ali e o selo parecer parte dele; o lado que encosta no
      //icone/texto fica encostado, sem inset (nao ha borda ali mesmo).
      //vertical strip on one side of the target, sized to what
      //gtk_entry_set_inner_border reserves (WarningIconMarginWidth) - the
      //same width THMIEdit asks to reserve for text. Shrunk by
      //BadgeEdgeInset on the outer side (top, bottom, and the edge that
      //does NOT touch the icon) so the target control's own border shows
      //through there and the badge looks like part of it; the side that
      //touches the icon/text stays flush, no inset (there's no border
      //there anyway).
      w := WarningIconMarginWidth(FTargetControl.Height);
      if w<1 then w := 1;
      if FAnchor=wbaLeftEdge then
        SetBounds(FTargetControl.Left + BadgeEdgeInset,
                  FTargetControl.Top  + BadgeEdgeInset,
                  Max(1, w - BadgeEdgeInset),
                  Max(1, FTargetControl.Height - BadgeEdgeInset*2))
      else
        SetBounds(FTargetControl.Left + FTargetControl.Width - w,
                  FTargetControl.Top  + BadgeEdgeInset,
                  Max(1, w - BadgeEdgeInset),
                  Max(1, FTargetControl.Height - BadgeEdgeInset*2));
    end;
    else begin
      //o simbolo tem proporcao 1:1 - usa a menor dimensao do controle-alvo
      //para que o selo nunca ultrapasse os limites dele. Encolhido por
      //BadgeEdgeInset (mesmo valor do modo lateral) pra deixar a borda do
      //proprio controle-alvo aparecer ao redor do selo.
      //the symbol has a 1:1 ratio - uses the target control's smallest
      //dimension so the badge never overflows its bounds. Shrunk by
      //BadgeEdgeInset (same value as the side-anchored mode) so the target
      //control's own border shows through around the badge.
      sz := FTargetControl.Width;
      if FTargetControl.Height<sz then
        sz := FTargetControl.Height;
      sz := sz - BadgeEdgeInset*2;
      if sz<1 then
        sz := 1;

      SetBounds(FTargetControl.Left + (FTargetControl.Width  - sz) div 2,
                FTargetControl.Top  + (FTargetControl.Height - sz) div 2,
                sz, sz);
    end;
  end;
end;

procedure THMIWarningBadge.SetAnchor(AAnchor: TWarningBadgeAnchor);
begin
  if FAnchor=AAnchor then exit;
  FAnchor := AAnchor;
  if FTargetControl<>nil then
    Reposition;
end;

procedure THMIWarningBadge.ShowBadge;
begin
  if FTargetControl=nil then exit;
  Reposition;
  Visible := True;
  BringToFront;
end;

procedure THMIWarningBadge.HideBadge;
begin
  Visible := False;
end;

procedure THMIWarningBadge.Paint;
var
  Icon: TBGRABitmap;
begin
  //tentativa anterior usava TWinControl.PaintTo/Parent.PaintTo pra "imitar"
  //transparencia copiando o que estaria por baixo do selo - revertido: no
  //GTK2 (e potencialmente em outros widgetsets) PaintTo nao e confiavel pra
  //varios controles nativos e causava access violation. Fundo solido por
  //enquanto; transparencia de verdade exigiria captura de tela real
  //(GetDC/BitBlt) ou janela em camadas (layered window), especifico por
  //plataforma - fica pra uma proxima etapa se for necessario.
  //previous attempt used TWinControl.PaintTo/Parent.PaintTo to "fake"
  //transparency by copying whatever was underneath the badge - reverted: on
  //GTK2 (and potentially other widgetsets) PaintTo isn't reliable for many
  //native controls and caused an access violation. Solid background for now;
  //real transparency would need actual screen capture (GetDC/BitBlt) or a
  //layered window, both platform-specific - left for a follow-up if needed.
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Style := psClear;
  Canvas.FillRect(ClientRect);

  Icon := GetWarningIcon;
  if (Icon=nil) or Icon.Empty then exit;

  //redesenha o icone de 128x128 escalado pro tamanho atual do selo (o mesmo
  //calculo de Reposition - centralizado, do tamanho da menor dimensao do
  //controle-alvo). BGRABitmap preserva a transparencia do PNG original ao
  //escalar/desenhar.
  //redraws the 128x128 icon scaled to the badge's current size (the same
  //Reposition calculation - centered, sized to the target control's smallest
  //dimension). BGRABitmap preserves the original PNG's transparency while
  //scaling/drawing.
  Icon.Draw(Canvas, Rect(0, 0, Width, Height), False);
end;

{ THMICommBadgeController }

destructor THMICommBadgeController.Destroy;
begin
  FreeAndNil(FBadge);
  inherited Destroy;
end;

procedure THMICommBadgeController.EnsureBadge;
begin
  if FBadge=nil then begin
    FBadge := THMIWarningBadge.Create(nil);
    FBadge.SetAnchor(FAnchor);
    if FHasColor then
      FBadge.Color := FColor;
  end;
end;

procedure THMICommBadgeController.SetAnchor(AAnchor: TWarningBadgeAnchor);
begin
  if FAnchor=AAnchor then exit;
  FAnchor := AAnchor;
  if FBadge<>nil then
    FBadge.SetAnchor(FAnchor);
end;

procedure THMICommBadgeController.SetColor(AColor: TColor);
begin
  if FHasColor and (FColor=AColor) then exit;
  FColor := AColor;
  FHasColor := True;
  if FBadge<>nil then
    FBadge.Color := FColor;
end;

function THMICommBadgeController.Faulted: Boolean;
begin
  Result := FFaultCount>0;
end;

procedure THMICommBadgeController.UpdateBadgeVisibility;
begin
  if FBadge=nil then exit;

  if (FTarget=nil) or ([csDesigning,csDestroying]*FTarget.ComponentState<>[]) then begin
    FBadge.HideBadge;
    exit;
  end;

  if FFaultCount>0 then
    FBadge.ShowBadge
  else
    FBadge.HideBadge;
end;

procedure THMICommBadgeController.SetTarget(ATarget: TControl);
begin
  if FTarget=ATarget then exit;

  FTarget := ATarget;
  if FBadge<>nil then begin
    if FTarget=nil then
      FBadge.DetachTarget
    else
      FBadge.AttachTo(FTarget);
  end;
  UpdateBadgeVisibility;
end;

procedure THMICommBadgeController.IncFault;
begin
  inc(FFaultCount);
  if FFaultCount=1 then begin
    EnsureBadge;
    if FTarget<>nil then
      FBadge.AttachTo(FTarget);
    UpdateBadgeVisibility;
  end;
end;

procedure THMICommBadgeController.DecFault;
begin
  if FFaultCount>0 then
    dec(FFaultCount);
  if FFaultCount=0 then
    UpdateBadgeVisibility;
end;

{ THMITagFaultBadgeLink }

constructor THMITagFaultBadgeLink.Create(AController: THMIFaultCounter);
begin
  inherited Create;
  FController := AController;
end;

destructor THMITagFaultBadgeLink.Destroy;
begin
  SetTag(nil);
  inherited Destroy;
end;

procedure THMITagFaultBadgeLink.SetTag(ATag: TPLCTag);
begin
  if FTag=ATag then exit;

  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);

  FTag := ATag;

  if FTag<>nil then begin
    FTag.AddReadFaultHandler(@ReadFaultCallBack);
    FTag.AddReadOkHandler(@ReadOkCallBack);
    FTag.AddWriteFaultHandler(@WriteFaultCallBack);
    FTag.AddWriteOkHandler(@WriteOkCallBack);
    FTag.AddRemoveTagHandler(@RemoveTagCallBack);
    FReadFaulted  := not (FTag.LastASyncReadStatus  in HealthyResults);
    FWriteFaulted := not (FTag.LastASyncWriteStatus in HealthyResults);
  end else begin
    FReadFaulted  := False;
    FWriteFaulted := False;
  end;

  Recompute;
end;

procedure THMITagFaultBadgeLink.ReadFaultCallBack(Sender: TObject);
begin
  FReadFaulted := True;
  Recompute;
end;

procedure THMITagFaultBadgeLink.ReadOkCallBack(Sender: TObject);
begin
  FReadFaulted := False;
  Recompute;
end;

procedure THMITagFaultBadgeLink.WriteFaultCallBack(Sender: TObject);
begin
  FWriteFaulted := True;
  Recompute;
end;

procedure THMITagFaultBadgeLink.WriteOkCallBack(Sender: TObject);
begin
  FWriteFaulted := False;
  Recompute;
end;

procedure THMITagFaultBadgeLink.RemoveTagCallBack(Sender: TObject);
begin
  if Sender=FTag then begin
    FTag := nil;
    FReadFaulted  := False;
    FWriteFaulted := False;
    Recompute;
  end;
end;

procedure THMITagFaultBadgeLink.Recompute;
var
  NewState: Boolean;
begin
  NewState := FReadFaulted or FWriteFaulted;
  if NewState=FReportedFaulted then exit;

  FReportedFaulted := NewState;
  if Assigned(FController) then begin
    if NewState then
      FController.IncFault
    else
      FController.DecFault;
  end;
end;

finalization
  FreeAndNil(FWarningIcon);

end.
