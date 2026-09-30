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
Desenha o ícone encostado numa lateral (esquerda ou direita) em vez de
centralizado sobre todo o controle - usado por controles como o THMIEdit,
onde um ícone centralizado ficaria por cima do texto. O ícone é dimensionado
pela altura disponível (menos uma pequena margem) e alinhado verticalmente
ao centro.
}
{$ELSE}
{:
Draws the icon flush against one side (left or right) instead of centered
over the whole control - used by controls like THMIEdit, where a centered
icon would sit on top of the text. The icon is sized to the available
height (minus a small margin) and centered vertically.
}
{$ENDIF}
procedure DrawWarningIconAt(ACanvas: TCanvas; AWidth, AHeight: Integer; AtRight: Boolean);

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
  WarningIconPNGBase64: String =
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
      FWarningIcon.LoadFromFile('/home/fabiolg/desenvolvimento/base_libs/pascalscada/src/hmi/hmi_commfaultbadge_icon2.png');
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

procedure DrawWarningIconAt(ACanvas: TCanvas; AWidth, AHeight: Integer; AtRight: Boolean);
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

  offY := (AHeight - sz) div 2;
  if AtRight then
    offX := AWidth - sz - IconSideMargin
  else
    offX := IconSideMargin;

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
      //para que o selo nunca ultrapasse os limites dele.
      //the symbol has a 1:1 ratio - uses the target control's smallest
      //dimension so the badge never overflows its bounds.
      sz := FTargetControl.Width;
      if FTargetControl.Height<sz then
        sz := FTargetControl.Height;
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
