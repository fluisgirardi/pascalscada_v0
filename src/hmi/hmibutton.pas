{$i ../common/language.inc}
{$IFDEF PORTUGUES}
//: Classe que implementa um botão (precisa ser implementada).
{$ELSE}
//: HMI button class (must be implemented)
{$ENDIF}
unit HMIButton;

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, LMessages, {$ENDIF}Controls,
  Graphics, Dialogs, Buttons, HMITypes, messages, PLCTag, ProtocolTypes, Tag
  {$IFDEF FPC}, GraphType, IntfGraphics, FPImage, Themes{$ENDIF};

type
  {$IFDEF PORTUGUES}
  //: Precisa ser implementado.
  {$ELSE}
  //: Must be implemented.
  {$ENDIF}

  { THMIButton }

  THMIButton = class(TSpeedButton, IHMIInterface)
  private
    FRegInSecMan:Boolean;
    FTag:TPLCTag;
    //tag opcional de retroalimentacao: se informado, e' dele que vem a
    //aparencia do botao; o clique continua escrevendo em PLCTag.
    //optional feedback tag: if set, the looks come from it; clicks keep
    //writing to PLCTag.
    FFeedbackTag:TPLCTag;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FClickFlag:Boolean;
    FAfterGrayed:Boolean;
    FButtonType:TButtonType;
    FOtherValues:TOtherValues;
    FValueDown, FValueUp:Double;
    FGlyphDown, FGlyphUp, FGlyphGrayed:TBitmap;
    //caminho do arquivo de imagem de cada estado (png, bmp, xpm, jpg...).
    //preenchido por GlyphUpFile/GlyphDownFile/GlyphGrayedFile.
    //image file path of each state, set through GlyphUpFile/...File.
    FGlyphUpFile, FGlyphDownFile, FGlyphGrayedFile:String;
    //imagens redimensionadas para o tamanho do botao (StretchGlyph=True).
    //a original fica intacta em FGlyph*; o desenho usa a versao escalada.
    //glyphs resized to the button size (StretchGlyph=True). The original stays
    //in FGlyph*; drawing uses the scaled copy.
    FGlyphUpScaled, FGlyphDownScaled, FGlyphGrayedScaled:TBitmap;
    FStretchGlyph:Boolean;
    FScaledGlyphW, FScaledGlyphH:Integer;
    FColorDown, FColorUp, FColorGrayed:TColor;
    FCaptionDown, FCaptionUp, FCaptionGrayed:TCaption;

    FSecurityCode:UTF8String;
    procedure SetSecurityCode(sc:UTF8String);

    function GetTagValue:Double;
    procedure SetValue(value:Double);

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
    procedure FeedbackTagChangeCallBack(Sender:TObject);
    procedure FeedbackRemoveTagCallBack(Sender:TObject);
    function  GetFeedbackTag:TPLCTag;
    procedure SetFeedbackTag(t:TPLCTag);
    procedure SetGlyphUp(v:TBitmap);
    procedure SetGlyphDown(v:TBitmap);
    procedure SetGlyphGrayed(v:TBitmap);
    //: carrega a imagem do arquivo para o bitmap do estado. '' limpa o bitmap.
    //: loads the image file into the state bitmap; '' clears it.
    procedure LoadGlyphFromFile(aBitmap:TBitmap; const aFileName, aPropertyName:String);
    procedure SetGlyphUpFile(const v:String);
    procedure SetGlyphDownFile(const v:String);
    procedure SetGlyphGrayedFile(const v:String);
    //: liga/desliga o redimensionamento das imagens ao tamanho do botao.
    //: turns the glyph rescaling to the button size on/off.
    procedure SetStretchGlyph(v:Boolean);
    //: recalcula as imagens escaladas quando o tamanho (ou a imagem) mudou.
    //: recomputes the scaled glyphs when the size (or the image) changed.
    procedure RefreshScaledGlyphs;
    //: redimensiona Src para AWidth x AHeight em Dst, preservando o alfa.
    //: resizes Src into Dst preserving alpha.
    procedure ScaleGlyphTo(const Src, Dst:TBitmap; AWidth, AHeight:Integer);
    //: bitmap que deve ser desenhado no estado informado.
    //: bitmap that has to be drawn for the given state.
    function  DisplayGlyph(aState:TButtonState):TBitmap;
    //: aplica a aparencia de acordo com o tag de exibicao (FeedbackTag se
    //: houver, senao PLCTag)
    //: applies the looks from the display tag (FeedbackTag if set, else PLCTag)
    procedure UpdateState;
  protected
    //: Evita o processamento da mensagem no botão.
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    {$IFDEF FPC}
    //: desenha sempre o estado normal/pressionado, nunca o de destaque:
    //: sem a moldura que o tema desenha quando o mouse passa por cima.
    //: always draws the normal/pressed state, never the hot one: no highlight
    //: frame the theme draws when the mouse is over the button.
    function GetDrawDetails: TThemedElementDetails; override;
    {$ENDIF}
    //: @exclude
    //procedure SetDown(value:Boolean);
    //: @exclude
    procedure SetButtonState(bs:TButtonState);
    //: @exclude
    procedure Resize; override;

    //: @seealso(IHMIInterface.SetHMITag)
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
     function GetControlSecurityCode:UTF8String;
    //: @seealso(IHMIInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean);
    //: @seealso(IHMIInterface.MakeUnsecure)
    procedure MakeUnsecure;

    //: @exclude
    procedure SetEnabled(e:Boolean); override;

    //: @seealso OtherValuesIs
    procedure SetOtherValues(v:TOtherValues);
    procedure SetButtonType(v:TButtonType);

    //: @exclude
    function GetGroupIndex:LongInt;
    //: @exclude
    function GetAllowAllUp:Boolean;
    //: @exclude
    function GetDown:Boolean;
    
    //: @exclude
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: LongInt); override;

    //: Retorna o caption corrente do botão;
    function GetCaption:TCaption;
    procedure SetCaptionDown(v:TCaption);
    procedure SetCaptionUp(v:TCaption);
    procedure SetCaptionGrayed(v:TCaption);

    //: Retorna a cor corrente do botão;
    function GetColor:TColor;
    procedure SetColorDown(v:TColor);
    procedure SetColorUp(v:TColor);
    procedure SetColorGrayed(v:TColor);
    
    procedure SetValueDown(v:Double);
    procedure SetValueUp(v:Double);
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
    //: @exclude
    procedure Click; override;
  published
    {:
    Diz como o botão irá interpretar valores diferentes de ValueDown e de
    ValueUp.
    @seealso(TOtherValues);
    }
    property OtherValuesIs:TOtherValues read FOtherValues write SetOtherValues;
    {:
    Define qual será o comportamento do botão.
    @seealso(TButtonType)
    }
    property ButtonType:TButtonType read FButtonType write SetButtonType;
    {:
    Informa qual será o estado que o botão será passado quando ele estiver
    Grayed (nem Precionado e nem Livre).
    
    @True fará que o botão fique precionado e @false fara com que o botão fique
    livre (solto).
    }
    property AfterGray:Boolean read FAfterGrayed write FAfterGrayed;

    //: Retorna o caption corrente do botão.
    property Caption:TCaption read GetCaption;
    {:
    Caption do botão caso o valor do tag associado seja igual a ValueDown
    @seealso(CaptionUp)
    @seealso(CaptionGrayed)
    @seealso(Caption)
    @seealso(PLCTag)
    }
    property CaptionDown:TCaption read FCaptionDown write SetCaptionDown;
    {:
    Caption do botão caso o valor do tag associado seja igual a ValueUp
    @seealso(CaptionDown)
    @seealso(CaptionGrayed)
    @seealso(Caption)
    @seealso(PLCTag)
    }
    property CaptionUp:TCaption read FCaptionUp write SetCaptionUp;
    {:
    Caption do botão caso o valor do tag seja diferente de ValueDown e
    ValueUp.
    @seealso(CaptionDown)
    @seealso(CaptionUp)
    @seealso(Caption)
    @seealso(PLCTag)
    }
    property CaptionGrayed:TCaption read FCaptionGrayed write SetCaptionGrayed;
    
    
    //: Retorna o cor corrente do botão.
    property Color:TColor read GetColor;
    {:
    Cor do botão caso o valor do tag associado seja igual a ValueDown
    @seealso(ColorUp)
    @seealso(ColorGrayed)
    @seealso(Color)
    @seealso(PLCTag)
    }
    property ColorDown:TColor read FColorDown write SetColorDown;
    {:
    Cor do botão caso o valor do tag associado seja igual a ValueUp
    @seealso(ColorDown)
    @seealso(ColorGrayed)
    @seealso(Color)
    @seealso(PLCTag)
    }
    property ColorUp:TColor read FColorUp write SetColorUp;
    {:
    Cor do botão caso o valor do tag seja diferente de ValueDown e
    ValueUp.
    @seealso(ColorDown)
    @seealso(ColorUp)
    @seealso(Color)
    @seealso(PLCTag)
    }
    property ColorGrayed:TColor read FColorGrayed write SetColorGrayed;

    //: @exclude
    property Enabled:Boolean read FIsEnabled write SetEnabled;
    
    {:
    Tag numérico usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCBlockElement)
    @seealso(TPLCTagNumber)
    }
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;

    //: Valor do tag que será considerado como botão precionado.
    property ValueDown:Double read FValueDown write SetValueDown;
    //: Valor do tag que será considerado como botão solto.
    property ValueUp:Double read FValueUp write SetValueUp;

    {:
    Imagem do botão enquanto afundado (o valor do tag de exibição é igual a
    ValueDown). PNG com transparência pode ser carregado direto aqui.
    }
    property GlyphDown:TBitmap read FGlyphDown write SetGlyphDown;
    {:
    Imagem do botão enquanto solto (o valor do tag de exibição é igual a
    ValueUp).
    }
    property GlyphUp:TBitmap read FGlyphUp write SetGlyphUp;
    {:
    Imagem do botão quando o valor do tag não é nem ValueDown nem ValueUp.
    }
    property GlyphGrayed:TBitmap read FGlyphGrayed write SetGlyphGrayed;

    {:
    Arquivo da imagem do estado solto (tag de exibição = ValueUp). Aceita PNG
    com transparência, BMP, XPM e JPG. Basta informar o caminho: o bitmap é
    carregado na hora. Caminho relativo é procurado na pasta do executável.

    Image file of the released state. PNG with transparency, BMP, XPM and JPG
    are accepted. Just give the path: the bitmap is loaded immediately.
    }
    property GlyphUpFile:String read FGlyphUpFile write SetGlyphUpFile;
    {:
    Arquivo da imagem do estado afundado (tag de exibição = ValueDown).

    Image file of the pressed state.
    }
    property GlyphDownFile:String read FGlyphDownFile write SetGlyphDownFile;
    {:
    Arquivo da imagem do estado indefinido (valor que não é nem ValueUp nem
    ValueDown, conforme OtherValues).

    Image file of the undefined state.
    }
    property GlyphGrayedFile:String read FGlyphGrayedFile write SetGlyphGrayedFile;

    {:
    Faz a imagem acompanhar o tamanho do botão: a figura é redimensionada
    (mantendo a transparência do PNG) sempre que o botão muda de tamanho.

    Makes the image follow the button size: the picture is resized (keeping the
    PNG transparency) whenever the button is resized.
    }
    property StretchGlyph:Boolean read FStretchGlyph write SetStretchGlyph default False;

    {:
    Tag opcional de retroalimentação (ex.: a variável que o CLP devolve dizendo
    o estado real). Se vazio, a aparência continua vindo de PLCTag. O clique
    escreve sempre em PLCTag, nunca aqui.

    Optional feedback tag (e.g. the variable the PLC gives back with the real
    state). Empty: the looks keep coming from PLCTag. Clicks always write to
    PLCTag, never here.
    }
    property FeedbackTag:TPLCTag read GetFeedbackTag write SetFeedbackTag;
    
    //: Retorna se o botão esta precionado ou não. Esconde a propriedade herdada.
    property Down: Boolean read GetDown;
    //: GroupIndex do botão. Esconde a propriedade herdada.
    property GroupIndex:LongInt read GetGroupIndex;
    //: Esconde a propriedade herdada.
    property AllowAllUp:Boolean read GetAllowAllUp;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;
  end;

implementation

uses math, hsstrings, ControlSecurityManager;

constructor THMIButton.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FRegInSecMan:=GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
  if not FRegInSecMan then begin
    {$IFNDEF WINDOWS}
    writeln('FIX-ME: Failed to register class ',ClassName,' instace with name="',Name,'" in the ControlSecurityManager?',{$i %FILE%},':',{$i %LINE%});
    {$ENDIF}
  end;
  TSpeedButton(Self).AllowAllUp:=true;
  TSpeedButton(Self).GroupIndex:=$FAB1016;
  //os mesmos valores no designer e em runtime: com ValueDown igual a ValueUp,
  //o TagChangeCallBack casa sempre no primeiro teste e o botao fica
  //permanentemente afundado, e qualquer clique escreve o mesmo zero. Quem
  //carrega de um .lfm sobrescreve os dois de qualquer forma.
  //the same values in the designer and at runtime: with ValueDown equal to
  //ValueUp, TagChangeCallBack always matches on the first test and the button
  //stays pressed forever, and any click writes the same zero. Whatever comes
  //from a .lfm overwrites both anyway.
  FValueDown := 1;
  FValueUp   := 0;
  FIsEnabled:=true;
  //: @seealso(THMIBasicControl.Create) sobre a flag de seguranca nascer verdadeira
  //: @seealso(THMIBasicControl.Create) on the security flag being born true
  FIsEnabledBySecurity:=true;
  FClickFlag:=false;
  FGlyphDown:=TBitmap.Create;
  FGlyphUp:=TBitmap.Create;
  FGlyphGrayed:=TBitmap.Create;
  FGlyphDownScaled:=TBitmap.Create;
  FGlyphUpScaled:=TBitmap.Create;
  FGlyphGrayedScaled:=TBitmap.Create;
  FScaledGlyphW:=-1;
  FScaledGlyphH:=-1;
  FColorDown:=clBtnFace;
  FColorUp:= clBtnFace;
  FColorGrayed:=clBtnShadow;
end;

destructor THMIButton.Destroy;
begin
   if FRegInSecMan then
     GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface)
   else begin
     {$IFNDEF WINDOWS}
     writeln('FIX-ME: Why class ',ClassName,', instace name="',Name,'" ins''t registered in ControlSecurityManager?',{$i %FILE%},':',{$i %LINE%});
     {$ENDIF}
   end;

   if FTag<>nil then
      FTag.RemoveAllHandlersFromObject(Self);
   if (FFeedbackTag<>nil) and (FFeedbackTag<>FTag) then
      FFeedbackTag.RemoveAllHandlersFromObject(Self);
   FGlyphDown.Destroy;
   FGlyphUp.Destroy;
   FGlyphGrayed.Destroy;
   FGlyphDownScaled.Destroy;
   FGlyphUpScaled.Destroy;
   FGlyphGrayedScaled.Destroy;
   inherited Destroy;
end;

procedure THMIButton.Click;
var
  writeflag:boolean;
  valuetowrite:double;
begin
   writeflag:=false;
   try
      //calcula o novo valor do botão..
      //btJog: grava o valor de ligado enquanto segura e o de desligado ao
      //soltar (o de soltar acontece em MouseUp).
      //btJog: writes the on value while held and the off one on release
      //(the release write happens in MouseUp).
      if (FButtonType=btJog) and ((FState=bsDown) or (FState=bsExclusive)) then begin
         valuetowrite := FValueDown;
         writeflag:=true;
         FClickFlag:=true;
         SetButtonState(bsDown);
      end;

      //btSet: cada clique grava o valor de ligado. A aparencia fica por conta
      //do tag (ou do tag de retroalimentacao).
      //btSet: every click writes the on value. The looks come from the tag
      //(or from the feedback tag).
      if FButtonType=btSet then begin
         valuetowrite := FValueDown;
         writeflag := true;
         SetButtonState(bsUp);
      end;

      //btReset: cada clique grava o valor de desligado.
      //btReset: every click writes the off value.
      if FButtonType=btReset then begin
         valuetowrite := FValueUp;
         writeflag := true;
         SetButtonState(bsUp);
      end;

      //btToogle: inverte o valor do tag. Se ha' tag de retroalimentacao a
      //aparencia segue o que o CLP devolveu; sem ele o botao fica sempre
      //solto.
      //btToogle: inverts the tag value. With a feedback tag the looks follow
      //what the PLC gave back; without one the button always looks released.
      if FButtonType=btToogle then begin
         if GetTagValue=FValueDown then
            valuetowrite := FValueUp
         else
            valuetowrite := FValueDown;
         writeflag := true;
         SetButtonState(bsUp);
      end;

      if writeflag then
         SetValue(valuetowrite);
   finally
     inherited Click;
   end;
end;

procedure THMIButton.SetSecurityCode(sc: UTF8String);
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

function THMIButton.GetTagValue:Double;
begin
  Result := 0;
  if FTag=Nil then exit;

  if Supports(FTag, ITagNumeric) then
    Result := (FTag as ITagNumeric).Value;
end;

procedure THMIButton.SetValue(value:Double);
begin
  if FTag=Nil then exit;

  if Supports(FTag, ITagNumeric) then
    (FTag as ITagNumeric).Value := value;
end;


procedure THMIButton.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  if (t<>nil) AND (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove; o tag de retroalimentacao pode ser
  //o mesmo objeto, entao limpa os dois e recoloca o que sobrar
  //if already linked to a tag, remove; the feedback tag can be the same
  //object, so clean both and put back what survives
  if FTag<>nil then
     FTag.RemoveAllHandlersFromObject(Self);
  if (FFeedbackTag<>nil) and (FFeedbackTag<>FTag) then
     FFeedbackTag.RemoveAllHandlersFromObject(Self);

  FTag := t;

  //adiona o callback para o novo tag
  if t<>nil then begin
     //os tres, como em todos os outros controles: sem o de mudanca de valor o
     //botao lia o tag uma unica vez, na hora de ser ligado, e depois mostrava
     //o clique do operador em vez do que o processo respondeu - o motor
     //ligando por outra tela ou por intertravamento nao aparecia aqui.
     //all three, as in every other control: without the value change one the
     //button read the tag a single time, when it was linked, and from then on
     //showed the operator's click instead of what the process answered - the
     //motor starting from another screen or from an interlock never showed up
     //here.
     t.AddWriteFaultHandler(@WriteFaultCallBack);
     t.AddTagChangeHandler(@TagChangeCallBack);
     t.AddRemoveTagHandler(@RemoveTagCallBack);
  end;

  //recoloca os tratadores de retroalimentacao, seja la' qual for o objeto
  //puts the feedback handlers back, whichever the object is
  if FFeedbackTag<>nil then begin
     FFeedbackTag.AddTagChangeHandler(@FeedbackTagChangeCallBack);
     FFeedbackTag.AddRemoveTagHandler(@FeedbackRemoveTagCallBack);
  end;

  UpdateState;
end;

function  THMIButton.GetHMITag:TPLCTag;
begin
   Result := FTag;
end;

function THMIButton.GetControlSecurityCode: UTF8String;
begin
   Result:=FSecurityCode;
end;

procedure THMIButton.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity :=a;
  SetEnabled(FIsEnabled);
end;

procedure THMIButton.MakeUnsecure;
begin
   FSecurityCode:='';
   CanBeAccessed(true);
end;

procedure THMIButton.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure THMIButton.CMButtonPressed(var Message: TMessage);
begin
   //nao faz nada alem de roubar o processamento da mensagem
   //CM_BUTTONPRESSED de TSpeedButton.
end;

{$IFDEF FPC}
//o tema desenha tbPushButtonHot/ttbButtonHot quando o mouse passa por cima e
//ttbButtonCheckedHot num botao Flat afundado com o mouse em cima - a moldura
//de destaque que atrapalha as imagens do botao. Aqui so' existem tres
//aparencias: normal, afundado e desabilitado; nenhuma muda com o mouse.
//the theme draws the hot elements when the mouse is over the button and the
//checked-hot one on a sunken Flat button under the mouse - the highlight
//frame that gets in the way of the button images. Only three looks exist
//here: normal, sunken and disabled; none of them changes with the mouse.
function THMIButton.GetDrawDetails: TThemedElementDetails;
begin
   if not IsEnabled then
      Result := ThemeServices.GetElementDetails(tbPushButtonDisabled)
   else
   if FState in [bsDown, bsExclusive] then
      Result := ThemeServices.GetElementDetails(tbPushButtonPressed)
   else
      Result := ThemeServices.GetElementDetails(tbPushButtonNormal);
end;
{$ENDIF}

//procedure THMIButton.SetDown(Value: Boolean);
//begin
   //FDown := Value;
   //if Value  then
      //FState := bsDown
   //else
      //FState :=  bsUp;
   //Invalidate;
//end;

procedure THMIButton.SetButtonState(bs:TButtonState);
begin
   FState:=bs;
   if FStretchGlyph then
      RefreshScaledGlyphs;
   case bs of
      bsUp {$IFDEF FPC}, bsHot{$ENDIF}:
         with TSpeedButton(Self) do begin
            Caption:=CaptionUp;
            Color:=ColorUp;
            Glyph.Assign(DisplayGlyph(bs));
            if FButtonType=btJog then
               SetDown(false);
         end;
      bsDisabled:
         with TSpeedButton(Self) do begin
            Caption:=CaptionGrayed;
            Color:=ColorGrayed;
            Glyph.Assign(DisplayGlyph(bs));
            if FButtonType=btJog then
               SetDown(false);
         end;
      bsDown, bsExclusive:
         with TSpeedButton(Self) do begin
            Caption:=CaptionDown;
            Color:=ColorDown;
            Glyph.Assign(DisplayGlyph(bs));
            if FButtonType=btJog then
               SetDown(true);
         end;
   end;
end;

procedure THMIButton.SetOtherValues(v: TOtherValues);
begin
   FOtherValues:=V;
   TagChangeCallBack(self);
end;

procedure THMIButton.SetButtonType(v:TButtonType);
begin
   FButtonType:=v;
end;

function THMIButton.GetGroupIndex:LongInt;
begin
   Result := TSpeedButton(Self).GroupIndex;
end;

function THMIButton.GetAllowAllUp:Boolean;
begin
   Result :=  TSpeedButton(Self).AllowAllUp;
end;

function THMIButton.GetDown:Boolean;
begin
   //bsExclusive tambem: o construtor sempre define um GroupIndex, e o
   //SetDown(true) da LCL leva um botao agrupado para bsExclusive, nao para
   //bsDown. Sem os dois, a propriedade lia false justamente nos tipos
   //que ficam afundados.
   //bsExclusive too: the constructor always sets a GroupIndex, and the LCL's
   //SetDown(true) takes a grouped button to bsExclusive, not to bsDown.
   //Without both, the property read false on exactly the types that stay
   //pressed.
   Result := FState in [bsDown, bsExclusive];
end;

procedure THMIButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: LongInt);
begin
    try
       if FClickFlag and (FButtonType=btJog) then
          SetValue(FValueUp);
       FClickFlag:=false;
    finally
       inherited MouseUp(Button, Shift, X, Y);
    end;
end;

function THMIButton.GetCaption:TCaption;
begin
   Result := TSpeedButton(Self).Caption;
end;

procedure THMIButton.SetCaptionDown(v:TCaption);
begin
   FCaptionDown:=v;
   SetButtonState(FState);
end;

procedure THMIButton.SetCaptionUp(v:TCaption);
begin
   FCaptionUp:=v;
   SetButtonState(FState);
end;

procedure THMIButton.SetCaptionGrayed(v:TCaption);
begin
   FCaptionGrayed:=v;
   SetButtonState(FState);
end;

function THMIButton.GetColor:TColor;
begin
   Result := TSpeedButton(Self).Color;
end;

procedure THMIButton.SetColorDown(v:TColor);
begin
   FColorDown := v;
   SetButtonState(FState);
end;

procedure THMIButton.SetColorUp(v:TColor);
begin
   FColorUp := v;
   SetButtonState(FState);
end;

procedure THMIButton.SetColorGrayed(v:TColor);
begin
   FColorGrayed := v;
   SetButtonState(FState);
end;

procedure THMIButton.SetValueDown(v:Double);
begin
   FValueDown:=v;
   TagChangeCallBack(self);
end;

procedure THMIButton.SetValueUp(v:Double);
begin
   FValueUp:=v;
   TagChangeCallBack(self);
end;

procedure THMIButton.Loaded;
begin
  inherited Loaded;
  CanBeAccessed(GetControlSecurityManager.CanAccess(GetControlSecurityCode));
end;

procedure THMIButton.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIButton.TagChangeCallBack(Sender: TObject);
begin
   //o clique muda o que sera' escrito; a aparencia vem de UpdateState
   //the click changes what will be written; the looks come from UpdateState
   UpdateState;
end;

procedure THMIButton.FeedbackTagChangeCallBack(Sender: TObject);
begin
   UpdateState;
end;

procedure THMIButton.FeedbackRemoveTagCallBack(Sender: TObject);
begin
   FFeedbackTag := nil;
   UpdateState;
end;

procedure THMIButton.UpdateState;
var
   value:Double;
begin
   //o botao de inversao sem retroalimentacao nunca fica afundado, seja qual
   //for o valor do tag. Com um tag de retroalimentacao a historia muda: o
   //clique inverte o valor escrito e a aparencia segue o que o CLP devolveu,
   //que e' justamente o que se espera de um botao de duas imagens.
   //the toggle button without feedback never looks pressed, whatever the tag
   //value. With a feedback tag it changes: the click inverts the written
   //value and the looks follow what the PLC gave back, which is exactly what
   //a two-image button is expected to do.
   if (FButtonType=btToogle) and (FFeedbackTag=nil) then begin
      SetButtonState(bsUp);
      exit;
   end;

   //a aparencia vem do tag de retroalimentacao quando houver; sem nenhum dos
   //dois tags nao ha' leitura: Infinity nao casa com nenhum dos dois valores,
   //entao o botao cai no tratamento de OtherValues.
   //the looks come from the feedback tag when there is one; with no tag at
   //all there is no reading: Infinity matches neither value, so the button
   //falls into the OtherValues handling.
   if FFeedbackTag<>nil then
      value := (FFeedbackTag as ITagNumeric).Value
   else
      if FTag=nil then
         value := Infinity
      else
         value := GetTagValue;

   if value = FValueDown then
      SetButtonState(bsDown)
   else begin
      if value = FValueUp then
         SetButtonState(bsUp)
      else
         case FOtherValues of
            isChecked:
               SetButtonState(bsDown);
            isUnchecked:
               SetButtonState(bsUp);
            IsGrayed:
               SetButtonState(bsDisabled);
         end;
   end;
end;

function THMIButton.GetFeedbackTag:TPLCTag;
begin
   Result := FFeedbackTag;
end;

procedure THMIButton.SetFeedbackTag(t:TPLCTag);
begin
   //so' tags numericos, como no PLCTag
   //numeric tags only, as in PLCTag
   if (t<>nil) and (not Supports(t, ITagNumeric)) then
      raise Exception.Create(SonlyNumericTags);

   if (FFeedbackTag<>nil) and (FFeedbackTag<>FTag) then
      FFeedbackTag.RemoveAllHandlersFromObject(Self);

   FFeedbackTag := t;
   if t<>nil then begin
      t.AddTagChangeHandler(@FeedbackTagChangeCallBack);
      t.AddRemoveTagHandler(@FeedbackRemoveTagCallBack);
   end;
   UpdateState;
end;

procedure THMIButton.SetGlyphUp(v:TBitmap);
begin
   FGlyphUp.Assign(v);
   FScaledGlyphW:=-1;
   FScaledGlyphH:=-1;
   SetButtonState(FState);
end;

procedure THMIButton.SetGlyphDown(v:TBitmap);
begin
   FGlyphDown.Assign(v);
   FScaledGlyphW:=-1;
   FScaledGlyphH:=-1;
   SetButtonState(FState);
end;

procedure THMIButton.SetGlyphGrayed(v:TBitmap);
begin
   FGlyphGrayed.Assign(v);
   FScaledGlyphW:=-1;
   FScaledGlyphH:=-1;
   SetButtonState(FState);
end;

procedure THMIButton.LoadGlyphFromFile(aBitmap:TBitmap; const aFileName,
  aPropertyName:String);
var
  aFullName:String;
  aPicture:TPicture;
begin
  if Trim(aFileName)='' then begin
     aBitmap.SetSize(0,0);
     exit;
  end;

  aFullName:=Trim(aFileName);
  //caminho relativo: procura na pasta do executavel, como o resto do SCADA.
  //relative path: look in the executable folder, as the rest of SCADA does.
  if (not FileExists(aFullName)) and (ExtractFileDrive(aFullName)='') then
     aFullName:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+Trim(aFileName);

  if not FileExists(aFullName) then
     raise Exception.CreateFmt('%s: imagem nao encontrada / image not found: %s',
       [aPropertyName, Trim(aFileName)]);

  aPicture:=TPicture.Create;
  try
     aPicture.LoadFromFile(aFullName);
     //Assign copia o alfa, entao o PNG continua transparente no botao.
     //Assign copies the alpha, so the PNG stays transparent on the button.
     aBitmap.Assign(aPicture.Graphic);
  finally
     aPicture.Free;
  end;
end;

procedure THMIButton.SetGlyphUpFile(const v:String);
begin
   FGlyphUpFile:=v;
   LoadGlyphFromFile(FGlyphUp, v, 'GlyphUpFile');
   FScaledGlyphW:=-1;
   FScaledGlyphH:=-1;
   SetButtonState(FState);
end;

procedure THMIButton.SetGlyphDownFile(const v:String);
begin
   FGlyphDownFile:=v;
   LoadGlyphFromFile(FGlyphDown, v, 'GlyphDownFile');
   FScaledGlyphW:=-1;
   FScaledGlyphH:=-1;
   SetButtonState(FState);
end;

procedure THMIButton.SetGlyphGrayedFile(const v:String);
begin
   FGlyphGrayedFile:=v;
   LoadGlyphFromFile(FGlyphGrayed, v, 'GlyphGrayedFile');
   FScaledGlyphW:=-1;
   FScaledGlyphH:=-1;
   SetButtonState(FState);
end;

procedure THMIButton.SetStretchGlyph(v:Boolean);
begin
   if FStretchGlyph=v then exit;
   FStretchGlyph:=v;
   FScaledGlyphW:=-1;
   FScaledGlyphH:=-1;
   SetButtonState(FState);
end;

procedure THMIButton.Resize;
begin
   inherited Resize;
   if FStretchGlyph then
      SetButtonState(FState);
end;

procedure THMIButton.RefreshScaledGlyphs;
var
  w, h:Integer;
begin
   w:=ClientWidth;
   h:=ClientHeight;
   if (w<=0) or (h<=0) then begin
      if (FScaledGlyphW<>0) or (FScaledGlyphH<>0) then begin
         FScaledGlyphW:=0;
         FScaledGlyphH:=0;
         FGlyphUpScaled.SetSize(0,0);
         FGlyphDownScaled.SetSize(0,0);
         FGlyphGrayedScaled.SetSize(0,0);
      end;
      exit;
   end;
   if (w=FScaledGlyphW) and (h=FScaledGlyphH) then
      exit;
   FScaledGlyphW:=w;
   FScaledGlyphH:=h;
   ScaleGlyphTo(FGlyphUp, FGlyphUpScaled, w, h);
   ScaleGlyphTo(FGlyphDown, FGlyphDownScaled, w, h);
   ScaleGlyphTo(FGlyphGrayed, FGlyphGrayedScaled, w, h);
end;

procedure THMIButton.ScaleGlyphTo(const Src, Dst:TBitmap; AWidth, AHeight:Integer);
{$IFDEF FPC}
var
  SrcImg, DstImg:TLazIntfImage;
  x, y:Integer;
  x0, x1, y0, y1:Integer;
  fx, fy:Double;
  w00, w10, w01, w11:Double;
  c00, c10, c01, c11:TFPColor;
  a00, a10, a01, a11, aSum:Double;
  rAcc, gAcc, bAcc:Double;
  c:TFPColor;
  HasTransparent:Boolean;
begin
   if (Src.Width<=0) or (Src.Height<=0) or (AWidth<=0) or (AHeight<=0) then begin
      Dst.SetSize(0,0);
      exit;
   end;
   Dst.SetSize(AWidth, AHeight);
   SrcImg:=Src.CreateIntfImage;
   DstImg:=Dst.CreateIntfImage;
   //o bitmap do LCL no Windows pode perder o canal alfa ao passar pelo
   //handle: os pixels totalmente transparentes tambem viram cor-chave, que e'
   //o que o TButtonGlyph usa para desenhar o fundo.
   //the LCL bitmap on Windows may lose the alpha channel through the handle:
   //fully transparent pixels also become a colour key, which is what
   //TButtonGlyph uses to let the background show through.
   HasTransparent:=Src.Transparent;
   try
      for y:=0 to AHeight-1 do begin
         //centro do pixel de destino levado para a grade de origem
         //destination pixel centre mapped back to the source grid
         fy:=(y+0.5)*Src.Height/AHeight-0.5;
         if fy<0 then fy:=0;
         y0:=Trunc(fy);
         if y0>Src.Height-1 then y0:=Src.Height-1;
         y1:=y0+1;
         if y1>Src.Height-1 then y1:=Src.Height-1;
         fy:=fy-y0;
         for x:=0 to AWidth-1 do begin
            fx:=(x+0.5)*Src.Width/AWidth-0.5;
            if fx<0 then fx:=0;
            x0:=Trunc(fx);
            if x0>Src.Width-1 then x0:=Src.Width-1;
            x1:=x0+1;
            if x1>Src.Width-1 then x1:=Src.Width-1;
            fx:=fx-x0;

            w00:=(1-fx)*(1-fy);
            w10:=fx*(1-fy);
            w01:=(1-fx)*fy;
            w11:=fx*fy;

            c00:=SrcImg.Colors[x0,y0];
            c10:=SrcImg.Colors[x1,y0];
            c01:=SrcImg.Colors[x0,y1];
            c11:=SrcImg.Colors[x1,y1];

            //premultiplicado por alfa: pixel transparente nao puxa o RGB
            //premultiplied by alpha: a transparent pixel does not drag RGB
            a00:=w00*c00.alpha;
            a10:=w10*c10.alpha;
            a01:=w01*c01.alpha;
            a11:=w11*c11.alpha;
            aSum:=a00+a10+a01+a11;

            if aSum>0 then begin
               rAcc:=a00*c00.red  +a10*c10.red  +a01*c01.red  +a11*c11.red;
               gAcc:=a00*c00.green+a10*c10.green+a01*c01.green+a11*c11.green;
               bAcc:=a00*c00.blue +a10*c10.blue +a01*c01.blue +a11*c11.blue;
               c.red  :=EnsureRange(Round(rAcc/aSum), 0, $FFFF);
               c.green:=EnsureRange(Round(gAcc/aSum), 0, $FFFF);
               c.blue :=EnsureRange(Round(bAcc/aSum), 0, $FFFF);
            end else begin
               //cor-chave: fucsia, a mesma usada pelo LCL nos bitmaps mascarados
               //colour key: fuchsia, the same one LCL uses on masked bitmaps
               HasTransparent:=True;
               c.red:=$FFFF;
               c.green:=0;
               c.blue:=$FFFF;
            end;
            c.alpha:=EnsureRange(Round(aSum), 0, $FFFF);
            DstImg.Colors[x,y]:=c;
         end;
      end;
      Dst.LoadFromIntfImage(DstImg);
      Dst.Transparent:=HasTransparent;
      if HasTransparent then
         Dst.TransparentColor:=clFuchsia;
   finally
      SrcImg.Free;
      DstImg.Free;
   end;
end;
{$ELSE}
begin
   if (Src.Width<=0) or (Src.Height<=0) or (AWidth<=0) or (AHeight<=0) then begin
      Dst.SetSize(0,0);
      exit;
   end;
   Dst.SetSize(AWidth, AHeight);
   Dst.Canvas.Brush.Style:=bsClear;
   Dst.Canvas.StretchDraw(Rect(0,0,AWidth,AHeight), Src);
end;
{$ENDIF}

function THMIButton.DisplayGlyph(aState:TButtonState):TBitmap;
begin
   if not FStretchGlyph then begin
      case aState of
         bsDown, bsExclusive: Result:=FGlyphDown;
         bsDisabled:          Result:=FGlyphGrayed;
      else
         Result:=FGlyphUp;
      end;
      exit;
   end;
   case aState of
      bsDown, bsExclusive: Result:=FGlyphDownScaled;
      bsDisabled:          Result:=FGlyphGrayedScaled;
   else
      Result:=FGlyphUpScaled;
   end;
end;

procedure THMIButton.RemoveTagCallBack(Sender: TObject);
begin
   FTag := nil;
   //o mesmo objeto pode estar nos dois papeis
   //the same object can play both roles
   if FFeedbackTag=Sender then FFeedbackTag := nil;
   TagChangeCallBack(Self);
end;

end.
