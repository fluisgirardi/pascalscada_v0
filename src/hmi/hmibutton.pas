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
  Graphics, Dialogs, Buttons, HMITypes, messages, PLCTag, ProtocolTypes, Tag,
  hmi_commfaultbadge;

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
    FCommIndicator:THMIInlineFaultIndicator;
    FCommFaultLink:THMITagFaultBadgeLink;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FClickFlag:Boolean;
    FAfterGrayed:Boolean;
    FButtonType:TButtonType;
    FOtherValues:TOtherValues;
    FValueDown, FValueUp:Double;
    FGlyphDown, FGlyphUp, FGlyphGrayed:TBitmap;
    FColorDown, FColorUp, FColorGrayed:TColor;
    FCaptionDown, FCaptionUp, FCaptionGrayed:TCaption;

    FSecurityCode:UTF8String;
    procedure SetSecurityCode(sc:UTF8String);

    function GetTagValue:Double;
    procedure SetValue(value:Double);

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
  protected
    //: Evita o processamento da mensagem no botão.
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    //: @exclude
    //procedure SetDown(value:Boolean);
    //: @exclude
    procedure SetButtonState(bs:TButtonState);

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
    procedure Paint; override;
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
  FClickFlag:=false;
  FGlyphDown:=TBitmap.Create;
  FGlyphUp:=TBitmap.Create;
  FGlyphGrayed:=TBitmap.Create;
  FColorDown:=clBtnFace;
  FColorUp:= clBtnFace;
  FColorGrayed:=clBtnShadow;

  FCommIndicator:=THMIInlineFaultIndicator.Create(Self);
  FCommFaultLink:=THMITagFaultBadgeLink.Create(FCommIndicator);
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
   FreeAndNil(FCommFaultLink);
   FreeAndNil(FCommIndicator);
   FGlyphDown.Destroy;
   FGlyphUp.Destroy;
   FGlyphGrayed.Destroy;
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
      if FButtonType = btOnOff  then begin
         //click é chamado apos a atualização
         //de FState
         case FState of
            bsUp {$IFDEF FPC}, bsHot{$ENDIF}:
              valuetowrite := FValueUp;
            bsDown, bsExclusive:
              valuetowrite := FValueDown;
            else
              if FAfterGrayed then
                 valuetowrite := FValueDown
              else
                 valuetowrite := FValueUp;
         end;
         writeflag := true;
         SetButtonState(FState);
      end;
      
      if (FButtonType=btJog) and ((FState=bsDown) or (FState=bsExclusive)) then begin
         valuetowrite := FValueDown;
         writeflag:=true;
         FClickFlag:=true;
         SetButtonState(bsDown);
      end;

      //btToogle: inverte o valor do tag e mantem a aparencia solta, como o
      //TButtonType documenta. Nao tinha ramo nenhum aqui - o tipo aparecia no
      //inspetor e o botao nao escrevia nada, nunca.
      //btToogle: inverts the tag value and keeps the released look, as
      //TButtonType documents. There was no branch at all here - the type
      //showed up on the inspector and the button never wrote anything.
      if FButtonType=btToogle then begin
         if GetTagValue=FValueDown then
            valuetowrite := FValueUp
         else
            valuetowrite := FValueDown;
         writeflag := true;
         SetButtonState(bsUp);
      end;

      if (FButtonType=btMomentary) and ((FState=bsDown) or (FState=bsExclusive)) then begin
         SetValue(FValueDown);
         SetButtonState(bsDown);
         Invalidate;
         valuetowrite := FValueUp;
         writeflag:=true;
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

  //se ja estou associado a um tag, remove
  if FTag<>nil then begin
     FTag.RemoveAllHandlersFromObject(Self);
  end;

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

     FTag := t;
  end;
  FTag := t;
  TagChangeCallBack(self);
  if Assigned(FCommFaultLink) then
    FCommFaultLink.SetTag(t);
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
   case bs of
      bsUp {$IFDEF FPC}, bsHot{$ENDIF}:
         with TSpeedButton(Self) do begin
            Caption:=CaptionUp;
            Color:=ColorUp;
            Glyph.Assign(FGlyphUp);
            if FButtonType in [btJog, btOnOff] then
               SetDown(false);
         end;
      bsDisabled:
         with TSpeedButton(Self) do begin
            Caption:=CaptionGrayed;
            Color:=ColorGrayed;
            Glyph.Assign(FGlyphGrayed);
            if FButtonType in [btJog, btOnOff] then
               SetDown(false);
         end;
      bsDown, bsExclusive:
         with TSpeedButton(Self) do begin
            Caption:=CaptionDown;
            Color:=ColorDown;
            Glyph.Assign(FGlyphDown);
            if FButtonType in [btJog, btOnOff] then
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
   //bsDown. Sem os dois, a propriedade lia false justamente nos dois tipos
   //que ficam afundados (btJog e btOnOff).
   //bsExclusive too: the constructor always sets a GroupIndex, and the LCL's
   //SetDown(true) takes a grouped button to bsExclusive, not to bsDown.
   //Without both, the property read false on exactly the two types that stay
   //pressed (btJog and btOnOff).
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

procedure THMIButton.Paint;
begin
  inherited Paint;
  if Assigned(FCommIndicator) and FCommIndicator.Faulted then
    DrawWarningIcon(Canvas, Width, Height);
end;

procedure THMIButton.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIButton.TagChangeCallBack(Sender: TObject);
var
   value:Double;
begin
   //o botao de inversao nunca fica afundado, seja qual for o valor do tag.
   //the toggle button never stays pressed, whatever the tag's value is.
   if FButtonType=btToogle then begin
      SetButtonState(bsUp);
      exit;
   end;

   //sem tag nao ha' leitura: Infinity nao casa com nenhum dos dois valores,
   //entao o botao cai no tratamento de OtherValues - "esse valor nao e' nem um
   //nem outro" - em vez de mostrar o ultimo estado lido.
   //with no tag there is no reading: Infinity matches neither value, so the
   //button falls into the OtherValues handling - "this value is neither of the
   //two" - instead of showing the last state read.
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

procedure THMIButton.RemoveTagCallBack(Sender: TObject);
begin
   FTag := nil;
   TagChangeCallBack(Self);
end;

end.
