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
  Graphics, Dialogs, Buttons, HMITypes, messages, PLCTag, ProtocolTypes, Tag;

type
  {$IFDEF PORTUGUES}
  //: Precisa ser implementado.
  {$ELSE}
  //: Must be implemented.
  {$ENDIF}
  THMIButton = class(TSpeedButton, IHMIInterface)
  private
    FTag:TPLCTag;
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

uses hsstrings, ControlSecurityManager;

constructor THMIButton.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TSpeedButton(Self).AllowAllUp:=true;
   TSpeedButton(Self).GroupIndex:=$FAB1016;
   if csDesigning in componentState then begin
      FValueDown  := 1;
      FValueUp := 0;
   end else begin
      FValueDown  := 0;
      FValueUp := 0;
   end;
   FIsEnabled:=true;
   FClickFlag:=false;
   FGlyphDown:=TBitmap.Create;
   FGlyphUp:=TBitmap.Create;
   FGlyphGrayed:=TBitmap.Create;
   FColorDown:=clBtnFace;
   FColorUp:= clBtnFace;
   FColorGrayed:=clBtnShadow;
   GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIButton.Destroy;
begin
   if FTag<>nil then
      FTag.RemoveAllHandlersFromObject(Self);
   FGlyphDown.Destroy;
   FGlyphUp.Destroy;
   FGlyphGrayed.Destroy;
   GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
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
     t.AddRemoveTagHandler(@RemoveTagCallBack);

     FTag := t;
     TagChangeCallBack(self);
  end;
  FTag := t;
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
   Result :=  FState = bsDown;
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

procedure THMIButton.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIButton.TagChangeCallBack(Sender: TObject);
var
   value:Double;
begin
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
end;

end.
