{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit que implementa uma caixa para entrada de valores em Tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a input to enter values on Tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIEdit;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, PLCTag, HMITypes, Graphics, Dialogs,
  {$IFDEF FPC}LCLIntf, LCLType,{$ELSE}Windows,{$ENDIF} ProtocolTypes, Tag,
  unumerickeyboard, Forms;

type
  {$IFDEF PORTUGUES}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    Implementa um Edit para leitura/escrita de valores texto/numéricos em tags.

    @bold(Para maiores informações consulte a documentação da classe TEdit
    de seu ambiente de desenvolvimento.)
  }
  {$ELSE}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    Implements a Edit to read and write numeric/text values on tags.

    @bold(To get more informations, see the documentation of the class TEdit of
    your development environment.)
  }
  {$ENDIF}

  { THMIEdit }

  THMIEdit = class(TEdit, IHMIInterface)
  private
    FAfterSendValueToTag: TAfterSendStringValueToTagEvent;
    FAlignment:TAlignment;
    FBeforeSendValueToTag: TBeforeSendStringValueToTagEvent;
    FNumericKBShowDecimal: Boolean;
    FNumericKBShowMinus: Boolean;
    FTag:TPLCTag;
    FShowFocused:Boolean;
    FDefFontColor:TColor;
    FDefColor:TColor;
    FFontChangeEvent:TNotifyEvent;
    FBlockFontChange:Boolean;
    FBlockChange:Boolean;
    oldValue:TCaption;
    FPrefix, FSufix:TCaption;
    FNumberFormat:AnsiString;
    FSend:TSendChange;
    FFreezeValue:Boolean;
    FFreezedValue:Boolean;
    HasFocus:Boolean;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FOnScreenKeyboardBehavior:TOnScreenKeyboardBehavior;

    FNumericKB:TpsHMIfrmNumericKeyBoard;

    {$IFDEF PORTUGUES}
    //: Armazena se devem ser verificados limites minimos e máximos
    {$ELSE}
    //: Stores if must be checked the minimum and maximum limits.
    {$ENDIF}
    FEnableMin, FEnableMax:Boolean;

    {$IFDEF PORTUGUES}
    //: Armazena os valores de limites inferior e superior, apenas entrada.
    {$ELSE}
    //: Stores the minimum and maximum limits.
    {$ENDIF}
    FMinLimit, FMaxLimit:Double;

    FSecurityCode:UTF8String;
    procedure SetSecurityCode(sc:UTF8String);

    procedure RemoveHMITag(Sender:TObject);

    procedure SetFormat(f:AnsiString);
    function  GetText:TCaption;
    procedure RefreshTagValue(DataPtr:PtrInt);
    procedure SetSend(s:TSendChange);
    procedure SetShowFocused(f:Boolean);
    procedure RepaintFocus;
    function  GetColor:TColor;
    procedure FontChange(Sender: TObject);
    procedure SetPrefix(s:TCaption);
    procedure SetSufix(s:TCaption);

    procedure SendValue(txt:TCaption);

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);

    procedure SetMinLimit(v:Double);
    procedure SetMaxLimit(v:Double);

    procedure ShowScreenKeyboard;
    procedure ShowScreenKeyboardIfNeeded;
    procedure HideScreenKeyboard;

    procedure NumericKBClosed(Sender: TObject; var CloseAction: TCloseAction);
  protected
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

    //: @exclude
    procedure Click; override;
    //: @exclude
    procedure Change; override;
    //: @exclude
    procedure DoEnter; override;
    //: @exclude
    procedure DoExit; override;
    //: @exclude
    procedure KeyDown(var Key: Word; shift : TShiftState); override;

    //: @exclude
    procedure Loaded; override;
    //: @exclude
    procedure SetColor(c:TColor); {$IFDEF FPC}override; {$ENDIF}
    {$IFNDEF FPC}
    //: @exclude
    procedure CreateParams(var Params: TCreateParams); override;
    //: @exclude
    procedure SetAlignment(Value: TAlignment);
    {$ENDIF}
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    {$IFNDEF FPC}
    {$IFDEF PORTUGUES}
    //: Alinhamento do texto dentro do Edit. Somente Delphi. Lazarus já oferece esse recurso.
    {$ELSE}
    //: Alignment of the text . Only Delphi. The Lazarus IDE has this feature built-in.
    {$ENDIF}
    property Alignment:TAlignment read FAlignment write SetAlignment default taRightJustify;
    {$ENDIF}

    //: @exclude
    property Enabled:Boolean read FIsEnabled write SetEnabled;

    {$IFDEF PORTUGUES}
    {:
    @name diz para o controle quando um valor alterado deve ser escrito no Tag.
    @seealso(TSendChange)
    }
    {$ELSE}
    {:
    @name tell when the Edit will send the modified value to tag.
    @seealso(TSendChange)
    }
    {$ENDIF}
    property SendValueWhen:TSendChange read FSend write SetSend stored true default [scLostFocus, scPressEnter];

    {$IFDEF PORTUGUES}
    {:
    Caso o tag associado ao controle seja numérico, especifica a formatação
    numérica adotada.

    Para maiores informações procure sobre a função FormatFloat de seu ambiente
    de desenvolvimento.
    }
    {$ELSE}
    {:
    If the linked tag is a numeric tag, specifies the format of the number.

    More information, see the documentation of the function FormatFloat of your
    development environment.
    }
    {$ENDIF}
    property NumberFormat:AnsiString read FNumberFormat write SetFormat;

    {$IFDEF PORTUGUES}
    {:
    Especifica o tag que será usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    @seealso(TPLCStructItem)
    @seealso(TPLCString)
    }
    {$ELSE}
    {:
    Tag that will be linked with Edit control.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    @seealso(TPLCStructItem)
    @seealso(TPLCString)
    }
    {$ENDIF}
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;

    {$IFDEF PORTUGUES}
    {:
    @name retorna o texto que está sendo exibido pelo controle. Inclui sufixo,
    prefixo e formatação.
    }
    {$ELSE}
    {:
    @name returns the text showed on control. Includes the suffix, prefix and
    the number format.
    }
    {$ENDIF}
    property Text:TCaption read GetText stored false;

    {$IFDEF PORTUGUES}
    {:
    @name é a cor de fundo do controle.
    }
    {$ELSE}
    {:
    @name is the actual background color of the control.
    }
    {$ENDIF}
    property Color:TColor read GetColor Write SetColor;

    {$IFDEF PORTUGUES}
    {:
    @name faz com que o controle passe a cor do fundo para a cor da fonte e
    vice-versa quando ele estiver com o foco.

    @bold(Deixe essa propriedade @false quando sua aplicação estiver rodando
    sobre o toolkit GTK1.)
    }
    {$ELSE}
    {:
    If @name is @true, swaps the colors of the foreground and background when
    control has the focus.

    @bold(Keep the value of this property to @false when you are using GTK1
    widgetset.)
    }
    {$ENDIF}
    property ShowFocused:Boolean read FShowFocused write SetShowFocused default true;

    {$IFDEF PORTUGUES}
    {:
    @name é o texto que é exibido a esquerda (antes) do valor do tag quando o controle
    @bold(não tem o foco).
    }
    {$ELSE}
    {:
    @name is the text that will be show on the left (before) of the value of the
    tag @bold(when the control doesn't has the focus).
    }
    {$ENDIF}
    property Prefix:TCaption read FPrefix write SetPrefix;

    {$IFDEF PORTUGUES}
    {:
    @name é o texto que é exibido a direita (após) do valor do tag quando o controle
    @bold(não tem o foco). Útil para informar o tipo da grandeza exibida.
    }
    {$ELSE}
    {:
    @name is the text that will be show on the right (after) of the value of the
    tag @bold(when the control doesn't has the focus). Useful to show the
    engineering unit of the tag.
    }
    {$ENDIF}
    property Sufix:TCaption read FSufix write SetSufix;

    {$IFDEF PORTUGUES}
    {:
    Caso o valor de @name seja @true, faz com que o controle pare de atualizar
    o valor do tag no controle quando este tem o foco da aplicação
    }
    {$ELSE}
    {:
    If the value of @name is @true, stops the updates of the value of the tag
    when the control has de focus.
    }
    {$ENDIF}
    property FreezeValueOnFocus:Boolean read FFreezeValue write FFreezeValue default true;

    {$IFDEF PORTUGUES}
    //: Habilita/desabilita o limite minimo para entrada de dados.
    {$ELSE}
    //: Enables/disables the minimum limit.
    {$ENDIF}
    property EnableMinValue:Boolean read FEnableMin write FEnableMin stored true;

    {$IFDEF PORTUGUES}
    //: Habilita/desabilita o limite máximo para entrada de dados.
    {$ELSE}
    //: Enables/disables the maximum limit.
    {$ENDIF}
    property EnableMaxValue:Boolean read FEnableMax write FEnableMax stored true;

    {$IFDEF PORTUGUES}
    //: Limite minimo para entrada de dados.
    {$ELSE}
    //: Minimum value acceptable if the minimum limit is enabled.
    {$ENDIF}
    property MinValue:Double read FMinLimit write SetMinLimit;

    {$IFDEF PORTUGUES}
    //: Limite máximo para entrada de dados.
    {$ELSE}
    //: Maximum value acceptable if the maximum limit is enabled.
    {$ENDIF}
    property MaxValue:Double read FMaxLimit write SetMaxLimit;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;

    {$IFDEF PORTUGUES}
    {:
    Comportamento do controle com relação ao teclado virtual. A seleção do tipo
    do teclado irá ser feita baseada no tipo de tag, informações de limite e de
    formatação do edit.
    }
    {$ELSE}
    {:
    On screen keyboard control behavior. The keyboard type will be made
    automacally based on tag type, limits and on NumberFormat property.
    }
    {$ENDIF}
    property ScreenKeyboardBehavior:TOnScreenKeyboardBehavior read FOnScreenKeyboardBehavior write FOnScreenKeyboardBehavior default oskbDisabled;

    property ScreenNumericKBShowMinus:Boolean read FNumericKBShowMinus write FNumericKBShowMinus default false;
    property ScreenNumericKBShowDecimal:Boolean read FNumericKBShowDecimal write FNumericKBShowDecimal default false;

    {$IFDEF PORTUGUES}
    //: Evento disparado antes do HMIEdit enviar um valor ao tag associado
    {$ELSE}
    //: Event triggered before HMIEdit send a value to linked tag.
    {$ENDIF}
    property BeforeSendAValueToTag:TBeforeSendStringValueToTagEvent read FBeforeSendValueToTag write FBeforeSendValueToTag;

    {$IFDEF PORTUGUES}
    //: Evento disparado quando o HMIEdit enviou um valor ao tag associado
    {$ELSE}
    //: Event triggered when the HMIEdit sent a value to linked tag.
    {$ENDIF}
    property AfterSendValueToTag:TAfterSendStringValueToTagEvent read FAfterSendValueToTag write FAfterSendValueToTag;
  end;

implementation

uses hsstrings, ControlSecurityManager;

constructor THMIEdit.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  inherited Color:=clWindow;
  Font.Color     :=clWindowText;
  FAlignment := taRightJustify;
  FIsEnabled := inherited Enabled;
  FSend := [scLostFocus, scPressEnter];
  FIsEnabled:=true;
  if (csDesigning in ComponentState) then begin
    inherited Text := SWithoutTag;
    Modified := false;
  end;

  FFontChangeEvent := Font.OnChange;
  Font.OnChange := @FontChange;
  FShowFocused := true;
  FFreezeValue := true;
  FNumberFormat := '#0.0';
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor  THMIEdit.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

{$IFNDEF FPC}
procedure THMIEdit.CreateParams(var Params: TCreateParams);
const Alignments: array[TAlignment] of Longint =
      (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or ES_MULTILINE or
                  Alignments[FAlignment];
end;

procedure THMIEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;
{$ENDIF}

procedure THMIEdit.NumericKBClosed(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FNumericKB:=nil; //numerickb.DoClose force CloseAction to caFree, so free
                   //isn´t need.
end;

procedure THMIEdit.SetSecurityCode(sc: UTF8String);
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

procedure THMIEdit.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity :=a;
  SetEnabled(FIsEnabled);
end;

procedure THMIEdit.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

function THMIEdit.GetControlSecurityCode:UTF8String;
begin
   Result:=FSecurityCode;
end;

procedure THMIEdit.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure THMIEdit.Click;
begin
  ShowScreenKeyboardIfNeeded;
  inherited Click;
end;

procedure THMIEdit.Loaded;
begin
  inherited Loaded;
  if not HasFocus then begin
    FDefFontColor := Font.Color;
    FDefColor := inherited Color;
  end;
end;

procedure THMIEdit.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  //
  //check if the tag is valid.
  if (t<>nil) and ((not Supports(t, ITagInterface)) and (not Supports(t, ITagNumeric)) and (not Supports(t, ITagString))) then
     raise Exception.Create(SinvalidTag);

  //se ja estou associado a um tag, remove
  //
  //if the control is linked with some tag, removes the link.
  if FTag<>nil then begin
    FTag.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link the control with the new tag.
  if t<>nil then begin
    t.AddWriteFaultHandler(@WriteFaultCallBack);
    t.AddTagChangeHandler(@TagChangeCallBack);
    t.AddRemoveTagHandler(@RemoveTagCallBack);
    FTag := t;
    RefreshTagValue(0);
  end;
  FTag := t;

  if (FTag=nil) and (csDesigning in ComponentState) then begin
    inherited Text := SWithoutTag;
    Modified := false;
  end;

end;

function  THMIEdit.GetHMITag:TPLCTag;
begin
  Result := FTag;
end;

procedure THMIEdit.SetFormat(f:AnsiString);
begin
  FNumberFormat := f;
  RefreshTagValue(0);
end;

procedure THMIEdit.RemoveHMITag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

function  THMIEdit.GetText:TCaption;
begin
  result := inherited Text;
end;

procedure THMIEdit.SetSend(s:TSendChange);
begin
  FSend := s;
end;

procedure THMIEdit.SetShowFocused(f:Boolean);
begin
   FShowFocused := f;
   RepaintFocus;
end;

procedure THMIEdit.RepaintFocus;
begin
  if [csReading, csLoading, csDesigning]*ComponentState<>[] then
     exit;

  FBlockFontChange := true;
  if FShowFocused then begin
    if HasFocus then begin
       inherited Color := FDefFontColor;
       Font.Color := FDefColor;
    end else begin
       inherited Color := FDefColor;
       Font.Color := FDefFontColor;
    end;
  end else begin
    inherited Color := FDefColor;
    Font.Color := FDefFontColor;
  end;
  FBlockFontChange := false;
end;

function  THMIEdit.GetColor:TColor;
begin
  Result := inherited Color;
end;

procedure THMIEdit.SetColor(c:TColor);
begin
  {$IFDEF FPC}
  inherited SetColor(c);
  {$ELSE}
  inherited Color := c;
  {$ENDIF}
  if not FBlockFontChange then
    FDefColor := c;
end;

procedure THMIEdit.SetPrefix(s:TCaption);
begin
  FPrefix := s;
  RefreshTagValue(0);
end;

procedure THMIEdit.SetSufix(s:TCaption);
begin
  FSufix := s;
  RefreshTagValue(0);
end;

procedure THMIEdit.SetMinLimit(v:Double);
begin
  if ([csReading, csLoading]*ComponentState=[]) and  (v>=FMaxLimit) then
    raise Exception.Create(SminMustBeLessThanMax);

  FMinLimit:=v;
end;

procedure THMIEdit.SetMaxLimit(v:Double);
begin
  if ([csReading, csLoading]*ComponentState=[]) and  (v<=FMinLimit) then
    raise Exception.Create(SmaxMustBeGreaterThanMin);

  FMaxLimit:=v;
end;

procedure THMIEdit.ShowScreenKeyboard;
var
  showMinus: Boolean;
  showDecPoint: Boolean;
begin
  if ReadOnly then exit;
  if FTag <> nil then begin
    if Supports(FTag,ITagNumeric) then begin
      if Assigned(FNumericKB) then begin
        FNumericKB.ShowOnTop;
        Exit;
      end;
      showMinus   :=(FEnableMin AND (FMinLimit<0)) or ((FEnableMin=false) and FNumericKBShowMinus);
      showDecPoint:=(Pos('.', FNumberFormat)>0) or FNumericKBShowDecimal;
      FNumericKB:=TpsHMIfrmNumericKeyBoard.Create(Self,Self,showMinus,showDecPoint);
      FNumericKB.OnClose:=@NumericKBClosed;
      FNumericKB.ShowAlongsideOfTheTarget;
      exit;
    end;
  end;
end;

procedure THMIEdit.ShowScreenKeyboardIfNeeded;
begin
  if FOnScreenKeyboardBehavior=oskbEnabled then
    ShowScreenKeyboard;
end;

procedure THMIEdit.HideScreenKeyboard;
begin
  if assigned(FNumericKB) and (FNumericKB.Target=Self) then begin
    FNumericKB.Close;
    FNumericKB:=nil;
  end;
end;

procedure THMIEdit.FontChange(Sender: TObject);
begin
  if Assigned(FFontChangeEvent) then
    FFontChangeEvent(Sender);

  if FBlockFontChange then exit;

  FDefFontColor := Font.Color;
end;

procedure THMIEdit.RefreshTagValue(DataPtr: PtrInt);
begin
  if ([csReading]*ComponentState<>[]) or (FTag=nil) or Modified then begin
    exit;
  end;

  if (FTag<>nil) AND Supports(FTag, ITagInterface) then begin
    FBlockChange := true;
    if HasFocus then begin
      if (FFreezeValue) then begin
        if (not FFreezedValue) then begin
          inherited Text := (FTag as ITagInterface).GetValueAsText('','',FNumberFormat);
          FFreezedValue := true;
        end;
      end else begin
        inherited Text := (FTag as ITagInterface).GetValueAsText('','',FNumberFormat);
      end;
    end else begin
      inherited Text := (FTag as ITagInterface).GetValueAsText(FPrefix,FSufix,FNumberFormat);
    end;
    oldValue := inherited Text;
    Modified := false;
    FBlockChange := false;
    exit;
  end;
end;

procedure THMIEdit.SendValue(txt: TCaption);
var
  x:Double;

  procedure DoAfterSendValue;
  begin
    if Assigned(FAfterSendValueToTag) then
      FAfterSendValueToTag(Self,Txt);
  end;

  function SendIt:Boolean;
  begin
    if Assigned(FBeforeSendValueToTag) then
      FBeforeSendValueToTag(Self,Txt,Result)
    else
      Result:=true;
  end;

begin
  if (csReading in ComponentState) or
     (FTag=nil) or
     (not Modified) then
     exit;

  if not SendIt then exit;

  if Supports(FTag, ITagNumeric) then begin
    x:=StrToFloat(Txt);
    if (FEnableMax and (x>FMaxLimit)) or (FEnableMin and (x<FMinLimit)) then
      raise Exception.Create(SoutOfBounds);

    (FTag as ITagNumeric).Value := x;
    DoAfterSendValue;
    exit;
  end;

  if Supports(FTag, ITagString) then begin
    (FTag as ITagString).Value := Txt;
    DoAfterSendValue;
    exit;
  end;
  
  if Supports(FTag, ITagInterface) then begin
    (FTag as ITagInterface).ValueVariant := Txt;
    DoAfterSendValue;
    exit;
  end;
end;

procedure THMIEdit.KeyDown(var Key: Word; shift : TShiftState);
begin
  if ((scPressEnter in FSend) and (key=VK_RETURN)) or
     ((scPressESC in FSend) and (key=VK_ESCAPE)) then begin
     SendValue(Text);
     FFreezedValue := false;
     Modified := false;
  end;

  if ( (not (scPressEnter in FSend)) and (key=VK_RETURN)) or
     ( (not (scPressESC in FSend)) and (key=VK_ESCAPE)) then begin
     Modified := false;
     FFreezedValue := false;
     RefreshTagValue(0);
  end;

  inherited KeyDown(Key, Shift);
end;

procedure THMIEdit.DoExit;
begin
  HasFocus := false;
  RepaintFocus;

  FFreezedValue := false;

  if (scLostFocus in FSend) then begin
     SendValue(Text);
     Modified := false;
  end;

  Modified := false;
  RefreshTagValue(0);

  HideScreenKeyboard;

  inherited DoExit;
end;

procedure THMIEdit.DoEnter;
begin
  oldValue := Text;
  HasFocus := true;
  RepaintFocus;
  RefreshTagValue(0);
  SelectAll;
  inherited DoEnter;
end;

procedure THMIEdit.Change;
var
  itag:ITagInterface;
begin
  if FBlockChange then exit;

  if (FTag<>nil) AND Supports(FTag, ITagInterface) then begin
    itag := (FTag as ITagInterface);
    if (itag<>nil) then begin
      if itag.IsValidValue(Text) then
        oldValue := Text
      else begin
        FBlockChange:=true;
        inherited Text := oldValue;
        FBlockChange:=false;
        SelectAll;
      end;
    end;
  end;

  if (scAnyChange in FSend) then
    SendValue(Text);

  inherited Change;
end;

procedure THMIEdit.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIEdit.TagChangeCallBack(Sender: TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    Application.QueueAsyncCall(@RefreshTagValue,0);
end;

procedure THMIEdit.RemoveTagCallBack(Sender: TObject);
begin
  if Ftag=Sender then
    FTag:=nil;
end;

end.
