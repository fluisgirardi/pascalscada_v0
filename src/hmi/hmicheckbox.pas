{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit que implementa um controle CheckBox ligado a um Tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a CheckBox control linked with a tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMICheckBox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, PLCTag, HMITypes, Graphics,
  ProtocolTypes, Tag;

type
  {$IFDEF PORTUGUES}
  {:
   @name é a classe de um controle booleano em forma de CheckBox que lê e escreve valores em Tags.
   @author(Fabio Luis Girardi <fabio@pascalscada.com>)

   @bold(Para maiores informações consulte a documentação da classe TCheckBox
   de seu ambiente de desenvolvimento.)
  }
  {$ELSE}
  {:
   @name is the class of the boolean control with appearance of an CheckBox that
   reads and writes values in tags.
   @author(Fabio Luis Girardi <fabio@pascalscada.com>)

   @bold(To get more information, see the documentation of the TCheckBox of your
   development environment.)
  }
  {$ENDIF}

  { THMICheckBox }

  THMICheckBox = class(TCheckBox, IHMIInterface)
  private
    FAfterSendValueToTag: TAfterSendNumericValueToTagEvent;
    FBeforeSendValueToTag: TBeforeSendNumericValueToTagEvent;
    FTag:TPLCTag;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FValueTrue, FValueFalse:Double;
    FWriteTrue, FWriteFalse:Boolean;
    FColorFalse, FColorTrue, FColorGrayed:TColor;
    FCaptionFalse, FCaptionTrue, FCaptionGrayed:TCaption;
    FFontFalse, FFontTrue, FFontGrayed:TFont;
    FOtherValues:TOtherValues;

    FSecurityCode:UTF8String;
    procedure SetSecurityCode(sc:UTF8String);

    function  GetTagValue:Double;
    procedure SetCaptionFalse (v:TCaption);
    procedure SetCaptionTrue  (v:TCaption);
    procedure SetCaptionGrayed(v:TCaption);
    procedure SetColorFalse(c:TColor);
    procedure SetColorTrue(c:TColor);
    procedure SetColorGrayed(c:TColor);

    procedure SetFontFalse(f:TFont);
    procedure SetFontTrue(f:TFont);
    procedure SetFontGrayed(f:TFont);

    function  GetState:TCheckBoxState;

    procedure SetWriteTrue(v:Boolean);
    procedure SetWriteFalse(v:Boolean);
    procedure SetValueTrue(v:Double);
    procedure SetValueFalse(v:Double);
    procedure SetOtherValues(v:TOtherValues);
    procedure RefreshTagValue(x:Double);
    procedure FontChange(Sender:TObject);

    procedure SetCaption(c:TCaption);
    procedure SetFont(f:TFont);

    function  GetAllowGrayed:Boolean;

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
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

    {$IFDEF FPC}
    //: @exclude
    procedure DoOnChange; override;
    {$ELSE}
    //: @exclude
    procedure Toggle; override;
    {$ENDIF}
    //: @exclude
    function  IntGetColor:TColor;
    //: @exclude
    procedure IntSetColor(c:TColor);
    //: @exclude
    function GetChecked: Boolean; override;
    //: @exclude
    procedure SetChecked(Value: Boolean); override;
    //: @exclude
    procedure UpdateTagValue;

    procedure Click; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    {$IFDEF FPC}
    //: @exclude
    procedure EditingDone; override;
    procedure RefreshCheckBox(Data: PtrInt);
    procedure UpdateStatus;
    {$ENDIF}
  published
    {$IFDEF PORTUGUES}
    {:
    @name informa se o controle aceita o estado intermediário (Grayed).
    O valor dessa propriedade depende principalmente da implementação que está
    sendo usada (Delphi ou Lazarus).
    }
    {$ELSE}
    {:
    @name tells if the control accepts a grayed state. The value of this property
    depends mainly of your development environment (Lazarus or Delphi).
    }
    {$ENDIF}
    property AllowGrayed:Boolean read GetAllowGrayed stored false;

    {$IFDEF PORTUGUES}
    {:
    Use @name para ver/setar o estado do controle. Se escrito através dessa
    propriedade, o valor ValueTrue/ValueFalse é escrito no Tag caso as
    propriedades WriteTrueValue/WriteFalseValue estejam em @true
    }
    {$ELSE}
    {:
    Use the @name property to read/write the state of the control. Changes on
    this property will write the value of property ValueTrue/ValueFalse on the
    tag if the the respective property WriteTrueValue/WriteFalseValue is set to
    @true.
    }
    {$ENDIF}
    property Checked stored false;

    {$IFDEF PORTUGUES}
    {:
    Tag numérico usado pelo controle. As alterações do valor do tag alteram a
    aparencia do controle, assim como as alterações do controle podem alterar
    o valor do tag.
    @seealso(TPLCTag)
    @seealso(TPLCBlockElement)
    @seealso(TPLCStructItem)
    @seealso(TPLCTagNumber)
    }
    {$ELSE}
    {:
    Numeric tag link. The value changes of the tag will update the control
    appearance and the changes made on control can update the tag value.
    @seealso(TPLCTag)
    @seealso(TPLCBlockElement)
    @seealso(TPLCStructItem)
    @seealso(TPLCTagNumber)
    }
    {$ENDIF}
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;

    {$IFDEF PORTUGUES}
    {:
    @name mostra o Caption atual do controle. Se esta propriedade for modificada,
    escreve seu valor em CaptionTrue, CaptionFalse e CaptionGrayed para
    agilizar sua configuração.
    @seealso(CaptionTrue)
    @seealso(CaptionFalse)
    @seealso(CaptionGrayed)
    }
    {$ELSE}
    {:
    @name shows the actual caption of the control. If this property is modified,
    it changes the properties CaptionFalse, CaptionTrue and CaptionGrayed with the
    same value, to accelerate your configuration.
    @seealso(CaptionTrue)
    @seealso(CaptionFalse)
    @seealso(CaptionGrayed)
    }
    {$ENDIF}
    property Caption write SetCaption stored false;

    {$IFDEF PORTUGUES}
    {:
    @name é texto que será mostrado no controle caso ele esteja desmarcado.
    @seealso(Caption)
    }
    {$ELSE}
    {:
    @name is the text that will be show if the checkbox is unchecked.
    @seealso(Caption)
    }
    {$ENDIF}
    property CaptionFalse:TCaption read FCaptionFalse write SetCaptionFalse stored true nodefault;

    {$IFDEF PORTUGUES}
    {:
    @name é texto que será mostrado no controle caso ele esteja marcado.
    @seealso(Caption)
    }
    {$ELSE}
    {:
    @name is the text that will be show if the checkbox is checked.
    @seealso(Caption)
    }
    {$ENDIF}
    property CaptionTrue:TCaption read FCaptionTrue write SetCaptionTrue stored true nodefault;

    {$IFDEF PORTUGUES}
    {:
    @name é texto que será mostrado no controle caso ele esteja no meio-termo
    (nem marcado e nem desmarcado).
    @seealso(Caption)
    }
    {$ELSE}
    {:
    @name is the text that will be show if the checkbox is grayed.
    @seealso(Caption)
    }
    {$ENDIF}
    property CaptionGrayed:TCaption read FCaptionGrayed write SetCaptionGrayed stored true nodefault;

    {$IFDEF PORTUGUES}
    {:
    @name mostra a cor atual do controle. Se esta propriedade for modificada,
    escreve seu valor em ColorTrue, ColorFalse e ColorGrayed, para agilizar sua
    configuração.
    @seealso(ColorTrue)
    @seealso(ColorFalse)
    @seealso(ColorGrayed)
    }
    {$ELSE}
    {:
    @name shows the actual color of the control. If this property is modified,
    it changes the properties ColorFalse, ColorTrue and ColorGrayed with the
    same value, to accelerate your configuration.
    @seealso(ColorTrue)
    @seealso(ColorFalse)
    @seealso(ColorGrayed)
    }
    {$ENDIF}
    property Color read IntGetColor write IntSetColor stored false;

    {$IFDEF PORTUGUES}
    {:
    @name é a cor do controle caso ele esteja desmarcado.
    @seealso(Color)
    }
    {$ELSE}
    {:
    @name is the color of the checkbox if it's unchecked.
    @seealso(Color)
    }
    {$ENDIF}
    property ColorFalse:TColor read FColorFalse write SetColorFalse stored true default clBtnFace;

    {$IFDEF PORTUGUES}
    {:
    @name é a cor do controle caso ele esteja marcado.
    @seealso(Color)
    }
    {$ELSE}
    {:
    @name is the color of the checkbox if it's checked.
    @seealso(Color)
    }
    {$ENDIF}
    property ColorTrue:TColor read FColorTrue write SetColorTrue stored true default clBtnFace;

    {$IFDEF PORTUGUES}
    {:
    @name é a cor do controle caso ele esteja no meio-termo (nem marcado e nem
    desmarcado).
    @seealso(Color)
    }
    {$ELSE}
    {:
    @name is the color of the checkbox if it's grayed.
    @seealso(Color)
    }
    {$ENDIF}
    property ColorGrayed:TColor read FColorGrayed write SetColorGrayed stored true default clBtnFace;

    {$IFDEF PORTUGUES}
    {:
    @name mostra o FontTrue, FontFalse ou FontGrayed de acordo com o valor do tag
    ou escreve seu valor em FontTrue, FontFalse e FontGrayed  para agilizar sua
    configuração.
    @seealso(FontTrue)
    @seealso(FontFalse)
    @seealso(FontGrayed)
    }
    {$ELSE}
    {:
    @name shows the actual font of the control. If this property is modified,
    it changes the properties FontFalse, FontTrue and FontGrayed with the
    same value, to accelerate your configuration.
    @seealso(FontTrue)
    @seealso(FontFalse)
    @seealso(FontGrayed)
    }
    {$ENDIF}
    property Font write SetFont stored false;

    {$IFDEF PORTUGUES}
    {:
    @name é a fonte que será usada pelo controle caso ele esteja desmarcado.
    @seealso(Font)
    }
    {$ELSE}
    {:
    @name is the font of checkbox if it's unchecked.
    @seealso(Font)
    }
    {$ENDIF}
    property FontFalse:TFont read FFontFalse write SetFontFalse stored true;

    {$IFDEF PORTUGUES}
    {:
    @name é a fonte que será usada pelo controle caso ele esteja marcado.
    @seealso(Font)
    }
    {$ELSE}
    {:
    @name is the font of checkbox if it's checked.
    @seealso(Font)
    }
    {$ENDIF}
    property FontTrue:TFont read FFontTrue write SetFontTrue stored true;

    {$IFDEF PORTUGUES}
    {:
    @name é a fonte que será usada pelo controle caso ele esteja  no meio-termo
    (nem marcado e nem desmarcado).
    @seealso(Font)
    }
    {$ELSE}
    {:
    @name is the font of the checkbox if it's grayed.
    @seealso(Font)
    }
    {$ENDIF}
    property FontGrayed:TFont read FFontGrayed write SetFontGrayed stored true;

    {$IFDEF PORTUGUES}
    {:
    Diz como o controle irá se comportar caso o valor do Tag não seja igual a
    ValueFalse e nem a ValueTrue.

    @seealso(TOtherValues)
    }
    {$ELSE}
    {:
    The value of this property tells how the checkbox will handle tag values that
    are others than ValueFalse and ValueTrue.

    @seealso(TOtherValues)
    }
    {$ENDIF}
    property OtherValuesIS:TOtherValues read FOtherValues write SetOtherValues stored true default IsGrayed;

    {$IFDEF PORTUGUES}
    {:
    Caso o valor do Tag seja igual a @name, o controle é desmarcado.
    }
    {$ELSE}
    {:
    If the value of Tag is equal to @name, the checkbox is unchecked.
    }
    {$ENDIF}
    property ValueFalse:Double read FValueFalse write SetValueFalse stored true;

    {$IFDEF PORTUGUES}
    {:
    Caso o valor do Tag seja igual a @name, o controle é marcado.
    }
    {$ELSE}
    {:
    If the value of the Tag is equal to @name, the checkbox is checked.
    }
    {$ENDIF}
    property ValueTrue:Double read FValueTrue write SetValueTrue stored true;

    {$IFDEF PORTUGUES}
    {:
    @name diz que qualquer ação feita pelo usuário que marque o controle, deve
    também escrever ValueTrue no tag associado.
    }
    {$ELSE}
    {:
    If @name is true, all actions that check the control will write the property
    ValueTrue value on the linked tag.
    }
    {$ENDIF}
    property WriteTrueValue:Boolean read FWriteTrue write SetWriteTrue stored true default true;

    {$IFDEF PORTUGUES}
    {:
    @name diz que qualquer ação feita pelo usuário que desmarque o controle, deve
    também escrever ValueFalse no tag associado.
    }
    {$ELSE}
    {:
    If @name is true, all actions that uncheck the control will write the property
    ValueFalse value on the linked tag.
    }
    {$ENDIF}
    property WriteFalseValue:Boolean read FWriteFalse write SetWriteFalse stored true default true;

    {$IFDEF PORTUGUES}
    //: Informa o atual estado do controle (marcado, desmarcado, acinzentado).
    {$ELSE}
    //: Tells the actual state of the control (checked, unchecked or grayed).
    {$ENDIF}
    property State:TCheckBoxState read GetState stored false nodefault;

    //: @exclude
    property Enabled:Boolean read FIsEnabled write SetEnabled;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;

    {$IFDEF PORTUGUES}
    //: Evento disparado antes do HMIEdit enviar um valor ao tag associado
    {$ELSE}
    //: Event triggered before HMIEdit send a value to linked tag.
    {$ENDIF}
    property BeforeSendAValueToTag:TBeforeSendNumericValueToTagEvent read FBeforeSendValueToTag write FBeforeSendValueToTag;

    {$IFDEF PORTUGUES}
    //: Evento disparado quando o HMIEdit enviou um valor ao tag associado
    {$ELSE}
    //: Event triggered when the HMIEdit sent a value to linked tag.
    {$ENDIF}
    property AfterSendValueToTag:TAfterSendNumericValueToTagEvent read FAfterSendValueToTag write FAfterSendValueToTag;
  end;

implementation

uses hsstrings, ControlSecurityManager, Forms;

constructor THMICheckBox.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FIsEnabled := inherited Enabled;
  if csDesigning in componentState then begin
    FValueTrue := 1;
    FValueFalse := 0;
  end else begin
    FValueTrue := 0;
    FValueFalse := 0;
  end;

  inherited AllowGrayed := false;

  FIsEnabled:=true;
  FFontFalse  := TFont.Create;
  FFontFalse.OnChange := @FontChange;
  FFontTrue   := TFont.Create;
  FFontTrue.OnChange := @FontChange;
  FFontGrayed := TFont.Create;
  FFontGrayed.OnChange := @FontChange;
  FColorTrue := clBtnFace;
  FColorFalse:= clBtnFace;
  FColorGrayed:= clBtnFace;
  FOtherValues := IsGrayed;
  FWriteTrue := true;
  FWriteFalse := true;
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMICheckBox.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);
  FreeAndNil(FFontFalse);
  FreeAndNil(FFontTrue);
  FreeAndNil(FFontGrayed);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

{$IFDEF FPC}
procedure THMICheckBox.EditingDone;
begin
  UpdateTagValue;

  inherited EditingDone;
end;

procedure THMICheckBox.RefreshCheckBox(Data: PtrInt);
begin
  if ([csReading]*ComponentState<>[]) or (FTag=nil) then begin
    exit;
  end;

  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.UpdateStatus;
begin
  RefreshTagValue(GetTagValue);
end;

{$ENDIF}

procedure THMICheckBox.SetHMITag(t:TPLCTag);
begin
  //se o tag é um tag numerico.
  //check if the tag is valid.
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  //if the control is linked with some tag, remove the link.
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
    RefreshTagValue(GetTagValue);
  end;
  FTag := t;
end;

function  THMICheckBox.GetHMITag:TPLCTag;
begin
   Result := FTag;
end;

function THMICheckBox.GetControlSecurityCode:UTF8String;
begin
   Result:=FSecurityCode;
end;

procedure THMICheckBox.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure THMICheckBox.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMICheckBox.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure THMICheckBox.RefreshTagValue(x:Double);
begin
  if csDestroying in ComponentState then exit;

  if x=FValueTrue then begin
    inherited State := cbChecked;
    inherited Font.Assign(FFontTrue);
    inherited Color := FColorTrue;
    inherited Caption := FCaptionTrue;
  end else begin
    if x=FValueFalse then begin
      inherited State := cbUnchecked;
      inherited Font.Assign(FFontFalse);
      inherited Color := FColorFalse;
      inherited Caption := FCaptionFalse;
    end else begin
      case FOtherValues of
        isChecked: begin
          inherited State := cbChecked;
          inherited Font.Assign(FFontTrue);
          inherited Color := FColorTrue;
          inherited Caption := FCaptionTrue;
        end;
        isUnchecked: begin
          inherited State := cbUnchecked;
          inherited Font.Assign(FFontFalse);
          inherited Color := FColorFalse;
          inherited Caption := FCaptionFalse;
        end;
        IsGrayed: begin
          inherited State := cbGrayed;
          inherited Font.Assign(FFontGrayed);
          inherited Color := FColorGrayed;
          inherited Caption := FCaptionGrayed;
        end;
      end;
    end;
  end;
end;

procedure THMICheckBox.SetWriteTrue(v:Boolean);
begin
  if (inherited GetChecked) and v then begin
    if (FTag<>nil) AND Supports(FTag, ITagNumeric) then
      (FTag as ITagNumeric).Value:=FValueTrue;
  end;
  FWriteTrue := v
end;

procedure THMICheckBox.SetWriteFalse(v:Boolean);
begin
  if (not (inherited GetChecked)) and v then begin
    if (FTag<>nil) AND Supports(FTag, ITagNumeric) then
      (FTag as ITagNumeric).Value:=FValueFalse;
  end;
  FWriteFalse := v
end;

procedure THMICheckBox.SetValueTrue(v:Double);
begin
  if ((ComponentState*[csReading, csLoading])=[]) and (v=FValueFalse) then
    raise Exception.Create(StheValueMustBeDifferentOfValueFalseProperty);

  if (v<>FValueTrue) and ((FTag<>nil) and Supports(FTag, ITagNumeric))then begin
    if GetChecked and FWriteTrue then begin
      (FTag as ITagNumeric).Value:=v;
    end;
  end;
  FValueTrue := v;
end;

procedure THMICheckBox.SetValueFalse(v:Double);
begin
  if ((ComponentState*[csReading, csLoading])=[]) and (v=FValueTrue) then
    raise Exception.Create(StheValueMustBeDifferentOfValueTrueProperty);

  if (v<>FValueFalse) and ((FTag<>nil) and Supports(FTag, ITagNumeric)) then begin
    if (not GetChecked) and FWriteFalse then begin
      (FTag as ITagNumeric).Value:=v;
    end;
  end;

  FValueFalse := v;
end;

function THMICheckBox.GetChecked: Boolean;
begin
  if FTag=nil then
    Result := inherited GetChecked
  else
    Result := (GetTagValue=FValueTrue);
end;

procedure THMICheckBox.SetChecked(Value: Boolean);
begin
  inherited SetChecked(Value);
  UpdateTagValue;
end;

procedure THMICheckBox.UpdateTagValue;
  procedure DoAfterSendValue(avalue:Double);
  begin
    if Assigned(FAfterSendValueToTag) then
      FAfterSendValueToTag(Self,avalue);
  end;

  function SendIt(avalue:Double):Boolean;
  begin
    if Assigned(FBeforeSendValueToTag) then
      FBeforeSendValueToTag(Self,avalue,Result)
    else
      Result:=true;
  end;
begin
  if (csReading in ComponentState) or (csLoading in ComponentState) or (FTag=nil) then
    exit;

  if ((FTag<>nil) and Supports(FTag, ITagNumeric)) then
    if State=cbChecked then begin
      if FWriteTrue and SendIt(FValueTrue) then begin
        (FTag as ITagNumeric).Value := FValueTrue;
        DoAfterSendValue(FValueTrue);
      end;
    end else begin
      if FWriteFalse and SendIt(FValueFalse) then begin
        (FTag as ITagNumeric).Value := FValueFalse;
        DoAfterSendValue(FValueFalse);
      end;
    end;
end;

procedure THMICheckBox.Click;
begin
  if Name='batata' then
    inherited Click
  else
    inherited Click
end;

{$IFDEF FPC}
procedure THMICheckBox.DoOnChange;
begin
  if [csLoading,csDestroying]*ComponentState<>[] then begin
    exit;
  end;

  EditingDone;

  if Assigned(OnChange) then
    OnChange(Self);
end;

{$ELSE}
procedure THMICheckBox.Toggle;
begin
  inherited Toggle;
  UpdateTagValue;
end;
{$ENDIF}

procedure THMICheckBox.SetSecurityCode(sc: UTF8String);
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

function THMICheckBox.GetTagValue:Double;
begin
  Result := 0;
  if Assigned(FTag) AND Supports(FTag, ITagNumeric) then
    Result := (FTag as ITagNumeric).Value;
end;


procedure THMICheckBox.SetCaptionFalse(v:TCaption);
begin
  if v=FCaptionFalse then exit;

  FCaptionFalse := v;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetCaptionTrue(v:TCaption);
begin
  if v=FCaptionTrue then exit;

  FCaptionTrue := v;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetCaptionGrayed(v:TCaption);
begin
  if v=FCaptionGrayed then exit;

  FCaptionGrayed := v;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetColorFalse(c:TColor);
begin
  if c=FColorFalse then exit;

  FColorFalse := c;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetColorTrue(c:TColor);
begin
  if c=FColorTrue then exit;

  FColorTrue := c;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetColorGrayed(c:TColor);
begin
  if c=FColorGrayed then exit;

  FColorGrayed := c;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.FontChange(Sender:TObject);
begin
  if csDestroying in ComponentState then exit;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetOtherValues(v:TOtherValues);
begin
  FOtherValues := v;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetFontFalse(f:TFont);
begin
  FFontFalse.Assign(f);
end;

procedure THMICheckBox.SetFontTrue(f:TFont);
begin
  FFontTrue.Assign(f);
end;

procedure THMICheckBox.SetFontGrayed(f:TFont);
begin
  FFontGrayed.Assign(f);
end;

function  THMICheckBox.GetState:TCheckBoxState;
begin
   Result := inherited State;
end;

procedure THMICheckBox.SetCaption(c:TCaption);
begin
  if not (csDesigning in ComponentState) then exit;

  FCaptionFalse := c;
  FCaptionGrayed := c;
  FCaptionTrue := c;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.IntSetColor(c:TColor);
begin
  if not (csDesigning in ComponentState) then exit;

  FColorFalse := c;
  FColorGrayed := c;
  FColorTrue := c;
  RefreshTagValue(GetTagValue);
end;

function  THMICheckBox.IntGetColor:TColor;
begin
  Result := inherited Color;
end;

procedure THMICheckBox.SetFont(f:TFont);
begin
  if not (csDesigning in ComponentState) then exit;

  FFontFalse.Assign(f);
  FFontGrayed.Assign(f);
  FFontTrue.Assign(f);
  RefreshTagValue(GetTagValue);
end;

function  THMICheckBox.GetAllowGrayed:Boolean;
begin
  result := inherited AllowGrayed;
end;

procedure THMICheckBox.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMICheckBox.TagChangeCallBack(Sender: TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    Application.QueueAsyncCall(@RefreshCheckBox,0);
end;

procedure THMICheckBox.RemoveTagCallBack(Sender: TObject);
begin
  if Ftag=Sender then
    FTag:=nil;
end;

end.
