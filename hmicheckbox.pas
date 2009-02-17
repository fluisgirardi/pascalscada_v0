//: Unit que implementa um controle CheckBox ligado a um Tag.
unit HMICheckBox;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, StdCtrls, PLCTag, HMITypes, Graphics,
  ProtocolTypes;

type
  //: @name é um controle booleano em forma de CheckBox que lê e escreve valores em Tags.
  THMICheckBox = class(TCheckBox, IHMIInterface)
  private
    FTag:TPLCTag;
    FValueTrue, FValueFalse:Double;
    FWriteTrue, FWriteFalse:Boolean;
    FColorFalse, FColorTrue, FColorGrayed:TColor;
    FCaptionFalse, FCaptionTrue, FCaptionGrayed:string;
    FFontFalse, FFontTrue, FFontGrayed:TFont;
    FOtherValues:TOtherValues;
    FIsEnabled:Boolean;

    procedure RemoveHMITag(Sender:TObject);

    function  GetTagValue:Double;
    procedure SetCaptionFalse(v:String);
    procedure SetCaptionTrue(v:String);
    procedure SetCaptionGrayed(v:String);
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
    procedure SetHMITag(t:TPLCTag);
    function  GetHMITag:TPLCTag;
    procedure RefreshTagValue(x:Double);
    procedure HMINotifyChangeCallback(Sender:TObject);
    procedure FontChange(Sender:TObject);

    procedure SetCaption(c:TCaption);
    procedure SetColor(c:TColor);
    procedure SetFont(f:TFont);

    function  GetAllowGrayed:Boolean;
    procedure RefreshHMISecurity;

    procedure SetHMIEnabled(v:Boolean);
    function  GetHMIEnabled:Boolean;
  protected
    {$IFDEF FPC}
    //: @exclude
    BlockOnChange:Boolean;
    //: @exclude
    LazLastState:TCheckBoxState;
    //: @exclude
    procedure DoOnChange; override;
    {$ELSE}
    //: @exclude
    procedure Toggle; override;
    {$ENDIF}
    //: @exclude
    function GetChecked: Boolean; override;
    //: @exclude
    procedure SetChecked(Value: Boolean); override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    {:
    @name informa se o controle aceita o estado intermediário (Grayed).
    O valor dessa propriedade depende principalmente da implementação que está
    sendo usada (Delphi ou Lazarus).
    }
    property AllowGrayed:Boolean read GetAllowGrayed stored false;
    {:
    Use @name para ver/setar o estado do controle. Se escrito através dessa
    propriedade, o valor ValueTrue/ValueFalse é escrito no Tag caso as
    propriedades WriteTrueValue/WriteFalseValue estejam em @true
    }
    property Checked stored false;
    {:
    Tag numérico usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCBlockElement)
    @seealso(TPLCTagNumber)
    }
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;
    {:
    @name mostra o CaptionTrue, CaptionFalse, CaptionGrayed de acordo com o estado
    do componente ou escreve seu valor em CaptionTrue, CaptionFalse e
    CaptionGrayed, para agilizar sua configuração.
    @seealso(CaptionTrue)
    @seealso(CaptionFalse)
    @seealso(CaptionGrayed)
    }
    property Caption write SetCaption stored false;
    {:
    @name é texto que será mostrado no controle caso ele esteja desmarcado.
    @seealso(Caption)
    }
    property CaptionFalse:String read FCaptionFalse write SetCaptionFalse stored true nodefault;
    {:
    @name é texto que será mostrado no controle caso ele esteja marcado.
    @seealso(Caption)
    }
    property CaptionTrue:String read FCaptionTrue write SetCaptionTrue stored true nodefault;
    {:
    @name é texto que será mostrado no controle caso ele esteja no meio-termo
    (nem marcado e nem desmarcado).
    @seealso(Caption)
    }
    property CaptionGrayed:string read FCaptionGrayed write SetCaptionGrayed stored true nodefault;


    {:
    @name mostra o ColorTrue, ColorFalse, ColorGrayed de acordo com o estado
    do componente ou escreve seu valor em ColorTrue, ColorFalse e ColorGrayed,
    para agilizar sua configuração.
    @seealso(ColorTrue)
    @seealso(ColorFalse)
    @seealso(ColorGrayed)
    }
    property Color write SetColor stored false;
    {:
    @name é a cor do controle caso ele esteja desmarcado.
    @seealso(Color)
    }
    property ColorFalse:TColor read FColorFalse write SetColorFalse stored true default clBtnFace;
    {:
    @name é a cor do controle caso ele esteja marcado.
    @seealso(Color)
    }
    property ColorTrue:TColor read FColorTrue write SetColorTrue stored true default clBtnFace;
    {:
    @name é a cor do controle caso ele esteja no meio-termo (nem marcado e nem
    desmarcado).
    @seealso(Color)
    }
    property ColorGrayed:TColor read FColorGrayed write SetColorGrayed stored true default clBtnFace;

    {:
    @name mostra o FontTrue, FontFalse ou FontGrayed de acordo com o estado
    do componente ou escreve seu valor em FontTrue, FontFalse e FontGrayed,
    para agilizar sua configuração.
    @seealso(ColorTrue)
    @seealso(ColorFalse)
    @seealso(ColorGrayed)
    }
    property Font write SetFont stored false;
    {:
    @name é a fonte que será usada pelo controle caso ele esteja desmarcado.
    @seealso(Font)
    }
    property FontFalse:TFont read FFontFalse write SetFontFalse stored true;
    {:
    @name é a fonte que será usada pelo controle caso ele esteja marcado.
    @seealso(Font)
    }
    property FontTrue:TFont read FFontTrue write SetFontTrue stored true;
    {:
    @name é a fonte que será usada pelo controle caso ele esteja  no meio-termo
    (nem marcado e nem desmarcado).
    @seealso(Font)
    }
    property FontGrayed:TFont read FFontGrayed write SetFontGrayed stored true;

    {:
    Diz como o controle irá se comportar caso o valor do Tag não seja igual a
    ValueFalse e nem a ValueTrue.

    @seealso(TOtherValues)
    }
    property OtherValuesIS:TOtherValues read FOtherValues write SetOtherValues stored true default IsGrayed;
    {:
    Caso o valor do Tag seja igual a @name, o controle é desmarcado.
    }
    property ValueFalse:Double read FValueFalse write SetValueFalse stored true;
    {:
    Caso o valor do Tag seja igual a @name, o controle é marcado.
    }
    property ValueTrue:Double read FValueTrue write SetValueTrue stored true;
    {:
    @name diz que qualquer ação feita pelo usuário que marque o controle, deve
    também escrever ValueTrue no tag associado.
    }
    property WriteTrueValue:Boolean read FWriteTrue write SetWriteTrue stored true default true;
    {:
    @name diz que qualquer ação feita pelo usuário que desmarque o controle, deve
    também escrever ValueFalse no tag associado.
    }
    property WriteFalseValue:Boolean read FWriteFalse write SetWriteFalse stored true default true;
    //: Informa o atual estado do controle (marcado, desmarcado, acinzentado).
    property State:TCheckBoxState read GetState stored false nodefault;
  end;

implementation

uses PLCNumber;

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
  {$IFDEF FPC}
  BlockOnChange := false;
  //no lazarus essa propriedade tem q estar
  //habilitada para q seja mostrado o controle
  //no meio termo...o tratamento do meio termo
  //esta em DoOnChange...
  inherited AllowGrayed := true;
  {$ELSE}
  //no Delphi, mesmo desabilitada essa propriedade
  //ele aceita por meio da prop State o meio termo
  //processando ou não o meio termo de acordo
  //com o valor dessa propriedade...
  inherited AllowGrayed := false;
  {$ENDIF}
  FFontFalse  := TFont.Create;
  FFontFalse.OnChange := FontChange;
  FFontTrue   := TFont.Create;
  FFontTrue.OnChange := FontChange;
  FFontGrayed := TFont.Create;
  FFontGrayed.OnChange := FontChange;
  FColorTrue := clBtnFace;
  FColorFalse:= clBtnFace;
  FColorGrayed:= clBtnFace;
  FOtherValues := IsGrayed;
  FWriteTrue := true;
  FWriteFalse := true;
end;

destructor THMICheckBox.Destroy;
begin
  if FTag<>nil then
    FTag.RemoveChangeCallBack(HMINotifyChangeCallback);
  FFontFalse.Destroy;
  FFontTrue.Destroy;
  FFontGrayed.Destroy;
  inherited Destroy;
end;

procedure THMICheckBox.SetHMITag(t:TPLCTag);
begin
  //se o tag é um tag numerico.
  if ((t as ITagNumeric)=nil) then
     raise Exception.Create('Somente tags numéricos são aceitos!');

  //se ja estou associado a um tag, remove
  if FTag<>nil then begin
    FTag.RemoveChangeCallBack(HMINotifyChangeCallback);
  end;

  //adiona o callback para o novo tag
  if t<>nil then begin
    t.AddChangeCallBack(HMINotifyChangeCallback, RemoveHMITag);
    FTag := t;
    RefreshTagValue(GetTagValue);
  end;
  FTag := t;
end;

function  THMICheckBox.GetHMITag:TPLCTag;
begin
   Result := FTag;
end;

procedure THMICheckBox.RemoveHMITag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

procedure THMICheckBox.RefreshTagValue(x:Double);
begin
  if csDestroying in ComponentState then exit;
  {$IFDEF FPC}
  BlockOnChange := true;
  {$ENDIF}
  if x=FValueTrue then begin
    inherited SetChecked(true);
    inherited State := cbChecked;
    inherited Font.Assign(FFontTrue);
    inherited Color := FColorTrue;
    inherited Caption := FCaptionTrue;
  end else
    if x=FValueFalse then begin
      inherited SetChecked(false);
      inherited State := cbUnchecked;
      inherited Font.Assign(FFontFalse);
      inherited Color := FColorFalse;
      inherited Caption := FCaptionFalse;
    end else
      case FOtherValues of
        isChecked: begin
          inherited SetChecked(true);
          inherited State := cbChecked;
          inherited Font.Assign(FFontTrue);
          inherited Color := FColorTrue;
          inherited Caption := FCaptionTrue;
        end;
        isUnchecked: begin
          inherited SetChecked(false);
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
        else begin
          //do nothing... :D
        end;
      end;
  {$IFDEF FPC}
  LazLastState := inherited State;
  BlockOnChange := false;
  {$ENDIF}
end;

procedure THMICheckBox.HMINotifyChangeCallback(Sender:TObject);
begin
  if (csDesigning in ComponentState) or (csReading in ComponentState) or (FTag=nil) then begin
    exit;
  end;

  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetWriteTrue(v:Boolean);
var
  itag:ITagNumeric;
begin
  if (inherited GetChecked) and v then begin
    itag := FTag as ITagNumeric;
    if itag<>nil then
      itag.Value:=FValueTrue;
  end;
  FWriteTrue := v
end;

procedure THMICheckBox.SetWriteFalse(v:Boolean);
var
  itag:ITagNumeric;
begin
  if (not (inherited GetChecked)) and v then begin
    itag := FTag as ITagNumeric;
    if itag<>nil then
      itag.Value:=FValueFalse;
  end;
  FWriteFalse := v
end;

procedure THMICheckBox.SetValueTrue(v:Double);
var
  itag:ITagNumeric;
begin
  if ((ComponentState*[csReading, csLoading])=[]) and (v=FValueFalse) then
    raise Exception.Create('O valor precisa ser diferente do valor FALSO!');

  itag := FTag as ITagNumeric;

  if (v<>FValueTrue) and (itag<>nil) then begin
    if GetChecked and FWriteTrue then begin
      itag.Value:=v;
    end;
  end;
  FValueTrue := v;
end;

procedure THMICheckBox.SetValueFalse(v:Double);
var
   itag:ITagNumeric;
begin
  if ((ComponentState*[csReading, csLoading])=[]) and (v=FValueTrue) then
    raise Exception.Create('O valor precisa ser diferente do valor VERDADEIRO!');

  itag := FTag as ITagNumeric;

  if (v<>FValueFalse) and (itag<>nil) then begin
    if (not GetChecked) and FWriteFalse then begin
      itag.Value:=v;
    end;
  end;

  FValueFalse := v;
end;

function THMICheckBox.GetChecked: Boolean;
begin
  if FTag=nil then begin
    Result := false;
    exit;
  end;

  Result := (GetTagValue=FValueTrue);
end;

procedure THMICheckBox.SetChecked(Value: Boolean);
var
   itag:ITagNumeric;
begin
  if (csReading in ComponentState) or (csLoading in ComponentState) or (FTag=nil) then
    exit;

  itag := FTag as ITagNumeric;

  if (itag<>nil) then
    if Value then begin
      if FWriteTrue then begin
        itag.Value := FValueTrue;
        RefreshTagValue(FValueTrue);
      end;
    end else begin
      if FWriteFalse then begin
        itag.Value := FValueFalse;
        RefreshTagValue(FValueFalse);
      end;
    end;

  inherited SetChecked(Value);
end;

{$IFDEF FPC}
procedure THMICheckBox.DoOnChange;
begin
  try
    if not BlockOnChange then
      Inherited DoOnChange;
  finally
    if not BlockOnChange then begin
      SetChecked((State=cbChecked) or (LazLastState=cbGrayed));
      HMINotifyChangeCallback(Self);
    end;
  end;
end;

{$ELSE}
procedure THMICheckBox.Toggle;
begin
  inherited Toggle;
  SetChecked(State=cbChecked);
  HMINotifyChangeCallback(Self);
end;
{$ENDIF}

function THMICheckBox.GetTagValue:Double;
var
   itag:ITagNumeric;
begin
  Result := 0;
  itag := FTag as ITagNumeric;
  if itag=Nil then exit;
  
  Result := ITag.Value;
end;


procedure THMICheckBox.SetCaptionFalse(v:String);
begin
  if v=FCaptionFalse then exit;

  FCaptionFalse := v;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetCaptionTrue(v:String);
begin
  if v=FCaptionTrue then exit;

  FCaptionTrue := v;
  RefreshTagValue(GetTagValue);
end;

procedure THMICheckBox.SetCaptionGrayed(v:String);
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

procedure THMICheckBox.SetColor(c:TColor);
begin
  if not (csDesigning in ComponentState) then exit;

  FColorFalse := c;
  FColorGrayed := c;
  FColorTrue := c;
  RefreshTagValue(GetTagValue);
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

procedure THMICheckBox.RefreshHMISecurity;
begin

end;

procedure THMICheckBox.SetHMIEnabled(v:Boolean);
begin
   { todo: }
   inherited Enabled := v;
   FIsEnabled := v;
end;

function  THMICheckBox.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

end.
