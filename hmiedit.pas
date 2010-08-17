{:
  @abstract(Unit que implementa uma caixa para entrada de valores em Tags.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit HMIEdit;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, StdCtrls, PLCTag, HMITypes, Graphics, Dialogs,
  {$IFDEF FPC}LCLIntf, LCLType,{$ELSE}Windows,{$ENDIF} ProtocolTypes, Tag;

type
  {:
    @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
    Implementa um Edit para leitura/escrita de valores texto/numéricos em tags.

    @bold(Para maiores informações consulte a documentação da classe TEdit
    de seu ambiente de desenvolvimento.)
  }
  THMIEdit = class(TEdit, IHMIInterface, IHMITagInterface)
  private
    FAlignment:TAlignment;
    FTag:TPLCTag;
    FShowFocused:Boolean;
    FDefFontColor:TColor;
    FDefColor:TColor;
    FFontChangeEvent:TNotifyEvent;
    FBlockFontChange:Boolean;
    FBlockChange:Boolean;
    oldValue:string;
    FPrefix, FSufix:string;
    FNumberFormat:string;
    FSend:TSendChange;
    FFreezeValue:Boolean;
    FFreezedValue:Boolean;
    HasFocus:Boolean;
    FIsEnabled:Boolean;

    procedure RemoveHMITag(Sender:TObject);

    procedure SetHMITag(t:TPLCTag);
    function  GetHMITag:TPLCTag;
    procedure SetFormat(f:string);
    function  GetText:TCaption;
    procedure RefreshTagValue;
    procedure SetSend(s:TSendChange);
    procedure SetShowFocused(f:Boolean);
    procedure RepaintFocus;
    function  GetColor:TColor;
    procedure FontChange(Sender: TObject);
    procedure SetPrefix(s:String);
    procedure SetSufix(s:String);

    procedure SendValue(txt:String);
    procedure RefreshHMISecurity;

    procedure SetHMIEnabled(v:Boolean);
    function  GetHMIEnabled:Boolean;

    //IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
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
    //: Alinhamento do texto dentro do Edit. Somente Delphi. Lazarus já oferece esse recurso.
    property Alignment:TAlignment read FAlignment write SetAlignment default taRightJustify;
    {$ENDIF}
    {:
    @name diz para o controle quando um valor alterado deve ser escrito no Tag.
    @seealso(TSendChange)
    }
    property SendValueWhen:TSendChange read FSend write SetSend stored true default [scLostFocus, scPressEnter];
    {:
    Caso o tag associado ao controle seja numérico, especifica a formatação
    numérica adotada.

    Para maiores informações procure sobre a função FormatFloat de seu ambiente
    de desenvolvimento.
    }
    property NumberFormat:string read FNumberFormat write SetFormat;
    {:
    Especifica o tag que será usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    @seealso(TPLCString)
    }
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;
    {:
    @name retorna o texto que está sendo exibido pelo controle. Inclui sufixo,
    prefixo e formatação.
    }
    property Text:TCaption read GetText stored false;
    {:
    @name é a cor de fundo do controle.
    }
    property Color:TColor read GetColor Write SetColor;
    {:
    @name faz com que o controle passe a cor do fundo para a cor da fonte e
    vice-versa quando ele estiver com o foco.
    
    @bold(Deixe essa propriedade @false quando sua aplicação estiver rodando
    sobre o toolkit GTK1.)
    }
    property ShowFocused:Boolean read FShowFocused write SetShowFocused default true;
    {:
    @name é o texto que é exibido a esquerda (antes) do valor do tag quando o controle
    @bold(não tem o foco).
    }
    property Prefix:string read FPrefix write SetPrefix;
    {:
    @name é o texto que é exibido a direita (após) do valor do tag quando o controle
    @bold(não tem o foco). Útil para informar o tipo da grandeza exibida.
    }
    property Sufix:String read FSufix write SetSufix;
    {:
    Caso o valor de @name seja @true, faz com que o controle pare de atualizar
    o valor do tag no controle quando este tem o foco da aplicação
    }
    property FreezeValueOnFocus:Boolean read FFreezeValue write FFreezeValue default true;
  end;

implementation

uses hsstrings;

constructor THMIEdit.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FAlignment := taRightJustify;
  FIsEnabled := inherited Enabled;
  FSend := [scLostFocus, scPressEnter];
  if (csDesigning in ComponentState) then begin
    inherited Text := SWithoutTag;
    Modified := false;
  end;

  FFontChangeEvent := Font.OnChange;
  Font.OnChange := FontChange;
  FShowFocused := true;
  FFreezeValue := true;
  FNumberFormat := '#0.0';
end;

destructor  THMIEdit.Destroy;
begin
  if FTag<>nil then
    FTag.RemoveCallBacks(Self as IHMITagInterface);
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

procedure THMIEdit.RefreshHMISecurity;
begin

end;

procedure THMIEdit.Loaded;
begin
  inherited Loaded;
  FDefFontColor := Font.Color;
  FDefColor := inherited Color;
end;

procedure THMIEdit.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  if (t<>nil) and ((not Supports(t, ITagInterface)) and (not Supports(t, ITagNumeric)) and (not Supports(t, ITagString))) then
     raise Exception.Create(SinvalidTag);

  //se ja estou associado a um tag, remove
  if FTag<>nil then begin
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  end;

  //adiona o callback para o novo tag
  if t<>nil then begin
    t.AddCallBacks(Self as IHMITagInterface);
    FTag := t;
    RefreshTagValue;
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

procedure THMIEdit.SetFormat(f:string);
begin
  FNumberFormat := f;
  RefreshTagValue;
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
  if [csReading]*ComponentState<>[] then
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

procedure THMIEdit.SetPrefix(s:String);
begin
  FPrefix := s;
  RefreshTagValue;
end;

procedure THMIEdit.SetSufix(s:String);
begin
  FSufix := s;
  RefreshTagValue;
end;

procedure THMIEdit.FontChange(Sender: TObject);
begin
  if Assigned(FFontChangeEvent) then
    FFontChangeEvent(Sender);

  if FBlockFontChange then exit;

  FDefFontColor := Font.Color;
end;

procedure THMIEdit.RefreshTagValue;
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

procedure THMIEdit.SendValue(Txt:String);
begin
  if (csReading in ComponentState) or
     (FTag=nil) or
     (not Modified) then
     exit;

  if Supports(FTag, ITagNumeric) then begin
    (FTag as ITagNumeric).Value := StrToFloat(Txt);
    exit;
  end;

  if Supports(FTag, ITagString) then begin
    (FTag as ITagString).Value := Txt;
    exit;
  end;
  
  if Supports(FTag, ITagInterface) then begin
    (FTag as ITagInterface).ValueVariant := Txt;
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
     RefreshTagValue;
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
  RefreshTagValue;

  inherited DoExit;
end;

procedure THMIEdit.DoEnter;
begin
  oldValue := Text;
  HasFocus := true;
  RepaintFocus;
  RefreshTagValue;
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

procedure THMIEdit.SetHMIEnabled(v:Boolean);
begin
   { todo: }
   inherited Enabled := v;
   FIsEnabled := v;
end;

function  THMIEdit.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

procedure THMIEdit.NotifyReadOk;
begin

end;

procedure THMIEdit.NotifyReadFault;
begin

end;

procedure THMIEdit.NotifyWriteOk;
begin

end;

procedure THMIEdit.NotifyWriteFault;
begin
  RefreshTagValue;
end;

procedure THMIEdit.NotifyTagChange(Sender:TObject);
begin
  RefreshTagValue;
end;

procedure THMIEdit.RemoveTag(Sender:TObject);
begin
  if Ftag=Sender then
    FTag:=nil;
end;

end.
