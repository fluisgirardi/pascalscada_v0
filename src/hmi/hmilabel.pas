{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit que implementa um controle para a exibição de valores de qualquer tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a control to show values of any tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMILabel;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, PLCTag, HMITypes, ProtocolTypes, Tag;

type

  {$IFDEF PORTUGUES}
  {:
    @name implementa o controle para a exibir de valores de qualquer tipo de tag.
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    @bold(Para maiores informações consulte a documentação da classe TLabel
    de seu ambiente de desenvolvimento.)
  }
  {$ELSE}
  {:
    @name implements the control to show values of any kind of tag.
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    @bold(To more information, see the documentation of the class TLabel of your
    development environment.
  }
  {$ENDIF}

  { THMILabel }

  THMILabel = class(TLabel, IHMIInterface)
  private
    FFormatDateTimeOptions: TFormatDateTimeOptions;
    FNumberFormat:AnsiString;
    FPrefix, FSufix:TCaption;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;

    FSecurityCode:UTF8String;
    procedure SetFormatDateTimeOptions(AValue: TFormatDateTimeOptions);
    procedure SetSecurityCode(sc:UTF8String);

    procedure SetFormat(f:AnsiString);
    procedure SetPrefix(s:TCaption);
    procedure SetSufix(s:TCaption);
    function  GetCaption:TCaption;

    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
     function GetControlSecurityCode:UTF8String;
    //: @seealso(IHMIInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean);
    //: @seealso(IHMIInterface.MakeUnsecure)
    procedure MakeUnsecure;

  protected
    //: @exclude
    FTag:TPLCTag;
    //: @exclude
    procedure SetEnabled(e:Boolean); override;
    //: @exclude
    procedure SetHMITag(t:TPLCTag); virtual;
    //: @exclude
    procedure RefreshTagValue; virtual;

    procedure WriteFaultCallBack(Sender:TObject); virtual;
    procedure RemoveTagCallBack(Sender:TObject); virtual;
    procedure TagChangeCallBack(Sender:TObject); virtual;

  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    procedure RefreshLabel(Data: PtrInt);
  published

    {$IFDEF PORTUGUES}
    {:
    @name informa o texto que está sendo exibido pelo controle.
    Inclui sufixo e prefixo.
    }
    {$ELSE}
    {:
    @name is the current text that is being showed by the control. Includes the
    prefix and the suffix.
    }
    {$ENDIF}
    property Caption:TCaption read GetCaption stored false;

    //: @exclude
    property Enabled:Boolean read FIsEnabled write SetEnabled;

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

    To get more information, see the documentation of the funcion FormatFloat of
    your development environment.
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
    The tag that the control will show the values.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    @seealso(TPLCStructItem)
    @seealso(TPLCString)
    }
    {$ENDIF}
    property PLCTag:TPLCTag read FTag write SetHMITag;

    {$IFDEF PORTUGUES}
    {:
    @name é o texto que é exibido a esquerda (antes) do valor do tag.
    }
    {$ELSE}
    {:
    @name is the text that will be show at the left (before) of the tag value.
    }
    {$ENDIF}
    property Prefix:TCaption read FPrefix write SetPrefix;

    {$IFDEF PORTUGUES}
    {:
    @name é o texto exibido a direita (após) do valor do tag. Útil para informar
    o tipo da grandeza exibida.
    }
    {$ELSE}
    {:
    @name is the text that will be show at the right (after) of the tag value.
    Useful to show the engineering unit of the tag.
    }
    {$ENDIF}
    property Sufix:TCaption read FSufix write SetSufix;
    //: @exclude
    property AutoSize default False;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;

    {$IFDEF PORTUGUES}
    //: Configuraçoes de formataçao data/hora.
    {$ELSE}
    //: Date/time format options.
    {$ENDIF}
    property FormatDateTimeOptions:TFormatDateTimeOptions read FFormatDateTimeOptions write SetFormatDateTimeOptions;
  end;

implementation

uses hsstrings, ControlSecurityManager, Forms;

constructor THMILabel.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if (csDesigning in ComponentState) then
    inherited Caption := SWithoutTag;
  AutoSize:=False;
  FIsEnabled:=true;
  FNumberFormat := '#0.0';
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor  THMILabel.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

procedure THMILabel.RefreshLabel(Data: PtrInt);
begin
  RefreshTagValue;
end;

procedure THMILabel.SetSecurityCode(sc: UTF8String);
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

procedure THMILabel.SetFormatDateTimeOptions(AValue: TFormatDateTimeOptions);
begin
  if FFormatDateTimeOptions=AValue then Exit;
  FFormatDateTimeOptions:=AValue;
  RefreshTagValue;
end;

procedure THMILabel.SetFormat(f: AnsiString);
begin
  FNumberFormat := f;
  RefreshTagValue;
end;

function  THMILabel.GetHMITag:TPLCTag;
begin
  Result := FTag;
end;

procedure THMILabel.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  //
  //check if the tag is valid.
  if (t<>nil) and (not Supports(t, ITagInterface)) then
     raise Exception.Create(SinvalidTag);

  //se ja estou associado a um tag, remove
  //
  //if the control is linked with some tag, remove the old link.
  if FTag<>nil then begin
    FTag.RemoveAllHandlersFromObject(Self);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if t<>nil then begin
    t.AddTagChangeHandler(@TagChangeCallBack);
    t.AddWriteFaultHandler(@WriteFaultCallBack);
    t.AddRemoveTagHandler(@RemoveTagCallBack);
    FTag := t;
    RefreshTagValue;
  end;
  FTag := t;
  
  if (FTag=nil) and (csDesigning in ComponentState) then
    inherited Caption := SWithoutTag;
end;

procedure THMILabel.SetPrefix(s: TCaption);
begin
  FPrefix := s;
  RefreshTagValue;
end;

procedure THMILabel.SetSufix(s: TCaption);
begin
  FSufix := s;
  RefreshTagValue;
end;

procedure THMILabel.RefreshTagValue;
begin
  if (csReading in ComponentState) or (FTag=nil) then begin
    if (csDesigning in ComponentState) then begin
      if (FTag=nil) then
        inherited Caption := SWithoutTag
      else
        inherited Caption := FTag.Name;
    end;
    exit;
  end;

  if (FTag<>nil) AND Supports(FTag, ITagInterface) then
    inherited Caption := (FTag as ITagInterface).GetValueAsText(FPrefix, FSufix, FNumberFormat, FFormatDateTimeOptions);
end;

function  THMILabel.GetCaption:TCaption;
begin
  Result := inherited Caption;
end;

function THMILabel.GetControlSecurityCode: UTF8String;
begin
   Result:=FSecurityCode;
end;

procedure THMILabel.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure THMILabel.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMILabel.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure THMILabel.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMILabel.TagChangeCallBack(Sender: TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    Application.QueueAsyncCall(@RefreshLabel,0);
end;

procedure THMILabel.RemoveTagCallBack(Sender: TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

end.
