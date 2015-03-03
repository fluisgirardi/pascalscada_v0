{$i ../common/pscada_settings.inc}
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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

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
  THMILabel = class(TLabel, IHMIInterface, IHMITagInterface)
  private
    FNumberFormat:string;
    FPrefix, FSufix:string;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;

    FSecurityCode:String;
    procedure SetSecurityCode(sc:String);

    procedure SetFormat(f:string);
    procedure SetPrefix(s:String);
    procedure SetSufix(s:String);
    function  GetCaption:TCaption;

    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
     function GetControlSecurityCode:String;
    //: @seealso(IHMIInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean);
    //: @seealso(IHMIInterface.MakeUnsecure)
    procedure MakeUnsecure;

    //the procedurs below implements the IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure RemoveTag(Sender:TObject);
  protected
    //: @exclude
    FTag:TPLCTag;
    //: @exclude
    procedure SetEnabled(e:Boolean); override;
    //: @exclude
    procedure SetHMITag(t:TPLCTag); virtual;
    //: @exclude
    procedure RefreshTagValue; virtual;

    //IHMITagInterface, visible to THMIText
    procedure NotifyTagChange(Sender:TObject); virtual;

  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
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
    property NumberFormat:string read FNumberFormat write SetFormat;

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
    property Prefix:string read FPrefix write SetPrefix;

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
    property Sufix:String read FSufix write SetSufix;
    //: @exclude
    property AutoSize default False;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;

  end;

implementation

uses hsstrings, HMIControlSecurityManager;

constructor THMILabel.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if (csDesigning in ComponentState) then
    inherited Caption := SWithoutTag;
  AutoSize:=False;
  FIsEnabled:=true;
  FNumberFormat := '#0.0';
  GetHMIControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor  THMILabel.Destroy;
begin
  if FTag<>nil then
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  GetHMIControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

procedure THMILabel.SetSecurityCode(Sc:String);
begin
  if Trim(sc)='' then
    Self.CanBeAccessed(true)
  else
    with GetHMIControlSecurityManager do begin
      ValidateSecurityCode(sc);
      if not SecurityCodeExists(sc) then
        RegisterSecurityCode(sc);

      Self.CanBeAccessed(CanAccess(sc));
    end;

  FSecurityCode:=sc;
end;

procedure THMILabel.SetFormat(f:string);
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
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  end;

  //adiona o callback para o novo tag
  //link with the new tag.
  if t<>nil then begin
    t.AddCallBacks(Self as IHMITagInterface);
    FTag := t;
    RefreshTagValue;
  end;
  FTag := t;
  
  if (FTag=nil) and (csDesigning in ComponentState) then
    inherited Caption := SWithoutTag;
end;

procedure THMILabel.SetPrefix(s:String);
begin
  FPrefix := s;
  RefreshTagValue;
end;

procedure THMILabel.SetSufix(s:String);
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
    inherited Caption := (FTag as ITagInterface).GetValueAsText(FPrefix, FSufix, FNumberFormat);
end;

function  THMILabel.GetCaption:TCaption;
begin
  Result := inherited Caption;
end;

function THMILabel.GetControlSecurityCode:String;
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

procedure THMILabel.NotifyReadOk;
begin

end;

procedure THMILabel.NotifyReadFault;
begin

end;

procedure THMILabel.NotifyWriteOk;
begin

end;

procedure THMILabel.NotifyWriteFault;
begin
  RefreshTagValue;
end;

procedure THMILabel.NotifyTagChange(Sender:TObject);
begin
  RefreshTagValue;
end;

procedure THMILabel.RemoveTag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

end.
