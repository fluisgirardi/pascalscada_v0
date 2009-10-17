//: implementa um controle para a exibição de valores de quaisquer tags.
unit HMILabel;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, StdCtrls, PLCTag, HMITypes, ProtocolTypes, Tag;

type
  //:@name implementa o controle para a exibir de valores de qualquer tipo de tag.
  THMILabel = class(TLabel, IHMIInterface, IHMITagInterface)
  private
    FNumberFormat:string;
    FPrefix, FSufix:string;
    FIsEnabled:Boolean;

    procedure SetFormat(f:string);
    function  GetHMITag:TPLCTag;
    procedure SetPrefix(s:String);
    procedure SetSufix(s:String);

    function  GetCaption:TCaption;

    procedure SetHMIEnabled(v:Boolean);
    function  GetHMIEnabled:Boolean;

    procedure RefreshHMISecurity;

    //IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure RemoveTag(Sender:TObject);
  protected
    //: @exclude
    FTag:TPLCTag;
    //: @exclude
    procedure SetHMITag(t:TPLCTag); virtual;
    //: @exclude
    procedure RefreshTagValue; virtual;

    //IHMITagInterface
    procedure NotifyTagChange(Sender:TObject); virtual;

  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    {:
    @name informa o texto que está sendo exibido pelo controle.
    Inclui sufixo e prefixo.
    }
    property Caption:TCaption read GetCaption stored false;
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
    property PLCTag:TPLCTag read FTag write SetHMITag;
    {:
    @name é o texto que é exibido a esquerda (antes) do valor do tag.
    }
    property Prefix:string read FPrefix write SetPrefix;
    {:
    @name é o texto que é exibido a direita (após) do valor do tag. Útil para
    informar o tipo da grandeza exibida, por exemplo ºC.
    }
    property Sufix:String read FSufix write SetSufix;
    //: @exclude
    property AutoSize default False;
  end;

implementation

constructor THMILabel.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if (csDesigning in ComponentState) then
    inherited Caption := 'SEM TAG!';
  AutoSize:=False;
  FNumberFormat := '#0.0';
end;

destructor  THMILabel.Destroy;
begin
  if FTag<>nil then
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  inherited Destroy;
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
  if (t<>nil) and ((t as ITagInterface)=nil) then
     raise Exception.Create('Tag inválido!');

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
  
  if (FTag=nil) and (csDesigning in ComponentState) then
    inherited Caption := 'SEM TAG!';
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
var
  itag:ITagInterface;
begin
  if (csDesigning in ComponentState) or (csReading in ComponentState) or (FTag=nil) then begin
    if (csDesigning in ComponentState) then begin
      if (FTag=nil) then
        inherited Caption := 'SEM TAG!'
      else
        inherited Caption := FTag.Name;
    end;
    exit;
  end;

  itag := (FTag as ITagInterface);
  if (itag<>nil) then
    inherited Caption := itag.GetValueAsText(FPrefix, FSufix, FNumberFormat);
end;

function  THMILabel.GetCaption:TCaption;
begin
  Result := inherited Caption;
end;

procedure THMILabel.SetHMIEnabled(v:Boolean);
begin
   { todo: }
   inherited Enabled := v;
   FIsEnabled := v;
end;

function  THMILabel.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

procedure THMILabel.RefreshHMISecurity;
begin

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
