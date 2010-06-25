{:
  @abstract(Implementa um controle para exibição de valores numéricos em forma de barra de progresso.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit HMIProgressBar;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, ComCtrls, HMITypes, PLCTag, hsutils,
  ProtocolTypes, Tag;

type
  {:
  Implementa um controle para exibição de valores numéricos em forma de barra de
  progresso.
  
  @bold(Para maiores informações consulte a documentação da classe TProgressBar
  de seu ambiente de desenvolvimento.)
  }
  THMIProgressBar = class(TProgressBar, IHMIInterface, IHMITagInterface)
  private
    FTag:TPLCTag;
    FIsEnabled:Boolean;
    procedure RefreshHMISecurity;                      //alquem efetuou login e é necessario verificar autorizações.
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    function  GetHMITag:TPLCTag;
    function  GetHMIEnabled:Boolean;
    procedure SetHMIEnabled(v:Boolean);
    function  GetPosition:Double;

    //IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    destructor Destroy; override;
  published
    //: Informa a posição (valor do tag) atual.
    property Position:Double read GetPosition;
    //: @exclude
    property Enabled:Boolean read GetHMIEnabled write SetHMIEnabled;
    {:
    Tag numérico que será usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    }
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;
  end;

implementation

uses hsstrings;

destructor THMIProgressBar.Destroy;
begin
  if Assigned(FTag) then
    Ftag.RemoveCallBacks(Self as IHMITagInterface);
  inherited Destroy;
end;

procedure THMIProgressBar.Loaded;
begin
   inherited Loaded;
   NotifyTagChange(Self);
end;

procedure THMIProgressBar.RefreshHMISecurity;
begin

end;

procedure THMIProgressBar.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  if FTag<>nil then begin
    FTag.RemoveCallBacks(Self as IHMITagInterface);
  end;

  //adiona o callback para o novo tag
  if t<>nil then begin
    t.AddCallBacks(Self as IHMITagInterface);
    FTag := t;
    NotifyTagChange(self);
  end;
  FTag := t;
end;

function  THMIProgressBar.GetHMITag:TPLCTag;
begin
  Result := FTag;
end;

function  THMIProgressBar.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

procedure THMIProgressBar.SetHMIEnabled(v:Boolean);
begin
   inherited Enabled := v;
   FIsEnabled := v;
end;

function  THMIProgressBar.GetPosition:Double;
begin
   Result := 0;
   if (FTag<>nil) AND Supports(FTag, ITagNumeric) then begin
      Result := (FTag as ITagNumeric).Value;
   end;
end;

procedure THMIProgressBar.NotifyReadOk;
begin

end;

procedure THMIProgressBar.NotifyReadFault;
begin

end;

procedure THMIProgressBar.NotifyWriteOk;
begin

end;

procedure THMIProgressBar.NotifyWriteFault;
begin
  NotifyTagChange(Self);
end;

procedure THMIProgressBar.NotifyTagChange(Sender:TObject);
begin
  if (csDesigning in ComponentState) or (csReading in ComponentState) or (FTag=nil) then
    exit;

  if (FTag<>nil) and (Supports(FTag, ITagNumeric)) then
    inherited Position := FloatToInteger((FTag as ITagNumeric).Value);
end;

procedure THMIProgressBar.RemoveTag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

end.
