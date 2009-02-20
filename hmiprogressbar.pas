//: Implementa um controle para exibição de valores numéricos em forma de barra de progresso.
unit HMIProgressBar;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, ComCtrls, HMITypes, PLCTag, hsutils,
  ProtocolTypes;

type
  {:
  Implementa um controle para exibição de valores numéricos em forma de barra de
  progresso.
  
  @bold(Para maiores informações consulte a documentação de TProgressBar de seu
  ambiente de desenvolvimento.)
  }
  THMIProgressBar = class(TProgressBar, IHMIInterface)
  private
    FTag:TPLCTag;
    FIsEnabled:Boolean;
    procedure HMINotifyChangeCallback(Sender:TObject); //notificação de change do valor do tag
    procedure RefreshHMISecurity;                      //alquem efetuou login e é necessario verificar autorizações
    procedure RemoveHMITag(Sender:TObject);            //Forca a eliminação de referencia do tag.
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    function  GetHMITag:TPLCTag;
    function  GetHMIEnabled:Boolean;
    procedure SetHMIEnabled(v:Boolean);
    function  GetPosition:Double;
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

destructor THMIProgressBar.Destroy;
begin
  if Assigned(FTag) then
    Ftag.RemoveChangeCallBack(HMINotifyChangeCallBack);
  inherited Destroy;
end;

procedure THMIProgressBar.Loaded;
begin
   inherited Loaded;
   HMINotifyChangeCallback(Self);
end;

procedure THMIProgressBar.HMINotifyChangeCallback(Sender:TObject);
begin
  if (csDesigning in ComponentState) or (csReading in ComponentState) or (FTag=nil) then
    exit;

  if ((FTag as ITagNumeric)<>nil) then
    inherited Position := FloatToInteger((FTag as ITagNumeric).Value);
end;

procedure THMIProgressBar.RefreshHMISecurity;
begin

end;

procedure THMIProgressBar.RemoveHMITag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

procedure THMIProgressBar.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  if (t<>nil) and ((t as ITagNumeric)=nil) then
     raise Exception.Create('Somente tags numéricos são aceitos!');

  //se ja estou associado a um tag, remove
  if FTag<>nil then begin
    FTag.RemoveChangeCallBack(HMINotifyChangeCallback);
  end;

  //adiona o callback para o novo tag
  if t<>nil then begin
    t.AddChangeCallBack(HMINotifyChangeCallback, RemoveHMITag);
    FTag := t;
    HMINotifyChangeCallback(self);
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
   if (FTag as ITagNumeric)<>nil then begin
      Result := (FTag as ITagNumeric).Value;
   end;
end;

end.
