//: Define um controle de opções para leitura/escrita de valores de tags numéricos.
unit HMIRadioGroup;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, {$ENDIF} Forms, Controls, Graphics,
  Dialogs, ExtCtrls, HMITypes, PLCTag, hsutils, ProtocolTypes;

type
  //: Define um controle de opções para leitura/escrita de valores de tags numéricos.
  THMIRadioGroup = class(TRadioGroup, IHMIInterface)
  private
    FTag:TPLCTag;
    FIsEnabled:Boolean;
    FDefaultIndex:Integer;
    FIgnore, FLoaded:Boolean;
    {$IFNDEF FPC}
    FOnClick:TNotifyEvent;
    procedure MyClick(Sender:TObject);
    {$ENDIF}
    procedure HMINotifyChangeCallback(Sender:TObject); //notificação de change do valor do tag
    procedure RefreshHMISecurity;                      //alquem efetuou login e é necessario verificar autorizações
    procedure RemoveHMITag(Sender:TObject);            //Forca a eliminação de referencia do tag.
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    function  GetHMITag:TPLCTag;
    function  GetHMIEnabled:Boolean;
    procedure SetHMIEnabled(v:Boolean);
    procedure SetDefaultIndex(v:integer);
    function  GetIndex:Integer;
    procedure SetIndex(v:Integer);
  protected
    //: @exclude
    procedure CheckItemIndexChanged; {$IFDEF FPC} override; {$ENDIF}
    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    //: @name retorna qual a opção selecionada.
    property  ItemIndex:Integer read GetIndex Write SetIndex;
    //: @exclude
    property  Enabled:Boolean read GetHMIEnabled write SetHMIEnabled;
    {:
    Tag numérico que será usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    }
    property  PLCTag:TPLCTag read GetHMITag write SetHMITag;
    {:
    Caso o valor inteiro do tag não esteja entre as opções oferecidas, usa o valor
    de @name.
    }
    property  DefaultIndex:Integer read FDefaultIndex write SetDefaultIndex default -1;
    {$IFNDEF FPC}
    property  OnClick:TNotifyEvent read FOnClick write FOnClick;
    {$ENDIF}
  end;

implementation

constructor THMIRadioGroup.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FIgnore:=false;
   FLoaded:=false;
   FDefaultIndex:=-1;
   {$IFNDEF FPC}
   inherited OnClick:= MyClick;
   {$ENDIF}
end;

destructor  THMIRadioGroup.Destroy;
begin
   if FTag<>nil then
      FTag.RemoveChangeCallBack(HMINotifyChangeCallback);
   inherited Destroy;
end;

procedure THMIRadioGroup.HMINotifyChangeCallback(Sender:TObject); //notificação de change do valor do tag
var
   Value:Double;
begin
   Value := 0;
   
   if (FTag as ITagNumeric)<>nil then
      Value := (FTag as ITagNumeric).Value;

   FIgnore:=true;
   if (Value>=0) and (Value<Items.Count) then
      inherited ItemIndex:= FloatToInteger(Value)
   else
      inherited ItemIndex := FDefaultIndex;
   FIgnore:=false;
end;

procedure THMIRadioGroup.RefreshHMISecurity;                      //alquem efetuou login e é necessario verificar autorizações
begin
   //todo
end;

procedure THMIRadioGroup.RemoveHMITag(Sender:TObject);            //Forca a eliminação de referencia do tag.
begin
  if FTag=Sender then
    FTag := nil;
end;

procedure THMIRadioGroup.SetHMITag(t:TPLCTag);                    //seta um tag
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

function  THMIRadioGroup.GetHMITag:TPLCTag;
begin
   Result:=FTag;
end;

function  THMIRadioGroup.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

procedure THMIRadioGroup.SetHMIEnabled(v:Boolean);
begin
   inherited Enabled := v;
   FIsEnabled := v;
end;

procedure THMIRadioGroup.CheckItemIndexChanged;
begin
   {$IFDEF FPC}
   inherited CheckItemIndexChanged;
   {$ENDIF}
   if [csLoading, csReading, csDestroying, csDesigning]*ComponentState<>[] then
      exit;

   if (FLoaded) and (not FIgnore) then
     if (FTag as ITagNumeric)<>nil then
        (FTag as ITagNumeric).Value := ItemIndex;
end;

procedure THMIRadioGroup.Loaded;
begin
   inherited Loaded;
   FLoaded:=true;
end;

procedure THMIRadioGroup.SetDefaultIndex(v:integer);
begin
  if v<(-1) then
     FDefaultIndex:=-1
  else
     FDefaultIndex:=v;
  HMINotifyChangeCallback(Self);
end;

function  THMIRadioGroup.GetIndex:Integer;
begin
   Result := inherited ItemIndex;
end;

procedure THMIRadioGroup.SetIndex(v:Integer);
begin

end;

{$IFNDEF FPC}
procedure THMIRadioGroup.MyClick(Sender:TObject);
begin
   CheckItemIndexChanged;
   if Assigned(FOnClick) then
      FOnClick(Sender);
end;
{$ENDIF}

end.
