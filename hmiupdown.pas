//: Implementa um controle em forma de Up/Down para escrita de valores em tags numéricos.
unit HMIUpDown;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, {$ENDIF} Forms, Controls, Graphics,
  Dialogs, ExtCtrls, HMITypes, PLCTag, ProtocolTypes, ComCtrls;

type
   //: Implementa um controle em forma de Up/Down para escrita de valores em tags numéricos.
  THMIUpDown = class(TUpDown, IHMIInterface)
  private
    FTag:TPLCTag;
    FIsEnabled:Boolean;
    FPosition, FIncrement:Double;
    FMax,FMin:Double;
    FEnableMax, FEnableMin:Boolean;
    procedure HMINotifyChangeCallback(Sender:TObject); //notificação de change do valor do tag
    procedure RefreshHMISecurity;                      //alquem efetuou login e é necessario verificar autorizações
    procedure RemoveHMITag(Sender:TObject);            //Forca a eliminação de referencia do tag.
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    function  GetHMITag:TPLCTag;
    function  GetHMIEnabled:Boolean;
    procedure SetHMIEnabled(v:Boolean);
    procedure SetPosition(v:Double);
    procedure SetIncrement(v:Double);
    procedure SetMax(v:Double);
    procedure SetMin(v:Double);
  protected
    //: @exclude
    procedure Loaded; override;
    //: @exclude
    procedure Click(Button: TUDBtnType); override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    {:
    Tag numérico que será usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    }
    property PLCTag:TPLCTag read FTag write SetHMITag;
    //: Valor máximo que o controle pode atingir caso EnableMax for igual a @true.
    property Max:Double read FMax write SetMax;
    //: Valor minimo que o controle pode atingir caso EnableMin for igual a @true.
    property Min:Double read FMin write SetMin;
    //: Valor que será incrementado/decrementado a cada clique no controle.
    property Increment:Double read FIncrement write SetIncrement;
    //: Valor atual do controle.
    property Position:Double read FPosition write SetPosition;
    //: Habilita/desabilita um valor máximo para o cotrole.
    property EnableMax:Boolean read FEnableMax write FEnableMax default false;
    //: Habilita/desabilita um valor minimo para o cotrole.
    property EnableMin:Boolean read FEnableMin write FEnableMin default false;    
  end;

implementation

constructor THMIUpDown.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if csDesigning in ComponentState then begin
    FIncrement := 1;
    FPosition := 0;
    FMax := 100;
    FMin := 0;
  end else begin
    FIncrement := 0;
    FPosition  := 0;
    FMax       := 0;
    FMin       := 0;
  end;
  inherited Position:=50;
  FEnableMin := false;
  FEnableMax := false;
end;

destructor THMIUpDown.Destroy;
begin
   if FTag<>nil then
      FTag.RemoveChangeCallBack(HMINotifyChangeCallback);
   inherited Destroy;
end;

procedure THMIUpDown.HMINotifyChangeCallback(Sender:TObject);
begin
  if (FTag as ITagNumeric) <> nil then
     FPosition := (FTag as ITagNumeric).Value;
      
  inherited Position:=50;
end;

procedure THMIUpDown.RefreshHMISecurity;
begin

end;

procedure THMIUpDown.RemoveHMITag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

procedure THMIUpDown.SetHMITag(t:TPLCTag);
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

function  THMIUpDown.GetHMITag:TPLCTag;
begin
   Result:=FTag;
end;

function  THMIUpDown.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

procedure THMIUpDown.SetHMIEnabled(v:Boolean);
begin
   inherited Enabled := v;
   FIsEnabled := v;
end;

procedure THMIUpDown.Loaded;
begin
  inherited Loaded;
  HMINotifyChangeCallback(Self);
end;

procedure THMIUpDown.Click(Button: TUDBtnType);
var
   Value:Double;
begin
   if FTag=nil then exit;
   
   inherited Position:=50;
     
   case Button of
     btNext:
        Value := Position+FIncrement;
     else
        Value := Position-FIncrement;
   end;

   if (FEnableMax And (Value>FMax)) then
      Value := FMax;
      
   if (FEnableMin And (Value<FMin)) then
      Value := FMin;

   if (FTag as ITagNumeric)<>nil then
      (FTag as ITagNumeric).Value := Value;
end;

procedure THMIUpDown.SetPosition(v:Double);
begin

end;

procedure THMIUpDown.SetIncrement(v:Double);
begin
   if (Increment<=0) and ([csReading, csLoading]*ComponentState=[]) then
      raise Exception.Create('Incremento deve ser um valor maior q zero!');

   FIncrement := v;
end;

procedure THMIUpDown.SetMax(v:Double);
begin
  if v<=FMin then
     raise Exception.Create('O valor máximo precisa ser maior que o mínimo!');

  FMax := v;
end;

procedure THMIUpDown.SetMin(v:Double);
begin
  if v>=FMax then
     raise Exception.Create('O valor mínimo precisa ser menor que o máximo!');

  FMin := v;
end;

end.
