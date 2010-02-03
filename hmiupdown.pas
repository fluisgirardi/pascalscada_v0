{:
  @abstract(Implementa um controle em forma de Up/Down para escrita de valores
            em tags numéricos.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit HMIUpDown;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, {$ENDIF} Forms, Controls, Graphics,
  Dialogs, ExtCtrls, HMITypes, PLCTag, ProtocolTypes, ComCtrls, Tag;

type
   //: Implementa um controle em forma de Up/Down para escrita de valores em tags numéricos.
  THMIUpDown = class(TUpDown, IHMIInterface, IHMITagInterface)
  private
    FTag:TPLCTag;
    FIsEnabled:Boolean;
    FPosition, FIncrement:Double;
    FMax,FMin:Double;
    FEnableMax, FEnableMin:Boolean;

    procedure RefreshHMISecurity;                      //alquem efetuou login e é necessário verificar as autorizações.
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    function  GetHMITag:TPLCTag;
    function  GetHMIEnabled:Boolean;
    procedure SetHMIEnabled(v:Boolean);
    procedure SetPosition(v:Double);
    procedure SetIncrement(v:Double);
    procedure SetMax(v:Double);
    procedure SetMin(v:Double);

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
    //: Habilita/desabilita o limite máximo para o cotrole.
    property EnableMax:Boolean read FEnableMax write FEnableMax default false;
    //: Habilita/desabilita o limite minimo para o cotrole.
    property EnableMin:Boolean read FEnableMin write FEnableMin default false;    
  end;

implementation

uses hsstrings;

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
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   inherited Destroy;
end;

procedure THMIUpDown.RefreshHMISecurity;
begin

end;

procedure THMIUpDown.SetHMITag(t:TPLCTag);
begin
   //se o tag esta entre um dos aceitos.
   if (t<>nil) and ((t as ITagNumeric)=nil) then
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
  NotifyTagChange(Self);
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
      raise Exception.Create(SincrementMustBeGreaterThanZero);

   FIncrement := v;
end;

procedure THMIUpDown.SetMax(v:Double);
begin
  if ([csLoading]*ComponentState=[]) and (v<=FMin) then
     raise Exception.Create(SmaxMustBeGreaterThanMin);

  FMax := v;
end;

procedure THMIUpDown.SetMin(v:Double);
begin
  if ([csLoading]*ComponentState=[]) and (v>=FMax) then
     raise Exception.Create(SminMustBeLessThanMax);

  FMin := v;
end;

procedure THMIUpDown.NotifyReadOk;
begin

end;

procedure THMIUpDown.NotifyReadFault;
begin

end;

procedure THMIUpDown.NotifyWriteOk;
begin

end;

procedure THMIUpDown.NotifyWriteFault;
begin
  NotifyTagChange(Self);
end;

procedure THMIUpDown.NotifyTagChange(Sender:TObject);
begin
  if (FTag as ITagNumeric) <> nil then
     FPosition := (FTag as ITagNumeric).Value;

  inherited Position:=50;
end;

procedure THMIUpDown.RemoveTag(Sender:TObject);
begin
  if Ftag=Sender then
    FTag:=nil;
end;

end.
