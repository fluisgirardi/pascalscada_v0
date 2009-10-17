{:
Implementa o controle responsável por mostrar textos pré-definidos em função
do valor do tag associado.
}
unit HMIText;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics,
  Dialogs, HMILabel, PLCTag, HMIZones, ProtocolTypes, StdCtrls, ExtCtrls;

type
  {:
  Implementa o controle responsável por mostrar textos pré-definidos em função
  do valor do tag associado.
  @seealso(THMILabel)
  }
  THMIText = class(THMILabel)
  private
    FTextZones:TTextZones;
    FTestValue:Double;
    FCurrentZone:TTextZone;
    FTimer:TTimer;
    FOwnerZoneShowed:Boolean;
    function  GetTextZones:TTextZones;
    procedure SetTextZones(zt:TTextZones);
    procedure ZoneChange(Sender:TObject);
    procedure NeedComState(var CurState:TComponentState);
    procedure BlinkTimer(Sender:TObject);
  protected
    //: @exclude
    procedure RefreshTagValue; override;
    //: @exclude
    procedure SetValue(v:Double);
    //: @exclude
    procedure ShowZone(zone:TTextZone);
    //: @exclude
    procedure SetTestValue(v:Double);
    //: @exclude
    procedure SetHMITag(t:TPLCTag); override;
    //: @exclude
    procedure NotifyTagChange(Sender:TObject); override;
    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
  published
    {:
    Propriedade criada para testar o controle sem a necessidade de valores vindos
    de um tag. @bold(Só pode ser usada em tempo de desenvolvimento.)
    }
    property TestValue:Double read FTestValue write SetTestValue stored false;
    {:
    Coleção de zonas (textos) que podem ser exibidos em função do valor do tag.
    @seealso(TZone)
    @seealso(TZones)
    @seealso(TTextZone)
    @seealso(TTextZones)
    }
    property Zones:TTextZones read GetTextZones write SetTextZones nodefault;
  end;

implementation

constructor THMIText.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FTimer:=TTimer.Create(Self);
   FTimer.OnTimer:=BlinkTimer;
   FTimer.Enabled:=false;
   FTextZones:=TTextZones.Create;
   FTextZones.OnNeedCompState:=NeedComState;
   FTextZones.OnZoneChange:=ZoneChange;
end;

destructor THMIText.Destroy;
begin
   FTimer.Destroy;
   FTextZones.Destroy;
   inherited Destroy;
end;

procedure THMIText.ZoneChange(Sender:TObject);
begin
   NotifyTagChange(Self);
end;

procedure THMIText.NeedComState(var CurState:TComponentState);
begin
   CurState:=ComponentState;
end;

procedure THMIText.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  if ((t as ITagNumeric)=nil) then
     raise Exception.Create('Somente tags numéricos são aceitos!');

  inherited SetHMITag(t);
end;

procedure THMIText.NotifyTagChange(Sender:TObject);
var
   value:Double;
begin
   value := 0;
   if [csDesigning, csReading]*ComponentState=[] then begin
   
      if (FTag as ITagNumeric)<>nil then
         value := (FTag as ITagNumeric).Value;
   end;
   SetValue(value);
end;

procedure THMIText.SetValue(v:Double);
begin
   FCurrentZone:=FTextZones.GetZoneFromValue(v) as TTextZone;
   FTimer.Enabled:=false;
   ShowZone(FCurrentZone);
   FOwnerZoneShowed:=true;
   if FCurrentZone<>nil then begin
      FTimer.Interval := FCurrentZone.BlinkTime;
      FTimer.Enabled := FCurrentZone.BlinkWith<>(-1);
   end;
end;

procedure THMIText.RefreshTagValue;
begin
  NotifyTagChange(self);
end;

procedure THMIText.ShowZone(zone:TTextZone);
begin
   if zone=nil then begin
      TLabel(Self).Caption := '';
      TLabel(Self).Transparent:=true;
   end else begin
      TLabel(self).Caption := zone.Text;
      TLabel(self).Color:=zone.Color;
      TLabel(self).Transparent:=zone.Transparent;
      TLabel(self).Font.Assign(zone.Font);
      TLabel(self).Layout:=zone.VerticalAlignment;
      TLabel(self).Alignment:=zone.HorizontalAlignment;
   end;
end;

procedure THMIText.SetTestValue(v:Double);
begin
   if [csDesigning]*ComponentState=[] then exit;

   FTestValue:=v;
   SetValue(v);
end;

procedure THMIText.Loaded;
begin
   inherited Loaded;
   FTextZones.Loaded;
   NotifyTagChange(Self);
end;

procedure THMIText.BlinkTimer(Sender:TObject);
begin
   if FOwnerZoneShowed then begin
      if FCurrentZone.BlinkWith<>(-1) then begin
         ShowZone(TTextZone(FTextZones.Items[FCurrentZone.BlinkWith]));
         FTimer.Enabled:=false;
         FTimer.Interval := TTextZone(FTextZones.Items[FCurrentZone.BlinkWith]).BlinkTime;
         FTimer.Enabled:=true;
      end
   end else begin
      ShowZone(FCurrentZone);
      FTimer.Enabled:=false;
      FTimer.Interval := FCurrentZone.BlinkTime;
      FTimer.Enabled:=true;
   end;
   FOwnerZoneShowed:= not FOwnerZoneShowed;
end;

function THMIText.GetTextZones:TTextZones;
begin
   Result := FTextZones;
end;

procedure THMIText.SetTextZones(zt:TTextZones);
begin
   FTextZones.Assign(zt);
end;

end.
