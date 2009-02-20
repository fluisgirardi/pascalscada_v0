//: Implementa o controle responsável por mostrar imagens em função do valor do tag associado.
unit HMIAnimation;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics,
  Dialogs, ExtCtrls, HMIZones, HMITypes, PLCTag, ProtocolTypes;

type
  //: Implementa o controle responsável por mostrar imagens em função do valor do tag associado.
  THMIAnimation = class(TImage, IHMIInterface)
  private
    FAnimationZones:TGraphicZones;
    FTag:TPLCTag;
    FIsEnabled:Boolean;
    FTestValue:Double;
    FCurrentZone:TGraphicZone;
    FTimer:TTimer;
    FOwnerZoneShowed:Boolean;    
    procedure ZoneChange(Sender:TObject);
    function  GetAnimationZones:TGraphicZones;
    procedure SetAnimationZones(v:TGraphicZones);
    procedure NeedComState(var CurState:TComponentState);
    procedure BlinkTimer(Sender:TObject);
  protected
    //: @exclude
    procedure SetValue(v:Double);
    //: @exclude
    procedure ShowZone(zone:TGraphicZone);
    //: @exclude
    procedure SetTestValue(v:Double);
    //: @seealso(IHMIInterface.HMINotifyChangeCallback)
    procedure HMINotifyChangeCallback(Sender:TObject);
    //: @seealso(IHMIInterface.RefreshHMISecurity)
    procedure RefreshHMISecurity;
    //: @seealso(IHMIInterface.RemoveHMITag)
    procedure RemoveHMITag(Sender:TObject);            //Forca a eliminação de referencia do tag.
    //: @seealso(IHMIInterface.SetHMITag)
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag;
    //: @seealso(IHMIInterface.GetHMIEnabled)
    function  GetHMIEnabled:Boolean;
    //: @seealso(IHMIInterface.SetHMIEnabled)
    procedure SetHMIEnabled(v:Boolean);
    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    {:
    Propriedade criada para testar o controle sem a necessidade de valores vindos
    de um tag. @bold(Só pode ser usada em tempo de desenvolvimento.)
    }
    property TestValue:Double read FTestValue write SetTestValue stored false;
    {:
    Coleção de zonas (imagens) que podem ser exibidas em função do valor do tag.
    @seealso(TZone)
    @seealso(TZones)
    @seealso(TGraphicZone)
    @seealso(TGraphicZones)
    }
    property Zones:TGraphicZones read GetAnimationZones write SetAnimationZones;
    {:
    Tag numérico que dispara eventos para o controle exibir imagens de acordo
    com as configurações de zonas.
    @seealso(TPLCTag)
    @seealso(TPLCBlockElement)
    @seealso(TPLCTagNumber)
    }
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;
  end;

implementation

constructor THMIAnimation.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FTimer:=TTimer.Create(Self);
   FTimer.OnTimer:=BlinkTimer;
   FTimer.Enabled:=false;
   FAnimationZones:=TGraphicZones.Create;
   FAnimationZones.OnNeedCompState:=NeedComState;
   FAnimationZones.OnZoneChange:=ZoneChange;
end;

destructor THMIAnimation.Destroy;
begin
   if FTag<>nil then
      FTag.RemoveChangeCallBack(HMINotifyChangeCallback);
   FTimer.Destroy;
   FAnimationZones.Destroy;
   inherited Destroy;
end;

procedure THMIAnimation.ZoneChange(Sender:TObject);
begin
   if [csReading, csDesigning]*ComponentState<>[] then exit;

   HMINotifyChangeCallback(self);
end;

function  THMIAnimation.GetAnimationZones:TGraphicZones;
begin
   Result := FAnimationZones;
end;

procedure THMIAnimation.SetAnimationZones(v:TGraphicZones);
begin
   FAnimationZones.Assign(v);
end;

procedure THMIAnimation.NeedComState(var CurState:TComponentState);
begin
   CurState:=ComponentState;
end;

procedure THMIAnimation.HMINotifyChangeCallback(Sender:TObject);
var
   tag:ITagNumeric;
begin
   if [csDesigning, csReading]*ComponentState=[] then begin

      if FTag=nil then exit;
      
      tag := FTag as ITagNumeric;
      if Tag<>nil then
         SetValue(Tag.Value)
   end;
end;

procedure THMIAnimation.SetValue(v:Double);
begin
   FCurrentZone:=FAnimationZones.GetZoneFromValue(v) as TGraphicZone;
   FTimer.Enabled:=false;
   ShowZone(FCurrentZone);
   FOwnerZoneShowed:=true;
   if FCurrentZone<>nil then begin
      FTimer.Interval := FCurrentZone.BlinkTime;
      FTimer.Enabled := FCurrentZone.BlinkWith<>(-1);
   end;
end;

procedure THMIAnimation.ShowZone(zone:TGraphicZone);
{$IFNDEF FPC}
var
   x:TPicture;
{$ENDIF}
begin
   //limpa a imagem
   {$IFDEF FPC}
   Picture.Clear;
   {$ELSE}
   x:= TPicture.Create;
   self.Picture.Assign(x);
   x.Destroy;
   {$ENDIF}

   if zone<>nil then begin
      if zone.ImageListAsDefault then begin
         if Assigned(zone.ImageList) AND (zone.ImageIndex<>-1) then
            zone.ImageList.GetBitmap(zone.ImageIndex, Picture.Bitmap)
         else
            if FileExists(zone.FileName) then
               Picture.LoadFromFile(zone.FileName);
      end else begin
         if FileExists(zone.FileName) then
            Picture.LoadFromFile(zone.FileName)
         else
            if Assigned(zone.ImageList) AND (zone.ImageIndex<>-1) then
               zone.ImageList.GetBitmap(zone.ImageIndex, Picture.Bitmap);
      end;
      Transparent := zone.Transparent;
      if zone.Transparent then
         Picture.Bitmap.TransparentColor:=zone.TransparentColor;
   end;
end;

procedure THMIAnimation.SetTestValue(v:Double);
begin
   if [csDesigning]*ComponentState=[] then exit;

   FTestValue:=v;
   SetValue(v);
end;

procedure THMIAnimation.RefreshHMISecurity;
begin
   //todo
end;

procedure THMIAnimation.RemoveHMITag(Sender:TObject);
begin
   FTag:=nil;
end;

procedure THMIAnimation.SetHMITag(t:TPLCTag);
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

function  THMIAnimation.GetHMITag:TPLCTag;
begin
  Result:=FTag;
end;

function  THMIAnimation.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

procedure THMIAnimation.SetHMIEnabled(v:Boolean);
begin
   inherited Enabled := v;
   FIsEnabled := v;
end;

procedure THMIAnimation.Loaded;
begin
  inherited Loaded;
  FAnimationZones.Loaded;
  HMINotifyChangeCallback(Self);
end;

procedure THMIAnimation.BlinkTimer(Sender:TObject);
begin
   if FOwnerZoneShowed then begin
      if FCurrentZone.BlinkWith<>(-1) then begin
         ShowZone(TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]));
         FTimer.Enabled:=false;
         FTimer.Interval := TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]).BlinkTime;
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

end.
