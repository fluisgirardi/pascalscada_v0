{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Implementa o controle responsável por mostrar imagens em função do valor do tag associado.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Implements the control that shows images based on the value of the associated tag.)
}
{$ENDIF}
unit HMIAnimation;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Controls, Graphics,
  Dialogs, ExtCtrls, HMIZones, HMITypes, PLCTag, ProtocolTypes, Tag;

type

  {$IFDEF PORTUGUES}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    Implementa o controle responsável por mostrar imagens em função do valor do
    tag associado.

    @bold(Para maiores informações consulte a documentação da classe TImage
    de seu ambiente de desenvolvimento.)
  }
  {$ELSE}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
    Implements the control that shows images based on the value of the
    associated tag.

    @bold(More informations, see the documentation of the class TImage of your
    IDE.)
  }
  {$ENDIF}
  THMIAnimation = class(TImage, IHMIInterface, IHMITagInterface)
  private
    FAnimationZones:TGraphicZones;
    FTag:TPLCTag;
    FIsEnabled:Boolean;
    FTestValue:Double;
    FCurrentZone,
    FOwnerZone:TGraphicZone;
    FTimer:TTimer;
    procedure ZoneChange(Sender:TObject);
    function  GetAnimationZones:TGraphicZones;
    procedure SetAnimationZones(v:TGraphicZones);
    procedure NeedComState(var CurState:TComponentState);
    procedure BlinkTimer(Sender:TObject);

    //Implements the IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    //: @exclude
    procedure SetValue(v:Double);
    //: @exclude
    procedure ShowZone(zone:TGraphicZone);
    //: @exclude
    procedure SetTestValue(v:Double);
    //: @seealso(IHMIInterface.RefreshHMISecurity)
    procedure RefreshHMISecurity;
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

    {$IFDEF PORTUGUES}
    {:
    Propriedade criada para testar o controle sem a necessidade de valores vindos
    de um tag. @bold(Só pode ser usada em tempo de desenvolvimento.)
    }
    {$ELSE}
    {:
    Property created to test the control without values comming from a device.
    @bold(Can only be used in design-time.)
    }
    {$ENDIF}
    property TestValue:Double read FTestValue write SetTestValue stored false;

    {$IFDEF PORTUGUES}
    {:
    Coleção de zonas (imagens) que podem ser exibidas em função do valor do tag.
    @seealso(TZone)
    @seealso(TZones)
    @seealso(TGraphicZone)
    @seealso(TGraphicZones)
    }
    {$ELSE}
    {:
    Zones collection (imagens) that can be showed based on the tag value.
    @seealso(TZone)
    @seealso(TZones)
    @seealso(TGraphicZone)
    @seealso(TGraphicZones)
    }
    {$ENDIF}
    property Zones:TGraphicZones read GetAnimationZones write SetAnimationZones;

    {$IFDEF PORTUGUES}
    {:
    Tag numérico que dispara eventos para o controle exibir imagens de acordo
    com as configurações de zonas.
    @seealso(TPLCTag)
    @seealso(TPLCBlockElement)
    @seealso(TPLCTagNumber)
    }
    {$ELSE}
    {:
    Numeric Tag which their value will choose the image (of the collection) that
    will be showed.
    @seealso(TPLCTag)
    @seealso(TPLCBlockElement)
    @seealso(TPLCTagNumber)
    }
    {$ENDIF}
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;
  end;

implementation

uses hsstrings;

constructor THMIAnimation.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FTimer:=TTimer.Create(Self);
   FTimer.OnTimer:=BlinkTimer;
   FTimer.Enabled:=false;
   FAnimationZones:=TGraphicZones.Create(Self);
   FAnimationZones.OnNeedCompState:=NeedComState;
   FAnimationZones.OnZoneChange:=ZoneChange;
end;

destructor THMIAnimation.Destroy;
begin
   if FTag<>nil then
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   FTimer.Destroy;
   FAnimationZones.Destroy;
   inherited Destroy;
end;

procedure THMIAnimation.ZoneChange(Sender:TObject);
begin
   if [csReading]*ComponentState<>[] then exit;

   NotifyTagChange(self);
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

procedure THMIAnimation.SetValue(v:Double);
begin
   FOwnerZone:=FAnimationZones.GetZoneFromValue(v) as TGraphicZone;
   FTimer.Enabled:=false;
   ShowZone(FOwnerZone);
   if FCurrentZone<>nil then begin
      FTimer.Interval := FOwnerZone.BlinkTime;
      FTimer.Enabled := FOwnerZone.BlinkWith<>(-1);
   end;
end;

procedure THMIAnimation.ShowZone(zone:TGraphicZone);
{$IFNDEF FPC}
var
   x:TPicture;
{$ENDIF}
begin
   FCurrentZone:=zone;
   {$IFDEF FPC}
   //limpa a imagem
   //Clears the image
   Picture.Clear;
   {$ELSE}
   x:= TPicture.Create;
   self.Picture.Assign(x);
   x.Destroy;
   {$ENDIF}

   if zone<>nil then begin
      if zone.ImageListAsDefault then begin
         if Assigned(zone.ImageList) AND (zone.ImageIndex<>-1) then begin
            zone.ImageList.GetBitmap(zone.ImageIndex, Picture.Bitmap);
            Repaint;
         end else
            if FileExists(zone.FileName) then
               Picture.LoadFromFile(zone.FileName);
      end else begin
         if FileExists(zone.FileName) then
            Picture.LoadFromFile(zone.FileName)
         else
            if Assigned(zone.ImageList) AND (zone.ImageIndex<>-1) then begin
               zone.ImageList.GetBitmap(zone.ImageIndex, Picture.Bitmap);
               Repaint;
            end;
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

procedure THMIAnimation.SetHMITag(t:TPLCTag);
begin
   //se o tag esta entre um dos aceitos.
   //if the new tag is valid.
   if (t<>nil) and (not Supports(t, ITagNumeric)) then
      raise Exception.Create(SonlyNumericTags);

   //se ja estou associado a um tag, remove
   //if the control are linked with some tag, remove the link.
   if FTag<>nil then begin
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   end;

   //adiona o callback para o novo tag
   //link with the new tag.
   if t<>nil then begin
      t.AddCallBacks(Self As IHMITagInterface);
      FTag := t;
      NotifyTagChange(self);
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
  NotifyTagChange(Self);
end;

procedure THMIAnimation.BlinkTimer(Sender:TObject);
begin
  if FCurrentZone.BlinkWith<0 then
    FTimer.Enabled:=false
  else begin
    if FTimer.Interval<>TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]).BlinkTime then
      FTimer.Interval := TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]).BlinkTime;
    ShowZone(TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]));
    if not FTimer.Enabled then FTimer.Enabled:=true;
  end;
end;

procedure THMIAnimation.NotifyReadOk;
begin

end;

procedure THMIAnimation.NotifyReadFault;
begin

end;

procedure THMIAnimation.NotifyWriteOk;
begin

end;

procedure THMIAnimation.NotifyWriteFault;
begin
  NotifyTagChange(Self);
end;

procedure THMIAnimation.NotifyTagChange(Sender:TObject);
begin
   if [csReading]*ComponentState=[] then begin

      if FTag=nil then exit;

      if Supports(FTag, ITagNumeric) then
         SetValue((FTag as ITagNumeric).Value)
   end;
end;

procedure THMIAnimation.RemoveTag(Sender:TObject);
begin
  FTag:=nil;
end;

end.
