{$i ../common/language.inc}
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

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Controls, Graphics,
  Dialogs, ExtCtrls, HMIZones, HMITypes, PLCTag, ProtocolTypes, Tag;

type
  TZoneChanged = procedure(Sender:TObject; ZoneIndex:Integer) of object;

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
  THMIAnimation = class(TCustomImage, IHMIInterface)
  private
    FAnimationZones:TGraphicZones;
    FTag:TPLCTag;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FTestValue:Double;
    FCurrentZone,
    FOwnerZone:TGraphicZone;

    FSecurityCode:UTF8String;
    FZoneChanged: TZoneChanged;
    function GetAnimationZone: TAnimationZone;
    procedure SetSecurityCode(sc:UTF8String);

    procedure ZoneChange(Sender:TObject);
    function  GetAnimationZones:TGraphicZones;
    procedure SetAnimationZones(v:TGraphicZones);
    procedure NeedComState(var CurState:TComponentState);
    procedure BlinkTimer(Sender:TObject);

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
  protected
    //: @exclude
    procedure ShowZone(zone:TGraphicZone);
    //: @exclude
    procedure SetTestValue(v:Double);

    //: @seealso(IHMIInterface.SetHMITag)
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
     function GetControlSecurityCode:UTF8String;
    //: @seealso(IHMIInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean);
    //: @seealso(IHMIInterface.MakeUnsecure)
    procedure MakeUnsecure;

    //: @exclude
    procedure SetEnabled(e:Boolean); override;

    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    procedure RefreshAnimation(Data: PtrInt);
    //: @exclude
    procedure SetValue(v:Double);
    property CurrentAnimationZone:TAnimationZone read GetAnimationZone;
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
    //: @exclude
    property Enabled:Boolean read FIsEnabled write SetEnabled;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;

    property AntialiasingMode;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Center;
    property Constraints;
    property DragCursor;
    property DragMode;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnPictureChanged;
    property OnResize;
    property OnStartDrag;
    property ParentShowHint;
    property PopupMenu;
    property Proportional;
    property ShowHint;
    property Stretch;
    property Transparent;
    property Visible;
    property ZoneChanged:TZoneChanged read FZoneChanged write FZoneChanged;
  end;

implementation

uses hsstrings, ControlSecurityManager, Forms, hmi_animation_timers;

constructor THMIAnimation.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FIsEnabled:=true;
   FAnimationZones:=TGraphicZones.Create(Self);
   FAnimationZones.OnNeedCompState:=@NeedComState;
   FAnimationZones.OnCollectionItemChange:=@ZoneChange;
   GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIAnimation.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  GetAnimationTimer.RemoveCallbacksFromObject(Self);

  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);

  FreeAndNil(FAnimationZones);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

procedure THMIAnimation.RefreshAnimation(Data: PtrInt);
begin
   if [csReading]*ComponentState=[] then begin
      if FTag=nil then exit;

      if Supports(FTag, ITagNumeric) then
         SetValue((FTag as ITagNumeric).Value)
   end;
end;

procedure THMIAnimation.SetSecurityCode(sc: UTF8String);
begin
  if Trim(sc)='' then
    Self.CanBeAccessed(true)
  else
    with GetControlSecurityManager do begin
      ValidateSecurityCode(sc);
      if not SecurityCodeExists(sc) then
        RegisterSecurityCode(sc);

      Self.CanBeAccessed(CanAccess(sc));
    end;

  FSecurityCode:=sc;
end;

function THMIAnimation.GetAnimationZone: TAnimationZone;
begin
  Result:=FCurrentZone;
end;

procedure THMIAnimation.ZoneChange(Sender:TObject);
begin
   if [csReading]*ComponentState<>[] then exit;

   RefreshAnimation(0);
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
   FCurrentZone:=FAnimationZones.GetZoneFromValue(v) as TGraphicZone;
   GetAnimationTimer.RemoveCallback(@BlinkTimer);
   ShowZone(FCurrentZone);
   //FOwnerZoneShowed:=true;
   if (FCurrentZone<>nil) and (FCurrentZone.BlinkWith<>(-1)) and (FCurrentZone.BlinkTime>0) then begin
     GetAnimationTimer.AddTimerCallback(FCurrentZone.BlinkTime,@BlinkTimer);
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

      if Assigned(FZoneChanged) then
         FZoneChanged(Self, zone.Index);
   end else
     if Assigned(FZoneChanged) then
        FZoneChanged(Self, -1);
end;

procedure THMIAnimation.SetTestValue(v:Double);
begin
   if [csDesigning]*ComponentState=[] then exit;

   FTestValue:=v;
   SetValue(v);
end;

function THMIAnimation.GetControlSecurityCode: UTF8String;
begin
   Result:=FSecurityCode;
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
      FTag.RemoveAllHandlersFromObject(Self);
   end;

   //adiona o callback para o novo tag
   //link with the new tag.
   if t<>nil then begin
      t.AddRemoveTagHandler(@RemoveTagCallBack);
      t.AddTagChangeHandler(@TagChangeCallBack);
      t.AddWriteFaultHandler(@WriteFaultCallBack);
      FTag := t;
      RefreshAnimation(0);
   end;
   FTag := t;
end;

function  THMIAnimation.GetHMITag:TPLCTag;
begin
  Result:=FTag;
end;

procedure THMIAnimation.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity :=a;
  SetEnabled(FIsEnabled);
end;

procedure THMIAnimation.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMIAnimation.Loaded;
begin
  inherited Loaded;
  FAnimationZones.Loaded;
  SetValue(0);
end;

procedure THMIAnimation.BlinkTimer(Sender:TObject);
begin
  if (FCurrentZone.BlinkWith<0) or (TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]).BlinkTime<>FCurrentZone.BlinkTime) then
    GetAnimationTimer.RemoveCallback(@BlinkTimer); //FTimer.Enabled:=false

  if (FCurrentZone.BlinkWith>=0) AND (TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]).BlinkTime<>FCurrentZone.BlinkTime) and (TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]).BlinkTime>0) then
      GetAnimationTimer.AddTimerCallback(TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]).BlinkTime, @BlinkTimer);

  ShowZone(TGraphicZone(FAnimationZones.Items[FCurrentZone.BlinkWith]));
end;

procedure THMIAnimation.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIAnimation.TagChangeCallBack(Sender: TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    Application.QueueAsyncCall(@RefreshAnimation,0);
end;

procedure THMIAnimation.RemoveTagCallBack(Sender: TObject);
begin
  FTag:=nil;
end;

procedure THMIAnimation.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

end.
