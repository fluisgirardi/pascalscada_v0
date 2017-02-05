{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementa o controle responsável por mostrar textos pré-definidos
            em função do valor do tag associado.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a control that shows a predefined text
            depending of the value of the tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIText;

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Controls, Graphics,
  Dialogs, HMILabel, PLCTag, HMIZones, ProtocolTypes, StdCtrls, ExtCtrls;

type
  {$IFDEF PORTUGUES}
  {:
    @abstract(Classe de controle que mostra um texto pré-definido em função do
              valor do tag associado.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
    @abstract(Class of control that shows a predefined text depending of the
              value of the tag.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  THMIText = class(THMILabel)
  private
    FTextZones:TTextZones;
    FTestValue:Double;
    FCurrentZone:TTextZone;
    FOwnerZoneShowed:Boolean;
    function  GetTextZones:TTextZones;
    procedure SetTextZones(zt:TTextZones);
    procedure ZoneChange(Sender:TObject);
    procedure NeedComState(var CurState:TComponentState);
    procedure BlinkTimer(Sender:TObject);
  protected

    {$IFDEF PORTUGUES}
    {:
    Escolhe (dependendo da condição de cada zona de texto) e mostra uma zona de texto.
    }
    {$ELSE}
    {:
    Choose (depending of the condition of each text zone) and shows a text zone.
    }
    {$ENDIF}
    procedure SetValue(v:Double);

    {$IFDEF PORTUGUES}
    {:
    Mostra uma zona de texto.
    }
    {$ELSE}
    {:
    Shows a text zone.
    }
    {$ENDIF}
    procedure ShowZone(zone:TTextZone);

    {$IFDEF PORTUGUES}
    {:
    Mostra uma zona de texto a partir do valor de teste.
    }
    {$ELSE}
    {:
    Shows a text zone depending of the value of TestValue property.
    }
    {$ENDIF}
    procedure SetTestValue(v:Double);

    //sobrescreve alguns métodos para permitir somente tags numéricos.
    //
    //override some procedures to allow only numeric tags in control.

    //: @exclude
    procedure RefreshTagValue; override;
    //: @exclude
    procedure SetHMITag(t:TPLCTag); override;
    //: @exclude
    procedure WriteFaultCallBack(Sender:TObject); override;
    //: @exclude
    procedure TagChangeCallBack(Sender:TObject); override;


    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
    procedure RefreshText(Data: PtrInt);
  published
    {$IFDEF PORTUGUES}
    {:
    Propriedade criada para testar o controle sem a necessidade de valores vindos
    de um tag. @bold(Só pode ser usada em tempo de desenvolvimento.)
    }
    {$ELSE}
    {:
    Use this property to test the control without values coming from your device.
    @bold(Can only be used on desing-time.)
    }
    {$ENDIF}
    property TestValue:Double read FTestValue write SetTestValue stored false;

    {$IFDEF PORTUGUES}
    {:
    Coleção de zonas (textos) que podem ser exibidos em função do valor do tag.
    @seealso(TZone)
    @seealso(TZones)
    @seealso(TTextZone)
    @seealso(TTextZones)
    }
    {$ELSE}
    {:
    Collection of texts that can be show depending of the value of the tag.
    @seealso(TZone)
    @seealso(TZones)
    @seealso(TTextZone)
    @seealso(TTextZones)
    }
    {$ENDIF}
    property Zones:TTextZones read GetTextZones write SetTextZones nodefault;
  end;

implementation

uses hsstrings, Forms, hmi_animation_timers;

constructor THMIText.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FTextZones:=TTextZones.Create(Self);
   FTextZones.OnNeedCompState:=@NeedComState;
   FTextZones.OnCollectionItemChange:=@ZoneChange;
end;

destructor THMIText.Destroy;
begin
   Application.RemoveAsyncCalls(Self);
   GetAnimationTimer.RemoveCallbacksFromObject(Self);

   FreeAndNil(FTextZones);

   inherited Destroy;
end;

procedure THMIText.RefreshText(Data: PtrInt);
var
   value:Double;
begin
   value := 0;
   if [csReading]*ComponentState=[] then begin

      if (FTag<>nil) AND Supports(FTag, ITagNumeric) then
         value := (FTag as ITagNumeric).Value;
   end;
   SetValue(value);
end;

procedure THMIText.ZoneChange(Sender:TObject);
begin
   RefreshText(0);
end;

procedure THMIText.NeedComState(var CurState:TComponentState);
begin
   CurState:=ComponentState;
end;

procedure THMIText.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  //
  //check if the tag is valid (only numeric tags)
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  inherited SetHMITag(t);
end;

procedure THMIText.WriteFaultCallBack(Sender:TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIText.TagChangeCallBack(Sender:TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    Application.QueueAsyncCall(@RefreshText,0);
end;

procedure THMIText.SetValue(v:Double);
begin
   FCurrentZone:=FTextZones.GetZoneFromValue(v) as TTextZone;
   GetAnimationTimer.RemoveCallback(@BlinkTimer);
   ShowZone(FCurrentZone);
   FOwnerZoneShowed:=true;
   if (FCurrentZone<>nil) and (FCurrentZone.BlinkWith<>(-1)) and (FCurrentZone.BlinkTime>0) then begin
     GetAnimationTimer.AddTimerCallback(FCurrentZone.BlinkTime,@BlinkTimer);
   end;
end;

procedure THMIText.RefreshTagValue;
begin
  RefreshText(0);
end;

procedure THMIText.ShowZone(zone:TTextZone);
begin
  FCurrentZone:=zone;
  if zone=nil then begin
    TLabel(Self).Caption := '';
    TLabel(Self).Transparent:=true;
  end else begin
    TLabel(self).Caption    := Prefix+zone.Text+Sufix;
    TLabel(self).Color      := zone.Color;
    TLabel(self).Transparent:= zone.Transparent;
    TLabel(self).Font       := zone.Font;
    TLabel(self).Layout     := zone.VerticalAlignment;
    TLabel(self).Alignment  := zone.HorizontalAlignment;
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
   RefreshText(0);
end;

//timer procedure (does a blink effect)
procedure THMIText.BlinkTimer(Sender:TObject);
begin
  if (FCurrentZone.BlinkWith<0) or (TTextZone(FTextZones.Items[FCurrentZone.BlinkWith]).BlinkTime<>FCurrentZone.BlinkTime) then
    GetAnimationTimer.RemoveCallback(@BlinkTimer); //FTimer.Enabled:=false

  if (FCurrentZone.BlinkWith>=0) AND (TTextZone(FTextZones.Items[FCurrentZone.BlinkWith]).BlinkTime<>FCurrentZone.BlinkTime) and (TTextZone(FTextZones.Items[FCurrentZone.BlinkWith]).BlinkTime>0) then
      GetAnimationTimer.AddTimerCallback(TTextZone(FTextZones.Items[FCurrentZone.BlinkWith]).BlinkTime, @BlinkTimer);

  ShowZone(TTextZone(FTextZones.Items[FCurrentZone.BlinkWith]));
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
