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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

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
    FTimer:TTimer;
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
    procedure NotifyTagChange(Sender:TObject); override;


    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
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

uses hsstrings;

constructor THMIText.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FTimer:=TTimer.Create(Self);
   FTimer.OnTimer:=BlinkTimer;
   FTimer.Enabled:=false;
   FTextZones:=TTextZones.Create(Self);
   FTextZones.OnNeedCompState:=NeedComState;
   FTextZones.OnCollectionItemChange:=ZoneChange;
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
  //
  //check if the tag is valid (only numeric tags)
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  inherited SetHMITag(t);
end;

procedure THMIText.NotifyTagChange(Sender:TObject);
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
  FCurrentZone:=zone;
  if zone=nil then begin
    TLabel(Self).Caption := '';
    TLabel(Self).Transparent:=true;
  end else begin
    TLabel(self).Caption := Prefix+zone.Text+Sufix;
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

//timer procedure (does a blink effect)
procedure THMIText.BlinkTimer(Sender:TObject);
begin
  if FCurrentZone.BlinkWith<0 then
    FTimer.Enabled:=false
  else begin
    FTimer.Enabled:=false;
    FTimer.Interval := TTextZone(FTextZones.Items[FCurrentZone.BlinkWith]).BlinkTime;
    ShowZone(TTextZone(FTextZones.Items[FCurrentZone.BlinkWith]));
    FTimer.Enabled:=true;
  end;
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
