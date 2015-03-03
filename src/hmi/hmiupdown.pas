{$i ../common/pscada_settings.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementa um controle em forma de Up/Down para escrita de valores
            em tags numéricos.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a UpDown control to write values on numeric tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIUpDown;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, {$ENDIF} Controls, Graphics,
  Dialogs, ExtCtrls, HMITypes, PLCTag, ProtocolTypes, ComCtrls, Tag;

type

  {$IFDEF PORTUGUES}
  {:
  Implementa um controle em forma de Up/Down para escrita de valores em tags
  numéricos.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Class of UpDown control to write values on numeric tags.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  THMIUpDown = class(TUpDown, IHMIInterface, IHMITagInterface)
  private
    FTag:TPLCTag;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FPosition, FIncrement:Double;
    FMax,FMin:Double;
    FEnableMax, FEnableMin:Boolean;

    FSecurityCode:String;
    procedure SetSecurityCode(sc:String);

    //implements the IHMIInterface interface
    //: @seealso(IHMIInterface.SetHMITag)
    procedure SetHMITag(t:TPLCTag);                    //seta um tag
    //: @seealso(IHMIInterface.GetHMITag)
    function  GetHMITag:TPLCTag;

    //: @seealso(IHMIInterface.GetControlSecurityCode)
     function GetControlSecurityCode:String;
    //: @seealso(IHMIInterface.CanBeAccessed)
    procedure CanBeAccessed(a:Boolean);
    //: @seealso(IHMIInterface.MakeUnsecure)
    procedure MakeUnsecure;

    procedure SetPosition(v:Double);
    procedure SetIncrement(v:Double);
    procedure SetMax(v:Double);
    procedure SetMin(v:Double);

    //implements the IHMITagInterface interface.
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    //: @exclude
    procedure SetEnabled(e:Boolean); override;
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
    //: @exclude
    property Enabled:Boolean read FIsEnabled write SetEnabled;

    {$IFDEF PORTUGUES}
    {:
    Tag numérico que será pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    @seealso(TPLCStructItem)
    }
    {$ELSE}
    {:
    Numeric tag that will be linked with the control.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    @seealso(TPLCStructItem)
    }
    {$ENDIF}
    property PLCTag:TPLCTag read FTag write SetHMITag;

    {$IFDEF PORTUGUES}
    //: Valor máximo que o controle pode atingir caso EnableMax for igual a @true.
    {$ELSE}
    //: Maximum value which control can write on tag if EnableMax is @true.
    {$ENDIF}
    property Max:Double read FMax write SetMax;

    {$IFDEF PORTUGUES}
    //: Valor minimo que o controle pode atingir caso EnableMin for igual a @true.
    {$ELSE}
    //: Minimum value which control can write on tag if EnableMin is @true.
    {$ENDIF}
    property Min:Double read FMin write SetMin;

    {$IFDEF PORTUGUES}
    //: Valor que será incrementado/decrementado a cada clique no controle.
    {$ELSE}
    //: Value that will be incremented/decremented on each click on control.
    {$ENDIF}
    property Increment:Double read FIncrement write SetIncrement;

    {$IFDEF PORTUGUES}
    //: Valor atual do controle.
    {$ELSE}
    //: Actual value of the control.
    {$ENDIF}
    property Position:Double read FPosition write SetPosition;

    {$IFDEF PORTUGUES}
    //: Habilita/desabilita o limite máximo para o cotrole.
    {$ELSE}
    //: Enables/disables the maximum value of the control.
    {$ENDIF}
    property EnableMax:Boolean read FEnableMax write FEnableMax default false;

    {$IFDEF PORTUGUES}
    //: Habilita/desabilita o limite minimo para o cotrole.
    {$ELSE}
    //: Enables/disables the minimum value of the control.
    {$ENDIF}
    property EnableMin:Boolean read FEnableMin write FEnableMin default false;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:String read FSecurityCode write SetSecurityCode;
  end;

implementation

uses hsstrings, HMIControlSecurityManager;

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
  GetHMIControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIUpDown.Destroy;
begin
   if FTag<>nil then
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   GetHMIControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
   inherited Destroy;
end;

procedure THMIUpDown.SetSecurityCode(Sc:String);
begin
  if Trim(sc)='' then
    Self.CanBeAccessed(true)
  else
    with GetHMIControlSecurityManager do begin
      ValidateSecurityCode(sc);
      if not SecurityCodeExists(sc) then
        RegisterSecurityCode(sc);

      Self.CanBeAccessed(CanAccess(sc));
    end;

  FSecurityCode:=sc;
end;

procedure THMIUpDown.SetHMITag(t:TPLCTag);
begin
   //se o tag esta entre um dos aceitos.
   //
   //check if the tag is valid (only numeric tags)
   if (t<>nil) and (not Supports(t, ITagNumeric)) then
      raise Exception.Create(SonlyNumericTags);

   //se ja estou associado a um tag, remove
   //
   //Remove the old link
   if FTag<>nil then begin
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   end;

   //adiona o callback para o novo tag
   //
   //link with the new tag.
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

function THMIUpDown.GetControlSecurityCode:String;
begin
   Result:=FSecurityCode;
end;

procedure THMIUpDown.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure THMIUpDown.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMIUpDown.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
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

   if Supports(FTag, ITagNumeric) then
      (FTag as ITagNumeric).Value := Value;

   inherited Click(Button);
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
  if (FTag<>nil) AND Supports(FTag, ITagNumeric) then
     FPosition := (FTag as ITagNumeric).Value;

  inherited Position:=50;
end;

procedure THMIUpDown.RemoveTag(Sender:TObject);
begin
  if Ftag=Sender then
    FTag:=nil;
end;

end.
