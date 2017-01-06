{$i ../common/language.inc}
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
  THMIUpDown = class(TUpDown, IHMIInterface)
  private
    FAfterSendValueToTag: TAfterSendNumericValueToTagEvent;
    FBeforeSendValueToTag: TBeforeSendNumericValueToTagEvent;
    FTag:TPLCTag;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FPosition, FIncrement:Double;
    FMax,FMin:Double;
    FEnableMax, FEnableMin:Boolean;

    FSecurityCode:UTF8String;
    procedure SetSecurityCode(sc:UTF8String);

    //implements the IHMIInterface interface
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

    procedure SetPosition(v:Double);
    procedure SetIncrement(v:Double);
    procedure SetMax(v:Double);
    procedure SetMin(v:Double);

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
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
    procedure RefreshUpDown(Data: PtrInt);
  published
    property Align;
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
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;

    {$IFDEF PORTUGUES}
    //: Evento disparado antes do HMIEdit enviar um valor ao tag associado
    {$ELSE}
    //: Event triggered before HMIEdit send a value to linked tag.
    {$ENDIF}
    property BeforeSendAValueToTag:TBeforeSendNumericValueToTagEvent read FBeforeSendValueToTag write FBeforeSendValueToTag;

    {$IFDEF PORTUGUES}
    //: Evento disparado quando o HMIEdit enviou um valor ao tag associado
    {$ELSE}
    //: Event triggered when the HMIEdit sent a value to linked tag.
    {$ENDIF}
    property AfterSendValueToTag:TAfterSendNumericValueToTagEvent read FAfterSendValueToTag write FAfterSendValueToTag;
  end;

implementation

uses hsstrings, ControlSecurityManager, Forms;

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
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIUpDown.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

procedure THMIUpDown.RefreshUpDown(Data: PtrInt);
begin
   if (FTag<>nil) AND Supports(FTag, ITagNumeric) then
      FPosition := (FTag as ITagNumeric).Value;

   inherited Position:=50;
end;

procedure THMIUpDown.SetSecurityCode(sc: UTF8String);
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
      FTag.RemoveAllHandlersFromObject(Self);
   end;

   //adiona o callback para o novo tag
   //
   //link with the new tag.
   if t<>nil then begin
      t.AddWriteFaultHandler(@WriteFaultCallBack);
      t.AddTagChangeHandler(@TagChangeCallBack);
      t.AddRemoveTagHandler(@RemoveTagCallBack);
      FTag := t;
      RefreshUpDown(0);
   end;
   FTag := t;
end;

function  THMIUpDown.GetHMITag:TPLCTag;
begin
   Result:=FTag;
end;

function THMIUpDown.GetControlSecurityCode:UTF8String;
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
  RefreshUpDown(0);
end;

procedure THMIUpDown.Click(Button: TUDBtnType);
var
  aValue:Double;

  procedure DoAfterSendValue(ivalue:Double);
  begin
    if Assigned(FAfterSendValueToTag) then
      FAfterSendValueToTag(Self,ivalue);
  end;

  function SendIt(ivalue:Double):Boolean;
  begin
    if Assigned(FBeforeSendValueToTag) then
      FBeforeSendValueToTag(Self,ivalue,Result)
    else
      Result:=true;
  end;
begin
   if FTag=nil then exit;
   
   inherited Position:=50;
     
   case Button of
     btNext:
        aValue := Position+FIncrement;
     else
        aValue := Position-FIncrement;
   end;

   if (FEnableMax And (aValue>FMax)) then
      aValue := FMax;
      
   if (FEnableMin And (aValue<FMin)) then
      aValue := FMin;

   if Supports(FTag, ITagNumeric) and SendIt(aValue) then begin
      (FTag as ITagNumeric).Value := aValue;
      DoAfterSendValue(aValue);
   end;

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

procedure THMIUpDown.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIUpDown.TagChangeCallBack(Sender: TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    Application.QueueAsyncCall(@RefreshUpDown, 0);
end;

procedure THMIUpDown.RemoveTagCallBack(Sender: TObject);
begin
  if Ftag=Sender then
    FTag:=nil;
end;

end.
