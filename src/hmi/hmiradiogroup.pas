{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Define um controle de opções para leitura/escrita de valores de tags numéricos.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a multiple-options control to read and write
  values in numeric tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIRadioGroup;

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, {$ENDIF} Controls, Graphics,
  Dialogs, ExtCtrls, HMITypes, PLCTag, ProtocolTypes, Tag;

type
  {$IFDEF PORTUGUES}
  {:
    @abstract(Classe de controle de multiplas opções para leitura/escrita de
    valores de tags numéricos.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
    @abstract(Class of multiple-options control to read and write
    values in numeric tags.)
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  THMIRadioGroup = class(TRadioGroup, IHMIInterface, IHMITagInterface)
  private
    FTag:TPLCTag;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FDefaultIndex:LongInt;
    FIgnore, FLoaded:Boolean;

    FSecurityCode:UTF8String;
    procedure SetSecurityCode(sc:UTF8String);

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

    procedure SetDefaultIndex(v:LongInt);
    function  GetIndex:LongInt;
    procedure SetIndex(v:LongInt);

    //implements the IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    {$IFNDEF FPC}
    procedure Click; override;
    {$ENDIF}
    //: @exclude
    procedure SetEnabled(e:Boolean); override;
    //: @exclude
    procedure CheckItemIndexChanged; {$IFDEF FPC} override; {$ENDIF}
    //: @exclude
    procedure Loaded; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
  published
    {$IFDEF PORTUGUES}
    //: @name retorna qual a opção selecionada.
    {$ELSE}
    //: @name tells what's the index of selected option.
    {$ENDIF}
    property  ItemIndex:LongInt read GetIndex Write SetIndex;

    //: @exclude
    property Enabled:Boolean read FIsEnabled write SetEnabled;

    {$IFDEF PORTUGUES}
    {:
    Tag numérico que será usado pelo controle.
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
    property  PLCTag:TPLCTag read GetHMITag write SetHMITag;

    {$IFDEF PORTUGUES}
    {:
    Caso o valor inteiro do tag não esteja entre as opções oferecidas, usa o valor
    de @name.
    }
    {$ELSE}
    {:
    If the LongInt value of tag doesn't match with one of the control list, uses
    the value of @name.
    }
    {$ENDIF}
    property  DefaultIndex:LongInt read FDefaultIndex write SetDefaultIndex default -1;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;
  end;

implementation

uses hsstrings, ControlSecurityManager;

constructor THMIRadioGroup.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FIgnore:=false;
   FLoaded:=false;
   FIsEnabled:=true;
   FDefaultIndex:=-1;
   GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor  THMIRadioGroup.Destroy;
begin
   if FTag<>nil then
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
   inherited Destroy;
end;

procedure THMIRadioGroup.SetSecurityCode(sc: UTF8String);
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

//link with tags
procedure THMIRadioGroup.SetHMITag(t:TPLCTag);
begin
   //se o tag esta entre um dos aceitos.
   //
   //Check if the tag is valid (only numeric tags).
   if (t<>nil) and (not Supports(t, ITagNumeric)) then
      raise Exception.Create(SonlyNumericTags);

   //se ja estou associado a um tag, remove
   //remove the old link.
   if FTag<>nil then begin
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   end;

   //adiona o callback para o novo tag
   //link with the new tag.
   if t<>nil then begin
      t.AddCallBacks(Self as IHMITagInterface);
      FTag := t;
      NotifyTagChange(self);
   end;
   FTag := t;
end;

function  THMIRadioGroup.GetHMITag:TPLCTag;
begin
   Result:=FTag;
end;

function THMIRadioGroup.GetControlSecurityCode: UTF8String;
begin
   Result:=FSecurityCode;
end;

procedure THMIRadioGroup.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure THMIRadioGroup.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMIRadioGroup.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure THMIRadioGroup.CheckItemIndexChanged;
begin
   {$IFDEF FPC}
   inherited CheckItemIndexChanged;
   {$ENDIF}

   if [csLoading, csReading, csDestroying]*ComponentState<>[] then
      exit;

   if (FLoaded) and (not FIgnore) then
     if (FTag<>nil) AND Supports(FTag, ITagNumeric) then
        (FTag as ITagNumeric).Value := ItemIndex;
end;

procedure THMIRadioGroup.Loaded;
begin
   inherited Loaded;
   FLoaded:=true;
end;

procedure THMIRadioGroup.SetDefaultIndex(v:LongInt);
begin
  if v<(-1) then
     FDefaultIndex:=-1
  else
     FDefaultIndex:=v;
  NotifyTagChange(Self);
end;

function  THMIRadioGroup.GetIndex:LongInt;
begin
   Result := inherited ItemIndex;
end;

procedure THMIRadioGroup.SetIndex(v:LongInt);
begin

end;

{$IFNDEF FPC}
procedure THMIRadioGroup.Click;
begin
   CheckItemIndexChanged;
   inherited Click;
end;
{$ENDIF}

procedure THMIRadioGroup.NotifyReadOk;
begin

end;

procedure THMIRadioGroup.NotifyReadFault;
begin

end;

procedure THMIRadioGroup.NotifyWriteOk;
begin

end;

procedure THMIRadioGroup.NotifyWriteFault;
begin
  NotifyTagChange(Self);
end;

procedure THMIRadioGroup.NotifyTagChange(Sender:TObject);
var
   Value:Double;
begin
   Value := 0;

   if (FTag<>nil) AND Supports(FTag, ITagNumeric) then
      Value := (FTag as ITagNumeric).Value;

   FIgnore:=true;
   if (Value>=0) and (Value<Items.Count) then
      inherited ItemIndex:= Trunc(Value)
   else
      inherited ItemIndex := FDefaultIndex;
   FIgnore:=false;
end;

procedure THMIRadioGroup.RemoveTag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

end.
