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
  THMIRadioGroup = class(TRadioGroup, IHMIInterface)
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

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
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
    procedure RefreshRadioGroup(Data: PtrInt);
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

uses hsstrings, ControlSecurityManager, Forms;

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
  Application.RemoveAsyncCalls(Self);
  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

procedure THMIRadioGroup.RefreshRadioGroup(Data: PtrInt);
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
      FTag.RemoveAllHandlersFromObject(Self);
   end;

   //adiona o callback para o novo tag
   //link with the new tag.
   if t<>nil then begin
      t.AddWriteFaultHandler(@WriteFaultCallBack);
      t.AddTagChangeHandler(@TagChangeCallBack);
      t.AddRemoveTagHandler(@RemoveTagCallBack);
      FTag := t;
      RefreshRadioGroup(0);
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
  RefreshRadioGroup(0);
end;

function  THMIRadioGroup.GetIndex:LongInt;
begin
   Result := inherited ItemIndex;
end;

procedure THMIRadioGroup.SetIndex(v:LongInt);
begin
  inherited ItemIndex := v;
end;

{$IFNDEF FPC}
procedure THMIRadioGroup.Click;
begin
   CheckItemIndexChanged;
   inherited Click;
end;
{$ENDIF}

procedure THMIRadioGroup.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Self);
end;

procedure THMIRadioGroup.TagChangeCallBack(Sender: TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    Application.QueueAsyncCall(@RefreshRadioGroup,0);
end;

procedure THMIRadioGroup.RemoveTagCallBack(Sender: TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

end.
