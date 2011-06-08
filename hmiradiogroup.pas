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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, {$ENDIF} Controls, Graphics,
  Dialogs, ExtCtrls, HMITypes, PLCTag, hsutils, ProtocolTypes, Tag;

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
    FIsEnabled:Boolean;
    FDefaultIndex:Integer;
    FIgnore, FLoaded:Boolean;
    //implementes the IHMIInterface
    procedure RefreshHMISecurity;                      //check security
    procedure SetHMITag(t:TPLCTag);                    //link with a tag
    function  GetHMITag:TPLCTag;
    function  GetHMIEnabled:Boolean;
    procedure SetHMIEnabled(v:Boolean);
    procedure SetDefaultIndex(v:integer);
    function  GetIndex:Integer;
    procedure SetIndex(v:Integer);

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
    property  ItemIndex:Integer read GetIndex Write SetIndex;
    //: @exclude
    property  Enabled:Boolean read GetHMIEnabled write SetHMIEnabled;

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
    If the integer value of tag doesn't match with one of the control list, uses
    the value of @name.
    }
    {$ENDIF}
    property  DefaultIndex:Integer read FDefaultIndex write SetDefaultIndex default -1;
  end;

implementation

uses hsstrings;

constructor THMIRadioGroup.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FIgnore:=false;
   FLoaded:=false;
   FDefaultIndex:=-1;
end;

destructor  THMIRadioGroup.Destroy;
begin
   if FTag<>nil then
      FTag.RemoveCallBacks(Self as IHMITagInterface);
   inherited Destroy;
end;

//check the security
procedure THMIRadioGroup.RefreshHMISecurity;
begin
   //todo
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

function  THMIRadioGroup.GetHMIEnabled:Boolean;
begin
   Result := FIsEnabled;
end;

procedure THMIRadioGroup.SetHMIEnabled(v:Boolean);
begin
   inherited Enabled := v;
   FIsEnabled := v;
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

procedure THMIRadioGroup.SetDefaultIndex(v:integer);
begin
  if v<(-1) then
     FDefaultIndex:=-1
  else
     FDefaultIndex:=v;
  NotifyTagChange(Self);
end;

function  THMIRadioGroup.GetIndex:Integer;
begin
   Result := inherited ItemIndex;
end;

procedure THMIRadioGroup.SetIndex(v:Integer);
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
      inherited ItemIndex:= FloatToInteger(Value)
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
