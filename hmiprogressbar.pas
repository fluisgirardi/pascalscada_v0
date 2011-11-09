{$i language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementa um controle para exibição de valores numéricos em forma de barra de progresso.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a control to show numeric values in a progress bar.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIProgressBar;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, ComCtrls, HMITypes, PLCTag, ProtocolTypes, Tag;

type

  {$IFDEF PORTUGUES}
  {:
  Implementa um controle para exibição de valores numéricos em forma de barra de
  progresso.

  @bold(Para maiores informações consulte a documentação da classe TProgressBar
  de seu ambiente de desenvolvimento.)
  }
  {$ELSE}
  {:
  Implements a control to show numeric values of tags in a progress bar.

  @bold(To get more information see the documentation of the class TProgressBar
  of your development environment.)
  }
  {$ENDIF}
  THMIProgressBar = class(TProgressBar, IHMIInterface, IHMITagInterface)
  private
    FTag:TPLCTag;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
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

    function  GetPosition:Double;

    //implements the IHMITagInterface
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
  public
    constructor Create(AOwner: TComponent); override;
    //: @exclude
    destructor Destroy; override;
  published
    //: @exclude
    property Enabled:Boolean read FIsEnabled write SetEnabled;

    {$IFDEF PORTUGUES}
    //: Informa a posição (valor do tag) atual.
    {$ELSE}
    //: Tells the current position (tag value).
    {$ENDIF}
    property Position:Double read GetPosition;

    {$IFDEF PORTUGUES}
    {:
    Tag numérico que será usado pelo controle.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    }
    {$ELSE}
    {:
    Numeric tag that will be used by the control.
    @seealso(TPLCTag)
    @seealso(TPLCTagNumber)
    @seealso(TPLCBlockElement)
    }
    {$ENDIF}
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;
  end;

implementation

uses hsstrings, ControlSecurityManager;

constructor THMIProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsEnabled:=true;
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIProgressBar.Destroy;
begin
  if Assigned(FTag) then
    Ftag.RemoveCallBacks(Self as IHMITagInterface);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

procedure THMIProgressBar.Loaded;
begin
   inherited Loaded;
   NotifyTagChange(Self);
end;

function THMIProgressBar.GetControlSecurityCode:String;
begin
   //todo
end;

procedure THMIProgressBar.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure THMIProgressBar.MakeUnsecure;
begin

end;

procedure THMIProgressBar.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure THMIProgressBar.SetHMITag(t:TPLCTag);
begin
  //se o tag esta entre um dos aceitos.
  //check if the tag is valid.
  if (t<>nil) and (not Supports(t, ITagNumeric)) then
     raise Exception.Create(SonlyNumericTags);

  //se ja estou associado a um tag, remove
  //if the control is linked with some tag, remove the old link.
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

function  THMIProgressBar.GetHMITag:TPLCTag;
begin
  Result := FTag;
end;

function  THMIProgressBar.GetPosition:Double;
begin
   Result := 0;
   if (FTag<>nil) AND Supports(FTag, ITagNumeric) then begin
      Result := (FTag as ITagNumeric).Value;
   end;
end;

procedure THMIProgressBar.NotifyReadOk;
begin

end;

procedure THMIProgressBar.NotifyReadFault;
begin

end;

procedure THMIProgressBar.NotifyWriteOk;
begin

end;

procedure THMIProgressBar.NotifyWriteFault;
begin
  NotifyTagChange(Self);
end;

procedure THMIProgressBar.NotifyTagChange(Sender:TObject);
begin
  if ([csReading]*ComponentState<>[]) or (FTag=nil) then
    exit;

  if (FTag<>nil) and (Supports(FTag, ITagNumeric)) then
    inherited Position := Trunc((FTag as ITagNumeric).Value);
end;

procedure THMIProgressBar.RemoveTag(Sender:TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

end.
