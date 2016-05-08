{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementa um controle em forma de ScrollBar para a leitura/escrita de valores
            em tags numéricos.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements a ScrollBar control to read/write values in
            numeric tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit HMIScrollBar;

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, {$ENDIF} Controls, Graphics,
  Dialogs, StdCtrls, HMITypes, PLCTag, ProtocolTypes, Tag;

type
  {$IFDEF PORTUGUES}
  {:
  Implementa um controle em forma de ScrollBar para a leitura/escrita de valores
  em tags numéricos.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Class of ScrollBar control to read/write values in numeric tags.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  THMIScrollBar = class(TScrollBar, IHMIInterface)
  private
    FTag:TPLCTag;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FUpdateOnMove:Boolean;
    FBusy:Boolean;
    FCmdCount:LongInt;
    FLastPosition:LongInt;

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

    procedure WriteValue(Value:LongInt);

    procedure WriteFaultCallBack(Sender:TObject);
    procedure TagChangeCallBack(Sender:TObject);
    procedure RemoveTagCallBack(Sender:TObject);
  protected
    //: @exclude
     procedure SetEnabled(e:Boolean); override;
    //: @exclude
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: LongInt); override;
    {$IF (not defined(WIN32)) and (not defined(WIN64))}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: LongInt); override;
    {$IFEND}
  public
    //: @exclude
    constructor Create(AOwner: TComponent); override;
    //: @exclude
    destructor Destroy; override;
    procedure RefreshScrollBar(Data: PtrInt);
  published
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
    property PLCTag:TPLCTag read GetHMITag write SetHMITag;

    {$IFDEF PORTUGUES}
    {:
    Caso @true, escreve seu valor para o tag ainda quando está sendo movido.
    Caso @false, escreve seu valor para o tag somente quando é solto.
    }
    {$ELSE}
    {:
    If @true, write its value while the scroll is being moved.
    If @false write its value only when the scroll is released.
    }
    {$ENDIF}
    property UpdateOnMove:Boolean read FUpdateOnMove write FUpdateOnMove default false;

    {$IFDEF PORTUGUES}
    //: Codigo de segurança que libera acesso ao controle
    {$ELSE}
    //: Security code that allows access to control.
    {$ENDIF}
    property SecurityCode:UTF8String read FSecurityCode write SetSecurityCode;
  end;

implementation

uses hsstrings, ControlSecurityManager, Forms;

constructor THMIScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsEnabled:=true;
  GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
end;

destructor THMIScrollBar.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);
  GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface);
  inherited Destroy;
end;

procedure THMIScrollBar.RefreshScrollBar(Data: PtrInt);
begin
  if not FBusy then begin
    if (FTag=nil) then exit;

    if Supports(FTag, ITagNumeric) then
      Position := Trunc((FTag as ITagNumeric).Value);
  end;
end;

procedure THMIScrollBar.SetSecurityCode(sc: UTF8String);
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

procedure THMIScrollBar.SetHMITag(t:TPLCTag);
begin
   //se o tag esta entre um dos aceitos.
   //
   //check if the tag is valid (only numeric tags);
   if (t<>nil) and (not Supports(t, ITagNumeric)) then
      raise Exception.Create(SonlyNumericTags);

   //se ja estou associado a um tag, remove
   //removes the old link.
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
      RefreshScrollBar(0);
   end;
   FTag := t;
end;

function  THMIScrollBar.GetHMITag:TPLCTag;
begin
   Result:=FTag;
end;

function THMIScrollBar.GetControlSecurityCode: UTF8String;
begin
   Result:=FSecurityCode;
end;

procedure THMIScrollBar.CanBeAccessed(a:Boolean);
begin
  FIsEnabledBySecurity := a;
  SetEnabled(FIsEnabled);
end;

procedure THMIScrollBar.MakeUnsecure;
begin
  FSecurityCode:='';
  CanBeAccessed(true);
end;

procedure THMIScrollBar.SetEnabled(e:Boolean);
begin
  FIsEnabled:=e;
  inherited SetEnabled(FIsEnabled and FIsEnabledBySecurity);
end;

procedure THMIScrollBar.Scroll(ScrollCode: TScrollCode; var ScrollPos: LongInt);
var
   WriteFlag:Boolean;
begin
   WriteFlag:=false;
   Try
      FLastPosition:=ScrollPos;

      if (ScrollCode=scEndScroll) then begin
         {$IF defined(WIN32) or defined(WIN64)}
         FBusy:=false;
         FCmdCount:=0;
         WriteFlag:=true;
         {$IFEND}
      end else begin
         inc(FCmdCount);
         if FCmdCount>5 then begin
            if FUpdateOnMove then
               WriteFlag:=true;
            FCmdCount:=0;
         end;
      end;
      if WriteFlag then
         WriteValue(ScrollPos);
      
   finally
      inherited Scroll(ScrollCode, ScrollPos);
   end;
end;

{$IF (not defined(WIN32)) and (not defined(WIN64))}
procedure THMIScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: LongInt);
begin
  try
    WriteValue(FLastPosition);
  finally
    inherited MouseUp(Button, Shift, X, Y);
  end;
end;
{$IFEND}

procedure THMIScrollBar.WriteValue(Value:LongInt);
begin
   if (FTag=nil)  then exit;

   if Supports(FTag, ITagNumeric) then
      (FTag as ITagNumeric).Value:=Value;
end;

procedure THMIScrollBar.WriteFaultCallBack(Sender: TObject);
begin
  TagChangeCallBack(Sender);
end;

procedure THMIScrollBar.TagChangeCallBack(Sender: TObject);
begin
  if Application.Flags*[AppDoNotCallAsyncQueue]=[] then
    application.QueueAsyncCall(@RefreshScrollBar,0);
end;

procedure THMIScrollBar.RemoveTagCallBack(Sender: TObject);
begin
  if FTag=Sender then
    FTag := nil;
end;

end.
