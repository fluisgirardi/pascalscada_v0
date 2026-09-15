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
  Dialogs, StdCtrls, HMITypes, PLCTag, ProtocolTypes, Tag, hmi_commfaultbadge,
  LMessages;

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
    FRegInSecMan:Boolean;
    FTag:TPLCTag;
    FCommBadge:THMICommBadgeController;
    FCommFaultLink:THMITagFaultBadgeLink;
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
    procedure Loaded; override;
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
  FRegInSecMan:=GetControlSecurityManager.RegisterControl(Self as IHMIInterface);
  if not FRegInSecMan then begin
    {$IFNDEF WINDOWS}
    writeln('FIX-ME: Failed to register class ',ClassName,' instace with name="',Name,'" in the ControlSecurityManager?',{$i %FILE%},':',{$i %LINE%});
    {$ENDIF}
  end;
  FIsEnabled:=true;
  //: @seealso(THMIBasicControl.Create) sobre a flag de seguranca nascer verdadeira
  //: @seealso(THMIBasicControl.Create) on the security flag being born true
  FIsEnabledBySecurity:=true;

  FCommBadge:=THMICommBadgeController.Create;
  FCommBadge.SetTarget(Self);
  FCommFaultLink:=THMITagFaultBadgeLink.Create(FCommBadge);
end;

destructor THMIScrollBar.Destroy;
begin
  if FRegInSecMan then
    GetControlSecurityManager.UnRegisterControl(Self as IHMIInterface)
  else begin
    {$IFNDEF WINDOWS}
    writeln('FIX-ME: Why class ',ClassName,', instace name="',Name,'" ins''t registered in ControlSecurityManager?',{$i %FILE%},':',{$i %LINE%});
    {$ENDIF}
  end;

  Application.RemoveAsyncCalls(Self);
  if FTag<>nil then
    FTag.RemoveAllHandlersFromObject(Self);
  FreeAndNil(FCommFaultLink);
  FreeAndNil(FCommBadge);
  inherited Destroy;
end;

procedure THMIScrollBar.RefreshScrollBar(Data: PtrInt);
begin
  if [csReading,csLoading,csDestroying]*ComponentState<>[] then exit;
  if not FBusy then begin
    if (FTag=nil) then begin
      //sem tag nao ha' leitura: a barra parada onde estava continua parecendo
      //leitura viva de um tag que nao existe mais.
      //with no tag there is no reading: the bar left where it was still reads
      //like a live value from a tag that is not there anymore.
      Position := Min;
      exit;
    end;

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
   end;
   FTag := t;
   RefreshScrollBar(0);
   if Assigned(FCommFaultLink) then
     FCommFaultLink.SetTag(t);
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
         //soltou: o valor do CLP volta a mandar na posicao.
         //let go: the PLC value rules the position again.
         FBusy:=false;
         {$IF defined(WIN32) or defined(WIN64)}
         FCmdCount:=0;
         WriteFlag:=true;
         {$IFEND}
      end else begin
         //a barra esta na mao do operador: o valor que chegar do CLP nao pode
         //puxar o cursor de onde ele esta' arrastando. Sem ligar a bandeira
         //aqui, o RefreshScrollBar reposicionava no meio do arrasto - e com
         //UpdateOnMove os dois brigavam, um escrevendo e o outro voltando.
         //the bar is in the operator's hand: a value arriving from the PLC
         //must not pull the thumb away from where they are dragging. Without
         //raising the flag here, RefreshScrollBar repositioned it mid drag -
         //and with UpdateOnMove the two fought, one writing and the other
         //putting it back.
         FBusy:=true;
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
    //fora do Windows e' aqui que o arrasto termina: o scEndScroll do widget
    //nao e' garantido, entao a bandeira tem que baixar tambem por este
    //caminho, senao o controle congela no ultimo valor arrastado.
    //outside Windows this is where the drag ends: the widget's scEndScroll is
    //not guaranteed, so the flag has to come down through this path too,
    //otherwise the control freezes on the last dragged value.
    FBusy:=false;
    WriteValue(FLastPosition);
  finally
    inherited MouseUp(Button, Shift, X, Y);
  end;
end;
{$IFEND}

procedure THMIScrollBar.Loaded;
begin
  inherited Loaded;
  CanBeAccessed(GetControlSecurityManager.CanAccess(GetControlSecurityCode));
  TagChangeCallBack(Self);
end;

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
  if FTag=Sender then begin
    FTag := nil;
    RefreshScrollBar(0);
  end;
end;

end.
