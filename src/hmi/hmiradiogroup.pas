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
  Dialogs, ExtCtrls, HMITypes, PLCTag, ProtocolTypes, Tag, hmi_commfaultbadge,
  LMessages;

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
    FRegInSecMan:Boolean;
    FTag:TPLCTag;
    FCommIndicator:THMIInlineFaultIndicator;
    FCommFaultLink:THMITagFaultBadgeLink;
    FIsEnabled,
    FIsEnabledBySecurity:Boolean;
    FDefaultIndex:LongInt;
    FLastIndex:LongInt;

    FSecurityCode:UTF8String;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
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
    procedure WriteIndexToTag(aIndex:LongInt);
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
   FDefaultIndex:=-1;
   FLastIndex:=-1;

   FCommIndicator:=THMIInlineFaultIndicator.Create(Self);
   FCommFaultLink:=THMITagFaultBadgeLink.Create(FCommIndicator);
end;

destructor  THMIRadioGroup.Destroy;
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
  FreeAndNil(FCommIndicator);
  inherited Destroy;
end;

procedure THMIRadioGroup.WMPaint(var Msg: TLMPaint);
var
  cnv: TCanvas;
begin
  inherited;
  if Assigned(FCommIndicator) and FCommIndicator.Faulted then begin
    cnv := TCanvas.Create;
    try
      cnv.Handle := Msg.DC;
      DrawWarningIcon(cnv, ClientWidth, ClientHeight);
    finally
      cnv.Free;
    end;
  end;
end;

procedure THMIRadioGroup.RefreshRadioGroup(Data: PtrInt);
var
   Value:Double;
begin
  if [csReading,csLoading,csDestroying]*ComponentState<>[] then exit;

  //sem tag nao ha' leitura: -1 nao e' posicao nenhuma, entao o grupo cai na
  //opcao padrao - que e' exatamente "esse valor nao e' nenhuma das opcoes".
  //Com zero no lugar, a primeira opcao ficava marcada sem ninguem ter dito
  //isso.
  //with no tag there is no reading: -1 is no position at all, so the group
  //falls on the default option - which is exactly "this value is none of the
  //options". With zero here, the first option stayed marked with nobody
  //having said so.
  Value := -1;

  if (FTag<>nil) AND Supports(FTag, ITagNumeric) then
    Value := (FTag as ITagNumeric).Value;

  Updating;
  try
    if (Value>=0) and (Value<Items.Count) then
      inherited ItemIndex:= Trunc(Value)
    else
      inherited ItemIndex := FDefaultIndex;
    FLastIndex:=inherited ItemIndex;
  finally
    Updated;
  end;
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
   end;
   FTag := t;
   //tambem sem tag: a opcao que ficasse marcada continuaria parecendo o modo
   //em que o processo esta'
   //with no tag as well: an option left marked would go on looking like the
   //mode the process is in
   RefreshRadioGroup(0);
   if Assigned(FCommFaultLink) then
     FCommFaultLink.SetTag(t);
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

   //o operador marcou uma opcao. A escrita ja' nao espera pelo Loaded: um
   //grupo criado em tempo de execucao nunca passa por ele, e nunca escrevia.
   //Durante a carga do formulario quem segura e' o ComponentState.
   //
   //E escreve uma vez por opcao marcada: cada botao do grupo avisa duas vezes
   //- o clique e a mudanca - e o que foi desmarcado avisa tambem. O TRadioGroup
   //filtra isso para o OnClick dele, mas nao para quem sobrescreve este
   //metodo; sem o filtro cada clique do operador ia ao CLP duas ou tres vezes.
   //the operator marked an option. The write no longer waits for Loaded: a
   //group created at run time never goes through it, and never wrote. While
   //the form is loading it is ComponentState that holds it back.
   //
   //And it writes once per marked option: each button in the group reports
   //twice - the click and the change - and the one unmarked reports as well.
   //TRadioGroup filters that for its own OnClick, but not for whoever
   //overrides this method; without the filter every operator click went to
   //the PLC two or three times.
   if ([csUpdating]*ComponentState<>[]) or (ItemIndex=FLastIndex) then exit;

   FLastIndex:=ItemIndex;
   WriteIndexToTag(ItemIndex);
end;

procedure THMIRadioGroup.WriteIndexToTag(aIndex:LongInt);
begin
   if [csLoading, csReading, csDesigning, csDestroying]*ComponentState<>[] then
      exit;

   if (FTag<>nil) AND Supports(FTag, ITagNumeric) then
      (FTag as ITagNumeric).Value := aIndex;
end;

procedure THMIRadioGroup.Loaded;
begin
   inherited Loaded;
   CanBeAccessed(GetControlSecurityManager.CanAccess(GetControlSecurityCode));
   TagChangeCallBack(Self);
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
  //o programa marcando uma opcao vale o mesmo que o operador marcando. So'
  //que o TRadioGroup so' passa pelo CheckItemIndexChanged quando tem janela:
  //sem ela, a posicao mudava na tela e o tag nao ficava sabendo. A escrita
  //fica aqui, uma vez, e o refresh e' silenciado para nao escrever duas.
  //the program marking an option is worth the same as the operator marking
  //it. But TRadioGroup only goes through CheckItemIndexChanged when it has a
  //window: without one the position moved on screen and the tag never heard
  //of it. The write lives here, once, and the refresh is muted so it does not
  //write twice.
  Updating;
  try
    inherited ItemIndex := v;
    FLastIndex:=inherited ItemIndex;
  finally
    Updated;
  end;
  WriteIndexToTag(v);
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
  if FTag=Sender then begin
    FTag := nil;
    RefreshRadioGroup(0);
  end;
end;

end.
