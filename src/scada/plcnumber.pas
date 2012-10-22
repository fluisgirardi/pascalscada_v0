{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementa um tag PLC numérico para uso geral.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
  @abstract(Unit that implements a numeric tag for general use.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ENDIF}
unit PLCNumber;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, PLCTag, ProtocolTypes, ValueProcessor;

type

  {$IFDEF PORTUGUES}
  {:
    @abstract(Classe base para tags numéricos.)
    @author(Fabio Luis Girardi fabio@pascalscada.com)
  }
  {$ELSE}
  {:
    @abstract(Base class of numeric tags.)
    @author(Fabio Luis Girardi fabio@pascalscada.com)
  }
  {$ENDIF}
  TPLCNumber = class(TPLCTag)
  protected
    //: @seealso(TTag.AsyncNotifyChange)    
    procedure AsyncNotifyChange(data:Pointer); override;
    //: @seealso(TTag.GetValueChangeData)
    function GetValueChangeData: Pointer; override;
    //: @seealso(TTag.ReleaseChangeData)
    procedure ReleaseChangeData(data: Pointer); override;
  protected
    {$IFDEF PORTUGUES}
    //: Armazena se devem ser verificados limites minimos e máximos
    {$ELSE}
    //: Stores if must be checked the minimum and maximum limits.
    {$ENDIF}
    FEnableMin, FEnableMax:Boolean;

    {$IFDEF PORTUGUES}
    //: Armazena os valores de limites inferior e superior, apenas entrada.
    {$ELSE}
    //: Stores the minimum and maximum limits.
    {$ENDIF}
    FMinLimit, FMaxLimit:Double;

    {$IFDEF PORTUGUES}
    //: Armazena a sequência de escalas aplicadas a esse tag.
    {$ELSE}
    //: Store the scales linked with the tag.
    {$ENDIF}
    PScaleProcessor:TScaleProcessor;

    {$IFDEF PORTUGUES}
    //: Armazena o valor puro (sem escalas).
    {$ELSE}
    //: Stores the raw value (without scales).
    {$ENDIF}
    PValueRaw:Double;

    {$IFDEF PORTUGUES}
    //: Retorna o valor processado pelas escalas associadas, ou o valor puro caso contrário.
    {$ELSE}
    //: Returns the value processed by the linked scales or the value raw.
    {$ENDIF}
    function GetValue:Double; virtual;

    {$IFDEF PORTUGUES}
    //: Retorna o valor PURO.
    {$ELSE}
    //: Returns the raw value.
    {$ENDIF}
    function GetValueRaw:Double; virtual; abstract;

    {$IFDEF PORTUGUES}
    {: Processa pelas escalas associadas e escreve o valor processado no dispositivo.
    @param(Value Double: Valor a processar e escrever.)
    @seealso(SetValueRaw)
    }
    {$ELSE}
    {:
    Processes the value using linked scales and writes the value processed on device.
    @param(Value Double: Value to be processed and written in device.)
    @seealso(SetValueRaw)
    }
    {$ENDIF}
    procedure SetValue(Value:Double); virtual;

    {$IFDEF PORTUGUES}
    {: Escreve o valor puro do tag no dispositivo.
       @param(Value Double: Valor a escrever.) }
    {$ELSE}
    {: Write the raw value on device.
       @param(Value Double: Value to be written.) }
    {$ENDIF}
    procedure SetValueRaw(Value:Double); virtual; abstract;

    {$IFDEF PORTUGUES}
    {:
    Configura a sequência de escalas.
    @param(sp TPIPE: Nova sequência de escalas.)
    @seealso(ScaleProcessor)
    }
    {$ELSE}
    {:
    Sets the new scales sequence.
    @param(sp TPIPE: The new scale sequence.)
    @seealso(ScaleProcessor)
    }
    {$ENDIF}
    procedure SetScaleProcessor(sp:TScaleProcessor);

    {$IFDEF PORTUGUES}
    //: seta o limite minimo
    {$ELSE}
    //: set the minimum limit.
    {$ENDIF}
    procedure SetMinLimit(v:Double);

    {$IFDEF PORTUGUES}
    //: seta o limite máximo
    {$ELSE}
    //: sets the maximum limit.
    {$ENDIF}
    procedure SetMaxLimit(v:Double);

    //: @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    {$IFDEF PORTUGUES}
    //: Habilita/desabilita o limite minimo para entrada de dados.
    {$ELSE}
    //: Enables/disables the minimum limit.
    {$ENDIF}
    property EnableMinValue:Boolean read FEnableMin write FEnableMin stored true;

    {$IFDEF PORTUGUES}
    //: Habilita/desabilita o limite máximo para entrada de dados.
    {$ELSE}
    //: Enables/disables the maximum limit.
    {$ENDIF}
    property EnableMaxValue:Boolean read FEnableMax write FEnableMax stored true;

    {$IFDEF PORTUGUES}
    //: Limite minimo para entrada de dados.
    {$ELSE}
    //: Minimum value acceptable if the minimum limit is enabled.
    {$ENDIF}
    property MinValue:Double read FMinLimit write SetMinLimit;

    {$IFDEF PORTUGUES}
    //: Limite máximo para entrada de dados.
    {$ELSE}
    //: Maximum value acceptable if the maximum limit is enabled.
    {$ENDIF}
    property MaxValue:Double read FMaxLimit write SetMaxLimit;
  public
    //: @exclude
    constructor Create(AOwner: TComponent); override;

    //: @exclude
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    //: Abre o assistente de mapeamento de tags bit.
    {$ELSE}
    //: Opens the bit mapper wizard.
    {$ENDIF}
    procedure OpenBitMapper(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); virtual;

    //: @seealso(TPLCTag.Write)
    procedure Write; overload; virtual;
    //: @seealso(TPLCTag.Write)
    procedure ScanWrite; overload; virtual;
    {$IFDEF PORTUGUES}
    //: Remove a sequência de processamento de escalas.
    {$ELSE}
    //: Removes the scales sequence, it's being destroyed.
    {$ENDIF}
    procedure RemoveScaleProcessor;

    {$IFDEF PORTUGUES}
    //: Valor do tag escalonado (processado).
    {$ELSE}
    //: Tag Value processed by the scales.
    {$ENDIF}
    property Value:Double read GetValue write SetValue;

    {$IFDEF PORTUGUES}
    //: Valor puro do tag.
    {$ELSE}
    //: Raw value of the tag.
    {$ENDIF}
    property ValueRaw:Double read PValueRaw write SetValueRaw;
  published
    {$IFDEF PORTUGUES}
    //: Sequência de escalas do Tag.
    {$ELSE}
    //: Scale sequence of tag.
    {$ENDIF}
    property ScaleProcessor:TScaleProcessor read PScaleProcessor write SetScaleProcessor;

    {$IFDEF PORTUGUES}
    //: Evento chamado ao ocorrer uma mudança no valor do tag. Chamado APÓS notificar os controles dependentes.
    {$ELSE}
    //: Event called when the value of tag changes. Called AFTER updates all dependent components.
    {$ENDIF}
    property OnValueChange stored false;

    {$IFDEF PORTUGUES}
    //: Evento chamado ao ocorrer uma mudança no valor do tag. Chamado ANTES DE notificar os controles dependentes.
    {$ELSE}
    //: Event called when the value of tag changes. Called BEFORE updates all dependent components.
    {$ENDIF}
    property OnValueChangeFirst;

    {$IFDEF PORTUGUES}
    //: Evento chamado ao ocorrer uma mudança no valor do tag. Chamado APÓS notificar os controles dependentes.
    {$ELSE}
    //: Event called when the value of tag changes. Called AFTER updates all dependent components.
    {$ENDIF}
    property OnValueChangeLast;

    {$IFDEF PORTUGUES}
    //: Evento assincrono chamado quando o valor do tag sofre uma alteração.
    {$ELSE}
    //: Asynchronous event called when the tag value changes.
    {$ENDIF}
    property OnAsyncValueChange;
  end;

implementation

uses ubitmapper, Controls, TagBit, tag, hsstrings, Dialogs;

constructor TPLCNumber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PValueRaw:=0;
end;

destructor TPLCNumber.Destroy;
begin
  SetScaleProcessor(nil);
  inherited Destroy;
end;

procedure TPLCNumber.AsyncNotifyChange(data:Pointer);
var
  x:PArrayOfDouble;
begin
  if not Assigned(POnAsyncValueChange) then exit;
  x:=data;
  POnAsyncValueChange(self,x^);
end;

function TPLCNumber.GetValueChangeData: Pointer;
var
  x:PArrayOfDouble;
begin
  New(x);
  SetLength(x^,1);
  x^[0]:=Value;
  Result:=x;
end;

procedure TPLCNumber.ReleaseChangeData(data: Pointer);
var
  x:PArrayOfDouble;
begin
  x:=data;
  SetLength(x^,0);
  Dispose(x);
end;

function  TPLCNumber.GetValue:Double;
begin
  if Assigned(PScaleProcessor) then
    Result := PScaleProcessor.SetInGetOut(self, GetValueRaw)
  else
    Result := GetValueRaw;
end;

procedure TPLCNumber.SetValue(Value:Double);
var
  towrite:Double;
begin
  if (FEnableMin and (Value<FMinLimit)) or (FEnableMax and (Value>FMaxLimit)) then begin
    MessageDlg(SoutOfBounds,mtWarning,[mbOK],0);
    NotifyWriteFault;
    exit;
  end;

  if Assigned(PScaleProcessor) then
    towrite := PScaleProcessor.SetOutGetIn(self, Value)
  else
    towrite := value;

  SetValueRaw(towrite);
end;

procedure TPLCNumber.SetScaleProcessor(sp:TScaleProcessor);
begin
  if sp=PScaleProcessor then exit;

  if PScaleProcessor<>nil then
    PScaleProcessor.RemoveFreeNotification(self);

  if sp<>nil then
    sp.FreeNotification(self);

  PScaleProcessor := sp;
end;

procedure TPLCNumber.SetMinLimit(v:Double);
begin
  if ([csReading, csLoading]*ComponentState=[]) and  (v>=FMaxLimit) then
    raise Exception.Create(SminMustBeLessThanMax);

  FMinLimit:=v;
end;

procedure TPLCNumber.SetMaxLimit(v:Double);
begin
  if ([csReading, csLoading]*ComponentState=[]) and  (v<=FMinLimit) then
    raise Exception.Create(SmaxMustBeGreaterThanMin);

  FMaxLimit:=v;
end;

procedure TPLCNumber.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation=opRemove) and (AComponent=PScaleProcessor) then
    PScaleProcessor:=nil;
  inherited Notification(AComponent,Operation);
end;

procedure TPLCNumber.Write;
var
  towrite:TArrayOfDouble;
begin
  SetLength(towrite,1);
  towrite[0]:=PValueRaw;
  Write(towrite,1,0);
  SetLength(towrite,0);
end;

procedure TPLCNumber.ScanWrite;
var
  towrite:TArrayOfDouble;
begin
  SetLength(towrite,1);
  towrite[0]:=PValueRaw;
  ScanWrite(towrite,1,0);
  SetLength(towrite,0);
end;

procedure TPLCNumber.RemoveScaleProcessor;
begin
  PScaleProcessor := nil;
end;

procedure TPLCNumber.OpenBitMapper(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
var
  dlg:TfrmBitMapper;
  bitnum,
  bytenum,
  wordnum,
  startbit,
  endbit,
  curbit:Integer;
  tbit:TTagBit;

  procedure updatenumbers;
  begin
    bitnum:=curbit;
    if dlg.bitnamestartsfrom1.Checked then inc(bitnum);

    bytenum:=curbit div 8;
    if dlg.bytenamestartsfrom1.Checked then inc(bytenum);

    wordnum:=curbit div 16;
    if dlg.Wordnamestartsfrom1.Checked then inc(wordnum);
  end;

  function GetNewTagBitName:String;
  var
    n:String;
  begin
    n:=IntToStr(bitnum);
    Result:=dlg.edtNamepattern.Text;
    Result := StringReplace(Result,'%b',n,[rfReplaceAll]);

    n:=IntToStr(bytenum);
    Result := StringReplace(Result,'%B',n,[rfReplaceAll]);

    n:=IntToStr(wordnum);
    Result := StringReplace(Result,'%w',n,[rfReplaceAll]);

    n:=Name;
    Result := StringReplace(Result,'%t',n,[rfReplaceAll]);
  end;
begin
  //bit mapper...

  //se não está em design sai.
  //if it's not at designtime, exit.
  if [csDesigning]*ComponentState=[] then exit;

  dlg:=TfrmBitMapper.Create(nil);
  try
    if dlg.ShowModal=mrOK then begin
      startbit:=31-dlg.StringGrid1.Selection.Right;
      endbit:=31-dlg.StringGrid1.Selection.Left;
      curbit:=startbit;
      if dlg.eachbitastag.Checked then begin
        while curbit<=endbit do begin
          updatenumbers;
          tbit:=TTagBit(CreateProc(TTagBit));
          tbit.Name:=GetNewTagBitName;
          tbit.PLCTag:=Self;
          tbit.EndBit:=curbit;
          tbit.StartBit:=curbit;
          InsertHook(tbit);
          inc(curbit);
        end;
      end else begin
        updatenumbers;
        tbit:=TTagBit(CreateProc(TTagBit));
        tbit.Name:=GetNewTagBitName;
        tbit.PLCTag:=Self;
        tbit.EndBit:=endbit;
        tbit.StartBit:=startbit;
        InsertHook(tbit);
      end;
    end;
  finally
    dlg.Destroy;
  end;
end;

end.
 
