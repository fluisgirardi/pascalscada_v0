{:
  @abstract(Implementa um tag PLC numérico para uso geral.)
  @author(Fabio Luis Girardi papelhigienico@gmail.com)
}
unit PLCNumber;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, PLCTag, ProtocolTypes, ValueProcessor;

type
  {:
    @abstract(Classe base para tags numéricos.)
    @author(Fabio Luis Girardi papelhigienico@gmail.com)
  }
  TPLCNumber = class(TPLCTag)
  protected
    //: Armazena a sequência de escalas aplicadas a esse tag.
    PScaleProcessor:TPIPE;
    //: Armazena o valor puro (sem escalas) lido @bold(assincrono).
    PValueRaw:Double;

    //: Retorna o valor @bold(assincrono) processado pelas escalas associadas.
    function GetValue:Double; virtual;
    //: Retorna o valor @bold(assincrono) PURO.
    function GetValueRaw:Double; virtual; abstract;

    {: Processa pelas escalas e escreve o valor processado de modo @bold(assincrono).
    @param(Value Double: Valor a processar e escrever.) }
    procedure SetValue(Value:Double); virtual;
    {: Escreve o valor do tag de modo @bold(assincrono).
       @param(Value Double: Valor a escrever.) }
    procedure SetValueRaw(Value:Double); virtual; abstract;
    {:
    Configura a sequência de escalas.
    @param(sp TPIPE: Nova sequência de escalas.)
    @seealso(ScaleProcessor)
    }
    procedure SetScaleProcessor(sp:TPIPE);
  public
    //: @exclude
    destructor Destroy; override;

    //: Abre o assistente de mapeamento de tags bit.
    procedure OpenBitMapper(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); virtual;

    //: @seealso(TPLCTag.Write)
    procedure Write; overload; virtual;
    //: @seealso(TPLCTag.Write)
    procedure ScanWrite; overload; virtual;
    //: Remove a sequência de processamento de escalas.
    procedure RemoveScaleProcessor;
    //: Valor do tag escalonado (processado) @bold(assincrono).
    property Value:Double read GetValue write SetValue;
    //: Valor do tag puro @bold(assincrono).
    property ValueRaw:Double read PValueRaw write SetValueRaw;
  published
    //: Sequência de escalas do Tag.
    property ScaleProcessor:TPIPE  read PScaleProcessor write SetScaleProcessor;
    //: Evento chamado ao ocorrer uma mudança no valor do tag.
    property OnValueChange;
  end;

implementation

uses ubitmapper, Controls, TagBit, tag;

destructor TPLCNumber.Destroy;
begin
  SetScaleProcessor(nil);
  inherited Destroy;
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
  if Assigned(PScaleProcessor) then
    towrite := PScaleProcessor.SetOutGetIn(self, Value)
  else
    towrite := value;

  SetValueRaw(towrite);
end;

procedure TPLCNumber.SetScaleProcessor(sp:TPIPE);
begin
  if sp=PScaleProcessor then exit;

  if PScaleProcessor<>nil then
    PScaleProcessor.DelTag(self);

  if sp<>nil then
    sp.AddTag(self);

  PScaleProcessor := sp;
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
  //se não está em design sai.
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
 
