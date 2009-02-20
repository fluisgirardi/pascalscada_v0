//: Implementa um tag PLC numérico.
unit PLCNumber;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, PLCTag, ProtocolTypes, ValueProcessor;

type
  //: Classe base para tags numéricos.
  TPLCNumber = class(TPLCTag)
  protected
    //: Armazena a seqüência de escalas aplicadas a esse tag.
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
    Configura a sequencia de escalas.
    @param(sp TPIPE: Nova seqüência de escalas.)
    @seealso(ScaleProcessor)
    }
    procedure SetScaleProcessor(sp:TPIPE);
  public
    //: @exclude
    destructor Destroy; override;

    procedure Write; overload; virtual;
    procedure ScanWrite; overload; virtual;
    //: Remove a seqüência de processamento de escalas.
    procedure RemoveScaleProcessor;
    //: Valor do tag escalonado (processado) @bold(assincrono).
    property Value:Double read GetValue write SetValue;
    //: Valor do tag puro @bold(assincrono).
    property ValueRaw:Double read PValueRaw write SetValueRaw;
  published
    //: Seqüência de escalas do Tag.
    property ScaleProcessor:TPIPE  read PScaleProcessor write SetScaleProcessor;
    //: Evento chamado ao ocorrer uma mudança no valor do tag.
    property OnValueChange;
  end;

implementation

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

end.
 
