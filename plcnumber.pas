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
    //: Armazena o valor puro (sem escalas) lido @bold(sincrono).
    PValueDirectRaw:Double;
    //: Armazena o valor puro (sem escalas) lido @bold(assincrono).
    PValueRaw:Double;
    //: Retorna o valor @bold(assincrono) processado pelas escalas associadas.
    function GetValue:Double; virtual; abstract;
    //: Retorna o valor @bold(sincrono) processado pelas escalas associadas.
    function GetValueDirect:Double; virtual; abstract;
    //: Retorna o valor @bold(sincrono) puro (sem ser processado pelas escalas associadas).
    function GetValueDirectRaw:Double;
    {:
    Processa pelas escalas e escreve o valor processado de modo @bold(assincrono).
    @param(Value Double: Valor a processar e escrever.)
    }
    procedure SetValue(Value:Double);
    {:
    Escreve o valor do tag de modo @bold(assincrono).
    @param(Value Double: Valor a escrever.)
    }
    procedure SetValueRaw(Value:Double); virtual; abstract;
    {:
    Processa pelas escalas e escreve o valor processado de modo @bold(sincrono).
    @param(Value Double: Valor a processar e escrever.)
    }
    procedure SetValueDirect(Value:Double);
    {:
    Escreve o valor do tag de modo @bold(sincrono).
    @param(Value Double: Valor a escrever.)
    }
    procedure SetValueDirectRaw(Value:Double); virtual; abstract;
    {:
    Configura a sequencia de escalas.
    @param(sp TPIPE: Nova seqüência de escalas.)
    @seealso(ScaleProcessor)
    }
    procedure SetScaleProcessor(sp:TPIPE);
  published
    //: Seqüência de escalas do Tag.
    property ScaleProcessor:TPIPE  read PScaleProcessor write SetScaleProcessor;
    //: Evento chamado ao ocorrer uma mudança no valor do tag.
    property OnValueChange;
  public
    //: Remove a seqüência de processamento de escalas.
    procedure RemoveScaleProcessor;
    //: Valor do tag escalonado (processado) @bold(assincrono).
    property Value:Double read GetValue write SetValue;
    //: Valor do tag puro @bold(assincrono).
    property ValueRaw:Double read PValueRaw write SetValueRaw;
    //: Valor do tag escalonado (processado) @bold(sincrono).
    property ValueDirect:Double read GetValueDirect write SetValueDirect;
    //: Valor do tag puro @bold(sincrono).
    property ValueDirectRaw:Double read GetValueDirectRaw write SetValueDirectRaw;
  end;

implementation

uses Tag;

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

function TPLCNumber.GetValueDirectRaw:Double;
begin
  Read;
  Result := PValueDirectRaw;
end;

procedure TPLCNumber.SetValueDirect(Value:Double);
var
  towrite:Double;
begin
  if PScaleProcessor=nil then
    towrite := value
  else
    towrite := PScaleProcessor.SetOutGetIn(self, Value);
  SetValueDirectRaw(towrite);
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

procedure TPLCNumber.RemoveScaleProcessor;
begin
  PScaleProcessor := nil;
end;

end.
 
