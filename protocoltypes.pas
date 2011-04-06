{:
  @abstract(Implementa tipos comuns para drivers de protocolos e tags.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit ProtocolTypes;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface


uses Tag, variants, Classes;


type
  {$IFNDEF FPC}
  PDWord = ^Cardinal;
  {$ENDIF}

  //: Array de objetos.
  TArrayOfObject = array of TObject;

  {:
  Enumera todos os possíveis tipos de dados retornados por um protocolo.
  @value(ptBit      1 bit.)
  @value(ptByte     Inteiro de 8 bits sem sinal.)
  @value(ptShortInt Inteiro de 16 bits COM sinal.)
  @value(ptWord,    Inteiro de 16 bits SEM sinal.)
  @value(ptInteger  Inteiro de 32 bits COM sinal.)
  @value(ptDWord,   Inteiro de 32 bits SEM sinal.)
  @value(ptFloat    Flutuante de 32 bits.)
  }
  TProtocolTagType = (
                   ptBit,
                   ptShortInt,
                   ptByte,
                   ptSmallInt,
                   ptWord,
                   ptInteger,
                   ptDWord,
                   ptFloat
                  );

  //: Adiciona um tag no editor de formulário do Delphi/Lazarus.
  TAddTagInEditorHook = procedure(Tag:TTag) of object;
  //: Cria um componente em tempo de desenvolvimento.
  TCreateTagProc = function(tagclass:TComponentClass):TComponent of object;

  {:
  Estrutura usada internamente pelo driver de protocolo (TProtocoloDriver) para
  processar leitura por scan.
  @member Values Valores lidos pelo ScanRead.
  @member ValuesTimestamp Data/Hora dos valores lidos.
  @member ReadsOK Número de leituras com sucesso da area de memória.
  @member ReadFaults Número de leituras com falha da area de memória.
  @member LastQueryResult Resulado do último pedido de leitura.
  @member Offset Indice dentro do bloco.
  }
  TScanReadRec = record
    Values:TArrayOfDouble;
    ValuesTimestamp:TDateTime;
    ReadsOK,ReadFaults:Cardinal;
    LastQueryResult:TProtocolIOResult;
    Offset,
    RealOffset:Integer;
  end;
  PScanReadRec = ^TScanReadRec;
  
  {:
  Estrutura usada internamente pelos drivers de protocolo (TProtocolDriver) para
  realizar escritas por scan (assincronas).
  @member SWID Identificação do comando de escrita (número único).
  @member Tag  Estrutura que contém as informações do Tag que será escrito.
  @member ValuesToWrite Array com os valores a serem escritos.
  @member WriteResult Resultado do comando de escrita.
  @member ValueTimeStamp Data/Hora em que os valores foram escritos.
  }
  TScanWriteRec = record
    SWID:Cardinal;
    Tag:TTagRec;
    ValuesToWrite:TArrayOfDouble;
    WriteResult:TProtocolIOResult;
    ValueTimeStamp:TDateTime;
  end;
  PScanWriteRec = ^TScanWriteRec;
  
  {:
  Define a função responsável por executar uma escrita por Scan.
  @param(Tag TTagRec: estrutura que contem as informações do tag que vai ser escrito.)
  @param(values TArrayOfDouble: Array com os valores que devem ser escritos.)
  @returns(Todos os possíveis retornos estão definidos em TProtocolIOResult.)
  }
  TScanWriteProc = function(const Tag:TTagRec; const values:TArrayOfDouble):TProtocolIOResult of object;
  //: Ponteiro para uma função de escrita por Scan.
  PScanWriteProc = ^TScanWriteProc;

  {:
  Define o procedimento responsável por executar leituras por Scan.
  @param(Sender TObject: thread do driver que está realizando o scan.)
  @param(Sleep Integer: informa ao driver se ele deve dormir por um tempo, a fim
                        de liberar processador.)
  }
  TScanReadProc = procedure(Sender:TObject; var NeedSleep:Integer) of object;

  {:
  Define o procedimento responsável por buscar os valores de um tag.
  @param(Tag TTagRec: estrutura que contem as informações do tag que vai ser escrito.)
  @param(values TArrayOfDouble: Array onde serão retornados os valores do tag.)
  }
  TGetValues = procedure(const Tag:TTagRec; var values:TScanReadRec) of object;
  TGetMultipleValues = function(var MultiValues:TArrayOfScanUpdateRec):Integer of object;

  //: Interface comum a todos os tags.
  ITagInterface = interface
    ['{188FEF6D-036D-4B01-A854-421973AA7D58}']
    function  GetValueAsText(Prefix, Sufix, Format:string):String;
    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(Value:Variant):Boolean;
    function  GetValueTimestamp:TDatetime;
    property  ValueVariant:Variant read GetVariantValue write SetVariantValue;
    property  ValueTimestamp:TDatetime read GetValueTimestamp;
  end;

  //: Interface de tags numéricos.
  ITagNumeric = interface(ITagInterface)
    ['{F15D0CCC-7C97-4611-A7F4-AD1BEAFA2C96}']
    function  GetValue:Double;
    procedure SetValue(v:Double);
    function  GetValueRaw:Double;
    procedure SetValueRaw(v:Double);

    property Value:Double read GetValue write SetValue;
    property ValueRaw:Double read GetValueRaw write SetValueRaw;
  end;

  //: Interface de tags de texto.
  ITagString = interface(ITagInterface)
    ['{D2CB0A30-B93B-4D8D-BD98-248AE9FC5F22}']
    function  GetValue:String;
    procedure SetValue(v:String);

    property Value:String read GetValue write SetValue;
  end;

const
  //: Constante de mensagem para adicionar um tag no scan.
  PSM_ADDTAG       =  200;

  //: Constante de mensagem para remover um tag do scan.
  PSM_DELTAG       =  201;

  //: Constante de mensagem para suspensão de thread sincronizada.
  PSM_PROTSUSPEND  =  202;

  //: Constante de mensagem para atualização de tag.
  PSM_UPDATETAG    =  203;

  //: Constante de mensagem para leitura de tag por scan.
  PSM_TAGSCANREAD  =  204;

  //: Constante de mensagem para escrita de valores de tags por scan
  PSM_TAGSCANWRITE =  205;

implementation

end.
 
