{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementa tipos comuns para drivers de protocolos e tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit that implements common types for protocol drivers and tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit ProtocolTypes;

interface


uses Tag, variants, Classes, sysutils;


type
  {$IFNDEF FPC}
  PDWord = ^Cardinal;
  {$ENDIF}

  {$IFDEF PORTUGUES}
  //: Array de objetos.
  {$ELSE}
  //: Object array.
  {$ENDIF}
  TArrayOfObject = array of TObject;

  {$IF defined(FPC_FULLVERSION) AND (FPC_FULLVERSION < 20701)}
  TFormatDateTimeOption = (fdoInterval);
  TFormatDateTimeOptions =  set of TFormatDateTimeOption;
  {$IFEND}

  {$IFDEF PORTUGUES}
  {:
  Enumera todos os possíveis tipos de dados retornados por um protocolo.
  @value(ptBit       1 bit.)
  @value(ptByte      Inteiro de 8 bits SEM sinal.)
  @value(ptShortInt  Inteiro de 8 bits COM sinal.)
  @value(ptWord,     Inteiro de 16 bits SEM sinal.)
  @value(ptSmallInt, Inteiro de 16 bits COM sinal.)
  @value(ptDWord,    Inteiro de 32 bits SEM sinal.)
  @value(ptLongInt   Inteiro de 32 bits COM sinal.)
  @value(ptFloat     Flutuante de 32 bits.)
  @value(ptInt64     Inteiro de 64 bits COM sinal)
  @value(ptQWord     Inteiro de 64 bits SEM sinal)
  @value(ptDouble    Flutuante de 64 bits.)
  }
  {$ELSE}
  {:
  Enumerates all datatypes that can be returned by a protocol driver.
  @value(ptBit       1 bit.)
  @value(ptByte      Unsigned LongInt, 8 bits sized.)
  @value(ptShortInt  Signed LongInt, 8 bits sized.)
  @value(ptWord,     Unsigned LongInt, 16 bits sized.)
  @value(ptSmallInt, Signed LongInt, 16 bits sized.)
  @value(ptDWord,    Unsigned LongInt, 32 bits sized.)
  @value(ptLongInt   Signed LongInt, 32 bits sized.)
  @value(ptFloat     Float, 32 bits sized.)
  @value(ptInt64     Signed LongInt, 64 bits sized)
  @value(ptQWord     Unsigned LongInt, 64 bits sized)
  @value(ptDouble    Float, 64 bits sized.)
  }
  {$ENDIF}
  TProtocolTagType = (ptUnknown,
                      ptBit,
                      ptShortInt,
                      ptByte,
                      ptSmallInt,
                      ptWord,
                      ptLongInt,
                      ptDWord,
                      ptFloat,
                      ptInt64,
                      ptQWord,
                      ptDouble
                     );

  {$IFDEF PORTUGUES}
  {:
    Que tipo de coisa um tag listado do CLP é, na visão de quem vai criar um tag
    do PascalSCADA para ele.
    @value(tkScalar Um valor só: TPLCTagNumber, com TagType.)
    @value(tkArray Vários valores do mesmo tipo: TPLCBlock com ElementCount elementos.)
    @value(tkStructure DB ou estrutura (UDT): não é um valor, é o nível que agrupa os
          membros, que vêm na lista como entradas próprias. SizeInBytes é o tamanho.)
    @value(tkString Texto: TPLCString. ElementCount é o número máximo de caracteres,
          0 quando o CLP não informa.)
    @value(tkUnsupported Existe no CLP mas o driver não lê (tipo sem equivalente).)
  }
  {$ELSE}
  {:
    What a tag listed from the PLC is, for whoever is going to create a PascalSCADA
    tag for it.
    @value(tkScalar A single value: TPLCTagNumber, with TagType.)
    @value(tkArray Several values of the same type: TPLCBlock with ElementCount elements.)
    @value(tkStructure A DB or a structure (UDT): not a value, the level that groups
          its members, which come on the list as entries of their own. SizeInBytes is
          its size.)
    @value(tkString Text: TPLCString. ElementCount is the maximum number of characters,
          0 when the PLC does not say.)
    @value(tkUnsupported It exists in the PLC but the driver cannot read it (a type with
          no equivalent).)
  }
  {$ENDIF}
  TPLCTagKind = (tkScalar, tkArray, tkStructure, tkString, tkUnsupported);

  {$IFDEF PORTUGUES}
  {:
    Um tag do CLP, como TProtocolDriver.ListTags o devolve - o mesmo formato para todos
    os drivers que sabem listar os tags do equipamento.
  }
  {$ELSE}
  {:
    A PLC tag, as TProtocolDriver.ListTags returns it - the same format for every
    driver that knows how to list the device's tags.
  }
  {$ENDIF}
  TPLCTagInfo = record
    {$IFDEF PORTUGUES}
    //: Endereço do tag, o que vai na LongAddress do tag do PascalSCADA.
    {$ELSE}
    //: Tag address, what goes in the LongAddress of the PascalSCADA tag.
    {$ENDIF}
    Path:String;
    //: Name (the last level of Path).
    Name:String;
    //: Path of the structure/DB that holds it ('' at the top level).
    ParentPath:String;
    {$IFDEF PORTUGUES}
    //: Onde ele mora: o DB ou a área (S7), o programa do controlador (Rockwell); '' se nenhum.
    {$ELSE}
    //: Where it lives: the DB or the area (S7), the controller program (Rockwell); '' if none.
    {$ENDIF}
    Group:String;
    Kind:TPLCTagKind;
    {$IFDEF PORTUGUES}
    //: Nome do tipo no CLP (DInt, Real, STRING...).
    {$ELSE}
    //: Name of the type in the PLC (DInt, Real, STRING...).
    {$ENDIF}
    NativeType:String;
    {$IFDEF PORTUGUES}
    //: TagType do tag que o lê (tkScalar e tkArray); pttDefault nos demais.
    {$ELSE}
    //: TagType of the tag that reads it (tkScalar and tkArray); pttDefault on the others.
    {$ENDIF}
    TagType:TTagType;
    {$IFDEF PORTUGUES}
    //: Elementos de um tkArray (1 nos demais); caracteres de um tkString, se conhecido.
    {$ELSE}
    //: Elements of a tkArray (1 on the others); characters of a tkString, if known.
    {$ENDIF}
    ElementCount:LongInt;
    {$IFDEF PORTUGUES}
    //: Tamanho em bytes; 0 se desconhecido. Em um tkStructure, ao menos isso se não for exato.
    {$ELSE}
    //: Size in bytes; 0 if unknown. On a tkStructure, at least this when it is not exact.
    {$ENDIF}
    SizeInBytes:LongInt;
    {$IFDEF PORTUGUES}
    //: Os Swap* que o tag precisa para ler o valor certo (o CLP S7 manda big-endian).
    {$ELSE}
    //: The Swap* the tag needs to read the right value (an S7 PLC sends big-endian).
    {$ENDIF}
    SwapBytes, SwapWords, SwapDWords:Boolean;
  end;

  //: Lista de tags de um CLP.
  TPLCTagInfoArray = array of TPLCTagInfo;

  {$IFDEF PORTUGUES}
  //: Adiciona um tag no editor de formulário do Delphi/Lazarus.
  {$ELSE}
  //: Add a tag in the form editor of Delphi/Lazarus.
  {$ENDIF}
  TAddTagInEditorHook = procedure(Tag:TTag) of object;

  {$IFDEF PORTUGUES}
  //: Cria um componente em tempo de desenvolvimento.
  {$ELSE}
  //: Creates a component in design-time.
  {$ENDIF}
  TCreateTagProc = function(tagclass:TComponentClass):TComponent of object;

  {$IFDEF PORTUGUES}
  {: Assinatura do metodo que chama a ferramenta Tag Builder de um protocolo em
     tempo de desenvolvimento.
  }
  {$ELSE}
  {: Method signature that calls the Tag Builder tool of a protocol driver
     at design-time.
  }
  {$ENDIF}
  TOpenTagEditor = procedure(Target,
                             OwnerOfNewTags:TComponent;
                             InsertHook:TAddTagInEditorHook;
                             CreateProc:TCreateTagProc);



  {$IFDEF PORTUGUES}
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
  {$ELSE}
  {:
  Record used internaly by the protocol driver to process scan read commands.
  @member Values Values read by the Scan Read.
  @member ValuesTimestamp Date/time when the values was read.
  @member ReadsOK Number of sucessfull reads.
  @member ReadFaults Number of failed reads.
  @member LastQueryResult I/O result of the last read request.
  @member Offset block index.
  }
  {$ENDIF}
  TScanReadRec = record
    Values:TArrayOfDouble;
    ClkMonotonicTStamp:QWord;
    ReadsOK,ReadFaults:Cardinal;
    LastQueryResult:TProtocolIOResult;
    Offset,
    RealOffset:LongInt;
  end;
  PScanReadRec = ^TScanReadRec;
  

  {$IFDEF PORTUGUES}
  {:
  Estrutura usada internamente pelos drivers de protocolo (TProtocolDriver) para
  realizar escritas por scan (assincronas).
  @member SWID Identificação do comando de escrita (número único).
  @member Tag  Estrutura que contém as informações do Tag que será escrito.
  @member ValuesToWrite Array com os valores a serem escritos.
  @member WriteResult Resultado do comando de escrita.
  @member ValueTimeStamp Data/Hora em que os valores foram escritos.
  }
  {$ELSE}
  {:
  Record used internaly by protocol driver to execute write by scan (asynchronous).
  @member SWID Identification of the scan write command.
  @member Tag Structure with informations about the tag.
  @member ValuesToWrite Array of values to be written.
  @member WriteResult I/O result of the write command.
  @member ValueTimeStamp Date/time when the values was written.
  }
  {$ENDIF}
  TScanReqRec = record
    Tag:TTagRec;
    Values:TArrayOfDouble;
    RequestResult:TProtocolIOResult;
    ClkMonotonicTStamp:QWord;
  end;
  PScanReqRec = ^TScanReqRec;

  {$IFDEF PORTUGUES}
  {:
  Uma escrita de um tag dentro de um lote (TProtocolDriver.MultiWrite/ScanMultiWrite).
  @member Tag Estrutura com as informações do tag que será escrito.
  @member Values Valores a escrever.
  @member Result Resultado dessa escrita, preenchido pelo driver (cada item tem o seu).
  }
  {$ELSE}
  {:
  One tag write inside a batch (TProtocolDriver.MultiWrite/ScanMultiWrite).
  @member Tag Record with informations about the tag.
  @member Values Values to be written.
  @member Result I/O result of this write, filled by the driver (each item has its own).
  }
  {$ENDIF}
  TMultiWriteRec = record
    Tag:TTagRec;
    Values:TArrayOfDouble;
    Result:TProtocolIOResult;
  end;
  TMultiWriteArray = array of TMultiWriteRec;

  {$IFDEF PORTUGUES}
  //: Pedidos de escrita por scan de um lote; cada ponteiro e' um pedido normal (TScanReqRec).
  {$ELSE}
  //: Scan write requests of one batch; each pointer is an ordinary request (TScanReqRec).
  {$ENDIF}
  TScanReqRecArray = array of PScanReqRec;

  {$IFDEF PORTUGUES}
  //: Mensagem de um lote de escritas por scan, enviada a thread do driver (PSM_TAGSCANMULTIWRITE).
  {$ELSE}
  //: Message of a batch of scan writes sent to the driver thread (PSM_TAGSCANMULTIWRITE).
  {$ENDIF}
  TScanMultiReqRec = record
    Requests:TScanReqRecArray;
  end;
  PScanMultiReqRec = ^TScanMultiReqRec;

  {$IFDEF PORTUGUES}
  {:
  Define o procedimento responsavel por executar um lote de escritas por Scan. Preenche o
  RequestResult de cada pedido; a entrega aos tags e' feita por quem chama.
  }
  {$ELSE}
  {:
  Defines the procedure that executes a batch of scan writes. It fills each request's
  RequestResult; delivering the results to the tags is up to the caller.
  }
  {$ENDIF}
  TScanMultiWriteProc = procedure(const Requests:TScanReqRecArray) of object;

  {$IFDEF PORTUGUES}
  {:
  Define a função responsável por executar uma escrita por Scan.
  @param(Tag TTagRec: estrutura que contem as informações do tag que vai ser escrito.)
  @param(values TArrayOfDouble: Array com os valores que devem ser escritos.)
  @returns(Todos os possíveis retornos estão definidos em TProtocolIOResult.)
  }
  {$ELSE}
  {:
  Defines the function that will execute a write by scan (asynchronous)
  @param(Tag TTagRec: structure with informations about the tag.)
  @param(values TArrayOfDouble: Array of values to be written.)
  @returns(See TProtocolIOResult.)
  }
  {$ENDIF}
  TScanWriteProc = function(const Tag:TTagRec; const values:TArrayOfDouble):TProtocolIOResult of object;

  {$IFDEF PORTUGUES}
  {:
  Define a função responsável por executar uma leitura por Scan.
  @param(Tag TTagRec: estrutura que contem as informações do tag que vai ser lido.)
  @param(values TArrayOfDouble: Array com os valores lidos.)
  @returns(Todos os possíveis retornos estão definidos em TProtocolIOResult.)
  }
  {$ELSE}
  {:
  Defines the function that will execute a write by scan (asynchronous)
  @param(Tag TTagRec: structure with informations about the tag.)
  @param(values TArrayOfDouble: Array of values to be written.)
  @returns(See TProtocolIOResult.)
  }
  {$ENDIF}
  TSingleScanReadProc = function(var Tag:TTagRec; var values:TArrayOfDouble):TProtocolIOResult of object;

  {$IFDEF PORTUGUES}
  //: Ponteiro para uma função de escrita por Scan.
  {$ELSE}
  //: Points to scan write function.
  {$ENDIF}
  PScanWriteProc = ^TScanWriteProc;

  {$IFDEF PORTUGUES}
  {:
  Define o procedimento responsável por executar leituras por Scan.
  @param(Sender TObject: thread do driver que está realizando o scan.)
  @param(Sleep LongInt: informa ao driver se ele deve dormir por um tempo, a fim
                        de liberar processador.)
  }
  {$ELSE}
  {:
  Defines the procedure that will execute the scan read commands.
  @param(Sender TObject: Thread object that is calling the procedure.)
  @param(Sleep LongInt: Tells to The caller thread if it must sleep or switch to
                        another thread.)
  }
  {$ENDIF}
  TScanReadProc = procedure(Sender:TObject; var NeedSleep:LongInt) of object;

  {$IFDEF PORTUGUES}
  {:
  Define o procedimento responsável por buscar os valores de um tag.
  @param(Tag TTagRec: estrutura que contem as informações do tag que vai ser escrito.)
  @param(values TArrayOfDouble: Array onde serão retornados os valores do tag.)
  }
  {$ELSE}
  {:
  Defines the procedure that will get values of one tag.
  @param(Tag TTagRec: structure with informations about the tag.)
  @param(values TArrayOfDouble: Array with tag values.)
  }
  {$ENDIF}
  TGetValues = procedure(const Tag:TTagRec; var values:TScanReadRec) of object;

  {$IFDEF PORTUGUES}
  {:
  Procedimento que busca valores de vários tags simultaneamente. Usado pela
  thread de atualização.
  @param(MultiValues TArrayOfScanUpdateRec. Retorna os dados para atualizar
         vários tags.)
  }
  {$ELSE}
  {:
  Procedure that gets values of a set of tags simultaneously. Used by Scan
  Update thread.
  @param(MultiValues TArrayOfScanUpdateRec. Returns information to update a set
         of tags.)
  }
  {$ENDIF}
  TGetMultipleValues = function(var MultiValues:TArrayOfScanUpdateRec):LongInt of object;

  {$IFDEF PORTUGUES}
  //: Interface comum a todos os tags.
  {$ELSE}
  //: Tag interface
  {$ENDIF}
  ITagInterface = interface
    ['{188FEF6D-036D-4B01-A854-421973AA7D58}']

    {$IFDEF PORTUGUES}
    //: Retorna o valor do tag como String, incluindo formatação (se aplicável), prefixo e sufixo.
    {$ELSE}
    //: Returns the tag value as string, including the format (if applicable), prefix and suffix.
    {$ENDIF}
    function  GetValueAsText(Prefix, Sufix, Format:UTF8String; FormatDateTimeOptions:TFormatDateTimeOptions=[]):UTF8String;

    {$IFDEF PORTUGUES}
    //: Retorna o valor do tag como variant.
    {$ELSE}
    //: Returns the tag value as variant.
    {$ENDIF}
    function  GetVariantValue:Variant;

    {$IFDEF PORTUGUES}
    //: Seta um variant como valor do tag se possível.
    {$ELSE}
    //: If possible, sets a variant as tag value.
    {$ENDIF}
    procedure SetVariantValue(V:Variant);

    {$IFDEF PORTUGUES}
    //: Retorna @true se o valor é aceito pelo tag.
    {$ELSE}
    //: Returns @true if the variant value will be accept by tag.
    {$ENDIF}
    function  IsValidValue(Value:Variant):Boolean;

    {$IFDEF PORTUGUES}
    //: Retorna a data/hora em que o tag foi atualizado pela última vez.
    {$ELSE}
    //: Returns the date/time of the last time wich the tag was updated.
    {$ENDIF}
    function  GetClockMonotonicTimestamp:QWord;

    {$IFDEF PORTUGUES}
    //: Lê/Escreve um valor variant do tag.
    {$ELSE}
    //: Read/Set a variant value on tag.
    {$ENDIF}
    property  ValueVariant:Variant read GetVariantValue write SetVariantValue;

    {$IFDEF PORTUGUES}
    //: Retorna a data/hora em que o tag foi atualizado pela última vez.
    {$ELSE}
    //: Returns the date/time of the last time wich the tag was updated.
    {$ENDIF}
    property  ClockMonotonicTimestamp:QWord read GetClockMonotonicTimestamp;
  end;


  {$IFDEF PORTUGUES}
  //: Interface de tags numéricos.
  {$ELSE}
  //: Numeric tag interface.
  {$ENDIF}
  ITagNumeric = interface(ITagInterface)
    ['{F15D0CCC-7C97-4611-A7F4-AD1BEAFA2C96}']
    {$IFDEF PORTUGUES}
    //: Retorna o valor processado pelas escalas associadas, ou o valor puro caso contrário.
    {$ELSE}
    //: Returns the value processed by the linked scales or the value raw.
    {$ENDIF}
    function  GetValue:Double;

    {$IFDEF PORTUGUES}
    {: Processa pelas escalas associadas e escreve o valor processado no dispositivo.
    @param(Value Double: Valor a processar e escrever.) }
    {$ELSE}
    {:
    Processes the value using linked scales and writes the value processed on device.
    @param(Value Double: Value to be processed and written in device.)
    }
    {$ENDIF}
    procedure SetValue(v:Double);

    {$IFDEF PORTUGUES}
    //: Retorna o valor PURO.
    {$ELSE}
    //: Returns the raw value.
    {$ENDIF}
    function  GetValueRaw:Double;

    {$IFDEF PORTUGUES}
    {: Escreve o valor puro do tag no dispositivo.
       @param(Value Double: Valor a escrever.) }
    {$ELSE}
    {: Write the raw value on device.
       @param(Value Double: Value to be written.) }
    {$ENDIF}
    procedure SetValueRaw(v:Double);

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
    property ValueRaw:Double read GetValueRaw write SetValueRaw;
  end;

  {$IFDEF PORTUGUES}
  //: Interface de tags de texto.
  {$ELSE}
  //: Text tag interface
  {$ENDIF}
  ITagString = interface(ITagInterface)
    ['{D2CB0A30-B93B-4D8D-BD98-248AE9FC5F22}']
    {$IFDEF PORTUGUES}
    //: Retorna o valor do tag texto.
    {$ELSE}
    //: Returns the text value of tag.
    {$ENDIF}
    function  GetValue:UTF8String;

    {$IFDEF PORTUGUES}
    //: Escreve um texto no tag.
    {$ELSE}
    //: Writes a text value on tag.
    {$ENDIF}
    procedure SetValue(v:UTF8String);

    //: Read/write a text value on tag.
    property Value:UTF8String read GetValue write SetValue;
  end;

const

  ProtocolTagTypeSizeInBits:array[low(TProtocolTagType)..high(TProtocolTagType)] of integer = (0,
                                                                                               1,
                                                                                               8,
                                                                                               8,
                                                                                               16,
                                                                                               16,
                                                                                               32,
                                                                                               32,
                                                                                               32,
                                                                                               64,
                                                                                               64,
                                                                                               64);

  {$IFDEF PORTUGUES}
  //: Constante de mensagem para leitura de tag por scan.
  {$ELSE}
  //: Identifies a message that does a scan read.
  {$ENDIF}
  PSM_TAGSCANREAD  =  204;

  {$IFDEF PORTUGUES}
  //: Constante de mensagem para escrita de valores de tags por scan
  {$ELSE}
  //: Identifies a message that does a scan write.
  {$ENDIF}
  PSM_TAGSCANWRITE =  205;

  {$IFDEF PORTUGUES}
  //: Constante de mensagem para escrita de valores de tags por scan
  {$ELSE}
  //: Identifies a message that does a scan write.
  {$ENDIF}
  PSM_SINGLESCANREAD =  206;

  {$IFDEF PORTUGUES}
  //: Constante de mensagem para um lote de escritas de valores de tags por scan
  {$ELSE}
  //: Identifies a message that does a batch of scan writes.
  {$ENDIF}
  PSM_TAGSCANMULTIWRITE =  207;

implementation

end.
 
