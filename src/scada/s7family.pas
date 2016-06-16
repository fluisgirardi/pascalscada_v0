{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementação do protocolo ISOTCP.)
  Este driver é baseado no driver ISOTCP da biblioteca
  LibNODAVE de Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Este driver não usa Libnodave, ele é uma reescrita da mesma.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implementation of ISOTCP protocol.)
  This driver is based on ISOTCP implementation of LibNODAVE library of Thomas
  Hergenhahn (thomas.hergenhahn@web.de).

  This driver doesn't use the Libnodave library, it's a rewritten of it.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Moved OpenTagEditor to SiemensTagAssistant to remove TForm dependencies
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
{$ENDIF}
unit s7family;

interface

uses
  classes, sysutils, ProtocolDriver, S7Types, Tag, ProtocolTypes, fgl, s7scanreq,
  commtypes;

type
  TS7ScanReqList = specialize TFPGList<TS7ScanReqItem>;

  {$IFDEF PORTUGUES}
  {: Familia de drivers Siemens S7. Baseado na biblioteca LibNodave
     de Thomas Hergenhahn (thomas.hergenhahn@web.de).

  Este driver não usa Libnodave, ele é uma reescrita da mesma.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Para configurar um tag, é necessário preencher as propriedade PLCStation,
  PLCRack e PLCSlot para endereçar o CLP. Para endereçar a memória dentro do CLP
  é necessário prencher as propriedades MemAddress e MemReadFunction e no caso
  de um DB preencha também a propriedade MemFile_DB com o número do DB. O tipo
  da variável é escolida na propriedade TagType.

  Caso o tipo da variável seja Word, ShortInt, LongInt, DWord ou Real é
  necessário que as propriedades SwapBytes e SwapWord sejam iguais a true para
  que os valores fiquem iguais aos do CLP.

  Para escolher o tipo (área de memória) que se deseja lêr preencha a
  propriedade MemReadFunction de acordo com a tabela abaixo:

  Area:
  @table(
    @rowHead( @cell(Area)                                 @cell(Valor) @cell(Observação) )
    @row(     @cell(Inputs, Entradas)                     @cell( 1)    @cell( - ))
    @row(     @cell(Outputs, Saidas)                      @cell( 2)    @cell( - ))
    @row(     @cell(Flags ou M's)                         @cell( 3)    @cell( - ))
    @row(     @cell(DB e VM no S7-200 )                   @cell( 4)    @cell( - ))
    @row(     @cell(Counter, S7 300/400)                  @cell( 5)    @cell(Precisa que a propriedade TagType seja igual pttWord))
    @row(     @cell(Timer, S7 300/400)                    @cell( 6)    @cell(Precisa que a propriedade TagType seja igual pttWord))

    @row(     @cell(Special Memory, SM, S7-200)           @cell( 7)    @cell( - ))
    @row(     @cell(Entrada analógica, S7-200)            @cell( 8)    @cell( - ))
    @row(     @cell(Saida analógica, S7-200)              @cell( 9)    @cell( - ))
    @row(     @cell(Counter, S7-200)                      @cell(10)    @cell(Precisa que a propriedade TagType seja igual pttWord))
    @row(     @cell(Timer, S7-200)                        @cell(11)    @cell(Precisa que a propriedade TagType seja igual pttWord))

    @row(     @cell(Entrada analógica (PIW), S7 300/400)  @cell(12)    @cell(Precisa que a propriedade TagType seja igual pttWord))
  )

  Logo para acessar a IB3, basta colocar na propriedade MemReadFunction o valor
  1, MemAddress o valor 3 e na propriedade TagType o valor pttByte e para
  acessar a MD100 (DWord) basta colocar o valor 5 em MemReadFunction, 100 em
  MemAddres e pttDword em TagType.
  }
  {$ELSE}
  {: Siemens S7 protocol drivers family. Based on LibNodave library of
  Thomas Hergenhahn (thomas.hergenhahn@web.de).

  This driver doesn't use the Libnodave library, it's a rewritten of it.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  To setup a tag, you must set the properties PLCStation, PLCRack and PLCSlot
  to address your PLC. To address a memory inside the PLC, you must set the
  properties MemAddress and MemReadFunction (see the table below) and if is a DB
  that is being addressed, set the DB number on property MemFile_DB. The datatype
  of tag can be selected on property TagType.

  If the TagType is a Word, SmalltInt, LongInt, DWord or Float you must set too
  the properties SwapBytes and SwapWord to @true to get the PLC values correctly.

  To choose the memory area, set the property MemReadFunction as show below:

  Area:
  @table(
    @rowHead( @cell(Area)                            @cell(Value) @cell(Observation) )
    @row(     @cell(Inputs)                          @cell( 1)    @cell( - ))
    @row(     @cell(Outputs)                         @cell( 2)    @cell( - ))
    @row(     @cell(Flags or M's)                    @cell( 3)    @cell( - ))
    @row(     @cell(DB and V no S7-200 )             @cell( 4)    @cell( - ))
    @row(     @cell(Counter, S7 300/400)             @cell( 5)    @cell(TagType property must be pttWord))
    @row(     @cell(Timer, S7 300/400)               @cell( 6)    @cell(TagType property must be pttWord))

    @row(     @cell(Special Memory, SM, S7-200)      @cell( 7)    @cell( - ))
    @row(     @cell(Analog Input, S7-200)            @cell( 8)    @cell( - ))
    @row(     @cell(Analog Output, S7-200)           @cell( 9)    @cell( - ))
    @row(     @cell(Counter, S7-200)                 @cell(10)    @cell(TagType property must be pttWord))
    @row(     @cell(Timer, S7-200)                   @cell(11)    @cell(TagType property must be pttWord))

    @row(     @cell(Analog Input (PIW), S7 300/400)  @cell(12)    @cell(TagType property must be pttWord))
  )

  So, to address the IB3, you must set the property MemReadFunction the value 1,
  the MemAddress the value 3 and the property TagType the value pttByte. To
  address the MD100 (DWord) you must set MemReadFunction to 5, the MemAddres to
  100 and pttDword on TagType.

  }
  {$ENDIF}

  { TSiemensProtocolFamily }

  TSiemensProtocolFamily = class(TProtocolDriver)
  protected
    {$IFDEF PORTUGUES}
    //: Retorna uma estrutura com informações sobre o tag.
    {$ELSE}
    //: Returns a structure with informations about the tag.
    {$ENDIF}
    function  GetTagInfo(tagobj:TTag):TTagRec; virtual;

    {$IFDEF PORTUGUES}
    //: Pega um byte de um ponteiro de bytes.
    {$ELSE}
    //: Gets a byte from a pointer of bytes.
    {$ENDIF}
    function  GetByte(Ptr:PByte; idx:LongInt):LongInt;

    {$IFDEF PORTUGUES}
    //: Seta um byte em um ponteiro de bytes.
    {$ELSE}
    //: Sets a byte in a pointer of bytes.
    {$ENDIF}
    procedure SetByte(Ptr:PByte; idx:LongInt; value:Byte);

    {$IFDEF PORTUGUES}
    //: Seta varios bytes em um ponteiro de bytes.
    {$ELSE}
    //: Sets a lot of bytes in a pointer of bytes.
    {$ENDIF}
    procedure SetBytes(Ptr:PByte; idx:LongInt; values:BYTES);
  protected
    {$IFDEF PORTUGUES}
    //: Onde inicia a PDU nos pacotes que chegam e que saem.
    {$ELSE}
    //: Where PDU starts on incoming and outgoing packets.
    {$ENDIF}
    PDUIncoming, PDUOutgoing:LongInt;

    {$IFDEF PORTUGUES}
    {:
    Lista de CPU's sendo lidas pelo driver.
    @seealso(TS7CPU)
    }
    {$ELSE}
    {:
    Lists all CPU's being read by the protocol driver.
    @seealso(TS7CPU)
    }
    {$ENDIF}
    FPLCs:TS7CPUs;

    {$IFDEF PORTUGUES}
    {:
    Caso for um protocolo que precise inicializar o adaptador, armazena se o
    adaptador foi inicializado.
    }
    {$ELSE}
    {:
    If the protocol needs to initialize the adapter, stores if it was initialized.
    }
    {$ENDIF}
    FAdapterInitialized:Boolean;

    {$IFDEF PORTUGUES}
    //: Inicializa o adaptador, se necessário.
    {$ELSE}
    //: Initializes the adapter, if needed.
    {$ENDIF}
    function  initAdapter:Boolean; virtual;

    {$IFDEF PORTUGUES}
    //: Desconecta do adaptador.
    {$ELSE}
    //: Disconnects from adapter.
    {$ENDIF}
    function  disconnectAdapter:Boolean; virtual;

    {$IFDEF PORTUGUES}
    {:
    Conecta em um CLP.
    @param(CPU TS7CPU. Representa o CLP a ser conectado.)
    }
    {$ELSE}
    {:
    Connects on a PLC.
    @param(CPU TS7CPU. Represents the PLC to connect.)
    }
    {$ENDIF}
    function  connectPLC(var CPU:TS7CPU):Boolean; virtual;

    {$IFDEF PORTUGUES}
    {:
    Desconecta de um CLP.
    @param(CPU TS7CPU. Representa o CLP a ser desconectado.)
    }
    {$ELSE}
    {:
    Disconnects from a PLC.
    @param(CPU TS7CPU. Represents the PLC to disconnect.)
    }
    {$ENDIF}
    function  disconnectPLC(var CPU:TS7CPU):Boolean; virtual;

    {$IFDEF PORTUGUES}
    {:
    Troca dados com um CLP.
    @param(CPU TS7CPU. CLP na qual dados vao ser trocados.)
    @param(msgOut BYTES. Mensagem a enviar para o CLP.)
    @param(msgIn BYTES. Armazena a mensagem recebida do CLP.)
    @param(IsWrite Boolean. Diz se a mensagem que está sendo enviada vai escrever
           nas memorias do CLP.)
    @returns(@True se bem sucessedido.)
    }
    {$ELSE}
    {:
    Exchange data with a PLC.
    @param(CPU TS7CPU. PLC to exchange data.)
    @param(msgOut BYTES. Packet to send to PLC.)
    @param(msgIn BYTES. Stores the received packet from PLC.)
    @param(IsWrite Boolean. Tells if the message to be send to PLC will write
           in PLC memory.)
    @returns(@True if successful.)
    }
    {$ENDIF}
    function  exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean; virtual;

    {$IFDEF PORTUGUES}
    {:
    Envia uma mensagem.
    @param(msgOut BYTES. Mensagem a enviar.)
    }
    {$ELSE}
    {:
    Sends a message.
    @param(msgOut BYTES. Message to send.)
    }
    {$ENDIF}
    procedure sendMessage(var msgOut:BYTES); virtual;

    {$IFDEF PORTUGUES}
    {:
    Pega um pacote que tenha chego na porta de comunicação.
    @param(msgIn BYTES. Armazena o pacote que chegou.)
    @param(BytesRead LongInt. Tamanho do pacote que chegou.)
    @returns(iorOK caso tenha chego algum pacote.)
    }
    {$ELSE}
    {:
    Gets a incoming packet from the communication port.
    @param(msgIn BYTES. Stores the incoming packet.)
    @param(BytesRead LongInt. Packet length.)
    @returns(iorOK if was come some packet.)
    }
    {$ENDIF}
    function  getResponse(var msgIn:BYTES; var BytesRead:LongInt):TIOResult; virtual;

    //: @exclude
    procedure listReachablePartners; virtual;
  protected
    {$IFDEF PORTUGUES}
    {:
    Inverte os bytes de uma Word.
    @param(W Word. Word a ser invertido os seus bytes.)
    @returns(A word com os bytes invertidos.)
    }
    {$ELSE}
    {:
    Swap the bytes of a word.
    @param(W Word. Word to swap their bytes.)
    @returns(The word with their bytes swaped.)
    }
    {$ENDIF}
    function  SwapBytesInWord(W:Word):Word;
    {$IFDEF PORTUGUES}
    {:
    Prepara uma mensagem para ser enviada.
    @param(msg BYTES. Mensagem a ser preparada)
    }
    {$ELSE}
    {:
    Prepares a message to be sent.
    @param(msg BYTES. Message to be prepared.)
    }
    {$ENDIF}
    procedure PrepareToSend(var msg:BYTES); virtual;
  protected
    {$IFDEF PORTUGUES}
    {:
    Adiciona um parametro na mensagem a ser enviada.
    @param(MsgOut BYTES. Mensagem a ser enviada.)
    @param(param BYTES. Parametro a ser adicionado.)
    }
    {$ELSE}
    {:
    Adds a parameter into the message to be sent.
    @param(MsgOut BYTES. Message to be sent.)
    @param(param BYTES. Parameter to be added.)
    }
    {$ENDIF}
    procedure AddParam(var MsgOut:BYTES; const param:BYTES); virtual;

    {$IFDEF PORTUGUES}
    {:
    Adiciona um conjunto de dados na mensagem a ser enviada.
    @param(MsgOut BYTES. Mensagem a ser enviada.)
    @param(data BYTES. Dados a serem adicionados.)
    }
    {$ELSE}
    {:
    Adds a dataset into the message to be sent.
    @param(MsgOut BYTES. Message to be sent.)
    @param(data BYTES. Data to be added.)
    }
    {$ENDIF}
    procedure AddData(var MsgOut:BYTES; const data:BYTES); virtual;

    {$IFDEF PORTUGUES}
    {:
    Inicializa a PDU em uma mensagem que se deseja enviar.
    @param(MsgOut BYTES. Mensagem a ser enviada onde se deve inicializar a PDU.)
    @param(PDUType LongInt. Tipo da PDU a criar.)
    }
    {$ELSE}
    {:
    Initialize the PDU on outgoing message.
    @param(MsgOut BYTES. Outgoing message to initiate the PDU.)
    @param(PDUType LongInt. Kind of PDU to create.)
    }
    {$ENDIF}
    procedure InitiatePDUHeader(var MsgOut:BYTES; PDUType:LongInt); virtual;

    {$IFDEF PORTUGUES}
    {:
    Negocia o tamanho máximo da PDU.
    @param(CPU TS7CPU. CLP a negociar o tamanho da PDU.)
    }
    {$ELSE}
    {:
    Negotiate the maximum PDU size.
    @param(CPU TS7CPU. PLC to negotiate the maximum PDU size.)
    }
    {$ENDIF}
    function  NegotiatePDUSize(var CPU:TS7CPU):Boolean; virtual;

    {$IFDEF PORTUGUES}
    {:
    Cria uma estrutura PDU a partir de uma Mensagem.
    @param(msg BYTES. Mensagem que se deseja obter a estrutura PDU.)
    @param(MsgOutgoing Boolean. Caso @true a mensagem vai ser enviada ao CLP, caso
           contrário a mensagem está chegando do CLP.)
    @param(PDU TPDU. Estrutura PDU retornada.)
    @returns(O código de erro presente na PDU, caso exista.)
    }
    {$ELSE}
    {:
    Creates a PDU structure from message.
    @param(msg BYTES. Menssage to get the PDU structure.)
    @param(MsgOutgoing Boolean. If @true the message will be sent to PLC, if not
           the message is comming from the PLC.)
    @param(PDU TPDU. The PDU structure extracted from message.)
    @returns(The error number of PDU, if exists.)
    }
    {$ENDIF}
    function  SetupPDU(var msg:BYTES; MsgOutgoing:Boolean; out PDU:TPDU):LongInt; virtual;

    {$IFDEF PORTUGUES}
    {:
    Prepara a mensagem para requisitar uma leitura de memórias do CLP.
    @param(msgOut BYTES. Mensagem a enviar ao CLP requisitando uma leitura.)
    }
    {$ELSE}
    {:
    Prepares the message to do a memory read request from PLC.
    @param(msgOut BYTES. Message to be sent to PLC requesting a memory read.)
    }
    {$ENDIF}
    procedure PrepareReadRequest(var msgOut:BYTES); virtual;

    {$IFDEF PORTUGUES}
    {:
    Prepara a mensagem para escrever nas memórias do CLP.
    @param(msgOut BYTES. Mensagem a enviar ao CLP requisitando uma leitura.)
    }
    {$ELSE}
    {:
    Prepares the message to write data into the PLC memory.
    @param(msgOut BYTES. Message to sent to write data into the PLC memory.)
    }
    {$ENDIF}
    procedure PrepareWriteRequest(var msgOut:BYTES); virtual;

    {$IFDEF PORTUGUES}
    {:
    Prepara a mensagem para ler ou escrever no CLP.
    @param(WriteRequest Boolean. Se true, a mensagem irá escrever dados na memória do CLP.)
    @param(msgOut BYTES. Mensagem a ser preparada para um comando de leitura/escrita.)
    }
    {$ELSE}
    {:
    Prepares the message to read or write on PLC.
    @param(WriteRequest Boolean. If @true, the message will write something in PLC memory.)
    @param(msgOut BYTES. Message to be prepared to request a read/write.)
    }
    {$ENDIF}
    procedure PrepareReadOrWriteRequest(const WriteRequest:Boolean; var msgOut:BYTES); virtual;

    {$IFDEF PORTUGUES}
    {:
    Adiciona as informações sobre o que deve ser lido do CLP.
    @param(msgOut BYTES. Mensagem a enviar ao CLP requisitando uma leitura.)
    @param(iArea LongInt. Área de memória desejada.
           @seealso(vtS7_200_SysInfo)
           @seealso(vtS7_200_SM)
           @seealso(vtS7_200_AnInput)
           @seealso(vtS7_200_AnOutput)
           @seealso(vtS7_200_Counter)
           @seealso(vtS7_200_Timer)
           @seealso(vtS7_Peripheral)
           @seealso(vtS7_Inputs)
           @seealso(vtS7_Outputs)
           @seealso(vtS7_Flags)
           @seealso(vtS7_DB)
           @seealso(vtS7_DI)
           @seealso(vtS7_Local)
           @seealso(vtS7_V)
           @seealso(vtS7_Counter)
           @seealso(vtS7_Timer)
    )
    @param(iDBnum LongInt. Caso iArea seja vtS7_DB, especifique aqui o número do DB.)
    @param(iStart LongInt. Endereço inicial da memória.)
    @param(iByteCount LongInt. Quantidade de bytes a ler.)
    }
    {$ELSE}
    {:
    Add into the outgoing message, informations about what must be read from PLC.
    @param(msgOut BYTES. Message to be sent to PLC requesting a memory read.)
    @param(iArea LongInt. Wanted memory area.
           @seealso(vtS7_200_SysInfo)
           @seealso(vtS7_200_SM)
           @seealso(vtS7_200_AnInput)
           @seealso(vtS7_200_AnOutput)
           @seealso(vtS7_200_Counter)
           @seealso(vtS7_200_Timer)
           @seealso(vtS7_Peripheral)
           @seealso(vtS7_Inputs)
           @seealso(vtS7_Outputs)
           @seealso(vtS7_Flags)
           @seealso(vtS7_DB)
           @seealso(vtS7_DI)
           @seealso(vtS7_Local)
           @seealso(vtS7_V)
           @seealso(vtS7_Counter)
           @seealso(vtS7_Timer)
    )
    @param(iDBnum LongInt. If iArea is vtS7_DB, what's the DB number.)
    @param(iStart LongInt. Start address of memory.)
    @param(iByteCount LongInt. How many bytes to read.)
    }
    {$ENDIF}
    procedure AddToReadRequest(var msgOut:BYTES; iArea, iDBnum, iStart, iByteCount:LongInt); virtual;

    {$IFDEF PORTUGUES}
    {:
    Adiciona as informações sobre onde e o que deve escrito na memória do CLP na mensagem a ser enviada.
    @param(msgOut BYTES. Mensagem a enviar com o comando de escrita de dados no CLP.)
    @param(iArea LongInt. Área de memória que se desejada escrever.
           @seealso(vtS7_200_SysInfo)
           @seealso(vtS7_200_SM)
           @seealso(vtS7_200_AnInput)
           @seealso(vtS7_200_AnOutput)
           @seealso(vtS7_200_Counter)
           @seealso(vtS7_200_Timer)
           @seealso(vtS7_Peripheral)
           @seealso(vtS7_Inputs)
           @seealso(vtS7_Outputs)
           @seealso(vtS7_Flags)
           @seealso(vtS7_DB)
           @seealso(vtS7_DI)
           @seealso(vtS7_Local)
           @seealso(vtS7_V)
           @seealso(vtS7_Counter)
           @seealso(vtS7_Timer)
    )
    @param(iDBnum LongInt. Caso iArea seja vtS7_DB, especifique aqui o número do DB.)
    @param(iStart LongInt. Endereço inicial da memória.)
    @param(buffer BYTES. Dados a serem escritos no CLP.)
    }
    {$ELSE}
    {:
    Add into the outgoing message, informations about the data to be written on PLC.
    @param(msgOut BYTES. Message to be sent to write data on PLC.)
    @param(iArea LongInt. Wanted memory area.
           @seealso(vtS7_200_SysInfo)
           @seealso(vtS7_200_SM)
           @seealso(vtS7_200_AnInput)
           @seealso(vtS7_200_AnOutput)
           @seealso(vtS7_200_Counter)
           @seealso(vtS7_200_Timer)
           @seealso(vtS7_Peripheral)
           @seealso(vtS7_Inputs)
           @seealso(vtS7_Outputs)
           @seealso(vtS7_Flags)
           @seealso(vtS7_DB)
           @seealso(vtS7_DI)
           @seealso(vtS7_Local)
           @seealso(vtS7_V)
           @seealso(vtS7_Counter)
           @seealso(vtS7_Timer)
    )
    @param(iDBnum LongInt. If iArea is vtS7_DB, what's the DB number.)
    @param(iStart LongInt. Start address of memory.)
    @param(buffer BYTES. Data to be written on PLC.)
    }
    {$ENDIF}
    procedure AddParamToWriteRequest(var msgOut:BYTES; iArea, iDBnum, iStart:LongInt; buffer:BYTES); virtual;

    {$IFDEF PORTUGUES}
    {:
    Adiciona os dados que devem ser escritos na memória do CLP na mensagem a ser enviada.
    @param(msgOut BYTES. Mensagem a enviar com o comando de escrita de dados no CLP.)
    @param(iArea LongInt. Área de memória que se desejada escrever.
           @seealso(vtS7_200_SysInfo)
           @seealso(vtS7_200_SM)
           @seealso(vtS7_200_AnInput)
           @seealso(vtS7_200_AnOutput)
           @seealso(vtS7_200_Counter)
           @seealso(vtS7_200_Timer)
           @seealso(vtS7_Peripheral)
           @seealso(vtS7_Inputs)
           @seealso(vtS7_Outputs)
           @seealso(vtS7_Flags)
           @seealso(vtS7_DB)
           @seealso(vtS7_DI)
           @seealso(vtS7_Local)
           @seealso(vtS7_V)
           @seealso(vtS7_Counter)
           @seealso(vtS7_Timer)
    )
    @param(iDBnum LongInt. Caso iArea seja vtS7_DB, especifique aqui o número do DB.)
    @param(iStart LongInt. Endereço inicial da memória.)
    @param(buffer BYTES. Dados a serem escritos no CLP.)
    }
    {$ELSE}
    {:
    Add into the outgoing message the data to be written on PLC.
    @param(msgOut BYTES. Message to be sent to write data on PLC.)
    @param(iArea LongInt. Wanted memory area.
           @seealso(vtS7_200_SysInfo)
           @seealso(vtS7_200_SM)
           @seealso(vtS7_200_AnInput)
           @seealso(vtS7_200_AnOutput)
           @seealso(vtS7_200_Counter)
           @seealso(vtS7_200_Timer)
           @seealso(vtS7_Peripheral)
           @seealso(vtS7_Inputs)
           @seealso(vtS7_Outputs)
           @seealso(vtS7_Flags)
           @seealso(vtS7_DB)
           @seealso(vtS7_DI)
           @seealso(vtS7_Local)
           @seealso(vtS7_V)
           @seealso(vtS7_Counter)
           @seealso(vtS7_Timer)
    )
    @param(iDBnum LongInt. If iArea is vtS7_DB, what's the DB number.)
    @param(iStart LongInt. Start address of memory.)
    @param(buffer BYTES. Data to be written on PLC.)
    }
    {$ENDIF}
    procedure AddDataToWriteRequest(var msgOut:BYTES; iArea, iDBnum, iStart:LongInt; buffer:BYTES); virtual;
  protected
    {$IFDEF PORTUGUES}
    //: Coloca o CLP em RUN, se possível. Não funciona ainda.
    {$ELSE}
    //: Put the PLC in RUN state, if possible. Don't work yet.
    {$ENDIF}
    procedure RunPLC(CPU:TS7CPU);

    {$IFDEF PORTUGUES}
    //: Coloca o CLP em STOP, se possível. Não funciona ainda.
    {$ELSE}
    //: Stops the PLC, if possible. Don't work yet.
    {$ENDIF}
    procedure StopPLC(CPU:TS7CPU);

    //: @exclude.
    procedure CopyRAMToROM(CPU:TS7CPU);
    //: @exclude.
    procedure CompressMemory(CPU:TS7CPU);

    {$IFDEF PORTUGUES}
    //: Converte um número de erro Siemens para um erro de protocolo.
    {$ELSE}
    //: Converts a Siemens error code to a protocol error code.
    {$ENDIF}
    function  S7ErrorCodeToProtocolErrorCode(code:Word):TProtocolIOResult;
  protected
    {$IFDEF PORTUGUES}
    {:
    Converte TArrayOfDouble para BYTES.
    @param(Values TArrayOfDouble. Array a ser convertida.)
    @param(Start LongInt. A partir de qual elemento deve ser iniciada a conversão.)
    @param(Len LongInt. Quantos elementos da array devem ser convertidos.)
    @returns(Array de BYTES convertida.)
    }
    {$ELSE}
    {:
    Converts TArrayOfDouble to BYTES.
    @param(Values TArrayOfDouble. Array to be converted.)
    @param(Start LongInt. First element of Values to be converted.)
    @param(Len LongInt. How many elements to convert from Start.)
    @returns(Converted Array of BYTES.)
    }
    {$ENDIF}
    function  DoublesToBytes(const Values:TArrayOfDouble; Start, Len:LongInt):BYTES;

    {$IFDEF PORTUGUES}
    {:
    Converte BYTES para TArrayOfDouble.
    @param(ByteSeq BYTES. Array de byte a ser convertida para double.)
    @param(Start LongInt. A partir de qual elemento deve ser iniciada a conversão.)
    @param(Len LongInt. Quantos elementos da array devem ser convertidos.)
    @returns(Array de TArrayOfDouble.)
    }
    {$ELSE}
    {:
    Converts BYTES to TArrayOfDouble.
    @param(ByteSeq BYTES. Array of byte to be converted to Double.)
    @param(Start LongInt. First element of ByteSeq to be converted.)
    @param(Len LongInt. How many elements to convert from Start.)
    @returns(Array of TArrayOfDouble.)
    }
    {$ENDIF}
    function  BytesToDoubles(const ByteSeq:BYTES; Start, Len:LongInt):TArrayOfDouble;

    {$IFDEF PORTUGUES}
    {:
    Cria um CLP na lista de CLP's endereçados.
    @param(iRack LongInt. Rack do CLP)
    @param(iSlot LongInt. Slot do CLP)
    @param(iStation LongInt. Endereço do CLP)
    @returns(O indice do CLP na lista.)

    }
    {$ELSE}
    {:
    Creates a  PLC in the addressed PLC's list.
    @param(iRack LongInt. PLC Rack.)
    @param(iSlot LongInt. PLC Slot.)
    @param(iStation LongInt. PLC Address.)
    @returns(The PLC index on PLC list.)
    }
    {$ENDIF}
    function  CreatePLC(iRack, iSlot, iStation:LongInt):LongInt; virtual;

    {$IFDEF PORTUGUES}
    {:
    Deleta um CLP da lista de CLP's endereçados.
    @param(PLCIndex LongInt. Posicao do CLP na lista)
    }
    {$ELSE}
    {:
    Delete a PLC from the addressed PLC's list.
    @param(PLCIndex LongInt. Index of PLC in the PLC's list.)
    }
    {$ENDIF}
    procedure DeletePLC(PLCIndex:Integer); virtual;

    {$IFDEF PORTUGUES}
    {:
    Atualiza os valores do gerenciador de memórias não continuas.
    @param(pkgin BYTES. Mensagem recebida ao CLP)
    @param(pkgout BYTES. Mensagem enviada do CLP)
    @param(writepkg Boolean. Se @true, o pacote enviado era para alterar os valores na memória do CLP)
    @param(ReqList TS7ReqList. Lista de todas as requisições da mensagem enviada)
    @param(ResultValues TArrayOfDouble. Valores da última requisição.)
    }
    {$ELSE}
    {:
    Updates the manager of non-continuous memory blocks.
    @param(pkgin BYTES. Message received from PLC)
    @param(pkgout BYTES. Message sent to PLC)
    @param(writepkg Boolean. If @true, the packet sent will change the PLC memory.)
    @param(ReqList TS7ReqList. List of all requests sent.)
    @param(ResultValues TArrayOfDouble. Values of the last request.)
    }
    {$ENDIF}
    procedure UpdateMemoryManager(pkgin, pkgout:BYTES; writepkg:Boolean; ReqList:TS7ReqList; var ResultValues:TArrayOfDouble);
    //: @seealso(TProtocolDriver.DoAddTag)
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    //: @seealso(TProtocolDriver.DoDelTag)
    procedure DoDelTag(TagObj:TTag); override;
    //: @seealso(TProtocolDriver.DoScanRead)
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    //: @seealso(TProtocolDriver.DoGetValue)
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;

    //estas funcoes ficaram apenas por motivos compatibilidade com os tags
    //e seus metodos de leitura e escrita diretas.

    //: @seealso(TProtocolDriver.DoWrite)
    function  DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    //: @seealso(TProtocolDriver.DoRead)
    function  DoRead(const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;

    //: @exclude
    destructor Destroy; override;

    //: @seealso(TProtocolDriver.SizeOfTag)
    function    SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;

    //: @seealso(TProtocolDriver.LiteralTagAddress)
    function LiteralTagAddress(aTag: TTag; aBlockTag: TTag=nil):AnsiString; override;

    //: @seealso(TProtocolDriver.OpenTagEditor)
    procedure OpenTagEditor(InsertHook: TAddTagInEditorHook;
      CreateProc: TCreateTagProc); override;
    //: @seealso(TProtocolDriver.HasTabBuilderEditor)
    function HasTabBuilderEditor: Boolean; override;
  published
    //: @seealso(TProtocolDriver.ReadSomethingAlways)
    property ReadSomethingAlways;
  end;

  procedure SetTagBuilderToolForSiemensS7ProtocolFamily(TagBuilderTool:TOpenTagEditor);

implementation

uses PLCTagNumber, PLCString, PLCStruct, hsstrings, PLCBlock, PLCMemoryManager,
  dateutils, strutils, math;

////////////////////////////////////////////////////////////////////////////////
// CONSTRUTORES E DESTRUTORES
// CONSTRUCTORS AND DESTRUCTORS
////////////////////////////////////////////////////////////////////////////////

function SortTagList(Item1, Item2: Pointer): Integer;
var
  ReqItem1:PS7ScanReqItem absolute Item1;
  ReqItem2:PS7ScanReqItem absolute Item2;
  BitCombination:Integer;
  ScanPercent1, ScanPercent2:Double;
begin
  BitCombination:=ifthen(ReqItem1^.NeedUpdate,1,0)+ifthen(ReqItem2^.NeedUpdate,2,0);
  case BitCombination of
    1:
      Result:=-1;
    2:
      Result:= 1;
    0,3: begin
      //if ReqItem1.LastUpdate=ReqItem2.LastUpdate then
      //  Result:=0
      //else begin
      //  if ReqItem1.LastUpdate<ReqItem2.LastUpdate then
      //    Result:=-1
      //  else
      //    Result:=1;
      //end;
      ScanPercent1:=(MilliSecondsBetween(Now,ReqItem1^.LastUpdate)/ReqItem1^.UpdateRate);
      ScanPercent2:=(MilliSecondsBetween(Now,ReqItem2^.LastUpdate)/ReqItem2^.UpdateRate);
      if ScanPercent1=ScanPercent2 then
        Result:=0
      else begin
        if ScanPercent1>ScanPercent2 then
          Result:=-1
        else
          Result:=1;
      end;
    end;
  end;
end;

function SortGenericTagList(const Item1, Item2: TS7ScanReqItem): Integer;
var
  BitCombination:Integer;
  ScanPercent1, ScanPercent2:Double;
begin
  BitCombination:=ifthen(Item1.NeedUpdate,1,0)+ifthen(Item2.NeedUpdate,2,0);
  case BitCombination of
    1:
      Result:=-1;
    2:
      Result:= 1;
    0,3: begin
      ScanPercent1:=0;
      if Item1.UpdateRate<>0 then
      ScanPercent1:=(MilliSecondsBetween(Now,Item1.LastUpdate)/Item1.UpdateRate);

      ScanPercent2:=0;
      if Item2.UpdateRate<>0 then
      ScanPercent2:=(MilliSecondsBetween(Now,Item2.LastUpdate)/Item2.UpdateRate);
      if ScanPercent1=ScanPercent2 then
        Result:=0
      else begin
        if ScanPercent1>ScanPercent2 then
          Result:=-1
        else
          Result:=1;
      end;
    end;
  end;
end;

constructor TSiemensProtocolFamily.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PReadSomethingAlways := true;
  FProtocolReady:=false;
  PDUIncoming:=0;
  PDUOutgoing:=0;
end;

destructor TSiemensProtocolFamily.Destroy;
var
  c: Integer;
begin
  inherited Destroy;
  for c:=0 to High(FPLCs) do
    DeletePLC(c);
end;

function  TSiemensProtocolFamily.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  //todos os tipos são retornados como byte
  //all kinds of tags are returned as byte
  ProtocolTagType:=ptByte;
  Result:=8;
end;

function TSiemensProtocolFamily.LiteralTagAddress(aTag: TTag; aBlockTag: TTag):AnsiString;

  function DescTagArea(area:LongInt):AnsiString;
  begin
    case area of
      1:
        Result:='I';
      2:
        Result:='Q';
      3:
        Result:='M';
      4:
        Result:='DB';
      5:
        Result:='C';
      6:
        Result:='T';
      7:
        Result:='SM';
      8:
        Result:='S7200 I';
      9:
        Result:='S7200 Q';
      10:
        Result:='S7200 C';
      11:
        Result:='S7200 T';
      12:
        Result:='PI';
      else
        Result:='(Unknown area)';
    end;
  end;

  function SiemensDescOfType(atype:TTagType):AnsiString;
  begin
    case atype of
      pttDefault, pttShortInt, pttByte:
        Result:='B';
      pttSmallInt, pttWord:
        Result:='W';
      pttLongInt, pttDWord, pttFloat:
        Result:='D';
    end;
  end;

  function PascalDescOfType(atype:TTagType):AnsiString;
  begin
    case atype of
      pttDefault, pttByte:
        Result:=' AS Byte [0..255]';
      pttShortInt:
        Result:=' AS ShortInt [-128..127]';
      pttSmallInt:
        Result:=' AS SmallInt [-32768..32767]';
      pttWord:
        Result:=' AS Word [0..65535]';
      pttLongInt:
        Result:=' AS LongInt [-2147483648..2147483647]';
      pttDWord:
        Result:=' AS SmallInt [0..4294967295]';
      pttFloat:
        Result:=' AS Float [±1.18×10E−38 to ±3.4×10E38]';
    end;
  end;

begin
  if aBlockTag=nil then begin
    if aTag is TPLCTagNumber then begin
      with atag as TPLCTagNumber do begin

        case MemReadFunction of
          4:
            Result:='DB'+IntToStr(MemFile_DB)+'.DB'+ SiemensDescOfType(TagType)+
                    IntToStr(MemAddress)+PascalDescOfType(TagType);
          1..3, 7:
            Result:=DescTagArea(MemReadFunction)+SiemensDescOfType(TagType)+
                    IntToStr(MemAddress)+PascalDescOfType(TagType);

          5..6, 8..12: begin
            Result:=DescTagArea(MemReadFunction)+
                    IntToStr(MemAddress)+PascalDescOfType(TagType);
            if (TagType<>pttWord) and (TagType<>pttSmallInt) then
              Result:=Result+LineEnding+'(wrong Tag data type, set the Tag data type to pttWord or pttSmallInt)';
          end;
          else
            Result:='Unknown area';
        end;
      end;
      exit;
    end;

    if aTag is TPLCString then begin
      with atag as TPLCString do begin
        Result:='String format '+ strutils.ifthen(StringType=stC,'"C" (null terminated)','"Siemens" (with one byte to max size and other with the current string size)') +
          ','+LineEnding+' starting at ';

        case MemReadFunction of
          4:
            Result:=Result+'DB'+IntToStr(MemFile_DB)+'.DBB'+IntToStr(MemAddress);
          1..3, 7: begin
            Result:=Result+DescTagArea(MemReadFunction)+'B'+
                    IntToStr(MemAddress);
            if MemReadFunction in [1..2] then
              Result:=Result+LineEnding+'(Digital inputs/outputs are a strange area to store a string, not?)';
          end;

          5..6, 8..12: begin
            Result:=DescTagArea(MemReadFunction)+IntToStr(MemAddress)+
            LineEnding+'('+DescTagArea(MemReadFunction)+' are a strange area to store a string, not?)';
          end;
          else
            Result:='Unknown area';
        end;
      end;
      exit;
    end;

    if aTag is TPLCStruct then begin
      Result:='Structure at ';
      with atag as TPLCStruct do begin

        case MemReadFunction of
          4:
            Result:=Result+'DB'+IntToStr(MemFile_DB)+'.DB'+ SiemensDescOfType(TagType)+
                    IntToStr(MemAddress);
          1..3, 7:
            Result:=Result+DescTagArea(MemReadFunction)+SiemensDescOfType(TagType)+
                    IntToStr(MemAddress);

          5..6, 8..12: begin
            Result:=Result+DescTagArea(MemReadFunction)+
                    IntToStr(MemAddress);
          end;
          else
            Result:=Result+'Unknown area';
        end;
        Result:=Result+LineEnding+'with '+IntToStr(TagSizeOnProtocol);
      end;
      exit;
    end;

    if aTag is TPLCBlock then begin //block and plcstruct
      Result:='Block of tags starting at ';
      with atag as TPLCBlock do begin

        case MemReadFunction of
          4:
            Result:=Result+'DB'+IntToStr(MemFile_DB)+'.DB'+ SiemensDescOfType(TagType)+
                    IntToStr(MemAddress);
          1..3, 7:
            Result:=Result+DescTagArea(MemReadFunction)+SiemensDescOfType(TagType)+
                    IntToStr(MemAddress);

          5..6, 8..12: begin
            Result:=Result+DescTagArea(MemReadFunction)+
                    IntToStr(MemAddress);
            if (TagType<>pttWord) and (TagType<>pttSmallInt) then
              Result:=Result+LineEnding+'(with a strange Tag data type, set the data type to pttWord or pttSmallInt)';
          end;
          else
            Result:=Result+'Unknown area';
        end;
        Result:=Result+LineEnding+'with '+IntToStr(Size)+' elements'+PascalDescOfType(TagType);
      end;
      exit;
    end;
  end else begin

  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Funcoes da interface
// Interface functions.
////////////////////////////////////////////////////////////////////////////////

function  TSiemensProtocolFamily.initAdapter:Boolean;
begin
  Result := true;
end;

function  TSiemensProtocolFamily.disconnectAdapter:Boolean;
begin
  Result:=false;
end;

function  TSiemensProtocolFamily.connectPLC(var CPU:TS7CPU):Boolean;
begin
  Result:=false;
end;

function  TSiemensProtocolFamily.disconnectPLC(var CPU:TS7CPU):Boolean;
begin
  Result:=false;
end;

function TSiemensProtocolFamily.exchange(var CPU:TS7CPU; var msgOut:BYTES; var msgIn:BYTES; IsWrite:Boolean):Boolean;
var
  pduo:TPDU;
  res:LongInt;
begin
  res := SetupPDU(msgOut, true, pduo);
  if res<>0 then  begin
    Result:=False;
    exit;
  end;

  if CPU.PDUId=$FFFF then
    CPU.PDUId:=0
  else
    inc(CPU.PDUId);

  PPDUHeader(pduo.header)^.number:=SwapBytesInWord(CPU.PDUId);
  Result := false;
end;

procedure TSiemensProtocolFamily.sendMessage(var msgOut:BYTES);
begin

end;

function  TSiemensProtocolFamily.getResponse(var msgIn:BYTES; var BytesRead:LongInt):TIOResult;
begin
  Result:=iorNone;
end;

function  TSiemensProtocolFamily.SwapBytesInWord(W:Word):Word;
var
  bl, bh:Byte;
begin
  bl := W mod $100;
  bh := W div $100;
  Result:=(bl*$100)+bh;
end;

procedure TSiemensProtocolFamily.PrepareToSend(var msg:BYTES);
begin

end;

function  TSiemensProtocolFamily.NegotiatePDUSize(var CPU:TS7CPU):Boolean;
var
  param, Msg, msgIn:BYTES;
  pdu:TPDU;
  res:LongInt;
  db:LongInt;
begin
  Result := false;
  SetLength(param,8);
  SetLength(msg, PDUOutgoing+10+8);
  SetLength(msgIn, 0);

  param[0] := $F0;
  param[1] := 0;
  param[2] := 0;
  param[3] := 1;
  param[4] := 0;
  param[5] := 1;
  param[6] := 3;
  param[7] := $C0;

  InitiatePDUHeader(msg,1);
  AddParam(Msg,param);
  if exchange(CPU,Msg,msgIn,false) then begin
    res := SetupPDU(msgIn, false, pdu);
    if res=0 then begin
      CPU.MaxPDULen:=GetByte(pdu.param,6)*256+GetByte(pdu.param,7);
      CPU.MaxBlockSize:=CPU.MaxPDULen-18; //10 bytes of header + 2 bytes of error code + 2 bytes of read request + 4 bytes of informations about the request.
      //ajusta o tamanho máximo dos blocos;
      //adjust the maximum block size.
      with CPU do begin
        Inputs.MaxBlockItems:=MaxBlockSize;
        Outputs.MaxBlockItems:=MaxBlockSize;
        Timers.MaxBlockItems:=MaxBlockSize;
        Counters.MaxBlockItems:=MaxBlockSize;
        Flags.MaxBlockItems:=MaxBlockSize;
        PeripheralInputs.MaxBlockItems:=MaxBlockSize;

        S7200SMs.MaxBlockItems:=MaxBlockSize;
        S7200Timers.MaxBlockItems:=MaxBlockSize;
        S7200Counters.MaxBlockItems:=MaxBlockSize;
        S7200AnInput.MaxBlockItems:=MaxBlockSize;
        S7200AnOutput.MaxBlockItems:=MaxBlockSize;

        for db:=0 to High(DBs) do
          DBs[db].DBArea.MaxBlockItems:=MaxBlockSize;
      end;
      Result := true;
    end;
  end;
end;

function  TSiemensProtocolFamily.SetupPDU(var msg:BYTES; MsgOutgoing:Boolean; out PDU:TPDU):LongInt;
var
  position:LongInt;
begin
  if MsgOutgoing then
    position:=PDUOutgoing
  else
    position:=PDUIncoming;

  Result := 0;

  PDU.header:=@msg[position];
  PDU.header_len:=10;
  if PPDUHeader(PDU.header)^.PDUHeadertype in [2,3] then begin
    PDU.header_len:=12;
    Result:=SwapBytesInWord(PPDUHeader(PDU.header)^.Error);
  end;

  PDU.param:=@msg[position+PDU.header_len];
  PDU.param_len:=SwapBytesInWord(PPDUHeader(PDU.header)^.param_len);

  if High(msg)>=(position + PDU.header_len + PDU.param_len) then begin
    PDU.data:=@msg[position + PDU.header_len + PDU.param_len];
    PDU.data_len:=SwapBytesInWord(PPDUHeader(PDU.header)^.data_len);
  end else begin
    PDU.data:=nil;
    PDU.data_len:=0;
  end;
  PDU.user_data_len:=0;
  PDU.udata:=nil
end;

procedure TSiemensProtocolFamily.PrepareReadRequest(var msgOut:BYTES);
begin
  PrepareReadOrWriteRequest(false, msgOut);
end;

procedure TSiemensProtocolFamily.PrepareWriteRequest(var msgOut:BYTES);
begin
  PrepareReadOrWriteRequest(True, msgOut);
end;

procedure TSiemensProtocolFamily.PrepareReadOrWriteRequest(const WriteRequest:Boolean; var msgOut:BYTES);
var
  param:BYTES;
begin
  SetLength(param, 2);

  param[0] :=  ifthen(WriteRequest, S7FuncWrite, S7FuncRead);
  param[1] := 0;
  InitiatePDUHeader(msgOut,1);
  AddParam(msgOut, param);

  SetLength(param,0);
end;

procedure TSiemensProtocolFamily.AddToReadRequest(var msgOut:BYTES; iArea, iDBnum, iStart, iByteCount:LongInt);
var
  param:BYTES;
  p:PS7Req;
  PDU:TPDU;
  NumReq:Byte;
  intArray:array[0..3] of byte;
  intStart:LongInt absolute intArray;
begin
  SetLength(param, 12);
  param[00] := $12;
  param[01] := $0a;
  param[02] := $10;
  param[03] := $02; //1=single bit, 2=byte, 4=word
  param[04] := $00; //size of request
  param[05] := $00; //size of request
  param[06] := $00; //DB Number
  param[07] := $00; //DB Number
  param[08] := $00; //area code;
  param[09] := $00; //start address in bits
  param[10] := $00; //start address in bits
  param[11] := $00; //start address in bits

  p := PS7Req(@param[00]);

  with p^ do begin
    case iArea of
      vtS7_200_AnInput, vtS7_200_AnOutput:
        WordLen:=4;

      vtS7_Counter,
      vtS7_Timer,
      vtS7_200_Counter,
      vtS7_200_Timer:
        WordLen:=iArea;
      else
        intStart:=iStart*8;
    end;

    ReqLength   :=SwapBytesInWord(iByteCount);
    DBNumber    :=SwapBytesInWord(iDBnum);
    AreaCode    :=iArea;
    HiBytes     :=0;
    //StartAddress:=SwapBytesInWord(iStart);
    param[09] := intArray[2];
    param[10] := intArray[1];
    param[11] := intArray[0];
  end;

  AddParam(msgOut, param);

  SetupPDU(msgOut, true, PDU);
  NumReq:=GetByte(PDU.param,1);
  NumReq:=NumReq+1;
  SetByte(PDU.param,1,NumReq);

  SetLength(param, 0);
end;

//executa somente uma escrita por vez!!!
//executes only one write per request.
procedure TSiemensProtocolFamily.AddParamToWriteRequest(var msgOut:BYTES; iArea, iDBnum, iStart:LongInt; buffer:BYTES);
var
  param:BYTES;
  bufferLen:LongInt;
  p:PS7Req;
  PDU:TPDU;
  NumReq:Byte;
  intArray:array[0..3] of byte;
  intStart:LongInt absolute intArray;
begin
  bufferLen:=Length(buffer);

  SetLength(param, 12);
  param[00] := $12;
  param[01] := $0a;
  param[02] := $10;
  param[03] := $02; //1=single bit, 2=byte, 4=word
  param[04] := $00; //size of request
  param[05] := $00; //size of request
  param[06] := $00; //DB Number
  param[07] := $00; //DB Number
  param[08] := $00; //area code;
  param[09] := $00; //start address in bits
  param[10] := $00; //start address in bits
  param[11] := $00; //start address in bits

  p := PS7Req(@param[00]);

  with p^ do begin
    case iArea of
      vtS7_200_AnInput, vtS7_200_AnOutput:
        begin
          WordLen:=4;
          ReqLength := SwapBytesInWord((bufferLen+1) div 2);
        end;
      vtS7_Counter,
      vtS7_Timer,
      vtS7_200_Counter,
      vtS7_200_Timer:
        begin
          WordLen:=iArea;
          ReqLength := SwapBytesInWord((bufferLen+1) div 2);
        end;
      else
        begin
          intStart:=iStart*8;
          ReqLength := SwapBytesInWord(bufferLen);
        end;
    end;

    DBNumber    :=SwapBytesInWord(iDBnum);
    AreaCode    :=iArea;
    HiBytes     :=0;
    //StartAddress:=SwapBytesInWord(iStart);
    param[09] := intArray[2];
    param[10] := intArray[1];
    param[11] := intArray[0];
  end;

  AddParam(msgOut, param);

  SetupPDU(msgOut, true, PDU);
  NumReq:=GetByte(PDU.param,1);
  NumReq:=NumReq+1;
  SetByte(PDU.param,1,NumReq);

  SetLength(param, 0);
end;

procedure TSiemensProtocolFamily.AddDataToWriteRequest(var msgOut:BYTES; iArea, iDBnum, iStart:LongInt; buffer:BYTES);
var
  da:BYTES;
  extra:LongInt;
  bufferlen:LongInt;
  lastdatabyte:LongInt;
begin
  bufferlen:=Length(buffer);

  extra := (bufferlen mod 2);

  SetLength(da,4+bufferlen+extra);
  da[00] := $00;
  da[01] := $04; //04 bits,
  da[02] := (bufferlen*8) div 256;
  da[03] := (bufferlen*8) mod 256;
  Move(buffer[0],da[4],Length(buffer));

  if extra=1 then begin
    lastdatabyte:=High(da);
    da[lastdatabyte]:=$80;
  end;

  AddData(msgOut, da);
end;

procedure TSiemensProtocolFamily.AddParam(var MsgOut:BYTES; const param:BYTES);
var
  pdu:TPDU;
  paramlen, extra, newparamlen:LongInt;
begin
  SetupPDU(MsgOut, true, pdu);
  paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);
  newparamlen := Length(param);

  extra := ifthen(PPDUHeader(pdu.header)^.PDUHeadertype in [2,3], 2, 0);

  if Length(MsgOut)<(PDUOutgoing+10+extra+paramlen+newparamlen) then begin
    SetLength(MsgOut,(PDUOutgoing+10+extra+paramlen+newparamlen));
    SetupPDU(MsgOut, true, pdu);
    paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);
  end;

  SetBytes(pdu.param, paramlen, param);
  PPDUHeader(pdu.header)^.param_len:=SwapBytesInWord(paramlen + Length(param));
end;

procedure TSiemensProtocolFamily.AddData(var MsgOut:BYTES; const data:BYTES);
var
  pdu:TPDU;
  paramlen, datalen, extra, newdatalen:LongInt;
begin
  SetupPDU(MsgOut, true, pdu);
  paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);
  datalen  := SwapBytesInWord(PPDUHeader(pdu.header)^.data_len);
  newdatalen := Length(data);

  extra := ifthen(PPDUHeader(pdu.header)^.PDUHeadertype in [2,3], 2, 0);

  if Length(MsgOut)<(PDUOutgoing+10+extra+paramlen+datalen+newdatalen) then begin
    SetLength(MsgOut,(PDUOutgoing+10+extra+paramlen+datalen+newdatalen));
    SetupPDU(MsgOut, true, pdu);
    paramlen := SwapBytesInWord(PPDUHeader(pdu.header)^.param_len);
    datalen  := SwapBytesInWord(PPDUHeader(pdu.header)^.data_len);
  end;

  SetBytes(pdu.data, datalen, data);
  PPDUHeader(pdu.header)^.data_len:=SwapBytesInWord(datalen + Length(data));
end;

procedure TSiemensProtocolFamily.InitiatePDUHeader(var MsgOut:BYTES; PDUType:LongInt);
var
  pduh:PPDUHeader;
  extra:LongInt;
begin
  extra := ifthen(PDUType in [2,3], 2, 0);

  if Length(MsgOut)<(PDUOutgoing+10+extra) then
    SetLength(MsgOut,(PDUOutgoing+10+extra));

  pduh:=@MsgOut[PDUOutgoing];
  with pduh^ do begin
    P:=$32;
    PDUHeadertype:=PDUType;
    a:=0;
    b:=0;
    number:=0;
    param_len:=0;
    data_len:=0;
    //evita escrever se não foi alocado.
    // avoid write if not allocated.
    if extra=2 then begin
      Error:=0;
    end;
  end;
end;

procedure TSiemensProtocolFamily.listReachablePartners;
begin

end;

////////////////////////////////////////////////////////////////////////////////
// FUNCOES DE MANIPULAÇAO DO DRIVER
// FUNCTIONS OF DRIVER HANDLING.
////////////////////////////////////////////////////////////////////////////////

function  TSiemensProtocolFamily.DoublesToBytes(const Values:TArrayOfDouble; Start, Len:LongInt):BYTES;
var
  arraylen,
  c:LongInt;
begin
  arraylen:=Length(Values);
  if (start+(Len-1))>=arraylen then
    raise Exception.Create(SoutOfBounds);

  SetLength(Result,Len);
  for c:=0 to Len-1 do begin
    Result[c]:=trunc(Values[c+Start]) and $FF;
  end;
end;

function  TSiemensProtocolFamily.BytesToDoubles(const ByteSeq:BYTES; Start, Len:LongInt):TArrayOfDouble;
var
  arraylen,
  c:LongInt;
begin
  arraylen:=Length(ByteSeq);
  if (start+(Len-1))>=arraylen then
    raise Exception.Create(SoutOfBounds);

  SetLength(Result,Len);
  for c:=0 to Len-1 do begin
    Result[c]:=ByteSeq[c+Start];
  end;
end;

function TSiemensProtocolFamily.CreatePLC(iRack, iSlot, iStation: LongInt
  ): LongInt;
begin
  Result:=Length(FPLCs);
  SetLength(FPLCs,Result+1);
  FPLCs[Result].MaxBlockSize:=0; //must be 0 instead of -1 to avoid fragmentation
  FPLCs[Result].MaxPDULen:=0;
  FPLCs[Result].Connected:=false;
  FPLCs[Result].Slot:=iSlot;
  FPLCs[Result].Rack:=iRack;
  FPLCs[Result].Station:=iStation;

  FPLCs[Result].Inputs           :=TPLCMemoryManager.Create;
  FPLCs[Result].Outputs          :=TPLCMemoryManager.Create;
  FPLCs[Result].PeripheralInputs :=TPLCMemoryManager.Create;
  FPLCs[Result].Timers           :=TPLCMemoryManager.Create;
  FPLCs[Result].Counters         :=TPLCMemoryManager.Create;
  FPLCs[Result].Flags            :=TPLCMemoryManager.Create;

  FPLCs[Result].S7200SMs         :=TPLCMemoryManager.Create;
  FPLCs[Result].S7200Timers      :=TPLCMemoryManager.Create;
  FPLCs[Result].S7200Counters    :=TPLCMemoryManager.Create;
  FPLCs[Result].S7200AnInput     :=TPLCMemoryManager.Create;
  FPLCs[Result].S7200AnOutput    :=TPLCMemoryManager.Create;

  FPLCs[Result].Inputs.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
  FPLCs[Result].Outputs.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
  FPLCs[Result].PeripheralInputs.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
  FPLCs[Result].Timers.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
  FPLCs[Result].Counters.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
  FPLCs[Result].Flags.MaxBlockItems:=FPLCs[Result].MaxBlockSize;

  FPLCs[Result].S7200SMs.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
  FPLCs[Result].S7200Timers.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
  FPLCs[Result].S7200Counters.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
  FPLCs[Result].S7200AnInput.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
  FPLCs[Result].S7200AnOutput.MaxBlockItems:=FPLCs[Result].MaxBlockSize;
end;

procedure TSiemensProtocolFamily.DeletePLC(PLCIndex: Integer);
var
  hPLC: Integer;
  db: Integer;
begin
  hPLC:=High(FPLCs);

  if (PLCIndex>=0) AND (PLCIndex<=hPLC) then begin
    FPLCs[PLCIndex].Inputs.Destroy;
    FPLCs[PLCIndex].Outputs.Destroy;
    FPLCs[PLCIndex].PeripheralInputs.Destroy;

    for db:=0 to High(FPLCs[PLCIndex].DBs) do begin
      FPLCs[PLCIndex].DBs[db].DBArea.Destroy;
    end;
    SetLength(FPLCs[PLCIndex].DBs, 0);

    FPLCs[PLCIndex].Timers.Destroy;
    FPLCs[PLCIndex].Counters.Destroy;
    FPLCs[PLCIndex].Flags.Destroy;

    FPLCs[PLCIndex].S7200SMs.Destroy;
    FPLCs[PLCIndex].S7200Timers.Destroy;
    FPLCs[PLCIndex].S7200Counters.Destroy;
    FPLCs[PLCIndex].S7200AnInput.Destroy;
    FPLCs[PLCIndex].S7200AnOutput.Destroy;

    FPLCs[PLCIndex]:=FPLCs[hPLC];
    SetLength(FPLCs, hPLC);
  end;
end;

procedure TSiemensProtocolFamily.UpdateMemoryManager(pkgin, pkgout:BYTES; writepkg:Boolean; ReqList:TS7ReqList; var ResultValues:TArrayOfDouble);
var
  PDU:TPDU;
  NumResults,
  CurResult,
  DataLen,
  DataIdx,
  ResultLen,
  ResultCode,
  CurValue:LongInt;
  ProtocolErrorCode:TProtocolIOResult;
begin
  if writepkg then begin
    SetupPDU(pkgout, true, PDU);
    if GetByte(PDU.param,0)<>S7FuncWrite then exit;
  end else begin
    SetupPDU(pkgin, false, PDU);
    if GetByte(PDU.param,0)<>S7FuncRead then exit;
  end;
  NumResults:=GetByte(PDU.param, 1);
  CurResult:=0;
  DataIdx:=0;
  DataLen:=PDU.data_len;
  while CurResult<NumResults do begin
    ResultCode:=GetByte(PDU.data, DataIdx);

    if writepkg and (ResultCode=0) then
      ProtocolErrorCode:=ioOk
    else
      ProtocolErrorCode:=S7ErrorCodeToProtocolErrorCode(ResultCode);

    if (writepkg or (ResultCode=$FF)) AND (DataLen>4) then begin
      ResultLen:=GetByte(PDU.data, DataIdx+2)*$100 + GetByte(PDU.data, DataIdx+3);
      //o tamanho está em bits, precisa de ajuste.
      //if the size is in bits, adjust to bytes
      if GetByte(PDU.data, DataIdx+1)=4 then
        ResultLen:=ResultLen div 8
      else begin
        //3 o restultado já está em bytes
        //e 9 o resultado está em bits, mas cada bit em um byte.
        //if 3, the result already is in bytes
        //if 9, the result is in bits, but each byte stores one bit
        if not (GetByte(PDU.data, DataIdx+1) in [3,9]) then
          exit;
      end;
    end else begin
      if ResultCode=$FF then
        ProtocolErrorCode:=ioEmptyPacket;
      ResultLen:=0;
    end;

    //move os dados recebidos para as respectivas areas.
    //move the received data to their area.
    SetLength(ResultValues,0);
    if ResultLen>0 then begin
      SetLength(ResultValues,ResultLen);
      CurValue:=0;
      while (CurValue<ResultLen) AND (CurValue<Length(ResultValues)) do begin
        ResultValues[CurValue]:=GetByte(PDU.data, DataIdx+4+CurValue);
        inc(CurValue);
      end;

      FProtocolReady:=true;

      with ReqList[CurResult] do begin
        if (PLCIdx>=0) and (PLCIdx<=High(FPLCs)) then
          case ReqType of
            vtS7_DB:
              if (DBIdx>=0) AND (DBIdx<=High(FPLCs[PLCIdx].DBs)) then
                FPLCs[PLCIdx].DBs[DBIdx].DBArea.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Inputs:
               FPLCs[PLCIdx].Inputs.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Outputs:
               FPLCs[PLCIdx].Outputs.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_AnInput:
               FPLCs[PLCIdx].S7200AnInput.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_AnOutput:
               FPLCs[PLCIdx].S7200AnOutput.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Timer:
               FPLCs[PLCIdx].Timers.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Counter:
               FPLCs[PLCIdx].Counters.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Flags:
               FPLCs[PLCIdx].Flags.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_SM:
               FPLCs[PLCIdx].S7200SMs.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_Timer:
               FPLCs[PLCIdx].S7200Timers.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_200_Counter:
               FPLCs[PLCIdx].S7200Counters.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
            vtS7_Peripheral:
               FPLCs[PLCIdx].PeripheralInputs.SetValues(StartAddress,ResultLen,1,ResultValues, ProtocolErrorCode);
          end;
      end;
    end else begin
      //seta a falha...
      //sets the fault.
      with ReqList[CurResult] do begin
        if (PLCIdx>=0) and (PLCIdx<=High(FPLCs)) then
          case ReqType of
            vtS7_DB:
              if (DBIdx>=0) AND (DBIdx<=High(FPLCs[PLCIdx].DBs)) then
                FPLCs[PLCIdx].DBs[DBIdx].DBArea.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Inputs:
               FPLCs[PLCIdx].Inputs.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Outputs:
               FPLCs[PLCIdx].Outputs.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_AnInput:
               FPLCs[PLCIdx].S7200AnInput.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_AnOutput:
               FPLCs[PLCIdx].S7200AnOutput.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Timer:
               FPLCs[PLCIdx].Timers.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Counter:
               FPLCs[PLCIdx].Counters.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Flags:
               FPLCs[PLCIdx].Flags.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_SM:
               FPLCs[PLCIdx].S7200SMs.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_Timer:
               FPLCs[PLCIdx].S7200Timers.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_200_Counter:
               FPLCs[PLCIdx].S7200Counters.SetFault(StartAddress,Size,1,ProtocolErrorCode);
            vtS7_Peripheral:
               FPLCs[PLCIdx].PeripheralInputs.SetFault(StartAddress,Size,1,ProtocolErrorCode);
          end;
      end;
    end;

    DataIdx:=DataIdx+ResultLen+4;
    dec(DataLen,ResultLen);

    //pelo que entendi, um resultado nunca vem com tamanho impar
    //no pacote.
    //the size of result never is a odd number
    if (ResultLen mod 2)=1 then begin
      inc(DataIdx);
      dec(DataLen);
    end;

    //proximo resultado.
    //goto the next result.
    inc(CurResult);
  end;
end;

procedure TSiemensProtocolFamily.DoAddTag(TagObj:TTag; TagValid:Boolean);
var
  plc, db:LongInt;
  tr:TTagRec;
  foundplc, founddb, valido:Boolean;
begin
  tr:=GetTagInfo(TagObj);
  foundplc:=false;

  valido:=true;

  for plc := 0 to High(FPLCs) do
    if (FPLCs[plc].Slot=Tr.Slot) AND (FPLCs[plc].Rack=Tr.Rack) AND (FPLCs[plc].Station=Tr.Station) then begin
      foundplc:=true;
      break;
    end;

  if not foundplc then begin
    plc:=CreatePLC(tr.Rack, tr.Slot, tr.Station);
  end;

  case tr.ReadFunction of
    1:
      FPLCs[plc].Inputs.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    2:
      FPLCs[plc].Outputs.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    3:
      FPLCs[plc].Flags.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    4: begin
      if tr.File_DB<=0 then
        tr.File_DB:=1;

      founddb:=false;
      for db:=0 to high(FPLCs[plc].DBs) do
        if FPLCs[plc].DBs[db].DBNum=tr.File_DB then begin
          founddb:=true;
          break;
        end;

      if not founddb then begin
        db:=Length(FPLCs[plc].DBs);
        SetLength(FPLCs[plc].DBs, db+1);
        FPLCs[plc].DBs[db].DBNum:=tr.File_DB;
        FPLCs[plc].DBs[db].DBArea:=TPLCMemoryManager.Create;
        FPLCs[plc].DBs[db].DBArea.MaxBlockItems:=FPLCs[plc].MaxBlockSize;
      end;

      FPLCs[plc].DBs[db].DBArea.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    end;
    5:
      FPLCs[plc].Counters.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    6:
      FPLCs[plc].Timers.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    7:
      FPLCs[plc].S7200SMs.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    8:
      FPLCs[plc].S7200AnInput.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    9:
      FPLCs[plc].S7200AnOutput.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    10:
      FPLCs[plc].S7200Counters.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    11:
      FPLCs[plc].S7200Timers.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    12:
      FPLCs[plc].PeripheralInputs.AddAddress(tr.Address,tr.Size,1,tr.UpdateTime);
    else
      valido:=false;
  end;

  Inherited DoAddTag(TagObj, valido);
end;

procedure TSiemensProtocolFamily.DoDelTag(TagObj:TTag);
var
  plc, db:LongInt;
  tr:TTagRec;
  foundplc, founddb:Boolean;
  TotalSize: Integer;
  hDB: Integer;
begin
  try
    tr:=GetTagInfo(TagObj);
    foundplc:=false;

    for plc := 0 to High(FPLCs) do
      if (FPLCs[plc].Slot=Tr.Slot) AND (FPLCs[plc].Rack=Tr.Rack) AND (FPLCs[plc].Station=Tr.Station) then begin
        foundplc:=true;
        break;
      end;

    if not foundplc then exit;

    case tr.ReadFunction of
      1: begin
        FPLCs[plc].Inputs.RemoveAddress(tr.Address,tr.Size,1);
      end;
      2:
        FPLCs[plc].Outputs.RemoveAddress(tr.Address,tr.Size,1);
      3:
        FPLCs[plc].Flags.RemoveAddress(tr.Address,tr.Size,1);
      4: begin
        if tr.File_DB<=0 then
          tr.File_DB:=1;

        founddb:=false;
        for db:=0 to high(FPLCs[plc].DBs) do
          if FPLCs[plc].DBs[db].DBNum=tr.File_DB then begin
            founddb:=true;
            break;
          end;

        if not founddb then exit;

        FPLCs[plc].DBs[db].DBArea.RemoveAddress(tr.Address,tr.Size,1);

        //delete the DB if their size = 0
        if FPLCs[plc].DBs[db].DBArea.Size=0 then begin
          hDB := High(FPLCs[plc].DBs);
          FreeAndNil(FPLCs[plc].DBs[db].DBArea);
          FPLCs[plc].DBs[db]:=FPLCs[plc].DBs[hDB];
          SetLength(FPLCs[plc].DBs,hDB);
        end;
      end;
      5:
        FPLCs[plc].Counters.RemoveAddress(tr.Address,tr.Size,1);
      6:
        FPLCs[plc].Timers.RemoveAddress(tr.Address,tr.Size,1);
      7:
        FPLCs[plc].S7200SMs.RemoveAddress(tr.Address,tr.Size,1);
      8:
        FPLCs[plc].S7200AnInput.RemoveAddress(tr.Address,tr.Size,1);
      9:
        FPLCs[plc].S7200AnOutput.RemoveAddress(tr.Address,tr.Size,1);
      10:
        FPLCs[plc].S7200Counters.RemoveAddress(tr.Address,tr.Size,1);
      11:
        FPLCs[plc].S7200Timers.RemoveAddress(tr.Address,tr.Size,1);
      12:
        FPLCs[plc].PeripheralInputs.RemoveAddress(tr.Address,tr.Size,1);
    end;

    //check if current plc has something to be read...
    TotalSize:=0;
    inc(TotalSize, FPLCs[plc].Inputs.Size);
    inc(TotalSize, FPLCs[plc].Outputs.Size);
    inc(TotalSize, FPLCs[plc].PeripheralInputs.Size);

    for db:=0 to high(FPLCs[plc].DBs) do
      inc(TotalSize, FPLCs[plc].DBs[db].DBArea.Size);

    inc(TotalSize, FPLCs[plc].Timers.Size);
    inc(TotalSize, FPLCs[plc].Counters.Size);
    inc(TotalSize, FPLCs[plc].Flags.Size);

    inc(TotalSize, FPLCs[plc].S7200SMs.Size);
    inc(TotalSize, FPLCs[plc].S7200Timers.Size);
    inc(TotalSize, FPLCs[plc].S7200Counters.Size);
    inc(TotalSize, FPLCs[plc].S7200AnInput.Size);
    inc(TotalSize, FPLCs[plc].S7200AnOutput.Size);

    //if current plc has nothing to be read
    //delete it...
    if (TotalSize=0) and (not FPLCs[plc].Connected) then
      DeletePLC(plc);
  finally
    Inherited DoDelTag(TagObj);
  end;
end;

procedure TSiemensProtocolFamily.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
var
  plc, db, block, retries:LongInt;
  msgout, msgin:BYTES;
  initialized:Boolean;
  ReqList:TS7ReqList;
  ReqOutOfScan:TS7ReqList;
  MsgOutSize:LongInt;
  RequestsPendding:Boolean;

  OutgoingPDUSize, IncomingPDUSize:LongInt;
  OutOffScanOutgoingPDUSize, OutOffScanIncomingPDUSize:LongInt;

  ivalues:TArrayOfDouble;
  EntireTagList: TS7ScanReqList;
  ReqItem, ReqItem2:TS7ScanReqItem;
  c, c2: Integer;
  started: TDateTime;

  procedure pkg_initialized;
  begin
    if not initialized then begin
      OutgoingPDUSize:=10+2; //10 of header + 2 bytes of read request;
      IncomingPDUSize:=10+2+2; //10 of header + 2 bytes of the error code + 2 bytes of the read request;
      MsgOutSize:=PDUOutgoing+12;
      SetLength(msgout,MsgOutSize);
      PrepareReadRequest(msgout);
      initialized:=true;
    end;
  end;

  function AcceptThisRequest(CPU:TS7CPU; iSize:LongInt):Boolean;
  begin
    if ((OutgoingPDUSize+12)<CPU.MaxPDULen) AND ((IncomingPDUSize+4+iSize)<CPU.MaxPDULen) then
      Result:=true
    else
      Result:=false;
  end;

  function OutOfScanAcceptThisRequest(CPU:TS7CPU; iSize:LongInt):Boolean;
  begin
    if ((OutOffScanOutgoingPDUSize+12)<CPU.MaxPDULen) AND ((OutOffScanIncomingPDUSize+4+iSize)<CPU.MaxPDULen) then
      Result:=true
    else
      Result:=false;
  end;

  procedure QueueOutOfScanReq(iPLC, iDB, iReqType, iStartAddress, iSize:LongInt);
  var
    h:LongInt;
  begin
    h:=Length(ReqOutOfScan);
    SetLength(ReqOutOfScan,h+1);
    with ReqOutOfScan[h] do begin
      PLCIdx := iPLC;
      DBIdx := iDB;
      ReqType := iReqType;
      StartAddress := iStartAddress;
      Size := iSize;
    end;
    inc(OutOffScanIncomingPDUSize,4+iSize);
    if (iSize mod 2)=1 then
      inc(OutOffScanIncomingPDUSize);

    inc(OutOffScanOutgoingPDUSize,12);
  end;

  procedure AddToReqList(iPLC, iDB, iReqType, iStartAddress, iSize:LongInt);
  var
    h:LongInt;
  begin
    h:=Length(ReqList);
    SetLength(ReqList,h+1);
    with ReqList[h] do begin
      PLCIdx := iPLC;
      DBIdx := iDB;
      ReqType := iReqType;
      StartAddress := iStartAddress;
      Size := iSize;
    end;
    inc(MsgOutSize, 12);
    inc(IncomingPDUSize,4+iSize);
    IncomingPDUSize:=IncomingPDUSize+(iSize mod 2);

    inc(OutgoingPDUSize,12);

    SetLength(msgout,MsgOutSize);
    RequestsPendding:=true;
  end;

  procedure Reset;
  begin
    initialized:=false;
    OutgoingPDUSize:=0;
    IncomingPDUSize:=0;
    MsgOutSize:=0;
    RequestsPendding:=false;
    SetLength(ReqList,0);
    SetLength(msgout,0);
    SetLength(msgin,0);
  end;

  procedure ReadQueuedRequests(var CPU:TS7CPU);
  begin
    if exchange(CPU, msgout, msgin, false) then begin
      UpdateMemoryManager(msgin, msgout, False, ReqList, ivalues);
      NeedSleep:=-1;
    end else
      NeedSleep:=1;
    Reset;
  end;

  procedure AddToTagList(iPLC, iDB, iDBNum, iReqType, iStartAddress, iSize, UpdateRate:LongInt; LastUpdate:TDateTime; NeedUpdate:Boolean);
  var
    info:TS7ScanReqItem;
  begin
    //new(info);
    info.iPLC          :=iPLC;
    info.iDB           :=iDB;
    info.iDBNum        :=iDBNum;
    info.iReqType      :=iReqType;
    info.iStartAddress :=iStartAddress;
    info.iSize         :=iSize;
    info.LastUpdate    :=LastUpdate;
    info.UpdateRate    :=UpdateRate;
    info.NeedUpdate    :=NeedUpdate;
    info.Read          :=false;

    EntireTagList.add(info);
  end;
begin
  retries := 0;
  while (not FAdapterInitialized) AND (retries<3) do begin
    FAdapterInitialized := initAdapter;
    inc(retries)
  end;

  if retries>=3 then begin
    NeedSleep:=-1;
    exit;
  end;

  NeedSleep:=ifthen(Length(FPLCs)<=0,-1,1);

  EntireTagList:=TS7ScanReqList.Create;
  try
    for plc:=0 to High(FPLCs) do begin
      if not FPLCs[plc].Connected then
        if not connectPLC(FPLCs[plc]) then exit;

      Reset;
      OutOffScanOutgoingPDUSize:=0;
      OutOffScanIncomingPDUSize:=0;

      //DBs     //////////////////////////////////////////////////////////////////
      for db := 0 to high(FPLCs[plc].DBs) do begin
        for block := 0 to High(FPLCs[plc].DBs[db].DBArea.Blocks) do begin
          AddToTagList(plc,
                       db,
                       FPLCs[plc].DBs[db].DBNum,
                       vtS7_DB,
                       FPLCs[plc].DBs[db].DBArea.Blocks[block].AddressStart,
                       FPLCs[plc].DBs[db].DBArea.Blocks[block].Size,
                       FPLCs[plc].DBs[db].DBArea.Blocks[block].ScanTime,
                       FPLCs[plc].DBs[db].DBArea.Blocks[block].LastUpdate,
                       FPLCs[plc].DBs[db].DBArea.Blocks[block].NeedRefresh);
        end;
      end;

      //INPUTS////////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].Inputs.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_Inputs,
                     FPLCs[plc].Inputs.Blocks[block].AddressStart,
                     FPLCs[plc].Inputs.Blocks[block].Size,
                     FPLCs[plc].Inputs.Blocks[block].ScanTime,
                     FPLCs[plc].Inputs.Blocks[block].LastUpdate,
                     FPLCs[plc].Inputs.Blocks[block].NeedRefresh);
      end;

      //OUTPUTS///////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].Outputs.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_Outputs,
                     FPLCs[plc].Outputs.Blocks[block].AddressStart,
                     FPLCs[plc].Outputs.Blocks[block].Size,
                     FPLCs[plc].Outputs.Blocks[block].ScanTime,
                     FPLCs[plc].Outputs.Blocks[block].LastUpdate,
                     FPLCs[plc].Outputs.Blocks[block].NeedRefresh);
      end;

      //Timers///////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].Timers.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_Timer,
                     FPLCs[plc].Timers.Blocks[block].AddressStart,
                     FPLCs[plc].Timers.Blocks[block].Size,
                     FPLCs[plc].Timers.Blocks[block].ScanTime,
                     FPLCs[plc].Timers.Blocks[block].LastUpdate,
                     FPLCs[plc].Timers.Blocks[block].NeedRefresh);
      end;

      //Counters//////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].Counters.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_Counter,
                     FPLCs[plc].Counters.Blocks[block].AddressStart,
                     FPLCs[plc].Counters.Blocks[block].Size,
                     FPLCs[plc].Counters.Blocks[block].ScanTime,
                     FPLCs[plc].Counters.Blocks[block].LastUpdate,
                     FPLCs[plc].Counters.Blocks[block].NeedRefresh);
      end;

      //Flags///////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].Flags.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_Flags,
                     FPLCs[plc].Flags.Blocks[block].AddressStart,
                     FPLCs[plc].Flags.Blocks[block].Size,
                     FPLCs[plc].Flags.Blocks[block].ScanTime,
                     FPLCs[plc].Flags.Blocks[block].LastUpdate,
                     FPLCs[plc].Flags.Blocks[block].NeedRefresh);
      end;

      //PeripheralInputs///////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].PeripheralInputs.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_Peripheral,
                     FPLCs[plc].PeripheralInputs.Blocks[block].AddressStart,
                     FPLCs[plc].PeripheralInputs.Blocks[block].Size,
                     FPLCs[plc].PeripheralInputs.Blocks[block].ScanTime,
                     FPLCs[plc].PeripheralInputs.Blocks[block].LastUpdate,
                     FPLCs[plc].PeripheralInputs.Blocks[block].NeedRefresh);
      end;

      //S7200AnInput///////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].S7200AnInput.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_200_AnInput,
                     FPLCs[plc].S7200AnInput.Blocks[block].AddressStart,
                     FPLCs[plc].S7200AnInput.Blocks[block].Size,
                     FPLCs[plc].S7200AnInput.Blocks[block].ScanTime,
                     FPLCs[plc].S7200AnInput.Blocks[block].LastUpdate,
                     FPLCs[plc].S7200AnInput.Blocks[block].NeedRefresh);
      end;

      //S7200AnOutput//////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].S7200AnOutput.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_200_AnOutput,
                     FPLCs[plc].S7200AnOutput.Blocks[block].AddressStart,
                     FPLCs[plc].S7200AnOutput.Blocks[block].Size,
                     FPLCs[plc].S7200AnOutput.Blocks[block].ScanTime,
                     FPLCs[plc].S7200AnOutput.Blocks[block].LastUpdate,
                     FPLCs[plc].S7200AnOutput.Blocks[block].NeedRefresh);
      end;

      //S7200Timers///////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].S7200Timers.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_200_Timer,
                     FPLCs[plc].S7200Timers.Blocks[block].AddressStart,
                     FPLCs[plc].S7200Timers.Blocks[block].Size,
                     FPLCs[plc].S7200Timers.Blocks[block].ScanTime,
                     FPLCs[plc].S7200Timers.Blocks[block].LastUpdate,
                     FPLCs[plc].S7200Timers.Blocks[block].NeedRefresh);
      end;

      //S7200Counters//////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].S7200Counters.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_200_Counter,
                     FPLCs[plc].S7200Counters.Blocks[block].AddressStart,
                     FPLCs[plc].S7200Counters.Blocks[block].Size,
                     FPLCs[plc].S7200Counters.Blocks[block].ScanTime,
                     FPLCs[plc].S7200Counters.Blocks[block].LastUpdate,
                     FPLCs[plc].S7200Counters.Blocks[block].NeedRefresh);
      end;

      //S7200SMs//////////////////////////////////////////////////////////////////
      for block := 0 to High(FPLCs[plc].S7200SMs.Blocks) do begin
        AddToTagList(plc,
                     0,
                     0,
                     vtS7_200_SM,
                     FPLCs[plc].S7200SMs.Blocks[block].AddressStart,
                     FPLCs[plc].S7200SMs.Blocks[block].Size,
                     FPLCs[plc].S7200SMs.Blocks[block].ScanTime,
                     FPLCs[plc].S7200SMs.Blocks[block].LastUpdate,
                     FPLCs[plc].S7200SMs.Blocks[block].NeedRefresh);
      end;

      EntireTagList.Sort(@SortGenericTagList);
      started:=Now;

      RequestsPendding:=false;
      for c:=0 to EntireTagList.Count-1 do begin
        ReqItem:=EntireTagList.Items[c];
        if ReqItem.Read then continue;
        if (PReadSomethingAlways=false) AND (ReqItem.NeedUpdate=false) then continue;
        if MilliSecondsBetween(Now, started)>100 then break;

        if not AcceptThisRequest(FPLCs[ReqItem.iPLC], ReqItem.iSize) and ((c+1) < EntireTagList.Count) then begin
          for c2:=(c+1) to EntireTagList.Count-1 do begin
            ReqItem2:=EntireTagList.Items[c2];
            if ReqItem2.Read then continue;

            if AcceptThisRequest(FPLCs[ReqItem2.iPLC], ReqItem2.iSize) then begin
              AddToReqList(ReqItem2.iPLC, ReqItem2.iDB, ReqItem2.iReqType, ReqItem2.iStartAddress, ReqItem2.iSize);
              AddToReadRequest(msgout, ReqItem2.iReqType, ReqItem2.iDBNum, ReqItem2.iStartAddress, ReqItem2.iSize);
              ReqItem2.Read:=true;
            end;
          end;
          ReadQueuedRequests(FPLCs[ReqItem.iPLC]);
          Reset;
          if ReqItem.NeedUpdate=false then exit;
        end;
        pkg_initialized;

        AddToReqList(ReqItem.iPLC, ReqItem.iDB, ReqItem.iReqType, ReqItem.iStartAddress, ReqItem.iSize);
        AddToReadRequest(msgout, ReqItem.iReqType, ReqItem.iDBNum, ReqItem.iStartAddress, ReqItem.iSize);

        ReqItem.Read:=true;
      end;
      if RequestsPendding then begin
        ReadQueuedRequests(FPLCs[plc]);
      end;

      EntireTagList.Clear;
    end;
  finally
    FreeAndNil(EntireTagList);
  end;

  SetLength(ivalues,0);
  SetLength(msgin,0);
  SetLength(msgout,0);
  SetLength(ReqList,0);
end;

procedure TSiemensProtocolFamily.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
var
  plc, db:LongInt;
  foundplc, founddb:Boolean;
begin
  foundplc:=false;

  for plc := 0 to High(FPLCs) do
    if (FPLCs[plc].Slot=TagRec.Slot) AND (FPLCs[plc].Rack=TagRec.Rack) AND (FPLCs[plc].Station=TagRec.Station) then begin
      foundplc:=true;
      break;
    end;

  if not foundplc then exit;

  SetLength(values.Values, TagRec.Size);

  case TagRec.ReadFunction of
    1:
      FPLCs[plc].Inputs.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    2:
      FPLCs[plc].Outputs.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    3:
      FPLCs[plc].Flags.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    4: begin
      if TagRec.File_DB<=0 then
        TagRec.File_DB:=1;

      founddb:=false;
      for db:=0 to high(FPLCs[plc].DBs) do
        if FPLCs[plc].DBs[db].DBNum=TagRec.File_DB then begin
          founddb:=true;
          break;
        end;

      if not founddb then exit;

      FPLCs[plc].DBs[db].DBArea.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    end;
    5:
      FPLCs[plc].Counters.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    6:
      FPLCs[plc].Timers.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    7:
      FPLCs[plc].S7200SMs.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    8:
      FPLCs[plc].S7200AnInput.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    9:
      FPLCs[plc].S7200AnOutput.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    10:
      FPLCs[plc].S7200Counters.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    11:
      FPLCs[plc].S7200Timers.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
    12:
      FPLCs[plc].PeripheralInputs.GetValues(TagRec.Address,TagRec.Size,1, values.Values, values.LastQueryResult, values.ValuesTimestamp);
  end;
end;

function  TSiemensProtocolFamily.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  c,
  OutgoingPacketSize,
  MaxBytesToSend,
  retries,
  BytesToSend,
  BytesSent,
  ReqType,
  dbidx:LongInt;
  foundplc,
  hasAtLeastOneSuccess:Boolean;
  PLCPtr:PS7CPU;
  msgout, msgin, BytesBuffer:BYTES;
  incomingPDU:TPDU;
  ReqList:TS7ReqList;
  ivalues:TArrayOfDouble;
  foundDB: Boolean;
begin
  PLCPtr:=nil;
  foundplc:=false;
  dbidx:=-1;
  SetLength(msgin,0);
  SetLength(ivalues,0);
  for c:=0 to High(FPLCs) do
    if (FPLCs[c].Slot=tagrec.Slot) and (FPLCs[c].Rack=tagrec.Rack) and (FPLCs[c].Station=tagrec.Station) then begin
      PLCPtr:=@FPLCs[c];
      foundplc:=true;
      break;
    end;

  if PLCPtr=nil then begin
    c:=CreatePLC(tagrec.Rack, tagrec.Slot, tagrec.Station);
    PLCPtr:=@FPLCs[c];
  end;

  retries := 0;
  while (not FAdapterInitialized) AND (retries<3) do begin
    FAdapterInitialized := initAdapter;
    inc(retries)
  end;

  if retries>=3 then begin
    Result:=ioDriverError;
    exit;
  end;

  if not PLCPtr^.Connected then
    if not connectPLC(PLCPtr^) then begin
      Result:=ioDriverError;
      exit;
    end;

  case tagrec.ReadFunction of
    1: begin
      Result:=ioIllegalFunction;
      exit;
    end;
    2:
      ReqType := vtS7_Outputs;
    3:
      ReqType := vtS7_Flags;
    4: begin
      ReqType := vtS7_DB;
      foundDB:=false;
      if foundplc then
        for dbidx:=0 to High(PLCPtr^.DBs) do
          if PLCPtr^.DBs[dbidx].DBNum=tagrec.File_DB then begin
            foundDB:=true;
            break;
          end;
    end;
    5:
      ReqType := vtS7_Counter;
    6:
      ReqType := vtS7_Timer;
    7:
      ReqType := vtS7_200_SM;
    8:
      ReqType := vtS7_200_AnInput;
    9:
      ReqType := vtS7_200_AnOutput;
    10:
      ReqType := vtS7_200_Counter;
    11:
      ReqType := vtS7_200_Timer;
    12:
      ReqType := vtS7_Peripheral;
  end;

  MaxBytesToSend:=PLCPtr^.MaxPDULen-28;
  BytesSent:=0;
  hasAtLeastOneSuccess:=false;

  while BytesSent<Length(Values) do begin
    SetLength(msgout,0);

    BytesToSend:=Min(MaxBytesToSend, Length(Values)-BytesSent);

    OutgoingPacketSize:=PDUOutgoing+28+BytesToSend;

    SetLength(msgout,OutgoingPacketSize);

    PrepareWriteRequest(msgout);

    BytesBuffer := DoublesToBytes(Values, BytesSent, BytesToSend);

    if ReqType=vtS7_DB then begin
      if tagrec.File_DB=0 then begin
        AddParamToWriteRequest(msgout, vtS7_DB, 1, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
        AddDataToWriteRequest(msgout, vtS7_DB, 1, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
      end else begin
        AddParamToWriteRequest(msgout, vtS7_DB, tagrec.File_DB, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
        AddDataToWriteRequest(msgout, vtS7_DB, tagrec.File_DB, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
      end;
    end else begin
      AddParamToWriteRequest(msgout, ReqType, 0, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
      AddDataToWriteRequest(msgout, ReqType, 0, tagrec.Address+tagrec.OffSet+BytesSent, BytesBuffer);
    end;

    if exchange(PLCPtr^, msgout, msgin, True) then begin
      SetupPDU(msgin,false,incomingPDU);
      if (incomingPDU.data_len>0) and (GetByte(incomingPDU.data,0)=$FF) then begin
        hasAtLeastOneSuccess:=true;
        Result:=ioOk;
        if foundplc then begin
          SetLength(ReqList,1);
          ReqList[0].DBIdx:=ifthen(foundDB, dbidx, -1);
          ReqList[0].PLCIdx:=c;
          ReqList[0].ReqType:=ReqType;
          ReqList[0].StartAddress:=tagrec.Address+BytesSent+tagrec.OffSet;
          ReqList[0].Size:=BytesToSend;
          UpdateMemoryManager(msgin, msgout, true, ReqList, ivalues);
        end;
      end else begin
        if hasAtLeastOneSuccess then begin
          Result:=ioPartialOk
        end else
          if incomingPDU.data_len>0 then begin
            Result:=S7ErrorCodeToProtocolErrorCode(GetByte(incomingPDU.data,0))
          end else
            Result := ioCommError;
        exit;
      end;
    end else begin
      if hasAtLeastOneSuccess then begin
        Result:=ioPartialOk
      end else
        Result:=ioCommError;

      exit;
    end;

    inc(BytesSent,BytesToSend);
  end;

  SetLength(ivalues, 0);
end;

function  TSiemensProtocolFamily.DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  c,
  IncomingPacketSize,
  OutgoingPacketSize,
  MaxBytesToRecv,
  retries,
  BytesToRecv,
  BytesReceived,
  ReqType,
  dbidx:LongInt;
  foundplc,
  hasAtLeastOneSuccess:Boolean;
  PLCPtr:PS7CPU;
  msgout, msgin:BYTES;
  incomingPDU:TPDU;
  ReqList:TS7ReqList;
  ivalues:TArrayOfDouble;
  founddb: Boolean;
begin
  PLCPtr:=nil;
  foundplc:=false;
  dbidx:=-1;
  SetLength(ivalues, 0);
  for c:=0 to High(FPLCs) do
    if (FPLCs[c].Slot=tagrec.Slot) and (FPLCs[c].Rack=tagrec.Rack) and (FPLCs[c].Station=tagrec.Station) then begin
      PLCPtr:=@FPLCs[c];
      foundplc:=true;
      break;
    end;

  if PLCPtr=nil then begin
    c:=CreatePLC(tagrec.Rack, tagrec.Slot, tagrec.Station);
    PLCPtr:=@FPLCs[c];
    foundplc:=true;
  end;

  retries := 0;
  while (not FAdapterInitialized) AND (retries<3) do begin
    FAdapterInitialized := initAdapter;
    inc(retries)
  end;

  if retries>=3 then begin
    Result:=ioDriverError;
    exit;
  end;

  if not PLCPtr^.Connected then
    if not connectPLC(PLCPtr^) then begin
      Result:=ioDriverError;
      exit;
    end;

  case tagrec.ReadFunction of
    1:
      ReqType:=vtS7_Inputs;
    2:
      ReqType := vtS7_Outputs;
    3:
      ReqType := vtS7_Flags;
    4: begin
      ReqType := vtS7_DB;
      founddb:=false;
      if foundplc then
        for dbidx:=0 to High(PLCPtr^.DBs) do
          if PLCPtr^.DBs[dbidx].DBNum=tagrec.File_DB then begin
            founddb:=true;
            break;
          end;
    end;
    5:
      ReqType := vtS7_Counter;
    6:
      ReqType := vtS7_Timer;
    7:
      ReqType := vtS7_200_SM;
    8:
      ReqType := vtS7_200_AnInput;
    9:
      ReqType := vtS7_200_AnOutput;
    10:
      ReqType := vtS7_200_Counter;
    11:
      ReqType := vtS7_200_Timer;
    12:
      ReqType := vtS7_Peripheral;
  end;

  MaxBytesToRecv:=PLCPtr^.MaxPDULen-18; //10 bytes of header, 2 bytes of error code, 2 bytes of read request, 4 bytes of result header.
  BytesReceived:=0;
  hasAtLeastOneSuccess:=false;

  SetLength(Values, tagrec.Size);

  while BytesReceived<tagrec.Size do begin
    SetLength(msgout,0);

    BytesToRecv:=Min(MaxBytesToRecv, tagrec.Size-BytesReceived);

    IncomingPacketSize:=PDUIncoming+18+BytesToRecv;
    OutgoingPacketSize:=PDUOutgoing+24; //10 bytes of header, 2 bytes of read request, 12 bytes of read request header.

    SetLength(msgout,OutgoingPacketSize);
    SetLength(msgin, IncomingPacketSize);

    PrepareReadRequest(msgout);

    if ReqType=vtS7_DB then begin
      if tagrec.File_DB=0 then begin
        AddToReadRequest(msgout, vtS7_DB, 1,              tagrec.Address+tagrec.OffSet+BytesReceived, Min(MaxBytesToRecv, tagrec.Size-BytesReceived));
      end else begin
        AddToReadRequest(msgout, vtS7_DB, tagrec.File_DB, tagrec.Address+tagrec.OffSet+BytesReceived, Min(MaxBytesToRecv, tagrec.Size-BytesReceived));
      end;
    end else begin
      AddToReadRequest(  msgout, ReqType, 0,              tagrec.Address+tagrec.OffSet+BytesReceived, Min(MaxBytesToRecv, tagrec.Size-BytesReceived));
    end;

    if exchange(PLCPtr^, msgout, msgin, false) then begin
      SetupPDU(msgin,false,incomingPDU);
      if (incomingPDU.data_len>0) and (GetByte(incomingPDU.data,0)=$FF) then begin
        hasAtLeastOneSuccess:=true;
        Result:=ioOk;
        if foundplc then begin
          SetLength(ReqList,1);
          ReqList[0].DBIdx:=ifthen(founddb,dbidx,-1);
          ReqList[0].PLCIdx:=c;
          ReqList[0].ReqType:=ReqType;
          ReqList[0].StartAddress:=tagrec.Address+BytesReceived+tagrec.OffSet;
          ReqList[0].Size:=BytesToRecv;
          UpdateMemoryManager(msgin, msgout, false, ReqList, ivalues);
          Move(ivalues[0],Values[BytesReceived], Length(ivalues)*sizeof(Double));
        end;
      end else begin
        if hasAtLeastOneSuccess then begin
          Result:=ioPartialOk
        end else
          if incomingPDU.data_len>0 then begin
            Result:=S7ErrorCodeToProtocolErrorCode(GetByte(incomingPDU.data,0))
          end else
            Result := ioCommError;
        exit;
      end;
    end else begin
      if hasAtLeastOneSuccess then begin
        Result:=ioPartialOk
      end else
        Result:=ioCommError;

      exit;
    end;

    inc(BytesReceived,BytesToRecv);
  end;
end;

procedure TSiemensProtocolFamily.RunPLC(CPU:TS7CPU);
var
  paramToRun, msgout, msgin:BYTES;
begin
  SetLength(paramToRun,20);
  SetLength(msgout, 0);
  SetLength(msgin, 0);
  paramToRun[00]:=$28;
  paramToRun[01]:=0;
  paramToRun[02]:=0;
  paramToRun[03]:=0;
  paramToRun[04]:=0;
  paramToRun[05]:=0;
  paramToRun[06]:=0;
  paramToRun[07]:=$FD;
  paramToRun[08]:=0;
  paramToRun[09]:=0;
  paramToRun[10]:=9;
  paramToRun[11]:=$50; //P
  paramToRun[12]:=$5F; //_
  paramToRun[13]:=$50; //P
  paramToRun[14]:=$52; //R
  paramToRun[15]:=$4F; //O
  paramToRun[16]:=$47; //G
  paramToRun[17]:=$52; //R
  paramToRun[18]:=$41; //A
  paramToRun[19]:=$4D; //M

  InitiatePDUHeader(msgout, 1);
  AddParam(msgout, paramToRun);

  if not exchange(CPU,msgout,msgin,false) then
    raise Exception.Create('Cannot swicth the PLC to RUN');

end;

procedure TSiemensProtocolFamily.StopPLC(CPU:TS7CPU);
begin

end;

procedure TSiemensProtocolFamily.CopyRAMToROM(CPU:TS7CPU);
begin

end;

procedure TSiemensProtocolFamily.CompressMemory(CPU:TS7CPU);
begin

end;

function  TSiemensProtocolFamily.S7ErrorCodeToProtocolErrorCode(code:Word):TProtocolIOResult;
begin
  case code of
    $FF: Result:=ioOk;
    $06: Result:=ioIllegalRequest;
    $0A ,$03: Result:=ioObjectNotExists;
    $05: Result:=ioIllegalMemoryAddress;
    else
      Result:=ioUnknownError;
  end;
end;

function  TSiemensProtocolFamily.GetTagInfo(tagobj:TTag):TTagRec;
begin
  if tagobj is TPLCTagNumber then begin
    with Result do begin
      Rack:=TPLCTagNumber(TagObj).PLCRack;
      Slot:=TPLCTagNumber(TagObj).PLCSlot;
      Station:=TPLCTagNumber(TagObj).PLCStation;
      File_DB:=TPLCTagNumber(TagObj).MemFile_DB;
      Address:=TPLCTagNumber(TagObj).MemAddress;
      SubElement:=TPLCTagNumber(TagObj).MemSubElement;
      Size:=TPLCTagNumber(TagObj).TagSizeOnProtocol;
      OffSet:=0;
      ReadFunction:=TPLCTagNumber(TagObj).MemReadFunction;
      WriteFunction:=TPLCTagNumber(TagObj).MemWriteFunction;
      UpdateTime:=TPLCTagNumber(TagObj).UpdateTime;
      CallBack:=nil;
    end;
    exit;
  end;

  if tagobj is TPLCBlock then begin
    with Result do begin
      Rack:=TPLCBlock(TagObj).PLCRack;
      Slot:=TPLCBlock(TagObj).PLCSlot;
      Station:=TPLCBlock(TagObj).PLCStation;
      File_DB:=TPLCBlock(TagObj).MemFile_DB;
      Address:=TPLCBlock(TagObj).MemAddress;
      SubElement:=TPLCBlock(TagObj).MemSubElement;
      Size:=TPLCBlock(TagObj).TagSizeOnProtocol;
      OffSet:=0;
      ReadFunction:=TPLCBlock(TagObj).MemReadFunction;
      WriteFunction:=TPLCBlock(TagObj).MemWriteFunction;
      UpdateTime:=TPLCBlock(TagObj).UpdateTime;
      CallBack:=nil;
    end;
    exit;
  end;

  if tagobj is TPLCString then begin
    with Result do begin
      Rack:=TPLCString(TagObj).PLCRack;
      Slot:=TPLCString(TagObj).PLCSlot;
      Station:=TPLCString(TagObj).PLCStation;
      File_DB:=TPLCString(TagObj).MemFile_DB;
      Address:=TPLCString(TagObj).MemAddress;
      SubElement:=TPLCString(TagObj).MemSubElement;
      Size:=TPLCString(TagObj).StringSize;
      OffSet:=0;
      ReadFunction:=TPLCString(TagObj).MemReadFunction;
      WriteFunction:=TPLCString(TagObj).MemWriteFunction;
      UpdateTime:=TPLCString(TagObj).UpdateTime;
      CallBack:=nil;
    end;
    exit;
  end;
  raise Exception.Create(SinvalidTag);
end;

function TSiemensProtocolFamily.GetByte(Ptr:PByte; idx:LongInt):LongInt;
var
  inptr:PByte;
begin
  inptr:=Ptr;
  inc(inptr, idx);
  Result := inptr^;
end;

procedure TSiemensProtocolFamily.SetByte(Ptr:PByte; idx:LongInt; value:Byte);
var
  inptr:PByte;
begin
  inptr:=Ptr;
  inc(inptr, idx);
  inptr^ := value;
end;

procedure TSiemensProtocolFamily.SetBytes(Ptr:PByte; idx:LongInt; values:BYTES);
var
  inptr:PByte;
begin
  inptr:=Ptr;
  inc(inptr, idx);
  Move(values[0],inptr^,Length(values));
end;

var
  TagBuilderEditor:TOpenTagEditor = nil;

procedure TSiemensProtocolFamily.OpenTagEditor(InsertHook: TAddTagInEditorHook;
  CreateProc: TCreateTagProc);
begin
  if Assigned(TagBuilderEditor) then
    TagBuilderEditor(Self, Self.Owner,InsertHook,CreateProc)
  else
    inherited OpenTagEditor(InsertHook,CreateProc);
end;

function TSiemensProtocolFamily.HasTabBuilderEditor: Boolean;
begin
  Result:=true;
end;

procedure SetTagBuilderToolForSiemensS7ProtocolFamily(TagBuilderTool:TOpenTagEditor);
begin
  if assigned(TagBuilderEditor) then
    raise Exception.Create('A Tag Builder editor for Siemens S7 protocol family was already assigned.')
  else
    TagBuilderEditor:=TagBuilderTool;
end;

end.
