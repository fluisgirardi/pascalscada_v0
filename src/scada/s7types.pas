{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Tipos comuns aos CLP's da familia Siemens.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Common types used by Siemens PLC's.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit S7Types;

interface

uses
  PLCMemoryManager;

type
  {$IFDEF PORTUGUES}
  {:
  Representa o cabeçalho de uma PDU.

  @member P             Seu valor é sempre 0x32
  @member PDUHeadertype Tipo do cabeçalho, podendo ser 1,2,3 ou 7. Os cabeçalhos tipos 2 e 3 são dois bytes maiores.
  @member a             Desconhecido.
  @member b             Desconhecido.
  @member number        Um número. Pode ser usado para assegurar que o pacote recebido corresponde ao que foi selecionado com o mesmo número.
  @member param_len     Tamanho dos parametros após o cabeçalho.
  @member data_len      Tamanho dos dados após o cabeçalho.
  @member Error         Somente presente em cabeçalhos tipo 2 e 3. Contem o código do erro.
  }
  {$ELSE}
  {:
  Represents the PDU header.

  @member P             Allways 0x32
  @member PDUHeadertype Header type, one of 1,2,3 or 7. type 2 and 3 headers are two bytes longer.
  @member a             Currently unknown. Maybe it can be used for long numbers?
  @member b             Currently unknown. Maybe it can be used for long numbers?
  @member number        A number. This can be used to make sure a received answer corresponds to the request with the same number.
  @member param_len     Length of parameters which follow this header.
  @member data_len      Length of data which follow the parameters.
  @member Error         Only present in type 2 and 3 headers. This contains error information.
  }
  {$ENDIF}
  TPDUHeader = record
      P,
      PDUHeadertype,
      a,b:Byte;
      number,
      param_len,
      data_len:Word;
      Error:Word;
  end;

  {$IFDEF PORTUGUES}
  {:
  Aponta para uma estrutura de cabeçalho PDU.
  }
  {$ELSE}
  {:
  Points to a PDU structure header.
  }
  {$ENDIF}
  PPDUHeader = ^TPDUHeader;

  {$IFDEF PORTUGUES}
  {:
  Estrutura de uma PDU.
  @member header        Aponta para o cabeçalho da PDU.
  @member param         Aponta para os parametros da PDU
  @member data          Aponta para os dados da PDU
  @member udata         Aponta para os dados de usuário da PDU
  @member header_len    Tamanho do cabeçalho.
  @member param_len     Tamanho dos parametros
  @member data_len      Tamanho dos dados.
  @member user_data_len Tamanho dos dados de usuário.
  }
  {$ELSE}
  {:
  PDU structure.
  @member header        Point to start of PDU (PDU header)
  @member param         Point to start of parameters inside PDU
  @member data          Point to start of data inside PDU
  @member udata         Point to start of data inside PDU
  @member header_len    Header length
  @member param_len     Parameter length
  @member data_len      Data length
  @member user_data_len User or result data length
  }
  {$ENDIF}
  TPDU = record
    header:PByte;
    param:PByte;
    data:PByte;
    udata:PByte;
    header_len:LongInt;
    param_len:LongInt;
    data_len:LongInt;
    user_data_len:LongInt;
  end;
  PPDU = ^TPDU;

  {$IFDEF PORTUGUES}
  {:
  Identifica um DB da familia S7-1200/S7-300/S7-400
  @member DBNum Número do DB.
  @member DBArea Gerenciador de blocos de memória não continuas.
  }
  {$ELSE}
  {:
  Identifies a DB of S7-1200/S7-300/S7-400 PLC's.
  @member DBNum DB Number.
  @member DBArea Manager of non-continuous memory blocks.
  }
  {$ENDIF}
  TS7DB = Record
    DBNum:Cardinal;
    DBArea:TPLCMemoryManager;
  end;

  {$IFDEF PORTUGUES}
  //: Identifica um conjunto de DB's da familia S7-300/S7-400
  {$ELSE}
  //: Identifies a set of DB's of S7-1200/S7-300/S7-400 PLC's.
  {$ENDIF}
  TS7DBs = array of TS7DB;

  {$IFDEF PORTUGUES}
  {:
  Representa uma requisição dentro varios pedidos de leitura.
  @member PLC          Aponta para o CLP da lista
  @member DB           Número do DB
  @member ReqType      Tipo da requisição.
  @member StartAddress Endereço inicial
  @member Size         Tamanho da requisição.
  }
  {$ELSE}
  {:
  Represents one request on a set of read requests.
  @member PLC          Index of PLC on PLC's list.
  @member DB           DB Number.
  @member ReqType      Request type
  @member StartAddress Start address.
  @member Size         Request length.
  }
  {$ENDIF}
  TS7ReqListItem = record
    PLCIdx,
    DBIdx,
    ReqType,
    StartAddress,
    Size:LongInt;
  end;

  {$IFDEF PORTUGUES}
  //: Uma lista de requisições.
  {$ELSE}
  //: A request list.
  {$ENDIF}
  TS7ReqList = array of TS7ReqListItem;

  {$IFDEF PORTUGUES}
  {:
  Representação de um CLP S7-200/300/400/1200 da Siemens.
  @member Station      Endereço do CLP.
  @member Rack         Rack do CLP.
  @member Slot         Slot do CLP
  @member PDUId        Identificação da PDU.
  @member MaxPDULen    Tamanho máximo da PDU
  @member MaxBlockSize Tamanho máximo dos blocos.
  @member Connected    Informa se o processo de conexão com o CLP já foi feito.

  @member Inputs           Gerenciador de blocos de memória não continuos das entradas digitais.
  @member Outputs          Gerenciador de blocos de memória não continuos das saidas digitais.
  @member PeripheralInputs Gerenciador de blocos de memória não continuos das entradas analógicas do S7-300/400.
  @member DBs              Lista de DB's em uso.
  @member Timers           Gerenciador de blocos de memória não continuos dos Timers do S7-300/400.
  @member Counters         Gerenciador de blocos de memória não continuos das Counters do S7-300/400.
  @member Flags            Gerenciador de blocos de memória não continuos das Flags (M's).

  @member S7200SMs         Gerenciador de blocos de memória não continuos das SM's do S7-200.
  @member S7200Timers      Gerenciador de blocos de memória não continuos dos Timers do S7-200.
  @member S7200Counters    Gerenciador de blocos de memória não continuos dos Counters do S7-200.
  @member S7200AnInput     Gerenciador de blocos de memória não continuos das entradas analógicas do S7-200.
  @member S7200AnOutput    Gerenciador de blocos de memória não continuos das saidas analógicas do S7-200.
  }
  {$ELSE}
  {:
  Represents a Siemens S7-200/300/400/1200 PLC.
  @member Station      PLC address.
  @member Rack         PLC Rack.
  @member Slot         PLC Slot.
  @member PDUId        PDU identification.
  @member MaxPDULen    Maximum PDU size.
  @member MaxBlockSize Maximum block size.
  @member Connected    Tells if the connect process already done.

  @member Inputs           Manager of non-continuous memory blocks of digital inputs.
  @member Outputs          Manager of non-continuous memory blocks of digital outputs.
  @member PeripheralInputs Manager of non-continuous memory blocks of analog inputs of S7-300/400.
  @member DBs              Lista de DB's em uso.
  @member Timers           Manager of non-continuous memory blocks of Timers of S7-300/400.
  @member Counters         Manager of non-continuous memory blocks of Counters of S7-300/400.
  @member Flags            Manager of non-continuous memory blocks of Flags (M's).

  @member S7200SMs         Manager of non-continuous memory blocks of SM's of S7-200.
  @member S7200Timers      Manager of non-continuous memory blocks Timers of S7-200.
  @member S7200Counters    Manager of non-continuous memory blocks of Counters of S7-200.
  @member S7200AnInput     Manager of non-continuous memory blocks of analog inputs of S7-200.
  @member S7200AnOutput    Manager of non-continuous memory blocks of analog outputs of S7-200.
  }
  {$ENDIF}
  TS7CPU=record
    Station,
    Rack,
    Slot:LongInt;
    PDUId:Word;
    MaxPDULen:Word;
    MaxBlockSize:LongInt;
    Connected:Boolean;

    Inputs:TPLCMemoryManager;
    Outputs:TPLCMemoryManager;
    PeripheralInputs:TPLCMemoryManager;
    DBs:TS7DBs;
    Timers:TPLCMemoryManager;
    Counters:TPLCMemoryManager;
    Flags:TPLCMemoryManager;

    S7200SMs:TPLCMemoryManager;
    S7200Timers:TPLCMemoryManager;
    S7200Counters:TPLCMemoryManager;
    S7200AnInput:TPLCMemoryManager;
    S7200AnOutput:TPLCMemoryManager;
  end;

  {$IFDEF PORTUGUES}
  //: Ponteiro de CLP.
  {$ELSE}
  //: Points to a PLC.
  {$ENDIF}
  PS7CPU = ^TS7CPU;

  {$IFDEF PORTUGUES}
  //: Representação de um conjunto de CLP's S7-200/300/400 da Siemens.
  {$ELSE}
  //: Represents a set of Siemens S7-200/300/400 PLC's.
  {$ENDIF}
  TS7CPUs = array of TS7CPU;

  {$IFDEF PORTUGUES}
  {:
  Estrutura que identifica um pedido de leitura/escrita de variáveis do CLP.
  @member header       Cabeçalho da requisição. É sempre a sequencia 0x12, 0x0a, 0x10.
  @member WordLen      Tamanho da palavra (1=single bit, 2=byte, 4=word)
  @member ReqLength    Tamanho da requisição.
  @member DBNumber     Número do DB
  @member AreaCode     Código da área requisitada.
  @member HiBytes      Endereço inicial byte alto.
  @member StartAddress Endereço inicial word baixa.

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
  }
  {$ELSE}
  {:
  Struture that identifies a request of read/write of PLC memories.
  @member header       Request header. Always is the sequence 0x12, 0x0a, 0x10.
  @member WordLen      Word length (1=single bit, 2=byte, 4=word)
  @member ReqLength    Request length.
  @member DBNumber     DB Number.
  @member AreaCode     Requested area code.
  @member HiBytes      Start address high byte.
  @member StartAddress Start address lower word.

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
  }
  {$ENDIF}
  TS7Req = record
    header:array[0..2] of Byte;
    WordLen:Byte;
    ReqLength:Word;
    DBNumber:Word;
    AreaCode:Byte;
    HiBytes:Byte;
    StartAddress:Word; //bits and low bytes
  end;

  PS7Req = ^TS7Req;


  {$IFDEF PORTUGUES}
  //: Identifica o meio de conexão com o CLP.
  {$ELSE}
  //: Identifies the connection way with PLC.
  {$ENDIF}
  TISOTCPConnectionWay = (ISOTCP,ISOTCP_VIA_CP243);

const
  {$IFDEF PORTUGUES}
  //: Identifica a area de informações do S7-200
  {$ELSE}
  //: Identifies a S7-200 information area.
  {$ENDIF}
  vtS7_200_SysInfo = $03;
  {$IFDEF PORTUGUES}
  //: Identifica as SM's do S7-200
  {$ELSE}
  //: Identifies the S7-200 SM's
  {$ENDIF}
  vtS7_200_SM      = $05;
  {$IFDEF PORTUGUES}
  //: Identifica as entradas analógicas do S7-200.
  {$ELSE}
  //: Identifies the analog inputs of S7-200.
  {$ENDIF}
  vtS7_200_AnInput = $06;
  {$IFDEF PORTUGUES}
  //: Identifica as saidas analógicas do S7-200.
  {$ELSE}
  //: Identifies the analog outpus of S7-200.
  {$ENDIF}
  vtS7_200_AnOutput= $07;
  {$IFDEF PORTUGUES}
  //: Identifica os counters do S7-200.
  {$ELSE}
  //: Identifies the counters of S7-200.
  {$ENDIF}
  vtS7_200_Counter =  30;
  {$IFDEF PORTUGUES}
  //: Identifica os Timers do S7-200.
  {$ELSE}
  //: Identifies the Timers of S7-200.
  {$ENDIF}
  vtS7_200_Timer   =  31;
  {$IFDEF PORTUGUES}
  //: Identifica as entradas analógicas do S7-300/400.
  {$ELSE}
  //: Identifies the analog inputs of S7-300/400.
  {$ENDIF}
  vtS7_Peripheral  = $80;
  {$IFDEF PORTUGUES}
  //: Identifica as entradas digitais.
  {$ELSE}
  //: Identifies the digital inputs.
  {$ENDIF}
  vtS7_Inputs      = $81;
  {$IFDEF PORTUGUES}
  //: Identifica as saidas digitais.
  {$ELSE}
  //: Identifies the digital outputs.
  {$ENDIF}
  vtS7_Outputs     = $82;
  {$IFDEF PORTUGUES}
  //: Identifica as Flags (M's).
  {$ELSE}
  //: Identifies the Flags (M's).
  {$ENDIF}
  vtS7_Flags       = $83;
  {$IFDEF PORTUGUES}
  //: Identifica os DB's e as V's do S7-200.
  {$ELSE}
  //: Identifies the DB's and V's area of S7-200.
  {$ENDIF}
  vtS7_DB          = $84;
  {$IFDEF PORTUGUES}
  //: Identifica os DB's instanciados.
  {$ELSE}
  //: Identifies the instantiated DB's.
  {$ENDIF}
  vtS7_DI          = $85;  //DB Instanciado
  //: Unknown.
  vtS7_Local       = $86;  //not tested
  //: Unknown.
  vtS7_V           = $87;
  {$IFDEF PORTUGUES}
  //: Identifica os Counters do S7-300/400.
  {$ELSE}
  //: Identifies the Counters of S7-300/400.
  {$ENDIF}
  vtS7_Counter     =  28;  //S7 counters
  {$IFDEF PORTUGUES}
  //: Identifica os Timers do S7-300/400.
  {$ELSE}
  //: Identifies the Timers of S7-300/400.
  {$ENDIF}
  vtS7_Timer       =  29;  // S7 timers

  {$IFDEF PORTUGUES}
  //: Desconhecido/não testado.
  {$ELSE}
  //: Unknown/Not tested.
  {$ENDIF}  S7FuncOpenS7Connection = $F0;
  {$IFDEF PORTUGUES}
  //: Identifica um pedido de leitura.
  {$ELSE}
  //: Identifies a read request.
  {$ENDIF}
  S7FuncRead             = $04;
  {$IFDEF PORTUGUES}
  //: Identifica um pedido de escrita.
  {$ELSE}
  //: Identifies a write request.
  {$ENDIF}
  S7FuncWrite            = $05;
  {$IFDEF PORTUGUES}
  //: Desconhecido/não testado.
  {$ELSE}
  //: Unknown/Not tested.
  {$ENDIF}
  S7FuncRequestDownload  = $1A;
  {$IFDEF PORTUGUES}
  //: Desconhecido/não testado.
  {$ELSE}
  //: Unknown/Not tested.
  {$ENDIF}
  S7FuncDownloadBlock    = $1B;
  {$IFDEF PORTUGUES}
  //: Desconhecido/não testado.
  {$ELSE}
  //: Unknown/Not tested.
  {$ENDIF}
  S7FuncDownloadEnded    = $1C;
  {$IFDEF PORTUGUES}
  //: Desconhecido/não testado.
  {$ELSE}
  //: Unknown/Not tested.
  {$ENDIF}
  S7FuncStartUpload      = $1D;
  {$IFDEF PORTUGUES}
  //: Desconhecido/não testado.
  {$ELSE}
  //: Unknown/Not tested.
  {$ENDIF}
  S7FuncUpload           = $1E;
  {$IFDEF PORTUGUES}
  //: Desconhecido/não testado.
  {$ELSE}
  //: Unknown/Not tested.
  {$ENDIF}
  S7FuncEndUpload        = $1F;
  {$IFDEF PORTUGUES}
  //: Desconhecido/não testado.
  {$ELSE}
  //: Unknown/Not tested.
  {$ENDIF}
  S7FuncInsertBlock      = $28;

implementation

end.

