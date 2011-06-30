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

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

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

  {$ELSE}

  {$ENDIF}
  TPDU = record
    header:PByte;           // pointer to start of PDU (PDU header)
    param:PByte;            // pointer to start of parameters inside PDU
    data:PByte;             // pointer to start of data inside PDU
    udata:PByte;            // pointer to start of data inside PDU
    header_len:Integer;     // header length
    param_len:Integer;      // parameter length
    data_len:Integer;       // data length
    user_data_len:Integer;  // user or result data length
  end;
  PPDU = ^TPDU;

  {$IFDEF PORTUGUES}
  //: Identifica um DB da familia S7-1200/S7-300/S7-400
  {$ELSE}
  //: Identifies a DB of S7-1200/S7-300/S7-400 PLC's.
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
  //: Representa uma requisição dentro varios pedidos de leitura.
  {$ELSE}
  //: Represents one request on a set of read requests.
  {$ENDIF}
  TS7ReqListItem = record
    PLC,
    DB,
    ReqType,
    StartAddress,
    Size:Integer;
  end;

  {$IFDEF PORTUGUES}
  //: Uma lista de requisições.
  {$ELSE}
  //: A request list.
  {$ENDIF}
  TS7ReqList = array of TS7ReqListItem;

  {$IFDEF PORTUGUES}
  //: Representação de um CLP S7-200/300/400/1200 da Siemens.
  {$ELSE}
  //: Represents a Siemens S7-200/300/400/1200 PLC.
  {$ENDIF}
  TS7CPU=record
    Station,
    Rack,
    Slot:Integer;
    PDUId:Word;
    MaxPDULen:Word;
    MaxBlockSize:Integer;
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

  //: Identifica o meio de conexão com o CLP.
  TISOTCPConnectionWay = (ISOTCP,ISOTCP_VIA_CP243);

const

  vtS7_200_SysInfo = $03;
  vtS7_200_SM      = $05;
  vtS7_200_AnInput = $06;
  vtS7_200_AnOutput= $07;
  vtS7_200_Counter =  30;
  vtS7_200_Timer   =  31;


  vtS7_Peripheral  = $80;
  vtS7_Inputs      = $81;
  vtS7_Outputs     = $82;
  vtS7_Flags       = $83;
  vtS7_DB          = $84;
  vtS7_DI          = $85;  //DB Instanciado
  vtS7_Local       = $86;  //not tested
  vtS7_V           = $87;
  vtS7_Counter     =  28;  //S7 counters
  vtS7_Timer       =  29;  // S7 timers

  S7FuncOpenS7Connection = $F0;
  S7FuncRead             = $04;
  S7FuncWrite            = $05;
  S7FuncRequestDownload  = $1A;
  S7FuncDownloadBlock    = $1B;
  S7FuncDownloadEnded    = $1C;
  S7FuncStartUpload      = $1D;
  S7FuncUpload           = $1E;
  S7FuncEndUpload        = $1F;
  S7FuncInsertBlock      = $28;

implementation

end.

