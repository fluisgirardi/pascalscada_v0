{:
  @abstract(Tipos comuns aos CLP's da familia Siemens.)
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)
}
unit S7Types;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  PLCMemoryManager;

type
  TPDUHeader = record
      P,             // allways 0x32
      PDUHeadertype, // Header type, one of 1,2,3 or 7. type 2 and 3 headers are two bytes longer.
      a,b:Byte;	     // currently unknown. Maybe it can be used for long numbers?
      number,        //A number. This can be used to make sure a received answer corresponds to the request with the same number.
      param_len,     //length of parameters which follow this header
      data_len:Word; //length of data which follow the parameters
      Error:Word;    //only present in type 2 and 3 headers. This contains error information.
  end;
  PPDUHeader = ^TPDUHeader;

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

  //: Identifica um DB da familia S7-300/S7-400
  TS7DB = Record
    DBNum:Cardinal;
    DBArea:TPLCMemoryManager;
  end;

  //: Identifica um conjunto de DB's da familia S7-300/S7-400
  TS7DBs = array of TS7DB;

  //: Representa uma requisição.
  TS7ReqListItem = record
    PLC,
    DB,
    ReqType,
    StartAddress,
    Size:Integer;
  end;

  TS7ReqList = array of TS7ReqListItem;

  //: Representação de um CLP S7-200/300/400 da Siemens.
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
    AnInput:TPLCMemoryManager;
    AnOutput:TPLCMemoryManager;
    DBs:TS7DBs;
    Timers:TPLCMemoryManager;
    Counters:TPLCMemoryManager;
    Flags:TPLCMemoryManager;
    SMs:TPLCMemoryManager;
  end;

  //: Representação de um conjunto de CLP's S7-200/300/400 da Siemens.
  TS7CPUs = array of TS7CPU;

  TS7Req = record
    header:array[0..2] of Byte;
    WordLen:Byte;
    ReqLength:Word;
    DBNumber:Word;
    AreaCode:Byte;
    StartAddress:Word;
    Bit:Byte;
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
  vtS7_200_VM      = $87;


  vtS7_Inputs  = $81;
  vtS7_Outputs = $82;
  vtS7_Flags   = $83;
  vtS7_DB      = $84;
  vtS7_DI      = $85;  //DB Instanciado
  //vtS7_Local   = $86;  //not tested
  vtS7_Counter =  28;  //S7 counters
  vtS7_Timer   =  29;  // S7 timers

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

