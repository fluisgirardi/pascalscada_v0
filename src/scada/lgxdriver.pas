unit LGXDriver;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, Math, ctypes, DateUtils, ProtocolDriver,
  PLCMemoryManager, Tag, ProtocolTypes, commtypes, PLCTagNumber, PLCBlock,
  PLCString, hsutils;

type

  //============================================================================
  TPDU = class;

  TPDUList = specialize TFPGObjectList<TPDU>;
  //============================================================================

  { TPDU }

  TPDU = class
  protected
    FEncapsuledPDUs:TPDUList;
    function GetEncapsuledPDUCount: Integer;
    function getPdu(index: Integer): TPDU;
  protected
    property EncapsuledPDUCount:Integer read GetEncapsuledPDUCount;
    property PDU[index:Integer]:TPDU read getPdu;
  public
    constructor Create; virtual;
    constructor CreateFromBytes(aBytes:BYTES; var newOffset:Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); virtual; //decodes
    class function CreatePDUClassFromBytes(aBytes:BYTES; var newOffset:Integer;
                                           offset:Integer; OptData:Integer = 0):TPDU; virtual;
    destructor Destroy; override;
    function getPacket:BYTES; virtual;                     //encodes
  end;

  //////////////////////////////////////////////////////////////////////////////
  //ethernet IP encapsulation header and related PDUs
  //////////////////////////////////////////////////////////////////////////////

  { TEIPEncapsulationHeader }

  TEIPEncapsulationHeader = class(TPDU)
  private
    FCommand: Word;
    FLength: Word;
    FOptions: LongWord;
    FSenderContext: QWord;
    FSessionHandler: LongWord;
    FStatus: LongWord;
    function GetCommandData: TPDU;
  public
    constructor CreateFromBytes(aBytes:BYTES; var newOffset:Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override; //decodes
    function getPacket: BYTES; override;
    class function CreatePDUClassFromBytes(aBytes:BYTES; var newOffset:Integer;
                                           offset:Integer; OptData:Integer = 0):TPDU; override;
    property Command:Word read FCommand write FCommand;
    property PkgLength:Word read FLength;
    property SessionHandler:LongWord read FSessionHandler write FSessionHandler;
    property Status:LongWord read FStatus write FStatus;
    property SenderContext:QWord read FSenderContext write FSenderContext;
    property Options:LongWord read FOptions write FOptions;
  public
    property CommandData:TPDU read GetCommandData;
  end;

  TServiceName = array[0..15] of AnsiChar;
  TCapabilitiesFlagBits = bitpacked record
    ReservedBit0,
    ReservedBit1,
    ReservedBit2,
    ReservedBit3,
    ReservedBit4,
    CIPPacketEncapsulationViaTCP,
    ReservedBit6,
    ReservedBit7,
    CIPPacketEncapsulationViaUDP_Class0_or_1,
    ReservedBit9,
    ReservedBit10,
    ReservedBit11,
    ReservedBit12,
    ReservedBit13,
    ReservedBit14,
    ReservedBit15:Boolean
  end;

  TCapabilitiesFlags = packed record
    case Integer of
      0: (WAccess: Word);
      1: (FlagsAccess: TCapabilitiesFlagBits);
  end;


  { TListServicesResponse }

  TListServicesResponse = class(TPDU)
  private
    FCapabilities: TCapabilitiesFlags;
    FEncapsulationVersion: Integer;
    FLen: Integer;
    FServiceName: TServiceName;
    FType: Integer;
  public
    function getPacket: BYTES; override;
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
      offset: Integer; OptData: Integer=0; CreateChildrenPDUs: Boolean=true);
      override;
    property &Type:Integer read FType;
    property Len:Integer read FLen;
    property EncapsulationVersion:Integer read FEncapsulationVersion;
    property Capabilities:TCapabilitiesFlags read FCapabilities;
    property ServiceName:TServiceName read FServiceName;
  end;

  { TEIPListServicesData }

  TEIPListServicesData= class(TPDU)
  private
    FItemCount: Integer;
    function GetItem(Index: Integer): TListServicesResponse;
  public
    function getPacket: BYTES; override;
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
      offset: Integer; OptData: Integer=0; CreateChildrenPDUs: Boolean=true);
      override;
    property ItemCount:Integer read FItemCount;
    property Item[Index:Integer]:TListServicesResponse read GetItem;

  end;

  TEIPListServices = class(TEIPEncapsulationHeader)
  private
    function GetListServicesData: TEIPListServicesData;
  public
    constructor Create; override;
    property ListServicesData:TEIPListServicesData read GetListServicesData;
  end;

  { TEIPHeaderCmdDataRegSessionPDU }

  TEIPHeaderCmdDataRegSessionPDU = class(TPDU)
  private
    FOptionFlags: Word;
    FProtocolVersion: Word;
  public
    constructor Create; override;
    constructor CreateFromBytes(aBytes:BYTES; var newOffset:Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override; //decodes
    function getPacket: BYTES; override;
    property ProtocolVersion:Word read FProtocolVersion;
    property OptionFlags:Word read FOptionFlags;
  end;

  TCommonPacketFormat = class; //forward class declaration

  { TGenericEIPSendDataCmdPDU }

  TGenericEIPSendDataCmdPDU = class(TPDU)
  private
    fInterfaceHandle: LongWord;
    fTimeout: Word;
    function GetCPF: TCommonPacketFormat;
  protected
    function ParamsOK:Boolean; virtual;
  public
    constructor Create; override;
    constructor CreateFromBytes(aBytes:BYTES; var newOffset:Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
    function getPacket: BYTES; override;
    property InterfaceHandle:LongWord read fInterfaceHandle;
    property Timeout:Word read fTimeout;
    property EncapsuledCPF:TCommonPacketFormat read GetCPF;
  end;

  TSendRRDataCmdPDU = class(TGenericEIPSendDataCmdPDU)
  public
    property InterfaceHandle:LongWord write FInterfaceHandle;
    property Timeout:Word write FTimeout;
  end;

  { TSendUnitDataCmdPDU }

  TSendUnitDataCmdPDU = class(TGenericEIPSendDataCmdPDU)
  protected
    constructor Create; override;
    function ParamsOK: Boolean; override;
  end;

  { Prebuilt pkgs to send - TEIPNopPDU }

  TEIPNopPDU = class(TEIPEncapsulationHeader)
    constructor Create; override;
  end;

  { Prebuilt pkgs to send - TEIPRegSessionPDU }

  TEIPRegSessionPDU = class(TEIPEncapsulationHeader)
    constructor Create; override;
  end;

  { Prebuilt pkgs to send - TEIPUnregSessionPDU }

  TEIPUnregSessionPDU = class(TEIPEncapsulationHeader)
    constructor Create; override;
  end;

  { Prebuilt pkgs to send - TEIPSendRRDataPDU }

  { TEIPSendRRDataPDU }

  TEIPSendRRDataPDU = class(TEIPEncapsulationHeader)
  private
    function GetSendRRDataPDU: TSendRRDataCmdPDU;
  public
    constructor Create; override;
  public
    property SendRRDataCmdData:TSendRRDataCmdPDU read GetSendRRDataPDU;
  end;

  { Prebuilt pkgs to send - TEIPSendRRDataPDU }

  { TEIPSendUnitDataPDU }

  TEIPSendUnitDataPDU = class(TEIPEncapsulationHeader)
  private
    function GetSendUnitDataPDU: TSendUnitDataCmdPDU;
  public
    constructor Create; override;
  public
    Property SendUnitDataCmdData:TSendUnitDataCmdPDU read GetSendUnitDataPDU;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //Common Packet Format and related PDUs
  //////////////////////////////////////////////////////////////////////////////

  TCPFItem = class(TPDU);

  //TCommonPacketFormat

  TCommonPacketFormat = class(TPDU)
  public
    constructor CreateFromBytes(aBytes:BYTES; var newOffset:Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override; //decodes
    function getPacket: BYTES; override;
    property ItemCount:Integer read GetEncapsuledPDUCount;
  public
    function Add(aItem:TCPFItem):Integer;
  public
    property CPFItemCount:Integer read GetEncapsuledPDUCount;
    property CPFItem[index:Integer]:TPDU read getPdu;
  end;

  TCPF = TCommonPacketFormat;

  { TCPFNullAddressItem }

  TCPFNullAddressItem = class(TCPFItem)
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
    function getPacket: BYTES; override;
  end;

  { TCPFAddressItem }

  TCPFAddressItem = class(TCPFItem)
  private
    FConnID: LongWord;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
    function getPacket: BYTES; override;
    property ConnectionID:LongWord read FConnID write FConnID;
  end;

  TCIPPDU = class; //forward class

  { TCPFUnconnectedDataItem }

  TCPFUnconnectedDataItem = class(TCPFItem)
  private
    function GetEncapsuledPacket: TCIPPDU;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
    function getPacket: BYTES; override;
  public
    function Add(aItem:TCIPPDU):Integer;
  public
    property EncapsuledPacket:TCIPPDU read GetEncapsuledPacket;
  end;

  { TCPFConnectedDataItem }

  TCPFConnectedDataItem = class(TCPFItem)
  private
    FCIPSeq: Word;
    function GetEncapsuledPacket: TCIPPDU;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
    function getPacket: BYTES; override;
  public
    function Add(aItem:TCIPPDU):Integer;
  public
    Property CIPSequence:Word read FCIPSeq write FCIPSeq;
    property EncapsuledPacket:TCIPPDU read GetEncapsuledPacket;
  end;

  //============================================================================

  //Forward declarations

  //prioridades, cima para baixo

  //sim é estranho esta classe estar com mais prioridade
  //que a TCIPMultiTagRead, mas ela é dependencia
  TCIPReadTagReq = class;
  TCIPReadTagStructInfoReq = class;
  TCIPReadTagRply= class;

  TCIPMultiTagReadReq = class;
  TCIPMultiTagReadRply = class;

  TCIPReadTagFragReq = class;
  TCIPReadTagFragRply = class;

  TCIPWriteTagFragReq = class;
  TCIPWriteTagFragRply = class;

  TCIPListTagsReq = class;
  TCIPListTagsRply = class;

  TCIPWriteTagReq = class;
  TCIPWriteTagRply = class;

  TCIPGetAttrListSrvReq = class;
  TCIPGetAttrListSrvRply = class;

  { TCIPPDU }

  TCIPPDU = class(TPDU)
  public
    class function CreatePDUClassFromBytes(aBytes:BYTES; var newOffset:Integer;
                                           offset:Integer; OptData:Integer = 0):TPDU; override;

  end;

  { TForwardOpenReqPDU }

  TForwardOpenReqPDU = class(TPDU)
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
    class function CreatePDUClassFromBytes(aBytes: BYTES; var newOffset: Integer;
                                           offset: Integer;
                                           OptData:Integer = 0): TPDU; override;
  end;

  { TForwardOpenReplyPDU }

  TForwardOpenReplyPDU = class(TPDU)
  public
    constructor Create; override;
    function getPacket: BYTES; override;
    class function CreatePDUClassFromBytes(aBytes: BYTES; var newOffset: Integer;
                                           offset: Integer;
                                           OptData:Integer = 0): TPDU; override;

  end;

  { TCMForwardOpenReqPDU }

  TCMForwardOpenReqPDU = class(TForwardOpenReqPDU)
  private
    FConnSerialNumber: WORD;
    FOriginatorSerialNumber: LONGWORD;
    FOriginatorVendorID: WORD;
    FOT_ConnID: LONGWORD;
    FOT_Parameters: WORD;
    FOT_RPI: LONGWORD;
    FPath: BYTES;
    FPriority: BYTE;
    FTimeOutMultiplier: BYTE;
    FTimeOut_Ticks: BYTE;
    FTO_ConnID: LONGWORD;
    FTO_Parameters: WORD;
    FTO_RPI: LONGWORD;
    FTransport: BYTE;
  public
    Property Priority:BYTE read FPriority Write FPriority;//used to calculate request timeout information
    Property TimeOut_Ticks:BYTE read FTimeOut_Ticks Write FTimeOut_Ticks;//used to calculate request timeout information
    Property OT_ConnID:LONGWORD read FOT_ConnID Write FOT_ConnID; //originator's CIP Produced session ID
    Property TO_ConnID:LONGWORD read FTO_ConnID Write FTO_ConnID; //originator's CIP consumed session ID
    Property ConnSerialNumber:WORD read FConnSerialNumber Write FConnSerialNumber;// session serial number
    Property OriginatorVendorID:WORD read FOriginatorVendorID Write FOriginatorVendorID;
    Property OriginatorSerialNumber:LONGWORD read FOriginatorSerialNumber Write FOriginatorSerialNumber;
    Property TimeOutMultiplier:BYTE read FTimeOutMultiplier Write FTimeOutMultiplier;
    //Property ReservedB1:BYTE read FReservedB1 Write FReservedB1;
    //Property ReservedB2:BYTE read FReservedB2 Write FReservedB2;
    //Property ReservedB3:BYTE read FReservedB3 Write FReservedB3;
    Property OT_RPI:LONGWORD read FOT_RPI Write FOT_RPI;// originator to target packet rate in msec
    Property OT_Parameters:WORD read FOT_Parameters Write FOT_Parameters ;
    Property TO_RPI:LONGWORD read FTO_RPI Write FTO_RPI;// target to originator packet rate in msec
    Property TO_Parameters:WORD read FTO_Parameters Write FTO_Parameters ;
    Property Transport:BYTE read FTransport Write FTransport ;
    //Property PathSize:BYTE read FPathSize Write FPathSize;// size of session path in 16 bits words
    //EPath Path;// padded EPath
    property Path:BYTES read FPath write FPath;
  public
    constructor Create; override;
    function getPacket: BYTES; override;
  end;

  { TCMForwardOpenSuccessfulReplyPDU }

  TCMForwardOpenSuccessfulReplyPDU = class(TForwardOpenReplyPDU)
  private
    FAppReply: BYTES;
    FAppRplySize: Byte;
    FConnSerialNumber: WORD;
    FOriginatorSerialNumber: LONGWORD;
    FOriginatorVendorID: WORD;
    FOT_API: LongWord;
    FOT_ConnID: LONGWORD;
    FTO_API: LongWord;
    FTO_ConnID: LONGWORD;
  public
    Property OT_ConnID:LONGWORD read FOT_ConnID; //originator's CIP Produced session ID
    Property TO_ConnID:LONGWORD read FTO_ConnID; //originator's CIP consumed session ID
    Property ConnSerialNumber:WORD read FConnSerialNumber;// session serial number
    Property OriginatorVendorID:WORD read FOriginatorVendorID;
    Property OriginatorSerialNumber:LONGWORD read FOriginatorSerialNumber;
    Property OT_API:LongWord read FOT_API;
    Property TO_API:LongWord read FTO_API;
    Property ApplicationReplySize:Byte read FAppRplySize;
    //Property Reserved:Byte;
    property AppReply:BYTES read FAppReply;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
  end;

  { TCMForwardOpenFailureReplyPDU }

  TCMForwardOpenFailureReplyPDU = class(TForwardOpenReplyPDU)
  private
    FConnSerialNumber: WORD;
    FOriginatorSerialNumber: LONGWORD;
    FOriginatorVendorID: WORD;
    FRemainingPathSize: Byte;
  public
    Property ConnSerialNumber:WORD read FConnSerialNumber;// session serial number
    Property OriginatorVendorID:WORD read FOriginatorVendorID;
    Property OriginatorSerialNumber:LONGWORD read FOriginatorSerialNumber;
    property RemainingPathSize:Byte read FRemainingPathSize;
    //Property Reserved:Byte;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
  end;

  { TEncodeOnlyCIPPDU }

  TEncodeOnlyCIPPDU = class(TCIPPDU)
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
    class function CreatePDUClassFromBytes(aBytes:BYTES; var newOffset:Integer;
                                           offset:Integer; OptData:Integer = 0):TPDU; override;
  end;

  { TDecodeOnlyCIPPDU }

  TDecodeOnlyCIPPDU = class(TCIPPDU)
  public
    constructor Create; override;
    function getPacket: BYTES; override;
  end;


  { TCIP_ForwardOpen_Req }
  TForwardOpenTarget = (CONNECTION_MANAGER,
                        ROUTER,
                        BACKPLANE_DATA,
                        OBJECT_PCCC,
                        OBJECT_DHP_A,
                        OBJECT_DHP_B);

  TCIP_ForwardOpen_Req = class(TEncodeOnlyCIPPDU)
  private
    FFOTarget: TForwardOpenTarget;
    function GetFOPDU: TForwardOpenReqPDU;
    procedure SetFOTarget(AValue: TForwardOpenTarget);
  public
    constructor Create; override;
    function getPacket: BYTES; override;
    property ForwardOpenTarget:TForwardOpenTarget read FFOTarget write SetFOTarget;
    property ForwardOpenPDU:TForwardOpenReqPDU read GetFOPDU;
  end;

  { TCIP_ForwardOpen_Reply }

  TCIP_ForwardOpen_Reply = class(TDecodeOnlyCIPPDU)
  private
    FAditionalStatus: BYTES;
    FStatus: Byte;
    function GetFORplyPDU: TForwardOpenReplyPDU;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData:Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
    property Status:Byte read FStatus;
    property AditionalStatus:BYTES read FAditionalStatus;
    property ForwardOpenReplyPDU:TForwardOpenReplyPDU read GetFORplyPDU;
  end;

  { TCIPReadTagReq }

  TCIPReadTagReq = class(TEncodeOnlyCIPPDU)
  private
    FReqCount: Word;
    FReqPathData: BYTES;
    function GetReqPath: String;
    procedure SetReqPath(AValue: String);
  public
    function getPacket: BYTES; override;
  public
    property ReqPathData:BYTES read FReqPathData write FReqPathData;
    property RequestPath:String read GetReqPath write SetReqPath;
    property ReqCount:Word read FReqCount write FReqCount;
  end;

  { TCIPReadTagStructInfoReq }

  TCIPReadTagStructInfoReq = class(TEncodeOnlyCIPPDU)
  private
    FNumBytesToRead: Word;
    FOffset: LongWord;
    FStructID: Word;
  public
    function getPacket: BYTES; override;
  public
    property StructID:Word read FStructID write FStructID;
    property Offset:LongWord read FOffset write FOffset;
    property NumBytesToRead:Word read FNumBytesToRead write FNumBytesToRead;
  end;

  TDynWordArray = array of Word;

  { TCIPReadTagRply }

  TCIPReadTagRply = class(TDecodeOnlyCIPPDU)
  private
    FData: BYTES;
    FExtendedStatus:TDynWordArray;
    FDataType, FStructHandler: Word;
    FExtendedStatusCount: Byte;
    FReplyData: BYTES;
    FStatus: Byte;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData: Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
  public
    function IsStructure:Boolean;
    property StructHandle: Word read FDataType;
    property Status:Byte read FStatus;
    property ExtendedStatusCount:Byte read FExtendedStatusCount;
    property ExtendedStatus:TDynWordArray read FExtendedStatus;
    property DataType:Word read FDataType;
    property Data:BYTES read FData;
    property ReplyData:BYTES read FReplyData;
  end;

  TAttrList = specialize TFPGList<Word>;

  { TCIPGetAttrListSrvReq }

  TCIPGetAttrListSrvReq = class(TEncodeOnlyCIPPDU)
  private
    FStructID: Word;
    FAttrList:TAttrList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function  getPacket: BYTES; override;
    function  AddAttr(aAttr:Word):Integer;
    procedure Delete(aIndex:Integer);
    procedure DeleteAttr(aAttr:Word);
    property  StructID:Word read FStructID write FStructID;
  end;

  { TAttrInfo }

  TAttrInfo = record
    Attribute:Word;
    Status:Word;
    Value:LongWord;

    class operator = (a, b: TAttrInfo) r: Boolean;
  end;

  TRepliedAttrList = specialize TFPGList<TAttrInfo>;

  { TCIPGetAttrListSrvRply }

  TCIPGetAttrListSrvRply = class(TDecodeOnlyCIPPDU)
  private
    FExtendedStatus: TDynWordArray;
    FExtendedStatusCount: Byte;
    FReadAttrCnt: Word;
    FRepliedAttrList:TRepliedAttrList;
    FStatus: Byte;
    function GetAttr(index: Integer): TAttrInfo;
    function GetAttrCount: Integer;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData: Integer=0;
                                CreateChildrenPDUs: Boolean=true); override;
    destructor Destroy; override;
  public
    property Status:Byte read FStatus;
    property ExtendedStatusCount:Byte read FExtendedStatusCount;
    property ExtendedStatus:TDynWordArray read FExtendedStatus;
    property ReadAttrCount:Word read FReadAttrCnt;
    property DecodedAtrrCount:Integer read GetAttrCount;
    property DecodedAttr[index:Integer]:TAttrInfo read GetAttr;
  end;

  { TCIPMultiTagReadReq }

  TCIPMultiTagReadReq = class(TEncodeOnlyCIPPDU)
  private type
    TReqList = specialize TFPGObjectList<TCIPReadTagReq>;
  private
    FGettingSize:Boolean;
    ReqList:TReqList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function getPacket: BYTES; override;
    function GetPacketSize:Integer;
    function AddItem(aTagPath:String; ItemCount:Word):Integer; overload;
    function AddItem(aTagPath:BYTES; ItemCount:Word):Integer; overload;

    procedure Delete(aIndex:Integer);
    function ReqCount:Integer;
  end;

  TCIPmultiRplyList = specialize TFPGObjectList<TCIPReadTagRply>;

  { TCIPMultiTagReadRply }

  TCIPMultiTagReadRply = class(TDecodeOnlyCIPPDU)
  private
    FExtendedStatus: TDynWordArray;
    FExtendedStatusCount: Byte;
    FReplyCount: Word;
    FReplyList:TCIPmultiRplyList;
    FStatus: Byte;
    function GetReply(index: Integer): TCIPReadTagRply;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData: Integer=0;
                                CreateChildrenPDUs:Boolean = true); override;
    destructor Destroy; override;
    property Status:Byte read FStatus;
    property ExtendedStatusCount:Byte read FExtendedStatusCount;
    property ExtendedStatus:TDynWordArray read FExtendedStatus;
    Property ReplyCount:Word read FReplyCount;
    property Reply[index:Integer]:TCIPReadTagRply read GetReply;
  end;

  TCIPReadTagFragReq = class(TEncodeOnlyCIPPDU)
  private
    FReqOffset: LongWord;
    FReqCount: Word;
    FReqPathData: BYTES;
    function GetReqPath: String;
    procedure SetReqPath(AValue: String);
  public
    function getPacket: BYTES; override;
  public
    property ReqPathData:BYTES read FReqPathData write FReqPathData;
    property RequestPath:String read GetReqPath write SetReqPath;
    property ReqCount:Word read FReqCount write FReqCount;
    property ReqOffset:LongWord read FReqOffset write FReqOffset;
  end;

  { TCIPReadTagFragRply }

  TCIPReadTagFragRply = class(TDecodeOnlyCIPPDU)
  private
    FData: BYTES;
    FExtendedStatus:TDynWordArray;
    FDataType, FStructHandler: Word;
    FExtendedStatusCount: Byte;
    FStatus: Byte;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData: Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
  public
    function IsStructure:Boolean;
    property StructHandle: Word read FDataType;
    property Status:Byte read FStatus;
    property ExtendedStatusCount:Byte read FExtendedStatusCount;
    property ExtendedStatus:TDynWordArray read FExtendedStatus;
    property DataType:Word read FDataType;
    property Data:BYTES read FData;
  end;

  { TCIPWriteTagFragReq }

  TCIPWriteTagFragReq = class(TEncodeOnlyCIPPDU)
  private
    FDataCRC: Word;
    FDataType: Word;
    FReqOffset: LongWord;
    FReqPathData: BYTES;
    FReqCount: Word;
    FWriteReqData: BYTES;
    function GetReqPath: String;
    procedure SetReqPath(AValue: String);
  public
    function getPacket: BYTES; override;
    property ReqPathData:BYTES read FReqPathData write FReqPathData;
    property RequestPath:String read GetReqPath write SetReqPath;
    property DType:Word read FDataType write FDataType;
    property DCRC:Word read FDataCRC write FDataCRC;
    property ReqCount:Word read FReqCount write FReqCount;
    property ReqOffset:LongWord read FReqOffset write FReqOffset;
    property WriteReqData:BYTES read FWriteReqData write FWriteReqData;
  end;

  { TCIPWriteTagFragRply }

  TCIPWriteTagFragRply = class(TDecodeOnlyCIPPDU)
  private
    FExtendedStatus: TDynWordArray;
    FExtendedStatusCount: Byte;
    FStatus: Byte;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData: Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
  public
    property Status:Byte read FStatus;
    property ExtendedStatusCount:Byte read FExtendedStatusCount;
    property ExtendedStatus:TDynWordArray read FExtendedStatus;
  end;



  { TCIPListTagsReq }

  TCIPListTagsReq = class(TEncodeOnlyCIPPDU)
  private
    FFromInstance: Word;
    FFromProgram: String;
  public
    function getPacket: BYTES; override;
  public
    Property FromProgram:String read FFromProgram write FFromProgram;
    Property FromInstance:Word read FFromInstance write FFromInstance;
  end;

  { TLGXTag }

  TLGXTag = record
    instance_id:LongInt;
    name:String;
    aType,
    elem_size:Integer;
    elem_count,
    num_dimensions:Word;
    dimensions:array[0..2] of LongWord;
    class operator = (a, b: TLGXTag) r: Boolean;
  end;

  TLGXTagList = specialize TFPGList<TLGXTag>;
  TLGXProgramList = specialize TFPGList<string>;
  TLGXTagMap = specialize TFPGMap<String, TLGXTag>;

  //: Cópia da lista de tags lida do CLP, usada pelo Tag Builder.
  TLGXTagInfoArray = array of TLGXTag;

  { TCIPListTagsRply }

  TCIPListTagsRply = class(TDecodeOnlyCIPPDU)
  public
  private
    FExtendedStatus: TDynWordArray;
    FExtendedStatusCount: Byte;
    FStatus: Byte;
    FTagList:TLGXTagList;
    function GetLGXTag(index: Integer): TLGXTag;
    function GetTagCount: Integer;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData: Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
    destructor Destroy; override;
  public
    property Status:Byte read FStatus;
    property ExtendedStatusCount:Byte read FExtendedStatusCount;
    property ExtendedStatus:TDynWordArray read FExtendedStatus;
    property LGXTagCount:Integer read GetTagCount;
    property LGXTag[index:Integer]:TLGXTag read GetLGXTag;
  end;

  { TCIPWriteTagReq }

  TCIPWriteTagReq = class(TEncodeOnlyCIPPDU)
  private
    FDataCRC: Word;
    FDataType: Word;
    FReqCount: Word;
    FReqPathData: BYTES;
    FWriteReqData: BYTES;
    function GetReqPath: String;
    procedure SetReqPath(AValue: String);
  public
    function getPacket: BYTES; override;
  public
    property ReqPathData:BYTES read FReqPathData write FReqPathData;
    property RequestPath:String read GetReqPath write SetReqPath;
    property DType:Word read FDataType write FDataType;
    property DCRC:Word read FDataCRC write FDataCRC;
    property ReqCount:Word read FReqCount write FReqCount;
    property WriteReqData:BYTES read FWriteReqData write FWriteReqData;
  end;

  { TCIPWriteTagRply }

  TCIPWriteTagRply = class(TDecodeOnlyCIPPDU)
  private
    FExtendedStatus: TDynWordArray;
    FExtendedStatusCount: Byte;
    FStatus: Byte;
  public
    constructor CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
                                offset: Integer; OptData: Integer = 0;
                                CreateChildrenPDUs:Boolean = true); override;
  public
    property Status:Byte read FStatus;
    property ExtendedStatusCount:Byte read FExtendedStatusCount;
    property ExtendedStatus:TDynWordArray read FExtendedStatus;
  end;

  //============================================================================

  { TLPTTagRec }

  TLPTTagRec = record
  private
    class operator initialize(var aRec:TLPTTagRec);
    class operator finalize(var aRec:TLPTTagRec);
    class operator Copy(constref aSrc: TLPTTagRec; var aDst: TLPTTagRec);
  public
    mm:TPLCMemoryManager;
    ExistsOnPLC:Boolean;
  end;
  PLPTTagRec = ^TLPTTagRec;

  TLPTTagMap = specialize TFPGMap<AnsiString, PLPTTagRec>;

  { TScanInfoItem }

  TScanInfoItem = record
    LastUpdate:TDateTime;

    Tag:AnsiString;

    Index,
    UpdateRate,
    Size:LongInt;
    Read,
    NeedUpdate:Boolean;
    class operator = (a, b: TScanInfoItem) r: Boolean;
  end;
  PScanInfoItem = ^TScanInfoItem;

  TScanList = specialize TFPGList<TScanInfoItem>;

  { Tudt_field_entry }

  TUDT_field_entry = record
    name:String;
    aType   :cuint16;
    metadata:cuint16;
    size  :cint32;
    offset:cint32;
    class operator = (a, b: Tudt_field_entry) r: Boolean;
  end;
  PUDT_field_entry = ^TUDT_field_entry;

  TUDTFieldList = specialize TFPGList<Pudt_field_entry>;

  { Tudt_entry }

  TUDT_entry = record
  private
    class operator initialize(var aRec:TUDT_entry);
    class operator finalize(var aRec:TUDT_entry);
    class operator Copy(constref aSrc: TUDT_entry; var aDst: TUDT_entry);
  public
    name:String;
    id,
    checksum     :cuint16;
    num_fields   :cuint16;
    definition_size,
    instance_size:cuint32;
    fields:TUDTFieldList;
  end;
  PUDT_entry = ^TUDT_entry;

  TUDTID = 0..$FFF;

  TUDTList = specialize TFPGMap<TUDTID,PUDT_entry>;

  //============================================================================

  { TLGXDriver }

  TLGXDriver = class(TProtocolDriver)
  private
    FCIPSession:LongWord;
    FConnectionID:LongWord;
  protected
    function  NotifyThisEvents: TNotifyThisEvents; override;
    procedure PortClosed(Sender: TObject); override;
    procedure PortDisconnected(Sender: TObject); override;
  protected
    CIPPkgID:Word;
    FTagList:TLPTTagMap;
    FLastCmdSentTimstamp:TDateTime;
    function  GetConnID:LongWord;
    function  GetConnSerialNumber:word;
    function  GetOriginatorSerialNumber:LongWord;

    function  OpenSession:LongWord;
    function  GetConnectionID:LongWord;
    procedure LoadLGXTagList;
    function  SendConnectedDataItem(var aCIPPDU:TCIPPDU):TPDU;
    procedure SendPingCmd;
  protected
    FTerminating:Integer;
    function  TestDone:Boolean;
    procedure BuildTagRec(aTag:AnsiString; aTagSize:Integer; out tr:TTagRec);
    function  ValidTagStr(ATagStr:String):Boolean;
    function  GetTagProperts(TagObj: TTag; var aTagPath: String; var aSize, aRefreshRate:Integer): Boolean;

    procedure DoAddTag(TagObj: TTag; TagValid: Boolean); override;
    procedure DoDelTag(TagObj: TTag); override;
    procedure DoGetValue(TagRec: TTagRec; var values: TScanReadRec); override;
    procedure DoScanRead(Sender: TObject; var NeedSleep: LongInt); override;
    function  DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble;
                     Sync: Boolean): TProtocolIOResult; override;
    function  DoWrite(const tagrec: TTagRec; const Values: TArrayOfDouble;
                     Sync: Boolean): TProtocolIOResult; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SizeOfTag(aTag: TTag; isWrite: Boolean; var ProtocolTagType:
                       TProtocolTagType): BYTE; override;
    function TagListLoaded:Boolean;

    function LastTagListed:String;

    {:
    Navega a lista de tags do CLP (tags de controlador, tags de programa,
    membros de UDTs e elementos de arrays) e devolve uma cópia dela.

    @bold(A porta de comunicação precisa estar ativa). Se CommunicationPort
    for @nil ou estiver inativa, o método retorna @false sem tentar nada. Como
    o TTCP_UDPPort não é um dispositivo exclusivo, ele conecta de verdade
    quando Active é ligado em tempo de projeto, e por isso este método também
    funciona dentro da IDE (é assim que o Tag Builder lê a lista de tags).

    @bold(A chamada é síncrona e bloqueante): ela pausa as threads de scan do
    driver, conversa com o CLP e só retorna quando a navegação termina. Num
    CLP com muitos tags isso leva vários segundos, e a thread que chamou fica
    parada esse tempo todo - em tempo de projeto, é a IDE que fica parada.

    A lista fica em cache por porta de comunicação: a primeira chamada navega
    o CLP e as seguintes devolvem a mesma lista, até que ForceReload seja
    @true ou ClearTagListCache seja chamado.

    @param(aTagList TLGXTagInfoArray. Recebe a cópia da lista de tags do CLP.)
    @param(ForceReload Boolean. Se @true, descarta a lista lida anteriormente
           e navega o CLP novamente.)
    @returns(@true se a lista de tags foi lida por completo. @false se a porta
             está inativa, se a conexão CIP não pôde ser aberta, ou se a
             navegação foi interrompida no meio - nesse último caso aTagList
             traz os tags que deram tempo de ser lidos.)
    @seealso(ClearTagListCache)
    }
    function BrowseTagList(out aTagList:TLGXTagInfoArray;
                           ForceReload:Boolean = false):Boolean;

    //: Descarta a lista de tags lida do CLP, forçando uma nova leitura.
    procedure ClearTagListCache;

    //: @seealso(TProtocolDriver.OpenTagEditor)
    procedure OpenTagEditor(InsertHook:TAddTagInEditorHook;
                            CreateProc:TCreateTagProc); override;
    //: @seealso(TProtocolDriver.HasTabBuilderEditor)
    function  HasTabBuilderEditor:Boolean; override;
  published
    property ReadSomethingAlways;
    property ReadOnly;
  end;



const
  TAG_STRING_SIZE = 200;

  maxPDUSize = 490;

  TYPE_IS_SYSTEM:Word = $1000;
  TYPE_IS_STRUCT:word = $8000;
  TAG_DIM_MASK:word   = $6000;

  TAG_CIP_TYPE_BOOL        = $00C1; //* BOOLEAN */
  TAG_CIP_TYPE_SINT        = $00C2; //* Signed 8-bit integer value */
  TAG_CIP_TYPE_INT         = $00C3; //* Signed 16-bit integer value */
  TAG_CIP_TYPE_DINT        = $00C4; //* Signed 32-bit integer value */
  TAG_CIP_TYPE_LINT        = $00C5; //* Signed 64-bit integer value */
  TAG_CIP_TYPE_USINT       = $00C6; //* Unsigned 8-bit integer value */
  TAG_CIP_TYPE_UINT        = $00C7; //* Unsigned 16-bit integer value */
  TAG_CIP_TYPE_UDINT       = $00C8; //* Unsigned 32-bit integer value */
  TAG_CIP_TYPE_ULINT       = $00C9; //* Unsigned 64-bit integer value */
  TAG_CIP_TYPE_REAL        = $00CA; //* 32-bit floating point value, IEEE format */
  TAG_CIP_TYPE_LREAL       = $00CB; //* 64-bit floating point value, IEEE format */
  TAG_CIP_TYPE_STRING      = $00D0; //* 88-byte string, with 82 bytes of data, 4-byte count and 2 bytes of padding */
  TAG_CIP_TYPE_DWORD       = $00D3; //* Unsigned 32-bit integer value */

//: Diz se o tipo CIP informado é uma estrutura (UDT).
function LGXTypeIsStruct(aCIPType:Word):Boolean;

//: Diz se o tipo CIP informado é um tipo interno/reservado do CLP.
function LGXTypeIsSystem(aCIPType:Word):Boolean;

//: Retorna o número de dimensões (0..3) codificado no tipo CIP.
function LGXTypeDimensions(aCIPType:Word):Byte;

//: Retorna o nome do tipo CIP, como visto no Studio 5000/RSLogix.
function LGXTypeName(aCIPType:Word):String;

{:
Converte um tipo CIP no tipo de tag equivalente do PascalSCADA.
@returns(@false se o tipo CIP não tem equivalente numérico, caso de estruturas.)
}
function LGXTypeToTagType(aCIPType:Word; out aTagType:TTagType):Boolean;

//: Retorna o tamanho, em bytes, de um elemento do tipo CIP informado.
function LGXTypeSizeInBytes(aCIPType:Word):Integer;

{:
Registra a ferramenta Tag Builder dos CLPs Rockwell Compact/ControlLogix.
@seealso(TLGXDriver.OpenTagEditor)
}
procedure SetTagBuilderToolForRockwellLogixProtocol(TagBuilderTool:TOpenTagEditor);

implementation

{/$DEFINE TEST}

uses syncobjs, CommPort, pascalScadaMTPCPU;

function EncodeTagPath(aPath:String):BYTES;
  function EncodeStringSeg(aStr:String):BYTES;
  var
    c: Integer;
  begin
    SetLength(Result,Length(aStr) + (Length(aStr) MOD 2) + 2);
    //zera antes de escrever: o FPC reaproveita o buffer do destino entre as
    //chamadas, e sem isto o byte de enchimento de um nome de tamanho impar
    //saia com um caractere do segmento codificado antes dele.
    //clear before writing: FPC reuses the destination buffer across calls, and
    //without this the pad byte of an odd length name came out carrying a
    //character from the segment encoded before it.
    FillChar(Result[0], Length(Result), 0);
    Result[0]:=$91;
    Result[1]:=Length(aStr);
    for c:=1 to Length(aStr) do
      Result[c+1]:=Ord(aStr[c]);
  end;

var
  c, p, h, d: Integer;
  strParts, strParts2: hsutils.TStringArray;
  aux: BYTES;
  index: LongInt;
  i: SizeInt;
begin
  if Length(aPath)<200 then begin
    strParts:=ExplodeString('.',aPath);
    for p:=0 to Length(strParts)-1 do begin
      if Pos('[', strParts[p])=0 then begin
        aux:=EncodeStringSeg(strParts[p]);
        if Length(aux)>0 then begin
          h:=Length(Result);
          SetLength(Result,h+Length(aux));
          Move(aux[0],Result[h],Length(aux));
        end;
      end else begin
        //array index
        strParts2:=ExplodeString('[', strParts[p]);
        aux:=EncodeStringSeg(strParts2[0]);
        if Length(aux)>0 then begin
          h:=Length(Result);
          SetLength(Result,h+Length(aux));
          Move(aux[0],Result[h],Length(aux));
        end;

        strParts2:=ExplodeString(']', strParts2[1]);
        strParts2:=ExplodeString(',', strParts2[0]);
        for d:=0 to Length(strParts2)-1 do begin
          index:=StrToIntDef(strParts2[d],65536);
          if index=65536 then
            writeln('FIX-ME: Out of bounds? ',{$i %FILE%},':',{$i %LINE%});

          case index of
            0..255: begin
              h:=Length(Result);
              SetLength(Result,h+2);
              Result[h+0]:=$28;
              Result[h+1]:=index;
            end;
            else begin
              h:=Length(Result);
              SetLength(Result,h+4);
              Result[h+0]:=$29;
              Result[h+1]:=$00;
              PWord(@Result[h+2])^:=index;
            end;
          end;
        end;
      end;
    end;
  end else
    raise Exception.Create('Tag path name too long!');
end;

function DecodeTagPath(aEncPath: BYTES): String;
begin
  //TODO: Decodificar o indecodificável?
end;

{ TEIPListServicesData }

function TEIPListServicesData.GetItem(Index: Integer): TListServicesResponse;
begin
  if Index>=FEncapsuledPDUs.Count then
    raise exception.Create('Out of bounds');

  result:=FEncapsuledPDUs.Items[index] as TListServicesResponse;
end;

function TEIPListServicesData.getPacket: BYTES;
begin
  raise Exception.Create('Can''t get ListService Data packet, it''s a response packet');
end;

constructor TEIPListServicesData.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  inherited CreateFromBytes(aBytes, newOffset, offset, OptData,
    CreateChildrenPDUs);

  if (Length(aBytes)-offset)<2 then
    raise exception.Create('List session reply data must have 2 bytes');

  FItemCount :=PWord    (@aBytes[Offset + 0])^;

  newOffset:=offset+2;

  if FItemCount>0 then begin
    repeat
      offset:=newOffset;
      FEncapsuledPDUs.Add(TListServicesResponse.CreateFromBytes(aBytes,newOffset,offset));
    until newOffset>=Length(aBytes);
  end;
end;

function TEIPListServices.GetListServicesData: TEIPListServicesData;
begin
  if FEncapsuledPDUs.Count=1 then
    Result := FEncapsuledPDUs.Items[0] as TEIPListServicesData
  else
    Result := nil;
end;

constructor TEIPListServices.Create;
begin
  inherited Create;
  FCommand:=$0004;
  FLength:=0;
  FSessionHandler:=0;
  FStatus:=0;
  FSenderContext:=0;
  FOptions:=0;
end;

{ TListServicesResponse }

function TListServicesResponse.getPacket: BYTES;
begin
  raise Exception.Create('Can''t get ListService response packet, it''s a response packet');
end;

constructor TListServicesResponse.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  if (Length(aBytes)-offset)<24 then begin
    raise exception.Create('List session reply data must have 24 bytes');

  end;
  FType                := PWord    (@aBytes[Offset + 0])^;
  FLen                 := PWord    (@aBytes[Offset + 2])^;

  if FLen<>20 then
    raise Exception.Create('Wrong List Service response length');

  FEncapsulationVersion:= PWord    (@aBytes[Offset + 4])^;
  FCapabilities.WAccess:= PWord    (@aBytes[Offset + 6])^;
  Move(aBytes[Offset + 8],FServiceName[0],16);
  newOffset:=offset+24;
end;

{ TCIPWriteTagRply }

constructor TCIPWriteTagRply.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  limit, Idx: Integer;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset, OptData);
  if (Length(aBytes)-offset)<4 then begin
    raise exception.Create('CIP Tag Write Srv should have at least 4 bytes of size');
  end;

  if (aBytes[offset+0]<>$CD) and (aBytes[offset+1]<>0) then
    raise exception.Create('CIP Tag Write Srv Reply inconsistency');

  FStatus:=aBytes[offset+2];
  FExtendedStatusCount:=aBytes[offset+3];
  newOffset:=offset+4;

  //if not (FStatus in [$00, $06]) then
  //  writeln('FIX-ME: got error ',IntToHex(FStatus),'. ',{$i %FILE%},':',{$i %LINE%});

  {$IFDEF LGXDEBUG}
  if FStatus=$04 then
    writeln('FIX-ME: path encode error on request. ',{$i %FILE%},':',{$i %LINE%});
  {$ENDIF}

  if FExtendedStatusCount=0 then exit;

  SetLength(FExtendedStatus, FExtendedStatusCount);

  if OptData<=0 then
    limit:=Length(aBytes)
  else
    limit:=OptData;

  if limit>Length(aBytes) then
    limit:=Length(aBytes);

  Idx:=0;
  //read extended status
  while (newOffset<limit) and (Idx<FExtendedStatusCount) do begin
    FExtendedStatus[Idx]:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    inc(Idx);
  end;
end;

{ TCIPWriteTagReq }

function TCIPWriteTagReq.GetReqPath: String;
begin
  Result:=DecodeTagPath(FReqPathData);
end;

procedure TCIPWriteTagReq.SetReqPath(AValue: String);
begin
  FReqPathData:=EncodeTagPath(AValue);
end;

function TCIPWriteTagReq.getPacket: BYTES;
var
  extraoffset: Integer;
  isStruct: Boolean;
begin
  isStruct:=(FDataType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT;
  SetLength(Result, 6+Length(FReqPathData)+Length(FWriteReqData)+IfThen(isStruct,2,0));
  Result[0]:=$4d;
  Result[1]:=Length(FReqPathData) div 2;
  Move(fReqPathData[0],Result[2],Length(FReqPathData));
  extraoffset:=0;
  if (isStruct) then begin //extracted from libplctag, why?
    PWord(@Result[2+Length(FReqPathData)])^:=$02a0;
    extraoffset:=2;
  end;
  PWord               (@Result[2+extraoffset+Length(FReqPathData)])^:=IfThen(isStruct,FDataCRC,FDataType);
  PWord               (@Result[4+extraoffset+Length(FReqPathData)])^:=FReqCount;
  Move(FWriteReqData[0],Result[6+extraoffset+Length(FReqPathData)],Length(FWriteReqData));
end;

{ Tudt_entry }

class operator Tudt_entry.initialize(var aRec: Tudt_entry);
begin
  aRec.fields:=TUDTFieldList.Create;
end;

class operator Tudt_entry.finalize(var aRec: Tudt_entry);
begin
  if Assigned(aRec.fields) then
    FreeAndNil(aRec.fields);
end;

class operator Tudt_entry.Copy(constref aSrc: Tudt_entry; var aDst: Tudt_entry);
begin
  ADst.name            := aSrc.name         ;
  ADst.id              := aSrc.id           ;
  ADst.checksum        := aSrc.checksum     ;
  ADst.num_fields      := aSrc.num_fields   ;
  ADst.definition_size := aSrc.definition_size;
  ADst.instance_size   := aSrc.instance_size;
  aDst.fields.Assign(aSrc.fields);
end;

{ TUDT_field_entry }

class operator TUDT_field_entry.=(a, b: Tudt_field_entry)r: Boolean;
begin
  r:=(a.name     = b.name    ) and
     (a.aType    = b.aType   ) and
     (a.metadata = b.metadata) and
     (a.size     = b.size    ) and
     (a.offset   = b.offset  );
end;

{ TAttrInfo }

class operator TAttrInfo.=(a, b: TAttrInfo)r: Boolean;
begin
  r:=(a.Attribute = b.Attribute) and
     (a.Status    = b.Status   ) and
     (a.Value     = b.Value    ) ;
end;

{ TCIPGetAttrListSrvRply }

function TCIPGetAttrListSrvRply.GetAttrCount: Integer;
begin
 result:=0;
 if Assigned(FRepliedAttrList) then
   Result:=FRepliedAttrList.Count;
end;

function TCIPGetAttrListSrvRply.GetAttr(index: Integer): TAttrInfo;
begin
  if Assigned(FRepliedAttrList) then
    Result:=FRepliedAttrList.Items[index];
end;

constructor TCIPGetAttrListSrvRply.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  infoRec:TAttrInfo;
  limit, Idx: Integer;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset, OptData,
    CreateChildrenPDUs);

  FRepliedAttrList:=TRepliedAttrList.Create;

  if (Length(aBytes)-offset)<6 then begin
    raise exception.Create('CIP GetAttrList Srv should have at least 6 bytes of size');
  end;

  if (aBytes[offset+0]<>$83) and (aBytes[offset+1]<>0) then
    raise exception.Create('CIP GetAttrList Reply inconsistency');

  FStatus:=aBytes[offset+2];
  FExtendedStatusCount:=aBytes[offset+3];
  SetLength(FExtendedStatus, FExtendedStatusCount);

  {$IFDEF LGXDEBUG}
  if FStatus=$04 then
    writeln('FIX-ME: path encode error on request. ',{$i %FILE%},':',{$i %LINE%});
  {$ENDIF}

  //if not (FStatus in [$00, $06]) then
  //  writeln('FIX-ME: got error ',IntToHex(FStatus),'. ',{$i %FILE%},':',{$i %LINE%});

  newOffset:=offset+4;
  if OptData<=0 then
    limit:=Length(aBytes)
  else
    limit:=OptData;

  if limit>Length(aBytes) then
    limit:=Length(aBytes);

  Idx:=0;
  //read extended status
  while (newOffset<limit) and (Idx<FExtendedStatusCount) do begin
    FExtendedStatus[Idx]:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    inc(Idx);
  end;

  if FStatus<>0 then begin
    writeln('FIX-ME: CIP Get Attr list service got error ',IntToHex(FStatus),'. ',{$i %FILE%},':',{$i %LINE%});
    exit; //TODO: Ver?
  end;

  FReadAttrCnt:=Pword(@aBytes[newOffset])^;
  inc(newOffset,2);

  FRepliedAttrList.Clear;
  while (newOffset<limit) and (FRepliedAttrList.Count<FReadAttrCnt) do begin
    infoRec.Attribute := Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    infoRec.Status    := Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    infoRec.Value:=0;
    case infoRec.Attribute of
      1: begin infoRec.Value:=Pword    (@aBytes[newOffset])^; inc(newOffset,2); end;
      2: begin infoRec.Value:=Pword    (@aBytes[newOffset])^; inc(newOffset,2); end;
      4: begin infoRec.Value:=PLongWord(@aBytes[newOffset])^; inc(newOffset,4); end;
      5: begin infoRec.Value:=PLongWord(@aBytes[newOffset])^; inc(newOffset,4); end;
      else writeln('FIX-ME: unknown attribute ',infoRec.Attribute,' ',{$i %FILE%},':',{$i %LINE%});
    end;
    FRepliedAttrList.Add(infoRec);
  end;
end;

destructor TCIPGetAttrListSrvRply.Destroy;
begin
  FRepliedAttrList.Destroy;
  inherited Destroy;
end;

{ TCIPGetAttrListSrvReq }

constructor TCIPGetAttrListSrvReq.Create;
begin
  inherited Create;
  FAttrList:=TAttrList.Create;
end;

destructor TCIPGetAttrListSrvReq.Destroy;
begin
  FAttrList.Free;
  inherited Destroy;
end;

function TCIPGetAttrListSrvReq.getPacket: BYTES;
var
  i: Integer;
begin
  if assigned(FAttrList) then begin
    SetLength(Result,10+2*FAttrList.Count);
    Result[0]:=$03;
    Result[1]:=$03;
    Result[2]:=$20;
    Result[3]:=$6c;
    Result[4]:=$25;
    Result[5]:=$00;
    Pword(@result[6])^ := FStructID;
    Pword(@result[8])^ := FAttrList.Count;

    for i:=0 to FAttrList.Count-1 do begin
      Pword(@result[10+i*2])^ := FAttrList.Items[i];
    end;

  end;
end;

function TCIPGetAttrListSrvReq.AddAttr(aAttr: Word): Integer;
var
  idx: Integer;
begin
  Result:=-1;
  if Assigned(FAttrList) then begin
    idx:=FAttrList.IndexOf(aAttr);
    if (idx<0) then
      Result:=FAttrList.Add(aAttr)
    else
      Result:=idx;
  end;
end;

procedure TCIPGetAttrListSrvReq.Delete(aIndex: Integer);
begin
  if Assigned(FAttrList) then begin
    FAttrList.Delete(aIndex);
  end;
end;

procedure TCIPGetAttrListSrvReq.DeleteAttr(aAttr: Word);
begin
  if Assigned(FAttrList) then begin
    FAttrList.Remove(aAttr);
  end;
end;

{ TCIPReadTagStructInfoReq }

function TCIPReadTagStructInfoReq.getPacket: BYTES;
begin
  SetLength(Result, 14);
  Result[0]:=$4c;
  Result[1]:=$03;
  Result[2]:=$20;
  Result[3]:=$6c;
  Result[4]:=$25;
  Result[5]:=$00;
  Pword(@result[6])^ := FStructID;
  PLongWord(@Result[8])^ := FOffset;
  Pword(@result[12])^ := FNumBytesToRead;
end;

{ TScanInfoItem }

class operator TScanInfoItem.=(a, b: TScanInfoItem)r: Boolean;
begin
  r:=(a.LastUpdate = b.LastUpdate) and
     (a.Tag        = b.Tag       ) and
     (a.Index      = b.Index     ) and
     (a.UpdateRate = b.UpdateRate) and
     (a.Size       = b.Size      );

end;

{ TCIPListTagsRply }

function TCIPListTagsRply.GetLGXTag(index: Integer): TLGXTag;
begin
  FillByte(Result,SizeOf(Result), 0);
  if Assigned(FTagList) and (index<FTagList.Count) then
    Result:=FTagList.Items[index];
end;

function TCIPListTagsRply.GetTagCount: Integer;
begin
  Result:=-1;
  if Assigned(FTagList) then
    Result:=FTagList.Count;
end;

constructor TCIPListTagsRply.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  tr: TLGXTag;
  Step, limit, Idx: Integer;
  auxStr: String;
  auxSize: Word;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset, OptData);
  if (Length(aBytes)-offset)<4 then begin
    raise exception.Create('CIP GetInstanceAttribute Srv should have at least 4 bytes of size');
  end;

  if (aBytes[offset+0]<>$D5) and (aBytes[offset+1]<>0) then
    raise exception.Create('CIP GetInstanceAttribute Srv Reply inconsistency');

  FTagList:=TLGXTagList.Create;

  FStatus:=aBytes[offset+2];
  FExtendedStatusCount:=aBytes[offset+3];
  newOffset:=offset+4;

  //if not (FStatus in [$00, $06]) then
  //  writeln('FIX-ME: got error ',IntToHex(FStatus),'. ',{$i %FILE%},':',{$i %LINE%});

  {$IFDEF LGXDEBUG}
  if FStatus=$04 then
    writeln('FIX-ME: path encode error on request. ',{$i %FILE%},':',{$i %LINE%});
  {$ENDIF}

  SetLength(FExtendedStatus, FExtendedStatusCount);

  if OptData<=0 then
    limit:=Length(aBytes)
  else
    limit:=OptData;

  if limit>Length(aBytes) then
    limit:=Length(aBytes);

  Idx:=0;
  //read extended status
  while (newOffset<limit) and (Idx<FExtendedStatusCount) do begin
    FExtendedStatus[Idx]:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    inc(Idx);
  end;

  Step:=0;
  while (newOffset<limit) do begin
    if Step=0 then
      FillByte(tr, sizeof(tr), 0);
    case Step of
      0: begin tr.instance_id   := PLongWord(@aBytes[newOffset])^;   inc(newOffset, 4); end;
      1: begin tr.aType         := PWord    (@aBytes[newOffset])^;   inc(newOffset, 2); end;
      2: begin tr.elem_size     := PWord    (@aBytes[newOffset])^;   inc(newOffset, 2); end;
      3: begin tr.dimensions[0] := PLongWord(@aBytes[newOffset])^;   inc(newOffset, 4); end;
      4: begin tr.dimensions[1] := PLongWord(@aBytes[newOffset])^;   inc(newOffset, 4); end;
      5: begin tr.dimensions[2] := PLongWord(@aBytes[newOffset])^;   inc(newOffset, 4); end;
      6: begin
        auxSize:=PWord    (@aBytes[newOffset])^;
        inc(newOffset,2);
        SetLength(auxStr, auxSize);
        Move(aBytes[newOffset],auxStr[1],auxSize);
        inc(newOffset,auxSize);
        tr.name:=auxStr;

        if (tr.elem_size<=0) and ((tr.aType and TYPE_IS_SYSTEM)<>TYPE_IS_SYSTEM)  then begin
          writeln('FIX-ME: NORMAL tag "',auxStr,'" with 0 bytes of size, fixing... ',{$i %FILE%},':',{$i %LINE%});
          tr.elem_size:=1;
        end;
        FTagList.Add(tr);
        Step:=0;
        continue;
      end;
    end;
    inc(Step);
  end;
end;

destructor TCIPListTagsRply.Destroy;
begin
  FTagList.Free;
  inherited Destroy;
end;

{ TCIPListTagsReq }

function TCIPListTagsReq.getPacket: BYTES;
var
  aux: BYTES;
  h: Integer;
begin
  if FFromProgram.Trim<>'' then
    aux:=EncodeTagPath(FFromProgram);

  h:=Length(aux);
  SetLength(aux,h+6);
  aux[h+0]:=$20;
  aux[h+1]:=$6b;
  PWord(@aux[h+2])^ := $25;
  PWord(@aux[h+4])^ := FFromInstance;


  SetLength(Result,12+Length(aux));
  Result[0]:=$55;
  Result[1]:=Length(aux) div 2;
  move(aux[0],Result[2],Length(aux));
  PWord(@Result[ 2+Length(aux)])^ := 4; //4 attributes
  PWord(@Result[ 4+Length(aux)])^ := 2; //attr 2
  PWord(@Result[ 6+Length(aux)])^ := 7; //attr 7
  PWord(@Result[ 8+Length(aux)])^ := 8; //attr 8
  PWord(@Result[10+Length(aux)])^ := 1; //attr 1
end;

{ TLGXTag }

class operator TLGXTag.=(a, b: TLGXTag)r: Boolean;
begin
  result:=
    (a.instance_id    = b.instance_id   ) and
    (a.name           = b.name          ) and
    (a.aType          = b.aType         ) and
    (a.elem_size      = b.elem_size     ) and
    (a.elem_count     = b.elem_count    ) and
    (a.num_dimensions = b.num_dimensions) and
    (a.dimensions[0]  = b.dimensions[0] ) and
    (a.dimensions[1]  = b.dimensions[1] ) and
    (a.dimensions[2]  = b.dimensions[2] );
end;

{ TCIPWriteTagFragRply }

constructor TCIPWriteTagFragRply.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  limit, Idx: Integer;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset, OptData);
  if (Length(aBytes)-offset)<4 then begin
    raise exception.Create('CIP Tag Write Fragmented Srv should have at least 4 bytes of size');
  end;

  if (aBytes[offset+0]<>$D3) and (aBytes[offset+1]<>0) then
    raise exception.Create('CIP Tag Write Fragmented Srv Reply inconsistency');

  FStatus:=aBytes[offset+2];
  FExtendedStatusCount:=aBytes[offset+3];
  newOffset:=offset+4;

  {$IFDEF LGXDEBUG}
  if FStatus=$04 then
    writeln('FIX-ME: path encode error on request. ',{$i %FILE%},':',{$i %LINE%});
  {$ENDIF}

  //if not (FStatus in [$00, $06]) then
  //  writeln('FIX-ME: got error ',IntToHex(FStatus),'. ',{$i %FILE%},':',{$i %LINE%});

  if FExtendedStatusCount=0 then exit;

  SetLength(FExtendedStatus, FExtendedStatusCount);

  if OptData<=0 then
    limit:=Length(aBytes)
  else
    limit:=OptData;

  if limit>Length(aBytes) then
    limit:=Length(aBytes);

  Idx:=0;
  //read extended status
  while (newOffset<limit) and (Idx<FExtendedStatusCount) do begin
    FExtendedStatus[Idx]:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    inc(Idx);
  end;
end;

{ TCIPWriteTagFragReq }

function TCIPWriteTagFragReq.GetReqPath: String;
begin
  Result:=DecodeTagPath(FReqPathData);
end;

procedure TCIPWriteTagFragReq.SetReqPath(AValue: String);
begin
  FReqPathData:=EncodeTagPath(AValue);
end;

function TCIPWriteTagFragReq.getPacket: BYTES;
var
  extraoffset: Integer;
  isStruct: Boolean;
begin
  isStruct:=(FDataType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT;
  SetLength(Result, 10+Length(FReqPathData)+Length(FWriteReqData)+IfThen(isStruct,2,0));
  Result[0]:=$53;
  Result[1]:=Length(FReqPathData) div 2;
  Move(fReqPathData[0],Result[2],Length(FReqPathData));
  extraoffset:=0;
  if (isStruct) then begin //extracted from libplctag, why?
    PWord(@Result[2+Length(FReqPathData)])^:=$02a0;
    extraoffset:=2;
  end;
  PWord               (@Result[2+extraoffset+Length(FReqPathData)])^:=ifthen(isStruct,FDataCRC,FDataType);
  PWord               (@Result[4+extraoffset+Length(FReqPathData)])^:=FReqCount;
  PLongWord           (@Result[6+extraoffset+Length(FReqPathData)])^:=FReqOffset;
  Move(FWriteReqData[0],Result[10+extraoffset+Length(FReqPathData)],Length(FWriteReqData));
end;

{ TCIPReadTagFragRply }

constructor TCIPReadTagFragRply.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  limit, Idx: Integer;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset, OptData);
  if (Length(aBytes)-offset)<4 then begin
    raise exception.Create('CIP Tag Read Fragmented Srv should have at least 4 bytes of size');
  end;

  if (aBytes[offset+0]<>$D2) and (aBytes[offset+1]<>0) then
    raise exception.Create('CIP Tag Read Fragmented Srv Reply inconsistency');

  FStatus:=aBytes[offset+2];
  FExtendedStatusCount:=aBytes[offset+3];
  SetLength(FExtendedStatus, FExtendedStatusCount);

  {$IFDEF LGXDEBUG}
  if FStatus=$04 then
    writeln('FIX-ME: path encode error on request. ',{$i %FILE%},':',{$i %LINE%});
  {$ENDIF}

  //if not (FStatus in [$00, $06]) then
  //  writeln('FIX-ME: got error ',IntToHex(FStatus),'. ',{$i %FILE%},':',{$i %LINE%});

  newOffset:=offset+4;
  if OptData<=0 then
    limit:=Length(aBytes)
  else
    limit:=OptData;

  if limit>Length(aBytes) then
    limit:=Length(aBytes);

  Idx:=0;
  //read extended status
  while (newOffset<limit) and (Idx<FExtendedStatusCount) do begin
    FExtendedStatus[Idx]:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    inc(Idx);
  end;

  if (FStatus<>0) and (FStatus<>6) then
    exit;

  FDataType:=Pword(@aBytes[newOffset])^;
  inc(newOffset,2);
  if FDataType=$02a0 then begin //estrutura
    FStructHandler:=FDataType;
    FDataType:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
  end;

  if newOffset>limit then exit;

  SetLength(FData,limit-newOffset);
  idx:=0;
  while (newOffset<limit) do begin
    FData[idx]:=aBytes[newOffset];
    inc(newOffset);
    inc(Idx);
  end;
end;

function TCIPReadTagFragRply.IsStructure: Boolean;
begin
  Result:=FStructHandler=$02a0;
end;

{ TCIPReadTagFragReq }

function TCIPReadTagFragReq.GetReqPath: String;
begin
  Result:=DecodeTagPath(FReqPathData);
end;

procedure TCIPReadTagFragReq.SetReqPath(AValue: String);
begin
  FReqPathData:=EncodeTagPath(AValue);
end;

function TCIPReadTagFragReq.getPacket: BYTES;
begin
  SetLength(Result,8+Length(FReqPathData));
  Result[0]:=$52;
  Result[1]:=Length(FReqPathData) div 2;
  Move(FReqPathData[0],Result[2],Length(FReqPathData));
  PWord(@Result[2+Length(ReqPathData)])^:=FReqCount;
  PLongWord(@Result[4+Length(ReqPathData)])^:=FReqOffset;
end;

{ TCIPMultiTagReadRply }

function TCIPMultiTagReadRply.GetReply(index: Integer): TCIPReadTagRply;
begin
  result:=nil;
  if Assigned(FReplyList) and (index<FReplyList.Count) then
    Result:=FReplyList.Items[index];
end;

constructor TCIPMultiTagReadRply.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  offsets:TDynWordArray;
  limit, Idx, r, offsetAtOffsetList: Integer;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset, OptData);
  FReplyList:=TCIPmultiRplyList.Create;
  if (Length(aBytes)-offset)<6 then begin
    raise exception.Create('CIP Multi Srv should have at least 6 bytes of size');
  end;

  if (aBytes[offset+0]<>$8A) and (aBytes[offset+1]<>0) then
    raise exception.Create('CIP Multi Srv Reply inconsistency');

  FStatus:=aBytes[offset+2];
  FExtendedStatusCount:=aBytes[offset+3];
  SetLength(FExtendedStatus, FExtendedStatusCount);

  {$IFDEF LGXDEBUG}
  if FStatus=$04 then
    writeln('FIX-ME: path encode error on request. ',{$i %FILE%},':',{$i %LINE%});
  {$ENDIF}

  //if not (FStatus in [$00, $06]) then
  //  writeln('FIX-ME: got error ',IntToHex(FStatus),'. ',{$i %FILE%},':',{$i %LINE%});

  newOffset:=offset+4;
  if OptData<=0 then
    limit:=Length(aBytes)
  else
    limit:=OptData;

  if limit>Length(aBytes) then
    limit:=Length(aBytes);

  Idx:=0;
  //read extended status
  while (newOffset<limit) and (Idx<FExtendedStatusCount) do begin
    FExtendedStatus[Idx]:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    inc(Idx);
  end;

  offsetAtOffsetList := newOffset;
  FReplyCount:=Pword(@aBytes[newOffset])^;
  inc(newOffset,2);
  SetLength(offsets, FReplyCount+1); //+1 for packet end.
  Idx:=0;
  //read reply offsets
  while (Idx<FReplyCount) do begin
    offsets[Idx]:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    inc(Idx);
  end;
  offsets[FReplyCount]:=Length(aBytes)-offsetAtOffsetList;

  if (Length(offsets)=0) then
    writeln('FIX-ME: Offsets list is empty? ',{$i %FILE%},':',{$i %LINE%});

  if (Length(offsets)>0) and (newOffset<>(offsetAtOffsetList+offsets[0])) then
    writeln('FIX-ME: First offset(',offsetAtOffsetList+offsets[0],') mismatch with calculated offset(',newOffset,')? ',{$i %FILE%},':',{$i %LINE%});

  offset:=newOffset;

  for r:=0 to FReplyCount-1 do begin
    FReplyList.Add(TCIPReadTagRply.CreateFromBytes(aBytes,newOffset,offset,offset + (offsets[r+1]-offsets[r])));
    inc(offset,offsets[r+1]-offsets[r]);
  end;
end;

destructor TCIPMultiTagReadRply.Destroy;
begin
  inherited Destroy;
  if Assigned(FReplyList) then
    FReplyList.Free;
end;

{ TCIPReadTagRply }

constructor TCIPReadTagRply.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  limit, Idx, offset2: Integer;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset, OptData);
  if (Length(aBytes)-offset)<4 then begin
    raise exception.Create('CIP Tag Read Srv should have at least 4 bytes of size');
  end;

  if (aBytes[offset+0]<>$CC) and (aBytes[offset+1]<>0) then
    raise exception.Create('CIP Tag Read Srv Reply inconsistency');

  FStatus:=aBytes[offset+2];
  FExtendedStatusCount:=aBytes[offset+3];
  SetLength(FExtendedStatus, FExtendedStatusCount);

  {$IFDEF LGXDEBUG}
  if FStatus=$04 then
    writeln('FIX-ME: path encode error on request. ',{$i %FILE%},':',{$i %LINE%});
  {$ENDIF}

  //if not (FStatus in [$00, $06]) then
  //  writeln('FIX-ME: got error ',IntToHex(FStatus),'. ',{$i %FILE%},':',{$i %LINE%});

  newOffset:=offset+4;
  if OptData<=0 then
    limit:=Length(aBytes)
  else
    limit:=OptData;

  if limit>Length(aBytes) then
    limit:=Length(aBytes);

  Idx:=0;
  //read extended status
  while (newOffset<limit) and (Idx<FExtendedStatusCount) do begin
    FExtendedStatus[Idx]:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
    inc(Idx);
  end;

  if (FStatus<>0) and (FStatus<>6) then exit;

  //DATA STARTS here...

  //copy the entire reply data buffer to
  //the pkg buffer...
  SetLength(FReplyData,limit-newOffset);
  idx:=0;
  offset2:=newOffset;
  while (offset2<limit) do begin
    FReplyData[idx]:=aBytes[offset2];
    inc(offset2);
    inc(Idx);
  end;

  //proceds with normal decodification

  FDataType:=Pword(@aBytes[newOffset])^;
  inc(newOffset,2);
  if FDataType=$02a0 then begin //estrutura
    FStructHandler:=FDataType;
    FDataType:=Pword(@aBytes[newOffset])^;
    inc(newOffset,2);
  end;

  if newOffset>limit then exit;

  SetLength(FData,limit-newOffset);
  idx:=0;
  while (newOffset<limit) do begin
    FData[idx]:=aBytes[newOffset];
    inc(newOffset);
    inc(Idx);
  end;
end;

function TCIPReadTagRply.IsStructure: Boolean;
begin
  Result:=FStructHandler=$02a0;
end;

{ TCIPMultiTagReadReq }

constructor TCIPMultiTagReadReq.Create;
begin
  inherited Create;
  FGettingSize:=false;
  ReqList:=TReqList.Create;
end;

destructor TCIPMultiTagReadReq.Destroy;
begin
  ReqList.Free;
  inherited Destroy;
end;

function TCIPMultiTagReadReq.getPacket: BYTES;
var
  aux: BYTES;
  offset, lastLen, c, idx:Integer;
begin
  if not assigned(ReqList) then
    raise Exception.Create('TODO: Deu Ruim');
  if (FGettingSize=false) and (ReqList.Count<1) then
    raise Exception.Create('TODO: Ai não dá né fio');

  //logix 5000 Ctrl Data Access Manual, pg 31
  SetLength(Result, 8+ReqList.Count*2); //
  Result[0]:=$0a;
  Result[1]:=$02;
  //msg router
  Result[2]:=$20;
  Result[3]:=$02;
  Result[4]:=$24;
  Result[5]:=$01;
  PWord(@Result[6])^ :=  ReqCount;

  offset:=8;
  lastLen:=0;
  for c:=0 to ReqList.Count-1 do begin
    PWord(@Result[offset])^ :=  2 + (ReqList.Count*2) + lastLen;
    inc(offset,2);
    aux:=ReqList.Items[c].getPacket;
    inc(lastLen, Length(aux));
    idx:=Length(Result);
    SetLength(Result,Length(Result)+Length(aux));
    Move(aux[0],Result[idx],Length(aux));
    SetLength(aux,0);
  end;
end;

function TCIPMultiTagReadReq.GetPacketSize: Integer;
var
  aux: BYTES;
begin
  try
    FGettingSize:=true;
    aux:=getPacket;
    Result:=Length(aux);
  finally
    FGettingSize:=false;
  end;
end;

function TCIPMultiTagReadReq.AddItem(aTagPath: String; ItemCount: Word
  ): Integer;
var
  aux: TCIPReadTagReq;
begin
  Result:=-1;
  if Assigned(ReqList) then begin
    aux:=TCIPReadTagReq.Create;
    aux.RequestPath:=aTagPath;
    aux.ReqCount:=ItemCount;
    Result:=ReqList.Add(aux);
  end;
end;

function TCIPMultiTagReadReq.AddItem(aTagPath: BYTES; ItemCount: Word): Integer;
var
  aux: TCIPReadTagReq;
begin
  Result:=-1;
  if Assigned(ReqList) then begin
    aux:=TCIPReadTagReq.Create;
    aux.ReqPathData:=aTagPath;
    aux.ReqCount:=ItemCount;
    Result:=ReqList.Add(aux);
  end;
end;

procedure TCIPMultiTagReadReq.Delete(aIndex: Integer);
begin
  if Assigned(ReqList) then
    ReqList.Delete(aIndex);
end;

function TCIPMultiTagReadReq.ReqCount: Integer;
begin
  Result:=0;
  if Assigned(ReqList) then
    Result:=ReqList.Count;
end;

{ TCPFConnectedDataItem }

function TCPFConnectedDataItem.GetEncapsuledPacket: TCIPPDU;
begin
  if Assigned(FEncapsuledPDUs) and (FEncapsuledPDUs.Count>0) and Assigned(FEncapsuledPDUs.Items[0]) and (FEncapsuledPDUs.Items[0] is TCIPPDU) then
    Result:=TCIPPDU(FEncapsuledPDUs.Items[0])
  else
    Result:=nil;
end;

constructor TCPFConnectedDataItem.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<6 then begin
    raise exception.Create('Common Packet Format, Connected data item should have at least 6 bytes of size');
  end;
  if (PWord(@aBytes[offset + 0])^ <> $00b1) or (PWord(@aBytes[offset + 2])^ < 2) then //should be TypeID = $b2, Unconnected Data Item, Length = 0
    raise exception.Create('Common Packet Format, Connected data item fields mismatch');

  FCIPSeq:=PWord(@aBytes[offset + 4])^;
  inc(newOffset, 6);

  FEncapsuledPDUs.Add(TCIPPDU.CreatePDUClassFromBytes(aBytes, newOffset, offset+6));
end;

function TCPFConnectedDataItem.getPacket: BYTES;
var
  offset, p: Integer;
  aux: BYTES;
begin
  SetLength(Result,6);
  PWord(@Result[0])^     := $00b1;   // TypeID = 0x00B2, UnConnected Data Item
  PWord(@Result[2])^     := 2;       // Length should be encapsuled PDU
  PWord(@Result[4])^     := FCIPSeq; // Length should be encapsuled PDU
  offset:=6;
  for p:=0 to FEncapsuledPDUs.Count-1 do begin
    aux:=FEncapsuledPDUs.Items[p].getPacket;
    if Length(aux)>0 then begin
      SetLength(Result,Length(Result)+Length(aux));
      Move(aux[0],Result[offset],Length(aux));
      inc(offset, Length(aux));
      inc(PWord(@Result[2])^, Length(aux));
    end;
    SetLength(aux, 0);
  end;
end;

function TCPFConnectedDataItem.Add(aItem: TCIPPDU): Integer;
begin
  Result:=FEncapsuledPDUs.Add(aItem);
end;

{ TCIPReadTagReq }

function TCIPReadTagReq.GetReqPath: String;
begin
  Result:=DecodeTagPath(FReqPathData);
end;

procedure TCIPReadTagReq.SetReqPath(AValue: String);
begin
  FReqPathData:=EncodeTagPath(AValue);
end;

function TCIPReadTagReq.getPacket: BYTES;
begin
  SetLength(Result,4+Length(FReqPathData));
  Result[0]:=$4c;
  Result[1]:=Length(FReqPathData) div 2;
  Move(FReqPathData[0],Result[2],Length(FReqPathData));
  PWord(@Result[2+Length(ReqPathData)])^:=FReqCount;
end;

{ TDecodeOnlyCIPPDU }

constructor TDecodeOnlyCIPPDU.Create;
begin
  raise exception.Create(ClassName+' is a decoding class and should be created using CreateFromBytes constructor!');
end;

function TDecodeOnlyCIPPDU.getPacket: BYTES;
begin
  raise exception.Create(ClassName+' is a decode class and can''t be used to encode a packet!');
end;

{ TEncodeOnlyCIPPDU }

constructor TEncodeOnlyCIPPDU.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  raise Exception.Create(ClassName+' can only be used to encode packet!');
end;

class function TEncodeOnlyCIPPDU.CreatePDUClassFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer): TPDU;
begin
  raise Exception.Create(ClassName+' can only be used to encode packet!');
end;

{ TCMForwardOpenFailureReplyPDU }

constructor TCMForwardOpenFailureReplyPDU.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<10 then begin
    raise exception.Create('CIP CM Forward Open failure reply should have at least 10 bytes of size');
  end;

  FConnSerialNumber       := PWORD(@aBytes[offset+0])^;
  FOriginatorVendorID     := PWORD(@aBytes[offset+2])^;
  FOriginatorSerialNumber := PLONGWORD(@aBytes[offset+4])^;
  FRemainingPathSize      := aBytes[8];
  //abytes[9] = reserved!
end;

{ TCMForwardOpenSuccessfulReplyPDU }

constructor TCMForwardOpenSuccessfulReplyPDU.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  c: Integer;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<26 then begin
    raise exception.Create('CIP CM Forward Open sucessful reply should have at least 26 bytes of size');
  end;

  FOT_ConnID := PLONGWORD(@aBytes[offset+0])^;
  FTO_ConnID := PLONGWORD(@aBytes[offset+4])^;
  FConnSerialNumber := PWORD(@aBytes[offset+8])^;
  FOriginatorVendorID := PWORD(@aBytes[offset+10])^;
  FOriginatorSerialNumber := PLONGWORD(@aBytes[offset+12])^;
  FOT_API := PLongWord(@aBytes[offset+16])^;
  FTO_API := PLongWord(@aBytes[offset+20])^;
  FAppRplySize := PByte(@aBytes[offset+24])^*2;
  SetLength(FAppReply,FAppRplySize);
  for c:=0 to FAppRplySize-1 do
    FAppReply[c]:=aBytes[offset+26+c];
  inc(newOffset,26+FAppRplySize);
end;

{ TCMForwardOpenReqPDU }

constructor TCMForwardOpenReqPDU.Create;
begin
  inherited Create;
  FPriority:=$a;
end;

function TCMForwardOpenReqPDU.getPacket: BYTES;
var
  c: Integer;
begin
  SetLength(Result,36+Length(FPath)+4);
  Result[0]               := FPriority;
  Result[1]               := FTimeOut_Ticks;
  PLONGWORD(@Result[2])^  := FOT_ConnID;
  PLONGWORD(@Result[6])^  := FTO_ConnID;
  PWORD(@Result[10])^     := FConnSerialNumber;
  PWORD(@Result[12])^     := FOriginatorVendorID;
  PLONGWORD(@Result[14])^ := FOriginatorSerialNumber;
  Result[18]              := FTimeOutMultiplier;
  Result[19]              := 0;//FReservedB1;
  Result[20]              := 0;//FReservedB2;
  Result[21]              := 0;//FReservedB3;
  PLONGWORD(@Result[22])^ := FOT_RPI;
  PWORD(@Result[26])^     := FOT_Parameters;
  PLONGWORD(@Result[28])^ := FTO_RPI;
  PWORD(@Result[32])^     := FTO_Parameters;
  Result[34]              := FTransport;
  Result[35]              := ((Length(FPath)+4) div 2)+((Length(FPath)+4) mod 2);

  for c:=0 to Length(FPath)-1 do
    Result[36+c]:=FPath[c];

  //ROUTER PATH...
  Result[36+Length(FPath)+0]:=$20;
  Result[36+Length(FPath)+1]:=$02;
  Result[36+Length(FPath)+2]:=$24;
  Result[36+Length(FPath)+3]:=$01;
end;

{ TForwardOpenReplyPDU }

constructor TForwardOpenReplyPDU.Create;
begin
  raise exception.Create(ClassName+' can only be used to decode received packets, not for encoding');
end;

function TForwardOpenReplyPDU.getPacket: BYTES;
begin
  raise exception.Create(ClassName+' can only be used to decode received packets, not for encoding');
end;

class function TForwardOpenReplyPDU.CreatePDUClassFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer): TPDU;
begin
  raise exception.Create(ClassName+' not supported');
end;

{ TForwardOpenReqPDU }

constructor TForwardOpenReqPDU.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  raise exception.Create(ClassName+' can only be used to built packets to send, not for decoding');
end;

class function TForwardOpenReqPDU.CreatePDUClassFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer): TPDU;
begin
  raise exception.Create(ClassName+' can only be used to built packets to send, not for decoding');
end;

{ TCIPPDU }

class function TCIPPDU.CreatePDUClassFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer): TPDU;
begin
  case aBytes[offset + 0] of
    $83: Result:= TCIPGetAttrListSrvRply.CreateFromBytes(abytes, newOffset, offset);
    $8a: Result:= TCIPMultiTagReadRply.CreateFromBytes  (abytes, newOffset, offset);
    $cc: Result:= TCIPReadTagRply.CreateFromBytes       (abytes, newOffset, offset);
    $cd: Result:= TCIPWriteTagRply.CreateFromBytes      (abytes, newOffset, offset);
    $d2: Result:= TCIPReadTagFragRply.CreateFromBytes   (abytes, newOffset, offset);
    $d3: Result:= TCIPWriteTagFragRply.CreateFromBytes  (abytes, newOffset, offset);
    $d4: Result:= TCIP_ForwardOpen_Reply.CreateFromBytes(abytes, newOffset, offset);
    $d5: Result:= TCIPListTagsRply.CreateFromBytes      (abytes, newOffset, offset);
    else
      raise exception.Create('Package without decode!');
  end;
end;

{ TCIP_ForwardOpen_Reply }

function TCIP_ForwardOpen_Reply.GetFORplyPDU: TForwardOpenReplyPDU;
begin
  if Assigned(FEncapsuledPDUs) and (FEncapsuledPDUs.Count>0) and Assigned(FEncapsuledPDUs.Items[0]) and (FEncapsuledPDUs.Items[0] is TForwardOpenReplyPDU) then
    Result:=TForwardOpenReplyPDU(FEncapsuledPDUs.Items[0])
  else
    Result:=nil;
end;

constructor TCIP_ForwardOpen_Reply.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  offsetDelta, c: Integer;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<4 then begin
    raise exception.Create('CIP CM ForwardOpen should have at least 4 bytes of size');
  end;

  offsetDelta:=4;
  if aBytes[offset+0]=$d4 then begin
    FStatus:=aBytes[offset+2];
    if aBytes[offset+3]>0 then begin
      SetLength(FAditionalStatus,2*aBytes[offset+3]);
      inc(offsetDelta,2*aBytes[offset+3]);
      for c:=0 to aBytes[offset+3]-1 do begin
        FAditionalStatus[c]:=aBytes[offset+4+c];
      end;
    end;
    inc(newOffset,offsetDelta);

    if Fstatus=0 then
      FEncapsuledPDUs.Add(TCMForwardOpenSuccessfulReplyPDU.CreateFromBytes(abytes,newOffset,offset+offsetDelta))
    else
      FEncapsuledPDUs.Add(TCMForwardOpenFailureReplyPDU.CreateFromBytes(abytes,newOffset,offset+offsetDelta));
  end else
    raise exception.Create('CIP CM ForwardOpen invalid reply');
end;

{ TCIP_ForwardOpen_Req }

function TCIP_ForwardOpen_Req.GetFOPDU: TForwardOpenReqPDU;
begin
  if Assigned(FEncapsuledPDUs) and (FEncapsuledPDUs.Count>0) and Assigned(FEncapsuledPDUs.Items[0]) and (FEncapsuledPDUs.Items[0] is TForwardOpenReqPDU) then
    Result:=TForwardOpenReqPDU(FEncapsuledPDUs.Items[0])
  else
    Result:=nil;
end;

procedure TCIP_ForwardOpen_Req.SetFOTarget(AValue: TForwardOpenTarget);
begin
  if FFOTarget=AValue then Exit;
  FFOTarget:=AValue;
  FEncapsuledPDUs.Clear;
  case FFOTarget of
    CONNECTION_MANAGER: FEncapsuledPDUs.Add(TCMForwardOpenReqPDU.Create);
    //ROUTER,
    //BACKPLANE_DATA,
    //OBJECT_PCCC,
    //OBJECT_DHP_A,
    //OBJECT_DHP_B
    ELSE RAISE EXCEPTION.Create('Unknown Forward open target');
  end;
end;

constructor TCIP_ForwardOpen_Req.Create;
begin
  inherited Create;
  FFOTarget:=CONNECTION_MANAGER;
  FEncapsuledPDUs.Add(TCMForwardOpenReqPDU.Create);
end;

function TCIP_ForwardOpen_Req.getPacket: BYTES;
var
  path:array[low(TForwardOpenTarget) .. high(TForwardOpenTarget)] of Byte = ($06, $02, $66, $67, $a6, $a6);
  extra:array[OBJECT_DHP_A..OBJECT_DHP_B] of byte = ($01, $02);
  aux: BYTES;
  extraoffset: Integer = 0;
begin
  //BYTE CM_PATH[4]            ={0x20,CONNECTION_MANAGER,0x24,0x01};
  //BYTE ROUTER_PATH[4]        ={0x20,ROUTER            ,0x24,0x01};
  //BYTE BACKPLANE_DATA_PATH[4]={0x20,BACKPLANE_DATA    ,0x24,0x01};
  //BYTE PCCC_PATH[4]          ={0x20,OBJECT_PCCC       ,0x24,0x01};
  //BYTE DHPA_PROXY_PATH[6]    ={0x20,OBJECT_DHP        ,0x24,0x01,0x2C,0x01};
  //BYTE DHPB_PROXY_PATH[6]    ={0x20,OBJECT_DHP        ,0x24,0x02,0x2C,0x01};
  //TODO: Melhorar tudo isso
  SetLength(Result,6);
  Result[0]:=$54;
  Result[1]:=2; //2 words de tamanho, 4 bytes
  Result[2]:=$20;
  Result[3]:=path[FFOTarget];
  Result[4]:=$24;
  Result[5]:=$01;

  if FFOTarget in [OBJECT_DHP_A, OBJECT_DHP_B] then begin
    SetLength(Result,8);
    Result[5]:=extra[FFOTarget];
    Result[6]:=$2c;
    Result[7]:=$01;
    extraoffset:=2;
  end;

  if assigned(GetFOPDU) then begin
    aux:=GetFOPDU.getPacket;
    if Length(aux)>0 then begin
      SetLength(Result,Length(Result)+Length(aux));
      Move(aux[0],Result[6+extraoffset],Length(aux));
      SetLength(aux, 0);
    end;
  end;
end;

{ TEIPNOPPDU }

constructor TEIPNopPDU.Create;
begin
  inherited Create;
  FCommand:=$0000;
  FLength:=0;
  FSessionHandler:=0;
  FStatus:=0;
  FSenderContext:=0;
  FOptions:=0;
end;

{ TEIPSendUnitDataPDU }

function TEIPSendUnitDataPDU.GetSendUnitDataPDU: TSendUnitDataCmdPDU;
begin
  if Assigned(FEncapsuledPDUs) and
     (FEncapsuledPDUs.Count>0) and
     Assigned(FEncapsuledPDUs.Items[0])
     and (FEncapsuledPDUs.Items[0] is TSendUnitDataCmdPDU) then
    Result:=TSendUnitDataCmdPDU(FEncapsuledPDUs.Items[0])
  else
    Result:=nil;
end;

constructor TEIPSendUnitDataPDU.Create;
begin
  inherited Create;
  FCommand:=$0070;
  FLength:=0;
  FSessionHandler:=0;
  FStatus:=0;
  FSenderContext:=0;
  FOptions:=0;
  FEncapsuledPDUs.Add(TSendUnitDataCmdPDU.Create);
end;

{ TEIPSendRRDataPDU }

function TEIPSendRRDataPDU.GetSendRRDataPDU: TSendRRDataCmdPDU;
begin
  if Assigned(FEncapsuledPDUs) and
     (FEncapsuledPDUs.Count>0) and
     Assigned(FEncapsuledPDUs.Items[0])
     and (FEncapsuledPDUs.Items[0] is TSendRRDataCmdPDU) then
    Result:=TSendRRDataCmdPDU(FEncapsuledPDUs.Items[0])
  else
    Result:=nil;
end;

constructor TEIPSendRRDataPDU.Create;
begin
  inherited Create;
  FCommand:=$006f;
  FLength:=0;
  FSessionHandler:=0;
  FStatus:=0;
  FSenderContext:=0;
  FOptions:=0;
  FEncapsuledPDUs.Add(TSendRRDataCmdPDU.Create);
  TSendRRDataCmdPDU(FEncapsuledPDUs.Items[0]).Timeout:=1;
end;

{ TEIPUnregSessionPDU }

constructor TEIPUnregSessionPDU.Create;
begin
  inherited Create;
  FCommand:=$0066;
  FLength:=0;
  FSessionHandler:=0;
  FStatus:=0;
  FSenderContext:=0;
  FOptions:=0;
end;

{ TSendUnitDataCmdPDU }

constructor TSendUnitDataCmdPDU.Create;
begin
  inherited Create;
  fInterfaceHandle:=0;
  fTimeout:=0;
end;

function TSendUnitDataCmdPDU.ParamsOK: Boolean;
begin
  Result:=(fTimeout=0) and (fInterfaceHandle=0);
end;

{ TGenericEIPSendDataCmdPDU }

function TGenericEIPSendDataCmdPDU.GetCPF: TCommonPacketFormat;
begin
  if Assigned(FEncapsuledPDUs) and (FEncapsuledPDUs.Count>0) and Assigned(FEncapsuledPDUs.Items[0]) and (FEncapsuledPDUs.Items[0] is TCPF) then
    Result:=TCPF(FEncapsuledPDUs.Items[0])
  else
    Result:=nil;
end;

function TGenericEIPSendDataCmdPDU.ParamsOK: Boolean;
begin
  Result:=true;
end;

constructor TGenericEIPSendDataCmdPDU.Create;
begin
  inherited Create;
  FEncapsuledPDUs.Add(TCommonPacketFormat.Create);
end;

constructor TGenericEIPSendDataCmdPDU.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<6 then begin
    raise exception.Create('Send RR/Unit data must have at least 6 bytes');
  end;

  fInterfaceHandle :=PLongWord(@aBytes[Offset + 0])^;
  fTimeout         :=PWord    (@aBytes[Offset + 4])^;

  if not ParamsOK then
    raise exception.Create('Send RR/Unit Params aren''t ok');

  inc(newOffset,6);

  FEncapsuledPDUs.Add(TCommonPacketFormat.CreateFromBytes(aBytes,newOffset,offset+6));
end;

function TGenericEIPSendDataCmdPDU.getPacket: BYTES;
var
  aux: BYTES;
begin
  SetLength(Result, 6);
  PLongWord(@Result[0])^ := fInterfaceHandle;
      PWord(@Result[4])^ := fTimeout;
  if Assigned(GetCPF) then begin
    aux:=GetCPF.getPacket;
    if Length(aux)>0 then begin
      SetLength(Result,Length(Result)+Length(aux));
      Move(aux[0],Result[6],Length(aux));
      SetLength(aux,0);
    end;
  end;
end;

{ TEIPRegSessionPDU }

constructor TEIPRegSessionPDU.Create;
begin
  inherited Create;
  FCommand:=$0065;
  FLength:=4;
  FSessionHandler:=0;
  FStatus:=0;
  FSenderContext:=0;
  FOptions:=0;
  FEncapsuledPDUs.Add(TEIPHeaderCmdDataRegSessionPDU.Create);
end;

{ TCPFGenericDataItem }

function TCPFUnconnectedDataItem.GetEncapsuledPacket: TCIPPDU;
begin
  if Assigned(FEncapsuledPDUs) and (FEncapsuledPDUs.Count>0) and Assigned(FEncapsuledPDUs.Items[0]) and (FEncapsuledPDUs.Items[0] is TCIPPDU) then
    Result:=TCIPPDU(FEncapsuledPDUs.Items[0])
  else
    Result:=nil;
end;

constructor TCPFUnconnectedDataItem.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<4 then begin
    raise exception.Create('Common Packet Format, Unconnected data item should have 4 bytes of size');
  end;
  if (PWord(@aBytes[offset + 0])^ <> $00b2) or (PWord(@aBytes[offset + 2])^ = 0) then //should be TypeID = $b2, Unconnected Data Item, Length = 0
    raise exception.Create('Common Packet Format, Unconnected data item fields mismatch');

  inc(newOffset, 4);

  FEncapsuledPDUs.Add(TCIPPDU.CreatePDUClassFromBytes(aBytes, newOffset, offset+4));
end;

function TCPFUnconnectedDataItem.getPacket: BYTES;
var
  offset, p: Integer;
  aux: BYTES;
begin
  SetLength(Result,4);
  PWord(@Result[0])^     := $00b2; // TypeID = 0x00B2, UnConnected Data Item
  PWord(@Result[2])^     := 0;     // Length should be encapsuled PDU
  offset:=4;
  for p:=0 to FEncapsuledPDUs.Count-1 do begin
    aux:=FEncapsuledPDUs.Items[p].getPacket;
    if Length(aux)>0 then begin
      SetLength(Result,Length(Result)+Length(aux));
      Move(aux[0],Result[offset],Length(aux));
      inc(offset, Length(aux));
      inc(PWord(@Result[2])^, Length(aux));
    end;
    SetLength(aux, 0);
  end;
end;

function TCPFUnconnectedDataItem.Add(aItem: TCIPPDU): Integer;
begin
  Result:=FEncapsuledPDUs.Add(aItem);
end;

{ TCPFAddressItem }

constructor TCPFAddressItem.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<8 then begin
    raise exception.Create('Common Packet Format, Address item should have 8 bytes of size');
  end;
  if (PWord(@aBytes[offset + 0])^ <> $00a1) or (PWord(@aBytes[offset + 2])^ <> 4) then //should be TypeID = $a1, Null Address, Length = 4
    raise exception.Create('Common Packet Format, Address item fields mismatch');

  FConnID:=PLongWord(@aBytes[4])^;

  inc(newOffset, 8);
end;

function TCPFAddressItem.getPacket: BYTES;
begin
  SetLength(Result,8);
  PWord(@Result[0])^     := $00a1; // TypeID = 0x00a1, Address item
  PWord(@Result[2])^     := 4;     // Length = 4
  PLongWord(@Result[4])^ := FConnID;
end;

{ TCPFNullAddressItem }

constructor TCPFNullAddressItem.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<4 then begin
    raise exception.Create('Common Packet Format, Null Address should have 4 bytes of size');
  end;
  if (PWord(@aBytes[offset + 0])^ <> 0) or (PWord(@aBytes[offset + 2])^ <> 0) then //should be TypeID = 0, Null Address, Length = 0
    raise exception.Create('Common Packet Format, Null Address fields should all be 0');

  inc(newOffset, 4);
end;

function TCPFNullAddressItem.getPacket: BYTES;
begin
  SetLength(Result,4);
  PWord(@Result[0])^ := 0; // TypeID = 0, Null Address
  PWord(@Result[2])^ := 0; // Length = 0
end;

{ TCommonPacketFormat }

constructor TCommonPacketFormat.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
var
  FExpectedItemCount: Word;
  InternalOffset, PreviousOffset, count: Integer;
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<10 then begin
    raise exception.Create('Common packet format should have at least 10 bytes of size');
  end;

  FExpectedItemCount := PWord(@aBytes[offset + 0])^;

  InternalOffset:=0;
  inc(newOffset, 2);
  inc(offset, 2);
  count:=0;
  while (count<FExpectedItemCount) do begin
    PreviousOffset := newOffset;
    case PWord(@aBytes[offset + InternalOffset])^ of
      $0000: FEncapsuledPDUs.Add(TCPFNullAddressItem.CreateFromBytes(aBytes, newOffset, offset + InternalOffset));      {Null address item}
      $000c: begin {TODO: ListIdentity}  end;
      $00a1: FEncapsuledPDUs.Add(TCPFAddressItem.CreateFromBytes(aBytes, newOffset, offset + InternalOffset));          {Connection Address item}
      $00b1: FEncapsuledPDUs.Add(TCPFConnectedDataItem.CreateFromBytes(aBytes, newOffset, offset + InternalOffset));   {Connected Transport packet}
      $00b2: FEncapsuledPDUs.Add(TCPFUnconnectedDataItem.CreateFromBytes(aBytes, newOffset, offset + InternalOffset)); {Unconnected message}
      $0100: begin {TODO: ListServices response} end;
      $8000: begin {TODO: Sockaddr Info, originator-to-target} end;
      $8001: begin {TODO: Sockaddr Info, target-to-originator} end;
      $8002: begin {TODO: Sequenced Address iteme} end;
    end;
    InternalOffset:=InternalOffset + (newOffset - PreviousOffset);
    inc(count);
  end;

  if FExpectedItemCount<>FEncapsuledPDUs.Count then
    raise exception.Create('Expected Common Packet Format.ItemCount mismatch with count of items read');

end;

function TCommonPacketFormat.getPacket: BYTES;
var
  c, offset: Integer;
  aux: BYTES;
begin
  if CPFItemCount<2 then
    raise Exception.Create('Common Packet Format should have at least two items!');


  SetLength(Result, 2);
  PWord(@Result[0])^ := CPFItemCount;
  offset:=2;
  for c:=0 to CPFItemCount-1 do begin
    aux:=CPFItem[c].getPacket;
    SetLength(Result,Length(Result)+Length(aux));
    Move(aux[0],Result[offset],Length(aux));
    inc(offset, Length(aux));
  end;
end;

function TCommonPacketFormat.Add(aItem: TCPFItem): Integer;
begin
  Result:=FEncapsuledPDUs.Add(aItem);
end;

{ TRegUnregSessionPDU }

constructor TEIPHeaderCmdDataRegSessionPDU.Create;
begin
  inherited Create;
  FProtocolVersion:=1;
  FOptionFlags    :=0;
end;

constructor TEIPHeaderCmdDataRegSessionPDU.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<>4 then begin
    raise exception.Create('Session data must have 4 bytes');
  end;
  FProtocolVersion :=PWord    (@aBytes[Offset + 0])^;
  FOptionFlags     :=PWord    (@aBytes[Offset + 2])^;

  if (FProtocolVersion<>1) Then
    raise exception.Create('Session data, Protocol version should be 1');

  if (FOptionFlags<>0) Then
    raise exception.Create('Session data, Option Flags should be 0');
  inc(newOffset, 4);
end;

function TEIPHeaderCmdDataRegSessionPDU.getPacket: BYTES;
begin
  SetLength(Result, 4);
  PWord    (@Result[0])^:=FProtocolVersion;
  PWord    (@Result[2])^:=FOptionFlags    ;
end;

{ TEIPEncapsulationHeader }

function TEIPEncapsulationHeader.GetCommandData: TPDU;
begin
  if Assigned(FEncapsuledPDUs) and (FEncapsuledPDUs.Count>0) and Assigned(FEncapsuledPDUs.Items[0]) then
    Result:=FEncapsuledPDUs.Items[0]
  else
    Result:=nil;
end;

constructor TEIPEncapsulationHeader.CreateFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer;
  CreateChildrenPDUs: Boolean);
begin
  inherited CreateFromBytes(aBytes, newOffset, offset);
  if (Length(aBytes)-offset)<24 then begin
    raise exception.Create('Encapsulation header must have at least 24 bytes');
  end;

  FCommand       :=PWord    (@aBytes[Offset + 0])^;
  FLength        :=PWord    (@aBytes[Offset + 2])^;
  FSessionHandler:=PLongWord(@aBytes[Offset + 4])^;
  FStatus        :=PLongWord(@aBytes[Offset + 8])^;
  FSenderContext :=PQWord   (@aBytes[Offset + 12])^;
  FOptions       :=PLongWord(@aBytes[Offset + 20])^;

  if CreateChildrenPDUs and (FLength>0) then
    case FCommand of
      $0000: begin end; //NOP, don't decode de Command specific data
      $0004: FEncapsuledPDUs.Add(TEIPListServicesData.CreateFromBytes(aBytes, newOffset, Offset+24)) ;
      $0063: begin {TODO: ListIdentity } end;
      $0064: begin {TODO: Listnterfaces} end;

      $0065: begin {RegisterSession}
        FEncapsuledPDUs.Add(TEIPHeaderCmdDataRegSessionPDU.CreateFromBytes(aBytes, newOffset, Offset+24));
        inc(newOffset,24);
      end;
      $0066: begin {UnRegisterSession}
        if FLength<>0 then
          raise exception.Create('UnregisterSession command specific data length should be Zero(0)');
        inc(newOffset,24);
      end;

      $006F: FEncapsuledPDUs.Add(TSendRRDataCmdPDU.CreateFromBytes(abytes,newOffset, Offset+24));   {SEndRRData / UCMM messages}
      $0070: FEncapsuledPDUs.Add(TSendUnitDataCmdPDU.CreateFromBytes(abytes,newOffset, Offset+24)); {SendUnitData}
      $0072: begin {TODO: IndicateStatus} end;
      $0073: begin {TODO: Cancel} end;
    end;

  {TODO: Check the Read Length with decoded Command Data size}
end;

function TEIPEncapsulationHeader.getPacket: BYTES;
var
  aux: BYTES;
begin
  SetLength(Result,24);
  PWord    (@Result[0])^  := FCommand       ;
  PWord    (@Result[2])^  := 0              ;
  PLongWord(@Result[4])^  := FSessionHandler;
  PLongWord(@Result[8])^  := FStatus        ;
  PQWord   (@Result[12])^ := FSenderContext ;
  PLongWord(@Result[20])^ := FOptions       ;

  if assigned(FEncapsuledPDUs) and (FEncapsuledPDUs.Count>0) and Assigned(FEncapsuledPDUs.Items[0]) and (FEncapsuledPDUs.Items[0] is TPDU) then begin
    aux:=FEncapsuledPDUs.Items[0].getPacket;
    if Length(aux)>0 then begin
      PWord(@Result[2])^  := Length(aux);
      SetLength(Result, Length(Result)+Length(aux));
      Move(aux[0],Result[24],Length(aux));
    end;
  end;
end;

class function TEIPEncapsulationHeader.CreatePDUClassFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer): TPDU;
begin
  case PWord    (@aBytes[Offset + 0])^ of
    $0000: Result:=TEIPNopPDU.CreateFromBytes(aBytes,newOffset,offset);  //NOP, don't decode de Command specific data
    $0004: Result:=TEIPListServices.CreateFromBytes(aBytes,newOffset,offset);
    $0063: begin {TODO: ListIdentity } end;
    $0064: begin {TODO: Listnterfaces} end;
    $0065: Result:=TEIPRegSessionPDU.CreateFromBytes(aBytes,newOffset,offset);   {RegisterSession}
    $0066: Result:=TEIPUnregSessionPDU.CreateFromBytes(aBytes,newOffset,offset); {UnRegisterSession}
    $006F: Result:=TEIPSendRRDataPDU.CreateFromBytes(aBytes,newOffset,offset);   {SEndRRData / UCMM messages}
    $0070: Result:=TEIPSendUnitDataPDU.CreateFromBytes(aBytes,newOffset,offset); {SendUnitData}
    $0072: begin {TODO: IndicateStatus} end;
    $0073: begin {TODO: Cancel} end;
  end;
end;

{ TPDU }

function TPDU.getPdu(index: Integer): TPDU;
begin
  if assigned(FEncapsuledPDUs) and (index<FEncapsuledPDUs.Count) then
    Result:=FEncapsuledPDUs.Items[index]
  else
    Result:=nil;
end;

function TPDU.GetEncapsuledPDUCount: Integer;
begin
  if Assigned(FEncapsuledPDUs) then
    Result:=FEncapsuledPDUs.Count
  else
    Result:=-1;
end;

constructor TPDU.Create;
begin
  inherited;
  FEncapsuledPDUs:=TPDUList.Create;
end;

constructor TPDU.CreateFromBytes(aBytes: BYTES; var newOffset: Integer;
  offset: Integer; OptData: Integer; CreateChildrenPDUs: Boolean);
begin
  inherited Create;
  FEncapsuledPDUs:=TPDUList.Create;
end;

class function TPDU.CreatePDUClassFromBytes(aBytes: BYTES;
  var newOffset: Integer; offset: Integer; OptData: Integer): TPDU;
begin
  Result:=nil;
end;

destructor TPDU.Destroy;
begin
  FEncapsuledPDUs.Free;
  inherited Destroy;
end;

function TPDU.getPacket: BYTES;
begin
end;

{ TLPTTagRec }

class operator TLPTTagRec.initialize(var aRec: TLPTTagRec);
begin
  aRec.mm:=TPLCMemoryManager.Create;
end;

class operator TLPTTagRec.finalize(var aRec: TLPTTagRec);
begin
  if assigned(aRec.mm) then
    aRec.mm.Free;
end;

class operator TLPTTagRec.Copy(constref aSrc: TLPTTagRec; var aDst: TLPTTagRec);
begin
  raise Exception.Create('Não permitido!');
end;

type
  TLGXPLCResources = record
    Mutex:TCriticalSection;
    FullyLoaded:Boolean;
    LastTagAdded:String;
    TagList:TLGXTagMap; //variáveis/tags presentes no CLP
    UDTList:TUDTList;   //estruturas presentes no CLP
  end;
  PLGXPLCResources = ^TLGXPLCResources;

  TPLCDataMap = specialize TFPGMap<TPortUniqueID, PLGXPLCResources>;

var
  FLGXGlobalMutex:TCriticalSection;
  FPLCData:TPLCDataMap;
  LGXTagBuilderEditor:TOpenTagEditor = nil;

{ TLGXDriver }

function SortGenericTagList(const Item1, Item2: TScanInfoItem): Integer;
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
      if Item1.UpdateRate<>0 then ScanPercent1:=(MilliSecondsBetween(Now,Item1.LastUpdate)/Item1.UpdateRate);

      ScanPercent2:=0;
      if Item2.UpdateRate<>0 then ScanPercent2:=(MilliSecondsBetween(Now,Item2.LastUpdate)/Item2.UpdateRate);


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

constructor TLGXDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTerminating:=0;
  FConnectionID:=0;
  FConnectionID:=0;
  PReadSomethingAlways:=true;
  FTagList:=TLPTTagMap.Create;
  FTagList.Sorted:=true;
end;


destructor TLGXDriver.Destroy;
var
  c, i: Integer;
begin
  for c:=FTagList.Count-1 downto 0 do begin
    Dispose(FTagList.Data[c]);
  end;
  FTagList.Free;
  inherited Destroy;
end;

function TLGXDriver.SizeOfTag(aTag: TTag; isWrite: Boolean;
  var ProtocolTagType: TProtocolTagType): BYTE;
begin
  ProtocolTagType:=ptByte;
  Result:=8;
end;

function TLGXDriver.TagListLoaded: Boolean;
var
  idx: Integer;
begin
  if (InterlockedExchange(FTerminating,FTerminating)<>0) or (csDestroying in ComponentState) then begin
    InterlockedExchange(FTerminating, 1);
    exit;
  end;

  result:=false;
  if Assigned(PCommPort) then begin
    FLGXGlobalMutex.Enter;
    try
      if FPLCData.Find(PCommPort.getPortId,idx) then begin
        Result:=FPLCData.Data[idx]^.FullyLoaded;
      end;
    finally
      FLGXGlobalMutex.Leave;
    end;
  end;
end;

function TLGXDriver.LastTagListed: String;
var
  idx: Integer;
  plcData: PLGXPLCResources;
begin
  if (InterlockedExchange(FTerminating,FTerminating)<>0) or (csDestroying in ComponentState) then begin
    InterlockedExchange(FTerminating, 1);
    exit;
  end;
  if Assigned(PCommPort) then begin
    try
      FLGXGlobalMutex.Enter;
      if FPLCData.Find(PCommPort.getPortId,idx) then begin
        plcData:=FPLCData.data[idx];
        Result:=plcData^.LastTagAdded;
      end;
    finally
      FLGXGlobalMutex.Leave;
    end;
  end;
end;

function TLGXDriver.BrowseTagList(out aTagList: TLGXTagInfoArray;
  ForceReload: Boolean): Boolean;
var
  idx, c: Integer;
  plcData: PLGXPLCResources = nil;
begin
  Result:=false;
  SetLength(aTagList,0);

  if (InterlockedExchange(FTerminating,FTerminating)<>0) or (csDestroying in ComponentState) then
    exit;

  if (not Assigned(PCommPort)) or (not PCommPort.ReallyActive) then
    exit;

  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;
    try
      if ForceReload then
        ClearTagListCache;

      //abre a sessão/conexão CIP, caso ainda não esteja aberta.
      if GetConnectionID=0 then exit;

      LoadLGXTagList;

      FLGXGlobalMutex.Enter;
      try
        if FPLCData.Find(PCommPort.getPortId,idx) then
          plcData:=FPLCData.Data[idx];
      finally
        FLGXGlobalMutex.Leave;
      end;

      if not Assigned(plcData) then exit;

      plcData^.Mutex.Enter;
      try
        SetLength(aTagList, plcData^.TagList.Count);
        for c:=0 to plcData^.TagList.Count-1 do
          aTagList[c]:=plcData^.TagList.Data[c];
        Result:=plcData^.FullyLoaded;
      finally
        plcData^.Mutex.Leave;
      end;
    finally
      FReadCS.Leave;
      FWriteCS.Leave;
    end;
  finally
    FPause.SetEvent;
  end;
end;

procedure TLGXDriver.ClearTagListCache;
var
  idx, u, f: Integer;
  plcData: PLGXPLCResources = nil;
begin
  if not Assigned(PCommPort) then exit;

  FLGXGlobalMutex.Enter;
  try
    if FPLCData.Find(PCommPort.getPortId,idx) then
      plcData:=FPLCData.Data[idx];
  finally
    FLGXGlobalMutex.Leave;
  end;

  if not Assigned(plcData) then exit;

  plcData^.Mutex.Enter;
  try
    plcData^.TagList.Clear;

    for u:=plcData^.UDTList.Count-1 downto 0 do begin
      for f:=plcData^.UDTList.Data[u]^.fields.Count-1 downto 0 do
        Dispose(plcData^.UDTList.Data[u]^.fields[f]);
      plcData^.UDTList.Data[u]^.fields.Clear;
      Dispose(plcData^.UDTList.Data[u]);
    end;
    plcData^.UDTList.Clear;

    plcData^.LastTagAdded:='';
    plcData^.FullyLoaded :=false;
  finally
    plcData^.Mutex.Leave;
  end;
end;

procedure TLGXDriver.OpenTagEditor(InsertHook: TAddTagInEditorHook;
  CreateProc: TCreateTagProc);
begin
  if Assigned(LGXTagBuilderEditor) then
    LGXTagBuilderEditor(Self, Self.Owner, InsertHook, CreateProc)
  else
    inherited OpenTagEditor(InsertHook, CreateProc);
end;

function TLGXDriver.HasTabBuilderEditor: Boolean;
begin
  Result:=true;
end;

function TLGXDriver.NotifyThisEvents: TNotifyThisEvents;
begin
  Result:=[ntePortClosed, ntePortDisconnected];
end;

procedure TLGXDriver.PortClosed(Sender: TObject);
begin
  PortDisconnected(Self);
end;

procedure TLGXDriver.PortDisconnected(Sender: TObject);
begin
  writeln('WARNING: Connection lost. Reseting all connection IDs ',{$i %FILE%},':',{$i %LINE%});
  InterlockedExchange(FCIPSession, 0);
  InterlockedExchange(FConnectionID, 0);
end;

var
  intTO_ConnID:LongWord = $12345677;
  intConnSerialNumber:LongWord = $6788;
  intOriginatorSerialNumber:longword = $98765431;

function TLGXDriver.GetConnID: LongWord;
begin
  Result:=InterlockedIncrement(intTO_ConnID);
end;

function TLGXDriver.GetConnSerialNumber: word;
begin
  Result:=InterlockedIncrement(intConnSerialNumber);
end;

function TLGXDriver.GetOriginatorSerialNumber: LongWord;
begin
  Result:=InterlockedIncrement(intOriginatorSerialNumber);
end;

function TLGXDriver.OpenSession: LongWord;
var
  sessionReq:TEIPRegSessionPDU;
  iopkg:TIOPacket;
  responsepkg: TEIPEncapsulationHeader;
  aux: Cardinal;
  aux2: BYTES;
  aux3: Integer;
begin
  Result:=0;
  aux:=InterlockedExchange(FCIPSession,FCIPSession);
  if aux=0 then begin
    if Assigned(PCommPort) and PCommPort.ReallyActive then begin
      sessionReq:=TEIPRegSessionPDU.Create;
      try
        aux2:=sessionReq.getPacket;
        PCommPort.IOCommandSync(iocWriteRead,Length(aux2),aux2,28,PDriverID,0,@iopkg,nil,nil);
        if (iopkg.ReadIOResult=iorOK) and (iopkg.Received=28) then begin
          responsepkg:=TEIPEncapsulationHeader.CreateFromBytes(iopkg.BufferToRead,aux3,0);
          try
            if (responsepkg.Command=sessionReq.Command) and (responsepkg.SessionHandler<>0) then begin
              InterlockedExchange(FCIPSession, responsepkg.SessionHandler);
              InterlockedExchange(Result,      FCIPSession);
            end;
          finally
            responsepkg.Free;
          end;
        end;
      finally
        sessionReq.Free;
      end;
    end;
  end else
    Result:=aux;
end;

function TLGXDriver.GetConnectionID: LongWord;
var
  aux: Cardinal;
  EIPHeaderRRCmd: TEIPSendRRDataPDU;
  cfpunconnected: TCPFUnconnectedDataItem;
  foreq: TCIP_ForwardOpen_Req;
  Session, aConnID: LongWord;
  aux2: BYTES;
  iopkg: TIOPacket;
  aux3: Integer = 0;
  aux4:TPDU;
  rplypkg: TEIPSendRRDataPDU absolute aux4;
  a1, a2, a3, a4, a5, a6, a7, a8, a9: Boolean;
begin
  Result:=0;
  Session:=OpenSession;
  if Session=0 then begin
    InterlockedExchange(FConnectionID, 0);
  end else begin
    aux:=InterlockedExchange(FConnectionID,FConnectionID);
    if aux=0 then begin
      if Assigned(PCommPort) and PCommPort.ReallyActive then begin
        EIPHeaderRRCmd:=TEIPSendRRDataPDU.Create;
        try
          //build the packet.
          EIPHeaderRRCmd.SessionHandler:=Session;
          EIPHeaderRRCmd.SendRRDataCmdData.InterfaceHandle:=0;
          EIPHeaderRRCmd.SendRRDataCmdData.Timeout:=1;
          EIPHeaderRRCmd.SendRRDataCmdData.EncapsuledCPF.Add(TCPFNullAddressItem.Create);
          cfpunconnected:=TCPFUnconnectedDataItem.Create;
          foreq:=TCIP_ForwardOpen_Req.Create;
          foreq.ForwardOpenTarget:=CONNECTION_MANAGER;
          if foreq.ForwardOpenPDU is TCMForwardOpenReqPDU then begin
            SetLength(TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).FPath,2);
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).Priority:=$0a; //timebase 1024ms
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).TimeOut_Ticks:=30; //5x1024ms= ?
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).OT_ConnID:=0;
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).TO_ConnID:=GetConnID;//$12345678;
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).ConnSerialNumber:=GetConnSerialNumber;//$6789;
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).OriginatorVendorID:=$0406;
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).OriginatorSerialNumber:=GetOriginatorSerialNumber;//$98765432;
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).TimeOutMultiplier := 1;
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).TO_Parameters:=$43f8;
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).TO_RPI:=30000*1000; //uS
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).OT_Parameters:=$43f8;
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).OT_RPI:=30000*1000; //uS
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).Transport:=$A3;

            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).FPath[0]:=1;
            TCMForwardOpenReqPDU(foreq.ForwardOpenPDU).FPath[1]:=0;

          end;
          cfpunconnected.Add(foreq);
          EIPHeaderRRCmd.SendRRDataCmdData.EncapsuledCPF.Add(cfpunconnected);

          aux2:=EIPHeaderRRCmd.getPacket;
          PCommPort.IOCommandSync(iocWriteRead,Length(aux2),aux2,70,PDriverID,0,@iopkg,nil,nil);
          if (iopkg.ReadIOResult=iorOK) and (iopkg.Received=70) then begin
            try
              aux4:=TEIPEncapsulationHeader.CreatePDUClassFromBytes(iopkg.BufferToRead,aux3,0);
            except
              aux4:=nil;
              exit;
            end;

            try
              if Assigned(rplypkg) and
                 (rplypkg is TEIPSendRRDataPDU) and
                 (rplypkg.Command=EIPHeaderRRCmd.Command) and
                 (rplypkg.SessionHandler=Session) and
                 (rplypkg.SendRRDataCmdData.EncapsuledCPF.ItemCount=2) and
                 (rplypkg.SendRRDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFUnconnectedDataItem) and
                 (TCPFUnconnectedDataItem(rplypkg.SendRRDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIP_ForwardOpen_Reply) and
                 (TCIP_ForwardOpen_Reply(TCPFUnconnectedDataItem(rplypkg.SendRRDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket).Status=0)and
                 (TCIP_ForwardOpen_Reply(TCPFUnconnectedDataItem(rplypkg.SendRRDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket).ForwardOpenReplyPDU is TCMForwardOpenSuccessfulReplyPDU)
              then begin

                aConnID := TCMForwardOpenSuccessfulReplyPDU(TCIP_ForwardOpen_Reply(TCPFUnconnectedDataItem(rplypkg.SendRRDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket).ForwardOpenReplyPDU).OT_ConnID;
                InterlockedExchange(FConnectionID, aConnID);
                InterlockedExchange(Result, aConnID);
              end;
            finally
              if Assigned(aux4) then
                aux4.Free;
            end;
          end;
        finally
          EIPHeaderRRCmd.Free; //free all objects used to built the packet
        end;
      end;
    end else
      Result:=aux;
  end;
end;



procedure TLGXDriver.LoadLGXTagList;
var
  aConnID:TPortUniqueID;
  plcData:PLGXPLCResources;

  function LoadUDTAndAddChildItemsToTagList(aUDT:TUDTID; const StructTagPath:String):Boolean;
    procedure LoadUDTFromPLC(aUDT:TUDTID);
    var
      itensOffset, h, i, a, f, idx: Integer;
      str: String;
      udtidx, aux1: LongInt;
      auxrec: Pudt_entry;
      aux: PUDT_field_entry;
      structAttr: TCIPGetAttrListSrvReq;
      structAttrRply, structItemsRply: TPDU;
      rply: TCIPGetAttrListSrvRply;
      structItemsReq: TCIPReadTagStructInfoReq;
      itemsRply: TCIPReadTagRply;
      buff: BYTES;
      itemsRplySt: Byte;
    begin
      if plcData^.UDTList.Find(aUDT, idx) then exit;
      structAttr:=TCIPGetAttrListSrvReq.Create;
      structAttr.StructID:=aUDT;
      structAttr.AddAttr(1);
      structAttr.AddAttr(2);
      structAttr.AddAttr(4);
      structAttr.AddAttr(5);

      structAttrRply := SendConnectedDataItem(TCIPPDU(structAttr));
      try
        if (structAttrRply is TEIPSendUnitDataPDU) and
           (TEIPSendUnitDataPDU(structAttrRply).FLength>0) and
           Assigned(TEIPSendUnitDataPDU(structAttrRply).SendUnitDataCmdData) and
           (TEIPSendUnitDataPDU(structAttrRply).SendUnitDataCmdData.EncapsuledCPF.CPFItemCount=2) and
           (TEIPSendUnitDataPDU(structAttrRply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFConnectedDataItem) and
           (TCPFConnectedDataItem(TEIPSendUnitDataPDU(structAttrRply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIPGetAttrListSrvRply)
        then begin
          rply:=TCIPGetAttrListSrvRply(TCPFConnectedDataItem(TEIPSendUnitDataPDU(structAttrRply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket);
          New(auxrec);
          udtidx:=plcData^.UDTList.Add(aUDT, auxrec);

          plcData^.UDTList.Data[udtidx]^.id := aUDT;
          for a:=0 to rply.DecodedAtrrCount-1 do begin
            case rply.DecodedAttr[a].Attribute of
              1: plcData^.UDTList.Data[udtidx]^.checksum        := rply.DecodedAttr[a].Value;
              2: plcData^.UDTList.Data[udtidx]^.num_fields      := rply.DecodedAttr[a].Value;
              4: plcData^.UDTList.Data[udtidx]^.definition_size := rply.DecodedAttr[a].Value;// * 4; //Attribute 4, pg 48 1756-pm20_-en-p.pdf: size in 32 bit words..
              5: plcData^.UDTList.Data[udtidx]^.instance_size   := rply.DecodedAttr[a].Value;
              else writeln('FIX-ME: unknown attribute ',rply.DecodedAttr[a].Attribute,' ',{$i %FILE%},':',{$i %LINE%});
            end;
          end;

          //agora vamos aos itens...
          itensOffset:=0;
          SetLength(buff ,0);
          repeat
            structItemsReq:=TCIPReadTagStructInfoReq.Create;
            structItemsReq.StructID:=aUDT;
            structItemsReq.Offset:=itensOffset;
            if ((plcData^.UDTList.Data[udtidx]^.definition_size*4) - itensOffset)>498 then
              aux1:=min(500, ((plcData^.UDTList.Data[udtidx]^.definition_size*4) - itensOffset)-16)
            else
              aux1:=((plcData^.UDTList.Data[udtidx]^.definition_size*4) - itensOffset) - 16; //somebody can explay why 16 ?  I only known that it works

            structItemsReq.NumBytesToRead:=aux1;

            structItemsRply := SendConnectedDataItem(TCIPPDU(structItemsReq));
            try
              if (structItemsRply is TEIPSendUnitDataPDU) and
                 (TEIPSendUnitDataPDU(structItemsRply).FLength>0) and
                 Assigned(TEIPSendUnitDataPDU(structItemsRply).SendUnitDataCmdData) and
                 (TEIPSendUnitDataPDU(structItemsRply).SendUnitDataCmdData.EncapsuledCPF.CPFItemCount=2) and
                 (TEIPSendUnitDataPDU(structItemsRply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFConnectedDataItem) and
                 (TCPFConnectedDataItem(TEIPSendUnitDataPDU(structItemsRply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIPReadTagRply)
              then begin
                itemsRply:=TCIPReadTagRply(TCPFConnectedDataItem(TEIPSendUnitDataPDU(structItemsRply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket);
                itemsRplySt:=itemsRply.Status;

                h:=Length(buff);
                SetLength(buff,h+Length(itemsRply.ReplyData));
                Move(itemsRply.ReplyData[0],buff[h],Length(itemsRply.ReplyData));
                inc(itensOffset, Length(itemsRply.ReplyData));
              end else begin
                itemsRplySt:=254;
              end;
            finally
              if Assigned(structItemsRply) then
                structItemsRply.Free;
              if Assigned(structItemsReq) then
                structItemsReq.Free;
            end;
          until itemsRplySt<>6;

          //starts the buffer decodings...
          //if(FLGXUDTList.Data[udtidx]^.id <> aUDT) then begin
          //  writeln('FIX-ME: UDT ID ',FLGXUDTList.Data[udtidx]^.id,' mismatch. Expected: ',aUDT,' ',{$i %FILE%},':',{$i %LINE%});
          //end;

          // first section is the field type and size info for all fields.
          itensOffset:=0;
          for f:=0 to plcData^.UDTList.Data[udtidx]^.num_fields-1 do begin
            new(aux);
            FillByte(aux^,SizeOf(aux),0);

            aux^.metadata := PWord(@buff[itensOffset])^;
            inc(itensOffset, 2);

            aux^.aType := PWord(@buff[itensOffset])^;
            inc(itensOffset, 2);

            aux^.offset := PLongint(@buff[itensOffset])^;
            inc(itensOffset, 4);

            plcData^.UDTList.Data[udtidx]^.fields.Add(aux);
          end;

          str:='';
          f:=-1; //first item is the structure name...
          for i:=itensOffset to High(buff) do begin
            if buff[i]=0 then begin
              if (f>=0) and (f<plcData^.UDTList.Data[udtidx]^.fields.Count) then begin
                plcData^.UDTList.Data[udtidx]^.fields[f]^.name:=str;
              end;
              inc(f);
              Str:='';
              if f>=plcData^.UDTList.Data[udtidx]^.fields.Count then begin
                break;
              end;
            end else
              str:=str+Chr(buff[i]);
          end;

          { TODO : Verificar o pq do -7 }
          if i<High(buff)-7 then
             writeln('FIX-ME: Why we had stopped before the end? Stopped at offset ',i,' expected offset ',High(buff)-7,' - ',{$i %FILE%},':',{$i %LINE%});
        end;
      finally
        if Assigned(structAttr) then
          structAttr.Free;
        if Assigned(structAttrRply) then
          structAttrRply.Free;
      end;
    end;

    procedure AddStructItemsToTagList(aUDT:TUDTID; aParentItem:String);
    var
      f, d1: Integer;
      structArray: Boolean;
      tag_entry, tag_entry2:TLGXTag;
      aux: PUDT_field_entry;
    begin
      for f:=0 to plcData^.UDTList.KeyData[aUDT]^.fields.Count-1 do begin

        if ((plcData^.UDTList.KeyData[aUDT]^.fields[f]^.aType and TYPE_IS_SYSTEM)=TYPE_IS_SYSTEM) then continue;

        tag_entry.instance_id := -1;
        tag_entry.name  := aParentItem+'.'+lowercase(plcData^.UDTList.KeyData[aUDT]^.fields[f]^.name);
        tag_entry.aType := plcData^.UDTList.KeyData[aUDT]^.fields[f]^.aType;

        if ((tag_entry.aType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT) then begin
          LoadUDTFromPLC(tag_entry.aType and $FFF);
          AddStructItemsToTagList(tag_entry.aType and $FFF, tag_entry.name);
        end;

        tag_entry.dimensions[0] := 0;//array_dims[0];
        tag_entry.dimensions[1] := 0;//array_dims[1];
        tag_entry.dimensions[2] := 0;//array_dims[2];

        //o item da estrtura é um array...
        if ((tag_entry.aType and TAG_DIM_MASK)<>0) then begin
          tag_entry.dimensions[0] := plcData^.UDTList.KeyData[aUDT]^.fields[f]^.metadata;//array_dims[0];
          tag_entry.elem_count    := tag_entry.dimensions[0];
        end else begin
          tag_entry.elem_count    := 1;
        end;

        //tag_entry.elem_size := plcData^.UDTList.KeyData[aUDT]^.fields[f]^.size; //não funciona...
        tag_entry.elem_size := 1;
        if ((tag_entry.aType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT) then begin
          tag_entry.elem_size := plcData^.UDTList.KeyData[tag_entry.aType and $FFF]^.instance_size;
        end else
          case tag_entry.aType and $FF of
            TAG_CIP_TYPE_SINT, TAG_CIP_TYPE_USINT, TAG_CIP_TYPE_BOOL  : tag_entry.elem_size := 1;
            TAG_CIP_TYPE_INT , TAG_CIP_TYPE_UINT                      : tag_entry.elem_size := 2;
            TAG_CIP_TYPE_DINT, TAG_CIP_TYPE_UDINT, TAG_CIP_TYPE_REAL  : tag_entry.elem_size := 4;
            TAG_CIP_TYPE_LINT, TAG_CIP_TYPE_ULINT, TAG_CIP_TYPE_LREAL : tag_entry.elem_size := 8;
            else                                                         tag_entry.elem_size := plcData^.UDTList.KeyData[aUDT]^.fields[f]^.size;
          end;

        tag_entry.num_dimensions := ((tag_entry.aType and TAG_DIM_MASK) shr 13);

        {$IFDEF SHOWTAGLIST}
        writeln(Format('%4.4x %s',[tag_entry.aType, tag_entry.name]));
        {$ENDIF}

        plcData^.TagList.Add(lowercase(tag_entry.name), tag_entry);
        plcData^.LastTagAdded:=tag_entry.name;

        //é uma array?
        if ((tag_entry.aType and TAG_DIM_MASK)<>0) then begin
          structArray:=((tag_entry.aType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT) and ((tag_entry.aType and TYPE_IS_SYSTEM)<>TYPE_IS_SYSTEM);
          for d1:=0 to tag_entry.dimensions[0] do begin
            if structArray then begin
              LoadUDTAndAddChildItemsToTagList(tag_entry.aType and $FFF,tag_entry.name+'['+d1.ToString+']');
            end else begin
              tag_entry2.instance_id    := -1;
              tag_entry2.name           := tag_entry.name+'['+d1.ToString+']';
              tag_entry2.aType          := tag_entry.aType and $FF;
              {$IFDEF SHOWTAGLIST}
              writeln(Format('%4.4x %s',[tag_entry2.aType, tag_entry2.name]));
              {$ENDIF}
              tag_entry2.elem_count     := 1;
              tag_entry2.elem_size      := tag_entry.elem_size;
              tag_entry2.num_dimensions := 0;
              tag_entry2.dimensions[0]  := 0;//array_dims[0];
              tag_entry2.dimensions[1]  := 0;//array_dims[1];
              tag_entry2.dimensions[2]  := 0;//array_dims[2];
              plcData^.TagList.Add(lowercase(tag_entry2.name), tag_entry2);
              plcData^.LastTagAdded:=tag_entry2.name;
            end;
          end;
        end;
      end;
      SendPingCmd;
    end;

  var
    idx: Integer;
  begin
    if plcData^.UDTList.Find(aUDT, idx) then
      AddStructItemsToTagList(aUDT,StructTagPath)
    else begin
      LoadUDTFromPLC(aUDT);
      if plcData^.UDTList.Find(aUDT, idx) then
        AddStructItemsToTagList(aUDT,StructTagPath)
      else
        writeln('FIX-ME: Cant find the structure even in the second try? ',{$i %FILE%},':',{$i %LINE%})
    end;
    SendPingCmd;
  end;

  procedure get_list(prefix:String; tag_list:TLGXTagMap; prog_list:TLGXProgramList);
  var
    CIPListTagSrv: TCIPListTagsReq;
    response: TPDU;
    cipListRply: TCIPListTagsRply;
    responseStatus: Byte;
    tr, tr2: TLGXTag;
    lastInstanceID: LongWord = 0;
    t, i, d1, d2, d3: Integer;
  begin
    repeat
      CIPListTagSrv:=TCIPListTagsReq.Create;
      if Trim(prefix)<>'' then  CIPListTagSrv.FromProgram:=prefix;

      CIPListTagSrv.FromInstance:=lastInstanceID;

      response:=SendConnectedDataItem(TCIPPDU(CIPListTagSrv)); //CIPListTagSrv é destruido ai...;
      try
        if (response is TEIPSendUnitDataPDU) and
           (TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItemCount=2) and
           (TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFConnectedDataItem) and
           (TCPFConnectedDataItem(TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIPListTagsRply)
        then begin
          cipListRply:=TCIPListTagsRply(TCPFConnectedDataItem(TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket);
          responseStatus:=cipListRply.Status;

          for t:=0 to cipListRply.LGXTagCount-1 do begin
            tr:=cipListRply.LGXTag[t];
            lastInstanceID:=tr.instance_id+1;
            if pos('program:',lowercase(tr.name))>0 then begin
              if Assigned(prog_list) then
                prog_list.Add(tr.name)
            end else begin
              if ((tr.aType and TYPE_IS_SYSTEM)<>TYPE_IS_SYSTEM) then begin

                tr.elem_count := 1;
                if prefix.Trim<>'' then
                  tr.name := prefix+'.'+tr.name;

                tr.num_dimensions := ((tr.aType and TAG_DIM_MASK) shr 13);

                for i:=0 to tr.num_dimensions-1 do
                  tr.elem_count := tr.elem_count * tr.dimensions[i];

                plcData^.TagList.add(tr.name.ToLower, tr);
                plcData^.LastTagAdded:=tr.name;
                {$IFNDEF WINDOWS}
                writeln(IntToHex(tr.aType,4),' - ', tr.name,' ec=',tr.elem_count,' es=',tr.elem_size);
                {$ENDIF}

                //é um array, então adiciona os acessos diretos aos elementos da array.
                if tr.num_dimensions in [1..3] then begin
                  tr2                := tr;
                  tr2.name           := tr.name;
                  tr2.aType          := tr.aType;
                  tr2.elem_count     := 1;
                  tr2.num_dimensions := 0;
                  tr2.dimensions[0]  := 0;
                  tr2.dimensions[1]  := 0;
                  tr2.dimensions[2]  := 0;


                  for d1:=0 to tr.dimensions[0]-1 do begin
                    if tr.num_dimensions=1 then begin
                      tr2.name:=tr.name+'['+inttostr(d1)+']';
                      plcData^.TagList.Add(tr2.name.ToLower, tr2);
                      plcData^.LastTagAdded:=tr2.name;
                      {$IFDEF SHOWTAGLIST}
                      writeln(IntToHex(tr2.instance_id,4),' - ', tr2.name,' ec=',tr2.elem_count,' es=',tr2.elem_size);
                      {$ENDIF}
                      if ((tr2.aType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT) and ((tr2.aType and TYPE_IS_SYSTEM)<>TYPE_IS_SYSTEM) then begin
                        LoadUDTAndAddChildItemsToTagList(tr2.aType AND $FFF, tr2.name);
                      end;
                    end else begin
                      for d2:=0 to tr.dimensions[1]-1 do begin
                        if tr.num_dimensions=2 then begin
                          tr2.name:=tr.name+'['+inttostr(d1)+','+inttostr(d2)+']';
                          plcData^.TagList.Add(tr2.name.ToLower, tr2);
                          plcData^.LastTagAdded:=tr2.name;
                          {$IFDEF SHOWTAGLIST}
                          writeln(IntToHex(tr2.instance_id,4),' - ', tr2.name,' ec=',tr2.elem_count,' es=',tr2.elem_size);
                          {$ENDIF}
                          if ((tr2.aType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT) and ((tr2.aType and TYPE_IS_SYSTEM)<>TYPE_IS_SYSTEM) then begin
                            LoadUDTAndAddChildItemsToTagList(tr2.aType AND $FFF, tr2.name);
                          end;
                        end else begin
                          for d3:=0 to tr.dimensions[1]-1 do begin
                            tr2.name:=tr.name+'['+inttostr(d1)+','+inttostr(d2)+','+inttostr(d3)+']';
                            plcData^.TagList.Add(tr2.name.ToLower, tr2);
                            plcData^.LastTagAdded:=tr2.name;
                            {$IFDEF SHOWTAGLIST}
                            writeln(IntToHex(tr2.instance_id,4),' - ', tr2.name,' ec=',tr2.elem_count,' es=',tr2.elem_size);
                            {$ENDIF}
                            if ((tr2.aType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT) and ((tr2.aType and TYPE_IS_SYSTEM)<>TYPE_IS_SYSTEM) then begin
                              LoadUDTAndAddChildItemsToTagList(tr2.aType AND $FFF, tr2.name);
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                end else begin
                  if ((tr.aType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT) and ((tr.aType and TYPE_IS_SYSTEM)<>TYPE_IS_SYSTEM) then
                    LoadUDTAndAddChildItemsToTagList(tr.aType AND $FFF, tr.name);
                end;
              end;
            end;
          end;
        end else
          Break;
      finally
        if assigned(CIPListTagSrv) then
          CIPListTagSrv.Free;
        if Assigned(response) then
          response.Free;
      end;
      SendPingCmd;
    until responseStatus<>6; //6 = ainda tem mais tag pra ser lido...
  end;

var
  FLGXPrgList: TLGXProgramList;
  i, idx: Integer;
begin
  if Assigned(PCommPort) and PCommPort.ReallyActive and (GetConnectionID<>0) then begin
    aConnID := PCommPort.getPortId;
    try
      FLGXGlobalMutex.Enter;
      if FPLCData.Find(aConnID,idx) then begin
        plcData:=FPLCData.data[idx];
      end else begin
        new(plcData);
        plcData^.Mutex:=TCriticalSection.Create;
        plcData^.FullyLoaded:=false;
        plcData^.TagList:=TLGXTagMap.Create;
        plcData^.TagList.Sorted:=true;
        plcData^.UDTList:=TUDTList.Create;
        plcData^.UDTList.Sorted:=true;
        FPLCData.Add(aConnID,plcData);
      end;
    finally
      FLGXGlobalMutex.Leave;
    end;

    while not plcData^.Mutex.TryEnter do begin
      SendPingCmd;
      Sleep(100);
    end;

    try
      if plcData^.FullyLoaded then exit;

      try
        FLGXPrgList:=TLGXProgramList.Create;

        get_list('', plcData^.TagList, FLGXPrgList);

        for i:=0 to FLGXPrgList.Count-1 do begin
          get_list(FLGXPrgList.Items[i], plcData^.TagList, nil);
        end;
      finally
        plcData^.FullyLoaded:=true;
        {$IFNDEF WINDOWS}
        writeln('Tag list fully loaded from controller');
        {$ENDIF}
        if Assigned(FLGXPrgList) then FLGXPrgList.Free;
      end;
    finally
      plcData^.Mutex.Leave;
    end;
  end;
end;

function TLGXDriver.SendConnectedDataItem(var aCIPPDU: TCIPPDU): TPDU;
var
  EIPHdrUnDt: TEIPSendUnitDataPDU;
  cpfConID: TCPFAddressItem;
  cpfConDI: TCPFConnectedDataItem;
  aux2: BYTES;
  iopkg, iopkg2: TIOPacket;
  aux1: Integer;
  helperpkg: TEIPEncapsulationHeader;
begin
  Result:=nil;
  if (GetConnectionID<>0) and Assigned(aCIPPDU) then begin
    EIPHdrUnDt:=TEIPSendUnitDataPDU.Create;
    try
      EIPHdrUnDt.SessionHandler:=FCIPSession;

      cpfConID:=TCPFAddressItem.Create;
      cpfConID.ConnectionID:=FConnectionID;
      EIPHdrUnDt.SendUnitDataCmdData.EncapsuledCPF.Add(cpfConID);

      FLastCmdSentTimstamp := Now;

      cpfConDI:=TCPFConnectedDataItem.Create; //?
      {$PUSH}
      {$R-}
      Inc(CIPPkgID);
      {$POP}
      cpfConDI.CIPSequence:=CIPPkgID;
      cpfConDI.Add(aCIPPDU);
      aCIPPDU:=nil;
      EIPHdrUnDt.SendUnitDataCmdData.EncapsuledCPF.Add(cpfConDI);

      aux2:=EIPHdrUnDt.getPacket;
      PCommPort.IOCommandSync(iocWriteRead,Length(aux2),aux2, 24, PDriverID, 24, @iopkg,nil,nil);
      if (iopkg.ReadIOResult=iorOk) and (iopkg.Received=24) then begin
         helperpkg:=TEIPEncapsulationHeader.CreateFromBytes(iopkg.BufferToRead,aux1,0,0,false);
         try
           if helperpkg.Status=0 then begin
             PCommPort.IOCommandSync(iocRead,0,nil, helperpkg.PkgLength, PDriverID, 0 , @iopkg2,nil,nil);
             if (iopkg2.ReadIOResult=iorOk) and (iopkg2.Received=helperpkg.PkgLength) then begin
               SetLength(aux2,24+helperpkg.PkgLength);
               Move(iopkg.BufferToRead[0],aux2[0], 24);
               Move(iopkg2.BufferToRead[0],aux2[24], helperpkg.PkgLength);
               Result:=TEIPEncapsulationHeader.CreatePDUClassFromBytes(aux2,aux1,0);
             end;
           end else begin
             writeln('FIX-ME: Ethernet/IP encapsulation packet returned status ',inttohex(helperpkg.Status),', WHY?. The connection will be renewed. ',{$i %FILE%},':',{$i %LINE%});
             PCommPort.RenewHandle;
           end;
         finally
           helperpkg.Free;
         end;
      end;
    finally
      EIPHdrUnDt.Free;
    end;
  end;
end;

procedure TLGXDriver.SendPingCmd;
var
  fakeReq: TCIPReadTagReq;
  reply: TPDU;
begin
  if MilliSecondsBetween(Now, FLastCmdSentTimstamp)>4000 then begin //internal timeout - 1s
    fakeReq:=TCIPReadTagReq.Create;
    fakeReq.ReqCount:=0;
    fakeReq.RequestPath:='ping_tag';

    try
      reply := SendConnectedDataItem(TCIPPDU(fakeReq));
    finally
      if Assigned(fakeReq) then
        fakeReq.Free;
      if Assigned(reply) then
        reply.Free;
    end;
  end;
end;

function TLGXDriver.TestDone: Boolean;
begin
  {$IFDEF TEST}
  Result:=(SystemTicks-FTimestamp)>=1800000000000;
  {$ELSE}
  Result:=false;
  {$ENDIF}
end;

procedure TLGXDriver.BuildTagRec(aTag: AnsiString; aTagSize: Integer; out
  tr: TTagRec);
var
  idx: Integer;
  plcData: PLGXPLCResources;
begin
  FillByte(tr,SizeOf(TTagRec),0);
  tr.Path:=aTag;
  tr.Count:=1;
  tr.Size:=aTagSize;

  if Assigned(PCommPort) then begin
    FLGXGlobalMutex.Enter;
    try
      if FPLCData.Find(PCommPort.getPortId, idx) then begin
        FPLCData.Data[idx]^.Mutex.Enter;
        plcData:=FPLCData.Data[idx];
      end else
        exit;
    finally
      FLGXGlobalMutex.Leave;
    end;

    try

      if plcdata^.TagList.Find(aTag, idx) then begin
        tr.Count:=aTagSize div plcdata^.TagList.Data[idx].elem_size;
      end;
    finally
      plcdata^.Mutex.Leave;
    end;

  end;
end;

function TLGXDriver.ValidTagStr(ATagStr: String): Boolean;
var
  c, Regra, arrayAccess: Integer;
  aux: Boolean;
  aChar: Char;
begin
  //Valida o nome do tag
  //
  //dados
  //dados.campo
  //dados.campo[1]
  //dados[1]
  //dados[1,2]
  //dados[1,2,3]
  //dados[1].campo
  //dados[1,2].campo
  //dados[1,2,3].campo
  //dados[1].campo[1]
  //dados[1,2].campo[1]
  //dados[1,2,3].campo[1]

  Result:=true;
  Regra:=0;
  arrayAccess:=0;
  for c:=1 to  Length(ATagStr) do begin
    aChar := ATagStr[c];
    case Regra of
      0: begin aux := ATagStr[c] in ['a'..'z','A'..'Z','_']; Regra:=1; end;
      1: aux := ATagStr[c] in ['0'..'9','a'..'z','A'..'Z','_',':','.','['];
      2: aux := ATagStr[c] in ['0'..'9'];
      3: aux := ATagStr[c] in ['0'..'9',']',','];
      4: aux := ATagStr[c] = '.';
    end;

    case aChar of
      '.',':': begin
        Regra := 0;
        if (c>=Length(ATagStr)) or (not (ATagStr[c+1] in ['a'..'z','A'..'Z','_'])) then
          exit(false);
      end;

      '[': begin
        if (c>=Length(ATagStr)) or (not (ATagStr[c+1] in ['0'..'9'])) then
          exit(false);

        if arrayAccess>0 then
          Exit(false);

        Regra := 2;

        inc(arrayAccess);
      end;
      '0'..'9': begin
        if Regra=2 then
          Regra:=3;
      end;

      ',': begin
        if arrayAccess<=0 then
          Exit(false);

        if (c>=Length(ATagStr)) or (not (ATagStr[c+1] in ['0'..'9'])) then
          exit(false);

        Regra:=2;
      end;
      ']': begin
        if arrayAccess<=0 then
          Exit(false);

        if (c<>Length(ATagStr)) and
           (((c<=(Length(ATagStr)-2)) and  (ATagStr[c+1]<>'.') and (not (ATagStr[c+2] in ['a'..'z','A'..'Z','_']))) or
            (c>(Length(ATagStr)-2))) then
          exit(false);

        Regra :=4;
        Dec(arrayAccess);
      end;
    end;

    Result:=Result and aux;
    if Result=false then break;
  end;

  if (arrayAccess>0) then
    Result:=false;
end;

function TLGXDriver.GetTagProperts(TagObj: TTag; var aTagPath: String;
  var aSize, aRefreshRate: Integer): Boolean;
var
  found:Boolean;
begin
  found := false;
  Result := false;
  //PLCTagNumber
  if (not found) and (TagObj is TPLCTagNumber) then begin
    found        := true;
    aTagPath     := LowerCase(TPLCTagNumber(TagObj).LongAddress);
    aSize        := TPLCTagNumber(TagObj).TagSizeOnProtocol;
    aRefreshRate :=TPLCTagNumber(TagObj).RefreshTime;
    Result       := found;
  end;

  //TPLCBlock and TPLCStruct
  if (not found) and (TagObj is TPLCBlock) then begin
    found        := true;
    aTagPath     := LowerCase(TPLCBlock(TagObj).LongAddress);
    aSize        := TPLCBlock(TagObj).TagSizeOnProtocol;
    aRefreshRate := TPLCBlock(TagObj).RefreshTime;
    Result       := found;
  end;

  //TPLCString
  if (not found) and (TagObj is TPLCString) then begin
    found        := true;
    aTagPath     := LowerCase(TPLCString(TagObj).LongAddress);
    aSize        := TPLCString(TagObj).Size;
    aRefreshRate := TPLCString(TagObj).RefreshTime;
    Result       := found;
  end;
end;

procedure TLGXDriver.DoAddTag(TagObj: TTag; TagValid: Boolean);
var
  aTagPath: String;
  aux:PLPTTagRec;
  aTagSize, aRefreshRate, idx: Integer;
  aux1, aux2: Boolean;
begin
  //at this point, the mutexes are acquired
  aux1:=GetTagProperts(TagObj, aTagPath, aTagSize, aRefreshRate);
  aux2:=ValidTagStr(aTagPath);
  TagValid:=aux1 and aux2;

  if aux2=false then writeln('APP FIX, TAG: "',aTagPath,'" is invalid',' ',{$i %FILE%},':',{$i %LINE%});

  try
    LoadLGXTagList;


    //rockwell e todo o ecosistema sendo rockwell...
    // toda aquela merda ali em baixo não funciona,
    // a dll trava, quando uma pancada de tags é
    //adicionada. Então assume que a merda do tag
    //é bom, e adiciona sapora pra ler depois... OU NÃO...
    TagValid:=true;
    if FTagList.Find(aTagPath,idx) then begin
      FTagList.Data[idx]^.mm.AddAddress(0, aTagSize, 1, aRefreshRate);
    end else begin
      New(aux); //cria a zona toda.
      aux^.mm.MaxBlockItems:=2097152;
      aux^.mm.MaxHole      :=2097152;
      aux^.mm.AddAddress(0, aTagSize, 1, aRefreshRate);
      aux^.ExistsOnPLC:=false;
      FTagList.Add(aTagPath, aux);
    end;

    //if TagValid then begin
    //  if (not Assigned(FLGXTagList)) or (FLGXTagList.Count<=0) then begin
    //    LoadLGXTagList;
    //  end;
    // TagExists:=FLGXTagList.Find(aTagPath,idx) and (aTagSize<=FLGXTagList.Data[idx].raw_size);
    //
    //  if TagExists then begin
    //    if FLGXTagList.Data[idx].elem_size>0 then begin
    //      ecount:=(aTagSize div FLGXTagList.Data[idx].elem_size)+IfThen((aTagSize mod FLGXTagList.Data[idx].elem_size)>0,1);
    //      esize :=ecount*FLGXTagList.Data[idx].elem_size;
    //    end else begin
    //      ecount:=aTagSize;
    //      esize :=aTagSize;
    //    end;
    //
    //
    //    if ecount>FLGXTagList.Data[idx].elem_count then begin
    //      TagValid:=false;
    //      {$IFNDEF WINDOWS}
    //      writeln('Trying to map ',aTagPath,' which is bigger (',aTagSize,' bytes) than in PLC tag (',esize,' bytes');
    //      {$ENDIF}
    //      exit;
    //    end;
    //
    //    if FTagList.Find(aTagPath,idx) then begin
    //      FTagList.Data[idx]^.mm.AddAddress(0, esize, 1, aRefreshRate);
    //    end else begin
    //      New(aux); //cria a zona toda.
    //      aux^.mm.MaxBlockItems:=2097152;
    //      aux^.mm.MaxHole      :=2097152;
    //      aux^.mm.AddAddress(0,esize, 1, aRefreshRate);
    //      aux^.tagpathname:=BuildLibPLCTagName+'name='+aTagPath+'&elem_count='+IntToStr(ecount);
    //      aux^.ExistsOnPLC:=true;
    //      idx:=FTagList.Add(aTagPath, aux);
    //    end;
    //  end else begin
    //    //tenta criar o tag da maneira convencional
    //
    //    if FTagList.Find(aTagPath,idx) then begin
    //      TagValid := aTagSize<=FTagList.Data[idx]^.mm.Size;
    //      if TagValid then
    //        FTagList.Data[idx]^.mm.AddAddress(0, FTagList.Data[idx]^.mm.Size, 1, aRefreshRate);
    //    end else begin
    //      fullTagPath:= BuildLibPLCTagName+'name='+aTagPath; //vamos de configuração padrão...
    //      tagHandler:=plc_tag_create(PChar(fullTagPath), DefaultTimeout);
    //
    //      try
    //        if tagHandler>0 then begin
    //          esize:=plc_tag_get_size(tagHandler);
    //
    //          if esize<=0 then
    //            esize:=1;
    //
    //          if esize>=aTagSize then begin
    //            New(aux); //cria a zona toda.
    //            aux^.mm.MaxBlockItems:=2097152;
    //            aux^.mm.MaxHole      :=2097152;
    //            aux^.mm.AddAddress(0, esize, 1, aRefreshRate);
    //            aux^.tagpathname:=fullTagPath;
    //            aux^.ExistsOnPLC:=false;
    //            FTagList.Add(aTagPath, aux);
    //            TagValid:=true;
    //          end else
    //            TagValid:=false;
    //        end else
    //          TagValid:=false;
    //      finally
    //        //if FirstTagCreated then
    //          plc_tag_destroy(tagHandler);
    //
    //        //FirstTagCreated:=true;
    //      end;
    //
    //    end;
    //  end;
    //end;
  finally
    inherited DoAddTag(TagObj, TagValid);
  end;
end;

procedure TLGXDriver.DoDelTag(TagObj: TTag);
var
  PreviousSize: LongInt;
  aux: PLPTTagRec;
  aTagPath, fullTagPath: String;
  aTagSize, aRefreshRate, idx: Integer;
  TagValid: Boolean;
  aux2: LongWord;
begin
  //at this point, the mutexes are acquired
  TagValid:=GetTagProperts(TagObj, aTagPath, aTagSize, aRefreshRate);

  if TagValid and FTagList.Find(aTagPath,idx) then begin
    PreviousSize:=FTagList.Data[idx]^.mm.Size;

    FTagList.Data[idx]^.mm.RemoveAddress(0, aTagSize, 1);

    if FTagList.Data[idx]^.mm.Size<>PreviousSize then begin
      if FTagList.Data[idx]^.mm.Size>0 then begin
        //TODO: PRECISAR REVER SAPORA AQUI!
        aux2:=FTagList.Data[idx]^.mm.MinScanTime;
      end else begin
        aux:=FTagList.Data[idx];
        Dispose(aux); //destroi o memorymanager e o handler para o tag.
        FTagList.Delete(idx);
      end;
    end;
  end;

  inherited DoDelTag(TagObj);
end;

procedure TLGXDriver.DoGetValue(TagRec: TTagRec; var values: TScanReadRec);
var
  idx, c: Integer;
begin
  if (InterlockedExchange(FTerminating,FTerminating)<>0) or (csDestroying in ComponentState) or TestDone then begin
    InterlockedExchange(FTerminating, 1);
    exit;
  end;

  if FTagList.Find(LowerCase(TagRec.Path), idx) then begin
    if Length(values.Values)<TagRec.Size then
      SetLength(values.Values,TagRec.Size);

    for c:=0 to Length(values.Values)-1 do
      values.Values[c] := 0;

    if TagRec.Size<=FTagList.Data[idx]^.mm.Size then begin
      FTagList.Data[idx]^.mm.GetValues(0,
                                       TagRec.Size,
                                       1,
                                       values.Values,
                                       values.LastQueryResult,
                                       values.ClkMonotonicTStamp);
    end else
      values.LastQueryResult:=ioIllegalRegSize;

    if values.LastQueryResult=ioOk then begin
      values.ReadsOK := 1;
      values.ReadFaults := 0;
    end else begin
      values.ReadsOK := 0;
      values.ReadFaults := 1;
    end;

  end else begin
    values.ClkMonotonicTStamp := GetTickCount64;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
    values.LastQueryResult := ioDriverError;
    SetLength(values.Values,0);
    exit;
  end;

end;

procedure TLGXDriver.DoScanRead(Sender: TObject; var NeedSleep: LongInt);
var
  EntireTagList:TScanList;
  i: Integer;
  tr: TTagRec;
  a, b, c: Boolean;

  procedure AddToTagList(aTag:AnsiString; index, UpdateRate, aSize:LongInt; LastUpdate:TDateTime; NeedUpdate:Boolean);
  var
    info:TScanInfoItem;
  begin
    info.Tag          := aTag;
    info.Index        := index;
    info.LastUpdate   := LastUpdate;
    info.UpdateRate   := UpdateRate;
    info.Size         := aSize;
    info.NeedUpdate   := NeedUpdate;
    info.Read         := false;

    EntireTagList.add(info);
  end;

  procedure ReadQueuedTags;
  var
    multi:TCIPMultiTagReadReq;
    idx, incomingCIPSize, r, addIdx, i2, ItemReq, responseStatus,
      ReqOffset, idx3, aux3: Integer;
    SendNow, fUnknownTag: Boolean;
    ReqCount, delta, s, es, addStrIdx: LongInt;
    single: TCIPReadTagFragReq = nil;
    response: TPDU;
    pkgaux: TCIPMultiTagReadRply;

    auxdataReal: TArrayOfDouble;
    auxdata, reqByID: BYTES;
    pkgaux2: TCIPReadTagFragRply;
    tr2: TLGXTag;
    tn: String;
    plcData: PLGXPLCResources = nil;
    multiList: TStringList;
  begin

    try
      FLGXGlobalMutex.Enter;
      if FPLCData.Find(PCommPort.getPortId,idx) then
        plcData:=FPLCData.data[idx];
    finally
      FLGXGlobalMutex.Leave;
    end;


    SetLength(reqByID,6);
    reqByID[0]:=$20;
    reqByID[1]:=$6b;
    reqByID[2]:=$25;
    reqByID[3]:=$00;
    reqByID[4]:=$00;
    reqByID[5]:=$00;
    incomingCIPSize:=6;
    multi:=TCIPMultiTagReadReq.Create;
    multiList:=TStringList.Create;
    r:=0;
    SendNow:=false;
    ItemReq:=-1;
    try
      while (multi.GetPacketSize<maxPDUSize) and (incomingCIPSize<maxPDUSize) and (r<EntireTagList.Count) do begin
        if Assigned(plcData) then begin
          plcData^.Mutex.Enter;
          try
            if plcData^.TagList.Find(EntireTagList.Items[r].Tag.ToLower, idx) then begin
              s:=EntireTagList.Items[r].Size;
              es:=plcData^.TagList.Data[idx].elem_size;
              tn:=EntireTagList.Items[r].Tag.ToLower;
              ReqCount:=(EntireTagList.Items[r].Size div plcData^.TagList.Data[idx].elem_size)+ifthen((EntireTagList.Items[r].Size mod plcData^.TagList.Data[idx].elem_size)<>0,1);
              delta:=2+6+ReqCount*plcData^.TagList.Data[idx].elem_size;
              if ((incomingCIPSize+delta)<maxPDUSize) then begin
                //toDO check if it supports req by ID
                if false and (plcData^.TagList.Data[idx].instance_id>0) then begin
                  PWord(@reqByID[4])^:=plcData^.TagList.Data[idx].instance_id;
                  addIdx:=multi.AddItem(reqByID,ReqCount);
                  addStrIdx:=multiList.Add('Instance ID: '+plcData^.TagList.Data[idx].instance_id.ToHexString+', count='+ReqCount.ToString);
                end else begin
                  addIdx:=multi.AddItem(EntireTagList.Items[r].Tag,ReqCount);
                  addStrIdx:=multiList.Add(EntireTagList.Items[r].Tag+', count='+ReqCount.ToString);
                end;

                aux3:=multi.GetPacketSize;
                if aux3<maxPDUSize then
                  inc(incomingCIPSize,delta)
                else begin
                  multi.Delete(addIdx);
                  multiList.Delete(addStrIdx);
                  break;
                end;
              end else begin
                if (r=0) and (multi.ReqCount=0) then begin
                  ItemReq:=r;
                  fUnknownTag:=false;
                end;
                break;
              end;
            end else begin
              if multi.ReqCount=0 then begin
                ItemReq:=r;
                fUnknownTag:=true;
              end;
              Break;
            end;
          finally
            plcData^.Mutex.Leave;
          end;
        end else begin
          if multi.ReqCount=0 then begin
            ItemReq:=r;
            fUnknownTag:=true;
          end;
          Break;
        end;
        inc(r);
      end;

      if (multi.ReqCount>0) and (ItemReq=-1) then begin
        //executes a multi request
        NeedSleep:=0;
        response:=SendConnectedDataItem(TCIPPDU(multi));
        try
          if (response is TEIPSendUnitDataPDU) and
             (TEIPSendUnitDataPDU(response).FLength>0) and
             Assigned(TEIPSendUnitDataPDU(response).SendUnitDataCmdData) and
             (TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItemCount=2) and
             (TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFConnectedDataItem) and
             (TCPFConnectedDataItem(TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIPMultiTagReadRply)
          then begin
            pkgaux:=TCIPMultiTagReadRply(TCPFConnectedDataItem(TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket);
            for r:=0 to pkgaux.ReplyCount-1 do begin
              case pkgaux.Reply[r].Status of
                0: begin
                  auxdata:=pkgaux.Reply[r].Data;
                  SetLength(auxdataReal,Length(auxdata));
                  for i2:=0 to High(auxdata) do
                    if (pkgaux.Reply[r].IsStructure=false) and ((pkgaux.Reply[r].DataType and $FF)=TAG_CIP_TYPE_BOOL) then
                      auxdataReal[i2]:=ifthen(auxdata[i2]=0,0,1)
                    else
                      auxdataReal[i2]:=auxdata[i2];

                  if Length(auxdataReal)<EntireTagList.Items[r].Size then
                    FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetValues(0,Length(auxdataReal),1,auxdataReal,ioPartialOk)
                  else begin
                    SetLength(auxdataReal,EntireTagList.Items[r].Size);
                    FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetValues(0,EntireTagList.Items[r].Size,1,auxdataReal,ioOk);
                  end;
                end;
                $04: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioIllegalRequest,true);               //TODO: update tag info...
                $05: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioObjectNotExists,true);              //TODO: update tag info...
                $06: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioDeviceGatewayFailedToRespond,true); //TODO: update tag info...
                $13: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioDeviceGatewayFailedToRespond,true); //TODO: update tag info...
                $26: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioIllegalRequest,true);               //TODO: update tag info...
                $FF: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioIllegalRegSize,true);               //TODO: update tag info...
                else FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioUnknownError,true);                 //TODO: update tag info...
              end;
              if pkgaux.Reply[r].Status<>0 then
                writeln('FIX-ME: Got status ',pkgaux.Reply[r].Status.ToHexString,' while processing tag: ',multiList.Strings[r],' - ',{$i %FILE%},':',{$i %LINE%});
            end;
          end;
        finally
          if Assigned(multi) then
            multi.Free;
          if assigned(response) then
            response.Free;
        end;
      end;

      //se caiu aqui é pq não há informação alguma sobre o tag
      //então, aproveita a leitura fragmentada e atualiza
      //as informaçõe do tag
      // OOOUUUU
      //o tag é muito grande para caber numa simples requisição
      if Assigned(multi) and  (multi.ReqCount=0) and (ItemReq>=0) then begin
        ReqOffset:=0;
        NeedSleep:=0;
        repeat
          single:=TCIPReadTagFragReq.Create;
          single.RequestPath:=EntireTagList.Items[ItemReq].Tag;
          single.ReqOffset:=ReqOffset;
          if fUnknownTag then
            single.ReqCount := 1
          else
            single.ReqCount:=ReqCount;

          response:=SendConnectedDataItem(TCIPPDU(single));
          try
            if (response is TEIPSendUnitDataPDU) and
               (TEIPSendUnitDataPDU(response).FLength>0) and
               Assigned(TEIPSendUnitDataPDU(response).SendUnitDataCmdData) and
               (TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItemCount=2) and
               (TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFConnectedDataItem) and
               (TCPFConnectedDataItem(TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIPReadTagFragRply)
            then begin
              pkgaux2:=TCIPReadTagFragRply(TCPFConnectedDataItem(TEIPSendUnitDataPDU(response).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket);
              responseStatus := pkgaux2.Status;
              case pkgaux2.Status of
                0, $06: begin
                  auxdata:=pkgaux2.Data;
                  SetLength(auxdataReal,Length(auxdataReal)+Length(auxdata));
                  for i2:=0 to High(auxdata) do
                    auxdataReal[ReqOffset+i2]:=auxdata[i2];

                  inc(ReqOffset, Length(auxdata));

                  if pkgaux2.Status=0 then begin

                    if Length(auxdataReal)<>EntireTagList.Items[r].Size then begin
                      writeln('FIX-ME: tag size mismatch',{$i %FILE%},':',{$i %LINE%});
                    end;

                    if Length(auxdataReal)<EntireTagList.Items[r].Size then
                      FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetValues(0,Length(auxdataReal),1,auxdataReal,ioPartialOk)
                    else begin
                      SetLength(auxdataReal,EntireTagList.Items[r].Size);
                      FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetValues(0,EntireTagList.Items[r].Size,1,auxdataReal,ioOk);
                    end;

                    if fUnknownTag and Assigned(plcdata) then begin
                      plcdata^.Mutex.Enter;
                      try
                        if plcdata^.TagList.Find(EntireTagList.Items[ItemReq].Tag, idx3) then
                          writeln('FIX-ME: If you can find me, why we are here? ',{$i %FILE%},':',{$i %LINE%})
                        else begin
                          FillByte(tr2,SizeOf(tr2), 0);
                          tr2.name:=EntireTagList.Items[ItemReq].Tag;
                          tr2.elem_count:=1;
                          tr2.elem_size:=ReqOffset;
                          tr2.instance_id:=-1;
                          if tr2.elem_size<=0 then begin
                            tr2.elem_size := 1;
                            Writeln('FIX-ME: Could not determine the element size in bytes ',{$i %FILE%},':',{$i %LINE%});
                          end;
                          tr2.aType:=pkgaux2.DataType;
                          plcdata^.TagList.add(tr2.name.ToLower, tr2);
                        end;
                      finally
                        plcData^.Mutex.Leave;
                      end;
                    end;
                  end;
                end;
                $04: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioIllegalRequest,true);               //TODO: update tag info...
                $05: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioObjectNotExists,true);              //TODO: update tag info...
                $13: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioDeviceGatewayFailedToRespond,true); //TODO: update tag info...
                $26: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioIllegalRequest,true);               //TODO: update tag info...
                $FF: FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioIllegalRegSize,true);               //TODO: update tag info...
                else FTagList.Data[EntireTagList.Items[r].Index]^.mm.SetFault(0,EntireTagList.Items[r].Size,1,ioUnknownError,true);                 //TODO: update tag info...
              end;

              if not pkgaux2.Status in [$00, $06] then
                Writeln('FIX-ME: got status ',inttohex(pkgaux2.Status),' when reading tag "',EntireTagList.Items[ItemReq].Tag,'" ',{$i %FILE%},':',{$i %LINE%});

            end else begin
              FTagList.Data[EntireTagList.Items[ItemReq].Index]^.mm.SetFault(0,EntireTagList.Items[ItemReq].Size,1,ioCommError,true);
              responseStatus:=255;
            end;
          finally
            if Assigned(single) then
              single.Free;
            if Assigned(response) then
              response.Free;
          end;
        until responseStatus<>6;
      end;
    finally
      if Assigned(multi)  then multi.Free;
      if Assigned(multiList) then multiList.Free;
      if Assigned(single) then single.Free;
    end;
  end;

begin
  if (InterlockedExchange(FTerminating,FTerminating)<>0) or (csDestroying in ComponentState) or TestDone then begin
    InterlockedExchange(FTerminating, 1);
    exit;
  end;

  if Assigned(PCommPort) and PCommPort.ReallyActive and (GetConnectionID<>0) then begin
    LoadLGXTagList;

    try
      EntireTagList:=TScanList.Create;

      for i:=0 to FTagList.Count-1 do begin
        if Length(FTagList.Data[i]^.mm.Blocks)=1 then
          AddToTagList(FTagList.Keys[i],
                       i,
                       FTagList.Data[i]^.mm.Blocks[0].ScanTime,
                       FTagList.Data[i]^.mm.Blocks[0].Size,
                       FTagList.Data[i]^.mm.Blocks[0].LastUpdate,
                       FTagList.Data[i]^.mm.Blocks[0].NeedRefresh)
        else begin
          {$IFNDEF WINDOWS}
          WriteLn('FIX-ME: Tag "',FTagList.Keys[i],'" has more than one internal data block');
          {$ENDIF}
        end;
      end;

      EntireTagList.Sort(@SortGenericTagList);

      NeedSleep := 1;
      if (EntireTagList.Count>0) and (EntireTagList.Items[0].NeedUpdate or PReadSomethingAlways) then
        ReadQueuedTags;

      SendPingCmd;

      EntireTagList.Clear;

    finally
      EntireTagList.Free;
    end;
  end else
    NeedSleep:=1;
end;

function TLGXDriver.DoRead(const tagrec: TTagRec; out Values: TArrayOfDouble;
  Sync: Boolean): TProtocolIOResult;
var
  req: TCIPReadTagFragReq;
  esize, lastOffset, idx, aux1, h, i: Integer;
  auxreply: TPDU;
  reply: TCIPReadTagFragRply;
  RplySt: Byte;
  buff: BYTES;
  RplyDT: Word;
  RplyIsStruct:Boolean;
  plcData: PLGXPLCResources = nil;
begin
  if (InterlockedExchange(FTerminating,FTerminating)<>0) or (csDestroying in ComponentState) or TestDone then begin
    InterlockedExchange(FTerminating, 1);
    exit;
  end;

  lastOffset:=0;
  esize:=-1;

  try
    FLGXGlobalMutex.Enter;
    if FPLCData.Find(PCommPort.getPortId,idx) then
      plcData:=FPLCData.data[idx];
  finally
    FLGXGlobalMutex.Leave;
  end;

  if Assigned(plcData) then begin
    plcData^.Mutex.Enter;
    try
      if  plcData^.TagList.Find(tagrec.Path.ToLower,idx) then
        esize:=plcData^.TagList.Data[idx].elem_size
    finally
      plcData^.Mutex.Leave;
    end;
  end;

  //stills unknown
  //se não conhece o tag, faz a primeira leitura no tamanho de um unico
  //elemento, pra descobrir seu temanho e continuar lendo o que falta...
  if esize=-1 then
    writeln('FIX-ME: Why we known nothing about tag "',tagrec.Path,'"? Maybe tag name mistake?',{$i %FILE%},':',{$i %LINE%});

  repeat
    repeat
      req:=TCIPReadTagFragReq.Create;
      req.RequestPath:=tagrec.Path;
      req.ReqOffset:=lastOffset;

      if esize>0 then
        aux1:=tagrec.Size div esize
      else
        aux1:=1;

      req.ReqCount:=aux1;

      auxreply := SendConnectedDataItem(TCIPPDU(req));
      try
        if (auxreply is TEIPSendUnitDataPDU) and
           (TEIPSendUnitDataPDU(auxreply).FLength>0) and
           Assigned(TEIPSendUnitDataPDU(auxreply).SendUnitDataCmdData) and
           (TEIPSendUnitDataPDU(auxreply).SendUnitDataCmdData.EncapsuledCPF.CPFItemCount=2) and
           (TEIPSendUnitDataPDU(auxreply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFConnectedDataItem) and
           (TCPFConnectedDataItem(TEIPSendUnitDataPDU(auxreply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIPReadTagFragRply)
        then begin
          reply:=TCIPReadTagFragRply(TCPFConnectedDataItem(TEIPSendUnitDataPDU(auxreply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket);
          RplySt:=reply.Status;
          RplyDT:=reply.DataType;
          RplyIsStruct:=reply.IsStructure;

          h:=Length(buff);
          SetLength(buff,h+Length(reply.Data));

          if (RplySt=0) and (esize=-1) then begin
            case reply.DataType of
              TAG_CIP_TYPE_BOOL,
              TAG_CIP_TYPE_SINT,
              TAG_CIP_TYPE_USINT: esize:=1;
            else esize:=Length(buff);
            end;

            writeln('FIX-ME: Assuming size of ',esize,' bytes for "',tagrec.Path,'" tag. ',{$i %FILE%},':',{$i %LINE%});
          end;

          if Length(reply.Data)>0 then begin
            Move(reply.Data[0],buff[h],Length(reply.Data));
            inc(lastOffset, Length(reply.Data));
          end;
        end else begin
          RplySt:=254;
        end;
      finally
        if Assigned(auxreply) then
          auxreply.Free;
        if Assigned(req) then
          req.Free;
      end;
    until RplySt<>6;
  until (Length(buff)>=tagrec.Size) or (RplySt=0) or (RplySt<>6);

  if RplySt=0 then begin
    SetLength(Values,Length(buff));
    IF (RplyIsStruct=false) and ((RplyDT and $FF)=TAG_CIP_TYPE_BOOL) then
      for i:=0 to High(buff) do
        Values[i]:=ifthen(buff[i]=0,0,1)
    else
      for i:=0 to High(buff) do
        Values[i]:=buff[i];


    if FTagList.Find(tagrec.Path.ToLower,idx) then
      FTagList.Data[idx]^.mm.SetValues(0,tagrec.Size,1,values,ioOk);

    exit(ioOk);
  end else begin
    Writeln('FIX-ME: got status ',inttohex(RplySt),' when reading tag "',tagrec.Path,'" ',{$i %FILE%},':',{$i %LINE%});
    case RplySt of
      $04: if FTagList.Find(tagrec.Path.ToLower,idx) then begin FTagList.Data[idx]^.mm.SetFault(0,tagrec.Size,1,ioIllegalRegAddress,true);    exit(ioIllegalRegAddress);    end;
      $05: if FTagList.Find(tagrec.Path.ToLower,idx) then begin FTagList.Data[idx]^.mm.SetFault(0,tagrec.Size,1,ioObjectNotExists,true);      exit(ioObjectNotExists);      end;
      $13: if FTagList.Find(tagrec.Path.ToLower,idx) then begin FTagList.Data[idx]^.mm.SetFault(0,tagrec.Size,1,ioIllegalRequest,true);       exit(ioIllegalRequest);       end;
      $26: if FTagList.Find(tagrec.Path.ToLower,idx) then begin FTagList.Data[idx]^.mm.SetFault(0,tagrec.Size,1,ioIllegalRegAddress,true);    exit(ioIllegalRegAddress);    end;
      $fe: if FTagList.Find(tagrec.Path.ToLower,idx) then begin FTagList.Data[idx]^.mm.SetFault(0,tagrec.Size,1,ioCommError,true);            exit(ioCommError);            end;
      $ff: if FTagList.Find(tagrec.Path.ToLower,idx) then begin FTagList.Data[idx]^.mm.SetFault(0,tagrec.Size,1,ioIllegalMemoryAddress,true); exit(ioIllegalMemoryAddress); end;
      else if FTagList.Find(tagrec.Path.ToLower,idx) then begin FTagList.Data[idx]^.mm.SetFault(0,tagrec.Size,1,ioUnknownError,true);         exit(ioUnknownError);         end;
    end;
  end;
end;

function TLGXDriver.DoWrite(const tagrec: TTagRec;
  const Values: TArrayOfDouble; Sync: Boolean): TProtocolIOResult;
var
  rreq: TCIPReadTagFragReq;
  esize, lastOffset, idx1, h, i, idx2, idx: Integer;
  auxrply: TPDU;
  rreply: TCIPReadTagFragRply;
  RplySt: Byte;
  buff: BYTES;
  etype: LongInt;
  req: TCIPWriteTagFragReq;
  reply: TCIPWriteTagFragRply;
  RplyStEx, ehand: Word;
  boolreq: TCIPWriteTagReq;
  boolreply: TCIPWriteTagRply;
  plcData: PLGXPLCResources = nil;
  fUnknownTag: Boolean;
begin
  if (InterlockedExchange(FTerminating,FTerminating)<>0) or (csDestroying in ComponentState) or TestDone then begin
    InterlockedExchange(FTerminating, 1);
    exit;
  end;

  if Length(Values)<1 then exit(ioIllegalValue);

  try
    FLGXGlobalMutex.Enter;
    if FPLCData.Find(PCommPort.getPortId,idx) then
      plcData:=FPLCData.data[idx];
  finally
    FLGXGlobalMutex.Leave;
  end;

  fUnknownTag := true;
  if Assigned(plcData) then begin
    plcData^.Mutex.Enter;
    try
      if plcData^.TagList.Find(tagrec.Path.ToLower,idx1) and (((plcData^.TagList.Data[idx1].aType and TYPE_IS_STRUCT)<>TYPE_IS_STRUCT) or plcData^.UDTList.Find(plcData^.TagList.Data[idx1].aType and $FFF,idx2)) then begin
        esize:=plcData^.TagList.Data[idx1].elem_size;
        etype:=plcData^.TagList.Data[idx1].aType;
        if (etype and TYPE_IS_STRUCT)=TYPE_IS_STRUCT then
          ehand:=plcData^.UDTList.Data[idx2]^.checksum
        else
          ehand:=etype;
        fUnknownTag:=false;
      end;
    finally
      plcData^.Mutex.Leave;
    end;
  end;
  if fUnknownTag then begin
    writeln('FIX-ME: Why we known nothing about tag "',tagrec.Path,'"? Maybe tag name mistake?',{$i %FILE%},':',{$i %LINE%});
    //diferente da leitura, precisamos saber o datatype da variavel para poder
    //escrever nela. Então enviamos um pacote de leitura simples com o numero
    //de elementos zerados para descobrir seu datatype.
    rreq:=TCIPReadTagFragReq.Create;
    rreq.RequestPath:=tagrec.Path;
    rreq.ReqCount:=0;
    rreq.ReqOffset:=0;

    auxrply := SendConnectedDataItem(TCIPPDU(rreq));
    try
      if (auxrply is TEIPSendUnitDataPDU) and
         (TEIPSendUnitDataPDU(auxrply).FLength>0) and
         Assigned(TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData) and
         (TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItemCount=2) and
         (TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFConnectedDataItem) and
         (TCPFConnectedDataItem(TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIPReadTagFragRply)
      then begin
        rreply:=TCIPReadTagFragRply(TCPFConnectedDataItem(TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket);
        RplySt:=rreply.Status;

        if RplySt in [0,6] then begin
          ehand:=rreply.DataType;
          esize:=-1;
        end else
          exit(ioIllegalRegAddress);
      end else
        exit(ioIllegalRequest);

    finally
      if Assigned(rreq) then
        rreq.Free;
      if Assigned(auxrply) then
        auxrply.Free;
    end;
  end;

  // AQUI COMECA A ESCRITA! TODA SAPORA AI PRA CIMA, FOI PRA PEGAR O
  // DATATYPE E O TAMANHO DE UM POSSIVEL TAG DESCONHECIDO...
  if (etype and $FF)=TAG_CIP_TYPE_BOOL then begin
    boolreq:=TCIPWriteTagReq.Create;
    boolreq.RequestPath:=tagrec.Path;
    boolreq.ReqCount:=1;
    boolreq.DType:=etype and $FF;
    boolreq.DCRC:=0;
    SetLength(buff,4);
    PLongWord(@buff[0])^:=IfThen(Values[0]=0,0,255);
    boolreq.WriteReqData:=buff;

    auxrply := SendConnectedDataItem(TCIPPDU(boolreq));
    try
      if (auxrply is TEIPSendUnitDataPDU) and
         (TEIPSendUnitDataPDU(auxrply).FLength>0) and
         Assigned(TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData) and
         (TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItemCount=2) and
         (TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFConnectedDataItem) and
         (TCPFConnectedDataItem(TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIPWriteTagRply)
      then begin
        boolreply:=TCIPWriteTagRply(TCPFConnectedDataItem(TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket);
        RplySt:=boolreply.Status;
        if Length(boolreply.ExtendedStatus)>0 then
          RplyStEx:=boolreply.ExtendedStatus[0];

      end else begin
        RplySt:=254;
      end;
    finally
      if Assigned(auxrply) then
        auxrply.Free;
      if Assigned(boolreq) then
        boolreq.Free;
    end;


  end else begin
    lastOffset:=tagrec.OffSet;
    repeat
      if esize<1 then
        esize := 1;

      req:=TCIPWriteTagFragReq.Create;
      req.RequestPath:=tagrec.Path;
      req.ReqOffset:=lastOffset;
      req.ReqCount :=(tagrec.OffSet+tagrec.Size) div esize + ifthen(((tagrec.OffSet+tagrec.Size) mod esize)<>0,1);
      req.DType:=etype;
      if (tagrec.Size div esize)=1 then
        req.DType := req.DType and $00ff;

      req.DCRC:=ehand;

      SetLength(buff,min(Length(values), maxPDUSize-(10+IfThen((ehand AND $FF)<=$A3,2,0))));
      for i:=0 to High(buff) do begin
        IF ((lastOffset-tagrec.OffSet)+i)>High(Values) then
          writeln('FIX-ME: wrong offset calculation. ',{$i %FILE%},':',{$i %LINE%});
        {$PUSH}
        {$R-}
        buff[i]:=Trunc(Values[(lastOffset-tagrec.OffSet)+i]);
        {$POP}
      end;

      req.WriteReqData:=buff;

      inc(lastOffset,Length(buff));

      auxrply := SendConnectedDataItem(TCIPPDU(req));
      try
        if (auxrply is TEIPSendUnitDataPDU) and
           (TEIPSendUnitDataPDU(auxrply).FLength>0) and
           Assigned(TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData) and
           (TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItemCount=2) and
           (TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1] is TCPFConnectedDataItem) and
           (TCPFConnectedDataItem(TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket is TCIPWriteTagFragRply)
        then begin
          reply:=TCIPWriteTagFragRply(TCPFConnectedDataItem(TEIPSendUnitDataPDU(auxrply).SendUnitDataCmdData.EncapsuledCPF.CPFItem[1]).EncapsuledPacket);
          RplySt:=reply.Status;
          if Length(reply.ExtendedStatus)>0 then
            RplyStEx:=reply.ExtendedStatus[0];

        end else begin
          RplySt:=254;
        end;
      finally
        if Assigned(auxrply) then
          auxrply.Free;
        if Assigned(req) then
          req.Free;
      end;
    until (lastOffset>=Length(buff)) or (RplySt<>0);
  end;

  if RplySt<>0 then
    Writeln('FIX-ME: got status ',inttohex(RplySt),' when writing tag "',tagrec.Path,'" ',{$i %FILE%},':',{$i %LINE%});;

  case RplySt of
    $00: begin
      if FTagList.Find(tagrec.Path.ToLower,idx) then
        FTagList.Data[idx]^.mm.SetValues(tagrec.OffSet,tagrec.Size,1,values,ioOk);
      Exit(ioOk);
    end;
    $04: Exit(ioIllegalRegAddress);
    $05: Exit(ioObjectNotExists);
    $13: Exit(ioIllegalRequest);
    $26: Exit(ioIllegalRegAddress);
    $fe: Exit(ioCommError);
    $ff: Exit(ioIllegalMemoryAddress);
    else exit(ioUnknownError);
  end;
end;

function LGXTypeIsStruct(aCIPType: Word): Boolean;
begin
  Result:=(aCIPType and TYPE_IS_STRUCT)=TYPE_IS_STRUCT;
end;

function LGXTypeIsSystem(aCIPType: Word): Boolean;
begin
  Result:=(aCIPType and TYPE_IS_SYSTEM)=TYPE_IS_SYSTEM;
end;

function LGXTypeDimensions(aCIPType: Word): Byte;
begin
  Result:=(aCIPType and TAG_DIM_MASK) shr 13;
end;

function LGXTypeName(aCIPType: Word): String;
begin
  if LGXTypeIsStruct(aCIPType) then
    Exit('STRUCT/UDT');

  case aCIPType and $FF of
    TAG_CIP_TYPE_BOOL  : Result:='BOOL';
    TAG_CIP_TYPE_SINT  : Result:='SINT';
    TAG_CIP_TYPE_INT   : Result:='INT';
    TAG_CIP_TYPE_DINT  : Result:='DINT';
    TAG_CIP_TYPE_LINT  : Result:='LINT';
    TAG_CIP_TYPE_USINT : Result:='USINT';
    TAG_CIP_TYPE_UINT  : Result:='UINT';
    TAG_CIP_TYPE_UDINT : Result:='UDINT';
    TAG_CIP_TYPE_ULINT : Result:='ULINT';
    TAG_CIP_TYPE_REAL  : Result:='REAL';
    TAG_CIP_TYPE_LREAL : Result:='LREAL';
    TAG_CIP_TYPE_STRING: Result:='STRING';
    TAG_CIP_TYPE_DWORD : Result:='DWORD';
    else                 Result:='0x'+IntToHex(aCIPType and $FF,4);
  end;
end;

function LGXTypeToTagType(aCIPType: Word; out aTagType: TTagType): Boolean;
begin
  aTagType:=pttDefault;

  if LGXTypeIsStruct(aCIPType) then
    Exit(false);

  Result:=true;
  case aCIPType and $FF of
    TAG_CIP_TYPE_BOOL,
    TAG_CIP_TYPE_USINT : aTagType:=pttByte;
    TAG_CIP_TYPE_SINT  : aTagType:=pttShortInt;
    TAG_CIP_TYPE_INT   : aTagType:=pttSmallInt;
    TAG_CIP_TYPE_UINT  : aTagType:=pttWord;
    TAG_CIP_TYPE_DINT  : aTagType:=pttLongInt;
    TAG_CIP_TYPE_UDINT,
    TAG_CIP_TYPE_DWORD : aTagType:=pttDWord;
    TAG_CIP_TYPE_LINT  : aTagType:=pttInt64;
    TAG_CIP_TYPE_ULINT : aTagType:=pttQWord;
    TAG_CIP_TYPE_REAL  : aTagType:=pttFloat;
    TAG_CIP_TYPE_LREAL : aTagType:=pttDouble;
    else                 Result:=false;
  end;
end;

function LGXTypeSizeInBytes(aCIPType: Word): Integer;
begin
  case aCIPType and $FF of
    TAG_CIP_TYPE_BOOL,
    TAG_CIP_TYPE_SINT,
    TAG_CIP_TYPE_USINT : Result:=1;
    TAG_CIP_TYPE_INT,
    TAG_CIP_TYPE_UINT  : Result:=2;
    TAG_CIP_TYPE_DINT,
    TAG_CIP_TYPE_UDINT,
    TAG_CIP_TYPE_DWORD,
    TAG_CIP_TYPE_REAL  : Result:=4;
    TAG_CIP_TYPE_LINT,
    TAG_CIP_TYPE_ULINT,
    TAG_CIP_TYPE_LREAL : Result:=8;
    else                 Result:=1;
  end;
end;

procedure SetTagBuilderToolForRockwellLogixProtocol(TagBuilderTool: TOpenTagEditor);
begin
  if Assigned(LGXTagBuilderEditor) then
    raise Exception.Create('A Tag Builder editor for Rockwell Compact/ControlLogix protocol was already assigned.')
  else
    LGXTagBuilderEditor:=TagBuilderTool;
end;

initialization

  FLGXGlobalMutex:=TCriticalSection.Create;
  FPLCData:=TPLCDataMap.Create;
  FPLCData.Sorted:=true;

finalization

  FLGXGlobalMutex.Enter;
  try
    { TODO : liberar o conteudo do mapa - os recursos de cada CLP (UDTs e
      listas de tags) continuam por liberar; aqui se libera o mapa em si. }
    FreeAndNil(FPLCData);
  finally
    FLGXGlobalMutex.Leave;
  end;
  FreeAndNil(FLGXGlobalMutex);

  { TODO : liberar toda a memoria alocada previamente... }
  //for c:=FLGXUDTList.Count-1 downto 0 do begin
  //  for i:=FLGXUDTList.Data[c]^.fields.Count-1 downto 0 do begin
  //    Dispose(FLGXUDTList.Data[c]^.fields.Items[i]);
  //  end;
  //  Dispose(FLGXUDTList.Data[c]);
  //end;
  //
  //FLGXTagList.Free;
end.
