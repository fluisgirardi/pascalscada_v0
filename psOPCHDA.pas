
{*******************************************************}
{                                                       }
{       OPC Historical Data Access 1.2                  }
{                                                       }
{       Delphi conversion generated and supplied by     }
{       Mike Dillamore                                  }
{       OPC Programmers' Connection                     }
{       http://www.opcconnect.com/                      }
{       mailto:opc@opcconnect.com                       }
{                                                       }
{       Based on OPC HDA 1.0 conversion by              }
{       Illya Kysil                                     }
{       mailto:ikysil@ua.fm                             }
{                                                       }
{*******************************************************}

unit psOPCHDA;

{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

// ************************************************************************ //
// Type Lib: opchda_ps.dll
// IID\LCID: {1F1217BA-DEE0-11D2-A5E5-000086339399}\0
// ************************************************************************ //

interface

uses
  Windows, ActiveX, psOPCtypes;

// *********************************************************************//
// GUIDS declared in the TypeLibrary                                    //
// *********************************************************************//
const
  LIBID_OPCHDA: TGUID = '{1F1217BA-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_Server: TGUID = '{1F1217B0-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_Browser: TGUID = '{1F1217B1-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_SyncRead: TGUID = '{1F1217B2-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_SyncUpdate: TGUID = '{1F1217B3-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_SyncAnnotations: TGUID = '{1F1217B4-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_AsyncRead: TGUID = '{1F1217B5-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_AsyncUpdate: TGUID = '{1F1217B6-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_AsyncAnnotations: TGUID =
                                      '{1F1217B7-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_Playback: TGUID = '{1F1217B8-DEE0-11D2-A5E5-000086339399}';
  IID_IOPCHDA_DataCallback: TGUID = '{1F1217B9-DEE0-11D2-A5E5-000086339399}';

  CATID_OPCHDAServer10: TGUID = '{7DE5B060-E089-11d2-A5E6-000086339399}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                  //
// *********************************************************************//
type
  OPCHDA_SERVERSTATUS = TOleEnum;
const
  OPCHDA_UP            = 1;
  OPCHDA_DOWN          = 2;
  OPCHDA_INDETERMINATE = 3;

type
  OPCHDA_BROWSEDIRECTION = TOleEnum;
const
  OPCHDA_BROWSE_UP     = 1;
  OPCHDA_BROWSE_DOWN   = 2;
  OPCHDA_BROWSE_DIRECT = 3;

type
  OPCHDA_BROWSETYPE = TOleEnum;
const
  OPCHDA_BRANCH = 1;
  OPCHDA_LEAF   = 2;
  OPCHDA_FLAT   = 3;
  OPCHDA_ITEMS  = 4;

type
  OPCHDA_ANNOTATIONCAPABILITIES = TOleEnum;
const
  OPCHDA_READANNOTATIONCAP   = 1;
  OPCHDA_INSERTANNOTATIONCAP = 2;

type
  OPCHDA_UPDATECAPABILITIES = TOleEnum;
const
  OPCHDA_INSERTCAP        = $01;
  OPCHDA_REPLACECAP       = $02;
  OPCHDA_INSERTREPLACECAP = $04;
  OPCHDA_DELETERAWCAP     = $08;
  OPCHDA_DELETEATTIMECAP  = $10;

type
  OPCHDA_OPERATORCODES = TOleEnum;
const
  OPCHDA_EQUAL        = 1;
  OPCHDA_LESS         = 2;
  OPCHDA_LESSEQUAL    = 3;
  OPCHDA_GREATER      = 4;
  OPCHDA_GREATEREQUAL = 5;
  OPCHDA_NOTEQUAL     = 6;
type
  OPCHDA_OPERATORCODESARRAY = array[0..65535] of OPCHDA_OPERATORCODES;
  POPCHDA_OPERATORCODESARRAY = ^OPCHDA_OPERATORCODESARRAY;

type
  OPCHDA_EDITTYPE = TOleEnum;
const
  OPCHDA_INSERT        = 1;
  OPCHDA_REPLACE       = 2;
  OPCHDA_INSERTREPLACE = 3;
  OPCHDA_DELETE        = 4;
type
  OPCHDA_EDITTYPEARRAY = array[0..65535] of OPCHDA_EDITTYPE;
  POPCHDA_EDITTYPEARRAY = ^OPCHDA_EDITTYPEARRAY;

type
  OPCHDA_AGGREGATE = TOleEnum;
const
  OPCHDA_NOAGGREGATE       = 0;
  OPCHDA_INTERPOLATIVE     = 1;
  OPCHDA_TOTAL             = 2;
  OPCHDA_AVERAGE           = 3;
  OPCHDA_TIMEAVERAGE       = 4;
  OPCHDA_COUNT             = 5;
  OPCHDA_STDEV             = 6;
  OPCHDA_MINIMUMACTUALTIME = 7;
  OPCHDA_MINIMUM           = 8;
  OPCHDA_MAXIMUMACTUALTIME = 9;
  OPCHDA_MAXIMUM           = 10;
  OPCHDA_START             = 11;
  OPCHDA_END               = 12;
  OPCHDA_DELTA             = 13;
  OPCHDA_REGSLOPE          = 14;
  OPCHDA_REGCONST          = 15;
  OPCHDA_REGDEV            = 16;
  OPCHDA_VARIANCE          = 17;
  OPCHDA_RANGE             = 18;
  OPCHDA_DURATIONGOOD      = 19;
  OPCHDA_DURATIONBAD       = 20;
  OPCHDA_PERCENTGOOD       = 21;
  OPCHDA_PERCENTBAD        = 22;
  OPCHDA_WORSTQUALITY      = 23;
  OPCHDA_ANNOTATIONS       = 24;
type
  OPCHDA_AGGREGATEARRAY = array[0..65535] of OPCHDA_AGGREGATE;
  POPCHDA_AGGREGATEARRAY = ^OPCHDA_AGGREGATEARRAY;

const

// AttributeID

  OPCHDA_DATA_TYPE          = $01;
  OPCHDA_DESCRIPTION        = $02;
  OPCHDA_ENG_UNITS          = $03;
  OPCHDA_STEPPED            = $04;
  OPCHDA_ARCHIVING          = $05;
  OPCHDA_DERIVE_EQUATION    = $06;
  OPCHDA_NODE_NAME          = $07;
  OPCHDA_PROCESS_NAME       = $08;
  OPCHDA_SOURCE_NAME        = $09;
  OPCHDA_SOURCE_TYPE        = $0A;
  OPCHDA_NORMAL_MAXIMUM     = $0B;
  OPCHDA_NORMAL_MINIMUM     = $0C;
  OPCHDA_ITEMID             = $0D;

  OPCHDA_MAX_TIME_INT       = $0E;
  OPCHDA_MIN_TIME_INT       = $0F;
  OPCHDA_EXCEPTION_DEV      = $10;
  OPCHDA_EXCEPTION_DEV_TYPE = $11;
  OPCHDA_HIGH_ENTRY_LIMIT   = $12;
  OPCHDA_LOW_ENTRY_LIMIT    = $13;

// OPCHDA_QUALITY -- these are the high-order 16 bits, OPC DA Quality
// occupies low-order 16 bits

  OPCHDA_EXTRADATA          = $00010000;
  OPCHDA_INTERPOLATED       = $00020000;
  OPCHDA_RAW                = $00040000;
  OPCHDA_CALCULATED         = $00080000;
  OPCHDA_NOBOUND            = $00100000;
  OPCHDA_NODATA             = $00200000;
  OPCHDA_DATALOST           = $00400000;
  OPCHDA_CONVERSION         = $00800000;
  OPCHDA_PARTIAL            = $01000000;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                  //
// *********************************************************************//
  IOPCHDA_Browser = interface;
  IOPCHDA_Server = interface;
  IOPCHDA_SyncRead = interface;
  IOPCHDA_SyncUpdate = interface;
  IOPCHDA_SyncAnnotations = interface;
  IOPCHDA_AsyncRead = interface;
  IOPCHDA_AsyncUpdate = interface;
  IOPCHDA_AsyncAnnotations = interface;
  IOPCHDA_Playback = interface;
  IOPCHDA_DataCallback = interface;

// *********************************************************************//
// Declaration of structures, unions and aliases.                       //
// *********************************************************************//
  OPCHDA_ANNOTATION = record
    hClient:                            OPCHANDLE;
    dwNumValues:                        DWORD;
    ftTimeStamps:                       PFileTimeArray;
    szAnnotation:                       POleStrList;
    ftAnnotationTime:                   PFileTimeArray;
    szUser:                             POleStrList;
  end;
  POPCHDA_ANNOTATION = ^OPCHDA_ANNOTATION;
  OPCHDA_ANNOTATIONARRAY = array[0..65535] of OPCHDA_ANNOTATION;
  POPCHDA_ANNOTATIONARRAY = ^OPCHDA_ANNOTATIONARRAY;

  OPCHDA_MODIFIEDITEM = record
    hClient:                            OPCHANDLE;
    dwCount:                            DWORD;
    pftTimeStamps:                      PFileTimeArray;
    pdwQualities:                       PDWORDARRAY;
    pvDataValues:                       POleVariantArray;
    pftModificationTime:                PFileTimeArray;
    pEditType:                          POPCHDA_EDITTYPEARRAY;
    szUser:                             POleStrList;
  end;
  POPCHDA_MODIFIEDITEM = ^OPCHDA_MODIFIEDITEM;
  OPCHDA_MODIFIEDITEMARRAY = array[0..65535] of OPCHDA_MODIFIEDITEM;
  POPCHDA_MODIFIEDITEMARRAY = ^OPCHDA_MODIFIEDITEMARRAY;

  OPCHDA_ATTRIBUTE = record
    hClient:                            OPCHANDLE;
    dwNumValues:                        DWORD;
    dwAttributeID:                      DWORD;
    ftTimeStamps:                       PFileTimeArray;
    vAttributeValues:                   POleVariantArray;
  end;
  POPCHDA_ATTRIBUTE = ^OPCHDA_ATTRIBUTE;
  OPCHDA_ATTRIBUTEARRAY = array[0..65535] of OPCHDA_ATTRIBUTE;
  POPCHDA_ATTRIBUTEARRAY = ^OPCHDA_ATTRIBUTEARRAY;

  OPCHDA_TIME = record
    bString:                            BOOL;
    szTime:                             POleStr;
    ftTime:                             TFileTime;
  end;
  POPCHDA_TIME = ^OPCHDA_TIME;

  OPCHDA_ITEM = record
    hClient:                            OPCHANDLE;
    haAggregate:                        DWORD;
    dwCount:                            DWORD;
    pftTimeStamps:                      PFileTimeArray;
    pdwQualities:                       PDWORDARRAY;
    pvDataValues:                       POleVariantArray;
  end;
  POPCHDA_ITEM = ^OPCHDA_ITEM;
  OPCHDA_ITEMARRAY = array[0..65535] of OPCHDA_ITEM;
  POPCHDA_ITEMARRAY = ^OPCHDA_ITEMARRAY;
  OPCHDA_ITEMPTRARRAY = array[0..65535] of POPCHDA_ITEM;
  POPCHDA_ITEMPTRARRAY = ^OPCHDA_ITEMPTRARRAY;

// *********************************************************************//
// Interface: IOPCHDA_Browser
// GUID:      {1F1217B1-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_Browser = interface(IUnknown)
    ['{1F1217B1-DEE0-11D2-A5E5-000086339399}']
    function GetEnum(
            dwBrowseType:               OPCHDA_BROWSETYPE;
      out   ppIEnumString:              IEnumString): HResult; stdcall;
    function ChangeBrowsePosition(
            dwBrowseDirection:          OPCHDA_BROWSEDIRECTION;
            szString:                   POleStr): HResult; stdcall;
    function GetItemID(
            szNode:                     POleStr;
      out   pszItemID:                  POleStr): HResult; stdcall;
    function GetBranchPosition(
      out   pszBranchPos:               POleStr): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCHDA_Server
// GUID:      {1F1217B0-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_Server = interface(IUnknown)
    ['{1F1217B0-DEE0-11D2-A5E5-000086339399}']
    function GetItemAttributes(
      out   pdwCount:                   DWORD;
      out   ppdwAttrID:                 PDWORDARRAY;
      out   ppszAttrName:               POleStrList;
      out   ppszAttrDesc:               POleStrList;
      out   ppvtAttrDataType:           PVarTypeList): HResult; stdcall;
    function GetAggregates(
      out   pdwCount:                   DWORD;
      out   ppdwAggrID:                 PDWORDARRAY;
      out   ppszAggrName:               POleStrList;
      out   ppszAggrDesc:               POleStrList): HResult; stdcall;
    function GetHistorianStatus(
      out   pwStatus:                   OPCHDA_SERVERSTATUS;
      out   pftCurrentTime:             PFileTimeArray;
      out   pftStartTime:               PFileTimeArray;
      out   pwMajorVersion:             Word;
      out   pwMinorVersion:             Word;
      out   pwBuildNumber:              Word;
      out   pdwMaxReturnValues:         DWORD;
      out   ppszStatusString:           POleStr;
      out   ppszVendorInfo:             POleStr): HResult; stdcall;
    function GetItemHandles(
            dwCount:                    DWORD;
            pszItemID:                  POleStrList;
            phClient:                   POPCHANDLEARRAY;
      out   pphServer:                  POPCHANDLEARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReleaseItemHandles(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ValidateItemIDs(
            dwCount:                    DWORD;
            pszItemID:                  POleStrList;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function CreateBrowse(
            dwCount:                    DWORD;
            pdwAttrID:                  PDWORDARRAY;
            pOperator:                  POPCHDA_OPERATORCODESARRAY;
            vFilter:                    POleVariantArray;
      out   pphBrowser:                 IOPCHDA_Browser;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCHDA_SyncRead
// GUID:      {1F1217B2-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_SyncRead = interface(IUnknown)
    ['{1F1217B2-DEE0-11D2-A5E5-000086339399}']
    function ReadRaw(
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            dwNumValues:                DWORD;
            bBounds:                    BOOL;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppItemValues:               POPCHDA_ITEMARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReadProcessed(
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            ftResampleInterval:         TFileTime;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            haAggregate:                PDWORDARRAY;
      out   ppItemValues:               POPCHDA_ITEMARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReadAtTime(
            dwNumTimeStamps:            DWORD;
            ftTimeStamps:               PFileTimeArray;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppItemValues:               POPCHDA_ITEMARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReadModified(
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            dwNumValues:                DWORD;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppItemValues:               POPCHDA_MODIFIEDITEMARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReadAttribute(
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            hServer:                    OPCHANDLE;
            dwNumAttributes:            DWORD;
            pdwAttributeIDs:            PDWORDARRAY;
      out   ppAttributeValues:          POPCHDA_ATTRIBUTEARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCHDA_SyncUpdate
// GUID:      {1F1217B3-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_SyncUpdate = interface(IUnknown)
    ['{1F1217B3-DEE0-11D2-A5E5-000086339399}']
    function QueryCapabilities(
      out   pCapabilities:              OPCHDA_UPDATECAPABILITIES):
            HResult; stdcall;
    function Insert(
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
            vDataValues:                POleVariantArray;
            pdwQualities:               PDWORDARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Replace(
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
            vDataValues:                POleVariantArray;
            pdwQualities:               PDWORDARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function InsertReplace(
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
            vDataValues:                POleVariantArray;
            pdwQualities:               PDWORDARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function DeleteRaw(
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function DeleteAtTime(
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCHDA_SyncAnnotations
// GUID:      {1F1217B4-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_SyncAnnotations = interface(IUnknown)
    ['{1F1217B4-DEE0-11D2-A5E5-000086339399}']
    function QueryCapabilities(
      out   pCapabilities:              OPCHDA_ANNOTATIONCAPABILITIES):
            HResult; stdcall;
    function Read(
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppAnnotationValues:         POPCHDA_ANNOTATIONARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Insert(
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
            pAnnotationValues:          POPCHDA_ANNOTATIONARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCHDA_AsyncRead
// GUID:      {1F1217B5-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_AsyncRead = interface(IUnknown)
    ['{1F1217B5-DEE0-11D2-A5E5-000086339399}']
    function ReadRaw(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            dwNumValues:                DWORD;
            bBounds:                    BOOL;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function AdviseRaw(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
            ftUpdateInterval:           TFileTime;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReadProcessed(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            ftResampleInterval:         TFileTime;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            haAggregate:                PDWORDARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function AdviseProcessed(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
            ftResampleInterval:         TFileTime;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            haAggregate:                PDWORDARRAY;
            dwNumIntervals:             DWORD;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReadAtTime(
            dwTransactionID:            DWORD;
            dwNumTimeStamps:            DWORD;
            ftTimeStamps:               PFileTimeArray;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReadModified(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            dwNumValues:                DWORD;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReadAttribute(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            hServer:                    OPCHANDLE;
            dwNumAttributes:            DWORD;
            dwAttributeIDs:             PDWORDARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Cancel(
            dwCancelID:                 DWORD): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCHDA_AsyncUpdate
// GUID:      {1F1217B6-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_AsyncUpdate = interface(IUnknown)
    ['{1F1217B6-DEE0-11D2-A5E5-000086339399}']
    function QueryCapabilities(
      out   pCapabilities:              OPCHDA_UPDATECAPABILITIES):
            HResult; stdcall;
    function Insert(
            dwTransactionID:            DWORD;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
            vDataValues:                POleVariantArray;
            pdwQualities:               PDWORDARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Replace(
            dwTransactionID:            DWORD;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
            vDataValues:                POleVariantArray;
            pdwQualities:               PDWORDARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function InsertReplace(
            dwTransactionID:            DWORD;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
            vDataValues:                POleVariantArray;
            pdwQualities:               PDWORDARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function DeleteRaw(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function DeleteAtTime(
            dwTransactionID:            DWORD;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Cancel(
            dwCancelID:                 DWORD): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCHDA_AsyncAnnotations
// GUID:      {1F1217B7-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_AsyncAnnotations = interface(IUnknown)
    ['{1F1217B7-DEE0-11D2-A5E5-000086339399}']
    function QueryCapabilities(
      out   pCapabilities:              OPCHDA_ANNOTATIONCAPABILITIES):
            HResult; stdcall;
    function Read(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Insert(
            dwTransactionID:            DWORD;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            ftTimeStamps:               PFileTimeArray;
            pAnnotationValues:          POPCHDA_ANNOTATIONARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Cancel(
            dwCancelID:                 DWORD): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCHDA_Playback
// GUID:      {1F1217B8-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_Playback = interface(IUnknown)
    ['{1F1217B8-DEE0-11D2-A5E5-000086339399}']
    function ReadRawWithUpdate(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            dwNumValues:                DWORD;
            ftUpdateDuration:           TFileTime;
            ftUpdateInterval:           TFileTime;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ReadProcessedWithUpdate(
            dwTransactionID:            DWORD;
      var   htStartTime:                OPCHDA_TIME;
      var   htEndTime:                  OPCHDA_TIME;
            ftResampleInterval:         TFileTime;
            dwNumIntervals:             DWORD;
            ftUpdateInterval:           TFileTime;
            dwNumItems:                 DWORD;
            phServer:                   POPCHANDLEARRAY;
            haAggregate:                PDWORDARRAY;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Cancel(
            dwCancelID:                 DWORD): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCHDA_DataCallback
// GUID:      {1F1217B9-DEE0-11D2-A5E5-000086339399}
// *********************************************************************//
  IOPCHDA_DataCallback = interface(IUnknown)
    ['{1F1217B9-DEE0-11D2-A5E5-000086339399}']
    function OnDataChange(
            dwTransactionID:            DWORD;
            hrStatus:                   HResult;
            dwNumItems:                 DWORD;
            pItemValues:                POPCHDA_ITEMARRAY;
            phrErrors:                  PResultList): HResult; stdcall;
    function OnReadComplete(
            dwTransactionID:            DWORD;
            hrStatus:                   HResult;
            dwNumItems:                 DWORD;
            pItemValues:                POPCHDA_ITEMARRAY;
            phrErrors:                  PResultList): HResult; stdcall;
    function OnReadModifiedComplete(
            dwTransactionID:            DWORD;
            hrStatus:                   HResult;
            dwNumItems:                 DWORD;
            pItemValues:                POPCHDA_MODIFIEDITEMARRAY;
            phrErrors:                  PResultList): HResult; stdcall;
    function OnReadAttributeComplete(
            dwTransactionID:            DWORD;
            hrStatus:                   HResult;
            hClient:                    OPCHANDLE;
            dwNumItems:                 DWORD;
            pAttributeValues:           POPCHDA_ATTRIBUTEARRAY;
            phrErrors:                  PResultList): HResult; stdcall;
    function OnReadAnnotations(
            dwTransactionID:            DWORD;
            hrStatus:                   HResult;
            dwNumItems:                 DWORD;
            pAnnotationValues:          POPCHDA_ANNOTATIONARRAY;
            phrErrors:                  PResultList): HResult; stdcall;
    function OnInsertAnnotations(
            dwTransactionID:            DWORD;
            hrStatus:                   HResult;
            dwCount:                    DWORD;
            phClients:                  POPCHANDLEARRAY;
            phrErrors:                  PResultList): HResult; stdcall;
    function OnPlayback(
            dwTransactionID:            DWORD;
            hrStatus:                   HResult;
            dwNumItems:                 DWORD;
            ppItemValues:               POPCHDA_ITEMPTRARRAY;
            phrErrors:                  PResultList): HResult; stdcall;
    function OnUpdateComplete(
            dwTransactionID:            DWORD;
            hrStatus:                   HResult;
            dwCount:                    DWORD;
            phClients:                  POPCHANDLEARRAY;
            phrErrors:                  PResultList): HResult; stdcall;
    function OnCancelComplete(
            dwCancelID:                 DWORD): HResult; stdcall;
  end;

implementation

end.
