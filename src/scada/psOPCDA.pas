
{*******************************************************}
{                                                       }
{       OPC Data Access 3.0 (and earlier versions)      }
{                                                       }
{       Delphi conversion generated and supplied by     }
{       Mike Dillamore                                  }
{       OPC Programmers' Connection                     }
{       http://www.opcconnect.com/                      }
{       mailto:opc@opcconnect.com                       }
{                                                       }
{       Improvements contributed by                     }
{       Martin Lafferty                                 }
{       Production Robots Engineering Ltd               }
{       http://www.prel.co.uk/                          }
{       mailto:prOpcKit@prel.co.uk                      }
{                                                       }
{*******************************************************}

unit psOPCDA;

{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

// ************************************************************************ //
// Type Lib: OPCProxy.dll
// IID\LCID: {3B540B51-0378-4551-ADCC-EA9B104302BF}\0 - Data Access 3.0
// IID\LCID: {B28EEDB2-AC6F-11D1-84D5-00608CB8A7E9}\0 - Data Access 2.0
// ************************************************************************ //

interface

uses
  Windows, ActiveX, SysUtils, psOPCtypes;

// *********************************************************************//
// GUIDS declared in the TypeLibrary                                    //
// *********************************************************************//
const
  LIBID_OPCDA: TGUID = '{3B540B51-0378-4551-ADCC-EA9B104302BF}';
  IID_IOPCServer: TIID = '{39C13A4D-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCServerPublicGroups: TIID = '{39C13A4E-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCBrowseServerAddressSpace: TIID =
                                      '{39C13A4F-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCGroupStateMgt: TIID = '{39C13A50-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCPublicGroupStateMgt: TIID = '{39C13A51-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCSyncIO: TIID = '{39C13A52-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCAsyncIO: TIID = '{39C13A53-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCItemMgt: TIID = '{39C13A54-011E-11D0-9675-0020AFD8ADB3}';
  IID_IEnumOPCItemAttributes: TIID = '{39C13A55-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCDataCallback: TIID = '{39C13A70-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCAsyncIO2: TIID = '{39C13A71-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCItemProperties: TIID = '{39C13A72-011E-11D0-9675-0020AFD8ADB3}';
  IID_IOPCItemDeadbandMgt: TIID = '{5946DA93-8B39-4ec8-AB3D-AA73DF5BC86F}';
  IID_IOPCItemSamplingMgt: TIID = '{3E22D313-F08B-41a5-86C8-95E95CB49FFC}';
  IID_IOPCBrowse: TIID = '{39227004-A18F-4b57-8B0A-5235670F4468}';
  IID_IOPCItemIO: TIID = '{85C0B427-2893-4cbc-BD78-E5FC5146F08F}';
  IID_IOPCSyncIO2: TIID = '{730F5F0F-55B1-4c81-9E18-FF8A0904E1FA}';
  IID_IOPCAsyncIO3: TIID = '{0967B97B-36EF-423e-B6F8-6BFF1E40D39D}';
  IID_IOPCGroupStateMgt2: TIID = '{8E368666-D72E-4f78-87ED-647611C61C9F}';

  CATID_OPCDAServer10: TGUID = '{63D5F430-CFE4-11d1-B2C8-0060083BA1FB}';
  CATID_OPCDAServer20: TGUID = '{63D5F432-CFE4-11d1-B2C8-0060083BA1FB}';
  CATID_OPCDAServer30: TGUID = '{CC603642-66D7-48f1-B69A-B625E73652D7}';
  CATID_XMLDAServer10: TGUID = '{3098EDA4-A006-48b2-A27F-247453959408}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                  //
// *********************************************************************//
type
  OPCDATASOURCE = TOleEnum;
const
  OPC_DS_CACHE  = 1;
  OPC_DS_DEVICE = 2;

type
  OPCBROWSETYPE = TOleEnum;
const
  OPC_BRANCH = 1;
  OPC_LEAF   = 2;
  OPC_FLAT   = 3;

type
  OPCNAMESPACETYPE = TOleEnum;
const
  OPC_NS_HIERARCHIAL = 1;
  OPC_NS_FLAT        = 2;

type
  OPCBROWSEDIRECTION = TOleEnum;
const
  OPC_BROWSE_UP   = 1;
  OPC_BROWSE_DOWN = 2;
  OPC_BROWSE_TO   = 3;

type
  OPCEUTYPE = TOleEnum;
const
  OPC_NOENUM     = 0;
  OPC_ANALOG     = 1;
  OPC_ENUMERATED = 2;

type
  OPCSERVERSTATE = TOleEnum;
const
  OPC_STATUS_RUNNING    = 1;
  OPC_STATUS_FAILED     = 2;
  OPC_STATUS_NOCONFIG   = 3;
  OPC_STATUS_SUSPENDED  = 4;
  OPC_STATUS_TEST       = 5;
  OPC_STATUS_COMM_FAULT = 6;

type
  OPCENUMSCOPE = TOleEnum;
const
  OPC_ENUM_PRIVATE_CONNECTIONS = 1;
  OPC_ENUM_PUBLIC_CONNECTIONS  = 2;
  OPC_ENUM_ALL_CONNECTIONS     = 3;
  OPC_ENUM_PRIVATE             = 4;
  OPC_ENUM_PUBLIC              = 5;
  OPC_ENUM_ALL                 = 6;

type
  OPCBROWSEFILTER = TOleEnum;
const
  OPC_BROWSE_FILTER_ALL      = 1;
  OPC_BROWSE_FILTER_BRANCHES = 2;
  OPC_BROWSE_FILTER_ITEMS    = 3;

// *********************************************************************//
// Constants                                                            //
// *********************************************************************//
// Category description strings
const
   OPC_CATEGORY_DESCRIPTION_DA10    = 'OPC Data Access Servers Version 1.0';
   OPC_CATEGORY_DESCRIPTION_DA20    = 'OPC Data Access Servers Version 2.0';
   OPC_CATEGORY_DESCRIPTION_DA30    = 'OPC Data Access Servers Version 3.0';
   OPC_CATEGORY_DESCRIPTION_XMLDA10 = 'OPC XML Data Access Servers Version 1.0';

// Values for access rights mask
const
  OPC_READABLE  = $01;
  OPC_WRITEABLE = $02;

// Values for browse element flags
const
  OPC_BROWSE_HASCHILDREN = $01;
  OPC_BROWSE_ISITEM      = $02;

// *********************************************************************//
// OPC Quality flags                                                    //
// *********************************************************************//
// Masks for extracting quality subfields
// (note 'status' mask also includes 'Quality' bits)
const
  OPC_QUALITY_MASK           = $C0;
  OPC_STATUS_MASK            = $FC;
  OPC_LIMIT_MASK             = $03;

// Values for QUALITY_MASK bit field
const
  OPC_QUALITY_BAD            = $00;
  OPC_QUALITY_UNCERTAIN      = $40;
  OPC_QUALITY_GOOD           = $C0;

// STATUS_MASK Values for Quality = BAD
const
  OPC_QUALITY_CONFIG_ERROR              = $04;
  OPC_QUALITY_NOT_CONNECTED             = $08;
  OPC_QUALITY_DEVICE_FAILURE            = $0C;
  OPC_QUALITY_SENSOR_FAILURE            = $10;
  OPC_QUALITY_LAST_KNOWN                = $14;
  OPC_QUALITY_COMM_FAILURE              = $18;
  OPC_QUALITY_OUT_OF_SERVICE            = $1C;
  OPC_QUALITY_WAITING_FOR_INITIAL_DATA  = $20;

// STATUS_MASK Values for Quality = UNCERTAIN
const
  OPC_QUALITY_LAST_USABLE    = $44;
  OPC_QUALITY_SENSOR_CAL     = $50;
  OPC_QUALITY_EGU_EXCEEDED   = $54;
  OPC_QUALITY_SUB_NORMAL     = $58;

// STATUS_MASK Values for Quality = GOOD
const
  OPC_QUALITY_LOCAL_OVERRIDE = $D8;

// Values for Limit Bitfield
const
  OPC_LIMIT_OK    = $00;
  OPC_LIMIT_LOW   = $01;
  OPC_LIMIT_HIGH  = $02;
  OPC_LIMIT_CONST = $03;

// *********************************************************************//
// Data Access 2.0 Property IDs:                                        //
// *********************************************************************//
const
  OPC_PROP_CDT            = 1;
  OPC_PROP_VALUE          = 2;
  OPC_PROP_QUALITY        = 3;
  OPC_PROP_TIME           = 4;
  OPC_PROP_RIGHTS         = 5;
  OPC_PROP_SCANRATE       = 6;

  OPC_PROP_UNIT           = 100;
  OPC_PROP_DESC           = 101;
  OPC_PROP_HIEU           = 102;
  OPC_PROP_LOEU           = 103;
  OPC_PROP_HIRANGE        = 104;
  OPC_PROP_LORANGE        = 105;
  OPC_PROP_CLOSE          = 106;
  OPC_PROP_OPEN           = 107;
  OPC_PROP_TIMEZONE       = 108;

  OPC_PROP_DSP            = 200;
  OPC_PROP_FGC            = 201;
  OPC_PROP_BGC            = 202;
  OPC_PROP_BLINK          = 203;
  OPC_PROP_BMP            = 204;
  OPC_PROP_SND            = 205;
  OPC_PROP_HTML           = 206;
  OPC_PROP_AVI            = 207;

  OPC_PROP_ALMSTAT        = 300;
  OPC_PROP_ALMHELP        = 301;
  OPC_PROP_ALMAREAS       = 302;
  OPC_PROP_ALMPRIMARYAREA = 303;
  OPC_PROP_ALMCONDITION   = 304;
  OPC_PROP_ALMLIMIT       = 305;
  OPC_PROP_ALMDB          = 306;
  OPC_PROP_ALMHH          = 307;
  OPC_PROP_ALMH           = 308;
  OPC_PROP_ALML           = 309;
  OPC_PROP_ALMLL          = 310;
  OPC_PROP_ALMROC         = 311;
  OPC_PROP_ALMDEV         = 312;

// *********************************************************************//
// Data Access 3.0 Property IDs:                                        //
// *********************************************************************//
const
  OPC_PROPERTY_DATATYPE           = 1;
  OPC_PROPERTY_VALUE              = 2;
  OPC_PROPERTY_QUALITY            = 3;
  OPC_PROPERTY_TIMESTAMP          = 4;
  OPC_PROPERTY_ACCESS_RIGHTS      = 5;
  OPC_PROPERTY_SCAN_RATE          = 6;
  OPC_PROPERTY_EU_TYPE            = 7;
  OPC_PROPERTY_EU_INFO            = 8;
  OPC_PROPERTY_EU_UNITS           = 100;
  OPC_PROPERTY_DESCRIPTION        = 101;
  OPC_PROPERTY_HIGH_EU            = 102;
  OPC_PROPERTY_LOW_EU             = 103;
  OPC_PROPERTY_HIGH_IR            = 104;
  OPC_PROPERTY_LOW_IR             = 105;
  OPC_PROPERTY_CLOSE_LABEL        = 106;
  OPC_PROPERTY_OPEN_LABEL         = 107;
  OPC_PROPERTY_TIMEZONE           = 108;
  OPC_PROPERTY_CONDITION_STATUS   = 300;
  OPC_PROPERTY_ALARM_QUICK_HELP   = 301;
  OPC_PROPERTY_ALARM_AREA_LIST    = 302;
  OPC_PROPERTY_PRIMARY_ALARM_AREA = 303;
  OPC_PROPERTY_CONDITION_LOGIC    = 304;
  OPC_PROPERTY_LIMIT_EXCEEDED     = 305;
  OPC_PROPERTY_DEADBAND           = 306;
  OPC_PROPERTY_HIHI_LIMIT         = 307;
  OPC_PROPERTY_HI_LIMIT           = 308;
  OPC_PROPERTY_LO_LIMIT           = 309;
  OPC_PROPERTY_LOLO_LIMIT         = 310;
  OPC_PROPERTY_CHANGE_RATE_LIMIT  = 311;
  OPC_PROPERTY_DEVIATION_LIMIT    = 312;
  OPC_PROPERTY_SOUND_FILE         = 313;

// *********************************************************************//
// Data Access 3.0 Property Descriptions:                               //
// *********************************************************************//
const
  OPC_PROPERTY_DESC_DATATYPE           = 'Item Canonical Data Type';
  OPC_PROPERTY_DESC_VALUE              = 'Item Value';
  OPC_PROPERTY_DESC_QUALITY            = 'Item Quality';
  OPC_PROPERTY_DESC_TIMESTAMP          = 'Item Timestamp';
  OPC_PROPERTY_DESC_ACCESS_RIGHTS      = 'Item Access Rights';
  OPC_PROPERTY_DESC_SCAN_RATE          = 'Server Scan Rate';
  OPC_PROPERTY_DESC_EU_TYPE            = 'Item EU Type';
  OPC_PROPERTY_DESC_EU_INFO            = 'Item EU Info';
  OPC_PROPERTY_DESC_EU_UNITS           = 'EU Units';
  OPC_PROPERTY_DESC_DESCRIPTION        = 'Item Description';
  OPC_PROPERTY_DESC_HIGH_EU            = 'High EU';
  OPC_PROPERTY_DESC_LOW_EU             = 'Low EU';
  OPC_PROPERTY_DESC_HIGH_IR            = 'High Instrument Range';
  OPC_PROPERTY_DESC_LOW_IR             = 'Low Instrument Range';
  OPC_PROPERTY_DESC_CLOSE_LABEL        = 'Contact Close Label';
  OPC_PROPERTY_DESC_OPEN_LABEL         = 'Contact Open Label';
  OPC_PROPERTY_DESC_TIMEZONE           = 'Item Timezone';
  OPC_PROPERTY_DESC_CONDITION_STATUS   = 'Condition Status';
  OPC_PROPERTY_DESC_ALARM_QUICK_HELP   = 'Alarm Quick Help';
  OPC_PROPERTY_DESC_ALARM_AREA_LIST    = 'Alarm Area List';
  OPC_PROPERTY_DESC_PRIMARY_ALARM_AREA = 'Primary Alarm Area';
  OPC_PROPERTY_DESC_CONDITION_LOGIC    = 'Condition Logic';
  OPC_PROPERTY_DESC_LIMIT_EXCEEDED     = 'Limit Exceeded';
  OPC_PROPERTY_DESC_DEADBAND           = 'Deadband';
  OPC_PROPERTY_DESC_HIHI_LIMIT         = 'HiHi Limit';
  OPC_PROPERTY_DESC_HI_LIMIT           = 'Hi Limit';
  OPC_PROPERTY_DESC_LO_LIMIT           = 'Lo Limit';
  OPC_PROPERTY_DESC_LOLO_LIMIT         = 'LoLo Limit';
  OPC_PROPERTY_DESC_CHANGE_RATE_LIMIT  = 'Rate of Change Limit';
  OPC_PROPERTY_DESC_DEVIATION_LIMIT    = 'Deviation Limit';
  OPC_PROPERTY_DESC_SOUND_FILE         = 'Sound File';

type

// *********************************************************************//
// Forward declaration of interfaces defined in Type Library            //
// *********************************************************************//
  IOPCServer = interface;
  IOPCServerPublicGroups = interface;
  IOPCBrowseServerAddressSpace = interface;
  IOPCGroupStateMgt = interface;
  IOPCPublicGroupStateMgt = interface;
  IOPCSyncIO = interface;
  IOPCAsyncIO = interface;
  IOPCItemMgt = interface;
  IEnumOPCItemAttributes = interface;
  IOPCDataCallback = interface;
  IOPCAsyncIO2 = interface;
  IOPCItemProperties = interface;
  IOPCItemDeadbandMgt = interface;
  IOPCItemSamplingMgt = interface;
  IOPCBrowse = interface;
  IOPCItemIO = interface;
  IOPCSyncIO2 = interface;
  IOPCAsyncIO3 = interface;
  IOPCGroupStateMgt2 = interface;

// *********************************************************************//
// Declaration of structures, unions and aliases.                       //
// *********************************************************************//
  OPCGROUPHEADER = record
    dwSize:               DWORD;
    dwItemCount:          DWORD;
    hClientGroup:         OPCHANDLE;
    dwTransactionID:      DWORD;
    hrStatus:             HResult;
  end;
  POPCGROUPHEADER = ^OPCGROUPHEADER;

  OPCITEMHEADER1 = record
    hClient:              OPCHANDLE;
    dwValueOffset:        DWORD;
    wQuality:             Word;
    wReserved:            Word;
    ftTimeStampItem:      TFileTime;
  end;
  POPCITEMHEADER1 = ^OPCITEMHEADER1;
  OPCITEMHEADER1ARRAY = array[0..65535] of OPCITEMHEADER1;
  POPCITEMHEADER1ARRAY = ^OPCITEMHEADER1ARRAY;

  OPCITEMHEADER2 = record
    hClient:              OPCHANDLE;
    dwValueOffset:        DWORD;
    wQuality:             Word;
    wReserved:            Word;
  end;
  POPCITEMHEADER2 = ^OPCITEMHEADER2;
  OPCITEMHEADER2ARRAY = array[0..65535] of OPCITEMHEADER2;
  POPCITEMHEADER2ARRAY = ^OPCITEMHEADER2ARRAY;

  OPCGROUPHEADERWRITE = record
    dwItemCount:          DWORD;
    hClientGroup:         OPCHANDLE;
    dwTransactionID:      DWORD;
    hrStatus:             HResult;
  end;
  POPCGROUPHEADERWRITE = ^OPCGROUPHEADERWRITE;

  OPCITEMHEADERWRITE = record
    hClient:              OPCHANDLE;
    dwError:              HResult;
  end;
  POPCITEMHEADERWRITE = ^OPCITEMHEADERWRITE;
  OPCITEMHEADERWRITEARRAY = array[0..65535] of OPCITEMHEADERWRITE;
  POPCITEMHEADERWRITEARRAY = ^OPCITEMHEADERWRITEARRAY;

  OPCITEMSTATE = record
    hClient:              OPCHANDLE;
    ftTimeStamp:          TFileTime;
    wQuality:             Word;
    wReserved:            Word;
    vDataValue:           OleVariant;
  end;
  POPCITEMSTATE = ^OPCITEMSTATE;
  OPCITEMSTATEARRAY = array[0..65535] of OPCITEMSTATE;
  POPCITEMSTATEARRAY = ^OPCITEMSTATEARRAY;

  OPCSERVERSTATUS = record
    ftStartTime:          TFileTime;
    ftCurrentTime:        TFileTime;
    ftLastUpdateTime:     TFileTime;
    dwServerState:        OPCSERVERSTATE;
    dwGroupCount:         DWORD;
    dwBandWidth:          DWORD;
    wMajorVersion:        Word;
    wMinorVersion:        Word;
    wBuildNumber:         Word;
    wReserved:            Word;
    szVendorInfo:         POleStr;
  end;
  POPCSERVERSTATUS = ^OPCSERVERSTATUS;

  OPCITEMDEF = record
    szAccessPath:         POleStr;
    szItemID:             POleStr;
    bActive:              BOOL;
    hClient:              OPCHANDLE;
    dwBlobSize:           DWORD;
    pBlob:                PByteArray;
    vtRequestedDataType:  TVarType;
    wReserved:            Word;
  end;
  POPCITEMDEF = ^OPCITEMDEF;
  OPCITEMDEFARRAY = array[0..65535] of OPCITEMDEF;
  POPCITEMDEFARRAY = ^OPCITEMDEFARRAY;

  OPCITEMATTRIBUTES = record
    szAccessPath:         POleStr;
    szItemID:             POleStr;
    bActive:              BOOL;
    hClient:              OPCHANDLE;
    hServer:              OPCHANDLE;
    dwAccessRights:       DWORD;
    dwBlobSize:           DWORD;
    pBlob:                PByteArray;
    vtRequestedDataType:  TVarType;
    vtCanonicalDataType:  TVarType;
    dwEUType:             OPCEUTYPE;
    vEUInfo:              OleVariant;
  end;
  POPCITEMATTRIBUTES = ^OPCITEMATTRIBUTES;
  OPCITEMATTRIBUTESARRAY = array[0..65535] of OPCITEMATTRIBUTES;
  POPCITEMATTRIBUTESARRAY = ^OPCITEMATTRIBUTESARRAY;

  OPCITEMRESULT = record
    hServer:              OPCHANDLE;
    vtCanonicalDataType:  TVarType;
    wReserved:            Word;
    dwAccessRights:       DWORD;
    dwBlobSize:           DWORD;
    pBlob:                PByteArray;
  end;
  POPCITEMRESULT = ^OPCITEMRESULT;
  OPCITEMRESULTARRAY = array[0..65535] of OPCITEMRESULT;
  POPCITEMRESULTARRAY = ^OPCITEMRESULTARRAY;

  OPCITEMPROPERTY = record
    vtDataType:           TVarType;
    wReserved:            Word;
    dwPropertyID:         DWORD;
    szItemID:             POleStr;
    szDescription:        POleStr;
    vValue:               OleVariant;
    hrErrorID:            HResult;
    dwReserved:           DWORD;
  end;
  POPCITEMPROPERTY = ^OPCITEMPROPERTY;
  OPCITEMPROPERTYARRAY = array[0..65535] of OPCITEMPROPERTY;
  POPCITEMPROPERTYARRAY = ^OPCITEMPROPERTYARRAY;

  OPCITEMPROPERTIES = record
    hrErrorID:            HResult;
    dwNumProperties:      DWORD;
    pItemProperties:      POPCITEMPROPERTYARRAY;
    dwReserved:           DWORD;
  end;
  POPCITEMPROPERTIES = ^OPCITEMPROPERTIES;
  OPCITEMPROPERTIESARRAY = array[0..65535] of OPCITEMPROPERTIES;
  POPCITEMPROPERTIESARRAY = ^OPCITEMPROPERTIESARRAY;

  OPCBROWSEELEMENT = record
    szName:               POleStr;
    szItemID:             POleStr;
    dwFlagValue:          DWORD;
    dwReserved:           DWORD;
    ItemProperties:       OPCITEMPROPERTIES;
  end;
  POPCBROWSEELEMENT = ^OPCBROWSEELEMENT;
  OPCBROWSEELEMENTARRAY = array[0..65535] of OPCBROWSEELEMENT;
  POPCBROWSEELEMENTARRAY = ^OPCBROWSEELEMENTARRAY;

  OPCITEMVQT = record
    vDataValue:           OleVariant;
    bQualitySpecified:    BOOL;
    wQuality:             Word;
    wReserved:            Word;
    bTimeStampSpecified:  BOOL;
    dwReserved:           DWORD;
    ftTimeStamp:          TFileTime;
  end;
  POPCITEMVQT = ^OPCITEMVQT;
  OPCITEMVQTARRAY = array[0..65535] of OPCITEMVQT;
  POPCITEMVQTARRAY = ^OPCITEMVQTARRAY;

// *********************************************************************//
// Interface: IOPCServer
// GUID:      {39C13A4D-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCServer = interface(IUnknown)
    ['{39C13A4D-011E-11D0-9675-0020AFD8ADB3}']
    function AddGroup(
            szName:                     POleStr;
            bActive:                    BOOL;
            dwRequestedUpdateRate:      DWORD;
            hClientGroup:               OPCHANDLE;
            pTimeBias:                  PLongint;
            pPercentDeadband:           PSingle;
            dwLCID:                     DWORD;
      out   phServerGroup:              OPCHANDLE;
      out   pRevisedUpdateRate:         DWORD;
      const riid:                       TIID;
      out   ppUnk:                      IUnknown): HResult; stdcall;
    function GetErrorString(
            dwError:                    HResult;
            dwLocale:                   TLCID;
      out   ppString:                   POleStr): HResult; stdcall;
    function GetGroupByName(
            szName:                     POleStr;
      const riid:                       TIID;
      out   ppUnk:                      IUnknown): HResult; stdcall;
    function GetStatus(
      out   ppServerStatus:             POPCSERVERSTATUS): HResult; stdcall;
    function RemoveGroup(
            hServerGroup:               OPCHANDLE;
            bForce:                     BOOL): HResult; stdcall;
    function CreateGroupEnumerator(
            dwScope:                    OPCENUMSCOPE;
      const riid:                       TIID;
      out   ppUnk:                      IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCServerPublicGroups
// GUID:      {39C13A4E-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCServerPublicGroups = interface(IUnknown)
    ['{39C13A4E-011E-11D0-9675-0020AFD8ADB3}']
    function GetPublicGroupByName(
            szName:                     POleStr;
      const riid:                       TIID;
      out   ppUnk:                      IUnknown): HResult; stdcall;
    function RemovePublicGroup(
            hServerGroup:               OPCHANDLE;
            bForce:                     BOOL): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCBrowseServerAddressSpace
// GUID:      {39C13A4F-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCBrowseServerAddressSpace = interface(IUnknown)
    ['{39C13A4F-011E-11D0-9675-0020AFD8ADB3}']
    function QueryOrganization(
      out   pNameSpaceType:             OPCNAMESPACETYPE): HResult; stdcall;
    function ChangeBrowsePosition(
            dwBrowseDirection:          OPCBROWSEDIRECTION;
            szString:                   POleStr): HResult; stdcall;
    function BrowseOPCItemIDs(
            dwBrowseFilterType:         OPCBROWSETYPE;
            szFilterCriteria:           POleStr;
            vtDataTypeFilter:           TVarType;
            dwAccessRightsFilter:       DWORD;
      out   ppIEnumString:              IEnumString): HResult; stdcall;
    function GetItemID(
            szItemDataID:               POleStr;
      out   szItemID:                   POleStr): HResult; stdcall;
    function BrowseAccessPaths(
            szItemID:                   POleStr;
      out   ppIEnumString:              IEnumString): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCGroupStateMgt
// GUID:      {39C13A50-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCGroupStateMgt = interface(IUnknown)
    ['{39C13A50-011E-11D0-9675-0020AFD8ADB3}']
    function GetState(
      out   pUpdateRate:                DWORD;
      out   pActive:                    BOOL;
      out   ppName:                     POleStr;
      out   pTimeBias:                  Longint;
      out   pPercentDeadband:           Single;
      out   pLCID:                      TLCID;
      out   phClientGroup:              OPCHANDLE;
      out   phServerGroup:              OPCHANDLE): HResult; stdcall;
    function SetState(
            pRequestedUpdateRate:       PDWORD;
      out   pRevisedUpdateRate:         DWORD;
            pActive:                    PBOOL;
            pTimeBias:                  PLongint;
            pPercentDeadband:           PSingle;
            pLCID:                      PLCID;
            phClientGroup:              POPCHANDLE): HResult; stdcall;
    function SetName(
            szName:                     POleStr): HResult; stdcall;
    function CloneGroup(
            szName:                     POleStr;
      const riid:                       TIID;
      out   ppUnk:                      IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCPublicGroupStateMgt
// GUID:      {39C13A51-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCPublicGroupStateMgt = interface(IUnknown)
    ['{39C13A51-011E-11D0-9675-0020AFD8ADB3}']
    function GetState(
      out   pPublic:                    BOOL): HResult; stdcall;
    function MoveToPublic: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCSyncIO
// GUID:      {39C13A52-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCSyncIO = interface(IUnknown)
    ['{39C13A52-011E-11D0-9675-0020AFD8ADB3}']
    function Read(
            dwSource:                   OPCDATASOURCE;
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppItemValues:               POPCITEMSTATEARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Write(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pItemValues:                POleVariantArray;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCAsyncIO
// GUID:      {39C13A53-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCAsyncIO = interface(IUnknown)
    ['{39C13A53-011E-11D0-9675-0020AFD8ADB3}']
    function Read(
            dwConnection:               DWORD;
            dwSource:                   OPCDATASOURCE;
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   pTransactionID:             DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Write(
            dwConnection:               DWORD;
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pItemValues:                POleVariantArray;
      out   pTransactionID:             DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Refresh(
            dwConnection:               DWORD;
            dwSource:                   OPCDATASOURCE;
      out   pTransactionID:             DWORD): HResult; stdcall;
    function Cancel(
            dwTransactionID:            DWORD): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCItemMgt
// GUID:      {39C13A54-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCItemMgt = interface(IUnknown)
    ['{39C13A54-011E-11D0-9675-0020AFD8ADB3}']
    function AddItems(
            dwCount:                    DWORD;
            pItemArray:                 POPCITEMDEFARRAY;
      out   ppAddResults:               POPCITEMRESULTARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ValidateItems(
            dwCount:                    DWORD;
            pItemArray:                 POPCITEMDEFARRAY;
            bBlobUpdate:                BOOL;
      out   ppValidationResults:        POPCITEMRESULTARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function RemoveItems(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function SetActiveState(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            bActive:                    BOOL;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function SetClientHandles(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            phClient:                   POPCHANDLEARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function SetDatatypes(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pRequestedDatatypes:        PVarTypeList;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function CreateEnumerator(
      const riid:                       TIID;
      out   ppUnk:                      IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumOPCItemAttributes
// GUID:      {39C13A55-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IEnumOPCItemAttributes = interface(IUnknown)
    ['{39C13A55-011E-11D0-9675-0020AFD8ADB3}']
    function Next(
            celt:                       ULONG;
      out   ppItemArray:                POPCITEMATTRIBUTESARRAY;
      out   pceltFetched:               ULONG): HResult; stdcall;
    function Skip(
            celt:                       ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(
      out   ppEnumItemAttributes:       IEnumOPCItemAttributes):
            HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCDataCallback
// GUID:      {39C13A70-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCDataCallback = interface(IUnknown)
    ['{39C13A70-011E-11D0-9675-0020AFD8ADB3}']
    function OnDataChange(
            dwTransid:                  DWORD;
            hGroup:                     OPCHANDLE;
            hrMasterquality:            HResult;
            hrMastererror:              HResult;
            dwCount:                    DWORD;
            phClientItems:              POPCHANDLEARRAY;
            pvValues:                   POleVariantArray;
            pwQualities:                PWordArray;
            pftTimeStamps:              PFileTimeArray;
            pErrors:                    PResultList): HResult; stdcall;
    function OnReadComplete(
            dwTransid:                  DWORD;
            hGroup:                     OPCHANDLE;
            hrMasterquality:            HResult;
            hrMastererror:              HResult;
            dwCount:                    DWORD;
            phClientItems:              POPCHANDLEARRAY;
            pvValues:                   POleVariantArray;
            pwQualities:                PWordArray;
            pftTimeStamps:              PFileTimeArray;
            pErrors:                    PResultList): HResult; stdcall;
    function OnWriteComplete(
            dwTransid:                  DWORD;
            hGroup:                     OPCHANDLE;
            hrMastererr:                HResult;
            dwCount:                    DWORD;
            pClienthandles:             POPCHANDLEARRAY;
            pErrors:                    PResultList): HResult; stdcall;
    function OnCancelComplete(
            dwTransid:                  DWORD;
            hGroup:                     OPCHANDLE): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCAsyncIO2
// GUID:      {39C13A71-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCAsyncIO2 = interface(IUnknown)
    ['{39C13A71-011E-11D0-9675-0020AFD8ADB3}']
    function Read(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            dwTransactionID:            DWORD;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Write(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pItemValues:                POleVariantArray;
            dwTransactionID:            DWORD;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function Refresh2(
            dwSource:                   OPCDATASOURCE;
            dwTransactionID:            DWORD;
      out   pdwCancelID:                DWORD): HResult; stdcall;
    function Cancel2(
            dwCancelID:                 DWORD): HResult; stdcall;
    function SetEnable(
            bEnable:                    BOOL): HResult; stdcall;
    function GetEnable(
      out   pbEnable:                   BOOL): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCItemProperties
// GUID:      {39C13A72-011E-11D0-9675-0020AFD8ADB3}
// *********************************************************************//
  IOPCItemProperties = interface(IUnknown)
    ['{39C13A72-011E-11D0-9675-0020AFD8ADB3}']
    function QueryAvailableProperties(
            szItemID:                   POleStr;
      out   pdwCount:                   DWORD;
      out   ppPropertyIDs:              PDWORDARRAY;
      out   ppDescriptions:             POleStrList;
      out   ppvtDataTypes:              PVarTypeList): HResult; stdcall;
    function GetItemProperties(
            szItemID:                   POleStr;
            dwCount:                    DWORD;
            pdwPropertyIDs:             PDWORDARRAY;
      out   ppvData:                    POleVariantArray;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function LookupItemIDs(
            szItemID:                   POleStr;
            dwCount:                    DWORD;
            pdwPropertyIDs:             PDWORDARRAY;
      out   ppszNewItemIDs:             POleStrList;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCItemDeadbandMgt
// GUID:      {5946DA93-8B39-4ec8-AB3D-AA73DF5BC86F}
// *********************************************************************//
  IOPCItemDeadbandMgt = interface(IUnknown)
    ['{5946DA93-8B39-4ec8-AB3D-AA73DF5BC86F}']
    function SetItemDeadband(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pPercentDeadband:           PSingleArray;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function GetItemDeadband(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppPercentDeadband:          PSingleArray;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ClearItemDeadband(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCItemSamplingMgt
// GUID:      {3E22D313-F08B-41a5-86C8-95E95CB49FFC}
// *********************************************************************//
  IOPCItemSamplingMgt = interface(IUnknown)
    ['{3E22D313-F08B-41a5-86C8-95E95CB49FFC}']
    function SetItemSamplingRate(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pdwRequestedSamplingRate:   PDWORDARRAY;
      out   ppdwRevisedSamplingRate:    PDWORDARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function GetItemSamplingRate(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppdwSamplingRate:           PDWORDARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function ClearItemSamplingRate(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function SetItemBufferEnable(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pbEnable:                   PBOOLARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function GetItemBufferEnable(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
      out   ppbEnable:                  PBOOLARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCBrowse
// GUID:      {39227004-A18F-4b57-8B0A-5235670F4468}
// *********************************************************************//
  IOPCBrowse = interface(IUnknown)
    ['{39227004-A18F-4b57-8B0A-5235670F4468}']
    function GetProperties(
            dwItemCount:                DWORD;
            pszItemIDs:                 POleStrList;
            bReturnPropertyValues:      BOOL;
            dwPropertyCount:            DWORD;
            pdwPropertyIDs:             PDWORDARRAY;
      out   ppItemProperties:           POPCITEMPROPERTIESARRAY):
            HResult; stdcall;
    function Browse(
            szItemID:                   POleStr;
      var   pszContinuationPoint:       POleStr;
            dwMaxElementsReturned:      DWORD;
            dwBrowseFilter:             OPCBROWSEFILTER;
            szElementNameFilter:        POleStr;
            szVendorFilter:             POleStr;
            bReturnAllProperties:       BOOL;
            bReturnPropertyValues:      BOOL;
            dwPropertyCount:            DWORD;
            pdwPropertyIDs:             PDWORDARRAY;
      out   pbMoreElements:             BOOL;
      out   pdwCount:                   DWORD;
      out   ppBrowseElements:           POPCBROWSEELEMENTARRAY):
            HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCItemIO
// GUID:      {85C0B427-2893-4cbc-BD78-E5FC5146F08F}
// *********************************************************************//
  IOPCItemIO = interface(IUnknown)
    ['{85C0B427-2893-4cbc-BD78-E5FC5146F08F}']
    function Read(
            dwCount:                    DWORD;
            pszItemIDs:                 POleStrList;
            pdwMaxAge:                  PDWORDARRAY;
      out   ppvValues:                  POleVariantArray;
      out   ppwQualities:               PWordArray;
      out   ppftTimeStamps:             PFileTimeArray;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function WriteVQT(
            dwCount:                    DWORD;
            pszItemIDs:                 POleStrList;
            pItemVQT:                   POPCITEMVQTARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCSyncIO2
// GUID:      {730F5F0F-55B1-4c81-9E18-FF8A0904E1FA}
// *********************************************************************//
  IOPCSyncIO2 = interface(IOPCSyncIO)
    ['{730F5F0F-55B1-4c81-9E18-FF8A0904E1FA}']
    function ReadMaxAge(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pdwMaxAge:                  PDWORDARRAY;
      out   ppvValues:                  POleVariantArray;
      out   ppwQualities:               PWordArray;
      out   ppftTimeStamps:             PFileTimeArray;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function WriteVQT(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pItemVQT:                   POPCITEMVQTARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCAsyncIO3
// GUID:      {0967B97B-36EF-423e-B6F8-6BFF1E40D39D}
// *********************************************************************//
  IOPCAsyncIO3 = interface(IOPCAsyncIO2)
    ['{0967B97B-36EF-423e-B6F8-6BFF1E40D39D}']
    function ReadMaxAge(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pdwMaxAge:                  PDWORDARRAY;
            dwTransactionID:            DWORD;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function WriteVQT(
            dwCount:                    DWORD;
            phServer:                   POPCHANDLEARRAY;
            pItemVQT:                   POPCITEMVQTARRAY;
            dwTransactionID:            DWORD;
      out   pdwCancelID:                DWORD;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function RefreshMaxAge(
            dwMaxAge:                   DWORD;
            dwTransactionID:            DWORD;
      out   pdwCancelID:                DWORD): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCGroupStateMgt2
// GUID:      {8E368666-D72E-4f78-87ED-647611C61C9F}
// *********************************************************************//
  IOPCGroupStateMgt2 = interface(IOPCGroupStateMgt)
    ['{8E368666-D72E-4f78-87ED-647611C61C9F}']
    function SetKeepAlive(
            dwKeepAliveTime:            DWORD;
      out   pdwRevisedKeepAliveTime:    DWORD): HResult; stdcall;
    function GetKeepAlive(
      out   pdwKeepAliveTime:           DWORD): HResult; stdcall;
  end;

// Register clipboard formats for use with IDataObject / IAdviseSink
procedure OPCDARegisterClipboardFormats;

var

// OPC data stream formats
  OPCSTMFORMATDATA: UINT;
  OPCSTMFORMATDATATIME: UINT;
  OPCSTMFORMATWRITECOMPLETE: UINT;

implementation

procedure OPCDARegisterClipboardFormats;
begin
  OPCSTMFORMATDATA := RegisterClipboardFormat('OPCSTMFORMATDATA');
  OPCSTMFORMATDATATIME := RegisterClipboardFormat('OPCSTMFORMATDATATIME');
  OPCSTMFORMATWRITECOMPLETE :=
    RegisterClipboardFormat('OPCSTMFORMATWRITECOMPLETE');
end;

initialization
  OPCDARegisterClipboardFormats;

end.
