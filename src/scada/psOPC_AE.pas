
{*******************************************************}
{                                                       }
{       OPC Alarms and Events 1.10                      }
{                                                       }
{       Delphi conversion generated and supplied by     }
{       Mike Dillamore                                  }
{       OPC Programmers' Connection                     }
{       http://www.opcconnect.com/                      }
{       mailto:opc@opcconnect.com                       }
{                                                       }
{*******************************************************}

unit psOPC_AE;

{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

// ************************************************************************ //
// Type Lib: opc_aeps.dll
// IID\LCID: {65168844-5783-11D1-84A0-00608CB8A7E9}\0
// ************************************************************************ //

interface

uses
  Windows, ActiveX, psOPCtypes;

// *********************************************************************//
// GUIDS declared in the TypeLibrary                                    //
// *********************************************************************//
const
  LIBID_OPC_AE: TGUID = '{65168844-5783-11D1-84A0-00608CB8A7E9}';
  IID_IOPCEventServer: TIID = '{65168851-5783-11D1-84A0-00608CB8A7E9}';
  IID_IOPCEventSubscriptionMgt: TIID = '{65168855-5783-11D1-84A0-00608CB8A7E9}';
  IID_IOPCEventAreaBrowser: TIID = '{65168857-5783-11D1-84A0-00608CB8A7E9}';
  IID_IOPCEventSink: TIID = '{6516885F-5783-11D1-84A0-00608CB8A7E9}';
  IID_IOPCEventServer2: TIID = '{71BBE88E-9564-4BCD-BCFC-71C558D94F2D}';
  IID_IOPCEventSubscriptionMgt2: TIID = '{94C955DC-3684-4CCB-AFAB-F898CE19AAC3}';

  OPCEventServerCATID: TGUID = '{58E13251-AC87-11D1-84D5-00608CB8A7E9}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                  //
// *********************************************************************//
type
  OPCAEBROWSEDIRECTION = TOleEnum;
const
  OPCAE_BROWSE_UP   = 1;
  OPCAE_BROWSE_DOWN = 2;
  OPCAE_BROWSE_TO   = 3;

type
  OPCAEBROWSETYPE = TOleEnum;
const
  OPC_AREA   = 1;
  OPC_SOURCE = 2;

type
  OPCEVENTSERVERSTATE = TOleEnum;
const
  OPCAE_STATUS_RUNNING    = 1;
  OPCAE_STATUS_FAILED     = 2;
  OPCAE_STATUS_NOCONFIG   = 3;
  OPCAE_STATUS_SUSPENDED  = 4;
  OPCAE_STATUS_TEST       = 5;
  OPCAE_STATUS_COMM_FAULT = 6;

// *********************************************************************//
// OPC Quality flags                                                    //
// *********************************************************************//
// Masks for extracting quality subfields
// (note 'status' mask also includes 'Quality' bits)
  OPC_QUALITY_MASK           = $C0;
  OPC_STATUS_MASK            = $FC;
  OPC_LIMIT_MASK             = $03;

// Values for QUALITY_MASK bit field
  OPC_QUALITY_BAD            = $00;
  OPC_QUALITY_UNCERTAIN      = $40;
  OPC_QUALITY_GOOD           = $C0;

// STATUS_MASK Values for Quality = BAD
  OPC_QUALITY_CONFIG_ERROR   = $04;
  OPC_QUALITY_NOT_CONNECTED  = $08;
  OPC_QUALITY_DEVICE_FAILURE = $0C;
  OPC_QUALITY_SENSOR_FAILURE = $10;
  OPC_QUALITY_LAST_KNOWN     = $14;
  OPC_QUALITY_COMM_FAILURE   = $18;
  OPC_QUALITY_OUT_OF_SERVICE = $1C;

// STATUS_MASK Values for Quality = UNCERTAIN
  OPC_QUALITY_LAST_USABLE    = $44;
  OPC_QUALITY_SENSOR_CAL     = $50;
  OPC_QUALITY_EGU_EXCEEDED   = $54;
  OPC_QUALITY_SUB_NORMAL     = $58;

// STATUS_MASK Values for Quality = GOOD
  OPC_QUALITY_LOCAL_OVERRIDE = $D8;

// State bit masks
  OPC_CONDITION_ENABLED = $0001;
  OPC_CONDITION_ACTIVE  = $0002;
  OPC_CONDITION_ACKED   = $0004;

// bit masks for m_wChangeMask
  OPC_CHANGE_ACTIVE_STATE = $0001;
  OPC_CHANGE_ACK_STATE    = $0002;
  OPC_CHANGE_ENABLE_STATE = $0004;
  OPC_CHANGE_QUALITY      = $0008;
  OPC_CHANGE_SEVERITY     = $0010;
  OPC_CHANGE_SUBCONDITION = $0020;
  OPC_CHANGE_MESSAGE      = $0040;
  OPC_CHANGE_ATTRIBUTE    = $0080;

// dwEventType
  OPC_SIMPLE_EVENT    = $0001;
  OPC_TRACKING_EVENT  = $0002;
  OPC_CONDITION_EVENT = $0004;
  OPC_ALL_EVENTS      = OPC_SIMPLE_EVENT or OPC_TRACKING_EVENT or
    OPC_CONDITION_EVENT;

// QueryAvailableFilters() bit masks
  OPC_FILTER_BY_EVENT    = $0001;
  OPC_FILTER_BY_CATEGORY = $0002;
  OPC_FILTER_BY_SEVERITY = $0004;
  OPC_FILTER_BY_AREA     = $0008;
  OPC_FILTER_BY_SOURCE   = $0010;

type

// *********************************************************************//
// Forward declaration of interfaces defined in Type Library            //
// *********************************************************************//
  IOPCEventServer = interface;
  IOPCEventSubscriptionMgt = interface;
  IOPCEventAreaBrowser = interface;
  IOPCEventSink = interface;
  IOPCEventServer2 = interface;
  IOPCEventSubscriptionMgt2 = interface;

// *********************************************************************//
// Declaration of structures, unions and aliases.                       //
// *********************************************************************//
  ONEVENTSTRUCT = record
    wChangeMask:          Word;
    wNewState:            Word;
    szSource:             POleStr;
    ftTime:               TFileTime;
    szMessage:            POleStr;
    dwEventType:          DWORD;
    dwEventCategory:      DWORD;
    dwSeverity:           DWORD;
    szConditionName:      POleStr;
    szSubconditionName:   POleStr;
    wQuality:             Word;
    wReserved:            Word;     // added to ensure natural alignment
    bAckRequired:         BOOL;
    ftActiveTime:         TFileTime;
    dwCookie:             DWORD;
    dwNumEventAttrs:      DWORD;
    pEventAttributes:     POleVariantArray;
    szActorID:            POleStr;
  end;
  PONEVENTSTRUCT = ^ONEVENTSTRUCT;
  ONEVENTSTRUCTARRAY = array[0..65535] of ONEVENTSTRUCT;
  PONEVENTSTRUCTARRAY = ^ONEVENTSTRUCTARRAY;

  OPCEVENTSERVERSTATUS = record
    ftStartTime:          TFileTime;
    ftCurrentTime:        TFileTime;
    ftLastUpdateTime:     TFileTime;
    dwServerState:        OPCEVENTSERVERSTATE;
    wMajorVersion:        Word;
    wMinorVersion:        Word;
    wBuildNumber:         Word;
    wReserved:            Word;     // added to ensure natural alignment
    szVendorInfo:         POleStr;
  end;
  POPCEVENTSERVERSTATUS = ^OPCEVENTSERVERSTATUS;

  OPCCONDITIONSTATE = record
    wState:               Word;
    wReserved1:           Word;     // added to ensure natural alignment
    szActiveSubCondition: POleStr;
    szASCDefinition:      POleStr;
    dwASCSeverity:        DWORD;
    szASCDescription:     POleStr;
    wQuality:             Word;
    wReserved2:           Word;     // added to ensure natural alignment
    ftLastAckTime:        TFileTime;
    ftSubCondLastActive:  TFileTime;
    ftCondLastActive:     TFileTime;
    ftCondLastInactive:   TFileTime;
    szAcknowledgerID:     POleStr;
    szComment:            POleStr;
    dwNumSCs:             DWORD;
    pszSCNames:           POleStrList;
    pszSCDefinitions:     POleStrList;
    pdwSCSeverities:      PDWORDARRAY;
    pszSCDescriptions:    POleStrList;
    dwNumEventAttrs:      DWORD;
    pEventAttributes:     POleVariantArray;
    pErrors:              PResultList;
  end;
  POPCCONDITIONSTATE = ^OPCCONDITIONSTATE;

// *********************************************************************//
// Interface: IOPCEventServer
// GUID:      {65168851-5783-11D1-84A0-00608CB8A7E9}
// *********************************************************************//
  IOPCEventServer = interface(IUnknown)
    ['{65168851-5783-11D1-84A0-00608CB8A7E9}']
    function GetStatus(
      out   ppEventServerStatus:        POPCEVENTSERVERSTATUS):
            HResult; stdcall;
    function CreateEventSubscription(
            bActive:                    BOOL;
            dwBufferTime:               DWORD;
            dwMaxSize:                  DWORD;
            hClientSubscription:        OPCHANDLE;
      const riid:                       TIID;
      out   ppUnk:                      IUnknown;
      out   pdwRevisedBufferTime:       DWORD;
      out   pdwRevisedMaxSize:          DWORD): HResult; stdcall;
    function QueryAvailableFilters(
      out   pdwFilterMask:              DWORD): HResult; stdcall;
    function QueryEventCategories(
            dwEventType:                DWORD;
      out   pdwCount:                   DWORD;
      out   ppdwEventCategories:        PDWORDARRAY;
      out   ppszEventCategoryDescs:     POleStrList): HResult; stdcall;
    function QueryConditionNames(
            dwEventCategory:            DWORD;
      out   pdwCount:                   DWORD;
      out   ppszConditionNames:         POleStrList): HResult; stdcall;
    function QuerySubConditionNames(
            szConditionName:            POleStr;
      out   pdwCount:                   DWORD;
      out   ppszSubConditionNames:      POleStrList): HResult; stdcall;
    function QuerySourceConditions(
            szSource:                   POleStr;
      out   pdwCount:                   DWORD;
      out   ppszConditionNames:         POleStrList): HResult; stdcall;
    function QueryEventAttributes(
            dwEventCategory:            DWORD;
      out   pdwCount:                   DWORD;
      out   ppdwAttrIDs:                PDWORDARRAY;
      out   ppszAttrDescs:              POleStrList;
      out   ppvtAttrTypes:              PVarTypeList): HResult; stdcall;
    function TranslateToItemIDs(
            szSource:                   POleStr;
            dwEventCategory:            DWORD;
            szConditionName:            POleStr;
            szSubconditionName:         POleStr;
            dwCount:                    DWORD;
            pdwAssocAttrIDs:            PDWORDARRAY;
      out   ppszAttrItemIDs:            POleStrList;
      out   ppszNodeNames:              POleStrList;
      out   ppCLSIDs:                   PGUIDList): HResult; stdcall;
    function GetConditionState(
            szSource:                   POleStr;
            szConditionName:            POleStr;
            dwNumEventAttrs:            DWORD;
            pdwAttributeIDs:            PDWORDARRAY;
      out   ppConditionState:           POPCCONDITIONSTATE): HResult; stdcall;
    function EnableConditionByArea(
            dwNumAreas:                 DWORD;
            pszAreas:                   POleStrList): HResult; stdcall;
    function EnableConditionBySource(
            dwNumSources:               DWORD;
            pszSources:                 POleStrList): HResult; stdcall;
    function DisableConditionByArea(
            dwNumAreas:                 DWORD;
            pszAreas:                   POleStrList): HResult; stdcall;
    function DisableConditionBySource(
            dwNumSources:               DWORD;
            pszSources:                 POleStrList): HResult; stdcall;
    function AckCondition(
            dwCount:                    DWORD;
            szAcknowledgerID:           POleStr;
            szComment:                  POleStr;
            pszSource:                  POleStrList;
            pszConditionName:           POleStrList;
            pftActiveTime:              PFileTimeArray;
            pdwCookie:                  PDWORDARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function CreateAreaBrowser(
      const riid:                       TIID;
      out   ppUnk:                      IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCEventSubscriptionMgt
// GUID:      {65168855-5783-11D1-84A0-00608CB8A7E9}
// *********************************************************************//
  IOPCEventSubscriptionMgt = interface(IUnknown)
    ['{65168855-5783-11D1-84A0-00608CB8A7E9}']
    function SetFilter(
            dwEventType:                DWORD;
            dwNumCategories:            DWORD;
            pdwEventCategories:         PDWORDARRAY;
            dwLowSeverity:              DWORD;
            dwHighSeverity:             DWORD;
            dwNumAreas:                 DWORD;
            pszAreaList:                POleStrList;
            dwNumSources:               DWORD;
            pszSourceList:              POleStrList): HResult; stdcall;
    function GetFilter(
      out   pdwEventType:               DWORD;
      out   pdwNumCategories:           DWORD;
      out   ppdwEventCategories:        PDWORDARRAY;
      out   pdwLowSeverity:             DWORD;
      out   pdwHighSeverity:            DWORD;
      out   pdwNumAreas:                DWORD;
      out   ppszAreaList:               POleStrList;
      out   pdwNumSources:              DWORD;
      out   ppszSourceList:             POleStrList): HResult; stdcall;
    function SelectReturnedAttributes(
            dwEventCategory:            DWORD;
            dwCount:                    DWORD;
            dwAttributeIDs:             PDWORDARRAY): HResult; stdcall;
    function GetReturnedAttributes(
            dwEventCategory:            DWORD;
      out   pdwCount:                   DWORD;
      out   ppdwAttributeIDs:           PDWORDARRAY): HResult; stdcall;
    function Refresh(
            dwConnection:               DWORD): HResult; stdcall;
    function CancelRefresh(
            dwConnection:               DWORD): HResult; stdcall;
    function GetState(
      out   pbActive:                   BOOL;
      out   pdwBufferTime:              DWORD;
      out   pdwMaxSize:                 DWORD;
      out   phClientSubscription:       OPCHANDLE): HResult; stdcall;
    function SetState(
            pbActive:                   PBOOL;
            pdwBufferTime:              PDWORD;
            pdwMaxSize:                 PDWORD;
            hClientSubscription:        OPCHANDLE;
      out   pdwRevisedBufferTime:       DWORD;
      out   pdwRevisedMaxSize:          DWORD): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCEventAreaBrowser
// GUID:      {65168857-5783-11D1-84A0-00608CB8A7E9}
// *********************************************************************//
  IOPCEventAreaBrowser = interface(IUnknown)
    ['{65168857-5783-11D1-84A0-00608CB8A7E9}']
    function ChangeBrowsePosition(
            dwBrowseDirection:          OPCAEBROWSEDIRECTION;
            szString:                   POleStr): HResult; stdcall;
    function BrowseOPCAreas(
            dwBrowseFilterType:         OPCAEBROWSETYPE;
            szFilterCriteria:           POleStr;
      out   ppIEnumString:              IEnumString): HResult; stdcall;
    function GetQualifiedAreaName(
            szAreaName:                 POleStr;
      out   pszQualifiedAreaName:       POleStr): HResult; stdcall;
    function GetQualifiedSourceName(
            szSourceName:               POleStr;
      out   pszQualifiedSourceName:     POleStr): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCEventSink
// GUID:      {6516885F-5783-11D1-84A0-00608CB8A7E9}
// *********************************************************************//
  IOPCEventSink = interface(IUnknown)
    ['{6516885F-5783-11D1-84A0-00608CB8A7E9}']
    function OnEvent(
            hClientSubscription:        OPCHANDLE;
            bRefresh:                   BOOL;
            bLastRefresh:               BOOL;
            dwCount:                    DWORD;
            pEvents:                    PONEVENTSTRUCTARRAY): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCEventServer2
// GUID:      {71BBE88E-9564-4BCD-BCFC-71C558D94F2D}
// *********************************************************************//
  IOPCEventServer2 = interface(IOPCEventServer)
    ['{71BBE88E-9564-4BCD-BCFC-71C558D94F2D}']
    function EnableConditionByArea2(
            dwNumAreas:                 DWORD;
            pszAreas:                   POleStrList;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function EnableConditionBySource2(
            dwNumSources:               DWORD;
            pszSources:                 POleStrList;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function DisableConditionByArea2(
            dwNumAreas:                 DWORD;
            pszAreas:                   POleStrList;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function DisableConditionBySource2(
            dwNumSources:               DWORD;
            pszSources:                 POleStrList;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function GetEnableStateByArea(
            dwNumAreas:                 DWORD;
            pszAreas:                   POleStrList;
            pbEnabled:                  PBOOLARRAY;
            pbEffectivelyEnabled:       PBOOLARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
    function GetEnableStateBySource(
            dwNumSources:               DWORD;
            pszSources:                 POleStrList;
            pbEnabled:                  PBOOLARRAY;
            pbEffectivelyEnabled:       PBOOLARRAY;
      out   ppErrors:                   PResultList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCEventSubscriptionMgt2
// GUID:      {94C955DC-3684-4CCB-AFAB-F898CE19AAC3}
// *********************************************************************//
  IOPCEventSubscriptionMgt2 = interface(IOPCEventSubscriptionMgt)
    ['{94C955DC-3684-4CCB-AFAB-F898CE19AAC3}']
    function SetKeepAlive(
            dwKeepAliveTime:            DWORD;
      out   pdwRevisedKeepAliveTime:    DWORD): HResult; stdcall;
    function GetKeepAlive(
      out   pdwKeepAliveTime:           DWORD): HResult; stdcall;
  end;

implementation

end.
