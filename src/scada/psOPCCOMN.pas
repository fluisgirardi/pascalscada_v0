
{*******************************************************}
{                                                       }
{       OPC Common 1.1                                  }
{                                                       }
{       Delphi conversion generated and supplied by     }
{       Mike Dillamore                                  }
{       OPC Programmers' Connection                     }
{       http://www.opcconnect.com/                      }
{       mailto:opc@opcconnect.com                       }
{                                                       }
{*******************************************************}

unit psOPCCOMN;

{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

// ************************************************************************ //
// Type Lib: opccomn_ps.dll
// IID\LCID: {B28EEDB1-AC6F-11D1-84D5-00608CB8A7E9}\0
// ************************************************************************ //

interface

uses
  Windows, ActiveX;

// *********************************************************************//
// GUIDS declared in the TypeLibrary                                    //
// *********************************************************************//
const
  LIBID_OPCCOMN: TGUID = '{B28EEDB1-AC6F-11D1-84D5-00608CB8A7E9}';
  IID_IOPCCommon: TIID = '{F31DFDE2-07B6-11D2-B2D8-0060083BA1FB}';
  IID_IOPCShutdown: TIID = '{F31DFDE1-07B6-11D2-B2D8-0060083BA1FB}';
  IID_IOPCServerList: TIID = '{13486D50-4821-11D2-A494-3CB306C10000}';
  IID_IOPCEnumGUID: TIID = '{55C382C8-21C7-4e88-96C1-BECFB1E3F483}';
  IID_IOPCServerList2: TIID = '{9DD0B56C-AD9E-43ee-8305-487F3188BF7A}';

  CLSID_OPCServerList: TGUID = '{13486D51-4821-11D2-A494-3CB306C10000}';

type

// *********************************************************************//
// Forward declaration of interfaces defined in Type Library            //
// *********************************************************************//
  IOPCCommon = interface;
  IOPCShutdown = interface;
  IOPCServerList = interface;
  IOPCEnumGUID = interface;
  IOPCServerList2 = interface;

  // *********************************************************************//
  // Neded for  IOPCServerList  (peraps has to be added in struct.inc?)   //
  // *********************************************************************//
   PGUIDList = ^TGUIDList;
   TGUIDList = array[0..65535] of TGUID;

// *********************************************************************//
// Declaration of structures, unions and aliases.                       //
// *********************************************************************//
  LCIDARRAY = array[0..65535] of LCID;
  PLCIDARRAY = ^LCIDARRAY;

// *********************************************************************//
// Interface: IOPCCommon
// GUID:      {F31DFDE2-07B6-11D2-B2D8-0060083BA1FB}
// *********************************************************************//
  IOPCCommon = interface(IUnknown)
    ['{F31DFDE2-07B6-11D2-B2D8-0060083BA1FB}']
    function SetLocaleID(
            dwLcid:                     TLCID): HResult; stdcall;
    function GetLocaleID(
      out   pdwLcid:                    TLCID): HResult; stdcall;
    function QueryAvailableLocaleIDs(
      out   pdwCount:                   UINT;
      out   pdwLcid:                    PLCIDARRAY): HResult; stdcall;
    function GetErrorString(
            dwError:                    HResult;
      out   ppString:                   POleStr): HResult; stdcall;
    function SetClientName(
            szName:                     POleStr): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCShutdown
// GUID:      {F31DFDE1-07B6-11D2-B2D8-0060083BA1FB}
// *********************************************************************//
  IOPCShutdown = interface(IUnknown)
    ['{F31DFDE1-07B6-11D2-B2D8-0060083BA1FB}']
    function ShutdownRequest(
            szReason:                   POleStr): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCServerList
// GUID:      {13486D50-4821-11D2-A494-3CB306C10000}
// *********************************************************************//
  IOPCServerList = interface(IUnknown)
    ['{13486D50-4821-11D2-A494-3CB306C10000}']
    function EnumClassesOfCategories(
            cImplemented:               ULONG;
            rgcatidImpl:                PGUIDList;
            cRequired:                  ULONG;
            rgcatidReq:                 PGUIDList;
      out   ppenumClsid:                IEnumGUID): HResult; stdcall;
    function GetClassDetails(
      const clsid:                      TCLSID;
      out   ppszProgID:                 POleStr;
      out   ppszUserType:               POleStr): HResult; stdcall;
    function CLSIDFromProgID(
            szProgId:                   POleStr;
      out   clsid:                      TCLSID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCEnumGUID
// GUID:      {55C382C8-21C7-4e88-96C1-BECFB1E3F483}
// *********************************************************************//
  IOPCEnumGUID = interface(IUnknown)
    ['{55C382C8-21C7-4e88-96C1-BECFB1E3F483}']
    function Next(
            celt:                       UINT;
      out   rgelt:                      TGUID;
      out   pceltFetched:               UINT): HResult; stdcall;
    function Skip(
            celt:                       UINT): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(
      out   ppenum:                     IOPCEnumGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCServerList2
// GUID:      {9DD0B56C-AD9E-43ee-8305-487F3188BF7A}
// *********************************************************************//
  IOPCServerList2 = interface(IUnknown)
    ['{9DD0B56C-AD9E-43ee-8305-487F3188BF7A}']
    function EnumClassesOfCategories(
            cImplemented:               ULONG;
            rgcatidImpl:                PGUIDList;
            cRequired:                  ULONG;
            rgcatidReq:                 PGUIDList;
      out   ppenumClsid:                IOPCEnumGUID): HResult; stdcall;
    function GetClassDetails(
      const clsid:                      TCLSID;
      out   ppszProgID:                 POleStr;
      out   ppszUserType:               POleStr;
      out   ppszVerIndProgID:           POleStr): HResult; stdcall;
    function CLSIDFromProgID(
            szProgId:                   POleStr;
      out   clsid:                      TCLSID): HResult; stdcall;
  end;

implementation

end.
