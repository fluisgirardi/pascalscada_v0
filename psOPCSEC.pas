
{*******************************************************}
{                                                       }
{       OPC Security 1.0                                }
{                                                       }
{       Delphi conversion maintained and supplied by    }
{       Mike Dillamore                                  }
{       OPC Programmers' Connection                     }
{       http://www.opcconnect.com/                      }
{       mailto:opc@opcconnect.com                       }
{                                                       }
{       Originally contributed by                       }
{       Illya Kysil                                     }
{       mailto:ikysil@ua.fm                             }
{                                                       }
{*******************************************************}

unit psOPCSEC;

{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

// ************************************************************************ //
// Type Lib: opcSec_PS.dll
// IID\LCID: {7AA83AFF-6C77-11D3-84F9-00008630A38B}\0
// ************************************************************************ //

interface

uses
  Windows, ActiveX, psOPCtypes;

// *********************************************************************//
// GUIDS declared in the TypeLibrary                                    //
// *********************************************************************//
const
  LIBID_OPCSEC: TIID = '{7AA83AFF-6C77-11D3-84F9-00008630A38B}';
  IID_IOPCSecurityNT: TIID = '{7AA83A01-6C77-11D3-84F9-00008630A38B}';
  IID_IOPCSecurityPrivate: TIID = '{7AA83A02-6C77-11D3-84F9-00008630A38B}';

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                  //
// *********************************************************************//
  IOPCSecurityNT = interface;
  IOPCSecurityPrivate = interface;

// *********************************************************************//
// Interface: IOPCSecurityNT
// GUID:      {7AA83A01-6C77-11D3-84F9-00008630A38B}
// *********************************************************************//
  IOPCSecurityNT = interface(IUnknown)
    ['{7AA83A01-6C77-11D3-84F9-00008630A38B}']
    function IsAvailableNT(
      out   pbAvailable:                BOOL): HResult; stdcall;
    function QueryMinImpersonationLevel(
      out   pdwMinImpLevel:             DWORD): HResult; stdcall;
    function ChangeUser: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOPCSecurityPrivate
// GUID:      {7AA83A02-6C77-11D3-84F9-00008630A38B}
// *********************************************************************//
  IOPCSecurityPrivate = interface(IUnknown)
    ['{7AA83A02-6C77-11D3-84F9-00008630A38B}']
    function IsAvailablePriv(
      out   pbAvailable:                BOOL): HResult; stdcall;
    function Logon(
            szUserID:                   POleStr;
            szPassword:                 POleStr): HResult; stdcall;
    function Logoff: HResult; stdcall;
  end;

implementation

end.
