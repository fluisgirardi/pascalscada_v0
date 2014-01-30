
{*******************************************************}
{                                                       }
{       OPCtypes.pas                                    }
{                                                       }
{       Standard type definitions shared across         }
{       multiple OPC specifications                     }
{                                                       }
{*******************************************************}

unit psOPCtypes;

interface

uses
  Windows, ActiveX;

type
  TOleEnum          = type LongInt;

  OPCHANDLE         = DWORD;
  POPCHANDLE        = ^OPCHANDLE;
  OPCHANDLEARRAY    = array[0..65535] of OPCHANDLE;
  POPCHANDLEARRAY   = ^OPCHANDLEARRAY;

  PVarType          = ^TVarType;
  TVarTypeList      = array[0..65535] of TVarType;
  PVarTypeList      = ^TVarTypeList;

  POleVariant       = ^OleVariant;
  OleVariantArray   = array[0..65535] of OleVariant;
  POleVariantArray  = ^OleVariantArray;

  PLCID             = ^TLCID;

  BOOLARRAY         = array[0..65535] of BOOL;
  PBOOLARRAY        = ^BOOLARRAY;

  DWORDARRAY        = array[0..65535] of DWORD;
  PDWORDARRAY       = ^DWORDARRAY;

  SingleArray       = array[0..65535] of Single;
  PSingleArray      = ^SingleArray;

  TFileTimeArray    = array[0..65535] of TFileTime;
  PFileTimeArray    = ^TFileTimeArray;

implementation

end.