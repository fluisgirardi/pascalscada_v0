
{*******************************************************}
{                                                       }
{       OPC status and error codes                      }
{                                                       }
{       Delphi conversion generated and supplied by     }
{       Mike Dillamore                                  }
{       OPC Programmers' Connection                     }
{       http://www.opcconnect.com/                      }
{       mailto:opc@opcconnect.com                       }
{                                                       }
{*******************************************************}

unit psOpcError;

{
Module Name:
    OpcError.h
Author:
    OPC Task Force
Revision History:
Release 1.0A
    Removed Unused messages
    Added OPC_S_INUSE, OPC_E_INVALIDCONFIGFILE, OPC_E_NOTFOUND
Release 2.0
    Added OPC_E_INVALID_PID
Release 3.0
    Added new error codes for DA3.0
    Added error codes for complex data

Module Name:
    opcae_er.h
Author:
    Jim Luth - OPC Alarm & Events Committee
Revision History:

Module Name:
    OpcHDAError.h
Author:
    Ayana Craven, OSI Software, Inc.
Revision History:

Module Name:
    OpcErrSec.h
Author:
    OPC Security Task Force
Revision History:
Release 1.0 08/18/00
    OPC security HRESULTs
}

{
Code Assignements:
  0000 to 0200 are reserved for Microsoft use
  (although some were inadverdantly used for OPC Data Access 1.0 errors).
  0200 to 7FFF are reserved for future OPC use.
  8000 to FFFF can be vendor specific.
}

interface

uses
  Windows;

const

  //
  //  Values are 32 bit values laid out as follows:
  //
  //   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
  //   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
  //  +---+-+-+-----------------------+-------------------------------+
  //  |Sev|C|R|     Facility          |               Code            |
  //  +---+-+-+-----------------------+-------------------------------+
  //
  //  where
  //
  //      Sev - is the severity code
  //
  //          00 - Success
  //          01 - Informational
  //          10 - Warning
  //          11 - Error
  //
  //      C - is the Customer code flag
  //
  //      R - is a reserved bit
  //
  //      Facility - is the facility code
  //
  //      Code - is the facility's status code
  //

  // OPC Data Access

  //
  // MessageId: OPC_E_INVALIDHANDLE
  //
  // MessageText:
  //
  //  The value of the handle is invalid.
  //
  OPC_E_INVALIDHANDLE = HResult($C0040001);

  //
  // MessageId: OPC_E_BADTYPE
  //
  // MessageText:
  //
  //  The server cannot convert the data between the
  //  requested data type and the canonical data type.
  //
  OPC_E_BADTYPE = HResult($C0040004);

  //
  // MessageId: OPC_E_PUBLIC
  //
  // MessageText:
  //
  //  The requested operation cannot be done on a public group.
  //
  OPC_E_PUBLIC = HResult($C0040005);

  //
  // MessageId: OPC_E_BADRIGHTS
  //
  // MessageText:
  //
  //  The Items AccessRights do not allow the operation.
  //
  OPC_E_BADRIGHTS = HResult($C0040006);

  //
  // MessageId: OPC_E_UNKNOWNITEMID
  //
  // MessageText:
  //
  //  The item is no longer available in the server address space.
  //
  OPC_E_UNKNOWNITEMID = HResult($C0040007);

  //
  // MessageId: OPC_E_INVALIDITEMID
  //
  // MessageText:
  //
  //  The item definition doesn't conform to the server's syntax.
  //
  OPC_E_INVALIDITEMID = HResult($C0040008);

  //
  // MessageId: OPC_E_INVALIDFILTER
  //
  // MessageText:
  //
  //  The filter string was not valid.
  //
  OPC_E_INVALIDFILTER = HResult($C0040009);

  //
  // MessageId: OPC_E_UNKNOWNPATH
  //
  // MessageText:
  //
  //  The item's access path is not known to the server.
  //
  OPC_E_UNKNOWNPATH = HResult($C004000A);

  //
  // MessageId: OPC_E_RANGE
  //
  // MessageText:
  //
  //  The value was out of range.
  //
  OPC_E_RANGE = HResult($C004000B);

  //
  // MessageId: OPC_E_DUPLICATENAME
  //
  // MessageText:
  //
  //  Duplicate name not allowed.
  //
  OPC_E_DUPLICATENAME = HResult($C004000C);

  //
  // MessageId: OPC_S_UNSUPPORTEDRATE
  //
  // MessageText:
  //
  //  The server does not support the requested data rate
  //  but will use the closest available rate.
  //
  OPC_S_UNSUPPORTEDRATE = HResult($0004000D);

  //
  // MessageId: OPC_S_CLAMP
  //
  // MessageText:
  //
  //  A value passed to WRITE was accepted but the output was clamped.
  //
  OPC_S_CLAMP = HResult($0004000E);

  //
  // MessageId: OPC_S_INUSE
  //
  // MessageText:
  //
  //  The operation cannot be completed because the
  //  object still has references that exist.
  //
  OPC_S_INUSE = HResult($0004000F);

  //
  // MessageId: OPC_E_INVALIDCONFIGFILE
  //
  // MessageText:
  //
  //  The server's configuration file is an invalid format.
  //
  OPC_E_INVALIDCONFIGFILE = HResult($C0040010);

  //
  // MessageId: OPC_E_NOTFOUND
  //
  // MessageText:
  //
  //  The server could not locate the requested object.
  //
  OPC_E_NOTFOUND = HResult($C0040011);

  //
  // MessageId: OPC_E_INVALID_PID
  //
  // MessageText:
  //
  //  The server does not recognise the passed property ID.
  //
  OPC_E_INVALID_PID = HResult($C0040203);

  //
  // MessageId: OPC_E_DEADBANDNOTSET
  //
  // MessageText:
  //
  //  The item deadband has not been set for this item.
  //
  OPC_E_DEADBANDNOTSET = HResult($C0040400);

  //
  // MessageId: OPC_E_DEADBANDNOTSUPPORTED
  //
  // MessageText:
  //
  //  The item does not support deadband.
  //
  OPC_E_DEADBANDNOTSUPPORTED = HResult($C0040401);

  //
  // MessageId: OPC_E_NOBUFFERING
  //
  // MessageText:
  //
  //  The server does not support buffering of data items that are collected at
  //  a faster rate than the group update rate.
  //
  OPC_E_NOBUFFERING = HResult($C0040402);

  //
  // MessageId: OPC_E_INVALIDCONTINUATIONPOINT
  //
  // MessageText:
  //
  //  The continuation point is not valid.
  //
  OPC_E_INVALIDCONTINUATIONPOINT = HResult($C0040403);

  //
  // MessageId: OPC_S_DATAQUEUEOVERFLOW
  //
  // MessageText:
  //
  //  Data Queue Overflow - Some value transitions were lost.
  //
  OPC_S_DATAQUEUEOVERFLOW = HResult($00040404);

  //
  // MessageId: OPC_E_RATENOTSET
  //
  // MessageText:
  //
  //  Server does not support requested rate.
  //
  OPC_E_RATENOTSET = HResult($C0040405);

  //
  // MessageId: OPC_E_NOTSUPPORTED
  //
  // MessageText:
  //
  //  The server does not support writing of quality and/or timestamp.
  //
  OPC_E_NOTSUPPORTED = HResult($C0040406);

  //
  // MessageId: OPCCPX_E_TYPE_CHANGED
  //
  // MessageText:
  //
  //  The dictionary and/or type description for the item has changed.
  //
  OPCCPX_E_TYPE_CHANGED = HResult($C0040407);

  //
  // MessageId: OPCCPX_E_FILTER_DUPLICATE
  //
  // MessageText:
  //
  //  A data filter item with the specified name already exists.
  //
  OPCCPX_E_FILTER_DUPLICATE = HResult($C0040408);

  //
  // MessageId: OPCCPX_E_FILTER_INVALID
  //
  // MessageText:
  //
  //  The data filter value does not conform to the server's syntax.
  //
  OPCCPX_E_FILTER_INVALID = HResult($C0040409);

  //
  // MessageId: OPCCPX_E_FILTER_ERROR
  //
  // MessageText:
  //
  //  An error occurred when the filter value was applied to the source data.
  //
  OPCCPX_E_FILTER_ERROR = HResult($C004040A);

  //
  // MessageId: OPCCPX_S_FILTER_NO_DATA
  //
  // MessageText:
  //
  //  The item value is empty because the data filter has excluded all fields.
  //
  OPCCPX_S_FILTER_NO_DATA = HResult($0004040B);

  // OPC Alarms & Events

  //
  // MessageId: OPC_S_ALREADYACKED
  //
  // MessageText:
  //
  //  The condition has already been acknowleged
  //
  OPC_S_ALREADYACKED = HResult($00040200);

  //
  // MessageId: OPC_S_INVALIDBUFFERTIME
  //
  // MessageText:
  //
  //  The buffer time parameter was invalid
  //
  OPC_S_INVALIDBUFFERTIME = HResult($00040201);

  //
  // MessageId: OPC_S_INVALIDMAXSIZE
  //
  // MessageText:
  //
  //  The max size parameter was invalid
  //
  OPC_S_INVALIDMAXSIZE = HResult($00040202);

  //
  // MessageId: OPC_S_INVALIDKEEPALIVETIME
  //
  // MessageText:
  //
  //  The KeepAliveTime parameter was invalid
  //
  OPC_S_INVALIDKEEPALIVETIME = HResult($00040203);

  //
  // MessageId: OPC_E_INVALIDBRANCHNAME
  //
  // MessageText:
  //
  //  The string was not recognized as an area name
  //
  OPC_E_INVALIDBRANCHNAME = HResult($C0040203);

  //
  // MessageId: OPC_E_INVALIDTIME
  //
  // MessageText:
  //
  //  The time does not match the latest active time
  //
  OPC_E_INVALIDTIME = HResult($C0040204);

  //
  // MessageId: OPC_E_BUSY
  //
  // MessageText:
  //
  //  A refresh is currently in progress
  //
  OPC_E_BUSY = HResult($C0040205);

  //
  // MessageId: OPC_E_NOINFO
  //
  // MessageText:
  //
  //  Information is not available
  //
  OPC_E_NOINFO = HResult($C0040206);

  // OPC Historical Data Access

  //
  // MessageId: OPC_E_MAXEXCEEDED
  //
  // MessageText:
  //
  //  The maximum number of values requested exceeds the server's limit.
  //
  OPC_E_MAXEXCEEDED = HResult($C0041001);

  //
  // MessageId: OPC_S_NODATA
  //
  // MessageText:
  //
  //  There is no data within the specified parameters
  //
  OPC_S_NODATA = HResult($40041002);

  //
  // MessageId: OPC_S_MOREDATA
  //
  // MessageText:
  //
  // There is more data satisfying the query than was returned
  //
  OPC_S_MOREDATA = HResult($40041003);

  //
  // MessageId: OPC_E_INVALIDAGGREGATE
  //
  // MessageText:
  //
  //  The aggregate requested is not valid.
  //
  OPC_E_INVALIDAGGREGATE = HResult($C0041004);

  //
  // MessageId: OPC_S_CURRENTVALUE
  //
  // MessageText:
  //
  //  The server only returns current values for the requested item attributes.
  //
  OPC_S_CURRENTVALUE = HResult($40041005);

  //
  // MessageId: OPC_S_EXTRADATA
  //
  // MessageText:
  //
  //  Additional data satisfying the query was found.
  //
  OPC_S_EXTRADATA = HResult($40041006);

  //
  // MessageId: OPC_W_NOFILTER
  //
  // MessageText:
  //
  //  The server does not support this filter.
  //
  OPC_W_NOFILTER = HResult($80041007);

  //
  // MessageId: OPC_E_UNKNOWNATTRID
  //
  // MessageText:
  //
  //  The server does not support this attribute.
  //
  OPC_E_UNKNOWNATTRID = HResult($C0041008);

  //
  // MessageId: OPC_E_NOT_AVAIL
  //
  // MessageText:
  //
  //  The requested aggregate is not available for the specified item.
  //
  OPC_E_NOT_AVAIL = HResult($C0041009);

  //
  // MessageId: OPC_E_INVALIDDATATYPE
  //
  // MessageText:
  //
  //  The supplied value for the attribute is not a correct data type.
  //
  OPC_E_INVALIDDATATYPE = HResult($C004100A);

  //
  // MessageId: OPC_E_DATAEXISTS
  //
  // MessageText:
  //
  //  Unable to insert - data already present.
  //
  OPC_E_DATAEXISTS = HResult($C004100B);

  //
  // MessageId: OPC_E_INVALIDATTRID
  //
  // MessageText:
  //
  //  The supplied attribute ID is not valid.
  //
  OPC_E_INVALIDATTRID = HResult($C004100C);

  //
  // MessageId: OPC_E_NODATAEXISTS
  //
  // MessageText:
  //
  //  The server has no value for the specified time and item ID.
  //
  OPC_E_NODATAEXISTS = HResult($C004100D);

  //
  // MessageId: OPC_S_INSERTED
  //
  // MessageText:
  //
  //  The requested insert occurred.
  //
  OPC_S_INSERTED = HResult($4004100E);

  //
  // MessageId: OPC_S_REPLACED
  //
  // MessageText:
  //
  //  The requested replace occurred.
  //
  OPC_S_REPLACED = HResult($4004100F);

  // OPC Security

  //
  // MessageId: OPC_E_PRIVATE_ACTIVE
  //
  // MessageText:
  //
  //  OPC Security: A session using private OPC credentials is already active.
  //
  OPC_E_PRIVATE_ACTIVE = HResult($C0040301);

  //
  // MessageId: OPC_E_LOW_IMPERS_LEVEL
  //
  // MessageText:
  //
  //  OPC Security: Server requires higher impersonation level to access secured
  //  data.
  //
  OPC_E_LOW_IMPERS_LEVEL = HResult($C0040302);

  //
  // MessageId: OPC_S_LOW_AUTHN_LEVEL
  //
  // MessageText:
  //
  //  OPC Security: Server expected higher level of package privacy.
  //
  OPC_S_LOW_AUTHN_LEVEL = HResult($00040303);

implementation

end.
