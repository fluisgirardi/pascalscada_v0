(*
 * open62541 is licensed under the Mozilla Public License v2.0 (MPLv2).
 * This allows the open62541 library to be combined and distributed with any proprietary software.
 * Only changes to the open62541 library itself need to be licensed under the MPLv2 when copied and distributed.
 * The plugins, as well as the server and client examples are in the public domain (CC0 license).
 * They can be reused under any license and changes do not have to be published.
 *
 * Free Pascal binding for open62541 1.5 (tested with 1.5.10).
 *
 * BEWARE: open62541 1.4 changed the client/server configuration (EventLoop,
 * certificate groups, loggers...) and the type array, so this unit is NOT
 * binary compatible with 1.2/1.3 libraries. LoadOpen62541 checks the loaded
 * library (every entry of UA_TYPES and the encryption support) and refuses a
 * library this unit was not generated for.
 *
 * The library is always loaded at runtime (LoadOpen62541/UnloadOpen62541), so a
 * program that uses this unit only needs libopen62541 when it actually calls it.
 *
 * Author: Lacak <lacak At Sourceforge>
 *
 * Contributors:
 *   Luca Olivetti <luca@ventoso.org>
 *   Fabio Luis Girardi (port to open62541 1.5)
 *)
unit open62541;

(*
 * Example:
 *   var
 *     client: PUA_Client;
 *     value: UA_Variant;
 *   begin
 *     LoadOpen62541();
 *     client := UA_Client_new();
 *     UA_ClientConfig_setDefault(UA_Client_getConfig(client));
 *     if UA_Client_connect(client, 'opc.tcp://localhost:4840') = UA_STATUSCODE_GOOD then begin
 *       UA_Variant_init(value);
 *       if (UA_Client_readValueAttribute(client, UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_CURRENTTIME), @value) = UA_STATUSCODE_GOOD) and
 *          UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_DATETIME]) then begin
 *          ...
 *       end;
 *       UA_Variant_clear(value);
 *     end;
 *     UA_Client_delete(client);
 *     UnloadOpen62541();
 *   end;
 *)

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4} // C enums are 4 bytes
  {$PACKRECORDS C}
{$ELSE}
  {$MINENUMSIZE 4}
  {$ALIGN 8}
{$ENDIF}
{$H+}
{$POINTERMATH ON}

{$DEFINE ENABLE_SERVER}

{ ---------------- }
{ --- config.h --- }
{ ---------------- }
{$DEFINE UA_ENABLE_METHODCALLS}
{$DEFINE UA_ENABLE_SUBSCRIPTIONS}
{$DEFINE UA_ENABLE_STATUSCODE_DESCRIPTIONS}
{$DEFINE UA_ENABLE_TYPEDESCRIPTION}
// The library built with encryption (OpenSSL or mbedTLS) has more fields in
// UA_ClientConfig. Undefine this to use a library built without encryption
// (LoadOpen62541 tells when this define does not match the library).
{$DEFINE UA_ENABLE_ENCRYPTION}

interface

const
{$IFDEF MSWINDOWS}
  libopen62541 = 'libopen62541.dll';
{$ELSE}
  {$IFDEF DARWIN}
  libopen62541 = 'libopen62541.dylib';
  {$ELSE}
  libopen62541 = 'libopen62541.so';
  {$ENDIF}
{$ENDIF}

  UA_VER = 1.5;

type
  {$IFNDEF FPC}
  DWord = LongWord;
  size_t = NativeUInt;
  {$ENDIF}
  {$IF NOT DECLARED(size_t)}
  size_t = NativeUInt;
  {$IFEND}
  Psize_t = ^size_t;

  UA_Client = record end;
  PUA_Client = ^UA_Client;
  UA_Server = record end;
  PUA_Server = ^UA_Server;
  UA_EventLoop = record end;
  PUA_EventLoop = ^UA_EventLoop;
  UA_SecurityPolicy = record end;
  PUA_SecurityPolicy = ^UA_SecurityPolicy;
  UA_NamespaceMapping = record end;
  PUA_NamespaceMapping = ^UA_NamespaceMapping;

  { --------------- }
  { --- types.h --- }
  { --------------- }

  UA_Boolean = Boolean;  PUA_Boolean = ^UA_Boolean; // C99 bool: 1 byte, true = 1
  UA_SByte = ShortInt;   PUA_SByte = ^UA_SByte;
  UA_Byte = Byte;        PUA_Byte = ^UA_Byte;
  UA_Int16 = SmallInt;   PUA_Int16 = ^UA_Int16;
  UA_UInt16 = Word;      PUA_UInt16 = ^UA_UInt16;
  UA_Int32 = LongInt;    PUA_Int32 = ^UA_Int32;
  UA_UInt32 = DWord;     PUA_UInt32 = ^UA_UInt32;  PPUA_UInt32 = ^PUA_UInt32;
  UA_Int64 = Int64;      PUA_Int64 = ^UA_Int64;
  UA_UInt64 = UInt64;    PUA_UInt64 = ^UA_UInt64;
  UA_Float = Single;     PUA_Float = ^UA_Float;
  UA_Double = Double;    PUA_Double = ^UA_Double;

  (* A numeric identifier for an error or condition that is associated with a
   * value or an operation. *)
  UA_StatusCode = DWord;
  PUA_StatusCode = ^UA_StatusCode;

  (* A sequence of Unicode characters. Strings are just an array of UA_Byte. *)
  UA_String = record
    length: size_t; (* The length of the string *)
    data: PUA_Byte; (* The content (not null-terminated) *)
  end;
  PUA_String = ^UA_String;
  PPUA_String = ^PUA_String;

  (* An instance in time, in 100 nanosecond intervals since January 1, 1601 (UTC). *)
  UA_DateTime = Int64;
  PUA_DateTime = ^UA_DateTime;

  PUA_DateTimeStruct = ^UA_DateTimeStruct;
  UA_DateTimeStruct = record
    nanoSec: UA_UInt16;
    microSec: UA_UInt16;
    milliSec: UA_UInt16;
    sec: UA_UInt16;
    min: UA_UInt16;
    hour: UA_UInt16;
    day: UA_UInt16;   (* From 1 to 31 *)
    month: UA_UInt16; (* From 1 to 12 *)
    year: UA_Int16;
  end;

  UA_Guid = record
    data1: UA_UInt32;
    data2: UA_UInt16;
    data3: UA_UInt16;
    data4: array[0..7] of UA_Byte;
  end;
  PUA_Guid = ^UA_Guid;

  UA_ByteString = UA_String;
  PUA_ByteString = ^UA_ByteString;
  PPUA_ByteString = ^PUA_ByteString;
  UA_XmlElement = UA_String;
  PUA_XmlElement = ^UA_XmlElement;

  UA_NodeIdType = (
    UA_NODEIDTYPE_NUMERIC    = 0,
    UA_NODEIDTYPE_STRING     = 3,
    UA_NODEIDTYPE_GUID       = 4,
    UA_NODEIDTYPE_BYTESTRING = 5
  );

  UA_NodeId = record
    namespaceIndex: UA_UInt16;
    identifierType: UA_NodeIdType;
    identifier: record
      case Integer of
        0: (numeric: UA_UInt32);
        1: (_string: UA_String);
        2: (guid: UA_Guid);
        3: (byteString: UA_ByteString);
    end;
  end;
  PUA_NodeId = ^UA_NodeId;

  UA_ExpandedNodeId = record
    nodeId: UA_NodeId;
    namespaceUri: UA_String;
    serverIndex: UA_UInt32;
  end;
  PUA_ExpandedNodeId = ^UA_ExpandedNodeId;

  UA_QualifiedName = record
    namespaceIndex: UA_UInt16;
    name: UA_String;
  end;
  PUA_QualifiedName = ^UA_QualifiedName;

  UA_LocalizedText = record
    locale: UA_String;
    text: UA_String;
  end;
  PUA_LocalizedText = ^UA_LocalizedText;

  UA_NumericRangeDimension = record
    min: UA_UInt32;
    max: UA_UInt32;
  end;
  PUA_NumericRangeDimension = ^UA_NumericRangeDimension;

  UA_NumericRange = record
    dimensionsSize: size_t;
    dimensions: PUA_NumericRangeDimension;
  end;
  PUA_NumericRange = ^UA_NumericRange;

  PUA_DataType = ^UA_DataType;

  UA_VariantStorageType = (
    UA_VARIANT_DATA,          (* The data has the same lifecycle as the variant *)
    UA_VARIANT_DATA_NODELETE  (* The data is "borrowed" by the variant *)
  );

  UA_Variant = record
    _type: PUA_DataType;          (* The data type description *)
    storageType: UA_VariantStorageType;
    arrayLength: size_t;          (* The number of elements in the data array *)
    data: Pointer;                (* Points to the scalar or array data *)
    arrayDimensionsSize: size_t;  (* The number of dimensions *)
    arrayDimensions: PUA_UInt32;  (* The length of each dimension *)
  end;
  PUA_Variant = ^UA_Variant;
  PPUA_Variant = ^PUA_Variant;

  UA_ExtensionObjectEncoding = (
    UA_EXTENSIONOBJECT_ENCODED_NOBODY     = 0,
    UA_EXTENSIONOBJECT_ENCODED_BYTESTRING = 1,
    UA_EXTENSIONOBJECT_ENCODED_XML        = 2,
    UA_EXTENSIONOBJECT_DECODED            = 3,
    UA_EXTENSIONOBJECT_DECODED_NODELETE   = 4
  );

  UA_ExtensionObject = record
    encoding: UA_ExtensionObjectEncoding;
    content: record
      case Integer of
        0: (encoded: record
              typeId: UA_NodeId;     (* The nodeid of the datatype *)
              body: UA_ByteString;   (* The bytestring of the encoded data *)
            end);
        1: (decoded: record
              _type: PUA_DataType;
              data: Pointer;
            end);
    end;
  end;
  PUA_ExtensionObject = ^UA_ExtensionObject;

  (* A data value with an associated status code and timestamps.
   * The has* bit fields of the C struct are in "flags" (UA_DATAVALUE_HASxxx constants). *)
  UA_DataValue = record
    value: UA_Variant;
    sourceTimestamp: UA_DateTime;
    serverTimestamp: UA_DateTime;
    sourcePicoseconds: UA_UInt16;
    serverPicoseconds: UA_UInt16;
    status: UA_StatusCode;
    flags: UA_Byte;
  end;
  PUA_DataValue = ^UA_DataValue;

  (* The has* bit fields of the C struct are in "flags" (UA_DIAGNOSTICINFO_HASxxx constants). *)
  PUA_DiagnosticInfo = ^UA_DiagnosticInfo;
  UA_DiagnosticInfo = record
    flags: UA_Byte;
    symbolicId: UA_Int32;
    namespaceUri: UA_Int32;
    localizedText: UA_Int32;
    locale: UA_Int32;
    additionalInfo: UA_String;
    innerStatusCode: UA_StatusCode;
    innerDiagnosticInfo: PUA_DiagnosticInfo;
  end;

  (* Describes the fields of structures/unions, or the values of enums.
   * "bits" holds padding:6, isArray:1, isOptional:1 (see the UA_DataTypeMember record). *)
  UA_DataTypeMember = record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    memberName: PAnsiChar;
    {$ENDIF}
    memberType: PUA_DataType;
    bits: UA_Byte;
  end;
  PUA_DataTypeMember = ^UA_DataTypeMember;

  UA_DataTypeKind = (
    UA_DATATYPEKIND_BOOLEAN = 0,
    UA_DATATYPEKIND_SBYTE = 1,
    UA_DATATYPEKIND_BYTE = 2,
    UA_DATATYPEKIND_INT16 = 3,
    UA_DATATYPEKIND_UINT16 = 4,
    UA_DATATYPEKIND_INT32 = 5,
    UA_DATATYPEKIND_UINT32 = 6,
    UA_DATATYPEKIND_INT64 = 7,
    UA_DATATYPEKIND_UINT64 = 8,
    UA_DATATYPEKIND_FLOAT = 9,
    UA_DATATYPEKIND_DOUBLE = 10,
    UA_DATATYPEKIND_STRING = 11,
    UA_DATATYPEKIND_DATETIME = 12,
    UA_DATATYPEKIND_GUID = 13,
    UA_DATATYPEKIND_BYTESTRING = 14,
    UA_DATATYPEKIND_XMLELEMENT = 15,
    UA_DATATYPEKIND_NODEID = 16,
    UA_DATATYPEKIND_EXPANDEDNODEID = 17,
    UA_DATATYPEKIND_STATUSCODE = 18,
    UA_DATATYPEKIND_QUALIFIEDNAME = 19,
    UA_DATATYPEKIND_LOCALIZEDTEXT = 20,
    UA_DATATYPEKIND_EXTENSIONOBJECT = 21,
    UA_DATATYPEKIND_DATAVALUE = 22,
    UA_DATATYPEKIND_VARIANT = 23,
    UA_DATATYPEKIND_DIAGNOSTICINFO = 24,
    UA_DATATYPEKIND_DECIMAL = 25,
    UA_DATATYPEKIND_ENUM = 26,
    UA_DATATYPEKIND_STRUCTURE = 27,
    UA_DATATYPEKIND_OPTSTRUCT = 28,
    UA_DATATYPEKIND_UNION = 29,
    UA_DATATYPEKIND_BITFIELDCLUSTER = 30
  );

  (* "bits" holds memSize:16, typeKind:6, pointerFree:1, overlayable:1,
   * membersSize:8 (see UA_DataType_memSize & co.) *)
  UA_DataType = record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    typeName: PAnsiChar;
    {$ENDIF}
    typeId: UA_NodeId;           (* The nodeid of the type *)
    binaryEncodingId: UA_NodeId; (* NodeId of datatype when encoded as binary *)
    xmlEncodingId: UA_NodeId;    (* NodeId of datatype when encoded as XML *)
    bits: UA_UInt32;
    members: PUA_DataTypeMember;
  end;

  PUA_DataTypeArray = ^UA_DataTypeArray;
  UA_DataTypeArray = record
    next: PUA_DataTypeArray;
    typesSize: size_t;
    types: PUA_DataType;
    cleanup: UA_Boolean;
  end;

  { ------------------------- }
  { --- types_generated.h --- }
  { ------------------------- }
  {$I types_generated.inc}

type
  { ---------------- }
  { --- common.h --- }
  { ---------------- }
  UA_AttributeId = (
    UA_ATTRIBUTEID_INVALID                 = 0,
    UA_ATTRIBUTEID_NODEID                  = 1,
    UA_ATTRIBUTEID_NODECLASS               = 2,
    UA_ATTRIBUTEID_BROWSENAME              = 3,
    UA_ATTRIBUTEID_DISPLAYNAME             = 4,
    UA_ATTRIBUTEID_DESCRIPTION             = 5,
    UA_ATTRIBUTEID_WRITEMASK               = 6,
    UA_ATTRIBUTEID_USERWRITEMASK           = 7,
    UA_ATTRIBUTEID_ISABSTRACT              = 8,
    UA_ATTRIBUTEID_SYMMETRIC               = 9,
    UA_ATTRIBUTEID_INVERSENAME             = 10,
    UA_ATTRIBUTEID_CONTAINSNOLOOPS         = 11,
    UA_ATTRIBUTEID_EVENTNOTIFIER           = 12,
    UA_ATTRIBUTEID_VALUE                   = 13,
    UA_ATTRIBUTEID_DATATYPE                = 14,
    UA_ATTRIBUTEID_VALUERANK               = 15,
    UA_ATTRIBUTEID_ARRAYDIMENSIONS         = 16,
    UA_ATTRIBUTEID_ACCESSLEVEL             = 17,
    UA_ATTRIBUTEID_USERACCESSLEVEL         = 18,
    UA_ATTRIBUTEID_MINIMUMSAMPLINGINTERVAL = 19,
    UA_ATTRIBUTEID_HISTORIZING             = 20,
    UA_ATTRIBUTEID_EXECUTABLE              = 21,
    UA_ATTRIBUTEID_USEREXECUTABLE          = 22,
    UA_ATTRIBUTEID_DATATYPEDEFINITION      = 23,
    UA_ATTRIBUTEID_ROLEPERMISSIONS         = 24,
    UA_ATTRIBUTEID_USERROLEPERMISSIONS     = 25,
    UA_ATTRIBUTEID_ACCESSRESTRICTIONS      = 26,
    UA_ATTRIBUTEID_ACCESSLEVELEX           = 27
  );

  UA_RuleHandling = (
    UA_RULEHANDLING_DEFAULT = 0,
    UA_RULEHANDLING_ABORT,
    UA_RULEHANDLING_WARN,
    UA_RULEHANDLING_ACCEPT
  );

  UA_ApplicationNotificationType = (
    UA_APPLICATIONNOTIFICATIONTYPE_LIFECYCLE_STARTED,
    UA_APPLICATIONNOTIFICATIONTYPE_LIFECYCLE_SHUTDOWN,
    UA_APPLICATIONNOTIFICATIONTYPE_LIFECYCLE_STOPPING,
    UA_APPLICATIONNOTIFICATIONTYPE_LIFECYCLE_STOPPED,
    UA_APPLICATIONNOTIFICATIONTYPE_SECURECHANNEL_OPENED,
    UA_APPLICATIONNOTIFICATIONTYPE_SECURECHANNEL_CLOSED,
    UA_APPLICATIONNOTIFICATIONTYPE_SESSION_CREATED,
    UA_APPLICATIONNOTIFICATIONTYPE_SESSION_ACTIVATED,
    UA_APPLICATIONNOTIFICATIONTYPE_SESSION_DEACTIVATED,
    UA_APPLICATIONNOTIFICATIONTYPE_SESSION_CLOSED,
    UA_APPLICATIONNOTIFICATIONTYPE_SERVICE_BEGIN,
    UA_APPLICATIONNOTIFICATIONTYPE_SERVICE_ASYNC,
    UA_APPLICATIONNOTIFICATIONTYPE_SERVICE_END,
    UA_APPLICATIONNOTIFICATIONTYPE_SUBSCRIPTION_CREATED,
    UA_APPLICATIONNOTIFICATIONTYPE_SUBSCRIPTION_MODIFIED,
    UA_APPLICATIONNOTIFICATIONTYPE_SUBSCRIPTION_PUBLISHINGMODE,
    UA_APPLICATIONNOTIFICATIONTYPE_SUBSCRIPTION_TRANSFERRED,
    UA_APPLICATIONNOTIFICATIONTYPE_SUBSCRIPTION_DELETED,
    UA_APPLICATIONNOTIFICATIONTYPE_MONITOREDITEM_CREATED,
    UA_APPLICATIONNOTIFICATIONTYPE_MONITOREDITEM_MODIFIED,
    UA_APPLICATIONNOTIFICATIONTYPE_MONITOREDITEM_MONITORINGMODE,
    UA_APPLICATIONNOTIFICATIONTYPE_MONITOREDITEM_DELETE
  );

  UA_SecureChannelState = (
    UA_SECURECHANNELSTATE_CLOSED = 0,
    UA_SECURECHANNELSTATE_REVERSE_LISTENING,
    UA_SECURECHANNELSTATE_CONNECTING,
    UA_SECURECHANNELSTATE_CONNECTED,
    UA_SECURECHANNELSTATE_REVERSE_CONNECTED,
    UA_SECURECHANNELSTATE_RHE_SENT,
    UA_SECURECHANNELSTATE_HEL_SENT,
    UA_SECURECHANNELSTATE_HEL_RECEIVED,
    UA_SECURECHANNELSTATE_ACK_SENT,
    UA_SECURECHANNELSTATE_ACK_RECEIVED,
    UA_SECURECHANNELSTATE_OPN_SENT,
    UA_SECURECHANNELSTATE_OPEN,
    UA_SECURECHANNELSTATE_CLOSING
  );
  PUA_SecureChannelState = ^UA_SecureChannelState;

  UA_SessionState = (
    UA_SESSIONSTATE_CLOSED = 0,
    UA_SESSIONSTATE_CREATE_REQUESTED,
    UA_SESSIONSTATE_CREATED,
    UA_SESSIONSTATE_ACTIVATE_REQUESTED,
    UA_SESSIONSTATE_ACTIVATED,
    UA_SESSIONSTATE_CLOSING
  );
  PUA_SessionState = ^UA_SessionState;

  UA_LifecycleState = (
    UA_LIFECYCLESTATE_STOPPED = 0,
    UA_LIFECYCLESTATE_STARTED,
    UA_LIFECYCLESTATE_STOPPING
  );

  { -------------- }
  { --- util.h --- }
  { -------------- }
  UA_KeyValueMap = record
    mapSize: size_t;
    map: PUA_KeyValuePair;
  end;
  PUA_KeyValueMap = ^UA_KeyValueMap;

  PUA_ConnectionConfig = ^UA_ConnectionConfig;
  UA_ConnectionConfig = record
    protocolVersion: UA_UInt32;
    recvBufferSize: UA_UInt32;
    sendBufferSize: UA_UInt32;
    localMaxMessageSize: UA_UInt32;  (* (0 = unbounded) *)
    remoteMaxMessageSize: UA_UInt32; (* (0 = unbounded) *)
    localMaxChunkCount: UA_UInt32;   (* (0 = unbounded) *)
    remoteMaxChunkCount: UA_UInt32;  (* (0 = unbounded) *)
  end;

  { -------------------- }
  { --- plugin/log.h --- }
  { -------------------- }
  UA_LogLevel = (
    UA_LOGLEVEL_TRACE   = 100,
    UA_LOGLEVEL_DEBUG   = 200,
    UA_LOGLEVEL_INFO    = 300,
    UA_LOGLEVEL_WARNING = 400,
    UA_LOGLEVEL_ERROR   = 500,
    UA_LOGLEVEL_FATAL   = 600
  );

  UA_LogCategory = (
    UA_LOGCATEGORY_NETWORK = 0,
    UA_LOGCATEGORY_SECURECHANNEL = 1,
    UA_LOGCATEGORY_SESSION = 2,
    UA_LOGCATEGORY_SERVER = 3,
    UA_LOGCATEGORY_CLIENT = 4,
    UA_LOGCATEGORY_APPLICATION = 5,
    UA_LOGCATEGORY_SECURITY = 6,
    UA_LOGCATEGORY_EVENTLOOP = 7,
    UA_LOGCATEGORY_PUBSUB = 8,
    UA_LOGCATEGORY_DISCOVERY = 9
  );

  PUA_Logger = ^UA_Logger;
  (* "args" is the C va_list: it is only forwarded, e.g. to UA_FormatLogMessage. *)
  UA_Logger = record
    log: procedure(logContext: Pointer; level: UA_LogLevel; category: UA_LogCategory; msg: PAnsiChar; args: Pointer); cdecl;
    context: Pointer;
    clear: procedure(logger: PUA_Logger); cdecl;
  end;

  { ---------------------------------- }
  { --- plugin/certificategroup.h --- }
  { ---------------------------------- }
  PUA_CertificateGroup = ^UA_CertificateGroup;
  UA_CertificateGroup = record
    certificateGroupId: UA_NodeId;
    context: Pointer;
    logging: PUA_Logger;
    getTrustList: function(certGroup: PUA_CertificateGroup; trustList: PUA_TrustListDataType): UA_StatusCode; cdecl;
    setTrustList: function(certGroup: PUA_CertificateGroup; const trustList: PUA_TrustListDataType): UA_StatusCode; cdecl;
    addToTrustList: function(certGroup: PUA_CertificateGroup; const trustList: PUA_TrustListDataType): UA_StatusCode; cdecl;
    removeFromTrustList: function(certGroup: PUA_CertificateGroup; const trustList: PUA_TrustListDataType): UA_StatusCode; cdecl;
    getRejectedList: function(certGroup: PUA_CertificateGroup; rejectedList: PPUA_ByteString; rejectedListSize: Psize_t): UA_StatusCode; cdecl;
    getCertificateCrls: function(certGroup: PUA_CertificateGroup; const certificate: PUA_ByteString; const isTrusted: UA_Boolean; crls: PPUA_ByteString; crlsSize: Psize_t): UA_StatusCode; cdecl;
    verifyCertificate: function(certGroup: PUA_CertificateGroup; const certificate: PUA_ByteString): UA_StatusCode; cdecl;
    clear: procedure(certGroup: PUA_CertificateGroup); cdecl;
  end;

  { ---------------- }
  { --- client.h --- }
  { ---------------- }
  PPUA_EndpointDescription = ^PUA_EndpointDescription;
  UA_ClientNotificationCallback = procedure(client: PUA_Client; _type: UA_ApplicationNotificationType; payload: UA_KeyValueMap); cdecl;
  UA_ClientCallback = procedure(client: PUA_Client; data: Pointer); cdecl;

  PUA_ClientConfig_PolicyHistory = ^UA_ClientConfig_PolicyHistory;
  UA_ClientConfig_PolicyHistory = record
    next: PUA_ClientConfig_PolicyHistory;
    policies: PUA_SecurityPolicy;
    policiesSize: size_t;
  end;

  PUA_ClientConfig = ^UA_ClientConfig;
  UA_ClientConfig = record
    clientContext: Pointer;     (* User-defined pointer attached to the client *)
    logging: PUA_Logger;        (* Plugin for log output *)
    timeout: UA_UInt32;         (* Response timeout in ms *)
    clientDescription: UA_ApplicationDescription;
    endpointUrl: UA_String;
    userIdentityToken: UA_ExtensionObject;
    sessionName: UA_String;
    sessionLocaleIds: PUA_LocaleId;
    sessionLocaleIdsSize: size_t;
    noSession: UA_Boolean;
    noReconnect: UA_Boolean;
    noNewSession: UA_Boolean;
    secureChannelLifeTime: UA_UInt32;
    requestedSessionTimeout: UA_UInt32;
    localConnectionConfig: UA_ConnectionConfig;
    connectivityCheckInterval: UA_UInt32;
    tcpReuseAddr: UA_Boolean;
    endpoint: UA_EndpointDescription;
    userTokenPolicy: UA_UserTokenPolicy;
    applicationUri: UA_String;
    securityMode: UA_MessageSecurityMode;
    securityPolicyUri: UA_String;
    authSecurityPolicyUri: UA_String;
    securityPoliciesSize: size_t;
    securityPolicies: PUA_SecurityPolicy;
    certificateVerification: UA_CertificateGroup;
    authSecurityPoliciesSize: size_t;
    authSecurityPolicies: PUA_SecurityPolicy;
    securityPolicyHistory: PUA_ClientConfig_PolicyHistory;
    allowNonePolicyPassword: UA_Boolean;
    {$IFDEF UA_ENABLE_ENCRYPTION}
    maxTrustListSize: UA_UInt32;
    maxRejectedListSize: UA_UInt32;
    privateKeyPasswordCallback: function(cc: PUA_ClientConfig; password: PUA_ByteString): UA_StatusCode; cdecl;
    {$ENDIF}
    customDataTypes: PUA_DataTypeArray;
    namespaces: PUA_String;
    namespacesSize: size_t;
    globalNotificationCallback: UA_ClientNotificationCallback;
    lifecycleNotificationCallback: UA_ClientNotificationCallback;
    serviceNotificationCallback: UA_ClientNotificationCallback;
    stateCallback: procedure(client: PUA_Client; channelState: UA_SecureChannelState; sessionState: UA_SessionState; connectStatus: UA_StatusCode); cdecl;
    inactivityCallback: procedure(client: PUA_Client); cdecl;
    subscriptionInactivityCallback: procedure(client: PUA_Client; subscriptionId: UA_UInt32; subContext: Pointer); cdecl;
    outStandingPublishRequests: UA_UInt16;
    eventLoop: PUA_EventLoop;
    externalEventLoop: UA_Boolean;
  end;

  { ------------------------------ }
  { --- client_subscriptions.h --- }
  { ------------------------------ }
  UA_Client_DeleteSubscriptionCallback = procedure(client: PUA_Client; subId: UA_UInt32; subContext: Pointer); cdecl;
  UA_Client_StatusChangeNotificationCallback = procedure(client: PUA_Client; subId: UA_UInt32; subContext: Pointer; notification: PUA_StatusChangeNotification); cdecl;
  UA_Client_DeleteMonitoredItemCallback = procedure(client: PUA_Client; subId: UA_UInt32; subContext: Pointer; monId: UA_UInt32; monContext: Pointer); cdecl;
  UA_Client_DataChangeNotificationCallback = procedure(client: PUA_Client; subId: UA_UInt32; subContext: Pointer; monId: UA_UInt32; monContext: Pointer; value: PUA_DataValue); cdecl;
  PUA_Client_DeleteMonitoredItemCallback = ^UA_Client_DeleteMonitoredItemCallback;
  PUA_Client_DataChangeNotificationCallback = ^UA_Client_DataChangeNotificationCallback;

  { ------------------------------------- }
  { --- plugin/create_certificate.h --- }
  { ------------------------------------- }
  UA_CertificateFormat = (
    UA_CERTIFICATEFORMAT_DER,
    UA_CERTIFICATEFORMAT_PEM
  );

  {$IFDEF ENABLE_SERVER}
  { ---------------- }
  { --- server.h --- }
  { ---------------- }
  (* Only the first fields of UA_ServerConfig are declared: use it through
   * PUA_ServerConfig only (UA_Server_getConfig). *)
  UA_ServerConfig = record
    context: Pointer;
    logging: PUA_Logger;
  end;
  PUA_ServerConfig = ^UA_ServerConfig;

  UA_UsernamePasswordLogin = record
    username: UA_String;
    password: UA_ByteString;
  end;
  PUA_UsernamePasswordLogin = ^UA_UsernamePasswordLogin;

  (* Login of a session (UA_AccessControl_defaultWithLoginCallback): userName is
   * empty for an anonymous login; the password comes decrypted. What is put in
   * sessionContext reaches the callbacks of that session (value sources...); the
   * default access control frees nothing of it. Not GOOD refuses the login. *)
  UA_UsernamePasswordLoginCallback = function(const userName: PUA_String; const password: PUA_ByteString;
                                              usernamePasswordLoginSize: size_t;
                                              const usernamePasswordLogin: PUA_UsernamePasswordLogin;
                                              sessionContext: PPointer; loginContext: Pointer): UA_StatusCode; cdecl;

  (* Value source of a variable whose value lives in the application: the
   * server asks read for every read (and for the sampling of the monitored
   * items) and hands every write of a client to write (nil = not writable).
   * Both may return UA_STATUSCODE_GOODCOMPLETESASYNCHRONOUSLY and give the
   * result later with UA_Server_setAsyncReadResult/UA_Server_setAsyncWriteResult
   * (the pointer given to the callback identifies the operation).
   * The status for the client goes in value^.status (read) or in the
   * returned code (write). *)
  UA_CallbackValueSource_read = function(server: PUA_Server; const sessionId: PUA_NodeId;
                                         sessionContext: Pointer; const nodeId: PUA_NodeId;
                                         nodeContext: Pointer; includeSourceTimeStamp: UA_Boolean;
                                         const range: PUA_NumericRange; value: PUA_DataValue): UA_StatusCode; cdecl;
  UA_CallbackValueSource_write = function(server: PUA_Server; const sessionId: PUA_NodeId;
                                          sessionContext: Pointer; const nodeId: PUA_NodeId;
                                          nodeContext: Pointer; const range: PUA_NumericRange;
                                          const value: PUA_DataValue): UA_StatusCode; cdecl;
  UA_CallbackValueSource = record
    read: UA_CallbackValueSource_read;
    write: UA_CallbackValueSource_write;
  end;
  PUA_CallbackValueSource = ^UA_CallbackValueSource;
  (* name of open62541 before 1.5 *)
  UA_DataSource = UA_CallbackValueSource;

  UA_MethodCallback = function(server: PUA_Server;
                               const sessionId: PUA_NodeId; sessionContext: Pointer;
                               const methodId: PUA_NodeId; methodContext: Pointer;
                               const objectId: PUA_NodeId; objectContext: Pointer;
                               inputSize: size_t; const input: PUA_Variant;
                               outputSize: size_t; output: PUA_Variant): UA_StatusCode; cdecl;
  {$ENDIF}

const
  UA_EMPTY_ARRAY_SENTINEL = Pointer($01);

  UA_DATETIME_USEC = 10;
  UA_DATETIME_MSEC = UA_DATETIME_USEC * 1000;
  UA_DATETIME_SEC  = UA_DATETIME_MSEC * 1000;
  UA_DATETIME_UNIX_EPOCH = Int64(11644473600) * UA_DATETIME_SEC;

  (* Access level masks *)
  UA_ACCESSLEVELMASK_READ           = $01;
  UA_ACCESSLEVELMASK_CURRENTREAD    = $01;
  UA_ACCESSLEVELMASK_WRITE          = $02;
  UA_ACCESSLEVELMASK_CURRENTWRITE   = $02;
  UA_ACCESSLEVELMASK_HISTORYREAD    = $04;
  UA_ACCESSLEVELMASK_HISTORYWRITE   = $08;
  UA_ACCESSLEVELMASK_SEMANTICCHANGE = $10;
  UA_ACCESSLEVELMASK_STATUSWRITE    = $20;
  UA_ACCESSLEVELMASK_TIMESTAMPWRITE = $40;

  (* Value ranks *)
  UA_VALUERANK_SCALAR_OR_ONE_DIMENSION = -3;
  UA_VALUERANK_ANY                     = -2;
  UA_VALUERANK_SCALAR                  = -1;
  UA_VALUERANK_ONE_OR_MORE_DIMENSIONS  = 0;
  UA_VALUERANK_ONE_DIMENSION           = 1;
  UA_VALUERANK_TWO_DIMENSIONS          = 2;
  UA_VALUERANK_THREE_DIMENSIONS        = 3;

  (* Event notifier masks *)
  UA_EVENTNOTIFIER_SUBSCRIBE_TO_EVENT = $01;
  UA_EVENTNOTIFIER_HISTORY_READ       = $04;
  UA_EVENTNOTIFIER_HISTORY_WRITE      = $08;

  (* UA_DataValue.flags *)
  UA_DATAVALUE_HASVALUE             = $01;
  UA_DATAVALUE_HASSTATUS            = $02;
  UA_DATAVALUE_HASSOURCETIMESTAMP   = $04;
  UA_DATAVALUE_HASSERVERTIMESTAMP   = $08;
  UA_DATAVALUE_HASSOURCEPICOSECONDS = $10;
  UA_DATAVALUE_HASSERVERPICOSECONDS = $20;

  (* UA_DiagnosticInfo.flags *)
  UA_DIAGNOSTICINFO_HASSYMBOLICID          = $01;
  UA_DIAGNOSTICINFO_HASNAMESPACEURI        = $02;
  UA_DIAGNOSTICINFO_HASLOCALIZEDTEXT       = $04;
  UA_DIAGNOSTICINFO_HASLOCALE              = $08;
  UA_DIAGNOSTICINFO_HASADDITIONALINFO      = $10;
  UA_DIAGNOSTICINFO_HASINNERSTATUSCODE     = $20;
  UA_DIAGNOSTICINFO_HASINNERDIAGNOSTICINFO = $40;

  (* Security policy URIs *)
  UA_SECURITY_POLICY_NONE_URI                = 'http://opcfoundation.org/UA/SecurityPolicy#None';
  UA_SECURITY_POLICY_BASIC128RSA15_URI       = 'http://opcfoundation.org/UA/SecurityPolicy#Basic128Rsa15';
  UA_SECURITY_POLICY_BASIC256_URI            = 'http://opcfoundation.org/UA/SecurityPolicy#Basic256';
  UA_SECURITY_POLICY_BASIC256SHA256_URI      = 'http://opcfoundation.org/UA/SecurityPolicy#Basic256Sha256';
  UA_SECURITY_POLICY_AES128SHA256RSAOAEP_URI = 'http://opcfoundation.org/UA/SecurityPolicy#Aes128_Sha256_RsaOaep';
  UA_SECURITY_POLICY_AES256SHA256RSAPSS_URI  = 'http://opcfoundation.org/UA/SecurityPolicy#Aes256_Sha256_RsaPss';

  { ------------------- }
  { --- statuscodes --- }
  { ------------------- }
  {$I statuscodes.inc}

  { --------------- }
  { --- nodeids --- }
  { --------------- }
  {$I nodeids.inc}

var
  (* The standard-defined types (an array of UA_TYPES_COUNT entries in the library) *)
  UA_TYPES: PUA_DataType;
  UA_STRING_NULL: UA_String;
  UA_NODEID_NULL: UA_NodeId;
  UA_EXPANDEDNODEID_NULL: UA_ExpandedNodeId;
  UA_VariableAttributes_default: UA_VariableAttributes;
  UA_ObjectAttributes_default: UA_ObjectAttributes;
  UA_MethodAttributes_default: UA_MethodAttributes;
  UA_ObjectTypeAttributes_default: UA_ObjectTypeAttributes;
  UA_ReferenceTypeAttributes_default: UA_ReferenceTypeAttributes;
  UA_DataTypeAttributes_default: UA_DataTypeAttributes;

  { ---------------- }
  { --- types.h --- }
  { ---------------- }
  UA_StatusCode_name: function(code: UA_StatusCode): PAnsiChar; cdecl;
  UA_String_fromChars: function(const src: PAnsiChar): UA_String; cdecl;
  UA_String_isEmpty: function(const s: PUA_String): UA_Boolean; cdecl;
  UA_ByteString_fromBase64: function(bs: PUA_ByteString; const input: PUA_String): UA_StatusCode; cdecl;
  UA_ByteString_toBase64: function(const bs: PUA_ByteString; output: PUA_String): UA_StatusCode; cdecl;
  UA_DateTime_now: function(): UA_DateTime; cdecl;
  UA_DateTime_toStruct: function(t: UA_DateTime): UA_DateTimeStruct; cdecl;
  UA_DateTime_fromStruct: function(ts: UA_DateTimeStruct): UA_DateTime; cdecl;
  UA_NodeId_isNull: function(const p: PUA_NodeId): UA_Boolean; cdecl;
  UA_NodeId_print: function(const id: PUA_NodeId; output: PUA_String): UA_StatusCode; cdecl;
  UA_NodeId_parse: function(id: PUA_NodeId; str: UA_String): UA_StatusCode; cdecl;
  (* printf of the library: besides the C formats, %S is a UA_String* and %N a UA_NodeId*.
     With str^.length > 0 it writes into str^.data (truncated, BadEncodingLimitsExceeded if
     it does not fit), with length 0 it allocates (UA_String_clear afterwards). *)
  UA_String_vformat: function(str: PUA_String; format: PAnsiChar; args: Pointer): UA_StatusCode; cdecl;
  UA_ExpandedNodeId_print: function(const id: PUA_ExpandedNodeId; output: PUA_String): UA_StatusCode; cdecl;
  UA_ExpandedNodeId_parse: function(id: PUA_ExpandedNodeId; str: UA_String): UA_StatusCode; cdecl;
  UA_NumericRange_parse: function(range: PUA_NumericRange; str: UA_String): UA_StatusCode; cdecl;
  UA_Variant_setScalar: procedure(v: PUA_Variant; p: Pointer; const _type: PUA_DataType); cdecl;
  UA_Variant_setScalarCopy: function(v: PUA_Variant; const p: Pointer; const _type: PUA_DataType): UA_StatusCode; cdecl;
  UA_Variant_setArray: procedure(v: PUA_Variant; arrayData: Pointer; arraySize: size_t; const _type: PUA_DataType); cdecl;
  UA_Variant_setArrayCopy: function(v: PUA_Variant; const arrayData: Pointer; arraySize: size_t; const _type: PUA_DataType): UA_StatusCode; cdecl;
  UA_Variant_setRange: function(v: PUA_Variant; arrayData: Pointer; arraySize: size_t; range: UA_NumericRange): UA_StatusCode; cdecl;
  UA_Variant_setRangeCopy: function(v: PUA_Variant; const arrayData: Pointer; arraySize: size_t; range: UA_NumericRange): UA_StatusCode; cdecl;
  UA_ExtensionObject_setValue: procedure(eo: PUA_ExtensionObject; p: Pointer; const _type: PUA_DataType); cdecl;
  UA_KeyValueMap_get: function(const map: PUA_KeyValueMap; key: UA_QualifiedName): PUA_Variant; cdecl;
  UA_findDataType: function(const typeId: PUA_NodeId): PUA_DataType; cdecl;
  UA_new: function(const _type: PUA_DataType): Pointer; cdecl;
  UA_init: procedure(p: Pointer; const _type: PUA_DataType); cdecl;
  UA_copy: function(const src: Pointer; dst: Pointer; const _type: PUA_DataType): UA_StatusCode; cdecl;
  UA_clear: procedure(p: Pointer; const _type: PUA_DataType); cdecl;
  UA_delete: procedure(p: Pointer; const _type: PUA_DataType); cdecl;
  UA_equal: function(const p1, p2: Pointer; const _type: PUA_DataType): UA_Boolean; cdecl;
  UA_print: function(const p: Pointer; const _type: PUA_DataType; output: PUA_String): UA_StatusCode; cdecl;
  UA_Array_new: function(size: size_t; const _type: PUA_DataType): Pointer; cdecl;
  UA_Array_copy: function(const src: Pointer; size: size_t; dst: PPointer; const _type: PUA_DataType): UA_StatusCode; cdecl;
  UA_Array_delete: procedure(p: Pointer; size: size_t; const _type: PUA_DataType); cdecl;

  { -------------------- }
  { --- plugin/log --- }
  { -------------------- }
  UA_Log_Stdout_new: function(minlevel: UA_LogLevel): PUA_Logger; cdecl;

  { ---------------- }
  { --- client.h --- }
  { ---------------- }
  UA_Client_new: function(): PUA_Client; cdecl;
  UA_Client_newWithConfig: function(const config: PUA_ClientConfig): PUA_Client; cdecl;
  UA_Client_getState: procedure(client: PUA_Client; channelState: PUA_SecureChannelState; sessionState: PUA_SessionState; connectStatus: PUA_StatusCode); cdecl;
  UA_Client_getConfig: function(client: PUA_Client): PUA_ClientConfig; cdecl;
  UA_Client_delete: procedure(client: PUA_Client); cdecl;
  UA_Client_run_iterate: function(client: PUA_Client; timeout: UA_UInt32): UA_StatusCode; cdecl;
  UA_Client_connect: function(client: PUA_Client; const endpointUrl: PAnsiChar): UA_StatusCode; cdecl;
  UA_Client_connectSecureChannel: function(client: PUA_Client; const endpointUrl: PAnsiChar): UA_StatusCode; cdecl;
  UA_Client_connectAsync: function(client: PUA_Client; const endpointUrl: PAnsiChar): UA_StatusCode; cdecl;
  UA_Client_disconnect: function(client: PUA_Client): UA_StatusCode; cdecl;
  UA_Client_disconnectSecureChannel: function(client: PUA_Client): UA_StatusCode; cdecl;
  UA_Client_getEndpoints: function(client: PUA_Client; const serverUrl: PAnsiChar; endpointDescriptionsSize: Psize_t; endpointDescriptions: PPUA_EndpointDescription): UA_StatusCode; cdecl;
  UA_Client_getNamespaceIndex: function(client: PUA_Client; nsUri: UA_String; outIndex: PUA_UInt16): UA_StatusCode; cdecl;
  __UA_Client_Service: procedure(client: PUA_Client; const request: Pointer; const requestType: PUA_DataType; response: Pointer; const responseType: PUA_DataType); cdecl;
  UA_Client_Service_read: function(client: PUA_Client; request: UA_ReadRequest): UA_ReadResponse; cdecl;
  UA_Client_Service_write: function(client: PUA_Client; request: UA_WriteRequest): UA_WriteResponse; cdecl;
  UA_Client_Service_browse: function(client: PUA_Client; request: UA_BrowseRequest): UA_BrowseResponse; cdecl;
  UA_Client_Service_browseNext: function(client: PUA_Client; request: UA_BrowseNextRequest): UA_BrowseNextResponse; cdecl;
  UA_Client_Service_translateBrowsePathsToNodeIds: function(client: PUA_Client; request: UA_TranslateBrowsePathsToNodeIdsRequest): UA_TranslateBrowsePathsToNodeIdsResponse; cdecl;
  UA_Client_Service_call: function(client: PUA_Client; request: UA_CallRequest): UA_CallResponse; cdecl;

  { ---------------------------------- }
  { --- client_config_default.h --- }
  { ---------------------------------- }
  UA_ClientConfig_setDefault: function(config: PUA_ClientConfig): UA_StatusCode; cdecl;
  UA_ClientConfig_clear: procedure(config: PUA_ClientConfig); cdecl;
  UA_ClientConfig_setAuthenticationUsername: function(config: PUA_ClientConfig; const username, password: PAnsiChar): UA_StatusCode; cdecl;
  {$IFDEF UA_ENABLE_ENCRYPTION}
  UA_ClientConfig_setDefaultEncryption: function(config: PUA_ClientConfig; localCertificate, privateKey: UA_ByteString;
                                                 const trustList: PUA_ByteString; trustListSize: size_t;
                                                 const revocationList: PUA_ByteString; revocationListSize: size_t): UA_StatusCode; cdecl;
  UA_ClientConfig_setAuthenticationCert: function(config: PUA_ClientConfig; certificateAuth, privateKeyAuth: UA_ByteString): UA_StatusCode; cdecl;
  UA_CreateCertificate: function(const logger: PUA_Logger; const subject: PUA_String; subjectSize: size_t;
                                 const subjectAltName: PUA_String; subjectAltNameSize: size_t;
                                 certFormat: UA_CertificateFormat; params: PUA_KeyValueMap;
                                 outPrivateKey, outCertificate: PUA_ByteString): UA_StatusCode; cdecl;
  UA_CertificateUtils_getExpirationDate: function(certificate: PUA_ByteString; expiryDateTime: PUA_DateTime): UA_StatusCode; cdecl;
  UA_CertificateUtils_getSubjectName: function(certificate: PUA_ByteString; subjectName: PUA_String): UA_StatusCode; cdecl;
  {$ENDIF}
  UA_CertificateGroup_AcceptAll: procedure(certGroup: PUA_CertificateGroup); cdecl;

  { -------------------------- }
  { --- client_highlevel.h --- }
  { -------------------------- }
  UA_Client_readValueAttribute: function(client: PUA_Client; nodeId: UA_NodeId; outValue: PUA_Variant): UA_StatusCode; cdecl;
  UA_Client_writeValueAttribute: function(client: PUA_Client; nodeId: UA_NodeId; const newValue: PUA_Variant): UA_StatusCode; cdecl;
  UA_Client_readArrayDimensionsAttribute: function(client: PUA_Client; nodeId: UA_NodeId; outArrayDimensionsSize: Psize_t; outArrayDimensions: PPUA_UInt32): UA_StatusCode; cdecl;
  UA_Client_writeArrayDimensionsAttribute: function(client: PUA_Client; nodeId: UA_NodeId; newArrayDimensionsSize: size_t; const newArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl;
  {$IFDEF UA_ENABLE_METHODCALLS}
  UA_Client_call: function(client: PUA_Client; objectId, methodId: UA_NodeId; inputSize: size_t; const input: PUA_Variant; outputSize: Psize_t; output: PPUA_Variant): UA_StatusCode; cdecl;
  {$ENDIF}

  {$IFDEF UA_ENABLE_SUBSCRIPTIONS}
  { ------------------------------ }
  { --- client_subscriptions.h --- }
  { ------------------------------ }
  UA_Client_Subscriptions_create: function(client: PUA_Client; request: UA_CreateSubscriptionRequest; subscriptionContext: Pointer;
                                           statusChangeCallback: UA_Client_StatusChangeNotificationCallback;
                                           deleteCallback: UA_Client_DeleteSubscriptionCallback): UA_CreateSubscriptionResponse; cdecl;
  UA_Client_Subscriptions_modify: function(client: PUA_Client; request: UA_ModifySubscriptionRequest): UA_ModifySubscriptionResponse; cdecl;
  UA_Client_Subscriptions_delete: function(client: PUA_Client; request: UA_DeleteSubscriptionsRequest): UA_DeleteSubscriptionsResponse; cdecl;
  UA_Client_Subscriptions_deleteSingle: function(client: PUA_Client; subscriptionId: UA_UInt32): UA_StatusCode; cdecl;
  UA_Client_MonitoredItems_createDataChanges: function(client: PUA_Client; request: UA_CreateMonitoredItemsRequest; contexts: PPointer;
                                                       callbacks: PUA_Client_DataChangeNotificationCallback;
                                                       deleteCallbacks: PUA_Client_DeleteMonitoredItemCallback): UA_CreateMonitoredItemsResponse; cdecl;
  UA_Client_MonitoredItems_createDataChange: function(client: PUA_Client; subscriptionId: UA_UInt32; timestampsToReturn: UA_TimestampsToReturn;
                                                      item: UA_MonitoredItemCreateRequest; context: Pointer;
                                                      callback: UA_Client_DataChangeNotificationCallback;
                                                      deleteCallback: UA_Client_DeleteMonitoredItemCallback): UA_MonitoredItemCreateResult; cdecl;
  UA_Client_MonitoredItems_delete: function(client: PUA_Client; request: UA_DeleteMonitoredItemsRequest): UA_DeleteMonitoredItemsResponse; cdecl;
  UA_Client_MonitoredItems_deleteSingle: function(client: PUA_Client; subscriptionId, monitoredItemId: UA_UInt32): UA_StatusCode; cdecl;
  {$ENDIF}

  {$IFDEF ENABLE_SERVER}
  { ------------------------------- }
  { --- server_config_default.h --- }
  { ------------------------------- }
  UA_Server_new: function(): PUA_Server; cdecl;
  UA_ServerConfig_setMinimalCustomBuffer: function(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString; sendBufferSize, recvBufferSize: UA_UInt32): UA_StatusCode; cdecl;
  {$IFDEF UA_ENABLE_ENCRYPTION}
  UA_ServerConfig_setDefaultWithSecurityPolicies: function(conf: PUA_ServerConfig; portNumber: UA_UInt16;
                                                           const certificate, privateKey: PUA_ByteString;
                                                           const trustList: PUA_ByteString; trustListSize: size_t;
                                                           const issuerList: PUA_ByteString; issuerListSize: size_t;
                                                           const revocationList: PUA_ByteString; revocationListSize: size_t): UA_StatusCode; cdecl;
  (* The same, without the None policy (no endpoint without security) *)
  UA_ServerConfig_setDefaultWithSecureSecurityPolicies: function(conf: PUA_ServerConfig; portNumber: UA_UInt16;
                                                           const certificate, privateKey: PUA_ByteString;
                                                           const trustList: PUA_ByteString; trustListSize: size_t;
                                                           const issuerList: PUA_ByteString; issuerListSize: size_t;
                                                           const revocationList: PUA_ByteString; revocationListSize: size_t): UA_StatusCode; cdecl;
  {$ENDIF}
  { ---------------- }
  { --- server.h --- }
  { ---------------- }
  UA_Server_delete: procedure(server: PUA_Server); cdecl;
  UA_Server_getConfig: function(server: PUA_Server): PUA_ServerConfig; cdecl;
  UA_Server_run: function(server: PUA_Server; const running: PUA_Boolean): UA_StatusCode; cdecl;
  UA_Server_run_startup: function(server: PUA_Server): UA_StatusCode; cdecl;
  UA_Server_run_iterate: function(server: PUA_Server; waitInternal: UA_Boolean): UA_UInt16; cdecl;
  UA_Server_run_shutdown: function(server: PUA_Server): UA_StatusCode; cdecl;
  UA_Server_addNamespace: function(server: PUA_Server; const name: PAnsiChar): UA_UInt16; cdecl;
  UA_Server_addVariableNode: function(server: PUA_Server; requestedNewNodeId, parentNodeId, referenceTypeId: UA_NodeId;
                                      browseName: UA_QualifiedName; typeDefinition: UA_NodeId; attr: UA_VariableAttributes;
                                      nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode; cdecl;
  UA_Server_addObjectNode: function(server: PUA_Server; requestedNewNodeId, parentNodeId, referenceTypeId: UA_NodeId;
                                    browseName: UA_QualifiedName; typeDefinition: UA_NodeId; attr: UA_ObjectAttributes;
                                    nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode; cdecl;
  {$IFDEF UA_ENABLE_METHODCALLS}
  UA_Server_addMethodNodeEx: function(server: PUA_Server; requestedNewNodeId, parentNodeId, referenceTypeId: UA_NodeId;
                                      browseName: UA_QualifiedName; attr: UA_MethodAttributes; method: UA_MethodCallback;
                                      inputArgumentsSize: size_t; const inputArguments: PUA_Argument;
                                      inputArgumentsRequestedNewNodeId: UA_NodeId; inputArgumentsOutNewNodeId: PUA_NodeId;
                                      outputArgumentsSize: size_t; const outputArguments: PUA_Argument;
                                      outputArgumentsRequestedNewNodeId: UA_NodeId; outputArgumentsOutNewNodeId: PUA_NodeId;
                                      nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode; cdecl;
  {$ENDIF}
  UA_Server_addReference: function(server: PUA_Server; sourceId, refTypeId: UA_NodeId; targetId: UA_ExpandedNodeId; isForward: UA_Boolean): UA_StatusCode; cdecl;
  UA_Server_deleteReference: function(server: PUA_Server; sourceNodeId, referenceTypeId: UA_NodeId; isForward: UA_Boolean;
                                      targetNodeId: UA_ExpandedNodeId; deleteBidirectional: UA_Boolean): UA_StatusCode; cdecl;
  UA_AccessControl_default: function(config: PUA_ServerConfig; allowAnonymous: UA_Boolean; const userTokenPolicyUri: PUA_String;
                                     usernamePasswordLoginSize: size_t; const usernamePasswordLogin: PUA_UsernamePasswordLogin): UA_StatusCode; cdecl;
  UA_AccessControl_defaultWithLoginCallback: function(config: PUA_ServerConfig; allowAnonymous: UA_Boolean;
                                     const userTokenPolicyUri: PUA_String;
                                     usernamePasswordLoginSize: size_t; const usernamePasswordLogin: PUA_UsernamePasswordLogin;
                                     loginCallback: UA_UsernamePasswordLoginCallback; loginContext: Pointer): UA_StatusCode; cdecl;
  UA_Server_writeValue: function(server: PUA_Server; nodeId: UA_NodeId; value: UA_Variant): UA_StatusCode; cdecl;
  UA_Server_readValue: function(server: PUA_Server; nodeId: UA_NodeId; outValue: PUA_Variant): UA_StatusCode; cdecl;
  UA_Server_addCallbackValueSourceVariableNode: function(server: PUA_Server; requestedNewNodeId, parentNodeId, referenceTypeId: UA_NodeId;
                                      browseName: UA_QualifiedName; typeDefinition: UA_NodeId; attr: UA_VariableAttributes;
                                      evs: UA_CallbackValueSource; nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode; cdecl;
  UA_Server_setVariableNode_callbackValueSource: function(server: PUA_Server; nodeId: UA_NodeId; evs: UA_CallbackValueSource): UA_StatusCode; cdecl;
  (* value is the pointer the write callback got; BADNOTFOUND when the
   * operation is no longer waiting (cancelled by the server or timed out). *)
  UA_Server_setAsyncWriteResult: function(server: PUA_Server; const value: PUA_DataValue; result: UA_StatusCode): UA_StatusCode; cdecl;
  UA_Server_setAsyncReadResult: function(server: PUA_Server; result: PUA_DataValue): UA_StatusCode; cdecl;
  UA_Server_deleteNode: function(server: PUA_Server; nodeId: UA_NodeId; deleteReferences: UA_Boolean): UA_StatusCode; cdecl;
  (* OPTIONAL (nil when the library was built without JSON): updates the fields
   * of the configuration present in a JSON text, e.g.
   * {"securityPolicyNoneDiscoveryOnly": true, "asyncOperationTimeout": 30000.0}
   * - a way to reach fields of UA_ServerConfig this binding does not declare. *)
  UA_ServerConfig_updateFromFile: function(config: PUA_ServerConfig; const json_config: UA_ByteString): UA_StatusCode; cdecl;
  {$ENDIF}

(* Loads the library (reference counted). Raises EInOutError when it can not be
 * loaded, misses a function this unit needs, or was built with another
 * selection of types/encryption than this unit expects. *)
(* LibName overrides the name/path of the library (libopen62541) on the first load. *)
procedure LoadOpen62541(const LibName: string = '');
procedure UnloadOpen62541();
function Open62541Loaded: Boolean;
(* The name/path the library was loaded from *)
function Open62541LibraryName: string;

{ --- DataType / DataTypeMember bit fields --- }
function UA_DataType_memSize(const t: PUA_DataType): UA_UInt32; inline;
function UA_DataType_typeKind(const t: PUA_DataType): UA_DataTypeKind; inline;
function UA_DataType_membersSize(const t: PUA_DataType): UA_UInt32; inline;

{ --- types.h helpers --- }
function _UA_StatusCode_Name(code: UA_StatusCode): AnsiString;
function UA_StatusCode_isBad(code: UA_StatusCode): Boolean; inline;
function UA_StatusCode_isUncertain(code: UA_StatusCode): Boolean; inline;
function UA_StatusCode_isGood(code: UA_StatusCode): Boolean; inline;
(* UA_STRING points to the original chars; the *_ALLOC variants make a copy owned by the library *)
function _UA_STRING(var chars: AnsiString): UA_String; inline;
function _UA_STRING_ALLOC(const chars: AnsiString): UA_String;
function _UA_BYTESTRING(var chars: AnsiString): UA_ByteString; inline;
function _UA_BYTESTRING_ALLOC(const chars: AnsiString): UA_ByteString;
function _UA_QUALIFIEDNAME(nsIndex: UA_UInt16; var chars: AnsiString): UA_QualifiedName;
function _UA_QUALIFIEDNAME_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_QualifiedName;
function _UA_LOCALIZEDTEXT(var locale, text: AnsiString): UA_LocalizedText;
function _UA_LOCALIZEDTEXT_ALLOC(const locale, text: AnsiString): UA_LocalizedText;
function _UA_NUMERICRANGE(const s: AnsiString): UA_NumericRange;
function _UA_String_equal(const s1: UA_String; const s2: AnsiString): Boolean;
function UA_StringToStr(const s: UA_String): AnsiString;
function UA_LocalizedTextToStr(const t: UA_LocalizedText): AnsiString;
function UA_NodeIdToStr(const id: UA_NodeId): AnsiString;
function UA_ExpandedNodeIdToStr(const id: UA_ExpandedNodeId): AnsiString;
function UA_DataTypeToStr(typeId: UA_NodeId): AnsiString;
(* Parses "ns=3;s=Name", "i=2258", "ns=2;g=...", "b=..." *)
function UA_StrToNodeId(const s: AnsiString; out id: UA_NodeId): UA_StatusCode;
function UA_NodeId_equal(const n1, n2: UA_NodeId): Boolean;
function UA_String_equal(const s1, s2: PUA_String): Boolean;
(* Formats the message of a log callback (msg/args as received from the library). The
   library uses formats of its own (%S UA_String, %N NodeId) that the C vsnprintf does not
   know: they are formatted by UA_String_vformat. *)
function UA_FormatLogMessage(msg: PAnsiChar; args: Pointer): AnsiString;

function UA_DateTime_toUnixTime(date: UA_DateTime): Int64; inline;
function UA_DateTime_fromUnixTime(unixDate: Int64): UA_DateTime; inline;
function UA_DateTimeToDateTime(date: UA_DateTime): TDateTime;
function UA_DateTimeFromDateTime(dt: TDateTime): UA_DateTime;

(* Returns true if the variant has no value defined (contains neither an array nor a scalar value). *)
function UA_Variant_isEmpty(const v: PUA_Variant): Boolean;
(* Returns true if the variant contains a scalar value. *)
function UA_Variant_isScalar(const v: PUA_Variant): Boolean;
function UA_Variant_hasScalarType(const v: PUA_Variant; const _type: PUA_DataType): Boolean;
function UA_Variant_hasArrayType(const v: PUA_Variant; const _type: PUA_DataType): Boolean;

function UA_Variant_getFloat(var v: UA_Variant): Single;
function UA_Variant_getDouble(var v: UA_Variant): Double;
function UA_Variant_getByte(var v: UA_Variant): Byte;
function UA_Variant_getSmallint(var v: UA_Variant): Smallint;
function UA_Variant_getInteger(var v: UA_Variant): Integer;
function UA_Variant_getInt64(var v: UA_Variant): Int64;
function UA_Variant_getString(var v: UA_Variant): AnsiString; overload;
function UA_Variant_getString(var v: UA_Variant; arrayIndex: DWord): AnsiString; overload;
procedure UA_Variant_setBoolean(out v: UA_Variant; b: Boolean);
procedure UA_Variant_setFloat(out v: UA_Variant; f: Single);
procedure UA_Variant_setDouble(out v: UA_Variant; d: Double);
procedure UA_Variant_setByte(out v: UA_Variant; i: Byte);
procedure UA_Variant_setSmallint(out v: UA_Variant; i: Smallint);
procedure UA_Variant_setUInt16(out v: UA_Variant; i: UInt16);
procedure UA_Variant_setInteger(out v: UA_Variant; i: Integer);
procedure UA_Variant_setUInt32(out v: UA_Variant; i: UInt32);
procedure UA_Variant_setInt64(out v: UA_Variant; i: Int64);
procedure UA_Variant_setUInt64(out v: UA_Variant; i: UInt64);
procedure UA_Variant_setString(out v: UA_Variant; const s: AnsiString);

function UA_NODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_UInt32): UA_NodeId;
function UA_NODEID_STRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
function UA_NODEID_STRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
function UA_NODEID_GUID(nsIndex: UA_UInt16; guid: UA_Guid): UA_NodeId;
function UA_NODEID_BYTESTRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
function UA_NODEID_BYTESTRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
function UA_EXPANDEDNODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_UInt32): UA_ExpandedNodeId;

{ --- types_generated_handling.h --- }
procedure UA_Variant_init(out p: UA_Variant);
procedure UA_Variant_clear(var p: UA_Variant);
procedure UA_String_clear(var p: UA_String);
procedure UA_NodeId_clear(var p: UA_NodeId);
procedure UA_DataValue_clear(var p: UA_DataValue);
procedure UA_CreateSubscriptionRequest_init(out p: UA_CreateSubscriptionRequest);
procedure UA_MonitoredItemCreateRequest_init(out p: UA_MonitoredItemCreateRequest);
procedure UA_BrowseRequest_init(out p: UA_BrowseRequest);
procedure UA_BrowseRequest_clear(var p: UA_BrowseRequest);
procedure UA_BrowseResponse_clear(var p: UA_BrowseResponse);
procedure UA_BrowseDescription_init(out p: UA_BrowseDescription);
procedure UA_BrowseDescription_clear(var p: UA_BrowseDescription);
procedure UA_ReadRequest_init(out p: UA_ReadRequest);
procedure UA_ReadResponse_clear(var p: UA_ReadResponse);
procedure UA_WriteRequest_init(out p: UA_WriteRequest);
procedure UA_WriteResponse_clear(var p: UA_WriteResponse);

{ --- client helpers --- }
(* Connect to the server and create+activate a Session with the given username and password. *)
function UA_Client_connectUsername(client: PUA_Client; const endpointUrl, username, password: AnsiString): UA_StatusCode;
function UA_Client_readValueAttribute_range(client: PUA_Client; const nodeId: UA_NodeId; const indexRange: AnsiString; out outValue: UA_Variant): UA_StatusCode;
{$IFDEF ENABLE_SERVER}
(* Name of UA_Server_addCallbackValueSourceVariableNode before open62541 1.5 *)
function UA_Server_addDataSourceVariableNode(server: PUA_Server; requestedNewNodeId, parentNodeId, referenceTypeId: UA_NodeId;
                                             browseName: UA_QualifiedName; typeDefinition: UA_NodeId; attr: UA_VariableAttributes;
                                             dataSource: UA_DataSource; nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
{$ENDIF}
{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
function UA_CreateSubscriptionRequest_default(): UA_CreateSubscriptionRequest;
function UA_MonitoredItemCreateRequest_default(nodeId: UA_NodeId): UA_MonitoredItemCreateRequest;
{$ENDIF}

{$IFDEF ENABLE_SERVER}
function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString): UA_StatusCode;
{$IFDEF UA_ENABLE_METHODCALLS}
function UA_Server_addMethodNode(server: PUA_Server; const requestedNewNodeId, parentNodeId, referenceTypeId: UA_NodeId;
                                 const browseName: UA_QualifiedName; const attr: UA_MethodAttributes; method: UA_MethodCallback;
                                 inputArgumentsSize: size_t; const inputArguments: PUA_Argument;
                                 outputArgumentsSize: size_t; const outputArguments: PUA_Argument;
                                 nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
{$ENDIF}
{$ENDIF}

implementation

uses
  SysUtils, DynLibs{$IFDEF MSWINDOWS}, Windows{$ENDIF};


var
  open62541LibHandle: TLibHandle = NilHandle;
  RefCount: Integer = 0;
  LoadedLibName: string = libopen62541;

type
  TOpen62541Import = record
    Name: PAnsiChar;
    Addr: PPointer;
  end;

function Imports: TArray<TOpen62541Import>;
  procedure Add(const AName: PAnsiChar; AAddr: PPointer);
  var n: Integer;
  begin
    n := Length(Result);
    SetLength(Result, n+1);
    Result[n].Name := AName;
    Result[n].Addr := AAddr;
  end;
begin
  Result := nil;
  Add('UA_StatusCode_name', @@UA_StatusCode_name);
  Add('UA_String_fromChars', @@UA_String_fromChars);
  Add('UA_String_isEmpty', @@UA_String_isEmpty);
  Add('UA_ByteString_fromBase64', @@UA_ByteString_fromBase64);
  Add('UA_ByteString_toBase64', @@UA_ByteString_toBase64);
  Add('UA_DateTime_now', @@UA_DateTime_now);
  Add('UA_DateTime_toStruct', @@UA_DateTime_toStruct);
  Add('UA_DateTime_fromStruct', @@UA_DateTime_fromStruct);
  Add('UA_NodeId_isNull', @@UA_NodeId_isNull);
  Add('UA_NodeId_print', @@UA_NodeId_print);
  Add('UA_NodeId_parse', @@UA_NodeId_parse);
  Add('UA_String_vformat', @@UA_String_vformat);
  Add('UA_ExpandedNodeId_print', @@UA_ExpandedNodeId_print);
  Add('UA_ExpandedNodeId_parse', @@UA_ExpandedNodeId_parse);
  Add('UA_NumericRange_parse', @@UA_NumericRange_parse);
  Add('UA_Variant_setScalar', @@UA_Variant_setScalar);
  Add('UA_Variant_setScalarCopy', @@UA_Variant_setScalarCopy);
  Add('UA_Variant_setArray', @@UA_Variant_setArray);
  Add('UA_Variant_setArrayCopy', @@UA_Variant_setArrayCopy);
  Add('UA_Variant_setRange', @@UA_Variant_setRange);
  Add('UA_Variant_setRangeCopy', @@UA_Variant_setRangeCopy);
  Add('UA_ExtensionObject_setValue', @@UA_ExtensionObject_setValue);
  Add('UA_KeyValueMap_get', @@UA_KeyValueMap_get);
  Add('UA_findDataType', @@UA_findDataType);
  Add('UA_new', @@UA_new);
  Add('UA_init', @@UA_init);
  Add('UA_copy', @@UA_copy);
  Add('UA_clear', @@UA_clear);
  Add('UA_delete', @@UA_delete);
  Add('UA_equal', @@UA_equal);
  Add('UA_print', @@UA_print);
  Add('UA_Array_new', @@UA_Array_new);
  Add('UA_Array_copy', @@UA_Array_copy);
  Add('UA_Array_delete', @@UA_Array_delete);
  Add('UA_Log_Stdout_new', @@UA_Log_Stdout_new);

  Add('UA_Client_new', @@UA_Client_new);
  Add('UA_Client_newWithConfig', @@UA_Client_newWithConfig);
  Add('UA_Client_getState', @@UA_Client_getState);
  Add('UA_Client_getConfig', @@UA_Client_getConfig);
  Add('UA_Client_delete', @@UA_Client_delete);
  Add('UA_Client_run_iterate', @@UA_Client_run_iterate);
  Add('UA_Client_connect', @@UA_Client_connect);
  Add('UA_Client_connectSecureChannel', @@UA_Client_connectSecureChannel);
  Add('UA_Client_connectAsync', @@UA_Client_connectAsync);
  Add('UA_Client_disconnect', @@UA_Client_disconnect);
  Add('UA_Client_disconnectSecureChannel', @@UA_Client_disconnectSecureChannel);
  Add('UA_Client_getEndpoints', @@UA_Client_getEndpoints);
  Add('UA_Client_getNamespaceIndex', @@UA_Client_getNamespaceIndex);
  Add('__UA_Client_Service', @@__UA_Client_Service);
  Add('UA_Client_Service_read', @@UA_Client_Service_read);
  Add('UA_Client_Service_write', @@UA_Client_Service_write);
  Add('UA_Client_Service_browse', @@UA_Client_Service_browse);
  Add('UA_Client_Service_browseNext', @@UA_Client_Service_browseNext);
  Add('UA_Client_Service_translateBrowsePathsToNodeIds', @@UA_Client_Service_translateBrowsePathsToNodeIds);
  Add('UA_Client_Service_call', @@UA_Client_Service_call);
  Add('UA_ClientConfig_setDefault', @@UA_ClientConfig_setDefault);
  Add('UA_ClientConfig_clear', @@UA_ClientConfig_clear);
  Add('UA_ClientConfig_setAuthenticationUsername', @@UA_ClientConfig_setAuthenticationUsername);
  {$IFDEF UA_ENABLE_ENCRYPTION}
  Add('UA_ClientConfig_setDefaultEncryption', @@UA_ClientConfig_setDefaultEncryption);
  Add('UA_ClientConfig_setAuthenticationCert', @@UA_ClientConfig_setAuthenticationCert);
  Add('UA_CreateCertificate', @@UA_CreateCertificate);
  Add('UA_CertificateUtils_getExpirationDate', @@UA_CertificateUtils_getExpirationDate);
  Add('UA_CertificateUtils_getSubjectName', @@UA_CertificateUtils_getSubjectName);
  {$ENDIF}
  Add('UA_CertificateGroup_AcceptAll', @@UA_CertificateGroup_AcceptAll);
  Add('UA_Client_readValueAttribute', @@UA_Client_readValueAttribute);
  Add('UA_Client_writeValueAttribute', @@UA_Client_writeValueAttribute);
  Add('UA_Client_readArrayDimensionsAttribute', @@UA_Client_readArrayDimensionsAttribute);
  Add('UA_Client_writeArrayDimensionsAttribute', @@UA_Client_writeArrayDimensionsAttribute);
  {$IFDEF UA_ENABLE_METHODCALLS}
  Add('UA_Client_call', @@UA_Client_call);
  {$ENDIF}
  {$IFDEF UA_ENABLE_SUBSCRIPTIONS}
  Add('UA_Client_Subscriptions_create', @@UA_Client_Subscriptions_create);
  Add('UA_Client_Subscriptions_modify', @@UA_Client_Subscriptions_modify);
  Add('UA_Client_Subscriptions_delete', @@UA_Client_Subscriptions_delete);
  Add('UA_Client_Subscriptions_deleteSingle', @@UA_Client_Subscriptions_deleteSingle);
  Add('UA_Client_MonitoredItems_createDataChanges', @@UA_Client_MonitoredItems_createDataChanges);
  Add('UA_Client_MonitoredItems_createDataChange', @@UA_Client_MonitoredItems_createDataChange);
  Add('UA_Client_MonitoredItems_delete', @@UA_Client_MonitoredItems_delete);
  Add('UA_Client_MonitoredItems_deleteSingle', @@UA_Client_MonitoredItems_deleteSingle);
  {$ENDIF}
  {$IFDEF ENABLE_SERVER}
  Add('UA_Server_new', @@UA_Server_new);
  Add('UA_ServerConfig_setMinimalCustomBuffer', @@UA_ServerConfig_setMinimalCustomBuffer);
  {$IFDEF UA_ENABLE_ENCRYPTION}
  Add('UA_ServerConfig_setDefaultWithSecurityPolicies', @@UA_ServerConfig_setDefaultWithSecurityPolicies);
  Add('UA_ServerConfig_setDefaultWithSecureSecurityPolicies', @@UA_ServerConfig_setDefaultWithSecureSecurityPolicies);
  {$ENDIF}
  Add('UA_Server_delete', @@UA_Server_delete);
  Add('UA_Server_getConfig', @@UA_Server_getConfig);
  Add('UA_Server_run', @@UA_Server_run);
  Add('UA_Server_run_startup', @@UA_Server_run_startup);
  Add('UA_Server_run_iterate', @@UA_Server_run_iterate);
  Add('UA_Server_run_shutdown', @@UA_Server_run_shutdown);
  Add('UA_Server_addNamespace', @@UA_Server_addNamespace);
  Add('UA_Server_addVariableNode', @@UA_Server_addVariableNode);
  Add('UA_Server_addObjectNode', @@UA_Server_addObjectNode);
  {$IFDEF UA_ENABLE_METHODCALLS}
  Add('UA_Server_addMethodNodeEx', @@UA_Server_addMethodNodeEx);
  {$ENDIF}
  Add('UA_Server_addReference', @@UA_Server_addReference);
  Add('UA_Server_deleteReference', @@UA_Server_deleteReference);
  Add('UA_AccessControl_default', @@UA_AccessControl_default);
  Add('UA_AccessControl_defaultWithLoginCallback', @@UA_AccessControl_defaultWithLoginCallback);
  Add('UA_Server_writeValue', @@UA_Server_writeValue);
  Add('UA_Server_readValue', @@UA_Server_readValue);
  Add('UA_Server_addCallbackValueSourceVariableNode', @@UA_Server_addCallbackValueSourceVariableNode);
  Add('UA_Server_setVariableNode_callbackValueSource', @@UA_Server_setVariableNode_callbackValueSource);
  Add('UA_Server_setAsyncWriteResult', @@UA_Server_setAsyncWriteResult);
  Add('UA_Server_setAsyncReadResult', @@UA_Server_setAsyncReadResult);
  Add('UA_Server_deleteNode', @@UA_Server_deleteNode);
  {$ENDIF}
end;

procedure ClearImports;
var
  imp: TOpen62541Import;
begin
  for imp in Imports do
    imp.Addr^ := nil;
  {$IFDEF ENABLE_SERVER}
  UA_ServerConfig_updateFromFile := nil;
  {$ENDIF}
  UA_TYPES := nil;
end;

procedure CheckLoadedLibrary;
var
  i: Integer;
  t: PUA_DataType;
  encSym: Pointer;
begin
  (* Every entry of the type array must be the type this unit was generated for *)
  for i := 0 to UA_TYPES_COUNT-1 do begin
    t := @UA_TYPES[i];
    if (t^.typeId.namespaceIndex <> 0) or (t^.typeId.identifierType <> UA_NODEIDTYPE_NUMERIC) or
       (t^.typeId.identifier.numeric <> UA_TYPES_TYPEIDS[i]) then
      raise EInOutError.CreateFmt('"%s" does not match this binding: UA_TYPES[%d] is i=%d, expected i=%d. '+
                                  'Use an open62541 1.5 library built with the default type selection '+
                                  '(UA_ENABLE_DATATYPES_ALL=ON) and UA_ENABLE_TYPEDESCRIPTION=ON.',
                                  [LoadedLibName, i, t^.typeId.identifier.numeric, UA_TYPES_TYPEIDS[i]]);
  end;
  (* UA_ClientConfig has more fields when the library is built with encryption *)
  encSym := GetProcedureAddress(open62541LibHandle, 'UA_ClientConfig_setDefaultEncryption');
  {$IFDEF UA_ENABLE_ENCRYPTION}
  if encSym = nil then
    raise EInOutError.CreateFmt('"%s" was built without encryption: undefine UA_ENABLE_ENCRYPTION in open62541.pas or use a library built with UA_ENABLE_ENCRYPTION.', [LoadedLibName]);
  {$ELSE}
  if encSym <> nil then
    raise EInOutError.CreateFmt('"%s" was built with encryption: define UA_ENABLE_ENCRYPTION in open62541.pas.', [LoadedLibName]);
  {$ENDIF}
end;

procedure LoadOpen62541(const LibName: string);
var
  imp: TOpen62541Import;
  missing: string;
  p: Pointer;
begin
  Inc(RefCount);
  if RefCount > 1 then exit;
  if LibName <> '' then
    LoadedLibName := LibName
  else
    LoadedLibName := libopen62541;
  open62541LibHandle := DynLibs.LoadLibrary(LoadedLibName);
  if open62541LibHandle = NilHandle then begin
    RefCount := 0;
    raise EInOutError.CreateFmt('Can not load library "%s". Check your installation.'+sLineBreak+'%s',
                                [LoadedLibName, GetLoadErrorStr()]);
  end;
  try
    missing := '';
    for imp in Imports do begin
      imp.Addr^ := GetProcedureAddress(open62541LibHandle, imp.Name);
      if imp.Addr^ = nil then
        missing := missing + ' ' + imp.Name;
    end;
    {$IFDEF ENABLE_SERVER}
    //optional: a library built without JSON does not have it
    UA_ServerConfig_updateFromFile := GetProcedureAddress(open62541LibHandle, 'UA_ServerConfig_updateFromFile');
    {$ENDIF}
    UA_TYPES := GetProcedureAddress(open62541LibHandle, 'UA_TYPES');
    if UA_TYPES = nil then
      missing := missing + ' UA_TYPES';
    if missing <> '' then
      raise EInOutError.CreateFmt('"%s" is not an open62541 1.5 library: missing%s', [LoadedLibName, missing]);
    CheckLoadedLibrary;

    p := GetProcedureAddress(open62541LibHandle, 'UA_VariableAttributes_default');
    if p <> nil then UA_VariableAttributes_default := PUA_VariableAttributes(p)^;
    p := GetProcedureAddress(open62541LibHandle, 'UA_ObjectAttributes_default');
    if p <> nil then UA_ObjectAttributes_default := PUA_ObjectAttributes(p)^;
    p := GetProcedureAddress(open62541LibHandle, 'UA_MethodAttributes_default');
    if p <> nil then UA_MethodAttributes_default := PUA_MethodAttributes(p)^;
    p := GetProcedureAddress(open62541LibHandle, 'UA_ObjectTypeAttributes_default');
    if p <> nil then UA_ObjectTypeAttributes_default := PUA_ObjectTypeAttributes(p)^;
    p := GetProcedureAddress(open62541LibHandle, 'UA_ReferenceTypeAttributes_default');
    if p <> nil then UA_ReferenceTypeAttributes_default := PUA_ReferenceTypeAttributes(p)^;
    p := GetProcedureAddress(open62541LibHandle, 'UA_DataTypeAttributes_default');
    if p <> nil then UA_DataTypeAttributes_default := PUA_DataTypeAttributes(p)^;
  except
    ClearImports;
    UnloadLibrary(open62541LibHandle);
    open62541LibHandle := NilHandle;
    RefCount := 0;
    raise;
  end;
end;

procedure UnloadOpen62541();
begin
  if RefCount > 0 then begin
    Dec(RefCount);
    if RefCount = 0 then begin
      ClearImports;
      UnloadLibrary(open62541LibHandle);
      open62541LibHandle := NilHandle;
    end;
  end;
end;

function Open62541Loaded: Boolean;
begin
  Result := RefCount > 0;
end;

function Open62541LibraryName: string;
begin
  Result := LoadedLibName;
end;

function UA_DataType_memSize(const t: PUA_DataType): UA_UInt32;
begin
  Result := t^.bits and $FFFF;
end;

function UA_DataType_typeKind(const t: PUA_DataType): UA_DataTypeKind;
begin
  Result := UA_DataTypeKind((t^.bits shr 16) and $3F);
end;

function UA_DataType_membersSize(const t: PUA_DataType): UA_UInt32;
begin
  Result := (t^.bits shr 24) and $FF;
end;

function _UA_StatusCode_Name(code: UA_StatusCode): AnsiString;
begin
  Result := Format('%x:%s', [code, AnsiString(UA_StatusCode_name(code))]);
end;

function UA_StatusCode_isBad(code: UA_StatusCode): Boolean;
begin
  Result := (code shr 30) >= 2;
end;

function UA_StatusCode_isUncertain(code: UA_StatusCode): Boolean;
begin
  Result := (code shr 30) = 1;
end;

function UA_StatusCode_isGood(code: UA_StatusCode): Boolean;
begin
  Result := (code shr 30) = 0;
end;

function _UA_STRING(var chars: AnsiString): UA_String;
begin
  if chars = '' then begin
    Result.length := 0;
    Result.data := nil;
  end
  else begin
    Result.length := Length(chars);
    Result.data := @chars[1];
  end;
end;

function _UA_STRING_ALLOC(const chars: AnsiString): UA_String;
var
  bs: UA_ByteString;
begin
  if chars = '' then begin
    Result.length := 0;
    Result.data := nil;
  end
  else begin
    // the library must allocate the memory (the C and Pascal heaps can not be mixed)
    bs.length := Length(chars);
    bs.data := @chars[1];
    UA_copy(@bs, @Result, @UA_TYPES[UA_TYPES_BYTESTRING]);
  end;
end;

function _UA_BYTESTRING(var chars: AnsiString): UA_ByteString;
begin
  Result := UA_ByteString(_UA_STRING(chars));
end;

function _UA_BYTESTRING_ALLOC(const chars: AnsiString): UA_ByteString;
begin
  Result := UA_ByteString(_UA_STRING_ALLOC(chars));
end;

function _UA_QUALIFIEDNAME(nsIndex: UA_UInt16; var chars: AnsiString): UA_QualifiedName;
begin
  Result.namespaceIndex := nsIndex;
  Result.name := _UA_STRING(chars);
end;

function _UA_QUALIFIEDNAME_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_QualifiedName;
begin
  Result.namespaceIndex := nsIndex;
  Result.name := _UA_STRING_ALLOC(chars);
end;

function _UA_LOCALIZEDTEXT(var locale, text: AnsiString): UA_LocalizedText;
begin
  Result.locale := _UA_STRING(locale);
  Result.text := _UA_STRING(text);
end;

function _UA_LOCALIZEDTEXT_ALLOC(const locale, text: AnsiString): UA_LocalizedText;
begin
  Result.locale := _UA_STRING_ALLOC(locale);
  Result.text := _UA_STRING_ALLOC(text);
end;

function _UA_NUMERICRANGE(const s: AnsiString): UA_NumericRange;
var
  tmp: AnsiString;
begin
  Result.dimensionsSize := 0;
  Result.dimensions := nil;
  tmp := s;
  UA_NumericRange_parse(@Result, _UA_STRING(tmp));
end;

function _UA_String_equal(const s1: UA_String; const s2: AnsiString): Boolean;
var
  tmp: AnsiString;
  s: UA_String;
begin
  tmp := s2;
  s := _UA_STRING(tmp);
  Result := UA_String_equal(@s1, @s);
end;

function UA_StringToStr(const s: UA_String): AnsiString;
begin
  SetString(Result, PAnsiChar(s.data), s.length);
end;

function UA_LocalizedTextToStr(const t: UA_LocalizedText): AnsiString;
begin
  SetString(Result, PAnsiChar(t.text.data), t.text.length);
end;

function UA_NodeIdToStr(const id: UA_NodeId): AnsiString;
var
  output: UA_String;
begin
  output := UA_STRING_NULL;
  UA_NodeId_print(@id, @output);
  SetString(Result, PAnsiChar(output.data), output.length);
  UA_String_clear(output);
end;

function UA_ExpandedNodeIdToStr(const id: UA_ExpandedNodeId): AnsiString;
var
  output: UA_String;
begin
  output := UA_STRING_NULL;
  UA_ExpandedNodeId_print(@id, @output);
  SetString(Result, PAnsiChar(output.data), output.length);
  UA_String_clear(output);
end;

function UA_DataTypeToStr(typeId: UA_NodeId): AnsiString;
var
  pDataType: PUA_DataType;
begin
  pDataType := UA_findDataType(@typeId);
  if pDataType = nil then
    Result := 'Unknown'
  else
    Result := pDataType^.typeName;
end;

function UA_StrToNodeId(const s: AnsiString; out id: UA_NodeId): UA_StatusCode;
var
  tmp: AnsiString;
begin
  FillChar(id, SizeOf(id), 0);
  tmp := s;
  Result := UA_NodeId_parse(@id, _UA_STRING(tmp));
end;

function UA_String_equal(const s1, s2: PUA_String): Boolean;
begin
  Result := UA_equal(s1, s2, @UA_TYPES[UA_TYPES_STRING]);
end;

function UA_NodeId_equal(const n1, n2: UA_NodeId): Boolean;
begin
  Result := UA_equal(@n1, @n2, @UA_TYPES[UA_TYPES_NODEID]);
end;

function UA_FormatLogMessage(msg: PAnsiChar; args: Pointer): AnsiString;
var
  buf: array[0..1023] of AnsiChar;
  str: UA_String;
begin
  buf[0] := #0;
  (* written into buf (no allocation); a longer message is cut, still ending in #0 *)
  str.length := SizeOf(buf);
  str.data := @buf[0];
  UA_String_vformat(@str, msg, args);
  buf[High(buf)] := #0;
  Result := PAnsiChar(@buf[0]);
end;

function UA_DateTime_toUnixTime(date: UA_DateTime): Int64;
begin
  Result := (date - UA_DATETIME_UNIX_EPOCH) div UA_DATETIME_SEC;
end;

function UA_DateTime_fromUnixTime(unixDate: Int64): UA_DateTime;
begin
  Result := (unixDate * UA_DATETIME_SEC) + UA_DATETIME_UNIX_EPOCH;
end;

const
  TicksPerDay: Double = 864000000000.0; // 100 ns ticks in a day (typed: a Double)

function UA_DateTimeToDateTime(date: UA_DateTime): TDateTime;
var
  d: Double;
begin
  // 100 ns ticks since 1601-01-01 UTC -> TDateTime (days since 1899-12-30), UTC
  // Double arithmetic: a "86400.0" literal would be a Single for FPC (precision of minutes)
  d := date - UA_DATETIME_UNIX_EPOCH;
  Result := d / TicksPerDay + UnixDateDelta;
end;

function UA_DateTimeFromDateTime(dt: TDateTime): UA_DateTime;
begin
  Result := Round((dt - UnixDateDelta) * TicksPerDay) + UA_DATETIME_UNIX_EPOCH;
end;

function UA_Variant_isEmpty(const v: PUA_Variant): Boolean;
begin
  Result := v^._type = nil;
end;

function UA_Variant_isScalar(const v: PUA_Variant): Boolean;
begin
  Result := (v^.arrayLength = 0) and (PByte(v^.data) > PByte(UA_EMPTY_ARRAY_SENTINEL));
end;

function UA_Variant_hasScalarType(const v: PUA_Variant; const _type: PUA_DataType): Boolean;
begin
  Result := UA_Variant_isScalar(v) and (_type = v^._type);
end;

function UA_Variant_hasArrayType(const v: PUA_Variant; const _type: PUA_DataType): Boolean;
begin
  Result := (not UA_Variant_isScalar(v)) and (_type = v^._type);
end;

function UA_Variant_getFloat(var v: UA_Variant): Single;
begin
  Result := PUA_Float(v.data)^;
end;

function UA_Variant_getDouble(var v: UA_Variant): Double;
begin
  Result := PUA_Double(v.data)^;
end;

function UA_Variant_getByte(var v: UA_Variant): Byte;
begin
  Result := PUA_Byte(v.data)^;
end;

function UA_Variant_getSmallint(var v: UA_Variant): Smallint;
begin
  Result := PUA_Int16(v.data)^;
end;

function UA_Variant_getInteger(var v: UA_Variant): Integer;
begin
  Result := PUA_Int32(v.data)^;
end;

function UA_Variant_getInt64(var v: UA_Variant): Int64;
begin
  Result := PUA_Int64(v.data)^;
end;

function UA_Variant_getString(var v: UA_Variant): AnsiString;
begin
  SetString(Result, PAnsiChar(PUA_String(v.data)^.data), PUA_String(v.data)^.length);
end;

function UA_Variant_getString(var v: UA_Variant; arrayIndex: DWord): AnsiString;
begin
  if arrayIndex < v.arrayLength then
    SetString(Result, PAnsiChar(PUA_String(v.data)[arrayIndex].data), PUA_String(v.data)[arrayIndex].length)
  else
    Result := '';
end;

procedure UA_Variant_setBoolean(out v: UA_Variant; b: Boolean);
var
  x: UA_Boolean;
begin
  UA_Variant_init(v);
  x := b;
  UA_Variant_setScalarCopy(@v, @x, @UA_TYPES[UA_TYPES_BOOLEAN]);
end;

procedure UA_Variant_setFloat(out v: UA_Variant; f: Single);
begin
  UA_Variant_init(v);
  UA_Variant_setScalarCopy(@v, @f, @UA_TYPES[UA_TYPES_FLOAT]);
end;

procedure UA_Variant_setDouble(out v: UA_Variant; d: Double);
begin
  UA_Variant_init(v);
  UA_Variant_setScalarCopy(@v, @d, @UA_TYPES[UA_TYPES_DOUBLE]);
end;

procedure UA_Variant_setByte(out v: UA_Variant; i: Byte);
begin
  UA_Variant_init(v);
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_BYTE]);
end;

procedure UA_Variant_setSmallint(out v: UA_Variant; i: Smallint);
begin
  UA_Variant_init(v);
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_INT16]);
end;

procedure UA_Variant_setUInt16(out v: UA_Variant; i: UInt16);
begin
  UA_Variant_init(v);
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_UINT16]);
end;

procedure UA_Variant_setInteger(out v: UA_Variant; i: Integer);
begin
  UA_Variant_init(v);
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_INT32]);
end;

procedure UA_Variant_setUInt32(out v: UA_Variant; i: UInt32);
begin
  UA_Variant_init(v);
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_UINT32]);
end;

procedure UA_Variant_setInt64(out v: UA_Variant; i: Int64);
begin
  UA_Variant_init(v);
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_INT64]);
end;

procedure UA_Variant_setUInt64(out v: UA_Variant; i: UInt64);
begin
  UA_Variant_init(v);
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_UINT64]);
end;

procedure UA_Variant_setString(out v: UA_Variant; const s: AnsiString);
var
  tmp: AnsiString;
  uas: UA_String;
begin
  UA_Variant_init(v);
  tmp := s;
  uas := _UA_STRING(tmp);
  UA_Variant_setScalarCopy(@v, @uas, @UA_TYPES[UA_TYPES_STRING]);
end;

function UA_NODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_UInt32): UA_NodeId;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_NUMERIC;
  Result.identifier.numeric := identifier;
end;

function UA_NODEID_STRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_STRING;
  Result.identifier._string := _UA_STRING(chars);
end;

function UA_NODEID_STRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_STRING;
  Result.identifier._string := _UA_STRING_ALLOC(chars);
end;

function UA_NODEID_GUID(nsIndex: UA_UInt16; guid: UA_Guid): UA_NodeId;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_GUID;
  Result.identifier.guid := guid;
end;

function UA_NODEID_BYTESTRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_BYTESTRING;
  Result.identifier.byteString := _UA_BYTESTRING(chars);
end;

function UA_NODEID_BYTESTRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_BYTESTRING;
  Result.identifier.byteString := _UA_BYTESTRING_ALLOC(chars);
end;

function UA_EXPANDEDNODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_UInt32): UA_ExpandedNodeId;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.nodeId := UA_NODEID_NUMERIC(nsIndex, identifier);
end;

procedure UA_Variant_init(out p: UA_Variant);
begin
  FillChar(p, SizeOf(UA_Variant), 0);
end;

procedure UA_Variant_clear(var p: UA_Variant);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_VARIANT]);
end;

procedure UA_String_clear(var p: UA_String);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_STRING]);
end;

procedure UA_NodeId_clear(var p: UA_NodeId);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_NODEID]);
end;

procedure UA_DataValue_clear(var p: UA_DataValue);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_DATAVALUE]);
end;

procedure UA_CreateSubscriptionRequest_init(out p: UA_CreateSubscriptionRequest);
begin
  FillChar(p, SizeOf(p), 0);
end;

procedure UA_MonitoredItemCreateRequest_init(out p: UA_MonitoredItemCreateRequest);
begin
  FillChar(p, SizeOf(p), 0);
end;

procedure UA_BrowseRequest_init(out p: UA_BrowseRequest);
begin
  FillChar(p, SizeOf(p), 0);
end;

procedure UA_BrowseRequest_clear(var p: UA_BrowseRequest);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_BROWSEREQUEST]);
end;

procedure UA_BrowseResponse_clear(var p: UA_BrowseResponse);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_BROWSERESPONSE]);
end;

procedure UA_BrowseDescription_init(out p: UA_BrowseDescription);
begin
  FillChar(p, SizeOf(p), 0);
end;

procedure UA_BrowseDescription_clear(var p: UA_BrowseDescription);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_BROWSEDESCRIPTION]);
end;

procedure UA_ReadRequest_init(out p: UA_ReadRequest);
begin
  FillChar(p, SizeOf(p), 0);
end;

procedure UA_ReadResponse_clear(var p: UA_ReadResponse);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_READRESPONSE]);
end;

procedure UA_WriteRequest_init(out p: UA_WriteRequest);
begin
  FillChar(p, SizeOf(p), 0);
end;

procedure UA_WriteResponse_clear(var p: UA_WriteResponse);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_WRITERESPONSE]);
end;

function UA_Client_connectUsername(client: PUA_Client; const endpointUrl, username, password: AnsiString): UA_StatusCode;
begin
  Result := UA_ClientConfig_setAuthenticationUsername(UA_Client_getConfig(client), PAnsiChar(username), PAnsiChar(password));
  if Result = UA_STATUSCODE_GOOD then
    Result := UA_Client_connect(client, PAnsiChar(endpointUrl));
end;

function UA_Client_readValueAttribute_range(client: PUA_Client; const nodeId: UA_NodeId; const indexRange: AnsiString; out outValue: UA_Variant): UA_StatusCode;
var
  item: UA_ReadValueId;
  request: UA_ReadRequest;
  response: UA_ReadResponse;
  tmp: AnsiString;
begin
  UA_Variant_init(outValue);
  FillChar(item, SizeOf(item), 0);
  item.nodeId := nodeId;
  item.attributeId := Ord(UA_ATTRIBUTEID_VALUE);
  tmp := indexRange;
  item.indexRange := _UA_STRING(tmp);
  UA_ReadRequest_init(request);
  request.nodesToRead := @item;
  request.nodesToReadSize := 1;
  response := UA_Client_Service_read(client, request);
  Result := response.responseHeader.serviceResult;
  if Result = UA_STATUSCODE_GOOD then begin
    if response.resultsSize = 1 then begin
      if (response.results^.flags and UA_DATAVALUE_HASSTATUS) <> 0 then
        Result := response.results^.status;
      if (response.results^.flags and UA_DATAVALUE_HASVALUE) <> 0 then begin
        outValue := response.results^.value;
        UA_Variant_init(response.results^.value);
      end
      else if Result = UA_STATUSCODE_GOOD then
        Result := UA_STATUSCODE_BADUNEXPECTEDERROR;
    end
    else
      Result := UA_STATUSCODE_BADUNEXPECTEDERROR;
  end;
  UA_ReadResponse_clear(response);
end;

{$IFDEF ENABLE_SERVER}
function UA_Server_addDataSourceVariableNode(server: PUA_Server; requestedNewNodeId, parentNodeId, referenceTypeId: UA_NodeId;
                                             browseName: UA_QualifiedName; typeDefinition: UA_NodeId; attr: UA_VariableAttributes;
                                             dataSource: UA_DataSource; nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result := UA_Server_addCallbackValueSourceVariableNode(server, requestedNewNodeId, parentNodeId, referenceTypeId,
                                                        browseName, typeDefinition, attr, dataSource, nodeContext, outNewNodeId);
end;
{$ENDIF}

{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
function UA_CreateSubscriptionRequest_default(): UA_CreateSubscriptionRequest;
begin
  UA_CreateSubscriptionRequest_init(Result);
  Result.requestedPublishingInterval := 500.0;
  Result.requestedLifetimeCount := 10000;
  Result.requestedMaxKeepAliveCount := 10;
  Result.maxNotificationsPerPublish := 0;
  Result.publishingEnabled := True;
  Result.priority := 0;
end;

function UA_MonitoredItemCreateRequest_default(nodeId: UA_NodeId): UA_MonitoredItemCreateRequest;
begin
  UA_MonitoredItemCreateRequest_init(Result);
  Result.itemToMonitor.nodeId := nodeId;
  Result.itemToMonitor.attributeId := Ord(UA_ATTRIBUTEID_VALUE);
  Result.monitoringMode := UA_MONITORINGMODE_REPORTING;
  Result.requestedParameters.samplingInterval := 250;
  Result.requestedParameters.discardOldest := True;
  Result.requestedParameters.queueSize := 1;
end;
{$ENDIF}

{$IFDEF ENABLE_SERVER}
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString): UA_StatusCode;
begin
  Result := UA_ServerConfig_setMinimalCustomBuffer(config, portNumber, certificate, 0, 0);
end;

function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;
begin
  Result := UA_ServerConfig_setMinimal(config, 4840, nil);
end;

{$IFDEF UA_ENABLE_METHODCALLS}
function UA_Server_addMethodNode(server: PUA_Server; const requestedNewNodeId, parentNodeId, referenceTypeId: UA_NodeId;
                                 const browseName: UA_QualifiedName; const attr: UA_MethodAttributes; method: UA_MethodCallback;
                                 inputArgumentsSize: size_t; const inputArguments: PUA_Argument;
                                 outputArgumentsSize: size_t; const outputArguments: PUA_Argument;
                                 nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result := UA_Server_addMethodNodeEx(server, requestedNewNodeId, parentNodeId,
                                      referenceTypeId, browseName, attr, method,
                                      inputArgumentsSize, inputArguments, UA_NODEID_NULL, nil,
                                      outputArgumentsSize, outputArguments, UA_NODEID_NULL, nil,
                                      nodeContext, outNewNodeId);
end;
{$ENDIF}
{$ENDIF}

initialization
  FillChar(UA_STRING_NULL, SizeOf(UA_STRING_NULL), 0);
  FillChar(UA_NODEID_NULL, SizeOf(UA_NODEID_NULL), 0);
  FillChar(UA_EXPANDEDNODEID_NULL, SizeOf(UA_EXPANDEDNODEID_NULL), 0);

end.
