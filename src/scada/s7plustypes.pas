{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Constantes e tipos do protocolo S7CommPlus, usado pelos CLPs S7-1200/1500 da Siemens.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Portado de python-snap7/s7commplus/protocol.py (referência: thomas-v2/S7CommPlusDriver,
  dissector S7CommPlus do Wireshark). O protocolo S7CommPlus não é documentado oficialmente
  pela Siemens; esta unit reflete o conhecimento obtido por engenharia reversa da comunidade.
}
{$ELSE}
{:
  @abstract(S7CommPlus protocol constants and types, used by Siemens S7-1200/1500 PLCs.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Ported from python-snap7/s7commplus/protocol.py (reference: thomas-v2/S7CommPlusDriver,
  Wireshark's S7CommPlus dissector). S7CommPlus is not officially documented by Siemens;
  this unit reflects community reverse-engineering knowledge.
}
{$ENDIF}
unit S7PlusTypes;

{$mode Delphi}{$H+}

interface

type
  //: S7CommPlus frame header, present on every message exchanged with the PLC.
  TS7PlusHeader = record
    ProtocolId:Byte;    //: Always $72 (vs $32 of legacy S7comm).
    Version:Byte;       //: Protocol version, one of TS7PlusProtocolVersion.
    DataLen:Word;       //: Length of the data that follows this header.
  end;

const
  //: Protocol identification byte (vs $32 of legacy S7comm).
  S7Plus_PROTOCOL_ID = $72;

  //-- Protocol versions ------------------------------------------------------
  //: Early S7-1200 (FW >= V4.0). Simple session handshake, no TLS.
  S7PlusVersion_V1 = $01;
  //: Adds integrity checking and session authentication. Requires TLS.
  S7PlusVersion_V2 = $02;
  //: Adds public-key-based key exchange.
  S7PlusVersion_V3 = $03;
  //: Seen on system-event notifications.
  S7PlusVersion_SystemEvent = $FE;

  //-- Opcodes (first byte of the request/response header, after the frame header) --
  S7PlusOpcode_Request      = $31;
  S7PlusOpcode_Response     = $32;
  S7PlusOpcode_Notification = $33;
  S7PlusOpcode_Response2    = $02; //: Seen in some older firmware.

  //-- Function codes ----------------------------------------------------------
  S7PlusFunc_Error               = $04B1;
  S7PlusFunc_Explore             = $04BB;
  S7PlusFunc_CreateObject        = $04CA;
  S7PlusFunc_DeleteObject        = $04D4;
  S7PlusFunc_SetVariable         = $04F2;
  S7PlusFunc_GetVariable         = $04FC; //: Only in old S7-1200 firmware.
  S7PlusFunc_AddLink             = $0506;
  S7PlusFunc_RemoveLink          = $051A;
  S7PlusFunc_GetLink             = $0524;
  S7PlusFunc_SetMultiVariables   = $0542;
  S7PlusFunc_GetMultiVariables   = $054C;
  S7PlusFunc_BeginSequence       = $0556;
  S7PlusFunc_EndSequence         = $0560;
  S7PlusFunc_Invoke              = $056B;
  S7PlusFunc_SetVarSubstreamed   = $057C;
  S7PlusFunc_GetVarSubstreamed   = $0586;
  S7PlusFunc_GetVariablesAddress = $0590;
  S7PlusFunc_Abort               = $059A;
  S7PlusFunc_Error2              = $05A9;
  S7PlusFunc_InitSSL             = $05B3;

  //-- Element IDs: tags of the object serialization format (nested/attribute model) --
  S7PlusElement_StartOfObject           = $A1;
  S7PlusElement_TerminatingObject       = $A2;
  S7PlusElement_Attribute               = $A3;
  S7PlusElement_Relation                = $A4;
  S7PlusElement_StartOfTagDescription   = $A7;
  S7PlusElement_TerminatingTagDescr     = $A8;
  S7PlusElement_VartypeList             = $AB;
  S7PlusElement_VarnameList             = $AC;

  //-- Well-known object IDs used in session establishment ---------------------
  S7PlusObjId_None                     = 0;
  S7PlusObjId_GetNewRIDOnServer        = 211;
  S7PlusObjId_ClassSubscriptions       = 255;
  S7PlusObjId_ClassServerSessionContainer  = 284;
  S7PlusObjId_ObjectServerSessionContainer = 285;
  S7PlusObjId_ClassServerSession       = 287;
  S7PlusObjId_ObjectNullServerSession  = 288;
  S7PlusObjId_ServerSessionClientRID   = 300;
  S7PlusObjId_ServerSessionVersion     = 306;

  //-- Legitimation (password authentication, V2+ with TLS) ---------------------
  S7PlusObjId_ServerSessionRequest       = 303;
  S7PlusObjId_ServerSessionResponse      = 304;
  S7PlusObjId_SessionSetupLegitimation   = 1830;
  S7PlusObjId_EffectiveProtectionLevel   = 1842;
  S7PlusObjId_Legitimate                 = 1846;
  S7PlusObjId_LegitimationPayloadStruct   = 40400;
  S7PlusObjId_LegitimationPayloadType     = 40401;
  S7PlusObjId_LegitimationPayloadUsername = 40402;
  S7PlusObjId_LegitimationPayloadPassword = 40403;

  //-- AccessLevel values returned by EffectiveProtectionLevel (lower = more access).
  S7PlusAccessLevel_FullAccess = 1;
  S7PlusAccessLevel_ReadAccess = 2;
  S7PlusAccessLevel_HMIAccess  = 3;
  S7PlusAccessLevel_NoAccess   = 4;

  //-- Subscriptions (on-change notifications, V2+) - Core/Ids.cs / Subscriptions/Subscription.cs.
  //-- (Attribute 233 is S7PlusIds_ObjectVariableTypeName, already declared further below.)
  S7PlusObjId_ClassSubscription           = 1001;
  S7PlusObjId_SubscriptionFunctionClassId = 1082;
  S7PlusObjId_SubscriptionMissedSendings  = 1002;
  S7PlusObjId_SubscriptionSubsystemError  = 1003;
  S7PlusObjId_SubscriptionRouteMode       = 1040;
  S7PlusObjId_SubscriptionActive          = 1041;
  S7PlusObjId_SubscriptionReferenceList   = 1048;
  S7PlusObjId_SubscriptionCycleTime       = 1049;
  S7PlusObjId_SubscriptionDisabled        = 1051;
  S7PlusObjId_SubscriptionCount           = 1052;
  S7PlusObjId_SubscriptionCreditLimit     = 1053;
  S7PlusObjId_SubscriptionTicks           = 1054;

  //-- TSAP used by every S7CommPlus connection ---------------------------------
  S7PlusLocalTSAP  = $0600;
  S7PlusRemoteTSAP : AnsiString = 'SIMATIC-ROOT-HMI'; //: 16-byte ASCII, fixed.

  //-- Wire data types (DataType) -----------------------------------------------
  S7PlusType_NULL      = $00;
  S7PlusType_BOOL      = $01;
  S7PlusType_USINT     = $02;
  S7PlusType_UINT      = $03;
  S7PlusType_UDINT     = $04;
  S7PlusType_ULINT     = $05;
  S7PlusType_SINT      = $06;
  S7PlusType_INT       = $07;
  S7PlusType_DINT      = $08;
  S7PlusType_LINT      = $09;
  S7PlusType_BYTE      = $0A;
  S7PlusType_WORD      = $0B;
  S7PlusType_DWORD     = $0C;
  S7PlusType_LWORD     = $0D;
  S7PlusType_REAL      = $0E;
  S7PlusType_LREAL     = $0F;
  S7PlusType_TIMESTAMP = $10;
  S7PlusType_TIMESPAN  = $11;
  S7PlusType_RID       = $12;
  S7PlusType_AID       = $13;
  S7PlusType_BLOB      = $14;
  S7PlusType_WSTRING   = $15;
  S7PlusType_VARIANT   = $16;
  S7PlusType_STRUCT    = $17;
  S7PlusType_S7STRING  = $19;

  //-- Well-known IDs (Ids.cs) ---------------------------------------------------

  //: Sub-area used to access the "actual value" of a data block.
  S7PlusIds_DBValueActual = 2550;
  //: Symbolic (LID-based) access to controller areas (I/Q/M).
  S7PlusIds_ControllerAreaValueActual = 3736;

  //: ObjectQualifier structure IDs.
  S7PlusIds_ObjectQualifier = 1256;
  S7PlusIds_ParentRID       = 1257;
  S7PlusIds_CompositionAID  = 1258;
  S7PlusIds_KeyQualifier    = 1259;

  //: Native object RIDs for memory areas.
  S7PlusIds_NativeIAreaRID        = 80;
  S7PlusIds_NativeQAreaRID        = 81;
  S7PlusIds_NativeMAreaRID        = 82;
  S7PlusIds_NativeS7CountersRID   = 83;
  S7PlusIds_NativeS7TimersRID     = 84;

  //: Native object RIDs used by EXPLORE.
  S7PlusIds_NativeThePLCProgramRID   = 3;
  S7PlusIds_NativeTheAlarmSubsysRID  = 8;
  S7PlusIds_NativeTheCPUExecUnitRID  = 52;

  //: Object attributes in EXPLORE responses.
  S7PlusIds_ObjectVariableTypeName = 233;
  S7PlusIds_BlockBlockNumber       = 2521;

  //: Type info classes.
  S7PlusIds_ClassTypeInfo               = 511;
  S7PlusIds_ClassOMSTypeInfoContainer   = 534;
  S7PlusIds_ObjectOMSTypeInfoContainer  = 537;
  S7PlusIds_PLCProgramClassRID          = 2520;
  S7PlusIds_DBClassRID                  = 2574; //: ClassId of a DataBlock object in EXPLORE.

  //: Subscription classes (data change notifications).
  S7PlusIds_ClassSubscriptions           = 255;
  S7PlusIds_ClassSubscription            = 1001;
  S7PlusIds_SubscriptionCycleTime        = 1049;
  S7PlusIds_SubscriptionActive           = 1041;
  S7PlusIds_SubscriptionCreditLimit      = 1053;
  S7PlusIds_SubscriptionReferenceList    = 1048;
  S7PlusIds_SubscriptionFunctionClassId  = 1082;

  //: Alarm subscription.
  S7PlusIds_AlarmSubscriptionRefClassRID       = 2662;
  S7PlusIds_AlarmSubscriptionRefAlarmDomain    = 2659;
  S7PlusIds_AlarmSubscriptionRefItsAlarmSubsys = 2660;

  //: DB AccessArea base (add the DB number to get the area ID for that DB).
  S7PlusIds_DBAccessAreaBase = $8A0E0000;

  //-- Legitimation IDs, used in password authentication (V2+/TLS) --------------
  S7PlusLegit_ServerSessionRequest    = 303;
  S7PlusLegit_ServerSessionResponse   = 304;
  S7PlusLegit_SessionSetupLegitimation = 1830;
  S7PlusLegit_Legitimate              = 1846;

//: Tells whether a function code belongs to the "read" IntegrityId track (V2+).
function IsS7PlusReadFunctionCode(FunctionCode:Word):Boolean;

implementation

function IsS7PlusReadFunctionCode(FunctionCode:Word):Boolean;
begin
  Result := (FunctionCode = S7PlusFunc_GetMultiVariables) or
            (FunctionCode = S7PlusFunc_Explore) or
            (FunctionCode = S7PlusFunc_GetVarSubstreamed) or
            (FunctionCode = S7PlusFunc_GetLink) or
            (FunctionCode = S7PlusFunc_GetVariable) or
            (FunctionCode = S7PlusFunc_GetVariablesAddress);
end;

end.
