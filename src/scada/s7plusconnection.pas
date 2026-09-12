{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Sessão/transporte do protocolo S7CommPlus (CLPs S7-1200/1500 da Siemens).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Implementa o transporte ISO-on-TCP (TPKT+COTP, igual ao S7 clássico, porém com o TSAP
  remoto fixo "SIMATIC-ROOT-HMI") e o estabelecimento de sessão S7CommPlus versão V1
  (CreateObject + SetupSession, sem TLS). V2/V3/TLS ficam para uma fase futura - ver
  ActivateTLS/Authenticate (ainda não implementados).

  Portado de python-snap7/s7commplus/connection.py e client.py (referência:
  thomas-v2/S7CommPlusDriver, C#, LGPL-3.0).
}
{$ELSE}
{:
  @abstract(S7CommPlus session/transport (Siemens S7-1200/1500 PLCs).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Implements the ISO-on-TCP transport (TPKT+COTP, same as legacy S7, but with the fixed
  remote TSAP "SIMATIC-ROOT-HMI") and S7CommPlus V1 session establishment (CreateObject +
  SetupSession, no TLS). V2/V3/TLS are left for a future phase - see ActivateTLS/Authenticate
  (not yet implemented).

  Ported from python-snap7/s7commplus/connection.py and client.py (reference:
  thomas-v2/S7CommPlusDriver, C#, LGPL-3.0).
}
{$ENDIF}
unit S7PlusConnection;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, ctypes, CommPort, CommTypes, S7PlusTypes, S7PlusVLQ, S7PlusCodec, S7PlusSSL,
  S7PlusTypeInfo, S7PlusHarpoKeys, S7PlusHarpoLegacyAuth, S7PlusHarpoBlobMeta,
  S7PlusHarpoLegitimate;

type
  ES7PlusError = class(Exception);

  //: Called with a human-readable trace line for every relevant protocol step.
  //: Assign it (e.g. to a WriteLn wrapper) to diagnose a connection against real hardware.
  TS7PlusDebugEvent = procedure(Sender:TObject; const Msg:String) of object;

  //: One symbolic (LID-based) item to read in a single batched GetMultiVariables request -
  //: see TS7PlusConnection.ReadMultipleSymbolic. Items may span different DBs/areas (each
  //: carries its own AccessArea), matching how the reference implementation's
  //: _build_read_payload batches items freely across DB numbers.
  TS7PlusMultiReadItem = record
    AccessArea:Cardinal;
    Lids:TS7PlusLIDArray;
    //: Only used by BuildSubscriptionReferenceList (a plain GetMultiVariables read ignores
    //: this) - the variable's real SymbolCrc from BrowseDB/BrowseNativeArea's TS7PlusVarInfo.
    //: Defaults to 0 (a normal read's "skip layout check"), which is wrong for subscriptions.
    SymbolCrc:Cardinal;
  end;
  TS7PlusMultiReadItemArray = array of TS7PlusMultiReadItem;

  //: One item's result from ReadMultipleSymbolic: Ok=false means the PLC reported this
  //: specific item as failed (e.g. bad LID) while the rest of the batch still succeeded -
  //: distinct from ReadMultipleSymbolic itself returning false, which means the whole
  //: request failed (comm error, or the PLC rejected it outright with a nonzero returnValue).
  TS7PlusMultiReadResult = record
    Data:TBytes;
    Ok:Boolean;
  end;
  TS7PlusMultiReadResultArray = array of TS7PlusMultiReadResult;

  //: One value out of a parsed Notification (Core/Notification.cs): RefId is the same
  //: 1-based id BuildSubscriptionReferenceList assigned to the subscribed item, Data its
  //: raw bytes (same convention as TS7PlusMultiReadResult.Data).
  TS7PlusNotificationValue = record
    RefId:Cardinal;
    Data:TBytes;
  end;
  TS7PlusNotificationValueArray = array of TS7PlusNotificationValue;

  { TS7PlusConnection }

  TS7PlusConnection = class
  private
    FCommPort:TCommPortDriver;
    FDriverID:Cardinal;

    FSequenceNumber:Word;
    FSessionId:Cardinal;
    FProtocolVersion:Byte;
    FConnected:Boolean;
    FServerSessionVersion:TBytes;
    //: AttrId 233 ("00:xxxxxxxxxxxxxxxx") and AttrId 303 (20-byte nonce), both seen in
    //: CreateObject's response alongside ServerSessionVersion - present on firmware that
    //: requires the legitimation handshake (SetupSession attribute 1830) even though
    //: ServerSessionVersion is also present (contradicts python-snap7's own assumption
    //: that the two are mutually exclusive - confirmed empirically wrong against a real
    //: S7-1513 FW2.9.2). Empty/nil when the PLC doesn't send them.
    FServerFingerprint:AnsiString;
    FServerChallenge:TBytes;
    //: 24-byte key HarpoS7's DeriveSessionKey produces from the legitimation exchange -
    //: also the input to PostAuthLegitimation's challenge-solving step.
    FHarpoSessionKey:TBytes;
    //: PLC public key/family resolved during SetupSession's SecurityKey blob build -
    //: kept for PostAuthLegitimation, which needs the same key to solve its own
    //: (separate) challenge.
    FHarpoAuthPublicKey:TBytes;
    FHarpoAuthFamily:TS7PlusHarpoKeyFamily;
    //: Incrementing "RequestId" byte seen on each of TIA's post-legitimation system
    //: CreateObjects (ChangeCounter=4, then 5, 6, ... for the system Subscription_* ones)
    //: - a different counter than ItemNumber/IntegrityId, starts at 4 for the first one.
    FTiaSysObjectRequestId:Cardinal;
    FSessionSetupOK:Boolean;
    FSrcRef:Word;
    FLastReturnValue:QWord;
    FOnDebug:TS7PlusDebugEvent;

    //-- V2+ TLS/IntegrityId ---------------------------------------------------
    FUseTLS:Boolean;
    FTLSActive:Boolean;
    FSSLCtx:PSSLCTX;
    FSSL:PSSL;
    FReadBIO, FWriteBIO:PBIO;
    FWithIntegrityId:Boolean;
    FIntegrityIdRead, FIntegrityIdWrite:Cardinal;
    FRecvBuf:TBytes;

    //-- Subscriptions (on-change notifications, V2+) --------------------------
    //: ObjectId the PLC assigned to the nested ClassSubscriptions container created
    //: alongside the session in CreateSession (see the second StartOfObject/
    //: TerminatingObject pair in its request body) - CreateObject's response there
    //: returns TWO object ids (session, then this container), but the shared
    //: ParseCreateObjectSessionId helper only kept the first. Confirmed against real
    //: hardware to be the missing "parent object" a Subscription's CreateObject needs
    //: (SubscriptionCreate's envelope "unknown value 1", previously hardcoded 0, was
    //: rejected as "download: no parent object!"/"Download error (IDs & states [main,
    //: sub, TI, next])!"). Also what the reference's SubscriptionDelete calls "SessionId2"
    //: and deletes by - NOT the connection's own FSessionId.
    FSubscriptionsContainerObjectId:Cardinal;
    FSubscriptionObjectId:Cardinal;
    FSubscriptionRelationId:Cardinal;
    FSubscriptionChangeCounter:Byte;
    FSubscriptionCreditLimit:SmallInt;

    //-- Symbolic (LID) resolution cache: the OMS type-info container is expensive to
    //-- EXPLORE (can be a large, multi-fragment response) - fetch it once per connection.
    FTypeInfoCached:Boolean;
    FTypeInfoObjects:TS7PlusObjectArray;
    FExploreDelayMs:Integer;

    function EnsureTypeInfoObjects:Boolean;
    //: Reads LID=1 of a DB to get its type-info RID (0 if the DB has no readable value) -
    //: needed because instance DBs' type-info RID differs from their own object RID.
    function ReadDBTypeInfoRid(DBAccessArea:Cardinal; out TiRid:Cardinal):Boolean;

    procedure Debug(const Msg:String);
    function NextSequenceNumber:Word;

    //-- TPKT/COTP transport --------------------------------------------------
    function CotpConnect:Boolean;
    function TransportSend(const Frame:TBytes):Boolean;
    function TransportRecv(out Frame:TBytes):TIOResult;

    //-- TLS tunnel (V2+), on top of the raw COTP transport above --------------
    function ActivateTLS:Boolean;
    procedure TLSFlushOutgoing;
    function TLSReadIncoming:Boolean;
    procedure ReleaseTLS;
    //: Routes a logical S7CommPlus frame through TLS (if active) or straight to the
    //: raw COTP transport (if not) - every session-handshake/request method below
    //: sends/receives through these instead of TransportSend/TransportRecv directly.
    function LogicalSend(const Frame:TBytes):Boolean;
    function LogicalRecv(out Frame:TBytes):TIOResult;
    //: Reads N bytes from FRecvBuf, topping it up via LogicalRecv as needed.
    function EnsureBuffered(N:Integer):Boolean;
    //: Receives a possibly multi-fragment S7CommPlus response (EXPLORE and other large
    //: responses split across several PDUs: [$72][ver][len:2][data] with no per-fragment
    //: trailer; a trailing len=0 fragment ends the sequence). Returns the concatenated data.
    function ReassembledRecv(out Data:TBytes):Boolean;

    //-- S7CommPlus session handshake (V1/V2) ----------------------------------
    function InitSSL:Boolean;
    function CreateSession:Boolean;
    function SetupSession:Boolean;
    //: V1-initial SessionKey handshake, post-SetupSession steps - see python-snap7's
    //: _session_activate: writes USINT(5) to address 323 on the session object. Required
    //: before any data operation on a PLC that needed the SecurityKey blob.
    function SessionActivate:Boolean;
    //: V1-initial SessionKey handshake, final step - see python-snap7's
    //: _post_auth_legitimation: re-reads the 20-byte challenge from address 303, solves
    //: it (S7PlusHarpoLegitimate.SolveLegitimateChallengeRealPlc), writes the 248-byte
    //: result to address 1846. Required before the PLC allows any data operation on a
    //: session that went through the SecurityKey blob exchange.
    function PostAuthLegitimation(const Password:AnsiString):Boolean;
    //: Generic single-address V1 GetVarSubstreamed read, reused by the TIA-observed
    //: post-legitimation sequence below (each call in that sequence targets a different
    //: InObjectId/address/seq_field, unlike PostAuthLegitimation's own hardcoded one).
    function GetVarSubstreamedRaw(InObjectId, Address, SeqField:Cardinal; out RawResp:TBytes):Boolean;
    function CreateChangeCounterObject:Boolean;
    function CreateEmptySystemSubscription(CycleTimeMs:Cardinal; RouteMode:Byte; FunctionClassId:Byte; CreditLimit:SmallInt):Boolean;
    //: GetLink(InObjectId, Attr) - resolves a fixed system RID's "link" attribute to a list
    //: of related object RIDs. TIA uses InObjectId=11 (a fixed, always-present system
    //: object) with Attr=2699 to get the RID backing DB-value monitoring (TisMonitorJob) -
    //: see GetDB4Fingerprint/CreateTisMonitorJob.
    function GetLinkRaw(InObjectId, Attr:Cardinal; out RIDs:TS7PlusLIDArray):Boolean;
    //: Generic "read N raw attribute fields off an object" (the GetMultiVariables shape
    //: TIA uses for system objects addressed by field id, not by symbolic LID - distinct
    //: from BuildSymbolicPayload/BuildMultiSymbolicReadPayload which are LID-based).
    function GetMultiVariablesFields(InObjectId:Cardinal; const FieldIds:array of Cardinal; out Results:TS7PlusMultiReadResultArray):Boolean;
    //: Fetches DB4's live "fingerprint" chunk (13 bytes, itself read out of field 9505 off
    //: the RID GetLink(11,2699) resolves) that TIA embeds verbatim into every
    //: TisMonitorJob it creates for that DB - see CreateTisMonitorJob. Confirmed byte-for-
    //: byte identical between the read and 2 different real TisMonitorJob captures.
    function GetDB4Fingerprint(out Fingerprint:TBytes):Boolean;
    //: CreateTisMonitorJob is declared in the public section below (Lids are the plain
    //: LIDs, e.g. from BrowseDB on a PLC where that works, of the DB4 members to monitor;
    //: each becomes a 12-byte record [LID+$58][fixed 7-byte type tag][countdown][$80]
    //: [offset], offsets packed 4 bytes apart starting at 20 - reverse-engineered
    //: 2026-08-31 from 2 real TIA captures with different monitored-item counts).
    //: TIA Portal, after PostAuthLegitimation succeeds, reads a handful of session/system
    //: attributes (300=ServerSessionClientRID, 1842=EffectiveProtectionLevel, and two
    //: fixed small-RID system objects) before EXPLORE/GetMultiVariables are accepted -
    //: decoded byte-for-byte from a real TIA V15 capture against this exact PLC
    //: (subscription_capture2.pcapng). Skips the CreateObject "SPL_PlcProgram_
    //: ChangeCounter"/GetLink/batch-GetMultiVariables/Subscription_* objects also seen in
    //: that capture - those look Watch-Table-UI-specific (not needed for Subscriptions
    //: created from code, which already works end-to-end against .210 without any of
    //: this). If EXPLORE still fails after this, those are the next things to try.
    function PostLegitimationSessionInfo:Boolean;
    //: Returns True if Payload (a SystemEvent frame's data section, Version=0xFE) is
    //: safe to ignore (matches python-snap7's _check_system_event): a bare 16-byte
    //: payload, or one without a Struct(0x17) tag at offset 16, is purely informational.
    //: A Struct payload is only fatal if its ReturnValue member (id 0x9D71) is negative.
    function IsNonFatalSystemEvent(const Payload:TBytes):Boolean;
    //: Reads one full application response frame, transparently consuming (and logging)
    //: any non-fatal SystemEvent frames (Version=0xFE) received while waiting - matches
    //: python-snap7's _recv_response_frame. Confirmed live 2026-08-31: the PLC can send
    //: an unsolicited SystemEvent in between our request and its real response, and
    //: treating that SystemEvent AS the response (the previous behaviour here) silently
    //: discarded the real one.
    function RecvApplicationFrame(out ResponseFrame:TBytes):Boolean;

    //-- payload builders/parsers ----------------------------------------------
    //: Picks the V1 fixed-width ObjectQualifier (KeyQualifier=1, matching TIA's own V1
    //: session-info reads byte-for-byte) when running under the legacy V1-initial/SecurityKey
    //: regime (FHarpoSessionKey set), otherwise the normal V2 VLQ-terminated one - every
    //: request builder that embeds an ObjectQualifier should go through this instead of
    //: calling EncodeObjectQualifier directly, so V1-only PLCs get the right wire shape
    //: everywhere, not just in the hand-ported GetVarSubstreamedRaw/SessionActivate calls.
    function CurrentObjectQualifier:TBytes;
    function BuildAreaPayload(AccessArea, AccessSubArea:Cardinal; Start:Integer; const WriteData:TBytes; IsWrite:Boolean; SizeIfRead:Integer):TBytes;
    function BuildSymbolicPayload(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const WriteData:TBytes; IsWrite:Boolean; SymbolCrc:Cardinal; WriteSoftDataType:Byte=0):TBytes;
    function ParseSingleReadResponse(const Response:TBytes; out Data:TBytes):Boolean;
    function ParseSingleWriteResponse(const Response:TBytes):Boolean;
    function BuildMultiSymbolicReadPayload(const Items:TS7PlusMultiReadItemArray):TBytes;
    function ParseMultiReadResponse(const Response:TBytes; ItemCount:Integer; out Results:TS7PlusMultiReadResultArray):Boolean;

    //: Best-effort scan of a tagged-object error/fault structure (the PLC attaches one
    //: after a non-zero returnValue) for any WSTRING-typed attributes, which usually
    //: carry a human-readable description of what was rejected and why (e.g. "Request
    //: GetVariableSubrangeStreamed"). Returns them joined by " | ", or '' if none found.
    function ExtractErrorText(const Data:TBytes):String;

    //-- Legitimation (password authentication, V2+ with TLS) - see Authenticate.
    //: Derives the 32-byte OMS exporter secret from the active TLS session (RFC 5705,
    //: label "EXPERIMENTAL_OMS"), used as the AES-256-CBC key material for "new"-style
    //: legitimation. Requires TLS to be active.
    function GetOMSSecret(out Secret:TBytes):Boolean;
    //: Serializes the {LegitimationType, Username, Password/PasswordHash} ValueStruct sent
    //: (encrypted, for the "new" style) as the legitimation response payload.
    function BuildLegitimationPayload(const Password, Username:AnsiString):TBytes;
    //: Legacy response: SHA-1(password) XORed with the challenge (first 20 bytes).
    function BuildLegacyLegitimationResponse(const Password:AnsiString; const Challenge:TBytes):TBytes;
    //: New-style response: the legitimation payload, AES-256-CBC encrypted with
    //: key=SHA-256(OmsSecret) and iv=challenge[:16].
    function BuildNewLegitimationResponse(const Password:AnsiString; const Challenge, OmsSecret:TBytes; const Username:AnsiString):TBytes;
    //: Requests the legitimation challenge from the PLC (GetVarSubStreamed on
    //: ServerSessionRequest).
    function GetLegitimationChallenge(out Challenge:TBytes):Boolean;
    //: Requests the session's current access level (GetVarSubStreamed on
    //: EffectiveProtectionLevel) - the reference always checks this before attempting
    //: legitimation, and skips it entirely if already at FullAccess (1). See
    //: S7PlusAccessLevel_* constants.
    function GetEffectiveProtectionLevel(out AccessLevel:Cardinal):Boolean;
    //: Sends the AES-256-CBC encrypted response (SetVariable on Legitimate).
    function SendLegitimationNew(const EncryptedResponse:TBytes):Boolean;
    //: Sends the legacy SHA-1/XOR response (SetVariable on ServerSessionResponse).
    function SendLegitimationLegacy(const Response:TBytes):Boolean;

    //-- Subscriptions (on-change notifications, V2+) - see SubscriptionCreate.
    //: Builds the SubscriptionReferenceList attribute value (Subscriptions/Subscription.cs's
    //: GetSubscriptionListArray): a UDInt array (Flags=$20, the same "Addressarray" flavor
    //: GetVarSubstreamedRequest uses) encoding a create/unsubscribe header followed by, per
    //: item, its 1-based reference id and full symbolic address (AccessArea/SymbolCrc/
    //: AccessSubArea/Lids). Items[i]'s reference id is i+1 - callers use that same mapping
    //: to route Notification values back to their originating item.
    function BuildSubscriptionReferenceList(const Items:TS7PlusMultiReadItemArray):TBytes;
  public
    constructor Create(ACommPort:TCommPortDriver; ADriverID:Cardinal);
    destructor Destroy; override;

    //: Establishes the COTP connection and the S7CommPlus V1 session (CreateObject+SetupSession).
    //: Password is only used on a V1-initial/no-TLS SecurityKey session (see
    //: PostAuthLegitimation) - ignored otherwise, matching every PLC that doesn't need it.
    function Connect(const Password:AnsiString=''):Boolean;
    procedure Disconnect;

    //: Sends a request and returns the response payload (after the 10-byte response header).
    //: IntegrityTail is how many trailing payload bytes the V2 IntegrityId is spliced
    //: before (4 for GetMultiVariables/SetMultiVariables, 5 for Explore). Reassemble
    //: receives a possibly multi-fragment response (needed for Explore).
    function SendRequest(FunctionCode:Word; const Payload:TBytes; out RespPayload:TBytes;
                         IntegrityTail:Integer=4; Reassemble:Boolean=false):Boolean;

    //: DIAGNOSTIC: sends a caller-provided request body verbatim (header + V3/HMAC framing
    //: added, but NO IntegrityId splicing - the body is sent exactly as given). Used by the
    //: block-download replay experiment to re-send captured TIA transaction requests. If
    //: PatchIntegrityAtEnd>=0, our own IntegrityId(write) VLQ is spliced at position
    //: Length(Body)-PatchIntegrityAtEnd (mirroring SendRequest) and the counter advanced;
    //: pass -1 to send with no integrity at all (sequenced ops carry none). Reassemble picks
    //: the multi-fragment response path (for Explore).
    function SendReplayRaw(FunctionCode:Word; const Body:TBytes; PatchIntegrityAtEnd:Integer;
                           out RespPayload:TBytes; Reassemble:Boolean=false):Boolean;

    //: Reads raw bytes from a controller memory area (M/I/Q/counters/timers), by native RID.
    //: Only works for areas/DBs whose value isn't symbol/LID addressed (rare in practice -
    //: see ReadSymbolic). Kept for Phase 1 compatibility/diagnostics.
    function ReadArea(AreaRID:Cardinal; Start, Size:Integer; out Data:TBytes):Boolean;
    //: Writes raw bytes to a controller memory area (M/I/Q/counters/timers), by native RID.
    function WriteArea(AreaRID:Cardinal; Start:Integer; const Data:TBytes):Boolean;
    //: Reads raw bytes from a data block by byte offset. See ReadArea's caveat: real
    //: S7-1200/1500 firmware requires symbolic (LID) access for DB variables - use
    //: ReadSymbolic/BrowseDB instead for actual data, not this.
    function DBRead(DBNumber, Start, Size:Integer; out Data:TBytes):Boolean;
    //: Writes raw bytes to a data block by byte offset. See DBRead's caveat.
    function DBWrite(DBNumber, Start:Integer; const Data:TBytes):Boolean;
    //: Creates a "TisMonitorJob" system object - the mechanism TIA Portal's DB declaration-
    //: table "Monitor all" actually uses to read live DB values on the V1-initial/
    //: SecurityKey firmware fallback, where the normal class-1001 Subscription is rejected
    //: with a constant fatal SystemEvent regardless of addressing style. See the private
    //: declaration below for the full explanation.
    function CreateTisMonitorJob(const Lids:array of Cardinal; out JobRID:Cardinal):Boolean;

    //-- Symbolic (LID-based) access - the only way to reliably read/write DB variables
    //-- and native-area (M/I/Q/Timers/Counters) named tags on real S7-1200/1500 firmware.

    //: Reads a variable's raw bytes by its resolved LID path (see BrowseDB/BrowseNativeArea).
    function ReadSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; out Data:TBytes; SymbolCrc:Cardinal=0):Boolean;
    //: Writes a variable's raw bytes by its resolved LID path.
    function WriteSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const Data:TBytes; SymbolCrc:Cardinal=0; SoftDataType:Byte=0):Boolean;
    //: Reads several symbolic (LID-based) items in a single GetMultiVariables round-trip -
    //: items may span different DBs/areas. Result=false means the whole request failed
    //: (comm error or the PLC rejected it outright); on Result=true, check each item's own
    //: Results[i].Ok, since the PLC can report individual items as failed within an
    //: otherwise-successful batch (e.g. one bad LID among several good ones).
    function ReadMultipleSymbolic(const Items:TS7PlusMultiReadItemArray; out Results:TS7PlusMultiReadResultArray):Boolean;
    //: Sends an EXPLORE request for ExploreId (a RID) and reassembles the (possibly
    //: multi-fragment) response payload.
    function Explore(ExploreId:Cardinal; const AttributeIds:array of Cardinal; out RespPayload:TBytes):Boolean;
    //: Browses a DB's variables via EXPLORE + the compiled type-info tree, returning a
    //: flat list of (name, LID path, softdatatype) - the only reliable way to know a DB
    //: variable's real address on S7CommPlus. The type-info container is cached after the
    //: first call (any DB/area), so subsequent browses of other DBs/areas are cheap.
    function BrowseDB(DBNumber:Cardinal; out Vars:TS7PlusVarInfoArray):Boolean;
    //: Browses a native process area (Inputs/Outputs/Flags/Timers/Counters) the same way,
    //: using its fixed, well-known type-info RID (no per-DB LID=1 lookup needed).
    function BrowseNativeArea(AreaRID, TiRid:Cardinal; const AreaName:String; out Vars:TS7PlusVarInfoArray):Boolean;
    //: Lists every DB in the PLC program with its symbolic (TIA Portal) name and numeric
    //: DB number, via a structured EXPLORE of the whole PLC program object tree. Used to
    //: resolve a Path given by symbolic DB name (e.g. "BlocoSimbolico_4") to its "DB<n>"
    //: form, since all other Path resolution is number-based.
    function ListDataBlocks(out Blocks:TS7PlusDataBlockInfoArray):Boolean;
    //: Performs PLC password authentication (legitimation), required by some PLCs before
    //: SetMultiVariables/DB writes are accepted even though reads are open (a protection
    //: level configured in TIA Portal). Must be called after Connect, with TLS active.
    //: Tries the "new" AES-256-CBC style first, falling back to legacy SHA-1/XOR if that's
    //: rejected. Username is only needed for the new-style with an explicit login name;
    //: leave it empty for a plain password.
    function Authenticate(const Password:AnsiString; const Username:AnsiString=''):Boolean;

    //: Creates a subscription (CreateObject, ClassId=ClassSubscription) that makes the PLC
    //: push on-change Notifications for Items instead of having to poll them via
    //: GetMultiVariables - see Subscriptions/Subscription.cs. Items[i]'s Notification
    //: reference id is i+1 (1-based, matching insertion order) - see
    //: BuildSubscriptionReferenceList. CycleTimeMs is the PLC-side sampling interval
    //: (~100ms minimum in practice). On success, SubscriptionObjectId identifies the
    //: subscription for SubscriptionSetCreditLimit/SubscriptionDelete.
    //: RouteMode/UseFixedRelationId/IncludeAttr1055/CreditLimitOverride are exploratory
    //: knobs (default to the values already validated as correct against the reference and
    //: real hardware) - see the "Download error (IDs & states [main, sub, TI, next])!"
    //: investigation in s7commplus_subscription_test.lpr's combo-sweep mode.
    function SubscriptionCreate(const Items:TS7PlusMultiReadItemArray; CycleTimeMs:Word;
                                 RouteMode:Byte=$14; UseFixedRelationId:Boolean=false;
                                 IncludeAttr1055:Boolean=true; CreditLimitOverride:SmallInt=10):Boolean;
    //: Renews the subscription's notification "credit" - the PLC stops pushing
    //: Notifications once NotificationCreditTick reaches the last limit set, so this must
    //: be called again (with a higher limit) before that happens to keep updates flowing.
    //: Fire-and-forget (TransportFlags=0x74 - the reference marks this "no response needed").
    function SubscriptionSetCreditLimit(Limit:SmallInt):Boolean;
    //: Deletes the active subscription (DeleteObject on the session).
    function SubscriptionDelete:Boolean;
    //: Parses one incoming Notification frame (unsolicited, Opcode=Notification - arrives
    //: interleaved with ordinary responses on the same connection, so callers must peek the
    //: opcode of whatever LogicalRecv returns before assuming it's a reply to their own
    //: last request). Values are keyed by the same 1-based reference id
    //: BuildSubscriptionReferenceList assigned to each subscribed item.
    function ParseNotification(const Frame:TBytes; out CreditTick:Byte; out Values:TS7PlusNotificationValueArray):Boolean;
    //: Blocks (up to the CommPort's own configured Timeout) for one incoming frame and
    //: returns its body starting at the Opcode byte (same slice ParseNotification expects,
    //: or what SendRequest would treat as a response body's Opcode..TransportFlags prefix).
    //: Only meaningful when nothing else is concurrently expecting a reply on this
    //: connection - see SubscriptionWaitNotification in TS7CommPlusDriver.
    function WaitForFrame(out FrameBody:TBytes):Boolean;

    //-- Block upload (read-only, Phase 4 first slice) ------------------------
    //: Uploads (reads) a compiled block's raw body from the PLC via GetVarSubStreamed, using
    //: the same "Addressarray" shape confirmed for GetLegitimationChallenge/
    //: GetEffectiveProtectionLevel, generalized to a 2-element address (BlockType,
    //: BlockNumber) instead of a single attribute id. BlockType uses the same numbering as
    //: TS7PlusSoftDataType's BLOCK_* values (e.g. 25=DB, 24=FC, 23=FB, 36=OB) - neither
    //: available reference (python-snap7, thomas-v2/S7CommPlusDriver) has a validated
    //: implementation of this; this is a first attempt, iterated against real hardware.
    //: Does not (yet) reassemble multi-fragment responses - large blocks may fail.
    function UploadBlock(BlockType, BlockNumber:Cardinal; out Data:TBytes):Boolean;

    property Connected:Boolean read FConnected;
    property SessionId:Cardinal read FSessionId;
    property ProtocolVersion:Byte read FProtocolVersion;
    property SessionSetupOK:Boolean read FSessionSetupOK;
    //: Return value (VLQ64) parsed from the last GetMultiVariables/SetMultiVariables
    //: response; 0 means the PLC reported success on that request.
    property LastReturnValue:QWord read FLastReturnValue;
    property OnDebug:TS7PlusDebugEvent read FOnDebug write FOnDebug;
    //: Whether to activate TLS (after InitSSL, before CreateObject). Most S7-1200/1500
    //: firmware from TIA Portal V15+ requires this for anything beyond the initial
    //: handshake - default true. Set to false only for known-legacy V1-only firmware.
    property UseTLS:Boolean read FUseTLS write FUseTLS;
    //: True once the TLS tunnel is established and in use for all further messages.
    property TLSActive:Boolean read FTLSActive;
    //: Diagnostic only: milliseconds to sleep right before sending the structured
    //: Explore(OMS TypeInfo container) request in EnsureTypeInfoObjects. Used to test
    //: whether the PLC needs breathing room between the preceding GetMultiVariables
    //: call and Explore. Default 0 (no delay).
    property ExploreDelayMs:Integer read FExploreDelayMs write FExploreDelayMs;
  end;

//: Formats bytes as a space-separated hex string, for debug traces.
function S7PlusHexStr(const B:TBytes):String;

implementation

function S7PlusHexStr(const B:TBytes):String;
const
  HexDigits:array[0..15] of Char = '0123456789ABCDEF';
var
  i, p:Integer;
begin
  if Length(B)=0 then begin
    Result := '';
    exit;
  end;
  SetLength(Result, Length(B)*3-1);
  p := 1;
  for i:=0 to High(B) do begin
    if i>0 then begin
      Result[p] := ' ';
      inc(p);
    end;
    Result[p]   := HexDigits[B[i] shr 4];
    Result[p+1] := HexDigits[B[i] and $0F];
    inc(p, 2);
  end;
end;

function BytesConcat(const A, B:TBytes):TBytes;
begin
  SetLength(Result, Length(A)+Length(B));
  if Length(A)>0 then Move(A[0], Result[0], Length(A));
  if Length(B)>0 then Move(B[0], Result[Length(A)], Length(B));
end;

function BytesOf(const B:array of Byte):TBytes;
begin
  SetLength(Result, Length(B));
  if Length(B)>0 then
    Move(B[0], Result[0], Length(B));
end;

function BytesCopy(const A:TBytes; Start, Len:Integer):TBytes;
begin
  if Len<0 then Len := 0;
  if (Start+Len)>Length(A) then Len := Length(A)-Start;
  if Len<0 then Len := 0;
  SetLength(Result, Len);
  if Len>0 then
    Move(A[Start], Result[0], Len);
end;

//: Scalar response trailer used by some S7-1200 firmware for single-byte reads: 00 04 00 00 00 00.
const
  ScalarResponseSuffix:array[0..5] of Byte = ($00,$04,$00,$00,$00,$00);

function EndsWithScalarSuffix(const Data:TBytes):Boolean;
var
  i, base:Integer;
begin
  Result := Length(Data)>=Length(ScalarResponseSuffix);
  if not Result then exit;
  base := Length(Data)-Length(ScalarResponseSuffix);
  for i:=0 to High(ScalarResponseSuffix) do
    if Data[base+i]<>ScalarResponseSuffix[i] then begin
      Result := false;
      exit;
    end;
end;

{ TS7PlusConnection }

constructor TS7PlusConnection.Create(ACommPort:TCommPortDriver; ADriverID:Cardinal);
begin
  inherited Create;
  FCommPort := ACommPort;
  FDriverID := ADriverID;
  FSequenceNumber := 0;
  FSessionId := 0;
  FProtocolVersion := S7PlusVersion_V1;
  FConnected := false;
  FSessionSetupOK := false;
  FSrcRef := $0001;
  //Most S7-1200/1500 firmware from TIA Portal V15+ requires TLS for anything beyond
  //the initial handshake (InitSSL/CreateObject) - default to on.
  FUseTLS := true;
  FTLSActive := false;
  FWithIntegrityId := false;
  FIntegrityIdRead := 0;
  FIntegrityIdWrite := 0;
end;

destructor TS7PlusConnection.Destroy;
begin
  ReleaseTLS;
  inherited Destroy;
end;

function TS7PlusConnection.NextSequenceNumber:Word;
begin
  Result := FSequenceNumber;
  FSequenceNumber := (FSequenceNumber+1) and $FFFF;
end;

procedure TS7PlusConnection.Debug(const Msg:String);
begin
  if Assigned(FOnDebug) then
    FOnDebug(Self, Msg);
end;

//===========================================================================
// TPKT/COTP transport
//===========================================================================

function TS7PlusConnection.CotpConnect:Boolean;
var
  BasePDU, Params, CallingTSAP, CalledTSAP, PDUSizeParam, CotpPDU, Frame:TBytes;
  TotalLen:Byte;
  IOResult1, IOResult2:TIOPacket;
  res:LongInt;
  RemoteTSAPBytes:TBytes;
  i, RespLen:Integer;
begin
  Result := false;
  FConnected := false;
  if (FCommPort=nil) or (not FCommPort.ReallyActive) then exit;

  //-- Calling (local) TSAP: fixed 2-byte value $0600.
  CallingTSAP := BytesOf([$C1, 2, Hi(S7PlusLocalTSAP), Lo(S7PlusLocalTSAP)]);

  //-- Called (remote) TSAP: fixed 16-byte ASCII string "SIMATIC-ROOT-HMI".
  SetLength(RemoteTSAPBytes, Length(S7PlusRemoteTSAP));
  for i:=1 to Length(S7PlusRemoteTSAP) do
    RemoteTSAPBytes[i-1] := Byte(S7PlusRemoteTSAP[i]);
  CalledTSAP := BytesConcat(BytesOf([$C2, Length(RemoteTSAPBytes)]), RemoteTSAPBytes);

  //-- Requested TPDU size: code $0A = 1024 bytes.
  PDUSizeParam := BytesOf([$C0, 1, $0A]);

  Params := BytesConcat(BytesConcat(CallingTSAP, CalledTSAP), PDUSizeParam);

  //-- COTP CR base: PDU type + dst-ref(0) + src-ref + class/option(0).
  BasePDU := BytesOf([$E0, 0,0, Hi(FSrcRef),Lo(FSrcRef), $00]);
  TotalLen := 6 + Length(Params);

  CotpPDU := BytesConcat(BytesConcat(BytesOf([TotalLen]), BasePDU), Params);

  SetLength(Frame, 4+Length(CotpPDU));
  Frame[0] := 3; Frame[1] := 0;
  Frame[2] := Hi(Word(Length(Frame)));
  Frame[3] := Lo(Word(Length(Frame)));
  Move(CotpPDU[0], Frame[4], Length(CotpPDU));

  Debug('COTP CR >> '+S7PlusHexStr(Frame));

  res := FCommPort.IOCommandSync(iocWriteRead, Length(Frame), Frame, 4, FDriverID, 0, @IOResult1);
  if (res=0) or (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>4) then begin
    Debug(Format('COTP CC << falha ao ler cabecalho TPKT (res=%d ioresult=%d received=%d)',[res,Ord(IOResult1.ReadIOResult),IOResult1.Received]));
    exit;
  end;

  RespLen := IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
  if RespLen<4 then begin
    Debug(Format('COTP CC << tamanho TPKT invalido (%d)',[RespLen]));
    exit;
  end;

  res := FCommPort.IOCommandSync(iocRead, 0, nil, RespLen-4, FDriverID, 0, @IOResult2);
  if (res=0) or (IOResult2.ReadIOResult<>iorOK) or (IOResult2.Received<>Cardinal(RespLen-4)) then begin
    Debug(Format('COTP CC << falha ao ler corpo (res=%d ioresult=%d received=%d esperado=%d)',[res,Ord(IOResult2.ReadIOResult),IOResult2.Received,RespLen-4]));
    exit;
  end;

  Debug('COTP CC << '+S7PlusHexStr(IOResult1.BufferToRead)+' '+S7PlusHexStr(IOResult2.BufferToRead));

  //-- Response must be a COTP Connection Confirm ($D0).
  if (Length(IOResult2.BufferToRead)<2) or (IOResult2.BufferToRead[1]<>$D0) then begin
    Debug('COTP CC << PDU nao e Connection Confirm ($D0)');
    exit;
  end;

  Result := true;
  FConnected := true;
  Debug('COTP conectado.');
end;

function TS7PlusConnection.TransportSend(const Frame:TBytes):Boolean;
var
  Msg:TBytes;
  res:LongInt;
begin
  Result := false;
  if (FCommPort=nil) or (not FCommPort.ReallyActive) then exit;

  //-- COTP Data-Transfer header (3 bytes: len=2, type=$F0, EOT+seq=$80) + TPKT header (4 bytes).
  SetLength(Msg, 7+Length(Frame));
  Msg[4] := $02; Msg[5] := $F0; Msg[6] := $80;
  if Length(Frame)>0 then
    Move(Frame[0], Msg[7], Length(Frame));
  Msg[0] := 3; Msg[1] := 0;
  Msg[2] := Hi(Word(Length(Msg)));
  Msg[3] := Lo(Word(Length(Msg)));

  Debug('TPKT/COTP-DT >> '+S7PlusHexStr(Msg));

  res := FCommPort.IOCommandSync(iocWrite, Length(Msg), Msg, 0, FDriverID, 0, nil);
  Result := res<>0;
  if not Result then
    Debug('TPKT/COTP-DT >> falha ao escrever no socket');
end;

function TS7PlusConnection.TransportRecv(out Frame:TBytes):TIOResult;
var
  IOResult1, IOResult2:TIOPacket;
  res, len:LongInt;
begin
  SetLength(Frame, 0);
  Result := iorNotReady;

  res := FCommPort.IOCommandSync(iocRead, 0, nil, 7, FDriverID, 0, @IOResult1);
  if res=0 then begin
    Debug('TPKT/COTP-DT << falha ao ler cabecalho (porta indisponivel)');
    exit;
  end;
  if (IOResult1.ReadIOResult<>iorOK) or (IOResult1.Received<>7) then begin
    Result := IOResult1.ReadIOResult;
    Debug(Format('TPKT/COTP-DT << falha ao ler cabecalho (ioresult=%d received=%d)',[Ord(IOResult1.ReadIOResult),IOResult1.Received]));
    exit;
  end;

  len := IOResult1.BufferToRead[2]*$100 + IOResult1.BufferToRead[3];
  if len<=7 then begin
    Result := iorOK;
    Debug('TPKT/COTP-DT << frame vazio (len<=7)');
    exit;
  end;

  res := FCommPort.IOCommandSync(iocRead, 0, nil, len-7, FDriverID, 0, @IOResult2);
  if res=0 then begin
    Debug('TPKT/COTP-DT << falha ao ler corpo (porta indisponivel)');
    exit;
  end;
  if (IOResult2.ReadIOResult<>iorOK) or (IOResult2.Received<>Cardinal(len-7)) then begin
    Result := IOResult2.ReadIOResult;
    Debug(Format('TPKT/COTP-DT << falha ao ler corpo (ioresult=%d received=%d esperado=%d)',[Ord(IOResult2.ReadIOResult),IOResult2.Received,len-7]));
    exit;
  end;

  SetLength(Frame, len-7);
  if Length(Frame)>0 then
    Move(IOResult2.BufferToRead[0], Frame[0], Length(Frame));
  Result := iorOK;
  Debug('TPKT/COTP-DT << '+S7PlusHexStr(Frame));
end;

//===========================================================================
// TLS tunnel (V2+) - TLS records travel as the payload of plain COTP DT frames;
// TPKT/COTP framing itself is never encrypted. Uses a memory-BIO pair so OpenSSL
// never touches the socket directly - every byte it wants to send/receive is
// pumped through TransportSend/TransportRecv above.
//===========================================================================

procedure TS7PlusConnection.TLSFlushOutgoing;
var
  Buf:TBytes;
  Pending, N:clong;
begin
  Pending := BIO_ctrl(FWriteBIO, BIO_CTRL_PENDING, 0, nil);
  while Pending>0 do begin
    SetLength(Buf, Pending);
    N := BIO_read(FWriteBIO, @Buf[0], Pending);
    if N>0 then begin
      SetLength(Buf, N);
      TransportSend(Buf);
    end;
    Pending := BIO_ctrl(FWriteBIO, BIO_CTRL_PENDING, 0, nil);
  end;
end;

function TS7PlusConnection.TLSReadIncoming:Boolean;
var
  Buf:TBytes;
begin
  Result := TransportRecv(Buf)=iorOK;
  if Result and (Length(Buf)>0) then
    BIO_write(FReadBIO, @Buf[0], Length(Buf))
  else
    Result := false;
end;

function TS7PlusConnection.ActivateTLS:Boolean;
var
  Ret, Err:cint;
  Retries:Integer;
begin
  Result := false;

  if not S7PlusSSLLoad then begin
    Debug('ActivateTLS: '+S7PlusSSLLoadError);
    exit;
  end;

  FSSLCtx := SSL_CTX_new(TLS_client_method());
  if FSSLCtx=nil then begin
    Debug('ActivateTLS: SSL_CTX_new falhou');
    S7PlusSSLUnload;
    exit;
  end;

  //Matches the cipher/group/option set known to be accepted by S7-1200/1500 firmware
  //(TIA Portal V15+): TLS >=1.2, ECDHE/AES-GCM preferred, EC groups restricted to
  //P-256/X25519/P-384 (the PLC RSTs the connection on unsupported groups like X448/
  //ffdhe*). P-256 listed FIRST: OpenSSL only pre-generates a proactive TLS 1.3 key_share
  //for the first group in this list (the rest are only advertised in supported_groups,
  //needing a HelloRetryRequest to actually get used) - confirmed against a S7-1513 FW
  //2.9.2 that RSTs the connection right after ClientHello when the only key_share offered
  //is X25519 (its embedded TLS stack apparently doesn't do HelloRetryRequest and needs a
  //key_share it can use immediately). P-256 is supported near-universally, including by
  //that older firmware, so putting it first keeps both old and new firmware happy without
  //needing a second round trip.
  S7PlusSSLCtxSetMinProtoVersion(FSSLCtx, TLS1_2_VERSION);
  SSL_CTX_set_cipher_list(FSSLCtx,
    'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:'+
    'AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256');
  S7PlusSSLCtxSet1GroupsList(FSSLCtx, 'P-256:X25519:P-384');
  SSL_CTX_set_options(FSSLCtx, SSL_OP_NO_TICKET or SSL_OP_NO_ENCRYPT_THEN_MAC or SSL_OP_NO_EXTENDED_MASTER_SECRET);
  //No CA/device certificate provisioned yet (out of scope for now) - accept whatever
  //certificate the PLC presents, same as the reference implementation without tls_ca.
  SSL_CTX_set_verify(FSSLCtx, SSL_VERIFY_NONE, nil);

  FSSL := SSL_new(FSSLCtx);
  if FSSL=nil then begin
    Debug('ActivateTLS: SSL_new falhou');
    SSL_CTX_free(FSSLCtx); FSSLCtx := nil;
    exit;
  end;

  FReadBIO  := BIO_new(BIO_s_mem());
  FWriteBIO := BIO_new(BIO_s_mem());
  SSL_set_bio(FSSL, FReadBIO, FWriteBIO); //FSSL now owns both BIOs

  SSL_set_connect_state(FSSL);

  Debug('ActivateTLS: iniciando handshake TLS (tunelado em frames COTP)...');

  Retries := 0;
  repeat
    Ret := SSL_do_handshake(FSSL);
    if Ret=1 then begin
      TLSFlushOutgoing;
      break;
    end;

    Err := SSL_get_error(FSSL, Ret);
    if Err=SSL_ERROR_WANT_READ then begin
      TLSFlushOutgoing;
      if not TLSReadIncoming then begin
        Debug('ActivateTLS: falha ao ler frame COTP durante o handshake');
        ReleaseTLS;
        exit;
      end;
    end else if Err=SSL_ERROR_WANT_WRITE then
      TLSFlushOutgoing
    else begin
      Debug(Format('ActivateTLS: handshake falhou (SSL_get_error=%d, ret=%d)',[Err,Ret]));
      ReleaseTLS;
      exit;
    end;

    inc(Retries);
  until Retries>200; //generous cap - a real handshake takes a handful of round-trips

  if Retries>200 then begin
    Debug('ActivateTLS: handshake nao terminou dentro do limite de tentativas');
    ReleaseTLS;
    exit;
  end;

  FTLSActive := true;
  Debug(Format('ActivateTLS: OK - versao=%s cipher=%s',
               [SSL_get_version(FSSL), SSL_CIPHER_get_name(SSL_get_current_cipher(FSSL))]));
  Result := true;
end;

procedure TS7PlusConnection.ReleaseTLS;
begin
  if FSSL<>nil then begin
    SSL_free(FSSL); //also frees FReadBIO/FWriteBIO (ownership transferred by SSL_set_bio)
    FSSL := nil;
    FReadBIO := nil;
    FWriteBIO := nil;
  end;
  if FSSLCtx<>nil then begin
    SSL_CTX_free(FSSLCtx);
    FSSLCtx := nil;
  end;
  if FTLSActive then
    S7PlusSSLUnload;
  FTLSActive := false;
end;

function TS7PlusConnection.LogicalSend(const Frame:TBytes):Boolean;
var
  N:cint;
begin
  if not FTLSActive then begin
    Result := TransportSend(Frame);
    exit;
  end;

  Result := false;
  if Length(Frame)=0 then exit;

  N := SSL_write(FSSL, @Frame[0], Length(Frame));
  TLSFlushOutgoing;
  Result := N=Length(Frame);
  if not Result then
    Debug(Format('LogicalSend: SSL_write retornou %d (esperado %d)',[N,Length(Frame)]));
end;

function TS7PlusConnection.LogicalRecv(out Frame:TBytes):TIOResult;
var
  Buf:array[0..65535] of Byte;
  N, Err:cint;
  Retries:Integer;
begin
  SetLength(Frame, 0);
  if not FTLSActive then begin
    Result := TransportRecv(Frame);
    exit;
  end;

  Result := iorNotReady;
  Retries := 0;
  repeat
    N := SSL_read(FSSL, @Buf[0], SizeOf(Buf));
    if N>0 then begin
      SetLength(Frame, N);
      Move(Buf[0], Frame[0], N);
      Result := iorOK;
      Debug(Format('LogicalRecv: SSL_read decifrou %d bytes: ',[N])+S7PlusHexStr(Frame));
      exit;
    end;

    Err := SSL_get_error(FSSL, N);
    if Err=SSL_ERROR_WANT_READ then begin
      if not TLSReadIncoming then begin
        Debug('LogicalRecv: falha ao ler frame COTP');
        Result := iorTimeOut;
        exit;
      end;
    end else begin
      Debug(Format('LogicalRecv: SSL_read falhou (SSL_get_error=%d)',[Err]));
      Result := iorNotReady;
      exit;
    end;

    inc(Retries);
  until Retries>200;

  Debug('LogicalRecv: excedeu o limite de tentativas de leitura TLS');
end;

function TS7PlusConnection.EnsureBuffered(N:Integer):Boolean;
var
  Chunk:TBytes;
begin
  Result := true;
  while Length(FRecvBuf)<N do begin
    if LogicalRecv(Chunk)<>iorOK then begin
      Result := false;
      exit;
    end;
    if Length(Chunk)=0 then begin
      Result := false;
      exit;
    end;
    FRecvBuf := BytesConcat(FRecvBuf, Chunk);
  end;
end;

function TS7PlusConnection.ReassembledRecv(out Data:TBytes):Boolean;
const
  MaxFragments = 4096;
  MaxTotalBytes = 16*1024*1024;
var
  FragLen:Integer;
  Fragments:Integer;
begin
  Result := false;
  SetLength(Data, 0);
  Fragments := 0;

  while true do begin
    Debug(Format('ReassembledRecv: buffer atual = %d bytes, pedindo 4 (cabecalho)',[Length(FRecvBuf)]));
    if not EnsureBuffered(4) then begin
      Debug('ReassembledRecv: falha ao ler cabecalho de fragmento');
      exit;
    end;
    if FRecvBuf[0]<>S7Plus_PROTOCOL_ID then begin
      Debug('ReassembledRecv: cabecalho de fragmento inesperado (nao comeca com $72): '+S7PlusHexStr(BytesCopy(FRecvBuf,0,4)));
      exit;
    end;

    //Note: the fragment's version byte (FRecvBuf[1]) is NOT checked here - continuation
    //fragments of a large reassembled response (e.g. Explore) can legitimately carry a
    //different version marker than the session's protocol version. The reference
    //implementation (python-snap7's _recv_reassembled_payload) only checks the leading
    //protocol id byte and otherwise treats every fragment identically, appending its
    //data to the accumulated payload regardless of the version byte. Special-casing an
    //unexpected version as an unsolicited notification to discard was found to
    //corrupt/truncate the reassembled payload - do not reintroduce that.
    FragLen := (FRecvBuf[2] shl 8) or FRecvBuf[3];
    FRecvBuf := BytesCopy(FRecvBuf, 4, Length(FRecvBuf)-4);
    Debug(Format('ReassembledRecv: cabecalho de fragmento: fragLen=%d (buffer apos remover cabecalho=%d bytes)',[FragLen, Length(FRecvBuf)]));

    if FragLen=0 then break; //standalone trailer (defensive)

    if not EnsureBuffered(FragLen) then begin
      Debug('ReassembledRecv: falha ao ler corpo do fragmento');
      exit;
    end;
    Data := BytesConcat(Data, BytesCopy(FRecvBuf, 0, FragLen));
    FRecvBuf := BytesCopy(FRecvBuf, FragLen, Length(FRecvBuf)-FragLen);
    Debug(Format('ReassembledRecv: fragmento %d consumido (%d bytes), total acumulado=%d, sobra no buffer=%d',
                 [Fragments+1, FragLen, Length(Data), Length(FRecvBuf)]));

    inc(Fragments);
    if (Fragments>MaxFragments) or (Length(Data)>MaxTotalBytes) then begin
      Debug('ReassembledRecv: resposta excede os limites de reassemblagem');
      exit;
    end;

    //Next 4 bytes are either the trailer ($72 ver 0000) or the next fragment's header.
    if not EnsureBuffered(4) then begin
      Debug('ReassembledRecv: falha ao ler cabecalho seguinte (nem trailer nem proximo fragmento chegaram)');
      exit;
    end;
    Debug('ReassembledRecv: proximos 4 bytes (trailer ou proximo cabecalho): '+S7PlusHexStr(BytesCopy(FRecvBuf,0,4)));
    if (FRecvBuf[0]=S7Plus_PROTOCOL_ID) and (FRecvBuf[2]=0) and (FRecvBuf[3]=0) then begin
      FRecvBuf := BytesCopy(FRecvBuf, 4, Length(FRecvBuf)-4); //consume trailer - last fragment
      Debug('ReassembledRecv: trailer encontrado, resposta completa.');
      break;
    end;
  end;

  Result := true;
end;

//===========================================================================
// S7CommPlus session handshake (V1/V2)
//===========================================================================

function TS7PlusConnection.InitSSL:Boolean;
var
  Seq:Word;
  Request, Frame, Response:TBytes;
  Version:Byte;
  DataLen:Word;
  Consumed:Integer;
begin
  Result := false;
  Seq := NextSequenceNumber;

  Request := EncodeRequestHeader(S7PlusFunc_InitSSL, Seq, 0, $30);
  Request := BytesConcat(Request, EncodeUInt32(0)); //trailing padding

  Frame := BytesConcat(EncodeS7PlusHeader(S7PlusVersion_V1, Length(Request)), Request);
  Frame := BytesConcat(Frame, EncodeS7PlusHeader(S7PlusVersion_V1, 0)); //trailer

  if not TransportSend(Frame) then exit;
  if TransportRecv(Response)<>iorOK then exit;
  if Length(Response)<4 then exit;

  Consumed := DecodeS7PlusHeader(Response, 0, Version, DataLen);
  if (Length(Response)-Consumed)<10 then exit; //InitSSL response too short

  Result := true;
  Debug(Format('InitSSL: version=V%d dataLen=%d',[Version,DataLen]));
end;

function TS7PlusConnection.CreateSession:Boolean;
var
  Seq:Word;
  Header, Body, Request, Frame, ResponseFrame, Response:TBytes;
  Version:Byte;
  DataLen:Word;
  Consumed, BodyOffset:Integer;
  RespBody, RawAttrBytes:TBytes;
  SessId:Cardinal;
  RetVal:QWord;
  HasSessId:Boolean;
  ObjCount, i, c:Integer;
  ObjId:Cardinal;
begin
  Result := false;
  Seq := NextSequenceNumber;

  Header := EncodeRequestHeader(S7PlusFunc_CreateObject, Seq, S7PlusObjId_ObjectNullServerSession, $36);

  Body := EncodeUInt32(S7PlusObjId_ObjectServerSessionContainer); //RequestId
  Body := BytesConcat(Body, BytesOf([$00, S7PlusType_UDINT]));
  Body := BytesConcat(Body, EncodeUInt32VLQ(0)); //RequestValue = ValueUDInt(0)
  Body := BytesConcat(Body, EncodeUInt32(0)); //unknown padding

  Body := BytesConcat(Body, BytesOf([S7PlusElement_StartOfObject]));
  Body := BytesConcat(Body, EncodeUInt32(S7PlusObjId_GetNewRIDOnServer)); //RelationId
  Body := BytesConcat(Body, EncodeUInt32VLQ(S7PlusObjId_ClassServerSession)); //ClassId
  Body := BytesConcat(Body, EncodeUInt32VLQ(0)); //ClassFlags
  Body := BytesConcat(Body, EncodeUInt32VLQ(0)); //AttributeId

  Body := BytesConcat(Body, BytesOf([S7PlusElement_Attribute]));
  Body := BytesConcat(Body, EncodeUInt32VLQ(S7PlusObjId_ServerSessionClientRID));
  Body := BytesConcat(Body, BytesOf([$00]));
  Body := BytesConcat(Body, EncodeTypedValueRID($80C3C901));

  Body := BytesConcat(Body, BytesOf([S7PlusElement_StartOfObject]));
  Body := BytesConcat(Body, EncodeUInt32(S7PlusObjId_GetNewRIDOnServer));
  Body := BytesConcat(Body, EncodeUInt32VLQ(S7PlusObjId_ClassSubscriptions));
  Body := BytesConcat(Body, EncodeUInt32VLQ(0)); //ClassFlags
  Body := BytesConcat(Body, EncodeUInt32VLQ(0)); //AttributeId
  Body := BytesConcat(Body, BytesOf([S7PlusElement_TerminatingObject]));

  Body := BytesConcat(Body, BytesOf([S7PlusElement_TerminatingObject]));
  Body := BytesConcat(Body, EncodeUInt32(0)); //trailing padding

  Request := BytesConcat(Header, Body);

  Frame := BytesConcat(EncodeS7PlusHeader(S7PlusVersion_V1, Length(Request)), Request);
  Frame := BytesConcat(Frame, EncodeS7PlusHeader(S7PlusVersion_V1, 0)); //trailer

  if not LogicalSend(Frame) then exit;
  if LogicalRecv(ResponseFrame)<>iorOK then exit;
  if Length(ResponseFrame)<4 then exit;

  Consumed := DecodeS7PlusHeader(ResponseFrame, 0, Version, DataLen);
  Debug(Format('CreateSession: PLC ofereceu version=V%d (antes de qualquer override)',[Version]));
  Response := BytesCopy(ResponseFrame, Consumed, DataLen);
  if Length(Response)<10 then exit; //CreateObject response too short

  RespBody := BytesCopy(Response, 10, Length(Response)-10);
  HasSessId := ParseCreateObjectSessionId(RespBody, SessId, BodyOffset, RetVal);

  if HasSessId then
    FSessionId := SessId
  else if Length(Response)>=13 then
    FSessionId := DecodeUInt32(Response, 9); //best-effort fallback, mirrors reference driver

  //ParseCreateObjectSessionId only keeps the FIRST of possibly several object ids in the
  //response - the request above creates the session AND (nested inside it) a
  //ClassSubscriptions container, so a successful response carries two: [0]=session,
  //[1]=that container. Re-walk the same list here (independently, since
  //ParseCreateObjectSessionId's own BodyOffset already points past all of them on return)
  //to also capture the second one - see FSubscriptionsContainerObjectId.
  FSubscriptionsContainerObjectId := 0;
  ObjCount := 0;
  if HasSessId then begin
    RetVal := DecodeUInt64VLQ(RespBody, 0, c); //re-skip ReturnValue the same way
    BodyOffset := c;
    if BodyOffset<Length(RespBody) then
      ObjCount := RespBody[BodyOffset]
    else
      ObjCount := 0;
    inc(BodyOffset);
    for i:=1 to ObjCount do begin
      ObjId := DecodeUInt32VLQ(RespBody, BodyOffset, c);
      BodyOffset := BodyOffset+c;
      if i=2 then FSubscriptionsContainerObjectId := ObjId;
    end;
  end;
  Debug(Format('CreateSession: ObjCount=%d sessionId=0x%.8x subscriptionsContainerObjectId=0x%.8x',
               [ObjCount, FSessionId, FSubscriptionsContainerObjectId]));

  FProtocolVersion := Version;
  FLastReturnValue := RetVal;

  //RetVal<>0 usually means the PLC requires TLS (not yet implemented here) - Connect will
  //still try SetupSession and report failure through SessionSetupOK.
  FServerSessionVersion := ParseServerSessionVersion(BytesCopy(Response, 10+BodyOffset, Length(Response)-(10+BodyOffset)));

  SetLength(FServerChallenge, 0);
  FServerFingerprint := '';
  if ParseAttributeRawValue(BytesCopy(Response, 10+BodyOffset, Length(Response)-(10+BodyOffset)),
                             S7PlusObjId_ServerCertificateFingerprint, RawAttrBytes) then
    FServerFingerprint := StringOf(RawAttrBytes);
  if ParseAttributeRawValue(BytesCopy(Response, 10+BodyOffset, Length(Response)-(10+BodyOffset)),
                             S7PlusObjId_ServerSessionRequest, RawAttrBytes) then
    FServerChallenge := RawAttrBytes;

  Debug(Format('CreateObject: version=V%d sessionId=0x%.8x returnValue=%d serverSessionVersionLen=%d fingerprint=%s challengeLen=%d',
               [Version, FSessionId, RetVal, Length(FServerSessionVersion), FServerFingerprint, Length(FServerChallenge)]));
  if Length(FServerSessionVersion)>0 then
    Debug('CreateObject: ServerSessionVersion = '+S7PlusHexStr(FServerSessionVersion));

  Result := true;
end;

//: Reads B[Offset..Offset+7] (stored little-endian, as every 8-byte key-id in the blob
//: is) and returns them reinterpreted as a big-endian QWord - matches a real TIA Portal
//: SetupSession capture byte-for-byte (see BuildAttr1830Value).
function QWordFromLEBytesReversed(const B:TBytes; Offset:Integer):QWord;
var i:Integer;
begin
  Result := 0;
  for i:=7 downto 0 do
    Result := (Result shl 8) or B[Offset+i];
end;

//: Builds the value for attribute 1830 (SessionSetupLegitimation) - decoded byte-for-byte
//: from a real TIA Portal V15 capture against this exact PLC (2026-08-30, see
//: SUBSCRIPTIONS_INVESTIGATION.md): a STRUCT(id=1800) with 5 members, not the flat byte
//: array/BLOB previously guessed (which the PLC accepted structurally but rejected with
//: an undocumented error). Two nested STRUCT(id=1825) members carry the public and
//: symmetric key identities as 64-bit ints - each is exactly the matching 8-byte KeyId
//: already embedded (little-endian) in the blob's own plaintext metadata header
//: (S7PlusHarpoBlobMeta.WriteMetadata: offset 32 for the server/public key, offset 16 for
//: the client/symmetric key), just reinterpreted big-endian. Confirmed identical across
//: 4 independent real sessions except for the client/symmetric key id (expected - a fresh
//: session key is generated every connection) and the blob itself.
function BuildAttr1830Value(const LegitBlob:TBytes; ServerKeyId:QWord; PublicKeyFlags:Cardinal):TBytes;
var
  ClientKeyId:QWord;
begin
  ClientKeyId := QWordFromLEBytesReversed(LegitBlob, 16);

  Result := BytesOf([$00, S7PlusType_STRUCT]);
  Result := BytesConcat(Result, EncodeUInt32(1800));

  Result := BytesConcat(Result, EncodeUInt32VLQ(1801));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_UDINT]));
  Result := BytesConcat(Result, EncodeUInt32VLQ(0));

  Result := BytesConcat(Result, EncodeUInt32VLQ(1802));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_USINT, 0]));

  Result := BytesConcat(Result, EncodeUInt32VLQ(1803));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_STRUCT]));
  Result := BytesConcat(Result, EncodeUInt32(1825));
  Result := BytesConcat(Result, EncodeUInt32VLQ(1826));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_ULINT]));
  Result := BytesConcat(Result, EncodeUInt64VLQ(ServerKeyId));
  Result := BytesConcat(Result, EncodeUInt32VLQ(1827));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_UDINT]));
  Result := BytesConcat(Result, EncodeUInt32VLQ(PublicKeyFlags));
  Result := BytesConcat(Result, EncodeUInt32VLQ(1828));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_UDINT]));
  Result := BytesConcat(Result, EncodeUInt32VLQ(0));
  Result := BytesConcat(Result, EncodeUInt32VLQ(0)); //terminator, struct 1825 (key 1803)

  Result := BytesConcat(Result, EncodeUInt32VLQ(1804));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_STRUCT]));
  Result := BytesConcat(Result, EncodeUInt32(1825));
  Result := BytesConcat(Result, EncodeUInt32VLQ(1826));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_ULINT]));
  Result := BytesConcat(Result, EncodeUInt64VLQ(ClientKeyId));
  Result := BytesConcat(Result, EncodeUInt32VLQ(1827));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_UDINT]));
  Result := BytesConcat(Result, EncodeUInt32VLQ(65537)); //fixed - identical in all 4 real sessions
  Result := BytesConcat(Result, EncodeUInt32VLQ(1828));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_UDINT]));
  Result := BytesConcat(Result, EncodeUInt32VLQ(0));
  Result := BytesConcat(Result, EncodeUInt32VLQ(0)); //terminator, struct 1825 (key 1804)

  Result := BytesConcat(Result, EncodeUInt32VLQ(1805));
  Result := BytesConcat(Result, EncodePValueBlob(LegitBlob));

  Result := BytesConcat(Result, EncodeUInt32VLQ(0)); //terminator, struct 1800
end;

function TS7PlusConnection.SetupSession:Boolean;
var
  Seq:Word;
  Header, Payload, Request, Frame, ResponseFrame, Response:TBytes;
  Version:Byte;
  DataLen:Word;
  Consumed:Integer;
  RespPayload:TBytes;
  RetVal:QWord;
  c, i:Integer;
  AttrIds:array of Cardinal;
  NeedsLegitimation:Boolean;
  Family:TS7PlusHarpoKeyFamily;
  KeyId:String;
  ServerKeyId:QWord;
  PublicKey, LegitBlob:TBytes;
begin
  Result := false;
  if Length(FServerSessionVersion)=0 then exit;

  //Confirmed live against a real S7-1513 FW2.9.2 (.211, no TLS): even though
  //ServerSessionVersion is present (which python-snap7's own reference assumes means
  //the legitimation handshake is NOT needed), this firmware still RSTs a SetupSession
  //that only echoes attribute 306 - it also expects attribute 1830 with the HarpoS7
  //SecurityKeyEncryptedKey blob, built from the fingerprint (attribute 233) and 20-byte
  //nonce (attribute 303, S7PlusObjId_ServerSessionRequest) CreateObject's response
  //already carried.
  //Gated on "not FTLSActive": .210 (real TLS active) ALSO sends the fingerprint/challenge
  //in CreateObject, but attempting this same legitimation there gets the TLS connection
  //dropped right after SetupSession is sent - regressing a path that was already fully
  //validated (session/browse/read/write, no legitimation needed). TLS already provides
  //its own server authentication (certificate), so the plaintext-only SessionKey
  //handshake is presumably specifically the substitute for that when TLS isn't used.
  NeedsLegitimation := (not FTLSActive) and (FServerFingerprint<>'') and (Length(FServerChallenge)=20);
  if NeedsLegitimation then begin
    try
      //S7PlusRandomBytes (used throughout the Harpo key/blob generation) silently
      //returns empty if OpenSSL isn't loaded yet - normally done by ActivateTLS, which
      //this path skips entirely when UseTLS=false.
      if not S7PlusSSLLoaded then begin
        if not S7PlusSSLLoad then
          raise Exception.Create('nao foi possivel carregar OpenSSL: '+S7PlusSSLLoadError);
      end;
      ParseFingerprint(FServerFingerprint, Family, KeyId);
      ServerKeyId := StrToQWord('$'+KeyId);
      PublicKey := GetPublicKey(FServerFingerprint);
      AuthenticateRealPlc(FServerChallenge, PublicKey, Family, LegitBlob, FHarpoSessionKey);
      FHarpoAuthPublicKey := PublicKey;
      FHarpoAuthFamily := Family;
      Debug(Format('SetupSession: blob de legitimacao construido (fingerprint=%s, %d bytes)',
                   [FServerFingerprint, Length(LegitBlob)]));
    except
      on E:Exception do begin
        Debug('SetupSession: falha ao construir blob de legitimacao: '+E.Message);
        NeedsLegitimation := false;
      end;
    end;
  end;

  Seq := NextSequenceNumber;
  //TransportFlags=$34 (not $36) - confirmed 2026-08-31 against a real, live-successful
  //SetupSession from python-snap7 (which hardcodes 0x34 here unconditionally, unlike its
  //own generic send_request's 0x34/0x36 branch): our .211 SetupSession kept getting
  //rejected with a "ServerSession"-class error even with an otherwise byte-perfect
  //attribute 1830 struct, and this was the one remaining difference from the working
  //Python request.
  Header := EncodeRequestHeader(S7PlusFunc_SetMultiVariables, Seq, FSessionId, $34);

  //Wire shape decoded byte-for-byte (2026-08-30) from a real TIA Portal V15 capture of a
  //SUCCESSFUL SetupSession against this same PLC (SUBSCRIPTIONS_INVESTIGATION.md): the
  //AttrIds are listed ALL UP FRONT (not interleaved as [AttrId,ItemNumber,Value] per
  //attribute, which is what every earlier guess here got wrong), THEN one
  //[ItemNumber VLQ][Value] pair per attribute, in the same order. ItemNumber is
  //per-attribute (1,2,...), not shared.
  //Attribute 299 dropped 2026-08-31: the TIA capture above included it (as a literal
  //UDINT 1), but python-snap7's own live-successful SetupSession against this same PLC
  //never sends it at all (just [1830,306], ItemCount=2) - apparently optional/ignored by
  //the PLC either way; omitted here to match the reference actually exercised live.
  if NeedsLegitimation then begin
    SetLength(AttrIds, 2);
    AttrIds[0] := S7PlusObjId_SessionSetupLegitimation;
    AttrIds[1] := S7PlusObjId_ServerSessionVersion;
  end else begin
    SetLength(AttrIds, 1);
    AttrIds[0] := S7PlusObjId_ServerSessionVersion;
  end;

  Payload := EncodeUInt32(FSessionId); //InObjectId
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(Length(AttrIds))); //item count
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(Length(AttrIds))); //field count
  for i:=0 to High(AttrIds) do
    Payload := BytesConcat(Payload, EncodeUInt32VLQ(AttrIds[i]));

  for i:=0 to High(AttrIds) do begin
    Payload := BytesConcat(Payload, EncodeUInt32VLQ(i+1)); //ItemNumber
    case AttrIds[i] of
      S7PlusObjId_SessionSetupLegitimation:
        Payload := BytesConcat(Payload, BuildAttr1830Value(LegitBlob, ServerKeyId,
                                           S7PlusHarpoBlobMeta.GetPublicKeyFlags(Family)));
      S7PlusObjId_ServerSessionVersion:
        Payload := BytesConcat(Payload, FServerSessionVersion); //echoed verbatim
      299:
        Payload := BytesConcat(Payload, BytesConcat(BytesOf([$00, S7PlusType_UDINT]), EncodeUInt32VLQ(1)));
    end;
  end;

  Payload := BytesConcat(Payload, BytesOf([$00])); //fill byte
  Payload := BytesConcat(Payload, EncodeObjectQualifier);
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  Request := BytesConcat(Header, Payload);

  Frame := BytesConcat(EncodeS7PlusHeader(FProtocolVersion, Length(Request)), Request);
  Frame := BytesConcat(Frame, EncodeS7PlusHeader(FProtocolVersion, 0)); //trailer

  if not LogicalSend(Frame) then exit;
  if LogicalRecv(ResponseFrame)<>iorOK then exit;
  if Length(ResponseFrame)<4 then exit;

  Consumed := DecodeS7PlusHeader(ResponseFrame, 0, Version, DataLen);
  Response := BytesCopy(ResponseFrame, Consumed, DataLen);
  if Length(Response)<10 then exit;

  RespPayload := BytesCopy(Response, 10, Length(Response)-10);
  if Length(RespPayload)=0 then exit;

  RetVal := DecodeUInt64VLQ(RespPayload, 0, c);
  FLastReturnValue := RetVal;
  Result := RetVal=0;
  if Result then
    Debug(Format('SetupSession: returnValue=%d -> %s',[RetVal, BoolToStr(Result,true)]))
  else
    Debug(Format('SetupSession: returnValue=%d (0x%.16x) -> False: %s',
                 [RetVal, RetVal, ExtractErrorText(BytesCopy(RespPayload, c, Length(RespPayload)-c))]));
end;

//: Ported from python-snap7's _session_activate (validated live against a real .211 -
//: matches TIA Portal frame 17 of TIAPortalWatchDB7.pcapng, GH-710): writes USINT(5) to
//: address 323 on the session object, using the V1-fixed-width ObjectQualifier
//: (KeyQualifier = the sequence number in use just before this request, not this
//: request's own - matches the reference's self._sequence_number read before
//: send_request internally advances it).
function TS7PlusConnection.GetVarSubstreamedRaw(InObjectId, Address, SeqField:Cardinal; out RawResp:TBytes):Boolean;
var
  Payload:TBytes;
begin
  Payload := EncodeUInt32(InObjectId);
  Payload := BytesConcat(Payload, BytesOf([$20, S7PlusType_UDINT]));
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(1)); //field count
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(Address));
  //KeyQualifier is fixed at 1 for every read in this TIA-observed post-legitimation
  //sequence (confirmed byte-for-byte across all 5 real reads in the capture, even
  //though seq_field itself increments 1..5) - NOT derived from FSequenceNumber, unlike
  //SessionActivate/PostAuthLegitimation's own ObjectQualifier usage.
  Payload := BytesConcat(Payload, EncodeObjectQualifierV1(1));
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(SeqField));
  Payload := BytesConcat(Payload, BytesOf([0,0,0])); //fill; IntegrityId splices before these

  Result := SendRequest(S7PlusFunc_GetVarSubstreamed, Payload, RawResp, 3);
end;

//: TIA Portal creates this object (inside the ClassSubscriptions container, same
//: CreateObject shape as SubscriptionCreate's own) right after the 5 reads in
//: PostLegitimationSessionInfo, before EXPLORE/GetMultiVariables are accepted - decoded
//: byte-for-byte from the same real TIA V15 capture (subscription_capture2.pcapng). Its
//: SubscriptionReferenceList isn't the normal per-variable one (SymbolCrc/AccessArea/
//: Lids) - it's some other fixed system reference whose exact meaning isn't understood
//: yet, so those 9 UDINT values are reproduced verbatim rather than derived.
function TS7PlusConnection.CreateChangeCounterObject:Boolean;
var
  Attrs:TS7PlusPObjectAttributeArray;
  ObjBytes, Payload, Resp:TBytes;
  RefListValues:array[0..8] of Cardinal;

  procedure AddAttr(AttrId:Cardinal; const Value:TBytes);
  var n:Integer;
  begin
    n := Length(Attrs);
    SetLength(Attrs, n+1);
    Attrs[n].AttrId := AttrId;
    Attrs[n].Value := Value;
  end;

begin
  RefListValues[0] := $80010000;
  RefListValues[1] := 0;
  RefListValues[2] := 1;
  RefListValues[3] := $80150001;
  RefListValues[4] := 1;
  RefListValues[5] := 1;
  RefListValues[6] := 3;
  RefListValues[7] := 0;
  RefListValues[8] := $9AF;

  SetLength(Attrs, 0);
  AddAttr(S7PlusIds_ObjectVariableTypeName, EncodeValuePWString('SPL_PlcProgram_ChangeCounter'));
  AddAttr(S7PlusObjId_SubscriptionFunctionClassId, EncodeValuePUSInt(1));
  AddAttr(S7PlusObjId_SubscriptionMissedSendings, EncodeValuePUInt(0));
  AddAttr(S7PlusObjId_SubscriptionSubsystemError, EncodeValuePLInt(0));
  AddAttr(S7PlusObjId_SubscriptionRouteMode, EncodeValuePUSInt(1));
  AddAttr(S7PlusObjId_SubscriptionActive, EncodeValuePBool(true));
  AddAttr(S7PlusObjId_SubscriptionReferenceList, EncodeValuePUDIntArray(RefListValues, $20));
  AddAttr(S7PlusObjId_SubscriptionCycleTime, EncodeValuePUDInt(0));
  AddAttr(S7PlusObjId_SubscriptionDisabled, EncodeValuePUSInt(0));
  AddAttr(S7PlusObjId_SubscriptionCount, EncodeValuePUSInt(0));
  AddAttr(S7PlusObjId_SubscriptionCreditLimit, EncodeValuePInt(-1));
  AddAttr(S7PlusObjId_SubscriptionTicks, EncodeValuePUInt(65535));
  AddAttr(1055, EncodeValuePUSInt(0));

  ObjBytes := EncodePObject(S7PlusObjId_GetNewRIDOnServer, S7PlusObjId_ClassSubscription, 0, 0, Attrs);

  Payload := EncodeUInt32(FSubscriptionsContainerObjectId); //InObjectId
  Payload := BytesConcat(Payload, EncodeValuePUDInt(0)); //RequestValue = ValueUDInt(0)
  //4 zero padding bytes + an incrementing "RequestId" byte before the object starts -
  //present verbatim in the real capture for this and the system Subscription_* objects
  //(4, then 5, 6, ...) but NOT for a normal user SubscriptionCreate (which only has the
  //4 zero bytes and no 5th byte); meaning unconfirmed, reproduced as observed.
  Payload := BytesConcat(Payload, BytesOf([0,0,0,0]));
  Payload := BytesConcat(Payload, BytesOf([Byte(FTiaSysObjectRequestId)]));
  Inc(FTiaSysObjectRequestId);
  Payload := BytesConcat(Payload, ObjBytes);
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  Result := SendRequest(S7PlusFunc_CreateObject, Payload, Resp, Length(ObjBytes)+4);
  if Result then
    Debug('CreateChangeCounterObject: OK')
  else
    Debug('CreateChangeCounterObject: falha');
end;

//: The two "Subscription_<n>" objects TIA creates right after ChangeCounter, both empty
//: (SubscriptionReferenceList = [changeCounterHead, 0 unsubscribe, 0 subscribe] - no
//: actual items) with different cycle/route/function-class/credit-limit parameters -
//: decoded byte-for-byte from the same real capture. Only the simpler of the two
//: (no nested sub-object) is reproduced here.
function TS7PlusConnection.CreateEmptySystemSubscription(CycleTimeMs:Cardinal; RouteMode:Byte;
                                                           FunctionClassId:Byte; CreditLimit:SmallInt):Boolean;
var
  Attrs:TS7PlusPObjectAttributeArray;
  ObjBytes, Payload, Resp:TBytes;
  RefListValues:array[0..2] of Cardinal;

  procedure AddAttr(AttrId:Cardinal; const Value:TBytes);
  var n:Integer;
  begin
    n := Length(Attrs);
    SetLength(Attrs, n+1);
    Attrs[n].AttrId := AttrId;
    Attrs[n].Value := Value;
  end;

begin
  RefListValues[0] := $80010000;
  RefListValues[1] := 0;
  RefListValues[2] := 0;

  SetLength(Attrs, 0);
  AddAttr(S7PlusIds_ObjectVariableTypeName, EncodeValuePWString('Subscription_'+IntToStr(FSubscriptionChangeCounter)));
  AddAttr(S7PlusObjId_SubscriptionFunctionClassId, EncodeValuePUSInt(FunctionClassId));
  AddAttr(S7PlusObjId_SubscriptionMissedSendings, EncodeValuePUInt(0));
  AddAttr(S7PlusObjId_SubscriptionSubsystemError, EncodeValuePLInt(0));
  AddAttr(S7PlusObjId_SubscriptionRouteMode, EncodeValuePUSInt(RouteMode));
  AddAttr(S7PlusObjId_SubscriptionActive, EncodeValuePBool(true));
  AddAttr(S7PlusObjId_SubscriptionReferenceList, EncodeValuePUDIntArray(RefListValues, $20));
  AddAttr(S7PlusObjId_SubscriptionCycleTime, EncodeValuePUDInt(CycleTimeMs));
  AddAttr(S7PlusObjId_SubscriptionDisabled, EncodeValuePUSInt(0));
  AddAttr(S7PlusObjId_SubscriptionCount, EncodeValuePUSInt(0));
  AddAttr(S7PlusObjId_SubscriptionCreditLimit, EncodeValuePInt(CreditLimit));
  AddAttr(S7PlusObjId_SubscriptionTicks, EncodeValuePUInt(65535));
  AddAttr(1055, EncodeValuePUSInt(0));

  ObjBytes := EncodePObject(S7PlusObjId_GetNewRIDOnServer, S7PlusObjId_ClassSubscription, 0, 0, Attrs);

  Payload := EncodeUInt32(FSubscriptionsContainerObjectId); //InObjectId
  Payload := BytesConcat(Payload, EncodeValuePUDInt(0)); //RequestValue = ValueUDInt(0)
  Payload := BytesConcat(Payload, BytesOf([0,0,0,0]));
  Payload := BytesConcat(Payload, BytesOf([Byte(FTiaSysObjectRequestId)]));
  Inc(FTiaSysObjectRequestId);
  Payload := BytesConcat(Payload, ObjBytes);
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  Result := SendRequest(S7PlusFunc_CreateObject, Payload, Resp, Length(ObjBytes)+4);
  if Result then
    Debug('CreateEmptySystemSubscription: OK')
  else
    Debug('CreateEmptySystemSubscription: falha');
end;

function TS7PlusConnection.GetLinkRaw(InObjectId, Attr:Cardinal; out RIDs:TS7PlusLIDArray):Boolean;
var
  Payload, Resp:TBytes;
  Offset, c, i:Integer;
  ItemCount:Cardinal;
begin
  Result := false;
  SetLength(RIDs, 0);
  Payload := EncodeUInt32(InObjectId);
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(Attr));
  Payload := BytesConcat(Payload, BytesOf([0,0]));
  Payload := BytesConcat(Payload, BytesOf([0,0,0])); //fill; IntegrityId splices before these
  if not SendRequest(S7PlusFunc_GetLink, Payload, Resp, 3) then exit;
  Offset := 0;
  ItemCount := DecodeUInt32VLQ(Resp, Offset, c);
  Offset := Offset+c;
  if Integer(ItemCount)<0 then exit;
  SetLength(RIDs, ItemCount);
  for i:=0 to Integer(ItemCount)-1 do begin
    if Offset+4>Length(Resp) then begin
      SetLength(RIDs, 0);
      exit;
    end;
    RIDs[i] := DecodeUInt32(Resp, Offset);
    Offset := Offset+4;
  end;
  Result := true;
end;

function TS7PlusConnection.GetMultiVariablesFields(InObjectId:Cardinal; const FieldIds:array of Cardinal; out Results:TS7PlusMultiReadResultArray):Boolean;
var
  Payload, Resp:TBytes;
  i:Integer;
begin
  Payload := EncodeUInt32(InObjectId);
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(Length(FieldIds)));
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(Length(FieldIds)));
  for i:=0 to High(FieldIds) do
    Payload := BytesConcat(Payload, EncodeUInt32VLQ(FieldIds[i]));
  Payload := BytesConcat(Payload, CurrentObjectQualifier);
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding
  Result := SendRequest(S7PlusFunc_GetMultiVariables, Payload, Resp);
  if not Result then exit;
  Result := ParseMultiReadResponse(Resp, Length(FieldIds), Results);
end;

//: DB4's live fingerprint (13 bytes) - read out of field 9505 off the RID that
//: GetLink(11,2699) resolves, at a fixed offset (skipping that field's own 4-byte
//: "40 <size> 32 <count>"-shaped preamble). Confirmed byte-for-byte identical between
//: this read and the same 13 bytes embedded in 2 different real TisMonitorJob captures
//: (21 and 23 monitored items respectively) - see CreateTisMonitorJob.
function TS7PlusConnection.GetDB4Fingerprint(out Fingerprint:TBytes):Boolean;
const
  FieldIds:array[0..4] of Cardinal = (9500, 9501, 9506, 9505, 9507);
var
  RIDs:TS7PlusLIDArray;
  Results:TS7PlusMultiReadResultArray;
begin
  Result := false;
  SetLength(Fingerprint, 0);
  if not GetLinkRaw(11, 2699, RIDs) or (Length(RIDs)=0) then begin
    Debug('GetDB4Fingerprint: falha no GetLink(11,2699)');
    exit;
  end;
  if not GetMultiVariablesFields(RIDs[0], FieldIds, Results) then begin
    Debug('GetDB4Fingerprint: falha ao ler os campos do objeto de tipo');
    exit;
  end;
  if (Length(Results)<4) or (not Results[3].Ok) or (Length(Results[3].Data)<17) then begin
    Debug('GetDB4Fingerprint: campo 9505 ausente ou curto demais');
    exit;
  end;
  Fingerprint := Copy(Results[3].Data, 4, 13);
  Result := true;
end;

function TS7PlusConnection.CreateTisMonitorJob(const Lids:array of Cardinal; out JobRID:Cardinal):Boolean;
var
  Fingerprint, BlobData, Payload, Resp, ObjBytes:TBytes;
  Attrs:TS7PlusPObjectAttributeArray;
  N, i, Offs, TotalLen:Integer;
  RelId:Cardinal;
begin
  Result := false;
  JobRID := 0;
  if not GetDB4Fingerprint(Fingerprint) then begin
    Debug('CreateTisMonitorJob: falha ao obter a fingerprint da DB');
    exit;
  end;

  N := Length(Lids);
  TotalLen := 20 + 4*N; //ResponsePrefix(20) + N x 4-byte values
  SetLength(BlobData, 0);
  BlobData := BytesConcat(BlobData, BytesOf([$40, Byte(TotalLen+20)]));
  BlobData := BytesConcat(BlobData, BytesOf([$32, $01]));
  BlobData := BytesConcat(BlobData, Fingerprint);
  BlobData := BytesConcat(BlobData, BytesOf([$80, $10]));
  for i:=0 to N-1 do begin
    Offs := 20 + i*4;
    BlobData := BytesConcat(BlobData, BytesOf([Byte(Lids[i]+$58)]));
    BlobData := BytesConcat(BlobData, BytesOf([$04,$03,$F0,$00,$05,$D0,$12]));
    BlobData := BytesConcat(BlobData, BytesOf([Byte(TotalLen-Offs)]));
    BlobData := BytesConcat(BlobData, BytesOf([$80, Byte(Offs)]));
  end;

  SetLength(Attrs, 2);
  Attrs[0].AttrId := S7PlusIds_ObjectVariableTypeName;
  Attrs[0].Value := EncodeValuePWString('TisMonitorJob_'+IntToStr(FTiaSysObjectRequestId));
  Attrs[1].AttrId := 2693;
  Attrs[1].Value := EncodePValueBlob(BlobData);

  RelId := $7FFFC000 + FTiaSysObjectRequestId;
  ObjBytes := EncodePObject(RelId, 2709, 0, 0, Attrs);

  Payload := EncodeUInt32(FSubscriptionsContainerObjectId);
  Payload := BytesConcat(Payload, EncodeValuePUDInt(0));
  Payload := BytesConcat(Payload, BytesOf([0,0,0,0]));
  Payload := BytesConcat(Payload, BytesOf([Byte(FTiaSysObjectRequestId)]));
  Inc(FTiaSysObjectRequestId);
  Payload := BytesConcat(Payload, ObjBytes);
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  Result := SendRequest(S7PlusFunc_CreateObject, Payload, Resp, Length(ObjBytes)+4);
  if Result then begin
    if Length(Resp)>=8 then
      JobRID := DecodeUInt32(Resp, Length(Resp)-8); //best-effort, verified against live response
    Debug(Format('CreateTisMonitorJob: OK jobRID=0x%.8x',[JobRID]));
  end else
    Debug('CreateTisMonitorJob: falha');
end;

function TS7PlusConnection.PostLegitimationSessionInfo:Boolean;
var
  Resp:TBytes;
begin
  Result := false;
  if not GetVarSubstreamedRaw(FSessionId, 300, 1, Resp) then exit;
  if not GetVarSubstreamedRaw($31, 3753, 2, Resp) then exit;
  if not GetVarSubstreamedRaw(FSessionId, 1842, 3, Resp) then exit;
  if not GetVarSubstreamedRaw(FSessionId, 300, 4, Resp) then exit;
  if not GetVarSubstreamedRaw(FSessionId, 1842, 5, Resp) then exit;
  FTiaSysObjectRequestId := 4;
  if not CreateChangeCounterObject then exit;
  if not GetVarSubstreamedRaw(FSessionId, 300, 6, Resp) then exit;
  if not GetVarSubstreamedRaw(FSessionId, 1842, 7, Resp) then exit;
  if not CreateEmptySystemSubscription(900, 1, 3, -1) then exit;
  Result := true;
  Debug('PostLegitimationSessionInfo: OK');
end;

function TS7PlusConnection.SessionActivate:Boolean;
var
  Payload, Resp:TBytes;
begin
  Payload := EncodeUInt32(FSessionId);
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(1)); //AddressCount
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(323));
  Payload := BytesConcat(Payload, BytesOf([$00, S7PlusType_USINT]));
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(5));
  Payload := BytesConcat(Payload, EncodeObjectQualifierV1(FSequenceNumber));
  Payload := BytesConcat(Payload, BytesOf([0,0,0])); //trailing zeros; IntegrityId splices before these

  Result := SendRequest(S7PlusFunc_SetVariable, Payload, Resp, 3);
  if Result then
    Debug('SessionActivate: OK')
  else
    Debug('SessionActivate: falha ao enviar/receber');
end;

//: Ported from python-snap7's _post_auth_legitimation (validated live against a real
//: .211): re-reads the 20-byte challenge from address 303 (a fresh one, distinct from
//: the one CreateObject carried), solves it via S7PlusHarpoLegitimate, writes the
//: 248-byte solved blob to address 1846. Required before the PLC allows any data
//: operation on a session that went through the SecurityKey blob exchange.
function TS7PlusConnection.PostAuthLegitimation(const Password:AnsiString):Boolean;
var
  Payload, Resp, LegitChallenge, LegitBlob, Svs:TBytes;
  Offset_, c:Integer;
  RetVal:QWord;
  Len_:Cardinal;
begin
  Result := false;

  //Step 1: re-read the legitimation challenge from address 303.
  Payload := EncodeUInt32(FSessionId);
  Payload := BytesConcat(Payload, BytesOf([$20, S7PlusType_UDINT]));
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(1)); //field count
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(S7PlusObjId_ServerSessionRequest));
  Payload := BytesConcat(Payload, EncodeObjectQualifierV1(FSequenceNumber));
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(1)); //seq_field
  Payload := BytesConcat(Payload, BytesOf([0,0,0])); //fill

  if not SendRequest(S7PlusFunc_GetVarSubstreamed, Payload, Resp, 3) then begin
    Debug('PostAuthLegitimation: falha ao ler desafio (endereco 303)');
    exit;
  end;

  //Response shape, AFTER SendRequest's own IntegrityId strip (2026-08-31 fix - see its
  //comment): ReturnValue(VLQ) + Flags(1) + DataType(1) + Length(VLQ) + data. python-
  //snap7's own inline ad-hoc parser here has a real bug (double-counts IntegrityId,
  //since its generic send_request ALSO already strips it) that happens to fail its own
  //"length==20" sanity check and silently fall back to the session's original challenge;
  //traced live 2026-08-31 against a real .211 that this re-read DOES return a genuinely
  //different 20-byte value than the original, and using this correctly-parsed one - not
  //the original - is what the PLC actually expects here.
  LegitChallenge := nil;
  Offset_ := 0;
  DecodeUInt64VLQ(Resp, Offset_, c); Inc(Offset_, c); //ReturnValue
  if Offset_+2<=Length(Resp) then begin
    Inc(Offset_, 2); //Flags, DataType (already known to be $10/USINT array here)
    Len_ := DecodeUInt32VLQ(Resp, Offset_, c); Inc(Offset_, c);
    if (Len_=20) and (Offset_+Integer(Len_)<=Length(Resp)) then
      LegitChallenge := BytesCopy(Resp, Offset_, 20);
  end;

  if Length(LegitChallenge)<20 then begin
    Debug('PostAuthLegitimation: nao foi possivel decodificar o desafio lido');
    exit;
  end;
  //Step 2: solve the challenge.
  if (Length(FHarpoSessionKey)=0) or (Length(FHarpoAuthPublicKey)=0) then begin
    Debug('PostAuthLegitimation: sem session key / chave publica (SetupSession nao rodou a legitimacao)');
    exit;
  end;
  try
    LegitBlob := SolveLegitimateChallengeRealPlc(LegitChallenge, FHarpoAuthPublicKey, FHarpoAuthFamily,
                                                  FHarpoSessionKey, Password);
  except
    on E:Exception do begin
      Debug('PostAuthLegitimation: falha ao resolver desafio: '+E.Message);
      exit;
    end;
  end;

  //Step 3: write the solved blob to address 1846 (S7PlusObjId_Legitimate).
  Svs := EncodeUInt32(FSessionId);
  Svs := BytesConcat(Svs, BytesOf([$20, S7PlusType_UDINT]));
  Svs := BytesConcat(Svs, EncodeUInt32VLQ(1));
  Svs := BytesConcat(Svs, EncodeUInt32VLQ(S7PlusObjId_Legitimate));
  Svs := BytesConcat(Svs, EncodeObjectQualifierV1(FSequenceNumber));
  Svs := BytesConcat(Svs, EncodeUInt32VLQ(1));
  Svs := BytesConcat(Svs, EncodePValueBlob(LegitBlob));
  Svs := BytesConcat(Svs, EncodeUInt32VLQ(FSequenceNumber));
  Svs := BytesConcat(Svs, BytesOf([0,0,0]));

  if not SendRequest(S7PlusFunc_SetVarSubstreamed, Svs, Resp, 3) then begin
    Debug('PostAuthLegitimation: falha ao enviar blob resolvido (endereco 1846)');
    exit;
  end;

  if Length(Resp)>=1 then begin
    RetVal := DecodeUInt64VLQ(Resp, 0, c);
    if (RetVal and QWord($8000000000000000))<>0 then begin
      Debug(Format('PostAuthLegitimation: rejeitado pela PLC (returnValue=0x%.16x)',[RetVal]));
      exit;
    end;
  end;

  Result := true;
  Debug('PostAuthLegitimation: OK');
end;

//===========================================================================
// Public connect/disconnect
//===========================================================================

function TS7PlusConnection.Connect(const Password:AnsiString):Boolean;
begin
  Result := false;
  FSessionSetupOK := false;
  FWithIntegrityId := false;
  FIntegrityIdRead := 0;
  FIntegrityIdWrite := 0;

  if not CotpConnect then exit;
  if not InitSSL then exit; //always unencrypted - triggers the PLC to prepare for TLS

  if FUseTLS then begin
    if not ActivateTLS then begin
      Debug('Connect: falha ao ativar TLS - abortando conexao.');
      exit;
    end;
  end;

  if not CreateSession then exit; //frame header still says V1; TLS (if active) already wraps it

  //Previously gated on FTLSActive ("once TLS is active, subsequent PDUs use V2"), matching
  //both available references (C# and python-snap7) - but confirmed WRONG against a real
  //S7-1513 FW2.9.2/TIA Portal V15 capture: TIA escalates to V2 for SetupSession right after
  //CreateObject regardless of whether TLS is active (that capture is fully plaintext, no TLS
  //records at all, yet SetupSession's S7+ header already says V2). The escalation is tied to
  //CreateObject having succeeded, not to TLS.
  FProtocolVersion := S7PlusVersion_V2;

  if Length(FServerSessionVersion)>0 then
    FSessionSetupOK := SetupSession
  else begin
    FSessionSetupOK := false; //older FW band (Struct-314 SessionKey handshake) - not implemented yet
    Debug('Connect: PLC nao enviou ServerSessionVersion escalar - SetupSession pulado (SessionSetupOK=false).');
  end;

  if FProtocolVersion>=S7PlusVersion_V3 then begin
    if not FTLSActive then
      Debug('Connect: PLC negociou V3 mas TLS nao esta ativo - a conexao pode nao funcionar.');
  end else if FProtocolVersion=S7PlusVersion_V2 then begin
    //Previously aborted here if TLS wasn't active ("V2 exige TLS") - also confirmed wrong by
    //the same TIA capture (V2 SetupSession succeeded, and IntegrityId-tracked reads/writes
    //kept working, entirely without TLS). IntegrityId is a plain per-read/write wire counter,
    //unrelated to encryption - no reason it would require TLS.
    FWithIntegrityId := true;
    FIntegrityIdRead := 0;
    FIntegrityIdWrite := 0;
    Debug('Connect: rastreamento de IntegrityId habilitado (V2).');
  end;

  FConnected := true;

  //V1-initial/no-TLS SecurityKey sessions (FHarpoSessionKey set by SetupSession) need two
  //more steps before the PLC allows any data operation - see SessionActivate/
  //PostAuthLegitimation's own comments. Ported from python-snap7's connect(): `if
  //self._session_key is not None and self._session_setup_ok: self._session_activate();
  //self._post_auth_legitimation(password)`.
  if FSessionSetupOK and (Length(FHarpoSessionKey)>0) then begin
    if not SessionActivate then
      Debug('Connect: falha na ativacao de sessao (SessionActivate) - operacoes de dados podem continuar bloqueadas.')
    else begin
      //TIA Portal never does this read(303)/write(1846) exchange at all (confirmed
      //2026-08-31 against a real TIA V15 capture: its first post-activation read already
      //uses IntegrityId(read)=0) - it authenticates via a different channel (a configured
      //communication certificate) that skips this entirely. Re-tested 2026-08-31: calling
      //PostAuthLegitimation here - even though the PLC accepts it and returns a positive
      //retval - poisons the session: the very same GetVarSubstreamedRaw(300) read that
      //succeeds when PostAuthLegitimation is skipped instead comes back with the exact
      //same FATAL SystemEvent as the later DB reads (member 0x9D71 = A2013B00029BFF88).
      //So PostAuthLegitimation must stay skipped entirely, matching TIA's own behavior.
      FIntegrityIdRead := 0;
      FIntegrityIdWrite := 0;
      if not PostLegitimationSessionInfo then
        Debug('Connect: falha na leitura de info pos-legitimacao - EXPLORE/leituras podem continuar bloqueadas.');
    end;
  end;

  Result := true;
  Debug(Format('Connect: OK - version=V%d sessionId=0x%.8x sessionSetupOK=%s tls=%s',
               [FProtocolVersion, FSessionId, BoolToStr(FSessionSetupOK,true), BoolToStr(FTLSActive,true)]));
end;

procedure TS7PlusConnection.Disconnect;
begin
  FConnected := false;
  FSessionSetupOK := false;
  FSessionId := 0;
  FSequenceNumber := 0;
  FWithIntegrityId := false;
  FIntegrityIdRead := 0;
  FIntegrityIdWrite := 0;
  SetLength(FServerSessionVersion, 0);
  ReleaseTLS;
end;

//===========================================================================
// Generic request/response
//===========================================================================

const
  S7PlusVersion_SystemEvent = $FE;
  S7PlusSystemEventReturnValueId = $9D71; //: 40305 - matches python-snap7's _SYSTEM_EVENT_RETURN_VALUE_ID.

function TS7PlusConnection.IsNonFatalSystemEvent(const Payload:TBytes):Boolean;
var
  Offset, c, MemberId:Integer;
  Flags, DataType:Byte;
  Size_:Integer;
  RawVal:QWord;
  Signed:Int64;
begin
  Result := true; //unrecognized/malformed shapes are treated as non-fatal, same as the reference falling through to "return None".
  if Length(Payload)<16 then exit;
  if Length(Payload)=16 then exit;
  if (Length(Payload)<20) or (DecodeUInt32(Payload, 16)<>Cardinal(S7PlusType_STRUCT)) then exit;

  Offset := 24;
  while Offset+4<=Length(Payload) do begin
    MemberId := DecodeUInt32(Payload, Offset);
    Inc(Offset, 4);
    if MemberId=0 then break;
    if Offset+4>Length(Payload) then exit;
    Flags := Payload[Offset+1];
    DataType := Payload[Offset+3];
    Inc(Offset, 4);
    case DataType of
      $01,$02,$06,$0A: Size_ := 1;
      $03,$07,$0B: Size_ := 2;
      $04,$08,$0C,$0E,$12,$13: Size_ := 4;
      $05,$09,$0D,$0F,$10,$11: Size_ := 8;
    else
      exit; //unknown scalar type - can't reliably keep walking, treat as non-fatal (best-effort)
    end;
    if Offset+Size_>Length(Payload) then exit;
    if MemberId=S7PlusSystemEventReturnValueId then begin
      RawVal := 0;
      for c:=0 to Size_-1 do RawVal := (RawVal shl 8) or Payload[Offset+c];
      if Size_<8 then begin
        Signed := Int64(RawVal);
        if (RawVal and (QWord(1) shl (Size_*8-1)))<>0 then
          Signed := Signed - (Int64(1) shl (Size_*8));
      end else
        Signed := Int64(RawVal);
      Result := Signed>=0;
      exit;
    end;
    Inc(Offset, Size_);
    if Flags=0 then; //silence unused-var warning - Flags isn't otherwise consulted, matching the reference
  end;
  //A Struct payload that never carries a ReturnValue member is fatal (matches the
  //reference: "S7CommPlus SystemEvent Struct has no ReturnValue").
  Result := false;
end;

function TS7PlusConnection.RecvApplicationFrame(out ResponseFrame:TBytes):Boolean;
const
  MaxSystemEvents = 8;
var
  Attempt, Consumed:Integer;
  Version:Byte;
  DataLen:Word;
  EventBody:TBytes;
begin
  Result := false;
  for Attempt:=1 to MaxSystemEvents+1 do begin
    if LogicalRecv(ResponseFrame)<>iorOK then exit;
    if Length(ResponseFrame)<4 then exit;
    Consumed := DecodeS7PlusHeader(ResponseFrame, 0, Version, DataLen);
    if Version<>S7PlusVersion_SystemEvent then begin
      Result := true;
      exit;
    end;
    EventBody := BytesCopy(ResponseFrame, Consumed, DataLen);
    if not IsNonFatalSystemEvent(EventBody) then begin
      Debug('RecvApplicationFrame: SystemEvent FATAL recebido: '+S7PlusHexStr(EventBody));
      exit;
    end;
    Debug(Format('RecvApplicationFrame: SystemEvent nao-fatal ignorado (%d bytes)',[Length(EventBody)]));
  end;
  Debug('RecvApplicationFrame: excesso de SystemEvents consecutivos');
end;

function TS7PlusConnection.SendRequest(FunctionCode:Word; const Payload:TBytes; out RespPayload:TBytes;
                                        IntegrityTail:Integer; Reassemble:Boolean):Boolean;
var
  Seq:Word;
  TransportFlags:Byte;
  Header, ActualPayload, Request, Frame, ResponseFrame, Response, IntegrityBytes, FrameData:TBytes;
  Version:Byte;
  DataLen:Word;
  Consumed, HashLen, c:Integer;
  IsReadFunc:Boolean;
begin
  Result := false;
  SetLength(RespPayload, 0);
  if not FConnected then exit;

  Seq := NextSequenceNumber;
  //After a successful SessionKey/legitimation handshake (FHarpoSessionKey<>nil), TIA
  //Portal uses transport flags 0x34 for every request, not just GetMultiVariables/Explore.
  if (Length(FHarpoSessionKey)>0) or (FunctionCode=S7PlusFunc_GetMultiVariables) or (FunctionCode=S7PlusFunc_Explore) then
    TransportFlags := $34
  else
    TransportFlags := $36;

  //V2+: the IntegrityId (a per read/write monotonic counter) is spliced into the payload
  //just before its trailing IntegrityTail bytes of padding (4 for GetMultiVariables/
  //SetMultiVariables, 5 for Explore - see BuildAreaPayload/Explore's request builder).
  IsReadFunc := IsS7PlusReadFunctionCode(FunctionCode);
  if FWithIntegrityId and (FProtocolVersion>=S7PlusVersion_V2) then begin
    if IsReadFunc then
      IntegrityBytes := EncodeUInt32VLQ(FIntegrityIdRead)
    else
      IntegrityBytes := EncodeUInt32VLQ(FIntegrityIdWrite);

    if Length(Payload)>=IntegrityTail then
      ActualPayload := BytesConcat(BytesConcat(BytesCopy(Payload,0,Length(Payload)-IntegrityTail), IntegrityBytes), BytesCopy(Payload,Length(Payload)-IntegrityTail,IntegrityTail))
    else
      ActualPayload := BytesConcat(IntegrityBytes, Payload);

    if IsReadFunc then
      Debug(Format('SendRequest: IntegrityId (read) = %d',[FIntegrityIdRead]))
    else
      Debug(Format('SendRequest: IntegrityId (write) = %d',[FIntegrityIdWrite]));
  end else
    ActualPayload := Payload;

  Header := EncodeRequestHeader(FunctionCode, Seq, FSessionId, TransportFlags);
  Request := BytesConcat(Header, ActualPayload);

  //Once a SessionKey/legitimation handshake has produced FHarpoSessionKey, TIA Portal
  //switches EVERY subsequent frame to V3 framing with a 32-byte HMAC-SHA256 digest
  //(computed over the whole request, keyed with the session key's first 24 bytes)
  //prepended before the request bytes - confirmed live 2026-08-31: SessionActivate's
  //plain V2 frame was accepted, but the PLC dropped the connection on the very next
  //(also plain V2) frame, matching python-snap7's send_request doing exactly this
  //switch unconditionally as soon as self._session_key is not None.
  if Length(FHarpoSessionKey)>0 then begin
    FrameData := BytesConcat(BytesOf([$20]), S7PlusHMACSHA256(Copy(FHarpoSessionKey,0,24), Request));
    FrameData := BytesConcat(FrameData, Request);
    Frame := BytesConcat(EncodeS7PlusHeader(S7PlusVersion_V3, Length(FrameData)), FrameData);
    Frame := BytesConcat(Frame, EncodeS7PlusHeader(S7PlusVersion_V3, 0)); //trailer
  end else begin
    Frame := BytesConcat(EncodeS7PlusHeader(FProtocolVersion, Length(Request)), Request);
    Frame := BytesConcat(Frame, EncodeS7PlusHeader(FProtocolVersion, 0)); //trailer
  end;

  Debug(Format('SendRequest: functionCode=$%.4x seq=%d',[FunctionCode,Seq])+' payload='+S7PlusHexStr(ActualPayload));

  if not LogicalSend(Frame) then begin
    Debug('SendRequest: falha ao enviar');
    exit;
  end;

  if FWithIntegrityId and (FProtocolVersion>=S7PlusVersion_V2) then begin
    if IsReadFunc then
      FIntegrityIdRead := (FIntegrityIdRead+1) and $FFFFFFFF
    else
      FIntegrityIdWrite := (FIntegrityIdWrite+1) and $FFFFFFFF;
  end;

  if Reassemble then begin
    if not ReassembledRecv(Response) then begin
      Debug('SendRequest: falha ao reassemblar resposta multi-fragmento');
      exit;
    end;
    if Length(Response)<10 then begin
      Debug('SendRequest: resposta reassemblada menor que o cabecalho de 10 bytes');
      exit;
    end;
    RespPayload := BytesCopy(Response, 10, Length(Response)-10);
    Result := true;
    Debug(Format('SendRequest: respPayload reassemblado (%d bytes)',[Length(RespPayload)]));
    exit;
  end;

  if not RecvApplicationFrame(ResponseFrame) then begin
    Debug('SendRequest: falha ao receber resposta');
    exit;
  end;
  if Length(ResponseFrame)<4 then begin
    Debug(Format('SendRequest: frame de resposta vazio/curto demais (%d bytes)',[Length(ResponseFrame)]));
    exit;
  end;

  Consumed := DecodeS7PlusHeader(ResponseFrame, 0, Version, DataLen);
  Response := BytesCopy(ResponseFrame, Consumed, DataLen);

  //V3 responses carry a [hashLen byte][hashLen-byte HMAC] prefix before the actual
  //10-byte response header + payload - strip it (matches python-snap7's send_request).
  if (Version=S7PlusVersion_V3) and (Length(Response)>33) then begin
    HashLen := Response[0];
    if 1+HashLen<Length(Response) then
      Response := BytesCopy(Response, 1+HashLen, Length(Response)-(1+HashLen));
  end;

  if Length(Response)<10 then begin
    Debug('SendRequest: resposta menor que o cabecalho de 10 bytes');
    exit;
  end;

  RespPayload := BytesCopy(Response, 10, Length(Response)-10);

  //SessionKey/HMAC (V3) responses prepend an IntegrityId VLQ before the actual
  //ReturnValue - ordinary V2 responses don't carry this. Missing this strip was
  //confirmed live 2026-08-31 to make PostAuthLegitimation always misread a garbled
  //[IntegrityId+ReturnValue] blob as if it were the ReturnValue alone - reliably
  //producing a sign-bit-set (looks like failure) value regardless of the real result.
  if (Length(FHarpoSessionKey)>0) and (Length(RespPayload)>1) then begin
    DecodeUInt32VLQ(RespPayload, 0, c);
    RespPayload := BytesCopy(RespPayload, c, Length(RespPayload)-c);
  end;

  Result := true;
  Debug('SendRequest: respPayload='+S7PlusHexStr(RespPayload));
end;

function TS7PlusConnection.SendReplayRaw(FunctionCode:Word; const Body:TBytes; PatchIntegrityAtEnd:Integer;
                                          out RespPayload:TBytes; Reassemble:Boolean):Boolean;
var
  Seq:Word;
  TransportFlags:Byte;
  Header, ActualPayload, Request, Frame, ResponseFrame, Response, IntegrityBytes, FrameData:TBytes;
  Version:Byte;
  DataLen:Word;
  Consumed, HashLen, c:Integer;
begin
  Result := false;
  SetLength(RespPayload, 0);
  if not FConnected then exit;

  Seq := NextSequenceNumber;
  TransportFlags := $34; //V3/SessionKey regime always uses 0x34

  if PatchIntegrityAtEnd>=0 then begin
    IntegrityBytes := EncodeUInt32VLQ(FIntegrityIdWrite);
    if Length(Body)>=PatchIntegrityAtEnd then
      ActualPayload := BytesConcat(BytesConcat(BytesCopy(Body,0,Length(Body)-PatchIntegrityAtEnd), IntegrityBytes), BytesCopy(Body,Length(Body)-PatchIntegrityAtEnd,PatchIntegrityAtEnd))
    else
      ActualPayload := Body;
    Debug(Format('SendReplayRaw: IntegrityId(write)=%d spliced @end-%d',[FIntegrityIdWrite,PatchIntegrityAtEnd]));
  end else
    ActualPayload := Body;

  Header := EncodeRequestHeader(FunctionCode, Seq, FSessionId, TransportFlags);
  Request := BytesConcat(Header, ActualPayload);

  if Length(FHarpoSessionKey)>0 then begin
    FrameData := BytesConcat(BytesOf([$20]), S7PlusHMACSHA256(Copy(FHarpoSessionKey,0,24), Request));
    FrameData := BytesConcat(FrameData, Request);
    Frame := BytesConcat(EncodeS7PlusHeader(S7PlusVersion_V3, Length(FrameData)), FrameData);
    Frame := BytesConcat(Frame, EncodeS7PlusHeader(S7PlusVersion_V3, 0));
  end else begin
    Frame := BytesConcat(EncodeS7PlusHeader(FProtocolVersion, Length(Request)), Request);
    Frame := BytesConcat(Frame, EncodeS7PlusHeader(FProtocolVersion, 0));
  end;

  Debug(Format('SendReplayRaw: functionCode=$%.4x seq=%d bodylen=%d',[FunctionCode,Seq,Length(ActualPayload)]));

  if not LogicalSend(Frame) then begin
    Debug('SendReplayRaw: falha ao enviar');
    exit;
  end;

  if PatchIntegrityAtEnd>=0 then
    FIntegrityIdWrite := (FIntegrityIdWrite+1) and $FFFFFFFF;

  if Reassemble then begin
    if not ReassembledRecv(Response) then begin
      Debug('SendReplayRaw: falha ao reassemblar resposta');
      exit;
    end;
    if Length(Response)<10 then exit;
    RespPayload := BytesCopy(Response, 10, Length(Response)-10);
    Result := true;
    exit;
  end;

  if not RecvApplicationFrame(ResponseFrame) then begin
    Debug('SendReplayRaw: falha ao receber resposta');
    exit;
  end;
  if Length(ResponseFrame)<4 then exit;

  Consumed := DecodeS7PlusHeader(ResponseFrame, 0, Version, DataLen);
  Response := BytesCopy(ResponseFrame, Consumed, DataLen);
  if (Version=S7PlusVersion_V3) and (Length(Response)>33) then begin
    HashLen := Response[0];
    if 1+HashLen<Length(Response) then
      Response := BytesCopy(Response, 1+HashLen, Length(Response)-(1+HashLen));
  end;
  if Length(Response)<10 then exit;
  RespPayload := BytesCopy(Response, 10, Length(Response)-10);
  if (Length(FHarpoSessionKey)>0) and (Length(RespPayload)>1) then begin
    DecodeUInt32VLQ(RespPayload, 0, c);
    RespPayload := BytesCopy(RespPayload, c, Length(RespPayload)-c);
  end;
  Result := true;
  Debug('SendReplayRaw: respPayload='+S7PlusHexStr(RespPayload));
end;

function TS7PlusConnection.ExtractErrorText(const Data:TBytes):String;
var
  Offset, c, i:Integer;
  Tag:Byte;
  Flags, DataType:Byte;
  Len:Cardinal;
  Txt:String;
begin
  Result := '';
  Offset := 0;
  while Offset<Length(Data) do begin
    Tag := Data[Offset];

    if Tag=S7PlusElement_Attribute then begin
      inc(Offset);
      if Offset>=Length(Data) then break;
      DecodeUInt32VLQ(Data, Offset, c); //AttrId - not needed here, just advancing Offset
      Offset := Offset+c;
      if (Offset+2)>Length(Data) then break;
      Flags := Data[Offset];
      DataType := Data[Offset+1];
      Offset := Offset+2;

      if (DataType=S7PlusType_WSTRING) and ((Flags and $10)=0) then begin
        Len := DecodeUInt32VLQ(Data, Offset, c);
        Offset := Offset+c;
        if (Offset+Integer(Len))<=Length(Data) then begin
          SetString(Txt, PAnsiChar(@Data[Offset]), Len);
          if Result<>'' then Result := Result+' | ';
          Result := Result+Txt;
        end;
        Offset := Offset+Integer(Len);
      end else
        Offset := SkipTypedValue(Data, Offset, DataType, Flags);

    end else if Tag=S7PlusElement_StartOfObject then begin
      inc(Offset);
      if (Offset+4)>Length(Data) then break;
      Offset := Offset+4; //RelationId (fixed)
      for i:=1 to 3 do begin //ClassId, ClassFlags, AttributeId (each VLQ)
        DecodeUInt32VLQ(Data, Offset, c);
        Offset := Offset+c;
      end;

    end else
      inc(Offset); //TerminatingObject/null/unknown tag - just skip one byte
  end;
end;

//===========================================================================
// Legitimation (PLC password authentication, V2+ with TLS)
//===========================================================================

function StrToByteArray(const S:AnsiString):TBytes;
begin
  SetLength(Result, Length(S));
  if Length(S)>0 then Move(S[1], Result[0], Length(S));
end;

function TS7PlusConnection.GetOMSSecret(out Secret:TBytes):Boolean;
const
  ExporterLabel = 'EXPERIMENTAL_OMS';
var
  Buf:array[0..31] of Byte;
begin
  Result := false;
  SetLength(Secret, 0);
  if (FSSL=nil) or (not FTLSActive) then exit;
  if SSL_export_keying_material(FSSL, @Buf[0], 32, PAnsiChar(ExporterLabel), Length(ExporterLabel), nil, 0, 0)<>1 then exit;
  SetLength(Secret, 32);
  Move(Buf[0], Secret[0], 32);
  Result := true;
end;

//: Mirrors python-snap7's _build_legitimation_payload: a Struct of 3 elements
//: (LegitimationType:UDINT, Username:BLOB, Password[Hash]:BLOB).
//: Mirrors the reference's ValueStruct.Serialize() (Core/PValue.cs) and buildLegitimationPayload
//: (Legitimation/Legitimation.cs): a fixed UInt32 struct-id - Ids.LID_LegitimationPayloadStruct
//: (40400), NOT an arbitrary/zero value: the PLC validates the struct-id against the known
//: "LegitimationPayload" schema, so any other value (0 included) gets rejected outright. Then
//: [VLQ elem-id][elem PValue] pairs (40401=LegitimationType, 40402=Username, 40403=Password),
//: terminated by a $00 list-terminator byte.
function TS7PlusConnection.BuildLegitimationPayload(const Password, Username:AnsiString):TBytes;
var
  LegitType:Cardinal;
  PasswordData, UsernameData:TBytes;
begin
  UsernameData := StrToByteArray(Username);
  if Username<>'' then begin
    LegitType := 2;
    PasswordData := StrToByteArray(Password);
  end else begin
    LegitType := 1;
    PasswordData := S7PlusSHA1(StrToByteArray(Password));
  end;

  Result := BytesOf([$00, S7PlusType_STRUCT]);
  Result := BytesConcat(Result, EncodeUInt32(S7PlusObjId_LegitimationPayloadStruct)); //fixed UInt32, not VLQ

  Result := BytesConcat(Result, EncodeUInt32VLQ(S7PlusObjId_LegitimationPayloadType));
  Result := BytesConcat(Result, BytesOf([$00, S7PlusType_UDINT]));
  Result := BytesConcat(Result, EncodeUInt32VLQ(LegitType));

  Result := BytesConcat(Result, EncodeUInt32VLQ(S7PlusObjId_LegitimationPayloadUsername));
  Result := BytesConcat(Result, EncodePValueBlob(UsernameData));

  Result := BytesConcat(Result, EncodeUInt32VLQ(S7PlusObjId_LegitimationPayloadPassword));
  Result := BytesConcat(Result, EncodePValueBlob(PasswordData));

  Result := BytesConcat(Result, BytesOf([$00])); //list terminator
end;

function TS7PlusConnection.BuildLegacyLegitimationResponse(const Password:AnsiString; const Challenge:TBytes):TBytes;
var
  Hash:TBytes;
  i, n:Integer;
begin
  Hash := S7PlusSHA1(StrToByteArray(Password));
  n := Length(Hash);
  if Length(Challenge)<n then n := Length(Challenge);
  SetLength(Result, n);
  for i:=0 to n-1 do
    Result[i] := Hash[i] xor Challenge[i];
end;

function TS7PlusConnection.BuildNewLegitimationResponse(const Password:AnsiString; const Challenge, OmsSecret:TBytes; const Username:AnsiString):TBytes;
var
  Key, IV, Payload:TBytes;
begin
  Key := S7PlusSHA256(OmsSecret);
  SetLength(IV, 16);
  FillChar(IV[0], 16, 0);
  if Length(Challenge)>0 then begin
    if Length(Challenge)>=16 then
      Move(Challenge[0], IV[0], 16)
    else
      Move(Challenge[0], IV[0], Length(Challenge));
  end;

  Payload := BuildLegitimationPayload(Password, Username);
  Result := S7PlusAES256CBCEncrypt(Key, IV, Payload);
end;

//: Mirrors the reference's GetVarSubstreamedRequest/Response (Core/GetVarSubstreamedRequest.cs,
//: Core/GetVarSubstreamedResponse.cs) - a genuinely different wire shape from GetMultiVariables/
//: SetVariable, confirmed against the C# reference: InObjectId, an "Addressarray" ($20 marker +
//: element datatype + array size + the address VLQ - not the field-count scheme used elsewhere),
//: an ObjectQualifier, then 2 unknown bytes (0x0001) before the (spliced) IntegrityId/padding.
//: The response carries ReturnValue, 1 unknown byte, then the challenge as a typed PValue.
function TS7PlusConnection.GetLegitimationChallenge(out Challenge:TBytes):Boolean;
var
  Payload, Resp:TBytes;
  Offset, c:Integer;
  RetVal:QWord;
begin
  Result := false;
  SetLength(Challenge, 0);

  Payload := EncodeUInt32(FSessionId); //InObjectId
  Payload := BytesConcat(Payload, BytesOf([$20])); //Addressarray marker
  Payload := BytesConcat(Payload, BytesOf([S7PlusType_UDINT])); //datatype of the address array elements
  Payload := BytesConcat(Payload, BytesOf([1])); //array size = 1
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(S7PlusObjId_ServerSessionRequest));
  Payload := BytesConcat(Payload, EncodeObjectQualifier);
  Payload := BytesConcat(Payload, EncodeUInt16(1)); //2 bytes unknown = 0x0001
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  if not SendRequest(S7PlusFunc_GetVarSubstreamed, Payload, Resp) then exit;

  Offset := 0;
  RetVal := DecodeUInt64VLQ(Resp, Offset, c);
  Offset := Offset+c;
  FLastReturnValue := RetVal;
  if RetVal<>0 then begin
    Debug(Format('GetLegitimationChallenge: PLC retornou erro (returnValue=%d)',[RetVal]));
    exit;
  end;

  if Offset>=Length(Resp) then begin
    Debug('GetLegitimationChallenge: resposta curta demais (sem byte desconhecido)');
    exit;
  end;
  Offset := Offset+1; //1 unknown byte before the PValue

  if (Offset+2)>Length(Resp) then begin
    Debug('GetLegitimationChallenge: resposta curta demais (sem PValue)');
    exit;
  end;
  Challenge := DecodePValueToBytes(Resp, Offset, c);
  Result := true;
end;

function TS7PlusConnection.GetEffectiveProtectionLevel(out AccessLevel:Cardinal):Boolean;
var
  Payload, Resp, Value:TBytes;
  Offset, c, i:Integer;
  RetVal:QWord;
begin
  Result := false;
  AccessLevel := 0;

  Payload := EncodeUInt32(FSessionId); //InObjectId
  Payload := BytesConcat(Payload, BytesOf([$20])); //Addressarray marker
  Payload := BytesConcat(Payload, BytesOf([S7PlusType_UDINT])); //datatype of the address array elements
  Payload := BytesConcat(Payload, BytesOf([1])); //array size = 1
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(S7PlusObjId_EffectiveProtectionLevel));
  Payload := BytesConcat(Payload, EncodeObjectQualifier);
  Payload := BytesConcat(Payload, EncodeUInt16(1)); //2 bytes unknown = 0x0001
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  if not SendRequest(S7PlusFunc_GetVarSubstreamed, Payload, Resp) then exit;

  Offset := 0;
  RetVal := DecodeUInt64VLQ(Resp, Offset, c);
  Offset := Offset+c;
  FLastReturnValue := RetVal;
  if RetVal<>0 then begin
    Debug(Format('GetEffectiveProtectionLevel: PLC retornou erro (returnValue=%d)',[RetVal]));
    exit;
  end;

  if Offset>=Length(Resp) then exit;
  Offset := Offset+1; //1 unknown byte before the PValue

  if (Offset+2)>Length(Resp) then exit;
  Value := DecodePValueToBytes(Resp, Offset, c);
  for i:=0 to High(Value) do
    AccessLevel := (AccessLevel shl 8) or Value[i];
  Result := true;
end;

//: Mirrors the reference's SetVariableRequest (Core/SetVariableRequest.cs): InObjectId,
//: VLQ(1) ("always 1"), the address VLQ, the value's own PValue.Serialize(), then
//: ObjectQualifier + 1 unknown byte before the (spliced) IntegrityId/padding - the same
//: framing gap (missing ObjectQualifier + unknown byte) that caused every write/legitimation
//: attempt built like SetMultiVariables to be rejected.
function TS7PlusConnection.SendLegitimationNew(const EncryptedResponse:TBytes):Boolean;
var
  Payload, Resp:TBytes;
  RetVal:QWord;
  c:Integer;
begin
  Result := false;

  Payload := EncodeUInt32(FSessionId); //InObjectId
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(1)); //count, always 1
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(S7PlusObjId_Legitimate));
  Payload := BytesConcat(Payload, EncodePValueBlob(EncryptedResponse));
  Payload := BytesConcat(Payload, EncodeObjectQualifier);
  Payload := BytesConcat(Payload, BytesOf([$00])); //1 byte unknown
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  if not SendRequest(S7PlusFunc_SetVariable, Payload, Resp) then exit;
  if Length(Resp)=0 then exit;

  RetVal := DecodeUInt64VLQ(Resp, 0, c);
  FLastReturnValue := RetVal;
  Result := RetVal=0;
  if not Result then
    Debug(Format('SendLegitimationNew: PLC rejeitou (returnValue=%d)',[RetVal]));
end;

function TS7PlusConnection.SendLegitimationLegacy(const Response:TBytes):Boolean;
var
  Payload, Resp:TBytes;
  RetVal:QWord;
  c:Integer;
begin
  Result := false;

  Payload := EncodeUInt32(FSessionId); //InObjectId
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(1)); //count, always 1
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(S7PlusObjId_ServerSessionResponse));
  Payload := BytesConcat(Payload, BytesOf([$10, S7PlusType_USINT])); //ValueUSIntArray: flags=array
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(Length(Response)));
  Payload := BytesConcat(Payload, Response);
  Payload := BytesConcat(Payload, EncodeObjectQualifier);
  Payload := BytesConcat(Payload, BytesOf([$00])); //1 byte unknown
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  if not SendRequest(S7PlusFunc_SetVariable, Payload, Resp) then exit;
  if Length(Resp)=0 then exit;

  RetVal := DecodeUInt64VLQ(Resp, 0, c);
  FLastReturnValue := RetVal;
  Result := RetVal=0;
  if not Result then
    Debug(Format('SendLegitimationLegacy: PLC rejeitou (returnValue=%d)',[RetVal]));
end;

function TS7PlusConnection.Authenticate(const Password:AnsiString; const Username:AnsiString):Boolean;
var
  Challenge, OmsSecret, Response:TBytes;
  AccessLevel:Cardinal;
begin
  Result := false;
  if not FConnected then begin
    Debug('Authenticate: nao conectado');
    exit;
  end;

  //The reference always queries this before attempting legitimation, and skips
  //legitimation entirely if the session already has FullAccess - possibly required
  //session-state sequencing, not just an optimization (untested either way against real
  //hardware until now, since it was omitted here originally).
  if GetEffectiveProtectionLevel(AccessLevel) then begin
    Debug(Format('Authenticate: EffectiveProtectionLevel=%d',[AccessLevel]));
    if AccessLevel<=S7PlusAccessLevel_FullAccess then begin
      Debug('Authenticate: sessao ja tem FullAccess - legitimacao desnecessaria');
      Result := true;
      exit;
    end;
  end else
    Debug('Authenticate: falha ao consultar EffectiveProtectionLevel - prosseguindo mesmo assim');

  if not GetOMSSecret(OmsSecret) then begin
    Debug('Authenticate: legitimacao requer TLS ativo (falha ao derivar o OMS exporter secret)');
    exit;
  end;

  if not GetLegitimationChallenge(Challenge) then begin
    Debug('Authenticate: falha ao obter o desafio (challenge) do CLP');
    exit;
  end;
  Debug(Format('Authenticate: desafio recebido (%d bytes)',[Length(Challenge)]));

  Response := BuildNewLegitimationResponse(Password, Challenge, OmsSecret, Username);
  if (Length(Response)>0) and SendLegitimationNew(Response) then begin
    Debug('Authenticate: legitimacao (AES-256-CBC) bem-sucedida');
    Result := true;
    exit;
  end;

  Debug('Authenticate: legitimacao AES-256-CBC falhou/indisponivel, tentando legado (SHA1 XOR)');
  Response := BuildLegacyLegitimationResponse(Password, Challenge);
  if SendLegitimationLegacy(Response) then begin
    Debug('Authenticate: legitimacao (legado SHA1 XOR) bem-sucedida');
    Result := true;
  end else
    Debug('Authenticate: legitimacao legada tambem falhou');
end;

//===========================================================================
// Subscriptions (on-change notifications, V2+) - Subscriptions/Subscription.cs
//===========================================================================

//: Mirrors GetSubscriptionListArray: a UDInt array (Flags=$20) encoding
//: [create-header][unsubscribe-count=0][subscribe-count], then per item
//: [head][refid][0][AccessArea][SymbolCrc][AccessSubArea][Lids...], where head packs
//: "1+LID count" into its low 16 bits (the "1" accounts for AccessSubArea).
function TS7PlusConnection.BuildSubscriptionReferenceList(const Items:TS7PlusMultiReadItemArray):TBytes;
var
  Values:array of Cardinal;
  i, j, n:Integer;
  Head, AccessSubArea:Cardinal;

  procedure AddVal(V:Cardinal);
  begin
    SetLength(Values, Length(Values)+1);
    Values[High(Values)] := V;
  end;

begin
  SetLength(Values, 0);
  AddVal($80000000 or (Cardinal(FSubscriptionChangeCounter) shl 16)); //create-header
  AddVal(0); //number of items to unsubscribe
  AddVal(Cardinal(Length(Items))); //number of items to subscribe

  for i:=0 to High(Items) do begin
    if Items[i].AccessArea>=S7PlusIds_DBAccessAreaBase then
      AccessSubArea := S7PlusIds_DBValueActual
    else
      AccessSubArea := S7PlusIds_ControllerAreaValueActual;

    Head := $80040000 or Cardinal(1+Length(Items[i].Lids));
    AddVal(Head);
    AddVal(Cardinal(i+1)); //1-based reference id
    AddVal(0); //unknown 1
    AddVal(Items[i].AccessArea);
    //SymbolCrc=0 (not the variable's real SymbolCrc): a prior comment here claimed the
    //opposite ("the reference uses the real SymbolCrc, unlike a plain read/write") based
    //on reading the C# reference's source, but that was never actually confirmed against
    //a working subscription - confirmed live 2026-08-31, comparing byte-for-byte against
    //python-snap7's build_subscription_request (proven working: real values received),
    //that it sends 0 for every item here, same as a plain read/write. Sending the real
    //SymbolCrc instead was the actual cause of every item coming back with per-item error
    //status 0x13 in the notification (subscription created fine, but no item ever
    //produced a value) - the PLC's subscription layout check apparently does NOT match
    //this the way EXPLORE walks report it.
    AddVal(0);
    AddVal(AccessSubArea);
    for j:=0 to High(Items[i].Lids) do
      AddVal(Items[i].Lids[j]);
  end;

  n := Length(Values);
  Result := EncodeValuePUDIntArray(Values, $20);
  if n=0 then; //silence unused-var warning on some FPC versions
end;

function TS7PlusConnection.SubscriptionCreate(const Items:TS7PlusMultiReadItemArray; CycleTimeMs:Word;
                                               RouteMode:Byte; UseFixedRelationId:Boolean;
                                               IncludeAttr1055:Boolean; CreditLimitOverride:SmallInt):Boolean;
var
  Attrs:TS7PlusPObjectAttributeArray;
  ObjBytes, Payload, Resp:TBytes;
  BodyOffset, IntegrityTail, c:Integer;
  RetVal:QWord;
  ObjectId:Cardinal;
  ErrText:String;

  procedure AddAttr(AttrId:Cardinal; const Value:TBytes);
  var n:Integer;
  begin
    n := Length(Attrs);
    SetLength(Attrs, n+1);
    Attrs[n].AttrId := AttrId;
    Attrs[n].Value := Value;
  end;

begin
  Result := false;

  //Both a fixed RelationId (0x7FFFC001, what the reference driver hardcodes) and
  //GetNewRIDOnServer (letting the PLC assign one) are confirmed working live against
  //.210 (2026-08-31) - the earlier "Download error" this comment used to blame on
  //RelationId was actually caused by three unrelated wire-encoding bugs elsewhere in
  //this function and in EncodeValuePWString/EncodeValuePUInt/EncodeValuePInt (see their
  //comments) that desynchronized the whole attribute list; GetNewRIDOnServer stays the
  //default since it avoids any chance of RelationId collision across sessions.
  if UseFixedRelationId then
    FSubscriptionRelationId := $7FFFC001
  else
    FSubscriptionRelationId := S7PlusObjId_GetNewRIDOnServer;
  Inc(FSubscriptionChangeCounter);
  if FSubscriptionChangeCounter=0 then FSubscriptionChangeCounter := 1; //keep it non-zero

  SetLength(Attrs, 0);
  AddAttr(S7PlusIds_ObjectVariableTypeName, EncodeValuePWString('Subscription_'+IntToStr(FSubscriptionChangeCounter)));
  AddAttr(S7PlusObjId_SubscriptionFunctionClassId, EncodeValuePUSInt(0));
  AddAttr(S7PlusObjId_SubscriptionMissedSendings, EncodeValuePUInt(0));
  AddAttr(S7PlusObjId_SubscriptionSubsystemError, EncodeValuePLInt(0));
  AddAttr(S7PlusObjId_SubscriptionRouteMode, EncodeValuePUSInt(RouteMode));
  AddAttr(S7PlusObjId_SubscriptionActive, EncodeValuePBool(true));
  AddAttr(S7PlusObjId_SubscriptionReferenceList, BuildSubscriptionReferenceList(Items));
  AddAttr(S7PlusObjId_SubscriptionCycleTime, EncodeValuePUDInt(CycleTimeMs));
  AddAttr(S7PlusObjId_SubscriptionDisabled, EncodeValuePUSInt(0));
  AddAttr(S7PlusObjId_SubscriptionCount, EncodeValuePUSInt(0));
  FSubscriptionCreditLimit := CreditLimitOverride;
  AddAttr(S7PlusObjId_SubscriptionCreditLimit, EncodeValuePInt(FSubscriptionCreditLimit));
  AddAttr(S7PlusObjId_SubscriptionTicks, EncodeValuePUInt(65535));
  if IncludeAttr1055 then
    AddAttr(1055, EncodeValuePUSInt(0)); //unknown - reference notes it works without setting it too

  ObjBytes := EncodePObject(FSubscriptionRelationId, S7PlusObjId_ClassSubscription, 0, 0, Attrs);

  //InObjectId/second-field order fixed 2026-08-31 by comparing byte-for-byte against
  //python-snap7's build_subscription_request (upstream branches feat/710-v1-subscription-
  //data + fix-v1-sessionkey-challenge-layout merged locally), confirmed live to actually
  //create a working subscription (real notifications received) against .210: InObjectId is
  //the ClassSubscriptions container (FSubscriptionsContainerObjectId, the parent object
  //CreateSession creates alongside the session - see that field's comment), and the second
  //4-byte field is plain zero padding, NOT the container id as this previously had it
  //(swapped) - that earlier version was the actual cause of "download: no parent object!",
  //not a wrong RelationId as originally suspected.
  Payload := EncodeUInt32(FSubscriptionsContainerObjectId); //InObjectId
  Payload := BytesConcat(Payload, EncodeValuePUDInt(0)); //RequestValue = ValueUDInt(0)
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //padding
  Payload := BytesConcat(Payload, ObjBytes);
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  IntegrityTail := Length(ObjBytes)+4; //IntegrityId splices right before [Object][padding]
  if not SendRequest(S7PlusFunc_CreateObject, Payload, Resp, IntegrityTail) then exit;

  //Decode the ReturnValue ourselves first (same convention as every other response - see
  //ParseSingleReadResponse) so a nonzero value can be paired with ExtractErrorText: on
  //failure the PLC's CreateObject response isn't shaped like the success case
  //ParseCreateObjectSessionId expects (ReturnValue+ObjectCount+ObjectIds) - it's
  //ReturnValue followed by ResponseExtension PObjects carrying the human-readable error
  //text (e.g. "Download error (IDs & states [main, sub, TI, next])!"), same shape the
  //read/write error paths already handle.
  RetVal := DecodeUInt64VLQ(Resp, 0, c);
  FLastReturnValue := RetVal;
  if RetVal=0 then begin
    Result := ParseCreateObjectSessionId(Resp, ObjectId, BodyOffset, RetVal) and (RetVal=0);
    if Result then begin
      FSubscriptionObjectId := ObjectId;
      Debug(Format('SubscriptionCreate: sucesso, ObjectId=%d, %d itens',[ObjectId,Length(Items)]));
    end else
      Debug('SubscriptionCreate: falha ao interpretar resposta de sucesso (returnValue=0)');
  end else begin
    Result := false;
    ErrText := ExtractErrorText(BytesCopy(Resp, c, Length(Resp)-c));
    if ErrText<>'' then
      Debug(Format('SubscriptionCreate: falha (returnValue=%d): %s',[RetVal, ErrText]))
    else
      Debug(Format('SubscriptionCreate: falha (returnValue=%d)',[RetVal]));
  end;
end;

function TS7PlusConnection.SubscriptionSetCreditLimit(Limit:SmallInt):Boolean;
var
  Payload, Resp:TBytes;
begin
  Payload := EncodeUInt32(FSubscriptionObjectId); //InObjectId
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(1)); //count, always 1
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(S7PlusObjId_SubscriptionCreditLimit));
  Payload := BytesConcat(Payload, EncodeValuePInt(Limit));
  Payload := BytesConcat(Payload, EncodeObjectQualifier);
  Payload := BytesConcat(Payload, BytesOf([$00])); //1 byte unknown
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  //Fire-and-forget: the reference sets TransportFlags=0x74 ("no response needed") for this
  //specific call - our SendRequest always uses the normal write flags ($36) and waits for a
  //reply, which the PLC still answers correctly, just with an extra (harmless) round-trip.
  Result := SendRequest(S7PlusFunc_SetVariable, Payload, Resp);
  if Result then
    FSubscriptionCreditLimit := Limit;
end;

function TS7PlusConnection.SubscriptionDelete:Boolean;
var
  Payload, Resp:TBytes;
begin
  //The reference deletes via "SessionId2", not the subscription's own ObjectId - that name
  //had us fooled into using FSessionId (the connection's actual session id) here, which is
  //wrong: SessionId2 is the ClassSubscriptions container's ObjectId (see
  //FSubscriptionsContainerObjectId) - the same "parent object" SubscriptionCreate's envelope
  //needs. Deleting it tears down the whole container, not just this one subscription -
  //matches the reference's own "basically a test" scope, but means a fresh
  //FSubscriptionsContainerObjectId won't be available again until the next new session.
  Payload := EncodeUInt32(FSubscriptionsContainerObjectId); //DeleteObjectId
  Payload := BytesConcat(Payload, BytesOf([$00]));
  Payload := BytesConcat(Payload, EncodeObjectQualifier);
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  Result := SendRequest(S7PlusFunc_DeleteObject, Payload, Resp);
  if Result then
    FSubscriptionObjectId := 0;
end;

//: Mirrors Notification.Deserialize. Frame is the frame body starting at the Opcode byte
//: (the same slice SendRequest would otherwise treat as a response - callers must check the
//: Opcode themselves, e.g. via LogicalRecv, before assuming a received frame is a normal
//: reply). Only the common (non-alarm, non-legacy-8-byte-object) branches are implemented;
//: anything else aborts the parse and returns false rather than risk mis-parsing.
function TS7PlusConnection.ParseNotification(const Frame:TBytes; out CreditTick:Byte; out Values:TS7PlusNotificationValueArray):Boolean;
var
  Offset, c, n:Integer;
  ChangeCounter, ItemTag:Byte;
  RefId:Cardinal;
  Value:TBytes;
begin
  Result := false;
  SetLength(Values, 0);
  CreditTick := 0;

  Offset := 0;
  if Length(Frame)<1 then exit;
  if Frame[Offset]<>S7PlusOpcode_Notification then exit;
  Inc(Offset);

  if (Offset+10)>Length(Frame) then exit;
  Offset := Offset+4; //SubscriptionObjectId - not needed, we only track one subscription
  Offset := Offset+6; //3x UInt16 unknown

  if Offset>=Length(Frame) then exit;
  CreditTick := Frame[Offset];
  Inc(Offset);

  DecodeUInt32VLQ(Frame, Offset, c); //NotificationSequenceNumber - not surfaced yet
  Offset := Offset+c;

  if Offset>=Length(Frame) then exit;
  ChangeCounter := Frame[Offset];
  if ChangeCounter>0 then
    Inc(Offset)
  else begin
    //Newer S7-1500: an 8-byte UTC microsecond timestamp + change counter byte, instead.
    if (Offset+9)>Length(Frame) then exit;
    Offset := Offset+8+1;
  end;

  repeat
    if Offset>=Length(Frame) then exit;
    ItemTag := Frame[Offset];
    Inc(Offset);
    case ItemTag of
      $00: ; //list terminator
      $92: begin
        if (Offset+4)>Length(Frame) then exit;
        RefId := DecodeUInt32(Frame, Offset);
        Offset := Offset+4;
        Value := DecodePValueToBytes(Frame, Offset, c);
        Offset := Offset+c;
        n := Length(Values);
        SetLength(Values, n+1);
        Values[n].RefId := RefId;
        Values[n].Data := Value;
      end;
      $9B: begin
        RefId := DecodeUInt32VLQ(Frame, Offset, c);
        Offset := Offset+c;
        Value := DecodePValueToBytes(Frame, Offset, c);
        Offset := Offset+c;
        n := Length(Values);
        SetLength(Values, n+1);
        Values[n].RefId := RefId;
        Values[n].Data := Value;
      end;
      $9C: begin
        if (Offset+4)>Length(Frame) then exit;
        Offset := Offset+4; //dummy value, unused
      end;
      $13, $03: begin
        //Per-item error report (RefId + a small error code) - not surfaced per-item yet.
        if (Offset+4)>Length(Frame) then exit;
        Offset := Offset+4;
      end;
    else
      Debug(Format('ParseNotification: item tag nao suportado ($%.2x) - abortando parse',[ItemTag]));
      exit;
    end;
  until ItemTag=$00;

  Result := true;
end;

function TS7PlusConnection.WaitForFrame(out FrameBody:TBytes):Boolean;
var
  ResponseFrame:TBytes;
  Version:Byte;
  DataLen:Word;
  Consumed:Integer;
begin
  Result := false;
  SetLength(FrameBody, 0);
  if LogicalRecv(ResponseFrame)<>iorOK then exit;
  if Length(ResponseFrame)<4 then exit;
  Consumed := DecodeS7PlusHeader(ResponseFrame, 0, Version, DataLen);
  FrameBody := BytesCopy(ResponseFrame, Consumed, DataLen);
  Result := true;
end;

function TS7PlusConnection.UploadBlock(BlockType, BlockNumber:Cardinal; out Data:TBytes):Boolean;
var
  Payload, Resp:TBytes;
  Offset, c:Integer;
  RetVal:QWord;
begin
  Result := false;
  SetLength(Data, 0);

  Payload := EncodeUInt32(FSessionId); //InObjectId
  Payload := BytesConcat(Payload, BytesOf([$20])); //Addressarray marker
  Payload := BytesConcat(Payload, BytesOf([S7PlusType_UDINT])); //datatype of the address array elements
  Payload := BytesConcat(Payload, BytesOf([2])); //array size = 2 (BlockType, BlockNumber)
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(BlockType));
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(BlockNumber));
  Payload := BytesConcat(Payload, EncodeObjectQualifier);
  Payload := BytesConcat(Payload, EncodeUInt16(1)); //2 bytes unknown = 0x0001
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailing padding

  if not SendRequest(S7PlusFunc_GetVarSubstreamed, Payload, Resp) then exit;

  Offset := 0;
  RetVal := DecodeUInt64VLQ(Resp, Offset, c);
  Offset := Offset+c;
  FLastReturnValue := RetVal;
  if RetVal<>0 then begin
    Debug(Format('UploadBlock: PLC retornou erro (returnValue=%d)',[RetVal]));
    exit;
  end;

  if Offset>=Length(Resp) then begin
    Debug('UploadBlock: resposta curta demais (sem byte desconhecido)');
    exit;
  end;
  Offset := Offset+1; //1 unknown byte before the PValue

  if (Offset+2)>Length(Resp) then begin
    Debug('UploadBlock: resposta curta demais (sem PValue)');
    exit;
  end;
  Data := DecodePValueToBytes(Resp, Offset, c);
  Result := true;
end;

function TS7PlusConnection.CurrentObjectQualifier:TBytes;
begin
  if Length(FHarpoSessionKey)>0 then
    Result := EncodeObjectQualifierV1(1)
  else
    Result := EncodeObjectQualifier;
end;

//===========================================================================
// Area / DB read-write (GetMultiVariables / SetMultiVariables, single item)
//===========================================================================

function TS7PlusConnection.BuildAreaPayload(AccessArea, AccessSubArea:Cardinal; Start:Integer; const WriteData:TBytes; IsWrite:Boolean; SizeIfRead:Integer):TBytes;
var
  Addr:TS7PlusItemAddress;
  Lids:array[0..1] of Cardinal;
begin
  if IsWrite then begin
    Lids[0] := Start+1;
    Lids[1] := Length(WriteData);
  end else begin
    Lids[0] := Start+1;
    Lids[1] := SizeIfRead;
  end;

  Addr := EncodeItemAddress(AccessArea, AccessSubArea, Lids);

  Result := EncodeUInt32(0); //InObjectId
  Result := BytesConcat(Result, EncodeUInt32VLQ(1)); //item count
  Result := BytesConcat(Result, EncodeUInt32VLQ(Addr.FieldCount));
  Result := BytesConcat(Result, Addr.Data);

  if IsWrite then begin
    Result := BytesConcat(Result, EncodeUInt32VLQ(1)); //item number 1
    Result := BytesConcat(Result, EncodePValueBlob(WriteData));
    Result := BytesConcat(Result, BytesOf([$00]));
  end;

  Result := BytesConcat(Result, CurrentObjectQualifier);
  Result := BytesConcat(Result, EncodeUInt32VLQ(1));
  Result := BytesConcat(Result, EncodeUInt32(0)); //trailing padding
end;

function TS7PlusConnection.ParseSingleReadResponse(const Response:TBytes; out Data:TBytes):Boolean;
var
  Offset, c:Integer;
  RetVal:QWord;
  ItemNr:Cardinal;
  Body:TBytes;
  ErrText:String;
begin
  Result := false;
  SetLength(Data, 0);

  if EndsWithScalarSuffix(Response) then begin
    Body := BytesCopy(Response, 0, Length(Response)-Length(ScalarResponseSuffix));
    if (Length(Body)=2) and (Body[1]=0) then begin
      SetLength(Data,1);
      Data[0] := Body[0];
      Result := true;
      exit;
    end;
  end;

  Offset := 0;
  RetVal := DecodeUInt64VLQ(Response, Offset, c);
  Offset := Offset+c;
  FLastReturnValue := RetVal;
  if RetVal<>0 then begin
    ErrText := ExtractErrorText(BytesCopy(Response, Offset, Length(Response)-Offset));
    if ErrText<>'' then
      Debug(Format('ParseSingleReadResponse: PLC retornou erro (returnValue=%d): %s',[RetVal,ErrText]))
    else
      Debug(Format('ParseSingleReadResponse: PLC retornou erro (returnValue=%d)',[RetVal]));
    exit;
  end;

  if Offset>=Length(Response) then begin
    Debug('ParseSingleReadResponse: resposta sem itens apos o returnValue');
    exit;
  end;
  ItemNr := DecodeUInt32VLQ(Response, Offset, c);
  Offset := Offset+c;
  if ItemNr=0 then begin
    ErrText := ExtractErrorText(BytesCopy(Response, Offset, Length(Response)-Offset));
    if ErrText<>'' then
      Debug('ParseSingleReadResponse: itemNr=0 (nenhum valor retornado): '+ErrText)
    else
      Debug('ParseSingleReadResponse: itemNr=0 (nenhum valor retornado)');
    exit;
  end;

  Data := DecodePValueToBytes(Response, Offset, c);
  Result := true;
end;

function TS7PlusConnection.ParseSingleWriteResponse(const Response:TBytes):Boolean;
var
  RetVal:QWord;
  c:Integer;
  ErrText:String;
begin
  Result := false;
  if Length(Response)=0 then exit;
  RetVal := DecodeUInt64VLQ(Response, 0, c);
  FLastReturnValue := RetVal;
  Result := RetVal=0;
  if not Result then begin
    ErrText := ExtractErrorText(BytesCopy(Response, c, Length(Response)-c));
    if ErrText<>'' then
      Debug(Format('ParseSingleWriteResponse: PLC retornou erro (returnValue=%d): %s',[RetVal,ErrText]))
    else
      Debug(Format('ParseSingleWriteResponse: PLC retornou erro (returnValue=%d)',[RetVal]));
  end;
end;

function TS7PlusConnection.ReadArea(AreaRID:Cardinal; Start, Size:Integer; out Data:TBytes):Boolean;
var
  Payload, Resp:TBytes;
begin
  Result := false;
  SetLength(Data,0);
  Payload := BuildAreaPayload(AreaRID, S7PlusIds_ControllerAreaValueActual, Start, nil, false, Size);
  if not SendRequest(S7PlusFunc_GetMultiVariables, Payload, Resp) then exit;
  Result := ParseSingleReadResponse(Resp, Data);
end;

function TS7PlusConnection.WriteArea(AreaRID:Cardinal; Start:Integer; const Data:TBytes):Boolean;
var
  Payload, Resp:TBytes;
begin
  Payload := BuildAreaPayload(AreaRID, S7PlusIds_ControllerAreaValueActual, Start, Data, true, 0);
  Result := SendRequest(S7PlusFunc_SetMultiVariables, Payload, Resp) and ParseSingleWriteResponse(Resp);
end;

function TS7PlusConnection.DBRead(DBNumber, Start, Size:Integer; out Data:TBytes):Boolean;
var
  Payload, Resp:TBytes;
  AccessArea:Cardinal;
begin
  Result := false;
  SetLength(Data,0);
  AccessArea := S7PlusIds_DBAccessAreaBase + Cardinal(DBNumber and $FFFF);
  Payload := BuildAreaPayload(AccessArea, S7PlusIds_DBValueActual, Start, nil, false, Size);
  if not SendRequest(S7PlusFunc_GetMultiVariables, Payload, Resp) then exit;
  Result := ParseSingleReadResponse(Resp, Data);
end;

function TS7PlusConnection.DBWrite(DBNumber, Start:Integer; const Data:TBytes):Boolean;
var
  Payload, Resp:TBytes;
  AccessArea:Cardinal;
begin
  AccessArea := S7PlusIds_DBAccessAreaBase + Cardinal(DBNumber and $FFFF);
  Payload := BuildAreaPayload(AccessArea, S7PlusIds_DBValueActual, Start, Data, true, 0);
  Result := SendRequest(S7PlusFunc_SetMultiVariables, Payload, Resp) and ParseSingleWriteResponse(Resp);
end;

//===========================================================================
// Symbolic (LID-based) access - the only reliable way to read/write DB
// variables and native-area named tags on real S7-1200/1500 firmware.
//===========================================================================

function TS7PlusConnection.BuildSymbolicPayload(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const WriteData:TBytes; IsWrite:Boolean; SymbolCrc:Cardinal; WriteSoftDataType:Byte):TBytes;
var
  AccessSubArea:Cardinal;
  Addr:TS7PlusItemAddress;
begin
  if AccessArea>=S7PlusIds_DBAccessAreaBase then
    AccessSubArea := S7PlusIds_DBValueActual
  else
    AccessSubArea := S7PlusIds_ControllerAreaValueActual;

  Addr := EncodeItemAddress(AccessArea, AccessSubArea, Lids, SymbolCrc);

  Result := EncodeUInt32(0); //InObjectId
  Result := BytesConcat(Result, EncodeUInt32VLQ(1)); //item count
  Result := BytesConcat(Result, EncodeUInt32VLQ(Addr.FieldCount));
  Result := BytesConcat(Result, Addr.Data);

  if IsWrite then begin
    Result := BytesConcat(Result, EncodeUInt32VLQ(1)); //item number 1
    //Must be encoded as the target's own type-specific scalar PValue (ValueDInt, ValueReal,
    //...), not a generic byte array/blob - confirmed against real hardware: a byte-array
    //value gets rejected by the PLC regardless of session/legitimation state, while a
    //correctly-typed scalar is accepted. See EncodeTypedWriteValue.
    Result := BytesConcat(Result, EncodeTypedWriteValue(WriteSoftDataType, WriteData));
    Result := BytesConcat(Result, BytesOf([$00]));
  end;

  Result := BytesConcat(Result, CurrentObjectQualifier);
  //The reference implementation only appends this VLQ(1) for writes
  //(_build_symbolic_write_payload) - reads (_build_symbolic_read_payload) omit it
  //entirely (with_integrity=False). Including it unconditionally made every
  //symbolic GetMultiVariables (read) request 1 byte longer than the PLC expects.
  if IsWrite then
    Result := BytesConcat(Result, EncodeUInt32VLQ(1));
  Result := BytesConcat(Result, EncodeUInt32(0)); //trailing padding
end;

//: Batched GetMultiVariables read payload, N symbolic (LID) items in one request. Ported
//: from the reference's _build_read_payload: unlike the single-item BuildSymbolicPayload,
//: the FieldCount here is the SUM across every item, followed by all items' address
//: encodings concatenated back-to-back (no per-item "item number" prefix on the read
//: side - that only exists in the write payload, one item value per write). Items may
//: carry different AccessAreas (different DBs), matching the reference's own db_number
//: parameter varying per item.
function TS7PlusConnection.BuildMultiSymbolicReadPayload(const Items:TS7PlusMultiReadItemArray):TBytes;
var
  i:Integer;
  AccessSubArea:Cardinal;
  Addr:TS7PlusItemAddress;
  AllAddrData:TBytes;
  TotalFieldCount:Cardinal;
begin
  TotalFieldCount := 0;
  SetLength(AllAddrData, 0);
  for i:=0 to High(Items) do begin
    if Items[i].AccessArea>=S7PlusIds_DBAccessAreaBase then
      AccessSubArea := S7PlusIds_DBValueActual
    else
      AccessSubArea := S7PlusIds_ControllerAreaValueActual;
    Addr := EncodeItemAddress(Items[i].AccessArea, AccessSubArea, Items[i].Lids, 0);
    inc(TotalFieldCount, Cardinal(Addr.FieldCount));
    AllAddrData := BytesConcat(AllAddrData, Addr.Data);
  end;

  Result := EncodeUInt32(0); //InObjectId
  Result := BytesConcat(Result, EncodeUInt32VLQ(Length(Items))); //item count
  Result := BytesConcat(Result, EncodeUInt32VLQ(TotalFieldCount));
  Result := BytesConcat(Result, AllAddrData);
  Result := BytesConcat(Result, CurrentObjectQualifier);
  Result := BytesConcat(Result, EncodeUInt32(0)); //trailing padding
end;

//: Generalizes ParseSingleReadResponse to N items: a values section (repeated [ItemNr
//: VLQ][PValue], ItemNr=0 terminates) followed by an errors section (repeated [ItemNr
//: VLQ][ErrValue VLQ64], ItemNr=0 terminates) - ported from the reference's
//: _parse_read_response. ItemNr is 1-based and indexes back into the Items array the
//: request was built from, in the same order. Result=false only for a whole-request
//: failure (nonzero returnValue); an individual item missing from the values section (PLC
//: reported it in the errors section instead) just leaves that Results[i].Ok=false.
function TS7PlusConnection.ParseMultiReadResponse(const Response:TBytes; ItemCount:Integer; out Results:TS7PlusMultiReadResultArray):Boolean;
var
  Offset, c, i:Integer;
  RetVal:QWord;
  ItemNr:Cardinal;
  ErrText:String;
begin
  Result := false;
  SetLength(Results, ItemCount);
  for i:=0 to ItemCount-1 do begin
    SetLength(Results[i].Data, 0);
    Results[i].Ok := false;
  end;

  Offset := 0;
  RetVal := DecodeUInt64VLQ(Response, Offset, c);
  Offset := Offset+c;
  FLastReturnValue := RetVal;
  if RetVal<>0 then begin
    ErrText := ExtractErrorText(BytesCopy(Response, Offset, Length(Response)-Offset));
    if ErrText<>'' then
      Debug(Format('ParseMultiReadResponse: PLC retornou erro (returnValue=%d): %s',[RetVal,ErrText]))
    else
      Debug(Format('ParseMultiReadResponse: PLC retornou erro (returnValue=%d)',[RetVal]));
    exit;
  end;

  //Values section.
  while Offset<Length(Response) do begin
    ItemNr := DecodeUInt32VLQ(Response, Offset, c);
    Offset := Offset+c;
    if ItemNr=0 then break;
    if (ItemNr>=1) and (ItemNr<=Cardinal(ItemCount)) then begin
      Results[ItemNr-1].Data := DecodePValueToBytes(Response, Offset, c);
      Results[ItemNr-1].Ok := true;
      Offset := Offset+c;
    end else begin
      //Unexpected item index - still consume its PValue to keep the parse in sync.
      DecodePValueToBytes(Response, Offset, c);
      Offset := Offset+c;
    end;
  end;

  //Errors section (per-item failures within an otherwise-successful batch).
  while Offset<Length(Response) do begin
    ItemNr := DecodeUInt32VLQ(Response, Offset, c);
    Offset := Offset+c;
    if ItemNr=0 then break;
    DecodeUInt64VLQ(Response, Offset, c); //error value - not surfaced per-item yet
    Offset := Offset+c;
  end;

  Result := true;
end;

function TS7PlusConnection.ReadMultipleSymbolic(const Items:TS7PlusMultiReadItemArray; out Results:TS7PlusMultiReadResultArray):Boolean;
var
  Payload, Resp:TBytes;
begin
  SetLength(Results, 0);
  if Length(Items)=0 then exit(true);
  Payload := BuildMultiSymbolicReadPayload(Items);
  if not SendRequest(S7PlusFunc_GetMultiVariables, Payload, Resp) then exit(false);
  Result := ParseMultiReadResponse(Resp, Length(Items), Results);
end;

function TS7PlusConnection.ReadSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; out Data:TBytes; SymbolCrc:Cardinal):Boolean;
var
  Payload, Resp:TBytes;
begin
  Result := false;
  SetLength(Data, 0);
  Payload := BuildSymbolicPayload(AccessArea, Lids, nil, false, SymbolCrc);
  if not SendRequest(S7PlusFunc_GetMultiVariables, Payload, Resp) then exit;
  Result := ParseSingleReadResponse(Resp, Data);
end;

function TS7PlusConnection.WriteSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const Data:TBytes; SymbolCrc:Cardinal; SoftDataType:Byte):Boolean;
var
  Payload, Resp:TBytes;
begin
  Payload := BuildSymbolicPayload(AccessArea, Lids, Data, true, SymbolCrc, SoftDataType);
  Result := SendRequest(S7PlusFunc_SetMultiVariables, Payload, Resp) and ParseSingleWriteResponse(Resp);
end;

function TS7PlusConnection.Explore(ExploreId:Cardinal; const AttributeIds:array of Cardinal; out RespPayload:TBytes):Boolean;
var
  Payload:TBytes;
  i:Integer;
begin
  Payload := EncodeUInt32(ExploreId); //ExploreId (fixed UInt32, not VLQ)
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(0)); //ExploreRequestId (0=none)
  Payload := BytesConcat(Payload, BytesOf([1])); //ExploreChildsRecursive
  Payload := BytesConcat(Payload, BytesOf([1])); //unknown flag - protocol always sends 1
  Payload := BytesConcat(Payload, BytesOf([0])); //ExploreParents
  Payload := BytesConcat(Payload, BytesOf([0])); //number of filter objects (none)
  Payload := BytesConcat(Payload, EncodeUInt32VLQ(Length(AttributeIds))); //AddressList count
  for i:=0 to High(AttributeIds) do
    Payload := BytesConcat(Payload, EncodeUInt32VLQ(AttributeIds[i]));
  Payload := BytesConcat(Payload, EncodeUInt32(0)); //trailer: UInt32 fill
  Payload := BytesConcat(Payload, BytesOf([0])); //+ a single filler byte (integrity_tail=5)

  Result := SendRequest(S7PlusFunc_Explore, Payload, RespPayload, 5, true);
end;

function TS7PlusConnection.EnsureTypeInfoObjects:Boolean;
var
  RespPayload:TBytes;
  EmptyAttrs:array of Cardinal;
begin
  if FTypeInfoCached then begin
    Result := true;
    exit;
  end;
  SetLength(EmptyAttrs, 0);
  if FExploreDelayMs>0 then begin
    Debug(Format('EnsureTypeInfoObjects: aguardando %d ms antes do Explore (diagnostico)',[FExploreDelayMs]));
    Sleep(FExploreDelayMs);
  end;
  Result := Explore(S7PlusIds_ObjectOMSTypeInfoContainer, EmptyAttrs, RespPayload);
  if not Result then begin
    Debug('EnsureTypeInfoObjects: falha ao explorar o container de tipos (OMS)');
    exit;
  end;
  Result := ExtractS7PlusTypeInfoObjects(RespPayload, FTypeInfoObjects);
  FTypeInfoCached := Result;
  Debug(Format('EnsureTypeInfoObjects: %d objetos de tipo carregados',[Length(FTypeInfoObjects)]));
end;

function TS7PlusConnection.ReadDBTypeInfoRid(DBAccessArea:Cardinal; out TiRid:Cardinal):Boolean;
var
  Data:TBytes;
  Lids:TS7PlusLIDArray;
begin
  TiRid := 0;
  SetLength(Lids, 1);
  Lids[0] := 1;
  Result := ReadSymbolic(DBAccessArea, Lids, Data);
  if not Result then exit;
  if Length(Data)<4 then begin
    Result := false;
    exit;
  end;
  TiRid := DecodeUInt32(Data, 0);
end;

function TS7PlusConnection.BrowseDB(DBNumber:Cardinal; out Vars:TS7PlusVarInfoArray):Boolean;
var
  AccessArea, TiRid:Cardinal;
  RootNodes:TS7PlusNodeArray;
begin
  SetLength(Vars, 0);
  Result := false;
  AccessArea := S7PlusIds_DBAccessAreaBase + DBNumber;

  if not ReadDBTypeInfoRid(AccessArea, TiRid) then begin
    Debug(Format('BrowseDB: falha ao ler o RID de tipo da DB%d (LID=1)',[DBNumber]));
    exit;
  end;
  if TiRid=0 then begin
    Debug(Format('BrowseDB: DB%d sem valor legivel (provavelmente so de carga/load-memory)',[DBNumber]));
    exit;
  end;
  if not EnsureTypeInfoObjects then exit;

  SetLength(RootNodes, 1);
  FillChar(RootNodes[0], SizeOf(RootNodes[0]), 0);
  RootNodes[0].NodeType := ntRoot;
  RootNodes[0].Name := 'DB'+IntToStr(DBNumber);
  RootNodes[0].AccessId := AccessArea;
  RootNodes[0].RelationId := TiRid;

  S7PlusBuildTree(RootNodes, FTypeInfoObjects);
  Vars := S7PlusBuildFlatList(RootNodes);
  Result := true;
  Debug(Format('BrowseDB: DB%d -> %d variaveis',[DBNumber, Length(Vars)]));
end;

function TS7PlusConnection.BrowseNativeArea(AreaRID, TiRid:Cardinal; const AreaName:String; out Vars:TS7PlusVarInfoArray):Boolean;
var
  RootNodes:TS7PlusNodeArray;
begin
  SetLength(Vars, 0);
  Result := EnsureTypeInfoObjects;
  if not Result then exit;

  SetLength(RootNodes, 1);
  FillChar(RootNodes[0], SizeOf(RootNodes[0]), 0);
  RootNodes[0].NodeType := ntRoot;
  RootNodes[0].Name := AreaName;
  RootNodes[0].AccessId := AreaRID;
  RootNodes[0].RelationId := TiRid;

  S7PlusBuildTree(RootNodes, FTypeInfoObjects);
  Vars := S7PlusBuildFlatList(RootNodes);
  Debug(Format('BrowseNativeArea: %s -> %d variaveis',[AreaName, Length(Vars)]));
end;

function TS7PlusConnection.ListDataBlocks(out Blocks:TS7PlusDataBlockInfoArray):Boolean;
var
  RespPayload:TBytes;
  AttrIds:array[0..1] of Cardinal;
begin
  SetLength(Blocks, 0);
  AttrIds[0] := S7PlusIds_ObjectVariableTypeName;
  AttrIds[1] := S7PlusIds_BlockBlockNumber;
  Result := Explore(S7PlusIds_NativeThePLCProgramRID, AttrIds, RespPayload);
  if not Result then begin
    Debug('ListDataBlocks: falha ao explorar o programa do CLP');
    exit;
  end;
  Blocks := S7PlusParseExploreDataBlocks(RespPayload);
  Debug(Format('ListDataBlocks: %d DBs encontradas',[Length(Blocks)]));
end;

end.
