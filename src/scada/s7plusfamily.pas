{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Driver de protocolo S7CommPlus, para CLPs S7-1200/1500 da Siemens.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Fase 1: transporte + sessão V1 (sem TLS) e leitura/escrita por endereço numérico
  (área de memória nativa ou DB, sem resolução de Path simbólico ainda - isso é a
  Fase 2). Use um @code(TTCP_UDPPort) (unit tcp_udpport.pas) como CommunicationPort,
  apontando para a porta 102 do CLP.

  Ver @link(S7PlusConnection) para o transporte/sessão propriamente dito.
}
{$ELSE}
{:
  @abstract(S7CommPlus protocol driver, for Siemens S7-1200/1500 PLCs.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Phase 1: transport + V1 session (no TLS) and read/write by numeric address (native
  memory area or DB, no symbolic Path resolution yet - that's Phase 2). Use a
  @code(TTCP_UDPPort) (unit tcp_udpport.pas) as CommunicationPort, pointing to the
  PLC's port 102.

  See @link(S7PlusConnection) for the actual transport/session handling.
}
{$ENDIF}
unit S7PlusFamily;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Types, pascalScadaMTPCPU, ProtocolDriver, ProtocolTypes, Tag, CommPort, CommTypes,
  S7PlusTypes, S7PlusConnection, S7PlusTypeInfo, PLCTagNumber, PLCBlock,
  PLCString, S7PlusTagPlan;

//: Registers the Tag Builder tool of the S7CommPlus protocol (the design package does it,
//: since the tool has forms) - see TS7CommPlusDriver.OpenTagEditor.
procedure SetTagBuilderToolForS7CommPlusProtocol(TagBuilderTool:TOpenTagEditor);

const
  //: Smallest subscription cycle a PLC was seen to accept (a PLCSim refused 90 ms and below,
  //: for DB tags and for process area tags alike). SubscriptionCycleMs never goes below it.
  S7PlusMinSubscriptionCycleMs = 100;
  //: A live subscription notifies on every cycle, even with nothing changed (measured on a
  //: PLCSim: one notification per 100 ms cycle, CreditTick counting them). SubscriptionTimeoutMs
  //: is never shorter than this many cycles.
  S7PlusSubTimeoutMinCycles = 10;

  //: How long Username/Password must stay unchanged before an open session logs in again,
  //: so that they can be set one after the other without a login with half of the new pair.
  S7PlusAuthSettleMs = 500;

  //: Pause after asking the port for a new connection, before waiting for it: the port's
  //: connection thread works in steps of 250 ms and closes the old socket before opening the new.
  S7PlusReconnectGapMs = 800;

  //-- Phase 1 addressing convention for TTagRec.ReadFunction/WriteFunction.
  //-- Symbolic Path-based addressing (Phase 2) will replace this.
  S7PlusRF_DB       = 1;
  S7PlusRF_Inputs   = 2;
  S7PlusRF_Outputs  = 3;
  S7PlusRF_Flags    = 4;
  S7PlusRF_Counter  = 5;
  S7PlusRF_Timer    = 6;

type
  //-- Phase 2: symbolic (Path-based) tag scan bookkeeping. One entry per tag added via
  //-- DoAddTag (TPLCTagNumber/TPLCBlock/TPLCString/TPLCStruct with LongAddress set to a
  //-- symbolic Path such as "DB4.Var1" or "MArea.Clock_10Hz"). Path resolution (to an
  //-- AccessArea + LID path) is lazy - it happens on first scan, not on DoAddTag, since
  //-- DoAddTag runs on whatever thread sets the tag's LongAddress (typically the main/UI
  //-- thread) and may run before the port is even open.
  //-- One top-level member of a "whole DB/area" composite entry (see below): a single
  //-- independently-addressable variable, read as its own item within the entry's own
  //-- batched request, then spliced into the entry's assembled Data at ByteOffset.
  TS7PlusCompositeItem = record
    AccessArea:Cardinal;
    Lids:TS7PlusLIDArray;
    ByteOffset:Integer;
    //-- Used to write the entry back: SoftDataType types the member's PValue, WriteSize is its
    //-- byte width in the assembled buffer. WriteSize=0: not writable as a member (BOOL, whose
    //-- byte is shared with other bits, STRING, structs, unknown types) - skipped on write.
    SoftDataType:Byte;
    WriteSize:Integer;
    //-- Bytes a read of this member returns (TS7PlusVarInfo.DataSize), 0 = not known.
    ReadSize:Integer;
  end;
  TS7PlusCompositeItemArray = array of TS7PlusCompositeItem;

  TS7PlusScanEntry = record
    TagObj:TTag;
    Path:AnsiString;
    Resolved:Boolean;
    AccessArea:Cardinal;
    Lids:TS7PlusLIDArray;
    ByteOffset:Integer;   //the resolved ImageOffset (position in the PLC memory image, see TS7PlusVarInfo.ImageOffset) - used by the read-dedup. -1=unknown.
    SoftDataType:Byte;
    ReadSize:Integer;     //bytes a read of it returns (TS7PlusVarInfo.DataSize), 0 = not known
    Data:TBytes;
    LastResult:TProtocolIOResult;
    LastScanTimeStamp:QWord;
    LastResolveAttempt:QWord;
    UpdateRate:LongInt;
    //-- Read-dedup: when another already-independently-scanned entry's Lids is a strict
    //-- prefix of this one's Lids (same AccessArea, both ByteOffset<>-1), this entry never
    //-- gets its own read - DoGetValue slices its bytes out of the container entry's Data
    //-- instead. See TS7CommPlusDriver.TryDeduplicateEntry.
    IsDerived:Boolean;
    ContainerPath:AnsiString;
    DerivedOffset:Integer; //offset within the container's own Data (already Self.ByteOffset - Container.ByteOffset)
    DerivedLen:Integer;
    //-- "Whole DB/area" tag: Path is a bare root (e.g. "DB4"/"BlocoSimbolico_4", no dot).
    //-- Reading the root's own (empty) LID path returns an unrelated small metadata blob,
    //-- not real memory content (confirmed against real hardware) - so instead this entry's
    //-- Data is assembled every scan round from a batched read of every one of the DB/area's
    //-- own top-level members (CompositeItems), spliced together at each member's own
    //-- ByteOffset. See TS7CommPlusDriver.ResolvePath and DoScanRead.
    IsComposite:Boolean;
    CompositeItems:TS7PlusCompositeItemArray;
    //-- On-change delivery: a tag with RefreshTime=0 is not polled, the PLC pushes it
    //-- through a subscription (see TS7CommPlusDriver.ManageSubscriptions). Data,
    //-- LastResult and LastScanTimeStamp are then updated by notifications instead of by
    //-- DoScanRead's batched read.
    OnChange:Boolean;
    Subscribed:Boolean;   //covered by a live subscription (cleared when the session is lost)
    SubFallback:Boolean;  //the subscription could not be created: polled instead, slowly
    SubRefBase:Cardinal;  //RefId of this entry's first item; a composite's member j is SubRefBase+j
  end;

  //-- One subscription object created on the PLC. NextCreditLimit follows the PLC's
  //-- CreditTick so a long-running subscription is never starved of credit.
  TS7PlusSubscriptionState = record
    ObjectId:Cardinal;
    NextCreditLimit:SmallInt;
    LastNotificationAt:QWord;   //GetTickCount64 of its creation, then of every notification
  end;

  //-- Cache of a DB's or native area's flat variable list (Name/Lids/SoftDataType), so
  //-- resolving many tags on the same DB/area doesn't re-EXPLORE for every single tag.
  TS7PlusDBCacheEntry = record
    DBNumber:Cardinal;
    TiRid:Cardinal;           //the type-info object of the DB (to ask its modification time)
    Vars:TS7PlusVarInfoArray;
  end;

  //-- The modification time of the type-info object of a DB the driver keeps browsed.
  TS7PlusTypeTime = record
    TiRid:Cardinal;
    HasTime:Boolean;          //the PLC reported one
    Time:TBytes;
  end;
  TS7PlusTypeTimeArray = array of TS7PlusTypeTime;

  //-- A DB browsed (by a tag's path or by BrowseDB, e.g. the Tag Builder) and its type-info object:
  //-- the program is checked against what these looked like.
  TS7PlusWatchedDB = record
    DBNumber:Cardinal;
    TiRid:Cardinal;
  end;
  TS7PlusWatchedDBArray = array of TS7PlusWatchedDB;

  { TS7CommPlusDriver }

  //: A tag the driver could read along with the request of this round (ReadSomethingAlways).
  TS7PlusFillCandidate = record
    Index:Integer;     //its place in the scan list
    Age:Int64;         //ms since its last read
    Items:Integer;     //wire items its read takes
    Bytes:Integer;     //bytes its reply takes in the request
  end;
  TS7PlusFillCandidateArray = array of TS7PlusFillCandidate;

  TS7CommPlusDriver = class(TProtocolDriver)
  private
    FConnection:TS7PlusConnection;
    FOnDebug:TS7PlusDebugEvent;
    FUseTLS:Boolean;
    FExploreDelayMs:Integer;
    FUsername, FPassword:AnsiString;
    FServerCertificate:AnsiString;
    FCertificateChanged:Boolean;   //ServerCertificate changed: the open session is not trusted any more
    FAccessLevel:Cardinal;      //the session's protection level (S7PlusAccessLevel_*), 0 = unknown
    FLegitimated:Boolean;       //the session logged in with Username/Password
    FAuthPending:Boolean;       //Username/Password changed: log in again once they settle
    FAuthChangedAt:QWord;
    FSessionTainted:Boolean;    //a login was refused on this session: the PLC takes no second one on it

    FScanList:array of TS7PlusScanEntry;
    FDBCache:array of TS7PlusDBCacheEntry;
    FIAreaVars, FQAreaVars, FMAreaVars, FTimersVars, FCountersVars:TS7PlusVarInfoArray;
    FIAreaBrowsed, FQAreaBrowsed, FMAreaBrowsed, FTimersBrowsed, FCountersBrowsed:Boolean;
    FDBNameCache:TS7PlusDataBlockInfoArray;
    FDBNameCacheLoaded:Boolean;

    //-- On-change tags (RefreshTime=0) delivered through subscriptions.
    FSubs:array of TS7PlusSubscriptionState;
    FSubNextRefId:Cardinal;
    FSubFailCount:Integer;
    FSubRetryAt:QWord;
    FSubResetPending:Boolean; //set by whoever notices the session is gone, consumed by the scan thread
    FSubCycleMs:Integer;
    FSubCreditLimit:SmallInt;
    FSubTimeoutMs:Integer;
    //-- What the program looked like when the browse caches were built, to tell when it changes.
    FProgramCheckMs:Integer;
    FLastProgramCheck:QWord;
    FProgramKnown:Boolean;
    FProgramSignature:String;
    FTypeTimes:TS7PlusTypeTimeArray;
    FWatch:TS7PlusWatchedDBArray;
    FProgramChangeWhy:String;
    FMaxItemsPerRequest, FMaxTagsPerScan, FMaxItemsPerSubscription:Integer;

    function GetUseTLS:Boolean;
    procedure SetUseTLS(AValue:Boolean);
    procedure SetMaxItemsPerRequest(AValue:Integer);
    procedure SetUsername(const AValue:AnsiString);
    procedure SetPassword(const AValue:AnsiString);
    procedure SetServerCertificate(const AValue:AnsiString);
    function GetPeerCertificate:AnsiString;
    procedure SetSubCycleMs(AValue:Integer);
    procedure SetSubTimeoutMs(AValue:Integer);
    procedure SetProgramCheckMs(AValue:Integer);
    //: RefreshCaches without the I/O lock, for the scan thread (which has it already).
    procedure DropCaches;
    //: A DB was browsed: it is watched from now on, and what its type looked like is learned
    //: (with the list of DBs, if that is not known yet).
    procedure LearnProgram(DBNumber, TiRid:Cardinal);
    //: Compares the list of DBs just read, and the time of every watched type, with what they were.
    procedure CheckProgramNow(const Blocks:TS7PlusDataBlockInfoArray);
    procedure SetMaxTagsPerScan(AValue:Integer);
    procedure SetMaxItemsPerSubscription(AValue:Integer);
    function GetExploreDelayMs:Integer;
    procedure SetExploreDelayMs(AValue:Integer);
    function EnsureConnected:Boolean;
    function GetConnected:Boolean;
    function AreaRIDOf(ReadFunction:LongInt; out AreaRID:Cardinal):Boolean;
    procedure ConnDebug(Sender:TObject; const Msg:String);
    procedure LockDriverIO;
    procedure UnlockDriverIO;

    //-- Phase 2: symbolic Path resolution (cached) and scan-list lookups.
    function GetCachedDBVars(DBNumber:Cardinal; out Vars:TS7PlusVarInfoArray):Boolean;
    function GetCachedAreaVars(AreaRID, TiRid:Cardinal; const AreaName:String; var Cache:TS7PlusVarInfoArray; var Browsed:Boolean; out Vars:TS7PlusVarInfoArray):Boolean;
    function GetDBNumberByName(const Name:AnsiString; out DBNumber:Integer):Boolean;
    function ResolvePath(const Path:AnsiString; out AccessArea:Cardinal; out Lids:TS7PlusLIDArray; out ByteOffset:Integer; out SoftDataType:Byte; out CompositeItems:TS7PlusCompositeItemArray; out DataSize:Integer; out ElementItems:TS7PlusCompositeItemArray):Boolean;
    //: True for an array of structures ("DB4.Arr") or one element of it ("DB4.Arr[2]").
    function IsStructArrayPath(const Vars:TS7PlusVarInfoArray; const CanonicalPath:AnsiString):Boolean;
    //: Adds, as composite items, every member of the elements of an array of structures (or of
    //: one element) at its classic byte offset minus BaseOffset - reading the structure as a
    //: whole does not work for an array of structures (the PLC answers with a value this driver
    //: does not decode), so it is read member by member and put together.
    procedure AddStructArrayMembers(const Vars:TS7PlusVarInfoArray; const CanonicalPath:AnsiString;
                                    AccessArea:Cardinal; BaseOffset:Integer;
                                    var Items:TS7PlusCompositeItemArray);
    function TagPathOf(TagObj:TTag; out Path:AnsiString):Boolean;
    function FindScanEntry(const Path:AnsiString; out Index:Integer):Boolean;
    function IsLidsPrefix(const Prefix, Full:TS7PlusLIDArray):Boolean;
    procedure TryDeduplicateEntry(EntryIdx:Integer);
    //The wire items an entry takes in a batched read: one, or one per member of a composite.
    function EntryReadItems(EntryIdx:Integer):TS7PlusMultiReadItemArray;
    function ReadComposite(const Items:TS7PlusCompositeItemArray; out Data:TBytes):Boolean;

    //-- On-change tags (subscriptions).
    function EntryItemCount(Idx:Integer):Integer;
    function FindEntryByRefId(RefId:Cardinal; out EntryIdx, ItemIdx:Integer):Boolean;
    procedure ManageSubscriptions(Now_:QWord);
    procedure PumpNotifications(WaitMs:LongInt);
    procedure RenewCredit(SubscriptionId:Cardinal; CreditTick:Byte);
  protected
    function CompositeWriteSize(SoftDataType:Byte):Integer;
    //: Appends to Items one write per writable member of a whole-DB/area entry, each cut out
    //: of Values (the tag's bytes, laid out like ReadComposite assembles them) at its
    //: ByteOffset. Members that are not writable or lie beyond Values are left out.
    //: Returns how many were appended.
    //: The elements of an array of scalars as write items (see ResolvePath); nothing when it is not
    //: one, or when the elements listed do not add up to the size of the array.
    procedure CollectArrayElements(const Vars:TS7PlusVarInfoArray; const CanonicalPath:AnsiString;
                                   AccessArea:Cardinal; ArraySize:Integer; var Items:TS7PlusCompositeItemArray);
    function ExpandCompositeWrite(const Composite:TS7PlusCompositeItemArray; const Values:TArrayOfDouble; var Items:TS7PlusMultiWriteItemArray; BaseOffset:Integer = 0):Integer;
    //: A write of PART of a symbolic value (one element of an array, from byte Offset): reads the
    //: current value, puts Values over it from Offset and gives the whole of it back, which is
    //: what the PLC takes (it writes a variable whole, never a slice of it).
    function PatchedValue(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; Offset:Integer;
                          const Values:TArrayOfDouble; out Whole:TBytes):Boolean;
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    //: Gathers the symbolic (Path) writes of the batch into as few SetMultiVariables requests
    //: as possible (see TS7PlusConnection.WriteMultipleSymbolic); tags addressed numerically
    //: (DB/area + offset) are still written one by one, as DoWrite does.
    procedure DoMultiWrite(var Writes:TMultiWriteArray; Sync:Boolean); override;
    function DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    procedure DoDelTag(TagObj:TTag); override;

    //-- On-change bookkeeping steps that ManageSubscriptions/the scan thread drive; protected
    //-- so the tests can drive them without a PLC.
    //: Records that the entry of Path is covered by a subscription whose first item has id RefBase.
    function MarkSubscribed(const Path:AnsiString; RefBase:Cardinal):Boolean;
    //: Stands in for path resolution (which needs the PLC): marks the entry of Path resolved,
    //: as a composite of Composite members when any are given.
    function MarkResolved(const Path:AnsiString; const Composite:TS7PlusCompositeItemArray):Boolean;
    //: Whether the entry of Path is resolved, and how many DBs the driver keeps browsed (tests).
    function EntryResolved(const Path:AnsiString):Boolean;
    //: What the program looks like now (the signature of the DB list and the modification time of
    //: the type of each DB kept browsed) against what it looked like when the caches were built.
    //: The first observation is the baseline. A difference drops the caches (DropCaches).
    //: True when the program changed.
    function NoteProgram(const Signature:String; const Times:TS7PlusTypeTimeArray):Boolean;
    //: Puts a DB on the list of the watched ones (no I/O) and says how many there are (tests).
    procedure WatchDB(DBNumber, TiRid:Cardinal);
    function WatchedDBCount:Integer;
    //: Reads that observation from the PLC at most every ProgramCheckIntervalMs and calls
    //: NoteProgram: 1 + N small requests (N = DBs kept browsed).
    procedure CheckProgramChange(Now_:QWord);
    function BrowsedDBCount:Integer;
    //: The session is gone (or brand new), and with it every subscription - see the body.
    procedure ResetSubscriptions;

    //-- Authentication state: what the session may do, and what the tags are told about it.
    //: True when the PLC protects what the session needs (its protection level is above full
    //: access) and the session has not logged in: every failure then is an authentication one.
    function SessionNeedsAuth:Boolean;
    //: The result of a failed operation: ioAuthError when SessionNeedsAuth, else ioCommError.
    function FailResult:TProtocolIOResult;
    //: Logs the open session in with the given credentials. True when the PLC accepted them
    //: (or the session needed none).
    function AuthenticateSession(const APassword, AUsername:AnsiString):Boolean;
    //: Closes the session and the TCP connection under it and opens them again. Needed to log
    //: in again after a refused login: a PLC (seen on a PLCSim) refuses every login after a
    //: failed one on the same session, whatever the password. False if the port did not come back.
    function RestartSession:Boolean;
    //: Tells the tags that cannot be read for lack of authentication (path not resolved, no
    //: notification delivered) with ioAuthError - see SessionNeedsAuth.
    procedure MarkAuthErrors(Now_:QWord);
    //: The session logged in: what was refused for lack of authentication is tried again.
    procedure ClearAuthErrors;
    //-- The authentication state, readable and settable by the tests (and by descendants).
    property AccessLevel:Cardinal read FAccessLevel write FAccessLevel;
    property Legitimated:Boolean read FLegitimated write FLegitimated;
    property AuthPending:Boolean read FAuthPending write FAuthPending;
    property SubResetPending:Boolean read FSubResetPending write FSubResetPending;

    //: Notes a subscription made on the PLC (state kept for the credit renewal and the watchdog).
    procedure RegisterSubscription(ObjectId:Cardinal; CreditLimit:SmallInt; Now_:QWord);
    //: A notification arrived for the subscription: it is alive.
    procedure TouchSubscription(ObjectId:Cardinal; Now_:QWord);
    //: How long a subscription may stay silent: SubscriptionTimeoutMs, but never under
    //: S7PlusSubTimeoutMinCycles cycles; 0 = watchdog off.
    function SubscriptionTimeoutMsNow:Integer;
    //: Watchdog: a subscription silent for longer than that while the session stays up (the
    //: PLC dropped it, ran out of credit, ...) makes the subscriptions be created again.
    procedure CheckSubscriptionWatchdog(Now_:QWord);
    function NotifyThisEvents:TNotifyThisEvents; override;
    procedure PortDisconnected(Sender:TObject); override;
    procedure PortClosed(Sender:TObject); override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    function SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;

    //: The certificate the PLC presented in the last TLS handshake, as the base64 of its DER
    //: ('' until a connection was made). Copy it into ServerCertificate to trust that PLC only.
    property PeerCertificate:AnsiString read GetPeerCertificate;

    //-- Phase 1 direct access helpers (numeric addressing, no Path/tag framework yet).
    //: Reads bytes from a native controller memory area (Inputs/Outputs/Flags/Counters/Timers).
    function ReadArea(ReadFunction:LongInt; Start, Size:Integer; out Data:TBytes):TProtocolIOResult;
    //: Writes bytes to a native controller memory area (Inputs/Outputs/Flags/Counters/Timers).
    function WriteArea(ReadFunction:LongInt; Start:Integer; const Data:TBytes):TProtocolIOResult;
    //: Reads bytes from a data block.
    function ReadDB(DBNumber, Start, Size:Integer; out Data:TBytes):TProtocolIOResult;
    //: Writes bytes to a data block.
    function WriteDB(DBNumber, Start:Integer; const Data:TBytes):TProtocolIOResult;

    //-- Phase 2: symbolic (LID-based) access - the only reliable way to read/write DB
    //-- variables and native-area named tags on real S7-1200/1500 firmware.
    //: Browses a DB's variables (name + LID path + softdatatype) via EXPLORE.
    function BrowseDB(DBNumber:Cardinal; out Vars:TS7PlusVarInfoArray):TProtocolIOResult;
    //: Browses a native process area (use the AreaRID/TiRid constants below).
    function BrowseNativeArea(AreaRID, TiRid:Cardinal; const AreaName:String; out Vars:TS7PlusVarInfoArray):TProtocolIOResult;
    //: Lists every DB in the PLC program with its symbolic (TIA Portal) name and number.
    function ListDataBlocks(out Blocks:TS7PlusDataBlockInfoArray):TProtocolIOResult;
    //: Drops everything the driver keeps from browsing the PLC: the type information, the
    //: variables of each DB and process area, the DB names. Call it after a download to the PLC
    //: (a new DB, a changed one), or the browse keeps answering with the program as it was when
    //: it was first read. The tags already created are resolved again at the next scan (their
    //: address may have changed) and the on-change ones subscribe again.
    procedure RefreshCaches;
    //: Reads the value of every STRING on the candidates list (S7PlusAddBrowsed) once and sets
    //: its StringLength (the maximum, the first byte of the value); the ones that cannot be read
    //: become srStringLength (not tags). ioOk, ioPartialOk when some could not be read.
    function ResolveStringLengths(var Candidates:TS7PlusCandidateArray):TProtocolIOResult;
    //: @seealso(TProtocolDriver.OpenTagEditor)
    procedure OpenTagEditor(InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); override;
    //: @seealso(TProtocolDriver.HasTabBuilderEditor)
    function HasTabBuilderEditor:Boolean; override;
    //: @seealso(TProtocolDriver.SupportsListTags)
    function SupportsListTags:Boolean; override;
    //: Lists every DB of the PLC (each as a tkStructure with the size of a whole DB read)
    //: and the variables in them, plus the I, Q and M process areas, in the common format of
    //: TProtocolDriver.ListTags. The path is symbolic, with the TIA Portal name of the DB
    //: (BlocoSimbolico_4.Var26.v1). A scalar is a tkScalar with its TagType and the Swap* the
    //: big-endian PLC needs, an array of scalars a tkArray (its elements are left out), a
    //: structure a tkStructure whose members follow as entries of their own, a STRING a
    //: tkString with its maximum length. ForceReload=True calls RefreshCaches first: use it after a
    //: download to the PLC, otherwise the type information read at the first browse is used.
    //: ioPartialOk: a DB or area could not be browsed.
    function ListTags(out Tags:TPLCTagInfoArray; ForceReload:Boolean = false):TProtocolIOResult; override;
    //: Reads a variable's raw bytes by its resolved LID path (see BrowseDB/BrowseNativeArea).
    function ReadSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; out Data:TBytes):TProtocolIOResult;
    //: Writes a variable's raw bytes by its resolved LID path.
    //: SoftDataType (from BrowseDB) is required for the PLC to accept the write - it's used
    //: to encode Data as the target's own typed PValue (ValueDInt, ValueReal, ...) rather
    //: than a generic byte array, which the PLC rejects outright. Default 0 falls back to a
    //: byte array (only correct for genuine byte-array-typed targets).
    //: Writes several symbolic items in as few requests as possible - see
    //: TS7PlusConnection.WriteMultipleSymbolic. ioOk with Results[i].Ok per item; ioCommError
    //: when a request failed as a whole (then Results says which items were taken before it).
    function WriteMultipleSymbolic(const Items:TS7PlusMultiWriteItemArray; out Results:TS7PlusMultiWriteResultArray):TProtocolIOResult;
    function WriteSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const Data:TBytes; SoftDataType:Byte=0):TProtocolIOResult;
    //: Reads several symbolic items in one round-trip - see TS7PlusConnection.ReadMultipleSymbolic.
    function ReadMultipleSymbolic(const Items:TS7PlusMultiReadItemArray; out Results:TS7PlusMultiReadResultArray):TProtocolIOResult;
    //: Sets Username and Password in one step and logs the session in with them: with a session
    //: open it logs in again right now (without waiting for the properties to settle); with none
    //: it opens one, which logs in by itself. ioOk: the session has access; ioAuthError: the PLC
    //: refused the credentials, or wants some; ioCommError: no connection. The tags that waited
    //: for authentication are read again and the on-change ones subscribed again.
    function Auth(const AUsername, APassword:AnsiString):TProtocolIOResult;
    //: Performs PLC password authentication (legitimation) - see TS7PlusConnection.Authenticate.
    //: Needed by some PLCs before writes are accepted (reads can remain open/unauthenticated).
    function Authenticate(const Password:AnsiString; const Username:AnsiString=''):TProtocolIOResult;
    //: Direct (low-level, not yet wired into the tag/scan framework) access to
    //: subscriptions - see TS7PlusConnection.SubscriptionCreate/SubscriptionSetCreditLimit/
    //: SubscriptionDelete/ParseNotification.
    function SubscriptionCreate(const Items:TS7PlusMultiReadItemArray; CycleTimeMs:Word;
                                 RouteMode:Byte=$14; UseFixedRelationId:Boolean=false;
                                 IncludeAttr1055:Boolean=true; CreditLimitOverride:SmallInt=10):TProtocolIOResult;
    function SubscriptionSetCreditLimit(Limit:SmallInt):TProtocolIOResult;
    function SubscriptionDelete:TProtocolIOResult;
    //: Blocks (up to CommunicationPort's own configured Timeout) for one incoming frame
    //: and, if it's a Notification, parses it. ioCommError on a comm/parse failure or a
    //: non-Notification frame arriving here (e.g. a stray response) - direct API calls and
    //: the scan thread must not race on the same connection, so nothing else should be
    //: expecting a reply while this is pending. ioOk with CreditTick/Values populated on
    //: success.
    function SubscriptionWaitNotification(out CreditTick:Byte; out Values:TS7PlusNotificationValueArray):TProtocolIOResult; overload;
    //: Same as above, also returning the items the PLC reported as failed (Errors) instead of a value.
    function SubscriptionWaitNotification(out CreditTick:Byte; out Values:TS7PlusNotificationValueArray;
                                          out Errors:TS7PlusNotificationErrorArray):TProtocolIOResult; overload;
    //: Uploads (reads) a compiled block's raw body - see TS7PlusConnection.UploadBlock.
    function UploadBlock(BlockType, BlockNumber:Cardinal; out Data:TBytes):TProtocolIOResult;

    //: Applies one parsed notification to the on-change tags: each value becomes its tag's
    //: new data (stamped Now_, which is what makes the framework push it to the tag), each
    //: per-item error marks its tag as failed. Ids that match no tag (a tag removed while its
    //: subscription lives on) are ignored. Called by the scan thread; public so the
    //: bookkeeping can be unit tested without a PLC.
    procedure ProcessNotification(const Values:TS7PlusNotificationValueArray;
                                  const Errors:TS7PlusNotificationErrorArray; Now_:QWord);

    //: True when the S7CommPlus session is established (and, if UseTLS, the TLS tunnel
    //: is active and IntegrityId tracking is enabled).
    property Connected:Boolean read GetConnected;
    //: Diagnostic trace of every relevant protocol step (COTP/InitSSL/TLS handshake/
    //: CreateObject/SetupSession/GetMultiVariables/SetMultiVariables). Assign it to
    //: troubleshoot a connection against real hardware/PLCSim.
    property OnDebug:TS7PlusDebugEvent read FOnDebug write FOnDebug;
    //: Diagnostic only: milliseconds to sleep right before the structured Explore(OMS
    //: TypeInfo container) request fired by BrowseDB/BrowseNativeArea. Default 0.
    property ExploreDelayMs:Integer read GetExploreDelayMs write SetExploreDelayMs;
  published
    property CommunicationPort;
    //: Activates TLS (after InitSSL, before CreateObject) - required by most
    //: S7-1200/1500 firmware from TIA Portal V15+ for anything beyond the initial
    //: handshake. Default true; set to false only for known-legacy V1-only firmware.
    property UseTLS:Boolean read GetUseTLS write SetUseTLS default true;
    //: Most items sent to the PLC in ONE read or write request. A bigger batch (many tags due at
    //: once, a WriteMultipleSymbolic with many items) is split into several requests, so this
    //: bounds the request, not how many tags a scan can serve. Default 100: a PLC refuses
    //: requests with more items than it supports (101 was refused when measured on a PLCSim; the
    //: protocol documentation gives no figure, so check the PLC at hand before raising it).
    property MaxItemsPerRequest:Integer read FMaxItemsPerRequest write SetMaxItemsPerRequest default S7PlusDefaultMaxItemsPerRequest;
    //: Most tags read in ONE scan round (each round = the requests needed for that many tags).
    //: Tags beyond it stay due and are served by the next round, so this only paces catch-up
    //: after a burst (right after connecting, for instance) and bounds how long a round holds
    //: the connection; it never drops a tag. A "whole DB" tag counts as one. Default 500.
    property MaxTagsPerScan:Integer read FMaxTagsPerScan write SetMaxTagsPerScan default 500;
    //: When true, a scan round that has to read something also reads the polled tags that are
    //: NOT due, as long as their replies fit in the request already going out (the item limit and
    //: the size of the reply, see MaxItemsPerRequest): the room is free, so those tags get
    //: fresher values for no extra round-trip. They are taken by the time since their last read,
    //: the oldest first; one that does not fit is skipped. A round with nothing due reads nothing,
    //: and tags that are on change are not touched. Unlike the other drivers (S7 ISO, Modbus) the
    //: default here is false. @seealso(TProtocolDriver.ReadSomethingAlways)
    property ReadSomethingAlways default false;
    //: When true no write reaches the PLC: every write (tags, WriteMultipleSymbolic through the
    //: tags) is answered with ioReadOnlyProtocol. @seealso(TProtocolDriver.ReadOnly)
    property ReadOnly;
    //: Most items in ONE subscription (one CreateObject request, and the size of its first
    //: notification). Tags beyond it get a subscription of their own. Default 500. Unlike the
    //: request limit this one is about size: a PLCSim took 2000 two-LID items (~32 KB) and refused
    //: 2005, so how many fit depends on how long the tags' LID paths are.
    property MaxItemsPerSubscription:Integer read FMaxItemsPerSubscription write SetMaxItemsPerSubscription default 500;
    //: Sampling cycle, in milliseconds, of the subscriptions that deliver tags whose
    //: RefreshTime is 0 (on change): how often the PLC looks for a change, so a value that
    //: flips faster than half of it can go unnoticed (a 10 Hz clock flips every 50 ms: with
    //: 100 ms it is always seen in the same state). The PLC takes nothing below 100
    //: (S7PlusMinSubscriptionCycleMs; a shorter subscription is refused and the tags fall
    //: back to 1 s polling), so a lower value is set to 100. For something faster, give the
    //: tag a RefreshTime above 0 (polling).
    property SubscriptionCycleMs:Integer read FSubCycleMs write SetSubCycleMs default 100;
    //: Notification credit of those subscriptions. A positive value is renewed
    //: automatically (the PLC stops sending once its CreditTick reaches the limit); -1 asks
    //: for unlimited credit with no renewal (documented by the S7CommPlusDriver author, not
    //: yet validated here). Default 10, the value real S7-1500 PLCs are known to accept.
    property SubscriptionCreditLimit:SmallInt read FSubCreditLimit write FSubCreditLimit default 10;
    //: Watchdog of the subscriptions, in milliseconds: a live subscription notifies on every
    //: cycle even when no value changed, so one that says nothing for this long, with the
    //: session still answering, was lost (dropped by the PLC, out of credit...): the driver
    //: creates the subscriptions again (the tags are flagged as failed until the new ones deliver
    //: the current values). Never less than 10 cycles (SubscriptionCycleMs). 0 turns it off.
    //: Default 5000.
    property SubscriptionTimeoutMs:Integer read FSubTimeoutMs write SetSubTimeoutMs default 5000;
    //: How often, in milliseconds, the driver looks for a download to the PLC while there are tags
    //: in use: it compares the list of DBs and the modification time of the type of each DB
    //: kept browsed with what they were when the tags were resolved; if something differs, the
    //: addresses of the tags are resolved again and the on-change tags subscribe again. It costs
    //: 1 + N small requests (N = DBs browsed). 0 turns it off; a smaller value than 1000 is set
    //: to 1000. Default 10000. This narrows the window in which an address of a program that
    //: was replaced is still used; it does not close it (a change that leaves the DB list and the
    //: modification time of the types as they were, or a PLC that does not report the time, goes
    //: unnoticed). RefreshCaches forces it.
    property ProgramCheckIntervalMs:Integer read FProgramCheckMs write SetProgramCheckMs default 10000;
    //: PLC password for legitimation (V2+ with TLS) - some PLCs (protection level "Full
    //: access with password" in TIA Portal) reject writes from an unauthenticated session
    //: even though reads stay open. When set (non-empty), the driver calls Authenticate
    //: automatically right after each new connection is established (see EnsureConnected).
    //: Changing it while a session is open logs in again with the new value (a moment after
    //: the last change, so Username and Password can be set one after the other; Auth does
    //: both at once). Leave empty to skip legitimation entirely (the default - matches every PLC that
    //: doesn't require it). Currently only the challenge-retrieval step of legitimation is
    //: confirmed working against real hardware; the PLC still rejects the encrypted
    //: response as of this writing - see TS7PlusConnection.Authenticate.
    property Password:AnsiString read FPassword write SetPassword;
    //: The certificate(s) of the PLC to trust, as ONE base64 string of the DER (in the Object
    //: Inspector the "..." button loads it from a .pem/.crt/.cer/.der file; several certificates
    //: may go in it, e.g. the CA of the project). EMPTY: whatever certificate the PLC presents is
    //: accepted (as before). With a value, the TLS handshake only succeeds if the certificate the
    //: PLC presents is one of them or is signed by one of them: a different one - another PLC, or a
    //: device in between - makes the connection fail (the OnDebug message says why). The name in
    //: the certificate is NOT compared (PLC certificates rarely carry their IP address). A
    //: certificate that cannot be read also fails the connection; it never falls back to accepting
    //: any. Changing it closes the session that is open, so the next one is checked.
    property ServerCertificate:AnsiString read FServerCertificate write SetServerCertificate;
    //: Optional username for "new"-style (AES-256-CBC) legitimation. Leave empty for a
    //: plain password login (the common case) - see Password.
    property Username:AnsiString read FUsername write SetUsername;
  end;

//: The Index of the candidates that go along in a request that has room for ItemsLeft items and
//: BytesLeft bytes of reply, at most MaxCount of them: the oldest read first (descending Age; a
//: tie keeps the scan list order), and one that does not fit is skipped, a younger one that fits
//: still goes. Candidates are all or nothing: a tag with several items is never cut.
function S7PlusPickFillers(const Candidates:TS7PlusFillCandidateArray; ItemsLeft, BytesLeft, MaxCount:Integer):TIntegerDynArray;

implementation

var
  S7PlusTagBuilderEditor:TOpenTagEditor = nil;

procedure SetTagBuilderToolForS7CommPlusProtocol(TagBuilderTool:TOpenTagEditor);
begin
  if Assigned(S7PlusTagBuilderEditor) then
    raise Exception.Create('A Tag Builder editor for the S7CommPlus protocol was already assigned.')
  else
    S7PlusTagBuilderEditor := TagBuilderTool;
end;

{ TS7CommPlusDriver }

constructor TS7CommPlusDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FUseTLS := true;
  PReadSomethingAlways := false;
  PDeliverReadAhead := true;      //DoScanRead reads tags ahead of their time: their values must reach the tags

  FSubCycleMs := 100;
  FSubCreditLimit := 10;
  FSubTimeoutMs := 5000;
  FProgramCheckMs := 10000;
  FMaxItemsPerRequest := S7PlusDefaultMaxItemsPerRequest;
  FMaxTagsPerScan := 500;
  FMaxItemsPerSubscription := 500;
  FSubNextRefId := 1;
end;

destructor TS7CommPlusDriver.Destroy;
begin
  FreeAndNil(FConnection);
  inherited Destroy;
end;

function TS7CommPlusDriver.GetConnected:Boolean;
begin
  Result := (FConnection<>nil) and FConnection.Connected;
end;

procedure TS7CommPlusDriver.ConnDebug(Sender:TObject; const Msg:String);
begin
  if Assigned(FOnDebug) then
    FOnDebug(Self, Msg);
end;

function TS7CommPlusDriver.GetUseTLS:Boolean;
begin
  Result := FUseTLS;
end;

procedure TS7CommPlusDriver.SetUseTLS(AValue:Boolean);
begin
  FUseTLS := AValue;
  if FConnection<>nil then
    FConnection.UseTLS := AValue;
end;

procedure TS7CommPlusDriver.SetMaxItemsPerRequest(AValue:Integer);
begin
  if AValue<1 then AValue := 1;
  FMaxItemsPerRequest := AValue;
  if FConnection<>nil then
    FConnection.MaxItemsPerRequest := AValue;
end;

procedure TS7CommPlusDriver.SetMaxTagsPerScan(AValue:Integer);
begin
  if AValue<1 then AValue := 1;
  FMaxTagsPerScan := AValue;
end;

procedure TS7CommPlusDriver.SetMaxItemsPerSubscription(AValue:Integer);
begin
  if AValue<1 then AValue := 1;
  FMaxItemsPerSubscription := AValue;
end;

function TS7CommPlusDriver.GetExploreDelayMs:Integer;
begin
  Result := FExploreDelayMs;
end;

procedure TS7CommPlusDriver.SetExploreDelayMs(AValue:Integer);
begin
  FExploreDelayMs := AValue;
  if FConnection<>nil then
    FConnection.ExploreDelayMs := AValue;
end;

function TS7CommPlusDriver.EnsureConnected:Boolean;
var
  WasConnected:Boolean;
begin
  Result := false;
  if (PCommPort=nil) or (not PCommPort.ReallyActive) then exit;

  if FConnection=nil then begin
    FConnection := TS7PlusConnection.Create(PCommPort, DriverID);
    FConnection.OnDebug := ConnDebug;
    FConnection.UseTLS := FUseTLS;
    FConnection.ServerCertificate := FServerCertificate;
    FConnection.ExploreDelayMs := FExploreDelayMs;
    FConnection.MaxItemsPerRequest := FMaxItemsPerRequest;
  end;

  //a session opened under another certificate (or none) is not the one that is asked for now
  if FCertificateChanged then begin
    FCertificateChanged := false;
    FConnection.ServerCertificate := FServerCertificate;
    if FConnection.Connected then
      RestartSession;
  end;

  WasConnected := FConnection.Connected;
  if not WasConnected then begin
    //a new session starts from nothing: no access known, no login, no subscriptions
    FAccessLevel := 0;
    FLegitimated := false;
    FSessionTainted := false;
    FConnection.Connect(FPassword);
  end;

  Result := FConnection.Connected;
  if not Result then exit;

  if not WasConnected then begin
    FSubResetPending := true;
    FAuthPending := false;

    //What the session may do, so that a failure can say "authentication" and not "comm error".
    if not FConnection.GetEffectiveProtectionLevel(FAccessLevel) then
      FAccessLevel := 0;

    //Authenticate once per freshly-established connection (not on every EnsureConnected call -
    //this runs on every scan tick even with no tags due). Password='' (default) means the PLC
    //doesn't need legitimation - skip entirely, matching every PLC that doesn't require it.
    if FPassword<>'' then
      if not AuthenticateSession(FPassword, FUsername) then
        ConnDebug(Self, 'EnsureConnected: falha na legitimacao (Username/Password) - writes podem continuar bloqueados pelo CLP.');
  end else if FAuthPending and (GetTickCount64-FAuthChangedAt>=S7PlusAuthSettleMs) then begin
    //Username/Password changed on an open session: log in again with the new pair
    FAuthPending := false;
    if FPassword<>'' then begin
      if FSessionTainted then begin
        //the last login was refused: only a new session takes another one
        if RestartSession then
          Result := EnsureConnected    //opens the session and logs in by itself
        else
          Result := false;
      end else if not AuthenticateSession(FPassword, FUsername) then
        ConnDebug(Self, 'EnsureConnected: falha na legitimacao com as novas credenciais (Username/Password).');
    end;
  end;
end;

procedure TS7CommPlusDriver.SetUsername(const AValue:AnsiString);
begin
  if AValue=FUsername then exit;
  FUsername := AValue;
  FAuthPending := true;
  FAuthChangedAt := GetTickCount64;
end;

procedure TS7CommPlusDriver.SetServerCertificate(const AValue:AnsiString);
begin
  if AValue=FServerCertificate then exit;
  FServerCertificate := AValue;
  //(the connection takes it at the next EnsureConnected, on the scan thread)
  FCertificateChanged := true;
end;

function TS7CommPlusDriver.GetPeerCertificate:AnsiString;
begin
  Result := '';
  if FConnection<>nil then
    Result := FConnection.PeerCertificate;
end;

procedure TS7CommPlusDriver.SetPassword(const AValue:AnsiString);
begin
  if AValue=FPassword then exit;
  FPassword := AValue;
  FAuthPending := true;
  FAuthChangedAt := GetTickCount64;
end;

function TS7CommPlusDriver.SessionNeedsAuth:Boolean;
begin
  Result := (FAccessLevel>S7PlusAccessLevel_FullAccess) and (not FLegitimated);
end;

function TS7CommPlusDriver.FailResult:TProtocolIOResult;
begin
  if SessionNeedsAuth then
    Result := ioAuthError
  else
    Result := ioCommError;
end;

function TS7CommPlusDriver.AuthenticateSession(const APassword, AUsername:AnsiString):Boolean;
var
  Level:Cardinal;
begin
  Result := FConnection.Authenticate(APassword, AUsername);
  FLegitimated := Result;
  if not Result then begin
    FSessionTainted := true;
    exit;
  end;

  if FConnection.GetEffectiveProtectionLevel(Level) then
    FAccessLevel := Level;
  //what was refused before the login is worth another try, and the subscriptions made
  //under the old access level deliver nothing: they are made again
  FSubResetPending := true;
  ClearAuthErrors;
end;

procedure TS7CommPlusDriver.MarkAuthErrors(Now_:QWord);
var
  i:Integer;
begin
  if not SessionNeedsAuth then exit;
  for i:=0 to High(FScanList) do begin
    //a tag that has not been read (path not resolved, no notification yet) is waiting for the
    //login; one that reads fine is left alone, and so is one that fails for another reason
    if FScanList[i].LastResult=ioOk then continue;
    if FScanList[i].LastResult=ioAuthError then continue;
    if (not FScanList[i].Resolved) or FScanList[i].OnChange then begin
      FScanList[i].LastResult := ioAuthError;
      FScanList[i].LastScanTimeStamp := Now_;    //what makes the framework deliver it
    end;
  end;
end;

procedure TS7CommPlusDriver.ClearAuthErrors;
var
  i:Integer;
begin
  for i:=0 to High(FScanList) do
    if FScanList[i].LastResult=ioAuthError then begin
      FScanList[i].LastResult := ioNullDriver;
      FScanList[i].LastResolveAttempt := 0;     //resolve it again at once
    end;
end;

function TS7CommPlusDriver.RestartSession:Boolean;
var
  Waited:Integer;
begin
  Result := false;
  if (FConnection=nil) or (PCommPort=nil) then exit;
  ConnDebug(Self, 'RestartSession: reabrindo a sessao (o CLP nao aceita uma nova legitimacao apos uma recusada).');
  FConnection.Disconnect;           //our side of the session
  FSessionTainted := false;
  FSubResetPending := true;
  PCommPort.RenewHandle;            //the TCP connection under it
  Sleep(S7PlusReconnectGapMs);
  Waited := 0;
  while (not PCommPort.ReallyActive) and (Waited<5000) do begin
    Sleep(50);
    Inc(Waited, 50);
  end;
  Result := PCommPort.ReallyActive;
end;

function TS7CommPlusDriver.Auth(const AUsername, APassword:AnsiString):TProtocolIOResult;
begin
  LockDriverIO;
  try
    FUsername := AUsername;
    FPassword := APassword;
    FAuthPending := false;

    //a refused login spoils the session for the next one: start over
    if (FConnection<>nil) and FConnection.Connected and FSessionTainted and (APassword<>'') then
      RestartSession;

    if (FConnection<>nil) and FConnection.Connected then begin
      //an open session logs in again, now
      if APassword<>'' then
        if not AuthenticateSession(APassword, AUsername) then
          ConnDebug(Self, 'Auth: falha na legitimacao (Username/Password).');
    end else if not EnsureConnected then begin
      //no session: nothing to log in to
      Result := ioCommError;
      exit;
    end;
    //(a session opened just now has already logged in, see EnsureConnected)

    if SessionNeedsAuth then
      Result := ioAuthError
    else
      Result := ioOk;
  finally
    UnlockDriverIO;
  end;
end;

//: Serializes direct/synchronous access (BrowseDB, ReadSymbolic, ...) against the
//: background scan thread, the same way the framework already serializes DoScanRead vs.
//: DoRead/DoWrite (see TProtocolDriver.SafeScanRead/SafeSingleScanRead). Without this,
//: DoScanRead's own EnsureConnected call (it runs unconditionally on every tick, even
//: with no tags registered) races with a direct call from application code on another
//: thread, corrupting the COTP/TLS handshake on the shared socket.
procedure TS7CommPlusDriver.LockDriverIO;
begin
  while not FPause.ResetEvent do
    CrossThreadSwitch;
  FWriteCS.Enter;
  FReadCS.Enter;
end;

procedure TS7CommPlusDriver.UnlockDriverIO;
begin
  FReadCS.Leave;
  FWriteCS.Leave;
  FPause.SetEvent;
end;

//: Without this the port never tells the driver it was closed or lost (the base class asks
//: for no events): the session kept "connected" over a TCP connection that was gone, and no
//: new session, login or subscription was made when the port came back.
function TS7CommPlusDriver.NotifyThisEvents:TNotifyThisEvents;
begin
  Result := [ntePortClosed, ntePortDisconnected];
end;

procedure TS7CommPlusDriver.PortDisconnected(Sender:TObject);
begin
  FSubResetPending := true;
  if FConnection<>nil then
    FConnection.Disconnect;
  inherited PortDisconnected(Sender);
end;

procedure TS7CommPlusDriver.PortClosed(Sender:TObject);
begin
  FSubResetPending := true;
  if FConnection<>nil then
    FConnection.Disconnect;
  inherited PortClosed(Sender);
end;

function TS7CommPlusDriver.AreaRIDOf(ReadFunction:LongInt; out AreaRID:Cardinal):Boolean;
begin
  Result := true;
  case ReadFunction of
    S7PlusRF_Inputs:  AreaRID := S7PlusIds_NativeIAreaRID;
    S7PlusRF_Outputs: AreaRID := S7PlusIds_NativeQAreaRID;
    S7PlusRF_Flags:   AreaRID := S7PlusIds_NativeMAreaRID;
    S7PlusRF_Counter: AreaRID := S7PlusIds_NativeS7CountersRID;
    S7PlusRF_Timer:   AreaRID := S7PlusIds_NativeS7TimersRID;
  else
    AreaRID := 0;
    Result := false;
  end;
end;

//===========================================================================
// Phase 2: symbolic Path resolution (cached) and scan-list management
//===========================================================================

function TS7CommPlusDriver.GetCachedDBVars(DBNumber:Cardinal; out Vars:TS7PlusVarInfoArray):Boolean;
var
  i, idx:Integer;
begin
  for i:=0 to High(FDBCache) do
    if FDBCache[i].DBNumber=DBNumber then begin
      Vars := FDBCache[i].Vars;
      exit(true);
    end;

  Result := FConnection.BrowseDB(DBNumber, Vars);
  if not Result then exit;

  idx := Length(FDBCache);
  SetLength(FDBCache, idx+1);
  FDBCache[idx].DBNumber := DBNumber;
  FDBCache[idx].TiRid := FConnection.LastBrowseTiRid;
  FDBCache[idx].Vars := Vars;
  LearnProgram(DBNumber, FConnection.LastBrowseTiRid);
end;

function TS7CommPlusDriver.GetCachedAreaVars(AreaRID, TiRid:Cardinal; const AreaName:String; var Cache:TS7PlusVarInfoArray; var Browsed:Boolean; out Vars:TS7PlusVarInfoArray):Boolean;
begin
  if Browsed then begin
    Vars := Cache;
    exit(true);
  end;

  Result := FConnection.BrowseNativeArea(AreaRID, TiRid, AreaName, Vars);
  if not Result then exit;

  Cache := Vars;
  Browsed := true;
end;

function TS7CommPlusDriver.GetDBNumberByName(const Name:AnsiString; out DBNumber:Integer):Boolean;
var
  i:Integer;
begin
  Result := false;
  DBNumber := 0;
  if not FDBNameCacheLoaded then begin
    //Leave FDBNameCacheLoaded false on failure, so a transient EXPLORE failure doesn't
    //permanently blacklist symbolic DB names for the rest of the session - it'll just
    //retry (and re-EXPLORE) on the next Path that needs it.
    if not FConnection.ListDataBlocks(FDBNameCache) then exit;
    FDBNameCacheLoaded := true;
  end;
  for i:=0 to High(FDBNameCache) do
    if SameText(FDBNameCache[i].Name, Name) then begin
      DBNumber := FDBNameCache[i].Number;
      Result := true;
      exit;
    end;
end;

procedure TS7CommPlusDriver.CollectArrayElements(const Vars:TS7PlusVarInfoArray; const CanonicalPath:AnsiString;
  AccessArea:Cardinal; ArraySize:Integer; var Items:TS7PlusCompositeItemArray);
var
  i, n, ElemSize:Integer;
  Prefix, Rest:AnsiString;
  Found:TS7PlusCompositeItemArray;
begin
  SetLength(Found, 0);
  Prefix := CanonicalPath+'[';
  for i:=0 to High(Vars) do begin
    if Copy(Vars[i].Name, 1, Length(Prefix))<>Prefix then continue;
    Rest := Copy(Vars[i].Name, Length(Prefix), MaxInt);          //"[k]"
    if (Rest='') or (Rest[Length(Rest)]<>']') or (Pos('.', Rest)>0) then continue;
    ElemSize := S7PlusDataTypeSize(Vars[i].SoftDataType, 0);
    if (ElemSize<=0) or (TS7PlusSoftDataType(Vars[i].SoftDataType) in [sdtSTRING, sdtWSTRING, sdtSTRUCT]) then begin
      SetLength(Found, 0);
      exit;
    end;
    n := Length(Found);
    SetLength(Found, n+1);
    Found[n].AccessArea := AccessArea;
    Found[n].Lids := Vars[i].Lids;
    Found[n].SoftDataType := Vars[i].SoftDataType;
    Found[n].WriteSize := CompositeWriteSize(Vars[i].SoftDataType);
    Found[n].ReadSize := ElemSize;
    Found[n].ByteOffset := n*ElemSize;       //the listing is in index order
  end;
  if (Length(Found)=0) or (Length(Found)*Found[0].ReadSize<>ArraySize) then exit;
  Items := Found;
end;

function TS7CommPlusDriver.IsStructArrayPath(const Vars:TS7PlusVarInfoArray; const CanonicalPath:AnsiString):Boolean;
var
  i:Integer;
  IsStruct:Boolean;
begin
  Result := false;
  IsStruct := false;
  for i:=0 to High(Vars) do
    if (Vars[i].Name=CanonicalPath) and (TS7PlusSoftDataType(Vars[i].SoftDataType)=sdtSTRUCT) and
       (Vars[i].ByteOffset>=0) then
      IsStruct := true;
  if not IsStruct then exit;
  //an element ("Arr[2]") or an array of structures (its elements are listed right after it)
  if (CanonicalPath<>'') and (CanonicalPath[Length(CanonicalPath)]=']') then begin
    Result := true;
    exit;
  end;
  for i:=0 to High(Vars) do
    if (Length(Vars[i].Name)>Length(CanonicalPath)) and
       (Copy(Vars[i].Name, 1, Length(CanonicalPath)+1)=CanonicalPath+'[') and
       (TS7PlusSoftDataType(Vars[i].SoftDataType)=sdtSTRUCT) then begin
      Result := true;
      exit;
    end;
end;

procedure TS7CommPlusDriver.AddStructArrayMembers(const Vars:TS7PlusVarInfoArray; const CanonicalPath:AnsiString;
  AccessArea:Cardinal; BaseOffset:Integer; var Items:TS7PlusCompositeItemArray);
var
  i, n:Integer;
  Prefix:AnsiString;
begin
  if CanonicalPath[Length(CanonicalPath)]=']' then
    Prefix := CanonicalPath+'.'
  else
    Prefix := CanonicalPath+'[';
  for i:=0 to High(Vars) do begin
    if Copy(Vars[i].Name, 1, Length(Prefix))<>Prefix then continue;
    if TS7PlusSoftDataType(Vars[i].SoftDataType)=sdtSTRUCT then continue;   //an element: its members are the leaves
    if Vars[i].ByteOffset<0 then continue;
    n := Length(Items);
    SetLength(Items, n+1);
    Items[n].AccessArea := AccessArea;
    Items[n].Lids := Vars[i].Lids;
    Items[n].ByteOffset := Vars[i].ByteOffset-BaseOffset;
    Items[n].SoftDataType := Vars[i].SoftDataType;
    Items[n].WriteSize := CompositeWriteSize(Vars[i].SoftDataType);
    Items[n].ReadSize := Vars[i].DataSize;
  end;
end;

function TS7CommPlusDriver.ResolvePath(const Path:AnsiString; out AccessArea:Cardinal; out Lids:TS7PlusLIDArray; out ByteOffset:Integer; out SoftDataType:Byte; out CompositeItems:TS7PlusCompositeItemArray; out DataSize:Integer; out ElementItems:TS7PlusCompositeItemArray):Boolean;
var
  DotPos, DBNumber, i, j, CIdx:Integer;
  AreaName, CanonicalPath, Rest:AnsiString;
  Vars:TS7PlusVarInfoArray;
  Found:Boolean;
  Tmp:TS7PlusCompositeItem;
begin
  Result := false;
  SetLength(Lids, 0);
  AccessArea := 0;
  ByteOffset := -1;
  SoftDataType := 0;
  DataSize := 0;
  SetLength(CompositeItems, 0);
  SetLength(ElementItems, 0);

  DotPos := Pos('.', Path);
  if DotPos=1 then exit; //malformed: leading dot
  if DotPos=0 then
    AreaName := Path //bare Path, e.g. "DB4" or "BlocoSimbolico_4" - the DB/area root itself
  else
    AreaName := Copy(Path, 1, DotPos-1);
  CanonicalPath := Path;

  Found := false;
  if (Length(AreaName)>2) and SameText(Copy(AreaName,1,2),'DB') and
     TryStrToInt(Copy(AreaName,3,Length(AreaName)), DBNumber) then begin
    if GetCachedDBVars(DBNumber, Vars) then begin
      AccessArea := S7PlusIds_DBAccessAreaBase + Cardinal(DBNumber);
      Found := true;
    end;
  end else if GetDBNumberByName(AreaName, DBNumber) then begin
    //Symbolic (TIA Portal) DB name, e.g. "BlocoSimbolico_4" -> DB4. BrowseDB's own flat
    //list always names its entries by number ("DB4"/"DB4.Var1"), so rewrite the prefix
    //before the final name lookup below.
    if GetCachedDBVars(DBNumber, Vars) then begin
      AccessArea := S7PlusIds_DBAccessAreaBase + Cardinal(DBNumber);
      if DotPos=0 then
        CanonicalPath := 'DB'+IntToStr(DBNumber)
      else
        CanonicalPath := 'DB'+IntToStr(DBNumber)+Copy(Path, DotPos, Length(Path));
      Found := true;
    end;
  end else if SameText(AreaName,'IArea') then begin
    Found := GetCachedAreaVars(S7PlusIds_NativeIAreaRID, S7PlusTiRid_IArea, 'IArea', FIAreaVars, FIAreaBrowsed, Vars);
    AccessArea := S7PlusIds_NativeIAreaRID;
  end else if SameText(AreaName,'QArea') then begin
    Found := GetCachedAreaVars(S7PlusIds_NativeQAreaRID, S7PlusTiRid_QArea, 'QArea', FQAreaVars, FQAreaBrowsed, Vars);
    AccessArea := S7PlusIds_NativeQAreaRID;
  end else if SameText(AreaName,'MArea') then begin
    Found := GetCachedAreaVars(S7PlusIds_NativeMAreaRID, S7PlusTiRid_MArea, 'MArea', FMAreaVars, FMAreaBrowsed, Vars);
    AccessArea := S7PlusIds_NativeMAreaRID;
  end else if SameText(AreaName,'S7Timers') then begin
    Found := GetCachedAreaVars(S7PlusIds_NativeS7TimersRID, S7PlusTiRid_S7Timers, 'S7Timers', FTimersVars, FTimersBrowsed, Vars);
    AccessArea := S7PlusIds_NativeS7TimersRID;
  end else if SameText(AreaName,'S7Counters') then begin
    Found := GetCachedAreaVars(S7PlusIds_NativeS7CountersRID, S7PlusTiRid_S7Counters, 'S7Counters', FCountersVars, FCountersBrowsed, Vars);
    AccessArea := S7PlusIds_NativeS7CountersRID;
  end;

  if not Found then exit;

  //Bare root Path (Path="DB4"/"BlocoSimbolico_4", no dot): a "whole DB/area" tag. The root's
  //own (empty) LID path only returns a small, unrelated metadata blob when read directly -
  //confirmed against real hardware, see IsLidsPrefix - so instead of that, collect every one
  //of the DB/area's own top-level members (one dot below the root, i.e. not itself another
  //member's nested field; array elements are skipped since their ByteOffset is unreliable).
  //DoScanRead batches all of these into one request per scan round and splices the results
  //together at each member's own ByteOffset - see TS7CommPlusDriver.DoScanRead.
  if DotPos=0 then begin
    for i:=0 to High(Vars) do begin
      if Vars[i].Name=CanonicalPath then continue; //the root entry itself, not a member
      if Vars[i].ByteOffset=-1 then continue;
      if (Length(Vars[i].Name)<=Length(CanonicalPath)) or
         (Copy(Vars[i].Name,1,Length(CanonicalPath))<>CanonicalPath) or
         (Vars[i].Name[Length(CanonicalPath)+1]<>'.') then continue;
      Rest := Copy(Vars[i].Name, Length(CanonicalPath)+2, MaxInt);
      if (Rest='') or (Pos('.', Rest)>0) then continue; //not a direct (top-level) member

      //an array of structures cannot be read as a whole: its members are read one by one
      if IsStructArrayPath(Vars, Vars[i].Name) then begin
        AddStructArrayMembers(Vars, Vars[i].Name, AccessArea, 0, CompositeItems);
        continue;
      end;

      CIdx := Length(CompositeItems);
      SetLength(CompositeItems, CIdx+1);
      CompositeItems[CIdx].AccessArea := AccessArea;
      CompositeItems[CIdx].Lids := Vars[i].Lids;
      CompositeItems[CIdx].ByteOffset := Vars[i].ByteOffset;
      CompositeItems[CIdx].SoftDataType := Vars[i].SoftDataType;
      CompositeItems[CIdx].WriteSize := CompositeWriteSize(Vars[i].SoftDataType);
      CompositeItems[CIdx].ReadSize := Vars[i].DataSize;
    end;
    //Insertion sort by ByteOffset ascending - composing "starting from the first element" is
    //what makes truncating the assembled buffer to the tag's own configured Size meaningful.
    for i:=1 to High(CompositeItems) do begin
      Tmp := CompositeItems[i];
      j := i;
      while (j>0) and (CompositeItems[j-1].ByteOffset>Tmp.ByteOffset) do begin
        CompositeItems[j] := CompositeItems[j-1];
        Dec(j);
      end;
      CompositeItems[j] := Tmp;
    end;
  end;

  //An array of structures ("DB4.Arr") or one element of it ("DB4.Arr[2]"): a "structure" tag
  //over it reads the members one by one and puts them together at their classic offsets,
  //counted from the start of the array (or of the element).
  if (DotPos>0) and IsStructArrayPath(Vars, CanonicalPath) then begin
    for i:=0 to High(Vars) do
      if Vars[i].Name=CanonicalPath then begin
        AddStructArrayMembers(Vars, CanonicalPath, AccessArea, Vars[i].ByteOffset, CompositeItems);
        break;
      end;
    for i:=1 to High(CompositeItems) do begin
      Tmp := CompositeItems[i];
      j := i;
      while (j>0) and (CompositeItems[j-1].ByteOffset>Tmp.ByteOffset) do begin
        CompositeItems[j] := CompositeItems[j-1];
        Dec(j);
      end;
      CompositeItems[j] := Tmp;
    end;
  end;

  for i:=0 to High(Vars) do
    if Vars[i].Name=CanonicalPath then begin
      Lids := Vars[i].Lids;
      //The position in the PLC's memory image (TS7PlusVarInfo.ImageOffset), not the classic
      //offset: the only user is the read-dedup, which cuts a member out of its struct's
      //already-read bytes, and an optimized struct is laid out by size, not by declaration.
      ByteOffset := Vars[i].ImageOffset;
      SoftDataType := Vars[i].SoftDataType;
      DataSize := Vars[i].DataSize;
      Result := true;
      //An array of scalars: its elements, each at its place in the array's bytes. The PLC takes a
      //write of one element (by its own address), never a slice of the array.
      if (Length(CompositeItems)=0) and (DataSize>0) then
        CollectArrayElements(Vars, CanonicalPath, AccessArea, DataSize, ElementItems);
      exit;
    end;
end;

function TS7CommPlusDriver.TagPathOf(TagObj:TTag; out Path:AnsiString):Boolean;
begin
  Path := '';
  if TagObj is TPLCTagNumber then
    Path := TPLCTagNumber(TagObj).LongAddress
  else if TagObj is TPLCBlock then
    Path := TPLCBlock(TagObj).LongAddress
  else if TagObj is TPLCString then
    Path := TPLCString(TagObj).LongAddress;
  Result := Path<>'';
end;

function TS7CommPlusDriver.FindScanEntry(const Path:AnsiString; out Index:Integer):Boolean;
var
  i:Integer;
begin
  for i:=0 to High(FScanList) do
    if FScanList[i].Path=Path then begin
      Index := i;
      exit(true);
    end;
  Index := -1;
  Result := false;
end;

function TS7CommPlusDriver.IsLidsPrefix(const Prefix, Full:TS7PlusLIDArray):Boolean;
var
  i:Integer;
begin
  Result := false;
  //An empty LID path (the DB/area root itself, e.g. Path="DB4") does NOT return its
  //children's bytes when read - confirmed against real hardware: reading a DB root
  //returns a small fixed-size metadata object (looks like TiRid + a generation/checksum
  //tag), not the DB's actual memory content. Treating it as a dedup container would slice
  //completely wrong bytes for every other tag on that DB/area - never allow it.
  if Length(Prefix)=0 then exit;
  if Length(Prefix)>=Length(Full) then exit; //must be a STRICT (shorter) prefix
  for i:=0 to High(Prefix) do
    if Prefix[i]<>Full[i] then exit;
  Result := true;
end;

//: Read-dedup: if some other already-resolved, independently-scanned entry's LID path is a
//: prefix of this one's (same AccessArea - e.g. a struct and one of its own fields), this
//: entry never gets its own GetMultiVariables read - DoGetValue slices its bytes out of the
//: container's already-read Data instead (see TS7PlusVarInfo.ByteOffset for how the byte
//: offset is computed and why it is directly comparable/subtractable across nesting levels).
//: Symmetrically, if this entry turns out to be a container for other, already-independent
//: entries, those get switched to derive from it instead of being read on their own.
procedure TS7CommPlusDriver.TryDeduplicateEntry(EntryIdx:Integer);
var
  i, BestContainerIdx, BestLen:Integer;
  CanBeDerived:Boolean;
begin
  if FScanList[EntryIdx].ByteOffset=-1 then exit; //offset unknown (e.g. inside an array)
  //An on-change entry gets its own subscribed items: it neither piggybacks on a polled
  //container nor lets others piggyback on it (its data only changes with notifications).
  if FScanList[EntryIdx].OnChange then exit;

  CanBeDerived := not (TS7PlusSoftDataType(FScanList[EntryIdx].SoftDataType) in [sdtSTRING, sdtWSTRING]);

  if CanBeDerived then begin
    BestContainerIdx := -1;
    BestLen := -1;
    for i:=0 to High(FScanList) do begin
      if i=EntryIdx then continue;
      if FScanList[i].IsDerived or (not FScanList[i].Resolved) or FScanList[i].OnChange then continue;
      if FScanList[i].AccessArea<>FScanList[EntryIdx].AccessArea then continue;
      if FScanList[i].ByteOffset=-1 then continue;
      if IsLidsPrefix(FScanList[i].Lids, FScanList[EntryIdx].Lids) and (Length(FScanList[i].Lids)>BestLen) then begin
        BestLen := Length(FScanList[i].Lids);
        BestContainerIdx := i;
      end;
    end;
    if BestContainerIdx>=0 then begin
      FScanList[EntryIdx].IsDerived := true;
      FScanList[EntryIdx].ContainerPath := FScanList[BestContainerIdx].Path;
      FScanList[EntryIdx].DerivedOffset := FScanList[EntryIdx].ByteOffset - FScanList[BestContainerIdx].ByteOffset;
      FScanList[EntryIdx].DerivedLen := S7PlusDataTypeSize(FScanList[EntryIdx].SoftDataType, 0);
      exit; //now derived - not eligible to also absorb others below
    end;
  end;

  //This entry stayed independent - does it cover any EXISTING independent entries?
  for i:=0 to High(FScanList) do begin
    if i=EntryIdx then continue;
    if FScanList[i].IsDerived or (not FScanList[i].Resolved) or FScanList[i].OnChange then continue;
    if FScanList[i].AccessArea<>FScanList[EntryIdx].AccessArea then continue;
    if FScanList[i].ByteOffset=-1 then continue;
    if TS7PlusSoftDataType(FScanList[i].SoftDataType) in [sdtSTRING, sdtWSTRING] then continue;
    if IsLidsPrefix(FScanList[EntryIdx].Lids, FScanList[i].Lids) then begin
      FScanList[i].IsDerived := true;
      FScanList[i].ContainerPath := FScanList[EntryIdx].Path;
      FScanList[i].DerivedOffset := FScanList[i].ByteOffset - FScanList[EntryIdx].ByteOffset;
      FScanList[i].DerivedLen := S7PlusDataTypeSize(FScanList[i].SoftDataType, 0);
    end;
  end;
end;

//: Reads every member of a "whole DB/area" composite Path (see ResolvePath) in one batched
//: request and splices the results together at each member's own ByteOffset. Used by the
//: synchronous DoRead path; DoScanRead does its own equivalent expansion inline so a
//: composite tag's members can share the same GetMultiVariables round-trip as any other due
//: tag in the same scan tick, instead of a separate request just for this Path.
function TS7CommPlusDriver.CompositeWriteSize(SoftDataType:Byte):Integer;
begin
  //The scalar types EncodeTypedWriteValue knows (BOOL and STRING are left out on purpose).
  case SoftDataType of
    2,3,4,5,6,7,8,48,49,50,51,52,53,54,55: Result := S7PlusDataTypeSize(SoftDataType, 0);
  else
    Result := 0;
  end;
end;

function TS7CommPlusDriver.PatchedValue(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; Offset:Integer;
  const Values:TArrayOfDouble; out Whole:TBytes):Boolean;
var
  b:Integer;
begin
  Result := false;
  SetLength(Whole, 0);
  if (Offset<0) or (not FConnection.ReadSymbolic(AccessArea, Lids, Whole)) then exit;
  if Offset+Length(Values)>Length(Whole) then exit;       //a slice that does not fit the value
  for b:=0 to High(Values) do
    Whole[Offset+b] := Trunc(Values[b]) and $FF;
  Result := true;
end;

//Values start at byte BaseOffset of the value the Composite describes (a write of part of it, as
//an item of a structure does): only the members that lie wholly inside Values are written.
function TS7CommPlusDriver.ExpandCompositeWrite(const Composite:TS7PlusCompositeItemArray; const Values:TArrayOfDouble; var Items:TS7PlusMultiWriteItemArray; BaseOffset:Integer):Integer;
var
  i, b, k:Integer;
begin
  Result := 0;
  for i:=0 to High(Composite) do begin
    if (Composite[i].WriteSize<=0) or (Composite[i].ByteOffset<BaseOffset) or
       (Composite[i].ByteOffset-BaseOffset+Composite[i].WriteSize>Length(Values)) then continue;
    k := Length(Items);
    SetLength(Items, k+1);
    Items[k].AccessArea := Composite[i].AccessArea;
    Items[k].Lids := Composite[i].Lids;
    Items[k].SoftDataType := Composite[i].SoftDataType;
    SetLength(Items[k].Data, Composite[i].WriteSize);
    for b:=0 to Composite[i].WriteSize-1 do
      Items[k].Data[b] := Trunc(Values[Composite[i].ByteOffset-BaseOffset+b]) and $FF;
    Inc(Result);
  end;
end;

function TS7CommPlusDriver.ReadComposite(const Items:TS7PlusCompositeItemArray; out Data:TBytes):Boolean;
var
  ReqItems:TS7PlusMultiReadItemArray;
  Results:TS7PlusMultiReadResultArray;
  i, b, NeedLen:Integer;
begin
  Result := false;
  SetLength(Data, 0);
  if Length(Items)=0 then exit;

  SetLength(ReqItems, Length(Items));
  for i:=0 to High(Items) do begin
    ReqItems[i].AccessArea := Items[i].AccessArea;
    ReqItems[i].Lids := Items[i].Lids;
    ReqItems[i].ExpectedSize := Items[i].ReadSize;
  end;

  if not FConnection.ReadMultipleSymbolic(ReqItems, Results) then exit;

  for i:=0 to High(Items) do begin
    if (i>High(Results)) or (not Results[i].Ok) then continue;
    NeedLen := Items[i].ByteOffset+Length(Results[i].Data);
    if NeedLen>Length(Data) then
      SetLength(Data, NeedLen);
    for b:=0 to High(Results[i].Data) do
      Data[Items[i].ByteOffset+b] := Results[i].Data[b];
  end;
  Result := true;
end;

procedure TS7CommPlusDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
var
  Path:AnsiString;
  Idx:Integer;
  Entry:TS7PlusScanEntry;
  tr:TTagRec;
begin
  if TagPathOf(TagObj, Path) and (not FindScanEntry(Path, Idx)) then begin
    FillChar(Entry, SizeOf(Entry), 0);
    Entry.TagObj := TagObj;
    Entry.Path := Path;
    Entry.Resolved := false;
    Entry.LastResult := ioNullDriver;
    if Supports(TagObj, IScanableTagInterface) then begin
      (TagObj as IScanableTagInterface).BuildTagRec(tr, 0, 0);
      Entry.UpdateRate := tr.UpdateTime;
      Entry.OnChange := tr.UpdateTime=0;
    end;
    Idx := Length(FScanList);
    SetLength(FScanList, Idx+1);
    FScanList[Idx] := Entry;
  end;
  //Validity is computed here, from the resolved Path, not from the incoming TagValid
  //parameter (the base TProtocolDriver.AddTag always calls DoAddTag(TagObj, false) -
  //each driver is expected to determine real validity itself, same as TModBusDriver.DoAddTag).
  inherited DoAddTag(TagObj, Path<>'');
end;

procedure TS7CommPlusDriver.DoDelTag(TagObj:TTag);
var
  Path:AnsiString;
  Idx, i:Integer;
begin
  if TagPathOf(TagObj, Path) and FindScanEntry(Path, Idx) then begin
    //Entries deriving their value from this one (see TryDeduplicateEntry) can't keep
    //pointing at it once it's gone - force them back through resolve+dedup on the next
    //tick, where they'll either find a different container or become independent reads.
    for i:=0 to High(FScanList) do
      if (i<>Idx) and FScanList[i].IsDerived and (FScanList[i].ContainerPath=Path) then begin
        FScanList[i].IsDerived := false;
        FScanList[i].ContainerPath := '';
        FScanList[i].Resolved := false;
        FScanList[i].LastResolveAttempt := 0;
      end;

    for i:=Idx to High(FScanList)-1 do
      FScanList[i] := FScanList[i+1];
    SetLength(FScanList, Length(FScanList)-1);
  end;
  inherited DoDelTag(TagObj);
end;

//===========================================================================
// TProtocolDriver abstract contract
//===========================================================================

function TS7CommPlusDriver.EntryReadItems(EntryIdx:Integer):TS7PlusMultiReadItemArray;
var
  k:Integer;
begin
  SetLength(Result, 0);
  if FScanList[EntryIdx].IsComposite then begin
    SetLength(Result, Length(FScanList[EntryIdx].CompositeItems));
    for k:=0 to High(Result) do begin
      Result[k].AccessArea := FScanList[EntryIdx].CompositeItems[k].AccessArea;
      Result[k].Lids := FScanList[EntryIdx].CompositeItems[k].Lids;
      Result[k].ExpectedSize := FScanList[EntryIdx].CompositeItems[k].ReadSize;
    end;
  end else begin
    SetLength(Result, 1);
    Result[0].AccessArea := FScanList[EntryIdx].AccessArea;
    Result[0].Lids := FScanList[EntryIdx].Lids;
    Result[0].ExpectedSize := FScanList[EntryIdx].ReadSize;
  end;
end;

function S7PlusPickFillers(const Candidates:TS7PlusFillCandidateArray; ItemsLeft, BytesLeft, MaxCount:Integer):TIntegerDynArray;
var
  Order:array of Integer;
  i, j, Tmp, n:Integer;
begin
  SetLength(Result, 0);
  n := Length(Candidates);
  if (n=0) or (ItemsLeft<=0) or (BytesLeft<=0) or (MaxCount<=0) then exit;
  SetLength(Order, n);
  for i:=0 to n-1 do Order[i] := i;
  //insertion sort, oldest first; a tie keeps the order of the list (stable)
  for i:=1 to n-1 do begin
    Tmp := Order[i];
    j := i-1;
    while (j>=0) and (Candidates[Order[j]].Age<Candidates[Tmp].Age) do begin
      Order[j+1] := Order[j];
      Dec(j);
    end;
    Order[j+1] := Tmp;
  end;
  for i:=0 to n-1 do begin
    if Length(Result)>=MaxCount then break;
    with Candidates[Order[i]] do begin
      if (Items<=0) or (Items>ItemsLeft) or (Bytes>BytesLeft) then continue;
      Dec(ItemsLeft, Items);
      Dec(BytesLeft, Bytes);
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := Index;
    end;
  end;
end;

procedure TS7CommPlusDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
const
  //How often an on-change tag is polled when its subscription could not be created.
  SubFallbackPollMs = 1000;
type
  //-- Which scan-list entry a given batched wire item belongs to, and (for a composite
  //-- entry's own members) where in that entry's assembled Data its bytes get spliced.
  TBatchOwner = record
    EntryIdx:Integer;
    IsCompositeItem:Boolean;
    CompByteOffset:Integer;
  end;
var
  DummyElements:TS7PlusCompositeItemArray;
  i, j, EntryIdx, ItemIdx, b:Integer;
  Now_, Elapsed:Int64;
  DueIndices:array of Integer;
  Items:TS7PlusMultiReadItemArray;
  Results:TS7PlusMultiReadResultArray;
  Owners:array of TBatchOwner;
  ok:Boolean;
  NeedLen:Integer;
  Rate:Int64;
  RoomItems, RoomBytes:Integer;
  Cands:TS7PlusFillCandidateArray;
  Fillers:TIntegerDynArray;
  Extra:TS7PlusMultiReadItemArray;
  Due:Boolean;

  procedure AddEntryItems(Idx:Integer);
  var
    k:Integer;
  begin
    if FScanList[Idx].IsComposite then begin
      for k:=0 to High(FScanList[Idx].CompositeItems) do begin
        ItemIdx := Length(Items);
        SetLength(Items, ItemIdx+1);
        SetLength(Owners, ItemIdx+1);
        Items[ItemIdx].AccessArea := FScanList[Idx].CompositeItems[k].AccessArea;
        Items[ItemIdx].Lids := FScanList[Idx].CompositeItems[k].Lids;
        Items[ItemIdx].ExpectedSize := FScanList[Idx].CompositeItems[k].ReadSize;
        Owners[ItemIdx].EntryIdx := Idx;
        Owners[ItemIdx].IsCompositeItem := true;
        Owners[ItemIdx].CompByteOffset := FScanList[Idx].CompositeItems[k].ByteOffset;
      end;
    end else begin
      ItemIdx := Length(Items);
      SetLength(Items, ItemIdx+1);
      SetLength(Owners, ItemIdx+1);
      Items[ItemIdx].AccessArea := FScanList[Idx].AccessArea;
      Items[ItemIdx].Lids := FScanList[Idx].Lids;
      Items[ItemIdx].ExpectedSize := FScanList[Idx].ReadSize;
      Owners[ItemIdx].EntryIdx := Idx;
      Owners[ItemIdx].IsCompositeItem := false;
    end;
  end;

begin
  NeedSleep := 0;

  if not EnsureConnected then begin
    NeedSleep := 100;
    exit;
  end;

  if FSubResetPending then
    ResetSubscriptions;

  Now_ := GetTickCount64;

  //Resolve any pending tags (throttled - don't hammer EXPLORE every tick for a bad Path).
  for i:=0 to High(FScanList) do
    if (not FScanList[i].Resolved) and (Now_-FScanList[i].LastResolveAttempt>=5000) then begin
      FScanList[i].LastResolveAttempt := Now_;
      if ResolvePath(FScanList[i].Path, FScanList[i].AccessArea, FScanList[i].Lids, FScanList[i].ByteOffset, FScanList[i].SoftDataType, FScanList[i].CompositeItems, FScanList[i].ReadSize, DummyElements) then begin
        FScanList[i].Resolved := true;
        FScanList[i].IsComposite := Length(FScanList[i].CompositeItems)>0;
        TryDeduplicateEntry(i);
      end;
    end;

  //On-change tags: subscribe what just became resolvable, and take in whatever the PLC has
  //already pushed (including notifications that arrived while a request was in flight).
  ManageSubscriptions(Now_);
  PumpNotifications(0);
  CheckSubscriptionWatchdog(GetTickCount64);
  CheckProgramChange(GetTickCount64);
  MarkAuthErrors(Now_);

  //Collect every resolved, independently-scanned tag (derived ones piggyback on their
  //container's read - see TryDeduplicateEntry - and are never picked here) that's
  //currently due, and read all of them in a single batched GetMultiVariables request -
  //see TS7PlusConnection.ReadMultipleSymbolic. Items may span different DBs/areas freely.
  SetLength(DueIndices, 0);
  for i:=0 to High(FScanList) do
    if FScanList[i].Resolved and (not FScanList[i].IsDerived) then begin
      Rate := FScanList[i].UpdateRate;
      if FScanList[i].OnChange then begin
        //Delivered by notifications, polled only if there is no subscription for it.
        if not FScanList[i].SubFallback then continue;
        Rate := SubFallbackPollMs;
      end;
      Elapsed := Int64(Now_-FScanList[i].LastScanTimeStamp) - Rate;
      if (FScanList[i].LastScanTimeStamp=0) or (Elapsed>=0) then begin
        SetLength(DueIndices, Length(DueIndices)+1);
        DueIndices[High(DueIndices)] := i;
        if Length(DueIndices)>=FMaxTagsPerScan then break;
      end;
    end;

  if Length(DueIndices)=0 then begin
    if Length(FSubs)>0 then
      //Nothing to poll: the idle wait is spent listening for notifications instead of
      //sleeping, so a change is picked up as soon as the PLC sends it.
      PumpNotifications(20)
    else
      NeedSleep := 20;
    exit;
  end;

  //Expand each due entry into one or more wire items - a plain entry contributes exactly
  //one item; a composite ("whole DB") entry contributes one item per top-level member, all
  //still folded into this same round's single GetMultiVariables request.
  SetLength(Items, 0);
  SetLength(Owners, 0);
  for i:=0 to High(DueIndices) do
    AddEntryItems(DueIndices[i]);

  //ReadSomethingAlways: the room left in the last request goes to polled tags that are not due.
  if PReadSomethingAlways and (Length(DueIndices)<FMaxTagsPerScan) then begin
    FConnection.ReadRoomLeft(Items, RoomItems, RoomBytes);
    SetLength(Cands, 0);
    for i:=0 to High(FScanList) do
      if FScanList[i].Resolved and (not FScanList[i].IsDerived) and (FScanList[i].LastScanTimeStamp<>0) and
         ((not FScanList[i].OnChange) or FScanList[i].SubFallback) then begin
        Due := false;
        for j:=0 to High(DueIndices) do
          if DueIndices[j]=i then begin
            Due := true;
            break;
          end;
        if Due then continue;
        Extra := EntryReadItems(i);
        SetLength(Cands, Length(Cands)+1);
        Cands[High(Cands)].Index := i;
        Cands[High(Cands)].Age := Int64(Now_-FScanList[i].LastScanTimeStamp);
        Cands[High(Cands)].Items := Length(Extra);
        Cands[High(Cands)].Bytes := 0;
        for j:=0 to High(Extra) do
          Inc(Cands[High(Cands)].Bytes, TS7PlusConnection.ReadItemReplyBytes(Extra[j]));
      end;
    Fillers := S7PlusPickFillers(Cands, RoomItems, RoomBytes, FMaxTagsPerScan-Length(DueIndices));
    for i:=0 to High(Fillers) do begin
      SetLength(DueIndices, Length(DueIndices)+1);
      DueIndices[High(DueIndices)] := Fillers[i];
      AddEntryItems(Fillers[i]);
    end;
  end;

  ok := FConnection.ReadMultipleSymbolic(Items, Results);
  Now_ := GetTickCount64; //re-stamp after the round-trip, not before

  //Optimistically mark every due entry as scanned+ok this round; a composite entry gets
  //downgraded below if any one of its own member reads actually failed.
  for i:=0 to High(DueIndices) do begin
    FScanList[DueIndices[i]].LastScanTimeStamp := Now_;
    FScanList[DueIndices[i]].LastResult := ioOk;
  end;

  //ReadMultipleSymbolic splits the batch into requests of MaxItemsPerRequest items; when one
  //fails, the items of the requests before it still hold their data and every other item is
  //not Ok - the per-item loop below turns exactly those into failed reads.
  if (not ok) and (Length(Results)<>Length(Items)) then begin
    for i:=0 to High(DueIndices) do
      FScanList[DueIndices[i]].LastResult := FailResult;
    exit;
  end;

  for i:=0 to High(Items) do begin
    EntryIdx := Owners[i].EntryIdx;
    if (i>High(Results)) or (not Results[i].Ok) then begin
      FScanList[EntryIdx].LastResult := FailResult;
      continue;
    end;
    if Owners[i].IsCompositeItem then begin
      NeedLen := Owners[i].CompByteOffset+Length(Results[i].Data);
      if NeedLen>Length(FScanList[EntryIdx].Data) then
        SetLength(FScanList[EntryIdx].Data, NeedLen);
      for b:=0 to High(Results[i].Data) do
        FScanList[EntryIdx].Data[Owners[i].CompByteOffset+b] := Results[i].Data[b];
    end else
      FScanList[EntryIdx].Data := Results[i].Data;
  end;
end;

//===========================================================================
// On-change tags (RefreshTime=0): subscriptions and notifications
//===========================================================================

//: How many wire items (= distinct reference ids) an entry subscribes: one per member of a
//: "whole DB/area" composite, one otherwise.
function TS7CommPlusDriver.EntryItemCount(Idx:Integer):Integer;
begin
  if FScanList[Idx].IsComposite then
    Result := Length(FScanList[Idx].CompositeItems)
  else
    Result := 1;
end;

function TS7CommPlusDriver.FindEntryByRefId(RefId:Cardinal; out EntryIdx, ItemIdx:Integer):Boolean;
var
  i:Integer;
begin
  Result := false;
  EntryIdx := -1;
  ItemIdx := -1;
  for i:=0 to High(FScanList) do
    if FScanList[i].Subscribed and (RefId>=FScanList[i].SubRefBase) and
       (RefId<FScanList[i].SubRefBase+Cardinal(EntryItemCount(i))) then begin
      EntryIdx := i;
      ItemIdx := Integer(RefId-FScanList[i].SubRefBase);
      Result := true;
      exit;
    end;
end;

function TS7CommPlusDriver.MarkSubscribed(const Path:AnsiString; RefBase:Cardinal):Boolean;
var
  Idx:Integer;
begin
  Result := FindScanEntry(Path, Idx);
  if Result then begin
    FScanList[Idx].SubRefBase := RefBase;
    FScanList[Idx].Subscribed := true;
  end;
end;

function TS7CommPlusDriver.EntryResolved(const Path:AnsiString):Boolean;
var
  Idx:Integer;
begin
  Result := FindScanEntry(Path, Idx) and FScanList[Idx].Resolved;
end;

function TS7CommPlusDriver.BrowsedDBCount:Integer;
begin
  Result := Length(FDBCache);
end;

function TS7CommPlusDriver.MarkResolved(const Path:AnsiString; const Composite:TS7PlusCompositeItemArray):Boolean;
var
  Idx:Integer;
begin
  Result := FindScanEntry(Path, Idx);
  if Result then begin
    FScanList[Idx].Resolved := true;
    FScanList[Idx].CompositeItems := Composite;
    FScanList[Idx].IsComposite := Length(Composite)>0;
  end;
end;

//: The session is gone (or brand new), and with it every subscription: forget them and let
//: ManageSubscriptions create them again. Tags keep their last value but are flagged as
//: failed until the new subscription delivers one.
procedure TS7CommPlusDriver.ResetSubscriptions;
var
  i:Integer;
  Now_:QWord;
begin
  FSubResetPending := false;
  SetLength(FSubs, 0);
  FSubFailCount := 0;
  FSubRetryAt := 0;
  Now_ := GetTickCount64;
  for i:=0 to High(FScanList) do
    if FScanList[i].OnChange then begin
      if FScanList[i].Subscribed then begin
        FScanList[i].LastResult := FailResult;
        FScanList[i].LastScanTimeStamp := Now_;
      end;
      FScanList[i].Subscribed := false;
      FScanList[i].SubFallback := false;
    end;
end;

//: Creates the subscriptions for the on-change tags that became resolvable and are not
//: subscribed yet. Several subscription objects may coexist (one per call), so tags added
//: later get their own without touching the ones already running - deleting a subscription
//: would tear down the whole container (see TS7PlusConnection.SubscriptionDelete). Reference
//: ids are unique across all of them, so a notification identifies its tag by id alone.
//: A tag whose subscription cannot be created is polled instead (SubFallback) rather than
//: left silent.
procedure TS7CommPlusDriver.SetSubCycleMs(AValue:Integer);
begin
  if AValue<S7PlusMinSubscriptionCycleMs then
    AValue := S7PlusMinSubscriptionCycleMs;
  FSubCycleMs := AValue;
end;

procedure TS7CommPlusDriver.SetSubTimeoutMs(AValue:Integer);
begin
  if AValue<0 then
    AValue := 0;
  FSubTimeoutMs := AValue;
end;

procedure TS7CommPlusDriver.RegisterSubscription(ObjectId:Cardinal; CreditLimit:SmallInt; Now_:QWord);
begin
  SetLength(FSubs, Length(FSubs)+1);
  FSubs[High(FSubs)].ObjectId := ObjectId;
  FSubs[High(FSubs)].NextCreditLimit := CreditLimit;
  FSubs[High(FSubs)].LastNotificationAt := Now_;
end;

procedure TS7CommPlusDriver.TouchSubscription(ObjectId:Cardinal; Now_:QWord);
var
  i:Integer;
begin
  for i:=0 to High(FSubs) do
    if FSubs[i].ObjectId=ObjectId then begin
      FSubs[i].LastNotificationAt := Now_;
      exit;
    end;
end;

function TS7CommPlusDriver.SubscriptionTimeoutMsNow:Integer;
begin
  Result := FSubTimeoutMs;
  if (Result>0) and (Result<S7PlusSubTimeoutMinCycles*FSubCycleMs) then
    Result := S7PlusSubTimeoutMinCycles*FSubCycleMs;
end;

procedure TS7CommPlusDriver.CheckSubscriptionWatchdog(Now_:QWord);
var
  i, Limit:Integer;
begin
  Limit := SubscriptionTimeoutMsNow;
  if (Limit=0) or FSubResetPending or (Length(FSubs)=0) then exit;
  //notifications still waiting to be taken in are not silence
  if (FConnection<>nil) and (FConnection.PendingNotificationCount>0) then exit;
  for i:=0 to High(FSubs) do
    if Int64(Now_)-Int64(FSubs[i].LastNotificationAt)>Limit then begin
      ConnDebug(Self, Format('Subscricao 0x%.8x: sem notificacoes ha mais de %d ms - subscricoes serao recriadas.',
                             [FSubs[i].ObjectId, Limit]));
      FSubResetPending := true;
      exit;
    end;
end;

procedure TS7CommPlusDriver.ManageSubscriptions(Now_:QWord);
const
  MaxCreateAttempts = 3;
  RetryDelayMs = 2000;
var
  Pending:array of Integer;
  Items:TS7PlusMultiReadItemArray;
  i, j, k, FirstPending, n:Integer;
  Idx:Integer;
  NextId:Cardinal;
  St:TS7PlusSubscriptionState;
begin
  SetLength(Pending, 0);
  for i:=0 to High(FScanList) do
    if FScanList[i].OnChange and FScanList[i].Resolved and (not FScanList[i].Subscribed) and
       (not FScanList[i].SubFallback) then begin
      SetLength(Pending, Length(Pending)+1);
      Pending[High(Pending)] := i;
    end;
  if Length(Pending)=0 then exit;
  if Now_<FSubRetryAt then exit;

  if not FConnection.HasSubscriptionContainer then begin
    ConnDebug(Self, 'ManageSubscriptions: sessao sem container de subscricoes - tags RefreshTime=0 serao lidas por polling.');
    for i:=0 to High(Pending) do
      FScanList[Pending[i]].SubFallback := true;
    exit;
  end;

  FirstPending := 0;
  while FirstPending<=High(Pending) do begin
    //Group whole entries (a composite keeps all its members together) up to the item cap.
    SetLength(Items, 0);
    NextId := FSubNextRefId;
    k := FirstPending;
    while k<=High(Pending) do begin
      Idx := Pending[k];
      if (Length(Items)>0) and (Length(Items)+EntryItemCount(Idx)>FMaxItemsPerSubscription) then break;
      n := Length(Items);
      SetLength(Items, n+EntryItemCount(Idx));
      if FScanList[Idx].IsComposite then begin
        for j:=0 to High(FScanList[Idx].CompositeItems) do begin
          Items[n+j].AccessArea := FScanList[Idx].CompositeItems[j].AccessArea;
          Items[n+j].Lids := FScanList[Idx].CompositeItems[j].Lids;
          Items[n+j].RefId := NextId;
          Inc(NextId);
        end;
      end else begin
        Items[n].AccessArea := FScanList[Idx].AccessArea;
        Items[n].Lids := FScanList[Idx].Lids;
        Items[n].RefId := NextId;
        Inc(NextId);
      end;
      Inc(k);
    end;

    if FConnection.SubscriptionCreate(Items, FSubCycleMs, $14, false, true, FSubCreditLimit) then begin
      St.ObjectId := FConnection.LastSubscriptionObjectId;
      RegisterSubscription(St.ObjectId, FSubCreditLimit, Now_);

      NextId := FSubNextRefId;
      for i:=FirstPending to k-1 do begin
        MarkSubscribed(FScanList[Pending[i]].Path, NextId);
        Inc(NextId, EntryItemCount(Pending[i]));
      end;
      FSubNextRefId := NextId;
      FSubFailCount := 0;
      ConnDebug(Self, Format('ManageSubscriptions: subscricao 0x%.8x criada com %d itens (%d tags)',
                             [St.ObjectId, Length(Items), k-FirstPending]));
    end else begin
      Inc(FSubFailCount);
      FSubRetryAt := Now_+RetryDelayMs;
      ConnDebug(Self, Format('ManageSubscriptions: falha ao criar subscricao (tentativa %d de %d)',
                             [FSubFailCount, MaxCreateAttempts]));
      if FSubFailCount>=MaxCreateAttempts then begin
        for i:=FirstPending to High(Pending) do
          FScanList[Pending[i]].SubFallback := true;
        FSubFailCount := 0;
        ConnDebug(Self, 'ManageSubscriptions: desistindo - tags RefreshTime=0 pendentes serao lidas por polling.');
      end;
      exit;
    end;
    FirstPending := k;
  end;
end;

procedure TS7CommPlusDriver.ProcessNotification(const Values:TS7PlusNotificationValueArray;
                                                const Errors:TS7PlusNotificationErrorArray; Now_:QWord);
var
  i, b, NeedLen, EntryIdx, ItemIdx, Off:Integer;
  Touched:array of Boolean;
begin
  SetLength(Touched, Length(FScanList));
  for i:=0 to High(Touched) do
    Touched[i] := false;

  for i:=0 to High(Values) do begin
    if not FindEntryByRefId(Values[i].RefId, EntryIdx, ItemIdx) then continue;
    if FScanList[EntryIdx].IsComposite then begin
      //Only the members that changed are sent: splice them into what is already there.
      Off := FScanList[EntryIdx].CompositeItems[ItemIdx].ByteOffset;
      NeedLen := Off+Length(Values[i].Data);
      if NeedLen>Length(FScanList[EntryIdx].Data) then
        SetLength(FScanList[EntryIdx].Data, NeedLen);
      for b:=0 to High(Values[i].Data) do
        FScanList[EntryIdx].Data[Off+b] := Values[i].Data[b];
    end else
      FScanList[EntryIdx].Data := Values[i].Data;
    FScanList[EntryIdx].LastResult := ioOk;
    Touched[EntryIdx] := true;
  end;

  //After the values: within one notification an error outweighs a value of the same tag.
  for i:=0 to High(Errors) do begin
    if not FindEntryByRefId(Errors[i].RefId, EntryIdx, ItemIdx) then continue;
    FScanList[EntryIdx].LastResult := FailResult;
    Touched[EntryIdx] := true;
  end;

  //The new timestamp is what tells the framework there is something to push to the tag.
  for i:=0 to High(Touched) do
    if Touched[i] then
      FScanList[i].LastScanTimeStamp := Now_;
end;

//: A positive credit limit stops the PLC once the notification CreditTick reaches it, so it
//: is raised ahead of the tick, by 5 each time - the rule the S7CommPlusDriver author's own
//: long-running loop uses. -1 (unlimited) needs nothing.
procedure TS7CommPlusDriver.RenewCredit(SubscriptionId:Cardinal; CreditTick:Byte);
var
  i:Integer;
begin
  if FSubCreditLimit<=0 then exit;
  for i:=0 to High(FSubs) do
    if FSubs[i].ObjectId=SubscriptionId then begin
      if CreditTick>=FSubs[i].NextCreditLimit-1 then begin
        FSubs[i].NextCreditLimit := (FSubs[i].NextCreditLimit+5) mod 255;
        ConnDebug(Self, Format('Subscricao 0x%.8x: CreditTick=%d, novo limite de credito %d',
                               [SubscriptionId, CreditTick, FSubs[i].NextCreditLimit]));
        FConnection.SubscriptionSetCreditLimitFor(SubscriptionId, FSubs[i].NextCreditLimit);
      end;
      exit;
    end;
end;

//: Takes in notifications: the first waits up to WaitMs for one to arrive, the rest only
//: drain what is already there. Bounded, so a flood cannot starve the polled tags.
procedure TS7CommPlusDriver.PumpNotifications(WaitMs:LongInt);
const
  MaxPerCall = 64;
var
  Got:Boolean;
  Credit:Byte;
  Values:TS7PlusNotificationValueArray;
  Errors:TS7PlusNotificationErrorArray;
  n:Integer;
begin
  if (FConnection=nil) or (not FConnection.Connected) then exit;
  if (Length(FSubs)=0) and (FConnection.PendingNotificationCount=0) then exit;

  for n:=1 to MaxPerCall do begin
    if not FConnection.PollNotification(WaitMs, Got, Credit, Values, Errors) then begin
      //Data was there and could not be read: the stream is no longer trustworthy.
      ConnDebug(Self, 'PumpNotifications: falha ao receber notificacao - subscricoes serao recriadas.');
      FSubResetPending := true;
      exit;
    end;
    if not Got then exit;
    ProcessNotification(Values, Errors, GetTickCount64);
    TouchSubscription(FConnection.LastNotificationSubscriptionId, GetTickCount64);
    RenewCredit(FConnection.LastNotificationSubscriptionId, Credit);
    WaitMs := 0;
  end;
end;

procedure TS7CommPlusDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
var
  Idx, ContainerIdx, i, Len:Integer;
begin
  //A composite ("whole DB/area") entry's assembled Data spans every one of its members,
  //starting at offset 0 - "reading the whole DB" as literally as the wire protocol allows.
  //If the tag declared a smaller Size than that (e.g. a TPLCStruct meant to mirror only the
  //DB's leading fields), truncate to it here; Size=0 (undeclared) keeps the full assembly.
  SetLength(values.Values,0);
  if not FindScanEntry(TagRec.Path, Idx) then begin
    values.LastQueryResult := ioNullDriver;
    exit;
  end;

  if FScanList[Idx].IsDerived then begin
    //DoScanRead never reads a derived entry on its own - its bytes are sliced out of
    //whichever entry it derives from (see TryDeduplicateEntry), which the container's
    //OWN independent scan already keeps fresh.
    if not FindScanEntry(FScanList[Idx].ContainerPath, ContainerIdx) then begin
      //Container was removed since this was marked derived; DoDelTag resets Resolved to
      //make it re-attach (or go independent) on the next scan, but until then report NA.
      values.LastQueryResult := ioNullDriver;
      exit;
    end;
    values.ClkMonotonicTStamp := FScanList[ContainerIdx].LastScanTimeStamp;
    values.LastQueryResult := FScanList[ContainerIdx].LastResult;
    if (FScanList[ContainerIdx].LastResult=ioOk) and (FScanList[Idx].DerivedOffset>=0) and
       (FScanList[Idx].DerivedOffset+FScanList[Idx].DerivedLen<=Length(FScanList[ContainerIdx].Data)) then begin
      Len := FScanList[Idx].DerivedLen;
      SetLength(values.Values, Len);
      for i:=0 to Len-1 do
        values.Values[i] := FScanList[ContainerIdx].Data[FScanList[Idx].DerivedOffset+i];
      values.ReadsOK := 1;
      values.ReadFaults := 0;
    end else begin
      values.ReadsOK := 0;
      values.ReadFaults := 1;
    end;
    exit;
  end;

  values.ClkMonotonicTStamp := FScanList[Idx].LastScanTimeStamp;
  values.LastQueryResult := FScanList[Idx].LastResult;

  if FScanList[Idx].LastResult=ioOk then begin
    Len := Length(FScanList[Idx].Data);
    if FScanList[Idx].IsComposite and (TagRec.Size>0) and (TagRec.Size<Len) then
      Len := TagRec.Size;
    SetLength(values.Values, Len);
    for i:=0 to Len-1 do
      values.Values[i] := FScanList[Idx].Data[i];
    values.ReadsOK := 1;
    values.ReadFaults := 0;
  end else begin
    values.ReadsOK := 0;
    values.ReadFaults := 1;
  end;
end;

function TS7CommPlusDriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  ElementItems:TS7PlusCompositeItemArray;
  Data:TBytes;
  ok:Boolean;
  i:Integer;
  AreaRID:Cardinal;
  AccessArea:Cardinal;
  Lids:TS7PlusLIDArray;
  DummyOffset, DummySize:Integer;
  DummyType:Byte;
  CompositeItems:TS7PlusCompositeItemArray;
  Len:Integer;
begin
  SetLength(Values,0);

  if not EnsureConnected then begin
    Result := FailResult;
    exit;
  end;

  //Symbolic (Path) tags: resolve and read by LID, bypassing the numeric Rack/Slot/DB/
  //Address addressing below entirely. Covers synchronous Tag.Read (DoScanRead/DoGetValue
  //handle the polled/async path separately, with their own resolution cache). A bare-root
  //("whole DB/area") Path resolves with a non-empty CompositeItems - read via a batched
  //multi-item request instead of the (otherwise meaningless) empty-LID ReadSymbolic.
  if tagrec.Path<>'' then begin
    if not ResolvePath(tagrec.Path, AccessArea, Lids, DummyOffset, DummyType, CompositeItems, DummySize, ElementItems) then begin
      Result := FailResult;
      exit;
    end;
    if Length(CompositeItems)>0 then
      ok := ReadComposite(CompositeItems, Data)
    else
      ok := FConnection.ReadSymbolic(AccessArea, Lids, Data);
    if ok then begin
      Len := Length(Data);
      if (Length(CompositeItems)>0) and (tagrec.Size>0) and (tagrec.Size<Len) then
        Len := tagrec.Size;
      SetLength(Values, Len);
      for i:=0 to Len-1 do
        Values[i] := Data[i];
      Result := ioOk;
    end else
      Result := FailResult;
    exit;
  end;

  //TODO: numeric addressing does not work on the PLCSim (see BuildAreaPayload); until it is
  //emulated through the browse, a tag without Path gets wrong data at offset 0 or an error.
  if tagrec.ReadFunction=S7PlusRF_DB then
    ok := FConnection.DBRead(tagrec.File_DB, tagrec.Address, tagrec.Size, Data)
  else if AreaRIDOf(tagrec.ReadFunction, AreaRID) then
    ok := FConnection.ReadArea(AreaRID, tagrec.Address, tagrec.Size, Data)
  else
    ok := false;

  if ok then begin
    SetLength(Values, Length(Data));
    for i:=0 to High(Data) do
      Values[i] := Data[i];
    Result := ioOk;
  end else
    Result := FailResult;
end;

function TS7CommPlusDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  ElementItems:TS7PlusCompositeItemArray;
  Data:TBytes;
  ok:Boolean;
  i:Integer;
  AreaRID:Cardinal;
  AccessArea:Cardinal;
  Lids:TS7PlusLIDArray;
  DummyOffset, DummySize:Integer;
  WriteSoftDataType:Byte;
  DummyComposite:TS7PlusCompositeItemArray;
  CompItems:TS7PlusMultiWriteItemArray;
  CompResults:TS7PlusMultiWriteResultArray;
begin
  if not EnsureConnected then begin
    Result := FailResult;
    exit;
  end;

  SetLength(Data, Length(Values));
  for i:=0 to High(Values) do
    Data[i] := Trunc(Values[i]) and $FF;

  //Symbolic (Path) tags: resolve and write by LID, bypassing the numeric addressing below.
  //A bare-root ("whole DB/area") Path is written member by member (see ExpandCompositeWrite):
  //the tag's bytes are cut at each scalar member's ByteOffset; BOOL, STRING and struct members
  //are not written. The resolved
  //SoftDataType is required so WriteSymbolic can encode the value as the target's own typed
  //PValue (ValueDInt, ValueReal, ...) - a generic byte-array/blob value is rejected by the
  //PLC outright, confirmed against real hardware.
  if tagrec.Path<>'' then begin
    if not ResolvePath(tagrec.Path, AccessArea, Lids, DummyOffset, WriteSoftDataType, DummyComposite, DummySize, ElementItems) then begin
      Result := FailResult;
      exit;
    end;
    //Whole DB/area: one write per scalar member, in as few requests as possible.
    if Length(DummyComposite)>0 then begin
      SetLength(CompItems, 0);
      if (ExpandCompositeWrite(DummyComposite, Values, CompItems, tagrec.OffSet)>0) and
         FConnection.WriteMultipleSymbolic(CompItems, CompResults) then begin
        Result := ioOk;
        for i:=0 to High(CompItems) do
          if (i>High(CompResults)) or (not CompResults[i].Ok) then Result := FailResult;
      end else
        Result := FailResult;
      exit;
    end;
    //An array of scalars: written element by element (each by its own address), whole or in part
    if Length(ElementItems)>0 then begin
      SetLength(CompItems, 0);
      if (ExpandCompositeWrite(ElementItems, Values, CompItems, tagrec.OffSet)>0) and
         FConnection.WriteMultipleSymbolic(CompItems, CompResults) then begin
        Result := ioOk;
        for i:=0 to High(CompItems) do
          if (i>High(CompResults)) or (not CompResults[i].Ok) then Result := FailResult;
      end else
        Result := FailResult;
      exit;
    end;
    //Part of a value (a slice of something else): the PLC writes a variable whole, so the rest of
    //it is read first and written back with the part replaced.
    if (tagrec.OffSet<>0) or ((DummySize>0) and (Length(Values)<DummySize)) then
      if not PatchedValue(AccessArea, Lids, tagrec.OffSet, Values, Data) then begin
        Result := FailResult;
        exit;
      end;
    if FConnection.WriteSymbolic(AccessArea, Lids, Data, 0, WriteSoftDataType) then
      Result := ioOk
    else
      Result := FailResult;
    exit;
  end;

  if tagrec.WriteFunction=S7PlusRF_DB then
    ok := FConnection.DBWrite(tagrec.File_DB, tagrec.Address, Data)
  else if AreaRIDOf(tagrec.WriteFunction, AreaRID) then
    ok := FConnection.WriteArea(AreaRID, tagrec.Address, Data)
  else
    ok := false;

  if ok then
    Result := ioOk
  else
    Result := FailResult;
end;

procedure TS7CommPlusDriver.DoMultiWrite(var Writes:TMultiWriteArray; Sync:Boolean);
var
  ElementItems:TS7PlusCompositeItemArray;
  Items:TS7PlusMultiWriteItemArray;
  Results:TS7PlusMultiWriteResultArray;
  Owner:array of Integer;  //Items[k] belongs to Writes[Owner[k]]
  i, k, b, n:Integer;
  Lids:TS7PlusLIDArray;
  AccessArea:Cardinal;
  DummyOffset, DummySize:Integer;
  SoftType:Byte;
  DummyComposite:TS7PlusCompositeItemArray;
begin
  if not EnsureConnected then begin
    for i:=0 to High(Writes) do
      Writes[i].Result := FailResult;
    exit;
  end;

  SetLength(Items, 0);
  SetLength(Owner, 0);
  for i:=0 to High(Writes) do begin
    if Writes[i].Tag.Path='' then begin
      //numeric addressing: nothing to gather, the single write already does it
      Writes[i].Result := DoWrite(Writes[i].Tag, Writes[i].Values, Sync);
      continue;
    end;
    if not ResolvePath(Writes[i].Tag.Path, AccessArea, Lids, DummyOffset, SoftType, DummyComposite, DummySize, ElementItems) then begin
      Writes[i].Result := FailResult; //same answer DoWrite gives for a Path it cannot resolve
      continue;
    end;
    if Length(DummyComposite)>0 then begin
      //Whole DB/area: its scalar members join the batch, all owned by this write.
      k := Length(Items);
      n := ExpandCompositeWrite(DummyComposite, Writes[i].Values, Items, Writes[i].Tag.OffSet);
      if n=0 then begin
        Writes[i].Result := FailResult;
        continue;
      end;
      SetLength(Owner, Length(Items));
      for b:=k to High(Items) do
        Owner[b] := i;
      Writes[i].Result := ioOk;
      continue;
    end;
    if Length(ElementItems)>0 then begin
      //An array of scalars: its elements join the batch, each by its own address
      k := Length(Items);
      n := ExpandCompositeWrite(ElementItems, Writes[i].Values, Items, Writes[i].Tag.OffSet);
      if n=0 then begin
        Writes[i].Result := FailResult;
        continue;
      end;
      SetLength(Owner, Length(Items));
      for b:=k to High(Items) do
        Owner[b] := i;
      Writes[i].Result := ioOk;
      continue;
    end;
    k := Length(Items);
    SetLength(Items, k+1);
    SetLength(Owner, k+1);
    Owner[k] := i;
    Items[k].AccessArea := AccessArea;
    Items[k].Lids := Lids;
    Items[k].SoftDataType := SoftType;
    SetLength(Items[k].Data, Length(Writes[i].Values));
    for b:=0 to High(Writes[i].Values) do
      Items[k].Data[b] := Trunc(Writes[i].Values[b]) and $FF;
    //part of a value: read the rest and write it whole (see PatchedValue)
    if (Writes[i].Tag.OffSet<>0) or ((DummySize>0) and (Length(Writes[i].Values)<DummySize)) then
      if not PatchedValue(AccessArea, Lids, Writes[i].Tag.OffSet, Writes[i].Values, Items[k].Data) then begin
        SetLength(Items, k);
        SetLength(Owner, k);
        Writes[i].Result := FailResult;
        continue;
      end;
    Writes[i].Result := ioOk;
  end;

  if Length(Items)=0 then exit;

  //Result=false: a request failed as a whole; Results still says what the earlier ones did,
  //and anything not Ok counts as a failed write either way.
  FConnection.WriteMultipleSymbolic(Items, Results);
  //A write is Ok only when every item it contributed (several for a whole DB) was written.
  for k:=0 to High(Items) do
    if (k>High(Results)) or (not Results[k].Ok) then
      Writes[Owner[k]].Result := FailResult;
end;

function TS7CommPlusDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  //Symbolic (Path) tags: like the classic S7 driver, every type is returned as byte -
  //the TagType the user picks (pttDInt, pttReal, ...) is what determines how many bytes
  //BuildTagRec asks for; no I/O is needed (or allowed) here - this runs synchronously,
  //possibly at design time, before any connection exists.
  if (aTag is TPLCTagNumber) or (aTag is TPLCBlock) or (aTag is TPLCString) then begin
    ProtocolTagType := ptByte;
    Result := 8;
  end else begin
    ProtocolTagType := ptUnknown;
    Result := 0;
  end;
end;

//===========================================================================
// Phase 1 direct access helpers
//===========================================================================

function TS7CommPlusDriver.ReadArea(ReadFunction:LongInt; Start, Size:Integer; out Data:TBytes):TProtocolIOResult;
var
  tr:TTagRec;
  Values:TArrayOfDouble;
  i:Integer;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.ReadFunction := ReadFunction;
  tr.Address := Start;
  tr.Size := Size;

  LockDriverIO;
  try
    Result := DoRead(tr, Values, true);
  finally
    UnlockDriverIO;
  end;
  SetLength(Data, Length(Values));
  for i:=0 to High(Values) do
    Data[i] := Trunc(Values[i]) and $FF;
end;

function TS7CommPlusDriver.WriteArea(ReadFunction:LongInt; Start:Integer; const Data:TBytes):TProtocolIOResult;
var
  tr:TTagRec;
  Values:TArrayOfDouble;
  i:Integer;
begin
  if ReadOnly then begin
    Result := ioReadOnlyProtocol;
    exit;
  end;
  FillChar(tr, SizeOf(tr), 0);
  tr.WriteFunction := ReadFunction;
  tr.Address := Start;
  tr.Size := Length(Data);

  SetLength(Values, Length(Data));
  for i:=0 to High(Data) do
    Values[i] := Data[i];

  LockDriverIO;
  try
    Result := DoWrite(tr, Values, true);
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.ReadDB(DBNumber, Start, Size:Integer; out Data:TBytes):TProtocolIOResult;
var
  tr:TTagRec;
  Values:TArrayOfDouble;
  i:Integer;
begin
  FillChar(tr, SizeOf(tr), 0);
  tr.ReadFunction := S7PlusRF_DB;
  tr.File_DB := DBNumber;
  tr.Address := Start;
  tr.Size := Size;

  LockDriverIO;
  try
    Result := DoRead(tr, Values, true);
  finally
    UnlockDriverIO;
  end;
  SetLength(Data, Length(Values));
  for i:=0 to High(Values) do
    Data[i] := Trunc(Values[i]) and $FF;
end;

function TS7CommPlusDriver.WriteDB(DBNumber, Start:Integer; const Data:TBytes):TProtocolIOResult;
var
  tr:TTagRec;
  Values:TArrayOfDouble;
  i:Integer;
begin
  if ReadOnly then begin
    Result := ioReadOnlyProtocol;
    exit;
  end;
  FillChar(tr, SizeOf(tr), 0);
  tr.WriteFunction := S7PlusRF_DB;
  tr.File_DB := DBNumber;
  tr.Address := Start;
  tr.Size := Length(Data);

  SetLength(Values, Length(Data));
  for i:=0 to High(Data) do
    Values[i] := Data[i];

  LockDriverIO;
  try
    Result := DoWrite(tr, Values, true);
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.BrowseDB(DBNumber:Cardinal; out Vars:TS7PlusVarInfoArray):TProtocolIOResult;
begin
  SetLength(Vars, 0);
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.BrowseDB(DBNumber, Vars) then begin
      Result := ioOk;
      LearnProgram(DBNumber, FConnection.LastBrowseTiRid);
    end else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.BrowseNativeArea(AreaRID, TiRid:Cardinal; const AreaName:String; out Vars:TS7PlusVarInfoArray):TProtocolIOResult;
begin
  SetLength(Vars, 0);
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.BrowseNativeArea(AreaRID, TiRid, AreaName, Vars) then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

procedure TS7CommPlusDriver.RefreshCaches;
begin
  LockDriverIO;
  try
    DropCaches;
  finally
    UnlockDriverIO;
  end;
end;

procedure TS7CommPlusDriver.SetProgramCheckMs(AValue:Integer);
begin
  if AValue<=0 then
    AValue := 0
  else if AValue<1000 then
    AValue := 1000;
  FProgramCheckMs := AValue;
end;

procedure TS7CommPlusDriver.DropCaches;
var
  i:Integer;
begin
  begin
    if FConnection<>nil then
      FConnection.RefreshTypeInfo;
    SetLength(FDBCache, 0);
    SetLength(FDBNameCache, 0);
    FDBNameCacheLoaded := false;
    SetLength(FIAreaVars, 0);
    SetLength(FQAreaVars, 0);
    SetLength(FMAreaVars, 0);
    SetLength(FTimersVars, 0);
    SetLength(FCountersVars, 0);
    FIAreaBrowsed := false;
    FQAreaBrowsed := false;
    FMAreaBrowsed := false;
    FTimersBrowsed := false;
    FCountersBrowsed := false;

    //the tags: their address (LIDs, offsets, which tag reads inside which) may be another now
    for i:=0 to High(FScanList) do begin
      FScanList[i].Resolved := false;
      FScanList[i].LastResolveAttempt := 0;
      FScanList[i].IsDerived := false;
      FScanList[i].ContainerPath := '';
      FScanList[i].DerivedOffset := 0;
      FScanList[i].DerivedLen := 0;
      FScanList[i].IsComposite := false;
      SetLength(FScanList[i].CompositeItems, 0);
      SetLength(FScanList[i].Lids, 0);
      FScanList[i].ByteOffset := -1;
    end;
    //and so is what the PLC subscribed: the subscriptions are made again
    FSubResetPending := true;
    //what the program looked like is learned again when the caches are filled
    FProgramKnown := false;
    FProgramSignature := '';
    SetLength(FTypeTimes, 0);
    SetLength(FWatch, 0);
  end;
end;

function TS7CommPlusDriver.NoteProgram(const Signature:String; const Times:TS7PlusTypeTimeArray):Boolean;
var
  i, j, n:Integer;
  Found, Changed:Boolean;
begin
  Result := false;
  if not FProgramKnown then begin
    FProgramKnown := true;
    FProgramSignature := Signature;
    FTypeTimes := Copy(Times, 0, Length(Times));
    n := 0;
    for i:=0 to High(Times) do
      if Times[i].HasTime then Inc(n);
    ConnDebug(Self, Format('CheckProgramChange: referencia aprendida - %d DBs navegados, %d com data de modificacao.',
                           [Length(Times), n]));
    exit;
  end;

  Changed := Signature<>FProgramSignature;
  if Changed then FProgramChangeWhy := 'a lista de DBs mudou: '+FProgramSignature+' -> '+Signature;
  //the time of the type of each DB kept: another one, or not answered any more, is a download
  if not Changed then
    for i:=0 to High(FTypeTimes) do begin
      if not FTypeTimes[i].HasTime then continue;       //the PLC never reported one for it
      Found := false;
      for j:=0 to High(Times) do
        if Times[j].TiRid=FTypeTimes[i].TiRid then begin
          Found := true;
          if (not Times[j].HasTime) or (Length(Times[j].Time)<>Length(FTypeTimes[i].Time)) or
             ((Length(Times[j].Time)>0) and (not CompareMem(@Times[j].Time[0], @FTypeTimes[i].Time[0], Length(Times[j].Time)))) then
            Changed := true;
          if Changed then
            FProgramChangeWhy := Format('a data do tipo $%.8x mudou (%d -> %d bytes, ou deixou de ser informada)',
                                        [FTypeTimes[i].TiRid, Length(FTypeTimes[i].Time), Length(Times[j].Time)]);
          break;
        end;
      if not Found then begin
        Changed := true;
        FProgramChangeWhy := Format('tipo $%.8x deixou de ser consultado', [FTypeTimes[i].TiRid]);
      end;
      if Changed then break;
    end;

  if Changed then begin
    DropCaches;
    Result := true;
    exit;
  end;

  //a DB browsed after the baseline: its time is the baseline from now on
  for j:=0 to High(Times) do begin
    Found := false;
    for i:=0 to High(FTypeTimes) do
      if FTypeTimes[i].TiRid=Times[j].TiRid then Found := true;
    if not Found then begin
      n := Length(FTypeTimes);
      SetLength(FTypeTimes, n+1);
      FTypeTimes[n] := Times[j];
    end;
  end;
end;

procedure TS7CommPlusDriver.WatchDB(DBNumber, TiRid:Cardinal);
var
  i, n:Integer;
begin
  if TiRid=0 then exit;
  for i:=0 to High(FWatch) do
    if FWatch[i].DBNumber=DBNumber then begin
      FWatch[i].TiRid := TiRid;
      exit;
    end;
  n := Length(FWatch);
  SetLength(FWatch, n+1);
  FWatch[n].DBNumber := DBNumber;
  FWatch[n].TiRid := TiRid;
end;

function TS7CommPlusDriver.WatchedDBCount:Integer;
begin
  Result := Length(FWatch);
end;

procedure TS7CommPlusDriver.LearnProgram(DBNumber, TiRid:Cardinal);
var
  Blocks:TS7PlusDataBlockInfoArray;
  Value:TBytes;
  T:TS7PlusTypeTime;
  i, n:Integer;
  Known:Boolean;
begin
  if (FConnection=nil) or (TiRid=0) then exit;
  WatchDB(DBNumber, TiRid);

  //the list of DBs the program had when the first one was browsed
  if not FProgramKnown then
    if FConnection.ListDataBlocks(Blocks) then begin
      FProgramKnown := true;
      FProgramSignature := S7PlusBlockSignature(Blocks);
    end;

  //the time of this type, learned once
  Known := false;
  for i:=0 to High(FTypeTimes) do
    if FTypeTimes[i].TiRid=TiRid then Known := true;
  if Known then exit;
  if not FConnection.ReadTypeInfoModTime(TiRid, Value) then exit;
  T.TiRid := TiRid;
  T.Time := Value;
  T.HasTime := Length(Value)>0;
  n := Length(FTypeTimes);
  SetLength(FTypeTimes, n+1);
  FTypeTimes[n] := T;
end;

procedure TS7CommPlusDriver.CheckProgramNow(const Blocks:TS7PlusDataBlockInfoArray);
var
  Times:TS7PlusTypeTimeArray;
  Empty:TS7PlusTypeTimeArray;
  i, n, tries:Integer;
  Value:TBytes;
  Sig:String;

  function BaselineHasTime(ARid:Cardinal):Boolean;
  var
    k:Integer;
  begin
    Result := false;
    for k:=0 to High(FTypeTimes) do
      if (FTypeTimes[k].TiRid=ARid) and FTypeTimes[k].HasTime then Result := true;
  end;

begin
  if FConnection=nil then exit;
  Sig := S7PlusBlockSignature(Blocks);
  SetLength(Times, 0);
  SetLength(Empty, 0);
  for i:=0 to High(FWatch) do begin
    if not FConnection.ReadTypeInfoModTime(FWatch[i].TiRid, Value) then exit;   //a failed request is not a change
    //The time a type had and that is not in the answer: a download that replaced the type, or an
    //answer that was not the one asked for (a PLC busy with something else). Ask again a couple
    //of times before taking it for a change.
    tries := 0;
    while (Length(Value)=0) and BaselineHasTime(FWatch[i].TiRid) and (tries<2) do begin
      Inc(tries);
      Sleep(100);
      if not FConnection.ReadTypeInfoModTime(FWatch[i].TiRid, Value) then exit;
    end;
    n := Length(Times);
    SetLength(Times, n+1);
    Times[n].TiRid := FWatch[i].TiRid;
    Times[n].Time := Value;
    Times[n].HasTime := Length(Value)>0;
  end;
  if NoteProgram(Sig, Times) then begin
    ConnDebug(Self, 'CheckProgramNow: o programa do CLP mudou ('+FProgramChangeWhy+') - os tipos serao lidos de novo e os tags resolvidos de novo.');
    //the list just read is the new baseline
    NoteProgram(Sig, Empty);
  end;
end;

procedure TS7CommPlusDriver.CheckProgramChange(Now_:QWord);
var
  Blocks:TS7PlusDataBlockInfoArray;
begin
  if (FProgramCheckMs<=0) or (FConnection=nil) or (not FConnection.Connected) then exit;
  //nothing browsed, nothing to keep right
  if Length(FWatch)=0 then exit;
  if (FLastProgramCheck<>0) and (Int64(Now_)-Int64(FLastProgramCheck)<FProgramCheckMs) then exit;
  FLastProgramCheck := Now_;

  if not FConnection.ListDataBlocks(Blocks) then exit;
  CheckProgramNow(Blocks);
end;

function TS7CommPlusDriver.ListDataBlocks(out Blocks:TS7PlusDataBlockInfoArray):TProtocolIOResult;
begin
  SetLength(Blocks, 0);
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.ListDataBlocks(Blocks) then begin
      Result := ioOk;
      //a download since the DBs were last browsed (or since this was last asked): the browse
      //that follows must not use the old types
      CheckProgramNow(Blocks);
    end else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

procedure TS7CommPlusDriver.OpenTagEditor(InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
begin
  if Assigned(S7PlusTagBuilderEditor) then
    S7PlusTagBuilderEditor(Self, Self.Owner, InsertHook, CreateProc)
  else
    inherited OpenTagEditor(InsertHook, CreateProc);
end;

function TS7CommPlusDriver.HasTabBuilderEditor:Boolean;
begin
  Result := true;
end;

function TS7CommPlusDriver.SupportsListTags:Boolean;
begin
  Result := true;
end;

function TS7CommPlusDriver.ResolveStringLengths(var Candidates:TS7PlusCandidateArray):TProtocolIOResult;
var
  TCand, TMem:array of Integer;     //what each item is for: the candidate and, for a member of an array of structures, which
  Items:TS7PlusMultiReadItemArray;
  Results:TS7PlusMultiReadResultArray;
  i, m, n, Len:Integer;
  Failed:Boolean;
  Area:Cardinal;
  ItemLids:TS7PlusLIDArray;

  procedure AddTarget(ACand, AMem:Integer; AArea:Cardinal; const ALids:TS7PlusLIDArray);
  begin
    SetLength(TCand, Length(TCand)+1);
    SetLength(TMem, Length(TMem)+1);
    TCand[High(TCand)] := ACand;
    TMem[High(TMem)] := AMem;
    SetLength(Items, Length(Items)+1);
    FillChar(Items[High(Items)], SizeOf(Items[High(Items)]), 0);
    Items[High(Items)].AccessArea := AArea;
    Items[High(Items)].Lids := ALids;
    Items[High(Items)].ExpectedSize := 256;     //the length is what is being asked: assume the biggest String
  end;

begin
  SetLength(TCand, 0);
  SetLength(TMem, 0);
  SetLength(Items, 0);
  Failed := false;
  Result := ioOk;

  for i:=0 to High(Candidates) do begin
    //a STRING variable
    if Candidates[i].IsString and (Candidates[i].StringLength<0) and (Candidates[i].Skip=srNone) then begin
      if (Candidates[i].AccessArea<>0) and (Length(Candidates[i].Lids)>0) then
        AddTarget(i, -1, Candidates[i].AccessArea, Candidates[i].Lids)
      else begin
        Candidates[i].Skip := srStringLength;
        Failed := true;
      end;
    end;
    //the STRING members of the structure of an array of structures: the first element tells
    if Candidates[i].IsStructArray then
      for m:=0 to High(Candidates[i].Members) do
        if Candidates[i].Members[m].IsString and (Candidates[i].Members[m].StringLength<0) and
           Candidates[i].Members[m].Supported then begin
          if (Candidates[i].Members[m].AccessArea<>0) and (Length(Candidates[i].Members[m].Lids)>0) then
            AddTarget(i, m, Candidates[i].Members[m].AccessArea, Candidates[i].Members[m].Lids)
          else begin
            Candidates[i].Members[m].Supported := false;
            Failed := true;
          end;
        end;
  end;
  if Length(Items)=0 then begin
    if Failed then Result := ioPartialOk;
    exit;
  end;

  SetLength(Results, 0);
  Result := ReadMultipleSymbolic(Items, Results);

  for n:=0 to High(TCand) do begin
    Len := -1;
    if (Result=ioOk) and (n<=High(Results)) and Results[n].Ok and S7PlusStringLengthOf(Results[n].Data, Len) then begin
      if TMem[n]<0 then
        Candidates[TCand[n]].StringLength := Len
      else
        Candidates[TCand[n]].Members[TMem[n]].StringLength := Len;
    end else begin
      Failed := true;
      if TMem[n]<0 then
        Candidates[TCand[n]].Skip := srStringLength
      else
        Candidates[TCand[n]].Members[TMem[n]].Supported := false;
    end;
  end;

  if Failed and (Result=ioOk) then
    Result := ioPartialOk;
end;

function TS7CommPlusDriver.ListTags(out Tags:TPLCTagInfoArray; ForceReload:Boolean):TProtocolIOResult;
var
  Blocks:TS7PlusDataBlockInfoArray;
  Vars:TS7PlusVarInfoArray;
  Cands:TS7PlusCandidateArray;
  i:Integer;
  Partial:Boolean;

  procedure BrowseArea(AreaRID, TiRid:Cardinal; const AreaName:String);
  begin
    if BrowseNativeArea(AreaRID, TiRid, AreaName, Vars)=ioOk then
      S7PlusAddBrowsed(Cands, Vars, '', AreaRID)
    else
      Partial := true;
  end;

begin
  SetLength(Tags, 0);
  SetLength(Cands, 0);
  Partial := false;
  if ForceReload then
    RefreshCaches;

  Result := ListDataBlocks(Blocks);
  if Result<>ioOk then exit;

  for i:=0 to High(Blocks) do
    if BrowseDB(Blocks[i].Number, Vars)=ioOk then
      S7PlusAddBrowsed(Cands, Vars, Blocks[i].Name, S7PlusIds_DBAccessAreaBase+Blocks[i].Number)   //'' (no symbolic name) keeps DB<n>
    else
      Partial := true;

  BrowseArea(S7PlusIds_NativeIAreaRID, S7PlusTiRid_IArea, 'IArea');
  BrowseArea(S7PlusIds_NativeQAreaRID, S7PlusTiRid_QArea, 'QArea');
  BrowseArea(S7PlusIds_NativeMAreaRID, S7PlusTiRid_MArea, 'MArea');

  S7PlusMarkArrays(Cands);
  //the maximum length of each STRING is in its value
  if ResolveStringLengths(Cands)<>ioOk then
    Partial := true;
  Tags := S7PlusTagInfosOf(Cands);

  if Partial and SessionNeedsAuth then
    Result := ioAuthError
  else if Partial then
    Result := ioPartialOk
  else
    Result := ioOk;
end;

function TS7CommPlusDriver.ReadSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; out Data:TBytes):TProtocolIOResult;
begin
  SetLength(Data, 0);
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.ReadSymbolic(AccessArea, Lids, Data) then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.ReadMultipleSymbolic(const Items:TS7PlusMultiReadItemArray; out Results:TS7PlusMultiReadResultArray):TProtocolIOResult;
begin
  SetLength(Results, 0);
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.ReadMultipleSymbolic(Items, Results) then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.WriteMultipleSymbolic(const Items:TS7PlusMultiWriteItemArray; out Results:TS7PlusMultiWriteResultArray):TProtocolIOResult;
begin
  SetLength(Results, 0);
  if ReadOnly then begin
    Result := ioReadOnlyProtocol;
    exit;
  end;
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.WriteMultipleSymbolic(Items, Results) then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.WriteSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const Data:TBytes; SoftDataType:Byte):TProtocolIOResult;
begin
  if ReadOnly then begin
    Result := ioReadOnlyProtocol;
    exit;
  end;
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.WriteSymbolic(AccessArea, Lids, Data, 0, SoftDataType) then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.Authenticate(const Password:AnsiString; const Username:AnsiString):TProtocolIOResult;
begin
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := ioCommError;
      exit;
    end;
    if AuthenticateSession(Password, Username) then
      Result := ioOk
    else
      Result := ioAuthError;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.SubscriptionCreate(const Items:TS7PlusMultiReadItemArray; CycleTimeMs:Word;
                                               RouteMode:Byte; UseFixedRelationId:Boolean;
                                               IncludeAttr1055:Boolean; CreditLimitOverride:SmallInt):TProtocolIOResult;
begin
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.SubscriptionCreate(Items, CycleTimeMs, RouteMode, UseFixedRelationId, IncludeAttr1055, CreditLimitOverride) then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.SubscriptionSetCreditLimit(Limit:SmallInt):TProtocolIOResult;
begin
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.SubscriptionSetCreditLimit(Limit) then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.SubscriptionDelete:TProtocolIOResult;
begin
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.SubscriptionDelete then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.SubscriptionWaitNotification(out CreditTick:Byte; out Values:TS7PlusNotificationValueArray):TProtocolIOResult;
var
  Errors:TS7PlusNotificationErrorArray;
begin
  Result := SubscriptionWaitNotification(CreditTick, Values, Errors);
end;

function TS7CommPlusDriver.SubscriptionWaitNotification(out CreditTick:Byte; out Values:TS7PlusNotificationValueArray;
                                                         out Errors:TS7PlusNotificationErrorArray):TProtocolIOResult;
var
  FrameBody:TBytes;
begin
  SetLength(Values, 0);
  SetLength(Errors, 0);
  CreditTick := 0;
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if not FConnection.WaitForFrame(FrameBody) then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.ParseNotification(FrameBody, CreditTick, Values, Errors) then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.UploadBlock(BlockType, BlockNumber:Cardinal; out Data:TBytes):TProtocolIOResult;
begin
  SetLength(Data, 0);
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := FailResult;
      exit;
    end;
    if FConnection.UploadBlock(BlockType, BlockNumber, Data) then
      Result := ioOk
    else
      Result := FailResult;
  finally
    UnlockDriverIO;
  end;
end;

end.
