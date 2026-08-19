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
  Classes, SysUtils, pascalScadaMTPCPU, ProtocolDriver, ProtocolTypes, Tag, CommPort,
  CommTypes, S7PlusTypes, S7PlusConnection, S7PlusTypeInfo, PLCTagNumber, PLCBlock,
  PLCString;

const
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
  end;
  TS7PlusCompositeItemArray = array of TS7PlusCompositeItem;

  TS7PlusScanEntry = record
    TagObj:TTag;
    Path:AnsiString;
    Resolved:Boolean;
    AccessArea:Cardinal;
    Lids:TS7PlusLIDArray;
    ByteOffset:Integer;   //cumulative, from resolution - see TS7PlusVarInfo.ByteOffset. -1=unknown.
    SoftDataType:Byte;
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
  end;

  //-- Cache of a DB's or native area's flat variable list (Name/Lids/SoftDataType), so
  //-- resolving many tags on the same DB/area doesn't re-EXPLORE for every single tag.
  TS7PlusDBCacheEntry = record
    DBNumber:Cardinal;
    Vars:TS7PlusVarInfoArray;
  end;

  { TS7CommPlusDriver }

  TS7CommPlusDriver = class(TProtocolDriver)
  private
    FConnection:TS7PlusConnection;
    FOnDebug:TS7PlusDebugEvent;
    FUseTLS:Boolean;
    FExploreDelayMs:Integer;
    FUsername, FPassword:AnsiString;

    FScanList:array of TS7PlusScanEntry;
    FDBCache:array of TS7PlusDBCacheEntry;
    FIAreaVars, FQAreaVars, FMAreaVars, FTimersVars, FCountersVars:TS7PlusVarInfoArray;
    FIAreaBrowsed, FQAreaBrowsed, FMAreaBrowsed, FTimersBrowsed, FCountersBrowsed:Boolean;
    FDBNameCache:TS7PlusDataBlockInfoArray;
    FDBNameCacheLoaded:Boolean;

    function GetUseTLS:Boolean;
    procedure SetUseTLS(AValue:Boolean);
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
    function ResolvePath(const Path:AnsiString; out AccessArea:Cardinal; out Lids:TS7PlusLIDArray; out ByteOffset:Integer; out SoftDataType:Byte; out CompositeItems:TS7PlusCompositeItemArray):Boolean;
    function TagPathOf(TagObj:TTag; out Path:AnsiString):Boolean;
    function FindScanEntry(const Path:AnsiString; out Index:Integer):Boolean;
    function IsLidsPrefix(const Prefix, Full:TS7PlusLIDArray):Boolean;
    procedure TryDeduplicateEntry(EntryIdx:Integer);
    function ReadComposite(const Items:TS7PlusCompositeItemArray; out Data:TBytes):Boolean;
  protected
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    procedure DoDelTag(TagObj:TTag); override;

    procedure PortDisconnected(Sender:TObject); override;
    procedure PortClosed(Sender:TObject); override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    function SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;

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
    //: Reads a variable's raw bytes by its resolved LID path (see BrowseDB/BrowseNativeArea).
    function ReadSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; out Data:TBytes):TProtocolIOResult;
    //: Writes a variable's raw bytes by its resolved LID path.
    //: SoftDataType (from BrowseDB) is required for the PLC to accept the write - it's used
    //: to encode Data as the target's own typed PValue (ValueDInt, ValueReal, ...) rather
    //: than a generic byte array, which the PLC rejects outright. Default 0 falls back to a
    //: byte array (only correct for genuine byte-array-typed targets).
    function WriteSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const Data:TBytes; SoftDataType:Byte=0):TProtocolIOResult;
    //: Reads several symbolic items in one round-trip - see TS7PlusConnection.ReadMultipleSymbolic.
    function ReadMultipleSymbolic(const Items:TS7PlusMultiReadItemArray; out Results:TS7PlusMultiReadResultArray):TProtocolIOResult;
    //: Performs PLC password authentication (legitimation) - see TS7PlusConnection.Authenticate.
    //: Needed by some PLCs before writes are accepted (reads can remain open/unauthenticated).
    function Authenticate(const Password:AnsiString; const Username:AnsiString=''):TProtocolIOResult;

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
    //: PLC password for legitimation (V2+ with TLS) - some PLCs (protection level "Full
    //: access with password" in TIA Portal) reject writes from an unauthenticated session
    //: even though reads stay open. When set (non-empty), the driver calls Authenticate
    //: automatically right after each new connection is established (see EnsureConnected).
    //: Leave empty to skip legitimation entirely (the default - matches every PLC that
    //: doesn't require it). Currently only the challenge-retrieval step of legitimation is
    //: confirmed working against real hardware; the PLC still rejects the encrypted
    //: response as of this writing - see TS7PlusConnection.Authenticate.
    property Password:AnsiString read FPassword write FPassword;
    //: Optional username for "new"-style (AES-256-CBC) legitimation. Leave empty for a
    //: plain password login (the common case) - see Password.
    property Username:AnsiString read FUsername write FUsername;
  end;

implementation

{ TS7CommPlusDriver }

constructor TS7CommPlusDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FUseTLS := true;
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
    FConnection.ExploreDelayMs := FExploreDelayMs;
  end;

  WasConnected := FConnection.Connected;
  if not WasConnected then
    FConnection.Connect;

  Result := FConnection.Connected;

  //Authenticate once per freshly-established connection (not on every EnsureConnected call -
  //this runs on every scan tick even with no tags due). Password='' (default) means the PLC
  //doesn't need legitimation - skip entirely, matching every PLC that doesn't require it.
  if Result and (not WasConnected) and (FPassword<>'') then begin
    if not FConnection.Authenticate(FPassword, FUsername) then
      ConnDebug(Self, 'EnsureConnected: falha na legitimacao (Username/Password) - writes podem continuar bloqueados pelo CLP.');
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

procedure TS7CommPlusDriver.PortDisconnected(Sender:TObject);
begin
  if FConnection<>nil then
    FConnection.Disconnect;
  inherited PortDisconnected(Sender);
end;

procedure TS7CommPlusDriver.PortClosed(Sender:TObject);
begin
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
  FDBCache[idx].Vars := Vars;
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

function TS7CommPlusDriver.ResolvePath(const Path:AnsiString; out AccessArea:Cardinal; out Lids:TS7PlusLIDArray; out ByteOffset:Integer; out SoftDataType:Byte; out CompositeItems:TS7PlusCompositeItemArray):Boolean;
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
  SetLength(CompositeItems, 0);

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

      CIdx := Length(CompositeItems);
      SetLength(CompositeItems, CIdx+1);
      CompositeItems[CIdx].AccessArea := AccessArea;
      CompositeItems[CIdx].Lids := Vars[i].Lids;
      CompositeItems[CIdx].ByteOffset := Vars[i].ByteOffset;
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

  for i:=0 to High(Vars) do
    if Vars[i].Name=CanonicalPath then begin
      Lids := Vars[i].Lids;
      ByteOffset := Vars[i].ByteOffset;
      SoftDataType := Vars[i].SoftDataType;
      Result := true;
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

  CanBeDerived := not (TS7PlusSoftDataType(FScanList[EntryIdx].SoftDataType) in [sdtSTRING, sdtWSTRING]);

  if CanBeDerived then begin
    BestContainerIdx := -1;
    BestLen := -1;
    for i:=0 to High(FScanList) do begin
      if i=EntryIdx then continue;
      if FScanList[i].IsDerived or (not FScanList[i].Resolved) then continue;
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
    if FScanList[i].IsDerived or (not FScanList[i].Resolved) then continue;
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

procedure TS7CommPlusDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
const
  //Caps how many due tags get batched into a single GetMultiVariables request. Bounds
  //worst-case request/response size if a large burst of tags all come due at once (e.g.
  //right after connecting); anything past the cap is still overdue and gets swept up on
  //the very next tick(s), so this only paces catch-up, it never drops a tag. A composite
  //("whole DB") entry counts as just one against this cap even though it expands into many
  //wire items below - it's still a single logical tag coming due.
  MaxBatchSize = 50;
type
  //-- Which scan-list entry a given batched wire item belongs to, and (for a composite
  //-- entry's own members) where in that entry's assembled Data its bytes get spliced.
  TBatchOwner = record
    EntryIdx:Integer;
    IsCompositeItem:Boolean;
    CompByteOffset:Integer;
  end;
var
  i, j, EntryIdx, ItemIdx, b:Integer;
  Now_, Elapsed:Int64;
  DueIndices:array of Integer;
  Items:TS7PlusMultiReadItemArray;
  Results:TS7PlusMultiReadResultArray;
  Owners:array of TBatchOwner;
  ok:Boolean;
  NeedLen:Integer;
begin
  NeedSleep := 0;

  if not EnsureConnected then begin
    NeedSleep := 100;
    exit;
  end;

  Now_ := GetTickCount64;

  //Resolve any pending tags (throttled - don't hammer EXPLORE every tick for a bad Path).
  for i:=0 to High(FScanList) do
    if (not FScanList[i].Resolved) and (Now_-FScanList[i].LastResolveAttempt>=5000) then begin
      FScanList[i].LastResolveAttempt := Now_;
      if ResolvePath(FScanList[i].Path, FScanList[i].AccessArea, FScanList[i].Lids, FScanList[i].ByteOffset, FScanList[i].SoftDataType, FScanList[i].CompositeItems) then begin
        FScanList[i].Resolved := true;
        FScanList[i].IsComposite := Length(FScanList[i].CompositeItems)>0;
        TryDeduplicateEntry(i);
      end;
    end;

  //Collect every resolved, independently-scanned tag (derived ones piggyback on their
  //container's read - see TryDeduplicateEntry - and are never picked here) that's
  //currently due, and read all of them in a single batched GetMultiVariables request -
  //see TS7PlusConnection.ReadMultipleSymbolic. Items may span different DBs/areas freely.
  SetLength(DueIndices, 0);
  for i:=0 to High(FScanList) do
    if FScanList[i].Resolved and (not FScanList[i].IsDerived) then begin
      Elapsed := Int64(Now_-FScanList[i].LastScanTimeStamp) - FScanList[i].UpdateRate;
      if (FScanList[i].LastScanTimeStamp=0) or (Elapsed>=0) then begin
        SetLength(DueIndices, Length(DueIndices)+1);
        DueIndices[High(DueIndices)] := i;
        if Length(DueIndices)>=MaxBatchSize then break;
      end;
    end;

  if Length(DueIndices)=0 then begin
    NeedSleep := 20;
    exit;
  end;

  //Expand each due entry into one or more wire items - a plain entry contributes exactly
  //one item; a composite ("whole DB") entry contributes one item per top-level member, all
  //still folded into this same round's single GetMultiVariables request.
  SetLength(Items, 0);
  SetLength(Owners, 0);
  for i:=0 to High(DueIndices) do begin
    EntryIdx := DueIndices[i];
    if FScanList[EntryIdx].IsComposite then begin
      for j:=0 to High(FScanList[EntryIdx].CompositeItems) do begin
        ItemIdx := Length(Items);
        SetLength(Items, ItemIdx+1);
        SetLength(Owners, ItemIdx+1);
        Items[ItemIdx].AccessArea := FScanList[EntryIdx].CompositeItems[j].AccessArea;
        Items[ItemIdx].Lids := FScanList[EntryIdx].CompositeItems[j].Lids;
        Owners[ItemIdx].EntryIdx := EntryIdx;
        Owners[ItemIdx].IsCompositeItem := true;
        Owners[ItemIdx].CompByteOffset := FScanList[EntryIdx].CompositeItems[j].ByteOffset;
      end;
    end else begin
      ItemIdx := Length(Items);
      SetLength(Items, ItemIdx+1);
      SetLength(Owners, ItemIdx+1);
      Items[ItemIdx].AccessArea := FScanList[EntryIdx].AccessArea;
      Items[ItemIdx].Lids := FScanList[EntryIdx].Lids;
      Owners[ItemIdx].EntryIdx := EntryIdx;
      Owners[ItemIdx].IsCompositeItem := false;
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

  if not ok then begin
    for i:=0 to High(DueIndices) do
      FScanList[DueIndices[i]].LastResult := ioCommError;
    exit;
  end;

  for i:=0 to High(Items) do begin
    EntryIdx := Owners[i].EntryIdx;
    if (i>High(Results)) or (not Results[i].Ok) then begin
      FScanList[EntryIdx].LastResult := ioCommError;
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
  Data:TBytes;
  ok:Boolean;
  i:Integer;
  AreaRID:Cardinal;
  AccessArea:Cardinal;
  Lids:TS7PlusLIDArray;
  DummyOffset:Integer;
  DummyType:Byte;
  CompositeItems:TS7PlusCompositeItemArray;
  Len:Integer;
begin
  SetLength(Values,0);

  if not EnsureConnected then begin
    Result := ioCommError;
    exit;
  end;

  //Symbolic (Path) tags: resolve and read by LID, bypassing the numeric Rack/Slot/DB/
  //Address addressing below entirely. Covers synchronous Tag.Read (DoScanRead/DoGetValue
  //handle the polled/async path separately, with their own resolution cache). A bare-root
  //("whole DB/area") Path resolves with a non-empty CompositeItems - read via a batched
  //multi-item request instead of the (otherwise meaningless) empty-LID ReadSymbolic.
  if tagrec.Path<>'' then begin
    if not ResolvePath(tagrec.Path, AccessArea, Lids, DummyOffset, DummyType, CompositeItems) then begin
      Result := ioCommError;
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
      Result := ioCommError;
    exit;
  end;

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
    Result := ioCommError;
end;

function TS7CommPlusDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
var
  Data:TBytes;
  ok:Boolean;
  i:Integer;
  AreaRID:Cardinal;
  AccessArea:Cardinal;
  Lids:TS7PlusLIDArray;
  DummyOffset:Integer;
  WriteSoftDataType:Byte;
  DummyComposite:TS7PlusCompositeItemArray;
begin
  if not EnsureConnected then begin
    Result := ioCommError;
    exit;
  end;

  SetLength(Data, Length(Values));
  for i:=0 to High(Values) do
    Data[i] := Trunc(Values[i]) and $FF;

  //Symbolic (Path) tags: resolve and write by LID, bypassing the numeric addressing below.
  //Writing a bare-root ("whole DB/area") Path isn't supported - ResolvePath still returns
  //the root's own empty LID path in that case, so this just writes to the root object as
  //before (the same no-op-ish metadata target it always was for a bare Path). The resolved
  //SoftDataType is required so WriteSymbolic can encode the value as the target's own typed
  //PValue (ValueDInt, ValueReal, ...) - a generic byte-array/blob value is rejected by the
  //PLC outright, confirmed against real hardware.
  if tagrec.Path<>'' then begin
    if ResolvePath(tagrec.Path, AccessArea, Lids, DummyOffset, WriteSoftDataType, DummyComposite) and
       FConnection.WriteSymbolic(AccessArea, Lids, Data, 0, WriteSoftDataType) then
      Result := ioOk
    else
      Result := ioCommError;
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
    Result := ioCommError;
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
      Result := ioCommError;
      exit;
    end;
    if FConnection.BrowseDB(DBNumber, Vars) then
      Result := ioOk
    else
      Result := ioCommError;
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
      Result := ioCommError;
      exit;
    end;
    if FConnection.BrowseNativeArea(AreaRID, TiRid, AreaName, Vars) then
      Result := ioOk
    else
      Result := ioCommError;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.ListDataBlocks(out Blocks:TS7PlusDataBlockInfoArray):TProtocolIOResult;
begin
  SetLength(Blocks, 0);
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := ioCommError;
      exit;
    end;
    if FConnection.ListDataBlocks(Blocks) then
      Result := ioOk
    else
      Result := ioCommError;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.ReadSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; out Data:TBytes):TProtocolIOResult;
begin
  SetLength(Data, 0);
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := ioCommError;
      exit;
    end;
    if FConnection.ReadSymbolic(AccessArea, Lids, Data) then
      Result := ioOk
    else
      Result := ioCommError;
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
      Result := ioCommError;
      exit;
    end;
    if FConnection.ReadMultipleSymbolic(Items, Results) then
      Result := ioOk
    else
      Result := ioCommError;
  finally
    UnlockDriverIO;
  end;
end;

function TS7CommPlusDriver.WriteSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const Data:TBytes; SoftDataType:Byte):TProtocolIOResult;
begin
  LockDriverIO;
  try
    if not EnsureConnected then begin
      Result := ioCommError;
      exit;
    end;
    if FConnection.WriteSymbolic(AccessArea, Lids, Data, 0, SoftDataType) then
      Result := ioOk
    else
      Result := ioCommError;
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
    if FConnection.Authenticate(Password, Username) then
      Result := ioOk
    else
      Result := ioCommError;
  finally
    UnlockDriverIO;
  end;
end;

end.
