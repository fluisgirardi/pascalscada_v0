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
  Classes, SysUtils, ProtocolDriver, ProtocolTypes, Tag, CommPort, CommTypes,
  S7PlusTypes, S7PlusConnection, S7PlusTypeInfo, PLCTagNumber, PLCBlock, PLCString;

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
  TS7PlusScanEntry = record
    TagObj:TTag;
    Path:AnsiString;
    Resolved:Boolean;
    AccessArea:Cardinal;
    Lids:TS7PlusLIDArray;
    Data:TBytes;
    LastResult:TProtocolIOResult;
    LastScanTimeStamp:QWord;
    LastResolveAttempt:QWord;
    UpdateRate:LongInt;
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

    FScanList:array of TS7PlusScanEntry;
    FDBCache:array of TS7PlusDBCacheEntry;
    FIAreaVars, FQAreaVars, FMAreaVars, FTimersVars, FCountersVars:TS7PlusVarInfoArray;
    FIAreaBrowsed, FQAreaBrowsed, FMAreaBrowsed, FTimersBrowsed, FCountersBrowsed:Boolean;

    function GetUseTLS:Boolean;
    procedure SetUseTLS(AValue:Boolean);
    function GetExploreDelayMs:Integer;
    procedure SetExploreDelayMs(AValue:Integer);
    function EnsureConnected:Boolean;
    function GetConnected:Boolean;
    function AreaRIDOf(ReadFunction:LongInt; out AreaRID:Cardinal):Boolean;
    procedure ConnDebug(Sender:TObject; const Msg:String);

    //-- Phase 2: symbolic Path resolution (cached) and scan-list lookups.
    function GetCachedDBVars(DBNumber:Cardinal; out Vars:TS7PlusVarInfoArray):Boolean;
    function GetCachedAreaVars(AreaRID, TiRid:Cardinal; const AreaName:String; var Cache:TS7PlusVarInfoArray; var Browsed:Boolean; out Vars:TS7PlusVarInfoArray):Boolean;
    function ResolvePath(const Path:AnsiString; out AccessArea:Cardinal; out Lids:TS7PlusLIDArray):Boolean;
    function TagPathOf(TagObj:TTag; out Path:AnsiString):Boolean;
    function FindScanEntry(const Path:AnsiString; out Index:Integer):Boolean;
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
    //: Reads a variable's raw bytes by its resolved LID path (see BrowseDB/BrowseNativeArea).
    function ReadSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; out Data:TBytes):TProtocolIOResult;
    //: Writes a variable's raw bytes by its resolved LID path.
    function WriteSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const Data:TBytes):TProtocolIOResult;

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
begin
  Result := false;
  if (PCommPort=nil) or (not PCommPort.ReallyActive) then exit;

  if FConnection=nil then begin
    FConnection := TS7PlusConnection.Create(PCommPort, DriverID);
    FConnection.OnDebug := ConnDebug;
    FConnection.UseTLS := FUseTLS;
    FConnection.ExploreDelayMs := FExploreDelayMs;
  end;

  if not FConnection.Connected then
    FConnection.Connect;

  Result := FConnection.Connected;
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

function TS7CommPlusDriver.ResolvePath(const Path:AnsiString; out AccessArea:Cardinal; out Lids:TS7PlusLIDArray):Boolean;
var
  DotPos, DBNumber, i:Integer;
  AreaName:AnsiString;
  Vars:TS7PlusVarInfoArray;
  Found:Boolean;
begin
  Result := false;
  SetLength(Lids, 0);
  AccessArea := 0;

  DotPos := Pos('.', Path);
  if DotPos<=1 then exit;
  AreaName := Copy(Path, 1, DotPos-1);

  Found := false;
  if (Length(AreaName)>2) and SameText(Copy(AreaName,1,2),'DB') and
     TryStrToInt(Copy(AreaName,3,Length(AreaName)), DBNumber) then begin
    if GetCachedDBVars(DBNumber, Vars) then begin
      AccessArea := S7PlusIds_DBAccessAreaBase + Cardinal(DBNumber);
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

  for i:=0 to High(Vars) do
    if Vars[i].Name=Path then begin
      Lids := Vars[i].Lids;
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
var
  i, MostOverdueIdx:Integer;
  Now_, Elapsed, WorstElapsed:Int64;
  Data:TBytes;
  ok:Boolean;
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
      if ResolvePath(FScanList[i].Path, FScanList[i].AccessArea, FScanList[i].Lids) then
        FScanList[i].Resolved := true;
    end;

  //Among the resolved tags, read the single most overdue one this tick (same "read the
  //block that most needs it" pattern TModBusDriver.DoScanRead already uses).
  MostOverdueIdx := -1;
  WorstElapsed := Low(Int64);
  for i:=0 to High(FScanList) do
    if FScanList[i].Resolved then begin
      Elapsed := Int64(Now_-FScanList[i].LastScanTimeStamp) - FScanList[i].UpdateRate;
      if (FScanList[i].LastScanTimeStamp=0) or (Elapsed>WorstElapsed) then begin
        WorstElapsed := Elapsed;
        MostOverdueIdx := i;
      end;
    end;

  if (MostOverdueIdx>=0) and ((FScanList[MostOverdueIdx].LastScanTimeStamp=0) or (WorstElapsed>=0)) then begin
    ok := FConnection.ReadSymbolic(FScanList[MostOverdueIdx].AccessArea, FScanList[MostOverdueIdx].Lids, Data);
    FScanList[MostOverdueIdx].LastScanTimeStamp := GetTickCount64;
    if ok then begin
      FScanList[MostOverdueIdx].Data := Data;
      FScanList[MostOverdueIdx].LastResult := ioOk;
    end else
      FScanList[MostOverdueIdx].LastResult := ioCommError;
  end else
    NeedSleep := 20;
end;

procedure TS7CommPlusDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
var
  Idx, i:Integer;
begin
  SetLength(values.Values,0);
  if not FindScanEntry(TagRec.Path, Idx) then begin
    values.LastQueryResult := ioNullDriver;
    exit;
  end;

  values.ClkMonotonicTStamp := FScanList[Idx].LastScanTimeStamp;
  values.LastQueryResult := FScanList[Idx].LastResult;

  if FScanList[Idx].LastResult=ioOk then begin
    SetLength(values.Values, Length(FScanList[Idx].Data));
    for i:=0 to High(FScanList[Idx].Data) do
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
begin
  SetLength(Values,0);

  if not EnsureConnected then begin
    Result := ioCommError;
    exit;
  end;

  //Symbolic (Path) tags: resolve and read by LID, bypassing the numeric Rack/Slot/DB/
  //Address addressing below entirely. Covers synchronous Tag.Read (DoScanRead/DoGetValue
  //handle the polled/async path separately, with their own resolution cache).
  if tagrec.Path<>'' then begin
    if ResolvePath(tagrec.Path, AccessArea, Lids) and FConnection.ReadSymbolic(AccessArea, Lids, Data) then begin
      SetLength(Values, Length(Data));
      for i:=0 to High(Data) do
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
begin
  if not EnsureConnected then begin
    Result := ioCommError;
    exit;
  end;

  SetLength(Data, Length(Values));
  for i:=0 to High(Values) do
    Data[i] := Trunc(Values[i]) and $FF;

  //Symbolic (Path) tags: resolve and write by LID, bypassing the numeric addressing below.
  if tagrec.Path<>'' then begin
    if ResolvePath(tagrec.Path, AccessArea, Lids) and FConnection.WriteSymbolic(AccessArea, Lids, Data) then
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

  Result := DoRead(tr, Values, true);
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

  Result := DoWrite(tr, Values, true);
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

  Result := DoRead(tr, Values, true);
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

  Result := DoWrite(tr, Values, true);
end;

function TS7CommPlusDriver.BrowseDB(DBNumber:Cardinal; out Vars:TS7PlusVarInfoArray):TProtocolIOResult;
begin
  SetLength(Vars, 0);
  if not EnsureConnected then begin
    Result := ioCommError;
    exit;
  end;
  if FConnection.BrowseDB(DBNumber, Vars) then
    Result := ioOk
  else
    Result := ioCommError;
end;

function TS7CommPlusDriver.BrowseNativeArea(AreaRID, TiRid:Cardinal; const AreaName:String; out Vars:TS7PlusVarInfoArray):TProtocolIOResult;
begin
  SetLength(Vars, 0);
  if not EnsureConnected then begin
    Result := ioCommError;
    exit;
  end;
  if FConnection.BrowseNativeArea(AreaRID, TiRid, AreaName, Vars) then
    Result := ioOk
  else
    Result := ioCommError;
end;

function TS7CommPlusDriver.ReadSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; out Data:TBytes):TProtocolIOResult;
begin
  SetLength(Data, 0);
  if not EnsureConnected then begin
    Result := ioCommError;
    exit;
  end;
  if FConnection.ReadSymbolic(AccessArea, Lids, Data) then
    Result := ioOk
  else
    Result := ioCommError;
end;

function TS7CommPlusDriver.WriteSymbolic(AccessArea:Cardinal; const Lids:TS7PlusLIDArray; const Data:TBytes):TProtocolIOResult;
begin
  if not EnsureConnected then begin
    Result := ioCommError;
    exit;
  end;
  if FConnection.WriteSymbolic(AccessArea, Lids, Data) then
    Result := ioOk
  else
    Result := ioCommError;
end;

end.
