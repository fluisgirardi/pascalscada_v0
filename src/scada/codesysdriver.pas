{$i ../common/language.inc}
{:
  @abstract(CODESYS V3 symbol protocol (TCP 11740) driver for PascalSCADA.)

  This driver talks directly to the proprietary CODESYS V3 symbol service on
  port 11740. It does not need OPC UA, an OPC server or a vendor DLL. Each tag
  carries the full symbol path in LongAddress, e.g.
  @code(Application.PLC_PRG.bBackward).

  The design-time Tag Builder imports a CODESYS Symbolconfiguration XML and
  creates the tags with their paths and IEC types filled in.

  Both packages compile; the
  handshake and Service layers were calibrated against a REAL bidirectional
  capture (MCGS HMI <-> CODESYS Control Win V3 soft PLC). On 2026-11-27 a
  REAL boot application (9 BOOL + 1 INT, Symbol Configuration exported
  ReadWrite) was loaded into the soft PLC.

  Two layers have now been verified end-to-end against that real application:
    - @bold(Protocol client:) tools/codesys_verify.pas read back all 10 real
      IEC values and wrote a BOOL and an INT (read-back confirmed). Key
      correction: variable values are tag 0x1D (one per registered symbol,
      4 byte header + IEC value), NOT tag 0x22 (a status/heartbeat word);
      Write(4) uses 0x13/0x21/0x1E with an 8/12 byte value buffer and
      tag 0x20 == 0 means success.
    - @bold(Driver layer:) tools/codesys_driver_verify.pas drives real
      TPLCTagNumber objects through DoAddTag/DoRead/DoWrite/DoScanRead/
      DoGetValue. That test EXPOSED and led to the fix of three mapping bugs
      in this unit: BOOL (pttDefault) was treated as an 8 byte LREAL, INT
      (pttSmallInt) was mapped to unsigned UINT, and WriteOne sent the wrong
      buffer length / wrote float bit patterns into 64 bit integers. BOOL
      and signed INT reads, writes, read-back and the scan cache now pass.

  @bold(REAL / LREAL verified - 2026-11-27.) A second boot application
  (ProbeSymbols3: 9 BOOL + INT + REAL + LREAL) was generated with the
  CODESYS ScriptEngine and loaded into the soft PLC. The XML wizard maps
  REAL -> pttFloat and LREAL -> pttDouble; TagTypeToIEC maps those to IEC
  $0E/4 bytes and $0F/8 bytes. tools/codesys_driver_verify.pas now covers
  SizeOfTag, DoRead initial values (3.14159 / 2.718281828459045), DoWrite
  (-12.5 / -9876.54321) with byte-exact read-back and the full
  DoScanRead/DoGetValue cache path: 29 PASS / 0 FAIL. On the wire
  tools/codesys_capture/verify_real_lreal.py gives 17 PASS / 0 FAIL.
  A 64 bit LREAL uses a 12 byte 0x1D (4 byte header + 8 byte double) and a
  12 byte Write(4) value buffer. The protocol client now BATCHES Register(1)
  requests (<= 10 names / <= 440 encoded bytes per batch), because this
  runtime resets the connection on larger requests and on BLK fragments.

  Still do not use in production - only anonymous runtimes, one runtime
  version (3.5.14.10) and BOOL/INT/REAL/LREAL have been exercised
  end-to-end; LINT/ULINT/STRING/WSTRING and password-protected runtimes
  remain untested.

  @bold(Anonymous access only:) the runtime must allow access without a
  password; login/challenge-response is not implemented. The protocol is
  private and the tag semantics above are based on a single runtime version
  (3.5.14.10); other versions may need byte-level corrections.
}
unit CodesysDriver;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math, syncobjs, ProtocolDriver, ProtocolTypes, Tag, commtypes,
  PLCTag, PLCTagNumber, PLCBlock, PLCString, CodesysSymbolClient, pascalScadaMTPCPU;

//: Registers the Tag Builder used by the Codesys symbol driver (design package).
procedure SetTagBuilderToolForCodesysDriver(TagBuilderTool:TOpenTagEditor);

type
  {: Gives access to the protected TagType property. }
  TPLCTagAccess = class(TPLCTag);

  {: Cached value of one CODESYS symbol. }
  TCodesysItem = class
  public
    Path:AnsiString;
    Values:TArrayOfDouble;
    TypeCode:Byte;
    Stamp:QWord;
    LastResult:TProtocolIOResult;
    TagCount:LongInt;
    constructor Create(const APath:AnsiString);
  end;

  {: CODESYS V3 symbol protocol driver. }
  TCodesysSymbolDriver = class(TProtocolDriver)
  private
    FClient:TCodesysSymbolClient;
    FItems:TStringList; // Path -> TCodesysItem
    FItemsCS:TCriticalSection;
    FHost:AnsiString;
    FPort:Word;
    FTimeout:LongInt;
    FReconnectInterval:LongInt;
    FNextConnect:QWord;
    FConnectResult:TProtocolIOResult;
    FSymbolDirty:Boolean;
    FLastPoll:QWord;

    function GetHost:AnsiString;
    procedure SetHost(const AValue:AnsiString);
    procedure SetPort(AValue:Word);
    procedure SetTimeout(AValue:LongInt);
    procedure SetReconnectInterval(AValue:LongInt);
    procedure SettingChanged;
    function PathOf(TagObj:TTag; out Path:AnsiString):Boolean;
    function IECOf(TagObj:TTag):Byte;
    function FindItem(const Path:AnsiString):TCodesysItem;
    function GetItem(const Path:AnsiString):TCodesysItem;
    procedure FreeItemIfUnused(Item:TCodesysItem);
    function EnsureConnected(Now_:QWord):Boolean;
    procedure DropConnection(R:TProtocolIOResult);
    procedure LockItems;
    procedure UnlockItems;
    function PollSymbols(const Names:array of AnsiString; Now_:QWord):Boolean;
    function ReadOne(const Path:AnsiString; out Values:TArrayOfDouble):TProtocolIOResult;
    function WriteOne(const Path:AnsiString; const Values:TArrayOfDouble):TProtocolIOResult;
  protected
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); override;
    procedure DoDelTag(TagObj:TTag); override;
    procedure DoScanRead(Sender:TObject; var NeedSleep:LongInt); override;
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); override;
    function DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
    function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function NeedsExternalPort:Boolean; override;
    function SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; override;
    function HasTabBuilderEditor:Boolean; override;
    procedure OpenTagEditor(InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); override;
    {: True while the TCP/channel session is established. }
    function Connected:Boolean;
    {: Last connection/protocol error. }
    function LastError:AnsiString;
  published
    {: PLC host name or IP address. }
    property Host:AnsiString read GetHost write SetHost;
    {: CODESYS symbol service TCP port (default 11740). }
    property Port:Word read FPort write SetPort default 11740;
    {: Socket read/write timeout in milliseconds (default 3000). }
    property Timeout:LongInt read FTimeout write SetTimeout default 3000;
    {: Delay before retrying a failed connection, in ms (default 3000). }
    property ReconnectInterval:LongInt read FReconnectInterval write SetReconnectInterval default 3000;
    property ReadOnly;
  end;

implementation

var
  CodesysDriverTagBuilderEditor:TOpenTagEditor = nil;

procedure SetTagBuilderToolForCodesysDriver(TagBuilderTool:TOpenTagEditor);
begin
  if Assigned(CodesysDriverTagBuilderEditor) then
    raise Exception.Create('A Tag Builder editor for the Codesys driver was already assigned.');
  CodesysDriverTagBuilderEditor := TagBuilderTool;
end;

{ IEC type code and byte width of a PascalSCADA TagType.

  Verified 2026-11-27 end-to-end through the driver (tools/
  codesys_driver_verify.pas) against a real running CODESYS application.
  Two mappings were WRONG before that test:
    * pttDefault had no case, so it fell through to the LREAL default.
      The Codesys XML Tag Builder emits pttDefault for BOOL (see
      src/scada_dsng/codesysxml.pas: 'bool' -> 'pttDefault'), so every
      BOOL tag was treated as an 8 byte LREAL: SizeOfTag returned 8,
      DecodeIEC always produced 0, and WriteOne sent 8 bytes which the
      PLC rejected with status 15. BOOL is 1 byte / IEC $00.
    * pttSmallInt (INT, signed 16 bit) was mapped to $0B (UINT), so a
      negative INT was displayed as 65535-N. It must be IEC $07.
  Keep the type codes in sync with DecodeIEC below and with the case in
  WriteOne; WriteOne derives the write buffer size from the code. }
function TagTypeToIEC(T:TTagType; out Bytes:LongInt):Byte;
begin
  Bytes := 8;
  case T of
    pttDefault:   begin Result := $00; Bytes := 1; end; // BOOL (XML wizard)
    pttShortInt:  begin Result := $06; Bytes := 1; end; // SINT
    pttByte:      begin Result := $02; Bytes := 1; end; // BYTE/USINT
    pttSmallInt:  begin Result := $07; Bytes := 2; end; // INT (signed!)
    pttWord:      begin Result := $0B; Bytes := 2; end; // UINT/WORD
    pttLongInt:   begin Result := $08; Bytes := 4; end; // DINT
    pttDWord:     begin Result := $0C; Bytes := 4; end; // UDINT/DWORD
    pttFloat:     begin Result := $0E; Bytes := 4; end; // REAL
    pttInt64:     begin Result := $09; Bytes := 8; end; // LINT
    pttQWord:     begin Result := $0D; Bytes := 8; end; // ULINT/LWORD
    pttDouble:    begin Result := $0F; Bytes := 8; end; // LREAL
  else
    Result := $0F; Bytes := 8; // unknown: assume LREAL
  end;
end;

{: Converts the raw IEC bytes returned by the PLC into one numeric value. }
function DecodeIEC(const Raw:TBytes; TypeCode:Byte):TArrayOfDouble;
var n:LongInt; U32:LongWord; U16:Word; F:Single; D:Double; I64:Int64; Q:QWord;
begin
  SetLength(Result,1);
  Result[0] := 0;
  n := Length(Raw);
  if n=0 then exit;
  case TypeCode of
    $00,$02,$0A: Result[0] := Raw[0];                     // BOOL, BYTE, USINT
    $06: Result[0] := ShortInt(Raw[0]);                   // SINT
    $03,$0B: Result[0] := CodesysGetU16(Raw,0);           // WORD, UINT
    $07: begin U16 := CodesysGetU16(Raw,0); Result[0] := SmallInt(U16); end; // INT
    $04,$0C: Result[0] := CodesysGetU32(Raw,0);           // DWORD, UDINT
    $08: begin U32 := CodesysGetU32(Raw,0); Result[0] := LongInt(U32); end; // DINT
    $0E: begin if n>=4 then begin Move(Raw[0],F,4); Result[0] := F; end; end; // REAL
    $0F: begin if n>=8 then begin Move(Raw[0],D,8); Result[0] := D; end; end; // LREAL
    $09: begin if n>=8 then begin Move(Raw[0],I64,8); Result[0] := I64; end; end; // LINT
    $0D: begin if n>=8 then begin Move(Raw[0],Q,8); Result[0] := Q; end; end; // ULINT
  else
    Result[0] := Raw[0];
  end;
end;

function EncodeFloat32(V:Single):TBytes;
var U:LongWord;
begin
  U := 0;
  Move(V,U,4);
  Result := CodesysU32(U);
end;

function EncodeDouble(V:Double):TBytes;
var Q:QWord; i:LongInt;
begin
  Q := 0;
  Move(V,Q,8);
  SetLength(Result,8);
  for i:=0 to 7 do Result[i] := Byte((Q shr (i*8)) and $FF);
end;

{ TCodesysItem }

constructor TCodesysItem.Create(const APath:AnsiString);
begin
  inherited Create;
  Path := APath;
  TagCount := 0;
  Stamp := 0;
  TypeCode := $0F;
  LastResult := ioNone;
  SetLength(Values,0);
end;

{ TCodesysSymbolDriver }

constructor TCodesysSymbolDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FItemsCS := TCriticalSection.Create;
  FItems := TStringList.Create;
  FItems.Sorted := true;
  FItems.CaseSensitive := true;
  FItems.Duplicates := dupError;
  FClient := TCodesysSymbolClient.Create;
  FPort := 11740;
  FTimeout := 3000;
  FReconnectInterval := 3000;
  FConnectResult := ioConnectPLCFailed;
  FSymbolDirty := true;
  PReadSomethingAlways := true;
end;

destructor TCodesysSymbolDriver.Destroy;
var i:LongInt;
begin
  // The inherited destructor stops the scan threads and unregisters every
  // still-bound tag through the virtual DoDelTag. That callback locks
  // FItemsCS and looks symbols up in FItems, so both must still be alive
  // while inherited Destroy runs. Freeing them first (the old order) made
  // DoDelTag dereference a freed/nil FItemsCS and raised EAccessViolation
  // whenever the driver was deleted with tags still attached (2026-11-28).
  // inherited first also guarantees the scan threads are gone before the
  // client socket and the item list are released.
  inherited Destroy;

  LockItems;
  try
    for i:=0 to FItems.Count-1 do FItems.Objects[i].Free;
    FItems.Clear;
  finally
    UnlockItems;
  end;
  FreeAndNil(FClient);
  FreeAndNil(FItems);
  FreeAndNil(FItemsCS);
end;

procedure TCodesysSymbolDriver.LockItems;
begin
  if Assigned(FItemsCS) then FItemsCS.Enter;
end;

procedure TCodesysSymbolDriver.UnlockItems;
begin
  if Assigned(FItemsCS) then FItemsCS.Leave;
end;

function TCodesysSymbolDriver.NeedsExternalPort:Boolean;
begin
  Result := false;
end;

function TCodesysSymbolDriver.SizeOfTag(aTag:TTag; isWrite:Boolean;
  var ProtocolTagType:TProtocolTagType):BYTE;
var Bytes:LongInt;
begin
  if aTag is TPLCString then begin
    ProtocolTagType := ptByte;
    exit(8);
  end;
  TagTypeToIEC(TPLCTagAccess(aTag).TagType,Bytes);
  case Bytes of
    1: ProtocolTagType := ptByte;
    2: ProtocolTagType := ptWord;
    4: ProtocolTagType := ptDWord;
  else
    ProtocolTagType := ptDouble;
  end;
  Result := Bytes;
end;

function TCodesysSymbolDriver.HasTabBuilderEditor:Boolean;
begin
  Result := true;
end;

procedure TCodesysSymbolDriver.OpenTagEditor(InsertHook:TAddTagInEditorHook;
  CreateProc:TCreateTagProc);
begin
  if Assigned(CodesysDriverTagBuilderEditor) then
    CodesysDriverTagBuilderEditor(Self,Self.Owner,InsertHook,CreateProc)
  else
    inherited OpenTagEditor(InsertHook,CreateProc);
end;

function TCodesysSymbolDriver.GetHost:AnsiString;
begin
  Result := FHost;
end;

procedure TCodesysSymbolDriver.SetHost(const AValue:AnsiString);
begin
  if FHost=AValue then exit;
  FHost := AValue;
  SettingChanged;
end;

procedure TCodesysSymbolDriver.SetPort(AValue:Word);
begin
  if AValue=0 then AValue := 11740;
  if FPort=AValue then exit;
  FPort := AValue;
  SettingChanged;
end;

procedure TCodesysSymbolDriver.SetTimeout(AValue:LongInt);
begin
  AValue := Max(100,AValue);
  if FTimeout=AValue then exit;
  FTimeout := AValue;
  SettingChanged;
end;

procedure TCodesysSymbolDriver.SetReconnectInterval(AValue:LongInt);
begin
  AValue := Max(100,AValue);
  if FReconnectInterval=AValue then exit;
  FReconnectInterval := AValue;
  SettingChanged;
end;

procedure TCodesysSymbolDriver.SettingChanged;
begin
  FNextConnect := 0;
  if FClient<>nil then begin
    FClient.Host := FHost;
    FClient.Port := FPort;
    FClient.Timeout := FTimeout;
  end;
end;

function TCodesysSymbolDriver.PathOf(TagObj:TTag; out Path:AnsiString):Boolean;
begin
  Result := false;
  Path := '';
  if TagObj is TPLCTagNumber then Path := TPLCTagNumber(TagObj).LongAddress
  else if TagObj is TPLCBlock then Path := TPLCBlock(TagObj).LongAddress
  else if TagObj is TPLCString then Path := TPLCString(TagObj).LongAddress;
  Path := Trim(Path);
  Result := Path<>'';
end;

function TCodesysSymbolDriver.IECOf(TagObj:TTag):Byte;
var Bytes:LongInt;
begin
  if TagObj is TPLCTag then Result := TagTypeToIEC(TPLCTagAccess(TagObj).TagType,Bytes)
  else Result := $0F;
end;

procedure TCodesysSymbolDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
var Path:AnsiString; Item:TCodesysItem;
begin
  inherited DoAddTag(TagObj,TagValid);
  if not PathOf(TagObj,Path) then exit;
  LockItems;
  try
    Item := FindItem(Path);
    if Item=nil then begin
      Item := TCodesysItem.Create(Path);
      FItems.AddObject(String(Path),Item);
    end;
    Inc(Item.TagCount);
    Item.TypeCode := IECOf(TagObj);
  finally
    UnlockItems;
  end;
  FSymbolDirty := true;
end;

procedure TCodesysSymbolDriver.DoDelTag(TagObj:TTag);
var Path:AnsiString; Item:TCodesysItem;
begin
  inherited DoDelTag(TagObj);
  if not PathOf(TagObj,Path) then exit;
  LockItems;
  try
    Item := FindItem(Path);
    if Item=nil then exit;
    Dec(Item.TagCount);
  finally
    UnlockItems;
  end;
  FSymbolDirty := true;
end;

function TCodesysSymbolDriver.FindItem(const Path:AnsiString):TCodesysItem;
var i:LongInt;
begin
  i := FItems.IndexOf(String(Path));
  if i<0 then exit(nil);
  Result := TCodesysItem(FItems.Objects[i]);
end;

function TCodesysSymbolDriver.GetItem(const Path:AnsiString):TCodesysItem;
begin
  Result := FindItem(Path);
  if Result=nil then begin
    Result := TCodesysItem.Create(Path);
    FItems.AddObject(String(Path),Result);
  end;
end;

procedure TCodesysSymbolDriver.FreeItemIfUnused(Item:TCodesysItem);
var i:LongInt;
begin
  if (Item=nil) or (Item.TagCount>0) then exit;
  i := FItems.IndexOf(String(Item.Path));
  if i>=0 then begin
    FItems.Delete(i);
    Item.Free;
  end;
end;

function TCodesysSymbolDriver.Connected:Boolean;
begin
  Result := (FClient<>nil) and FClient.Connected;
end;

function TCodesysSymbolDriver.LastError:AnsiString;
begin
  if FClient=nil then Result := 'Codesys client not created'
  else Result := FClient.LastError;
end;

procedure TCodesysSymbolDriver.DropConnection(R:TProtocolIOResult);
begin
  FClient.Disconnect;
  FConnectResult := R;
  FNextConnect := GetTickCount64 + QWord(FReconnectInterval);
  FSymbolDirty := true;
end;

function TCodesysSymbolDriver.EnsureConnected(Now_:QWord):Boolean;
begin
  Result := false;
  if FClient=nil then exit;
  if FClient.Connected then exit(true);
  if Now_<FNextConnect then exit;
  FClient.Host := FHost;
  FClient.Port := FPort;
  FClient.Timeout := FTimeout;
  if FClient.Connect(FHost,FPort,FTimeout) then begin
    FConnectResult := ioOk;
    FSymbolDirty := true;
    Result := true;
  end else begin
    FConnectResult := ioConnectPLCFailed;
    FNextConnect := GetTickCount64 + QWord(FReconnectInterval);
  end;
end;

function TCodesysSymbolDriver.PollSymbols(const Names:array of AnsiString;
  Now_:QWord):Boolean;
var Results:array of TCodesysVarResult; i:LongInt; Item:TCodesysItem;
begin
  Result := false;
  if Length(Names)=0 then exit(true);
  SetLength(Results,Length(Names));
  if not FClient.ReadVariables(Names,Results) then begin
    DropConnection(ioCommError);
    exit;
  end;
  LockItems;
  try
    for i:=0 to High(Names) do begin
      Item := FindItem(Names[i]);
      if Item=nil then continue;
      // Verified 2026-11-27 against a REAL loaded CODESYS application:
      // OK is set when a 0x1D value tag arrives for this variable (the
      // client maps the N 0x1D tags to Names in request order). 0x20 is
      // an error code (0 = OK, 20 = no data) and is absent on a good read;
      // do not gate success on Status. ReadOne judges by OK alone too.
      if Results[i].OK then begin
        Item.LastResult := ioOk;
        Item.Stamp := Now_;
        Item.Values := DecodeIEC(Results[i].Raw,Item.TypeCode);
      end else begin
        Item.LastResult := ioCommError;
      end;
    end;
  finally
    UnlockItems;
  end;
  FLastPoll := Now_;
  Result := true;
end;

procedure TCodesysSymbolDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
var Now_:QWord; Names:array of AnsiString; i,n:LongInt; Item:TCodesysItem;
begin
  try
    if [csDestroying]*ComponentState<>[] then begin
      CrossThreadSwitch;
      NeedSleep := 20;
      exit;
    end;
    NeedSleep := 10;
    LockItems;
    try
      SetLength(Names,0);
      n := 0;
      for i:=0 to FItems.Count-1 do begin
        Item := TCodesysItem(FItems.Objects[i]);
        if Item.TagCount>0 then begin
          SetLength(Names,n+1);
          Names[n] := Item.Path;
          Inc(n);
        end;
      end;
    finally
      UnlockItems;
    end;
    if Length(Names)=0 then begin
      NeedSleep := 20;
      exit;
    end;
    Now_ := GetTickCount64;
    if not EnsureConnected(Now_) then begin
      NeedSleep := 20;
      exit;
    end;
    PollSymbols(Names,Now_);
  except
    on E:Exception do NeedSleep := 20;
  end;
  FProtocolReady := true;
end;

procedure TCodesysSymbolDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
var Item:TCodesysItem; n,i:LongInt;
begin
  SetLength(values.Values,0);
  Item := FindItem(TagRec.Path);
  if Item=nil then begin
    values.LastQueryResult := ioNullDriver;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
    exit;
  end;
  values.ClkMonotonicTStamp := Item.Stamp;
  values.Offset := 0;
  values.RealOffset := 0;
  if Item.LastResult<>ioOk then begin
    values.LastQueryResult := Item.LastResult;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
    exit;
  end;
  n := TagRec.Size;
  if n<=0 then n := Length(Item.Values);
  SetLength(values.Values,n);
  for i:=0 to n-1 do
    if i<Length(Item.Values) then values.Values[i] := Item.Values[i]
    else values.Values[i] := 0;
  values.LastQueryResult := ioOk;
  values.ReadsOK := 1;
  values.ReadFaults := 0;
end;

function TCodesysSymbolDriver.ReadOne(const Path:AnsiString;
  out Values:TArrayOfDouble):TProtocolIOResult;
var Names:array of AnsiString; Results:array of TCodesysVarResult;
    Item:TCodesysItem; Now_:QWord;
begin
  SetLength(Values,0);
  if not EnsureConnected(GetTickCount64) then exit(FConnectResult);
  SetLength(Names,1);
  Names[0] := Path;
  SetLength(Results,1);
  if not FClient.ReadVariables(Names,Results) then begin
    DropConnection(ioCommError);
    exit(ioCommError);
  end;
  if not Results[0].OK then exit(ioCommError);
  Item := GetItem(Path);
  Now_ := GetTickCount64;
  Item.LastResult := ioOk;
  Item.Stamp := Now_;
  Item.Values := DecodeIEC(Results[0].Raw,Item.TypeCode);
  Values := Copy(Item.Values);
  Result := ioOk;
end;

function TCodesysSymbolDriver.WriteOne(const Path:AnsiString;
  const Values:TArrayOfDouble):TProtocolIOResult;
var Data:TBytes; Item:TCodesysItem; i,n:LongInt; V:Double; F:Single; Q:QWord;
begin
  if not EnsureConnected(GetTickCount64) then exit(FConnectResult);
  if Length(Values)=0 then exit(ioIllegalValue);
  Item := GetItem(Path);
  // The write buffer length MUST equal the IEC type size: the client puts
  // Length(Data) into tag 0x1E as the u16 type size, and the PLC rejects
  // a mismatch with status 15. Verified 2026-11-27 end-to-end.
  n := 8;
  case Item.TypeCode of
    $00,$02,$06,$0A: n := 1;   // BOOL, BYTE/USINT, SINT
    $07,$03,$0B:     n := 2;   // INT, WORD/UINT
    $08,$04,$0C,$0E: n := 4;   // DINT, DWORD/UDINT, REAL
  else n := 8;                 // LINT, ULINT, LREAL
  end;
  SetLength(Data,n);
  for i:=0 to n-1 do Data[i] := 0;
  V := Values[0];
  case n of
    1: Data[0] := Byte(Round(V) and $FF);
    2: begin
         Data[0] := Byte(Round(V) and $FF);
         Data[1] := Byte((Round(V) shr 8) and $FF);
       end;
    4: begin
         if Item.TypeCode=$0E then begin
           F := V;
           Data := EncodeFloat32(F);
         end else begin
           Data := CodesysU32(LongWord(Round(V)));
         end;
       end;
  else
    if Item.TypeCode=$0F then Data := EncodeDouble(V)
    else begin
      // LINT / ULINT: little-endian 64 bit integer. The old code used
      // EncodeDouble here, writing a FLOAT bit pattern into an INT64.
      Q := QWord(Int64(Round(V)));
      SetLength(Data,8);
      for i:=0 to 7 do Data[i] := Byte((Q shr (i*8)) and $FF);
    end;
  end;
  if FClient.WriteVariable(Path,Data) then begin
    Item.Stamp := GetTickCount64;
    exit(ioOk);
  end;
  DropConnection(ioCommError);
  Result := ioCommError;
end;

function TCodesysSymbolDriver.DoRead(const tagrec:TTagRec;
  out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  Result := ReadOne(tagrec.Path,Values);
end;

function TCodesysSymbolDriver.DoWrite(const tagrec:TTagRec;
  const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  Result := WriteOne(tagrec.Path,Values);
end;

initialization
  // nothing
end.
