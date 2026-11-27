{$i ../common/language.inc}
{:
  @abstract(Minimal CODESYS V3 symbol protocol (TCP 11740) client used by
            TCodesysSymbolDriver.)

  This is a self-contained implementation of the proprietary CODESYS V3
  "symbol" protocol. It does not use OPC UA, an OPC server, or any vendor DLL.

  Wire format (Microsoft CoDe16 Wireshark dissector + a REAL packet capture
  of the Kunlun-Tongtai MCGS HMI client on 2026-11-27):

    Block driver : 00 01 17 E8 + TOTAL frame length (u32 LE, includes these
                   8 header bytes). E.g. 24 00 00 00 = 8 + 28 datagram bytes.
    Datagram     : C5 6B 40 <service> <msgid> 43 <receiver 6B> <sender 8B>
                   6B = hop count 13, header length 3 (=> 6 byte header)
                   40 = priority normal, full addressing
                   43 = receiver length 3*2, sender length 4*2
                   receiver = 2 byte port (network order) + 4 byte IPv4
                   channel payload starts at offset 20
    Channel      : 0xC3 OpenChannel request / 0x83 response, 0x01 BLK
                   (0x81 first) + CRC32, 0x02 ACK, 0x03 KeepAlive, 0x84 error,
                   0xC4 CloseChannel
    Service      : 0xCD55, header size, cmd group, subcmd, session id,
                   content size, additional data, then TLV tags
    Tags         : 7-bit varint id + 7-bit varint size + data;
                   id >= 0x80 means a parent tag (children follow in data)

  Both the handshake layers AND the Service layer have now been calibrated
  against a REAL bidirectional capture (MCGS HMI <-> CODESYS Control Win V3
  soft PLC, 2026-11-27, 108 client frames + 119 server frames via a local
  mitm on TCP 11740 -> 11741). Verified from that capture:
    - Block: 00 01 17 E8 + total length; Channel/Datagram/OpenChannel framing,
      CRC32 = crc32(service payload) for BLK first blocks, and crc32 over the
      whole OpenChannel request with its CRC field zeroed (0xE326BA42).
    - OpenChannel RESPONSE carries the real channel id at offset 14 (the HMI
      uses it in every following BLK/ACK/KeepAlive frame).
    - BLK first block: total@12, crc@16, service payload@20.
    - Service header (20 bytes): 0xCD55, hdrsize=16, group (bit 0x80 set on
      responses), subcmd, session id, content size, additional data, TLVs.
    - Session flow: CmpDevice(1) subcmd=1 device info -> subcmd=2 anonymous
      login (fixed nonce blob) -> tag 0x21 in the parent 0x82 response gives
      the session id used by every later request.
    - CmpIecVarAccess(9): GetHandle(7) -> tag 0x14 handle on a runtime with
      NO application; Register(1) -> tags 0x13, 0x00, 0x00, 0x14 (or 0x3A),
      0x18, 0x19(name); response tag 0x10 is the registration handle;
      Read(3) -> only tag 0x10.
    - 0x19 name encoding = u16(L+1) + name + #0 + pad to align4(L+1).
    - channel type 0x80 is a 162 byte router/name-service announcement with
      UTF-16LE "CODESYS Control Win V3 x64" and "3S - Smart Software
      Solutions GmbH"; it is skipped during the handshake.

  @bold(VERIFIED against a REAL running application - 2026-11-27.)
  A boot application (tools/codesys_project/ProbeSymbols2, 9 BOOL + 1 INT,
  Symbol Configuration exported ReadWrite) was loaded into CODESYS Control
  Win V3 and probed directly with tools/codesys_dbg*.pas. This corrected
  several earlier conclusions that had been drawn from an EMPTY runtime
  (no application loaded, so no symbol had a real value):

    - GetHandle(7) on a runtime WITH an application does NOT return 0x14.
      It returns the browse root: tag 0x30 = application name,
      0x38 = 20 byte browse blob, 0x3A = u32 session handle. BOTH 0x14
      (constant 0x0018 from the HMI capture) and the 0x3A handle work as
      the Register(1) handle; 0x38 does NOT (it closes the channel).
      This client prefers 0x14, falls back to 0x3A, and remembers which
      tag id to send in RegisterVars.
    - Register(1) with N names + ONE Read(3) returns N tags @bold(0x1D),
      one per variable IN REQUEST ORDER. Each is 8 bytes: a 4 byte header
      (00 00 00 00) followed by the IEC value in the low bytes
      (BOOL 01/00, INT 39 30 for 12345, etc). @bold(0x22 is NOT the value):
      it is a fixed status/heartbeat tag (58 00 01 00 + u32 tick counter).
      The old "one 0x22 per read" conclusion was an artifact of an empty
      runtime where no 0x1D values existed at all.
    - @bold(0x20 is an ERROR code, not a status word): 0 = success,
      20 (0x14) = no data / invalid name, 15 (0x0F) = bad value layout.
      It is ABSENT on a successful read that returns 0x1D values.
    - Write(4) request, verified to really change the variable:
        0x13 = 00 00 00 00
        0x21 = ASCII name + #0 + zero pad to align4(L+1) (NO u16 prefix)
        0x1E = [u16 IEC size][00 00][value buffer]
               value buffer = 8 bytes for types <= 4 bytes,
                              12 bytes for 64 bit types (not yet verified),
               IEC value in the LOW bytes, remainder zero.
      Success = response tag 0x20 == 0. BOOL TRUE -> 1,0,0,0,0,0,0,0;
      INT 54321 -> 31,D4,0,0,0,0,0,0. Read-back confirmed both.

  @bold(REAL / LREAL + Register batching - 2026-11-27.)
  A second boot application (ProbeSymbols3, 9 BOOL + INT + REAL + LREAL,
  all ReadWrite in the Symbol Configuration) was generated and loaded into
  the same runtime. tools/codesys_verify.pas (12 symbols),
  tools/codesys_driver_verify.pas (full TCodesysSymbolDriver layer) and
  tools/codesys_capture/verify_real_lreal.py (byte-level capture replay)
  established:

    - REAL (IEC $0E) and LREAL (IEC $0F) read/write work end to end.
      REAL 3.14159 = D0 0F 49 40, LREAL 2.718281828459045 =
      69 57 14 8B 0A BF 05 40. Writing -12.5 (00 00 48 C1) and
      -9876.54321 (6E C0 E7 87 45 4A C3 C0) really changes the PLC and
      reads back byte-identically. Driver layer: 29 PASS / 0 FAIL.
    - A 64 bit LREAL 0x1D is @bold(12 bytes), not 8: 4 byte header
      (00 00 00 00) + the 8 byte double. <=32 bit types use 8 bytes.
    - Write(4) 0x1E uses a @bold(12 byte value buffer) for 64 bit types
      (verified working, previously marked "not yet verified").
    - @bold(The runtime REJECTS large Register(1) requests.) 10 symbols /
      470 encoded bytes are accepted, 11 symbols / ~512 bytes make the PLC
      close the TCP connection (WSAECONNRESET 10054). BLK continuation
      frames (the SendAppBlock fragment path) are ALSO rejected. Therefore
      ReadVariables splits the caller's name list into batches by encoded
      payload size (<= 440 bytes) and by count (<= 10 names), and does one
      Register(1) + Read(3) per batch. 0x1D[n] maps to
      Results[Start + FRegMap[n]], so an unknown symbol interleaved in a
      batch can never shift a value onto the wrong tag.

  This client supports anonymous/no-password runtimes only (no
  challenge-response / RSA / scrypt login). Do not use in production;
  keep the test-stage tag until wider type/PLC coverage is confirmed.
}
unit CodesysSymbolClient;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math, SyncObjs
  {$IFDEF FPC}
  , ssockets
  {$ENDIF}
  ;

type
  {: A decoded TLV tag. Parent tags are flattened: ChildrenOfParent is the
     id of the enclosing tag, 0 for top level tags. }
  TCodesysTag = record
    Id: QWord;
    Parent: QWord;
    Data: TBytes;
  end;
  TCodesysTags = array of TCodesysTag;

  {: Raw result of one symbol read. Status is the u16 value of tag 0x20 when
     the PLC sent one; 0 = OK, 20 = no data / unknown symbol, 15 = bad write
     layout. A successful read carrying 0x1D values normally sends NO 0x20
     at all, so an absent tag means Status = 0. OK is true when a 0x1D tag
     was received for this variable. Raw is the 0x1D payload with its 4 byte
     header stripped, i.e. the real IEC value bytes (verified against a
     running CODESYS application, see the unit header). }
  TCodesysVarResult = record
    Raw: TBytes;
    Status: LongWord;
    OK: Boolean;
  end;

  {: Self-contained CODESYS V3 symbol protocol client. One instance owns one
     TCP connection and one channel; all operations are serialized by a
     critical section. }
  TCodesysSymbolClient = class
  private
    FCS:TCriticalSection;
    {$IFDEF FPC}
    FSocket:TInetSocket;
    {$ENDIF}
    FHost:AnsiString;
    FPort:Word;
    FTimeout:LongInt;
    FChannelID:Word;
    FMsgID:Byte;
    FBlkID:LongWord;
    FAckID:LongWord;
    FVarHandle:TBytes;   // CmpIecVarAccess handle for Register(1)
    FVarHandleTag:Byte;  // $14 (no app) or $3A (app loaded) - see GetVarHandles
    FSessionID:LongWord;
    FRegKey:AnsiString;
    FRegHandle:TBytes;
    { Request index of every SUCCESSFUL registration (0x1B), in response
      order. Read(3) returns one 0x1D per successful registration, so
      ReadVariables maps 0x1D[n] to Results[FRegMap[n]] instead of blindly
      to Results[n]. Empty => legacy sequential mapping. See RegisterVars. }
    FRegMap:array of LongInt;
    FOpened:Boolean;
    FLogged:Boolean;
    FLastError:AnsiString;
    FLastRecv:QWord;

    procedure Fail(const Msg:AnsiString);
    function ReadExact(out Buf:TBytes; Count:LongInt; TimeoutMs:LongInt):Boolean;
    function SendRaw(const Data:TBytes):Boolean;
    function SendDatagram(ServiceID:Byte; const Payload:TBytes):Boolean;
    function ReceiveFrame(out ServiceID:Byte; out Payload:TBytes;
      out IsService:Boolean):Boolean;
    function SendAppBlock(const Payload:TBytes):Boolean;
    function SendAck(BlkID:LongWord):Boolean;
    function SendKeepAlive:Boolean;
    function ServiceCall(Group, SubCmd:Word; const Tags:TCodesysTags;
      out ResponseTags:TCodesysTags):Boolean;
    function DeviceLogin:Boolean;
    function GetVarHandles:Boolean;
    function RegisterVars(const Names:array of AnsiString):Boolean;
    procedure CloseSocket;
  public
    constructor Create;
    destructor Destroy; override;

    {: Opens TCP, opens a CODESYS channel and obtains the anonymous var handles. }
    function Connect(const AHost:AnsiString; APort:Word; TimeoutMs:LongInt):Boolean;
    {: Closes the channel and the TCP socket. }
    procedure Disconnect;
    {: True while the channel is open. }
    function Connected:Boolean;

    {: Reads several symbol variables with one register + one read request.
       Results[i] corresponds to Names[i]. }
    function ReadVariables(const Names:array of AnsiString;
      var Results:array of TCodesysVarResult):Boolean;
    {: Writes one symbol variable. Data contains the encoded IEC value. }
    function WriteVariable(const Name:AnsiString; const Data:TBytes):Boolean;

    property Host:AnsiString read FHost write FHost;
    property Port:Word read FPort write FPort;
    property Timeout:LongInt read FTimeout write FTimeout;
    property LastError:AnsiString read FLastError;
  end;

{: CRC32 (reflected IEEE polynomial) used by the CODESYS channel layer. }
function CodesysCRC32(const Data:TBytes; Count:LongInt):LongWord;
{: Encodes an unsigned integer as CODESYS 7-bit varint. }
procedure CodesysPutVarInt(var B:TBytes; Value:QWord);
{: Appends a TLV tag. }
procedure CodesysPutTag(var B:TBytes; Id:QWord; const Data:TBytes);
{: Decodes a flat list of TLV tags (parent tags are flattened). }
function CodesysParseTags(const B:TBytes; out Tags:TCodesysTags):Boolean;
{: Finds the first decoded tag with the given id. }
function CodesysFindTag(const Tags:TCodesysTags; Id:QWord; out Data:TBytes):Boolean;
{: Little endian helpers. }
function CodesysU16(V:Word):TBytes;
function CodesysU32(V:LongWord):TBytes;
function CodesysGetU16(const B:TBytes; Offset:LongInt):Word;
function CodesysGetU32(const B:TBytes; Offset:LongInt):LongWord;
{: Encodes a symbol name the way CmpIecVarAccess expects it. }
function CodesysNameData(const Name:AnsiString):TBytes;
{: Builds a TBytes from a list of bytes (FPC compatible). }
function MakeBytes(const Vals:array of Byte):TBytes;

implementation

const
  CODESYS_TCP_MAGIC:array[0..3] of Byte = ($00,$01,$17,$E8);
  CODESYS_DATAGRAM_MAGIC = $C5;
  CODESYS_SERVICE_NSSERVER = 3;      // ACK frames in the real HMI capture
  CODESYS_SERVICE_CHANNELMANAGER = 64;
  CODESYS_CH_OPENREQ  = $C3;
  CODESYS_CH_OPENRESP = $83;
  CODESYS_CH_BLK      = $01;
  CODESYS_CH_ACK      = $02;
  CODESYS_CH_KEEPALIVE= $03;
  CODESYS_CH_CLOSE    = $C4;         // seen in the real HMI capture
  CODESYS_CH_ERROR    = $84;
  CODESYS_PROTOCOL_ID = $CD55;
  CODESYS_CMP_DEVICE  = $01;
  CODESYS_CMP_VARACCESS = $09;
  // Verified: the MCGS HMI advertises 0x00004000, not 0x7D000.
  CODESYS_RECV_BUFFER = $00004000;
  // 4096 was chosen after a real-runtime experiment (2026-11-27): the PLC
  // ACCEPTS one large BLK block but RESETS the TCP connection as soon as it
  // sees a BLK continuation frame (SendAppBlock's fragment path). Therefore
  // the driver never fragments: ReadVariables keeps every Register(1) small
  // (see CODESYS_MAX_BATCH_CONTENT) and SendAppBlock always emits one block.
  CODESYS_MAX_PDU     = 4096;
  // Verified limit on CODESYS Control Win V3 3.5.14 with a loaded boot
  // application: Register(1) with 10 symbols / 470 encoded bytes is accepted,
  // 11 symbols / 512 bytes makes the PLC close the connection (10054).
  // Keep a comfortable margin below both the size and the count limit.
  CODESYS_BATCH_BASE        = 42;   // 20 byte service header + 22 fixed TLV bytes
  CODESYS_MAX_BATCH_CONTENT = 440;  // encoded service payload bytes per batch
  CODESYS_MAX_BATCH_NAMES   = 10;   // never register more than 10 names at once
  // Anonymous login payload, byte-for-byte from the real HMI capture. The
  // 0x11 blob and the 0x23 seed were IDENTICAL in all 12 captured login
  // requests, so they can be sent verbatim; a password-protected runtime will
  // still reject this client (no challenge-response / RSA / scrypt).
  CODESYS_LOGIN_MODE  = $00000001; // tag 0x22
  CODESYS_LOGIN_SEED  = $08F02F26; // tag 0x23, bytes 26 2F F0 08
  CODESYS_LOGIN_NONCE:array[0..31] of Byte = (
    $A0,$65,$44,$52,$5F,$36,$45,$66,$7B,$23,$32,$37,$9C,$75,$70,$68,
    $5D,$54,$68,$75,$88,$3F,$70,$68,$87,$44,$72,$2A,$98,$55,$62,$52);

var
  CRC32Table:array[0..255] of LongWord;
  CRC32Ready:Boolean = false;

procedure InitCRC32;
var i,j:LongInt; C:LongWord;
begin
  if CRC32Ready then exit;
  for i:=0 to 255 do begin
    C := LongWord(i);
    for j:=0 to 7 do
      if (C and 1)<>0 then C := (C shr 1) xor $EDB88320
      else C := C shr 1;
    CRC32Table[i] := C;
  end;
  CRC32Ready := true;
end;

function CodesysCRC32(const Data:TBytes; Count:LongInt):LongWord;
var i,n:LongInt;
begin
  InitCRC32;
  if Count>Length(Data) then Count := Length(Data);
  Result := $FFFFFFFF;
  for i:=0 to Count-1 do
    Result := CRC32Table[(Result xor Data[i]) and $FF] xor (Result shr 8);
  Result := Result xor $FFFFFFFF;
end;

procedure CodesysPutVarInt(var B:TBytes; Value:QWord);
begin
  while Value >= $80 do begin
    SetLength(B,Length(B)+1);
    B[High(B)] := Byte((Value and $7F) or $80);
    Value := Value shr 7;
  end;
  SetLength(B,Length(B)+1);
  B[High(B)] := Byte(Value);
end;

procedure CodesysPutTag(var B:TBytes; Id:QWord; const Data:TBytes);
var i,n:LongInt;
begin
  CodesysPutVarInt(B,Id);
  CodesysPutVarInt(B,QWord(Length(Data)));
  n := Length(Data);
  if n>0 then begin
    i := Length(B);
    SetLength(B,i+n);
    Move(Data[0],B[i],n);
  end;
end;

function ReadVarInt(const B:TBytes; var Pos:LongInt; out Value:QWord):Boolean;
var Shift:LongInt; Byte_:Byte;
begin
  Result := false;
  Value := 0;
  Shift := 0;
  while (Pos<Length(B)) and (Shift<64) do begin
    Byte_ := B[Pos];
    Inc(Pos);
    Value := Value or (QWord(Byte_ and $7F) shl Shift);
    if (Byte_ and $80)=0 then exit(true);
    Inc(Shift,7);
  end;
end;

function CodesysParseTags(const B:TBytes; out Tags:TCodesysTags):Boolean;
var Pos,Start,DataStart,DataLen,n:LongInt; Id,Size:QWord;

  procedure ParseLevel(AParent:QWord; ALimit:LongInt);
  var LId,LSize:QWord; LStart,LDataStart,LDataLen:LongInt;
  begin
    while Pos < ALimit do begin
      LStart := Pos;
      if not ReadVarInt(B,Pos,LId) then exit;
      if not ReadVarInt(B,Pos,LSize) then exit;
      LDataStart := Pos;
      LDataLen := LongInt(LSize);
      if (LDataLen<0) or (LDataStart+LDataLen>ALimit) then exit;
      n := Length(Tags);
      SetLength(Tags,n+1);
      Tags[n].Id := LId;
      Tags[n].Parent := AParent;
      SetLength(Tags[n].Data,LDataLen);
      if LDataLen>0 then Move(B[LDataStart],Tags[n].Data[0],LDataLen);
      if LId >= $80 then ParseLevel(LId,LDataStart+LDataLen);
      Pos := LDataStart+LDataLen;
      if Pos<=LStart then exit;
    end;
  end;

begin
  SetLength(Tags,0);
  Pos := 0;
  if Length(B)=0 then exit(true);
  ParseLevel(0,Length(B));
  Result := Pos = Length(B);
end;

function CodesysFindTag(const Tags:TCodesysTags; Id:QWord; out Data:TBytes):Boolean;
var i:LongInt;
begin
  SetLength(Data,0);
  for i:=0 to High(Tags) do
    if Tags[i].Id=Id then begin
      Data := Tags[i].Data;
      exit(true);
    end;
  Result := false;
end;

function CodesysU16(V:Word):TBytes;
begin
  SetLength(Result,2);
  Result[0] := Byte(V and $FF);
  Result[1] := Byte(V shr 8);
end;

function CodesysU32(V:LongWord):TBytes;
begin
  SetLength(Result,4);
  Result[0] := Byte(V and $FF);
  Result[1] := Byte((V shr 8) and $FF);
  Result[2] := Byte((V shr 16) and $FF);
  Result[3] := Byte((V shr 24) and $FF);
end;

function CodesysGetU16(const B:TBytes; Offset:LongInt):Word;
begin
  Result := 0;
  if (Offset<0) or (Offset+2>Length(B)) then exit;
  Result := Word(B[Offset]) or (Word(B[Offset+1]) shl 8);
end;

function CodesysGetU32(const B:TBytes; Offset:LongInt):LongWord;
begin
  Result := 0;
  if (Offset<0) or (Offset+4>Length(B)) then exit;
  Result := LongWord(B[Offset]) or (LongWord(B[Offset+1]) shl 8) or
            (LongWord(B[Offset+2]) shl 16) or (LongWord(B[Offset+3]) shl 24);
end;

function MakeBytes(const Vals:array of Byte):TBytes;
var i:LongInt;
begin
  SetLength(Result,Length(Vals));
  for i:=0 to High(Vals) do Result[i] := Vals[i];
end;

function CodesysNameData(const Name:AnsiString):TBytes;
var L,NameLen,Aligned,n,i:LongInt;
begin
  L := Length(Name);
  // Verified 2026-11-27 against 9 captured 0x19 tags: the data is
  //   u16(L+1) + name + #0 + zero padding,
  // where the NAME AREA (L+1 bytes) is aligned to 4 and the 2 byte prefix is
  // NOT included in that alignment. Total = 2 + align4(L+1).
  // Examples: L=36 -> 2+40 = 42 bytes, L=40 -> 2+44 = 46 bytes.
  // (The previous align4(2+L+1) produced 40/44 and was off by two bytes.)
  NameLen := L + 1;
  Aligned := ((NameLen + 3) div 4) * 4;
  n := 2 + Aligned;
  SetLength(Result,n);
  for i:=0 to n-1 do Result[i] := 0;
  Result[0] := Byte(NameLen and $FF);
  Result[1] := Byte((NameLen shr 8) and $FF);
  if L>0 then Move(Name[1],Result[2],L);
  Result[2+L] := 0;
end;

procedure AppendBytes(var Dest:TBytes; const Src:TBytes);
var i,n:LongInt;
begin
  n := Length(Src);
  if n=0 then exit;
  i := Length(Dest);
  SetLength(Dest,i+n);
  Move(Src[0],Dest[i],n);
end;

{: Parses a dotted IPv4 address into 4 bytes. Host names are not resolved:
   the CODESYS receiver address is only used for routing, so an unresolved
   name falls back to 0.0.0.0 (a directly attached PLC still accepts it). }
function CodesysIPv4(const Host:AnsiString):TBytes;
var i,Start,j,v,n:LongInt; S:AnsiString; C:AnsiChar;
begin
  Result := MakeBytes([$00,$00,$00,$00]);
  S := Trim(String(Host));
  if S='' then exit;
  Start := 1;
  n := 0;
  for j:=1 to Length(S)+1 do
    if (j>Length(S)) or (S[j]='.') then begin
      if n>3 then begin
        Result := MakeBytes([$00,$00,$00,$00]);
        exit;
      end;
      v := 0;
      for i:=Start to j-1 do begin
        C := S[i];
        if not (C in ['0'..'9']) then begin
          Result := MakeBytes([$00,$00,$00,$00]);
          exit;
        end;
        v := v*10 + (Ord(C)-Ord('0'));
      end;
      if v>255 then begin
        Result := MakeBytes([$00,$00,$00,$00]);
        exit;
      end;
      Result[n] := Byte(v);
      Inc(n);
      Start := j+1;
    end;
  if n<>4 then Result := MakeBytes([$00,$00,$00,$00]);
end;

constructor TCodesysSymbolClient.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FPort := 11740;
  FTimeout := 3000;
  // The real HMI sends message id 0 in its handshake frames.
  FMsgID := 0;
end;

destructor TCodesysSymbolClient.Destroy;
begin
  Disconnect;
  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TCodesysSymbolClient.Fail(const Msg:AnsiString);
begin
  FLastError := Msg;
end;

function TCodesysSymbolClient.Connected:Boolean;
begin
  FCS.Enter;
  try
    Result := FLogged and FOpened;
  finally
    FCS.Leave;
  end;
end;

procedure TCodesysSymbolClient.CloseSocket;
begin
  {$IFDEF FPC}
  if FSocket<>nil then FreeAndNil(FSocket);
  {$ENDIF}
  FOpened := false;
  FLogged := false;
end;

procedure TCodesysSymbolClient.Disconnect;
begin
  FCS.Enter;
  try
    CloseSocket;
    FRegKey := '';
    FSessionID := 0;
    SetLength(FVarHandle,0);
    SetLength(FRegHandle,0);
    SetLength(FRegMap,0);
  finally
    FCS.Leave;
  end;
end;

function TCodesysSymbolClient.ReadExact(out Buf:TBytes; Count:LongInt;
  TimeoutMs:LongInt):Boolean;
var Deadline:QWord; Got,Chunk:LongInt;
begin
  Result := false;
  SetLength(Buf,0);
  if (FSocket=nil) or (Count<=0) then exit;
  SetLength(Buf,Count);
  Got := 0;
  Deadline := GetTickCount64 + QWord(Max(1,TimeoutMs));
  while Got<Count do begin
    if GetTickCount64>Deadline then begin
      Fail('Codesys: read timeout');
      exit;
    end;
    FSocket.IOTimeout := Max(1,TimeoutMs);
    Chunk := FSocket.Read(Buf[Got],Count-Got);
    if Chunk>0 then begin
      Inc(Got,Chunk);
      FLastRecv := GetTickCount64;
    end else begin
      Fail('Codesys: socket closed or read error ('+IntToStr(FSocket.LastError)+')');
      exit;
    end;
  end;
  Result := true;
end;

function TCodesysSymbolClient.SendRaw(const Data:TBytes):Boolean;
var Sent:LongInt;
begin
  Result := false;
  if (FSocket=nil) or (Length(Data)=0) then exit;
  Sent := FSocket.Write(Data[0],Length(Data));
  if Sent=Length(Data) then exit(true);
  Fail('Codesys: socket write failed ('+IntToStr(FSocket.LastError)+')');
end;

function TCodesysSymbolClient.SendDatagram(ServiceID:Byte;
  const Payload:TBytes):Boolean;
var DG,Frame,Addr:TBytes;
begin
  // Calibrated against the real MCGS HMI capture of 2026-11-27.
  // Datagram header (6 bytes): C5, hop=13/headerlen=3, priority normal +
  // full addressing, service id, message id, address-length nibbles (0x43).
  DG := MakeBytes([$C5,$6B,$40,ServiceID,FMsgID,$43]);
  // Receiver (6 bytes): 2 byte TCP port in network order + PLC IPv4.
  Addr := MakeBytes([Byte(FPort shr 8),Byte(FPort and $FF)]);
  AppendBytes(Addr,CodesysIPv4(FHost));
  AppendBytes(DG,Addr);
  // Sender (8 bytes): port 0, IPv4 (the HMI puts the PLC address here too),
  // then the two reserved bytes 85 EA that appear in every captured frame.
  Addr := MakeBytes([$00,$00]);
  AppendBytes(Addr,CodesysIPv4(FHost));
  AppendBytes(Addr,MakeBytes([$85,$EA]));
  AppendBytes(DG,Addr);
  AppendBytes(DG,Payload);

  // Block driver layer: magic + TOTAL length (8 header bytes + datagram).
  // The previous implementation omitted this layer entirely.
  Frame := MakeBytes([$00,$01,$17,$E8]);
  AppendBytes(Frame,CodesysU32(LongWord(8+Length(DG))));
  AppendBytes(Frame,DG);

  Inc(FMsgID);
  if FMsgID=0 then Inc(FMsgID);
  Result := SendRaw(Frame);
end;

function TCodesysSymbolClient.ReceiveFrame(out ServiceID:Byte;
  out Payload:TBytes; out IsService:Boolean):Boolean;
var BlockHdr,DatagramHdr,Address,ServiceData,ChannelData,Crcd,Chunk:TBytes;
    Len,HeaderSize,SenderLen,ReceiverLen,Pad,Off,Pos,Type_,Flags,Chan:LongInt;
    Remaining,BlkID,AckID,Total,PartLen:LongWord;
begin
  Result := false;
  ServiceID := 0;
  IsService := false;
  SetLength(Payload,0);

  if not ReadExact(BlockHdr,8,FTimeout) then exit;
  if (BlockHdr[0]<>CODESYS_TCP_MAGIC[0]) or (BlockHdr[1]<>CODESYS_TCP_MAGIC[1]) or
     (BlockHdr[2]<>CODESYS_TCP_MAGIC[2]) or (BlockHdr[3]<>CODESYS_TCP_MAGIC[3]) then begin
    Fail('Codesys: invalid block magic');
    exit;
  end;
  Len := LongInt(CodesysGetU32(BlockHdr,4));
  // Verified against the real HMI capture: the block length is the TOTAL
  // frame length INCLUDING these 8 header bytes.
  if (Len<12) or (Len>65535) then begin
    Fail('Codesys: invalid block length '+IntToStr(Len));
    exit;
  end;
  if not ReadExact(Chunk,Len-8,FTimeout) then exit;
  if Chunk[0]<>CODESYS_DATAGRAM_MAGIC then begin
    Fail('Codesys: invalid datagram magic');
    exit;
  end;
  HeaderSize := (Chunk[1] and $07)*2;
  if HeaderSize<6 then HeaderSize := 6;
  ServiceID := Chunk[3];
  ReceiverLen := (Chunk[5] and $0F)*2;
  SenderLen := (Chunk[5] shr 4)*2;
  Off := HeaderSize + ReceiverLen + SenderLen;
  Pad := Off mod 4;
  if Pad<>0 then Inc(Off,4-Pad);
  if Off>Length(Chunk) then begin
    Fail('Codesys: truncated datagram');
    exit;
  end;
  SetLength(ChannelData,Length(Chunk)-Off);
  if Length(ChannelData)>0 then Move(Chunk[Off],ChannelData[0],Length(ChannelData));

  if Length(ChannelData)<2 then begin
    Fail('Codesys: empty channel frame');
    exit;
  end;
  Type_ := ChannelData[0];
  Flags := ChannelData[1];
  case Type_ of
    CODESYS_CH_OPENRESP:
      begin
        if Length(ChannelData)<18 then begin
          Fail('Codesys: short OpenChannel response');
          exit;
        end;
        if CodesysGetU16(ChannelData,10)<>0 then begin
          Fail('Codesys: OpenChannel rejected (reason '+IntToStr(CodesysGetU16(ChannelData,10))+')');
          exit;
        end;
        // Verified 2026-11-27: the assigned channel id is at offset 14
        // (e.g. 0xDA58), and the HMI then uses it in every following BLK,
        // ACK and KeepAlive frame. Offset 12 is zero in a real response.
        FChannelID := CodesysGetU16(ChannelData,14);
        Payload := ChannelData;
        Result := true;
      end;
    CODESYS_CH_ERROR:
      begin
        Fail('Codesys: channel error '+IntToHex(CodesysGetU16(ChannelData,4),4));
        Result := false;
      end;
    CODESYS_CH_KEEPALIVE:
      begin
        SendKeepAlive;
        Result := ReceiveFrame(ServiceID,Payload,IsService);
      end;
    CODESYS_CH_ACK:
      begin
        Result := ReceiveFrame(ServiceID,Payload,IsService);
      end;
    CODESYS_CH_BLK:
      begin
        if Length(ChannelData)<12 then exit;
        Chan := CodesysGetU16(ChannelData,2);
        BlkID := CodesysGetU32(ChannelData,4);
        AckID := CodesysGetU32(ChannelData,8);
        FAckID := AckID;
        if (Flags and $01)<>0 then begin
          if Length(ChannelData)<22 then exit;
          // Verified 2026-11-27: BLK first block is
          //   total@12, crc@16, service payload@20 (crc = crc32(service)).
          // Reading total@14 mixed the total's high half with the CRC and
          // made the reassembly loop wait for non-existent continuation frames.
          Total := CodesysGetU32(ChannelData,12);
          if Total>CODESYS_MAX_PDU*64 then begin
            Fail('Codesys: payload too large');
            exit;
          end;
          SetLength(Payload,0);
          ServiceData := Copy(ChannelData,20,Length(ChannelData)-20);
          AppendBytes(Payload,ServiceData);
          SendAck(BlkID);
          while LongWord(Length(Payload))<Total do begin
            if not ReadExact(BlockHdr,8,FTimeout) then exit;
            Len := LongInt(CodesysGetU32(BlockHdr,4));
            if (Len<12) or (Len>65535) then exit;
            if not ReadExact(Chunk,Len-8,FTimeout) then exit;
            HeaderSize := (Chunk[1] and $07)*2;
            if HeaderSize<6 then HeaderSize := 6;
            ReceiverLen := (Chunk[5] and $0F)*2;
            SenderLen := (Chunk[5] shr 4)*2;
            Off := HeaderSize + ReceiverLen + SenderLen;
            Pad := Off mod 4;
            if Pad<>0 then Inc(Off,4-Pad);
            if Off+12>Length(Chunk) then exit;
            if Chunk[Off]<>CODESYS_CH_BLK then exit;
            BlkID := CodesysGetU32(Chunk,Off+4);
            PartLen := Length(Chunk)-Off-12;
            if PartLen>0 then begin
              Pos := Length(Payload);
              SetLength(Payload,Pos+PartLen);
              Move(Chunk[Off+12],Payload[Pos],PartLen);
            end;
            SendAck(BlkID);
          end;
          IsService := true;
          Result := true;
        end else begin
          // continuation block without a first block: ignore
          SendAck(BlkID);
          Result := ReceiveFrame(ServiceID,Payload,IsService);
        end;
      end;
    $80:
      begin
        // Router / name-service announcement (type 0x80, 162 bytes) seen
        // once per connection in the s2c direction. Decoded 2026-11-27: it
        // carries a GUID/node id and the UTF-16LE strings
        // "CODESYS Control Win V3 x64" and "3S - Smart Software Solutions
        // GmbH". It is not a service frame; skip it and keep reading
        // instead of aborting the handshake.
        Result := ReceiveFrame(ServiceID,Payload,IsService);
      end;
  else
    Fail('Codesys: unknown channel frame type '+IntToHex(Type_,2));
  end;
end;

function TCodesysSymbolClient.SendAck(BlkID:LongWord):Boolean;
var Data:TBytes;
begin
  SetLength(Data,0);
  Data := MakeBytes([CODESYS_CH_ACK,$00]);
  AppendBytes(Data,CodesysU16(FChannelID));
  AppendBytes(Data,CodesysU32(BlkID));
  Result := SendDatagram(CODESYS_SERVICE_CHANNELMANAGER,Data);
end;

function TCodesysSymbolClient.SendKeepAlive:Boolean;
var Data:TBytes;
begin
  SetLength(Data,0);
  Data := MakeBytes([CODESYS_CH_KEEPALIVE,$00]);
  AppendBytes(Data,CodesysU16(FChannelID));
  Result := SendDatagram(CODESYS_SERVICE_CHANNELMANAGER,Data);
end;

function TCodesysSymbolClient.SendAppBlock(const Payload:TBytes):Boolean;
var Data,Part,Crcd:TBytes; Pos,ChunkLen:LongInt; First:Boolean;
begin
  Result := false;
  Pos := 0;
  First := true;
  repeat
    ChunkLen := Length(Payload)-Pos;
    if ChunkLen>CODESYS_MAX_PDU then ChunkLen := CODESYS_MAX_PDU;
    SetLength(Part,ChunkLen);
    if ChunkLen>0 then Move(Payload[Pos],Part[0],ChunkLen);
    SetLength(Data,0);
    Data := MakeBytes([CODESYS_CH_BLK,$80]);
    if First then Data[1] := $81;
    AppendBytes(Data,CodesysU16(FChannelID));
    AppendBytes(Data,CodesysU32(FBlkID));
    Inc(FBlkID);
    AppendBytes(Data,CodesysU32(FAckID));
    if First then begin
      AppendBytes(Data,CodesysU32(LongWord(Length(Payload))));
      Crcd := CodesysU32(CodesysCRC32(Payload,Length(Payload)));
      AppendBytes(Data,Crcd);
      First := false;
    end;
    AppendBytes(Data,Part);
    if not SendDatagram(CODESYS_SERVICE_CHANNELMANAGER,Data) then exit;
    Inc(Pos,ChunkLen);
  until Pos>=Length(Payload);
  Result := true;
end;

function TCodesysSymbolClient.ServiceCall(Group,SubCmd:Word;
  const Tags:TCodesysTags; out ResponseTags:TCodesysTags):Boolean;
var Service,Content,TData:TBytes; i:LongInt; T:TCodesysTag; SID:Byte;
    IsService:Boolean;
begin
  Result := false;
  SetLength(ResponseTags,0);
  SetLength(Content,0);
  for i:=0 to High(Tags) do begin
    T := Tags[i];
    CodesysPutTag(Content,T.Id,T.Data);
  end;
  SetLength(Service,0);
  AppendBytes(Service,CodesysU16(CODESYS_PROTOCOL_ID));
  AppendBytes(Service,CodesysU16(16)); // header size, +4 bytes below
  AppendBytes(Service,CodesysU16(Group));
  AppendBytes(Service,CodesysU16(SubCmd));
  AppendBytes(Service,CodesysU32(FSessionID));
  AppendBytes(Service,CodesysU32(LongWord(Length(Content))));
  AppendBytes(Service,CodesysU32(0)); // additional data
  AppendBytes(Service,Content);

  if not SendAppBlock(Service) then exit;
  if not ReceiveFrame(SID,TData,IsService) then exit;
  if not IsService then begin
    Fail('Codesys: expected a service response');
    exit;
  end;
  if Length(TData)<20 then begin
    Fail('Codesys: short service response');
    exit;
  end;
  if CodesysGetU16(TData,0)<>CODESYS_PROTOCOL_ID then begin
    Fail('Codesys: unexpected service protocol id');
    exit;
  end;
  // Verified 2026-11-27 against real responses: the group u16 at offset 4
  // has bit 0x80 set on responses (0x0081 CmpDevice, 0x0089 CmpIecVarAccess)
  // and group = value AND $7F. These two checks are therefore correct and
  // are kept unchanged.
  if (CodesysGetU16(TData,4) and $80)=0 then begin
    Fail('Codesys: service response flag missing');
    exit;
  end;
  if (CodesysGetU16(TData,4) and $7F)<>Group then begin
    Fail('Codesys: unexpected service group');
    exit;
  end;
  if not CodesysParseTags(Copy(TData,20,Length(TData)-20),ResponseTags) then begin
    Fail('Codesys: malformed response tags');
    exit;
  end;
  Result := true;
end;

function TCodesysSymbolClient.DeviceLogin:Boolean;
var Tags,Resp:TCodesysTags; Child,Blob,D:TBytes; i:LongInt;
begin
  Result := false;
  FSessionID := 0;

  // Step 1: CmpDevice(1) subcmd=1 - device information, request has NO tags
  // (content=0). The response describes the runtime; we do not need it.
  SetLength(Tags,0);
  if not ServiceCall(CODESYS_CMP_DEVICE,$01,Tags,Resp) then exit;

  // Step 2: CmpDevice(1) subcmd=2 - anonymous login. The MCGS HMI sends this
  // exact payload (verified identical in all 12 captured login requests):
  //   tag 0x22 = u32(1), tag 0x23 = u32(0x08F02F26),
  //   parent 0x81 { tag 0x10 = u16(0), tag 0x11 = 32 byte fixed nonce }.
  // TLV size varints are emitted in their canonical (shortest) form; the
  // runtime accepts both, as its own responses mix canonical and overlong.
  SetLength(Child,0);
  CodesysPutTag(Child,$10,MakeBytes([$00,$00]));
  SetLength(Blob,Length(CODESYS_LOGIN_NONCE));
  for i:=0 to High(CODESYS_LOGIN_NONCE) do Blob[i] := CODESYS_LOGIN_NONCE[i];
  CodesysPutTag(Child,$11,Blob);

  SetLength(Tags,3);
  Tags[0].Id := $22; Tags[0].Parent := 0; Tags[0].Data := CodesysU32(CODESYS_LOGIN_MODE);
  Tags[1].Id := $23; Tags[1].Parent := 0; Tags[1].Data := CodesysU32(CODESYS_LOGIN_SEED);
  Tags[2].Id := $81; Tags[2].Parent := 0; Tags[2].Data := Child;
  if not ServiceCall(CODESYS_CMP_DEVICE,$02,Tags,Resp) then exit;

  // Response parent 0x82 holds tag 0x21 = session id (u32 LE). It is
  // different on every connection, so it must be read from the response.
  if not CodesysFindTag(Resp,$21,D) then begin
    Fail('Codesys: login response has no session id (tag 0x21). '+
         'A password-protected runtime is not supported by this client.');
    exit;
  end;
  if Length(D)<4 then begin
    Fail('Codesys: short login session id');
    exit;
  end;
  FSessionID := CodesysGetU32(D,0);
  Result := true;
end;

function TCodesysSymbolClient.GetVarHandles:Boolean;
var Tags,Resp:TCodesysTags; D:TBytes;
begin
  Result := false;
  SetLength(Tags,0);
  SetLength(FVarHandle,0);
  FVarHandleTag := $14;
  // GetHandle(7) sends NO tags. Its answer depends on whether the runtime
  // has a loaded application:
  //   * empty runtime  : tag 0x14 = u16 handle (HMI capture: 0x0018)
  //   * app loaded     : tag 0x30 = app name, 0x38 = 20 byte browse blob,
  //                     0x3A = u32 session handle (CmpIecVarAccess browse)
  // Both 0x14 and 0x3A are accepted by Register(1) as the var-access
  // handle; 0x38 is NOT (verified: it closes the channel). Verified with a
  // real boot application on 2026-11-27 (tools/codesys_dbg6.pas).
  if not ServiceCall(CODESYS_CMP_VARACCESS,$07,Tags,Resp) then exit;
  if CodesysFindTag(Resp,$14,FVarHandle) then begin
    FVarHandleTag := $14;
    Result := true;
    exit;
  end;
  if CodesysFindTag(Resp,$3A,D) then begin
    FVarHandle := D;
    FVarHandleTag := $3A;
    Result := true;
    exit;
  end;
  Fail('Codesys: GetHandle(7) returned neither tag 0x14 nor 0x3A. '+
       'A password-protected runtime may require a login, which this client does not support.');
end;

function TCodesysSymbolClient.RegisterVars(const Names:array of AnsiString):Boolean;
var Tags,Resp:TCodesysTags; NameData,Flags:TBytes;
    i,j,k:LongInt; Key:AnsiString;
begin
  Result := false;
  if Length(Names)=0 then exit(true);
  Key := '';
  for i:=0 to High(Names) do Key := Key+Names[i]+#10;
  if (FRegKey=Key) and (Length(FRegHandle)>0) then exit(true);

  // Verified 2026-11-27 Register(1) request TLV order:
  //   0x13 flags(48 00 01 00), 0x00 (empty), 0x00 (empty),
  //   <var-access handle: 0x14 on an empty runtime, 0x3A when an app is
  //    loaded>, 0x18 u32(count), 0x19 name x N.
  // The handle tag id comes from GetVarHandles (FVarHandleTag).
  SetLength(Tags,0);
  Flags := MakeBytes([$48,$00,$01,$00]);
  SetLength(Tags,5+Length(Names));
  Tags[0].Id := $13; Tags[0].Parent := 0; Tags[0].Data := Flags;
  Tags[1].Id := $00; Tags[1].Parent := 0; SetLength(Tags[1].Data,0);
  Tags[2].Id := $00; Tags[2].Parent := 0; SetLength(Tags[2].Data,0);
  Tags[3].Id := FVarHandleTag; Tags[3].Parent := 0;
  Tags[3].Data := FVarHandle;
  Tags[4].Id := $18; Tags[4].Parent := 0; Tags[4].Data := CodesysU32(LongWord(Length(Names)));
  for i:=0 to High(Names) do begin
    NameData := CodesysNameData(Names[i]);
    Tags[5+i].Id := $19;
    Tags[5+i].Parent := 0;
    Tags[5+i].Data := NameData;
  end;
  if not ServiceCall(CODESYS_CMP_VARACCESS,$01,Tags,Resp) then exit;
  // Response tag 0x10 is the registration handle (captured: 2F 15 00 00).
  if not CodesysFindTag(Resp,$10,FRegHandle) then begin
    Fail('Codesys: variable registration failed (no tag 0x10)');
    exit;
  end;

  // Per-variable registration results, IN REQUEST ORDER. Verified 2026-11-27
  // against a real loaded application (captures/unknown_probe_*):
  //   0x1B size=12 -> success: [u32 IEC type][u32 byte size][u32 offset]
  //   0x1C size=2  -> rejected name (u16 error code)
  // A 10-symbol all-valid Register produced 10 x 0x1B. With one unknown
  // name in the middle the response was 0x1B, 0x1C, 0x1B and Read(3)
  // returned only TWO 0x1D values. Mapping those N values onto
  // Results[0..N-1] silently shifted the INT value onto the unknown name
  // and left the real INT tag empty (tools/codesys_unknown_probe.pas).
  // Remember the request index of each successful 0x1B so ReadVariables
  // can map 0x1D[n] -> Results[FRegMap[n]].
  SetLength(FRegMap,0);
  k := 0;
  for j:=0 to High(Resp) do
    if (Resp[j].Id=$1B) or (Resp[j].Id=$1C) then begin
      if k<=High(Names) then begin
        if Resp[j].Id=$1B then begin
          SetLength(FRegMap,Length(FRegMap)+1);
          FRegMap[High(FRegMap)] := k;
        end;
        Inc(k);
      end;
    end;
  FRegKey := Key;
  Result := true;
end;

function TCodesysSymbolClient.ReadVariables(const Names:array of AnsiString;
  var Results:array of TCodesysVarResult):Boolean;
var Tags,Resp:TCodesysTags; Batch:array of AnsiString;
    i,j,n,Idx,Start,Cnt,Payload,L,Aligned,Item:LongInt;
    D:TBytes; Stat,LastStat:LongWord; AnyValue:Boolean;
begin
  Result := false;
  FCS.Enter;
  try
    for i:=0 to High(Results) do begin
      SetLength(Results[i].Raw,0);
      Results[i].Status := 0;
      Results[i].OK := false;
    end;
    if not FLogged then exit;
    if Length(Names)=0 then exit(true);
    if Length(Names)<>Length(Results) then begin
      Fail('Codesys: read array size mismatch');
      exit;
    end;

    // Verified 2026-11-27 against a REAL running CODESYS application:
    // ONE Register(1) followed by ONE Read(3) returns one tag 0x1D per
    // SUCCESSFULLY registered variable, in acceptance order. Each 0x1D is
    // 4 header bytes + the IEC value (4 bytes for <=32 bit types, 8 bytes
    // for LREAL/LINT - verified: LREAL 0x1D is 12 bytes total).
    // Tag 0x22 is a fixed status/heartbeat word, NOT a value; 0x20 is an
    // error code (0 = OK). The earlier "one 0x22 only" rule was an artifact
    // of probing an EMPTY runtime where no 0x1D values exist at all.
    //
    // REAL RUNTIME LIMIT (CODESYS Control Win V3 3.5.14, boot app loaded):
    // a Register(1) with 11 symbols / 512 encoded bytes makes the PLC reset
    // the TCP connection (WSAECONNRESET 10054); 10 symbols / 470 bytes is
    // accepted. BLK continuation frames are rejected too. So the caller's
    // name list is split into batches by ENCODED payload size and count,
    // and each batch gets its own Register(1) + Read(3). RegisterVars fills
    // FRegMap with the accepted indices WITHIN that batch, so 0x1D[n] maps
    // to Results[Start + FRegMap[n]] - values never land on the wrong tag
    // even when an unknown symbol is interleaved.
    Start := 0;
    AnyValue := false;
    LastStat := 0;
    while Start < Length(Names) do begin
      Payload := CODESYS_BATCH_BASE;
      Cnt := 0;
      while (Start+Cnt < Length(Names)) and
            (Cnt < CODESYS_MAX_BATCH_NAMES) do begin
        L := Length(Names[Start+Cnt]);
        Aligned := ((L+1+3) div 4)*4;
        Item := 2 + Aligned + 3;  // 0x19 data + tag byte + worst-case varint
        if (Cnt>0) and (Payload+Item > CODESYS_MAX_BATCH_CONTENT) then break;
        Inc(Payload,Item);
        Inc(Cnt);
      end;
      if Cnt=0 then Cnt := 1;  // always make progress (one very long name)
      SetLength(Batch,Cnt);
      for i:=0 to Cnt-1 do Batch[i] := Names[Start+i];

      if not RegisterVars(Batch) then exit;
      SetLength(Tags,1);
      Tags[0].Id := $10; Tags[0].Parent := 0; Tags[0].Data := FRegHandle;
      if not ServiceCall(CODESYS_CMP_VARACCESS,$03,Tags,Resp) then exit;
      Stat := 0;
      for j:=0 to High(Resp) do
        if Resp[j].Id=$20 then begin
          D := Resp[j].Data;
          if Length(D)>=2 then Stat := CodesysGetU16(D,0);
        end;
      if Stat<>0 then LastStat := Stat;
      n := 0;
      for j:=0 to High(Resp) do begin
        if Resp[j].Id=$1D then begin
          if Length(FRegMap)>0 then begin
            if n<=High(FRegMap) then Idx := FRegMap[n] else Idx := -1;
          end else Idx := n;
          if Idx>=0 then Inc(Idx,Start);
          if (Idx>=0) and (Idx<=High(Results)) then begin
            D := Resp[j].Data;
            if Length(D)>4 then begin
              SetLength(Results[Idx].Raw,Length(D)-4);
              Move(D[4],Results[Idx].Raw[0],Length(Results[Idx].Raw));
            end;
            Results[Idx].Status := Stat;
            Results[Idx].OK := true;
          end;
          Inc(n);
        end;
      end;
      if n>0 then AnyValue := true;
      Inc(Start,Cnt);
    end;
    Result := AnyValue;
    if not Result then
      Fail('Codesys: read returned no values, last status '+IntToStr(LastStat));
  finally
    FCS.Leave;
  end;
end;

function TCodesysSymbolClient.WriteVariable(const Name:AnsiString;
  const Data:TBytes):Boolean;
var Tags,Resp:TCodesysTags; D,Nm,VB,Val:TBytes;
    i,L,Pad,Size,BufLen:LongInt; Status:Word;
begin
  // VERIFIED 2026-11-27 against a REAL running CODESYS application: this
  // request really changes the variable (BOOL FALSE->TRUE and INT 0->54321
  // were both read back). Layout:
  //   0x13 = 00 00 00 00
  //   0x21 = ASCII name + #0 + zero pad to align4(L+1)  (NO u16 prefix!)
  //   0x1E = [u16 IEC size][00 00][value buffer]
  //          value buffer = 8 bytes for types <= 4 bytes, 12 bytes for
  //          64 bit types, IEC value in the LOW bytes, rest zero.
  // Success = response tag 0x20 == 0. Non-zero (20 / 15) is a rejection.
  // Data must contain exactly the IEC value bytes (Length = type size).
  Result := false;
  FCS.Enter;
  try
    if not FLogged then exit;
    if Length(Name)=0 then exit;
    Size := Length(Data);
    if (Size<1) or (Size>8) then begin
      Fail('Codesys: invalid write value length');
      exit;
    end;

    // 0x21 name: unlike 0x19 there is NO u16 length prefix. Verified with
    // tools/codesys_dbg5.pas (name-mode 1 and 3 both accepted; mode 1 is
    // the minimal one, consistent with the 0x19 suffix rule).
    L := Length(Name);
    Pad := ((L+1)+3) and not 3;
    SetLength(Nm,Pad);
    for i:=1 to L do Nm[i-1] := Byte(Name[i]);
    for i:=L to Pad-1 do Nm[i] := 0;

    // 0x1E value buffer. 8 bytes for <=4 byte types, 12 for 64 bit types.
    if Size>4 then BufLen := 12 else BufLen := 8;
    SetLength(VB,BufLen);
    for i:=0 to BufLen-1 do VB[i] := 0;
    for i:=0 to Size-1 do VB[i] := Data[i];
    SetLength(Val,4+BufLen);
    Val[0] := Byte(Size and $FF);
    Val[1] := Byte((Size shr 8) and $FF);
    Val[2] := 0; Val[3] := 0;
    for i:=0 to BufLen-1 do Val[4+i] := VB[i];

    SetLength(Tags,3);
    Tags[0].Id := $13; Tags[0].Parent := 0;
    Tags[0].Data := MakeBytes([$00,$00,$00,$00]);
    Tags[1].Id := $21; Tags[1].Parent := 0; Tags[1].Data := Nm;
    Tags[2].Id := $1E; Tags[2].Parent := 0; Tags[2].Data := Val;
    if not ServiceCall(CODESYS_CMP_VARACCESS,$04,Tags,Resp) then exit;
    if not CodesysFindTag(Resp,$20,D) then begin
      Fail('Codesys: write rejected (no status tag 0x20 in response)');
      exit;
    end;
    Status := 0;
    if Length(D)>=2 then Status := CodesysGetU16(D,0);
    if Status<>0 then begin
      Fail('Codesys: write rejected (status '+IntToStr(Status)+')');
      exit;
    end;
    Result := true;
  finally
    FCS.Leave;
  end;
end;

function TCodesysSymbolClient.Connect(const AHost:AnsiString; APort:Word;
  TimeoutMs:LongInt):Boolean;
var Data,Tmp,SIDBytes:TBytes; SID:Byte; IsService:Boolean;
begin
  Result := false;
  FCS.Enter;
  try
    CloseSocket;
    FHost := AHost;
    FPort := APort;
    FTimeout := Max(100,TimeoutMs);
    FLastError := '';
    if Trim(String(FHost))='' then begin
      Fail('Codesys: empty host');
      exit;
    end;
    {$IFDEF FPC}
    try
      FSocket := TInetSocket.Create(String(FHost),FPort,FTimeout);
      FSocket.IOTimeout := FTimeout;
    except
      on E:Exception do begin
        Fail('Codesys: cannot connect to '+String(FHost)+':'+IntToStr(FPort)+' - '+E.Message);
        FSocket := nil;
        exit;
      end;
    end;
    {$ELSE}
    Fail('Codesys: FPC ssockets required');
    exit;
    {$ENDIF}

    // OpenChannel request (20 bytes), byte-for-byte from the real HMI capture:
    // C3 00 01 01 | CRC32 | msgid=1 | recvbuf=0x4000 | maxchannels=2
    // CRC32 covers the WHOLE channel frame with the CRC field zeroed
    // (verified 0xE326BA42 on the captured frame).
    SetLength(Data,0);
    Data := MakeBytes([CODESYS_CH_OPENREQ,$00,$01,$01]);
    AppendBytes(Data,CodesysU32(0)); // CRC placeholder
    AppendBytes(Data,CodesysU32(1)); // message id
    AppendBytes(Data,CodesysU32(CODESYS_RECV_BUFFER));
    AppendBytes(Data,CodesysU32(2)); // max channels
    Tmp := Copy(Data,0,Length(Data));
    Tmp[4] := 0; Tmp[5] := 0; Tmp[6] := 0; Tmp[7] := 0;
    SIDBytes := CodesysU32(CodesysCRC32(Tmp,Length(Tmp)));
    Data[4] := SIDBytes[0]; Data[5] := SIDBytes[1];
    Data[6] := SIDBytes[2]; Data[7] := SIDBytes[3];
    if not SendDatagram(CODESYS_SERVICE_CHANNELMANAGER,Data) then exit;
    if not ReceiveFrame(SID,Data,IsService) then exit;
    FOpened := true;
    FBlkID := 1;
    FAckID := 0;
    FRegKey := '';
    SetLength(FRegMap,0);
    FSessionID := 0;
    SetLength(FVarHandle,0);
    // Verified 2026-11-27 session flow: after OpenChannel the HMI performs
    // CmpDevice device-info + anonymous login (which yields the session id
    // used by EVERY later request) and only then GetHandle. The old code
    // skipped login entirely and talked to the PLC with session id 0.
    if not DeviceLogin then begin
      CloseSocket;
      exit;
    end;
    if not GetVarHandles then begin
      CloseSocket;
      exit;
    end;
    FLogged := true;
    Result := true;
  finally
    FCS.Leave;
  end;
end;

end.
