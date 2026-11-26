{$i ../common/language.inc}
{:
  @abstract(Minimal CODESYS V3 symbol protocol (TCP 11740) client used by
            TCodesysSymbolDriver.)

  This is a self-contained implementation of the proprietary CODESYS V3
  "symbol" protocol. It does not use OPC UA, an OPC server, or any vendor DLL.

  Wire format (verified against the Microsoft CoDe16 Wireshark dissector):

    Block driver : 00 01 17 E8 (big endian magic) + payload length (u32 LE)
    Datagram     : C5, hop/header length, packet info, service id (64),
                   message id, address lengths, [addresses], padding
    Channel      : 0xC3 OpenChannel request / 0x83 response, 0x01 BLK
                   (0x81 first) + CRC32, 0x02 ACK, 0x03 KeepAlive, 0x84 error
    Service      : 0xCD55, header size, cmd group, subcmd, session id,
                   content size, additional data, then TLV tags
    Tags         : 7-bit varint id + 7-bit varint size + data;
                   id >= 0x80 means a parent tag (children follow in data)

  @bold(TEST STAGE / 测试阶段 - git tag @code(codesys11740-test-stage).)
  Experimental snapshot: compiled and checked by a local wire-encoding
  self-test, but never exercised against a real CODESYS runtime. The TLV
  layout, name padding (0x19) and subcommand values still need a real-PLC
  packet capture to be confirmed.

  Only anonymous (no password) access is implemented. PLCs whose runtime
  requires a login cannot be accessed with this client (no challenge-response).
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

  {: Raw result of one symbol read. Status is the value of tag 0x20 when the
     PLC sent one; OK is false when the request or the socket failed. }
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
    FHandle38:TBytes;
    FHandle3A:TBytes;
    FSessionID:LongWord;
    FRegKey:AnsiString;
    FRegHandle:TBytes;
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
  CODESYS_SERVICE_CHANNELMANAGER = 64;
  CODESYS_CH_OPENREQ  = $C3;
  CODESYS_CH_OPENRESP = $83;
  CODESYS_CH_BLK      = $01;
  CODESYS_CH_ACK      = $02;
  CODESYS_CH_KEEPALIVE= $03;
  CODESYS_CH_ERROR    = $84;
  CODESYS_PROTOCOL_ID = $CD55;
  CODESYS_CMP_DEVICE  = $01;
  CODESYS_CMP_VARACCESS = $09;
  CODESYS_RECV_BUFFER = $0007D000;
  CODESYS_MAX_PDU     = 512;

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
var L,n:LongInt;
begin
  L := Length(Name);
  // u16 length prefix, then zero-terminated name, then pad to 4 bytes
  n := 2 + L + 1;
  if (n mod 4)<>0 then Inc(n,4-(n mod 4));
  SetLength(Result,n);
  Result[0] := Byte((L+1) and $FF);
  Result[1] := Byte(((L+1) shr 8) and $FF);
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

constructor TCodesysSymbolClient.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FPort := 11740;
  FTimeout := 3000;
  FMsgID := 1;
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
    SetLength(FRegHandle,0);
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
var Frame:TBytes; HeaderLen:Byte; Lengths:Byte;
begin
  SetLength(Frame,0);
  // magic, hop=3 -> 6 byte datagram header, priority normal, no addresses
  Frame := MakeBytes([$C5,$03,$40,ServiceID,FMsgID,$00,$00,$00]);
  Inc(FMsgID);
  if FMsgID=0 then Inc(FMsgID);
  AppendBytes(Frame,Payload);
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
  if (Len<8) or (Len>65535) then begin
    Fail('Codesys: invalid block length '+IntToStr(Len));
    exit;
  end;
  if not ReadExact(Chunk,Len,FTimeout) then exit;
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
        FChannelID := CodesysGetU16(ChannelData,12);
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
          Total := CodesysGetU32(ChannelData,14);
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
            if (Len<8) or (Len>65535) then exit;
            if not ReadExact(Chunk,Len,FTimeout) then exit;
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

function TCodesysSymbolClient.GetVarHandles:Boolean;
var Tags,Resp:TCodesysTags; D:TBytes;
begin
  Result := false;
  SetLength(Tags,0);
  SetLength(FHandle38,0);
  SetLength(FHandle3A,0);
  if not ServiceCall(CODESYS_CMP_VARACCESS,$07,Tags,Resp) then exit;
  if not CodesysFindTag(Resp,$3A,FHandle3A) then begin
    Fail('Codesys: PLC did not return the variable access handle (tag 0x3a). '+
         'A password-protected runtime may require a login, which this client does not support.');
    exit;
  end;
  CodesysFindTag(Resp,$38,FHandle38);
  Result := true;
end;

function TCodesysSymbolClient.RegisterVars(const Names:array of AnsiString):Boolean;
var Tags,Resp:TCodesysTags; D,NameData,Flags:TBytes; i:LongInt; Key:AnsiString;
begin
  Result := false;
  if Length(Names)=0 then exit(true);
  Key := '';
  for i:=0 to High(Names) do Key := Key+Names[i]+#10;
  if (FRegKey=Key) and (Length(FRegHandle)>0) then exit(true);

  SetLength(Tags,0);
  Flags := MakeBytes([$48,$00,$01,$00]);
  SetLength(Tags,3+Length(Names));
  Tags[0].Id := $13; Tags[0].Parent := 0; Tags[0].Data := Flags;
  Tags[1].Id := $3A; Tags[1].Parent := 0; Tags[1].Data := FHandle3A;
  Tags[2].Id := $18; Tags[2].Parent := 0; Tags[2].Data := CodesysU32(LongWord(Length(Names)));
  for i:=0 to High(Names) do begin
    NameData := CodesysNameData(Names[i]);
    Tags[3+i].Id := $19;
    Tags[3+i].Parent := 0;
    Tags[3+i].Data := NameData;
  end;
  if not ServiceCall(CODESYS_CMP_VARACCESS,$01,Tags,Resp) then exit;
  if not CodesysFindTag(Resp,$10,FRegHandle) then begin
    Fail('Codesys: variable registration failed (no tag 0x10)');
    exit;
  end;
  FRegKey := Key;
  Result := true;
end;

function TCodesysSymbolClient.ReadVariables(const Names:array of AnsiString;
  var Results:array of TCodesysVarResult):Boolean;
var Tags,Resp:TCodesysTags; i,n:LongInt; D:TBytes;
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
    if not RegisterVars(Names) then exit;

    SetLength(Tags,3);
    Tags[0].Id := $13; Tags[0].Parent := 0; SetLength(Tags[0].Data,4);
    Tags[0].Data[0] := $48; Tags[0].Data[1] := $00;
    Tags[0].Data[2] := $01; Tags[0].Data[3] := $00;
    Tags[1].Id := $3A; Tags[1].Parent := 0; Tags[1].Data := FHandle3A;
    Tags[2].Id := $10; Tags[2].Parent := 0; Tags[2].Data := FRegHandle;
    if not ServiceCall(CODESYS_CMP_VARACCESS,$03,Tags,Resp) then exit;

    n := 0;
    for i:=0 to High(Resp) do begin
      if Resp[i].Id=$1D then begin
        if n<=High(Results) then begin
          // tag 0x1d carries a 4 byte header followed by the raw IEC value
          if Length(Resp[i].Data)>4 then begin
            SetLength(Results[n].Raw,Length(Resp[i].Data)-4);
            Move(Resp[i].Data[4],Results[n].Raw[0],Length(Results[n].Raw));
          end;
          Results[n].OK := true;
        end;
        Inc(n);
      end else if Resp[i].Id=$20 then begin
        D := Resp[i].Data;
        if (n>0) and (n<=Length(Results)) then
          Results[n-1].Status := CodesysGetU32(D,0);
      end;
    end;
    Result := true;
  finally
    FCS.Leave;
  end;
end;

function TCodesysSymbolClient.WriteVariable(const Name:AnsiString;
  const Data:TBytes):Boolean;
var Tags,Resp:TCodesysTags; D,Val,Flags,Tmp:TBytes;
begin
  Result := false;
  FCS.Enter;
  try
    if not FLogged then exit;
    if Length(Name)=0 then exit;
    SetLength(Tags,3);
    Flags := MakeBytes([$00,$00,$00,$00]);
    Tags[0].Id := $13; Tags[0].Parent := 0; Tags[0].Data := Flags;
    Tags[1].Id := $21; Tags[1].Parent := 0; Tags[1].Data := CodesysNameData(Name);
    // value: u16 length, 2 reserved bytes, then the IEC value (8/12 byte slot)
    SetLength(Tmp,4);
    Tmp[0] := Byte(Length(Data) and $FF);
    Tmp[1] := Byte((Length(Data) shr 8) and $FF);
    Tmp[2] := 0; Tmp[3] := 0;
    Val := Tmp;
    AppendBytes(Val,Data);
    Tags[2].Id := $1E; Tags[2].Parent := 0; Tags[2].Data := Val;
    if not ServiceCall(CODESYS_CMP_VARACCESS,$04,Tags,Resp) then exit;
    if CodesysFindTag(Resp,$20,D) and (Length(D)>=4) then begin
      if CodesysGetU32(D,0)<>0 then begin
        Fail('Codesys: write rejected, status '+IntToStr(CodesysGetU32(D,0)));
        exit;
      end;
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

    // OpenChannel request: type, flags, version, CRC32, msg id, recv buffer
    SetLength(Data,0);
    Data := MakeBytes([CODESYS_CH_OPENREQ,$00,$01,$01]);
    AppendBytes(Data,CodesysU32(0)); // CRC filled below
    AppendBytes(Data,CodesysU32(1)); // message id
    AppendBytes(Data,CodesysU32(CODESYS_RECV_BUFFER));
    // CRC32 covers msg id + receiver buffer size (the 8 bytes after the CRC)
    Tmp := Copy(Data,8,8);
    SIDBytes := CodesysU32(CodesysCRC32(Tmp,Length(Tmp)));
    Data[4] := SIDBytes[0]; Data[5] := SIDBytes[1];
    Data[6] := SIDBytes[2]; Data[7] := SIDBytes[3];
    if not SendDatagram(CODESYS_SERVICE_CHANNELMANAGER,Data) then exit;
    if not ReceiveFrame(SID,Data,IsService) then exit;
    FOpened := true;
    FBlkID := 1;
    FAckID := 0;
    FRegKey := '';
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
