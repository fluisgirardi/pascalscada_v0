{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Codificação e decodificação dos pacotes do protocolo MQTT 3.1.1.)
  @author(PascalSCADA)
}
{$ELSE}
{:
  @abstract(Encoding and decoding of MQTT 3.1.1 protocol packets.)
  @author(PascalSCADA)

  Pure packet layer: no socket, no PascalSCADA tag classes. Any byte string
  handled here is UTF-8 encoded (AnsiString holds the raw bytes).
}
{$ENDIF}
unit MQTTProtocol;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, Classes, Math, commtypes;

type
  {: MQTT control packet types. The ordinal values match the wire values. }
  TMQTTPacketType = (
    mpNone,          // 0 - not a valid MQTT packet
    mpCONNECT,       // 1
    mpCONNACK,       // 2
    mpPUBLISH,       // 3
    mpPUBACK,        // 4
    mpPUBREC,        // 5
    mpPUBREL,        // 6
    mpPUBCOMP,       // 7
    mpSUBSCRIBE,     // 8
    mpSUBACK,        // 9
    mpUNSUBSCRIBE,   // 10
    mpUNSUBACK,      // 11
    mpPINGREQ,       // 12
    mpPINGRESP,      // 13
    mpDISCONNECT     // 14
  );

  {: Quality of service of a PUBLISH packet. }
  TMQTTQoS = (mqQoS0 = 0, mqQoS1 = 1, mqQoS2 = 2);

  {: Parameters of the CONNECT packet. }
  TMQTTConnectOptions = record
    ClientID:AnsiString;
    UserName:AnsiString;
    Password:AnsiString;
    KeepAliveSec:Word;
    CleanSession:Boolean;
  end;

  {: A decoded MQTT packet. }
  TMQTTPacket = record
    PktType:TMQTTPacketType;
    Flags:Byte;
    QoS:TMQTTQoS;
    Retain:Boolean;
    DUP:Boolean;
    PacketID:Word;
    Topic:AnsiString;
    Payload:AnsiString;
    ConnAckSessionPresent:Boolean;
    ConnAckCode:Byte;
    SubAckCodes:BYTES;
  end;

  {: Accumulates received bytes and yields complete packets. MQTT allows a
     packet to be split over several TCP reads and several packets to arrive
     in a single read. }
  TMQTTDecoder = class
  private
    FBuffer:BYTES;
    function TryReadRemaining(var Pkt:TMQTTPacket; HeaderLen:LongInt; Remaining:LongInt; out Consumed:LongInt):Boolean;
  public
    procedure AddBytes(const Data:BYTES; Count:LongInt);
    procedure AddRaw(const Data; Count:LongInt);
    function NextPacket(out Pkt:TMQTTPacket; out Consumed:LongInt):Boolean;
    function Buffered:LongInt;
    procedure Clear;
  end;

function MQTTEncodeConnect(const Opts:TMQTTConnectOptions):BYTES;
function MQTTEncodeSubscribe(PacketID:Word; const Topics:array of AnsiString; QoS:TMQTTQoS):BYTES;
function MQTTEncodeUnsubscribe(PacketID:Word; const Topics:array of AnsiString):BYTES;
function MQTTEncodePublish(const Topic, Payload:AnsiString; QoS:TMQTTQoS; Retain:Boolean; PacketID:Word):BYTES;
function MQTTEncodePubAck(PacketID:Word):BYTES;
function MQTTEncodePingReq:BYTES;
function MQTTEncodeDisconnect:BYTES;

{: Converts a UTF-8 MQTT payload into a number. Accepts decimal numbers,
   true/false and 0x/$ prefixed hexadecimal integers. }
function MQTTPayloadToDouble(const Payload:AnsiString; out Value:Double):Boolean;

{: Human readable name of a CONNACK return code (MQTT 3.1.1). }
function MQTTConnAckText(Code:Byte):AnsiString;

implementation

procedure AppendByte(var B:BYTES; V:Byte); inline;
begin
  SetLength(B, Length(B)+1);
  B[High(B)] := V;
end;

procedure AppendWord(var B:BYTES; V:Word); inline;
begin
  AppendByte(B, Byte(V shr 8));
  AppendByte(B, Byte(V and $FF));
end;

procedure AppendString(var B:BYTES; const S:AnsiString); inline;
var
  L:LongInt;
  i:LongInt;
begin
  L := Length(S);
  if L > 65535 then L := 65535;
  AppendWord(B, Word(L));
  for i := 1 to L do
    AppendByte(B, Byte(S[i]));
end;

procedure AppendBytesTo(var Dest:BYTES; const Src:BYTES); inline;
var
  off:LongInt;
begin
  off := Length(Dest);
  SetLength(Dest, off+Length(Src));
  if Length(Src)>0 then
    Move(Src[0], Dest[off], Length(Src));
end;

procedure AppendRemainingLength(var B:BYTES; Len:LongInt);
var
  D:Byte;
begin
  repeat
    D := Byte(Len mod 128);
    Len := Len div 128;
    if Len > 0 then D := D or $80;
    AppendByte(B, D);
  until Len = 0;
end;

function DecodeRemainingLength(const B:BYTES; Start:LongInt; out Len:LongInt; out BytesUsed:LongInt):Boolean;
var
  i:LongInt;
  multiplier:LongInt;
  digit:Byte;
begin
  Result := false;
  Len := 0;
  BytesUsed := 0;
  multiplier := 1;
  for i := Start to Min(Start+3, High(B)) do begin
    digit := B[i];
    Len := Len + (digit and $7F) * multiplier;
    Inc(BytesUsed);
    if (digit and $80) = 0 then begin
      Result := true;
      exit;
    end;
    multiplier := multiplier * 128;
  end;
end;

function ReadString(const B:BYTES; var Pos:LongInt; out S:AnsiString):Boolean;
var
  L, i:LongInt;
begin
  Result := false;
  S := '';
  if Pos+2 > Length(B) then exit;
  L := (B[Pos] shl 8) or B[Pos+1];
  Inc(Pos, 2);
  if Pos+L > Length(B) then exit;
  SetLength(S, L);
  for i := 1 to L do
    S[i] := AnsiChar(B[Pos+i-1]);
  Inc(Pos, L);
  Result := true;
end;

function ReadWord(const B:BYTES; var Pos:LongInt; out V:Word):Boolean;
begin
  Result := false;
  V := 0;
  if Pos+2 > Length(B) then exit;
  V := (B[Pos] shl 8) or B[Pos+1];
  Inc(Pos, 2);
  Result := true;
end;

function MQTTEncodeConnect(const Opts:TMQTTConnectOptions):BYTES;
var
  body:BYTES;
  flags:Byte;
begin
  Result := nil;
  body := nil;

  // variable header
  AppendString(body, 'MQTT');
  AppendByte(body, 4); // protocol level 3.1.1
  flags := 0;
  if Opts.CleanSession then flags := flags or $02;
  if (Opts.UserName<>'') and (Opts.Password<>'') then flags := flags or $C0
  else if Opts.UserName<>'' then flags := flags or $80;
  AppendByte(body, flags);
  AppendWord(body, Opts.KeepAliveSec);

  // payload
  AppendString(body, Opts.ClientID);
  if (flags and $80) <> 0 then AppendString(body, Opts.UserName);
  if (flags and $40) <> 0 then AppendString(body, Opts.Password);

  SetLength(Result, 0);
  AppendByte(Result, Byte(mpCONNECT) shl 4);
  AppendRemainingLength(Result, Length(body));
  AppendBytesTo(Result, body);
end;

function MQTTEncodeSubscribe(PacketID:Word; const Topics:array of AnsiString; QoS:TMQTTQoS):BYTES;
var
  body:BYTES;
  i:LongInt;
  q:Byte;
begin
  Result := nil;
  body := nil;
  AppendWord(body, PacketID);
  q := Byte(QoS);
  for i := 0 to High(Topics) do begin
    AppendString(body, Topics[i]);
    AppendByte(body, q);
  end;
  AppendByte(Result, (Byte(mpSUBSCRIBE) shl 4) or $02);
  AppendRemainingLength(Result, Length(body));
  AppendBytesTo(Result, body);
end;

function MQTTEncodeUnsubscribe(PacketID:Word; const Topics:array of AnsiString):BYTES;
var
  body:BYTES;
  i:LongInt;
begin
  Result := nil;
  body := nil;
  AppendWord(body, PacketID);
  for i := 0 to High(Topics) do
    AppendString(body, Topics[i]);
  AppendByte(Result, (Byte(mpUNSUBSCRIBE) shl 4) or $02);
  AppendRemainingLength(Result, Length(body));
  AppendBytesTo(Result, body);
end;

function MQTTEncodePublish(const Topic, Payload:AnsiString; QoS:TMQTTQoS; Retain:Boolean; PacketID:Word):BYTES;
var
  body:BYTES;
  flags:Byte;
  i:LongInt;
begin
  Result := nil;
  body := nil;
  flags := Byte(QoS) shl 1;
  if Retain then flags := flags or $01;

  AppendString(body, Topic);
  if QoS <> mqQoS0 then AppendWord(body, PacketID);
  for i := 1 to Length(Payload) do
    AppendByte(body, Byte(Payload[i]));

  AppendByte(Result, (Byte(mpPUBLISH) shl 4) or flags);
  AppendRemainingLength(Result, Length(body));
  AppendBytesTo(Result, body);
end;

function MQTTEncodePubAck(PacketID:Word):BYTES;
begin
  Result := nil;
  AppendByte(Result, Byte(mpPUBACK) shl 4);
  AppendByte(Result, 2);
  AppendWord(Result, PacketID);
end;

function MQTTEncodePingReq:BYTES;
begin
  Result := nil;
  AppendByte(Result, Byte(mpPINGREQ) shl 4);
  AppendByte(Result, 0);
end;

function MQTTEncodeDisconnect:BYTES;
begin
  Result := nil;
  AppendByte(Result, Byte(mpDISCONNECT) shl 4);
  AppendByte(Result, 0);
end;

function MQTTDecodePacket(const B:BYTES; out Pkt:TMQTTPacket; out Consumed:LongInt):Boolean;
var
  rem, hlen, pos:LongInt;
  t:TMQTTPacketType;
  flags:Byte;
  i:LongInt;
begin
  Result := false;
  Consumed := 0;
  FillChar(Pkt, SizeOf(Pkt), 0);
  Pkt.PktType := mpNone;
  SetLength(Pkt.SubAckCodes, 0);

  if Length(B) < 2 then exit;
  t := TMQTTPacketType(B[0] shr 4);
  if (t = mpNone) or (t > mpDISCONNECT) then exit;
  flags := B[0] and $0F;
  if not DecodeRemainingLength(B, 1, rem, hlen) then exit;
  hlen := hlen + 1;
  if Length(B) < hlen + rem then exit; // incomplete

  Pkt.PktType := t;
  Pkt.Flags := flags;
  Consumed := hlen + rem;
  pos := hlen;

  case t of
    mpCONNACK: begin
      if rem < 2 then exit;
      Pkt.ConnAckSessionPresent := (B[pos] and $01) <> 0;
      Pkt.ConnAckCode := B[pos+1];
    end;

    mpPUBLISH: begin
      Pkt.QoS := TMQTTQoS((flags shr 1) and $03);
      Pkt.Retain := (flags and $01) <> 0;
      Pkt.DUP := (flags and $08) <> 0;
      if not ReadString(B, pos, Pkt.Topic) then exit;
      if Pkt.QoS <> mqQoS0 then
        if not ReadWord(B, pos, Pkt.PacketID) then exit;
      SetLength(Pkt.Payload, hlen+rem-pos);
      for i := 0 to Length(Pkt.Payload)-1 do
        Pkt.Payload[i+1] := AnsiChar(B[pos+i]);
    end;

    mpPUBACK, mpPUBREC, mpPUBREL, mpPUBCOMP,
    mpSUBACK, mpUNSUBACK: begin
      if not ReadWord(B, pos, Pkt.PacketID) then exit;
      if t = mpSUBACK then begin
        SetLength(Pkt.SubAckCodes, hlen+rem-pos);
        for i := 0 to High(Pkt.SubAckCodes) do begin
          Pkt.SubAckCodes[i] := B[pos+i];
          Inc(pos);
        end;
      end;
    end;

    mpPINGRESP, mpDISCONNECT, mpPINGREQ: begin
      // no variable header, no payload
    end;

    mpCONNECT, mpSUBSCRIBE, mpUNSUBSCRIBE: begin
      // server side packets: ignored, but consume correctly
    end;
  end;

  Result := true;
end;

{ TMQTTDecoder }

procedure TMQTTDecoder.AddBytes(const Data:BYTES; Count:LongInt);
begin
  if Count <= 0 then exit;
  if Count > Length(Data) then Count := Length(Data);
  AddRaw(Data[0], Count);
end;

procedure TMQTTDecoder.AddRaw(const Data; Count:LongInt);
var
  old:LongInt;
begin
  if Count <= 0 then exit;
  old := Length(FBuffer);
  SetLength(FBuffer, old+Count);
  Move(Data, FBuffer[old], Count);
end;

function TMQTTDecoder.NextPacket(out Pkt:TMQTTPacket; out Consumed:LongInt):Boolean;
begin
  Consumed := 0;
  Result := MQTTDecodePacket(FBuffer, Pkt, Consumed);
  if Result then begin
    if Consumed >= Length(FBuffer) then SetLength(FBuffer, 0)
    else begin
      Move(FBuffer[Consumed], FBuffer[0], Length(FBuffer)-Consumed);
      SetLength(FBuffer, Length(FBuffer)-Consumed);
    end;
  end;
end;

function TMQTTDecoder.Buffered:LongInt;
begin
  Result := Length(FBuffer);
end;

procedure TMQTTDecoder.Clear;
begin
  SetLength(FBuffer, 0);
end;

function TMQTTDecoder.TryReadRemaining(var Pkt:TMQTTPacket; HeaderLen:LongInt; Remaining:LongInt; out Consumed:LongInt):Boolean;
begin
  Result := false;
  Consumed := 0;
end;

function MQTTPayloadToDouble(const Payload:AnsiString; out Value:Double):Boolean;
var
  s, t:AnsiString;
  fs:TFormatSettings;
  i:LongInt;
  neg:Boolean;
begin
  Result := false;
  Value := 0;
  s := Trim(Payload);
  if s = '' then exit;

  if (AnsiCompareText(s, 'true')=0) or (AnsiCompareText(s, 'on')=0) or (s='1') then begin
    Value := 1;
    exit(true);
  end;
  if (AnsiCompareText(s, 'false')=0) or (AnsiCompareText(s, 'off')=0) or (s='0') then begin
    Value := 0;
    exit(true);
  end;

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;

  // hexadecimal: 0x1A or $1A
  t := s;
  neg := false;
  if (Length(t)>0) and ((t[1]='$') or ((t[1]='-') and (Length(t)>1) and (t[2]='$'))) then begin
    if t[1]='-' then begin neg := true; Delete(t,1,1); end;
    Delete(t,1,1);
    if t='' then exit;
    Value := 0;
    for i := 1 to Length(t) do begin
      case t[i] of
        '0'..'9': Value := Value*16 + (Ord(t[i])-Ord('0'));
        'a'..'f': Value := Value*16 + (Ord(t[i])-Ord('a')+10);
        'A'..'F': Value := Value*16 + (Ord(t[i])-Ord('A')+10);
      else
        exit;
      end;
    end;
    if neg then Value := -Value;
    exit(true);
  end;

  if TryStrToFloat(String(s), Value, fs) then begin
    Result := true;
    exit;
  end;
  // try "0xFF" style
  if (Length(s)>2) and (s[1]='0') and ((s[2]='x') or (s[2]='X')) then begin
    t := Copy(s, 3, MaxInt);
    neg := false;
    if (Length(t)>0) and (t[1]='-') then begin neg := true; Delete(t,1,1); end;
    Value := 0;
    if t='' then exit;
    for i := 1 to Length(t) do begin
      case t[i] of
        '0'..'9': Value := Value*16 + (Ord(t[i])-Ord('0'));
        'a'..'f': Value := Value*16 + (Ord(t[i])-Ord('a')+10);
        'A'..'F': Value := Value*16 + (Ord(t[i])-Ord('A')+10);
        '_': ; //ignore digit separators
      else
        exit;
      end;
    end;
    if neg then Value := -Value;
    Result := true;
  end;
end;

function MQTTConnAckText(Code:Byte):AnsiString;
begin
  case Code of
    0: Result := 'Connection accepted';
    1: Result := 'Unacceptable protocol version';
    2: Result := 'Identifier rejected';
    3: Result := 'Server unavailable';
    4: Result := 'Bad user name or password';
    5: Result := 'Not authorized';
  else
    Result := 'Unknown return code';
  end;
end;

end.
