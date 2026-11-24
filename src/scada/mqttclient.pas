{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Cliente MQTT 3.1.1 sobre TCP, usado pelo TMQTTDriver.)
  @author(PascalSCADA)
}
{$ELSE}
{:
  @abstract(MQTT 3.1.1 TCP client used by TMQTTDriver.)
  @author(PascalSCADA)

  The client owns the socket and is pumped by the protocol driver scan thread.
  All socket I/O is serialized by an internal critical section.
}
{$ENDIF}
unit MQTTClient;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, Classes, Math, syncobjs, commtypes, MQTTProtocol
  {$IFDEF FPC}
  , ssockets
  {$ENDIF}
  ;

type
  TMQTTPublishEvent = procedure(Sender:TObject; const Pkt:TMQTTPacket) of object;
  TMQTTLogEvent = procedure(Sender:TObject; const Msg:AnsiString) of object;

  {: MQTT 3.1.1 client. Connect/Pump/Publish are all protected by FCS. }
  TMQTTClient = class
  private
    FCS:TCriticalSection;
    FDecoder:TMQTTDecoder;
    {$IFDEF FPC}
    FSocket:TInetSocket;
    {$ENDIF}
    FHost:AnsiString;
    FPort:Word;
    FClientID:AnsiString;
    FUserName:AnsiString;
    FPassword:AnsiString;
    FKeepAlive:Word;
    FCleanSession:Boolean;
    FConnectTimeout:LongInt;
    FIOTimeout:LongInt;
    FLoggedIn:Boolean;
    FLastRecv:QWord;
    FLastSend:QWord;
    FPingOutstanding:Boolean;
    FNextPacketID:Word;
    FReadErrors:LongInt;
    FLastError:AnsiString;
    FOnPublish:TMQTTPublishEvent;
    FOnLog:TMQTTLogEvent;

    function SendRaw(const Data:BYTES):Boolean;
    procedure Log(const Msg:AnsiString);
    function NextPacketID:Word;
    procedure HandlePacket(const Pkt:TMQTTPacket);
    function ReadSome(WaitMs:LongInt; out GotData:Boolean):Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    {: Opens the TCP connection and performs the MQTT CONNECT handshake. }
    function Connect:Boolean;
    {: Sends DISCONNECT and closes the socket. }
    procedure Disconnect;
    {: True when the MQTT session is established. }
    function Connected:Boolean;
    {: Called by the driver scan thread: reads, parses and keeps the session alive. }
    procedure Pump;
    {: Subscribes to topics. Returns false on socket error. }
    function Subscribe(const Topics:array of AnsiString; QoS:TMQTTQoS):Boolean;
    {: Publishes a payload. For QoS1 waits for PUBACK up to TimeoutMs. }
    function Publish(const Topic, Payload:AnsiString; QoS:TMQTTQoS; Retain:Boolean; TimeoutMs:LongInt):Boolean;

    property Host:AnsiString read FHost write FHost;
    property Port:Word read FPort write FPort;
    property ClientID:AnsiString read FClientID write FClientID;
    property UserName:AnsiString read FUserName write FUserName;
    property Password:AnsiString read FPassword write FPassword;
    property KeepAlive:Word read FKeepAlive write FKeepAlive;
    property CleanSession:Boolean read FCleanSession write FCleanSession;
    property ConnectTimeout:LongInt read FConnectTimeout write FConnectTimeout;
    property IOTimeout:LongInt read FIOTimeout write FIOTimeout;
    property LastError:AnsiString read FLastError;
    property OnPublish:TMQTTPublishEvent read FOnPublish write FOnPublish;
    property OnLog:TMQTTLogEvent read FOnLog write FOnLog;
  end;

implementation

constructor TMQTTClient.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FDecoder := TMQTTDecoder.Create;
  FPort := 1883;
  FClientID := '';
  FKeepAlive := 60;
  FCleanSession := true;
  FConnectTimeout := 3000;
  FIOTimeout := 10;
  FNextPacketID := 1;
end;

destructor TMQTTClient.Destroy;
begin
  Disconnect;
  FreeAndNil(FDecoder);
  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TMQTTClient.Log(const Msg:AnsiString);
begin
  if Assigned(FOnLog) then FOnLog(Self, Msg);
end;

function TMQTTClient.NextPacketID:Word;
begin
  Result := FNextPacketID;
  Inc(FNextPacketID);
  if FNextPacketID = 0 then FNextPacketID := 1;
end;

function TMQTTClient.Connected:Boolean;
begin
  FCS.Enter;
  try
    Result := FLoggedIn and Assigned(FSocket);
  finally
    FCS.Leave;
  end;
end;

function TMQTTClient.SendRaw(const Data:BYTES):Boolean;
var
  n, sent:LongInt;
begin
  Result := false;
  if (FSocket=nil) or (Length(Data)=0) then exit;
  n := Length(Data);
  sent := FSocket.Write(Data[0], n);
  if sent = n then begin
    FLastSend := GetTickCount64;
    Result := true;
  end else begin
    FLastError := 'MQTT: socket write failed ('+IntToStr(FSocket.LastError)+')';
    Log(FLastError);
  end;
end;

function TMQTTClient.ReadSome(WaitMs:LongInt; out GotData:Boolean):Boolean;
var
  buf:array[0..4095] of Byte;
  n:LongInt;
begin
  GotData := false;
  Result := false;
  if FSocket=nil then exit;
  if WaitMs>0 then FSocket.IOTimeout := WaitMs;
  n := FSocket.Read(buf[0], SizeOf(buf));
  if n>0 then begin
    FDecoder.AddRaw(buf[0], n);
    FLastRecv := GetTickCount64;
    FReadErrors := 0;
    GotData := true;
    Result := true;
  end else if n=0 then begin
    // orderly shutdown by peer
    FLastError := 'MQTT: broker closed the connection';
  end else begin
    // timeout or socket error: count consecutive failures
    Inc(FReadErrors);
    Result := true; // not fatal by itself
    if FReadErrors>=20 then begin
      FLastError := 'MQTT: too many read errors ('+IntToStr(FSocket.LastError)+')';
      Result := false;
    end;
  end;
end;

procedure TMQTTClient.HandlePacket(const Pkt:TMQTTPacket);
var
  ack:BYTES;
begin
  case Pkt.PktType of
    mpCONNACK:
      if Pkt.ConnAckCode<>0 then begin
        FLastError := 'MQTT: CONNACK '+IntToStr(Pkt.ConnAckCode)+' ('+String(MQTTConnAckText(Pkt.ConnAckCode))+')';
        Log(FLastError);
      end else begin
        FLoggedIn := true;
        Log('MQTT: connected to '+String(FHost)+':'+IntToStr(FPort));
      end;
    mpPUBLISH: begin
      if Assigned(FOnPublish) then FOnPublish(Self, Pkt);
      if Pkt.QoS=mqQoS1 then begin
        ack := MQTTEncodePubAck(Pkt.PacketID);
        SendRaw(ack);
      end;
    end;
    mpPINGRESP: FPingOutstanding := false;
    mpDISCONNECT: begin
      FLastError := 'MQTT: broker sent DISCONNECT';
      FLoggedIn := false;
    end;
  end;
end;

procedure TMQTTClient.Pump;
var
  pkt:TMQTTPacket;
  consumed:LongInt;
  ok, got:Boolean;
  keep:QWord;
begin
  FCS.Enter;
  try
    if FSocket=nil then exit;
    ok := ReadSome(FIOTimeout, got);
    if not ok then begin
      Log(FLastError);
      Disconnect;
      exit;
    end;
    while FDecoder.NextPacket(pkt, consumed) do
      HandlePacket(pkt);

    if not FLoggedIn then begin
      // CONNACK did not arrive in time
      if (FLastSend>0) and (GetTickCount64-FLastSend>Max(3000, FConnectTimeout)) then begin
        FLastError := 'MQTT: CONNACK timeout';
        Log(FLastError);
        Disconnect;
      end;
      exit;
    end;

    keep := QWord(FKeepAlive)*500;
    if keep<1000 then keep := 1000;
    if FPingOutstanding then begin
      if (FLastRecv>0) and (GetTickCount64-FLastRecv>QWord(FKeepAlive)*1000+FIOTimeout) then begin
        FLastError := 'MQTT: PINGRESP timeout';
        Log(FLastError);
        Disconnect;
      end;
    end else if (FLastSend>0) and (GetTickCount64-FLastSend>keep) then begin
      FPingOutstanding := true;
      SendRaw(MQTTEncodePingReq);
    end;

    if (FLastRecv>0) and (GetTickCount64-FLastRecv>QWord(FKeepAlive)*1500) then begin
      FLastError := 'MQTT: no traffic from broker';
      Log(FLastError);
      Disconnect;
    end;
  finally
    FCS.Leave;
  end;
end;

function TMQTTClient.Connect:Boolean;
var
  opt:TMQTTConnectOptions;
  pkt:TMQTTPacket;
  consumed:LongInt;
  deadline:QWord;
  ok, got:Boolean;
begin
  Result := false;
  FCS.Enter;
  try
    if FLoggedIn and Assigned(FSocket) then exit(true);
    Disconnect;
    if Trim(String(FHost))='' then begin
      FLastError := 'MQTT: empty broker host';
      exit;
    end;
    if FClientID='' then
      FClientID := 'PascalSCADA_'+IntToStr(GetTickCount64 and $FFFFFF);
    try
      {$IFDEF FPC}
      FSocket := TInetSocket.Create(String(FHost), FPort, FConnectTimeout);
      FSocket.IOTimeout := FConnectTimeout;
      {$ELSE}
      FLastError := 'MQTT: this unit requires FPC ssockets';
      exit;
      {$ENDIF}
    except
      on e:Exception do begin
        FLastError := 'MQTT: connect to '+String(FHost)+':'+IntToStr(FPort)+' failed: '+e.Message;
        Log(FLastError);
        FSocket := nil;
        exit;
      end;
    end;

    opt.ClientID := FClientID;
    opt.UserName := FUserName;
    opt.Password := FPassword;
    opt.KeepAliveSec := FKeepAlive;
    opt.CleanSession := FCleanSession;

    FLoggedIn := false;
    FPingOutstanding := false;
    FLastRecv := GetTickCount64;
    FDecoder.Clear;
    if not SendRaw(MQTTEncodeConnect(opt)) then exit;

    deadline := GetTickCount64 + QWord(Max(1000, FConnectTimeout));
    while (GetTickCount64<deadline) and (not FLoggedIn) do begin
      ok := ReadSome(50, got);
      if not ok then break;
      while FDecoder.NextPacket(pkt, consumed) do
        HandlePacket(pkt);
    end;
    Result := FLoggedIn;
    if not Result and (FLastError='') then FLastError := 'MQTT: CONNACK timeout';
  finally
    FCS.Leave;
  end;
end;

procedure TMQTTClient.Disconnect;
begin
  FCS.Enter;
  try
    if Assigned(FSocket) then begin
      try
        if FLoggedIn then SendRaw(MQTTEncodeDisconnect);
      except
        // ignore errors while shutting down
      end;
      FreeAndNil(FSocket);
    end;
    FLoggedIn := false;
    FPingOutstanding := false;
    FDecoder.Clear;
  finally
    FCS.Leave;
  end;
end;

function TMQTTClient.Subscribe(const Topics:array of AnsiString; QoS:TMQTTQoS):Boolean;
var
  id:Word;
  pkt:TMQTTPacket;
  consumed:LongInt;
  deadline:QWord;
  ok, got:Boolean;
  ackOk:Boolean;
begin
  Result := false;
  if Length(Topics)=0 then exit(true);
  FCS.Enter;
  try
    if not FLoggedIn then begin FLastError := 'MQTT: not connected'; exit; end;
    id := NextPacketID;
    if not SendRaw(MQTTEncodeSubscribe(id, Topics, QoS)) then exit;

    ackOk := false;
    deadline := GetTickCount64 + QWord(Max(1000, FConnectTimeout));
    while (GetTickCount64<deadline) and (not ackOk) do begin
      ok := ReadSome(50, got);
      if not ok then break;
      while FDecoder.NextPacket(pkt, consumed) do begin
        HandlePacket(pkt);
        if (pkt.PktType=mpSUBACK) and (pkt.PacketID=id) then begin
          ackOk := true;
          if Length(pkt.SubAckCodes)>0 then
            if pkt.SubAckCodes[0]=$80 then begin
              FLastError := 'MQTT: SUBACK rejected the subscription';
              exit;
            end;
        end;
      end;
    end;
    Result := ackOk;
    if not Result then FLastError := 'MQTT: SUBACK timeout';
  finally
    FCS.Leave;
  end;
end;

function TMQTTClient.Publish(const Topic, Payload:AnsiString; QoS:TMQTTQoS;
  Retain:Boolean; TimeoutMs:LongInt):Boolean;
var
  id:Word;
  pkt:TMQTTPacket;
  consumed:LongInt;
  deadline:QWord;
  ok, got:Boolean;
  acked:Boolean;
begin
  Result := false;
  FCS.Enter;
  try
    if not FLoggedIn then begin FLastError := 'MQTT: not connected'; exit; end;
    id := NextPacketID;
    if not SendRaw(MQTTEncodePublish(Topic, Payload, QoS, Retain, id)) then exit;
    if QoS=mqQoS0 then exit(true);

    acked := false;
    deadline := GetTickCount64 + QWord(Max(100, TimeoutMs));
    while (GetTickCount64<deadline) and (not acked) do begin
      ok := ReadSome(20, got);
      if not ok then break;
      while FDecoder.NextPacket(pkt, consumed) do begin
        HandlePacket(pkt);
        if (pkt.PktType=mpPUBACK) and (pkt.PacketID=id) then acked := true;
      end;
    end;
    Result := acked;
    if not Result then FLastError := 'MQTT: PUBACK timeout';
  finally
    FCS.Leave;
  end;
end;

end.
