{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Driver de protocolo MQTT 3.1.1 para PascalSCADA.)
  @author(PascalSCADA)

  Cada tag tem o seu tópico MQTT no LongAddress. O driver mantém uma conexão
  MQTT própria (não usa CommunicationPort), assina os tópicos dos tags e
  publica os valores escritos.
}
{$ELSE}
{:
  @abstract(MQTT 3.1.1 protocol driver for PascalSCADA.)
  @author(PascalSCADA)

  Each tag carries its MQTT topic in LongAddress. The driver owns the MQTT
  connection (no CommunicationPort needed), subscribes to the tags' topics
  and publishes written values.
}
{$ENDIF}
unit MQTTDriver;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Math, syncobjs, ProtocolDriver, ProtocolTypes, Tag, commtypes,
  PLCTag, PLCTagNumber, PLCBlock, PLCString, PLCStruct,
  MQTTProtocol, MQTTClient, pscadalog, pascalScadaMTPCPU;

type
  {: Gives access to the protected TagType property (same trick as OPCUADriver). }
  TPLCTagAccess = class(TPLCTag);

  {: Cached value of one MQTT topic. Internal. }
  TMQTTItem = class
  public
    Topic:AnsiString;
    Values:TArrayOfDouble;
    Stamp:QWord;
    LastResult:TProtocolIOResult;
    TagCount:Integer;
    Subscribed:Boolean;
    NextSubscribe:QWord;
    constructor Create(const ATopic:AnsiString);
  end;

  {: MQTT protocol driver. }
  TMQTTDriver = class(TProtocolDriver)
  private
    FClient:TMQTTClient;
    FItems:TStringList; // sorted: Topic -> TMQTTItem
    FItemsCS:TCriticalSection;
    FHost:AnsiString;
    FPort:Word;
    FClientID:AnsiString;
    FUserName:AnsiString;
    FPassword:AnsiString;
    FKeepAlive:Word;
    FCleanSession:Boolean;
    FQoS:TMQTTQoS;
    FRetain:Boolean;
    FConnectTimeout:LongInt;
    FWriteTimeout:LongInt;
    FReconnectInterval:LongInt;
    FNextConnect:QWord;
    FConnectResult:TProtocolIOResult;
    FSubsDirty:Boolean;
    FLibUnavailable:Boolean;

    function GetHost:AnsiString;
    procedure SetHost(const AValue:AnsiString);
    procedure SetPort(AValue:Word);
    procedure SetClientID(const AValue:AnsiString);
    procedure SetUserName(const AValue:AnsiString);
    procedure SetPassword(const AValue:AnsiString);
    procedure SetKeepAlive(AValue:Word);
    procedure SetCleanSession(AValue:Boolean);
    procedure SetQoS(AValue:TMQTTQoS);
    procedure SetRetain(AValue:Boolean);
    procedure SetConnectTimeout(AValue:LongInt);
    procedure SetWriteTimeout(AValue:LongInt);
    procedure SetReconnectInterval(AValue:LongInt);
    procedure SettingChanged;
    function TagTopicOf(TagObj:TTag; out Topic:AnsiString):Boolean;
    function FindItem(const Topic:AnsiString):TMQTTItem;
    function GetItem(const Topic:AnsiString):TMQTTItem;
    procedure FreeItemIfUnused(Item:TMQTTItem);
    function EnsureConnected(Now_:QWord):Boolean;
    procedure DropConnection(R:TProtocolIOResult);
    procedure UpdateSubscriptions(Now_:QWord);
    procedure ClientPublish(Sender:TObject; const Pkt:TMQTTPacket);
    procedure ClientLog(Sender:TObject; const Msg:AnsiString);
    function DoReadImpl(const tagrec:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
    function DoWriteImpl(const tagrec:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
    procedure LockItems;
    procedure UnlockItems;
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
    {: True while the MQTT session is established. }
    function Connected:Boolean;
    {: Last connection/protocol error. }
    function LastError:AnsiString;
    {: Publishes an arbitrary payload on a topic (outside the tag scan). }
    function PublishTopic(const Topic, Payload:AnsiString):Boolean;
  published
    {: MQTT broker host name or IP. }
    property Host:AnsiString read GetHost write SetHost;
    {: MQTT broker TCP port (default 1883). }
    property Port:Word read FPort write SetPort default 1883;
    {: MQTT client identifier. Empty = generated automatically. }
    property ClientID:AnsiString read FClientID write SetClientID;
    {: Optional user name. }
    property UserName:AnsiString read FUserName write SetUserName;
    {: Optional password. }
    property Password:AnsiString read FPassword write SetPassword;
    {: MQTT keep alive in seconds (default 60). }
    property KeepAlive:Word read FKeepAlive write SetKeepAlive default 60;
    {: MQTT clean session flag (default true). }
    property CleanSession:Boolean read FCleanSession write SetCleanSession default true;
    {: QoS used for subscriptions and publications (0 or 1). }
    property QoS:TMQTTQoS read FQoS write SetQoS default mqQoS1;
    {: RETAIN flag of published values. }
    property Retain:Boolean read FRetain write SetRetain default false;
    {: TCP connect timeout in milliseconds (default 3000). }
    property ConnectTimeout:LongInt read FConnectTimeout write SetConnectTimeout default 3000;
    {: PUBACK wait timeout in milliseconds (default 3000). }
    property WriteTimeout:LongInt read FWriteTimeout write SetWriteTimeout default 3000;
    {: Delay before retrying a failed connection, in ms (default 3000). }
    property ReconnectInterval:LongInt read FReconnectInterval write SetReconnectInterval default 3000;
    property ReadOnly;
  end;

implementation

constructor TMQTTItem.Create(const ATopic:AnsiString);
begin
  inherited Create;
  Topic := ATopic;
  TagCount := 0;
  Subscribed := false;
  NextSubscribe := 0;
  LastResult := ioNone;
  Stamp := 0;
  SetLength(Values, 0);
end;

{ TMQTTDriver }

constructor TMQTTDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FItemsCS := TCriticalSection.Create;
  FItems := TStringList.Create;
  FItems.Sorted := true;
  FItems.CaseSensitive := true;
  FItems.Duplicates := dupError;
  FClient := TMQTTClient.Create;
  FClient.OnPublish := ClientPublish;
  FClient.OnLog := ClientLog;
  FPort := 1883;
  FKeepAlive := 60;
  FCleanSession := true;
  FQoS := mqQoS1;
  FConnectTimeout := 3000;
  FWriteTimeout := 3000;
  FReconnectInterval := 3000;
  FConnectResult := ioConnectPLCFailed;
  PReadSomethingAlways := true;
  PDeliverReadAhead := true;
end;

destructor TMQTTDriver.Destroy;
var
  i:LongInt;
begin
  inherited Destroy; // stop the scan threads first
  if FClient<>nil then FClient.Disconnect;
  LockItems;
  try
    if FItems<>nil then
      for i:=0 to FItems.Count-1 do
        FItems.Objects[i].Free;
  finally
    UnlockItems;
  end;
  FreeAndNil(FItems);
  FreeAndNil(FClient);
  FreeAndNil(FItemsCS);
end;

function TMQTTDriver.NeedsExternalPort:Boolean;
begin
  Result := false;
end;

function TMQTTDriver.SizeOfTag(aTag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE;
begin
  if aTag is TPLCString then begin
    ProtocolTagType := ptByte;
    exit(8);
  end;
  // values arrive already converted; keep the tag's own type
  ProtocolTagType := ptUnknown;
  Result := 64;
  if aTag is TPLCTag then
    case TPLCTagAccess(aTag).TagType of
      pttShortInt, pttByte:          Result := 8;
      pttSmallInt, pttWord:          Result := 16;
      pttLongInt, pttDWord, pttFloat: Result := 32;
    else
      Result := 64;
    end;
end;

{ settings }

procedure TMQTTDriver.SettingChanged;
begin
  FNextConnect := 0;
  FSubsDirty := true;
  if FClient<>nil then begin
    FClient.UserName := FUserName;
    FClient.Password := FPassword;
    FClient.KeepAlive := FKeepAlive;
    FClient.CleanSession := FCleanSession;
    FClient.ConnectTimeout := FConnectTimeout;
  end;
end;

function TMQTTDriver.GetHost:AnsiString;
begin
  Result := FHost;
end;

procedure TMQTTDriver.SetHost(const AValue:AnsiString);
begin
  if FHost=AValue then exit;
  FHost := AValue;
  if FClient<>nil then begin
    FClient.Disconnect;
    FClient.Host := FHost;
  end;
  SettingChanged;
end;

procedure TMQTTDriver.SetPort(AValue:Word);
begin
  if FPort=AValue then exit;
  FPort := AValue;
  if FClient<>nil then begin
    FClient.Disconnect;
    FClient.Port := FPort;
  end;
  SettingChanged;
end;

procedure TMQTTDriver.SetClientID(const AValue:AnsiString);
begin
  if FClientID=AValue then exit;
  FClientID := AValue;
  if FClient<>nil then begin
    FClient.Disconnect;
    FClient.ClientID := FClientID;
  end;
  SettingChanged;
end;

procedure TMQTTDriver.SetUserName(const AValue:AnsiString);
begin
  if FUserName=AValue then exit;
  FUserName := AValue;
  if FClient<>nil then begin
    FClient.Disconnect;
    FClient.UserName := FUserName;
  end;
  SettingChanged;
end;

procedure TMQTTDriver.SetPassword(const AValue:AnsiString);
begin
  if FPassword=AValue then exit;
  FPassword := AValue;
  if FClient<>nil then begin
    FClient.Disconnect;
    FClient.Password := FPassword;
  end;
  SettingChanged;
end;

procedure TMQTTDriver.SetKeepAlive(AValue:Word);
begin
  if FKeepAlive=AValue then exit;
  FKeepAlive := AValue;
  SettingChanged;
end;

procedure TMQTTDriver.SetCleanSession(AValue:Boolean);
begin
  if FCleanSession=AValue then exit;
  FCleanSession := AValue;
  SettingChanged;
end;

procedure TMQTTDriver.SetQoS(AValue:TMQTTQoS);
begin
  if FQoS=AValue then exit;
  if AValue=mqQoS2 then exit; // QoS2 not implemented in this version
  FQoS := AValue;
  FSubsDirty := true;
end;

procedure TMQTTDriver.SetRetain(AValue:Boolean);
begin
  FRetain := AValue;
end;

procedure TMQTTDriver.SetConnectTimeout(AValue:LongInt);
begin
  if AValue<100 then AValue := 100;
  FConnectTimeout := AValue;
  SettingChanged;
end;

procedure TMQTTDriver.SetWriteTimeout(AValue:LongInt);
begin
  if AValue<100 then AValue := 100;
  FWriteTimeout := AValue;
end;

procedure TMQTTDriver.SetReconnectInterval(AValue:LongInt);
begin
  if AValue<100 then AValue := 100;
  FReconnectInterval := AValue;
end;

{ items }

procedure TMQTTDriver.LockItems;
begin
  FItemsCS.Enter;
end;

procedure TMQTTDriver.UnlockItems;
begin
  FItemsCS.Leave;
end;

function TMQTTDriver.TagTopicOf(TagObj:TTag; out Topic:AnsiString):Boolean;
begin
  Topic := '';
  if TagObj is TPLCStruct then exit(false);
  if TagObj is TPLCTagNumber then
    Topic := TPLCTagNumber(TagObj).LongAddress
  else if TagObj is TPLCBlock then
    Topic := TPLCBlock(TagObj).LongAddress
  else if TagObj is TPLCString then
    Topic := TPLCString(TagObj).LongAddress;
  Topic := Trim(Topic);
  Result := Topic<>'';
end;

function TMQTTDriver.FindItem(const Topic:AnsiString):TMQTTItem;
var
  i:LongInt;
begin
  LockItems;
  try
    if FItems.Find(Trim(Topic), i) then
      Result := TMQTTItem(FItems.Objects[i])
    else
      Result := nil;
  finally
    UnlockItems;
  end;
end;

function TMQTTDriver.GetItem(const Topic:AnsiString):TMQTTItem;
var
  t:AnsiString;
  i:LongInt;
begin
  t := Trim(Topic);
  LockItems;
  try
    if FItems.Find(t, i) then begin
      Result := TMQTTItem(FItems.Objects[i]);
      exit;
    end;
    Result := TMQTTItem.Create(t);
    FItems.AddObject(t, Result);
  finally
    UnlockItems;
  end;
  FSubsDirty := true;
end;

procedure TMQTTDriver.FreeItemIfUnused(Item:TMQTTItem);
var
  i:LongInt;
begin
  if (Item=nil) or (Item.TagCount>0) then exit;
  LockItems;
  try
    if FItems.Find(Item.Topic, i) then FItems.Delete(i);
  finally
    UnlockItems;
  end;
  Item.Free;
  FSubsDirty := true;
end;

procedure TMQTTDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
var
  topic:AnsiString;
  item:TMQTTItem;
  ok:Boolean;
begin
  ok := TagTopicOf(TagObj, topic);
  if ok then begin
    item := GetItem(topic);
    Inc(item.TagCount);
    item.NextSubscribe := 0;
  end;
  inherited DoAddTag(TagObj, ok);
end;

procedure TMQTTDriver.DoDelTag(TagObj:TTag);
var
  topic:AnsiString;
  item:TMQTTItem;
begin
  if TagTopicOf(TagObj, topic) then begin
    item := FindItem(topic);
    if item<>nil then begin
      Dec(item.TagCount);
      FreeItemIfUnused(item);
    end;
  end;
  inherited DoDelTag(TagObj);
end;

{ connection }

procedure TMQTTDriver.ClientLog(Sender:TObject; const Msg:AnsiString);
begin
  if PSLogWants(lcProtocol, llInfo) then
    PSLog(Self, lcProtocol, llInfo, 'MQTT: '+String(Msg));
end;

procedure TMQTTDriver.DropConnection(R:TProtocolIOResult);
var
  i:LongInt;
  item:TMQTTItem;
begin
  if FClient<>nil then FClient.Disconnect;
  LockItems;
  try
    for i:=0 to FItems.Count-1 do begin
      item := TMQTTItem(FItems.Objects[i]);
      item.Subscribed := false;
      item.NextSubscribe := 0;
      if item.TagCount>0 then item.LastResult := R;
    end;
  finally
    UnlockItems;
  end;
  FConnectResult := R;
  FSubsDirty := true;
  FNextConnect := GetTickCount64 + QWord(Max(100, FReconnectInterval));
end;

function TMQTTDriver.EnsureConnected(Now_:QWord):Boolean;
begin
  if FLibUnavailable then exit(false);
  if FClient=nil then exit(false);
  if FClient.Connected then exit(true);
  if Now_<FNextConnect then exit(false);

  FClient.Host := FHost;
  FClient.Port := FPort;
  FClient.ClientID := FClientID;
  FClient.UserName := FUserName;
  FClient.Password := FPassword;
  FClient.KeepAlive := FKeepAlive;
  FClient.CleanSession := FCleanSession;
  FClient.ConnectTimeout := FConnectTimeout;

  if not FClient.Connect then begin
    FConnectResult := ioConnectPLCFailed;
    FNextConnect := GetTickCount64 + QWord(Max(100, FReconnectInterval));
    exit(false);
  end;
  FConnectResult := ioOk;
  FSubsDirty := true;
  Result := true;
end;

procedure TMQTTDriver.UpdateSubscriptions(Now_:QWord);
var
  topics:array of AnsiString;
  i, n:LongInt;
  item:TMQTTItem;
begin
  if not FSubsDirty then exit;
  if FClient=nil then exit;
  LockItems;
  try
    n := 0;
    SetLength(topics, FItems.Count);
    for i:=0 to FItems.Count-1 do begin
      item := TMQTTItem(FItems.Objects[i]);
      if item.TagCount<=0 then continue;
      if (not item.Subscribed) or (item.NextSubscribe<=Now_) then begin
        topics[n] := item.Topic;
        Inc(n);
      end;
    end;
  finally
    UnlockItems;
  end;
  if n=0 then begin FSubsDirty := false; exit; end;
  SetLength(topics, n);

  if not FClient.Subscribe(topics, FQoS) then begin
    DropConnection(ioCommError);
    exit;
  end;
  LockItems;
  try
    for i:=0 to FItems.Count-1 do begin
      item := TMQTTItem(FItems.Objects[i]);
      item.Subscribed := true;
      item.NextSubscribe := Now_ + 60000; // renew every minute
    end;
  finally
    UnlockItems;
  end;
  FSubsDirty := false;
end;

procedure TMQTTDriver.ClientPublish(Sender:TObject; const Pkt:TMQTTPacket);
var
  item:TMQTTItem;
  i:LongInt;
  v:Double;
  txt:AnsiString;
begin
  item := FindItem(Pkt.Topic);
  if item=nil then exit;
  SetLength(item.Values, 0);
  if MQTTPayloadToDouble(Pkt.Payload, v) then begin
    SetLength(item.Values, 1);
    item.Values[0] := v;
    item.LastResult := ioOk;
  end else begin
    // text payload: store bytes as UTF-8 code units for TPLCString tags
    txt := Pkt.Payload;
    SetLength(item.Values, Length(txt));
    for i:=1 to Length(txt) do
      item.Values[i-1] := Ord(txt[i]);
    if Length(txt)=0 then exit;
    item.LastResult := ioOk;
  end;
  item.Stamp := GetTickCount64;
end;

{ scan }

procedure TMQTTDriver.DoScanRead(Sender:TObject; var NeedSleep:LongInt);
var
  now_:QWord;
  used:LongInt;
  i:LongInt;
begin
  try
    if [csDestroying]*ComponentState<>[] then begin
      CrossThreadSwitch;
      exit;
    end;

    used := 0;
    LockItems;
    try
      for i:=0 to FItems.Count-1 do
        if TMQTTItem(FItems.Objects[i]).TagCount>0 then Inc(used);
    finally
      UnlockItems;
    end;

    if used=0 then begin
      NeedSleep := 20;
      exit;
    end;

    now_ := GetTickCount64;
    if not EnsureConnected(now_) then begin
      NeedSleep := 20;
      exit;
    end;
    UpdateSubscriptions(now_);
    if FClient=nil then exit;
    FClient.Pump;

    if not FClient.Connected then begin
      DropConnection(ioCommError);
      NeedSleep := 20;
      exit;
    end;
    NeedSleep := 1;
  except
    on e:Exception do begin
      NeedSleep := 20;
    end;
  end;
  FProtocolReady := true;
end;

procedure TMQTTDriver.DoGetValue(TagRec:TTagRec; var values:TScanReadRec);
var
  item:TMQTTItem;
  n, i, off:LongInt;
begin
  SetLength(values.Values, 0);
  item := FindItem(TagRec.Path);
  if item=nil then begin
    values.LastQueryResult := ioNullDriver;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
    exit;
  end;
  values.ClkMonotonicTStamp := item.Stamp;
  values.Offset := 0;
  values.RealOffset := 0;
  if item.LastResult<>ioOk then begin
    values.LastQueryResult := item.LastResult;
    values.ReadsOK := 0;
    values.ReadFaults := 1;
    exit;
  end;
  n := TagRec.Size;
  if n<=0 then n := Length(item.Values);
  off := Max(0, TagRec.OffSet);
  SetLength(values.Values, n);
  for i:=0 to n-1 do
    if off+i<Length(item.Values) then
      values.Values[i] := item.Values[off+i]
    else
      values.Values[i] := 0;
  values.LastQueryResult := ioOk;
  values.ReadsOK := 1;
  values.ReadFaults := 0;
end;

function TMQTTDriver.DoRead(const tagrec:TTagRec; out Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  Result := DoReadImpl(tagrec, Values);
end;

function TMQTTDriver.DoReadImpl(const tagrec:TTagRec; out Values:TArrayOfDouble):TProtocolIOResult;
var
  item:TMQTTItem;
  now_:QWord;
begin
  SetLength(Values, 0);
  if not EnsureConnected(GetTickCount64) then exit(FConnectResult);
  now_ := GetTickCount64;
  UpdateSubscriptions(now_);
  if FClient=nil then exit(ioNullDriver);
  item := GetItem(tagrec.Path);
  FClient.Pump;
  if item.LastResult=ioOk then begin
    Values := Copy(item.Values);
    Result := ioOk;
  end else
    Result := item.LastResult;
  FreeItemIfUnused(item);
end;

function TMQTTDriver.DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;
begin
  Result := DoWriteImpl(tagrec, Values);
end;

function TMQTTDriver.DoWriteImpl(const tagrec:TTagRec; const Values:TArrayOfDouble):TProtocolIOResult;
var
  payload, txt:AnsiString;
  i:LongInt;
  allText:Boolean;
  item:TMQTTItem;
  v:Double;
begin
  if not EnsureConnected(GetTickCount64) then exit(FConnectResult);
  if FClient=nil then exit(ioNullDriver);
  if Length(Values)=0 then exit(ioIllegalValue);

  // TPLCString tags hand their value as an array of byte codes. If all codes
  // are printable characters, publish them as UTF-8 text; otherwise publish
  // the numeric representation.
  txt := '';
  allText := Length(Values)>0;
  for i:=0 to High(Values) do begin
    v := Values[i];
    if (v<32) or (v>126) then allText := false;
    txt := txt + AnsiChar(Byte(Round(v) and $FF));
  end;

  if allText and (Length(txt)>1) then
    payload := txt
  else begin
    payload := '';
    for i:=0 to High(Values) do begin
      if i>0 then payload := payload + ',';
      payload := payload + AnsiString(FloatToStr(Values[i], DefaultFormatSettings));
    end;
  end;

  if FClient.Publish(TagRec.Path, payload, FQoS, FRetain, FWriteTimeout) then begin
    Result := ioOk;
    // update local cache so the tag reflects the write immediately
    item := GetItem(TagRec.Path);
    SetLength(item.Values, 0);
    if MQTTPayloadToDouble(payload, v) then begin
      SetLength(item.Values, 1);
      item.Values[0] := v;
    end else begin
      SetLength(item.Values, Length(payload));
      for i:=1 to Length(payload) do
        item.Values[i-1] := Ord(payload[i]);
    end;
    item.LastResult := ioOk;
    item.Stamp := GetTickCount64;
  end else
    Result := ioCommError;
end;

function TMQTTDriver.Connected:Boolean;
begin
  Result := (FClient<>nil) and FClient.Connected;
end;

function TMQTTDriver.LastError:AnsiString;
begin
  if FClient=nil then Result := 'MQTT: client not created'
  else Result := FClient.LastError;
end;

function TMQTTDriver.PublishTopic(const Topic, Payload:AnsiString):Boolean;
begin
  Result := false;
  if not EnsureConnected(GetTickCount64) then exit;
  if FClient=nil then exit;
  Result := FClient.Publish(Topic, Payload, FQoS, FRetain, FWriteTimeout);
end;

end.
