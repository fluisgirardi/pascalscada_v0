{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Alarmes ativos do CLP S7-1200/1500 (S7CommPlus): resposta do EXPLORE do subsistema de alarmes.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Esta unit não fala com o CLP: ela monta a lista de atributos do EXPLORE e decodifica
  a resposta (S7PlusParseAlarmExplore), então é testável sem CLP. Quem envia o pedido é
  TS7PlusConnection.ReadActiveAlarms / TS7CommPlusDriver.ReadActiveAlarms.

  Cada alarme é um objeto DAI (Device Alarm Information) do subsistema de alarmes
  (RID 8, classe 2681): ID na CPU, domínio, tipo, contador de sequência, o evento
  (chegou "Coming" ou saiu "Going", com horário e horário do reconhecimento), os
  valores associados (SD_1..SD_10), o HmiInfo (prioridade, classe, grupo...) e os
  textos em cada idioma (ID 1031 = de-DE, 1033 = en-US, 1046 = pt-BR...). Os textos
  podem citar os valores associados como @1%d@ (índice e formato printf), trocados
  aqui pelo valor.

  O formato segue as bibliotecas de referência S7CommPlusDriver (.NET) e s7commplus (Python).
}
{$ELSE}
{:
  @abstract(Active alarms of an S7-1200/1500 PLC (S7CommPlus): the reply of the EXPLORE of the alarm subsystem.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This unit does not talk to the PLC: it gives the attribute list of the EXPLORE and
  decodes the reply (S7PlusParseAlarmExplore), so it can be tested without a PLC. The
  request is sent by TS7PlusConnection.ReadActiveAlarms / TS7CommPlusDriver.ReadActiveAlarms.

  Each alarm is a DAI (Device Alarm Information) object of the alarm subsystem
  (RID 8, class 2681): CPU alarm ID, domain, type, sequence counter, the event
  (arrived "Coming" or left "Going", with its time and the acknowledge time), the
  associated values (SD_1..SD_10), the HmiInfo (priority, class, group...) and the
  texts in each language (ID 1031 = de-DE, 1033 = en-US, 1046 = pt-BR...). The texts may
  quote the associated values as @1%d@ (index and printf format), replaced here by the
  value.

  The format follows the reference libraries S7CommPlusDriver (.NET) and s7commplus (Python).
}
{$ENDIF}
unit S7PlusAlarms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, S7PlusTypes, S7PlusVLQ, S7PlusCodec;

const
  S7PlusIds_AlarmSubsystemItsUpdateRelevantDAI = 2667;
  S7PlusIds_DAI_CPUAlarmID       = 2670;
  S7PlusIds_DAI_AllStatesInfo    = 2671;
  S7PlusIds_DAI_AlarmDomain      = 2672;
  S7PlusIds_DAI_Coming           = 2673;
  S7PlusIds_DAI_Going            = 2677;
  S7PlusIds_DAI_ClassRID         = 2681;
  S7PlusIds_DAI_AlarmTexts       = 2715;
  S7PlusIds_DAI_MessageType      = 4079;
  S7PlusIds_DAI_HmiInfo          = 7813;
  S7PlusIds_DAI_SequenceCounter  = 7917;
  S7PlusIds_AS_CGS_AllStatesInfo = 3474;
  S7PlusIds_AS_CGS_Timestamp     = 3475;
  S7PlusIds_AS_CGS_AssocValues   = 3476;
  S7PlusIds_AS_CGS_AckTimestamp  = 3646;

  //: Ids of the alarm subscription (a subscription object with an AlarmSubscriptionRef child).
  S7PlusIds_SubscriptionReferenceTriggerAndTransmitMode = 1005;
  S7PlusIds_SubscriptionDelayTime = 1050;
  S7PlusIds_AlarmSubscriptionRef_AlarmDomain = 2659;
  S7PlusIds_AlarmSubscriptionRef_ItsAlarmSubsystem = 2660;
  S7PlusIds_AlarmSubscriptionRef_ClassRID = 2662;
  S7PlusIds_AlarmSubscriptionRef_AlarmDomainFilter = 7731;
  S7PlusIds_AlarmSubscriptionRef_SendAlarmTexts = 8173;
  S7PlusIds_AlarmSubscriptionRef_AlarmTextLanguages = 8181;

  //: Language IDs (Windows LCID) used by the alarm texts.
  S7PlusLang_German_DE    = 1031;
  S7PlusLang_English_US   = 1033;
  S7PlusLang_Spanish      = 1034;
  S7PlusLang_French_FR    = 1036;
  S7PlusLang_Italian      = 1040;
  S7PlusLang_Portuguese_BR = 1046;

type
  TS7PlusAlarmEvent = (aeUnknown, aeComing, aeGoing);

  TS7PlusAlarmText = record
    LanguageId:Cardinal;
    InfoText:UTF8String;
    AlarmText:UTF8String;
    //: Additional texts 1..9 (text ids 3..11), index 0..8.
    AdditionalText:array[0..8] of UTF8String;
  end;
  TS7PlusAlarmTextArray = array of TS7PlusAlarmText;

  TS7PlusAlarmHmiInfo = record
    Present:Boolean;
    SyntaxId:Word;
    Version:Word;
    ClientAlarmId:Cardinal;
    Priority:Byte;
    //: Only from SyntaxId 258.
    AlarmClass:Word;
    Producer:Byte;
    GroupId:Byte;
    Flags:Byte;
  end;

  TS7PlusAlarm = record
    //: Object name in the PLC (ObjectVariableTypeName).
    Name:UTF8String;
    CpuAlarmId:QWord;
    AllStatesInfo:Byte;
    Domain:Word;
    MessageType:LongInt;
    SequenceCounter:Cardinal;
    Event:TS7PlusAlarmEvent;
    //: When the alarm came/went and when it was acknowledged: nanoseconds since 1970-01-01 UTC
    //: (0 = none), and the same as TDateTime (UTC, 0 = none).
    TimestampNs:QWord;
    Timestamp:TDateTime;
    AckTimestampNs:QWord;
    AckTimestamp:TDateTime;
    HmiInfo:TS7PlusAlarmHmiInfo;
    //: SD_1..SD_10 (index 1..10; index 0 is the type information and stays empty), already as text.
    AssociatedValues:array of UTF8String;
    //: One entry per language sent by the PLC (or per language asked for).
    Texts:TS7PlusAlarmTextArray;
  end;
  TS7PlusAlarmArray = array of TS7PlusAlarm;

//: The attribute IDs asked for by the EXPLORE of the active alarms (the same a notification carries).
function S7PlusAlarmExploreAttributes:TCardinalDynArray;

//: Decodes the payload of the EXPLORE reply (from the ReturnValue on). Languages is the list of
//: languages to keep the texts of (empty = all of them). False when the reply is an error (ReturnValue
//: is not 0) or is not well formed; no alarms at all is True with an empty list.
function S7PlusParseAlarmExplore(const Response:TBytes; const Languages:array of Cardinal; out Alarms:TS7PlusAlarmArray):Boolean;

//: The object of an alarm subscription (what goes in the CreateObject after the envelope): a
//: subscription (function class 2, route mode 2) with an AlarmSubscriptionRef child that has the
//: relation to the alarm subsystem. With SendTextsAndFilter the child also asks for the texts of the
//: Languages (empty = every language) and filters by Domains (empty = all, 65535); S7-1200 without TLS
//: refuse those attributes - leave it False for them (the texts are then read with ReadActiveAlarms).
function S7PlusBuildAlarmSubscriptionObject(RelationId:Cardinal; CreditLimit:SmallInt;
  SendTextsAndFilter:Boolean; const Languages:array of Cardinal; const Domains:array of Word):TBytes;

//: Decodes an alarm notification (Frame = the frame body from the Opcode byte on, like
//: TS7PlusConnection.ParseNotification takes). True only when the frame is a notification that carries the
//: alarm part (the objects after the data item list); a notification without it, or one that mixes data items
//: with alarms, gives False. SubscriptionId is the subscription the PLC sent it for (taken from the alarm
//: part when the header has 0), CreditTick the notification counter used to renew the credit.
function S7PlusParseAlarmNotification(const Frame:TBytes; const Languages:array of Cardinal;
  out SubscriptionId:Cardinal; out CreditTick:Byte; out Alarms:TS7PlusAlarmArray):Boolean;

//: The text of one language of an alarm (the first one, when Language is 0 or is not there).
function S7PlusAlarmTextOf(const Alarm:TS7PlusAlarm; Language:Cardinal; out Text:TS7PlusAlarmText):Boolean;

//: Replaces the @n%fmt@ tokens of a text (n = index of the associated value, fmt = printf format)
//: by the value; a token that cannot be resolved stays as it is.
function S7PlusInterpolateAssociatedValues(const Text:UTF8String; const Values:array of UTF8String):UTF8String;

//: Converts nanoseconds since 1970-01-01 UTC to TDateTime (0 stays 0).
function S7PlusTimestampToDateTime(Ns:QWord):TDateTime;

//: A short description ("coming/going", sequence, ID, the text of the language) for logs.
function S7PlusAlarmToString(const Alarm:TS7PlusAlarm; Language:Cardinal=0):String;

implementation

type
  TAlarmValueKind = (vkNull, vkNumber, vkText, vkBlob, vkStruct, vkList);

  //The decoded value of an attribute: a number (Num; Signed is the same value sign-extended for DINT/LINT/...),
  //a text, a blob (RootId + Bytes), or a list of children (struct members, sparse array, array) with their keys.
  TAlarmValue = class
    Kind:TAlarmValueKind;
    Num:QWord;
    Signed:Int64;
    Real:Double;
    IsReal:Boolean;
    Text:UTF8String;
    RootId:Cardinal;
    Bytes:TBytes;
    Keys:array of Cardinal;
    Items:array of TAlarmValue;
    destructor Destroy; override;
    procedure Add(AKey:Cardinal; AItem:TAlarmValue);
    function Find(AKey:Cardinal):TAlarmValue;
  end;

  TAlarmObject = class
    RelationId:Cardinal;
    ClassId:Cardinal;
    AttrIds:array of Cardinal;
    AttrValues:array of TAlarmValue;
    Children:array of TAlarmObject;
    destructor Destroy; override;
    function Attr(AId:Cardinal):TAlarmValue;
  end;

  EAlarmParse = class(Exception);

destructor TAlarmValue.Destroy;
var
  i:Integer;
begin
  for i:=0 to High(Items) do
    Items[i].Free;
  inherited Destroy;
end;

procedure TAlarmValue.Add(AKey:Cardinal; AItem:TAlarmValue);
begin
  SetLength(Keys, Length(Keys)+1);
  SetLength(Items, Length(Items)+1);
  Keys[High(Keys)] := AKey;
  Items[High(Items)] := AItem;
end;

function TAlarmValue.Find(AKey:Cardinal):TAlarmValue;
var
  i:Integer;
begin
  Result := nil;
  for i:=0 to High(Keys) do
    if Keys[i]=AKey then begin
      Result := Items[i];
      exit;
    end;
end;

destructor TAlarmObject.Destroy;
var
  i:Integer;
begin
  for i:=0 to High(AttrValues) do
    AttrValues[i].Free;
  for i:=0 to High(Children) do
    Children[i].Free;
  inherited Destroy;
end;

function TAlarmObject.Attr(AId:Cardinal):TAlarmValue;
var
  i:Integer;
begin
  Result := nil;
  for i:=0 to High(AttrIds) do
    if AttrIds[i]=AId then begin
      Result := AttrValues[i];
      exit;
    end;
end;

procedure Need(const Data:TBytes; Offset, Count:Integer);
begin
  if (Offset<0) or (Count<0) or (Offset+Count>Length(Data)) then
    raise EAlarmParse.Create('S7PlusAlarms: truncated data');
end;

function ReadVLQ32(const Data:TBytes; var Offset:Integer):Cardinal;
var
  c:Integer;
begin
  Need(Data, Offset, 1);
  Result := DecodeUInt32VLQ(Data, Offset, c);
  Offset := Offset+c;
end;

function ReadBlob(const Data:TBytes; var Offset:Integer):TAlarmValue;
var
  Size:Cardinal;
begin
  Result := TAlarmValue.Create;
  try
    Result.Kind := vkBlob;
    Result.RootId := ReadVLQ32(Data, Offset);
    if Result.RootId>1 then begin //typed blob: 8 bytes of type timestamp + a type byte (2 or 3)
      Need(Data, Offset, 9);
      Offset := Offset+8;
      if not (Data[Offset] in [2,3]) then
        raise EAlarmParse.Create('S7PlusAlarms: unsupported blob type');
      inc(Offset);
    end;
    Size := ReadVLQ32(Data, Offset);
    Need(Data, Offset, Integer(Size));
    SetLength(Result.Bytes, Size);
    if Size>0 then
      Move(Data[Offset], Result.Bytes[0], Size);
    Offset := Offset+Integer(Size);
  except
    Result.Free;
    raise;
  end;
end;

function ReadValue(const Data:TBytes; var Offset:Integer):TAlarmValue; forward;

function ReadScalar(const Data:TBytes; var Offset:Integer; DataType:Byte):TAlarmValue;
var
  c:Integer;
  Size:Cardinal;
  Members:TAlarmValue;
  Key:Cardinal;
  S:Single;
  D:Double;
  Q:QWord;
begin
  if DataType=S7PlusType_BLOB then begin
    Result := ReadBlob(Data, Offset);
    exit;
  end;
  Result := TAlarmValue.Create;
  try
    Result.Kind := vkNumber;
    case DataType of
      S7PlusType_NULL: Result.Kind := vkNull;
      S7PlusType_BOOL, S7PlusType_USINT, S7PlusType_BYTE: begin
        Need(Data, Offset, 1);
        Result.Num := Data[Offset];
        Result.Signed := Data[Offset];
        inc(Offset);
      end;
      S7PlusType_SINT: begin
        Need(Data, Offset, 1);
        Result.Signed := ShortInt(Data[Offset]);
        Result.Num := QWord(Result.Signed);
        inc(Offset);
      end;
      S7PlusType_UINT, S7PlusType_WORD: begin
        Need(Data, Offset, 2);
        Result.Num := (Word(Data[Offset]) shl 8) or Data[Offset+1];
        Result.Signed := Result.Num;
        Offset := Offset+2;
      end;
      S7PlusType_INT: begin
        Need(Data, Offset, 2);
        Result.Signed := SmallInt((Word(Data[Offset]) shl 8) or Data[Offset+1]);
        Result.Num := QWord(Result.Signed);
        Offset := Offset+2;
      end;
      S7PlusType_UDINT, S7PlusType_AID: begin
        Result.Num := ReadVLQ32(Data, Offset);
        Result.Signed := Result.Num;
      end;
      S7PlusType_DINT: begin
        Need(Data, Offset, 1);
        Result.Signed := DecodeInt32VLQ(Data, Offset, c);
        Result.Num := QWord(Result.Signed);
        Offset := Offset+c;
      end;
      S7PlusType_ULINT: begin
        Need(Data, Offset, 1);
        Result.Num := DecodeUInt64VLQ(Data, Offset, c);
        Result.Signed := Int64(Result.Num);
        Offset := Offset+c;
      end;
      S7PlusType_LINT, S7PlusType_TIMESPAN: begin
        Need(Data, Offset, 1);
        Result.Signed := DecodeInt64VLQ(Data, Offset, c);
        Result.Num := QWord(Result.Signed);
        Offset := Offset+c;
      end;
      S7PlusType_DWORD, S7PlusType_RID: begin
        Need(Data, Offset, 4);
        Result.Num := DecodeUInt32(Data, Offset);
        Result.Signed := Result.Num;
        Offset := Offset+4;
      end;
      S7PlusType_LWORD, S7PlusType_TIMESTAMP: begin
        Need(Data, Offset, 8);
        Result.Num := DecodeUInt64(Data, Offset);
        Result.Signed := Int64(Result.Num);
        Offset := Offset+8;
      end;
      S7PlusType_REAL: begin
        Need(Data, Offset, 4);
        Q := DecodeUInt32(Data, Offset);
        Cardinal(S) := Cardinal(Q);
        Result.IsReal := true;
        Result.Real := S;
        Offset := Offset+4;
      end;
      S7PlusType_LREAL: begin
        Need(Data, Offset, 8);
        Q := DecodeUInt64(Data, Offset);
        QWord(D) := Q;
        Result.IsReal := true;
        Result.Real := D;
        Offset := Offset+8;
      end;
      S7PlusType_WSTRING: begin
        Result.Kind := vkText;
        Size := ReadVLQ32(Data, Offset);
        Need(Data, Offset, Integer(Size));
        SetString(Result.Text, PAnsiChar(@Data[Offset]), Size);
        Offset := Offset+Integer(Size);
      end;
      S7PlusType_STRUCT: begin
        Result.Kind := vkStruct;
        Need(Data, Offset, 4);
        Result.RootId := DecodeUInt32(Data, Offset); //the struct id
        Offset := Offset+4;
        Need(Data, Offset, 1);
        while Data[Offset]<>0 do begin
          Key := ReadVLQ32(Data, Offset);
          Members := ReadValue(Data, Offset);
          Result.Add(Key, Members);
          Need(Data, Offset, 1);
        end;
        inc(Offset); //the 0 key that ends the members
      end;
    else
      raise EAlarmParse.CreateFmt('S7PlusAlarms: unsupported datatype $%.2x',[DataType]);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function ReadValue(const Data:TBytes; var Offset:Integer):TAlarmValue;
var
  Flags, DataType:Byte;
  Count, i:Cardinal;
  Key:Cardinal;
begin
  Need(Data, Offset, 2);
  Flags := Data[Offset];
  DataType := Data[Offset+1];
  Offset := Offset+2;
  if Flags=$40 then begin //sparse array: key, value, ..., 0
    Result := TAlarmValue.Create;
    try
      Result.Kind := vkList;
      Key := ReadVLQ32(Data, Offset);
      while Key<>0 do begin
        Result.Add(Key, ReadScalar(Data, Offset, DataType));
        Key := ReadVLQ32(Data, Offset);
      end;
    except
      Result.Free;
      raise;
    end;
  end else if (Flags=$10) or (Flags=$20) then begin //array (or address array)
    Result := TAlarmValue.Create;
    try
      Result.Kind := vkList;
      Count := ReadVLQ32(Data, Offset);
      i := 0;
      while i<Count do begin
        Result.Add(i, ReadScalar(Data, Offset, DataType));
        inc(i);
      end;
    except
      Result.Free;
      raise;
    end;
  end else
    Result := ReadScalar(Data, Offset, DataType);
end;

function ReadObject(const Data:TBytes; var Offset:Integer):TAlarmObject;
var
  Tag:Byte;
  AttrId:Cardinal;
  Child:TAlarmObject;
begin
  Need(Data, Offset, 5);
  if Data[Offset]<>S7PlusElement_StartOfObject then
    raise EAlarmParse.Create('S7PlusAlarms: object expected');
  inc(Offset);
  Result := TAlarmObject.Create;
  try
    Result.RelationId := DecodeUInt32(Data, Offset);
    Offset := Offset+4;
    Result.ClassId := ReadVLQ32(Data, Offset);
    ReadVLQ32(Data, Offset); //class flags
    ReadVLQ32(Data, Offset); //attribute id
    while true do begin
      Need(Data, Offset, 1);
      Tag := Data[Offset];
      if Tag=S7PlusElement_TerminatingObject then begin
        inc(Offset);
        break;
      end else if Tag=S7PlusElement_StartOfObject then begin
        Child := ReadObject(Data, Offset);
        SetLength(Result.Children, Length(Result.Children)+1);
        Result.Children[High(Result.Children)] := Child;
      end else if Tag=S7PlusElement_Attribute then begin
        inc(Offset);
        AttrId := ReadVLQ32(Data, Offset);
        SetLength(Result.AttrIds, Length(Result.AttrIds)+1);
        SetLength(Result.AttrValues, Length(Result.AttrValues)+1);
        Result.AttrIds[High(Result.AttrIds)] := AttrId;
        Result.AttrValues[High(Result.AttrValues)] := nil; //kept safe if the value fails
        Result.AttrValues[High(Result.AttrValues)] := ReadValue(Data, Offset);
      end else if Tag=S7PlusElement_Relation then begin
        inc(Offset);
        ReadVLQ32(Data, Offset);
        Need(Data, Offset, 4);
        Offset := Offset+4;
      end else
        raise EAlarmParse.CreateFmt('S7PlusAlarms: unsupported object element $%.2x',[Tag]);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function S7PlusTimestampToDateTime(Ns:QWord):TDateTime;
begin
  if Ns=0 then
    Result := 0
  else
    Result := EncodeDate(1970,1,1) + (Ns div 1000000)/MSecsPerDay;
end;

function S7PlusAlarmExploreAttributes:TCardinalDynArray;
begin
  SetLength(Result, 10);
  Result[0] := S7PlusIds_DAI_CPUAlarmID;
  Result[1] := S7PlusIds_DAI_AllStatesInfo;
  Result[2] := S7PlusIds_DAI_AlarmDomain;
  Result[3] := S7PlusIds_DAI_Coming;
  Result[4] := S7PlusIds_DAI_Going;
  Result[5] := S7PlusIds_DAI_MessageType;
  Result[6] := S7PlusIds_DAI_HmiInfo;
  Result[7] := S7PlusIds_ObjectVariableTypeName;
  Result[8] := S7PlusIds_DAI_SequenceCounter;
  Result[9] := S7PlusIds_DAI_AlarmTexts;
end;

//-- associated values ----------------------------------------------------------

function BlobBE(const B:TBytes; Count:Integer):QWord;
var
  i:Integer;
begin
  Result := 0;
  for i:=0 to Count-1 do
    Result := (Result shl 8) or B[i];
end;

function UCS2BEToUTF8(const B:TBytes; Start, Chars:Integer):UTF8String;
var
  W:UnicodeString;
  i:Integer;
begin
  SetLength(W, Chars);
  for i:=0 to Chars-1 do
    W[i+1] := WideChar((Word(B[Start+2*i]) shl 8) or B[Start+2*i+1]);
  Result := UTF8Encode(W);
end;

//The value of an associated-value blob as text (by the soft data type in the blob root id); ok=false when unknown.
function AssociatedValueText(Root:Cardinal; const B:TBytes; out Text:UTF8String):Boolean;
const
  SDT_BOOL=$02000001; SDT_BYTE=$02000002; SDT_CHAR=$02000003; SDT_WORD=$02000004; SDT_INT=$02000005;
  SDT_DWORD=$02000006; SDT_DINT=$02000007; SDT_REAL=$02000008; SDT_STRING=$02000013; SDT_LREAL=$02000030;
  SDT_USINT=$02000034; SDT_UINT=$02000035; SDT_UDINT=$02000036; SDT_SINT=$02000037; SDT_WCHAR=$0200003D;
  SDT_WSTRING=$0200003E;
var
  Len:Integer;
  S:Single;
  D:Double;
  Q:QWord;
begin
  Result := false;
  Text := '';
  try
    if (Root=SDT_STRING) or ((Root>=$020A0000) and (Root<=$020AFFFF)) then begin
      Len := B[1];
      if Len>Length(B)-2 then Len := Length(B)-2;
      SetString(Text, PAnsiChar(@B[2]), Len);
      Text := AnsiToUtf8(Text);
    end else if (Root=SDT_WSTRING) or ((Root>=$020B0000) and (Root<=$020BFFFF)) then begin
      Len := (Word(B[2]) shl 8) or B[3]; //max length (2 bytes) + current length (2 bytes): the current one
      if Len>(Length(B)-4) div 2 then Len := (Length(B)-4) div 2;
      Text := UCS2BEToUTF8(B, 4, Len);
    end else case Root of
      SDT_BOOL: if B[0]<>0 then Text := 'TRUE' else Text := 'FALSE';
      SDT_BYTE, SDT_USINT: Text := IntToStr(B[0]);
      SDT_CHAR: Text := AnsiToUtf8(Chr(B[0]));
      SDT_SINT: Text := IntToStr(ShortInt(B[0]));
      SDT_WORD, SDT_UINT: Text := IntToStr(BlobBE(B,2));
      SDT_INT: Text := IntToStr(SmallInt(BlobBE(B,2)));
      SDT_DWORD, SDT_UDINT: Text := IntToStr(BlobBE(B,4));
      SDT_DINT: Text := IntToStr(LongInt(BlobBE(B,4)));
      SDT_REAL: begin
        Q := BlobBE(B,4);
        Cardinal(S) := Cardinal(Q);
        Text := FloatToStr(S);
      end;
      SDT_LREAL: begin
        Q := BlobBE(B,8);
        QWord(D) := Q;
        Text := FloatToStr(D);
      end;
      SDT_WCHAR: Text := UCS2BEToUTF8(B, 0, 1);
    else
      exit;
    end;
    Result := true;
  except
    Result := false;
    Text := '';
  end;
end;

//Applies a printf-style spec ("%d", "%5.2f", "%-8s", "%04x") to a value given as text, when it is a number
//the numeric conversions use it. ok=false when the spec is not understood.
function ApplyPrintf(const Spec, ValueText:String; out Res:String):Boolean;
var
  p:Integer;
  LeftAlign, ZeroPad:Boolean;
  Width, Prec:Integer;
  ConvCh:Char;
  I:Int64;
  F:Double;
  fs:TFormatSettings;
begin
  Result := false;
  Res := '';
  if (Length(Spec)<2) or (Spec[1]<>'%') then exit;
  p := 2;
  LeftAlign := false;
  ZeroPad := false;
  while (p<=Length(Spec)) and (Spec[p] in ['-','0','+',' ','#']) do begin
    if Spec[p]='-' then LeftAlign := true;
    if Spec[p]='0' then ZeroPad := true;
    inc(p);
  end;
  Width := 0;
  while (p<=Length(Spec)) and (Spec[p] in ['0'..'9']) do begin
    Width := Width*10 + Ord(Spec[p])-48;
    inc(p);
  end;
  Prec := -1;
  if (p<=Length(Spec)) and (Spec[p]='.') then begin
    inc(p);
    Prec := 0;
    while (p<=Length(Spec)) and (Spec[p] in ['0'..'9']) do begin
      Prec := Prec*10 + Ord(Spec[p])-48;
      inc(p);
    end;
  end;
  while (p<=Length(Spec)) and (Spec[p] in ['l','h','L','q','j','z','t']) do inc(p); //length modifiers
  if p<>Length(Spec) then exit;
  ConvCh := Spec[p];
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  case ConvCh of
    'd','i','u': begin
      if not TryStrToInt64(ValueText, I) then begin
        if not TryStrToFloat(ValueText, F, fs) then exit;
        I := Round(F);
      end;
      Res := IntToStr(I);
    end;
    'x': begin
      if not TryStrToInt64(ValueText, I) then exit;
      Res := LowerCase(IntToHex(I, 1));
    end;
    'X': begin
      if not TryStrToInt64(ValueText, I) then exit;
      Res := UpperCase(IntToHex(I, 1));
    end;
    'f','F','e','g','G': begin
      if not TryStrToFloat(ValueText, F, fs) then begin
        if not TryStrToInt64(ValueText, I) then exit;
        F := I;
      end;
      if Prec<0 then Prec := 6;
      Res := FloatToStrF(F, ffFixed, 18, Prec, fs);
    end;
    's': begin
      Res := ValueText;
      if (Prec>=0) and (Prec<Length(Res)) then SetLength(Res, Prec);
    end;
    'c': Res := ValueText;
  else
    exit;
  end;
  if Length(Res)<Width then begin
    if LeftAlign then
      Res := Res + StringOfChar(' ', Width-Length(Res))
    else if ZeroPad and (ConvCh<>'s') and (ConvCh<>'c') then begin
      if (Res<>'') and (Res[1]='-') then
        Res := '-' + StringOfChar('0', Width-Length(Res)) + Copy(Res,2,MaxInt)
      else
        Res := StringOfChar('0', Width-Length(Res)) + Res;
    end else
      Res := StringOfChar(' ', Width-Length(Res)) + Res;
  end;
  Result := true;
end;

function S7PlusInterpolateAssociatedValues(const Text:UTF8String; const Values:array of UTF8String):UTF8String;
var
  i, j, k, Idx, Code:Integer;
  Spec, Formatted:String;
begin
  Result := '';
  i := 1;
  while i<=Length(Text) do begin
    if Text[i]='@' then begin
      j := i+1;
      while (j<=Length(Text)) and (Text[j] in ['0'..'9']) do inc(j);
      if (j>i+1) and (j<=Length(Text)) and (Text[j]='%') then begin
        k := j+1;
        while (k<=Length(Text)) and (Text[k]<>'@') do inc(k);
        if (k<=Length(Text)) and (k>j+1) then begin
          Val(Copy(Text, i+1, j-i-1), Idx, Code);
          Spec := Copy(Text, j, k-j);
          if (Code=0) and (Idx>=0) and (Idx<=High(Values)) and (Values[Idx]<>'') and
             ApplyPrintf(Spec, Values[Idx], Formatted) then begin
            Result := Result + Formatted;
            i := k+1;
            continue;
          end;
        end;
      end;
    end;
    Result := Result + Text[i];
    inc(i);
  end;
end;

//-- alarm objects --------------------------------------------------------------

function NumOf(V:TAlarmValue):QWord;
begin
  if (V<>nil) and (V.Kind=vkNumber) then Result := V.Num else Result := 0;
end;

procedure FillHmiInfo(V:TAlarmValue; var H:TS7PlusAlarmHmiInfo);
var
  B:TBytes;
begin
  FillChar(H, SizeOf(H), 0);
  if (V=nil) or (V.Kind<>vkBlob) or (Length(V.Bytes)<9) then exit;
  B := V.Bytes;
  H.Present := true;
  H.SyntaxId := (Word(B[0]) shl 8) or B[1];
  H.Version := (Word(B[2]) shl 8) or B[3];
  H.ClientAlarmId := (Cardinal(B[4]) shl 24) or (Cardinal(B[5]) shl 16) or (Cardinal(B[6]) shl 8) or B[7];
  H.Priority := B[8];
  if (H.SyntaxId>=258) and (Length(B)>=16) then begin
    H.AlarmClass := (Word(B[12]) shl 8) or B[13];
    H.Producer := B[14];
    H.GroupId := B[15];
    if Length(B)>=17 then H.Flags := B[16];
  end;
end;

procedure FillTexts(V:TAlarmValue; const Languages:array of Cardinal; const Values:array of UTF8String;
  var Texts:TS7PlusAlarmTextArray);
var
  i, j, n:Integer;
  Lang, TextId:Cardinal;
  S:UTF8String;
  Found:Boolean;
begin
  SetLength(Texts, 0);
  if (V=nil) or (V.Kind<>vkList) then exit;
  for i:=0 to High(V.Items) do begin
    if V.Items[i].Kind<>vkBlob then continue;
    Lang := V.Keys[i] shr 16;
    TextId := V.Keys[i] and $FFFF;
    if (TextId<1) or (TextId>11) then continue;
    if Length(Languages)>0 then begin
      Found := false;
      for j:=0 to High(Languages) do
        if Languages[j]=Lang then Found := true;
      if not Found then continue;
    end;
    n := -1;
    for j:=0 to High(Texts) do
      if Texts[j].LanguageId=Lang then n := j;
    if n<0 then begin
      n := Length(Texts);
      SetLength(Texts, n+1);
      Texts[n] := Default(TS7PlusAlarmText);
      Texts[n].LanguageId := Lang;
    end;
    SetString(S, PAnsiChar(Pointer(V.Items[i].Bytes)), Length(V.Items[i].Bytes));
    S := S7PlusInterpolateAssociatedValues(S, Values);
    case TextId of
      1: Texts[n].InfoText := S;
      2: Texts[n].AlarmText := S;
    else
      Texts[n].AdditionalText[TextId-3] := S;
    end;
  end;
end;

function AlarmOf(Obj:TAlarmObject; const Languages:array of Cardinal):TS7PlusAlarm;
var
  V, Ev, T:TAlarmValue;
  i:Integer;
  Txt:UTF8String;
begin
  Result := Default(TS7PlusAlarm);
  V := Obj.Attr(S7PlusIds_ObjectVariableTypeName);
  if (V<>nil) and (V.Kind=vkText) then Result.Name := V.Text;
  Result.CpuAlarmId := NumOf(Obj.Attr(S7PlusIds_DAI_CPUAlarmID));
  Result.AllStatesInfo := Byte(NumOf(Obj.Attr(S7PlusIds_DAI_AllStatesInfo)));
  Result.Domain := Word(NumOf(Obj.Attr(S7PlusIds_DAI_AlarmDomain)));
  V := Obj.Attr(S7PlusIds_DAI_MessageType);
  if (V<>nil) and (V.Kind=vkNumber) then Result.MessageType := LongInt(V.Signed);
  Result.SequenceCounter := Cardinal(NumOf(Obj.Attr(S7PlusIds_DAI_SequenceCounter)));
  FillHmiInfo(Obj.Attr(S7PlusIds_DAI_HmiInfo), Result.HmiInfo);

  Ev := Obj.Attr(S7PlusIds_DAI_Coming);
  Result.Event := aeComing;
  if Ev=nil then begin
    Ev := Obj.Attr(S7PlusIds_DAI_Going);
    Result.Event := aeGoing;
  end;
  if (Ev<>nil) and (Ev.Kind=vkStruct) then begin
    T := Ev.Find(S7PlusIds_AS_CGS_Timestamp);
    Result.TimestampNs := NumOf(T);
    Result.Timestamp := S7PlusTimestampToDateTime(Result.TimestampNs);
    T := Ev.Find(S7PlusIds_AS_CGS_AckTimestamp);
    Result.AckTimestampNs := NumOf(T);
    Result.AckTimestamp := S7PlusTimestampToDateTime(Result.AckTimestampNs);
    T := Ev.Find(S7PlusIds_AS_CGS_AssocValues);
    if (T<>nil) and (T.Kind=vkList) then begin
      SetLength(Result.AssociatedValues, Length(T.Items));
      for i:=0 to High(T.Items) do
        if (T.Items[i].Kind=vkBlob) and (i>0) and AssociatedValueText(T.Items[i].RootId, T.Items[i].Bytes, Txt) then
          Result.AssociatedValues[i] := Txt;
    end;
  end else
    Result.Event := aeUnknown;
  FillTexts(Obj.Attr(S7PlusIds_DAI_AlarmTexts), Languages, Result.AssociatedValues, Result.Texts);
end;

procedure CollectAlarms(Objs:array of TAlarmObject; Count:Integer; const Languages:array of Cardinal;
  var Alarms:TS7PlusAlarmArray);
var
  i:Integer;
begin
  for i:=0 to Count-1 do
    if Objs[i].ClassId=S7PlusIds_DAI_ClassRID then begin
      SetLength(Alarms, Length(Alarms)+1);
      Alarms[High(Alarms)] := AlarmOf(Objs[i], Languages);
    end;
end;

//Where the next $A1 (StartOfObject) is at or after From; -1 if there is none.
function NextObjectStart(const R:TBytes; From:Integer):Integer;
begin
  Result := From;
  while (Result<Length(R)) and (R[Result]<>S7PlusElement_StartOfObject) do
    inc(Result);
  if Result>=Length(R) then Result := -1;
end;

function S7PlusParseAlarmExplore(const Response:TBytes; const Languages:array of Cardinal; out Alarms:TS7PlusAlarmArray):Boolean;
const
  MaxCandidates = 8;
var
  Offset, c, Tries, Start, i:Integer;
  ReturnValue:QWord;
  Objs:array of TAlarmObject;
begin
  SetLength(Alarms, 0);
  Result := false;
  if Length(Response)=0 then exit;
  try
    ReturnValue := DecodeUInt64VLQ(Response, 0, c);
    if ReturnValue<>0 then exit;
    Offset := c;
    if Offset+4>Length(Response) then exit;
    Offset := Offset+4; //ExploreId
    //between the ExploreId and the objects there is the IntegrityId in VLQ; when its first byte is $A1 that
    //byte looks like the start of an object, so the $A1 bytes are tried one after the other
    Start := NextObjectStart(Response, Offset);
    if Start<0 then begin
      Result := true; //no object at all: no active alarm
      exit;
    end;
    for Tries:=1 to MaxCandidates do begin
      Start := NextObjectStart(Response, Offset);
      if Start<0 then break;
      SetLength(Objs, 0);
      try
        Offset := Start;
        while (Offset<Length(Response)) and (Response[Offset]=S7PlusElement_StartOfObject) do begin
          SetLength(Objs, Length(Objs)+1);
          Objs[High(Objs)] := nil;
          Objs[High(Objs)] := ReadObject(Response, Offset);
        end;
        SetLength(Alarms, 0);
        CollectAlarms(Objs, Length(Objs), Languages, Alarms);
        Result := true;
      except
        on Exception do begin
          Result := false;
          Offset := Start+1;
        end;
      end;
      for i:=0 to High(Objs) do
        Objs[i].Free;
      SetLength(Objs, 0);
      if Result then break;
    end;
  except
    SetLength(Alarms, 0);
    Result := false;
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
  if Length(B)>0 then Move(B[0], Result[0], Length(B));
end;

function EncodeUIntArrayValue(const Values:array of Word; Flags:Byte):TBytes;
var
  i:Integer;
begin
  Result := BytesConcat(BytesOf([Flags, S7PlusType_UINT]), EncodeUInt32VLQ(Length(Values)));
  for i:=0 to High(Values) do
    Result := BytesConcat(Result, EncodeUInt16(Values[i]));
end;

//An object without its terminating byte, so that children/relations can still be appended.
function OpenObject(RelationId, ClassId:Cardinal; const Attrs:TS7PlusPObjectAttributeArray):TBytes;
begin
  Result := EncodePObject(RelationId, ClassId, 0, 0, Attrs);
  SetLength(Result, Length(Result)-1);
end;

function S7PlusBuildAlarmSubscriptionObject(RelationId:Cardinal; CreditLimit:SmallInt;
  SendTextsAndFilter:Boolean; const Languages:array of Cardinal; const Domains:array of Word):TBytes;
const
  RefRelationId = $51010001;
var
  Attrs, RefAttrs:TS7PlusPObjectAttributeArray;
  Child, DomainFilter:TBytes;
  DomainsZero:array[0..9] of Word;
  i:Integer;

  procedure Add(var A:TS7PlusPObjectAttributeArray; Id:Cardinal; const V:TBytes);
  begin
    SetLength(A, Length(A)+1);
    A[High(A)].AttrId := Id;
    A[High(A)].Value := V;
  end;

begin
  SetLength(Attrs, 0);
  Add(Attrs, S7PlusIds_ObjectVariableTypeName, EncodeValuePWString('Subscription_'+IntToStr(RelationId)));
  Add(Attrs, S7PlusObjId_SubscriptionFunctionClassId, EncodeValuePUSInt(2));
  Add(Attrs, S7PlusObjId_SubscriptionMissedSendings, EncodeValuePUInt(0));
  Add(Attrs, S7PlusObjId_SubscriptionSubsystemError, EncodeValuePLInt(0));
  Add(Attrs, S7PlusObjId_SubscriptionRouteMode, EncodeValuePUSInt(2));
  Add(Attrs, S7PlusObjId_SubscriptionActive, EncodeValuePBool(true));
  Add(Attrs, S7PlusObjId_SubscriptionReferenceList, EncodeValuePUDIntArray([$80010000, 0, 0], $20));
  Add(Attrs, S7PlusObjId_SubscriptionCycleTime, EncodeValuePUDInt(0));
  Add(Attrs, S7PlusIds_SubscriptionDelayTime, EncodeValuePUDInt(0));
  Add(Attrs, S7PlusObjId_SubscriptionDisabled, EncodeValuePUSInt(0));
  Add(Attrs, S7PlusObjId_SubscriptionCount, EncodeValuePUSInt(0));
  Add(Attrs, S7PlusObjId_SubscriptionCreditLimit, EncodeValuePInt(CreditLimit));
  Add(Attrs, S7PlusObjId_SubscriptionTicks, EncodeValuePUInt(65535));

  for i:=0 to High(DomainsZero) do
    DomainsZero[i] := 0;
  SetLength(RefAttrs, 0);
  Add(RefAttrs, S7PlusIds_ObjectVariableTypeName, EncodeValuePWString('S7pDriver_Alarming'));
  Add(RefAttrs, S7PlusIds_SubscriptionReferenceTriggerAndTransmitMode, EncodeValuePUSInt(3));
  Add(RefAttrs, S7PlusIds_AlarmSubscriptionRef_AlarmDomain, EncodeUIntArrayValue(DomainsZero, $10));
  if SendTextsAndFilter then begin
    if Length(Domains)=0 then
      DomainFilter := EncodeUIntArrayValue([$FFFF], $20)
    else
      DomainFilter := EncodeUIntArrayValue(Domains, $20);
    Add(RefAttrs, S7PlusIds_AlarmSubscriptionRef_AlarmDomainFilter, DomainFilter);
    Add(RefAttrs, S7PlusIds_AlarmSubscriptionRef_AlarmTextLanguages, EncodeValuePUDIntArray(Languages, $20));
    Add(RefAttrs, S7PlusIds_AlarmSubscriptionRef_SendAlarmTexts, EncodeValuePBool(true));
  end;

  Child := OpenObject(RefRelationId, S7PlusIds_AlarmSubscriptionRef_ClassRID, RefAttrs);
  //relation to the alarm subsystem
  Child := BytesConcat(Child, BytesOf([S7PlusElement_Relation]));
  Child := BytesConcat(Child, EncodeUInt32VLQ(S7PlusIds_AlarmSubscriptionRef_ItsAlarmSubsystem));
  Child := BytesConcat(Child, EncodeUInt32(S7PlusIds_NativeTheAlarmSubsysRID));
  Child := BytesConcat(Child, BytesOf([S7PlusElement_TerminatingObject]));

  Result := BytesConcat(OpenObject(RelationId, S7PlusObjId_ClassSubscription, Attrs), Child);
  Result := BytesConcat(Result, BytesOf([S7PlusElement_TerminatingObject]));
end;

function S7PlusParseAlarmNotification(const Frame:TBytes; const Languages:array of Cardinal;
  out SubscriptionId:Cardinal; out CreditTick:Byte; out Alarms:TS7PlusAlarmArray):Boolean;
var
  Offset, c, i:Integer;
  Objs:array of TAlarmObject;
begin
  SetLength(Alarms, 0);
  Result := false;
  SubscriptionId := 0;
  CreditTick := 0;
  SetLength(Objs, 0);
  try
    if (Length(Frame)<12) or (Frame[0]<>S7PlusOpcode_Notification) then exit;
    SubscriptionId := DecodeUInt32(Frame, 1);
    Offset := 1+4+6; //subscription id and three unknown UInt16
    CreditTick := Frame[Offset];
    Inc(Offset);
    DecodeUInt32VLQ(Frame, Offset, c); //sequence number
    Offset := Offset+c;
    if Offset>=Length(Frame) then exit;
    if Frame[Offset]<>0 then
      Inc(Offset)
    else
      Offset := Offset+9; //timestamp (8) and the additional change counter
    //the data item list must be empty (an alarm only subscription): its terminator comes next
    if (Offset>=Length(Frame)) or (Frame[Offset]<>0) then exit;
    Inc(Offset);
    //no alarm part
    if (Offset>=Length(Frame)) or (Frame[Offset]=0) then exit;
    if Offset+7>Length(Frame) then exit;
    if SubscriptionId=0 then
      SubscriptionId := DecodeUInt32(Frame, Offset);
    Offset := Offset+6; //alarm subscription id and an unknown UInt16
    if Frame[Offset]<>$81 then exit; //return value: objects follow
    Inc(Offset);
    while (Offset<Length(Frame)) and (Frame[Offset]=S7PlusElement_StartOfObject) do begin
      SetLength(Objs, Length(Objs)+1);
      Objs[High(Objs)] := nil;
      Objs[High(Objs)] := ReadObject(Frame, Offset);
    end;
    CollectAlarms(Objs, Length(Objs), Languages, Alarms);
    Result := true;
  except
    on Exception do begin
      SetLength(Alarms, 0);
      Result := false;
    end;
  end;
  for i:=0 to High(Objs) do
    Objs[i].Free;
end;

function S7PlusAlarmTextOf(const Alarm:TS7PlusAlarm; Language:Cardinal; out Text:TS7PlusAlarmText):Boolean;
var
  i:Integer;
begin
  Result := false;
  Text := Default(TS7PlusAlarmText);
  for i:=0 to High(Alarm.Texts) do
    if (Language<>0) and (Alarm.Texts[i].LanguageId=Language) then begin
      Text := Alarm.Texts[i];
      Result := true;
      exit;
    end;
  if Length(Alarm.Texts)>0 then begin
    Text := Alarm.Texts[0];
    Result := true;
  end;
end;

function S7PlusAlarmToString(const Alarm:TS7PlusAlarm; Language:Cardinal):String;
const
  EventNames:array[TS7PlusAlarmEvent] of String = ('?', 'coming', 'going');
var
  T:TS7PlusAlarmText;
begin
  Result := Format('#%d [%s] seq=%d', [Alarm.CpuAlarmId, EventNames[Alarm.Event], Alarm.SequenceCounter]);
  if Alarm.Timestamp<>0 then
    Result := Result + ' ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Alarm.Timestamp);
  if S7PlusAlarmTextOf(Alarm, Language, T) then
    Result := Result + ' ' + T.AlarmText
  else if Alarm.Name<>'' then
    Result := Result + ' ' + Alarm.Name;
end;

end.
