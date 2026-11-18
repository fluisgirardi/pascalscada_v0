{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Log de todo o PascalSCADA: hub global + componente TpSCADALogger.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Qualquer parte do framework (portas, drivers, threads, tags, eventos...)
  publica mensagens no hub global com PSLog. Cada TpSCADALogger registrado
  recebe uma copia e decide, por uma lista de regras (fonte, classe, categoria
  e nivel), se a mensagem vai para a tela, para um arquivo, para o evento
  OnLog, ou para lugar nenhum.

  Custo: enquanto nenhuma regra ativa pedir uma categoria/nivel, PSLogWants
  devolve False sem lock e o produtor nao monta a mensagem. O PSLog so copia a
  mensagem para a fila de cada logger; arquivo, tela e evento sao tratados pela
  thread do logger, em lotes, e a tela/evento chegam na thread principal por
  TThread.Queue (aplicacoes sem LCL precisam chamar CheckSynchronize).
}
{$ELSE}
{:
  @abstract(PascalSCADA wide logging: global hub + the TpSCADALogger component.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Any part of the framework (ports, drivers, threads, tags, events...) posts
  messages to the global hub with PSLog. Each registered TpSCADALogger gets a
  copy and decides, through a list of rules (source, class, category and
  level), whether the message goes to the screen, to a file, to the OnLog
  event, or nowhere.

  Cost: while no active rule asks for a category/level, PSLogWants returns
  False without any lock and the producer does not build the message. PSLog only
  copies the message into each logger's queue; files, screen and event are
  handled by the logger's thread, in batches, and screen/event reach the main
  thread through TThread.Queue (applications without the LCL must call
  CheckSynchronize).
}
{$ENDIF}
unit pscadalog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  //: Severity of a log message.
  TpSCADALogLevel = (llTrace, llDebug, llInfo, llWarning, llError);

  //: Subsystem a log message comes from.
  TpSCADALogCategory = (lcComm,       //bytes sent/received by a port
                        lcConnection, //open/close/reconnect/timeouts
                        lcProtocol,   //frames, decoded requests
                        lcScan,       //scan/update threads, queues
                        lcTag,        //tag reads/writes/values
                        lcEvent,      //events fired
                        lcThread,     //thread start/stop/affinity
                        lcSecurity,   //login/logout/authorization
                        lcDatabase,   //database layer
                        lcHMI,        //visual controls
                        lcUser);      //application messages
  TpSCADALogCategories = set of TpSCADALogCategory;

  //: Where a rule sends the messages it matches.
  TpSCADALogTarget  = (ltScreen, ltFile, ltEvent);
  TpSCADALogTargets = set of TpSCADALogTarget;

  //: One log message. Source and SourceClass are captured when it is posted.
  TpSCADALogEntry = record
    Seq:QWord;            //global order of posting
    Timestamp:TDateTime;
    ThreadID:TThreadID;
    ThreadNo:Integer;     //0 = main thread, 1, 2... in the order threads first post
    Category:TpSCADALogCategory;
    Level:TpSCADALogLevel;
    Source:String;        //'Form1.ModBusTCP1', or the class name of a non component
    SourceClass:TClass;   //nil when posted without a sender
    Msg:String;
    Data:TBytes;          //optional payload, written as hex
  end;
  TpSCADALogEntries = array of TpSCADALogEntry;

  {$IFDEF PORTUGUES}
  {:
  Destino de tela do TpSCADALogger. O pacote pascalscada_hmi traz um memo
  (TpSCADALogMemo) que a implementa; e' chamada sempre na thread principal.
  }
  {$ELSE}
  {:
  Screen target of TpSCADALogger. The pascalscada_hmi package brings a memo
  (TpSCADALogMemo) that implements it; always called in the main thread.
  }
  {$ENDIF}
  IpSCADALogDisplay = interface
    ['{6E1F3C52-8B0A-4D47-9C1E-2A7F5D0B3E91}']
    procedure AddLogLines(Lines:TStrings);
  end;

  TpSCADALogger = class;

  //: OnLog event: Line is the entry already formatted with LineFormat.
  TpSCADALogEvent = procedure(Sender:TObject; const Entry:TpSCADALogEntry;
                              const Line:String) of object;

  { TpSCADALogRule }

  {$IFDEF PORTUGUES}
  {:
  Uma regra de roteamento. Uma mensagem casa com a regra quando o nivel e'
  >= MinLevel, a categoria esta em Categories, a fonte casa com SourceMask e a
  classe (ou um ancestral dela) casa com ClassMask. As mascaras aceitam * e ?,
  sem diferenciar maiusculas, e varias alternativas separadas por ';'. Vazio
  equivale a '*'.

  Targets vazio descarta a mensagem; com StopProcessing a regra vira uma
  exclusao. FileName aceita %date%, %source%, %class%, %category% e %level%;
  um caminho relativo e' relativo ao diretorio do executavel.
  }
  {$ELSE}
  {:
  One routing rule. A message matches the rule when its level is >= MinLevel,
  its category is in Categories, its source matches SourceMask and its class
  (or one of its ancestors) matches ClassMask. Masks accept * and ?, are case
  insensitive and take several alternatives separated by ';'. Empty means '*'.

  Empty Targets drops the message; together with StopProcessing the rule
  becomes an exclusion. FileName accepts %date%, %source%, %class%, %category%
  and %level%; a relative path is relative to the executable's directory.
  }
  {$ENDIF}
  TpSCADALogRule = class(TCollectionItem)
  private
    FCategories:TpSCADALogCategories;
    FClassMask:String;
    FEnabled:Boolean;
    FFileName:String;
    FMinLevel:TpSCADALogLevel;
    FSourceMask:String;
    FStopProcessing:Boolean;
    FTargets:TpSCADALogTargets;
    procedure SetCategories(AValue:TpSCADALogCategories);
    procedure SetClassMask(const AValue:String);
    procedure SetEnabled(AValue:Boolean);
    procedure SetFileName(const AValue:String);
    procedure SetMinLevel(AValue:TpSCADALogLevel);
    procedure SetSourceMask(const AValue:String);
    procedure SetStopProcessing(AValue:Boolean);
    procedure SetTargets(AValue:TpSCADALogTargets);
  protected
    function GetDisplayName:String; override;
  public
    constructor Create(ACollection:TCollection); override;
    procedure Assign(Source:TPersistent); override;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default true;
    property SourceMask:String read FSourceMask write SetSourceMask;
    property ClassMask:String read FClassMask write SetClassMask;
    property Categories:TpSCADALogCategories read FCategories write SetCategories
      default [Low(TpSCADALogCategory)..High(TpSCADALogCategory)];
    property MinLevel:TpSCADALogLevel read FMinLevel write SetMinLevel default llInfo;
    property Targets:TpSCADALogTargets read FTargets write SetTargets default [ltScreen];
    property FileName:String read FFileName write SetFileName;
    property StopProcessing:Boolean read FStopProcessing write SetStopProcessing default false;
  end;

  { TpSCADALogRules }

  TpSCADALogRules = class(TOwnedCollection)
  private
    function GetItem(Index:Integer):TpSCADALogRule;
    procedure SetItem(Index:Integer; AValue:TpSCADALogRule);
  protected
    procedure Update(Item:TCollectionItem); override;
  public
    constructor Create(AOwner:TpSCADALogger);
    function Add:TpSCADALogRule;
    property Items[Index:Integer]:TpSCADALogRule read GetItem write SetItem; default;
  end;

  //: Internal: a frozen copy of a rule, read by the logger's thread.
  TpSCADALogRuleSnapshot = record
    SourceMasks,
    ClassMasks:array of String;
    Categories:TpSCADALogCategories;
    MinLevel:TpSCADALogLevel;
    Targets:TpSCADALogTargets;
    FileName:String;       //already resolved when it has no macro
    FileHasMacros:Boolean;
    StopProcessing:Boolean;
  end;
  TpSCADALogRuleSnapshots = array of TpSCADALogRuleSnapshot;

  TpSCADALogFileWriter = class;
  TpSCADALogDispatcher = class;

  { TpSCADALogger }

  {$IFDEF PORTUGUES}
  {:
  Componente que recebe as mensagens do hub e as roteia pelas regras (Rules)
  para a tela (Display), arquivos e o evento OnLog. Inerte em tempo de projeto.
  Pode haver varios no mesmo programa; cada um recebe todas as mensagens.
  }
  {$ELSE}
  {:
  Component that receives the hub's messages and routes them through its Rules
  to the screen (Display), files and the OnLog event. Inert at design time.
  Several of them may exist in a program; each one gets every message.
  }
  {$ENDIF}
  TpSCADALogger = class(TComponent)
  private
    FActive:Boolean;
    FDisplay:TComponent;
    FDisplayIntf:IpSCADALogDisplay;
    FFileIdleCloseMs:Cardinal;
    FFlushIntervalMs:Cardinal;
    FLineFormat:String;
    FMaxFileSize:Int64;
    FMaxQueue:Integer;
    FMaxRotatedFiles:Integer;
    FOnLog:TpSCADALogEvent;
    FRules:TpSCADALogRules;

    //runtime state
    FRunning:Boolean;
    FDispatcher:TpSCADALogDispatcher;
    FQueueLock:TCriticalSection;
    FQueue:TpSCADALogEntries;
    FQueueCount:Integer;
    FDropped:QWord;
    FDroppedUnreported:QWord;
    FEnqueued:QWord;     //under FQueueLock
    FProcessed:QWord;    //written by the dispatcher (interlocked read)
    FLevelMask:array[TpSCADALogCategory] of Byte; //read by the hub, under its lock

    FRulesLock:TCriticalSection;
    FSnapshot:TpSCADALogRuleSnapshots;

    FUILock:TCriticalSection;
    FUIScreen:TStringList;
    FUIEventEntries:TpSCADALogEntries;
    FUIEventLines:array of String;
    FUIEventCount:Integer;
    FUIQueued:Boolean;
    FUIScreenDropped:QWord;

    FWriters:TStringList; //only touched by the dispatcher thread

    procedure SetActive(AValue:Boolean);
    procedure SetDisplay(AValue:TComponent);
    procedure SetFlushIntervalMs(AValue:Cardinal);
    procedure SetLineFormat(const AValue:String);
    procedure SetOnLog(AValue:TpSCADALogEvent);
    procedure SetRules(AValue:TpSCADALogRules);
    function  CanRun:Boolean;
    procedure UpdateRunning;
    procedure StartLogging;
    procedure StopLogging;
    procedure RebuildSnapshot;
    function  GetSnapshot:TpSCADALogRuleSnapshots;
    procedure Enqueue(const Entry:TpSCADALogEntry);
    function  TakeQueue(out Entries:TpSCADALogEntries; out Count:Integer;
                        out Dropped:QWord):Boolean;
    procedure ProcessBatch(const Entries:TpSCADALogEntries; Count:Integer;
                           Dropped:QWord);
    function  GetWriter(const AFileName:String):TpSCADALogFileWriter;
    procedure CloseIdleWriters(Force:Boolean);
    procedure DeliverToUI;
    function  GetLineFormat:String;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent:TComponent; Operation:TOperation); override;
    procedure RulesChanged;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    //: Formats an entry with LineFormat.
    function  FormatEntry(const Entry:TpSCADALogEntry):String;
    //: Formats an entry with the given line format (see LineFormat).
    class function FormatLine(const Fmt:String; const Entry:TpSCADALogEntry):String;
    {:
    Waits until everything posted before the call has been written (files)
    and, when called in the main thread, delivered to Display/OnLog.
    Returns False on timeout.
    }
    function  Flush(TimeoutMs:Cardinal = 5000):Boolean;
    //: Messages dropped because the queue was full (since Active).
    property  Dropped:QWord read FDropped;
  published
    property Active:Boolean read FActive write SetActive default true;
    property Rules:TpSCADALogRules read FRules write SetRules;
    //: A component that implements IpSCADALogDisplay (e.g. TpSCADALogMemo).
    property Display:TComponent read FDisplay write SetDisplay;
    {:
    Tokens: %time% %date% %level% %category% %thread% (main, T1, T2... in
    the order the threads first post; logger for the logger's own messages)
    %threadid% (the system id) %source% %class% %msg% %data% (hex of the
    payload, empty when there is none).
    }
    property LineFormat:String read GetLineFormat write SetLineFormat;
    property FlushIntervalMs:Cardinal read FFlushIntervalMs write SetFlushIntervalMs default 200;
    property MaxQueue:Integer read FMaxQueue write FMaxQueue default 100000;
    //: 0 = no size limit. Over the limit the file is renamed to .1, .2 ...
    property MaxFileSize:Int64 read FMaxFileSize write FMaxFileSize default 0;
    property MaxRotatedFiles:Integer read FMaxRotatedFiles write FMaxRotatedFiles default 5;
    //: A file that got nothing for this long is closed (reopened on demand).
    property FileIdleCloseMs:Cardinal read FFileIdleCloseMs write FFileIdleCloseMs default 10000;
    //: Called in the main thread, for each entry routed to ltEvent.
    property OnLog:TpSCADALogEvent read FOnLog write SetOnLog;
  end;

  { TpSCADALogFileWriter }

  //: Internal: one open log file, owned by the logger's thread.
  TpSCADALogFileWriter = class
  private
    FFileName:String;
    FStream:TFileStream;
    FPending:String;     //capacity grows by doubling; FPendingLen is the used part
    FPendingLen:Integer;
    FLastUse:QWord;
    FRetryAt:QWord;
    FLogger:TpSCADALogger;
    function  Open:Boolean;
    procedure Rotate;
  public
    constructor Create(ALogger:TpSCADALogger; const AFileName:String);
    destructor Destroy; override;
    procedure Add(const Line:String);
    procedure WritePending;
    procedure CloseFile;
    property FileName:String read FFileName;
  end;

  { TpSCADALogDispatcher }

  //: Internal: the thread of a logger.
  TpSCADALogDispatcher = class(TThread)
  private
    FLogger:TpSCADALogger;
    FWake:TEventObject;
  protected
    procedure Execute; override;
  public
    constructor Create(ALogger:TpSCADALogger);
    destructor Destroy; override;
    procedure Wake;
  end;

const
  //: PSLogLevelMask value of a category nobody is listening to.
  PSLogNoLevel = 255;
  PSLogAllCategories = [Low(TpSCADALogCategory)..High(TpSCADALogCategory)];
  PSLogDefaultLineFormat = '%time% [%level%] [%category%] %thread% %source% (%class%): %msg%%data%';

var
  {:
  Lowest level any active rule wants, per category (PSLogNoLevel = nobody).
  Kept up to date by the hub; read without lock by PSLogWants.
  }
  PSLogLevelMask:array[TpSCADALogCategory] of Byte;

//: True when some active rule may want a message of this category/level.
function  PSLogWants(Category:TpSCADALogCategory; Level:TpSCADALogLevel):Boolean; inline;

//: Posts a message to every logger. Sender may be nil.
procedure PSLog(Sender:TObject; Category:TpSCADALogCategory;
                Level:TpSCADALogLevel; const Msg:String); overload;
procedure PSLog(Sender:TObject; Category:TpSCADALogCategory;
                Level:TpSCADALogLevel; const Msg:String; const Data:TBytes); overload;
procedure PSLogFmt(Sender:TObject; Category:TpSCADALogCategory;
                   Level:TpSCADALogLevel; const Fmt:String; const Args:array of const);

//: Flushes every active logger. See TpSCADALogger.Flush.
function  PSLogFlushAll(TimeoutMs:Cardinal = 5000):Boolean;

//: 'Form1.ModBusTCP1' for a component; the class name for other objects.
function  PSLogSourceName(Sender:TObject):String;
//: Case insensitive * and ? match.
function  PSLogMatchMask(const Mask, Value:String):Boolean;
function  PSLogLevelName(Level:TpSCADALogLevel):String;
function  PSLogCategoryName(Category:TpSCADALogCategory):String;
function  PSLogHex(const Data:TBytes):String;

implementation

const
  LevelNames:array[TpSCADALogLevel] of String =
    ('TRACE', 'DEBUG', 'INFO ', 'WARN ', 'ERROR');
  CategoryNames:array[TpSCADALogCategory] of String =
    ('comm', 'connection', 'protocol', 'scan', 'tag', 'event', 'thread',
     'security', 'database', 'hmi', 'user');
  //lines kept for the screen while the main thread does not pick them up
  MaxUIBacklog = 10000;
  //a file that failed to open is retried after this
  FileRetryMs = 5000;

type

  { TpSCADALogHub }

  TpSCADALogHub = class
  private
    FLock:TCriticalSection;
    FLoggers:TFPList;
    FThreads:array of TThreadID;
    function  ThreadNumber(ID:TThreadID):Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(L:TpSCADALogger);
    procedure Unregister(L:TpSCADALogger);
    procedure RecalcMask;
    procedure Emit(var Entry:TpSCADALogEntry);
    function  FlushAll(TimeoutMs:Cardinal):Boolean;
  end;

var
  Hub:TpSCADALogHub = nil;
  GlobalSeq:Int64 = 0;

{ helpers }

function PSLogWants(Category:TpSCADALogCategory; Level:TpSCADALogLevel):Boolean;
begin
  Result := Ord(Level)>=PSLogLevelMask[Category];
end;

function PSLogLevelName(Level:TpSCADALogLevel):String;
begin
  Result := LevelNames[Level];
end;

function PSLogCategoryName(Category:TpSCADALogCategory):String;
begin
  Result := CategoryNames[Category];
end;

function PSLogHex(const Data:TBytes):String;
const
  HexDigits:array[0..15] of Char = '0123456789ABCDEF';
var
  i:Integer;
begin
  if Length(Data)=0 then exit('');
  SetLength(Result, Length(Data)*3-1);
  for i:=0 to High(Data) do begin
    Result[i*3+1] := HexDigits[Data[i] shr 4];
    Result[i*3+2] := HexDigits[Data[i] and $F];
    if i<High(Data) then
      Result[i*3+3] := ' ';
  end;
end;

function PSLogSourceName(Sender:TObject):String;
var
  C:TComponent;
  part:String;
begin
  if Sender=nil then exit('');
  if not (Sender is TComponent) then exit(Sender.ClassName);
  C := TComponent(Sender);
  Result := '';
  while C<>nil do begin
    //the application object (LCL TApplication or TCustomApplication) is not
    //part of a useful path
    if (C<>Sender) and (C.Owner=nil) and (C.Name='') then break;
    if C.ClassName='TApplication' then break;
    part := C.Name;
    if part='' then part := '<'+C.ClassName+'>';
    if Result='' then
      Result := part
    else
      Result := part+'.'+Result;
    C := C.Owner;
  end;
end;

function PSLogMatchMask(const Mask, Value:String):Boolean;
var
  m, v:String;

  function Match(mi, vi:Integer):Boolean;
  begin
    while mi<=Length(m) do begin
      case m[mi] of
        '*': begin
          //collapse repeated stars
          while (mi<=Length(m)) and (m[mi]='*') do Inc(mi);
          if mi>Length(m) then exit(true);
          while vi<=Length(v) do begin
            if Match(mi, vi) then exit(true);
            Inc(vi);
          end;
          exit(false);
        end;
        '?': begin
          if vi>Length(v) then exit(false);
        end;
      else
        if (vi>Length(v)) or (m[mi]<>v[vi]) then exit(false);
      end;
      Inc(mi);
      Inc(vi);
    end;
    Result := vi>Length(v);
  end;

begin
  m := LowerCase(Mask);
  v := LowerCase(Value);
  Result := Match(1, 1);
end;

function SplitMasks(const S:String):TStringArray;
var
  rest, p:String;
  i, n:Integer;
begin
  Result := nil;
  if Trim(S)='' then exit;
  n := 0;
  rest := S+';';
  repeat
    i := Pos(';', rest);
    p := Trim(Copy(rest, 1, i-1));
    Delete(rest, 1, i);
    if p='*' then exit(nil); //matches everything
    if p<>'' then begin
      SetLength(Result, n+1);
      Result[n] := p;
      Inc(n);
    end;
  until rest='';
end;

function IsAbsolutePath(const S:String):Boolean;
begin
  {$IFDEF WINDOWS}
  Result := (ExtractFileDrive(S)<>'') or ((S<>'') and (S[1] in ['\', '/']));
  {$ELSE}
  Result := (S<>'') and (S[1]='/');
  {$ENDIF}
end;

function AnyMaskMatches(const Masks:array of String; const Value:String):Boolean;
var
  i:Integer;
begin
  if Length(Masks)=0 then exit(true);
  for i:=0 to High(Masks) do
    if PSLogMatchMask(Masks[i], Value) then exit(true);
  Result := false;
end;

function ClassMatches(const Masks:array of String; AClass:TClass):Boolean;
var
  c:TClass;
  i:Integer;
begin
  if Length(Masks)=0 then exit(true);
  c := AClass;
  while c<>nil do begin
    for i:=0 to High(Masks) do
      if PSLogMatchMask(Masks[i], c.ClassName) then exit(true);
    c := c.ClassParent;
  end;
  Result := false;
end;

function SafeFilePart(const S:String):String;
var
  i:Integer;
begin
  Result := S;
  for i:=1 to Length(Result) do
    if Result[i] in ['/', '\', ':', '*', '?', '"', '<', '>', '|', #0..#31] then
      Result[i] := '_';
  if Result='' then Result := '_';
end;

function ResolveFileName(const Template:String; const Entry:TpSCADALogEntry):String;
var
  cls:String;
begin
  Result := Template;
  if Pos('%', Result)>0 then begin
    if Entry.SourceClass<>nil then cls := Entry.SourceClass.ClassName else cls := '';
    Result := StringReplace(Result, '%date%',     FormatDateTime('yyyy-mm-dd', Entry.Timestamp), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%source%',   SafeFilePart(Entry.Source), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%class%',    SafeFilePart(cls), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%category%', CategoryNames[Entry.Category], [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%level%',    Trim(LevelNames[Entry.Level]), [rfReplaceAll, rfIgnoreCase]);
  end;
  if not IsAbsolutePath(Result) then
    Result := ExtractFilePath(ParamStr(0))+Result;
  Result := ExpandFileName(Result);
end;

procedure DoEmit(Sender:TObject; Category:TpSCADALogCategory;
                 Level:TpSCADALogLevel; const Msg:String; const Data:TBytes);
var
  E:TpSCADALogEntry;
begin
  if (Hub=nil) or not PSLogWants(Category, Level) then exit;
  E.Seq := QWord(InterLockedIncrement64(GlobalSeq));
  E.Timestamp := Now;
  E.ThreadID := GetCurrentThreadId;
  E.ThreadNo := 0; //numbered by the hub
  E.Category := Category;
  E.Level := Level;
  E.Source := PSLogSourceName(Sender);
  if Sender<>nil then E.SourceClass := Sender.ClassType else E.SourceClass := nil;
  E.Msg := Msg;
  E.Data := Copy(Data);
  Hub.Emit(E);
end;

procedure PSLog(Sender:TObject; Category:TpSCADALogCategory;
                Level:TpSCADALogLevel; const Msg:String);
begin
  DoEmit(Sender, Category, Level, Msg, nil);
end;

procedure PSLog(Sender:TObject; Category:TpSCADALogCategory;
                Level:TpSCADALogLevel; const Msg:String; const Data:TBytes);
begin
  DoEmit(Sender, Category, Level, Msg, Data);
end;

procedure PSLogFmt(Sender:TObject; Category:TpSCADALogCategory;
                   Level:TpSCADALogLevel; const Fmt:String; const Args:array of const);
begin
  if not PSLogWants(Category, Level) then exit;
  DoEmit(Sender, Category, Level, Format(Fmt, Args), nil);
end;

function PSLogFlushAll(TimeoutMs:Cardinal):Boolean;
begin
  if Hub=nil then exit(true);
  Result := Hub.FlushAll(TimeoutMs);
end;

{ TpSCADALogHub }

constructor TpSCADALogHub.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FLoggers := TFPList.Create;
end;

destructor TpSCADALogHub.Destroy;
begin
  FreeAndNil(FLoggers);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TpSCADALogHub.Register(L:TpSCADALogger);
begin
  FLock.Enter;
  try
    if FLoggers.IndexOf(L)<0 then
      FLoggers.Add(L);
  finally
    FLock.Leave;
  end;
  RecalcMask;
end;

procedure TpSCADALogHub.Unregister(L:TpSCADALogger);
begin
  FLock.Enter;
  try
    FLoggers.Remove(L);
  finally
    FLock.Leave;
  end;
  RecalcMask;
end;

procedure TpSCADALogHub.RecalcMask;
var
  m:array[TpSCADALogCategory] of Byte;
  c:TpSCADALogCategory;
  i:Integer;
  L:TpSCADALogger;
begin
  FLock.Enter;
  try
    for c:=Low(c) to High(c) do m[c] := PSLogNoLevel;
    for i:=0 to FLoggers.Count-1 do begin
      L := TpSCADALogger(FLoggers[i]);
      for c:=Low(c) to High(c) do
        if L.FLevelMask[c]<m[c] then
          m[c] := L.FLevelMask[c];
    end;
    for c:=Low(c) to High(c) do
      PSLogLevelMask[c] := m[c];
  finally
    FLock.Leave;
  end;
end;

function TpSCADALogHub.ThreadNumber(ID:TThreadID):Integer;
var
  i:Integer;
begin
  //called under FLock; a program has few threads, a linear search is enough
  if ID=MainThreadID then exit(0);
  for i:=0 to High(FThreads) do
    if FThreads[i]=ID then exit(i+1);
  //an id the system reuses after a thread ends keeps the old number
  SetLength(FThreads, Length(FThreads)+1);
  FThreads[High(FThreads)] := ID;
  Result := Length(FThreads);
end;

procedure TpSCADALogHub.Emit(var Entry:TpSCADALogEntry);
var
  i:Integer;
  L:TpSCADALogger;
begin
  FLock.Enter;
  try
    Entry.ThreadNo := ThreadNumber(Entry.ThreadID);
    for i:=0 to FLoggers.Count-1 do begin
      L := TpSCADALogger(FLoggers[i]);
      if Ord(Entry.Level)>=L.FLevelMask[Entry.Category] then
        L.Enqueue(Entry);
    end;
  finally
    FLock.Leave;
  end;
end;

function TpSCADALogHub.FlushAll(TimeoutMs:Cardinal):Boolean;
var
  list:TFPList;
  i:Integer;
begin
  Result := true;
  list := TFPList.Create;
  try
    FLock.Enter;
    try
      list.Assign(FLoggers);
    finally
      FLock.Leave;
    end;
    //only the main thread destroys loggers, and FlushAll is meant for it too
    for i:=0 to list.Count-1 do
      if not TpSCADALogger(list[i]).Flush(TimeoutMs) then
        Result := false;
  finally
    list.Free;
  end;
end;

{ TpSCADALogRule }

constructor TpSCADALogRule.Create(ACollection:TCollection);
begin
  FEnabled := true;
  FCategories := PSLogAllCategories;
  FMinLevel := llInfo;
  FTargets := [ltScreen];
  inherited Create(ACollection);
end;

procedure TpSCADALogRule.Assign(Source:TPersistent);
var
  R:TpSCADALogRule;
begin
  if Source is TpSCADALogRule then begin
    R := TpSCADALogRule(Source);
    FEnabled := R.FEnabled;
    FSourceMask := R.FSourceMask;
    FClassMask := R.FClassMask;
    FCategories := R.FCategories;
    FMinLevel := R.FMinLevel;
    FTargets := R.FTargets;
    FFileName := R.FFileName;
    FStopProcessing := R.FStopProcessing;
    Changed(false);
  end else
    inherited Assign(Source);
end;

function TpSCADALogRule.GetDisplayName:String;
begin
  Result := '';
  if FSourceMask<>'' then Result := 'src='+FSourceMask;
  if FClassMask<>'' then begin
    if Result<>'' then Result := Result+' ';
    Result := Result+'class='+FClassMask;
  end;
  if Result='' then Result := '*';
  Result := Result+' >= '+Trim(LevelNames[FMinLevel]);
  if FTargets=[] then Result := Result+' -> none';
  if not FEnabled then Result := '('+Result+')';
end;

procedure TpSCADALogRule.SetCategories(AValue:TpSCADALogCategories);
begin
  if FCategories=AValue then exit;
  FCategories := AValue;
  Changed(false);
end;

procedure TpSCADALogRule.SetClassMask(const AValue:String);
begin
  if FClassMask=AValue then exit;
  FClassMask := AValue;
  Changed(false);
end;

procedure TpSCADALogRule.SetEnabled(AValue:Boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled := AValue;
  Changed(false);
end;

procedure TpSCADALogRule.SetFileName(const AValue:String);
begin
  if FFileName=AValue then exit;
  FFileName := AValue;
  Changed(false);
end;

procedure TpSCADALogRule.SetMinLevel(AValue:TpSCADALogLevel);
begin
  if FMinLevel=AValue then exit;
  FMinLevel := AValue;
  Changed(false);
end;

procedure TpSCADALogRule.SetSourceMask(const AValue:String);
begin
  if FSourceMask=AValue then exit;
  FSourceMask := AValue;
  Changed(false);
end;

procedure TpSCADALogRule.SetStopProcessing(AValue:Boolean);
begin
  if FStopProcessing=AValue then exit;
  FStopProcessing := AValue;
  Changed(false);
end;

procedure TpSCADALogRule.SetTargets(AValue:TpSCADALogTargets);
begin
  if FTargets=AValue then exit;
  FTargets := AValue;
  Changed(false);
end;

{ TpSCADALogRules }

constructor TpSCADALogRules.Create(AOwner:TpSCADALogger);
begin
  inherited Create(AOwner, TpSCADALogRule);
end;

function TpSCADALogRules.Add:TpSCADALogRule;
begin
  Result := TpSCADALogRule(inherited Add);
end;

function TpSCADALogRules.GetItem(Index:Integer):TpSCADALogRule;
begin
  Result := TpSCADALogRule(inherited Items[Index]);
end;

procedure TpSCADALogRules.SetItem(Index:Integer; AValue:TpSCADALogRule);
begin
  inherited Items[Index] := AValue;
end;

procedure TpSCADALogRules.Update(Item:TCollectionItem);
begin
  inherited Update(Item);
  if Owner is TpSCADALogger then
    TpSCADALogger(Owner).RulesChanged;
end;

{ TpSCADALogFileWriter }

constructor TpSCADALogFileWriter.Create(ALogger:TpSCADALogger; const AFileName:String);
begin
  inherited Create;
  FLogger := ALogger;
  FFileName := AFileName;
end;

destructor TpSCADALogFileWriter.Destroy;
begin
  CloseFile;
  inherited Destroy;
end;

function TpSCADALogFileWriter.Open:Boolean;
var
  dir:String;
begin
  if FStream<>nil then exit(true);
  if (FRetryAt<>0) and (GetTickCount64<FRetryAt) then exit(false);
  try
    dir := ExtractFileDir(FFileName);
    if (dir<>'') and not DirectoryExists(dir) then
      ForceDirectories(dir);
    if FileExists(FFileName) then begin
      FStream := TFileStream.Create(FFileName, fmOpenWrite or fmShareDenyWrite);
      FStream.Seek(0, soEnd);
    end else
      FStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
    FRetryAt := 0;
    Result := true;
  except
    on E:Exception do begin
      FreeAndNil(FStream);
      FRetryAt := GetTickCount64+FileRetryMs;
      Result := false;
      //goes through the rules like any other message (one per retry period)
      PSLog(FLogger, lcUser, llError, 'cannot open log file '+FFileName+': '+E.Message);
    end;
  end;
end;

procedure TpSCADALogFileWriter.Rotate;
var
  i:Integer;
begin
  CloseFile;
  if FLogger.FMaxRotatedFiles<=0 then begin
    DeleteFile(FFileName);
    exit;
  end;
  DeleteFile(FFileName+'.'+IntToStr(FLogger.FMaxRotatedFiles));
  for i:=FLogger.FMaxRotatedFiles-1 downto 1 do
    if FileExists(FFileName+'.'+IntToStr(i)) then
      RenameFile(FFileName+'.'+IntToStr(i), FFileName+'.'+IntToStr(i+1));
  RenameFile(FFileName, FFileName+'.1');
end;

procedure TpSCADALogFileWriter.Add(const Line:String);
const
  FlushAt = 1024*1024;
  EOL:String = LineEnding;
var
  need:Integer;
begin
  need := FPendingLen+Length(Line)+Length(EOL);
  if need>Length(FPending) then begin
    if Length(FPending)*2>need then
      SetLength(FPending, Length(FPending)*2)
    else
      SetLength(FPending, need+4096);
  end;
  if Line<>'' then
    Move(Line[1], FPending[FPendingLen+1], Length(Line));
  Move(EOL[1], FPending[FPendingLen+Length(Line)+1], Length(EOL));
  FPendingLen := need;
  FLastUse := GetTickCount64;
  //a huge batch does not have to sit in memory
  if FPendingLen>=FlushAt then WritePending;
end;

procedure TpSCADALogFileWriter.WritePending;
begin
  if FPendingLen=0 then exit;
  if not Open then begin
    //keeps nothing back: what cannot be written is lost
    FPendingLen := 0;
    exit;
  end;
  try
    if (FLogger.FMaxFileSize>0) and (FStream.Size>0) and
       (FStream.Size+FPendingLen>FLogger.FMaxFileSize) then begin
      Rotate;
      if not Open then begin
        FPendingLen := 0;
        exit;
      end;
    end;
    FStream.WriteBuffer(FPending[1], FPendingLen);
  except
    on E:Exception do begin
      CloseFile;
      FRetryAt := GetTickCount64+FileRetryMs;
      PSLog(FLogger, lcUser, llError, 'cannot write log file '+FFileName+': '+E.Message);
    end;
  end;
  FPendingLen := 0;
  //a buffer that grew for a burst is not kept around
  if Length(FPending)>1024*1024 then FPending := '';
end;

procedure TpSCADALogFileWriter.CloseFile;
begin
  FreeAndNil(FStream);
end;

{ TpSCADALogDispatcher }

constructor TpSCADALogDispatcher.Create(ALogger:TpSCADALogger);
begin
  FLogger := ALogger;
  FWake := TEventObject.Create(nil, false, false, '');
  inherited Create(false);
end;

destructor TpSCADALogDispatcher.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FWake);
end;

procedure TpSCADALogDispatcher.Wake;
begin
  FWake.SetEvent;
end;

procedure TpSCADALogDispatcher.Execute;
var
  entries:TpSCADALogEntries;
  count:Integer;
  dropped:QWord;
  last:Boolean;
begin
  last := false;
  repeat
    if not Terminated then
      FWake.WaitFor(FLogger.FFlushIntervalMs);
    //one more pass after Terminate drains what is left
    last := Terminated;
    while FLogger.TakeQueue(entries, count, dropped) do
      FLogger.ProcessBatch(entries, count, dropped);
    FLogger.CloseIdleWriters(last);
  until last;
end;

{ TpSCADALogger }

constructor TpSCADALogger.Create(AOwner:TComponent);
var
  c:TpSCADALogCategory;
begin
  inherited Create(AOwner);
  FActive := true;
  FLineFormat := PSLogDefaultLineFormat;
  FFlushIntervalMs := 200;
  FMaxQueue := 100000;
  FMaxRotatedFiles := 5;
  FFileIdleCloseMs := 10000;
  for c:=Low(c) to High(c) do FLevelMask[c] := PSLogNoLevel;
  FQueueLock := TCriticalSection.Create;
  FRulesLock := TCriticalSection.Create;
  FUILock := TCriticalSection.Create;
  FUIScreen := TStringList.Create;
  FWriters := TStringList.Create;
  FWriters.Sorted := true;
  FWriters.Duplicates := dupError;
  {$IFDEF WINDOWS}
  FWriters.CaseSensitive := false;
  {$ELSE}
  FWriters.CaseSensitive := true;
  {$ENDIF}
  FRules := TpSCADALogRules.Create(Self);
  //a logger created in code (not streamed) starts right away
  if not (csLoading in ComponentState) then
    UpdateRunning;
end;

destructor TpSCADALogger.Destroy;
begin
  //csDestroying before StopLogging: WaitFor in the main thread runs
  //CheckSynchronize, and DeliverToUI must not call OnLog/Display on a form
  //that is being destroyed
  Destroying;
  StopLogging;
  FreeAndNil(FRules);
  FreeAndNil(FWriters);
  FreeAndNil(FUIScreen);
  FreeAndNil(FUILock);
  FreeAndNil(FRulesLock);
  FreeAndNil(FQueueLock);
  inherited Destroy;
end;

procedure TpSCADALogger.Loaded;
begin
  inherited Loaded;
  RebuildSnapshot;
  UpdateRunning;
end;

procedure TpSCADALogger.Notification(AComponent:TComponent; Operation:TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FDisplay) then begin
    FUILock.Enter;
    try
      FDisplayIntf := nil;
      FDisplay := nil;
    finally
      FUILock.Leave;
    end;
  end;
end;

function TpSCADALogger.CanRun:Boolean;
begin
  Result := FActive and
            ([csDesigning, csLoading, csDestroying]*ComponentState=[]) and
            (FRules<>nil);
end;

procedure TpSCADALogger.UpdateRunning;
begin
  if CanRun then begin
    if not FRunning then
      StartLogging;
  end else
    if FRunning then
      StopLogging;
end;

procedure TpSCADALogger.StartLogging;
begin
  FDropped := 0;
  FDroppedUnreported := 0;
  FUIScreenDropped := 0;
  RebuildSnapshot;
  FDispatcher := TpSCADALogDispatcher.Create(Self);
  FRunning := true;
  if Hub<>nil then Hub.Register(Self);
end;

procedure TpSCADALogger.StopLogging;
begin
  if not FRunning then exit;
  //what was already routed to the screen/event is still delivered by
  //WaitFor (it runs CheckSynchronize in the main thread), unless the logger
  //is being destroyed (see DeliverToUI)
  //after Unregister no Emit can be inside Enqueue for this logger anymore
  if Hub<>nil then Hub.Unregister(Self);
  FRunning := false;
  if FDispatcher<>nil then begin
    FDispatcher.Terminate;
    FDispatcher.Wake;
    FDispatcher.WaitFor;
    FreeAndNil(FDispatcher);
  end;
  //nothing reaches the screen/event from here on
  TThread.RemoveQueuedEvents(@DeliverToUI);
  FUILock.Enter;
  try
    FUIQueued := false;
    FUIScreen.Clear;
    FUIEventEntries := nil;
    FUIEventLines := nil;
    FUIEventCount := 0;
  finally
    FUILock.Leave;
  end;
  //closes and frees the files (the thread is gone)
  CloseIdleWriters(true);
  while FWriters.Count>0 do begin
    FWriters.Objects[0].Free;
    FWriters.Delete(0);
  end;
  FQueueLock.Enter;
  try
    FQueue := nil;
    FQueueCount := 0;
    FProcessed := FEnqueued;
  finally
    FQueueLock.Leave;
  end;
end;

procedure TpSCADALogger.RulesChanged;
begin
  if csLoading in ComponentState then exit;
  RebuildSnapshot;
end;

procedure TpSCADALogger.RebuildSnapshot;
var
  snap:TpSCADALogRuleSnapshots;
  mask:array[TpSCADALogCategory] of Byte;
  c:TpSCADALogCategory;
  i, n:Integer;
  R:TpSCADALogRule;
  targets:TpSCADALogTargets;
begin
  if FRules=nil then exit;
  for c:=Low(c) to High(c) do mask[c] := PSLogNoLevel;
  snap := nil;
  SetLength(snap, FRules.Count);
  n := 0;
  for i:=0 to FRules.Count-1 do begin
    R := FRules[i];
    if not R.Enabled then continue;
    targets := R.Targets;
    if (ltFile in targets) and (Trim(R.FileName)='') then
      Exclude(targets, ltFile);
    snap[n].SourceMasks := SplitMasks(R.SourceMask);
    snap[n].ClassMasks := SplitMasks(R.ClassMask);
    snap[n].Categories := R.Categories;
    snap[n].MinLevel := R.MinLevel;
    snap[n].Targets := targets;
    snap[n].FileName := Trim(R.FileName);
    snap[n].FileHasMacros := Pos('%', snap[n].FileName)>0;
    if (snap[n].FileName<>'') and not snap[n].FileHasMacros then
      snap[n].FileName := ResolveFileName(snap[n].FileName, Default(TpSCADALogEntry));
    snap[n].StopProcessing := R.StopProcessing;
    Inc(n);
    //an exclusion does not make anybody post messages
    if targets<>[] then
      for c:=Low(c) to High(c) do
        if (c in R.Categories) and (Ord(R.MinLevel)<mask[c]) then
          mask[c] := Ord(R.MinLevel);
  end;
  SetLength(snap, n);

  FRulesLock.Enter;
  try
    FSnapshot := snap;
  finally
    FRulesLock.Leave;
  end;
  //read by the hub under its own lock
  for c:=Low(c) to High(c) do FLevelMask[c] := mask[c];
  if FRunning and (Hub<>nil) then Hub.RecalcMask;
end;

function TpSCADALogger.GetSnapshot:TpSCADALogRuleSnapshots;
begin
  FRulesLock.Enter;
  try
    Result := FSnapshot;
  finally
    FRulesLock.Leave;
  end;
end;

procedure TpSCADALogger.Enqueue(const Entry:TpSCADALogEntry);
var
  wake:Boolean;
begin
  wake := false;
  FQueueLock.Enter;
  try
    if FQueueCount>=FMaxQueue then begin
      Inc(FDropped);
      Inc(FDroppedUnreported);
      exit;
    end;
    if FQueueCount>=Length(FQueue) then
      SetLength(FQueue, 64+Length(FQueue)*2);
    FQueue[FQueueCount] := Entry;
    Inc(FQueueCount);
    Inc(FEnqueued);
    //no need to wait for the interval when the queue is filling up
    wake := FQueueCount=(FMaxQueue div 2);
  finally
    FQueueLock.Leave;
  end;
  if wake and (FDispatcher<>nil) then FDispatcher.Wake;
end;

function TpSCADALogger.TakeQueue(out Entries:TpSCADALogEntries; out Count:Integer;
                                 out Dropped:QWord):Boolean;
begin
  FQueueLock.Enter;
  try
    Entries := FQueue;
    Count := FQueueCount;
    Dropped := FDroppedUnreported;
    FDroppedUnreported := 0;
    FQueue := nil;
    FQueueCount := 0;
    Result := Count>0;
  finally
    FQueueLock.Leave;
  end;
end;

function TpSCADALogger.GetLineFormat:String;
begin
  FRulesLock.Enter;
  try
    Result := FLineFormat;
  finally
    FRulesLock.Leave;
  end;
end;

function TpSCADALogger.FormatEntry(const Entry:TpSCADALogEntry):String;
begin
  Result := FormatLine(GetLineFormat, Entry);
end;

class function TpSCADALogger.FormatLine(const Fmt:String; const Entry:TpSCADALogEntry):String;
var
  i, j, n:Integer;
  token, piece:String;
begin
  //one pass over the format; the message and the payload are not scanned, so
  //a "%data%" inside a message stays as it is
  Result := '';
  n := Length(Fmt);
  i := 1;
  while i<=n do begin
    j := i;
    while (j<=n) and (Fmt[j]<>'%') do Inc(j);
    if j>i then Result := Result+Copy(Fmt, i, j-i);
    if j>n then break;
    //Fmt[j]='%': looks for the closing one
    i := j+1;
    while (i<=n) and (Fmt[i]<>'%') do Inc(i);
    if i>n then begin
      Result := Result+Copy(Fmt, j, MaxInt);
      break;
    end;
    token := LowerCase(Copy(Fmt, j+1, i-j-1));
    case token of
      'time':     piece := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Entry.Timestamp);
      'date':     piece := FormatDateTime('yyyy-mm-dd', Entry.Timestamp);
      'level':    piece := LevelNames[Entry.Level];
      'category': piece := CategoryNames[Entry.Category];
      'thread':   case Entry.ThreadNo of
                     0: piece := 'main';
                    -1: piece := 'logger';
                  else
                    piece := 'T'+IntToStr(Entry.ThreadNo);
                  end;
      'threadid': piece := IntToStr(PtrUInt(Entry.ThreadID));
      'source':   piece := Entry.Source;
      'class':    if Entry.SourceClass<>nil then piece := Entry.SourceClass.ClassName else piece := '';
      'msg':      piece := Entry.Msg;
      'data':     if Length(Entry.Data)>0 then piece := ' | '+PSLogHex(Entry.Data) else piece := '';
    else
      //not a token: the first % is literal, the second may open a token
      Result := Result+'%';
      i := j+1;
      continue;
    end;
    Result := Result+piece;
    i := i+1;
  end;
end;

procedure TpSCADALogger.ProcessBatch(const Entries:TpSCADALogEntries;
  Count:Integer; Dropped:QWord);
var
  snap:TpSCADALogRuleSnapshots;
  i:Integer;
  targets:TpSCADALogTargets;
  files:array of String;
  nfiles:Integer;
  line, fn, fmt:String;
  queueUI, dup:Boolean;
  E:TpSCADALogEntry;
  W:TpSCADALogFileWriter;

  procedure Route(const Entry:TpSCADALogEntry);
  var
    k, f:Integer;
  begin
    targets := [];
    nfiles := 0;
    for k:=0 to High(snap) do begin
      if Ord(Entry.Level)<Ord(snap[k].MinLevel) then continue;
      if not (Entry.Category in snap[k].Categories) then continue;
      if not AnyMaskMatches(snap[k].SourceMasks, Entry.Source) then continue;
      if not ClassMatches(snap[k].ClassMasks, Entry.SourceClass) then continue;
      targets := targets+snap[k].Targets;
      if ltFile in snap[k].Targets then begin
        if nfiles>=Length(files) then SetLength(files, nfiles+4);
        if snap[k].FileHasMacros then
          files[nfiles] := ResolveFileName(snap[k].FileName, Entry)
        else
          files[nfiles] := snap[k].FileName;
        Inc(nfiles);
      end;
      if snap[k].StopProcessing then break;
    end;
    if targets=[] then exit;

    line := FormatLine(fmt, Entry);

    if ltFile in targets then
      for k:=0 to nfiles-1 do begin
        fn := files[k];
        //two rules on the same file write the line once
        dup := false;
        for f:=0 to k-1 do
          if files[f]=fn then begin
            dup := true;
            break;
          end;
        if dup then continue;
        W := GetWriter(fn);
        W.Add(line);
      end;

    if [ltScreen, ltEvent]*targets<>[] then begin
      FUILock.Enter;
      try
        if (ltScreen in targets) and (FDisplayIntf<>nil) then begin
          if FUIScreen.Count>=MaxUIBacklog then begin
            FUIScreen.Delete(0);
            Inc(FUIScreenDropped);
          end;
          FUIScreen.Add(line);
          queueUI := true;
        end;
        if (ltEvent in targets) and Assigned(FOnLog) and
           (FUIEventCount<MaxUIBacklog) then begin
          if FUIEventCount>=Length(FUIEventEntries) then begin
            SetLength(FUIEventEntries, 64+FUIEventCount*2);
            SetLength(FUIEventLines, Length(FUIEventEntries));
          end;
          FUIEventEntries[FUIEventCount] := Entry;
          FUIEventLines[FUIEventCount] := line;
          Inc(FUIEventCount);
          queueUI := true;
        end;
      finally
        FUILock.Leave;
      end;
    end;
  end;

begin
  snap := GetSnapshot;
  fmt := GetLineFormat;
  files := nil;
  queueUI := false;

  //what the queue could not take is told first, routed like any message
  if Dropped>0 then begin
    E := Default(TpSCADALogEntry);
    E.Seq := 0;
    E.Timestamp := Now;
    E.ThreadID := GetCurrentThreadId;
    E.ThreadNo := -1; //the logger's own thread
    E.Category := lcUser;
    E.Level := llWarning;
    E.Source := PSLogSourceName(Self);
    E.SourceClass := ClassType;
    E.Msg := Format('%d log messages dropped (queue full)', [Dropped]);
    Route(E);
  end;

  for i:=0 to Count-1 do
    Route(Entries[i]);

  for i:=0 to FWriters.Count-1 do
    TpSCADALogFileWriter(FWriters.Objects[i]).WritePending;

  if queueUI then begin
    FUILock.Enter;
    try
      if FUIQueued then
        queueUI := false
      else
        FUIQueued := true;
    finally
      FUILock.Leave;
    end;
    if queueUI then
      TThread.Queue(nil, @DeliverToUI);
  end;

  InterLockedExchangeAdd64(Int64(FProcessed), Count);
end;

function TpSCADALogger.GetWriter(const AFileName:String):TpSCADALogFileWriter;
var
  idx:Integer;
begin
  if FWriters.Find(AFileName, idx) then
    exit(TpSCADALogFileWriter(FWriters.Objects[idx]));
  Result := TpSCADALogFileWriter.Create(Self, AFileName);
  FWriters.AddObject(AFileName, Result);
end;

procedure TpSCADALogger.CloseIdleWriters(Force:Boolean);
var
  i:Integer;
  now64:QWord;
  W:TpSCADALogFileWriter;
begin
  now64 := GetTickCount64;
  for i:=FWriters.Count-1 downto 0 do begin
    W := TpSCADALogFileWriter(FWriters.Objects[i]);
    if Force or (now64-W.FLastUse>=FFileIdleCloseMs) then begin
      W.WritePending;
      W.Free;
      FWriters.Delete(i);
    end;
  end;
end;

procedure TpSCADALogger.DeliverToUI;
var
  lines:TStringList;
  entries:TpSCADALogEntries;
  elines:array of String;
  ecount, i:Integer;
  disp:IpSCADALogDisplay;
  ev:TpSCADALogEvent;
  lost:QWord;
begin
  lines := TStringList.Create;
  try
    FUILock.Enter;
    try
      FUIQueued := false;
      lines.Assign(FUIScreen);
      FUIScreen.Clear;
      entries := FUIEventEntries;
      elines := FUIEventLines;
      ecount := FUIEventCount;
      FUIEventEntries := nil;
      FUIEventLines := nil;
      FUIEventCount := 0;
      lost := FUIScreenDropped;
      FUIScreenDropped := 0;
      disp := FDisplayIntf;
      ev := FOnLog;
    finally
      FUILock.Leave;
    end;
    if csDestroying in ComponentState then exit;
    if lost>0 then
      lines.Insert(0, Format('... %d lines not shown (screen too slow)', [lost]));
    if (disp<>nil) and (lines.Count>0) then
      disp.AddLogLines(lines);
    if Assigned(ev) then
      for i:=0 to ecount-1 do
        ev(Self, entries[i], elines[i]);
  finally
    lines.Free;
  end;
end;

function TpSCADALogger.Flush(TimeoutMs:Cardinal):Boolean;
var
  target:QWord;
  start:QWord;
begin
  if not FRunning then exit(true);
  FQueueLock.Enter;
  try
    target := FEnqueued;
  finally
    FQueueLock.Leave;
  end;
  start := GetTickCount64;
  Result := false;
  repeat
    if QWord(InterLockedExchangeAdd64(Int64(FProcessed), 0))>=target then begin
      Result := true;
      break;
    end;
    FDispatcher.Wake;
    Sleep(2);
  until GetTickCount64-start>=TimeoutMs;
  if Result and (GetCurrentThreadId=MainThreadID) then
    CheckSynchronize(0);
end;

procedure TpSCADALogger.SetActive(AValue:Boolean);
begin
  if FActive=AValue then exit;
  FActive := AValue;
  UpdateRunning;
end;

procedure TpSCADALogger.SetDisplay(AValue:TComponent);
var
  intf:IpSCADALogDisplay;
begin
  if FDisplay=AValue then exit;
  intf := nil;
  if (AValue<>nil) and not Supports(AValue, IpSCADALogDisplay, intf) then
    raise EInvalidOperation.CreateFmt('%s does not implement IpSCADALogDisplay',
                                      [AValue.ClassName]);
  if FDisplay<>nil then FDisplay.RemoveFreeNotification(Self);
  FUILock.Enter;
  try
    FDisplay := AValue;
    FDisplayIntf := intf;
  finally
    FUILock.Leave;
  end;
  if FDisplay<>nil then FDisplay.FreeNotification(Self);
end;

procedure TpSCADALogger.SetFlushIntervalMs(AValue:Cardinal);
begin
  if AValue<10 then AValue := 10;
  FFlushIntervalMs := AValue;
end;

procedure TpSCADALogger.SetLineFormat(const AValue:String);
begin
  FRulesLock.Enter;
  try
    if AValue='' then
      FLineFormat := PSLogDefaultLineFormat
    else
      FLineFormat := AValue;
  finally
    FRulesLock.Leave;
  end;
end;

procedure TpSCADALogger.SetOnLog(AValue:TpSCADALogEvent);
begin
  FUILock.Enter;
  try
    FOnLog := AValue;
  finally
    FUILock.Leave;
  end;
end;

procedure TpSCADALogger.SetRules(AValue:TpSCADALogRules);
begin
  FRules.Assign(AValue);
end;

var
  c:TpSCADALogCategory;

initialization
  for c:=Low(c) to High(c) do PSLogLevelMask[c] := PSLogNoLevel;
  Hub := TpSCADALogHub.Create;

finalization
  for c:=Low(c) to High(c) do PSLogLevelMask[c] := PSLogNoLevel;
  FreeAndNil(Hub);

end.
