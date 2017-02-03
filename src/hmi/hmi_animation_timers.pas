unit hmi_animation_timers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, fgl;

type
  TCallbackList = specialize TFPGList<TNotifyEvent>;

  { TTimerEntry }

  TTimerEntry = class(TObject)
  private
    Timer:TTimer;
    CallbackList:TCallbackList;
    procedure OnTimer(Sender: TObject);
  public
    constructor Create(Interval:LongWord); overload;
    constructor Create(Interval:LongWord; aCallBack:TNotifyEvent); overload;
    destructor Destroy; override;
    procedure AddTimerCallback(aCallBack:TNotifyEvent);
    procedure RemoveCallback(aCallBack:TNotifyEvent);
    procedure RemoveCallbacksFromObject(aObject:TObject);

  end;

  TTimerList = specialize TFPGMap<LongWord,TTimerEntry>;

  { TTimerManager }

  TTimerManager = class(TObject)
  private
    fTimerList:TTimerList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTimerCallback(Interval:LongWord; aCallBack:TNotifyEvent);
    procedure RemoveTimerCallback(Interval:LongWord; aCallBack:TNotifyEvent);
    procedure RemoveCallback(aCallBack:TNotifyEvent);
    procedure RemoveCallbacksFromObject(aObject:TObject);
  end;

implementation

{ TTimerEntry }

procedure TTimerEntry.OnTimer(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to CallbackList.Count-1 do
    try
      CallbackList.Items[i](Sender);
    except
    end;
end;

constructor TTimerEntry.Create(Interval: LongWord);
begin
  CallbackList:=TCallbackList.Create;
  Timer:=TTimer.Create(nil);
  Timer.OnTimer := @OnTimer;
end;

constructor TTimerEntry.Create(Interval: LongWord; aCallBack: TNotifyEvent);
begin
  Create(Interval);
  AddTimerCallback(aCallBack);
end;

destructor TTimerEntry.Destroy;
begin
  inherited Destroy;
end;

procedure TTimerEntry.AddTimerCallback(aCallBack: TNotifyEvent);
begin
  if CallbackList.IndexOf(aCallBack)=-1 then
    CallbackList.Add(aCallBack);
end;

procedure TTimerEntry.RemoveCallback(aCallBack: TNotifyEvent);
var
  idx: LongInt;
begin
  idx:=CallbackList.IndexOf(aCallBack);
  if idx>=0 then
    CallbackList.Delete(idx);
end;

procedure TTimerEntry.RemoveCallbacksFromObject(aObject: TObject);
var
  i: Integer;
begin
  for i:=CallbackList.Count-1 downto 0 do
    if TMethod(CallbackList.Items[i]).Data = Pointer(aObject) then
      CallbackList.Delete(i);
end;

{ TTimerManager }

constructor TTimerManager.Create;
begin
  fTimerList:=TTimerList.Create;
end;

destructor TTimerManager.Destroy;
begin
  FreeAndNil(fTimerList);
  inherited Destroy;
end;

procedure TTimerManager.AddTimerCallback(Interval: LongWord;
  aCallBack: TNotifyEvent);
var
  idx: LongInt;
begin
  idx:=fTimerList.IndexOf(Interval);
  if idx=-1 then
    fTimerList.Add(Interval,TTimerEntry.Create(Interval, aCallBack))
  else
    fTimerList.KeyData[Interval].AddTimerCallback(aCallBack);
end;

procedure TTimerManager.RemoveTimerCallback(Interval: LongWord;
  aCallBack: TNotifyEvent);
var
  idx: LongInt;
begin
  idx:=fTimerList.IndexOf(Interval);
  if idx<>-1 then
    fTimerList.KeyData[Interval].RemoveCallback(aCallBack);
end;

procedure TTimerManager.RemoveCallback(aCallBack: TNotifyEvent);
begin
  //for k:=0 to fTimerList.Count-1
end;

procedure TTimerManager.RemoveCallbacksFromObject(aObject: TObject);
begin

end;

end.

