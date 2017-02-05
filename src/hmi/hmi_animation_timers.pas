unit hmi_animation_timers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, fgl;

type
  TCallbackList = array of TNotifyEvent;

  { TTimerEntry }

  TTimerEntry = class(TObject)
  private
    FInterval:LongWord;
    FID:LongInt;
    FTimer:TTimer;
    FCallbackList:TCallbackList;
    procedure OnTimer(Sender: TObject);
  public
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

  function GetAnimationTimer:TTimerManager;

implementation

{ TTimerEntry }

function SortCallbackList(const Item1, Item2: TNotifyEvent): Integer;
begin
  //if (TMethod(Item1).Data=TMethod(Item2).Data) and (TMethod(Item1).Code=TMethod(Item2).Code) then
  //  Result:=0
  //else begin
  //  if Pointer(Item1)<Pointer(Item2) then
  //    Result:=-1
  //  else
  //    Result:=1;
  //end;
end;

procedure TTimerEntry.OnTimer(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to High(FCallbackList) do
    try
      FCallbackList[i](Sender);
    except
    end;
end;

var
  TimerCount:Integer = 0;

constructor TTimerEntry.Create(Interval: LongWord; aCallBack: TNotifyEvent);
begin
  inherited Create;
  FInterval:=Interval;
  FID:=TimerCount;
  Inc(TimerCount);
  SetLength(FCallbackList, 0);
  FTimer:=TTimer.Create(nil);
  FTimer.Interval := Interval;
  FTimer.OnTimer  := @OnTimer;
  FTimer.Enabled  := true;
  AddTimerCallback(aCallBack);
end;

destructor TTimerEntry.Destroy;
begin
  SetLength(FCallbackList,0);
  inherited Destroy;
end;

procedure TTimerEntry.AddTimerCallback(aCallBack: TNotifyEvent);
var
  i: Integer;
  found: Boolean;
begin
  found:=false;
  for i:=0 to High(FCallbackList) do
    if (TMethod(FCallbackList[i]).Data=TMethod(aCallBack).Data) and (TMethod(FCallbackList[i]).Code=TMethod(aCallBack).Code) then begin
      found:=true;
      break;
    end;
  if not found then begin
    i:=Length(FCallbackList);
    SetLength(FCallbackList,i+1);
    FCallbackList[i]:=aCallBack;
  end;
end;

procedure TTimerEntry.RemoveCallback(aCallBack: TNotifyEvent);
var
  i: Integer;
begin
  for i:=High(FCallbackList) downto 0 do
    if (TMethod(FCallbackList[i]).Data=TMethod(aCallBack).Data) and (TMethod(FCallbackList[i]).Code=TMethod(aCallBack).Code) then begin
      FCallbackList[i]:=FCallbackList[High(FCallbackList)];
      SetLength(FCallbackList,High(FCallbackList));
    end;
end;

procedure TTimerEntry.RemoveCallbacksFromObject(aObject: TObject);
var
  i: Integer;
begin
  for i:=high(FCallbackList) downto 0 do
    if TMethod(FCallbackList[i]).Data=Pointer(aObject) then begin
      FCallbackList[i]:=FCallbackList[High(FCallbackList)];
      SetLength(FCallbackList,High(FCallbackList));
    end;

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
var
  k: Integer;
begin
  for k:=0 to fTimerList.Count-1 do
    fTimerList.KeyData[fTimerList.Keys[k]].RemoveCallback(aCallBack);
end;

procedure TTimerManager.RemoveCallbacksFromObject(aObject: TObject);
var
  k: Integer;
begin
  for k:=0 to fTimerList.Count-1 do
    fTimerList.KeyData[fTimerList.Keys[k]].RemoveCallbacksFromObject(aObject);
end;

var
  FAnimationTimerManager:TTimerManager;

function GetAnimationTimer:TTimerManager;
begin
  Result:=FAnimationTimerManager;
end;

initialization
  FAnimationTimerManager:=TTimerManager.Create;

finalization
  FreeAndNil(FAnimationTimerManager);


end.

