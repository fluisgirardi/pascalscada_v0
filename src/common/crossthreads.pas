unit crossthreads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, crossevent, syncobjs
  {$IF defined(WIN32) or defined(WIN64) or defined(WINCE)}
  , Windows;
  {$ELSEIF defined(freebsd) or defined(darwin)}
  , sysctl;
  {$ELSEIF defined(linux)}
  {$linklib c};
  {$IFEND}


{$IFDEF FPC}
  {$IF defined(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20404)}
  {$DEFINE USE_TTHREAD_START}
  {$IFEND}
{$ELSE}
  {$IFDEF DELPHI2010_UP}
    {$DEFINE USE_TTHREAD_START}
  {$ENDIF}
{$ENDIF}

type

  { TpSCADACoreAffinityThread }

  TpSCADACoreAffinityThread = class(TThread)
  private
    function GetUniqueID:Int64;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
  DefaultStackSize);

    {$IFDEF PORTUGUES}
    //: Acorda/inicia uma thread suspensa.
    {$ELSE}
    //: Wake/starts a suspended thread.
    {$ENDIF}
    procedure WakeUp;

    {$IFDEF PORTUGUES}
    //: Fornece o número único da thread.
    {$ELSE}
    //: Provides a unique number of identification.
    {$ENDIF}
    property UniqueID:Int64 read GetUniqueID;

    procedure MoveThreadToTheNextCore;
    procedure SetAffinity(CPUIndex:cint32);
    procedure Terminate; virtual;
    class function GetSystemThreadCount: cint32;
  end;

  TpSCADACoreAffinityThreadWithLoop = class(TpSCADACoreAffinityThread)
  private
    FStartLoop,
    FEndLoop:TCrossEvent;
  protected
    procedure Execute; override;
    procedure Loop; virtual; abstract;
    procedure LoopTerminated; virtual;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
      DefaultStackSize);
    destructor Destroy; override;

    function   LoopStarted(timeout:Integer):Boolean;
    procedure  WaitLoopStarts;
    procedure  WaitForLoopTerminates;
    function   WaitEnd(const Timeout:Integer):TWaitResult;
  end;

  {$IFDEF Linux}
  const _SC_NPROCESSORS_ONLN = 83;
  function sysconf(i: cint): clong; cdecl; external name 'sysconf';
  {$ENDIF}

  {$IFDEF UNIX}
  function sched_getaffinity(pid:Ptruint; cpusetsize:longint; cpuset:pointer):cint; cdecl; external;
  function sched_setaffinity(pid:Ptruint; cpusetsize:longint; cpuset:pointer):cint; cdecl; external;

  function pthread_setaffinity_np(pid:TThreadID; cpusetsize:longint; cpuset:pointer):cint; cdecl; external;
  function pthread_getaffinity_np(pid:TThreadID; cpusetsize:longint; cpuset:pointer):cint; cdecl; external;

  function pthread_setname_np(thread:TThreadID; name:PChar):cint;               cdecl; external;
  {$ENDIF}


implementation

{ TpSCADACoreAffinityThreadWithLoop }

procedure TpSCADACoreAffinityThreadWithLoop.Execute;
begin
  while not FEndLoop.ResetEvent do;
  while not FStartLoop.SetEvent do;

  while not Terminated do
    Loop;

  LoopTerminated;

  while not FEndLoop.SetEvent do;
end;

procedure TpSCADACoreAffinityThreadWithLoop.LoopTerminated;
begin

end;

constructor TpSCADACoreAffinityThreadWithLoop.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FStartLoop:=TCrossEvent.Create(true,false);
  FEndLoop:=TCrossEvent.Create(true, false);
end;

destructor TpSCADACoreAffinityThreadWithLoop.Destroy;
begin
  Terminate;
  WaitForLoopTerminates;
  FreeAndNil(FEndLoop);
  FreeAndNil(FStartLoop);
  inherited Destroy;
end;

function TpSCADACoreAffinityThreadWithLoop.LoopStarted(timeout: Integer
  ): Boolean;
begin
  Result:=false;
  if Assigned(FStartLoop) then
    Result:=FStartLoop.WaitFor(timeout)=wrSignaled;
end;

procedure TpSCADACoreAffinityThreadWithLoop.WaitLoopStarts;
begin
  if Assigned(FStartLoop) then
    while FStartLoop.WaitFor($FFFFFFFF)<>wrSignaled do
      CheckSynchronize(1);
end;

procedure TpSCADACoreAffinityThreadWithLoop.WaitForLoopTerminates;
begin
  if Assigned(FEndLoop) then
    while FEndLoop.WaitFor(1)<>wrSignaled do
      CheckSynchronize(1);
end;

function TpSCADACoreAffinityThreadWithLoop.WaitEnd(const Timeout: Integer
  ): TWaitResult;
begin
  Result:=wrAbandoned;
  Result:=FEndLoop.WaitFor(Timeout);
end;

{ TMultiCoreThread }

class function TpSCADACoreAffinityThread.GetSystemThreadCount: cint32;
// returns a good default for the number of threads on this system
{$IF defined(WIN32) or defined(WIN64)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: cint32;
  ProcessAffinityMask, SystemAffinityMask: NativeUInt;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask)
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := 1 shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
  end else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF defined(WINCE)}
begin
  Result:=1;
end;
{$ELSEIF defined(solaris)}
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array[0..1] of cint;
  len: cint;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := sizeof(t);
  fpsysctl(pchar(@mib), 2, @t, @len, Nil, 0);
  Result:=t;
end;
{$ELSEIF defined(linux)}
begin
   Result:=sysconf(_SC_NPROCESSORS_ONLN);
end;
{$ELSE}
begin
  Result:=1;
end;
{$IFEND}

var
  CurrentCore:cint32 = 1; //Core Number 0 reserved to mainthread;

function TpSCADACoreAffinityThread.GetUniqueID: Int64;
begin
  {$IFDEF UNIX}
  Result := Int64(ThreadID);
  {$ELSE}
  Result := ThreadID;
  {$ENDIF}
end;

constructor TpSCADACoreAffinityThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  MoveThreadToTheNextCore;
end;

procedure TpSCADACoreAffinityThread.WakeUp;
begin
  {$IFDEF USE_TTHREAD_START}
    Start;
  {$ELSE}
    Resume;
  {$ENDIF}
end;

procedure TpSCADACoreAffinityThread.MoveThreadToTheNextCore;
var
  CPUSet,
  CurCore:cint32;
begin
  CurCore:=0;
  InterLockedExchange(CurCore,CurrentCore);
  CPUSet:=1 shl CurCore;
  CurCore+=1;
  if (CurCore>=32) OR (CurCore>=GetSystemThreadCount) then
    InterLockedExchange(CurrentCore, 1)
  else
    InterLockedExchange(CurrentCore, CurCore);

  {$IFDEF UNIX}
  pthread_setaffinity_np(ThreadID,SizeOf(cint32),@CPUSet);
  {$ELSE}
  //{$ERROR Implementation missing for Windows!}
  {$ENDIF}
end;

procedure TpSCADACoreAffinityThread.SetAffinity(CPUIndex: cint32);
var
  CPUSet:cint32;
begin
  CPUSet:=1 shl CPUIndex;
  {$IFDEF UNIX}
  pthread_setaffinity_np(ThreadID,SizeOf(cint32),@CPUSet);
  {$ELSE}
  //{$ERROR Implementation missing for Windows!}
  {$ENDIF}
end;

procedure TpSCADACoreAffinityThread.Terminate;
begin
  inherited Terminate;
end;

//var
//  FirstCoreSet:cuint32 = 1;
initialization
  //sets the main thread affinity with first core.
  {$IFDEF UNIX}
  //sched_setaffinity(GetProcessID,SizeOf(FirstCoreSet), @FirstCoreSet);
  {$ELSE}
  //{$ERROR Implementation missing for Windows!}
  {$ENDIF}
end.


