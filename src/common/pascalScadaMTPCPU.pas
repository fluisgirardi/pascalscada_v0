{ System depending code for light weight threads.

  This file is part of the Free Pascal run time library.

  Copyright (C) 2008 Mattias Gaertner mattias@freepascal.org

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i ../common/language.inc}
{$i ../common/delphiver.inc}
unit pascalScadaMTPCPU;

interface

{$IF defined(WIN32) or defined(WIN64) or defined(WINCE)}
uses Windows;
{$ELSEIF defined(freebsd) or defined(darwin)}
uses ctypes, sysctl;
{$ELSEIF defined(linux)}
{$linklib c}
uses ctypes, sysutils;
{$IFEND}

type
  {$IFNDEF FPC}
    {$IFDEF DELPHI_XE2_UP}
    crossNativeUInt = NativeUInt;
    {$ELSE}
    crossNativeUInt = Cardinal;
    {$ENDIF}
  {$ELSE}
    crossNativeUInt = PtrUInt;
  {$ENDIF}

function GetSystemThreadCount: LongInt;

{$IFDEF PORTUGUES}
{:
Troca a thread em execução da maneira mais eficiente de acordo com várias
configurações de sistema operacional/número de processadores.
}
{$ELSE}
{:
More efficient thread switch, checking the OS/number of processors.
}
{$ENDIF}
procedure CrossThreadSwitch;

implementation

{$IFDEF Linux}
const _SC_NPROCESSORS_ONLN = 83;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}

function GetSystemThreadCount: LongInt;
// returns a good default for the number of threads on this system
{$IF defined(WIN32) or defined(WIN64)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: LongInt;
  ProcessAffinityMask, SystemAffinityMask: crossNativeUInt;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, {%H-}ProcessAffinityMask, {%H-}SystemAffinityMask)
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      {$PUSH}
      {$R-}
      Mask := 1 shl i;
      {$POP}
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
  end else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo({%H-}SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF defined(WINCE)}
begin
  Result:=1;
end;
{$ELSEIF defined(UNTESTEDsolaris)}
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array[0..1] of cint;
  len: csize_t;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  t   := 1;
  //oldlenp is a psize_t, so len must have the size of a size_t, not of a cint.
  len := sizeof(t);
  //since fpc 3.0.4 the first argument of fpsysctl is a pcint (it was a pchar).
  {$IF defined(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30004)}
  if fpsysctl(pcint(@mib[0]), 2, @t, @len, Nil, 0)<>0 then
  {$ELSE}
  if fpsysctl(pchar(@mib[0]), 2, @t, @len, Nil, 0)<>0 then
  {$IFEND}
    t:=1;
  Result:=t;
end;
{$ELSEIF defined(linux)}
  begin
    Result:=sysconf(_SC_NPROCESSORS_ONLN);
    //sysconf devolve -1 quando falha; quem decide como esperar precisa de um
    //numero de processadores de verdade
    //sysconf gives back -1 when it fails; whoever decides how to wait needs a
    //real processor count
    if Result<1 then
      Result:=1;
  end;
{$ELSE}
  begin
    Result:=1;
  end;
{$IFEND}

procedure CrossThreadSwitch;
begin
  if GetSystemThreadCount>1 then
    {$IFDEF FPC}
    ThreadSwitch
    //Sleep(1)
    {$ELSE}
    SwitchToThread
    {$ENDIF}
  else
    {$IF defined(WINDOWS) or defined(WIN32) or defined(WIN64) or defined(WINCE)}
    Sleep(1);
    {$ELSE}
    {$IFDEF FPC}
    ThreadSwitch;
    {$ELSE}
    SwitchToThread;
    {$ENDIF}
    {$IFEND}
end;

end.

