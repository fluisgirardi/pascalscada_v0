unit crossdatetime;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils {$IFDEF WINCE}, windows{$ENDIF};

  function CrossNow:TDateTime;
  {$IFDEF WINCE}
  function GetOffset:Word;
  {$ENDIF}

implementation

function CrossNow:TDateTime;
{$IFDEF WINCE}
var
  ms:Word;
  encoded_ms:TDateTime;
{$ENDIF}
begin
  Result:=Now;
  {$IFDEF WINCE}
  ms:=(GetTickCount-GetOffset) mod 1000;
  encoded_ms:=EncodeTime(0,0,0,ms);
  Result:=Result+encoded_ms;
  {$ENDIF}
end;


{$IFDEF WINCE}
var
  a, sec2,sec:Word;
  offset:word;

function GetOffset: Word;
begin
  Result:=offset;
end;


initialization

  DecodeTime(CrossNow, a, a, sec, a);
  while true do begin
    DecodeTime(CrossNow, a, a, sec2, a);

    if sec<>sec2 then begin
      offset := GetTickCount mod 1000;
      break;
    end;
  end;
{$ENDIF}
end.
