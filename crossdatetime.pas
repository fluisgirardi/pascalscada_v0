unit crossdatetime;

interface

uses
  SysUtils {$IFDEF WINCE}, windows{$ENDIF};

  function CrossNow:TDateTime;
  function GetOffset:Word;

implementation

function CrossNow:TDateTime;
var
  ms:Word;
  encoded_ms:TDateTime;
begin
  Result:=CrossNow;
  {$IFDEF WINCE}
  ms:=(GetTickCount-GetOffset) mod 1000;
  encoded_ms:=EncodeTime(0,0,0,ms);
  Result:=Result+encoded_ms;
  {$ENDIF}
end;

var
  a, sec2,sec:Word;
  offset:word;

function GetOffset: Word;
begin
  Result:=offset;
end;

{$IFDEF WINCE}
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
