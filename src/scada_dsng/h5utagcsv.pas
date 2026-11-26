{$i ../common/language.inc}
{:
  @abstract(Parser for the CSV exported by AutoShop "导出 HMI 监视变量表".)

  The AutoShop variable monitor table is a GBK/GB18030 encoded CSV with a
  number of '#'
  comment lines at the top. Each data line describes one variable of the
  H5U variable area, e.g.

  @code(
    "旋转伺服操作.Jog_Vel","","Float","4","1","UW 21004F0","100ms","1",,,,"点动速度",1
  )

  This unit only reads the file and exposes the fields needed to build tags.
  It has no LCL / IDE dependency so it can also be used from a console test.
  Column indexes (zero based) follow the AutoShop export layout:

  @unorderedList(
    @item(0: name, 2: type, 3: byte size, 4: count, 5: address, 6: period,
          11: comment)
  )
}
unit h5utagcsv;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math
  {$IFDEF FPC}
  , LConvEncoding
  {$ENDIF}
  ;

type
  {: One variable read from the AutoShop CSV. }
  TH5UTagCsvVar = record
    Name: string;        //full variable name, e.g. "Axis1.Jog_Vel"
    VarType: string;     //AutoShop type: Bool, Int16, UInt16, Int32, ...
    Device: string;      //'UB' (bit) or 'UW' (word)
    Address: LongInt;    //hexadecimal value of the CSV, as an integer
    Period: LongInt;     //refresh time in milliseconds
    Comment: string;     //comment column of the CSV
    Count: LongInt;      //number of elements (array), 1 for a scalar
    LineNo: LongInt;     //line number in the source file, for messages
  end;

  {: Array of variables found in the CSV. }
  TH5UTagCsvVars = array of TH5UTagCsvVar;

{: Reads an AutoShop CSV. Returns TRUE when at least one valid variable was
  found; ErrorMsg describes the first problem otherwise. }
function ParseAutoShopCSV(const FileName: string; out Vars: TH5UTagCsvVars;
  out ErrorMsg: string): Boolean;

{: Converts a variable type name from AutoShop to a Pascal tag type name.
  Used by the wizard to fill TPLCTagNumber.TagType. }
function H5UVarTypeToTagTypeName(const V: string): string;

{: Converts a variable type name from AutoShop to a word count. }
function H5UVarTypeToWords(const V: string): LongInt;

{: Builds a valid Pascal identifier from an AutoShop variable name.
  The part after the last '.' is used; non ASCII characters are replaced by
  underscores. }
function H5UIdentFromVarName(const Name: string; Index: LongInt): string;

implementation

function DecodeGBK(const Raw: AnsiString): string;
begin
  {$IFDEF FPC}
  Result := CP936ToUTF8(Raw);
  {$ELSE}
  Result := string(Raw);
  {$ENDIF}
end;

{: Minimal RFC4180 style CSV splitter: honors double quoted fields and the
  doubled quote escape. }
function SplitCSVLine(const Line: string): TStringList;
var
  i, n: LongInt;
  InQuotes: Boolean;
  Field: string;
begin
  Result := TStringList.Create;
  Field := '';
  InQuotes := false;
  n := Length(Line);
  i := 1;
  while i <= n do begin
    if Line[i] = '"' then begin
      if InQuotes and (i < n) and (Line[i+1] = '"') then begin
        Field := Field + '"';
        inc(i,2);
        continue;
      end;
      InQuotes := not InQuotes;
      inc(i);
      continue;
    end;
    if (Line[i] = ',') and (not InQuotes) then begin
      Result.Add(Field);
      Field := '';
      inc(i);
      continue;
    end;
    Field := Field + Line[i];
    inc(i);
  end;
  Result.Add(Field);
end;

function ParsePeriod(const S: string; DefaultValue: LongInt): LongInt;
var
  T: string;
  V: Double;
begin
  Result := DefaultValue;
  T := Trim(LowerCase(S));
  if T = '' then exit;
  try
    if Copy(T,Length(T)-1,2) = 'ms' then begin
      V := StrToFloat(Copy(T,1,Length(T)-2));
      Result := Max(0,Round(V));
    end else if (Length(T)>0) and (T[Length(T)] = 's') then begin
      V := StrToFloat(Copy(T,1,Length(T)-1));
      Result := Max(0,Round(V*1000));
    end else begin
      V := StrToFloat(T);
      Result := Max(0,Round(V));
    end;
  except
    Result := DefaultValue;
  end;
end;

function ParseAddress(const S: string; out Device: string;
  out Address: LongInt): Boolean;
var
  P: string;
  SpacePos: LongInt;
  HexPart: string;
begin
  Result := false;
  Device := '';
  Address := 0;
  P := UpperCase(Trim(S));
  SpacePos := Pos(' ',P);
  if SpacePos <= 0 then exit;
  Device := Trim(Copy(P,1,SpacePos-1));
  if (Device <> 'UB') and (Device <> 'UW') then exit;
  HexPart := Trim(Copy(P,SpacePos+1,MaxInt));
  if HexPart = '' then exit;
  try
    Address := StrToInt('$' + HexPart);
  except
    exit;
  end;
  Result := (Address >= 0) and (Address <= $04C4B400);
end;

function ParseAutoShopCSV(const FileName: string; out Vars: TH5UTagCsvVars;
  out ErrorMsg: string): Boolean;
var
  F: TFileStream;
  Raw: AnsiString;
  Text: string;
  Lines: TStringList;
  Fields: TStringList;
  V: TH5UTagCsvVar;
  i, Count: LongInt;
  Line: string;
begin
  Result := false;
  ErrorMsg := '';
  SetLength(Vars,0);
  Count := 0;
  if not FileExists(FileName) then begin
    ErrorMsg := 'File not found: ' + FileName;
    exit;
  end;

  F := TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Raw,F.Size);
    if F.Size > 0 then
      F.ReadBuffer(Raw[1],F.Size);
  finally
    F.Free;
  end;
  Text := DecodeGBK(Raw);

  Lines := TStringList.Create;
  Fields := nil;
  try
    Lines.Text := Text;
    for i:=0 to Lines.Count-1 do begin
      Line := Lines[i];
      if (Trim(Line) = '') or (Line[1] = '#') then continue;
      Fields := SplitCSVLine(Line);
      try
        if Fields.Count < 6 then continue;
        if (Trim(Fields[0]) = '') or (Trim(Fields[0]) = '名称') then continue;
        if Copy(Trim(Fields[0]),1,1) = '#' then continue;

        V.Name := Trim(Fields[0]);
        V.VarType := Trim(Fields[2]);
        if not ParseAddress(Fields[5],V.Device,V.Address) then continue;

        V.Period := 100;
        if Fields.Count > 6 then
          V.Period := ParsePeriod(Fields[6],100);

        V.Comment := '';
        if Fields.Count > 11 then V.Comment := Trim(Fields[11]);

        V.Count := 1;
        if Fields.Count > 4 then begin
          try
            V.Count := StrToInt(Trim(Fields[4]));
          except
            V.Count := 1;
          end;
        end;
        if V.Count < 1 then V.Count := 1;

        V.LineNo := i + 1;
        inc(Count);
        SetLength(Vars,Count);
        Vars[Count-1] := V;
      finally
        Fields.Free;
        Fields := nil;
      end;
    end;
  finally
    Lines.Free;
  end;

  Result := Count > 0;
  if not Result then
    ErrorMsg := 'No valid variables found in ' + FileName;
end;

function H5UVarTypeToWords(const V: string): LongInt;
var
  T: string;
begin
  T := LowerCase(Trim(V));
  if T = 'bool' then Result := 0
  else if (T = 'int16') or (T='uint16') or (T='word') then Result := 1
  else if (T = 'int32') or (T='uint32') or (T='float') or (T='dword') then Result := 2
  else if (T = 'int64') or (T='uint64') or (T='double') or (T='lreal') then Result := 4
  else Result := 1;
end;

function H5UVarTypeToTagTypeName(const V: string): string;
var
  T: string;
begin
  T := LowerCase(Trim(V));
  if T = 'bool' then Result := 'pttDefault'
  else if T = 'int16' then Result := 'pttSmallInt'
  else if T = 'uint16' then Result := 'pttWord'
  else if T = 'word' then Result := 'pttWord'
  else if T = 'int32' then Result := 'pttLongInt'
  else if T = 'uint32' then Result := 'pttDWord'
  else if T = 'dword' then Result := 'pttDWord'
  else if T = 'float' then Result := 'pttFloat'
  else if T = 'real' then Result := 'pttFloat'
  else if T = 'int64' then Result := 'pttInt64'
  else if T = 'uint64' then Result := 'pttQWord'
  else if T = 'double' then Result := 'pttDouble'
  else if T = 'lreal' then Result := 'pttDouble'
  else Result := 'pttDefault';
end;

function H5UIdentFromVarName(const Name: string; Index: LongInt): string;
var
  Tail: string;
  i, DotPos: LongInt;
  c: Char;
begin
  DotPos := 0;
  for i:=1 to Length(Name) do
    if Name[i] = '.' then DotPos := i;
  if DotPos > 0 then
    Tail := Copy(Name,DotPos+1,MaxInt)
  else
    Tail := Name;

  Result := '';
  for i:=1 to Length(Tail) do begin
    c := Tail[i];
    if (c in ['A'..'Z','a'..'z','0'..'9','_']) then
      Result := Result + c
    else
      Result := Result + '_';
  end;
  if Result = '' then Result := 'Tag_' + IntToStr(Index);
  if not (Result[1] in ['A'..'Z','a'..'z','_']) then
    Result := 'Tag_' + Result;
end;

end.
