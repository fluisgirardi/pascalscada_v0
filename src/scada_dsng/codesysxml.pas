{$i ../common/language.inc}
{:
  @abstract(Parser for the Codesys symbol configuration XML (Symbolconfiguration.xsd).)

  Codesys exports the OPC UA symbol table of an application as an XML file
  like this:

  @code(
    <Symbolconfiguration xmlns="http://www.3s-software.com/schemas/Symbolconfiguration.xsd">
      <Header>
        <ProjectInfo name="LianDong" devicename="Device" appname="Application" />
      </Header>
      <TypeList>
        <TypeSimple name="T_BOOL" size="1" typeclass="Bool" iecname="BOOL" />
      </TypeList>
      <NodeList>
        <Node name="Application">
          <Node name="GVL">
            <Node name="bStop" type="T_BOOL" access="ReadWrite" />
          </Node>
        </Node>
      </NodeList>
    </Symbolconfiguration>
  )

  This unit only reads the file and exposes the fields needed to build OPC UA
  tags. It has no LCL / IDE dependency. A minimal tag scanner is used instead
  of a DOM implementation so the design-time package needs no extra FPC
  package (fcl-xml) at link time.
}
unit codesysxml;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  {: One variable leaf found in the Codesys NodeList. }
  TCodesysXmlVar = record
    Name: string;          //full symbol path, e.g. "Application.GVL.bStop"
    VarType: string;       //type reference, e.g. "T_BOOL"
    TypeClass: string;     //resolved typeclass, e.g. "Bool", "LReal"
    IECName: string;       //resolved IEC name, e.g. "BOOL", "LREAL"
    Size: LongInt;         //byte size, 0 when unknown
    Access: string;        //"ReadWrite" / "ReadOnly" / "WriteOnly"
    DirectAddress: string; //optional Codesys address, e.g. "%MB0"
    LineNo: LongInt;       //line number in the source file, for messages
  end;

  {: Array of variables found in the XML. }
  TCodesysXmlVars = array of TCodesysXmlVar;

{: Reads a Codesys Symbolconfiguration XML. Returns TRUE when at least one
  variable leaf with a type reference was found; ErrorMsg describes the
  first problem otherwise. }
function ParseCodesysSymbols(const FileName: string; out Vars: TCodesysXmlVars;
  out ErrorMsg: string): Boolean;

{: Converts a Codesys type (typeclass or IEC name) to a PascalSCADA TTagType
  name, e.g. "LReal" -> "pttDouble", "UInt" -> "pttWord".
  TEXTUAL types return 'text' and are imported as TPLCString. Unsupported
  types return 'pttDefault'. }
function CodesysVarTypeToTagTypeName(const TypeClass, IECName: string): string;

{: Builds a valid Pascal identifier from the last segment of a symbol path.
  Codesys identifiers are already ASCII, but the function still sanitizes
  any unexpected character and prefixes identifiers starting with a digit. }
function CodesysIdentFromVarName(const Name: string; Index: LongInt): string;

{: Builds the OPC UA NodeId of a variable:
    @code(ns=<NamespaceIndex>;s=<PathPrefix><symbol path>)
  Codesys normally uses namespace 4 for application symbols and the
  @code(|var|) prefix; both are configurable in the wizard. }
function CodesysVarNodeId(const V: TCodesysXmlVar; NamespaceIndex: LongInt;
  const PathPrefix: string): string;

implementation

type
  TCodesysTypeDef = record
    Name: string;
    TypeClass: string;
    IECName: string;
    Size: LongInt;
  end;

  TCodesysTypeDefs = array of TCodesysTypeDef;

{: Minimal XML tag reader. Returns the tag name (without '<', '/', attributes)
  and fills Attrs with "name=value" pairs. IsOpen is TRUE for a normal start
  tag, FALSE for a closing tag; SelfClosing marks @code(<Node ... />). }
function ReadTag(const S: string; var Pos: LongInt; out TagName: string;
  Attrs: TStringList; out IsOpen, SelfClosing: Boolean): Boolean;
var
  n, Start: LongInt;
  AttrName, AttrValue, Raw: string;
  Quote: Char;
begin
  Result := false;
  TagName := '';
  IsOpen := true;
  SelfClosing := false;
  Attrs.Clear;
  n := Length(S);
  if (Pos > n) or (S[Pos] <> '<') then exit;

  inc(Pos);
  // XML declaration, comments and CDATA are skipped by the caller.
  if (Pos <= n) and (S[Pos] = '/') then begin
    IsOpen := false;
    inc(Pos);
  end else if (Pos <= n) and (S[Pos] = '!') then begin
    // Skip to the next '>' (comment / DOCTYPE).
    while (Pos <= n) and (S[Pos] <> '>') do inc(Pos);
    if Pos <= n then inc(Pos);
    Result := true;
    exit;
  end else if (Pos <= n) and (S[Pos] = '?') then begin
    while (Pos <= n) and (S[Pos] <> '>') do inc(Pos);
    if Pos <= n then inc(Pos);
    Result := true;
    exit;
  end;

  Start := Pos;
  while (Pos <= n) and not (S[Pos] in [' ',#9,#10,#13,'/','>']) do inc(Pos);
  TagName := Copy(S,Start,Pos-Start);

  while Pos <= n do begin
    // Skip whitespace.
    while (Pos <= n) and (S[Pos] in [' ',#9,#10,#13]) do inc(Pos);
    if Pos > n then break;
    if S[Pos] = '>' then begin inc(Pos); break; end;
    if S[Pos] = '/' then begin
      SelfClosing := true;
      inc(Pos);
      if (Pos <= n) and (S[Pos] = '>') then inc(Pos);
      break;
    end;
    // Read attribute name.
    Start := Pos;
    while (Pos <= n) and not (S[Pos] in ['=', ' ',#9,#10,#13,'>','/']) do inc(Pos);
    AttrName := Copy(S,Start,Pos-Start);
    while (Pos <= n) and (S[Pos] in [' ',#9,#10,#13]) do inc(Pos);
    if (Pos > n) or (S[Pos] <> '=') then begin
      // Malformed attribute: consume until '>' to keep scanning.
      while (Pos <= n) and (S[Pos] <> '>') do inc(Pos);
      break;
    end;
    inc(Pos);
    while (Pos <= n) and (S[Pos] in [' ',#9,#10,#13]) do inc(Pos);
    AttrValue := '';
    if Pos <= n then begin
      Quote := S[Pos];
      if (Quote = '"') or (Quote = '''') then begin
        inc(Pos);
        Start := Pos;
        while (Pos <= n) and (S[Pos] <> Quote) do inc(Pos);
        Raw := Copy(S,Start,Pos-Start);
        if Pos <= n then inc(Pos);
      end else begin
        Start := Pos;
        while (Pos <= n) and not (S[Pos] in [' ',#9,#10,#13,'>']) do inc(Pos);
        Raw := Copy(S,Start,Pos-Start);
      end;
      // Minimal entity unescape (Codesys names rarely contain entities).
      AttrValue := StringReplace(Raw,'&quot;','"',[rfReplaceAll]);
      AttrValue := StringReplace(AttrValue,'&apos;','''',[rfReplaceAll]);
      AttrValue := StringReplace(AttrValue,'&lt;','<',[rfReplaceAll]);
      AttrValue := StringReplace(AttrValue,'&gt;','>',[rfReplaceAll]);
      AttrValue := StringReplace(AttrValue,'&amp;','&',[rfReplaceAll]);
    end;
    if AttrName <> '' then Attrs.Add(AttrName+'='+AttrValue);
  end;
  Result := true;
end;

function FindDef(const Defs: TCodesysTypeDefs; const Name: string): LongInt;
var
  i: LongInt;
begin
  for i:=0 to High(Defs) do
    if Defs[i].Name = Name then exit(i);
  Result := -1;
end;

function ParseCodesysSymbols(const FileName: string; out Vars: TCodesysXmlVars;
  out ErrorMsg: string): Boolean;
var
  F: TFileStream;
  Raw: AnsiString;
  Text, TagName, A, V, FullPath, S: string;
  Attrs, Stack: TStringList;
  Defs: TCodesysTypeDefs;
  Def: TCodesysTypeDef;
  VarRec: TCodesysXmlVar;
  Pos, i, LineNo, Count, DefCount: LongInt;
  IsOpen, SelfClosing, InNodeList: Boolean;
begin
  Result := false;
  ErrorMsg := '';
  Vars := nil;
  Raw := '';
  if not FileExists(FileName) then begin
    ErrorMsg := 'File not found: ' + FileName;
    exit;
  end;

  F := TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Raw,F.Size);
    if F.Size > 0 then F.ReadBuffer(Raw[1],F.Size);
  finally
    F.Free;
  end;
  Text := string(Raw);
  // Strip the optional UTF-8 BOM.
  if (Length(Text) >= 3) and (Text[1]=#$EF) and (Text[2]=#$BB) and (Text[3]=#$BF) then
    Delete(Text,1,3);

  Attrs := TStringList.Create;
  Stack := TStringList.Create;
  try
    Pos := 1;
    LineNo := 1;
    Count := 0;
    DefCount := 0;
    InNodeList := false;
    Defs := nil;
    while Pos <= Length(Text) do begin
      if Text[Pos] <> '<' then begin
        if Text[Pos] = #10 then inc(LineNo);
        inc(Pos);
        continue;
      end;
      if Copy(Text,Pos,4) = '<!--' then begin
        // Skip comments, counting line breaks for the error line number.
        i := Pos;
        while (i <= Length(Text)) and not ((i+2 <= Length(Text)) and
              (Text[i]='-') and (Text[i+1]='-') and (Text[i+2]='>')) do begin
          if Text[i] = #10 then inc(LineNo);
          inc(i);
        end;
        Pos := i + 3;
        if Pos <= Length(Text) then inc(Pos);
        continue;
      end;
      if not ReadTag(Text,Pos,TagName,Attrs,IsOpen,SelfClosing) then break;

      if not IsOpen then begin
        if TagName = 'NodeList' then InNodeList := false
        else if (TagName = 'Node') and (Stack.Count > 0) then Stack.Delete(Stack.Count-1);
        continue;
      end;

      if TagName = 'TypeList' then begin
        // no action: TypeSimple tags are read below
      end else if TagName = 'NodeList' then begin
        InNodeList := true;
      end else if TagName = 'TypeSimple' then begin
        Def.Name := ''; Def.TypeClass := ''; Def.IECName := ''; Def.Size := 0;
        for i:=0 to Attrs.Count-1 do begin
          A := Attrs.Names[i];
          V := Attrs.ValueFromIndex[i];
          if A = 'name' then Def.Name := V
          else if A = 'typeclass' then Def.TypeClass := V
          else if A = 'iecname' then Def.IECName := V
          else if A = 'size' then begin
            try Def.Size := StrToInt(V) except Def.Size := 0; end;
          end;
        end;
        if Def.Name <> '' then begin
          inc(DefCount);
          SetLength(Defs,DefCount);
          Defs[DefCount-1] := Def;
        end;
      end else if (TagName = 'Node') and InNodeList then begin
        FullPath := '';
        for i:=0 to Stack.Count-1 do begin
          if FullPath <> '' then FullPath := FullPath + '.';
          FullPath := FullPath + Stack[i];
        end;
        S := '';
        VarRec.VarType := '';
        for i:=0 to Attrs.Count-1 do begin
          A := Attrs.Names[i];
          V := Attrs.ValueFromIndex[i];
          if A = 'name' then S := V
          else if A = 'type' then VarRec.VarType := V;
        end;
        if S = '' then S := 'Node'+IntToStr(LineNo);

        if SelfClosing and (VarRec.VarType <> '') then begin
          // Leaf variable: record it with the resolved type information.
          VarRec.Name := FullPath;
          if VarRec.Name <> '' then VarRec.Name := VarRec.Name + '.' + S
          else VarRec.Name := S;
          VarRec.TypeClass := '';
          VarRec.IECName := '';
          VarRec.Size := 0;
          i := FindDef(Defs,VarRec.VarType);
          if i >= 0 then begin
            VarRec.TypeClass := Defs[i].TypeClass;
            VarRec.IECName := Defs[i].IECName;
            VarRec.Size := Defs[i].Size;
          end;
          VarRec.Access := '';
          VarRec.DirectAddress := '';
          for i:=0 to Attrs.Count-1 do begin
            A := Attrs.Names[i];
            V := Attrs.ValueFromIndex[i];
            if A = 'access' then VarRec.Access := V
            else if A = 'directaddress' then VarRec.DirectAddress := V;
          end;
          VarRec.LineNo := LineNo;
          inc(Count);
          SetLength(Vars,Count);
          Vars[Count-1] := VarRec;
        end else if not SelfClosing then begin
          // Grouping node: push on the path stack.
          Stack.Add(S);
        end;
      end;
    end;
  finally
    Attrs.Free;
    Stack.Free;
  end;

  Result := Count > 0;
  if not Result then
    ErrorMsg := 'No variables found in ' + FileName;
end;

function CodesysVarTypeToTagTypeName(const TypeClass, IECName: string): string;
var
  T: string;
begin
  T := LowerCase(Trim(TypeClass));
  if T = '' then T := LowerCase(Trim(IECName));
  if (T = 'string') or (T = 'wstring') then Result := 'text'
  else if (T = 'bool') then Result := 'pttDefault'
  else if (T = 'byte') or (T = 'usint') then Result := 'pttByte'
  else if (T = 'sint') then Result := 'pttShortInt'
  else if (T = 'uint') or (T = 'word') then Result := 'pttWord'
  else if (T = 'int') then Result := 'pttSmallInt'
  else if (T = 'dword') or (T = 'udint') then Result := 'pttDWord'
  else if (T = 'dint') then Result := 'pttLongInt'
  else if (T = 'real') then Result := 'pttFloat'
  else if (T = 'lreal') then Result := 'pttDouble'
  else if (T = 'lint') then Result := 'pttInt64'
  else if (T = 'ulint') or (T = 'lword') then Result := 'pttQWord'
  else Result := 'pttDefault';
end;

function CodesysIdentFromVarName(const Name: string; Index: LongInt): string;
var
  Tail: string;
  i, DotPos: LongInt;
  c: Char;
begin
  DotPos := 0;
  for i:=1 to Length(Name) do
    if Name[i] = '.' then DotPos := i;
  if DotPos > 0 then Tail := Copy(Name,DotPos+1,MaxInt)
  else Tail := Name;

  Result := '';
  for i:=1 to Length(Tail) do begin
    c := Tail[i];
    if c in ['A'..'Z','a'..'z','0'..'9','_'] then Result := Result + c
    else Result := Result + '_';
  end;
  if Result = '' then Result := 'Tag_' + IntToStr(Index);
  if not (Result[1] in ['A'..'Z','a'..'z','_']) then Result := 'Tag_' + Result;
end;

function CodesysVarNodeId(const V: TCodesysXmlVar; NamespaceIndex: LongInt;
  const PathPrefix: string): string;
begin
  if NamespaceIndex < 0 then NamespaceIndex := 0;
  Result := 'ns=' + IntToStr(NamespaceIndex) + ';s=' + PathPrefix + V.Name;
end;

end.
