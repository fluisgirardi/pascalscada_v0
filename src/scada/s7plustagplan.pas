{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Regras do Tag Builder do S7CommPlus: que variável do CLP vira que tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Esta unit não tem formulário: ela decide, para cada variável lida do CLP
  (TS7PlusVarInfo, de TS7CommPlusDriver.BrowseDB/BrowseNativeArea), se ela pode
  virar um tag, qual o TagType e quais Swap* o tag precisa, e dá o nome sugerido do
  componente. O formulário (us7plustagbuilder, no pacote de design) só mostra e
  coleta; assim estas regras são testáveis sem o IDE.

  O que vira tag: as variáveis escalares (BOOL, BYTE, CHAR, SINT, USINT, INT, UINT,
  WORD, DINT, UDINT, DWORD, REAL, LINT, ULINT, LWORD, LREAL), sempre um
  TPLCTagNumber. Os valores vêm do CLP em big-endian, por isso o tag liga
  SwapBytes (2, 4 ou 8 bytes), SwapWords (4 ou 8) e SwapDWords (8).

  O que não vira tag, e por quê: STRING/WSTRING (o tamanho máximo não vem na
  lista), tipos de data/hora, estruturas inteiras (os membros aparecem um a um),
  elementos de array e tipos internos.
}
{$ELSE}
{:
  @abstract(S7CommPlus Tag Builder rules: which PLC variable becomes which tag.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  This unit has no form: for each variable read from the PLC (TS7PlusVarInfo, from
  TS7CommPlusDriver.BrowseDB/BrowseNativeArea) it decides whether it can become a
  tag, which TagType and Swap* the tag needs, and gives the suggested component
  name. The form (us7plustagbuilder, in the design package) only shows and
  collects; this way the rules can be tested without the IDE.

  What becomes a tag: the scalar variables (BOOL, BYTE, CHAR, SINT, USINT, INT,
  UINT, WORD, DINT, UDINT, DWORD, REAL, LINT, ULINT, LWORD, LREAL), always a
  TPLCTagNumber. The PLC sends the values big-endian, so the tag turns on
  SwapBytes (2, 4 or 8 bytes), SwapWords (4 or 8) and SwapDWords (8).

  What does not become a tag, and why: STRING/WSTRING (the maximum length is not
  on the list), date/time types, whole structures (their members are offered one
  by one), array elements and internal types.
}
{$ENDIF}
unit S7PlusTagPlan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Tag, S7PlusTypeInfo;

type
  {$IFDEF PORTUGUES}
  //: Por que uma variável do CLP não vira tag.
  {$ELSE}
  //: Why a PLC variable does not become a tag.
  {$ENDIF}
  TS7PlusSkipReason = (srNone, srNotAScalar, srUnsupportedType, srArrayElement);

  {$IFDEF PORTUGUES}
  //: Uma variável do CLP e o que dela dá para fazer.
  {$ELSE}
  //: A PLC variable and what can be done with it.
  {$ENDIF}
  TS7PlusCandidate = record
    //: Caminho simbólico do tag (LongAddress), ex. BlocoSimbolico_4.Var26.v1.
    Path:String;
    //: Código do SoftDataType do CLP.
    SoftDataType:Byte;
    //: Tipo do tag criado (só vale com Skip=srNone).
    TagType:TTagType;
    SwapBytes, SwapWords, SwapDWords:Boolean;
    //: srNone: dá um tag. Senão, o motivo.
    Skip:TS7PlusSkipReason;
    //: Membro de uma estrutura (mais de um nível abaixo do DB/área).
    Nested:Boolean;
    //: Array de escalares: vira um TPLCBlock com ElementCount elementos (ver S7PlusMarkArrays).
    IsArray:Boolean;
    ElementCount:LongInt;
    //: Caminho de cada elemento do array, na ordem em que o CLP os lista (a ordem do bloco).
    ElementPaths:TStringArray;
  end;
  TS7PlusCandidateArray = array of TS7PlusCandidate;

  {$IFDEF PORTUGUES}
  //: Um tag que será criado no formulário/datamodule.
  {$ELSE}
  //: A tag that will be created on the form/datamodule.
  {$ENDIF}
  TS7PlusImportItem = record
    Path:String;
    CompName:String;
    TagType:TTagType;
    SwapBytes, SwapWords, SwapDWords:Boolean;
    //: Tempo de atualização em ms; 0 = por mudança (subscrição).
    Scan:LongInt;
    //: True: um TPLCBlock de Size elementos de TagType; False: um TPLCTagNumber.
    IsBlock:Boolean;
    Size:LongInt;
    //: Nome de componente de cada elemento do bloco (vazio: não criar os elementos).
    ElementNames:TStringArray;
  end;
  TS7PlusImportItems = array of TS7PlusImportItem;

{$IFDEF PORTUGUES}
//: TagType e Swap* de um tipo escalar do CLP. False se o tipo não vira tag.
{$ELSE}
//: TagType and Swap* of a PLC scalar type. False if the type does not become a tag.
{$ENDIF}
function S7PlusTagTypeOf(SoftDataType:Byte; out TagType:TTagType;
                         out SwapBytes, SwapWords, SwapDWords:Boolean):Boolean;

{$IFDEF PORTUGUES}
//: Nome do tipo como o TIA Portal o escreve (DInt, Real, ...); '?<código>' se desconhecido.
{$ELSE}
//: Type name the way TIA Portal writes it (DInt, Real, ...); '?<code>' if unknown.
{$ENDIF}
function S7PlusSoftTypeName(SoftDataType:Byte):String;

{$IFDEF PORTUGUES}
//: Classifica uma variável lida do CLP. DBName: nome simbólico do DB dela (''
//: para ficar com o número, ex. DB4); é trocado no começo do caminho.
{$ELSE}
//: Classifies a variable read from the PLC. DBName: the symbolic name of its DB
//: ('' keeps the number, e.g. DB4); it replaces the start of the path.
{$ENDIF}
function S7PlusClassifyVar(const V:TS7PlusVarInfo; const DBName:String):TS7PlusCandidate;

{$IFDEF PORTUGUES}
//: Nome de componente válido a partir de um caminho (BlocoSimbolico_4.Var26.v1 ->
//: prefixoBlocoSimbolico_4_Var26_v1).
{$ELSE}
//: Valid component name from a path (BlocoSimbolico_4.Var26.v1 ->
//: prefixBlocoSimbolico_4_Var26_v1).
{$ENDIF}
function S7PlusSuggestedName(const Path, Prefix:String):String;

{$IFDEF PORTUGUES}
//: Acha os arrays de escalares na lista (o PLC lista o array e, logo depois, cada
//: elemento como Nome[i]): o array passa a ser IsArray com o número de elementos, e os
//: elementos continuam não virando tag. Chamar depois de classificar a lista toda.
{$ELSE}
//: Finds the arrays of scalars on the list (the PLC lists the array and, right after it,
//: each element as Name[i]): the array becomes IsArray with its element count, and the
//: elements still do not become tags. Call it once the whole list is classified.
{$ENDIF}
procedure S7PlusMarkArrays(var Candidates:TS7PlusCandidateArray);

{$IFDEF PORTUGUES}
//: Os níveis de um caminho (BlocoSimbolico_4.Var26.v1 -> BlocoSimbolico_4, Var26, v1),
//: que são os níveis da árvore do assistente. O que está entre colchetes (índices de
//: array) fica no nível do array, mesmo que tenha ponto.
{$ELSE}
//: The levels of a path (BlocoSimbolico_4.Var26.v1 -> BlocoSimbolico_4, Var26, v1),
//: which are the levels of the wizard tree. What is between brackets (array indexes)
//: stays in the array's level, even if it has a dot.
{$ENDIF}
function S7PlusSplitPath(const Path:String):TStringArray;

implementation

procedure S7PlusMarkArrays(var Candidates:TS7PlusCandidateArray);
var
  i, j, Count:Integer;
  Rest:String;
begin
  for i:=0 to High(Candidates) do begin
    if Candidates[i].Skip<>srNone then continue;
    Count := 0;
    SetLength(Candidates[i].ElementPaths, 0);
    j := i+1;
    //the elements follow their array on the list: Name[...] with nothing after the ']'
    while (j<=High(Candidates)) and (Candidates[j].Skip=srArrayElement) and
          (Pos(Candidates[i].Path+'[', Candidates[j].Path)=1) do begin
      Rest := Copy(Candidates[j].Path, Length(Candidates[i].Path)+1, MaxInt);
      if (Rest<>'') and (Rest[Length(Rest)]=']') and (Pos('.', Rest)=0) then begin
        SetLength(Candidates[i].ElementPaths, Count+1);
        Candidates[i].ElementPaths[Count] := Candidates[j].Path;
        Inc(Count);
      end;
      Inc(j);
    end;
    if Count>0 then begin
      Candidates[i].IsArray := true;
      Candidates[i].ElementCount := Count;
    end;
  end;
end;

function S7PlusSplitPath(const Path:String):TStringArray;
var
  i, Depth, Start:Integer;
begin
  SetLength(Result, 0);
  if Path='' then exit;
  Depth := 0;
  Start := 1;
  for i:=1 to Length(Path) do begin
    case Path[i] of
      '[': Inc(Depth);
      ']': if Depth>0 then Dec(Depth);
      '.': if Depth=0 then begin
        SetLength(Result, Length(Result)+1);
        Result[High(Result)] := Copy(Path, Start, i-Start);
        Start := i+1;
      end;
    end;
  end;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := Copy(Path, Start, MaxInt);
end;

function S7PlusTagTypeOf(SoftDataType:Byte; out TagType:TTagType;
  out SwapBytes, SwapWords, SwapDWords:Boolean):Boolean;
var
  Size:Integer;
begin
  Result := true;
  case TS7PlusSoftDataType(SoftDataType) of
    sdtBOOL, sdtBBOOL, sdtBYTE, sdtCHAR, sdtUSINT: begin TagType := pttByte;     Size := 1; end;
    sdtSINT:                                       begin TagType := pttShortInt; Size := 1; end;
    sdtWORD, sdtUINT, sdtWCHAR:                    begin TagType := pttWord;     Size := 2; end;
    sdtINT:                                        begin TagType := pttSmallInt; Size := 2; end;
    sdtDWORD, sdtUDINT:                            begin TagType := pttDWord;    Size := 4; end;
    sdtDINT:                                       begin TagType := pttLongInt;  Size := 4; end;
    sdtREAL:                                       begin TagType := pttFloat;    Size := 4; end;
    sdtLWORD, sdtULINT:                            begin TagType := pttQWord;    Size := 8; end;
    sdtLINT:                                       begin TagType := pttInt64;    Size := 8; end;
    sdtLREAL:                                      begin TagType := pttDouble;   Size := 8; end;
  else
    Result := false;
    TagType := pttDefault;
    Size := 0;
  end;
  //big-endian from the PLC: every doubling of the width needs one more swap level
  SwapBytes  := Size>=2;
  SwapWords  := Size>=4;
  SwapDWords := Size>=8;
end;

function S7PlusSoftTypeName(SoftDataType:Byte):String;
begin
  case TS7PlusSoftDataType(SoftDataType) of
    sdtBOOL, sdtBBOOL: Result := 'Bool';
    sdtBYTE:   Result := 'Byte';
    sdtCHAR:   Result := 'Char';
    sdtWORD:   Result := 'Word';
    sdtINT:    Result := 'Int';
    sdtDWORD:  Result := 'DWord';
    sdtDINT:   Result := 'DInt';
    sdtREAL:   Result := 'Real';
    sdtDATE:   Result := 'Date';
    sdtTIMEOFDAY: Result := 'Time_Of_Day';
    sdtTIME:   Result := 'Time';
    sdtS5TIME: Result := 'S5Time';
    sdtDATEANDTIME: Result := 'Date_And_Time';
    sdtSTRUCT: Result := 'Struct';
    sdtSTRING: Result := 'String';
    sdtLREAL:  Result := 'LReal';
    sdtULINT:  Result := 'ULInt';
    sdtLINT:   Result := 'LInt';
    sdtLWORD:  Result := 'LWord';
    sdtUSINT:  Result := 'USInt';
    sdtUINT:   Result := 'UInt';
    sdtUDINT:  Result := 'UDInt';
    sdtSINT:   Result := 'SInt';
    sdtWCHAR:  Result := 'WChar';
    sdtWSTRING:Result := 'WString';
    sdtLTIME:  Result := 'LTime';
    sdtLTOD:   Result := 'LTime_Of_Day';
    sdtLDT:    Result := 'LDT';
    sdtDTL:    Result := 'DTL';
  else
    Result := '?'+IntToStr(SoftDataType);
  end;
end;

//DB<digits> at the very start of Path, up to the first dot (or the whole Path)
function StartsWithDBNumber(const Path:String; out RestStart:Integer):Boolean;
var
  i:Integer;
begin
  Result := false;
  RestStart := 0;
  if (Length(Path)<3) or (UpCase(Path[1])<>'D') or (UpCase(Path[2])<>'B') then exit;
  i := 3;
  while (i<=Length(Path)) and (Path[i] in ['0'..'9']) do Inc(i);
  if i=3 then exit;
  if (i<=Length(Path)) and (Path[i]<>'.') then exit;
  RestStart := i;
  Result := true;
end;

function S7PlusClassifyVar(const V:TS7PlusVarInfo; const DBName:String):TS7PlusCandidate;
var
  RestStart, Depth, i:Integer;
begin
  Result.Path := V.Name;
  Result.SoftDataType := V.SoftDataType;
  Result.TagType := pttDefault;
  Result.SwapBytes := false;
  Result.SwapWords := false;
  Result.SwapDWords := false;
  Result.Skip := srNone;
  Result.IsArray := false;
  Result.ElementCount := 0;
  SetLength(Result.ElementPaths, 0);

  if (DBName<>'') and StartsWithDBNumber(V.Name, RestStart) then
    Result.Path := DBName + Copy(V.Name, RestStart, MaxInt);

  //members below the DB/area: count the dots outside the "[...]" labels
  Depth := 0;
  for i:=1 to Length(V.Name) do
    if V.Name[i]='.' then Inc(Depth);
  Result.Nested := Depth>1;

  if Pos('[', V.Name)>0 then begin
    Result.Skip := srArrayElement;
    exit;
  end;
  if TS7PlusSoftDataType(V.SoftDataType) in [sdtVOID, sdtSTRUCT, sdtENDSTRUCT, sdtARRAY] then begin
    Result.Skip := srNotAScalar;
    exit;
  end;
  if not S7PlusTagTypeOf(V.SoftDataType, Result.TagType, Result.SwapBytes, Result.SwapWords, Result.SwapDWords) then
    Result.Skip := srUnsupportedType;
end;

function S7PlusSuggestedName(const Path, Prefix:String):String;
var
  c:Integer;
  LastWasSep:Boolean;
begin
  Result := '';
  LastWasSep := false;
  for c:=1 to Length(Path) do begin
    if Path[c] in ['0'..'9', 'a'..'z', 'A'..'Z'] then begin
      Result := Result + Path[c];
      LastWasSep := false;
    end else begin
      if not LastWasSep then
        Result := Result + '_';
      LastWasSep := true;
    end;
  end;

  while (Length(Result)>0) and (Result[Length(Result)]='_') do
    Delete(Result, Length(Result), 1);

  Result := Trim(Prefix) + Result;

  //a component name cannot start with a digit
  if (Length(Result)>0) and (Result[1] in ['0'..'9']) then
    Result := '_' + Result;

  if Trim(Result)='' then
    Result := 'tag';
end;

end.
