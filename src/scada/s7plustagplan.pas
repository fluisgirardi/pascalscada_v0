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

  STRING vira um TPLCString (StringType=stSIEMENS): o tamanho máximo não vem na
  lista, então o driver lê o valor uma vez (ResolveStringLengths) e o primeiro byte
  dele é esse máximo; sem essa leitura (srStringLength) a STRING não vira tag.

  Tudo o que é de um DATABLOCK é importado como uma ESTRUTURA, e não como tags soltos
  (S7PlusBuildImportPlan): um TPLCStruct por DB, lido inteiro numa só varredura (o caminho é
  o nome do DB), com um TPLCStructItem (TPLCStructString para STRING) por variável marcada,
  cada um no seu byte da estrutura. As áreas I, Q e M continuam tags soltos.

  O que não vira tag, e por quê: WSTRING, tipos de data/hora, estruturas inteiras (os membros aparecem um a um),
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

  A STRING becomes a TPLCString (StringType=stSIEMENS): the maximum length is not
  on the list, so the driver reads the value once (ResolveStringLengths) and its
  first byte is that maximum; without that read (srStringLength) the STRING does
  not become a tag.

  Everything that belongs to a DATABLOCK is imported as a STRUCTURE, not as loose tags
  (S7PlusBuildImportPlan): one TPLCStruct per DB, read whole in one scan (its path is the
  DB name), with one TPLCStructItem (TPLCStructString for a STRING) per ticked variable,
  each at its own byte of the structure. The I, Q and M areas stay loose tags.

  What does not become a tag, and why: WSTRING, date/time types, whole structures (their members are offered one
  by one), array elements and internal types.
}
{$ENDIF}
unit S7PlusTagPlan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Tag, ProtocolTypes, S7PlusTypeInfo;

type
  {$IFDEF PORTUGUES}
  //: Por que uma variável do CLP não vira tag.
  {$ELSE}
  //: Why a PLC variable does not become a tag.
  {$ENDIF}
  TS7PlusSkipReason = (srNone, srNotAScalar, srUnsupportedType, srArrayElement, srStringLength);

  {$IFDEF PORTUGUES}
  //: Uma variável do CLP e o que dela dá para fazer.
  {$ELSE}
  //: A PLC variable and what can be done with it.
  {$ENDIF}
  {$IFDEF PORTUGUES}
  //: Um membro de uma estrutura que se repete num array de estruturas (o de um elemento só).
  {$ELSE}
  //: A member of the structure repeated in an array of structures (of one element only).
  {$ENDIF}
  TS7PlusStructMember = record
    //: Nome dentro do elemento (Dint1 em Arr[0].Dint1).
    Name:String;
    //: Posição, em bytes, a partir do começo do elemento.
    Offset:LongInt;
    SoftDataType:Byte;
    //: False: tipo que não vira item (data/hora, estrutura dentro da estrutura...).
    Supported:Boolean;
    TagType:TTagType;
    SwapBytes, SwapWords, SwapDWords:Boolean;
    //: STRING: vira um TPLCStructString; StringLength como em TS7PlusCandidate.
    IsString:Boolean;
    StringLength:LongInt;
    //: Onde ler o membro do primeiro elemento (para o tamanho de uma STRING).
    AccessArea:Cardinal;
    Lids:TS7PlusLIDArray;
  end;
  TS7PlusStructMemberArray = array of TS7PlusStructMember;

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
    //: Posição no layout clássico e na imagem de memória do CLP (ver TS7PlusVarInfo); -1 = desconhecida.
    ByteOffset, ImageOffset:Integer;
    //: STRING do CLP: vira um TPLCString. StringLength = o máximo de caracteres (o primeiro
    //: byte do valor), -1 enquanto não foi lido (ver TS7CommPlusDriver.ResolveStringLengths).
    IsString:Boolean;
    StringLength:LongInt;
    //: Onde ler a variável pelo caminho simbólico (área de acesso e LIDs), para o que precisa
    //: do valor (o tamanho de uma STRING). AccessArea=0: não informada.
    AccessArea:Cardinal;
    Lids:TS7PlusLIDArray;
    //: Array de estruturas: vira um TPLCStruct do array todo (ElementCount x ElementSize bytes)
    //: e, se pedido, um item por membro de cada elemento (Members é o de um elemento só).
    IsStructArray:Boolean;
    ElementSize:LongInt;
    Members:TS7PlusStructMemberArray;
  end;
  TS7PlusCandidateArray = array of TS7PlusCandidate;

  {$IFDEF PORTUGUES}
  //: Um tag que será criado no formulário/datamodule.
  {$ELSE}
  //: A tag that will be created on the form/datamodule.
  {$ENDIF}
  {$IFDEF PORTUGUES}
  //: Um item (TPLCStructItem ou TPLCStructString) de um TPLCStruct que será criado.
  {$ELSE}
  //: An item (TPLCStructItem or TPLCStructString) of a TPLCStruct to be created.
  {$ENDIF}
  TS7PlusImportStructItem = record
    CompName:String;
    Offset:LongInt;
    TagType:TTagType;
    SwapBytes, SwapWords, SwapDWords:Boolean;
    IsString:Boolean;
    StringSize:LongInt;
  end;
  TS7PlusImportStructItems = array of TS7PlusImportStructItem;

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
    //: True: um TPLCString (stSIEMENS) de StringSize caracteres.
    IsString:Boolean;
    StringSize:LongInt;
    //: True: um TPLCStruct de Size bytes (um array de estruturas) com os itens abaixo.
    IsStruct:Boolean;
    StructItems:TS7PlusImportStructItems;
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
function S7PlusClassifyVar(const V:TS7PlusVarInfo; const DBName:String; AccessArea:Cardinal=0):TS7PlusCandidate;

{$IFDEF PORTUGUES}
//: O comprimento máximo de uma STRING a partir dos bytes do valor lido: o primeiro byte é o
//: máximo e o valor tem máximo+2 bytes (máximo, atual, caracteres). False se não bate.
{$ELSE}
//: The maximum length of a STRING from the bytes of its value: the first byte is the maximum
//: and the value has maximum+2 bytes (maximum, current, characters). False if it does not fit.
{$ENDIF}
function S7PlusStringLengthOf(const Data:TBytes; out MaxLength:LongInt):Boolean;

{$IFDEF PORTUGUES}
//: Nome de componente válido a partir de um caminho (BlocoSimbolico_4.Var26.v1 ->
//: prefixoBlocoSimbolico_4_Var26_v1).
{$ELSE}
//: Valid component name from a path (BlocoSimbolico_4.Var26.v1 ->
//: prefixBlocoSimbolico_4_Var26_v1).
{$ENDIF}
function S7PlusSuggestedName(const Path, Prefix:String):String;

{$IFDEF PORTUGUES}
//: Classifica as variáveis de um DB ou área (a lista de BrowseDB/BrowseNativeArea) e as
//: acrescenta a Candidates. A raiz (o próprio DB/área) não é uma variável e fica de fora.
//: DBName: o nome simbólico do DB, ou '' para ficar com o número.
{$ELSE}
//: Classifies the variables of a DB or area (the BrowseDB/BrowseNativeArea list) and adds
//: them to Candidates. The root (the DB/area itself) is not a variable and is left out.
//: DBName: the symbolic name of the DB, or '' to keep the number.
{$ENDIF}
procedure S7PlusAddBrowsed(var Candidates:TS7PlusCandidateArray; const Vars:TS7PlusVarInfoArray;
                           const DBName:String; AccessArea:Cardinal=0);

{$IFDEF PORTUGUES}
//: A lista de candidatos no formato comum de TProtocolDriver.ListTags: os DBs (como
//: tkStructure, com o tamanho da leitura do DB inteiro) e, depois, cada variável. Elementos
//: de array ficam de fora (o array os cobre); chamar S7PlusMarkArrays antes.
{$ELSE}
//: The candidates in the common format of TProtocolDriver.ListTags: the DBs (as
//: tkStructure, with the size of a whole DB read) and then each variable. Array elements
//: are left out (the array covers them); call S7PlusMarkArrays first.
{$ENDIF}
function S7PlusTagInfosOf(const Candidates:TS7PlusCandidateArray):TPLCTagInfoArray;

{$IFDEF PORTUGUES}
//: True para o nome de uma área de processo (IArea, QArea, MArea, S7Timers, S7Counters).
{$ELSE}
//: True for the name of a process area (IArea, QArea, MArea, S7Timers, S7Counters).
{$ENDIF}
function S7PlusIsAreaName(const S:String):Boolean;

{$IFDEF PORTUGUES}
//: Tamanho em bytes de um DB (IsDBRoot) ou de uma estrutura, a partir dos membros listados.
//: Estrutura: os bytes que uma leitura dela devolve (o layout real do CLP, que num DB
//: otimizado ordena os membros por tamanho). DB: os bytes da leitura do DB inteiro (o
//: TPLCStruct), ou seja, os membros de primeiro nível nos offsets clássicos. Exact=False
//: quando há algo de tamanho desconhecido (STRING, array de estruturas): o tamanho é então
//: um mínimo.
{$ELSE}
//: Size in bytes of a DB (IsDBRoot) or of a structure, worked out from the members listed.
//: Structure: the bytes a read of it returns (the PLC's real layout, which in an optimized
//: DB orders the members by size). DB: the bytes of a whole DB read (the TPLCStruct), i.e.
//: the top level members at their classic offsets. Exact=False when something has an
//: unknown size (STRING, an array of structures): the size is then a minimum.
{$ENDIF}
function S7PlusContainerSize(const Candidates:TS7PlusCandidateArray; const Path:String;
                             IsDBRoot:Boolean; out Exact:Boolean):LongInt;

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
//: O plano de importação do Tag Builder a partir dos candidatos marcados (Checked, na ordem de
//: Candidates). Cada DB com algo marcado vira UM item IsStruct (um TPLCStruct do DB inteiro, com
//: o tamanho da leitura do DB todo) com um StructItem por variável: os de primeiro nível no byte
//: clássico (ByteOffset); os de dentro de uma estrutura no byte dela mais a distância na imagem
//: de memória (a estrutura é lida com o layout real do CLP); os elementos de arrays e de arrays
//: de estruturas só com CreateElements. Fora de um DB (áreas I, Q, M) cada variável é um tag solto
//: (TPLCTagNumber, TPLCBlock ou TPLCString).
{$ELSE}
//: The Tag Builder import plan from the ticked candidates (Checked, in the order of Candidates).
//: Every DB with something ticked becomes ONE IsStruct item (a TPLCStruct of the whole DB, sized
//: as a whole DB read) with a StructItem per variable: top level ones at their classic byte
//: (ByteOffset); the ones inside a structure at its byte plus their distance in the memory image
//: (the structure is read in the PLC's real layout); the elements of arrays and of arrays of
//: structures only with CreateElements. Outside a DB (the I, Q, M areas) each variable is a loose
//: tag (TPLCTagNumber, TPLCBlock or TPLCString).
{$ENDIF}
function S7PlusBuildImportPlan(const Candidates:TS7PlusCandidateArray; const Checked:array of Boolean;
                               const Prefix:String; Scan:LongInt; CreateElements:Boolean):TS7PlusImportItems;

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

//bytes a variable takes in the block that holds it; Exact=False when its width is not known
function LeafSize(const C:TS7PlusCandidate; var Exact:Boolean):LongInt;
var
  One:LongInt;
begin
  One := S7PlusDataTypeSize(C.SoftDataType, 0);
  if C.IsStructArray then
    One := C.ElementSize
  else if C.IsString and (C.StringLength>=0) then
    One := C.StringLength+2     //read from the PLC: maximum, current length, characters
  else if TS7PlusSoftDataType(C.SoftDataType) in [sdtSTRING, sdtWSTRING] then
    Exact := false;      //the maximum length is not known
  if One=0 then
    Exact := false;
  if C.IsStructArray then
    Result := C.ElementCount*C.ElementSize
  else if C.IsArray then
    Result := C.ElementCount*One
  else
    Result := One;
end;

function StructSizeOf(const Candidates:TS7PlusCandidateArray; const Path:String;
                      var Exact:Boolean):LongInt; forward;

function S7PlusContainerSize(const Candidates:TS7PlusCandidateArray; const Path:String;
  IsDBRoot:Boolean; out Exact:Boolean):LongInt;
var
  i, Extent, ChildSize:LongInt;
  Prefix, Rest:String;
begin
  Exact := true;
  Result := 0;
  if not IsDBRoot then begin
    Result := StructSizeOf(Candidates, Path, Exact);
    exit;
  end;

  //a whole DB read puts every top level member at its classic offset
  Prefix := Path+'.';
  Extent := 0;
  for i:=0 to High(Candidates) do begin
    if Pos(Prefix, Candidates[i].Path)<>1 then continue;
    Rest := Copy(Candidates[i].Path, Length(Prefix)+1, MaxInt);
    if (Rest='') or (Pos('.', Rest)>0) or (Candidates[i].Skip=srArrayElement) then continue;
    if Candidates[i].ByteOffset<0 then begin
      Exact := false;
      continue;
    end;
    if Candidates[i].Skip=srNotAScalar then
      ChildSize := StructSizeOf(Candidates, Candidates[i].Path, Exact)
    else
      ChildSize := LeafSize(Candidates[i], Exact);
    if Candidates[i].ByteOffset+ChildSize>Extent then
      Extent := Candidates[i].ByteOffset+ChildSize;
  end;
  Result := Extent;
end;

function StructSizeOf(const Candidates:TS7PlusCandidateArray; const Path:String;
  var Exact:Boolean):LongInt;
var
  i, Self_, Extent:LongInt;
  Prefix, Rest:String;
begin
  Result := 0;
  Self_ := -1;
  for i:=0 to High(Candidates) do
    if Candidates[i].Path=Path then Self_ := i;
  if (Self_<0) or (Candidates[Self_].ImageOffset<0) then begin
    Exact := false;
    exit;
  end;

  Prefix := Path+'.';
  Extent := 0;
  for i:=0 to High(Candidates) do begin
    if Pos(Prefix, Candidates[i].Path)<>1 then continue;
    //a structure inside is measured through its own leaves
    if Candidates[i].Skip=srNotAScalar then continue;
    if Candidates[i].Skip=srArrayElement then begin
      //an element of an array of scalars is covered by its array; one of an array of
      //structures ("T[0].x") is not known
      Rest := Copy(Candidates[i].Path, Length(Prefix)+1, MaxInt);
      if (Rest='') or (Rest[Length(Rest)]<>']') then Exact := false;
      continue;
    end;
    //an array of structures counts in the classic layout; in the real image it may be laid out
    //differently
    if Candidates[i].IsStructArray then
      Exact := false;
    if Candidates[i].ImageOffset<0 then begin
      Exact := false;
      continue;
    end;
    if Candidates[i].ImageOffset-Candidates[Self_].ImageOffset+LeafSize(Candidates[i], Exact)>Extent then
      Extent := Candidates[i].ImageOffset-Candidates[Self_].ImageOffset+LeafSize(Candidates[i], Exact);
  end;
  Result := Extent;
end;

procedure S7PlusAddBrowsed(var Candidates:TS7PlusCandidateArray; const Vars:TS7PlusVarInfoArray;
  const DBName:String; AccessArea:Cardinal);
var
  c, n:Integer;
begin
  for c:=0 to High(Vars) do begin
    //the DB/area root itself ("DB4") is not a variable
    if Pos('.', Vars[c].Name)=0 then continue;
    n := Length(Candidates);
    SetLength(Candidates, n+1);
    Candidates[n] := S7PlusClassifyVar(Vars[c], DBName, AccessArea);
  end;
end;

function JoinLevels(const Levels:TStringArray; Count:Integer):String;
var
  i:Integer;
begin
  Result := '';
  for i:=0 to Count-1 do begin
    if i>0 then Result := Result+'.';
    Result := Result+Levels[i];
  end;
end;

function S7PlusTagInfosOf(const Candidates:TS7PlusCandidateArray):TPLCTagInfoArray;
var
  i, n:Integer;
  Levels:TStringArray;
  Seen:TStringList;
  Exact:Boolean;
  Info:TPLCTagInfo;
begin
  SetLength(Result, 0);
  Seen := TStringList.Create;
  try
    Seen.CaseSensitive := false;

    //the DBs first, each one a structure the size of a whole DB read
    for i:=0 to High(Candidates) do begin
      Levels := S7PlusSplitPath(Candidates[i].Path);
      if (Length(Levels)=0) or S7PlusIsAreaName(Levels[0]) or (Seen.IndexOf(Levels[0])>=0) then continue;
      Seen.Add(Levels[0]);
      FillChar(Info, SizeOf(Info), 0);
      Info.Path := Levels[0];
      Info.Name := Levels[0];
      Info.Group := Levels[0];
      Info.Kind := tkStructure;
      Info.NativeType := 'DB';
      Info.TagType := pttDefault;
      Info.ElementCount := 1;
      Info.SizeInBytes := S7PlusContainerSize(Candidates, Levels[0], true, Exact);
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := Info;
    end;

    for i:=0 to High(Candidates) do begin
      //an element of an array is covered by the array
      if Candidates[i].Skip=srArrayElement then continue;
      Levels := S7PlusSplitPath(Candidates[i].Path);
      if Length(Levels)=0 then continue;

      FillChar(Info, SizeOf(Info), 0);
      Info.Path := Candidates[i].Path;
      Info.Name := Levels[High(Levels)];
      Info.ParentPath := JoinLevels(Levels, High(Levels));
      Info.Group := Levels[0];
      Info.NativeType := S7PlusSoftTypeName(Candidates[i].SoftDataType);
      Info.TagType := pttDefault;
      Info.ElementCount := 1;
      n := S7PlusDataTypeSize(Candidates[i].SoftDataType, 0);

      case Candidates[i].Skip of
        srNone: begin
          Info.TagType := Candidates[i].TagType;
          Info.SwapBytes := Candidates[i].SwapBytes;
          Info.SwapWords := Candidates[i].SwapWords;
          Info.SwapDWords := Candidates[i].SwapDWords;
          if Candidates[i].IsStructArray then begin
            Info.Kind := tkStructure;
            Info.TagType := pttDefault;
            Info.ElementCount := Candidates[i].ElementCount;
            Info.SizeInBytes := Candidates[i].ElementCount*Candidates[i].ElementSize;
          end else if Candidates[i].IsString then begin
            Info.Kind := tkString;
            Info.TagType := pttDefault;
            if Candidates[i].StringLength>=0 then begin
              Info.ElementCount := Candidates[i].StringLength;
              Info.SizeInBytes := Candidates[i].StringLength+2;
            end else
              Info.ElementCount := 0;     //not read from the PLC
          end else if Candidates[i].IsArray then begin
            Info.Kind := tkArray;
            Info.ElementCount := Candidates[i].ElementCount;
            Info.SizeInBytes := Candidates[i].ElementCount*n;
          end else begin
            Info.Kind := tkScalar;
            Info.SizeInBytes := n;
          end;
        end;
        srNotAScalar: begin
          Info.Kind := tkStructure;
          Info.SizeInBytes := S7PlusContainerSize(Candidates, Candidates[i].Path, false, Exact);
        end;
        srStringLength: begin
          Info.Kind := tkString;
          Info.ElementCount := 0;
        end;
        srUnsupportedType: begin
          if TS7PlusSoftDataType(Candidates[i].SoftDataType) in [sdtSTRING, sdtWSTRING] then begin
            Info.Kind := tkString;
            Info.ElementCount := 0;     //the maximum length is not on the list
          end else begin
            Info.Kind := tkUnsupported;
            Info.SizeInBytes := n;
          end;
        end;
      end;

      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := Info;
    end;
  finally
    Seen.Free;
  end;
end;

function StructArrayOf(var Candidates:TS7PlusCandidateArray; ArrIdx:Integer):Boolean; forward;

procedure S7PlusMarkArrays(var Candidates:TS7PlusCandidateArray);
var
  i, j, Count:Integer;
  Rest:String;
begin
  for i:=0 to High(Candidates) do begin
    if Candidates[i].Skip<>srNone then continue;
    if Candidates[i].IsStructArray then continue;      //found already: its elements are structures
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
    if (Count>0) and Candidates[i].IsString then begin
      //an array of STRINGs: not a tag (one TPLCString reads one string)
      Candidates[i].IsString := false;
      Candidates[i].Skip := srUnsupportedType;
    end else if Count>0 then begin
      Candidates[i].IsArray := true;
      Candidates[i].ElementCount := Count;
    end;
  end;

  //arrays of structures: the array is listed as a structure, and its elements right after it
  for i:=0 to High(Candidates) do
    if (Candidates[i].Skip=srNotAScalar) and (not Candidates[i].IsStructArray) and
       (TS7PlusSoftDataType(Candidates[i].SoftDataType)=sdtSTRUCT) and
       (Candidates[i].ByteOffset>=0) and (Pos('[', Candidates[i].Path)=0) and
       StructArrayOf(Candidates, i) then begin
      Candidates[i].IsStructArray := true;
      Candidates[i].Skip := srNone;
    end;
end;

//The structure repeated in an array of structures, from the members of its first element.
//False when it has something this does not handle (a structure or an array inside it).
function StructArrayOf(var Candidates:TS7PlusCandidateArray; ArrIdx:Integer):Boolean;
var
  j, k, Count, ElemStart, Extent, End_:Integer;
  Rest, Prefix0:String;
  ElemIdx:array of Integer;
  M:TS7PlusStructMember;
begin
  Result := false;
  SetLength(ElemIdx, 0);
  Count := 0;
  j := ArrIdx+1;
  while (j<=High(Candidates)) and (Pos(Candidates[ArrIdx].Path+'[', Candidates[j].Path)=1) do begin
    Rest := Copy(Candidates[j].Path, Length(Candidates[ArrIdx].Path)+1, MaxInt);
    //"[n]" with nothing after the bracket: an element
    if (Rest<>'') and (Rest[Length(Rest)]=']') and (Pos('.', Rest)=0) and
       (TS7PlusSoftDataType(Candidates[j].SoftDataType)=sdtSTRUCT) then begin
      SetLength(ElemIdx, Count+1);
      ElemIdx[Count] := j;
      Inc(Count);
    end;
    Inc(j);
  end;
  if Count=0 then exit;
  if Candidates[ElemIdx[0]].ByteOffset<0 then exit;   //offsets inside the array not known

  ElemStart := Candidates[ElemIdx[0]].ByteOffset;
  Prefix0 := Candidates[ElemIdx[0]].Path+'.';
  Extent := 0;
  SetLength(Candidates[ArrIdx].Members, 0);
  for k:=ElemIdx[0]+1 to High(Candidates) do begin
    if Pos(Prefix0, Candidates[k].Path)<>1 then break;
    Rest := Copy(Candidates[k].Path, Length(Prefix0)+1, MaxInt);
    //a structure or an array inside the element: not handled
    if (Pos('.', Rest)>0) or (Pos('[', Rest)>0) or
       (TS7PlusSoftDataType(Candidates[k].SoftDataType)=sdtSTRUCT) then exit;
    if Candidates[k].ByteOffset<0 then exit;

    Finalize(M);                //it holds strings and an array: FillChar alone would leak the last ones
    FillChar(M, SizeOf(M), 0);
    M.Name := Rest;
    M.Offset := Candidates[k].ByteOffset-ElemStart;
    M.SoftDataType := Candidates[k].SoftDataType;
    M.StringLength := -1;
    M.AccessArea := Candidates[k].AccessArea;
    M.Lids := Candidates[k].Lids;
    if TS7PlusSoftDataType(M.SoftDataType)=sdtSTRING then begin
      M.IsString := true;
      M.Supported := true;
      End_ := M.Offset;      //its size is added once the length is read
    end else begin
      M.Supported := S7PlusTagTypeOf(M.SoftDataType, M.TagType, M.SwapBytes, M.SwapWords, M.SwapDWords);
      End_ := M.Offset+S7PlusDataTypeSize(M.SoftDataType, 0);
    end;
    if End_>Extent then Extent := End_;
    SetLength(Candidates[ArrIdx].Members, Length(Candidates[ArrIdx].Members)+1);
    Candidates[ArrIdx].Members[High(Candidates[ArrIdx].Members)] := M;
  end;

  Candidates[ArrIdx].ElementCount := Count;
  SetLength(Candidates[ArrIdx].ElementPaths, Count);
  for k:=0 to Count-1 do
    Candidates[ArrIdx].ElementPaths[k] := Candidates[ElemIdx[k]].Path;
  //the size of an element: what separates the first two (the PLC's own stride), or the end of
  //its last member rounded up to an even number of bytes
  if (Count>=2) and (Candidates[ElemIdx[1]].ByteOffset>ElemStart) then
    Candidates[ArrIdx].ElementSize := Candidates[ElemIdx[1]].ByteOffset-ElemStart
  else begin
    if Odd(Extent) then Inc(Extent);
    Candidates[ArrIdx].ElementSize := Extent;
  end;
  Result := true;
end;

//The byte of a variable of a DB in the structure that reads the whole DB: a top level variable
//is at its classic byte; one inside a structure is at the byte of the top level structure (read
//whole, in the PLC's real layout) plus its distance to it in the memory image.
function DBOffsetOf(const Candidates:TS7PlusCandidateArray; Idx:Integer; out Off:Integer):Boolean;
var
  Levels:TStringArray;
  T:Integer;
  TPath:String;
begin
  Result := false;
  Off := 0;
  Levels := S7PlusSplitPath(Candidates[Idx].Path);
  if Length(Levels)<2 then exit;
  if Length(Levels)=2 then begin
    if Candidates[Idx].ByteOffset<0 then exit;
    Off := Candidates[Idx].ByteOffset;
    Result := true;
    exit;
  end;
  TPath := Levels[0]+'.'+Levels[1];
  for T:=0 to High(Candidates) do
    if Candidates[T].Path=TPath then begin
      if (Candidates[T].ByteOffset<0) or (Candidates[T].ImageOffset<0) or (Candidates[Idx].ImageOffset<0) then exit;
      Off := Candidates[T].ByteOffset+(Candidates[Idx].ImageOffset-Candidates[T].ImageOffset);
      Result := true;
      exit;
    end;
end;

function S7PlusBuildImportPlan(const Candidates:TS7PlusCandidateArray; const Checked:array of Boolean;
  const Prefix:String; Scan:LongInt; CreateElements:Boolean):TS7PlusImportItems;
var
  Plan:TS7PlusImportItems;
  Roots:TStringList;
  Ends:array of LongInt;       //per plan entry that is a structure: where its last item ends
  c, k, m, p, n, Off, ElemSize, FinalCount:Integer;
  Levels:TStringArray;
  Exact:Boolean;
  Size:LongInt;

  function NewEntry:Integer;
  begin
    Result := Length(Plan);
    SetLength(Plan, Result+1);
    SetLength(Ends, Result+1);
    FillChar(Plan[Result], SizeOf(Plan[Result]), 0);
    Ends[Result] := 0;
    Plan[Result].Scan := Scan;
  end;

  procedure AddItem(Entry:Integer; const AName:String; AOffset:LongInt; ATagType:TTagType;
                    ASwapB, ASwapW, ASwapD, AIsString:Boolean; AStringSize, AByteSize:LongInt);
  var
    q:Integer;
  begin
    q := Length(Plan[Entry].StructItems);
    SetLength(Plan[Entry].StructItems, q+1);
    Plan[Entry].StructItems[q].CompName   := AName;
    Plan[Entry].StructItems[q].Offset     := AOffset;
    Plan[Entry].StructItems[q].TagType    := ATagType;
    Plan[Entry].StructItems[q].SwapBytes  := ASwapB;
    Plan[Entry].StructItems[q].SwapWords  := ASwapW;
    Plan[Entry].StructItems[q].SwapDWords := ASwapD;
    Plan[Entry].StructItems[q].IsString   := AIsString;
    Plan[Entry].StructItems[q].StringSize := AStringSize;
    if AOffset+AByteSize>Ends[Entry] then Ends[Entry] := AOffset+AByteSize;
  end;

begin
  SetLength(Plan, 0);
  SetLength(Ends, 0);
  Roots := TStringList.Create;
  try
    Roots.CaseSensitive := false;
    for c:=0 to High(Candidates) do begin
      if (c>High(Checked)) or (not Checked[c]) then continue;
      if Candidates[c].Skip<>srNone then continue;
      Levels := S7PlusSplitPath(Candidates[c].Path);
      if Length(Levels)=0 then continue;

      if S7PlusIsAreaName(Levels[0]) then begin
        //an area: loose tags, as always
        n := NewEntry;
        Plan[n].Path       := Candidates[c].Path;
        Plan[n].CompName   := S7PlusSuggestedName(Candidates[c].Path, Prefix);
        Plan[n].TagType    := Candidates[c].TagType;
        Plan[n].SwapBytes  := Candidates[c].SwapBytes;
        Plan[n].SwapWords  := Candidates[c].SwapWords;
        Plan[n].SwapDWords := Candidates[c].SwapDWords;
        Plan[n].IsBlock    := Candidates[c].IsArray;
        Plan[n].IsString   := Candidates[c].IsString;
        Plan[n].StringSize := Candidates[c].StringLength;
        Plan[n].Size       := 1;
        if Candidates[c].IsArray then begin
          Plan[n].Size := Candidates[c].ElementCount;
          if CreateElements then begin
            SetLength(Plan[n].ElementNames, Length(Candidates[c].ElementPaths));
            for k:=0 to High(Candidates[c].ElementPaths) do
              Plan[n].ElementNames[k] := S7PlusSuggestedName(Candidates[c].ElementPaths[k], Prefix);
          end;
        end;
        continue;
      end;

      //a variable of a DB: an item of the structure of that DB
      p := Roots.IndexOf(Levels[0]);
      if p<0 then begin
        n := NewEntry;
        Plan[n].Path     := Levels[0];
        Plan[n].CompName := S7PlusSuggestedName(Levels[0], Prefix);
        Plan[n].IsStruct := true;
        p := Roots.AddObject(Levels[0], TObject(PtrInt(n)));
      end;
      n := PtrInt(Roots.Objects[p]);

      if Candidates[c].IsStructArray then begin
        //an array of structures: an item per member of each element (classic bytes)
        if CreateElements and (Length(Levels)=2) and (Candidates[c].ByteOffset>=0) then
          for k:=0 to Candidates[c].ElementCount-1 do
            for m:=0 to High(Candidates[c].Members) do begin
              if not Candidates[c].Members[m].Supported then continue;
              if Candidates[c].Members[m].IsString and (Candidates[c].Members[m].StringLength<0) then continue;
              Off := Candidates[c].ByteOffset+k*Candidates[c].ElementSize+Candidates[c].Members[m].Offset;
              if Candidates[c].Members[m].IsString then
                AddItem(n, S7PlusSuggestedName(Candidates[c].ElementPaths[k]+'.'+Candidates[c].Members[m].Name, Prefix),
                        Off, pttDefault, false, false, false, true, Candidates[c].Members[m].StringLength,
                        Candidates[c].Members[m].StringLength+2)
              else
                AddItem(n, S7PlusSuggestedName(Candidates[c].ElementPaths[k]+'.'+Candidates[c].Members[m].Name, Prefix),
                        Off, Candidates[c].Members[m].TagType, Candidates[c].Members[m].SwapBytes,
                        Candidates[c].Members[m].SwapWords, Candidates[c].Members[m].SwapDWords, false, 0,
                        S7PlusDataTypeSize(Candidates[c].Members[m].SoftDataType, 0));
            end;
        continue;
      end;

      if not DBOffsetOf(Candidates, c, Off) then continue;

      if Candidates[c].IsArray then begin
        //an array of scalars: one item per element
        ElemSize := S7PlusDataTypeSize(Candidates[c].SoftDataType, 0);
        if CreateElements and (ElemSize>0) then
          for k:=0 to High(Candidates[c].ElementPaths) do
            AddItem(n, S7PlusSuggestedName(Candidates[c].ElementPaths[k], Prefix), Off+k*ElemSize,
                    Candidates[c].TagType, Candidates[c].SwapBytes, Candidates[c].SwapWords,
                    Candidates[c].SwapDWords, false, 0, ElemSize);
      end else if Candidates[c].IsString then begin
        if Candidates[c].StringLength>=0 then
          AddItem(n, S7PlusSuggestedName(Candidates[c].Path, Prefix), Off, pttDefault, false, false, false,
                  true, Candidates[c].StringLength, Candidates[c].StringLength+2);
      end else
        AddItem(n, S7PlusSuggestedName(Candidates[c].Path, Prefix), Off, Candidates[c].TagType,
                Candidates[c].SwapBytes, Candidates[c].SwapWords, Candidates[c].SwapDWords, false, 0,
                S7PlusDataTypeSize(Candidates[c].SoftDataType, 0));
    end;

    //the size of each structure: a whole DB read, or where its last item ends if that is further
    FinalCount := 0;
    SetLength(Result, Length(Plan));
    for c:=0 to High(Plan) do begin
      if Plan[c].IsStruct then begin
        if Length(Plan[c].StructItems)=0 then continue;      //nothing to put in it
        Size := S7PlusContainerSize(Candidates, Plan[c].Path, true, Exact);
        if Ends[c]>Size then Size := Ends[c];
        Plan[c].Size := Size;
      end;
      Result[FinalCount] := Plan[c];
      Inc(FinalCount);
    end;
    SetLength(Result, FinalCount);
  finally
    Roots.Free;
  end;
end;

function S7PlusIsAreaName(const S:String):Boolean;
begin
  Result := SameText(S, 'IArea') or SameText(S, 'QArea') or SameText(S, 'MArea') or
            SameText(S, 'S7Timers') or SameText(S, 'S7Counters');
end;

function S7PlusSplitPath(const Path:String):TStringArray;
var
  i, Depth, Start, FirstDot:Integer;
begin
  SetLength(Result, 0);
  if Path='' then exit;
  //a process area holds flat, named tags, and a tag name may have dots in it
  //(MArea.Clock_2.5Hz): the area is one level and the whole name the other
  FirstDot := Pos('.', Path);
  if (FirstDot>0) and S7PlusIsAreaName(Copy(Path, 1, FirstDot-1)) then begin
    SetLength(Result, 2);
    Result[0] := Copy(Path, 1, FirstDot-1);
    Result[1] := Copy(Path, FirstDot+1, MaxInt);
    exit;
  end;
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

function S7PlusClassifyVar(const V:TS7PlusVarInfo; const DBName:String; AccessArea:Cardinal):TS7PlusCandidate;
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
  Result.ByteOffset := V.ByteOffset;
  Result.ImageOffset := V.ImageOffset;
  Result.IsString := false;
  Result.StringLength := -1;
  Result.AccessArea := AccessArea;
  Result.Lids := V.Lids;
  Result.IsStructArray := false;
  Result.ElementSize := 0;
  SetLength(Result.Members, 0);

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
  if TS7PlusSoftDataType(V.SoftDataType)=sdtSTRING then begin
    //a TPLCString; its length comes from reading the value (ResolveStringLengths)
    Result.IsString := true;
    exit;
  end;
  if not S7PlusTagTypeOf(V.SoftDataType, Result.TagType, Result.SwapBytes, Result.SwapWords, Result.SwapDWords) then
    Result.Skip := srUnsupportedType;
end;

function S7PlusStringLengthOf(const Data:TBytes; out MaxLength:LongInt):Boolean;
begin
  MaxLength := -1;
  Result := (Length(Data)>=2) and (Length(Data)=Data[0]+2);
  if Result then
    MaxLength := Data[0];
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
    end else if (Path[c]='-') and (c>1) and (Path[c-1] in ['[', ',']) and (c<Length(Path)) and
                (Path[c+1] in ['0'..'9']) then begin
      //a negative index (Arr[-2]) is not the same name as the positive one (Arr[2])
      if not LastWasSep then
        Result := Result + '_';
      Result := Result + 'm';
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
