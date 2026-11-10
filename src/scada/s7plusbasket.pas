{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(A cesta do Tag Builder do S7CommPlus: o que o usuario arrastou e como vira componentes.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  A cesta substitui as marcas por modo. O usuario arrasta variaveis (ou um DB) para a cesta e,
  em cada entrada, escolhe nome, scan time e, para blocos e estruturas, se os itens filhos sao
  mapeados. O que a entrada vira depende so do que foi arrastado:
  @unorderedList(
    @item(variavel simples: TPLCTagNumber (TPLCString para STRING);)
    @item(array de variaveis simples: TPLCBlock;)
    @item(estrutura, array de estruturas ou DB: TPLCStruct.)
  )
  Com os filhos mapeados, cada BOOL vira um TTagBit sobre um item (ou elemento) de byte de apoio,
  sempre criado, porque o driver le um BOOL como o byte inteiro.

  Nao precisa de CLP nem do IDE: so decide, sobre os candidatos de S7PlusTagPlan, o que criar.
}
{$ELSE}
{:
  @abstract(The S7CommPlus Tag Builder basket: what the user dragged and how it becomes components.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  The basket replaces the ticks per mode. The user drags variables (or a DB) into the basket and,
  on each entry, picks name, scan time and, for blocks and structures, whether the child items
  are mapped. What an entry becomes depends only on what was dragged:
  @unorderedList(
    @item(a simple variable: TPLCTagNumber (TPLCString for a STRING);)
    @item(an array of simple variables: TPLCBlock;)
    @item(a structure, an array of structures or a DB: TPLCStruct.)
  )
  With the children mapped, each BOOL becomes a TTagBit over a support byte item (or element),
  always created, because the driver reads a BOOL as the whole byte.

  Needs neither a PLC nor the IDE: it only decides, over the candidates of S7PlusTagPlan, what
  to create.
}
{$ENDIF}
unit S7PlusBasket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Tag, S7PlusTypeInfo, S7PlusTagPlan;

type
  {$IFDEF PORTUGUES}
  //: O que uma entrada da cesta vira: bkTag (TPLCTagNumber/TPLCString), bkBlock (TPLCBlock), bkStruct (TPLCStruct).
  {$ELSE}
  //: What a basket entry becomes: bkTag (TPLCTagNumber/TPLCString), bkBlock (TPLCBlock), bkStruct (TPLCStruct).
  {$ENDIF}
  TS7PlusBasketKind = (bkTag, bkBlock, bkStruct);

  {$IFDEF PORTUGUES}
  //: Uma entrada da cesta, com as opcoes dela.
  {$ELSE}
  //: A basket entry, with its own options.
  {$ENDIF}
  TS7PlusBasketEntry = record
    //: Caminho do candidato arrastado; com IsDB, o nome do DB.
    Path:String;
    //: True: um DB inteiro (um TPLCStruct lido pelo nome do DB).
    IsDB:Boolean;
    Kind:TS7PlusBasketKind;
    //: Nome do componente; vazio: o sugerido a partir de Prefix e do caminho.
    Name:String;
    //: Prefixo do nome sugerido e dos nomes dos filhos.
    Prefix:String;
    //: Tempo de atualizacao em ms; 0 = por mudanca (subscricao).
    Scan:LongInt;
    //: Blocos e estruturas: True cria os itens filhos (TPLCBlockElement/TPLCStructItem, e os
    //: TTagBit dos BOOL); False cria so o bloco/estrutura.
    MapChildren:Boolean;
  end;
  TS7PlusBasketEntries = array of TS7PlusBasketEntry;

  TS7PlusNameProblemKind = (npInvalid, npDuplicate, npExists);

  {$IFDEF PORTUGUES}
  //: Um problema de nome: invalido, repetido na cesta ou ja usado por um componente existente.
  {$ELSE}
  //: A name problem: invalid, repeated in the basket or already used by an existing component.
  {$ENDIF}
  TS7PlusNameProblem = record
    Name:String;
    Kind:TS7PlusNameProblemKind;
  end;
  TS7PlusNameProblems = array of TS7PlusNameProblem;

{$IFDEF PORTUGUES}
//: Se o candidato Idx pode ser arrastado para a cesta e no que vira. Nao podem: o que nao e
//: uma variavel de tipo suportado, WSTRING, datas, estruturas dentro de um elemento de array, etc.
{$ELSE}
//: Whether candidate Idx can be dragged into the basket and what it becomes. Not draggable:
//: what is not a variable of a supported type, WSTRING, dates, structures inside an array
//: element, etc.
{$ENDIF}
function S7PlusBasketKindOf(const Candidates:TS7PlusCandidateArray; Idx:Integer;
                            out Kind:TS7PlusBasketKind):Boolean;

{$IFDEF PORTUGUES}
//: A entrada de um candidato arrastado, com as opcoes dadas. False se ele nao pode entrar.
{$ELSE}
//: The entry of a dragged candidate, with the given options. False if it cannot go in.
{$ENDIF}
function S7PlusBasketNewEntry(const Candidates:TS7PlusCandidateArray; Idx:Integer; const Prefix:String;
                              Scan:LongInt; MapChildren:Boolean; out Entry:TS7PlusBasketEntry):Boolean;

{$IFDEF PORTUGUES}
//: A entrada de um DB inteiro (bkStruct). Areas (IArea, QArea, MArea...) nao viram entrada.
{$ELSE}
//: The entry of a whole DB (bkStruct). Areas (IArea, QArea, MArea...) do not become an entry.
{$ENDIF}
function S7PlusBasketNewDBEntry(const DBKey, Prefix:String; Scan:LongInt; MapChildren:Boolean;
                                out Entry:TS7PlusBasketEntry):Boolean;

{$IFDEF PORTUGUES}
//: Os componentes que a cesta cria: um item de plano por entrada (na ordem da cesta; entrada
//: cujo candidato nao existe mais fica de fora). Nome e Scan da entrada valem; os filhos usam
//: o Prefix dela. Com MapChildren, cada BOOL vira um TTagBit (Bits) sobre um item de byte
//: (StructItems, nomeado <Nome>_Byte<offset>) ou um elemento de byte (ElementNames, <nome>_Byte).
{$ELSE}
//: The components the basket creates: a plan item per entry (in basket order; an entry whose
//: candidate no longer exists is left out). The entry's Name and Scan apply; the children use
//: its Prefix. With MapChildren each BOOL becomes a TTagBit (Bits) over a byte item
//: (StructItems, named <Name>_Byte<offset>) or a byte element (ElementNames, <name>_Byte).
{$ENDIF}
function S7PlusBasketBuild(const Candidates:TS7PlusCandidateArray; const Entries:TS7PlusBasketEntries):TS7PlusImportItems;

{$IFDEF PORTUGUES}
//: Os problemas de nome do que a cesta cria: nome que nao e um identificador, nome repetido
//: (a comparacao ignora maiusculas, como o Pascal) ou ja usado por um componente existente
//: (Existing). Cada nome problematico entra uma vez por tipo de problema.
{$ELSE}
//: The name problems of what the basket creates: a name that is not an identifier, a repeated
//: name (the comparison ignores case, like Pascal) or one already used by an existing component
//: (Existing). Each problematic name is listed once per kind of problem.
{$ENDIF}
function S7PlusBasketNameProblems(const Items:TS7PlusImportItems; const Existing:array of String):TS7PlusNameProblems;

{$IFDEF PORTUGUES}
//: Um nome a partir de um modelo: {path} (o caminho todo, no formato dos nomes sugeridos),
//: {name} (o último nível), {db} (o primeiro nível) e {n} (N). O resultado não é validado.
{$ELSE}
//: A name from a template: {path} (the whole path, in the format of the suggested names),
//: {name} (the last level), {db} (the first level) and {n} (N). The result is not validated.
{$ENDIF}
function S7PlusBasketApplyTemplate(const Template, Path:String; N:LongInt):String;

{$IFDEF PORTUGUES}
//: Se o tipo do CLP está na lista de nomes separados por vírgula (Real, DInt...; a comparação
//: ignora maiúsculas). Lista vazia aceita todos.
{$ELSE}
//: Whether the PLC type is in the comma separated list of names (Real, DInt...; the comparison
//: ignores case). An empty list accepts all.
{$ENDIF}
function S7PlusTypeListMatches(const List:String; SoftDataType:Byte):Boolean;

implementation

function S7PlusBasketApplyTemplate(const Template, Path:String; N:LongInt):String;
var
  Levels:TStringArray;
begin
  Levels := S7PlusSplitPath(Path);
  Result := StringReplace(Template, '{path}', S7PlusSuggestedName(Path, ''), [rfReplaceAll, rfIgnoreCase]);
  if Length(Levels)>0 then begin
    Result := StringReplace(Result, '{name}', S7PlusSuggestedName(Levels[High(Levels)], ''), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '{db}', S7PlusSuggestedName(Levels[0], ''), [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '{n}', IntToStr(N), [rfReplaceAll, rfIgnoreCase]);
end;

function S7PlusTypeListMatches(const List:String; SoftDataType:Byte):Boolean;
var
  Items:TStringList;
  i:Integer;
begin
  Result := Trim(List)='';
  if Result then exit;
  Items := TStringList.Create;
  try
    Items.StrictDelimiter := true;
    Items.Delimiter := ',';
    Items.DelimitedText := List;
    for i:=0 to Items.Count-1 do
      if SameText(Trim(Items[i]), S7PlusSoftTypeName(SoftDataType)) then begin
        Result := true;
        exit;
      end;
  finally
    Items.Free;
  end;
end;

function FindCandidate(const Candidates:TS7PlusCandidateArray; const Path:String):Integer;
var
  i:Integer;
begin
  for i:=0 to High(Candidates) do
    if SameText(Candidates[i].Path, Path) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

//The array of structures a candidate is an element of (Arr[1]); False if it is not one.
function StructArrayHeadOf(const Candidates:TS7PlusCandidateArray; Idx:Integer; out Head:Integer):Boolean;
var
  C:TS7PlusCandidate;
  j, k:Integer;
begin
  Result := false;
  Head := -1;
  C := Candidates[Idx];
  if (C.Skip<>srArrayElement) or (TS7PlusSoftDataType(C.SoftDataType)<>sdtSTRUCT) then exit;
  if (C.Path='') or (C.Path[Length(C.Path)]<>']') then exit;
  for j:=Idx-1 downto 0 do begin
    if (not Candidates[j].IsStructArray) or (Pos(Candidates[j].Path+'[', C.Path)<>1) then continue;
    if Candidates[j].ElementSize<=0 then continue;
    for k:=0 to High(Candidates[j].ElementPaths) do
      if SameText(Candidates[j].ElementPaths[k], C.Path) then begin
        Head := j;
        Result := true;
        exit;
      end;
  end;
end;

function S7PlusBasketKindOf(const Candidates:TS7PlusCandidateArray; Idx:Integer;
  out Kind:TS7PlusBasketKind):Boolean;
var
  Head:Integer;
  C:TS7PlusCandidate;
begin
  Result := false;
  Kind := bkTag;
  if (Idx<0) or (Idx>High(Candidates)) then exit;
  C := Candidates[Idx];
  if S7PlusIsStructArrayHead(C) then begin
    Kind := bkStruct;
    Result := (C.ElementSize>0) and (C.ElementCount>0);
  end else if StructArrayHeadOf(Candidates, Idx, Head) then begin
    //one element of an array of structures: a TPLCStruct of its own, read by its path
    Kind := bkStruct;
    Result := true;
  end else if S7PlusSelectable(C, imArray, false) then begin
    Kind := bkBlock;
    Result := true;
  end else if S7PlusSelectable(C, imStruct, false) then begin
    Kind := bkStruct;
    Result := true;
  end else if S7PlusSelectable(C, imSimple, false) then begin
    Kind := bkTag;
    Result := not (C.IsString and (C.StringLength<0));
  end;
end;

function S7PlusBasketNewEntry(const Candidates:TS7PlusCandidateArray; Idx:Integer; const Prefix:String;
  Scan:LongInt; MapChildren:Boolean; out Entry:TS7PlusBasketEntry):Boolean;
begin
  Finalize(Entry);
  FillChar(Entry, SizeOf(Entry), 0);
  Result := S7PlusBasketKindOf(Candidates, Idx, Entry.Kind);
  if not Result then exit;
  Entry.Path := Candidates[Idx].Path;
  Entry.Prefix := Prefix;
  Entry.Scan := Scan;
  //a simple tag has no children
  Entry.MapChildren := MapChildren and (Entry.Kind<>bkTag);
end;

function S7PlusBasketNewDBEntry(const DBKey, Prefix:String; Scan:LongInt; MapChildren:Boolean;
  out Entry:TS7PlusBasketEntry):Boolean;
begin
  Finalize(Entry);
  FillChar(Entry, SizeOf(Entry), 0);
  Result := (DBKey<>'') and (not S7PlusIsAreaName(DBKey));
  if not Result then exit;
  Entry.Path := DBKey;
  Entry.IsDB := true;
  Entry.Kind := bkStruct;
  Entry.Prefix := Prefix;
  Entry.Scan := Scan;
  Entry.MapChildren := MapChildren;
end;

//A TPLCStruct of ONE element of an array of structures, read by the path of the element: as big
//as an element, with the members of the array (those of its first element, at the offsets
//inside it) when the children are mapped. The items are placed the way the array has them
//(the PLC's classic bytes).
procedure ItemOfStructElement(const Head:TS7PlusCandidate; const ElemPath:String;
  const E:TS7PlusBasketEntry; out Item:TS7PlusImportItem);
var
  m, q:Integer;
  Name:String;
begin
  Finalize(Item);
  FillChar(Item, SizeOf(Item), 0);
  Item.Path     := ElemPath;
  Item.CompName := S7PlusSuggestedName(ElemPath, E.Prefix);
  Item.Scan     := E.Scan;
  Item.IsStruct := true;
  Item.Size     := Head.ElementSize;
  if not E.MapChildren then exit;

  for m:=0 to High(Head.Members) do begin
    if not Head.Members[m].Supported then continue;
    Name := S7PlusSuggestedName(ElemPath+'.'+Head.Members[m].Name, E.Prefix);
    if Head.Members[m].IsString and (Head.Members[m].StringLength<0) then continue;
    q := Length(Item.StructItems);
    SetLength(Item.StructItems, q+1);
    FillChar(Item.StructItems[q], SizeOf(Item.StructItems[q]), 0);
    Item.StructItems[q].CompName := Name;
    Item.StructItems[q].Offset   := Head.Members[m].Offset;
    if Head.Members[m].IsString then begin
      Item.StructItems[q].IsString   := true;
      Item.StructItems[q].StringSize := Head.Members[m].StringLength;
      Item.StructItems[q].TagType    := pttDefault;
      if Head.Members[m].Offset+Head.Members[m].StringLength+2>Item.Size then
        Item.Size := Head.Members[m].Offset+Head.Members[m].StringLength+2;
    end else begin
      Item.StructItems[q].TagType    := Head.Members[m].TagType;
      Item.StructItems[q].SwapBytes  := Head.Members[m].SwapBytes;
      Item.StructItems[q].SwapWords  := Head.Members[m].SwapWords;
      Item.StructItems[q].SwapDWords := Head.Members[m].SwapDWords;
      if TS7PlusSoftDataType(Head.Members[m].SoftDataType) in [sdtBOOL, sdtBBOOL] then begin
        Item.StructItems[q].IsBool   := true;
        Item.StructItems[q].BitIndex := Head.Members[m].BitOffset;
      end;
    end;
  end;
end;

//The plan item of one entry before the BOOLs get their bits.
function ItemOfEntry(const Candidates:TS7PlusCandidateArray; const E:TS7PlusBasketEntry;
  out Item:TS7PlusImportItem):Boolean;
var
  Idx, j, Hit:Integer;
  Kind:TS7PlusBasketKind;
  Checked:TBooleanDynArray;
  ArrOf, MemOf:TIntegerDynArray;
  Plan:TS7PlusImportItems;
  Mode:TS7PlusImportMode;
begin
  Result := false;
  Finalize(Item);
  FillChar(Item, SizeOf(Item), 0);

  if E.IsDB then begin
    Plan := S7PlusBuildDBStructPlan(Candidates, E.Path, E.Prefix, E.Scan, E.MapChildren);
    if Length(Plan)=0 then exit;
    Item := Plan[0];
    Result := true;
    exit;
  end;

  Idx := FindCandidate(Candidates, E.Path);
  if (Idx<0) or (not S7PlusBasketKindOf(Candidates, Idx, Kind)) then exit;

  if StructArrayHeadOf(Candidates, Idx, Hit) then begin
    ItemOfStructElement(Candidates[Hit], Candidates[Idx].Path, E, Item);
    Result := true;
    exit;
  end;

  SetLength(Checked, Length(Candidates));
  for j:=0 to High(Checked) do Checked[j] := false;

  if S7PlusIsStructArrayHead(Candidates[Idx]) then begin
    if not E.MapChildren then begin
      //the whole array as one structure, with no items
      Item.Path     := Candidates[Idx].Path;
      Item.CompName := S7PlusSuggestedName(Candidates[Idx].Path, E.Prefix);
      Item.Scan     := E.Scan;
      Item.IsStruct := true;
      Item.Size     := Candidates[Idx].ElementCount*Candidates[Idx].ElementSize;
      Result := true;
      exit;
    end;
    //every supported member of the first element: the plan repeats them in every index
    S7PlusMapStructArrayMembers(Candidates, ArrOf, MemOf);
    for j:=0 to High(Candidates) do
      if (ArrOf[j]=Idx) and (MemOf[j]>=0) and Candidates[Idx].Members[MemOf[j]].Supported then
        Checked[j] := true;
    Mode := imStructArray;
  end else begin
    Checked[Idx] := true;
    case Candidates[Idx].IsArray of
      true:  Mode := imArray;
      false: if S7PlusSelectable(Candidates[Idx], imStruct, false) then Mode := imStruct else Mode := imSimple;
    end;
  end;

  //the plan also makes the entries of what is below a ticked structure: keep the one dragged
  Plan := S7PlusBuildImportPlan(Candidates, Checked, Mode, E.Prefix, E.Scan, E.MapChildren);
  Hit := -1;
  for j:=0 to High(Plan) do
    if SameText(Plan[j].Path, Candidates[Idx].Path) then begin
      Hit := j;
      break;
    end;
  if Hit<0 then exit;
  Item := Plan[Hit];
  Result := true;
end;

//Turns the BOOLs of a struct item list into byte items (one per byte) and bits over them.
procedure BitsOfStruct(var Item:TS7PlusImportItem);
var
  Items:TS7PlusImportStructItems;
  i, k, q:Integer;
  SupportName:String;
  Bit:TS7PlusImportBitTag;
  Done:array of LongInt;      //byte offsets that got their support item
begin
  SetLength(Items, 0);
  SetLength(Done, 0);
  for i:=0 to High(Item.StructItems) do begin
    if not Item.StructItems[i].IsBool then begin
      SetLength(Items, Length(Items)+1);
      Items[High(Items)] := Item.StructItems[i];
      continue;
    end;

    SupportName := Item.CompName+'_Byte'+IntToStr(Item.StructItems[i].Offset);
    q := -1;
    for k:=0 to High(Done) do
      if Done[k]=Item.StructItems[i].Offset then q := k;
    if q<0 then begin
      //the first BOOL of the byte brings the support item
      SetLength(Done, Length(Done)+1);
      Done[High(Done)] := Item.StructItems[i].Offset;
      SetLength(Items, Length(Items)+1);
      FillChar(Items[High(Items)], SizeOf(Items[High(Items)]), 0);
      Items[High(Items)].CompName := SupportName;
      Items[High(Items)].Offset   := Item.StructItems[i].Offset;
      Items[High(Items)].TagType  := pttByte;
    end;

    Bit.CompName    := Item.StructItems[i].CompName;
    Bit.SupportName := SupportName;
    Bit.BitIndex    := Item.StructItems[i].BitIndex;
    SetLength(Item.Bits, Length(Item.Bits)+1);
    Item.Bits[High(Item.Bits)] := Bit;
  end;
  Item.StructItems := Items;
end;

//The elements of an array of BOOLs become byte elements, and the old names go to the bits.
procedure BitsOfBlock(var Item:TS7PlusImportItem);
var
  k:Integer;
begin
  for k:=0 to High(Item.ElementNames) do begin
    SetLength(Item.Bits, Length(Item.Bits)+1);
    Item.Bits[High(Item.Bits)].CompName    := Item.ElementNames[k];
    Item.Bits[High(Item.Bits)].SupportName := Item.ElementNames[k]+'_Byte';
    Item.Bits[High(Item.Bits)].BitIndex    := 0;
    Item.ElementNames[k] := Item.ElementNames[k]+'_Byte';
  end;
end;

function S7PlusBasketBuild(const Candidates:TS7PlusCandidateArray; const Entries:TS7PlusBasketEntries):TS7PlusImportItems;
var
  i:Integer;
  Item:TS7PlusImportItem;
begin
  SetLength(Result, 0);
  for i:=0 to High(Entries) do begin
    if not ItemOfEntry(Candidates, Entries[i], Item) then continue;
    if Entries[i].Name<>'' then Item.CompName := Entries[i].Name;
    Item.Scan := Entries[i].Scan;
    if Entries[i].MapChildren then begin
      if Item.IsStruct then BitsOfStruct(Item);
      if Item.IsBlock and Item.ElementIsBool then BitsOfBlock(Item);
    end;
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := Item;
  end;
end;

function S7PlusBasketNameProblems(const Items:TS7PlusImportItems; const Existing:array of String):TS7PlusNameProblems;
var
  Seen, Taken, Reported:TStringList;
  i, k:Integer;

  procedure Problem(const AName:String; AKind:TS7PlusNameProblemKind);
  var
    Key:String;
  begin
    Key := IntToStr(Ord(AKind))+':'+UpperCase(AName);
    if Reported.IndexOf(Key)>=0 then exit;
    Reported.Add(Key);
    SetLength(Result, Length(Result)+1);
    Result[High(Result)].Name := AName;
    Result[High(Result)].Kind := AKind;
  end;

  procedure Check(const AName:String);
  var
    p:Integer;
  begin
    if not IsValidIdent(AName) then Problem(AName, npInvalid);
    if Taken.Find(AName, p) then Problem(AName, npExists);
    if Seen.Find(AName, p) then
      Problem(AName, npDuplicate)
    else
      Seen.Add(AName);
  end;

begin
  SetLength(Result, 0);
  Seen := TStringList.Create;
  Taken := TStringList.Create;
  Reported := TStringList.Create;
  try
    Seen.CaseSensitive := false;
    Seen.Sorted := true;
    Taken.CaseSensitive := false;
    Taken.Sorted := true;
    Taken.Duplicates := dupIgnore;
    Reported.CaseSensitive := false;
    for k:=0 to High(Existing) do
      Taken.Add(Existing[k]);

    for i:=0 to High(Items) do begin
      Check(Items[i].CompName);
      for k:=0 to High(Items[i].StructItems) do
        Check(Items[i].StructItems[k].CompName);
      for k:=0 to High(Items[i].ElementNames) do
        Check(Items[i].ElementNames[k]);
      for k:=0 to High(Items[i].Bits) do
        Check(Items[i].Bits[k].CompName);
    end;
  finally
    Reported.Free;
    Taken.Free;
    Seen.Free;
  end;
end;

end.
