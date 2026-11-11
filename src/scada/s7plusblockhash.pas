{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Os dois SHA-1 de 20 bytes que o TIA Portal grava em cada bloco no download: IfrHash (7949) e AlarmIFRHash (7951).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Só confere e calcula: não escreve nada no CLP. As regras vêm de uma análise estática (descrita em
  docs/ ou na investigação de download de blocos) e @bold(não foram comparadas com um CLP), então o uso
  é diagnóstico: ler os atributos de um DB (TS7CommPlusDriver.ReadObjectAttributes), calcular e comparar
  com o 7949/7951 que o próprio CLP devolve.

  7949 = SHA-1 de um fluxo (BinaryWriter do .NET, UTF-8) com o nome do bloco e os atributos de engenharia
  em ordem fixa: texto = bytes UTF-8 sem tamanho; bytes = crus; UInt16 = 2 bytes little-endian; dicionário =
  valores por ordem crescente de chave. Atributo ausente entra vazio (0). O código de máquina não entra.

  7951 = SHA-1 do texto UTF-8: o XML da lista de classes de alarme + CRLF, cada alarme + CRLF e cada lista de
  textos + CRLF. Um DB sem alarmes dá sempre 6610F3B5B089454CA1C582423CA2E79A4930A770.
}
{$ELSE}
{:
  @abstract(The two 20 byte SHA-1 that TIA Portal writes on every block when downloading: IfrHash (7949) and AlarmIFRHash (7951).)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  It only checks and computes: it writes nothing to the PLC. The rules come from a static analysis (see the
  block download investigation) and @bold(were not compared with a PLC yet), so use it as a diagnostic: read the
  attributes of a DB (TS7CommPlusDriver.ReadObjectAttributes), compute and compare with the 7949/7951 the PLC
  itself returns.

  7949 = SHA-1 of a stream (.NET BinaryWriter, UTF-8) with the name of the block and its engineering
  attributes in a fixed order: text = UTF-8 bytes with no length; bytes = raw; UInt16 = 2 bytes little-endian;
  dictionary = the values in ascending key order. A missing attribute goes in empty (0). The machine code is not
  part of it.

  7951 = SHA-1 of the UTF-8 text: the XML of the alarm class list + CRLF, each alarm + CRLF and each text list +
  CRLF. A DB with no alarms always gives 6610F3B5B089454CA1C582423CA2E79A4930A770.
}
{$ENDIF}
unit S7PlusBlockHash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, sha1, S7PlusAlarms;

const
  //attribute ids (DataInterface and the block classes of the PLC's object model)
  S7PlusAttr_Comment            = 4288;
  S7PlusAttr_Designators        = 2450;
  S7PlusAttr_IdentES            = 2449;
  S7PlusAttr_CompilerSwitches   = 2545;
  S7PlusAttr_InterfaceDesc      = 2544;
  S7PlusAttr_LineComments       = 2546;
  S7PlusAttr_Title              = 4287;
  S7PlusAttr_XrefInfo           = 7564;
  S7PlusAttr_BodyDescription    = 2533;
  S7PlusAttr_BlockLanguage      = 2523;
  S7PlusAttr_BlockNumber        = 2521;
  S7PlusAttr_KnowhowProtection  = 2524;   //a struct: 3448 mode (Bitset16), 3449 password (bytes)
  S7PlusAttr_KnowhowMode        = 3448;
  S7PlusAttr_KnowhowPassword    = 3449;
  S7PlusAttr_OptimizeInfo       = 2537;
  S7PlusAttr_TypeInfo           = 4578;
  S7PlusAttr_IfrHash            = 7949;
  S7PlusAttr_AlarmIfrHash       = 7951;
  S7PlusAttr_AlarmDescription   = 7853;
  S7PlusAttr_AlarmTexts         = 7845;
  S7PlusAttr_StructureModified  = 7733;
  S7PlusAttr_Name               = 233;

type
  TS7PlusDictItem = record
    Key:Cardinal;
    Value:TBytes;
  end;
  TS7PlusDict = array of TS7PlusDictItem;

  {$IFDEF PORTUGUES}
  //: O que entra no 7949 de um DB (ou de um UDT, com IsBlock=False: só o que a classe DataInterface tem).
  {$ELSE}
  //: What goes into the 7949 of a DB (or of a UDT, with IsBlock=False: only what the DataInterface class has).
  {$ENDIF}
  TS7PlusBlockHashInput = record
    IsBlock:Boolean;
    Name:UTF8String;
    Comment, Title:TS7PlusDict;
    Designators, IdentES, CompilerSwitches, InterfaceDescription:TBytes;
    LineComments, XrefInfo, BodyDescription:TS7PlusDict;
    BlockLanguage, BlockNumber, KnowhowMode:Word;
    KnowhowPassword, OptimizeInfo, TypeInfo:TBytes;
  end;

{$IFDEF PORTUGUES}
//: Os atributos a pedir ao CLP (ReadObjectAttributes) para montar o TS7PlusBlockHashInput, na ordem
//: que S7PlusBlockHashInputFromAttrs espera; seguidos de 7949, 7951, 7853, 7845 e 7733 (índices S7PlusHashIdx*).
{$ELSE}
//: The attributes to ask the PLC for (ReadObjectAttributes) to build the TS7PlusBlockHashInput, in the order
//: S7PlusBlockHashInputFromAttrs expects; followed by 7949, 7951, 7853, 7845 and 7733 (S7PlusHashIdx* indexes).
{$ENDIF}
function S7PlusBlockHashAttrIds:TCardinalDynArray;

const
  S7PlusHashIdx_IfrHash           = 15;
  S7PlusHashIdx_AlarmIfrHash      = 16;
  S7PlusHashIdx_AlarmDescription  = 17;
  S7PlusHashIdx_AlarmTexts        = 18;
  S7PlusHashIdx_StructureModified = 19;

{$IFDEF PORTUGUES}
//: Monta a entrada do 7949 a partir dos valores lidos (na ordem de S7PlusBlockHashAttrIds).
{$ELSE}
//: Builds the 7949 input from the values read (in the order of S7PlusBlockHashAttrIds).
{$ENDIF}
function S7PlusBlockHashInputFromAttrs(const AName:UTF8String; const Values:TS7PlusAttrValues; IsBlock:Boolean):TS7PlusBlockHashInput;

//: The stream the 7949 is the SHA-1 of (to compare byte by byte when it does not match).
function S7PlusBlockIfrStream(const Input:TS7PlusBlockHashInput):TBytes;
//: The 7949 (20 bytes).
function S7PlusBlockIfrHash(const Input:TS7PlusBlockHashInput):TBytes;

//: The text the 7951 is the SHA-1 of, from the class list, the alarms (by id) and the text lists (by LCID).
function S7PlusAlarmIfrText(const AlarmDescription, AlarmTexts:TS7PlusDict):UTF8String;
//: The 7951 (20 bytes) from the attributes 7853 (AlarmDescription: key 0 = class list, key = alarm id)
//: and 7845 (AlarmTexts: key = LCID); both empty = a DB with no alarms.
function S7PlusAlarmIfrHash(const AlarmDescription, AlarmTexts:TS7PlusDict):TBytes;

//: A dictionary (sparse array) read from the PLC as it is used here: text and blob items by key.
function S7PlusDictOf(const Value:TS7PlusAttrValue):TS7PlusDict;

//: SHA-1 of bytes / of UTF-8 text.
function S7PlusSha1(const Data:TBytes):TBytes;
function S7PlusHex(const Data:TBytes):String;
function S7PlusSameDigest(const A, B:TBytes):Boolean;

implementation

const
  EmptyClassList = '<?xml version="1.0" encoding="utf-8"?><AlarmClassList />';

function S7PlusSha1(const Data:TBytes):TBytes;
var
  D:TSHA1Digest;
begin
  if Length(Data)=0 then
    D := SHA1String('')
  else
    D := SHA1Buffer(Data[0], Length(Data));
  SetLength(Result, 20);
  Move(D[0], Result[0], 20);
end;

function S7PlusHex(const Data:TBytes):String;
var
  i:Integer;
begin
  Result := '';
  for i:=0 to High(Data) do
    Result := Result+IntToHex(Data[i], 2);
end;

function S7PlusSameDigest(const A, B:TBytes):Boolean;
begin
  Result := (Length(A)=Length(B)) and ((Length(A)=0) or CompareMem(@A[0], @B[0], Length(A)));
end;

function S7PlusBlockHashAttrIds:TCardinalDynArray;
begin
  SetLength(Result, 20);
  Result[0]  := S7PlusAttr_Name;
  Result[1]  := S7PlusAttr_Comment;
  Result[2]  := S7PlusAttr_Designators;
  Result[3]  := S7PlusAttr_IdentES;
  Result[4]  := S7PlusAttr_CompilerSwitches;
  Result[5]  := S7PlusAttr_InterfaceDesc;
  Result[6]  := S7PlusAttr_LineComments;
  Result[7]  := S7PlusAttr_Title;
  Result[8]  := S7PlusAttr_XrefInfo;
  Result[9]  := S7PlusAttr_BodyDescription;
  Result[10] := S7PlusAttr_BlockLanguage;
  Result[11] := S7PlusAttr_BlockNumber;
  Result[12] := S7PlusAttr_KnowhowProtection;
  Result[13] := S7PlusAttr_OptimizeInfo;
  Result[14] := S7PlusAttr_TypeInfo;
  Result[S7PlusHashIdx_IfrHash]           := S7PlusAttr_IfrHash;
  Result[S7PlusHashIdx_AlarmIfrHash]      := S7PlusAttr_AlarmIfrHash;
  Result[S7PlusHashIdx_AlarmDescription]  := S7PlusAttr_AlarmDescription;
  Result[S7PlusHashIdx_AlarmTexts]        := S7PlusAttr_AlarmTexts;
  Result[S7PlusHashIdx_StructureModified] := S7PlusAttr_StructureModified;
end;

function S7PlusDictOf(const Value:TS7PlusAttrValue):TS7PlusDict;
var
  i:Integer;
begin
  SetLength(Result, 0);
  if not Value.Found then exit;
  if Value.Kind in [akList, akStruct] then begin
    SetLength(Result, Length(Value.Items));
    for i:=0 to High(Value.Items) do begin
      Result[i].Key := Value.Items[i].Key;
      Result[i].Value := Value.Items[i].Bytes;
    end;
  end;
end;

function BytesOfAttr(const Value:TS7PlusAttrValue):TBytes;
begin
  SetLength(Result, 0);
  if Value.Found and (Value.Kind in [akBlob, akText, akList]) then
    Result := Value.Bytes;
end;

function WordOfAttr(const Value:TS7PlusAttrValue):Word;
begin
  Result := 0;
  if Value.Found and (Value.Kind=akNumber) then Result := Word(Value.Num);
end;

function S7PlusBlockHashInputFromAttrs(const AName:UTF8String; const Values:TS7PlusAttrValues; IsBlock:Boolean):TS7PlusBlockHashInput;
var
  i, k:Integer;
begin
  Finalize(Result);
  FillChar(Result, SizeOf(Result), 0);
  Result.IsBlock := IsBlock;
  Result.Name := AName;
  if Length(Values)<15 then exit;
  Result.Comment := S7PlusDictOf(Values[1]);
  Result.Designators := BytesOfAttr(Values[2]);
  Result.IdentES := BytesOfAttr(Values[3]);
  Result.CompilerSwitches := BytesOfAttr(Values[4]);
  Result.InterfaceDescription := BytesOfAttr(Values[5]);
  Result.LineComments := S7PlusDictOf(Values[6]);
  Result.Title := S7PlusDictOf(Values[7]);
  Result.XrefInfo := S7PlusDictOf(Values[8]);
  Result.BodyDescription := S7PlusDictOf(Values[9]);
  Result.BlockLanguage := WordOfAttr(Values[10]);
  Result.BlockNumber := WordOfAttr(Values[11]);
  //2524 is a struct: 3448 the protection mode, 3449 the password (a fixed array of bytes)
  if Values[12].Found then
    for i:=0 to High(Values[12].Items) do begin
      k := Values[12].Items[i].Key;
      if (k=S7PlusAttr_KnowhowMode) and (Values[12].Items[i].Kind=akNumber) then
        Result.KnowhowMode := Word(Values[12].Items[i].Num)
      else if k=S7PlusAttr_KnowhowPassword then
        Result.KnowhowPassword := Values[12].Items[i].Bytes;
    end;
  Result.OptimizeInfo := BytesOfAttr(Values[13]);
  Result.TypeInfo := BytesOfAttr(Values[14]);
end;

//-- the stream of the 7949 -------------------------------------------------------

procedure PutBytes(var Stream:TBytes; const B:TBytes);
var
  n:Integer;
begin
  if Length(B)=0 then exit;
  n := Length(Stream);
  SetLength(Stream, n+Length(B));
  Move(B[0], Stream[n], Length(B));
end;

procedure PutText(var Stream:TBytes; const T:UTF8String);
var
  n:Integer;
begin
  if T='' then exit;
  n := Length(Stream);
  SetLength(Stream, n+Length(T));
  Move(T[1], Stream[n], Length(T));
end;

procedure PutWord(var Stream:TBytes; W:Word);
var
  n:Integer;
begin
  n := Length(Stream);
  SetLength(Stream, n+2);
  Stream[n] := W and $FF;
  Stream[n+1] := W shr 8;
end;

//the values of a dictionary in ascending key order; OnlyPublic keeps the keys with bit 31 clear
procedure PutDict(var Stream:TBytes; const D:TS7PlusDict; OnlyPublic:Boolean);
var
  Order:array of Integer;
  i, j, t:Integer;
begin
  SetLength(Order, Length(D));
  for i:=0 to High(D) do Order[i] := i;
  //insertion sort by key (the dictionaries are short)
  for i:=1 to High(Order) do begin
    t := Order[i];
    j := i-1;
    while (j>=0) and (D[Order[j]].Key>D[t].Key) do begin
      Order[j+1] := Order[j];
      Dec(j);
    end;
    Order[j+1] := t;
  end;
  for i:=0 to High(Order) do begin
    if OnlyPublic and ((D[Order[i]].Key and $80000000)<>0) then continue;
    PutBytes(Stream, D[Order[i]].Value);
  end;
end;

function S7PlusBlockIfrStream(const Input:TS7PlusBlockHashInput):TBytes;
begin
  SetLength(Result, 0);
  PutText(Result, Input.Name);
  PutDict(Result, Input.Comment, false);
  PutBytes(Result, Input.Designators);
  PutBytes(Result, Input.IdentES);
  PutBytes(Result, Input.CompilerSwitches);
  PutBytes(Result, Input.InterfaceDescription);
  PutDict(Result, Input.LineComments, false);
  PutDict(Result, Input.Title, false);
  PutDict(Result, Input.XrefInfo, false);
  if Input.IsBlock then begin
    PutDict(Result, Input.BodyDescription, true);
    PutWord(Result, Input.BlockLanguage);
    PutWord(Result, Input.BlockNumber);
    PutWord(Result, Input.KnowhowMode);
    PutBytes(Result, Input.KnowhowPassword);
    PutBytes(Result, Input.OptimizeInfo);
    PutBytes(Result, Input.TypeInfo);
  end;
end;

function S7PlusBlockIfrHash(const Input:TS7PlusBlockHashInput):TBytes;
begin
  Result := S7PlusSha1(S7PlusBlockIfrStream(Input));
end;

//-- the alarm hash -----------------------------------------------------------------

function AsText(const B:TBytes):UTF8String;
begin
  SetLength(Result, Length(B));
  if Length(B)>0 then Move(B[0], Result[1], Length(B));
end;

function SortedKeys(const D:TS7PlusDict):TCardinalDynArray;
var
  i, j:Integer;
  t:Cardinal;
begin
  SetLength(Result, Length(D));
  for i:=0 to High(D) do Result[i] := D[i].Key;
  for i:=1 to High(Result) do begin
    t := Result[i];
    j := i-1;
    while (j>=0) and (Result[j]>t) do begin
      Result[j+1] := Result[j];
      Dec(j);
    end;
    Result[j+1] := t;
  end;
end;

function ValueOfKey(const D:TS7PlusDict; AKey:Cardinal; out Value:TBytes):Boolean;
var
  i:Integer;
begin
  for i:=0 to High(D) do
    if D[i].Key=AKey then begin
      Value := D[i].Value;
      Result := true;
      exit;
    end;
  SetLength(Value, 0);
  Result := false;
end;

function S7PlusAlarmIfrText(const AlarmDescription, AlarmTexts:TS7PlusDict):UTF8String;
const
  CRLF = #13#10;
var
  Keys:TCardinalDynArray;
  V:TBytes;
  i:Integer;
begin
  //the class list (key 0), or the empty one when the block has none
  if ValueOfKey(AlarmDescription, 0, V) and (Length(V)>0) then
    Result := AsText(V)+CRLF
  else
    Result := EmptyClassList+CRLF;
  //then the alarms by id
  Keys := SortedKeys(AlarmDescription);
  for i:=0 to High(Keys) do
    if Keys[i]<>0 then begin
      ValueOfKey(AlarmDescription, Keys[i], V);
      Result := Result+AsText(V)+CRLF;
    end;
  //and the text lists by LCID
  Keys := SortedKeys(AlarmTexts);
  for i:=0 to High(Keys) do begin
    ValueOfKey(AlarmTexts, Keys[i], V);
    Result := Result+AsText(V)+CRLF;
  end;
end;

function S7PlusAlarmIfrHash(const AlarmDescription, AlarmTexts:TS7PlusDict):TBytes;
var
  T:UTF8String;
  B:TBytes;
begin
  T := S7PlusAlarmIfrText(AlarmDescription, AlarmTexts);
  SetLength(B, Length(T));
  if Length(T)>0 then Move(T[1], B[0], Length(T));
  Result := S7PlusSha1(B);
end;

end.
