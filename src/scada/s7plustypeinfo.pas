{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Decodificação da árvore de tipos (TypeInfo) do S7CommPlus e resolução de LID simbólico.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  O S7CommPlus não permite ler/escrever variáveis de DB (nem áreas nativas M/I/Q/Timers/
  Counters) por offset de byte bruto - todo acesso é feito por LID (Local ID) simbólico,
  atribuído pelo compilador a cada variável, obtido através de EXPLORE na DB/área e na
  árvore de tipos compilada (TypeInfo). Esta unit decodifica essa árvore e a "achata" numa
  lista de variáveis endereçáveis (nome + caminho de LIDs), confirmado contra CLP real.

  Portado de python-snap7 s7commplus/typeinfo.py.
}
{$ELSE}
{:
  @abstract(S7CommPlus type-info tree decoding and symbolic LID resolution.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  S7CommPlus does not allow reading/writing DB variables (nor native M/I/Q/Timers/Counters
  areas) by raw byte offset - all access goes through a compiler-assigned symbolic LID
  (Local ID) per variable, discovered by EXPLORE'ing the DB/area and the compiled type
  tree (TypeInfo). This unit decodes that tree and flattens it into a list of addressable
  variables (name + LID path), confirmed against real hardware.

  Ported from python-snap7 s7commplus/typeinfo.py.
}
{$ENDIF}
unit S7PlusTypeInfo;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, S7PlusTypes, S7PlusVLQ, S7PlusCodec;

type
  TS7PlusLIDArray = array of Cardinal;
  TS7PlusDim6Cardinal = array[0..5] of Cardinal;
  TS7PlusDim6Int = array[0..5] of LongInt;

  //-- PLC "software datatype" ids (distinct from the wire PValue DataType in S7PlusTypes).
  TS7PlusSoftDataType = (
    sdtVOID=0, sdtBOOL=1, sdtBYTE=2, sdtCHAR=3, sdtWORD=4, sdtINT=5, sdtDWORD=6, sdtDINT=7,
    sdtREAL=8, sdtDATE=9, sdtTIMEOFDAY=10, sdtTIME=11, sdtS5TIME=12, sdtDATEANDTIME=14,
    sdtINTERNETTIME=15, sdtARRAY=16, sdtSTRUCT=17, sdtENDSTRUCT=18, sdtSTRING=19,
    sdtPOINTER=20, sdtMULTIFB=21, sdtANY=22, sdtBLOCKFB=23, sdtBLOCKFC=24, sdtBLOCKDB=25,
    sdtBLOCKSDB=26, sdtCOUNTER=28, sdtTIMER=29, sdtBBOOL=40, sdtLREAL=48, sdtULINT=49,
    sdtLINT=50, sdtLWORD=51, sdtUSINT=52, sdtUINT=53, sdtUDINT=54, sdtSINT=55, sdtWCHAR=61,
    sdtWSTRING=62, sdtVARIANT=63, sdtLTIME=64, sdtLTOD=65, sdtLDT=66, sdtDTL=67,
    sdtREMOTE=96, sdtAOMIDENT=128, sdtUNKNOWN=255
  );

const
  S7PlusExplore_ClassOMSTypeInfoContainer = 534;
  S7PlusExplore_TI_TComSize               = 1502;

  //-- Native process-area synthetic type-info RIDs (fixed, no lookup needed).
  S7PlusTiRid_IArea     = $90010000;
  S7PlusTiRid_QArea     = $90020000;
  S7PlusTiRid_MArea     = $90030000;
  S7PlusTiRid_S7Timers  = $90050000;
  S7PlusTiRid_S7Counters= $90060000;

//: True if SoftDataType is a leaf that should be exposed as a readable tag
//: (containers/markers like ARRAY/STRUCT/ENDSTRUCT/MULTIFB/VARIANT/DTL are not leaves).
function IsSoftDataTypeSupported(Code:Byte):Boolean;
//: Element byte stride for a software datatype (StringLen only matters for STRING/WSTRING).
function S7PlusDataTypeSize(Code:Byte; StringLen:Integer=0):Integer;

type
  TS7PlusOffsetInfo = record
    Code:Byte;
    OptAddr, NonOptAddr:Cardinal;
    Unspecified1, Unspecified2:Word;
    ArrayLowerBound:LongInt;
    ArrayElementCount:Cardinal;
    MDimLowerBounds:TS7PlusDim6Int;
    MDimElementCount:TS7PlusDim6Cardinal;
    NonOptStructSize, OptStructSize:Cardinal;
    RelationId:Cardinal;
    Is1Dim, IsMDim, HasRelation:Boolean;
  end;

//: Parses one POffsetInfoType (selected by OffsetInfoType, 0..15). All fields inside are
//: little-endian (unlike the rest of the wire protocol, which is big-endian/VLQ).
function ParseOffsetInfo(const Data:TBytes; Offset:Integer; OffsetInfoType:Byte; out Info:TS7PlusOffsetInfo):Integer;

type
  TS7PlusVartypeElement = record
    Lid:Cardinal;
    SymbolCrc:Cardinal;
    SoftDataType:Byte;
    AttributeFlags:Word;
    BitOffsetInfoFlags:Byte;
    OffsetInfo:TS7PlusOffsetInfo;
  end;
  TS7PlusVartypeElementArray = array of TS7PlusVartypeElement;

//: The 4-bit POffsetInfoType selector held in AttributeFlags bits 12..15.
function S7PlusVteOffsetInfoType(const El:TS7PlusVartypeElement):Byte;
//: Optimized bit offset carried in the low 3 bits of AttributeFlags.
function S7PlusVteAttributeBitOffset(const El:TS7PlusVartypeElement):Byte;
function S7PlusVteNonOptBitOffset(const El:TS7PlusVartypeElement):Byte;
function S7PlusVteOptBitOffset(const El:TS7PlusVartypeElement):Byte;
function S7PlusVteClassic(const El:TS7PlusVartypeElement):Boolean;

function ParseVartypeElement(const Data:TBytes; Offset:Integer; out El:TS7PlusVartypeElement):Integer;
//: Parses a PVartypeList: one or more length-prefixed blocks, terminated by a zero-length block.
function ParseVartypeList(const Data:TBytes; Offset:Integer; out Elements:TS7PlusVartypeElementArray):Integer;
//: Parses a VarnameList: length-prefixed blocks of Pascal-style-length name entries.
function ParseVarnameList(const Data:TBytes; Offset:Integer; out Names:TStringArray):Integer;

type
  PS7PlusObject = ^TS7PlusObject;
  TS7PlusObject = record
    RelationId:Cardinal;
    ClassId:Cardinal;
    AttrIds:array of Cardinal;
    AttrValues:array of TBytes;
    VartypeList:TS7PlusVartypeElementArray;
    VarnameList:TStringArray;
    Objects:array of TS7PlusObject;
  end;
  TS7PlusObjectArray = array of TS7PlusObject;

function S7PlusObjectAttr(const Obj:TS7PlusObject; AttrId:Cardinal; out Value:TBytes):Boolean;

//: Parses a single PObject starting at a $A1 (StartOfObject) tag, up to its $A2 terminator.
function ParseS7PlusObject(const Data:TBytes; Offset:Integer; out Obj:TS7PlusObject):Integer;
//: Parses a sequence of sibling PObjects (consecutive $A1 blocks).
function ParseS7PlusObjectList(const Data:TBytes; Offset:Integer; out Objects:TS7PlusObjectArray):Integer;
//: Finds an object (recursively) by ClassId.
function S7PlusFindContainer(const Objects:TS7PlusObjectArray; ClassId:Cardinal; out Found:TS7PlusObject):Boolean;
//: Skips the leading ReturnValue and any preamble, returns the type-info container's children.
function ExtractS7PlusTypeInfoObjects(const Response:TBytes; out Objects:TS7PlusObjectArray):Boolean;

type
  TS7PlusNodeType = (ntUndefined, ntRoot, ntVar, ntArray, ntStructArray);

  PS7PlusNode = ^TS7PlusNode;
  TS7PlusNode = record
    NodeType:TS7PlusNodeType;
    Name:String;
    AccessId:Cardinal;
    SoftDataType:Byte;
    RelationId:Cardinal;
    HasVte:Boolean;
    Vte:TS7PlusVartypeElement;
    Children:array of TS7PlusNode;
  end;
  TS7PlusNodeArray = array of TS7PlusNode;

  TS7PlusVarInfo = record
    Name:String;
    Lids:TS7PlusLIDArray;
    SoftDataType:Byte;
    //: Cumulative byte offset from the DB/area root (sum of every ancestor's own
    //: OffsetInfo.OptAddr along the struct-nesting path), used to slice a member's bytes
    //: out of an ancestor's already-read blob instead of re-reading it independently -
    //: see TS7CommPlusDriver.TryDeduplicateEntry. -1 means "not reliably known": this
    //: happens for anything inside an array (S7PlusAddMDimSubnodes/array element offsets
    //: are explicitly not computed for LID addressing today), so those never participate
    //: in the dedup optimization and are always read independently.
    ByteOffset:Integer;
  end;
  TS7PlusVarInfoArray = array of TS7PlusVarInfo;

  //: One DB found by S7PlusParseExploreDataBlocks: its symbolic (TIA Portal) name, its
  //: numeric DB number, and the raw RelationId ($8A0E0000+Number) it came from.
  TS7PlusDataBlockInfo = record
    Name:String;
    Number:Cardinal;
    Rid:Cardinal;
  end;
  TS7PlusDataBlockInfoArray = array of TS7PlusDataBlockInfo;

//: Expands each ROOT node (name+AccessId+RelationId already filled) against the matching
//: type object from TypeObjects (matched by RelationId), in place.
procedure S7PlusBuildTree(var RootNodes:TS7PlusNodeArray; const TypeObjects:TS7PlusObjectArray);
//: Walks the expanded tree and produces the flat list of readable (name, LID path) tags.
function S7PlusBuildFlatList(const RootNodes:TS7PlusNodeArray):TS7PlusVarInfoArray;

//: Parses the raw response of a structured EXPLORE(NativeThePLCProgramRID,
//: [ObjectVariableTypeName, BlockBlockNumber]) call: walks the flat StartOfObject/
//: Attribute/TerminatingObject tag stream of the whole PLC program tree (not the nested
//: PObject form the OMS TypeInfo container uses) with an explicit stack, picking out every
//: object whose ClassId is a DataBlock (S7PlusIds_DBClassRID) and whose RelationId falls in
//: the DB access-area range ($8A0Exxxx) - giving the symbolic (TIA Portal) name -> DB number
//: mapping that Path resolution needs to accept a DB's programming name instead of "DB<n>".
//: Ported from python-snap7 client._parse_explore_datablocks.
function S7PlusParseExploreDataBlocks(const Response:TBytes):TS7PlusDataBlockInfoArray;

//: Formats a LID path as dot-separated uppercase hex, matching python-snap7's access_sequence.
function S7PlusFormatLids(const Lids:TS7PlusLIDArray):String;

implementation

function IsSoftDataTypeSupported(Code:Byte):Boolean;
begin
  case TS7PlusSoftDataType(Code) of
    sdtBOOL, sdtBYTE, sdtCHAR, sdtWORD, sdtINT, sdtDWORD, sdtDINT, sdtREAL, sdtDATE,
    sdtTIMEOFDAY, sdtTIME, sdtS5TIME, sdtDATEANDTIME, sdtSTRING, sdtPOINTER, sdtANY,
    sdtBBOOL, sdtLREAL, sdtULINT, sdtLINT, sdtLWORD, sdtUSINT, sdtUINT, sdtUDINT, sdtSINT,
    sdtWCHAR, sdtWSTRING, sdtLTIME, sdtLTOD, sdtLDT:
      Result := true;
  else
    Result := false;
  end;
end;

function S7PlusDataTypeSize(Code:Byte; StringLen:Integer):Integer;
begin
  case TS7PlusSoftDataType(Code) of
    sdtBOOL, sdtBYTE, sdtCHAR, sdtUSINT, sdtSINT, sdtBBOOL: Result := 1;
    sdtWORD, sdtINT, sdtUINT, sdtDATE, sdtS5TIME, sdtWCHAR: Result := 2;
    sdtDWORD, sdtDINT, sdtREAL, sdtTIMEOFDAY, sdtTIME, sdtUDINT: Result := 4;
    sdtLREAL, sdtULINT, sdtLINT, sdtLWORD, sdtDATEANDTIME, sdtLTIME, sdtLTOD, sdtLDT: Result := 8;
    sdtDTL: Result := 12;
    sdtPOINTER: Result := 6;
    sdtANY, sdtREMOTE: Result := 10;
    sdtSTRING, sdtWSTRING: Result := StringLen+2;
  else
    Result := 0;
  end;
end;

//-- little-endian primitives (this section of the protocol is LE, unlike the rest) --------

function DecU16LE(const Data:TBytes; Offset:Integer):Word; inline;
begin
  Result := Data[Offset] or (Word(Data[Offset+1]) shl 8);
end;

function DecU32LE(const Data:TBytes; Offset:Integer):Cardinal; inline;
begin
  Result := Cardinal(Data[Offset]) or (Cardinal(Data[Offset+1]) shl 8) or
            (Cardinal(Data[Offset+2]) shl 16) or (Cardinal(Data[Offset+3]) shl 24);
end;

function DecI32LE(const Data:TBytes; Offset:Integer):LongInt; inline;
begin
  Result := LongInt(DecU32LE(Data, Offset));
end;

function ParseOffsetInfo(const Data:TBytes; Offset:Integer; OffsetInfoType:Byte; out Info:TS7PlusOffsetInfo):Integer;
var
  a,b:Word;
  i:Integer;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.Code := OffsetInfoType;

  case OffsetInfoType of
    1,8: begin //Std
      a := DecU16LE(Data, Offset);
      b := DecU16LE(Data, Offset+2);
      if OffsetInfoType=8 then begin Info.OptAddr:=a; Info.NonOptAddr:=b; end
      else begin Info.NonOptAddr:=a; Info.OptAddr:=b; end; //legacy: order swapped
      Result := Offset+4;
    end;

    2,9: begin //String
      Info.Unspecified1 := DecU16LE(Data, Offset);
      Info.Unspecified2 := DecU16LE(Data, Offset+2);
      Info.OptAddr := DecU32LE(Data, Offset+4);
      Info.NonOptAddr := DecU32LE(Data, Offset+8);
      Result := Offset+12;
    end;

    3,10: begin //Array1Dim
      Info.Unspecified1 := DecU16LE(Data, Offset);
      Info.Unspecified2 := DecU16LE(Data, Offset+2);
      Info.OptAddr := DecU32LE(Data, Offset+4);
      Info.NonOptAddr := DecU32LE(Data, Offset+8);
      Info.ArrayLowerBound := DecI32LE(Data, Offset+12);
      Info.ArrayElementCount := DecU32LE(Data, Offset+16);
      Info.Is1Dim := true;
      Result := Offset+20;
    end;

    4,11: begin //ArrayMDim
      Info.Unspecified1 := DecU16LE(Data, Offset);
      Info.Unspecified2 := DecU16LE(Data, Offset+2);
      Info.OptAddr := DecU32LE(Data, Offset+4);
      Info.NonOptAddr := DecU32LE(Data, Offset+8);
      Info.ArrayLowerBound := DecI32LE(Data, Offset+12);
      Info.ArrayElementCount := DecU32LE(Data, Offset+16);
      Offset := Offset+20;
      for i:=0 to 5 do Info.MDimLowerBounds[i] := DecI32LE(Data, Offset+i*4);
      Offset := Offset+24;
      for i:=0 to 5 do Info.MDimElementCount[i] := DecU32LE(Data, Offset+i*4);
      Offset := Offset+24;
      Info.IsMDim := true;
      Result := Offset;
    end;

    5,12: begin //Struct
      Info.Unspecified1 := DecU16LE(Data, Offset);
      Info.Unspecified2 := DecU16LE(Data, Offset+2);
      Info.OptAddr := DecU32LE(Data, Offset+4);
      Info.NonOptAddr := DecU32LE(Data, Offset+8);
      Info.RelationId := DecU32LE(Data, Offset+12);
      Info.HasRelation := true;
      Result := Offset + 2+2+4+4+4+(4*4); //HHIII4I
    end;

    6,13: begin //Struct1Dim
      Info.Unspecified1 := DecU16LE(Data, Offset);
      Info.Unspecified2 := DecU16LE(Data, Offset+2);
      Info.OptAddr := DecU32LE(Data, Offset+4);
      Info.NonOptAddr := DecU32LE(Data, Offset+8);
      Info.ArrayLowerBound := DecI32LE(Data, Offset+12);
      Info.ArrayElementCount := DecU32LE(Data, Offset+16);
      Info.NonOptStructSize := DecU32LE(Data, Offset+20);
      Info.OptStructSize := DecU32LE(Data, Offset+24);
      Info.RelationId := DecU32LE(Data, Offset+28);
      Info.HasRelation := true;
      Info.Is1Dim := true;
      Result := Offset + 2+2+4+4+4+4+4+4+4+(4*4); //HHIIiIIII4I
    end;

    7,14: begin //StructMDim
      Info.Unspecified1 := DecU16LE(Data, Offset);
      Info.Unspecified2 := DecU16LE(Data, Offset+2);
      Info.OptAddr := DecU32LE(Data, Offset+4);
      Info.NonOptAddr := DecU32LE(Data, Offset+8);
      Info.ArrayLowerBound := DecI32LE(Data, Offset+12);
      Info.ArrayElementCount := DecU32LE(Data, Offset+16);
      Offset := Offset+20;
      for i:=0 to 5 do Info.MDimLowerBounds[i] := DecI32LE(Data, Offset+i*4);
      Offset := Offset+24;
      for i:=0 to 5 do Info.MDimElementCount[i] := DecU32LE(Data, Offset+i*4);
      Offset := Offset+24;
      Info.NonOptStructSize := DecU32LE(Data, Offset);
      Info.OptStructSize := DecU32LE(Data, Offset+4);
      Info.RelationId := DecU32LE(Data, Offset+8);
      Offset := Offset + 4+4+4+(4*4); //III4I
      Info.HasRelation := true;
      Info.IsMDim := true;
      Result := Offset;
    end;

    15: begin //FbSfb
      Info.Unspecified1 := DecU16LE(Data, Offset);
      Info.Unspecified2 := DecU16LE(Data, Offset+2);
      Info.OptAddr := DecU32LE(Data, Offset+4);
      Info.NonOptAddr := DecU32LE(Data, Offset+8);
      Info.RelationId := DecU32LE(Data, Offset+12);
      Info.HasRelation := true;
      Result := Offset + 2+2+4+4+4+(4*4)+4+4; //HHIII4III
    end;

    0: begin //FbArray
      Info.Unspecified1 := DecU16LE(Data, Offset);
      Info.Unspecified2 := DecU16LE(Data, Offset+2);
      Info.OptAddr := DecU32LE(Data, Offset+4);
      Info.NonOptAddr := DecU32LE(Data, Offset+8);
      Offset := Offset+12;
      Info.RelationId := DecU32LE(Data, Offset); //I4I - only first value used
      Offset := Offset+20;
      Info.ArrayElementCount := DecU32LE(Data, Offset+8); //6I - sections[2]
      Offset := Offset+24;
      for i:=0 to 5 do Info.MDimLowerBounds[i] := DecI32LE(Data, Offset+i*4);
      Offset := Offset+24;
      for i:=0 to 5 do Info.MDimElementCount[i] := DecU32LE(Data, Offset+i*4);
      Offset := Offset+24;
      Info.HasRelation := true;
      Result := Offset;
    end;
  else
    raise Exception.CreateFmt('S7PlusTypeInfo: unsupported OffsetInfoType: %d',[OffsetInfoType]);
  end;
end;

function S7PlusVteOffsetInfoType(const El:TS7PlusVartypeElement):Byte;
begin
  Result := (El.AttributeFlags shr 12) and $0F;
end;

function S7PlusVteAttributeBitOffset(const El:TS7PlusVartypeElement):Byte;
begin
  Result := El.AttributeFlags and $07;
end;

function S7PlusVteNonOptBitOffset(const El:TS7PlusVartypeElement):Byte;
begin
  Result := (El.BitOffsetInfoFlags and $70) shr 4;
end;

function S7PlusVteOptBitOffset(const El:TS7PlusVartypeElement):Byte;
begin
  Result := El.BitOffsetInfoFlags and $07;
end;

function S7PlusVteClassic(const El:TS7PlusVartypeElement):Boolean;
begin
  Result := (El.BitOffsetInfoFlags and $08)<>0;
end;

function ParseVartypeElement(const Data:TBytes; Offset:Integer; out El:TS7PlusVartypeElement):Integer;
begin
  FillChar(El, SizeOf(El), 0);
  El.Lid := DecU32LE(Data, Offset);
  El.SymbolCrc := DecU32LE(Data, Offset+4);
  Offset := Offset+8;
  El.SoftDataType := Data[Offset];
  inc(Offset);
  El.AttributeFlags := (Word(Data[Offset]) shl 8) or Data[Offset+1]; //big-endian ">H" in reference
  Offset := Offset+2;
  El.BitOffsetInfoFlags := Data[Offset];
  inc(Offset);
  Offset := ParseOffsetInfo(Data, Offset, S7PlusVteOffsetInfoType(El), El.OffsetInfo);
  Result := Offset;
end;

function ParseVartypeList(const Data:TBytes; Offset:Integer; out Elements:TS7PlusVartypeElementArray):Integer;
var
  BlockLen:Word;
  BlockEnd:Integer;
  FirstBlock:Boolean;
  El:TS7PlusVartypeElement;
  Count:Integer;
begin
  SetLength(Elements, 0);
  Count := 0;
  FirstBlock := true;
  while true do begin
    BlockLen := (Word(Data[Offset]) shl 8) or Data[Offset+1]; //big-endian ">H"
    Offset := Offset+2;
    if BlockLen=0 then break;
    BlockEnd := Offset+BlockLen;
    if FirstBlock then begin
      Offset := Offset+4; //leading LE-u32 FirstId - a starting index, not a count
      FirstBlock := false;
    end;
    while Offset<BlockEnd do begin
      Offset := ParseVartypeElement(Data, Offset, El);
      SetLength(Elements, Count+1);
      Elements[Count] := El;
      inc(Count);
    end;
  end;
  Result := Offset;
end;

function ParseVarnameList(const Data:TBytes; Offset:Integer; out Names:TStringArray):Integer;
var
  BlockLen:Word;
  EndOff:Integer;
  NameLen:Byte;
  S:String;
  Count:Integer;
begin
  SetLength(Names, 0);
  Count := 0;
  while true do begin
    BlockLen := (Word(Data[Offset]) shl 8) or Data[Offset+1];
    Offset := Offset+2;
    if BlockLen=0 then break;
    EndOff := Offset+BlockLen;
    while Offset<EndOff do begin
      NameLen := Data[Offset];
      inc(Offset);
      SetString(S, PAnsiChar(@Data[Offset]), NameLen);
      Offset := Offset+NameLen;
      inc(Offset); //trailing zero byte
      SetLength(Names, Count+1);
      Names[Count] := S;
      inc(Count);
    end;
  end;
  Result := Offset;
end;

function S7PlusObjectAttr(const Obj:TS7PlusObject; AttrId:Cardinal; out Value:TBytes):Boolean;
var
  i:Integer;
begin
  Result := false;
  for i:=0 to High(Obj.AttrIds) do
    if Obj.AttrIds[i]=AttrId then begin
      Value := Obj.AttrValues[i];
      Result := true;
      exit;
    end;
end;

function ParseS7PlusObject(const Data:TBytes; Offset:Integer; out Obj:TS7PlusObject):Integer;
var
  Tag:Byte;
  c, i:Integer;
  AttrId:Cardinal;
  AttrVal:TBytes;
  Child:TS7PlusObject;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  if Data[Offset]<>S7PlusElement_StartOfObject then
    raise Exception.Create('S7PlusTypeInfo: expected StartOfObject ($A1)');
  inc(Offset);
  Obj.RelationId := DecodeUInt32(Data, Offset);
  Offset := Offset+4;
  Obj.ClassId := DecodeUInt32VLQ(Data, Offset, c);
  Offset := Offset+c;
  for i:=1 to 2 do begin //ClassFlags, AttributeId - not used by the tree builder, just skipped
    DecodeUInt32VLQ(Data, Offset, c);
    Offset := Offset+c;
  end;

  while Offset<Length(Data) do begin
    Tag := Data[Offset];
    if Tag=S7PlusElement_TerminatingObject then begin
      inc(Offset);
      break;
    end else if Tag=S7PlusElement_Attribute then begin
      inc(Offset);
      AttrId := DecodeUInt32VLQ(Data, Offset, c);
      Offset := Offset+c;
      AttrVal := DecodePValueToBytes(Data, Offset, c);
      Offset := Offset+c;
      SetLength(Obj.AttrIds, Length(Obj.AttrIds)+1);
      Obj.AttrIds[High(Obj.AttrIds)] := AttrId;
      SetLength(Obj.AttrValues, Length(Obj.AttrValues)+1);
      Obj.AttrValues[High(Obj.AttrValues)] := AttrVal;
    end else if Tag=S7PlusElement_VartypeList then begin
      inc(Offset);
      Offset := ParseVartypeList(Data, Offset, Obj.VartypeList);
    end else if Tag=S7PlusElement_VarnameList then begin
      inc(Offset);
      Offset := ParseVarnameList(Data, Offset, Obj.VarnameList);
    end else if Tag=S7PlusElement_StartOfObject then begin
      Offset := ParseS7PlusObject(Data, Offset, Child);
      SetLength(Obj.Objects, Length(Obj.Objects)+1);
      Obj.Objects[High(Obj.Objects)] := Child;
    end else
      inc(Offset); //unknown tag - skip defensively
  end;
  Result := Offset;
end;

function ParseS7PlusObjectList(const Data:TBytes; Offset:Integer; out Objects:TS7PlusObjectArray):Integer;
var
  Obj:TS7PlusObject;
begin
  SetLength(Objects, 0);
  while (Offset<Length(Data)) and (Data[Offset]=S7PlusElement_StartOfObject) do begin
    Offset := ParseS7PlusObject(Data, Offset, Obj);
    SetLength(Objects, Length(Objects)+1);
    Objects[High(Objects)] := Obj;
  end;
  Result := Offset;
end;

function S7PlusFindContainer(const Objects:TS7PlusObjectArray; ClassId:Cardinal; out Found:TS7PlusObject):Boolean;
var
  i:Integer;
begin
  Result := false;
  for i:=0 to High(Objects) do begin
    if Objects[i].ClassId=ClassId then begin
      Found := Objects[i];
      Result := true;
      exit;
    end;
    if S7PlusFindContainer(Objects[i].Objects, ClassId, Found) then begin
      Result := true;
      exit;
    end;
  end;
end;

function ExtractS7PlusTypeInfoObjects(const Response:TBytes; out Objects:TS7PlusObjectArray):Boolean;
var
  Offset, c:Integer;
  AllObjects:TS7PlusObjectArray;
  Container:TS7PlusObject;
begin
  SetLength(Objects, 0);
  Result := false;
  if Length(Response)=0 then exit;
  DecodeUInt32VLQ(Response, 0, c);
  Offset := c;
  while (Offset<Length(Response)) and (Response[Offset]<>S7PlusElement_StartOfObject) do
    inc(Offset);
  if Offset>=Length(Response) then exit;
  ParseS7PlusObjectList(Response, Offset, AllObjects);
  if S7PlusFindContainer(AllObjects, S7PlusExplore_ClassOMSTypeInfoContainer, Container) then begin
    Objects := Container.Objects;
    Result := true;
  end;
end;

function S7PlusTComSize(HasObj:Boolean; const Obj:TS7PlusObject):Cardinal;
var
  Raw:TBytes;
  i:Integer;
begin
  Result := 0;
  if not HasObj then exit;
  if not S7PlusObjectAttr(Obj, S7PlusExplore_TI_TComSize, Raw) then exit;
  if Length(Raw)=0 then exit;
  //Stored big-endian, but be defensive about the exact length.
  for i:=0 to High(Raw) do
    Result := (Result shl 8) or Raw[i];
end;

function S7PlusFindTypeObject(const Objects:TS7PlusObjectArray; RelationId:Cardinal; out Obj:TS7PlusObject):Boolean;
var
  i:Integer;
begin
  Result := false;
  for i:=0 to High(Objects) do
    if Objects[i].RelationId=RelationId then begin
      Obj := Objects[i];
      Result := true;
      exit;
    end;
end;

procedure S7PlusAddSubnodes(var Node:TS7PlusNode; const Obj:TS7PlusObject; const Objects:TS7PlusObjectArray); forward;

procedure S7PlusAddMDimSubnodes(var Subnode:TS7PlusNode; const Vte:TS7PlusVartypeElement; const Oi:TS7PlusOffsetInfo; const Objects:TS7PlusObjectArray);
var
  Counts:TS7PlusDim6Cardinal;
  Lowers:TS7PlusDim6Int;
  ActDimensions, i, j, ElemOff, ArrId, N:Integer;
  HasStructType:Boolean;
  StructType:TS7PlusObject;
  Stride:Integer;
  Label_:String;
  ArrNode:TS7PlusNode;
  Xx:array[0..5] of Integer;
begin
  Counts := Oi.MDimElementCount;
  Lowers := Oi.MDimLowerBounds;
  ActDimensions := 0;
  for i:=0 to 5 do if Counts[i]>0 then inc(ActDimensions);

  HasStructType := Oi.HasRelation and S7PlusFindTypeObject(Objects, Oi.RelationId, StructType);
  if Oi.HasRelation then
    Stride := S7PlusTComSize(HasStructType, StructType)
  else
    Stride := S7PlusDataTypeSize(Vte.SoftDataType, Oi.Unspecified1);

  for i:=0 to 5 do Xx[i] := 0;
  ArrId := 0;
  N := 1;
  while N<=Integer(Oi.ArrayElementCount) do begin
    Label_ := '[';
    for j:=ActDimensions-1 downto 0 do begin
      Label_ := Label_ + IntToStr(Xx[j]+Lowers[j]);
      if j>0 then Label_ := Label_+',';
    end;
    Label_ := Label_+']';
    ElemOff := (N-1)*Stride; //kept for parity with the reference; not used for LID addressing

    FillChar(ArrNode, SizeOf(ArrNode), 0);
    if Oi.HasRelation then begin
      ArrNode.NodeType := ntStructArray;
      ArrNode.Name := Label_;
      ArrNode.AccessId := ArrId;
      ArrNode.SoftDataType := Vte.SoftDataType;
      ArrNode.RelationId := Oi.RelationId;
      ArrNode.HasVte := true;
      ArrNode.Vte := Vte;
      SetLength(Subnode.Children, Length(Subnode.Children)+1);
      Subnode.Children[High(Subnode.Children)] := ArrNode;
      if HasStructType then
        S7PlusAddSubnodes(Subnode.Children[High(Subnode.Children)], StructType, Objects);
    end else begin
      ArrNode.NodeType := ntArray;
      ArrNode.Name := Label_;
      ArrNode.AccessId := ArrId;
      ArrNode.SoftDataType := Vte.SoftDataType;
      ArrNode.HasVte := true;
      ArrNode.Vte := Vte;
      SetLength(Subnode.Children, Length(Subnode.Children)+1);
      Subnode.Children[High(Subnode.Children)] := ArrNode;
    end;

    //Odometer step (axis 0 fastest).
    inc(Xx[0]);
    if (TS7PlusSoftDataType(Vte.SoftDataType)=sdtBBOOL) and (Xx[0]>=Integer(Counts[0])) and ((Counts[0] mod 8)<>0) then
      inc(ArrId, 8 - (Xx[0] mod 8));
    for i:=0 to 4 do
      if Xx[i]>=Integer(Counts[i]) then begin
        Xx[i] := 0;
        inc(Xx[i+1]);
      end;
    inc(ArrId);
    inc(N);
  end;
end;

procedure S7PlusAddSubnodes(var Node:TS7PlusNode; const Obj:TS7PlusObject; const Objects:TS7PlusObjectArray);
var
  i, Elem:Integer;
  Vte:TS7PlusVartypeElement;
  NameStr:String;
  Subnode:TS7PlusNode;
  Oi:TS7PlusOffsetInfo;
  HasStructType:Boolean;
  StructType:TS7PlusObject;
  Stride:Integer;
  Label_:String;
  ArrNode:TS7PlusNode;
begin
  for i:=0 to High(Obj.VartypeList) do begin
    Vte := Obj.VartypeList[i];
    if i<Length(Obj.VarnameList) then NameStr := Obj.VarnameList[i] else NameStr := '';

    FillChar(Subnode, SizeOf(Subnode), 0);
    Subnode.NodeType := ntUndefined;
    Subnode.Name := NameStr;
    Subnode.AccessId := Vte.Lid;
    Subnode.SoftDataType := Vte.SoftDataType;
    Subnode.HasVte := true;
    Subnode.Vte := Vte;
    SetLength(Node.Children, Length(Node.Children)+1);
    Node.Children[High(Node.Children)] := Subnode;

    Oi := Vte.OffsetInfo;

    if Oi.Is1Dim then begin
      for Elem:=0 to Integer(Oi.ArrayElementCount)-1 do begin
        Label_ := '['+IntToStr(Elem+Oi.ArrayLowerBound)+']';
        if Oi.HasRelation then begin
          HasStructType := S7PlusFindTypeObject(Objects, Oi.RelationId, StructType);
          Stride := S7PlusTComSize(HasStructType, StructType);
          FillChar(ArrNode, SizeOf(ArrNode), 0);
          ArrNode.NodeType := ntStructArray;
          ArrNode.Name := Label_;
          ArrNode.AccessId := Elem;
          ArrNode.SoftDataType := Vte.SoftDataType;
          ArrNode.RelationId := Oi.RelationId;
          ArrNode.HasVte := true;
          ArrNode.Vte := Vte;
          SetLength(Node.Children[High(Node.Children)].Children, Length(Node.Children[High(Node.Children)].Children)+1);
          Node.Children[High(Node.Children)].Children[High(Node.Children[High(Node.Children)].Children)] := ArrNode;
          if HasStructType then
            S7PlusAddSubnodes(Node.Children[High(Node.Children)].Children[High(Node.Children[High(Node.Children)].Children)], StructType, Objects);
        end else begin
          Stride := S7PlusDataTypeSize(Vte.SoftDataType, Oi.Unspecified1);
          FillChar(ArrNode, SizeOf(ArrNode), 0);
          ArrNode.NodeType := ntArray;
          ArrNode.Name := Label_;
          ArrNode.AccessId := Elem;
          ArrNode.SoftDataType := Vte.SoftDataType;
          ArrNode.HasVte := true;
          ArrNode.Vte := Vte;
          SetLength(Node.Children[High(Node.Children)].Children, Length(Node.Children[High(Node.Children)].Children)+1);
          Node.Children[High(Node.Children)].Children[High(Node.Children[High(Node.Children)].Children)] := ArrNode;
        end;
      end;
    end else if Oi.IsMDim then
      S7PlusAddMDimSubnodes(Node.Children[High(Node.Children)], Vte, Oi, Objects)
    else if Oi.HasRelation then begin
      HasStructType := S7PlusFindTypeObject(Objects, Oi.RelationId, StructType);
      if HasStructType then
        S7PlusAddSubnodes(Node.Children[High(Node.Children)], StructType, Objects);
    end;
    //else: scalar leaf, no children.
  end;
end;

procedure S7PlusBuildTree(var RootNodes:TS7PlusNodeArray; const TypeObjects:TS7PlusObjectArray);
var
  i:Integer;
  Obj:TS7PlusObject;
begin
  for i:=0 to High(RootNodes) do begin
    if RootNodes[i].NodeType<>ntRoot then continue;
    if S7PlusFindTypeObject(TypeObjects, RootNodes[i].RelationId, Obj) then
      S7PlusAddSubnodes(RootNodes[i], Obj, TypeObjects);
  end;
end;

function S7PlusFormatLids(const Lids:TS7PlusLIDArray):String;
var
  i:Integer;
begin
  Result := '';
  for i:=0 to High(Lids) do begin
    if i>0 then Result := Result+'.';
    Result := Result+IntToHex(Lids[i],1);
  end;
end;

procedure S7PlusWalk(const Node:TS7PlusNode; Names:String; const Lids:TS7PlusLIDArray; ByteOffset:Integer; var Result_:TS7PlusVarInfoArray); forward;

procedure S7PlusWalk(const Node:TS7PlusNode; Names:String; const Lids:TS7PlusLIDArray; ByteOffset:Integer; var Result_:TS7PlusVarInfoArray);
var
  NewLids:TS7PlusLIDArray;
  NewByteOffset:Integer;
  i:Integer;
  Info:TS7PlusVarInfo;
begin
  case Node.NodeType of
    ntRoot: begin
      //The root's AccessId is the DB/area's AccessArea (e.g. $8A0E0004), not a LID - it
      //is passed separately to EncodeItemAddress, so it must NOT go into the Lids array
      //used to actually address the variable (it only shows up in the display name).
      Names := Names + Node.Name;
      NewLids := Lids;
      NewByteOffset := 0;
    end;
    ntArray, ntStructArray: begin
      //Array element byte offsets are not computed today (see S7PlusAddSubnodes/
      //S7PlusAddMDimSubnodes - ElemOff/Stride are kept only for parity with the reference
      //and explicitly not used for LID addressing), so nothing under an array element can
      //reliably participate in the containment-based read-dedup optimization.
      if Node.NodeType=ntArray then begin
        Names := Names + Node.Name; //"[..]" index label, no dot
        SetLength(NewLids, Length(Lids)+1);
        if Length(Lids)>0 then Move(Lids[0], NewLids[0], Length(Lids)*SizeOf(Cardinal));
        NewLids[High(NewLids)] := Node.AccessId;
      end else begin
        Names := Names + Node.Name;
        SetLength(NewLids, Length(Lids)+2);
        if Length(Lids)>0 then Move(Lids[0], NewLids[0], Length(Lids)*SizeOf(Cardinal));
        NewLids[High(NewLids)-1] := Node.AccessId;
        NewLids[High(NewLids)] := 1;
      end;
      NewByteOffset := -1;
    end;
  else //UNDEFINED / VAR member
    Names := Names + '.' + Node.Name;
    SetLength(NewLids, Length(Lids)+1);
    if Length(Lids)>0 then Move(Lids[0], NewLids[0], Length(Lids)*SizeOf(Cardinal));
    NewLids[High(NewLids)] := Node.AccessId;
    if (ByteOffset=-1) or (not Node.HasVte) then
      NewByteOffset := -1
    else
      NewByteOffset := ByteOffset + Integer(Node.Vte.OffsetInfo.OptAddr);
  end;

  if Length(Node.Children)>0 then begin
    //Struct-typed containers (plain UDT members, or a struct-array element) are, besides
    //being walked into for their individual fields below, also addressable as a whole -
    //the PLC resolves the LID path down to this object and (per GetMultiVariables/PValue)
    //returns the raw byte blob for its full layout. This also includes the root itself
    //(the whole DB/area, e.g. "DB4") - EncodeItemAddress with an empty Lids array just
    //encodes the AccessSubArea alone (field count 1, no LIDs), which addresses the area's
    //"value actual" object directly - untested against real hardware until now, kept
    //here experimentally to find out whether the PLC accepts it.
    Info.Name := Names;
    Info.Lids := NewLids;
    Info.SoftDataType := Node.SoftDataType;
    Info.ByteOffset := NewByteOffset;
    SetLength(Result_, Length(Result_)+1);
    Result_[High(Result_)] := Info;

    for i:=0 to High(Node.Children) do
      S7PlusWalk(Node.Children[i], Names, NewLids, NewByteOffset, Result_);
    exit;
  end;

  //Leaf node - emit if the datatype is a readable leaf.
  if not IsSoftDataTypeSupported(Node.SoftDataType) then exit;

  Info.Name := Names;
  Info.Lids := NewLids;
  Info.SoftDataType := Node.SoftDataType;
  Info.ByteOffset := NewByteOffset;
  SetLength(Result_, Length(Result_)+1);
  Result_[High(Result_)] := Info;
end;

function S7PlusBuildFlatList(const RootNodes:TS7PlusNodeArray):TS7PlusVarInfoArray;
var
  i:Integer;
  EmptyLids:TS7PlusLIDArray;
begin
  SetLength(Result, 0);
  SetLength(EmptyLids, 0);
  for i:=0 to High(RootNodes) do begin
    if Length(RootNodes[i].Children)=0 then continue;
    S7PlusWalk(RootNodes[i], '', EmptyLids, 0, Result);
  end;
end;

//: Decodes a block-name PValue's raw bytes: the S7-1500 sends either genuine UTF-16-BE (an
//: embedded 0x00 high byte on every ASCII char) or packed one-byte-per-char ASCII with no
//: high bytes at all - the presence of a null byte disambiguates the two (PLC identifiers
//: never contain an embedded null). Block/tag names are ASCII identifiers in practice, so
//: this only needs to handle that range, not full Unicode.
function S7PlusDecodeNameBytes(const Value:TBytes):String;
var
  HasNull:Boolean;
  i:Integer;
begin
  HasNull := false;
  for i:=0 to High(Value) do
    if Value[i]=0 then begin
      HasNull := true;
      break;
    end;

  Result := '';
  if HasNull then begin
    i := 0;
    while i+1<=High(Value) do begin
      if Value[i]<>0 then
        Result := Result + Chr(Value[i]) //non-ASCII high byte: best-effort passthrough
      else
        Result := Result + Chr(Value[i+1]);
      inc(i, 2);
    end;
  end else begin
    SetLength(Result, Length(Value));
    for i:=0 to High(Value) do
      Result[i+1] := Chr(Value[i]);
  end;

  while (Length(Result)>0) and (Result[Length(Result)]=#0) do
    SetLength(Result, Length(Result)-1);
end;

function S7PlusParseExploreDataBlocks(const Response:TBytes):TS7PlusDataBlockInfoArray;
type
  TStackEntry = record
    RelId:Cardinal;
    ClassId:Cardinal;
    Name:String;
  end;
var
  Offset, Consumed:Integer;
  Tag:Byte;
  RelId, ClassId, AttrId:Cardinal;
  Value:TBytes;
  Stack:array of TStackEntry;
begin
  SetLength(Result, 0);
  SetLength(Stack, 0);
  Offset := 0;

  if Offset<Length(Response) then begin
    DecodeUInt64VLQ(Response, Offset, Consumed); //leading ReturnValue - discarded
    Offset := Offset+Consumed;
  end;

  while Offset<Length(Response) do begin
    Tag := Response[Offset];

    if Tag=S7PlusElement_StartOfObject then begin
      inc(Offset);
      if Offset+4>Length(Response) then break;
      RelId := (Cardinal(Response[Offset]) shl 24) or (Cardinal(Response[Offset+1]) shl 16) or
               (Cardinal(Response[Offset+2]) shl 8) or Cardinal(Response[Offset+3]);
      Offset := Offset+4;
      ClassId := DecodeUInt32VLQ(Response, Offset, Consumed); Offset := Offset+Consumed;
      DecodeUInt32VLQ(Response, Offset, Consumed); Offset := Offset+Consumed; //ClassFlags, unused
      DecodeUInt32VLQ(Response, Offset, Consumed); Offset := Offset+Consumed; //AttributeId, unused
      SetLength(Stack, Length(Stack)+1);
      Stack[High(Stack)].RelId := RelId;
      Stack[High(Stack)].ClassId := ClassId;
      Stack[High(Stack)].Name := '';
    end

    else if Tag=S7PlusElement_TerminatingObject then begin
      inc(Offset);
      if Length(Stack)>0 then begin
        if (Stack[High(Stack)].ClassId=S7PlusIds_DBClassRID) and
           ((Stack[High(Stack)].RelId shr 16)=$8A0E) then begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)].Name := Stack[High(Stack)].Name;
          Result[High(Result)].Number := Stack[High(Stack)].RelId and $FFFF;
          Result[High(Result)].Rid := Stack[High(Stack)].RelId;
        end;
        SetLength(Stack, Length(Stack)-1);
      end;
    end

    else if Tag=S7PlusElement_Attribute then begin
      inc(Offset);
      AttrId := DecodeUInt32VLQ(Response, Offset, Consumed); Offset := Offset+Consumed;
      try
        Value := DecodePValueToBytes(Response, Offset, Consumed);
      except
        break;
      end;
      Offset := Offset+Consumed;
      if (AttrId=S7PlusIds_ObjectVariableTypeName) and (Length(Stack)>0) then
        Stack[High(Stack)].Name := S7PlusDecodeNameBytes(Value);
    end

    else
      inc(Offset); //response preamble / unhandled element tags (e.g. Relation)
  end;
end;

end.
