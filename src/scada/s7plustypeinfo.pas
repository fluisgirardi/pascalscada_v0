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
  //: Modification time of a type-info object (a ULInt the PLC changes when the type is
  //: downloaded again); not every PLC reports it. Name from the attribute table of Wireshark's
  //: S7CommPlus dissector.
  S7PlusExplore_TI_ModificationTime       = 529;

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
    //: ntStructArray: where this element starts, counted from the start of the array
    //: (index x the size of the structure).
    ElemOffset:Integer;
    Children:array of TS7PlusNode;
  end;
  TS7PlusNodeArray = array of TS7PlusNode;

  TS7PlusVarInfo = record
    Name:String;
    Lids:TS7PlusLIDArray;
    SoftDataType:Byte;
    //: The variable's own type-layout CRC, as parsed from its PVartypeElement (0 if the
    //: node has no Vte, e.g. the whole-DB/area root). A plain GetMultiVariables read/write
    //: accepts 0 here ("skip layout check"), but a Subscription's reference list is a
    //: persistent object bound to the layout for as long as it lives - passing 0 there was
    //: rejected by the PLC ("Download error"); see TS7PlusConnection.
    //: BuildSubscriptionReferenceList, which is the one caller that actually needs this.
    SymbolCrc:Cardinal;
    //: Cumulative byte offset from the DB/area root (sum of every ancestor's own
    //: OffsetInfo.NonOptAddr along the struct-nesting path - the classic, byte-addressable
    //: layout, not OptAddr's runtime-internal one), used to slice a member's bytes out of an
    //: ancestor's already-read blob instead of re-reading it independently - see
    //: TS7CommPlusDriver.TryDeduplicateEntry. -1 means "not reliably known": this
    //: happens for anything inside an array (S7PlusAddMDimSubnodes/array element offsets
    //: are explicitly not computed for LID addressing today), so those never participate
    //: in the dedup optimization and are always read independently.
    ByteOffset:Integer;
    //: Bit within the byte at ByteOffset (0..7) - only meaningful for BOOL members, which
    //: share a byte with the next ones; 0 for everything else. Read from the low 3 bits of
    //: the element's AttributeFlags (confirmed on a PLC: three BOOLs of one struct, all at
    //: byte 114, came out as bits 0, 1, 2; the BitOffsetInfoFlags fields were 0 for all).
    BitOffset:Byte;
    //: Where the member sits in the PLC's memory image of the DB/area: the sum of the
    //: OptAddr of the element and of every ancestor. Unlike ByteOffset (the classic layout),
    //: this follows an "optimized" struct's real order - the PLC lays its members out by size
    //: (4-byte ones, then 2, then 1, then BOOLs one byte each), not by declaration - so the
    //: difference between a member's and its struct's ImageOffset is where the member is in
    //: the bytes a read of that struct returns (confirmed on a PLC: DB4.Var26's v3 DWord at
    //: 0, Static_1 at 4, v2 at 8, v1 at 10, BOOLs at 11..14, against ByteOffset's 0, 10, 2, 4,
    //: 8..14). For a classic DB both layouts agree. -1: not known (inside an array, no Vte).
    ImageOffset:Integer;
    //: Bytes a read of this variable returns (a STRING: maximum length + 2; an array of
    //: scalars: count x element); 0 = not known (a structure, a multi-dimensional array).
    //: Used to keep the reply of a batched read inside one PDU.
    DataSize:Integer;
    //: The 4-bit POffsetInfoType and the BitOffsetInfoFlags of the element (what the PLC says of
    //: how it lays the variable out); 0/0 when the node has no Vte.
    OffsetInfoType:Byte;
    BitOffsetFlags:Byte;
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

//: The identity of a list of DBs (name, number, RID of each, in a fixed order), to tell whether
//: the program of the PLC has other DBs than when it was last read.
function S7PlusBlockSignature(const Blocks:TS7PlusDataBlockInfoArray):String;

//: Reads, from an EXPLORE reply, the attribute AttrId of the object whose RelationId is
//: RelationId (searched through the nested objects too). False when the reply has no such
//: object or the object does not carry the attribute.
function S7PlusFindObjectAttr(const Response:TBytes; RelationId, AttrId:Cardinal; out Value:TBytes):Boolean;

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

//Where the next $A1 (StartOfObject) is at or after From; -1 if there is none.
function NextObjectStart(const R:TBytes; From:Integer):Integer;
begin
  Result := From;
  while (Result<Length(R)) and (R[Result]<>S7PlusElement_StartOfObject) do
    inc(Result);
  if Result>=Length(R) then Result := -1;
end;

//The preamble of an EXPLORE reply holds a request counter in VLQ, and when its first byte is $A1
//that byte is taken for the start of an object. So the $A1 bytes are tried one after the other,
//up to this many, until one of them starts the objects wanted (as the Python library does).
const
  MaxObjectStartCandidates = 8;

function ExtractS7PlusTypeInfoObjects(const Response:TBytes; out Objects:TS7PlusObjectArray):Boolean;
var
  Offset, c, Tries:Integer;
  AllObjects:TS7PlusObjectArray;
  Container:TS7PlusObject;
begin
  SetLength(Objects, 0);
  Result := false;
  if Length(Response)=0 then exit;
  DecodeUInt32VLQ(Response, 0, c);
  Offset := c;
  for Tries:=1 to MaxObjectStartCandidates do begin
    Offset := NextObjectStart(Response, Offset);
    if Offset<0 then exit;
    SetLength(AllObjects, 0);
    try
      ParseS7PlusObjectList(Response, Offset, AllObjects);
    except
      SetLength(AllObjects, 0);     //not an object after all
    end;
    if S7PlusFindContainer(AllObjects, S7PlusExplore_ClassOMSTypeInfoContainer, Container) then begin
      Objects := Container.Objects;
      Result := true;
      exit;
    end;
    inc(Offset);
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

//The size of a structure type counted from its members (classic layout: the end of the last
//member, rounded up to an even number of bytes), for a type that does not say its size
//(TComSize). 0 when a member has no known size (a nested structure or array, an unknown type).
function S7PlusStructExtent(const StructType:TS7PlusObject; const Objects:TS7PlusObjectArray):Integer;
var
  Tmp:TS7PlusNode;
  i, Sz, End_:Integer;
begin
  Result := 0;
  FillChar(Tmp, SizeOf(Tmp), 0);
  S7PlusAddSubnodes(Tmp, StructType, Objects);
  for i:=0 to High(Tmp.Children) do begin
    if (not Tmp.Children[i].HasVte) or (Length(Tmp.Children[i].Children)>0) then begin
      Result := 0;
      exit;
    end;
    Sz := S7PlusDataTypeSize(Tmp.Children[i].SoftDataType, Tmp.Children[i].Vte.OffsetInfo.Unspecified1);
    if Sz=0 then begin
      Result := 0;
      exit;
    end;
    End_ := Integer(Tmp.Children[i].Vte.OffsetInfo.NonOptAddr)+Sz;
    if End_>Result then Result := End_;
  end;
  if Odd(Result) then Inc(Result);
end;

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
  //the structure type does not always say its size: work it out from its members
  if Oi.HasRelation and HasStructType and (Stride=0) then
    Stride := S7PlusStructExtent(StructType, Objects);

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
    ElemOff := (N-1)*Stride; //only used for the byte offsets of an array of structures (not for LID addressing)

    FillChar(ArrNode, SizeOf(ArrNode), 0);
    if Oi.HasRelation then begin
      ArrNode.NodeType := ntStructArray;
      ArrNode.ElemOffset := ElemOff;
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
          //the structure type does not always say its size: work it out from its members
          if HasStructType and (Stride=0) then
            Stride := S7PlusStructExtent(StructType, Objects);
          FillChar(ArrNode, SizeOf(ArrNode), 0);
          ArrNode.NodeType := ntStructArray;
          ArrNode.ElemOffset := Elem*Stride;
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

procedure S7PlusWalk(const Node:TS7PlusNode; Names:String; const Lids:TS7PlusLIDArray; ByteOffset, ImageOffset:Integer; Classic:Boolean; var Result_:TS7PlusVarInfoArray); forward;

procedure S7PlusWalk(const Node:TS7PlusNode; Names:String; const Lids:TS7PlusLIDArray; ByteOffset, ImageOffset:Integer; Classic:Boolean; var Result_:TS7PlusVarInfoArray);
var
  ThisClassic:Boolean;
  NewLids:TS7PlusLIDArray;
  NewByteOffset, NewImageOffset:Integer;
  i:Integer;
  Info:TS7PlusVarInfo;
begin
  ThisClassic := Classic;
  case Node.NodeType of
    ntRoot: begin
      //The root's AccessId is the DB/area's AccessArea (e.g. $8A0E0004), not a LID - it
      //is passed separately to EncodeItemAddress, so it must NOT go into the Lids array
      //used to actually address the variable (it only shows up in the display name).
      Names := Names + Node.Name;
      NewLids := Lids;
      NewByteOffset := 0;
      NewImageOffset := 0;
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
      NewImageOffset := -1;
      //An element of an array of STRUCTURES is placed after the ones before it, each as big as
      //the structure, and its members have their own offsets inside it: both are known.
      //(An array of scalars keeps no offsets: its elements are addressed through the array.)
      if (Node.NodeType=ntStructArray) and (ByteOffset<>-1) then
        NewByteOffset := ByteOffset + Node.ElemOffset;
    end;
  else //UNDEFINED / VAR member
    Names := Names + '.' + Node.Name;
    SetLength(NewLids, Length(Lids)+1);
    if Length(Lids)>0 then Move(Lids[0], NewLids[0], Length(Lids)*SizeOf(Cardinal));
    NewLids[High(NewLids)] := Node.AccessId;
    if (ImageOffset=-1) or (not Node.HasVte) then
      NewImageOffset := -1
    else begin
      //A DB with "optimized block access" off: the PLC answers a read of a structure in the classic
      //order - the image IS the classic layout (OptAddr means nothing there). Only the node of the
      //structure carries the flag (BitOffsetInfoFlags $08), not its members: they inherit it.
      if S7PlusVteClassic(Node.Vte) then ThisClassic := true;
      if ThisClassic then
        NewImageOffset := ImageOffset + Integer(Node.Vte.OffsetInfo.NonOptAddr)
      else
        NewImageOffset := ImageOffset + Integer(Node.Vte.OffsetInfo.OptAddr);
    end;
    if (ByteOffset=-1) or (not Node.HasVte) then
      NewByteOffset := -1
    else
      //NonOptAddr (not OptAddr): OptAddr is the "optimized"/runtime-internal address, which
      //can include compiler bookkeeping gaps ahead of the first declared member (confirmed
      //against real hardware: DB4.Var1's OptAddr was 4, not 0, throwing off "whole DB"
      //byte-buffer assembly). NonOptAddr is the classic, byte-addressable layout (what
      //DB.DBBx/DBWx/DBDx addressing and a "read the whole DB" buffer both expect) - see
      //python-snap7's typeinfo.py _walk, which tracks both in parallel for the same reason.
      NewByteOffset := ByteOffset + Integer(Node.Vte.OffsetInfo.NonOptAddr);
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
    if Node.HasVte then Info.SymbolCrc := Node.Vte.SymbolCrc else Info.SymbolCrc := 0;
    Info.ByteOffset := NewByteOffset;
    Info.BitOffset := 0;
    Info.ImageOffset := NewImageOffset;
    Info.DataSize := 0;
    Info.OffsetInfoType := 0;
    Info.BitOffsetFlags := 0;
    if Node.HasVte then begin
      Info.OffsetInfoType := S7PlusVteOffsetInfoType(Node.Vte);
      Info.BitOffsetFlags := Node.Vte.BitOffsetInfoFlags;
    end;
    //an array of scalars is read whole: count x element
    if Node.HasVte and Node.Vte.OffsetInfo.Is1Dim and (not Node.Vte.OffsetInfo.HasRelation) then
      Info.DataSize := Integer(Node.Vte.OffsetInfo.ArrayElementCount)*
                       S7PlusDataTypeSize(Node.SoftDataType, Node.Vte.OffsetInfo.Unspecified1);
    SetLength(Result_, Length(Result_)+1);
    Result_[High(Result_)] := Info;

    for i:=0 to High(Node.Children) do
      S7PlusWalk(Node.Children[i], Names, NewLids, NewByteOffset, NewImageOffset, ThisClassic, Result_);
    exit;
  end;

  //Leaf node - emit if the datatype is a readable leaf.
  if not IsSoftDataTypeSupported(Node.SoftDataType) then exit;

  Info.Name := Names;
  Info.Lids := NewLids;
  Info.SoftDataType := Node.SoftDataType;
  if Node.HasVte then Info.SymbolCrc := Node.Vte.SymbolCrc else Info.SymbolCrc := 0;
  Info.ByteOffset := NewByteOffset;
  if Node.HasVte and (NewByteOffset<>-1) then
    Info.BitOffset := S7PlusVteAttributeBitOffset(Node.Vte)
  else
    Info.BitOffset := 0;
  Info.ImageOffset := NewImageOffset;
  if Node.HasVte then
    Info.DataSize := S7PlusDataTypeSize(Node.SoftDataType, Node.Vte.OffsetInfo.Unspecified1)
  else
    Info.DataSize := 0;
  Info.OffsetInfoType := 0;
  Info.BitOffsetFlags := 0;
  if Node.HasVte then begin
    Info.OffsetInfoType := S7PlusVteOffsetInfoType(Node.Vte);
    Info.BitOffsetFlags := Node.Vte.BitOffsetInfoFlags;
  end;
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
    S7PlusWalk(RootNodes[i], '', EmptyLids, 0, 0, false, Result);
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

function S7PlusBlockSignature(const Blocks:TS7PlusDataBlockInfoArray):String;
var
  L:TStringList;
  i:Integer;
begin
  L := TStringList.Create;
  try
    for i:=0 to High(Blocks) do
      L.Add(Format('%s|%d|%d', [Blocks[i].Name, Blocks[i].Number, Blocks[i].Rid]));
    L.Sort;
    L.Delimiter := ';';
    L.StrictDelimiter := true;
    Result := L.DelimitedText;
  finally
    L.Free;
  end;
end;

function FindObjectByRelation(const Objects:TS7PlusObjectArray; RelationId:Cardinal; out Found:TS7PlusObject):Boolean;
var
  i:Integer;
begin
  Result := false;
  for i:=0 to High(Objects) do begin
    if Objects[i].RelationId=RelationId then begin
      Found := Objects[i];
      Result := true;
      exit;
    end;
    if FindObjectByRelation(Objects[i].Objects, RelationId, Found) then begin
      Result := true;
      exit;
    end;
  end;
end;

function S7PlusFindObjectAttr(const Response:TBytes; RelationId, AttrId:Cardinal; out Value:TBytes):Boolean;
var
  Offset, c, Tries:Integer;
  AllObjects:TS7PlusObjectArray;
  Obj:TS7PlusObject;
begin
  SetLength(Value, 0);
  Result := false;
  if Length(Response)=0 then exit;
  DecodeUInt32VLQ(Response, 0, c);      //the ReturnValue
  Offset := c;
  //see MaxObjectStartCandidates: a $A1 in the counter of the preamble is not an object
  for Tries:=1 to MaxObjectStartCandidates do begin
    Offset := NextObjectStart(Response, Offset);
    if Offset<0 then exit;
    SetLength(AllObjects, 0);
    try
      ParseS7PlusObjectList(Response, Offset, AllObjects);
    except
      SetLength(AllObjects, 0);
    end;
    if FindObjectByRelation(AllObjects, RelationId, Obj) and S7PlusObjectAttr(Obj, AttrId, Value) then begin
      Result := true;
      exit;
    end;
    inc(Offset);
  end;
end;

end.
