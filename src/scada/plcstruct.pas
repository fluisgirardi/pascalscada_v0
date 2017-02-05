{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementação de um tag estrutura de comunicação.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit that implements a structure communication tag.)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Moved OpenElementMapper to StructTagAssistant to remove form dependencies
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  10/2014 - Switched back to the old behavior but keeping the improvemnt
  of Juanjo (do not link with GUI);
  ***********************************************************************
}
{$ENDIF}
unit PLCStruct;

interface

uses
  Classes, PLCBlock, Tag, ProtocolTypes;

type
  {$IFDEF PORTUGUES}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    @abstract(Classe de tag estrutura de comunicação.)
  }
  {$ELSE}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    @abstract(Class of an structure communication tag.)
  }
  {$ENDIF}

  { TPLCStruct }

  TPLCStruct = class(TPLCBlock)
  protected
    //: @seealso(TPLCTag.IsMyCallBack)
    function IsMyCallBack(Cback: TTagCommandCallBack): Boolean; override;
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:LongInt); override;
    //: @seealso(TPLCTag.SetTagType)
    procedure SetTagType(newType:TTagType); override;
    //: @seealso(TPLCTag.SwapDWords)
    procedure SetSwapDWords(v:Boolean); override;
    //: @seealso(TPLCTag.SetSwapWords)
    procedure SetSwapWords(v:Boolean); override;
    //: @seealso(TPLCTag.SetSwapBytes)
    procedure SetSwapBytes(v:Boolean); override;
  public
    //: @xclude
    constructor Create(AOwner:TComponent); override;

    //: @seealso(TPLCBlock.MapElements)
    procedure MapElements(InsertHook: TAddTagInEditorHook;
       CreateProc: TCreateTagProc); override;

    class procedure addByte (aArray: TArrayOfDouble; offset: Integer; aByte:  Byte);
    class procedure AddDWord(aArray: TArrayOfDouble; offset: Integer; aDWord: LongInt;  aSwapBytes, aSwapWords: Boolean);
    class procedure AddDWord(aArray: TArrayOfDouble; offset: Integer; aDWord: DWord;    aSwapBytes, aSwapWords: Boolean);
    class procedure AddDWord(aArray: TArrayOfDouble; offset: Integer; aDWord: Single;   aSwapBytes, aSwapWords: Boolean);
    class procedure AddQWord(aArray: TArrayOfDouble; offset: Integer; aQWord: QWord;    aSwapBytes, aSwapWords, aSwapDWords: Boolean);
    class procedure AddQWord(aArray: TArrayOfDouble; offset: Integer; aQWord: Int64;    aSwapBytes, aSwapWords, aSwapDWords: Boolean);
    class procedure AddQWord(aArray: TArrayOfDouble; offset: Integer; aQWord: Double;   aSwapBytes, aSwapWords, aSwapDWords: Boolean);
    class procedure AddWord (aArray: TArrayOfDouble; offset: Integer; aWord:  Word;     aSwapBytes: Boolean);
    class procedure AddWord (aArray: TArrayOfDouble; offset: Integer; aWord:  SmallInt; aSwapBytes: Boolean);

    class procedure AddCString      (aArray: TArrayOfDouble; offset: Integer; aString: AnsiString);
    class procedure AddSiemensString(aArray: TArrayOfDouble; offset: Integer; aString: AnsiString; MaxSize: Byte);

    function GetByte(Offset:Integer):Byte;
    function GetWord(Offset:Integer; aSwapBytes:Boolean):Word;
    function GetSmallInt(Offset:Integer; aSwapBytes:Boolean):SmallInt;
    function GetLongWord(Offset:Integer; aSwapBytes, aSwapWords:Boolean):LongWord;
    function GetLongInt(Offset:Integer; aSwapBytes, aSwapWords:Boolean):LongInt;
    function GetSingle(Offset:Integer; aSwapBytes, aSwapWords:Boolean):Single;

    function GetQWord (Offset:Integer; aSwapBytes, aSwapWords, aSwapDWords:Boolean):QWord;
    function GetInt64 (Offset:Integer; aSwapBytes, aSwapWords, aSwapDWords:Boolean):Int64;
    function GetDouble(Offset:Integer; aSwapBytes, aSwapWords, aSwapDWords:Boolean):Double;

    function GetSiemensString(Offset:Integer; MaxStringSize:Integer = 255):String;

  end;

  procedure SetStructItemMapper(StructItemMapperTool:TOpenTagEditor);

implementation

uses sysutils, math, hsstrings;

constructor TPLCStruct.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Inherited SetTagType(pttByte);
end;

function TPLCStruct.IsMyCallBack(Cback: TTagCommandCallBack): Boolean;
begin
  Result:=inherited IsMyCallBack(Cback) and (TMethod(Cback).Code=Pointer(@TPLCStruct.TagCommandCallBack));
end;

procedure TPLCStruct.TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:LongInt);
begin
  inherited TagCommandCallBack(Values, ValuesTimeStamp, TagCommand, LastResult, Offset);
end;

procedure TPLCStruct.SetTagType(newType:TTagType);
begin
  Inherited SetTagType(pttByte);
end;

procedure TPLCStruct.SetSwapDWords(v: Boolean);
begin
  inherited SetSwapDWords(false);
end;

procedure TPLCStruct.SetSwapWords(v:Boolean);
begin
  inherited SetSwapWords(false);
end;

procedure TPLCStruct.SetSwapBytes(v:Boolean);
begin
  inherited SetSwapBytes(false);
end;

class procedure TPLCStruct.addByte(aArray:TArrayOfDouble; offset:Integer; aByte:Byte);
begin
  if offset<Length(aArray) then
    aArray[offset]:=aByte;
end;

class procedure TPLCStruct.AddWord(aArray:TArrayOfDouble; offset:Integer; aWord:Word; aSwapBytes:Boolean);
var
  aInvertWordBytes:array[0..1] of byte absolute aWord;
  aux:Byte;
begin
  if aSwapBytes then begin
    aux:=aInvertWordBytes[0];
    aInvertWordBytes[0]:=aInvertWordBytes[1];
    aInvertWordBytes[1]:=aux;
  end;

  addByte(aArray, offset+0, aInvertWordBytes[0]);
  addByte(aArray, offset+1, aInvertWordBytes[1]);
end;

class procedure TPLCStruct.AddWord(aArray:TArrayOfDouble; offset:Integer; aWord:SmallInt; aSwapBytes:Boolean);
begin
  AddWord(aArray,offset, Word(aWord), aSwapBytes);
end;

class procedure TPLCStruct.AddDWord(aArray:TArrayOfDouble; offset:Integer; aDWord:DWord; aSwapBytes, aSwapWords:Boolean);
var
  InvertedWordsOfDword:array[0..1] of Word absolute aDWord;
  aux:Word;
begin
  if aSwapWords then begin
    aux:=InvertedWordsOfDword[0];
    InvertedWordsOfDword[0]:=InvertedWordsOfDword[1];
    InvertedWordsOfDword[1]:=aux;
  end;

  AddWord(aArray, offset+0, InvertedWordsOfDword[0], aSwapBytes);
  AddWord(aArray, offset+2, InvertedWordsOfDword[1], aSwapBytes);
end;

class procedure TPLCStruct.AddDWord(aArray:TArrayOfDouble; offset:Integer; aDWord:LongInt; aSwapBytes, aSwapWords:Boolean);
begin
  AddDWord(aArray,offset,DWord(aDWord),aSwapBytes,aSwapWords);
end;

class procedure TPLCStruct.AddDWord(aArray:TArrayOfDouble; offset:Integer; aDWord:Single; aSwapBytes, aSwapWords:Boolean);
var
  asDWord:DWord absolute aDWord;
begin
  AddDWord(aArray,offset,asDWord,aSwapBytes,aSwapWords);
end;

class procedure TPLCStruct.AddQWord(aArray:TArrayOfDouble; offset:Integer; aQWord:QWord; aSwapBytes, aSwapWords, aSwapDWords:Boolean);
var
  InvertedWordsOfDword:array[0..1] of DWord absolute aQWord;
  aux:DWord;
begin
  if aSwapDWords then begin
    aux:=InvertedWordsOfDword[0];
    InvertedWordsOfDword[0]:=InvertedWordsOfDword[1];
    InvertedWordsOfDword[1]:=aux;
  end;
  AddDWord(aArray,offset+0, InvertedWordsOfDword[0],aSwapBytes, aSwapWords);
  AddDWord(aArray,offset+4, InvertedWordsOfDword[1],aSwapBytes, aSwapWords);
end;

class procedure TPLCStruct.AddQWord(aArray:TArrayOfDouble; offset:Integer; aQWord:Int64; aSwapBytes, aSwapWords, aSwapDWords:Boolean);
begin
  AddQWord(aArray,offset,QWord(aQWord),aSwapBytes,aSwapWords,aSwapDWords);
end;

class procedure TPLCStruct.AddQWord(aArray:TArrayOfDouble; offset:Integer; aQWord:Double; aSwapBytes, aSwapWords, aSwapDWords:Boolean);
var
  asQWord:QWord absolute aQWord;
begin
  AddQWord(aArray,offset,asQWord,aSwapBytes,aSwapWords,aSwapDWords);
end;

class procedure TPLCStruct.AddCString(aArray:TArrayOfDouble; offset:Integer; aString:AnsiString);
var
  i: Integer;
begin
  for i:=1 to Length(aString) do
    addByte(aArray,offset+(i-1),byte(aString[i]));
  addByte(aArray,Length(aString),0);
end;

class procedure TPLCStruct.AddSiemensString(aArray:TArrayOfDouble; offset:Integer; aString:AnsiString; MaxSize:Byte);
var
  aSize:Byte;
  i: Integer;
begin
  aSize:=Min(MaxSize,Length(aString));

  addByte(aArray,offset+0,MaxSize);
  addByte(aArray,offset+1,aSize);

  for i:=1 to aSize do
    addByte(aArray,offset+(i+1),byte(aString[i]));
end;

function TPLCStruct.GetByte(Offset: Integer): Byte;
begin
  if ((Offset<0) or (Offset>High(PValues))) then
    raise Exception.Create(SoutOfBounds);
  Result:=trunc(PValues[Offset]);
end;

function TPLCStruct.GetWord(Offset: Integer; aSwapBytes: Boolean): Word;
var
  aResult:Word;
  aBytes:array[0..1] of byte absolute aResult;
begin
  if ((Offset<0) or ((Offset+1)>High(PValues))) then
    raise Exception.Create(SoutOfBounds);

  if aSwapBytes then begin
    aBytes[0]:=GetByte(Offset+1);
    aBytes[1]:=GetByte(Offset+0);
  end else begin
    aBytes[0]:=GetByte(Offset+0);
    aBytes[1]:=GetByte(Offset+1);
  end;

  Result:=aResult;
end;

function TPLCStruct.GetSmallInt(Offset: Integer; aSwapBytes: Boolean): SmallInt;
begin
  Result:=SmallInt(GetWord(offset,aSwapBytes));
end;

function TPLCStruct.GetLongWord(Offset: Integer; aSwapBytes, aSwapWords: Boolean
  ): LongWord;
var
  aResult:LongWord;
  aWords:array[0..1] of Word absolute aResult;
begin
  if ((Offset<0) or ((Offset+3)>High(PValues))) then
    raise Exception.Create(SoutOfBounds);

  if aSwapWords then begin
    aWords[0]:=GetWord(Offset+2,aSwapBytes);
    aWords[1]:=GetWord(Offset+0,aSwapBytes);
  end else begin
    aWords[0]:=GetWord(Offset+0,aSwapBytes);
    aWords[1]:=GetWord(Offset+2,aSwapBytes);
  end;

  Result:=aResult;
end;

function TPLCStruct.GetLongInt(Offset: Integer; aSwapBytes, aSwapWords: Boolean
  ): LongInt;
begin
  Result:=LongInt(GetLongWord(Offset,aSwapBytes,aSwapWords));
end;

function TPLCStruct.GetSingle(Offset: Integer; aSwapBytes, aSwapWords: Boolean
  ): Single;
var
  aResult:Single;
  aResDWord:LongWord absolute aResult;
begin
  aResDWord:=GetLongWord(Offset,aSwapBytes,aSwapWords);
  Result:=aResult;
end;

function TPLCStruct.GetQWord(Offset: Integer; aSwapBytes, aSwapWords,
  aSwapDWords: Boolean): QWord;
var
  aResult:QWord;
  aDWords:array[0..1] of LongWord absolute aResult;
begin
  if ((Offset<0) or ((Offset+7)>High(PValues))) then
    raise Exception.Create(SoutOfBounds);

  if aSwapDWords then begin
    aDWords[0]:=GetLongWord(Offset+4,aSwapBytes,SwapWords);
    aDWords[1]:=GetLongWord(Offset+0,aSwapBytes,SwapWords);
  end else begin
    aDWords[0]:=GetLongWord(Offset+0,aSwapBytes,SwapWords);
    aDWords[1]:=GetLongWord(Offset+4,aSwapBytes,SwapWords);
  end;

  Result:=aResult;
end;

function TPLCStruct.GetInt64(Offset: Integer; aSwapBytes, aSwapWords,
  aSwapDWords: Boolean): Int64;
begin
  Result:=Int64(GetQWord(Offset,aSwapBytes,aSwapWords,aSwapDWords));
end;

function TPLCStruct.GetDouble(Offset: Integer; aSwapBytes, aSwapWords,
  aSwapDWords: Boolean): Double;
var
  aResult:Double;
  aResQWord:QWord absolute aResult;
begin
  aResQWord:=GetQWord(Offset,aSwapBytes,aSwapWords,aSwapDWords);
  Result:=aResQWord;
end;

function TPLCStruct.GetSiemensString(Offset: Integer; MaxStringSize: Integer
  ): String;
var
  maxSize, curSize, b: Byte;
  i: Integer;
  limit: integer;
begin
  maxSize:=GetByte(Offset);
  curSize:=GetByte(Offset+1);

  Result:='';
  limit:=min(min(min(curSize,maxSize),MaxStringSize),Size-Offset);
  for i:=0 to limit-1 do begin
    b:=GetByte(Offset+2+i);
    if b=0 then break;
    Result:=Result+chr(GetByte(Offset+2+i));
  end;
end;

var
  StructItemMapperEditor:TOpenTagEditor = nil;

procedure TPLCStruct.MapElements(InsertHook: TAddTagInEditorHook;
  CreateProc: TCreateTagProc);
begin
    if Assigned(StructItemMapperEditor) then
    StructItemMapperEditor(Self, Self.Owner,InsertHook,CreateProc)
  else
    raise exception.Create('None element mapper tool has been assigned!');
end;

procedure SetStructItemMapper(StructItemMapperTool:TOpenTagEditor);
begin
  if assigned(StructItemMapperEditor) then
    raise Exception.Create('A Bit Mapper editor was already assigned.')
  else
    StructItemMapperEditor:=StructItemMapperTool;
end;


end.
