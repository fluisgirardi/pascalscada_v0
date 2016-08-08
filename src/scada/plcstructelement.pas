{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementação de tag item de uma estrutura de comunicação.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Unit that implements a tag item of an structure communication.)
}
{$ENDIF}
unit PLCStructElement;

interface

uses
  Classes, sysutils, Tag, PLCTag, PLCBlockElement, ProtocolTypes, PLCStruct;

type
  {$IFDEF PORTUGUES}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    @abstract(Classe de tag item de uma estrutura de comunicação.)
  }
  {$ELSE}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    @abstract(Class of a tag item of a structure communication tag.)
  }
  {$ENDIF}

  { TPLCStructItem }

  TPLCStructItem = class(TPLCBlockElement, ITagInterface, ITagNumeric)
  private
    PBlock:TPLCStruct;
    function GetMySize(newType: TTagType): Integer;
  protected
    procedure SetBlock(blk:TPLCStruct);
    //: @seealso(TPLCTag.GetValueRaw)
    function GetValueRaw: Double; override;
    //: @seealso(TPLCNumber.SetValueRaw)
    procedure SetValueRaw(aValue: Double); override;
    //: @seealso(TPLCBlockElement.SetIndex)
    procedure SetIndex(i: Cardinal); override;
    //: @seealso(TPLCTag.SetTagType)
    procedure SetTagType(newType: TTagType); override;

    //IHMITagInterface
    procedure TagChangeCallback(Sender:TObject); override;
    procedure RemoveTagCallBack(Sender:TObject); override;
    //: @exclude
    procedure Loaded; override;
    procedure UpdateTagSizeOnProtocol; override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
  published
    //: @seealso(TPLCTag.TagType);
    property TagType;
    //: @seealso(TPLCTag.SwapBytes);
    property SwapBytes;
    //: @seealso(TPLCTag.SwapWords);
    property SwapWords;
    //: @seealso(TPLCTag.SwapDWords)
    property SwapDWords;

    {$IFDEF PORTUGUES}
    //: Tag estrutura a que o item pertence.
    {$ELSE}
    //: Structure tag which the item belongs.
    {$ENDIF}
    property PLCBlock:TPLCStruct read PBlock write SetBlock;
  end;

implementation

uses ProtocolDriver, hsstrings, math;

constructor TPLCStructItem.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FProtocolTagType:=ptByte;
  FProtocolWordSize:=8;
end;

destructor TPLCStructItem.Destroy;
begin
  if Assigned(PBlock) then
    PBlock.RemoveAllHandlersFromObject(Self);
  PBlock:=nil;
  inherited Destroy;
end;

procedure TPLCStructItem.TagChangeCallback(Sender:TObject);
begin
  GetValueRaw();
end;

procedure TPLCStructItem.RemoveTagCallBack(Sender:TObject);
begin
  if PBlock=sender then
    PBlock := nil;
end;

procedure TPLCStructItem.Loaded;
var
  oldwordsize:Byte;
begin
  oldwordsize:=FCurrentWordSize;
  inherited Loaded;
  FCurrentWordSize:=oldwordsize;

  UpdateTagSizeOnProtocol;
end;

procedure TPLCStructItem.UpdateTagSizeOnProtocol;
var
  oldprotocol: TProtocolDriver;
begin
  oldprotocol:=PProtocolDriver;
  PProtocolDriver:=TProtocolDriver(1);
  inherited UpdateTagSizeOnProtocol;
  PProtocolDriver:=oldprotocol;
end;

procedure TPLCStructItem.SetBlock(blk:TPLCStruct);
begin
  if blk=PLCBlock then exit;
  //se esta setando o bloco
  //if the block is being set
  if (blk<>nil) then begin
    if (PIndex+GetMySize(FTagType))>blk.Size then
      raise Exception.Create(STagIdxMoreSizeExceedStructLen);

    blk.AddRemoveTagHandler(@RemoveTagCallBack);
    blk.AddTagChangeHandler(@TagChangeCallback);
    blk.AddWriteFaultHandler(@WriteFaultCallback);
  end;

  //esta removendo do bloco.
  //removing the link with the block
  if Assigned(PBlock) then begin
    PBlock.RemoveAllHandlersFromObject(Self);
  end;


  PBlock:=blk;
end;

function  TPLCStructItem.GetValueRaw: Double;
var
  notify:Boolean;
  data, converted_value:TArrayOfDouble;
begin
  Result:=0;
  if Assigned(PBlock) then begin
    if FCurrentWordSize>=8 then begin
      SetLength(data,1);
      data[0]:=PBlock.ValueRaw[PIndex];
    end;

    if FCurrentWordSize>=16 then begin
      SetLength(data,2);
      data[1]:=PBlock.ValueRaw[PIndex+1];
    end;

    if FCurrentWordSize>=32 then begin
      SetLength(data,4);
      data[2]:=PBlock.ValueRaw[PIndex+2];
      data[3]:=PBlock.ValueRaw[PIndex+3];
    end;

    if FCurrentWordSize>=64 then begin
      SetLength(data,8);
      data[4]:=PBlock.ValueRaw[PIndex+4];
      data[5]:=PBlock.ValueRaw[PIndex+5];
      data[6]:=PBlock.ValueRaw[PIndex+6];
      data[7]:=PBlock.ValueRaw[PIndex+7];
    end;

    converted_value := PLCValuesToTagValues(data,0);

    if Length(converted_value)<=0 then exit;

    notify := (IsNan(converted_value[0]) and (not IsNan(PValueRaw))) or
              ((not IsNan(converted_value[0])) and IsNan(PValueRaw)) or (PValueRaw<>converted_value[0]) ;
    PValueRaw := converted_value[0];
    PValueTimeStamp := PBlock.ValueTimestamp;

    Result:=PValueRaw;

    if notify or PFirstUpdate then begin
      PFirstUpdate:=false;
      NotifyChange();
    end;

    SetLength(data,0);
    SetLength(converted_value,0);
  end;
end;

procedure TPLCStructItem.SetValueRaw(aValue:Double);
var
  blkvalues, values:TArrayOfDouble;
begin
  if Assigned(PBlock) then begin
    SetLength(values,1);
    values[0]:=aValue;
    blkvalues := TagValuesToPLCValues(values,0);
    if PBlock.SyncWrites then
      PBlock.Write(blkvalues,Length(blkvalues),PIndex)
    else
      PBlock.ScanWrite(blkvalues,Length(blkvalues),PIndex);
    SetLength(blkvalues,0);
    SetLength(values,0);
  end else
    if PValueRaw<>Value then begin
      PValueRaw:=Value;
      NotifyChange;
    end;
end;

procedure TPLCStructItem.SetIndex(i: Cardinal);
var
  MySize:LongInt;
begin
  MySize:=FCurrentWordSize div 8;
  if PBlock<>nil then
    if (i+MySize)>PBlock.Size then
      raise Exception.Create(SItemOutOfStructure);

  inherited SetIndex(i);
end;

function TPLCStructItem.GetMySize(newType:TTagType):Integer;
begin
  case newType of
    pttDefault:
      if FProtocolWordSize=1 then
        Result:=1
      else
        Result := FProtocolWordSize div 8;
    pttByte, pttShortInt:
      Result:=1;
    pttSmallInt, pttWord:
      Result:=2;
    pttLongInt, pttDWord, pttFloat:
      Result:=4;
    pttInt64, pttQWord, pttDouble:
      Result:=8;
  end;
end;

procedure TPLCStructItem.SetTagType(newType:TTagType);
var
  MySize: Integer;
begin
  MySize:=GetMySize(newType);

  if PBlock<>nil then
    if (PIndex+MySize)>PBlock.Size then
      raise Exception.Create(SItemOutOfStructure);

  inherited SetTagType(newType);
end;

end.
