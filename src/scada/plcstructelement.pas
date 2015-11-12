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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

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
  TPLCStructItem = class(TPLCBlockElement, ITagInterface, ITagNumeric)
  private
    PBlock:TPLCStruct;
  protected
    procedure SetBlock(blk:TPLCStruct);
    //: @seealso(TPLCTag.GetValueRaw)
    function GetValueRaw: Double; override;
    //: @seealso(TPLCNumber.SetValueRaw)
    procedure SetValueRaw(Value: Double); override;
    //: @seealso(TPLCBlockElement.SetIndex)
    procedure SetIndex(i: Cardinal); override;
    //: @seealso(TPLCTag.SetTagType)
    procedure SetTagType(newType: TTagType); override;

    //IHMITagInterface
    procedure NotifyTagChange(Sender:TObject); override;
    procedure RemoveTag(Sender:TObject); override;
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
    PBlock.RemoveCallBacks(Self as IHMITagInterface);
  PBlock:=nil;
  inherited Destroy;
end;

procedure TPLCStructItem.NotifyTagChange(Sender:TObject);
begin
  GetValueRaw();
end;

procedure TPLCStructItem.RemoveTag(Sender:TObject);
begin
  if PBlock=sender then
    PBlock := nil;
end;

procedure TPLCStructItem.Loaded;
var
  oldwordsize:Byte;
  oldprotocol:TProtocolDriver;
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
  //esta removendo do bloco.
  //if the structure is being removed.
  if (blk=nil) and (Assigned(PBlock)) then begin
    PBlock.RemoveCallBacks(Self as IHMITagInterface);
    PBlock := nil;
    exit;
  end;

  //se esta setando o bloco
  //if the structure is being set.
  if (blk<>nil) and (PBlock=nil) then begin
    PBlock := blk;
    PBlock.AddCallBacks(Self as IHMITagInterface);
    exit;
  end;

  //se esta setado o bloco, mas esta trocando
  //if the structure is being replaced.
  if blk<>PBlock then begin
    PBlock.RemoveCallBacks(Self as IHMITagInterface);
    PBlock := blk;
    PBlock.AddCallBacks(Self as IHMITagInterface);
    if PIndex>=PBlock.Size then
      PIndex := 0;
  end;
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

procedure TPLCStructItem.SetValueRaw(Value:Double);
var
  blkvalues, values:TArrayOfDouble;
begin
  if Assigned(PBlock) then begin
    SetLength(values,1);
    values[0]:=Value;
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

procedure TPLCStructItem.SetTagType(newType:TTagType);
var
  MySize:LongInt;
begin
  case newType of
    pttDefault:
      if FProtocolWordSize=1 then
        MySize:=1
      else
        MySize := FProtocolWordSize div 8;
    pttByte, pttShortInt:
      MySize:=1;
    pttSmallInt, pttWord:
      MySize:=2;
    pttLongInt, pttDWord, pttFloat:
      MySize:=4;
    pttInt64, pttQWord, pttDouble:
      MySize:=8;
  end;

  if PBlock<>nil then
    if (PIndex+MySize)>PBlock.Size then
      raise Exception.Create(SItemOutOfStructure);

  inherited SetTagType(newType);
end;

end.
