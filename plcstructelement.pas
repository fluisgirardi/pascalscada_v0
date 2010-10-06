unit PLCStructElement;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, sysutils, Tag, PLCTag, PLCBlockElement, ProtocolTypes, PLCStruct;

type
  TPLCStructItem = class(TPLCBlockElement, ITagInterface, ITagNumeric)
  private
    PBlock:TPLCStruct;
  protected
    procedure SetBlock(blk:TPLCStruct);
    procedure SetValueRaw(Value: Double); override;
    procedure SetIndex(i: Cardinal); override;
    procedure SetTagType(newType: TTagType); override;

    //IHMITagInterface
    procedure NotifyTagChange(Sender:TObject); override;
    procedure RemoveTag(Sender:TObject); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    //: @seealso(TPLCTag.TagType);
    property TagType;
    //: @seealso(TPLCTag.SwapBytes);
    property SwapBytes;
    //: @seealso(TPLCTag.SwapWords);
    property SwapWords;

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
var
  notify:Boolean;
  data, value:TArrayOfDouble;
begin
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

    value := PLCValuesToTagValues(data,0);

    if Length(value)<0 then exit;

    notify := (PValueRaw<>value[0]) or (IsNan(value[0]) and (not IsNan(PValueRaw)));
    PValueRaw := value[0];
    PValueTimeStamp := PBlock.ValueTimestamp;

    if notify then
      NotifyChange();

    SetLength(data,0);
  end;
end;

procedure TPLCStructItem.RemoveTag(Sender:TObject);
begin
  if PBlock=sender then
    PBlock := nil;
end;

procedure TPLCStructItem.Loaded;
var
  oldprot:TProtocolDriver;
begin
  oldprot:=PProtocolDriver;
  PProtocolDriver:=TProtocolDriver(1);
  inherited Loaded;
  PProtocolDriver:=oldprot;
end;

procedure TPLCStructItem.SetBlock(blk:TPLCStruct);
begin
  //esta removendo do bloco.
  if (blk=nil) and (Assigned(PBlock)) then begin
    PBlock.RemoveCallBacks(Self as IHMITagInterface);
    PBlock := nil;
    exit;
  end;

  //se esta setando o bloco
  if (blk<>nil) and (PBlock=nil) then begin
    PBlock := blk;
    PBlock.AddCallBacks(Self as IHMITagInterface);
    exit;
  end;

  //se esta setado o bloco, mas esta trocando
  if blk<>PBlock then begin
    PBlock.RemoveCallBacks(Self as IHMITagInterface);
    PBlock := blk;
    PBlock.AddCallBacks(Self as IHMITagInterface);
    if PIndex>=PBlock.Size then
      PIndex := 0;
  end;
end;

procedure TPLCStructItem.SetValueRaw(Value:Double);
var
  blkvalues, values:TArrayOfDouble;
  c:Integer;
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
  MySize:Integer;
begin
  MySize:=FCurrentWordSize div 8;
  if PBlock<>nil then
    if (i+MySize)>PBlock.Size then
      raise Exception.Create(SItemOutOfStructure);

  inherited SetIndex(i);
end;

procedure TPLCStructItem.SetTagType(newType:TTagType);
var
  MySize:Integer;
begin
  case newType of
    pttDefault:
      if FProtocolWordSize=1 then
        MySize:=1
      else
        MySize := FProtocolWordSize div 8;
    pttByte:
      MySize:=1;
    pttShortInt, pttWord:
      MySize:=2;
    pttInteger, pttDWord, pttFloat:
      MySize:=4;
  end;

  if PBlock<>nil then
    if (PIndex+MySize)>PBlock.Size then
      raise Exception.Create(SItemOutOfStructure);

  inherited SetTagType(newType);
end;

end.
