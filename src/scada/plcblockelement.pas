{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementação de um elemento de um tag bloco de comunicação.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit that implements a block element tag.)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - TPLCBlockElement is descendant of TPLCNumberMappable
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************

}
{$ENDIF}
unit PLCBlockElement;

interface

uses
  SysUtils, Classes, PLCNumber, PLCBlock, ProtocolTypes, variants, Tag;

type
  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Classe de Tag Elemento de Bloco de comunicação.
  Usado para retirar uma informação de um conjunto (bloco) de informações.

  @seealso(TPLCBlock)
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Class of Block element tag.
  Used to get a single value from a set of values (block).

  @seealso(TPLCBlock)
  }
  {$ENDIF}
  TPLCBlockElement = class(TPLCNumberMappable, ITagInterface, ITagNumeric)
  private
    PBlock:TPLCBlock;
  protected
    PIndex:Cardinal;

    function GetLastAsyncReadStatus: TProtocolIOResult;  override;
    function GetLastAsyncWriteStatus: TProtocolIOResult; override;
    function GetLastSyncReadStatus: TProtocolIOResult;   override;
    function GetLastSyncWriteStatus: TProtocolIOResult;  override;

    procedure SetBlock(blk:TPLCBlock);
    procedure SetIndex(i:Cardinal); virtual;

    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(aValue:Variant):Boolean;
    function  GetValueTimestamp:TDatetime;

    procedure WriteFaultCallback(Sender:TObject); virtual;
    procedure TagChangeCallback(Sender:TObject); virtual;
    procedure RemoveTagCallBack(Sender:TObject); virtual;
  protected
    //: @seealso(TPLCNumber.GetValueRaw)
    function  GetValueRaw:Double; override;
    //: @seealso(TPLCNumber.SetValueRaw)
    procedure SetValueRaw(aValue:Double); override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    //: @seealso(TPLCTag.ScanRead)
    function ScanRead:Int64; override;
    //: @seealso(TPLCTag.ScanWrite)
    function ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal; const IgnoreAutoWrite:Boolean = false):Int64; override;
    //: @seealso(TPLCTag.Read)
    procedure Read; override;
    //: @seealso(TPLCTag.Write)
    procedure Write(Values:TArrayOfDouble; Count, Offset:Cardinal); override;
  published

    {$IFDEF PORTUGUES}
    //: Bloco de comunicações que o elemento pertence.
    {$ELSE}
    //: Communication Block of the element.
    {$ENDIF}
    property PLCBlock:TPLCBlock read PBlock write SetBlock;

    {$IFDEF PORTUGUES}
    //: Offset da elemento de memória dentro do bloco (indice).
    {$ELSE}
    //: Index of tag element on the Tag Block.
    {$ENDIF}
    property Index:Cardinal read PIndex write SetIndex;
    //: @seealso(TPLCNumber.EnableMaxValue)
    property EnableMaxValue;
    //: @seealso(TPLCNumber.EnableMinValue)
    property EnableMinValue;
    //: @seealso(TPLCNumber.MaxValue)
    property MaxValue;
    //: @seealso(TPLCNumber.MinValue)
    property MinValue;

    property LastASyncReadStatus;
    property LastASyncWriteStatus;
    property LastSyncReadStatus;
    property LastSyncWriteStatus;
  end;

implementation

uses hsstrings, math;

constructor TPLCBlockElement.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PBlock := nil;
  AutoRead := false;
  AutoWrite := false;
  PIndex := 0;
end;

destructor  TPLCBlockElement.Destroy;
begin
  if Assigned(PBlock) then
     PBlock.RemoveAllHandlersFromObject(Self);
  PBlock:=nil;
  inherited Destroy;
end;

function TPLCBlockElement.GetLastAsyncReadStatus: TProtocolIOResult;
begin
  if Assigned(PBlock) then
    Result:=PBlock.LastASyncReadStatus
  else
    Result:=ioNullTagBlock
end;

function TPLCBlockElement.GetLastAsyncWriteStatus: TProtocolIOResult;
begin
  if Assigned(PBlock) then
    Result:=PBlock.LastASyncWriteStatus
  else
    Result:=ioNullTagBlock
end;

function TPLCBlockElement.GetLastSyncReadStatus: TProtocolIOResult;
begin
  if Assigned(PBlock) then
    Result:=PBlock.LastSyncReadStatus
  else
    Result:=ioNullTagBlock;
end;

function TPLCBlockElement.GetLastSyncWriteStatus: TProtocolIOResult;
begin
  if Assigned(PBlock) then
    Result:=PBlock.LastSyncWriteStatus
  else
    Result:=ioNullTagBlock;
end;

procedure TPLCBlockElement.SetBlock(blk:TPLCBlock);
begin
  if blk=PLCBlock then exit;
  //esta removendo do bloco.
  //removing the link with the block
  if Assigned(PBlock) then begin
    PBlock.RemoveAllHandlersFromObject(Self);
  end;

  //se esta setando o bloco
  //if the block is being set
  if (blk<>nil) then begin
    blk.AddRemoveTagHandler(@RemoveTagCallBack);
    blk.AddTagChangeHandler(@TagChangeCallback);
    blk.AddWriteFaultHandler(@WriteFaultCallback);
    if PIndex>=blk.Size then
      PIndex := blk.Size - 1;
  end;
  PBlock:=blk;
end;

procedure TPLCBlockElement.SetIndex(i:Cardinal);
begin
  if PBlock=nil then begin
    PIndex := i;
    exit;
  end;

  if i>=PBlock.Size then
    raise Exception.Create(SoutOfBounds);
  PIndex := i;
end;

function TPLCBlockElement.GetValueRaw:Double;
begin
  if Assigned(PBlock) then
     Result := PBlock.ValueRaw[PIndex]
  else
     Result := PValueRaw ;
end;

function  TPLCBlockElement.GetVariantValue:Variant;
begin
   Result := Value;
end;

procedure TPLCBlockElement.SetVariantValue(V:Variant);
var
   aux:double;
begin
   if VarIsNumeric(v) then begin
      Value := V
   end else
      if VarIsStr(V) then begin
         if TryStrToFloat(V,aux) then
            Value := aux
         else
            raise exception.Create(SinvalidValue);
      end else
         if VarIsType(V,varboolean) then begin
            if V=true then
               Value := 1
            else
               Value := 0;
         end else
            raise exception.Create(SinvalidValue);
end;

function  TPLCBlockElement.IsValidValue(aValue:Variant):Boolean;
var
   aux:Double;
   aValueStr: AnsiString;
begin
   aValueStr:=aValue;
   Result := VarIsNumeric(aValue) or
             (VarIsStr(aValue) and TryStrToFloat(aValueStr,aux)) or
             VarIsType(aValue, varboolean);
end;

function TPLCBlockElement.GetValueTimestamp:TDatetime;
begin
   Result := PValueTimeStamp;
end;

procedure TPLCBlockElement.SetValueRaw(aValue:Double);
begin
  if Assigned(PBlock) then
    PBlock.ValueRaw[PIndex] := aValue
  else
    if PValueRaw<>Value then begin
      PValueRaw:=Value;
      NotifyChange;
    end;
end;

function TPLCBlockElement.ScanRead: Int64;
begin
  if Assigned(PBlock) then
    Result:=PBlock.ScanRead;
end;

function TPLCBlockElement.ScanWrite(Values: TArrayOfDouble; Count,
  Offset: Cardinal; const IgnoreAutoWrite: Boolean): Int64;
begin
  if Assigned(PBlock) then
    Result := PBlock.ScanWrite(values, 1, PIndex, IgnoreAutoWrite)
  else
    Result:=-1;
end;

procedure TPLCBlockElement.Read;
begin
  if Assigned(PBlock) then begin
    PBlock.Read;
  end;
end;

procedure TPLCBlockElement.Write(Values:TArrayOfDouble; Count, Offset:Cardinal);
begin
  if Assigned(PBlock) then
    PBlock.Write(values, 1, PIndex)
end;

procedure TPLCBlockElement.WriteFaultCallback(Sender: TObject);
var
  notify:Boolean;
begin
  if Assigned(PBlock) then begin
    notify := (PValueRaw<>PBlock.ValueRaw[PIndex]) or (IsNan(PBlock.ValueRaw[PIndex]) and (not IsNan(PValueRaw)));
    PValueRaw := PBlock.ValueRaw[PIndex];
    PValueTimeStamp := PBlock.ValueTimestamp;

    if notify or PFirstUpdate then begin
      PFirstUpdate:=false;
      NotifyWriteFault();
    end;
  end;
end;

procedure TPLCBlockElement.TagChangeCallback(Sender:TObject);
var
  notify:Boolean;
begin
  if Assigned(PBlock) then begin
    notify := (PValueRaw<>PBlock.ValueRaw[PIndex]) or (IsNan(PBlock.ValueRaw[PIndex]) and (not IsNan(PValueRaw)));
    PValueRaw := PBlock.ValueRaw[PIndex];
    PValueTimeStamp := PBlock.ValueTimestamp;

    if notify or PFirstUpdate then begin
      PFirstUpdate:=false;
      NotifyChange();
    end;
  end;
end;

procedure TPLCBlockElement.RemoveTagCallBack(Sender: TObject);
begin
  if PBlock=sender then
    PBlock := nil;
end;

end.
