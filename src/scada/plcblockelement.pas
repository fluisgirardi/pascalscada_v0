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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

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
  TPLCBlockElement = class(TPLCNumberMappable, ITagInterface, ITagNumeric, IHMITagInterface)
  private
    PBlock:TPLCBlock;
  protected
    PIndex:Cardinal;
    procedure SetBlock(blk:TPLCBlock);
    procedure SetIndex(i:Cardinal); virtual;

    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(Value:Variant):Boolean;
    function  GetValueTimestamp:TDatetime;

    //implements the IHMITagInterface
    procedure NotifyReadOk; virtual;
    procedure NotifyReadFault; virtual;
    procedure NotifyWriteOk; virtual;
    procedure NotifyWriteFault; virtual;
    procedure NotifyTagChange(Sender:TObject); virtual;
    procedure RemoveTag(Sender:TObject); virtual;
  protected
    //: @seealso(TPLCNumber.GetValueRaw)
    function  GetValueRaw:Double; override;
    //: @seealso(TPLCNumber.SetValueRaw)
    procedure SetValueRaw(Value:Double); override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    //: @seealso(TPLCTag.ScanRead)
    procedure ScanRead; override;
    //: @seealso(TPLCTag.ScanWrite)
    procedure ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal); override;
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
     PBlock.RemoveCallBacks(Self as IHMITagInterface);
  PBlock:=nil;
  inherited Destroy;
end;

procedure TPLCBlockElement.SetBlock(blk:TPLCBlock);
begin
  //esta removendo do bloco.
  //removing the link with the block
  if (blk=nil) and (Assigned(PBlock)) then begin
    PBlock.RemoveCallBacks(Self as IHMITagInterface);
    PBlock := nil;
    exit;
  end;

  //se esta setando o bloco
  //if the block is being set
  if (blk<>nil) and (PBlock=nil) then begin
    PBlock := blk;
    PBlock.AddCallBacks(Self as IHMITagInterface);
    exit;
  end;

  //se esta setado o bloco, mas esta trocando
  //if the block is being replaced.
  if blk<>PBlock then begin
    PBlock.RemoveCallBacks(Self as IHMITagInterface);
    PBlock := blk;
    PBlock.AddCallBacks(Self as IHMITagInterface);
    if PIndex>=PBlock.Size then
      PIndex := PBlock.Size - 1;
  end;
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

function  TPLCBlockElement.IsValidValue(Value:Variant):Boolean;
var
   aux:Double;
begin
   Result := VarIsNumeric(Value) or
             (VarIsStr(Value) and TryStrToFloat(Value,aux)) or
             VarIsType(Value, varboolean);
end;

function TPLCBlockElement.GetValueTimestamp:TDatetime;
begin
   Result := PValueTimeStamp;
end;

procedure TPLCBlockElement.SetValueRaw(Value:Double);
begin
  if Assigned(PBlock) then
    PBlock.ValueRaw[PIndex] := Value
  else
    if PValueRaw<>Value then begin
      PValueRaw:=Value;
      NotifyChange;
    end;
end;

procedure TPLCBlockElement.ScanRead;
begin
  if Assigned(PBlock) then
    PBlock.ScanRead;
end;

procedure TPLCBlockElement.ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal);
begin
  if Assigned(PBlock) then
    PBlock.ScanWrite(values, 1, PIndex)
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

procedure TPLCBlockElement.NotifyReadOk;
begin

end;

procedure TPLCBlockElement.NotifyReadFault;
begin

end;

procedure TPLCBlockElement.NotifyWriteOk;
begin

end;

procedure TPLCBlockElement.NotifyWriteFault;
begin
  NotifyTagChange(Self);
end;

procedure TPLCBlockElement.NotifyTagChange(Sender:TObject);
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

procedure TPLCBlockElement.RemoveTag(Sender:TObject);
begin
  if PBlock=sender then
    PBlock := nil;
end;

end.
