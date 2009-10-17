//: Implementação de um elemento de bloco.
unit PLCBlockElement;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, PLCNumber, PLCBlock, ProtocolTypes, variants, Tag;

type
  {:
  Classe de Tag Elemento de Bloco de comunicação.
  @seealso(TPLCBlock)
  }
  TPLCBlockElement = class(TPLCNumber, ITagInterface, ITagNumeric, IHMITagInterface)
  private
    PBlock:TPLCBlock;
    PIndex:Cardinal;
    procedure SetBlock(blk:TPLCBlock);
    procedure SetIndex(i:Cardinal);

    function  GetValueAsText(Prefix, Sufix, Format:string):String;
    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(Value:Variant):Boolean;
    function  GetValueTimestamp:TDatetime;

    //IHMITagInterface
    procedure NotifyReadOk;
    procedure NotifyReadFault;
    procedure NotifyWriteOk;
    procedure NotifyWriteFault;
    procedure NotifyTagChange(Sender:TObject);
    procedure RemoveTag(Sender:TObject);
  protected
    //: @seealso(TPLCNumber.GetValueRaw)
    function  GetValueRaw:Double; override;
    //: @seealso(TPLCNumber.SetValueRaw)
    procedure SetValueRaw(Value:Double); override;
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); override;
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
    //: Bloco de comunicações que o elemento pertence.
    property PLCBlock:TPLCBlock read PBlock write SetBlock;
    //: Offset da elemento de memória dentro do bloco (indice).
    property Index:Cardinal read PIndex write SetIndex;
  end;

implementation

constructor TPLCBlockElement.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  ProtocolDriver := nil;
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
    raise Exception.Create('Fora dos limites!');
  PIndex := i;
end;

function TPLCBlockElement.GetValueRaw:Double;
begin
  if Assigned(PBlock) then
     Result := PBlock.ValueRaw[PIndex]
  else
     Result := PValueRaw ;
end;

function TPLCBlockElement.GetValueAsText(Prefix, Sufix, Format:string):String;
begin
   if Trim(Format)<>'' then
      Result := Prefix + FormatFloat(Format,Value)+Sufix
   else
      Result := Prefix + FloatToStr(Value)+Sufix;
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
            raise exception.Create('Valor inválido!');
      end else
         if VarIsType(V,varboolean) then begin
            if V=true then
               Value := 1
            else
               Value := 0;
         end else
            raise exception.Create('Valor inválido!');
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

procedure TPLCBlockElement.TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer);
begin
  //do nothing...
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
    notify := (PValueRaw<>PBlock.ValueRaw[PIndex]);
    PValueRaw := PBlock.ValueRaw[PIndex];
    PValueTimeStamp := PBlock.ValueTimestamp;

    if notify then
      NotifyChange();
  end;
end;

procedure TPLCBlockElement.RemoveTag(Sender:TObject);
begin
  if PBlock=sender then
    PBlock := nil;
end;

end.
