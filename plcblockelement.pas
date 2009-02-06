//: Implementação de um elemento de bloco.
unit PLCBlockElement;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, PLCNumber, PLCBlock, ProtocolTypes, variants;

type
  {:
  Classe de Tag Elemento de Bloco de comunicação.
  @seealso(TPLCBlock)
  }
  TPLCBlockElement = class(TPLCNumber, ITagInterface, ITagNumeric)
  private
    PBlock:TPLCBlock;
    PIndex:Cardinal;
    procedure SetBlock(blk:TPLCBlock);
    procedure SetIndex(i:Cardinal);
    procedure RemoveTag(Sender:TObject);
    
    function  GetValueRaw:Double;
    function  GetValueAsText(Prefix, Sufix, Format:string):String;
    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(Value:Variant):Boolean;
    function  GetValueTimestamp:TDatetime;
  protected
    //: Método chamado pelo bloco para informar ao elemento de alterações de valores.
    procedure ChangeCallback(Sender:TObject);
    //: @seealso(TPLCNumber.GetValue)
    function  GetValue:Double; override;
    //: @seealso(TPLCNumber.GetValueDirect)
    function  GetValueDirect:Double; override;
    //: @seealso(TPLCNumber.SetValueRaw)
    procedure SetValueRaw(Value:Double); override;
    //: @seealso(TPLCNumber.SetValueDirectRaw)
    procedure SetValueDirectRaw(Value:Double); override;
    //: @seealso(TPLCTag.ScanRead)
    procedure ScanRead; override;
    //: @seealso(TPLCTag.ScanWrite)
    procedure ScanWrite(Values:TArrayOfDouble; Count, Offset:DWORD); override;
    //: @seealso(TPLCTag.Read)
    procedure Read; override;
    //: @seealso(TPLCTag.Write)
    procedure Write(Values:TArrayOfDouble; Count, Offset:DWORD); override;
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
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
  if PBlock<>nil then;
     PLCBlock.RemoveChangeCallBack(ChangeCallback);
  PBlock:=nil;
  inherited Destroy;
end;

procedure TPLCBlockElement.SetBlock(blk:TPLCBlock);
begin
  //esta removendo do bloco.
  if (blk=nil) and (PBlock<>nil) then begin
    PBlock.RemoveChangeCallBack(ChangeCallback);
    PBlock := nil;
    exit;
  end;

  //se esta setando o bloco
  if (blk<>nil) and (PBlock=nil) then begin
    PBlock := blk;
    PBlock.AddChangeCallBack(ChangeCallback,RemoveTag);
    exit;
  end;

  //se esta setado o bloco, mas esta trocando
  if blk<>PBlock then begin
    PBlock.RemoveChangeCallBack(ChangeCallback);
    PBlock := blk;
    PBlock.AddChangeCallBack(ChangeCallback, RemoveTag);
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

procedure TPLCBlockElement.RemoveTag(Sender:TObject);
begin
  if PBlock=sender then 
    PBlock := nil;
end;

function TPLCBlockElement.GetValueRaw:Double;
begin
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

procedure TPLCBlockElement.ChangeCallback(Sender:TObject);
var
  notify:Boolean;
begin
  if PBlock<>nil then begin
    notify := (PValueRaw<>PBlock.ValueRaw[PIndex]);
    PValueRaw := PBlock.ValueRaw[PIndex];
    PValueDirectRaw := PValueRaw;
    PValueTimeStamp := PBlock.ValueTimestamp;

    if notify then
      NotifyChange();
  end;
end;

function  TPLCBlockElement.GetValue:Double;
begin
  if PScaleProcessor=nil then
    Result := PValueRaw
  else
    Result := PScaleProcessor.SetInGetOut(self, PValueRaw);
end;

function  TPLCBlockElement.GetValueDirect:Double;
begin
  if Assigned(PScaleProcessor) then
    Result := PScaleProcessor.SetInGetOut(self, GetValueDirectRaw)
  else
    Result := GetValueDirectRaw;
end;

procedure TPLCBlockElement.SetValueRaw(Value:Double);
begin
  if PBlock<>nil then
     PBlock.ValueRaw[PIndex] := Value;
end;

procedure TPLCBlockElement.SetValueDirectRaw(Value:Double);
begin
  if PBlock<>nil then
     PBlock.ValueDirectRaw[PIndex] := Value;
end;

procedure TPLCBlockElement.ScanRead;
begin
  if PBlock<>nil then
    PValueRaw := PBlock.ValueRaw[PIndex];

  PValueDirectRaw := PValueRaw;
end;

procedure TPLCBlockElement.ScanWrite(Values:TArrayOfDouble; Count, Offset:DWORD);
begin
  if PBlock<>nil then
    PBlock.ValueRaw[PIndex] := values[0]
end;

procedure TPLCBlockElement.Read;
begin
  if PBlock<>nil then
    PValueRaw := PBlock.ValueDirectRaw[PIndex];

  PValueDirectRaw := PValueRaw;
end;

procedure TPLCBlockElement.Write(Values:TArrayOfDouble; Count, Offset:DWORD);
begin
  if PBlock<>nil then
    PBlock.ValueDirectRaw[PIndex] := values[0]
end;

procedure TPLCBlockElement.TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer);
begin

end;


end.
