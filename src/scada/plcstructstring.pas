unit plcstructstring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PLCString, PLCTag, Tag, ProtocolTypes, PLCStruct;

type

  { TPLCStructString }

  TPLCStructString = class(TPLCTag, ITagInterface, ITagString)
  private
    FStringEncoding: TStringEncodings;
    PLoadedBlock,
    PBlock: TPLCStruct;
    PIndex: Cardinal;
    PStringSize: Cardinal;
    PStringType: TPLCStringTypes;
    PValue: UTF8String;
    function GetRealStringSize: Integer;
    function GetValueChangeData: Pointer;
    procedure ReleaseChangeData(data: Pointer);
    procedure SetBlock(AValue: TPLCStruct);
    procedure SetIndex(AValue: Cardinal);
    procedure SetStringEncoding(AValue: TStringEncodings);
    procedure SetStringSize(AValue: Cardinal);
    procedure SetStringType(AValue: TPLCStringTypes);
  private
    procedure BlockReadOk(Sender:TObject);
    procedure BlockReadFault(Sender:TObject);
    procedure BlockWriteOk(Sender:TObject);
    procedure BlockWriteFault(Sender:TObject);
    procedure BlockTagChange(Sender:TObject);
    procedure BlockRemoveTag(Sender:TObject);
  private
    ////////////////////////////////////////////////////////////////////////////
    // ITAGInterface
    ////////////////////////////////////////////////////////////////////////////

    {$IFDEF PORTUGUES}
    //: Retorna o valor do tag como String, incluindo formatação (se aplicável), prefixo e sufixo.
    {$ELSE}
    //: Returns the tag value as string, including the format (if applicable), prefix and suffix.
    {$ENDIF}
    function  GetValueAsText(Prefix, Sufix, Format:UTF8String; FormatDateTimeOptions:TFormatDateTimeOptions=[]):UTF8String;

    {$IFDEF PORTUGUES}
    //: Retorna o valor do tag como variant.
    {$ELSE}
    //: Returns the tag value as variant.
    {$ENDIF}
    function  GetVariantValue:Variant;

    {$IFDEF PORTUGUES}
    //: Seta um variant como valor do tag se possível.
    {$ELSE}
    //: If possible, sets a variant as tag value.
    {$ENDIF}
    procedure SetVariantValue(V:Variant);

    {$IFDEF PORTUGUES}
    //: Retorna @true se o valor é aceito pelo tag.
    {$ELSE}
    //: Returns @true if the variant value will be accept by tag.
    {$ENDIF}
    function  IsValidValue(Value:Variant):Boolean;

    {$IFDEF PORTUGUES}
    //: Retorna a data/hora em que o tag foi atualizado pela última vez.
    {$ELSE}
    //: Returns the date/time of the last time wich the tag was updated.
    {$ENDIF}
    function  GetValueTimestamp:TDatetime;
  private
    POnAsyncStringValueChange: TASyncStringValueChange;
    ////////////////////////////////////////////////////////////////////////////
    // ITAGString interface..
    ////////////////////////////////////////////////////////////////////////////

    {$IFDEF PORTUGUES}
    //: Retorna o valor do tag texto.
    {$ELSE}
    //: Returns the text value of tag.
    {$ENDIF}
    function  GetValue:UTF8String;

    {$IFDEF PORTUGUES}
    //: Escreve um texto no tag.
    {$ELSE}
    //: Writes a text value on tag.
    {$ENDIF}
    procedure SetValue(AValue:UTF8String);
  protected
    procedure Loaded; override;
    function ValidSettings(aBlockSize, aIndex, aStrSize:Cardinal; aStrType:TPLCStringTypes):Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    {$IFDEF PORTUGUES}
    //: Offset da elemento de memória dentro do bloco (indice).
    {$ELSE}
    //: Index of tag element on the Tag Block.
    {$ENDIF}
    property Index:Cardinal read PIndex write SetIndex;

    {$IFDEF PORTUGUES}
    //: Bloco de comunicações que o elemento pertence.
    {$ELSE}
    //: Communication Block of the element.
    {$ENDIF}
    property PLCBlock:TPLCStruct read PBlock write SetBlock;

    {$IFDEF PORTUGUES}
    //: Tamanho real do bloco (somente-leitura).
    {$ELSE}
    //: Real block size (read-only).
    {$ENDIF}
    property StringBlockSize:Integer read GetRealStringSize;

    property StringEncoding:TStringEncodings read FStringEncoding write SetStringEncoding default UTF_8;

    {$IFDEF PORTUGUES}
    //: Quantidade máxima de caracteres da string.
    {$ELSE}
    //: Maximum length of your string.
    {$ENDIF}
    property StringSize:Cardinal read PStringSize write SetStringSize;

    {$IFDEF PORTUGUES}
    {:
    Tipo da String.
    @seealso(TPLCStringTypes)
    }
    {$ELSE}
    {:
    String format.
    @seealso(TPLCStringTypes)
    }
    {$ENDIF}
    property StringType:TPLCStringTypes read PStringType write SetStringType default stC;

    {$IFDEF PORTUGUES}
    //: Lê/escreve uma string do equipamento.
    {$ELSE}
    //: Read/writes a string value on device
    {$ENDIF}
    property Value:UTF8String read PValue write SetValue;

    //: @seealso(TTag.OnValueChangeFirst)
    property OnValueChangeFirst;
    //: @seealso(TTag.OnValueChangeLast)
    property OnValueChangeLast;
    {$IFDEF PORTUGUES}
    //: Evento assincrono chamado quando o valor do tag sofre uma alteração.
    {$ELSE}
    //: Asynchronous event called when the tag value changes.
    {$ENDIF}
    property OnAsyncStringChange:TASyncStringValueChange read POnAsyncStringValueChange write POnAsyncStringValueChange;
  end;

implementation

uses Math, Variants;

{ TPLCStructString }

function TPLCStructString.GetValueChangeData: Pointer;
var
  x:PUTF8String;
begin
  New(x);
  x^:=Value;
  Result:=x;
end;

procedure TPLCStructString.ReleaseChangeData(data: Pointer);
var
  x:PUTF8String;
begin
  x:=data;
  SetLength(x^,0);
  Dispose(x);
end;

function TPLCStructString.GetRealStringSize: Integer;
begin
  case PStringType of
    stC: Result:=StringSize+1;        //+1 = Null terminator at string end.
    stSIEMENS: Result:=StringSize+2;  //+2 string size and string capacity at begining of string.
    stROCKWELL: Result:=StringSize+4; //+4 string size (dword) at begining of string;
  end;
end;

procedure TPLCStructString.SetBlock(AValue: TPLCStruct);
begin
  if [csLoading,csReading]*ComponentState<>[] then begin
    PLoadedBlock:=AValue;
    exit;
  end;

  if PBlock=AValue then Exit;

  if Assigned(AValue) then begin
    if not ValidSettings(AValue.Size,PIndex,PStringSize,PStringType) then
      exit;

    AValue.AddTagChangeHandler(@BlockTagChange);
    AValue.AddRemoveTagHandler(@BlockRemoveTag);
    aValue.AddWriteFaultHandler(@BlockWriteFault);
  end;

  if Assigned(PBlock) then
    PBlock.RemoveAllHandlersFromObject(Self);

  PBlock:=AValue;
end;

procedure TPLCStructString.SetIndex(AValue: Cardinal);
begin
  if PIndex=AValue then Exit;

  if Assigned(PBlock) and (not ValidSettings(PBlock.Size,AValue,PStringSize,PStringType)) then
      exit;

  PIndex:=AValue;
end;

procedure TPLCStructString.SetStringEncoding(AValue: TStringEncodings);
begin
  if FStringEncoding=AValue then Exit;
  FStringEncoding:=AValue;
  BlockTagChange(Self);
end;

procedure TPLCStructString.SetStringSize(AValue: Cardinal);
begin
  if PStringSize=AValue then Exit;

  if Assigned(PBlock) and (not ValidSettings(PBlock.Size,PIndex,AValue,PStringType)) then
    exit;

  PStringSize:=AValue;
end;

procedure TPLCStructString.SetStringType(AValue: TPLCStringTypes);
begin
  if PStringType=AValue then Exit;

  if Assigned(PBlock) and (not ValidSettings(PBlock.Size,PIndex,PStringSize,AValue)) then
    exit;

  PStringType:=AValue;
end;

procedure TPLCStructString.BlockReadOk(Sender: TObject);
begin
  { TODO -oFabio : Not necessary yet. }
end;

procedure TPLCStructString.BlockReadFault(Sender: TObject);
begin
  { TODO -oFabio : Not necessary yet. }
end;

procedure TPLCStructString.BlockWriteOk(Sender: TObject);
begin
  { TODO -oFabio : Not necessary yet. }
end;

procedure TPLCStructString.BlockWriteFault(Sender: TObject);
begin
  BlockTagChange(Sender);
end;

procedure TPLCStructString.BlockTagChange(Sender: TObject);
var
  aux:RawByteString;
  PCurrStrSize: LongWord;
  i: Cardinal;
  b: Byte;
  aux2: UTF8String;
begin
  if Assigned(PBlock) and ValidSettings(PLCBlock.Size,PIndex,PStringSize,PStringType) then begin
    case PStringType of
      stC: begin
        aux:='';
        for i:=PIndex to (PIndex+PStringSize)-1 do begin
          b:=PBlock.GetByte(i);
          if b=0 then break;
          aux:=aux+chr(PBlock.GetByte(i));
        end;
        aux2:=TPLCString.ConvertRawByteStringToUTF8(aux,FStringEncoding);
      end;

      stSIEMENS: begin
        aux:='';
        aux:=PBlock.GetSiemensString(PIndex,PStringSize);
        aux2:=TPLCString.ConvertRawByteStringToUTF8(aux,FStringEncoding);
      end;

      stROCKWELL: begin
        aux:='';
        PCurrStrSize:=PBlock.GetLongWord(PIndex,false,false);
        for i:=PIndex+4 to PIndex+3+min(PCurrStrSize,PStringSize) do begin
          b:=PBlock.GetByte(i);
          if b=0 then break;
          aux:=aux+chr(PBlock.GetByte(i));
        end;
        aux2:=TPLCString.ConvertRawByteStringToUTF8(aux,FStringEncoding);
      end;
      else aux2:='';
    end;

    if aux2<>PValue then begin
      PValue:=aux2;
      NotifyChange;
    end;
  end;
end;

procedure TPLCStructString.BlockRemoveTag(Sender: TObject);
begin
  if Sender=PBlock then PBlock:=nil;
end;

procedure TPLCStructString.SetValue(AValue: UTF8String);
var
  aux:TArrayOfDouble;
  limit: Extended;
  c, PCurrStrSize: Integer;
begin
  if Assigned(PBlock) and ValidSettings(PLCBlock.Size,PIndex,PStringSize,PStringType) then begin
    case PStringType of
      stC: begin
        limit:=min(PStringSize,Length(AValue));
        SetLength(aux,PStringSize);
        for c:=0 to high(aux) do begin
          if c<limit then
            aux[c]:=TPLCString.ConvertUTF8CharToByte(AValue,FStringEncoding,c+1)
          else
            aux[c]:=0;
        end;
        aux[high(aux)]:=0;
      end;
      stSIEMENS: begin
        SetLength(aux,PStringSize+2);
        PBlock.AddSiemensString(aux,0,AValue,PStringSize);
      end;
      stROCKWELL: begin
        SetLength(aux,PStringSize+4);
        PCurrStrSize:=Length(AValue);

        PBlock.AddDWord(aux,0,PCurrStrSize,false,false);
        limit:=min(PStringSize,Length(AValue));
        for c:=4 to high(aux) do begin
          if (c-4)<limit then
            aux[c]:=TPLCString.ConvertUTF8CharToByte(AValue,FStringEncoding,c-3)
          else
            aux[c]:=0;
        end;
        aux[high(aux)]:=0;
      end
      else exit;
    end;
    PBlock.ScanWrite(aux, Length(aux), PIndex);
  end;

  if PValue<>AValue then NotifyChange;
  PValue:=AValue;
end;

procedure TPLCStructString.Loaded;
begin
  inherited Loaded;
  SetBlock(PLoadedBlock);
end;

function TPLCStructString.ValidSettings(aBlockSize, aIndex, aStrSize: Cardinal;
  aStrType: TPLCStringTypes): Boolean;
var
  RealSize:Integer;
begin
  case aStrType of
    stC:        RealSize:=aStrSize+1; //+1 = Null terminator at string end.
    stSIEMENS:  RealSize:=aStrSize+2; //+2 string size and string capacity at begining of string.
    stROCKWELL: RealSize:=aStrSize+4; //+4 string size (dword) at begining of string;
    else exit(false);
  end;

  if (aindex+RealSize)>aBlockSize then exit(false);

  Result:=true;
end;

function TPLCStructString.GetValueAsText(Prefix, Sufix, Format: UTF8String;
  FormatDateTimeOptions: TFormatDateTimeOptions): UTF8String;
begin
  Result := Prefix + Value + Sufix;
end;

function TPLCStructString.GetVariantValue: Variant;
begin
  Result:=Value;
end;

procedure TPLCStructString.SetVariantValue(V: Variant);
begin
  Value:=V;
end;

function TPLCStructString.IsValidValue(Value: Variant): Boolean;
begin
  Result := VarIsNumeric(Value) or VarIsStr(Value) or
            VarIsType(Value,vardate) or VarIsType(Value,varboolean);
end;

function TPLCStructString.GetValueTimestamp: TDatetime;
begin
  Result:=0;
  if Assigned(PBlock) then
    Result:=PBlock.ValueTimestamp;
end;

function TPLCStructString.GetValue: UTF8String;
begin
  Result:=Value;
end;

constructor TPLCStructString.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PStringSize := 10;
  PStringType := stC;
end;

destructor TPLCStructString.Destroy;
begin
  if Assigned(PBlock) then
    PBlock.RemoveAllHandlersFromObject(Self);
  inherited Destroy;
end;

end.

