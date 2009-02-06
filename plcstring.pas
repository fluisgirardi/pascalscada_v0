//: Implementação para Tags String.
unit PLCString;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, TagBlock, ProtocolTypes, ProtocolDriver, Math,
  hsutils;

type
  {:
  Define os possíveis tipos de strings.
  @value(stSIEMENS String no formato SIEMENS, onde os dois primeiros bytes da
  cadeia informam o tamanho máximo e quantos bytes desse tamanho já foram usados.)
  @value(stC A string só termina quando o código ASCII 0 é encontrado.)
  }
  TPLCStringTypes = (stSIEMENS, stC);
  //: Tag de comunicação em formato String.
  TPLCString = class(TTagBlock, ITagInterface, ITagString)
  private
    PValue:String;
    PByteSize:Byte;
    PStringType:TPLCStringTypes;
    PStringSize:DWORD;

    procedure SetBlockSize(size:DWORD);
    procedure SetStringSize(size:DWORD);
    procedure SetByteSize(bsize:Byte);
    procedure SetStringType(stype:TPLCStringTypes);
    procedure SetDummySize(s:DWORD);

    function  GetValue:String;
    function  GetValueDirect:String;
    procedure SetValue(Value:String);
    procedure SetValueDirect(Value:String);
    function  CalcBlockSize(IsWrite:Boolean):Cardinal;
    function  EncodeValues(values:TArrayOfDouble):String;
    function  DecodeValue(value:String):TArrayOfDouble;
    
    function  GetValueAsText(Prefix, Sufix, Format:string):String;
    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(Value:Variant):Boolean;
    function GetValueTimestamp:TDatetime;
  protected
    //: @seealso(TPLCTag.SetPLCHack)
    procedure SetPLCHack(v:DWORD); override;
    //: @seealso(TPLCTag.SetPLCSlot)
    procedure SetPLCSlot(v:DWORD); override;
    //: @seealso(TPLCTag.SetPLCStation)
    procedure SetPLCStation(v:DWORD); override;
    //: @seealso(TPLCTag.SetMemFileDB)
    procedure SetMemFileDB(v:DWORD); override;
    //: @seealso(TPLCTag.SetMemAddress)
    procedure SetMemAddress(v:DWORD); override;
    //: @seealso(TPLCTag.SetMemSubElement)
    procedure SetMemSubElement(v:DWORD); override;
    //: @seealso(TPLCTag.SetMemReadFunction)
    procedure SetMemReadFunction(v:DWORD); override;
    //: @seealso(TPLCTag.SetMemWriteFunction)
    procedure SetMemWriteFunction(v:DWORD); override;
    //: @seealso(TPLCTag.SetPath)
    procedure SetPath(v:String); override;
    //: @seealso(TPLCTag.SetProtocolDriver)
    procedure SetProtocolDriver(p:TProtocolDriver); override;
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    //: Lê/escreve uma string do equipamento de modo assincrono.
    property Value:String read PValue write SetValue;
    //: Lê/escreve uma string do equipamento de modo sincrono.
    property ValueDirect:String read GetValueDirect write SetValueDirect;
  published
    //: Quantidade máxima de caracteres da string.
    property StringSize:DWORD read PStringSize write SetStringSize;
    {:
    Tipo da String.
    @seealso(TPLCStringTypes)
    }
    property StringType:TPLCStringTypes read PStringType write SetStringType default stC;
    //: Tamanho em bits de cada caracter da string.
    property ByteSize:Byte read PByteSize write SetByteSize default 8;
    
    //: @seealso(TTag.OnValueChange)
    property OnValueChange;
    //: Tamanho do bloco (somente-leitura).
    property Size write SetDummySize;
  end;

implementation

uses variants;

constructor TPLCString.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PStringSize := 10;
  PByteSize := 8;
  PStringType := stC;
  SetBlockSize(11);
end;

destructor TPLCString.Destroy;
begin
  inherited Destroy;
  SetLength(PValues,0);
end;

//codifica uma array de valores em uma string
function  TPLCString.EncodeValues(values:TArrayOfDouble):String;
var
  aux1, maxbits, bit:Integer;
  ValueAux2, ValueP, ByteP, ValueBitP, ByteBitP:Integer;
  ValueAux, strLen, BitsByType :Byte;
begin
  //use a funcao de leitura
  if PProtocolDriver<>nil then
    BitsByType := Min(PProtocolDriver.SizeOfTag(Self, false),32)
  else
    BitsByType := 8;

  if Length(values)<=0 then begin
    Result := '';
    exit;
  end;

  case PStringType of
      //string formato SIEMENS de 7 ou 8 bits
     stSIEMENS:
     begin
       maxbits := length(values)*BitsByType;
       bit := 0;
       ByteP := 0;
       ByteBitP := 0;
       ValueP := 0;
       ValueBitP := 0;
       ValueAux := 0;
       strlen := 255;
       ValueAux2 := FloatToInteger(values[ValueP]);
       //passa bit a bit para montar a string
       while bit<maxbits do begin
         aux1 := Power(2,ValueBitP);
         if ((ValueAux2 and aux1)=aux1) then
           ValueAux := ValueAux + Power(2,ByteBitP);

         inc(bit);
         inc(ByteBitP);
         inc(ValueBitP);

         //incrementa os ponteiros
         if ByteBitP>=PByteSize then begin
           //se esta nos primeiros 2 bytes
           //acha o tamanho real da string
           //(o menor dos dois primeiros bytes)
           if ByteP<2 then begin
             strlen := min(strlen,ValueAux);
           end else begin
             Result := Result + char(ValueAux);
             //se alcançou o tamanho da string.
             if Length(Result)>=strlen then
               exit;
           end;
           inc(ByteP);
           ByteBitP := 0;
           ValueAux := 0;
         end;
         if ValueBitP>=BitsByType then begin
           ValueBitP := 0;
           Inc(ValueP);
           ValueAux2 := FloatToInteger(values[ValueP]);
         end;
       end;
       if ByteBitP<PByteSize then begin
         Result := Result + char(ValueAux);
       end;
     end;

      //string formato C de 7 ou 8 bits
     stC:
     begin
       maxbits := length(values)*BitsByType;
       bit := 0;
       //ByteP := 1;
       ByteBitP := 0;
       ValueP := 0;
       ValueBitP := 0;
       ValueAux := 0;
       ValueAux2 := FloatToInteger(values[ValueP]);
       //passa bit a bit para montar a string
       while bit<maxbits do begin
         aux1 := Power(2,ValueBitP);
         if ((ValueAux2 and aux1)=aux1) then
           ValueAux := ValueAux + Power(2,ByteBitP);

         inc(bit);
         inc(ByteBitP);
         inc(ValueBitP);

         //incrementa os ponteiros
         if ByteBitP>=PByteSize then begin
           //se encontrou um byte ZERO (fim de string)
           //para de processar os valores.
           if ValueAux=0 then
             exit
           else
             Result := Result + char(ValueAux);
           //inc(ByteP);
           ByteBitP := 0;
           ValueAux := 0;
         end;
         if ValueBitP>=BitsByType then begin
           ValueBitP := 0;
           Inc(ValueP);
           ValueAux2 := FloatToInteger(values[ValueP]);           
         end;
       end;
       if ByteBitP<PByteSize then begin
         Result := Result + char(ValueAux);
       end;
     end;
     else
       Result := '';
  end;
end;

//codifica uma uma string em array de valores
function  TPLCString.DecodeValue(value:String):TArrayOfDouble;
var
  ValueAux, aux1, maxbits, bit, bs:Integer;
  ValueP, ByteP, ValueBitP, ByteBitP:Integer;
  strLen, BitsByType :Byte;
begin
  if PProtocolDriver<>nil then
    BitsByType := Min(PProtocolDriver.SizeOfTag(Self,true),32)
  else
    BitsByType := 8;


  //usa a WriteFunction
  bs := CalcBlockSize(true);
  SetLength(Result,bs);

  case PStringType of
    //formato de String SIEMENS de 7 ou  8 bits
    stSIEMENS:
    begin
      strlen := Min(PStringSize, power(2,PByteSize)-1);
      strlen := Min(strLen,Length(value));
      maxbits := PByteSize * (strlen+2);
      bit := 0;
      ByteBitP := 0;
      ByteP := 1;
      ValueBitP := 0;
      ValueP := 0;
      ValueAux := 0;

      while bit<maxbits do begin
        //processa os dois primeiros bytes do formato siemens
        //que dizem o tamanho da string;
        if bit<(2*PByteSize) then begin
          aux1 := Power(2,ByteBitP);
          if (strLen and aux1)=aux1 then
            ValueAux := ValueAux + Power(2,ValueBitP);
        end else begin
          if bit=(2*PByteSize) then begin
            ByteBitP := 0;
            ByteP := 1;
          end;
          //processa os bytes da string
          aux1 := Power(2,ByteBitP);
          if (ord(value[ByteP]) and aux1)=aux1 then begin
            ValueAux := ValueAux + Power(2,ValueBitP);
          end;
        end;


        inc(bit);
        inc(ByteBitP);
        inc(ValueBitP);

        //incrementa os ponteiros
        if ByteBitP>=PByteSize then begin
          inc(ByteP);
          ByteBitP := 0;
        end;
        if ValueBitP>=BitsByType then begin
          Result[ValueP] := ValueAux;
          ValueAux :=0;
          ValueBitP := 0;
          Inc(ValueP);
        end;
      end;
      if (ValueP<bs) and (ValueBitP<BitsByType) then begin
        Result[ValueP] := ValueAux;
      end;
    end;
    //formato de String C de 7 ou  8 bits
    stC:
    begin
      strlen := Min(Length(value), power(2,PByteSize)-1);
      maxbits := PByteSize * (strlen);
      bit := 0;
      ByteBitP := 0;
      ByteP := 1;
      ValueBitP := 0;
      ValueP := 0;
      ValueAux := 0;
      while bit<maxbits do begin
        //processa os bytes da string
        aux1 := Power(2,ByteBitP);
        if (ord(value[ByteP]) and aux1)=aux1 then
          ValueAux := ValueAux + Power(2,ValueBitP);

        inc(bit);
        inc(ByteBitP);
        inc(ValueBitP);

        //incrementa os ponteiros
        if ByteBitP>=PByteSize then begin
          inc(ByteP);
          ByteBitP := 0;
        end;
        if ValueBitP>=BitsByType then begin
          Result[ValueP] := ValueAux;
          ValueAux :=0;
          ValueBitP := 0;
          Inc(ValueP);
        end;
      end;
      if (ValueP<bs)and (ValueBitP<BitsByType) then begin
        Result[ValueP] := ValueAux;
      end;
    end;
  end;

end;

function TPLCString.GetValueAsText(Prefix, Sufix, Format:string):String;
begin
   Result := Prefix + Value + Sufix;
end;

procedure TPLCString.SetPLCHack(v:DWORD);
begin
  inherited SetPLCHack(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetPLCSlot(v:DWORD);
begin
  inherited SetPLCSlot(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetPLCStation(v:DWORD);
begin
  inherited SetPLCStation(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemFileDB(v:DWORD);
begin
  inherited SetMemFileDB(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemAddress(v:DWORD);
begin
  inherited SetMemAddress(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemSubElement(v:DWORD);
begin
  inherited SetMemSubElement(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemReadFunction(v:DWORD);
begin
  inherited SetMemReadFunction(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemWriteFunction(v:DWORD);
begin
  inherited SetMemWriteFunction(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetPath(v:String);
begin
  inherited SetPath(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetProtocolDriver(p:TProtocolDriver);
begin
  inherited SetProtocolDriver(p);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer);
var
  c:Integer;
  notify:Boolean;
begin
  if (csDestroying in ComponentState) then exit;
  try
    notify := false;
    case TagCommand of
      tcScanRead, tcRead:
      begin
        if LastResult in [ioOk, ioNullDriver] then begin
          for c := 0 to Length(Values) - 1 do begin
            notify := notify or (PValues[c+Offset]<>values[c]);
            PValues[c+Offset]:=values[c];
          end;
          PValueTimeStamp := ValuesTimeStamp;
          if LastResult=ioOk then
             IncCommReadOK(1);
        end else
          IncCommReadFaults(1);
      end;
      tcScanWrite,tcWrite:
      begin
        if LastResult in [ioOk, ioNullDriver] then begin
          if LastResult=ioOk then
             IncCommWriteOK(1);
          for c := 0 to Length(Values) - 1 do begin
            notify := notify or (PValues[c+Offset]<>values[c]);
            PValues[c+Offset]:=values[c]
          end;
        end else
          IncCommWriteFaults(1);
      end;
    end;

    case TagCommand of
      tcScanRead:
        PLastASyncReadCmdResult := LastResult;
      tcScanWrite:
        PLastASyncWriteCmdResult := LastResult;
      tcRead:
        PLastSyncReadCmdResult := LastResult;
      tcWrite:
        PLastSyncWriteCmdResult := LastResult;
    end;

    if notify then begin
      PValue := EncodeValues(PValues);
      NotifyChange;
    end;
  finally
  end;
end;

procedure TPLCString.SetBlockSize(size:DWORD);
var
  old:DWORD;
begin
  if size>0 then begin
    old:=PSize;
    PSize := size;
    SetLength(PValues, PSize);
    if PProtocolDriver<>nil then
       PProtocolDriver.TagChanges(self,tcSize,old,size);
  end;
end;

procedure TPLCString.SetStringSize(size:DWORD);
begin
   if size>255 then
     raise Exception.Create('Tamanho máximo da string fora dos limites!');
   PStringSize := size;
   SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetByteSize(bsize:Byte);
begin
   if (bsize<7) or (bsize>8) then
     raise Exception.Create('O tamanho do byte pode ser 7 ou 8 somente!');
   
   PByteSize := bsize;
   SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetStringType(stype:TPLCStringTypes);
begin
  PStringType := stype;
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetDummySize(s:DWORD);
begin

end;

function  TPLCString.GetValue:String;
begin
  Result := PValue;
end;

function  TPLCString.GetValueDirect:String;
begin
  Read;
  Result := PValue;
end;

procedure TPLCString.SetValue(Value:String);
var
  x:TArrayOfDouble;
begin
  x:=DecodeValue(Value);
  ScanWrite(x,Length(x),0);
  SetLength(x,0);
end;

procedure TPLCString.SetValueDirect(Value:String);
var
  x:TArrayOfDouble;
begin
  x:=DecodeValue(Value);
  Write(x,Length(x),0);
  SetLength(x,0);
end;

function TPLCString.CalcBlockSize(IsWrite:Boolean):Cardinal;
var
  BitsByType:Byte;
  strlen:integer;
begin
  if PProtocolDriver<>nil then begin
    BitsByType := PProtocolDriver.SizeOfTag(Self,IsWrite);
    BitsByType := IfThen(BitsByType=0,1,BitsByType);
  end else
    BitsByType := 8;

  //calcula o tamanho da string conforme o tipo
  case PStringType of
    stSIEMENS:
      strlen := (PStringSize + 2)*PByteSize;
    stC:
      strlen := (PStringSize + 1)*PByteSize;
    else
      strlen := 1;
  end;

  Result := strlen div BitsByType + IfThen((strlen mod BitsByType)>0,1,0);
end;

function  TPLCString.GetVariantValue:Variant;
begin
   Result := Value;
end;

procedure TPLCString.SetVariantValue(V:Variant);
begin
   Value := V
end;

function  TPLCString.IsValidValue(Value:Variant):Boolean;
begin
  Result := VarIsNumeric(Value) or VarIsStr(Value) or
            VarIsType(Value,vardate) or VarIsType(Value,varboolean);
end;

function  TPLCString.GetValueTimestamp:TDatetime;
begin
   Result := PValueTimeStamp;
end;

end.
