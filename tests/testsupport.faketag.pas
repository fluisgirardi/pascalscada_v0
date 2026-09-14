{$i ../src/common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Tag numerico de mentira, para os testes da camada de tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Os tags que derivam de outro - o de bits, o de blocos, os processadores de
  valor - conversam com o tag de origem por ITagNumeric e pelos avisos de
  mudanca. Nenhum deles precisa de driver, de porta ou de CLP: precisam de
  alguem que guarde um valor e avise quando ele muda. E' o que esta aqui.
}
{$ELSE}
{:
  @abstract(A fake numeric tag, for the tag layer tests.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  Tags derived from another one - bit tags, block tags, value processors -
  talk to their source through ITagNumeric and the change notifications. None
  of them needs a driver, a port or a PLC: they need someone holding a value
  and telling when it changes. That is what lives here.
}
{$ENDIF}
unit testsupport.faketag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants, PLCNumber, ProtocolTypes, Tag;

type

  { TFakeNumber }

  TFakeNumber = class(TPLCNumber, ITagInterface, ITagNumeric)
  private
    FValorPuro:Double;
    FLeituras, FEscritas:LongInt;
  protected
    function  GetValueRaw:Double; override;
    procedure SetValueRaw(aValue:Double); override;

    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(aValue:Variant):Boolean;
    function  GetClockMonotonicTimestamp:QWord;

    function GetLastAsyncReadStatus: TProtocolIOResult;  override;
    function GetLastAsyncWriteStatus: TProtocolIOResult; override;
    function GetLastSyncReadStatus: TProtocolIOResult;   override;
    function GetLastSyncWriteStatus: TProtocolIOResult;  override;
  public
    constructor Create(AOwner:TComponent); override;

    //: escreve o valor e avisa quem escuta, como faria uma varredura
    procedure ChegouDoCLP(aValor:Double);

    procedure Read; override;
    procedure Write(Values:TArrayOfDouble; Count, Offset:Cardinal); overload; override;
    function  ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal; const IgnoreAutoWrite:Boolean = false):Int64; override;

    //: quantas vezes pediram leitura e escrita a este tag
    property Leituras:LongInt read FLeituras;
    property Escritas:LongInt read FEscritas;
  end;

implementation

constructor TFakeNumber.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  AutoRead :=false;
  AutoWrite:=false;
  FValorPuro:=0;
  FLeituras :=0;
  FEscritas :=0;
end;

function TFakeNumber.GetValueRaw:Double;
begin
  Result:=FValorPuro;
end;

procedure TFakeNumber.SetValueRaw(aValue:Double);
begin
  inc(FEscritas);
  ChegouDoCLP(aValue);
end;

procedure TFakeNumber.ChegouDoCLP(aValor:Double);
begin
  FValorPuro:=aValor;
  PValueRaw :=aValor;
  PClockMonotonicTimeStamp:=GetTickCount64;
  NotifyChange;
end;

function TFakeNumber.GetVariantValue:Variant;
begin
  Result:=Value;
end;

procedure TFakeNumber.SetVariantValue(V:Variant);
begin
  Value:=V;
end;

function TFakeNumber.IsValidValue(aValue:Variant):Boolean;
var
  aux:Double;
  aValueStr:AnsiString;
begin
  //a mesma regra do TPLCTagNumber e do TPLCBlockElement: os controles de
  //entrada perguntam isso com o TEXTO que o operador digitou, entao recusar
  //string faria a caixa de edicao devolver tudo que fosse teclado.
  //the same rule as TPLCTagNumber and TPLCBlockElement: the input controls ask
  //this with the TEXT the operator typed, so refusing strings would make the
  //edit box give back everything that was typed.
  aValueStr:=aValue;
  Result:=VarIsNumeric(aValue) or
          (VarIsStr(aValue) and TryStrToFloat(aValueStr,aux)) or
          VarIsType(aValue, varboolean);
end;

function TFakeNumber.GetClockMonotonicTimestamp:QWord;
begin
  Result:=PClockMonotonicTimeStamp;
end;

function TFakeNumber.GetLastAsyncReadStatus: TProtocolIOResult;
begin
  Result:=ioOk;
end;

function TFakeNumber.GetLastAsyncWriteStatus: TProtocolIOResult;
begin
  Result:=ioOk;
end;

function TFakeNumber.GetLastSyncReadStatus: TProtocolIOResult;
begin
  Result:=ioOk;
end;

function TFakeNumber.GetLastSyncWriteStatus: TProtocolIOResult;
begin
  Result:=ioOk;
end;

procedure TFakeNumber.Read;
begin
  inc(FLeituras);
end;

procedure TFakeNumber.Write(Values:TArrayOfDouble; Count, Offset:Cardinal);
begin
  if Length(Values)>0 then
    ChegouDoCLP(Values[0]);
  inc(FEscritas);
end;

function TFakeNumber.ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal; const IgnoreAutoWrite:Boolean = false):Int64;
begin
  Write(Values, Count, Offset);
  Result:=0;
end;

end.
