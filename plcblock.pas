{:
  @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

  @abstract(Implementação de um tag de bloco de comunicação.)
}
unit PLCBlock;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, TagBlock, ProtocolTypes;

type
  {:
    @author(Fabio Luis Girardi <papelhigienico@gmail.com>)

    Tag para comunicação em blocos.
  }
  TPLCBlock = class(TTagBlock)
  private
    procedure SetSize(size:Cardinal);
    function  GetValue(Index:Integer):Double;
    procedure SetValue(index:Integer; Value:Double);

    function  GetValues:TArrayOfDouble;
    procedure SetValues(values:TArrayOfDouble);
  protected
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    {:
    @name escreve assincronamente os valores atribuidos ao bloco.
    @bold(Só tem efeito caso AutoWrite = @false.)
    }
    procedure WriteByScan;
    {:
    @name escreve sincronamente os valores atribuidos ao bloco.
    @bold(Só tem efeito caso AutoWrite = @false.)
    }
    procedure WriteDirect;

    //: @seealso(TPLCTag.ScanRead)
    procedure ScanRead; override;
    //: @seealso(TPLCTag.ScanWrite)
    procedure Read; override;

    //: @seealso(TPLCTag.ScanWrite)
    procedure ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal); override;
    //: @seealso(TPLCTag.Write)
    procedure Write(Values:TArrayOfDouble; Count, Offset:Cardinal); override;

    //: Lê/escreve um valor puro de modo assincrono em um item do bloco.
    property ValueRaw[index:Integer]:Double read GetValue write SetValue;
    //: Lê/escreve valores (array) puros de modo assincrono em um item do bloco.
    property ValuesRaw:TArrayOfDouble read GetValues write SetValues;
  published
    //: Tamanho de elementos do bloco.
    property Size write SetSize;
    //: @seealso(TTag.OnValueChange)
    property OnValueChange;
    //: @seealso(TPLCTag.SyncWrites)
    property SyncWrites;
    //: @seealso(TPLCTag.TagType)
    property TagType;
    //: @seealso(TPLCTag.SwapBytes)
    property SwapBytes;
    //: @seealso(TPLCTag.SwapWords)
    property SwapWords;
    //: @seealso(TPLCTag.TagSizeOnProtocol)
    property TagSizeOnProtocol;
  end;

implementation

uses hsstrings;

constructor TPLCBlock.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   PSize:=1;
   SetLength(PValues,PSize);
end;

destructor TPLCBlock.Destroy;
begin
   inherited Destroy;
   SetLength(PValues,0);
end;

procedure TPLCBlock.TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer);
var
  c:Integer;
  notify:Boolean;
  TagValues:TArrayOfDouble;
begin
  if (csDestroying in ComponentState) then exit;
  if TagCommand in [tcScanWrite, tcWrite] then
    notify:=false;
  try
    inherited TagCommandCallBack(Values, ValuesTimeStamp, TagCommand, LastResult, Offset);
    TagValues:=PLCValuesToTagValues(Values, Offset);
    notify := false;
    case TagCommand of
      tcScanRead, tcRead:
      begin
        if LastResult in [ioOk, ioNullDriver] then begin
          for c := 0 to High(TagValues) do begin
            notify := notify or (PValues[c+Offset]<>TagValues[c]);
            PValues[c+Offset]:=TagValues[c];
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
          for c := 0 to High(TagValues) do begin
            notify := notify or (PValues[c+Offset]<>TagValues[c]);
            PValues[c+Offset]:=TagValues[c]
          end;
          PValueTimeStamp := ValuesTimeStamp;
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

    if notify then
      NotifyChange;
  finally
    SetLength(TagValues,0);
  end;
end;

procedure TPLCBlock.SetSize(size:Cardinal);
var
  old:Cardinal;
begin
  if size>0 then begin
    old:=PSize;
    PSize := size;
    SetLength(PValues,PSize);
    if PProtocolDriver<>nil then
       PProtocolDriver.TagChanges(self,tcSize,old,size);
  end;
end;

function  TPLCBlock.GetValue(Index:Integer):Double;
begin
   if ((index<0) or (Index>High(PValues))) then
     raise Exception.Create(SoutOfBounds);
   Result := PValues[Index];
end;

procedure TPLCBlock.SetValue(index:Integer; Value:Double);
var
  towrite:TArrayOfDouble;
begin
  SetLength(towrite,1);
  towrite[0] := Value;
  if FSyncWrites then
    Write(towrite,1,index)
  else
    ScanWrite(towrite,1,index);
  SetLength(towrite,0);
end;

function TPLCBlock.GetValues:TArrayOfDouble;
begin
  Result:=PValues;
end;

procedure TPLCBlock.SetValues(values:TArrayOfDouble);
var
  towrite:TArrayOfDouble;
begin
  towrite := values;
  if FSyncWrites then
    Write(towrite,PSize,0)
  else
    ScanWrite(towrite,PSize,0);
  SetLength(towrite,0);
end;

procedure TPLCBlock.WriteByScan;
var
  x:Boolean;
begin
  x:=PAutoWrite;
  PAutoWrite := true;
  ScanWrite(PValues,PSize,0);
  PAutoWrite := x;
end;

procedure TPLCBlock.WriteDirect;
begin
  Write(PValues,PSize,0);
end;

procedure TPLCBlock.ScanRead;
begin
  inherited ScanRead;
end;

procedure TPLCBlock.Read;
begin
  inherited Read;
end;

procedure TPLCBlock.ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal);
var
  PLCValues:TArrayOfDouble;
begin
  PLCValues:=TagValuesToPLCValues(Values,Offset);
  inherited ScanWrite(PLCValues, Count, Offset);
  SetLength(PLCValues,0);
end;

procedure TPLCBlock.Write(Values:TArrayOfDouble; Count, Offset:Cardinal);
var
  PLCValues:TArrayOfDouble;
begin
  PLCValues:=TagValuesToPLCValues(Values, Offset);
  inherited Write(Values, Count, Offset);
  SetLength(PLCValues,0);
end;

end.
