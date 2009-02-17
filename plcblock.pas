//: Implementação de um bloco de comunicação
unit PLCBlock;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, TagBlock, ProtocolTypes;

type
  //: Tag para comunicação em blocos.
  TPLCBlock = class(TTagBlock)
  private
    procedure SetSize(size:DWORD);
    function  GetValue(Index:Integer):Double;
    procedure SetValue(index:Integer; Value:Double);

    function  GetValues:TArrayOfDouble;
    procedure SetValues(values:TArrayOfDouble);
  protected
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); override;
    //: @exclude
    procedure Loaded; override;
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
    procedure ScanWrite(Values:TArrayOfDouble; Count, Offset:DWORD); override;
    //: @seealso(TPLCTag.Write)
    procedure Write(Values:TArrayOfDouble; Count, Offset:DWORD); override;

    //: Lê/escreve um valor puro de modo assincrono em um item do bloco.
    property ValueRaw[index:Integer]:Double read GetValue write SetValue;
    //: Lê/escreve valores (array) puros de modo assincrono em um item do bloco.
    property ValuesRaw:TArrayOfDouble read GetValues write SetValues;
  published
    //: Tamanho de elementos do bloco.
    property Size write SetSize;
    //: @seealso(TTag.OnValueChange)
    property OnValueChange;
  end;

implementation

uses Tag, Math;

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
begin
  if (csDestroying in ComponentState) then exit;
  try
    notify := false;
    case TagCommand of
      tcScanRead, tcRead:
      begin
        if LastResult in [ioOk, ioNullDriver] then begin
          for c := 0 to High(Values) do begin
             //if (c+Offset)<Length(PValues) then begin
             notify := notify or (PValues[c+Offset]<>values[c]);
             PValues[c+Offset]:=values[c];
             //end;
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
          for c := 0 to High(Values) do begin
            //if (c+Offset)<Length(PValues) then begin
            notify := notify or (PValues[c+Offset]<>values[c]);
            PValues[c+Offset]:=values[c]
            //end;
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
  end;
end;

procedure TPLCBlock.Loaded;
begin
  inherited Loaded;
  //faca alguma coisa q precise ser feita apos o tag ser carregado...
end;

procedure TPLCBlock.SetSize(size:DWORD);
var
  old:DWORD;
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
     raise Exception.Create('Fora dos limites!');
   Result := PValues[Index];
end;

procedure TPLCBlock.SetValue(index:Integer; Value:Double);
var
  towrite:TArrayOfDouble;
begin
  SetLength(towrite,1);
  towrite[0] := Value;
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
  c:Integer;
begin
  towrite := values;
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

procedure TPLCBlock.ScanWrite(Values:TArrayOfDouble; Count, Offset:DWORD);
begin
  inherited ScanWrite(Values, Count, Offset);
end;

procedure TPLCBlock.Write(Values:TArrayOfDouble; Count, Offset:DWORD);
begin
  inherited Write(Values, Count, Offset);
end;

end.
