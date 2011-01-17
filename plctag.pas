{:
@abstract(Implementa a base para Tags de comunicação.)
@author(Fabio Luis Girardi papelhigienico@gmail.com)
}
unit PLCTag;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, ExtCtrls, Classes, Tag, ProtocolDriver, ProtocolTypes, Math;

type
  {:
  @abstract(Classe base para todos os tags de comunicação.)
  @author(Fabio Luis Girardi papelhigienico@gmail.com)
  }
  TPLCTag = class(TTag, IManagedTagInterface)
  private
    FRawProtocolValues:TArrayOfDouble;
    FTotalTime, FReadCount:Int64;
    FFirtsRead:Boolean;
    FProtocoloOnLoading:TProtocolDriver;
  private
    procedure RebuildTagGUID;
    function  GetTagSizeOnProtocol:Integer;
  protected
    PValidTag:Boolean;
    procedure GetNewProtocolTagSize;
    function RemainingMiliseconds:Int64; virtual;
    function RemainingMilisecondsForNextScan:Int64; virtual;
    function IsValidTag:Boolean; virtual;
    procedure SetTagValidity(TagValidity:Boolean); virtual;
  protected
    //: Referencia ao objeto gerenciador de tags.
    FTagManager:TObject;
    //: A escrita do tag deve ser sincrona ou assincrona
    FSyncWrites:Boolean;
    //: Armazena o driver de protocolo usado para comunicação do tag.
    PProtocolDriver:TProtocolDriver;
    //: Data/Hora da última tentativa de leitura do valor do tag.
    PLastScanTimeStamp:TDateTime;
    //: Data/Hora da última atualização do valor do tag.
    PValueTimeStamp:TDateTime;
    //: Armazena o resultado da última leitura @bold(sincrona) realizada pelo tag.
    PLastSyncReadCmdResult:TProtocolIOResult;
    //: Armazena o resultado da última escrita sincrona realizada pelo tag.
    PLastSyncWriteCmdResult:TProtocolIOResult;
    //: Armazena o resultado da última leitura @bold(assincrona) realizada pelo tag.
    PLastASyncReadCmdResult:TProtocolIOResult;
    //: Armazena o resultado da última escrita @bold(assincrona) realizada pelo tag.
    PLastASyncWriteCmdResult:TProtocolIOResult;

    //: Tipo de dado retornado pelo protocolo.
    FProtocolTagType:TProtocolTagType;
    //: Tipo de dado do tag
    FTagType:TTagType;
    //: As words da palavra mesclada serão invertidas
    FSwapWords:Boolean;
    //: As bytes das words da palavra mesclada serão invertidas
    FSwapBytes:Boolean;

    FProtocolWordSize,
    FCurrentWordSize:Byte;

    //: Valores vindos do PLC são convertidos para o tipo de dados configurado no tag.
    function PLCValuesToTagValues(Values:TArrayOfDouble; Offset:Cardinal):TArrayOfDouble; virtual;

    //: Valores vindo do tag são convertidos para o tipo de aceito pelo driver.
    function TagValuesToPLCValues(Values:TArrayOfDouble; Offset:Cardinal):TArrayOfDouble; virtual;

    //: Retorna a média de tempo que o tag é atualizado.
    function GetAvgUpdateRate:Double;

    //: Retorna o tamanho real do tag.
    procedure UpdateTagSizeOnProtocol;

    //: Recompila os valores do tag.
    procedure RebuildValues; virtual;

    {:
    Habilita/Desabilita o swap de words.
    @param(v Boolean: @true habilita, @false desabilita.)
    }
    procedure SetSwapWords(v:Boolean); virtual;

    {:
    Habilita/Desabilita o swap de bytes.
    @param(v Boolean: @true habilita, @false desabilita.)
    }
    procedure SetSwapBytes(v:Boolean); virtual;

    //: @exclude
    procedure SetGUID(v:String);

    //##########################################################################

    {:
    Habilita/Desabilita a leitura automática do tag.
    @param(v Boolean: @true habilita, @false desabilita.)
    }
    procedure SetAutoRead(v:Boolean); virtual;
    {:
    Habilita/Desabilita a escrita automática de valores do tag.
    @param(v Boolean: @true habilita, @false desabilita.)
    }
    procedure SetAutoWrite(v:Boolean); virtual;
    {:
    Seta o endereço da memória sendo mapeada.
    @param(v Cardinal. Endereço da memória sendo mapeada.)
    }
    procedure SetMemAddress(v:Cardinal); virtual;
    {:
    Seta o Arquivo/DB que contem a memória sendo mapeada.
    @param(v Cardinal. Arquivo/DB que a memória mapeada pertence.)
    }
    procedure SetMemFileDB(v:Cardinal); virtual;
    {:
    Seta o função do driver para leitura da memória.
    @param(v Cardinal. Função do driver usada para leitura da memória.)
    }
    procedure SetMemReadFunction(v:Cardinal); virtual;
    {:
    Seta o função do driver para escrita de valores da memória.
    @param(v Cardinal. Função do driver usada para escrita de valores da memória.)
    }
    procedure SetMemWriteFunction(v:Cardinal); virtual;
    {:
    Seta o sub-endereço da memória sendo mapeada.
    @param(v Cardinal. Sub-endereço da memória sendo mapeada.)
    }
    procedure SetMemSubElement(v:Cardinal); virtual;
    {:
    Seta o endereço longo (texto) do tag.
    @param(v String. Endereço longo (texto) do tag.)
    }
    procedure SetPath(v:String); virtual;
    {:
    Seta o endereço do equipamento que contem a memória sendo mapeada.
    @param(v Cardinal. Endereço do equipamento onde está a memória.)
    }
    procedure SetPLCStation(v:Cardinal); virtual;
    {:
    Seta o Hack do equipamento que contem a memória sendo mapeada.
    @param(v Cardinal. Hack do equipamento onde está a memória.)
    }
    procedure SetPLCHack(v:Cardinal); virtual;
    {:
    Seta o Slot do equipamento que contem a memória sendo mapeada.
    @param(v Cardinal. Slot do equipamento onde está a memória.)
    }
    procedure SetPLCSlot(v:Cardinal); virtual;

    {:
    Seta o tempo de varredura (atualização) da memória em milisegundos.
    @param(v Cardinal. Tempo em milisegundos que a memória deve ser atualizada.)
    }
    procedure SetRefreshTime(v:TRefreshTime); virtual;
    {:
    Seta o driver de protocolo usado para a comunicação dessa memória.
    @param(p TProtocolDriver. Componente de protocolo usado para comunicação do tag.)
    }
    procedure SetProtocolDriver(p:TProtocolDriver); virtual;

    //: configura o novo tipo de dado do tag.
    procedure SetTagType(newType:TTagType); virtual;

    //##########################################################################

    //: Procedimento chamado pelo driver de protocolo para atualização de valores do tag.
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); virtual;
    {:
    Compila uma estrutura com as informações do tag.
    @seealso(TTagRec)
    }
    procedure BuildTagRec(out tr:TTagRec; Count, OffSet:Integer);

    //: Faz uma leitura @bold(assincrona) do tag.
    procedure ScanRead; virtual;
    {:
    Escreve valores de maneira @bold(assincrona).
    @param(Values TArrayOfDouble: Array de valores a serem escritos.)
    @param(Count Cardinal: Quantidade de valores a serem escritos.)
    @param(Offset Cardinal: A partir de qual elemento deve comecar a escrita.)
    }
    procedure ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal); virtual; abstract;
    //: Faz uma leitura @bold(sincrona) do valor do tag.
    procedure Read; virtual; abstract;
    {:
    Escreve valores de maneira @bold(sincrona).
    @param(Values TArrayOfDouble: Array de valores a serem escritos.)
    @param(Count Cardinal: Quantidade de valores a serem escritos.)
    @param(Offset Cardinal: A partir de qual elemento deve comecar a escrita.)
    }
    procedure Write(Values:TArrayOfDouble; Count, Offset:Cardinal); virtual; abstract;

    //: @exclude
    procedure Loaded; override;

    //: @seealso(TTag.AutoRead)
    property AutoRead write SetAutoRead default true;
    //: @seealso(TTag.AutoWrite)
    property AutoWrite write SetAutoWrite default true;
    //: @seealso(TTag.CommReadErrors)
    property CommReadErrors default 0;
    //: @seealso(TTag.CommReadsOK)
    property CommReadsOK nodefault;
    //: @seealso(TTag.CommWriteErrors)
    property CommWriteErrors default 0;
    //: @seealso(TTag.CommWritesOK)
    property CommWritesOk nodefault;
    //: @seealso(TTag.PLCHack)
    property PLCHack write SetPLCHack nodefault;
    //: @seealso(TTag.PLCSlot)
    property PLCSlot write SetPLCSlot nodefault;
    //: @seealso(TTag.PLCStation)
    property PLCStation write SetPLCStation nodefault;
    //: @seealso(TTag.MemFile_DB)
    property MemFile_DB write SetMemFileDB nodefault;
    //: @seealso(TTag.MemAddress)
    property MemAddress write SetMemAddress nodefault;
    //: @seealso(TTag.MemSubElement)
    property MemSubElement write SetMemSubElement nodefault;
    //: @seealso(TTag.MemReadFunction)
    property MemReadFunction write SetMemReadFunction nodefault;
    //: @seealso(TTag.MemWriteFunction)
    property MemWriteFunction write SetMemWriteFunction nodefault;
    //: @seealso(TTag.Retries)
    property Retries write PRetries default 1;
    //: @seealso(TTag.RefreshTime)
    property RefreshTime write SetRefreshTime default 1000;
    //: @seealso(TTag.Size)
    property Size nodefault;
    //: @seealso(TTag.LongAddress)
    property LongAddress write SetPath nodefault;
    {:
    Driver de protocolo usado para comunicação do mapeamento de memória.
    @seealso(TProtocolDriver)
    }
    property ProtocolDriver:TProtocolDriver read PProtocolDriver write SetProtocolDriver;
    //: Data/Hora em que o valor do tag foi atualizado.
    property ValueTimestamp:TDateTime read PValueTimeStamp;
    //: A escrita do tag deve ser sincrona
    property SyncWrites:Boolean read FSyncWrites write FSyncWrites default false ;

    //: Tipo do tag.
    property TagType:TTagType read FTagType write SetTagType default pttDefault;
    //: Diz se as words da palavra formada serão invertidas.
    property SwapBytes:Boolean read FSwapBytes write SetSwapBytes default false;
    //: Diz se os bytes das words da palavra formada serão invertidas.
    property SwapWords:Boolean read FSwapWords write SetSwapWords default false;
    //: Informa ao driver o tamanho real do tag.
    property TagSizeOnProtocol:Integer read GetTagSizeOnProtocol;
    //: Informa a média de milisegundos que o tag está sendo atualizado.
    property AvgUpdateRate:Double read GetAvgUpdateRate;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;
    {:
    Método chamado pelo driver de protocolo que elimina referências a ele.
    }
    procedure RemoveDriver;
  published
    {:
    Exibe o GUID do tag. Somente leitura.
    }
    property TagGUID:String read PGUID write SetGUID;
    {:
    Resultado da última leitura @bold(sincrona) realizada pelo tag.
    @seealso(TProtocolIOResult)
    }
    property LastSyncReadStatus:TProtocolIOResult Read PLastSyncReadCmdResult;
    {:
    Resultado da última escrita @bold(sincrona) realizada pelo tag.
    @seealso(TProtocolIOResult)
    }
    property LastSyncWriteStatus:TProtocolIOResult Read PLastSyncWriteCmdResult;
    {:
    Resultado da última leitura @bold(assincrona) realizada pelo tag.
    @seealso(TProtocolIOResult)
    }
    property LastASyncReadStatus:TProtocolIOResult Read PLastASyncReadCmdResult;
    {:
    Resultado da última escrita @bold(assincrona) realizada pelo tag.
    @seealso(TProtocolIOResult)
    }
    property LastASyncWriteStatus:TProtocolIOResult Read PLastASyncWriteCmdResult;
  end;

  TManagedTags = array of TPLCTag;
  TTagMananger=class
  private
    ftags:TManagedTags;
  public
    constructor Create;
    destructor Destroy;
    procedure AddTag(Tag:TPLCTag);
    procedure RemoveTag(Tag:TPLCTag);
  end;

  function GetTagManager:TTagMananger;

implementation

uses hsutils, hsstrings, dateutils;

constructor TPLCTag.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PValueTimeStamp:=Now;
  PAutoRead:=true;
  PAutoWrite:=true;
  PValidTag:=false;
  PCommReadErrors:=0;
  PCommReadOK:=0;
  PCommWriteErrors:=0;
  PCommWriteOk:=0;
  PHack:=0;
  PSlot:=0;
  PStation:=0;
  PFile_DB:=0;
  PAddress:=0;
  PSubElement:=0;
  PSize:=1;
  PPath:='';
  PReadFunction:=0;
  PWriteFunction:=0;
  PRetries:=1;
  PScanTime:=1000;
  FTagType:=pttDefault;
  FSwapBytes:=false;
  FSwapWords:=false;
  FCurrentWordSize:=1;
  FProtocolWordSize:=1;
  FFirtsRead:=true;
  FTotalTime:=0;
  PProtocolDriver:=nil;
  FTagManager := GetTagManager;
  SetLength(FRawProtocolValues,1);
end;

destructor TPLCTag.Destroy;
begin
  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(self);
  PProtocolDriver := nil;
  (FTagManager as TTagMananger).RemoveTag(Self);
  inherited Destroy;
end;

procedure TPLCTag.RemoveDriver;
begin
  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(self);
  PProtocolDriver := nil;
end;

procedure TPLCTag.SetProtocolDriver(p:TProtocolDriver);
begin
  //estou carregando meus parametros...
  if ([csReading,csLoading]*ComponentState<>[]) then begin
    FProtocoloOnLoading:=p;
    Exit;
  end;

  //estou em tempo de desenvolvimento...
  //if (csDesigning in ComponentState) then begin
  //  PProtocolDriver := p;
  //  GetNewProtocolTagSize;
  //  exit;
  //end;

  if p=PProtocolDriver then exit;

  //remove o driver antigo.
  if (PProtocolDriver<>nil) then begin
    //remove do scan do driver...
    if PAutoRead then
      PProtocolDriver.RemoveTag(self);
    PProtocolDriver := nil;
  end;

  //seta o novo driver.
  if (p<>nil) then begin
    //adiciona no scan do driver...
    PProtocolDriver := p;
    GetNewProtocolTagSize;

    if Self.PAutoRead then
      P.AddTag(self);
  end;
end;

procedure TPLCTag.TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer);
var
  c, poffset:Integer;
begin
  if (not FFirtsRead) and (TagCommand =tcScanRead) and (LastResult=ioOk) and (ValuesTimeStamp<>PValueTimeStamp) then begin
    inc(FTotalTime, MilliSecondsBetween(ValuesTimeStamp,PValueTimeStamp));
    inc(FReadCount);
  end;

  if TagCommand=tcScanRead then
    PLastScanTimeStamp:=Now;

  if (LastResult=ioOk) then
    FFirtsRead:=false;

  if LastResult in [ioOk, ioNullDriver] then begin
    if FCurrentWordSize>=FProtocolWordSize then begin
      poffset := (FCurrentWordSize div FProtocolWordSize)*offset
    end else begin
      poffset := (OffSet * FCurrentWordSize) div FProtocolWordSize;
    end;

    for c := 0 to High(Values) do
      if (c+poffset)<=High(FRawProtocolValues) then
        FRawProtocolValues[c+poffset]:=Values[c];
  end;
end;

procedure TPLCTag.SetAutoRead(v:Boolean);
begin
  if PAutoRead=v then exit;

  PAutoRead := v;

  if (PProtocolDriver<>nil) then begin
    if v then begin
      PLastScanTimeStamp:=Now;
      PProtocolDriver.AddTag(self)
    end else
      PProtocolDriver.RemoveTag(self);
  end;
end;

procedure TPLCTag.SetAutoWrite(v:Boolean);
begin
  PAutoWrite := v;
end;

procedure TPLCTag.SetPLCHack(v:Cardinal);
begin
  if PHack=v then exit;

  if (PProtocolDriver<>nil) and PAutoRead then
    PProtocolDriver.RemoveTag(self);

  PHack := v;

  if (PProtocolDriver<>nil) and PAutoRead then
    PProtocolDriver.AddTag(self);

  if ([csReading,csLoading]*ComponentState=[]) then
    GetNewProtocolTagSize;
end;

procedure TPLCTag.SetPLCSlot(v:Cardinal);
begin
  if PSlot=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  PSlot := v;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);

  if ([csReading,csLoading]*ComponentState=[]) then
    GetNewProtocolTagSize;
end;

procedure TPLCTag.SetPLCStation(v:Cardinal);
begin
  if PStation=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(self);

  PStation := v;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(self);

  if ([csReading,csLoading]*ComponentState=[]) then
    GetNewProtocolTagSize;
end;

procedure TPLCTag.SetMemFileDB(v:Cardinal);
begin
  if PFile_DB=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  PFile_DB := v;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);

  if ([csReading,csLoading]*ComponentState=[]) then
    GetNewProtocolTagSize;
end;

procedure TPLCTag.SetMemAddress(v:Cardinal);
begin
  if PAddress=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  PAddress := v;

  if ([csReading,csLoading]*ComponentState=[]) then
    GetNewProtocolTagSize;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);
end;

procedure TPLCTag.SetMemSubElement(v:Cardinal);
begin
  if PSubElement=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  PSubElement := v;

  if ([csReading,csLoading]*ComponentState=[]) then
    GetNewProtocolTagSize;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);
end;

procedure TPLCTag.SetMemReadFunction(v:Cardinal);
begin
  if PReadFunction=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  PReadFunction := v;

  if ([csReading,csLoading]*ComponentState=[]) then
    GetNewProtocolTagSize;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);
end;

procedure TPLCTag.SetMemWriteFunction(v:Cardinal);
begin
  if PWriteFunction=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  PWriteFunction := v;

  if ([csReading,csLoading]*ComponentState=[]) then
    GetNewProtocolTagSize;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);
end;

procedure TPLCTag.SetPath(v:String);
begin
  if PPath=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  PPath := v;

  if ([csReading,csLoading]*ComponentState=[]) then
    GetNewProtocolTagSize;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);
end;

procedure TPLCTag.SetRefreshTime(v:TRefreshTime);
begin
  if PScanTime=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  PScanTime := v;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);
end;

procedure TPLCTag.BuildTagRec(out tr:TTagRec; Count, OffSet:Integer);
begin
  tr.Hack := PHack;
  tr.Slot := PSlot;
  tr.Station := PStation;
  tr.File_DB := PFile_DB;
  tr.Address := PAddress;
  tr.SubElement := PSubElement;
  Count := ifthen(Count=0, PSize, Count);

  //calcula o tamanho real e o offset de acordo com
  //o tipo de tag e tamanho da palavra de dados
  //que está chegando do protocolo...
  if FCurrentWordSize>=FProtocolWordSize then begin
    tr.Size   := (FCurrentWordSize div FProtocolWordSize)*Count;
    tr.OffSet := (FCurrentWordSize div FProtocolWordSize)*offset
  end else begin
    tr.OffSet := (OffSet * FCurrentWordSize) div FProtocolWordSize;
    tr.Size   := (((OffSet*FCurrentWordSize)+(Count*FCurrentWordSize)) div FProtocolWordSize) + ifthen((((OffSet*FCurrentWordSize)+(Count*FCurrentWordSize)) mod FProtocolWordSize)<>0,1,0) - tr.OffSet;
  end;

  tr.RealOffset:=OffSet;

  tr.Path := PPath;
  tr.ReadFunction := PReadFunction;
  tr.WriteFunction := PWriteFunction;
  tr.Retries := PRetries;
  tr.ScanTime := PScanTime;
  tr.CallBack := TagCommandCallBack;
end;

procedure TPLCTag.ScanRead;
begin

end;

procedure TPLCTag.GetNewProtocolTagSize;
begin
  if PProtocolDriver=nil then begin
    FProtocolWordSize:=1;
    exit;
  end;

  FProtocolWordSize:=PProtocolDriver.SizeOfTag(Self,False,FProtocolTagType);
  if FTagType=pttDefault then
    FCurrentWordSize := FProtocolWordSize;

  UpdateTagSizeOnProtocol;
end;

function  TPLCTag.GetTagSizeOnProtocol:Integer;
begin
  Result := Length(FRawProtocolValues);
end;

procedure TPLCTag.RebuildTagGUID;
var
  x:TGuid;
begin
  CreateGUID(x);
  PGUID:=GUIDToString(x);
end;

procedure TPLCTag.Loaded;
var
  olddriver:TProtocolDriver;
begin
  inherited Loaded;

  ProtocolDriver:=FProtocoloOnLoading;

  if PProtocolDriver=nil then begin
    olddriver:=PProtocolDriver;
    PProtocolDriver:=TProtocolDriver(1);
    FCurrentWordSize:=FProtocolWordSize;
    UpdateTagSizeOnProtocol;
    PProtocolDriver:=olddriver;
  end else begin
    UpdateTagSizeOnProtocol;
  end;

  with FTagManager as TTagMananger do
    AddTag(Self);
end;

procedure TPLCTag.SetGUID(v:String);
begin
  if ComponentState*[csReading]=[] then exit;
  PGUID:=v;
end;

procedure TPLCTag.SetTagType(newType:TTagType);
begin
  if newType=FTagType then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  FTagType:=newType;

  case FTagType of
    pttDefault:
      FCurrentWordSize := FProtocolWordSize;
    pttShortInt, pttByte:
      FCurrentWordSize:=8;
    pttSmallInt, pttWord:
      FCurrentWordSize:=16;
    pttInteger, pttDWord, pttFloat:
      FCurrentWordSize:=32;
  end;

  if [csReading,csLoading]*ComponentState=[] then begin
    UpdateTagSizeOnProtocol;
    RebuildValues;
  end;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);
end;

procedure TPLCTag.UpdateTagSizeOnProtocol;
var
  Tamanho:Integer;
begin
  if PProtocolDriver=nil then begin
    exit;
  end;

  if FCurrentWordSize>=FProtocolWordSize then begin
    Tamanho := (FCurrentWordSize div FProtocolWordSize)*PSize;
  end else begin
    Tamanho := ((PSize*FCurrentWordSize) div FProtocolWordSize) + ifthen(((PSize*FCurrentWordSize) mod FProtocolWordSize)<>0,1,0);
  end;

  SetLength(FRawProtocolValues, Tamanho);
end;

procedure TPLCTag.SetSwapWords(v:Boolean);
begin
  if v=FSwapWords then exit;

  FSwapWords:=v;
  RebuildValues;
end;

procedure TPLCTag.SetSwapBytes(v:Boolean);
begin
  if v=FSwapBytes then exit;

  FSwapBytes:=v;
  RebuildValues;
end;

procedure TPLCTag.RebuildValues;
begin
  TagCommandCallBack(FRawProtocolValues,ValueTimestamp,tcInternalUpdate,ioOk,0);
end;

function TPLCTag.PLCValuesToTagValues(Values:TArrayOfDouble; Offset:Cardinal):TArrayOfDouble;
var
  PtrByte, PtrByteWalker:PByte;
  PtrWordWalker:PWord;
  PtrDWordWalker:PDWord;

  AreaSize:Integer;
  AreaIdx:Integer;
  valueidx:Integer;

  WordAux:Word;
  ByteAux:Byte;

  PtrByte1, PtrByte2:PByte;
  PtrWord1, PtrWord2:PWord;

  procedure ResetPointers;
  begin
    PtrByteWalker  :=PtrByte;
    PtrWordWalker  :=PWord(PtrByte);
    PtrDWordWalker :=PDWord(PtrByte);
  end;

  procedure AddToResult(ValueToAdd:Double; var Result:TArrayOfDouble);
  var
    i:Integer;
  begin
    i:=Length(Result);
    SetLength(Result,i+1);
    Result[i]:=ValueToAdd;
  end;

begin
  if (FTagType=pttDefault) OR
     ((FProtocolTagType=ptByte) AND (FTagType=pttByte)) OR
     ((FProtocolTagType=ptShortInt) AND (FTagType=pttShortInt)) OR
     ((FProtocolTagType=ptWord) AND (FTagType=pttWord)) OR
     ((FProtocolTagType=ptSmallInt) AND (FTagType=pttSmallInt)) OR
     ((FProtocolTagType=ptDWord) AND (FTagType=pttDWord)) OR
     ((FProtocolTagType=ptInteger) AND (FTagType=pttInteger)) OR
     ((FProtocolTagType=ptFloat) AND (FTagType=pttFloat))
  then begin
    Result:=Values;
    exit;
  end;

  //calcula quantos bytes precisam ser alocados.
  SetLength(Result,0);

  case FProtocolTagType of
    ptBit:
      AreaSize := Length(Values) div 8;
    ptByte, ptShortInt:
      AreaSize := Length(Values);
    ptWord, ptSmallInt:
      AreaSize := Length(Values)*2;
    ptDWord, ptInteger, ptFloat:
      AreaSize := Length(Values)*4;
  end;

  GetMem(PtrByte, AreaSize);
  ResetPointers;

  //move os dados para area de trabalho.
  valueidx:=0;
  case FProtocolTagType of
    ptBit:
       while valueidx<Length(Values) do begin
         if Values[valueidx]<>0 then
           PtrByteWalker^:=PtrByteWalker^ + (power(2,valueidx mod 8) AND $FF);

         inc(valueidx);
         if (valueidx mod 8)=0 then
           inc(PtrByteWalker);
       end;
    ptByte, ptShortInt:
       while valueidx<Length(Values) do begin
         PtrByteWalker^:=trunc(Values[valueidx]) AND $FF;
         inc(valueidx);
         Inc(PtrByteWalker);
       end;
    ptWord, ptSmallInt:
       while valueidx<Length(Values) do begin
         PtrWordWalker^:=trunc(Values[valueidx]) AND $FFFF;
         inc(valueidx);
         Inc(PtrWordWalker);
       end;
    ptDWord, ptInteger, ptFloat:
       while valueidx<Length(Values) do begin
         if FProtocolTagType=ptFloat then
           PSingle(PtrDWordWalker)^:=Values[valueidx]
         else
           PtrDWordWalker^:=trunc(Values[valueidx]) AND $FFFFFFFF;

         inc(valueidx);
         Inc(PtrDWordWalker);
       end;
  end;

  ResetPointers;
  AreaIdx:=0;

  //faz as inversoes caso necessário e move os dados para o resultado
  case FTagType of
    pttShortInt, pttByte: begin
      inc(PtrByteWalker,((Offset*FCurrentWordSize) mod FProtocolWordSize) div FCurrentWordSize);
      inc(AreaIdx,(((Offset*FCurrentWordSize) mod FProtocolWordSize) div FCurrentWordSize));
      while AreaIdx<AreaSize do begin
        if FTagType=pttShortInt then
          AddToResult(PShortInt(PtrByteWalker)^, Result)
        else
          AddToResult(PtrByteWalker^, Result);
        inc(AreaIdx);
        inc(PtrByteWalker);
      end;
    end;
    pttSmallInt, pttWord: begin
      inc(PtrWordWalker,((Offset*FCurrentWordSize) mod FProtocolWordSize) div FCurrentWordSize);
      inc(AreaIdx,(((Offset*FCurrentWordSize) mod FProtocolWordSize) div FCurrentWordSize)*2);
      while AreaIdx<AreaSize do begin
        if FSwapBytes then begin
          PtrByte1:=PByte(PtrWordWalker);
          PtrByte2:=PtrByte1;
          inc(PtrByte2);
          ByteAux:=PtrByte1^;
          PtrByte1^:=PtrByte2^;
          PtrByte2^:=ByteAux;
        end;
        if FTagType=pttSmallInt then
          AddToResult(PSmallInt(PtrWordWalker)^, Result)
        else
          AddToResult(PtrWordWalker^, Result);

        inc(AreaIdx, 2);
        inc(PtrWordWalker);
      end;
    end;
    pttInteger,
    pttDWord,
    pttFloat: begin
      inc(PtrDWordWalker,((Offset*FCurrentWordSize) mod FProtocolWordSize) div FCurrentWordSize);
      inc(AreaIdx,       (((Offset*FCurrentWordSize) mod FProtocolWordSize) div FCurrentWordSize)*4);
      while AreaIdx<AreaSize do begin

        if FSwapWords or FSwapBytes then begin
          PtrWord1:=PWord(PtrDWordWalker);
          PtrWord2:=PtrWord1;
          inc(PtrWord2);
        end;

        if FSwapWords then begin
          WordAux:=PtrWord1^;
          PtrWord1^:=PtrWord2^;
          PtrWord2^:=WordAux;
        end;

        if FSwapBytes then begin
          PtrByte1:=PByte(PtrWord1);
          PtrByte2:=PtrByte1;
          inc(PtrByte2);
          ByteAux:=PtrByte1^;
          PtrByte1^:=PtrByte2^;
          PtrByte2^:=ByteAux;

          PtrByte1:=PByte(PtrWord2);
          PtrByte2:=PtrByte1;
          inc(PtrByte2);
          ByteAux:=PtrByte1^;
          PtrByte1^:=PtrByte2^;
          PtrByte2^:=ByteAux;
        end;

        case FTagType of
          pttDWord:
            AddToResult(PtrDWordWalker^, Result);
          pttInteger:
            AddToResult(PInteger(PtrDWordWalker)^, Result);
          pttFloat: begin
            if IsNan(PSingle(PtrDWordWalker)^) or IsInfinite(PSingle(PtrDWordWalker)^) then
              SetExceptionMask([exInvalidOp, exDenormalized, {exZeroDivide,} exOverflow, exUnderflow, exPrecision]);

            AddToResult(PSingle(PtrDWordWalker)^, Result);
          end;
        end;
        inc(AreaIdx, 4);
        inc(PtrDWordWalker);
      end;
    end;
  end;
  Freemem(PtrByte);
end;

function TPLCTag.TagValuesToPLCValues(Values:TArrayOfDouble; Offset:Cardinal):TArrayOfDouble;
var
  PtrByte, PtrByteWalker:PByte;
  PtrWordWalker:PWord;
  PtrDWordWalker:PDWord;

  AreaSize:Integer;
  AreaIdx:Integer;
  valueidx:Integer;

  WordAux:Word;
  ByteAux:Byte;

  PtrByte1, PtrByte2:PByte;
  PtrWord1, PtrWord2:PWord;

  bitaux:Integer;

  ProtocolOffSet, ProtocolSize:Integer;

  procedure ResetPointers;
  begin
    PtrByteWalker  :=PtrByte;
    PtrWordWalker  :=PWord(PtrByte);
    PtrDWordWalker :=PDWord(PtrByte);
  end;

  procedure AddToResult(ValueToAdd:Double; var Result:TArrayOfDouble);
  var
    i:Integer;
  begin
    i:=Length(Result);
    SetLength(Result,i+1);
    Result[i]:=ValueToAdd;
  end;

begin
  if (FTagType=pttDefault) OR
     ((FProtocolTagType=ptByte) AND (FTagType=pttByte)) OR
     ((FProtocolTagType=ptShortInt) AND (FTagType=pttShortInt)) OR
     ((FProtocolTagType=ptWord) AND (FTagType=pttWord)) OR
     ((FProtocolTagType=ptSmallInt) AND (FTagType=pttSmallInt)) OR
     ((FProtocolTagType=ptDWord) AND (FTagType=pttDWord)) OR
     ((FProtocolTagType=ptInteger) AND (FTagType=pttInteger)) OR
     ((FProtocolTagType=ptFloat) AND (FTagType=pttFloat))
  then begin
    Result:=Values;
    exit;
  end;

  //calcula quantos bytes precisam ser alocados.
  SetLength(Result,0);

  if FCurrentWordSize>=FProtocolWordSize then begin
    ProtocolSize   := (FCurrentWordSize div FProtocolWordSize)*Length(Values);
    ProtocolOffSet := (FCurrentWordSize div FProtocolWordSize)*Offset
  end else begin
    ProtocolOffSet := (OffSet * FCurrentWordSize) div FProtocolWordSize;
    ProtocolSize   := (((OffSet*FCurrentWordSize)+(Length(Values)*FCurrentWordSize)) div FProtocolWordSize) + ifthen((((OffSet*FCurrentWordSize)+(Length(Values)*FCurrentWordSize)) mod FProtocolWordSize)<>0,1,0) - ProtocolOffSet;
  end;

  case FProtocolTagType of
    ptBit:
      AreaSize := ProtocolSize div 8;
    ptByte, ptShortInt:
      AreaSize := ProtocolSize;
    ptWord, ptSmallInt:
      AreaSize := ProtocolSize*2;
    ptDWord, ptInteger, ptFloat:
      AreaSize := ProtocolSize*4;
  end;

  GetMem(PtrByte, AreaSize);
  ResetPointers;

  //joga os valores puros lidos do driver para a area de memória
  //para nao perder valores.
  valueidx:=0;
  case FProtocolTagType of
    ptByte, ptShortInt:
       while valueidx<ProtocolSize do begin
         PtrByteWalker^:=trunc(FRawProtocolValues[valueidx+ProtocolOffSet]) AND $FF;
         inc(valueidx);
         Inc(PtrByteWalker);
       end;
    ptWord, ptSmallInt:
       while valueidx<ProtocolSize do begin
         PtrWordWalker^:=trunc(FRawProtocolValues[valueidx+ProtocolOffSet]) AND $FFFF;
         inc(valueidx);
         Inc(PtrWordWalker);
       end;
    ptDWord, ptInteger, ptFloat:
       while valueidx<ProtocolSize do begin
         if FProtocolTagType = ptFloat then
           PSingle(PtrDWordWalker)^:=FRawProtocolValues[valueidx+ProtocolOffSet]
         else begin
           if FProtocolTagType = ptInteger then
             PInteger(PtrDWordWalker)^:=trunc(FRawProtocolValues[valueidx+ProtocolOffSet])
           else
             PtrDWordWalker^:=trunc(FRawProtocolValues[valueidx+ProtocolOffSet]) AND $FFFFFFFF;
         end;
         inc(valueidx);
         Inc(PtrDWordWalker);
       end;
  end;
  ResetPointers;

  //move os dados para area de trabalho.
  valueidx:=0;
  case FTagType of
    pttByte, pttShortInt: begin
       inc(PtrByteWalker,((Offset*FCurrentWordSize) mod FProtocolWordSize) div FCurrentWordSize);
       while valueidx<Length(Values) do begin
         PtrByteWalker^:=trunc(Values[valueidx]) AND $FF;
         inc(valueidx);
         Inc(PtrByteWalker);
       end;
    end;
    pttWord, pttSmallInt: begin
       inc(PtrWordWalker,((Offset*FCurrentWordSize) mod FProtocolWordSize) div FCurrentWordSize);
       while valueidx<Length(Values) do begin
         PtrWordWalker^:=trunc(Values[valueidx]) AND $FFFF;

         if FSwapBytes then begin
           PtrByte1:=PByte(PtrWordWalker);
           PtrByte2:=PtrByte1;
           inc(PtrByte2);
           ByteAux:=PtrByte1^;
           PtrByte1^:=PtrByte2^;
           PtrByte2^:=ByteAux;
         end;

         inc(valueidx);
         Inc(PtrWordWalker);
       end;
    end;
    pttDWord,
    pttInteger,
    pttFloat: begin
       inc(PtrDWordWalker,((Offset*FCurrentWordSize) mod FProtocolWordSize) div FCurrentWordSize);
       while valueidx<Length(Values) do begin

         if FTagType=pttInteger then
           PInteger(PtrDWordWalker)^:=trunc(Values[valueidx]);
         if FTagType=pttDWord then
           PtrDWordWalker^:=trunc(Values[valueidx]) AND $FFFFFFFF;
         if FTagType=pttFloat then
           PSingle(PtrDWordWalker)^:=Values[valueidx];

         if FSwapWords or FSwapBytes then begin
           PtrWord1:=PWord(PtrDWordWalker);
           PtrWord2:=PtrWord1;
           inc(PtrWord2);
         end;

         if FSwapWords then begin
           WordAux:=PtrWord1^;
           PtrWord1^:=PtrWord2^;
           PtrWord2^:=WordAux;
         end;

         if FSwapBytes then begin
           PtrByte1:=PByte(PtrWord1);
           PtrByte2:=PtrByte1;
           inc(PtrByte2);
           ByteAux:=PtrByte1^;
           PtrByte1^:=PtrByte2^;
           PtrByte2^:=ByteAux;

           PtrByte1:=PByte(PtrWord2);
           PtrByte2:=PtrByte1;
           inc(PtrByte2);
           ByteAux:=PtrByte1^;
           PtrByte1^:=PtrByte2^;
           PtrByte2^:=ByteAux;
         end;

         inc(valueidx);
         Inc(PtrDWordWalker);
       end;
    end;
  end;

  ResetPointers;
  AreaIdx:=0;
  //faz as inversoes e move para o resultado.
  case FProtocolTagType of
    ptBit: begin
       while AreaIdx<AreaSize do begin
         bitaux := Power(2,AreaIdx mod 8);
         if (PtrByteWalker^ AND bitaux)=bitaux then
           AddToResult(1, Result)
         else
           AddToResult(0, Result);

         inc(AreaIdx);

         if (AreaIdx mod 8)=0 then
           inc(PtrByteWalker);
       end;
    end;
    ptByte,
    ptShortInt: begin
      while AreaIdx<AreaSize do begin
        if FProtocolTagType=ptShortInt then
         AddToResult(PShortInt(PtrByteWalker)^, Result)
        else
          AddToResult(PtrByteWalker^, Result);
        inc(AreaIdx);
        inc(PtrByteWalker);
      end;
    end;
    ptSmallInt,
    ptWord: begin
      while AreaIdx<AreaSize do begin
        if FProtocolTagType=ptSmallInt then
          AddToResult(PSmallInt(PtrWordWalker)^, Result)
        else
          AddToResult(PtrWordWalker^, Result);

        inc(AreaIdx, 2);
        inc(PtrWordWalker);
      end;
    end;
    ptInteger,
    ptDWord,
    ptFloat: begin
      while AreaIdx<AreaSize do begin
        case FProtocolTagType of
          ptDWord:
            AddToResult(PtrDWordWalker^, Result);
          ptInteger:
            AddToResult(PInteger(PtrDWordWalker)^, Result);
          ptFloat:
            AddToResult(PSingle(PtrDWordWalker)^, Result);
        end;
        inc(AreaIdx, 4);
        inc(PtrDWordWalker);
      end;
    end;
  end;
  Freemem(PtrByte);
end;

function TPLCTag.GetAvgUpdateRate:Double;
begin
  if FReadCount=0 then
    Result:=-1
  else
    Result:=FTotalTime/FReadCount;
end;

function TPLCTag.RemainingMiliseconds:Int64;
begin
  Result:=PScanTime-MilliSecondsBetween(Now,PValueTimeStamp);
end;

function TPLCTag.RemainingMilisecondsForNextScan:Int64;
begin
  Result:=PScanTime-MilliSecondsBetween(Now,PLastScanTimeStamp);
end;

function TPLCTag.IsValidTag:Boolean;
begin
  Result:=PValidTag;
end;

procedure TPLCTag.SetTagValidity(TagValidity:Boolean);
begin
  PValidTag:=TagValidity;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TTagMananger.Create;
begin
  SetLength(ftags,0);
end;

destructor TTagMananger.Destroy;
begin
  if Length(ftags)>0 then
    Raise Exception.Create(SCannotDestroyBecauseTagsStillManaged);
end;

procedure  TTagMananger.AddTag(Tag:TPLCTag);
var
  c,h:Integer;
begin
  for c:=0 to High(ftags) do begin
    if ftags[c]=Tag then exit;
    if ftags[c].TagGUID=tag.TagGUID then begin
      if Supports(Tag, IManagedTagInterface) then
        (Tag as IManagedTagInterface).RebuildTagGUID
      else
        raise Exception.Create(SCannotRebuildTagID);
    end;
  end;
  h:=Length(ftags);
  SetLength(ftags,h+1);
  ftags[h]:=Tag;
end;

procedure  TTagMananger.RemoveTag(Tag:TPLCTag);
var
  c,h:Integer;
  found:Boolean;
begin
  found:=false;
  for c:=0 to High(ftags) do
    if ftags[c]=Tag then begin
      found:=true;
      break;
    end;

  if found then begin
    h:=High(ftags);
    ftags[c]:=ftags[h];
    SetLength(ftags,Max(0,h-1));
  end;
end;

var
  QPascalTagManager:TTagMananger;

function GetTagManager:TTagMananger;
begin
  Result:=QPascalTagManager;
end;

initialization
  QPascalTagManager:=TTagMananger.Create;
finalization
  QPascalTagManager.Destroy;
end.
