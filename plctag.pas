{$IFDEF PORTUGUES}
{:
@abstract(Implementa a base para Tags de comunicação.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
@abstract(Unit that implements the base of an communication tag.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ENDIF}
unit PLCTag;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, ExtCtrls, Classes, Tag, ProtocolDriver, ProtocolTypes, Math;

type

  {$IFDEF PORTUGUES}
  {:
  @abstract(Classe base para todos os tags de comunicação.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
  }
  {$ELSE}
  {:
  @abstract(Base class of a communication tag.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
  }
  {$ENDIF}
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
    function  IsMyCallBack(Cback:TTagCommandCallBack):Boolean; virtual;
    procedure GetNewProtocolTagSize;
    function RemainingMiliseconds:Int64; virtual;
    function RemainingMilisecondsForNextScan:Int64; virtual;
    function IsValidTag:Boolean; virtual;
    procedure SetTagValidity(TagValidity:Boolean); virtual;
  protected
    {$IFDEF PORTUGUES}
    //: Referencia ao objeto gerenciador de tags.
    {$ELSE}
    //: Stores the tag manager.
    {$ENDIF}
    FTagManager:TObject;

    {$IFDEF PORTUGUES}
    //: A escrita do tag deve ser sincrona ou assincrona
    {$ELSE}
    //: Tells if the write command will be synchronous or asynchronous.
    {$ENDIF}
    FSyncWrites:Boolean;

    {$IFDEF PORTUGUES}
    //: Armazena o driver de protocolo usado para comunicação do tag.
    {$ELSE}
    //: Stores the protocol driver used by tag.
    {$ENDIF}
    PProtocolDriver:TProtocolDriver;

    {$IFDEF PORTUGUES}
    //: Data/Hora da última tentativa de leitura do valor do tag.
    {$ELSE}
    //: Date/time of the last scan read request of tag.
    {$ENDIF}
    PLastScanTimeStamp:TDateTime;

    {$IFDEF PORTUGUES}
    //: Data/Hora da última atualização do valor do tag.
    {$ELSE}
    //: Date/time of the last update of the tag value.
    {$ENDIF}
    PValueTimeStamp:TDateTime;

    {$IFDEF PORTUGUES}
    //: Armazena o resultado da última leitura @bold(sincrona) realizada pelo tag.
    {$ELSE}
    //: Stores the I/O result of the last synchronous read command done.
    {$ENDIF}
    PLastSyncReadCmdResult:TProtocolIOResult;

    {$IFDEF PORTUGUES}
    //: Armazena o resultado da última escrita @bold(sincrona) realizada pelo tag.
    {$ELSE}
    //: Stores the I/O result of the last synchronous write command done.
    {$ENDIF}
    PLastSyncWriteCmdResult:TProtocolIOResult;

    {$IFDEF PORTUGUES}
    //: Armazena o resultado da última leitura @bold(asincrona) realizada pelo tag.
    {$ELSE}
    //: Stores the I/O result of the last @bold(asynchronous) read command done.
    {$ENDIF}
    PLastASyncReadCmdResult:TProtocolIOResult;

    {$IFDEF PORTUGUES}
    //: Armazena o resultado da última escrita @bold(assincrona) realizada pelo tag.
    {$ELSE}
    //: Stores the I/O result of the last synchronous write command done.
    {$ENDIF}
    PLastASyncWriteCmdResult:TProtocolIOResult;

    {$IFDEF PORTUGUES}
    //: Tipo de dado retornado pelo protocolo.
    {$ELSE}
    //: Stores the datatype returned by the protocol driver.
    {$ENDIF}
    FProtocolTagType:TProtocolTagType;

    {$IFDEF PORTUGUES}
    //: Tipo de dado do tag
    {$ELSE}
    //: Datatype of the tag.
    {$ENDIF}
    FTagType:TTagType;

    {$IFDEF PORTUGUES}
    //: Diz se as words da palavra (integer, cardinal e float) serão invertidas
    {$ELSE}
    //: Tells if the words of an DWORD (integer,cardinal and float) will be swaped.
    {$ENDIF}
    FSwapWords:Boolean;

    {$IFDEF PORTUGUES}
    //: Diz se os bytes da WORD (SmallInt, Word) serão invertidas.
    {$ELSE}
    //: Tells if the bytes of an WORD (SmallInt, Word) will be swaped.
    {$ENDIF}
    FSwapBytes:Boolean;

    {$IFDEF PORTUGUES}
    //: Tamanho da palavra retornada pelo protocolo e em uso pelo tag, em bits.
    {$ELSE}
    //: Word size returned by the protocol and current word size of the tag.
    {$ENDIF}
    FProtocolWordSize,
    FCurrentWordSize:Byte;

    {$IFDEF PORTUGUES}
    //: Valores vindos do PLC são convertidos para o tipo de dados configurado no tag.
    {$ELSE}
    //: Convert values comming from the PLC to the datatype of the tag.
    {$ENDIF}
    function PLCValuesToTagValues(Values:TArrayOfDouble; Offset:Cardinal):TArrayOfDouble; virtual;

    {$IFDEF PORTUGUES}
    //: Valores vindo do tag são convertidos para o tipo de aceito pelo driver.
    {$ELSE}
    //: Convert values of the datatype of the tag to the datatype of the driver.
    {$ENDIF}
    function TagValuesToPLCValues(Values:TArrayOfDouble; Offset:Cardinal):TArrayOfDouble; virtual;

    {$IFDEF PORTUGUES}
    //: Retorna a média de tempo que o tag é atualizado.
    {$ELSE}
    //: Average scan update rate.
    {$ENDIF}
    function GetAvgUpdateRate:Double;

    {$IFDEF PORTUGUES}
    //: Retorna o tamanho real do tag.
    {$ELSE}
    //: Returns the real size of the tag.
    {$ENDIF}
    procedure UpdateTagSizeOnProtocol;

    {$IFDEF PORTUGUES}
    //: Recompila os valores do tag.
    {$ELSE}
    //: Rebuild the tag values.
    {$ENDIF}
    procedure RebuildValues; virtual;

    {$IFDEF PORTUGUES}
    {:
    Habilita/Desabilita o swap de words.
    @param(v Boolean: @true habilita, @false desabilita.)
    }
    {$ELSE}
    {:
    Enable/disables the swap of words.
    @param(v Boolean: @true enables the swap, @false disables.)
    }
    {$ENDIF}
    procedure SetSwapWords(v:Boolean); virtual;

    {$IFDEF PORTUGUES}
    {:
    Habilita/Desabilita o swap de bytes.
    @param(v Boolean: @true habilita, @false desabilita.)
    }
    {$ELSE}
    {:
    Enable/disables the swap of bytes.
    @param(v Boolean: @true enables the swap, @false disables.)
    }
    {$ENDIF}
    procedure SetSwapBytes(v:Boolean); virtual;

    {$IFDEF PORTUGUES}
    //: Define uma identificação unica do tag. Chamada pelo Tag Manager.
    {$ELSE}
    //: Sets a new unique tag identification. Called by the Tag Manager.
    {$ENDIF}
    procedure SetGUID(v:String);

    //##########################################################################

    {$IFDEF PORTUGUES}
    {:
    Habilita/Desabilita a leitura automática do tag.
    @param(v Boolean: @true habilita, @false desabilita.)
    }
    {$ELSE}
    {:
    Enable/disables the automatic tag read.
    @param(v Boolean: @true enables, @false disables (manual).)
    }
    {$ENDIF}
    procedure SetAutoRead(v:Boolean); virtual;

    {$IFDEF PORTUGUES}
    {:
    Habilita/Desabilita a escrita automática de valores do tag.
    @param(v Boolean: @true automatico, @false manual.)
    }
    {$ELSE}
    {:
    Enable/disables the automatic write of values of the tag.
    @param(v Boolean: @true automatic, @false manual.)
    }
    {$ENDIF}
    procedure SetAutoWrite(v:Boolean); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o endereço da memória sendo mapeada.
    @param(v Cardinal. Endereço da memória sendo mapeada.)
    }
    {$ELSE}
    {:
    Sets the memory address.
    @param(v Cardinal. The memory address.)
    }
    {$ENDIF}
    procedure SetMemAddress(v:Cardinal); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o Arquivo/DB que contem a memória sendo mapeada.
    @param(v Cardinal. Arquivo/DB que a memória mapeada pertence.)
    }
    {$ELSE}
    {:
    Sets the File/DB that contains the mapped memory.
    @param(v Cardinal. File/DB number of your memory.)
    }
    {$ENDIF}
    procedure SetMemFileDB(v:Cardinal); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o função do driver para leitura da memória.
    @param(v Cardinal. Função do driver usada para leitura da memória.)
    }
    {$ELSE}
    {:
    Sets the function to be used to read the memory.
    @param(v Cardinal. Function number to read the memory.)
    }
    {$ENDIF}
    procedure SetMemReadFunction(v:Cardinal); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o função do driver para escrita de valores da memória.
    @param(v Cardinal. Função do driver usada para escrita de valores da memória.)
    }
    {$ELSE}
    {:
    Sets the function to be used to write values on memory.
    @param(v Cardinal. Function number to write values on memory.)
    }
    {$ENDIF}
    procedure SetMemWriteFunction(v:Cardinal); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o sub-endereço da memória sendo mapeada.
    @param(v Cardinal. Sub-endereço da memória sendo mapeada.)
    }
    {$ELSE}
    {:
    Sets the sub-element of the memory being mapped.
    @param(v Cardinal. The sub-element number of the memory being mapped.)
    }
    {$ENDIF}
    procedure SetMemSubElement(v:Cardinal); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o endereço longo (texto) do tag.
    @param(v String. Endereço longo (texto) do tag.)
    }
    {$ELSE}
    {:
    Sets the long address (text) of the tag.
    @param(v String. The long address of the tag (text).)
    }
    {$ENDIF}
    procedure SetPath(v:String); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o endereço do equipamento que contem a memória sendo mapeada.
    @param(v Cardinal. Endereço do equipamento onde está a memória.)
    }
    {$ELSE}
    {:
    Sets the address of device being mapped.
    @param(v Cardinal. The device address.)
    }
    {$ENDIF}
    procedure SetPLCStation(v:Cardinal); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o Rack do equipamento que contem a memória sendo mapeada.
    @param(v Cardinal. Hack do equipamento onde está a memória.)
    }
    {$ELSE}
    {:
    Sets the Rack of the device being mapped.
    @param(v Cardinal. The device Rack number.)
    }
    {$ENDIF}
    procedure SetPLCHack(v:Cardinal); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o Slot do equipamento que contem a memória sendo mapeada.
    @param(v Cardinal. Slot do equipamento onde está a memória.)
    }
    {$ELSE}
    {:
    Sets the Slot number of the device being mapped.
    @param(v Cardinal. The Slot number.)
    }
    {$ENDIF}
    procedure SetPLCSlot(v:Cardinal); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o tempo de varredura (atualização) da memória em milisegundos.
    @param(v Cardinal. Tempo em milisegundos que a memória deve ser atualizada.)
    }
    {$ELSE}
    {:
    Sets the scan rate of the tag in milliseconds.
    @param(v Cardinal. Scan rate in milliseconds.)
    }
    {$ENDIF}
    procedure SetRefreshTime(v:TRefreshTime); virtual;

    {$IFDEF PORTUGUES}
    {:
    Seta o driver de protocolo usado para a comunicação dessa memória.
    @param(p TProtocolDriver. Componente de protocolo usado para comunicação do tag.)
    }
    {$ELSE}
    {:
    Sets the protocol driver to be used the read/write values on device.
    @param(p TProtocolDriver. The protocol driver to be used to read/write values of your device.)
    }
    {$ENDIF}
    procedure SetProtocolDriver(p:TProtocolDriver); virtual;

    {$IFDEF PORTUGUES}
    //: Configura o novo tipo de dado do tag. @seealso(TTagType)
    {$ELSE}
    //: Sets the datatype of the tag. @seealso(TTagType)
    {$ENDIF}
    procedure SetTagType(newType:TTagType); virtual;

    //##########################################################################


    {$IFDEF PORTUGUES}
    //: Procedimento chamado pelo driver de protocolo para atualização de valores do tag.
    {$ELSE}
    //: Procedure called by the protocol driver to update tag values.
    {$ENDIF}
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); virtual;

    {$IFDEF PORTUGUES}
    {:
    Compila uma estrutura com as informações do tag.
    @seealso(TTagRec)
    }
    {$ELSE}
    {:
    Returns a structure with all informations about the tag.
    @seealso(TTagRec)
    }
    {$ENDIF}
    procedure BuildTagRec(out tr:TTagRec; Count, OffSet:Integer);

    {$IFDEF PORTUGUES}
    //: Faz uma leitura @bold(assincrona) do tag.
    {$ELSE}
    //: Request a update of tag values.
    {$ENDIF}
    procedure ScanRead; virtual;

    {$IFDEF PORTUGUES}
    {:
    Escreve valores de maneira @bold(assincrona).
    @param(Values TArrayOfDouble: Array de valores a serem escritos.)
    @param(Count Cardinal: Quantidade de valores a serem escritos.)
    @param(Offset Cardinal: A partir de qual elemento deve comecar a escrita.)
    }
    {$ELSE}
    {:
    Write values of the tag on your device @bold(asynchronous).
    @param(Values TArrayOfDouble: Array of values to be written.)
    @param(Count Cardinal: How many values will be written.)
    @param(Offset Cardinal: Tells offset after the address where the values will be written.)
    }
    {$ENDIF}
    procedure ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal); virtual; abstract;

    {$IFDEF PORTUGUES}
    //: Faz uma leitura @bold(sincrona) do valor do tag.
    {$ELSE}
    //: Request a @bold(synchronous) read of the tag value.
    {$ENDIF}
    procedure Read; virtual; abstract;

    {$IFDEF PORTUGUES}
    {:
    Escreve valores de maneira @bold(sincrona).
    @param(Values TArrayOfDouble: Array de valores a serem escritos.)
    @param(Count Cardinal: Quantidade de valores a serem escritos.)
    @param(Offset Cardinal: A partir de qual elemento deve comecar a escrita.)
    }
    {$ELSE}
    {:
    Write values of the tag on your device @bold(synchronous).
    @param(Values TArrayOfDouble: Array of values to be written.)
    @param(Count Cardinal: How many values will be written.)
    @param(Offset Cardinal: Tells offset after the address where the values will be written.)
    }
    {$ENDIF}
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
    property PLCHack write SetPLCHack stored false;
    //: @seealso(TTag.PLCRack)
    property PLCRack write SetPLCHack nodefault;
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
    property RefreshTime write SetRefreshTime stored false;
    //: @seealso(TTag.ScanRate)
    property UpdateTime write SetRefreshTime default 1000;
    //: @seealso(TTag.Size)
    property Size nodefault;
    //: @seealso(TTag.LongAddress)
    property LongAddress write SetPath nodefault;

    {$IFDEF PORTUGUES}
    {:
    Driver de protocolo usado para comunicação do mapeamento de memória.
    @seealso(TProtocolDriver)
    }
    {$ELSE}
    {:
    Protocol driver used by tag to read/write values on your device.
    @seealso(TProtocolDriver)
    }
    {$ENDIF}
    property ProtocolDriver:TProtocolDriver read PProtocolDriver write SetProtocolDriver;

    {$IFDEF PORTUGUES}
    //: Data/Hora em que o valor do tag foi atualizado.
    {$ELSE}
    //: Date/time of the last update of the tag value.
    {$ENDIF}
    property ValueTimestamp:TDateTime read PValueTimeStamp;

    {$IFDEF PORTUGUES}
    //: Caso @true, a escrita de valoes do tag vai ser @bold(sincrona).
    {$ELSE}
    //: If @true, the write of values will be @bold(synchronous).
    {$ENDIF}
    property SyncWrites:Boolean read FSyncWrites write FSyncWrites default false ;

    {$IFDEF PORTUGUES}
    //: Tipo do tag.
    {$ELSE}
    //: Datatype of the tag.
    {$ENDIF}
    property TagType:TTagType read FTagType write SetTagType default pttDefault;

    {$IFDEF PORTUGUES}
    //: Diz se os bytes da WORD (SmallInt, Word) serão invertidas.
    {$ELSE}
    //: Tells if the bytes of an WORD (SmallInt, Word) will be swaped.
    {$ENDIF}
    property SwapBytes:Boolean read FSwapBytes write SetSwapBytes default false;

    {$IFDEF PORTUGUES}
    //: Diz se as words da palavra formada serão invertidas.
    {$ELSE}
    //: Tells if the words of an DWORD (integer, cardinal and float) will be swaped.
    {$ENDIF}
    property SwapWords:Boolean read FSwapWords write SetSwapWords default false;

    {$IFDEF PORTUGUES}
    //: Informa ao driver o tamanho real do tag no driver de protocolo.
    {$ELSE}
    //: Tells the real size of the tag on protocol driver.
    {$ENDIF}
    property TagSizeOnProtocol:Integer read GetTagSizeOnProtocol;

    {$IFDEF PORTUGUES}
    //: Informa a média de milisegundos que o tag está sendo atualizado.
    {$ELSE}
    //: Average update rate of the tag.
    {$ENDIF}
    property AvgUpdateRate:Double read GetAvgUpdateRate;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor Destroy; override;

    {$IFDEF PORTUGUES}
    {:
    Método chamado pelo driver de protocolo que elimina referências a ele.
    }
    {$ELSE}
    //: Called when the protocol driver is being destroyed.
    {$ENDIF}
    procedure RemoveDriver;
  published

    {$IFDEF PORTUGUES}
    {:
    Exibe o GUID do tag. Somente leitura.
    }
    {$ELSE}
    //: Tells the unique tag identification.
    {$ENDIF}
    property TagGUID:String read PGUID write SetGUID;

    {$IFDEF PORTUGUES}
    {:
    Resultado da última leitura @bold(sincrona) realizada pelo tag.
    @seealso(TProtocolIOResult)
    }
    {$ELSE}
    {:
    I/O result of the last @bold(synchronous) read done.
    @seealso(TProtocolIOResult)
    }
    {$ENDIF}
    property LastSyncReadStatus:TProtocolIOResult Read PLastSyncReadCmdResult;

    {$IFDEF PORTUGUES}
    {:
    Resultado da última escrita @bold(sincrona) realizada pelo tag.
    @seealso(TProtocolIOResult)
    }
    {$ELSE}
    {:
    I/O result of the last @bold(synchronous) write done.
    @seealso(TProtocolIOResult)
    }
    {$ENDIF}
    property LastSyncWriteStatus:TProtocolIOResult Read PLastSyncWriteCmdResult;

    {$IFDEF PORTUGUES}
    {:
    Resultado da última leitura @bold(asincrona) realizada pelo tag.
    @seealso(TProtocolIOResult)
    }
    {$ELSE}
    {:
    I/O result of the last @bold(asynchronous) read done.
    @seealso(TProtocolIOResult)
    }
    {$ENDIF}
    property LastASyncReadStatus:TProtocolIOResult Read PLastASyncReadCmdResult;

    {$IFDEF PORTUGUES}
    {:
    Resultado da última escrita @bold(asincrona) realizada pelo tag.
    @seealso(TProtocolIOResult)
    }
    {$ELSE}
    {:
    I/O result of the last @bold(asynchronous) write done.
    @seealso(TProtocolIOResult)
    }
    {$ENDIF}
    property LastASyncWriteStatus:TProtocolIOResult Read PLastASyncWriteCmdResult;
  end;

  TManagedTags = array of TPLCTag;
  TTagMananger=class
  private
    ftags:TManagedTags;
  public
    constructor Create;
    destructor Destroy; override;
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
  PRack:=0;
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
  PUpdateTime:=1000;
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
  //if the tag is being loaded.
  if ([csReading,csLoading]*ComponentState<>[]) then begin
    FProtocoloOnLoading:=p;
    Exit;
  end;

  if p=PProtocolDriver then exit;

  //remove o driver antigo.
  //removes the link with the old driver.
  if (PProtocolDriver<>nil) then begin
    //remove do scan do driver...
    //removes the tag of the scan of the driver.
    if PAutoRead then
      PProtocolDriver.RemoveTag(self);
    PProtocolDriver := nil;
  end;

  //seta o novo driver.
  //sets the new protocol driver.
  if (p<>nil) then begin
    //adiciona no scan do driver...
    //add the tag to the scan of protocolo driver.
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
  if PRack=v then exit;

  if (PProtocolDriver<>nil) and PAutoRead then
    PProtocolDriver.RemoveTag(self);

  PRack := v;

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
  if PUpdateTime=v then exit;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.RemoveTag(Self);

  PUpdateTime := v;

  if (PProtocolDriver<>nil) AND PAutoRead then
    PProtocolDriver.AddTag(Self);
end;

procedure TPLCTag.BuildTagRec(out tr:TTagRec; Count, OffSet:Integer);
begin
  tr.Rack := PRack;
  tr.Slot := PSlot;
  tr.Station := PStation;
  tr.File_DB := PFile_DB;
  tr.Address := PAddress;
  tr.SubElement := PSubElement;
  Count := ifthen(Count=0, PSize, Count);

  //calcula o tamanho real e o offset de acordo com
  //o tipo de tag e tamanho da palavra de dados
  //que está chegando do protocolo...
  //
  //calculate the real size and the real offset depending
  //of the tag datatype and of protocol datatype.
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
  tr.UpdateTime := PUpdateTime;
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

function TPLCTag.IsMyCallBack(Cback:TTagCommandCallBack):Boolean;
begin
  Result:=(TMethod(Cback).Data=Self);
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
  //calculate how many bytes must be allocated.
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
  //move data to work memory.
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
  //swap bytes and words (if necessary)
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
  //calculate how many bytes must be allocated.
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

  //joga os valores puros lidos do driver para a area de memória para nao perder valores.
  //move the raw values to the work memory to don't loose data.
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
  //move data to the work memory.
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
  //swap bytes and words (if necessary).
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
  Result:=PUpdateTime-MilliSecondsBetween(Now,PValueTimeStamp);
end;

function TPLCTag.RemainingMilisecondsForNextScan:Int64;
begin
  Result:=PUpdateTime-MilliSecondsBetween(Now,PLastScanTimeStamp);
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
//PASCALSCADA TAG MANAGER.
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
