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
  TPLCTag = class(TTag)
  private
    CScanTimer:TTimer;
  protected
    //: A escrita do tag deve ser sincrona ou assincrona
    FSyncWrites:Boolean;
    //: Armazena o driver de protocolo usado para comunicação do tag.
    PProtocolDriver:TProtocolDriver;
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

    //: @exclude
    procedure SetGUID(v:String);
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
    Seta o endereço do equipamento que contem a memória sendo mapeada.
    @param(v Cardinal. Endereço do equipamento onde está a memória.)
    }
    procedure SetPLCStation(v:Cardinal); virtual;
    {:
    Seta o Arquivo/DB que contem a memória sendo mapeada.
    @param(v Cardinal. Arquivo/DB que a memória mapeada pertence.)
    }
    procedure SetMemFileDB(v:Cardinal); virtual;
    {:
    Seta o endereço da memória sendo mapeada.
    @param(v Cardinal. Endereço da memória sendo mapeada.)
    }
    procedure SetMemAddress(v:Cardinal); virtual;
    {:
    Seta o sub-endereço da memória sendo mapeada.
    @param(v Cardinal. Sub-endereço da memória sendo mapeada.)
    }
    procedure SetMemSubElement(v:Cardinal); virtual;
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
    Seta o endereço longo (texto) do tag.
    @param(v String. Endereço longo (texto) do tag.)
    }
    procedure SetPath(v:String); virtual;
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

    //: Procedimento chamado pelo driver de protocolo para atualização de valores do tag.
    procedure TagCommandCallBack(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:Integer); virtual; abstract;
    {:
    Compila uma estrutura com as informações do tag.
    @seealso(TTagRec)
    }
    procedure BuildTagRec(var tr:TTagRec; const Count, OffSet:Integer);
    //: Faz uma leitura @bold(assincrona) do tag.
    procedure ScanRead; virtual; abstract;
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
    //: Evento chamado pelo timer (TTimer interno) para atualizar o valor do tag.
    procedure DoScanTimerEvent(Sender:TObject);
    //: A escrita do tag deve ser sincrona
    property SyncWrites:Boolean read FSyncWrites write FSyncWrites default false ;
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

implementation

constructor TPLCTag.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PAutoRead:=true;
  PAutoWrite:=true;
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
  CScanTimer := TTimer.Create(self);
  CScanTimer.OnTimer := DoScanTimerEvent;
end;

destructor TPLCTag.Destroy;
begin
  CScanTimer.Destroy;
  if PProtocolDriver<>nil then
    PProtocolDriver.RemoveTag(self);
  PProtocolDriver := nil;
  inherited Destroy;
end;

procedure TPLCTag.RemoveDriver;
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.RemoveTag(self);
  PProtocolDriver := nil;
end;

procedure TPLCTag.SetProtocolDriver(p:TProtocolDriver);
begin
  //estou carregando meus parametros...
  if (csReading in ComponentState) then exit;
  
  //estou em tempo de desenvolvimento...
  if (csDesigning in ComponentState) then begin
    PProtocolDriver := p;
    exit;
  end;

  //estou trocando de driver...
  if (p<>nil) and (PProtocolDriver<>nil) then begin
    //se esta habilitado a leitura por scan, remove do driver antigo
    //e coloca no scan do driver novo
    if Self.PAutoRead then begin
      PProtocolDriver.RemoveTag(self);
      P.AddTag(self);
    end;
    PProtocolDriver := p;
  end;

  //estou removendo meu driver...
  if (p=nil) and (PProtocolDriver<>nil) then begin
    //remove do scan do driver...
    if Self.PAutoRead then
      PProtocolDriver.RemoveTag(self);
    PProtocolDriver := nil;
  end;

  //estou setando meu driver...
  if (p<>nil) and (PProtocolDriver=nil) then begin
    //adiciona no scan do driver...
    if Self.PAutoRead then
      P.AddTag(self);
    PProtocolDriver := p;
  end;
end;

procedure TPLCTag.SetAutoRead(v:Boolean);
begin
  PAutoRead := v;
  if CScanTimer<>nil then
    CScanTimer.Enabled := v;

  //adiciona ou remove do scan do driver...
  if (PProtocolDriver<>nil) then begin
    if v then
      PProtocolDriver.AddTag(self)
    else
      PProtocolDriver.RemoveTag(self);
  end;
end;

procedure TPLCTag.SetAutoWrite(v:Boolean);
begin
  PAutoWrite := v;
end;

procedure TPLCTag.SetPLCHack(v:Cardinal);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcPLCHack,PHack,v);
  PHack := v;
end;

procedure TPLCTag.SetPLCSlot(v:Cardinal);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcPLCSlot,PSlot,v);
  PSlot := v;
end;

procedure TPLCTag.SetPLCStation(v:Cardinal);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcPLCStation,PStation,v);
  PStation := v;
end;

procedure TPLCTag.SetMemFileDB(v:Cardinal);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcMemFile_DB,PFile_DB,v);
  PFile_DB := v;
end;

procedure TPLCTag.SetMemAddress(v:Cardinal);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcMemAddress,PAddress,v);
  PAddress := v;
end;

procedure TPLCTag.SetMemSubElement(v:Cardinal);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcMemSubElement,PSubElement,v);
  PSubElement := v;
end;

procedure TPLCTag.SetMemReadFunction(v:Cardinal);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcMemReadFunction,PReadFunction,v);
  PReadFunction := v;
end;

procedure TPLCTag.SetMemWriteFunction(v:Cardinal);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcMemWriteFunction,PWriteFunction,v);
  PWriteFunction := v;
end;

procedure TPLCTag.SetPath(v:String);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcPath,0,0);
  PPath := v;
end;

procedure TPLCTag.SetRefreshTime(v:TRefreshTime);
begin
  if PProtocolDriver<>nil then
    PProtocolDriver.TagChanges(self,tcScanTime,PScanTime,v);
  PScanTime := v;
  CScanTimer.Interval := v;
end;

procedure TPLCTag.DoScanTimerEvent(Sender:TObject);
begin
  if ComponentState*[csDesigning, csReading,csLoading]<>[] then exit;
  if PProtocolDriver<>nil then
    ScanRead;
end;

procedure TPLCTag.BuildTagRec(var tr:TTagRec; const Count, OffSet:Integer);
begin
  tr.Hack := PHack;
  tr.Slot := PSlot;
  tr.Station := PStation;
  tr.File_DB := PFile_DB;
  tr.Address := PAddress;
  tr.SubElement := PSubElement;
  tr.Size := ifthen(count=0, PSize, Count);
  tr.OffSet := offset;
  tr.Path := PPath;
  tr.ReadFunction := PReadFunction;
  tr.WriteFunction := PWriteFunction;
  tr.Retries := PRetries;
  tr.ScanTime := PScanTime;
  tr.CallBack := TagCommandCallBack;
end;

procedure TPLCTag.Loaded;
begin
  inherited Loaded;
end;

procedure TPLCTag.SetGUID(v:String);
begin
  if ComponentState*[csReading]=[] then exit;
  PGUID:=v;
end;

end.
