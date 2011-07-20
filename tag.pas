{$IFDEF PORTUGUES}
{:
  @abstract(Implementação da base de todos os tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements the base class of tags.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit Tag;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes;

type
  {$IFDEF PORTUGUES}
  {:
  Define o intervalo de atualização dos tags.
  }
  {$ELSE}
  {:
  Defines the the update rate range of tags.
  }
  {$ENDIF}
  TRefreshTime = 1..$7FFFFFFF;

  {$IFDEF PORTUGUES}
  //: Array dinamico de valores flutuantes.
  {$ELSE}
  //: Dynamic array of double.
  {$ENDIF}
  TArrayOfDouble = array of double;

  {$IFDEF PORTUGUES}
  //: Ponteiro para um array dinamico de pontos flutuantes.
  {$ELSE}
    //: Points to a array of doubles.
  {$ENDIF}
  PArrayOfDouble = ^TArrayOfDouble;

  {$IFDEF PORTUGUES}
  {:
  Enumera todos os possíveis tamanhos de palavras dos tags.
  @value(pttDefault  Tamanho e tipo de dados dependentes das configuração do tag.)
  @value(pttShortInt Inteiro de 8 bits COM sinal.)
  @value(pttByte     Inteiro de 8 bits sem sinal.)
  @value(pttSmallInt Inteiro de 16 bits COM sinal.)
  @value(pttWord,    Inteiro de 16 bits SEM sinal.)
  @value(pttInteger  Inteiro de 32 bits COM sinal.)
  @value(pttDWord,   Inteiro de 32 bits SEM sinal.)
  @value(pttFloat    Flutuante de 32 bits.)
  }
  {$ELSE}
  {:
  Enumerates all tag data types.
  @value(pttDefault  Word length and data type variable.)
  @value(pttShortInt Signed Integer, 8 bits sized.)
  @value(pttByte     Unsigned Integer, 8 bits sized.)
  @value(pttSmallInt Signed Integer, 16 bits sized.)
  @value(pttWord,    Unsigned Integer, 16 bits sized.)
  @value(pttInteger  Signed Integer, 32 bits sized.)
  @value(pttDWord,   Unsigned Integer, 32 bits sized.)
  @value(pttFloat    Float, 32 bits sized.)
  }
  {$ENDIF}
  TTagType = (pttDefault,                    //size variable
              pttShortInt, pttByte,          //8 bits
              pttSmallInt, pttWord,          //16 bits
              pttInteger, pttDWord, pttFloat //32 bits
             );

  {$IFDEF PORTUGUES}
  {:
  Enumera os possíveis tipos de comandos aceitos pelo tag.

  @value(tcScanRead        Leitura de valor através do scan do driver de protocolo (assincrona).)
  @value(tcScanWrite       Escrita de valor através do scan do driver de protocolo (assincrona).)
  @value(tcRead            Leitura de valor direta (sincrona).)
  @value(tcWrite           Escrita de valor direta (sincrona).)
  @value(tcInternalUpdate  Comando interno de atualização.)
  }
  {$ELSE}
  {:
  Enumerates all commands accept by the tag.

  @value(tcScanRead        Values are read using the driver scan.)
  @value(tcScanWrite       Values are write using the driver scan.)
  @value(tcRead            Values are read synchronous (without driver scan).)
  @value(tcWrite           Values are write synchronous (without driver scan).)
  @value(tcInternalUpdate  Internal tag update command.)
  }
  {$ENDIF}
  TTagCommand = (tcScanRead, tcScanWrite, tcRead, tcWrite, tcInternalUpdate);

  {$IFDEF PORTUGUES}
  {:
  Enumera todos os possíveis resultados de um pedido de leitura/escrita de um
  tag para um driver de protocolo (TProtocolDriver).

  @value(ioDriverError            Erro interno do driver.)
  @value(ioCommError              Erro de comunicação.)
  @value(ioOk                     Comando com sucesso.)
  @value(ioTimeout                Timeout de comunicação.)
  @value(ioIllegalFunction        Função de IO inválida.)
  @value(ioIllegalRegAddress      O endereco da memória é inválido.)
  @value(ioIllegalValue           O valor é inválido.)
  @value(ioPLCError               Erro no equipamento.)
  @value(ioTagError               Erro interno do Tag.)
  @value(ioNullDriver             Tag SEM DRIVER.)
  @value(ioIllegalStationAddress  Endereço do equipamento é inválido.)
  @value(ioIllegalRequest         Requisição inválida ou não suportada.)
  @value(ioObjectNotExists        O objeto requisitado não existe.)
  @value(ioIllegalMemoryAddress   O inicio ou o fim da requisição estão fora do espaço de endereços do equipamento.)
  @value(ioUnknownError           Um código de erro desconhecido foi retornado.)
  @value(ioEmptyPacket            Um pacote vazio (sem dados) foi retornado.)
  @value(ioPartialOk              Uma ação teve sucesso parcial.)
  }
  {$ELSE}
  {:
  Enumerates all results that can be returned by the protocol driver to a
  read/write request of a tag.

  @value(ioDriverError            Internal driver error.)
  @value(ioCommError              Communication error.)
  @value(ioOk                     Sucessfull request.)
  @value(ioTimeout                Communication timeout.)
  @value(ioIllegalFunction        Invalid IO function.)
  @value(ioIllegalRegAddress      Invalid memory address.)
  @value(ioIllegalValue           Invalid value.)
  @value(ioPLCError               Device error.)
  @value(ioTagError               Internal tag error.)
  @value(ioNullDriver             Tag without a driver.)
  @value(ioIllegalStationAddress  Invalid device address.)
  @value(ioIllegalRequest         The request is invalid or not supported.)
  @value(ioObjectNotExists        The requested object doesn't exists.)
  @value(ioIllegalMemoryAddress   The request is out of bound of the memory space of device.)
  @value(ioUnknownError           A invalid error code was returned.)
  @value(ioEmptyPacket            A empty packet was returned.)
  @value(ioPartialOk              An action was partially successful.)
  }
  {$ENDIF}
  TProtocolIOResult = (ioNone, ioDriverError, ioCommError, ioOk, ioTimeOut,
                       ioIllegalFunction, ioIllegalRegAddress,ioIllegalValue,
                       ioPLCError, ioTagError, ioNullDriver, ioIllegalRequest,
                       ioIllegalStationAddress, ioObjectNotExists,
                       ioIllegalMemoryAddress, ioUnknownError, ioEmptyPacket,
                       ioPartialOk);

  {$IFDEF PORTUGUES}
  {:
  Callback chamado pelo driver de protocolo (TProtocolDriver) para retornar o
  resultado de uma solicitação e os respectivos valores.
  @param(Values TArrayOfDouble: Array com os valores lidos/escritos.)
  @param(ValuesTimeStamp TDateTime: Data/Hora em que esses valores foram lidos/escritos.)
  @param(TagCommand TTagCommand: Tipo de comando.)
  @param(LastResult TProtocolIOResult: Resultado do driver ao processar o pedido.)
  @param(Offset Cardinal: Posição dentro do bloco onde os valores começam.)
  }
  {$ELSE}
  {:
  Callback called by the protocol driver (TProtocolDriver) to return the result
  of an request and theirs values.
  @param(Values TArrayOfDouble: Array with the values read/written.)
  @param(ValuesTimeStamp TDateTime: Date/Time when these values are read/written.)
  @param(TagCommand TTagCommand: Command type.)
  @param(LastResult TProtocolIOResult: I/O result after process this request.)
  @param(Offset Cardinal: Block Offset.)
  }
  {$ENDIF}
  TTagCommandCallBack = procedure(Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; OffSet:Integer) of object;

  {$IFDEF PORTUGUES}
  {:
  Estrutura usada internamente pelos drivers de protocolo (TProtocolDriver) para
  realizar leituras e escritas por Scan. Representa a configuração do tag que
  está sendo tratado.

  @member Hack Valor da propriedade PLCHack.
  @member Slot Valor da propriedade PLCSlot.
  @member Station Valor da propriedade PLCStation.
  @member File_DB Valor da propriedade MemFile_DB.
  @member Address Valor da propriedade MemAddress.
  @member SubElement Valor da propriedade MemSubElement.
  @member Size Valor da propriedade Size (Tags Blocos).
  @member OffSet Indice do bloco (Tags Blocos).
  @member Path Valor da propriedade LongAddress.
  @member ReadFunction Valor da propriedade MemReadFunction.
  @member WriteFunction Valor da propriedade MemWriteFunction.
  @member Retries Valor da propriedade Retries.
  @member ScanTime Valor da propriedade RefreshTime.
  @member CallBack Procedimento que será chamado quando o comando for completado.
  }
  {$ELSE}
  {:
  Struture used internaly by the protocolo driver (TProtocolDriver) to process
  read and write requests. Represents the configuration of the tag being
  processed.

  @member Rack Value of PLCRack property.
  @member Slot Value of PLCSlot property.
  @member Station Value of PLCStation property.
  @member File_DB Value of MemFile_DB property.
  @member Address Value of MemAddress property.
  @member SubElement Value of MemSubElement property.
  @member Size Value of Size (Block tags) property.
  @member OffSet Index inside the block (Block Tags).
  @member Path Value of LongAddress property.
  @member ReadFunction Value of MemReadFunction property.
  @member WriteFunction Value of MemWriteFunction property.
  @member Retries Value of Retries property.
  @member UpdateTime Value of UpdateTime property.
  @member CallBack Procedure called when the request is done to return the data of the request.
  }
  {$ENDIF}
  TTagRec = record
    Rack:Integer;
    Slot:Integer;
    Station:Integer;
    File_DB:Integer;
    Address:Integer;
    SubElement:Integer;
    Size:Integer;
    OffSet:Integer;
    RealOffset:Integer;
    Path:String;
    ReadFunction:Integer;
    WriteFunction:Integer;
    Retries:Integer;
    UpdateTime:Integer;
    CallBack:TTagCommandCallBack;
  end;

  {$IFDEF PORTUGUES}
  //: Aponta para uma estrutura de Tag.
  {$ELSE}
  //: Points to a tag structure
  {$ENDIF}
  PTagRec = ^TTagRec;

  {$IFDEF PORTUGUES}
  {:
  Interface de notificação de eventos do tag. Usado para notificar os controles
  ligados ao tag sobre o que ocorre com o tag.
  }
  {$ELSE}
  {:
  Tag events notification interface. Used to notifies the linked controls about
  any event that occurs with the tag.
  }
  {$ENDIF}
  IHMITagInterface = interface
    ['{4301B240-79D9-41F9-A814-68CFEFD032B8}']
    {$IFDEF PORTUGUES}
    //: Chama o evento quando o tag tem uma letura com exito.
    {$ELSE}
    //: Called when the tag has a successful read.
    {$ENDIF}
    procedure NotifyReadOk;

    {$IFDEF PORTUGUES}
    //: Chama o evento quando uma leitura falha.
    {$ELSE}
    //: Called when the tag has a fault read.
    {$ENDIF}
    procedure NotifyReadFault;

    {$IFDEF PORTUGUES}
    //: Chama o evento quando uma escrita tem sucesso.
    {$ELSE}
    //: Called when the tag has a successful write.
    {$ENDIF}
    procedure NotifyWriteOk;

    {$IFDEF PORTUGUES}
    //: Chama o evento quando uma escrita do tag falha.
    {$ELSE}
    //: Called when the tag has a fault write.
    {$ENDIF}
    procedure NotifyWriteFault;

    {$IFDEF PORTUGUES}
    //: Chama o evento quando o valor do tag muda.
    {$ELSE}
    //: Called when the tag value changes.
    {$ENDIF}
    procedure NotifyTagChange(Sender:TObject);

    {$IFDEF PORTUGUES}
    {:
    Notifica objetos dependentes que o tag será removido.
    }
    {$ELSE}
    //: Notifies the control about the destruction of the tag.
    {$ENDIF}
    procedure RemoveTag(Sender:TObject);
  end;

  {$IFDEF PORTUGUES}
  //: Interface de gerenciamento de identificação do tag.
  {$ELSE}
  //: Interface of management of unique tag identification.
  {$ENDIF}
  IManagedTagInterface = interface
    ['{5CC728FD-B75F-475E-BDE7-07A862B6B2B6}']
    {$IFDEF PORTUGUES}
    //: Ordena o tag refazer sua identificação a fim de torná-la única.
    {$ELSE}
    //: Forces the tag to rebuild their identification to make it unique.
    {$ENDIF}
    procedure RebuildTagGUID;
  end;

  {$IFDEF PORTUGUES}
  {:
  Interface de atualização do tag. Interface usada pelo driver de protocolo
  para saber quando o tag deve ser atualizado.
  }
  {$ELSE}
  {:
  Tag scan interface. Used by the protocol driver to know when a tag must be
  updated.
  }
  {$ENDIF}
  IScanableTagInterface = interface
    ['{6D57805C-D779-4607-BDA5-DF8A68F49973}']
    {$IFDEF PORTUGUES}
    //: Informa quanto tempo se passou desde a última atualização do valor do tag.
    {$ELSE}
    //: Tells how many time has elapsed from the last update of tag value.
    {$ENDIF}
    function RemainingMiliseconds:Int64;

    {$IFDEF PORTUGUES}
    //: Informa quanto tempo se passou desde a última verificação do tag.
    {$ELSE}
    //: Tells how many time has elapsed from the last scan of tag.
    {$ENDIF}
    function RemainingMilisecondsForNextScan:Int64;

    {$IFDEF PORTUGUES}
    //: Diz se o tag está configurado corretamente.
    {$ELSE}
    //: Tells if the tag is set properly.
    {$ENDIF}
    function IsValidTag:Boolean;

    {$IFDEF PORTUGUES}
    //: Informa se o callback pertence ao tag.
    {$ELSE}
    //: Tells if callback belongs to the tag
    {$ENDIF}
    function IsMyCallBack(Cback:TTagCommandCallBack):Boolean;

    {$IFDEF PORTUGUES}
    //: Seta o tag como válido ou não.
    {$ELSE}
    //: Sets the tag as valid or not.
    {$ENDIF}
    procedure SetTagValidity(TagValidity:Boolean);

    {$IFDEF PORTUGUES}
    //: Obtem a estrutura com as informações do tag.
    {$ELSE}
    //: Gets a structure with informations about the tag.
    {$ENDIF}
    procedure BuildTagRec(out tr:TTagRec; Count, OffSet:Integer);
  end;

  {$IFDEF PORTUGUES}
  //: Classe base para todos os tags.
  {$ELSE}
  //: Base class for all tags.
  {$ENDIF}
  TTag = class(TComponent)
  protected
    {$IFDEF PORTUGUES}
    //: Booleano que armazena se o tag vai ser lido automaticamente.
    {$ELSE}
    //: Stores if the tag will be updated automatically.
    {$ENDIF}
    PAutoRead:Boolean;

    {$IFDEF PORTUGUES}
    //: Booleano que armazena se o tag vai ter seu valor escrito automaticamente.
    {$ELSE}
    //: Stores if the tag will write their value automatically.
    {$ENDIF}
    PAutoWrite:Boolean;
    //: Conta os erros de leitura.
    PCommReadErrors:Cardinal;
    //: Conta as leituras com exito.
    PCommReadOK:Cardinal;
    //: Conta os erros de escritas do tag.
    PCommWriteErrors:Cardinal;
    //: Conta as escritas com sucesso do tag.
    PCommWriteOk:Cardinal;
    //: Armazena o Hack do equipamento da memória que está sendo mapeada.
    PRack:Cardinal;
    //: Armazena o Slot do equipamento da memória que está sendo mapeada.
    PSlot:Cardinal;
    //: Armazena o endereço da estação da memória que está sendo mapeada.
    PStation:Cardinal;
    //: Armazena o Arquivo/DB dentro do equipamento da memória que está sendo mapeada.
    PFile_DB:Cardinal;
    //: Armazena o endereço da memória no equipamento que está sendo mapeada.
    PAddress:Cardinal;
    //: Armazena o subendereço da memória no equipamento que está sendo mapeada.
    PSubElement:Cardinal;
    //: Armazena o número de memórias que estão mapeadas.
    PSize:Cardinal;
    //: Armazena o endereço completo da memória em formato texto.
    PPath:String;
    //: Armazena a função usada para leitura da memória.
    PReadFunction:Cardinal;
    //: Armazena a função usada para escrita da memória.
    PWriteFunction:Cardinal;
    //: Armazena o número de tentivas de leitura/escrita da memória.
    PRetries:Cardinal;
    //: Armazena o tempo de varredura a memória.
    PScanTime:TRefreshTime;
    //: Armazena o evento chamado pelo quando uma leitura do tag tem sucesso.
    POnReadOk:TNotifyEvent;
    //: Armazena o evento chamado pelo tag quando uma leitura do tag falha.
    POnReadFail:TNotifyEvent;
    //: Armazena o evento chamado pelo tag quando uma escrita tem sucesso.
    POnWriteOk:TNotifyEvent;
    //: Armazena o evento chamado pelo tag quando uma escrita falha.
    POnWriteFail:TNotifyEvent;
    //: Armazena o evento chamado pelo tag quando o seu valor se altera.
    POnValueChangeFirst:TNotifyEvent;
    //: Armazena o evento chamado pelo tag quando o seu valor se altera.
    POnValueChangeLast:TNotifyEvent;
    //: Armazena os procedimentos que o tag deve chamar quando o seu valor altera.
    PChangeCallBacks:array of IHMITagInterface;
    //: Conta os callbacks que dependem desse tag.
    PChangeCallBackCount:integer;

    //: Armazena o identificador desse tag. GUID
    PGUID:String;

    //: Chama o evento quando uma letura tem exito.
    procedure NotifyReadOk;
    //: Chama o evento quando uma leitura falha.
    procedure NotifyReadFault;
    //: Chama o evento quando uma escrita tem sucesso.
    procedure NotifyWriteOk;
    //: Chama o evento quando uma escrita do tag falha.
    procedure NotifyWriteFault;
    //: Chama o evento quando o valor do tag muda.
    procedure NotifyChange;

    //: Incrementa o contador de leituras com sucesso.
    procedure IncCommReadOK(value:Cardinal);
    //: Incrementa o contador de leituras com falha do tag.
    procedure IncCommReadFaults(value:Cardinal);
    //: Incrementa o contador de escritas com exito do tag.
    procedure IncCommWriteOK(value:Cardinal);
    //: Incrementa o contador de falhas de escrita do tag.
    procedure IncCommWriteFaults(value:Cardinal);

    //: Caso @true, o tag será lido automaticamente.
    property AutoRead:Boolean read PAutoRead;
    {:
    Caso @true, toda a vez que ocorrerem escritas no tag,
    ele irá escrever o valor no equipamento.
    }
    property AutoWrite:Boolean read PAutoWrite;
    //: Informa o total de erros de leitura do tag.
    property CommReadErrors:Cardinal read PCommReadErrors;
    //: Informa o total de leituras com exito do tag.
    property CommReadsOK:Cardinal read PCommReadOK;
    //: Informa o total de erros de escrita do tag.
    property CommWriteErrors:Cardinal read PCommWriteErrors;
    //: Informa o total de escritas com sucesso do tag.
    property CommWritesOk:Cardinal read PCommWriteOk;
    //: Hack do equipamento que contem a memória que está sendo mapeada, se aplicável.
    property PLCHack:Cardinal read PRack stored false;
    //: Rack do equipamento que contem a memória que está sendo mapeada, se aplicável.
    property PLCRack:Cardinal read PRack;
    //: Slot do equipamento que contem a memória que está sendo mapeada, se aplicável.
    property PLCSlot:Cardinal read PSlot;
    //: Endereço da estação que contem a memória que está sendo mapeada, se aplicável.
    property PLCStation:Cardinal read PStation;
    //: Arquivo/DB dentro do equipamento que contem a memória que está sendo mapeada, se aplicável.
    property MemFile_DB:Cardinal read PFile_DB;
    //: Endereço da memória que está sendo mapeada.
    property MemAddress:Cardinal read PAddress;
    //: Subendereço da memória que está sendo mapeada, se aplicável.
    property MemSubElement:Cardinal read PSubElement;
    //: Função do driver responsável por realizar a leitura dessa memória.
    property MemReadFunction:Cardinal read PReadFunction;
    //: Função do driver responsável por realizar a escrita de valores dessa memória.
    property MemWriteFunction:Cardinal read PWriteFunction;
    //: Número tentivas de leitura/escrita dessa memória.
    property Retries:Cardinal read PRetries;
    //: Tempo de varredura (atualização) dessa memória em milisegundos.
    property RefreshTime:TRefreshTime read PScanTime stored false;
    //: Tempo de varredura (atualização) dessa memória em milisegundos.
    property UpdateTime:TRefreshTime read PScanTime;
    //: Número de memórias que serão mapeadas, se aplicável.
    property Size:Cardinal read PSize;
    //: Endereço longo (texto), se aplicável ao driver.
    property LongAddress:String read PPath;

    //: Evento chamado quando uma leitura do tag tem exito.
    property OnReadOK:TNotifyEvent      read POnReadOk       write POnReadOk;
    //: Evento chamado quando uma leitura do tag falha.
    property OnReadFail:TNotifyEvent    read POnReadFail     write POnReadFail;
    //: Evento chamado quando uma escrita de valor do tag tem exito.
    property OnWriteOk:TNotifyEvent     read POnWriteOk      write POnWriteOk;
    //: Evento chamado quando uma escrita do tag falha.
    property OnWriteFail:TNotifyEvent   read POnWriteFail    write POnWriteFail;
    //: Evento chamado quando o valor do tag sofre alguma mudançaW.
    property OnValueChange:TNotifyEvent read POnValueChangeLast  write POnValueChangeLast stored false;
    property OnValueChangeLast:TNotifyEvent read POnValueChangeLast  write POnValueChangeLast;
    //: Evento chamado quando o valor do tag sofre alguma mudança.
    property OnValueChangeFirst:TNotifyEvent read POnValueChangeFirst  write POnValueChangeFirst;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;
    //: Adiciona um conjunto de notificação de alteração para o tag.
    procedure AddCallBacks(ITag:IHMITagInterface);
    //: Remove um conjunto de notificação de mudanças do tag.
    procedure RemoveCallBacks(ITag:IHMITagInterface);
  end;

  TScanUpdateRec = record
    Owner:TTag;
    CallBack:TTagCommandCallBack;
    ValueTimeStamp:TDateTime;
    LastResult:TProtocolIOResult;
    Values:TArrayOfDouble;
  end;

  TArrayOfScanUpdateRec = array of TScanUpdateRec;

implementation

uses hsstrings;


constructor TTag.Create(AOwner:TComponent);
var
  x:TGuid;
begin
  inherited Create(AOwner);
  PChangeCallBackCount := 0;
  PCommReadErrors := 0;
  PCommReadOK := 0;
  PCommWriteErrors := 0;
  PCommWriteOk := 0;
  PScanTime:=1000;

  if ComponentState*[csReading, csLoading]=[] then begin
    CreateGUID(x);
    PGUID:=GUIDToString(x);
  end;
end;

destructor TTag.Destroy;
var
  c:Integer;
begin
  for c := 0 to High(PChangeCallBacks) do
    PChangeCallBacks[c].RemoveTag(Self);
  inherited Destroy;
end;

procedure TTag.AddCallBacks(ITag:IHMITagInterface);
begin
  if (ITag<>nil) and ((ITag as IHMITagInterface)=nil) then
    raise Exception.Create(SinvalidInterface);
  
  inc(PChangeCallBackCount);
  SetLength(PChangeCallBacks, PChangeCallBackCount);
  PChangeCallBacks[PChangeCallBackCount-1]:=ITag;
end;

procedure TTag.RemoveCallBacks(ITag:IHMITagInterface);
var
  c,h:Integer;
  found:Boolean;
begin
  found:=false;
  h := High(PChangeCallbacks);
  for c:=0 to h do
    if (ITag)=(PChangeCallBacks[c]) then begin
      found := true;
      break;
    end;
  if found then begin
    PChangeCallBacks[c] := PChangeCallbacks[h];
    PChangeCallBackCount := PChangeCallBackCount - 1;
    SetLength(PChangeCallBacks, PChangeCallBackCount);
  end;
end;

procedure TTag.NotifyChange;
var
  c:Integer;
begin
  //notifica a mudanca antes de notificar os
  //demais controles.
  try
    if Assigned(POnValueChangeFirst) then
      POnValueChangeFirst(Self);
  except
  end;

  //notifica controles e objetos dependentes
  for c:=0 to High(PChangeCallBacks) do
    try
      PChangeCallBacks[c].NotifyTagChange(self);
    except
    end;

  //notificação de mudanca após notificar os controles.
  try
    if Assigned(POnValueChangeLast) then
      POnValueChangeLast(Self);
  except
  end;
end;

procedure TTag.NotifyReadOk;
var
  c:Integer;
begin
  for c:=0 to High(PChangeCallBacks) do
    try
      PChangeCallBacks[c].NotifyReadOk;
    except
    end;

  if Assigned(POnReadOk) then
    POnReadOk(self)
end;

procedure TTag.NotifyReadFault;
var
  c:Integer;
begin
  for c:=0 to High(PChangeCallBacks) do
    try
      PChangeCallBacks[c].NotifyReadFault;
    except
    end;

  if Assigned(POnReadFail) then
    POnReadFail(self)
end;

procedure TTag.NotifyWriteOk;
var
  c:Integer;
begin
  for c:=0 to High(PChangeCallBacks) do
    try
      PChangeCallBacks[c].NotifyWriteOk;
    except
    end;

  if Assigned(POnWriteOk) then
    POnWriteOk(self)
end;

procedure TTag.NotifyWriteFault;
var
  c:Integer;
begin
  for c:=0 to High(PChangeCallBacks) do
    try
      PChangeCallBacks[c].NotifyWriteFault;
    except
    end;

  if Assigned(POnWriteFail) then
    POnWriteFail(self)
end;

procedure TTag.IncCommReadOK(value:Cardinal);
begin
  inc(PCommReadOK,value);
  if value>0 then
    NotifyReadOk;
end;

procedure TTag.IncCommReadFaults(value:Cardinal);
begin
  inc(PCommReadErrors,value);
  if value>0 then
    NotifyReadFault;
end;

procedure TTag.IncCommWriteOK(value:Cardinal);
begin
  inc(PCommWriteOk,value);
  if value>0 then
    NotifyWriteOk;
end;

procedure TTag.IncCommWriteFaults(value:Cardinal);
begin
  inc(PCommWriteErrors,value);
  if value>0 then
    NotifyWriteFault;
end;

end.