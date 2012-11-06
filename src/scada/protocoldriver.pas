{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
@abstract(Implementação da base de um driver de protocolo.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
@abstract(Unit that implements a base class of protocol driver.)
@author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ENDIF}
unit ProtocolDriver;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, CommPort, CommTypes, ProtocolTypes, protscanupdate,
  protscan, CrossEvent, Tag, syncobjs {$IFNDEF FPC}, Windows{$ENDIF};

type

  {$IFDEF PORTUGUES}
    {:
  @abstract(Classe base para drivers de protocolo.)

  @author(Fabio Luis Girardi fabio@pascalscada.com)

  Para você criar um novo driver, basta sobrescrever alguns métodos e funções,
  de acordo com as necessidades de seu driver de protocolo. São eles:

  @code(procedure DoAddTag(TagObj:TTag);)
  Sobrescreva esse procedimento para adicionar tags ao scan do driver. Faça as
  devidas verificações do tag nesse método e caso ele não seja um tag válido
  gere uma excessão para abortar a adição do tag no driver.
  Não esqueça de chamar o método herdado com @code(inherited DoAddTag(TagObj:TTag))
  para adicionar o tag na classe base (@name).

  @code(procedure DoDelTag(TagObj:TTag);)
  Procedimento por remover tags do scan do driver. Não esqueça de chamar o
  método herdado com @code(inherited DoDelTag(TagObj:TTag)) para remover o tag
  da classe base (@name).

  @code(procedure DoScanRead(Sender:TObject; var NeedSleep:Integer);)
  Prodimento chamado para verificar se há algum tag necessitando ser lido.

  @code(procedure DoGetValue(TagObj:TTagRec; var values:TScanReadRec);)
  Procedimento chamado pelo driver para retornar os valores lidos que estão
  em algum gerenciador de memória para os tags.

  @code(function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;)
  Executa as escritas de valores sincronas e assincronas dos tags. É este método
  que escreve os valores do tag no seu equipamento.

  @code(function DoRead (const tagrec:TTagRec; var   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult;)
  Executa as leituras sincronas e assincronas dos tags. É o método que vai
  buscar os valores no seu equipamento.

  @code(function  SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; )
  Função responsável por informar o tamanho das palavras de dados em bits
  que o tag está referenciando.

  Sobrescrevendo esses métodos e rotinas, seu driver estará pronto. @bold(Veja
  a documentação detalhada de cada método para enteder como cada um funciona.)
  }
  {$ELSE}

  {$ENDIF}
  TProtocolDriver = class(TComponent, IPortDriverEventNotification)
  private
    //Array de tags associados ao driver.
    //Array of linked tags.
    PTags:array of TTag;

    //Tempo de gasto em millisegundos atualizando valores dos tags (e seus dependentes)
    //Time used in milliseconds to update the tag value and their dependents. 
    FUserUpdateTime:Double;

    //thread de execução do scan dos tags
    //Scan read thread object
    PScanReadThread:TScanThread;
    //Thread de execução de escritas
    //scan write thead object
    PScanWriteThread:TScanThread;
    //thread de atualização dos pedidos dos tags
    //thread that updates tag values.
    PScanUpdateThread:TScanUpdate;

    //excessao caso o index to tag esteja fora dos limites
    //raises an exception if the that index is out of bounds.
    procedure DoExceptionIndexOut(index:integer);

    //metodos para manipulação da lista de tags
    //procedures to handle the taglist.
    function  GetTagCount:Integer;
    function  GetTag(index:integer):TTag;
    function  GetTagName(index:integer):String;
    function  GetTagByName(Nome:String):TTag;

    //metodo chamado pela thread de scan para ler valores do dispositivo.
    //procedure called to read data from your device.
    procedure SafeScanRead(Sender:TObject; var NeedSleep:Integer);
    //metodo chamado pela thred de scan de escrita para escrever valores no dispositivo.
    //procedure called by the scan write thread to write data on your device.
    function  SafeScanWrite(const TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
    //metodo chamado para atualizar o valor de um tag (simples ou bloco)
    //procedure called to update the value of an tag (single or block)
    procedure SafeGetValue(const TagRec:TTagRec; var values:TScanReadRec);
    //metodo chamado para atualizar o valores de varios tags (simples ou bloco)
    //procedure called to update values of multiples tag (single or block)
    function  GetMultipleValues(var MultiValues:TArrayOfScanUpdateRec):Integer;

    procedure DoPortOpened(Sender: TObject);
    procedure DoPortClosed(Sender: TObject);
    procedure DoPortDisconnected(Sender: TObject);
    procedure DoPortRemoved(Sender:TObject);
    procedure UpdateUserTime(usertime:Double);
  protected

    {$IFDEF PORTUGUES}
    {:
    Informa ao driver que uma operação de alta latência irá começar, fazendo
    que ele libere os recursos para atualização dos tags.
    }
    {$ELSE}
    {:
    Tells to driver that a high latency operation will begins, releasing some
    resources of the driver.
    }
    {$ENDIF}
    procedure HighLatencyOperationWillBegin(Sender:TObject);

    {$IFDEF PORTUGUES}
    {:
    Informa ao driver que uma operação de alta latência foi finalizada,
    recuperando os recursos necessários para o driver.
    }
    {$ELSE}
    {:
    Tells to driver that a high latency operation was ended, taking back some
    resources back to driver.
    }
    {$ENDIF}
    procedure HighLatencyOperationWasEnded(Sender:TObject);

  protected
    {$IFDEF PORTUGUES}
    {:
    Flag que informa ao driver se ao menos uma variavel deve ser lida a cada
    ciclo de scan do driver.
    }
    {$ELSE}
    {:
    Tells if at least one item must be read at each scan cycle of protocol driver.
    }
    {$ENDIF}
    PReadSomethingAlways:Boolean;

    {$IFDEF PORTUGUES}
    // Indica se o driver está pronto.
    {$ELSE}
    //: Tells if the protocol driver is ready
    {$ENDIF}
    FProtocolReady:Boolean;

    {$IFDEF PORTUGUES}
    //: Armazena a ID (número único) do driver.
    {$ELSE}
    //: Stores the unique identification of protocol driver.
    {$ENDIF}
    PDriverID:Cardinal;

    {$IFDEF PORTUGUES}
    //: Armazena o driver de porta associado a esse driver de protocolo.
    {$ELSE}
    //: Stores the communication port driver used by protocol driver.
    {$ENDIF}
    PCommPort:TCommPortDriver;

    {$IFDEF PORTUGUES}
    //: Armazena o ID (número único) esses pedidos.
    {$ELSE}
    //: Stores the unique identification of each kind of request.
    {$ENDIF}
    FScanReadID, FScanWriteID, FReadID, FWriteID:Cardinal;

    {$IFDEF PORTUGUES}
    //: Mutex que proteje o driver de protocolo.
    {$ELSE}
    //: Mutex that protect the protocol driver.
    {$ENDIF}
    FReadCS, FWriteCS:TCriticalSection;

    {$IFDEF PORTUGUES}
    //: Forca a suspensão das threads, a fim de manter a execução correta do sistema.
    {$ELSE}
    //: Stop thread to keep the normal execution of the all system.
    {$ENDIF}
    FPause:TCrossEvent;

    {$IFDEF PORTUGUES}
    //: Mutex que proteje as chamadas de scan do driver.
    {$ELSE}
    //: Mutex that protect the scan procedures of the protocol driver.
    {$ENDIF}
    PCallersCS:TCriticalSection;

    {$IFDEF PORTUGUES}
    {:
    Retorna o procedimento do driver de protocolo que deve ser chamado quando
    a porta de comunicação é aberta.

    @seealso(PortOpened)
    @seealso(NotifyThisEvents)
    }
    {$ELSE}
    {:
    Returns the procedure of protocol driver that must be called when the
    communication port was open.

    @seealso(PortOpened)
    @seealso(NotifyThisEvents)
    }
    {$ENDIF}
    function  GetPortOpenedEvent:TNotifyEvent;

    {$IFDEF PORTUGUES}
    {:
    Retorna o procedimento do driver de protocolo que deve ser chamado quando
    a porta de comunicação é fechada.
    @seealso(PortClosed)
    @seealso(NotifyThisEvents)
    }
    {$ELSE}
    {:
    Returns the procedure of protocol driver that must be called when the
    communication port was closed.
    @seealso(PortClosed)
    @seealso(NotifyThisEvents)
    }
    {$ENDIF}
    function  GetPortClosedEvent:TNotifyEvent;

    {$IFDEF PORTUGUES}
    {:
    Retorna o procedimento do driver de protocolo que deve ser chamado quando
    a porta de comunicação é desconectada.
    @seealso(PortDisconnected)
    @seealso(NotifyThisEvents)
    }
    {$ELSE}
    {:
    Returns the procedure of protocol driver that must be called when the
    communication port was disconnected.
    @seealso(PortDisconnected)
    @seealso(NotifyThisEvents)
    }
    {$ENDIF}
    function  GetPortDisconnectedEvent:TNotifyEvent;

    {$IFDEF PORTUGUES}
    {:
    Retorna os eventos de porta de comunicação que o driver de protocolo deseja
    ser notificado.
    @seealso(PortOpened)
    @seealso(PortClosed)
    @seealso(PortDisconnected)
    @seealso(TNotifyEvent)
    }
    {$ELSE}
    {:
    Returns what's the events of communication port driver must be notified to
    protocol driver.
    @seealso(PortOpened)
    @seealso(PortClosed)
    @seealso(PortDisconnected)
    @seealso(TNotifyEvent)
    }
    {$ENDIF}
    function  NotifyThisEvents:TNotifyThisEvents; virtual;

    {$IFDEF PORTUGUES}
    {:
    Procedimento chamado pela porta de comunicação é aberta.
    @seealso(NotifyThisEvents)
    @seealso(GetPortOpenedEvent)
    }
    {$ELSE}
    {:
    Procedure called by the communication port when it was open.
    @seealso(NotifyThisEvents)
    @seealso(GetPortOpenedEvent)
    }
    {$ENDIF}
    procedure PortOpened(Sender: TObject); virtual;

    {$IFDEF PORTUGUES}
    {:
    Procedimento chamado pela porta de comunicação é fechada.
    @seealso(NotifyThisEvents)
    @seealso(GetPortClosedEvent)
    }
    {$ELSE}
    {:
    Procedure called by the communication port when it was closed.
    @seealso(NotifyThisEvents)
    @seealso(GetPortClosedEvent)
    }
    {$ENDIF}
    procedure PortClosed(Sender: TObject); virtual;

    {$IFDEF PORTUGUES}
    {:
    Procedimento chamado pela porta de comunicação é desconectada.
    @seealso(NotifyThisEvents)
    @seealso(GetPortClosedEvent)
    }
    {$ELSE}
    {:
    Procedure called by the communication port when it was disconnected.
    @seealso(NotifyThisEvents)
    @seealso(GetPortDisconnectedEvent)
    }
    {$ENDIF}
    procedure PortDisconnected(Sender: TObject); virtual;


    {$IFDEF PORTUGUES}
    //: Configura a porta de comunicação que será usada pelo driver.
    {$ELSE}
    //: Sets the communication port driver that will be used by the protocol driver.
    {$ENDIF}
    procedure SetCommPort(CommPort:TCommPortDriver);

    {$IFDEF PORTUGUES}
    {:
    Copia uma estrutura TIOPacket para outra.
    @param(Source TIOPacket. Estrutura de origem dos dados.)
    @param(Dest TIOPacket. Estrutura para onde os dados serão copiados.)
    }
    {$ELSE}
    {:
    Copies a TIOPacket to another
    @param(Source TIOPacket. Source record.)
    @param(Dest TIOPacket. Destination record.)
    }
    {$ENDIF}
    procedure CopyIOPacket(const Source:TIOPacket; var Dest:TIOPacket);

    {$IFDEF PORTUGUES}
    {:
    Callback que o driver de porta (TCommPortDriver) irá chamar para retornar os
    resultados de E/S.
    @param(Result TIOPacket. Estrutura com os dados de retorno da solicitação
           de I/O. @bold(É automaticamente destruida após retornar desse
           método.)
    }
    {$ELSE}
    {:
    Callback called by the communication port driver to returns the result of I/O.
    @param(Result TIOPacket. Record with the data and I/O results returned by
    the communication port. @bold(Is destroyed automaticaly.)
    }
    {$ENDIF}
    procedure CommPortCallBack(var Result:TIOPacket); virtual;

    {$IFDEF PORTUGUES}
    {:
    Método chamado para adicionar um tag ao scan driver.
    @param(TagObj TTag. Tag a adicionar como dependente do driver.)
    @seealso(AddTag)
    }
    {$ELSE}
    {:
    Procedure called to add a tag into the scan of the protocol driver.
    @param(TagObj TTag. Tag to be added into the scan of protocol driver.)
    @seealso(AddTag)
    }
    {$ENDIF}
    procedure DoAddTag(TagObj:TTag; TagValid:Boolean); virtual;

    {$IFDEF PORTUGUES}
    {:
    Método chamado pelo driver de protocolo para remover um tag do scan do driver.
    @param(TagObj TTag. Tag dependente para remover do driver.)
    @seealso(RemoveTag)
    }
    {$ELSE}
    {:
    Procedure called to remove a tag from the scan of the protocol driver.
    @param(TagObj TTag. Tag to be removed of the scan of protocol driver.)
    @seealso(RemoveTag)
    }
    {$ENDIF}
    procedure DoDelTag(TagObj:TTag); virtual;

    {$IFDEF PORTUGUES}
    {:
    Método chamado pelas threads do driver de protocolo para verificar se há tags
    que precisam ser lidos do dispositivo.
    @param(Sender TObject. Thread que está solicitando a varredura de atualização.)
    @param(NeedSleep Integer. Caso o procedimento não encontrou nada que precise
                              ser lido, escreva um valor negativo na variável para
                              forçar o scheduler do seu sistema operacional a
                              executar outra thread ou um valor positivo para
                              fazer a thread de scan dormir. O tempo que ela
                              ficará dormindo é o valor escrito nessa variável.
                              Caso o seu driver encontrou algum tag necessitando
                              de atualização, retorne 0 (Zero).)
    @seealso(TProtocolDriver.DoRead)
    }
    {$ELSE}
    {:
    Procedure called by the protocol driver threads to check if has some tags
    that must be updated/readed from device.
    @param(Sender TObject. Thread that's calling the procedure.)
    @param(NeedSleep Integer. If the procedure did not found anything to be
    read/updated, write in this variable a negative value to force the scheduler
    of the OS to switch to another thread, or a positive value to make the caller
    thread sleep. The time of the sleep is the value of this variable. If this
    procedure found some tag that was updated, write 0 (zero) on this variable.)
    @seealso(TProtocolDriver.DoRead)
    }
    {$ENDIF}
    procedure DoScanRead(Sender:TObject; var NeedSleep:Integer); virtual; abstract;

    {$IFDEF PORTUGUES}
    {:
    Método chamado pelas threads do driver de protocolo para atualizar os valores
    dos tags.
    @param(TagRec TTagRec. Estrutura com informações do tag.)
    @param(values TScanReadRec. Armazena os valores que serão enviados ao tag.)
    }
    {$ELSE}
    {:
    Procedure called by the protocol driver threads to update the tag values.
    @param(TagRec TTagRec. Structure with informations about the tag.)
    @param(values TScanReadRec. Array with the tag values.)
    }
    {$ENDIF}
    procedure DoGetValue(TagRec:TTagRec; var values:TScanReadRec); virtual; abstract;

    {$IFDEF PORTUGUES}
    {:
    Função chamada para escrever valores de um tag (simples ou bloco) no
    equipamento.

    @param(tagrec TTagRec. Estrutura com informações do tag.)
    @param(Values TArrayOfDouble. Valores a serem escritos no equipamento.)

    @returns(TProtocolIOResult).
    }
    {$ELSE}
    {:
    Function called to write tag values (single or block) on device.

    @param(tagrec TTagRec. Strucutre with informations about the tag.)
    @param(Values TArrayOfDouble. Values to be written on device.)

    @returns(TProtocolIOResult).
    }
    {$ENDIF}
    function DoWrite(const tagrec:TTagRec; const Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; virtual; abstract;

    {$IFDEF PORTUGUES}
    {:
    Função chamada para ler valores do equipamento.

    @param(tagrec TTagRec. Estrutura com informações do tag.)
    @param(Values TArrayOfDouble. Array que irá armazenar os valores lidos do equipamento.)

    @returns(TProtocolIOResult).
    }
    {$ELSE}
    {:
    Function called to read values from your device.

    @param(tagrec TTagRec. Strucutre with informations about the tag.)
    @param(Values TArrayOfDouble. Array that will store the values read from your device.)

    @returns(TProtocolIOResult).
    }
    {$ENDIF}
    function DoRead (const tagrec:TTagRec; out   Values:TArrayOfDouble; Sync:Boolean):TProtocolIOResult; virtual; abstract;

    {$IFDEF PORTUGUES}
    //: Informa ao driver se ele deve ler algum tag a todo scan.
    {$ELSE}
    //: Tells if the protocol driver must read something on each scan cycle.
    {$ENDIF}
    property ReadSomethingAlways:Boolean read PReadSomethingAlways write PReadSomethingAlways default true;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;

    //: @exclude
    procedure AfterConstruction; override;

    //: @exclude
    destructor  Destroy; override;

    {$IFDEF PORTUGUES}
    {:
    Adiciona um tag ao scan do driver.
    @param(Tag TTag. Tag a adicionar no scan do driver.)
    @raises(Exception caso alguma configuração esteja errada.)
    }
    {$ELSE}
    {:
    Add a tag into the scan cycle of the protocol driver.
    @param(Tag TTag. Tag to be added into scan cycle of the protocol driver.)
    @raises(Exception if something is wrong.)
    }
    {$ENDIF}
    procedure AddTag(TagObj:TTag);

    {$IFDEF PORTUGUES}
    //: Chama o editor de tags do driver.
    {$ELSE}
    //: Opens the Tag Builder of the protocol driver (if exists)
    {$ENDIF}
    procedure OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); virtual;

    {$IFDEF PORTUGUES}
    {:
    Remove um tag do scan do driver.
    @param(Tag TTag. Tag a remover do scan do driver.)
    }
    {$ELSE}
    {:
    Remove a tag from the scan cycle of the protocol driver.
    @param(Tag TTag. Tag to be removed.)
    }
    {$ENDIF}
    procedure RemoveTag(TagObj:TTag);

    {$IFDEF PORTUGUES}
    {:
    Função que informa se o Tag está associado ao driver.
    @param(TagObj TTag. Tag que deseja saber se está associado ao driver.)
    @returns(@true caso o tag esteja associado ao driver.)
    }
    {$ELSE}
    {:
    Function that returns if the tag is already linked with the protocol driver.
    @param(TagObj TTag. Tag to be checked if it's linked.)
    @returns(@true if the tag is linked with protocol.)
    }
    {$ENDIF}
    function  IsMyTag(TagObj:TTag):Boolean;

    {$IFDEF PORTUGUES}
    {:
    Função que retorna o tamanho em bits do registrador mapeado pelo tag.
    @param(Tag TTag. Tag que se deseja saber o tamanho do registrador.)
    @param(isWrite Boolean. Caso @true, informa o tamanho em bits usando as
           funções de escrita.)
    @returns(Tamanho em bits do registrador associado ou 0 (zero) caso falhe.)
    }
    {$ELSE}
    {:
    Returns the word size of the tag on protocol, in bits.
    @param(Tag TTag. Tag that wants know the word size.)
    @param(isWrite Boolean. If @true, returns the word size if want know the size
           using the write function.)
    @returns(The current word size on protocol of the tag, OR 0 (zero) if it fails.)
    }
    {$ENDIF}
    function  SizeOfTag(Tag:TTag; isWrite:Boolean; var ProtocolTagType:TProtocolTagType):BYTE; virtual; abstract;

    {$IFDEF PORTUGUES}
    {:
    Solicita a leitura por scan (@bold(assincrona)) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja ler.)
    @returns(Cardinal. Número único do pedido de leitura por scan.)
    }
    {$ELSE}
    {:
    Requests a tag update.
    @param(tagrec TTagRec. Record with informations about the tag.)
    @returns(Cardinal. The unique identification number of the request.)
    }
    {$ENDIF}
    function  ScanRead(const tagrec:TTagRec):Cardinal;

    {$IFDEF PORTUGUES}
    {:
    Solicita a escrita por scan (assincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja escrever.)
    @param(Values TArrayOfDouble Conjunto de valores a escrever.)
    @returns(Cardinal. Número único do pedido de escrita por scan.)
    }
    {$ELSE}
    {:
    Write values @bold(asynchronous) using the scan of the protocol driver.
    @param(tagrec TTagRec. Record with informations about the tag.)
    @param(Values TArrayOfDouble Values to be written.)
    @returns(Cardinal. The unique identification number of the request.)
    }
    {$ENDIF}
    function  ScanWrite(const tagrec:TTagRec; const Values:TArrayOfDouble):Cardinal;

    {$IFDEF PORTUGUES}
    {:
    Solicita a leitura (sincrona) de um tag.
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja ler.)
    }
    {$ELSE}
    {:
    Read the tag value from the device (synchronous).
    @param(tagrec TTagRec. Record with informations about the tag.)
    }
    {$ENDIF}
    procedure Read(const tagrec:TTagRec);

    {$IFDEF PORTUGUES}
    {:
    Escreve valores de um tag (sincrono).
    @param(tagrec TTagRec. Estrutura com as informações do tag que se deseja escrever.)
    @param(Values TArrayOfDouble Conjunto de valores a escrever.)
    }
    {$ELSE}
    {:
    Write the tag values (synchronous).
    @param(tagrec TTagRec. Record with informations about the tag.)
    @param(Values TArrayOfDouble Values to be written.)
    }
    {$ENDIF}
    procedure Write(const tagrec:TTagRec; const Values:TArrayOfDouble);

    {$IFDEF PORTUGUES}
    //: Retorna o endereço literal de um tag.
    {$ELSE}
    //: Returns the literal address of the tag.
    {$ENDIF}
    function LiteralTagAddress(aTag:TTag; aBlockTag:TTag=nil):String; virtual;

    {$IFDEF PORTUGUES}
    //: Conta os tags dependentes desse driver de protocolo.
    {$ELSE}
    //: Return how many tags are on scan cycle of the protocol driver.
    {$ENDIF}
    property TagCount:Integer read GetTagCount;

    {$IFDEF PORTUGUES}
    //: Lista cada tag dependente desse driver.
    {$ELSE}
    //: Return the tags using the index.
    {$ENDIF}
    property Tag[index:integer]:TTag read GetTag;

    {$IFDEF PORTUGUES}
    //: Lista o nome de cada tag dependente desse driver.
    {$ELSE}
    //: Return the tag names.
    {$ENDIF}
    property TagName[index:integer]:String read GetTagName;

    {$IFDEF PORTUGUES}
    //: Lista cada tag dependente desse driver usando o nome do tag como indice.
    {$ELSE}
    //: Returns the tag by the name index.
    {$ENDIF}
    property TagByName[Nome:String]:TTag read GetTagByName;
  published
    {$IFDEF PORTUGUES}
    {:
    Driver de porta que será usado para realizar as operações de comunicação
    do protoloco.
    @seealso(TCommPortDriver)
    }
    {$ELSE}
    {:
    Communication port driver used by the protocol driver.
    @seealso(TCommPortDriver)
    }
    {$ENDIF}
    property CommunicationPort:TCommPortDriver read PCommPort write SetCommPort nodefault;

    {$IFDEF PORTUGUES}
    //: Identificação (número único) do driver.
    {$ELSE}
    //: Unique protocol identification.
    {$ENDIF}
    property DriverID:Cardinal read PDriverID;

    {$IFDEF PORTUGUES}
    //: Tempo médio em millisegundos gasto atualizando valores dos tags e seus dependentes.
    {$ELSE}
    //: Average time in milliseconds used to update values of tags and their dependents.
    {$ENDIF}
    property AvgTagUpdateTime:Double read FUserUpdateTime;
  end;

var

  {$IFDEF PORTUGUES}
  {:
  Contador de drivers criados, usado para gerar nomes únicos dos eventos
  seções críticas e semaforos em ambiente Windows.

  @bold(Não altere o valor dessa variável.)
  }
  {$ELSE}
  {:
  Protocol driver counter, used to generate unique names of events, mutexes and
  semaphores on Windows platforms.

  @bold(Don't change the value of this variable.)
  }
  {$ENDIF}
   DriverCount:Cardinal;

implementation

uses PLCTag, hsstrings, Dialogs, math, crossdatetime, pascalScadaMTPCPU;

////////////////////////////////////////////////////////////////////////////////
//             inicio da implementação de TProtocolDriver
//                 implementation of TProtocolDriver
////////////////////////////////////////////////////////////////////////////////

constructor TProtocolDriver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PDriverID := DriverCount;
  Inc(DriverCount);

  FProtocolReady:=true;

  FReadCS := TCriticalSection.Create;
  FWriteCS:= TCriticalSection.Create;

  FPause := TCrossEvent.Create(nil,true,true,'');

  PCallersCS := TCriticalSection.Create;

  PScanUpdateThread := TScanUpdate.Create(true, Self, UpdateUserTime);
  {$IFNDEF WINCE}
  PScanUpdateThread.Priority:=tpHighest;
  {$ENDIF}
  PScanUpdateThread.OnGetValue     := SafeGetValue;
  PScanUpdateThread.OnScanTags     := GetMultipleValues;
  PScanUpdateThread.FreeOnTerminate:=true;

  PScanReadThread := TScanThread.Create(true, PScanUpdateThread);
  {$IFNDEF WINCE}
  PScanReadThread.Priority:=tpTimeCritical;
  {$ENDIF}
  PScanReadThread.OnDoScanRead := SafeScanRead;
  PScanReadThread.OnDoScanWrite := nil;
  PScanReadThread.FreeOnTerminate:=true;

  PScanWriteThread := TScanThread.Create(true, PScanUpdateThread);
  {$IFNDEF WINCE}
  PScanWriteThread.Priority:=tpTimeCritical;
  {$ENDIF}
  PScanWriteThread.OnDoScanRead    := nil;
  PScanWriteThread.OnDoScanWrite   := SafeScanWrite;
  PScanWriteThread.FreeOnTerminate := true;
end;

procedure TProtocolDriver.AfterConstruction;
begin
  Inherited AfterConstruction;
  PScanUpdateThread.WakeUp;

  PScanReadThread.WakeUp;
  PScanReadThread.WaitInit;

  PScanWriteThread.WakeUp;
  PScanWriteThread.WaitInit;
end;

destructor TProtocolDriver.Destroy;
var
  c:Integer;
begin
  PScanReadThread.Terminate;
  PScanReadThread.WaitFor;

  PScanWriteThread.Terminate;
  PScanWriteThread.WaitFor;

  PScanUpdateThread.Terminate;
  PScanUpdateThread.WaitFor;

  for c:=0 to High(PTags) do
    TPLCTag(PTags[c]).RemoveDriver;

  SetCommPort(nil);

  FReadCS.Destroy;
  FWriteCS.Destroy;

  FPause.Destroy;
  
  SetLength(PTags,0);
  PCallersCS.Destroy;
  inherited Destroy;
end;

procedure TProtocolDriver.OpenTagEditor(OwnerOfNewTags:TComponent; InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc);
begin
  MessageDlg(SWithoutTagBuilder, mtError,[mbOK],0);
end;

procedure TProtocolDriver.SetCommPort(CommPort:TCommPortDriver);
begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;

    //se for a mesma porta cai fora...
    //if is the same communication port, exit.
    if CommPort=PCommPort then exit;

    if PCommPort<>nil then begin
      if PCommPort.LockedBy=PDriverID then
        PCommPort.Unlock(PDriverID);
      PCommPort.DelProtocol(Self);
    end;

    if CommPort<>nil then begin
      CommPort.AddProtocol(Self);
    end;
    PCommPort := CommPort;
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoAddTag(TagObj:TTag; TagValid:Boolean);
var
  c:integer;
begin
  for c:=0 to High(PTags) do
    if PTags[c]=TagObj then
      raise Exception.Create(STagAlreadyRegiteredWithThisDriver);

  c:=Length(Ptags);
  SetLength(PTags,c+1);
  PTags[c] := TagObj;

  (TagObj as IScanableTagInterface).SetTagValidity(TagValid);
end;

procedure TProtocolDriver.DoDelTag(TagObj:TTag);
var
  c:Integer;
  h:integer;
  found:boolean;
begin
  if Length(PTags)<=0 then exit;

  h:=High(PTags);
  found := false;
  for c:=0 to h do
    if PTags[c]=TagObj then begin
      found := true;
      break;
    end;
  if found then begin
    (PTags[c] as IScanableTagInterface).SetTagValidity(false);
    PTags[c] := PTags[h];
    SetLength(PTags,h);
  end;
end;

procedure TProtocolDriver.AddTag(TagObj:TTag);
begin
  if not Supports(TagObj, IScanableTagInterface) then
    raise Exception.Create(SScanableNotSupported);

  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;
    
    DoAddTag(TagObj,false);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.RemoveTag(TagObj:TTag);
begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;

    DoDelTag(TagObj);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoExceptionIndexOut(index:integer);
begin
  if (index>high(PTags)) then
    raise Exception.Create(SoutOfBounds);
end;

function TProtocolDriver.GetTagCount;
begin
  //FCritical.Enter;
  try
    Result := Length(PTags);
  finally
    //FCritical.Leave;
  end;
end;

function TProtocolDriver.GetTag(index:integer):TTag;
begin
  //FCritical.Enter;
  try
    DoExceptionIndexOut(index);
    result:=PTags[index];
  finally
    //FCritical.Leave;
  end;
end;

function TProtocolDriver.GetTagName(index:integer):String;
begin
  Result:='';
  //FCritical.Enter;
  try
    DoExceptionIndexOut(index);
    result:=PTags[index].Name;
  finally
    //FCritical.Leave;
  end;

end;

function TProtocolDriver.GetTagByName(Nome:String):TTag;
var
  c:Integer;
begin
  Result := nil;
  //FCritical.Enter;
  try
    for c:=0 to High(PTags) do
      if PTags[c].Name = Nome then begin
        Result := PTags[c];
        break;
      end;
  finally
    //FCritical.Leave;
  end;
end;

function TProtocolDriver.IsMyTag(TagObj:TTag):Boolean;
var
  c:integer;
begin
  Result := false;
  //FCritical.Enter;
  try
    for c:=0 to High(PTags) do
      if TagObj=PTags[c] then begin
        Result := true;
        break;
      end;
  finally
    //FCritical.Leave;
  end;
end;

function TProtocolDriver.ScanRead(const tagrec:TTagRec):Cardinal;
begin
  try
    PCallersCS.Enter;
    //verifica se esta em edição, caso positivo evita o comando.
    //check if is in design-time
    if (csReading in ComponentState) or
       (csDestroying in ComponentState) then begin
       Result := 0;
       exit;
    end;

    //incrementa o contador de scanReads
    //zera o contador para evitar overflow;
    //
    //increment the scan read unique identification
    if FScanReadID=$FFFFFFFF then
       FScanReadID := 0
    else
       inc(FScanReadID);

    //posta uma mensagem de Leitura por Scan
    //send a message requesting a scanread
    if (PScanUpdateThread<>nil) then
      PScanUpdateThread.ScanRead(tagrec);

    Result := FScanReadID;

  finally
    PCallersCS.Leave;
  end;
end;

function TProtocolDriver.ScanWrite(const tagrec:TTagRec; const Values:TArrayOfDouble):Cardinal;
var
   pkg:PScanWriteRec;
begin
  try
    PCallersCS.Enter;
    //verifica se esta em edição, caso positivo evita o comando.
    //check if is in design-time.
    if (csReading in ComponentState) or
       (csDestroying in ComponentState) then begin
       Result := 0;
       exit;
    end;

    //incrementa o contador de ScanWrites
    //zera o contador para evitar overflow;
    //
    //increment the scan write unique identification
    if FScanWriteID=$FFFFFFFF then
       FScanWriteID := 0
    else
       inc(FScanWriteID);
       
    //cria um pacote de escrita por scan
    //
    //creates the message of scan write
    New(pkg);
    pkg^.SWID:=FScanReadID;
    //copia o TagRec
    //copy the tagrec
    Move(tagrec, pkg^.Tag, sizeof(TTagRec));
    //copia os valores
    //copy the values
    pkg^.ValuesToWrite := Values;
    pkg^.WriteResult:=ioNone;
    pkg^.ValueTimeStamp:=CrossNow;

    //posta uma mensagem de Escrita por Scan
    //send the scanwrite message to thread.
    if (PScanWriteThread<>nil) then begin
      PScanWriteThread.ScanWrite(pkg);
      CrossThreadSwitch;
    end;

    Result := FScanWriteID;
  finally
    PCallersCS.Leave;
  end;
end;

procedure TProtocolDriver.Read(const tagrec:TTagRec);
var
  res:TProtocolIOResult;
  Values:TArrayOfDouble;
begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;
    res := DoRead(tagrec,Values,true);
    if assigned(tagrec.CallBack) then
      tagrec.CallBack(Values,CrossNow,tcRead,res,tagrec.RealOffset);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
    SetLength(Values,0);
  end;
end;

procedure TProtocolDriver.Write(const tagrec:TTagRec; const Values:TArrayOfDouble);
var
  res:TProtocolIOResult;
begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;

    res := DoWrite(tagrec,Values,true);
    if assigned(tagrec.CallBack) then
      tagrec.CallBack(Values,CrossNow,tcWrite,res,tagrec.RealOffset);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
  end;
end;

function TProtocolDriver.LiteralTagAddress(aTag: TTag; aBlockTag: TTag):String;
begin
  Result:='';
end;

procedure TProtocolDriver.CommPortCallBack(var Result:TIOPacket);
begin
  if Result.Res2<>nil then
     CopyIOPacket(Result,PIOPacket(Result.Res2)^);
  if Result.res1 is TCrossEvent then
     TCrossEvent(Result.res1).SetEvent;
end;

procedure TProtocolDriver.CopyIOPacket(const Source:TIOPacket; var Dest:TIOPacket);
begin
  Dest.PacketID := Source.PacketID;
  Dest.WriteIOResult := Source.WriteIOResult;
  Dest.ToWrite := Source.ToWrite;
  Dest.Written := Source.Written;
  Dest.WriteRetries := Source.WriteRetries;
  Dest.DelayBetweenCommand := Source.DelayBetweenCommand;
  Dest.ReadIOResult := Source.ReadIOResult;
  Dest.ToRead := Source.ToRead;
  Dest.Received := Source.Received;
  Dest.ReadRetries := Source.ReadRetries;
  SetLength(Dest.BufferToRead, 0);
  SetLength(Dest.BufferToWrite, 0);
  Dest.BufferToRead := Source.BufferToRead;
  Dest.BufferToWrite:= Source.BufferToWrite;
  Dest.Res1 := Source.Res1;
  Dest.Res2 := Source.Res2;
end;

procedure TProtocolDriver.SafeScanRead(Sender:TObject; var NeedSleep:Integer);
begin
   try
      FPause.WaitFor($FFFFFFFF);
      FWriteCS.Enter;
      FReadCS.Enter;
      DoScanRead(Sender, NeedSleep);
   finally
      FReadCS.Leave;
      FWriteCS.Leave;
      CrossThreadSwitch;
   end;
end;

function  TProtocolDriver.SafeScanWrite(const TagRec:TTagRec; const values:TArrayOfDouble):TProtocolIOResult;
begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;

    Result := DoWrite(TagRec,values,false)
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.SafeGetValue(const TagRec:TTagRec; var values:TScanReadRec);
begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FReadCS.Enter;

    DoGetValue(TagRec,values);
  finally
    FReadCS.Leave;
    FPause.SetEvent;
  end;
end;

function  TProtocolDriver.GetMultipleValues(var MultiValues:TArrayOfScanUpdateRec):Integer;
var
  t, valueSet:Integer;
  first:Boolean;
  tagiface:IScanableTagInterface;
  tr:TTagRec;
  remainingMs:Int64;
  ScanReadRec:TScanReadRec;
  doneOne:Boolean;
begin
  doneOne:=false;
  try
    Result:=0;
    valueSet:=-1;
    
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FReadCS.Enter;

    if ComponentState*[csDestroying]<>[] then exit;

    if (PCommPort=nil) or (not PCommPort.ReallyActive) then begin
      Result:=50; //waits 50ms
      exit;
    end;

    for t:=0 to TagCount-1 do begin
      if Supports(Tag[t], IScanableTagInterface) then begin
        tagiface:=Tag[t] as IScanableTagInterface;
        if tagiface.IsValidTag then begin

          if PReadSomethingAlways then
            remainingMs:=tagiface.RemainingMiliseconds
          else
            remainingMs:=tagiface.RemainingMilisecondsForNextScan;

          //se o tempo restante é maior que zero
          //if the remaining time is greater than zero.
          if remainingMs>0 then begin
            if first then begin
              Result:=remainingMs;
              first:=false;
            end else
              Result := Min(remainingMs, Result);
          end else begin
            doneOne:=true;
            inc(valueSet);
            SetLength(MultiValues,valueSet+1);

            tagiface.BuildTagRec(tr,0,0);

            SetLength(ScanReadRec.Values, tr.Size);
            DoGetValue(tr, ScanReadRec);

            MultiValues[valueSet].Owner         :=Tag[t];
            MultiValues[valueSet].LastResult    :=ScanReadRec.LastQueryResult;
            MultiValues[valueSet].CallBack      :=tr.CallBack;
            MultiValues[valueSet].Values        :=ScanReadRec.Values;
            MultiValues[valueSet].ValueTimeStamp:=ScanReadRec.ValuesTimestamp;
          end;
        end;
      end;
    end;

  finally
    FReadCS.Leave;
    if doneOne then Result:=0;
    FPause.SetEvent;
  end;
end;

function  TProtocolDriver.GetPortOpenedEvent:TNotifyEvent;
begin
  Result := DoPortOpened;
end;

function  TProtocolDriver.GetPortClosedEvent:TNotifyEvent;
begin
  Result := DoPortClosed;
end;

function  TProtocolDriver.GetPortDisconnectedEvent:TNotifyEvent;
begin
  Result := DoPortDisconnected;
end;

function  TProtocolDriver.NotifyThisEvents:TNotifyThisEvents;
begin
  Result:=[];
end;

procedure TProtocolDriver.DoPortOpened(Sender: TObject);
begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;

    PortOpened(Sender);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoPortClosed(Sender: TObject);
begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;

    PortClosed(Sender);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoPortDisconnected(Sender: TObject);
begin
  try
    //tenta entrar no Mutex
    //try enter on mutex
    while not FPause.ResetEvent do
      CrossThreadSwitch;

    FWriteCS.Enter;
    FReadCS.Enter;

    PortDisconnected(Sender);
  finally
    FReadCS.Leave;
    FWriteCS.Leave;
    FPause.SetEvent;
  end;
end;

procedure TProtocolDriver.DoPortRemoved(Sender:TObject);
begin
  if CommunicationPort=Sender then
    CommunicationPort:=nil;
end;

procedure TProtocolDriver.UpdateUserTime(usertime: Double);
begin
  FUserUpdateTime:=usertime;
end;

procedure TProtocolDriver.HighLatencyOperationWillBegin(Sender: TObject);
begin
  FReadCS.Leave;
end;

procedure TProtocolDriver.HighLatencyOperationWasEnded(Sender: TObject);
begin
  FReadCS.Enter;
end;

procedure TProtocolDriver.PortOpened(Sender: TObject);
begin

end;

procedure TProtocolDriver.PortClosed(Sender: TObject);
begin

end;

procedure TProtocolDriver.PortDisconnected(Sender: TObject);
begin

end;

initialization

DriverCount:=1;

end.
